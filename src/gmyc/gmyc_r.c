#define R_NO_REMAP
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <string.h>
#include "gmyc_core.h"

/* ── helpers ──────────────────────────────────────────────────── */
static void parse_ape_tree(SEXP r_edge, SEXP r_edge_len, SEXP r_tip_label,
                           int n_tips, int n_nodes, GTree *tree,
                           double *bt, int n_bt)
{
    int n_edge = Rf_length(r_edge_len);
    const char **tip_label = (const char **)R_alloc(n_tips, sizeof(char *));
    for (int i = 0; i < n_tips; i++)
        tip_label[i] = CHAR(STRING_ELT(r_tip_label, i));

    if (gtree_from_ape(INTEGER(r_edge), REAL(r_edge_len), n_edge,
                       tip_label, n_tips, n_nodes, tree) != 0)
        Rf_error("gmyc: failed to parse tree.");

    for (int j = 0; j < n_bt; j++)
        tree->nodes[n_tips + j].height = bt[j];
    tree->n_internal = n_bt;
    tree->heights = NULL;
}

/* ── external-pointer finalizer ───────────────────────────────── */
static void gmyc_prep_finalize(SEXP xptr)
{
    GMYCPrep *p = (GMYCPrep *)R_ExternalPtrAddr(xptr);
    if (p) {
        gmyc_prep_free(p);
        free(p);
        R_ClearExternalPtr(xptr);
    }
}

/* ── C_gmyc_setup ─────────────────────────────────────────────── */
/* Parse tree, precompute per-threshold state, run null model.
   Returns an external pointer to GMYCPrep + metadata attributes. */
SEXP C_gmyc_setup(SEXP r_edge, SEXP r_edge_len, SEXP r_tip_label,
                  SEXP r_n_tips, SEXP r_n_nodes, SEXP r_bt)
{
    int n_tips  = Rf_asInteger(r_n_tips);
    int n_nodes = Rf_asInteger(r_n_nodes);
    int n_bt    = Rf_length(r_bt);

    if (n_tips < 3)          Rf_error("gmyc: need at least 3 tips.");
    if (n_nodes >= MAX_NODES) Rf_error("gmyc: tree too large.");
    if (n_bt != n_nodes - n_tips)
        Rf_error("gmyc: bt length %d != n_internal %d.", n_bt, n_nodes - n_tips);

    /* Build tree in R_alloc temp storage, then copy into GMYCPrep */
    GTree *tmp = (GTree *)R_alloc(1, sizeof(GTree));
    parse_ape_tree(r_edge, r_edge_len, r_tip_label,
                   n_tips, n_nodes, tmp, REAL(r_bt), n_bt);

    GMYCPrep *prep = (GMYCPrep *)malloc(sizeof(GMYCPrep));
    if (!prep) Rf_error("gmyc_setup: out of memory.");
    if (gmyc_prep_init(prep, tmp) != 0) {
        free(prep);
        Rf_error("gmyc_setup: precomputation failed.");
    }

    SEXP xptr = PROTECT(R_MakeExternalPtr(prep, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(xptr, gmyc_prep_finalize, TRUE);

    /* Attach metadata as attributes */
    Rf_setAttrib(xptr, Rf_install("n_thresh"),
                 PROTECT(Rf_ScalarInteger(prep->m)));
    Rf_setAttrib(xptr, Rf_install("stthresh_c"),
                 PROTECT(Rf_ScalarInteger(prep->stthresh_c)));
    Rf_setAttrib(xptr, Rf_install("null_ll"),
                 PROTECT(Rf_ScalarReal(prep->null_ll)));
    Rf_setAttrib(xptr, Rf_install("null_p"),
                 PROTECT(Rf_ScalarReal(prep->null_p)));
    Rf_setAttrib(xptr, Rf_install("null_lam"),
                 PROTECT(Rf_ScalarReal(prep->null_lam)));

    /* Sorted event times (= sb in R, the threshold-time vector) */
    SEXP r_ev = PROTECT(Rf_allocVector(REALSXP, prep->m));
    for (int i = 0; i < prep->m; i++) REAL(r_ev)[i] = prep->ev[i].time;
    Rf_setAttrib(xptr, Rf_install("ev_times"), r_ev);

    /* Per-threshold K (for building cluster/entity vectors in R) */
    SEXP r_K = PROTECT(Rf_allocVector(INTSXP, prep->m));
    for (int i = 0; i < prep->m; i++) INTEGER(r_K)[i] = prep->thresh_K[i];
    Rf_setAttrib(xptr, Rf_install("thresh_K"), r_K);

    UNPROTECT(8);  /* xptr + 7 attribute values */
    return xptr;
}

/* ── C_gmyc_loglik ────────────────────────────────────────────── */
/* Evaluate GMYC log-likelihood at one threshold + (spe_p, coa_p).
   r_T_idx: 0-based threshold index (integer scalar).
   r_pq:    length-2 real vector c(spe_p, coa_p).
   Returns: scalar log-likelihood. */
SEXP C_gmyc_loglik(SEXP r_setup, SEXP r_T_idx, SEXP r_pq)
{
    if (TYPEOF(r_setup) != EXTPTRSXP)
        Rf_error("C_gmyc_loglik: expected external pointer.");
    GMYCPrep *prep = (GMYCPrep *)R_ExternalPtrAddr(r_setup);
    if (!prep) Rf_error("C_gmyc_loglik: null pointer (already freed?).");

    int    T_idx = Rf_asInteger(r_T_idx);
    double spe_p = REAL(r_pq)[0];
    double coa_p = REAL(r_pq)[1];

    double ll = gmyc_loglik_at(prep, T_idx, spe_p, coa_p, NULL, NULL);
    return Rf_ScalarReal(ll);
}

/* ── C_gmyc_loglik_params ─────────────────────────────────────── */
/* Like C_gmyc_loglik but also returns lambda_spe, lambda_coa.
   Returns: length-3 real vector c(ll, lam_spe, lam_coa). */
SEXP C_gmyc_loglik_params(SEXP r_setup, SEXP r_T_idx, SEXP r_pq)
{
    GMYCPrep *prep = (GMYCPrep *)R_ExternalPtrAddr(r_setup);
    int    T_idx = Rf_asInteger(r_T_idx);
    double spe_p = REAL(r_pq)[0], coa_p = REAL(r_pq)[1];
    double lam_spe = 0.0, lam_coa = 0.0;
    double ll = gmyc_loglik_at(prep, T_idx, spe_p, coa_p, &lam_spe, &lam_coa);
    SEXP res = PROTECT(Rf_allocVector(REALSXP, 3));
    REAL(res)[0] = ll;
    REAL(res)[1] = lam_spe;
    REAL(res)[2] = lam_coa;
    UNPROTECT(1);
    return res;
}

/* ── C_gmyc_clusters ──────────────────────────────────────────── */
/* Compute per-tip cluster assignment at a given threshold.
   r_T_idx: 0-based threshold index.
   Returns: integer vector of length n_taxa, cluster IDs (1-based). */
SEXP C_gmyc_clusters(SEXP r_setup, SEXP r_T_idx)
{
    GMYCPrep *prep = (GMYCPrep *)R_ExternalPtrAddr(r_setup);
    int T_idx = Rf_asInteger(r_T_idx);
    int n_taxa = prep->tree->n_taxa;

    /* node_cid is precomputed per threshold */
    const int *node_cid = prep->node_cid + (size_t)T_idx * prep->tree->n_nodes;
    int K = prep->thresh_K[T_idx];

    SEXP res = PROTECT(Rf_allocVector(INTSXP, n_taxa));
    int singleton = K + 1;
    for (int i = 0; i < n_taxa; i++) {
        int cid = node_cid[i];
        INTEGER(res)[i] = (cid > 0) ? cid : singleton++;
    }
    UNPROTECT(1);
    return res;
}

/* ── C_gmyc_threshold_data ────────────────────────────────────── */
/* Return per-event state for threshold T_idx so R can compute the
   loglik with R arithmetic (matching splits' floating-point exactly).
   Returns a named list: n_spe (int[m]), jc_mat (int m×K, col-major),
   x (real[m]), numSpe, numCoa, K (all scalars). */
SEXP C_gmyc_threshold_data(SEXP r_setup, SEXP r_T_idx)
{
    GMYCPrep *prep = (GMYCPrep *)R_ExternalPtrAddr(r_setup);
    int T_idx = Rf_asInteger(r_T_idx);
    int m = prep->m;
    int K = prep->thresh_K[T_idx];
    int numSpe = prep->thresh_numSpe[T_idx];
    int numCoa = m - numSpe;
    const int *nod_type = prep->nod_type + (size_t)T_idx * m;
    const int *node_cid = prep->node_cid + (size_t)T_idx * prep->tree->n_nodes;

    SEXP r_nspe  = PROTECT(Rf_allocVector(INTSXP, m));
    SEXP r_jcmat = PROTECT(Rf_allocMatrix(INTSXP, m, K > 0 ? K : 1));
    SEXP r_x     = PROTECT(Rf_allocVector(REALSXP, m));

    int *nspe_p  = INTEGER(r_nspe);
    int *jcmat_p = INTEGER(r_jcmat);
    double *xp   = REAL(r_x);

    /* zero jcmat (handles K=0 and padding) */
    memset(jcmat_p, 0, (size_t)m * (K > 0 ? K : 1) * sizeof(int));

    /* Run state machine — identical to gmyc_loglik_pq pass 1 */
    int jc[MAX_NODES];
    memset(jc, 0, (K + 1) * sizeof(int));
    int n_spe = 1;
    for (int i = 0; i < m; i++) {
        int nt = nod_type[i];
        int nid = prep->ev[i].node_id;
        if      (nt == 0) { n_spe++; }
        else if (nt == 2) { n_spe--;
                            int c = node_cid[nid];
                            if (c > 0 && c <= K) jc[c] = 2; }
        else              { int c = node_cid[nid];
                            if (c > 0 && c <= K) jc[c]++; }
        nspe_p[i] = n_spe;
        xp[i]     = prep->x[i];
        for (int c = 1; c <= K; c++)
            jcmat_p[i + (size_t)(c - 1) * m] = jc[c];  /* col-major */
    }

    SEXP res  = PROTECT(Rf_allocVector(VECSXP, 6));
    SEXP nms  = PROTECT(Rf_allocVector(STRSXP, 6));
    SET_VECTOR_ELT(res, 0, r_nspe);
    SET_VECTOR_ELT(res, 1, r_jcmat);
    SET_VECTOR_ELT(res, 2, r_x);
    SET_VECTOR_ELT(res, 3, Rf_ScalarInteger(numSpe));
    SET_VECTOR_ELT(res, 4, Rf_ScalarInteger(numCoa));
    SET_VECTOR_ELT(res, 5, Rf_ScalarInteger(K));
    SET_STRING_ELT(nms, 0, Rf_mkChar("n_spe"));
    SET_STRING_ELT(nms, 1, Rf_mkChar("jc_mat"));
    SET_STRING_ELT(nms, 2, Rf_mkChar("x"));
    SET_STRING_ELT(nms, 3, Rf_mkChar("numSpe"));
    SET_STRING_ELT(nms, 4, Rf_mkChar("numCoa"));
    SET_STRING_ELT(nms, 5, Rf_mkChar("K"));
    Rf_setAttrib(res, R_NamesSymbol, nms);
    UNPROTECT(5);
    return res;
}

/* ── C_gmyc (legacy full-C path, kept for compatibility) ─────── */
SEXP C_gmyc(SEXP r_edge, SEXP r_edge_len, SEXP r_tip_label,
            SEXP r_n_tips, SEXP r_n_nodes, SEXP r_bt)
{
    int n_tips  = Rf_asInteger(r_n_tips);
    int n_nodes = Rf_asInteger(r_n_nodes);
    int n_bt    = Rf_length(r_bt);

    if (n_tips < 3)          Rf_error("gmyc: need at least 3 tips.");
    if (n_nodes >= MAX_NODES) Rf_error("gmyc: tree too large.");
    if (n_bt != n_nodes - n_tips)
        Rf_error("gmyc: bt length %d != n_internal %d.", n_bt, n_nodes - n_tips);

    GTree *tree = (GTree *)R_alloc(1, sizeof(GTree));
    parse_ape_tree(r_edge, r_edge_len, r_tip_label,
                   n_tips, n_nodes, tree, REAL(r_bt), n_bt);

    GResult *res = (GResult *)R_alloc(1, sizeof(GResult));
    res->thresh_ll      = (double *)R_alloc(n_bt, sizeof(double));
    res->thresh_K       = (int    *)R_alloc(n_bt, sizeof(int));
    res->thresh_lam_div = (double *)R_alloc(n_bt, sizeof(double));
    res->thresh_lam_coa = (double *)R_alloc(n_bt, sizeof(double));
    res->thresh_p_div   = (double *)R_alloc(n_bt, sizeof(double));
    res->thresh_p_coa   = (double *)R_alloc(n_bt, sizeof(double));
    for (int i = 0; i < n_bt; i++) {
        res->thresh_ll[i]      = NA_REAL;
        res->thresh_K[i]       = NA_INTEGER;
        res->thresh_lam_div[i] = NA_REAL;
        res->thresh_lam_coa[i] = NA_REAL;
        res->thresh_p_div[i]   = NA_REAL;
        res->thresh_p_coa[i]   = NA_REAL;
    }
    res->n_thresh   = n_bt;
    res->stthresh_c = 0;

    if (gmyc_core(tree, res) != 0)
        Rf_error("gmyc: analysis failed.");

    const int NF = 18;
    SEXP result = PROTECT(Rf_allocVector(VECSXP, NF));
    SEXP names  = PROTECT(Rf_allocVector(STRSXP, NF));
    int fi = 0;
#define SF(nm,val) SET_VECTOR_ELT(result,fi,(val));SET_STRING_ELT(names,fi,Rf_mkChar(nm));fi++
    SEXP s;
    s=PROTECT(Rf_ScalarReal(res->log_lik_null));   SF("likelihood.null",s);   UNPROTECT(1);
    s=PROTECT(Rf_ScalarReal(res->log_lik_gmyc));   SF("likelihood.gmyc",s);   UNPROTECT(1);
    s=PROTECT(Rf_ScalarReal(res->lr_stat));        SF("likelihood.ratio",s);  UNPROTECT(1);
    s=PROTECT(Rf_ScalarReal(res->p_value));        SF("p.value",s);           UNPROTECT(1);
    s=PROTECT(Rf_ScalarReal(res->threshold));      SF("threshold",s);         UNPROTECT(1);
    s=PROTECT(Rf_ScalarInteger(res->n_clusters));  SF("clusters",s);          UNPROTECT(1);
    s=PROTECT(Rf_ScalarInteger(res->n_entities));  SF("entities",s);          UNPROTECT(1);
    s=PROTECT(Rf_allocVector(INTSXP,n_tips));
    for(int i=0;i<n_tips;i++)INTEGER(s)[i]=res->cluster[i];
    SF("cluster.tips",s); UNPROTECT(1);
    s=PROTECT(Rf_allocVector(STRSXP,n_tips));
    for(int i=0;i<n_tips;i++)SET_STRING_ELT(s,i,Rf_mkChar(res->tip_labels[i]));
    SF("tip.label",s); UNPROTECT(1);
    s=PROTECT(Rf_allocVector(REALSXP,n_bt));
    for(int i=0;i<n_bt;i++)REAL(s)[i]=res->thresh_ll[i];
    SF("thresh_ll",s); UNPROTECT(1);
    s=PROTECT(Rf_allocVector(INTSXP,n_bt));
    for(int i=0;i<n_bt;i++)INTEGER(s)[i]=res->thresh_K[i];
    SF("thresh_K",s); UNPROTECT(1);
    s=PROTECT(Rf_ScalarInteger(res->stthresh_c)); SF("stthresh",s); UNPROTECT(1);
    s=PROTECT(Rf_ScalarInteger(res->n_thresh));   SF("n_thresh",s); UNPROTECT(1);
    s=PROTECT(Rf_mkString("single"));             SF("method",s);   UNPROTECT(1);
    s=PROTECT(Rf_allocVector(REALSXP,n_bt));
    for(int i=0;i<n_bt;i++)REAL(s)[i]=res->thresh_lam_div[i];
    SF("thresh_lam_div",s); UNPROTECT(1);
    s=PROTECT(Rf_allocVector(REALSXP,n_bt));
    for(int i=0;i<n_bt;i++)REAL(s)[i]=res->thresh_lam_coa[i];
    SF("thresh_lam_coa",s); UNPROTECT(1);
    s=PROTECT(Rf_allocVector(REALSXP,n_bt));
    for(int i=0;i<n_bt;i++)REAL(s)[i]=res->thresh_p_div[i];
    SF("thresh_p_div",s); UNPROTECT(1);
    s=PROTECT(Rf_allocVector(REALSXP,n_bt));
    for(int i=0;i<n_bt;i++)REAL(s)[i]=res->thresh_p_coa[i];
    SF("thresh_p_coa",s); UNPROTECT(1);
#undef SF
    Rf_setAttrib(result,R_NamesSymbol,names);
    s=PROTECT(Rf_mkString("gmyc_raw"));Rf_classgets(result,s);UNPROTECT(1);
    UNPROTECT(2);
    return result;
}

/* ── bGMYC fast loglik helpers ───────────────────────────────────── */

/* Run the GMYC state machine for T_idx and cache:
     n_spe_arr[i]       = speciation lineage count after event i
     log_n[i]           = log(n_spe_arr[i])  (or -HUGE_VAL when 0)
     log_jj[i*m + c]    = log(jc[c+1]*(jc[c+1]-1)) for cluster c+1  (0-based c)
                          (-HUGE_VAL when jj == 0)
   Stride for log_jj is always m (max possible K), so the array must be m*m. */
static void bgmyc_rebuild_cache(const GMYCPrep *prep, int T_idx,
                                 int *n_spe_arr, double *log_n, double *log_jj)
{
    int m = prep->m;
    int K = prep->thresh_K[T_idx];
    const int *nod_type = prep->nod_type + (size_t)T_idx * m;
    const int *node_cid = prep->node_cid + (size_t)T_idx * prep->tree->n_nodes;

    /* Local jc array, reusing across events */
    int jc[MAX_NODES];
    memset(jc, 0, (K + 2) * sizeof(int));
    int n_spe = 1;

    for (int i = 0; i < m; i++) {
        int nt = nod_type[i], nid = prep->ev[i].node_id;
        if      (nt == 0) { n_spe++; }
        else if (nt == 2) { n_spe--;
                            int c = node_cid[nid]; if (c > 0 && c <= K) jc[c] = 2; }
        else              { int c = node_cid[nid]; if (c > 0 && c <= K) jc[c]++; }

        n_spe_arr[i] = n_spe;
        log_n[i]     = (n_spe > 0) ? log((double)n_spe) : -HUGE_VAL;

        double *row = log_jj + (size_t)i * m;
        for (int c = 1; c <= K; c++) {
            int jj = jc[c] * (jc[c] - 1);
            row[c - 1] = (jj > 0) ? log((double)jj) : -HUGE_VAL;
        }
    }
}

/* GMYC loglik using precomputed log_n / log_jj (replaces pow with exp).
   Identical math to gmyc_loglik_pq; faster because log values are cached
   and exp(p * log_x) avoids the hidden log() inside pow(x, p). */
static double bgmyc_fast_loglik(const GMYCPrep *prep, int T_idx,
                                 double spe_p, double coa_p,
                                 const int *n_spe_arr,
                                 const double *log_n, const double *log_jj)
{
    int m      = prep->m;
    int K      = prep->thresh_K[T_idx];
    int numSpe = prep->thresh_numSpe[T_idx];
    int numCoa = m - numSpe;
    if (K <= 0 || numCoa <= 0) return -INFINITY;
    const double *x = prep->x;

    /* Pass 1: MLE denominators Ty (speciation) and Tc (coalescent) */
    double Ty = 0.0, Tc = 0.0;
    for (int i = 0; i < m; i++) {
        int n = n_spe_arr[i];
        double Bspe = (n > 0) ? exp(spe_p * log_n[i]) : 0.0;
        Ty += Bspe * x[i];

        const double *row = log_jj + (size_t)i * m;
        double Bcoa = 0.0;
        for (int c = 0; c < K; c++) {
            if (row[c] > -HUGE_VAL / 2.0) Bcoa += exp(coa_p * row[c]);
        }
        if (!isfinite(Bcoa)) return -INFINITY;
        Tc += Bcoa * x[i];
    }
    if (!isfinite(Ty) || !isfinite(Tc)) return -INFINITY;

    double lam_spe = (numSpe > 0 && Ty > 0.0) ? (double)numSpe / Ty : 0.0;
    double lam_coa = (numCoa > 0 && Tc > 0.0) ? (double)numCoa / Tc : 0.0;

    /* Pass 2: log-likelihood */
    double logl = 0.0;
    for (int i = 0; i < m; i++) {
        int n = n_spe_arr[i];
        double b = 0.0;
        if (lam_spe > 0.0 && n > 0)
            b += lam_spe * exp(spe_p * log_n[i]);
        if (lam_coa > 0.0) {
            const double *row = log_jj + (size_t)i * m;
            for (int c = 0; c < K; c++) {
                if (row[c] > -HUGE_VAL / 2.0) b += lam_coa * exp(coa_p * row[c]);
            }
        }
        if (b <= 0.0 || !isfinite(b)) return -INFINITY;
        logl += log(b) - b * x[i];
    }
    return logl;
}

/* ── C_bgmyc_mcmc ────────────────────────────────────────────────── */
/* Bayesian GMYC (bGMYC) Metropolis-within-Gibbs sampler running entirely
   in C.  Replaces the R-level for-loop bottleneck in run_bgmyc().

   Arguments:
     r_setup    GMYCPrep external pointer (from C_gmyc_setup)
     r_mcmc     integer  — total MCMC steps
     r_burnin   integer  — steps discarded as burn-in
     r_thinning integer  — thinning interval
     r_prior    real[6]  — c(py1, py2, pc1, pc2, t1, t2) uniform prior bounds
     r_scale    real[3]  — c(scale_py, scale_pc, scale_t) proposal widths
     r_start    real[3]  — c(start_py, start_pc, start_t) initial values
     r_quiet    integer  — 0 = print progress, 1 = silent

   Returns list(par = matrix(n_keep × 4), accept = numeric(3)).
   Columns of par: py, pc, t, loglik. */
SEXP C_bgmyc_mcmc(SEXP r_setup, SEXP r_mcmc, SEXP r_burnin, SEXP r_thinning,
                  SEXP r_prior, SEXP r_scale, SEXP r_start, SEXP r_quiet)
{
    if (TYPEOF(r_setup) != EXTPTRSXP)
        Rf_error("C_bgmyc_mcmc: expected external pointer from C_gmyc_setup.");
    GMYCPrep *prep = (GMYCPrep *)R_ExternalPtrAddr(r_setup);
    if (!prep) Rf_error("C_bgmyc_mcmc: null pointer (already freed?).");

    int mcmc     = Rf_asInteger(r_mcmc);
    int burnin   = Rf_asInteger(r_burnin);
    int thinning = Rf_asInteger(r_thinning);
    int quiet    = Rf_asInteger(r_quiet);

    const double *pr = REAL(r_prior);
    double py1_b = pr[0], py2_b = pr[1];
    double pc1_b = pr[2], pc2_b = pr[3];
    int    t1_b  = (int)pr[4], t2_b = (int)pr[5];

    const double *sc = REAL(r_scale);
    double sc_py = sc[0], sc_pc = sc[1], sc_t = sc[2];

    const double *st = REAL(r_start);
    double py = st[0], pc = st[1];
    int    t  = (int)st[2];

    int NNodes = prep->m;   /* number of precomputed threshold positions */

    int n_keep = (mcmc - burnin) / thinning;
    if (n_keep < 0) n_keep = 0;

    /* Output matrix: n_keep rows × 4 cols, column-major */
    SEXP r_par = PROTECT(Rf_allocMatrix(REALSXP, n_keep, 4));
    double *col_py = REAL(r_par);
    double *col_pc = col_py + n_keep;
    double *col_t  = col_pc + n_keep;
    double *col_ll = col_t  + n_keep;

    int acc_py = 0, acc_pc = 0, acc_t = 0;

    /* Pre-compute log-value cache for current threshold (fast py/pc updates) */
    int    *n_spe_arr = (int    *)R_alloc((size_t)NNodes, sizeof(int));
    double *log_n     = (double *)R_alloc((size_t)NNodes, sizeof(double));
    double *log_jj    = (double *)R_alloc((size_t)NNodes * NNodes, sizeof(double));
    bgmyc_rebuild_cache(prep, t - 1, n_spe_arr, log_n, log_jj);

    double f0 = bgmyc_fast_loglik(prep, t - 1, py, pc,
                                  n_spe_arr, log_n, log_jj);

    /* Progress reporting: fire at 10%, 20%, ..., 100% */
    int pct_step = mcmc / 10;
    if (pct_step < 1) pct_step = 1;
    /* User-interrupt check every 10k steps */
    int check_step = 10000;

    GetRNGstate();

    int ki = 0;
    for (int i = 0; i < mcmc; i++) {

        /* ── Update py (Yule rate-change exponent) ── */
        /* Cache is valid (T unchanged): use fast loglik */
        {
            /* Gamma proposal: shape = sc_py, scale = py / sc_py */
            double py_n = Rf_rgamma(sc_py, py / sc_py);
            if (py_n > py1_b && py_n < py2_b) {
                double f1 = bgmyc_fast_loglik(prep, t - 1, py_n, pc,
                                              n_spe_arr, log_n, log_jj);
                /* MH correction for asymmetric gamma proposal:
                   log q(py_old | py_new) - log q(py_new | py_old) */
                double log_q = Rf_dgamma(py,   sc_py, py_n / sc_py, 1)
                             - Rf_dgamma(py_n, sc_py, py   / sc_py, 1);
                if (R_finite(f1) && unif_rand() < exp(f1 - f0 + log_q)) {
                    py = py_n; f0 = f1; acc_py++;
                }
            }
        }

        /* ── Update pc (coalescent rate-change exponent) ── */
        /* Cache is valid (T unchanged): use fast loglik */
        {
            double pc_n = Rf_rgamma(sc_pc, pc / sc_pc);
            if (pc_n > pc1_b && pc_n < pc2_b) {
                double f1 = bgmyc_fast_loglik(prep, t - 1, py, pc_n,
                                              n_spe_arr, log_n, log_jj);
                double log_q = Rf_dgamma(pc,   sc_pc, pc_n / sc_pc, 1)
                             - Rf_dgamma(pc_n, sc_pc, pc   / sc_pc, 1);
                if (R_finite(f1) && unif_rand() < exp(f1 - f0 + log_q)) {
                    pc = pc_n; f0 = f1; acc_pc++;
                }
            }
        }

        /* ── Update t (threshold / N species, integer random walk) ── */
        /* T changes: use original gmyc_loglik_at (avoids rebuilding cache
           for proposals that will be rejected ~89% of the time).  On
           acceptance, rebuild cache for the new T and recompute f0 via the
           fast path so subsequent py/pc updates use consistent cached data. */
        {
            int t_n = (int)round((double)t + norm_rand() * sc_t);
            if (t_n >= t1_b && t_n <= t2_b && t_n >= 2 && t_n < NNodes) {
                double f1 = gmyc_loglik_at(prep, t_n - 1, py, pc, NULL, NULL);
                /* Gaussian proposal: symmetric — no MH correction */
                if (R_finite(f1) && unif_rand() < exp(f1 - f0)) {
                    t = t_n; acc_t++;
                    bgmyc_rebuild_cache(prep, t - 1, n_spe_arr, log_n, log_jj);
                    f0 = bgmyc_fast_loglik(prep, t - 1, py, pc,
                                           n_spe_arr, log_n, log_jj);
                }
            }
        }

        /* ── Retain sample ──
           Mirrors R: `i > burnin && (i - burnin) %% thinning == 0`
           where R i is 1-based.  C i is 0-based, so R i = i + 1. */
        int r_i = i + 1;
        if (r_i > burnin && (r_i - burnin) % thinning == 0 && ki < n_keep) {
            col_py[ki] = py;
            col_pc[ki] = pc;
            col_t [ki] = (double)t;
            col_ll[ki] = f0;
            ki++;
        }

        /* Progress and interrupt checks */
        if ((i + 1) % pct_step == 0) {
            if (!quiet) Rprintf("%.0f%%\n", 100.0 * (i + 1) / mcmc);
        }
        if ((i + 1) % check_step == 0) {
            R_CheckUserInterrupt();
        }
    }

    PutRNGstate();

    if (!quiet) {
        Rprintf("Acceptance rates (py / pc / t):\n");
        Rprintf("%.4f  %.4f  %.4f\n",
                (double)acc_py / mcmc,
                (double)acc_pc / mcmc,
                (double)acc_t  / mcmc);
    }

    /* Column names for par matrix */
    SEXP r_dn   = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP r_cnms = PROTECT(Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(r_cnms, 0, Rf_mkChar("py"));
    SET_STRING_ELT(r_cnms, 1, Rf_mkChar("pc"));
    SET_STRING_ELT(r_cnms, 2, Rf_mkChar("t"));
    SET_STRING_ELT(r_cnms, 3, Rf_mkChar("loglik"));
    SET_VECTOR_ELT(r_dn, 0, R_NilValue);
    SET_VECTOR_ELT(r_dn, 1, r_cnms);
    Rf_setAttrib(r_par, R_DimNamesSymbol, r_dn);

    /* Acceptance rates */
    SEXP r_acc = PROTECT(Rf_allocVector(REALSXP, 3));
    REAL(r_acc)[0] = (double)acc_py / mcmc;
    REAL(r_acc)[1] = (double)acc_pc / mcmc;
    REAL(r_acc)[2] = (double)acc_t  / mcmc;

    /* Return list(par = ..., accept = ...) */
    SEXP r_res = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP r_nms = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_VECTOR_ELT(r_res, 0, r_par);
    SET_VECTOR_ELT(r_res, 1, r_acc);
    SET_STRING_ELT(r_nms, 0, Rf_mkChar("par"));
    SET_STRING_ELT(r_nms, 1, Rf_mkChar("accept"));
    Rf_setAttrib(r_res, R_NamesSymbol, r_nms);

    UNPROTECT(6);
    return r_res;
}

/* Registration handled by delimtools_init.c */
