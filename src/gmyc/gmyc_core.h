#ifndef GMYC_CORE_H
#define GMYC_CORE_H

#define MAX_TAXA    4096
#define MAX_NODES   (2 * MAX_TAXA)
#define MAX_LABEL   256
#define NEG_INF     (-1e300)

typedef struct {
    int    id;
    int    parent;
    int    left;
    int    right;
    double branch_len;
    double height;        /* negative: root most negative, tips ~ 0 */
    int    is_tip;
    char   label[MAX_LABEL];
} GNode;

typedef struct {
    GNode  nodes[MAX_NODES];
    int    n_nodes;
    int    n_taxa;
    int    root;
    double *heights;      /* sorted internal heights (malloc'd) */
    int    n_internal;
} GTree;

typedef struct {
    double log_lik_null;
    double log_lik_gmyc;
    double lr_stat;
    double p_value;
    double threshold;
    int    n_clusters;
    int    n_entities;
    int    cluster[MAX_NODES];
    char   tip_labels[MAX_NODES][MAX_LABEL];
    int    n_tips;
    /* Per-threshold output (pre-allocated by caller; length n_thresh = n_internal).
       Caller must initialise thresh_ll[] to NA_REAL and thresh_K[] to NA_INTEGER
       before calling gmyc_core(); gmyc_core() only writes computed positions. */
    double *thresh_ll;       /* GMYC loglik per threshold position */
    int    *thresh_K;        /* K (cluster count) per threshold position */
    int     n_thresh;        /* = n_internal */
    int     stthresh_c;      /* 0-based index of first valid threshold */
    double *thresh_lam_div;  /* lambda.div per threshold */
    double *thresh_lam_coa;  /* lambda.coal per threshold */
    double *thresh_p_div;    /* p.div (spe_p) per threshold */
    double *thresh_p_coa;    /* p.coal (coa_p) per threshold */
} GResult;

/* Sorted branching event (used by GMYCPrep) */
typedef struct { double time; int node_id; } BEvent;

/* ─────────────────────────────────────────────────────────────────────────
 * GMYCPrep: precomputed per-threshold data for fast log-likelihood
 * evaluation from R's optim().  Created by gmyc_prep_init(), freed by
 * gmyc_prep_free().
 * ───────────────────────────────────────────────────────────────────────── */
typedef struct {
    GTree  *tree;          /* heap-allocated copy */
    int     m;             /* n_internal events */
    BEvent *ev;            /* sorted events, length m */
    double *x;             /* inter-event intervals, length m */
    int     stthresh_c;    /* 0-based first valid threshold index */

    /* Per-threshold state (precomputed at init time) */
    int    *nod_type;      /* [m * m]: row=T_idx (0..m-1), col=event idx */
    int    *node_cid;      /* [m * n_nodes]: row=T_idx */
    int    *thresh_K;      /* [m] K per threshold */
    int    *thresh_numSpe; /* [m] numSpe per threshold */

    /* Null model (golden-section, computed at init) */
    double  null_ll;
    double  null_p;
    double  null_lam;
} GMYCPrep;

/* Public API */
int    gtree_from_ape(const int *edge, const double *edge_len,
                      int n_edge, const char **tip_label,
                      int n_tips, int n_nodes_total, GTree *tree);

void   gtree_compute_heights(GTree *tree);  /* fallback, C-only */
void   gtree_free(GTree *tree);
int    gmyc_core(GTree *tree, GResult *result);
double chi2_sf(double x, double df);

/* GMYCPrep API (for R's optim() + C log-likelihood design) */
int    gmyc_prep_init(GMYCPrep *prep, const GTree *tree);
void   gmyc_prep_free(GMYCPrep *prep);
double gmyc_loglik_at(const GMYCPrep *prep, int T_idx,
                      double spe_p, double coa_p,
                      double *lam_spe_out, double *lam_coa_out);

#endif
