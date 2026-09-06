/*
 * rmptp_wrap.c
 *
 * .Call()-compatible wrapper around the mPTP core routines.
 *
 * Design decisions
 * ----------------
 * 1. fatal() is redefined here (overrides util.c) to call Rf_error()
 *    instead of exit(1), so errors propagate cleanly as R conditions.
 *
 * 2. All meaningful results are extracted from the in-memory tree
 *    (node->event, node->support, node->label) — no files are written
 *    or parsed.  opt_outfile is set but unused after the file-I/O
 *    layer was removed from dp.c / aic.c / multirun.c.
 *
 * 3. Global opt_* variables are set before each call and are not
 *    touched concurrently (R is single-threaded by default).
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Visibility.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "mptp.h"

/* ── re-export the registration table at the bottom ── */

/* ================================================================
 * 1.  fatal() — defined here (util.c version excluded via R_PACKAGE).
 *     Calls Rf_error() so errors propagate as R conditions, not exit().
 * ================================================================ */
void fatal(const char * format, ...)
{
  char buf[512];
  va_list ap;
  va_start(ap, format);
  vsnprintf(buf, sizeof(buf), format, ap);
  va_end(ap);
  Rf_error("mPTP error: %s", buf);
}

/* ================================================================
 * 2.  Helpers: set global opts, walk tree, collect results
 * ================================================================ */

/* Set every opt_* that the core functions read */
static void set_globals(const char * treefile,
                        const char * outfile,
                        const char * outgroup,
                        int          method,       /* 0=single, 1=multi */
                        double       minbr,
                        double       pvalue,
                        int          quiet,
                        long         seed,
                        int          crop,
                        /* MCMC-specific (ignored for ML) */
                        long         mcmc_steps,
                        long         mcmc_sample,
                        long         mcmc_burnin,
                        long         mcmc_runs,
                        double       mcmc_credible)
{
  opt_treefile     = (char *)treefile;
  opt_outfile      = (char *)outfile;
  opt_outgroup     = outgroup ? (char *)outgroup : NULL;
  opt_method       = method;
  opt_minbr        = minbr;
  opt_pvalue       = pvalue;
  opt_quiet        = quiet;
  opt_seed         = seed;
  opt_crop         = crop;
  opt_ml           = 0;
  opt_mcmc         = 0;
  opt_multi        = (method == PTP_METHOD_MULTI) ? 1 : 0;
  opt_single       = (method == PTP_METHOD_SINGLE) ? 1 : 0;
  opt_treeshow     = 0;
  opt_svg          = 0;
  opt_precision    = 7;
  opt_mcmc_steps   = mcmc_steps;
  opt_mcmc_sample  = mcmc_sample;
  opt_mcmc_burnin  = mcmc_burnin;
  opt_mcmc_runs    = mcmc_runs;
  opt_mcmc_credible = mcmc_credible;
  opt_mcmc_log     = 0;
  opt_mcmc_startnull   = 0;
  opt_mcmc_startrandom = 0;
  opt_mcmc_startml     = 0;
  opt_pdist_file   = NULL;
  cmdline          = (char *)"rmptp";

  /* Initialize RNG */
  random_init(global_xsubi, seed);
}

/*
 * build_assignments_ml()
 *
 * Walks the tree after dp_ptp() / backtrack() has set node->event on every
 * inner node.  A new species begins whenever we enter a coalescent subtree
 * from a speciation node (or from the root).  Coalescent inner nodes that
 * are *inside* a coalescent subtree do NOT start a new species — they are
 * just internal structure within the same group.
 *
 * This mirrors exactly what backtrack() does: it recurses only into
 * SPECIATION nodes and treats each COALESCENT subtree as one atomic group.
 */
static SEXP build_assignments_ml(rtree_t * root)
{
  int n_tips = root->leaves;

  SEXP taxon_vec   = PROTECT(Rf_allocVector(STRSXP, n_tips));
  SEXP species_vec = PROTECT(Rf_allocVector(INTSXP, n_tips));

  /* Stack entry: (node, species_id, inside_coalescent_subtree) */
  typedef struct { rtree_t *node; int sp_id; int in_coal; } Frame;
  Frame *stack = (Frame *)malloc((size_t)(2 * n_tips) * sizeof(Frame));
  if (!stack) Rf_error("rmptp: out of memory in build_assignments_ml");

  int top = 0, pos = 0, sp_counter = 0;
  stack[top++] = (Frame){ root, 0, 0 };

  while (top > 0) {
    Frame f    = stack[--top];
    rtree_t *node = f.node;

    if (!node->left && !node->right) {
      /* tip: record species assignment.
       * Singleton species: the tip IS the coalescent root (backtrack set
       * EVENT_COALESCENT on it but never recursed further).  The parent was
       * a speciation node, so in_coal==0 here — we must open a new species
       * instead of inheriting sp_id=0 from the frame. */
      int tip_sp = f.sp_id;
      if (node->event == EVENT_COALESCENT && !f.in_coal) {
        sp_counter++;
        tip_sp = sp_counter;
      }
      SET_STRING_ELT(taxon_vec, pos,
                     Rf_mkChar(node->label ? node->label : ""));
      INTEGER(species_vec)[pos] = tip_sp;
      pos++;
      continue;
    }

    int cur_sp   = f.sp_id;
    int in_coal  = f.in_coal;

    if (node->event == EVENT_COALESCENT && !in_coal) {
      /* Entering a coalescent subtree from outside — new species */
      sp_counter++;
      cur_sp  = sp_counter;
      in_coal = 1;          /* descendants stay in this species */
    }
    /* If EVENT_SPECIATION: reset coal flag so children can start new species */
    if (node->event == EVENT_SPECIATION) {
      in_coal = 0;
    }

    /* Push children */
    stack[top++] = (Frame){ node->right, cur_sp, in_coal };
    stack[top++] = (Frame){ node->left,  cur_sp, in_coal };
  }

  free(stack);

  /* Build named list */
  SEXP lst   = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("taxon"));
  SET_STRING_ELT(names, 1, Rf_mkChar("species"));
  SET_VECTOR_ELT(lst, 0, taxon_vec);
  SET_VECTOR_ELT(lst, 1, species_vec);
  Rf_setAttrib(lst, R_NamesSymbol, names);

  UNPROTECT(4);
  return lst;
}

/*
 * Collect per-inner-node support values and labels after multirun().
 * Returns list(node_label = character(), support = double())
 */
static SEXP build_support(rtree_t * root)
{
  int n_inner = root->leaves - 1;

  rtree_t ** node_list =
    (rtree_t **)malloc((size_t)n_inner * sizeof(rtree_t *));
  if (!node_list) Rf_error("rmptp: out of memory in build_support");

  int count = rtree_query_innernodes(root, node_list);

  SEXP label_vec   = PROTECT(Rf_allocVector(STRSXP,  count));
  SEXP support_vec = PROTECT(Rf_allocVector(REALSXP, count));

  char fallback[32];
  for (int i = 0; i < count; i++) {
    const char *lbl = node_list[i]->label;
    if (!lbl || !*lbl) {
      snprintf(fallback, sizeof(fallback), "Inner_%d", i + 1);
      lbl = fallback;
    }
    SET_STRING_ELT(label_vec, i, Rf_mkChar(lbl));
    REAL(support_vec)[i] = node_list[i]->support;
  }

  free(node_list);

  SEXP lst   = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("node_label"));
  SET_STRING_ELT(names, 1, Rf_mkChar("support"));
  SET_VECTOR_ELT(lst, 0, label_vec);
  SET_VECTOR_ELT(lst, 1, support_vec);
  Rf_setAttrib(lst, R_NamesSymbol, names);

  UNPROTECT(4);
  return lst;
}

/* ================================================================
 * 3.  Load tree helper (mirrors load_tree() from mptp.c but is
 *     callable from the wrapper without touching argc/argv)
 * ================================================================ */
static rtree_t * rmptp_load_tree(void)
{
  rtree_t * rtree = rtree_parse_newick(opt_treefile);

  if (!rtree) {
    unsigned int tip_count;
    utree_t * utree = utree_parse_newick(opt_treefile, &tip_count);
    if (!utree)
      Rf_error("mPTP: tree is neither rooted nor unrooted Newick.");

    utree_t * og_root = NULL;
    if (!opt_outgroup)
      og_root = utree_longest_branchtip(utree, tip_count);
    else {
      og_root = utree_outgroup_lca(utree, tip_count);
      if (!og_root) {
        utree_destroy(utree);
        Rf_error("mPTP: outgroup not found or is not a monophyletic tip set.");
      }
    }

    rtree = opt_crop ? utree_crop(og_root)
                     : utree_convert_rtree(og_root);
    utree_destroy(utree);
  } else {
    if (opt_crop) {
      if (!opt_outgroup)
        Rf_error("mPTP: --outgroup must be specified with outgroup_crop.");
      rtree_t * og_root = get_outgroup_lca(rtree);
      rtree = rtree_crop(rtree, og_root);
      if (!rtree)
        Rf_error("mPTP: cropping the outgroup leaves fewer than 2 tips.");
    }
  }
  return rtree;
}

/* ================================================================
 * 4.  .Call() entry point — ML
 *
 *  Arguments (all SEXP):
 *    tree_file, outfile_base, outgroup, method_int,
 *    minbr, pvalue, quiet, seed, crop
 *
 *  Returns an R list with all results in memory.
 * ================================================================ */
SEXP rmptp_ml(SEXP tree_file_r,
              SEXP outfile_r,
              SEXP outgroup_r,
              SEXP method_r,
              SEXP minbr_r,
              SEXP pvalue_r,
              SEXP quiet_r,
              SEXP seed_r,
              SEXP crop_r)
{
  const char * treefile = CHAR(STRING_ELT(tree_file_r, 0));
  const char * outfile  = CHAR(STRING_ELT(outfile_r,   0));
  const char * outgroup = (Rf_isNull(outgroup_r) || !Rf_length(outgroup_r))
                            ? NULL
                            : CHAR(STRING_ELT(outgroup_r, 0));
  int    method  = INTEGER(method_r)[0];
  double minbr   = REAL(minbr_r)[0];
  double pval    = REAL(pvalue_r)[0];
  int    quiet   = INTEGER(quiet_r)[0];
  long   seed    = (long)REAL(seed_r)[0];
  int    crop    = INTEGER(crop_r)[0];

  set_globals(treefile, outfile, outgroup, method, minbr, pval,
              quiet, seed, crop,
              0, 0, 0, 1, 0.95);

  opt_ml   = 1;
  opt_mcmc = 0;

  /* Load and initialise tree */
  rtree_t * rtree = rmptp_load_tree();

  dp_init(rtree);
  dp_set_pernode_spec_edges(rtree);

  dp_ptp(rtree, method);

  /* ── Collect results from in-memory tree ── */

  /* Derive best_logl, pvalue, lrt_pass from the filled DP table */
  double       best_logl  = 0;
  double       pvalue_res = -1;
  int          lrt_pass   = 0;
  unsigned int n_sp_dp    = 0;
  dp_get_stats(rtree, method, &best_logl, &pvalue_res, &lrt_pass, &n_sp_dp);
  /* When LRT fails backtrack() is never called; the null model (1 species)
   * is the reported result, matching the executable's behaviour. */
  if (!lrt_pass) n_sp_dp = 1;

  double null_logl   = rtree->coal_logl;
  int    edge_count  = rtree->edge_count;
  int    total_edges = 2 * rtree->leaves - 2;

  /* Species assignments via node->event (set by backtrack inside dp_ptp) */
  SEXP assignments = PROTECT(build_assignments_ml(rtree));

  dp_free(rtree);
  rtree_destroy(rtree);

  /* ── Build return list ── */
  const char * field_names[] = {
    "assignments", "n_species",
    "null_logl",   "best_logl",  "pvalue",
    "lrt_passed",  "edge_count", "total_edges",
    ""
  };
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, field_names));

  SET_VECTOR_ELT(result, 0, assignments);
  SET_VECTOR_ELT(result, 1, Rf_ScalarInteger((int)n_sp_dp));
  SET_VECTOR_ELT(result, 2, Rf_ScalarReal(null_logl));
  SET_VECTOR_ELT(result, 3, Rf_ScalarReal(best_logl));
  SET_VECTOR_ELT(result, 4, Rf_ScalarReal(pvalue_res));
  SET_VECTOR_ELT(result, 5, Rf_ScalarLogical(lrt_pass));
  SET_VECTOR_ELT(result, 6, Rf_ScalarInteger(edge_count));
  SET_VECTOR_ELT(result, 7, Rf_ScalarInteger(total_edges));

  UNPROTECT(2);
  return result;
}

/* ================================================================
 * 5.  .Call() entry point — MCMC
 *
 *  Returns an R list with support values (per inner node) and the
 *  ML assignments from the internal mltree.
 * ================================================================ */
SEXP rmptp_mcmc(SEXP tree_file_r,
                SEXP outfile_r,
                SEXP outgroup_r,
                SEXP method_r,
                SEXP minbr_r,
                SEXP pvalue_r,
                SEXP quiet_r,
                SEXP seed_r,
                SEXP crop_r,
                SEXP mcmc_steps_r,
                SEXP mcmc_sample_r,
                SEXP mcmc_burnin_r,
                SEXP mcmc_runs_r,
                SEXP mcmc_credible_r,
                SEXP mcmc_startnull_r,
                SEXP mcmc_startrandom_r,
                SEXP mcmc_startml_r)
{
  const char * treefile = CHAR(STRING_ELT(tree_file_r, 0));
  const char * outfile  = CHAR(STRING_ELT(outfile_r,   0));
  const char * outgroup = (Rf_isNull(outgroup_r) || !Rf_length(outgroup_r))
                            ? NULL
                            : CHAR(STRING_ELT(outgroup_r, 0));
  int    method         = INTEGER(method_r)[0];
  double minbr          = REAL(minbr_r)[0];
  double pval           = REAL(pvalue_r)[0];
  int    quiet          = INTEGER(quiet_r)[0];
  long   seed           = (long)REAL(seed_r)[0];
  int    crop           = INTEGER(crop_r)[0];
  long   mcmc_steps     = (long)REAL(mcmc_steps_r)[0];
  long   mcmc_sample    = (long)INTEGER(mcmc_sample_r)[0];
  long   mcmc_burnin    = (long)REAL(mcmc_burnin_r)[0];
  long   mcmc_runs      = (long)INTEGER(mcmc_runs_r)[0];
  double mcmc_credible  = REAL(mcmc_credible_r)[0];

  set_globals(treefile, outfile, outgroup, method, minbr, pval,
              quiet, seed, crop,
              mcmc_steps, mcmc_sample, mcmc_burnin, mcmc_runs, mcmc_credible);

  opt_mcmc             = mcmc_steps;
  opt_ml               = 0;
  opt_mcmc_startnull   = INTEGER(mcmc_startnull_r)[0];
  opt_mcmc_startrandom = INTEGER(mcmc_startrandom_r)[0];
  opt_mcmc_startml     = INTEGER(mcmc_startml_r)[0];

  /* Load tree */
  rtree_t * rtree = rmptp_load_tree();

  multirun(rtree, method);

  /* ── Collect results ── */

  /* Support values on inner nodes (set by multirun on rtree) */
  SEXP support_lst = PROTECT(build_support(rtree));

  /* ML species assignments: re-run ML on rtree so node->event is set. */
  dp_init(rtree);
  dp_set_pernode_spec_edges(rtree);
  dp_ptp(rtree, method);

  SEXP assignments = PROTECT(build_assignments_ml(rtree));

  /* Derive best_logl, pvalue, lrt_pass */
  double       best_logl  = 0;
  double       pvalue_res = -1;
  int          lrt_pass   = 0;
  unsigned int n_sp_dp    = 0;
  dp_get_stats(rtree, method, &best_logl, &pvalue_res, &lrt_pass, &n_sp_dp);
  if (!lrt_pass) n_sp_dp = 1;

  double null_logl   = rtree->coal_logl;
  int    edge_count  = rtree->edge_count;
  int    total_edges = 2 * rtree->leaves - 2;
  long   used_seed   = opt_seed;

  dp_free(rtree);
  rtree_destroy(rtree);

  /* ── Build return list ── */
  const char * field_names[] = {
    "assignments", "n_species",
    "null_logl",   "best_logl",  "pvalue",
    "lrt_passed",  "edge_count", "total_edges",
    "support",     "seed",
    ""
  };
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, field_names));

  SET_VECTOR_ELT(result, 0, assignments);
  SET_VECTOR_ELT(result, 1, Rf_ScalarInteger((int)n_sp_dp));
  SET_VECTOR_ELT(result, 2, Rf_ScalarReal(null_logl));
  SET_VECTOR_ELT(result, 3, Rf_ScalarReal(best_logl));
  SET_VECTOR_ELT(result, 4, Rf_ScalarReal(pvalue_res));
  SET_VECTOR_ELT(result, 5, Rf_ScalarLogical(lrt_pass));
  SET_VECTOR_ELT(result, 6, Rf_ScalarInteger(edge_count));
  SET_VECTOR_ELT(result, 7, Rf_ScalarInteger(total_edges));
  SET_VECTOR_ELT(result, 8, support_lst);
  SET_VECTOR_ELT(result, 9, Rf_ScalarReal((double)used_seed));

  UNPROTECT(3);
  return result;
}

/* ── Lex fatal error handler ─────────────────────────────────────────────────
 * Invoked via the YY_FATAL_ERROR macro (defined in PKG_CPPFLAGS in Makevars)
 * instead of the default handler that calls exit().
 */
void rmptp_lex_fatal_error(const char * msg)
{
  Rf_error("mPTP lexer error: %s", msg);
}
