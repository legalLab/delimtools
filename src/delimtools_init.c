#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* ── abgd ──────────────────────────────────────────────────────── */
extern SEXP abgd_run_call(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

/* ── asap ──────────────────────────────────────────────────────── */
extern SEXP asap_run_call(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP asap_dist_call(SEXP, SEXP, SEXP);

/* ── gmyc ──────────────────────────────────────────────────────── */
extern SEXP C_gmyc(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_gmyc_setup(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_gmyc_loglik(SEXP, SEXP, SEXP);
extern SEXP C_gmyc_loglik_params(SEXP, SEXP, SEXP);
extern SEXP C_gmyc_clusters(SEXP, SEXP);
extern SEXP C_gmyc_threshold_data(SEXP, SEXP);
extern SEXP C_bgmyc_mcmc(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

/* ── mptp ──────────────────────────────────────────────────────── */
extern SEXP rmptp_ml(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rmptp_mcmc(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                       SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"abgd_run_call",         (DL_FUNC) &abgd_run_call,         7},
    {"asap_run_call",         (DL_FUNC) &asap_run_call,         7},
    {"asap_dist_call",        (DL_FUNC) &asap_dist_call,        3},
    {"C_gmyc",                (DL_FUNC) &C_gmyc,                6},
    {"C_gmyc_setup",          (DL_FUNC) &C_gmyc_setup,          6},
    {"C_gmyc_loglik",         (DL_FUNC) &C_gmyc_loglik,         3},
    {"C_gmyc_loglik_params",  (DL_FUNC) &C_gmyc_loglik_params,  3},
    {"C_gmyc_clusters",       (DL_FUNC) &C_gmyc_clusters,       2},
    {"C_gmyc_threshold_data", (DL_FUNC) &C_gmyc_threshold_data, 2},
    {"C_bgmyc_mcmc",         (DL_FUNC) &C_bgmyc_mcmc,          8},
    {"rmptp_ml",              (DL_FUNC) &rmptp_ml,              9},
    {"rmptp_mcmc",            (DL_FUNC) &rmptp_mcmc,           17},
    {NULL, NULL, 0}
};

void R_init_delimtools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
