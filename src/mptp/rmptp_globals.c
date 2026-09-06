/*
 * rmptp_globals.c
 *
 * Definitions of all global variables that mPTP scatters across mptp.c.
 * Originally these lived in mptp.c alongside main(), which we cannot
 * include in the shared library.  By moving them here we can compile
 * the library without mptp.c while still satisfying every extern
 * declaration in mptp.h.
 */

#include "mptp.h"

/* command-line string (set by wrapper before each call) */
char * cmdline;

/* global error message buffer */
char errmsg[200] = {0};

/* global pseudo-random number generator 48-bit state */
unsigned short global_xsubi[3];

/* libpll errno */
int pll_errno;

/* --- options (mirroring declarations in mptp.h / mptp.c) --- */
int    opt_quiet;
int    opt_precision;
int    opt_svg_showlegend;
long   opt_help;
long   opt_version;
long   opt_treeshow;
long   opt_method;
long   opt_mcmc_sample;
long   opt_mcmc_steps;
long   opt_mcmc_log;
long   opt_mcmc_startnull;
long   opt_mcmc_startrandom;
long   opt_mcmc_startml;
long   opt_mcmc_burnin;
long   opt_mcmc_runs;
long   opt_seed;
long   opt_mcmc;
long   opt_ml;
long   opt_multi;
long   opt_single;
long   opt_crop;
long   opt_svg;
long   opt_svg_width;
long   opt_svg_fontsize;
long   opt_svg_tipspace;
long   opt_svg_marginleft;
long   opt_svg_marginright;
long   opt_svg_margintop;
long   opt_svg_marginbottom;
long   opt_svg_inner_radius;
double opt_mcmc_credible;
double opt_svg_legend_ratio;
double opt_pvalue;
double opt_minbr;
char * opt_treefile;
char * opt_outfile;
char * opt_outgroup;
char * opt_pdist_file;
