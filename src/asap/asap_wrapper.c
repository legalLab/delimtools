/*
 * asap_wrapper.c
 * --------------
 * Interface mínima entre R (.Call) e o algoritmo ASAP original.
 *
 * Todo o código do algoritmo (asap_core.c, asap_common.c) é compilado
 * sem alterações. Este ficheiro apenas:
 *   1. Fornece substitutos para unirandom/poissondev/exponentialdev
 *      que usam o RNG do R (para reprodutibilidade com set.seed()).
 *   2. Inicializa as estruturas que o main() do original faria.
 *   3. Executa do_agglutine() e devolve os resultados a R.
 *   4. Calcula o ASAP-score exactamente como asap.c original:
 *        score = rank_pente*(1-pond_score) + rank_proba*pond_score
 *
 * Função exportada: asap_run_call(dist_vec, n, len_seq, replicates,
 *                                 seuil_pvalue, pond_pente, pond_score)
 */

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

/* Rinternals.h define: #define elt Rf_elt
 * asap.h define:       typedef struct Elt { ... } elt;
 * Conflito resolvido removendo o macro antes de incluir asap.h */
#ifdef elt
#undef elt
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* ---- Headers originais ---- */
#include "asap.h"
#include "asap_core.h"
#include <ctype.h>

/* =========================================================================
 * compare_DNA — cópia fiel de oldfns.c (linha 252)
 * Usada em asap_dist_call abaixo
 * ======================================================================= */
static short compare_DNA_r(char s1, char s2)
{
    if ((s1=='A'||s1=='C'||s1=='G'||s1=='T') &&
        (s2=='A'||s2=='C'||s2=='G'||s2=='T'))
        return (s1 == s2) ? 1 : 0;

    if (s1=='-'||s2=='-'||s1=='+'||s2=='+') {
        if ((s1=='-'&&s2=='+')||(s2=='-'&&s1=='+')) return 0;
        return 1;
    }
    if (s1=='N'||s2=='N') return 1;

    if (s1=='A') return (s2=='M'||s2=='R'||s2=='W'||s2=='V'||s2=='H'||s2=='D') ? 1 : 0;
    if (s2=='A') return (s1=='M'||s1=='R'||s1=='W'||s1=='V'||s1=='H'||s1=='D') ? 1 : 0;
    if (s1=='C') return (s2=='M'||s2=='S'||s2=='Y'||s2=='V'||s2=='H'||s2=='B') ? 1 : 0;
    if (s2=='C') return (s1=='M'||s1=='S'||s1=='Y'||s1=='V'||s1=='H'||s1=='B') ? 1 : 0;
    if (s1=='G') return (s2=='R'||s2=='S'||s2=='K'||s2=='V'||s2=='D'||s2=='B') ? 1 : 0;
    if (s2=='G') return (s1=='R'||s1=='S'||s1=='K'||s1=='V'||s1=='D'||s1=='B') ? 1 : 0;
    if (s1=='T') return (s2=='W'||s2=='Y'||s2=='K'||s2=='H'||s2=='D'||s2=='B') ? 1 : 0;
    if (s2=='T') return (s1=='W'||s1=='Y'||s1=='K'||s1=='H'||s1=='D'||s1=='B') ? 1 : 0;

    if ((s1=='M'&&s2=='K')||(s1=='K'&&s2=='M')) return 0;
    if ((s1=='R'&&s2=='Y')||(s1=='Y'&&s2=='R')) return 0;
    if ((s1=='W'&&s2=='S')||(s1=='S'&&s2=='W')) return 0;
    return 1;
}

/* =========================================================================
 * asap_dist_call — cálculo de distâncias em C via .Call()
 *
 * Implementa distancesimple() de oldfns.c:
 *   d(a,b) = v / (L - ncor)
 * onde v = nº de diferenças entre bases ACGT, ncor = gaps/N
 * (sem correcção de Laplace — fiel ao original)
 *
 * Argumentos:
 *   seq_ints_r  INTSXP  vetor plano de códigos ASCII, nseq × L (row-major)
 *   nseq_r      INTSXP  número de sequências
 *   L_r         INTSXP  comprimento das sequências
 *
 * Retorna REALSXP com nseq*(nseq-1)/2 distâncias (triangular inferior,
 * mesma ordem que .mat_to_lower_vec no R: i>j, i*(i-1)/2+j base 0)
 * ======================================================================= */
SEXP asap_dist_call(SEXP seq_ints_r, SEXP nseq_r, SEXP L_r)
{
    int  n   = INTEGER(nseq_r)[0];
    int  L   = INTEGER(L_r)[0];
    int *seq = INTEGER(seq_ints_r);

    int npairs = n * (n - 1) / 2;
    SEXP dist_r = PROTECT(allocVector(REALSXP, npairs));
    double *dist = REAL(dist_r);

    int k = 0;
    for (int a = 1; a < n; a++) {
        for (int b = 0; b < a; b++) {
            double v    = 0.0;
            int    ncor = 0;
            for (int i = 0; i < L; i++) {
                char c1 = (char) toupper(seq[a * L + i]);
                char c2 = (char) toupper(seq[b * L + i]);
                if (compare_DNA_r(c1, c2) == 0) v += 1.0;
                if (c1=='-'||c2=='-'||c1=='N'||c2=='N'||
                    c1=='+'||c2=='+') ncor++;
            }
            int denom = L - ncor;
            dist[k++] = (denom > 0) ? v / (double)denom : 1.0;
        }
    }

    UNPROTECT(1);
    return dist_r;
}

/* =========================================================================
 * RNG: canonical definitions using R's RNG for reproducibility with set.seed().
 * unirandom/exponentialdev/poissondev were removed from asap_common.c;
 * these are the sole definitions, so no --wrap linker trick is needed.
 * ======================================================================= */

double unirandom(void)
{
    double u;
    do { u = unif_rand(); } while (u == 0.0 || u >= 1.0);
    return u;
}

double exponentialdev(void)
{
    double u;
    do { u = unif_rand(); } while (u == 0.0);
    return -log(u);
}

double poissondev(double mean)
{
    return (double) rpois(mean);
}

/* =========================================================================
 * Helpers internos
 * ======================================================================= */

/* Índice triangular inferior (i > j): posição no vetor plano */
static inline double get_dist_v(const double *dv, int n, int i, int j)
{
    if (i == j) return 0.0;
    if (i < j)  { int t = i; i = j; j = t; }
    /* linha i, coluna j → offset = i*(i-1)/2 + j   (base 0) */
    return dv[(long)i * (i - 1) / 2 + j];
}

/* =========================================================================
 * asap_run_call — ponto de entrada via .Call()
 *
 * Argumentos:
 *   dist_vec_r   REALSXP  vetor triangular inferior, n*(n-1)/2 valores
 *                         ordem: (1,0),(2,0),(2,1),(3,0),...  (i>j)
 *   n_r          INTSXP   número de sequências
 *   len_seq_r    INTSXP   comprimento das sequências
 *   replicates_r INTSXP   réplicas coalescentes (padrão original: 1000)
 *   seuil_r      REALSXP  p-value threshold (padrão original: 0.001)
 *   pond_pente_r REALSXP  ponderação do slope (padrão original: 0.1)
 *   pond_score_r REALSXP  ponderação p-valor no score (padrão: 0.5)
 *
 * Retorna VECSXP com 10 vectores nomeados (uma linha por partição,
 * ordenada por rank_general ascendente = ASAP-score crescente):
 *   rank, nbgroups, nbspecRec, dist, d_jump,
 *   proba, intra, inter, slope, asap_score
 * ======================================================================= */
SEXP asap_run_call(SEXP dist_vec_r, SEXP n_r, SEXP len_seq_r,
                   SEXP replicates_r, SEXP seuil_r,
                   SEXP pond_pente_r, SEXP pond_score_r)
{
    int    n          = INTEGER(n_r)[0];
    int    len_seq    = INTEGER(len_seq_r)[0];
    int    replicates = INTEGER(replicates_r)[0];
    double seuil      = REAL(seuil_r)[0];
    double pond_pente = REAL(pond_pente_r)[0];
    double pond_score = REAL(pond_score_r)[0];
    double *dv        = REAL(dist_vec_r);

    int i, j;

    /* ---- Monta DistMat como o original espera ---- */
    DistMat mat;
    mat.n = (long)n;
    mat.ratio_ts_tv = 2.0;

    /* nomes fictícios (não usados na análise, só no output gráfico) */
    mat.names = (char **)malloc(sizeof(char *) * n);
    for (i = 0; i < n; i++) {
        mat.names[i] = (char *)malloc(16);
        sprintf(mat.names[i], "s%d", i + 1);
    }

    /* Matriz 2D de distâncias (original usa dist[i][j]) */
    mat.dist = (double **)malloc(sizeof(double *) * n);
    for (i = 0; i < n; i++) {
        mat.dist[i] = (double *)calloc(n, sizeof(double));
        for (j = 0; j < i; j++) {
            double d = get_dist_v(dv, n, i, j);
            mat.dist[i][j] = d;
            mat.dist[j][i] = d;
        }
    }

    /* ---- Parâmetros (espelhando asap.c main()) ---- */
    Parameter asap_param;
    asap_param.lenSeq       = len_seq;
    asap_param.replicates   = replicates;
    asap_param.seuil_pvalue = (float)seuil;
    asap_param.pond_pente   = (float)pond_pente;
    asap_param.pond_score   = (float)pond_score;
    asap_param.onlyspart    = 1;          /* suprime output de ficheiros */
    asap_param.fres         = stderr;
    asap_param.f_out        = stderr;
    asap_param.ledir        = (char *)"";
    asap_param.nbpairs      = n * (n - 1) / 2;
    asap_param.web          = 0;
    asap_param.fit_to_page  = 0;

    /* ---- ListDistance (mattolist) ---- */
    DistPair *ListDistance = (DistPair *)malloc(
        sizeof(DistPair) * asap_param.nbpairs);
    float maxDist = 0.0f, minDist = 0.0f;
    mattolist(ListDistance, &mat, &maxDist, &minDist);

    /* ---- Inicializa Composante ---- */
    Composante comp;
    initcomp(&comp, n, stderr, (char *)"");

    /* ---- Tabcompo ---- */
    Tabcompo *strucompo = (Tabcompo *)malloc(sizeof(Tabcompo) * n);
    inittabcompo(strucompo, n, stderr, (char *)"");

    /* ---- Nodes ---- */
    int nbnodesmax = (2 * n) - 1;
    Node *zenodes = (Node *)malloc(sizeof(Node) * nbnodesmax);
    initNodes(stderr, zenodes, mat, (char *)"");

    int *no_node = (int *)malloc(sizeof(int) * n);
    for (i = 0; i < n; i++) no_node[i] = i;   /* original: no_node[i] = i (linha 934) */
    int  last_node = n - 1;                    /* original: last_node = mat.n - 1 (linha 949) */
    double best_score = 0.0;
    int    firstpart  = 0;

    /* ---- Scores ---- */
    /* original: malloc(mat.n) sem init, mas listNodes é acedido em compo_rspecie
     * antes de ser alocado em do_agglutine → inicializamos a NULL para segurança */
    /* Aloca 4*n para evitar realloc interno de do_agglutine */
    int scores_max = 4 * n + 2;
    Results *scores = (Results *)malloc(sizeof(Results) * scores_max);
    memset(scores, 0, sizeof(Results) * scores_max);
    for (i = 0; i < scores_max; i++) {
        scores[i].listNodes  = (int *)malloc(sizeof(int) * n);
        scores[i].eff_groups = (int *)calloc(n, sizeof(int));
        scores[i].proba_part = NULL;
    }

    /* ---- RNG do R ---- */
    GetRNGstate();

    /* ---- Executa o algoritmo original sem alterações ---- */
    int nbresults = do_agglutine(mat, &comp, ListDistance, scores,
                                 strucompo, &best_score, &firstpart,
                                 zenodes, no_node, &last_node, asap_param);

    PutRNGstate();

    /* ---- Calcula ASAP-score exactamente como asap.c (linhas 973-993) ----
     *
     * 1. Ordena por proba ascendente  → rank_proba
     * 2. Ordena por slope descendente → rank_pente
     * 3. score = rank_pente*(1-pond_score) + rank_proba*pond_score
     * 4. Ordena por score ascendente  → rank_general
     * ------------------------------------------------------------------- */
    qsort(scores, nbresults, sizeof(Results), compareProba);
    for (i = 0; i < nbresults; i++) scores[i].rank_proba = i + 1;

    qsort(scores, nbresults, sizeof(Results), compareParameter);
    for (i = 0; i < nbresults; i++) scores[i].rank_pente = i + 1;

    for (i = 0; i < nbresults; i++)
        scores[i].score = scores[i].rank_pente * (1.0 - pond_score)
                        + scores[i].rank_proba * pond_score;

    qsort(scores, nbresults, sizeof(Results), compareRang);
    for (i = 0; i < nbresults; i++) scores[i].rank_general = i + 1;

    /* ---- Monta VECSXP de retorno ---- */
    static const char *nms[] = {
        "rank","nbgroups","nbspecRec","dist","d_jump",
        "proba","intra","inter","slope","asap_score", ""
    };

    SEXP out      = PROTECT(mkNamed(VECSXP, nms));
    SEXP rank_r   = PROTECT(allocVector(INTSXP,  nbresults));
    SEXP nbg_r    = PROTECT(allocVector(INTSXP,  nbresults));
    SEXP nbrec_r  = PROTECT(allocVector(INTSXP,  nbresults));
    SEXP dist_r2  = PROTECT(allocVector(REALSXP, nbresults));
    SEXP djump_r  = PROTECT(allocVector(REALSXP, nbresults));
    SEXP proba_r  = PROTECT(allocVector(REALSXP, nbresults));
    SEXP intra_r  = PROTECT(allocVector(REALSXP, nbresults));
    SEXP inter_r  = PROTECT(allocVector(REALSXP, nbresults));
    SEXP slope_r  = PROTECT(allocVector(REALSXP, nbresults));
    SEXP ascore_r = PROTECT(allocVector(REALSXP, nbresults));

    for (i = 0; i < nbresults; i++) {
        INTEGER(rank_r)[i]   = scores[i].rank_general;
        INTEGER(nbg_r)[i]    = scores[i].nbgroups;
        INTEGER(nbrec_r)[i]  = scores[i].nbspecRec;
        REAL(dist_r2)[i]     = scores[i].d;
        REAL(djump_r)[i]     = scores[i].d_jump;
        REAL(proba_r)[i]     = scores[i].proba;
        REAL(intra_r)[i]     = scores[i].intra;
        REAL(inter_r)[i]     = scores[i].inter;
        REAL(slope_r)[i]     = scores[i].other_parameter;
        REAL(ascore_r)[i]    = scores[i].score;
    }

    SET_VECTOR_ELT(out, 0, rank_r);
    SET_VECTOR_ELT(out, 1, nbg_r);
    SET_VECTOR_ELT(out, 2, nbrec_r);
    SET_VECTOR_ELT(out, 3, dist_r2);
    SET_VECTOR_ELT(out, 4, djump_r);
    SET_VECTOR_ELT(out, 5, proba_r);
    SET_VECTOR_ELT(out, 6, intra_r);
    SET_VECTOR_ELT(out, 7, inter_r);
    SET_VECTOR_ELT(out, 8, slope_r);
    SET_VECTOR_ELT(out, 9, ascore_r);

    UNPROTECT(11);

    /* ---- Liberta memória ---- */
    for (i = 0; i < scores_max; i++) {
        if (scores[i].listNodes)  free(scores[i].listNodes);
        if (scores[i].eff_groups) free(scores[i].eff_groups);
        if (scores[i].proba_part) free(scores[i].proba_part);
    }
    free(scores);

    for (i = 0; i < nbnodesmax; i++)
        if (zenodes[i].desc) free(zenodes[i].desc);
    free(zenodes);

    free(no_node);
    free(ListDistance);
    freecomp(&comp, n);
    for (i = 0; i < n; i++) {
        free(strucompo[i].effcompo);
        free(strucompo[i].nodecompo);
    }
    free(strucompo);
    for (i = 0; i < n; i++) {
        free(mat.dist[i]);
        free(mat.names[i]);
    }
    free(mat.dist);
    free(mat.names);

    return out;
}
