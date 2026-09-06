/*
 * abgd_r.c — Interface R para o ABGD original de G. Achaz
 *
 * O C original (abgdCore.c, main_abgd.c, bionjcabgd.c) é compilado
 * intacto. A flag -Dismodule no Makevars suprime o main() original.
 * Este arquivo expõe abgd_run_call() via .Call() ao R.
 */

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "abgd.h"
#include "main_abgd.h"

#define SIGN(a) (((a)>0)?1:(((a)==0)?0:-1))
static int Increase(const void *v1, const void *v2){
    return (int)SIGN(*((double *)v1) - *((double *)v2));
}
#undef SIGN

/* =========================================================================
 * abgd_run_call — exposta via .Call()
 *
 * Argumentos:
 *   file_r    STRSXP  — caminho do arquivo FASTA ou matriz de distâncias
 *   pmin_r    REALSXP — prior mínimo  (default 0.001)
 *   pmax_r    REALSXP — prior máximo  (default 0.1)
 *   steps_r   INTSXP  — número de steps (default 10)
 *   method_r  INTSXP  — 0=K80 1=JC69 2=TN93 3=simple (default 1)
 *   slope_r   REALSXP — minSlopeIncrease (default 1.5)
 *   tstv_r    REALSXP — ts/tv para K80 (default 2.0)
 *
 * Retorna named list:
 *   $names       character  — nomes das sequências
 *   $dist_matrix numeric    — matriz nseq × nseq
 *   $initial     list       — partição inicial por prior (integer vectors)
 *   $recursive   list       — partição recursiva por prior (integer vectors)
 *   $priors      numeric    — grade de priors usada
 *   $n_init      integer    — n_groups por prior (inicial)
 *   $n_rec       integer    — n_groups por prior (recursivo)
 * ======================================================================= */
SEXP abgd_run_call(SEXP file_r, SEXP pmin_r, SEXP pmax_r,
                   SEXP steps_r, SEXP method_r, SEXP slope_r, SEXP tstv_r)
{
    const char *file    = CHAR(STRING_ELT(file_r, 0));
    double  minDist     = REAL(pmin_r)[0];
    double  MaxDistGlob = REAL(pmax_r)[0];
    int     nbSteps     = INTEGER(steps_r)[0];
    int     imethode    = INTEGER(method_r)[0];
    double  minSlope    = REAL(slope_r)[0];
    float   ts_tv       = (float) REAL(tstv_r)[0];

    /* leitura do arquivo */
    FILE *f = fopen(file, "r");
    if (!f) error("abgd: cannot open file '%s'", file);

    int first = fgetc(f);
    rewind(f);

    struct DistanceMatrix distmat;
    if (first == '>')
        distmat = compute_dis(f, imethode, ts_tv);
    else
        distmat = read_distmat(f, ts_tv);

    fclose(f);

    int n = (int) distmat.n;

    /* grade de priors */
    double *myDist = Compute_myDist(minDist, MaxDistGlob, nbSteps);

    /* alocações auxiliares */
    char *mask     = (char *) malloc(n * sizeof(char));
    int  *specInit = (int *)  malloc(nbSteps * sizeof(int));
    int  *specRec  = (int *)  malloc(nbSteps * sizeof(int));

    int **part_init = (int **) malloc(nbSteps * sizeof(int *));
    int **part_rec  = (int **) malloc(nbSteps * sizeof(int *));
    for (int s = 0; s < nbSteps; s++) {
        part_init[s] = (int *) calloc(n, sizeof(int));
        part_rec[s]  = (int *) calloc(n, sizeof(int));
    }

    /* ── loop principal — fiel ao main_abgd.c ─────────────────────── */
    int nStepsDone = 0;

    for (int myD = 0; myD < nbSteps; myD++) {

        double MaxDist = myDist[myD];

        long NVal = 0;
        for (int j = 0; j < n; j++) mask[j] = 1;
        double *ValArray = matrix2list(distmat, mask, &NVal);
        qsort((void *) ValArray, (size_t) NVal, sizeof(double), Increase);

        long ws_min = min_ws(NVal);
        long ws_max = NVal - 1;

        struct Peak my_abgd = find_abgd(ValArray, NVal, ws_min, ws_max,
                                         0, MaxDist, minSlope);

        if (my_abgd.Rank == NVal + 0.5) {
            specInit[myD] = 1;
            specRec[myD]  = 1;
            for (int i = 0; i < n; i++) {
                part_init[myD][i] = 1;
                part_rec[myD][i]  = 1;
            }
            free(ValArray);
            nStepsDone = myD + 1;
            break;
        }

        /* partição inicial */
        struct Composante comp = extract_composante(distmat, my_abgd.Dist, mask);
        specInit[myD] = comp.nc;
        for (int i = 0; i < n; i++)
            part_init[myD][i] = comp.node_compid[i] + 1;

        /* recursão — fiel ao main_abgd.c */
        int flag = 1;
        while (flag) {
            flag = 0;
            int nc = comp.nc;

            for (int a = 0; a < nc; a++) {
                struct Composante recursive_comp;
                reset_composante(&recursive_comp);

                memset((void *) mask, 0, (size_t) n * sizeof(char));
                for (int b = 0; b < comp.n_in_comp[a]; b++)
                    mask[comp.comp[a][b]] = 1;

                long nval = 0;
                double *vals = matrix2list(distmat, mask, &nval);
                qsort((void *) vals, (size_t) nval, sizeof(double), Increase);

                if (nval > 2) {
                    long rws_min = min_ws(nval);
                    long rws_max = nval - 1;
                    struct Peak rec = find_abgd(vals, nval, rws_min, rws_max,
                                                0, MaxDist, minSlope);

                    if (rec.Rank != nval + 0.5) {
                        recursive_comp = extract_composante(distmat, rec.Dist, mask);
                        if (recursive_comp.nc > 1) {
                            update_composante(&comp, a, recursive_comp);
                            flag = 1;
                        }
                    }
                }
                free(vals);
                free_composante(recursive_comp);
            }
        }

        specRec[myD] = comp.nc;
        for (int i = 0; i < n; i++)
            part_rec[myD][i] = comp.node_compid[i] + 1;

        free(ValArray);
        free_composante(comp);

        nStepsDone = myD + 1;
        if (specRec[myD] == 1) break;
    }

    /* ── monta resultado SEXP ──────────────────────────────────────── */
    int np = nStepsDone;

    SEXP names_r  = PROTECT(allocVector(STRSXP,  n));
    for (int i = 0; i < n; i++)
        SET_STRING_ELT(names_r, i, mkChar(distmat.names[i]));

    SEXP dist_r   = PROTECT(allocMatrix(REALSXP, n, n));
    double *dptr  = REAL(dist_r);
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
            dptr[j * n + i] = distmat.dist[i][j];

    SEXP priors_r = PROTECT(allocVector(REALSXP, np));
    SEXP n_init_r = PROTECT(allocVector(INTSXP,  np));
    SEXP n_rec_r  = PROTECT(allocVector(INTSXP,  np));
    for (int s = 0; s < np; s++) {
        REAL(priors_r)[s]    = myDist[s];
        INTEGER(n_init_r)[s] = specInit[s];
        INTEGER(n_rec_r)[s]  = specRec[s];
    }

    SEXP init_list = PROTECT(allocVector(VECSXP, np));
    SEXP rec_list  = PROTECT(allocVector(VECSXP, np));
    for (int s = 0; s < np; s++) {
        SEXP vi = PROTECT(allocVector(INTSXP, n));
        SEXP vr = PROTECT(allocVector(INTSXP, n));
        for (int i = 0; i < n; i++) {
            INTEGER(vi)[i] = part_init[s][i];
            INTEGER(vr)[i] = part_rec[s][i];
        }
        SET_VECTOR_ELT(init_list, s, vi);
        SET_VECTOR_ELT(rec_list,  s, vr);
        UNPROTECT(2);
    }

    SEXP result    = PROTECT(allocVector(VECSXP, 7));
    SEXP res_nms   = PROTECT(allocVector(STRSXP, 7));
    const char *nm[7] = {"names","dist_matrix","initial","recursive",
                          "priors","n_init","n_rec"};
    for (int i = 0; i < 7; i++)
        SET_STRING_ELT(res_nms, i, mkChar(nm[i]));
    setAttrib(result, R_NamesSymbol, res_nms);

    SET_VECTOR_ELT(result, 0, names_r);
    SET_VECTOR_ELT(result, 1, dist_r);
    SET_VECTOR_ELT(result, 2, init_list);
    SET_VECTOR_ELT(result, 3, rec_list);
    SET_VECTOR_ELT(result, 4, priors_r);
    SET_VECTOR_ELT(result, 5, n_init_r);
    SET_VECTOR_ELT(result, 6, n_rec_r);

    UNPROTECT(9);

    /* libera memória C */
    free_distmat(distmat);
    free(myDist); free(mask); free(specInit); free(specRec);
    for (int s = 0; s < nbSteps; s++) { free(part_init[s]); free(part_rec[s]); }
    free(part_init); free(part_rec);

    return result;
}
