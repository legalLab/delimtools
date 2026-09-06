/*
	Header file for main_abgd.c, used by abgd_r.c.
*/

int ReadFastaSequence( FILE *f, struct FastaSeq *laseq);
struct DistanceMatrix compute_dis(FILE *f, int method, float ts_tv);
int myIndex(char *l, char c);
void remplace(char *name, char c, char newc);
void readMatrixMega(FILE *f_in, struct DistanceMatrix *my_mat);
struct DistanceMatrix read_distmat(FILE *f_in, float ts_tv);
double * Compute_myDist( double minDist, double MaxDist, int nbStepsABGD );
