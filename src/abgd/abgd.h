#ifndef ABGDWEB_H

#define ABGDWEB_H

#define DATE "April 11 2013"

#define SIZE_NAME_DIST 100   /* do not store characters after SIZE_NAME_DIST */
#define WEB_ADMIN "sophie.brouillet@mnhn.fr"

#define DIRWEB "/abi/public/abgd/"
#define WORKDIR "/var/www/html/abi/public/abgd/temp/"
#define WORKDIR_CL ""
#define HOSTNAME "http://bioinfo.mnhn.fr"


struct Peak {

	double Dist;
	double Rank;
	double theta_hat;
};


struct DistanceMatrix {

	long n;            /* number of sequence */
	char **names;      /* store names, at most SIZE_NAME_DIST char */
	double **dist;     /* a 2-d matrix of distance [0, \inf] */
	double ratio_ts_tv;		/*transition/transversion rate*/

};


struct Composante {

	int nc;           /* number of composantes */
	int nn;           /* number of nodes / sequences */
	int nm;           /* number of masked/excluded nodes (with -1 in node_compid) */

	int *node_compid;   /* the comp_id of each node */

	int *n_in_comp;     /* number of nodes in each composante */
	int **comp;         /* a list of node id in each composante */

};

struct FastaSeq {
char *name;
char *seq;
};

#define MINI(a,b) ((a<=b)?a:b)

struct DistanceMatrix GetDistMat (int nseq, struct FastaSeq *mesSeqs, int method, float ts_tv, FILE *f, char *d);
struct Peak find_abgd( double *Array, long N, long windsize_min, long windsize_max, short output_slope, double MaxDist, double SlopeIncrease );
struct Peak FindFirstPeak( double *Array, long N, int winsiz, short output_slope, double *Pi, double MaxDist, double SlopeIncrease );
double *matrix2list( struct DistanceMatrix distmat, char *mask, long *Nval );
void setcomp( int node, int compid, int *node_compid, struct DistanceMatrix matrix, double max_dist, char *mask);
struct Composante compute_node_compid( struct DistanceMatrix matrix, double max_dist, char *mask );
struct Composante extract_composante( struct DistanceMatrix matrix, double max_dist, char *mask );

void distanceTN93 (struct FastaSeq *, int l, struct DistanceMatrix mymat, FILE *f, char *d);
void distanceK80  (struct FastaSeq *, int l, struct DistanceMatrix mymat, FILE *f, char *d);
void distanceJC69 (struct FastaSeq *, int l, struct DistanceMatrix mymat, FILE *f, char *d);

/* exit_properly is static in abgdCore.c */
void html_error(FILE *f, int nb);
int  check_compat(char *s1, char *s2, int l);
int  check_names(struct FastaSeq *mesSeq, int nbseq);
long min_ws( long nval );
void reset_composante( struct Composante *c );
void update_composante( struct Composante *main_comp, int id, struct Composante sub_comp );
void free_composante( struct Composante c );
void free_distmat( struct DistanceMatrix mat );

#endif
