#ifndef ASAP_H
#define ASAP_H
#define DIR_TEMP "temp"
#define DATE ""


#define WORKDIR ""

/*if more than MAXSEQ4NEWICK then the newick string will not be computed (but all partitions will be computed) */
#define MAXSEQ4NEWICK 3000
#define SIZE_NAME_DIST 100   /* do not store characters after SIZE_NAME_DIST */
#define WEB_ADMIN "sophie.brouillet@mnhn.fr"
#define MAXSEQTOT 10000
#define MINI(a,b) ((a<=b)?a:b)
#define SIZEOFTEXT 20
#define WIDTHCLADO 3500
#define MARGECLADO 120
#define HAUTEURCOURBE 540
#define MAXSPECIESGRAPH 80
typedef  struct composante {

	int nc;             /* numnber of composantes = number of groups  */
	int nn;             /* number of  sequences */
	int nm;             /* number of masked/excluded nodes (with -1 in node_compid) */

	int *node_compid;   /* the comp_id of each seq  */

	int *n_in_comp;     /* number  in each group */
	int **comp;         /* a list of seq id in each group */
	double *Sall_in_comp;  /* sum of all distances between all pairs of sequences within the group */
//	int **oldcomp;
	int *altered_comp;         /* all groups that have been altered this round -- composantes newly formed by merging-- */
	int **altered_comp_prev;   /* a list of all comp that was merged into this comp */
	
	int naltered; 	    /* nb of altered groups */
	char *altered; 	    /* an array of 0, one for each group. Becomes temporarily 1 when the group has just changed */
	
}	Composante;


//another way to store distance matrix
typedef struct distance {

	int a;          /* id of seq 1 */
	int b;          /* id of seq 2 */
	double d;	/* distance between a and b */
	int g;	        /* to be defined ?? CHECK ?? */

} DistPair;

typedef struct node {
	int anc; 		// Ancestor number in the node array
	int *desc; 		// Descendnats numbers in the node array
	long int x;		// x Graphical position of the node *maybe useless*
	long int y;		// y Graphical position of the node 
	long int xx;	//  x Graphical position of the box
	long int yy;	// y Graphical position of the box 
	int round;		// Round for node creation **redumdant with dist 
	int nbgroups; 	// Nbr of groups if spltting at this node
	int color;  	// indicating proba
//	int nbgspec;
//	int nbgspecRec;
	double dist; 	// Dist for node creation **redumdant with round
	char *name; 	//Only if node is a leaf
	int specnumber; // Nbr of species  if splitting under this node
	double pval; 	// Pvalue of the node 
	double sum_inter;  	//Sum intra species for the node (all species below the node)
	double sum_intra; 	//Sum inter species for the node (all species not below the node)
	double sum_all; 
	//double tmrca_obs;
	long int nb_inter; 	//nbr of intra comparaison
	long int nb_intra; 	//nb of inter comparaison
	long int nb_all; 	//nbr all comparaison
	int nbdesc;		//Nb descendants (sizeof of *desc array)
	double time;	//check
	int nb_under; 	// Nb leaves under the node 
	int nbmut_under; 
	int nbmut;
	char to_draw; //if 1 then draw in yellow thats for cherries....
	int first_to_draw; // for clado gram: first specie (upper) under the node
	char to_be_checked; //=0 if skipped becase pvalue is bad =1 if node is going to be seriously evaluated
	long int replicates; // how many replicates for simulations on this node 
	double S_all_theo; //Theoritical Sum intra species 
	double S_intra_theo; //Theoritical Sum inter species 
} Node;

typedef struct
{
	int R;
	int L;
} LeftRight;

typedef struct spart{
		char *name;
	int *nb_ind; 
	int *specie;
	int *group;
	int rank;
	int nbsubsets;
}Spart;
//struct to store the composition of a composant
// for a new component C1 whose content is listed in compo.compo[c1] before joined it was nb groups of size effcompo each  and this nb groups were formed with the coposent listed in nodecompo

typedef struct tabcompo
{
	int nb; 	//nbr of groups composing
	int *effcompo; // array of nb elts, each elt is the size of compsant at previous stage
	int *nodecompo; //list of old compo composing //ne sert a rien... a verifier

} Tabcompo;

typedef struct result
{
	int nbgroups;
	int nbspec;
	int nbspecRec;
	int last_node;
	double proba;
	double d;
	double d_jump;
	int *eff_groups; //effectifs_group[nbgroups]
	int rank;   /* only for small part of results */
	int rank_pente;//rank for slope
	int rank_proba; //rank for proba
	int rank_general; //rank for asap score
	int *listNodes;// liste de nouedspresents pour cette part
	int nb_nodes;
	double inter;
	double intra;
	double other_parameter;
	double score; //asap score
	double *proba_part;
} Results;

typedef struct paramet
{
char *ledir;
int lenSeq; 
int nbpairs;
int onlyspart;
int replicates;
float seuil_pvalue;
float pond_score;
float pond_pente;
FILE *fres;
FILE *f_out;
int fit_to_page;
int web;
} Parameter;

struct DistanceMatrix {

	long n;            /* number of sequence */
	char **names;      /* store names, at most SIZE_NAME_DIST char */
	double **dist;     /* a 2-d matrix of distance [0, \inf] */
	double ratio_ts_tv;		/*transition/transversion rate*/

};

typedef struct DistanceMatrix  DistMat ;


struct FastaSeq {
	char *name;
	char *seq;
};


typedef struct Elt{
	int n;
	struct Elt *next;
	struct Elt *previous;
	} elt;

typedef elt *List;

void clearalltab(Tabcompo *strucompo, Composante *comp, int nbseq);
int compareCase(void const *a, void const *b);
int compareParameter(void const *a, void const *b);
int compareProba(void const *a, void const *b);
int compareRang(void const *a, void const *b);
void asap_exit_properly(char *ledir);
double exponentialdev();
void freecomp(Composante *comp, int nbseq);
void initcomp(Composante *comp, int nbseq, FILE *ff, char *ledir);
void initNodes(FILE *f, Node *zenodes, struct DistanceMatrix mat, char *ledir);
void inittabcompo(Tabcompo *strucompo, int nbseq, FILE *ff, char *ledir);
void mattolist(DistPair *g , DistMat *d , float *max, float *min);
double poissondev(double mean);
int RandomPi( int n, double MutRate, double *lengthTreeLeft, double *lengthTreeRight, LeftRight *size, double *S_intra, long int *n_intra, double *S_all,int k_obs);
void reinit_nod(int nbfeuilles, Node *simnodes);
double unirandom();
#endif
