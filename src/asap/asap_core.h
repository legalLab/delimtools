#ifndef ASAP_CORE_H
#define ASAP_CORE_H
#define EPSILON 0.0001
void 		add_one_mutation(Node *zenodes, int nbn, double lenghtTot);
int 		agglutine(FILE *f, int a, int b, Composante *compo, DistMat mat, long *nbinter, double *Sinter, Tabcompo *strucompo);
int 		BuildRandomTree(int nleaves, Node *zenode, FILE *f);
double 		calcul_pente(double d, DistPair *glutine, int nbv ,float pond_pente);
void	compute_length(double t, LeftRight * size, double *lengthTreeLeft,double *lengthTreeRight,int min,int max);
int 		compo_rspecie(Composante *comp, Node *zenodes, double seuil,int *list_node,int nbseq, Results *scores,int round);
int do_agglutine(DistMat mat, Composante *comp, DistPair *ListDist, Results *scores, Tabcompo *strucompo,  double *best, int *fi,  Node *zenodes, int *list_node, int *lastnode,  Parameter asap_param);
//int 		do_agglutine(DistMat mat, Composante *comp, DistPair *ListDist, Results *scores, Tabcompo *strucompo, int nb_pairs, FILE *f_out, double *best, int *fi, FILE *ff, Node *zenodes, int *list_node, int *lastnode, char *ledir, int lenSeq, int replicates,float seuil_pvalue,float pond_pente);
void 		from_compo_to_tree(Composante *comp, Node *zenodes, int *node_ori, int *list_nodes, DistMat mat, FILE *f, Tabcompo *strucompo, int r, double dist, char *ledir, int nbgr);
void 		go_spectre(int no_nodes, int *spectre, Node *zenodes);
void 		newStatCoal(Node *zenodes,FILE *fres,int lenSeq,Composante *comp, double pi_inter_obs,  double pi_intra_obs,  int part,	LeftRight *size,double *lengthTreeLeft,double *lengthTreeRight, Results *scores, int *list_node,char *ledir,int maxfeuilles,  double S_intra_tot_obs, long nb_intra_tot_obs, int replicates );
void 		place_two_nodes_firstpos(int * array, int size);
int RandomPi( int n, double MutRate, double *lengthTreeLeft, double *lengthTreeRight, LeftRight *size, double *S_intra, long int *n_intra, double *S_all,int k_obs);
	int  		specie_node_recurse(Node *zenodes,int n,double score);
void 		SplitSize( LeftRight *size, int tot, int *min, int *max);
long int 	sommeprodtab(int *t, int nb);

#endif