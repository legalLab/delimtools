/*
	Copyright (C) 2015-2016 G Achaz/ S Brouillet

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2.1
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

for more information, please contact guillaume achaz <guillaume.achaz@mnhn.fr>/<sophie.brouillet@mnhn.fr>

*/
/******
        file     : asap_core
        function : all fns neened for the core algorithm


        created  : JAnuary 2019


        author   : madamesophie


*****/

/*--------------------------------------------------*/
/*
	both sequence nbr a and nbr b will be grouped doing this:
	merge composante to which b belongs to the composant to which a belong
	reset b composante to 0

	a: seq1, b: seq2
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <float.h>
#include <ctype.h>
#include <limits.h>
#include "asap.h"
#include "asap_core.h"

#include "wrapio.h"


int agglutine(FILE *f, int a, int b, Composante *compo, DistMat mat, long *nbinter, double *Sinter, Tabcompo *strucompo)
{
	int i, j, k;

	/*
		retrieve the component id of a and b
	*/
	int aa = compo->node_compid[a];
	int bb = compo->node_compid[b];
	int nbnewcomp = compo->n_in_comp[aa] + compo->n_in_comp[bb];   /* the new group will enompass all elts of a and b */


	/*
		if a and b don't belong to same composante
	*/
	if (aa != bb )
	{
//	printf("seq:%d et %d-->compo: %d et %d \n",a,b,aa,bb);
	if (bb<aa) // choice to keep the smallest index after grouping
			{
			int temp=bb;
			bb=aa;
			aa=temp;

			temp=b;
			b=a;
			a=temp;
			}




		compo->Sall_in_comp[aa]+=compo->Sall_in_comp[bb];             /* aa now also includes the bb */


		/*
			then add distances and nnbrs of comparisons
		*/
		for (i = 0; i < compo->n_in_comp[aa]; i++)
		{
			for (j = 0; j < compo->n_in_comp[bb]; j++)
			{
				(*Sinter) += mat.dist[compo->comp[aa][i]][compo->comp[bb][j]];
				(*nbinter) ++;

				compo->Sall_in_comp[aa] += mat.dist[compo->comp[aa][i]][compo->comp[bb][j]];   /* also add the distance between the sequence of a vs b */
			}
		}


		/*
			add compo b at the end of compo a, change all compid of old b to compid (a)
		*/
		for (k = compo->n_in_comp[aa], j = 0; k < nbnewcomp; k++, j++)
		{
			compo->comp[aa][k] = compo->comp[bb][j];
			compo->node_compid[compo->comp[bb][j]] = aa;
			compo->comp[bb][j] = -1;
		}

//			compo->altered[bb] = 0;

			/*
			put a flag on aa in order to not count it if it has to be merged again
		*/
		if (compo->altered[aa] == 0)
		{
			compo->altered_comp[compo->naltered] = aa; /* add to the list the aa */
			compo->naltered++;
			compo->altered[aa] = 1;;
		}

		if (compo->altered[bb] == 1) //bb is in the list of modfied comp so remove it
		{
			for (i=0;i<compo->naltered;i++)
				if (compo->altered_comp[i]==bb)
				{
				for (j=i;j<compo->naltered-1;j++)
					compo->altered_comp[j]=compo->altered_comp[j+1];
				break;
				}
		compo->naltered--;

		}


		/**
			Update the strucompo
		**/

		/*
			add at the end of strucompo[aa].effcompo, the struct compo of bb
		*/
		for (i = strucompo[aa].nb, j = 0; i < strucompo[aa].nb + strucompo[bb].nb; i++, j++)
			{
			strucompo[aa].effcompo[i] = strucompo[bb].effcompo[j];
			strucompo[aa].nodecompo[i] = strucompo[bb].nodecompo[j];
			}
		/*
			add to aa the nbr of elt in bb
		*/
		strucompo[aa].nb = strucompo[aa].nb + strucompo[bb].nb;


		/*
			reset strucompo[bb]
		*/
		strucompo[bb].nb = 0;
		memset(strucompo[bb].effcompo, 0, mat.n);
		memset(strucompo[bb].nodecompo, 0, mat.n);
		compo->altered[bb] = 0;


		/*
			update single values
		*/
		compo->nc=compo->nc -1;
		compo->n_in_comp[aa]=nbnewcomp;
		compo->n_in_comp[bb]=0;

/*	printf("apres agglo %d",aa);
 		for (i=0;i<strucompo[aa].nb;i++)
 			printf("node %d (eff %d ) ",strucompo[aa].nodecompo[i],strucompo[aa].effcompo[i] );
printf("\n");*/
		return 1;

	}

	return 0;
}

/*--------------------------------------------------*/
long int sommeprodtab(int *t, int nb)
{
	int i, j;
	long int s = 0;

	for (i = 0; i < nb - 1; i++)
		for (j = i + 1; j < nb; j++)
			s += (t[i] * t[j]);

	return s;
}


/*------------------------------------------------------*/
/*
	for one component turns it into the node structure
	component is new each time a new distance is aglutinated but node is just filled from node_ori
	no_nodes contains the number of the last build node which leads to the seq in the tree
*/
void from_compo_to_tree(Composante *comp, Node *zenodes, int *node_ori, int *list_nodes, DistMat mat, FILE *f, Tabcompo *strucompo, int r, double dist, char *ledir, int nbgr)

{

	int i, j, l, k,c1,  desc;

	double sum_intra = 0,
	       sum_all = 0,
	       sum_inter = 0;
	       //tmrca = 0;


	long int nb_inter = 0,
	         nb_all = 0;

	int  nbstruct;
	int maxnodes=(2*mat.n)-1;


	/*
		Scan all groups/composantes that were altered this round
	*/

	for (i = 0; i < comp->naltered; i++)
	{

		c1 = comp->altered_comp[i];   /* id of the altered comp */
		l = comp->n_in_comp[c1];      /* number of elts in this comp */

//fprintf(stderr,"%d altered comp %d de taille %d\n ",comp->naltered,c1,l);

		if (l > 1)
		{
			int nleaves = 0;
			sum_all = sum_intra = sum_inter = 0.0;
			nb_all = nb_inter = 0;


			(*node_ori)++;                   // we have a new node


			if (*node_ori >(mat.n*2)-1)
				fprintf(stderr,"grossss grosss pbn\n"),exit(1);



			if (*node_ori>=maxnodes)fprintf(stderr,"not enough nodes?????\n"),fprintf(f, "not enough nodes"),fclose(f), asap_exit_properly(ledir);

			nbstruct = strucompo[c1].nb;     // component c1 is made of nbstruct same than nbr of descendant of the new_node

			zenodes[*node_ori ].nbdesc = nbstruct;
			zenodes[*node_ori ].desc = malloc(sizeof(Node) * nbstruct);
			if ( ! zenodes[*node_ori ].desc )fprintf(f, "from_compo_to_tree: memory error cant allocate zenode[%d].desc", *node_ori), fclose(f), asap_exit_properly(ledir);

			zenodes[*node_ori ].color=(*node_ori)%16;
			zenodes[*node_ori ].round = r;             // usefull for sintra
			zenodes[*node_ori ].dist = dist;           // distance used to agglomerate seqs under this node
			zenodes[*node_ori ].nbgroups = nbgr;

			for (j = 0; j<nbstruct; j++)
				{
					int olnod=list_nodes[strucompo[c1].nodecompo[j]];// get node of the first sequence


					zenodes[*node_ori].desc[j] = olnod; // link the new node with the old seq's node
					if (olnod<0 || olnod>=maxnodes) fprintf(stderr," node:%d (j=%d c1=%d)PB\n",olnod,j,c1),exit(1);


					nleaves += zenodes[olnod].nb_under;


					zenodes[olnod].anc = *node_ori;

//					printf("---->node[%d].anc=%d desc(%d)=%d --->zenold[%d].anc=%d\n",olnod,*node_ori,j,olnod,olnod,zenodes[olnod].anc);


				}
			zenodes[*node_ori].nb_under = nleaves;



			//upddate listnode: all the new grouped must have the same new node number

			for (j = 0; j<nbstruct; j++)
				{
				int olnod=strucompo[c1].nodecompo[j];
				if (olnod<0 || olnod>=maxnodes) fprintf(stderr," node:%d (j=%d c1=%d)PB\n",olnod,j,c1),exit(1);
				for(k=0;k<comp->n_in_comp[olnod];k++)
					list_nodes[comp->comp[olnod][k]]= *node_ori;// all seqs previously in the same than the fisrt seq must change
				}
		//	free(tempo_nodes);



			/*
				Compute S_all
			*/
			sum_all = comp->Sall_in_comp[c1];                        /* get what was computed in agglutine() */

			nb_all = (l * (l - 1)) / 2;


			sum_intra = 0;
			for (j = 0; j < zenodes[*node_ori].nbdesc; j++)
			{
				desc = zenodes[*node_ori].desc[j];
				sum_intra = sum_intra + zenodes[desc].sum_all;   /* sum of all of the previous step is the new intra*/
			}


			sum_inter =	sum_all - sum_intra;
			nb_inter = sommeprodtab(strucompo[c1].effcompo, strucompo[c1].nb);

			/*
				Store for next round
			*/
			zenodes[*node_ori].sum_all  = sum_all;
			zenodes[*node_ori].nb_all = nb_all;
			zenodes[*node_ori].nb_inter = nb_inter;
			zenodes[*node_ori].sum_inter  = sum_inter;
			//zenodes[*node_ori].tmrca_obs = tmrca;s
			zenodes[*node_ori].nb_intra = nb_all - nb_inter;
			zenodes[*node_ori].sum_intra  = sum_all - sum_inter;


		}
		else
			{fprintf(stderr, "comp to tree: this should never happen !\n");exit (1);}


	}

}


/*--------------------------------------------------------------------------------------
Evaluate the stepness of the slope around dist d (look for nbv values after and before and see wahts the distance there)

--------------------------------------------------------------------------------------*/
double calcul_pente(double d, DistPair *ListDist, int nbv ,float pond_pente)
{

	int shift_r_H=0,
	    shift_r_L=0;

	double d_min, d_max,
		 d_L = ListDist[0].d,
	       d_H = ListDist[nbv-1].d;

	int i=0;


	i=0;
	while( ListDist[i].d != d && i<nbv )i++;      // set it to the first value of distance d

	d_min = ListDist[i].d*(1.0+pond_pente);       // d*1.1
	d_max = (i>0)?ListDist[i-1].d*(1.0-pond_pente):ListDist[i].d*(1.0-pond_pente);  // d[i-1]*0.9

	while( i+shift_r_H<nbv && ListDist[i+shift_r_H].d <= d_min )                    // find the distance >= dmin
			shift_r_H++;

	d_H=ListDist[i+shift_r_H].d;

	while( (i-shift_r_L)>0 && ListDist[i-shift_r_L].d >= d_max )
			shift_r_L++;
	d_L=ListDist[i-shift_r_L].d;

//	printf("for d_T= %f => [%f,%f]; d_L=%f; d_H=%f; n: %d+%d --> p:%f\n", d,d_min,d_max,d_L, d_H, shift_r_H,shift_r_L,100*(d_H-d_L)/(double)((1.0+d_H+d_L)*(shift_r_H+shift_r_L)));

	if(shift_r_H+shift_r_L)
		return  (d_H-d_L)/(double)( (1.0+d_H+d_L)*(shift_r_H+shift_r_L) );

	else
		return 0;

}




/*--------------------------------------------------------------------------------------*/
#define MIN_SUCCESS 50

void newStatCoal(Node *zenodes,
                 FILE *fres,
                 int lenSeq, 			//length of seq
                 Composante *comp,
                 double pi_inter_obs,  	// for one partition
                 double pi_intra_obs,  	// for one partition
                 int part,				//partition pour laquelle on fait la stat
           		 LeftRight *size,
                 double *lengthTreeLeft,
                 double *lengthTreeRight,
                 Results *scores,
                 int *list_node,			// array of leaves(seqs) containing the node above the seq at this stage of part
                 char *ledir,
                 int maxfeuilles,            // nbr of leaves in real tree
                 double S_intra_tot_obs,     // total Sintra in all subgroups in observed data
		 long nb_intra_tot_obs,           	// total number of intra comparison in observed data
		 int replicates                  	// number of simulations

){

	int i,  k, j, c1;
	int current_node;
	int nleaves;
	int nbreplicates_ini=500;
	int effort=1e5;
	int nb_skipped=0;

	double S_intra_altered_obs=0;

	double S_intra_theo=0,
	       S_all_theo=0;

	double  pi_intra_theo=0;

	double S_intra_theo_partition=0;

	double pi_intra_theo_partition;
	long int nb_intra_theo;
	long int nb_intra_theo_partition;
	long int nb_intra_altered=0;

	double theta;

	long part_replicates=0;


	double pi_inter_node , pi_intra_node;

	//int tag=0;


	/*
		First, retrieve info on altered nodes.
	*/
	scores[part].proba=0;
	S_intra_altered_obs = 0;
	nb_intra_altered = 0;

	for (i = 0; i < comp->naltered; i++)
	{
		c1 = comp->altered_comp[i];
		current_node = list_node[comp->comp[c1][0]];

		nb_intra_altered    += zenodes[current_node].nb_intra;
		S_intra_altered_obs += zenodes[current_node].sum_intra;

	//	if (zenodes[current_node].nb_under<=2)zenodes[current_node].to_draw=0; else zenodes[current_node].to_draw=1;

		// if only leaves under, set to_draw=0, otherwise to_draw=1
/*		for (k=0;k<zenodes[current_node].nbdesc;k++)
		{
			int d=zenodes[current_node].desc[k];
			if (zenodes[d].nbdesc>0)                     // then it is not a leaf
				break;
		}
*/
		if( zenodes[current_node].nb_intra == 0)            // all descendants are leaves
		{
			zenodes[current_node].pval=1;
			zenodes[current_node].to_draw=0;
		}else{
			zenodes[current_node].to_draw=1;
			zenodes[current_node].pval=0;
		}

	}


	if( part == 0 )
		{
			scores[part].proba=1;
			return;
		}


	/*
		Then compare them with simulations
	*/
	replicates = nbreplicates_ini;       // start with a small nuber of replicates
	nb_skipped = 0;


	for (k = 0; k < replicates; k++)
	{

		S_intra_theo_partition  = 0;
		nb_intra_theo_partition = 0;

		/*
			Evaluate all nodes that has been altered
		*/
		for (i = 0; i < comp->naltered; i++)
		{

			int k_obs=0;

			c1 = comp->altered_comp[i];      // the focal group

			if (comp->comp[c1][0] == -1)
				fprintf(fres, "***** bug*** comp %d seq %d PB seq=-1<br>", c1, comp->comp[c1][0]), fclose(fres), asap_exit_properly(ledir);


			/*
				retrieve info from the observed node
			*/
			current_node = list_node[comp->comp[c1][0]];                    // access the node of the focal group by the first sequence of c1


			pi_inter_node = (zenodes[current_node].nb_inter>0) ? zenodes[current_node].sum_inter / zenodes[current_node].nb_inter : 0;
			pi_intra_node = (zenodes[current_node].nb_intra>0) ? zenodes[current_node].sum_intra / zenodes[current_node].nb_intra : 0;




			nleaves = zenodes[current_node].nb_under;

			for (j=0;j<zenodes[current_node].nbdesc;j++)
					{
						int d=zenodes[current_node].desc[j];
						if (zenodes[d].nb_under>k_obs)
								k_obs=zenodes[d].nb_under;
					}


			if ( zenodes[current_node].to_be_checked == 0 ) continue;

			/*
				Run coalescent simulations with adequate theta
			*/
			theta = lenSeq * pi_inter_node * nleaves/(2.0*(nleaves-1.0));     // set theta so that inter is the same on average

			RandomPi( nleaves, theta/2.0, lengthTreeLeft, lengthTreeRight, size, &S_intra_theo, &nb_intra_theo, &S_all_theo,k_obs);

			pi_intra_theo = (nb_intra_theo == 0) ? 0 : S_intra_theo / (double) nb_intra_theo;
			/*
				Compare random stats with obs
			*/
			if ( zenodes[current_node].nb_intra>0 && pi_intra_theo - EPSILON <= pi_intra_node * lenSeq )
				zenodes[current_node].pval++;


			zenodes[current_node].S_all_theo   += S_all_theo;
			zenodes[current_node].S_intra_theo += S_intra_theo;

			nb_intra_theo_partition += nb_intra_theo;
			S_intra_theo_partition += S_intra_theo;


			if ( k > nbreplicates_ini-1 && scores[part].proba >= MIN_SUCCESS )   // once partition proba is good enough, you can start dropping nodes.
			{
				if( zenodes[current_node].pval >= MIN_SUCCESS ) //50 is min success ether for node AND part??????
				{
					nb_skipped++;
					zenodes[current_node].to_be_checked = 0;
					zenodes[current_node].replicates = (k+1);

					//zenodes[current_node].pval = (zenodes[current_node].pval+1.0)/( (k+1)+1.0);   //  k is ++ at the loop end.

					zenodes[current_node].pval = (zenodes[current_node].pval+1)/( (k+1)+1.0);   //  k is ++ at the loop end.

					if( (zenodes[current_node].pval+1.0) > (k+1)+1.0 )
						{printf("PB pval:%f k: %d curent_node:%d ",zenodes[current_node].pval,k,current_node);exit (54);}

				}


			}


		} // end of comp->naltered

		/*
			Do partition statistics while all nodes are still computed
		*/
		if ( nb_skipped == 0 )
		{

			/*
				Add unaltered nodes
			*/
			S_intra_theo_partition  +=  (S_intra_tot_obs - S_intra_altered_obs) * lenSeq;
			nb_intra_theo_partition +=   nb_intra_tot_obs - nb_intra_altered;

			pi_intra_theo_partition = S_intra_theo_partition / (double)nb_intra_theo_partition;

			part_replicates = k+1;

			if ( pi_intra_theo_partition-EPSILON <= (pi_intra_obs * lenSeq))
				scores[part].proba++;

		}


		if( nb_skipped < comp->naltered )
		{
			replicates*=2;
			if(replicates>effort)replicates=effort;
//			nb_skipped=0;
		}


	} //end of replicates
//	if(tag)
//
for (i = 0; i < comp->naltered; i++)
{
	c1 = comp->altered_comp[i];
	current_node = list_node[comp->comp[c1][0]];
	if (zenodes[current_node].to_be_checked ==1 && zenodes[current_node].nb_intra>0 )
			zenodes[current_node].pval= (zenodes[current_node].pval+1.0)/(k+1.0);
}

				//exit(1);
	scores[part].proba = (scores[part].proba + 1) / (double)( part_replicates + 1);
	if (scores[part].proba >1 || scores[part].proba<0)
		printf("PB %d %f\n",part,scores[part].proba);
	//scores[part].other_parameter=(pi_inter_obs*lenSeq)/(1+(pi_intra_obs*lenSeq));
	//scores[part].other_parameter=calcul_pente(double d,glutine, int nbv )
	scores[part].intra=pi_intra_obs;
	scores[part].inter=pi_inter_obs;

}

/*--------------------------------------------------*/
/*renvoie le nbre de noueds fils ayant une proba < score recursivement pour chaque noued un nbre en plus.. mais a appeler sur chaque noued de compo pour avoir le nbre en plus*/
int  (specie_node_recurse)(Node *zenodes,int n,double score)
{
int i,m;
//printf("->%d \n",zenodes[n].nbdesc);
int nb=0;
//if (FFflag==1) printf("ds sp rec: %d %f nbdesc:%d %d\n",zenodes[n].nbdesc,zenodes[n].pval,zenodes[n].nbdesc,nb);
if (zenodes[n].nbdesc>0 && zenodes[n].pval<= score )

		{
			for (i=0;i<zenodes[n].nbdesc;i++)
				{
				m=	zenodes[n].desc[i];
				//if (FFflag==1)printf("\tn=%d-->%d *\n",m,nb);

				nb+=(specie_node_recurse(zenodes,m,score) );


				}
				//if (FFflag==1)printf("fils:%d nb:%d\n",zenodes[n].nbdesc,nb);
				return (nb);
		}

//printf("sortie->%d \n",zenodes[n].nbdesc);
//if (zenodes[n].nbdesc==zenodes[n].nb_under)
//return zenodes[n].nbdesc;
//else

return (1);

}


int compo_rspecie(Composante *comp, Node *zenodes, double seuil,int *list_node,int nbseq, Results *scores,int round){

	int current_node,nb_rplus=0,i,j=0, k;
//printf("nc:%d (%d)\n",comp->nc,nbseq);

scores[round].nb_nodes=0;
	for( i =0; i< nbseq ;i++)
	{

		if (comp->n_in_comp[i]!=0)
		{

			current_node = list_node[comp->comp[i][0]];

			//if (current_node==129)	FFflag=1	; else FFflag=0;

			if( zenodes[current_node].dist == scores[round].d )
			{

				for(k=0;k<zenodes[current_node].nbdesc;k++)
				{

					nb_rplus += specie_node_recurse(zenodes,zenodes[current_node].desc[k],seuil);

				}
			}
			else
			{

				nb_rplus += specie_node_recurse(zenodes,current_node,seuil);
			}

			scores[round].listNodes[j]=list_node[comp->comp[i][0]];
			scores[round].nb_nodes++;
	//	printf("noeud:%d nbplu:%d desc=%d  p=%f under=%d[",current_node,nb_rplus,zenodes[current_node].nbdesc,zenodes[current_node].pval,zenodes[current_node].nb_under);
	//	int oo;
	//	for (oo=0;oo<zenodes[current_node].nbdesc;oo++)
	//		printf("%d,",zenodes[current_node].desc[oo]);
	//	printf("]\n");
			j++;
		}
	}

	return nb_rplus;

}


/*--------------------------------------------------*/

int do_agglutine(DistMat mat, Composante *comp, DistPair *ListDist, Results *scores, Tabcompo *strucompo,  double *best, int *fi,  Node *zenodes, int *list_node, int *lastnode,  Parameter asap_param)

{
	int i = 0,
	    nbresults = 0,
	    nbrealloc=1,
	    test = 0;

	float pi_inter = 0,
	      pi_intra = 0;


	double S_inter = 0,       /* the sum of all comparison between groups */
	       S_intra = 0,
	       dist,
	       S_all = 0;

	long nbinter = 0,
	     nbintra = 0,
	     n_all = 0;

	LeftRight *size;
	double *lengthTreeLeft;
	double *lengthTreeRight;
	int nbspecies_rec=0;
	int nbnodesmax=(2 * mat.n) - 1;

	//if (ff == NULL)
	//	ff = stderr;

	*best = 1;
	if (asap_param.onlyspart==0)
	{
	fprintf(asap_param.f_out, "#pi_inter=pi pi_intra=pa nb_ra=nb_intra nb_er=nb_inter\n");
	fprintf(asap_param.f_out, "#ID   \t#subsets \t#subsets w/rec\tdist     \tPi_i/Pi_a\tPi_inter\tPi_intra\t#inter\t#intra\tpval\n");
	}

/*	simnodes = (Node * )malloc( sizeof(Node) * nbnodesmax);
	for (i = 0; i <	nbnodesmax; i++)
		simnodes[i].desc = (int *)malloc( (size_t) sizeof(int) * 2);
*/
	lengthTreeLeft = (double *)calloc(nbnodesmax, sizeof(double) );
	lengthTreeRight = (double *)calloc(nbnodesmax, sizeof(double));
	size = (LeftRight *) calloc(nbnodesmax, sizeof(LeftRight));



	/*	scores[0].d=ListDist[0].d;
		scores[0].nbgroups=oldcomp;
		scores[0].proba=1;

		scores[0].rank=0;*/
	i = 0;
	dist=1;
	while (i < asap_param.nbpairs && comp->nc >= 1)       /* while there are still some untreated pairs AND more than 1 group */
	{

		dist = ListDist[i].d;               /* set the next distance */
		test = 0;


		//fprintf(stderr,"\n***> Now at distance %f\n", dist);

		/*
			Group all sequences with the same distance ('dist')
		*/
		while (i < asap_param.nbpairs && dist == ListDist[i].d)
		{

			test += agglutine(asap_param.fres, ListDist[i].a, ListDist[i].b, comp, mat, &nbinter, &S_inter, strucompo);
			i++;
		}



		/*
			If the composantes have changed, proceed to statistical test
		*/
		if( test )
		{
			//fprintf(stderr,"*");

			scores[nbresults].d = dist;                 /* this store the results */


			scores[nbresults].d_jump = (dist + ((nbresults)?scores[nbresults-1].d:0) ) /2.0;                /* this store the results */


			scores[nbresults].nbgroups = comp->nc; //
			scores[nbresults].rank = i;


			from_compo_to_tree(comp, zenodes, lastnode, list_node, mat, asap_param.fres, strucompo, nbresults, dist, asap_param.ledir, comp->nc);    /* All altered composants are turned into nodes, can be improved CHECK */

			scores[nbresults].last_node= *lastnode;

			if (comp->naltered == 0) fprintf(asap_param.fres, "Should not happen...*bug*\n"), fclose(asap_param.fres), asap_exit_properly(asap_param.ledir);

			if (nbintra != 0)
				pi_intra = (float)(S_intra) / (float)nbintra; //just for output no use
			else
				pi_intra = 0;


			if (nbinter != 0)
				pi_inter = (float)(S_inter) / (float)nbinter; //just for output no use
			else
				pi_inter = 0;

			S_all = S_intra + S_inter;
			n_all = nbintra + nbinter;

			newStatCoal(zenodes, asap_param.fres, asap_param.lenSeq, comp, pi_inter, pi_intra, nbresults, size, lengthTreeLeft, lengthTreeRight, scores, list_node, asap_param.ledir, mat.n, S_intra, nbintra, asap_param.replicates);



			if (*best > scores[nbresults].proba)
				*best = scores[nbresults].proba;

			int nbspecies = (nbresults==0)?mat.n:scores[nbresults-1].nbgroups;//voir fn qui descend recursivt autre var qui descend recur

			nbspecies_rec=compo_rspecie(comp, zenodes,asap_param.seuil_pvalue,list_node,mat.n,scores,nbresults);
			scores[nbresults].proba_part=malloc(sizeof(double)*(nbspecies_rec+1));// needed for Spart

			//printf("ds comp %d--->%d 	%d more (res:%d) \n",comp->nc,nbspecies,nbspecies_rec,nbresults);
			//print_comp(*comp, comp->nc, strucompo);
			if (nbspecies_rec < nbspecies) {nbspecies_rec=nbspecies;}//PB HERE FOR FIRST PARTITIONS OTHERWISE WEIRD FIRST RES if
	//print_comp(*comp, comp->nc, strucompo);
	//			if (scores[nbresults].proba<=0.05)
	//		{printf("\n >> nbresults:%d respecies2:%d nc=%d ncalt=%d °°°°°°°°°°\n",nbresults,compo_rspecie2(comp, zenodes,0.05,list_node,mat.n),comp->nc,comp->naltered);}



			scores[nbresults].nbspecRec=nbspecies_rec;
			scores[nbresults].nbspec=nbspecies;
			scores[nbresults].eff_groups=malloc(sizeof(int)*(nbspecies_rec+1));
			int hhh;
			for (hhh=0;hhh<=nbspecies_rec;hhh++)
			scores[nbresults].eff_groups[hhh]=0;
			//fprintf(ff,"using %f\n",pond_pente);
			scores[nbresults].other_parameter=calcul_pente(dist,ListDist, asap_param.nbpairs,asap_param.pond_pente);
		if (asap_param.onlyspart==0)
			if (pi_intra != 0)
				fprintf(asap_param.f_out, "%-5d\t%-5d\t%-5d\t%f\t%f\t%f\t%f\t%ld\t%ld\t%e\n",
				        nbresults, nbspecies, nbspecies_rec,dist, pi_inter/pi_intra, pi_inter, pi_intra, nbintra, nbinter, scores[nbresults].proba);
			else
				fprintf(asap_param.f_out, "%-5d\t%-5d\t%-5d\t%f\t%f\t0.000000\t%f\t%ld\t%ld\t%e\n",
				        nbresults, nbspecies, nbspecies_rec, dist, pi_inter, pi_intra, nbintra, nbinter, scores[nbresults].proba);


			nbresults++;

			if (nbresults >mat.n)
					{
					nbrealloc++;
					scores=realloc(scores,sizeof(Results)*(mat.n*nbrealloc));

					}
		}

		S_intra = S_all;
		nbintra = n_all;

		S_inter = 0;
		nbinter = 0;

//		IMPORTANT :reset all needed variables between 2 components

		clearalltab(strucompo, comp, mat.n);



	}
/*	scores[nbresults].d = dist;    //  est ce quil ne manque pas les dernieres ditsanceS......
	scores[nbresults].nbgroups = 1;
	scores[nbresults].rank = i;
	nbresults++;
*/

	free(lengthTreeRight);
	free(lengthTreeLeft);
	free(size);
//	for (i = 0; i < nbnodesmax ; i++)
//		free(simnodes[i].desc)	;
//	free(simnodes);

	return nbresults;
}
//do_specie_recurs(comp, scores,zenodes,nbNodes,*lastnode,round)
//pour chaque noued de la compo descendre voir si les descendants ont des proba<=score si oui on augmente lenbr de groupe

/*--------------------------------------------------*/
//STATISTICS
/*--------------------------------------------------*/

void place_two_nodes_firstpos(int * array, int size) {

	int n = 0;
	int temp = 0;

	n = (int)floor( unirandom() * size);

	temp = array[0];
	array[0] = array[n];
	array[n] = temp;

	n = (int)floor( unirandom() * (size-1.0) )+1;

	temp = array[1];
	array[1] = array[n];
	array[n] = temp;


}



/*--------------------------------------------------*/
void go_spectre(int no_nodes, int *spectre, Node *zenodes)
{

	int i, j;

	i = zenodes[no_nodes].nbmut;
	j = zenodes[no_nodes].nb_under - 1;


	spectre[j] += i;

	if (j > 0) {
		go_spectre(zenodes[no_nodes].desc[0], spectre, zenodes);
		go_spectre(zenodes[no_nodes].desc[1], spectre, zenodes);
	}

}


/*--------------------------------------------------*/
void add_one_mutation(Node *zenodes, int nbn, double lenghtTot)
{

	double t_rand = lenghtTot * unirandom(),
	       t      = 0;

	int c = 2*nbn-2;

	t = zenodes[c].time;

	while( t_rand > t ){
		c--;
		t += zenodes[c].time;
	}

	zenodes[c].nbmut++;

}





/*--------------------------------------------------*/
int BuildRandomTree(int nleaves, Node *zenode, FILE *f)
{
	int *restants, *prestants;
	int i, j;
	int current_node;
	double t = 0;


	restants = malloc(sizeof(int) * nleaves);
	if (restants == NULL) fprintf(f, "pb malloc<BR>"), exit (1);

	for (i = 0; i < nleaves; i++)
		restants[i] = i;
	prestants = restants;

	current_node = nleaves;


	for (i = 0; i < nleaves; i++)   /* reset the number of desc to 0 (for leaves)*/
		zenode[i].nbdesc = 0;


	while (nleaves >= 2)
	{

		/*
			Random time
		*/
		t += exponentialdev() * ( 2.0 / ( (double)nleaves * (nleaves - 1.0) ) );

		/*
			Place two random nodes at cells 0 and 1 of prestant
		*/
		place_two_nodes_firstpos(prestants, nleaves);

		/*
			Connect nodes at cells 0 and 1 to a new ancestor 'current_node'
		*/
		zenode[current_node].nbdesc = 2;
		i = prestants[0];
		j = prestants[1];
		zenode[current_node].desc[0] = i;
		zenode[current_node].desc[1] = j;
		zenode[current_node].time = t;
		zenode[current_node].nb_under = zenode[i].nb_under + zenode[j].nb_under;
		zenode[i].anc = current_node;
		zenode[j].anc = current_node;
		zenode[current_node].name = NULL;
		zenode[current_node].anc = -1;


		/*
			update for next round
		*/
		nleaves--;
		prestants ++;
		*prestants = current_node;
		current_node++;
	}

	free(restants);

	return	current_node + 1;
}

void SplitSize( LeftRight *size, int tot, int *min, int *max){

        int U1,U2;
        int i;

        int c=0;
        char side=0;


        /*
                Pick U1 and choose in size
        */
        U1=(int) floor( unirandom() * tot )+1;  // an int in [1,tot]
        c=0;
        for(i=*min;i<=*max;i++)
        {
                c+=size[i].L*i;
                if(c>=U1){side='l';break;}

                c+=size[i].R*i;
                if(c>=U1){side='r';break;}

        }

        /*
                Pick U2 to split the lineage of the chosen size into U2 and n-U2
        */
        U2=(int) floor( unirandom() * i )+1; // an int in [1,i]. Remember i is size-1


        /*
                Split
        */
        if (side=='r'){
        	size[i].R--;   // remove a lineage of size i+1
	        size[ U2 -1 ].R++;
    	    size[ (i+1)-U2 -1].R++;
        }
        else{
        	size[i].L--;
	        size[ U2 -1 ].L++;
    	    size[ (i+1)-U2 -1].L++;
        }

        /*
                Update Min and Max
        */
        *min=( U2-1 <*min )? U2-1 : *min;
        *min=( (i+1)-U2 -1<*min )?(i+1)-U2 -1 : *min;

        while(size[*max].L==0 && size[*max].R==0)(*max)--;

}
/******************************/
void	compute_length(double t, LeftRight * size, double *lengthTreeLeft,double *lengthTreeRight,int min,int max)
{
	int i;

	for (i=min;i	<=max;i++)
	{
		lengthTreeLeft[i]+=(size[i].L *t);
		lengthTreeRight[i]+=(size[i].R *t);
	}
}

int RandomPi( int n, double MutRate, double *lengthTreeLeft, double *lengthTreeRight, LeftRight *size, double *S_intra, long int *n_intra, double *S_all,int k_obs){

	int i, min, max;      /* min and max in the size array */
	double t;
	int k=0;               /* number of lineage to the left */
	int m;

	/*
		Init Top Down
	*/

	/*
		Split r and l
	*/

//	k=uniInt(1,n-1);
		k=k_obs;
	size[k-1].L=1;
	size[n-k-1].R=1;
	if (k>n-k){max=k-1;min=n-k-1;}else{min=k-1;max=n-k-1;}

	/*
		Update tables
	*/
	t = exponentialdev();
	compute_length(t,size,lengthTreeLeft,lengthTreeRight,min,max);



	/*
		Built Right and Left lengthTree for each subsequent step
	*/
	for(i=3;i<=n;i++)
	{
		SplitSize( size, n-i+1, &min, &max);

		t = (exponentialdev() * ( 2.0 / ( (double)i * (i - 1.0) ) ));
		compute_length(t,size,lengthTreeLeft,lengthTreeRight,min,max);

	}


	/*
		Compute Pintra
	*/
	*S_intra=0;
	*S_all=0;





	//for(i=0;i<k;i++) sofiz
	for(i=0;i<=k;i++)
	{
		m=poissondev( MutRate * lengthTreeLeft[i] );
		*S_intra += m * (i + 1.0) * (k - i - 1);    // is 0 when i=k-1
		*S_all   += m * (i + 1.0) * (n - i - 1.0);
	}

	//for(i=0;i<n-k;i++) sofiz
	for(i=0;i<=n-k;i++)
	{
		m = poissondev( MutRate * lengthTreeRight[i] );
		*S_intra += m * (i + 1.0) * (n-k - i - 1);    // is 0 when i=n-k-1
		*S_all   += m * (i + 1.0) * (n  -  i - 1.0);
	}

	*n_intra = ( (k*(k-1.0)/2.0) + ((n-k)*(n-k-1.0)/2.0) );

	/*
		Clean mess
	*/
	memset( lengthTreeLeft,0,k*sizeof(double) );
	memset( lengthTreeRight,0,(n-k)*sizeof(double) );
	size[0].L=0;
	size[0].R=0;

	return k;
}
