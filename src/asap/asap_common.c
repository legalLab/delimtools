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
        file     : asap_common
        function : all fns needed by asap (web version and command line version) except the core algorithm


        created  : November 2015


        author   : madamesophie


*****/



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <float.h>
#include <ctype.h>
#include <limits.h>
#include "asap.h"
#ifdef ASAP_CL
#define WORKDIR ""
#endif

#define NBTEST 10000
#ifdef MACOSX
#include <float.h>
#elif defined(SGI)
#include <limits.h>
#elif defined(LINUX)
#include <values.h>
#endif

#ifdef _PROTOTYPES
#undef _PROTOTYPES
#include <float.h>
#define _PROTOTYPES
#else
#include <float.h>
#endif

#include "wrapio.h"

unsigned long idum_ran1 = 0;
char *strcasestr(const char *haystack, const char *needle);


char debug;


/****************************************************/
/*--------------------------------------------------*/
/* Misc functions*/
/*--------------------------------------------------*/
void asap_exit_properly(char *ledir)
{
	if (strlen (ledir) >1)
	{
		exit(1);
	}
}
/*-----explor---------------------------------------------*/
/*COMPARAISON FNS*/
/*--------------------------------------------------*/
/*ascending sort on distance*/
int compareCase(void const *a, void const *b)
{
	DistPair *pa = (DistPair  *) a;
	DistPair *pb = (DistPair  *) b;

	double x = pa->d;
	double y = pb->d;

	if (x < y)
	return (-1);
	else if (x > y)
		return (1);
	else

	{
		if (pa->a == pb->a)
			return (pa->b - pb->b) ;
		else
			return (pa->a - pb->a) ;
	}

}
/*--------------------------------------------------*/
/*descending sort on slope*/
int compareParameter(void const *a, void const *b)
{
	Results *pa = (Results  *) a;
	Results *pb = (Results  *) b;

	double x = pa->other_parameter;
	double y = pb->other_parameter;
	if (x > y) return (-1);
	else return (1);
}

/*--------------------------------------------------*/
/*ascending sort on probabiliy*/
int compareProba(void const *a, void const *b)
{
	Results *pa = (Results  *) a;
	Results *pb = (Results  *) b;

	double x = pa->proba;
	double y = pb->proba;

	if (x < y) return (-1);
	else return (1);
}


/*--------------------------------------------------*/
/*ascending sort on asap rank*/
int compareRang(void const *a, void const *b)
{
	Results *pa = (Results  *) a;
	Results *pb = (Results  *) b;

	double x = pa->score;
	double y = pb->score;


	double x2=pa->proba;
	double y2=pb->proba;


	double x1=pa->other_parameter;
	double y1=pb->other_parameter;
	if (x < y)
		return (-1);
	else
	{
		if (x > y)
		 return (1);

		else //egalite des scores on teste les proba
			{
			if (x2 < y2) return (-1);
				else
					if (x2 >y2)return (1);
				else //egalité des probas
					if (x1 > y1) return (-1);
			}
	}
		return (1);
}



/****************************************************/
/*--------------------------------------------------*/
/* unirandom(), exponentialdev(), poissondev() are defined in asap_wrapper.c
 * using R's RNG (unif_rand) for reproducibility with set.seed(). */

/****************************************************/
/*--------------------------------------------------*/
/*INIT , REINIT OR CLEAN FONCTIONS*/
/*--------------------------------------------------*/
void reinit_nod(int nleaves, Node *simnodes)
{
	int i;
	int nbnodes = (2 * nleaves) - 1;
	for (i = 0; i < nleaves; i++)
	{

		simnodes[i].nbdesc = 0;
		simnodes[i].anc = -1;
		simnodes[i].time = 0;
		simnodes[i].nbmut = 0;
		simnodes[i].nb_under = 1;

	}

	for (i = nleaves; i < nbnodes; i++)
	{
		simnodes[i].nbdesc = 0;
		simnodes[i].anc = -1;
		simnodes[i].time = 0;
		simnodes[i].nbmut = 0;
		simnodes[i].nb_under = 0;
	}
}


/*--------------------------------------------------*/
/*init struct tabcompo*/
/*
	Now it is a [nbseq * nbseq] matrix
	filled with -1, with the exception of the first cell that is 1 because init state is one compo

*/
void inittabcompo(Tabcompo *strucompo, int nbseq, FILE *ff, char *ledir)
{
	int i;
	if (ff == NULL)
		ff = stderr;
	for (i = 0; i < nbseq; i++)
	{
		strucompo[i].nb = 1;
		strucompo[i].effcompo = (int *)malloc(sizeof(int) * nbseq);
		if (strucompo[i].effcompo == NULL) {
			fprintf(ff, "inittabcompo: MEMORY ERROR cant alloc strucompo[%d].effcompo\n", i);
			if (ff != NULL)
				{fclose(ff); asap_exit_properly(ledir);}
			else exit(1);
		}
		memset(strucompo[i].effcompo, -1, nbseq);
		strucompo[i].effcompo[0] = 1;
		strucompo[i].nodecompo = (int *)malloc(sizeof(int) * nbseq);
		if (strucompo[i].nodecompo == NULL) {
			fprintf(ff, "inittabcompo: MEMORY ERROR cant alloc strucompo[%d].effcompo\n", i);
			if (ff != NULL)
				{fclose(ff); asap_exit_properly(ledir);}
			else exit(1);
		}
		memset(strucompo[i].nodecompo, -1, nbseq);
		strucompo[i].nodecompo[0] = i;

	}
}

/*--------------------------------------------------*/
/*

*/
void initNodes(FILE *f, Node *zenodes, struct DistanceMatrix mat, char *ledir)
{
	int i;
	int nbnodes = (2*mat.n) - 1 ;

	for (i = 0; i < nbnodes; i++)
	{

		zenodes[i].anc = -1;

		zenodes[i].x = -1;

		zenodes[i].y = -1;

		zenodes[i].dist = 0;

		zenodes[i].name = NULL;
		zenodes[i].pval = -1;

		zenodes[i].nbdesc = 0;
		zenodes[i].sum_inter = 0;
		zenodes[i].sum_all = 0;
		zenodes[i].desc = NULL;
		zenodes[i].nb_inter = 0;
		zenodes[i].nb_all = 0;
		zenodes[i].round = -1;
		zenodes[i].nb_under = 0;
		zenodes[i].color=(i+1)%16;
		zenodes[i].first_to_draw=i;
		zenodes[i].to_be_checked=1;

		zenodes[i].S_all_theo=0.0;
		zenodes[i].S_intra_theo=0.0;

	}


	for (i = 0; i < mat.n; i++)
	{
		zenodes[i].name = malloc((strlen(mat.names[i]) + 1) * sizeof(char));
		if (zenodes[i].name == NULL)
		{	fprintf(f, "ERRORRRR<BR>\n"), fclose(f), asap_exit_properly(ledir);
			if (f != NULL)
			{fclose(f); asap_exit_properly(ledir);}
			else exit(1);
		}

		strcpy(zenodes[i].name, mat.names[i]);
		zenodes[i].color=i%16;
		zenodes[i].nb_under = 1;

	}

}
/*--------------------------------------------------*/
/*clear  all struccompo which have been modified */
void clearalltab(Tabcompo *strucompo, Composante *comp, int nbseq)
{
	int i, j;


	for (j = 0; j < comp->naltered; j++)
	{
		i = comp->altered_comp[j];
		if (i == -1) fprintf(stderr, "this is the end of the world: negative value saved\n"), exit(1);
		strucompo[i].nb = comp->n_in_comp[i];
		memset(strucompo[i].effcompo, -1, nbseq);
		strucompo[i].effcompo[0] = comp->n_in_comp[i];
		strucompo[i].nb = 1;
		comp->altered[i] = 0;
	}
	memset(comp->altered_comp, -1, comp->naltered);
	memset(comp->altered,0,nbseq);
	comp->naltered = 0;
}


/*--------------------------------------------------*/
void freecomp(Composante *comp, int nbseq)
{
	int i;


	free(comp->node_compid);

	free(comp->n_in_comp);

	free(comp->Sall_in_comp);

	free(comp->altered_comp);

	free(comp->altered);

	comp->naltered = 0;


	for (i = 0; i < nbseq; i++)
		if(comp->altered_comp_prev[i] != NULL)
			free(comp->altered_comp_prev[i]);

	free(comp->altered_comp_prev);

	for (i = 0; i < nbseq; i++)
		free(comp->comp[i]);

	free(comp->comp);
}


/*--------------------------------------------------*/
/* as the name says: init everything in the struc Composante*/
void initcomp(Composante *comp, int nbseq, FILE *ff, char *ledir)
{
	int i, j;
	if (ff == NULL)
		ff = stderr;

	comp->nc = nbseq;         /* number of composantes. At the start each sequence is in a different composante */

	comp->node_compid = (int *)malloc(sizeof(int) * nbseq);
	if (!comp->node_compid) fprintf(ff, "initcomp: MEMORY ERROR error can allocate node_compid\n"), fclose(ff), asap_exit_properly(ledir);

	comp->n_in_comp = (int *)malloc(sizeof(int) * nbseq);
	if (!comp->n_in_comp) fprintf(ff, "initcomp:MEMORY ERROR error can allocate n_in_comp\n"), fclose(ff), asap_exit_properly(ledir);

	comp->Sall_in_comp = (double *)malloc(sizeof(double) * nbseq);
	if (!comp->Sall_in_comp) fprintf(ff, "initcomp:MEMORY ERROR error can allocate Sall_in_comp\n"), fclose(ff), asap_exit_properly(ledir);

	comp->comp = (int **)malloc(sizeof(int*)*nbseq);
	if (!comp->comp) fprintf(ff, "initcomp:MEMORY ERROR error can allocate n_incomp\n"), fclose(ff), asap_exit_properly(ledir);

	comp->naltered = 0;
	comp->altered_comp = (int *)malloc(sizeof(int)*nbseq);
	if (!comp->altered_comp) fprintf(ff, "initcomp:MEMORY ERROR error can allocate n_inaltered_comp\n"), fclose(ff), asap_exit_properly(ledir);

	comp->altered_comp_prev=(int **)malloc(sizeof(int *)*nbseq);
	if (!comp->altered_comp_prev) fprintf(ff, "initcomp:MEMORY ERROR error can allocate altered\n"), fclose(ff), asap_exit_properly(ledir);


	comp->altered = (char *)malloc(sizeof(char) * nbseq);
	if (!comp->altered) fprintf(ff, "initcomp:MEMORY ERROR error can allocate altered\n"), fclose(ff), asap_exit_properly(ledir);



	for (i = 0; i < nbseq; i++)
	{
		comp->comp[i] = (int *)malloc(sizeof(int) * nbseq);
		if (!comp->comp[i]) fprintf(ff, "initcomp:MEMORY ERROR error can allocate n_incomp[%d]\n", i), fclose(ff), asap_exit_properly(ledir);
		for (j = 1; j < nbseq; j++)
			comp->comp[i][j] = -1;
		comp->altered_comp[i] = -1;
		comp->comp[i][0] = i;          /* at the begining, each composante has only its own sequence. They share the same id */
		comp->n_in_comp[i] = 1;
		comp->Sall_in_comp[i] = 0.0;
		comp->node_compid[i] = i;
		comp->altered[i] = 0;

		comp->altered_comp_prev[i]=NULL;


	}
}


/*--------------------------------------------------*/
/*Put a distance matrix (n  n) into a struct of size (n*(n-1))/2 which can be sorted according to distance*/

void mattolist(DistPair *dist_list , DistMat *d , float *max, float *min)
{
	int i,
	    j,
	    k = 0;

	*max = *min = d->dist[0][0];

	for (i = 0; i < d-> n - 1; i++)
		for (j = i + 1; j < d->n; j++)
		{
			dist_list[k].a = i;
			dist_list[k].b = j;
			dist_list[k].d = d->dist[i][j];

			if (*max < d->dist[i][j])
				*max = d->dist[i][j];
			if (*min > d->dist[i][j])
				*min = d->dist[i][j];

			dist_list[k].g = -1;

			k++;
		}

	qsort(dist_list, k, sizeof(DistPair), compareCase);


}
