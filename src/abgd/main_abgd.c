/*
	Copyright (C) 2008-2013 G Achaz

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

 	for more information, please contact guillaume achaz <achaz@abi.snv.jussieu.fr>/<gachaz@gmail.com>

*/

/******
        file     : main_abgd.c -- file I/O and distance-grid routines
        author   : gachaz
*****/

#define _GNU_SOURCE
/* Suppress warnings from third-party code — do not modify this block */
#if defined(__GNUC__) || defined(__clang__)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wunused-result"
#  pragma GCC diagnostic ignored "-Wunused-variable"
#  pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#  pragma GCC diagnostic ignored "-Wunused-function"
#  pragma GCC diagnostic ignored "-Wformat-overflow"
#  pragma GCC diagnostic ignored "-Wstringop-overflow"
#  pragma GCC diagnostic ignored "-Wimplicit-function-declaration"
#  pragma GCC diagnostic ignored "-Warray-bounds"
#  pragma GCC diagnostic ignored "-Wmisleading-indentation"
#  pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#endif
#undef _FORTIFY_SOURCE  /* prevent __sprintf_chk from glibc fortification */
#include <R.h>          /* Rprintf, REprintf, error */
#include <R_ext/Utils.h>  /* R_FlushConsole */
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>  /* errno */
#include "abgd.h"
#ifndef _WIN32
#include <unistd.h>
#include <strings.h>
#include <dirent.h>
#endif
#define NBCHARMALLOC 256
static char DEBUG;
static short verbose;
#define SIGN( a ) ( ( (a) > 0 )?1: ( ((a)==0)?0:-1)  )
static int Increase(const void *v1, const void *v2){  	return (int)SIGN( *((double *)v1) - *((double *)v2));  };
#undef SIGN

/*Read one fasta sequence in a file pointer store it in a fastaseq struct
returns 0 if some pbs or some pbs and 1 if everything ok*/
int ReadFastaSequence( FILE *f, struct FastaSeq *laseq)
	{
  	char *name;
  	char *seq;
  	int   n,c;
  	int   nalloc;
	char *nucs="ATGC-+NMRWSYKVHDBNZ";
		nalloc = 128;
	c=fgetc(f);
	if (c!='>')
		{return 0;}
	n=0;
	name= malloc(sizeof(char) * 128);
 	while (1){
 		c=fgetc(f);
 		if (c=='\n' || c=='\r' || c==10 || c==13  ) //do not store weird chars in names
 			break;
 		name[n++]=c;
 		if (n>127)
 			{
 			nalloc += 128;
 			name=realloc(name,sizeof(char) *nalloc);
 			}

 	}

 	name[n]='\0';
	laseq->name=malloc(sizeof(char)*(n+1));

  	strcpy(laseq->name, name);

  	seq = malloc(sizeof(char) * 128);      /* allocate seq in blocks of 128 residues */
  	nalloc = 128;
  	n = 0;

 	 while (1)
    	{
    	c=fgetc(f);
    	if (c==EOF )
    		break;
    	if (c=='>' )
    		{ungetc(c,f);break;} //put back in the stream the next new seq indicator
		if( c!='\n' && c!='\r' && c!='\t' && c!=' ')
		  {
		  if (strchr(nucs,toupper(c))==NULL) {Rprintf( "Your data contains at least one other symbol than ATGC-+NMRWSYKVHDBNZ<BR>Please correct it\n"); error("abgd: fatal error (code 1)");}/*weird symbol found*/

		  seq[n++]=toupper(c);
		  if (nalloc == n)
	    		{
	      		nalloc += 128;
	      		seq = realloc(seq, sizeof(char) * nalloc);
	    		}
			}
    	}

 	seq[n] = '\0';

	laseq->seq=malloc(sizeof(char)*n+1);
	strcpy(laseq->seq,seq);

  	free(seq);

	 if (c==EOF)
 		return(0);
	 else
		return(1);
}


/*Read a Fasta File and compute the distance Matrix according to method*/
struct DistanceMatrix compute_dis(FILE *f,int method,float ts_tv)
{
struct FastaSeq *mesSeq;

int i=1;;
int nalloc=256;
int nseq=0;
struct DistanceMatrix my_mat;   /* store distance matrix, names and matrix size */


mesSeq=(struct FastaSeq *)malloc (sizeof (struct FastaSeq ) *nalloc);

while (i)
	{

	i=ReadFastaSequence(f, &mesSeq[nseq]);
	nseq++;
	if (nseq==nalloc)
		{
		 nalloc+=256;
		 mesSeq=realloc(mesSeq,sizeof (struct FastaSeq ) * nalloc);
		if (mesSeq==NULL){Rprintf("not enough memory\n");error("abgd: fatal error (code 1)");}
		}
	}
if (check_names(mesSeq,nseq)==0)
	Rprintf("Two seqs found with same name. Exit\n"),error("abgd: fatal error (code 1)");

my_mat=GetDistMat(nseq,mesSeq, method,ts_tv, NULL,"");


for (i=0;i<nseq;i++)
	{free(mesSeq[i].seq);free(mesSeq[i].name);}
free(mesSeq);
return my_mat;
}


//returns the position of c in string l 1 to length(l) return 0 if not
int myIndex(char *l, char c)
{
int i,lo=strlen(l);

for (i=1;i<=lo;i++)
	if (l[i-1]==c)
	return(i);
return(0);
}


/*do some text editing */
void remplace(char *name,char c,char newc)
{
int i=0;

while (name[i]!='\0')
		{
		if (name[i]==c)
			name[i]=newc;
		i++;
		}

}


/*MEGA matrix is a plague because output can be customize a lot..  */
void readMatrixMega(FILE *f_in,struct DistanceMatrix *my_mat)
{

	int a,b,nbc=0,c,n;

	char *ligne,letter,nombre[17];

	int lower=-1;
	int nbcharmax=NBCHARMALLOC;
	int lindex=0;


	ligne=(char *)malloc(sizeof(char)*nbcharmax);

	my_mat->n=0;
	my_mat->names=NULL;
	my_mat->dist=NULL;

	Rprintf("Read Mega Format\n");

	//read the header
	while (1) {
		fscanf(f_in,"%[^\n]\n",ligne);

		char *s = ligne;
		while (*s) {
			*s = toupper((unsigned char) *s);
			s++;
		}

		if (feof(f_in)) Rprintf("pb reading file...\n"),error("abgd: fatal error (code 1)");

	 	if (strstr(ligne," OF TAXA :") !=NULL)
			my_mat->n=atoi(strchr(ligne,':')+1);

		if (strstr(ligne,"NTAXA=") !=NULL)
			my_mat->n=atoi(strchr(strstr(ligne,"NTAXA="),'=')+1);

		if (strstr(ligne,"DATAFORMAT=")!=NULL)
			{
			if (strstr(ligne,"LOWERLEFT")!=NULL)
				lower=1;
			else
				if (strstr(ligne,"UPPERRIGHT")!=NULL)
					lower=0;
				else
				Rprintf("Unknown data format\n"),error("abgd: fatal error (code 1)");
			}
		if (*ligne!='!' && strchr(ligne,';'))// we have reach the species desc line
			break;

	}


	Rprintf("%ld data\n",my_mat->n);

	if (my_mat->n ==0) Rprintf("abgd was not able to read your MEGA file: [TAXA] number not in the header\n"),error("abgd: fatal error (code 1)");


	nbc=0;


//do some memory initialisation
	my_mat->names = (char **)malloc( sizeof(char *)* my_mat->n );
	if( ! my_mat->names )REprintf( "read_distmat: cannot allocate my_mat->names, bye"), error("abgd: fatal error (code 4)");

	my_mat->dist = (double **)malloc( sizeof(double *)* my_mat->n );
	if( ! my_mat->dist )REprintf( "read_distmat: cannot allocate my_mat->dist, bye"), error("abgd: fatal error (code 4)");
	for(a=0;a<my_mat->n; a++){
		my_mat->dist[a] = (double *)malloc( sizeof(double)* my_mat->n );
		if( ! my_mat->dist[a] )
			REprintf( "read_distmat: cannot allocate my_mat->dist[%d], bye",a), error("abgd: fatal error (code 4)");
		}


	a=0;


//read species name
	while (1)
		{
			lindex=0;
			do
				fscanf(f_in,"%[^\n]\n",ligne);
			while (strlen(ligne)<=1); //skip white lines if needed

			if (strlen(ligne)<=1) break;

 			if (strchr(ligne,'#')!=0)
 					lindex=myIndex(ligne,'#');
 				else
 					{
 					if (strchr(ligne,']'))
 				 		lindex=myIndex(ligne,']');
					else
 						lindex=0;
 					}
 			n=strlen(ligne+lindex);
 			my_mat->names[a]= (char *)malloc( sizeof(char)*(n+1));
 			strncpy(my_mat->names[a],ligne+lindex,n);
 			my_mat->names[a][n]='\0';

 			if (strchr(my_mat->names[a],'('))
 				remplace(my_mat->names[a],'(','_');
 			if (strchr(my_mat->names[a],')'))
 				remplace(my_mat->names[a],')','_');


 			a++;

 			if (a==my_mat->n)
 				break;

		}



	do {
		letter=fgetc(f_in);
		if (feof(f_in)) Rprintf("error reading values\n"),error("abgd: fatal error (code 1)");
		}
	while (letter!=']');	//last line read should be very long but some empty lines occur ....


letter=fgetc(f_in); //be sure we are on line 1 of matrix
for (a=0;a<my_mat->n;a++){
		c=0;
		while( letter != ']' && !feof(f_in)) //reading after the name.
			letter=fgetc(f_in);

		if (feof(f_in))Rprintf("problem reading your file\n"),error("abgd: fatal error (code 1)");

		for (b=0;b<=a;b++)
			{
			c=0;
			while( (letter=fgetc(f_in)) == ' ');
			if (feof(f_in) ) break;
			while ( (letter != ' ') && (letter!='\n') && (letter != 10 ) && (letter!=13) && (letter!='[')){
				if (letter==',') letter='.';
				if (letter=='?')
				{
				REprintf("**Warning distance between %s and %s is unknown,exiting<BR>\n",my_mat->names[a],my_mat->names[b]);error("abgd: fatal error (code 1)");
				}


				nombre[c]=(char) letter;
				c++;
				if (c>15) {Rprintf("too much char %d \n",letter);break;}

				letter=fgetc(f_in);
				if (feof(f_in)) break;
				}
	    	nombre[c]='\0';
	    	if (c==0)
	    		my_mat->dist[b][a]=my_mat->dist[a][b]=0;
	    	else
				my_mat->dist[b][a]=my_mat->dist[a][b]=strtod(nombre,NULL);

			}

		while (letter != 10  && letter != ']'  && letter!=13 && letter !='\n'&& !feof(f_in))/* go to end of line*/
			{letter=fgetc(f_in);}
		if (a!=my_mat->n -1 && feof(f_in))
			Rprintf("pb reading matrix CSV\n"),error("abgd: fatal error (code 1)");

	}

	free(ligne);

}

/*
	Takes a distance file as an input (phylip or MEGA format)
	Return a struct with a distance matrix
*/
struct DistanceMatrix read_distmat(FILE *f_in, float ts_tv){

	int a=0,b,c;
	int letter;
	char first_c;
	int kk=0;
	long ppos=0;
	int toalloc=0;
	struct DistanceMatrix my_mat;

	my_mat.ratio_ts_tv= ts_tv;
	first_c=fgetc(f_in);

	rewind (f_in);
	if(first_c=='#')
 	    readMatrixMega(f_in,&my_mat);
 	else {
 		Rprintf("Phylip distance file\n");
		my_mat.n=0;
		my_mat.names=NULL;
		my_mat.dist=NULL;

		fscanf( f_in, "%ld", &my_mat.n);
		while( (letter=fgetc(f_in)) != '\n' && !feof(f_in)) kk++;

		if (feof(f_in))Rprintf("Pb with file\n"),error("abgd: fatal error (code 1)");

		if (kk>10){
		Rprintf("There might be a problem with your Phylip distance file\n");
		Rprintf("If you have a MEGA file stop this by hitting ctrL C and check the help\n");
		}

		my_mat.names = (char **)malloc( (size_t) sizeof(char *)*my_mat.n );
		if( ! my_mat.names )REprintf( "read_distmat: cannot allocate my_mat.names, bye"), error("abgd: fatal error (code 4)");

		my_mat.dist = (double **)malloc( (size_t) sizeof(double *)*my_mat.n );
		if( ! my_mat.dist )REprintf( "read_distmat: cannot allocate my_mat.dist, bye"), error("abgd: fatal error (code 4)");
		for(a=0;a<my_mat.n; a++){
			my_mat.dist[a] = (double *)malloc( (size_t) sizeof(double)*my_mat.n );
			if( ! my_mat.dist[a] )
				REprintf( "read_distmat: cannot allocate my_mat.dist[%d], bye",a), error("abgd: fatal error (code 4)");
		}

		for(a=0;a<my_mat.n; a++){

			c=0;
			toalloc=0;
			ppos=ftell(f_in);
			while( ((letter=fgetc(f_in)) != ' ')&& (letter !='\t')){

				if(c < SIZE_NAME_DIST-1){

					toalloc++;
				}

			}
		my_mat.names[a] = (char *)malloc( (size_t) sizeof(char)*(toalloc+1));
		fseek(f_in,ppos,SEEK_SET);
		while( ((letter=fgetc(f_in)) != ' ')&& (letter !='\t') ) {

				if(c < SIZE_NAME_DIST-1){

					my_mat.names[a][c] = (char)letter;
					c++;
				}

			}
			my_mat.names[a][c]=0;

			for(b=0;b<my_mat.n; b++)
				{
				fscanf( f_in, "%lf", ( my_mat.dist[a] + b) );
				}

			while( ( (letter=fgetc(f_in)) != '\n') && (letter !='\t'));
		}

		fclose(f_in);
	}

	return my_mat;

}


double * Compute_myDist( double minDist, double MaxDist, int nbStepsABGD ){

	double *myDist;
	double myScale,myInit;
	int ii;

	myDist = (double *) malloc( (size_t) sizeof(double) * nbStepsABGD );

	myDist[0]=minDist;

	myScale=log10( MaxDist/myDist[0] ) / (float)( nbStepsABGD-1.0 );

	myInit=log10( myDist[0] );

 	for (ii=1;ii< nbStepsABGD-1	;ii++)	{
 		myDist[ii]=pow(10,myInit+(myScale*ii));
 		}
 	myDist[ii]=MaxDist;


	return myDist;

}

#if defined(__GNUC__) || defined(__clang__)
#  pragma GCC diagnostic pop
#endif
