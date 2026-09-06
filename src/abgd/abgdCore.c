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
        file     : abgdCore.c -- core algorithm for automatic barcode gap discovery
        function : rank values and find a gap in their density - devised to find the limit
	           	between population and species in phylogeography (done with/for Nicolas Puillandre)

        created  : April 2008
        modif    : Nov 09 --stable version-- (with a minimum of slope increase)
        modif    : April 10 --stable version-- (with (A) some minimum divergence and (B) at least 1.5 times of slope increase)

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
#include <sys/stat.h>
#include <errno.h>  /* errno */
#include "abgd.h"
static char DEBUG;
static short verbose;



#define ABS( x )  (((x)>0)?(x):(-x))


static void exit_properly(char *ledir)
{
	char commande [1024];
	if (strlen (ledir) >1)
	{
	sprintf(commande, "mv %s%s/results_.html %s%s/results.html", WORKDIR, ledir, WORKDIR, ledir);

	system(commande);
	}
	error("abgd: fatal error (code 1)");
}

/*out put some error in html way because this is mainly important for cgi scripts but some fonction of Core are calling it*/
void html_error(FILE *fres,int nb)
{
fprintf(fres,"******abgdWeb Error %d ****<BR><B><H4>",nb);
switch (nb)
	{
	case 1:
	case 2:
	case 3:
	case 4:

	case 5:

	case 10:
	fprintf(fres,"Pb while reading CGI data. If this problem persists please contact <A HREF=\"mailto:%s\"> web site administrator</A>\n",WEB_ADMIN) ;
	break;

	case 8:
	fprintf(fres,"Please try to rename your file with \".txt\" extension. If the problem persists or if the file extension is already \".txt\", please contact <A HREF=\"mailto:%s\"> web site administrator</A>\n",WEB_ADMIN) ;
	break;

	case 11:
	fprintf(fres,"Your data appears to be a matrix distance but some characters have been found instead of numerical value\n") ;
	fprintf(fres,"(inf or sup matrix are not supported _ yet_)\n") ;
	break;

	case 20:
	case 56:
	case 144:
	fprintf(fres,"can't open file. please contact <A HREF=\"mailto:%s\"> web site administrator</A>\n",WEB_ADMIN) ;
	break;

	case 60:
	fprintf(fres,"Your data contains at least one other symbol than ATGC-+NMRWSYKVHDBNZ<BR>Please correct it\n");
	break;

    case 50:
    case 66:
	fprintf(fres,"Weird FASTA format . Please check your data\n");
    break;

    case 95:
	fprintf(fres,"Please check the P value you entered\n");
    break;

    case 99:
	fprintf(fres,"FASTA sequences not all same size . Please check your data\n");
    break;

    case 100:
	fprintf(fres,"Length of seq appears to be 0\n");
    break;

  	case 105:
	fprintf(fres,"Either you have a CSV MEGA matrix and didn't check the option in previous page, <BR>either your matrix is not well formated\n");
    break;

	case 110:
	fprintf(fres,"ABGD can't deal with sequences which names are only composed of digits, or with \": , ( )\" present in names. <BR>Please add at least one alphabetic character in sequences names or remove others\n");
	break;

    case 111:
	case 112:
    case 155:
    case 55:
    case 44:
    fprintf(fres,"Memory pb. can't malloc. Try with smaller data file\n");
    break;

   	case 177:
     fprintf(fres,"Minimum number is 2. Bye\n");
    break;


    case 200:
    case 300:
	fprintf(fres,"pb while choosing distance model\n");
    break;
    case 255:
    fprintf(fres,"newick file has some pb.. This should never arrives so.. try again\n");
    break;

    case 257:
    fprintf(fres,"MEGA distance file not well formated. Please resave your file with MEGA\n");
    break;

    case 344:

    fprintf(fres,"MEGA CSV distance file not well formated. Please resave your file with MEGA\n");
    break;

    case 343:
    fprintf(fres,"Pb reading MEGA CSV distance file . Keyword \"Table\" not found at bottom of the file\n");
    break;

    default:
    fprintf(fres,"Undocumented error \n");
    }

    fprintf(fres,"</H4></B>");
 }


/********************

	ABGD

*********************/

short compare_DNA( char s1, char s2 ){

	/*
		if we are in standard DNA
	*/
	if(  (s1=='A'||s1=='C'||s1=='G'||s1=='T') &&  (s2=='A'||s2=='C'||s2=='G'||s2=='T') ){
		if	(s1==s2 )return 1;
		else			return 0;
	}

	/*
		What about deletion
	*/

	if(s1 == '-' || s2 =='-' || s1 =='+' || s2 == '+'){                      /* consider deletion as a diff only if the alignemt has been recoded */

		if(  (s1 == '-' && s2 == '+') || (s2 == '-' && s1 == '+') )
			return 0;
		else
			return 1;
	}

	/*
		if there is an N
	*/
	if( s1 == 'N' || s2 == 'N')
		return 1;


	/*
		if only one is an A
	*/
	if(s1=='A'){
		if(  s2=='M'||s2=='R'||s2=='W'||s2=='V'||s2=='H'||s2=='D' )	return 1;
		else 							return 0;
	}
	if(s2=='A'){
		if(s1=='M'||s1=='R'||s1=='W'||s1=='V'||s1=='H'||s1=='D')	return 1;
		else 							return 0;
	}

	/*
		if only one is a C
	*/
	if(s1=='C'){
		if( s2=='M'||s2=='S'||s2=='Y'||s2=='V'||s2=='H'||s2=='B' )	return 1;
		else 							return 0;
	}
	if(s2=='C'){
		if(  s1=='M'||s1=='S'||s1=='Y'||s1=='V'||s1=='H'||s1=='B' )	return 1;
		else 							return 0;
	}

	/*
		if only one is a G
	*/
	if(s1=='G'){
		if( s2=='R'||s2=='S'||s2=='K'||s2=='V'||s2=='D'||s2=='B' )	return 1;
		else 							return 0;
	}
	if(s2=='G'){
		if( s1=='R'||s1=='S'||s1=='K'||s1=='V'||s1=='D'||s1=='B' )	return 1;
		else 							return 0;
	}

	/*
		if only one is a T
	*/
	if(s1=='T'){
		if( s2=='W'||s2=='Y'||s2=='K'||s2=='H'||s2=='D'||s2=='B' )	return 1;
		else 							return 0;
	}
	if(s2=='T'){
		if( s1=='W'||s1=='Y'||s1=='K'||s1=='H'||s1=='D'||s1=='B' )	return 1;
		else 							return 0;
	}


	/*
		More complex case
	*/
	if( (s1=='M' && s2=='K') || (s1=='K' && s2=='M')  )
		return 0;

	if( (s1=='R' && s2=='Y') || (s1=='Y' && s2=='R')  )
		return 0;

	if( (s1=='W' && s2=='S') || (s1=='S' && s2=='W')  )
		return 0;


	/*
		Else there is at least one overlap
	*/
	return 1;

}


/*
	This function computes the derivative using
	winsiz and find its maximal value. Actually,
	it scans for a peak and find it summit using both
	left (low values) and right (high values) sides by
	averaging them.
*/

struct Peak FindFirstPeak( double *Array, long N, int winsiz, short output_slope, double *Pi, double PriorDist ,double minSlopeIncrease){

	long i,             /* Indice of the slope */
	     top=0;         /* indices the summit of the preak */


	double *Slope;           /* Slope <=> derivative of array */
	double SlopeMax;         /* Current max of Slope */


	float Mean_i=0;          /* The a priori rank of the summit --> then it is set to j+0.5 */

	double Mean_dist=0.0;    /* The distance that corresponds to SlopeMax */

	struct Peak my_abgd;     /* The structure that contain both the estimated indice and distance */

	long wt;
	long ct;


	my_abgd.Dist = -1;
	my_abgd.Rank = -1;
	my_abgd.theta_hat = 0;

	Slope = (double *)malloc(  (size_t) (N-winsiz+1) *sizeof(double) );
	if(!Slope )REprintf( "FindMaxDifferiental: cannot allocate SlopeS --%ld double--, bye\n", N-winsiz+1 ), error("abgd: fatal error (code 2)");


	for(i=0; i <= N-winsiz ; i++)
		Slope[i] = (Array[i+winsiz-1]-Array[i])/(double)(winsiz-1);


	for(i=1; i<N && Array[i] <= PriorDist  ; i++);
	my_abgd.theta_hat = Pi[i-1];


	if(output_slope)
		for(i=0; i <= N-winsiz ; i++)
			Rprintf("slope %ld %.10f ; dist %ld %f\n", i, Slope[i], i,  Array[i] );

	if(DEBUG){
		for(i=0;i<N;i++){
			if(i<N-winsiz)
				Rprintf( "[%ld] ; Val= %.5f -> %.5f (ws: %d) ; Slope= %f\n",i,Array[i],Array[i+winsiz-1], winsiz, Slope[i] );
			else
				Rprintf( "[%ld] ; Val= %.5f \n",i, Array[i]  );
		}
		error("abgd: fatal error (code 5)");
	}


	i=0;
	top=0;
	my_abgd.Dist = -1;
	my_abgd.Rank = -1;


	SlopeMax = Slope[0];
	for(i=1; i < N-winsiz && Array[i+winsiz] <= PriorDist; i++){
		if( Slope[i] > SlopeMax )
			SlopeMax = Slope[i];
	}


	while( i<N-winsiz ){


		while(  i<N-winsiz && Slope[i+1] >= Slope[i] ){     /* Find a Local Maxima */
			i++;
		}

		top = i;
		while( i<N-winsiz  && ABS(i-top) <= winsiz/10  ){                 /* explore winsiz/10 from slope[top] on right */

			if(Slope[i] > Slope[top]){
				top=i;
			}

			i++;
		}

		Mean_i=0;
		Mean_dist=0;

		ct = top;

		for( wt = winsiz-1; wt>=2; wt--){

			if( Array[ct+wt-1]-Array[ct] <   Array[ct+1+wt-1]-Array[ct+1] && ct < N-wt-1)
				ct++;
		}
		Mean_dist = (Array[ct]+Array[ct+1])/2.0;


		if( Mean_dist  > 2.581 *2* my_abgd.theta_hat &&
		    Slope[top] > minSlopeIncrease*SlopeMax){

			my_abgd.Dist = Mean_dist;
			my_abgd.Rank = ct+0.5;
			my_abgd.theta_hat = Pi[ct];

			if( Pi[ct] <= my_abgd.theta_hat && my_abgd.Rank == -1 ){

				SlopeMax = 0;
				i=0;
				top=0;
			}
			else{

				break;
			}

		}
		else{

			SlopeMax= ( Slope[top]>SlopeMax && Pi[ct] <= my_abgd.theta_hat )?Slope[top]:SlopeMax;

			i =  (top>ct)?top+1:ct+1;
			while( i<N-winsiz && Slope[i+1] <=  Slope[i] )           /* go downhill as far as we can */
				i++;

		}

	}

	free(Slope);

	return my_abgd;
}


struct Peak find_abgd( double *Array, long N, long windsize_min, long windsize_max, short output_slope, double PriorDist ,double minSlopeIncrease ){

	int c,i;
	int stable=0;

	int windsize_step = (windsize_min>10)?windsize_min/10:1;

	struct Peak my_abgd;

	double *Pi;              /* average from array[0] to array[i] */

	double stable_dist=-1;

	my_abgd.Dist = -1;
	my_abgd.Rank = -1;
	my_abgd.theta_hat = -1;

	Pi = (double *)malloc(  (size_t) N *sizeof(double) );
	if(!Pi )REprintf( "FindMaxDifferiental: cannot allocate Pi --%ld double--, bye\n", N ), error("abgd: fatal error (code 2)");

	for(Pi[0]=Array[0], i=1; i<N ; i++)
		Pi[i] = (Array[i] + Pi[i-1]*i)/(i+1.0);


	for(c=windsize_min; c <= windsize_max && stable<3; c+=windsize_step){

			my_abgd = FindFirstPeak( Array, N, c, output_slope, Pi,  PriorDist, minSlopeIncrease );

			if(DEBUG)
				REprintf("abs( %f - %f )= %f vs %f\n", my_abgd.Dist,stable_dist, fabs(my_abgd.Dist-stable_dist) , 0.1*stable_dist );

			if( my_abgd.Dist != -1 && fabs( my_abgd.Dist-stable_dist) < 0.1*stable_dist ){

				stable++;
			}
			else{
				stable=1;
				stable_dist=my_abgd.Dist;
			}

			if(DEBUG)
				Rprintf("w: %d  d_peak: %f r_peak: %.1f\n", c, my_abgd.Dist, my_abgd.Rank);


	}

	if(my_abgd.Dist == -1){

		my_abgd.Dist=Array[N-1];
		my_abgd.Rank = N+0.5;

	}

	free(Pi);


	return my_abgd;
}



/********************

	Distance Matrix

*********************/

void distancesimple(struct FastaSeq *mesSeqs,int l,struct  DistanceMatrix  my_mat,FILE *fres,char *ledir)

{
	char *s1,*s2,c1,c2;;
	double v=0;
	int i,a,b,ncor=0;
	int nseq=my_mat.n;

	if (l==0)
		html_error(fres,100);
	;
	for (a=0;a<nseq-1;a++)
	{
		s1= mesSeqs[a].seq;
		my_mat.dist[a][a]=0;

		for (b=a+1;b<nseq;b++)
		{
			v=0;ncor=0;
			s2= mesSeqs[b].seq;
			if (check_compat(s1,s2, l)==0)
			{fprintf(fres,"<H4>Sequence %s and %s have no common site. Distance can't be computed. Bye </H4><BR>",my_mat.names[a],my_mat.names[b]);fclose(fres);exit_properly(ledir);}

			for (i=0;i<l;i++)
				{
				c1=toupper(*(s1+i));
				c2=toupper(*(s2+i));
				if(compare_DNA(c1,c2)==0)
				  v=v+1;

				if ( ( (*(s1+i) )=='-') || ((*(s2+i) )=='-') ||	((*(s1+i) )=='N') || ((*(s2+i) )=='N' ))
				  ncor++;



				}

			v=((v+1)/(double)(l-ncor+1));
			my_mat.dist[a][b]=my_mat.dist[b][a]=v;
		}
	}
}


/*compute distance according to Jukes Cantor method*/
/*do not take in consideration gaps or N*/


void distanceJC69 (struct FastaSeq *mesSeqs, int l, struct  DistanceMatrix  mymat,FILE *fres,char *ledir)
{
double v=0,h;
int i,newl=0;
char c1,c2;
int a,b;
char *s1,*s2;
int nseq=mymat.n;


if (l==0)
html_error(fres,100);

for (a=0;a<nseq-1;a++)
	{
	s1= mesSeqs[a].seq;
	mymat.dist[a][a]=0;
	for (b=a+1;b<nseq;b++)
		{
		s2= mesSeqs[b].seq;
		newl=0;v=0;
		if (check_compat(s1,s2, l)==0)
			{fprintf(fres,"<H4>Sequence %s and %s have no common site. Distance can't be computed. Bye </H4><BR>",mymat.names[a],mymat.names[b]);fclose(fres);exit_properly(ledir);}
		for (i=0;i<l;i++)
			{
			c1=toupper(*(s1+i));
			c2=toupper(*(s2+i));
			if (c1!='-'&& c1!='N' && c2!='-' &&c2!='N')
                newl++;
			if (compare_DNA(c1,c2)==0)
					v=v+1;
			}
			if (newl!=0)
			  v=v/(double)newl;

			if(v>0.74)v=0.74;

			h=(-3.0/4.0)*log(1.0-((4.0/3.0)*v));

			if (h==-0)
				h=0;
			mymat.dist[a][b]=mymat.dist[b][a]=h;

		}
	}
Rprintf("doneJC\n");
}


long del_sequences(char *seq1, char *seq2, long L){

	long i, del=0;

	for(i=0;i<L;i++)
		if( *(seq1+i) == '-' || *(seq2+i) == '-')
			del++;

	return del;
}

char IsTransition( char nt1, char nt2 ){

	if(nt1 == 'T'){
		if( nt2 == 'C' || nt2 == 'S' ||  nt2 == 'M' || nt2 == 'V')
			return 1;
		else
			return 0;
	}
	if(nt1 == 'C'){
		if( nt2 == 'T' || nt2 == 'W' ||  nt2 == 'K' || nt2 == 'D')
			return 1;
		else
			return 0;
	}
	if(nt1 == 'A'){
		if( nt2 == 'G' || nt2 == 'S' ||  nt2 == 'K' || nt2 == 'B')
			return 1;
		else
			return 0;
	}
	if(nt1 == 'G'){
		if( nt2 == 'A' || nt2 == 'W' ||  nt2 == 'M' || nt2 == 'H')
			return 1;
		else
			return 0;
	}
return(0);
}

char IsTransversion( char nt1, char nt2 ){

	if( compare_DNA(nt1,nt2)==0 && IsTransition(nt1,nt2 )==0 )
		return 1;
	else
		return 0;

}
void transition_transversion_sequences(char *seq1, char *seq2, long L, long *tsi, long *tsv){

	long i;

	*tsi=*tsv=0;

	for( i=0 ; i<L ; i++ )
		if ( compare_DNA( *(seq1+i), *(seq2+i) ) == 0 ){
			if( IsTransition( *(seq1+i), *(seq2+i) ) == 1 )
				(*tsi)++;
			else
				(*tsv)++;
		}

	return;
}

double P_given_t_R( double t, double R ){
	return 0.25 - 0.5*exp( - t*(2*R+1)/(R+1) ) + 0.25*exp(- 2*t/(R+1));
}
double Q_given_t_R( double t, double R ){
	return 0.5 - 0.5*exp(- 2*t/(R+1));
}
double compute_logL_given_t_R( long nsites, long n_tsv, long n_tsi, double t, double R ){
	return nsites*log(0.25) +
	       (nsites- n_tsv-n_tsi)*log( 1.00 - P_given_t_R(t,R)  - Q_given_t_R(t,R) ) +
	       n_tsi * log( P_given_t_R(t,R) ) +
	       n_tsv * log( Q_given_t_R(t,R) );
}

double compute_k80( long nsites, long n_tsv, long n_tsi ){

	double Q=n_tsv/(double)nsites,
	       P=n_tsi/(double)nsites;
	return -0.25 * log( (1-2*Q)*(1-2*P-Q)*(1-2*P-Q) );

}

double find_ML_t_given_R( double R, long nsites, long n_tsv, long n_tsi ){

	double t = compute_k80( nsites, n_tsv, n_tsi );

	double epsilon=1e-7;
	double eps=1e-3;

	while( eps >= epsilon ){

		while( compute_logL_given_t_R( nsites, n_tsv, n_tsi, t+eps, R ) > compute_logL_given_t_R( nsites, n_tsv, n_tsi, t, R )  )
			t+=eps;
		while( compute_logL_given_t_R( nsites, n_tsv, n_tsi, t-eps, R ) > compute_logL_given_t_R( nsites, n_tsv, n_tsi, t, R )  )
			t-=eps;

		eps *= 0.1;

	}

	return t;
}

void distanceK80 (struct FastaSeq *mesSeqs,int l,struct  DistanceMatrix  my_mat,FILE *fres,char *ledir){
	int i,j;
	long tsi,tsv;
	long del;
int nseq=my_mat.n;


	for(i=0;i<nseq; i++){
		Rprintf("seq:%d\n",i);
		my_mat.dist[i][i]=0;

		for(j=i+1;j<nseq; j++){

			if (check_compat(mesSeqs[i].seq, mesSeqs[j].seq, l)==0)
				{fprintf(fres,"<H4>Sequence %s and %s have no common site. Distance can't be computed. Bye </H4><BR>",my_mat.names[i],my_mat.names[j]);fclose(fres);exit_properly(ledir);}

			transition_transversion_sequences(mesSeqs[i].seq, mesSeqs[j].seq, l, &tsi, &tsv);

			del = del_sequences(mesSeqs[i].seq, mesSeqs[j].seq,l);

			my_mat.dist[i][j] =my_mat.dist[j][i] = find_ML_t_given_R( my_mat.ratio_ts_tv, l-del, tsv, tsi );

			if (my_mat.dist[i][j]==-0)
				my_mat.dist[i][j] =my_mat.dist[j][i] = 0;


		}
	}

}



/*check if we have at least one common symbol beetween the 2 seqs*/
int check_compat(char *s1,char *s2,int l)
{
int i,newl=0;
char c1,c2;
for (i=0;i<l;i++)
	{
	c1=toupper(*(s1+i));
	c2=toupper(*(s2+i));
	if ((c1=='-' && c2!='-') || (c2=='-' && c1!='-') || (c2=='-' && c1=='-'))
		newl++;
	}


return (l-newl);
}




/*compute distance according to Tamura Nei method*/
/*do not take in consideration gaps or N*/
void distanceTN93(struct FastaSeq *mesSeqs,int l,struct  DistanceMatrix  my_mat, FILE *fres,char *ledir)
{
double v=0;
double transitions=0,transversions=0,p,q,p1,p2,ga,gg,gc,gt,gr,gy;
double transitionsag=0,transitionsct=0;
char c1,c2;
double f[5];
char nuc[5]="ACGT-";
int i,newl=0;
char *s1,*s2;
int a, b;
int nseq=my_mat.n;


if (l==0)
html_error(fres,100);

for (a=0;a<nseq-1;a++)
	{
	s1= mesSeqs[a].seq;
	my_mat.dist[a][a]=0;
	for (b=a+1;b<nseq;b++)
		{
		s2= mesSeqs[b].seq;
		if (check_compat(s1,s2, l)==0)
			{fprintf(fres,"<H4>Sequence %s and %s have no common site. Distance can't be computed. Bye </H4><BR>",my_mat.names[a],my_mat.names[b]);fclose(fres);exit_properly(ledir);}
		newl=0;v=0;
		for (i=0;i<5;i++)
			f[i]=0;
		for (i=0;i<l;i++)
			{
			c1=toupper(*(s1+i));
			c2=toupper(*(s2+i));
			if (strchr(nuc,c1) && strchr(nuc,c2) )
				{
				f[(int)(strchr(nuc,c1)-nuc)]++;
				f[(int)(strchr(nuc,c2)-nuc)]++;
				}

			if (compare_DNA(c1,c2)==0)
				{
				v++;
				if ((c1=='A' && c2=='G') || (c2=='A' && c1=='G') )transitionsag++;
				else
				if ((c1=='C' && c2=='T') || (c2=='C' && c1=='T')) transitionsct++;

				}
			}


			transversions=v-transitions;
			q=transversions/(double)newl;
			p=transitions/(double)newl;
			p1=transitionsag/(double)newl;
			p2=transitionsct/(double)newl;
			ga=f[0]/(2.0*newl);
			gc=f[1]/(2.0*newl);
			gg=f[2]/(2.0*newl);
			gt=f[3]/(2.0*newl);
			gr=ga+gg;
			gy=gc+gt;
			v = ((-2*ga*gg/gr) * log (1.0 - ((gr/(2.0*ga*gg))*p1) - ((1/(2*gr))*q))) -
			(( (2*gt*gc)/gy) * log(1.0 - ((gy/(2.0*gt*gc))*p2)-((1/(2*gy))*q))) -
			((2.0* ((gr*gy)-( (ga*gg*gy)/gr) - ((gt*gc*gr)/gy))) *log (1.0 - ((1.0/(2.0*gr*gy))*q))) ;
			if (v==-0)
				v=0;
			my_mat.dist[a][b]=my_mat.dist[b][a]=v;

		}
	}
}


/*
take a fasta file as input and compute distance as method(seq1,seq2,length)
*/
struct DistanceMatrix GetDistMat (int nseq, struct FastaSeq *mesSeqs, int method,float ts_tv,FILE *fres,char *ledir)
{

	struct DistanceMatrix my_mat;
	void (*distance) (struct FastaSeq *,int ,struct DistanceMatrix ,FILE *,char *)=NULL;
	int a;
	int length;

	length=strlen(mesSeqs[0].seq);

	switch(method){

		case 0:
			distance=distanceK80;
			Rprintf("Kimura distance\n");
			break;

		case 1:
			distance=distanceJC69;
			Rprintf("Jukes Cantor distance\n");
			break;

		case 2:
			distance=distanceTN93;
			break;

		case 3:
			distance=distancesimple;
			Rprintf("Simple distance\n");
			break;
	}

	my_mat.names = (char **)malloc( (size_t) sizeof(char *)*nseq+1);

	my_mat.n=nseq;
	my_mat.ratio_ts_tv=ts_tv;


	if( ! my_mat.names )Rprintf("read_distmat: cannot allocate my_mat.names, bye<BR>"), error("abgd: fatal error (code 4)");

	for(a=0;a<my_mat.n; a++){
			my_mat.names[a] = (char *)malloc( (size_t) sizeof(char)*(strlen(mesSeqs[a].name) +1));
		if( ! my_mat.names[a] )
			Rprintf( "read_distmat: cannot allocate my_mat.names[%d], bye<BR>",a), error("abgd: fatal error (code 4)");
	strcpy(my_mat.names[a],mesSeqs[a].name);


	}

	my_mat.dist = (double **)malloc( (size_t) sizeof(double *)*my_mat.n );
	if( ! my_mat.dist)Rprintf( "read_distmat: cannot allocate my_mat.dist, bye<BR>"), error("abgd: fatal error (code 4)");
	for(a=0;a<my_mat.n; a++){
		my_mat.dist[a] = (double *)malloc( (size_t) sizeof(double)*my_mat.n );
		if( ! my_mat.dist[a] )
			Rprintf( "read_distmat: cannot allocate my_mat.dist[%d], bye<BR>",a), error("abgd: fatal error (code 4)");
               my_mat.dist[a][a]=0;

	}

	distance(mesSeqs,length,my_mat,fres,ledir);
	return my_mat;

}


void free_distmat(  struct DistanceMatrix mat ){


	int a;
	for(a=0;a<mat.n;a++){
		free(mat.dist[a]);
		free(mat.names[a]);
	}

	free(mat.dist);
	free(mat.names);
}



/*
	In mask 1 means yes take it, 0, don't use it
*/
double *matrix2list( struct DistanceMatrix  distmat, char *mask, long *Nval ){

	int i,j;
	int nseq=0;

	double *Pairs;

	for(i=0;i<distmat.n;i++)
		nseq+=mask[i];

	Pairs = (double *)malloc( ((nseq*(nseq-1))/ 2 )*sizeof(double) );
	if(!Pairs)REprintf( "matrix2list: cannot allocate Pairs, bye\n"), error("abgd: fatal error (code 4)");


	*Nval=0;
	for(i=0; i<distmat.n;i++){

		if( mask[i] == 0 )
			continue;
		if (verbose) Rprintf("%d non masque\n",i);
		for(j=i+1;j<distmat.n; j++)
			{
			if( mask[j] )
				Pairs[ (*Nval)++ ] = distmat.dist[i][j];

			}

	}


	return Pairs;

}



/********************

	  Groups (graph composantes) extraction

*********************/

/*
	recursive function that add a node to the current composante and
	check for next ones in the composante
*/
void setcomp( int node, int compid, int * node_compid, struct DistanceMatrix matrix, double max_dist, char *mask){


	int j;

	node_compid[node] = compid;

	for( j=0; j<matrix.n; j++  ){

		if( mask[j] == 0 )
			continue;

		if( matrix.dist[node][j] < max_dist ){

			if( node_compid[j] == 0 )
				setcomp( j, compid, node_compid, matrix, max_dist, mask);
			else{

				if( node_compid[j] != compid ){
					Rprintf("setcomp: really strange ??? This should not happen %d in comp %d && %d in comp %d\n", node, compid, j , node_compid[j] );
					error("abgd: fatal error (code 1)");
				}
				else
					continue;
			}
		}
	}
}


/*
	take a distance matrix with a maximum distance
	and return a list with the composante id of each sequence
	-- all connections are propagated --
*/
struct Composante compute_node_compid(  struct DistanceMatrix matrix, double max_dist, char *mask ){


	int row=0;
	struct Composante my_comp;
	int i;


	my_comp.nm = 0;
	for(i=0; i<matrix.n; i++)
		if( mask[i] == 0 )
			my_comp.nm++;

	my_comp.nc        = 1;
	my_comp.nn        = matrix.n - my_comp.nm;
	my_comp.comp      = NULL;
	my_comp.n_in_comp = NULL;


	my_comp.node_compid = (int *)calloc( (size_t) matrix.n , (size_t)sizeof(int) );
	if( !my_comp.node_compid )REprintf( "compute_composante: cannot allocate comp, bye"),error("abgd: fatal error (code 4)");


	for( row=0; row < matrix.n ; row++){

		if(my_comp.node_compid[row] > 0 || mask[row] == 0)
			continue;

		setcomp( row, my_comp.nc, my_comp.node_compid, matrix, max_dist, mask );

		my_comp.nc++;

	}


	my_comp.nc--;

	for( i=0 ; i < matrix.n ; i++ )
		my_comp.node_compid[i]--;

	return my_comp;
}

/*
	From a list where each node has a composante,
	generate the composante list
*/
struct Composante extract_composante(  struct DistanceMatrix matrix, double max_dist, char *mask ){

	int i;
	struct Composante my_comp;


	my_comp = compute_node_compid(  matrix, max_dist, mask );


	my_comp.n_in_comp = (int *)calloc( (size_t)my_comp.nc , (size_t)sizeof(int) );
	if(!my_comp.n_in_comp)REprintf( "extract_composante: cannot allocate my_comp.n_in_comp, bye\n"), error("abgd: fatal error (code 2)");

	my_comp.comp = (int **)malloc( (size_t)my_comp.nc * (size_t)sizeof(int*) );
	if(!my_comp.comp)REprintf( "extract_composante: cannot allocate my_comp.comp, bye\n"), error("abgd: fatal error (code 2)");


	for( i=0;  i< matrix.n ;  i++ ){

		if( my_comp.node_compid[i] == -1 )continue;

		my_comp.n_in_comp[ my_comp.node_compid[i] ]++;

	}

	for( i=0;  i<my_comp.nc;  i++ ){
		my_comp.comp[i] = (int *)malloc(  sizeof(int)*my_comp.n_in_comp[ i ] );
		if( !my_comp.comp[i] )REprintf( "compute_composante: cannot allocate my_comp.comp[%d], bye\n", i), error("abgd: fatal error (code 2)");
	}


	for( i=0;i<my_comp.nc;i++ )
		my_comp.n_in_comp[ i ] = 0;


	for(i=0; i<matrix.n; i++ ){

		if( my_comp.node_compid[i] == -1)continue;

		my_comp.comp[ my_comp.node_compid[i] ][ my_comp.n_in_comp[ my_comp.node_compid[i] ] ] = i;
		my_comp.n_in_comp[ my_comp.node_compid[i] ] ++;
	}


	return my_comp;
}
/*
	Use this function to split one composante (given by id) into several ones given by sub_comp)
*/
void update_composante(  struct Composante *main_comp, int id, struct Composante sub_comp ){

	int i;

	if( main_comp->n_in_comp[id] != sub_comp.nn )
		REprintf( "update_composante: make sure the size of the composante to split equals the number of nodes in the splitted one\n"), error("abgd: fatal error (code 1)");


	main_comp->n_in_comp = (int *)realloc( (void *) main_comp->n_in_comp, (size_t) (main_comp->nc+sub_comp.nc-1)*sizeof(int)  );
	main_comp->comp = (int **)realloc( (void *) main_comp->comp, (size_t) (main_comp->nc+sub_comp.nc-1)*sizeof(int *)  );
	if( !main_comp->n_in_comp || !main_comp->n_in_comp )
		REprintf( "update_composante: cannot reallocate main_comp->n_in_comp or main_comp->n_in_comp, bye\n"), error("abgd: fatal error (code 3)");


	for(i =0; i < main_comp->nn+main_comp->nm ; i++){

		if( sub_comp.node_compid[ i ] > 0 )
			main_comp->node_compid[ i ] = main_comp->nc + sub_comp.node_compid[ i ];
	}


	main_comp->n_in_comp[ id ] = sub_comp.n_in_comp[ 0 ];

	for(i =1; i < sub_comp.nc ; i++){
		main_comp->n_in_comp[ main_comp->nc + i-1 ] = sub_comp.n_in_comp[ i ];
	}


	main_comp->comp[ id ] = sub_comp.comp[ 0 ];
	sub_comp.comp[ 0 ] = NULL;

	for(i =1; i < sub_comp.nc ; i++){

		main_comp->comp[ main_comp->nc+i-1 ] = sub_comp.comp[ i ];
		sub_comp.comp[ i ] = NULL;

	}


	main_comp->nc = main_comp->nc + sub_comp.nc - 1;

}


void free_composante(  struct Composante c  ){

	int i;
	if (c.node_compid !=NULL)
	free(c.node_compid);
	for(i=0;i<c.nc;i++)
		if( c.comp[i] != NULL  )
			free(c.comp[i]);
	free(c.n_in_comp);
	free(c.comp);
}

void reset_composante( struct Composante * c ){

	c->nc = 0;
	c->nn = 0;
	c->nm = 0;

	c->node_compid = NULL;
	c->n_in_comp   = NULL;
	c->comp        = NULL;


}


/******
	heuristics to get a min window size
******/
long min_ws( long nval ){

	if( nval > 10000 )
		return 1000;

	return (nval/10>1)?nval/10:1;
}

/*check that names are uniques in alignment*/
int check_names(struct FastaSeq *mesSeq, int nbseq)
{
int i,j,c;
for (i=0;i<nbseq-1;i++)
	for (j=i+1;j<nbseq;j++)
	{
		if (strcmp(mesSeq[i].name,mesSeq[j].name)==0)
			{
			Rprintf("seq %d: %s and seq %d: %s have same name\n",i+1,mesSeq[i].name,j+1,mesSeq[j].name);
			return(0);
			}

		if (strstr(mesSeq[i].name,mesSeq[j].name)!=NULL) //name j included in i
			{
			c=strlen(mesSeq[j].name);
			if (mesSeq[i].name[c]==' ')
				{
				Rprintf("seq %d: %s has a name included in seq %d: %s . ABGD can't deal with that; change at least one of the names\n",i+1,mesSeq[i].name,j+1,mesSeq[j].name);
				return(0);
				}
			}

		if  (strstr(mesSeq[j].name,mesSeq[i].name)!=NULL)
			{
			c=strlen(mesSeq[i].name);
			if (mesSeq[j].name[c]==' ')
				{
				Rprintf("seq %d: %s has a name included in seq %d: %s . ABGD can't deal with that; change at least one of the names\n\n",i+1,mesSeq[i].name,j+1,mesSeq[j].name);
				return(0);
				}
			}
	}
return(1);
}

#if defined(__GNUC__) || defined(__clang__)
#  pragma GCC diagnostic pop
#endif
