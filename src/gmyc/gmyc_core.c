/*
 * gmyc_core.c  --  GMYC single-threshold likelihood engine
 *
 * Direct translation of splits (Fujisawa & Barraclough 2013), gmyc.R:
 *   l.null()      --> null_loglik_p()    optimised with golden-section
 *   l.min2()      --> gmyc_loglik_pq()   optimised with Nelder-Mead
 *   gmyc.single() --> gmyc_core()        threshold scan
 *
 * KEY: splits optimises scaling exponents p (null) and (spe_p, coa_p)
 * (GMYC) -- NOT fixed at 1.  Fixing p=1 gives ~980 for both null and
 * GMYC, making LR=0.  The optimised values give null~995, GMYC~1010.
 *
 * ═══════════════════════════════════════════════════════════════
 * INTERVAL LAYOUT  (splits internod[1..numnod])
 *   m internal nodes → events ev[0..m-1] oldest→youngest
 *   intervals x[0..m-1]:
 *     x[i] = ev[i+1].time - ev[i].time   (i=0..m-2)
 *     x[m-1] = 0 - ev[m-1].time          (final → present)
 *
 * ═══════════════════════════════════════════════════════════════
 * NULL MODEL  (splits l.null(p), optimised over p in [0,5])
 *   S_i = ((i+2)*(i+1))^p   (splits i.div^p, i.div=2..numnod+1)
 *   lambda_p = m / sum(S_i * x_i)
 *   logl(p) = sum[ log(S_i*lambda_p) - S_i*lambda_p*x_i ]
 *           = m*(log(m)-log(sum S_i*x_i)) + p*sum(log((i+2)*(i+1))) - m
 *
 * ═══════════════════════════════════════════════════════════════
 * GMYC MODEL  (splits l.min2(q), q=(spe_p,coa_p), Nelder-Mead)
 *   State after event ev[i] fires:
 *     n_spe: +1 pre-threshold, -1 MRCA, unchanged coalescent; starts at 1
 *     jc[c]: cluster c lineage count; set to 2 at MRCA, +1 at coal
 *   Per interval i:
 *     B_spe_i = n_spe_i ^ spe_p         (splits i.mat[n+1,i]^p[n+1])
 *     B_coa_i = sum_c (jc*(jc-1))^coa_p (splits sum of i.mat[1:n,i]^p)
 *   MLE rates:
 *     lambda_spe = numSpeEvents / sum(B_spe_i * x_i)
 *     lambda_coa = numCoaEvents / sum(B_coa_i * x_i)
 *   Combined rate: b_i = lambda_spe*B_spe_i + lambda_coa*B_coa_i
 *   logl = sum[ log(b_i) - b_i*x_i ]
 *
 * ═══════════════════════════════════════════════════════════════
 */

#include "gmyc_core.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>

/* ── chi2 survival ─────────────────────────────────────────────── */
static double gamma_series(double a,double x){double ap=a,s=1.0/a,d=s;for(int n=1;n<=300;n++){ap+=1.0;d*=x/ap;s+=d;if(fabs(d)<fabs(s)*1e-14)break;}return s*exp(-x+a*log(x)-lgamma(a));}
static double gamma_cf(double a,double x){const double fp=1e-300;double b=x+1.0-a,c=1.0/fp,d=1.0/b,h=d;for(int i=1;i<=300;i++){double an=-i*(i-a);b+=2.0;d=an*d+b;if(fabs(d)<fp)d=fp;c=b+an/c;if(fabs(c)<fp)c=fp;d=1.0/d;h*=d*c;if(fabs(d*c-1.0)<1e-14)break;}return exp(-x+a*log(x)-lgamma(a))*h;}
double chi2_sf(double x,double df){if(x<=0.0)return 1.0;double a=df/2.0;return(x<a+1.0)?1.0-gamma_series(a,x/2.0):gamma_cf(a,x/2.0);}

/* ── tree I/O ──────────────────────────────────────────────────── */
int gtree_from_ape(const int *edge,const double *edge_len,int n_edge,
                   const char **tip_label,int n_tips,int n_nodes_total,GTree *tree)
{
    if(n_nodes_total>=MAX_NODES)return -1;
    memset(tree,0,sizeof(GTree));
    tree->n_nodes=n_nodes_total;tree->n_taxa=n_tips;tree->root=n_tips;
    for(int i=0;i<n_nodes_total;i++){
        tree->nodes[i].id=i;tree->nodes[i].parent=-1;tree->nodes[i].left=-1;
        tree->nodes[i].right=-1;tree->nodes[i].branch_len=0.0;tree->nodes[i].height=0.0;
        tree->nodes[i].is_tip=(i<n_tips)?1:0;tree->nodes[i].label[0]='\0';
    }
    for(int k=0;k<n_edge;k++){
        int par=edge[k]-1,child=edge[k+n_edge]-1;
        if(par<0||par>=n_nodes_total||child<0||child>=n_nodes_total)return -1;
        tree->nodes[child].parent=par;tree->nodes[child].branch_len=edge_len[k];
        if(tree->nodes[par].left<0)tree->nodes[par].left=child;
        else tree->nodes[par].right=child;
    }
    for(int i=0;i<n_tips;i++)
        if(tip_label&&tip_label[i])strncpy(tree->nodes[i].label,tip_label[i],MAX_LABEL-1);
    return 0;
}

/* ── heights ───────────────────────────────────────────────────── */
static int cmp_dbl(const void*a,const void*b){double da=*(const double*)a,db=*(const double*)b;return(da>db)-(da<db);}
void gtree_compute_heights(GTree *tree){
    typedef struct{int id;double d;}SE;
    SE*stk=(SE*)malloc(tree->n_nodes*sizeof(SE));if(!stk)return;
    int top=0;stk[0].id=tree->root;stk[0].d=0.0;top++;
    while(top>0){top--;int id=stk[top].id;double d=stk[top].d;tree->nodes[id].height=d;
        int r=tree->nodes[id].right,l=tree->nodes[id].left;
        if(r>=0){stk[top].id=r;stk[top].d=d+tree->nodes[r].branch_len;top++;}
        if(l>=0){stk[top].id=l;stk[top].d=d+tree->nodes[l].branch_len;top++;}
    }free(stk);
    double mx=0.0;for(int i=0;i<tree->n_nodes;i++)if(tree->nodes[i].height>mx)mx=tree->nodes[i].height;
    for(int i=0;i<tree->n_nodes;i++)tree->nodes[i].height-=mx;
    tree->n_internal=tree->n_nodes-tree->n_taxa;
    tree->heights=(double*)malloc(tree->n_internal*sizeof(double));if(!tree->heights)return;
    int k=0;for(int i=0;i<tree->n_nodes;i++)if(!tree->nodes[i].is_tip)tree->heights[k++]=tree->nodes[i].height;
    qsort(tree->heights,tree->n_internal,sizeof(double),cmp_dbl);
}


void gtree_free(GTree *tree){if(tree->heights){free(tree->heights);tree->heights=NULL;}}

/* ── events ────────────────────────────────────────────────────── */
static int cmp_bev(const void*a,const void*b){double ta=((const BEvent*)a)->time,tb=((const BEvent*)b)->time;return(ta>tb)-(ta<tb);}
static int collect_events(const GTree *tree,BEvent *ev){
    int k=0;for(int i=0;i<tree->n_nodes;i++)
        if(!tree->nodes[i].is_tip){ev[k].time=tree->nodes[i].height;ev[k].node_id=i;k++;}
    qsort(ev,k,sizeof(BEvent),cmp_bev);return k;
}
static void make_intervals(const BEvent *ev,int m,double *x){
    for(int i=0;i<m-1;i++)x[i]=ev[i+1].time-ev[i].time;
    x[m-1]=0.0-ev[m-1].time;
}

/* ── node classification ───────────────────────────────────────── */
/* nod_type: 0=pre-threshold, 2=MRCA, 1=coalescent */
static void classify_nodes(const GTree *tree,const BEvent *ev,int m,
                            double T,int *nod_type,int *K_out)
{
    int K=0;
    for(int i=0;i<m;i++){
        int nid=ev[i].node_id;
        double h=tree->nodes[nid].height;
        int par=tree->nodes[nid].parent;
        double hp=(par>=0)?tree->nodes[par].height:-1e300;
        if(h<T){nod_type[i]=0;}
        else if(hp<T){nod_type[i]=2;K++;}
        else{nod_type[i]=1;}
    }
    *K_out=K;
}

/* ── cluster assignment for tip output ────────────────────────── */
static int assign_clusters(const GTree *tree,double T,int *node_cid){
    typedef struct{int id;int cid;}SE;
    SE*stk=(SE*)malloc(tree->n_nodes*sizeof(SE));if(!stk)return 0;
    for(int i=0;i<tree->n_nodes;i++)node_cid[i]=0;
    int top=0,next=1;
    int rc=(tree->nodes[tree->root].height>=T)?next++:0;
    stk[0].id=tree->root;stk[0].cid=rc;top++;
    while(top>0){top--;int id=stk[top].id,cid=stk[top].cid;node_cid[id]=cid;
        const GNode*n=&tree->nodes[id];if(n->is_tip)continue;
        int ch[2]={n->left,n->right};
        for(int ci=1;ci>=0;ci--){int c=ch[ci];if(c<0)continue;int cc=cid;
            /* Only start a new cluster at an internal MRCA — tips are singletons */
            if(cid==0&&!tree->nodes[c].is_tip&&tree->nodes[c].height>=T)cc=next++;
            stk[top].id=c;stk[top].cid=cc;top++;}
    }
    free(stk);return next-1;
}

/* ════════════════════════════════════════════════════════════════
   NULL MODEL  l.null(p) evaluated at given p
   ════════════════════════════════════════════════════════════════ */
static double null_loglik_p(const double *x,int m,double p)
{
    /* S_i = ((i+2)*(i+1))^p,  sum over i=0..m-1 */
    double sum_Sx=0.0, sum_logS=0.0;
    for(int i=0;i<m;i++){
        double S_base=(double)((i+2)*(i+1));
        double Sp=(p==1.0)?S_base:pow(S_base,p);
        sum_Sx   += Sp*x[i];
        sum_logS += (p==1.0)?log(S_base):p*log(S_base);
    }
    if(sum_Sx<=0.0)return -INFINITY;
    return (double)m*(log((double)m)-log(sum_Sx))+sum_logS-(double)m;
}

/* ════════════════════════════════════════════════════════════════
   GOLDEN-SECTION SEARCH (maximise f on [a,b])
   Matches R's optimise() for unimodal functions.
   tolerance 1e-8 matches R default.
   ════════════════════════════════════════════════════════════════ */
typedef double (*scalar_fn)(double,void*);

static double golden_section(scalar_fn f,double a,double b,void *ud,double tol)
{
    const double gr=(sqrt(5.0)+1.0)/2.0;
    double c=b-(b-a)/gr;
    double d=a+(b-a)/gr;
    double fc=f(c,ud),fd=f(d,ud);
    while(fabs(b-a)>tol){
        if(fc>fd){b=d;d=c;fd=fc;c=b-(b-a)/gr;fc=f(c,ud);}
        else      {a=c;c=d;fc=fd;d=a+(b-a)/gr;fd=f(d,ud);}
    }
    return (a+b)/2.0;
}

typedef struct{const double *x;int m;}NullCtx;
static double null_obj(double p,void *ud){
    NullCtx *ctx=(NullCtx*)ud;
    return null_loglik_p(ctx->x,ctx->m,p);
}

/* ════════════════════════════════════════════════════════════════
   GMYC MODEL  l.min2(spe_p, coa_p) at given p values
   ════════════════════════════════════════════════════════════════ */
typedef struct {
    const GTree  *tree;
    const BEvent *ev;
    const double *x;
    int           m;
    int          *nod_type;  /* pre-allocated, size m */
    int          *node_cid;  /* pre-allocated, size n_nodes */
    int          *jc;        /* pre-allocated, size K+1 */
    int           K;
    int           numSpeEvents;
    int           numCoaEvents;
} GMYCCtx;

static double gmyc_loglik_pq(const GMYCCtx *ctx,double spe_p,double coa_p,
                              double *lam_spe_out,double *lam_coa_out)
{
    int m=ctx->m,K=ctx->K;
    const double *x=ctx->x;

    /* ── pass 1: compute MLE denominators ─────────────────────────
       Include zero-state terms (n_spe=0, jc[c]=0): when the exponent
       is negative, pow(0,negative)=+Inf → the denominator becomes Inf
       → the corresponding lambda collapses to 0.  This matches splits'
       i.mat^p computation exactly (splits does not skip zero entries).
       ─────────────────────────────────────────────────────────────── */
    int n_spe=1;
    memset(ctx->jc,0,(K+1)*sizeof(int));
    double Ty=0.0,Tc=0.0;

    for(int i=0;i<m;i++){
        int nt=ctx->nod_type[i],nid=ctx->ev[i].node_id;
        if(nt==0){n_spe++;}
        else if(nt==2){n_spe--;int c=ctx->node_cid[nid];if(c>0&&c<=K)ctx->jc[c]=2;}
        else          {int c=ctx->node_cid[nid];if(c>0&&c<=K)ctx->jc[c]++;}

        double Bspe=(spe_p==1.0)?(double)n_spe:pow((double)n_spe,spe_p);
        double Bcoa=0.0;
        for(int c=1;c<=K;c++){
            double jj=(double)(ctx->jc[c]*(ctx->jc[c]-1));
            double term=(coa_p==1.0)?jj:pow(jj,coa_p);
            if(!isfinite(term)){Bcoa=1.0/0.0;break;}
            Bcoa+=term;
        }
        /* isfinite check: Inf*x propagates Inf into Ty/Tc → lambda=0 */
        Ty+=Bspe*x[i];
        Tc+=Bcoa*x[i];
    }

    /* lambda = 0 when denominator is Inf or when count is 0 */
    double lam_spe=(ctx->numSpeEvents>0&&Ty>0.0&&isfinite(Ty))
                   ?(double)ctx->numSpeEvents/Ty:0.0;
    double lam_coa=(ctx->numCoaEvents>0&&Tc>0.0&&isfinite(Tc))
                   ?(double)ctx->numCoaEvents/Tc:0.0;

    /* When Tc=Inf (inactive cluster + coa_p<0), splits computes 0*Inf=NaN in
       the loglik which optim treats as a rejected move.  Match that by
       returning -Inf here so NM never settles in the coa_p<0 degenerate region. */
    if (!isfinite(Tc) && ctx->numCoaEvents > 0) return -INFINITY;

    if(lam_spe_out)*lam_spe_out=lam_spe;
    if(lam_coa_out)*lam_coa_out=lam_coa;

    /* ── pass 2: compute logl ── */
    n_spe=1;
    memset(ctx->jc,0,(K+1)*sizeof(int));
    double logl=0.0;

    for(int i=0;i<m;i++){
        int nt=ctx->nod_type[i],nid=ctx->ev[i].node_id;
        if(nt==0){n_spe++;}
        else if(nt==2){n_spe--;int c=ctx->node_cid[nid];if(c>0&&c<=K)ctx->jc[c]=2;}
        else          {int c=ctx->node_cid[nid];if(c>0&&c<=K)ctx->jc[c]++;}

        /* skip zero-lambda component to avoid 0*Inf=NaN */
        double b=0.0;
        if(lam_spe>0.0){
            double Bspe=(spe_p==1.0)?(double)n_spe:pow((double)n_spe,spe_p);
            b+=lam_spe*Bspe;
        }
        if(lam_coa>0.0){
            double Bcoa=0.0;
            for(int c=1;c<=K;c++){
                double jj=(double)(ctx->jc[c]*(ctx->jc[c]-1));
                Bcoa+=(coa_p==1.0)?jj:pow(jj,coa_p);
            }
            b+=lam_coa*Bcoa;
        }
        if(b<=0.0||!isfinite(b))return -INFINITY;
        logl+=log(b)-b*x[i];
    }
    return logl;
}

/* ════════════════════════════════════════════════════════════════
   NELDER-MEAD (2-dimensional, maximise f)
   Unconstrained, matching R's optim(...,method="Nelder-Mead"):
     rho=1, chi=2, psi=0.5, sigma=0.5, maxit=500
     convergence: reltol = sqrt(DBL_EPSILON) ≈ 1.49e-8
     (same as R's default reltol for optim)
   No bounds — the objective returns -Inf for invalid regions.
   ════════════════════════════════════════════════════════════════ */
typedef double (*fn2d)(double,double,void*);

static double nm_run(fn2d f,double p0,double q0,void *ud,
                     double *best_p,double *best_q)
{
    /* simplex vertices [3][2] */
    double v[3][2], fv[3];
    /* initial simplex: step = 0.05*|theta| or 0.00025 (matches R's optim) */
    double step_p=(p0!=0.0)?0.05*fabs(p0):0.00025;
    double step_q=(q0!=0.0)?0.05*fabs(q0):0.00025;
    v[0][0]=p0;          v[0][1]=q0;
    v[1][0]=p0+step_p;   v[1][1]=q0;
    v[2][0]=p0;          v[2][1]=q0+step_q;
    for(int i=0;i<3;i++)
        fv[i]=-f(v[i][0],v[i][1],ud); /* negate: minimise */

    const double rho=1.0,chi=2.0,psi=0.5,sigma=0.5;
    const int maxit=500;
    /* R's reltol = sqrt(DBL_EPSILON) */
    const double reltol=1.490116119384766e-8;

    for(int iter=0;iter<maxit;iter++){
        /* sort: v[0]=best (lowest fv), v[2]=worst */
        for(int i=0;i<2;i++)for(int j=i+1;j<3;j++)
            if(fv[j]<fv[i]){double tmp;
                tmp=fv[i];fv[i]=fv[j];fv[j]=tmp;
                tmp=v[i][0];v[i][0]=v[j][0];v[j][0]=tmp;
                tmp=v[i][1];v[i][1]=v[j][1];v[j][1]=tmp;}

        /* R's relative convergence criterion */
        double conv=2.0*fabs(fv[2]-fv[0])
                    /(fabs(fv[0])+fabs(fv[2])+reltol);
        if(conv<=reltol)break;

        /* centroid of best 2 */
        double cx=(v[0][0]+v[1][0])/2.0;
        double cy=(v[0][1]+v[1][1])/2.0;

        /* reflect */
        double rx=cx+rho*(cx-v[2][0]);
        double ry=cy+rho*(cy-v[2][1]);
        double fr=-f(rx,ry,ud);

        if(fr<fv[0]){
            /* try expand */
            double ex=cx+chi*(rx-cx);
            double ey=cy+chi*(ry-cy);
            double fe=-f(ex,ey,ud);
            if(fe<fr){v[2][0]=ex;v[2][1]=ey;fv[2]=fe;}
            else     {v[2][0]=rx;v[2][1]=ry;fv[2]=fr;}
        } else if(fr<fv[1]){
            v[2][0]=rx;v[2][1]=ry;fv[2]=fr;
        } else {
            /* contract */
            int use_r=(fr<fv[2]);
            double cx2=cx+psi*(use_r?rx-cx:v[2][0]-cx);
            double cy2=cy+psi*(use_r?ry-cy:v[2][1]-cy);
            double fc2=-f(cx2,cy2,ud);
            if(fc2<(use_r?fr:fv[2])){
                v[2][0]=cx2;v[2][1]=cy2;fv[2]=fc2;
            } else {
                /* shrink */
                for(int i=1;i<3;i++){
                    v[i][0]=v[0][0]+sigma*(v[i][0]-v[0][0]);
                    v[i][1]=v[0][1]+sigma*(v[i][1]-v[0][1]);
                    fv[i]=-f(v[i][0],v[i][1],ud);
                }
            }
        }
    }
    /* sort final */
    for(int i=0;i<2;i++)for(int j=i+1;j<3;j++)
        if(fv[j]<fv[i]){double tmp;
            tmp=fv[i];fv[i]=fv[j];fv[j]=tmp;
            tmp=v[i][0];v[i][0]=v[j][0];v[j][0]=tmp;
            tmp=v[i][1];v[i][1]=v[j][1];v[j][1]=tmp;}
    *best_p=v[0][0]; *best_q=v[0][1];
    return -fv[0]; /* return maximum */
}

static double gmyc_obj(double sp,double cp,void *ud){
    return gmyc_loglik_pq((GMYCCtx*)ud,sp,cp,NULL,NULL);
}

/* ════════════════════════════════════════════════════════════════
   GMYC LOGLIK AT THRESHOLD (optimised over spe_p, coa_p)
   ════════════════════════════════════════════════════════════════ */
static double gmyc_loglik_at_opt(const GTree *tree,const BEvent *ev,
                                  const double *x,int m,int T_idx,
                                  double *prev_sp,double *prev_cp,
                                  int *K_out,
                                  double *lam_spe_out,double *lam_coa_out,
                                  double *sp_opt_out,double *cp_opt_out)
{
    double T=ev[T_idx].time;

    int *nod_type=(int*)malloc(m*sizeof(int));if(!nod_type)return -INFINITY;
    int K=0;
    classify_nodes(tree,ev,m,T,nod_type,&K);
    if(K_out)*K_out=K;
    if(K<=0){free(nod_type);return -INFINITY;}

    int numSpe=0;for(int i=0;i<m;i++)if(nod_type[i]==0)numSpe++;
    /* splits: sum(s.nod[1:n,]) = total MRCA+coalescent events = m - numSpe */
    int numCoa=m-numSpe;
    if(numCoa<=0){free(nod_type);return -INFINITY;}

    int *node_cid=(int*)malloc(tree->n_nodes*sizeof(int));
    if(!node_cid){free(nod_type);return -INFINITY;}
    int K_ac=assign_clusters(tree,T,node_cid);

    int *jc=(int*)calloc(K+1,sizeof(int));
    if(!jc){free(node_cid);free(nod_type);return -INFINITY;}

    GMYCCtx ctx={tree,ev,x,m,nod_type,node_cid,jc,K,numSpe,numCoa};

    /* Warm-start only — exactly like splits' temp.params re-use.
       prev_sp/prev_cp start at (1,1) and carry forward each threshold's
       optimal solution, matching R's optim() warm-start exactly. */
    double sp_opt,cp_opt;
    double ll=nm_run(gmyc_obj,*prev_sp,*prev_cp,&ctx,&sp_opt,&cp_opt);
    *prev_sp=sp_opt; *prev_cp=cp_opt;

    /* extract MLE lambdas at the optimal exponents */
    double lam_spe_f=0.0,lam_coa_f=0.0;
    gmyc_loglik_pq(&ctx,sp_opt,cp_opt,&lam_spe_f,&lam_coa_f);
    if(lam_spe_out)*lam_spe_out=lam_spe_f;
    if(lam_coa_out)*lam_coa_out=lam_coa_f;
    if(sp_opt_out) *sp_opt_out =sp_opt;
    if(cp_opt_out) *cp_opt_out =cp_opt;

    free(jc);free(node_cid);free(nod_type);
    return ll;
}

/* ════════════════════════════════════════════════════════════════
   MAIN ENTRY POINT
   ════════════════════════════════════════════════════════════════ */
int gmyc_core(GTree *tree,GResult *result){
    if(!tree||!result||tree->n_taxa<3)return -1;
    /* Save caller-allocated arrays before memset wipes the pointers */
    double *save_thresh_ll      = result->thresh_ll;
    int    *save_thresh_K       = result->thresh_K;
    int     save_n_thresh       = result->n_thresh;
    double *save_thresh_lam_div = result->thresh_lam_div;
    double *save_thresh_lam_coa = result->thresh_lam_coa;
    double *save_thresh_p_div   = result->thresh_p_div;
    double *save_thresh_p_coa   = result->thresh_p_coa;
    memset(result,0,sizeof(GResult));
    result->thresh_ll      = save_thresh_ll;
    result->thresh_K       = save_thresh_K;
    result->n_thresh       = save_n_thresh;
    result->thresh_lam_div = save_thresh_lam_div;
    result->thresh_lam_coa = save_thresh_lam_coa;
    result->thresh_p_div   = save_thresh_p_div;
    result->thresh_p_coa   = save_thresh_p_coa;
    result->n_tips=tree->n_taxa;

    BEvent *ev=(BEvent*)malloc(tree->n_internal*sizeof(BEvent));if(!ev)return -1;
    int m=collect_events(tree,ev);

    double *x=(double*)malloc(m*sizeof(double));if(!x){free(ev);return -1;}
    make_intervals(ev,m,x);

    /* ── NULL model: optimise p in [0,5] ── */
    NullCtx nctx={x,m};
    double p_null=golden_section(null_obj,0.0,5.0,&nctx,1e-8);
    result->log_lik_null=null_loglik_p(x,m,p_null);

    /* Null model lambda = m / sum(S_i^p_null * x_i) */
    {
        double sum_Sx=0.0;
        for(int i=0;i<m;i++){double Sp=pow((double)((i+2)*(i+1)),p_null);sum_Sx+=Sp*x[i];}
        double lam_null=(sum_Sx>0.0)?(double)m/sum_Sx:0.0;
        if(result->thresh_lam_div)result->thresh_lam_div[0]=lam_null;
        if(result->thresh_p_div)  result->thresh_p_div[0]  =p_null;
        /* thresh_lam_coa[0] and thresh_p_coa[0] stay NA (null model has no coalescent) */
    }

    /* ── GMYC scan ── */
    int stthresh=1;
    while(stthresh<m&&ev[stthresh].time==ev[0].time)stthresh++;

    result->n_thresh=m;
    result->stthresh_c=stthresh;
    /* Fill null position (index 0 in C = index 1 in R) */
    if(result->thresh_ll)result->thresh_ll[0]=result->log_lik_null;
    if(result->thresh_K) result->thresh_K[0]=1;

    double best_ll=-INFINITY;int best_t=stthresh;
    double prev_sp=1.0,prev_cp=1.0;

    /* Scan t=stthresh..m-1 (matches splits: j=stthresh..nthresh=m, 1-based) */
    for(int t=stthresh;t<m;t++){
        int K_t=0;
        double lam_spe_t=0.0,lam_coa_t=0.0,sp_t=0.0,cp_t=0.0;
        double ll=gmyc_loglik_at_opt(tree,ev,x,m,t,&prev_sp,&prev_cp,
                                     &K_t,&lam_spe_t,&lam_coa_t,&sp_t,&cp_t);
        if(result->thresh_ll)     result->thresh_ll[t]      =ll;
        if(result->thresh_K)      result->thresh_K[t]       =K_t;
        if(result->thresh_lam_div)result->thresh_lam_div[t] =lam_spe_t;
        if(result->thresh_lam_coa)result->thresh_lam_coa[t] =lam_coa_t;
        if(result->thresh_p_div)  result->thresh_p_div[t]   =sp_t;
        if(result->thresh_p_coa)  result->thresh_p_coa[t]   =cp_t;
        if(ll>best_ll){best_ll=ll;best_t=t;}
    }

    result->log_lik_gmyc=best_ll;
    result->lr_stat=2.0*(best_ll-result->log_lik_null);
    if(result->lr_stat<0.0)result->lr_stat=0.0;
    result->p_value=chi2_sf(result->lr_stat,2.0);
    result->threshold=ev[best_t].time;

    /* ── cluster assignments ── */
    int *node_cid=(int*)malloc(tree->n_nodes*sizeof(int));
    if(!node_cid){free(x);free(ev);return -1;}
    int K=assign_clusters(tree,result->threshold,node_cid);
    result->n_clusters=K;

    int singleton=K+1;
    for(int i=0;i<tree->n_taxa;i++){
        int cid=node_cid[i];
        result->cluster[i]=(cid>0)?cid:singleton++;
    }
    result->n_entities=singleton-1;

    for(int i=0;i<tree->n_taxa;i++)
        strncpy(result->tip_labels[i],tree->nodes[i].label,MAX_LABEL-1);

    free(node_cid);free(x);free(ev);
    return 0;
}

/* ════════════════════════════════════════════════════════════════
   GMYCPrep: precomputed per-threshold state for R's optim() design
   ════════════════════════════════════════════════════════════════ */

void gmyc_prep_free(GMYCPrep *prep) {
    if (!prep) return;
    if (prep->tree)         { free(prep->tree);         prep->tree         = NULL; }
    if (prep->ev)           { free(prep->ev);           prep->ev           = NULL; }
    if (prep->x)            { free(prep->x);            prep->x            = NULL; }
    if (prep->nod_type)     { free(prep->nod_type);     prep->nod_type     = NULL; }
    if (prep->node_cid)     { free(prep->node_cid);     prep->node_cid     = NULL; }
    if (prep->thresh_K)     { free(prep->thresh_K);     prep->thresh_K     = NULL; }
    if (prep->thresh_numSpe){ free(prep->thresh_numSpe);prep->thresh_numSpe= NULL; }
}

int gmyc_prep_init(GMYCPrep *prep, const GTree *src_tree)
{
    memset(prep, 0, sizeof(GMYCPrep));

    prep->tree = (GTree *)malloc(sizeof(GTree));
    if (!prep->tree) return -1;
    memcpy(prep->tree, src_tree, sizeof(GTree));

    int n_internal = src_tree->n_internal;
    if (n_internal <= 0) return -1;

    prep->ev = (BEvent *)malloc(n_internal * sizeof(BEvent));
    prep->x  = (double *)malloc(n_internal * sizeof(double));
    if (!prep->ev || !prep->x) { gmyc_prep_free(prep); return -1; }

    prep->m = collect_events(prep->tree, prep->ev);
    make_intervals(prep->ev, prep->m, prep->x);

    /* stthresh */
    prep->stthresh_c = 1;
    while (prep->stthresh_c < prep->m &&
           prep->ev[prep->stthresh_c].time == prep->ev[0].time)
        prep->stthresh_c++;

    int m = prep->m, n_nodes = prep->tree->n_nodes;

    prep->nod_type      = (int *)malloc((size_t)m * m * sizeof(int));
    prep->node_cid      = (int *)malloc((size_t)m * n_nodes * sizeof(int));
    prep->thresh_K      = (int *)malloc(m * sizeof(int));
    prep->thresh_numSpe = (int *)malloc(m * sizeof(int));
    if (!prep->nod_type || !prep->node_cid ||
        !prep->thresh_K || !prep->thresh_numSpe) {
        gmyc_prep_free(prep); return -1;
    }

    /* Precompute per-threshold nod_type, node_cid, K, numSpe */
    for (int t = 0; t < m; t++) {
        prep->thresh_K[t]      = 0;
        prep->thresh_numSpe[t] = 0;
    }
    for (int t = prep->stthresh_c; t < m; t++) {
        double T = prep->ev[t].time;
        int *nt = prep->nod_type + (size_t)t * m;
        int K = 0;
        classify_nodes(prep->tree, prep->ev, m, T, nt, &K);
        prep->thresh_K[t] = K;
        int numSpe = 0;
        for (int i = 0; i < m; i++) if (nt[i] == 0) numSpe++;
        prep->thresh_numSpe[t] = numSpe;
        assign_clusters(prep->tree, T, prep->node_cid + (size_t)t * n_nodes);
    }

    /* Null model: golden-section over p in [0,5] */
    NullCtx nctx = {prep->x, prep->m};
    double p_null = golden_section(null_obj, 0.0, 5.0, &nctx, 1e-8);
    prep->null_ll  = null_loglik_p(prep->x, prep->m, p_null);
    prep->null_p   = p_null;
    {
        double sum_Sx = 0.0;
        for (int i = 0; i < m; i++) {
            double Sp = pow((double)((i+2)*(i+1)), p_null);
            sum_Sx += Sp * prep->x[i];
        }
        prep->null_lam = (sum_Sx > 0.0) ? (double)m / sum_Sx : 0.0;
    }
    return 0;
}

/* Per-threshold log-likelihood using precomputed state.
   Uses a stack-allocated jc[] so no heap allocation per call. */
double gmyc_loglik_at(const GMYCPrep *prep, int T_idx,
                      double spe_p, double coa_p,
                      double *lam_spe_out, double *lam_coa_out)
{
    int m = prep->m, K = prep->thresh_K[T_idx];
    if (K <= 0) return -INFINITY;

    int numSpe = prep->thresh_numSpe[T_idx];
    int numCoa = m - numSpe;
    if (numCoa <= 0) return -INFINITY;

    const int  *nod_type = prep->nod_type + (size_t)T_idx * m;
    const int  *node_cid = prep->node_cid + (size_t)T_idx * prep->tree->n_nodes;

    /* Stack-allocate jc[] — K <= m <= n_internal <= MAX_NODES */
    int jc[MAX_NODES];
    memset(jc, 0, (K + 1) * sizeof(int));

    GMYCCtx ctx = {prep->tree, prep->ev, prep->x, m,
                   (int *)nod_type, (int *)node_cid, jc,
                   K, numSpe, numCoa};
    return gmyc_loglik_pq(&ctx, spe_p, coa_p, lam_spe_out, lam_coa_out);
}
