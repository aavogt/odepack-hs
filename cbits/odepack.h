#include "f2c.h"

extern doublereal dumach_(void);
extern int dumsum_(doublereal *a, doublereal *b, doublereal *c__);
extern int dcfode_(integer *meth, doublereal *elco, doublereal *tesco);
extern int dintdy_(doublereal *t, integer *k, doublereal *yh, integer *nyh, doublereal *dky, integer *iflag);
extern int dprepj_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *ewt, doublereal *ftem, doublereal *savf, doublereal *wm, integer *iwm, S_fp f, S_fp jac);
extern int dsolsy_(doublereal *wm, integer *iwm, doublereal *x, doublereal *tem);
extern int dsrcom_(doublereal *rsav, integer *isav, integer *job);
extern int dstode_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *yh1, doublereal *ewt, doublereal *savf, doublereal *acor, doublereal *wm, integer *iwm, S_fp f, U_fp jac, S_fp pjac, S_fp slvs);
extern int dewset_(integer *n, integer *itol, doublereal *rtol, doublereal *atol, doublereal *ycur, doublereal *ewt);
extern doublereal dvnorm_(integer *n, doublereal *v, doublereal *w);
extern int diprep_(integer *neq, doublereal *y, doublereal *rwork, integer *ia, integer *ja, integer *ipflag, U_fp f, U_fp jac);
extern int dprep_(integer *neq, doublereal *y, doublereal *yh, doublereal *savf, doublereal *ewt, doublereal *ftem, integer *ia, integer *ja, doublereal *wk, integer *iwk, integer *ipper, S_fp f, S_fp jac);
extern int jgroup_(integer *n, integer *ia, integer *ja, integer *maxg, integer *ngrp, integer *igp, integer *jgp, integer *incl, integer *jdone, integer *ier);
extern int adjlr_(integer *n, integer *isp, integer *ldif);
extern int cntnzu_(integer *n, integer *ia, integer *ja, integer *nzsut);
extern int dprjs_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *ewt, doublereal *ftem, doublereal *savf, doublereal *wk, integer *iwk, S_fp f, S_fp jac);
extern int dsolss_(doublereal *wk, integer *iwk, doublereal *x, doublereal *tem);
extern int dsrcms_(doublereal *rsav, integer *isav, integer *job);
extern int odrv_(integer *n, integer *ia, integer *ja, doublereal *a, integer *p, integer *ip, integer *nsp, integer *isp, integer *path, integer *flag__);
extern int md_(integer *n, integer *ia, integer *ja, integer *max__, integer *v, integer *l, integer *head, integer *last, integer *next, integer *mark, integer *flag__);
extern int mdi_(integer *n, integer *ia, integer *ja, integer *max__, integer *v, integer *l, integer *head, integer *last, integer *next, integer *mark, integer *tag, integer *flag__);
extern int mdm_(integer *vk, integer *tail, integer *v, integer *l, integer *last, integer *next, integer *mark);
extern int mdp_(integer *k, integer *ek, integer *tail, integer *v, integer *l, integer *head, integer *last, integer *next, integer *mark);
extern int mdu_(integer *ek, integer *dmin__, integer *v, integer *l, integer *head, integer *last, integer *next, integer *mark);
extern int sro_(integer *n, integer *ip, integer *ia, integer *ja, doublereal *a, integer *q, integer *r__, logical *dflag);
extern int cdrv_(integer *n, integer *r__, integer *c__, integer *ic, integer *ia, integer *ja, doublereal *a, doublereal *b, doublereal *z__, integer *nsp, integer *isp, doublereal *rsp, integer *esp, integer *path, integer *flag__);
extern int nroc_(integer *n, integer *ic, integer *ia, integer *ja, doublereal *a, integer *jar, doublereal *ar, integer *p, integer *flag__);
extern int nsfc_(integer *n, integer *r__, integer *ic, integer *ia, integer *ja, integer *jlmax, integer *il, integer *jl, integer *ijl, integer *jumax, integer *iu, integer *ju, integer *iju, integer *q, integer *ira, integer *jra, integer *irac, integer *irl, integer *jrl, integer *iru, integer *jru, integer *flag__);
extern int nnfc_(integer *n, integer *r__, integer *c__, integer *ic, integer *ia, integer *ja, doublereal *a, doublereal *z__, doublereal *b, integer *lmax, integer *il, integer *jl, integer *ijl, doublereal *l, doublereal *d__, integer *umax, integer *iu, integer *ju, integer *iju, doublereal *u, doublereal *row, doublereal *tmp, integer *irl, integer *jrl, integer *flag__);
extern int nnsc_(integer *n, integer *r__, integer *c__, integer *il, integer *jl, integer *ijl, doublereal *l, doublereal *d__, integer *iu, integer *ju, integer *iju, doublereal *u, doublereal *z__, doublereal *b, doublereal *tmp);
extern int nntc_(integer *n, integer *r__, integer *c__, integer *il, integer *jl, integer *ijl, doublereal *l, doublereal *d__, integer *iu, integer *ju, integer *iju, doublereal *u, doublereal *z__, doublereal *b, doublereal *tmp);
extern int dstoda_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *yh1, doublereal *ewt, doublereal *savf, doublereal *acor, doublereal *wm, integer *iwm, S_fp f, U_fp jac, S_fp pjac, S_fp slvs);
extern int dprja_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *ewt, doublereal *ftem, doublereal *savf, doublereal *wm, integer *iwm, S_fp f, S_fp jac);
extern doublereal dmnorm_(integer *n, doublereal *v, doublereal *w);
extern doublereal dfnorm_(integer *n, doublereal *a, doublereal *w);
extern doublereal dbnorm_(integer *n, doublereal *a, integer *nra, integer *ml, integer *mu, doublereal *w);
extern int dsrcma_(doublereal *rsav, integer *isav, integer *job);
extern int drchek_(integer *job, S_fp g, integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *g0, doublereal *g1, doublereal *gx, integer *jroot, integer *irt);
extern int droots_(integer *ng, doublereal *hmin, integer *jflag, doublereal *x0, doublereal *x1, doublereal *g0, doublereal *g1, doublereal *gx, doublereal *x, integer *jroot);
extern int dsrcar_(doublereal *rsav, integer *isav, integer *job);
extern int dstodpk_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *yh1, doublereal *ewt, doublereal *savf, doublereal *savx, doublereal *acor, doublereal *wm, integer *iwm, S_fp f, U_fp jac, U_fp psol);
extern int dpkset_(integer *neq, doublereal *y, doublereal *ysv, doublereal *ewt, doublereal *ftem, doublereal *savf, doublereal *wm, integer *iwm, S_fp f, S_fp jac);
extern int dsolpk_(integer *neq, doublereal *y, doublereal *savf, doublereal *x, doublereal *ewt, doublereal *wm, integer *iwm, S_fp f, U_fp psol);
extern int dspiom_(integer *neq, doublereal *tn, doublereal *y, doublereal *savf, doublereal *b, doublereal *wght, integer *n, integer *maxl, integer *kmp, doublereal *delta, doublereal *hl0, integer *jpre, integer *mnewt, S_fp f, S_fp psol, integer *npsl, doublereal *x, doublereal *v, doublereal *hes, integer *ipvt, integer *liom, doublereal *wp, integer *iwp, doublereal *wk, integer *iflag);
extern int datv_(integer *neq, doublereal *y, doublereal *savf, doublereal *v, doublereal *wght, doublereal *ftem, S_fp f, S_fp psol, doublereal *z__, doublereal *vtem, doublereal *wp, integer *iwp, doublereal *hl0, integer *jpre, integer *ier, integer *npsl);
extern int dorthog_(doublereal *vnew, doublereal *v, doublereal *hes, integer *n, integer *ll, integer *ldhes, integer *kmp, doublereal *snormw);
extern int dspigmr_(integer *neq, doublereal *tn, doublereal *y, doublereal *savf, doublereal *b, doublereal *wght, integer *n, integer *maxl, integer *maxlp1, integer *kmp, doublereal *delta, doublereal *hl0, integer *jpre, integer *mnewt, S_fp f, S_fp psol, integer *npsl, doublereal *x, doublereal *v, doublereal *hes, doublereal *q, integer *lgmr, doublereal *wp, integer *iwp, doublereal *wk, doublereal *dl, integer *iflag);
extern int dpcg_(integer *neq, doublereal *tn, doublereal *y, doublereal *savf, doublereal *r__, doublereal *wght, integer *n, integer *maxl, doublereal *delta, doublereal *hl0, integer *jpre, integer *mnewt, S_fp f, S_fp psol, integer *npsl, doublereal *x, doublereal *p, doublereal *w, doublereal *z__, integer *lpcg, doublereal *wp, integer *iwp, doublereal *wk, integer *iflag);
extern int dpcgs_(integer *neq, doublereal *tn, doublereal *y, doublereal *savf, doublereal *r__, doublereal *wght, integer *n, integer *maxl, doublereal *delta, doublereal *hl0, integer *jpre, integer *mnewt, S_fp f, S_fp psol, integer *npsl, doublereal *x, doublereal *p, doublereal *w, doublereal *z__, integer *lpcg, doublereal *wp, integer *iwp, doublereal *wk, integer *iflag);
extern int datp_(integer *neq, doublereal *y, doublereal *savf, doublereal *p, doublereal *wght, doublereal *hl0, doublereal *wk, S_fp f, doublereal *w);
extern int dusol_(integer *neq, doublereal *tn, doublereal *y, doublereal *savf, doublereal *b, doublereal *wght, integer *n, doublereal *delta, doublereal *hl0, integer *mnewt, S_fp psol, integer *npsl, doublereal *x, doublereal *wp, integer *iwp, doublereal *wk, integer *iflag);
extern int dsrcpk_(doublereal *rsav, integer *isav, integer *job);
extern int dhefa_(doublereal *a, integer *lda, integer *n, integer *ipvt, integer *info, integer *job);
extern int dhesl_(doublereal *a, integer *lda, integer *n, integer *ipvt, doublereal *b);
extern int dheqr_(doublereal *a, integer *lda, integer *n, doublereal *q, integer *info, integer *ijob);
extern int dhels_(doublereal *a, integer *lda, integer *n, doublereal *q, doublereal *b);
extern int dlhin_(integer *neq, integer *n, doublereal *t0, doublereal *y0, doublereal *ydot, S_fp f, doublereal *tout, doublereal *uround, doublereal *ewt, integer *itol, doublereal *atol, doublereal *y, doublereal *temp, doublereal *h0, integer *niter, integer *ier);
extern int dstoka_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *yh1, doublereal *ewt, doublereal *savf, doublereal *savx, doublereal *acor, doublereal *wm, integer *iwm, S_fp f, U_fp jac, U_fp psol);
extern int dsetpk_(integer *neq, doublereal *y, doublereal *ysv, doublereal *ewt, doublereal *ftem, doublereal *savf, integer *jok, doublereal *wm, integer *iwm, S_fp f, S_fp jac);
extern int dsrckr_(doublereal *rsav, integer *isav, integer *job);
extern int dainvg_(S_fp res, S_fp adda, integer *neq, doublereal *t, doublereal *y, doublereal *ydot, integer *miter, integer *ml, integer *mu, doublereal *pw, integer *ipvt, integer *ier);
extern int dstodi_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *yh1, doublereal *ewt, doublereal *savf, doublereal *savr, doublereal *acor, doublereal *wm, integer *iwm, S_fp res, U_fp adda, U_fp jac, S_fp pjac, S_fp slvs);
extern int dprepji_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *ewt, doublereal *rtem, doublereal *savr, doublereal *s, doublereal *wm, integer *iwm, S_fp res, S_fp jac, S_fp adda);
extern int daigbt_(S_fp res, S_fp adda, integer *neq, doublereal *t, doublereal *y, doublereal *ydot, integer *mb, integer *nb, doublereal *pw, integer *ipvt, integer *ier);
extern int dpjibt_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *ewt, doublereal *rtem, doublereal *savr, doublereal *s, doublereal *wm, integer *iwm, S_fp res, S_fp jac, S_fp adda);
extern int dslsbt_(doublereal *wm, integer *iwm, doublereal *x, doublereal *tem);
extern int ddecbt_(integer *m, integer *n, doublereal *a, doublereal *b, doublereal *c__, integer *ip, integer *ier);
extern int dsolbt_(integer *m, integer *n, doublereal *a, doublereal *b, doublereal *c__, doublereal *y, integer *ip);
extern int diprepi_(integer *neq, doublereal *y, doublereal *s, doublereal *rwork, integer *ia, integer *ja, integer *ic, integer *jc, integer *ipflag, U_fp res, U_fp jac, U_fp adda);
extern int dprepi_(integer *neq, doublereal *y, doublereal *s, doublereal *yh, doublereal *savr, doublereal *ewt, doublereal *rtem, integer *ia, integer *ja, integer *ic, integer *jc, doublereal *wk, integer *iwk, integer *ipper, S_fp res, S_fp jac, S_fp adda);
extern int dainvgs_(integer *neq, doublereal *t, doublereal *y, doublereal *wk, integer *iwk, doublereal *tem, doublereal *ydot, integer *ier, S_fp res, S_fp adda);
extern int dprjis_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *ewt, doublereal *rtem, doublereal *savr, doublereal *s, doublereal *wk, integer *iwk, S_fp res, S_fp jac, S_fp adda);
/* comlen dls001_ 1892 */
/* comlen dlss01_ 184 */
/* comlen dlsa01_ 212 */
/* comlen dlsr01_ 76 */
/* comlen dlpk01_ 84 */
/* comlen dls002_ 24 */
/*:ref: xerrwd_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
/*:ref: dgefa_ 14 5 7 4 4 4 4 */
/*:ref: dgbfa_ 14 7 7 4 4 4 4 4 4 */
/*:ref: dgesl_ 14 6 7 4 4 4 7 4 */
/*:ref: dgbsl_ 14 8 7 4 4 4 4 4 7 4 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: dpkset_ 14 10 4 7 7 7 7 7 7 4 214 214 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: dspiom_ 14 25 4 7 7 7 7 7 4 4 4 7 7 4 4 214 214 4 7 7 7 4 4 7 4 7 4 */
/*:ref: dspigmr_ 14 27 4 7 7 7 7 7 4 4 4 4 7 7 4 4 214 214 4 7 7 7 7 4 7 4 7 7 4 */
/*:ref: dpcg_ 14 24 4 7 7 7 7 7 4 4 7 7 4 4 214 214 4 7 7 7 7 4 7 4 7 4 */
/*:ref: dpcgs_ 14 24 4 7 7 7 7 7 4 4 7 7 4 4 214 214 4 7 7 7 7 4 7 4 7 4 */
/*:ref: dusol_ 14 17 4 7 7 7 7 7 4 7 7 4 214 4 7 7 4 7 4 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: idamax_ 4 3 4 7 4 */
/*:ref: dsetpk_ 14 11 4 7 7 7 7 7 4 7 4 214 214 */
/* Rerunning f2c -P may change prototypes or declarations. */
extern int dgefa_(doublereal *a, integer *lda, integer *n, integer *ipvt, integer *info);
extern int dgesl_(doublereal *a, integer *lda, integer *n, integer *ipvt, doublereal *b, integer *job);
extern int dgbfa_(doublereal *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, integer *info);
extern int dgbsl_(doublereal *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, doublereal *b, integer *job);
extern int daxpy_(integer *n, doublereal *da, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern int dcopy_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern doublereal ddot_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern doublereal dnrm2_(integer *n, doublereal *dx, integer *incx);
extern int dscal_(integer *n, doublereal *da, doublereal *dx, integer *incx);
extern integer idamax_(integer *n, doublereal *dx, integer *incx);
extern int xerrwd_(char *msg, integer *nmes, integer *nerr, integer *level, integer *ni, integer *i1, integer *i2, integer *nr, doublereal *r1, doublereal *r2, ftnlen msg_len);
extern int xsetf_(integer *mflag);
extern int xsetun_(integer *lun);
extern integer ixsav_(integer *ipar, integer *ivalue, logical *iset);
extern integer iumach_(void);
extern int dlsode_(S_fp f, integer *neq, doublereal *y, doublereal *t, doublereal *tout, integer *itol, doublereal *rtol, doublereal *atol, integer *itask, integer *istate, integer *iopt, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, U_fp jac, integer *mf);
extern int dlsodes_(S_fp f, integer *neq, doublereal *y, doublereal *t, doublereal *tout, integer *itol, doublereal *rtol, doublereal *atol, integer *itask, integer *istate, integer *iopt, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, U_fp jac, integer *mf);
extern int dlsoda_(S_fp f, integer *neq, doublereal *y, doublereal *t, doublereal *tout, integer *itol, doublereal *rtol, doublereal *atol, integer *itask, integer *istate, integer *iopt, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, U_fp jac, integer *jt);
extern int dlsodar_(S_fp f, integer *neq, doublereal *y, doublereal *t, doublereal *tout, integer *itol, doublereal *rtol, doublereal *atol, integer *itask, integer *istate, integer *iopt, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, U_fp jac, integer *jt, U_fp g, integer *ng, integer *jroot);
extern int dlsodpk_(S_fp f, integer *neq, doublereal *y, doublereal *t, doublereal *tout, integer *itol, doublereal *rtol, doublereal *atol, integer *itask, integer *istate, integer *iopt, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, U_fp jac, U_fp psol, integer *mf);
extern int dlsodkr_(S_fp f, integer *neq, doublereal *y, doublereal *t, doublereal *tout, integer *itol, doublereal *rtol, doublereal *atol, integer *itask, integer *istate, integer *iopt, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, U_fp jac, U_fp psol, integer *mf, U_fp g, integer *ng, integer *jroot);
extern int dlsodi_(S_fp res, U_fp adda, U_fp jac, integer *neq, doublereal *y, doublereal *ydoti, doublereal *t, doublereal *tout, integer *itol, doublereal *rtol, doublereal *atol, integer *itask, integer *istate, integer *iopt, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, integer *mf);
extern int dlsoibt_(S_fp res, U_fp adda, U_fp jac, integer *neq, doublereal *y, doublereal *ydoti, doublereal *t, doublereal *tout, integer *itol, doublereal *rtol, doublereal *atol, integer *itask, integer *istate, integer *iopt, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, integer *mf);
extern int dlsodis_(S_fp res, U_fp adda, U_fp jac, integer *neq, doublereal *y, doublereal *ydoti, doublereal *t, doublereal *tout, integer *itol, doublereal *rtol, doublereal *atol, integer *itask, integer *istate, integer *iopt, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, integer *mf);
/* comlen dls001_ 1892 */
/* comlen dlss01_ 184 */
/* comlen dlsa01_ 212 */
/* comlen dlsr01_ 76 */
/* comlen dlpk01_ 84 */
/* comlen dls002_ 24 */
/*:ref: dumach_ 7 0 */
/*:ref: dewset_ 14 6 4 4 7 7 7 7 */
/*:ref: dvnorm_ 7 3 4 7 7 */
/*:ref: dintdy_ 14 6 7 4 7 4 7 4 */
/*:ref: xerrwd_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
/*:ref: diprep_ 14 8 4 7 7 4 4 4 214 200 */
/*:ref: dmnorm_ 7 3 4 7 7 */
/*:ref: dstoda_ 14 14 4 7 7 4 7 7 7 7 7 4 214 200 200 200 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: drchek_ 14 11 4 200 4 7 7 4 7 7 7 4 4 */
/*:ref: dstodpk_ 14 14 4 7 7 4 7 7 7 7 7 7 4 214 200 200 */
/*:ref: dlhin_ 14 16 4 4 7 7 7 214 7 7 7 4 7 7 7 7 4 4 */
/*:ref: dstoka_ 14 14 4 7 7 4 7 7 7 7 7 7 4 214 200 200 */
/*:ref: dainvg_ 14 12 214 200 4 7 7 7 4 4 4 7 4 4 */
/*:ref: daigbt_ 14 11 214 200 4 7 7 7 4 4 7 4 4 */
/*:ref: diprepi_ 14 12 4 7 7 7 4 4 4 4 4 214 200 200 */
/*:ref: dainvgs_ 14 10 4 7 7 7 7 7 7 4 214 200 */
