{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "odepack.h"
module Odepack.Raw where
import Foreign.Ptr
import Foreign.C

type C'S_fp = FunPtr ()
type C'U_fp = FunPtr ()

#ccall dlsode_ , <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> <U_fp> -> Ptr CInt -> IO CInt
#ccall dlsodes_ , <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> <U_fp> -> Ptr CInt -> IO CInt
#ccall dlsoda_ , <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> <U_fp> -> Ptr CInt -> IO CInt
#ccall dlsodar_ , <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> <U_fp> -> Ptr CInt -> <U_fp> -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall dlsodpk_ , <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> <U_fp> -> <U_fp> -> Ptr CInt -> IO CInt
#ccall dlsodkr_ , <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> <U_fp> -> <U_fp> -> Ptr CInt -> <U_fp> -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall dlsodi_ , <S_fp> -> <U_fp> -> <U_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall dlsoibt_ , <S_fp> -> <U_fp> -> <U_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall dlsodis_ , <S_fp> -> <U_fp> -> <U_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

{- probably not needed?

 #strict_import
 
 {- typedef int integer; -}
 #synonym_t integer , CInt
 {- typedef unsigned int uinteger; -}
 #synonym_t uinteger , CUInt
 {- typedef char * address; -}
 #synonym_t address , CChar
 {- typedef short int shortint; -}
 #synonym_t shortint , CShort
 {- typedef float real; -}
 #synonym_t real , CFloat
 {- typedef double doublereal; -}
 #synonym_t doublereal , CDouble
 {- typedef struct {
             real r, i;
         } complex; -}
 #starttype complex
 #field r , CFloat
 #field i , CFloat
 #stoptype
 {- typedef struct {
             doublereal r, i;
         } doublecomplex; -}
 #starttype doublecomplex
 #field r , CDouble
 #field i , CDouble
 #stoptype
 {- typedef int logical; -}
 #synonym_t logical , CInt
 {- typedef short int shortlogical; -}
 #synonym_t shortlogical , CShort
 {- typedef char logical1; -}
 #synonym_t logical1 , CChar
 {- typedef char integer1; -}
 #synonym_t integer1 , CChar
 {- typedef int flag; -}
 #synonym_t flag , CInt
 {- typedef int ftnlen; -}
 #synonym_t ftnlen , CInt
 {- typedef int ftnint; -}
 #synonym_t ftnint , CInt
 {- typedef struct {
             flag cierr; ftnint ciunit; flag ciend; char * cifmt; ftnint cirec;
         } cilist; -}
 #starttype cilist
 #field cierr , CInt
 #field ciunit , CInt
 #field ciend , CInt
 #field cifmt , CString
 #field cirec , CInt
 #stoptype
 {- typedef struct {
             flag icierr;
             char * iciunit;
             flag iciend;
             char * icifmt;
             ftnint icirlen;
             ftnint icirnum;
         } icilist; -}
 #starttype icilist
 #field icierr , CInt
 #field iciunit , CString
 #field iciend , CInt
 #field icifmt , CString
 #field icirlen , CInt
 #field icirnum , CInt
 #stoptype
 {- typedef struct {
             flag oerr;
             ftnint ounit;
             char * ofnm;
             ftnlen ofnmlen;
             char * osta;
             char * oacc;
             char * ofm;
             ftnint orl;
             char * oblnk;
         } olist; -}
 #starttype olist
 #field oerr , CInt
 #field ounit , CInt
 #field ofnm , CString
 #field ofnmlen , CInt
 #field osta , CString
 #field oacc , CString
 #field ofm , CString
 #field orl , CInt
 #field oblnk , CString
 #stoptype
 {- typedef struct {
             flag cerr; ftnint cunit; char * csta;
         } cllist; -}
 #starttype cllist
 #field cerr , CInt
 #field cunit , CInt
 #field csta , CString
 #stoptype
 {- typedef struct {
             flag aerr; ftnint aunit;
         } alist; -}
 #starttype alist
 #field aerr , CInt
 #field aunit , CInt
 #stoptype
 {- typedef struct {
             flag inerr;
             ftnint inunit;
             char * infile;
             ftnlen infilen;
             ftnint * inex;
             ftnint * inopen;
             ftnint * innum;
             ftnint * innamed;
             char * inname;
             ftnlen innamlen;
             char * inacc;
             ftnlen inacclen;
             char * inseq;
             ftnlen inseqlen;
             char * indir;
             ftnlen indirlen;
             char * infmt;
             ftnlen infmtlen;
             char * inform;
             ftnint informlen;
             char * inunf;
             ftnlen inunflen;
             ftnint * inrecl;
             ftnint * innrec;
             char * inblank;
             ftnlen inblanklen;
         } inlist; -}
 #starttype inlist
 #field inerr , CInt
 #field inunit , CInt
 #field infile , CString
 #field infilen , CInt
 #field inex , Ptr CInt
 #field inopen , Ptr CInt
 #field innum , Ptr CInt
 #field innamed , Ptr CInt
 #field inname , CString
 #field innamlen , CInt
 #field inacc , CString
 #field inacclen , CInt
 #field inseq , CString
 #field inseqlen , CInt
 #field indir , CString
 #field indirlen , CInt
 #field infmt , CString
 #field infmtlen , CInt
 #field inform , CString
 #field informlen , CInt
 #field inunf , CString
 #field inunflen , CInt
 #field inrecl , Ptr CInt
 #field innrec , Ptr CInt
 #field inblank , CString
 #field inblanklen , CInt
 #stoptype
 {- union Multitype {
     integer1 g;
     shortint h;
     integer i;
     real r;
     doublereal d;
     complex c;
     doublecomplex z;
 }; -}
 #starttype union Multitype
 #field g , CChar
 #field h , CShort
 #field i , CInt
 #field r , CFloat
 #field d , CDouble
 #field c , <complex>
 #field z , <doublecomplex>
 #stoptype
 {- typedef union Multitype Multitype; -}
 #opaque_t union Multitype
 #synonym_t Multitype , <union Multitype>
 {- struct Vardesc {
     char * name; char * addr; ftnlen * dims; int type;
 }; -}
 #starttype struct Vardesc
 #field name , CString
 #field addr , CString
 #field dims , Ptr CInt
 #field type , CInt
 #stoptype
 {- typedef struct Vardesc Vardesc; -}
 #opaque_t struct Vardesc
 #synonym_t Vardesc , <struct Vardesc>
 {- struct Namelist {
     char * name; Vardesc * * vars; int nvars;
 }; -}
 #starttype struct Namelist
 #field name , CString
 #field vars , Ptr (Ptr <struct Vardesc>)
 #field nvars , CInt
 #stoptype
 {- typedef struct Namelist Namelist; -}
 #opaque_t struct Namelist
 #synonym_t Namelist , <struct Namelist>
 #callback U_fp , IO CInt
 #callback J_fp , IO CShort
 #callback I_fp , IO CInt
 #callback R_fp , IO CFloat
 #callback D_fp , IO CDouble
 #callback E_fp , IO CDouble
 #callback C_fp , IO ()
 #callback Z_fp , IO ()
 #callback L_fp , IO CInt
 #callback K_fp , IO CShort
 #callback H_fp , IO ()
 #callback S_fp , IO CInt
 {- typedef void C_f; -}
 {- typedef void H_f; -}
 {- typedef void Z_f; -}
 {- typedef doublereal E_f; -}
 #synonym_t E_f , CDouble
 #ccall dumach_ , IO CDouble
 #ccall dumsum_ , Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
 #ccall dcfode_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
 #ccall dintdy_ , Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall dprepj_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dsolsy_ , Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
 #ccall dsrcom_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dstode_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <U_fp> -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dewset_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
 #ccall dvnorm_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CDouble
 #ccall diprep_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> <U_fp> -> <U_fp> -> IO CInt
 #ccall dprep_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> <S_fp> -> <S_fp> -> IO CInt
 #ccall jgroup_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall adjlr_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall cntnzu_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dprjs_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dsolss_ , Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
 #ccall dsrcms_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall odrv_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall md_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall mdi_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall mdm_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall mdp_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall mdu_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall sro_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall cdrv_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall nroc_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall nsfc_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall nnfc_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall nnsc_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
 #ccall nntc_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
 #ccall dstoda_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <U_fp> -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dprja_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dmnorm_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CDouble
 #ccall dfnorm_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CDouble
 #ccall dbnorm_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CDouble
 #ccall dsrcma_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall drchek_ , Ptr CInt -> <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall droots_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall dsrcar_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dstodpk_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <U_fp> -> <U_fp> -> IO CInt
 #ccall dpkset_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dsolpk_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <U_fp> -> IO CInt
 #ccall dspiom_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> <S_fp> -> <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall datv_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> <S_fp> -> <S_fp> -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dorthog_ , Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
 #ccall dspigmr_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> <S_fp> -> <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall dpcg_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> <S_fp> -> <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall dpcgs_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> <S_fp> -> <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall datp_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> <S_fp> -> Ptr CDouble -> IO CInt
 #ccall dusol_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall dsrcpk_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dhefa_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dhesl_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
 #ccall dheqr_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dhels_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
 #ccall dlhin_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> <S_fp> -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dstoka_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <U_fp> -> <U_fp> -> IO CInt
 #ccall dsetpk_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dsrckr_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dainvg_ , <S_fp> -> <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dstodi_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <U_fp> -> <U_fp> -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dprepji_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <S_fp> -> <S_fp> -> IO CInt
 #ccall daigbt_ , <S_fp> -> <S_fp> -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dpjibt_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dslsbt_ , Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
 #ccall ddecbt_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dsolbt_ , Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall diprepi_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> <U_fp> -> <U_fp> -> <U_fp> -> IO CInt
 #ccall dprepi_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> <S_fp> -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dainvgs_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dprjis_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> <S_fp> -> <S_fp> -> <S_fp> -> IO CInt
 #ccall dgefa_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dgesl_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall dgbfa_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall dgbsl_ , Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall daxpy_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall dcopy_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall ddot_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CDouble
 #ccall dnrm2_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CDouble
 #ccall dscal_ , Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall idamax_ , Ptr CInt -> Ptr CDouble -> Ptr CInt -> IO CInt
 #ccall xerrwd_ , CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt
 #ccall xsetf_ , Ptr CInt -> IO CInt
 #ccall xsetun_ , Ptr CInt -> IO CInt
 #ccall ixsav_ , Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
 #ccall iumach_ , IO CInt
 -}
