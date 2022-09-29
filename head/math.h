/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: math.h,v 1.9.3.2 90/05/10 01:01:41 wje Exp $ */

#ifndef	_MATH_
#define	_MATH_	1


#ifndef _POLY9
extern int errno, signgam;

/* features from sys v */

extern double fmod(), gamma();
extern int matherr();

/* features from 4.3 BSD */

extern double asinh(double), acosh(double), atanh(double);
extern double erf(double), erfc(double);
extern double exp(double), expm1(double);
extern double log(double), log10(double), log1p(double), pow(double, double);
extern double fabs(double), floor(double), ceil(double), rint(double);
extern double lgamma(double);
extern double hypot(double, double), cabs(/*struct {double; double}*/);
extern double copysign(double, double), drem(double, double);
extern double logb(double), scalb(double, int);
extern int finite(double);
extern double j0(double), j1(double), jn(int, double);
extern double y0(double), y1(double), yn(int, double);
extern double sin(double), cos(double), tan(double);
extern double asin(double), acos(double), atan(double), atan2(double, double);
extern double sinh(double), cosh(double), tanh(double);
extern double cbrt(double), sqrt(double);
extern double modf(double, double *);
extern double ldexp(double, int), frexp(double, int *);
extern double atof(char *);

/* MIPS single-precision forms */

/*extern float fasinh(float), facosh(float), fatanh(float);*/
/*extern float ferf(float), ferfc(float);*/
extern float fexp(float)/*, fexpm1(float)*/;
extern float flog(float)/*, flog10(float), flog1p(float), fpow(float, float)*/;
extern float /*ffabs(float),*/ ffloor(float), fceil(float)/*, frint(float)*/;
/*extern float flgamma(float);*/
extern float fhypot(float, float)/*, fcabs(/*struct {float; float})*/;
/*extern float fcopysign(float, float), fdrem(float, float);*/
/*extern float flogb(float), fscalb(float, int);*/
/*extern int ffinite(float);*/
/*extern float fj0(float), fj1(float), fjn(int, float);*/
/*extern float fy0(float), fy1(float), fyn(int, float);*/
extern float fsin(float), fcos(float), ftan(float);
extern float fasin(float), facos(float), fatan(float), fatan2(float, float);
extern float fsinh(float), fcosh(float), ftanh(float);
extern float /*fcbrt(float),*/ fsqrt(float);
/*extern float fmodf(float, float *);*/
/*extern float fldexp(float, int), ffrexp(float, int *);*/
/*extern float fatof(char *);*/

/* some useful constants */
#define M_E	2.7182818284590452354
#define M_LOG2E	1.4426950408889634074
#define M_LOG10E	0.43429448190325182765
#define M_LN2	0.69314718055994530942
#define M_LN10	2.30258509299404568402
#define M_PI	3.14159265358979323846
#define M_PI_2	1.57079632679489661923
#define M_PI_4	0.78539816339744830962
#define M_1_PI	0.31830988618379067154
#define M_2_PI	0.63661977236758134308
#define M_2_SQRTPI	1.12837916709551257390
#define M_SQRT2	1.41421356237309504880
#define M_SQRT1_2	0.70710678118654752440
#define MAXFLOAT	((float)1.701411733192644299e+38)
#define HUGE	MAXFLOAT

#define _ABS(x)	((x) < 0 ? -(x) : (x))
#define _REDUCE(TYPE, X, XN, C1, C2)	{ \
	double x1 = (double)(TYPE)X, x2 = X - x1; \
	X = x1 - (XN) * (C1); X += x2; X -= (XN) * (C2); }
#define _POLY1(x, c)	((c)[0] * (x) + (c)[1])
#define _POLY2(x, c)	(_POLY1((x), (c)) * (x) + (c)[2])
#define _POLY3(x, c)	(_POLY2((x), (c)) * (x) + (c)[3])
#define _POLY4(x, c)	(_POLY3((x), (c)) * (x) + (c)[4])
#define _POLY5(x, c)	(_POLY4((x), (c)) * (x) + (c)[5])
#define _POLY6(x, c)	(_POLY5((x), (c)) * (x) + (c)[6])
#define _POLY7(x, c)	(_POLY6((x), (c)) * (x) + (c)[7])
#define _POLY8(x, c)	(_POLY7((x), (c)) * (x) + (c)[8])
#define _POLY9(x, c)	(_POLY8((x), (c)) * (x) + (c)[9])

struct exception {
	int type;
	char *name;
	double arg1;
	double arg2;
	double retval;
};

#define DOMAIN		1
#define	SING		2
#define	OVERFLOW	3
#define	UNDERFLOW	4
#define	TLOSS		5
#define	PLOSS		6
#endif

#endif	_MATH_
