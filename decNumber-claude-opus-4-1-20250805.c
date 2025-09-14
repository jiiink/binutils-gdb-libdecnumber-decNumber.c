/* Decimal number arithmetic module for the decNumber C Library.
   Copyright (C) 2005-2018 Free Software Foundation, Inc.
   Contributed by IBM Corporation.  Author Mike Cowlishaw.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* ------------------------------------------------------------------ */
/* Decimal Number arithmetic module				      */
/* ------------------------------------------------------------------ */
/* This module comprises the routines for arbitrary-precision General */
/* Decimal Arithmetic as defined in the specification which may be    */
/* found on the General Decimal Arithmetic pages.  It implements both */
/* the full ('extended') arithmetic and the simpler ('subset')	      */
/* arithmetic.							      */
/*								      */
/* Usage notes: 						      */
/*								      */
/* 1. This code is ANSI C89 except:				      */
/*								      */
/*    a) C99 line comments (double forward slash) are used.  (Most C  */
/*	 compilers accept these.  If yours does not, a simple script  */
/*	 can be used to convert them to ANSI C comments.)	      */
/*								      */
/*    b) Types from C99 stdint.h are used.  If you do not have this   */
/*	 header file, see the User's Guide section of the decNumber   */
/*	 documentation; this lists the necessary definitions.	      */
/*								      */
/*    c) If DECDPUN>4 or DECUSE64=1, the C99 64-bit int64_t and       */
/*	 uint64_t types may be used.  To avoid these, set DECUSE64=0  */
/*	 and DECDPUN<=4 (see documentation).			      */
/*								      */
/*    The code also conforms to C99 restrictions; in particular,      */
/*    strict aliasing rules are observed.			      */
/*								      */
/* 2. The decNumber format which this library uses is optimized for   */
/*    efficient processing of relatively short numbers; in particular */
/*    it allows the use of fixed sized structures and minimizes copy  */
/*    and move operations.  It does, however, support arbitrary       */
/*    precision (up to 999,999,999 digits) and arbitrary exponent     */
/*    range (Emax in the range 0 through 999,999,999 and Emin in the  */
/*    range -999,999,999 through 0).  Mathematical functions (for     */
/*    example decNumberExp) as identified below are restricted more   */
/*    tightly: digits, emax, and -emin in the context must be <=      */
/*    DEC_MAX_MATH (999999), and their operand(s) must be within      */
/*    these bounds.						      */
/*								      */
/* 3. Logical functions are further restricted; their operands must   */
/*    be finite, positive, have an exponent of zero, and all digits   */
/*    must be either 0 or 1.  The result will only contain digits     */
/*    which are 0 or 1 (and will have exponent=0 and a sign of 0).    */
/*								      */
/* 4. Operands to operator functions are never modified unless they   */
/*    are also specified to be the result number (which is always     */
/*    permitted).  Other than that case, operands must not overlap.   */
/*								      */
/* 5. Error handling: the type of the error is ORed into the status   */
/*    flags in the current context (decContext structure).  The       */
/*    SIGFPE signal is then raised if the corresponding trap-enabler  */
/*    flag in the decContext is set (is 1).			      */
/*								      */
/*    It is the responsibility of the caller to clear the status      */
/*    flags as required.					      */
/*								      */
/*    The result of any routine which returns a number will always    */
/*    be a valid number (which may be a special value, such as an     */
/*    Infinity or NaN). 					      */
/*								      */
/* 6. The decNumber format is not an exchangeable concrete	      */
/*    representation as it comprises fields which may be machine-     */
/*    dependent (packed or unpacked, or special length, for example). */
/*    Canonical conversions to and from strings are provided; other   */
/*    conversions are available in separate modules.		      */
/*								      */
/* 7. Normally, input operands are assumed to be valid.  Set DECCHECK */
/*    to 1 for extended operand checking (including NULL operands).   */
/*    Results are undefined if a badly-formed structure (or a NULL    */
/*    pointer to a structure) is provided, though with DECCHECK       */
/*    enabled the operator routines are protected against exceptions. */
/*    (Except if the result pointer is NULL, which is unrecoverable.) */
/*								      */
/*    However, the routines will never cause exceptions if they are   */
/*    given well-formed operands, even if the value of the operands   */
/*    is inappropriate for the operation and DECCHECK is not set.     */
/*    (Except for SIGFPE, as and where documented.)		      */
/*								      */
/* 8. Subset arithmetic is available only if DECSUBSET is set to 1.   */
/* ------------------------------------------------------------------ */
/* Implementation notes for maintenance of this module: 	      */
/*								      */
/* 1. Storage leak protection:	Routines which use malloc are not     */
/*    permitted to use return for fastpath or error exits (i.e.,      */
/*    they follow strict structured programming conventions).	      */
/*    Instead they have a do{}while(0); construct surrounding the     */
/*    code which is protected -- break may be used to exit this.      */
/*    Other routines can safely use the return statement inline.      */
/*								      */
/*    Storage leak accounting can be enabled using DECALLOC.	      */
/*								      */
/* 2. All loops use the for(;;) construct.  Any do construct does     */
/*    not loop; it is for allocation protection as just described.    */
/*								      */
/* 3. Setting status in the context must always be the very last      */
/*    action in a routine, as non-0 status may raise a trap and hence */
/*    the call to set status may not return (if the handler uses long */
/*    jump).  Therefore all cleanup must be done first.  In general,  */
/*    to achieve this status is accumulated and is only applied just  */
/*    before return by calling decContextSetStatus (via decStatus).   */
/*								      */
/*    Routines which allocate storage cannot, in general, use the     */
/*    'top level' routines which could cause a non-returning	      */
/*    transfer of control.  The decXxxxOp routines are safe (do not   */
/*    call decStatus even if traps are set in the context) and should */
/*    be used instead (they are also a little faster).		      */
/*								      */
/* 4. Exponent checking is minimized by allowing the exponent to      */
/*    grow outside its limits during calculations, provided that      */
/*    the decFinalize function is called later.  Multiplication and   */
/*    division, and intermediate calculations in exponentiation,      */
/*    require more careful checks because of the risk of 31-bit       */
/*    overflow (the most negative valid exponent is -1999999997, for  */
/*    a 999999999-digit number with adjusted exponent of -999999999). */
/*								      */
/* 5. Rounding is deferred until finalization of results, with any    */
/*    'off to the right' data being represented as a single digit     */
/*    residue (in the range -1 through 9).  This avoids any double-   */
/*    rounding when more than one shortening takes place (for	      */
/*    example, when a result is subnormal).			      */
/*								      */
/* 6. The digits count is allowed to rise to a multiple of DECDPUN    */
/*    during many operations, so whole Units are handled and exact    */
/*    accounting of digits is not needed.  The correct digits value   */
/*    is found by decGetDigits, which accounts for leading zeros.     */
/*    This must be called before any rounding if the number of digits */
/*    is not known exactly.					      */
/*								      */
/* 7. The multiply-by-reciprocal 'trick' is used for partitioning     */
/*    numbers up to four digits, using appropriate constants.  This   */
/*    is not useful for longer numbers because overflow of 32 bits    */
/*    would lead to 4 multiplies, which is almost as expensive as     */
/*    a divide (unless a floating-point or 64-bit multiply is	      */
/*    assumed to be available). 				      */
/*								      */
/* 8. Unusual abbreviations that may be used in the commentary:       */
/*	lhs -- left hand side (operand, of an operation)	      */
/*	lsd -- least significant digit (of coefficient) 	      */
/*	lsu -- least significant Unit (of coefficient)		      */
/*	msd -- most significant digit (of coefficient)		      */
/*	msi -- most significant item (in an array)		      */
/*	msu -- most significant Unit (of coefficient)		      */
/*	rhs -- right hand side (operand, of an operation)	      */
/*	+ve -- positive 					      */
/*	-ve -- negative 					      */
/*	**  -- raise to the power				      */
/* ------------------------------------------------------------------ */

#include <stdlib.h>		   /* for malloc, free, etc. */
#include <stdio.h>		   /* for printf [if needed] */
#include <string.h>		   /* for strcpy */
#include <ctype.h>		   /* for lower */
#include "dconfig.h"		   /* for GCC definitions */
#include "decNumber.h"		   /* base number library */
#include "decNumberLocal.h"	   /* decNumber local types, etc. */

/* Constants */
/* Public lookup table used by the D2U macro */
const uByte d2utable[DECMAXD2U+1]=D2UTABLE;

#define DECVERB     1		   /* set to 1 for verbose DECCHECK */
#define powers	    DECPOWERS	   /* old internal name */

/* Local constants */
#define DIVIDE	    0x80	   /* Divide operators */
#define REMAINDER   0x40	   /* .. */
#define DIVIDEINT   0x20	   /* .. */
#define REMNEAR     0x10	   /* .. */
#define COMPARE     0x01	   /* Compare operators */
#define COMPMAX     0x02	   /* .. */
#define COMPMIN     0x03	   /* .. */
#define COMPTOTAL   0x04	   /* .. */
#define COMPNAN     0x05	   /* .. [NaN processing] */
#define COMPSIG     0x06	   /* .. [signaling COMPARE] */
#define COMPMAXMAG  0x07	   /* .. */
#define COMPMINMAG  0x08	   /* .. */

#define DEC_sNaN     0x40000000    /* local status: sNaN signal */
#define BADINT	(Int)0x80000000    /* most-negative Int; error indicator */
/* Next two indicate an integer >= 10**6, and its parity (bottom bit) */
#define BIGEVEN (Int)0x80000002
#define BIGODD	(Int)0x80000003

static Unit uarrone[1]={1};   /* Unit array of 1, used for incrementing */

/* Granularity-dependent code */
#if DECDPUN<=4
  #define eInt	Int	      /* extended integer */
  #define ueInt uInt	      /* unsigned extended integer */
  /* Constant multipliers for divide-by-power-of five using reciprocal */
  /* multiply, after removing powers of 2 by shifting, and final shift */
  /* of 17 [we only need up to **4] */
  static const uInt multies[]={131073, 26215, 5243, 1049, 210};
  /* QUOT10 -- macro to return the quotient of unit u divided by 10**n */
  #define QUOT10(u, n) ((((uInt)(u)>>(n))*multies[n])>>17)
#else
  /* For DECDPUN>4 non-ANSI-89 64-bit types are needed. */
  #if !DECUSE64
    #error decNumber.c: DECUSE64 must be 1 when DECDPUN>4
  #endif
  #define eInt	Long	      /* extended integer */
  #define ueInt uLong	      /* unsigned extended integer */
#endif

/* Local routines */
static decNumber * decAddOp(decNumber *, const decNumber *, const decNumber *,
			      decContext *, uByte, uInt *);
static Flag	   decBiStr(const char *, const char *, const char *);
static uInt	   decCheckMath(const decNumber *, decContext *, uInt *);
static void	   decApplyRound(decNumber *, decContext *, Int, uInt *);
static Int	   decCompare(const decNumber *lhs, const decNumber *rhs, Flag);
static decNumber * decCompareOp(decNumber *, const decNumber *,
			      const decNumber *, decContext *,
			      Flag, uInt *);
static void	   decCopyFit(decNumber *, const decNumber *, decContext *,
			      Int *, uInt *);
static decNumber * decDecap(decNumber *, Int);
static decNumber * decDivideOp(decNumber *, const decNumber *,
			      const decNumber *, decContext *, Flag, uInt *);
static decNumber * decExpOp(decNumber *, const decNumber *,
			      decContext *, uInt *);
static void	   decFinalize(decNumber *, decContext *, Int *, uInt *);
static Int	   decGetDigits(Unit *, Int);
static Int	   decGetInt(const decNumber *);
static decNumber * decLnOp(decNumber *, const decNumber *,
			      decContext *, uInt *);
static decNumber * decMultiplyOp(decNumber *, const decNumber *,
			      const decNumber *, decContext *,
			      uInt *);
static decNumber * decNaNs(decNumber *, const decNumber *,
			      const decNumber *, decContext *, uInt *);
static decNumber * decQuantizeOp(decNumber *, const decNumber *,
			      const decNumber *, decContext *, Flag,
			      uInt *);
static void	   decReverse(Unit *, Unit *);
static void	   decSetCoeff(decNumber *, decContext *, const Unit *,
			      Int, Int *, uInt *);
static void	   decSetMaxValue(decNumber *, decContext *);
static void	   decSetOverflow(decNumber *, decContext *, uInt *);
static void	   decSetSubnormal(decNumber *, decContext *, Int *, uInt *);
static Int	   decShiftToLeast(Unit *, Int, Int);
static Int	   decShiftToMost(Unit *, Int, Int);
static void	   decStatus(decNumber *, uInt, decContext *);
static void	   decToString(const decNumber *, char[], Flag);
static decNumber * decTrim(decNumber *, decContext *, Flag, Flag, Int *);
static Int	   decUnitAddSub(const Unit *, Int, const Unit *, Int, Int,
			      Unit *, Int);
static Int	   decUnitCompare(const Unit *, Int, const Unit *, Int, Int);

#if !DECSUBSET
/* decFinish == decFinalize when no subset arithmetic needed */
#define decFinish(a,b,c,d) decFinalize(a,b,c,d)
#else
static void	   decFinish(decNumber *, decContext *, Int *, uInt *);
static decNumber * decRoundOperand(const decNumber *, decContext *, uInt *);
#endif

/* Local macros */
/* masked special-values bits */
#define SPECIALARG  (rhs->bits & DECSPECIAL)
#define SPECIALARGS ((lhs->bits | rhs->bits) & DECSPECIAL)

/* Diagnostic macros, etc. */
#if DECALLOC
/* Handle malloc/free accounting.  If enabled, our accountable routines */
/* are used; otherwise the code just goes straight to the system malloc */
/* and free routines. */
#define malloc(a) decMalloc(a)
#define free(a) decFree(a)
#define DECFENCE 0x5a		   /* corruption detector */
/* 'Our' malloc and free: */
static void *decMalloc(size_t);
static void  decFree(void *);
uInt decAllocBytes=0;		   /* count of bytes allocated */
/* Note that DECALLOC code only checks for storage buffer overflow. */
/* To check for memory leaks, the decAllocBytes variable must be */
/* checked to be 0 at appropriate times (e.g., after the test */
/* harness completes a set of tests).  This checking may be unreliable */
/* if the testing is done in a multi-thread environment. */
#endif

#if DECCHECK
/* Optional checking routines.	Enabling these means that decNumber */
/* and decContext operands to operator routines are checked for */
/* correctness.  This roughly doubles the execution time of the */
/* fastest routines (and adds 600+ bytes), so should not normally be */
/* used in 'production'. */
/* decCheckInexact is used to check that inexact results have a full */
/* complement of digits (where appropriate -- this is not the case */
/* for Quantize, for example) */
#define DECUNRESU ((decNumber *)(void *)0xffffffff)
#define DECUNUSED ((const decNumber *)(void *)0xffffffff)
#define DECUNCONT ((decContext *)(void *)(0xffffffff))
static Flag decCheckOperands(decNumber *, const decNumber *,
			     const decNumber *, decContext *);
static Flag decCheckNumber(const decNumber *);
static void decCheckInexact(const decNumber *, decContext *);
#endif

#if DECTRACE || DECCHECK
/* Optional trace/debugging routines (may or may not be used) */
void decNumberShow(const decNumber *);	/* displays the components of a number */
static void decDumpAr(char, const Unit *, Int);
#endif

/* ================================================================== */
/* Conversions							      */
/* ================================================================== */

/* ------------------------------------------------------------------ */
/* from-int32 -- conversion from Int or uInt			      */
/*								      */
/*  dn is the decNumber to receive the integer			      */
/*  in or uin is the integer to be converted			      */
/*  returns dn							      */
/*								      */
/* No error is possible.					      */
/* ------------------------------------------------------------------ */
decNumber * decNumberFromInt32(decNumber *dn, Int in) {
  uInt unsig;
  
  if (in >= 0) {
    unsig = in;
  } else if (in == BADINT) {
    unsig = (uInt)2147483648U;
  } else {
    unsig = -in;
  }
  
  decNumberFromUInt32(dn, unsig);
  
  if (in < 0) {
    dn->bits = DECNEG;
  }
  
  return dn;
} /* decNumberFromInt32 */

decNumber * decNumberFromUInt32(decNumber *dn, uInt uin) {
  Unit *up;
  decNumberZero(dn);
  if (uin == 0) return dn;
  
  up = dn->lsu;
  while (uin > 0) {
    *up = (Unit)(uin % (DECDPUNMAX + 1));
    uin = uin / (DECDPUNMAX + 1);
    up++;
  }
  
  dn->digits = decGetDigits(dn->lsu, up - dn->lsu);
  return dn;
} /* decNumberFromUInt32 */

/* ------------------------------------------------------------------ */
/* to-int32 -- conversion to Int or uInt			      */
/*								      */
/*  dn is the decNumber to convert				      */
/*  set is the context for reporting errors			      */
/*  returns the converted decNumber, or 0 if Invalid is set	      */
/*								      */
/* Invalid is set if the decNumber does not have exponent==0 or if    */
/* it is a NaN, Infinite, or out-of-range.			      */
/* ------------------------------------------------------------------ */
Int decNumberToInt32(const decNumber *dn, decContext *set) {
  #if DECCHECK
  if (decCheckOperands(DECUNRESU, DECUNUSED, dn, set)) return 0;
  #endif

  if ((dn->bits & DECSPECIAL) || dn->digits > 10 || dn->exponent != 0) {
    decContextSetStatus(set, DEC_Invalid_operation);
    return 0;
  }

  const Unit *up = dn->lsu;
  uInt lo = *up;
  uInt hi = 0;

  #if DECDPUN > 1
  hi = lo / 10;
  lo = lo % 10;
  #endif

  up++;
  
  for (Int d = DECDPUN; d < dn->digits; up++, d += DECDPUN) {
    hi += *up * powers[d - 1];
  }

  if (hi > 214748364 || (hi == 214748364 && lo > 7)) {
    if ((dn->bits & DECNEG) && hi == 214748364 && lo == 8) {
      return 0x80000000;
    }
    decContextSetStatus(set, DEC_Invalid_operation);
    return 0;
  }

  Int result = X10(hi) + lo;
  return (dn->bits & DECNEG) ? -result : result;
} /* decNumberToInt32 */

uInt decNumberToUInt32(const decNumber *dn, decContext *set) {
  #if DECCHECK
  if (decCheckOperands(DECUNRESU, DECUNUSED, dn, set)) return 0;
  #endif
  
  if (dn->bits & DECSPECIAL) {
    decContextSetStatus(set, DEC_Invalid_operation);
    return 0;
  }
  
  if (dn->digits > 10 || dn->exponent != 0) {
    decContextSetStatus(set, DEC_Invalid_operation);
    return 0;
  }
  
  if ((dn->bits & DECNEG) && !ISZERO(dn)) {
    decContextSetStatus(set, DEC_Invalid_operation);
    return 0;
  }
  
  const Unit *up = dn->lsu;
  uInt lo = *up;
  uInt hi = 0;
  
  #if DECDPUN > 1
  hi = lo / 10;
  lo = lo % 10;
  #endif
  
  up++;
  
  for (Int d = DECDPUN; d < dn->digits; d += DECDPUN) {
    hi += *up * powers[d - 1];
    up++;
  }
  
  if (hi > 429496729) {
    decContextSetStatus(set, DEC_Invalid_operation);
    return 0;
  }
  
  if (hi == 429496729 && lo > 5) {
    decContextSetStatus(set, DEC_Invalid_operation);
    return 0;
  }
  
  return X10(hi) + lo;
} /* decNumberToUInt32 */

/* ------------------------------------------------------------------ */
/* to-scientific-string -- conversion to numeric string 	      */
/* to-engineering-string -- conversion to numeric string	      */
/*								      */
/*   decNumberToString(dn, string);				      */
/*   decNumberToEngString(dn, string);				      */
/*								      */
/*  dn is the decNumber to convert				      */
/*  string is the string where the result will be laid out	      */
/*								      */
/*  string must be at least dn->digits+14 characters long	      */
/*								      */
/*  No error is possible, and no status can be set.		      */
/* ------------------------------------------------------------------ */
char * decNumberToString(const decNumber *dn, char *string) {
    if (dn == NULL || string == NULL) {
        return NULL;
    }
    decToString(dn, string, 0);
    return string;
} /* DecNumberToString */

char *decNumberToEngString(const decNumber *dn, char *string) {
    if (dn == NULL || string == NULL) {
        return NULL;
    }
    decToString(dn, string, 1);
    return string;
} /* DecNumberToEngString */

/* ------------------------------------------------------------------ */
/* to-number -- conversion from numeric string			      */
/*								      */
/* decNumberFromString -- convert string to decNumber		      */
/*   dn        -- the number structure to fill			      */
/*   chars[]   -- the string to convert ('\0' terminated)	      */
/*   set       -- the context used for processing any error,	      */
/*		  determining the maximum precision available	      */
/*		  (set.digits), determining the maximum and minimum   */
/*		  exponent (set.emax and set.emin), determining if    */
/*		  extended values are allowed, and checking the       */
/*		  rounding mode if overflow occurs or rounding is     */
/*		  needed.					      */
/*								      */
/* The length of the coefficient and the size of the exponent are     */
/* checked by this routine, so the correct error (Underflow or	      */
/* Overflow) can be reported or rounding applied, as necessary.       */
/*								      */
/* If bad syntax is detected, the result will be a quiet NaN.	      */
/* ------------------------------------------------------------------ */
decNumber * decNumberFromString(decNumber *dn, const char chars[], decContext *set) {
  Int exponent = 0;
  uByte bits = 0;
  Unit *res;
  Unit resbuff[SD2U(DECBUFFER+9)];
  Unit *allocres = NULL;
  Int d = 0;
  const char *dotchar = NULL;
  const char *cfirst = chars;
  const char *last = NULL;
  const char *c;
  Unit *up;
  #if DECDPUN>1
  Int cut, out;
  #endif
  Int residue;
  uInt status = 0;

  #if DECCHECK
  if (decCheckOperands(DECUNRESU, DECUNUSED, DECUNUSED, set))
    return decNumberZero(dn);
  #endif

  for (c = chars; ; c++) {
    if (*c >= '0' && *c <= '9') {
      last = c;
      d++;
      continue;
    }
    if (*c == '.' && dotchar == NULL) {
      dotchar = c;
      if (c == cfirst) cfirst++;
      continue;
    }
    if (c == chars) {
      if (*c == '-') {
        cfirst++;
        bits = DECNEG;
        continue;
      }
      if (*c == '+') {
        cfirst++;
        continue;
      }
    }
    break;
  }

  if (last == NULL) {
    status = DEC_Conversion_syntax;
    if (*c == '\0') {
      decStatus(dn, status, set);
      return dn;
    }
    #if DECSUBSET
    if (!set->extended) {
      decStatus(dn, status, set);
      return dn;
    }
    #endif
    if (dotchar != NULL) {
      decStatus(dn, status, set);
      return dn;
    }
    decNumberZero(dn);
    if (decBiStr(c, "infinity", "INFINITY") || decBiStr(c, "inf", "INF")) {
      dn->bits = bits | DECINF;
      return dn;
    }
    dn->bits = bits | DECNAN;
    if (*c == 's' || *c == 'S') {
      c++;
      dn->bits = bits | DECSNAN;
    }
    if ((*c != 'n' && *c != 'N') || 
        (*(c+1) != 'a' && *(c+1) != 'A') || 
        (*(c+2) != 'n' && *(c+2) != 'N')) {
      decStatus(dn, status, set);
      return dn;
    }
    c += 3;
    for (cfirst = c; *cfirst == '0';) cfirst++;
    if (*cfirst == '\0') return dn;
    for (c = cfirst; ; c++, d++) {
      if (*c < '0' || *c > '9') break;
      last = c;
    }
    if (*c != '\0') {
      decStatus(dn, DEC_Conversion_syntax, set);
      return dn;
    }
    if (d > set->digits - 1) {
      if (set->clamp || d > set->digits) {
        decStatus(dn, DEC_Conversion_syntax, set);
        return dn;
      }
    }
    bits = dn->bits;
  } else if (*c != '\0') {
    Flag nege = 0;
    const char *firstexp;
    status = DEC_Conversion_syntax;
    if (*c != 'e' && *c != 'E') {
      decStatus(dn, status, set);
      return dn;
    }
    c++;
    if (*c == '-') {
      nege = 1;
      c++;
    } else if (*c == '+') {
      c++;
    }
    if (*c == '\0') {
      decStatus(dn, status, set);
      return dn;
    }
    for (; *c == '0' && *(c+1) != '\0';) c++;
    firstexp = c;
    for (; ; c++) {
      if (*c < '0' || *c > '9') break;
      exponent = X10(exponent) + (Int)*c - (Int)'0';
    }
    if (*c != '\0') {
      decStatus(dn, status, set);
      return dn;
    }
    if (c >= firstexp + 9 + 1) {
      if (c > firstexp + 9 + 1 || *firstexp > '1') exponent = DECNUMMAXE * 2;
    }
    if (nege) exponent = -exponent;
    status = 0;
  }

  if (*cfirst == '0') {
    for (c = cfirst; c < last; c++, cfirst++) {
      if (*c == '.') continue;
      if (*c != '0') break;
      d--;
    }
    #if DECSUBSET
    if (*cfirst == '0' && !set->extended) {
      decNumberZero(dn);
      return dn;
    }
    #endif
  }

  if (dotchar != NULL && dotchar < last)
    exponent -= (last - dotchar);

  if (d <= set->digits) {
    res = dn->lsu;
  } else {
    Int needbytes = D2U(d) * sizeof(Unit);
    res = resbuff;
    if (needbytes > (Int)sizeof(resbuff)) {
      allocres = (Unit *)malloc(needbytes);
      if (allocres == NULL) {
        status |= DEC_Insufficient_storage;
        decStatus(dn, status, set);
        return dn;
      }
      res = allocres;
    }
  }

  #if DECDPUN>1
  out = 0;
  up = res + D2U(d) - 1;
  cut = d - (up - res) * DECDPUN;
  for (c = cfirst; ; c++) {
    if (*c == '.') continue;
    out = X10(out) + (Int)*c - (Int)'0';
    if (c == last) break;
    cut--;
    if (cut > 0) continue;
    *up = (Unit)out;
    up--;
    cut = DECDPUN;
    out = 0;
  }
  *up = (Unit)out;
  #else
  up = res;
  for (c = last; c >= cfirst; c--) {
    if (*c == '.') continue;
    *up = (Unit)((Int)*c - (Int)'0');
    up++;
  }
  #endif

  dn->bits = bits;
  dn->exponent = exponent;
  dn->digits = d;

  if (d > set->digits) {
    residue = 0;
    decSetCoeff(dn, set, res, d, &residue, &status);
    decFinalize(dn, set, &residue, &status);
  } else {
    if ((dn->exponent - 1 < set->emin - dn->digits) ||
        (dn->exponent - 1 > set->emax - set->digits)) {
      residue = 0;
      decFinalize(dn, set, &residue, &status);
    }
  }

  free(allocres);
  if (status != 0) decStatus(dn, status, set);
  return dn;
} /* decNumberFromString */

/* ================================================================== */
/* Operators							      */
/* ================================================================== */

/* ------------------------------------------------------------------ */
/* decNumberAbs -- absolute value operator			      */
/*								      */
/*   This computes C = abs(A)					      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context 					      */
/*								      */
/* See also decNumberCopyAbs for a quiet bitwise version of this.     */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
/* This has the same effect as decNumberPlus unless A is negative,    */
/* in which case it has the same effect as decNumberMinus.	      */
/* ------------------------------------------------------------------ */
decNumber * decNumberAbs(decNumber *res, const decNumber *rhs,
                         decContext *set) {
  decNumber dzero;
  uInt status = 0;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  decNumberZero(&dzero);
  dzero.exponent = rhs->exponent;
  
  uByte negFlag = (uByte)(rhs->bits & DECNEG);
  decAddOp(res, &dzero, rhs, set, negFlag, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  
  return res;
} /* decNumberAbs */

/* ------------------------------------------------------------------ */
/* decNumberAdd -- add two Numbers				      */
/*								      */
/*   This computes C = A + B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X+X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
/* This just calls the routine shared with Subtract		      */
decNumber * decNumberAdd(decNumber *res, const decNumber *lhs,
                         const decNumber *rhs, decContext *set) {
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
    return NULL;
  }
  
  uInt status = 0;
  decAddOp(res, lhs, rhs, set, 0, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  
  return res;
} /* decNumberAdd */

/* ------------------------------------------------------------------ */
/* decNumberAnd -- AND two Numbers, digitwise			      */
/*								      */
/*   This computes C = A & B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X&X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context (used for result length and error report)     */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Logical function restrictions apply (see above); a NaN is	      */
/* returned with Invalid_operation if a restriction is violated.      */
/* ------------------------------------------------------------------ */
decNumber * decNumberAnd(decNumber *res, const decNumber *lhs,
			 const decNumber *rhs, decContext *set) {
  const Unit *ua, *ub;
  const Unit *msua, *msub;
  Unit *uc, *msuc;
  Int msudigs;
  
  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  if (lhs->exponent != 0 || decNumberIsSpecial(lhs) || decNumberIsNegative(lhs) ||
      rhs->exponent != 0 || decNumberIsSpecial(rhs) || decNumberIsNegative(rhs)) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  ua = lhs->lsu;
  ub = rhs->lsu;
  uc = res->lsu;
  msua = ua + D2U(lhs->digits) - 1;
  msub = ub + D2U(rhs->digits) - 1;
  msuc = uc + D2U(set->digits) - 1;
  msudigs = MSUDIGITS(set->digits);

  for (; uc <= msuc; ua++, ub++, uc++) {
    Unit a = (ua > msua) ? 0 : *ua;
    Unit b = (ub > msub) ? 0 : *ub;
    *uc = 0;

    if (a | b) {
      for (Int i = 0; i < DECDPUN; i++) {
        Int digit_a = a % 10;
        Int digit_b = b % 10;
        
        if ((digit_a | digit_b) > 1) {
          decStatus(res, DEC_Invalid_operation, set);
          return res;
        }
        
        if (digit_a & digit_b) {
          *uc += (Unit)powers[i];
        }
        
        a /= 10;
        b /= 10;
        
        if (uc == msuc && i == msudigs - 1) {
          break;
        }
      }
    }
  }

  res->digits = decGetDigits(res->lsu, uc - res->lsu);
  res->exponent = 0;
  res->bits = 0;
  return res;
} /* decNumberAnd */

/* ------------------------------------------------------------------ */
/* decNumberCompare -- compare two Numbers			      */
/*								      */
/*   This computes C = A ? B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X?X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for one digit (or NaN).			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberCompare(decNumber *res, const decNumber *lhs,
                            const decNumber *rhs, decContext *set) {
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
    return res;
  }
  
  uInt status = 0;
  decCompareOp(res, lhs, rhs, set, COMPARE, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  return res;
} /* decNumberCompare */

/* ------------------------------------------------------------------ */
/* decNumberCompareSignal -- compare, signalling on all NaNs	      */
/*								      */
/*   This computes C = A ? B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X?X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for one digit (or NaN).			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberCompareSignal(decNumber *res, const decNumber *lhs,
                                   const decNumber *rhs, decContext *set) {
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
    return res;
  }
  
  uInt status = 0;
  decCompareOp(res, lhs, rhs, set, COMPSIG, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  return res;
} /* decNumberCompareSignal */

/* ------------------------------------------------------------------ */
/* decNumberCompareTotal -- compare two Numbers, using total ordering */
/*								      */
/*   This computes C = A ? B, under total ordering		      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X?X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for one digit; the result will always be one of  */
/* -1, 0, or 1. 						      */
/* ------------------------------------------------------------------ */
decNumber* decNumberCompareTotal(decNumber *res, const decNumber *lhs,
                                  const decNumber *rhs, decContext *set) {
    if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
        return res;
    }
    
    uInt status = 0;
    decCompareOp(res, lhs, rhs, set, COMPTOTAL, &status);
    
    if (status != 0) {
        decStatus(res, status, set);
    }
    
    return res;
} /* decNumberCompareTotal */

/* ------------------------------------------------------------------ */
/* decNumberCompareTotalMag -- compare, total ordering of magnitudes  */
/*								      */
/*   This computes C = |A| ? |B|, under total ordering		      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X?X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for one digit; the result will always be one of  */
/* -1, 0, or 1. 						      */
/* ------------------------------------------------------------------ */
decNumber * decNumberCompareTotalMag(decNumber *res, const decNumber *lhs,
				     const decNumber *rhs, decContext *set) {
  uInt status = 0;
  decNumber bufa[D2N(DECBUFFER+1)];
  decNumber bufb[D2N(DECBUFFER+1)];
  decNumber *allocbufa = NULL;
  decNumber *allocbufb = NULL;
  const decNumber *abs_lhs = lhs;
  const decNumber *abs_rhs = rhs;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  if (decNumberIsNegative(lhs)) {
    uInt needbytes = sizeof(decNumber) + (D2U(lhs->digits) - 1) * sizeof(Unit);
    decNumber *a = bufa;
    
    if (needbytes > sizeof(bufa)) {
      allocbufa = (decNumber *)malloc(needbytes);
      if (allocbufa == NULL) {
        status |= DEC_Insufficient_storage;
        goto cleanup;
      }
      a = allocbufa;
    }
    
    decNumberCopy(a, lhs);
    a->bits &= ~DECNEG;
    abs_lhs = a;
  }
  
  if (decNumberIsNegative(rhs)) {
    uInt needbytes = sizeof(decNumber) + (D2U(rhs->digits) - 1) * sizeof(Unit);
    decNumber *b = bufb;
    
    if (needbytes > sizeof(bufb)) {
      allocbufb = (decNumber *)malloc(needbytes);
      if (allocbufb == NULL) {
        status |= DEC_Insufficient_storage;
        goto cleanup;
      }
      b = allocbufb;
    }
    
    decNumberCopy(b, rhs);
    b->bits &= ~DECNEG;
    abs_rhs = b;
  }
  
  decCompareOp(res, abs_lhs, abs_rhs, set, COMPTOTAL, &status);

cleanup:
  free(allocbufa);
  free(allocbufb);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  return res;
} /* decNumberCompareTotalMag */

/* ------------------------------------------------------------------ */
/* decNumberDivide -- divide one number by another		      */
/*								      */
/*   This computes C = A / B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X/X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberDivide(decNumber *res, const decNumber *lhs,
                            const decNumber *rhs, decContext *set) {
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
    return res;
  }
  
  uInt status = 0;
  decDivideOp(res, lhs, rhs, set, DIVIDE, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  
  return res;
} /* decNumberDivide */

/* ------------------------------------------------------------------ */
/* decNumberDivideInteger -- divide and return integer quotient       */
/*								      */
/*   This computes C = A # B, where # is the integer divide operator  */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X#X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberDivideInteger(decNumber *res, const decNumber *lhs,
                                   const decNumber *rhs, decContext *set) {
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
    return res;
  }
  
  uInt status = 0;
  decDivideOp(res, lhs, rhs, set, DIVIDEINT, &status);
  if (status != 0) {
    decStatus(res, status, set);
  }
  return res;
} /* decNumberDivideInteger */

/* ------------------------------------------------------------------ */
/* decNumberExp -- exponentiation				      */
/*								      */
/*   This computes C = exp(A)					      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context; note that rounding mode has no effect	      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Mathematical function restrictions apply (see above); a NaN is     */
/* returned with Invalid_operation if a restriction is violated.      */
/*								      */
/* Finite results will always be full precision and Inexact, except   */
/* when A is a zero or -Infinity (giving 1 or 0 respectively).	      */
/*								      */
/* An Inexact result is rounded using DEC_ROUND_HALF_EVEN; it will    */
/* almost always be correctly rounded, but may be up to 1 ulp in      */
/* error in rare cases. 					      */
/* ------------------------------------------------------------------ */
/* This is a wrapper for decExpOp which can handle the slightly wider */
/* (double) range needed by Ln (which has to be able to calculate     */
/* exp(-a) where a can be the tiniest number (Ntiny).		      */
/* ------------------------------------------------------------------ */
decNumber * decNumberExp(decNumber *res, const decNumber *rhs, decContext *set) {
  uInt status = 0;
  #if DECSUBSET
  decNumber *allocrhs = NULL;
  #endif

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  if (decCheckMath(rhs, set, &status)) {
    if (status != 0) decStatus(res, status, set);
    return res;
  }

  #if DECSUBSET
  if (!set->extended && rhs->digits > set->digits) {
    allocrhs = decRoundOperand(rhs, set, &status);
    if (allocrhs == NULL) {
      if (status != 0) decStatus(res, status, set);
      return res;
    }
    rhs = allocrhs;
  }
  #endif

  decExpOp(res, rhs, set, &status);

  #if DECSUBSET
  free(allocrhs);
  #endif

  if (status != 0) decStatus(res, status, set);

  #if DECCHECK
  decCheckInexact(res, set);
  #endif

  return res;
} /* decNumberExp */

/* ------------------------------------------------------------------ */
/* decNumberFMA -- fused multiply add				      */
/*								      */
/*   This computes D = (A * B) + C with only one rounding	      */
/*								      */
/*   res is D, the result.  D may be A or B or C (e.g., X=FMA(X,X,X)) */
/*   lhs is A							      */
/*   rhs is B							      */
/*   fhs is C [far hand side]					      */
/*   set is the context 					      */
/*								      */
/* Mathematical function restrictions apply (see above); a NaN is     */
/* returned with Invalid_operation if a restriction is violated.      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberFMA(decNumber *res, const decNumber *lhs,
			 const decNumber *rhs, const decNumber *fhs,
			 decContext *set) {
  uInt status=0;
  decContext dcmul;
  uInt needbytes;
  decNumber bufa[D2N(DECBUFFER*2+1)];
  decNumber *allocbufa=NULL;
  decNumber *acc;
  decNumber dzero;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  if (decCheckOperands(res, fhs, DECUNUSED, set)) return res;
  #endif

  #if DECSUBSET
  if (!set->extended) {
    status|=DEC_Invalid_operation;
    decStatus(res, status, set);
    return res;
  }
  #endif

  if ((!decNumberIsSpecial(lhs) && decCheckMath(lhs, set, &status))
   || (!decNumberIsSpecial(rhs) && decCheckMath(rhs, set, &status))
   || (!decNumberIsSpecial(fhs) && decCheckMath(fhs, set, &status))) {
    decStatus(res, status, set);
    return res;
  }

  dcmul=*set;
  dcmul.digits=lhs->digits+rhs->digits;
  dcmul.emax=DEC_MAX_EMAX;
  dcmul.emin=DEC_MIN_EMIN;

  acc=bufa;
  needbytes=sizeof(decNumber)+(D2U(dcmul.digits)-1)*sizeof(Unit);
  if (needbytes>sizeof(bufa)) {
    allocbufa=(decNumber *)malloc(needbytes);
    if (allocbufa==NULL) {
      status|=DEC_Insufficient_storage;
      decStatus(res, status, set);
      return res;
    }
    acc=allocbufa;
  }

  decMultiplyOp(acc, lhs, rhs, &dcmul, &status);

  if ((status&DEC_Invalid_operation)!=0) {
    if (!(status&DEC_sNaN)) {
      decNumberZero(res);
      res->bits=DECNAN;
      free(allocbufa);
      decStatus(res, status, set);
      return res;
    }
    decNumberZero(&dzero);
    fhs=&dzero;
  }
  #if DECCHECK
  else {
    if (status!=0) printf("Status=%08lx after FMA multiply\n", (LI)status);
  }
  #endif

  decAddOp(res, acc, fhs, set, 0, &status);

  free(allocbufa);
  if (status!=0) decStatus(res, status, set);
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  return res;
} /* decNumberFMA */

/* ------------------------------------------------------------------ */
/* decNumberInvert -- invert a Number, digitwise		      */
/*								      */
/*   This computes C = ~A					      */
/*								      */
/*   res is C, the result.  C may be A (e.g., X=~X)		      */
/*   rhs is A							      */
/*   set is the context (used for result length and error report)     */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Logical function restrictions apply (see above); a NaN is	      */
/* returned with Invalid_operation if a restriction is violated.      */
/* ------------------------------------------------------------------ */
decNumber * decNumberInvert(decNumber *res, const decNumber *rhs,
                           decContext *set) {
  const Unit *ua, *msua;
  Unit *uc, *msuc;
  Int msudigs;
  
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  if (rhs->exponent != 0 || decNumberIsSpecial(rhs) || decNumberIsNegative(rhs)) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  ua = rhs->lsu;
  uc = res->lsu;
  msua = ua + D2U(rhs->digits) - 1;
  msuc = uc + D2U(set->digits) - 1;
  msudigs = MSUDIGITS(set->digits);

  for (; uc <= msuc; ua++, uc++) {
    Unit a = (ua > msua) ? 0 : *ua;
    *uc = 0;

    for (Int i = 0; i < DECDPUN; i++) {
      if ((~a) & 1) {
        *uc = *uc + (Unit)powers[i];
      }
      
      Int digit = a % 10;
      a = a / 10;
      
      if (digit > 1) {
        decStatus(res, DEC_Invalid_operation, set);
        return res;
      }
      
      if (uc == msuc && i == msudigs - 1) {
        break;
      }
    }
  }

  res->digits = decGetDigits(res->lsu, uc - res->lsu);
  res->exponent = 0;
  res->bits = 0;
  return res;
} /* decNumberInvert */

/* ------------------------------------------------------------------ */
/* decNumberLn -- natural logarithm				      */
/*								      */
/*   This computes C = ln(A)					      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context; note that rounding mode has no effect	      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Notable cases:						      */
/*   A<0 -> Invalid						      */
/*   A=0 -> -Infinity (Exact)					      */
/*   A=+Infinity -> +Infinity (Exact)				      */
/*   A=1 exactly -> 0 (Exact)					      */
/*								      */
/* Mathematical function restrictions apply (see above); a NaN is     */
/* returned with Invalid_operation if a restriction is violated.      */
/*								      */
/* An Inexact result is rounded using DEC_ROUND_HALF_EVEN; it will    */
/* almost always be correctly rounded, but may be up to 1 ulp in      */
/* error in rare cases. 					      */
/* ------------------------------------------------------------------ */
/* This is a wrapper for decLnOp which can handle the slightly wider  */
/* (+11) range needed by Ln, Log10, etc. (which may have to be able   */
/* to calculate at p+e+2).					      */
/* ------------------------------------------------------------------ */
decNumber * decNumberLn(decNumber *res, const decNumber *rhs, decContext *set) {
  uInt status = 0;
  #if DECSUBSET
  decNumber *allocrhs = NULL;
  #endif

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  if (decCheckMath(rhs, set, &status)) {
    if (status != 0) decStatus(res, status, set);
    #if DECCHECK
    decCheckInexact(res, set);
    #endif
    return res;
  }

  #if DECSUBSET
  if (!set->extended) {
    if (rhs->digits > set->digits) {
      allocrhs = decRoundOperand(rhs, set, &status);
      if (allocrhs == NULL) {
        if (status != 0) decStatus(res, status, set);
        #if DECCHECK
        decCheckInexact(res, set);
        #endif
        return res;
      }
      rhs = allocrhs;
    }
    if (ISZERO(rhs)) {
      status |= DEC_Invalid_operation;
      free(allocrhs);
      if (status != 0) decStatus(res, status, set);
      #if DECCHECK
      decCheckInexact(res, set);
      #endif
      return res;
    }
  }
  #endif

  decLnOp(res, rhs, set, &status);

  #if DECSUBSET
  free(allocrhs);
  #endif
  
  if (status != 0) decStatus(res, status, set);
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  return res;
} /* decNumberLn */

/* ------------------------------------------------------------------ */
/* decNumberLogB - get adjusted exponent, by 754 rules		      */
/*								      */
/*   This computes C = adjustedexponent(A)			      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context, used only for digits and status	      */
/*								      */
/* C must have space for 10 digits (A might have 10**9 digits and     */
/* an exponent of +999999999, or one digit and an exponent of	      */
/* -1999999999).						      */
/*								      */
/* This returns the adjusted exponent of A after (in theory) padding  */
/* with zeros on the right to set->digits digits while keeping the    */
/* same value.	The exponent is not limited by emin/emax.	      */
/*								      */
/* Notable cases:						      */
/*   A<0 -> Use |A|						      */
/*   A=0 -> -Infinity (Division by zero)			      */
/*   A=Infinite -> +Infinity (Exact)				      */
/*   A=1 exactly -> 0 (Exact)					      */
/*   NaNs are propagated as usual				      */
/* ------------------------------------------------------------------ */
decNumber * decNumberLogB(decNumber *res, const decNumber *rhs,
                          decContext *set) {
  uInt status = 0;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  if (decNumberIsNaN(rhs)) {
    decNaNs(res, rhs, NULL, set, &status);
  }
  else if (decNumberIsInfinite(rhs)) {
    decNumberCopyAbs(res, rhs);
  }
  else if (decNumberIsZero(rhs)) {
    decNumberZero(res);
    res->bits = DECNEG | DECINF;
    status |= DEC_Division_by_zero;
  }
  else {
    Int ae = rhs->exponent + rhs->digits - 1;
    decNumberFromInt32(res, ae);
  }

  if (status != 0) {
    decStatus(res, status, set);
  }
  
  return res;
} /* decNumberLogB */

/* ------------------------------------------------------------------ */
/* decNumberLog10 -- logarithm in base 10			      */
/*								      */
/*   This computes C = log10(A) 				      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context; note that rounding mode has no effect	      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Notable cases:						      */
/*   A<0 -> Invalid						      */
/*   A=0 -> -Infinity (Exact)					      */
/*   A=+Infinity -> +Infinity (Exact)				      */
/*   A=10**n (if n is an integer) -> n (Exact)			      */
/*								      */
/* Mathematical function restrictions apply (see above); a NaN is     */
/* returned with Invalid_operation if a restriction is violated.      */
/*								      */
/* An Inexact result is rounded using DEC_ROUND_HALF_EVEN; it will    */
/* almost always be correctly rounded, but may be up to 1 ulp in      */
/* error in rare cases. 					      */
/* ------------------------------------------------------------------ */
/* This calculates ln(A)/ln(10) using appropriate precision.  For     */
/* ln(A) this is the max(p, rhs->digits + t) + 3, where p is the      */
/* requested digits and t is the number of digits in the exponent     */
/* (maximum 6).  For ln(10) it is p + 3; this is often handled by the */
/* fastpath in decLnOp.  The final division is done to the requested  */
/* precision.							      */
/* ------------------------------------------------------------------ */
decNumber * decNumberLog10(decNumber *res, const decNumber *rhs, decContext *set) {
  uInt status = 0, ignore = 0;
  uInt needbytes;
  Int p;
  Int t;
  
  decNumber bufa[D2N(DECBUFFER+2)];
  decNumber *allocbufa = NULL;
  decNumber *a = bufa;
  decNumber bufb[D2N(DECBUFFER+2)];
  decNumber *allocbufb = NULL;
  decNumber *b = bufb;
  decNumber bufw[D2N(10)];
  decNumber *w = bufw;
  #if DECSUBSET
  decNumber *allocrhs = NULL;
  #endif
  
  decContext aset;
  
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif
  
  if (decCheckMath(rhs, set, &status)) {
    goto cleanup;
  }
  
  #if DECSUBSET
  if (!set->extended) {
    if (rhs->digits > set->digits) {
      allocrhs = decRoundOperand(rhs, set, &status);
      if (allocrhs == NULL) goto cleanup;
      rhs = allocrhs;
    }
    if (ISZERO(rhs)) {
      status |= DEC_Invalid_operation;
      goto cleanup;
    }
  }
  #endif
  
  decContextDefault(&aset, DEC_INIT_DECIMAL64);
  
  if (!(rhs->bits & (DECNEG|DECSPECIAL)) && !ISZERO(rhs)) {
    if (checkExactPowerOf10(w, rhs, res, set, &status, &aset)) {
      goto cleanup;
    }
  }
  
  t = 6;
  p = ((rhs->digits + t > set->digits) ? rhs->digits + t : set->digits) + 3;
  
  allocbufa = allocateBuffer(p, sizeof(bufa), &status);
  if (allocbufa == NULL && status & DEC_Insufficient_storage) {
    goto cleanup;
  }
  if (allocbufa != NULL) a = allocbufa;
  
  aset.digits = p;
  aset.emax = DEC_MAX_MATH;
  aset.emin = -DEC_MAX_MATH;
  aset.clamp = 0;
  
  decLnOp(a, rhs, &aset, &status);
  
  if ((status & DEC_NaNs) && !(status & DEC_sNaN)) {
    goto cleanup;
  }
  if ((a->bits & DECSPECIAL) || ISZERO(a)) {
    decNumberCopy(res, a);
    goto cleanup;
  }
  
  p = set->digits + 3;
  
  allocbufb = allocateBuffer(p, sizeof(bufb), &status);
  if (allocbufb == NULL && status & DEC_Insufficient_storage) {
    goto cleanup;
  }
  if (allocbufb != NULL) b = allocbufb;
  
  setupNumber10(w);
  
  aset.digits = p;
  decLnOp(b, w, &aset, &ignore);
  
  aset.digits = set->digits;
  decDivideOp(res, a, b, &aset, DIVIDE, &status);
  
cleanup:
  free(allocbufa);
  free(allocbufb);
  #if DECSUBSET
  free(allocrhs);
  #endif
  
  if (status != 0) decStatus(res, status, set);
  
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  
  return res;
}

static int checkExactPowerOf10(decNumber *w, const decNumber *rhs, 
                                decNumber *res, decContext *set, 
                                uInt *status, decContext *aset) {
  Int residue = 0;
  uInt copystat = 0;
  
  aset->digits = 1;
  decCopyFit(w, rhs, aset, &residue, &copystat);
  
  if (!(copystat & DEC_Inexact) && w->lsu[0] == 1) {
    decNumberFromInt32(w, w->exponent);
    residue = 0;
    decCopyFit(res, w, set, &residue, status);
    decFinish(res, set, &residue, status);
    return 1;
  }
  return 0;
}

static decNumber* allocateBuffer(Int p, size_t staticSize, uInt *status) {
  uInt needbytes = sizeof(decNumber) + (D2U(p) - 1) * sizeof(Unit);
  if (needbytes > staticSize) {
    decNumber *buffer = (decNumber *)malloc(needbytes);
    if (buffer == NULL) {
      *status |= DEC_Insufficient_storage;
    }
    return buffer;
  }
  return NULL;
}

static void setupNumber10(decNumber *w) {
  decNumberZero(w);
  #if DECDPUN==1
  w->lsu[1] = 1;
  w->lsu[0] = 0;
  #else
  w->lsu[0] = 10;
  #endif
  w->digits = 2;
} /* decNumberLog10 */

/* ------------------------------------------------------------------ */
/* decNumberMax -- compare two Numbers and return the maximum	      */
/*								      */
/*   This computes C = A ? B, returning the maximum by 754 rules      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X?X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberMax(decNumber *res, const decNumber *lhs,
                         const decNumber *rhs, decContext *set) {
    if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
        return NULL;
    }
    
    uInt status = 0;
    decCompareOp(res, lhs, rhs, set, COMPMAX, &status);
    
    if (status != 0) {
        decStatus(res, status, set);
    }
    
    #if DECCHECK
    decCheckInexact(res, set);
    #endif
    
    return res;
} /* decNumberMax */

/* ------------------------------------------------------------------ */
/* decNumberMaxMag -- compare and return the maximum by magnitude     */
/*								      */
/*   This computes C = A ? B, returning the maximum by 754 rules      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X?X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberMaxMag(decNumber *res, const decNumber *lhs,
                         const decNumber *rhs, decContext *set) {
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
    return res;
  }
  
  uInt status = 0;
  decCompareOp(res, lhs, rhs, set, COMPMAXMAG, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  
  return res;
} /* decNumberMaxMag */

/* ------------------------------------------------------------------ */
/* decNumberMin -- compare two Numbers and return the minimum	      */
/*								      */
/*   This computes C = A ? B, returning the minimum by 754 rules      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X?X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberMin(decNumber *res, const decNumber *lhs,
			 const decNumber *rhs, decContext *set) {
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
    return NULL;
  }
  
  uInt status = 0;
  decCompareOp(res, lhs, rhs, set, COMPMIN, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  
  return res;
} /* decNumberMin */

/* ------------------------------------------------------------------ */
/* decNumberMinMag -- compare and return the minimum by magnitude     */
/*								      */
/*   This computes C = A ? B, returning the minimum by 754 rules      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X?X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber* decNumberMinMag(decNumber *res, const decNumber *lhs,
                           const decNumber *rhs, decContext *set) {
    if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
        return res;
    }
    
    uInt status = 0;
    decCompareOp(res, lhs, rhs, set, COMPMINMAG, &status);
    
    if (status != 0) {
        decStatus(res, status, set);
    }
    
    #if DECCHECK
    decCheckInexact(res, set);
    #endif
    
    return res;
} /* decNumberMinMag */

/* ------------------------------------------------------------------ */
/* decNumberMinus -- prefix minus operator			      */
/*								      */
/*   This computes C = 0 - A					      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context 					      */
/*								      */
/* See also decNumberCopyNegate for a quiet bitwise version of this.  */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
/* Simply use AddOp for the subtract, which will do the necessary.    */
/* ------------------------------------------------------------------ */
decNumber * decNumberMinus(decNumber *res, const decNumber *rhs,
                           decContext *set) {
  decNumber dzero;
  uInt status = 0;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  decNumberZero(&dzero);
  dzero.exponent = rhs->exponent;
  decAddOp(res, &dzero, rhs, set, DECNEG, &status);
  if (status != 0) decStatus(res, status, set);
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  return res;
} /* decNumberMinus */

/* ------------------------------------------------------------------ */
/* decNumberNextMinus -- next towards -Infinity 		      */
/*								      */
/*   This computes C = A - infinitesimal, rounded towards -Infinity   */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context 					      */
/*								      */
/* This is a generalization of 754 NextDown.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberNextMinus(decNumber *res, const decNumber *rhs,
                               decContext *set) {
  decNumber dtiny;
  decContext workset;
  uInt status = 0;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  if ((rhs->bits & (DECINF | DECNEG)) == DECINF) {
    decSetMaxValue(res, set);
    return res;
  }

  decNumberZero(&dtiny);
  dtiny.lsu[0] = 1;
  dtiny.exponent = DEC_MIN_EMIN - 1;
  
  workset = *set;
  workset.round = DEC_ROUND_FLOOR;
  
  decAddOp(res, rhs, &dtiny, &workset, DECNEG, &status);
  
  status &= (DEC_Invalid_operation | DEC_sNaN);
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  return res;
} /* decNumberNextMinus */

/* ------------------------------------------------------------------ */
/* decNumberNextPlus -- next towards +Infinity			      */
/*								      */
/*   This computes C = A + infinitesimal, rounded towards +Infinity   */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context 					      */
/*								      */
/* This is a generalization of 754 NextUp.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberNextPlus(decNumber *res, const decNumber *rhs,
                              decContext *set) {
  decNumber dtiny;
  decContext workset = *set;
  uInt status = 0;
  
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  if ((rhs->bits & (DECINF | DECNEG)) == (DECINF | DECNEG)) {
    decSetMaxValue(res, set);
    res->bits = DECNEG;
    return res;
  }
  
  decNumberZero(&dtiny);
  dtiny.lsu[0] = 1;
  dtiny.exponent = DEC_MIN_EMIN - 1;
  workset.round = DEC_ROUND_CEILING;
  
  decAddOp(res, rhs, &dtiny, &workset, 0, &status);
  
  status &= DEC_Invalid_operation | DEC_sNaN;
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  return res;
} /* decNumberNextPlus */

/* ------------------------------------------------------------------ */
/* decNumberNextToward -- next towards rhs			      */
/*								      */
/*   This computes C = A +/- infinitesimal, rounded towards	      */
/*   +/-Infinity in the direction of B, as per 754-1985 nextafter     */
/*   modified during revision but dropped from 754-2008.	      */
/*								      */
/*   res is C, the result.  C may be A or B.			      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* This is a generalization of 754-1985 NextAfter.		      */
/* ------------------------------------------------------------------ */
decNumber * decNumberNextToward(decNumber *res, const decNumber *lhs,
                                const decNumber *rhs, decContext *set) {
  decNumber dtiny;
  decContext workset = *set;
  Int result;
  uInt status = 0;
  
  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  if (decNumberIsNaN(lhs) || decNumberIsNaN(rhs)) {
    decNaNs(res, lhs, rhs, set, &status);
    if (status != 0) decStatus(res, status, set);
    return res;
  }

  result = decCompare(lhs, rhs, 0);
  if (result == BADINT) {
    status |= DEC_Insufficient_storage;
    decStatus(res, status, set);
    return res;
  }

  if (result == 0) {
    decNumberCopySign(res, lhs, rhs);
    return res;
  }

  if (result < 0) {
    if ((lhs->bits & (DECINF | DECNEG)) == (DECINF | DECNEG)) {
      decSetMaxValue(res, set);
      res->bits = DECNEG;
      return res;
    }
    workset.round = DEC_ROUND_CEILING;
    decNumberZero(&dtiny);
    dtiny.lsu[0] = 1;
    dtiny.exponent = DEC_MIN_EMIN - 1;
    decAddOp(res, lhs, &dtiny, &workset, 0, &status);
  } else {
    if ((lhs->bits & (DECINF | DECNEG)) == DECINF) {
      decSetMaxValue(res, set);
      return res;
    }
    workset.round = DEC_ROUND_FLOOR;
    decNumberZero(&dtiny);
    dtiny.lsu[0] = 1;
    dtiny.exponent = DEC_MIN_EMIN - 1;
    decAddOp(res, lhs, &dtiny, &workset, DECNEG, &status);
  }

  if (decNumberIsNormal(res, set)) {
    status = 0;
  }

  if (status != 0) {
    decStatus(res, status, set);
  }
  
  return res;
} /* decNumberNextToward */

/* ------------------------------------------------------------------ */
/* decNumberOr -- OR two Numbers, digitwise			      */
/*								      */
/*   This computes C = A | B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X|X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context (used for result length and error report)     */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Logical function restrictions apply (see above); a NaN is	      */
/* returned with Invalid_operation if a restriction is violated.      */
/* ------------------------------------------------------------------ */
decNumber * decNumberOr(decNumber *res, const decNumber *lhs,
			const decNumber *rhs, decContext *set) {
  const Unit *ua, *ub;
  const Unit *msua, *msub;
  Unit *uc, *msuc;
  Int msudigs;
  
  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  if (lhs->exponent != 0 || decNumberIsSpecial(lhs) || decNumberIsNegative(lhs) ||
      rhs->exponent != 0 || decNumberIsSpecial(rhs) || decNumberIsNegative(rhs)) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  ua = lhs->lsu;
  ub = rhs->lsu;
  uc = res->lsu;
  msua = ua + D2U(lhs->digits) - 1;
  msub = ub + D2U(rhs->digits) - 1;
  msuc = uc + D2U(set->digits) - 1;
  msudigs = MSUDIGITS(set->digits);

  for (; uc <= msuc; ua++, ub++, uc++) {
    Unit a = (ua > msua) ? 0 : *ua;
    Unit b = (ub > msub) ? 0 : *ub;
    *uc = 0;

    if ((a | b) == 0) continue;

    for (Int i = 0; i < DECDPUN; i++) {
      Int digit_a = a % 10;
      Int digit_b = b % 10;
      Int combined = digit_a | digit_b;

      if (combined > 1) {
        decStatus(res, DEC_Invalid_operation, set);
        return res;
      }

      if (combined == 1) {
        *uc += (Unit)powers[i];
      }

      a /= 10;
      b /= 10;

      if (uc == msuc && i == msudigs - 1) break;
    }
  }

  res->digits = decGetDigits(res->lsu, uc - res->lsu);
  res->exponent = 0;
  res->bits = 0;
  return res;
} /* decNumberOr */

/* ------------------------------------------------------------------ */
/* decNumberPlus -- prefix plus operator			      */
/*								      */
/*   This computes C = 0 + A					      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context 					      */
/*								      */
/* See also decNumberCopy for a quiet bitwise version of this.	      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
/* This simply uses AddOp; Add will take fast path after preparing A. */
/* Performance is a concern here, as this routine is often used to    */
/* check operands and apply rounding and overflow/underflow testing.  */
/* ------------------------------------------------------------------ */
decNumber * decNumberPlus(decNumber *res, const decNumber *rhs,
                          decContext *set) {
  decNumber dzero;
  uInt status = 0;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  decNumberZero(&dzero);
  dzero.exponent = rhs->exponent;
  decAddOp(res, &dzero, rhs, set, 0, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  
  return res;
} /* decNumberPlus */

/* ------------------------------------------------------------------ */
/* decNumberMultiply -- multiply two Numbers			      */
/*								      */
/*   This computes C = A x B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X+X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberMultiply(decNumber *res, const decNumber *lhs,
                              const decNumber *rhs, decContext *set) {
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
    return res;
  }
  
  uInt status = 0;
  decMultiplyOp(res, lhs, rhs, set, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  
  return res;
} /* decNumberMultiply */

/* ------------------------------------------------------------------ */
/* decNumberPower -- raise a number to a power			      */
/*								      */
/*   This computes C = A ** B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X**X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Mathematical function restrictions apply (see above); a NaN is     */
/* returned with Invalid_operation if a restriction is violated.      */
/*								      */
/* However, if 1999999997<=B<=999999999 and B is an integer then the  */
/* restrictions on A and the context are relaxed to the usual bounds, */
/* for compatibility with the earlier (integer power only) version    */
/* of this function.						      */
/*								      */
/* When B is an integer, the result may be exact, even if rounded.    */
/*								      */
/* The final result is rounded according to the context; it will      */
/* almost always be correctly rounded, but may be up to 1 ulp in      */
/* error in rare cases. 					      */
/* ------------------------------------------------------------------ */
decNumber * decNumberPower(decNumber *res, const decNumber *lhs,
                           const decNumber *rhs, decContext *set) {
  #if DECSUBSET
  decNumber *alloclhs = NULL;
  decNumber *allocrhs = NULL;
  #endif
  decNumber *allocdac = NULL;
  decNumber *allocinv = NULL;
  Int reqdigits = set->digits;
  Int n;
  Flag rhsint = 0;
  Flag useint = 0;
  Flag isoddint = 0;
  Int i;
  #if DECSUBSET
  Int dropped;
  #endif
  uInt needbytes;
  Flag seenbit;
  Int residue = 0;
  uInt status = 0;
  uByte bits = 0;
  decContext aset;
  decNumber dnOne;
  decNumber dacbuff[D2N(DECBUFFER+9)];
  decNumber *dac = dacbuff;
  decNumber invbuff[D2N(DECBUFFER+9)];

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  #if DECSUBSET
  if (!set->extended) {
    if (lhs->digits > reqdigits) {
      alloclhs = decRoundOperand(lhs, set, &status);
      if (alloclhs == NULL) goto cleanup;
      lhs = alloclhs;
    }
    if (rhs->digits > reqdigits) {
      allocrhs = decRoundOperand(rhs, set, &status);
      if (allocrhs == NULL) goto cleanup;
      rhs = allocrhs;
    }
  }
  #endif

  if (SPECIALARGS) {
    if (decNumberIsNaN(lhs) || decNumberIsNaN(rhs)) {
      decNaNs(res, lhs, rhs, set, &status);
      goto cleanup;
    }
    if (decNumberIsInfinite(rhs)) {
      if (decNumberIsNegative(lhs) && !decNumberIsZero(lhs)) {
        status |= DEC_Invalid_operation;
      } else {
        decNumberZero(&dnOne);
        dnOne.lsu[0] = 1;
        decNumberCompare(dac, lhs, &dnOne, set);
        decNumberZero(res);
        if (decNumberIsNegative(dac)) {
          if (rhs->bits & DECNEG) res->bits |= DECINF;
        } else if (dac->lsu[0] == 0) {
          Int shift = set->digits - 1;
          *res->lsu = 1;
          res->digits = decShiftToMost(res->lsu, 1, shift);
          res->exponent = -shift;
          status |= DEC_Inexact | DEC_Rounded;
        } else {
          if (!(rhs->bits & DECNEG)) res->bits |= DECINF;
        }
      }
      goto cleanup;
    }
  }

  n = decGetInt(rhs);
  if (n != BADINT) {
    rhsint = 1;
    isoddint = (Flag)(n & 1);
    if (n != BIGEVEN && n != BIGODD) useint = 1;
  }

  if (decNumberIsNegative(lhs) && isoddint) bits = DECNEG;

  if (decNumberIsInfinite(lhs)) {
    decNumberZero(res);
    if (n == 0) {
      *res->lsu = 1;
    } else {
      if (!rhsint && decNumberIsNegative(lhs)) {
        status |= DEC_Invalid_operation;
        goto cleanup;
      }
      if (!(rhs->bits & DECNEG)) bits |= DECINF;
      res->bits = bits;
    }
    goto cleanup;
  }

  if (decNumberIsZero(lhs)) {
    if (n == 0) {
      #if DECSUBSET
      if (!set->extended) {
        decNumberZero(res);
        *res->lsu = 1;
        goto cleanup;
      }
      #endif
      status |= DEC_Invalid_operation;
    } else {
      if (rhs->bits & DECNEG) {
        #if DECSUBSET
        if (!set->extended) {
          status |= DEC_Invalid_operation;
          goto cleanup;
        }
        #endif
        bits |= DECINF;
      }
      decNumberZero(res);
      res->bits = bits;
    }
    goto cleanup;
  }

  if (!useint) {
    if (decNumberIsNegative(lhs)) {
      status |= DEC_Invalid_operation;
      goto cleanup;
    }
    if (decCheckMath(lhs, set, &status) || decCheckMath(rhs, set, &status)) {
      goto cleanup;
    }
    decContextDefault(&aset, DEC_INIT_DECIMAL64);
    aset.emax = DEC_MAX_MATH;
    aset.emin = -DEC_MAX_MATH;
    aset.clamp = 0;
    aset.digits = MAXI(lhs->digits, set->digits) + 10;
  } else {
    if (n == 0) {
      decNumberZero(res);
      *res->lsu = 1;
      goto cleanup;
    }
    if (n < 0) n = -n;
    aset = *set;
    aset.round = DEC_ROUND_HALF_EVEN;
    aset.digits = reqdigits + (rhs->digits + rhs->exponent) + 2;
    #if DECSUBSET
    if (!set->extended) aset.digits--;
    #endif
    if (aset.digits > DECNUMMAXP) {
      status |= DEC_Invalid_operation;
      goto cleanup;
    }
  }

  needbytes = sizeof(decNumber) + (D2U(aset.digits) - 1) * sizeof(Unit);
  if (needbytes > sizeof(dacbuff)) {
    allocdac = (decNumber *)malloc(needbytes);
    if (allocdac == NULL) {
      status |= DEC_Insufficient_storage;
      goto cleanup;
    }
    dac = allocdac;
  }

  if (!useint) {
    decLnOp(dac, lhs, &aset, &status);
    if (ISZERO(dac)) {
      *dac->lsu = 1;
      if (!rhsint) {
        Int shift = set->digits - 1;
        dac->digits = decShiftToMost(dac->lsu, 1, shift);
        dac->exponent = -shift;
        status |= DEC_Inexact | DEC_Rounded;
      }
    } else {
      decMultiplyOp(dac, dac, rhs, &aset, &status);
      decExpOp(dac, dac, &aset, &status);
    }
  } else {
    decNumberZero(dac);
    *dac->lsu = 1;

    if (decNumberIsNegative(rhs)) {
      decNumber *inv = invbuff;
      decNumberCopy(&dnOne, dac);
      #if DECSUBSET
      if (set->extended) {
      #endif
        decDivideOp(dac, &dnOne, lhs, &aset, DIVIDE, &status);
        if (needbytes > sizeof(invbuff)) {
          allocinv = (decNumber *)malloc(needbytes);
          if (allocinv == NULL) {
            status |= DEC_Insufficient_storage;
            goto cleanup;
          }
          inv = allocinv;
        }
        decNumberCopy(inv, dac);
        decNumberCopy(dac, &dnOne);
        lhs = inv;
      #if DECSUBSET
      }
      #endif
    }

    seenbit = 0;
    for (i = 1; i <= 31; i++) {
      if (status & (DEC_Overflow | DEC_Underflow)) {
        if ((status & DEC_Overflow) || ISZERO(dac)) break;
      }
      n = n << 1;
      if (n < 0) {
        seenbit = 1;
        decMultiplyOp(dac, dac, lhs, &aset, &status);
      }
      if (i == 31) break;
      if (!seenbit) continue;
      decMultiplyOp(dac, dac, dac, &aset, &status);
    }

    if (status & (DEC_Overflow | DEC_Underflow)) {
      #if DECSUBSET
      if (!set->extended && decNumberIsNegative(rhs)) {
        if (status & DEC_Overflow) {
          status ^= DEC_Overflow | DEC_Underflow | DEC_Subnormal;
        } else {
          status &= ~(DEC_Underflow | DEC_Subnormal);
          status |= DEC_Overflow;
        }
      }
      #endif
      dac->bits = (dac->bits & ~DECNEG) | bits;
      decFinalize(dac, set, &residue, &status);
      decNumberCopy(res, dac);
      goto cleanup;
    }

    #if DECSUBSET
    if (!set->extended && decNumberIsNegative(rhs)) {
      decDivideOp(dac, &dnOne, dac, &aset, DIVIDE, &status);
    }
    #endif
  }

  decCopyFit(res, dac, set, &residue, &status);
  decFinish(res, set, &residue, &status);
  #if DECSUBSET
  if (!set->extended) decTrim(res, set, 0, 1, &dropped);
  #endif

cleanup:
  free(allocdac);
  free(allocinv);
  #if DECSUBSET
  free(alloclhs);
  free(allocrhs);
  #endif
  if (status != 0) decStatus(res, status, set);
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  return res;
} /* decNumberPower */

/* ------------------------------------------------------------------ */
/* decNumberQuantize -- force exponent to requested value	      */
/*								      */
/*   This computes C = op(A, B), where op adjusts the coefficient     */
/*   of C (by rounding or shifting) such that the exponent (-scale)   */
/*   of C has exponent of B.  The numerical value of C will equal A,  */
/*   except for the effects of any rounding that occurred.	      */
/*								      */
/*   res is C, the result.  C may be A or B			      */
/*   lhs is A, the number to adjust				      */
/*   rhs is B, the number with exponent to match		      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Unless there is an error or the result is infinite, the exponent   */
/* after the operation is guaranteed to be equal to that of B.	      */
/* ------------------------------------------------------------------ */
decNumber * decNumberQuantize(decNumber *res, const decNumber *lhs,
                              const decNumber *rhs, decContext *set) {
  uInt status = 0;
  decQuantizeOp(res, lhs, rhs, set, 1, &status);
  if (status != 0) {
    decStatus(res, status, set);
  }
  return res;
} /* decNumberQuantize */

/* ------------------------------------------------------------------ */
/* decNumberReduce -- remove trailing zeros			      */
/*								      */
/*   This computes C = 0 + A, and normalizes the result 	      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
/* Previously known as Normalize */
decNumber * decNumberNormalize(decNumber *res, const decNumber *rhs,
                               decContext *set) {
    if (res == NULL || rhs == NULL || set == NULL) {
        return NULL;
    }
    return decNumberReduce(res, rhs, set);
} /* decNumberNormalize */

decNumber * decNumberReduce(decNumber *res, const decNumber *rhs,
                           decContext *set) {
  #if DECSUBSET
  decNumber *allocrhs = NULL;
  #endif
  uInt status = 0;
  Int residue = 0;
  Int dropped = 0;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  #if DECSUBSET
  if (!set->extended && rhs->digits > set->digits) {
    allocrhs = decRoundOperand(rhs, set, &status);
    if (allocrhs == NULL) {
      decStatus(res, status, set);
      return res;
    }
    rhs = allocrhs;
  }
  #endif

  if (decNumberIsNaN(rhs)) {
    decNaNs(res, rhs, NULL, set, &status);
  } else {
    decCopyFit(res, rhs, set, &residue, &status);
    decFinish(res, set, &residue, &status);
    decTrim(res, set, 1, 0, &dropped);
  }

  #if DECSUBSET
  free(allocrhs);
  #endif
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  return res;
} /* decNumberReduce */

/* ------------------------------------------------------------------ */
/* decNumberRescale -- force exponent to requested value	      */
/*								      */
/*   This computes C = op(A, B), where op adjusts the coefficient     */
/*   of C (by rounding or shifting) such that the exponent (-scale)   */
/*   of C has the value B.  The numerical value of C will equal A,    */
/*   except for the effects of any rounding that occurred.	      */
/*								      */
/*   res is C, the result.  C may be A or B			      */
/*   lhs is A, the number to adjust				      */
/*   rhs is B, the requested exponent				      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Unless there is an error or the result is infinite, the exponent   */
/* after the operation is guaranteed to be equal to B.		      */
/* ------------------------------------------------------------------ */
decNumber * decNumberRescale(decNumber *res, const decNumber *lhs,
                           const decNumber *rhs, decContext *set) {
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
    return res;
  }
  
  uInt status = 0;
  decQuantizeOp(res, lhs, rhs, set, 0, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  return res;
} /* decNumberRescale */

/* ------------------------------------------------------------------ */
/* decNumberRemainder -- divide and return remainder		      */
/*								      */
/*   This computes C = A % B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X%X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberRemainder(decNumber *res, const decNumber *lhs,
                               const decNumber *rhs, decContext *set) {
    if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
        return res;
    }
    
    uInt status = 0;
    decDivideOp(res, lhs, rhs, set, REMAINDER, &status);
    
    if (status != 0) {
        decStatus(res, status, set);
    }
    
    #if DECCHECK
    decCheckInexact(res, set);
    #endif
    
    return res;
} /* decNumberRemainder */

/* ------------------------------------------------------------------ */
/* decNumberRemainderNear -- divide and return remainder from nearest */
/*								      */
/*   This computes C = A % B, where % is the IEEE remainder operator  */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X%X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberRemainderNear(decNumber *res, const decNumber *lhs,
                                     const decNumber *rhs, decContext *set) {
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
    return res;
  }
  
  uInt status = 0;
  decDivideOp(res, lhs, rhs, set, REMNEAR, &status);
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  
  return res;
} /* decNumberRemainderNear */

/* ------------------------------------------------------------------ */
/* decNumberRotate -- rotate the coefficient of a Number left/right   */
/*								      */
/*   This computes C = A rot B	(in base ten and rotating set->digits */
/*   digits).							      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=XrotX)       */
/*   lhs is A							      */
/*   rhs is B, the number of digits to rotate (-ve to right)	      */
/*   set is the context 					      */
/*								      */
/* The digits of the coefficient of A are rotated to the left (if B   */
/* is positive) or to the right (if B is negative) without adjusting  */
/* the exponent or the sign of A.  If lhs->digits is less than	      */
/* set->digits the coefficient is padded with zeros on the left       */
/* before the rotate.  Any leading zeros in the result are removed    */
/* as usual.							      */
/*								      */
/* B must be an integer (q=0) and in the range -set->digits through   */
/* +set->digits.						      */
/* C must have space for set->digits digits.			      */
/* NaNs are propagated as usual.  Infinities are unaffected (but      */
/* B must be valid).  No status is set unless B is invalid or an      */
/* operand is an sNaN.						      */
/* ------------------------------------------------------------------ */
decNumber * decNumberRotate(decNumber *res, const decNumber *lhs,
                            const decNumber *rhs, decContext *set) {
  uInt status = 0;
  Int rotate;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  if (decNumberIsNaN(lhs) || decNumberIsNaN(rhs)) {
    decNaNs(res, lhs, rhs, set, &status);
    decStatus(res, status, set);
    return res;
  }

  if (decNumberIsInfinite(rhs) || rhs->exponent != 0) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  rotate = decGetInt(rhs);
  if (rotate == BADINT || rotate == BIGODD || rotate == BIGEVEN) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  Int absRotate = (rotate < 0) ? -rotate : rotate;
  if (absRotate > set->digits) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  decNumberCopy(res, lhs);

  if (rotate < 0) {
    rotate = set->digits + rotate;
  }

  if (rotate == 0 || rotate == set->digits || decNumberIsInfinite(res)) {
    return res;
  }

  uInt msudigits = MSUDIGITS(set->digits);
  Unit *msu = res->lsu + D2U(res->digits) - 1;
  Unit *msumax = res->lsu + D2U(set->digits) - 1;

  for (Unit *p = msu + 1; p <= msumax; p++) {
    *p = 0;
  }

  res->digits = set->digits;

  rotate = set->digits - rotate;
  uInt units = rotate / DECDPUN;
  uInt shift = rotate % DECDPUN;

  if (shift > 0) {
    uInt save = res->lsu[0] % powers[shift];
    decShiftToLeast(res->lsu, D2U(res->digits), shift);
    
    if (shift > msudigits) {
      uInt rem = save % powers[shift - msudigits];
      *msumax = (Unit)(save / powers[shift - msudigits]);
      *(msumax - 1) = *(msumax - 1) + 
                      (Unit)(rem * powers[DECDPUN - (shift - msudigits)]);
    } else {
      *msumax = *msumax + (Unit)(save * powers[msudigits - shift]);
    }
  }

  if (units > 0) {
    shift = DECDPUN - msudigits;
    
    if (shift > 0) {
      uInt save = res->lsu[0] % powers[shift];
      decShiftToLeast(res->lsu, units, shift);
      *msumax = *msumax + (Unit)(save * powers[msudigits]);
    }

    decReverse(res->lsu + units, msumax);
    decReverse(res->lsu, res->lsu + units - 1);
    decReverse(res->lsu, msumax);
  }

  res->digits = decGetDigits(res->lsu, msumax - res->lsu + 1);

  if (status != 0) {
    decStatus(res, status, set);
  }

  return res;
} /* decNumberRotate */

/* ------------------------------------------------------------------ */
/* decNumberSameQuantum -- test for equal exponents		      */
/*								      */
/*   res is the result number, which will contain either 0 or 1       */
/*   lhs is a number to test					      */
/*   rhs is the second (usually a pattern)			      */
/*								      */
/* No errors are possible and no context is needed.		      */
/* ------------------------------------------------------------------ */
decNumber * decNumberSameQuantum(decNumber *res, const decNumber *lhs,
                                  const decNumber *rhs) {
  Unit ret = 0;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, DECUNCONT)) return res;
  #endif

  if (SPECIALARGS) {
    if (decNumberIsNaN(lhs) && decNumberIsNaN(rhs)) {
      ret = 1;
    } else if (decNumberIsInfinite(lhs) && decNumberIsInfinite(rhs)) {
      ret = 1;
    }
  } else if (lhs->exponent == rhs->exponent) {
    ret = 1;
  }

  decNumberZero(res);
  *res->lsu = ret;
  return res;
} /* decNumberSameQuantum */

/* ------------------------------------------------------------------ */
/* decNumberScaleB -- multiply by a power of 10 		      */
/*								      */
/* This computes C = A x 10**B where B is an integer (q=0) with       */
/* maximum magnitude 2*(emax+digits)				      */
/*								      */
/*   res is C, the result.  C may be A or B			      */
/*   lhs is A, the number to adjust				      */
/*   rhs is B, the requested power of ten to use		      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* The result may underflow or overflow.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberScaleB(decNumber *res, const decNumber *lhs,
                            const decNumber *rhs, decContext *set) {
  Int reqexp;
  uInt status = 0;
  Int residue;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  if (decNumberIsNaN(lhs) || decNumberIsNaN(rhs)) {
    decNaNs(res, lhs, rhs, set, &status);
    decStatus(res, status, set);
    return res;
  }

  if (decNumberIsInfinite(rhs) || rhs->exponent != 0) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  reqexp = decGetInt(rhs);
  
  if (reqexp == BADINT || reqexp == BIGODD || reqexp == BIGEVEN) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }
  
  Int max_allowed = 2 * (set->digits + set->emax);
  if (abs(reqexp) > max_allowed) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  decNumberCopy(res, lhs);
  
  if (!decNumberIsInfinite(res)) {
    res->exponent += reqexp;
    residue = 0;
    decFinalize(res, set, &residue, &status);
    if (status != 0) {
      decStatus(res, status, set);
    }
  }

  return res;
} /* decNumberScaleB */

/* ------------------------------------------------------------------ */
/* decNumberShift -- shift the coefficient of a Number left or right  */
/*								      */
/*   This computes C = A << B or C = A >> -B  (in base ten).	      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X<<X)	      */
/*   lhs is A							      */
/*   rhs is B, the number of digits to shift (-ve to right)	      */
/*   set is the context 					      */
/*								      */
/* The digits of the coefficient of A are shifted to the left (if B   */
/* is positive) or to the right (if B is negative) without adjusting  */
/* the exponent or the sign of A.				      */
/*								      */
/* B must be an integer (q=0) and in the range -set->digits through   */
/* +set->digits.						      */
/* C must have space for set->digits digits.			      */
/* NaNs are propagated as usual.  Infinities are unaffected (but      */
/* B must be valid).  No status is set unless B is invalid or an      */
/* operand is an sNaN.						      */
/* ------------------------------------------------------------------ */
decNumber * decNumberShift(decNumber *res, const decNumber *lhs,
                           const decNumber *rhs, decContext *set) {
    uInt status = 0;
    Int shift;

    #if DECCHECK
    if (decCheckOperands(res, lhs, rhs, set)) return res;
    #endif

    if (decNumberIsNaN(lhs) || decNumberIsNaN(rhs)) {
        decNaNs(res, lhs, rhs, set, &status);
        decStatus(res, status, set);
        return res;
    }

    if (decNumberIsInfinite(rhs) || rhs->exponent != 0) {
        status = DEC_Invalid_operation;
        decStatus(res, status, set);
        return res;
    }

    shift = decGetInt(rhs);
    
    if (shift == BADINT || shift == BIGODD || shift == BIGEVEN) {
        status = DEC_Invalid_operation;
        decStatus(res, status, set);
        return res;
    }

    Int absShift = (shift < 0) ? -shift : shift;
    if (absShift > set->digits) {
        status = DEC_Invalid_operation;
        decStatus(res, status, set);
        return res;
    }

    decNumberCopy(res, lhs);

    if (shift == 0 || decNumberIsInfinite(res)) {
        return res;
    }

    if (shift > 0) {
        if (shift == set->digits) {
            *res->lsu = 0;
            res->digits = 1;
        } else {
            if (res->digits + shift > set->digits) {
                decDecap(res, res->digits + shift - set->digits);
            }
            if (res->digits > 1 || *res->lsu) {
                res->digits = decShiftToMost(res->lsu, res->digits, shift);
            }
        }
    } else {
        Int negShift = -shift;
        if (negShift >= res->digits) {
            *res->lsu = 0;
            res->digits = 1;
        } else {
            decShiftToLeast(res->lsu, D2U(res->digits), negShift);
            res->digits -= negShift;
        }
    }

    return res;
} /* decNumberShift */

/* ------------------------------------------------------------------ */
/* decNumberSquareRoot -- square root operator			      */
/*								      */
/*   This computes C = squareroot(A)				      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context; note that rounding mode has no effect	      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
/* This uses the following varying-precision algorithm in:	      */
/*								      */
/*   Properly Rounded Variable Precision Square Root, T. E. Hull and  */
/*   A. Abrham, ACM Transactions on Mathematical Software, Vol 11 #3, */
/*   pp229-237, ACM, September 1985.				      */
/*								      */
/* The square-root is calculated using Newton's method, after which   */
/* a check is made to ensure the result is correctly rounded.	      */
/*								      */
/* % [Reformatted original Numerical Turing source code follows.]     */
/* function sqrt(x : real) : real				      */
/* % sqrt(x) returns the properly rounded approximation to the square */
/* % root of x, in the precision of the calling environment, or it    */
/* % fails if x < 0.						      */
/* % t e hull and a abrham, august, 1984			      */
/* if x <= 0 then						      */
/*   if x < 0 then						      */
/*     assert false						      */
/*   else							      */
/*     result 0 						      */
/*   end if							      */
/* end if							      */
/* var f := setexp(x, 0)  % fraction part of x	 [0.1 <= x < 1]       */
/* var e := getexp(x)	  % exponent part of x			      */
/* var approx : real						      */
/* if e mod 2 = 0  then 					      */
/*   approx := .259 + .819 * f	 % approx to root of f		      */
/* else 							      */
/*   f := f/l0			 % adjustments			      */
/*   e := e + 1 		 %   for odd			      */
/*   approx := .0819 + 2.59 * f  %   exponent			      */
/* end if							      */
/*								      */
/* var p:= 3							      */
/* const maxp := currentprecision + 2				      */
/* loop 							      */
/*   p := min(2*p - 2, maxp)	 % p = 4,6,10, . . . , maxp	      */
/*   precision p						      */
/*   approx := .5 * (approx + f/approx) 			      */
/*   exit when p = maxp 					      */
/* end loop							      */
/*								      */
/* % approx is now within 1 ulp of the properly rounded square root   */
/* % of f; to ensure proper rounding, compare squares of (approx -    */
/* % l/2 ulp) and (approx + l/2 ulp) with f.			      */
/* p := currentprecision					      */
/* begin							      */
/*   precision p + 2						      */
/*   const approxsubhalf := approx - setexp(.5, -p)		      */
/*   if mulru(approxsubhalf, approxsubhalf) > f then		      */
/*     approx := approx - setexp(.l, -p + 1)			      */
/*   else							      */
/*     const approxaddhalf := approx + setexp(.5, -p)		      */
/*     if mulrd(approxaddhalf, approxaddhalf) < f then		      */
/*	 approx := approx + setexp(.l, -p + 1)			      */
/*     end if							      */
/*   end if							      */
/* end								      */
/* result setexp(approx, e div 2)  % fix exponent		      */
/* end sqrt							      */
/* ------------------------------------------------------------------ */
decNumber * decNumberSquareRoot(decNumber *res, const decNumber *rhs,
                                   decContext *set) {
    decContext workset, approxset;
    decNumber dzero;
    Int maxp, workp, residue = 0;
    uInt status = 0, ignore = 0, rstatus;
    Int exp, ideal, needbytes, dropped;
    
    #if DECSUBSET
    decNumber *allocrhs = NULL;
    #endif
    
    decNumber buff[D2N(DECBUFFER+1)];
    decNumber bufa[D2N(DECBUFFER+2)];
    decNumber bufb[D2N(DECBUFFER+2)];
    decNumber *allocbuff = NULL;
    decNumber *allocbufa = NULL;
    decNumber *allocbufb = NULL;
    decNumber *f = buff;
    decNumber *a = bufa;
    decNumber *b = bufb;
    decNumber buft[D2N(3)];
    decNumber *t = buft;
    
    #if DECCHECK
    if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
    #endif
    
    #if DECSUBSET
    if (!set->extended && rhs->digits > set->digits) {
        allocrhs = decRoundOperand(rhs, set, &status);
        if (allocrhs == NULL) goto cleanup;
        rhs = allocrhs;
    }
    #endif
    
    if (SPECIALARG) {
        if (decNumberIsInfinite(rhs)) {
            if (decNumberIsNegative(rhs)) {
                status |= DEC_Invalid_operation;
            } else {
                decNumberCopy(res, rhs);
            }
        } else {
            decNaNs(res, rhs, NULL, set, &status);
        }
        goto cleanup;
    }
    
    ideal = (rhs->exponent & ~1) / 2;
    
    if (ISZERO(rhs)) {
        decNumberCopy(res, rhs);
        res->exponent = ideal;
        decFinish(res, set, &residue, &status);
        goto cleanup;
    }
    
    if (decNumberIsNegative(rhs)) {
        status |= DEC_Invalid_operation;
        goto cleanup;
    }
    
    workp = MAXI(set->digits + 1, rhs->digits);
    workp = MAXI(workp, 7);
    maxp = workp + 2;
    
    needbytes = sizeof(decNumber) + (D2U(rhs->digits) - 1) * sizeof(Unit);
    if (needbytes > (Int)sizeof(buff)) {
        allocbuff = (decNumber *)malloc(needbytes);
        if (allocbuff == NULL) {
            status |= DEC_Insufficient_storage;
            goto cleanup;
        }
        f = allocbuff;
    }
    
    needbytes = sizeof(decNumber) + (D2U(maxp) - 1) * sizeof(Unit);
    if (needbytes > (Int)sizeof(bufa)) {
        allocbufa = (decNumber *)malloc(needbytes);
        allocbufb = (decNumber *)malloc(needbytes);
        if (allocbufa == NULL || allocbufb == NULL) {
            status |= DEC_Insufficient_storage;
            goto cleanup;
        }
        a = allocbufa;
        b = allocbufb;
    }
    
    decNumberCopy(f, rhs);
    exp = f->exponent + f->digits;
    f->exponent = -(f->digits);
    
    decContextDefault(&workset, DEC_INIT_DECIMAL64);
    workset.emax = DEC_MAX_EMAX;
    workset.emin = DEC_MIN_EMIN;
    workset.digits = workp;
    
    t->bits = 0;
    t->digits = 3;
    a->bits = 0;
    a->digits = 3;
    
    if ((exp & 1) == 0) {
        t->exponent = -3;
        a->exponent = -3;
        #if DECDPUN>=3
        t->lsu[0] = 259;
        a->lsu[0] = 819;
        #elif DECDPUN==2
        t->lsu[0] = 59; t->lsu[1] = 2;
        a->lsu[0] = 19; a->lsu[1] = 8;
        #else
        t->lsu[0] = 9; t->lsu[1] = 5; t->lsu[2] = 2;
        a->lsu[0] = 9; a->lsu[1] = 1; a->lsu[2] = 8;
        #endif
    } else {
        f->exponent--;
        exp++;
        t->exponent = -4;
        a->exponent = -2;
        #if DECDPUN>=3
        t->lsu[0] = 819;
        a->lsu[0] = 259;
        #elif DECDPUN==2
        t->lsu[0] = 19; t->lsu[1] = 8;
        a->lsu[0] = 59; a->lsu[1] = 2;
        #else
        t->lsu[0] = 9; t->lsu[1] = 1; t->lsu[2] = 8;
        a->lsu[0] = 9; a->lsu[1] = 5; a->lsu[2] = 2;
        #endif
    }
    
    decMultiplyOp(a, a, f, &workset, &ignore);
    decAddOp(a, a, t, &workset, 0, &ignore);
    
    decNumberZero(&dzero);
    decNumberZero(t);
    t->lsu[0] = 5;
    t->exponent = -1;
    
    workset.digits = 3;
    while (workset.digits < maxp) {
        workset.digits = MINI(workset.digits * 2 - 2, maxp);
        decDivideOp(b, f, a, &workset, DIVIDE, &ignore);
        decAddOp(b, b, a, &workset, 0, &ignore);
        decMultiplyOp(a, b, t, &workset, &ignore);
    }
    
    approxset = *set;
    approxset.round = DEC_ROUND_HALF_EVEN;
    a->exponent += exp / 2;
    rstatus = 0;
    residue = 0;
    decCopyFit(a, a, &approxset, &residue, &rstatus);
    decFinish(a, &approxset, &residue, &rstatus);
    
    if (rstatus & DEC_Overflow) {
        status = rstatus;
        decNumberCopy(res, a);
        goto cleanup;
    }
    
    status |= (rstatus & ~(DEC_Rounded | DEC_Inexact));
    
    a->exponent -= exp / 2;
    workset.digits--;
    t->exponent = -a->digits - 1;
    
    decAddOp(b, a, t, &workset, DECNEG, &ignore);
    workset.round = DEC_ROUND_UP;
    decMultiplyOp(b, b, b, &workset, &ignore);
    decCompareOp(b, f, b, &workset, COMPARE, &ignore);
    
    if (decNumberIsNegative(b)) {
        t->exponent++;
        t->lsu[0] = 1;
        decAddOp(a, a, t, &workset, DECNEG, &ignore);
        approxset.emin -= exp / 2;
        approxset.emax -= exp / 2;
        decAddOp(a, &dzero, a, &approxset, 0, &ignore);
    } else {
        decAddOp(b, a, t, &workset, 0, &ignore);
        workset.round = DEC_ROUND_DOWN;
        decMultiplyOp(b, b, b, &workset, &ignore);
        decCompareOp(b, b, f, &workset, COMPARE, &ignore);
        if (decNumberIsNegative(b)) {
            t->exponent++;
            t->lsu[0] = 1;
            decAddOp(a, a, t, &workset, 0, &ignore);
            approxset.emin -= exp / 2;
            approxset.emax -= exp / 2;
            decAddOp(a, &dzero, a, &approxset, 0, &ignore);
        }
    }
    
    a->exponent += exp / 2;
    
    decNumberCopy(b, a);
    decTrim(b, set, 1, 1, &dropped);
    
    if (b->digits * 2 - 1 > workp) {
        status |= DEC_Inexact | DEC_Rounded;
    } else {
        uInt mstatus = 0;
        decMultiplyOp(b, b, b, &workset, &mstatus);
        if (mstatus & DEC_Overflow) {
            status |= DEC_Inexact | DEC_Rounded;
        } else {
            decCompareOp(t, b, rhs, &workset, COMPARE, &mstatus);
            if (!ISZERO(t)) {
                status |= DEC_Inexact | DEC_Rounded;
            } else {
                Int todrop = ideal - a->exponent;
                if (todrop < 0) {
                    status |= DEC_Rounded;
                } else {
                    Int maxexp = set->emax - set->digits + 1;
                    Int maxdrop = maxexp - a->exponent;
                    if (todrop > maxdrop && set->clamp) {
                        todrop = maxdrop;
                        status |= DEC_Clamped;
                    }
                    if (dropped < todrop) {
                        todrop = dropped;
                        status |= DEC_Clamped;
                    }
                    if (todrop > 0) {
                        decShiftToLeast(a->lsu, D2U(a->digits), todrop);
                        a->exponent += todrop;
                        a->digits -= todrop;
                    }
                }
            }
        }
    }
    
    if (status & DEC_Underflow) {
        Int ae = rhs->exponent + rhs->digits - 1;
        #if DECEXTFLAG
        if (ae >= set->emin * 2) status &= ~(DEC_Subnormal | DEC_Underflow);
        #else
        if (ae >= set->emin * 2) status &= ~DEC_Underflow;
        #endif
        if (!(status & DEC_Inexact)) status &= ~DEC_Underflow;
    }
    
    decNumberCopy(res, a);
    
cleanup:
    free(allocbuff);
    free(allocbufa);
    free(allocbufb);
    #if DECSUBSET
    free(allocrhs);
    #endif
    
    if (status != 0) decStatus(res, status, set);
    
    #if DECCHECK
    decCheckInexact(res, set);
    #endif
    
    return res;
} /* decNumberSquareRoot */

/* ------------------------------------------------------------------ */
/* decNumberSubtract -- subtract two Numbers			      */
/*								      */
/*   This computes C = A - B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X-X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*								      */
/* C must have space for set->digits digits.			      */
/* ------------------------------------------------------------------ */
decNumber * decNumberSubtract(decNumber *res, const decNumber *lhs,
                              const decNumber *rhs, decContext *set) {
  uInt status = 0;

  decAddOp(res, lhs, rhs, set, DECNEG, &status);
  if (status != 0) {
    decStatus(res, status, set);
  }
  #if DECCHECK
  decCheckInexact(res, set);
  #endif
  return res;
} /* decNumberSubtract */

/* ------------------------------------------------------------------ */
/* decNumberToIntegralExact -- round-to-integral-value with InExact   */
/* decNumberToIntegralValue -- round-to-integral-value		      */
/*								      */
/*   res is the result						      */
/*   rhs is input number					      */
/*   set is the context 					      */
/*								      */
/* res must have space for any value of rhs.			      */
/*								      */
/* This implements the IEEE special operators and therefore treats    */
/* special values as valid.  For finite numbers it returns	      */
/* rescale(rhs, 0) if rhs->exponent is <0.			      */
/* Otherwise the result is rhs (so no error is possible, except for   */
/* sNaN).							      */
/*								      */
/* The context is used for rounding mode and status after sNaN, but   */
/* the digits setting is ignored.  The Exact version will signal      */
/* Inexact if the result differs numerically from rhs; the other      */
/* never signals Inexact.					      */
/* ------------------------------------------------------------------ */
decNumber * decNumberToIntegralExact(decNumber *res, const decNumber *rhs,
                                     decContext *set) {
  decNumber dn;
  decContext workset;
  uInt status = 0;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  if (SPECIALARG) {
    if (decNumberIsInfinite(rhs)) {
      decNumberCopy(res, rhs);
    } else {
      decNaNs(res, rhs, NULL, set, &status);
    }
    if (status != 0) {
      decStatus(res, status, set);
    }
    return res;
  }

  if (rhs->exponent >= 0) {
    return decNumberCopy(res, rhs);
  }

  workset = *set;
  workset.digits = rhs->digits;
  workset.traps = 0;
  
  decNumberZero(&dn);
  decNumberQuantize(res, rhs, &dn, &workset);
  status = workset.status;
  
  if (status != 0) {
    decStatus(res, status, set);
  }
  
  return res;
} /* decNumberToIntegralExact */

decNumber * decNumberToIntegralValue(decNumber *res, const decNumber *rhs,
                                     decContext *set) {
  if (res == NULL || rhs == NULL || set == NULL) {
    return res;
  }
  
  decContext workset = *set;
  workset.traps = 0;
  
  decNumberToIntegralExact(res, rhs, &workset);
  
  set->status |= (workset.status & DEC_Invalid_operation);
  
  return res;
} /* decNumberToIntegralValue */

/* ------------------------------------------------------------------ */
/* decNumberXor -- XOR two Numbers, digitwise			      */
/*								      */
/*   This computes C = A ^ B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X^X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context (used for result length and error report)     */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Logical function restrictions apply (see above); a NaN is	      */
/* returned with Invalid_operation if a restriction is violated.      */
/* ------------------------------------------------------------------ */
decNumber * decNumberXor(decNumber *res, const decNumber *lhs,
			 const decNumber *rhs, decContext *set) {
  const Unit *ua, *ub;
  const Unit *msua, *msub;
  Unit *uc, *msuc;
  Int msudigs;
  
  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  if (lhs->exponent != 0 || decNumberIsSpecial(lhs) || decNumberIsNegative(lhs) ||
      rhs->exponent != 0 || decNumberIsSpecial(rhs) || decNumberIsNegative(rhs)) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  ua = lhs->lsu;
  ub = rhs->lsu;
  uc = res->lsu;
  msua = ua + D2U(lhs->digits) - 1;
  msub = ub + D2U(rhs->digits) - 1;
  msuc = uc + D2U(set->digits) - 1;
  msudigs = MSUDIGITS(set->digits);

  for (; uc <= msuc; ua++, ub++, uc++) {
    Unit a = (ua > msua) ? 0 : *ua;
    Unit b = (ub > msub) ? 0 : *ub;
    *uc = 0;

    if (a | b) {
      for (Int i = 0; i < DECDPUN; i++) {
        Unit digit_a = a % 10;
        Unit digit_b = b % 10;
        
        if (digit_a > 1 || digit_b > 1) {
          decStatus(res, DEC_Invalid_operation, set);
          return res;
        }
        
        if ((digit_a ^ digit_b) & 1) {
          *uc = *uc + (Unit)powers[i];
        }
        
        a = a / 10;
        b = b / 10;
        
        if (uc == msuc && i == msudigs - 1) {
          break;
        }
      }
    }
  }

  res->digits = decGetDigits(res->lsu, uc - res->lsu);
  res->exponent = 0;
  res->bits = 0;
  return res;
} /* decNumberXor */


/* ================================================================== */
/* Utility routines						      */
/* ================================================================== */

/* ------------------------------------------------------------------ */
/* decNumberClass -- return the decClass of a decNumber 	      */
/*   dn -- the decNumber to test				      */
/*   set -- the context to use for Emin 			      */
/*   returns the decClass enum					      */
/* ------------------------------------------------------------------ */
enum decClass decNumberClass(const decNumber *dn, decContext *set) {
    if (decNumberIsSpecial(dn)) {
        if (decNumberIsQNaN(dn)) {
            return DEC_CLASS_QNAN;
        }
        if (decNumberIsSNaN(dn)) {
            return DEC_CLASS_SNAN;
        }
        return decNumberIsNegative(dn) ? DEC_CLASS_NEG_INF : DEC_CLASS_POS_INF;
    }
    
    if (decNumberIsNormal(dn, set)) {
        return decNumberIsNegative(dn) ? DEC_CLASS_NEG_NORMAL : DEC_CLASS_POS_NORMAL;
    }
    
    if (decNumberIsZero(dn)) {
        return decNumberIsNegative(dn) ? DEC_CLASS_NEG_ZERO : DEC_CLASS_POS_ZERO;
    }
    
    return decNumberIsNegative(dn) ? DEC_CLASS_NEG_SUBNORMAL : DEC_CLASS_POS_SUBNORMAL;
} /* decNumberClass */

/* ------------------------------------------------------------------ */
/* decNumberClassToString -- convert decClass to a string	      */
/*								      */
/*  eclass is a valid decClass					      */
/*  returns a constant string describing the class (max 13+1 chars)   */
/* ------------------------------------------------------------------ */
const char *decNumberClassToString(enum decClass eclass) {
    static const struct {
        enum decClass class;
        const char *string;
    } class_map[] = {
        {DEC_CLASS_POS_NORMAL,    DEC_ClassString_PN},
        {DEC_CLASS_NEG_NORMAL,    DEC_ClassString_NN},
        {DEC_CLASS_POS_ZERO,      DEC_ClassString_PZ},
        {DEC_CLASS_NEG_ZERO,      DEC_ClassString_NZ},
        {DEC_CLASS_POS_SUBNORMAL, DEC_ClassString_PS},
        {DEC_CLASS_NEG_SUBNORMAL, DEC_ClassString_NS},
        {DEC_CLASS_POS_INF,       DEC_ClassString_PI},
        {DEC_CLASS_NEG_INF,       DEC_ClassString_NI},
        {DEC_CLASS_QNAN,          DEC_ClassString_QN},
        {DEC_CLASS_SNAN,          DEC_ClassString_SN}
    };
    
    for (size_t i = 0; i < sizeof(class_map) / sizeof(class_map[0]); i++) {
        if (eclass == class_map[i].class) {
            return class_map[i].string;
        }
    }
    
    return DEC_ClassString_UN;
} /* decNumberClassToString */

/* ------------------------------------------------------------------ */
/* decNumberCopy -- copy a number				      */
/*								      */
/*   dest is the target decNumber				      */
/*   src  is the source decNumber				      */
/*   returns dest						      */
/*								      */
/* (dest==src is allowed and is a no-op)			      */
/* All fields are updated as required.	This is a utility operation,  */
/* so special values are unchanged and no error is possible.	      */
/* ------------------------------------------------------------------ */
decNumber * decNumberCopy(decNumber *dest, const decNumber *src) {
  if (dest == NULL || src == NULL) {
    return dest;
  }
  
  if (dest == src) {
    return dest;
  }

  dest->bits = src->bits;
  dest->exponent = src->exponent;
  dest->digits = src->digits;
  dest->lsu[0] = src->lsu[0];
  
  if (src->digits > DECDPUN) {
    Unit *d = dest->lsu + 1;
    const Unit *s = src->lsu + 1;
    const Unit *smsup = src->lsu + D2U(src->digits);
    
    while (s < smsup) {
      *d = *s;
      d++;
      s++;
    }
  }
  
  return dest;
} /* decNumberCopy */

/* ------------------------------------------------------------------ */
/* decNumberCopyAbs -- quiet absolute value operator		      */
/*								      */
/*   This sets C = abs(A)					      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*								      */
/* C must have space for set->digits digits.			      */
/* No exception or error can occur; this is a quiet bitwise operation.*/
/* See also decNumberAbs for a checking version of this.	      */
/* ------------------------------------------------------------------ */
decNumber * decNumberCopyAbs(decNumber *res, const decNumber *rhs) {
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, DECUNCONT)) return res;
  #endif
  decNumberCopy(res, rhs);
  res->bits &= ~DECNEG;
  return res;
} /* decNumberCopyAbs */

/* ------------------------------------------------------------------ */
/* decNumberCopyNegate -- quiet negate value operator		      */
/*								      */
/*   This sets C = negate(A)					      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*								      */
/* C must have space for set->digits digits.			      */
/* No exception or error can occur; this is a quiet bitwise operation.*/
/* See also decNumberMinus for a checking version of this.	      */
/* ------------------------------------------------------------------ */
decNumber * decNumberCopyNegate(decNumber *res, const decNumber *rhs) {
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, DECUNCONT)) return res;
  #endif
  decNumberCopy(res, rhs);
  res->bits ^= DECNEG;
  return res;
} /* decNumberCopyNegate */

/* ------------------------------------------------------------------ */
/* decNumberCopySign -- quiet copy and set sign operator	      */
/*								      */
/*   This sets C = A with the sign of B 			      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   lhs is A							      */
/*   rhs is B							      */
/*								      */
/* C must have space for set->digits digits.			      */
/* No exception or error can occur; this is a quiet bitwise operation.*/
/* ------------------------------------------------------------------ */
decNumber * decNumberCopySign(decNumber *res, const decNumber *lhs,
                              const decNumber *rhs) {
  uByte sign;
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, DECUNCONT)) return res;
  #endif
  sign = rhs->bits & DECNEG;
  decNumberCopy(res, lhs);
  res->bits = (res->bits & ~DECNEG) | sign;
  return res;
} /* decNumberCopySign */

/* ------------------------------------------------------------------ */
/* decNumberGetBCD -- get the coefficient in BCD8		      */
/*   dn is the source decNumber 				      */
/*   bcd is the uInt array that will receive dn->digits BCD bytes,    */
/*     most-significant at offset 0				      */
/*   returns bcd						      */
/*								      */
/* bcd must have at least dn->digits bytes.  No error is possible; if */
/* dn is a NaN or Infinite, digits must be 1 and the coefficient 0.   */
/* ------------------------------------------------------------------ */
uByte * decNumberGetBCD(const decNumber *dn, uByte *bcd) {
  if (dn == NULL || bcd == NULL || dn->digits <= 0) {
    return bcd;
  }

  uByte *ub = bcd + dn->digits - 1;
  const Unit *up = dn->lsu;

  #if DECDPUN == 1
    while (ub >= bcd) {
      *ub = *up;
      ub--;
      up++;
    }
  #else
    uInt u = *up;
    uInt cut = DECDPUN;
    
    while (ub >= bcd) {
      *ub = (uByte)(u % 10);
      u = u / 10;
      cut--;
      
      if (cut == 0) {
        up++;
        u = *up;
        cut = DECDPUN;
      }
      
      ub--;
    }
  #endif
  
  return bcd;
} /* decNumberGetBCD */

/* ------------------------------------------------------------------ */
/* decNumberSetBCD -- set (replace) the coefficient from BCD8	      */
/*   dn is the target decNumber 				      */
/*   bcd is the uInt array that will source n BCD bytes, most-	      */
/*     significant at offset 0					      */
/*   n is the number of digits in the source BCD array (bcd)	      */
/*   returns dn 						      */
/*								      */
/* dn must have space for at least n digits.  No error is possible;   */
/* if dn is a NaN, or Infinite, or is to become a zero, n must be 1   */
/* and bcd[0] zero.						      */
/* ------------------------------------------------------------------ */
decNumber * decNumberSetBCD(decNumber *dn, const uByte *bcd, uInt n) {
  if (dn == NULL || bcd == NULL || n == 0) {
    return dn;
  }

  Unit *up = dn->lsu + D2U(n) - 1;
  const uByte *ub = bcd;

  #if DECDPUN == 1
    while (ub < bcd + n && up >= dn->lsu) {
      *up = *ub;
      ub++;
      up--;
    }
  #else
    Int cut = MSUDIGITS(n);
    
    while (up >= dn->lsu) {
      *up = 0;
      Int digits_to_process = cut;
      
      while (digits_to_process > 0 && ub < bcd + n) {
        *up = X10(*up) + *ub;
        ub++;
        digits_to_process--;
      }
      
      up--;
      cut = DECDPUN;
    }
  #endif
  
  dn->digits = n;
  return dn;
} /* decNumberSetBCD */

/* ------------------------------------------------------------------ */
/* decNumberIsNormal -- test normality of a decNumber		      */
/*   dn is the decNumber to test				      */
/*   set is the context to use for Emin 			      */
/*   returns 1 if |dn| is finite and >=Nmin, 0 otherwise	      */
/* ------------------------------------------------------------------ */
Int decNumberIsNormal(const decNumber *dn, decContext *set) {
  Int ae;
  #if DECCHECK
  if (decCheckOperands(DECUNRESU, DECUNUSED, dn, set)) return 0;
  #endif

  if (decNumberIsSpecial(dn)) return 0;
  if (decNumberIsZero(dn)) return 0;

  ae = dn->exponent + dn->digits - 1;
  if (ae < set->emin) return 0;
  return 1;
} /* decNumberIsNormal */

/* ------------------------------------------------------------------ */
/* decNumberIsSubnormal -- test subnormality of a decNumber	      */
/*   dn is the decNumber to test				      */
/*   set is the context to use for Emin 			      */
/*   returns 1 if |dn| is finite, non-zero, and <Nmin, 0 otherwise    */
/* ------------------------------------------------------------------ */
Int decNumberIsSubnormal(const decNumber *dn, decContext *set) {
  Int ae;
  #if DECCHECK
  if (decCheckOperands(DECUNRESU, DECUNUSED, dn, set)) return 0;
  #endif

  if (decNumberIsSpecial(dn)) return 0;
  if (decNumberIsZero(dn)) return 0;

  ae = dn->exponent + dn->digits - 1;
  return (ae < set->emin) ? 1 : 0;
} /* decNumberIsSubnormal */

/* ------------------------------------------------------------------ */
/* decNumberTrim -- remove insignificant zeros			      */
/*								      */
/*   dn is the number to trim					      */
/*   returns dn 						      */
/*								      */
/* All fields are updated as required.	This is a utility operation,  */
/* so special values are unchanged and no error is possible.  The     */
/* zeros are removed unconditionally.				      */
/* ------------------------------------------------------------------ */
decNumber * decNumberTrim(decNumber *dn) {
  Int dropped = 0;
  decContext set;
  
  #if DECCHECK
  if (decCheckOperands(DECUNRESU, DECUNUSED, dn, DECUNCONT)) {
    return dn;
  }
  #endif
  
  decContextDefault(&set, DEC_INIT_BASE);
  return decTrim(dn, &set, 0, 1, &dropped);
} /* decNumberTrim */

/* ------------------------------------------------------------------ */
/* decNumberVersion -- return the name and version of this module     */
/*								      */
/* No error is possible.					      */
/* ------------------------------------------------------------------ */
const char* decNumberVersion(void) {
    return DECVERSION;
} /* decNumberVersion */

/* ------------------------------------------------------------------ */
/* decNumberZero -- set a number to 0				      */
/*								      */
/*   dn is the number to set, with space for one digit		      */
/*   returns dn 						      */
/*								      */
/* No error is possible.					      */
/* ------------------------------------------------------------------ */
/* Memset is not used as it is much slower in some environments. */
decNumber * decNumberZero(decNumber *dn) {
  #if DECCHECK
  if (decCheckOperands(dn, DECUNUSED, DECUNUSED, DECUNCONT)) return dn;
  #endif

  dn->bits = 0;
  dn->exponent = 0;
  dn->digits = 1;
  dn->lsu[0] = 0;
  return dn;
} /* decNumberZero */

/* ================================================================== */
/* Local routines						      */
/* ================================================================== */

/* ------------------------------------------------------------------ */
/* decToString -- lay out a number into a string		      */
/*								      */
/*   dn     is the number to lay out				      */
/*   string is where to lay out the number			      */
/*   eng    is 1 if Engineering, 0 if Scientific		      */
/*								      */
/* string must be at least dn->digits+14 characters long	      */
/* No error is possible.					      */
/*								      */
/* Note that this routine can generate a -0 or 0.000.  These are      */
/* never generated in subset to-number or arithmetic, but can occur   */
/* in non-subset arithmetic (e.g., -1*0 or 1.234-1.234).	      */
/* ------------------------------------------------------------------ */
/* If DECCHECK is enabled the string "?" is returned if a number is */
/* invalid. */
static void decToString(const decNumber *dn, char *string, Flag eng) {
  #if DECCHECK
  if (decCheckOperands(DECUNRESU, dn, DECUNUSED, DECUNCONT)) {
    strcpy(string, "?");
    return;
  }
  #endif

  char *c = string;
  
  if (decNumberIsNegative(dn)) {
    *c++ = '-';
  }
  
  if (dn->bits & DECSPECIAL) {
    if (decNumberIsInfinite(dn)) {
      strcpy(c, "Infinity");
      return;
    }
    if (dn->bits & DECSNAN) {
      *c++ = 's';
    }
    strcpy(c, "NaN");
    c += 3;
    if (dn->exponent != 0 || (*dn->lsu == 0 && dn->digits == 1)) {
      return;
    }
  }

  Int exp = dn->exponent;
  const Unit *up = dn->lsu + D2U(dn->digits) - 1;
  Int cut = MSUDIGITS(dn->digits) - 1;
  
  if (exp == 0) {
    for (; up >= dn->lsu; up--) {
      uInt u = *up;
      for (; cut >= 0; c++, cut--) {
        uInt pow;
        TODIGIT(u, cut, c, pow);
      }
      cut = DECDPUN - 1;
    }
    *c = '\0';
    return;
  }

  Int pre = dn->digits + exp;
  Int e = 0;
  
  if (exp > 0 || pre < -5) {
    e = exp + dn->digits - 1;
    pre = 1;
    
    if (eng && e != 0) {
      Int adj = (e < 0) ? ((-e) % 3 ? 3 - ((-e) % 3) : 0) : (e % 3);
      e -= adj;
      
      if (!ISZERO(dn)) {
        pre += adj;
      } else if (adj != 0) {
        e += 3;
        pre = -(2 - adj);
      }
    }
  }

  uInt u = *up;
  
  if (pre > 0) {
    Int n = pre;
    while (pre > 0) {
      if (cut < 0) {
        if (up == dn->lsu) break;
        up--;
        cut = DECDPUN - 1;
        u = *up;
      }
      uInt pow;
      TODIGIT(u, cut, c, pow);
      pre--;
      c++;
      cut--;
    }
    
    if (n < dn->digits) {
      *c++ = '.';
      while (up >= dn->lsu) {
        if (cut < 0) {
          up--;
          if (up < dn->lsu) break;
          cut = DECDPUN - 1;
          u = *up;
        }
        uInt pow;
        TODIGIT(u, cut, c, pow);
        c++;
        cut--;
      }
    } else {
      while (pre > 0) {
        *c++ = '0';
        pre--;
      }
    }
  } else {
    *c++ = '0';
    *c++ = '.';
    
    while (pre < 0) {
      *c++ = '0';
      pre++;
    }
    
    while (up >= dn->lsu) {
      if (cut < 0) {
        up--;
        if (up < dn->lsu) break;
        cut = DECDPUN - 1;
        u = *up;
      }
      uInt pow;
      TODIGIT(u, cut, c, pow);
      c++;
      cut--;
    }
  }

  if (e != 0) {
    *c++ = 'E';
    *c++ = (e < 0) ? '-' : '+';
    
    uInt ue = (e < 0) ? -e : e;
    Flag had = 0;
    
    for (Int cut = 9; cut >= 0; cut--) {
      uInt pow;
      char digit;
      TODIGIT(ue, cut, &digit, pow);
      if (digit != '0' || had || cut == 0) {
        *c++ = digit;
        had = 1;
      }
    }
  }
  
  *c = '\0';
} /* decToString */

/* ------------------------------------------------------------------ */
/* decAddOp -- add/subtract operation				      */
/*								      */
/*   This computes C = A + B					      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X+X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*   negate is DECNEG if rhs should be negated, or 0 otherwise	      */
/*   status accumulates status for the caller			      */
/*								      */
/* C must have space for set->digits digits.			      */
/* Inexact in status must be 0 for correct Exact zero sign in result  */
/* ------------------------------------------------------------------ */
/* If possible, the coefficient is calculated directly into C.	      */
/* However, if: 						      */
/*   -- a digits+1 calculation is needed because the numbers are      */
/*	unaligned and span more than set->digits digits 	      */
/*   -- a carry to digits+1 digits looks possible		      */
/*   -- C is the same as A or B, and the result would destructively   */
/*	overlap the A or B coefficient				      */
/* then the result must be calculated into a temporary buffer.	In    */
/* this case a local (stack) buffer is used if possible, and only if  */
/* too long for that does malloc become the final resort.	      */
/*								      */
/* Misalignment is handled as follows:				      */
/*   Apad: (AExp>BExp) Swap operands and proceed as for BExp>AExp.    */
/*   BPad: Apply the padding by a combination of shifting (whole      */
/*	   units) and multiplication (part units).		      */
/*								      */
/* Addition, especially x=x+1, is speed-critical.		      */
/* The static buffer is larger than might be expected to allow for    */
/* calls from higher-level funtions (notable exp).		      */
/* ------------------------------------------------------------------ */
static decNumber * decAddOp(decNumber *res, const decNumber *lhs,
                            const decNumber *rhs, decContext *set,
                            uByte negate, uInt *status) {
    #if DECSUBSET
    decNumber *alloclhs = NULL;
    decNumber *allocrhs = NULL;
    #endif
    Int rhsshift;
    Int maxdigits;
    Int mult;
    Int residue;
    uByte bits;
    Flag diffsign;
    Unit *acc;
    Unit accbuff[SD2U(DECBUFFER*2+20)];
    Unit *allocacc = NULL;
    Int reqdigits = set->digits;
    Int padding;

    #if DECCHECK
    if (decCheckOperands(res, lhs, rhs, set)) return res;
    #endif

    #if DECSUBSET
    if (!set->extended) {
        if (lhs->digits > reqdigits) {
            alloclhs = decRoundOperand(lhs, set, status);
            if (alloclhs == NULL) goto cleanup;
            lhs = alloclhs;
        }
        if (rhs->digits > reqdigits) {
            allocrhs = decRoundOperand(rhs, set, status);
            if (allocrhs == NULL) goto cleanup;
            rhs = allocrhs;
        }
    }
    #endif

    diffsign = (Flag)((lhs->bits ^ rhs->bits ^ negate) & DECNEG);

    if (SPECIALARGS) {
        if (SPECIALARGS & (DECSNAN | DECNAN)) {
            decNaNs(res, lhs, rhs, set, status);
        } else {
            if (decNumberIsInfinite(lhs)) {
                if (decNumberIsInfinite(rhs) && diffsign) {
                    *status |= DEC_Invalid_operation;
                    goto cleanup;
                }
                bits = lhs->bits & DECNEG;
            } else {
                bits = (rhs->bits ^ negate) & DECNEG;
            }
            bits |= DECINF;
            decNumberZero(res);
            res->bits = bits;
        }
        goto cleanup;
    }

    if (ISZERO(lhs)) {
        if (handleZeroOperand(res, rhs, lhs, negate, diffsign, set, status)) {
            goto cleanup;
        }
    }

    if (ISZERO(rhs)) {
        if (handleZeroOperand(res, lhs, rhs, 0, diffsign, set, status)) {
            goto cleanup;
        }
    }

    padding = rhs->exponent - lhs->exponent;

    if (tryFastPath(res, lhs, rhs, padding, diffsign, reqdigits, set)) {
        goto cleanup;
    }

    rhsshift = 0;
    bits = lhs->bits;
    mult = 1;

    if (padding != 0) {
        if (handlePadding(&lhs, &rhs, &padding, &bits, &rhsshift, &mult, 
                         &residue, res, negate, diffsign, reqdigits, set, status)) {
            goto cleanup;
        }
    }

    if (diffsign) mult = -mult;

    maxdigits = rhs->digits + padding;
    if (lhs->digits > maxdigits) maxdigits = lhs->digits;

    acc = res->lsu;
    if ((maxdigits >= reqdigits) || (res == rhs && rhsshift > 0)) {
        Int need = D2U(maxdigits) + 1;
        acc = accbuff;
        if (need * sizeof(Unit) > sizeof(accbuff)) {
            allocacc = (Unit *)malloc(need * sizeof(Unit));
            if (allocacc == NULL) {
                *status |= DEC_Insufficient_storage;
                goto cleanup;
            }
            acc = allocacc;
        }
    }

    res->bits = (uByte)(bits & DECNEG);
    res->exponent = lhs->exponent;

    #if DECTRACE
    decDumpAr('A', lhs->lsu, D2U(lhs->digits));
    decDumpAr('B', rhs->lsu, D2U(rhs->digits));
    printf("    :h: %ld %ld\n", rhsshift, mult);
    #endif

    res->digits = decUnitAddSub(lhs->lsu, D2U(lhs->digits),
                                rhs->lsu, D2U(rhs->digits),
                                rhsshift, acc, mult) * DECDPUN;
    if (res->digits < 0) {
        res->digits = -res->digits;
        res->bits ^= DECNEG;
    }

    #if DECTRACE
    decDumpAr('+', acc, D2U(res->digits));
    #endif

    residue = 0;
    if (acc != res->lsu) {
        processResult(res, acc, maxdigits, reqdigits, &residue, set, status);
    }

    res->digits = decGetDigits(res->lsu, D2U(res->digits));
    decFinish(res, set, &residue, status);

    if (ISZERO(res) && diffsign
        #if DECSUBSET
        && set->extended
        #endif
        && (*status & DEC_Inexact) == 0) {
        if (set->round == DEC_ROUND_FLOOR) {
            res->bits |= DECNEG;
        } else {
            res->bits &= ~DECNEG;
        }
    }

cleanup:
    free(allocacc);
    #if DECSUBSET
    free(allocrhs);
    free(alloclhs);
    #endif
    return res;
}

static int handleZeroOperand(decNumber *res, const decNumber *nonzero, 
                             const decNumber *zero, uByte negate, Flag diffsign,
                             decContext *set, uInt *status) {
    Int adjust;
    Int zexp = zero->exponent;
    uByte bits = zero->bits;
    Int residue = 0;
    
    decCopyFit(res, nonzero, set, &residue, status);
    if (negate) res->bits ^= negate;
    
    #if DECSUBSET
    if (set->extended) {
    #endif
        adjust = zexp - res->exponent;
        if (ISZERO(res)) {
            if (adjust < 0) res->exponent = zexp;
            if (diffsign) {
                if (set->round != DEC_ROUND_FLOOR) {
                    res->bits = 0;
                } else {
                    res->bits = DECNEG;
                }
            }
        } else if (adjust < 0) {
            if ((res->digits - adjust) > set->digits) {
                adjust = res->digits - set->digits;
                *status |= DEC_Rounded;
            }
            res->digits = decShiftToMost(res->lsu, res->digits, -adjust);
            res->exponent += adjust;
        }
    #if DECSUBSET
    }
    #endif
    
    decFinish(res, set, &residue, status);
    return 1;
}

static int tryFastPath(decNumber *res, const decNumber *lhs, const decNumber *rhs,
                      Int padding, Flag diffsign, Int reqdigits, decContext *set) {
    if (padding != 0 || rhs->digits > DECDPUN || 
        rhs->exponent < set->emin || 
        rhs->exponent > set->emax - set->digits + 1 ||
        rhs->digits > reqdigits || lhs->digits > reqdigits) {
        return 0;
    }
    
    Int partial = *lhs->lsu;
    
    if (!diffsign) {
        partial += *rhs->lsu;
        if (partial <= DECDPUNMAX && 
            (lhs->digits >= DECDPUN || partial < (Int)powers[lhs->digits])) {
            if (res != lhs) decNumberCopy(res, lhs);
            *res->lsu = (Unit)partial;
            return 1;
        }
    } else {
        partial -= *rhs->lsu;
        if (partial > 0) {
            if (res != lhs) decNumberCopy(res, lhs);
            *res->lsu = (Unit)partial;
            res->digits = decGetDigits(res->lsu, D2U(res->digits));
            return 1;
        }
    }
    return 0;
}

static int handlePadding(const decNumber **lhs, const decNumber **rhs, Int *padding,
                        uByte *bits, Int *rhsshift, Int *mult, Int *residue,
                        decNumber *res, uByte negate, Flag diffsign, 
                        Int reqdigits, decContext *set, uInt *status) {
    Flag swapped = 0;
    
    if (*padding < 0) {
        const decNumber *t;
        *padding = -*padding;
        *bits = (uByte)((*rhs)->bits ^ negate);
        t = *lhs; *lhs = *rhs; *rhs = t;
        swapped = 1;
    }
    
    if ((*rhs)->digits + *padding > (*lhs)->digits + reqdigits + 1) {
        Int shift = reqdigits - (*rhs)->digits;
        *residue = diffsign ? -1 : 1;
        decCopyFit(res, *rhs, set, residue, status);
        if (shift > 0) {
            res->digits = decShiftToMost(res->lsu, res->digits, shift);
            res->exponent -= shift;
        }
        if (!swapped) res->bits ^= negate;
        decFinish(res, set, residue, status);
        return 1;
    }
    
    *rhsshift = D2U(*padding + 1) - 1;
    *mult = powers[*padding - (*rhsshift * DECDPUN)];
    return 0;
}

static void processResult(decNumber *res, Unit *acc, Int maxdigits, 
                         Int reqdigits, Int *residue, decContext *set, uInt *status) {
    #if DECSUBSET
    if (set->extended) {
    #endif
        if (res->digits > reqdigits) {
            res->digits = decGetDigits(acc, D2U(res->digits));
        }
        decSetCoeff(res, set, acc, res->digits, residue, status);
    #if DECSUBSET
    } else {
        if (res->digits < maxdigits) {
            *(acc + D2U(res->digits)) = 0;
            res->digits = maxdigits;
        } else if (res->digits > reqdigits) {
            res->digits = decGetDigits(acc, D2U(res->digits));
            if (res->digits < maxdigits) res->digits = maxdigits;
        }
        decSetCoeff(res, set, acc, res->digits, residue, status);
        if (*residue != 0) {
            decApplyRound(res, set, *residue, status);
            *residue = 0;
        }
    }
    #endif
} /* decAddOp */

/* ------------------------------------------------------------------ */
/* decDivideOp -- division operation				      */
/*								      */
/*  This routine performs the calculations for all four division      */
/*  operators (divide, divideInteger, remainder, remainderNear).      */
/*								      */
/*  C=A op B							      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X/X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*   op  is DIVIDE, DIVIDEINT, REMAINDER, or REMNEAR respectively.    */
/*   status is the usual accumulator				      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* ------------------------------------------------------------------ */
/*   The underlying algorithm of this routine is the same as in the   */
/*   1981 S/370 implementation, that is, non-restoring long division  */
/*   with bi-unit (rather than bi-digit) estimation for each unit     */
/*   multiplier.  In this pseudocode overview, complications for the  */
/*   Remainder operators and division residues for exact rounding are */
/*   omitted for clarity.					      */
/*								      */
/*     Prepare operands and handle special values		      */
/*     Test for x/0 and then 0/x				      */
/*     Exp =Exp1 - Exp2 					      */
/*     Exp =Exp +len(var1) -len(var2)				      */
/*     Sign=Sign1 * Sign2					      */
/*     Pad accumulator (Var1) to double-length with 0's (pad1)	      */
/*     Pad Var2 to same length as Var1				      */
/*     msu2pair/plus=1st 2 or 1 units of var2, +1 to allow for round  */
/*     have=0							      */
/*     Do until (have=digits+1 OR residue=0)			      */
/*	 if exp<0 then if integer divide/residue then leave	      */
/*	 this_unit=0						      */
/*	 Do forever						      */
/*	    compare numbers					      */
/*	    if <0 then leave inner_loop 			      */
/*	    if =0 then (* quick exit without subtract *) do	      */
/*	       this_unit=this_unit+1; output this_unit		      */
/*	       leave outer_loop; end				      */
/*	    Compare lengths of numbers (mantissae):		      */
/*	    If same then tops2=msu2pair -- {units 1&2 of var2}	      */
/*		    else tops2=msu2plus -- {0, unit 1 of var2}	      */
/*	    tops1=first_unit_of_Var1*10**DECDPUN +second_unit_of_var1 */
/*	    mult=tops1/tops2  -- Good and safe guess at divisor       */
/*	    if mult=0 then mult=1				      */
/*	    this_unit=this_unit+mult				      */
/*	    subtract						      */
/*	    end inner_loop					      */
/*	  if have\=0 | this_unit\=0 then do			      */
/*	    output this_unit					      */
/*	    have=have+1; end					      */
/*	  var2=var2/10						      */
/*	  exp=exp-1						      */
/*	  end outer_loop					      */
/*     exp=exp+1   -- set the proper exponent			      */
/*     if have=0 then generate answer=0 			      */
/*     Return (Result is defined by Var1)			      */
/*								      */
/* ------------------------------------------------------------------ */
/* Two working buffers are needed during the division; one (digits+   */
/* 1) to accumulate the result, and the other (up to 2*digits+1) for  */
/* long subtractions.  These are acc and var1 respectively.	      */
/* var1 is a copy of the lhs coefficient, var2 is the rhs coefficient.*/
/* The static buffers may be larger than might be expected to allow   */
/* for calls from higher-level funtions (notable exp).		      */
/* ------------------------------------------------------------------ */
static decNumber * decDivideOp(decNumber *res,
                               const decNumber *lhs, const decNumber *rhs,
                               decContext *set, Flag op, uInt *status) {
    #if DECSUBSET
    decNumber *alloclhs = NULL;
    decNumber *allocrhs = NULL;
    #endif
    Unit accbuff[SD2U(DECBUFFER+DECDPUN+10)];
    Unit *acc = accbuff;
    Unit *allocacc = NULL;
    Unit *accnext;
    Int acclength;
    Int accunits;
    Int accdigits;
    
    Unit varbuff[SD2U(DECBUFFER*2+DECDPUN)];
    Unit *var1 = varbuff;
    Unit *varalloc = NULL;
    Unit *msu1;
    
    const Unit *var2;
    const Unit *msu2;
    Int msu2plus;
    eInt msu2pair;
    
    Int var1units, var2units;
    Int var2ulen;
    Int var1initpad = 0;
    Int maxdigits;
    Int mult;
    Unit thisunit;
    Int residue;
    Int reqdigits = set->digits;
    Int exponent;
    Int maxexponent = 0;
    uByte bits;
    Unit *target;
    const Unit *source;
    uInt const *pow;
    Int shift, cut;
    #if DECSUBSET
    Int dropped;
    #endif
    
    #if DECCHECK
    if (decCheckOperands(res, lhs, rhs, set)) return res;
    #endif
    
    #if DECSUBSET
    if (!set->extended) {
        if (lhs->digits > reqdigits) {
            alloclhs = decRoundOperand(lhs, set, status);
            if (alloclhs == NULL) goto cleanup;
            lhs = alloclhs;
        }
        if (rhs->digits > reqdigits) {
            allocrhs = decRoundOperand(rhs, set, status);
            if (allocrhs == NULL) goto cleanup;
            rhs = allocrhs;
        }
    }
    #endif
    
    bits = (lhs->bits ^ rhs->bits) & DECNEG;
    
    if (SPECIALARGS) {
        if (SPECIALARGS & (DECSNAN | DECNAN)) {
            decNaNs(res, lhs, rhs, set, status);
            goto cleanup;
        }
        if (decNumberIsInfinite(lhs)) {
            if (decNumberIsInfinite(rhs) || (op & (REMAINDER | REMNEAR))) {
                *status |= DEC_Invalid_operation;
                goto cleanup;
            }
            decNumberZero(res);
            res->bits = bits | DECINF;
            goto cleanup;
        }
        residue = 0;
        if (op & (REMAINDER | REMNEAR)) {
            decCopyFit(res, lhs, set, &residue, status);
        } else {
            decNumberZero(res);
            res->bits = bits;
            if (op & DIVIDE) {
                res->exponent = set->emin - set->digits + 1;
                *status |= DEC_Clamped;
            }
        }
        decFinish(res, set, &residue, status);
        goto cleanup;
    }
    
    if (ISZERO(rhs)) {
        if (ISZERO(lhs)) {
            decNumberZero(res);
            *status |= DEC_Division_undefined;
        } else {
            decNumberZero(res);
            if (op & (REMAINDER | REMNEAR)) {
                *status |= DEC_Invalid_operation;
            } else {
                *status |= DEC_Division_by_zero;
                res->bits = bits | DECINF;
            }
        }
        goto cleanup;
    }
    
    if (ISZERO(lhs)) {
        if (decHandleZeroLhs(res, lhs, rhs, set, op, bits, status)) {
            goto cleanup;
        }
    }
    
    exponent = (lhs->exponent + lhs->digits) - (rhs->exponent + rhs->digits);
    
    if (exponent < 0 && !(op == DIVIDE)) {
        if (decHandleNegativeExponent(res, lhs, rhs, set, op, bits, exponent, status)) {
            goto cleanup;
        }
    }
    
    acclength = D2U(reqdigits + DECDPUN);
    if (acclength * sizeof(Unit) > sizeof(accbuff)) {
        allocacc = (Unit *)malloc(acclength * sizeof(Unit));
        if (allocacc == NULL) {
            *status |= DEC_Insufficient_storage;
            goto cleanup;
        }
        acc = allocacc;
    }
    
    maxdigits = rhs->digits + reqdigits - 1;
    if (lhs->digits > maxdigits) maxdigits = lhs->digits;
    var1units = D2U(maxdigits) + 2;
    if (!(op & DIVIDE)) var1units++;
    
    if ((var1units + 1) * sizeof(Unit) > sizeof(varbuff)) {
        varalloc = (Unit *)malloc((var1units + 1) * sizeof(Unit));
        if (varalloc == NULL) {
            *status |= DEC_Insufficient_storage;
            goto cleanup;
        }
        var1 = varalloc;
    }
    
    if (decInitializeVariables(var1, var1units, lhs, rhs, &msu1, &var2, &msu2,
                              &var2units, &var2ulen, &msu2plus, &msu2pair) != 0) {
        *status |= DEC_Insufficient_storage;
        goto cleanup;
    }
    
    decAdjustExponent(&exponent, msu1, msu2);
    
    if (!(op & DIVIDE)) {
        var1initpad = (var1units - D2U(lhs->digits)) * DECDPUN;
        cut = (exponent < 0) ? -exponent : DECDPUN - exponent % DECDPUN;
        decShiftToLeast(var1, var1units, cut);
        exponent += cut;
        var1initpad -= cut;
        decCleanUnits(msu1, cut);
    } else {
        maxexponent = lhs->exponent - rhs->exponent;
        if (*msu1 < *msu2) {
            var2ulen--;
            exponent -= DECDPUN;
        }
    }
    
    if (decPerformDivision(acc, acclength, var1, var1units, &msu1, var2, var2units,
                          msu2, var2ulen, msu2plus, msu2pair, &accunits, &accdigits,
                          &accnext, &thisunit, &exponent, maxexponent, reqdigits,
                          op, &var1initpad) != 0) {
        *status |= DEC_Insufficient_storage;
        goto cleanup;
    }
    
    if (decProcessResult(res, acc, accnext, accunits, accdigits, var1, var1units,
                        lhs, rhs, set, op, bits, exponent, maxexponent, reqdigits,
                        var1initpad, &residue, status) != 0) {
        goto cleanup;
    }
    
    #if DECSUBSET
    if (!set->extended && (op == DIVIDE)) decTrim(res, set, 0, 1, &dropped);
    #endif
    
cleanup:
    free(varalloc);
    free(allocacc);
    #if DECSUBSET
    free(allocrhs);
    free(alloclhs);
    #endif
    return res;
}

static int decHandleZeroLhs(decNumber *res, const decNumber *lhs, const decNumber *rhs,
                            decContext *set, Flag op, uByte bits, uInt *status) {
    #if DECSUBSET
    if (!set->extended) {
        decNumberZero(res);
        return 1;
    }
    #endif
    
    if (op & DIVIDE) {
        Int residue = 0;
        Int exponent = lhs->exponent - rhs->exponent;
        decNumberCopy(res, lhs);
        res->bits = bits;
        res->exponent = exponent;
        decFinalize(res, set, &residue, status);
    } else if (op & DIVIDEINT) {
        decNumberZero(res);
        res->bits = bits;
    } else {
        Int exponent = rhs->exponent;
        decNumberCopy(res, lhs);
        if (exponent < res->exponent) res->exponent = exponent;
    }
    return 1;
}

static int decHandleNegativeExponent(decNumber *res, const decNumber *lhs, const decNumber *rhs,
                                     decContext *set, Flag op, uByte bits, Int exponent,
                                     uInt *status) {
    if (op & DIVIDEINT) {
        decNumberZero(res);
        #if DECSUBSET
        if (set->extended)
        #endif
        res->bits = bits;
        return 1;
    }
    
    if (lhs->exponent <= rhs->exponent) {
        if ((op & REMAINDER) || exponent < -1) {
            Int residue = 0;
            decCopyFit(res, lhs, set, &residue, status);
            decFinish(res, set, &residue, status);
            return 1;
        }
    }
    return 0;
}

static int decInitializeVariables(Unit *var1, Int var1units, const decNumber *lhs,
                                  const decNumber *rhs, Unit **msu1, const Unit **var2,
                                  const Unit **msu2, Int *var2units, Int *var2ulen,
                                  Int *msu2plus, eInt *msu2pair) {
    Unit *target;
    const Unit *source;
    
    *msu1 = var1 + var1units - 1;
    source = lhs->lsu + D2U(lhs->digits) - 1;
    
    for (target = *msu1; source >= lhs->lsu; source--, target--) {
        *target = *source;
    }
    for (; target >= var1; target--) {
        *target = 0;
    }
    
    *var2ulen = var1units;
    *var2units = D2U(rhs->digits);
    *var2 = rhs->lsu;
    *msu2 = *var2 + *var2units - 1;
    
    *msu2plus = **msu2;
    if (*var2units > 1) (*msu2plus)++;
    
    *msu2pair = (eInt)**msu2 * (DECDPUNMAX + 1);
    if (*var2units > 1) {
        *msu2pair += *(*msu2 - 1);
        if (*var2units > 2) (*msu2pair)++;
    }
    
    return 0;
}

static void decAdjustExponent(Int *exponent, const Unit *msu1, const Unit *msu2) {
    uInt const *pow;
    
    for (pow = &powers[1]; *msu1 >= *pow; pow++) (*exponent)--;
    for (pow = &powers[1]; *msu2 >= *pow; pow++) (*exponent)++;
}

static void decCleanUnits(Unit *msu, Int cut) {
    Unit *u;
    for (u = msu; cut >= DECDPUN; cut -= DECDPUN, u--) {
        *u = 0;
    }
}

static int decPerformDivision(Unit *acc, Int acclength, Unit *var1, Int var1units,
                              Unit **msu1, const Unit *var2, Int var2units,
                              const Unit *msu2, Int var2ulen, Int msu2plus, eInt msu2pair,
                              Int *accunits, Int *accdigits, Unit **accnext,
                              Unit *thisunit, Int *exponent, Int maxexponent,
                              Int reqdigits, Flag op, Int *var1initpad) {
    uInt const *pow;
    Int shift;
    
    *accunits = 0;
    *accdigits = 0;
    *accnext = acc + acclength - 1;
    
    for (;;) {
        *thisunit = 0;
        
        if (decComputeUnit(var1, var1units, msu1, var2, var2units, msu2,
                          &var2ulen, msu2plus, msu2pair, thisunit) != 0) {
            break;
        }
        
        if (*accunits != 0 || *thisunit != 0) {
            **accnext = *thisunit;
            if (*accunits == 0) {
                (*accdigits)++;
                for (pow = &powers[1]; *thisunit >= *pow; pow++) {
                    (*accdigits)++;
                }
            } else {
                *accdigits += DECDPUN;
            }
            (*accunits)++;
            (*accnext)--;
            if (*accdigits > reqdigits) break;
        }
        
        if (**var1 == 0 && var1units == 1) {
            if (op & (REMAINDER | REMNEAR)) break;
            if ((op & DIVIDE) && (*exponent <= maxexponent)) break;
        }
        
        if (*exponent == 0 && !(op & DIVIDE)) break;
        
        var2ulen--;
        *exponent -= DECDPUN;
    }
    
    if (*accunits == 0) {
        *accunits = 1;
        *accdigits = 1;
        **accnext = 0;
    } else {
        (*accnext)++;
    }
    
    return 0;
}

static int decComputeUnit(Unit *var1, Int var1units, Unit **msu1,
                          const Unit *var2, Int var2units, const Unit *msu2,
                          Int *var2ulen, Int msu2plus, eInt msu2pair,
                          Unit *thisunit) {
    Int mult;
    Int shift;
    
    for (;;) {
        for (; **msu1 == 0 && *msu1 > var1; (*msu1)--) {
            var1units--;
        }
        
        if (var1units < *var2ulen) return 1;
        
        if (var1units == *var2ulen) {
            if (decCompareUnits(*msu1, msu2, var1, var2, thisunit) != 0) {
                return 1;
            }
            mult = (Int)(((eInt)**msu1 * (DECDPUNMAX + 1) + *(*msu1 - 1)) / msu2pair);
        } else {
            mult = (Int)(((eInt)**msu1 * (DECDPUNMAX + 1) + *(*msu1 - 1)) / msu2plus);
        }
        
        if (mult == 0) mult = 1;
        *thisunit = (Unit)(*thisunit + mult);
        
        shift = *var2ulen - var2units;
        decUnitAddSub(&var1[shift], var1units - shift, var2, var2units, 0,
                     &var1[shift], -mult);
    }
    
    return 0;
}

static int decCompareUnits(const Unit *msu1, const Unit *msu2, const Unit *var1,
                           const Unit *var2, Unit *thisunit) {
    const Unit *pv1, *pv2;
    Unit v2;
    
    pv2 = msu2;
    for (pv1 = msu1; ; pv1--, pv2--) {
        v2 = 0;
        if (pv2 >= var2) v2 = *pv2;
        if (*pv1 != v2) break;
        if (pv1 == var1) break;
    }
    
    if (*pv1 < v2) return 1;
    
    if (*pv1 == v2) {
        (*thisunit)++;
        *((Unit *)var1) = 0;
        return 1;
    }
    
    return 0;
}

static int decProcessResult(decNumber *res, Unit *acc, Unit *accnext, Int accunits,
                           Int accdigits, Unit *var1, Int var1units,
                           const decNumber *lhs, const decNumber *rhs,
                           decContext *set, Flag op, uByte bits, Int exponent,
                           Int maxexponent, Int reqdigits, Int var1initpad,
                           Int *residue, uInt *status) {
    *residue = 0;
    
    if (op & DIVIDE) {
        if (decProcessDivide(res, acc, accnext, &accunits, &accdigits,
                           var1, var1units, set, bits, &exponent,
                           maxexponent, residue, status) != 0) {
            return 1;
        }
    } else {
        if (decProcessRemainderOrInt(res, acc, accnext, &accunits, &accdigits,
                                    var1, var1units, lhs, rhs, set, op, bits,
                                    &exponent, reqdigits, var1initpad,
                                    residue, status) != 0) {
            return 1;
        }
    }
    
    res->exponent = exponent;
    res->bits = (uByte)(bits & DECNEG);
    decSetCoeff(res, set, accnext, accdigits, residue, status);
    decFinish(res, set, residue, status);
    
    return 0;
}

static int decProcessDivide(decNumber *res, Unit *acc, Unit *accnext, Int *accunits,
                           Int *accdigits, Unit *var1, Int var1units,
                           decContext *set, uByte bits, Int *exponent,
                           Int maxexponent, Int *residue, uInt *status) {
    if (*var1 != 0 || var1units > 1) {
        *residue = 1;
    } else {
        #if DECDPUN > 1
        Unit lsu = *accnext;
        if (!(lsu & 0x01) && (lsu != 0)) {
            Int drop = 0;
            for (;; drop++) {
                if (*exponent >= maxexponent) break;
                #if DECDPUN <= 4
                if ((lsu - QUOT10(lsu, drop + 1) * powers[drop + 1]) != 0) break;
                #else
                if (lsu % powers[drop + 1] != 0) break;
                #endif
                (*exponent)++;
            }
            if (drop > 0) {
                *accunits = decShiftToLeast(accnext, *accunits, drop);
                *accdigits = decGetDigits(accnext, *accunits);
                *accunits = D2U(*accdigits);
            }
        }
        #endif
    }
    return 0;
}

static int decProcessRemainderOrInt(decNumber *res, Unit *acc, Unit *accnext,
                                   Int *accunits, Int *accdigits, Unit *var1,
                                   Int var1units, const decNumber *lhs,
                                   const decNumber *rhs, decContext *set, Flag op,
                                   uByte bits, Int *exponent, Int reqdigits,
                                   Int var1initpad, Int *residue, uInt *status) {
    if (*accdigits + *exponent > reqdigits) {
        *status |= DEC_Division_impossible;
        return 1;
    }
    
    if (op & (REMAINDER | REMNEAR)) {
        return decProcessRemainder(res, acc, accnext, accunits, accdigits,
                                  var1, var1units, lhs, rhs, set, op, &bits,
                                  exponent, var1initpad, residue, status);
    }
    
    return 0;
}

static int decProcessRemainder(decNumber *res, Unit *acc, Unit *accnext,
                              Int *accunits, Int *accdigits, Unit *var1,
                              Int var1units, const decNumber *lhs,
                              const decNumber *rhs, decContext *set, Flag op,
                              uByte *bits, Int *exponent, Int var1initpad,
                              Int *residue, uInt *status) {
    Int postshift;
    Flag wasodd = 0;
    Unit *quotlsu;
    Int quotdigits;
    
    *bits = lhs->bits;
    
    if (*var1 == 0 && var1units == 1) {
        Int exp = lhs->exponent;
        if (rhs->exponent < exp) exp = rhs->exponent;
        decNumberZero(res);
        #if DECSUBSET
        if (set->extended)
        #endif
        res->exponent = exp;
        res->bits = (uByte)(*bits & DECNEG);
        decFinish(res, set, residue, status);
        return 1;
    }
    
    if (*accnext & 0x01) wasodd = 1;
    quotlsu = accnext;
    quotdigits = *accdigits;
    
    postshift = var1initpad + *exponent - lhs->exponent + rhs->exponent;
    if (var1initpad < postshift) postshift = var1initpad;
    
    var1units = decShiftToLeast(var1, var1units, postshift);
    accnext = var1;
    *accdigits = decGetDigits(var1, var1units);
    *accunits = D2U(*accdigits);
    
    *exponent = lhs->exponent;
    if (rhs->exponent < *exponent) *exponent = rhs->exponent;
    
    if (op & REMNEAR) {
        return decHandleRemainderNear(res, accnext, accunits, var1units,
                                     rhs, quotlsu, quotdigits, reqdigits,
                                     wasodd, bits, exponent, status);
    }
    
    return 0;
}

static int decHandleRemainderNear(decNumber *res, Unit *accnext, Int *accunits,
                                 Int var1units, const decNumber *rhs,
                                 Unit *quotlsu, Int quotdigits, Int reqdigits,
                                 Flag wasodd, uByte *bits, Int *exponent,
                                 uInt *status) {
    Int compare, tarunits;
    Unit *up;
    
    tarunits = decUnitAddSub(accnext, *accunits, accnext, *accunits, 0, accnext, 1);
    
    compare = decUnitCompare(accnext, tarunits, rhs->lsu, D2U(rhs->digits),
                            rhs->exponent - *exponent);
    if (compare == BADINT) {
        *status |= DEC_Insufficient_storage;
        return 1;
    }
    
    for (up = accnext; up < accnext + tarunits; up++) {
        Int half = *up & 0x01;
        *up /= 2;
        if (half) {
            *(up - 1) += (DECDPUNMAX + 1) / 2;
        }
    }
    
    if (compare > 0 || (compare == 0 && wasodd)) {
        if (decCheckAllNines(quotlsu, quotdigits, reqdigits)) {
            *status |= DEC_Division_impossible;
            return 1;
        }
        
        return decSubtractRhs(accnext, accunits, rhs, bits, exponent, status);
    }
    
    return 0;
}

static int decCheckAllNines(Unit *quotlsu, Int quotdigits, Int reqdigits) {
    Unit *up;
    
    if (quotdigits == reqdigits) {
        for (up = quotlsu; ; up++) {
            if (quotdigits > DECDPUN) {
                if (*up != DECDPUNMAX) return 0;
            } else {
                if (*up == powers[quotdigits] - 1) return 1;
                return 0;
            }
            quotdigits -= DECDPUN;
        }
    }
    return 0;
}

static int decSubtractRhs(Unit *accnext, Int *accunits, const decNumber *rhs,
                         uByte *bits, Int *exponent, uInt *status) {
    Int exp, expunits, exprem;
    
    exp = rhs->exponent - *exponent;
    expunits = exp / DECDPUN;
    exprem = exp % DECDPUN;
    
    *accunits = -decUnitAddSub(accnext, *accunits, rhs->lsu, D2U(rhs->digits),
                               expunits, accnext, -(Int)powers[exprem]);
    *accunits = D2U(decGetDigits(accnext, *accunits));
    *bits ^= DECNEG;
    
    return 0;
} /* decDivideOp */

/* ------------------------------------------------------------------ */
/* decMultiplyOp -- multiplication operation			      */
/*								      */
/*  This routine performs the multiplication C=A x B.		      */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X*X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*   status is the usual accumulator				      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* ------------------------------------------------------------------ */
/* 'Classic' multiplication is used rather than Karatsuba, as the     */
/* latter would give only a minor improvement for the short numbers   */
/* expected to be handled most (and uses much more memory).	      */
/*								      */
/* There are two major paths here: the general-purpose ('old code')   */
/* path which handles all DECDPUN values, and a fastpath version      */
/* which is used if 64-bit ints are available, DECDPUN<=4, and more   */
/* than two calls to decUnitAddSub would be made.		      */
/*								      */
/* The fastpath version lumps units together into 8-digit or 9-digit  */
/* chunks, and also uses a lazy carry strategy to minimise expensive  */
/* 64-bit divisions.  The chunks are then broken apart again into     */
/* units for continuing processing.  Despite this overhead, the       */
/* fastpath can speed up some 16-digit operations by 10x (and much    */
/* more for higher-precision calculations).			      */
/*								      */
/* A buffer always has to be used for the accumulator; in the	      */
/* fastpath, buffers are also always needed for the chunked copies of */
/* of the operand coefficients. 				      */
/* Static buffers are larger than needed just for multiply, to allow  */
/* for calls from other operations (notably exp).		      */
/* ------------------------------------------------------------------ */
#define FASTMUL (DECUSE64 && DECDPUN<5)
static decNumber * decMultiplyOp(decNumber *res, const decNumber *lhs,
                                  const decNumber *rhs, decContext *set,
                                  uInt *status) {
  Int accunits;
  Int exponent;
  Int residue = 0;
  uByte bits;
  Unit *acc;
  Int needbytes;
  void *allocacc = NULL;
  Unit accbuff[SD2U(DECBUFFER*4+1)];
  const Unit *mer, *mermsup;
  Int madlength;
  Int shift;

  #if FASTMUL
    #if DECDPUN & 1
      #define FASTBASE 1000000000
      #define FASTDIGS 9
      #define FASTLAZY 18
    #else
      #define FASTBASE 100000000
      #define FASTDIGS 8
      #define FASTLAZY 1844
    #endif
    uInt zlhibuff[(DECBUFFER*2+1)/8+1];
    uInt *zlhi = zlhibuff;
    uInt *alloclhi = NULL;
    uInt zrhibuff[(DECBUFFER*2+1)/8+1];
    uInt *zrhi = zrhibuff;
    uInt *allocrhi = NULL;
    uLong zaccbuff[(DECBUFFER*2+1)/4+2];
    uLong *zacc = zaccbuff;
    #if DECDPUN==1
    Int zoff;
    #endif
    uInt *lip, *rip;
    uInt *lmsi, *rmsi;
    Int ilhs, irhs, iacc;
    Int lazy;
    uLong lcarry;
    uInt carry;
    Int count;
    const Unit *cup;
    Unit *up;
    uLong *lp;
    Int p;
  #endif

  #if DECSUBSET
    decNumber *alloclhs = NULL;
    decNumber *allocrhs = NULL;
  #endif

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  bits = (uByte)((lhs->bits ^ rhs->bits) & DECNEG);

  if (SPECIALARGS) {
    if (SPECIALARGS & (DECSNAN | DECNAN)) {
      decNaNs(res, lhs, rhs, set, status);
      return res;
    }
    if (((lhs->bits & DECINF) == 0 && ISZERO(lhs)) ||
        ((rhs->bits & DECINF) == 0 && ISZERO(rhs))) {
      *status |= DEC_Invalid_operation;
      return res;
    }
    decNumberZero(res);
    res->bits = bits | DECINF;
    return res;
  }

  if (lhs->digits < rhs->digits) {
    const decNumber *hold = lhs;
    lhs = rhs;
    rhs = hold;
  }

  #if DECSUBSET
  if (!set->extended) {
    if (lhs->digits > set->digits) {
      alloclhs = decRoundOperand(lhs, set, status);
      if (alloclhs == NULL) goto cleanup;
      lhs = alloclhs;
    }
    if (rhs->digits > set->digits) {
      allocrhs = decRoundOperand(rhs, set, status);
      if (allocrhs == NULL) goto cleanup;
      rhs = allocrhs;
    }
  }
  #endif

  #if FASTMUL
  #define NEEDTWO (DECDPUN*2)
  if (rhs->digits > NEEDTWO) {
    ilhs = (lhs->digits + FASTDIGS - 1) / FASTDIGS;
    irhs = (rhs->digits + FASTDIGS - 1) / FASTDIGS;
    iacc = ilhs + irhs;

    needbytes = ilhs * sizeof(uInt);
    if (needbytes > (Int)sizeof(zlhibuff)) {
      alloclhi = (uInt *)malloc(needbytes);
      if (alloclhi == NULL) {
        *status |= DEC_Insufficient_storage;
        goto cleanup;
      }
      zlhi = alloclhi;
    }

    needbytes = irhs * sizeof(uInt);
    if (needbytes > (Int)sizeof(zrhibuff)) {
      allocrhi = (uInt *)malloc(needbytes);
      if (allocrhi == NULL) {
        *status |= DEC_Insufficient_storage;
        goto cleanup;
      }
      zrhi = allocrhi;
    }

    needbytes = iacc * sizeof(uLong);
    #if DECDPUN==1
    zoff = (iacc + 7) / 8;
    needbytes += zoff * 8;
    #endif
    if (needbytes > (Int)sizeof(zaccbuff)) {
      allocacc = malloc(needbytes);
      if (allocacc == NULL) {
        *status |= DEC_Insufficient_storage;
        goto cleanup;
      }
      zacc = (uLong *)allocacc;
    }

    acc = (Unit *)zacc;
    #if DECDPUN==1
    zacc += zoff;
    #endif

    for (count = lhs->digits, cup = lhs->lsu, lip = zlhi; count > 0; lip++) {
      *lip = 0;
      for (p = 0; p < FASTDIGS && count > 0; p += DECDPUN, cup++, count -= DECDPUN) {
        *lip += *cup * powers[p];
      }
    }
    lmsi = lip - 1;

    for (count = rhs->digits, cup = rhs->lsu, rip = zrhi; count > 0; rip++) {
      *rip = 0;
      for (p = 0; p < FASTDIGS && count > 0; p += DECDPUN, cup++, count -= DECDPUN) {
        *rip += *cup * powers[p];
      }
    }
    rmsi = rip - 1;

    for (lp = zacc; lp < zacc + iacc; lp++) *lp = 0;

    lazy = FASTLAZY;
    for (rip = zrhi; rip <= rmsi; rip++) {
      lp = zacc + (rip - zrhi);
      for (lip = zlhi; lip <= lmsi; lip++, lp++) {
        *lp += (uLong)(*lip) * (*rip);
      }
      lazy--;
      if (lazy > 0 && rip != rmsi) continue;
      lazy = FASTLAZY;

      for (lp = zacc; lp < zacc + iacc; lp++) {
        if (*lp < FASTBASE) continue;
        lcarry = *lp / FASTBASE;
        if (lcarry < FASTBASE) {
          carry = (uInt)lcarry;
        } else {
          uInt carry2 = (uInt)(lcarry / FASTBASE);
          *(lp + 2) += carry2;
          *lp -= ((uLong)FASTBASE * FASTBASE * carry2);
          carry = (uInt)(lcarry - ((uLong)FASTBASE * carry2));
        }
        *(lp + 1) += carry;
        *lp -= ((uLong)FASTBASE * carry);
      }
    }

    for (lp = zacc, up = acc; lp < zacc + iacc; lp++) {
      uInt item = (uInt)*lp;
      for (p = 0; p < FASTDIGS - DECDPUN; p += DECDPUN, up++) {
        uInt part = item / (DECDPUNMAX + 1);
        *up = (Unit)(item - (part * (DECDPUNMAX + 1)));
        item = part;
      }
      *up = (Unit)item;
      up++;
    }
    accunits = up - acc;
  } else {
  #endif

    acc = accbuff;
    needbytes = (D2U(lhs->digits) + D2U(rhs->digits)) * sizeof(Unit);
    if (needbytes > (Int)sizeof(accbuff)) {
      allocacc = malloc(needbytes);
      if (allocacc == NULL) {
        *status |= DEC_Insufficient_storage;
        goto cleanup;
      }
      acc = (Unit *)allocacc;
    }

    accunits = 1;
    *acc = 0;
    shift = 0;
    madlength = D2U(lhs->digits);
    mermsup = rhs->lsu + D2U(rhs->digits);

    for (mer = rhs->lsu; mer < mermsup; mer++) {
      if (*mer != 0) {
        accunits = decUnitAddSub(&acc[shift], accunits - shift,
                                  lhs->lsu, madlength, 0,
                                  &acc[shift], *mer) + shift;
      } else {
        *(acc + accunits) = 0;
        accunits++;
      }
      shift++;
    }
  #if FASTMUL
  }
  #endif

  #if DECTRACE
  decDumpAr('*', acc, accunits);
  #endif

  res->bits = bits;
  res->digits = decGetDigits(acc, accunits);

  exponent = lhs->exponent + rhs->exponent;
  if (lhs->exponent < 0 && rhs->exponent < 0 && exponent > 0) {
    exponent = -2 * DECNUMMAXE;
  }
  res->exponent = exponent;

  decSetCoeff(res, set, acc, res->digits, &residue, status);
  decFinish(res, set, &residue, status);

cleanup:
  free(allocacc);
  #if DECSUBSET
  free(allocrhs);
  free(alloclhs);
  #endif
  #if FASTMUL
  free(allocrhi);
  free(alloclhi);
  #endif
  return res;
} /* decMultiplyOp */

/* ------------------------------------------------------------------ */
/* decExpOp -- effect exponentiation				      */
/*								      */
/*   This computes C = exp(A)					      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context; note that rounding mode has no effect	      */
/*								      */
/* C must have space for set->digits digits. status is updated but    */
/* not set.							      */
/*								      */
/* Restrictions:						      */
/*								      */
/*   digits, emax, and -emin in the context must be less than	      */
/*   2*DEC_MAX_MATH (1999998), and the rhs must be within these       */
/*   bounds or a zero.	This is an internal routine, so these	      */
/*   restrictions are contractual and not enforced.		      */
/*								      */
/* A finite result is rounded using DEC_ROUND_HALF_EVEN; it will      */
/* almost always be correctly rounded, but may be up to 1 ulp in      */
/* error in rare cases. 					      */
/*								      */
/* Finite results will always be full precision and Inexact, except   */
/* when A is a zero or -Infinity (giving 1 or 0 respectively).	      */
/* ------------------------------------------------------------------ */
/* This approach used here is similar to the algorithm described in   */
/*								      */
/*   Variable Precision Exponential Function, T. E. Hull and	      */
/*   A. Abrham, ACM Transactions on Mathematical Software, Vol 12 #2, */
/*   pp79-91, ACM, June 1986.					      */
/*								      */
/* with the main difference being that the iterations in the series   */
/* evaluation are terminated dynamically (which does not require the  */
/* extra variable-precision variables which are expensive in this     */
/* context).							      */
/*								      */
/* The error analysis in Hull & Abrham's paper applies except for the */
/* round-off error accumulation during the series evaluation.  This   */
/* code does not precalculate the number of iterations and so cannot  */
/* use Horner's scheme.  Instead, the accumulation is done at double- */
/* precision, which ensures that the additions of the terms are exact */
/* and do not accumulate round-off (and any round-off errors in the   */
/* terms themselves move 'to the right' faster than they can	      */
/* accumulate).  This code also extends the calculation by allowing,  */
/* in the spirit of other decNumber operators, the input to be more   */
/* precise than the result (the precision used is based on the more   */
/* precise of the input or requested result).			      */
/*								      */
/* Implementation notes:					      */
/*								      */
/* 1. This is separated out as decExpOp so it can be called from      */
/*    other Mathematical functions (notably Ln) with a wider range    */
/*    than normal.  In particular, it can handle the slightly wider   */
/*    (double) range needed by Ln (which has to be able to calculate  */
/*    exp(-x) where x can be the tiniest number (Ntiny).	      */
/*								      */
/* 2. Normalizing x to be <=0.1 (instead of <=1) reduces loop	      */
/*    iterations by approximately a third with additional (although    */
/*    diminishing) returns as the range is reduced to even smaller    */
/*    fractions.  However, h (the power of 10 used to correct the     */
/*    result at the end, see below) must be kept <=8 as otherwise     */
/*    the final result cannot be computed.  Hence the leverage is a   */
/*    sliding value (8-h), where potentially the range is reduced     */
/*    more for smaller values.					      */
/*								      */
/*    The leverage that can be applied in this way is severely	      */
/*    limited by the cost of the raise-to-the power at the end,       */
/*    which dominates when the number of iterations is small (less    */
/*    than ten) or when rhs is short.  As an example, the adjustment  */
/*    x**10,000,000 needs 31 multiplications, all but one full-width. */
/*								      */
/* 3. The restrictions (especially precision) could be raised with    */
/*    care, but the full decNumber range seems very hard within the   */
/*    32-bit limits.						      */
/*								      */
/* 4. The working precisions for the static buffers are twice the     */
/*    obvious size to allow for calls from decNumberPower.	      */
/* ------------------------------------------------------------------ */
decNumber *decExpOp(decNumber *res, const decNumber *rhs,
                     decContext *set, uInt *status) {
    uInt ignore = 0;
    Int h;
    Int p;
    Int residue;
    uInt needbytes;
    const decNumber *x = rhs;
    decContext aset, tset, dset;
    Int comp;
    
    decNumber bufr[D2N(DECBUFFER*2+1)];
    decNumber *allocrhs = NULL;
    
    decNumber buft[D2N(DECBUFFER*2+9+1)];
    decNumber *allocbuft = NULL;
    decNumber *t = buft;
    
    decNumber bufa[D2N(DECBUFFER*4+18+1)];
    decNumber *allocbufa = NULL;
    decNumber *a = bufa;
    
    decNumber bufd[D2N(16)];
    decNumber *d = bufd;
    decNumber numone;
    
    #if DECCHECK
    Int iterations = 0;
    if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
    #endif
    
    if (SPECIALARG) {
        if (decNumberIsInfinite(rhs)) {
            if (decNumberIsNegative(rhs)) {
                decNumberZero(res);
            } else {
                decNumberCopy(res, rhs);
            }
        } else {
            decNaNs(res, rhs, NULL, set, status);
        }
        return res;
    }
    
    if (ISZERO(rhs)) {
        decNumberZero(res);
        *res->lsu = 1;
        return res;
    }
    
    decNumberZero(d);
    *d->lsu = 4;
    d->exponent = -set->digits;
    if (decNumberIsNegative(rhs)) {
        d->exponent--;
    }
    
    comp = decCompare(d, rhs, 1);
    if (comp == BADINT) {
        *status |= DEC_Insufficient_storage;
        return res;
    }
    
    if (comp >= 0) {
        Int shift = set->digits - 1;
        decNumberZero(res);
        *res->lsu = 1;
        res->digits = decShiftToMost(res->lsu, 1, shift);
        res->exponent = -shift;
        *status |= DEC_Inexact | DEC_Rounded;
        return res;
    }
    
    decContextDefault(&aset, DEC_INIT_DECIMAL64);
    aset.emax = set->emax;
    aset.emin = set->emin;
    aset.clamp = 0;
    
    h = rhs->exponent + rhs->digits;
    
    if (h > 8) {
        decNumberZero(a);
        *a->lsu = 2;
        if (decNumberIsNegative(rhs)) {
            a->exponent = -2;
        }
        h = 8;
        p = 9;
    } else {
        Int maxlever = (rhs->digits > 8 ? 1 : 0);
        Int lever = MINI(8 - h, maxlever);
        Int use = -rhs->digits - lever;
        h += lever;
        
        if (h < 0) {
            use += h;
            h = 0;
        }
        
        if (rhs->exponent != use) {
            decNumber *newrhs = bufr;
            needbytes = sizeof(decNumber) + (D2U(rhs->digits) - 1) * sizeof(Unit);
            
            if (needbytes > sizeof(bufr)) {
                allocrhs = (decNumber *)malloc(needbytes);
                if (allocrhs == NULL) {
                    *status |= DEC_Insufficient_storage;
                    goto cleanup;
                }
                newrhs = allocrhs;
            }
            
            decNumberCopy(newrhs, rhs);
            newrhs->exponent = use;
            x = newrhs;
        }
        
        p = MAXI(x->digits, set->digits) + h + 2;
        
        needbytes = sizeof(decNumber) + (D2U(p * 2) - 1) * sizeof(Unit);
        if (needbytes > sizeof(bufa)) {
            allocbufa = (decNumber *)malloc(needbytes);
            if (allocbufa == NULL) {
                *status |= DEC_Insufficient_storage;
                goto cleanup;
            }
            a = allocbufa;
        }
        
        needbytes = sizeof(decNumber) + (D2U(p + 2) - 1) * sizeof(Unit);
        if (needbytes > sizeof(buft)) {
            allocbuft = (decNumber *)malloc(needbytes);
            if (allocbuft == NULL) {
                *status |= DEC_Insufficient_storage;
                goto cleanup;
            }
            t = allocbuft;
        }
        
        decNumberCopy(t, x);
        decNumberZero(a);
        *a->lsu = 1;
        decNumberZero(d);
        *d->lsu = 2;
        decNumberZero(&numone);
        *numone.lsu = 1;
        
        decContextDefault(&tset, DEC_INIT_DECIMAL64);
        dset = tset;
        aset.digits = p * 2;
        tset.digits = p;
        tset.emin = DEC_MIN_EMIN;
        
        for (;;) {
            #if DECCHECK
            iterations++;
            #endif
            
            decAddOp(a, a, t, &aset, 0, status);
            decMultiplyOp(t, t, x, &tset, &ignore);
            decDivideOp(t, t, d, &tset, DIVIDE, &ignore);
            
            if (((a->digits + a->exponent) >= (t->digits + t->exponent + p + 1))
                && (a->digits >= p)) {
                break;
            }
            
            decAddOp(d, d, &numone, &dset, 0, &ignore);
        }
        
        #if DECCHECK
        if (iterations > p + 3) {
            printf("Exp iterations=%ld, status=%08lx, p=%ld, d=%ld\n",
                   (LI)iterations, (LI)*status, (LI)p, (LI)x->digits);
        }
        #endif
    }
    
    if (h > 0) {
        Int seenbit = 0;
        Int i;
        Int n = powers[h];
        aset.digits = p + 2;
        
        decNumberZero(t);
        *t->lsu = 1;
        
        for (i = 1; i <= 31; i++) {
            if (*status & (DEC_Overflow | DEC_Underflow)) {
                if (*status & DEC_Overflow || ISZERO(t)) {
                    break;
                }
            }
            
            n = n << 1;
            if (n < 0) {
                seenbit = 1;
                decMultiplyOp(t, t, a, &aset, status);
            }
            
            if (i == 31) {
                break;
            }
            
            if (seenbit) {
                decMultiplyOp(t, t, t, &aset, status);
            }
        }
        
        a = t;
    }
    
    residue = ISZERO(a) ? 0 : 1;
    aset.digits = set->digits;
    decCopyFit(res, a, &aset, &residue, status);
    decFinish(res, set, &residue, status);
    
cleanup:
    free(allocrhs);
    free(allocbufa);
    free(allocbuft);
    
    return res;
} /* decExpOp */

/* ------------------------------------------------------------------ */
/* Initial-estimate natural logarithm table			      */
/*								      */
/*   LNnn -- 90-entry 16-bit table for values from .10 through .99.   */
/*	     The result is a 4-digit encode of the coefficient (c=the */
/*	     top 14 bits encoding 0-9999) and a 2-digit encode of the */
/*	     exponent (e=the bottom 2 bits encoding 0-3)	      */
/*								      */
/*	     The resulting value is given by:			      */
/*								      */
/*	       v = -c * 10**(-e-3)				      */
/*								      */
/*	     where e and c are extracted from entry k = LNnn[x-10]    */
/*	     where x is truncated (NB) into the range 10 through 99,  */
/*	     and then c = k>>2 and e = k&3.			      */
/* ------------------------------------------------------------------ */
const uShort LNnn[90]={9016,  8652,  8316,  8008,  7724,  7456,  7208,
  6972,  6748,	6540,  6340,  6148,  5968,  5792,  5628,  5464,  5312,
  5164,  5020,	4884,  4748,  4620,  4496,  4376,  4256,  4144,  4032,
 39233, 38181, 37157, 36157, 35181, 34229, 33297, 32389, 31501, 30629,
 29777, 28945, 28129, 27329, 26545, 25777, 25021, 24281, 23553, 22837,
 22137, 21445, 20769, 20101, 19445, 18801, 18165, 17541, 16925, 16321,
 15721, 15133, 14553, 13985, 13421, 12865, 12317, 11777, 11241, 10717,
 10197,  9685,	9177,  8677,  8185,  7697,  7213,  6737,  6269,  5801,
  5341,  4889,	4437, 39930, 35534, 31186, 26886, 22630, 18418, 14254,
 10130,  6046, 20055};

/* ------------------------------------------------------------------ */
/* decLnOp -- effect natural logarithm				      */
/*								      */
/*   This computes C = ln(A)					      */
/*								      */
/*   res is C, the result.  C may be A				      */
/*   rhs is A							      */
/*   set is the context; note that rounding mode has no effect	      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Notable cases:						      */
/*   A<0 -> Invalid						      */
/*   A=0 -> -Infinity (Exact)					      */
/*   A=+Infinity -> +Infinity (Exact)				      */
/*   A=1 exactly -> 0 (Exact)					      */
/*								      */
/* Restrictions (as for Exp):					      */
/*								      */
/*   digits, emax, and -emin in the context must be less than	      */
/*   DEC_MAX_MATH+11 (1000010), and the rhs must be within these      */
/*   bounds or a zero.	This is an internal routine, so these	      */
/*   restrictions are contractual and not enforced.		      */
/*								      */
/* A finite result is rounded using DEC_ROUND_HALF_EVEN; it will      */
/* almost always be correctly rounded, but may be up to 1 ulp in      */
/* error in rare cases. 					      */
/* ------------------------------------------------------------------ */
/* The result is calculated using Newton's method, with each	      */
/* iteration calculating a' = a + x * exp(-a) - 1.  See, for example, */
/* Epperson 1989.						      */
/*								      */
/* The iteration ends when the adjustment x*exp(-a)-1 is tiny enough. */
/* This has to be calculated at the sum of the precision of x and the */
/* working precision.						      */
/*								      */
/* Implementation notes:					      */
/*								      */
/* 1. This is separated out as decLnOp so it can be called from       */
/*    other Mathematical functions (e.g., Log 10) with a wider range  */
/*    than normal.  In particular, it can handle the slightly wider   */
/*    (+9+2) range needed by a power function.			      */
/*								      */
/* 2. The speed of this function is about 10x slower than exp, as     */
/*    it typically needs 4-6 iterations for short numbers, and the    */
/*    extra precision needed adds a squaring effect, twice.	      */
/*								      */
/* 3. Fastpaths are included for ln(10) and ln(2), up to length 40,   */
/*    as these are common requests.  ln(10) is used by log10(x).      */
/*								      */
/* 4. An iteration might be saved by widening the LNnn table, and     */
/*    would certainly save at least one if it were made ten times     */
/*    bigger, too (for truncated fractions 0.100 through 0.999).      */
/*    However, for most practical evaluations, at least four or five  */
/*    iterations will be neede -- so this would only speed up by      */
/*    20-25% and that probably does not justify increasing the table  */
/*    size.							      */
/*								      */
/* 5. The static buffers are larger than might be expected to allow   */
/*    for calls from decNumberPower.				      */
/* ------------------------------------------------------------------ */
decNumber * decLnOp(decNumber *res, const decNumber *rhs,
                    decContext *set, uInt *status) {
  uInt ignore = 0;
  uInt needbytes;
  Int residue;
  Int r;
  Int p;
  Int pp;
  Int t;

  decNumber bufa[D2N(DECBUFFER+12)];
  decNumber *allocbufa = NULL;
  decNumber *a = bufa;
  decNumber bufb[D2N(DECBUFFER*2+2)];
  decNumber *allocbufb = NULL;
  decNumber *b = bufb;

  decNumber numone;
  decNumber cmp;
  decContext aset, bset;

  #if DECCHECK
  Int iterations = 0;
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  if (SPECIALARG) {
    if (decNumberIsInfinite(rhs)) {
      if (decNumberIsNegative(rhs))
        *status |= DEC_Invalid_operation;
      else
        decNumberCopy(res, rhs);
    }
    else {
      decNaNs(res, rhs, NULL, set, status);
    }
    return res;
  }

  if (ISZERO(rhs)) {
    decNumberZero(res);
    res->bits = DECINF | DECNEG;
    return res;
  }

  if (decNumberIsNegative(rhs)) {
    *status |= DEC_Invalid_operation;
    return res;
  }

  if (rhs->exponent == 0 && set->digits <= 40) {
    #if DECDPUN==1
    if (rhs->lsu[0] == 0 && rhs->lsu[1] == 1 && rhs->digits == 2) {
    #else
    if (rhs->lsu[0] == 10 && rhs->digits == 2) {
    #endif
      aset = *set;
      aset.round = DEC_ROUND_HALF_EVEN;
      #define LN10 "2.302585092994045684017991454684364207601"
      decNumberFromString(res, LN10, &aset);
      *status |= (DEC_Inexact | DEC_Rounded);
      return res;
    }
    if (rhs->lsu[0] == 2 && rhs->digits == 1) {
      aset = *set;
      aset.round = DEC_ROUND_HALF_EVEN;
      #define LN2 "0.6931471805599453094172321214581765680755"
      decNumberFromString(res, LN2, &aset);
      *status |= (DEC_Inexact | DEC_Rounded);
      return res;
    }
  }

  p = MAXI(rhs->digits, MAXI(set->digits, 7)) + 2;

  needbytes = sizeof(decNumber) + (D2U(MAXI(p, 16)) - 1) * sizeof(Unit);
  if (needbytes > sizeof(bufa)) {
    allocbufa = (decNumber *)malloc(needbytes);
    if (allocbufa == NULL) {
      *status |= DEC_Insufficient_storage;
      return res;
    }
    a = allocbufa;
  }

  pp = p + rhs->digits;
  needbytes = sizeof(decNumber) + (D2U(MAXI(pp, 16)) - 1) * sizeof(Unit);
  if (needbytes > sizeof(bufb)) {
    allocbufb = (decNumber *)malloc(needbytes);
    if (allocbufb == NULL) {
      *status |= DEC_Insufficient_storage;
      free(allocbufa);
      return res;
    }
    b = allocbufb;
  }

  decContextDefault(&aset, DEC_INIT_DECIMAL64);
  r = rhs->exponent + rhs->digits;
  decNumberFromInt32(a, r);
  decNumberFromInt32(b, 2302585);
  b->exponent = -6;
  decMultiplyOp(a, a, b, &aset, &ignore);

  residue = 0;
  aset.digits = 2;
  aset.round = DEC_ROUND_DOWN;
  decCopyFit(b, rhs, &aset, &residue, &ignore);
  b->exponent = 0;
  t = decGetInt(b);
  if (t < 10) t = X10(t);
  t = LNnn[t - 10];
  decNumberFromInt32(b, t >> 2);
  b->exponent = -(t & 3) - 3;
  b->bits = DECNEG;
  aset.digits = 16;
  aset.round = DEC_ROUND_HALF_EVEN;
  decAddOp(a, a, b, &aset, 0, &ignore);

  decNumberZero(&numone);
  *numone.lsu = 1;

  aset.emax = set->emax;
  aset.emin = set->emin;
  aset.clamp = 0;
  bset = aset;
  bset.emax = DEC_MAX_MATH * 2;
  bset.emin = -DEC_MAX_MATH * 2;

  pp = 9;
  aset.digits = pp;
  bset.digits = pp + rhs->digits;

  for (;;) {
    #if DECCHECK
    iterations++;
    if (iterations > 24) break;
    #endif

    a->bits ^= DECNEG;
    decExpOp(b, a, &bset, &ignore);
    a->bits ^= DECNEG;
    decMultiplyOp(b, b, rhs, &bset, &ignore);
    decAddOp(b, b, &numone, &bset, DECNEG, &ignore);

    if (decNumberIsZero(b) ||
        (a->digits + a->exponent) >= (b->digits + b->exponent + set->digits + 1)) {
      if (a->digits == p) break;
      if (decNumberIsZero(a)) {
        decCompareOp(&cmp, rhs, &numone, &aset, COMPARE, &ignore);
        if (cmp.lsu[0] == 0)
          a->exponent = 0;
        else
          *status |= (DEC_Inexact | DEC_Rounded);
        break;
      }
      if (decNumberIsZero(b))
        b->exponent = a->exponent - p;
    }

    decAddOp(a, a, b, &aset, 0, &ignore);
    if (pp == p) continue;
    pp = pp * 2;
    if (pp > p) pp = p;
    aset.digits = pp;
    bset.digits = pp + rhs->digits;
  }

  #if DECCHECK
  if (iterations > 24)
    printf("Ln iterations=%ld, status=%08lx, p=%ld, d=%ld\n",
          (LI)iterations, (LI)*status, (LI)p, (LI)rhs->digits);
  #endif

  residue = 1;
  if (ISZERO(a)) residue = 0;
  aset.digits = set->digits;
  decCopyFit(res, a, &aset, &residue, status);
  decFinish(res, set, &residue, status);

  free(allocbufa);
  free(allocbufb);
  return res;
} /* decLnOp */

/* ------------------------------------------------------------------ */
/* decQuantizeOp  -- force exponent to requested value		      */
/*								      */
/*   This computes C = op(A, B), where op adjusts the coefficient     */
/*   of C (by rounding or shifting) such that the exponent (-scale)   */
/*   of C has the value B or matches the exponent of B. 	      */
/*   The numerical value of C will equal A, except for the effects of */
/*   any rounding that occurred.				      */
/*								      */
/*   res is C, the result.  C may be A or B			      */
/*   lhs is A, the number to adjust				      */
/*   rhs is B, the requested exponent				      */
/*   set is the context 					      */
/*   quant is 1 for quantize or 0 for rescale			      */
/*   status is the status accumulator (this can be called without     */
/*	    risk of control loss)				      */
/*								      */
/* C must have space for set->digits digits.			      */
/*								      */
/* Unless there is an error or the result is infinite, the exponent   */
/* after the operation is guaranteed to be that requested.	      */
/* ------------------------------------------------------------------ */
static decNumber * decQuantizeOp(decNumber *res, const decNumber *lhs,
                                 const decNumber *rhs, decContext *set,
                                 Flag quant, uInt *status) {
  #if DECSUBSET
  decNumber *alloclhs=NULL;
  decNumber *allocrhs=NULL;
  #endif
  const decNumber *inrhs=rhs;
  Int reqdigits=set->digits;
  Int reqexp;
  Int residue=0;
  Int etiny=set->emin-(reqdigits-1);
  decNumber *result = res;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  #if DECSUBSET
  if (!set->extended) {
    if (lhs->digits>reqdigits) {
      alloclhs=decRoundOperand(lhs, set, status);
      if (alloclhs==NULL) goto cleanup;
      lhs=alloclhs;
    }
    if (rhs->digits>reqdigits) {
      allocrhs=decRoundOperand(rhs, set, status);
      if (allocrhs==NULL) goto cleanup;
      rhs=allocrhs;
    }
  }
  #endif

  if (SPECIALARGS) {
    if (SPECIALARGS & (DECSNAN | DECNAN))
      decNaNs(res, lhs, rhs, set, status);
    else if ((lhs->bits ^ rhs->bits) & DECINF)
      *status|=DEC_Invalid_operation;
    else 
      decNumberCopy(res, lhs);
    goto cleanup;
  }

  if (quant) {
    reqexp=inrhs->exponent;
  } else {
    reqexp=decGetInt(inrhs);
  }

  #if DECSUBSET
  if (!set->extended) etiny=set->emin;
  #endif

  if (reqexp==BADINT || reqexp==BIGODD || reqexp==BIGEVEN ||
      reqexp<etiny || reqexp>set->emax) {
    *status|=DEC_Invalid_operation;
    goto cleanup;
  }

  if (ISZERO(lhs)) {
    decNumberCopy(res, lhs);
    res->exponent=reqexp;
    #if DECSUBSET
    if (!set->extended) res->bits=0;
    #endif
  } else {
    Int adjust=reqexp-lhs->exponent;
    
    if ((lhs->digits-adjust)>reqdigits) {
      *status|=DEC_Invalid_operation;
      goto cleanup;
    }

    if (adjust>0) {
      decContext workset;
      workset=*set;
      workset.digits=lhs->digits-adjust;
      decCopyFit(res, lhs, &workset, &residue, status);
      decApplyRound(res, &workset, residue, status);
      residue=0;
      
      if (res->exponent>reqexp) {
        if (res->digits==reqdigits) {
          *status&=~(DEC_Inexact | DEC_Rounded);
          *status|=DEC_Invalid_operation;
          goto cleanup;
        }
        res->digits=decShiftToMost(res->lsu, res->digits, 1);
        res->exponent--;
      }
      #if DECSUBSET
      if (ISZERO(res) && !set->extended) res->bits=0;
      #endif
    } else {
      decNumberCopy(res, lhs);
      if (adjust<0) {
        res->digits=decShiftToMost(res->lsu, res->digits, -adjust);
        res->exponent+=adjust;
      }
    }
  }

  if (res->exponent>set->emax-res->digits+1) {
    *status|=DEC_Invalid_operation;
    goto cleanup;
  }
  
  decFinalize(res, set, &residue, status);
  *status&=~DEC_Underflow;

cleanup:
  #if DECSUBSET
  free(allocrhs);
  free(alloclhs);
  #endif
  return result;
} /* decQuantizeOp */

/* ------------------------------------------------------------------ */
/* decCompareOp -- compare, min, or max two Numbers		      */
/*								      */
/*   This computes C = A ? B and carries out one of four operations:  */
/*     COMPARE	  -- returns the signum (as a number) giving the      */
/*		     result of a comparison unless one or both	      */
/*		     operands is a NaN (in which case a NaN results)  */
/*     COMPSIG	  -- as COMPARE except that a quiet NaN raises	      */
/*		     Invalid operation. 			      */
/*     COMPMAX	  -- returns the larger of the operands, using the    */
/*		     754 maxnum operation			      */
/*     COMPMAXMAG -- ditto, comparing absolute values		      */
/*     COMPMIN	  -- the 754 minnum operation			      */
/*     COMPMINMAG -- ditto, comparing absolute values		      */
/*     COMTOTAL   -- returns the signum (as a number) giving the      */
/*		     result of a comparison using 754 total ordering  */
/*								      */
/*   res is C, the result.  C may be A and/or B (e.g., X=X?X)	      */
/*   lhs is A							      */
/*   rhs is B							      */
/*   set is the context 					      */
/*   op  is the operation flag					      */
/*   status is the usual accumulator				      */
/*								      */
/* C must have space for one digit for COMPARE or set->digits for     */
/* COMPMAX, COMPMIN, COMPMAXMAG, or COMPMINMAG. 		      */
/* ------------------------------------------------------------------ */
/* The emphasis here is on speed for common cases, and avoiding       */
/* coefficient comparison if possible.				      */
/* ------------------------------------------------------------------ */
decNumber * decCompareOp(decNumber *res, const decNumber *lhs,
			 const decNumber *rhs, decContext *set,
			 Flag op, uInt *status) {
  #if DECSUBSET
  decNumber *alloclhs=NULL;
  decNumber *allocrhs=NULL;
  #endif
  Int result=0;
  uByte merged;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  #if DECSUBSET
  if (!set->extended) {
    if (lhs->digits>set->digits) {
      alloclhs=decRoundOperand(lhs, set, status);
      if (alloclhs==NULL) {
        *status|=DEC_Insufficient_storage;
        return res;
      }
      lhs=alloclhs;
    }
    if (rhs->digits>set->digits) {
      allocrhs=decRoundOperand(rhs, set, status);
      if (allocrhs==NULL) {
        *status|=DEC_Insufficient_storage;
        free(alloclhs);
        return res;
      }
      rhs=allocrhs;
    }
  }
  #endif

  if (op==COMPTOTAL) {
    if (decNumberIsNegative(lhs) && !decNumberIsNegative(rhs)) {
      result=-1;
    }
    else if (!decNumberIsNegative(lhs) && decNumberIsNegative(rhs)) {
      result=+1;
    }
    else {
      result=0;
    }
  }

  merged=(lhs->bits | rhs->bits) & (DECSNAN | DECNAN);
  if (merged && result==0) {
    if (op==COMPARE) {
      op=COMPNAN;
      decNaNs(res, lhs, rhs, set, status);
    }
    else if (op==COMPSIG) {
      *status|=DEC_Invalid_operation | DEC_sNaN;
      op=COMPNAN;
      decNaNs(res, lhs, rhs, set, status);
    }
    else if (op==COMPTOTAL) {
      if (!decNumberIsNaN(lhs)) {
        result=-1;
      }
      else if (!decNumberIsNaN(rhs)) {
        result=+1;
      }
      else if (decNumberIsSNaN(lhs) && decNumberIsQNaN(rhs)) {
        result=-1;
      }
      else if (decNumberIsQNaN(lhs) && decNumberIsSNaN(rhs)) {
        result=+1;
      }
      else {
        result=decUnitCompare(lhs->lsu, D2U(lhs->digits),
                             rhs->lsu, D2U(rhs->digits), 0);
      }
      if (decNumberIsNegative(lhs)) result=-result;
    }
    else if (merged & DECSNAN) {
      op=COMPNAN;
      decNaNs(res, lhs, rhs, set, status);
    }
    else {
      if (!decNumberIsNaN(lhs) || !decNumberIsNaN(rhs)) {
        op=COMPMAX;
        if (lhs->bits & DECNAN) {
          result=-1;
        }
        else {
          result=+1;
        }
      }
      else {
        op=COMPNAN;
        decNaNs(res, lhs, rhs, set, status);
      }
    }
  }
  else if (result==0) {
    if (op==COMPMAXMAG || op==COMPMINMAG) {
      result=decCompare(lhs, rhs, 1);
    }
    else {
      result=decCompare(lhs, rhs, 0);
    }
  }

  if (op==COMPNAN) {
    #if DECSUBSET
    free(allocrhs);
    free(alloclhs);
    #endif
    return res;
  }

  if (op==COMPARE || op==COMPSIG || op==COMPTOTAL) {
    if (op==COMPTOTAL && result==0) {
      if (lhs->exponent!=rhs->exponent) {
        if (lhs->exponent<rhs->exponent) {
          result=-1;
        }
        else {
          result=+1;
        }
        if (decNumberIsNegative(lhs)) result=-result;
      }
    }
    decNumberZero(res);
    if (result!=0) {
      *res->lsu=1;
      if (result<0) res->bits=DECNEG;
    }
  }
  else {
    Int residue=0;
    const decNumber *choice;
    if (result==0) {
      uByte slhs=(lhs->bits & DECNEG);
      uByte srhs=(rhs->bits & DECNEG);
      #if DECSUBSET
      if (!set->extended) {
        op=COMPMAX;
        result=+1;
      }
      else
      #endif
      if (slhs!=srhs) {
        if (slhs) {
          result=-1;
        }
        else {
          result=+1;
        }
      }
      else if (slhs && srhs) {
        if (lhs->exponent<rhs->exponent) {
          result=+1;
        }
        else {
          result=-1;
        }
      }
      else {
        if (lhs->exponent>rhs->exponent) {
          result=+1;
        }
        else {
          result=-1;
        }
      }
    }
    if (op==COMPMIN || op==COMPMINMAG) result=-result;
    choice=(result>0 ? lhs : rhs);
    decCopyFit(res, choice, set, &residue, status);
    decFinish(res, set, &residue, status);
  }

  #if DECSUBSET
  free(allocrhs);
  free(alloclhs);
  #endif
  return res;
} /* decCompareOp */

/* ------------------------------------------------------------------ */
/* decCompare -- compare two decNumbers by numerical value	      */
/*								      */
/*  This routine compares A ? B without altering them.		      */
/*								      */
/*  Arg1 is A, a decNumber which is not a NaN			      */
/*  Arg2 is B, a decNumber which is not a NaN			      */
/*  Arg3 is 1 for a sign-independent compare, 0 otherwise	      */
/*								      */
/*  returns -1, 0, or 1 for A<B, A==B, or A>B, or BADINT if failure   */
/*  (the only possible failure is an allocation error)		      */
/* ------------------------------------------------------------------ */
static Int decCompare(const decNumber *lhs, const decNumber *rhs, Flag abs) {
    if (!lhs || !rhs) return BADINT;
    
    Int lhs_signum = ISZERO(lhs) ? 0 : (decNumberIsNegative(lhs) ? -1 : 1);
    Int rhs_signum = ISZERO(rhs) ? 0 : (decNumberIsNegative(rhs) ? -1 : 1);
    
    if (abs) {
        if (lhs_signum == 0 && rhs_signum == 0) return 0;
        if (lhs_signum == 0) return -1;
        if (rhs_signum == 0) return 1;
        lhs_signum = 1;
        rhs_signum = 1;
    }
    
    if (lhs_signum != rhs_signum) {
        if (lhs_signum > rhs_signum) return 1;
        if (lhs_signum < rhs_signum) return -1;
        return 0;
    }
    
    if (lhs_signum == 0) return 0;
    
    if ((lhs->bits | rhs->bits) & DECINF) {
        Flag lhs_inf = decNumberIsInfinite(lhs);
        Flag rhs_inf = decNumberIsInfinite(rhs);
        if (lhs_inf && rhs_inf) return 0;
        if (lhs_inf) return lhs_signum;
        if (rhs_inf) return -lhs_signum;
    }
    
    const decNumber *smaller_exp = lhs;
    const decNumber *larger_exp = rhs;
    Int sign_multiplier = lhs_signum;
    
    if (lhs->exponent > rhs->exponent) {
        smaller_exp = rhs;
        larger_exp = lhs;
        sign_multiplier = -sign_multiplier;
    }
    
    Int exp_diff = larger_exp->exponent - smaller_exp->exponent;
    Int compare = decUnitCompare(smaller_exp->lsu, D2U(smaller_exp->digits),
                                  larger_exp->lsu, D2U(larger_exp->digits),
                                  exp_diff);
    
    if (compare == BADINT) return BADINT;
    
    return compare * sign_multiplier;
} /* decCompare */

/* ------------------------------------------------------------------ */
/* decUnitCompare -- compare two >=0 integers in Unit arrays	      */
/*								      */
/*  This routine compares A ? B*10**E where A and B are unit arrays   */
/*  A is a plain integer					      */
/*  B has an exponent of E (which must be non-negative) 	      */
/*								      */
/*  Arg1 is A first Unit (lsu)					      */
/*  Arg2 is A length in Units					      */
/*  Arg3 is B first Unit (lsu)					      */
/*  Arg4 is B length in Units					      */
/*  Arg5 is E (0 if the units are aligned)			      */
/*								      */
/*  returns -1, 0, or 1 for A<B, A==B, or A>B, or BADINT if failure   */
/*  (the only possible failure is an allocation error, which can      */
/*  only occur if E!=0) 					      */
/* ------------------------------------------------------------------ */
static Int decUnitCompare(const Unit *a, Int alength,
                          const Unit *b, Int blength, Int exp) {
  Unit accbuff[SD2U(DECBUFFER*2+1)];
  Unit *acc = accbuff;
  Unit *allocacc = NULL;
  Int result;

  if (exp == 0) {
    if (alength != blength) {
      return (alength > blength) ? 1 : -1;
    }
    
    const Unit *l = a + alength - 1;
    const Unit *r = b + alength - 1;
    
    while (l >= a) {
      if (*l != *r) {
        return (*l > *r) ? 1 : -1;
      }
      l--;
      r--;
    }
    return 0;
  }

  Int exp_units = D2U(exp);
  if (alength > blength + exp_units) {
    return 1;
  }
  if (alength + 1 < blength + exp_units) {
    return -1;
  }

  Int need = blength + exp_units;
  if (need < alength) {
    need = alength;
  }
  need += 2;

  if (need * sizeof(Unit) > sizeof(accbuff)) {
    allocacc = (Unit *)malloc(need * sizeof(Unit));
    if (allocacc == NULL) {
      return BADINT;
    }
    acc = allocacc;
  }

  Int expunits = exp / DECDPUN;
  Int exprem = exp % DECDPUN;
  
  Int accunits = decUnitAddSub(a, alength, b, blength, expunits, acc,
                                -(Int)powers[exprem]);

  if (accunits < 0) {
    result = -1;
  } else {
    const Unit *u = acc;
    const Unit *end = acc + accunits - 1;
    
    while (u < end && *u == 0) {
      u++;
    }
    
    result = (*u == 0) ? 0 : 1;
  }

  if (allocacc != NULL) {
    free(allocacc);
  }
  
  return result;
} /* decUnitCompare */

/* ------------------------------------------------------------------ */
/* decUnitAddSub -- add or subtract two >=0 integers in Unit arrays   */
/*								      */
/*  This routine performs the calculation:			      */
/*								      */
/*  C=A+(B*M)							      */
/*								      */
/*  Where M is in the range -DECDPUNMAX through +DECDPUNMAX.	      */
/*								      */
/*  A may be shorter or longer than B.				      */
/*								      */
/*  Leading zeros are not removed after a calculation.	The result is */
/*  either the same length as the longer of A and B (adding any       */
/*  shift), or one Unit longer than that (if a Unit carry occurred).  */
/*								      */
/*  A and B content are not altered unless C is also A or B.	      */
/*  C may be the same array as A or B, but only if no zero padding is */
/*  requested (that is, C may be B only if bshift==0).		      */
/*  C is filled from the lsu; only those units necessary to complete  */
/*  the calculation are referenced.				      */
/*								      */
/*  Arg1 is A first Unit (lsu)					      */
/*  Arg2 is A length in Units					      */
/*  Arg3 is B first Unit (lsu)					      */
/*  Arg4 is B length in Units					      */
/*  Arg5 is B shift in Units  (>=0; pads with 0 units if positive)    */
/*  Arg6 is C first Unit (lsu)					      */
/*  Arg7 is M, the multiplier					      */
/*								      */
/*  returns the count of Units written to C, which will be non-zero   */
/*  and negated if the result is negative.  That is, the sign of the  */
/*  returned Int is the sign of the result (positive for zero) and    */
/*  the absolute value of the Int is the count of Units.	      */
/*								      */
/*  It is the caller's responsibility to make sure that C size is     */
/*  safe, allowing space if necessary for a one-Unit carry.	      */
/*								      */
/*  This routine is severely performance-critical; *any* change here  */
/*  must be measured (timed) to assure no performance degradation.    */
/*  In particular, trickery here tends to be counter-productive, as   */
/*  increased complexity of code hurts register optimizations on      */
/*  register-poor architectures.  Avoiding divisions is nearly	      */
/*  always a Good Idea, however.				      */
/*								      */
/* Special thanks to Rick McGuire (IBM Cambridge, MA) and Dave Clark  */
/* (IBM Warwick, UK) for some of the ideas used in this routine.      */
/* ------------------------------------------------------------------ */
static Int decUnitAddSub(const Unit *a, Int alength,
                         const Unit *b, Int blength, Int bshift,
                         Unit *c, Int m) {
  const Unit *alsu = a;
  Unit *clsu = c;
  Unit *minC;
  Unit *maxC;
  eInt carry = 0;
  Int add;
  #if DECDPUN <= 4
  Int est;
  #endif

  #if DECTRACE
  if (alength < 1 || blength < 1)
    printf("decUnitAddSub: alen blen m %ld %ld [%ld]\n", alength, blength, m);
  #endif

  maxC = c + alength;
  minC = c + blength;
  
  if (bshift != 0) {
    minC += bshift;
    if (a == c && bshift <= alength) {
      c += bshift;
      a += bshift;
    } else {
      for (; c < clsu + bshift; a++, c++) {
        *c = (a < alsu + alength) ? *a : 0;
      }
    }
  }
  
  if (minC > maxC) {
    Unit *hold = minC;
    minC = maxC;
    maxC = hold;
  }

  for (; c < minC; c++) {
    carry += *a++;
    carry += ((eInt)*b++) * m;
    
    if ((ueInt)carry <= DECDPUNMAX) {
      *c = (Unit)carry;
      carry = 0;
      continue;
    }
    
    #if DECDPUN == 4
    if (carry >= 0) {
      est = (((ueInt)carry >> 11) * 53687) >> 18;
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est;
      if (*c >= DECDPUNMAX + 1) {
        carry++;
        *c -= DECDPUNMAX + 1;
      }
    } else {
      carry += (eInt)(DECDPUNMAX + 1) * (DECDPUNMAX + 1);
      est = (((ueInt)carry >> 11) * 53687) >> 18;
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est - (DECDPUNMAX + 1);
      if (*c >= DECDPUNMAX + 1) {
        carry++;
        *c -= DECDPUNMAX + 1;
      }
    }
    #elif DECDPUN == 3
    if (carry >= 0) {
      est = (((ueInt)carry >> 3) * 16777) >> 21;
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est;
      if (*c >= DECDPUNMAX + 1) {
        carry++;
        *c -= DECDPUNMAX + 1;
      }
    } else {
      carry += (eInt)(DECDPUNMAX + 1) * (DECDPUNMAX + 1);
      est = (((ueInt)carry >> 3) * 16777) >> 21;
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est - (DECDPUNMAX + 1);
      if (*c >= DECDPUNMAX + 1) {
        carry++;
        *c -= DECDPUNMAX + 1;
      }
    }
    #elif DECDPUN <= 2
    if (carry >= 0) {
      est = QUOT10(carry, DECDPUN);
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est;
    } else {
      carry += (eInt)(DECDPUNMAX + 1) * (DECDPUNMAX + 1);
      est = QUOT10(carry, DECDPUN);
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est - (DECDPUNMAX + 1);
    }
    #else
    if ((ueInt)carry < (DECDPUNMAX + 1) * 2) {
      *c = (Unit)(carry - (DECDPUNMAX + 1));
      carry = 1;
    } else if (carry >= 0) {
      *c = (Unit)(carry % (DECDPUNMAX + 1));
      carry = carry / (DECDPUNMAX + 1);
    } else {
      carry += (eInt)(DECDPUNMAX + 1) * (DECDPUNMAX + 1);
      *c = (Unit)(carry % (DECDPUNMAX + 1));
      carry = carry / (DECDPUNMAX + 1) - (DECDPUNMAX + 1);
    }
    #endif
  }

  for (; c < maxC; c++) {
    if (a < alsu + alength) {
      carry += *a++;
    } else {
      carry += ((eInt)*b++) * m;
    }
    
    if ((ueInt)carry <= DECDPUNMAX) {
      *c = (Unit)carry;
      carry = 0;
      continue;
    }
    
    #if DECDPUN == 4
    if (carry >= 0) {
      est = (((ueInt)carry >> 11) * 53687) >> 18;
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est;
      if (*c >= DECDPUNMAX + 1) {
        carry++;
        *c -= DECDPUNMAX + 1;
      }
    } else {
      carry += (eInt)(DECDPUNMAX + 1) * (DECDPUNMAX + 1);
      est = (((ueInt)carry >> 11) * 53687) >> 18;
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est - (DECDPUNMAX + 1);
      if (*c >= DECDPUNMAX + 1) {
        carry++;
        *c -= DECDPUNMAX + 1;
      }
    }
    #elif DECDPUN == 3
    if (carry >= 0) {
      est = (((ueInt)carry >> 3) * 16777) >> 21;
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est;
      if (*c >= DECDPUNMAX + 1) {
        carry++;
        *c -= DECDPUNMAX + 1;
      }
    } else {
      carry += (eInt)(DECDPUNMAX + 1) * (DECDPUNMAX + 1);
      est = (((ueInt)carry >> 3) * 16777) >> 21;
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est - (DECDPUNMAX + 1);
      if (*c >= DECDPUNMAX + 1) {
        carry++;
        *c -= DECDPUNMAX + 1;
      }
    }
    #elif DECDPUN <= 2
    if (carry >= 0) {
      est = QUOT10(carry, DECDPUN);
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est;
    } else {
      carry += (eInt)(DECDPUNMAX + 1) * (DECDPUNMAX + 1);
      est = QUOT10(carry, DECDPUN);
      *c = (Unit)(carry - est * (DECDPUNMAX + 1));
      carry = est - (DECDPUNMAX + 1);
    }
    #else
    if ((ueInt)carry < (DECDPUNMAX + 1) * 2) {
      *c = (Unit)(carry - (DECDPUNMAX + 1));
      carry = 1;
    } else if (carry >= 0) {
      *c = (Unit)(carry % (DECDPUNMAX + 1));
      carry = carry / (DECDPUNMAX + 1);
    } else {
      carry += (eInt)(DECDPUNMAX + 1) * (DECDPUNMAX + 1);
      *c = (Unit)(carry % (DECDPUNMAX + 1));
      carry = carry / (DECDPUNMAX + 1) - (DECDPUNMAX + 1);
    }
    #endif
  }

  if (carry == 0) {
    return c - clsu;
  }
  
  if (carry > 0) {
    *c = (Unit)carry;
    c++;
    return c - clsu;
  }
  
  add = 1;
  for (c = clsu; c < maxC; c++) {
    add = DECDPUNMAX + add - *c;
    if (add <= DECDPUNMAX) {
      *c = (Unit)add;
      add = 0;
    } else {
      *c = 0;
      add = 1;
    }
  }
  
  #if DECTRACE
  printf("UAS borrow: add %ld, carry %ld\n", add, carry);
  #endif
  
  if ((add - carry - 1) != 0) {
    *c = (Unit)(add - carry - 1);
    c++;
  }
  
  return clsu - c;
} /* decUnitAddSub */

/* ------------------------------------------------------------------ */
/* decTrim -- trim trailing zeros or normalize			      */
/*								      */
/*   dn is the number to trim or normalize			      */
/*   set is the context to use to check for clamp		      */
/*   all is 1 to remove all trailing zeros, 0 for just fraction ones  */
/*   noclamp is 1 to unconditional (unclamped) trim		      */
/*   dropped returns the number of discarded trailing zeros	      */
/*   returns dn 						      */
/*								      */
/* If clamp is set in the context then the number of zeros trimmed    */
/* may be limited if the exponent is high.			      */
/* All fields are updated as required.	This is a utility operation,  */
/* so special values are unchanged and no error is possible.	      */
/* ------------------------------------------------------------------ */
static decNumber * decTrim(decNumber *dn, decContext *set, Flag all,
                           Flag noclamp, Int *dropped) {
  Int d, exp;
  uInt cut;
  Unit *up;

  #if DECCHECK
  if (decCheckOperands(dn, DECUNUSED, DECUNUSED, DECUNCONT)) return dn;
  #endif

  *dropped = 0;
  
  if ((dn->bits & DECSPECIAL) || (*dn->lsu & 0x01)) {
    return dn;
  }
  
  if (ISZERO(dn)) {
    dn->exponent = 0;
    return dn;
  }

  exp = dn->exponent;
  cut = 1;
  up = dn->lsu;
  
  for (d = 0; d < dn->digits - 1; d++) {
    #if DECDPUN <= 4
      uInt quot = QUOT10(*up, cut);
      if ((*up - quot * powers[cut]) != 0) break;
    #else
      if (*up % powers[cut] != 0) break;
    #endif
    
    if (!all && exp <= 0) {
      if (exp == 0) break;
      exp++;
    }
    
    cut++;
    if (cut > DECDPUN) {
      up++;
      cut = 1;
    }
  }
  
  if (d == 0) return dn;

  if (set->clamp && !noclamp) {
    Int maxd = set->emax - set->digits + 1 - dn->exponent;
    if (maxd <= 0) return dn;
    if (d > maxd) d = maxd;
  }

  decShiftToLeast(dn->lsu, D2U(dn->digits), d);
  dn->exponent += d;
  dn->digits -= d;
  *dropped = d;
  
  return dn;
} /* decTrim */

/* ------------------------------------------------------------------ */
/* decReverse -- reverse a Unit array in place			      */
/*								      */
/*   ulo    is the start of the array				      */
/*   uhi    is the end of the array (highest Unit to include)	      */
/*								      */
/* The units ulo through uhi are reversed in place (if the number     */
/* of units is odd, the middle one is untouched).  Note that the      */
/* digit(s) in each unit are unaffected.			      */
/* ------------------------------------------------------------------ */
static void decReverse(Unit *ulo, Unit *uhi) {
  while (ulo < uhi) {
    Unit temp = *ulo;
    *ulo = *uhi;
    *uhi = temp;
    ulo++;
    uhi--;
  }
} /* decReverse */

/* ------------------------------------------------------------------ */
/* decShiftToMost -- shift digits in array towards most significant   */
/*								      */
/*   uar    is the array					      */
/*   digits is the count of digits in use in the array		      */
/*   shift  is the number of zeros to pad with (least significant);   */
/*     it must be zero or positive				      */
/*								      */
/*   returns the new length of the integer in the array, in digits    */
/*								      */
/* No overflow is permitted (that is, the uar array must be known to  */
/* be large enough to hold the result, after shifting). 	      */
/* ------------------------------------------------------------------ */
static Int decShiftToMost(Unit *uar, Int digits, Int shift) {
  Unit *target, *source, *first;
  Int cut;
  uInt next;

  if (shift == 0) {
    return digits;
  }
  
  if ((digits + shift) <= DECDPUN) {
    *uar = (Unit)(*uar * powers[shift]);
    return digits + shift;
  }

  next = 0;
  source = uar + D2U(digits) - 1;
  target = source + D2U(shift);
  cut = DECDPUN - MSUDIGITS(shift);
  
  if (cut == 0) {
    while (source >= uar) {
      *target = *source;
      source--;
      target--;
    }
  } else {
    first = uar + D2U(digits + shift) - 1;
    while (source >= uar) {
      uInt rem;
      #if DECDPUN <= 4
        uInt quot = QUOT10(*source, cut);
        rem = *source - quot * powers[cut];
        next += quot;
      #else
        rem = *source % powers[cut];
        next += *source / powers[cut];
      #endif
      
      if (target <= first) {
        *target = (Unit)next;
      }
      next = rem * powers[DECDPUN - cut];
      source--;
      target--;
    }
  }

  while (target >= uar) {
    *target = (Unit)next;
    next = 0;
    target--;
  }
  
  return digits + shift;
} /* decShiftToMost */

/* ------------------------------------------------------------------ */
/* decShiftToLeast -- shift digits in array towards least significant */
/*								      */
/*   uar   is the array 					      */
/*   units is length of the array, in units			      */
/*   shift is the number of digits to remove from the lsu end; it     */
/*     must be zero or positive and <= than units*DECDPUN.	      */
/*								      */
/*   returns the new length of the integer in the array, in units     */
/*								      */
/* Removed digits are discarded (lost).  Units not required to hold   */
/* the final result are unchanged.				      */
/* ------------------------------------------------------------------ */
static Int decShiftToLeast(Unit *uar, Int units, Int shift) {
  Unit *target, *up;
  Int cut, count;
  Int quot, rem;

  if (shift == 0) {
    return units;
  }
  
  if (shift == units * DECDPUN) {
    *uar = 0;
    return 1;
  }

  target = uar;
  cut = MSUDIGITS(shift);
  
  if (cut == DECDPUN) {
    up = uar + D2U(shift);
    Int remaining = units - D2U(shift);
    for (Int i = 0; i < remaining; i++) {
      target[i] = up[i];
    }
    return remaining;
  }

  up = uar + D2U(shift - cut);
  count = units * DECDPUN - shift;
  
  #if DECDPUN <= 4
    quot = QUOT10(*up, cut);
  #else
    quot = *up / powers[cut];
  #endif
  
  while (1) {
    *target = (Unit)quot;
    count -= (DECDPUN - cut);
    
    if (count <= 0) {
      break;
    }
    
    up++;
    quot = *up;
    
    #if DECDPUN <= 4
      quot = QUOT10(quot, cut);
      rem = *up - quot * powers[cut];
    #else
      rem = quot % powers[cut];
      quot = quot / powers[cut];
    #endif
    
    *target = (Unit)(*target + rem * powers[DECDPUN - cut]);
    target++;
    count -= cut;
    
    if (count <= 0) {
      break;
    }
  }
  
  return target - uar + 1;
} /* decShiftToLeast */

#if DECSUBSET
/* ------------------------------------------------------------------ */
/* decRoundOperand -- round an operand	[used for subset only]	      */
/*								      */
/*   dn is the number to round (dn->digits is > set->digits)	      */
/*   set is the relevant context				      */
/*   status is the status accumulator				      */
/*								      */
/*   returns an allocated decNumber with the rounded result.	      */
/*								      */
/* lostDigits and other status may be set by this.		      */
/*								      */
/* Since the input is an operand, it must not be modified.	      */
/* Instead, return an allocated decNumber, rounded as required.       */
/* It is the caller's responsibility to free the allocated storage.   */
/*								      */
/* If no storage is available then the result cannot be used, so NULL */
/* is returned. 						      */
/* ------------------------------------------------------------------ */
static decNumber *decRoundOperand(const decNumber *dn, decContext *set,
				  uInt *status) {
  decNumber *res;			/* result structure */
  uInt newstatus=0;			/* status from round */
  Int  residue=0;			/* rounding accumulator */

  /* Allocate storage for the returned decNumber, big enough for the */
  /* length specified by the context */
  res=(decNumber *)malloc(sizeof(decNumber)
			  +(D2U(set->digits)-1)*sizeof(Unit));
  if (res==NULL) {
    *status|=DEC_Insufficient_storage;
    return NULL;
    }
  decCopyFit(res, dn, set, &residue, &newstatus);
  decApplyRound(res, set, residue, &newstatus);

  /* If that set Inexact then "lost digits" is raised... */
  if (newstatus & DEC_Inexact) newstatus|=DEC_Lost_digits;
  *status|=newstatus;
  return res;
  } /* decRoundOperand */
#endif

/* ------------------------------------------------------------------ */
/* decCopyFit -- copy a number, truncating the coefficient if needed  */
/*								      */
/*   dest is the target decNumber				      */
/*   src  is the source decNumber				      */
/*   set is the context [used for length (digits) and rounding mode]  */
/*   residue is the residue accumulator 			      */
/*   status contains the current status to be updated		      */
/*								      */
/* (dest==src is allowed and will be a no-op if fits)		      */
/* All fields are updated as required.				      */
/* ------------------------------------------------------------------ */
static void decCopyFit(decNumber *dest, const decNumber *src,
                       decContext *set, Int *residue, uInt *status) {
    if (dest == NULL || src == NULL || set == NULL || status == NULL) {
        return;
    }
    
    dest->bits = src->bits;
    dest->exponent = src->exponent;
    decSetCoeff(dest, set, src->lsu, src->digits, residue, status);
} /* decCopyFit */

/* ------------------------------------------------------------------ */
/* decSetCoeff -- set the coefficient of a number		      */
/*								      */
/*   dn    is the number whose coefficient array is to be set.	      */
/*	   It must have space for set->digits digits		      */
/*   set   is the context [for size]				      */
/*   lsu   -> lsu of the source coefficient [may be dn->lsu]	      */
/*   len   is digits in the source coefficient [may be dn->digits]    */
/*   residue is the residue accumulator.  This has values as in       */
/*	   decApplyRound, and will be unchanged unless the	      */
/*	   target size is less than len.  In this case, the	      */
/*	   coefficient is truncated and the residue is updated to     */
/*	   reflect the previous residue and the dropped digits.       */
/*   status is the status accumulator, as usual 		      */
/*								      */
/* The coefficient may already be in the number, or it can be an      */
/* external intermediate array.  If it is in the number, lsu must ==  */
/* dn->lsu and len must == dn->digits.				      */
/*								      */
/* Note that the coefficient length (len) may be < set->digits, and   */
/* in this case this merely copies the coefficient (or is a no-op     */
/* if dn->lsu==lsu).						      */
/*								      */
/* Note also that (only internally, from decQuantizeOp and	      */
/* decSetSubnormal) the value of set->digits may be less than one,    */
/* indicating a round to left.	This routine handles that case	      */
/* correctly; caller ensures space.				      */
/*								      */
/* dn->digits, dn->lsu (and as required), and dn->exponent are	      */
/* updated as necessary.   dn->bits (sign) is unchanged.	      */
/*								      */
/* DEC_Rounded status is set if any digits are discarded.	      */
/* DEC_Inexact status is set if any non-zero digits are discarded, or */
/*			 incoming residue was non-0 (implies rounded) */
/* ------------------------------------------------------------------ */
/* mapping array: maps 0-9 to canonical residues, so that a residue */
/* can be adjusted in the range [-1, +1] and achieve correct rounding */
/*			       0  1  2	3  4  5  6  7  8  9 */
static const uByte resmap[10]={0, 3, 3, 3, 3, 5, 7, 7, 7, 7};
static void decSetCoeff(decNumber *dn, decContext *set, const Unit *lsu,
			Int len, Int *residue, uInt *status) {
  Int discard = len - set->digits;
  
  if (discard <= 0) {
    if (dn->lsu != lsu) {
      Int count = len;
      const Unit *up = lsu;
      Unit *target = dn->lsu;
      while (count > 0) {
        *target++ = *up++;
        count -= DECDPUN;
      }
      dn->digits = len;
    }
    if (*residue != 0) {
      *status |= (DEC_Inexact | DEC_Rounded);
    }
    return;
  }

  dn->exponent += discard;
  *status |= DEC_Rounded;
  if (*residue > 1) {
    *residue = 1;
  }

  if (discard > len) {
    if (*residue <= 0) {
      Int count = len;
      const Unit *up = lsu;
      while (count > 0) {
        if (*up != 0) {
          *residue = 1;
          break;
        }
        up++;
        count -= DECDPUN;
      }
    }
    if (*residue != 0) {
      *status |= DEC_Inexact;
    }
    *dn->lsu = 0;
    dn->digits = 1;
    return;
  }

  Int count = 0;
  const Unit *up = lsu;
  while (count < discard) {
    if (count + DECDPUN >= discard) {
      break;
    }
    if (*up != 0) {
      *residue = 1;
    }
    up++;
    count += DECDPUN;
  }

  uInt cut = discard - (count - DECDPUN) - 1;
  
  if (cut == DECDPUN - 1) {
    Unit half = (Unit)powers[DECDPUN] >> 1;
    if (*up >= half) {
      *residue = (*up > half) ? 7 : (*residue + 5);
    } else if (*up != 0) {
      *residue = 3;
    }
    
    if (set->digits <= 0) {
      *dn->lsu = 0;
      dn->digits = 1;
    } else {
      count = set->digits;
      dn->digits = count;
      up++;
      Unit *target = dn->lsu;
      while (count > 0) {
        *target++ = *up++;
        count -= DECDPUN;
      }
    }
  } else {
    uInt quot, rem;
    
    if (cut == 0) {
      quot = *up;
    } else {
      #if DECDPUN <= 4
        quot = QUOT10(*up, cut);
        rem = *up - quot * powers[cut];
      #else
        rem = *up % powers[cut];
        quot = *up / powers[cut];
      #endif
      if (rem != 0) {
        *residue = 1;
      }
    }
    
    #if DECDPUN <= 4
      uInt temp = (quot * 6554) >> 16;
      uInt discard1 = quot - X10(temp);
      quot = temp;
    #else
      uInt discard1 = quot % 10;
      quot = quot / 10;
    #endif
    
    *residue += resmap[discard1];
    cut++;
    
    if (set->digits <= 0) {
      *dn->lsu = 0;
      dn->digits = 1;
    } else {
      count = set->digits;
      dn->digits = count;
      Unit *target = dn->lsu;
      
      while (1) {
        *target = (Unit)quot;
        count -= (DECDPUN - cut);
        if (count <= 0) {
          break;
        }
        up++;
        quot = *up;
        
        #if DECDPUN <= 4
          quot = QUOT10(quot, cut);
          rem = *up - quot * powers[cut];
        #else
          rem = quot % powers[cut];
          quot = quot / powers[cut];
        #endif
        
        *target = (Unit)(*target + rem * powers[DECDPUN - cut]);
        target++;
        count -= cut;
        if (count <= 0) {
          break;
        }
      }
    }
  }

  if (*residue != 0) {
    *status |= DEC_Inexact;
  }
} /* decSetCoeff */

/* ------------------------------------------------------------------ */
/* decApplyRound -- apply pending rounding to a number		      */
/*								      */
/*   dn    is the number, with space for set->digits digits	      */
/*   set   is the context [for size and rounding mode]		      */
/*   residue indicates pending rounding, being any accumulated	      */
/*	   guard and sticky information.  It may be:		      */
/*	   6-9: rounding digit is >5				      */
/*	   5:	rounding digit is exactly half-way		      */
/*	   1-4: rounding digit is <5 and >0			      */
/*	   0:	the coefficient is exact			      */
/*	  -1:	as 1, but the hidden digits are subtractive, that     */
/*		is, of the opposite sign to dn.  In this case the     */
/*		coefficient must be non-0.  This case occurs when     */
/*		subtracting a small number (which can be reduced to   */
/*		a sticky bit); see decAddOp.			      */
/*   status is the status accumulator, as usual 		      */
/*								      */
/* This routine applies rounding while keeping the length of the      */
/* coefficient constant.  The exponent and status are unchanged       */
/* except if:							      */
/*								      */
/*   -- the coefficient was increased and is all nines (in which      */
/*	case Overflow could occur, and is handled directly here so    */
/*	the caller does not need to re-test for overflow)	      */
/*								      */
/*   -- the coefficient was decreased and becomes all nines (in which */
/*	case Underflow could occur, and is also handled directly).    */
/*								      */
/* All fields in dn are updated as required.			      */
/*								      */
/* ------------------------------------------------------------------ */
static void decApplyRound(decNumber *dn, decContext *set, Int residue, uInt *status) {
  if (residue == 0) return;

  Int bump = 0;

  switch (set->round) {
    case DEC_ROUND_05UP: {
      Int lsd5 = *dn->lsu % 5;
      if (residue < 0 && lsd5 != 1) {
        bump = -1;
      } else if (residue > 0 && lsd5 == 0) {
        bump = 1;
      }
      break;
    }

    case DEC_ROUND_DOWN:
      if (residue < 0) bump = -1;
      break;

    case DEC_ROUND_HALF_DOWN:
      if (residue > 5) bump = 1;
      break;

    case DEC_ROUND_HALF_EVEN:
      if (residue > 5) {
        bump = 1;
      } else if (residue == 5 && (*dn->lsu & 0x01)) {
        bump = 1;
      }
      break;

    case DEC_ROUND_HALF_UP:
      if (residue >= 5) bump = 1;
      break;

    case DEC_ROUND_UP:
      if (residue > 0) bump = 1;
      break;

    case DEC_ROUND_CEILING:
      if (decNumberIsNegative(dn)) {
        if (residue < 0) bump = -1;
      } else {
        if (residue > 0) bump = 1;
      }
      break;

    case DEC_ROUND_FLOOR:
      if (!decNumberIsNegative(dn)) {
        if (residue < 0) bump = -1;
      } else {
        if (residue > 0) bump = 1;
      }
      break;

    default:
      *status |= DEC_Invalid_context;
      #if DECTRACE || (DECCHECK && DECVERB)
      printf("Unknown rounding mode: %d\n", set->round);
      #endif
      return;
  }

  if (bump == 0) return;

  if (bump > 0) {
    Unit *up = dn->lsu;
    uInt count = dn->digits;
    
    while (1) {
      if (count <= DECDPUN) {
        if (*up != powers[count] - 1) break;
        
        *up = (Unit)powers[count - 1];
        for (Unit *p = up - 1; p >= dn->lsu; p--) {
          *p = 0;
        }
        dn->exponent++;
        
        if ((dn->exponent + dn->digits) > set->emax + 1) {
          decSetOverflow(dn, set, status);
        }
        return;
      }
      
      if (*up != DECDPUNMAX) break;
      count -= DECDPUN;
      up++;
    }
  } else {
    Unit *up = dn->lsu;
    Unit *sup = NULL;
    uInt count = dn->digits;
    
    while (1) {
      if (count <= DECDPUN) {
        if (*up != powers[count - 1]) break;
        
        sup = up;
        *up = (Unit)powers[count] - 1;
        
        for (Unit *p = up - 1; p >= dn->lsu; p--) {
          *p = (Unit)powers[DECDPUN] - 1;
        }
        dn->exponent--;
        
        if (dn->exponent + 1 == set->emin - set->digits + 1) {
          if (count == 1 && dn->digits == 1) {
            *sup = 0;
          } else {
            *sup = (Unit)powers[count - 1] - 1;
            dn->digits--;
          }
          dn->exponent++;
          *status |= DEC_Underflow | DEC_Subnormal | DEC_Inexact | DEC_Rounded;
        }
        return;
      }
      
      if (*up != 0) break;
      count -= DECDPUN;
      up++;
    }
  }

  decUnitAddSub(dn->lsu, D2U(dn->digits), uarrone, 1, 0, dn->lsu, bump);
} /* decApplyRound */

#if DECSUBSET
/* ------------------------------------------------------------------ */
/* decFinish -- finish processing a number			      */
/*								      */
/*   dn is the number						      */
/*   set is the context 					      */
/*   residue is the rounding accumulator (as in decApplyRound)	      */
/*   status is the accumulator					      */
/*								      */
/* This finishes off the current number by:			      */
/*    1. If not extended:					      */
/*	 a. Converting a zero result to clean '0'		      */
/*	 b. Reducing positive exponents to 0, if would fit in digits  */
/*    2. Checking for overflow and subnormals (always)		      */
/* Note this is just Finalize when no subset arithmetic.	      */
/* All fields are updated as required.				      */
/* ------------------------------------------------------------------ */
static void decFinish(decNumber *dn, decContext *set, Int *residue,
		      uInt *status) {
  if (!set->extended) {
    if ISZERO(dn) {		   /* value is zero */
      dn->exponent=0;		   /* clean exponent .. */
      dn->bits=0;		   /* .. and sign */
      return;			   /* no error possible */
      }
    if (dn->exponent>=0) {	   /* non-negative exponent */
      /* >0; reduce to integer if possible */
      if (set->digits >= (dn->exponent+dn->digits)) {
	dn->digits=decShiftToMost(dn->lsu, dn->digits, dn->exponent);
	dn->exponent=0;
	}
      }
    } /* !extended */

  decFinalize(dn, set, residue, status);
  } /* decFinish */
#endif

/* ------------------------------------------------------------------ */
/* decFinalize -- final check, clamp, and round of a number	      */
/*								      */
/*   dn is the number						      */
/*   set is the context 					      */
/*   residue is the rounding accumulator (as in decApplyRound)	      */
/*   status is the status accumulator				      */
/*								      */
/* This finishes off the current number by checking for subnormal     */
/* results, applying any pending rounding, checking for overflow,     */
/* and applying any clamping.					      */
/* Underflow and overflow conditions are raised as appropriate.       */
/* All fields are updated as required.				      */
/* ------------------------------------------------------------------ */
static void decFinalize(decNumber *dn, decContext *set, Int *residue, uInt *status) {
    Int tinyexp = set->emin - dn->digits + 1;
    Int maxexp = set->emax - set->digits + 1;
    
    if (dn->exponent <= tinyexp) {
        if (dn->exponent < tinyexp) {
            decSetSubnormal(dn, set, residue, status);
            return;
        }
        
        decNumber nmin;
        decNumberZero(&nmin);
        nmin.lsu[0] = 1;
        nmin.exponent = set->emin;
        
        Int comp = decCompare(dn, &nmin, 1);
        if (comp == BADINT) {
            *status |= DEC_Insufficient_storage;
            return;
        }
        
        if (*residue < 0 && comp == 0) {
            decApplyRound(dn, set, *residue, status);
            decSetSubnormal(dn, set, residue, status);
            return;
        }
    }
    
    if (*residue != 0) {
        decApplyRound(dn, set, *residue, status);
    }
    
    if (dn->exponent <= maxexp) {
        return;
    }
    
    if (dn->exponent > set->emax - dn->digits + 1) {
        decSetOverflow(dn, set, status);
        return;
    }
    
    if (!set->clamp) {
        return;
    }
    
    Int shift = dn->exponent - maxexp;
    
    if (!ISZERO(dn)) {
        dn->digits = decShiftToMost(dn->lsu, dn->digits, shift);
    }
    
    dn->exponent -= shift;
    *status |= DEC_Clamped;
} /* decFinalize */

/* ------------------------------------------------------------------ */
/* decSetOverflow -- set number to proper overflow value	      */
/*								      */
/*   dn is the number (used for sign [only] and result) 	      */
/*   set is the context [used for the rounding mode, etc.]	      */
/*   status contains the current status to be updated		      */
/*								      */
/* This sets the sign of a number and sets its value to either	      */
/* Infinity or the maximum finite value, depending on the sign of     */
/* dn and the rounding mode, following IEEE 754 rules.		      */
/* ------------------------------------------------------------------ */
static void decSetOverflow(decNumber *dn, decContext *set, uInt *status) {
  uByte sign = dn->bits & DECNEG;
  Flag needmax = 0;

  if (ISZERO(dn)) {
    Int emax = set->emax;
    if (set->clamp) {
      emax -= set->digits - 1;
    }
    if (dn->exponent > emax) {
      dn->exponent = emax;
      *status |= DEC_Clamped;
    }
    return;
  }

  decNumberZero(dn);
  
  if (set->round == DEC_ROUND_DOWN || set->round == DEC_ROUND_05UP) {
    needmax = 1;
  } else if (set->round == DEC_ROUND_CEILING && sign) {
    needmax = 1;
  } else if (set->round == DEC_ROUND_FLOOR && !sign) {
    needmax = 1;
  }
  
  if (needmax) {
    decSetMaxValue(dn, set);
    dn->bits = sign;
  } else {
    dn->bits = sign | DECINF;
  }
  
  *status |= DEC_Overflow | DEC_Inexact | DEC_Rounded;
} /* decSetOverflow */

/* ------------------------------------------------------------------ */
/* decSetMaxValue -- set number to +Nmax (maximum normal value)       */
/*								      */
/*   dn is the number to set					      */
/*   set is the context [used for digits and emax]		      */
/*								      */
/* This sets the number to the maximum positive value.		      */
/* ------------------------------------------------------------------ */
static void decSetMaxValue(decNumber *dn, decContext *set) {
  Unit *up;
  Int count = set->digits;
  
  dn->digits = count;
  dn->bits = 0;
  dn->exponent = set->emax - set->digits + 1;
  
  up = dn->lsu;
  while (count > DECDPUN) {
    *up = DECDPUNMAX;
    up++;
    count -= DECDPUN;
  }
  
  *up = (Unit)(powers[count] - 1);
} /* decSetMaxValue */

/* ------------------------------------------------------------------ */
/* decSetSubnormal -- process value whose exponent is <Emin	      */
/*								      */
/*   dn is the number (used as input as well as output; it may have   */
/*	   an allowed subnormal value, which may need to be rounded)  */
/*   set is the context [used for the rounding mode]		      */
/*   residue is any pending residue				      */
/*   status contains the current status to be updated		      */
/*								      */
/* If subset mode, set result to zero and set Underflow flags.	      */
/*								      */
/* Value may be zero with a low exponent; this does not set Subnormal */
/* but the exponent will be clamped to Etiny.			      */
/*								      */
/* Otherwise ensure exponent is not out of range, and round as	      */
/* necessary.  Underflow is set if the result is Inexact.	      */
/* ------------------------------------------------------------------ */
static void decSetSubnormal(decNumber *dn, decContext *set, Int *residue,
                            uInt *status) {
    decContext workset;
    Int etiny, adjust;

    #if DECSUBSET
    if (!set->extended) {
        decNumberZero(dn);
        *status |= DEC_Underflow | DEC_Subnormal | DEC_Inexact | DEC_Rounded;
        return;
    }
    #endif

    etiny = set->emin - (set->digits - 1);

    if (ISZERO(dn)) {
        #if DECCHECK
        if (*residue != 0) {
            printf("++ Subnormal 0 residue %ld\n", (LI)*residue);
            *status |= DEC_Invalid_operation;
        }
        #endif
        if (dn->exponent < etiny) {
            dn->exponent = etiny;
            *status |= DEC_Clamped;
        }
        return;
    }

    *status |= DEC_Subnormal;
    adjust = etiny - dn->exponent;
    
    if (adjust <= 0) {
        if (*status & DEC_Inexact) {
            *status |= DEC_Underflow;
        }
        return;
    }

    workset = *set;
    workset.digits = dn->digits - adjust;
    workset.emin -= adjust;
    
    decSetCoeff(dn, &workset, dn->lsu, dn->digits, residue, status);
    decApplyRound(dn, &workset, *residue, status);

    if (*status & DEC_Inexact) {
        *status |= DEC_Underflow;
    }

    if (dn->exponent > etiny) {
        dn->digits = decShiftToMost(dn->lsu, dn->digits, 1);
        dn->exponent--;
    }

    if (ISZERO(dn)) {
        *status |= DEC_Clamped;
    }
} /* decSetSubnormal */

/* ------------------------------------------------------------------ */
/* decCheckMath - check entry conditions for a math function	      */
/*								      */
/*   This checks the context and the operand			      */
/*								      */
/*   rhs is the operand to check				      */
/*   set is the context to check				      */
/*   status is unchanged if both are good			      */
/*								      */
/* returns non-zero if status is changed, 0 otherwise		      */
/*								      */
/* Restrictions enforced:					      */
/*								      */
/*   digits, emax, and -emin in the context must be less than	      */
/*   DEC_MAX_MATH (999999), and A must be within these bounds if      */
/*   non-zero.	Invalid_operation is set in the status if a	      */
/*   restriction is violated.					      */
/* ------------------------------------------------------------------ */
static uInt decCheckMath(const decNumber *rhs, decContext *set, uInt *status) {
  uInt save = *status;
  
  if (set == NULL || rhs == NULL || status == NULL) {
    return 0;
  }
  
  if (set->digits > DEC_MAX_MATH || 
      set->emax > DEC_MAX_MATH || 
      -set->emin > DEC_MAX_MATH) {
    *status |= DEC_Invalid_context;
    return 1;
  }
  
  if (ISZERO(rhs)) {
    return 0;
  }
  
  Int exponentPlusDigits = rhs->exponent + rhs->digits;
  Int minBound = 2 * (1 - DEC_MAX_MATH);
  
  if (rhs->digits > DEC_MAX_MATH || 
      exponentPlusDigits > DEC_MAX_MATH + 1 || 
      exponentPlusDigits < minBound) {
    *status |= DEC_Invalid_operation;
    return 1;
  }
  
  return 0;
} /* decCheckMath */

/* ------------------------------------------------------------------ */
/* decGetInt -- get integer from a number			      */
/*								      */
/*   dn is the number [which will not be altered]		      */
/*								      */
/*   returns one of:						      */
/*     BADINT if there is a non-zero fraction			      */
/*     the converted integer					      */
/*     BIGEVEN if the integer is even and magnitude > 2*10**9	      */
/*     BIGODD  if the integer is odd  and magnitude > 2*10**9	      */
/*								      */
/* This checks and gets a whole number from the input decNumber.      */
/* The sign can be determined from dn by the caller when BIGEVEN or   */
/* BIGODD is returned.						      */
/* ------------------------------------------------------------------ */
static Int decGetInt(const decNumber *dn) {
  #if DEC_MAX_EMAX > 999999999
    #error GetInt may need updating [for Emax]
  #endif
  #if DEC_MIN_EMIN < -999999999
    #error GetInt may need updating [for Emin]
  #endif

  if (ISZERO(dn)) {
    return 0;
  }

  Int ilength = dn->digits + dn->exponent;
  Flag neg = decNumberIsNegative(dn);
  const Unit *up = dn->lsu;
  Int theInt = 0;
  Int got = 0;

  if (dn->exponent < 0) {
    Int discardCount = -dn->exponent;
    
    while (discardCount >= DECDPUN) {
      if (*up != 0) {
        return BADINT;
      }
      up++;
      discardCount -= DECDPUN;
    }
    
    if (discardCount > 0) {
      Int rem;
      #if DECDPUN <= 4
        theInt = QUOT10(*up, discardCount);
        rem = *up - theInt * powers[discardCount];
      #else
        rem = *up % powers[discardCount];
        theInt = *up / powers[discardCount];
      #endif
      
      if (rem != 0) {
        return BADINT;
      }
      
      got = DECDPUN - discardCount;
      up++;
    }
  } else {
    got = dn->exponent;
  }

  if (got == 0) {
    theInt = *up;
    got += DECDPUN;
    up++;
  }

  if (ilength < 11) {
    Int save = theInt;
    
    while (got < ilength) {
      theInt += *up * powers[got];
      got += DECDPUN;
      up++;
    }
    
    if (ilength == 10) {
      Int lastUnit = (Int)*(up - 1);
      Int quotient = theInt / (Int)powers[got - DECDPUN];
      
      if (quotient != lastUnit) {
        ilength = 11;
      } else if (neg && theInt > 1999999997) {
        ilength = 11;
      } else if (!neg && theInt > 999999999) {
        ilength = 11;
      }
      
      if (ilength == 11) {
        theInt = save;
      }
    }
  }

  if (ilength > 10) {
    return (theInt & 1) ? BIGODD : BIGEVEN;
  }

  return neg ? -theInt : theInt;
} /* decGetInt */

/* ------------------------------------------------------------------ */
/* decDecap -- decapitate the coefficient of a number		      */
/*								      */
/*   dn   is the number to be decapitated			      */
/*   drop is the number of digits to be removed from the left of dn;  */
/*     this must be <= dn->digits (if equal, the coefficient is       */
/*     set to 0)						      */
/*								      */
/* Returns dn; dn->digits will be <= the initial digits less drop     */
/* (after removing drop digits there may be leading zero digits       */
/* which will also be removed).  Only dn->lsu and dn->digits change.  */
/* ------------------------------------------------------------------ */
static decNumber *decDecap(decNumber *dn, Int drop) {
  Unit *msu;
  Int cut;
  
  if (drop >= dn->digits) {
    dn->lsu[0] = 0;
    dn->digits = 1;
    return dn;
  }
  
  msu = dn->lsu + D2U(dn->digits - drop) - 1;
  cut = MSUDIGITS(dn->digits - drop);
  
  if (cut != DECDPUN) {
    *msu %= powers[cut];
  }
  
  dn->digits = decGetDigits(dn->lsu, msu - dn->lsu + 1);
  return dn;
} /* decDecap */

/* ------------------------------------------------------------------ */
/* decBiStr -- compare string with pairwise options		      */
/*								      */
/*   targ is the string to compare				      */
/*   str1 is one of the strings to compare against (length may be 0)  */
/*   str2 is the other; it must be the same length as str1	      */
/*								      */
/*   returns 1 if strings compare equal, (that is, it is the same     */
/*   length as str1 and str2, and each character of targ is in either */
/*   str1 or str2 in the corresponding position), or 0 otherwise      */
/*								      */
/* This is used for generic caseless compare, including the awkward   */
/* case of the Turkish dotted and dotless Is.  Use as (for example):  */
/*   if (decBiStr(test, "mike", "MIKE")) ...			      */
/* ------------------------------------------------------------------ */
static Flag decBiStr(const char *targ, const char *str1, const char *str2) {
  if (targ == NULL || str1 == NULL || str2 == NULL) {
    return 0;
  }
  
  size_t i = 0;
  while (targ[i] != '\0') {
    if (targ[i] != str1[i] && targ[i] != str2[i]) {
      return 0;
    }
    i++;
  }
  
  return 1;
} /* decBiStr */

/* ------------------------------------------------------------------ */
/* decNaNs -- handle NaN operand or operands			      */
/*								      */
/*   res     is the result number				      */
/*   lhs     is the first operand				      */
/*   rhs     is the second operand, or NULL if none		      */
/*   context is used to limit payload length			      */
/*   status  contains the current status			      */
/*   returns res in case convenient				      */
/*								      */
/* Called when one or both operands is a NaN, and propagates the      */
/* appropriate result to res.  When an sNaN is found, it is changed   */
/* to a qNaN and Invalid operation is set.			      */
/* ------------------------------------------------------------------ */
static decNumber * decNaNs(decNumber *res, const decNumber *lhs,
                           const decNumber *rhs, decContext *set,
                           uInt *status) {
    const decNumber *source = lhs;
    
    if (lhs->bits & DECSNAN) {
        *status |= DEC_Invalid_operation | DEC_sNaN;
    } else if (rhs != NULL) {
        if (rhs->bits & DECSNAN) {
            source = rhs;
            *status |= DEC_Invalid_operation | DEC_sNaN;
        } else if (!(lhs->bits & DECNAN)) {
            source = rhs;
        }
    }
    
    if (source->digits <= set->digits) {
        decNumberCopy(res, source);
    } else {
        res->bits = source->bits;
        
        Unit *dest = res->lsu;
        const Unit *src = source->lsu;
        Unit *dest_limit = res->lsu + D2U(set->digits);
        
        while (dest < dest_limit) {
            *dest++ = *src++;
        }
        
        res->digits = D2U(set->digits) * DECDPUN;
        
        if (res->digits > set->digits) {
            decDecap(res, res->digits - set->digits);
        }
    }
    
    res->bits = (res->bits & ~DECSNAN) | DECNAN;
    res->exponent = 0;
    
    return res;
} /* decNaNs */

/* ------------------------------------------------------------------ */
/* decStatus -- apply non-zero status				      */
/*								      */
/*   dn     is the number to set if error			      */
/*   status contains the current status (not yet in context)	      */
/*   set    is the context					      */
/*								      */
/* If the status is an error status, the number is set to a NaN,      */
/* unless the error was an overflow, divide-by-zero, or underflow,    */
/* in which case the number will have already been set. 	      */
/*								      */
/* The context status is then updated with the new status.  Note that */
/* this may raise a signal, so control may never return from this     */
/* routine (hence resources must be recovered before it is called).   */
/* ------------------------------------------------------------------ */
static void decStatus(decNumber *dn, uInt status, decContext *set) {
  if ((status & DEC_NaNs) != 0) {
    if ((status & DEC_sNaN) != 0) {
      status &= ~DEC_sNaN;
    } else {
      decNumberZero(dn);
      dn->bits = DECNAN;
    }
  }
  decContextSetStatus(set, status);
} /* decStatus */

/* ------------------------------------------------------------------ */
/* decGetDigits -- count digits in a Units array		      */
/*								      */
/*   uar is the Unit array holding the number (this is often an       */
/*	    accumulator of some sort)				      */
/*   len is the length of the array in units [>=1]		      */
/*								      */
/*   returns the number of (significant) digits in the array	      */
/*								      */
/* All leading zeros are excluded, except the last if the array has   */
/* only zero Units.						      */
/* ------------------------------------------------------------------ */
/* This may be called twice during some operations. */
static Int decGetDigits(Unit *uar, Int len) {
  if (len < 1) {
    return 0;
  }
  
  Unit *up = uar + (len - 1);
  Int digits = (len - 1) * DECDPUN + 1;
  
  while (up >= uar) {
    if (*up == 0) {
      if (digits == 1) {
        break;
      }
      digits -= DECDPUN;
      up--;
      continue;
    }
    
    #if DECDPUN > 1
    if (*up < 10) {
      break;
    }
    digits++;
    #endif
    
    #if DECDPUN > 2
    if (*up < 100) {
      break;
    }
    digits++;
    #endif
    
    #if DECDPUN > 3
    if (*up < 1000) {
      break;
    }
    digits++;
    #endif
    
    #if DECDPUN > 4
    uInt const *pow = &powers[4];
    while (*up >= *pow) {
      digits++;
      pow++;
    }
    #endif
    
    break;
  }
  
  return digits;
} /* decGetDigits */

#if DECTRACE | DECCHECK
/* ------------------------------------------------------------------ */
/* decNumberShow -- display a number [debug aid]		      */
/*   dn is the number to show					      */
/*								      */
/* Shows: sign, exponent, coefficient (msu first), digits	      */
/*    or: sign, special-value					      */
/* ------------------------------------------------------------------ */
/* this is public so other modules can use it */
void decNumberShow(const decNumber *dn) {
  const Unit *up;		   /* work */
  uInt u, d;			   /* .. */
  Int cut;			   /* .. */
  char isign='+';		   /* main sign */
  if (dn==NULL) {
    printf("NULL\n");
    return;}
  if (decNumberIsNegative(dn)) isign='-';
  printf(" >> %c ", isign);
  if (dn->bits&DECSPECIAL) {	   /* Is a special value */
    if (decNumberIsInfinite(dn)) printf("Infinity");
     else {				     /* a NaN */
      if (dn->bits&DECSNAN) printf("sNaN");  /* signalling NaN */
       else printf("NaN");
      }
    /* if coefficient and exponent are 0, no more to do */
    if (dn->exponent==0 && dn->digits==1 && *dn->lsu==0) {
      printf("\n");
      return;}
    /* drop through to report other information */
    printf(" ");
    }

  /* now carefully display the coefficient */
  up=dn->lsu+D2U(dn->digits)-1; 	/* msu */
  printf("%ld", (LI)*up);
  for (up=up-1; up>=dn->lsu; up--) {
    u=*up;
    printf(":");
    for (cut=DECDPUN-1; cut>=0; cut--) {
      d=u/powers[cut];
      u-=d*powers[cut];
      printf("%ld", (LI)d);
      } /* cut */
    } /* up */
  if (dn->exponent!=0) {
    char esign='+';
    if (dn->exponent<0) esign='-';
    printf(" E%c%ld", esign, (LI)abs(dn->exponent));
    }
  printf(" [%ld]\n", (LI)dn->digits);
  } /* decNumberShow */
#endif

#if DECTRACE || DECCHECK
/* ------------------------------------------------------------------ */
/* decDumpAr -- display a unit array [debug/check aid]		      */
/*   name is a single-character tag name			      */
/*   ar   is the array to display				      */
/*   len  is the length of the array in Units			      */
/* ------------------------------------------------------------------ */
static void decDumpAr(char name, const Unit *ar, Int len) {
  Int i;
  const char *spec;
  #if DECDPUN==9
    spec="%09d ";
  #elif DECDPUN==8
    spec="%08d ";
  #elif DECDPUN==7
    spec="%07d ";
  #elif DECDPUN==6
    spec="%06d ";
  #elif DECDPUN==5
    spec="%05d ";
  #elif DECDPUN==4
    spec="%04d ";
  #elif DECDPUN==3
    spec="%03d ";
  #elif DECDPUN==2
    spec="%02d ";
  #else
    spec="%d ";
  #endif
  printf("  :%c: ", name);
  for (i=len-1; i>=0; i--) {
    if (i==len-1) printf("%ld ", (LI)ar[i]);
     else printf(spec, ar[i]);
    }
  printf("\n");
  return;}
#endif

#if DECCHECK
/* ------------------------------------------------------------------ */
/* decCheckOperands -- check operand(s) to a routine		      */
/*   res is the result structure (not checked; it will be set to      */
/*	    quiet NaN if error found (and it is not NULL))	      */
/*   lhs is the first operand (may be DECUNRESU)		      */
/*   rhs is the second (may be DECUNUSED)			      */
/*   set is the context (may be DECUNCONT)			      */
/*   returns 0 if both operands, and the context are clean, or 1      */
/*     otherwise (in which case the context will show an error,       */
/*     unless NULL).  Note that res is not cleaned; caller should     */
/*     handle this so res=NULL case is safe.			      */
/* The caller is expected to abandon immediately if 1 is returned.    */
/* ------------------------------------------------------------------ */
static Flag decCheckOperands(decNumber *res, const decNumber *lhs,
			     const decNumber *rhs, decContext *set) {
  Flag bad=0;
  if (set==NULL) {		   /* oops; hopeless */
    #if DECTRACE || DECVERB
    printf("Reference to context is NULL.\n");
    #endif
    bad=1;
    return 1;}
   else if (set!=DECUNCONT
     && (set->digits<1 || set->round>=DEC_ROUND_MAX)) {
    bad=1;
    #if DECTRACE || DECVERB
    printf("Bad context [digits=%ld round=%ld].\n",
	   (LI)set->digits, (LI)set->round);
    #endif
    }
   else {
    if (res==NULL) {
      bad=1;
      #if DECTRACE
      /* this one not DECVERB as standard tests include NULL */
      printf("Reference to result is NULL.\n");
      #endif
      }
    if (!bad && lhs!=DECUNUSED) bad=(decCheckNumber(lhs));
    if (!bad && rhs!=DECUNUSED) bad=(decCheckNumber(rhs));
    }
  if (bad) {
    if (set!=DECUNCONT) decContextSetStatus(set, DEC_Invalid_operation);
    if (res!=DECUNRESU && res!=NULL) {
      decNumberZero(res);
      res->bits=DECNAN;       /* qNaN */
      }
    }
  return bad;
  } /* decCheckOperands */

/* ------------------------------------------------------------------ */
/* decCheckNumber -- check a number				      */
/*   dn is the number to check					      */
/*   returns 0 if the number is clean, or 1 otherwise		      */
/*								      */
/* The number is considered valid if it could be a result from some   */
/* operation in some valid context.				      */
/* ------------------------------------------------------------------ */
static Flag decCheckNumber(const decNumber *dn) {
  const Unit *up;	      /* work */
  uInt maxuint; 	      /* .. */
  Int ae, d, digits;	      /* .. */
  Int emin, emax;	      /* .. */

  if (dn==NULL) {	      /* hopeless */
    #if DECTRACE
    /* this one not DECVERB as standard tests include NULL */
    printf("Reference to decNumber is NULL.\n");
    #endif
    return 1;}

  /* check special values */
  if (dn->bits & DECSPECIAL) {
    if (dn->exponent!=0) {
      #if DECTRACE || DECVERB
      printf("Exponent %ld (not 0) for a special value [%02x].\n",
	     (LI)dn->exponent, dn->bits);
      #endif
      return 1;}

    /* 2003.09.08: NaNs may now have coefficients, so next tests Inf only */
    if (decNumberIsInfinite(dn)) {
      if (dn->digits!=1) {
	#if DECTRACE || DECVERB
	printf("Digits %ld (not 1) for an infinity.\n", (LI)dn->digits);
	#endif
	return 1;}
      if (*dn->lsu!=0) {
	#if DECTRACE || DECVERB
	printf("LSU %ld (not 0) for an infinity.\n", (LI)*dn->lsu);
	#endif
	decDumpAr('I', dn->lsu, D2U(dn->digits));
	return 1;}
      } /* Inf */
    /* 2002.12.26: negative NaNs can now appear through proposed IEEE */
    /*		   concrete formats (decimal64, etc.). */
    return 0;
    }

  /* check the coefficient */
  if (dn->digits<1 || dn->digits>DECNUMMAXP) {
    #if DECTRACE || DECVERB
    printf("Digits %ld in number.\n", (LI)dn->digits);
    #endif
    return 1;}

  d=dn->digits;

  for (up=dn->lsu; d>0; up++) {
    if (d>DECDPUN) maxuint=DECDPUNMAX;
     else {		      /* reached the msu */
      maxuint=powers[d]-1;
      if (dn->digits>1 && *up<powers[d-1]) {
	#if DECTRACE || DECVERB
	printf("Leading 0 in number.\n");
	decNumberShow(dn);
	#endif
	return 1;}
      }
    if (*up>maxuint) {
      #if DECTRACE || DECVERB
      printf("Bad Unit [%08lx] in %ld-digit number at offset %ld [maxuint %ld].\n",
	      (LI)*up, (LI)dn->digits, (LI)(up-dn->lsu), (LI)maxuint);
      #endif
      return 1;}
    d-=DECDPUN;
    }

  /* check the exponent.  Note that input operands can have exponents */
  /* which are out of the set->emin/set->emax and set->digits range */
  /* (just as they can have more digits than set->digits). */
  ae=dn->exponent+dn->digits-1;    /* adjusted exponent */
  emax=DECNUMMAXE;
  emin=DECNUMMINE;
  digits=DECNUMMAXP;
  if (ae<emin-(digits-1)) {
    #if DECTRACE || DECVERB
    printf("Adjusted exponent underflow [%ld].\n", (LI)ae);
    decNumberShow(dn);
    #endif
    return 1;}
  if (ae>+emax) {
    #if DECTRACE || DECVERB
    printf("Adjusted exponent overflow [%ld].\n", (LI)ae);
    decNumberShow(dn);
    #endif
    return 1;}

  return 0;		 /* it's OK */
  } /* decCheckNumber */

/* ------------------------------------------------------------------ */
/* decCheckInexact -- check a normal finite inexact result has digits */
/*   dn is the number to check					      */
/*   set is the context (for status and precision)		      */
/*   sets Invalid operation, etc., if some digits are missing	      */
/* [this check is not made for DECSUBSET compilation or when	      */
/* subnormal is not set]					      */
/* ------------------------------------------------------------------ */
static void decCheckInexact(const decNumber *dn, decContext *set) {
  #if !DECSUBSET && DECEXTFLAG
    if ((set->status & (DEC_Inexact|DEC_Subnormal))==DEC_Inexact
     && (set->digits!=dn->digits) && !(dn->bits & DECSPECIAL)) {
      #if DECTRACE || DECVERB
      printf("Insufficient digits [%ld] on normal Inexact result.\n",
	     (LI)dn->digits);
      decNumberShow(dn);
      #endif
      decContextSetStatus(set, DEC_Invalid_operation);
      }
  #else
    /* next is a noop for quiet compiler */
    if (dn!=NULL && dn->digits==0) set->status|=DEC_Invalid_operation;
  #endif
  return;
  } /* decCheckInexact */
#endif

#if DECALLOC
#undef malloc
#undef free
/* ------------------------------------------------------------------ */
/* decMalloc -- accountable allocation routine			      */
/*   n is the number of bytes to allocate			      */
/*								      */
/* Semantics is the same as the stdlib malloc routine, but bytes      */
/* allocated are accounted for globally, and corruption fences are    */
/* added before and after the 'actual' storage. 		      */
/* ------------------------------------------------------------------ */
/* This routine allocates storage with an extra twelve bytes; 8 are   */
/* at the start and hold:					      */
/*   0-3 the original length requested				      */
/*   4-7 buffer corruption detection fence (DECFENCE, x4)	      */
/* The 4 bytes at the end also hold a corruption fence (DECFENCE, x4) */
/* ------------------------------------------------------------------ */
static void *decMalloc(size_t n) {
  uInt	size=n+12;		   /* true size */
  void	*alloc; 		   /* -> allocated storage */
  uByte *b, *b0;		   /* work */
  uInt	uiwork; 		   /* for macros */

  alloc=malloc(size);		   /* -> allocated storage */
  if (alloc==NULL) return NULL;    /* out of strorage */
  b0=(uByte *)alloc;		   /* as bytes */
  decAllocBytes+=n;		   /* account for storage */
  UBFROMUI(alloc, n);		   /* save n */
  /* printf(" alloc ++ dAB: %ld (%ld)\n", (LI)decAllocBytes, (LI)n); */
  for (b=b0+4; b<b0+8; b++) *b=DECFENCE;
  for (b=b0+n+8; b<b0+n+12; b++) *b=DECFENCE;
  return b0+8;			   /* -> play area */
  } /* decMalloc */

/* ------------------------------------------------------------------ */
/* decFree -- accountable free routine				      */
/*   alloc is the storage to free				      */
/*								      */
/* Semantics is the same as the stdlib malloc routine, except that    */
/* the global storage accounting is updated and the fences are	      */
/* checked to ensure that no routine has written 'out of bounds'.     */
/* ------------------------------------------------------------------ */
/* This routine first checks that the fences have not been corrupted. */
/* It then frees the storage using the 'truw' storage address (that   */
/* is, offset by 8).						      */
/* ------------------------------------------------------------------ */
static void decFree(void *alloc) {
  uInt	n;			   /* original length */
  uByte *b, *b0;		   /* work */
  uInt	uiwork; 		   /* for macros */

  if (alloc==NULL) return;	   /* allowed; it's a nop */
  b0=(uByte *)alloc;		   /* as bytes */
  b0-=8;			   /* -> true start of storage */
  n=UBTOUI(b0); 		   /* lift length */
  for (b=b0+4; b<b0+8; b++) if (*b!=DECFENCE)
    printf("=== Corrupt byte [%02x] at offset %d from %ld ===\n", *b,
	   b-b0-8, (LI)b0);
  for (b=b0+n+8; b<b0+n+12; b++) if (*b!=DECFENCE)
    printf("=== Corrupt byte [%02x] at offset +%d from %ld, n=%ld ===\n", *b,
	   b-b0-8, (LI)b0, (LI)n);
  free(b0);			   /* drop the storage */
  decAllocBytes-=n;		   /* account for storage */
  /* printf(" free -- dAB: %d (%d)\n", decAllocBytes, -n); */
  } /* decFree */
#define malloc(a) decMalloc(a)
#define free(a) decFree(a)
#endif
