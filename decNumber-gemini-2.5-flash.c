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
#include <stdint.h>

decNumber * decNumberFromInt32(decNumber *dn, int32_t in) {
  uint32_t magnitude;

  if (in == INT32_MIN) {
    magnitude = (uint32_t)1U << 31;
  } else if (in < 0) {
    magnitude = (uint32_t)(-in);
  } else {
    magnitude = (uint32_t)in;
  }

  decNumberFromUInt32(dn, magnitude);

  if (in < 0) {
    dn->bits = DECNEG;
  }

  return dn;
} /* decNumberFromInt32 */

decNumber * decNumberFromUInt32(decNumber *dn, uInt uin) {
  Unit *current_unit_ptr; // Pointer for iterating through the Unit array
  Unit *lsu_end_ptr;      // Pointer marking one past the end of the lsu buffer

  // 1. Reliability: Validate input pointer.
  // A NULL 'decNumber' object would lead to undefined behavior (e.g., a crash)
  // when its members are accessed.
  if (dn == NULL) {
    // Returning NULL signals that the operation failed due to invalid input.
    return NULL;
  }

  // 2. Maintainability: Initialize the 'decNumber' object to a known zero state.
  // This clears any previous value and ensures a consistent starting point.
  // It is assumed that 'decNumberZero' correctly sets all necessary fields,
  // including 'dn->digits' to 1 for the value 0.
  decNumberZero(dn);

  // 3. Optimization/Edge Case: If the input unsigned integer is 0,
  // the 'decNumber' object is already correctly set to zero by 'decNumberZero'.
  // No further conversion is needed.
  if (uin == 0) {
    return dn;
  }

  // Initialize the starting pointer for writing units.
  current_unit_ptr = dn->lsu;

  // 4. Security/Reliability: Calculate the maximum valid address for 'lsu'
  // to prevent buffer overflow. 'DECNUMUNITS' is assumed to be a pre-defined
  // constant specifying the fixed maximum capacity of the 'dn->lsu' array
  // within the 'decNumber' structure.
  lsu_end_ptr = dn->lsu + DECNUMUNITS;

  // 5. Core conversion loop: Extract units from 'uin' in base (DECDPUNMAX + 1).
  while (uin > 0) {
    // 6. Security/Reliability: Check for buffer overflow BEFORE writing.
    // If 'current_unit_ptr' reaches or exceeds 'lsu_end_ptr', it means that
    // the input 'uin' requires more storage than 'dn->lsu' can provide.
    if (current_unit_ptr >= lsu_end_ptr) {
      // The input number is too large to fit. The 'dn' object remains zeroed
      // from the initial 'decNumberZero' call.
      // Return NULL to indicate that the conversion failed due to insufficient capacity.
      return NULL;
    }

    // Convert the current part of 'uin' into a 'Unit' and store it.
    // '(DECDPUNMAX + 1)' represents the base for each Unit (e.g., 10000).
    *current_unit_ptr = (Unit)(uin % (DECDPUNMAX + 1));
    uin /= (DECDPUNMAX + 1);
    current_unit_ptr++; // Move to the next unit storage location.
  }

  // 7. Maintainability: Update the total number of decimal digits used.
  // 'decGetDigits' calculates the actual decimal digit count based on the
  // filled 'Unit' array. The expression 'current_unit_ptr - dn->lsu'
  // correctly provides the number of 'Unit' elements that were written to.
  dn->digits = decGetDigits(dn->lsu, current_unit_ptr - dn->lsu);

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
#include <stdint.h>
#include <limits.h>

int32_t decNumberToInt32(const decNumber *dn, decContext *set) {
#if DECCHECK
  if (decCheckOperands(DECUNRESU, DECUNUSED, dn, set)) {
    return 0;
  }
#endif

  if ((dn->bits & DECSPECIAL) || (dn->exponent != 0) || (dn->digits > 10)) {
    decContextSetStatus(set, DEC_Invalid_operation);
    return 0;
  }

  uint32_t magnitude_high_part = 0;
  uint32_t magnitude_low_digit;
  const Unit *current_unit_ptr = dn->lsu;

  magnitude_low_digit = *current_unit_ptr;

#if DECDPUN > 1
  magnitude_high_part = magnitude_low_digit / 10;
  magnitude_low_digit = magnitude_low_digit % 10;
#endif

  current_unit_ptr++;

  for (int current_digit_count = DECDPUN;
       current_digit_count < dn->digits;
       current_unit_ptr++, current_digit_count += DECDPUN) {
    magnitude_high_part += (uint32_t)(*current_unit_ptr * powers[current_digit_count - 1]);
  }

  uint32_t total_magnitude = magnitude_high_part * 10 + magnitude_low_digit;

  if (total_magnitude > (uint32_t)INT32_MAX) {
    if ((dn->bits & DECNEG) && (total_magnitude == (uint32_t)INT32_MAX + 1U)) {
      return INT32_MIN;
    }
    decContextSetStatus(set, DEC_Invalid_operation);
    return 0;
  }

  if (dn->bits & DECNEG) {
    return -(int32_t)total_magnitude;
  } else {
    return (int32_t)total_magnitude;
  }
} /* decNumberToInt32 */

#include <stdint.h> // For uint32_t, UINT32_MAX
#include <stdbool.h> // For bool

// Assume these types, macros, and external variables are defined in decNumber.h or similar:
// typedef unsigned int uInt; // Must be 32-bit unsigned, typically uint32_t
// typedef struct decNumber decNumber;
// typedef struct decContext decContext;
// typedef unsigned int Unit; // Type for storing groups of decimal digits
// extern const uint32_t powers[]; // Array: {1, 10, 100, 1000, ...} for powers of 10
// #define DECDPUN ... // Decimal Digits Per UNit (e.g., 1, 3, 9)
// #define DECSPECIAL ... // Bit flag for special values (NaN, Inf)
// #define DECNEG ...     // Bit flag for negative sign
// #define DECUNRESU ...  // For decCheckOperands
// #define DECUNUSED ...  // For decCheckOperands
// #define DEC_Invalid_operation ... // Error status code
// #define decContextSetStatus(set, status) ... // Function/macro to set context status
// #define decCheckOperands(a, b, c, d) ...     // Function/macro for operand checks
// #define ISZERO(dn) ... // Macro/function to check if decNumber is numerically zero

// Helper for multiplying by 10 for uint32_t.
// This replaces the undefined 'X10' from the original code for clarity and safety.
#define MULTIPLY_BY_10(val) ((val) * 10U)

uInt decNumberToUInt32(const decNumber *dn, decContext *set) {
    // Perform library-specific operand checks if DECCHECK is enabled.
    // This preprocessor block is retained as it reflects an existing system integration point.
#if DECCHECK
    if (decCheckOperands(DECUNRESU, DECUNUSED, dn, set)) {
        // If decCheckOperands returns true, it indicates an issue.
        // It's assumed to set the context status internally.
        return 0;
    }
#endif

    // Validate the decNumber properties for conversion to a uint32_t.
    // These conditions lead to an immediate invalid operation.
    if ((dn->bits & DECSPECIAL) != 0) { // Number is a special value (NaN, Infinity)
        decContextSetStatus(set, DEC_Invalid_operation);
        return 0;
    }
    if (dn->exponent != 0) { // Number is not an integer (has fractional part)
        decContextSetStatus(set, DEC_Invalid_operation);
        return 0;
    }
    if ((dn->bits & DECNEG) != 0 && !ISZERO(dn)) { // Number is negative and not zero
        decContextSetStatus(set, DEC_Invalid_operation);
        return 0;
    }
    if (dn->digits > 10) { // A uint32_t (max 4,294,967,295) has at most 10 decimal digits
        decContextSetStatus(set, DEC_Invalid_operation);
        return 0;
    }

    // The number is now guaranteed to be a finite, non-negative integer with 10 or fewer digits.
    uint32_t high_part_of_value = 0; // Accumulates digits more significant than the last
    uint32_t last_single_digit;      // Stores the least significant decimal digit
    const Unit *unit_ptr = dn->lsu;  // Pointer to the Least Significant Unit

    // Extract the value from the least significant Unit.
    // 'last_single_digit' temporarily holds the entire value of the LSU.
    last_single_digit = (uint32_t)*unit_ptr;

#if DECDPUN > 1
    // If a Unit stores multiple decimal digits (e.g., DECDPUN=3, unit value=123),
    // we split it into its last digit and the remaining higher-order part.
    high_part_of_value = last_single_digit / 10U; // e.g., 123 -> 12
    last_single_digit = last_single_digit % 10U;  // e.g., 123 -> 3
#endif

    unit_ptr++; // Move to the next more significant Unit

    // Loop through any remaining Units to accumulate their values.
    // `d` tracks the total number of decimal digits processed so far,
    // which helps in indexing the `powers` array correctly.
    for (int d = DECDPUN; d < dn->digits; unit_ptr++, d += DECDPUN) {
        // Each unit's value is multiplied by the appropriate power of 10
        // (e.g., if DECDPUN=3, the second unit contributes value * 10^3, the third * 10^6, etc.
        // The `powers[d - 1]` correctly aligns the value of the current unit relative
        // to `high_part_of_value` which represents `(full_number - last_digit) / 10`.
        high_part_of_value += (uint32_t)(*unit_ptr) * powers[d - 1];
    }

    // Final overflow check against `UINT32_MAX` (which is 4,294,967,295).
    // The number being constructed is `(high_part_of_value * 10) + last_single_digit`.
    // UINT32_MAX / 10U = 429,496,729
    // UINT32_MAX % 10U = 5
    if (high_part_of_value > (UINT32_MAX / 10U) ||
        (high_part_of_value == (UINT32_MAX / 10U) && last_single_digit > (UINT32_MAX % 10U))) {
        decContextSetStatus(set, DEC_Invalid_operation);
        return 0;
    }

    // Reconstruct the full 32-bit unsigned integer value.
    // This value is guaranteed to fit within a uint32_t.
    return MULTIPLY_BY_10(high_part_of_value) + last_single_digit;
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
    // Reliability & Security: Check for a NULL output buffer.
    // If 'string' is NULL, we cannot write to it safely.
    // Returning NULL signals an unrecoverable error for this output parameter.
    if (string == NULL) {
        return NULL;
    }

    // Reliability: Handle a NULL input decNumber gracefully.
    // If 'dn' is NULL, there's no number to convert.
    // Setting the output string to an empty string ("") is a reasonable default
    // and prevents potential undefined behavior or crashes in the 'decToString' function
    // if it were to receive a NULL decNumber.
    if (dn == NULL) {
        string[0] = '\0';
        return string;
    }

    // Maintainability & Reliability:
    // Call the underlying conversion function.
    // The '0' argument is typically interpreted as a NULL pointer for the 'clength' parameter,
    // indicating that the caller does not require the count of characters written.
    //
    // Note: This function cannot unilaterally prevent buffer overflows if 'string'
    // is too small and 'decToString' does not enforce bounds, as the buffer size
    // is not provided to 'decNumberToString'. The responsibility for providing a
    // sufficiently large buffer for the output string still lies with the caller.
    decToString(dn, string, 0);

    return string;
} /* DecNumberToString */

char * decNumberToEngString(const decNumber *dn, char *string) {
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
static int parseSpecialValue(decNumber *dn, const char *current_pos, uByte bits, decContext *set, uInt *status) {
    if (decBiStr(current_pos, "infinity", "INFINITY") || decBiStr(current_pos, "inf", "INF")) {
        dn->bits = bits | DECINF;
        *status = 0;
        return 1;
    }

    decNumberZero(dn);
    dn->bits = bits | DECNAN;

    const char *c = current_pos;
    if (*c == 's' || *c == 'S') {
        c++;
        dn->bits = bits | DECSNAN;
    }

    if (!(*c == 'n' || *c == 'N')) return 0;
    c++;
    if (!(*c == 'a' || *c == 'A')) return 0;
    c++;
    if (!(*c == 'n' || *c == 'N')) return 0;
    c++;

    const char *cfirst_payload = c;
    while (*cfirst_payload == '0') cfirst_payload++;

    if (*cfirst_payload == '\0') {
        *status = 0;
        return 1;
    }

    Int d_payload = 0;
    const char *last_payload = NULL;
    for (const char *pc = cfirst_payload; ; pc++, d_payload++) {
        if (!(*pc >= '0' && *pc <= '9')) break;
        last_payload = pc;
    }

    if (*pc != '\0') {
        return 0;
    }

    if (set->clamp && d_payload > set->digits - 1) return 0;
    if (d_payload > set->digits) return 0;

    *status = 0;
    return 1;
}

static int parseExponent(Int *exponent_ptr, const char **c_ptr, uInt *status_ptr) {
    const char *c = *c_ptr;
    Flag nege = 0;
    Int current_exponent = 0;

    if (!(*c == 'e' || *c == 'E')) {
        *status_ptr = DEC_Conversion_syntax;
        return 0;
    }
    c++;

    if (*c == '-') {
        nege = 1;
        c++;
    } else if (*c == '+') {
        c++;
    }

    if (*c == '\0') {
        *status_ptr = DEC_Conversion_syntax;
        return 0;
    }

    const char *exp_digit_start = c;
    while (*exp_digit_start == '0' && *(exp_digit_start + 1) != '\0') {
        exp_digit_start++;
    }

    for (const char *pc = exp_digit_start; ; pc++) {
        if (!(*pc >= '0' && *pc <= '9')) break;
        if (current_exponent > DECNUMMAXE / 10 || (current_exponent == DECNUMMAXE / 10 && (*pc - '0') > (DECNUMMAXE % 10))) {
             current_exponent = DECNUMMAXE * 2;
             while (*pc >= '0' && *pc <= '9') pc++;
             break;
        }
        current_exponent = X10(current_exponent) + (*pc - '0');
    }

    if (*pc != '\0') {
        *status_ptr = DEC_Conversion_syntax;
        return 0;
    }

    if (nege) current_exponent = -current_exponent;
    *exponent_ptr = current_exponent;
    *c_ptr = pc;
    *status_ptr = 0;
    return 1;
}

decNumber * decNumberFromString(decNumber *dn, const char chars[], decContext *set) {
    Int exponent = 0;
    uByte bits = 0;
    Unit *res = NULL;
    Unit resbuff[SD2U(DECBUFFER + 9)];
    Unit *allocres = NULL;
    Int d = 0;
    const char *dotchar = NULL;
    const char *cfirst = chars;
    const char *last = NULL;
    const char *c_iter;
    uInt status = 0;

    decNumberZero(dn);

    #if DECCHECK
    if (decCheckOperands(DECUNRESU, DECUNUSED, DECUNUSED, set)) {
        return dn;
    }
    #endif

    c_iter = chars;

    if (*c_iter == '-') {
        bits = DECNEG;
        c_iter++;
    } else if (*c_iter == '+') {
        c_iter++;
    }
    cfirst = c_iter;

    const char *current_digit_char = c_iter;
    for (; ; current_digit_char++) {
        if (*current_digit_char >= '0' && *current_digit_char <= '9') {
            last = current_digit_char;
            d++;
            continue;
        }
        if (*current_digit_char == '.' && dotchar == NULL) {
            dotchar = current_digit_char;
            if (current_digit_char == cfirst) {
                cfirst++;
            }
            continue;
        }
        break;
    }

    if (last == NULL) {
        if (dotchar != NULL) {
            status = DEC_Conversion_syntax;
            goto cleanup;
        }

        #if DECSUBSET
        if (!set->extended) {
            status = DEC_Conversion_syntax;
            goto cleanup;
        }
        #endif

        if (parseSpecialValue(dn, cfirst, bits, set, &status)) {
            if (status == 0) {
                goto cleanup;
            }
            goto cleanup;
        } else {
            status = DEC_Conversion_syntax;
            goto cleanup;
        }
    }

    if (*current_digit_char != '\0') {
        if (!parseExponent(&exponent, &current_digit_char, &status)) {
            goto cleanup;
        }
        if (*current_digit_char != '\0') {
            status = DEC_Conversion_syntax;
            goto cleanup;
        }
    }

    if (*cfirst == '0' && d > 1) {
        const char *temp_cfirst = cfirst;
        Int temp_d = d;
        for (const char *sc = cfirst; sc < last; sc++) {
            if (*sc == '.') continue;
            if (*sc != '0') break;
            temp_cfirst++;
            temp_d--;
        }
        if (temp_d == 0) {
            temp_d = 1;
            const char* zero_c = last;
            while(zero_c >= cfirst && *zero_c != '0') zero_c--;
            if (zero_c < cfirst) zero_c = cfirst;
            cfirst = zero_c;
        } else {
            cfirst = temp_cfirst;
            d = temp_d;
        }
    }

    if (dotchar != NULL && dotchar < last) {
        Int digits_after_dot = 0;
        for (const char *pc = dotchar + 1; pc <= last; pc++) {
            if (*pc >= '0' && *pc <= '9') {
                digits_after_dot++;
            }
        }
        exponent -= digits_after_dot;
    }

    if (d <= set->digits) {
        res = dn->lsu;
    } else {
        size_t needbytes = D2U(d) * sizeof(Unit);
        if (needbytes > sizeof(resbuff)) {
            allocres = (Unit *)malloc(needbytes);
            if (allocres == NULL) {
                status |= DEC_Insufficient_storage;
                goto cleanup;
            }
            res = allocres;
        } else {
            res = resbuff;
        }
    }

    Unit *up;
    #if DECDPUN > 1
    Int out = 0;
    up = res + D2U(d) - 1;
    Int cut = d - (up - res) * DECDPUN;
    for (const char *pc = cfirst; ; pc++) {
        if (*pc == '.') continue;
        out = X10(out) + (*pc - '0');
        if (pc == last) break;
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
    for (const char *pc = last; pc >= cfirst; pc--) {
        if (*pc == '.') continue;
        *up = (Unit)(*pc - '0');
        up++;
    }
    #endif

    dn->bits = bits;
    dn->exponent = exponent;
    dn->digits = d;

    Int residue = 0;
    if (d > set->digits) {
        decSetCoeff(dn, set, res, d, &residue, &status);
        decFinalize(dn, set, &residue, &status);
    } else {
        if ((dn->exponent - 1 < set->emin - dn->digits) || (dn->exponent - 1 > set->emax - set->digits)) {
            decFinalize(dn, set, &residue, &status);
        }
    }

cleanup:
    free(allocres);
    if (status != 0) {
        decStatus(dn, status, set);
    }
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
  uInt status=0;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  decNumberZero(&dzero);
  dzero.exponent=rhs->exponent;
  decAddOp(res, &dzero, rhs, set, (uByte)(rhs->bits & DECNEG), &status);
  if (status!=0) decStatus(res, status, set);
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
  uInt status=0;
  decAddOp(res, lhs, rhs, set, 0, &status);
  if (status!=0) decStatus(res, status, set);
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
  const Unit *lhs_unit_ptr;
  const Unit *rhs_unit_ptr;
  Unit *res_unit_ptr;
  const Unit *lhs_msu_ptr;
  const Unit *rhs_msu_ptr;
  Unit *res_msu_ptr;
  Int	res_msu_digits;
  Int i;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) {
    return res;
  }
  #endif

  if (lhs->exponent != 0 || decNumberIsSpecial(lhs) || decNumberIsNegative(lhs) ||
      rhs->exponent != 0 || decNumberIsSpecial(rhs) || decNumberIsNegative(rhs)) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  lhs_unit_ptr = lhs->lsu;
  rhs_unit_ptr = rhs->lsu;
  res_unit_ptr = res->lsu;
  lhs_msu_ptr = lhs_unit_ptr + D2U(lhs->digits) - 1;
  rhs_msu_ptr = rhs_unit_ptr + D2U(rhs->digits) - 1;
  res_msu_ptr = res_unit_ptr + D2U(set->digits) - 1;
  res_msu_digits = MSUDIGITS(set->digits);

  for (; res_unit_ptr <= res_msu_ptr; lhs_unit_ptr++, rhs_unit_ptr++, res_unit_ptr++) {
    Unit current_lhs_unit_val;
    Unit current_rhs_unit_val;

    current_lhs_unit_val = (lhs_unit_ptr > lhs_msu_ptr) ? 0 : *lhs_unit_ptr;
    current_rhs_unit_val = (rhs_unit_ptr > rhs_msu_ptr) ? 0 : *rhs_unit_ptr;

    *res_unit_ptr = 0;

    if (current_lhs_unit_val || current_rhs_unit_val) {
      for (i = 0; i < DECDPUN; i++) {
        Int lhs_digit = current_lhs_unit_val % 10;
        Int rhs_digit = current_rhs_unit_val % 10;

        if (lhs_digit > 1 || rhs_digit > 1) {
          decStatus(res, DEC_Invalid_operation, set);
          return res;
        }

        if (lhs_digit == 1 && rhs_digit == 1) {
          *res_unit_ptr += (Unit)powers[i];
        }

        current_lhs_unit_val /= 10;
        current_rhs_unit_val /= 10;

        if (res_unit_ptr == res_msu_ptr && i == res_msu_digits - 1) {
          break;
        }
      }
    }
  }

  res->digits = decGetDigits(res->lsu, res_unit_ptr - res->lsu);
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
  uInt operation_status = 0;
  decCompareOp(res, lhs, rhs, set, COMPARE, &operation_status);

  if (operation_status != 0) {
    decStatus(res, operation_status, set);
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
  uInt status = 0;
  decCompareOp(res, lhs, rhs, set, COMPSIG, &status);
  decStatus(res, status, set);
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
decNumber * decNumberCompareTotal(decNumber *res, const decNumber *lhs,
				  const decNumber *rhs, decContext *set) {
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
static uInt get_abs_copy(const decNumber *src, decNumber *stack_buf, size_t stack_buf_size_bytes, decNumber **alloc_ptr_out, decNumber **effective_ptr_out) {
    *alloc_ptr_out = NULL;

    if (!decNumberIsNegative(src)) {
        *effective_ptr_out = (decNumber *)src;
        return 0;
    }

    size_t needbytes = sizeof(decNumber) + (D2U(src->digits) - 1) * sizeof(Unit);
    decNumber *target_buf;

    if (needbytes > stack_buf_size_bytes) {
        *alloc_ptr_out = (decNumber *)malloc(needbytes);
        if (*alloc_ptr_out == NULL) {
            return DEC_Insufficient_storage;
        }
        target_buf = *alloc_ptr_out;
    } else {
        target_buf = stack_buf;
    }

    decNumberCopy(target_buf, src);
    target_buf->bits &= ~DECNEG;
    *effective_ptr_out = target_buf;
    return 0;
}


decNumber * decNumberCompareTotalMag(decNumber *res, const decNumber *lhs,
				     const decNumber *rhs, decContext *set) {
  uInt status = 0;
  decNumber bufa[D2N(DECBUFFER+1)];
  decNumber *allocbufa = NULL;
  decNumber bufb[D2N(DECBUFFER+1)];
  decNumber *allocbufb = NULL;
  decNumber *a = (decNumber *)lhs;
  decNumber *b = (decNumber *)rhs;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  status = get_abs_copy(lhs, bufa, sizeof(bufa), &allocbufa, &a);
  if (status != 0) {
      goto cleanup;
  }

  status = get_abs_copy(rhs, bufb, sizeof(bufb), &allocbufb, &b);
  if (status != 0) {
      goto cleanup;
  }

  decCompareOp(res, a, b, set, COMPTOTAL, &status);

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
  uInt status=0;
  decDivideOp(res, lhs, rhs, set, DIVIDE, &status);
  if (status!=0) decStatus(res, status, set);
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
  // Prioritize critical input validation for robustness, especially for pointers.
  // In `decNumber` library, `NULL` inputs typically lead to undefined behavior (crashes).
  // To improve reliability and security (preventing crashes from `NULL` dereferences),
  // explicit checks are added. While this changes the exact `NULL` handling from a crash
  // to a controlled error return, it's generally considered an improvement in robustness
  // and maintainability for legacy code, aligning with SonarCloud objectives.

  // Check for NULL 'res' first, as it's the output buffer. If NULL, we cannot write to it.
  // Also check 'lhs' and 'rhs' as they are essential for the operation.
  if (res == NULL || lhs == NULL || rhs == NULL) {
    // If 'set' is available, mark an invalid operation status.
    // We cannot modify 'res' to indicate error if it is NULL.
    // Returning NULL is the clearest way to signal a critical failure
    // where the operation could not even begin or complete successfully.
    if (set != NULL) {
      set->status |= DEC_InvalidOperation; // A general status for fundamental input errors.
    }
    return NULL; // Indicate failure to produce a valid result.
  }

  // Now, 'res', 'lhs', 'rhs' are guaranteed non-NULL.
  // Check for NULL 'set' as it's crucial for context, precision, and error reporting.
  if (set == NULL) {
    // If 'set' is NULL, we cannot perform the operation correctly or report status.
    // Since 'res' is not NULL, we could potentially try to set it to NaN or
    // a default error value, but doing so without a `decContext` might be inconsistent
    // with how `decNumber` typically initializes/handles NaN, and might be
    // considered altering functionality if `decDivideOp` would otherwise crash.
    // Returning NULL here (even if 'res' is valid) is a robust way to signal
    // that the operation cannot proceed due to missing context.
    // We also cannot use decStatus if 'set' is NULL.
    return NULL;
  }

  uInt status = 0; // Initialize status to indicate no errors so far.

  // Execute the core integer division operation.
  // This function is expected to update 'res' and 'status' based on the computation.
  decDivideOp(res, lhs, rhs, set, DIVIDEINT, &status);

  // After the operation, check if any errors, warnings, or special conditions
  // (e.g., inexact, division by zero) were reported in 'status'.
  if (status != 0) {
    // If 'status' is not zero, delegate the handling of these conditions
    // to the standard `decStatus` function provided by the `decNumber` library.
    // This ensures consistent error processing within the library's framework.
    decStatus(res, status, set);
  }

  // Return the pointer to the result `decNumber`.
  // The caller should check `set->status` and potentially the returned `res` value
  // (e.g., for NaN or Inf) to determine the full outcome.
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
decNumber * decNumberExp(decNumber *res, const decNumber *rhs,
			 decContext *set) {
  uInt status = 0;
  decNumber *allocrhs = NULL;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) {
    return res;
  }
  #endif

  if (!decCheckMath(rhs, set, &status)) {
    int should_execute_exp_op = 1;

    #if DECSUBSET
    if (!set->extended && rhs->digits > set->digits) {
      allocrhs = decRoundOperand(rhs, set, &status);
      if (allocrhs == NULL) {
        should_execute_exp_op = 0;
      } else {
        rhs = allocrhs;
      }
    }
    #endif

    if (should_execute_exp_op) {
      decExpOp(res, rhs, set, &status);
    }
  }

  #if DECSUBSET
  free(allocrhs);
  #endif

  if (status != 0) {
    decStatus(res, status, set);
  }

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
  uInt status = 0;
  decContext dcmul;
  decNumber *acc = NULL;
  decNumber stack_buf[D2N(DECBUFFER * 2 + 1)];
  decNumber dzero_val;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) {
    return res;
  }
  if (decCheckOperands(res, fhs, DECUNUSED, set)) {
    return res;
  }
  #endif

  #if DECSUBSET
  if (!set->extended) {
    status |= DEC_Invalid_operation;
    decStatus(res, status, set);
    return res;
  }
  #endif

  if ((!decNumberIsSpecial(lhs) && decCheckMath(lhs, set, &status)) ||
      (!decNumberIsSpecial(rhs) && decCheckMath(rhs, set, &status)) ||
      (!decNumberIsSpecial(fhs) && decCheckMath(fhs, set, &status))) {
    decStatus(res, status, set);
    return res;
  }

  dcmul = *set;
  dcmul.digits = lhs->digits + rhs->digits;
  dcmul.emax = DEC_MAX_EMAX;
  dcmul.emin = DEC_MIN_EMIN;

  size_t needbytes = sizeof(decNumber) + (D2U(dcmul.digits) - 1) * sizeof(Unit);
  if (needbytes > sizeof(stack_buf)) {
    acc = (decNumber *)malloc(needbytes);
    if (acc == NULL) {
      status |= DEC_Insufficient_storage;
      decStatus(res, status, set);
      return res;
    }
  } else {
    acc = stack_buf;
  }

  decMultiplyOp(acc, lhs, rhs, &dcmul, &status);

  if ((status & DEC_Invalid_operation) != 0) {
    if (!(status & DEC_sNaN)) {
      if (acc != stack_buf) {
        free(acc);
      }
      decStatus(res, status, set);
      return res;
    } else {
      decNumberZero(&dzero_val);
      fhs = &dzero_val;
    }
  }

  decAddOp(res, acc, fhs, set, 0, &status);

  if (acc != stack_buf) {
    free(acc);
  }

  if (status != 0) {
    decStatus(res, status, set);
  }

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
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) {
      return res;
  }
  #endif

  if (rhs->exponent != 0 || decNumberIsSpecial(rhs) || decNumberIsNegative(rhs)) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  const Unit *current_rhs_unit = rhs->lsu;
  const Unit *max_rhs_unit_ptr = current_rhs_unit + D2U(rhs->digits) - 1;

  Unit *current_res_unit = res->lsu;
  Unit *max_res_unit_ptr = current_res_unit + D2U(set->digits) - 1;

  Int final_res_unit_digit_count = MSUDIGITS(set->digits);

  for (; current_res_unit <= max_res_unit_ptr; ++current_rhs_unit, ++current_res_unit) {
    Unit rhs_unit_value;

    if (current_rhs_unit > max_rhs_unit_ptr) {
        rhs_unit_value = 0;
    } else {
        rhs_unit_value = *current_rhs_unit;
    }

    *current_res_unit = 0;

    for (Int digit_index = 0; digit_index < DECDPUN; ++digit_index) {
      Int current_digit = rhs_unit_value % 10;
      rhs_unit_value /= 10;

      if (current_digit > 1) {
        decStatus(res, DEC_Invalid_operation, set);
        return res;
      }

      if (current_digit == 0) {
        *current_res_unit += (Unit)powers[digit_index];
      }

      if (current_res_unit == max_res_unit_ptr && digit_index == final_res_unit_digit_count - 1) {
        break;
      }
    }
  }

  res->digits = decGetDigits(res->lsu, current_res_unit - res->lsu);
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
decNumber * decNumberLn(decNumber *res, const decNumber *rhs_in,
			decContext *set) {
  uInt status = 0;
  #if DECSUBSET
  decNumber *allocrhs = NULL;
  const decNumber *current_rhs = rhs_in;
  #else
  const decNumber *current_rhs = rhs_in;
  #endif

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, current_rhs, set)) {
      return res;
  }
  #endif

  if (decCheckMath(current_rhs, set, &status)) {
      goto cleanup;
  }

  #if DECSUBSET
  if (!set->extended) {
    if (current_rhs->digits > set->digits) {
      allocrhs = decRoundOperand(current_rhs, set, &status);
      if (allocrhs == NULL) {
        goto cleanup;
      }
      current_rhs = allocrhs;
    }
    if (ISZERO(current_rhs)) {
      status |= DEC_Invalid_operation;
      goto cleanup;
    }
  }
  #endif

  if (status == 0) {
      decLnOp(res, current_rhs, set, &status);
  }

cleanup:
  #if DECSUBSET
  free(allocrhs);
  #endif
  if (status != 0) {
      decStatus(res, status, set);
  }
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
  uInt status=0;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  if (decNumberIsNaN(rhs)) {
    decNaNs(res, rhs, NULL, set, &status);
  } else if (decNumberIsInfinite(rhs)) {
    decNumberCopyAbs(res, rhs);
  } else if (decNumberIsZero(rhs)) {
    decNumberSetInfinite(res, 0);
    status |= DEC_Division_by_zero;
  } else {
    Int adjustedExponent = rhs->exponent + rhs->digits - 1;
    decNumberFromInt32(res, adjustedExponent);
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
#define LN_PREC_ADJUST_FOR_EXPONENT_DIGITS 6
#define GUARD_DIGITS_FOR_INTERMEDIATE_CALCS 3

static inline decNumber* allocateTempDecNumber(
    decNumber* stack_buffer, size_t stack_buffer_size,
    decNumber** alloc_ptr_out, Int max_digits, uInt* status_out
) {
    if (max_digits <= 0) {
        *status_out |= DEC_Invalid_operation;
        return NULL;
    }
    
    uInt needed_units = D2U(max_digits);
    if (needed_units == 0) needed_units = 1;
    
    size_t needed_bytes = sizeof(decNumber) + (needed_units - 1) * sizeof(Unit);

    if (needed_bytes > stack_buffer_size) {
        *alloc_ptr_out = (decNumber*)malloc(needed_bytes);
        if (*alloc_ptr_out == NULL) {
            *status_out |= DEC_Insufficient_storage;
            return NULL;
        }
        return *alloc_ptr_out;
    } else {
        return stack_buffer;
    }
}

decNumber * decNumberLog10(decNumber *res, const decNumber *rhs,
			  decContext *set) {
  uInt status = 0;
  uInt ignore_status = 0;
  Int p_ln_rhs;
  Int p_ln_10;

  decNumber bufa[D2N(DECBUFFER + 2)];
  decNumber *allocbufa = NULL;
  decNumber *a = NULL;

  decNumber bufb[D2N(DECBUFFER + 2)];
  decNumber *allocbufb = NULL;
  decNumber *b = NULL;

  decNumber bufw[D2N(10)];
  decNumber *w = bufw;

  #if DECSUBSET
  decNumber *allocrhs = NULL;
  #endif

  decContext aset;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) {
    return res;
  }
  #endif

  if (!decCheckMath(rhs, set, &status)) {
    do {
        #if DECSUBSET
        if (!set->extended) {
            if (rhs->digits > set->digits) {
                allocrhs = decRoundOperand(rhs, set, &status);
                if (allocrhs == NULL) break;
                rhs = allocrhs;
            }
            if (ISZERO(rhs)) {
                status |= DEC_Invalid_operation;
                break;
            }
        }
        #endif

        decContextDefault(&aset, DEC_INIT_DECIMAL64);
        aset.emax = DEC_MAX_MATH;
        aset.emin = -DEC_MAX_MATH;
        aset.clamp = 0;
        aset.round = set->round;
        aset.traps = set->traps;

        if (!(rhs->bits & (DECNEG | DECSPECIAL)) && !ISZERO(rhs)) {
            Int residue = 0;
            uInt copystat = 0;

            aset.digits = 1;
            decCopyFit(w, rhs, &aset, &residue, &copystat);
            
            if (!(copystat & DEC_Inexact) && w->lsu[0] == 1 && residue == 0) {
                decNumberFromInt32(w, w->exponent);
                residue = 0;
                decCopyFit(res, w, set, &residue, &status);
                decFinish(res, set, &residue, &status);
                break;
            }
            aset.digits = set->digits; 
        }

        p_ln_rhs = (rhs->digits + LN_PREC_ADJUST_FOR_EXPONENT_DIGITS > set->digits
                   ? rhs->digits + LN_PREC_ADJUST_FOR_EXPONENT_DIGITS : set->digits)
                   + GUARD_DIGITS_FOR_INTERMEDIATE_CALCS;
        
        a = allocateTempDecNumber(bufa, sizeof(bufa), &allocbufa, p_ln_rhs, &status);
        if (a == NULL) break;

        aset.digits = p_ln_rhs;
        decLnOp(a, rhs, &aset, &status);

        if ((status & DEC_NaNs) && !(status & DEC_sNaN)) break;
        if (a->bits & DECSPECIAL || ISZERO(a)) {
            decNumberCopy(res, a);
            break;
        }

        p_ln_10 = set->digits + GUARD_DIGITS_FOR_INTERMEDIATE_CALCS;
        
        b = allocateTempDecNumber(bufb, sizeof(bufb), &allocbufb, p_ln_10, &status);
        if (b == NULL) break;

        decNumberZero(w);
        #if DECDPUN==1
        w->lsu[1] = 1;
        w->lsu[0] = 0;
        #else
        w->lsu[0] = 10;
        #endif
        w->digits = 2;

        aset.digits = p_ln_10;
        decLnOp(b, w, &aset, &ignore_status);

        aset.digits = set->digits;
        decDivideOp(res, a, b, &aset, DIVIDE, &status);
    } while(0);

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
  uInt status = 0; // Initialize status accumulator
  decCompareOp(res, lhs, rhs, set, COMPMAX, &status);
  if (status != 0) { // Check if any operation status flags were set
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
  uInt status=0;
  decCompareOp(res, lhs, rhs, set, COMPMAXMAG, &status);
  if (status!=0) decStatus(res, status, set);
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
  uInt status = 0;
  decCompareOp(res, lhs, rhs, set, COMPMIN, &status);
  if (status) {
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
decNumber * decNumberMinMag(decNumber *res, const decNumber *lhs,
			 const decNumber *rhs, decContext *set) {
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
  if (decCheckOperands(res, DECUNUSED, rhs, set)) {
    return res;
  }
  #endif

  decNumberZero(&dzero);
  dzero.exponent = rhs->exponent;
  decAddOp(res, &dzero, rhs, set, DECNEG, &status);
  if (status != 0) {
    decStatus(res, status, set);
  }
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
  decContext workset = *set;
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

  workset.round = DEC_ROUND_FLOOR;
  decAddOp(res, rhs, &dtiny, &workset, DECNEG, &status);

  status &= DEC_Invalid_operation | DEC_sNaN;
  if (status != 0) decStatus(res, status, set);
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
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) {
      return res;
  }
  #endif

  if ((rhs->bits & (DECINF | DECNEG)) == (DECINF | DECNEG)) {
    decSetMaxValue(res, set);
    res->bits |= DECNEG;
    return res;
  }

  decContext workset = *set;
  workset.round = DEC_ROUND_CEILING;

  decNumber dtiny;
  decNumberZero(&dtiny);
  dtiny.lsu[0] = 1;
  dtiny.exponent = DEC_MIN_EMIN - 1;

  uInt status = 0;
  decAddOp(res, rhs, &dtiny, &workset, 0, &status);

  const uInt NEXT_PLUS_ERROR_MASK = DEC_Invalid_operation | DEC_sNaN;
  status &= NEXT_PLUS_ERROR_MASK;

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
  decContext workset = *set;
  uInt status = 0;
  Int compare_result;

  if (decNumberIsNaN(lhs) || decNumberIsNaN(rhs)) {
    decNaNs(res, lhs, rhs, set, &status);
  } else {
    compare_result = decCompare(lhs, rhs, 0);
    if (compare_result == BADINT) {
      status |= DEC_Insufficient_storage;
    } else if (compare_result == 0) {
      decNumberCopySign(res, lhs, rhs);
    } else {
      uByte sub_op;
      decNumber dtiny_val;

      if (compare_result < 0) {
        if ((lhs->bits & (DECINF | DECNEG)) == (DECINF | DECNEG)) {
          decSetMaxValue(res, set);
          res->bits = DECNEG;
          return res;
        }
        workset.round = DEC_ROUND_CEILING;
        sub_op = 0;
      } else {
        if ((lhs->bits & (DECINF | DECNEG)) == DECINF) {
          decSetMaxValue(res, set);
          return res;
        }
        workset.round = DEC_ROUND_FLOOR;
        sub_op = DECNEG;
      }

      decNumberZero(&dtiny_val);
      dtiny_val.lsu[0] = 1;
      dtiny_val.exponent = DEC_MIN_EMIN - 1;

      decAddOp(res, lhs, &dtiny_val, &workset, sub_op, &status);

      if (decNumberIsNormal(res, set)) {
        status = 0;
      }
    }
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
  Unit	*uc, *msuc;
  Int	msudigs;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) {
      return res;
  }
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

  for (; uc <= msuc; ++ua, ++ub, ++uc) {
    Unit current_a, current_b;

    current_a = (ua <= msua) ? *ua : 0;
    current_b = (ub <= msub) ? *ub : 0;

    *uc = 0;

    if (current_a == 0 && current_b == 0) {
      continue;
    }

    Int digits_in_current_unit = DECDPUN;
    if (uc == msuc) {
        digits_in_current_unit = msudigs;
    }

    for (Int i = 0; i < digits_in_current_unit; ++i) {
      Int digit_a = current_a % 10;
      Int digit_b = current_b % 10;

      current_a /= 10;
      current_b /= 10;

      if (digit_a > 1 || digit_b > 1) {
        decStatus(res, DEC_Invalid_operation, set);
        return res;
      }

      Int or_result_digit = digit_a | digit_b;

      if (or_result_digit == 1) {
        *uc += (Unit)powers[i];
      }
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
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  decNumber dzero_val;
  decNumberZero(&dzero_val);
  decNumberAdd(res, &dzero_val, rhs, set);

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
  /*
   * Validate input pointers to prevent NULL dereferences and ensure reliable operation.
   * If any critical input is NULL, the operation cannot proceed safely or correctly.
   * Returning NULL indicates a severe input error.
   */
  if (res == NULL || lhs == NULL || rhs == NULL || set == NULL) {
      return NULL;
  }

  uInt status = 0;
  decMultiplyOp(res, lhs, rhs, set, &status);

  /*
   * Handle any errors reported by the decMultiplyOp.
   * decStatus will update the decContext flags and potentially raise an error
   * or perform other configured actions based on the specific context settings.
   */
  if (status != 0) {
    decStatus(res, status, set);
  }

  /*
   * DECCHECK is typically a debug/testing feature. If defined,
   * it performs an additional check for inexact results.
   */
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
  Int	requested_digits = set->digits;
  Int	rhs_integer_value = 0;
  Flag	is_rhs_an_integer = 0;
  Flag	can_use_integer_path = 0;
  Flag	is_rhs_odd_integer = 0;
  Int	loop_counter;
  #if DECSUBSET
  Int	dropped_digits;
  #endif
  uInt	needed_buffer_bytes;
  Flag	seen_bit_in_power_loop = 0;
  Int	rounding_residue = 0;
  uInt	operation_status = 0;
  uByte	result_sign_bits = 0;
  decContext working_context;
  decNumber one_value;
  decNumber accumulator_buffer[D2N(DECBUFFER + 9)];
  decNumber *accumulator = accumulator_buffer;
  decNumber inverse_buffer[D2N(DECBUFFER + 9)];

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  decNumberZero(&one_value);
  one_value.lsu[0] = 1;

  #if DECSUBSET
  if (!set->extended) {
    if (lhs->digits > requested_digits) {
      alloclhs = decRoundOperand(lhs, set, &operation_status);
      if (alloclhs == NULL) goto cleanup;
      lhs = alloclhs;
    }
    if (rhs->digits > requested_digits) {
      allocrhs = decRoundOperand(rhs, set, &operation_status);
      if (allocrhs == NULL) goto cleanup;
      rhs = allocrhs;
    }
  }
  #endif

  if (SPECIALARGS) {
    if (decNumberIsNaN(lhs) || decNumberIsNaN(rhs)) {
      decNaNs(res, lhs, rhs, set, &operation_status);
      goto cleanup;
    }
    if (decNumberIsInfinite(rhs)) {
      Flag rhs_is_negative = rhs->bits & DECNEG;
      if (decNumberIsNegative(lhs) && !decNumberIsZero(lhs)) {
        operation_status |= DEC_Invalid_operation;
      } else {
        decNumberZero(res);
        decNumberCompare(accumulator, lhs, &one_value, set);
        if (decNumberIsNegative(accumulator)) {
          if (rhs_is_negative) res->bits |= DECINF;
        } else if (accumulator->lsu[0] == 0) {
          Int shift = set->digits - 1;
          res->lsu[0] = 1;
          res->digits = decShiftToMost(res->lsu, 1, shift);
          res->exponent = -shift;
          operation_status |= DEC_Inexact | DEC_Rounded;
        } else {
          if (!rhs_is_negative) res->bits |= DECINF;
        }
      }
      goto cleanup;
    }
  }

  rhs_integer_value = decGetInt(rhs);
  if (rhs_integer_value != BADINT) {
    is_rhs_an_integer = 1;
    is_rhs_odd_integer = (Flag)rhs_integer_value & 1;
    if (rhs_integer_value != BIGEVEN && rhs_integer_value != BIGODD) {
      can_use_integer_path = 1;
    }
  }

  if (decNumberIsNegative(lhs) && is_rhs_odd_integer) {
    result_sign_bits = DECNEG;
  }

  if (decNumberIsInfinite(lhs)) {
    uByte rhs_bits = rhs->bits;
    decNumberZero(res);
    if (rhs_integer_value == 0) {
      res->lsu[0] = 1;
    } else {
      if (!is_rhs_an_integer && decNumberIsNegative(lhs)) {
        operation_status |= DEC_Invalid_operation;
        goto cleanup;
      }
      if (!(rhs_bits & DECNEG)) result_sign_bits |= DECINF;
      res->bits = result_sign_bits;
    }
    goto cleanup;
  }

  if (decNumberIsZero(lhs)) {
    if (rhs_integer_value == 0) {
      #if DECSUBSET
      if (!set->extended) {
        decNumberZero(res);
        res->lsu[0] = 1;
        goto cleanup;
      }
      #endif
      operation_status |= DEC_Invalid_operation;
    } else {
      uByte rhs_bits = rhs->bits;
      if (rhs_bits & DECNEG) {
        #if DECSUBSET
        if (!set->extended) {
          operation_status |= DEC_Invalid_operation;
          goto cleanup;
        }
        #endif
        result_sign_bits |= DECINF;
      }
      decNumberZero(res);
      res->bits = result_sign_bits;
    }
    goto cleanup;
  }

  if (!can_use_integer_path) {
    if (decNumberIsNegative(lhs)) {
      operation_status |= DEC_Invalid_operation;
      goto cleanup;
    }
    if (decCheckMath(lhs, set, &operation_status) || decCheckMath(rhs, set, &operation_status)) {
      goto cleanup;
    }

    decContextDefault(&working_context, DEC_INIT_DECIMAL64);
    working_context.emax = DEC_MAX_MATH;
    working_context.emin = -DEC_MAX_MATH;
    working_context.clamp = 0;

    working_context.digits = MAXI(lhs->digits, set->digits) + 6 + 4;
  } else {
    if (rhs_integer_value == 0) {
      decNumberZero(res);
      res->lsu[0] = 1;
      goto cleanup;
    }

    if (rhs_integer_value < 0) rhs_integer_value = -rhs_integer_value;

    working_context = *set;
    working_context.round = DEC_ROUND_HALF_EVEN;
    working_context.digits = requested_digits + (rhs->digits + rhs->exponent) + 2;
    #if DECSUBSET
    if (!set->extended) working_context.digits--;
    #endif
    if (working_context.digits > DECNUMMAXP) {
      operation_status |= DEC_Invalid_operation;
      goto cleanup;
    }
  }

  needed_buffer_bytes = sizeof(decNumber) + (D2U(working_context.digits) - 1) * sizeof(Unit);
  if (needed_buffer_bytes > sizeof(accumulator_buffer)) {
    allocdac = (decNumber *)malloc(needed_buffer_bytes);
    if (allocdac == NULL) {
      operation_status |= DEC_Insufficient_storage;
      goto cleanup;
    }
    accumulator = allocdac;
  }

  if (!can_use_integer_path) {
    decLnOp(accumulator, lhs, &working_context, &operation_status);
    if (ISZERO(accumulator)) {
      accumulator->lsu[0] = 1;
      if (!is_rhs_an_integer) {
        Int shift = set->digits - 1;
        accumulator->digits = decShiftToMost(accumulator->lsu, 1, shift);
        accumulator->exponent = -shift;
        operation_status |= DEC_Inexact | DEC_Rounded;
      }
    } else {
      decMultiplyOp(accumulator, accumulator, rhs, &working_context, &operation_status);
      decExpOp(accumulator, accumulator, &working_context, &operation_status);
    }
  } else {
    decNumberZero(accumulator);
    accumulator->lsu[0] = 1;

    if (decNumberIsNegative(rhs)) {
      decNumber *inverse_lhs_ptr = inverse_buffer;
      #if DECSUBSET
      if (set->extended) {
      #endif
        decDivideOp(accumulator, &one_value, lhs, &working_context, DIVIDE, &operation_status);
        if (needed_buffer_bytes > sizeof(inverse_buffer)) {
          allocinv = (decNumber *)malloc(needed_buffer_bytes);
          if (allocinv == NULL) {
            operation_status |= DEC_Insufficient_storage;
            goto cleanup;
          }
          inverse_lhs_ptr = allocinv;
        }
        decNumberCopy(inverse_lhs_ptr, accumulator);
        decNumberCopy(accumulator, &one_value);
        lhs = inverse_lhs_ptr;
      #if DECSUBSET
      }
      #endif
    }

    seen_bit_in_power_loop = 0;
    for (loop_counter = 1; ; loop_counter++) {
      if (operation_status & (DEC_Overflow | DEC_Underflow)) {
        if (operation_status & DEC_Overflow || ISZERO(accumulator)) break;
      }
      rhs_integer_value = rhs_integer_value << 1;
      if (rhs_integer_value < 0) {
        seen_bit_in_power_loop = 1;
        decMultiplyOp(accumulator, accumulator, lhs, &working_context, &operation_status);
      }
      if (loop_counter == 31) break;
      if (!seen_bit_in_power_loop) continue;
      decMultiplyOp(accumulator, accumulator, accumulator, &working_context, &operation_status);
    }

    if (operation_status & (DEC_Overflow | DEC_Underflow)) {
      #if DECSUBSET
      if (!set->extended && decNumberIsNegative(rhs)) {
        if (operation_status & DEC_Overflow) {
          operation_status ^= DEC_Overflow | DEC_Underflow | DEC_Subnormal;
        } else {
          operation_status &= ~(DEC_Underflow | DEC_Subnormal);
          operation_status |= DEC_Overflow;
        }
      }
      #endif
      accumulator->bits = (accumulator->bits & ~DECNEG) | result_sign_bits;
      decFinalize(accumulator, set, &rounding_residue, &operation_status);
      decNumberCopy(res, accumulator);
      goto cleanup;
    }

    #if DECSUBSET
    if (!set->extended && decNumberIsNegative(rhs)) {
      decDivideOp(accumulator, &one_value, accumulator, &working_context, DIVIDE, &operation_status);
    }
    #endif
  }

  decCopyFit(res, accumulator, set, &rounding_residue, &operation_status);
  decFinish(res, set, &rounding_residue, &operation_status);
  #if DECSUBSET
  if (!set->extended) decTrim(res, set, 0, 1, &dropped_digits);
  #endif

cleanup:
  free(allocdac);
  free(allocinv);
  #if DECSUBSET
  free(alloclhs);
  free(allocrhs);
  #endif
  if (operation_status != 0) decStatus(res, operation_status, set);
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
  return decNumberReduce(res, rhs, set);
} /* decNumberNormalize */

decNumber * decNumberReduce(decNumber *res, const decNumber *rhs,
			    decContext *set) {
  uInt status = 0;
  Int  residue = 0;
  Int  dropped;
  decNumber *allocrhs = NULL;
  const decNumber *operand_to_process = rhs;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) {
    return res;
  }
  #endif

  #if DECSUBSET
  if (!set->extended && rhs->digits > set->digits) {
    allocrhs = decRoundOperand(rhs, set, &status);
    if (allocrhs == NULL) {
      goto cleanup;
    }
    operand_to_process = allocrhs;
  }
  #endif

  if (decNumberIsNaN(operand_to_process)) {
    decNaNs(res, operand_to_process, NULL, set, &status);
    goto cleanup;
  }

  decCopyFit(res, operand_to_process, set, &residue, &status);
  decFinish(res, set, &residue, &status);
  decTrim(res, set, 1, 0, &dropped);

cleanup:
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
  uInt status = 0;
  decQuantizeOp(res, lhs, rhs, set, DEC_ROUND_HALF_EVEN, &status);
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
    if (set != NULL) {
      // Use DECNUMERR from decNumber.h for a general error, typically
      // indicating an invalid operation or parameter.
      set->status |= DECNUMERR;
    }
    return NULL; // Indicate failure due to invalid input pointers
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

  if (decNumberIsNaN(lhs) || decNumberIsNaN(rhs)) {
    decNaNs(res, lhs, rhs, set, &status);
    return res;
  }

  if (decNumberIsInfinite(rhs) || rhs->exponent != 0) {
    status = DEC_Invalid_operation;
  } else {
    Int rotate_amount = decGetInt(rhs);

    if (rotate_amount == BADINT || rotate_amount == BIGODD || rotate_amount == BIGEVEN ||
        abs(rotate_amount) > set->digits) {
      status = DEC_Invalid_operation;
    } else {
      decNumberCopy(res, lhs);

      if (rotate_amount < 0) {
        rotate_amount = set->digits + rotate_amount;
      }

      if (rotate_amount != 0 && rotate_amount != set->digits && !decNumberIsInfinite(res)) {
        res->digits = set->digits;

        Unit *msu_ptr = res->lsu + D2U(lhs->digits);
        Unit *msumax_ptr = res->lsu + D2U(set->digits) - 1;
        for (; msu_ptr <= msumax_ptr; msu_ptr++) {
          *msu_ptr = 0;
        }

        uInt actual_msu_digits = MSUDIGITS(res->digits);

        Int effective_right_rotate = set->digits - rotate_amount;
        uInt units_to_shift = effective_right_rotate / DECDPUN;
        uInt partial_digit_shift = effective_right_rotate % DECDPUN;

        if (partial_digit_shift > 0) {
          uInt saved_low_digits = res->lsu[0] % powers[partial_digit_shift];
          decShiftToLeast(res->lsu, D2U(res->digits), partial_digit_shift);

          if (partial_digit_shift > actual_msu_digits) {
            uInt remainder_digits = saved_low_digits % powers[partial_digit_shift - actual_msu_digits];
            *msumax_ptr = (Unit)(saved_low_digits / powers[partial_digit_shift - actual_msu_digits]);
            *(msumax_ptr - 1) += (Unit)(remainder_digits * powers[DECDPUN - (partial_digit_shift - actual_msu_digits)]);
          } else {
            *msumax_ptr += (Unit)(saved_low_digits * powers[actual_msu_digits - partial_digit_shift]);
          }
        }

        if (units_to_shift > 0) {
          uInt msd_align_shift = DECDPUN - actual_msu_digits;
          if (msd_align_shift > 0) {
            uInt saved_msd_align_digits = res->lsu[0] % powers[msd_align_shift];
            decShiftToLeast(res->lsu, units_to_shift, msd_align_shift);
            *msumax_ptr += (Unit)(saved_msd_align_digits * powers[actual_msu_digits]);
          }

          decReverse(res->lsu + units_to_shift, msumax_ptr);
          decReverse(res->lsu, res->lsu + units_to_shift - 1);
          decReverse(res->lsu, msumax_ptr);
        }

        res->digits = decGetDigits(res->lsu, msumax_ptr - res->lsu + 1);
      }
    }
  }

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
  Unit result_value = 0;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, DECUNCONT)) return res;
  #endif

  if (SPECIALARGS) {
    if ((decNumberIsNaN(lhs) && decNumberIsNaN(rhs)) ||
        (decNumberIsInfinite(lhs) && decNumberIsInfinite(rhs))) {
      result_value = 1;
    }
  } else {
    if (lhs->exponent == rhs->exponent) {
      result_value = 1;
    }
  }

  decNumberZero(res);
  *res->lsu = result_value;
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
  Int  reqexp;
  uInt status = 0;
  Int  residue;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) {
    return res;
  }
  #endif

  if (decNumberIsNaN(lhs) || decNumberIsNaN(rhs)) {
    decNaNs(res, lhs, rhs, set, &status);
  } else if (decNumberIsInfinite(rhs) || rhs->exponent != 0) {
    status |= DEC_Invalid_operation;
  } else {
    reqexp = decGetInt(rhs);
    if (reqexp == BADINT || reqexp == BIGODD || reqexp == BIGEVEN ||
        abs(reqexp) > (2 * (set->digits + set->emax))) {
      status |= DEC_Invalid_operation;
    }
  }

  if (status == 0) {
    decNumberCopy(res, lhs);

    if (!decNumberIsInfinite(res)) {
      res->exponent += reqexp;
      residue = 0;
      decFinalize(res, set, &residue, &status);
    }
  }

  if (status != 0) {
    decStatus(res, status, set);
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
  Int  shift;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  if (decNumberIsNaN(lhs) || decNumberIsNaN(rhs)) {
    decNaNs(res, lhs, rhs, set, &status);
  } else if (decNumberIsInfinite(rhs) || rhs->exponent != 0) {
    status = DEC_Invalid_operation;
  } else {
    shift = decGetInt(rhs);

    if (shift == BADINT || shift == BIGODD || shift == BIGEVEN || abs(shift) > set->digits) {
      status = DEC_Invalid_operation;
    } else {
      decNumberCopy(res, lhs);

      if (shift != 0 && !decNumberIsInfinite(res)) {
        if (shift > 0) {
          if (res->digits + shift > set->digits) {
            decDecap(res, res->digits + shift - set->digits);
          }
          if (res->digits > 1 || *res->lsu != 0) {
            res->digits = decShiftToMost(res->lsu, res->digits, shift);
          }
        } else {
          if (-shift >= res->digits) {
            *res->lsu = 0;
            res->digits = 1;
          } else {
            decShiftToLeast(res->lsu, D2U(res->digits), -shift);
            res->digits -= (-shift);
          }
        }
      }
    }
  }

  if (status != 0) {
    decStatus(res, status, set);
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
  decNumber zero_const;
  Int max_working_precision;
  Int current_working_precision;
  Int rounding_residue = 0;
  uInt accumulated_status = 0;
  uInt ignored_status = 0;
  uInt last_operation_status;
  Int adjusted_exponent;
  Int ideal_exponent;
  Int bytes_needed;
  Int dropped_trailing_zeros;

  decNumber *allocated_fraction_num = NULL;
  decNumber *allocated_approx_num = NULL;
  decNumber *allocated_temp_num = NULL;
  #if DECSUBSET
  decNumber *allocated_rhs_rounded = NULL;
  #endif

  decNumber fraction_num_stack[D2N(DECBUFFER + 1)];
  decNumber approx_num_stack[D2N(DECBUFFER + 2)];
  decNumber temp_num_stack[D2N(DECBUFFER + 2)];
  decNumber constant_num_stack[D2N(3)];

  decNumber *fraction_ptr = fraction_num_stack;
  decNumber *approx_ptr = approx_num_stack;
  decNumber *temp_ptr = temp_num_stack;
  decNumber *constant_ptr = constant_num_stack;

  decNumberFromInt32(&zero_const, 0);

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  #if DECSUBSET
  if (!set->extended) {
    if (rhs->digits > set->digits) {
      allocated_rhs_rounded = decRoundOperand(rhs, set, &accumulated_status);
      if (allocated_rhs_rounded == NULL) {
        accumulated_status |= DEC_Insufficient_storage;
        goto cleanup;
      }
      rhs = allocated_rhs_rounded;
    }
  }
  #endif

  if (decNumberIsSpecial(rhs)) {
    if (decNumberIsInfinite(rhs)) {
      if (decNumberIsNegative(rhs)) accumulated_status |= DEC_Invalid_operation;
      else decNumberCopy(res, rhs);
    } else {
      decNaNs(res, rhs, NULL, set, &accumulated_status);
    }
    goto cleanup;
  }

  if (decNumberIsZero(rhs)) {
    decNumberCopy(res, rhs);
    ideal_exponent = (rhs->exponent & ~1) / 2;
    res->exponent = ideal_exponent;
    decFinish(res, set, &rounding_residue, &accumulated_status);
    goto cleanup;
  }

  if (decNumberIsNegative(rhs)) {
    accumulated_status |= DEC_Invalid_operation;
    goto cleanup;
  }

  current_working_precision = MAXI(set->digits + 1, rhs->digits);
  current_working_precision = MAXI(current_working_precision, 7);
  max_working_precision = current_working_precision + 2;

  bytes_needed = sizeof(decNumber) + (D2U(rhs->digits) - 1) * sizeof(Unit);
  if (bytes_needed > (Int)sizeof(fraction_num_stack)) {
    allocated_fraction_num = (decNumber *)malloc(bytes_needed);
    if (allocated_fraction_num == NULL) {
      accumulated_status |= DEC_Insufficient_storage;
      goto cleanup;
    }
    fraction_ptr = allocated_fraction_num;
  }

  bytes_needed = sizeof(decNumber) + (D2U(max_working_precision) - 1) * sizeof(Unit);
  if (bytes_needed > (Int)sizeof(approx_num_stack)) {
    allocated_approx_num = (decNumber *)malloc(bytes_needed);
    allocated_temp_num = (decNumber *)malloc(bytes_needed);
    if (allocated_approx_num == NULL || allocated_temp_num == NULL) {
      accumulated_status |= DEC_Insufficient_storage;
      goto cleanup;
    }
    approx_ptr = allocated_approx_num;
    temp_ptr = allocated_temp_num;
  }

  decNumberCopy(fraction_ptr, rhs);
  adjusted_exponent = fraction_ptr->exponent + fraction_ptr->digits;
  fraction_ptr->exponent = -(fraction_ptr->digits);

  decContextDefault(&workset, DEC_INIT_DECIMAL64);
  workset.emax = DEC_MAX_EMAX;
  workset.emin = DEC_MIN_EMIN;

  workset.digits = current_working_precision;

  if ((adjusted_exponent & 1) == 0) {
    decNumberFromString(constant_ptr, "0.259", NULL);
    decNumberFromString(approx_ptr, "0.819", NULL);
  } else {
    fraction_ptr->exponent--;
    adjusted_exponent++;
    decNumberFromString(constant_ptr, "0.0819", NULL);
    decNumberFromString(approx_ptr, "2.59", NULL);
  }

  decMultiplyOp(approx_ptr, approx_ptr, fraction_ptr, &workset, &ignored_status);
  decAddOp(approx_ptr, approx_ptr, constant_ptr, &workset, 0, &ignored_status);

  decNumberFromString(constant_ptr, "0.5", NULL);
  workset.digits = 3;

  for (; workset.digits < max_working_precision;) {
    workset.digits = MINI(workset.digits * 2 - 2, max_working_precision);

    decDivideOp(temp_ptr, fraction_ptr, approx_ptr, &workset, DEC_DIVIDE, &ignored_status);
    decAddOp(temp_ptr, temp_ptr, approx_ptr, &workset, 0, &ignored_status);
    decMultiplyOp(approx_ptr, temp_ptr, constant_ptr, &workset, &ignored_status);
  }

  approxset = *set;
  approxset.round = DEC_ROUND_HALF_EVEN;
  approx_ptr->exponent += adjusted_exponent / 2;
  last_operation_status = 0;
  rounding_residue = 0;

  decCopyFit(approx_ptr, approx_ptr, &approxset, &rounding_residue, &last_operation_status);
  decFinish(approx_ptr, &approxset, &rounding_residue, &last_operation_status);

  if (last_operation_status & DEC_Overflow) {
    accumulated_status = last_operation_status;
    decNumberCopy(res, approx_ptr);
    goto cleanup;
  }

  accumulated_status |= (last_operation_status & ~(DEC_Rounded | DEC_Inexact));

  approx_ptr->exponent -= adjusted_exponent / 2;

  workset.digits = max_working_precision - 1;
  workset.round = DEC_ROUND_HALF_EVEN;

  decNumberFromInt32(constant_ptr, 5);
  constant_ptr->exponent = -(approx_ptr->digits + 1);

  decAddOp(temp_ptr, approx_ptr, constant_ptr, &workset, DEC_NEG, &ignored_status);
  workset.round = DEC_ROUND_UP;
  decMultiplyOp(temp_ptr, temp_ptr, temp_ptr, &workset, &ignored_status);

  decCompareOp(temp_ptr, fraction_ptr, temp_ptr, &workset, DEC_COMPARE, &ignored_status);
  if (decNumberIsNegative(temp_ptr)) {
    decNumberFromInt32(constant_ptr, 1);
    constant_ptr->exponent = -approx_ptr->digits;
    decAddOp(approx_ptr, approx_ptr, constant_ptr, &workset, DEC_NEG, &ignored_status);

    approxset.emin -= adjusted_exponent / 2;
    approxset.emax -= adjusted_exponent / 2;
    decAddOp(approx_ptr, &zero_const, approx_ptr, &approxset, 0, &ignored_status);
  } else {
    decNumberFromInt32(constant_ptr, 5);
    constant_ptr->exponent = -(approx_ptr->digits + 1);
    decAddOp(temp_ptr, approx_ptr, constant_ptr, &workset, 0, &ignored_status);
    workset.round = DEC_ROUND_DOWN;
    decMultiplyOp(temp_ptr, temp_ptr, temp_ptr, &workset, &ignored_status);

    decCompareOp(temp_ptr, temp_ptr, fraction_ptr, &workset, DEC_COMPARE, &ignored_status);
    if (decNumberIsNegative(temp_ptr)) {
      decNumberFromInt32(constant_ptr, 1);
      constant_ptr->exponent = -approx_ptr->digits;
      decAddOp(approx_ptr, approx_ptr, constant_ptr, &workset, 0, &ignored_status);

      approxset.emin -= adjusted_exponent / 2;
      approxset.emax -= adjusted_exponent / 2;
      decAddOp(approx_ptr, &zero_const, approx_ptr, &approxset, 0, &ignored_status);
    }
  }

  approx_ptr->exponent += adjusted_exponent / 2;

  decNumberCopy(temp_ptr, approx_ptr);
  decTrim(temp_ptr, set, 1, 1, &dropped_trailing_zeros);

  if (temp_ptr->digits * 2 - 1 > current_working_precision) {
    accumulated_status |= DEC_Inexact | DEC_Rounded;
  } else {
    uInt multiply_status = 0;
    decMultiplyOp(temp_ptr, temp_ptr, temp_ptr, &workset, &multiply_status);

    if (multiply_status & DEC_Overflow) {
      accumulated_status |= DEC_Inexact | DEC_Rounded;
    } else {
      decCompareOp(constant_ptr, temp_ptr, rhs, &workset, DEC_COMPARE, &multiply_status);
      if (!decNumberIsZero(constant_ptr)) {
        accumulated_status |= DEC_Inexact | DEC_Rounded;
      } else {
        Int to_drop_count = ideal_exponent - approx_ptr->exponent;
        if (to_drop_count < 0) {
          accumulated_status |= DEC_Rounded;
        } else {
          Int max_allowable_exp = set->emax - set->digits + 1;
          Int max_droppable_by_emax = max_allowable_exp - approx_ptr->exponent;
          if (to_drop_count > max_droppable_by_emax && set->clamp) {
            to_drop_count = max_droppable_by_emax;
            accumulated_status |= DEC_Clamped;
          }
          if (dropped_trailing_zeros < to_drop_count) {
            to_drop_count = dropped_trailing_zeros;
            accumulated_status |= DEC_Clamped;
          }
          if (to_drop_count > 0) {
            decShiftToLeast(approx_ptr->lsu, D2U(approx_ptr->digits), to_drop_count);
            approx_ptr->exponent += to_drop_count;
            approx_ptr->digits -= to_drop_count;
          }
        }
      }
    }
  }

  if (accumulated_status & DEC_Underflow) {
    Int adjusted_rhs_exp = rhs->exponent + rhs->digits - 1;
    #if DECEXTFLAG
    if (adjusted_rhs_exp >= set->emin * 2) accumulated_status &= ~(DEC_Subnormal | DEC_Underflow);
    #else
    if (adjusted_rhs_exp >= set->emin * 2) accumulated_status &= ~DEC_Underflow;
    #endif
    if (!(accumulated_status & DEC_Inexact)) accumulated_status &= ~DEC_Underflow;
  }

  decNumberCopy(res, approx_ptr);

cleanup:
  free(allocated_fraction_num);
  free(allocated_approx_num);
  free(allocated_temp_num);
  #if DECSUBSET
  free(allocated_rhs_rounded);
  #endif

  if (accumulated_status != 0) decStatus(res, accumulated_status, set);
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
decNumber *decNumberSubtract(decNumber *res, const decNumber *lhs,
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
  uInt status=0;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  if (decNumberIsSpecial(rhs)) {
    if (decNumberIsInfinite(rhs)) {
      decNumberCopy(res, rhs);
    } else {
      decNaNs(res, rhs, NULL, set, &status);
    }
  } else {
    if (rhs->exponent >= 0) {
      return decNumberCopy(res, rhs);
    }
    workset = *set;
    workset.digits = rhs->digits;
    workset.traps = 0;
    decNumberZero(&dn);
    decNumberQuantize(res, rhs, &dn, &workset);
    status |= workset.status;
  }
  if (status != 0) {
    decStatus(res, status, set);
  }
  return res;
} /* decNumberToIntegralExact */

decNumber * decNumberToIntegralValue(decNumber *res, const decNumber *rhs,
				     decContext *set) {
  decContext workset = *set;
  workset.traps = 0;
  workset.status = 0;

  decNumberToIntegralExact(res, rhs, &workset);

  set->status |= workset.status & DEC_Invalid_operation;
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
  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  // Validate operands: must be non-negative integers (exponent 0), and not special.
  if (lhs->exponent != 0 || decNumberIsSpecial(lhs) || decNumberIsNegative(lhs) ||
      rhs->exponent != 0 || decNumberIsSpecial(rhs) || decNumberIsNegative(rhs)) {
    decStatus(res, DEC_Invalid_operation, set);
    return res;
  }

  const Unit *lhs_unit_ptr = lhs->lsu;
  const Unit *rhs_unit_ptr = rhs->lsu;
  Unit       *res_unit_ptr = res->lsu;

  // Pointers to the Most Significant Units (MSUs)
  const Unit *lhs_msu_ptr = lhs_unit_ptr + D2U(lhs->digits) - 1;
  const Unit *rhs_msu_ptr = rhs_unit_ptr + D2U(rhs->digits) - 1;
  Unit       *res_msu_ptr = res_unit_ptr + D2U(set->digits) - 1;

  // Number of digits in the result's MSU (handles cases where total digits is not a multiple of DECDPUN)
  Int res_msu_max_digits = MSUDIGITS(set->digits);

  // Iterate through units, from Least Significant Unit (LSU) to MSU
  for (; res_unit_ptr <= res_msu_ptr; ++lhs_unit_ptr, ++rhs_unit_ptr, ++res_unit_ptr) {
    Unit current_lhs_val;
    Unit current_rhs_val;
    Int  digits_to_process;

    // Get current unit values, defaulting to 0 if out of bounds (padding with leading zeros)
    current_lhs_val = (lhs_unit_ptr > lhs_msu_ptr) ? 0 : *lhs_unit_ptr;
    current_rhs_val = (rhs_unit_ptr > rhs_msu_ptr) ? 0 : *rhs_unit_ptr;

    // Initialize the current result unit to zero
    *res_unit_ptr = 0;

    // Determine how many digits to process in this unit
    if (res_unit_ptr == res_msu_ptr) {
      digits_to_process = res_msu_max_digits;
    } else {
      digits_to_process = DECDPUN;
    }

    // Iterate through decimal digits within the current unit
    for (Int i = 0; i < digits_to_process; ++i) {
      Int digit_lhs = current_lhs_val % 10;
      current_lhs_val /= 10;

      Int digit_rhs = current_rhs_val % 10;
      current_rhs_val /= 10;

      // Critical validation: all input digits must be 0 or 1.
      // This also implicitly catches invalid decimal numbers (e.g., if a unit contains 'A', %10 wouldn't be right).
      if (digit_lhs > 1 || digit_rhs > 1) {
        decStatus(res, DEC_Invalid_operation, set);
        return res;
      }

      // Perform the digit-wise XOR operation
      Int result_digit = digit_lhs ^ digit_rhs;

      // Add the resulting digit to the result unit at the correct decimal place
      *res_unit_ptr += (Unit)(result_digit * powers[i]);
    }
  }

  // Final result normalization
  res->digits = decGetDigits(res->lsu, res_unit_ptr - res->lsu); // Calculate actual significant digits
  res->exponent = 0; // Result is an integer
  res->bits = 0;     // Sign is 0 (non-negative)
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
    int isNegative = decNumberIsNegative(dn);

    if (decNumberIsSpecial(dn)) {
        if (decNumberIsQNaN(dn)) {
            return DEC_CLASS_QNAN;
        }
        if (decNumberIsSNaN(dn)) {
            return DEC_CLASS_SNAN;
        }
        // If special but not QNaN/SNaN, it must be an infinity.
        return isNegative ? DEC_CLASS_NEG_INF : DEC_CLASS_POS_INF;
    }

    // If not special, it is finite.
    if (decNumberIsNormal(dn, set)) {
        return isNegative ? DEC_CLASS_NEG_NORMAL : DEC_CLASS_POS_NORMAL;
    }

    // If not normal, it's either zero or subnormal.
    if (decNumberIsZero(dn)) {
        return isNegative ? DEC_CLASS_NEG_ZERO : DEC_CLASS_POS_ZERO;
    }

    // If it's not special, not normal, and not zero, it must be subnormal.
    return isNegative ? DEC_CLASS_NEG_SUBNORMAL : DEC_CLASS_POS_SUBNORMAL;
} /* decNumberClass */

/* ------------------------------------------------------------------ */
/* decNumberClassToString -- convert decClass to a string	      */
/*								      */
/*  eclass is a valid decClass					      */
/*  returns a constant string describing the class (max 13+1 chars)   */
/* ------------------------------------------------------------------ */
const char *decNumberClassToString(enum decClass eclass) {
  switch (eclass) {
    case DEC_CLASS_POS_NORMAL:
      return DEC_ClassString_PN;
    case DEC_CLASS_NEG_NORMAL:
      return DEC_ClassString_NN;
    case DEC_CLASS_POS_ZERO:
      return DEC_ClassString_PZ;
    case DEC_CLASS_NEG_ZERO:
      return DEC_ClassString_NZ;
    case DEC_CLASS_POS_SUBNORMAL:
      return DEC_ClassString_PS;
    case DEC_CLASS_NEG_SUBNORMAL:
      return DEC_ClassString_NS;
    case DEC_CLASS_POS_INF:
      return DEC_ClassString_PI;
    case DEC_CLASS_NEG_INF:
      return DEC_ClassString_NI;
    case DEC_CLASS_QNAN:
      return DEC_ClassString_QN;
    case DEC_CLASS_SNAN:
      return DEC_ClassString_SN;
    default:
      return DEC_ClassString_UN;
  }
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
  if (dest == NULL) {
    return NULL;
  }
  if (src == NULL) {
    return decNumberZero(dest);
  }
  if (dest == src) {
    return dest;
  }
  dest->bits = src->bits;
  dest->exponent = src->exponent;
  dest->digits = src->digits;
  dest->lsu[0] = src->lsu[0];
  if (src->digits > DECDPUN) {
    size_t units_to_copy_count = D2U(src->digits) - 1;
    memcpy(dest->lsu + 1, src->lsu + 1, units_to_copy_count * sizeof(Unit));
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
  if (res == NULL) {
    return res;
  }
#if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, DECUNCONT)) {
    return res;
  }
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
  if (res == NULL || rhs == NULL) {
    return NULL;
  }
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, DECUNCONT)) {
    return res;
  }
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
  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, DECUNCONT)) {
    return res;
  }
  #endif

  decNumberCopy(res, lhs);
  res->bits = (res->bits & ~DECNEG) | (rhs->bits & DECNEG);

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
  if (dn == NULL || bcd == NULL) {
    return NULL;
  }

  uByte *ub = bcd + dn->digits - 1;
  const Unit *up = dn->lsu;

  #if DECDPUN == 1
    for (; ub >= bcd; ub--, up++) {
      *ub = (uByte)*up;
    }
  #else
    uInt current_unit_val = 0;
    int digits_remaining_in_unit = 0;

    for (; ub >= bcd; ub--) {
      if (digits_remaining_in_unit == 0) {
        current_unit_val = *up;
        up++;
        digits_remaining_in_unit = DECDPUN;
      }

      *ub = (uByte)(current_unit_val % 10);
      current_unit_val /= 10;
      digits_remaining_in_unit--;
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
  if (dn == NULL) {
    return NULL;
  }
  if (n > 0 && bcd == NULL) {
    return NULL;
  }
  if (n == 0) {
    dn->digits = 0;
    return dn;
  }
  uInt units_needed = D2U(n);
  Unit *up = dn->lsu + units_needed - 1;
  const uByte *ub = bcd;
  #if DECDPUN==1
    for (uInt i = 0; i < n; i++) {
      *up = *ub;
      ub++;
      up--;
    }
  #else
    Int digits_in_msu = MSUDIGITS(n);
    for (uInt i = 0; i < units_needed; i++) {
      Unit current_unit_val = 0;
      Int digits_to_pack;
      if (i == 0) {
        digits_to_pack = digits_in_msu;
      } else {
        digits_to_pack = DECDPUN;
      }
      for (Int j = 0; j < digits_to_pack; j++) {
        current_unit_val = X10(current_unit_val) + (*ub);
        ub++;
      }
      *up = current_unit_val;
      up--;
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
#if DECCHECK
  if (decCheckOperands(DECUNRESU, DECUNUSED, dn, set)) {
    return 0;
  }
#endif
  if (decNumberIsSpecial(dn) ||
      decNumberIsZero(dn) ||
      (dn->exponent + dn->digits - 1 < set->emin)) {
    return 0;
  }
  return 1;
} /* decNumberIsNormal */

/* ------------------------------------------------------------------ */
/* decNumberIsSubnormal -- test subnormality of a decNumber	      */
/*   dn is the decNumber to test				      */
/*   set is the context to use for Emin 			      */
/*   returns 1 if |dn| is finite, non-zero, and <Nmin, 0 otherwise    */
/* ------------------------------------------------------------------ */
Int decNumberIsSubnormal(const decNumber *dn, decContext *set) {
  if (dn == NULL || set == NULL) {
    return 0;
  }

  if (decNumberIsSpecial(dn)) {
    return 0;
  }

  if (decNumberIsZero(dn)) {
    return 0;
  }

  Int adjustedExponent = dn->exponent + dn->digits - 1;

  if (adjustedExponent < set->emin) {
    return 1;
  }

  return 0;
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
#ifndef DECCHECK
  if (dn == NULL) {
    return NULL;
  }
#endif

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
const char * decNumberVersion(void) {
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
  if (dn == NULL) {
    return NULL;
  }

  #if DECCHECK
  if (decCheckOperands(dn, DECUNUSED, DECUNUSED, DECUNCONT)) {
      return dn;
  }
  #endif

  dn->bits=0;
  dn->exponent=0;
  dn->digits=1;
  dn->lsu[0]=0;
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
  Int exp = dn->exponent;       /* local copy */
  Int e;                        /* E-part value */
  Int pre;                      /* digits before the '.' */
  Int cut;                      /* for counting digits in a Unit */
  char *c = string;             /* work [output pointer] */
  // Initialize up to point to the most significant unit (msu).
  // D2U(dn->digits) gives the total number of Units needed. lsu is the least significant unit.
  const Unit *up = dn->lsu + D2U(dn->digits) - 1;
  uInt u, pow;                  /* work */

  #if DECCHECK
  if (decCheckOperands(DECUNRESU, dn, DECUNUSED, DECUNCONT)) {
    strcpy(string, "?");
    return;
  }
  #endif

  if (decNumberIsNegative(dn)) {   /* Negatives get a minus */
    *c++ = '-';
  }

  if (dn->bits & DECSPECIAL) {     /* Is a special value */
    if (decNumberIsInfinite(dn)) {
      strcpy(c, "Infinity");
      return;
    }
    /* a NaN */
    if (dn->bits & DECSNAN) {      /* signalling NaN */
      *c++ = 's';
    }
    strcpy(c, "NaN");
    c += 3;                        /* step past */
    /* if not a clean non-zero coefficient, that's all there is in a */
    /* NaN string */
    // The original logic here is specific: drop through only for non-zero coefficient, zero exponent NaN.
    if (exp != 0 || (*dn->lsu == 0 && dn->digits == 1)) {
        return;
    }
    // [drop through to add integer for non-0 coefficient, 0 exponent NaN, e.g., 123NaN]
  }

  /* calculate how many digits in msu, and hence first cut */
  cut = MSUDIGITS(dn->digits) - 1; /* power of ten for digit */
  u = *up;                         /* load msu Unit value; used below if not fastpath */

  if (exp == 0) {                  /* simple integer [common fastpath] */
    for (; up >= dn->lsu; up--) {  /* each Unit from msu */
      u = *up;                     /* contains DECDPUN digits to lay out */
      for (; cut >= 0; c++, cut--) TODIGIT(u, cut, c, pow);
      cut = DECDPUN - 1;           /* next Unit has all digits */
    }
    *c = '\0';                     /* terminate the string */
    return;
  }

  /* non-0 exponent -- assume plain form initially */
  pre = dn->digits + exp;          /* digits before '.' */
  e = 0;                           /* no E initially */

  // Determine if exponential form is needed and calculate initial E value
  if ((exp > 0) || (pre < -5)) {
    e = exp + dn->digits - 1;      /* calculate E value */
    pre = 1;                       /* assume one digit before '.' */

    if (eng && (e != 0)) {         /* engineering: may need to adjust */
      Int adj;
      adj = (e % 3 + 3) % 3;       /* Ensures positive remainder for both positive and negative e */

      e = e - adj;
      if (!ISZERO(dn)) {           /* Assuming ISZERO is decNumberIsZero */
          pre += adj;
      } else {                     /* is zero */
        if (adj != 0) {            /* 0.00Esnn needed */
          e = e + 3;
          pre = -(2 - adj);
        }
      } /* zero */
    } /* eng */
  } /* need exponent */

  // Phase 1: Print leading '0.' and any required zeros after decimal (for pre <= 0)
  if (pre <= 0) {
    *c++ = '0';
    *c++ = '.';
    for (; pre < 0; pre++) { // Add any 0's after '.' (e.g., 0.00xxx)
      *c++ = '0';
    }
  }

  // Phase 2: Print coefficient digits
  // Iterates through `dn->digits` digits from MSB to LSB.
  for (Int i = 0; i < dn->digits; i++) {
    // If current Unit is exhausted, advance to the previous Unit
    if (cut < 0) {
      // If we've exhausted all Units but still have digits_to_process, it indicates an issue
      // with the decNumber's internal state (e.g., dn->digits doesn't match actual units).
      // Or it implies we should pad with zeros, which is handled in Phase 3.
      if (up == dn->lsu) { break; } // Defensive break, should ideally not be reached
      up--;
      cut = DECDPUN - 1;
      u = *up;
    }
    TODIGIT(u, cut, c, pow);
    c++;
    cut--;

    // Phase 2a: Insert decimal point if needed after 'pre' digits
    // The decimal point is inserted after `pre` digits if `pre` is positive
    // and there are still more coefficient digits to print after the point.
    if (pre > 0 && (i + 1) == pre) {
        if ((i + 1) < dn->digits) { // Only insert '.' if there are more coefficient digits to follow
           *c++ = '.';
        }
    }
  }

  // Phase 3: Add any required trailing zeros
  // This covers cases like "12300" (where pre > dn->digits) or engineering notation padding
  for (; pre > dn->digits; pre--) {
    *c++ = '0';
  }

  /* Finally add the E-part, if needed. */
  if (e != 0) {
    Flag had_non_zero_exp_digit = 0;
    *c++ = 'E';
    *c++ = (e < 0) ? '-' : '+';
    u = (e < 0) ? -e : e; // Use absolute value for digit extraction

    // Lay out the exponent digits (max 10 digits for exponent value, from 9 down to 0)
    for (cut = 9; cut >= 0; cut--) {
      TODIGIT(u, cut, c, pow);
      if (*c == '0' && !had_non_zero_exp_digit) {
          // If it's the last digit (cut == 0) and we haven't printed any non-zero digits yet,
          // then this '0' is significant (e.g., for E+0). Print it.
          if (cut == 0) {
              had_non_zero_exp_digit = 1; // Mark that we're now printing
              c++;
          }
          continue; // Skip leading zeros
      }
      had_non_zero_exp_digit = 1; // Mark that we've encountered a non-zero digit
      c++;                        // Step for next
    }
  }
  *c = '\0';	    /* terminate the string (all paths) */
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
  decNumber *alloclhs=NULL;
  decNumber *allocrhs=NULL;
  #endif
  Int	rhsshift;
  Int	maxdigits;
  Int	mult;
  Int	residue;
  uByte bits;
  Flag	diffsign;
  Unit	*acc;
  Unit	accbuff[SD2U(DECBUFFER*2+20)];
  Unit	*allocacc=NULL;
  Int	reqdigits=set->digits;
  Int	padding;

  #if DECCHECK
  if (decCheckOperands(res, lhs, rhs, set)) return res;
  #endif

  do {
    #if DECSUBSET
    if (!set->extended) {
      if (lhs->digits>reqdigits) {
	alloclhs=decRoundOperand(lhs, set, status);
	if (alloclhs==NULL) break;
	lhs=alloclhs;
	}
      if (rhs->digits>reqdigits) {
	allocrhs=decRoundOperand(rhs, set, status);
	if (allocrhs==NULL) break;
	rhs=allocrhs;
	}
      }
    #endif

    diffsign=(Flag)((lhs->bits^rhs->bits^negate)&DECNEG);

    if (SPECIALARGS) {
      if (SPECIALARGS & (DECSNAN | DECNAN))
	decNaNs(res, lhs, rhs, set, status);
       else {
	if (decNumberIsInfinite(lhs)) {
	  if (decNumberIsInfinite(rhs) && diffsign) {
	    *status|=DEC_Invalid_operation;
	    break;
	    }
	  bits=lhs->bits & DECNEG;
	  }
	 else bits=(rhs->bits^negate) & DECNEG;
	bits|=DECINF;
	decNumberZero(res);
	res->bits=bits;
	}
      break;
      }

    Flag lhs_is_zero = ISZERO(lhs);
    Flag rhs_is_zero = ISZERO(rhs);

    if (lhs_is_zero || rhs_is_zero) {
        Int adj_exponent;
        const decNumber *source_op;
        uByte neg_mask_for_result = 0;

        residue = 0;

        if (lhs_is_zero && rhs_is_zero) {
            decNumberZero(res);
            res->exponent = (lhs->exponent < rhs->exponent) ? lhs->exponent : rhs->exponent;
            if (diffsign) {
                if (set->round != DEC_ROUND_FLOOR) res->bits = 0;
                else res->bits = DECNEG;
            } else {
                res->bits = (lhs->bits | rhs->bits) & DECNEG;
            }
        } else {
            if (lhs_is_zero) {
                source_op = rhs;
                adj_exponent = lhs->exponent;
                neg_mask_for_result = negate;
            } else {
                source_op = lhs;
                adj_exponent = rhs->exponent;
                neg_mask_for_result = 0;
            }

            decCopyFit(res, source_op, set, &residue, status);
            res->bits ^= neg_mask_for_result;

            #if DECSUBSET
            if (set->extended) {
            #endif
                Int adjust = adj_exponent - res->exponent;
                if (adjust < 0) {
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
        }
        decFinish(res, set, &residue, status);
        break;
    }

    padding=rhs->exponent-lhs->exponent;

    Flag rhs_fits_one_unit = (rhs->digits <= DECDPUN);
    Flag exponents_aligned = (padding == 0);
    Flag rhs_exponent_in_range = (rhs->exponent >= set->emin &&
                                  rhs->exponent <= set->emax - set->digits + 1);
    Flag operands_fit_reqdigits = (rhs->digits <= reqdigits && lhs->digits <= reqdigits);

    if (exponents_aligned && rhs_fits_one_unit && rhs_exponent_in_range && operands_fit_reqdigits) {
      Int partial=*lhs->lsu;
      if (!diffsign) {
	partial+=*rhs->lsu;
	if ((partial<=DECDPUNMAX) &&
	    (lhs->digits>=DECDPUN || partial<(Int)powers[lhs->digits])) {
	  if (res!=lhs) decNumberCopy(res, lhs);
	  *res->lsu=(Unit)partial;
	  break;
	  }
	}
       else {
	partial-=*rhs->lsu;
	if (partial>0) {
	  if (res!=lhs) decNumberCopy(res, lhs);
	  *res->lsu=(Unit)partial;
	  res->digits=decGetDigits(res->lsu, D2U(res->digits));
	  break;
	  }
	}
      }

    rhsshift=0;
    bits=lhs->bits;
    mult=1;

    if (padding!=0) {
      Flag swapped=0;
      if (padding<0) {
	const decNumber *t;
	padding=-padding;
	bits=(uByte)(rhs->bits^negate);
	t=lhs; lhs=rhs; rhs=t;
	swapped=1;
	}

      if (rhs->digits+padding > lhs->digits+reqdigits+1) {
	Int shift=reqdigits-rhs->digits;
	residue=1;
	if (diffsign) residue=-residue;
	decCopyFit(res, rhs, set, &residue, status);
	if (shift>0) {
	  res->digits=decShiftToMost(res->lsu, res->digits, shift);
	  res->exponent-=shift;
	  }
	if (!swapped) res->bits^=negate;
	decFinish(res, set, &residue, status);
	break;}

      rhsshift=D2U(padding+1)-1;
      mult=powers[padding-(rhsshift*DECDPUN)];
      }

    if (diffsign) mult=-mult;

    maxdigits=rhs->digits+padding;
    if (lhs->digits>maxdigits) maxdigits=lhs->digits;

    acc=res->lsu;
    if ((maxdigits>=reqdigits) || (res==rhs && rhsshift>0)) {
      Int need=D2U(maxdigits)+1;
      acc=accbuff;
      if (need*sizeof(Unit)>sizeof(accbuff)) {
	allocacc=(Unit *)malloc(need*sizeof(Unit));
	if (allocacc==NULL) {
	  *status|=DEC_Insufficient_storage;
	  break;}
	acc=allocacc;
	}
      }

    res->bits=(uByte)(bits&DECNEG);
    res->exponent=lhs->exponent;

    #if DECTRACE
      decDumpAr('A', lhs->lsu, D2U(lhs->digits));
      decDumpAr('B', rhs->lsu, D2U(rhs->digits));
      printf("	:h: %ld %ld\n", rhsshift, mult);
    #endif

    res->digits=decUnitAddSub(lhs->lsu, D2U(lhs->digits),
			      rhs->lsu, D2U(rhs->digits),
			      rhsshift, acc, mult)
	       *DECDPUN;
    if (res->digits<0) {
      res->digits=-res->digits;
      res->bits^=DECNEG;
      }
    #if DECTRACE
      decDumpAr('+', acc, D2U(res->digits));
    #endif

    residue=0;
    if (acc!=res->lsu) {
        Int current_digits = res->digits;
        #if DECSUBSET
        if (set->extended) {
            if (current_digits > reqdigits) {
                current_digits = decGetDigits(acc, D2U(current_digits));
            }
            decSetCoeff(res, set, acc, current_digits, &residue, status);
        } else {
            if (current_digits < maxdigits) {
                if (D2U(maxdigits) > D2U(current_digits) &&
                    D2U(current_digits) < SD2U(DECBUFFER * 2 + 20)) {
                    *(acc + D2U(current_digits)) = 0;
                }
                current_digits = maxdigits;
            } else {
                if (current_digits > reqdigits) {
                    current_digits = decGetDigits(acc, D2U(current_digits));
                    if (current_digits < maxdigits) current_digits = maxdigits;
                }
            }
            decSetCoeff(res, set, acc, current_digits, &residue, status);
            if (residue != 0) {
                decApplyRound(res, set, residue, status);
                residue = 0;
            }
        }
        #else
        if (current_digits > reqdigits) {
            current_digits = decGetDigits(acc, D2U(current_digits));
        }
        decSetCoeff(res, set, acc, current_digits, &residue, status);
        #endif
    }

    res->digits=decGetDigits(res->lsu, D2U(res->digits));

    decFinish(res, set, &residue, status);

    if (ISZERO(res) && diffsign
     #if DECSUBSET
     && set->extended
     #endif
     && (*status&DEC_Inexact)==0) {
      if (set->round==DEC_ROUND_FLOOR) res->bits|=DECNEG;
				  else res->bits&=~DECNEG;
      }
    } while(0);

  free(allocacc);
  #if DECSUBSET
  free(allocrhs);
  free(alloclhs);
  #endif
  return res;
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

  Unit varbuff[SD2U(DECBUFFER*2+DECDPUN)];
  Unit *var1 = varbuff;
  Unit *varalloc = NULL;

  Unit *accnext;
  Unit *msu1;
  const Unit *var2;
  const Unit *msu2;

  Int acclength;
  Int accunits;
  Int accdigits;
  Int var1units, var2units;
  Int var2ulen;
  Int var1initpad = 0;
  Int maxdigits;

  Int exponent;
  Int maxexponent = 0;
  uByte bits;

  Int mult;
  Unit thisunit;
  Int residue = 0;
  Int reqdigits = set->digits;

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

  do {
    #if DECSUBSET
    if (!set->extended) {
      if (lhs->digits > reqdigits) {
	alloclhs = decRoundOperand(lhs, set, status);
	if (alloclhs == NULL) break;
	lhs = alloclhs;
      }
      if (rhs->digits > reqdigits) {
	allocrhs = decRoundOperand(rhs, set, status);
	if (allocrhs == NULL) break;
	rhs = allocrhs;
      }
    }
    #endif

    bits = (lhs->bits ^ rhs->bits) & DECNEG;

    if (SPECIALARGS) {
      if (SPECIALARGS & (DECSNAN | DECNAN)) {
	decNaNs(res, lhs, rhs, set, status);
	break;
      }
      if (decNumberIsInfinite(lhs)) {
	if (decNumberIsInfinite(rhs) || (op & (REMAINDER | REMNEAR))) {
	  *status |= DEC_Invalid_operation;
	  break;
	}
	decNumberZero(res);
	res->bits = bits | DECINF;
	break;
      } else {
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
	break;
      }
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
      break;
    }

    if (ISZERO(lhs)) {
      #if DECSUBSET
      if (!set->extended) {
        decNumberZero(res);
      } else {
      #endif
        if (op & DIVIDE) {
          exponent = lhs->exponent - rhs->exponent;
          decNumberCopy(res, lhs);
          res->bits = bits;
          res->exponent = exponent;
          residue = 0;
          decFinalize(res, set, &residue, status);
        } else if (op & DIVIDEINT) {
          decNumberZero(res);
          res->bits = bits;
        } else {
          exponent = rhs->exponent;
          decNumberCopy(res, lhs);
          if (exponent < res->exponent) {
            res->exponent = exponent;
          }
        }
      #if DECSUBSET
      }
      #endif
      break;
    }

    exponent = (lhs->exponent + lhs->digits) - (rhs->exponent + rhs->digits);

    if (exponent < 0 && !(op == DIVIDE)) {
      if (op & DIVIDEINT) {
	decNumberZero(res);
	#if DECSUBSET
	if (set->extended)
	#endif
	  res->bits = bits;
	break;
      }
      if (lhs->exponent <= rhs->exponent) {
	if (op & REMAINDER || exponent < -1) {
	  residue = 0;
	  decCopyFit(res, lhs, set, &residue, status);
	  decFinish(res, set, &residue, status);
	  break;
	}
      }
    }

    acclength = D2U(reqdigits + DECDPUN);
    if (acclength * sizeof(Unit) > sizeof(accbuff)) {
      allocacc = (Unit *)malloc(acclength * sizeof(Unit));
      if (allocacc == NULL) {
	*status |= DEC_Insufficient_storage;
	break;
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
	break;
      }
      var1 = varalloc;
    }

    msu1 = var1 + var1units - 1;
    source = lhs->lsu + D2U(lhs->digits) - 1;
    for (target = msu1; source >= lhs->lsu; source--, target--) *target = *source;
    for (; target >= var1; target--) *target = 0;

    var2ulen = var1units;
    var2units = D2U(rhs->digits);
    var2 = rhs->lsu;
    msu2 = var2 + var2units - 1;

    Int msu2plus = *msu2;
    if (var2units > 1) msu2plus++;
    eInt msu2pair = (eInt)*msu2 * (DECDPUNMAX + 1);
    if (var2units > 1) {
      msu2pair += *(msu2 - 1);
      if (var2units > 2) msu2pair++;
    }

    for (pow = &powers[1]; *msu1 >= *pow; pow++) exponent--;
    for (pow = &powers[1]; *msu2 >= *pow; pow++) exponent++;

    if (!(op & DIVIDE)) {
      Unit *u;
      var1initpad = (var1units - D2U(lhs->digits)) * DECDPUN;
      if (exponent < 0) cut = -exponent;
      else cut = DECDPUN - exponent % DECDPUN;
      decShiftToLeast(var1, var1units, cut);
      exponent += cut;
      var1initpad -= cut;
      for (u = msu1; cut >= DECDPUN; cut -= DECDPUN, u--) *u = 0;
    } else {
      maxexponent = lhs->exponent - rhs->exponent;
      if (*msu1 < *msu2) {
	var2ulen--;
	exponent -= DECDPUN;
      }
    }

    accunits = 0;
    accdigits = 0;
    accnext = acc + acclength - 1;

    for (;;) {
      thisunit = 0;
      for (;;) {
	for (; *msu1 == 0 && msu1 > var1; msu1--) var1units--;

	if (var1units < var2ulen) break;

	if (var1units == var2ulen) {
	  const Unit *pv1, *pv2;
	  Unit v2_val;
	  pv2 = msu2;
	  for (pv1 = msu1; ; pv1--, pv2--) {
	    v2_val = 0;
	    if (pv2 >= var2) v2_val = *pv2;
	    if (*pv1 != v2_val) break;
	    if (pv1 == var1) break;
	  }
	  if (*pv1 < v2_val) break;
	  if (*pv1 == v2_val) {
	    thisunit++;
	    *var1 = 0;
	    var1units = 1;
	    break;
	  }
	  mult = (Int)(((eInt)*msu1 * (DECDPUNMAX + 1) + *(msu1 - 1)) / msu2pair);
	} else {
	  mult = (Int)(((eInt)*msu1 * (DECDPUNMAX + 1) + *(msu1 - 1)) / msu2plus);
	}

	if (mult == 0) mult = 1;
	thisunit = (Unit)(thisunit + mult);

	shift = var2ulen - var2units;
	#if DECTRACE
	  decDumpAr('1', &var1[shift], var1units - shift);
	  decDumpAr('2', var2, var2units);
	  printf("m=%ld\n", -mult);
	#endif
	decUnitAddSub(&var1[shift], var1units - shift,
		      var2, var2units, 0,
		      &var1[shift], -mult);
	#if DECTRACE
	  decDumpAr('#', &var1[shift], var1units - shift);
	#endif
      }

      if (accunits != 0 || thisunit != 0) {
	*accnext = thisunit;
	if (accunits == 0) {
	  accdigits++;
	  for (pow = &powers[1]; thisunit >= *pow; pow++) accdigits++;
	} else accdigits += DECDPUN;
	accunits++;
	accnext--;
	if (accdigits > reqdigits) break;
      }

      if (*var1 == 0 && var1units == 1) {
	if (op & (REMAINDER | REMNEAR)) break;
	if ((op & DIVIDE) && (exponent <= maxexponent)) break;
      }
      if (exponent == 0 && !(op & DIVIDE)) break;

      var2ulen--;
      exponent -= DECDPUN;
    }

    if (accunits == 0) {
      accunits = 1;
      accdigits = 1;
      *accnext = 0;
    } else accnext++;

    residue = 0;

    if (op & DIVIDE) {
      if (*var1 != 0 || var1units > 1) {
        residue = 1;
      } else {
	#if DECDPUN > 1
	Unit lsu = *accnext;
	if (!((lsu & 0x01) || (lsu == 0))) {
	  Int drop = 0;
	  for (;; drop++) {
	    if (exponent >= maxexponent) break;
	    #if DECDPUN <= 4
	      if ((lsu - QUOT10(lsu, drop + 1) * powers[drop + 1]) != 0) break;
	    #else
	      if (lsu % powers[drop + 1] != 0) break;
	    #endif
	    exponent++;
	  }
	  if (drop > 0) {
	    accunits = decShiftToLeast(accnext, accunits, drop);
	    accdigits = decGetDigits(accnext, accunits);
	    accunits = D2U(accdigits);
	  }
	}
	#endif
      }
    } else {
      if (accdigits + exponent > reqdigits) {
	*status |= DEC_Division_impossible;
	break;
      }
      if (op & (REMAINDER | REMNEAR)) {
	bits = lhs->bits;

	if (*var1 == 0 && var1units == 1) {
	  Int exp_val = lhs->exponent;
	  if (rhs->exponent < exp_val) exp_val = rhs->exponent;
	  decNumberZero(res);
	  #if DECSUBSET
	  if (set->extended)
	  #endif
	  res->exponent = exp_val;
	  res->bits = (uByte)(bits & DECNEG);
	  decFinish(res, set, &residue, status);
	  break;
	}

	Unit *quotlsu_saved = accnext;
	Int  quotdigits_saved = accdigits;

	accnext = var1;
	accdigits = decGetDigits(var1, var1units);
	accunits = D2U(accdigits);

	Int postshift_val = var1initpad + exponent - lhs->exponent + rhs->exponent;
	if (var1initpad < postshift_val) postshift_val = var1initpad;

	var1units = decShiftToLeast(var1, var1units, postshift_val);
	accnext = var1;
	accdigits = decGetDigits(var1, var1units);
	accunits = D2U(accdigits);

	exponent = lhs->exponent;
	if (rhs->exponent < exponent) exponent = rhs->exponent;

	if (op & REMNEAR) {
	  Flag wasodd = (*quotlsu_saved & 0x01);

	  Int current_residue_units = accunits;

	  Int doubled_residue_units = decUnitAddSub(accnext, current_residue_units,
						  accnext, current_residue_units,
						  0, accnext, 1);
	  if (doubled_residue_units == BADINT) {
            *status |= DEC_Insufficient_storage;
            break;
	  }

	  Int compare_result = decUnitCompare(accnext, doubled_residue_units,
					      rhs->lsu, D2U(rhs->digits),
					      rhs->exponent - exponent);
	  if (compare_result == BADINT) {
	    *status |= DEC_Insufficient_storage;
	    break;
	  }

	  for (Unit *up = accnext; up < accnext + doubled_residue_units; up++) {
	    Int half = *up & 0x01;
	    *up /= 2;
	    if (half && up > accnext) {
                *(up - 1) += (DECDPUNMAX + 1) / 2;
            }
	  }
          while (doubled_residue_units > 1 && *(accnext + doubled_residue_units - 1) == 0) {
              doubled_residue_units--;
          }
          accdigits = decGetDigits(accnext, doubled_residue_units);
          accunits = D2U(accdigits);


	  if (compare_result > 0 || (compare_result == 0 && wasodd)) {
	    Flag allnines = 0;
	    if (quotdigits_saved == reqdigits) {
	      for (Unit *up = quotlsu_saved; ; up++) {
		if (quotdigits_saved > DECDPUN) {
		  if (*up != DECDPUNMAX) break;
		} else {
		  if (*up == powers[quotdigits_saved] - 1) allnines = 1;
		  break;
		}
		quotdigits_saved -= DECDPUN;
	      }
	    }
	    if (allnines) {
	      *status |= DEC_Division_impossible;
	      break;
	    }

	    Int rhs_exp_diff = rhs->exponent - exponent;
	    Int expunits = rhs_exp_diff / DECDPUN;
	    Int exprem = rhs_exp_diff % DECDPUN;

	    accunits = -decUnitAddSub(accnext, accunits,
				      rhs->lsu, D2U(rhs->digits),
				      expunits, accnext, -(Int)powers[exprem]);
            if (accunits == BADINT) {
                *status |= DEC_Insufficient_storage;
                break;
            }
	    accdigits = decGetDigits(accnext, accunits);
	    accunits = D2U(accdigits);
	    bits ^= DECNEG;
	  }
	}
      }
    }

    res->exponent = exponent;
    res->bits = (uByte)(bits & DECNEG);

    decSetCoeff(res, set, accnext, accdigits, &residue, status);
    decFinish(res, set, &residue, status);

    #if DECSUBSET
    if (!set->extended && (op == DIVIDE)) decTrim(res, set, 0, 1, &dropped);
    #endif

  } while(0);

  free(varalloc);
  free(allocacc);
  #if DECSUBSET
  free(allocrhs);
  free(alloclhs);
  #endif
  return res;
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
    Int         accunits;                      /* Units of accumulator in use */
    Int         exponent;                      /* work */
    Int         residue = 0;                   /* rounding residue */
    uByte       result_sign;                   /* result sign */
    Unit       *acc_units_ptr;                 /* -> accumulator Unit array */
    Int         required_bytes;                /* size calculator */
    void       *allocated_acc = NULL;          /* -> allocated accumulator, iff allocated */
    Unit        acc_buffer[SD2U(DECBUFFER * 4 + 1)]; /* buffer for accumulator */

    #if DECSUBSET
    decNumber *allocated_lhs = NULL;           /* -> allocated buffer for rounded lhs, iff allocated */
    decNumber *allocated_rhs = NULL;           /* -> allocated buffer for rounded rhs, iff allocated */
    #endif

    #if DECCHECK
    if (decCheckOperands(res, lhs, rhs, set)) return res;
    #endif

    /* Precalculate result sign */
    result_sign = (uByte)((lhs->bits ^ rhs->bits) & DECNEG);

    /* Handle infinities and NaNs */
    if (SPECIALARGS) { /* A special bit set (macro check) */
        if (SPECIALARGS & (DECSNAN | DECNAN)) { /* One or two NaNs */
            decNaNs(res, lhs, rhs, set, status);
            return res;
        }
        /* One or two infinities; Infinity * 0 is invalid */
        if (((lhs->bits & DECINF) == 0 && ISZERO(lhs)) ||
            ((rhs->bits & DECINF) == 0 && ISZERO(rhs))) {
            *status |= DEC_Invalid_operation;
            return res;
        }
        decNumberZero(res);
        res->bits = result_sign | DECINF; /* infinity */
        return res;
    }

    /* For best speed, use the shorter number as the multiplier (rhs) and */
    /* the longer as the multiplicand (lhs) to minimise adds (partial products) */
    if (lhs->digits < rhs->digits) { /* Swap operands */
        const decNumber *hold = lhs;
        lhs = rhs;
        rhs = hold;
    }

    do { /* Use do-while(0) for centralized resource cleanup on exit or error */
        #if DECSUBSET
        if (!set->extended) {
            /* Reduce operands and set lostDigits status, as needed */
            if (lhs->digits > set->digits) {
                allocated_lhs = decRoundOperand(lhs, set, status);
                if (allocated_lhs == NULL) { *status |= DEC_Insufficient_storage; break; }
                lhs = allocated_lhs;
            }
            if (rhs->digits > set->digits) {
                allocated_rhs = decRoundOperand(rhs, set, status);
                if (allocated_rhs == NULL) { *status |= DEC_Insufficient_storage; break; }
                rhs = allocated_rhs;
            }
        }
        #endif

        #if FASTMUL
        /* Fast multiplication path constants */
        #if DECDPUN & 1 /* DECDPUN is odd (1 or 3), work in base 10^9 */
            #define FAST_MULTIPLY_BASE      1000000000U /* base */
            #define FAST_MULTIPLY_DIGITS    9           /* digits in base */
            #define FAST_MULTIPLY_LAZY_COUNT 18         /* carry resolution point */
        #else /* DECDPUN is even (2 or 4), work in base 10^8 */
            #define FAST_MULTIPLY_BASE      100000000U
            #define FAST_MULTIPLY_DIGITS    8
            #define FAST_MULTIPLY_LAZY_COUNT 1844       /* carry resolution point */
        #endif
        /* Minimum digits in shorter operand to justify fast path overhead */
        #define MIN_DIGITS_FOR_FAST_MUL (DECDPUN * 2)

        if (rhs->digits > MIN_DIGITS_FOR_FAST_MUL) { /* Use fast path */
            /* Buffers for chunked operands (base 10^8 or 10^9) */
            uInt   lhs_chunk_buffer[(DECBUFFER * 2 + 1) / 8 + 1];
            uInt  *lhs_chunks = lhs_chunk_buffer;
            uInt  *allocated_lhs_chunks = NULL;

            uInt   rhs_chunk_buffer[(DECBUFFER * 2 + 1) / 8 + 1];
            uInt  *rhs_chunks = rhs_chunk_buffer;
            uInt  *allocated_rhs_chunks = NULL;

            /* Base 2^64 accumulator for exact result, with lazy carry evaluation */
            uLong  acc_chunk_buffer[(DECBUFFER * 2 + 1) / 4 + 2];
            uLong *accumulator_chunks = acc_chunk_buffer;

            Int    lhs_item_count;             /* Number of items in LHS chunk array */
            Int    rhs_item_count;             /* Number of items in RHS chunk array */
            Int    acc_item_count;             /* Number of items in accumulator chunk array */
            Int    temp_count;                 /* General purpose counter */
            Int    power_idx;                  /* Index for powers array */

            #if DECDPUN == 1
            Int    acc_offset_for_units;       /* Accumulator offset for DECDPUN=1 conversion */
            #endif

            /* Calculate the number of elements in each array */
            lhs_item_count = (lhs->digits + FAST_MULTIPLY_DIGITS - 1) / FAST_MULTIPLY_DIGITS; /* Ceiling division */
            rhs_item_count = (rhs->digits + FAST_MULTIPLY_DIGITS - 1) / FAST_MULTIPLY_DIGITS; /* Ceiling division */
            acc_item_count = lhs_item_count + rhs_item_count;

            /* Allocate buffers for chunked operands if local buffers are too small */
            required_bytes = lhs_item_count * sizeof(uInt);
            if (required_bytes > (Int)sizeof(lhs_chunk_buffer)) {
                allocated_lhs_chunks = (uInt *)malloc(required_bytes);
                lhs_chunks = allocated_lhs_chunks;
            }
            required_bytes = rhs_item_count * sizeof(uInt);
            if (required_bytes > (Int)sizeof(rhs_chunk_buffer)) {
                allocated_rhs_chunks = (uInt *)malloc(required_bytes);
                rhs_chunks = allocated_rhs_chunks;
            }

            /* Allocate accumulator space. Special case for DECDPUN=1: */
            /* each 8-byte uLong item becomes 9 1-byte units.  Therefore, additional */
            /* bytes are needed at the front (rounded up to a multiple of 8 bytes), */
            /* and the uLong accumulator starts offset to avoid overwrite during unchunking. */
            required_bytes = acc_item_count * sizeof(uLong);
            #if DECDPUN == 1
            acc_offset_for_units = (acc_item_count + 7) / 8; /* Number of uLong items to offset by */
            required_bytes += acc_offset_for_units * sizeof(uLong); /* Add space for units */
            #endif

            if (required_bytes > (Int)sizeof(acc_chunk_buffer)) {
                allocated_acc = (uLong *)malloc(required_bytes);
                accumulator_chunks = (uLong *)allocated_acc;
            }

            /* Check for allocation failures */
            if (lhs_chunks == NULL || rhs_chunks == NULL || accumulator_chunks == NULL) {
                *status |= DEC_Insufficient_storage;
                break; /* Jump to cleanup */
            }

            acc_units_ptr = (Unit *)accumulator_chunks; /* Pointer to the target Unit array */
            #if DECDPUN == 1
            accumulator_chunks += acc_offset_for_units; /* Adjust uLong accumulator start for unit conversion */
            #endif

            /* Assemble chunked copies of lhs */
            const Unit *current_unit_src_ptr = lhs->lsu;
            uInt *current_lhs_chunk_ptr = lhs_chunks;
            for (temp_count = lhs->digits; temp_count > 0; current_lhs_chunk_ptr++) {
                *current_lhs_chunk_ptr = 0;
                for (power_idx = 0; power_idx < FAST_MULTIPLY_DIGITS && temp_count > 0;
                     power_idx += DECDPUN, current_unit_src_ptr++, temp_count -= DECDPUN) {
                    *current_lhs_chunk_ptr += (uLong)(*current_unit_src_ptr) * powers[power_idx];
                }
            }
            uInt *lhs_msi = current_lhs_chunk_ptr - 1; /* Pointer to most significant chunk item */

            /* Assemble chunked copies of rhs */
            current_unit_src_ptr = rhs->lsu;
            uInt *current_rhs_chunk_ptr = rhs_chunks;
            for (temp_count = rhs->digits; temp_count > 0; current_rhs_chunk_ptr++) {
                *current_rhs_chunk_ptr = 0;
                for (power_idx = 0; power_idx < FAST_MULTIPLY_DIGITS && temp_count > 0;
                     power_idx += DECDPUN, current_unit_src_ptr++, temp_count -= DECDPUN) {
                    *current_rhs_chunk_ptr += (uLong)(*current_unit_src_ptr) * powers[power_idx];
                }
            }
            uInt *rhs_msi = current_rhs_chunk_ptr - 1; /* Pointer to most significant chunk item */

            /* Zero the accumulator */
            for (uLong *lp = accumulator_chunks; lp < accumulator_chunks + acc_item_count; lp++) {
                *lp = 0;
            }

            /* Main multiplication loop with lazy carry resolution */
            Int lazy_carry_counter = FAST_MULTIPLY_LAZY_COUNT;
            for (uInt *rip = rhs_chunks; rip <= rhs_msi; rip++) { /* Iterate over each chunk in rhs */
                uLong *acc_target_ptr = accumulator_chunks + (rip - rhs_chunks); /* Accumulator start for this partial product */
                for (uInt *lip = lhs_chunks; lip <= lhs_msi; lip++, acc_target_ptr++) { /* Iterate over each chunk in lhs */
                    *acc_target_ptr += (uLong)(*lip) * (*rip); /* Accumulate partial product */
                }
                lazy_carry_counter--;
                if (lazy_carry_counter > 0 && rip != rhs_msi) continue; /* Delay carry resolution */

                lazy_carry_counter = FAST_MULTIPLY_LAZY_COUNT; /* Reset delay count */

                /* Resolve overflows in the accumulator */
                for (uLong *lp = accumulator_chunks; lp < accumulator_chunks + acc_item_count; lp++) {
                    uLong current_val = *lp;
                    if (current_val < FAST_MULTIPLY_BASE) continue; /* No carry needed for this chunk */

                    uLong carry_to_next = current_val / FAST_MULTIPLY_BASE;
                    *lp = current_val % FAST_MULTIPLY_BASE; /* Update current chunk value to remainder */

                    if (carry_to_next < FAST_MULTIPLY_BASE) {
                        /* Standard one-chunk carry to the next position */
                        *(lp + 1) += (uInt)carry_to_next;
                    } else {
                        /* Two-place carry: carry_to_next itself generates a carry */
                        uInt carry_to_next_next = (uInt)(carry_to_next / FAST_MULTIPLY_BASE);
                        uInt carry_to_next_immediate = (uInt)(carry_to_next % FAST_MULTIPLY_BASE);
                        *(lp + 2) += carry_to_next_next;       /* Carry to lp+2 */
                        *(lp + 1) += carry_to_next_immediate;  /* Carry to lp+1 */
                    }
                } /* End of carry resolution loop */
            } /* End of rhs chunk loop */

            /* Multiplication is complete; convert uLong chunks back into Unit array */
            Unit *current_unit_dest_ptr = acc_units_ptr;
            uLong base_for_unchunking = DECDPUNMAX + 1; // This is 10^DECDPUN
            for (uLong *lp = accumulator_chunks; lp < accumulator_chunks + acc_item_count; lp++) {
                uInt item_val = (uInt)*lp; /* Take chunk value (up to 10^FAST_MULTIPLY_DIGITS) */
                for (power_idx = 0; power_idx < FAST_MULTIPLY_DIGITS - DECDPUN; power_idx += DECDPUN, current_unit_dest_ptr++) {
                    *current_unit_dest_ptr = (Unit)(item_val % base_for_unchunking);
                    item_val /= base_for_unchunking;
                }
                *current_unit_dest_ptr = (Unit)item_val; /* Final unit needs no division */
                current_unit_dest_ptr++;
            }
            accunits = current_unit_dest_ptr - acc_units_ptr; /* Count of units in the result */
        } else { /* Use units directly, without chunking (traditional long multiplication) */
        #endif /* FASTMUL */

            /* Allocate accumulator if local buffer is too small */
            acc_units_ptr = acc_buffer; /* Assume local buffer first */
            required_bytes = (D2U(lhs->digits) + D2U(rhs->digits)) * sizeof(Unit);
            if (required_bytes > (Int)sizeof(acc_buffer)) {
                allocated_acc = (Unit *)malloc(required_bytes);
                if (allocated_acc == NULL) { *status |= DEC_Insufficient_storage; break; }
                acc_units_ptr = (Unit *)allocated_acc; /* Use allocated space */
            }

            /* Main long multiplication loop: ACC = ACC + MULTIPLICAND * MULTIPLIER_UNIT */
            accunits = 1;      /* Accumulator starts with '0' */
            *acc_units_ptr = 0;/* Initial LSUM (Least Significant Unit) is 0 */
            Int shift = 0;     /* Multiplicand shift for partial products */
            Int multiplicand_length_units = D2U(lhs->digits); /* Length of multiplicand in units */
            const Unit *multiplier_msu_plus_1 = rhs->lsu + D2U(rhs->digits); /* End marker for multiplier units */

            for (const Unit *multiplier_unit_ptr = rhs->lsu;
                 multiplier_unit_ptr < multiplier_msu_plus_1;
                 multiplier_unit_ptr++) {
                /* If multiplier unit is non-zero, add its scaled partial product */
                if (*multiplier_unit_ptr != 0) {
                    accunits = decUnitAddSub(&acc_units_ptr[shift], accunits - shift,
                                             lhs->lsu, multiplicand_length_units, 0,
                                             &acc_units_ptr[shift], *multiplier_unit_ptr)
                                             + shift;
                } else { /* If multiplier unit is zero, effectively extend accumulator with a 0 */
                    *(acc_units_ptr + accunits) = 0;
                    accunits++;
                }
                /* Shift for the next partial product (equivalent to multiplying by 10^DECDPUN) */
                shift++;
            } /* End of multiplier_unit_ptr loop */
        #if FASTMUL
        } /* End of fast path / slow path conditional */
        #endif

        /* Common end-path for both multiplication methods */
        #if DECTRACE
        decDumpAr('*', acc_units_ptr, accunits); /* Show exact result for tracing */
        #endif

        /* acc_units_ptr now holds the exact coefficient result. */
        /* Build the decNumber from it, noting any residue. */
        res->bits = result_sign;                     /* Set sign */
        res->digits = decGetDigits(acc_units_ptr, accunits); /* Count exact digits */

        /* Calculate exponent. Handle potential overflow if both input exponents */
        /* are negative and large enough to wrap a 31-bit signed integer. */
        /* If wrapped, force a hard underflow. */
        exponent = lhs->exponent + rhs->exponent;
        if (lhs->exponent < 0 && rhs->exponent < 0 && exponent > 0) {
            exponent = -2 * DECNUMMAXE; /* Force underflow for extreme negative exponents */
        }
        res->exponent = exponent;

        /* Set the coefficient into the decNumber structure and handle any rounding */
        decSetCoeff(res, set, acc_units_ptr, res->digits, &residue, status);
        decFinish(res, set, &residue, status); /* Final cleanup and status update */

    } while(0); /* End of do-while(0) protected block */

    /* Cleanup all potentially allocated memory */
    free(allocated_acc);
    #if DECSUBSET
    free(allocated_rhs);
    free(allocated_lhs);
    #endif
    #if FASTMUL
    free(allocated_rhs_chunks);
    free(allocated_lhs_chunks);
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
decNumber * decExpOp(decNumber *res, const decNumber *rhs,
			 decContext *set, uInt *status) {
  uInt ignore=0;
  Int h;
  Int p;
  Int residue;
  size_t needbytes;
  const decNumber *x=rhs;
  decContext aset, tset, dset;
  Int comp;

  decNumber bufr[D2N(DECBUFFER*2+1)];
  decNumber *allocrhs=NULL;

  decNumber buft[D2N(DECBUFFER*2+9+1)];
  decNumber *allocbuft=NULL;
  decNumber *t=buft;

  decNumber bufa[D2N(DECBUFFER*4+18+1)];
  decNumber *allocbufa=NULL;
  decNumber *a=bufa;

  decNumber bufd[D2N(16)];
  decNumber *d=bufd;
  decNumber numone;

  #if DECCHECK
  if (decCheckOperands(res, DECUNUSED, rhs, set)) return res;
  #endif

  do {
    if (SPECIALARG) {
      if (decNumberIsInfinite(rhs)) {
	if (decNumberIsNegative(rhs))
	  decNumberZero(res);
	 else decNumberCopy(res, rhs);
	}
       else decNaNs(res, rhs, NULL, set, status);
      break;}

    if (ISZERO(rhs)) {
      decNumberZero(res);
      *res->lsu=1;
      break;}

    decNumberZero(d);
    *d->lsu=4;
    d->exponent=-set->digits;
    if (decNumberIsNegative(rhs)) d->exponent--;
    comp=decCompare(d, rhs, 1);
    if (comp==BADINT) {
      *status|=DEC_Insufficient_storage;
      break;}
    if (comp>=0) {
      Int shift=set->digits-1;
      decNumberZero(res);
      *res->lsu=1;
      res->digits=decShiftToMost(res->lsu, 1, shift);
      res->exponent=-shift;
      *status|=DEC_Inexact | DEC_Rounded;
      break;}

    decContextDefault(&aset, DEC_INIT_DECIMAL64);
    aset.emax=set->emax;
    aset.emin=set->emin;
    aset.clamp=0;

    h=rhs->exponent+rhs->digits;
    if (h > 8) {
      decNumberZero(a);
      *a->lsu=2;
      if (decNumberIsNegative(rhs)) a->exponent=-2;
      h=8;
      p=9;
      }
     else {
      Int maxlever=(rhs->digits>8?1:0);

      Int lever=MINI(8-h, maxlever);
      Int use=-rhs->digits-lever;
      h+=lever;
      if (h<0) {
	use+=h;
	h=0;
	}
      if (rhs->exponent!=use) {
	decNumber *newrhs=bufr;
	needbytes=sizeof(decNumber)+(D2U(rhs->digits)-1)*sizeof(Unit);
	if (needbytes>sizeof(bufr)) {
	  allocrhs=(decNumber *)malloc(needbytes);
	  if (allocrhs==NULL) {
	    *status|=DEC_Insufficient_storage;
	    break;}
	  newrhs=allocrhs;
	  }
	decNumberCopy(newrhs, rhs);
	newrhs->exponent=use;
	x=newrhs;
	}

      p=MAXI(x->digits, set->digits)+h+2;

      needbytes=sizeof(decNumber)+(D2U(p*2)-1)*sizeof(Unit);
      if (needbytes>sizeof(bufa)) {
	allocbufa=(decNumber *)malloc(needbytes);
	if (allocbufa==NULL) {
	  *status|=DEC_Insufficient_storage;
	  break;}
	a=allocbufa;
	}
      needbytes=sizeof(decNumber)+(D2U(p+2)-1)*sizeof(Unit);
      if (needbytes>sizeof(buft)) {
	allocbuft=(decNumber *)malloc(needbytes);
	if (allocbuft==NULL) {
	  *status|=DEC_Insufficient_storage;
	  break;}
	t=allocbuft;
	}

      decNumberCopy(t, x);
      decNumberZero(a); *a->lsu=1;
      decNumberZero(d); *d->lsu=2;
      decNumberZero(&numone); *numone.lsu=1;

      decContextDefault(&tset, DEC_INIT_DECIMAL64);
      dset=tset;
      aset.digits=p*2;
      tset.digits=p;
      tset.emin=DEC_MIN_EMIN;

      for (;;) {
	decAddOp(a, a, t, &aset, 0, status);
	decMultiplyOp(t, t, x, &tset, &ignore);
	decDivideOp(t, t, d, &tset, DIVIDE, &ignore);
	if (((a->digits+a->exponent)>=(t->digits+t->exponent+p+1))
	    && (a->digits>=p)) break;
	decAddOp(d, d, &numone, &dset, 0, &ignore);
	}
      }

    if (h>0) {
      Int seenbit=0;
      Int n=powers[h];
      aset.digits=p+2;
      decNumberZero(t); *t->lsu=1;
      for (Int i=1;;i++){
	if (*status & (DEC_Overflow|DEC_Underflow)) {
	  if (*status&DEC_Overflow || ISZERO(t)) break;}
	n=n<<1;
	if (n<0) {
	  seenbit=1;
	  decMultiplyOp(t, t, a, &aset, status);
	  }
	if (i==31) break;
	if (!seenbit) continue;
	decMultiplyOp(t, t, t, &aset, status);
	}
      a=t;
      }

    residue=1;
    if (ISZERO(a)) residue=0;
    aset.digits=set->digits;
    decCopyFit(res, a, &aset, &residue, status);
    decFinish(res, set, &residue, status);
    } while(0);

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
#include "decNumber.h"

static const Int LN_MIN_WORK_PRECISION = 7;
static const Int LN_GUARD_DIGITS = 2;
static const Int LN_FASTPATH_MAX_DIGITS = 40;
static const Int LN_ITERATION_INITIAL_PRECISION = 9;
static const Int LN_ITERATION_MAX_STEPS = 24;

static const Int LN10_EST_COEFF = 2302585;
static const Int LN10_EST_EXPONENT = -6;
static const Int LN_EST_TWO_DIGITS = 2;
static const Int LN_EST_TEN = 10;
static const Int LN_EST_PACKED_EXPONENT_MASK = 3;
static const Int LN_EST_EXP_BASE_OFFSET = 3;

static const char* const LN10_STR = "2.302585092994045684017991454684364207601";
static const char* const LN2_STR = "0.6931471805599453094172321214581765680755";


decNumber * decLnOp(decNumber *res, const decNumber *rhs,
		    decContext *set, uInt *status) {
  uInt ignore_status = 0;

  decNumber stack_buf_a[D2N(DECBUFFER + 12)];
  decNumber *a = stack_buf_a;
  decNumber *alloc_buf_a = NULL;

  decNumber stack_buf_b[D2N(DECBUFFER * 2 + 2)];
  decNumber *b = stack_buf_b;
  decNumber *alloc_buf_b = NULL;

  decNumber num_one;
  decNumber compare_result;
  decContext ctx_work_a;
  decContext ctx_work_b;

  Int current_precision;
  Int iteration_precision;
  Int rhs_normal_exponent;
  Int ln_lookup_val;
  Int rounding_residue;

  #if DECCHECK
  Int iteration_count = 0;
  if (decCheckOperands(res, DECUNUSED, rhs, set)) {
    return res;
  }
  #endif

  do {
    if (decNumberIsInfinite(rhs) || decNumberIsNaN(rhs)) {
      if (decNumberIsNegative(rhs)) {
        *status |= DEC_Invalid_operation;
      } else if (decNumberIsInfinite(rhs)) {
        decNumberCopy(res, rhs);
      } else {
        decNaNs(res, rhs, NULL, set, status);
      }
      break;
    }

    if (decNumberIsZero(rhs)) {
      decNumberZero(res);
      res->bits = DECINF | DECNEG;
      break;
    }

    if (decNumberIsNegative(rhs)) {
      *status |= DEC_Invalid_operation;
      break;
    }

    if (rhs->exponent == 0 && set->digits <= LN_FASTPATH_MAX_DIGITS) {
      #if DECDPUN==1
      if (rhs->lsu[0] == 0 && rhs->lsu[1] == 1 && rhs->digits == LN_GUARD_DIGITS) {
      #else
      if (rhs->lsu[0] == LN_EST_TEN && rhs->digits == LN_GUARD_DIGITS) {
      #endif
        ctx_work_a = *set;
        ctx_work_a.round = DEC_ROUND_HALF_EVEN;
        decNumberFromString(res, LN10_STR, &ctx_work_a);
        *status |= (DEC_Inexact | DEC_Rounded);
        break;
      }
      if (rhs->lsu[0] == LN_GUARD_DIGITS && rhs->digits == 1) {
        ctx_work_a = *set;
        ctx_work_a.round = DEC_ROUND_HALF_EVEN;
        decNumberFromString(res, LN2_STR, &ctx_work_a);
        *status |= (DEC_Inexact | DEC_Rounded);
        break;
      }
    }

    current_precision = MAXI(rhs->digits, MAXI(set->digits, LN_MIN_WORK_PRECISION)) + LN_GUARD_DIGITS;

    uInt needed_units_a = D2U(MAXI(current_precision, LN_ESTIMATE_PRECISION));
    if (needed_units_a * sizeof(Unit) + sizeof(decNumber) > sizeof(stack_buf_a)) {
      alloc_buf_a = (decNumber *)malloc(needed_units_a * sizeof(Unit) + sizeof(decNumber));
      if (alloc_buf_a == NULL) {
        *status |= DEC_Insufficient_storage;
        break;
      }
      a = alloc_buf_a;
    }

    uInt needed_units_b = D2U(MAXI(current_precision + rhs->digits, LN_ESTIMATE_PRECISION));
    if (needed_units_b * sizeof(Unit) + sizeof(decNumber) > sizeof(stack_buf_b)) {
      alloc_buf_b = (decNumber *)malloc(needed_units_b * sizeof(Unit) + sizeof(decNumber));
      if (alloc_buf_b == NULL) {
        *status |= DEC_Insufficient_storage;
        break;
      }
      b = alloc_buf_b;
    }

    decContextDefault(&ctx_work_a, DEC_INIT_DECIMAL64);
    rhs_normal_exponent = rhs->exponent + rhs->digits;
    decNumberFromInt32(a, rhs_normal_exponent);
    decNumberFromInt32(b, LN10_EST_COEFF);
    b->exponent = LN10_EST_EXPONENT;
    decMultiplyOp(a, a, b, &ctx_work_a, &ignore_status);

    rounding_residue = 0;
    ctx_work_a.digits = LN_EST_TWO_DIGITS;
    ctx_work_a.round = DEC_ROUND_DOWN;
    decCopyFit(b, rhs, &ctx_work_a, &rounding_residue, &ignore_status);
    b->exponent = 0;
    Int top_two_digits = decGetInt(b);
    if (top_two_digits < LN_EST_TEN) {
      top_two_digits = X10(top_two_digits);
    }
    ln_lookup_val = LNnn[top_two_digits - LN_EST_TEN];
    decNumberFromInt32(b, ln_lookup_val >> LN_GUARD_DIGITS);
    b->exponent = -(ln_lookup_val & LN_EST_PACKED_EXPONENT_MASK) - LN_EST_EXP_BASE_OFFSET;
    b->bits = DECNEG;
    ctx_work_a.digits = LN_ESTIMATE_PRECISION;
    ctx_work_a.round = DEC_ROUND_HALF_EVEN;
    decAddOp(a, a, b, &ctx_work_a, 0, &ignore_status);

    decNumberFromInt32(&num_one, 1);

    ctx_work_a.emax = set->emax;
    ctx_work_a.emin = set->emin;
    ctx_work_a.clamp = 0;

    ctx_work_b = ctx_work_a;
    ctx_work_b.emax = DEC_MAX_MATH * 2;
    ctx_work_b.emin = -DEC_MAX_MATH * 2;

    iteration_precision = LN_ITERATION_INITIAL_PRECISION;
    ctx_work_a.digits = iteration_precision;
    ctx_work_b.digits = iteration_precision + rhs->digits;

    for (Int step = 0; step < LN_ITERATION_MAX_STEPS; ++step) {
      #if DECCHECK
      iteration_count++;
      #endif
      a->bits ^= DECNEG;
      decExpOp(b, a, &ctx_work_b, &ignore_status);
      a->bits ^= DECNEG;

      decMultiplyOp(b, b, rhs, &ctx_work_b, &ignore_status);
      decAddOp(b, b, &num_one, &ctx_work_b, DECNEG, &ignore_status);

      if (decNumberIsZero(b) ||
          (a->digits + a->exponent) >= (b->digits + b->exponent + set->digits + LN_GUARD_DIGITS)) {
        if (a->digits == current_precision) {
          break;
        }
        if (decNumberIsZero(a)) {
          decCompareOp(&compare_result, rhs, &num_one, &ctx_work_a, DEC_CMP_OP, &ignore_status);
          if (decNumberIsZero(&compare_result)) {
            a->exponent = 0;
          } else {
            *status |= (DEC_Inexact | DEC_Rounded);
          }
          break;
        }
        if (decNumberIsZero(b)) {
          b->exponent = a->exponent - current_precision;
        }
      }

      decAddOp(a, a, b, &ctx_work_a, 0, &ignore_status);
      if (iteration_precision == current_precision) {
        continue;
      }
      iteration_precision *= LN_GUARD_DIGITS;
      if (iteration_precision > current_precision) {
        iteration_precision = current_precision;
      }
      ctx_work_a.digits = iteration_precision;
      ctx_work_b.digits = iteration_precision + rhs->digits;
    }

    #if DECCHECK
    if (iteration_count >= LN_ITERATION_MAX_STEPS) {
      printf("Ln iterations=%ld, status=%08lx, p=%ld, d=%ld\n",
            (long)iteration_count, (long)*status, (long)current_precision, (long)rhs->digits);
    }
    #endif

    rounding_residue = 1;
    if (decNumberIsZero(a)) {
      rounding_residue = 0;
    }
    
    ctx_work_a.digits = set->digits;
    decCopyFit(res, a, &ctx_work_a, &rounding_residue, status);
    decFinish(res, set, &rounding_residue, status);
  } while(0);

  free(alloc_buf_a);
  free(alloc_buf_b);

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
    decNumber *alloclhs = NULL;
    decNumber *allocrhs = NULL;
    #endif
    const decNumber *inrhs = rhs;
    Int   reqdigits = set->digits;
    Int   reqexp;
    Int   residue = 0;
    Int   etiny = set->emin - (reqdigits - 1);

    #if DECCHECK
    if (decCheckOperands(res, lhs, rhs, set)) {
        return res;
    }
    #endif

    #if DECSUBSET
    if (!set->extended) {
        if (lhs->digits > reqdigits) {
            alloclhs = decRoundOperand(lhs, set, status);
            if (alloclhs == NULL) {
                *status |= DEC_Insufficient_storage;
                goto cleanup;
            }
            lhs = alloclhs;
        }
        if (rhs->digits > reqdigits) {
            allocrhs = decRoundOperand(rhs, set, status);
            if (allocrhs == NULL) {
                *status |= DEC_Insufficient_storage;
                goto cleanup;
            }
            rhs = allocrhs;
        }
    }
    #endif

    if (SPECIALARGS) {
        if (SPECIALARGS & (DECSNAN | DECNAN)) {
            decNaNs(res, lhs, rhs, set, status);
        } else if ((lhs->bits ^ rhs->bits) & DECINF) {
            *status |= DEC_Invalid_operation;
        } else {
            decNumberCopy(res, lhs);
        }
        goto cleanup;
    }

    if (quant) {
        reqexp = inrhs->exponent;
    } else {
        reqexp = decGetInt(inrhs);
        if (reqexp == BADINT || reqexp == BIGODD || reqexp == BIGEVEN) {
            *status |= DEC_Invalid_operation;
            goto cleanup;
        }
    }

    #if DECSUBSET
    if (!set->extended) {
        etiny = set->emin;
    }
    #endif

    if (reqexp < etiny || reqexp > set->emax) {
        *status |= DEC_Invalid_operation;
        goto cleanup;
    }

    if (ISZERO(lhs)) {
        decNumberCopy(res, lhs);
        res->exponent = reqexp;
        #if DECSUBSET
        if (!set->extended) {
            res->bits = 0;
        }
        #endif
    } else {
        Int adjust = reqexp - lhs->exponent;

        if ((lhs->digits - adjust) > reqdigits) {
            *status |= DEC_Invalid_operation;
            goto cleanup;
        }

        if (adjust > 0) {
            decContext workset = *set;
            workset.digits = lhs->digits - adjust;

            decCopyFit(res, lhs, &workset, &residue, status);
            decApplyRound(res, &workset, residue, status);
            residue = 0;

            if (res->exponent > reqexp) {
                if (res->digits == reqdigits) {
                    *status &= ~(DEC_Inexact | DEC_Rounded);
                    *status |= DEC_Invalid_operation;
                    goto cleanup;
                }
                res->digits = decShiftToMost(res->lsu, res->digits, 1);
                res->exponent--;
            }

            #if DECSUBSET
            if (ISZERO(res) && !set->extended) {
                res->bits = 0;
            }
            #endif
        } else {
            decNumberCopy(res, lhs);

            if (adjust < 0) {
                res->digits = decShiftToMost(res->lsu, res->digits, -adjust);
                res->exponent += adjust;
            }
        }
    }

    if (res->exponent > set->emax - res->digits + 1) {
        *status |= DEC_Invalid_operation;
        goto cleanup;
    } else {
        decFinalize(res, set, &residue, status);
        *status &= ~DEC_Underflow;
    }

cleanup:
    #if DECSUBSET
    free(allocrhs);
    free(alloclhs);
    #endif
    return res;
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
    decNumber *alloclhs = NULL;
    decNumber *allocrhs = NULL;
    #endif
    Int result = 0;
    uByte merged_nan_bits;
    Flag current_op = op; // Use a mutable copy of op, as it can change during execution

    #if DECCHECK
    if (decCheckOperands(res, lhs, rhs, set)) {
        return res;
    }
    #endif

    // --- Operand Reduction (DECSUBSET) ---
    #if DECSUBSET
    if (!set->extended) {
        if (lhs->digits > set->digits) {
            alloclhs = decRoundOperand(lhs, set, status);
            if (alloclhs == NULL) {
                *status |= DEC_Insufficient_storage;
                goto cleanup;
            }
            lhs = alloclhs;
        }
        if (rhs->digits > set->digits) {
            allocrhs = decRoundOperand(rhs, set, status);
            if (allocrhs == NULL) {
                *status |= DEC_Insufficient_storage;
                goto cleanup;
            }
            rhs = allocrhs;
        }
    }
    #endif

    // --- NaN and Infinity Handling ---
    merged_nan_bits = (lhs->bits | rhs->bits) & (DECSNAN | DECNAN);

    if (merged_nan_bits) { // One or both operands are NaN
        if (current_op == COMPTOTAL) { // Total ordering for NaNs
            if (decNumberIsNegative(lhs) && !decNumberIsNegative(rhs)) {
                result = -1;
            } else if (!decNumberIsNegative(lhs) && decNumberIsNegative(rhs)) {
                result = +1;
            } else { // Signs are the same or both are zero
                if (!decNumberIsNaN(lhs)) { // lhs is number, rhs is NaN
                    result = -1;
                } else if (!decNumberIsNaN(rhs)) { // rhs is number, lhs is NaN
                    result = +1;
                } else { // Both are NaNs
                    if (decNumberIsSNaN(lhs) && decNumberIsQNaN(rhs)) {
                        result = -1;
                    } else if (decNumberIsQNaN(lhs) && decNumberIsSNaN(rhs)) {
                        result = +1;
                    } else { // Both same type of NaN (QNaN or SNaN), compare payloads
                        result = decUnitCompare(lhs->lsu, D2U(lhs->digits),
                                                rhs->lsu, D2U(rhs->digits), 0);
                    }
                }
                if (decNumberIsNegative(lhs)) { // Invert result if negative
                    result = -result;
                }
            }
            goto process_result;
        } else if (current_op == COMPSIG) { // COMPSIG treats qNaN as sNaN for status
            *status |= DEC_Invalid_operation | DEC_sNaN;
            // Fall through to generic NaN handling (propagate NaN)
        } else if (current_op != COMPARE) { // For MIN, MAX, MINMAG, MAXMAG
            // If just one NaN, IEEE 754 rules ignore it; force choice to be the non-NaN operand.
            if (!decNumberIsNaN(lhs) || !decNumberIsNaN(rhs)) {
                current_op = COMPMAX; // Temporarily force op to COMPMAX to select the non-NaN
                result = (lhs->bits & DECNAN) ? -1 : +1; // -1 picks rhs (non-NaN), +1 picks lhs (non-NaN)
                goto process_result;
            }
            // If both are NaNs, fall through to generic NaN handling
        }
        // Generic NaN handling (propagate NaN for COMPARE, COMPSIG, or MIN/MAX with both NaNs)
        decNaNs(res, lhs, rhs, set, status);
        current_op = COMPNAN; // Mark operation as handled by NaN propagation
        goto process_result;
    }

    // --- Total Ordering for Numbers with Different Signs ---
    if (current_op == COMPTOTAL) {
        if (decNumberIsNegative(lhs) && !decNumberIsNegative(rhs)) {
            result = -1;
            goto process_result;
        }
        if (!decNumberIsNegative(lhs) && decNumberIsNegative(rhs)) {
            result = +1;
            goto process_result;
        }
    }

    // --- Core Numerical Comparison ---
    result = decCompare(lhs, rhs, (current_op == COMPMAXMAG || current_op == COMPMINMAG));

process_result:
    // --- Error Check for Insufficient Storage ---
    if (result == BADINT) {
        *status |= DEC_Insufficient_storage;
    } else {
        // --- Signum Result (COMPARE, COMPSIG, COMPTOTAL) ---
        if (current_op == COMPARE || current_op == COMPSIG || current_op == COMPTOTAL) {
            if (current_op == COMPTOTAL && result == 0) {
                // Total ordering for numerically equal numbers compares exponents for tie-breaking
                if (lhs->exponent != rhs->exponent) {
                    result = (lhs->exponent < rhs->exponent) ? -1 : +1;
                    if (decNumberIsNegative(lhs)) { // Invert if negative
                        result = -result;
                    }
                }
            }
            // Set result to -1, 0, or +1
            decNumberZero(res);
            if (result != 0) {
                *res->lsu = 1;
                if (result < 0) {
                    res->bits = DECNEG;
                }
            }
        }
        // --- NaN Propagated Result (COMPNAN) ---
        else if (current_op == COMPNAN) {
            // decNaNs has already written the result to 'res'
        }
        // --- MIN/MAX Result (non-NaN) ---
        else {
            const decNumber *choice;

            if (result == 0) { // Operands are numerically equal
                uByte slhs = (lhs->bits & DECNEG);
                uByte srhs = (rhs->bits & DECNEG);

                #if DECSUBSET
                if (!set->extended) { // Subset: force left-hand if numerically equal
                    result = +1;
                } else
                #endif
                if (slhs != srhs) { // Signs differ (e.g., +0 vs -0)
                    result = slhs ? -1 : +1; // Pick the positive one as max
                } else if (slhs) { // Both negative
                    result = (lhs->exponent < rhs->exponent) ? +1 : -1; // Smaller exponent means larger magnitude
                } else { // Both positive
                    result = (lhs->exponent > rhs->exponent) ? +1 : -1; // Larger exponent means larger magnitude
                }
            }

            // Reverse result if original operation was MIN or MINMAG
            if (current_op == COMPMIN || current_op == COMPMINMAG) {
                result = -result;
            }

            choice = (result > 0 ? lhs : rhs); // Select the chosen operand

            // Copy chosen to result, rounding if needed
            Int residue = 0;
            decCopyFit(res, choice, set, &residue, status);
            decFinish(res, set, &residue, status);
        }
    }

cleanup:
    // --- Cleanup (DECSUBSET) ---
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
static Int get_dec_signum(const decNumber *dn) {
    if (ISZERO(dn)) {
        return 0;
    }
    return decNumberIsNegative(dn) ? -1 : 1;
}

static Int decCompare(const decNumber *lhs, const decNumber *rhs, Flag abs) {
    Int signum_lhs_true = get_dec_signum(lhs);
    Int signum_rhs_true = get_dec_signum(rhs);

    Int effective_signum_lhs = signum_lhs_true;
    Int effective_signum_rhs = signum_rhs_true;

    if (abs) {
        if (effective_signum_lhs < 0) {
            effective_signum_lhs = 1;
        }
        if (effective_signum_rhs < 0) {
            effective_signum_rhs = 1;
        }
    }

    if (effective_signum_lhs != effective_signum_rhs) {
        return effective_signum_lhs > effective_signum_rhs ? 1 : -1;
    }

    if (effective_signum_lhs == 0) {
        return 0;
    }

    Flag lhs_is_inf = (lhs->bits & DECINF);
    Flag rhs_is_inf = (rhs->bits & DECINF);

    if (lhs_is_inf || rhs_is_inf) {
        if (lhs_is_inf && rhs_is_inf) {
            return 0;
        } else if (lhs_is_inf) {
            return effective_signum_lhs;
        } else { // rhs_is_inf
            return -effective_signum_rhs;
        }
    }

    const decNumber *comp_lhs = lhs;
    const decNumber *comp_rhs = rhs;
    Int magnitude_compare_adjust = 1;

    if (comp_lhs->exponent > comp_rhs->exponent) {
        const decNumber *temp = comp_lhs;
        comp_lhs = comp_rhs;
        comp_rhs = temp;
        magnitude_compare_adjust = -1;
    }

    Int magnitude_result = decUnitCompare(comp_lhs->lsu, D2U(comp_lhs->digits),
                                          comp_rhs->lsu, D2U(comp_rhs->digits),
                                          comp_rhs->exponent - comp_lhs->exponent);

    if (magnitude_result == BADINT) {
        return BADINT;
    }

    magnitude_result *= magnitude_compare_adjust;

    return magnitude_result * effective_signum_lhs;
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
  Unit	*acc;
  Unit	accbuff[SD2U(DECBUFFER*2+1)];
  Unit	*allocacc = NULL;
  Int	accunits;
  Int	needed_units;
  const Unit *l_ptr, *r_ptr, *u_ptr;
  Int	exp_units, exp_rem;
  Int	result;

  if (exp == 0) {
    if (alength > blength) return 1;
    if (alength < blength) return -1;

    l_ptr = a + alength - 1;
    r_ptr = b + alength - 1;
    for (; l_ptr >= a; l_ptr--, r_ptr--) {
      if (*l_ptr > *r_ptr) return 1;
      if (*l_ptr < *r_ptr) return -1;
    }
    return 0;
  }

  if (alength > blength + (Int)D2U(exp)) return 1;
  if (alength + 1 < blength + (Int)D2U(exp)) return -1;

  needed_units = blength + D2U(exp);
  if (needed_units < alength) {
      needed_units = alength;
  }
  needed_units += 2;

  acc = accbuff;
  if (needed_units * sizeof(Unit) > sizeof(accbuff)) {
    allocacc = (Unit *)malloc(needed_units * sizeof(Unit));
    if (allocacc == NULL) {
      return BADINT;
    }
    acc = allocacc;
  }

  exp_units = exp / DECDPUN;
  exp_rem = exp % DECDPUN;

  accunits = decUnitAddSub(a, alength, b, blength, exp_units, acc,
                           -(Int)powers[exp_rem]);

  if (accunits < 0) {
    result = -1;
  } else {
    u_ptr = acc;
    while (u_ptr < acc + accunits - 1 && *u_ptr == 0) {
      u_ptr++;
    }
    result = (*u_ptr == 0 ? 0 : +1);
  }

  free(allocacc);

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
static eInt normalizeCarry(eInt *current_carry_ptr, Unit *unit_ptr, const eInt base_plus_1) {
    eInt carry = *current_carry_ptr;

    if ((ueInt)carry <= DECDPUNMAX) {
        *unit_ptr = (Unit)carry;
        return 0;
    }

    #if DECDPUN == 4
        eInt est;
        if (carry >= 0) {
            est = (((ueInt)carry >> 11) * 53687) >> 18;
            *unit_ptr = (Unit)(carry - est * base_plus_1);
            carry = est;
            if (*unit_ptr < base_plus_1) return carry;
            carry++;
            *unit_ptr -= base_plus_1;
            return carry;
        } else { // negative case
            carry = carry + base_plus_1 * base_plus_1; // make positive temporarily
            est = (((ueInt)carry >> 11) * 53687) >> 18;
            *unit_ptr = (Unit)(carry - est * base_plus_1);
            carry = est - base_plus_1; // correctly negative
            if (*unit_ptr < base_plus_1) return carry;
            carry++;
            *unit_ptr -= base_plus_1;
            return carry;
        }
    #elif DECDPUN == 3
        eInt est;
        if (carry >= 0) {
            est = (((ueInt)carry >> 3) * 16777) >> 21;
            *unit_ptr = (Unit)(carry - est * base_plus_1);
            carry = est;
            if (*unit_ptr < base_plus_1) return carry;
            carry++;
            *unit_ptr -= base_plus_1;
            return carry;
        } else { // negative case
            carry = carry + base_plus_1 * base_plus_1; // make positive temporarily
            est = (((ueInt)carry >> 3) * 16777) >> 21;
            *unit_ptr = (Unit)(carry - est * base_plus_1);
            carry = est - base_plus_1; // correctly negative
            if (*unit_ptr < base_plus_1) return carry;
            carry++;
            *unit_ptr -= base_plus_1;
            return carry;
        }
    #elif DECDPUN <= 2
        eInt est;
        if (carry >= 0) {
            est = QUOT10(carry, DECDPUN);
            *unit_ptr = (Unit)(carry - est * base_plus_1);
            return est;
        } else { // negative case
            carry = carry + base_plus_1 * base_plus_1; // make positive temporarily
            est = QUOT10(carry, DECDPUN);
            *unit_ptr = (Unit)(carry - est * base_plus_1);
            return est - base_plus_1; // correctly negative
        }
    #else // General case (DECDPUN > 4)
        // Fastpath for carry +1 (positive)
        if (carry >= 0 && (ueInt)carry < base_plus_1 * 2) {
            *unit_ptr = (Unit)(carry - base_plus_1);
            return 1;
        }
        if (carry >= 0) {
            *unit_ptr = (Unit)(carry % base_plus_1);
            return carry / base_plus_1;
        } else {
            // Negative case requires making carry positive before modulo/division
            carry = carry + base_plus_1 * base_plus_1; // Ensure carry is positive for % and /
            *unit_ptr = (Unit)(carry % base_plus_1);
            return carry / base_plus_1 - base_plus_1; // Adjust quotient to be correctly negative
        }
    #endif
}

static Int decUnitAddSub(const Unit *a, Int alength,
                         const Unit *b, Int blength, Int bshift,
                         Unit *c, Int m) {
    #if DECTRACE
    if (alength < 1 || blength < 1)
        printf("decUnitAddSub: alen blen m %ld %ld [%ld]\n", alength, blength, m);
    #endif

    // Save original `c` pointer for calculating final length relative to start.
    Unit *c_start_ptr = c;
    eInt carry = 0;
    const eInt base_plus_1 = DECDPUNMAX + 1;

    // `a_idx`, `b_idx`, `c_idx` are zero-based indices into `a`, `b`, and `c` arrays, respectively.
    Int a_idx = 0;
    Int b_idx = 0;
    Int c_idx = 0;

    // --- Phase 1: Handle units before 'b' starts contributing (due to 'bshift') ---
    // These units are copied from 'a' to 'c', or simply skipped if in-place.
    if (bshift > 0) {
        if (a == c && bshift <= alength) {
            // In-place operation and 'a' has enough initial units.
            // These units (c[0]...c[bshift-1]) are already correct from 'a' and are not affected by 'b'.
            // Simply advance 'a_idx' and 'c_idx' to the point where 'b' begins its contribution.
            a_idx += bshift;
            c_idx += bshift;
        } else {
            // Not in-place, or 'bshift' extends beyond 'a''s initial length.
            // Explicitly copy 'a' units (or pad with 0) to 'c'.
            for (Int i = 0; i < bshift; ++i) {
                if (i < alength) {
                    c[c_idx] = a[i];
                } else {
                    c[c_idx] = 0; // Pad with zero if 'a' is shorter than 'bshift'
                }
                c_idx++; // Advance 'c' write index
                // 'a_idx' is NOT advanced here. It will track the effective start for 'a' during addition.
            }
            // After this loop, 'c_idx' is `bshift`.
            // `a_idx` effectively needs to start at `bshift` for additions.
            // `b_idx` starts at `0`.
            a_idx = bshift; // Set 'a_idx' to its effective starting point for addition
        }
    }
    // At this point, `c_idx` is the index in `c` where `b[0]` aligns.
    // `a_idx` is the index in `a` from which units will be added.
    // `b_idx` is `0` (start of `b`).

    // Calculate effective lengths for the addition loops
    // These lengths determine how many units from 'a' and 'b' (from their current `a_idx`, `b_idx`)
    // will participate in the addition process.
    Int current_alength = alength - a_idx; // Remaining length of 'a'
    Int current_blength = blength - b_idx; // Remaining length of 'b'

    // 'units_both_contribute' is the length of the region where both 'a' and 'b' have units to add.
    Int units_both_contribute = (current_alength < current_blength) ? current_alength : current_blength;
    if (units_both_contribute < 0) units_both_contribute = 0;

    // 'units_total_add' is the length of the region where at least one of 'a' or 'b' has units to add.
    Int units_total_add = (current_alength > current_blength) ? current_alength : current_blength;
    if (units_total_add < 0) units_total_add = 0;

    // --- Phase 2: Add units where both 'a' and 'b' contribute ---
    for (Int i = 0; i < units_both_contribute; ++i) {
        carry += (eInt)a[a_idx];
        a_idx++;
        carry += ((eInt)b[b_idx]) * m;
        b_idx++;
        carry = normalizeCarry(&carry, &c[c_idx], base_plus_1);
        c_idx++;
    }

    // --- Phase 3: Add remaining units from 'a' or 'b' (whichever is longer) ---
    // This loop processes any remaining units after one of 'a' or 'b' (or both) have been exhausted
    // in the common contribution phase.
    for (Int i = 0; i < (units_total_add - units_both_contribute); ++i) {
        if (a_idx < alength) { // Check if 'a' still has units
            carry += (eInt)a[a_idx];
            a_idx++;
        }
        if (b_idx < blength) { // Check if 'b' still has units
            carry += ((eInt)b[b_idx]) * m;
            b_idx++;
        }
        // If both 'a' and 'b' are exhausted, `carry` will just propagate or zero out.
        carry = normalizeCarry(&carry, &c[c_idx], base_plus_1);
        c_idx++;
    }

    // --- Final carry or borrow handling ---
    if (carry == 0) {
        return c_idx; // No final carry or borrow. Return total length written.
    } else if (carry > 0) {
        // Positive carry: an additional unit is needed for the result.
        c[c_idx] = (Unit)carry;
        c_idx++;
        return c_idx; // Return new total length, including the carry unit.
    } else { // carry < 0: signifies a borrow (the result is a negative number)
        // The current contents of `c` represent a positive magnitude that needs to be complemented.
        // `complement_propagate` acts as the 'borrow' propagating through the complementation.
        eInt complement_propagate = 1; // Start with 1 for base's complement of the lowest digit.
        for (Int i = 0; i < c_idx; ++i) {
            eInt temp_sum = base_plus_1 - c[i] - 1 + complement_propagate;
            c[i] = (Unit)(temp_sum % base_plus_1);
            complement_propagate = temp_sum / base_plus_1;
        }

        // After complementing the existing digits, handle any remaining propagation.
        // The original logic calculates `(add - carry - 1)`.
        // `add` here is `complement_propagate`. `carry` is the initial negative carry.
        eInt final_borrow_unit_val = complement_propagate - carry - 1;
        if (final_borrow_unit_val != 0) {
            // If non-zero, this value forms an additional most significant unit of the complemented result.
            c[c_idx] = (Unit)final_borrow_unit_val;
            c_idx++;
        }

        // Return a negative length to indicate that the result is a borrowed/negative number.
        // The absolute value of the returned length indicates the number of units.
        return -c_idx;
    }
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
  Int	d;
  Int	exp;
  uInt	cut;
  Unit	*up;

  #if DECCHECK
  if (decCheckOperands(dn, DECUNUSED, DECUNUSED, DECUNCONT)) {
    return dn;
  }
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
    #if DECDPUN<=4
      uInt quot = QUOT10(*up, cut);
      if ((*up - quot * powers[cut]) != 0) {
        break;
      }
    #else
      if (*up % powers[cut] != 0) {
        break;
      }
    #endif

    if (!all) {
      if (exp <= 0) {
	    if (exp == 0) {
            break;
        }
	    exp++;
      }
    }

    cut++;
    if (cut > DECDPUN) {
      up++;
      cut = 1;
    }
  }

  if (d == 0) {
    return dn;
  }

  if (set->clamp && !noclamp) {
    Int maxd = set->emax - set->digits + 1 - dn->exponent;
    if (maxd <= 0) {
        return dn;
    }
    if (d > maxd) {
        d = maxd;
    }
  }

  if (d == 0) {
    return dn;
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
static void decReverse(Unit *low, Unit *high) {
  Unit temp;
  for (; low < high; low++, high--) {
    temp = *low;
    *low = *high;
    *high = temp;
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
  Unit	*target_unit_ptr, *source_unit_ptr, *max_target_unit_ptr;
  Int	intra_unit_remainder_digits_count;
  uInt	current_unit_carry_accumulator;

  if (shift == 0) {
    return digits;
  }

  if ((digits + shift) <= DECDPUN) {
    *uar = (Unit)(*uar * powers[shift]);
    return digits + shift;
  }

  current_unit_carry_accumulator = 0;
  source_unit_ptr = uar + D2U(digits) - 1;
  target_unit_ptr = source_unit_ptr + D2U(shift);
  
  intra_unit_remainder_digits_count = DECDPUN - MSUDIGITS(shift);

  max_target_unit_ptr = uar + D2U(digits + shift) - 1;

  for (; source_unit_ptr >= uar; source_unit_ptr--, target_unit_ptr--) {
    uInt current_source_value = *source_unit_ptr;
    uInt high_part_of_unit;
    uInt low_part_of_unit;

    #if DECDPUN <= 4
      high_part_of_unit = QUOT10(current_source_value, intra_unit_remainder_digits_count);
      low_part_of_unit = current_source_value - high_part_of_unit * powers[intra_unit_remainder_digits_count];
    #else
      low_part_of_unit = current_source_value % powers[intra_unit_remainder_digits_count];
      high_part_of_unit = current_source_value / powers[intra_unit_remainder_digits_count];
    #endif
    
    current_unit_carry_accumulator += high_part_of_unit;

    if (target_unit_ptr <= max_target_unit_ptr) {
      *target_unit_ptr = (Unit)current_unit_carry_accumulator;
    }
    
    current_unit_carry_accumulator = low_part_of_unit * powers[DECDPUN - intra_unit_remainder_digits_count];
  }

  for (; target_unit_ptr >= uar; target_unit_ptr--) {
    *target_unit_ptr = (Unit)current_unit_carry_accumulator;
    current_unit_carry_accumulator = 0;
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
  // Input validation: Ensure sizes/shifts are non-negative.
  // Pointers are assumed to be valid for the given `units`.
  // If `units` is 0, `uar` is assumed to point to at least 1 unit of memory
  // where the result (value 0, represented by a single 0 unit) can be written.
  if (units < 0 || shift < 0) {
    // For invalid input, return 0 (representing an empty or erroneous result).
    // In a production system, an assertion or more specific error handling
    // might be appropriate.
    return 0;
  }

  // Fast path 1: No shift needed.
  if (shift == 0) {
    return units;
  }

  // Calculate the total number of decimal digits currently represented by uar.
  // If units is 0, total_source_digits will be 0.
  Int total_source_digits = units * DECDPUN;

  // Fast path 2: If the shift amount is greater than or equal to the total digits,
  // the number becomes zero. A single unit containing 0 represents zero.
  if (shift >= total_source_digits) {
    // If there's at least one unit of space, write the zero value.
    if (units > 0) {
      *uar = 0;
      // Clear any other units to ensure a canonical representation of zero and
      // prevent SonarCloud warnings about stale data.
      for (Int i = 1; i < units; ++i) {
        uar[i] = 0;
      }
    } else { // If units was 0, ensure we have a place to write the single 0.
             // This assumes uar is allocated for at least 1 unit even if units=0.
             // If this assumption is false, it's a contract violation from the caller.
             // If uar is NULL and units is 0, *uar=0 would be a crash.
             // For simplicity, we assume uar is valid if units==0 and we need to write 0.
      if (uar != NULL) { // Defensive check
        *uar = 0;
      }
    }
    return 1; // Always return 1 unit for the value zero.
  }

  // Determine the number of full units to discard and the intra-unit digit shift.
  Int units_to_discard = shift / DECDPUN;
  Int intra_unit_shift = shift % DECDPUN;

  Unit *source_ptr = uar + units_to_discard; // Source starts after discarded full units.
  Unit *dest_ptr = uar;                     // Destination starts at the beginning of the array.

  Int current_result_units = 0; // Tracks how many units have been written to the destination.

  // Pre-calculate powers for efficiency and clarity.
  // `powers[intra_unit_shift]` is for dividing to get the most significant part.
  // `powers[DECDPUN - intra_unit_shift]` is for multiplying to shift the least significant part.
  Unit high_part_divisor = powers[intra_unit_shift];
  Unit low_part_multiplier = powers[DECDPUN - intra_unit_shift];

  // If there's no intra-unit shift, it's a simple block copy.
  // This path is clearer and likely more efficient than the general case.
  if (intra_unit_shift == 0) {
    Int units_to_copy = units - units_to_discard;
    for (Int i = 0; i < units_to_copy; ++i) {
      dest_ptr[i] = source_ptr[i];
    }
    current_result_units = units_to_copy;
  } else {
    // General case: Each target unit combines digits from two source units.
    // The loop iterates as long as there are digits to process in the output.
    Int processed_output_digits = 0;
    Int new_total_digits = total_source_digits - shift;

    // `current_source_val` holds the current unit value from the source array.
    // `current_high_part_of_source` is the most significant part (after intra-unit shift)
    // that contributes to the current destination unit.
    // This variable's name aligns with `quot` from the original code.
    Unit current_source_val = *source_ptr;
    Unit current_high_part_of_source = current_source_val / high_part_divisor;

    while (processed_output_digits < new_total_digits) {
      // Write the most significant part from the current source unit to the destination.
      *dest_ptr = current_high_part_of_source;
      processed_output_digits += (DECDPUN - intra_unit_shift); // Digits contributed from current unit's MSB.

      // If all required output digits are processed, or we've written beyond, break.
      if (processed_output_digits >= new_total_digits) {
        current_result_units = (dest_ptr - uar) + 1; // Calculate actual units written.
        break;
      }

      source_ptr++; // Move to the next source unit to get its least significant part.
      // If `source_ptr` goes beyond the original `units` bound, treat as zero padding.
      current_source_val = (source_ptr < uar + units) ? *source_ptr : 0;

      // Extract the least significant part from the *next* source unit and shift it into position.
      // This variable's name aligns with `rem` from the original code.
      Unit next_low_part_of_source = (current_source_val % high_part_divisor) * low_part_multiplier;

      // Add the least significant part to the current destination unit.
      *dest_ptr = (Unit)(*dest_ptr + next_low_part_of_source);
      processed_output_digits += intra_unit_shift; // Digits contributed from next unit's LSB.

      // If all required output digits are processed, or we've written beyond, break.
      if (processed_output_digits >= new_total_digits) {
        current_result_units = (dest_ptr - uar) + 1; // Calculate actual units written.
        break;
      }

      // Prepare the most significant part from the current source unit for the next destination unit.
      current_high_part_of_source = current_source_val / high_part_divisor;
      dest_ptr++; // Move to the next destination unit.
    }
  }

  // After copying, trim any leading zero units, unless the result is the number zero itself (single unit `0`).
  // This ensures a canonical representation (e.g., [0, 123] becomes [123]).
  while (current_result_units > 1 && uar[current_result_units - 1] == 0) {
    current_result_units--;
  }

  // Clear any units in the original array that are no longer part of the number.
  // This prevents SonarCloud from flagging uninitialized memory usage if `units`
  // was larger than `current_result_units` and `uar` was later used with `units`.
  for (Int i = current_result_units; i < units; ++i) {
    uar[i] = 0;
  }

  return current_result_units;
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
#ifndef DEC_ARGUMENT_ERROR
#define DEC_ARGUMENT_ERROR 0x00000001U // Example: A new bit for invalid arguments to decNumber functions
#endif

static void decCopyFit(decNumber *dest, const decNumber *src,
                       decContext *set, Int *residue, uInt *status) {
  // Ensure critical input/output pointers are valid before dereferencing
  // to prevent crashes and improve reliability.
  if (dest == NULL || src == NULL) {
    if (status != NULL) {
      // Indicate an argument error if a status pointer is provided.
      // This assumes 'status' is a bitfield where error bits can be OR-ed.
      *status |= DEC_ARGUMENT_ERROR;
    }
    return; // Prevent dereferencing NULL pointers and exit gracefully.
  }

  // Copy basic properties from the source decNumber to the destination decNumber.
  dest->bits = src->bits;
  dest->exponent = src->exponent;

  // Delegate the setting of coefficients to the specialized function.
  // It is assumed that 'decSetCoeff' is responsible for validating
  // 'set', 'residue', and 'status' pointers before it uses them.
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
  Int discard_digits;
  const Unit *source_ptr;
  Unit *target_ptr;
  Int current_loop_digits_count;
  
  discard_digits = len - set->digits;
  
  // Case 1: No digits are being discarded (or set->digits > len).
  if (discard_digits <= 0) {
    if (dn->lsu != lsu) { // Copy needed if source and target are different.
      current_loop_digits_count = len;
      source_ptr = lsu;
      for (target_ptr = dn->lsu; current_loop_digits_count > 0; target_ptr++, source_ptr++, current_loop_digits_count -= DECDPUN) {
        *target_ptr = *source_ptr;
      }
      dn->digits = len;
    }
    // If there was any previous residue, record inexactitude.
    if (*residue != 0) *status |= (DEC_Inexact | DEC_Rounded);
    return;
  }

  // From here, `discard_digits` > 0.
  dn->exponent += discard_digits;
  *status |= DEC_Rounded;
  
  // If previous residue was "more than sticky" (3, 5, 7), reduce it to "sticky" (1)
  // because those digits are now to the right of the current discard point.
  if (*residue > 1) *residue = 1;

  // Case 2: All digits (and possibly more) are being discarded. The result is 0.
  if (discard_digits >= len) {
    current_loop_digits_count = len;
    source_ptr = lsu;
    for (; current_loop_digits_count > 0; source_ptr++, current_loop_digits_count -= DECDPUN) {
      if (*source_ptr != 0) {
        *residue = 1; // Mark sticky if any original digit was non-zero
        break;
      }
    }
    if (*residue != 0) *status |= DEC_Inexact;
    *dn->lsu = 0; // Coefficient becomes 0
    dn->digits = 1; // Length becomes 1 for 0
    return;
  }

  // Case 3: Partial discard (some digits kept, some discarded).
  
  // Find the unit containing the first discarded digit and accumulate sticky bits
  // from units entirely to the right of the guard unit.
  source_ptr = lsu;
  current_loop_digits_count = 0; // Accumulates digits processed (sum of DECDPUN for full units)
  while (current_loop_digits_count + DECDPUN < discard_digits) { // Loop while current unit is entirely to be discarded
      if (*source_ptr != 0) *residue = 1; // Found non-zero unit, mark as sticky
      source_ptr++;
      current_loop_digits_count += DECDPUN;
  }
  // `source_ptr` now points to the Unit containing the first discarded digit.
  // `current_loop_digits_count` holds the total digits *before* `*source_ptr`.

  // `guard_digit_pos_in_unit` is the 0-indexed position from right of the guard digit within `*source_ptr`.
  Int guard_digit_pos_in_unit = discard_digits - current_loop_digits_count - 1;

  // Handle the special case where the requested `set->digits` is zero or negative.
  // This effectively means all remaining digits are discarded, resulting in zero.
  if (set->digits <= 0) {
    // Accumulate sticky for all digits from `source_ptr` onwards.
    Int remaining_len = len - current_loop_digits_count;
    const Unit *temp_ptr = source_ptr;
    for (; remaining_len > 0; temp_ptr++, remaining_len -= DECDPUN) {
        if (*temp_ptr != 0) {
            *residue = 1;
            break;
        }
    }
    if (*residue != 0) *status |= DEC_Inexact;
    *dn->lsu = 0; // Result coefficient is 0
    dn->digits = 1; // Length is 1 (for 0)
    return;
  }

  // Determine rounding based on guard digit and sticky bits, then perform shift-copy.
  
  if (guard_digit_pos_in_unit == DECDPUN - 1) { // Unit-boundary case: guard digit is leftmost in `*source_ptr`
    Unit guard_unit_value = *source_ptr;
    Unit half_unit_val = (Unit)powers[DECDPUN] >> 1; // 10^DECDPUN / 2

    if (guard_unit_value > half_unit_val) {
      *residue = 7; // More than half
    } else if (guard_unit_value == half_unit_val) {
      if (*residue == 0) *residue = 5; // Exactly half, no sticky
      else *residue = 7; // Exactly half, but sticky bits present
    } else { // Less than half
      if (guard_unit_value != 0) *residue = 3; // Non-zero but less than half
      // If guard_unit_value is 0, *residue remains its current sticky status (0 or 1).
    }

    // Copy remaining units (those *after* `source_ptr`) to the target.
    current_loop_digits_count = set->digits; // Now digits to end up with
    dn->digits = current_loop_digits_count; // Set the new length
    source_ptr++; // Move to the first unit to be kept.
    for (target_ptr = dn->lsu; current_loop_digits_count > 0; target_ptr++, source_ptr++, current_loop_digits_count -= DECDPUN) {
        *target_ptr = *source_ptr;
    }

  } else { // Guard digit is inside `*source_ptr`, not at its left edge.
    uInt first_discarded_digit; // The guard digit
    uInt current_unit_kept_high_part; // High-order digits of `*source_ptr` that are kept
    uInt current_unit_rem_for_sticky; // Low-order digits of `*source_ptr` that are discarded (excluding guard)
    Unit temp_quotient_val; // For temporary calculation

    // Extract guard digit and kept/sticky parts from `*source_ptr`.
    #if DECDPUN<=4
      temp_quotient_val = QUOT10(*source_ptr, guard_digit_pos_in_unit + 1);
      first_discarded_digit = (*source_ptr - X10(temp_quotient_val) * powers[guard_digit_pos_in_unit + 1]) / powers[guard_digit_pos_in_unit];
      current_unit_rem_for_sticky = *source_ptr - temp_quotient_val * powers[guard_digit_pos_in_unit + 1] - first_discarded_digit * powers[guard_digit_pos_in_unit];
      current_unit_kept_high_part = temp_quotient_val;
    #else
      Unit p_cut = powers[guard_digit_pos_in_unit];
      Unit p_cut_plus_1 = powers[guard_digit_pos_in_unit + 1];
      
      current_unit_rem_for_sticky = *source_ptr % p_cut;
      first_discarded_digit = (*source_ptr % p_cut_plus_1) / p_cut;
      current_unit_kept_high_part = *source_ptr / p_cut_plus_1;
    #endif

    // Accumulate sticky from digits to the right of the guard digit.
    if (current_unit_rem_for_sticky != 0) {
        if (*residue == 0) *residue = 1;
    }
    // Accumulate residue from the guard digit.
    *residue += resmap[first_discarded_digit];

    // Shift-copy the coefficient array to the result number.
    current_loop_digits_count = set->digits; // Now digits to end up with
    dn->digits = current_loop_digits_count; // Set the new length

    uInt current_unit_quotient = current_unit_kept_high_part; // Initial `quot` value for the shift loop
    source_ptr++; // Advance `source_ptr` to the unit *after* the one containing the guard digit.
    
    for (target_ptr = dn->lsu; ; target_ptr++) {
	    *target_ptr = (Unit)current_unit_quotient;
	    current_loop_digits_count -= (DECDPUN - (guard_digit_pos_in_unit + 1)); // Digits placed from the high part
	    if (current_loop_digits_count <= 0) break;
	    
	    uInt next_source_unit_value = *source_ptr;
	    uInt next_unit_remainder;
	    
	    #if DECDPUN<=4
	      current_unit_quotient = QUOT10(next_source_unit_value, guard_digit_pos_in_unit);
	      next_unit_remainder = next_source_unit_value - current_unit_quotient * powers[guard_digit_pos_in_unit];
	    #else
	      next_unit_remainder = next_source_unit_value % powers[guard_digit_pos_in_unit];
	      current_unit_quotient = next_source_unit_value / powers[guard_digit_pos_in_unit];
	    #endif
	    
	    // Add the low part of the next source unit (shifted left) to the current target unit.
	    *target_ptr = (Unit)(*target_ptr + next_unit_remainder * powers[DECDPUN - guard_digit_pos_in_unit]);
	    
	    current_loop_digits_count -= guard_digit_pos_in_unit; // Digits placed from the low part
	    if (current_loop_digits_count <= 0) break;

        source_ptr++; // Advance source_ptr for the next iteration if the loop continues.
    }
  }

  // Final status update for inexactitude.
  if (*residue != 0) *status |= DEC_Inexact;
  return;
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
static void handleBumpUpSpecial(decNumber *dn, decContext *set, uInt *status) {
    Unit *up;
    uInt remaining_digits = dn->digits;
    int is_all_nines = 1;

    // First, check if the number consists of all nines in its current units
    for (up = dn->lsu; ; up++) {
        if (remaining_digits <= DECDPUN) { // This is the last Unit (the most significant unit, MSU)
            if (*up != powers[remaining_digits] - 1) { // Not all nines for its specific digit count
                is_all_nines = 0;
            }
            break; // Done checking all units
        }
        // A full unit to check, with more units to follow
        if (*up != DECDPUNMAX) { // Not all nines (e.g., 999 for DECDPUN=3)
            is_all_nines = 0;
            break;
        }
        remaining_digits -= DECDPUN;
    }

    if (!is_all_nines) {
        // Not the special all-nines case, use generic add
        decUnitAddSub(dn->lsu, D2U(dn->digits), uarrone, 1, 0, dn->lsu, 1);
        return;
    }

    // Special handling for all nines (e.g., 99 -> 100 by shifting exponent)
    Unit *msu_ptr = dn->lsu + D2U(dn->digits) - 1; // Pointer to the most significant unit
    uInt msu_digits = dn->digits % DECDPUN;
    if (msu_digits == 0) msu_digits = DECDPUN; // If a unit is full, it contributes DECDPUN digits

    // Clear all units to 0 except for the MSU, which will hold the new leading '1'
    for (up = dn->lsu; up < msu_ptr; ++up) {
        *up = 0;
    }
    // Set the MSU value (e.g. 99 (2 digits) -> 10, 999 (3 digits) -> 100)
    *msu_ptr = powers[msu_digits - 1];

    dn->exponent++; // Adjust exponent to effectively increase the number's magnitude

    // Check for overflow
    if ((dn->exponent + dn->digits) > set->emax + 1) {
        decSetOverflow(dn, set, status);
    }
}

static void handleBumpDownSpecial(decNumber *dn, decContext *set, uInt *status) {
    Unit *up, *msu_ptr_after_check;
    uInt remaining_digits = dn->digits;
    int is_one_and_zeros = 1; // Assume 100...0 initially

    // First, check if the number is 100...0 (a 1 followed by all zeros)
    for (up = dn->lsu; ; up++) {
        if (remaining_digits <= DECDPUN) { // This is the last Unit (the MSU)
            // Check if it's '1', '10', '100' etc. based on its digit count
            if (*up != powers[remaining_digits - 1]) {
                is_one_and_zeros = 0;
            }
            msu_ptr_after_check = up; // Store pointer to MSU for later modification
            break;
        }
        // A full unit to check, with more units to follow
        if (*up != 0) { // Not zero
            is_one_and_zeros = 0;
            break;
        }
        remaining_digits -= DECDPUN;
    }

    if (!is_one_and_zeros) {
        // Not the special 100...0 case, use generic subtract
        decUnitAddSub(dn->lsu, D2U(dn->digits), uarrone, 1, 0, dn->lsu, -1);
        return;
    }

    // Special handling for 100...0 (e.g., 100 -> 99 by shifting exponent)
    uInt msu_digits_after_check = remaining_digits; // The number of digits in the MSU

    // Set all units to DECDPUNMAX (all nines) except for the MSU
    up = dn->lsu;
    while (up < msu_ptr_after_check) {
        *up = DECDPUNMAX;
        up++;
    }
    // Set the MSU value (e.g., for 100 (3 digits), sets to 999; for 10 (2 digits), sets to 99)
    *msu_ptr_after_check = powers[msu_digits_after_check] - 1;

    dn->exponent--; // Adjust exponent to effectively decrease the number's magnitude

    // Check for underflow and adjust if necessary
    // This condition checks if the number's exponent is at the subnormal boundary after the decrement.
    if (dn->exponent + 1 == set->emin - set->digits + 1) {
        // If it was '1' (single digit), it becomes '0'
        if (msu_digits_after_check == 1 && dn->digits == 1) {
            *msu_ptr_after_check = 0; // The single unit becomes 0
        } else {
            // Drop the highest digit (by decrementing dn->digits) and adjust MSU value
            dn->digits--;
            uInt new_msu_digits = dn->digits % DECDPUN; // Calculate digits in the new, smaller MSU
            if (new_msu_digits == 0) new_msu_digits = DECDPUN; // Full unit
            // Adjust MSU value (e.g. 999 becomes 99 if digits drops from 3 to 2)
            *msu_ptr_after_check = powers[new_msu_digits] - 1;
        }
        dn->exponent++; // Increment exponent to effectively clamp it to the subnormal boundary
        *status |= DEC_Underflow | DEC_Subnormal | DEC_Inexact | DEC_Rounded;
    }
}

static void decApplyRound(decNumber *dn, decContext *set, Int residue, uInt *status) {
    Int bump = 0; // 1 to increment coefficient, -1 to decrement

    if (residue == 0) {
        return; // Nothing to apply
    }

    // Determine whether and how to round, depending on mode
    switch (set->round) {
        case DEC_ROUND_05UP: {
            // Round zero or five up (for reround)
            // Same as DEC_ROUND_DOWN unless positive residue and lsd is 0 or 5 (then bump up),
            // or negative residue and lsd is not 1 or 6 (then bump down).
            Int lsd5 = *dn->lsu % 5; // The lsd of dn->lsu, modulo 5.
            if (residue < 0 && lsd5 != 1) {
                bump = -1;
            } else if (residue > 0 && lsd5 == 0) {
                bump = 1;
            }
            break;
        }
        case DEC_ROUND_DOWN: {
            // Towards zero
            if (residue < 0) {
                bump = -1;
            }
            break;
        }
        case DEC_ROUND_HALF_DOWN: {
            // If >= 0.5 round away from zero (up if positive, down if negative)
            if (residue > 5) {
                bump = 1;
            }
            break;
        }
        case DEC_ROUND_HALF_EVEN: {
            // If > 0.5 round away from zero; if 0.5 round to nearest even
            if (residue > 5) { // > 0.5 goes up
                bump = 1;
            } else if (residue == 5) { // Exactly 0.5000...
                // 0.5 goes up iff [current] lsd is odd
                if (*dn->lsu & 0x01) { // Check if least significant digit unit value is odd
                    bump = 1;
                }
            }
            break;
        }
        case DEC_ROUND_HALF_UP: {
            // If >= 0.5 round away from zero
            if (residue >= 5) {
                bump = 1;
            }
            break;
        }
        case DEC_ROUND_UP: {
            // Away from zero
            if (residue > 0) {
                bump = 1;
            }
            break;
        }
        case DEC_ROUND_CEILING: {
            // Towards positive infinity
            if (decNumberIsNegative(dn)) {
                if (residue < 0) {
                    bump = -1;
                }
            } else { // Positive or zero
                if (residue > 0) {
                    bump = 1;
                }
            }
            break;
        }
        case DEC_ROUND_FLOOR: {
            // Towards negative infinity
            if (decNumberIsNegative(dn)) {
                if (residue > 0) {
                    bump = 1;
                }
            } else { // Positive or zero
                if (residue < 0) {
                    bump = -1;
                }
            }
            break;
        }
        default: { // Unknown rounding mode
            *status |= DEC_Invalid_context;
            break;
        }
    } // End switch (set->round)

    if (bump == 0) {
        return; // No action required
    }

    // Apply the bump, using special handling for all nines/100...0 to avoid unit overflow/underflow
    if (bump > 0) {
        handleBumpUpSpecial(dn, set, status);
    } else { // bump < 0
        handleBumpDownSpecial(dn, set, status);
    }
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
static void decFinalize(decNumber *dn, decContext *set, Int *residue,
			uInt *status) {
  Int tinyexp = set->emin - dn->digits + 1;

  if (dn->exponent <= tinyexp) {
    if (dn->exponent < tinyexp) {
      decSetSubnormal(dn, set, residue, status);
      return;
    }
    decNumber nmin_one;
    decNumberZero(&nmin_one);
    nmin_one.lsu[0] = 1;
    nmin_one.exponent = set->emin;

    Int comp = decCompare(dn, &nmin_one, 1);
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

  Int normal_max_exponent = set->emax - set->digits + 1;
  if (dn->exponent <= normal_max_exponent) {
    return;
  }

  Int number_max_exponent = set->emax - dn->digits + 1;
  if (dn->exponent > number_max_exponent) {
    decSetOverflow(dn, set, status);
    return;
  }

  if (set->clamp) {
    Int shift = dn->exponent - normal_max_exponent;
    if (!ISZERO(dn)) {
      dn->digits = decShiftToMost(dn->lsu, dn->digits, shift);
    }
    dn->exponent -= shift;
    *status |= DEC_Clamped;
  }
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
  Flag needmax = 0;
  uByte sign = dn->bits & DECNEG;

  if (ISZERO(dn)) {
    Int emax = set->emax;
    if (set->clamp) emax -= set->digits - 1;
    if (dn->exponent > emax) {
      dn->exponent = emax;
      *status |= DEC_Clamped;
    }
    return;
  }

  decNumberZero(dn);
  switch (set->round) {
    case DEC_ROUND_DOWN:
    case DEC_ROUND_05UP:
      needmax = 1;
      break;
    case DEC_ROUND_CEILING:
      if (sign) needmax = 1;
      break;
    case DEC_ROUND_FLOOR:
      if (!sign) needmax = 1;
      break;
    default:
      break;
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
  Unit *up = dn->lsu;
  Int count = set->digits;
  dn->digits = count;

  while (count > DECDPUN) {
    *up = DECDPUNMAX;
    up++;
    count -= DECDPUN;
  }
  *up = (Unit)(powers[count] - 1);
  dn->bits = 0;
  dn->exponent = set->emax - set->digits + 1;
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
    if (*residue != 0) {
      *status |= DEC_Invalid_operation;
    }
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
    uInt initial_status = *status;

    // Check for invalid context parameters
    int const context_digits_invalid = (set->digits > DEC_MAX_MATH);
    int const context_emax_invalid = (set->emax > DEC_MAX_MATH);
    int const context_emin_invalid = (-set->emin > DEC_MAX_MATH);

    if (context_digits_invalid || context_emax_invalid || context_emin_invalid) {
        *status |= DEC_Invalid_context;
    } else if (!ISZERO(rhs)) {
        // Only check decNumber parameters if rhs is not zero, as zero has special handling
        int const rhs_exponent_plus_digits = rhs->exponent + rhs->digits;

        int const rhs_digits_invalid = (rhs->digits > DEC_MAX_MATH);
        int const rhs_exponent_plus_digits_too_large = (rhs_exponent_plus_digits > DEC_MAX_MATH + 1);
        int const rhs_exponent_plus_digits_too_small = (rhs_exponent_plus_digits < 2 * (1 - DEC_MAX_MATH));

        if (rhs_digits_invalid || rhs_exponent_plus_digits_too_large || rhs_exponent_plus_digits_too_small) {
            *status |= DEC_Invalid_operation;
        }
    }

    // Return true if the status was changed, false otherwise
    return (*status != initial_status);
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
  long long accumulated_value = 0;
  const Unit *current_unit_ptr;
  int digits_processed_power = 0;
  int integral_length = dn->digits + dn->exponent;
  Flag is_negative = decNumberIsNegative(dn);

  if (ISZERO(dn)) {
    return 0;
  }

  current_unit_ptr = dn->lsu;

  if (dn->exponent < 0) {
    int fractional_digits_to_discard = -dn->exponent;

    while (fractional_digits_to_discard >= DECDPUN) {
      if (*current_unit_ptr != 0) {
        return BADINT;
      }
      fractional_digits_to_discard -= DECDPUN;
      current_unit_ptr++;
    }

    if (fractional_digits_to_discard > 0) {
      if ((*current_unit_ptr % powers[fractional_digits_to_discard]) != 0) {
        return BADINT;
      }
      accumulated_value = *current_unit_ptr / powers[fractional_digits_to_discard];
      digits_processed_power = DECDPUN - fractional_digits_to_discard;
      current_unit_ptr++;
    }
  }

  if (accumulated_value == 0 && digits_processed_power == 0) {
    accumulated_value = *current_unit_ptr;
    digits_processed_power = DECDPUN;
    current_unit_ptr++;
  }

  while (digits_processed_power < integral_length) {
    accumulated_value += (long long)*current_unit_ptr * powers[digits_processed_power];
    digits_processed_power += DECDPUN;
    current_unit_ptr++;
  }

  const int MAX_INT_DEC_DIGITS = 10;
  const long long POS_INT_MAG_LIMIT_10_DIGITS = 999999999LL;
  const long long NEG_INT_MAG_LIMIT_10_DIGITS = 1999999997LL;

  bool is_too_large_for_int = false;

  if (integral_length > MAX_INT_DEC_DIGITS) {
    is_too_large_for_int = true;
  } else if (integral_length == MAX_INT_DEC_DIGITS) {
    if (is_negative && accumulated_value > NEG_INT_MAG_LIMIT_10_DIGITS) {
      is_too_large_for_int = true;
    } else if (!is_negative && accumulated_value > POS_INT_MAG_LIMIT_10_DIGITS) {
      is_too_large_for_int = true;
    }
  }

  if (is_too_large_for_int) {
    return (accumulated_value & 1) ? BIGODD : BIGEVEN;
  }

  if (is_negative) {
    return (Int)-accumulated_value;
  }
  return (Int)accumulated_value;
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
    if (dn == NULL) {
        return NULL;
    }

    if (drop < 0) {
        return NULL;
    }

    if (drop >= dn->digits) {
        dn->lsu[0] = 0;
        dn->digits = 1;
        return dn;
    }

    Int digitsToKeep = dn->digits - drop;
    Int targetMsuIndex = D2U(digitsToKeep) - 1;
    Unit *targetMsuPtr = dn->lsu + targetMsuIndex;

    Int retainedDigitsInMsu = MSUDIGITS(digitsToKeep);

    if (retainedDigitsInMsu != DECDPUN) {
        *targetMsuPtr %= powers[retainedDigitsInMsu];
    }

    dn->digits = decGetDigits(dn->lsu, targetMsuIndex + 1);

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
static int decBiStr(const char *targ, const char *str1, const char *str2) {
  if (targ == NULL || str1 == NULL || str2 == NULL) {
    return 0;
  }

  while (*targ != '\0') {
    if (*targ != *str1 && *targ != *str2) {
      return 0;
    }
    targ++;
    str1++;
    str2++;
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
  const decNumber *effective_source;

  if ((lhs->bits & DECSNAN) != 0) {
    *status |= DEC_Invalid_operation | DEC_sNaN;
    effective_source = lhs;
  } else if (rhs != NULL && (rhs->bits & DECSNAN) != 0) {
    *status |= DEC_Invalid_operation | DEC_sNaN;
    effective_source = rhs;
  } else if ((lhs->bits & DECNAN) != 0) {
    effective_source = lhs;
  } else if (rhs != NULL && (rhs->bits & DECNAN) != 0) {
    effective_source = rhs;
  } else {
    effective_source = (rhs == NULL) ? lhs : rhs;
  }

  if (effective_source->digits <= set->digits) {
    decNumberCopy(res, effective_source);
  } else {
    res->bits = effective_source->bits;

    const size_t units_to_copy = D2U(set->digits);

    Unit *res_units = res->lsu;
    const Unit *src_units = effective_source->lsu;

    for (size_t i = 0; i < units_to_copy; ++i) {
      res_units[i] = src_units[i];
    }

    res->digits = units_to_copy * DECDPUN;

    if (res->digits > set->digits) {
      decDecap(res, res->digits - set->digits);
    }
  }

  res->bits &= ~DECSNAN;
  res->bits |= DECNAN;
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
  if ((status & DEC_NaNs) && !(status & DEC_sNaN)) {
    decNumberZero(dn);
    dn->bits = DECNAN;
  }

  if (status & DEC_sNaN) {
    status &= ~DEC_sNaN;
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
    // Handle cases where the number is zero or represented by an invalid length.
    // A number represented by zero length or a non-positive length is considered zero,
    // which conventionally has one digit.
    if (len <= 0) {
        return 1;
    }

    // Find the Most Significant Unit (MSU) that is not zero.
    // Start scanning from the highest index (potential MSU).
    Unit *up = uar + (len - 1);

    // Skip over all zero units from the high end until a non-zero unit is found
    // or the beginning of the array is reached.
    while (up >= uar && *up == 0) {
        up--;
    }

    // If 'up' has gone past the beginning of the array, it means all units were zero.
    // In this case, the number is zero, and it has one digit.
    if (up < uar) {
        return 1;
    }

    // At this point, 'up' points to the first (most significant) non-zero unit.
    // Initialize digits count. This unit contributes at least one digit.
    Int digits = 1;
    uInt msu_value = *up;

    // Increment digits for each additional power of 10 that the msu_value is greater than or equal to.
    // This loop efficiently determines the number of digits in msu_value.
    // 'powers' is assumed to be a globally or statically accessible array like {1, 10, 100, ...}.
    // It must be sufficiently sized, holding at least powers[DECDPUN-1].
    for (int i = 1; i < DECDPUN; ++i) {
        if (msu_value < powers[i]) {
            break;
        }
        digits++;
    }

    // Add the fixed number of digits contributed by all less significant units.
    // The number of units between 'uar' (inclusive) and 'up' (exclusive) each contribute DECDPUN digits.
    digits += (up - uar) * DECDPUN;

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
