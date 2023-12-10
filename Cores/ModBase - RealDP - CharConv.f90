
MODULE ModBase_RealDP_CharConv

!^ **PURPOSE OF THIS MODULE**:
    ! This module contains routines that perform a conversion between a 64-bit
    !   floating point number and a string.
    !
!^ **REFERENCE TECHNICAL ARTICLES**:
    ! [1]  Junekey Jeon.  [Dragonbox: A New Floating-Point Binary-to-Decimal Conversion Algorithm](https://github.com/jk-jeon/dragonbox/blob/master/other_files/Dragonbox.pdf)
    ! [2]  Ulf Adams.  [Ryu: Fast Float-to-String Conversion](https://dl.acm.org/doi/10.1145/3192366.3192369)
    ! [3]  Raffaello Giulietti.  [The Schubfach way to render doubles](https://drive.google.com/open?id=1luHhyQF9zKlM8yJ1nebU0OgVYhfC6CBN)
    ! [4]  Clinger WD. [How to Read Floating Point Numbers Accurately](https://doi.org/10.1145/989393.989430),
    !   SIGPLAN Not 2004 Apr;39(4):360–371.
    ! [5]  Daniel Lemire.  [Number Parsing at a Gigabyte per Second](https://arxiv.org/abs/2101.11408),
    !   Software: Practice and Experience 51 (8), 2021.
    ! [6]  Noble Mushtak and Daniel Lemire.  [Fast Number Parsing Without Fallback](https://arxiv.org/abs/2212.06644),
    !   Software: Practice and Experience 53 (7), 2023.
    ! [7]  Bouvier & Zimmermann.  [Division-Free Binary-to-Decimal Conversion](https://hal.inria.fr/hal-00864293v1/document)
    ! [8]  Hacker's Delight, 2nd Edition.
    ! [9]  Nigel Tao.  [The Eisel-Lemire ParseNumberF64 Algorithm](https://nigeltao.github.io/blog/2020/eisel-lemire.html)
    ! [10] Nigel Tao.  [ParseNumberF64 by Simple Decimal Conversion](https://nigeltao.github.io/blog/2020/parse-number-f64-simple.html)
    !
!^ **REFERENCE CODE IMPLEMENTATION**:
    ! [11] [DragonBox: C++ reference implementation](https://github.com/jk-jeon/dragonbox)
    ! [12] [Ryu: C reference implementation](https://github.com/ulfjack/ryu)
    ! [13] [Schubfach: Java reference implementation](https://github.com/c4f7fcce9cb06515/Schubfach)
    ! [14] [Drachennest: Different algorithms for converting binary to decimal floating-point numbers](https://github.com/abolz/Drachennest)
    ! [15] [Number Conversion Benchmark in C](https://github.com/ibireme/c_numconv_benchmark)
    ! [16] [fast_float number parsing library: 4x faster than strtod](https://github.com/fastfloat/fast_float)
    ! [17] [fast_double_parser: 4x faster than strtod](https://github.com/lemire/fast_double_parser)
    ! [18] [The LLVM Project: LibC Support](https://github.com/llvm/llvm-project/tree/main/libc/src/__support)
    ! [19] [Double Conversion: Efficient binary-decimal and decimal-binary conversion routines for IEEE doubles](https://github.com/google/double-conversion)
    ! [20] [fmt: A modern formatting library](https://github.com/fmtlib/fmt)
    !
!^ **TECHNICAL AND IMPLEMENTATION NOTES**:
    ! ***On the output to string***:
    ! 1) Three routines are available to convert a real (floating-point) number into a string.
    !    - "RealToString_DragonBox" is based on the Dragonbox binary-to-decimal conversion algorithm [1]
    !      and the reference implementation [11, 14, 20]
    !    - "RealToString_Ryu" is based on the Ryu binary-to-decimal conversion algorithm [2]
    !      and the reference implementation [12, 14]
    !    - "RealToString_Schubfach" is based on the Schubfach binary-to-decimal conversion algorithm [3]
    !      and the reference implementation [13, 14, 15]
    ! 2) All three binary-to-decimal conversion algorithms employed here produce the so-called shortest
    !    output representation that provide an error-free write-read cycle.  This means that any correct
    !    parsers (e.g. RealFromString routines) will read in the output string and return the original
    !    real (floating-point) number.
    ! 3) Although the DragonBox reference implementation provides several modes of rounding, only the
    !    round-to-nearest mode is implemented here (the other two algorithms also use this mode).
    ! 4) Although the Ryu reference implementation provides several conversion output formats (Shortest,
    !    Scientific, Fixed), only the shortest representation (as mentioned above) is implemented.
    !    Therefore, all three routines will produces the output string in a format similar to "G0" format
    !    specification in Fortran.
    ! 5) Actually, the RealToString routines have an optional "format" argument that we can use to specify
    !    whether to output the string in "General (G)" or "Scientific (ES)" format.  However, because they
    !    always produce the shortest output, no input argument to the routines is provided to specify
    !    the desired number of significant digits as typically done in Fortran format specifications.
    ! ***On the input from string***:
    ! 1) Four routines are available to convert a string into a real (floating-point) number.  All four
    !    routines utilize the so-call Clinger's fast-path algorithm [4].  Three of them (except "YY") employ
    !    the so-call Eisel-Lemire decimal-to-binary conversion algorithm [5, 9] but are based on different
    !    reference implementation.  When the Eisel-Lemire (or YY's fast-path) algorithm is NOT valid, three
    !    of the routines (except "LibC") use multi-precision (unsigned) integer arithmetic (i.e. BigUInt)
    !    whereas "LibC" employs the so-call Simple Decimal Conversion algorithm [10].
    !    - "RealFromString_FastFloat" is based on the reference implementation [16]
    !    - "RealFromString_LibC" is based on the reference implementation [18]
    !    - "RealFromString_YY" is based on the reference implementation [15, 19]
    !    - "RealFromString_Lemire" is based on the reference implementation [17, 19]
    ! 2) The RealFromString routines have an optional "parsing" argument that we can use to specify how
    !    the routines interpret the input string.
    ! 3) The "Parse_Fortran_String" routine is called when the optional "parsing" argument is not specified
    !    (i.e. the default option) or "FortNum (or 1)" value is supplied as the parsing argument.  The routine
    !    will interpret the input string as a valid Fortran real (floating point) number if it has one of
    !    the two following forms:
    !    <1> A number without exponent part -> [S]N[N...]
    !    <2> A number with exponent part    -> [S]N[N...]E[S]N[N...]
    !       where
    !       [ ] indicates an optional field
    !       S is a sign indicator (required if negative '-', optional if positive '+').
    !       N is a decimal digit (0 through 9). A decimal point (a period) may appear anywhere
    !           after the sign (but before the exponent).
    !       E is an exponent indicator (either 'e' or 'E')
    !    The valid number is similar to "Real" Fortran constant (literal) with some small differences.
    !    - A whole number without a decimal point (i.e. "Integer" constant) is considered valid.
    !    - The optional kind parameter (e.g. 10.0_DP) is not allowed here.
    !    Leading and/or trailing space(s) are allowed.  For example, "  1.23" and "1.23   " are considered
    !    valid.  However, no space is allowed inside the supposedly valid number.  For instance, "1 .2 3"
    !    is considered NOT valid. Therefore, this routine is not totally compatible with Fortran READ statement
    !    where spaces inside the valid number are allowed. However, this can easily be done by adding an
    !    optional 'Inside Space' flag that provide an interpretation of the spaces as 'zero' or 'ignored'.
    !    Then, the input will be pre-processed according to the flag.  Nonetheless, this routine neglects
    !    this optional input because it will make the routine much less efficient due to the fact that
    !    we will need to scan the whole string twice and we will also need to copy the input string into
    !    a buffer string and working with the buffer instead of directly handling the input string.
    ! 4) The "Parse_FPlus_String" routine is called when "FPlusNum (or 2)" value is supplied as the parsing
    !    argument.  The routine will parse a valid Fortran real (floating point) number with more relaxed
    !    rules than those used in "Parse_Fortran_Number" routine. The relaxed rules consider the following
    !    numbers as valid:
    !    - a number expressed in the scientific format can use 'd', 'D', 'q' and 'Q'
    !      in place of 'e' or 'E'.
    !    - a number with '+' or '-' after digits (e.g. 1.23-20 or 123+50) is considered to
    !      be expressed in a valid number expressed in the scientific format.
    !    - digits before any invalid character encountered are treated as a valid number
    !      and any characters after the first encounter (including the first invalid one)
    !      are neglected.  therefore, for example, a '12.56ax-300' is considered to be
    !      a valid number with a value of 12.56.
    ! 5) The "Parse_JSON_String" routine is called when "JsonNum (or 3)" value is supplied as the parsing
    !    argument.  The routine will parse a valid JSON floating point number where its differences from
    !    from Fortran number are as follows:
    !    - leading and trailing spaces are not allowed.
    !    - a plus sign as the first character is not allowed.
    !    - leading zero(s) is not allowed (if 0 is the first character, the second one
    !      must either be a period or an exponent indicator.)
    !    - a period must be followed by at least one digit.
    !
!^ **USAGE**:
    ! ***On the output to string***:
    ! => cStr = RealXP_ToString_DragonBox(Number, IsScientific)
    ! => cStr = RealXP_ToString_Ryu(Number, IsScientific)
    ! => cStr = RealXP_ToString_Schubfach(Number, IsScientific)
    !   where
    !   "cStr" is an "allocatable" character string representing the output string
    !   "Number" is a real number representing the floating point value
    !   "IsScientific" is a logical flag (optional argument) indicating whether
    !       the output string is in "General" or "Scientific" format.
    !       If present and true, the output string is in "Scientific" format.
    !       Otherwise, the output string is in "General" format.
    ! ***On the input from string***:
    ! => Number = RealXP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg)
    ! => Number = RealXP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg)
    ! => Number = RealXP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg)
    ! => Number = RealXP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg)
    !   where
    !   "Number" is a real number representing the floating point value if the input string is valid
    !   "cStr" is a character string representing the floating-point number string
    !   "ParseOpt" is an integer input flag (optional) indicating how to interpret the input string.
    !       The valid value is FortNum (1), FPlusNum (2) or JsonNum (3).
    !       If not specified and invalid, the routines will interpret the input string as a Fortran number.
    !   "ErrFlag" is a logical output flag (optional) indicating whether there is an error in parsing
    !       the input string.  True if the string represents a valid number.  False, otherwise.
    !   "ErrMsg" is an allocatable output character string (optional) that returns a message that describe
    !       the result of parsing the input string.
    ! **NOTE**: "XP" in the routine names shown above indicate the precision of the real number.
    !   The actual name will either be "RealSP_...", "RealDP_..." or "RealQP_..." for single-precision,
    !   double-precision and quadruple-precision number, respectively.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_SIntUtil
    USE ModBase_UIntUtil
    USE ModBase_UInt128
    USE ModBase_Tables_CharConv
    USE, INTRINSIC :: IEEE_ARITHMETIC

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! Real-To-String
    PUBLIC :: RealDP_ToString_DragonBox
    PUBLIC :: RealDP_ToString_Ryu
    PUBLIC :: RealDP_ToString_Schubfach
    ! Real-From-String
    PUBLIC :: RealDP_FromString_FastFloat
    PUBLIC :: RealDP_FromString_LibC
    PUBLIC :: RealDP_FromString_YY
    PUBLIC :: RealDP_FromString_Lemire

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! options for type of number to be parsed
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: FortNum  = 1     !! strict Fortran number
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: FPlusNum = 2     !! relaxed Fortran number
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: JsonNum  = 3     !! JSON number
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ parameters used to convert bit widths to whole decimal digits +++
    INTEGER(KIND=I8B), PARAMETER  :: LB2To10_M1 = 301029995664_I8B     ! LogBaseTenOfTwoTimesTenToThe12th
    INTEGER(KIND=I8B), PARAMETER  :: LB2To10_M2 = 1000000000000_I8B    ! TenToThe12th
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ Characteristics of IEEE-754 & related binary floating-point numbers +++
    INTEGER(KIND=I4B), PARAMETER  :: RealKind         = 8
    INTEGER(KIND=I4B), PARAMETER  :: BinaryPrecision  = 53
    INTEGER(KIND=I4B), PARAMETER  :: TotalBits        = 64
    INTEGER(KIND=I4B), PARAMETER  :: SignBits         = TotalBits - 1                                             ! 63
    INTEGER(KIND=I4B), PARAMETER  :: SignificandBits  = BinaryPrecision - 1                                       ! 52
    INTEGER(KIND=I4B), PARAMETER  :: ExponentBits     = TotalBits - BinaryPrecision                               ! 11
    INTEGER(KIND=I4B), PARAMETER  :: MaxExponent      = SHIFTL(1, ExponentBits) - 1                               ! 2047
    INTEGER(KIND=I4B), PARAMETER  :: ExponentBias     = SHIFTL(1, ExponentBits - 1) - 1                           ! 1023
    INTEGER(KIND=I4B), PARAMETER  :: DecimalPrecision = INT((SignificandBits * LB2To10_M1) / LB2To10_M2, KIND=I4B)      ! 15
    INTEGER(KIND=I4B), PARAMETER  :: DecimalRange     = INT(((ExponentBias - 1) * LB2To10_M1) / LB2To10_M2, KIND=I4B)   ! 307
    INTEGER(KIND=I4B), PARAMETER  :: MaxDecimalConversionDigits = 767
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ masking parameters +++
    INTEGER(KIND=I8B), PARAMETER  :: SigHidBitMask    = SHIFTL(1_I8B, SignificandBits)
    INTEGER(KIND=I8B), PARAMETER  :: SignificandMask  = SigHidBitMask - 1_I8B
    INTEGER(KIND=I8B), PARAMETER  :: SignMask         = SHIFTL(1_I8B, SignBits)
    INTEGER(KIND=I8B), PARAMETER  :: ExponentMask     = NOT(IOR(SignMask, SignificandMask))
    INTEGER(KIND=I8B), PARAMETER  :: ExpMantMask      = SignificandMask + ExponentMask        ! = NOT(SignMask)
    INTEGER(KIND=I8B), PARAMETER  :: QuietNaNMask     = SHIFTL(1_I8B, SignificandBits - 1)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Exceptional exponent value for NaN or Infinity
    INTEGER(KIND=I4B), PARAMETER  :: ExceptionalExponent = INT(Z'7FFFFFFF', KIND=I4B)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ maximum and minimum (positive) parameters +++
    ! note:  These parameters are stored in an integer data type that have the same number of bits to
    !        a real data type of the floating point number.
    !# minimum value of subnormal floating point number
    INTEGER(KIND=I8B), PARAMETER, PUBLIC  :: MinSubnormal = 1_I8B
    !# maximum value of subnormal floating point number
    INTEGER(KIND=I8B), PARAMETER, PUBLIC  :: MaxSubnormal = SHIFTL(1_I8B, SignificandBits) - 1_I8B
    !# minimum value of normal floating point number
    INTEGER(KIND=I8B), PARAMETER, PUBLIC  :: MinNormal    = SHIFTL(1_I8B, SignificandBits)
    !# maximum value of normal floating point number
    INTEGER(KIND=I8B), PARAMETER, PUBLIC  :: MaxNormal    = IOR(SHIFTL(INT(MaxExponent - 1, KIND=I8B), &
                                                                       SignificandBits), MaxSubnormal)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -------------------------------------------------------------------------
    ! -----   parameters for high-precision decimal conversion algorithm  -----
    ! -------------------------------------------------------------------------
    ! 600 is an arbitrary number of digits, but should be large enough for any practical number.
    ! Important note: a number of digits large enough to represent the smallest subnormal
    ! for double-precision number is about 1109 (= 342 + 767).
    INTEGER(KIND=I4B), PARAMETER  :: MAX_NUM_DIGITS = 600
    ! The maximum amount we can shift is the number of bits used in the Accumulator,
    ! minus the number of bits needed to represent the base (in this case 4).
    INTEGER(KIND=I4B), PARAMETER  :: MAX_SHIFT_AMOUNT = 4
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------------------
    ! -----   parameters for BigUInt of FastFloat algorithm  -----
    ! -----------------------------------------------------------
    ! the number of bits of 'Digit' of BigUInt.
    INTEGER(KIND=I4B), PARAMETER  :: DigitBits = 64
    ! the total number of bits of a BigUInt that needs to be at least the number of bits
    ! required to store the largest BigUInt, which is Log2(10**(MaxDigits + MaxExp10)), or
    ! Log2(10**(767 + 342))`, or ~3684 bits, so we round to 3712.
    INTEGER(KIND=I4B), PARAMETER  :: BigUIntBits = 3712
    ! the (fixed) capacity of a BigUInt
    INTEGER(KIND=I4B), PARAMETER  :: BigCapacity = BigUIntBits / DigitBits   ! = 58
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    INTEGER(KIND=I8B), PARAMETER  :: DivBase      = 10_I8B
    INTEGER(KIND=I8B), PARAMETER  :: MaxDivbyBase = 1844674407370955161_I8B
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ number parameters +++
    INTEGER(KIND=I8B), PARAMETER  :: TwoUInt   = 2_I8B
    INTEGER(KIND=I8B), PARAMETER  :: ThreeUInt = 3_I8B
    INTEGER(KIND=I8B), PARAMETER  :: FourUInt  = 4_I8B
    INTEGER(KIND=I8B), PARAMETER  :: FiveUInt  = 5_I8B
    INTEGER(KIND=I8B), PARAMETER  :: FortyUInt = 40_I8B
    INTEGER(KIND=I8B), PARAMETER  :: TenUInt   = 10_I8B
    INTEGER(KIND=I8B), PARAMETER  :: HundredUInt     = 100_I8B
    INTEGER(KIND=I8B), PARAMETER  :: TenThousandUInt = 10000_I8B
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ----------------------------------------------------
    ! -----   Simple-Decimal-Algorithm' parameters   -----
    ! ----------------------------------------------------
    ! The nth item in Powers_Of_Two represents the greatest power of two less than
    ! 10^n. This tells us how much we can safely shift without overshooting.
    INTEGER(KIND=I1B), PARAMETER  :: Powers_Of_Two(0:18) =  &
            [0, 3, 6, 9, 13, 16, 19, 23, 26, 29, 33, 36, 39, 43, 46, 49, 53, 56, 59]
    INTEGER(KIND=I4B), PARAMETER  :: Num_Powers_Of_Two = SIZE(Powers_Of_Two)                                      ! = 19
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! --------------------------------------------------
    ! -----   Eisel-Lemire-Algorithm' parameters   -----
    ! --------------------------------------------------
    INTEGER(KIND=I4B), PARAMETER  :: LowBits = TotalBits - SignificandBits - 3                                ! = 9
    ! The halfway constant is used to check if the bits that will be shifted away intially are all 1.
    INTEGER(KIND=I8B), PARAMETER  :: HalfWay = SHIFTL(1_I8B, LowBits) - 1_I8B                           ! = 511
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! --------------------------------------------------
    ! -----   Clinger-Algorithm' parameters   -----
    ! --------------------------------------------------
    INTEGER(KIND=I4B)             :: Idx
    REAL(KIND=DP),     PARAMETER  :: Powers_Of_Ten(0:22)  = [(10.0D0**Idx, Idx = 0, 22)]
    INTEGER(KIND=I4B), PARAMETER  :: Num_Exact_Pow10 = 22
    INTEGER(KIND=I4B), PARAMETER  :: Num_Mantissa_Digits = 15
    REAL(KIND=DP),     PARAMETER  :: Max_Exact_Integer = 9007199254740991.0_DP
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    INTEGER(KIND=I4B), PARAMETER  :: Exponent_UppBound =  309     ! = 308 + 1
    INTEGER(KIND=I4B), PARAMETER  :: Exponent_LowBound = -343     ! = (-324) - 19
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Dragonbox-Algorithm' parameters   -----
    ! -----------------------------------------------
    ! parameters for main routine
    INTEGER(KIND=I4B), PARAMETER  :: Kappa = 2
    INTEGER(KIND=I4B), PARAMETER  :: Big_Divisor = 10**(Kappa+1)              ! 1000
    INTEGER(KIND=I4B), PARAMETER  :: Small_Divisor = Big_Divisor / 10         ! 100
    INTEGER(KIND=I4B), PARAMETER  :: Half_Small_Divisor = Small_Divisor / 2   ! 50
    INTEGER(KIND=I4B), PARAMETER  :: Divisibility_Check_By_5_Threshold = 86
    INTEGER(KIND=I4B), PARAMETER  :: Case_Fc_Pm_Half_Lower_Threshold = -2
    ! parameters for short interval case
    INTEGER(KIND=I4B), PARAMETER  :: Case_Shorter_Interval_Left_Endpoint_Lower_Threshold = 2
    INTEGER(KIND=I4B), PARAMETER  :: Case_Shorter_Interval_Left_Endpoint_Upper_Threshold = 3
    INTEGER(KIND=I4B), PARAMETER  :: Shorter_Interval_Tie_Lower_Threshold = -77
    INTEGER(KIND=I4B), PARAMETER  :: Shorter_Interval_Tie_Upper_Threshold = -77
    ! parameters for Is_Divisible_By_Pow10 routine
    INTEGER(KIND=I4B), PARAMETER  :: Info_Shift_Amount = 20
    INTEGER(KIND=I4B), PARAMETER  :: OneShiftL = SHIFTL(1, Info_Shift_Amount)
    INTEGER(KIND=I4B), PARAMETER  :: Comparison_Mask = OneShiftL - 1
    INTEGER(KIND=I4B), PARAMETER  :: Magic_Number = OneShiftL/Small_Divisor + 1
    ! parameters for Divide_By_10_To_Kappa_Plus_1
    INTEGER(KIND=I8B), PARAMETER  :: DivM = 2361183241434822607_I8B
    INTEGER(KIND=I4B), PARAMETER  :: DivS = 7     ! 71 - 64
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Ryu-Algorithm' parameters   -----
    ! -----------------------------------------------
    INTEGER(KIND=I4B), PARAMETER  :: BitsPerPow5 = 128
    INTEGER(KIND=I4B), PARAMETER  :: MaxExp_ModInv5 = 27
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Schubfach-Algorithm' parameters   -----
    ! -----------------------------------------------
    INTEGER(KIND=I4B), PARAMETER  :: Pow10_Min_Exact_Exp = 0
    INTEGER(KIND=I4B), PARAMETER  :: Pow10_Max_Exact_Exp = 55
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   FastFloat-Algorithm' parameters   -----
    ! -----------------------------------------------
    ! Bias so we can get the real exponent with an invalid adjusted_mantissa
    INTEGER(KIND=I4B), PARAMETER  :: Invalid_AM_Bias = -INT(Z'00008000', KIND=I4B)
    INTEGER(KIND=I4B), PARAMETER  :: Mantissa_Explicit_Bits     = SignificandBits
    INTEGER(KIND=I4B), PARAMETER  :: Minimum_Exponent           = -ExponentBias
    INTEGER(KIND=I4B), PARAMETER  :: Infinite_Power             = MaxExponent
    INTEGER(KIND=I4B), PARAMETER  :: Sign_Index                 = SignBits
    INTEGER(KIND=I4B), PARAMETER  :: MantTotalBits              = 64
    ! see section 6 in 'Number Parsing at a Gigabyte per Second' paper for
    ! how the following two numbers can be obtained
    INTEGER(KIND=I4B), PARAMETER  :: Max_Exponent_Round_To_Even = 23
    INTEGER(KIND=I4B), PARAMETER  :: Min_Exponent_Round_To_Even = -4
    INTEGER(KIND=I4B), PARAMETER  :: Largest_Power_of_Ten       = Exponent_UppBound - 1
    INTEGER(KIND=I4B), PARAMETER  :: Smallest_Power_of_Ten      = Exponent_LowBound + 1
    INTEGER(KIND=I4B), PARAMETER  :: Max_Digits                 = MaxDecimalConversionDigits + 2
    INTEGER(KIND=I8B), PARAMETER  :: OneMant                    = 1_I8B
    INTEGER(KIND=I8B), PARAMETER  :: Max_Mantissa_Fast_Path     = SHIFTL(2_I8B, Mantissa_Explicit_Bits)
    INTEGER(KIND=I8B), PARAMETER  :: Exponent_Mask              = ExponentMask
    INTEGER(KIND=I8B), PARAMETER  :: Mantissa_Mask              = SignificandMask
    INTEGER(KIND=I8B), PARAMETER  :: Hidden_Bit_Mask            = SigHidBitMask
    INTEGER(KIND=I8B), PARAMETER  :: MaxMant                    = MAX_U64
    INTEGER(KIND=I8B), PARAMETER  :: NotOneMant                 = NOT(1_I8B)
    INTEGER(KIND=I8B), PARAMETER  :: NotSigHidBitMask           = NOT(SHIFTL(1_I8B, SignificandBits))
    INTEGER(KIND=I8B), PARAMETER  :: Powers_of_Ten_Uint64(0:19) = &
        [0_I8B, &
         10_I8B, &
         100_I8B, &
         1000_I8B, &
         10000_I8B, &
         100000_I8B, &
         1000000_I8B, &
         10000000_I8B, &
         100000000_I8B, &
         1000000000_I8B, &
         10000000000_I8B, &
         100000000000_I8B, &
         1000000000000_I8B, &
         10000000000000_I8B, &
         100000000000000_I8B, &
         1000000000000000_I8B, &
         10000000000000000_I8B, &
         100000000000000000_I8B, &
         1000000000000000000_I8B, &
         -8446744073709551616_I8B]
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ------------------------------------------------
    ! -----   YY/Lemire-Algorithm's parameters   -----
    ! ------------------------------------------------
    INTEGER(KIND=I8B), PARAMETER  :: MaxU64         = MAX_U64
    INTEGER(KIND=I8B), PARAMETER  :: BitMask        = SHIFTL(1_I8B, LowBits) - 1_I8B    ! = Halfway
    INTEGER(KIND=I8B), PARAMETER  :: BitMaskMinus1  = BitMask - 1_I8B
    INTEGER(KIND=I8B), PARAMETER  :: AddRound       = SHIFTL(1_I8B, ExponentBits - 1)
    INTEGER(KIND=I8B), PARAMETER  :: MaxUInt        = MaxU64
    INTEGER(KIND=I8B), PARAMETER  :: FpRawInf       = INT(Z'7FF0000000000000', KIND=I8B)            ! = ExponentMask
    INTEGER(KIND=I4B), PARAMETER  :: MaxExpBin      = 1024
    INTEGER(KIND=I4B), PARAMETER  :: MinExpBin      = -1021
    INTEGER(KIND=I4B), PARAMETER  :: UIntSafeDigits = 19
    INTEGER(KIND=I4B), PARAMETER  :: MaxDecDigits   = MaxDecimalConversionDigits + 1
    INTEGER(KIND=I8B), PARAMETER  :: MaxMantissa    = SHIFTL(1_I8B, BinaryPrecision)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !** DERIVED TYPE DEFINITIONS
    ! ----------------------------------------------------------------------------
    ! auxiliary string information
    ! ----------------------------------------------------------------------------
    TYPE StringAux
        INTEGER(KIND=I4B)     :: Start        ! starting position that exclude the leading spaces
        INTEGER(KIND=I4B)     :: SigCut       ! position after the string is truncated
                                              ! = zero if Truncated = False; non-zero if Truncated = True
        INTEGER(KIND=I4B)     :: Indices(4)   ! positions of characters representing the
                                              ! significand in the string
        LOGICAL               :: Truncated    ! flag indicating whether the computed decimal
                                              ! significand is based on a truncated string
    END TYPE StringAux
    ! ----------------------------------------------------------------------------
    ! -----   derived types for high-precision decimal conversion algorithm  -----
    ! ----------------------------------------------------------------------------
    TYPE HPDecimal
        INTEGER(KIND=I4B)     :: NumDigits = 0
        INTEGER(KIND=I4B)     :: DecimalPoint = 0
        LOGICAL               :: Truncated = FalseVal
        INTEGER(KIND=I1B)     :: Digits(0:MAX_NUM_DIGITS-1)
    CONTAINS
        PROCEDURE   :: ShouldRoundUp        => HPDec_Should_Round_Up
        PROCEDURE   :: GetNumNewDigits      => HPDec_Get_Num_New_Digits
        PROCEDURE   :: TrimTrailingZeroes   => HPDec_Trim_Trailing_Zeroes
        PROCEDURE   :: RightShift           => HPDec_Right_Shift
        PROCEDURE   :: LeftShift            => HPDec_Left_Shift
        PROCEDURE   :: Construct            => HPDec_Construct
        PROCEDURE   :: Shift                => HPDec_Shift
        PROCEDURE   :: RoundToUIntType      => HPDec_Round_To_UInt
    END TYPE HPDecimal
    ! ----------------------------------------------------------------------------
    ! -----   derived types for FastFloat algorithm                          -----
    ! ----------------------------------------------------------------------------
    ! a multi-precision (fixed capacity) unsigned integer where its representation are:
    ! - Base is 2**64.
    ! - Magnitude as array in little endian order.
    ! - The 'Length' first 'Digit' count as the number.
    ! ----------------------------------------------------------------------------
    TYPE BigUInt
        INTEGER(KIND=I8B)     :: Digit(0:BigCapacity-1)
        INTEGER(KIND=I4B)     :: Length = 0               ! number of digit currently stored
    CONTAINS
        PROCEDURE   :: IsEmpty      => BigUInt_IsEmpty
        PROCEDURE   :: IsNonZero    => BigUInt_IsNonZero
        PROCEDURE   :: Push         => BigUInt_Push
        PROCEDURE   :: Extend       => BigUInt_Extend
        PROCEDURE   :: Normalize    => BigUInt_Normalize
        PROCEDURE   :: FromU64      => BigUInt_From_U64
        PROCEDURE   :: Hi64         => BigUInt_Get_Hi64
        PROCEDURE   :: Compare      => BigUInt_Compare
        PROCEDURE   :: ShiftL       => BigUInt_ShiftL
        PROCEDURE   :: LeadZ        => BigUInt_LeadZ
        PROCEDURE   :: BitLen       => BigUInt_BitLen
        PROCEDURE   :: SmallMul     => BigUInt_SmallMul
        PROCEDURE   :: LongMul      => BigUInt_LongMul
        PROCEDURE   :: Add          => BigUInt_Add
        PROCEDURE   :: Pow2         => BigUInt_Pow2
        PROCEDURE   :: Pow5         => BigUInt_Pow5
        PROCEDURE   :: Pow10        => BigUInt_Pow10
    END TYPE BigUInt
    ! parsed number information
    TYPE Parsed_Number_Info
        INTEGER(KIND=I4B)     :: Exp              ! base-10 exponent
        INTEGER(KIND=I8B)     :: Sig              ! base-10 significand
        INTEGER(KIND=I4B)     :: IntegralStart    ! starting index of integral part of the significand
        INTEGER(KIND=I4B)     :: IntegralEnd      ! ending index of integral part of the significand
        INTEGER(KIND=I4B)     :: FractionStart    ! starting index of fractional part of the significand
        INTEGER(KIND=I4B)     :: FractionEnd      ! ending index of fractional part of the significand
    END TYPE
    ! ----------------------------------------------------------------------------
    ! binary floating-point representation in base 2
    ! --> ((-1)**S) * M * (2**E)
    ! ----------------------------------------------------------------------------
    TYPE BinRep
        INTEGER(KIND=I8B)     :: Significand  ! significand/mantissa (M)
        INTEGER(KIND=I4B)     :: Exponent     ! exponent (E); negative value is invalid
        LOGICAL               :: Negative     ! negative sign flag; true if the value is negative
    END TYPE BinRep
! ----------------------------------------------------------------------------

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE/GENERIC DEFINITIONS:
    ! interfaces to routines used by FastFloat algorithm
    ABSTRACT INTERFACE
        SUBROUTINE CB_Round(E, M, Min)
            IMPORT
            INTEGER(KIND=I4B), INTENT(INOUT)  :: E
            INTEGER(KIND=I8B), INTENT(INOUT)  :: M
            INTEGER(KIND=I4B), INTENT(IN)     :: Min
        END SUBROUTINE
        FUNCTION CB_Round_Nearest(IsOdd, IsHalfway, IsAbove) RESULT(Flag)
            IMPORT
            LOGICAL, INTENT(IN)    :: IsOdd, IsHalfway, IsAbove
            LOGICAL                :: Flag
        END FUNCTION
    END INTERFACE

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!------------------------------------------------------------------------------
!
!                       REAL64-TO-STRING MAIN ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION RealDP_ToString_DragonBox(Number, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
!^ To convert a double-precision floating-point value to a character (decimal) string
! using the DragonBox algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=DP),     INTENT(IN)   :: Number       !! number
    LOGICAL, OPTIONAL, INTENT(IN)   :: IsScientific
    !^ format flag
    ! true  if to write the given number in scientific format
    ! false if to write the given number in general format
    ! default is false
    CHARACTER(LEN=:), ALLOCATABLE   :: cStr         !! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: RawBin       ! raw IEEE binary floating point representation
    INTEGER(KIND=I8B)   :: SigRaw       ! raw (biased) significand in base 2
    INTEGER(KIND=I4B)   :: ExpRaw       ! raw (biased) exponent in base 2
    INTEGER(KIND=I8B)   :: SigBin       ! (unbiased) significand in base 2
    INTEGER(KIND=I4B)   :: ExpBin       ! (unbiased) exponent in base 2
    LOGICAL             :: Negative     ! sign flag (true if real value is negative)
    INTEGER(KIND=I8B)   :: SigDec       ! significand in base 10
    INTEGER(KIND=I4B)   :: ExpDec       ! exponent in base 10
    LOGICAL             :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    REAL(KIND=DP)       :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(RawBin, FloatVal)
    INTEGER(KIND=I4B)   :: wPos
    CHARACTER(LEN=48)   :: wStr         ! working string
    INTEGER(KIND=I4B)   :: wLen         ! length of string

!** FLOW

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ conversion of real value to its binary representation  +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number

! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= 0_I8B
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = INT(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits), KIND=I4B)

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ conversion from binary to decimal representation +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == 0_I8B)) THEN
! zero
        SigDec = 0_I8B
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF

    IF (ConvFlag) THEN
! perform binary-to-decimal conversion
        CALL Bin2Dec_DragonBox(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
    END IF

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ conversion from decimal representation to decimal string  +++++
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! write output
    IF (Negative) THEN
        wStr(1:1) = '-'
        wPos = 2
    ELSE
        wPos = 1
    END IF
    wLen = (wPos - 1) + Write_RealDP(SigDec, ExpDec, wStr(wPos:), IsScientific)

! set output
    cStr = wStr(1:wLen)

    RETURN

END FUNCTION RealDP_ToString_DragonBox

!******************************************************************************

FUNCTION RealDP_ToString_Ryu(Number, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
!^ To convert a double-precision floating-point value to a character (decimal) string
! using the Ryu algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=DP),     INTENT(IN)   :: Number       !! number
    LOGICAL, OPTIONAL, INTENT(IN)   :: IsScientific
    !^ format flag
    ! true  if to write the given number in scientific format
    ! false if to write the given number in general format
    ! default is false
    CHARACTER(LEN=:), ALLOCATABLE   :: cStr         !! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: RawBin       ! raw IEEE binary floating point representation
    INTEGER(KIND=I8B)   :: SigRaw       ! raw (biased) significand in base 2
    INTEGER(KIND=I4B)   :: ExpRaw       ! raw (biased) exponent in base 2
    INTEGER(KIND=I8B)   :: SigBin       ! (unbiased) significand in base 2
    INTEGER(KIND=I4B)   :: ExpBin       ! (unbiased) exponent in base 2
    LOGICAL             :: Negative     ! sign flag (true if real value is negative)
    INTEGER(KIND=I8B)   :: SigDec       ! significand in base 10
    INTEGER(KIND=I4B)   :: ExpDec       ! exponent in base 10
    LOGICAL             :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    REAL(KIND=DP)       :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(RawBin, FloatVal)
    INTEGER(KIND=I4B)   :: wPos
    CHARACTER(LEN=48)   :: wStr         ! working string
    INTEGER(KIND=I4B)   :: wLen         ! length of string

!** FLOW

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ conversion of real value to its binary representation  +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number

! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= 0_I8B
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = INT(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits), KIND=I4B)

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ conversion from binary to decimal representation +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == 0_I8B)) THEN
! zero
        SigDec = 0_I8B
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF

    IF (ConvFlag) THEN
! perform binary-to-decimal conversion
        CALL Bin2Dec_Ryu(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
    END IF

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ conversion from decimal representation to decimal string  +++++
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! write output
    IF (Negative) THEN
        wStr(1:1) = '-'
        wPos = 2
    ELSE
        wPos = 1
    END IF
    wLen = (wPos - 1) + Write_RealDP(SigDec, ExpDec, wStr(wPos:), IsScientific)

! set output
    cStr = wStr(1:wLen)

    RETURN

END FUNCTION RealDP_ToString_Ryu

!******************************************************************************

FUNCTION RealDP_ToString_Schubfach(Number, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
!^ To convert a double-precision floating-point value to a character (decimal) string
! using the Schubfach algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=DP),     INTENT(IN)   :: Number       !! number
    LOGICAL, OPTIONAL, INTENT(IN)   :: IsScientific
    !^ format flag
    ! true  if to write the given number in scientific format
    ! false if to write the given number in general format
    ! default is false
    CHARACTER(LEN=:), ALLOCATABLE   :: cStr         !! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: RawBin       ! raw IEEE binary floating point representation
    INTEGER(KIND=I8B)   :: SigRaw       ! raw (biased) significand in base 2
    INTEGER(KIND=I4B)   :: ExpRaw       ! raw (biased) exponent in base 2
    INTEGER(KIND=I8B)   :: SigBin       ! (unbiased) significand in base 2
    INTEGER(KIND=I4B)   :: ExpBin       ! (unbiased) exponent in base 2
    LOGICAL             :: Negative     ! sign flag (true if real value is negative)
    INTEGER(KIND=I8B)   :: SigDec       ! significand in base 10
    INTEGER(KIND=I4B)   :: ExpDec       ! exponent in base 10
    LOGICAL             :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    REAL(KIND=DP)       :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(RawBin, FloatVal)
    INTEGER(KIND=I4B)   :: wPos
    CHARACTER(LEN=48)   :: wStr         ! working string
    INTEGER(KIND=I4B)   :: wLen         ! length of string

!** FLOW

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ conversion of real value to its binary representation  +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number

! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= 0_I8B
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = INT(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits), KIND=I4B)

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ conversion from binary to decimal representation +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == 0_I8B)) THEN
! zero
        SigDec = 0_I8B
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF

    IF (ConvFlag) THEN
! perform binary-to-decimal conversion
        CALL Bin2Dec_Schubfach(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
    END IF

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ conversion from decimal representation to decimal string  +++++
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! write output
    IF (Negative) THEN
        wStr(1:1) = '-'
        wPos = 2
    ELSE
        wPos = 1
    END IF
    wLen = (wPos - 1) + Write_RealDP(SigDec, ExpDec, wStr(wPos:), IsScientific)

! set output
    cStr = wStr(1:wLen)

    RETURN

END FUNCTION RealDP_ToString_Schubfach

!------------------------------------------------------------------------------
!
!                       REAL64-FROM-STRING MAIN ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION RealDP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
!^ To convert a character (decimal) string to a double-precision floating-point value
! using the FastFloat algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*),                        INTENT(IN)    :: cStr      !! input string
    INTEGER(KIND=I4B),             OPTIONAL, INTENT(IN)    :: ParseOpt  !! parsing option
    LOGICAL,                       OPTIONAL, INTENT(OUT)   :: ErrFlag   !! error flag
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg    !! error message
    REAL(KIND=DP)                                          :: Number    !! floating point number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: SigDec       ! significand in base 10
    INTEGER(KIND=I4B)   :: ExpDec       ! exponent in base 10
    LOGICAL             :: Negative     ! sign flag (true if real value is negative)
    INTEGER(KIND=I8B)   :: SigBin       ! (unbiased) significand in base 2
    INTEGER(KIND=I4B)   :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux)     :: Aux
    INTEGER(KIND=I8B)   :: RawVal
    LOGICAL             :: Valid
    LOGICAL             :: SlowPath
    INTEGER(KIND=I4B)   :: ParseFormat
    REAL(KIND=DP)       :: FloatVal
    EQUIVALENCE(RawVal, FloatVal)

!** FLOW

! check and set optional input (parsing format)
    ParseFormat = FortNum
    IF (PRESENT(ParseOpt)) THEN
        IF ((ParseOpt >= 1).AND.(ParseOpt <= 3)) ParseFormat = ParseOpt
    END IF

! parse floating-point-number string
    SELECT CASE (ParseFormat)
    CASE (FortNum)
        Valid = Parse_Fortran_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (FPlusNum)
        Valid = Parse_FPlus_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (JsonNum)
        Valid = Parse_JSON_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    END SELECT
    IF (PRESENT(ErrFlag)) ErrFlag = .NOT.Valid

    IF (Valid) THEN
! ++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ perform decimal to binary conversion +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++
! set flag
        SlowPath = TrueVal

! If the exponent is too large and can't be represented in this size of
! float, return inf. These bounds are relatively loose, but are mostly
! serving as a first pass. Some close numbers getting through is okay.
        IF (ExpDec > Exponent_UppBound) THEN
! infinity
            SigBin = 0_I8B
            ExpBin = MaxExponent
            SlowPath = FalseVal
! If the exponent is too small even for a subnormal, return 0.
        ELSEIF (ExpDec < Exponent_LowBound) THEN
! zero
            SigBin = 0_I8B
            ExpBin = 0
            SlowPath = FalseVal
        ELSEIF (.NOT.Aux%Truncated) THEN
            IF (Dec2Bin_Clinger(SigDec, ExpDec, SigBin, ExpBin)) THEN
! clinger's fast path is valid
                SlowPath = FalseVal
            END IF
        END IF

! perform decimal to binary conversion using FastFloat algorithm if SlowPath is true
        IF (SlowPath) CALL Dec2Bin_FastFloat(SigDec, ExpDec, cStr, Aux%Truncated, Aux%Indices, SigBin, ExpBin)

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++ convert binary representation into real number +++
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
! construct raw binary representation of floating point number
! set sign bit
        IF (Negative) THEN
            RawVal = SignMask
        ELSE
            RawVal = 0_I8B
        END IF
! add exponent bits
        RawVal = IOR(RawVal, SHIFTL(ToUnsignedLong(ExpBin), SignificandBits))
! add (both implicit and explicit) significand bits
        RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
! convert raw binary representation to floating point number (little-endian order)
        Number = FloatVal
    ELSE
! handle special cases (infinity or NaN)
        Number = Handle_Invalid_String(cStr, Aux%Start, Negative)
    END IF

    RETURN

END FUNCTION RealDP_FromString_FastFloat

!******************************************************************************

FUNCTION RealDP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
!^ To convert a character (decimal) string to a double-precision floating-point value
! using the LibC algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*),                        INTENT(IN)    :: cStr      !! input string
    INTEGER(KIND=I4B),             OPTIONAL, INTENT(IN)    :: ParseOpt  !! parsing option
    LOGICAL,                       OPTIONAL, INTENT(OUT)   :: ErrFlag   !! error flag
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg    !! error message
    REAL(KIND=DP)                                          :: Number    !! floating point number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: SigDec       ! significand in base 10
    INTEGER(KIND=I4B)   :: ExpDec       ! exponent in base 10
    LOGICAL             :: Negative     ! sign flag (true if real value is negative)
    INTEGER(KIND=I8B)   :: SigBin       ! (unbiased) significand in base 2
    INTEGER(KIND=I4B)   :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux)     :: Aux
    INTEGER(KIND=I8B)   :: RawVal
    LOGICAL             :: Valid
    LOGICAL             :: SlowPath
    INTEGER(KIND=I4B)   :: ParseFormat
    REAL(KIND=DP)       :: FloatVal
    EQUIVALENCE(RawVal, FloatVal)

!** FLOW

! check and set optional input (parsing format)
    ParseFormat = FortNum
    IF (PRESENT(ParseOpt)) THEN
        IF ((ParseOpt >= 1).AND.(ParseOpt <= 3)) ParseFormat = ParseOpt
    END IF

! parse floating-point-number string
    SELECT CASE (ParseFormat)
    CASE (FortNum)
        Valid = Parse_Fortran_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (FPlusNum)
        Valid = Parse_FPlus_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (JsonNum)
        Valid = Parse_JSON_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    END SELECT
    IF (PRESENT(ErrFlag)) ErrFlag = .NOT.Valid

    IF (Valid) THEN
! ++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ perform decimal to binary conversion +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++
! set flag
        SlowPath = TrueVal

! If the exponent is too large and can't be represented in this size of
! float, return inf. These bounds are relatively loose, but are mostly
! serving as a first pass. Some close numbers getting through is okay.
        IF (ExpDec > Exponent_UppBound) THEN
! infinity
            SigBin = 0_I8B
            ExpBin = MaxExponent
            SlowPath = FalseVal
! If the exponent is too small even for a subnormal, return 0.
        ELSEIF (ExpDec < Exponent_LowBound) THEN
! zero
            SigBin = 0_I8B
            ExpBin = 0
            SlowPath = FalseVal
        ELSEIF (.NOT.Aux%Truncated) THEN
            IF (Dec2Bin_Clinger(SigDec, ExpDec, SigBin, ExpBin)) THEN
! clinger's fast path is valid
                SlowPath = FalseVal
            END IF
        END IF

! perform decimal to binary conversion using LibC algorithm if SlowPath is true
        IF (SlowPath) CALL Dec2Bin_LibC(SigDec, ExpDec, cStr, Aux%Start, Aux%Truncated, SigBin, ExpBin)

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
! +++ convert binary representation into real number +++
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
! construct raw binary representation of floating point number
! set sign bit
        IF (Negative) THEN
            RawVal = SignMask
        ELSE
            RawVal = 0_I8B
        END IF
! add exponent bits
        RawVal = IOR(RawVal, SHIFTL(ToUnsignedLong(ExpBin), SignificandBits))
! add (both implicit and explicit) significand bits
        RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
! convert raw binary representation to floating point number (little-endian order)
        Number = FloatVal
    ELSE
! handle special cases (infinity or NaN)
        Number = Handle_Invalid_String(cStr, Aux%Start, Negative)
    END IF

    RETURN

END FUNCTION RealDP_FromString_LibC

!******************************************************************************

FUNCTION RealDP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
!^ To convert a character (decimal) string to a double-precision floating-point value
! using the YY algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*),                        INTENT(IN)    :: cStr      !! input string
    INTEGER(KIND=I4B),             OPTIONAL, INTENT(IN)    :: ParseOpt  !! parsing option
    LOGICAL,                       OPTIONAL, INTENT(OUT)   :: ErrFlag   !! error flag
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg    !! error message
    REAL(KIND=DP)                                          :: Number    !! floating point number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: SigDec       ! significand in base 10
    INTEGER(KIND=I4B)   :: ExpDec       ! exponent in base 10
    LOGICAL             :: Negative     ! sign flag (true if real value is negative)
    INTEGER(KIND=I8B)   :: SigBin       ! (unbiased) significand in base 2
    INTEGER(KIND=I4B)   :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux)     :: Aux
    INTEGER(KIND=I8B)   :: RawVal
    LOGICAL             :: Valid
    LOGICAL             :: SlowPath
    INTEGER(KIND=I4B)   :: ParseFormat
    REAL(KIND=DP)       :: FloatVal
    EQUIVALENCE(RawVal, FloatVal)

!** FLOW

! check and set optional input (parsing format)
    ParseFormat = FortNum
    IF (PRESENT(ParseOpt)) THEN
        IF ((ParseOpt >= 1).AND.(ParseOpt <= 3)) ParseFormat = ParseOpt
    END IF

! parse floating-point-number string
    SELECT CASE (ParseFormat)
    CASE (FortNum)
        Valid = Parse_Fortran_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (FPlusNum)
        Valid = Parse_FPlus_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (JsonNum)
        Valid = Parse_JSON_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    END SELECT
    IF (PRESENT(ErrFlag)) ErrFlag = .NOT.Valid

    IF (Valid) THEN
! ++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ perform decimal to binary conversion +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++
! set flag
        SlowPath = TrueVal

! If the exponent is too large and can't be represented in this size of
! float, return inf. These bounds are relatively loose, but are mostly
! serving as a first pass. Some close numbers getting through is okay.
        IF (ExpDec > Exponent_UppBound) THEN
! infinity
            SigBin = 0_I8B
            ExpBin = MaxExponent
            SlowPath = FalseVal
! If the exponent is too small even for a subnormal, return 0.
        ELSEIF (ExpDec < Exponent_LowBound) THEN
! zero
            SigBin = 0_I8B
            ExpBin = 0
            SlowPath = FalseVal
        ELSEIF (.NOT.Aux%Truncated) THEN
            IF (Dec2Bin_Clinger(SigDec, ExpDec, SigBin, ExpBin)) THEN
! clinger's fast path is valid
                SlowPath = FalseVal
            END IF
        END IF

        IF (SlowPath) THEN
! +++ perform decimal to binary conversion using YY's algorithm +++
            RawVal = Dec2Bin_YY(SigDec, ExpDec, Negative, cStr, Aux)
        ELSE
! +++ construct raw binary representation of floating point number +++
! set sign bit
            IF (Negative) THEN
                RawVal = SignMask
            ELSE
                RawVal = 0_I8B
            END IF
! add exponent bits
            RawVal = IOR(RawVal, SHIFTL(ToUnsignedLong(ExpBin), SignificandBits))
! add (both implicit and explicit) significand bits
            RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
        END IF

! convert raw binary representation to floating point number (little-endian order)
        Number = FloatVal
    ELSE
! handle special cases (infinity or NaN)
        Number = Handle_Invalid_String(cStr, Aux%Start, Negative)
    END IF

    RETURN

END FUNCTION RealDP_FromString_YY

!******************************************************************************

FUNCTION RealDP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
!^ To convert a character (decimal) string to a double-precision floating-point value
! using the Lemire algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*),                        INTENT(IN)    :: cStr      !! input string
    INTEGER(KIND=I4B),             OPTIONAL, INTENT(IN)    :: ParseOpt  !! parsing option
    LOGICAL,                       OPTIONAL, INTENT(OUT)   :: ErrFlag   !! error flag
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg    !! error message
    REAL(KIND=DP)                                          :: Number    !! floating point number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: SigDec       ! significand in base 10
    INTEGER(KIND=I4B)   :: ExpDec       ! exponent in base 10
    LOGICAL             :: Negative     ! sign flag (true if real value is negative)
    INTEGER(KIND=I8B)   :: SigBin       ! (unbiased) significand in base 2
    INTEGER(KIND=I4B)   :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux)     :: Aux
    INTEGER(KIND=I8B)   :: RawVal
    LOGICAL             :: Valid
    LOGICAL             :: SlowPath
    INTEGER(KIND=I4B)   :: ParseFormat
    REAL(KIND=DP)       :: FloatVal
    EQUIVALENCE(RawVal, FloatVal)

!** FLOW

! check and set optional input (parsing format)
    ParseFormat = FortNum
    IF (PRESENT(ParseOpt)) THEN
        IF ((ParseOpt >= 1).AND.(ParseOpt <= 3)) ParseFormat = ParseOpt
    END IF

! parse floating-point-number string
    SELECT CASE (ParseFormat)
    CASE (FortNum)
        Valid = Parse_Fortran_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (FPlusNum)
        Valid = Parse_FPlus_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (JsonNum)
        Valid = Parse_JSON_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    END SELECT
    IF (PRESENT(ErrFlag)) ErrFlag = .NOT.Valid

    IF (Valid) THEN
! ++++++++++++++++++++++++++++++++++++++++++++++++
! +++++ perform decimal to binary conversion +++++
! ++++++++++++++++++++++++++++++++++++++++++++++++
! set flag
        SlowPath = TrueVal

! If the exponent is too large and can't be represented in this size of
! float, return inf. These bounds are relatively loose, but are mostly
! serving as a first pass. Some close numbers getting through is okay.
        IF (ExpDec > Exponent_UppBound) THEN
! infinity
            SigBin = 0_I8B
            ExpBin = MaxExponent
            SlowPath = FalseVal
! If the exponent is too small even for a subnormal, return 0.
        ELSEIF (ExpDec < Exponent_LowBound) THEN
! zero
            SigBin = 0_I8B
            ExpBin = 0
            SlowPath = FalseVal
        ELSEIF (.NOT.Aux%Truncated) THEN
            IF (Dec2Bin_Clinger(SigDec, ExpDec, SigBin, ExpBin)) THEN
! clinger's fast path is valid
                SlowPath = FalseVal
            END IF
        END IF

        IF (SlowPath) THEN
! +++ perform decimal to binary conversion using Lemire's algorithm +++
            RawVal = Dec2Bin_Lemire(SigDec, ExpDec, Negative, cStr, Aux)
        ELSE
! +++ construct raw binary representation of floating point number +++
! set sign bit
            IF (Negative) THEN
                RawVal = SignMask
            ELSE
                RawVal = 0_I8B
            END IF
! add exponent bits
            RawVal = IOR(RawVal, SHIFTL(ToUnsignedLong(ExpBin), SignificandBits))
! add (both implicit and explicit) significand bits
            RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
        END IF

! convert raw binary representation to floating point number (little-endian order)
        Number = FloatVal
    ELSE
! handle special cases (infinity or NaN)
        Number = Handle_Invalid_String(cStr, Aux%Start, Negative)
    END IF

    RETURN

END FUNCTION RealDP_FromString_Lemire

!------------------------------------------------------------------------------
!
!                           COMMON AND GENERIC ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION Parse_Eight_Digits_Unrolled(InVal) RESULT(OutVal)

!DIR$ ATTRIBUTES FORCEINLINE :: Parse_Eight_Digits_Unrolled

!** PURPOSE OF THIS SUBROUTINE:
! To parse eight digits immediately.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN) :: InVal
    INTEGER(KIND=I8B)             :: OutVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER  :: K1 = INT(Z'0F0F0F0F0F0F0F0F', KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: K2 = INT(Z'00FF00FF00FF00FF', KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: K3 = INT(Z'0000FFFF0000FFFF', KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: M1 = 2561_I8B
    INTEGER(KIND=I8B), PARAMETER  :: M2 = 6553601_I8B
    INTEGER(KIND=I8B), PARAMETER  :: M3 = 42949672960001_I8B
! parameters for alternative implementation
!    INTEGER(KIND=I8B), PARAMETER  :: Mask = INT(Z'000000FF000000FF', KIND=I8B)
!    INTEGER(KIND=I8B), PARAMETER  :: Mul1 = INT(Z'000F424000000064', KIND=I8B)   ! 100 + (1000000ULL << 32)
!    INTEGER(KIND=I8B), PARAMETER  :: Mul2 = INT(Z'0000271000000001', KIND=I8B)   ! 1 + (10000ULL << 32)
!    INTEGER(KIND=I8B), PARAMETER  :: Sub  = INT(Z'3030303030303030', KIND=I8B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    OutVal = SHIFTR(IAND(SHIFTR(IAND(SHIFTR(IAND(InVal, K1)*M1, 8), K2)*M2, 16), K3)*M3, 32)
! alternative implementation
!    OutVal = InVal - Sub
!    OutVal = (OutVal*10) + SHIFTR(OutVal, 8)    ! OutVal = (OutVal * 2561) >> 8
!    OutVal = SHIFTR(((IAND(OutVal, Mask)*Mul1) + (IAND(SHIFTR(OutVal, 16), Mask)*Mul2)), 32)

    RETURN

END FUNCTION Parse_Eight_Digits_Unrolled

!******************************************************************************

FUNCTION Is_Made_Of_Eight_Digits(InVal) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: Is_Made_Of_Eight_Digits

!** PURPOSE OF THIS SUBROUTINE:
! To check whether we can process eight digits immediately

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN) :: InVal
    LOGICAL                       :: Flag

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER  :: C1 = INT(Z'F0F0F0F0F0F0F0F0', KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: C2 = INT(Z'3333333333333333', KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: C3 = INT(Z'0606060606060606', KIND=I8B)
!    INTEGER(KIND=I8B), PARAMETER  :: K1 = INT(Z'4646464646464646', KIND=I8B)
!    INTEGER(KIND=I8B), PARAMETER  :: K2 = INT(Z'3030303030303030', KIND=I8B)
!    INTEGER(KIND=I8B), PARAMETER  :: K3 = INT(Z'8080808080808080', KIND=I8B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Flag = IOR(IAND(InVal, C1), SHIFTR(IAND((InVal + C3), C1), 4)) ==  C2
! alternative implementations
!    Flag = (IAND(InVal, C1) == K2).AND.(IAND(InVal + C3, C1) ==  K2)
!    Flag = (IAND(IOR((InVal + K1), (InVal - K2)), K3) == 0_I8B)
!    Flag = IAND(IAND(InVal, InVal + C3), C1) == C2

    RETURN

END FUNCTION Is_Made_Of_Eight_Digits

!******************************************************************************

FUNCTION Floor_Log10_ThreeQuartersPow2(E) RESULT(K)

!DIR$ ATTRIBUTES FORCEINLINE :: Floor_Log10_ThreeQuartersPow2

!** PURPOSE OF THIS SUBROUTINE:
! To compute K = FLOOR(LOG10((3/4)*(2**E))) where -2956395 <= E <= 2500325

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: E    ! base-2 exponent
    INTEGER(KIND=I4B)             :: K    ! base-10 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! Multiplier = FLOOR(LOG10(2) * 2**Shift)
! Addend     = FLOOR(LOG10(3/4) * 2**Shift)
    INTEGER(KIND=I4B), PARAMETER  :: Shift      = 41
    INTEGER(KIND=I8B), PARAMETER  :: Multiplier = 661971961083_I8B
    INTEGER(KIND=I8B), PARAMETER  :: Addend     = -274743187321_I8B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    K = INT(SHIFTA(INT(E, KIND=I8B)*Multiplier + Addend, Shift), KIND=I4B)

    RETURN

END FUNCTION Floor_Log10_ThreeQuartersPow2

!******************************************************************************

FUNCTION Floor_Log10_Pow2(E) RESULT(K)

!DIR$ ATTRIBUTES FORCEINLINE :: Floor_Log10_Pow2

!** PURPOSE OF THIS SUBROUTINE:
! To compute K = FLOOR(LOG10(2**E)) where -5456721 <= E <= 5456721

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: E    ! base-2 exponent
    INTEGER(KIND=I4B)             :: K    ! base-10 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! Multiplier = FLOOR(LOG10(2) * 2**Shift)
    INTEGER(KIND=I4B), PARAMETER  :: Shift      = 41
    INTEGER(KIND=I8B), PARAMETER  :: Multiplier = 661971961083_I8B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    K = INT(SHIFTA(INT(E, KIND=I8B)*Multiplier, Shift), KIND=I4B)

    RETURN

END FUNCTION Floor_Log10_Pow2

!******************************************************************************

FUNCTION Floor_Log2_Pow10(K) RESULT(E)

!DIR$ ATTRIBUTES FORCEINLINE :: Floor_Log2_Pow10

!** PURPOSE OF THIS SUBROUTINE:
! To compute E = FLOOR(LOG2(10**K)) where -1838394 <= K <= 1838394

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: K    ! base-10 exponent
    INTEGER(KIND=I4B)             :: E    ! base-2 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! Multiplier = FLOOR(LOG2(10) * 2**Shift)
    INTEGER(KIND=I4B), PARAMETER  :: Shift      = 38
    INTEGER(KIND=I8B), PARAMETER  :: Multiplier = 913124641741_I8B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    E = INT(SHIFTA(INT(K, KIND=I8B)*Multiplier, Shift), KIND=I4B)

    RETURN

END FUNCTION Floor_Log2_Pow10

!******************************************************************************

FUNCTION Floor_Log2_Pow5(P) RESULT(E)

!DIR$ ATTRIBUTES FORCEINLINE :: Floor_Log2_Pow5

!** PURPOSE OF THIS SUBROUTINE:
! To compute E = FLOOR(LOG2(5**P)) where -32768 <= Exp <= 32768

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: P    ! base-5 exponent
    INTEGER(KIND=I4B)             :: E    ! base-2 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! Multiplier = FLOOR(LOG2(5) * 2**Shift)
    INTEGER(KIND=I4B), PARAMETER  :: Shift      = 46
    INTEGER(KIND=I8B), PARAMETER  :: Multiplier = 163391164108059_I8B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    E = INT(SHIFTA(INT(P, KIND=I8B)*Multiplier, Shift), KIND=I4B)

    RETURN

END FUNCTION Floor_Log2_Pow5

!******************************************************************************

FUNCTION Floor_Log10_Pow5(E) RESULT(K)

!DIR$ ATTRIBUTES FORCEINLINE :: Floor_Log10_Pow5

!** PURPOSE OF THIS SUBROUTINE:
! To compute K = Floor(Log10(5**E))

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)    :: E    ! ! 0 <= Exp <= 2**15
    INTEGER(KIND=I4B)                :: K

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: Shift      = 48
    INTEGER(KIND=I8B), PARAMETER  :: Multiplier = 196742565691928_I8B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

! The first value this approximation fails for is 5^2621 which is just greater than 10^1832.
    K = INT(SHIFTR(INT(E, KIND=I8B)*Multiplier, Shift), KIND=I4B)

    RETURN

END FUNCTION Floor_Log10_Pow5

!******************************************************************************

FUNCTION Ceiling_Log2_Pow5(P) RESULT(E)

!DIR$ ATTRIBUTES FORCEINLINE :: Ceiling_Log2_Pow5

!** PURPOSE OF THIS SUBROUTINE:
! To compute E = CEILING(LOG2(5**P))

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: P    ! base-5 exponent
    INTEGER(KIND=I4B)             :: E    ! base-2 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    E = Floor_Log2_Pow5(P) + 1

    RETURN

END FUNCTION Ceiling_Log2_Pow5

!******************************************************************************

FUNCTION Pow5Bits(Exp) RESULT(Pow5)

!DIR$ ATTRIBUTES FORCEINLINE :: Pow5Bits

!** PURPOSE OF THIS SUBROUTINE:
! To compute Pow5 = Ceiling(Log2(5**Exp)).

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: Exp       ! 0 <= Exp <= 32768
    INTEGER(KIND=I4B)             :: Pow5

!** SUBROUTINE ARGUMENT DECLARATIONS:
! Multiplier = FLOOR(LOG2(5) * 2**Shift)
    INTEGER(KIND=I4B), PARAMETER  :: Shift      = 46
    INTEGER(KIND=I8B), PARAMETER  :: Multiplier = 163391164108059_I8B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

! note: this is similar to 'Ceiling_Log2_Pow5' but only for positive Exp
    Pow5 = INT(SHIFTR(Exp*Multiplier, Shift) + 1_I8B, KIND=I4B)

    RETURN

END FUNCTION Pow5Bits

!**************************************************************************

SUBROUTINE MultiplyBasic(X, XLen, Y, YLen, Z)

!DIR$ ATTRIBUTES INLINE :: MultiplyBasic

!** PURPOSE OF THIS SUBROUTINE:
! To multiply two magnitude arrays and return the result using grade-school algorithm

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)     :: XLen             ! The length of the first array
    INTEGER(KIND=I8B), INTENT(IN)     :: X(0:XLen-1)      ! The first magnitude array
    INTEGER(KIND=I4B), INTENT(IN)     :: YLen             ! The length of the second array
    INTEGER(KIND=I8B), INTENT(IN)     :: Y(0:YLen-1)      ! The second magnitude array
    INTEGER(KIND=I8B), INTENT(OUT)    :: Z(0:XLen+YLen-1) ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER  :: MinI64   = INT(Z'8000000000000000', KIND=I8B)   ! min signed 64-bit

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry64, ProductHi, ProductLo, Sum
    INTEGER(KIND=I4B)     :: I, J

!** FLOW

    Carry64 = 0_I8B
    DO J = 0, YLen-1
        CALL UMul128(X(0), Y(J), ProductHi, ProductLo)
        Z(J) = ProductLo + Carry64
        IF (IEOR(Z(J), MinI64) < IEOR(ProductLo, MinI64)) THEN
            Carry64 = ProductHi + 1_I8B
        ELSE
            Carry64 = ProductHi
        END IF
    END DO
    Z(YLen) = Carry64
    DO I = 1, XLen-1
        Carry64 = 0_I8B
        DO J = 0, YLen-1
            CALL UMul128(X(I), Y(J), ProductHi, ProductLo)
            Sum = ProductLo + Z(I+J)
            IF (IEOR(Sum, MinI64) < IEOR(ProductLo, MinI64)) ProductHi = ProductHi + 1_I8B
            Z(I+J) = Sum + Carry64
            IF (IEOR(Z(I+J), MinI64) < IEOR(Sum, MinI64)) THEN
                Carry64 = ProductHi + 1_I8B
            ELSE
                Carry64 = ProductHi
            END IF
        END DO
        Z(I+YLen) = Carry64
    END DO

    RETURN

END SUBROUTINE MultiplyBasic

!******************************************************************************

SUBROUTINE ShiftRight(X, ShiftPos)

!DIR$ ATTRIBUTES INLINE :: ShiftRight

!** PURPOSE OF THIS SUBROUTINE:
! To shift the input right by the specified amount

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(INOUT)  :: X(0:)
    INTEGER(KIND=I4B), INTENT(IN)     :: ShiftPos

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: LargeShift, SmallShift

!** FLOW

    IF (ShiftPos == 0) RETURN
    LargeShift = SHIFTR(ShiftPos, 6)
    IF (LargeShift >= SIZE(X)) THEN
        X = 0_I8B
    ELSE
        SmallShift = IAND(ShiftPos, 63)
        IF (LargeShift > 0) CALL ShiftLarge(X, LargeShift)
        IF (SmallShift > 0) CALL ShiftSmall(X, SmallShift)
    END IF

    RETURN
    CONTAINS

    SUBROUTINE ShiftSmall(X, Shift)

!DIR$ ATTRIBUTES FORCEINLINE :: ShiftSmall

!** PURPOSE OF THIS SUBROUTINE:
! To shift the input right by the given amount (less than 64).

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(INOUT)  :: X(0:)
        INTEGER(KIND=I4B), INTENT(IN)     :: Shift

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: I, XLen
        INTEGER(KIND=I8B)     :: Nxt

!** FLOW

        XLen = SIZE(X)
	    Nxt = X(0)
        DO I = 0, XLen-2
            X(I) = IOR(SHIFTR(Nxt, Shift), SHIFTL(X(I+1), 64-Shift))
            Nxt = X(I+1)
        END DO
        X(XLen-1) = SHIFTR(X(XLen-1), Shift)

        RETURN

    END SUBROUTINE ShiftSmall

!******************************************************************************

    SUBROUTINE ShiftLarge(X, Shift)

!DIR$ ATTRIBUTES FORCEINLINE :: ShiftLarge

!** PURPOSE OF THIS SUBROUTINE:
! To shift the input right by 64*shift, i.e. moves each
! element of the array shift positions to the right.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(INOUT)  :: X(0:)
        INTEGER(KIND=I4B), INTENT(IN)     :: Shift

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: Index, XLen

!** FLOW

        XLen = SIZE(X)
	    DO Index = 0, XLen-Shift-1
            X(Index) = X(Shift+Index)
        END DO
        X(XLen-Shift:) = 0_I8B

        RETURN

    END SUBROUTINE ShiftLarge

!******************************************************************************

END SUBROUTINE ShiftRight

!******************************************************************************

SUBROUTINE Multiply_N_ShiftRight(X, XLen, Y, YLen, Shift, Z)

!DIR$ ATTRIBUTES INLINE :: Multiply_N_ShiftRight

!** PURPOSE OF THIS SUBROUTINE:
! To perform multiplication and then right shift

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)     :: XLen             ! The length of the first array
    INTEGER(KIND=I8B), INTENT(IN)     :: X(0:XLen-1)      ! The first magnitude array
    INTEGER(KIND=I4B), INTENT(IN)     :: YLen             ! The length of the second array
    INTEGER(KIND=I8B), INTENT(IN)     :: Y(0:YLen-1)      ! The second magnitude array
    INTEGER(KIND=I4B), INTENT(IN)     :: Shift            ! shift position of bits in the result array
    INTEGER(KIND=I8B), INTENT(OUT)    :: Z(0:XLen+YLen-1) ! The result array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    CALL MultiplyBasic(X, XLen, Y, YLen, Z)
    CALL ShiftRight(Z, Shift)

    RETURN

END SUBROUTINE Multiply_N_ShiftRight

!******************************************************************************

SUBROUTINE Increment_Value(X)

!DIR$ ATTRIBUTES INLINE :: Increment_Value

!** PURPOSE OF THIS SUBROUTINE:
! To increase value of the input by 1

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(INOUT) :: X(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: XLen, I
    INTEGER(KIND=I8B)     :: Sum, Carry

!** FLOW

    XLen = SIZE(X)
    Sum = X(0) + 1_I8B
	Carry = SHIFTR(IOR(IAND(X(0), 1_I8B), IAND(IOR(X(0), 1_I8B), NOT(Sum))), 63)
	X(0) = Sum
	IF (Carry /= 0_I8B) THEN
		I = 1_I4B
        DO
            X(I) = X(I) + 1_I8B
            IF (.NOT.((I < XLen).AND.(X(I) == 0))) EXIT
            I = I + 1_I4B
        END DO
	END IF

    RETURN

END SUBROUTINE Increment_Value

!******************************************************************************

FUNCTION Get_Pow10_128Bits(K) RESULT(Pow10)

!DIR$ ATTRIBUTES FORCEINLINE :: Get_Pow10_128Bits

!** PURPOSE OF THIS SUBROUTINE:
! To get the 128-bit approximation of power of ten
! -> Pow10 = 10**K

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: K        ! the power
    TYPE(UInt128)                 :: Pow10    ! the power of ten

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Pow10Cache(0:1)  ! the power of ten in little-endian order

!** FLOW

    IF ((K >= Pow10_256_Small_MinExp).AND.(K <= Pow10_256_Small_MaxExp)) THEN
! get only the upper 128 bits
        Pow10Cache(0:1) = Pow10_256_Small_Table(2:3, K)
    ELSE
        CALL Compute_Pow10_128Bits(K, Pow10Cache)
    END IF
    Pow10 = UInt128(Pow10Cache(1), Pow10Cache(0))

    RETURN

END FUNCTION Get_Pow10_128Bits

!******************************************************************************

SUBROUTINE Compute_Pow10_128Bits(K, Pow10)

!DIR$ ATTRIBUTES INLINE :: Compute_Pow10_128Bits

!** PURPOSE OF THIS SUBROUTINE:
! To compute the 128-bit approximation of power of ten
! -> Pow10 = 10**K

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)     :: K            ! the power
    INTEGER(KIND=I8B), INTENT(OUT)    :: Pow10(0:1)   ! the power of ten in little-endian order

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Pow10_Index, KBase, Offset, Alpha
    INTEGER(KIND=I8B)     :: Pow10_Cache(0:1), Pow5(0:1)
    INTEGER(KIND=I8B)     :: Pow10_256(0:3)

!** FLOW

! compute essential indices
    Pow10_Index = (K - Pow10_256_Compressed_MinExp) / Pow5_128_Size
    KBase = Pow10_Index * Pow5_128_Size + Pow10_256_Compressed_MinExp
    Offset = K - KBase

! get base cache (only upper 128 bits) where table data is stored in little-endian
! order (i.e. the least significant byte is 0 and the most significant byte is 3)
    Pow10_Cache(0:1) = Pow10_256_Compressed_Table(2:3, Pow10_Index)
    IF (Offset == 0) THEN
        Pow10 = Pow10_Cache
        RETURN
    END IF

! compute the required amount of bit-shift where Alpha should be in the range (0, 128)
    Alpha = Floor_Log2_Pow10(KBase + Offset) - Floor_Log2_Pow10(KBase) - Offset

! compute the approximation for the specified power K
    Pow5 = Pow5_128_Table(:, Offset)
    CALL Multiply_N_ShiftRight(Pow10_Cache, 2, Pow5, 2, Alpha, Pow10_256)
    Pow10(0:1) = Pow10_256(0:1)
    CALL Increment_Value(Pow10)

    RETURN

END SUBROUTINE Compute_Pow10_128Bits

!******************************************************************************

FUNCTION Handle_Invalid_String(cStr, Start, Negative) RESULT(RealNum)

!** PURPOSE OF THIS SUBROUTINE:
! To handle invalid input string.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*),   INTENT(IN)   :: cStr
    INTEGER(KIND=I4B),  INTENT(IN)   :: Start
    LOGICAL,            INTENT(IN)   :: Negative
    REAL(KIND=DP)                    :: RealNum

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: Finish
    INTEGER(KIND=I4B)    :: Ptr, Q

!** FLOW

! Could not parse a decimal floating-point number.  Start has been
! advanced over any leading spaces.
    Ptr = Start
    Finish = LEN_TRIM(cStr)
    IF (Start > Finish) THEN
! empty string
        RealNum = IEEE_VALUE(0.0_DP, IEEE_QUIET_NAN)
    ELSEIF (Finish == Ptr + 2) THEN
        IF ((ToUpper(cStr(Ptr:Ptr)) == 'N').AND.(ToUpper(cStr(Ptr+1:Ptr+1)) == 'A').AND. &
            (ToUpper(cStr(Ptr+2:Ptr+2)) == 'N')) THEN
! Exact NAN
            RealNum = IEEE_VALUE(0.0_DP, IEEE_QUIET_NAN)
        ELSE
! Invalid NAN
            RealNum = IEEE_VALUE(0.0_DP, IEEE_SIGNALING_NAN)
        END IF
    ELSE
! Try to parse Inf, maybe with a sign
        Q = Ptr
        IF (Q <= Finish) THEN
            IF (Is_Character_Sign(cStr(Q:Q))) Q = Q + 1
        END IF
        IF (Finish == Q + 2) THEN
            IF ((ToUpper(cStr(Q:Q)) == 'I').AND.(ToUpper(cStr(Q+1:Q+1)) == 'N').AND. &
                (ToUpper(cStr(Q+2:Q+2)) == 'F')) THEN
                IF (Negative) THEN
                    RealNum = IEEE_VALUE(1.0_DP, IEEE_NEGATIVE_INF)
                ELSE
                    RealNum = IEEE_VALUE(1.0_DP, IEEE_POSITIVE_INF)
                END IF
            ELSE
! Invalid NAN
                RealNum = IEEE_VALUE(0.0_DP, IEEE_SIGNALING_NAN)
            END IF
        ELSEIF (Finish == Q + 7) THEN
            IF ((ToUpper(cStr(Q:Q)) == 'I').AND.(ToUpper(cStr(Q+1:Q+1)) == 'N').AND. &
                (ToUpper(cStr(Q+2:Q+2)) == 'F').AND.(ToUpper(cStr(Q+3:Q+3)) == 'I').AND. &
                (ToUpper(cStr(Q+4:Q+4)) == 'N').AND.(ToUpper(cStr(Q+5:Q+5)) == 'I').AND. &
                (ToUpper(cStr(Q+6:Q+6)) == 'T').AND.(ToUpper(cStr(Q+7:Q+7)) == 'Y')) THEN
                IF (Negative) THEN
                    RealNum = IEEE_VALUE(1.0_DP, IEEE_NEGATIVE_INF)
                ELSE
                    RealNum = IEEE_VALUE(1.0_DP, IEEE_POSITIVE_INF)
                END IF
            ELSE
! Invalid NAN
                RealNum = IEEE_VALUE(0.0_DP, IEEE_SIGNALING_NAN)
            END IF
        ELSE
! Invalid input
            RealNum = IEEE_VALUE(0.0_DP, IEEE_SIGNALING_NAN)
        END IF
    END IF

    RETURN

CONTAINS

    FUNCTION Is_Character_Sign(Chr) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the given character is a 'sign' character

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=1),   INTENT(IN)  :: Chr          ! character
        LOGICAL                         :: Flag         ! true if the character is valid

    !** SUBROUTINE PARAMETER DECLARATIONS:
        CHARACTER(LEN=*), PARAMETER  :: SET_SIGNS = '+-'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW:

        Flag = (INDEX(SET_SIGNS, Chr) /= 0)

        RETURN

    END FUNCTION Is_Character_Sign

!**************************************************************************

    FUNCTION ToUpper(ChrIn) RESULT(ChrOut)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To change case of the input character to upper case if applicable

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=1),  INTENT(IN)  :: ChrIn
        CHARACTER(LEN=1)               :: ChrOut

    !** SUBROUTINE PARAMETER DECLARATIONS:
        CHARACTER(LEN=*), PARAMETER  :: SET_ALPHABETS_LOWER = 'abcdefghijklmnopqrstuvwxyz'
        CHARACTER(LEN=*), PARAMETER  :: SET_ALPHABETS_UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: ID

    !** FLOW:

        ID = INDEX(SET_ALPHABETS_LOWER, ChrIn)
        IF (ID > 0) THEN
            ChrOut = SET_ALPHABETS_UPPER(ID:ID)
        ELSE
            ChrOut = ChrIn
        END IF

        RETURN

    END FUNCTION ToUpper

!**************************************************************************

END FUNCTION Handle_Invalid_String

!******************************************************************************

!------------------------------------------------------------------------------
!
!           (RAW) FLOATING-POINT BINARY REPRESENTATION ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION RawFP_BiasedExponent(RawVal) RESULT(BiasedExponent)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_BiasedExponent

!** PURPOSE OF THIS SUBROUTINE:
! To determine the biased exponent of the floating point value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    INTEGER(KIND=I4B)               :: BiasedExponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    BiasedExponent = INT(SHIFTR(IAND(RawVal, ExponentMask), SignificandBits), KIND=I4B)

    RETURN

END FUNCTION RawFP_BiasedExponent

!******************************************************************************

FUNCTION RawFP_UnbiasedExponent(RawVal) RESULT(UnbiasedExponent)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_UnbiasedExponent

!** PURPOSE OF THIS SUBROUTINE:
! To determine the unbiased exponent of the floating point value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    INTEGER(KIND=I4B)               :: UnbiasedExponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: BiasedExponent

!** FLOW

    BiasedExponent   = RawFP_BiasedExponent(RawVal)
    UnbiasedExponent = BiasedExponent - ExponentBias
    IF (BiasedExponent == 0) UnbiasedExponent = UnbiasedExponent + 1

    RETURN

END FUNCTION RawFP_UnbiasedExponent

!******************************************************************************

FUNCTION RawFP_Significand(RawVal) RESULT(Significand)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_Significand

!** PURPOSE OF THIS SUBROUTINE:
! To determine the significand of the floating point value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    INTEGER(KIND=I8B)               :: Significand

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Significand = IAND(RawVal, SignificandMask)

    RETURN

END FUNCTION RawFP_Significand

!******************************************************************************

FUNCTION RawFP_Fraction(RawVal) RESULT(Fraction)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_Fraction

!** PURPOSE OF THIS SUBROUTINE:
! To determine the fraction part of the floating point value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    INTEGER(KIND=I8B)               :: Fraction

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Fraction = RawFP_Significand(RawVal)
    IF (RawFP_BiasedExponent(RawVal) > 0) Fraction = IOR(Fraction, SigHidBitMask)

    RETURN

END FUNCTION RawFP_Fraction

!******************************************************************************

FUNCTION RawFP_IsZero(RawVal) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_IsZero

!** PURPOSE OF THIS SUBROUTINE:
! To determine whether the input value is zero

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

! Remove sign bit by shift
    Flag = SHIFTL(RawVal, 1) == 0_I8B

    RETURN

END FUNCTION RawFP_IsZero

!******************************************************************************

FUNCTION RawFP_IsNaN(RawVal) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_IsNaN

!** PURPOSE OF THIS SUBROUTINE:
! To determine whether the input value is not a number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Flag = (RawFP_BiasedExponent(RawVal) == MaxExponent).AND. &
           (RawFP_Significand(RawVal) /= 0_I8B)

    RETURN

END FUNCTION RawFP_IsNaN

!******************************************************************************

FUNCTION RawFP_IsQuietNaN(RawVal) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_IsQuietNaN

!** PURPOSE OF THIS SUBROUTINE:
! To determine whether the input value is a quiet NaN

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Flag = IAND(RawVal, ExpMantMask) == IOR(ExponentMask, QuietNaNMask)

    RETURN

END FUNCTION RawFP_IsQuietNaN

!******************************************************************************

FUNCTION RawFP_IsInfinite(RawVal) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_IsInfinite

!** PURPOSE OF THIS SUBROUTINE:
! To determine whether the input value is infinite

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Flag = (RawFP_BiasedExponent(RawVal) == MaxExponent).AND. &
           (RawFP_Significand(RawVal) == 0_I8B)

    RETURN

END FUNCTION RawFP_IsInfinite

!******************************************************************************

FUNCTION RawFP_IsInfOrNaN(RawVal) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_IsInfOrNaN

!** PURPOSE OF THIS SUBROUTINE:
! To determine whether the input value is infinite or NaN

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Flag = IAND(RawVal, ExponentMask) == ExponentMask

    RETURN

END FUNCTION RawFP_IsInfOrNaN

!******************************************************************************

FUNCTION RawFP_IsMaximalFiniteMagnitude(RawVal) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_IsMaximalFiniteMagnitude

!** PURPOSE OF THIS SUBROUTINE:
! To determine whether the input value is equal to the maximal finite magnitude

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Flag = (RawFP_BiasedExponent(RawVal) == (MaxExponent - 1)).AND. &
           (RawFP_Significand(RawVal) == SignificandMask)

    RETURN

END FUNCTION RawFP_IsMaximalFiniteMagnitude

!******************************************************************************

FUNCTION RawFP_IsNegative(RawVal) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_IsNegative

!** PURPOSE OF THIS SUBROUTINE:
! To determine whether the input value is negative

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Flag = IAND(RawVal, SignMask) /= 0_I8B

    RETURN

END FUNCTION RawFP_IsNegative

!******************************************************************************

FUNCTION RawFP_Negate(InRaw) RESULT(OutRaw)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_Negate

!** PURPOSE OF THIS SUBROUTINE:
! To negate the input value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: InRaw
    INTEGER(KIND=I8B)               :: OutRaw

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    OutRaw = IEOR(InRaw, SignMask)

    RETURN

END FUNCTION RawFP_Negate

!******************************************************************************

FUNCTION RawFP_NeighborLow(InRaw) RESULT(OutRaw)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_NeighborLow

!** PURPOSE OF THIS SUBROUTINE:
! To determine the nearest floating point value that is smaller than the input value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: InRaw
    INTEGER(KIND=I8B)               :: OutRaw

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    OutRaw = InRaw - 1_I8B

    RETURN

END FUNCTION RawFP_NeighborLow

!******************************************************************************

FUNCTION RawFP_NeighborHigh(InRaw) RESULT(OutRaw)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_NeighborHigh

!** PURPOSE OF THIS SUBROUTINE:
! To determine the nearest floating point value that is greater than the input value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: InRaw
    INTEGER(KIND=I8B)               :: OutRaw

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    OutRaw = InRaw + 1_I8B

    RETURN

END FUNCTION RawFP_NeighborHigh

!******************************************************************************

FUNCTION RawFP_Construct(FpBin) RESULT(RawVal)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_Construct

!** PURPOSE OF THIS SUBROUTINE:
! To construct a raw binary floating point number based on
! its three parts

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BinRep), INTENT(IN)    :: FpBin
    INTEGER(KIND=I8B)           :: RawVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

! set sign bit
    IF (FpBin%Negative) THEN
        RawVal = SignMask
    ELSE
        RawVal = 0_I8B
    END IF
! add exponent bits
    RawVal = IOR(RawVal, SHIFTL(ToUnsignedLong(FpBin%Exponent), SignificandBits))
! add (both implicit and explicit) significand bits
    RawVal = IOR(RawVal, IAND(FpBin%Significand, SignificandMask))

    RETURN

END FUNCTION RawFP_Construct

!******************************************************************************

FUNCTION RawFP_Decompose(RawVal) RESULT(FpBin)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_Decompose

!** PURPOSE OF THIS SUBROUTINE:
! To decompose a raw binary floating point number into
! its three parts

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    TYPE(BinRep)                    :: FpBin

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

! set sign
    FpBin%Negative    = IAND(RawVal, SignMask) /= 0_I8B
! set significand
    FpBin%Significand = RawFP_Significand(RawVal)
! set exponent
    FpBin%Exponent    = RawFP_BiasedExponent(RawVal)

    RETURN

END FUNCTION RawFP_Decompose

!******************************************************************************

FUNCTION RawFP_FromFloat(RealVal) RESULT(RawVal)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_FromFloat

!** PURPOSE OF THIS SUBROUTINE:
! To construct a raw binary floating point number based on
! the specified real number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=DP), INTENT(IN)   :: RealVal
    INTEGER(KIND=I8B)           :: RawVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: IntVal
    REAL(KIND=DP)       :: FloatVal
    EQUIVALENCE(IntVal, FloatVal)

!** FLOW

    FloatVal = RealVal
    RawVal   = IntVal

    RETURN

END FUNCTION RawFP_FromFloat

!******************************************************************************

FUNCTION RawFP_ToFloat(RawVal) RESULT(RealVal)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_ToFloat

!** PURPOSE OF THIS SUBROUTINE:
! To convert a raw binary floating point number into
! its equivalent real number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    REAL(KIND=DP)                  :: RealVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: IntVal
    REAL(KIND=DP)       :: FloatVal
    EQUIVALENCE(IntVal, FloatVal)

!** FLOW

    IntVal  = RawVal
    RealVal = FloatVal

    RETURN

END FUNCTION RawFP_ToFloat

!******************************************************************************

FUNCTION RawFP_SetZero(Negative) RESULT(RawVal)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_SetZero

!** PURPOSE OF THIS SUBROUTINE:
! To set value to zero

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    LOGICAL, INTENT(IN)     :: Negative
    INTEGER(KIND=I8B)       :: RawVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    IF (Negative) THEN
        RawVal = SignMask
    ELSE
        RawVal = 0_I8B
    END IF

    RETURN

END FUNCTION RawFP_SetZero

!******************************************************************************

FUNCTION RawFP_SetInfinity(Negative) RESULT(RawVal)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_SetInfinity

!** PURPOSE OF THIS SUBROUTINE:
! To set value to infinity

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    LOGICAL, INTENT(IN)     :: Negative
    INTEGER(KIND=I8B)       :: RawVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)       :: Exponent

!** FLOW

! set sign of infinity
    RawVal = RawFP_SetZero(Negative)
! set infinity biased exponent
    Exponent = IAND(SHIFTL(ToUnsignedLong(MaxExponent), SignificandBits), ExponentMask)
    RawVal = IAND(RawVal, NOT(ExponentMask))
    RawVal = IOR(RawVal, Exponent)

    RETURN

END FUNCTION RawFP_SetInfinity

!******************************************************************************

FUNCTION RawFP_SetNaN(Quiet) RESULT(RawVal)

!DIR$ ATTRIBUTES FORCEINLINE :: RawFP_SetNaN

!** PURPOSE OF THIS SUBROUTINE:
! To set value to NaN

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    LOGICAL, INTENT(IN)     :: Quiet
    INTEGER(KIND=I8B)       :: RawVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: Mantissa

!** FLOW

! set infinity biased exponent
    RawVal = RawFP_SetInfinity(FalseVal)
! set NaN significand
    RawVal = IOR(RawVal, SHIFTL(1_I8B, SignificandBits - 2))
    IF (Quiet) THEN
        Mantissa = IAND(QuietNaNMask, SignificandMask)
        RawVal = IAND(RawVal, NOT(SignificandMask))
        RawVal = IOR(RawVal, Mantissa)
    END IF

    RETURN

END FUNCTION RawFP_SetNaN

!******************************************************************************

!------------------------------------------------------------------------------
!
!               HIGH-PRECISION DECIMAL (HPDECIMAL) ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION HPDec_Should_Round_Up(HP, RoundToDigit) RESULT(Flag)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal),   INTENT(IN)    :: HP
    INTEGER(KIND=I4B),  INTENT(IN)    :: RoundToDigit
    LOGICAL                           :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    IF ((RoundToDigit < 0).OR.(RoundToDigit >= HP%NumDigits)) THEN
        Flag = FalseVal
        RETURN
    END IF

! If we're right in the middle and there are no extra digits
    IF ((HP%Digits(RoundToDigit) == 5) .AND.(RoundToDigit + 1 == HP%NumDigits)) THEN

! Round up if we've truncated (since that means the result is slightly
! higher than what's represented.)
        IF (HP%Truncated) THEN
            Flag = TrueVal
            RETURN
        END IF

! If this exactly halfway, round to even.
        IF (RoundToDigit == 0) THEN
! When the input is ".5".
            Flag = FalseVal
            RETURN
        END IF
        Flag = MOD(HP%Digits(RoundToDigit - 1), 2) /= 0
        RETURN
    END IF
! If there are digits after roundToDigit, they must be non-zero since we
! trim trailing zeros after all operations that change digits.
    Flag = HP%Digits(RoundToDigit) >= 5

    RETURN

END FUNCTION HPDec_Should_Round_Up

!******************************************************************************

FUNCTION HPDec_Get_Num_New_Digits(HP, LShiftAmount) RESULT(NewDigits)

! Takes an amount to left shift and returns the number of new digits needed
! to store the result based on LEFT_SHIFT_DIGIT_TABLE.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal),   INTENT(IN)    :: HP
    INTEGER(KIND=I4B),  INTENT(IN)    :: LShiftAmount
    INTEGER(KIND=I4B)                 :: NewDigits

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER   :: A0 = IACHAR('0')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=:), ALLOCATABLE  :: PowerOfFive
    INTEGER(KIND=I1B)              :: CurDigit, P5Digit
    INTEGER(KIND=I4B)              :: Indx
    INTEGER(KIND=I4B)              :: Length

!** FLOW

    Length      = LShift_Length(LShiftAmount)
    PowerOfFive = LShift_PowFive(LShiftAmount)(1:Length)
    NewDigits   = LShift_Digits(LShiftAmount)

    Indx = 1
    DO WHILE (Indx <= Length)
        IF (Indx > HP%NumDigits) THEN
            NewDigits = NewDigits - 1
            RETURN
        END IF
        P5Digit  = INT(IACHAR(PowerOfFive(Indx:Indx)) - A0, KIND=I1B)
        CurDigit = HP%Digits(Indx-1)
        IF (CurDigit /= P5Digit) THEN
            IF (CurDigit < P5Digit) NewDigits = NewDigits - 1
            RETURN
        END IF
        Indx = Indx + 1
    END DO

    RETURN

END FUNCTION HPDec_Get_Num_New_Digits

!******************************************************************************

SUBROUTINE HPDec_Trim_Trailing_Zeroes(HP)

! Trim all trailing 0s

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(INOUT) :: HP

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    DO WHILE ((HP%NumDigits > 0).AND.(HP%Digits(HP%NumDigits - 1) == 0))
        HP%NumDigits = HP%NumDigits - 1
    END DO
    IF (HP%NumDigits == 0) THEN
        HP%DecimalPoint = 0
    END IF

    RETURN

END SUBROUTINE HPDec_Trim_Trailing_Zeroes

!******************************************************************************

SUBROUTINE HPDec_Right_Shift(HP, ShiftAmount)

! Perform a digitwise binary non-rounding right shift on this value by ShiftAmount.
! The ShiftAmount can't be more than MAX_SHIFT_AMOUNT to prevent overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal),   INTENT(INOUT) :: HP
    INTEGER(KIND=I4B),  INTENT(IN)    :: ShiftAmount

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: ReadIndx
    INTEGER(KIND=I4B)     :: WriteIndx
    INTEGER(KIND=I8B)     :: Accumulator
    INTEGER(KIND=I8B)     :: ShiftMask
    INTEGER(KIND=I8B)     :: ReadDigit
    INTEGER(KIND=I8B)     :: WriteDigit

!** FLOW

! initialize
    ReadIndx = 0
    WriteIndx = 0
    Accumulator = 0_I8B
    ShiftMask = SHIFTL(1_I8B, ShiftAmount) - 1_I8B

! Warm Up phase: we don't have enough digits to start writing, so just
! read them into the Accumulator.
    DO WHILE (SHIFTR(Accumulator, ShiftAmount) == 0_I8B)
        ReadDigit = 0_I8B
! If there are still digits to read, read the next one, else the digit is
! assumed to be 0.
        IF (ReadIndx < HP%NumDigits) ReadDigit = HP%Digits(ReadIndx)
        Accumulator = Accumulator * 10_I8B + ReadDigit
        ReadIndx = ReadIndx + 1
    END DO

! Shift the decimal point by the number of digits it took to fill the
! Accumulator.
    HP%DecimalPoint = HP%DecimalPoint - (ReadIndx - 1)

! Middle phase: we have enough digits to write, as well as more digits to
! read. Keep reading until we run out of digits.
    DO WHILE (ReadIndx < HP%NumDigits)
        ReadDigit = HP%Digits(ReadIndx)
        WriteDigit = SHIFTR(Accumulator, ShiftAmount)
        Accumulator = IAND(Accumulator, ShiftMask)
        HP%Digits(WriteIndx) = INT(WriteDigit, KIND=I1B)
        Accumulator = Accumulator * 10_I8B + ReadDigit
        ReadIndx = ReadIndx + 1
        WriteIndx = WriteIndx + 1
    END DO

! Cool Down phase: All of the readable digits have been read, so just write
! the remainder, DO WHILE treating any more digits as 0.
! DO WHILE (Accumulator > 0_I8B)
    DO WHILE (Accumulator /= 0_I8B)   ! +++ unsigned comparison +++
        WriteDigit = SHIFTR(Accumulator, ShiftAmount)
        Accumulator = IAND(Accumulator, ShiftMask)
        IF (WriteIndx < MAX_NUM_DIGITS) THEN
            HP%Digits(WriteIndx) = INT(WriteDigit, KIND=I1B)
            WriteIndx = WriteIndx + 1
        ELSEIF (WriteDigit /= 0_I8B) THEN     ! +++ unsigned comparison +++
            HP%Truncated = TrueVal
        END IF
        Accumulator = Accumulator * 10_I8B
    END DO
    HP%NumDigits = WriteIndx
    CALL HP%TrimTrailingZeroes()

    RETURN

END SUBROUTINE HPDec_Right_Shift

!******************************************************************************

SUBROUTINE HPDec_Left_Shift(HP, ShiftAmount)

! Perform a digitwise binary non-rounding left shift on this value by ShiftAmount.
! The ShiftAmount can't be more than MAX_SHIFT_AMOUNT to prevent overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal),   INTENT(INOUT) :: HP
    INTEGER(KIND=I4B),  INTENT(IN)    :: ShiftAmount

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: NewDigits
    INTEGER(KIND=I4B)     :: ReadIndx
    INTEGER(KIND=I4B)     :: WriteIndx
    INTEGER(KIND=I8B)     :: Accumulator
    INTEGER(KIND=I8B)     :: NextAccumulator
    INTEGER(KIND=I8B)     :: WriteDigit

!** FLOW

! initialize
    NewDigits = HP%GetNumNewDigits(ShiftAmount)
    ReadIndx  = HP%NumDigits - 1
    WriteIndx = HP%NumDigits + NewDigits
    Accumulator = 0_I8B

! No Warm Up phase. Since we're putting digits in at the top and taking
! digits from the bottom we don't have to wait for the Accumulator to fill.

! Middle phase: while we have more digits to read, keep reading as well as
! writing.
    DO WHILE (ReadIndx >= 0)
        Accumulator = Accumulator + SHIFTL(INT(HP%Digits(ReadIndx), KIND=I8B), ShiftAmount)
! +++ unsigned division and modulation +++
! NextAccumulator = Accumulator / 10_I8B
! WriteDigit = Accumulator - (10_I8B * NextAccumulator)
        CALL UDivMod(Accumulator, 10_I8B, NextAccumulator, WriteDigit)
        WriteIndx = WriteIndx - 1
        IF (WriteIndx < MAX_NUM_DIGITS) THEN
            HP%Digits(WriteIndx) = INT(WriteDigit, KIND=I1B)
        ELSEIF (WriteDigit /= 0_I8B) THEN
            HP%Truncated = TrueVal
        END IF
        Accumulator = NextAccumulator
        ReadIndx = ReadIndx - 1
    END DO

! Cool Down phase: there are no more digits to read, so just write the
! remaining digits in the Accumulator.
! DO WHILE (Accumulator > 0_I8B)
    DO WHILE (Accumulator /= 0_I8B)   ! +++ unsigned comparison +++
! +++ unsigned division and modulation +++
! NextAccumulator = Accumulator / 10_I8B
! WriteDigit = Accumulator - (10_I8B * NextAccumulator)
        CALL UDivMod(Accumulator, 10_I8B, NextAccumulator, WriteDigit)
        WriteIndx = WriteIndx - 1
        IF (WriteIndx < MAX_NUM_DIGITS) THEN
            HP%Digits(WriteIndx) = INT(WriteDigit, KIND=I1B)
        ELSEIF (WriteDigit /= 0_I8B) THEN
            HP%Truncated = TrueVal
        END IF
        Accumulator = NextAccumulator
    END DO

    HP%NumDigits = HP%NumDigits + NewDigits
    IF (HP%NumDigits > MAX_NUM_DIGITS) THEN
        HP%NumDigits = MAX_NUM_DIGITS
    END IF
    HP%DecimalPoint = HP%DecimalPoint + NewDigits
    CALL HP%TrimTrailingZeroes()

    RETURN

END SUBROUTINE HPDec_Left_Shift

!******************************************************************************

SUBROUTINE HPDec_Construct(HP, cStr, Start, Finish)

! To construct 'HPDecimal' based on input string (cStr) where
!   - Start is the index of the first valid numeric character, and
!   - Finish is the index of the last valid character (== length of the input
!     string excluding trailing space(s))
! The routine assumes that cStr is a 'VALID' floating point string and
! Start is less than Finish.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal),   INTENT(INOUT) :: HP
    CHARACTER(LEN=*),   INTENT(IN)    :: cStr
    INTEGER(KIND=I4B),  INTENT(IN)    :: Start
    INTEGER(KIND=I4B),  INTENT(IN)    :: Finish

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER    :: A0 = IACHAR('0')
    CHARACTER(LEN=*),  PARAMETER    :: SET_DIGITS    = '0123456789'
    CHARACTER(LEN=*),  PARAMETER    :: SET_EXPONENTS = 'EeDdQq'
    CHARACTER(LEN=*),  PARAMETER    :: SET_SIGNS     = '+-'
    CHARACTER(LEN=*),  PARAMETER    :: SET_INTEGERS  = SET_DIGITS // SET_SIGNS

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Ptr
    INTEGER(KIND=I4B)     :: Add2Exp
    LOGICAL               :: SawDot
    INTEGER(KIND=I4B)     :: TotalDigits  ! This counts the digits in the number, even if
! there isn't space to store them all.

!** FLOW

! initialize
    Ptr = Start
    SawDot = FalseVal
    TotalDigits = 0

    DO WHILE ((Is_Character_Digit(cStr(Ptr:Ptr))).OR.(cStr(Ptr:Ptr) == '.'))
        IF (cStr(Ptr:Ptr) == '.') THEN
            IF (SawDot) EXIT
            HP%DecimalPoint = TotalDigits
            SawDot = TrueVal
        ELSE
            IF ((cStr(Ptr:Ptr) == '0').AND.(HP%NumDigits == 0)) THEN
                HP%DecimalPoint = HP%DecimalPoint - 1
                Ptr = Ptr + 1
                IF (Ptr <= Finish) THEN
                    CYCLE
                ELSE
                    EXIT
                END IF
            END IF
            TotalDigits = TotalDigits + 1
            IF (HP%NumDigits < MAX_NUM_DIGITS) THEN
                HP%Digits(HP%NumDigits) = INT(IACHAR(cStr(Ptr:Ptr))-A0, KIND=I1B)
                HP%NumDigits = HP%NumDigits + 1
            ELSEIF (cStr(Ptr:Ptr) /= '0') THEN
                HP%Truncated = TrueVal
            END IF
        END IF
        Ptr = Ptr + 1
        IF (Ptr > Finish) EXIT
    END DO

    IF (.NOT.SawDot) HP%DecimalPoint = TotalDigits

    IF (Ptr <= Finish) THEN
        IF (Is_Character_Exponent(cStr(Ptr:Ptr))) THEN
            Ptr = Ptr + 1
            IF (Ptr <= Finish) THEN
                IF (Is_Character_Integer(cStr(Ptr:Ptr))) THEN
                    Add2Exp = I32_FromChar(cStr(Ptr:))
                    IF (Add2Exp > 100000) THEN
                        Add2Exp = 100000
                    ELSEIF (Add2Exp < -100000) THEN
                        Add2Exp = -100000
                    END IF
                    HP%DecimalPoint = HP%DecimalPoint + Add2Exp
                END IF
            END IF
        ELSEIF (Is_Character_Sign(cStr(Ptr:Ptr))) THEN
            Add2Exp = I32_FromChar(cStr(Ptr:))
            IF (Add2Exp > 100000) THEN
                Add2Exp = 100000
            ELSEIF (Add2Exp < -100000) THEN
                Add2Exp = -100000
            END IF
            HP%DecimalPoint = HP%DecimalPoint + Add2Exp
        END IF
    END IF

    CALL HP%TrimTrailingZeroes()

    RETURN
    CONTAINS

    FUNCTION Is_Character_Digit(Chr) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the given character is in the 'DIGIT' set

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=1),  INTENT(IN)  :: Chr          ! character
        LOGICAL                        :: Flag         ! true if the character is valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW:

        Flag = (INDEX(SET_DIGITS, Chr) /= 0)

        RETURN

    END FUNCTION Is_Character_Digit

!**************************************************************************

    FUNCTION Is_Character_Exponent(Chr) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the given character is an 'exponent' character

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=1),  INTENT(IN)  :: Chr          ! character
        LOGICAL                        :: Flag         ! true if the character is valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW:

        Flag = (INDEX(SET_EXPONENTS, Chr) /= 0)

        RETURN

    END FUNCTION Is_Character_Exponent

!**************************************************************************

    FUNCTION Is_Character_Integer(Chr) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the given character is in the 'INTEGER' set

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=1),  INTENT(IN)  :: Chr          ! character
        LOGICAL                        :: Flag         ! true if the character is valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW:

        Flag = (INDEX(SET_INTEGERS, Chr) /= 0)

        RETURN

    END FUNCTION Is_Character_Integer

!**************************************************************************

    FUNCTION Is_Character_Sign(Chr) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the given character is in the 'SIGN' set

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=1), INTENT(IN)  :: Chr          ! character
        LOGICAL                       :: Flag         ! true if the character is valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW:

        Flag = (INDEX(SET_SIGNS, Chr) /= 0)

        RETURN

    END FUNCTION Is_Character_Sign

!**************************************************************************

END SUBROUTINE HPDec_Construct

!******************************************************************************

SUBROUTINE HPDec_Shift(HP, Shift)

! Binary shift left (ShiftAmount > 0) or right (ShiftAmount < 0)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal),   INTENT(INOUT) :: HP
    INTEGER(KIND=I4B),  INTENT(IN)    :: Shift

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: ShiftAmount

!** FLOW

    ShiftAmount = Shift
    IF (ShiftAmount > 0) THEN
! Left shift
        DO WHILE (ShiftAmount > MAX_SHIFT_AMOUNT)
            CALL HP%LeftShift(MAX_SHIFT_AMOUNT)
            ShiftAmount = ShiftAmount - MAX_SHIFT_AMOUNT
        END DO
        CALL HP%LeftShift(ShiftAmount)
    ELSEIF (ShiftAmount < 0) THEN
! Right shift
        DO WHILE (ShiftAmount < -MAX_SHIFT_AMOUNT)
            CALL HP%RightShift(MAX_SHIFT_AMOUNT)
            ShiftAmount = ShiftAmount + MAX_SHIFT_AMOUNT
        END DO
        CALL HP%RightShift(-ShiftAmount)
    END IF

    RETURN

END SUBROUTINE HPDec_Shift

!******************************************************************************

SUBROUTINE HPDec_Round_To_UInt(HP, ResVal)

! Round the number represented to the closest value of UIntType.
! This is done ignoring overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal),   INTENT(IN)    :: HP
    INTEGER(KIND=I8B),  INTENT(OUT)   :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: CurDigit

!** FLOW

    ResVal = 0_I8B
    CurDigit = 0

    DO WHILE ((CurDigit < HP%DecimalPoint).AND.(CurDigit < HP%NumDigits))
        ResVal = ResVal * 10_I8B + INT(HP%Digits(CurDigit), KIND=I8B)
        CurDigit = CurDigit + 1
    END DO

! If there are implicit 0s at the end of the number, include those.
    DO WHILE (CurDigit < HP%DecimalPoint)
        ResVal = ResVal * 10_I8B
        CurDigit = CurDigit + 1
    END DO
    IF (HP%ShouldRoundUp(HP%DecimalPoint)) THEN
        ResVal = ResVal + 1_I8B
    END IF

    RETURN

END SUBROUTINE HPDec_Round_To_UInt

!******************************************************************************

!------------------------------------------------------------------------------
!
!            MULTI-PRECISION UNSIGNED INTEGER (BIGUINT) ROUTINES
!
!------------------------------------------------------------------------------

! -----------------------------------------------------------------------------
! -----   BigUInt Routines for FastFloat Algorithms                       -----
! -----------------------------------------------------------------------------

FUNCTION Empty_Hi64(Truncated) RESULT(Val)

!DIR$ ATTRIBUTES FORCEINLINE :: Empty_Hi64

!** PURPOSE OF THIS SUBROUTINE:
! To return empty value.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    LOGICAL, INTENT(OUT)   :: Truncated
    INTEGER(KIND=I8B)      :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Truncated = FalseVal
    Val = 0_I8B

    RETURN

END FUNCTION Empty_Hi64

!******************************************************************************

FUNCTION UInt64_Hi64_I(R0, Truncated) RESULT(Val)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt64_Hi64_I

!** PURPOSE OF THIS SUBROUTINE:
! To return high bit of uint64 value.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)    :: R0
    LOGICAL, INTENT(OUT)              :: Truncated
    INTEGER(KIND=I8B)                 :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: ShiftPos

!** FLOW

    Truncated = FalseVal
    ShiftPos = LEADZ(R0)
    Val = SHIFTL(R0, ShiftPos)

    RETURN

END FUNCTION UInt64_Hi64_I

!******************************************************************************

FUNCTION UInt64_Hi64_II(R0, R1, Truncated) RESULT(Val)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt64_Hi64_II

!** PURPOSE OF THIS SUBROUTINE:
! To return high bit of uint64 values.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)    :: R0, R1
    LOGICAL, INTENT(OUT)              :: Truncated
    INTEGER(KIND=I8B)                 :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: ShiftPos

!** FLOW

    ShiftPos = LEADZ(R0)
    IF (ShiftPos == 0) THEN
        Truncated = R1 /= 0_I8B
        Val = R0
    ELSE
        Truncated = SHIFTL(R1, ShiftPos) /= 0_I8B
        Val = IOR(SHIFTL(R0, ShiftPos), SHIFTR(R1, 64-ShiftPos))
    END IF

    RETURN

END FUNCTION UInt64_Hi64_II

!******************************************************************************

FUNCTION BigUInt_IsEmpty(Big) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
! To check whether BigUInt is empty or not.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big
    LOGICAL                     :: Flag     ! true if empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Flag = Big%Length == 0

    RETURN

END FUNCTION BigUInt_IsEmpty

!******************************************************************************

FUNCTION BigUInt_IsNonZero(Big, Index) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
! To check if any limbs are non-zero after the given index.
! this needs to be done in reverse order, since the index
! is relative to the most significant limbs.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(IN)  :: Big      ! BigUInt object
    INTEGER(KIND=I4B),  INTENT(IN)  :: Index    ! the specified index
    LOGICAL                         :: Flag     ! true if the stack is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: I, RIndex

!** FLOW

    I = Index
    DO WHILE (I < Big%Length)
        RIndex = Big%Length - I - 1
        IF (Big%Digit(RIndex) /= 0_I8B) THEN
            Flag = TrueVal
            RETURN
        END IF
        I = I + 1
    END DO
    Flag = FalseVal

    RETURN

END FUNCTION BigUInt_IsNonZero

!******************************************************************************

SUBROUTINE BigUInt_Push(Big, Value)

!** PURPOSE OF THIS SUBROUTINE:
! To append the item to the BigUInt

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(INOUT)  :: Big       ! BigUInt object
    INTEGER(KIND=I8B),  INTENT(IN)     :: Value    ! item to be appended

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

! FLOW

    Big%Digit(Big%Length) = Value
    Big%Length = Big%Length + 1

    RETURN

END SUBROUTINE BigUInt_Push

!******************************************************************************

SUBROUTINE BigUInt_Extend(Big, Span)

!** PURPOSE OF THIS SUBROUTINE:
! To append a span of items to the stack

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(INOUT)  :: Big       ! BigUInt object
    INTEGER(KIND=I8B),  INTENT(IN)     :: Span(0:)  ! span of items to be appended

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: SpanLen

! FLOW

    SpanLen = SIZE(Span)
    Big%Digit(Big%Length:Big%Length+SpanLen-1) = Span(0:SpanLen-1)
    Big%Length = Big%Length + SpanLen

    RETURN

END SUBROUTINE BigUInt_Extend

!******************************************************************************

SUBROUTINE BigUInt_Normalize(Big)

!** PURPOSE OF THIS SUBROUTINE:
! To normalize the BigUInt, so most-significant zero digits are removed.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)   :: Big  ! BigUInt object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: RIndex

! FLOW

    RIndex = Big%Length - 1
    IF (RIndex >= 0) THEN
        DO WHILE (Big%Digit(RIndex) == 0_I8B)
            Big%Length = Big%Length - 1
            RIndex = Big%Length - 1
            IF (RIndex < 0) EXIT
        END DO
    END IF

    RETURN

END SUBROUTINE BigUInt_Normalize

!******************************************************************************

FUNCTION ScalarAdd(X, Y, Overflow) RESULT(Z)

!** PURPOSE OF THIS SUBROUTINE:
! To add two small integers, checking for overflow.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)    :: X, Y
    LOGICAL,            INTENT(OUT)   :: Overflow
    INTEGER(KIND=I8B)                 :: Z

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

! add value
    Z = X + Y

! check overflow
    Overflow = Z .ULT. X

    RETURN

END FUNCTION ScalarAdd

!******************************************************************************

FUNCTION ScalarMul(X, Y, Carry) RESULT(Z_Low)

!** PURPOSE OF THIS SUBROUTINE:
! To multiply two small integers, getting both the high and low bits.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)     :: X, Y
    INTEGER(KIND=I8B), INTENT(INOUT)  :: Carry
    INTEGER(KIND=I8B)                 :: Z_Low

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Z_Hi
    LOGICAL    :: Overflow

!** FLOW

    CALL UMul128(X, Y, Z_Hi, Z_Low)

    Z_Low = ScalarAdd(Z_Low, Carry, Overflow)

    IF (Overflow) Z_Hi  = Z_Hi + 1_I8B    ! cannot overflow
    Carry = Z_Hi

    RETURN

END FUNCTION ScalarMul

!******************************************************************************

SUBROUTINE BigUInt_SmallMul(Big, Y)

!** PURPOSE OF THIS SUBROUTINE:
! To multiply BigUInt by scalar value.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(INOUT)   :: Big  ! BigUInt object
    INTEGER(KIND=I8B),  INTENT(IN)      :: Y    ! value to be added

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Index
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    Carry = 0_I8B
    DO Index = 0, Big%Length-1
        Big%Digit(Index) = ScalarMul(Big%Digit(Index), Y, Carry)
    END DO
    IF (Carry /= 0_I8B) CALL Big%Push(Carry)

    RETURN

END SUBROUTINE BigUInt_SmallMul

!******************************************************************************

SUBROUTINE BigUInt_LongMul(Big, Span)

!** PURPOSE OF THIS SUBROUTINE:
! To multiply BigUInt and BigUInt using grade-school multiplication algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(INOUT)   :: Big      ! BigUInt object
    INTEGER(KIND=I8B),  INTENT(IN)      :: Span(0:) ! span of values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: SpanLen
    INTEGER(KIND=I8B)     :: Z(0:Big%Length+SIZE(Span)-1)

!** FLOW

    SpanLen = SIZE(Span)
    IF (SpanLen /= 0) THEN
! perform multiplication
        CALL MultiplyBasic(Big%Digit, Big%Length, Span, SpanLen, Z)
! transfer output from the buffer back to the stack
        Big%Length = Big%Length + SpanLen
        Big%Digit(0:Big%Length-1) = Z(0:Big%Length-1)
    END IF

    CALL Big%Normalize()

    RETURN

END SUBROUTINE BigUInt_LongMul

!******************************************************************************

SUBROUTINE BigUInt_From_U64(Big, Val)

!** PURPOSE OF THIS SUBROUTINE:
! To create BigUInt from a unsigned 64-bit integer.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(INOUT)    :: Big
    INTEGER(KIND=I8B),  INTENT(IN)       :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    CALL Big%Push(Val)
    CALL Big%Normalize()

    RETURN

END SUBROUTINE BigUInt_From_U64

!******************************************************************************

FUNCTION BigUInt_Get_Hi64(Big, Truncated) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
! To get the high 64 bits from the vector, and if bits were truncated.
! this is to get the significant digits for the float.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big
    LOGICAL,        INTENT(OUT) :: Truncated
    INTEGER(KIND=I8B)           :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: RIndex

!** FLOW

    IF (Big%Length == 0) THEN
        Val = Empty_Hi64(Truncated)
    ELSEIF (Big%Length == 1) THEN
        RIndex = Big%Length - 1
        Val = Uint64_Hi64_I(Big%Digit(RIndex), Truncated)
    ELSE
        RIndex = Big%Length - 1
        Val = Uint64_Hi64_II(Big%Digit(RIndex), Big%Digit(RIndex-1), Truncated)
        Truncated = Truncated .OR. Big%IsNonZero(2)
    END IF

    RETURN

END FUNCTION BigUInt_Get_Hi64

!******************************************************************************

FUNCTION BigUInt_Compare(Big, Other) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
! To compare two big integers, returning the large value.
! assumes both are normalized. if the return value is
! negative, other is larger, if the return value is
! positive, this is larger, otherwise they are equal.
! the limbs are stored in little-endian order, so we
! must compare the limbs in ever order.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big, Other
    INTEGER(KIND=I4B)           :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Index

!** FLOW

    IF (Big%Length > Other%Length) THEN
        Flag = 1
    ELSEIF (Big%Length < Other%Length) THEN
        Flag = -1
    ELSE
        DO Index = Big%Length-1, 0, -1
            ASSOCIATE (XI => Big%Digit(Index), YI => Other%Digit(Index))
                IF (XI .UGT. YI) THEN
                    Flag = 1
                    RETURN
                ELSEIF (XI .ULT. YI) THEN
                    Flag = -1
                    RETURN
                END IF
            END ASSOCIATE
        END DO
        Flag = 0
    END IF

    RETURN

END FUNCTION BigUInt_Compare

!******************************************************************************

SUBROUTINE BigUInt_ShiftL(Big, N)

!** PURPOSE OF THIS SUBROUTINE:
! To move the limbs left by `n` bits.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(INOUT)   :: Big
    INTEGER(KIND=I4B),  INTENT(IN)      :: N

!** SUBROUTINE PARAMETER DECLARATIONS:
! these parameters are for DigitBits = 64
    INTEGER(KIND=I4B), PARAMETER  :: LargePos  = 6
    INTEGER(KIND=I4B), PARAMETER  :: SmallMask = 63

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: LargeShift, SmallShift

!** FLOW

    LargeShift = SHIFTR(N, LargePos)
    SmallShift = IAND(N, SmallMask)
    IF (LargeShift > 0) CALL BigUInt_ShiftL_Limbs(Big, LargeShift)
    IF (SmallShift > 0) CALL BigUInt_ShiftL_Bits(Big, SmallShift)

    RETURN
    CONTAINS

    SUBROUTINE BigUInt_ShiftL_Bits(Big, N)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To shift left each limb n bits, carrying over to the new limb
    ! returns true if we were able to shift all the digits.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(BigUInt),     INTENT(INOUT)   :: Big
        INTEGER(KIND=I4B),  INTENT(IN)      :: N

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: Index, Shl, Shr
        INTEGER(KIND=I8B)     :: XI, Carry, Prev

    !** FLOW

    ! Internally, for each item, we shift left by n, and add the previous
    ! right shifted limb-bits.
    ! For example, we transform (for u8) shifted left 2, to:
    !      b10100100 b01000010
    !      b10 b10010001 b00001000
    ! ASSERT(n /= 0)
    ! ASSERT(n < sizeof(limb) * 8)

        Shl = N
        Shr = DigitBits - Shl
        Prev = 0_I8B
        DO Index = 0, Big%Length-1
            XI = Big%Digit(Index)
            Big%Digit(Index) = IOR(SHIFTL(XI, Shl), SHIFTR(Prev, Shr))
            Prev = XI
        END DO

        Carry = SHIFTR(Prev, Shr)
        IF (Carry /= 0_I8B) CALL Big%Push(Carry)

        RETURN

    END SUBROUTINE BigUInt_ShiftL_Bits

!**************************************************************************

    SUBROUTINE BigUInt_ShiftL_Limbs(Big, N)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To move the limbs left by `n` limbs.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(BigUInt),     INTENT(INOUT)   :: Big
        INTEGER(KIND=I4B),  INTENT(IN)      :: N

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: Buffer(0:Big%Length-1)

    !** FLOW

        IF (.NOT.Big%IsEmpty()) THEN
    ! move limbs by first copy source to buffer
            Buffer(0:Big%Length-1) = Big%Digit(0:Big%Length-1)
    ! then copy from the buffer to the destination
            Big%Digit(N:Big%Length+N-1) = Buffer(0:Big%Length-1)
    ! fill in empty limbs
            Big%Digit(0:N-1) = 0_I8B
    ! set length
            Big%Length = Big%Length + N
        END IF

        RETURN

    END SUBROUTINE BigUInt_ShiftL_Limbs

!**************************************************************************

END SUBROUTINE BigUInt_ShiftL

!******************************************************************************

FUNCTION BigUInt_LeadZ(Big) RESULT(N)

!** PURPOSE OF THIS SUBROUTINE:
! To get the number of leading zeros in the BigUInt.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big
    INTEGER(KIND=I4B)           :: N

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    IF (Big%IsEmpty()) THEN
        N = 0
    ELSE
        N = LEADZ(Big%Digit(Big%Length - 1))
    END IF

    RETURN

END FUNCTION BigUInt_LeadZ

!******************************************************************************

FUNCTION BigUInt_BitLen(Big) RESULT(N)

!** PURPOSE OF THIS SUBROUTINE:
! To get the number of bits in the BigUInt.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big
    INTEGER(KIND=I4B)           :: N

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: LZ

!** FLOW

    LZ = Big%LeadZ()
    N  = DigitBits*Big%Length - LZ

    RETURN

END FUNCTION BigUInt_BitLen

!******************************************************************************

SUBROUTINE BigUInt_Add(Big, Y)

!** PURPOSE OF THIS SUBROUTINE:
! To add a long number to the BigUInt.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(INOUT)   :: Big
    INTEGER(KIND=I8B),  INTENT(IN)      :: Y

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Index
    INTEGER(KIND=I8B)     :: Carry
    LOGICAL               :: Overflow

!** FLOW

    Index = 0
    Carry = Y

    DO WHILE ((Carry /= 0_I8B).AND.(Index < Big%Length))
        Big%Digit(Index) = ScalarAdd(Big%Digit(Index), Carry, Overflow)
        IF (Overflow) THEN
            Carry = 1_I8B
        ELSE
            Carry = 0_I8B
        END IF
        Index = Index + 1
    END DO
    IF (Carry /= 0_I8B) CALL Big%Push(Carry)

    RETURN

END SUBROUTINE BigUInt_Add

!******************************************************************************

SUBROUTINE BigUInt_Pow2(Big, Exp)

!** PURPOSE OF THIS SUBROUTINE:
! To multiply as if by 2 raised to a power.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(INOUT)   :: Big
    INTEGER(KIND=I4B),  INTENT(IN)      :: Exp

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    CALL Big%ShiftL(Exp)

    RETURN

END SUBROUTINE BigUInt_Pow2

!******************************************************************************

SUBROUTINE BigUInt_Pow5(Big, Exp)

!** PURPOSE OF THIS SUBROUTINE:
! To multiply as if by 5 raised to a power.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(INOUT)   :: Big
    INTEGER(KIND=I4B),  INTENT(IN)      :: Exp

!** SUBROUTINE PARAMETER DECLARATIONS:
! multiply by a power of 5
    INTEGER(KIND=I4B), PARAMETER  :: Large_Step = 135
    INTEGER(KIND=I4B), PARAMETER  :: Small_Step = 27
    INTEGER(KIND=I4B), PARAMETER  :: Large_Length = 5
    INTEGER(KIND=I8B), PARAMETER  :: Small_Power_of_5(0:Small_Step) = [         &
        INT(Z'0000000000000001', KIND=I8B), INT(Z'0000000000000005', KIND=I8B), &
        INT(Z'0000000000000019', KIND=I8B), INT(Z'000000000000007D', KIND=I8B), &
        INT(Z'0000000000000271', KIND=I8B), INT(Z'0000000000000C35', KIND=I8B), &
        INT(Z'0000000000003D09', KIND=I8B), INT(Z'000000000001312D', KIND=I8B), &
        INT(Z'000000000005F5E1', KIND=I8B), INT(Z'00000000001DCD65', KIND=I8B), &
        INT(Z'00000000009502F9', KIND=I8B), INT(Z'0000000002E90EDD', KIND=I8B), &
        INT(Z'000000000E8D4A51', KIND=I8B), INT(Z'0000000048C27395', KIND=I8B), &
        INT(Z'000000016BCC41E9', KIND=I8B), INT(Z'000000071AFD498D', KIND=I8B), &
        INT(Z'0000002386F26FC1', KIND=I8B), INT(Z'000000B1A2BC2EC5', KIND=I8B), &
        INT(Z'000003782DACE9D9', KIND=I8B), INT(Z'00001158E460913D', KIND=I8B), &
        INT(Z'000056BC75E2D631', KIND=I8B), INT(Z'0001B1AE4D6E2EF5', KIND=I8B), &
        INT(Z'000878678326EAC9', KIND=I8B), INT(Z'002A5A058FC295ED', KIND=I8B), &
        INT(Z'00D3C21BCECCEDA1', KIND=I8B), INT(Z'0422CA8B0A00A425', KIND=I8B), &
        INT(Z'14ADF4B7320334B9', KIND=I8B), INT(Z'6765C793FA10079D', KIND=I8B)]
    INTEGER(KIND=I8B), PARAMETER  :: Max_Native = Small_Power_of_5(Small_Step)    ! 7450580596923828125_I8B
    INTEGER(KIND=I8B), PARAMETER  :: Large_Power_of_5(0:Large_Length-1) = [     &
        INT(Z'13A1D71CFF1B172D', KIND=I8B), INT(Z'7F682D3DEFA07617', KIND=I8B), &
        INT(Z'3F0131E7FF8C90C0', KIND=I8B), INT(Z'917B01773FDCB9FE', KIND=I8B), &
        INT(Z'2C06B9D16C407A7', KIND=I8B)]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: IExp

!** FLOW

    IExp = Exp

! multiply Big by 5**Large_Step
    DO WHILE (IExp >= Large_Step)
        CALL Big%LongMul(Large_Power_of_5)
        IExp = IExp - Large_Step
    END DO

! multiply Big by 5**Small_Step
    DO WHILE (IExp >= Small_Step)
        CALL Big%SmallMul(Max_Native)
        IExp = IExp - Small_Step
    END DO

! multiply Big by 5**IExp
    IF (IExp /= 0) CALL Big%SmallMul(Small_Power_of_5(IExp))

    RETURN

END SUBROUTINE BigUInt_Pow5

!******************************************************************************

SUBROUTINE BigUInt_Pow10(Big, Exp)

!** PURPOSE OF THIS SUBROUTINE:
! To multiply as if by 10 raised to a power.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt),     INTENT(INOUT)   :: Big
    INTEGER(KIND=I4B),  INTENT(IN)      :: Exp

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    CALL Big%Pow5(Exp)
    CALL Big%Pow2(Exp)

    RETURN

END SUBROUTINE BigUInt_Pow10

! -----------------------------------------------------------------------------
! -----   BigUInt Routines for YY Algorithms                              -----
! -----------------------------------------------------------------------------

SUBROUTINE BigInt_Add_U64(Big, Val)

!** PURPOSE OF THIS SUBROUTINE:
! To evaluate 'Big = Big + Val'.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt),      INTENT(INOUT)    :: Big  ! a big number (can be 0)
    INTEGER(KIND=I8B),  INTENT(IN)       :: Val  ! an unsigned integer (can be 0)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Idx, Max
    INTEGER(KIND=I8B)     :: Num, Add

!** FLOW

    Num = Big%Digit(0)
    Add = Num + Val
    Big%Digit(0) = Add
    IF ((Add .UGE. Num).OR.(Add .UGE. Val)) RETURN
! add digit
    Max = Big%Length
    DO Idx = 1, Max-1
        IF (Big%Digit(Idx) /= MaxU64) THEN
            Big%Digit(Idx) = Big%Digit(Idx) + 1_I8B
            RETURN
        END IF
        Big%Digit(Idx) = 0_I8B
    END DO
    Big%Digit(Big%Length) = 1_I8B
    Big%Length = Big%Length + 1

    RETURN

END SUBROUTINE BigInt_Add_U64

!******************************************************************************

SUBROUTINE BigInt_Mul_U64(Big, Val)

!** PURPOSE OF THIS SUBROUTINE:
! To evaluate 'Big = Big * Val'.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt),      INTENT(INOUT)    :: Big  ! a big number (can be 0)
    INTEGER(KIND=I8B),  INTENT(IN)       :: Val  ! an unsigned integer (cannot be 0)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Idx, Max
    INTEGER(KIND=I8B)     :: Hi, Lo, Carry

!** FLOW

! initialize
    Idx = 0
    Max = Big%Length
    Carry = 0_I8B
    DO WHILE (Idx < Max)
        IF (Big%Digit(Idx) /= 0_I8B) EXIT
        Idx = Idx + 1
    END DO
    DO WHILE (Idx < Max)
        CALL UMul128_N_Add(Big%Digit(Idx), Val, Carry, Hi, Lo)
        Big%Digit(Idx) = Lo
        Carry = Hi
        Idx = Idx + 1
    END DO
    IF (Carry /= 0_I8B) THEN
        Big%Digit(Big%Length) = Carry
        Big%Length = Big%Length + 1
    END IF

    RETURN

END SUBROUTINE BigInt_Mul_U64

!******************************************************************************

SUBROUTINE BigInt_Mul_Pow2(Big, Exp)

!** PURPOSE OF THIS SUBROUTINE:
! To evaluate 'Big = Big * (2**Exp)'.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt),      INTENT(INOUT)    :: Big  ! a big number (can be 0)
    INTEGER(KIND=I4B),  INTENT(IN)       :: Exp  ! an exponent integer (can be 0)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Shift, Move, Idx
    INTEGER(KIND=I8B)     :: Num

!** FLOW

! initialize
    Shift = IAND(Exp, 63)   ! small shift == MOD(Exp, 64)
    Move  = SHIFTR(Exp, 6)  ! large shift == Exp / 64
    Idx   = Big%Length
    IF (Shift == 0) THEN
        DO WHILE (Idx > 0)
            Big%Digit(Idx + Move - 1) = Big%Digit(Idx - 1)
            Idx = Idx - 1
        END DO
        Big%Length = Big%Length + Move
        DO WHILE (Move /= 0)
            Move = Move - 1
            Big%Digit(Move) = 0_I8B
        END DO
    ELSE
        Big%Digit(Idx) = 0_I8B
        DO WHILE (Idx > 0)
            Num = SHIFTL(Big%Digit(Idx), Shift)
            Num = IOR(Num, SHIFTR(Big%Digit(Idx - 1), (64 - Shift)))
            Big%Digit(Idx + Move) = Num
            Idx = Idx - 1
        END DO
        Big%Digit(Move) = SHIFTL(Big%Digit(0), Shift)
        IF (Big%Digit(Big%Length + Move) /= 0_I8B) THEN
            Big%Length = Big%Length + (Move + 1)
        ELSE
            Big%Length = Big%Length + Move
        END IF
        DO WHILE (Move /= 0)
            Move = Move - 1
            Big%Digit(Move) = 0_I8B
        END DO
    END IF

    RETURN

END SUBROUTINE BigInt_Mul_Pow2

!******************************************************************************

SUBROUTINE BigInt_Mul_Pow10(Big, Exp)

!** PURPOSE OF THIS SUBROUTINE:
! To evaluate 'Big = Big * (10**Exp)'.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt),      INTENT(INOUT)    :: Big  ! a big number (can be 0)
    INTEGER(KIND=I4B),  INTENT(IN)       :: Exp  ! an exponent integer (cannot be 0)

!** SUBROUTINE PARAMETER DECLARATIONS:
! Maximum exponent of exact pow10
    INTEGER(KIND=I4B), PARAMETER  :: U64_POW10_MAX_EXP = 19
! Table: [ 10^0, ..., 10^19 ]
    INTEGER(KIND=I8B), PARAMETER  :: U64_Pow10_Table(0:U64_POW10_MAX_EXP) = [   &
        INT(Z'0000000000000001', KIND=I8B), INT(Z'000000000000000A', KIND=I8B), &
        INT(Z'0000000000000064', KIND=I8B), INT(Z'00000000000003E8', KIND=I8B), &
        INT(Z'0000000000002710', KIND=I8B), INT(Z'00000000000186A0', KIND=I8B), &
        INT(Z'00000000000F4240', KIND=I8B), INT(Z'0000000000989680', KIND=I8B), &
        INT(Z'0000000005F5E100', KIND=I8B), INT(Z'000000003B9ACA00', KIND=I8B), &
        INT(Z'00000002540BE400', KIND=I8B), INT(Z'000000174876E800', KIND=I8B), &
        INT(Z'000000E8D4A51000', KIND=I8B), INT(Z'000009184E72A000', KIND=I8B), &
        INT(Z'00005AF3107A4000', KIND=I8B), INT(Z'00038D7EA4C68000', KIND=I8B), &
        INT(Z'002386F26FC10000', KIND=I8B), INT(Z'016345785D8A0000', KIND=I8B), &
        INT(Z'0DE0B6B3A7640000', KIND=I8B), INT(Z'8AC7230489E80000', KIND=I8B)]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Xpn

!** FLOW

! initialize
    Xpn = Exp

    DO WHILE (Xpn >= U64_POW10_MAX_EXP)
        CALL BigInt_Mul_U64(Big, U64_Pow10_Table(U64_POW10_MAX_EXP))
        Xpn = Xpn - U64_POW10_MAX_EXP
    END DO

    IF (Xpn /= 0) CALL BigInt_Mul_U64(Big, U64_Pow10_Table(Xpn))

    RETURN

END SUBROUTINE BigInt_Mul_Pow10

!******************************************************************************

FUNCTION BigInt_Compare(A, B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
! To compare two BigUInt.
! return -1 if 'a < b', +1 if 'a > b', 0 if 'a == b'.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt), INTENT(IN)   :: A
    TYPE(BigUInt), INTENT(IN)   :: B
    INTEGER(KIND=I4B)           :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Idx

!** FLOW

! first check Length components
    IF (A%Length < B%Length) THEN
        Flag = -1
        RETURN
    END IF
    IF (A%Length > B%Length) THEN
        Flag = +1
        RETURN
    END IF

! next check Digit components
    Idx = A%Length
    DO WHILE (Idx > 0)
        Idx = Idx - 1
        ASSOCIATE (Av => A%Digit(Idx), Bv => B%Digit(Idx))
            IF (Av .ULT. Bv) THEN
                Flag = -1
                RETURN
            END IF
            IF (Av .UGT. Bv) THEN
                Flag = +1
                RETURN
            END IF
        END ASSOCIATE
    END DO
    Flag = 0

    RETURN

END FUNCTION BigInt_Compare

!******************************************************************************

SUBROUTINE BigInt_Set_UIntType(Big, Val)

!** PURPOSE OF THIS SUBROUTINE:
! To set 'Big' with the specified unsigned integer value.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt),      INTENT(INOUT)    :: Big  ! a big number (can be 0)
    INTEGER(KIND=I8B),  INTENT(IN)       :: Val  ! an unsigned integer (can be 0)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Big%Length = 1
    Big%Digit(0) = Val

    RETURN

END SUBROUTINE BigInt_Set_UIntType

!******************************************************************************

SUBROUTINE BigInt_Set_String(Big, SigDec, ExpDec, cStr, Aux)

!** PURPOSE OF THIS SUBROUTINE:
! To set 'Big' with the specified floating point number string and its related information.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt),            INTENT(INOUT)    :: Big      ! a big number (can be 0)
    INTEGER(KIND=I8B),        INTENT(IN)       :: SigDec   ! significand in base 10
    INTEGER(KIND=I4B),        INTENT(INOUT)    :: ExpDec   ! exponent in base 10
    CHARACTER(LEN=*), TARGET, INTENT(IN)       :: cStr     ! floating-point number string
    TYPE(StringAux),          INTENT(IN)       :: Aux      ! auxiliary string information

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: A0       = IACHAR('0')
    INTEGER(KIND=I4B), PARAMETER  :: IBase    = 10

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    IF (.NOT.Aux%Truncated) THEN
! no digit cut, set significant part only
        CALL BigInt_Set_UIntType(Big, SigDec)
        RETURN
    END IF

! some digits were cut, read them from 'SigCut' to 'SigEnd'
    BLOCK
! +++ local variables +++
        INTEGER(KIND=I4B)     :: Header, SigEnd, CurIdx, Length, DigitTotLen, DotPos
        INTEGER(KIND=I8B)     :: CurVal
        LOGICAL    :: DigitCut, HasDot
! +++ execution +++
! initialize and process auxiliary string information needed
        Header = Aux%SigCut
        SigEnd = Aux%Indices(4)
        CurIdx = Header
        Length = 0
        CurVal = 0_I8B
        DigitCut = FalseVal
        HasDot = TrueVal
        DotPos = Aux%Indices(3) - 1
        IF (SigEnd == 0) THEN
            SigEnd = Aux%Indices(2)
            HasDot = FalseVal
            DotPos = 0
        END IF
        DigitTotLen = UIntSafeDigits + (SigEnd - Header)
        IF (HasDot) DigitTotLen = DigitTotLen + 1

        IF (DigitTotLen > MaxDecDigits) THEN
            DigitCut = TrueVal
            SigEnd = SigEnd - (DigitTotLen - (MaxDecDigits + 1))
            IF ((Aux%Indices(3) == Aux%Indices(4))) SigEnd = SigEnd - 1
            DigitTotLen = (MaxDecDigits + 1)
        END IF
        ExpDec = ExpDec - (DigitTotLen - UIntSafeDigits)

! set the truncated significand
        CALL BigInt_Set_UIntType(Big, SigDec)
        IF (HasDot) THEN
! SigCut occurred before encountering the period so we must check
! whether the current position is at the period
            DO WHILE (CurIdx <= SigEnd)
                IF (CurIdx /= DotPos) THEN
                    CurVal = CurVal*IBase + (IACHAR(cStr(CurIdx:CurIdx))-A0)
                    CurIdx = CurIdx + 1
                    Length = Length + 1
                    IF ((CurIdx > SigEnd).AND.(DigitCut)) THEN
! The last digit must be non-zero, set it to '1' for correct rounding.
                        CurVal = CurVal - UMOD(CurVal, 10_I8B) + 1_I8B
                    END IF
                    IF ((Length == UIntSafeDigits).OR.(CurIdx > SigEnd)) THEN
                        CALL Bigint_Mul_Pow10(Big, Length)
                        CALL Bigint_Add_U64(Big, CurVal)
                        CurVal = 0_I8B
                        Length = 0
                    END IF
                ELSE
                    CurIdx = CurIdx + 1
                END IF
            END DO
        ELSE
! SigCut occurred after encountering the period so we do not need to check
! whether the current position is at the period
            DO WHILE (CurIdx <= SigEnd)
                CurVal = CurVal*IBase + (IACHAR(cStr(CurIdx:CurIdx))-A0)
                CurIdx = CurIdx + 1
                Length = Length + 1
                IF ((CurIdx > SigEnd).AND.(DigitCut)) THEN
! The last digit must be non-zero, set it to '1' for correct rounding.
                    CurVal = CurVal - UMOD(CurVal, 10_I8B) + 1_I8B
                END IF
                IF ((Length == UIntSafeDigits).OR.(CurIdx > SigEnd)) THEN
                    CALL Bigint_Mul_Pow10(Big, Length)
                    CALL Bigint_Add_U64(Big, CurVal)
                    CurVal = 0_I8B
                    Length = 0
                END IF
            END DO
        END IF
    END BLOCK

    RETURN

END SUBROUTINE BigInt_Set_String

!******************************************************************************

!------------------------------------------------------------------------------
!
!            PARSING FLOATING-POINT-NUMBER STRING ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION Parse_Fortran_String(cStr, SigDec, ExpDec, NegSign, Aux, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
! To parse a valid Fortran real (floating point) number that has one of the two following forms:
! 1. A number without exponent part -> [S]N[N...]
! 2. A number with exponent part    -> [S]N[N...]E[S]N[N...]
!   Where
!   [ ] indicates an optional field
!   S is a sign indicator (required if negative '-', optional if positive '+').
!   N is a decimal digit (0 through 9). A decimal point may appear anywhere
!       after the sign (but before the exponent).
!   E is an exponent indicator (either 'e' or 'E')
! The valid number is similar to "Real" Fortran constant (literal) with some small differences.
! 1. A whole number without a decimal point (i.e. "Integer" constant) is considered valid.
! 2. The optional kind parameter (_k) is not allowed here.
!
! Note: Leading and/or trailing space(s) are allowed.  For example, "  1.23"
!   and "1.23   " are considered valid.  However, no space is allowed inside
!   the supposedly valid number.  For instance, "1 .2 3" is considered NOT valid.
!   Therefore, this routine is not totally compatible with Fortran READ statement
!   where spaces inside the valid number are allowed.
!   However, this can easily be done by adding an optional 'Inside Space' flag that
!   provide an interpretation of the spaces as 'zero' or 'ignored'.  Then, the input
!   will be pre-processed according to the flag.  Nonetheless, this routine neglects
!   this optional input because it will make the routine much less efficient due to
!   the fact that we will need to scan the whole string twice and we will also need
!   to copy the input string into a buffer string and working with the buffer instead
!   of directly handling the input string.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)    :: cStr
    INTEGER(KIND=I8B),                       INTENT(OUT)   :: SigDec   ! significand in base 10
    INTEGER(KIND=I4B),                       INTENT(OUT)   :: ExpDec   ! exponent in base 10
    LOGICAL,                                 INTENT(OUT)   :: NegSign
    TYPE(StringAux),                         INTENT(OUT)   :: Aux
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    LOGICAL                                                :: Valid

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: A0       = IACHAR('0')
    INTEGER(KIND=I4B), PARAMETER  :: IBase    = 10
    INTEGER(KIND=I4B), PARAMETER  :: ExpLimit = INT(Z'10000000', KIND=I4B)
    INTEGER(KIND=I4B), PARAMETER  :: FP_Max_Digits = 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)           :: SigLimit
    INTEGER(KIND=I4B)           :: Indx, StrLen
    INTEGER(KIND=I4B)           :: NFrac
    INTEGER(KIND=I4B)           :: ESign
    INTEGER(KIND=I4B)           :: SigCount
    INTEGER(KIND=I4B)           :: IntegralStart, IntegralEnd
    INTEGER(KIND=I4B)           :: FractionStart, FractionEnd
    CHARACTER(LEN=1), POINTER   :: CurChr
    LOGICAL                     :: AtLeastOneDigit, Truncated

!** FLOW

! initialize
    SigDec = 0_I8B
    ExpDec = 0
    Valid = FalseVal
    AtLeastOneDigit = FalseVal
    Truncated = FalseVal
    StrLen = LEN_TRIM(cStr)     ! get valid string length by removing the trailing space(s)
    Aux%Truncated = Truncated
    Aux%Indices   = 0
    Aux%Start     = 0
    Aux%SigCut    = 0
    IF (StrLen == 0) THEN
        IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
        RETURN
    END IF

! check whether there are spaces in front of the number
! (only allow space(s) in front of the number but no spaces inside it)
    Indx = 1
    IF (cStr(Indx:Indx) == ' ') THEN
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= ' ') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
            RETURN
        END IF
    END IF

! check for sign of the significand
    NegSign = FalseVal
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') NegSign = TrueVal
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this string only contains a sign without a digit.'
            RETURN
        END IF
! check whether the following character is a digit or a dot
        CurChr => cStr(Indx:Indx)
        IF (((CurChr < '0').OR.(CurChr > '9')).AND.(CurChr /= '.')) THEN
! current character is neither a digit nor a dot
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: a sign must be followed by a digit or the dot.'
            RETURN
        END IF
    END IF

    Aux%Start = Indx

! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
        AtLeastOneDigit = TrueVal
! the current digit is zero so loop through the following
! characters until a non-zero character is found
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= '0') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
! only zero digits encountered
            Valid = TrueVal
            RETURN
        END IF
    END IF

! compute for the significand in the integral part
    IntegralStart = 0
    IntegralEnd   = 0
    SigCount      = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        AtLeastOneDigit = TrueVal
        IntegralStart = Indx
        DO WHILE (Indx <= StrLen)
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
        END DO
        SigCount = Indx - IntegralStart
        IntegralEnd = Indx - 1
    END IF

! check whether the current character is a dot
    FractionStart = 0
    FractionEnd   = 0
    NFrac         = 0
    IF (Indx <= StrLen) THEN
        IF (cStr(Indx:Indx) == '.') THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                CurChr => cStr(Indx:Indx)
                IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                    AtLeastOneDigit = TrueVal
                    NFrac = Indx
                    IF (SigCount > 0) THEN
                        FractionStart = Indx
! continue computing for the significand
                        DO WHILE (Indx <= StrLen)
                            CurChr => cStr(Indx:Indx)
                            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
                            Indx = Indx + 1
                        END DO
                        NFrac = Indx - NFrac
                        SigCount = SigCount + NFrac
                        FractionEnd = Indx - 1
                    ELSE
! check for leading zero(s)
                        IF (cStr(Indx:Indx) == '0') THEN
                            DO WHILE (Indx <= StrLen)
                                IF (cStr(Indx:Indx) /= '0') EXIT
                                Indx = Indx + 1
                            END DO
                            IF (Indx > StrLen) THEN
! only zero digits encountered
                                Valid = TrueVal
                                RETURN
                            END IF
                        END IF
                        CurChr => cStr(Indx:Indx)
                        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
                            FractionStart = Indx
! start computing for the significand
                            DO WHILE (Indx <= StrLen)
                                CurChr => cStr(Indx:Indx)
                                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                                SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
                                Indx = Indx + 1
                                SigCount = SigCount + 1
                            END DO
                            NFrac = Indx - NFrac
                            FractionEnd = Indx - 1
                        END IF
                    END IF
                END IF
            END IF
        END IF
    END IF

! done for the significand part so check the number of significant digits
! (there must be at least one significant digit)
    IF (SigCount == 0) THEN
        IF (AtLeastOneDigit) THEN
            Valid = TrueVal
        ELSE
! this happens when not a number is encountered (i.e. the first non-blank character
! is not a sign, a digit or a period)
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid input: the first non-blank character is not a sign, a digit or a period.'
        END IF
        RETURN
    END IF

    ESign = 1
! check whether the current character is an exponent indicator
    IF (Indx <= StrLen) THEN
        CurChr => cStr(Indx:Indx)
        IF (.NOT.((CurChr == 'e').OR.(CurChr == 'E'))) THEN
! the current character is NOT an exponent indicator
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: invalid character after a digit.'
            RETURN
        END IF
        Indx = Indx + 1
        IF (Indx <= StrLen) THEN
            CurChr => cStr(Indx:Indx)
            IF ((CurChr == '+').OR.(CurChr == '-')) THEN
                IF (CurChr == '-') ESign = -1
                Indx = Indx + 1
                IF (Indx > StrLen) THEN
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent+sign indicators.'
                    RETURN
                END IF
! check whether the following character is a digit
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
! current character is not a digit
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent+sign indicators.'
                    RETURN
                END IF
            ELSE
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
! current character is not a digit
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent indicator.'
                    RETURN
                END IF
            END IF
! here the current character is a digit so this is likely a valid number
            ExpDec = (IACHAR(CurChr)-A0)
            DO
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
! there is a non-integer character after the exponent indicator
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: not a digit after the exponent(+sign) indicator(s).'
                    RETURN
                END IF
                ExpDec = ExpDec*IBase + (IACHAR(CurChr)-A0)
                IF (ExpDec > ExpLimit) EXIT
            END DO
        END IF
    END IF

! check number of significant digits
    IF (SigCount > FP_Max_Digits) THEN
! the input string have more digits than 'SigDec' can normally handle so
! start again this time and avoid overflow
        SigDec = 0_I8B
        SigLimit = MaxDivbyBase - 10
        IF (IntegralStart > 0) THEN
            Indx = IntegralStart
            DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= IntegralEnd))
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF
        IF (SigDec .UGE. SigLimit) THEN
! We have a big integer (but we can handle it) so determine exponent
            ExpDec = IntegralEnd + 1 - Indx + ESign*ExpDec
        ELSE
! We may have a value with a fractional component.
            IF (FractionStart > 0) THEN
                Indx = FractionStart
                DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= FractionEnd))
                    SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                    Indx = Indx + 1
                END DO
            END IF
! determine exponent
            ExpDec = FractionStart - Indx + ESign*ExpDec
        END IF
! We have now corrected both exponent and significand, to a truncated value
        Truncated  = TrueVal
        Aux%SigCut = Indx
    ELSE
! determine exponent
        ExpDec = ESign*ExpDec - NFrac
    END IF

! set output
    Valid = TrueVal
    Aux%Truncated  = Truncated
    Aux%Indices(1) = IntegralStart
    Aux%Indices(2) = IntegralEnd
    Aux%Indices(3) = FractionStart
    Aux%Indices(4) = FractionEnd
    IF (PRESENT(ErrMsg)) ErrMsg = 'Valid Input: no error encountered.'

    RETURN

END FUNCTION Parse_Fortran_String

!******************************************************************************

FUNCTION Parse_JSON_String(cStr, SigDec, ExpDec, NegSign, Aux, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
! To parse a valid JSON real (floating point) number where its differences
! from Fortran number are as follows:
!   1. leading and trailing spaces are not allowed.
!   2. a plus sign as the first character is not allowed.
!   3. leading zero(s) is not allowed (if 0 is the first character, the second one
!      must either be a period or an exponent indicator.)
!   4. a period must be followed by at least one digit.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)    :: cStr
    INTEGER(KIND=I8B),                       INTENT(OUT)   :: SigDec   ! significand in base 10
    INTEGER(KIND=I4B),                       INTENT(OUT)   :: ExpDec   ! exponent in base 10
    LOGICAL,                                 INTENT(OUT)   :: NegSign
    TYPE(StringAux),                         INTENT(OUT)   :: Aux
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    LOGICAL                                                :: Valid

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: A0       = IACHAR('0')
    INTEGER(KIND=I4B), PARAMETER  :: IBase    = 10
    INTEGER(KIND=I4B), PARAMETER  :: ExpLimit = INT(Z'10000000', KIND=I4B)
    INTEGER(KIND=I4B), PARAMETER  :: FP_Max_Digits = 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)           :: SigLimit
    INTEGER(KIND=I4B)           :: Indx, StrLen
    INTEGER(KIND=I4B)           :: NFrac
    INTEGER(KIND=I4B)           :: ESign
    INTEGER(KIND=I4B)           :: SigCount
    INTEGER(KIND=I4B)           :: IntegralStart, IntegralEnd
    INTEGER(KIND=I4B)           :: FractionStart, FractionEnd
    CHARACTER(LEN=1), POINTER   :: CurChr
    LOGICAL                     :: Truncated

!** FLOW

! initialize
    SigDec = 0_I8B
    ExpDec = 0
    Valid = FalseVal
    Truncated = FalseVal
    StrLen = LEN(cStr)
    Aux%Truncated = Truncated
    Aux%Indices   = 0
    Aux%Start     = 0
    Aux%SigCut    = 0
    IF (StrLen == 0) THEN
        IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
        RETURN
    END IF

! check for sign of the significand
    NegSign = FalseVal
    Indx = 1
    IF (cStr(Indx:Indx) == '-') THEN
        NegSign = TrueVal
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this string only contains a sign without a digit.'
            RETURN
        END IF
! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
! current character is not a digit
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: a sign must be followed by a digit.'
            RETURN
        END IF
    END IF

    Aux%Start = Indx
! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
! the current (leading) digit is zero
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
! only a zero digit encountered
            Valid = TrueVal
            RETURN
        END IF
! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
! leading zero cannot be followed by an integer (i.e. no leading zeros)
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: leading zero(s) is/are not allowed.'
            RETURN
        END IF
        IntegralStart = 0
        IntegralEnd   = 0
        SigCount      = 0
    ELSE
! check whether the current character is a non-zero digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
! must start with an integer
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: a leading character is not a digit.'
            RETURN
        END IF
! compute for the significand in the integral part
        IntegralStart = Indx
        SigDec = IACHAR(cStr(Indx:Indx)) - A0
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
        END DO
        SigCount = Indx - IntegralStart
        IntegralEnd = Indx - 1
    END IF

! check whether the current character is a dot
    FractionStart = 0
    FractionEnd   = 0
    NFrac         = 0
    IF (Indx <= StrLen) THEN
        IF (cStr(Indx:Indx) == '.') THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                CurChr => cStr(Indx:Indx)
                IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                    NFrac = Indx
                    IF (SigCount > 0) THEN
                        FractionStart = Indx
! continue computing for the significand
                        DO WHILE (Indx <= StrLen)
                            CurChr => cStr(Indx:Indx)
                            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
                            Indx = Indx + 1
                        END DO
                        NFrac = Indx - NFrac
                        SigCount = SigCount + NFrac
                        FractionEnd = Indx - 1
                    ELSE
! check for leading zero(s)
                        IF (cStr(Indx:Indx) == '0') THEN
                            DO WHILE (Indx <= StrLen)
                                IF (cStr(Indx:Indx) /= '0') EXIT
                                Indx = Indx + 1
                            END DO
                            IF (Indx > StrLen) THEN
! only zero digits encountered
                                Valid = TrueVal
                                RETURN
                            END IF
                        END IF
                        CurChr => cStr(Indx:Indx)
                        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
                            FractionStart = Indx
! start computing for the significand
                            DO WHILE (Indx <= StrLen)
                                CurChr => cStr(Indx:Indx)
                                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                                SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
                                Indx = Indx + 1
                                SigCount = SigCount + 1
                            END DO
                            NFrac = Indx - NFrac
                            FractionEnd = Indx - 1
                        END IF
                    END IF
                END IF
            END IF
        END IF
    END IF

! done for the significand part so check the number of significant digits
! (there must be at least one significant digit)
    IF (SigCount == 0) THEN
! this should not happen here since the algorithm implemented above should take care of this already?
        IF (PRESENT(ErrMsg)) ErrMsg = 'There must be something wrong with the implementation.'
        RETURN
    END IF

    ESign = 1
! check whether the current character is an exponent indicator
    IF (Indx <= StrLen) THEN
        CurChr => cStr(Indx:Indx)
        IF (.NOT.((CurChr == 'e').OR.(CurChr == 'E'))) THEN
! the current character is NOT an exponent indicator
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: invalid character after a digit.'
            RETURN
        END IF
        Indx = Indx + 1
        IF (Indx <= StrLen) THEN
            CurChr => cStr(Indx:Indx)
            IF ((CurChr == '+').OR.(CurChr == '-')) THEN
                IF (CurChr == '-') ESign = -1
                Indx = Indx + 1
                IF (Indx > StrLen) THEN
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent+sign indicators.'
                    RETURN
                END IF
! check whether the following character is a digit
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
! current character is not a digit
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent+sign indicators.'
                    RETURN
                END IF
            ELSE
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
! current character is not a digit
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent indicator.'
                    RETURN
                END IF
            END IF
! here the current character is a digit so this is likely a valid number
            ExpDec = (IACHAR(CurChr)-A0)
            DO
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
! there is a non-integer character after the exponent indicator
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: not a digit after the exponent(+sign) indicator(s).'
                    RETURN
                END IF
                ExpDec = ExpDec*IBase + (IACHAR(CurChr)-A0)
                IF (ExpDec > ExpLimit) EXIT
            END DO
        END IF
    END IF

! check number of significant digits
    IF (SigCount > FP_Max_Digits) THEN
! the input string have more digits than 'SigDec' can normally handle so
! start again this time and avoid overflow
        SigDec = 0_I8B
        SigLimit = MaxDivbyBase - 10
        IF (IntegralStart > 0) THEN
            Indx = IntegralStart
            DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= IntegralEnd))
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF
        IF (SigDec .UGE. SigLimit) THEN
! We have a big integer (but we can handle it) so determine exponent
            ExpDec = IntegralEnd + 1 - Indx + ESign*ExpDec
        ELSE
! We may have a value with a fractional component.
            IF (FractionStart > 0) THEN
                Indx = FractionStart
                DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= FractionEnd))
                    SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                    Indx = Indx + 1
                END DO
            END IF
! determine exponent
            ExpDec = FractionStart - Indx + ESign*ExpDec
        END IF
! We have now corrected both exponent and significand, to a truncated value
        Truncated  = TrueVal
        Aux%SigCut = Indx
    ELSE
! determine exponent
        ExpDec = ESign*ExpDec - NFrac
    END IF

! set output
    Valid = TrueVal
    Aux%Truncated  = Truncated
    Aux%Indices(1) = IntegralStart
    Aux%Indices(2) = IntegralEnd
    Aux%Indices(3) = FractionStart
    Aux%Indices(4) = FractionEnd
    IF (PRESENT(ErrMsg)) ErrMsg = 'Valid Input: no error encountered.'

    RETURN

END FUNCTION Parse_JSON_String

!******************************************************************************

FUNCTION Parse_FPlus_String(cStr, SigDec, ExpDec, NegSign, Aux, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
! To parse a valid Fortran real (floating point) number with more relaxed rules than
! those used in "Parse_Fortran_Number" routine.
! The relaxed rules consider the following numbers as valid:
!   1. a number expressed in the scientific format can use 'd', 'D', 'q' and 'Q'
!      in place of 'e' or 'E'.
!   2. a number with '+' or '-' after digits (e.g. 1.23-20 or 123+50) is considered to
!      be expressed in a valid number expressed in the scientific format
!   3. digits before any invalid character encountered are treated as a valid number
!      and any characters after the first encounter (including the first invalid one)
!      are neglected.  therefore, for example, a '12.56ax-300' is considered to be
!      a valid number with value of 12.56.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)    :: cStr
    INTEGER(KIND=I8B),                       INTENT(OUT)   :: SigDec   ! significand in base 10
    INTEGER(KIND=I4B),                       INTENT(OUT)   :: ExpDec   ! exponent in base 10
    LOGICAL,                                 INTENT(OUT)   :: NegSign
    TYPE(StringAux),                         INTENT(OUT)   :: Aux
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    LOGICAL                                                :: Valid

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: A0       = IACHAR('0')
    INTEGER(KIND=I4B), PARAMETER  :: IBase    = 10
    INTEGER(KIND=I4B), PARAMETER  :: ExpLimit = INT(Z'10000000', KIND=I4B)
    INTEGER(KIND=I4B), PARAMETER  :: FP_Max_Digits = 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)           :: SigLimit
    INTEGER(KIND=I4B)           :: Indx, StrLen
    INTEGER(KIND=I4B)           :: NFrac
    INTEGER(KIND=I4B)           :: ESign
    INTEGER(KIND=I4B)           :: SigCount
    INTEGER(KIND=I4B)           :: IntegralStart, IntegralEnd
    INTEGER(KIND=I4B)           :: FractionStart, FractionEnd
    CHARACTER(LEN=1), POINTER   :: CurChr
    LOGICAL                     :: AtLeastOneDigit, Truncated

!** FLOW

! initialize
    SigDec = 0_I8B
    ExpDec = 0
    Valid = FalseVal
    AtLeastOneDigit = FalseVal
    Truncated = FalseVal
    StrLen = LEN_TRIM(cStr)     ! get valid string length by removing the trailing space(s)
    Aux%Truncated = Truncated
    Aux%Indices   = 0
    Aux%Start     = 0
    Aux%SigCut    = 0
    IF (StrLen == 0) THEN
        IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
        RETURN
    END IF

! check whether there are spaces in front of the number
! (only allow space(s) in front of the number but no spaces inside it)
    Indx = 1
    IF (cStr(Indx:Indx) == ' ') THEN
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= ' ') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
            RETURN
        END IF
    END IF

! check for sign of the significand
    NegSign = FalseVal
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') NegSign = TrueVal
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this string only contains a sign without a digit.'
            RETURN
        END IF
! check whether the following character is a digit or a dot
        CurChr => cStr(Indx:Indx)
        IF (((CurChr < '0').OR.(CurChr > '9')).AND.(CurChr /= '.')) THEN
! current character is neither a digit nor a dot
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: a sign must be followed by a digit or the dot.'
            RETURN
        END IF
    END IF

    Aux%Start = Indx

! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
        AtLeastOneDigit = TrueVal
! the current digit is zero so loop through the following
! characters until a non-zero character is found
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= '0') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
! only zero digits encountered
            Valid = TrueVal
            RETURN
        END IF
    END IF

! compute for the significand in the integral part
    IntegralStart = 0
    IntegralEnd   = 0
    SigCount      = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        AtLeastOneDigit = TrueVal
        IntegralStart = Indx
        DO WHILE (Indx <= StrLen)
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
        END DO
        SigCount = Indx - IntegralStart
        IntegralEnd = Indx - 1
    END IF

! check whether the current character is a dot
    FractionStart = 0
    FractionEnd   = 0
    NFrac         = 0
    IF (Indx <= StrLen) THEN
        IF (cStr(Indx:Indx) == '.') THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                CurChr => cStr(Indx:Indx)
                IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                    AtLeastOneDigit = TrueVal
                    NFrac = Indx
                    IF (SigCount > 0) THEN
                        FractionStart = Indx
! continue computing for the significand
                        DO WHILE (Indx <= StrLen)
                            CurChr => cStr(Indx:Indx)
                            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
                            Indx = Indx + 1
                        END DO
                        NFrac = Indx - NFrac
                        SigCount = SigCount + NFrac
                        FractionEnd = Indx - 1
                    ELSE
! check for leading zero(s)
                        IF (cStr(Indx:Indx) == '0') THEN
                            DO WHILE (Indx <= StrLen)
                                IF (cStr(Indx:Indx) /= '0') EXIT
                                Indx = Indx + 1
                            END DO
                            IF (Indx > StrLen) THEN
! only zero digits encountered
                                Valid = TrueVal
                                RETURN
                            END IF
                        END IF
                        CurChr => cStr(Indx:Indx)
                        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
                            FractionStart = Indx
! start computing for the significand
                            DO WHILE (Indx <= StrLen)
                                CurChr => cStr(Indx:Indx)
                                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                                SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
                                Indx = Indx + 1
                                SigCount = SigCount + 1
                            END DO
                            NFrac = Indx - NFrac
                            FractionEnd = Indx - 1
                        END IF
                    END IF
                END IF
            END IF
        END IF
    END IF

! done for the significand part so check the number of significant digits
! (there must be at least one significant digit)
    IF (SigCount == 0) THEN
        IF (AtLeastOneDigit) THEN
            Valid = TrueVal
        ELSE
! this happens when not a number is encountered (i.e. the first non-blank character
! is not a sign, a digit or a period)
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid input: the first non-blank character is not a sign, a digit or a period.'
        END IF
        RETURN
    END IF

    ESign = 1
! check whether the current character is an exponent indicator
    IF (Indx <= StrLen) THEN
        DO
            SELECT CASE (cStr(Indx:Indx))
            CASE ('e', 'E', 'd', 'D', 'q', 'Q')
                Indx = Indx + 1
! check for a sign of the exponent
                IF (Indx <= StrLen) THEN
                    CurChr => cStr(Indx:Indx)
                    IF (CurChr == '-') THEN
                        ESign = -1
                        Indx = Indx + 1
                    ELSEIF (CurChr == '+') THEN
                        Indx = Indx + 1
                    END IF
                ELSE
                    EXIT
                END IF
            CASE ('-')
                ESign = -1
                Indx = Indx + 1
            CASE ('+')
                Indx = Indx + 1
            CASE DEFAULT
                EXIT
            END SELECT
            IF (Indx <= StrLen) THEN
                DO
                    CurChr => cStr(Indx:Indx)
                    IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                    ExpDec = ExpDec*IBase + (IACHAR(CurChr)-A0)
                    IF (ExpDec > ExpLimit) EXIT
                    Indx = Indx + 1
                    IF (Indx > StrLen) EXIT
                END DO
            END IF
            EXIT
        END DO
    END IF

! check number of significant digits
    IF (SigCount > FP_Max_Digits) THEN
! the input string have more digits than 'SigDec' can normally handle so
! start again this time and avoid overflow
        SigDec = 0_I8B
        SigLimit = MaxDivbyBase - 10
        IF (IntegralStart > 0) THEN
            Indx = IntegralStart
            DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= IntegralEnd))
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF
        IF (SigDec .UGE. SigLimit) THEN
! We have a big integer (but we can handle it) so determine exponent
            ExpDec = IntegralEnd + 1 - Indx + ESign*ExpDec
        ELSE
! We may have a value with a fractional component.
            IF (FractionStart > 0) THEN
                Indx = FractionStart
                DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= FractionEnd))
                    SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                    Indx = Indx + 1
                END DO
            END IF
! determine exponent
            ExpDec = FractionStart - Indx + ESign*ExpDec
        END IF
! We have now corrected both exponent and significand, to a truncated value
        Truncated  = TrueVal
        Aux%SigCut = Indx
    ELSE
! determine exponent
        ExpDec = ESign*ExpDec - NFrac
    END IF

! set output
    Valid = TrueVal
    Aux%Truncated  = Truncated
    Aux%Indices(1) = IntegralStart
    Aux%Indices(2) = IntegralEnd
    Aux%Indices(3) = FractionStart
    Aux%Indices(4) = FractionEnd
    IF (PRESENT(ErrMsg)) ErrMsg = 'Valid Input: no error encountered.'

    RETURN

END FUNCTION Parse_FPlus_String

!******************************************************************************

!------------------------------------------------------------------------------
!
!                       BINARY-TO-DECIMAL CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE Bin2Dec_DragonBox(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)

!** PURPOSE OF THIS SUBROUTINE:
! To convert a binary floating point number into the shortest and correctly
! rounded decimal representation based on the DragonBox algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: SigRaw   ! The raw value of significand in IEEE 754 format
    INTEGER(KIND=I4B), INTENT(IN)   :: ExpRaw   ! The raw value of exponent in IEEE 754 format
    INTEGER(KIND=I8B), INTENT(IN)   :: SigBin   ! The decoded value of significand in binary
    INTEGER(KIND=I4B), INTENT(IN)   :: ExpBin   ! The decoded value of exponent in binary
    INTEGER(KIND=I8B), INTENT(OUT)  :: SigDec   ! The output value of significand in decimal
    INTEGER(KIND=I4B), INTENT(OUT)  :: ExpDec   ! The output value of exponent in decimal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)       :: Pow10
    LOGICAL             :: Include_Left_Endpoint, Include_Right_Endpoint
    INTEGER(KIND=I4B)   :: Minus_K, Beta
    INTEGER(KIND=I4B)   :: DeltaI, R, Dist
    INTEGER(KIND=I8B)   :: Two_Fl, Two_Fc
    INTEGER(KIND=I8B)   :: ZMul_Val
    LOGICAL             :: ZMul_IsInteger
    LOGICAL             :: XMul_IsInteger, XMul_Parity
    LOGICAL             :: YMul_IsInteger, Approx_Y_Parity
    LOGICAL             :: Divisible_By_Small_Divisor

!** FLOW:

! Step 1: integer promotion & Schubfach multiplier calculation.

! Check if normal.
    IF ((ExpRaw /= 0).AND.(SigRaw == 0_I8B)) THEN
        CALL Shorter_Interval_Case(ExpBin, SigDec, ExpDec)
        RETURN
    END IF

    Include_Left_Endpoint  = (IAND(SigBin, 1_I8B) == 0_I8B)
    Include_Right_Endpoint = Include_Left_Endpoint

! Compute K and Beta as well as get cached data
    Minus_K = Floor_Log10_Pow2(ExpBin) - Kappa
    Beta    = ExpBin + Floor_Log2_Pow10(-Minus_K)

    Pow10 = Get_Pow10_128Bits(-Minus_K)
    IF (Minus_K > 0) Pow10%Low = Pow10%Low + 1_I8B

! Compute Zi and Deltai.
! 10**Kappa <= Deltai < 10**(Kappa + 1)
    DeltaI = Compute_Delta(Pow10, Beta)
    Two_Fc = SHIFTL(SigBin, 1)

! For the case of binary32, the result of integer check is not correct for
! 29711844 * 2^-82
! = 6.1442653300000000008655037797566933477355632930994033813476... * 10^-18
! and 29711844 * 2^-81
! = 1.2288530660000000001731007559513386695471126586198806762695... * 10^-17,
! and they are the unique counterexamples. However, since 29711844 is even,
! this does not cause any problem for the endpoints calculations; it can only
! cause a problem when we need to perform integer check for the center.
! Fortunately, with these inputs, that branch is never executed, so we are
! fine.
    CALL Compute_Mul(SHIFTL(IOR(Two_Fc, 1_I8B), Beta), Pow10, ZMul_Val, ZMul_IsInteger)

! Step 2: Try larger divisor; remove trailing zeros if necessary.

! Using an upper bound on zi, we might be able to optimize the division
! better than the compiler; we are computing zi / big_divisor here.
    SigDec = Divide_By_10_To_Kappa_Plus_1(ZMul_Val)
    R = ZMul_Val - Big_Divisor*SigDec   ! implicit conversion if necessary

    IF (R .ULT. DeltaI) THEN
! Exclude the right endpoint if necessary.
        IF ((R == 0).AND.ZMul_IsInteger.AND.(.NOT.Include_Right_Endpoint)) THEN
            SigDec = SigDec - 1_I8B
            R = Big_Divisor
! must perform Step 3
        ELSE
            ExpDec = Minus_K + Kappa + 1
            RETURN
        END IF
    ELSEIF (R == DeltaI) THEN
! r == deltai; compare fractional parts.
        Two_Fl = Two_Fc - 1_I8B
        XMul_Parity = Compute_Mul_Parity(Two_Fl, Pow10, Beta, XMul_IsInteger)
        IF ((.NOT.Include_Left_Endpoint).OR.(ExpBin < Case_Fc_Pm_Half_Lower_Threshold).OR. &
            (ExpBin > Divisibility_Check_By_5_Threshold)) THEN
! If the left endpoint is not included, the condition for
! success is z^(f) < delta^(f) (odd parity).
! Otherwise, the inequalities on exponent ensure that
! x is not an integer, so if z^(f) >= delta^(f) (even parity), we in fact
! have strict inequality.
            IF (XMul_Parity) THEN
                ExpDec = Minus_K + Kappa + 1
                RETURN
            END IF
! must perform Step 3
        ELSE
            IF (XMul_Parity.OR.XMul_IsInteger) THEN
                ExpDec = Minus_K + Kappa + 1
                RETURN
            END IF
! must perform Step 3
        END IF
    ELSE
! must perform Step 3
    END IF

! Step 3: Find the significand with the smaller divisor
    SigDec = SigDec*TenUInt
    ExpDec = Minus_K + Kappa

    Dist = R - SHIFTR(DeltaI, 1) + Half_Small_Divisor
    Approx_Y_Parity = IAND(IEOR(Dist, Half_Small_Divisor), 1) /= 0

    Divisible_By_Small_Divisor = Is_Divisible_By_Pow10(Dist)

! Add dist / 10^kappa to the significand.
    SigDec = SigDec + Dist

! Is dist divisible by 10^kappa?
    IF (Divisible_By_Small_Divisor) THEN
! Check z^(f) >= epsilon^(f).
! We have either yi == zi - epsiloni or yi == (zi - epsiloni) - 1,
! where yi == zi - epsiloni if and only if z^(f) >= epsilon^(f)
! Since there are only 2 possibilities, we only need to care about the
! parity. Also, zi and r should have the same parity since the divisor
! is an even number.
        IF (Compute_Mul_Parity(Two_Fc, Pow10, Beta, YMul_IsInteger) .NEQV. Approx_Y_Parity) THEN
            SigDec = SigDec - 1_I8B
        ELSE
! If z^(f) >= epsilon^(f), we might have a tie
! when z^(f) == epsilon^(f), or equivalently, when y is an integer
            IF (YMul_IsInteger) THEN
                IF (IAND(SigDec, 1_I8B) /= 0_I8B) SigDec = SigDec - 1_I8B
            END IF
        END IF
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Compute_Mul(U, Pow10, ResHi, IsInteger)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute the multiplication of U and Pow10

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B),  INTENT(IN)   :: U
        TYPE(UInt128),      INTENT(IN)   :: Pow10
        INTEGER(KIND=I8B),  INTENT(OUT)  :: ResHi
        LOGICAL,            INTENT(OUT)  :: IsInteger

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)   :: ResLo

    !** FLOW

        CALL UMul192_Upper128(U, Pow10%High, Pow10%Low, ResHi, ResLo)
        IsInteger = ResLo == 0_I8B

        RETURN

    END SUBROUTINE Compute_Mul

!**************************************************************************

    FUNCTION Compute_Mul_Parity(Two_F, Pow10, Beta, IsInteger) RESULT(Parity)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To check multiplication parity

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B),  INTENT(IN)   :: Two_F
        TYPE(UInt128),      INTENT(IN)   :: Pow10
        INTEGER(KIND=I4B),  INTENT(IN)   :: Beta
        LOGICAL,            INTENT(OUT)  :: IsInteger
        LOGICAL                          :: Parity

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: U128Hi, U128Lo

    !** FLOW

        CALL UMul192_Lower128(Two_F, Pow10%High, Pow10%Low, U128Hi, U128Lo)
        Parity = IAND(SHIFTR(U128Hi, (64 - Beta)), 1_I8B) /= 0_I8B
        IsInteger = IOR(SHIFTL(U128Hi, Beta), SHIFTR(U128Lo, (64 - Beta))) == 0_I8B

        RETURN

    END FUNCTION Compute_Mul_Parity

!**************************************************************************

    FUNCTION Is_Divisible_By_Pow10(N) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To replace N by Floor(N / Pow(10, M)) returning true if and only if N is
    ! divisible by pow(10, M).
    ! Precondition: N <= Pow(10, M + 1).
    ! Note: M = Kappa

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(INOUT) :: N
        LOGICAL                           :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

    ! The numbers below are chosen such that:
    !   1. floor(n/d) = floor(nm / 2^k) where d=10 or d=100,
    !   2. nm mod 2^k < m if and only if n is divisible by d,
    ! where m is magic_number, k is shift_amount
    ! and d is divisor.
    !
    ! Item 1 is a common technique of replacing division by a constant with
    ! multiplication, see e.g. "Division by Invariant Integers Using
    ! Multiplication" by Granlund and Montgomery (1994). magic_number (m) is set
    ! to ceil(2^k/d) for large enough k.
    ! The idea for item 2 originates from Schubfach.

        N = N * Magic_Number
        Flag = IAND(N, Comparison_Mask) .ULT. Magic_Number
        N = SHIFTR(N, Info_Shift_Amount)

        RETURN

    END FUNCTION Is_Divisible_By_Pow10

!**************************************************************************

    FUNCTION Divide_By_10_To_Kappa_Plus_1(N) RESULT(M)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute M = Floor(N / 10**(Kappa + 1))

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: N
        INTEGER(KIND=I8B)               :: M

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        M = SHIFTR(UMul128_Upper64(N, DivM), DivS)

        RETURN

    END FUNCTION Divide_By_10_To_Kappa_Plus_1

!**************************************************************************

    FUNCTION Compute_Delta(Pow10, Beta) RESULT(Delta)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute Delta

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(UInt128),      INTENT(IN)    :: Pow10
        INTEGER(KIND=I4B),  INTENT(IN)    :: Beta
        INTEGER(KIND=I4B)                 :: Delta

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        Delta = INT(IAND(SHIFTR(Pow10%High, (64 - 1 - Beta)), INT(Z'00000000FFFFFFFF', KIND=I8B)), KIND=I4B)

        RETURN

    END FUNCTION Compute_Delta

!**************************************************************************

    SUBROUTINE Shorter_Interval_Case(Exponent, SigDec, ExpDec)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To convert a binary floating point number into the decimal representation
    ! for shorter interval case.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)   :: Exponent ! The decoded value of exponent in binary
        INTEGER(KIND=I8B),  INTENT(OUT)  :: SigDec   ! The output value of significand in decimal
        INTEGER(KIND=I4B),  INTENT(OUT)  :: ExpDec   ! The output value of exponent in decimal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: Minus_K, Beta
        INTEGER(KIND=I8B)   :: Xi, Zi
        TYPE(UInt128)       :: Pow10

    !** FLOW:

    ! Compute k and beta
        Minus_K = Floor_Log10_ThreeQuartersPow2(Exponent)
        Beta = Exponent + Floor_Log2_Pow10(-Minus_K)

    ! Compute Xi and Zi
        Pow10 = Get_Pow10_128Bits(-Minus_K)
        IF (Minus_K > 0) Pow10%Low = Pow10%Low + 1_I8B
        Xi = Compute_Left_Endpoint(Pow10, Beta)
        Zi = Compute_Right_Endpoint(Pow10, Beta)

    ! If the left endpoint is not an integer, increase it
        IF (.NOT.Is_Left_Endpoint_Integer(Exponent)) Xi = Xi + 1_I8B

    ! Try bigger divisor
        SigDec = Zi .UDIV. TenUInt

    ! If succeed, remove trailing zeros if necessary and return
        IF (SigDec * TenUInt .UGE. Xi) THEN
            ExpDec = Minus_K + 1
            ExpDec = ExpDec
            RETURN
        END IF

    ! Otherwise, compute the round-up of y
        SigDec = Compute_Round_Up(Pow10, Beta)
        ExpDec = Minus_K

    ! When tie occurs, choose one of them according to the rule
        IF (Exponent >= Shorter_Interval_Tie_Lower_Threshold .AND. &
            Exponent <= Shorter_Interval_Tie_Upper_Threshold) THEN
            IF (IAND(SigDec, 1_I8B) /= 0_I8B) SigDec = SigDec - 1_I8B    ! Round to even.
        ELSEIF (SigDec .ULT. Xi) THEN
            SigDec = SigDec + 1_I8B
        END IF

        RETURN

    END SUBROUTINE Shorter_Interval_Case

!**************************************************************************

    FUNCTION Compute_Left_Endpoint(Pow10, Beta) RESULT(X)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute the left end point (Xi) for the shorter interval case

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(UInt128),      INTENT(IN)  :: Pow10
        INTEGER(KIND=I4B),  INTENT(IN)  :: Beta
        INTEGER(KIND=I8B)               :: X

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        X = SHIFTR((Pow10%High - SHIFTR(Pow10%High, (SignificandBits + 2))), &
                    (TotalBits - SignificandBits - 1 - Beta))

        RETURN

    END FUNCTION Compute_Left_Endpoint

!**************************************************************************

    FUNCTION Compute_Right_Endpoint(Pow10, Beta) RESULT(Z)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute the right end point (Zi) for the shorter interval case

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(UInt128),      INTENT(IN)  :: Pow10
        INTEGER(KIND=I4B),  INTENT(IN)  :: Beta
        INTEGER(KIND=I8B)               :: Z

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        Z = SHIFTR((Pow10%High + SHIFTR(Pow10%High, (SignificandBits + 1))), &
                    (TotalBits - SignificandBits - 1 - Beta))

        RETURN

    END FUNCTION Compute_Right_Endpoint

!**************************************************************************

    FUNCTION Compute_Round_Up(Pow10, Beta) RESULT(Y)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute the rounded-up value of Yi for the shorter interval case

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(UInt128),      INTENT(IN)  :: Pow10
        INTEGER(KIND=I4B),  INTENT(IN)  :: Beta
        INTEGER(KIND=I8B)               :: Y

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        Y = SHIFTR(SHIFTR(Pow10%High, (TotalBits - SignificandBits - 2 - Beta)) + 1_I8B, 1)

        RETURN

    END FUNCTION Compute_Round_Up

!**************************************************************************

    FUNCTION Is_Left_Endpoint_Integer(E) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To chaeck whether the left end point (Xi) is an integer for the shorter interval case

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)  :: E
        LOGICAL                         :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        Flag = ((E >= Case_Shorter_Interval_Left_Endpoint_Lower_Threshold) .AND. &
                (E <= Case_Shorter_Interval_Left_Endpoint_Upper_Threshold))

        RETURN

    END FUNCTION Is_Left_Endpoint_Integer

!**************************************************************************

END SUBROUTINE Bin2Dec_DragonBox

!******************************************************************************

SUBROUTINE Bin2Dec_Ryu(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)

!** PURPOSE OF THIS SUBROUTINE:
! To convert a binary floating point number into the shortest and correctly
! rounded decimal representation based on the Ryu algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: SigRaw   ! The raw value of significand in IEEE 754 format
    INTEGER(KIND=I4B), INTENT(IN)   :: ExpRaw   ! The raw value of exponent in IEEE 754 format
    INTEGER(KIND=I8B), INTENT(IN)   :: SigBin   ! The decoded value of significand in binary
    INTEGER(KIND=I4B), INTENT(IN)   :: ExpBin   ! The decoded value of exponent in binary
    INTEGER(KIND=I8B), INTENT(OUT)  :: SigDec   ! The output value of significand in decimal
    INTEGER(KIND=I4B), INTENT(OUT)  :: ExpDec   ! The output value of exponent in decimal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    REAL(KIND=DP),     PARAMETER  :: Log2Base5 = LOG(2.0_DP)/LOG(5.0_DP)
    INTEGER(KIND=I4B), PARAMETER  :: QLimit    = FLOOR(Log2Base5*BinaryPrecision)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: E2
    INTEGER(KIND=I8B)   :: M2
    LOGICAL             :: Even, AcceptBounds
    INTEGER(KIND=I8B)   :: MV, MP, MM
    INTEGER(KIND=I8B)   :: Vr, Vp, Vm
    INTEGER(KIND=I8B)   :: VrDiv10, VpDiv10, VmDiv10
    INTEGER(KIND=I8B)   :: VrMod10, VmMod10
    INTEGER(KIND=I4B)   :: E10, Q
    LOGICAL             :: VmIsTrailingZeros, VrIsTrailingZeros, DecrementVp
    INTEGER(KIND=I4B)   :: Removed, LastRemovedDigit

!** FLOW

! We subtract 2 in all cases so that the bounds computation has 2 additional bits.
    E2 = ExpBin - 2
    M2 = SigBin

    Even = IAND(M2, 1_I8B) == 0_I8B
    AcceptBounds = Even

! Step 2: Determine the interval of legal decimal representations.
! Implicit bool -> int conversion. True is 1, false is 0.
    MV = M2 * FourUInt
    MP = MV + TwoUInt
! check whether Cb is closer to the lower bound
    IF ((SigRaw == 0_I8B).AND.(ExpRaw > 1)) THEN
! closer to the lower bound; irregular spacing
        MM = MV - 1_I8B
    ELSE
! not closer to the lower bound; regular spacing
        MM = MV - TwoUInt
    END IF

! Step 3: Convert to a decimal power base using 128-bit arithmetic.
    VmIsTrailingZeros = FalseVal
    VrIsTrailingZeros = FalseVal
    DecrementVp = FalseVal

    IF (E2 >= 0) THEN
! We need (Vm, Vr, Vp) = (MM, MV, MP) * 2**E2
! and we need to remove at least Q' = LOG10(2**E2) digits from the
! scaled values Vm, Vr, Vp, i.e. we want to compute
!  (Vm, Vr, Vp) = (MM, MV, MP) * 2**E2 / 10**Q'
!               = (MM, MV, MP) * 2**E2 / 10**(E10)
!               = (MM, MV, MP) * 5**(-E10) / 2**(E10 - E2)
! However, to correctly round the result we need to know the value of
! the last removed digit.  We therefore remove only Q = Q' - 1 digits in
! the first step and make sure that we execute the loop below at least
! once and determine the correct value of the last removed digit.
        Q = Floor_Log10_Pow2(E2)
        IF (E2 > 3) Q = Q - 1       ! == MAX(0, Q' - 1)
        E10 = Q

! Determine whether all the removed digits are 0.
!
! Z(X, E2, Q) = MOD((X * 2**E2), 10**Q) == 0
!             = P10(X * 2**E2) >= Q
!             = MIN(P2(X) + P2(E2), P5(X)) >= Q
!             = P2(X) + E2 >= Q and P5(x) >= Q
!             = P5(X) >= Q
!             = MOD(X, 5**Q) == 0

! QLimit = FLOOR(LOG5(2**BinaryPrecision))
        IF (Q <= QLimit) THEN
! Only one of MP, MV, and MM can be a multiple of 5, if any.
            IF (UMOD(MV, FiveUInt) == 0_I8B) THEN
                VrIsTrailingZeros = Is_Multiple_Of_Pow5(MV, Q - 1)
            ELSEIF (AcceptBounds) THEN
! Same as min(E2 + (~MM & 1), Pow5Factor(MM)) >= Q
! <=> E2 + (~MM & 1) >= Q && Pow5Factor(MM) >= Q
! <=> true && Pow5Factor(MM) >= Q, since E2 >= Q.
                VmIsTrailingZeros = Is_Multiple_Of_Pow5(MM, Q)
            ELSE
! Same as min(E2 + 1, Pow5Factor(MP)) >= Q.
! Vp -= Is_Multiple_Of_Pow5(MP, Q)
                DecrementVp = Is_Multiple_Of_Pow5(MP, Q)
            END IF
        END IF
    ELSE
! We need (Vm, Vr, Vp) = (MM, MV, MP) * 2**E2 / 10**E2
! and we need to remove at least Q' = LOG10(5**-E2) digits from the
! scaled values Vm, Vr, Vp, i.e. we want to compute
!  (Vm, Vr, Vp) = (MM, MV, MP) * 2**E2 / 10**(E2 + Q')
!               = (MM, MV, MP) * 2**E2 / 10**(E10),
!               = (MM, MV, MP) * 5**(-E10) / 2**(E10 - E2)
        Q = Floor_Log10_Pow5(-E2)
        IF (-E2 > 1) Q = Q - 1      ! == MAX(0, Q' - 1)
        E10 = Q + E2

! Determine whether all the removed digits are 0.
!
! Z(X, E2, Q) = MOD((X * 5**-E2), 10**Q) == 0
!             = MIN(P2(X), P5(X) - E2) >= Q
!             = P2(X) >= Q and P5(X) - E2 >= Q
!             = P2(X) >= Q
!             = MOD(X, 2**Q) == 0
        IF (Q <= 1) THEN
! {Vr,Vp,Vm} is trailing zeros if {MV,MP,MM} has at least Q trailing 0 bits.
! MV = 4 M2, so it always has at least two trailing 0 bits.
            VrIsTrailingZeros = TrueVal
            IF (AcceptBounds) THEN
! MM = MV - 1 - MMShift, so it has 1 trailing 0 bit iff MMShift == 1.
                VmIsTrailingZeros = (MM == (MV - TwoUInt))
            ELSE
! MP = MV + 2, so it always has at least one trailing 0 bit.
                DecrementVp = TrueVal
            END IF
        ELSEIF (Q < (TotalBits-1)) THEN ! TODO(ulfjack): Use a tighter bound here.
! We need to compute min(ntz(MV), Pow5Factor(MV) - E2) >= Q-1
! <=> ntz(MV) >= Q-1  &&  Pow5Factor(MV) - E2 >= Q-1
! <=> ntz(MV) >= Q-1    (E2 is negative and -E2 >= Q)
! <=> (MV & ((1 << (Q-1)) - 1)) == 0
! We also need to make sure that the left shift does not overflow.
            VrIsTrailingZeros = Is_Multiple_Of_Pow2(MV, Q)
        END IF
    END IF

    CALL MulPow5DivPow2(MM, MV, MP, -E10, E10-E2, Vm, Vr, Vp)
    IF (DecrementVp) Vp = Vp - 1_I8B

! Step 4: Find the shortest decimal representation in the interval of legal representations.
    Removed = 0
    LastRemovedDigit = 0

    VpDiv10 = Divide_By_Pow10Factor(Vp, 10)
    CALL DivMod_By_Pow10Factor(Vm, 10_I8B, VmDiv10, VmMod10)
    DO WHILE (VpDiv10 .UGT. VmDiv10)
        VmIsTrailingZeros = VmIsTrailingZeros .AND. (VmMod10 == 0_I8B)
        VrIsTrailingZeros = VrIsTrailingZeros .AND. (LastRemovedDigit == 0)
        CALL DivMod_By_Pow10Factor(Vr, 10_I8B, VrDiv10, VrMod10)
        LastRemovedDigit = VrMod10
        Vr = VrDiv10
        Vp = VpDiv10
        VpDiv10 = Divide_By_Pow10Factor(Vp, 10)
        Vm = VmDiv10
        CALL DivMod_By_Pow10Factor(Vm, 10_I8B, VmDiv10, VmMod10)
        Removed = Removed + 1
    END DO

    IF (VmIsTrailingZeros) THEN
        DO WHILE (Mod_By_Pow10Factor(Vm, 10_I8B) == 0_I8B)
            VrIsTrailingZeros = VrIsTrailingZeros .AND. (LastRemovedDigit == 0)
            CALL DivMod_By_Pow10Factor(Vr, 10_I8B, VrDiv10, VrMod10)
            LastRemovedDigit = VrMod10
            Vr = VrDiv10
            Vp = Divide_By_Pow10Factor(Vp, 10)
            Vm = Divide_By_Pow10Factor(Vm, 10)
            Removed = Removed + 1
        END DO
    END IF

    IF (VrIsTrailingZeros.AND.(LastRemovedDigit == 5).AND.(Mod_By_Pow10Factor(Vr, 2_I8B) == 0_I8B)) THEN
! Round even if the exact numbers is .....50..0.
        LastRemovedDigit = 4
    END IF

! We need to take Vr+1 if Vr is outside bounds or we need to round up.
    SigDec = Vr

    IF (((Vr == Vm).AND.((.NOT.AcceptBounds).OR.(.NOT.VmIsTrailingZeros))) &
        .OR.(LastRemovedDigit >= 5)) THEN
        SigDec = SigDec + 1_I8B
    END IF
    ExpDec = E10 + Removed

    RETURN

    CONTAINS

    SUBROUTINE MulPow5DivPow2(U, V, W, E5, E2, A, B, C)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To perform multipy by power of 5 and divide by power of 2

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: U, V, W
        INTEGER(KIND=I4B), INTENT(IN)   :: E5, E2
        INTEGER(KIND=I8B), INTENT(OUT)  :: A, B, C

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: Shift
        INTEGER(KIND=I8B)     :: U64(0:0), V64(0:0), W64(0:0)
        INTEGER(KIND=I8B)     :: Pow5(0:1)    ! in little endian order; most significant byte is 1
        TYPE(UInt128)         :: Pow5_128
        INTEGER(KIND=I8B)     :: A64(0:2), B64(0:2), C64(0:2)

    !** FLOW

        Shift = E2 - (Floor_Log2_Pow5(E5) + 1 - BitsPerPow5)

        Pow5_128 = Get_Pow10_128Bits(E5)
        Pow5(0) = Pow5_128%Low
        Pow5(1) = Pow5_128%High
        IF ((E5 < Pow10_Min_Exact_Exp).OR.(E5 > Pow10_Max_Exact_Exp)) Pow5(0) = Pow5(0) + 1_I8B

        U64(0) = U
        CALL Multiply_N_ShiftRight(U64, 1, Pow5, 2, Shift, A64)
        A = A64(0)

        V64(0) = V
        CALL Multiply_N_ShiftRight(V64, 1, Pow5, 2, Shift, B64)
        B = B64(0)

        W64(0) = W
        CALL Multiply_N_ShiftRight(W64, 1, Pow5, 2, Shift, C64)
        C = C64(0)

        RETURN

    END SUBROUTINE MulPow5DivPow2

!**************************************************************************

    FUNCTION Is_Multiple_Of_Pow5(Value, Exp) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: Is_Multiple_Of_Pow5

    !** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the given value is divisible by 5**Exp

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: Value
        INTEGER(KIND=I4B), INTENT(IN)   :: Exp
        LOGICAL                         :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        IF (Exp <= MaxExp_ModInv5) THEN
            Flag = IsMultipleOfPow5_64Bits(Value, Exp)
        ELSE
            Flag = Pow5Factor_64Bits(Value) .UGE. Exp
        END IF

        RETURN

    END FUNCTION Is_Multiple_Of_Pow5

!**************************************************************************

    FUNCTION Is_Multiple_Of_Pow2(Value, Exp) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: Is_Multiple_Of_Pow2

    !** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the given value is divisible by 2**Exp

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: Value
        INTEGER(KIND=I4B), INTENT(IN)   :: Exp
        LOGICAL                         :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        Flag = IAND(Value, SHIFTL(1_I8B, Exp) - 1_I8B) == 0_I8B

        RETURN

    END FUNCTION Is_Multiple_Of_Pow2

!**************************************************************************

    SUBROUTINE DivMod_By_Pow10Factor(X, Y, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To perform division by 5 or 10 (i.e. Y is 5 or 10).

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)     :: X
        INTEGER(KIND=I8B), INTENT(IN)     :: Y    ! must be 5 or 10
        INTEGER(KIND=I8B), INTENT(OUT)    :: Quotient
        INTEGER(KIND=I8B), INTENT(OUT)    :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I4B), PARAMETER  :: Long_BitSize = BIT_SIZE(1_I8B) ! 64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: Q, R

    !** FLOW

    ! The following algorithm actually can be used for all positive divisor (Y)
        Q = SHIFTL(SHIFTR(X, 1)/Y, 1)
        R = X - Q*Y
        Quotient  = Q + SHIFTR(IOR(R, NOT(R-Y)), (Long_BitSize-1))
    ! Here, 0 <= r < 2 * divisor
    ! (1) When 0 <= r < divisor, the remainder is simply r.
    ! (2) Otherwise the remainder is r - divisor.
    !
    ! In case (1), r - divisor < 0. Applying ~ produces a long with
    ! sign bit 0, so >> produces 0. The returned value is thus r.
    !
    ! In case (2), a similar reasoning shows that >> produces -1,
    ! so the returned value is r - divisor.
        Remainder = R - IAND(SHIFTA(NOT(R-Y), (Long_BitSize-1)), Y)

        RETURN

    END SUBROUTINE DivMod_By_Pow10Factor

!**************************************************************************

    FUNCTION Divide_By_Pow10Factor(X, Y) RESULT(Quotient)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To perform division by 5 or 10.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN) :: X
        INTEGER(KIND=I4B), INTENT(IN) :: Y    ! must be 5 or 10
        INTEGER(KIND=I8B)             :: Quotient

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I4B), PARAMETER  :: Long_BitSize = BIT_SIZE(1_I8B) ! 64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: Q, R

    !** FLOW

    ! The following algorithm actually can be used for all positive divisor (Y)
        Q = SHIFTL(SHIFTR(X, 1)/Y, 1)
        R = X - Q*Y
        Quotient  = Q + SHIFTR(IOR(R, NOT(R-Y)), (Long_BitSize-1))

        RETURN

    END FUNCTION Divide_By_Pow10Factor

!**************************************************************************

    FUNCTION Mod_By_Pow10Factor(X, Y) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulus of 2, 5 or 10 (i.e. Y is 2 or 5 or 10.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN) :: X
        INTEGER(KIND=I8B), INTENT(IN) :: Y    ! must be 2, 5 or 10
        INTEGER(KIND=I8B)             :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I4B), PARAMETER  :: Long_BitSize = BIT_SIZE(1_I8B) ! 64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: Q, R

    !** FLOW

    ! The following algorithm actually can be used for all positive divisor (Y)
        Q = SHIFTL(SHIFTR(X, 1)/Y, 1)
        R = X - Q*Y
    ! Here, 0 <= r < 2 * divisor
    ! (1) When 0 <= r < divisor, the remainder is simply r.
    ! (2) Otherwise the remainder is r - divisor.
    !
    ! In case (1), r - divisor < 0. Applying ~ produces a long with
    ! sign bit 0, so >> produces 0. The returned value is thus r.
    !
    ! In case (2), a similar reasoning shows that >> produces -1,
    ! so the returned value is r - divisor.
        Remainder = R - IAND(SHIFTA(NOT(R-Y), (Long_BitSize-1)), Y)

        RETURN

    END FUNCTION Mod_By_Pow10Factor

!**************************************************************************

    FUNCTION Pow5Factor_64Bits(Value) RESULT(Count)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute the factor of power of 5

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)     :: Value
        INTEGER(KIND=I4B)                 :: Count

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER  :: MInv5 = INT(Z'CCCCCCCCCCCCCCCD', KIND=I8B)  ! 14757395258967641293_I8B
        INTEGER(KIND=I8B), PARAMETER  :: NDiv5 = INT(Z'3333333333333333', KIND=I8B)  ! 3689348814741910323_I8B

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: Dividend

    !** FLOW

        Count = 0
        IF (Value == 0_I8B) RETURN
        Dividend = Value
        DO
            Dividend = Dividend * MInv5
            IF (Dividend .UGT. NDiv5) RETURN
            Count = Count + 1
        END DO

        RETURN

    END FUNCTION Pow5Factor_64Bits

!**************************************************************************

    FUNCTION IsMultipleOfPow5_64Bits(Value, Exp) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the given value is divisible by 5**Exp

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN) :: Value
        INTEGER(KIND=I4B), INTENT(IN) :: Exp
        LOGICAL                       :: Flag

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER  :: ModInv5(0:1,0:MaxExp_ModInv5) = RESHAPE( [     &
                INT(Z'0000000000000001', KIND=I8B), INT(Z'FFFFFFFFFFFFFFFF', KIND=I8B), &
                INT(Z'CCCCCCCCCCCCCCCD', KIND=I8B), INT(Z'3333333333333333', KIND=I8B), &
                INT(Z'8F5C28F5C28F5C29', KIND=I8B), INT(Z'0A3D70A3D70A3D70', KIND=I8B), &
                INT(Z'1CAC083126E978D5', KIND=I8B), INT(Z'020C49BA5E353F7C', KIND=I8B), &
                INT(Z'D288CE703AFB7E91', KIND=I8B), INT(Z'0068DB8BAC710CB2', KIND=I8B), &
                INT(Z'5D4E8FB00BCBE61D', KIND=I8B), INT(Z'0014F8B588E368F0', KIND=I8B), &
                INT(Z'790FB65668C26139', KIND=I8B), INT(Z'000431BDE82D7B63', KIND=I8B), &
                INT(Z'E5032477AE8D46A5', KIND=I8B), INT(Z'0000D6BF94D5E57A', KIND=I8B), &
                INT(Z'C767074B22E90E21', KIND=I8B), INT(Z'00002AF31DC46118', KIND=I8B), &
                INT(Z'8E47CE423A2E9C6D', KIND=I8B), INT(Z'0000089705F4136B', KIND=I8B), &
                INT(Z'4FA7F60D3ED61F49', KIND=I8B), INT(Z'000001B7CDFD9D7B', KIND=I8B), &
                INT(Z'0FEE64690C913975', KIND=I8B), INT(Z'00000057F5FF85E5', KIND=I8B), &
                INT(Z'3662E0E1CF503EB1', KIND=I8B), INT(Z'000000119799812D', KIND=I8B), &
                INT(Z'A47A2CF9F6433FBD', KIND=I8B), INT(Z'0000000384B84D09', KIND=I8B), &
                INT(Z'54186F653140A659', KIND=I8B), INT(Z'00000000B424DC35', KIND=I8B), &
                INT(Z'7738164770402145', KIND=I8B), INT(Z'0000000024075F3D', KIND=I8B), &
                INT(Z'E4A4D1417CD9A041', KIND=I8B), INT(Z'000000000734ACA5', KIND=I8B), &
                INT(Z'C75429D9E5C5200D', KIND=I8B), INT(Z'000000000170EF54', KIND=I8B), &
                INT(Z'C1773B91FAC10669', KIND=I8B), INT(Z'000000000049C977', KIND=I8B), &
                INT(Z'26B172506559CE15', KIND=I8B), INT(Z'00000000000EC1E4', KIND=I8B), &
                INT(Z'D489E3A9ADDEC2D1', KIND=I8B), INT(Z'000000000002F394', KIND=I8B), &
                INT(Z'90E860BB892C8D5D', KIND=I8B), INT(Z'000000000000971D', KIND=I8B), &
                INT(Z'502E79BF1B6F4F79', KIND=I8B), INT(Z'0000000000001E39', KIND=I8B), &
                INT(Z'DCD618596BE30FE5', KIND=I8B), INT(Z'000000000000060B', KIND=I8B), &
                INT(Z'2C2AD1AB7BFA3661', KIND=I8B), INT(Z'0000000000000135', KIND=I8B), &
                INT(Z'08D55D224BFED7AD', KIND=I8B), INT(Z'000000000000003D', KIND=I8B), &
                INT(Z'01C445D3A8CC9189', KIND=I8B), INT(Z'000000000000000C', KIND=I8B), &
                INT(Z'CD27412A54F5B6B5', KIND=I8B), INT(Z'0000000000000002', KIND=I8B)], [2,28])

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: Cache(0:1)
        INTEGER(KIND=I8B)     :: ModInverse, MaxQuotient

    !** FLOW

        Cache = ModInv5(:,Exp)
        ModInverse  = Cache(0)
        MaxQuotient = Cache(1)
        Flag = Value*ModInverse .ULE. MaxQuotient

        RETURN

    END FUNCTION IsMultipleOfPow5_64Bits

!**************************************************************************

END SUBROUTINE Bin2Dec_Ryu

!******************************************************************************

SUBROUTINE Bin2Dec_Schubfach(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)

!** PURPOSE OF THIS SUBROUTINE:
! To convert a binary floating point number into the shortest and correctly
! rounded decimal representation based on the Schubfach algorithm.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: SigRaw   ! The raw value of significand in IEEE 754 format
    INTEGER(KIND=I4B), INTENT(IN)   :: ExpRaw   ! The raw value of exponent in IEEE 754 format
    INTEGER(KIND=I8B), INTENT(IN)   :: SigBin   ! The decoded value of significand in binary
    INTEGER(KIND=I4B), INTENT(IN)   :: ExpBin   ! The decoded value of exponent in binary
    INTEGER(KIND=I8B), INTENT(OUT)  :: SigDec   ! The output value of significand in decimal
    INTEGER(KIND=I4B), INTENT(OUT)  :: ExpDec   ! The output value of exponent in decimal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)       :: Pow10
    INTEGER(KIND=I8B)   :: Cb, Cbl, Cbr, Vb, Vbl, Vbr
    INTEGER(KIND=I4B)   :: kExp, hExp, Exp10
    LOGICAL             :: uInside, wInside
    INTEGER(KIND=I8B)   :: Sx, Sx4, Sp
    INTEGER(KIND=I8B)   :: Upper, Lower, Middle

!** FLOW:

    Cb  = SHIFTL(SigBin, 2)
    Cbr = Cb + TwoUInt

! check whether Cb is closer to the lower bound
    IF ((SigRaw == 0_I8B).AND.(ExpRaw > 1)) THEN
! closer to the lower bound; irregular spacing
        Cbl = Cb - 1_I8B
        kExp = Floor_Log10_ThreeQuartersPow2(ExpBin)
    ELSE
! not closer to the lower bound; regular spacing
        Cbl = Cb - TwoUInt
        kExp = Floor_Log10_Pow2(ExpBin)
    END IF

! compute Exp10 and shift
    Exp10 = -kExp
    hExp  = ExpBin + Floor_Log2_Pow10(Exp10) + 1

! get the cached pow10 value from Pow10_Sig_Table or compute it
    Pow10 = Get_Pow10_128Bits(Exp10)
    IF ((Exp10 < Pow10_Min_Exact_Exp).OR.(Exp10 > Pow10_Max_Exact_Exp)) THEN
        Pow10%Low = Pow10%Low + 1_I8B
    END IF

! To perform integer multiplications and get upper bits of rounded values
    Vbl = Round2Odd(Pow10, SHIFTL(Cbl,  hExp))
    Vb  = Round2Odd(Pow10, SHIFTL(Cb,   hExp))
    Vbr = Round2Odd(Pow10, SHIFTL(Cbr,  hExp))

    IF (IAND(SigBin, 1_I8B) == 0_I8B) THEN
        Lower = Vbl
        Upper = Vbr
    ELSE
        Lower = Vbl + 1_I8B
        Upper = Vbr - 1_I8B
    END IF

    Sx = SHIFTR(Vb, 2)  ! Sx = Vb / 4
    IF (Sx .UGE. TenUInt) THEN
        ! Vb >= 40
        Sp  = Sx .UDIV. TenUInt     ! Vb / 40
        Sx4 = FortyUInt * Sp
        uInside = (Lower .ULE. Sx4)
        wInside = (Upper .UGE. (Sx4 + FortyUInt))
        IF (uInside .NEQV. wInside) THEN
            IF (wInside) THEN
                SigDec = Sp + 1_I8B
            ELSE
                SigDec = Sp
            END IF
            ExpDec = kExp + 1
            RETURN
        END IF
    END IF

    Sx4 = SHIFTL(Sx, 2)
    uInside = (Lower .ULE. Sx4)
    wInside = (Upper .UGE. (Sx4 + FourUInt))

    ExpDec  = kExp
    SigDec  = Sx
    IF (uInside .NEQV. wInside) THEN
        IF (wInside) SigDec = SigDec + 1_I8B
        RETURN
    END IF

    Middle  = Sx4 + TwoUInt
    IF ((Vb .UGT. Middle).OR.((Vb == Middle).AND.(IAND(Sx, 1_I8B) /= 0_I8B))) THEN
        SigDec = SigDec + 1_I8B
    END IF

    RETURN

    CONTAINS

    FUNCTION Round2Odd(G, Cx) RESULT(Vx)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To perform the rounding of input

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(UInt128),      INTENT(IN)    :: G
        INTEGER(KIND=I8B),  INTENT(IN)    :: Cx
        INTEGER(KIND=I8B)                 :: Vx

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B) :: X_Hi, Y_Lo

    !** FLOW

    ! perform Cp * G%Lo and get the upper 64 bits of the result
        X_Hi = UMul128_Upper64(Cx, G%Low)

    ! perform Cp * G%Hi + X_Hi and return Vx as the upper 64 bits of the result
        CALL UMul128_N_AddU64(Cx, G%High, X_Hi, Vx, Y_Lo)
        IF (Y_Lo .UGT. 1_I8B)  Vx = IOR(Vx, 1_I8B)

        RETURN

    END FUNCTION Round2Odd

!**************************************************************************

    SUBROUTINE UMul128_N_AddU64(A, B, C, U128Hi, U128Lo)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To multiply two 64-bit unsigned integers and add a 64-bit unsigned integer
    ! (A*B + C), and then return the 128-bit result as U128Hi, U128Lo.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)     :: A, B, C
        INTEGER(KIND=I8B), INTENT(OUT)    :: U128Hi, U128Lo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: H, L, Carry

    !** FLOW

    ! multiply A and B
        CALL UMul128(A, B, H, L)

    ! add carry
        Carry = 0_I8B
        CALL AddU64_WithCarry(L, C, Carry, U128Lo)
        U128Hi = H + Carry

        RETURN

    END SUBROUTINE UMul128_N_AddU64

!**************************************************************************

    SUBROUTINE AddU64_WithCarry(X, Y, Carry, Sum)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To return the sum with carry of X, Y and CarryIn: Sum = X + Y + CarryIn.
    ! The carry input must be 0 or 1; otherwise the behavior is undefined.
    ! The carry output is guaranteed to be 0 or 1.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)     :: X, Y
        INTEGER(KIND=I8B), INTENT(INOUT)  :: Carry
        INTEGER(KIND=I8B), INTENT(OUT)    :: Sum

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

	    Sum = X + Y + Carry
    ! The sum will overflow if both top bits are set (x & y) or if one of them
    ! is (x | y), and a carry from the lower place happened. If such a carry
    ! happens, the top bit will be 1 + 0 + 1 = 0 (&^ sum).
	    Carry = SHIFTR(IOR(IAND(X, Y), IAND(IOR(X, Y), NOT(Sum))), 63)

        RETURN

    END SUBROUTINE AddU64_WithCarry

!**************************************************************************

END SUBROUTINE Bin2Dec_Schubfach

!******************************************************************************

!------------------------------------------------------------------------------
!
!                       DECIMAL-TO-BINARY CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION Dec2Bin_Clinger(SigDec, ExpDec, SigBin, ExpBin) RESULT(Valid)

! To convert decimal floating point representation into its exact
! binary floating point representation using the Clinger algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: SigDec   ! significand in base 10
    INTEGER(KIND=I4B), INTENT(IN)   :: ExpDec   ! exponent in base 10
    INTEGER(KIND=I8B), INTENT(OUT)  :: SigBin   ! significand in base 2
    INTEGER(KIND=I4B), INTENT(OUT)  :: ExpBin   ! exponent in base 2
    LOGICAL                         :: Valid    ! true if conversion can be done

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: RawFP
    INTEGER(KIND=I4B)   :: Exp10
    REAL(KIND=DP)       :: FloatMantissa

!** FLOW

    IF (SHIFTR(SigDec, SignificandBits) /= 0_I8B) THEN
        Valid = FalseVal
        RETURN
    END IF

    FloatMantissa = REAL(SigDec, KIND=DP)
    Exp10 = ExpDec

    IF (Exp10 == 0) THEN
        RawFP = RawFP_FromFloat(FloatMantissa)
    END IF
    IF (Exp10 > 0) THEN
        IF (Exp10 > Num_Exact_Pow10 + Num_Mantissa_Digits) THEN
            Valid = FalseVal
            RETURN
        END IF
        IF (Exp10 > Num_Exact_Pow10) THEN
            FloatMantissa = FloatMantissa * Powers_Of_Ten(Exp10 - Num_Exact_Pow10)
            Exp10 = Num_Exact_Pow10
        END IF
        IF (FloatMantissa > Max_Exact_Integer) THEN
            Valid = FalseVal
            RETURN
        END  IF
        RawFP = RawFP_FromFloat(FloatMantissa * Powers_Of_Ten(Exp10))
    ELSEIF (Exp10 < 0) THEN
        IF (-Exp10 > Num_Exact_Pow10) THEN
            Valid = FalseVal
            RETURN
        END IF
        RawFP = RawFP_FromFloat(FloatMantissa / Powers_Of_Ten(-Exp10))
    END IF

    SigBin = RawFP_Significand(RawFP)
    ExpBin = RawFP_BiasedExponent(RawFP)
    Valid = TrueVal

    RETURN

END FUNCTION Dec2Bin_Clinger

!******************************************************************************

SUBROUTINE Dec2Bin_LibC(SigDec, ExpDec, cStr, Start, Truncated, SigBin, ExpBin)

!** PURPOSE OF THIS SUBROUTINE:
! To use LibC algorithm to convert string to real number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: SigDec
    INTEGER(KIND=I4B), INTENT(IN)   :: ExpDec
    CHARACTER(LEN=*),  INTENT(IN)   :: cStr
    INTEGER(KIND=I4B), INTENT(IN)   :: Start
    LOGICAL,           INTENT(IN)   :: Truncated
    INTEGER(KIND=I8B), INTENT(OUT)  :: SigBin
    INTEGER(KIND=I4B), INTENT(OUT)  :: ExpBin

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: FirstSigBin
    INTEGER(KIND=I4B)   :: FirstExpBin

!** FLOW

! try the Eisel-Lemire's algorithm
    IF (Eisel_Lemire(SigDec, ExpDec, SigBin, ExpBin)) THEN
! the Eisel-Lemire's algorithm is possibly valid
        IF (.NOT.Truncated) RETURN
! If the mantissa is truncated, then the result may be off by the LSB, so
! check if rounding the mantissa up changes the result. If not, then it's
! safe, else use the fallback.
        FirstSigBin = SigBin
        FirstExpBin = ExpBin
        IF (Eisel_Lemire(SigDec + 1_I8B, ExpDec, SigBin, ExpBin)) THEN
! check if the Eisel-Lemire's algorithm is definitely valid
            IF ((SigBin == FirstSigBin).AND.(ExpBin == FirstExpBin)) RETURN
        END IF
    END IF

! use the slow Simple Decimal Conversion algorithm
    CALL Simple_Decimal_Conversion(cStr, Start, LEN_TRIM(cStr), SigBin, ExpBin)

    RETURN

    CONTAINS

    FUNCTION Eisel_Lemire(SigDec, ExpDec, SigBin, ExpBin) RESULT(Valid)

    ! To convert decimal floating point representation into its closest
    ! binary floating point representation using the Eisel-Lemire algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)     :: SigDec   ! significand in base 10
        INTEGER(KIND=I4B), INTENT(IN)     :: ExpDec   ! exponent in base 10
        INTEGER(KIND=I8B), INTENT(OUT)    :: SigBin   ! significand in base 2
        INTEGER(KIND=I4B), INTENT(OUT)    :: ExpBin   ! exponent in base 2
        LOGICAL                           :: Valid    ! true if conversion can be done

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: Exp2
        INTEGER(KIND=I4B)     :: CLZ
        INTEGER(KIND=I8B)     :: HighU64, Mantissa
        TYPE(UInt128)         :: PowTen
        TYPE(UInt128)         :: FirstApprox, FinalApprox, LowBits, SecondApprox
        INTEGER(KIND=I8B)     :: FinalMantissa
        INTEGER(KIND=I4B)     :: MSB

    !** FLOW

    ! normalization
        CLZ = LEADZ(SigDec)
        Mantissa = SHIFTL(SigDec, CLZ)

        Exp2 = Floor_Log2_Pow10(ExpDec) + TotalBits + ExponentBias - CLZ

    ! multiplication
        PowTen = Get_Pow10_128Bits(ExpDec)
        FirstApprox = UInt128(0_I8B, Mantissa) * PowTen%High

    ! Wider Approximation
        IF ((IAND(FirstApprox%High, HalfWay) == HalfWay).AND. &
            (FirstApprox%Low + Mantissa .ULT. Mantissa)) THEN
            LowBits = UInt128(0_I8B, Mantissa) * PowTen%Low
            SecondApprox = FirstApprox + UInt128(0_I8B, LowBits%High)
            IF ((IAND(SecondApprox%High, HalfWay) == HalfWay).AND. &
                (SecondApprox%Low + 1_I8B == 0_I8B).AND. &
                (LowBits%Low + Mantissa .ULT. Mantissa)) THEN
                Valid = FalseVal
                RETURN
            END IF
            FinalApprox = SecondApprox
        ELSE
            FinalApprox = FirstApprox
        END IF

    ! Shifting to 54 bits for doubles
        HighU64 = FinalApprox%High
        MSB = INT(SHIFTR(HighU64, TotalBits - 1), KIND=I4B)
        FinalMantissa = SHIFTR(HighU64, (MSB + TotalBits - (SignificandBits + 3)))
        Exp2 = Exp2 - IEOR(1, MSB)  ! same as NOT(MSB)

    ! Half-way ambiguity
        IF ((FinalApprox%Low == 0_I8B).AND.(IAND(HighU64, HalfWay) == 0_I8B) &
                .AND.(IAND(FinalMantissa, 3_I8B) == 1_I8B)) THEN
            Valid = FalseVal
            RETURN
        END IF

    ! From 54 to 53 bits for doubles
        FinalMantissa = FinalMantissa + IAND(FinalMantissa, 1_I8B)
        FinalMantissa = SHIFTR(FinalMantissa, 1)
        IF (SHIFTR(FinalMantissa, (SignificandBits + 1)) /= 0_I8B) THEN
            FinalMantissa = SHIFTR(FinalMantissa, 1)
            Exp2 = Exp2 + 1
        END IF

    ! check exponent validity
        IF ((Exp2 < 1).OR.(Exp2 > (MaxExponent-1))) THEN
            Valid = FalseVal
            RETURN
        END IF

        SigBin = FinalMantissa      ! implicit conversion if type is mismatch
        ExpBin = Exp2
        Valid = TrueVal

        RETURN

    END FUNCTION Eisel_Lemire

!******************************************************************************

    SUBROUTINE Simple_Decimal_Conversion(cStr, Start, Finish, SigBin, ExpBin)

    ! To convert decimal string into its closest floating point binary representation
    ! using the Simple Decimal Conversion algorithm.
    ! The routine assumes that cStr is a 'VALID' floating point string and
    ! Start is less than Finish where
    !   - Start is the index of the first valid numeric character, and
    !   - Finish is the index of the last valid character (== length of the input
    !     string excluding trailing space(s))

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),  INTENT(IN)   :: cStr
        INTEGER(KIND=I4B), INTENT(IN)   :: Start
        INTEGER(KIND=I4B), INTENT(IN)   :: Finish
        INTEGER(KIND=I8B), INTENT(OUT)  :: SigBin   ! significand in base 2
        INTEGER(KIND=I4B), INTENT(OUT)  :: ExpBin   ! exponent in base 2

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I4B), PARAMETER  :: A0 = IACHAR('0')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(HPDecimal)         :: HP
        INTEGER(KIND=I8B)       :: FinalMantissa
        INTEGER(KIND=I4B)       :: Exp2
        INTEGER(KIND=I4B)       :: ShiftAmount

    !** FLOW

    ! initialize
        Exp2 = 0

    ! construct HPDecimal object
        CALL HP%Construct(cStr, Start, Finish)

        IF (HP%NumDigits == 0) THEN
            SigBin = 0_I8B
            ExpBin = 0
            RETURN
        END IF

    ! If the exponent is too large and can't be represented in this size of
    ! float, return inf.
        IF ((HP%DecimalPoint > 0).AND.(Floor_Log2_Pow10(HP%DecimalPoint-1) > ExponentBias)) THEN
            SigBin = 0_I8B
            ExpBin = MaxExponent
            RETURN
        END IF

    ! If the exponent is too small even for a subnormal, return 0.
        IF ((HP%DecimalPoint < 0).AND. &
            (Floor_Log2_Pow10(-HP%DecimalPoint) > (ExponentBias + SignificandBits))) THEN
            SigBin = 0_I8B
            ExpBin = 0
            RETURN
        END IF

    ! Right shift until the number is smaller than 1.
        DO WHILE (HP%DecimalPoint > 0)
            ShiftAmount = 0
            IF (HP%DecimalPoint >= Num_Powers_Of_Two) THEN
                ShiftAmount = 60
            ELSE
                ShiftAmount = Powers_Of_Two(HP%DecimalPoint)
            END IF
            Exp2 = Exp2 + ShiftAmount
            CALL HP%Shift(-ShiftAmount)
        END DO

    ! Left shift until the number is between 1/2 and 1
        DO WHILE ((HP%DecimalPoint < 0).OR.((HP%DecimalPoint == 0).AND.(HP%Digits(0) < 5)))
            ShiftAmount = 0
            IF (-HP%DecimalPoint >= Num_Powers_Of_Two) THEN
                ShiftAmount = 60
            ELSEIF (HP%DecimalPoint /= 0) THEN
                ShiftAmount = Powers_Of_Two(-HP%DecimalPoint)
            ELSE
    ! This handles the case of the number being between .1 and .5
                ShiftAmount = 1
            END IF
            Exp2 = Exp2 - ShiftAmount
            CALL HP%Shift(ShiftAmount)
        END DO

    ! Left shift once so that the number is between 1 and 2
        Exp2 = Exp2 - 1
        CALL HP%Shift(1)

    ! Get the biased exponent
        Exp2 = Exp2 + ExponentBias

    ! Handle the exponent being too large (and return inf).
        IF (Exp2 >= MaxExponent) THEN
            SigBin = 0
            ExpBin = MaxExponent
            RETURN
        END IF

    ! Shift left to fill the mantissa
        CALL HP%Shift(SignificandBits)
        CALL HP%RoundToUIntType(FinalMantissa)

    ! Handle subnormals
        IF (Exp2 <= 0) THEN
    ! Shift right until there is a valid exponent
            DO WHILE (Exp2 < 0)
                CALL HP%Shift(-1)
                Exp2 = Exp2 + 1
            END DO
    ! Shift right one more time to compensate for the left shift to get it
    ! between 1 and 2.
            CALL HP%Shift(-1)
            CALL HP%RoundToUIntType(FinalMantissa)

    ! Check if by shifting right we've caused this to round to a normal number.
            IF (SHIFTR(FinalMantissa, SignificandBits) /= 0_I8B) THEN
                Exp2 = Exp2 + 1
            END IF
        END IF

    ! Check if rounding added a bit, and shift down if that's the case.
        IF (FinalMantissa == SHIFTL(ToUnsignedLong(2), SignificandBits)) THEN
            FinalMantissa = SHIFTR(FinalMantissa, 1)
            Exp2 = Exp2 + 1

    ! Check if this rounding causes Exp2 to go out of range and make the result
    ! INF. If this is the case, then finalMantissa and Exp2 are already the
    ! correct values for an INF result.
            IF (Exp2 >= MaxExponent) THEN
    ! report error if applicable
            END IF
        END IF

        IF (Exp2 == 0) THEN
    ! report error if applicable
        END IF

        SigBin = FinalMantissa
        ExpBin = Exp2

        RETURN

    END SUBROUTINE Simple_Decimal_Conversion

!******************************************************************************

END SUBROUTINE Dec2Bin_LibC

!******************************************************************************

SUBROUTINE Dec2Bin_FastFloat(SigDec, ExpDec, cStr, SigCut, Indices, SigBin, ExpBin)

!** PURPOSE OF THIS SUBROUTINE:
! To use FastFloat algorithm to convert string to real number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),   INTENT(IN)   :: SigDec
    INTEGER(KIND=I4B),   INTENT(IN)   :: ExpDec
    CHARACTER(LEN=*),    INTENT(IN)   :: cStr
    LOGICAL,             INTENT(IN)   :: SigCut
    INTEGER(KIND=I4B),   INTENT(IN)   :: Indices(4)
    INTEGER(KIND=I8B),   INTENT(OUT)  :: SigBin
    INTEGER(KIND=I4B),   INTENT(OUT)  :: ExpBin

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: EBase
    INTEGER(KIND=I4B)     :: ECmp
    INTEGER(KIND=I8B)     :: MBase
    INTEGER(KIND=I8B)     :: MCmp

!** FLOW

! compute float
    CALL Compute_Float(ExpDec, SigDec, EBase, MBase)
    IF (SigCut .AND. EBase >= 0) THEN
        CALL Compute_Float(ExpDec, SigDec+OneMant, ECmp, MCmp)
        IF (Is_AdjustedMantissa_NE(EBase, MBase, ECmp, MCmp)) THEN
            CALL Compute_Error(ExpDec, SigDec, EBase, MBase)
        END IF
    END IF

! If we have an invalid power (EBase < 0), then we need to go
! the long way around again. This is very uncommon.
    IF (EBase < 0) THEN
        BLOCK
            TYPE(Parsed_Number_Info)  :: NumInfo
! set NumInfo
            NumInfo%Exp           = ExpDec
            NumInfo%Sig           = SigDec
            NumInfo%IntegralStart = Indices(1)
            NumInfo%IntegralEnd   = Indices(2)
            NumInfo%FractionStart = Indices(3)
            NumInfo%FractionEnd   = Indices(4)
            ECmp = EBase
            MCmp = MBase
! compare digits
            CALL Digit_Comparision(cStr, NumInfo, ECmp, MCmp, EBase, MBase)
        END BLOCK
    END IF

    SigBin = MBase
    ExpBin = EBase

    RETURN

    CONTAINS

    SUBROUTINE Compute_Product_Approximation(Q, W, ProductHi, ProductLo)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute or rather approximate W * 5**Q and return a pair of 64-bit words
    ! approximating the result, with the "high" part corresponding to the most
    ! significant bits and the low part corresponding to the least significant bits.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN) :: Q                    ! exponent
        INTEGER(KIND=I8B), INTENT(IN) :: W                    ! mantissa
        INTEGER(KIND=I8B)             :: ProductHi, ProductLo ! product approximation

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: BitPrecision
        INTEGER(KIND=I8B)     :: PrecisionMask, SecondProductHi
        TYPE(UInt128)         :: Pow10

    !** FLOW

    ! The required precision is Mantissa_Explicit_Bits + 3 because
    ! 1. We need the implicit bit
    ! 2. We need an extra bit for rounding purposes
    ! 3. We might lose a bit due to the "UpperBit" (result too small, requiring a shift)
    ! BitPrecision = 26 for 32-bit number and 55 for 64-bit number
        BitPrecision = Mantissa_Explicit_Bits + 3

    ! compute precision mask
        PrecisionMask = SHIFTR(MaxMant, BitPrecision)

    ! get 128-bit approximation of power of ten (or power of five)
        Pow10 = Get_Pow10_128Bits(Q)

    ! For small values of Q, e.g., Q in [0,27], the product is always exact.
        CALL UMul128(W, Pow10%High, ProductHi, ProductLo)

        IF (IAND(ProductHi, PrecisionMask) == PrecisionMask) THEN
    ! could further guard with  (ProductLo + W < ProductLo)
    ! regarding the second product, we only need the upper bits of the product.
            SecondProductHi = UMul128_Upper64(W, Pow10%Low)
            ProductLo = ProductLo + SecondProductHi
            IF (SecondProductHi .UGT. ProductLo) ProductHi = ProductHi + 1_I8B
        END IF

        RETURN

    END SUBROUTINE Compute_Product_Approximation

!**************************************************************************

    FUNCTION Power(Q) RESULT(E)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute power in base 2 based on the power in base 10

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN) :: Q    ! power in base 10
        INTEGER(KIND=I4B)             :: E    ! power in base 2

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        E = Floor_Log2_Pow10(Q) + MantTotalBits - 1

        RETURN

    END FUNCTION Power

!**************************************************************************

    SUBROUTINE Compute_Error_Scaled(Q, W, LZ, E, M)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To create an adjusted mantissa, biased by the invalid power2
    ! for significant digits already multiplied by 10 ** Q.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)    :: LZ   ! leading zeros in W
        INTEGER(KIND=I4B),  INTENT(IN)    :: Q    ! exponent in base 10
        INTEGER(KIND=I4B),  INTENT(OUT)   :: E    ! exponent in base 2
        INTEGER(KIND=I8B),  INTENT(IN)    :: W    ! significand in base 10
        INTEGER(KIND=I8B),  INTENT(OUT)   :: M    ! adjusted significand in base 2

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: HiLZ, Bias

    !** FLOW

        HiLZ = IEOR(INT(SHIFTR(W, MantTotalBits - 1), KIND=I4B), 1)
        Bias = Mantissa_Explicit_Bits - Minimum_Exponent
        M = SHIFTL(W, HiLZ)
        E = Power(Q) + Bias - HiLZ - LZ - (MantTotalBits-2) + Invalid_AM_Bias

        RETURN

    END SUBROUTINE Compute_Error_Scaled

!**************************************************************************

    SUBROUTINE Compute_Error(Q, W, E, M)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute W * 10 ** Q, without rounding the representation up.
    ! the power2 in the exponent will be adjusted by Invalid_AM_Bias.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)    :: Q    ! exponent in base 10
        INTEGER(KIND=I4B),  INTENT(OUT)   :: E    ! exponent in base 2
        INTEGER(KIND=I8B),  INTENT(IN)    :: W    ! significand in base 10
        INTEGER(KIND=I8B),  INTENT(OUT)   :: M    ! adjusted significand in base 2

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: LZ
        INTEGER(KIND=I8B)     :: LocalW
        INTEGER(KIND=I8B)     :: ProductHi, ProductLo

    !** FLOW

    ! perform normalization
        LZ = LEADZ(W)
        LocalW = SHIFTL(W, LZ)

    ! compute the product approximation
        CALL Compute_Product_Approximation(Q, LocalW, ProductHi, ProductLo)

    ! compute the adjusted mantissa biased by the invalid power2
        CALL Compute_Error_Scaled(Q, ProductHi, LZ, E, M)

        RETURN

    END SUBROUTINE Compute_Error

!**************************************************************************

    SUBROUTINE Compute_Float(Q, W, E, M)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compute W * 10 ** Q
    ! The returned value should be a valid ieee64 number that simply need to be packed.
    ! However, in some very rare cases, the computation will fail. In such cases, we
    ! return an adjusted_mantissa with a negative power of 2: the caller should recompute
    ! in such cases.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)    :: Q    ! exponent in base 10
        INTEGER(KIND=I4B),  INTENT(OUT)   :: E    ! exponent in base 2
        INTEGER(KIND=I8B),  INTENT(IN)    :: W    ! significand in base 10
        INTEGER(KIND=I8B),  INTENT(OUT)   :: M    ! adjusted significand in base 2

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: LZ, UpperBit
        INTEGER(KIND=I8B)     :: LocalW
        INTEGER(KIND=I8B)     :: ProductHi, ProductLo

    !** FLOW

    ! check for special cases (may not be needed since it is taken care of in the caller?)
        IF ((W == 0_I8B) .OR. (Q < Smallest_Power_of_Ten)) THEN
            E = 0
            M = 0_I8B
    ! result should be zero
            RETURN
        END IF
        IF (Q > Largest_Power_of_Ten) THEN
    ! we want to get infinity:
            E = Infinite_Power
            M = 0_I8B
            RETURN
        END IF

    ! At this point in time Q is in [Smallest_Power_of_Ten, Largest_Power_of_Ten].

    ! We want the most significant bit of i to be 1. Shift if needed.
    ! (i.e. perform normalization)
        LZ = LEADZ(W)
        LocalW = SHIFTL(W, LZ)

    ! compute the product approximation
        CALL Compute_Product_Approximation(Q, LocalW, ProductHi, ProductLo)
    ! The computed product is always sufficient.
    ! See mathematical proof in the following reference:
    ! Noble Mushtak, Daniel Lemire, Fast Number Parsing Without Fallback,
    !       Software: Practice and Experience 53 (7), 2023.

    ! Shifting to Mantissa_Explicit_Bits + 2 bits
        UpperBit = INT(SHIFTR(ProductHi, MantTotalBits - 1), KIND=I4B)
        M = SHIFTR(ProductHi, (UpperBit + MantTotalBits - Mantissa_Explicit_Bits - 3))
        E = Power(Q) + UpperBit - LZ - Minimum_Exponent

        IF (E <= 0) THEN    ! we have a subnormal?
    ! Here have that E <= 0 so -E >= 0
            IF (-E + 1 >= MantTotalBits) THEN
    ! if we have more than 'MantTotalBits' bits below the minimum exponent, you have a zero for sure.
                E = 0
                M = 0_I8B
    ! result should be zero
                RETURN
            END IF
    ! next line is safe because -E + 1 < MantTotalBits
            M = SHIFTR(M, -E + 1)
    ! Thankfully, we can't have both "round-to-even" and subnormals because
    ! "round-to-even" only occurs for powers close to 0.
            M = M + IAND(M, OneMant)  ! round up
            M = SHIFTR(M, 1)
    ! There is a weird scenario where we don't have a subnormal but just.
    ! Suppose we start with 2.2250738585072013e-308, we end up
    ! with 0x3fffffffffffff x 2^-1023-53 which is technically subnormal
    ! whereas 0x40000000000000 x 2^-1023-53  is normal. Now, we need to round
    ! up 0x3fffffffffffff x 2^-1023-53  and once we do, we are no longer
    ! subnormal, but we can only know this after rounding.
    ! So we only declare a subnormal if we are smaller than the threshold.
    ! IF (M .ULT. SHIFTL(OneMant, Mantissa_Explicit_Bits)) THEN
            IF (M .ULT. Hidden_Bit_Mask) THEN
                E = 0
            ELSE
                E = 1
            END IF
            RETURN
        END IF

    ! usually, we round *up*, but if we fall right in between and and we have an
    ! even basis, we need to round down
    ! We are only concerned with the cases where 5**Q fits in single 64-bit word.
        IF ((ProductLo .ULE. OneMant) .AND. (Q >= Min_Exponent_Round_To_Even) .AND. &
            (Q <= Max_Exponent_Round_To_Even) .AND. &
            (IAND(M, ThreeUInt) == OneMant)) THEN  ! we may fall between two floats!
    ! To be in-between two floats we need that in doing
    !   M = ProductHi >> (UpperBit + 64 - Mantissa_Explicit_Bits - 3)
    ! ... we dropped out only zeros. But if this happened, then we can go back!!!
            IF (SHIFTL(M, (UpperBit + MantTotalBits - Mantissa_Explicit_Bits - 3)) == &
                ProductHi) THEN
                M = IAND(M, NotOneMant)   ! flip it so that we do not round up
            END IF
        END IF

        M = M + IAND(M, OneMant)        ! round up
        M = SHIFTR(M, 1)
        IF (M .UGE. Max_Mantissa_Fast_Path) THEN
            M = SHIFTL(OneMant, Mantissa_Explicit_Bits)
            E = E + 1                   ! undo previous addition
        END IF

        M = IAND(M, NotSigHidBitMask)
        IF (E >= Infinite_Power) THEN   ! infinity
            E = Infinite_Power
            M = 0_I8B
        END IF

        RETURN

    END SUBROUTINE Compute_Float

!**************************************************************************

    FUNCTION Scientific_Exponent(Number) RESULT(Exponent)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To calculate the exponent, in scientific notation, of the number.
    ! this algorithm is not even close to optimized, but it has no practical
    ! effect on performance: in order to have a faster algorithm, we'd need
    ! to slow down performance for faster algorithms, and this is still fast.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(Parsed_Number_Info), INTENT(IN)    :: Number
        INTEGER(KIND=I4B)                       :: Exponent

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: Mantissa

    !** FLOW

        Mantissa = Number%Sig   ! implicit narrow conversion for 32-bit
        Exponent = Number%Exp
        DO WHILE (Mantissa .UGE. TenThousandUInt)
            Mantissa = DivByPow10(Mantissa, 4)
            Exponent = Exponent + 4
        END DO
        DO WHILE (Mantissa .UGE. HundredUInt)
            Mantissa = DivByPow10(Mantissa, 2)
            Exponent = Exponent + 2
        END DO
        DO WHILE (Mantissa .UGE. TenUInt)
            Mantissa = DivByPow10(Mantissa, 1)
            Exponent = Exponent + 1
        END DO

        RETURN

    END FUNCTION Scientific_Exponent

!**************************************************************************

    SUBROUTINE Digit_Comparision(cStr, NumInfo, EIn, MIn, EOut, MOut)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To parse the significant digits as a big integer to unambiguously round the
    ! the significant digits. here, we are trying to determine how to round
    ! an extended float representation close to `b+h`, halfway between `b`
    ! (the float rounded-down) and `b+u`, the next positive float. this
    ! algorithm is always correct, and uses one of two approaches. when
    ! the exponent is positive relative to the significant digits (such as
    ! 1234), we create a big-integer representation, get the high 64-bits,
    ! determine if any lower bits are truncated, and use that to direct
    ! rounding. in case of a negative exponent relative to the significant
    ! digits (such as 1.2345), we create a theoretical representation of
    ! `b` as a big-integer type, scaled to the same binary exponent as
    ! the actual digits. we then compare the big integer representations
    ! of both, and use that to direct rounding.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),         INTENT(IN)    :: cStr
        TYPE(Parsed_Number_Info), INTENT(IN)    :: NumInfo
        INTEGER(KIND=I4B),        INTENT(IN)    :: EIn
        INTEGER(KIND=I4B),        INTENT(OUT)   :: EOut
        INTEGER(KIND=I8B),        INTENT(IN)    :: MIn
        INTEGER(KIND=I8B),        INTENT(OUT)   :: MOut

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(BigUInt)       :: Big
        INTEGER(KIND=I4B)   :: Sci_Exp, Digits, Exponent
        INTEGER(KIND=I4B)   :: EIn2

    !** FLOW

    ! remove the invalid exponent bias
        EIn2 = EIn - Invalid_AM_Bias

        Sci_Exp = Scientific_Exponent(NumInfo)
        Digits = 0
        CALL Parse_Mantissa(cStr, Big, NumInfo, Max_Digits, Digits)
    ! can't underflow, since digits is at most max_digits.
        Exponent = Sci_Exp + 1 - Digits
        IF (Exponent >= 0) THEN
            CALL Positive_Digit_Comparision(Big, Exponent, EOut, MOut)
        ELSE
            CALL Negative_Digit_Comparision(Big, EIn2, MIn, Exponent, EOut, MOut)
        END IF

        RETURN

    END SUBROUTINE Digit_Comparision

!**************************************************************************

    FUNCTION Is_AdjustedMantissa_NE(ELhs, MLhs, ERhs, MRhs) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To compare whether LHS /= RHS.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)    :: ELhs
        INTEGER(KIND=I4B),  INTENT(IN)    :: ERhs
        INTEGER(KIND=I8B),  INTENT(IN)    :: MLhs
        INTEGER(KIND=I8B),  INTENT(IN)    :: MRhs
        LOGICAL                           :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        Flag = (MLhs /= MRhs).OR.(ELhs /= ERhs)

        RETURN

    END FUNCTION Is_AdjustedMantissa_NE

!**************************************************************************

END SUBROUTINE Dec2Bin_FastFloat

!******************************************************************************

SUBROUTINE Round(E, M, CB)

!** PURPOSE OF THIS SUBROUTINE:
! To round an extended-precision float to the nearest machine float.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(INOUT) :: E    ! exponent in base 2
    INTEGER(KIND=I8B),  INTENT(INOUT) :: M    ! adjusted significand in base 2
    PROCEDURE(CB_Round)               :: CB   ! actual procedure that perform rounding

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: Mantissa_Shift, Shift

!** FLOW

    Mantissa_Shift = MantTotalBits - Mantissa_Explicit_Bits - 1
    IF (-E >= Mantissa_Shift) THEN
! have a denormal float
        Shift = -E + 1
        CALL CB(E, M, MIN(Shift, MantTotalBits))
! check for round-up: if rounding-nearest carried us to the hidden bit.
        IF (M .ULT. Hidden_Bit_Mask) THEN
            E = 0
        ELSE
            E = 1
        END IF
        RETURN
    END IF

! have a normal float, use the default shift.
    CALL CB(E, M, Mantissa_Shift)

! check for carry
    IF (M .UGE. Max_Mantissa_Fast_Path) THEN
        M = Hidden_Bit_Mask
        E = E + 1
    END IF

! check for infinite: we could have carried to an infinite power
    M = IAND(M, NotSigHidBitMask)
    IF (E >= Infinite_Power) THEN
        E = Infinite_Power
        M = 0_I8B
    END IF

    RETURN

END SUBROUTINE Round

!******************************************************************************

SUBROUTINE Round_Nearest_Tie_Even(E, M, Shift, CB)

!** PURPOSE OF THIS SUBROUTINE:
! To round an extended-precision float to the nearest tie to even.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(INOUT)     :: E    ! exponent in base 2
    INTEGER(KIND=I8B),  INTENT(INOUT)     :: M    ! adjusted significand in base 2
    INTEGER(KIND=I4B), INTENT(IN)         :: Shift
    PROCEDURE(CB_Round_Nearest)           :: CB

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: Mask, Halfway, Truncated_Bits
    LOGICAL             :: Is_Above, Is_Halfway, Is_Odd

!** FLOW

    IF (Shift == MantTotalBits) THEN
        Mask = MaxMant
    ELSE
        Mask = SHIFTL(OneMant, Shift) - OneMant
    END IF
    IF (Shift == 0) THEN
        Halfway = 0_I8B
    ELSE
        Halfway = SHIFTL(OneMant, (Shift - 1))
    END IF

    Truncated_Bits = IAND(M, Mask)
    Is_Above   = Truncated_Bits .UGT. Halfway
    Is_Halfway = Truncated_Bits == Halfway

! shift digits into position
    IF (Shift == MantTotalBits) THEN
        M = 0_I8B
    ELSE
        M = SHIFTR(M, Shift)
    END IF
    E = E + Shift

    Is_Odd = IAND(M, OneMant) == OneMant
    IF (CB(Is_Odd, Is_Halfway, Is_Above)) M = M + OneMant

    RETURN

END SUBROUTINE Round_Nearest_Tie_Even

!******************************************************************************

SUBROUTINE Parse_Mantissa(cStr, Big, NumInfo, Max_Digits, Digits)

!** PURPOSE OF THIS SUBROUTINE:
! To parse the significant digits into a BigUInt

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*),         INTENT(IN)    :: cStr
    TYPE(BigUInt),            INTENT(INOUT) :: Big
    TYPE(Parsed_Number_Info), INTENT(IN)    :: NumInfo
    INTEGER(KIND=I4B),        INTENT(IN)    :: Max_Digits
    INTEGER(KIND=I4B),        INTENT(INOUT) :: Digits

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Indx, IEnd
    INTEGER(KIND=I4B)     :: Counter, Step
    INTEGER(KIND=I8B)     :: Value
    LOGICAL               :: Truncated

!** FLOW

! try to minimize the number of big integer and scalar multiplication.
! therefore, try to parse 8 digits at a time, and multiply by the largest
! scalar value (19 digits) for each step.
    Counter = 0
    Digits = 0
    Value = 0_I8B
    Step = 19

! process all integer digits.
    IF (NumInfo%IntegralStart /= 0) THEN
        Indx = NumInfo%IntegralStart
        IEnd = NumInfo%IntegralEnd
        CALL Skip_Zeros(cStr, Indx, IEnd)
! process all digits, in increments of step per loop
        DO WHILE (Indx <= IEnd)
            DO WHILE ((Indx+7 <= IEnd).AND.(Step-Counter >= 8).AND.(Max_Digits-Digits >= 8))
                CALL Parse_Eight_Digits(cStr, Indx, Value, Counter, Digits)
            END DO
            DO WHILE ((Counter < Step).AND.(Indx <= IEnd).AND.(Digits < Max_Digits))
                CALL Parse_One_Digit(cStr, Indx, Value, Counter, Digits)
            END DO
            IF (Digits == Max_Digits) THEN
! add the temporary value, then check if we've truncated any digits
                CALL Add_Native(Big, Powers_of_Ten_Uint64(Counter), Value)
                Truncated = Is_Truncated(cStr, Indx, IEnd)
                IF (NumInfo%FractionStart /= 0) THEN
                    Truncated = Truncated.OR.Is_Truncated(cStr, NumInfo%FractionStart, NumInfo%FractionEnd)
                END IF
                IF (Truncated) THEN
                    CALL Round_Up_BigUInt(Big, Digits)
                END IF
                RETURN
            ELSE
                CALL Add_Native(Big, Powers_of_Ten_Uint64(Counter), Value)
                Counter = 0
                Value = 0_I8B
            END IF
        END DO
    END IF

! add our fraction digits, if they're available.
    IF (NumInfo%FractionStart /= 0) THEN
        Indx = NumInfo%FractionStart
        IEnd = NumInfo%FractionEnd
        IF (Digits == 0) THEN
            CALL Skip_Zeros(cStr, Indx, IEnd)
        END IF
! process all digits, in increments of step per loop
        DO WHILE (Indx <= IEnd)
            DO WHILE ((Indx+7 <= IEnd).AND.(Step-Counter >= 8).AND.(Max_Digits-Digits >= 8))
                CALL Parse_Eight_Digits(cStr, Indx, Value, Counter, Digits)
            END DO
            DO WHILE ((Counter < Step).AND.(Indx <= IEnd).AND.(Digits < Max_Digits))
                CALL Parse_One_Digit(cStr, Indx, Value, Counter, Digits)
            END DO
            IF (Digits == Max_Digits) THEN
! add the temporary value, then check if we've truncated any digits
                CALL Add_Native(Big, Powers_of_Ten_Uint64(Counter), Value)
                IF (Is_Truncated(cStr, Indx, IEnd)) THEN
                    CALL Round_Up_BigUInt(Big, Digits)
                END IF
                RETURN
            ELSE
                CALL Add_Native(Big, Powers_of_Ten_Uint64(Counter), Value)
                Counter = 0
                Value = 0_I8B
            END IF
        END DO
    END IF

    IF (Counter /= 0) THEN
        CALL Add_Native(Big, Powers_of_Ten_Uint64(Counter), Value)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Skip_Zeros(cStr, IStart, IEnd)

!DIR$ ATTRIBUTES FORCEINLINE :: Skip_Zeros

    !** PURPOSE OF THIS SUBROUTINE:
    ! To find IStart by skipping zeros.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),  INTENT(IN)       :: cStr
        INTEGER(KIND=I4B), INTENT(INOUT)    :: IStart
        INTEGER(KIND=I4B), INTENT(IN)       :: IEnd

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER  :: MConst = INT(Z'3030303030303030', KIND=I8B)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CHARACTER(LEN=8)    :: wStr
        INTEGER(KIND=I8B)   :: wVal
        EQUIVALENCE(wStr, wVal)

    !** FLOW

        DO WHILE (IStart + 7 <= IEnd)
            wStr = cStr(IStart:IStart+7)
            IF (wVal /= MConst) EXIT
            IStart = IStart + 8
        END DO
        DO WHILE (IStart <= IEnd)
            IF (cStr(IStart:IStart) /= '0') EXIT
            IStart = IStart + 1
        END DO

        RETURN

    END SUBROUTINE Skip_Zeros

!**************************************************************************

    FUNCTION Is_Truncated(cStr, IStart, IEnd) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: Is_Truncated

    !** PURPOSE OF THIS SUBROUTINE:
    ! To determine if any non-zero digits were truncated.
    ! all characters must be valid digits.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),  INTENT(IN)   :: cStr
        INTEGER(KIND=I4B), INTENT(IN)   :: IStart
        INTEGER(KIND=I4B), INTENT(IN)   :: IEnd
        LOGICAL                         :: Flag

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER  :: MConst = INT(Z'3030303030303030', KIND=I8B)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: Indx
        CHARACTER(LEN=8)    :: wStr
        INTEGER(KIND=I8B)   :: wVal
        EQUIVALENCE(wStr, wVal)

    !** FLOW

    ! initialize
        Indx = IStart
        Flag = TrueVal

    ! do 8-bit optimizations, can just compare to 8 literal 0s.
        DO WHILE (Indx + 7 <= IEnd)
            wStr = cStr(Indx:Indx+7)
            IF (wVal /= MConst) RETURN
            Indx = Indx + 8
        END DO
        DO WHILE (Indx <= IEnd)
            IF (cStr(Indx:Indx) /= '0') RETURN
            Indx = Indx + 1
        END DO
        Flag = FalseVal

        RETURN

    END FUNCTION Is_Truncated

!**************************************************************************

    SUBROUTINE Parse_Eight_Digits(cStr, Indx, Value, Counter, Count)

!DIR$ ATTRIBUTES FORCEINLINE :: Parse_Eight_Digits

    !** PURPOSE OF THIS SUBROUTINE:
    ! To parse 8 digits immediately.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),   INTENT(IN)       :: cStr
        INTEGER(KIND=I4B),  INTENT(INOUT)    :: Indx, Counter, Count
        INTEGER(KIND=I8B),  INTENT(INOUT)    :: Value

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CHARACTER(LEN=8)    :: wStr
        INTEGER(KIND=I8B)   :: wVal
        EQUIVALENCE(wStr, wVal)

    !** FLOW

        wStr = cStr(Indx:Indx+7)
        Value = Value*100000000_I8B + Parse_Eight_Digits_Unrolled(wVal)
        Indx = Indx + 8
        Counter = Counter + 8
        Count = Count + 8

        RETURN

    END SUBROUTINE Parse_Eight_Digits

!**************************************************************************

    SUBROUTINE Parse_One_Digit(cStr, Indx, Value, Counter, Count)

!DIR$ ATTRIBUTES FORCEINLINE :: Parse_One_Digit

    !** PURPOSE OF THIS SUBROUTINE:
    ! To parse 1 digit

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),   INTENT(IN)       :: cStr
        INTEGER(KIND=I4B),  INTENT(INOUT)    :: Indx, Counter, Count
        INTEGER(KIND=I8B),  INTENT(INOUT)    :: Value

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER  :: A0 = IACHAR('0')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        Value = Value*10_I8B + (IACHAR(cStr(Indx:Indx))-A0)
        Indx = Indx + 1
        Counter = Counter + 1
        Count = Count + 1

        RETURN

    END SUBROUTINE Parse_One_Digit

!**************************************************************************

    SUBROUTINE Add_Native(Big, Power, Value)

!DIR$ ATTRIBUTES FORCEINLINE :: Add_Native

    !** PURPOSE OF THIS SUBROUTINE:
    ! To add value to BigUInt

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(BigUInt),      INTENT(INOUT)    :: Big
        INTEGER(KIND=I8B),  INTENT(IN)       :: Power, Value

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        CALL Big%SmallMul(Power)
        CALL Big%Add(Value)

        RETURN

    END SUBROUTINE Add_Native

!**************************************************************************

    SUBROUTINE Round_Up_BigUInt(Big, Count)

!DIR$ ATTRIBUTES FORCEINLINE :: Round_Up_BigUInt

    !** PURPOSE OF THIS SUBROUTINE:
    ! To round BigUInt up

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(BigUInt),      INTENT(INOUT)    :: Big
        INTEGER(KIND=I4B),  INTENT(INOUT)    :: Count

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

    ! need to round-up the digits, but need to avoid rounding
    ! ....9999 to ...10000, which could cause a false halfway point.
        CALL Add_Native(Big, 10_I8B, 1_I8B)
        Count = Count + 1

        RETURN

    END SUBROUTINE Round_Up_BigUInt

!**************************************************************************

END SUBROUTINE Parse_Mantissa

!******************************************************************************

SUBROUTINE Positive_Digit_Comparision(Big, Exp, E2, M2)

!** PURPOSE OF THIS SUBROUTINE:
! To compare BigInt for positive exponent.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt),      INTENT(INOUT)    :: Big
    INTEGER(KIND=I4B),  INTENT(IN)       :: Exp
    INTEGER(KIND=I4B),  INTENT(OUT)      :: E2    ! exponent in base 2
    INTEGER(KIND=I8B),  INTENT(OUT)      :: M2    ! adjusted significand in base 2

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: Offset = Mantissa_Explicit_Bits - Minimum_Exponent - MantTotalBits

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    LOGICAL    :: Truncated

!** FLOW

    CALL Big%Pow10(Exp)
    M2 = Big%Hi64(Truncated)
    E2 = Big%BitLen() + Offset
    CALL Round(E2, M2, Callback_Round)

    RETURN

CONTAINS

    SUBROUTINE Callback_Round(E, M, Shift)
        ! arguments
        INTEGER(KIND=I4B),  INTENT(INOUT) :: E    ! exponent in base 2
        INTEGER(KIND=I8B),  INTENT(INOUT) :: M    ! adjusted significand in base 2
        INTEGER(KIND=I4B),  INTENT(IN)    :: Shift
        ! execution
        CALL Round_Nearest_Tie_Even(E, M, Shift, Callback_Round_Nearest)
        RETURN
    END SUBROUTINE

!**************************************************************************

    FUNCTION Callback_Round_Nearest(IsOdd, IsHalfway, IsAbove) RESULT(Flag)
        ! arguments
        LOGICAL, INTENT(IN)    :: IsOdd, IsHalfway, IsAbove
        LOGICAL                :: Flag
        ! execution
        Flag = IsAbove.OR.(IsHalfway.AND.Truncated).OR.(IsOdd.AND.IsHalfway)
        RETURN
    END FUNCTION

!**************************************************************************

END SUBROUTINE Positive_Digit_Comparision

!******************************************************************************

SUBROUTINE Negative_Digit_Comparision(Big, EIn, MIn, Exp, EOut, MOut)

!** PURPOSE OF THIS SUBROUTINE:
! To compare BigInt for negative exponent.
!
! The scaling here is quite simple: we have, for the real digits `m * 10^e`,
! and for the theoretical digits `n * 2^f`. Since `e` is always negative,
! to scale them identically, we do `n * 2^f * 5^-f`, so we now have `m * 2^e`.
! we then need to scale by `2^(f- e)`, and then the two significant digits
! are of the same magnitude.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt), TARGET, INTENT(INOUT)    :: Big
    INTEGER(KIND=I4B),     INTENT(IN)       :: EIn
    INTEGER(KIND=I4B),     INTENT(IN)       :: Exp
    INTEGER(KIND=I4B),     INTENT(OUT)      :: EOut
    INTEGER(KIND=I8B),     INTENT(IN)       :: MIn
    INTEGER(KIND=I8B),     INTENT(OUT)      :: MOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(BigUInt), POINTER  :: RealDigits => NULL()
    TYPE(BigUInt)           :: TheoryDigits
    INTEGER(KIND=I4B)       :: RealExp, TheoryExp
    INTEGER(KIND=I4B)       :: Pow2_Exp, Pow5_Exp
    INTEGER(KIND=I4B)       :: Ord
    REAL(KIND=DP)           :: FloatBase
    INTEGER(KIND=I4B)       :: EBase
    INTEGER(KIND=I4B)       :: ETheory
    INTEGER(KIND=I8B)       :: MBase
    INTEGER(KIND=I8B)       :: MTheory

!** FLOW

! set working variables
    RealDigits => Big
    RealExp = Exp

! get the value of `b`, rounded down, and get a bigint representation of b+h
    EBase = EIn
    MBase = MIn
    CALL Round(EBase, MBase, CBRound)
    CALL To_Float(FalseVal, EBase, MBase, FloatBase)

    CALL To_Extended_Halfway(FloatBase, ETheory, MTheory)

    CALL TheoryDigits%FromU64(MTheory)

    TheoryExp = ETheory

! scale real digits and theor digits to be same power.
    Pow2_Exp = TheoryExp - RealExp
    Pow5_Exp = -RealExp
    IF (Pow5_Exp /= 0) THEN
        CALL TheoryDigits%Pow5(Pow5_Exp)
    END IF
    IF (Pow2_Exp > 0) THEN
        CALL TheoryDigits%Pow2(Pow2_Exp)
    ELSEIF (Pow2_Exp < 0) THEN
        CALL RealDigits%Pow2(-Pow2_Exp)
    END IF

! compare digits, and use it to director rounding
    Ord = RealDigits%Compare(TheoryDigits)
    EOut = EIn
    MOut = MIn
    CALL Round(EOut, MOut, Callback_Round)

! free pointer
    NULLIFY(RealDigits)

    RETURN

CONTAINS

    SUBROUTINE CBRound(E, M, Shift)
        ! arguments
        INTEGER(KIND=I4B),  INTENT(INOUT) :: E    ! exponent in base 2
        INTEGER(KIND=I8B),  INTENT(INOUT) :: M    ! adjusted significand in base 2
        INTEGER(KIND=I4B),  INTENT(IN)    :: Shift
        ! execution
        CALL Round_Down(E, M, Shift)
        RETURN
    END SUBROUTINE

!**************************************************************************

    SUBROUTINE Callback_Round(E, M, Shift)
        ! arguments
        INTEGER(KIND=I4B),  INTENT(INOUT) :: E    ! exponent in base 2
        INTEGER(KIND=I8B),  INTENT(INOUT) :: M    ! adjusted significand in base 2
        INTEGER(KIND=I4B),  INTENT(IN)    :: Shift
        ! execution
        CALL Round_Nearest_Tie_Even(E, M, Shift, Callback_Round_Nearest)
        RETURN
    END SUBROUTINE

!**************************************************************************

    FUNCTION Callback_Round_Nearest(IsOdd, IsHalfway, IsAbove) RESULT(Flag)
        ! arguments
        LOGICAL, INTENT(IN)    :: IsOdd, IsHalfway, IsAbove
        LOGICAL                :: Flag
        ! execution
        IF (Ord > 0) THEN
            Flag = TrueVal
        ELSEIF (Ord < 0) THEN
            Flag = FalseVal
        ELSE
            Flag = IsOdd
        END IF
        RETURN
    END FUNCTION

!**************************************************************************

    SUBROUTINE To_Extended(Value, E, M)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To convert a native floating-point number to an extended-precision float.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        REAL(KIND=DP),      INTENT(IN)    :: Value
        INTEGER(KIND=I4B),  INTENT(OUT)   :: E    ! exponent in base 2
        INTEGER(KIND=I8B),  INTENT(OUT)   :: M    ! adjusted significand in base 2

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I4B), PARAMETER  :: Bias = Mantissa_Explicit_Bits - Minimum_Exponent

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)    :: Bits

    !** FLOW

        Bits = RawFP_FromFloat(Value)

        IF (IAND(Bits, Exponent_Mask) == 0_I8B) THEN
    ! denormal
            E = 1 - Bias
            M = IAND(Bits, Mantissa_Mask)
        ELSE
    ! normal
            E = INT(SHIFTR(IAND(Bits, Exponent_Mask), Mantissa_Explicit_Bits), KIND=I4B) - Bias
            M = IOR(IAND(Bits, Mantissa_Mask), Hidden_Bit_Mask)
        END IF

        RETURN

    END SUBROUTINE To_Extended

!**************************************************************************

    SUBROUTINE To_Extended_Halfway(Value, E, M)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To get the extended precision value of the halfway point between b and b+u.
    ! we are given a native float that represents b, so we need to adjust it
    ! halfway between b and b+u.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        REAL(KIND=DP),      INTENT(IN)    :: Value
        INTEGER(KIND=I4B),  INTENT(OUT)   :: E    ! exponent in base 2
        INTEGER(KIND=I8B),  INTENT(OUT)   :: M    ! adjusted significand in base 2

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        CALL To_Extended(Value, E, M)
        M = SHIFTL(M, 1) + OneMant
        E = E - 1

        RETURN

    END SUBROUTINE To_Extended_Halfway

!**************************************************************************

    SUBROUTINE Round_Down(E, M, Shift)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To round an extended-precision float down.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(INOUT) :: E    ! exponent in base 2
        INTEGER(KIND=I8B),  INTENT(INOUT) :: M    ! adjusted significand in base 2
        INTEGER(KIND=I4B),  INTENT(IN)    :: Shift

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW

        IF (Shift == MantTotalBits) THEN
            M = 0_I8B
        ELSE
            M = SHIFTR(M, Shift)
        END IF
        E = E + Shift

        RETURN

    END SUBROUTINE Round_Down

!**************************************************************************

    SUBROUTINE To_Float(Negative, E, M, Value)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To convert adjusted mantissa to double-precision value.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        LOGICAL,            INTENT(IN)    :: Negative
        INTEGER(KIND=I4B),  INTENT(IN)    :: E    ! exponent in base 2
        INTEGER(KIND=I8B),  INTENT(IN)    :: M    ! adjusted significand in base 2
        REAL(KIND=DP),      INTENT(OUT)   :: Value

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(BinRep)        :: FpBin
        INTEGER(KIND=I8B)   :: Word

    !** FLOW

    ! get input
        FpBin%Negative = Negative
        FpBin%Exponent = E
        FpBin%Significand = M       ! implicit narrowing conversion for 32 bit

    ! compose the component parts into word
        Word = RawFP_Construct(FpBin)

    ! convert word to real number
        Value = RawFP_ToFloat(Word)

        RETURN

    END SUBROUTINE To_Float

!**************************************************************************

END SUBROUTINE Negative_Digit_Comparision

!******************************************************************************

FUNCTION Dec2Bin_YY(SigDec, ExpDec, Negative, cStr, Aux) RESULT(RawFP)

!** PURPOSE OF THIS SUBROUTINE:
! To use YY's algorithm to convert from decimal representation
! to (raw) binary representation

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN) :: SigDec   ! significand, base 10
    INTEGER(KIND=I4B),  INTENT(IN) :: ExpDec   ! exponent, base 10
    LOGICAL,            INTENT(IN) :: Negative ! true if the floating point value is negative
    CHARACTER(LEN=*),   INTENT(IN) :: cStr     ! floating-point number string
    TYPE(StringAux),    INTENT(IN) :: Aux      ! auxiliary string information
    INTEGER(KIND=I8B)              :: RawFP    ! floating point number as an unsigned integer

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B),  PARAMETER :: MinExpFastPath = -DecimalRange
    INTEGER(KIND=I4B),  PARAMETER :: MaxExpFastPath = DecimalRange - UIntSafeDigits

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: SigBin   ! significand, base 2
    INTEGER(KIND=I4B)   :: ExpBin   ! exponent, base 2

!** FLOW

! check whether to use YY's fast path
    IF ((.NOT.Aux%Truncated).AND.(ExpDec > MinExpFastPath).AND.(ExpDec < MaxExpFastPath)) THEN
        IF (D2B_YY_FastPath(SigDec, ExpDec, SigBin, ExpBin)) THEN
! YY's fast path is success so set sign bit
            IF (Negative) THEN
                RawFP = SignMask
            ELSE
                RawFP = 0_I8B
            END IF
! then, add exponent bits
            RawFP = IOR(RawFP, SHIFTL(ToUnsignedLong(ExpBin), SignificandBits))
! finally, add (both implicit and explicit) significand bits
            RawFP = IOR(RawFP, IAND(SigBin, SignificandMask))
            RETURN
        END IF
    END IF

! perform decimal to binary conversion using YY's slow path
    RawFP = D2B_YY_SlowPath(SigDec, ExpDec, Negative, cStr, Aux)

    RETURN

    CONTAINS

    FUNCTION D2B_YY_FastPath(SigDec, ExpDec, SigBin, ExpBin) RESULT(Success)

!DIR$ ATTRIBUTES FORCEINLINE :: D2B_YY_FastPath

    !** PURPOSE OF THIS SUBROUTINE:
    ! To use YY's fast path algorithm to convert from decimal representation
    ! to binary representation

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: SigDec   ! significand in base 10
        INTEGER(KIND=I4B), INTENT(IN)   :: ExpDec   ! exponent in base 10
        INTEGER(KIND=I8B), INTENT(OUT)  :: SigBin   ! significand in base 2
        INTEGER(KIND=I4B), INTENT(OUT)  :: ExpBin   ! exponent in base 2
        LOGICAL                         :: Success  ! true if conversion can be handled

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(UInt128)       :: Pow10
        INTEGER(KIND=I8B)   :: Sig2, Sig2_Ext
        INTEGER(KIND=I8B)   :: Hi, Lo, Hi2
        INTEGER(KIND=I8B)   :: Sig1, Add, Bits
        INTEGER(KIND=I4B)   :: Exp2, Lz
        LOGICAL             :: Exact

    !** FLOW

    ! To keep it simple, we only accept normal number here,
    ! let the slow path handle subnormal and infinity number.

    ! The result value is exactly equal to (SigDec * 10**ExpDec),
    ! the exponent part (10**ExpDec) can be converted to (Sig2 * 2**Exp2).

    ! The Sig2 can be an infinite length number, only the highest 256 bits
    ! is cached in the Pow10_Sig_Table.
    ! (Quad uses 256 bits, Double uses 128 bits, and Single uses 64 bits)

    ! Now we have these bits:
    ! Sig1 (normalized 128/64/32 bit)   : aaaaaaaaaaaaaaaa
    ! Sig2 (higher 128/64/32 bit)       : bbbbbbbbbbbbbbbb
    ! Sig2_Ext (lower 128/64/32 bit)    : cccccccccccccccc
    ! Sig2_Cut (extra unknown bits)     : dddddddddddddddddddddddd....

    ! And the calculation process is:
    ! -------------------------------------------------------------
    !         aaaaaaaaaaaaaaaa *
    !         bbbbbbbbbbbbbbbbccccccccccccccccdddddddddddd....
    ! -------------------------------------------------------------
    ! abababababababababababababababab +
    !         acacacacacacacacacacacacacacacac +
    !                 adadadadadadadadadadadadadadadadadadadad....
    ! -------------------------------------------------------------
    ! [Hi____][Lo____] +
    !         [Hi2___][Lo2___] +
    !                 [unknown___________....]
    ! -------------------------------------------------------------

    ! The addition with carry may affect higher bits, but if there is a 0
    ! in higher bits, the bits higher than 0 will not be affected.

    ! 'Lo2' + 'unknown' may get a carry bit and may affect 'Hi2', the max value
    ! of 'Hi2' is 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE/0xFFFFFFFFFFFFFFFE/0xFFFFFFFE,
    ! so 'Hi2' will not overflow.

    ! 'Lo' + 'Hi2' may alse get a carry bit and may affect 'Hi', but only
    ! the highest significant 113/53/24 bits of 'Hi' is needed. If there is a 0
    ! in the lower bits of 'Hi', then all the following bits can be dropped.

    ! To convert the result to IEEE-754 double number, we need to perform
    ! correct rounding:
    ! 1. if bit 114/54/25 is 0, round down,
    ! 2. if bit 114/54/25 is 1 and any bit beyond bit 114/54/25 is 1, round up,
    ! 3. if bit 114/54/25 is 1 and all bits beyond bit 114/54/25 are 0, round to even,
    !    as the extra bits is unknown, this case will not be handled here.

    ! initialize
        Exact   = FalseVal
        Success = FalseVal

    ! convert (10*ExpDec) to (Sig2 * 2**Exp2)
        Pow10    = Get_Pow10_128Bits(ExpDec)
        Sig2     = Pow10%High
        Sig2_Ext = Pow10%Low
        Exp2     = Floor_Log2_Pow10(ExpDec) - SignBits

    ! normalize and multiply
        Lz   = LEADZ(SigDec)
        Sig1 = SHIFTL(SigDec, Lz)
        Exp2 = Exp2 - Lz
        CALL UMul128(Sig1, Sig2, Hi, Lo)

    ! To get normalized value, 'Hi' should be shifted to the left by 0 or 1.

    ! The highest significant 113/53/24 bits is used by IEEE-754 double number,
    ! and the bit 114/54/25 is used to detect rounding direction.

    ! The lowest 13 (= 128 - 114 - 1) / 9 (= 64 - 54 - 1) / 6 (= 32 - 25 - 1) bits (LowBits)
    ! is used to check whether it contains 0.
    ! Note: BitMask = SHIFTL(1, LowBits) - 1
        Bits = IAND(Hi, BitMask)

        IF ((Bits /= 0_I8B).AND.(Bits /= BitMask)) THEN
    ! The 'Bits' is not zero, so we don't need to check 'round to even' case.
    ! The 'Bits' contains bit '0', so we can drop the extra bits after '0'.
            Exact = TrueVal
        ELSE
    ! The 'Bits' is filled with all '0' or all '1', so we need to check
    ! more lower bits with another multiplication.
            Hi2 = UMul128_Upper64(Sig1, Sig2_Ext)
            Add = Lo + Hi2
            IF ((Add /= 0_I8B).AND.(Add /= MaxUInt)) THEN
    ! The 'Add' is not zero, so we don't need to check 'round to even' case.
    ! The 'Add' contains bit '0', so we can drop the extra bits after '0'.
    ! The 'Hi' cannot be MaxUInt, so it will not overflow.
                IF ((Add .ULT. Lo).OR.(Add .ULT. Hi2)) Hi = Hi + 1_I8B
                Exact = TrueVal
            END IF
        END IF
        IF (Exact) THEN
    ! normalize
            IF (Hi .ULT. SignMask) THEN
                Hi = SHIFTL(Hi, 1)
                Exp2 = Exp2 - 1
            END IF
            Exp2 = Exp2 + TotalBits

    ! test the bit 114 and get rounding direction
            IF (IAND(Hi, AddRound) /= 0_I8B) Hi = Hi + AddRound

    ! test overflow
            IF (Hi .ULT. AddRound) THEN
                Hi = SignMask
                Exp2 = Exp2 + 1
            END IF

    ! This is a normal number, convert it to binary representation.
            SigBin = SHIFTR(Hi, ExponentBits)
            ExpBin = Exp2 + (ExponentBits + SignificandBits) + ExponentBias
            Success = TrueVal
        END IF

        RETURN

    END FUNCTION D2B_YY_FastPath

    !**************************************************************************

END FUNCTION Dec2Bin_YY

!******************************************************************************

FUNCTION D2B_YY_SlowPath(SigDec, ExpDec, Negative, cStr, Aux) RESULT(RawFP)

!** PURPOSE OF THIS SUBROUTINE:
! To use YY's slow path algorithm to convert from decimal representation
! to (raw) binary representation

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN) :: SigDec   ! significand
    INTEGER(KIND=I4B),  INTENT(IN) :: ExpDec   ! exponent
    LOGICAL,            INTENT(IN) :: Negative ! true if the floating point value is negative
    CHARACTER(LEN=*),   INTENT(IN) :: cStr     ! floating-point number string
    TYPE(StringAux),    INTENT(IN) :: Aux      ! auxiliary string information
    INTEGER(KIND=I8B)              :: RawFP    ! floating point number as an unsigned integer

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: ERR_ULP_LOG    = 3
    INTEGER(KIND=I4B), PARAMETER  :: ERR_ULP        = SHIFTL(1, ERR_ULP_LOG)
    INTEGER(KIND=I4B), PARAMETER  :: ERR_CACHED_POW = ERR_ULP / 2
    INTEGER(KIND=I4B), PARAMETER  :: ERR_MUL_FIXED  = ERR_ULP / 2
    INTEGER(KIND=I4B), PARAMETER  :: DIY_SIG_BITS   = TotalBits
    INTEGER(KIND=I4B), PARAMETER  :: EXP_BIAS       = ExponentBias + SignificandBits
    INTEGER(KIND=I4B), PARAMETER  :: EXP_SUBNORMAL  = -EXP_BIAS + 1
    INTEGER(KIND=I4B), PARAMETER  :: A0             = IACHAR('0')

!** SUBROUTINE DERIVED TYPE DEFINITIONS
! "Do It Yourself Floating Point"
    TYPE Diy_Fp
        INTEGER(KIND=I8B)   :: Sig      ! significand
        INTEGER(KIND=I4B)   :: Exp      ! exponent, base 2
    END TYPE
! ----------------------------------------------------------------------------

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)       :: Sign
    INTEGER(KIND=I8B)       :: FpErr, PrecisionBits, HalfWay
    INTEGER(KIND=I4B)       :: Bits
    INTEGER(KIND=I4B)       :: Order_of_Magnitude, Effective_Significand_Size
    INTEGER(KIND=I4B)       :: PrecisionDigitsCount, Cmp, Exp10
    TYPE(Diy_Fp)            :: Fp, FpUpper
    TYPE(BigUInt)           :: BigFull, BigComp

!** FLOW

! Slow path: read floating-point number exactly with diyfp.
! 1. Use cached diyfp to get an approximation value.
! 2. Use bigcomp to check the approximation value if needed.

! This algorithm refers to google's double-conversion project:
! https://github.com/google/double-conversion

! initialize
    IF (Negative) THEN
        Sign = 1_I8B
    ELSE
        Sign = 0_I8B
    END IF

    Fp%Sig = SigDec
    Fp%Exp = 0
    IF (Aux%Truncated) THEN
        FpErr = ToUnsignedLong(ERR_ULP / 2)
! round up if the next digit after the cut is more than or equal to 5
        IF ((IACHAR(cStr(Aux%SigCut:Aux%SigCut))-A0) >= 5) Fp%Sig = Fp%Sig + 1_I8B
    ELSE
        FpErr = 0_I8B
    END IF

! normalize
    Bits   = LEADZ(Fp%Sig)
    Fp%Sig = SHIFTL(Fp%Sig, Bits)
    Fp%Exp = Fp%Exp - Bits
    FpErr  = SHIFTL(FpErr, Bits)

! multiply and add error
    Fp = Diy_Fp_Mul(Fp, Diy_Fp_Get_Cached_Pow10(ExpDec))
    IF (FpErr == 0_I8B) THEN
        FpErr = FpErr + ToUnsignedLong(ERR_CACHED_POW + ERR_MUL_FIXED)
    ELSE
        FpErr = FpErr + ToUnsignedLong(ERR_CACHED_POW + ERR_MUL_FIXED + 1)
    END IF

! normalize
    Bits   = LEADZ(Fp%Sig)
    Fp%Sig = SHIFTL(Fp%Sig, Bits)
    Fp%Exp = Fp%Exp - Bits
    FpErr  = SHIFTL(FpErr, Bits)

! effective significand
    Order_of_Magnitude = DIY_SIG_BITS + Fp%Exp
    IF (Order_of_Magnitude >= EXP_SUBNORMAL + BinaryPrecision) THEN
        Effective_Significand_Size = BinaryPrecision
    ELSEIF (Order_of_Magnitude <= EXP_SUBNORMAL) THEN
        Effective_Significand_Size = 0
    ELSE
        Effective_Significand_Size = Order_of_Magnitude - EXP_SUBNORMAL
    END IF

! precision digits count
    PrecisionDigitsCount = DIY_SIG_BITS - Effective_Significand_Size
    IF (PrecisionDigitsCount + ERR_ULP_LOG >= DIY_SIG_BITS) THEN
        BLOCK
            INTEGER(KIND=I4B)     :: Shr
            Shr = (PrecisionDigitsCount + ERR_ULP_LOG) - DIY_SIG_BITS + 1
            Fp%Sig = SHIFTR(Fp%Sig, Shr)
            Fp%Exp = Fp%Exp + Shr
            FpErr  = SHIFTR(FpErr, Shr) + ToUnsignedLong(1 + ERR_ULP)
            PrecisionDigitsCount = PrecisionDigitsCount - Shr
        END BLOCK
    END IF

! half way
    PrecisionBits = IAND(Fp%Sig, (SHIFTL(1_I8B, PrecisionDigitsCount) - 1_I8B))
    PrecisionBits = PrecisionBits*ERR_ULP
    HalfWay = SHIFTL(1_I8B, (PrecisionDigitsCount - 1))
    HalfWay = HalfWay*ERR_ULP

! rounding
    Fp%Sig = SHIFTR(Fp%Sig, PrecisionDigitsCount)
    IF (PrecisionBits .UGE. HalfWay + FpErr) Fp%Sig = Fp%Sig + 1_I8B
    Fp%Exp = Fp%Exp + PrecisionDigitsCount

! get IEEE raw value
    RawFP = Diy_Fp_To_IEEE_Raw(Fp)

    IF (RawFP == FpRawInf) THEN
        RawFP = IOR(SHIFTL(Sign, SignBits), RawFP)
        RETURN
    END IF
    IF ((PrecisionBits .ULE. HalfWay - FpErr).OR.(PrecisionBits .UGE. HalfWay + FpErr)) THEN
! number is accurate
        RawFP = IOR(SHIFTL(Sign, SignBits), RawFP)
        RETURN
    END IF

! -------------------------------------------------------------------------
! now the number is the correct value, or the next lower value
! -------------------------------------------------------------------------

! upper boundary
    IF (IAND(RawFP, ExponentMask) /= 0_I8B) THEN
        FpUpper%Sig = IAND(RawFP, SignificandMask) + SHIFTL(1_I8B, SignificandBits)
        FpUpper%Exp = INT(SHIFTR(IAND(RawFP, ExponentMask), SignificandBits), KIND=I4B)
    ELSE
        FpUpper%Sig = IAND(RawFP, SignificandMask)
        FpUpper%Exp = 1
    END IF
    FpUpper%Exp = FpUpper%Exp - (ExponentBias + SignificandBits)
    FpUpper%Sig = SHIFTL(FpUpper%Sig, 1)
    FpUpper%Exp = FpUpper%Exp - 1
    FpUpper%Sig = FpUpper%Sig + 1     ! add half ulp

! compare with BigInt
    Exp10 = ExpDec
    CALL BigInt_Set_String(BigFull, SigDec, Exp10, cStr, Aux)
    CALL BigInt_Set_UIntType(BigComp, FpUpper%Sig)
    IF (Exp10 >= 0) THEN
        CALL BigInt_Mul_Pow10(BigFull, +Exp10)
    ELSE
        CALL BigInt_Mul_Pow10(BigComp, -Exp10)
    END IF
    IF (FpUpper%Exp > 0) THEN
        CALL BigInt_Mul_Pow2(BigComp, +FpUpper%Exp)
    ELSE
        CALL BigInt_Mul_Pow2(BigFull, -FpUpper%Exp)
    END IF

    Cmp = BigInt_Compare(BigFull, BigComp)
    IF (Cmp /= 0) THEN
! round down or round up
        IF (Cmp > 0) RawFP = RawFP + 1_I8B
    ELSE
! falls midway, round to even
        IF (IAND(RawFP, 1_I8B) /= 0_I8B) RawFP = RawFP + 1_I8B
    END IF
    RawFP = IOR(SHIFTL(Sign, SignBits), RawFP)

    RETURN

    CONTAINS

    FUNCTION Diy_Fp_Get_Cached_Pow10(Exp10) RESULT(Fp)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To get cached rounded diy_fp with pow(10, e). The input value must in range
    ! [POW10_SIG_TABLE_MIN_EXP, POW10_SIG_TABLE_MAX_EXP].

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: Exp10    ! an exponent
        TYPE(Diy_Fp)                    :: Fp       ! Diy_Fp data

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(UInt128)       :: Pow10
        INTEGER(KIND=I8B)   :: Sig_Ext

    !** FLOW

        Pow10   = Get_Pow10_128Bits(Exp10)
        Fp%Sig  = Pow10%High
        Sig_Ext = Pow10%Low

        Fp%Exp = Floor_Log2_Pow10(Exp10) - SignBits
        Fp%Sig = Fp%Sig + SHIFTR(Sig_Ext, SignBits)

        RETURN

    END FUNCTION Diy_Fp_Get_Cached_Pow10

!**************************************************************************

    FUNCTION Diy_Fp_Mul(Fp1, Fp2) RESULT(Fp)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To evaluate 'fp1 * fp2'.

            IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
            TYPE(Diy_Fp), INTENT(IN)    :: Fp1, Fp2
            TYPE(Diy_Fp)                :: Fp

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
            INTEGER(KIND=I8B)   :: Hi, Lo

    !** FLOW

            CALL UMul128(Fp1%Sig, Fp2%Sig, Hi, Lo)

            Fp%Sig = Hi + SHIFTR(Lo, SignBits)
            Fp%Exp = Fp1%Exp + Fp2%Exp + TotalBits

            RETURN

    END FUNCTION Diy_Fp_Mul

!**************************************************************************

    FUNCTION Diy_Fp_To_IEEE_Raw(Fp) RESULT(Val)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To convert diy_fp to IEEE-754 raw value.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(Diy_Fp), INTENT(IN)    :: Fp
        INTEGER(KIND=I8B)           :: Val

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)   :: Sig
        INTEGER(KIND=I4B)   :: Exp
        INTEGER(KIND=I4B)   :: Lz_Bits

    !** FLOW

    ! initialize
        Sig = Fp%Sig
        Exp = Fp%Exp
        Val = 0_I8B
        IF (Sig == 0_I8B) RETURN

    ! compute significand and exponent
        Lz_Bits = LEADZ(Sig)
        Sig = SHIFTL(Sig, Lz_Bits)
        Sig = SHIFTR(Sig, ExponentBits)
        Exp = Exp - Lz_Bits + ExponentBits + SignificandBits

    ! check which range the result falls
        IF (Exp >= MaxExpBin) THEN
    ! overflow
            Val = RawFP_SetInfinity(FalseVal)
        ELSEIF (Exp >= (MinExpBin - 1)) THEN
    ! normal
            Exp = Exp + ExponentBias
            Val = IOR(SHIFTL(ToUnsignedLong(Exp), SignificandBits), IAND(Sig, SignificandMask))
        ELSEIF (Exp >= (MinExpBin - BinaryPrecision)) THEN
    ! subnormal
            Val = SHIFTR(Sig, (MinExpBin - Exp - 1))
        ELSE
    ! underflow
            Val = 0_I8B
        END IF

        RETURN

    END FUNCTION Diy_Fp_To_IEEE_Raw

!**************************************************************************

END FUNCTION D2B_YY_SlowPath

!******************************************************************************

FUNCTION Dec2Bin_Lemire(SigDec, ExpDec, Negative, cStr, Aux) RESULT(RawFP)

!** PURPOSE OF THIS SUBROUTINE:
! To use Lemire's algorithm to convert from decimal representation
! to (raw) binary representation

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN) :: SigDec   ! significand, base 10
    INTEGER(KIND=I4B),  INTENT(IN) :: ExpDec   ! exponent, base 10
    LOGICAL,            INTENT(IN) :: Negative ! true if the floating point value is negative
    CHARACTER(LEN=*),   INTENT(IN) :: cStr     ! floating-point number string
    TYPE(StringAux),    INTENT(IN) :: Aux      ! auxiliary string information
    INTEGER(KIND=I8B)              :: RawFP    ! floating point number as an unsigned integer

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B),  PARAMETER :: MinExpFastPath = - DecimalRange - UIntSafeDigits + 1
    INTEGER(KIND=I4B),  PARAMETER :: MaxExpFastPath = DecimalRange + 2

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: SigBin   ! significand, base 2
    INTEGER(KIND=I4B)   :: ExpBin   ! exponent, base 2

!** FLOW

! check whether to use Lemire's fast path
    IF ((.NOT.Aux%Truncated).AND.(ExpDec > MinExpFastPath).AND.(ExpDec < MaxExpFastPath)) THEN
        IF (D2B_Lemire_FastPath(SigDec, ExpDec, SigBin, ExpBin)) THEN
! Lemire's fast path is success so set sign bit
            IF (Negative) THEN
                RawFP = SignMask
            ELSE
                RawFP = 0_I8B
            END IF
! then, add exponent bits
            RawFP = IOR(RawFP, SHIFTL(ToUnsignedLong(ExpBin), SignificandBits))
! finally, add (both implicit and explicit) significand bits
            RawFP = IOR(RawFP, IAND(SigBin, SignificandMask))
            RETURN
        END IF
    END IF

! perform decimal to binary conversion using YY's slow path
    RawFP = D2B_YY_SlowPath(SigDec, ExpDec, Negative, cStr, Aux)

    RETURN

    CONTAINS

    FUNCTION D2B_Lemire_FastPath(SigDec, ExpDec, SigBin, ExpBin) RESULT(Success)

!DIR$ ATTRIBUTES FORCEINLINE :: D2B_Lemire_FastPath

    !** PURPOSE OF THIS SUBROUTINE:
    ! To use Lemire's fast path algorithm to convert from decimal to binary representation.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: SigDec   ! significand in base 10
        INTEGER(KIND=I4B), INTENT(IN)   :: ExpDec   ! exponent in base 10
        INTEGER(KIND=I8B), INTENT(OUT)  :: SigBin   ! significand in base 2
        INTEGER(KIND=I4B), INTENT(OUT)  :: ExpBin   ! exponent in base 2
        LOGICAL                         :: Success  ! true if conversion can be handled

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(UInt128)       :: Pow10
        INTEGER(KIND=I4B)   :: LZ
        INTEGER(KIND=I4B)   :: Upperbit
        INTEGER(KIND=I8B)   :: Significand
        INTEGER(KIND=I8B)   :: Lower, Upper
        INTEGER(KIND=I4B)   :: Exponent

    !** FLOW

    ! get 256/128/64-bit approximation of power of 10 (or power of 5)
        Pow10 = Get_Pow10_128Bits(ExpDec)

    ! compute the exponent
        Exponent = Floor_Log2_Pow10(ExpDec) + MaxExpBin + SignBits

    ! +++ normalize the significand +++
    ! We want the most significant bit of Significand to be 1. Shift if needed.
        LZ = LEADZ(SigDec)
        Significand = SHIFTL(SigDec, LZ)

    ! +++ perform multiplication +++
    ! We want the most significant 128/64/32 bits of the product. We know this will be non-zero
    ! because the most significant bit of Significand is 1.
        CALL UMul128(Significand, Pow10%High, Upper, Lower)

    ! We know that Upper has at most one leading zero because both Significand and  Pow10 have a leading one.
    ! As long as the first 13/9/6 bits of "upper" are not "1", then we know that we have an exact computed
    ! value for the leading 125/55/26 bits because any imprecision would play out as a +1, in the worst case.
    ! Having 125/55/26 bits is necessary because we need 123/53/24 bits for the mantissa but we have to have
    ! one rounding bit and we can waste a bit if the most significant bit of the product is zero. We expect
    ! this next branch to be rarely taken (say 1% of the time). When (Upper & BitMask) == BitMask,
    ! it can be common for Lower + Significand < Lower to be true (proba. much higher than 1%).
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        IF ((IAND(Upper, BitMask) == BitMask).AND.((Lower + Significand) .ULT. Lower)) THEN
            BLOCK
                ! --- declaration ---
                INTEGER(KIND=I8B)   :: Product_Low, Product_High
                INTEGER(KIND=I8B)   :: Product_Middle, Product_Middle1, Product_Middle2
                ! --- execution ---
    ! perform multiplication
                CALL UMul128(Significand, Pow10%Low, Product_Middle2, Product_Low)
                Product_Middle1 = Lower
                Product_High    = Upper
                Product_Middle  = Product_Middle1 + Product_Middle2
    ! overflow carry
                IF (Product_Middle .ULT. Product_Middle1) Product_High = Product_High + 1_I8B
    ! we want to check whether Pow10*Significand + Significand would affect our result
    ! This does happen, e.g. with 7.3177701707893310E+15 (for double-precision)
                IF (((Product_Middle + 1_I8B == 0_I8B).AND.(IAND(Product_High, BitMask) == BitMask) &
                    .AND.(Product_Low + Significand .ULT. Product_Low))) THEN
    ! let us be prudent and bail out.
                    Success = FalseVal
                    RETURN
                END IF
                Lower = Product_Middle
                Upper = Product_High
            END BLOCK
        END IF

    ! The final mantissa should be 123/53/24 (BinaryPrecision) bits with a leading 1.
    ! We shift it so that it occupies 124/54/25 (BinaryPrecision+1) bits with a leading 1.
        Upperbit = INT(SHIFTR(Upper, SignBits), KIND=I4B)
        SigBin   = SHIFTR(Upper, (Upperbit + LowBits))
        LZ = LZ + IEOR(1, Upperbit)

    ! Here we have SigBin < SHIFTL(1, BinaryPrecision+1).

    ! We have to round to even. The "to even" part
    ! is only a problem when we are right in between two floats
    ! which we guard against.
    ! If we have lots of trailing zeros, we may fall right between two
    ! floating-point values.
        IF ((Lower == 0_I8B).AND.(IAND(Upper, BitMask) == 0_I8B).AND. &
            (IAND(SigBin, ThreeUInt) == 1_I8B)) THEN
    ! if IAND(SigBin, 1) == 1 we might need to round up.
    ! Scenarios:
    ! 1. We are not in the middle. Then we should round up.
    ! 2. We are right in the middle. Whether we round up depends on the last significant
    !    bit: if it is "one" then we round up (round to even) otherwise, we do not.
    ! So if the last significant bit is 1, we can safely round up.  Hence we only need
    ! to bail out if IAND(SigBin, 3) == 1.  Otherwise we may need more accuracy or analysis
    ! to determine whether we are exactly between two floating-point numbers.
    ! It can be triggered with 1E23.
    ! Note: because the factor_mantissa and factor_mantissa_low are almost always rounded
    !       down (except for small positive powers), almost always should round up.
            Success = FalseVal
            RETURN
        END IF
        SigBin = SHIFTR(SigBin + IAND(SigBin, 1_I8B), 1)
    ! Here we have SigBin < SHIFTL(1, BinaryPrecision), unless there was an overflow
        IF (SigBin .UGE. MaxMantissa) THEN
    ! This will happen when parsing values such as 7.2057594037927933E+16
            SigBin = SigHidBitMask
    ! undo previous addition
            LZ = LZ - 1
        END IF
        SigBin = IAND(SigBin, NOT(SigHidBitMask))
        ExpBin = Exponent - LZ
    ! we have to check that ExpBin is in range, otherwise we bail out
        IF ((ExpBin < 1).OR.(ExpBin > (MaxExponent-1))) THEN
            Success = FalseVal
        ELSE
            Success = TrueVal
        END IF

        RETURN

    END FUNCTION D2B_Lemire_FastPath

!**************************************************************************

END FUNCTION Dec2Bin_Lemire

!******************************************************************************

!------------------------------------------------------------------------------
!
!                           REAL64 AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION DivByPow10(X, P) RESULT(Y)

!** PURPOSE OF THIS SUBROUTINE:
! To compute Y = X .UDIV. (10**P)

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN) :: X    ! X <= 10**20
    INTEGER(KIND=I4B), INTENT(IN) :: P    ! 1 <= P <= 10
    INTEGER(KIND=I8B)             :: Y

!** SUBROUTINE IPARAMETER DECLARATIONS:
! Parameters for division by power of 10 applicable for N <= 20 digits
! (i.e. used for division of the 'Significand')
! Note: elements in the row are in little-endian order
! (i.e. element 0 is the least significant byte and element 1 is the most one)
    INTEGER(KIND=I8B), PARAMETER  :: MagicM(0:1,1:10) = RESHAPE([               &
        INT(Z'6666666666666667', KIND=I8B), INT(Z'0000000000000006', KIND=I8B), &
        INT(Z'3D70A3D70A3D70A4', KIND=I8B), INT(Z'000000000000000A', KIND=I8B), &
        INT(Z'3126E978D4FDF3B7', KIND=I8B), INT(Z'0000000000000008', KIND=I8B), &
        INT(Z'8DB8BAC710CB295F', KIND=I8B), INT(Z'0000000000000006', KIND=I8B), &
        INT(Z'7C5AC471B4784231', KIND=I8B), INT(Z'000000000000000A', KIND=I8B), &
        INT(Z'637BD05AF6C69B5B', KIND=I8B), INT(Z'0000000000000008', KIND=I8B), &
        INT(Z'B5FCA6AF2BD215E2', KIND=I8B), INT(Z'0000000000000006', KIND=I8B), &
        INT(Z'BCC77118461CEFD0', KIND=I8B), INT(Z'000000000000000A', KIND=I8B), &
        INT(Z'9705F4136B4A5974', KIND=I8B), INT(Z'0000000000000008', KIND=I8B), &
        INT(Z'DF37F675EF6EADF6', KIND=I8B), INT(Z'0000000000000006', KIND=I8B)], [2,10])
    INTEGER(KIND=I4B), PARAMETER  :: MagicS(1:10) = [70, 74, 77, 80, 84, 87, 90, 94, 97, 100]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: MulProduct(0:2)
    INTEGER(KIND=I8B)     :: Input(0:0)
    INTEGER(KIND=I8B)     :: Multiplier(0:1)
    INTEGER(KIND=I4B)     :: Shift

!** FLOW

    Input(0)   = X
    Multiplier = MagicM(:,P)
    Shift      = MagicS(P)
    CALL Multiply_N_ShiftRight(Input, 1, Multiplier, 2, Shift, MulProduct)
    Y = MulProduct(0)

    RETURN

END FUNCTION DivByPow10

!******************************************************************************

FUNCTION Write_RealDP(Fp, Ep, cStr, IsScientific) RESULT(sLen)

!DIR$ ATTRIBUTES INLINE :: Write_RealDP

!** PURPOSE OF THIS SUBROUTINE:
! To format the decimal F*10**E

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)      :: Fp           ! significand
    INTEGER(KIND=I4B),  INTENT(IN)      :: Ep           ! exponent
    CHARACTER(LEN=*),   INTENT(INOUT)   :: cStr         ! character string
    LOGICAL, OPTIONAL,  INTENT(IN)      :: IsScientific ! format flag
                                                        ! true  if to write the given number in scientific format
                                                        ! false if to write the given number in general format
                                                        ! default is false
    INTEGER(KIND=I4B)                   :: sLen         ! length of string written

!** SUBROUTINE PARAMETER DECLARATIONS:
! maximum number of significant digits (i.e. the maximum decimal precision
!   that guarantees an error-free write-read cycle.)
    INTEGER(KIND=I4B), PARAMETER  :: H = 17
! shift and multiplier parameters (i.e. magic number) for integer division
    INTEGER(KIND=I4B), PARAMETER  :: S98   = 57
    INTEGER(KIND=I8B), PARAMETER  :: M98   = 1441151881_I8B
    INTEGER(KIND=I4B), PARAMETER  :: S178  = 20                         ! = 84-64
    INTEGER(KIND=I8B), PARAMETER  :: M178  = 193428131138340668_I8B
    INTEGER(KIND=I8B), PARAMETER  :: DivE8 = 100000000_I8B
! The first powers of 10. The last entry must be 10^H.
    INTEGER(KIND=I4B)             :: I
    INTEGER(KIND=I8B), PARAMETER  :: Pow10(0:H) = [(10_I8B**I, I = 0, H)]
! Used for left-to-tight digit extraction.
    INTEGER(KIND=I4B), PARAMETER  :: MASK_28 = SHIFTL(1, 28) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: F, HM
    INTEGER(KIND=I4B)   :: E, HF, MF, LF
    LOGICAL             :: IsGeneral    ! true if to write the given number in general format

!** FLOW

! check for special cases
    IF (Ep == ExceptionalExponent) THEN
! either NaN or Infinity
        IF (Fp /= 0_I8B) THEN
            cStr(1:3) = 'NaN'
            sLen = 3
        ELSE
            cStr(1:8) = 'Infinity'
            sLen = 8
        END IF
        RETURN
    END IF

    IF (Fp == 0_I8B) THEN
! zero
        cStr(1:3) = '0.0'
        sLen = 3
        RETURN
    END IF

! For details not discussed here see section 10 of [3].
! Determine sLen such that 10**(sLen-1) <= F < 10**sLen
    sLen = Floor_Log10_Pow2(64 - LEADZ(Fp))
    IF (Fp >= Pow10(sLen)) sLen = sLen + 1

! Let Fp and Ep be the original F and E, respectively.
! Transform F and E to ensure
!    10**(H-1) <= F < 10**H
!    Fp*10**Ep = F*10**(E-H) = 0.F*10**E
    F = Fp*Pow10(H - sLen)
    E = Ep + sLen

! ToChars perform digits extraction using integers,
! provided that the arguments are limited to 8 digits.
! Therefore, split the H = 17 digits of F into:
!     HF = the most significant digit of F
!     MF = the next 8 most significant digits of F
!     LF = the last 8, least significant digits of F
!
! For N = 17, M = 8 the table in section 10 of [2] shows
!     Floor(F/10**8) = Floor(193,428,131,138,340,668*F/2**84) =
!     Floor(Floor(193,428,131,138,340,668*F/2**64) / 2**20)
! and for N = 9, M = 8
!     Floor(HM/10**8) = Floor(1,441,151,881*HM/2**57)
!
    HM = SHIFTR(UMul128_Upper64(F, M178), S178)
    LF = INT(F - DivE8*HM, KIND=I4B)
    HF = INT(SHIFTR(HM*M98, S98), KIND=I4B)
    MF = INT(HM - DivE8*INT(HF, KIND=I8B), KIND=I4B)

! set format flag
    IsGeneral = TrueVal
    IF (PRESENT(IsScientific)) IsGeneral = .NOT.IsScientific
! write output
    IF (IsGeneral) THEN
        IF ((0 < E).AND.(E <= 7)) THEN
! plain format without leading zeros
            sLen = ToChar_Plain_Without_LZ(HF, MF, LF, E, cStr)
        ELSEIF ((-3 < E).AND.(E <= 0)) THEN
! plain format with leading zeros
            sLen = ToChar_Plain_With_LZ(HF, MF, LF, E, cStr)
        ELSE
! scientific notation
            sLen = ToChar_Scientific(HF, MF, LF, E, cStr)
        END IF
    ELSE
! scientific notation
        sLen = ToChar_Scientific(HF, MF, LF, E, cStr)
    END IF

    RETURN

    CONTAINS

    FUNCTION ToChar_Plain_Without_LZ(H, M, L, E, cStr) RESULT(sLen)

!DIR$ ATTRIBUTES FORCEINLINE :: ToChar_Plain_Without_LZ

    !** PURPOSE OF THIS SUBROUTINE:
    ! For 0 < E <= 7, plain format without leading zeros.
    ! Left-to-right digits extraction:
    ! algorithm 1 in [7], with b = 10, k = 8, n = 28.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),   INTENT(IN)       :: H        ! high digit
        INTEGER(KIND=I4B),   INTENT(IN)       :: M        ! middle digits
        INTEGER(KIND=I4B),   INTENT(IN)       :: L        ! low digits
        INTEGER(KIND=I4B),   INTENT(IN)       :: E        ! exponent
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B)                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: Y, T, I, Pos

    !** FLOW

        cStr(1:1) = Char1Digit(H)
        Pos = 2
    ! Algorithm 1 in [7] needs computation of floor((a + 1) 2^n / b^k) - 1
    ! with a < 10^8, b = 10, k = 8, n = 28.
    ! Noting that (a + 1) 2^n <= 10^8 2^28 < 10^17
    ! For n = 17, m = 8 the table in section 10 of [3] leads to:
        Y = INT(SHIFTR(UMul128_Upper64(SHIFTL(INT(M+1, KIND=I8B), 28), M178), S178), KIND=I4B) - 1
        I = 1
        DO WHILE (I < E)
            T = 10*Y
    ! append digit
            cStr(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
    ! append period
        cStr(Pos:Pos) = '.'
        Pos = Pos + 1
        DO WHILE (I <= 8)
            T = 10*Y
    ! append digit
            cStr(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
    ! append L
        Pos = Pos + Write_I32_8_Digits(L, cStr(Pos:)) - 1
    ! remove trailing zero(s)
        DO WHILE (cStr(Pos:Pos) == '0')
            Pos = Pos - 1
        END DO
    ! ... but do not remove the one directly to the right of '.'
        IF (cStr(Pos:Pos) == '.') Pos = Pos + 1

    ! set length
        sLen = Pos

        RETURN

    END FUNCTION ToChar_Plain_Without_LZ

!**************************************************************************

    FUNCTION ToChar_Plain_With_LZ(H, M, L, E, cStr) RESULT(sLen)

!DIR$ ATTRIBUTES FORCEINLINE :: ToChar_Plain_With_LZ

    !** PURPOSE OF THIS SUBROUTINE:
    ! For -3 < E <= 0: plain format with leading zeros.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),   INTENT(IN)       :: H        ! high digit
        INTEGER(KIND=I4B),   INTENT(IN)       :: M        ! middle digits
        INTEGER(KIND=I4B),   INTENT(IN)       :: L        ! low digits
        INTEGER(KIND=I4B),   INTENT(IN)       :: E        ! exponent
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B)                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: Y, T, I, Pos

    !** FLOW

    ! fill the first 4 characters
        cStr(1:4) = '0.00'
    ! compute Pos
        Pos = 3 - E
    ! append H
        cStr(Pos:Pos) = Char1Digit(H)
        Pos = Pos + 1
    ! append M and L
        Pos = Pos + Write_2I32_16_Digits(M, L, cStr(Pos:)) - 1
    ! remove trailing zero(s)
        DO WHILE (cStr(Pos:Pos) == '0')
            Pos = Pos - 1
        END DO
    ! ... but do not remove the one directly to the right of '.'
        IF (cStr(Pos:Pos) == '.') Pos = Pos + 1

    ! set length
        sLen = Pos

        RETURN

    END FUNCTION ToChar_Plain_With_LZ

!**************************************************************************

    FUNCTION ToChar_Scientific(H, M, L, E, cStr) RESULT(sLen)

!DIR$ ATTRIBUTES FORCEINLINE :: ToChar_Scientific

    !** PURPOSE OF THIS SUBROUTINE:
    ! For E <= -3 or E > 7: computerized scientific notation.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),   INTENT(IN)       :: H        ! high digit
        INTEGER(KIND=I4B),   INTENT(IN)       :: M        ! middle digits
        INTEGER(KIND=I4B),   INTENT(IN)       :: L        ! low digits
        INTEGER(KIND=I4B),   INTENT(IN)       :: E        ! exponent
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B)                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)    :: Y, T, I, Pos

    !** FLOW

    ! append H
        cStr(1:1) = Char1Digit(H)
    ! append period
        cStr(2:2) = '.'
        Pos = 3
    ! append M and L
        Pos = Pos + Write_2I32_16_Digits(M, L, cStr(Pos:)) - 1
    ! remove trailing zero(s)
        DO WHILE (cStr(Pos:Pos) == '0')
            Pos = Pos - 1
        END DO
    ! ... but do not remove the one directly to the right of '.'
        IF (cStr(Pos:Pos) == '.') Pos = Pos + 1

    ! append exponent
        Pos = Pos + 1
        cStr(Pos:Pos) = 'E'
        sLen = Pos + Write_I32_Exponent(E-1, cStr(Pos+1:))

        RETURN

    END FUNCTION ToChar_Scientific

!**************************************************************************

    FUNCTION Write_I32_8_Digits(Number, cStr) RESULT(sLen)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_I32_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
    ! To write an (unsigned) integer number with a length of 8 digits

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),   INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B)                     :: sLen     ! length of string written

    !** SUBROUTINE PARAMETER DECLARATIONS:
    ! shift and multiplier parameters (i.e. magic number) for integer division
        INTEGER(KIND=I4B), PARAMETER  :: Shf78 = 40
        INTEGER(KIND=I8B), PARAMETER  :: Mul78 = 109951163_I8B
        INTEGER(KIND=I4B), PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: NxtNum, RemNum

    !** FLOW

    ! compute NxtNum = PosNum/10000
        NxtNum = INT(SHIFTR(INT(Number, KIND=I8B)*Mul78, Shf78), KIND=I4B)

    ! compute RemNum = MOD(PosNum, 10000)
        RemNum = Number - NxtNum*Divisor

    ! convert the remainder to a working string
        cStr(5:8) = Char4Digits(RemNum)

    ! convert the rest (NxtNum)
        cStr(1:4) = Char4Digits(NxtNum)

        sLen = 8

        RETURN

    END FUNCTION Write_I32_8_Digits

!**************************************************************************

    FUNCTION Write_2I32_16_Digits(FirstNum, SecondNum, cStr) RESULT(sLen)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_2I32_16_Digits

    !** PURPOSE OF THIS SUBROUTINE:
    ! To write two (unsigned) integer numbers with a length of 16 digits

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),   INTENT(IN)       :: FirstNum     ! first number
        INTEGER(KIND=I4B),   INTENT(IN)       :: SecondNum    ! first number
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr         ! character string
        INTEGER(KIND=I4B)                     :: sLen         ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: DumLen

    !** FLOW

    ! write first number
        DumLen = Write_I32_8_Digits(FirstNum, cStr(1:8))

    ! write second number
        DumLen = Write_I32_8_Digits(SecondNum, cStr(9:16))

    ! set length
        sLen = 16

        RETURN

    END FUNCTION Write_2I32_16_Digits

!**************************************************************************

    FUNCTION Write_I32_Exponent(Exp, cStr) RESULT(sLen)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_I32_Exponent

    !** PURPOSE OF THIS SUBROUTINE:
    ! To write a signed integer in the range -324 to 308

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),   INTENT(IN)       :: Exp      ! exponent number
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B)                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: PosExp

    !** FLOW

        IF (Exp < 0) THEN
            cStr(1:1) = '-'
        ELSE
            cStr(1:1) = '+'
        END IF
        PosExp = ABS(Exp)
        IF (PosExp < 100) THEN
            IF (PosExp < 10) THEN
    ! 1 digit
                cStr(2:2) = Char1Digit(PosExp)
                sLen = 2
            ELSE
    ! 2 digits
                cStr(2:3) = Char2Digits(PosExp)
                sLen = 3
            END IF
        ELSE
    ! 3 digits
            cStr(2:4) = Char4Digits(PosExp)(2:4)
            sLen = 4
        END IF

        RETURN

    END FUNCTION Write_I32_Exponent

!**************************************************************************

END FUNCTION Write_RealDP

!******************************************************************************

END MODULE ModBase_RealDP_CharConv

!******************************************************************************
