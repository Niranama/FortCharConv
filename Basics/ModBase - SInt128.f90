
MODULE ModBase_SInt128

!** PURPOSE OF THIS MODULE:
    ! This module contains a derived type and basic operations for a signed 128-bit integer.

!** REFERENCES:
    ! [1] Absl's Numeric Library: https://github.com/abseil/abseil-cpp/tree/master/absl/numeric
    ! [2] Fast 128-bit math library for Java: https://github.com/martint/int128/
    ! [3] Extended precision integer C++ library: https://github.com/chfast/intx

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Error_Handlers
    USE ModBase_SIntUtil
    USE ModBase_UIntUtil
    USE ModBase_UInt128
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE, INTRINSIC :: ISO_FORTRAN_ENV,  ONLY: OUTPUT_UNIT
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type + constructor
    PUBLIC :: SInt128
    ! assignment + conversion
    PUBLIC :: ASSIGNMENT(=)
    PUBLIC :: ToR32, ToR64, ToR128
    PUBLIC :: ToU32, ToU64, ToU128
    PUBLIC :: ToDecString, ToHexString
    ! comparision
    PUBLIC :: OPERATOR(==), OPERATOR(/=)
    PUBLIC :: OPERATOR(<), OPERATOR(<=)
    PUBLIC :: OPERATOR(>), OPERATOR(>=)
    PUBLIC :: Compare
    ! arithmetic
    PUBLIC :: OPERATOR(+), OPERATOR(-)
    PUBLIC :: OPERATOR(*), OPERATOR(/)
    PUBLIC :: Increment, Decrement, Add, Subtract
    PUBLIC :: Multiply, Divide, MOD, DivMod
    ! bitwise
    PUBLIC :: SHIFTL, SHIFTR, SHIFTA, ISHFT, ISHFTC
    PUBLIC :: IOR, IEOR, IAND, NOT, LEADZ, TRAILZ
    PUBLIC :: POPCNT, POPPAR, IBSET, IBCLR
    PUBLIC :: IBCHNG, BTEST, IBITS
    PUBLIC :: MoveBits ! == MVBITS
    ! bitwise (specialized)
    PUBLIC :: ShiftLOnce, ShiftROnce, ShiftAOnce
    PUBLIC :: ShiftL64, ShiftR64, ShiftA64
    PUBLIC :: ShiftL63Down, ShiftR63Down, ShiftA63Down
    PUBLIC :: ShiftL64Up, ShiftR64Up, ShiftA64Up
    ! inquiry
    PUBLIC :: IsPositive, IsNegative, IsZero
    ! auxiliary
    PUBLIC :: ABS
    PUBLIC :: Display

    PRIVATE          ! by default, hide all data and routines except those declared explicitly
    
!** MODULE PARAMETERS:
    ! module name
    CHARACTER(LEN=*),  PARAMETER    :: ModName = 'ModBase_SInt128'
    ! unsigned limit parameters
    INTEGER(KIND=I8B), PARAMETER    :: MaxU64 = INT(Z'FFFFFFFFFFFFFFFF', KIND=I8B)    ! max unsigned 64-bit
    INTEGER(KIND=I8B), PARAMETER    :: MinU64 = INT(Z'0000000000000000', KIND=I8B)    ! min unsigned 64-bit
    INTEGER(KIND=I8B), PARAMETER    :: MaxU32 = INT(Z'00000000FFFFFFFF', KIND=I8B)    ! max unsigned 32-bit
    INTEGER(KIND=I8B), PARAMETER    :: MinU32 = INT(Z'0000000000000000', KIND=I8B)    ! min unsigned 32-bit
    ! signed limit parameters
    INTEGER(KIND=I8B), PARAMETER    :: MaxI64 = INT(Z'7FFFFFFFFFFFFFFF', KIND=I8B)    ! max signed 64-bit
    INTEGER(KIND=I8B), PARAMETER    :: MinI64 = INT(Z'8000000000000000', KIND=I8B)    ! min signed 64-bit
    INTEGER(KIND=I8B), PARAMETER    :: MaxI32 = INT(Z'000000007FFFFFFF', KIND=I8B)    ! max signed 32-bit
    INTEGER(KIND=I8B), PARAMETER    :: MinI32 = INT(Z'0000000080000000', KIND=I8B)    ! min signed 32-bit
    ! miscellaneous
    INTEGER(KIND=I8B), PARAMETER    :: Mask32 = MaxU32
    INTEGER(KIND=I8B), PARAMETER    :: TopBit = SHIFTL(1_I8B, 63)
    LOGICAL,           PARAMETER    :: Positive = FalseVal

!** DERIVED TYPE DEFINITIONS
	TYPE SInt128
        INTEGER(KIND=I8B) :: High     ! number representing upper 64 bits treated as signed
        INTEGER(KIND=I8B) :: Low      ! number representing lower 64 bits treated as unsigned
    END TYPE SInt128

!** MODULE PARAMETERS (PART 2):
    ! SInt128 parameters
    TYPE(SInt128), PARAMETER, PUBLIC    :: MaxI128  = SInt128(MaxI64, MaxU64)
    TYPE(SInt128), PARAMETER, PUBLIC    :: MinI128  = SInt128(MinI64, MinU64)
    TYPE(SInt128), PARAMETER, PUBLIC    :: OneI128  = SInt128(0_I8B, 1_I8B)
    TYPE(SInt128), PARAMETER, PUBLIC    :: ZeroI128 = SInt128(0_I8B, 0_I8B)
    TYPE(SInt128), PARAMETER            :: TenI128  = SInt128(0_I8B, 10_I8B)

!** INTERFACE DEFINITIONS:
    ! na

!** GENERIC DEFINITIONS:
    ! assignment (for conversions between SInt128 and unsigned 32/64 bit integers)
    GENERIC     :: ASSIGNMENT(=)    => I128_From_I32, I128_From_I64, &
                                       I128_To_I32,   I128_To_I64
    ! constructor
    GENERIC     :: SInt128          => I32_To_I128, I64_To_I128, &
                                       U32_To_I128, U64_To_I128, R32_To_I128,    &
                                       R64_To_I128, R128_To_I128, DecString_To_I128
    ! conversion
    GENERIC     :: ToU32            => U32_From_I128
    GENERIC     :: ToU64            => U64_From_I128
    GENERIC     :: ToR32            => R32_From_I128
    GENERIC     :: ToR64            => R64_From_I128
    GENERIC     :: ToR128           => R128_From_I128
    GENERIC     :: ToU128           => U128_From_I128
    GENERIC     :: ToDecString      => DecString_From_I128
    GENERIC     :: ToHexString      => HexString_From_I128
    ! comparision
    GENERIC     :: OPERATOR(==)     => I128_Equal
    GENERIC     :: OPERATOR(/=)     => I128_NotEqual
    GENERIC     :: OPERATOR(<)      => I128_LessThan
    GENERIC     :: OPERATOR(<=)     => I128_LessEqual
    GENERIC     :: OPERATOR(>)      => I128_GreaterThan
    GENERIC     :: OPERATOR(>=)     => I128_GreaterEqual
    GENERIC     :: Compare          => I128_Compare
    ! arithmetic
    GENERIC     :: OPERATOR(+)      => I128_UnaryPlus,    I128_Plus_I128,    &
                                       I128_Plus_I32,     I32_Plus_I128,     &
                                       I128_Plus_I64,     I64_Plus_I128
    GENERIC     :: OPERATOR(-)      => I128_Negate,       I128_Minus_I128,   &
                                       I128_Minus_I32,    I32_Minus_I128,    &
                                       I128_Minus_I64,    I64_Minus_I128
    GENERIC     :: OPERATOR(*)      => I128_Multiply_I128,                   &
                                       I128_Multiply_I32, I32_Multiply_I128, &
                                       I128_Multiply_I64, I64_Multiply_I128
    GENERIC     :: OPERATOR(/)      => I128_Divide_I32,   I128_Divide_I64,   &
                                       I128_Divide_I128
    GENERIC     :: MOD              => I128_Mod_I32,      I128_Mod_I64,      &
                                       I128_Mod_I128
    GENERIC     :: DivMod           => I128_DivMod_I32,   I128_DivMod_I64,   &
                                       I128_DivMod_I128
    GENERIC     :: Increment        => I128_Increment
    GENERIC     :: Decrement        => I128_Decrement
    GENERIC     :: Add              => I128_Add_I32,      I128_Add_I64,      &
                                       I128_Add_I128
    GENERIC     :: Subtract         => I128_Subtract_I32, I128_Subtract_I64, &
                                       I128_Subtract_I128
    GENERIC     :: Multiply         => I128_Times_I32,    I128_Times_I64,    &
                                       I128_Times_I128
    GENERIC     :: Divide           => I128_Over_I32,     I128_Over_I64,     &
                                       I128_Over_I128
    ! bitwise
    GENERIC     :: ShiftLOnce       => I128_ShiftL_Once
    GENERIC     :: ShiftROnce       => I128_ShiftR_Once
    GENERIC     :: ShiftAOnce       => I128_ShiftA_Once
    GENERIC     :: ShiftL64         => I128_ShiftL_64
    GENERIC     :: ShiftR64         => I128_ShiftR_64
    GENERIC     :: ShiftA64         => I128_ShiftA_64
    GENERIC     :: ShiftL63Down     => I128_ShiftL_63Down
    GENERIC     :: ShiftR63Down     => I128_ShiftR_63Down
    GENERIC     :: ShiftA63Down     => I128_ShiftA_63Down
    GENERIC     :: ShiftL64Up       => I128_ShiftL_64Up
    GENERIC     :: ShiftR64Up       => I128_ShiftR_64Up
    GENERIC     :: ShiftA64Up       => I128_ShiftA_64Up
    GENERIC     :: SHIFTL           => I128_ShiftLeft
    GENERIC     :: SHIFTA           => I128_ShiftRightArithmetic
    GENERIC     :: SHIFTR           => I128_ShiftRightLogical
    GENERIC     :: ISHFT            => I128_ShiftLogical
    GENERIC     :: ISHFTC           => I128_Rotate
    GENERIC     :: NOT              => I128_Not
    GENERIC     :: IOR              => I128_Ior
    GENERIC     :: IEOR             => I128_Ieor
    GENERIC     :: IAND             => I128_Iand
    GENERIC     :: LEADZ            => I128_LeadingZeros
    GENERIC     :: TRAILZ           => I128_TrailingZeros
    GENERIC     :: POPCNT           => I128_Count1Bits
    GENERIC     :: POPPAR           => I128_Parity
    GENERIC     :: IBSET            => I128_SetBit
    GENERIC     :: IBCLR            => I128_ClearBit
    GENERIC     :: IBCHNG           => I128_FlipBit
    GENERIC     :: BTEST            => I128_TestBit
    GENERIC     :: IBITS            => I128_ExtractBits
    GENERIC     :: MoveBits         => I128_MoveBits
    ! inquiry
    GENERIC     :: IsPositive       => I128_Is_Positive
    GENERIC     :: IsNegative       => I128_Is_Negative
    GENERIC     :: IsZero           => I128_Is_Zero
    ! auxiliary
    GENERIC     :: BitCastToSigned  => U64_To_I64
    GENERIC     :: UABS             => I128_UnsignedAbsolute
    GENERIC     :: ABS              => I128_Absolute
    GENERIC     :: Display          => I128_Write

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!------------------------------------------------------------------------------
!
!                           ASSIGNMENT ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE I128_From_I32(I128, I32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 32-bit integer number to a signed 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(OUT)   :: I128
    INTEGER(KIND=I4B),  INTENT(IN)    :: I32      ! number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I128%High = SHIFTA(INT(I32, KIND=I8B), 63)
    I128%Low  = INT(I32, KIND=I8B)

    RETURN
            
END SUBROUTINE I128_From_I32

!******************************************************************************

SUBROUTINE I128_From_I64(I128, I64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 64-bit integer number to a signed 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(OUT)   :: I128
    INTEGER(KIND=I8B),  INTENT(IN)    :: I64      ! number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I128%High = SHIFTA(I64, 63)
    I128%Low  = I64
 
    RETURN
            
END SUBROUTINE I128_From_I64

!******************************************************************************

SUBROUTINE I128_To_I32(I32, I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 128-bit integer number to a signed 32-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(OUT)   :: I32      ! number treated as signed
    TYPE(SInt128),      INTENT(IN)    :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I32 = INT(BitCastToSigned(I128%Low), KIND=I4B)
 
    RETURN
            
END SUBROUTINE I128_To_I32

!******************************************************************************

SUBROUTINE I128_To_I64(I64, I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 128-bit integer number to a signed 64-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(OUT)   :: I64      ! number treated as signed
    TYPE(SInt128),      INTENT(IN)    :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I64 = BitCastToSigned(I128%Low)
 
    RETURN
            
END SUBROUTINE I128_To_I64

!------------------------------------------------------------------------------
!
!                           CONSTRUCTOR ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION I32_To_I128(I32) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 32-bit integer number to a signed 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: I32      ! number treated as signed
    TYPE(SInt128)                   :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I128%High = SHIFTA(INT(I32, KIND=I8B), 63)
    I128%Low  = INT(I32, KIND=I8B)
 
    RETURN
            
END FUNCTION I32_To_I128

!******************************************************************************

FUNCTION I64_To_I128(I64) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 64-bit integer number to a signed 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: I64      ! number treated as signed
    TYPE(SInt128)                   :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I128%High = SHIFTA(I64, 63)
    I128%Low  = I64

    RETURN
            
END FUNCTION I64_To_I128

!******************************************************************************

FUNCTION U32_To_I128(U32, Negative) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 32-bit integer number to a signed 128-bit integer number
    ! where the sign flag is used to indicate whether the 128-bit integer value is
    ! positive or negative

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)  :: U32      ! number treated as unsigned
    LOGICAL,            INTENT(IN)  :: Negative ! true if the 128-bit integer value is negative
                                                ! otherwise, the 128-bit integer value is positive
    TYPE(SInt128)                   :: I128

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER    :: Mask = INT(Z'00000000FFFFFFFF', KIND=I8B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: U32L

!** FLOW

    IF (Negative) THEN
        I128%High = MaxU64      ! MaxU64 = NOT(0_I8B)
        U32L = IAND(INT(U32, KIND=I8B), Mask)
        IF (U32L == 0_I8B) I128%High = I128%High + 1_I8B
        I128%Low = NOT(U32L) + 1_I8B
    ELSE
        I128%High = 0_I8B
        I128%Low  = IAND(INT(U32, KIND=I8B), Mask)
    END IF
 
    RETURN

END FUNCTION U32_To_I128

!******************************************************************************

FUNCTION U64_To_I128(U64, Negative) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 64-bit integer number to a signed 128-bit integer number
    ! where the sign flag is used to indicate whether the 128-bit integer value is
    ! positive or negative

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)  :: U64      ! number treated as unsigned
    LOGICAL,            INTENT(IN)  :: Negative ! true if the 128-bit integer value is negative
                                                ! otherwise, the 128-bit integer value is positive
    TYPE(SInt128)                   :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

     IF (Negative) THEN
        I128%High = MaxU64      ! MaxU64 = NOT(0_I8B)
        IF (U64 == 0_I8B) I128%High = I128%High + 1_I8B
        I128%Low = NOT(U64) + 1_I8B
    ELSE
        I128%High = 0_I8B
        I128%Low  = U64
    END IF

    RETURN
            
END FUNCTION U64_To_I128

!******************************************************************************

FUNCTION R32_To_I128(R32) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 32-bit floating point number to a signed 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=SP), INTENT(IN)   :: R32
    TYPE(SInt128)               :: I128

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: Mask = INT(Z'000000FF', KIND=I4B)  ! 255
    INTEGER(KIND=I4B), PARAMETER  :: C1   = SHIFTL(1, 23)               ! 2**23
    INTEGER(KIND=I4B), PARAMETER  :: C2   = C1 - 1                      ! 2**23 - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)       :: Exp
    INTEGER(KIND=I4B)       :: IBits
    REAL(KIND=SP)           :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R32 is NaN or cannot fit into I128.
    IF (.NOT.IEEE_IS_FINITE (R32)) THEN
        CALL DisplaySevereError('Message from Routine '//'R32_To_I128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R32 is NOT finite.')
        RETURN
    ELSEIF (R32 < -2.0_SP**127) THEN
        CALL DisplaySevereError('Message from Routine '//'R32_To_I128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R32 < I128Min.')
        RETURN
    ELSEIF (R32 >= 2.0_SP**127) THEN
        CALL DisplaySevereError('Message from Routine '//'R32_To_I128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R32 > I128Max.')
        RETURN

    END IF
    
    ! get absolute value and transfer bits from real to integer
    RBits = ABS(R32)
    ! determine exponent bits
    Exp = IAND(SHIFTR(IBits, 23), Mask) - 150   ! 150 = 127 + 23
    ! determine significand bits
    IBits = IOR(IAND(IBits, C2), C1)
    ! convert to SInt128
    ! I128 = IBits
    ! add exponent bits
    ! I128 = ISHFT(I128, Exp)
    IF (Exp < 0) THEN
        Exp = -Exp
        IF (Exp >= 64) THEN
            I128 = SInt128(0_I8B, 0_I8B)
        ELSE
            I128 = SInt128(0_I8B, SHIFTR(INT(IBits, KIND=I8B), Exp))
        END IF
    ELSE
        IF (Exp >= 128) THEN
            I128 = SInt128(0_I8B, 0_I8B)
        ELSEIF (Exp >= 64) THEN
            I128 = SInt128(SHIFTL(INT(IBits, KIND=I8B), Exp - 64), 0_I8B)
        ELSE
            I128 = SInt128(SHIFTR(INT(IBits, KIND=I8B), 64 - Exp), SHIFTL(INT(IBits, KIND=I8B), Exp))
        END IF
    END IF
    ! add sign bit
    IF (R32 < 0.0_SP) THEN
        !  I128 = -I128
        I128%High = NOT(I128%High)
        IF (I128%Low == 0_I8B) I128%High = I128%High + 1_I8B
        I128%Low = NOT(I128%Low) + 1_I8B
    END IF
 
    RETURN
            
END FUNCTION R32_To_I128

!******************************************************************************

FUNCTION R64_To_I128(R64) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 64-bit floating point number to a signed 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=DP), INTENT(IN)   :: R64
    TYPE(SInt128)               :: I128

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER  :: Mask = INT(Z'00000000000007FF', KIND=I8B)  ! 2047
    INTEGER(KIND=I8B), PARAMETER  :: C1   = SHIFTL(1_I8B, 52)                   ! 2**52
    INTEGER(KIND=I8B), PARAMETER  :: C2   = C1 - 1_I8B                          ! 2**52 - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)       :: Exp
    INTEGER(KIND=I8B)       :: IBits
    REAL(KIND=DP)           :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R64 is NaN or cannot fit into I128.
    IF (.NOT.IEEE_IS_FINITE (R64)) THEN
        CALL DisplaySevereError('Message from Routine '//'R64_To_I128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R64 is NOT finite.')
        RETURN
    ELSEIF (R64 < -2.0_DP**127) THEN
        CALL DisplaySevereError('Message from Routine '//'R64_To_I128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R64 < I128Min.')
        RETURN
    ELSEIF (R64 >= 2.0_DP**127) THEN
        CALL DisplaySevereError('Message from Routine '//'R64_To_I128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R64 > I128Max.')
        RETURN
    END IF
    
    ! get absolute value and transfer bits from real to integer
    RBits = ABS(R64)
    ! determine exponent bits
    Exp = INT(IAND(SHIFTR(IBits, 52), Mask), KIND=I4B) - 1075   ! 1075 = 1023 + 52
    ! determine significand bits
    IBits = IOR(IAND(IBits, C2), C1)
    ! convert to SInt128
    ! I128 = SInt128(0_I8B, IBits)
    ! add exponent bits
    ! I128 = ISHFT(I128, Exp)
    IF (Exp < 0) THEN
        Exp = -Exp
        IF (Exp >= 64) THEN
            I128 = SInt128(0_I8B, 0_I8B)
        ELSE
            I128 = SInt128(0_I8B, SHIFTR(IBits, Exp))
        END IF
    ELSE
        IF (Exp >= 128) THEN
            I128 = SInt128(0_I8B, 0_I8B)
        ELSEIF (Exp >= 64) THEN
            I128 = SInt128(SHIFTL(IBits, Exp - 64), 0_I8B)
        ELSE
            I128 = SInt128(SHIFTR(IBits, 64 - Exp), SHIFTL(IBits, Exp))
        END IF
    END IF
    ! add sign bit
    IF (R64 < 0.0_DP) THEN
        !  I128 = -I128
        I128%High = NOT(I128%High)
        IF (I128%Low == 0_I8B) I128%High = I128%High + 1_I8B
        I128%Low = NOT(I128%Low) + 1_I8B
    END IF
 
    RETURN

END FUNCTION R64_To_I128

!******************************************************************************

FUNCTION R128_To_I128(R128) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 128-bit floating point number to an unsigned 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=QP), INTENT(IN)   :: R128
    TYPE(SInt128)               :: I128

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: Mask  = INT(Z'00007FFF', KIND=I4B)     ! 32767
    INTEGER(KIND=I8B), PARAMETER  :: C1(2) = [ 0_I8B, 281474976710656_I8B]  ! 2**112 = SHIFTL(1, 112)
    INTEGER(KIND=I8B), PARAMETER  :: C2(2) = [-1_I8B, 281474976710655_I8B]  ! 2**112 - 1 = SHIFTL(1, 112) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: Exp
    INTEGER(KIND=I8B)   :: ExpL
    INTEGER(KIND=I8B)   :: IBits(2)
    REAL(KIND=QP)       :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R128 is NaN or cannot fit into I128.
    IF (.NOT.IEEE_IS_FINITE (R128)) THEN
        CALL DisplaySevereError('Message from Routine '//'R128_To_I128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R128 is NOT finite.')
        RETURN
    ELSEIF (R128 < -2.0_QP**127) THEN
        CALL DisplaySevereError('Message from Routine '//'R128_To_I128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R128 < I128Min.')
        RETURN
    ELSEIF (R128 >= 2.0_QP**127) THEN
        CALL DisplaySevereError('Message from Routine '//'R128_To_I128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R128 > I128Max.')
        RETURN
    END IF

    ! get absolute value and transfer bits from real to integer
    RBits = ABS(R128)
    ! determine exponent bits
    ExpL  = SHIFTR(IBits(2), 48)                    ! 48 = 112-64
    Exp = IAND(INT(ExpL, KIND=I4B), Mask) - 16495   ! 16495 = 16383 + 112
    ! determine significand bits and convert to SInt128
    I128%Low  = IOR(IAND(IBits(1), C2(1)), C1(1))
    I128%High = IOR(IAND(IBits(2), C2(2)), C1(2))
    ! add exponent bits
    ! I128 = ISHFT(I128, Exp)
    IF (Exp < 0) THEN
        I128 = SHIFTR(I128, -Exp)
    ELSE
        I128 = SHIFTL(I128, Exp)
    END IF
    ! add sign bit
    IF (R128 < 0.0_QP) THEN
        !  I128 = -I128
        I128%High = NOT(I128%High)
        IF (I128%Low == 0_I8B) I128%High = I128%High + 1_I8B
        I128%Low = NOT(I128%Low) + 1_I8B
    END IF
 
    RETURN

END FUNCTION R128_To_I128

!******************************************************************************

FUNCTION DecString_To_I128(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to a signed 128-bit integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL,                       OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    TYPE(SInt128)                                           :: Number   ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B),  PARAMETER   :: A0           = IACHAR('0')
    INTEGER(KIND=I4B),  PARAMETER   :: A4           = IACHAR('4')
    INTEGER(KIND=I4B),  PARAMETER   :: A9           = IACHAR('9')
    INTEGER(KIND=I4B),  PARAMETER   :: MaxDigitI32  = 10
    INTEGER(KIND=I4B),  PARAMETER   :: MaxDigitI64  = 19
    INTEGER(KIND=I4B),  PARAMETER   :: MaxDigitI128 = 39
    TYPE(SInt128),      PARAMETER   :: I128Base     = TenI128
    CHARACTER(LEN=*),   PARAMETER   :: MaxStr       = '170141183460469231731687303715884105727'

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)                       :: Indx, StrLen, DigitLen
    INTEGER(KIND=I4B)                       :: NumDigit
    INTEGER(KIND=I4B)                       :: IStart, IndxP7
    CHARACTER(LEN=1), POINTER               :: CurChr
    LOGICAL                                 :: NegSign
    LOGICAL                                 :: Overflow
    CHARACTER(LEN=:), ALLOCATABLE, TARGET   :: CurStr
    LOGICAL                                 :: ErrorFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE           :: ErrorMsg   ! message if input is not invalid
    INTEGER(KIND=I4B)                       :: I32Val
    INTEGER(KIND=I8B)                       :: I64Val
    CHARACTER(LEN=8)                        :: wStr
    INTEGER(KIND=I8B)                       :: wVal
    EQUIVALENCE(wStr, wVal)

!** FLOW

    ! get valid string length by removing the trailing space(s)
    StrLen = LEN_TRIM(cStr)
    IF (PRESENT(ErrFlag)) ErrFlag = FalseVal
    
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
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this is an empty string.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF
    END IF
    
    ! check for sign
    NegSign = FalseVal
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') NegSign = TrueVal
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF
    END IF
    
    ! check for leading zero(s)
    Number = 0_I8B
    IF (cStr(Indx:Indx) == '0') THEN
        ! the first digit is zero so loop through the following
        ! characters until a non-zero character is found
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= '0') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            ! only zero digits encountered
            Number = 0_I8B
            RETURN
        END IF
    END IF

    ! compute the length of digits
    DigitLen = StrLen - Indx + 1

    ! return quickly if possible
    IF (DigitLen < MaxDigitI32) THEN
        I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
        IF (ErrorFlag) THEN
            Number = MinI128
        ELSE
            IF (NegSign) THEN
                Number = -I32Val
            ELSE
                Number = I32Val
            END IF
        END IF
        IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
        IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        RETURN
    ELSEIF (DigitLen < MaxDigitI64) THEN
        I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
        IF (ErrorFlag) THEN
            Number = MinI128
        ELSE
            IF (NegSign) THEN
                Number = -I64Val
            ELSE
                Number = I64Val
            END IF
        END IF
        IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
        IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        RETURN
    END IF

    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        IndxP7 = Indx + 7
        DO WHILE (IndxP7 <= StrLen)
            wStr = cStr(Indx:IndxP7)
            IF (Is8Digits(WVal)) THEN
                ! process 8 digits at once
                ! Number = Number*100000000 + Parse8Digits(wVal)
                CALL Multiply(Number, 100000000)
                CALL Add(Number, Parse8Digits(wVal))
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
            Indx = Indx + 8
            IndxP7 = Indx + 7
        END DO
        IF (Indx <= StrLen) THEN
            CurChr => cStr(Indx:Indx)
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                ! Number = Number*10 + (IACHAR(CurChr)-A0)
                CALL Multiply(Number, 10)
                CALL Add(Number, (IACHAR(CurChr)-A0))
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI128
                    RETURN
                END IF
            END DO
        END IF
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI128
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI128) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI128) THEN
        ! value might be in the applicable range
        IF (IsNegative(Number)) THEN
            ! overflow likely occurs
            Overflow = TrueVal
            IF ((NegSign).AND.(Number == MinI128)) THEN
                ! actually not overflow
                CurStr = '-' // cStr(IStart:StrLen)
                IF (ToDecString(MinI128) == CurStr) THEN
                    Overflow = FalseVal
                    NegSign = FalseVal
                END IF
            END IF
        ELSE
            ! positive value so check overflow
            CurStr = cStr(IStart:StrLen)
            Overflow = FalseVal
            DO Indx = 1, MaxDigitI128
                CurChr => CurStr(Indx:Indx)
                IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                    EXIT
                ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                    Overflow = TrueVal
                    EXIT
                END IF
            END DO
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (NegSign) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI128
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI128
        END IF
    ELSE
        IF (NegSign) Number = -Number
    END IF

    RETURN
    CONTAINS

    FUNCTION Parse8Digits(InVal) RESULT(OutVal)

!DIR$ ATTRIBUTES FORCEINLINE :: Parse8Digits

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

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
        
        OutVal = SHIFTR(IAND(SHIFTR(IAND(SHIFTR(IAND(InVal, K1)*M1, 8), K2)*M2, 16), K3)*M3, 32)
    
        RETURN

    END FUNCTION Parse8Digits

!******************************************************************************

    FUNCTION Is8Digits(InVal) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: Is8Digits

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

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
        
        Flag = IOR(IAND(InVal, C1), SHIFTR(IAND((InVal + C3), C1), 4)) ==  C2

        RETURN

    END FUNCTION Is8Digits

!******************************************************************************

END FUNCTION DecString_To_I128

!------------------------------------------------------------------------------
!
!                           CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION U32_From_I128(I128) RESULT(U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 128-bit integer number to an unsigned 32-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    INTEGER(KIND=I4B)            :: U32      ! number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U32 = INT(I128%Low, KIND=I4B)
 
    RETURN
            
END FUNCTION U32_From_I128

!******************************************************************************

FUNCTION U64_From_I128(I128) RESULT(U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 128-bit integer number to an unsigned 64-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    INTEGER(KIND=I8B)           :: U64      ! number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U64 = I128%Low
 
    RETURN
            
END FUNCTION U64_From_I128

!******************************************************************************

FUNCTION R32_From_I128(I128) RESULT(R32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 128-bit integer number to a 32-bit floating point number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    REAL(KIND=SP)               :: R32
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    REAL(KIND=SP),     PARAMETER  :: TwoPow64 = 2.0_SP**64
    INTEGER(KIND=I4B), PARAMETER  :: TwoPow23 = SHIFTL(1, 23)
    INTEGER(KIND=I4B), PARAMETER  :: Mask     = INT(Z'000000FF', KIND=I4B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    LOGICAL             :: Negative
    INTEGER(KIND=I8B)   :: High
    INTEGER(KIND=I8B)   :: Low
    INTEGER(KIND=I4B)   :: S, Exp
    INTEGER(KIND=I4B)   :: IBits
    REAL(KIND=SP)       :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW
    
    ! get sign flag and absolute values of components
    Negative = (I128%High < 0_I8B)
    IF (Negative) THEN
        High = NOT(I128%High)
        IF (I128%Low == 0_I8B) High = High + 1_I8B
        Low  = NOT(I128%Low) + 1_I8B
    ELSE
        High = I128%High
        Low  = I128%Low
    END IF
    
    IF (High == 0_I8B) THEN
        R32 = U64_To_R32(Low)
        IF (IsNegative(I128)) R32 = -R32
        RETURN
    END IF

    S = LEADZ(High)
    ! Mask out the 24 MSBits
    ! Also, the leading bit is implicit so cancel it out to get the significand
    IF (S <= 40) THEN
        IBits = IEOR(INT(SHIFTR(High, 40-S), KIND=I4B), TwoPow23)
    ELSE
        ! S-40 == additional bits we need
        IBits = IEOR(INT(IOR(SHIFTL(High, S-40), SHIFTR(Low, 104-S)), KIND=I4B), TwoPow23)
    END IF
    ! get the binary exponent
    Exp = IAND(254-S, Mask)         ! 254 = 64 + 64 + 127 - 1

    ! Add the exponent
    IBits = IOR(IBits, SHIFTL(Exp, 23))
    
    ! transfer output (RBits mapped to IBits using equivalence)
	R32 = RBits
    
    ! check and add sign if needed
    IF (Negative) R32 = -R32
    
    RETURN

CONTAINS

    FUNCTION U64_To_R32(LongVal) RESULT(SingleVal)

!DIR$ ATTRIBUTES FORCEINLINE :: U64_To_R32

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 32-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: LongVal      ! integer number treated as unsigned one
        REAL(KIND=SP)                   :: SingleVal    ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_I8B) THEN
            SingleVal = REAL(LongVal, KIND=SP)
        ELSE
            SingleVal = TwoPow64 + REAL(LongVal, KIND=SP)
        END IF

        RETURN

    END FUNCTION U64_To_R32

!******************************************************************************

END FUNCTION R32_From_I128

!******************************************************************************

FUNCTION R64_From_I128(I128) RESULT(R64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 128-bit integer number to a 64-bit floating point number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    REAL(KIND=DP)               :: R64
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    REAL(KIND=DP),     PARAMETER  :: TwoPow64 = 2.0_DP**64
    INTEGER(KIND=I8B), PARAMETER  :: TwoPow52 = SHIFTL(1_I8B, 52)
    INTEGER(KIND=I4B), PARAMETER  :: Mask     = INT(Z'000007FF', KIND=I4B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    LOGICAL             :: Negative
    INTEGER(KIND=I8B)   :: High
    INTEGER(KIND=I8B)   :: Low
    INTEGER(KIND=I4B)   :: S
    INTEGER(KIND=I8B)   :: IBits, Exp
    REAL(KIND=DP)       :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    ! get sign flag and absolute values of components
    Negative = (I128%High < 0_I8B)
    IF (Negative) THEN
        High = NOT(I128%High)
        IF (I128%Low == 0_I8B) High = High + 1_I8B
        Low  = NOT(I128%Low) + 1_I8B
    ELSE
        High = I128%High
        Low  = I128%Low
    END IF
    
    IF (High == 0_I8B) THEN
        R64 = U64_To_R64(Low)
        IF (Negative) R64 = -R64
        RETURN
    END IF

    S = LEADZ(High)
    ! Mask out the 53 MSBits
    ! Also, the leading bit is implicit so cancel it out to get the significand
    IF (S <= 11) THEN
        IBits = IEOR(SHIFTR(High, 11-S), TwoPow52)
    ELSE
        ! S-11 == additional bits we need
        IBits = IEOR(IOR(SHIFTL(High, S-11), SHIFTR(Low, 75-S)), TwoPow52)
    END IF
    ! get the binary exponent
    Exp = INT(IAND(1150-S, Mask), KIND=I8B)        ! 1150 = 64 + 64 + 1023 - 1

    ! Add the exponent
    IBits = IOR(IBits, SHIFTL(Exp, 52))
    
    ! transfer output (RBits mapped to IBits using equivalence)
	R64 = RBits
    
    ! check and add sign if needed
    IF (Negative) R64 = -R64
     
    RETURN

CONTAINS

    FUNCTION U64_To_R64(LongVal) RESULT(DoubleVal)

!DIR$ ATTRIBUTES FORCEINLINE :: U64_To_R64

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 64-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: LongVal      ! integer number treated as unsigned one
        REAL(KIND=DP)                   :: DoubleVal    ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_I8B) THEN
            DoubleVal = REAL(LongVal, KIND=DP)
        ELSE
            DoubleVal = TwoPow64 + REAL(LongVal, KIND=DP)
        END IF

        RETURN

    END FUNCTION U64_To_R64

!******************************************************************************

END FUNCTION R64_From_I128

!******************************************************************************

FUNCTION R128_From_I128(I128) RESULT(R128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 128-bit integer number to a 128-bit floating point number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    REAL(KIND=QP)               :: R128
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    REAL(KIND=QP),     PARAMETER  :: TwoPow64     = 2.0_QP**64
    INTEGER(KIND=I8B), PARAMETER  :: TwoPow112(2) = [ 0_I8B, 281474976710656_I8B] ! SHIFTL(1, 112)
    INTEGER(KIND=I4B), PARAMETER  :: Mask         = INT(Z'00007FFF', KIND=I4B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    LOGICAL             :: Negative
    INTEGER(KIND=I8B)   :: High
    INTEGER(KIND=I8B)   :: Low
    INTEGER(KIND=I4B)   :: S, Shift
    INTEGER(KIND=I8B)   :: Exp
    INTEGER(KIND=I8B)   :: IBits(2)
    REAL(KIND=QP)       :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    ! get sign flag and absolute values of components
    Negative = (I128%High < 0_I8B)
    IF (Negative) THEN
        High = NOT(I128%High)
        IF (I128%Low == 0_I8B) High = High + 1_I8B
        Low  = NOT(I128%Low) + 1_I8B
    ELSE
        High = I128%High
        Low  = I128%Low
    END IF
    
    IF (High == 0_I8B) THEN
        R128 = U64_To_R128(Low)
        IF (Negative) R128 = -R128
        RETURN
    END IF

    S = LEADZ(High)
    IF (S >= 15) THEN
        R128 = U64_To_R128(Low) + REAL(High, KIND=QP)*TwoPow64
        IF (Negative) R128 = -R128
        RETURN
    END IF
    
    ! Mask out the 113 MSBits
    Shift = 15 - S
    IBits(2) = SHIFTR(High, Shift)
    IBits(1) = IOR(SHIFTR(Low, Shift), SHIFTL(High, 64-Shift))

    ! get the binary exponent
    Exp = INT(IAND(16510-S, Mask), KIND=I8B)   ! 16510 = 64 + 64 + 16383 - 1

    ! The leading bit is implicit, cancel it out to get the significand
    ! and also add the exponent
    IBits(1) = IEOR(IOR(SHIFTR(Low, Shift), SHIFTL(High, 64-Shift)), TwoPow112(1))
    IBits(2) = IOR(IEOR(SHIFTR(High, Shift), TwoPow112(2)), SHIFTL(Exp, 48))    ! 48 = 112 - 64
    
    ! transfer output (RBits mapped to IBits using equivalence)
    ! For big-endian machine, this one is likely wrong so we must
    ! swap IBits(1) and IBits(2) before the assigment.
    !   Tmp = IBits(1); IBits(1) = IBits(2); IBits(2) = Tmp
	R128 = RBits
    
    ! check and add sign if needed
    IF (Negative) R128 = -R128
     
    RETURN

CONTAINS

    FUNCTION U64_To_R128(LongVal) RESULT(QuadVal)

!DIR$ ATTRIBUTES FORCEINLINE :: U64_To_R128

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 128-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: LongVal  ! integer number treated as unsigned one
        REAL(KIND=QP)                   :: QuadVal  ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_I8B) THEN
            QuadVal = REAL(LongVal, KIND=QP)
        ELSE
            QuadVal = TwoPow64 + REAL(LongVal, KIND=QP)
        END IF

        RETURN

    END FUNCTION U64_To_R128

!******************************************************************************

END FUNCTION R128_From_I128

!******************************************************************************

FUNCTION U128_From_I128(I128) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a signed 128-bit integer to an unsigned 128-bit integer

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(UInt128)               :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    U128 = UInt128(I128%High, I128%Low)

    RETURN

END FUNCTION U128_From_I128

!******************************************************************************

FUNCTION DecString_From_I128(I128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a decimal string

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)       :: I128
    CHARACTER(LEN=:), ALLOCATABLE   :: Str
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    CHARACTER(LEN=1), PARAMETER     :: NumStr(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)       :: BufLen, Top, I, J
    CHARACTER(LEN=41)       :: Buffer
    TYPE(SInt128)           :: Copy
    INTEGER(KIND=I8B)       :: Tmp
    INTEGER(KIND=I8B)       :: Indx
    LOGICAL                 :: Negative

!** FLOW
    
	IF (I128 == ZeroI128) THEN
        Str = '0'
        RETURN
    END IF

    BufLen = 41
    FORALL (I=1:BufLen) Buffer(I:I) = '0'
	Top  = BufLen
    Negative = IsNegative(I128)
    IF (Negative) THEN
        IF (I128 == MinI128) THEN
            Str  = '-170141183460469231731687303715884105728'
            RETURN
        ELSE        
            Copy = -I128
        END IF
    ELSE
        Copy = I128
    END IF
	DO
        J = Top
        Tmp = ToStringDivide(Copy)
        DO WHILE (Tmp > 0_I8B)
            Indx = MOD(Tmp, 10_I8B)
            Buffer(Top:Top) = NumStr(Indx)
            Top = Top - 1
            Tmp = Tmp / 10_I8B
        END DO
        IF ((Copy%High == 0).AND.(Copy%Low == 0)) THEN
            EXIT
        ELSE
            Top = J - 13
        END IF
    END DO
    IF (Negative) THEN
        Buffer(Top:Top) = '-'
        Str = Buffer(Top:BufLen)
    ELSE
        Str = Buffer(Top+1:BufLen)
    END IF

    RETURN
    
    CONTAINS

    FUNCTION ToStringDivide(I128) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the number by 10**13 and return the remainder

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(SInt128), INTENT(INOUT)    :: I128
        INTEGER(KIND=I8B)               :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I4B),  PARAMETER   :: Pow2  = SHIFTL(1, 13)
        INTEGER(KIND=I4B),  PARAMETER   :: Pow5  = 1220703125
        INTEGER(KIND=I8B),  PARAMETER   :: Pow10 = INT(Pow2, KIND=I8B)*INT(Pow5, KIND=I8B)
        LOGICAL,            PARAMETER   :: Positive = FalseVal
        LOGICAL,            PARAMETER   :: AsUnsigned = TrueVal
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(SInt128)       :: Rem, Quot, Numer, Pow10_128
        INTEGER(KIND=I8B)   :: Q, R, Mod2

    !** FLOW
        
        Q = I128%High / Pow5
        R = I128%High - Q*Pow5
		I128%High = SHIFTR(Q, 13)
        
        Numer%High = R
        Numer%Low  = I128%Low
	    Mod2 = IAND(I128%Low, Pow2 - 1_I8B)
        
        CALL DivMod(Numer, SInt128(Pow5), Quot, Rem)
        I128%Low = IOR(SHIFTL(Q, 51), SHIFTR(Quot%Low, 13))
        
        ! Applies the Chinese Rem Theorem.
        ! -67*5^13 + 9983778*2^13 = 1
        Pow10_128 = SInt128(0_I8B, Pow10)
        Rem = SMOD((Rem - SMOD(Pow5*(Mod2 - Rem), Pow10_128)*67), Pow10_128)
	    IF (Rem%High < 0_I8B) Rem = Rem + Pow10
        Remainder = Rem%Low
    
        RETURN
            
    END FUNCTION ToStringDivide

    !**************************************************************************

    FUNCTION SMOD(Dividend, Divisor) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division of two UInt128 objects (Dividend / Divisor)
        ! and return the remainder
        ! note: although the input and output objects are of UInt128 type,
        !       they are all treated as if they are signed 128-bit integers

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(SInt128), INTENT(IN)    :: Dividend
        TYPE(SInt128), INTENT(IN)    :: Divisor
        TYPE(SInt128)                :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(SInt128)    :: Quotient

    !** FLOW

        CALL DivMod(Dividend, Divisor, Quotient, Remainder)
    
        RETURN

    END FUNCTION SMOD

    !**************************************************************************

END FUNCTION DecString_From_I128

!******************************************************************************

FUNCTION HexString_From_I128(I128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a hexadecimal string

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: I128
    CHARACTER(LEN=:), ALLOCATABLE              :: Str
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: U128

!** FLOW
    
    IF (I128 == MinI128) THEN
        Str  = '-80000000000000000000000000000000'
    ELSEIF (IsNegative(I128)) THEN
        U128 = ToU128(-I128)
        Str  = '-' // ToHexString(U128)
    ELSE
        U128 = ToU128(I128)
        Str  = ToHexString(U128)
    END IF
    
    RETURN

END FUNCTION HexString_From_I128

!------------------------------------------------------------------------------
!
!                           COMPARISION ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION I128_Equal(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether two SInt128 objects are equal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: LHS
    TYPE(SInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (LHS%High == RHS%High).AND.(LHS%Low == RHS%Low)

    RETURN

END FUNCTION I128_Equal

!******************************************************************************

FUNCTION I128_NotEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether two SInt128 objects are NOT equal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: LHS
    TYPE(SInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (LHS%High /= RHS%High).OR.(LHS%Low /= RHS%Low)

    RETURN

END FUNCTION I128_NotEqual

!******************************************************************************

FUNCTION I128_LessThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the LHS SInt128 object is less than the RHS SInt128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: LHS
    TYPE(SInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) < IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (LHS%High < RHS%High)
    END IF

    RETURN

END FUNCTION I128_LessThan

!******************************************************************************

FUNCTION I128_LessEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the LHS SInt128 object is less than or equal to the RHS SInt128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: LHS
    TYPE(SInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) <= IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (LHS%High <= RHS%High)
    END IF

    RETURN

END FUNCTION I128_LessEqual

!******************************************************************************

FUNCTION I128_GreaterThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the LHS SInt128 object is greater than the RHS SInt128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: LHS
    TYPE(SInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) > IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (LHS%High > RHS%High)
    END IF

    RETURN

END FUNCTION I128_GreaterThan

!******************************************************************************

FUNCTION I128_GreaterEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the LHS SInt128 object is greater than or equal to the RHS SInt128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: LHS
    TYPE(SInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) >= IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (LHS%High >= RHS%High)
    END IF

    RETURN

END FUNCTION I128_GreaterEqual

!******************************************************************************

FUNCTION I128_Compare(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compare LHS and RHS objects.
    ! - return -1 if LHS < RHS
    ! - return  0 if LHS == RHS
    ! - return +1 if LHS > RHS

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: LHS
    TYPE(SInt128), INTENT(IN)   :: RHS
    INTEGER(KIND=I4B)           :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: ULHS, URHS
        
!** FLOW
    
    IF (LHS%High < RHS%High) THEN
        Flag = -1
    ELSEIF (LHS%High > RHS%High) THEN
        Flag = +1
    ELSE
        ULHS = IEOR(LHS%Low, MinI64)
        URHS = IEOR(RHS%Low, MinI64)
        IF (ULHS < URHS) THEN
            Flag = -1
        ELSEIF (ULHS > URHS) THEN
            Flag = +1
        ELSE
            Flag = 0
        END IF
    END IF
    
    RETURN

END FUNCTION I128_Compare

!------------------------------------------------------------------------------
!
!                           BITWISE ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION I128_SHIFTL_Once(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical left shift by 1

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: InVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IOR(SHIFTL(InVal%High, 1), SHIFTR(InVal%Low, 63))
    OutVal%Low  = SHIFTL(InVal%Low, 1)

    RETURN

END FUNCTION I128_SHIFTL_Once

!******************************************************************************

FUNCTION I128_SHIFTR_Once(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical right shift by 1

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: InVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTR(InVal%High, 1)
    OutVal%Low  = IOR(SHIFTR(InVal%Low, 1), SHIFTL(SHIFTL(InVal%High, 1), 62))

    RETURN

END FUNCTION I128_SHIFTR_Once

!******************************************************************************

FUNCTION I128_SHIFTA_Once(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical right shift by 1

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: InVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTA(InVal%High, 1)
    OutVal%Low  = IOR(SHIFTR(InVal%Low, 1), SHIFTL(SHIFTL(InVal%High, 1), 62))

    RETURN

END FUNCTION I128_SHIFTA_Once

!******************************************************************************

FUNCTION I128_SHIFTL_64(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical left shift by 64

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: InVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = InVal%Low
    OutVal%Low  = 0_I8B

    RETURN

END FUNCTION I128_SHIFTL_64

!******************************************************************************

FUNCTION I128_SHIFTR_64(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical right shift by 64

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: InVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = 0_I8B
    OutVal%Low  = InVal%High

    RETURN

END FUNCTION I128_SHIFTR_64

!******************************************************************************

FUNCTION I128_SHIFTA_64(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical right shift by 64

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: InVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTA(InVal%High, 63)
    OutVal%Low  = InVal%High

    RETURN

END FUNCTION I128_SHIFTA_64

!******************************************************************************

FUNCTION I128_SHIFTL_63Down(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical left shift by 63 or less

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IOR(SHIFTL(InVal%High, ShiftPos), SHIFTR(InVal%Low, 64 - ShiftPos))
    OutVal%Low  = SHIFTL(InVal%Low, ShiftPos)

    RETURN

END FUNCTION I128_SHIFTL_63Down

!******************************************************************************

FUNCTION I128_SHIFTR_63Down(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical right shift by 63 or less

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTR(InVal%High, ShiftPos)
    OutVal%Low  = IOR(SHIFTR(InVal%Low, ShiftPos), &
                      SHIFTL(SHIFTL(InVal%High, 1), 63 - ShiftPos))

    RETURN

END FUNCTION I128_SHIFTR_63Down

!******************************************************************************

FUNCTION I128_SHIFTA_63Down(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical right shift by 63 or less

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTA(InVal%High, ShiftPos)
    OutVal%Low  = IOR(SHIFTR(InVal%Low, ShiftPos), &
                      SHIFTL(SHIFTL(InVal%High, 1), 63 - ShiftPos))

    RETURN

END FUNCTION I128_SHIFTA_63Down

!******************************************************************************

FUNCTION I128_SHIFTL_64Up(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical left shift by 64 or more

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTL(InVal%Low, ShiftPos - 64)
    OutVal%Low  = 0_I8B

    RETURN

END FUNCTION I128_SHIFTL_64Up

!******************************************************************************

FUNCTION I128_SHIFTR_64Up(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical right shift by 64 or more

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = 0_I8B
    OutVal%Low  = SHIFTR(InVal%High, ShiftPos - 64)

    RETURN

END FUNCTION I128_SHIFTR_64Up

!******************************************************************************

FUNCTION I128_SHIFTA_64Up(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical right shift by 64 or more

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTA(InVal%High, 63)
    OutVal%Low  = SHIFTA(InVal%High, ShiftPos - 64)

    RETURN

END FUNCTION I128_SHIFTA_64Up

!******************************************************************************

FUNCTION I128_ShiftLogical(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical (left or rigth) shift of the SInt128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos     ! must be between -128 and 128
                                                    ! positive, the shift is to the left
                                                    ! negative, the shift is to the right
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ShiftPos < 0) THEN
        OutVal = SHIFTR(InVal, -ShiftPos)
    ELSE
        OutVal = SHIFTL(InVal, ShiftPos)
    END IF

    RETURN

END FUNCTION I128_ShiftLogical

!******************************************************************************

FUNCTION I128_ShiftLeft(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical/arithmetic left shift of the SInt128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos ! must be nonnegative and <= 128
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    IF (ShiftPos < 0) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_ShiftLeft'//' in Module '//ModName//'.')
        CALL DisplayContinueError('ShiftPos must be nonnegative number');
    ELSEIF (ShiftPos == 0) THEN
        OutVal = InVal
    ELSEIF (ShiftPos >= 128) THEN
        OutVal = ZeroI128
    ELSEIF (ShiftPos >= 64) THEN
        OutVal%High = SHIFTL(InVal%Low, ShiftPos - 64)
        OutVal%Low  = 0_I8B
    ELSE
        OutVal%High = IOR(SHIFTL(InVal%High, ShiftPos), SHIFTR(InVal%Low, 64 - ShiftPos))
        OutVal%Low  = SHIFTL(InVal%Low, ShiftPos)
    END IF

    RETURN

END FUNCTION I128_ShiftLeft

!******************************************************************************

FUNCTION I128_ShiftRightArithmetic(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform arithmetic right shift of the SInt128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos ! must be nonnegative and <= 128
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ShiftPos < 0) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_ShiftRightArithmetic'//' in Module '//ModName//'.')
        CALL DisplayContinueError('ShiftPos must be nonnegative number')
    ELSEIF (ShiftPos == 0) THEN
        OutVal = InVal
    ELSEIF (ShiftPos >= 128) THEN
        IF (IsNegative(InVal)) THEN
            OutVal = -1
        ELSE
            OutVal = ZeroI128
        END IF
    ELSEIF (ShiftPos >= 64) THEN
        OutVal%High = SHIFTA(InVal%High, 63)
        OutVal%Low  = SHIFTA(InVal%High, ShiftPos - 64)
    ELSE
        OutVal%High = SHIFTA(InVal%High, ShiftPos)
        OutVal%Low  = IOR(SHIFTR(InVal%Low, ShiftPos), &
                          SHIFTL(SHIFTL(InVal%High, 1), 63 - ShiftPos))
    END IF

    RETURN

END FUNCTION I128_ShiftRightArithmetic

!******************************************************************************

FUNCTION I128_ShiftRightLogical(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform logical right shift of the SInt128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos ! must be nonnegative and <= 128
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW
    
    IF (ShiftPos < 0) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_ShiftRightLogical'//' in Module '//ModName//'.')
        CALL DisplayContinueError('ShiftPos must be nonnegative number')
    ELSEIF (ShiftPos == 0) THEN
        OutVal = InVal
    ELSEIF (ShiftPos >= 128) THEN
        OutVal = ZeroI128
    ELSEIF (ShiftPos >= 64) THEN
        OutVal%High = 0_I8B
        OutVal%Low  = SHIFTR(InVal%High, ShiftPos - 64)
    ELSE
        OutVal%High = SHIFTR(InVal%High, ShiftPos)
        OutVal%Low  = IOR(SHIFTR(InVal%Low, ShiftPos), &
                          SHIFTL(SHIFTL(InVal%High, 1), 63 - ShiftPos))
    END IF

    RETURN

END FUNCTION I128_ShiftRightLogical

!******************************************************************************

FUNCTION I128_Rotate(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform a circular shift of the rightmost bits

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos ! must be between -128 and 128
                                                ! positive, the shift is to the left
                                                ! negative, the shift is to the right
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: LeftShift

!** FLOW

    IF (ShiftPos == 0) THEN
        OutVal = InVal
        RETURN
    ELSEIF (ABS(ShiftPos) == 128) THEN
        OutVal = ZeroI128
        RETURN
    ELSEIF (ABS(ShiftPos) > 128) THEN
        LeftShift = MOD(ShiftPos, 128)
    ELSE
        LeftShift = ShiftPos
    END IF
    IF (LeftShift < 0) LeftShift = 128 + LeftShift
    OutVal = IOR(SHIFTL(InVal, LeftShift), SHIFTR(InVal, 128 - LeftShift))

    RETURN

END FUNCTION I128_Rotate

!******************************************************************************

FUNCTION I128_Not(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the bitwise logical complement of the SInt128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: InVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    OutVal%High = NOT(InVal%High)
    OutVal%Low  = NOT(InVal%Low)

    RETURN

END FUNCTION I128_Not

!******************************************************************************

FUNCTION I128_Ior(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform an inclusive OR on corresponding bits of the SInt128 objects

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: LhsVal
    TYPE(SInt128), INTENT(IN)    :: RhsVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    OutVal%High = IOR(LhsVal%High, RhsVal%High)
    OutVal%Low  = IOR(LhsVal%Low, RhsVal%Low)

    RETURN

END FUNCTION I128_Ior

!******************************************************************************

FUNCTION I128_Iand(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform a logical AND on corresponding bits of the SInt128 objects

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: LhsVal
    TYPE(SInt128), INTENT(IN)    :: RhsVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    OutVal%High = IAND(LhsVal%High, RhsVal%High)
    OutVal%Low  = IAND(LhsVal%Low, RhsVal%Low)

    RETURN

END FUNCTION I128_Iand

!******************************************************************************

FUNCTION I128_Ieor(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform an exclusive OR on corresponding bits of the SInt128 objects

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: LhsVal
    TYPE(SInt128), INTENT(IN)    :: RhsVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    OutVal%High = IEOR(LhsVal%High, RhsVal%High)
    OutVal%Low  = IEOR(LhsVal%Low, RhsVal%Low)

    RETURN

END FUNCTION I128_Ieor

!******************************************************************************

FUNCTION I128_LeadingZeros(I128) RESULT(NumLZ)

!** PURPOSE OF THIS SUBROUTINE:
    ! To count the number of leading zero bits

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    INTEGER(KIND=I4B)           :: NumLZ

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (I128%High == 0_I8B) THEN
        NumLZ = LEADZ(I128%Low) + 64
    ELSE
        NumLZ = LEADZ(I128%High)
    END IF

    RETURN

END FUNCTION I128_LeadingZeros

!******************************************************************************

FUNCTION I128_TrailingZeros(I128) RESULT(NumTZ)

!** PURPOSE OF THIS SUBROUTINE:
    ! To count the number of trailing zero bits

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    INTEGER(KIND=I4B)           :: NumTZ

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (I128%Low == 0_I8B) THEN
        NumTZ = TRAILZ(I128%High) + 64
    ELSE
        NumTZ = TRAILZ(I128%Low)
    END IF

    RETURN

END FUNCTION I128_TrailingZeros

!******************************************************************************

FUNCTION I128_Count1Bits(I128) RESULT(NumBits)

!** PURPOSE OF THIS SUBROUTINE:
    ! To count the number of 1 bits in the specified input

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    INTEGER(KIND=I4B)           :: NumBits

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    NumBits = POPCNT(I128%Low) + POPCNT(I128%High)
    
    RETURN

END FUNCTION I128_Count1Bits

!******************************************************************************

FUNCTION I128_Parity(I128) RESULT(ParNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine the parity of the specified input

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    INTEGER(KIND=I4B)           :: ParNum

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! ParNum = IAND(POPCNT(I128), 1)
    ParNum = POPPAR(I128%Low) + POPPAR(I128%High)
    IF (ParNum == 2) ParNum = 0
    
    RETURN

END FUNCTION I128_Parity

!******************************************************************************

FUNCTION I128_SetBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To set the bit at the specified position to 1
    ! For more detail, see explanation of elemental intrinsic function 'IBSET'

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: Pos
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Pos < 0).OR.(Pos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_SetBit'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos must be between 0 and 127.')
        RETURN
    END IF
    
    IF (Pos < 64) THEN
        OutVal%Low  = IBSET(InVal%Low, Pos)
        OutVal%High = InVal%High
    ELSE
        OutVal%Low  = InVal%Low
        OutVal%High = IBSET(InVal%High, Pos-64)
    END IF
    
    RETURN

END FUNCTION I128_SetBit

!******************************************************************************

FUNCTION I128_ClearBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To set the bit at the specified position to 0
    ! For more detail, see explanation of elemental intrinsic function 'IBCLR'

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: Pos
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Pos < 0).OR.(Pos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_ClearBit'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos must be between 0 and 127.')
        RETURN
    END IF
    
    IF (Pos < 64) THEN
        OutVal%Low  = IBCLR(InVal%Low, Pos)
        OutVal%High = InVal%High
    ELSE
        OutVal%Low  = InVal%Low
        OutVal%High = IBCLR(InVal%High, Pos-64)
    END IF
    
    RETURN

END FUNCTION I128_ClearBit

!******************************************************************************

FUNCTION I128_FlipBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To reverse the bit at the specified position

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: Pos
    TYPE(SInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: HiPos

!** FLOW
    
    IF ((Pos < 0).OR.(Pos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_FlipBit'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos must be between 0 and 127.')
        RETURN
    END IF
    
    IF (Pos < 64) THEN
        IF (BTEST(InVal%Low, Pos)) THEN
            ! clear bit
            OutVal%Low = IBCLR(InVal%Low, Pos)
        ELSE
            ! set bit
            OutVal%Low = IBSET(InVal%Low, Pos)
        END IF
        OutVal%High = InVal%High
    ELSE
        HiPos = Pos-64
        IF (BTEST(InVal%High, HiPos)) THEN
            ! clear bit
            OutVal%High = IBCLR(InVal%High, HiPos)
        ELSE
            ! set bit
            OutVal%High = IBSET(InVal%High, HiPos)
        END IF
        OutVal%Low = InVal%Low
    END IF
    
    RETURN

END FUNCTION I128_FlipBit

!******************************************************************************

FUNCTION I128_TestBit(I128, Pos) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the bit at the specifed position is 0 (False) or 1 (True)
    ! For more detail, see explanation of elemental intrinsic function 'BTEST'

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)  :: I128
    INTEGER(KIND=I4B),  INTENT(IN)  :: Pos
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF ((Pos < 0).OR.(Pos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_TestBit'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos must be between 0 and 127.')
        RETURN
    END IF
    
    IF (Pos < 64) THEN
        Flag = BTEST(I128%Low, Pos)
    ELSE
        Flag = BTEST(I128%High, Pos-64)
    END IF
    
    RETURN

END FUNCTION I128_TestBit

!******************************************************************************

FUNCTION I128_ExtractBits(InVal, Pos, Len) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To extract a sequence of bits according to the specified input
    ! For more detail, see explanation of elemental intrinsic function 'IBITS'

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: Pos
    INTEGER(KIND=I4B),  INTENT(IN)    :: Len
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Len1, Len2, Len3

!** FLOW

    ! first, check input validity
    IF (Len < 0) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_ExtractBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Len must be nonnegative.')
        RETURN
    ELSEIF (Len == 0) THEN
        OutVal = ZeroI128
        RETURN
    ELSEIF ((Pos < 0).OR.(Pos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_ExtractBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos must be between 0 and 127.')
        RETURN
    ELSEIF (Pos + Len > 128) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_ExtractBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos + Len > 128.')
        RETURN
    END IF

    OutVal = ZeroI128
    IF (Pos < 64) THEN
        IF (Pos + Len <= 64) THEN
            ! bit fields are in only lower elements of both input and output
            CALL MVBITS(InVal%Low, Pos, Len, OutVal%Low, 0)
        ELSE
            IF (Len <= 64) THEN
                ! bit fields are in both lower and upper elements of input
                ! but only in lower element of output
                Len1 = 64-Pos
                Len2 = Len - Len1
                CALL MVBITS(InVal%Low,  Pos, Len1, OutVal%Low,    0)
                CALL MVBITS(InVal%High,   0, Len2, OutVal%Low, Len1)
            ELSE
                ! bit fields are in lower and upper elements of both input and output
                Len1 = 64-Pos           ! Input%Low  -> Output%Low
                Len2 = 64-Len1          ! Input%High -> Output%Low
                Len3 = Len-(Len1+Len2)  ! Input%High -> Output%High
                CALL MVBITS(InVal%Low,   Pos, Len1, OutVal%Low,     0)
                CALL MVBITS(InVal%High,    0, Len2, OutVal%Low,  Len1)
                CALL MVBITS(InVal%High, Len2, Len3, OutVal%High,    0)
            END IF
        END IF
    ELSE
        ! one of the simplest cases where bit fields are in upper element of input
        ! and in lower element of output
        CALL MVBITS(InVal%High, Pos-64, Len, OutVal%Low, 0)
    END IF

    RETURN

END FUNCTION I128_ExtractBits

!******************************************************************************

SUBROUTINE I128_MoveBits(InVal, InPos, Len, OutVal, OutPos)

!** PURPOSE OF THIS SUBROUTINE:
    ! To copy a sequence of bits (a bit field) from one location to another
    ! For more detail, see explanation of elemental intrinsic subroutine
    ! 'MVBITS'

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: InPos
    INTEGER(KIND=I4B),  INTENT(IN)    :: Len
    TYPE(SInt128),      INTENT(INOUT) :: OutVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: OutPos

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Len1, Len2, Len3

!** FLOW

    ! first, check input validity
    IF (Len < 0) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_MoveBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Len must be nonnegative.')
        RETURN
    ELSEIF (Len == 0) THEN
        RETURN
    ELSEIF ((InPos < 0).OR.(InPos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_MoveBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('InPos must be between 0 and 127.')
        RETURN
    ELSEIF ((OutPos < 0).OR.(OutPos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_MoveBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('OutPos must be between 0 and 127.')
        RETURN
    ELSEIF (InPos + Len > 128) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_MoveBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('InPos + Len > 128.')
        RETURN
    ELSEIF (OutPos + Len > 128) THEN
        CALL DisplaySevereError('Message from Routine '//'I128_MoveBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('OutPos + Len > 128.')
        RETURN
    END IF

    IF (InPos < 64) THEN
        IF (InPos + Len <= 64) THEN
            IF (OutPos < 64) THEN
                IF (OutPos + Len <= 64) THEN
                    ! one of the simplest cases where bit fields are in lower elements
                    CALL MVBITS(InVal%Low, InPos, Len, OutVal%Low, OutPos)
                ELSE
                    ! bit fields are in lower element of input but in both lower and
                    ! upper elements of output
                    Len1 = 64-OutPos
                    Len2 = Len - Len1
                    CALL MVBITS(InVal%Low, InPos,      Len1, OutVal%Low,  OutPos)
                    CALL MVBITS(InVal%Low, InPos+Len1, Len2, OutVal%High,      0)
                END IF
            ELSE
                ! one of the simplest cases where bit fields are in lower element of input
                ! and upper element of output, respectively
                CALL MVBITS(InVal%Low, InPos, Len, OutVal%High, OutPos-64)
            END IF
        ELSE
            IF (OutPos < 64) THEN
                IF (OutPos + Len <= 64) THEN
                    ! bit fields are in both lower and upper element of input but
                    ! only in lower element of output
                    Len1 = 64-InPos
                    Len2 = Len - Len1
                    CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%Low,      OutPos)
                    CALL MVBITS(InVal%High,     0, Len2, OutVal%Low, OutPos+Len1)
                ELSE
                    ! the most complicated cases where bit fields are in lower
                    ! and upper elements of both input and output
                    IF (InPos == OutPos) THEN
                        Len1 = 64-InPos         ! Input%Low  -> Output%Low
                        Len2 = Len-Len1         ! Input%High -> Output%High
                        CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%Low,  OutPos)
                        CALL MVBITS(InVal%High,     0, Len2, OutVal%High,      0)
                    ELSEIF (InPos < OutPos) THEN
                        Len1 = 64-OutPos        ! Input%Low  -> Output%Low
                        Len2 = 64-(InPos+Len1)  ! Input%Low  -> Output%High
                        Len3 = Len-(Len1+Len2)  ! Input%High -> Output%High
                        CALL MVBITS(InVal%Low,      InPos,  Len1, OutVal%Low,  OutPos)
                        CALL MVBITS(InVal%Low,  InPos+Len1, Len2, OutVal%High,      0)
                        CALL MVBITS(InVal%High,          0, Len3, OutVal%High,   Len2)
                    ELSE
                        Len1 = 64-InPos         ! Input%Low  -> Output%Low
                        Len2 = 64-(OutPos+Len1) ! Input%High -> Output%Low
                        Len3 = Len-(Len1+Len2)  ! Input%High -> Output%High
                        CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%Low,       OutPos)
                        CALL MVBITS(InVal%High,     0, Len2, OutVal%Low,  OutPos+Len1)
                        CALL MVBITS(InVal%High,  Len2, Len3, OutVal%High,           0)
                    END IF
                END IF
            ELSE
                ! bit fields are in both lower and upper element of input but
                ! only in upper element of output
                Len1 = 64-InPos
                Len2 = Len - Len1
                CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%High,      OutPos-64)
                CALL MVBITS(InVal%High,     0, Len2, OutVal%High, OutPos-64+Len1)
            END IF
        END IF
    ELSE
        IF (OutPos < 64) THEN
            IF (OutPos + Len <= 64) THEN
                ! one of the simplest cases where bit fields are in upper element of input
                ! and lower element of output, respectively
                CALL MVBITS(InVal%High, InPos-64, Len, OutVal%Low, OutPos)
            ELSE
                ! bit fields are in upper element of input but in both lower and
                ! upper elements of output
                Len1 = 64-OutPos
                Len2 = Len - Len1
                CALL MVBITS(InVal%High, InPos-64,      Len1, OutVal%Low,  OutPos)
                CALL MVBITS(InVal%High, InPos-64+Len1, Len2, OutVal%High,      0)
            END IF
        ELSE
            ! one of the simplest cases where bit fields are in upper elements
            CALL MVBITS(InVal%High, InPos-64, Len, OutVal%High, OutPos-64)
        END IF
    END IF

    RETURN

END SUBROUTINE I128_MoveBits

!------------------------------------------------------------------------------
!
!                           ARITHMETIC ROUTINES
!
!------------------------------------------------------------------------------

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    ADDITION OPERATIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION I128_UnaryPlus(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return result of the unary plus sign of the Sint128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: InVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = InVal

    RETURN

END FUNCTION I128_UnaryPlus

!******************************************************************************

SUBROUTINE I128_Increment(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase value of the input by 1

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(INOUT) :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Val%Low == -1_I8B) THEN
        Val%High = Val%High + 1_I8B
        Val%Low  = 0_I8B
    ELSE
        Val%Low  = Val%Low + 1_I8B
    END IF

    RETURN

END SUBROUTINE I128_Increment

!******************************************************************************

SUBROUTINE I128_Add_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I4B),  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry, OutLo

!** FLOW

    CALL AddU64(This%Low, INT(Other, KIND=I8B), 0_I8B, OutLo, Carry)
    This%Low  = OutLo
    IF (Other < 0_I8B) THEN
        This%High = This%High - 1_I8B + Carry
    ELSE
        This%High = This%High + Carry
    END IF

    RETURN
    
END SUBROUTINE I128_Add_I32

!******************************************************************************

SUBROUTINE I128_Add_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I8B),  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry, OutLo

!** FLOW

    CALL AddU64(This%Low, Other, 0_I8B, OutLo, Carry)
    This%Low  = OutLo
    IF (Other < 0_I8B) THEN
        This%High = This%High - 1_I8B + Carry
    ELSE
        This%High = This%High + Carry
    END IF

    RETURN

END SUBROUTINE I128_Add_I64

!******************************************************************************

SUBROUTINE I128_Add_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(INOUT) :: This
    TYPE(SInt128), INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry, OutLo, OutHi

!** FLOW
        
    CALL AddU64(This%Low, Other%Low, 0_I8B, OutLo, Carry)
    CALL AddU64(This%High, Other%High, Carry, OutHi)
    This%Low  = OutLo
    This%High = OutHi

    RETURN
    
END SUBROUTINE I128_Add_I128

!******************************************************************************

FUNCTION I128_Plus_I32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = LhsVal + RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, INT(RhsVal, KIND=I8B), 0_I8B, OutVal%Low, Carry)
    IF (RhsVal < 0) THEN
        OutVal%High = LhsVal%High - 1_I8B + Carry
    ELSE
        OutVal%High = LhsVal%High + Carry
    END IF

    RETURN
    
END FUNCTION I128_Plus_I32

!******************************************************************************

FUNCTION I32_Plus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = LhsVal + RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)    :: LhsVal
    TYPE(SInt128),      INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)       :: Carry

!** FLOW

    CALL AddU64(INT(LhsVal, KIND=I8B), RhsVal%Low, 0_I8B, OutVal%Low, Carry)
    IF (LhsVal < 0) THEN
        OutVal%High = RhsVal%High - 1_I8B + Carry
    ELSE
        OutVal%High = RhsVal%High + Carry
    END IF

    RETURN
    
END FUNCTION I32_Plus_I128

!******************************************************************************

FUNCTION I128_Plus_I64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = LhsVal + RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I8B),  INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW
        
    CALL AddU64(LhsVal%Low, RhsVal, 0_I8B, OutVal%Low, Carry)
    IF (RhsVal < 0_I8B) THEN
        OutVal%High = LhsVal%High - 1_I8B + Carry
    ELSE
        OutVal%High = LhsVal%High + Carry
    END IF

    RETURN
    
END FUNCTION I128_Plus_I64

!******************************************************************************

FUNCTION I64_Plus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = LhsVal + RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)    :: LhsVal
    TYPE(SInt128),      INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL AddU64(LhsVal, RhsVal%Low, 0_I8B, OutVal%Low, Carry)
    IF (LhsVal < 0_I8B) THEN
        OutVal%High = RhsVal%High - 1_I8B + Carry
    ELSE
        OutVal%High = RhsVal%High + Carry
    END IF

    RETURN
    
END FUNCTION I64_Plus_I128

!******************************************************************************

FUNCTION I128_Plus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition of two SInt128 objects (Lhs + Rhs)

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: LhsVal
    TYPE(SInt128), INTENT(IN)    :: RhsVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, RhsVal%Low, 0_I8B, OutVal%Low, Carry)
    CALL AddU64(LhsVal%High, RhsVal%High, Carry, OutVal%High)

    RETURN

END FUNCTION I128_Plus_I128

!******************************************************************************

PURE SUBROUTINE AddU64(X, Y, CarryIn, Sum, CarryOut)

!DIR$ ATTRIBUTES INLINE :: AddU64

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the sum with carry of X, Y and CarryIn: Sum = X + Y + CarryIn.
    ! The carry input must be 0 or 1; otherwise the behavior is undefined.
    ! The carry output is guaranteed to be 0 or 1.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)     :: X, Y, CarryIn
    INTEGER(KIND=I8B), INTENT(OUT)    :: Sum, CarryOut
    OPTIONAL                          :: CarryOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

	Sum = X + Y + CarryIn
    ! The sum will overflow if both top bits are set (x & y) or if one of them
    ! is (x | y), and a carry from the lower place happened. If such a carry
    ! happens, the top bit will be 1 + 0 + 1 = 0 (&^ sum).
	IF (PRESENT(CarryOut)) THEN
        CarryOut = SHIFTR(IOR(IAND(X, Y), IAND(IOR(X, Y), NOT(Sum))), 63)
    END IF

    RETURN

END SUBROUTINE AddU64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  SUBTRACTION OPERATIONS                  ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION I128_Negate(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To negate the Uint128 object

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: InVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = NOT(InVal%High)
    IF (InVal%Low == 0_I8B) OutVal%High = OutVal%High + 1_I8B
    OutVal%Low = NOT(InVal%Low) + 1_I8B

    RETURN

END FUNCTION I128_Negate

!******************************************************************************

SUBROUTINE I128_Decrement(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease value of the input by 1

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(INOUT) :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Val%Low == 0_I8B) THEN
        Val%High = Val%High - 1_I8B
        Val%Low  = -1_I8B
    ELSE
        Val%Low  = Val%Low - 1_I8B
    END IF

    RETURN

END SUBROUTINE I128_Decrement

!******************************************************************************

SUBROUTINE I128_Subtract_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I4B),  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow, OutLo

!** FLOW

    CALL SubU64(This%Low, INT(Other, KIND=I8B), 0_I8B, OutLo, Borrow)
    This%Low = OutLo
    IF (Other < 0) THEN
        This%High = This%High + 1_I8B - Borrow
    ELSE
        This%High = This%High - Borrow
    END IF

    RETURN
    
END SUBROUTINE I128_Subtract_I32

!******************************************************************************

SUBROUTINE I128_Subtract_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I8B),  INTENT(IN)        :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow, OutLo

!** FLOW

    CALL SubU64(This%Low, Other, 0_I8B, OutLo, Borrow)
    This%Low  = OutLo
    IF (Other < 0_I8B) THEN
        This%High = This%High + 1_I8B - Borrow
    ELSE
        This%High = This%High - Borrow
    END IF

    RETURN
    
END SUBROUTINE I128_Subtract_I64

!******************************************************************************

SUBROUTINE I128_Subtract_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(INOUT) :: This
    TYPE(SInt128), INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow, OutLo, OutHi

!** FLOW

    CALL SubU64(This%Low, Other%Low, 0_I8B, OutLo, Borrow)
    CALL SubU64(This%High, Other%High, Borrow, OutHi)
    This%Low  = OutLo
    This%High = OutHi

    RETURN
    
END SUBROUTINE I128_Subtract_I128

!******************************************************************************

FUNCTION I128_Minus_I32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = LhsVal - RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, INT(RhsVal, KIND=I8B), 0_I8B, OutVal%Low, Borrow)
    IF (RhsVal < 0) THEN
        OutVal%High = LhsVal%High + 1_I8B - Borrow
    ELSE
        OutVal%High = LhsVal%High - Borrow
    END IF

    RETURN
    
END FUNCTION I128_Minus_I32

!******************************************************************************

FUNCTION I32_Minus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = LhsVal - RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)    :: LhsVal
    TYPE(SInt128),      INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow

!** FLOW

    CALL SubU64(INT(LhsVal, KIND=I8B), RhsVal%Low, 0_I8B, OutVal%Low, Borrow)
    IF (LhsVal < 0) THEN
        OutVal%High = -(RhsVal%High + 1_I8B + Borrow)
    ELSE
        OutVal%High = -(RhsVal%High + Borrow)
    END IF

    RETURN
    
END FUNCTION I32_Minus_I128

!******************************************************************************

FUNCTION I128_Minus_I64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = LhsVal - RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I8B),  INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, RhsVal, 0_I8B, OutVal%Low, Borrow)
    IF (RhsVal < 0_I8B) THEN
        OutVal%High = LhsVal%High + 1_I8B - Borrow
    ELSE
        OutVal%High = LhsVal%High - Borrow
    END IF

    RETURN
    
END FUNCTION I128_Minus_I64

!******************************************************************************

FUNCTION I64_Minus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = LhsVal - RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)    :: LhsVal
    TYPE(SInt128),      INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow

!** FLOW

    CALL SubU64(LhsVal, RhsVal%Low, 0_I8B, OutVal%Low, Borrow)
    IF (LhsVal < 0_I8B) THEN
        OutVal%High = -(RhsVal%High + 1_I8B + Borrow)
    ELSE
        OutVal%High = -(RhsVal%High + Borrow)
    END IF

    RETURN
    
END FUNCTION I64_Minus_I128

!******************************************************************************

FUNCTION I128_Minus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction of two SInt128 objects (Lhs - Rhs)

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: LhsVal
    TYPE(SInt128), INTENT(IN)    :: RhsVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow

!** FLOW
    
    CALL SubU64(LhsVal%Low, RhsVal%Low, 0_I8B, OutVal%Low, Borrow)
    CALL SubU64(LhsVal%High, RhsVal%High, Borrow, OutVal%High)

    RETURN

END FUNCTION I128_Minus_I128

!******************************************************************************

PURE SUBROUTINE SubU64(X, Y, BorrowIn, Diff, BorrowOut)

!DIR$ ATTRIBUTES INLINE :: SubU64

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the difference of X, Y and BorrowIn: Diff = X - Y - BorrowIn.
    ! The borrow input must be 0 or 1; otherwise the behavior is undefined.
    ! The borrow output is guaranteed to be 0 or 1.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)     :: X, Y, BorrowIn
    INTEGER(KIND=I8B), INTENT(OUT)    :: Diff, BorrowOut
    OPTIONAL                :: BorrowOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

	Diff = X - Y - BorrowIn
    ! The difference will underflow if the top bit of x is not set and the top
    ! bit of y is set (^x & y) or if they are the same (^(x ^ y)) and a Borrow
    ! from the lower place happens. If that Borrow happens, the result will be
    ! 1 - 1 - 1 = 0 - 0 - 1 = 1 (& diff).
	IF (PRESENT(BorrowOut)) THEN
	    BorrowOut = SHIFTR(IOR(IAND(NOT(X), Y), IAND(NOT(IEOR(X, Y)), Diff)), 63)
    END IF

    RETURN

END SUBROUTINE SubU64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                 MULTIPLICATION OPERATIONS                ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE I128_Times_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I4B),  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: X_Lo, Y_Lo, Y_Hi, ProductHi
    INTEGER(KIND=I8B)     :: AbsOther

!** FLOW
    
    AbsOther = ABS(Other)

    ! perform 'UMul128_Upper64'
    X_Lo = IAND(AbsOther, Mask32)
    Y_Lo = IAND(This%Low, Mask32)
    Y_Hi = SHIFTR(This%Low, 32)
    ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32) + X_Lo*Y_Hi, 32)
    
    This%High = AbsOther * This%High + ProductHi
    This%Low  = AbsOther * This%Low
    
    IF (Other < 0) This = -This

    RETURN

END SUBROUTINE I128_Times_I32

!******************************************************************************

SUBROUTINE I128_Times_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I8B),  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: AbsOther

!** FLOW
    
    AbsOther = ABS(Other)

    This%High = This%High * AbsOther + UMul128_Upper64(This%Low, AbsOther)
    This%Low  = This%Low * AbsOther
    
    IF (Other < 0_I8B) This = -This

    RETURN

END SUBROUTINE I128_Times_I64

!******************************************************************************

SUBROUTINE I128_Times_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(INOUT) :: This
    TYPE(SInt128), INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    This%High = This%Low * Other%High + This%High * Other%Low + &
                UMul128_Upper64(This%Low, Other%Low)
    This%Low  = This%Low * Other%Low

    RETURN

END SUBROUTINE I128_Times_I128

!******************************************************************************

FUNCTION I32_Multiply_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = LhsVal * RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)    :: LhsVal
    TYPE(SInt128),      INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: X_Lo, Y_Lo, Y_Hi, ProductHi
    INTEGER(KIND=I8B)     :: AbsLhs

!** FLOW
    
    AbsLhs = ABS(LhsVal)

    ! perform 'UMul128_Upper64'
    X_Lo = IAND(AbsLhs, Mask32)
    Y_Lo = IAND(RhsVal%Low, Mask32)
    Y_Hi = SHIFTR(RhsVal%Low, 32)
    ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32) + X_Lo*Y_Hi, 32)
    
    OutVal%High = AbsLhs * RhsVal%High + ProductHi
    OutVal%Low  = AbsLhs * RhsVal%Low
    
    IF (LhsVal < 0) OutVal = -OutVal

    RETURN

END FUNCTION I32_Multiply_I128

!******************************************************************************

FUNCTION I128_Multiply_I32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = LhsVal * RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: X_Lo, Y_Lo, Y_Hi, ProductHi
    INTEGER(KIND=I8B)     :: AbsRhs

!** FLOW
    
    AbsRhs = ABS(RhsVal)

    ! perform 'UMul128_Upper64'
    X_Lo = IAND(AbsRhs, Mask32)
    Y_Lo = IAND(LhsVal%Low, Mask32)
    Y_Hi = SHIFTR(LhsVal%Low, 32)
    ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32) + X_Lo*Y_Hi, 32)
    
    OutVal%High = LhsVal%High * AbsRhs + ProductHi
    OutVal%Low  = LhsVal%Low * AbsRhs
    
    IF (RhsVal < 0) OutVal = -OutVal

    RETURN

END FUNCTION I128_Multiply_I32

!******************************************************************************

FUNCTION I64_Multiply_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = LhsVal * RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)    :: LhsVal
    TYPE(SInt128),      INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: AbsLhs

!** FLOW
    
    AbsLhs = ABS(LhsVal)

    OutVal%High = AbsLhs * RhsVal%High + UMul128_Upper64(AbsLhs, RhsVal%Low)
    OutVal%Low  = AbsLhs * RhsVal%Low

    IF (LhsVal < 0_I8B) OutVal = -OutVal

    RETURN

END FUNCTION I64_Multiply_I128

!******************************************************************************

FUNCTION I128_Multiply_I64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = LhsVal * RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I8B),  INTENT(IN)    :: RhsVal
    TYPE(SInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: AbsRhs

!** FLOW
    
    AbsRhs = ABS(RhsVal)

    OutVal%High = LhsVal%High * AbsRhs + UMul128_Upper64(LhsVal%Low, AbsRhs)
    OutVal%Low  = LhsVal%Low * AbsRhs
    
    IF (RhsVal < 0_I8B) OutVal = -OutVal

    RETURN

END FUNCTION I128_Multiply_I64

!******************************************************************************

FUNCTION I128_Multiply_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication of two SInt128 objects (Lhs * Rhs)

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: LhsVal
    TYPE(SInt128), INTENT(IN)    :: RhsVal
    TYPE(SInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = LhsVal%Low * RhsVal%High + LhsVal%High * RhsVal%Low + &
                  UMul128_Upper64(LhsVal%Low, RhsVal%Low)
    OutVal%Low  = LhsVal%Low * RhsVal%Low

    RETURN

END FUNCTION I128_Multiply_I128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++             DIVISION/MODULATION OPERATIONS               ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE I128_DivMod_I32(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor
    ! return both quotient and remainder

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I4B),  INTENT(IN)    :: Divisor
    TYPE(SInt128),      INTENT(OUT)   :: Quotient
    TYPE(SInt128),      INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END SUBROUTINE I128_DivMod_I32

!******************************************************************************

SUBROUTINE I128_DivMod_I64(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor and
    ! return both quotient and remainder

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I8B),  INTENT(IN)    :: Divisor
    TYPE(SInt128),      INTENT(OUT)   :: Quotient
    TYPE(SInt128),      INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END SUBROUTINE I128_DivMod_I64

!******************************************************************************

SUBROUTINE I128_DivMod_I128(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two SInt128 objects (Dividend / Divisor)
    ! and return both the quotient and the remainder

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: Dividend
    TYPE(SInt128), INTENT(IN)    :: Divisor
    TYPE(SInt128), INTENT(OUT)   :: Quotient
    TYPE(SInt128), INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)    :: UQuotient, URemainder

!** FLOW

    IF ((Dividend == MinI128).AND.(Divisor == -OneI128)) THEN
        ! not applicable for unsigned binary on two's complement
        CALL DisplaySevereError('Message from Routine '//'I128_Divide_I128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Dividend = MinI128 and Divisor = -1')
        RETURN
    END IF
    
    CALL UDivMod(UABS(Dividend), UABS(Divisor), UQuotient, URemainder)
    
    IF ((Dividend%High < 0_I8B) /= (Divisor%High < 0_I8B)) UQuotient = -UQuotient
    IF (Dividend%High < 0_I8B) URemainder = -URemainder

    Quotient%High  = BitCastToSigned(UQuotient%High)
    Quotient%Low   = UQuotient%Low
    Remainder%High = BitCastToSigned(URemainder%High)
    Remainder%Low  = URemainder%Low
    
    RETURN

END SUBROUTINE I128_DivMod_I128

!******************************************************************************

SUBROUTINE I128_Over_I32(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),               INTENT(INOUT)    :: This
    INTEGER(KIND=I4B),           INTENT(IN)       :: Other
    INTEGER(KIND=I4B), OPTIONAL, INTENT(OUT)      :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)    :: Dividend, Rem

!** FLOW

    Dividend = This
    CALL DivMod(Dividend, SInt128(Other), This, Rem)
    IF (PRESENT(Remainder)) Remainder = Rem

    RETURN

END SUBROUTINE I128_Over_I32

!******************************************************************************

SUBROUTINE I128_Over_I64(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),               INTENT(INOUT)    :: This
    INTEGER(KIND=I8B),           INTENT(IN)       :: Other
    INTEGER(KIND=I8B), OPTIONAL, INTENT(OUT)      :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)    :: Dividend, Rem

!** FLOW

    Dividend = This
    CALL DivMod(Dividend, SInt128(Other), This, Rem)
    IF (PRESENT(Remainder)) Remainder = Rem

    RETURN

END SUBROUTINE I128_Over_I64

!******************************************************************************

SUBROUTINE I128_Over_I128(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),           INTENT(INOUT)   :: This
    TYPE(SInt128),           INTENT(IN)      :: Other
    TYPE(SInt128), OPTIONAL, INTENT(OUT)     :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)    :: Dividend, Rem

!** FLOW

    Dividend = This
    IF (PRESENT(Remainder)) THEN
        CALL DivMod(Dividend, Other, This, Remainder)
    ELSE
        CALL DivMod(Dividend, Other, This, Rem)
    END IF

    RETURN

END SUBROUTINE I128_Over_I128

!******************************************************************************

FUNCTION I128_Divide_I32(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I4B),  INTENT(IN)    :: Divisor
    TYPE(SInt128)                     :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)    :: Remainder

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END FUNCTION I128_Divide_I32

!******************************************************************************

FUNCTION I128_Divide_I64(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I8B),  INTENT(IN)    :: Divisor
    TYPE(SInt128)                     :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)    :: Remainder

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END FUNCTION I128_Divide_I64

!******************************************************************************

FUNCTION I128_Divide_I128(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two SInt128 objects (Dividend / Divisor)
    ! and return the quotient

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: Dividend
    TYPE(SInt128), INTENT(IN)    :: Divisor
    TYPE(SInt128)                :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)    :: Remainder

!** FLOW

    CALL DivMod(Dividend, Divisor, Quotient, Remainder)

    RETURN

END FUNCTION I128_Divide_I128

!******************************************************************************

FUNCTION I128_Mod_I32(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation:  Remainder = Dividend MOD Divisor

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I4B),  INTENT(IN)    :: Divisor
    TYPE(SInt128)                     :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)    :: Quotient

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END FUNCTION I128_Mod_I32

!******************************************************************************

FUNCTION I128_Mod_I64(Dividend, Divisor) RESULT(Remainder)  

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation:  Remainder = Dividend MOD Divisor

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I8B),  INTENT(IN)    :: Divisor
    TYPE(SInt128)                     :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)    :: Quotient

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END FUNCTION I128_Mod_I64

!******************************************************************************

FUNCTION I128_Mod_I128(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two SInt128 objects (Dividend / Divisor)
    ! and return the remainder

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: Dividend
    TYPE(SInt128), INTENT(IN)    :: Divisor
    TYPE(SInt128)                :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)    :: Quotient

!** FLOW

    CALL DivMod(Dividend, Divisor, Quotient, Remainder)
    
    RETURN

END FUNCTION I128_Mod_I128

!------------------------------------------------------------------------------
!
!                           INQUIRY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION I128_Is_Zero(I128) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the number is zero or not

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: I128
    LOGICAL                      :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = I128 == ZeroI128

    RETURN

END FUNCTION I128_Is_Zero

!******************************************************************************

FUNCTION I128_Is_Negative(I128) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the number is negative or not

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: I128
    LOGICAL                      :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = I128%High < 0_I8B

    RETURN

END FUNCTION I128_Is_Negative

!******************************************************************************

FUNCTION I128_Is_Positive(I128) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the number is positive or not

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: I128
    LOGICAL                      :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (I128%High > 0_I8B).OR.((I128%High == 0_I8B).AND.(I128%Low /= 0_I8B))

    RETURN

END FUNCTION I128_Is_Positive

!------------------------------------------------------------------------------
!
!                           AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION U64_To_I64(U64) RESULT(I64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert unsigned 64-bit integer to signed 64-bit integer

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN) :: U64      ! number treated as unsigned
    INTEGER(KIND=I8B)             :: I64      ! number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! Casting an unsigned integer to a signed integer of the same
    ! width is implementation defined behavior if the source value would not fit
    ! in the destination type. We step around it with a roundtrip bitwise not
    ! operation to make sure this function remains constexpr.
    IF (IAND(U64, SHIFTL(1_I8B, 63)) /= 0_I8B) THEN
        I64 = NOT(NOT(U64))
    ELSE
        I64 = U64
    END IF

    RETURN
            
END FUNCTION U64_To_I64

!******************************************************************************

FUNCTION I128_UnsignedAbsolute(I128) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the unsigned absolute value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: I128
    TYPE(UInt128)                :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! Cast to UInt128 before possibly negating because -Int128Min is undefined.
    IF (I128%High < 0_I8B) THEN
        U128 = -ToU128(I128)
    ELSE
        U128 = ToU128(I128)
    END IF

    RETURN

END FUNCTION I128_UnsignedAbsolute

!******************************************************************************

FUNCTION I128_Absolute(I128) RESULT(ABS)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the absolute value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)    :: I128
    TYPE(SInt128)                :: ABS

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (IsNegative(I128)) THEN
        ABS = -I128
    ELSE
        ABS = I128
    END IF

    RETURN

END FUNCTION I128_Absolute

!******************************************************************************

SUBROUTINE I128_Write(I128, Unit, IOStat, IOMsg, ShowComponent, Prefix)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write 'SInt128' object to the screen (or the specified unit)

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128),               INTENT(IN)     :: I128
    INTEGER(KIND=I4B), OPTIONAL, INTENT(IN)     :: Unit             ! output logical unit
    INTEGER(KIND=I4B), OPTIONAL, INTENT(OUT)    :: IOStat           ! io stat
    CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT)    :: IOMsg            ! io message
    LOGICAL,           OPTIONAL, INTENT(IN)     :: ShowComponent    ! flag
                                                                    ! if flag is present and true, write compoents of the object
                                                                    ! otherwise, write the object as a decimal string
    CHARACTER(LEN=*),  OPTIONAL, INTENT(IN)     :: Prefix           ! prefix string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    LOGICAL                         :: AsString
    INTEGER(KIND=I4B)               :: OutUnit
    INTEGER(KIND=I4B)               :: IO_Stat
    CHARACTER(LEN=128)              :: IO_Msg
    CHARACTER(LEN=:), ALLOCATABLE   :: DispStr

!** FLOW

    ! set defaults
    OutUnit  = OUTPUT_UNIT
    AsString = TrueVal
    
    ! check optional input
    IF (PRESENT(ShowComponent)) THEN
        IF (ShowComponent) AsString = FalseVal
    END IF
    IF (PRESENT(Unit)) OutUnit = Unit
    
    ! write the object
    IF (AsString) THEN
        IF (PRESENT(Prefix)) THEN
            DispStr = Prefix // ToDecString(I128)
        ELSE
            DispStr = ' I128 = ' // ToDecString(I128)
        END IF
        WRITE(UNIT=OutUnit, FMT='(A)', IOSTAT=IO_Stat, IOMSG=IO_Msg) DispStr
    ELSE
        DispStr = '-: '
        IF (PRESENT(Prefix)) DispStr = Prefix
        WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
              DispStr // 'High value = ', I128%High
        WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
              DispStr // 'Low value = ', I128%Low
    END IF
    
    ! return output if requested
    IF (PRESENT(IOStat)) IOStat = IO_Stat
    IF (PRESENT(IOMsg))  IOMsg  = IO_Msg
 
    RETURN
            
END SUBROUTINE I128_Write

!******************************************************************************

END MODULE ModBase_SInt128

!******************************************************************************
