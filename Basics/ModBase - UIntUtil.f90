
MODULE ModBase_UIntUtil

!^ **PURPOSE OF THIS MODULE**:  
    ! This module contains routines that perform comparisons and arithmetic
    ! operations for unsigned integers.  The module also provide various
    ! utility routines including conversions from an unsigned integer to
    ! a string as well as unsigned multiplications (to higher precision).  
    !    
!^ **REFERENCES**:  
    !	[1] [Guava: Google Core Libraries for Java](https://github.com/google/guava)  
    !	[2] [Java's OpenJDK](https://github.com/openjdk/jdk)  
    !	[3] [FLIBS - A collection of Fortran modules](https://flibs.sourceforge.net/)
    !    
!^ **TECHNICAL NOTES**:  
    ! According to [3], Fortran does not natively support unsigned integers but
    !   intrinsic (signed) integer types in Fortran behave the same as unsigned
    !   one for positive values.  For negative values, however, they behave differently
    !   for the following operations:  
    !    - *conversion*:   to and from string  
    !    - *comparision*:  less than (<), less than or equal to(<=),
    !                      greater than (>), greater than or equal to (>=)  
    !    - *arithmetic*:   division (/) and modulation (MOD)  
    !   (note that in two's complement arithmetic, the three other basic
    !    arithmetic operations of add, subtract, and multiply are bit-wise
    !    identical if the two operands are regarded as both being signed
    !    or both being unsigned.)  
    ! Therefore, the module only provides routines for those operations that differ.
    !   For those operations (the one that not yet mentioned including bitwise one)
    !   that have the same behaviors, normal Fortran expressions can be used.  
    ! It is important to note that routines in this module cannot differentiate between
    !   signed and unsigned integers (i.e. they assume all integers (input and/or output)
    !   to be unsigned with the exception of output of 'CompareUnsigned' routines); thus,
    !   users of this module must be extremely careful not to mix up signed and unsigned
    !   integers in the same expressions.  
    ! The application interfaces (APIs) used in this module follows closely those used in
    !   reference [3].  However, the implementations used here are mostly based on those
    !   in references [1] and [2] because it appears that algorithms used in [1] and [2]
    !   are more efficient than those used in [3].

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_SIntUtil

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! parameters
    PUBLIC :: MAX_U8, MAX_U16, MAX_U32, MAX_U64
    PUBLIC :: MIN_U8, MIN_U16, MIN_U32, MIN_U64
    ! comparison operators
    PUBLIC :: OPERATOR (.ULT.)
    PUBLIC :: OPERATOR (.ULE.)
    PUBLIC :: OPERATOR (.UGT.)
    PUBLIC :: OPERATOR (.UGE.)
    ! arithmetic operator and procedures
    PUBLIC :: OPERATOR (.UDIV.)
    PUBLIC :: UMOD, UDivMod
    ! comparison procedures
    PUBLIC :: CompareUnsigned
    ! conversion procedures
    PUBLIC :: ToUnsignedInteger
    PUBLIC :: ToUnsignedLong
    PUBLIC :: ToDecStrUnsigned
    PUBLIC :: ToHexStrUnsigned
    ! unsigned multiplications
    PUBLIC :: UMul96_Upper64
    PUBLIC :: UMul96_Lower64
    PUBLIC :: UMul128           ! U128_Multiply
    PUBLIC :: UMul128_Upper64   ! U128_Multiply_High
    PUBLIC :: UMul128_N_Add     ! U128_Multiply_N_Add
    PUBLIC :: UMul128_N_Shift   ! U128_Multiply_N_Shift
    PUBLIC :: UMul192_Upper128
    PUBLIC :: UMul192_Lower128

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! bit and byte sizes
    INTEGER(KIND=I4B), PARAMETER  :: Byte_BitSize     = BIT_SIZE(1_I1B)     !  8
    INTEGER(KIND=I4B), PARAMETER  :: Byte_ByteSize    = I1B                 !  1
    INTEGER(KIND=I4B), PARAMETER  :: Short_BitSize    = BIT_SIZE(1_I2B)     ! 16
    INTEGER(KIND=I4B), PARAMETER  :: Short_ByteSize   = I2B                 !  2
    INTEGER(KIND=I4B), PARAMETER  :: Integer_BitSize  = BIT_SIZE(1_I4B)     ! 32
    INTEGER(KIND=I4B), PARAMETER  :: Integer_ByteSize = I4B                 !  4
    INTEGER(KIND=I4B), PARAMETER  :: Long_BitSize     = BIT_SIZE(1_I8B)     ! 64
    INTEGER(KIND=I4B), PARAMETER  :: Long_ByteSize    = I8B                 !  8
    ! maximum values
    INTEGER(KIND=I1B), PARAMETER  :: MAX_U8  = INT(Z'FF', KIND=I1B)                 !! 255
    INTEGER(KIND=I2B), PARAMETER  :: MAX_U16 = INT(Z'FFFF', KIND=I2B)               !! 65535
    INTEGER(KIND=I4B), PARAMETER  :: MAX_U32 = INT(Z'FFFFFFFF', KIND=I4B)           !! 4294967295
    INTEGER(KIND=I8B), PARAMETER  :: MAX_U64 = INT(Z'FFFFFFFFFFFFFFFF', KIND=I8B)   !! 18446744073709551615
    ! minimum values
    INTEGER(KIND=I1B), PARAMETER  :: MIN_U8  = 0_I1B
    INTEGER(KIND=I2B), PARAMETER  :: MIN_U16 = 0_I2B
    INTEGER(KIND=I4B), PARAMETER  :: MIN_U32 = 0_I4B
    INTEGER(KIND=I8B), PARAMETER  :: MIN_U64 = 0_I8B
    ! other parameters
    INTEGER(KIND=I8B), PARAMETER  :: MaskU32 = INT(Z'00000000FFFFFFFF', KIND=I8B)

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE OPERATOR (.ULT.)
        !^ **Operator Overload**: OPERATOR(.ULT.)  
        !  **Purpose**:  To check if the LHS value is less than the RHS value  
        !   return .TRUE. if LHS < RHS; otherwise return .FALSE.  
        !  **Usage**:  
        !   --->    Flag = LHS .ULT. RHS  
        !   --->    IF (LHS .ULT. RHS) DoSomething
        MODULE PROCEDURE    UInt8_LT
        MODULE PROCEDURE    UInt16_LT
        MODULE PROCEDURE    UInt32_LT
        MODULE PROCEDURE    UInt64_LT
    END INTERFACE
    INTERFACE OPERATOR (.ULE.)
        !^ **Operator Overload**: OPERATOR(.ULE.)  
        !  **Purpose**:  To check if the LHS value is less than or equal to the RHS value  
        !   return .TRUE. if LHS <= RHS; otherwise return .FALSE.  
        !  **Usage**:  
        !   --->    Flag = LHS .ULE. RHS  
        !   --->    IF (LHS .ULE. RHS) DoSomething
        MODULE PROCEDURE    UInt8_LE
        MODULE PROCEDURE    UInt16_LE
        MODULE PROCEDURE    UInt32_LE
        MODULE PROCEDURE    UInt64_LE
    END INTERFACE
    INTERFACE OPERATOR (.UGT.)
        !^ **Operator Overload**: OPERATOR(.UGT.)  
        !  **Purpose**:  To check if the LHS value is greater than the RHS value  
        !   return .TRUE. if LHS > RHS; otherwise return .FALSE.  
        !  **Usage**:  
        !   --->    Flag = LHS .UGT. RHS  
        !   --->    IF (LHS .UGT. RHS) DoSomething
        MODULE PROCEDURE    UInt8_GT
        MODULE PROCEDURE    UInt16_GT
        MODULE PROCEDURE    UInt32_GT
        MODULE PROCEDURE    UInt64_GT
    END INTERFACE
    INTERFACE OPERATOR (.UGE.)
        !^ **Operator Overload**: OPERATOR(.UGE.)  
        !  **Purpose**:  To check if the LHS value is greater than or equal to the RHS value  
        !   return .TRUE. if LHS >= RHS; otherwise return .FALSE.  
        !  **Usage**:  
        !   --->    Flag = LHS .UGE. RHS  
        !   --->    IF (LHS .UGE. RHS) DoSomething
        MODULE PROCEDURE    UInt8_GE
        MODULE PROCEDURE    UInt16_GE
        MODULE PROCEDURE    UInt32_GE
        MODULE PROCEDURE    UInt64_GE
    END INTERFACE
    INTERFACE OPERATOR (.UDIV.)
        !^ **Operator Overload**: OPERATOR(.UDIV.)  
        !  **Purpose**:  To return the quotient of a division of two unsigned integers,
        !   where both input and an output have the same kind  
        !  **Usage**:  
        !   --->    QUOT = NUMER .UDIV. DENOM
        MODULE PROCEDURE    UInt8_Divide
        MODULE PROCEDURE    UInt16_Divide
        MODULE PROCEDURE    UInt32_Divide
        MODULE PROCEDURE    UInt64_Divide
    END INTERFACE
    INTERFACE UMOD
        !^ **Function Interface**: UMOD  
        !  **Purpose**:  To return the remainder of a division of two unsigned integers,
        !   where both input and an output have the same kind  
        !  **Usage**:  
        !   --->    REM = UMOD(NUMER, DENOM)
        MODULE PROCEDURE    UInt8_Remainder
        MODULE PROCEDURE    UInt16_Remainder
        MODULE PROCEDURE    UInt32_Remainder
        MODULE PROCEDURE    UInt64_Remainder
    END INTERFACE
    INTERFACE UDivMod
        !^ **Subroutine Interface**: UDivMod  
        !  **Purpose**:  To perform a division of two unsigned integers and then return both
        !   the quotient and the remainder where both input and output have the same kind  
        !  **Usage**:  
        !   --->    CALL UDivMod(NUMER, DENOM, QUOT, REM)
        MODULE PROCEDURE    UInt32_DivMod
        MODULE PROCEDURE    UInt64_DivMod
    END INTERFACE
    INTERFACE CompareUnsigned
        !^ **Function Interface**: CompareUnsigned  
        !  **Purpose**:  To compare two unsigned integers (of the same kind) and return  
        !   -1 if LHS < RHS  
        !    0 if LHS == RHS  
        !    1 if LHS > RHS  
        !  **Usage**:  
        !   --->    Flag = CompareUnsigned(LHS, RHS)  
        !   --->    IF (CompareUnsigned(LHS, RHS) /= 0) DoSomething
        MODULE PROCEDURE    Compare_UInt8
        MODULE PROCEDURE    Compare_UInt16
        MODULE PROCEDURE    Compare_UInt32
        MODULE PROCEDURE    Compare_UInt64
    END INTERFACE
    INTERFACE ToUnsignedInteger
        !^ **Function Interface**: ToUnsignedInteger  
        !  **Purpose**:  To perform unsigned conversion from lower-precision unsigned integer
        !   to 32-bit unsigned integer  
        !  **Usage**:  
        !   --->    U32 = ToUnsignedInteger(U8)
        MODULE PROCEDURE    ByteToUnsignedInteger
        MODULE PROCEDURE    ShortToUnsignedInteger
    END INTERFACE
    INTERFACE ToUnsignedLong
        !^ **Function Interface**: ToUnsignedLong  
        !  **Purpose**:  To perform unsigned conversion from lower-precision unsigned integer
        !   to 64-bit unsigned integer  
        !  **Usage**:  
        !   --->    U64 = ToUnsignedLong(U32)
        MODULE PROCEDURE    ByteToUnsignedLong
        MODULE PROCEDURE    ShortToUnsignedLong
        MODULE PROCEDURE    IntegerToUnsignedLong
    END INTERFACE
    INTERFACE ToDecStrUnsigned
        !^ **Function Interface**: ToDecStrUnsigned  
        !  **Purpose**:  To convert an unsigned integer to a decimal string  
        !  **Usage**:  
        !   --->    Str = ToDecStrUnsigned(U32)
        MODULE PROCEDURE    U32_ToDecString
        MODULE PROCEDURE    U64_ToDecString
    END INTERFACE
    INTERFACE ToHexStrUnsigned
        !^ **Function Interface**: ToHexStrUnsigned  
        !  **Purpose**:  To convert an unsigned integer to a hexadecimal string  
        !  **Usage**:  
        !   --->    Str = ToHexStrUnsigned(U32)
        MODULE PROCEDURE    U32_ToHexString
        MODULE PROCEDURE    U64_ToHexString
    END INTERFACE

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ROUTINES FOR BYTE INTEGER NUMBER
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION UInt8_LT(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt8_LT

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS < RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I1B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) < ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt8_LT

!******************************************************************************

PURE FUNCTION UInt8_LE(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt8_LE

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS <= RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I1B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) <= ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt8_LE

!******************************************************************************

PURE FUNCTION UInt8_GT(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt8_GT

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS > RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I1B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) > ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt8_GT

!******************************************************************************

PURE FUNCTION UInt8_GE(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt8_GT

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS >= RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I1B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) >= ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt8_GE

!******************************************************************************

PURE FUNCTION UInt8_Divide(Dividend, Divisor) RESULT(ResVal)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt8_Divide

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I1B), INTENT(IN)   :: Dividend, Divisor
    INTEGER(KIND=I1B)               :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ResVal = INT(ToUnsignedInteger(Dividend)/ToUnsignedInteger(Divisor), KIND=I1B)

    RETURN

END FUNCTION UInt8_Divide

!******************************************************************************

PURE FUNCTION UInt8_Remainder(Dividend, Divisor) RESULT(Rem)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt8_Remainder

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned remainder from dividing the first argument
    ! by the second where each argument and the result is interpreted
    ! as an unsigned value.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I1B), INTENT(IN)   :: Dividend, Divisor
    INTEGER(KIND=I1B)               :: Rem

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
! na

!** FLOW

    Rem = INT(MOD(ToUnsignedInteger(Dividend), ToUnsignedInteger(Divisor)), KIND=I1B)

    RETURN

END FUNCTION UInt8_Remainder

!******************************************************************************

PURE FUNCTION Compare_UInt8(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: Compare_UInt8

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS where both numbers are treated as unsigned.  
    ! - return -1 if LHS < RHS  
    ! - return  0 if LHS == RHS  
    ! - return +1 if LHS > RHS

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I1B), INTENT(IN)   :: LHS, RHS
    INTEGER(KIND=I4B)               :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Compare(ToUnsignedInteger(LHS), ToUnsignedInteger(RHS))

    RETURN

END FUNCTION Compare_UInt8

!******************************************************************************

PURE FUNCTION ByteToUnsignedInteger(InVal) RESULT(OutVal)

!DIR$ ATTRIBUTES FORCEINLINE :: ByteToUnsignedInteger

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the argument to an integer by an unsigned
    ! conversion.  In an unsigned conversion to an integer, the
    ! high-order 24 bits of the integer are zero and the low-order
    ! 8 bits are equal to the bits of the byte argument.
    ! Consequently, zero and positive byte values are mapped
    ! to a numerically equal integer value and negative byte values
    ! are mapped to a integer value equal to the input plus 2**8.  
    ! InVal  - the value to convert to an unsigned integer  
    ! OutVal - the result converted to integer by an unsigned conversion

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I1B), INTENT(IN)   :: InVal
    INTEGER(KIND=I4B)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(INT(InVal, KIND=I4B), Z'000000FF')

    RETURN

END FUNCTION ByteToUnsignedInteger

!******************************************************************************

PURE FUNCTION ByteToUnsignedLong(InVal) RESULT(OutVal)

!DIR$ ATTRIBUTES FORCEINLINE :: ByteToUnsignedLong

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the argument to a long integer by an unsigned
    ! conversion.  In an unsigned conversion to a long, the
    ! high-order 56 bits of the long are zero and the low-order
    ! 8 bits are equal to the bits of the byte argument.
    ! Consequently, zero and positive byte values are mapped
    ! to a numerically equal long value and negative byte values
    ! are mapped to a long value equal to the input plus 2**8.  
    ! InVal  - the value to convert to an unsigned long  
    ! OutVal - the result converted to long by an unsigned conversion

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I1B), INTENT(IN)   :: InVal
    INTEGER(KIND=I8B)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(INT(InVal, KIND=I8B), Z'00000000000000FF')

    RETURN

END FUNCTION ByteToUnsignedLong

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ROUTINES FOR SHORT INTEGER NUMBER
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION UInt16_LT(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt16_LT

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS < RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I2B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) < ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt16_LT

!******************************************************************************

PURE FUNCTION UInt16_LE(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt16_LE

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS <= RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I2B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) <= ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt16_LE

!******************************************************************************

PURE FUNCTION UInt16_GT(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt16_GT

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS > RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I2B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) > ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt16_GT

!******************************************************************************

PURE FUNCTION UInt16_GE(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt16_GE

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS >= RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I2B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) >= ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt16_GE

!******************************************************************************

PURE FUNCTION UInt16_Divide(Dividend, Divisor) RESULT(ResVal)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt16_Divide

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I2B), INTENT(IN)   :: Dividend, Divisor
    INTEGER(KIND=I2B)               :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ResVal = INT(ToUnsignedInteger(Dividend)/ToUnsignedInteger(Divisor), KIND=I2B)

    RETURN

END FUNCTION UInt16_Divide

!******************************************************************************

PURE FUNCTION UInt16_Remainder(Dividend, Divisor) RESULT(Rem)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt16_Remainder

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned remainder from dividing the first argument
    ! by the second where each argument and the result is interpreted
    ! as an unsigned value.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I2B), INTENT(IN)   :: Dividend, Divisor
    INTEGER(KIND=I2B)               :: Rem

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Rem = INT(MOD(ToUnsignedInteger(Dividend), ToUnsignedInteger(Divisor)), KIND=I2B)

    RETURN

END FUNCTION UInt16_Remainder

!******************************************************************************

PURE FUNCTION Compare_UInt16(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: Compare_UInt16

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS where both numbers are treated as unsigned.  
    ! - return -1 if LHS < RHS  
    ! - return  0 if LHS == RHS  
    ! - return +1 if LHS > RHS

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I2B), INTENT(IN)   :: LHS, RHS
    INTEGER(KIND=I4B)               :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Compare(ToUnsignedInteger(LHS), ToUnsignedInteger(RHS))

    RETURN

END FUNCTION Compare_UInt16

!******************************************************************************

PURE FUNCTION ShortToUnsignedInteger(InVal) RESULT(OutVal)

!DIR$ ATTRIBUTES FORCEINLINE :: ShortToUnsignedInteger

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the argument to an integer by an unsigned
    ! conversion.  In an unsigned conversion to an integer, the
    ! high-order 16 bits of the integer are zero and the low-order
    ! 16 bits are equal to the bits of the short argument.
    ! Consequently, zero and positive short values are mapped
    ! to a numerically equal integer value and negative short values
    ! are mapped to a integer value equal to the input plus 2**16.  
    ! InVal  - the value to convert to an unsigned integer  
    ! OutVal - the result converted to integer by an unsigned conversion

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I2B), INTENT(IN)   :: InVal
    INTEGER(KIND=I4B)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(INT(InVal, KIND=I4B), Z'0000FFFF')

    RETURN

END FUNCTION ShortToUnsignedInteger

!******************************************************************************

PURE FUNCTION ShortToUnsignedLong(InVal) RESULT(OutVal)

!DIR$ ATTRIBUTES FORCEINLINE :: ShortToUnsignedLong

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the argument to a long integer by an unsigned
    ! conversion.  In an unsigned conversion to a long, the
    ! high-order 48 bits of the long are zero and the low-order
    ! 16 bits are equal to the bits of the short argument.
    ! Consequently, zero and positive short values are mapped
    ! to a numerically equal long value and negative short values
    ! are mapped to a long value equal to the input plus 2**16.  
    ! InVal  - the value to convert to an unsigned long  
    ! OutVal - the result converted to long by an unsigned conversion

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I2B), INTENT(IN)   :: InVal
    INTEGER(KIND=I8B)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(INT(InVal, KIND=I8B), Z'000000000000FFFF')

    RETURN

END FUNCTION ShortToUnsignedLong

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ROUTINES FOR DEFAULT INTEGER NUMBER
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION UInt32_LT(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt32_LT

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS < RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I32) < IEOR(RHS, MIN_I32)

    RETURN

END FUNCTION UInt32_LT

!******************************************************************************

PURE FUNCTION UInt32_LE(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt32_LE

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS <= RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I32) <= IEOR(RHS, MIN_I32)

    RETURN

END FUNCTION UInt32_LE

!******************************************************************************

PURE FUNCTION UInt32_GT(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt32_GT

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS > RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I32) > IEOR(RHS, MIN_I32)

    RETURN

END FUNCTION UInt32_GT

!******************************************************************************

PURE FUNCTION UInt32_GE(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt32_GE

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS >= RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I32) >= IEOR(RHS, MIN_I32)

    RETURN

END FUNCTION UInt32_GE

!******************************************************************************

PURE FUNCTION UInt32_Divide(Dividend, Divisor) RESULT(ResVal)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt32_Divide

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value.  
    ! Dividend - the value to be divided  
    ! Divisor  - the value doing the dividing  
    ! ResVal   - the unsigned quotient of the first argument divided by the second argument

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: Dividend, Divisor
    INTEGER(KIND=I4B)               :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! In lieu of tricky code, for now just use long arithmetic.
    ResVal = INT(ToUnsignedLong(Dividend)/ToUnsignedLong(Divisor), KIND=I4B)

    RETURN

END FUNCTION UInt32_Divide

!******************************************************************************

PURE FUNCTION UInt32_Remainder(Dividend, Divisor) RESULT(Rem)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt32_Remainder

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned remainder from dividing the first argument
    ! by the second where each argument and the result is interpreted
    ! as an unsigned value.  
    ! Dividend - the value to be divided  
    ! Divisor  - the value doing the dividing  
    ! Rem      - the unsigned remainder of the first argument divided by the second argument

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: Dividend, Divisor
    INTEGER(KIND=I4B)               :: Rem

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! In lieu of tricky code, for now just use long arithmetic.
    Rem = INT(MOD(ToUnsignedLong(Dividend), ToUnsignedLong(Divisor)), KIND=I4B)

    RETURN

END FUNCTION UInt32_Remainder

!******************************************************************************

PURE SUBROUTINE UInt32_DivMod(Dividend, Divisor, Quotient, Remainder)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt32_DivMod

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value.  
    ! Dividend  - the value to be divided  
    ! Divisor   - the value doing the dividing  
    ! Quotient  - the unsigned quotient of the first argument divided by the second argument  
    ! Remainder - the unsigned remainder of the first argument divided by the second argument

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)     :: Dividend, Divisor
    INTEGER(KIND=I4B), INTENT(OUT)    :: Quotient, Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: Q, R

!** FLOW

    ! In lieu of tricky code, for now just use long arithmetic.
    CALL UDivMod(ToUnsignedLong(Dividend), ToUnsignedLong(Divisor), Q, R)
    Quotient  = INT(Q, KIND=I4B)
    Remainder = INT(R, KIND=I4B)

    RETURN

END SUBROUTINE UInt32_DivMod

!******************************************************************************

PURE FUNCTION Compare_UInt32(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: Compare_UInt32

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS where both numbers are treated as unsigned.  
    ! - return -1 if LHS < RHS  
    ! - return  0 if LHS == RHS  
    ! - return +1 if LHS > RHS

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: LHS, RHS
    INTEGER(KIND=I4B)               :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Compare(IEOR(LHS, MIN_I32), IEOR(RHS, MIN_I32))

    RETURN

END FUNCTION Compare_UInt32

!******************************************************************************

PURE FUNCTION IntegerToUnsignedLong(InVal) RESULT(OutVal)

!DIR$ ATTRIBUTES FORCEINLINE :: IntegerToUnsignedLong

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the argument to a long integer by an unsigned
    ! conversion.  In an unsigned conversion to a long, the
    ! high-order 32 bits of the long are zero and the low-order
    ! 32 bits are equal to the bits of the integer argument.
    ! Consequently, zero and positive integer values are mapped
    ! to a numerically equal long value and negative integer values
    ! are mapped to a long value equal to the input plus 2**32.  
    ! InVal  - the value to convert to an unsigned long  
    ! OutVal - the result converted to long by an unsigned conversion

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: InVal
    INTEGER(KIND=I8B)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(INT(InVal, KIND=I8B), Z'00000000FFFFFFFF')

    RETURN

END FUNCTION IntegerToUnsignedLong

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ROUTINES FOR LONG INTEGER NUMBER
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION UInt64_LT(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt64_LT

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS < RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I64) < IEOR(RHS, MIN_I64)

    RETURN

END FUNCTION UInt64_LT

!******************************************************************************

PURE FUNCTION UInt64_LE(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt64_LE

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS <= RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I64) <= IEOR(RHS, MIN_I64)

    RETURN

END FUNCTION UInt64_LE

!******************************************************************************

PURE FUNCTION UInt64_GT(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt64_GT

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS > RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I64) > IEOR(RHS, MIN_I64)

    RETURN

END FUNCTION UInt64_GT

!******************************************************************************

PURE FUNCTION UInt64_GE(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt64_GE

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS >= RHS where both numbers are treated as unsigned.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: LHS, RHS
    LOGICAL                         :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I64) >= IEOR(RHS, MIN_I64)

    RETURN

END FUNCTION UInt64_GE

!******************************************************************************

PURE FUNCTION UInt64_Divide(Dividend, Divisor) RESULT(ResVal)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt64_Divide

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value.  
    ! Dividend - the value to be divided  
    ! Divisor  - the value doing the dividing  
    ! ResVal   - the unsigned quotient of the first argument divided by the second argument

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: Dividend, Divisor
    INTEGER(KIND=I8B)               :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: Quotient, Remainder

!** FLOW

    ! See Hacker's Delight (2nd ed), section 9.3
    IF (Divisor >= 0_I8B) THEN
        Quotient  = SHIFTL(SHIFTR(Dividend, 1)/Divisor, 1)
        Remainder = Dividend - Quotient*Divisor
        ResVal = Quotient + SHIFTR(IOR(Remainder, NOT(Remainder-Divisor)), (Long_BitSize-1))
    ELSE
        ResVal = SHIFTR(IAND(Dividend, NOT(Dividend-Divisor)), (Long_BitSize-1))
    END IF

    RETURN

END FUNCTION UInt64_Divide

!******************************************************************************

PURE FUNCTION UInt64_Remainder(Dividend, Divisor) RESULT(Rem)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt64_Remainder

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned remainder from dividing the first argument
    ! by the second where each argument and the result is interpreted
    ! as an unsigned value.  
    ! Dividend - the value to be divided  
    ! Divisor  - the value doing the dividing  
    ! Rem      - the unsigned remainder of the first argument divided by the second argument

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: Dividend, Divisor
    INTEGER(KIND=I8B)               :: Rem

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: Q, R

!** FLOW

    ! See Hacker's Delight (2nd ed), section 9.3
    IF (Divisor >= 0_I8B) THEN
        Q = SHIFTL(SHIFTR(Dividend, 1)/Divisor, 1)
        R = Dividend - Q*Divisor
        ! Here, 0 <= r < 2 * divisor
        ! (1) When 0 <= r < divisor, the remainder is simply r.
        ! (2) Otherwise the remainder is r - divisor.
        !
        ! In case (1), r - divisor < 0. Applying ~ produces a long with
        ! sign bit 0, so >> produces 0. The returned value is thus r.
        !
        ! In case (2), a similar reasoning shows that >> produces -1,
        ! so the returned value is r - divisor.
        Rem = R - IAND(SHIFTA(NOT(R-Divisor), (Long_BitSize-1)), Divisor)
    ELSE
        ! (1) When dividend >= 0, the remainder is dividend.
        ! (2) Otherwise
        !      (2.1) When dividend < divisor, the remainder is dividend.
        !      (2.2) Otherwise the remainder is dividend - divisor
        !
        ! A reasoning similar to the above shows that the returned value
        ! is as expected.
        Rem = Dividend - IAND(SHIFTA(IAND(Dividend, NOT(Dividend-Divisor)), &
                                     (Long_BitSize-1)), Divisor)
    END IF

    RETURN

END FUNCTION UInt64_Remainder

!******************************************************************************

PURE SUBROUTINE UInt64_DivMod(Dividend, Divisor, Quotient, Remainder)

!DIR$ ATTRIBUTES FORCEINLINE :: UInt64_DivMod

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value.  
    ! Dividend  - the value to be divided  
    ! Divisor   - the value doing the dividing  
    ! Quotient  - the unsigned quotient of the first argument divided by the second argument  
    ! Remainder - the unsigned remainder of the first argument divided by the second argument

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: Dividend, Divisor
    INTEGER(KIND=I8B), INTENT(OUT)  :: Quotient, Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: Q, R

!** FLOW

    ! See Hacker's Delight (2nd ed), section 9.3
    IF (Divisor >= 0_I8B) THEN
        Q = SHIFTL(SHIFTR(Dividend, 1)/Divisor, 1)
        R = Dividend - Q*Divisor
        Quotient  = Q + SHIFTR(IOR(R, NOT(R-Divisor)), (Long_BitSize-1))
        ! Here, 0 <= r < 2 * divisor
        ! (1) When 0 <= r < divisor, the remainder is simply r.
        ! (2) Otherwise the remainder is r - divisor.
        !
        ! In case (1), r - divisor < 0. Applying ~ produces a long with
        ! sign bit 0, so >> produces 0. The returned value is thus r.
        !
        ! In case (2), a similar reasoning shows that >> produces -1,
        ! so the returned value is r - divisor.
        Remainder = R - IAND(SHIFTA(NOT(R-Divisor), (Long_BitSize-1)), Divisor)
    ELSE
        Quotient = SHIFTR(IAND(Dividend, NOT(Dividend-Divisor)), (Long_BitSize-1))
        ! (1) When dividend >= 0, the remainder is dividend.
        ! (2) Otherwise
        !      (2.1) When dividend < divisor, the remainder is dividend.
        !      (2.2) Otherwise the remainder is dividend - divisor
        !
        ! A reasoning similar to the above shows that the returned value
        ! is as expected.
        Remainder = Dividend - IAND(SHIFTA(IAND(Dividend, NOT(Dividend-Divisor)), &
                                           (Long_BitSize-1)), Divisor)
    END IF

    RETURN

END SUBROUTINE UInt64_DivMod

!******************************************************************************

PURE FUNCTION Compare_UInt64(LHS, RHS) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: Compare_UInt64

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS where both numbers are treated as unsigned.  
    ! - return -1 if LHS < RHS  
    ! - return  0 if LHS == RHS  
    ! - return +1 if LHS > RHS

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: LHS, RHS
    INTEGER(KIND=I4B)               :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Compare(IEOR(LHS, MIN_I64), IEOR(RHS, MIN_I64))

    RETURN

END FUNCTION Compare_UInt64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ROUTINES FOR CONVERSION TO STRING
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION U32_ToDecString(Val) RESULT(RetStr)

! PURPOSE OF THIS FUNCTION:
    !! To convert a 32-bit integer treated as an unsigned number into a string.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: Val
    CHARACTER(LEN=:), ALLOCATABLE   :: RetStr

! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

! FLOW:

    IF (Val >= 0) THEN
        RetStr = ToDecStrSigned(Val)
    ELSE
        BLOCK
            INTEGER(KIND=I8B)               :: LongVal
            INTEGER(KIND=I8B)               :: Quotient, Remainder
            CHARACTER(LEN=:), ALLOCATABLE   :: QuotStr, RemStr
            ! execution
            LongVal   = IAND(INT(Val, KIND=I8B), Z'00000000FFFFFFFF')
            Quotient  = SHIFTR(LongVal, 1) / 5
            Remainder = LongVal - Quotient * 10
            QuotStr = ToDecStrSigned(Quotient)
            RemStr  = ToDecStrSigned(Remainder)
            RetStr  = QuotStr // RemStr
        END BLOCK
    END IF

    RETURN

END FUNCTION

!******************************************************************************

FUNCTION U64_ToDecString(Val) RESULT(RetStr)

! PURPOSE OF THIS FUNCTION:
    !! To convert a 64-bit integer treated as an unsigned number into a string.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: Val
    CHARACTER(LEN=:), ALLOCATABLE   :: RetStr

! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

! FLOW:

    IF (Val >= 0) THEN
        RetStr = ToDecStrSigned(Val)
    ELSE
        BLOCK
            INTEGER(KIND=I8B)               :: Quotient, Remainder
            CHARACTER(LEN=:), ALLOCATABLE   :: QuotStr, RemStr
            ! execution
            Quotient  = SHIFTR(Val, 1) / 5
            Remainder = Val - Quotient * 10
            QuotStr = ToDecStrSigned(Quotient)
            RemStr  = ToDecStrSigned(Remainder)
            RetStr  = QuotStr // RemStr
        END BLOCK
    END IF

    RETURN

END FUNCTION

!******************************************************************************

FUNCTION U64_ToHexString(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an usigned 64-bit integer number to a hexadecimal string

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: Number   ! number treated as unsigned one
    CHARACTER(LEN=:), ALLOCATABLE   :: cStr     ! hexadecimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER    :: Shift = 4

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)               :: Quotient, Remainder
    CHARACTER(LEN=:), ALLOCATABLE   :: QuotStr,  RemStr

!** FLOW

    IF (Number >= 0_I8B) THEN
        cStr = ToHexStrSigned(Number)
    ELSE
        Quotient  = SHIFTR(Number, Shift)
        Remainder = Number - SHIFTL(Quotient, Shift)
        QuotStr = ToHexStrSigned(Quotient)
        RemStr  = ToHexStrSigned(Remainder)
        cStr    = QuotStr // RemStr
    END IF

    RETURN

END FUNCTION U64_ToHexString

!******************************************************************************

FUNCTION U32_ToHexString(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an usigned 32-bit integer number to a hexadecimal string

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE   :: cStr     ! hexadecimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER    :: Shift = 4

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)               :: Quotient, Remainder
    CHARACTER(LEN=:), ALLOCATABLE   :: QuotStr,  RemStr

!** FLOW

    IF (Number >= 0) THEN
        cStr = ToHexStrSigned(Number)
    ELSE
        Quotient  = SHIFTR(Number, Shift)
        Remainder = Number - SHIFTL(Quotient, Shift)
        QuotStr = ToHexStrSigned(Quotient)
        RemStr  = ToHexStrSigned(Remainder)
        cStr    = QuotStr // RemStr
    END IF

    RETURN

END FUNCTION U32_ToHexString

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ROUTINES FOR UNSIGNED MULTIPLICATION
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE SUBROUTINE UMul128(X, Y, U128Hi, U128Lo)

!DIR$ ATTRIBUTES INLINE :: UMul128

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute 128-bit result of multiplication of two 64-bit unsigned integers.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: X, Y
    INTEGER(KIND=I8B), INTENT(OUT)  :: U128Hi, U128Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: X_Lo, X_Hi, Y_Lo, Y_Hi
    INTEGER(KIND=I8B)   :: Lo_Lo, Hi_Lo, Cross

!** FLOW

    X_Lo = IAND(X, MaskU32)
    X_Hi = SHIFTR(X, 32)
    Y_Lo = IAND(Y, MaskU32)
    Y_Hi = SHIFTR(Y, 32)
    Lo_Lo = X_Lo*Y_Lo
    Hi_Lo = X_Hi*Y_Lo
    Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, MaskU32) + X_Lo*Y_Hi
    U128Hi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
    U128Lo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, MaskU32))

    RETURN

END SUBROUTINE UMul128

!******************************************************************************

PURE FUNCTION UMul128_Upper64(X, Y) RESULT(U128Hi)

!DIR$ ATTRIBUTES INLINE :: UMul128_Upper64

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute upper 64 bits of multiplication of two 64-bit unsigned integers

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: X, Y
    INTEGER(KIND=I8B)               :: U128Hi

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: X_Lo, X_Hi, Y_Lo, Y_Hi
    INTEGER(KIND=I8B)   :: Hi_Lo, Cross

!** FLOW

    X_Lo = IAND(X, MaskU32)
    X_Hi = SHIFTR(X, 32)
    Y_Lo = IAND(Y, MaskU32)
    Y_Hi = SHIFTR(Y, 32)
    Hi_Lo  = X_Hi*Y_Lo
    Cross  = SHIFTR(X_Lo*Y_Lo, 32) + IAND(Hi_Lo, MaskU32) + X_Lo*Y_Hi
    U128Hi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi

    RETURN

END FUNCTION UMul128_Upper64

!******************************************************************************

PURE SUBROUTINE UMul192_Upper128(X, YHi, YLo, U128Hi, U128Lo)

!DIR$ ATTRIBUTES INLINE :: UMul192_Upper128

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute upper 128 bits of multiplication of a 64-bit unsigned integer and
    ! a 128-bit unsigned integer.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: X, YHi, YLo
    INTEGER(KIND=I8B), INTENT(OUT)  :: U128Hi, U128Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: Add

!** FLOW

    CALL UMul128(X, YHi, U128Hi, U128Lo)
    Add = UMul128_Upper64(X, YLo)
    U128Lo = U128Lo + Add
    IF (U128Lo .ULT. Add) U128Hi = U128Hi + 1_I8B

    RETURN

END SUBROUTINE UMul192_Upper128

!******************************************************************************

PURE FUNCTION UMul96_Upper64(X, Y) RESULT(U128Hi)

!DIR$ ATTRIBUTES INLINE :: UMul96_Upper64

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute upper 64 bits of multiplication of a 32-bit unsigned integer and
    ! a 64-bit unsigned integer.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: X
    INTEGER(KIND=I8B), INTENT(IN)   :: Y
    INTEGER(KIND=I8B)               :: U128Hi

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U128Hi = UMul128_Upper64(SHIFTL(ToUnsignedLong(X), 32), Y)

    RETURN

END FUNCTION UMul96_Upper64

!******************************************************************************

PURE SUBROUTINE UMul192_Lower128(X, YHi, YLo, U128Hi, U128Lo)

!DIR$ ATTRIBUTES INLINE :: UMul192_Lower128

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute lower 128 bits of multiplication of a 64-bit unsigned integer and
    ! a 128-bit unsigned integer.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: X, YHi, YLo
    INTEGER(KIND=I8B), INTENT(OUT)  :: U128Hi, U128Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL UMul128(X, YLo, U128Hi, U128Lo)
    U128Hi = U128Hi + X*YHi

    RETURN

END SUBROUTINE UMul192_Lower128

!******************************************************************************

PURE FUNCTION UMul96_Lower64(X, Y) RESULT(U128Lo)

!DIR$ ATTRIBUTES INLINE :: UMul96_Lower64

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute lower 64 bits of multiplication of a 32-bit unsigned integer and
    ! a 64-bit unsigned integer.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: X
    INTEGER(KIND=I8B), INTENT(IN)   :: Y
    INTEGER(KIND=I8B)               :: U128Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U128Lo = X*Y

    RETURN

END FUNCTION UMul96_Lower64

!******************************************************************************

PURE SUBROUTINE UMul128_N_Add(A, B, C, U128Hi, U128Lo)

!DIR$ ATTRIBUTES INLINE :: UMul128_N_Add

!** PURPOSE OF THIS SUBROUTINE:
    !^ To multiply two 64-bit unsigned integers and add a value (A*B + C), and
    ! return the 128-bit result as U128Hi, U128Lo.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: A, B, C
    INTEGER(KIND=I8B), INTENT(OUT)  :: U128Hi, U128Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: H, L, T

!** FLOW

    ! multiply A and B
    CALL UMul128(A, B, H, L)

    ! add carry
    T = L + C

    ! check whether to add 1 to high bit
    IF (T .ULT. L) THEN
        H = H + 1_I8B
    ELSE
        IF (T .ULT. C) H = H + 1_I8B
    END IF

    U128Hi = H
    U128Lo = T

    RETURN

END SUBROUTINE UMul128_N_Add

!******************************************************************************

PURE FUNCTION UMul128_N_Shift(A, B_Hi, B_Lo, ShrPos) RESULT(ResVal)

!DIR$ ATTRIBUTES INLINE :: UMul128_N_Shift

!** PURPOSE OF THIS SUBROUTINE:
    !^ To multiply two 64-bit unsigned integers, and then shift
    ! the 128-bit result by ShrPos => SHIFTR(A*B, ShrPos).  
    ! Note: ShrPos should be in the range [64, 128].

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: A, B_Hi, B_Lo
    INTEGER(KIND=I4B), INTENT(IN)   :: ShrPos
    INTEGER(KIND=I8B)               :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: R0_Hi, R1_Lo, R1_Hi

!** FLOW

    ! multiply A and B_Lo and return the high bit R0_Hi
    R0_Hi = UMul128_Upper64(A, B_Lo)

    ! multiply A and B_Hi and add R0_Hi to the result
    CALL UMul128_N_Add(A, B_Hi, R0_Hi, R1_Hi, R1_Lo)

    ! shift the result by ShrPos position => SHIFTR(A*B, ShrPos)
    ResVal = IOR(SHIFTL(R1_Hi, (128 - ShrPos)), SHIFTR(R1_Lo, (ShrPos - 64)))

    RETURN

END FUNCTION UMul128_N_Shift

!******************************************************************************

END MODULE ModBase_UIntUtil

!******************************************************************************
