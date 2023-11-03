
MODULE Class_LongRNG
       
!** PURPOSE OF THIS MODULE:
    ! This module contains an abstract class that provides the application interfaces
    ! (APIs) for a "long" (64-bit) random number generator.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_SInt128
    USE ModBase_UInt128
    USE Class_BaseRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: LongRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! Abstract Long Random Number Generator
	TYPE, ABSTRACT, EXTENDS(BaseRNG)  :: LongRNG
    CONTAINS
        ! deferred procedures
        PROCEDURE       :: NextIntegerImpl  => Default_NextInteger
        PROCEDURE       :: NextI128Impl     => Default_NextI128
        PROCEDURE       :: NextU128Impl     => Default_NextU128
        PROCEDURE       :: NextDoubleImpl   => Default_NextDouble
        PROCEDURE       :: NextQuadImpl     => Default_NextQuad
    END TYPE LongRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES or FUNCTIONS:

FUNCTION Default_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a random 32-bit-integer value.  This default implementation
    ! uses the 32 high-order bits from a call to the 'NextLong' procedure

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      ! 'LongRNG' object
    INTEGER(KIND=I4B)               :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RandNum = INT(SHIFTR(RNG%NextLong(), 32), KIND=I4B)
    
    RETURN

END FUNCTION Default_NextInteger

!******************************************************************************

FUNCTION Default_NextI128(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the signed 128-bit random integer value

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      ! 'LongRNG' object
    TYPE(SInt128)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! construct SInt128 object from two 64-bit random numbers
    RandNum = SInt128(RNG%NextLong(), RNG%NextLong())
    
    RETURN

END FUNCTION Default_NextI128

!******************************************************************************

FUNCTION Default_NextU128(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the unsigned 128-bit random integer value

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      ! 'LongRNG' object
    TYPE(UInt128)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! construct UInt128 object from two 64-bit random numbers
    RandNum = UInt128(RNG%NextLong(), RNG%NextLong())
    
    RETURN

END FUNCTION Default_NextU128

!******************************************************************************

FUNCTION Default_NextDouble(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a random 64-bit-floating-point value between zero (inclusive)
    ! and one (exclusive).  This default implementation uses the 53 high-order
    ! bits from a call to the 'NextLong' procedure

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      ! 'LongRNG' object
    REAL(KIND=DP)                         :: RandNum  ! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! although these three parameters use different formalas, they are essentially the same.
    REAL(KIND=DP), PARAMETER  :: DNorm1 = 2.0_DP**(-53)
    REAL(KIND=DP), PARAMETER  :: DNorm2 = 1.0_DP/SHIFTL(1_I8B, 53)
    REAL(KIND=DP), PARAMETER  :: DNorm3 = 0.5_DP*EPSILON(1.0_DP)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! use the most 53 significant bits
    RandNum = SHIFTR(RNG%NextLong(), 11)*DNorm1
    
    RETURN

END FUNCTION Default_NextDouble

!******************************************************************************

FUNCTION Default_NextQuad_Alternative(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a random 128-bit-floating-point value between zero (inclusive)
    ! and one (exclusive).

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      ! 'LongRNG' object
    REAL(KIND=QP)                   :: RandNum  ! random number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=QP), PARAMETER    :: Multiplier = 1.0Q+4932/6.73Q0
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    REAL(KIND=QP)           :: QuadVal
    INTEGER(KIND=I8B)       :: LongVal(2)
    EQUIVALENCE(QuadVal, LongVal)
    INTEGER(KIND=I8B)       :: High, Low

! FLOW
    
    ! get two long values
    High = RNG%NextLong()
    Low  = RNG%NextLong()
    
    ! join the most significant 57 bits of Low and 56 bits of High
    LongVal = 0_I8B
    CALL MVBITS(Low,   7, 57, LongVal(1),  0)
    CALL MVBITS(High,  8,  7, LongVal(1), 57)
    CALL MVBITS(High, 15, 49, LongVal(2),  0)
    
    ! set output
    RandNum = QuadVal*Multiplier
    
    RETURN

END FUNCTION Default_NextQuad_Alternative

!******************************************************************************

FUNCTION Default_NextQuad(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a random 128-bit-floating-point value between zero (inclusive)
    ! and one (exclusive).

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      ! 'LongRNG' object
    REAL(KIND=QP)                   :: RandNum  ! random number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! although these three parameters use different formalas, they are essentially the same.
    REAL(KIND=QP), PARAMETER    :: QNorm1 = 2.0_QP**(-113)
!    REAL(KIND=QP), PARAMETER    :: QNorm2 = 1.0_QP/ToR128(SHIFTL(OneI128, 113))  ! can't be parameter
    REAL(KIND=QP), PARAMETER    :: QNorm3 = 0.5_QP*EPSILON(1.0_QP)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)   :: High, Low

! FLOW
    
    ! get two long values
    High = SHIFTR(RNG%NextLong(), 8)    ! 56-bits remain
    Low  = SHIFTR(RNG%NextLong(), 7)    ! 57-bits remain
    High = SHIFTL(High, 57)
    
    ! join the most significant 57 bits of Low and 56 bits of High
    RandNum = ToR128(IOR(High, Low))*QNorm1

    RETURN

END FUNCTION Default_NextQuad

!******************************************************************************

END MODULE Class_LongRNG
    
!******************************************************************************
