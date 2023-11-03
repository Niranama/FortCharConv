
MODULE Class_IntegerRNG
       
!** PURPOSE OF THIS MODULE:
    ! This module contains an abstract class that provides the application interfaces
    ! (APIs) for an "integer" (32-bit) random number generator.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_SInt128
    USE ModBase_UInt128
    USE Class_BaseRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: IntegerRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    INTEGER(KIND=I8B), PARAMETER, PUBLIC    :: MaskL = INT(Z'00000000FFFFFFFF', KIND=I8B)

!** DERIVED TYPE DEFINITIONS
    ! Abstract Integer Random Number Generator
	TYPE, ABSTRACT, EXTENDS(BaseRNG)  :: IntegerRNG
    CONTAINS
        ! deferred procedures implemented
        PROCEDURE       :: NextLongImpl     => Default_NextLong
        PROCEDURE       :: NextI128Impl     => Default_NextI128
        PROCEDURE       :: NextU128Impl     => Default_NextU128
        PROCEDURE       :: NextDoubleImpl   => Default_NextDouble
        PROCEDURE       :: NextQuadImpl     => Default_NextQuad
    END TYPE IntegerRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES or FUNCTIONS:

FUNCTION Default_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the 64-bit random integer value

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      ! 'IntegerRNG' object
    INTEGER(KIND=I8B)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RandNum = IOR(SHIFTL(INT(RNG%NextInteger(), KIND=I8B), 32), IAND(INT(RNG%NextInteger(), KIND=I8B), MaskL))
    
    RETURN

END FUNCTION Default_NextLong

!******************************************************************************

FUNCTION Default_NextI128(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the signed 128-bit random integer value

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      ! 'IntegerRNG' object
    TYPE(SInt128)                       :: RandNum  ! random number

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
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      ! 'IntegerRNG' object
    TYPE(UInt128)                       :: RandNum  ! random number

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
    ! and one (exclusive).

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      ! 'IntegerRNG' object
    REAL(KIND=DP)                             :: RandNum  ! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! although these three parameters use different formalas, they are essentially the same.
    REAL(KIND=DP), PARAMETER  :: DNorm1 = 2.0_DP**(-53)
    REAL(KIND=DP), PARAMETER  :: DNorm2 = 1.0_DP/SHIFTL(1_I8B, 53)
    REAL(KIND=DP), PARAMETER  :: DNorm3 = 0.5_DP*EPSILON(1.0_DP)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)       :: High, Low

! FLOW
    
    ! Require the least significant 53-bits from a long.
    ! Join the most significant 26 from the first random integer
    ! with 27 from the second random integer.
    High = SHIFTL(INT(SHIFTR(RNG%NextInteger(), 6), KIND=I8B), 27)     ! 26-bits remain
    Low  = INT(SHIFTR(RNG%NextInteger(), 5), KIND=I8B)                 ! 27-bits remain
    RandNum = IOR(High, Low)*DNorm1
    
    RETURN

END FUNCTION Default_NextDouble

!******************************************************************************

FUNCTION Default_NextQuad_Alternative(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a random 128-bit-floating-point value between zero (inclusive)
    ! and one (exclusive).

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      ! 'IntegerRNG' object
    REAL(KIND=QP)                       :: RandNum  ! random number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=QP), PARAMETER    :: Multiplier = 1.0Q+4932/6.73Q0
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    REAL(KIND=QP)        :: QuadVal
    INTEGER(KIND=I4B)    :: IntVal(4)
    EQUIVALENCE(QuadVal, IntVal)
    INTEGER(KIND=I4B)    :: Hi1, Hi2, Lo1, Lo2

! FLOW
    
    ! get four integer values
    Hi1 = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    Hi2 = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    Lo1 = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    Lo2 = SHIFTR(RNG%NextInteger(), 3)  ! use the most significant 29 bits
    
    ! join the most significant 57 bits of Low and 56 bits of High
    IntVal = 0
    CALL MVBITS(Lo2,  0, 29, IntVal(1),  0)
    CALL MVBITS(Lo1,  0,  3, IntVal(1), 29)
    CALL MVBITS(Lo1,  3, 25, IntVal(2),  0)
    CALL MVBITS(Hi2,  0,  7, IntVal(2), 25)
    CALL MVBITS(Hi2,  7, 21, IntVal(3),  0)
    CALL MVBITS(Hi1,  0, 11, IntVal(3), 21)
    CALL MVBITS(Hi1, 11, 17, IntVal(4),  0)
    
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
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      ! 'IntegerRNG' object
    REAL(KIND=QP)                               :: RandNum  ! random number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! although these three parameters use different formalas, they are essentially the same.
    REAL(KIND=QP), PARAMETER    :: QNorm1 = 2.0_QP**(-113)
!    REAL(KIND=QP), PARAMETER    :: QNorm2 = 1.0_QP/ToR128(SHIFTL(OneI128, 113))  ! can't be parameter
    REAL(KIND=QP), PARAMETER    :: QNorm3 = 0.5_QP*EPSILON(1.0_QP)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)   :: HiHi, HiLo, LoHi, LoLo

! FLOW
    
    ! get four integer values
    HiHi = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    HiLo = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    LoHi = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    LoLo = SHIFTR(RNG%NextInteger(), 3)  ! use the most significant 29 bits
    LoHi = SHIFTL(LoHi, 29)
    HiLo = SHIFTL(HiLo, 57)
    HiHi = SHIFTL(HiHi, 85)
    
    ! join the most significant 57 bits of Low and 56 bits of High
    RandNum = ToR128(IOR(IOR(HiHi, HiLo), IOR(LoHi, LoLo)))*QNorm1
    
    RETURN

END FUNCTION Default_NextQuad

!******************************************************************************

END MODULE Class_IntegerRNG
    
!******************************************************************************
