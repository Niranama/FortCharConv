
SUBMODULE (Class_BaseRNG) SubClass_Rng_Auxiliary
       
!** PURPOSE OF THIS MODULE:
    ! This submodule contains auxiliary routines to support random number generation.

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES or FUNCTIONS:

MODULE FUNCTION Initialize_Seed64(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a 64-bit integer value that may be useful for initializing a source of
    ! seed values for instances of random number generators created by zero-argument
    ! constructors.  This routine should only be called once per construction.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(IN)  :: RNG
    INTEGER(KIND=I8B)           :: Output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: T(8)
    INTEGER(KIND=I8B)   :: Time1, Time2

! FLOW
    
    ! get times
    CALL SYSTEM_CLOCK(Time1)
    CALL DATE_AND_TIME(VALUES=T)
    Time2 = T(7) + 60*(T(6) + 60*(T(5) + 24*(T(3) - 1 + 31*(T(2) - 1 + 12*T(1))))) + T(8)
    ! get output seed
    Output = IEOR(Mix_Stafford_13(Time1), Mix_Stafford_13(Time2))
    ! to prevent warning of unused variable(s)
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Initialize_Seed64

!******************************************************************************

MODULE FUNCTION Mix_Murmur_64(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the 64-bit mixing function of the MurmurHash3 hash function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: Input
    INTEGER(KIND=I8B)               :: Output

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER    :: C1 = INT(Z'FF51AFD7ED558CCD', KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER    :: C2 = INT(Z'C4CEB9FE1A85EC53', KIND=I8B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = IEOR(Input,  SHIFTR(Input,  33)) * C1
    Output = IEOR(Output, SHIFTR(Output, 33)) * C2
    Output = IEOR(Output, SHIFTR(Output, 33))
    
    RETURN

END FUNCTION Mix_Murmur_64

!******************************************************************************

MODULE FUNCTION Mix_Stafford_13(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute Stafford variant 13 of the 64-bit mixing function of
    ! the MurmurHash3 hash function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: Input
    INTEGER(KIND=I8B)               :: Output

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER    :: C1 = INT(Z'BF58476D1CE4E5B9', KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER    :: C2 = INT(Z'94D049BB133111EB', KIND=I8B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = IEOR(Input,  SHIFTR(Input,  30)) * C1
    Output = IEOR(Output, SHIFTR(Output, 27)) * C2
    Output = IEOR(Output, SHIFTR(Output, 31))
    
    RETURN

END FUNCTION Mix_Stafford_13

!******************************************************************************

MODULE FUNCTION Mix_Lea_64(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute Doug Lea's 64-bit mixing function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: Input
    INTEGER(KIND=I8B)               :: Output

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER    :: C = INT(Z'DABA0B6EB09322E3', KIND=I8B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = IEOR(Input,  SHIFTR(Input,  32)) * C
    Output = IEOR(Output, SHIFTR(Output, 32)) * C
    Output = IEOR(Output, SHIFTR(Output, 32))
    
    RETURN

END FUNCTION Mix_Lea_64

!******************************************************************************

MODULE FUNCTION Mix_Murmur_32(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the 32-bit mixing function of the MurmurHash3 hash function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)    :: Input
    INTEGER(KIND=I4B)                :: Output

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER :: C1 = INT(Z'85EBCA6B', KIND=I4B)
    INTEGER(KIND=I4B), PARAMETER :: C2 = INT(Z'C2B2AE35', KIND=I4B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = IEOR(Input,  SHIFTR(Input,  16)) * C1
    Output = IEOR(Output, SHIFTR(Output, 13)) * C2
    Output = IEOR(Output, SHIFTR(Output, 16))
    
    RETURN

END FUNCTION Mix_Murmur_32

!******************************************************************************

MODULE FUNCTION Mix_Lea_32(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute Doug Lea's 32-bit mixing function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)    :: Input
    INTEGER(KIND=I4B)                :: Output

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER :: C = INT(Z'D36D884B', KIND=I4B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = IEOR(Input,  SHIFTR(Input,  16)) * C
    Output = IEOR(Output, SHIFTR(Output, 16)) * C
    Output = IEOR(Output, SHIFTR(Output, 16))
    
    RETURN

END FUNCTION Mix_Lea_32

!******************************************************************************

MODULE FUNCTION ScrambleWell(Seed, Add) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! To transform the initial state of a generator.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)    :: Seed     ! seed element
    INTEGER(KIND=I4B), INTENT(IN)    :: Add      ! offset
    INTEGER(KIND=I8B)                :: Output   ! the transformed seed element

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER    :: Mult = 1812433253_I8B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! code inspired from "AbstractWell" class
    Output = Mult * IEOR(Seed, SHIFTA(Seed, 30)) + Add
    
    RETURN

END FUNCTION ScrambleWell

!******************************************************************************

MODULE SUBROUTINE Fill_State32(Seed, State)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill state based on the given seed.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)    :: Seed(0:)
    INTEGER(KIND=I4B), INTENT(OUT)   :: State(0:)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER :: Mask = INT(Z'00000000FFFFFFFF', KIND=I8B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: StateSize, SeedSize, MinSize
    INTEGER(KIND=I4B)    :: I

! FLOW
    
    StateSize = SIZE(State)
    SeedSize  = SIZE(Seed)
    MinSize   = MIN(StateSize, SeedSize)
    State(0:MinSize-1) = Seed(0:MinSize-1)
    IF (SeedSize < StateSize) THEN
        DO I = SeedSize, StateSize-1
            State(I) = INT(IAND(ScrambleWell(INT(State(I - SeedSize), KIND=I8B), I), Mask), KIND=I4B)
        END DO
    END IF

    RETURN

END SUBROUTINE Fill_State32

!******************************************************************************

MODULE SUBROUTINE Fill_State64(Seed, State)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill state based on the given seed.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: Seed(0:)
    INTEGER(KIND=I8B), INTENT(OUT)  :: State(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: StateSize, SeedSize, MinSize
    INTEGER(KIND=I4B)    :: I

! FLOW
    
    StateSize = SIZE(State)
    SeedSize  = SIZE(Seed)
    MinSize   = MIN(StateSize, SeedSize)
    State(0:MinSize-1) = Seed(0:MinSize-1)
    IF (SeedSize < StateSize) THEN
        DO I = SeedSize, StateSize-1
            State(I) = ScrambleWell(State(I - SeedSize), I)
        END DO
    END IF

    RETURN

END SUBROUTINE Fill_State64

!******************************************************************************

MODULE SUBROUTINE Extend_Seed32(SeedIn, SeedOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To extend the seed if the length of SeedIn is less than that of SeedOut.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)    :: SeedIn(0:)
    INTEGER(KIND=I4B), INTENT(OUT)   :: SeedOut(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: InSize, OutSize, MinSize
    INTEGER(KIND=I4B)    :: I, X

! FLOW
    
    InSize  = SIZE(SeedIn)
    OutSize = SIZE(SeedOut)
    MinSize = MIN(InSize, OutSize)
    SeedOut(0:MinSize-1) = SeedIn(0:MinSize-1)
    IF (InSize < OutSize) THEN
        X = SeedOut(0)
        DO I = InSize, OutSize-1
            X = X + GOLDEN_RATIO_32
            SeedOut(I) = Mix_Murmur_32(X)
        END DO
    END IF

    RETURN

END SUBROUTINE Extend_Seed32

!******************************************************************************

MODULE SUBROUTINE Extend_Seed64(SeedIn, SeedOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To extend the seed if the length of SeedIn is less than that of SeedOut.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)    :: SeedIn(0:)
    INTEGER(KIND=I8B), INTENT(OUT)   :: SeedOut(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: InSize, OutSize, MinSize
    INTEGER(KIND=I4B)    :: I
    INTEGER(KIND=I8B)       :: X

! FLOW
    
    InSize  = SIZE(SeedIn)
    OutSize = SIZE(SeedOut)
    MinSize = MIN(InSize, OutSize)
    SeedOut(0:MinSize-1) = SeedIn(0:MinSize-1)
    IF (InSize < OutSize) THEN
        X = SeedOut(0)
        DO I = InSize, OutSize-1
            X = X + GOLDEN_RATIO_64
            SeedOut(I) = Mix_Stafford_13(X)
        END DO
    END IF

    RETURN

END SUBROUTINE Extend_Seed64

!******************************************************************************

END SUBMODULE SubClass_Rng_Auxiliary
    
!******************************************************************************
