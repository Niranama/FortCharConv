
MODULE Class_Mt64RNG

!** PURPOSE OF THIS MODULE:
    ! This module contains a random number generator class based on the Mersenne Twister
    ! algorithm by Makoto Matsumoto and Takuji Nishimura.

!** REFERENCES:
    ! [1] M. Matsumoto and T. Nishimura.  1998.  Mersenne Twister: A 623-Dimensionally
    !    Equidistributed Uniform Pseudo-Random Number Generator.  ACM Transactions on
    !    Modeling and Computer Simulation, Vol. 8, No. 1, January 1998, pp 3--30
    ! [2] http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt64.html

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_BaseRNG
    USE Class_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Mt64RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! Most significant 33 bits.
    INTEGER(KIND=I8B),    PARAMETER :: UM = INT(Z'FFFFFFFF80000000', KIND=I8B)
    ! Least significant 31 bits.
    INTEGER(KIND=I8B),    PARAMETER :: LM = INT(Z'000000007FFFFFFF', KIND=I8B)
    ! Size of the bytes pool.
    INTEGER(KIND=I4B), PARAMETER :: NN = 312
    ! Period second parameter.
    INTEGER(KIND=I4B), PARAMETER :: MM = 156
    ! X * MATRIX_A for X = {0, 1}.
    INTEGER(KIND=I8B),    PARAMETER :: MAG01(0:1) = [0_I8B, INT(Z'B5026F5AA96619E9', KIND=I8B)]

!** DERIVED TYPE DEFINITIONS
    TYPE, EXTENDS(LongRNG)  :: Mt64RNG
        ! Bytes pool
        INTEGER(KIND=I8B)       :: MT(0:NN-1)
        ! Current index in the bytes pool
        INTEGER(KIND=I4B)    :: MTI
        ! initial seed used to re-initialize the working States
        INTEGER(KIND=I8B)       :: Seed0(0:NN-1)
    CONTAINS
        ! public procedures
        PROCEDURE       :: Initialize   => Mt64RNG_Initialize
        ! deferred procedures
        PROCEDURE       :: NextLongImpl => Mt64RNG_NextLong
        PROCEDURE       :: ReInit       => Mt64RNG_ReInitialize
        PROCEDURE       :: GetName      => Mt64RNG_GetName
    END TYPE Mt64RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES or FUNCTIONS:

SUBROUTINE Mt64RNG_Initialize(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    ! To initialize the 'Mt64RNG' object with optional initial seeds.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt64RNG),              INTENT(INOUT)  :: RNG      ! 'Mt64RNG' object
    INTEGER(KIND=I8B), OPTIONAL, INTENT(IN)     :: Seed(:)  ! seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)    :: InitSeed(1)

! FLOW

    ! set initial seed
    IF (PRESENT(Seed)) THEN
        CALL Fill_State(Seed, RNG%Seed0)
    ELSE
        InitSeed(1) = RNG%InitSeed() + GOLDEN_RATIO_64
        CALL Fill_State(InitSeed, RNG%Seed0)
    END IF
    
    ! set states
    RNG%MT = RNG%Seed0
    
    ! Initial index
    RNG%MTI = NN

    RETURN
    
    CONTAINS

    SUBROUTINE Fill_State(Seed, State)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To fill State based on the given seed.

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)    :: Seed(0:)
        INTEGER(KIND=I8B), INTENT(OUT)   :: State(0:)

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER    :: C1 = INT(Z'369DEA0F31A53F85', KIND=I8B)
        INTEGER(KIND=I8B), PARAMETER    :: C2 = INT(Z'27BB2EE687B0B0FD', KIND=I8B)
        INTEGER(KIND=I8B), PARAMETER    :: C3 = INT(Z'8000000000000000', KIND=I8B)
        
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: I, J, K, SeedSize
        INTEGER(KIND=I8B)   :: MM1

    ! FLOW
    
        CALL Initialize_State(19650218_I8B, State)

        I = 1
        J = 0
        SeedSize = SIZE(Seed)

        DO K = MAX(NN, SeedSize), 1, -1
            MM1 = State(I - 1)
            ! non linear
            State(I) = IEOR(State(I), (IEOR(MM1, SHIFTR(MM1, 62)) * C1)) + Seed(J) + J
            I = I + 1
            J = J + 1
            IF (I >= NN) THEN
                State(0) = State(NN - 1)
                I = 1
            END IF
            IF (J >= SeedSize) THEN
                J = 0
            END IF
        END DO
        DO K = NN-1, 1, -1
            MM1 = State(I - 1)
            ! non linear
            State(I) = IEOR(State(I), (IEOR(MM1, SHIFTR(MM1, 62)) * C2)) - I
            I = I + 1
            IF (I >= NN) THEN
                State(0) = State(NN - 1)
                I = 1
            END IF
        END DO

        ! MSB is 1 assuring non-zero initial array
        State(0) = C3

        RETURN

    END SUBROUTINE Fill_State

!******************************************************************************

    SUBROUTINE Initialize_State(Seed, State)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To fill the State using a defined pseudo-random sequence.

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: Seed
        INTEGER(KIND=I8B), INTENT(OUT)  :: State(0:)

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER    :: CPAR = INT(Z'5851F42D4C957F2D', KIND=I8B)
        
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)       :: MM1
        INTEGER(KIND=I4B)    :: MTI

    ! FLOW
    
        State(0) = Seed
        DO MTI = 1, NN-1
            MM1 = State(MTI - 1)
            State(MTI) = CPAR * IEOR(MM1, SHIFTR(MM1, 62)) + MTI
        END DO

        RETURN

    END SUBROUTINE Initialize_State

!******************************************************************************

END SUBROUTINE Mt64RNG_Initialize

!******************************************************************************

FUNCTION Mt64RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the 32-bit random integer value

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt64RNG), INTENT(INOUT)   :: RNG      ! 'Mt64RNG' object
    INTEGER(KIND=I8B)               :: RandNum  ! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER    :: C1 = INT(Z'5555555555555555', KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER    :: C2 = INT(Z'71d67fffeda60000', KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER    :: C3 = INT(Z'fff7eee000000000', KIND=I8B)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: X
    INTEGER(KIND=I4B)   :: I

! FLOW

    IF (RNG%MTI >= NN) THEN
        
        ! Generate N words at one time
        DO I = 0, NN - MM -1
            X = IOR(IAND(RNG%MT(I), UM), IAND(RNG%MT(I + 1), LM))
            RNG%MT(I) = IEOR(IEOR(RNG%MT(I + MM), SHIFTR(X, 1)), &
                             MAG01(INT(IAND(X, 1_I8B), KIND=I4B)))
        END DO
        DO I = NN - MM, NN - 2
            X = IOR(IAND(RNG%MT(I), UM), IAND(RNG%MT(I + 1), LM))
            RNG%MT(I) = IEOR(IEOR(RNG%MT( I + (MM - NN)), SHIFTR(X, 1)), &
                             MAG01(INT(IAND(X, 1_I8B), KIND=I4B)))
        END DO

        X = IOR(IAND(RNG%MT(NN - 1), UM), IAND(RNG%MT(0), LM))
        RNG%MT(NN - 1) = IEOR(IEOR(RNG%MT(MM - 1), SHIFTR(X, 1)), &
                              MAG01(INT(IAND(X, 1_I8B), KIND=I4B)))

        RNG%MTI = 0
    END IF

    X = RNG%MT(RNG%MTI)
    RNG%MTI = RNG%MTI + 1

    ! Tempering
    X = IEOR(X, IAND(SHIFTR(X, 29), C1))
    X = IEOR(X, IAND(SHIFTL(X, 17), C2))
    X = IEOR(X, IAND(SHIFTL(X, 37), C3))
    X = IEOR(X, SHIFTR(X, 43))

    RandNum = X

    RETURN

END FUNCTION Mt64RNG_NextLong

!******************************************************************************

SUBROUTINE Mt64RNG_ReInitialize(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    ! To re-initialize the 'Mt64RNG' object.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt64RNG), INTENT(INOUT)   :: RNG  ! 'Mt64RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL RNG%Initialize(RNG%Seed0)
    
    RETURN

END SUBROUTINE Mt64RNG_ReInitialize

!******************************************************************************

FUNCTION Mt64RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the name of the generator

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt64RNG),   INTENT(IN)    :: RNG      ! 'Mt64RNG' object
    CHARACTER(LEN=:), ALLOCATABLE   :: Name     ! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Mt64RNG'
    ! to prevent warning of unused variable(s)
    ASSOCIATE (Dummy => RNG); END ASSOCIATE
    
    RETURN

END FUNCTION Mt64RNG_GetName

!******************************************************************************

END MODULE Class_Mt64RNG
    
!******************************************************************************
