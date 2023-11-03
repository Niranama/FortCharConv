
MODULE Class_Mt32RNG

!** PURPOSE OF THIS MODULE:
    ! This module contains a random number generator class based on the Mersenne Twister
    ! algorithm by Makoto Matsumoto and Takuji Nishimura.
    ! The generator features an extremely long period (2**19937 - 1) and 623-dimensional
    ! equidistribution up to 32 bits accuracy.

!** REFERENCES:
    ! [1] M. Matsumoto and T. Nishimura.  1998.  Mersenne Twister: A 623-Dimensionally
    !    Equidistributed Uniform Pseudo-Random Number Generator.  ACM Transactions on
    !    Modeling and Computer Simulation, Vol. 8, No. 1, January 1998, pp 3--30

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_BaseRNG
    USE Class_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Mt32RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! Mask 32 most significant bits.
    INTEGER(KIND=I8B), PARAMETER :: INT_MASK_LONG   = INT(Z'00000000FFFFFFFF', KIND=I8B)
    ! Most significant w-r bits.
    INTEGER(KIND=I8B), PARAMETER :: UPPER_MASK_LONG = INT(Z'0000000080000000', KIND=I8B)
    ! Least significant r bits.
    INTEGER(KIND=I8B), PARAMETER :: LOWER_MASK_LONG = INT(Z'000000007FFFFFFF', KIND=I8B)
    ! Most significant w-r bits.
    INTEGER(KIND=I4B), PARAMETER :: UPPER_MASK = INT(Z'80000000', KIND=I4B)
    ! Least significant r bits.
    INTEGER(KIND=I4B), PARAMETER :: LOWER_MASK = INT(Z'7FFFFFFF', KIND=I4B)
    ! Size of the bytes pool.
    INTEGER(KIND=I4B), PARAMETER :: N = 624
    ! Period second parameter.
    INTEGER(KIND=I4B), PARAMETER :: M = 397
    ! X * MATRIX_A for X = {0, 1}.
    INTEGER(KIND=I4B), PARAMETER :: MAG01(0:1) = [0, INT(Z'9908B0DF', KIND=I4B)]

!** DERIVED TYPE DEFINITIONS
    TYPE, EXTENDS(IntegerRNG)  :: Mt32RNG
        ! Bytes pool
        INTEGER(KIND=I4B)    :: MT(0:N-1)
        ! Current index in the bytes pool
        INTEGER(KIND=I4B)    :: MTI
        ! initial seed used to re-initialize the working States
        INTEGER(KIND=I4B)    :: Seed0(0:N-1)
    CONTAINS
        ! public procedures
        PROCEDURE       :: Initialize       => Mt32RNG_Initialize
        ! deferred procedures
        PROCEDURE       :: NextIntegerImpl  => Mt32RNG_NextInteger
        PROCEDURE       :: ReInit           => Mt32RNG_ReInitialize
        PROCEDURE       :: GetName          => Mt32RNG_GetName
    END TYPE Mt32RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES or FUNCTIONS:

SUBROUTINE Mt32RNG_Initialize(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    ! To initialize the 'Mt32RNG' object with optional initial seeds.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt32RNG),              INTENT(INOUT)   :: RNG      ! 'Mt32RNG' object
    INTEGER(KIND=I4B), OPTIONAL, INTENT(IN)      :: Seed(:)  ! seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: InitSeed(1)

! FLOW

    ! set initial seed
    IF (PRESENT(Seed)) THEN
        IF (SIZE(Seed) < N) THEN
            CALL Fill_State(Seed, RNG%Seed0)
        ELSE
            RNG%Seed0(0:N-1) = Seed(1:N)
        END IF
    ELSE
        InitSeed(1) = INT(IAND(RNG%InitSeed(), MaskL), KIND=I4B) + GOLDEN_RATIO_32
        CALL Fill_State(InitSeed, RNG%Seed0)
    END IF
    
    ! set States
    RNG%MT = RNG%Seed0
    
    ! Initial index
    RNG%MTI = N

    RETURN
    
    CONTAINS

    SUBROUTINE Fill_State(Seed, State)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To fill State based on the given seed.

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)    :: Seed(0:)
        INTEGER(KIND=I4B), INTENT(OUT)   :: State(0:)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER :: Mask = INT(Z'00000000FFFFFFFF', KIND=I8B)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)    :: NextIndex

    ! FLOW
    
        CALL Initialize_State(State)

        NextIndex = Mix_Seed_N_State(Seed, State)

        CALL Mix_State(State, NextIndex)

        ! MSB is 1, ensuring non-zero initial array
        State(0) = INT(UPPER_MASK_LONG, KIND=I4B)

        RETURN

    END SUBROUTINE Fill_State

!******************************************************************************

    SUBROUTINE Initialize_State(State)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To fill the State using a defined pseudo-random sequence.

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(OUT)   :: State(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)   :: MT
        INTEGER(KIND=I4B)   :: I

    ! FLOW
    
        MT = IAND(19650218_I8B, INT_MASK_LONG)
        State(0) = INT(MT, KIND=I4B)
        DO I = 1, SIZE(State)-1
            MT = IAND((1812433253_I8B * IEOR(MT, SHIFTA(MT, 30)) + I), INT_MASK_LONG)
            State(I) = INT(MT, KIND=I4B)
        END DO
            
        RETURN

    END SUBROUTINE Initialize_State

!******************************************************************************

    FUNCTION Mix_Seed_N_State(Seed, State) RESULT(NextID)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To mix the seed into the state using a non-linear combination.  The procedure
        ! uses K steps where {K = MAX(SIZE(State), SIZE(Seed))}.  If the seed is smaller
        ! than the state it is wrapped to obtain enough values.  If the seed is larger
        ! than the state then the procedure visits entries in the state multiple times.
        ! Also, to return the index immediately after the most recently visited position
        ! in the state array.

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(INOUT) :: State(0:)
        INTEGER(KIND=I4B), INTENT(IN)    :: Seed(0:)
        INTEGER(KIND=I4B)                :: NextID

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: StateSize, SeedSize, MaxSize
        INTEGER(KIND=I4B)   :: I, J, K
        INTEGER(KIND=I8B)   :: A, B, C

    ! FLOW
    
        StateSize = SIZE(State)
        SeedSize  = SIZE(Seed)
        MaxSize   = MAX(StateSize, SeedSize)
        I = 1
        J = 0
        
        DO K = MaxSize, 1, -1
            IF (State(I) < 0) THEN
                A = IOR(IAND(State(I), LOWER_MASK_LONG), UPPER_MASK_LONG)
            ELSE
                A = IOR(IAND(State(I), LOWER_MASK_LONG), 0_I8B)
            END IF
            IF (State(I-1) < 0) THEN
                B = IOR(IAND(State(I-1), LOWER_MASK_LONG), UPPER_MASK_LONG)
            ELSE
                B = IOR(IAND(State(I-1), LOWER_MASK_LONG), 0_I8B)
            END IF
            ! Non linear
            C = IEOR(A, (IEOR(B, SHIFTA(B, 30)) * 1664525_I8B)) + Seed(J) + J
            State(I) = INT(IAND(C, INT_MASK_LONG), KIND=I4B)
            I = I + 1
            J = J + 1
            IF (I >= StateSize) THEN
                State(0) = State(StateSize - 1)
                I = 1
            END IF
            IF (J >= SeedSize) THEN
                J = 0
            END IF
        END DO
        
        ! return the next index
        NextID = I

        RETURN

    END FUNCTION Mix_Seed_N_State

!******************************************************************************

    SUBROUTINE Mix_State(State, StartID)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To mix each position of the state using a non-linear combination. The
        ! procedure starts from the specified index in the state array and wraps
        ! iteration through the array if required.

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(INOUT) :: State(0:)
        INTEGER(KIND=I4B), INTENT(IN)    :: StartID

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: StateSize
        INTEGER(KIND=I4B)   :: I, K
        INTEGER(KIND=I8B)   :: A, B, C

    ! FLOW
    
        StateSize = SIZE(State)
        I = StartID
        
        DO K = StateSize, 1, -1
            IF (State(I) < 0) THEN
                A = IOR(IAND(State(I), LOWER_MASK_LONG), UPPER_MASK_LONG)
            ELSE
                A = IOR(IAND(State(I), LOWER_MASK_LONG), 0_I8B)
            END IF
            IF (State(I-1) < 0) THEN
                B = IOR(IAND(State(I-1), LOWER_MASK_LONG), UPPER_MASK_LONG)
            ELSE
                B = IOR(IAND(State(I-1), LOWER_MASK_LONG), 0_I8B)
            END IF
            ! Non linear
            C = IEOR(A, (IEOR(B, SHIFTA(B, 30)) * 1566083941_I8B)) - I
            State(I) = INT(IAND(C, INT_MASK_LONG), KIND=I4B)
            I = I + 1
            IF (I >= StateSize) THEN
                State(0) = State(StateSize - 1)
                I = 1
            END IF
        END DO
            
        RETURN

    END SUBROUTINE Mix_State

!******************************************************************************

END SUBROUTINE Mt32RNG_Initialize

!******************************************************************************

FUNCTION Mt32RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the 32-bit random integer value

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt32RNG), INTENT(INOUT)   :: RNG      ! 'Mt32RNG' object
    INTEGER(KIND=I4B)               :: RandNum  ! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Tempering parameters
    INTEGER(KIND=I4B), PARAMETER :: TMaskB = INT(Z'9D2C5680', KIND=I4B)
    INTEGER(KIND=I4B), PARAMETER :: TMaskC = INT(Z'EFC60000', KIND=I4B)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: Y, MTCurr, MTNext, K

! FLOW

    IF (RNG%MTI >= N) THEN
        ! Generate N words at one time
        MTNext = RNG%MT(0)
        DO K = 0, N-M-1
            MTCurr = MTNext
            MTNext = RNG%MT(K + 1)
            Y = IOR(IAND(MTCurr, UPPER_MASK), IAND(MTNext, LOWER_MASK))
            RNG%MT(K) = IEOR(IEOR(RNG%MT(K + M), SHIFTR(Y, 1)), MAG01(IAND(Y, 1)))
        END DO
        DO K = N-M, N-2
            MTCurr = MTNext
            MTNext = RNG%MT(K + 1)
            Y = IOR(IAND(MTCurr, UPPER_MASK), IAND(MTNext, LOWER_MASK))
            RNG%MT(K) = IEOR(IEOR(RNG%MT(K + (M - N)), SHIFTR(Y, 1)), MAG01(IAND(Y, 1)))
        END DO
        Y = IOR(IAND(MTNext, UPPER_MASK), IAND(RNG%MT(0), LOWER_MASK))
        RNG%MT(N - 1) = IEOR(IEOR(RNG%MT(M - 1), SHIFTR(Y, 1)), MAG01(IAND(Y, 1)))

        RNG%MTI = 0
    END IF

    Y = RNG%MT(RNG%MTI)
    RNG%MTI = RNG%MTI + 1

    ! Tempering
    Y = IEOR(Y, SHIFTR(Y, 11))
    Y = IEOR(Y, IAND(SHIFTL(Y, 7), TMaskB))
    Y = IEOR(Y, IAND(SHIFTL(Y, 15), TMaskC))
    Y = IEOR(Y, SHIFTR(Y, 18))

    RandNum = Y

    RETURN

END FUNCTION Mt32RNG_NextInteger

!******************************************************************************

SUBROUTINE Mt32RNG_ReInitialize(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    ! To re-initialize the 'Mt32RNG' object.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt32RNG), INTENT(INOUT)   :: RNG  ! 'Mt32RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL RNG%Initialize(RNG%Seed0)
    
    RETURN

END SUBROUTINE Mt32RNG_ReInitialize

!******************************************************************************

FUNCTION Mt32RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the name of the generator

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt32RNG),   INTENT(IN)    :: RNG      ! 'Mt32RNG' object
    CHARACTER(LEN=:), ALLOCATABLE   :: Name     ! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Mt32RNG'
    ! to prevent warning of unused variable(s)
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Mt32RNG_GetName

!******************************************************************************

END MODULE Class_Mt32RNG
    
!******************************************************************************
