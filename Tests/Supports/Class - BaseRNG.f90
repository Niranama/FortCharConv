
MODULE Class_BaseRNG

!** PURPOSE OF THIS MODULE:
    ! This module contains an abstract class that provides the application interfaces
    ! (APIs) for a random number generator.  All random number generators should extend
    ! from this class or its extended classes (i.e. IntegerRNG and LongRNG).

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE ModBase_Common
    USE ModBase_Error_Handlers
    USE ModBase_UInt128
    USE ModBase_SInt128

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: BaseRNG
    ! parameters
    PUBLIC :: GOLDEN_RATIO_32, GOLDEN_RATIO_64
    PUBLIC :: SILVER_RATIO_32, SILVER_RATIO_64
    PUBLIC :: AlphaOnlyCap,    AlphaOnlyMix
    PUBLIC :: AlphaNumericCap, AlphaNumericMix
    PUBLIC :: DecimalString,   HexadecimalString
    ! procedures
    PUBLIC :: Mix_Stafford_13, Mix_Murmur, Mix_Lea
    PUBLIC :: ScrambleWell,    Fill_State, Extend_Seed

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    CHARACTER(LEN=*),  PARAMETER :: ModName = 'Class_BaseRNG'
    ! The first 32 bits of the golden ratio (1+sqrt(5))/2, forced to be odd.
    ! Useful for producing good Weyl sequences or as an arbitrary nonzero odd
    ! value.
    INTEGER(KIND=I4B), PARAMETER :: GOLDEN_RATIO_32 = INT(Z'9E3779B9', KIND=I4B)
    ! The first 64 bits of the golden ratio (1+sqrt(5))/2, forced to be odd.
    ! Useful for producing good Weyl sequences or as an arbitrary nonzero odd
    ! value.
    INTEGER(KIND=I8B), PARAMETER :: GOLDEN_RATIO_64 = INT(Z'9E3779B97F4A7C15', KIND=I8B)
    ! The first 32 bits of the silver ratio 1+sqrt(2), forced to be odd. Useful
    ! for producing good Weyl sequences or as an arbitrary nonzero odd value.
    INTEGER(KIND=I4B), PARAMETER :: SILVER_RATIO_32 = INT(Z'6A09E667', KIND=I4B)
    ! The first 64 bits of the silver ratio 1+sqrt(2), forced to be odd. Useful
    ! for producing good Weyl sequences or as an arbitrary nonzero odd value.
    INTEGER(KIND=I8B), PARAMETER :: SILVER_RATIO_64 = INT(Z'6A09E667F3BCC909', KIND=I8B)
    ! parameters for character-string type
    INTEGER(KIND=I4B), PARAMETER :: AlphaOnlyCap      = 1 ! upper-case alphabet
    INTEGER(KIND=I4B), PARAMETER :: AlphaOnlyMix      = 2 ! mixed-case alphabet
    INTEGER(KIND=I4B), PARAMETER :: AlphaNumericCap   = 3 ! upper-case alphabet + decimal number
    INTEGER(KIND=I4B), PARAMETER :: AlphaNumericMix   = 4 ! mixed-case alphabet + decimal number
    INTEGER(KIND=I4B), PARAMETER :: DecimalString     = 5 ! decimal (number) string
    INTEGER(KIND=I4B), PARAMETER :: HexadecimalString = 6 ! hexadecimal (number) string
    ! character sets
    CHARACTER(LEN=*),  PARAMETER :: SET_ALPHABETS_LOW = 'abcdefghijklmnopqrstuvwxyz'
    CHARACTER(LEN=*),  PARAMETER :: SET_DEC_DIGITS = '0123456789'
    CHARACTER(LEN=*),  PARAMETER :: SET_HEX_DIGITS = SET_DEC_DIGITS // 'ABCDEF'
    CHARACTER(LEN=*),  PARAMETER :: SET_ALPHABETS_CAP = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(LEN=*),  PARAMETER :: SET_ALPHABETS_MIX = SET_ALPHABETS_LOW // SET_ALPHABETS_CAP
    CHARACTER(LEN=*),  PARAMETER :: SET_ALPHANUM_CAP  = SET_ALPHABETS_CAP // SET_DEC_DIGITS
    CHARACTER(LEN=*),  PARAMETER :: SET_ALPHANUM_MIX  = SET_ALPHABETS_LOW // SET_DEC_DIGITS // SET_ALPHABETS_CAP
    ! huge (maximum) numbers for intrinsic types used to prevent overflow
!    REAL(KIND=SP),     PARAMETER :: Huge_RSP = HUGE(1.0_SP)  ! = 3.4028235E+38
!    REAL(KIND=DP),     PARAMETER :: Huge_RDP = HUGE(1.0_DP)  ! = 1.797693134862316E+308
!    REAL(KIND=QP),     PARAMETER :: Huge_RQP = HUGE(1.0_QP)  ! = 1.189731495357231765085759326628007E+4932

!** DERIVED TYPE DEFINITIONS
    ! Abstract Random Number Generator
	TYPE, ABSTRACT  :: BaseRNG
    CONTAINS
        ! deferred procedures
        PROCEDURE(NextI32),  DEFERRED   :: NextIntegerImpl
        PROCEDURE(NextI64),  DEFERRED   :: NextLongImpl
        PROCEDURE(NextI128), DEFERRED   :: NextI128Impl
        PROCEDURE(NextU128), DEFERRED   :: NextU128Impl
        PROCEDURE(NextR64),  DEFERRED   :: NextDoubleImpl
        PROCEDURE(NextR128), DEFERRED   :: NextQuadImpl
        PROCEDURE(Reset),    DEFERRED   :: ReInit
        PROCEDURE(RNGName),  DEFERRED   :: GetName
        ! public procedures
        PROCEDURE                       :: NextString       => Default_NextString
        PROCEDURE                       :: NextLogical      => Default_NextLogical
        PROCEDURE                       :: NextLogicalArray => Default_NextLogicalArray
        PROCEDURE                       :: NextByte         => Default_NextByte
        PROCEDURE                       :: NextByteArray    => Default_NextByteArray
        PROCEDURE                       :: NextShort        => Default_NextShort
        PROCEDURE                       :: NextShortArray   => Default_NextShortArray
        PROCEDURE                       :: NextLongArray    => Default_NextLongArray
        PROCEDURE                       :: NextI128Array    => Default_NextI128Array
        PROCEDURE                       :: NextU128Array    => Default_NextU128Array
        PROCEDURE                       :: NextIntegerArray => Default_NextIntegerArray
        PROCEDURE                       :: NextSingleArray  => Default_NextSingleArray
        PROCEDURE                       :: NextDoubleArray  => Default_NextDoubleArray
        PROCEDURE                       :: NextQuadArray    => Default_NextQuadArray
        PROCEDURE                       :: NextGaussian     => Default_NextGaussian
        PROCEDURE                       :: NextExponential  => Default_NextExponential
        PROCEDURE                       :: InitSeed         => Initialize_Seed64
        ! private procedures
        PROCEDURE, PRIVATE              :: Default_NextIntegerUpper
        PROCEDURE, PRIVATE              :: Default_NextIntegerBound
        PROCEDURE, PRIVATE              :: Default_NextLongUpper
        PROCEDURE, PRIVATE              :: Default_NextLongBound
        PROCEDURE, PRIVATE              :: Default_NextI128Upper
        PROCEDURE, PRIVATE              :: Default_NextI128Bound
        PROCEDURE, PRIVATE              :: Default_NextU128Upper
        PROCEDURE, PRIVATE              :: Default_NextU128Bound
        PROCEDURE, PRIVATE              :: Default_NextSingle
        PROCEDURE, PRIVATE              :: Default_NextSingleUpper
        PROCEDURE, PRIVATE              :: Default_NextSingleBound
        PROCEDURE, PRIVATE              :: Default_NextDoubleUpper
        PROCEDURE, PRIVATE              :: Default_NextDoubleBound
        PROCEDURE, PRIVATE              :: Default_NextQuadUpper
        PROCEDURE, PRIVATE              :: Default_NextQuadBound
        ! generic procedures
        GENERIC                         :: NextInteger  => NextIntegerImpl,          &
                                                           Default_NextIntegerUpper, &
                                                           Default_NextIntegerBound
        GENERIC                         :: NextLong     => NextLongImpl,             &
                                                           Default_NextLongUpper,    &
                                                           Default_NextLongBound
        GENERIC                         :: NextI128     => NextI128Impl,             &
                                                           Default_NextI128Upper,    &
                                                           Default_NextI128Bound
        GENERIC                         :: NextU128     => NextU128Impl,             &
                                                           Default_NextU128Upper,    &
                                                           Default_NextU128Bound
        GENERIC                         :: NextSingle   => Default_NextSingle,       &
                                                           Default_NextSingleUpper,  &
                                                           Default_NextSingleBound
        GENERIC                         :: NextDouble   => NextDoubleImpl,           &
                                                           Default_NextDoubleUpper,  &
                                                           Default_NextDoubleBound
        GENERIC                         :: NextQuad     => NextQuadImpl,             &
                                                           Default_NextQuadUpper,    &
                                                           Default_NextQuadBound
    END TYPE BaseRNG

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        ! To return a random 32-bit-integer value
        FUNCTION NextI32(RNG) RESULT(RandNum)
            IMPORT
            IMPLICIT NONE
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            INTEGER(KIND=I4B)               :: RandNum
        END FUNCTION
        ! To return a random 64-bit-integer value
        FUNCTION NextI64(RNG) RESULT(RandNum)
            IMPORT
            IMPLICIT NONE
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            INTEGER(KIND=I8B)               :: RandNum
        END FUNCTION
        ! To return a random signed 128-bit-integer value
        FUNCTION NextI128(RNG) RESULT(RandNum)
            IMPORT
            IMPLICIT NONE
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            TYPE(SInt128)                   :: RandNum
        END FUNCTION
        ! To return a random unsigned 128-bit-integer value
        FUNCTION NextU128(RNG) RESULT(RandNum)
            IMPORT
            IMPLICIT NONE
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            TYPE(UInt128)                   :: RandNum
        END FUNCTION
        ! To return a random 64-bit-real value
        FUNCTION NextR64(RNG) RESULT(RandNum)
            IMPORT
            IMPLICIT NONE
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            REAL(KIND=DP)                   :: RandNum
        END FUNCTION
        ! To return a random 128-bit-real value
        FUNCTION NextR128(RNG) RESULT(RandNum)
            IMPORT
            IMPLICIT NONE
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            REAL(KIND=QP)                   :: RandNum
        END FUNCTION
        ! To reset the generator to its initial state
        SUBROUTINE Reset(RNG)
            IMPORT
            IMPLICIT NONE
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
        ! To return the name of the generator
        FUNCTION RNGName(RNG) RESULT(Name)
            IMPORT
            IMPLICIT NONE
            CLASS(BaseRNG),   INTENT(IN)    :: RNG
            CHARACTER(LEN=:), ALLOCATABLE   :: Name
        END FUNCTION
    END INTERFACE
    INTERFACE
        MODULE FUNCTION Default_NextGaussian(RNG) RESULT(RandNum)
            !IMPORT
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            REAL(KIND=DP)                   :: RandNum
        END FUNCTION
        MODULE FUNCTION Default_NextExponential(RNG) RESULT(RandNum)
            !IMPORT
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            REAL(KIND=DP)                   :: RandNum
        END FUNCTION
    END INTERFACE
    INTERFACE
        MODULE FUNCTION Mix_Murmur_64(Input) RESULT(Output)
            !IMPORT
            INTEGER(KIND=I8B), INTENT(IN)   :: Input
            INTEGER(KIND=I8B)               :: Output
        END FUNCTION
        MODULE FUNCTION Mix_Stafford_13(Input) RESULT(Output)
            !IMPORT
            INTEGER(KIND=I8B), INTENT(IN)   :: Input
            INTEGER(KIND=I8B)               :: Output
        END FUNCTION
        MODULE FUNCTION Mix_Lea_64(Input) RESULT(Output)
            !IMPORT
            INTEGER(KIND=I8B), INTENT(IN)   :: Input
            INTEGER(KIND=I8B)               :: Output
        END FUNCTION
        MODULE FUNCTION Mix_Murmur_32(Input) RESULT(Output)
            !IMPORT
            INTEGER(KIND=I4B), INTENT(IN)    :: Input
            INTEGER(KIND=I4B)                :: Output
        END FUNCTION
        MODULE FUNCTION Mix_Lea_32(Input) RESULT(Output)
            !IMPORT
            INTEGER(KIND=I4B), INTENT(IN)    :: Input
            INTEGER(KIND=I4B)                :: Output
        END FUNCTION
        MODULE FUNCTION Initialize_Seed64(RNG) RESULT(Output)
            !IMPORT
            CLASS(BaseRNG), INTENT(IN)  :: RNG
            INTEGER(KIND=I8B)           :: Output
        END FUNCTION
        MODULE FUNCTION ScrambleWell(Seed, Add) RESULT(Output)
            !IMPORT
            INTEGER(KIND=I8B), INTENT(IN)    :: Seed     ! seed element
            INTEGER(KIND=I4B), INTENT(IN)    :: Add      ! offset
            INTEGER(KIND=I8B)                :: Output   ! the transformed seed element
        END FUNCTION
        MODULE SUBROUTINE Fill_State32(Seed, State)
            !IMPORT
            INTEGER(KIND=I4B), INTENT(IN)    :: Seed(0:)
            INTEGER(KIND=I4B), INTENT(OUT)   :: State(0:)
        END SUBROUTINE
        MODULE SUBROUTINE Fill_State64(Seed, State)
            !IMPORT
            INTEGER(KIND=I8B), INTENT(IN)   :: Seed(0:)
            INTEGER(KIND=I8B), INTENT(OUT)  :: State(0:)
        END SUBROUTINE
        MODULE SUBROUTINE Extend_Seed32(SeedIn, SeedOut)
            !IMPORT
            INTEGER(KIND=I4B), INTENT(IN)    :: SeedIn(0:)
            INTEGER(KIND=I4B), INTENT(OUT)   :: SeedOut(0:)
        END SUBROUTINE
        MODULE SUBROUTINE Extend_Seed64(SeedIn, SeedOut)
            !IMPORT
            INTEGER(KIND=I8B), INTENT(IN)   :: SeedIn(0:)
            INTEGER(KIND=I8B), INTENT(OUT)  :: SeedOut(0:)
        END SUBROUTINE
    END INTERFACE

!** GENERIC DECLARATIONS:
    !GENERIC     :: Mix_Murmur   => Mix_Murmur_32, Mix_Murmur_64
    !GENERIC     :: Mix_Lea      => Mix_Lea_32,    Mix_Lea_64
    !GENERIC     :: Fill_State   => Fill_State32,  Fill_State64
    !GENERIC     :: Extend_Seed  => Extend_Seed32, Extend_Seed64
    INTERFACE Mix_Murmur
        MODULE PROCEDURE Mix_Murmur_32
        MODULE PROCEDURE Mix_Murmur_64
    END INTERFACE
    INTERFACE Mix_Lea
        MODULE PROCEDURE Mix_Lea_32
        MODULE PROCEDURE Mix_Lea_64
    END INTERFACE
    INTERFACE Fill_State
        MODULE PROCEDURE Fill_State32
        MODULE PROCEDURE Fill_State64
    END INTERFACE
    INTERFACE Extend_Seed
        MODULE PROCEDURE Extend_Seed32
        MODULE PROCEDURE Extend_Seed64
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES or FUNCTIONS:

FUNCTION Default_NextLogical(RNG) RESULT(RandVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a random 8-bit-logical value.  This default implementation
    ! uses the sign bit from a call to the 'NextInteger' procedure

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    LOGICAL                         :: RandVal  ! random value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandVal = (RNG%NextInteger() < 0)

    RETURN

END FUNCTION Default_NextLogical

!******************************************************************************

SUBROUTINE Default_NextLogicalArray(RNG, BoolArray)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill a user-supplied 8-bit-logical array with generated bool values
    ! based on calls to NextLogical()

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG          ! 'BaseRNG' object
    LOGICAL,        INTENT(OUT)     :: BoolArray(:) ! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(BoolArray)
        BoolArray(I) = RNG%NextLogical()
    END DO

    RETURN

END SUBROUTINE Default_NextLogicalArray

!******************************************************************************

FUNCTION Default_NextByte(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a random 8-bit-integer value.  This default implementation
    ! uses the 8 high-order bits from a call to the 'NextInteger' procedure

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    INTEGER(KIND=I1B)               :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = INT(SHIFTR(RNG%NextInteger(), 24), KIND=I1B)

    RETURN

END FUNCTION Default_NextByte

!******************************************************************************

SUBROUTINE Default_NextByteArray(RNG, ByteArray)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill a user-supplied 8-bit-integer array with generated byte values
    ! pseudorandomly chosen uniformly from the range of values between
    ! -128 (inclusive) and 127 (inclusive).
    ! This default implementation generates random bytes from repeated
    ! calls to the 'NextLong' procedure

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG              ! 'BaseRNG' object
    INTEGER(KIND=I1B),  INTENT(OUT)     :: ByteArray(0:)    ! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)       :: I, ByteLen, WordLen, N
    INTEGER(KIND=I8B)       :: RndLong

! FLOW

    ! initialize
    I = 0_I4B
    ByteLen = SIZE(ByteArray, KIND=I4B)
    WordLen = SHIFTA(ByteLen, 3)

    ! fill the byte array, 8 bytes at a time
    DO WHILE (WordLen > 0)
        WordLen = WordLen - 1
        RndLong = RNG%NextLong()
        N = 8
        DO WHILE (N > 0)
            N = N - 1
            ByteArray(I) = INT(RndLong, KIND=I1B)
            I = I + 1
            RndLong = SHIFTR(RndLong, 8)
        END DO
    END DO

    ! fill the remaining bytes
    IF (I < ByteLen) THEN
        RndLong = RNG%NextLong()
        DO WHILE (I < ByteLen)
            ByteArray(I) = INT(RndLong, KIND=I1B)
            I = I + 1
            RndLong = SHIFTR(RndLong, 8)
        END DO
    END IF

    RETURN

END SUBROUTINE Default_NextByteArray

!******************************************************************************

FUNCTION Default_NextShort(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a random 16-bit-integer value.  This default implementation
    ! uses the 16 high-order bits from a call to the 'NextInteger' procedure

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    INTEGER(KIND=I2B)               :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = INT(SHIFTR(RNG%NextInteger(), 16), KIND=I2B)

    RETURN

END FUNCTION Default_NextShort

!******************************************************************************

SUBROUTINE Default_NextShortArray(RNG, ShortArray)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill a user-supplied 16-bit-integer array with generated short values
    ! pseudorandomly chosen uniformly from the range of values between
    ! -32768 (inclusive) and 32767 (inclusive).
    ! This default implementation generates random shorts from repeated
    ! calls to the 'NextLong' procedure

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG              ! 'BaseRNG' object
    INTEGER(KIND=I2B),  INTENT(OUT)     :: ShortArray(0:)   ! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)       :: I, ShortLen, WordLen, N
    INTEGER(KIND=I8B)       :: RndLong

! FLOW

    ! initialize
    I = 0_I4B
    ShortLen = SIZE(ShortArray, KIND=I4B)
    WordLen  = SHIFTA(ShortLen, 2)

    ! fill the short array, 4 elements at a time
    DO WHILE (WordLen > 0)
        WordLen = WordLen - 1
        RndLong = RNG%NextLong()
        N = 4
        DO WHILE (N > 0)
            N = N - 1
            ShortArray(I) = INT(RndLong, KIND=I2B)
            I = I + 1
            RndLong = SHIFTR(RndLong, 16)
        END DO
    END DO

    ! fill the remaining elements
    IF (I < ShortLen) THEN
        RndLong = RNG%NextLong()
        DO WHILE (I < ShortLen)
            ShortArray(I) = INT(RndLong, KIND=I2B)
            I = I + 1
            RndLong = SHIFTR(RndLong, 16)
        END DO
    END IF

    RETURN

END SUBROUTINE Default_NextShortArray

!******************************************************************************

FUNCTION Default_NextIntegerUpper(RNG, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random 32-bit-integer value in the range between
    ! 0 (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    INTEGER(KIND=I4B),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                    ! must be positive
    INTEGER(KIND=I4B)                   :: RandNum  ! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER    :: POW_32 = SHIFTL(1_I8B, 32)
    INTEGER(KIND=I8B), PARAMETER    :: MaskL  = INT(Z'FFFFFFFFFFFFFFFF', KIND=I8B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)       :: M, L, T

! FLOW

    ! check upper bound value
    IF (Upper <= 0) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextIntegerUpper'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be positive.')
        RandNum = 0
        RETURN
    END IF

    ! Lemire (2019): Fast Random Integer Generation in an Interval
    ! https://arxiv.org/abs/1805.10941
    M = IAND(INT(RNG%NextInteger(), KIND=I8B), MaskL) * Upper
    L = IAND(M, MaskL)
    IF (L < Upper) THEN
        ! 2^32 % N
        T = MOD(POW_32, Upper)
        DO WHILE (L < T)
            M = IAND(INT(RNG%NextInteger(), KIND=I8B), MaskL) * Upper
            L = IAND(M, MaskL)
        END DO
    END IF
    RandNum = INT(SHIFTR(M, 32), KIND=I4B)

    RETURN

END FUNCTION Default_NextIntegerUpper

!******************************************************************************

FUNCTION Default_NextIntegerBound(RNG, Lower, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random 32-bit-integer value in the range between the specified
    ! lower value (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    INTEGER(KIND=I4B),  INTENT(IN)      :: Lower    ! lower bound of the generated value
    INTEGER(KIND=I4B),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                    ! must be greater than the lower bound
    INTEGER(KIND=I4B)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: N

! FLOW

    ! check bound values
    IF (Lower >= Upper) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextIntegerBound'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be greater than the lower bound.')
        RandNum = Lower
        RETURN
    END IF

    N = Upper - Lower
    IF (N > 0) THEN
        RandNum = RNG%NextInteger(N) + Lower
    ELSE
        ! Range too large to fit in a positive integer.
        ! Use simple rejection.
        RandNum = RNG%NextInteger()
        DO WHILE ((RandNum < Lower).OR.(RandNum >= Upper))
            RandNum = RNG%NextInteger()
        END DO
    END IF

    RETURN

END FUNCTION Default_NextIntegerBound

!******************************************************************************

SUBROUTINE Default_NextIntegerArray(RNG, IntegerArray)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill a user-supplied 32-bit-integer array with generated integer values
    ! based on calls to NextInteger()

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG              ! 'BaseRNG' object
    INTEGER(KIND=I4B),  INTENT(OUT)     :: IntegerArray(:)  ! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(IntegerArray)
        IntegerArray(I) = RNG%NextInteger()
    END DO

    RETURN

END SUBROUTINE Default_NextIntegerArray

!******************************************************************************

FUNCTION Default_NextLongUpper(RNG, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random 64-bit-integer value in the range between
    ! 0 (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    INTEGER(KIND=I8B),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                    ! must be positive
    INTEGER(KIND=I8B)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)       :: Bits, Val

! FLOW

    ! check upper bound value
    IF (Upper <= 0_I8B) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextLongUpper'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be positive.')
        RandNum = 0_I8B
        RETURN
    END IF

    DO
        Bits = SHIFTR(RNG%NextLong(), 1)
        Val  = MOD(Bits, Upper)
        IF (Bits - Val + (Upper - 1_I8B) >= 0_I8B) EXIT
    END DO

    RandNum = Val

    RETURN

END FUNCTION Default_NextLongUpper

!******************************************************************************

FUNCTION Default_NextLongBound(RNG, Lower, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random 64-bit-integer value in the range between the specified
    ! lower value (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    INTEGER(KIND=I8B),  INTENT(IN)      :: Lower    ! lower bound of the generated value
    INTEGER(KIND=I8B),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                    ! must be greater than the lower bound
    INTEGER(KIND=I8B)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)    :: N

! FLOW

    ! check bound values
    IF (Lower >= Upper) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextLongBound'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be greater than the lower bound.')
        RandNum = Lower
        RETURN
    END IF

    N = Upper - Lower
    IF (N > 0_I8B) THEN
        RandNum = RNG%NextLong(N) + Lower
    ELSE
        ! Range too large to fit in a positive integer.
        ! Use simple rejection.
        RandNum = RNG%NextLong()
        DO WHILE ((RandNum < Lower).OR.(RandNum >= Upper))
            RandNum = RNG%NextLong()
        END DO
    END IF

    RETURN

END FUNCTION Default_NextLongBound

!******************************************************************************

SUBROUTINE Default_NextLongArray(RNG, LongArray)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill a user-supplied 64-bit-integer array with generated long values
    ! based on calls to NextLong()

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG          ! 'BaseRNG' object
    INTEGER(KIND=I8B),  INTENT(OUT)     :: LongArray(:) ! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(LongArray)
        LongArray(I) = RNG%NextLong()
    END DO

    RETURN

END SUBROUTINE Default_NextLongArray

!******************************************************************************

FUNCTION Default_NextSingle(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a random 32-bit-floating-point value between zero (inclusive)
    ! and one (exclusive).  This default implementation uses the 24 high-order
    ! bits from a call to the 'NextInteger' procedure

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    REAL(KIND=SP)                   :: RandNum  ! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! although these three parameters use different formalas, they are essentially the same.
    REAL(KIND=SP), PARAMETER  :: SNorm1 = 2.0_SP**(-24)
    REAL(KIND=SP), PARAMETER  :: SNorm2 = 1.0_SP/SHIFTL(1, 24)
    REAL(KIND=SP), PARAMETER  :: SNorm3 = 0.5_SP*EPSILON(1.0_SP)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = SHIFTR(RNG%NextInteger(), 8)*SNorm1

    RETURN

END FUNCTION Default_NextSingle

!******************************************************************************

FUNCTION Default_NextSingleUpper(RNG, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random 32-bit-floating-point value in the range between
    ! 0 (inclusive) and the specified upper value (exclusive)

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    REAL(KIND=SP),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                ! must be positive and finite
    REAL(KIND=SP)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check upper bound value
    ! negation of the logic will detect NaN/Inifinity
    IF (.NOT.((Upper > 0.0_SP).AND.(Upper <= Huge_RSP))) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextSingleUpper'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be positive and finite.')
        RandNum = 0.0_SP
        RETURN
    END IF

    RandNum = RNG%NextSingle()*Upper
    IF (RandNum >= Upper) THEN
        ! correct rounding
        RandNum = IEEE_NEXT_DOWN_SP(Upper)
    END IF

    RETURN

END FUNCTION Default_NextSingleUpper

!******************************************************************************

FUNCTION Default_NextSingleBound(RNG, Lower, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random 32-bit-integer value in the range between the specified
    ! lower value (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    REAL(KIND=SP),  INTENT(IN)      :: Lower    ! lower bound of the generated value
    REAL(KIND=SP),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                ! must be greater than the lower bound
    REAL(KIND=SP)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check bound values
    IF ((Lower >= Upper).OR.(.NOT.IEEE_IS_FINITE(Lower)).OR.(.NOT.IEEE_IS_FINITE(Upper))) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextSingleBound'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be greater than the lower bound and both must be finite.')
        RandNum = 0.0_SP
        RETURN
    END IF

    RandNum = RNG%NextSingle()
    ! This expression allows (Upper - Lower) to be infinite
    ! Lower + (Upper - Lower) * RandNum == Lower - Lower * RandNum + Upper * RandNum
    RandNum = (1.0_SP - RandNum) * Lower + RandNum * Upper
    IF (RandNum >= Upper) THEN
        ! correct rounding
        RandNum = IEEE_NEXT_DOWN_SP(Upper)
    END IF

    RETURN

END FUNCTION Default_NextSingleBound

!******************************************************************************

SUBROUTINE Default_NextSingleArray(RNG, SingleArray)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill a user-supplied 32-bit-floating-point array with generated
    ! single values based on calls to NextSingle()

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG              ! 'BaseRNG' object
    REAL(KIND=SP),  INTENT(OUT)     :: SingleArray(:)   ! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(SingleArray)
        SingleArray(I) = RNG%NextSingle()
    END DO

    RETURN

END SUBROUTINE Default_NextSingleArray

!******************************************************************************

FUNCTION Default_NextDoubleUpper(RNG, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random 64-bit-floating-point value in the range between
    ! 0 (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    REAL(KIND=DP),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                ! must be positive and finite
    REAL(KIND=DP)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check upper bound value
    ! negation of the logic will detect NaN/Infinity
    IF (.NOT.((Upper > 0.0_DP).AND.(Upper <= Huge_RDP))) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextDoubleUpper'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be positive and finite.')
        RandNum = 0.0_DP
        RETURN
    END IF

    RandNum = RNG%NextDouble()*Upper
    IF (RandNum >= Upper) THEN
        ! correct rounding
        RandNum = IEEE_NEXT_DOWN_DP(Upper)
    END IF

    RETURN

END FUNCTION Default_NextDoubleUpper

!******************************************************************************

FUNCTION Default_NextDoubleBound(RNG, Lower, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random 64-bit-integer value in the range between the specified
    ! lower value (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    REAL(KIND=DP),  INTENT(IN)      :: Lower    ! lower bound of the generated value
    REAL(KIND=DP),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                ! must be greater than the lower bound
    REAL(KIND=DP)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check bound values
    IF ((Lower >= Upper).OR.(.NOT.IEEE_IS_FINITE(Lower)).OR.(.NOT.IEEE_IS_FINITE(Upper))) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextDoubleBound'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be greater than the lower bound and both must be finite.')
        RandNum = 0.0_DP
        RETURN
    END IF

    RandNum = RNG%NextDouble()
    ! This expression allows (Upper - Lower) to be infinite
    ! Lower + (Upper - Lower) * RandNum == Lower - Lower * RandNum + Upper * RandNum
    RandNum = (1.0_DP - RandNum) * Lower + RandNum * Upper
    IF (RandNum >= Upper) THEN
        ! correct rounding
        RandNum = IEEE_NEXT_DOWN_DP(Upper)
    END IF

    RETURN

END FUNCTION Default_NextDoubleBound

!******************************************************************************

SUBROUTINE Default_NextDoubleArray(RNG, DoubleArray)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill a user-supplied 64-bit-floating-point array with generated
    ! single values based on calls to NextDouble()

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG              ! 'BaseRNG' object
    REAL(KIND=DP),  INTENT(OUT)     :: DoubleArray(:)   ! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(DoubleArray)
        DoubleArray(I) = RNG%NextDouble()
    END DO

    RETURN

END SUBROUTINE Default_NextDoubleArray

!******************************************************************************

FUNCTION Default_NextQuadUpper(RNG, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random 128-bit-floating-point value in the range between
    ! 0 (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    REAL(KIND=QP),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                ! must be positive and finite
    REAL(KIND=QP)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check upper bound value
    ! negation of the logic will detect NaN/Infinity
    IF (.NOT.((Upper > 0.0_QP).AND.(Upper <= Huge_RQP))) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextQuadUpper'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be positive and finite.')
        RandNum = 0.0_QP
        RETURN
    END IF

    RandNum = RNG%NextQuad()*Upper
    IF (RandNum >= Upper) THEN
        ! correct rounding
        RandNum = IEEE_NEXT_DOWN_QP(Upper)
    END IF

    RETURN

END FUNCTION Default_NextQuadUpper

!******************************************************************************

FUNCTION Default_NextQuadBound(RNG, Lower, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random 128-bit-integer value in the range between the specified
    ! lower value (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    REAL(KIND=QP),  INTENT(IN)      :: Lower    ! lower bound of the generated value
    REAL(KIND=QP),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                ! must be greater than the lower bound
    REAL(KIND=QP)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check bound values
    IF ((Lower >= Upper).OR.(.NOT.IEEE_IS_FINITE(Lower)).OR.(.NOT.IEEE_IS_FINITE(Upper))) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextQuadBound'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be greater than the lower bound and both must be finite.')
        RandNum = 0.0_QP
        RETURN
    END IF

    RandNum = RNG%NextQuad()
    ! This expression allows (Upper - Lower) to be infinite
    ! Lower + (Upper - Lower) * RandNum == Lower - Lower * RandNum + Upper * RandNum
    RandNum = (1.0_QP - RandNum) * Lower + RandNum * Upper
    IF (RandNum >= Upper) THEN
        ! correct rounding
        RandNum = IEEE_NEXT_DOWN_QP(Upper)
    END IF

    RETURN
    
END FUNCTION Default_NextQuadBound

!******************************************************************************

SUBROUTINE Default_NextQuadArray(RNG, QuadArray)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill a user-supplied 128-bit-floating-point array with generated
    ! single values based on calls to NextQuad()

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG          ! 'BaseRNG' object
    REAL(KIND=QP),  INTENT(OUT)     :: QuadArray(:) ! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(QuadArray)
        QuadArray(I) = RNG%NextQuad()
    END DO

    RETURN

END SUBROUTINE Default_NextQuadArray

!******************************************************************************

FUNCTION Default_NextI128Upper(RNG, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random signed 128-bit-integer value in the range between
    ! 0 (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    TYPE(SInt128),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                ! must be positive
    TYPE(SInt128)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)   :: Bits, Val

! FLOW

    ! check upper bound value
    IF (Upper <= ZeroI128) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextI128Upper'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be positive.')
        RandNum = ZeroI128
        RETURN
    END IF

    DO
        Bits = SHIFTR(RNG%NextI128(), 1)
        Val  = MOD(Bits, Upper)
        IF (Bits - Val + (Upper - OneI128) >= ZeroI128) EXIT
    END DO

    RandNum = Val

    RETURN

END FUNCTION Default_NextI128Upper

!******************************************************************************

FUNCTION Default_NextI128Bound(RNG, Lower, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
! To generate a random signed 128-bit-integer value in the range between the specified
! lower value (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    TYPE(SInt128),  INTENT(IN)      :: Lower    ! lower bound of the generated value
    TYPE(SInt128),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                ! must be greater than the lower bound
    TYPE(SInt128)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)   :: N

! FLOW

    ! check bound values
    IF (Lower >= Upper) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextI128Bound'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be greater than the lower bound.')
        RandNum = Lower
        RETURN
    END IF

    N = Upper - Lower
    IF (N > ZeroI128) THEN
        RandNum = RNG%NextI128(N) + Lower
    ELSE
        ! Range too large to fit in a positive integer.
        ! Use simple rejection.
        RandNum = RNG%NextI128()
        DO WHILE ((RandNum < Lower).OR.(RandNum >= Upper))
            RandNum = RNG%NextI128()
        END DO
    END IF

    RETURN

END FUNCTION Default_NextI128Bound

!******************************************************************************

SUBROUTINE Default_NextI128Array(RNG, I128Array)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill a user-supplied signed 128-bit-integer array with generated long values
    ! based on calls to NextI128()

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG          ! 'BaseRNG' object
    TYPE(SInt128),  INTENT(OUT)     :: I128Array(:) ! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(I128Array)
        I128Array(I) = RNG%NextI128()
    END DO

    RETURN

END SUBROUTINE Default_NextI128Array

!******************************************************************************

FUNCTION Default_NextU128Upper(RNG, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random unsigned 128-bit-integer value in the range between
    ! 0 (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    TYPE(UInt128),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                ! must be positive
    TYPE(UInt128)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: Bits, Val

! FLOW

    ! check upper bound value
    IF (Upper == ZeroU128) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextU128Upper'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be positive.')
        RandNum = ZeroU128
        RETURN
    END IF

    DO
        Bits = SHIFTR(RNG%NextU128(), 1)
        Val  = UMOD(Bits, Upper)
        IF (Bits - Val + (Upper - OneU128) .UGE. ZeroU128) EXIT
    END DO

    RandNum = Val

    RETURN

END FUNCTION Default_NextU128Upper

!******************************************************************************

FUNCTION Default_NextU128Bound(RNG, Lower, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random unsigned 128-bit-integer value in the range between the specified
    ! lower value (inclusive) and the specified upper value (exclusive)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    TYPE(UInt128),  INTENT(IN)      :: Lower    ! lower bound of the generated value
    TYPE(UInt128),  INTENT(IN)      :: Upper    ! upper bound of the generated value
                                                ! must be greater than the lower bound
    TYPE(UInt128)                   :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check bound values
    IF (Lower .UGE. Upper) THEN
        CALL DisplayWarningError('Message from Routine '//'Default_NextU128Bound'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The upper-bound value must be greater than the lower bound.')
        RandNum = Lower
        RETURN
    END IF

    RandNum = RNG%NextU128(Upper - Lower) + Lower

    RETURN

END FUNCTION Default_NextU128Bound

!******************************************************************************

SUBROUTINE Default_NextU128Array(RNG, U128Array)

!** PURPOSE OF THIS SUBROUTINE:
    ! To fill a user-supplied unsigned 128-bit-integer array with generated long values
    ! based on calls to NextU128()

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG          ! 'BaseRNG' object
    TYPE(UInt128),  INTENT(OUT)     :: U128Array(:) ! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(U128Array)
        U128Array(I) = RNG%NextU128()
    END DO

    RETURN

END SUBROUTINE Default_NextU128Array

!******************************************************************************

FUNCTION Default_NextString(RNG, StrType, StrLen, MaxLen) RESULT(RandStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate a random string (according to the specified optional
    !   input if they are present; otherwise, according default settings)

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),              INTENT(INOUT)   :: RNG      ! 'BaseRNG' object
    INTEGER(KIND=I4B), OPTIONAL, INTENT(IN)      :: StrType  ! type of string (1-6)
    INTEGER(KIND=I4B), OPTIONAL, INTENT(IN)      :: StrLen   ! length of output string
    INTEGER(KIND=I4B), OPTIONAL, INTENT(IN)      :: MaxLen   ! maximum length of output string
                                                             ! if StrLen present, this parameter is ignored
    CHARACTER(LEN=:), ALLOCATABLE                :: RandStr  ! random string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)               :: LenMax, OutLen, I, J
    CHARACTER(LEN=:), ALLOCATABLE   :: CharacterSet
    INTEGER(KIND=I4B)               :: CharSetLen

! FLOW

    LenMax = 0
    ! check input string length whether it is present and valid
    IF (PRESENT(StrLen)) THEN
        IF (StrLen > 0) THEN
            OutLen = StrLen
            LenMax  = StrLen
        END IF
    END IF
    IF (LenMax == 0) THEN
        ! check input maximum length whether it is present and valid
        IF (PRESENT(MaxLen)) THEN
            IF (MaxLen > 0) THEN
                LenMax = MaxLen
            END IF
        END IF
        IF (LenMax == 0) LenMax = 100
        OutLen = RNG%NextInteger(0, LenMax)
    END IF

    ! determine character set
    IF (PRESENT(StrType)) THEN
        SELECT CASE (StrType)
        CASE (AlphaOnlyCap)
            CharacterSet = SET_ALPHABETS_CAP
        CASE (AlphaOnlyMix)
            CharacterSet = SET_ALPHABETS_MIX
        CASE (AlphaNumericCap)
            CharacterSet = SET_ALPHANUM_CAP
        CASE (AlphaNumericMix)
            CharacterSet = SET_ALPHANUM_MIX
        CASE (DecimalString)
            CharacterSet = SET_DEC_DIGITS
        CASE (HexadecimalString)
            CharacterSet = SET_HEX_DIGITS
        CASE DEFAULT
            CharacterSet = SET_ALPHANUM_MIX
        END SELECT
    ELSE
        CharacterSet = SET_ALPHANUM_MIX
    END IF
    ! determine length of the character set
    CharSetLen = LEN(CharacterSet)

    ! generate random string
    ALLOCATE(CHARACTER(LEN=OutLen) :: RandStr)
    DO I = 1, OutLen
        J = RNG%NextInteger(1, CharSetLen)
        RandStr(I:I) = CharacterSet(J:J)
    END DO

    RETURN

END FUNCTION Default_NextString

!******************************************************************************

FUNCTION IEEE_NEXT_DOWN_SP(X) RESULT(XNext)

!** PURPOSE OF THIS SUBROUTINE:
    ! To emulate IEEE_NEXT_DOWN intrinsic function since GFortran does not have one.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=SP), INTENT(IN)   :: X
    REAL(KIND=SP)               :: XNext

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    REAL(KIND=SP)       :: RVal
    INTEGER(KIND=I4B)   :: IVal
    EQUIVALENCE(IVal, RVal)

! FLOW

    ! get input
    RVal = X
    ! get next down
    IF (IVal > 0) IVal = IVal - 1
    ! set output
    XNext = RVal
    
    RETURN

END FUNCTION

!******************************************************************************

FUNCTION IEEE_NEXT_DOWN_DP(X) RESULT(XNext)

!** PURPOSE OF THIS SUBROUTINE:
    ! To emulate IEEE_NEXT_DOWN intrinsic function since GFortran does not have one.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=DP), INTENT(IN)   :: X
    REAL(KIND=DP)               :: XNext

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    REAL(KIND=DP)       :: RVal
    INTEGER(KIND=I8B)   :: IVal
    EQUIVALENCE(IVal, RVal)

! FLOW

    ! get input
    RVal = X
    ! get next down
    IF (IVal > 0) IVal = IVal - 1
    ! set output
    XNext = RVal
    
    RETURN

END FUNCTION

!******************************************************************************

FUNCTION IEEE_NEXT_DOWN_QP(X) RESULT(XNext)

!** PURPOSE OF THIS SUBROUTINE:
    ! To emulate IEEE_NEXT_DOWN intrinsic function since GFortran does not have one.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=QP), INTENT(IN)   :: X
    REAL(KIND=QP)               :: XNext

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    REAL(KIND=QP)       :: RVal
    INTEGER(KIND=I8B)   :: IVal(2)
    EQUIVALENCE(IVal, RVal)

! FLOW

    ! get input
    RVal = X
    ! assuming little-endian order; get next down
    ! (if big-endian order just replace IVal(2) by IVal(1) in the following statement)
    IF (IVal(2) > 0) IVal(2) = IVal(2) - 1
    ! set output
    XNext = RVal
    
    RETURN

END FUNCTION

!******************************************************************************

END MODULE Class_BaseRNG

!******************************************************************************
