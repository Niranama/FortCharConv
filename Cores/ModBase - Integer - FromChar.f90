
MODULE ModBase_Integer_FromChar

!** PURPOSE OF THIS MODULE:
    ! This module contains routines that convert a decimal string into an integer value.

!** REFERENCES:
    ! [1] atoi_yy: https://github.com/ibireme/c_numconv_benchmark
    ! [2] atoi_lemire: https://github.com/ibireme/c_numconv_benchmark

!** TECHNICAL NOTES:
    ! 1. A Fortran number (FortNum) that has the form as: [S]N[N...] where
    !       S is a sign indicator (required if negative '-', optional if positive '+').
    !       N is a decimal digit (0 through 9). Any leading zeros, leading and trailing
    !           spaces are ignored.
    !    Unlike Fortran constants, the optional kind parameter (_k) is not allowed here.
    ! 2. A FPlus number (FortPlus) has a slightly more relaxed rule than that of a Fortran
    !    number such that any invalid characters after characters that are valid are ignored.
    !    For example, -3567e23 is treated as a valid number with a value of -3567.
    ! 3. A JSON number (JsonNum) has a slightly stricter rule than that of a Fortran number
    !    such that a plus sign and leading zoroes are not allowed.

!** USE STATEMENTS:
    USE ModBase_Common
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! 32-bit integer
    PUBLIC :: I32_FromChar_CC_FortNum,  I32_FromChar_CC_FortPlus
    PUBLIC :: I32_FromChar_CC_JsonNum,  I32_FromChar_Lemire_FortPlus
    PUBLIC :: I32_FromChar_YY_JsonNum
    ! 64-bit integer
    PUBLIC :: I64_FromChar_CC_FortNum,  I64_FromChar_CC_FortPlus
    PUBLIC :: I64_FromChar_CC_JsonNum,  I64_FromChar_Lemire_FortPlus
    PUBLIC :: I64_FromChar_YY_JsonNum

    PRIVATE          ! by default, hide all data and routines except those declared explicitly
    
!** MODULE PARAMETERS:
    INTEGER(KIND=I4B), PARAMETER  :: MinI32      = INT(Z'80000000', KIND=I4B)           ! -2,147,483,648
    INTEGER(KIND=I4B), PARAMETER  :: MaxI32      = INT(Z'7FFFFFFF', KIND=I4B)           !  2,147,483,647
    INTEGER(KIND=I8B), PARAMETER  :: MinI64      = INT(Z'8000000000000000', KIND=I8B)   ! -9,223,372,036,854,775,808
    INTEGER(KIND=I8B), PARAMETER  :: MaxI64      = INT(Z'7FFFFFFFFFFFFFFF', KIND=I8B)   !  9,223,372,036,854,775,807
    INTEGER(KIND=I4B), PARAMETER  :: MaxI32Div10 = MaxI32/10                            ! = 214,748,364
    INTEGER(KIND=I4B), PARAMETER  :: MaxI32Mod10 = MOD(MaxI32, 10)                      ! = 7
    INTEGER(KIND=I8B), PARAMETER  :: MaxI64Div10 = MaxI64/10_I8B                        ! = 922,337,203,685,477,580
    INTEGER(KIND=I8B), PARAMETER  :: MaxI64Mod10 = MOD(MaxI64, 10_I8B)                  ! = 7
    INTEGER(KIND=I4B), PARAMETER  :: MaxDigitI32 = 10
    INTEGER(KIND=I4B), PARAMETER  :: MaxDigitI64 = 19
    INTEGER(KIND=I4B), PARAMETER  :: A0          = IACHAR('0')
    INTEGER(KIND=I4B), PARAMETER  :: A4          = IACHAR('4')
    INTEGER(KIND=I4B), PARAMETER  :: A9          = IACHAR('9')
    INTEGER(KIND=I4B), PARAMETER  :: IBase       = 10
    INTEGER(KIND=I8B), PARAMETER  :: LBase       = 10_I8B

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!------------------------------------------------------------------------------
!
!                           32-BIT INTEGER ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION I32_FromChar_CC_FortNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL(KIND=4),               OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    INTEGER(KIND=I4B)                                       :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)           :: Indx, StrLen
    INTEGER(KIND=I4B)           :: Sign
    INTEGER(KIND=I4B)           :: NumDigit
    INTEGER(KIND=I4B)           :: IStart
    CHARACTER(LEN=1), POINTER   :: CurChr
    LOGICAL(KIND=4)             :: Overflow

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
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for leading zero(s)
    Number = 0
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
            Number = 0
            RETURN
        END IF
    END IF
    
    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI32
                RETURN
            END IF
        END DO
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI32
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI32) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI32) THEN
        ! value might be in the applicable range
        IF (Number < 0) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1).AND.(Number == MinI32)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1
            END IF
        ELSE
            IF (IACHAR(cStr(IStart:IStart)) < A4) THEN
                ! no overflow
                Overflow = FalseVal
            ELSE
                ! overflow
                Overflow = TrueVal
            END IF
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI32
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I32_FromChar_CC_FortNum

!******************************************************************************

FUNCTION I32_FromChar_CC_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL(KIND=4),               OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    INTEGER(KIND=I4B)                                       :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)           :: Indx, StrLen
    INTEGER(KIND=I4B)           :: Sign
    INTEGER(KIND=I4B)           :: NumDigit
    INTEGER(KIND=I4B)           :: IStart
    CHARACTER(LEN=1), POINTER   :: CurChr
    LOGICAL(KIND=4)             :: Overflow

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
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for leading zero(s)
    Number = 0
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
            Number = 0
            RETURN
        END IF
    END IF
    
    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
        END DO
        NumDigit = Indx - IStart
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI32) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI32) THEN
        ! value might be in the applicable range
        IF (Number < 0) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1).AND.(Number == MinI32)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1
            END IF
        ELSE
            IF (IACHAR(cStr(IStart:IStart)) < A4) THEN
                ! no overflow
                Overflow = FalseVal
            ELSE
                ! overflow
                Overflow = TrueVal
            END IF
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI32
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I32_FromChar_CC_FortPlus

!******************************************************************************

FUNCTION I32_FromChar_CC_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL(KIND=4),               OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    INTEGER(KIND=I4B)                                       :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)           :: Indx, StrLen
    INTEGER(KIND=I4B)           :: Sign
    INTEGER(KIND=I4B)           :: NumDigit
    INTEGER(KIND=I4B)           :: IStart
    CHARACTER(LEN=1), POINTER   :: CurChr
    LOGICAL(KIND=4)             :: Overflow

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
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1
    IF (cStr(Indx:Indx) == '-') THEN
        Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
    ELSEIF (cStr(Indx:Indx) == '+') THEN
        ! < for JSON number, a plus sign is not allowed. >
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a plus sign is not allowed.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI32
        RETURN
    END IF

    ! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
        IF (Indx == StrLen) THEN
            Number = 0
        ELSE
            CurChr => cStr(Indx+1:Indx+1)
            IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: leading zero(s) is not allowed.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI32
            ELSE
                Number = 0
            END IF
        END IF
        RETURN
    END IF
    
    ! compute value of the input string
    Number   = 0
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
        END DO
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: the first character is not a digit.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI32
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI32) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI32) THEN
        ! value might be in the applicable range
        IF (Number < 0) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1).AND.(Number == MinI32)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1
            END IF
        ELSE
            IF (IACHAR(cStr(IStart:IStart)) < A4) THEN
                ! no overflow
                Overflow = FalseVal
            ELSE
                ! overflow
                Overflow = TrueVal
            END IF
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI32
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I32_FromChar_CC_JsonNum

!******************************************************************************

FUNCTION I32_FromChar_Lemire_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL(KIND=4),               OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    INTEGER(KIND=I4B)                                       :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)           :: Indx, IndxP7
    INTEGER(KIND=I4B)           :: Sign, StrLen
    INTEGER(KIND=I4B)           :: IStart, NumDigit
    CHARACTER(LEN=1), POINTER   :: CurChr
    CHARACTER(LEN=8)            :: wStr
    INTEGER(KIND=I8B)           :: wVal
    EQUIVALENCE(wStr, wVal)
    LOGICAL(KIND=4)             :: Overflow

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
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for leading zero(s)
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
            Number = 0
            RETURN
        END IF
    END IF
    
    ! initialize
    Number = 0
    IStart   = 0
    NumDigit = 0
    
    ! process 8 digits immediately if possible
    IndxP7 = Indx + 7
    IF (IndxP7 <= StrLen) THEN
        wStr = cStr(Indx:IndxP7)
        IF (Is_Made_Of_Eight_Digits(WVal)) THEN
            ! process 8 digits at once
            Number = INT(Parse_Eight_Digits_Unrolled(wVal), KIND=I4B)
            IStart = Indx
            NumDigit = 8
            Indx = Indx + 8
        END IF
    END IF
    
    ! process the remaining digits
    IF (Indx <= StrLen) THEN
        CurChr => cStr(Indx:Indx)
        IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
            IF (IStart == 0) IStart = Indx
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                Number = Number*IBase + (IACHAR(CurChr)-A0)
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
            END DO
            NumDigit = Indx - IStart
        END IF
    END IF
    
    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI32) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI32) THEN
        ! value might be in the applicable range
        IF (Number < 0) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1).AND.(Number == MinI32)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1
            END IF
        ELSE
            IF (IACHAR(cStr(IStart:IStart)) < A4) THEN
                ! no overflow
                Overflow = FalseVal
            ELSE
                ! overflow
                Overflow = TrueVal
            END IF
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI32
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I32_FromChar_Lemire_FortPlus

!******************************************************************************

FUNCTION I32_FromChar_YY_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL(KIND=4),               OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    INTEGER(KIND=I4B)                                       :: Number   ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Digit: '0'
    INTEGER(KIND=I1B), PARAMETER   :: DIGI_TYPE_ZERO    = SHIFTL(1, 0)     ! 1 = Z'01'
    ! Digit: [1-9]
    INTEGER(KIND=I1B), PARAMETER   :: DIGI_TYPE_NONZERO = SHIFTL(1, 1)     ! 2 = Z'02'
    ! Digit type table (generate with misc/make_tables.c)
    INTEGER(KIND=I1B), PARAMETER   :: DigitTable(0:127) = [                    &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'04', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'08', KIND=I1B), INT(Z'10', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'01', KIND=I1B), INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), &
        INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), &
        INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B)]
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)       :: IVal, Add
    INTEGER(KIND=I4B)       :: Indx, StrLen, I
    INTEGER(KIND=I4B)       :: Sign, SignBit
    INTEGER(KIND=I4B)       :: CurCode      ! ASCII code of current character
    LOGICAL(KIND=4)         :: Digit_End, Overflow

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
            Number = MinI32
            RETURN
        END IF
    END IF

    ! check for sign
    Sign  = 1
    IF (cStr(Indx:Indx) == '-') THEN
        Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
    ELSEIF (cStr(Indx:Indx) == '+') THEN
        ! < for JSON number, a plus sign is not allowed. >
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a plus sign is not allowed.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI32
        RETURN
    END IF
    
    ! check whether first digit is zero
    ! < for JSON number, the first digit being zero is not allowed. >
    CurCode = IACHAR(cStr(Indx:Indx))
    IF (.NOT.(IAND(DigitTable(CurCode), DIGI_TYPE_NONZERO) /= 0)) THEN
        IF (CurCode == A0) THEN
            IF (Indx+1 > StrLen) THEN
                Number = 0
            ELSE
                IF (.NOT.(IAND(DigitTable(IACHAR(cStr(Indx+1:Indx+1))), IOR(DIGI_TYPE_ZERO, DIGI_TYPE_NONZERO)) /= 0)) THEN
                    Number = 0
                ELSE
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: leading zero(s) is not allowed.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI32
                END IF
            END IF
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: the first character is not a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
        END IF
        RETURN
    END IF
    
    ! compute IVal for the next 8 characters (digits)
    IVal = CurCode - A0
    I = 1
!DIR$ UNROLL = 8
    DO
        IF (Indx+I <= StrLen) THEN
            CurCode = IACHAR(cStr(Indx+I:Indx+I))
            IF ((IAND(DigitTable(CurCode), IOR(DIGI_TYPE_ZERO, DIGI_TYPE_NONZERO)) /= 0)) THEN
                IVal = IVal*LBase + (CurCode-A0)
            ELSE
                Digit_End = TrueVal
                EXIT
            END IF
        ELSE
            Digit_End = TrueVal
            EXIT
        END IF
        I = I + 1
        IF (I > 8) THEN
            Digit_End = FalseVal
            EXIT
        END IF
    END DO
    
    IF ((Digit_End).OR.(Indx + I > StrLen)) THEN
        Number = INT(IVal*Sign, KIND=I4B)
        RETURN
    END IF

    ! deal with more digit(s)
    Indx = Indx + I
    CurCode = IACHAR(cStr(Indx:Indx))
    IF ((IAND(DigitTable(CurCode), IOR(DIGI_TYPE_ZERO, DIGI_TYPE_NONZERO)) /= 0)) THEN
        ! must check overflow
        Add = CurCode - A0
        ! check overflow
        IF (IVal < MaxI32Div10) THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                IF ((IAND(DigitTable(IACHAR(cStr(Indx:Indx))), IOR(DIGI_TYPE_ZERO, DIGI_TYPE_NONZERO)) /= 0)) THEN
                    Overflow = TrueVal
                ELSE
                    Overflow = FalseVal
                END IF
            ELSE
                Overflow = FalseVal
            END IF
        ELSEIF (IVal > MaxI32Div10) THEN
            Overflow = TrueVal
        ELSE
            ! IVal is equal to MaxI32Div10
            SignBit = 0
            IF (Sign == -1) SignBit = 1
            IF (Add > MaxI32Mod10 + SignBit) THEN
                Overflow = TrueVal
            ELSE
                Overflow = FalseVal
            END IF
        END IF
        IF (Overflow) THEN
            ! overflow
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (Sign == 1) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                Number = MaxI32
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                Number = MinI32
            END IF
        ELSE
            ! not overflow
            IVal = IVal*LBase + Add
            Number = INT(IVal*Sign, KIND=I4B)
        END IF
    ELSE
        ! not overflow
        Number = INT(IVal*Sign, KIND=I4B)
    END IF
    
    RETURN

END FUNCTION I32_FromChar_YY_JsonNum

!------------------------------------------------------------------------------
!
!                           64-BIT INTEGER ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION I64_FromChar_CC_FortNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL(KIND=4),               OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    INTEGER(KIND=I8B)                                       :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)           :: Indx, StrLen
    INTEGER(KIND=I8B)           :: Sign
    INTEGER(KIND=I4B)           :: NumDigit
    INTEGER(KIND=I4B)           :: IStart
    CHARACTER(LEN=1), POINTER   :: CurChr
    LOGICAL(KIND=4)             :: Overflow

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
            Number = MinI64
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1_I8B
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1_I8B
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
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
    
    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*LBase + INT(IACHAR(CurChr)-A0, KIND=I8B)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI64
                RETURN
            END IF
        END DO
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI64
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI64) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI64) THEN
        ! value might be in the applicable range
        IF (Number < 0_I8B) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1_I8B).AND.(Number == MinI64)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1_I8B
            END IF
        ELSE
            ! no overflow
            Overflow = FalseVal
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1_I8B) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI64
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I64_FromChar_CC_FortNum

!******************************************************************************

FUNCTION I64_FromChar_CC_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL(KIND=4),               OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    INTEGER(KIND=I8B)                                       :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)           :: Indx, StrLen
    INTEGER(KIND=I8B)           :: Sign
    INTEGER(KIND=I4B)           :: NumDigit
    INTEGER(KIND=I4B)           :: IStart
    CHARACTER(LEN=1), POINTER   :: CurChr
    LOGICAL(KIND=4)             :: Overflow

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
            Number = MinI64
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1_I8B
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1_I8B
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
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
    
    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*LBase + INT(IACHAR(CurChr)-A0, KIND=I8B)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
        END DO
        NumDigit = Indx - IStart
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI64) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI64) THEN
        ! value might be in the applicable range
        IF (Number < 0_I8B) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1_I8B).AND.(Number == MinI64)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1_I8B
            END IF
        ELSE
            ! no overflow
            Overflow = FalseVal
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1_I8B) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI64
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I64_FromChar_CC_FortPlus

!******************************************************************************

FUNCTION I64_FromChar_CC_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL(KIND=4),               OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    INTEGER(KIND=I8B)                                       :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)           :: Indx, StrLen
    INTEGER(KIND=I8B)           :: Sign
    INTEGER(KIND=I4B)           :: NumDigit
    INTEGER(KIND=I4B)           :: IStart
    CHARACTER(LEN=1), POINTER   :: CurChr
    LOGICAL(KIND=4)             :: Overflow

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
            Number = MinI64
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1_I8B
    IF (cStr(Indx:Indx) == '-') THEN
        Sign = -1_I8B
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
    ELSEIF (cStr(Indx:Indx) == '+') THEN
        ! < for JSON number, a plus sign is not allowed. >
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a plus sign is not allowed.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI64
        RETURN
    END IF

    ! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
        IF (Indx == StrLen) THEN
            Number = 0_I8B
        ELSE
            CurChr => cStr(Indx+1:Indx+1)
            IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: leading zero(s) is not allowed.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI64
            ELSE
                Number = 0_I8B
            END IF
        END IF
        RETURN
    END IF
    
    ! compute value of the input string
    Number   = 0_I8B
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
        END DO
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: the first character is not a digit.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI64
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI64) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI64) THEN
        ! value might be in the applicable range
        IF (Number < 0_I8B) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1_I8B).AND.(Number == MinI64)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1_I8B
            END IF
        ELSE
            ! no overflow
            Overflow = FalseVal
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1_I8B) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI64
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I64_FromChar_CC_JsonNum

!******************************************************************************

FUNCTION I64_FromChar_Lemire_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL(KIND=4),               OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    INTEGER(KIND=I8B)                                       :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)           :: Indx, IndxP7, StrLen
    INTEGER(KIND=I8B)           :: Sign
    INTEGER(KIND=I4B)           :: IStart, NumDigit
    CHARACTER(LEN=1), POINTER   :: CurChr
    CHARACTER(LEN=8)            :: wStr
    INTEGER(KIND=I8B)           :: wVal
    EQUIVALENCE(wStr, wVal)
    LOGICAL(KIND=4)             :: Overflow

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
            Number = MinI64
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1_I8B
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1_I8B
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
    END IF
    
    ! check for leading zero(s)
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
    
    ! initialize
    Number = 0_I8B
    IStart   = 0
    NumDigit = 0
    
    ! process 8 digits immediately if possible
    IndxP7 = Indx + 7
    IF (IndxP7 <= StrLen) THEN
        wStr = cStr(Indx:IndxP7)
        IF (Is_Made_Of_Eight_Digits(WVal)) THEN
            ! process 8 digits at once
            Number = Parse_Eight_Digits_Unrolled(wVal)
            IStart = Indx
            NumDigit = 8
            Indx = Indx + 8
            ! process another 8 digits immediately if possible
            IndxP7 = Indx + 7
            IF (IndxP7 <= StrLen) THEN
                wStr = cStr(Indx:IndxP7)
                IF (Is_Made_Of_Eight_Digits(WVal)) THEN
                    ! process 8 digits at once
                    Number = Number*100000000_I8B + Parse_Eight_Digits_Unrolled(wVal)
                    NumDigit = 16
                    Indx = Indx + 8
                END IF
            END IF
        END IF
    END IF
    
    ! process the remaining digits
    IF (Indx <= StrLen) THEN
        CurChr => cStr(Indx:Indx)
        IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
            IF (IStart == 0) IStart = Indx
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                Number = Number*LBase + INT(IACHAR(CurChr)-A0, KIND=I8B)
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
            END DO
            NumDigit = Indx - IStart
        END IF
    END IF
    
    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI64) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI64) THEN
        ! value might be in the applicable range
        IF (Number < 0_I8B) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1_I8B).AND.(Number == MinI64)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1_I8B
            END IF
        ELSE
            ! no overflow
            Overflow = FalseVal
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1_I8B) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI64
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I64_FromChar_Lemire_FortPlus

!******************************************************************************

FUNCTION I64_FromChar_YY_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)     :: cStr     ! character string
    LOGICAL(KIND=4),               OPTIONAL, INTENT(OUT)    :: ErrFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: ErrMsg   ! message if input is not invalid
    INTEGER(KIND=I8B)                                       :: Number   ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Digit: '0'
    INTEGER(KIND=I1B), PARAMETER   :: DIGI_TYPE_ZERO    = SHIFTL(1, 0)     ! 1 = Z'01'
    ! Digit: [1-9]
    INTEGER(KIND=I1B), PARAMETER   :: DIGI_TYPE_NONZERO = SHIFTL(1, 1)     ! 2 = Z'02'
    ! Digit type table (generate with misc/make_tables.c)
    INTEGER(KIND=I1B), PARAMETER   :: DigitTable(0:127) = [                    &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'04', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'08', KIND=I1B), INT(Z'10', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'01', KIND=I1B), INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), &
        INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), &
        INT(Z'02', KIND=I1B), INT(Z'02', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), &
        INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B), INT(Z'00', KIND=I1B)]
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: IVal, Add
    INTEGER(KIND=I4B)   :: Indx, StrLen, I
    INTEGER(KIND=I8B)   :: Sign, SignBit
    INTEGER(KIND=I4B)   :: CurCode      ! ASCII code of current character
    LOGICAL             :: Digit_End, Overflow

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
            Number = MinI64
            RETURN
        END IF
    END IF

    ! check for sign
    Sign  = 1_I8B
    IF (cStr(Indx:Indx) == '-') THEN
        Sign = -1_I8B
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
    ELSEIF (cStr(Indx:Indx) == '+') THEN
        ! < for JSON number, a plus sign is not allowed. >
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a plus sign is not allowed.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI64
        RETURN
    END IF
    
    ! check whether first digit is zero
    ! < for JSON number, the first digit being zero is not allowed. >
    CurCode = IACHAR(cStr(Indx:Indx))
    IF (.NOT.(IAND(DigitTable(CurCode), DIGI_TYPE_NONZERO) /= 0)) THEN
        IF (CurCode == A0) THEN
            IF (Indx+1 > StrLen) THEN
                Number = 0_I8B
            ELSE
                IF (.NOT.(IAND(DigitTable(IACHAR(cStr(Indx+1:Indx+1))), IOR(DIGI_TYPE_ZERO, DIGI_TYPE_NONZERO)) /= 0)) THEN
                    Number = 0_I8B
                ELSE
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: leading zero(s) is not allowed.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI64
                END IF
            END IF
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: the first character is not a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
        END IF
        RETURN
    END IF
    
    ! compute IVal for the next 17 characters (digits)
    IVal = INT(CurCode - A0, KIND=I8B)
    I = 1
!DIR$ UNROLL = 17
    DO
        IF (Indx+I <= StrLen) THEN
            CurCode = IACHAR(cStr(Indx+I:Indx+I))
            IF ((IAND(DigitTable(CurCode), IOR(DIGI_TYPE_ZERO, DIGI_TYPE_NONZERO)) /= 0)) THEN
                IVal = IVal*LBase + INT(CurCode-A0, KIND=I8B)
            ELSE
                Digit_End = TrueVal
                EXIT
            END IF
        ELSE
            Digit_End = TrueVal
            EXIT
        END IF
        I = I + 1
        IF (I > 17) THEN
            Digit_End = FalseVal
            EXIT
        END IF
    END DO
    
    IF ((Digit_End).OR.(Indx + I > StrLen)) THEN
        Number = IVal*Sign
        RETURN
    END IF

    ! deal with more digit(s)
    Indx = Indx + I
    CurCode = IACHAR(cStr(Indx:Indx))
    IF ((IAND(DigitTable(CurCode), IOR(DIGI_TYPE_ZERO, DIGI_TYPE_NONZERO)) /= 0)) THEN
        ! must check overflow
        Add = INT(CurCode - A0, KIND=I8B)
        ! check overflow
        IF (IVal < MaxI64Div10) THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                IF ((IAND(DigitTable(IACHAR(cStr(Indx:Indx))), IOR(DIGI_TYPE_ZERO, DIGI_TYPE_NONZERO)) /= 0)) THEN
                    Overflow = TrueVal
                ELSE
                    Overflow = FalseVal
                END IF
            ELSE
                Overflow = FalseVal
            END IF
        ELSEIF (IVal > MaxI64Div10) THEN
            Overflow = TrueVal
        ELSE
            ! IVal is equal to MaxI64Div10
            SignBit = 0_I8B
            IF (Sign == -1_I8B) SignBit = 1_I8B
            IF (Add > MaxI64Mod10 + SignBit) THEN
                Overflow = TrueVal
            ELSE
                Overflow = FalseVal
            END IF
        END IF
        IF (Overflow) THEN
            ! overflow
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (Sign == 1_I8B) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                Number = MaxI64
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                Number = MinI64
            END IF
        ELSE
            ! not overflow
            IVal = IVal*LBase + Add
            Number = IVal*Sign
        END IF
    ELSE
        ! no overflow
        Number = IVal*Sign
    END IF
    
    RETURN

END FUNCTION I64_FromChar_YY_JsonNum

!------------------------------------------------------------------------------
!
!                           AUXILIARY ROUTINES
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
    LOGICAL(KIND=4)            :: Flag

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

END MODULE ModBase_Integer_FromChar

!******************************************************************************
