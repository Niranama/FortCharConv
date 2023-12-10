
MODULE ModBase_Integer_ToChar

!^ **PURPOSE OF THIS MODULE**:  
    ! This module contains routines that convert an integer value into a decimal string.  
    !    
!^ **REFERENCES**:  
    ! [1] [Number Conversion Benchmark in C](https://github.com/ibireme/c_numconv_benchmark)    
    ! [2] [itoa - Fast integer to ascii / integer to string conversion](https://github.com/jeaiii/itoa)

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_UIntUtil,           ONLY: U128_Multiply_High => UMul128_Upper64
    USE ModBase_Tables_CharConv,    ONLY: Char1Digit, Char2Digits, Char4Digits

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! 32-bit integer
    PUBLIC :: I32_ToChar_Basic,         I32_ToChar_CC
    PUBLIC :: I32_ToChar_YY,            I32_ToChar_YYLL
    PUBLIC :: I32_ToChar_JEA
    ! 64-bit integer
    PUBLIC :: I64_ToChar_Basic,         I64_ToChar_CC
    PUBLIC :: I64_ToChar_YY,            I64_ToChar_YYLL
    PUBLIC :: I64_ToChar_JEA

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    INTEGER(KIND=I4B), PARAMETER  :: MinI32 = INT(Z'80000000', KIND=I4B)            ! -2147483648
    INTEGER(KIND=I8B), PARAMETER  :: MinI64 = INT(Z'8000000000000000', KIND=I8B)    ! -9223372036854775808

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

FUNCTION I32_ToChar_Basic(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 32-bit integer to a decimal string using basic (naive) algorithm

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 10
    INTEGER(KIND=I4B), PARAMETER  :: Base   = 10
    CHARACTER(LEN=1),  PARAMETER  :: NumStr(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=MaxLen)         :: wStr     ! working string
    INTEGER(KIND=I4B)             :: PosNum   ! positive number (working number)
    INTEGER(KIND=I4B)             :: CurNum   ! current (saved) working number
    INTEGER(KIND=I4B)             :: RemNum   ! remainder number
    INTEGER(KIND=I4B)             :: Indx

!** FLOW

    ! check whether the number is zero
    IF (Number == 0) THEN
        cStr = '0'
        RETURN
    END IF
    IF (Number < 0) THEN
        IF (Number == MinI32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        PosNum = ABS(Number)
    ELSE
        PosNum = Number
    END IF
    Indx = MaxLen

    ! start the conversion
    DO
        ! save current number
        CurNum = PosNum
        ! compute the next round of working number
        PosNum = PosNum/Base
        ! compute the remainder
        RemNum = CurNum - PosNum*Base
        ! convert the remainder to a working string
        wStr(Indx:Indx) = NumStr(RemNum)
        Indx = Indx - 1
        IF (PosNum == 0) EXIT
    END DO

    ! allocate the resulting string and transfer
    ! characters from the working string
    Indx = Indx + 1
    IF (Number < 0) THEN
        cStr = '-' // wStr(Indx:MaxLen)
    ELSE
        cStr = wStr(Indx:MaxLen)
    END IF

    RETURN

END FUNCTION I32_ToChar_Basic

!******************************************************************************

FUNCTION I32_ToChar_CC(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 32-bit integer to a decimal string using CC algorithm

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 10
    INTEGER(KIND=I4B), PARAMETER  :: ShiftPos = 45
    INTEGER(KIND=I8B), PARAMETER  :: Multiplier = INT(Z'00000000D1B71759', KIND=I8B)
    INTEGER(KIND=I4B), PARAMETER  :: Divisor = 10000

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=MaxLen)         :: wStr     ! working string
    INTEGER(KIND=I4B)             :: PosNum   ! positive number (working number)
    INTEGER(KIND=I4B)             :: NxtNum   ! next round of positive number
    INTEGER(KIND=I4B)             :: RemNum   ! remainder number
    INTEGER(KIND=I4B)             :: Start, Finish

!** FLOW

    ! set positive number
    PosNum = ABS(Number)

    ! start the conversion
    IF (PosNum < 10000) THEN
        Start  = 1
        IF (PosNum < 100) THEN
            wStr(1:2) = Char2Digits(PosNum)
            Finish = 2
            IF (wStr(Start:Start) == '0') Start = 2
        ELSE
            wStr(1:4) = Char4Digits(PosNum)
            Finish = 4
            IF (wStr(Start:Start) == '0') Start = 2
        END IF
    ELSE
        ! compute the next round of working number
        NxtNum = INT(SHIFTR(PosNum*Multiplier, ShiftPos), KIND=I4B) ! NxtNum = PosNum/10000
        ! compute the remainder
        RemNum = PosNum - NxtNum*Divisor                            ! RemNum = MOD(PosNum, 10000)
        ! convert the remainder to a working string
        wStr(7:10) = Char4Digits(RemNum)
        Finish = 10
        PosNum = NxtNum
        IF (PosNum < 10000) THEN
            IF (PosNum < 100) THEN
                wStr(5:6) = Char2Digits(PosNum)
                Start  = 5
                IF (wStr(Start:Start) == '0') Start = 6
            ELSE
                wStr(3:6) = Char4Digits(PosNum)
                Start  = 3
                IF (wStr(Start:Start) == '0') Start = 4
            END IF
        ELSE
            ! compute the next round of working number
            NxtNum = INT(SHIFTR(PosNum*Multiplier, ShiftPos), KIND=I4B) ! NxtNum = PosNum/10000
            ! compute the remainder
            RemNum = PosNum - NxtNum*Divisor                            ! RemNum = MOD(PosNum, 10000)
            ! convert the remainder to a working string
            wStr(3:6) = Char4Digits(RemNum)
            IF (NxtNum > 0) THEN
                IF (NxtNum < 10) THEN
                    wStr(2:2) = Char2Digits(NxtNum)(2:2)
                    Start = 2
                ELSE
                    wStr(1:2) = Char2Digits(NxtNum)
                    Start = 1
                END IF
            ELSE
                Start = 3
            END IF
        END IF
    END IF

    IF (Number < 0) THEN
        IF (Number == MinI32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF

    RETURN

END FUNCTION I32_ToChar_CC

!******************************************************************************

FUNCTION I32_ToChar_YY(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 32-bit integer to a decimal string using YY algorithm

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 10

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=MaxLen)         :: wStr
    INTEGER(KIND=I4B)             :: PosNum
    INTEGER(KIND=I4B)             :: Finish, Start
    INTEGER(KIND=I4B)             :: AA, BB, CC, DD
    INTEGER(KIND=I4B)             :: AABB, BBCC, CCDD
    INTEGER(KIND=I4B)             :: AABBCC, DDEE, EE

!** FLOW

    ! set positive number
    PosNum = ABS(Number)

    ! start the conversion
    IF (PosNum < 100) THEN                                      ! 1-2 digits
        wStr(1:2) = Char2Digits(PosNum)
        Finish = 2
    ELSEIF (PosNum < 10000) THEN                                ! 3-4 digits
        AA = INT(SHIFTR(PosNum*5243, 19), KIND=I4B)             ! PosNum / 100
        BB = PosNum - AA*100                                    ! MOD(PosNum, 100)
        wStr(1:2) = Char2Digits(AA)
        wStr(3:4) = Char2Digits(BB)
        Finish = 4
    ELSEIF (PosNum < 1000000) THEN                              ! 5-6 digits
        AA = INT(SHIFTR(PosNum*429497_I8B, 32), KIND=I4B)       ! PosNum / 10000
        BBCC = PosNum - AA*10000                                ! MOD(PosNum, 10000)
        BB = SHIFTR(BBCC*5243, 19)                              ! BBCC / 100
        CC = BBCC - BB*100                                      ! MOD(BBCC, 100)
        wStr(1:2) = Char2Digits(AA)
        wStr(3:4) = Char2Digits(BB)
        wStr(5:6) = Char2Digits(CC)
        Finish = 6
    ELSEIF (PosNum < 100000000) THEN                            ! 7-8 digits
        AABB = INT(SHIFTR(PosNum*109951163_I8B, 40), KIND=I4B)  ! PosNum / 10000
        CCDD = PosNum - AABB*10000                              ! MOD(PosNum, 10000)
        AA = SHIFTR(AABB*5243, 19)                              ! AABB / 100
        CC = SHIFTR(CCDD*5243, 19)                              ! CCDD / 100
        BB = AABB - AA*100                                      ! MOD(AABB, 100)
        DD = CCDD - CC*100                                      ! MOD(CCDD, 100)
        wStr(1:2) = Char2Digits(AA)
        wStr(3:4) = Char2Digits(BB)
        wStr(5:6) = Char2Digits(CC)
        wStr(7:8) = Char2Digits(DD)
        Finish = 8
    ELSE                                                            ! 9-10 digits
        AABBCC = INT(SHIFTR(PosNum*3518437209_I8B, 45), KIND=I4B)   ! PosNum / 10000
        AA   = INT(SHIFTR(AABBCC*429497_I8B, 32), KIND=I4B)         ! aabbcc / 10000
        DDEE = PosNum - AABBCC*10000                                ! MOD(PosNum, 10000)
        BBCC = AABBCC - AA*10000                                    ! MOD(aabbcc, 10000)
        BB = SHIFTR(BBCC*5243, 19)                                  ! bbcc / 100
        DD = SHIFTR(DDEE*5243, 19)                                  ! ddee / 100
        CC = BBCC - BB*100                                          ! MOD(bbcc, 100)
        EE = DDEE - DD*100                                          ! MOD(ddee, 100)
        wStr(1:2)  = Char2Digits(AA)
        wStr(3:4)  = Char2Digits(BB)
        wStr(5:6)  = Char2Digits(CC)
        wStr(7:8)  = Char2Digits(DD)
        wStr(9:10) = Char2Digits(EE)
        Finish = 10
    END IF

    Start = 1
    IF (wStr(1:1) == '0') Start = 2

    IF (Number < 0) THEN
        IF (Number == MinI32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF

    RETURN

END FUNCTION I32_ToChar_YY

!******************************************************************************

FUNCTION I32_ToChar_YYLL(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 32-bit integer to a decimal string using YY algorithm with large table

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 10

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=MaxLen)         :: wStr
    INTEGER(KIND=I4B)             :: PosNum
    INTEGER(KIND=I4B)             :: Finish, Start
    INTEGER(KIND=I4B)             :: AA
    INTEGER(KIND=I4B)             :: AABB, BBCC, CCDD
    INTEGER(KIND=I4B)             :: BBCCDDEE, DDEE

!** FLOW

    ! set positive number
    PosNum = ABS(Number)
    Start = 1

    ! start the conversion
    IF (PosNum < 10000) THEN                                    ! 1-4 digits
        wStr(1:4) = Char4Digits(PosNum)
        IF (wStr(1:1) == '0') THEN
            Start = 2
            IF (wStr(2:2) == '0') THEN
                Start = 3
                IF (wStr(3:3) == '0') THEN
                    Start = 4
                END IF
            END IF
        END IF
        Finish = 4
    ELSEIF (PosNum < 100000000) THEN                            ! 5-8 digits
        AABB = INT(SHIFTR(PosNum*109951163_I8B, 40), KIND=I4B)  ! PosNum / 10000
        CCDD = PosNum - AABB*10000                              ! MOD(PosNum, 10000)
        wStr(1:4) = Char4Digits(AABB)
        wStr(5:8) = Char4Digits(CCDD)
        IF (wStr(1:1) == '0') THEN
            Start = 2
            IF (wStr(2:2) == '0') THEN
                Start = 3
                IF (wStr(3:3) == '0') THEN
                    Start = 4
                END IF
            END IF
        END IF
        Finish = 8
    ELSE                                                            ! 9-10 digits
        AA = INT(SHIFTR(PosNum*1441151881_I8B, 57), KIND=I4B)       ! PosNum / 100000000
        BBCCDDEE = PosNum - AA*100000000                            ! MOD(PosNum, 100000000)
        BBCC = INT(SHIFTR(BBCCDDEE*109951163_I8B, 40), KIND=I4B)    ! BBCCDDEE / 10000
        DDEE = BBCCDDEE - BBCC*10000                                ! MOD(BBCCDDEE, 10000)
        wStr(1:2)  = Char2Digits(AA)
        wStr(3:6)  = Char4Digits(BBCC)
        wStr(7:10) = Char4Digits(DDEE)
        IF (wStr(1:1) == '0') THEN
            Start = 2
        END IF
        Finish = 10
    END IF

    IF (Number < 0) THEN
        IF (Number == MinI32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF

    RETURN

END FUNCTION I32_ToChar_YYLL

!******************************************************************************

FUNCTION I32_ToChar_JEA(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 32-bit integer to a decimal string using JEA algorithm

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN) :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 10
    REAL(KIND=QP),     PARAMETER  :: R1  = 1.0_QP
    REAL(KIND=QP),     PARAMETER  :: R2  = 2.0_QP
    INTEGER(KIND=I8B), PARAMETER  :: K24 = INT(R2**24/1.0E2_QP + R1, KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: K32 = INT(R2**32/1.0E4_QP + R1, KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: K48 = INT(R2**48/1.0E6_QP + R1, KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: K57 = INT(R2**57/1.0E8_QP + R1, KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: M24 = SHIFTL(1_I8B, 24) - 1_I8B
    INTEGER(KIND=I8B), PARAMETER  :: M32 = SHIFTL(1_I8B, 32) - 1_I8B
    INTEGER(KIND=I8B), PARAMETER  :: M57 = SHIFTL(1_I8B, 57) - 1_I8B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=MaxLen)         :: wStr
    INTEGER(KIND=I8B)             :: PosNum
    INTEGER(KIND=I4B)             :: Finish, Start
    INTEGER(KIND=I8B)             :: F0, F2, F4, F6, F8

!** FLOW

    ! set positive number
    PosNum = ABS(Number)

    ! start the conversion
    IF (PosNum < 100_I8B) THEN                   ! 1-2 digits
        wStr(1:2) = Char2Digits(PosNum)
        Finish = 2
    ELSEIF (PosNum < 10000_I8B) THEN             ! 3-4 digits
        F0 = K24*PosNum
        F2 = IAND(F0, M24)*100_I8B
        wStr(1:2) = Char2Digits(SHIFTR(F0, 24))
        wStr(3:4) = Char2Digits(SHIFTR(F2, 24))
        Finish = 4
    ELSEIF (PosNum < 1000000_I8B) THEN           ! 5-6 digits
        F0 = K32*PosNum
        F2 = IAND(F0, M32)*100_I8B
        F4 = IAND(F2, M32)*100_I8B
        wStr(1:2) = Char2Digits(SHIFTR(F0, 32))
        wStr(3:4) = Char2Digits(SHIFTR(F2, 32))
        wStr(5:6) = Char2Digits(SHIFTR(F4, 32))
        Finish = 6
    ELSEIF (PosNum < 100000000_I8B) THEN         ! 7-8 digits
        F0 = SHIFTR(K48*PosNum, 16)
        F2 = IAND(F0, M32)*100_I8B
        F4 = IAND(F2, M32)*100_I8B
        F6 = IAND(F4, M32)*100_I8B
        wStr(1:2) = Char2Digits(SHIFTR(F0, 32))
        wStr(3:4) = Char2Digits(SHIFTR(F2, 32))
        wStr(5:6) = Char2Digits(SHIFTR(F4, 32))
        wStr(7:8) = Char2Digits(SHIFTR(F6, 32))
        Finish = 8
    ELSE                                        ! 9-10 digits
        F0 = K57*PosNum
        F2 = IAND(F0, M57)*100_I8B
        F4 = IAND(F2, M57)*100_I8B
        F6 = IAND(F4, M57)*100_I8B
        F8 = IAND(F6, M57)*100_I8B
        wStr(1:2)  = Char2Digits(SHIFTR(F0, 57))
        wStr(3:4)  = Char2Digits(SHIFTR(F2, 57))
        wStr(5:6)  = Char2Digits(SHIFTR(F4, 57))
        wStr(7:8)  = Char2Digits(SHIFTR(F6, 57))
        wStr(9:10) = Char2Digits(SHIFTR(F8, 57))
        Finish = 10
    END IF

    Start = 1
    IF (wStr(1:1) == '0') Start = 2

    IF (Number < 0) THEN
        IF (Number == MinI32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF

    RETURN

END FUNCTION I32_ToChar_JEA

!------------------------------------------------------------------------------
!
!                           64-BIT INTEGER ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION I64_ToChar_Basic(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 64-bit integer to a decimal string using basic (naive) algorithm

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN) :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 19
    INTEGER(KIND=I8B), PARAMETER  :: Base   = 10_I8B
    CHARACTER(LEN=1),  PARAMETER  :: NumStr(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=MaxLen)         :: wStr     ! working string
    INTEGER(KIND=I8B)             :: PosNum   ! positive number (working number)
    INTEGER(KIND=I8B)             :: CurNum   ! current (saved) working number
    INTEGER(KIND=I8B)             :: RemNum   ! remainder number
    INTEGER(KIND=I4B)             :: Indx

!** FLOW

    ! check whether the number is zero
    IF (Number == 0_I8B) THEN
        cStr = '0'
        RETURN
    END IF
    IF (Number < 0_I8B) THEN
        IF (Number == MinI64) THEN
            cStr = '-9223372036854775808'
            RETURN
        END IF
        PosNum = ABS(Number)
    ELSE
        PosNum = Number
    END IF
    Indx = MaxLen

    ! start the conversion
    DO
        ! save current number
        CurNum = PosNum
        ! compute the next round of working number
        PosNum = PosNum/Base
        ! compute the remainder
        RemNum = CurNum - PosNum*Base
        ! convert the remainder to a working string
        wStr(Indx:Indx) = NumStr(RemNum)
        Indx = Indx - 1
        IF (PosNum == 0_I8B) EXIT
    END DO

    ! allocate the resulting string and transfer
    ! characters from the working string
    Indx = Indx + 1
    IF (Number < 0_I8B) THEN
        cStr = '-' // wStr(Indx:MaxLen)
    ELSE
        cStr = wStr(Indx:MaxLen)
    END IF

    RETURN

END FUNCTION I64_ToChar_Basic

!******************************************************************************

FUNCTION I64_ToChar_CC(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 64-bit integer to a decimal string using CC algorithm

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN) :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 19
    INTEGER(KIND=I8B), PARAMETER  :: Div1E8 = 100000000_I8B
    ! multiplier and shift for 19 digits and divisor of 1.0E8
    INTEGER(KIND=I8B), PARAMETER  :: M90 = INT(Z'ABCC77118461CEFD', KIND=I8B)
    INTEGER(KIND=I4B), PARAMETER  :: S90 = 90 - 64
    ! multiplier for 11 digits and divisor of 1.0E8
    INTEGER(KIND=I8B), PARAMETER  :: M64 = INT(Z'0000002AF31DC462', KIND=I8B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=MaxLen)         :: wStr
    INTEGER(KIND=I8B)             :: PosNum
    INTEGER(KIND=I8B)             :: NxtNum, RemNum
    INTEGER(KIND=I4B)             :: Start

!** FLOW

    ! set positive number
    PosNum = ABS(Number)

    ! start the conversion
    IF (PosNum < 1000000000_I8B) THEN
        ! utilize I32_ToChar_CC routine
        cStr = I32_ToChar_CC(INT(Number, KIND=I4B))
        RETURN
    ELSE
        ! compute NxtNum = PosNum/100000000
        NxtNum = SHIFTR(U128_Multiply_High(PosNum, M90), S90)
        ! compute RemNum = MOD(PosNum, 100000000)
        RemNum = PosNum - NxtNum*Div1E8
        ! convert the remainder to a working string
        CALL Write_8_Digits(INT(RemNum, KIND=I4B), wStr(12:19))

        PosNum = NxtNum
        IF (PosNum > Div1E8) THEN
            ! compute NxtNum = PosNum/100000000
            NxtNum = U128_Multiply_High(PosNum, M64)
            ! compute RemNum = MOD(PosNum, 100000000)
            RemNum = PosNum - NxtNum*Div1E8
            ! convert the remainder to a working string
            CALL Write_8_Digits(INT(RemNum, KIND=I4B), wStr(4:11))

            ! convert the rest
            IF (NxtNum < 10) THEN
                wStr(3:3) = Char2Digits(NxtNum)(2:2)
                Start = 3
            ELSEIF (NxtNum < 100) THEN
                wStr(2:3) = Char2Digits(NxtNum)
                Start = 2
            ELSE
                wStr(1:3) = Char4Digits(NxtNum)(2:4)
                Start = 1
            END IF
        ELSE
            ! convert the rest
            Start = 3 + Write_1_to_8_Digits(INT(PosNum, KIND=I4B), wStr(4:11))
        END IF
        ! transfer to output string
        IF (Number < 0_I8B) THEN
            IF (Number == MinI64) THEN
                cStr = '-9223372036854775808'
                RETURN
            END IF
            cStr = '-' // wStr(Start:MaxLen)
        ELSE
            cStr = wStr(Start:MaxLen)
        END IF
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Write_8_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! multiplier and shift for 4-byte integer and divisor of 10000
        INTEGER(KIND=I4B), PARAMETER  :: ShiftPos = 45
        INTEGER(KIND=I8B), PARAMETER  :: Multiplier = INT(Z'00000000D1B71759', KIND=I8B)
        INTEGER(KIND=I4B), PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: PosNum, NxtNum, RemNum

    !** FLOW

        ! set working number
        PosNum = Number
        ! compute NxtNum = PosNum/10000
        NxtNum = INT(SHIFTR(PosNum*Multiplier, ShiftPos), KIND=I4B)
        ! compute RemNum = MOD(PosNum, 10000)
        RemNum = PosNum - NxtNum*Divisor
        ! convert the remainder to a working string
        cStr(5:8) = Char4Digits(RemNum)
        ! convert the rest
        cStr(1:4) = Char4Digits(NxtNum)

        RETURN

    END SUBROUTINE Write_8_Digits

!**************************************************************************

    FUNCTION Write_1_to_8_Digits(Number, cStr) RESULT(Start)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_1_to_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 1 to 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),   INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B)                     :: Start

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! multiplier and shift for 4-byte integer and divisor of 10000
        INTEGER(KIND=I4B), PARAMETER  :: ShiftPos = 45
        INTEGER(KIND=I8B), PARAMETER  :: Multiplier = INT(Z'00000000D1B71759', KIND=I8B)
        INTEGER(KIND=I4B), PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: PosNum, NxtNum, RemNum

    !** FLOW

        PosNum = Number
        IF (PosNum < 10000) THEN
            IF (PosNum < 100) THEN
                cStr(7:8) = Char2Digits(PosNum)
                Start = 7
                IF (cStr(Start:Start) == '0') Start = 8
            ELSE
                cStr(5:8) = Char4Digits(PosNum)
                Start = 5
                IF (cStr(Start:Start) == '0') Start = 6
            END IF
        ELSE
            ! compute NxtNum = PosNum/10000
            NxtNum = INT(SHIFTR(PosNum*Multiplier, ShiftPos), KIND=I4B)
            ! compute RemNum = MOD(PosNum, 10000)
            RemNum = PosNum - NxtNum*Divisor
            ! convert the remainder to a working string
            cStr(5:8) = Char4Digits(RemNum)
            IF (NxtNum < 100) THEN
                cStr(3:4) = Char2Digits(NxtNum)
                Start = 3
                IF (cStr(Start:Start) == '0') Start = 4
            ELSE
                cStr(1:4) = Char4Digits(NxtNum)
                Start  = 1
                IF (cStr(Start:Start) == '0') Start = 2
            END IF
        END IF

        RETURN

    END FUNCTION Write_1_to_8_Digits

!**************************************************************************

END FUNCTION I64_ToChar_CC

!******************************************************************************

FUNCTION I64_ToChar_YY(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 64-bit integer to a decimal string using YY algorithm

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN) :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 20

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=MaxLen)         :: wStr
    INTEGER(KIND=I8B)             :: PosNum, TmpNum, HiNum, LoNum, MidNum
    INTEGER(KIND=I4B)             :: Start

!** FLOW

    ! set positive number
    PosNum = ABS(Number)

    ! start conversion and store digits in working string
    IF (PosNum < 100000000_I8B) THEN                        ! 1-8 digits
        Start = 12 + Write_1_to_8_Digits(INT(PosNum, KIND=I4B), wStr(13:20))
    ELSEIF (PosNum < 10000000000000000_I8B) THEN            ! 9-16 digits
        HiNum = PosNum / 100000000_I8B
        LoNum = PosNum - HiNum * 100000000_I8B              ! MOD(PosNum, 100000000)
        CALL Write_8_Digits(INT(LoNum, KIND=I4B), wStr(13:20))
        Start = 4 + Write_1_to_8_Digits(INT(HiNum, KIND=I4B), wStr(5:12))
    ELSE                                                    ! 17-20 digits
        TmpNum = PosNum / 100000000_I8B
        LoNum = PosNum - TmpNum * 100000000_I8B             ! MOD(PosNum, 100000000)
        HiNum = TmpNum / 10000_I8B
        MidNum = TmpNum - HiNum * 10000_I8B                 ! MOD(TmpNum, 10000)
        CALL Write_8_Digits(INT(LoNum, KIND=I4B), wStr(13:20))
        CALL Write_4_Digits(INT(MidNum, KIND=I4B), wStr(9:12))
        Start = Write_5_to_8_Digits(INT(HiNum, KIND=I4B), wStr(1:8))
    END IF

    ! transfer to output string
    IF (Number < 0_I8B) THEN
        IF (Number == MinI64) THEN
            cStr = '-9223372036854775808'
            RETURN
        END IF
        cStr = '-' // wStr(Start:MaxLen)
    ELSE
        cStr = wStr(Start:MaxLen)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Write_8_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),   INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: AA, BB, CC, DD   ! working indices
        INTEGER(KIND=I4B)     :: AABB, CCDD       ! working variables

    !** FLOW

        AABB = INT(SHIFTR(INT(Number, KIND=I8B)*109951163_I8B, 40), KIND=I4B)   ! Number / 10000
        CCDD = Number - AABB*10000                                              ! MOD(Number, 10000)
        AA = SHIFTR(AABB*5243, 19)                                              ! AABB / 100
        CC = SHIFTR(CCDD*5243, 19)                                              ! CCDD / 100
        BB = AABB - AA*100                                                      ! MOD(AABB, 100)
        DD = CCDD - CC*100                                                      ! MOD(CCDD, 100)
        cStr(1:2) = Char2Digits(AA)
        cStr(3:4) = Char2Digits(BB)
        cStr(5:6) = Char2Digits(CC)
        cStr(7:8) = Char2Digits(DD)

        RETURN

    END SUBROUTINE Write_8_Digits

!**************************************************************************

    SUBROUTINE Write_4_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_4_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: AA, BB   ! working indices

    !** FLOW

        AA = SHIFTR(Number*5243, 19)            ! Number / 100
        BB = Number - AA*100                    ! MOD(Number, 100)
        cStr(1:2) = Char2Digits(AA)
        cStr(3:4) = Char2Digits(BB)

        RETURN

    END SUBROUTINE Write_4_Digits

!**************************************************************************

    FUNCTION Write_1_to_8_Digits(Number, cStr) RESULT(Start)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_1_to_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 1 to 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B)                   :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: AA, BB, CC, DD   ! working indices
        INTEGER(KIND=I4B)     :: AABB, BBCC, CCDD ! working variables

    !** FLOW

        IF (Number < 100) THEN                                              ! 1-2 digits
            AA = Number
            IF (AA < 10) THEN
                cStr(8:8) = Char2Digits(AA)(2:2)
                Start = 8
            ELSE
                cStr(7:8) = Char2Digits(AA)
                Start = 7
            END IF
        ELSEIF (Number < 10000) THEN                                        ! 3-4 digits
            AA = INT(SHIFTR(INT(Number, KIND=I8B)*5243_I8B, 19), KIND=I4B)  ! Number / 100
            BB = Number - AA*100                                            ! MOD(Number, 100)
            IF (AA < 10) THEN
                cStr(6:6) = Char2Digits(AA)(2:2)
                cStr(7:8) = Char2Digits(BB)
                Start = 6
            ELSE
                cStr(5:6) = Char2Digits(AA)
                cStr(7:8) = Char2Digits(BB)
                Start = 5
            END IF
        ELSEIF (Number < 1000000) THEN                                          ! 5-6 digits
            AA = INT(SHIFTR(INT(Number, KIND=I8B)*429497_I8B, 32), KIND=I4B)    ! Number / 10000
            BBCC = Number - AA*10000                                            ! MOD(Number, 10000)
            BB = SHIFTR(BBCC*5243, 19)                                          ! BBCC / 100
            CC = BBCC - BB*100                                                  ! MOD(BBCC, 100)
            IF (AA < 10) THEN
                cStr(4:4) = Char2Digits(AA)(2:2)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 4
            ELSE
                cStr(3:4) = Char2Digits(AA)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 3
            END IF
        ELSEIF (Number < 100000000) THEN                                            ! 7-8 digits
            AABB = INT(SHIFTR(INT(Number, KIND=I8B)*109951163_I8B, 40), KIND=I4B)   ! Number / 10000
            CCDD = Number - AABB*10000                                              ! MOD(Number, 10000)
            AA = SHIFTR(AABB*5243, 19)                                              ! AABB / 100
            CC = SHIFTR(CCDD*5243, 19)                                              ! CCDD / 100
            BB = AABB - AA*100                                                      ! MOD(AABB, 100)
            DD = CCDD - CC*100                                                      ! MOD(CCDD, 100)
            IF (AA < 10) THEN
                cStr(2:2) = Char2Digits(AA)(2:2)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 2
            ELSE
                cStr(1:2) = Char2Digits(AA)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 1
            END IF
        END IF

        RETURN

    END FUNCTION Write_1_to_8_Digits

!**************************************************************************

    FUNCTION Write_5_to_8_Digits(Number, cStr) RESULT(Start)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_5_to_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 5 to 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B)                   :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: AA, BB, CC, DD   ! working indices
        INTEGER(KIND=I4B)     :: AABB, BBCC, CCDD ! working variables

    !** FLOW

        IF (Number < 1000000) THEN                                              ! 5-6 digits
            AA = INT(SHIFTR(INT(Number, KIND=I8B)*429497_I8B, 32), KIND=I4B)     ! Number / 10000
            BBCC = Number - AA*10000                                            ! MOD(Number, 10000)
            BB = SHIFTR(BBCC*5243, 19)                                          ! BBCC / 100
            CC = BBCC - BB*100                                                  ! MOD(BBCC, 100)
            IF (AA < 10) THEN
                cStr(4:4) = Char2Digits(AA)(2:2)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 4
            ELSE
                cStr(3:4) = Char2Digits(AA)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 3
            END IF
        ELSEIF (Number < 100000000) THEN                                            ! 7-8 digits
            AABB = INT(SHIFTR(INT(Number, KIND=I8B)*109951163_I8B, 40), KIND=I4B)   ! Number / 10000
            CCDD = Number - AABB*10000                                              ! MOD(Number, 10000)
            AA = SHIFTR(AABB*5243, 19)                                              ! AABB / 100
            CC = SHIFTR(CCDD*5243, 19)                                              ! CCDD / 100
            BB = AABB - AA*100                                                      ! MOD(AABB, 100)
            DD = CCDD - CC*100                                                      ! MOD(CCDD, 100)
            IF (AA < 10) THEN
                cStr(2:2) = Char2Digits(AA)(2:2)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 2
            ELSE
                cStr(1:2) = Char2Digits(AA)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 1
            END IF
        END IF

        RETURN

    END FUNCTION Write_5_to_8_Digits

!**************************************************************************

END FUNCTION I64_ToChar_YY

!******************************************************************************

FUNCTION I64_ToChar_YYLL(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 64-bit integer to a decimal string using YY algorithm with large table

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN) :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 20

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=MaxLen)         :: wStr
    INTEGER(KIND=I8B)             :: PosNum, TmpNum, HiNum, LoNum, MidNum
    INTEGER(KIND=I4B)             :: Start

!** FLOW

    ! set positive number
    PosNum = ABS(Number)

    ! start conversion and store digits in working string
    IF (PosNum < 100000000_I8B) THEN                        ! 1-8 digits
        Start = 12 + Write_1_to_8_Digits(INT(PosNum, KIND=I4B), wStr(13:20))
    ELSEIF (PosNum < 10000000000000000_I8B) THEN            ! 9-16 digits
        HiNum = PosNum / 100000000_I8B
        LoNum = PosNum - HiNum * 100000000_I8B           ! MOD(PosNum, 100000000)
        CALL Write_8_Digits(INT(LoNum, KIND=I4B), wStr(13:20))
        Start = 4 + Write_1_to_8_Digits(INT(HiNum, KIND=I4B), wStr(5:12))
    ELSE                                                    ! 17-20 digits
        TmpNum = PosNum / 100000000_I8B
        LoNum = PosNum - TmpNum * 100000000_I8B             ! MOD(PosNum, 100000000)
        HiNum = TmpNum / 10000_I8B
        MidNum = TmpNum - HiNum * 10000_I8B                 ! MOD(TmpNum, 10000)
        CALL Write_8_Digits(INT(LoNum, KIND=I4B), wStr(13:20))
        CALL Write_4_Digits(INT(MidNum, KIND=I4B), wStr(9:12))
        Start = Write_5_to_8_Digits(INT(HiNum, KIND=I4B), wStr(1:8))
    END IF

    ! transfer to output string
    IF (Number < 0_I8B) THEN
        IF (Number == MinI64) THEN
            cStr = '-9223372036854775808'
            RETURN
        END IF
        cStr = '-' // wStr(Start:MaxLen)
    ELSE
        cStr = wStr(Start:MaxLen)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Write_8_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: AABB, CCDD       ! working variables

    !** FLOW

        AABB = INT(SHIFTR(INT(Number, KIND=I8B)*109951163_I8B, 40), KIND=I4B)   ! Number / 10000
        CCDD = Number - AABB*10000                                              ! MOD(Number, 10000)
        cStr(1:4) = Char4Digits(AABB)
        cStr(5:8) = Char4Digits(CCDD)

        RETURN

    END SUBROUTINE Write_8_Digits

!**************************************************************************

    SUBROUTINE Write_4_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_4_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        cStr(1:4) = Char4Digits(Number)

        RETURN

    END SUBROUTINE Write_4_Digits

!**************************************************************************

    FUNCTION Write_1_to_8_Digits(Number, cStr) RESULT(Start)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_1_to_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 1 to 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B)                   :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: AABB, CCDD

    !** FLOW

        IF (Number < 10000) THEN                                                    ! 1-4 digits
            cStr(5:8) = Char4Digits(Number)
            Start = 5
            IF (cStr(Start:Start) == '0') THEN
                Start = 6
                IF (cStr(Start:Start) == '0') THEN
                    Start = 7
                    IF (cStr(Start:Start) == '0') THEN
                        Start = 8
                    END IF
                END IF
            END IF
        ELSE                                                                        ! 5-8 digits
            AABB = INT(SHIFTR(INT(Number, KIND=I8B)*109951163_I8B, 40), KIND=I4B)   ! Number / 10000
            CCDD = Number - AABB*10000                                              ! MOD(Number, 10000)
            cStr(1:4) = Char4Digits(AABB)
            cStr(5:8) = Char4Digits(CCDD)
            Start = 1
            IF (cStr(Start:Start) == '0') THEN
                Start = 2
                IF (cStr(Start:Start) == '0') THEN
                    Start = 3
                    IF (cStr(Start:Start) == '0') THEN
                        Start = 4
                    END IF
                END IF
            END IF
        END IF

        RETURN

    END FUNCTION Write_1_to_8_Digits

!**************************************************************************

    FUNCTION Write_5_to_8_Digits(Number, cStr) RESULT(Start)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_5_to_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 5 to 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B)                  :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: AABB, CCDD

    !** FLOW

        AABB = INT(SHIFTR(INT(Number, KIND=I8B)*109951163_I8B, 40), KIND=I4B)   ! Number / 10000
        CCDD = Number - AABB*10000                                              ! MOD(Number, 10000)
        cStr(1:4) = Char4Digits(AABB)
        cStr(5:8) = Char4Digits(CCDD)
        Start = 1
        IF (cStr(Start:Start) == '0') THEN
            Start = 2
            IF (cStr(Start:Start) == '0') THEN
                Start = 3
                IF (cStr(Start:Start) == '0') THEN
                    Start = 4
                END IF
            END IF
        END IF

        RETURN

    END FUNCTION Write_5_to_8_Digits

!**************************************************************************

END FUNCTION I64_ToChar_YYLL

!******************************************************************************

FUNCTION I64_ToChar_JEA(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 64-bit integer to a decimal string using JEA algorithm

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN) :: Number   ! number
    CHARACTER(LEN=:), ALLOCATABLE :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 20
    REAL(KIND=QP),     PARAMETER  :: R1  = 1.0_QP
    REAL(KIND=QP),     PARAMETER  :: R2  = 2.0_QP
    INTEGER(KIND=I8B), PARAMETER  :: K24 = INT(R2**24/1.0E2_QP + R1, KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: K32 = INT(R2**32/1.0E4_QP + R1, KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: K48 = INT(R2**48/1.0E6_QP + R1, KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: K57 = INT(R2**57/1.0E8_QP + R1, KIND=I8B)
    INTEGER(KIND=I8B), PARAMETER  :: M24 = SHIFTL(1_I8B, 24) - 1_I8B
    INTEGER(KIND=I8B), PARAMETER  :: M32 = SHIFTL(1_I8B, 32) - 1_I8B
    INTEGER(KIND=I8B), PARAMETER  :: M57 = SHIFTL(1_I8B, 57) - 1_I8B
    INTEGER(KIND=I8B), PARAMETER  :: MaxU32 = INT(Z'00000000FFFFFFFF', KIND=I8B) !  4,294,967,296

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=MaxLen)         :: wStr
    INTEGER(KIND=I8B)             :: PosNum
    INTEGER(KIND=I4B)             :: Finish, Start
    INTEGER(KIND=I8B)             :: F0, F2, F4, F6, F8
    INTEGER(KIND=I8B)             :: Z, Y

!** FLOW

    ! set positive number
    PosNum = ABS(Number)

    ! start the conversion
    IF (PosNum < 100_I8B) THEN                          ! 1-2 digits
        wStr(1:2) = Char2Digits(PosNum)
        Finish = 2
    ELSEIF (PosNum < 1000000_I8B) THEN                  ! 3-6 digits
        IF (PosNum < 10000_I8B) THEN                    ! 3-4 digits
            CALL Write_4_Digits(PosNum, wStr(1:4))
            Finish = 4
        ELSE                                            ! 5-6 digits
            CALL Write_6_Digits(PosNum, wStr(1:6))
            Finish = 6
        END IF
    ELSEIF (PosNum < MaxU32)    THEN                    ! 7-10 digits
        IF (PosNum < 100000000_I8B) THEN                ! 7-8 digits
            CALL Write_8_Digits(PosNum, wStr(1:8))
            Finish = 8
        ELSE                                            ! 9-10 digits
            CALL Write_10_Digits(PosNum, wStr(1:10))
            Finish = 10
        END IF
    ELSE
        Z = MOD(PosNum, 100000000_I8B)
        PosNum = PosNum/100000000_I8B
        IF (PosNum < 100_I8B) THEN                      ! 1-2 digits
            wStr(1:2) = Char2Digits(PosNum)
            Start = 3
        ELSEIF (PosNum < 1000000_I8B) THEN              ! 3-6 digits
            IF (PosNum < 10000_I8B) THEN                ! 3-4 digits
                CALL Write_4_Digits(PosNum, wStr(1:4))
                Start = 5
            ELSE                                        ! 5-6 digits
                CALL Write_6_Digits(PosNum, wStr(1:6))
                Start = 7
            END IF
        ELSEIF (PosNum < 100000000_I8B) THEN            ! 7-8 digits
            CALL Write_8_Digits(PosNum, wStr(1:8))
            Start = 9
        ELSEIF (PosNum < MaxU32)    THEN                ! 9-10 digits
            CALL Write_10_Digits(PosNum, wStr(1:10))
            Start = 11
        ELSE
            Y = MOD(PosNum, 100000000_I8B)
            PosNum = PosNum/100000000_I8B
            ! n is 2, 3, or 4 digits (if n < 10 it would have been handled above)
            IF (PosNum < 100_I8B) THEN                  ! 1-2 digits
                wStr(1:2) = Char2Digits(PosNum)
                Start = 3
            ELSE
                F0 = K24*PosNum
                F2 = IAND(F0, M24)*100_I8B
                wStr(1:2) = Char2Digits(SHIFTR(F0, 24))
                wStr(3:4) = Char2Digits(SHIFTR(F2, 24))
                Start = 5
            END IF
            ! do 8 digits
            Finish = Start + 7
            CALL Write_8_Digits(Y, wStr(Start:Finish))
            Start = Finish + 1
        END IF
        ! do 8 digits
        Finish = Start + 7
        CALL Write_8_Digits(Z, wStr(Start:Finish))
    END IF

    Start = 1
    IF (wStr(1:1) == '0') Start = 2

    IF (Number < 0) THEN
        IF (Number == MinI64) THEN
            cStr = '-9223372036854775808'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Write_4_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_4_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        F0 = K24*Number
        F2 = IAND(F0, M24)*100_I8B
        cStr(1:2) = Char2Digits(SHIFTR(F0, 24))
        cStr(3:4) = Char2Digits(SHIFTR(F2, 24))

        RETURN

    END SUBROUTINE Write_4_Digits

!**************************************************************************

    SUBROUTINE Write_6_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_6_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 6

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        F0 = K32*Number
        F2 = IAND(F0, M32)*100_I8B
        F4 = IAND(F2, M32)*100_I8B
        cStr(1:2) = Char2Digits(SHIFTR(F0, 32))
        cStr(3:4) = Char2Digits(SHIFTR(F2, 32))
        cStr(5:6) = Char2Digits(SHIFTR(F4, 32))

        RETURN

    END SUBROUTINE Write_6_Digits

!**************************************************************************

    SUBROUTINE Write_8_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: AABB, CCDD       ! working variables

    !** FLOW

        AABB = INT(SHIFTR(Number*109951163_I8B, 40), KIND=I4B)  ! Number / 10000
        CCDD = INT(Number - AABB*10000, KIND=I4B)               ! MOD(Number, 10000)
        cStr(1:4) = Char4Digits(AABB)
        cStr(5:8) = Char4Digits(CCDD)

! Note: the following statements (original code by JEA algorithm)
!       do not write correct numbers sometimes
!        F0 = SHIFTR(K48*Number, 16)
!        F2 = IAND(F0, M32)*100_I8B
!        F4 = IAND(F2, M32)*100_I8B
!        F6 = IAND(F4, M32)*100_I8B
!        cStr(1:2) = Char2Digits(SHIFTR(F0, 32))
!        cStr(3:4) = Char2Digits(SHIFTR(F2, 32))
!        cStr(5:6) = Char2Digits(SHIFTR(F4, 32))
!        cStr(7:8) = Char2Digits(SHIFTR(F6, 32))

        RETURN

    END SUBROUTINE Write_8_Digits

!**************************************************************************

    SUBROUTINE Write_10_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_10_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),  INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        F0 = K57*Number
        F2 = IAND(F0, M57)*100_I8B
        F4 = IAND(F2, M57)*100_I8B
        F6 = IAND(F4, M57)*100_I8B
        F8 = IAND(F6, M57)*100_I8B
        cStr(1:2)  = Char2Digits(SHIFTR(F0, 57))
        cStr(3:4)  = Char2Digits(SHIFTR(F2, 57))
        cStr(5:6)  = Char2Digits(SHIFTR(F4, 57))
        cStr(7:8)  = Char2Digits(SHIFTR(F6, 57))
        cStr(9:10) = Char2Digits(SHIFTR(F8, 57))

        RETURN

    END SUBROUTINE Write_10_Digits

!**************************************************************************

END FUNCTION I64_ToChar_JEA

!******************************************************************************

END MODULE ModBase_Integer_ToChar

!******************************************************************************
