
MODULE ModTest_RealSP_CharConv

!** PURPOSE OF THIS MODULE:
    ! This module contains routines that test functions from 'ModBase_RealSP_CharConv' module.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_RealSP_CharConv
    USE ModBase_Integer_ToChar,     ONLY: ToChar => I32_ToChar_CC
    USE ModTest_RealQP_CharConv,    ONLY: GetWriteFormat, ListDr, G_Desc, E_Desc, &
                                          GeneralFmt, ScientificFmt, FString
    USE Class_Mt32RNG
    USE Class_Timer
    USE Class_ProgressBar

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: RoundTripTest_RealSP
    PUBLIC :: BenchTest_RealSP_ToString
    PUBLIC :: BenchTest_RealSP_FromString

    PRIVATE          ! by default, hide all data and routines except those declared explicitly
    
!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Real32_ToChar(Number, IsScientific) RESULT(cStr)
            IMPORT
            REAL(KIND=SP),     INTENT(IN)  :: Number
            LOGICAL, OPTIONAL, INTENT(IN)  :: IsScientific
            CHARACTER(LEN=:), ALLOCATABLE  :: cStr
        END FUNCTION
        FUNCTION Real32_FromChar(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            IMPORT
            CHARACTER(LEN=*),                        INTENT(IN)    :: cStr
            INTEGER(KIND=I4B),             OPTIONAL, INTENT(IN)    :: ParseOpt
            LOGICAL,                       OPTIONAL, INTENT(OUT)   :: ErrFlag
            CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg
            REAL(KIND=SP)                                          :: Number
        END FUNCTION
    END INTERFACE

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!------------------------------------------------------------------------------
!
!                       TESTING ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE RoundTripTest_RealSP()

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform the so-called round-trip test to check the correctness of write-read cycle of
    !   various combinations of "WRITE/ToString" and "READ/FromString" functions.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
    
!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! na
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER    :: TotNum = 200000
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FString), ALLOCATABLE  :: FStr(:)
    REAL(KIND=SP), ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)           :: TestNum
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++           RoundTripTest_RealSP Started.               +++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    
    ! allocate working variables
    ALLOCATE(FStr(TotNum))
    ALLOCATE(RVal(TotNum))
    
    DO TestNum = 1, 9
        ! generate random numbers
        SELECT CASE (TestNum)
        CASE (1)
            !  subnormal range
            CALL Generate_R32_SubNormal(TotNum, RVal)
        CASE (2)
            ! normal (single) range
            CALL Generate_R32_Normal(TotNum, RVal)
        CASE (3)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, -38, -21, RVal)
        CASE (4)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, -20, -11, RVal)
        CASE (5)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, -10, -1, RVal)
        CASE (6)
            ! single range
            CALL Generate_R32_ZeroToOne(TotNum, RVal)
        CASE (7)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, 1, 10, RVal)
        CASE (8)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, 11, 20, RVal)
        CASE (9)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, 21, 38, RVal)
        END SELECT
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++                   Internal Write                       +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        ! test internal writes with READ statement and FromString functions
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE List-DR statement', ListDr)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE G-Dest (G0) statement', G_Desc)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE G-Dest (G0.8) statement', G_Desc, 0, 8)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE G-Dest (G0.9) statement', G_Desc, 0, 9)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE E-Dest (E0.8E2) statement', E_Desc, 0, 8, 2)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE E-Dest (E0.9E2) statement', E_Desc, 0, 9, 2)
        ! test ToChar/ToString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++           ToString_Conversion - General Format         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real32_ToChar(TotNum, FStr, 'Dragonbox - General', RealSP_ToString_DragonBox, GeneralFmt)
        CALL Test_Real32_ToChar(TotNum, FStr, 'Ryu       - General', RealSP_ToString_Ryu,       GeneralFmt)
        CALL Test_Real32_ToChar(TotNum, FStr, 'Schubfach - General', RealSP_ToString_Schubfach, GeneralFmt)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++        ToString_Conversion - Scientific Format         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real32_ToChar(TotNum, FStr, 'Dragonbox - Scientific', RealSP_ToString_DragonBox, ScientificFmt)
        CALL Test_Real32_ToChar(TotNum, FStr, 'Ryu       - Scientific', RealSP_ToString_Ryu,       ScientificFmt)
        CALL Test_Real32_ToChar(TotNum, FStr, 'Schubfach - Scientific', RealSP_ToString_Schubfach, ScientificFmt)
        WRITE(*,*) ' '
        PAUSE
    END DO
    
    RETURN

CONTAINS

    SUBROUTINE Test_Internal_Write(TotalLoop, OutStr, TestName, WriteFormat, Width, Digits, Exponent)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-write conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)           :: TotalLoop
        TYPE(FString),     INTENT(OUT)          :: OutStr(TotalLoop)
        CHARACTER(LEN=*),  INTENT(IN)           :: TestName
        INTEGER(KIND=I4B), INTENT(IN)           :: WriteFormat
        INTEGER(KIND=I4B), INTENT(IN), OPTIONAL :: Width
        INTEGER(KIND=I4B), INTENT(IN), OPTIONAL :: Digits
        INTEGER(KIND=I4B), INTENT(IN), OPTIONAL :: Exponent
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CHARACTER(LEN=40)               :: CStr
        TYPE(ProgressBar)               :: Bar
        INTEGER(KIND=I4B)               :: OutLoop, InLoop
        INTEGER(KIND=I4B)               :: I, J, Indx, IErr
        CHARACTER(LEN=:), ALLOCATABLE   :: Fmt
    
    !** FLOW:
        
        ! get run-time format
        Fmt = GetWriteFormat(WriteFormat, Width, Digits, Exponent)

        WRITE(*,*) 'Start testing =: ' // TestName
        OutLoop = 100
        InLoop = TotalLoop / OutLoop
        Indx = 1

        ! set up progress bar
        CALL Bar%Setup(Progress_Delimited, Prefix = "Progress: ", Show = Progress_Value_Max, &
                        MaxVal = REAL(OutLoop, KIND=SP))
        
        ! perform internal write
        DO J = 1, OutLoop
            DO I = 1, InLoop
                SELECT CASE (WriteFormat)
                CASE (ListDr)
                    WRITE(CStr, FMT=*, IOSTAT=IErr) RVal(Indx)
                CASE (G_Desc)
                    WRITE(CStr, FMT=Fmt, IOSTAT=IErr) RVal(Indx)
                CASE (E_Desc)
                    WRITE(CStr, FMT=Fmt, IOSTAT=IErr) RVal(Indx)
                END SELECT
                OutStr(Indx)%cStr = TRIM(ADJUSTL(CStr))
                Indx = Indx + 1
            END DO
            CALL Bar%Update(REAL(J, KIND=SP))
        END DO

        ! perform Internal Read / FromString conversions
        CALL Check_Results(TotalLoop, OutStr, TestName)

        RETURN
    END SUBROUTINE Test_Internal_Write

    !--------------------------------------------------------------------------

    SUBROUTINE Test_Real32_ToChar(TotalLoop, OutStr, TestName, TestFunc, IsScientific)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check real-tochar conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: TotalLoop
        TYPE(FString),     INTENT(OUT)  :: OutStr(TotalLoop)
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        LOGICAL,           INTENT(IN)   :: IsScientific
        PROCEDURE(Real32_ToChar)       :: TestFunc
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(ProgressBar)   :: Bar
        INTEGER(KIND=I4B)   :: OutLoop, InLoop
        INTEGER(KIND=I4B)   :: I, J, Indx, IErr
    
    !** FLOW:

        WRITE(*,*) 'Start testing =: ' // TestName
        OutLoop = 100
        InLoop = TotalLoop / OutLoop
        Indx = 1

        ! set up progress bar
        CALL Bar%Setup(Progress_Delimited, Prefix = "Progress: ", Show = Progress_Value_Max, &
                        MaxVal = REAL(OutLoop, KIND=SP))

        ! perform ToString conversions
        DO J = 1, OutLoop
            DO I = 1, InLoop
                OutStr(Indx)%cStr = TestFunc(RVal(Indx), IsScientific)
                Indx = Indx + 1
            END DO
            CALL Bar%Update(REAL(J, KIND=SP))
        END DO

        ! perform Internal Read / FromString conversions
        CALL Check_Results(TotalLoop, OutStr, TestName)

        RETURN
    END SUBROUTINE Test_Real32_ToChar

    !--------------------------------------------------------------------------

    SUBROUTINE Check_Results(TotalLoop, OutStr, TestName)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: TotalLoop
        TYPE(FString),     INTENT(IN)   :: OutStr(TotalLoop)
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na
    
    !** FLOW:

        ! perform internal read
        WRITE(*,*) 'Check internal read'
        CALL Check_InternalRead_Results(TotalLoop, OutStr, TestName)
        
        ! perform FromString conversions
        WRITE(*,*) 'Check FastFloat'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'FastFloat', RealSP_FromString_FastFloat)
        SELECT CASE (TestNum)
        CASE (2, 4:9)
            ! for subnormal and normal with Exp < -2000, LibC is very very very slow for "200000" test data
            !   (i.e. when Eisel-Lemire is not valid)
            WRITE(*,*) 'Check LibC'
            CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'LibC     ', RealSP_FromString_LibC)
        END SELECT
        WRITE(*,*) 'Check YY'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'YY       ', RealSP_FromString_YY)
        WRITE(*,*) 'Check Lemire'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'Lemire   ', RealSP_FromString_Lemire)
        WRITE(*,*) '-----------------------------------------------------------'
        
        RETURN
    END SUBROUTINE Check_Results

    !--------------------------------------------------------------------------
    SUBROUTINE Check_InternalRead_Results(TotalLoop, OutStr, TestName1)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal read results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: TotalLoop
        TYPE(FString),     INTENT(IN)   :: OutStr(TotalLoop)
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName1    ! name of ToString

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)               :: IErr
        INTEGER(KIND=I4B)               :: Count, I
        REAL(KIND=SP)                   :: ROut, AccErr, RelErr
        CHARACTER(LEN=:), ALLOCATABLE   :: TestName
    
    !** FLOW:

        ! initialize
        Count = 0
        AccErr = 0.0E0
        
        DO I = 1, TotalLoop
            READ(OutStr(I)%cStr, FMT=*, IOSTAT=IErr) ROut
            IF (RVal(I) /= 0.0E0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
            IF (RelErr > EPSILON(1.0E0)*10.0E0) Count = Count + 1
        END DO
        TestName = 'ToString: ' // TestName1 // '   FromString: Internal Read'
        IF (Count /= 0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Err Count = ', Count
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSEIF (AccErr /= 0.0E0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        END IF
        
        RETURN
    END SUBROUTINE Check_InternalRead_Results

    !--------------------------------------------------------------------------

    SUBROUTINE Check_FromString_Results(TotalLoop, OutStr, TestName1, TestName2, TestFunc)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check FromString conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: TotalLoop
        TYPE(FString),     INTENT(IN)   :: OutStr(TotalLoop)
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName1    ! name of ToString
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName2    ! name of FromString
        PROCEDURE(Real32_FromChar)     :: TestFunc

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)               :: Count, I
        REAL(KIND=SP)                   :: ROut, AccErr, RelErr
        CHARACTER(LEN=:), ALLOCATABLE   :: TestName
    
    !** FLOW:

        ! initialize
        Count = 0
        AccErr = 0.0E0
        
        DO I = 1, TotalLoop
            ROut = TestFunc(OutStr(I)%cStr, FPlusNum)
            IF (RVal(I) /= 0.0E0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
            IF (RelErr > EPSILON(1.0E0)*10.0E0) Count = Count + 1
        END DO
        TestName = 'ToString: ' // TestName1 // '   FromString: ' // TestName2
        IF (Count /= 0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Err Count = ', Count
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSEIF (AccErr /= 0.0E0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        END IF
        
        RETURN
    END SUBROUTINE Check_FromString_Results

    !--------------------------------------------------------------------------

END SUBROUTINE RoundTripTest_RealSP

!******************************************************************************

SUBROUTINE BenchTest_RealSP_ToString()

!** PURPOSE OF THIS SUBROUTINE:
    ! To test performance and correctness (write-read cycle) of Real_ToString functions.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
    
!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! na
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: TotNum = 200000
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=40),  ALLOCATABLE  :: CStr(:)
    TYPE(FString),      ALLOCATABLE  :: FStr(:)
    REAL(KIND=SP),      ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)                :: K
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++       BenchTest_RealSP_ToString Started.              +++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    
    ! allocate working variables
    ALLOCATE(CStr(TotNum))
    ALLOCATE(FStr(TotNum))
    ALLOCATE(RVal(TotNum))
    
    DO K = 1, 9
        ! generate random numbers
        SELECT CASE (K)
        CASE (1)
            !  subnormal range
            CALL Generate_R32_SubNormal(TotNum, RVal)
        CASE (2)
            ! normal (single) range
            CALL Generate_R32_Normal(TotNum, RVal)
        CASE (3)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, -38, -21, RVal)
        CASE (4)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, -20, -11, RVal)
        CASE (5)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, -10, -1, RVal)
        CASE (6)
            ! single range
            CALL Generate_R32_ZeroToOne(TotNum, RVal)
        CASE (7)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, 1, 10, RVal)
        CASE (8)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, 11, 20, RVal)
        CASE (9)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, 21, 38, RVal)
        END SELECT
        ! test internal writes
        CALL Test_Internal_Write('WRITE List-DR statement', '*')
        CALL Test_Internal_Write('WRITE G-Dest (G0) statement', '(G0)')
        CALL Test_Internal_Write('WRITE G-Dest (G0.8) statement', '(G0.8)')
        CALL Test_Internal_Write('WRITE G-Dest (G0.9) statement', '(G0.9)')
        CALL Test_Internal_Write('WRITE E-Dest (E0.8E2) statement', '(E0.8E2)')
        CALL Test_Internal_Write('WRITE E-Dest (E0.9E2) statement', '(E0.9E2)')
        ! test ToChar/ToString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++           String_Conversion - General Format           +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real32_ToChar('DummyCall - General', RealSP_ToString_DragonBox, IsScientific=FalseVal)
        CALL Test_Real32_ToChar('Dragonbox - General', RealSP_ToString_DragonBox, IsScientific=FalseVal)
        CALL Test_Real32_ToChar('Ryu       - General', RealSP_ToString_Ryu,       IsScientific=FalseVal)
        CALL Test_Real32_ToChar('Schubfach - General', RealSP_ToString_Schubfach, IsScientific=FalseVal)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++        String_Conversion - Scientific Format           +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real32_ToChar('Dragonbox - Scientific', RealSP_ToString_DragonBox, IsScientific=TrueVal)
        CALL Test_Real32_ToChar('Ryu       - Scientific', RealSP_ToString_Ryu,       IsScientific=TrueVal)
        CALL Test_Real32_ToChar('Schubfach - Scientific', RealSP_ToString_Schubfach, IsScientific=TrueVal)
        WRITE(*,*) ' '
        PAUSE
    END DO
    
    RETURN

CONTAINS

    SUBROUTINE Test_Internal_Write(TestName, Fmt)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-write conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*), INTENT(IN)   :: TestName
        CHARACTER(LEN=*), INTENT(IN)   :: Fmt
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(ProgressBar)               :: Bar
        TYPE(Timer)                     :: Clock
        INTEGER(KIND=I4B)               :: OutLoop, InLoop
        INTEGER(KIND=I4B)               :: I, J, Indx, IErr
        CHARACTER(LEN=20)               :: ExeTime
        CHARACTER(LEN=:), ALLOCATABLE   :: ReportTime
    
    !** FLOW:

        WRITE(*,*) 'Start testing ' // TestName
        OutLoop = 100
        InLoop = TotNum / OutLoop
        Indx = 1

        ! perform bench test
        CALL Bar%Setup(Progress_Delimited, Prefix = "Progress: ", Show = Progress_Value_Max, &
                        MaxVal = REAL(OutLoop, KIND=SP))
        IF (Fmt == '*') THEN
            CALL Clock%Start()
            DO J = 1, OutLoop
                DO I = 1, InLoop
                    WRITE(CStr(Indx), FMT=*, IOSTAT=IErr) RVal(Indx)
                    Indx = Indx + 1
                END DO
                CALL Bar%Update(REAL(J, KIND=SP))
            END DO
            CALL Clock%Stop()
        ELSE
            CALL Clock%Start()
            DO J = 1, OutLoop
                DO I = 1, InLoop
                    WRITE(CStr(Indx), FMT=Fmt, IOSTAT=IErr) RVal(Indx)
                    Indx = Indx + 1
                END DO
                CALL Bar%Update(REAL(J, KIND=SP))
            END DO
            CALL Clock%Stop()
        END IF
        
        ! report bench time
        WRITE(ExeTime,'(F15.5)') Clock%ElapsedTime(MILLI_SEC)
        ReportTime = '          Bench Time = ' // TRIM(ADJUSTL(ExeTime)) // ' Milli Seconds'
        WRITE(*,*) ReportTime
        
        ! check results
        CALL Check_Write_Results(TotNum, TestName)

        RETURN
    END SUBROUTINE Test_Internal_Write

    !--------------------------------------------------------------------------

    SUBROUTINE Test_Real32_ToChar(TestName, TestFunc, IsScientific)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check real-tochar conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        LOGICAL, OPTIONAL, INTENT(IN)   :: IsScientific
        PROCEDURE(Real32_ToChar)        :: TestFunc
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(ProgressBar)               :: Bar
        TYPE(Timer)                     :: Clock
        INTEGER(KIND=I4B)               :: OutLoop, InLoop
        INTEGER(KIND=I4B)               :: I, J, Indx, IErr
        CHARACTER(LEN=20)               :: ExeTime
        CHARACTER(LEN=:), ALLOCATABLE   :: ReportTime
    
    !** FLOW:

        WRITE(*,*) 'Start testing ' // TestName
        OutLoop = 100
        InLoop = TotNum / OutLoop
        Indx = 1

        ! perform bench test
        CALL Bar%Setup(Progress_Delimited, Prefix = "Progress: ", Show = Progress_Value_Max, &
                        MaxVal = REAL(OutLoop, KIND=SP))
        CALL Clock%Start()
        DO J = 1, OutLoop
            DO I = 1, InLoop
                FStr(Indx)%cStr = TestFunc(RVal(Indx), IsScientific)
                Indx = Indx + 1
            END DO
            CALL Bar%Update(REAL(J, KIND=SP))
        END DO
        CALL Clock%Stop()

        ! report bench time
        WRITE(ExeTime,'(F15.5)') Clock%ElapsedTime(MILLI_SEC)
        ReportTime = '          Bench Time = ' // TRIM(ADJUSTL(ExeTime)) // ' Milli Seconds'
        WRITE(*,*) ReportTime
        
        ! check results
        CALL Check_Q2S_Results(TotNum, TestName)

        RETURN
    END SUBROUTINE Test_Real32_ToChar

    !--------------------------------------------------------------------------

    SUBROUTINE Check_Write_Results(Loop, TestName)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-write conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: Loop
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: IErr
        INTEGER(KIND=I4B)   :: I
        REAL(KIND=SP)       :: ROut, AccErr, RelErr
    
    !** FLOW:

        ! initialize
        AccErr = 0.0E0
        
        DO I = 1, Loop
            READ(CStr(I), FMT=*, IOSTAT=IErr) ROut
            IF (RVal(I) /= 0.0E0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
        END DO
        IF (AccErr /= 0.0E0) THEN
            WRITE(*,*) '          ' // TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSE
            WRITE(*,*) '          ' // TestName // ' - SUCCESS'
        END IF
        
        RETURN
    END SUBROUTINE Check_Write_Results

    !--------------------------------------------------------------------------

    SUBROUTINE Check_Q2S_Results(Loop, TestName)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check R32-to-string conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: Loop
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: I
        REAL(KIND=SP)       :: ROut, AccErr, RelErr
    
    !** FLOW:

        ! initialize
        AccErr = 0.0E0
        
        DO I = 1, Loop
            ROut = RealSP_FromString_FastFloat(FStr(I)%cStr)
            IF (RVal(I) /= 0.0E0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
        END DO
        IF (AccErr /= 0.0E0) THEN
            WRITE(*,*) '          ' // TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSE
            WRITE(*,*) '          ' // TestName // ' - SUCCESS'
        END IF
        
        RETURN
    END SUBROUTINE Check_Q2S_Results

!--------------------------------------------------------------------------

END SUBROUTINE BenchTest_RealSP_ToString

!******************************************************************************

SUBROUTINE BenchTest_RealSP_FromString()

!** PURPOSE OF THIS SUBROUTINE:
    ! To test performance and correctness (write-read cycle) of Real_FromString functions.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
    
!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! na
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: TotNum = 200000
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=40),  ALLOCATABLE  :: CStr(:)
    TYPE(FString),      ALLOCATABLE  :: FStr(:)
    REAL(KIND=SP),      ALLOCATABLE  :: RRef(:)
    REAL(KIND=SP),      ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)                :: K, L, IErr
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++       BenchTest_RealSP_FromString Started.            +++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    
    ! allocate working variables
    ALLOCATE(CStr(TotNum))
    ALLOCATE(FStr(TotNum))
    ALLOCATE(RRef(TotNum))
    ALLOCATE(RVal(TotNum))
    
    DO K = 1, 9
        ! generate random numbers
        SELECT CASE (K)
        CASE (1)
            !  subnormal range
            CALL Generate_R32_SubNormal(TotNum, RRef)
        CASE (2)
            ! normal (single) range
            CALL Generate_R32_Normal(TotNum, RRef)
        CASE (3)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, -38, -21, RRef)
        CASE (4)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, -20, -11, RRef)
        CASE (5)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, -10, -1, RRef)
        CASE (6)
            ! single range
            CALL Generate_R32_ZeroToOne(TotNum, RRef)
        CASE (7)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, 1, 10, RRef)
        CASE (8)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, 11, 20, RRef)
        CASE (9)
            ! single range
            CALL Generate_R32_ExpRange(TotNum, 21, 38, RRef)
        END SELECT
        ! test internal reads
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++       String_Conversion - READ from WRITE E0.9E2      +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        DO L = 1, TotNum
            WRITE(CStr(L), FMT='(E0.9E2)', IOSTAT=IErr) RRef(L)
            FStr(L)%cStr = TRIM(ADJUSTL(CStr(L)))
        END DO
        CALL Test_Internal_Read('READ from WRITE E-Dest (E0.9E2)')
        CALL Test_Real32_FromChar('FastFloat - WRITE E-Dest', RealSP_FromString_FastFloat, FPlusNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real32_FromChar('LibC      - WRITE E-Dest', RealSP_FromString_LibC,      FPlusNum)
        END SELECT
        CALL Test_Real32_FromChar('YY        - WRITE E-Dest', RealSP_FromString_YY,        FPlusNum)
        CALL Test_Real32_FromChar('Lemire    - WRITE E-Dest', RealSP_FromString_Lemire,    FPlusNum)
        ! test FromChar/FromString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++       String_Conversion - Parse Fortran Number         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        ! generate input string
        DO L = 1, TotNum
            FStr(L)%cStr = RealSP_ToString_DragonBox(RRef(L))
        END DO
        CALL Test_Real32_FromChar('FastFloat - Fortran', RealSP_FromString_FastFloat, FortNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real32_FromChar('LibC      - Fortran', RealSP_FromString_LibC,      FortNum)
        END SELECT
        CALL Test_Real32_FromChar('YY        - Fortran', RealSP_FromString_YY,        FortNum)
        CALL Test_Real32_FromChar('Lemire    - Fortran', RealSP_FromString_Lemire,    FortNum)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++    String_Conversion - Parse FortranPlus Number        +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real32_FromChar('FastFloat - FortPlus', RealSP_FromString_FastFloat, FPlusNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real32_FromChar('LibC      - FortPlus', RealSP_FromString_LibC,      FPlusNum)
        END SELECT
        CALL Test_Real32_FromChar('YY        - FortPlus', RealSP_FromString_YY,        FPlusNum)
        CALL Test_Real32_FromChar('Lemire    - FortPlus', RealSP_FromString_Lemire,    FPlusNum)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++        String_Conversion - Parse JSON Number           +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real32_FromChar('FastFloat - JSON', RealSP_FromString_FastFloat, JsonNum)
!        SELECT CASE (K)
!        CASE (2, 4:9)
            CALL Test_Real32_FromChar('LibC      - JSON', RealSP_FromString_LibC,      JsonNum)
!        END SELECT
        CALL Test_Real32_FromChar('YY        - JSON', RealSP_FromString_YY,        JsonNum)
        CALL Test_Real32_FromChar('Lemire    - JSON', RealSP_FromString_Lemire,    JsonNum)
        WRITE(*,*) ' '
        PAUSE
    END DO
    
    RETURN

CONTAINS

    SUBROUTINE Test_Internal_Read(TestName)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-read conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*), INTENT(IN)   :: TestName
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(ProgressBar)               :: Bar
        TYPE(Timer)                     :: Clock
        INTEGER(KIND=I4B)               :: OutLoop, InLoop
        INTEGER(KIND=I4B)               :: I, J, Indx, IErr
        CHARACTER(LEN=20)               :: ExeTime
        CHARACTER(LEN=:), ALLOCATABLE   :: ReportTime
    
    !** FLOW:

        WRITE(*,*) 'Start testing ' // TestName
        OutLoop = 100
        InLoop = TotNum / OutLoop
        Indx = 1

        CALL Bar%Setup(Progress_Delimited, Prefix = "Progress: ", Show = Progress_Value_Max, &
                        MaxVal = REAL(OutLoop, KIND=SP))
        
        ! perform bench test
        CALL Clock%Start()
        DO J = 1, OutLoop
            DO I = 1, InLoop
                READ(CStr(Indx), FMT=*, IOSTAT=IErr) RVal(Indx)
                Indx = Indx + 1
            END DO
            CALL Bar%Update(REAL(J, KIND=SP))
        END DO
        CALL Clock%Stop()

        ! report bench time
        WRITE(ExeTime,'(F15.5)') Clock%ElapsedTime(MILLI_SEC)
        ReportTime = '          Bench Time = ' // TRIM(ADJUSTL(ExeTime)) // ' Milli Seconds'
        WRITE(*,*) ReportTime
        
        ! check results
        CALL Check_Results(TotNum, TestName)

        RETURN
    END SUBROUTINE Test_Internal_Read

    !--------------------------------------------------------------------------

    SUBROUTINE Test_Real32_FromChar(TestName, TestFunc, ParseOpt)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check from-string conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        PROCEDURE(Real32_FromChar)      :: TestFunc
        INTEGER(KIND=I4B), INTENT(IN)   :: ParseOpt

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(ProgressBar)               :: Bar
        TYPE(Timer)                     :: Clock
        INTEGER(KIND=I4B)               :: OutLoop, InLoop
        INTEGER(KIND=I4B)               :: I, J, Indx, IErr
        CHARACTER(LEN=20)               :: ExeTime
        CHARACTER(LEN=:), ALLOCATABLE   :: ReportTime
    
    !** FLOW:

        WRITE(*,*) 'Start testing ' // TestName
        OutLoop = 100
        InLoop = TotNum / OutLoop
        Indx = 1

        ! perform bench test
        CALL Bar%Setup(Progress_Delimited, Prefix = "Progress: ", Show = Progress_Value_Max, &
                        MaxVal = REAL(OutLoop, KIND=SP))
        CALL Clock%Start()
        DO J = 1, OutLoop
            DO I = 1, InLoop
                RVal(Indx) = TestFunc(FStr(Indx)%cStr, ParseOpt)
                Indx = Indx + 1
            END DO
            CALL Bar%Update(REAL(J, KIND=SP))
        END DO
        CALL Clock%Stop()

        ! report bench time
        WRITE(ExeTime,'(F15.5)') Clock%ElapsedTime(MILLI_SEC)
        ReportTime = '          Bench Time = ' // TRIM(ADJUSTL(ExeTime)) // ' Milli Seconds'
        WRITE(*,*) ReportTime
        
        ! check results
        CALL Check_Results(TotNum, TestName)

        RETURN
    END SUBROUTINE Test_Real32_FromChar

    !--------------------------------------------------------------------------

    SUBROUTINE Check_Results(Loop, TestName)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-read / from-string conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: Loop
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: IErr
        INTEGER(KIND=I4B)   :: I
        REAL(KIND=SP)       :: AccErr, RelErr, ROut
    
    !** FLOW:

        ! initialize
        AccErr = 0.0E0
        
        DO I = 1, Loop
            IF (RRef(I) /= 0.0E0) THEN
                RelErr = ABS((RRef(I)-RVal(I))/RRef(I))
            ELSE
                RelErr = ABS((RRef(I)-RVal(I)))
            END IF
            AccErr = AccErr + RelErr
            ! This is for debugging LibC purpose.
            IF (RelErr /= 0.0E0) THEN
                OPEN(UNIT=999, FILE='LibC Debug.Txt', POSITION='APPEND' )
                WRITE(999,*)                   'FStr    = ', FStr(I)%cStr
                WRITE(999,'(A10, 1X, E0.9E2)') 'RRef    = ', RRef(I)
                WRITE(999,'(A10, 1X, E0.9E2)') 'RVal    = ', RVal(I)
                WRITE(999,'(A10, 1X, E0.9E2)') 'ROut-FF = ', RealSP_FromString_FastFloat(FStr(I)%cStr, FPlusNum)
                WRITE(999,'(A10, 1X, E0.9E2)') 'ROut-LC = ', RealSP_FromString_LibC(FStr(I)%cStr, FPlusNum)
                WRITE(999,'(A10, 1X, E0.9E2)') 'ROut-YY = ', RealSP_FromString_YY(FStr(I)%cStr, FPlusNum)
                WRITE(999,'(A10, 1X, E0.9E2)') 'ROut-LM = ', RealSP_FromString_Lemire(FStr(I)%cStr, FPlusNum)
                BLOCK
                    INTEGER(KIND=I4B)               :: IErr
                    READ(FStr(I)%cStr, FMT=*, IOSTAT=IErr) ROut
                END BLOCK
                WRITE(999,'(A10, 1X, E0.9E2)') 'ROut-IR = ', ROut
                WRITE(999,*) '-------------------------------------------------------------'
                CLOSE(999)
            END IF
        END DO
        IF (AccErr /= 0.0E0) THEN
            WRITE(*,*) '          ' // TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSE
            WRITE(*,*) '          ' // TestName // ' - SUCCESS'
        END IF
        
        RETURN
    END SUBROUTINE Check_Results

    !--------------------------------------------------------------------------

END SUBROUTINE BenchTest_RealSP_FromString

!------------------------------------------------------------------------------
!
!                       RANDOM-NUMBER GENERATION ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE Generate_R32_SubNormal(N, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the subnormal range

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)     :: N            ! number of random numbers
    REAL(KIND=SP),      INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I
    INTEGER(KIND=I4B)   :: RawFP32
    TYPE(Mt32RNG)       :: RNG
    
!** FLOW
    
    WRITE(*,*) '-------------------------------------------------------------'
    WRITE(*,*) 'Generate random  32-bit real numbers in the subnormal range. '
    WRITE(*,*) '-------------------------------------------------------------'

    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate 32-bit unsigned integer in the subnormal range
        RawFP32    = RNG%NextInteger(MinSubnormal, MaxSubnormal)
        ! convert to 32-bit floating point number
        RandNum(I) = RawFP_ToSingle(RawFP32)
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R32_SubNormal

!******************************************************************************

SUBROUTINE Generate_R32_Normal(N, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the normal range

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)     :: N            ! number of random numbers
    REAL(KIND=SP),      INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I
    INTEGER(KIND=I4B)   :: RawFP32
    TYPE(Mt32RNG)       :: RNG
    
!** FLOW
    
    WRITE(*,*) '----------------------------------------------------------'
    WRITE(*,*) 'Generate random  32-bit real numbers in the normal range. '
    WRITE(*,*) '----------------------------------------------------------'

    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate 32-bit unsigned integer in the normal range
        RawFP32    = RNG%NextInteger(MinNormal, MaxNormal)
        ! convert to 32-bit floating point number
        RandNum(I) = RawFP_ToSingle(RawFP32)
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R32_Normal

!******************************************************************************

SUBROUTINE Generate_R32_ExpRange(N, ExpLo, ExpHi, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the specified exponent range
    ! where ExpHi must be greater than ExpLo and both variables must be
    ! in the applicable (normal) exponent range [-38, 38]

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)     :: N            ! number of random numbers
    INTEGER(KIND=I4B), INTENT(IN)     :: ExpLo        ! lower bound of exponent range
    INTEGER(KIND=I4B), INTENT(IN)     :: ExpHi        ! upper bound of exponent range
    REAL(KIND=SP),     INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! exponent bounds for 'normal' range
    INTEGER(KIND=I4B), PARAMETER  :: Exponent_UppBound =  38
    INTEGER(KIND=I4B), PARAMETER  :: Exponent_LowBound = -38     ! lowest normal
    REAL(KIND=SP),     PARAMETER  :: TenSingle = 10.0E0

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I, ExpLow, ExpUpp, Exp
    REAL(KIND=SP)       :: Rand
    TYPE(Mt32RNG)       :: RNG
    
!** FLOW
    
    ! first check whether ExpLo is greater than ExpHi or not
    ! and set ExpLow to the lower value while set ExpUpp to the higher value
    IF (ExpLo > ExpHi) THEN
        ExpLow = ExpHi
        ExpUpp = ExpLo
    ELSEIF (ExpLo == ExpHi) THEN
        ExpLow = ExpLo - 1
        ExpUpp = ExpHi + 1
    ELSE
        ExpLow = ExpLo
        ExpUpp = ExpHi
    END IF
    
    ! then check whether the exponent bounds are in the applicable range
    ! and set them to the limits if they are out of the applicable range
    IF (ExpLow < Exponent_LowBound) ExpLow = Exponent_LowBound
    IF (ExpUpp > Exponent_UppBound) ExpUpp = Exponent_UppBound

    WRITE(*,*) '----------------------------------------------------------------------'
    WRITE(*,*) 'Generate random  32-bit real numbers in the specified exponent range. '
    WRITE(*,*) 'ExpLow = ', ExpLow, ' and ExpUpp = ', ExpUpp
    WRITE(*,*) '----------------------------------------------------------------------'
    
    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate the random exponent in the valid range
        Exp = RNG%NextInteger(ExpLow, ExpUpp)
        ! generate the random real number in [0, 1) range
        Rand = RNG%NextSingle()
        ! set the current random number
        RandNum(I) = Rand*(TenSingle**Exp)
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R32_ExpRange

!******************************************************************************

SUBROUTINE Generate_R32_ZeroToOne(N, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the range [0.0, 1.0)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)     :: N            ! number of random numbers
    REAL(KIND=SP),      INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I
    TYPE(Mt32RNG)       :: RNG
    
!** FLOW
    
    WRITE(*,*) '--------------------------------------------------------------'
    WRITE(*,*) 'Generate random  32-bit real numbers in the range [0.0, 1.0). '
    WRITE(*,*) '--------------------------------------------------------------'
    
    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate the random real number in [0, 1) range
        RandNum(I) = RNG%NextSingle()
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R32_ZeroToOne

!******************************************************************************

FUNCTION RawFP_ToSingle(RawVal) RESULT(RealVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a raw binary floating point number into
    ! its equivalent real number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: RawVal
    REAL(KIND=SP)                   :: RealVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: IntVal
    REAL(KIND=SP)       :: FloatVal
    EQUIVALENCE(IntVal, FloatVal)

!** FLOW
    
    IntVal  = RawVal
    RealVal = FloatVal

    RETURN

END FUNCTION RawFP_ToSingle

!******************************************************************************

END MODULE ModTest_RealSP_CharConv

!******************************************************************************
