
MODULE ModTest_RealDP_CharConv

!** PURPOSE OF THIS MODULE:
    ! This module contains routines that test functions from 'ModBase_RealDP_CharConv' module.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_RealDP_CharConv
    USE ModBase_Integer_ToChar,     ONLY: ToChar => I32_ToChar_CC
    USE ModTest_RealQP_CharConv,    ONLY: GetWriteFormat, ListDr, G_Desc, E_Desc, &
                                          GeneralFmt, ScientificFmt, FString
    USE Class_Mt64RNG
    USE Class_Timer
    USE Class_ProgressBar

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: RoundTripTest_RealDP
    PUBLIC :: RoundTripTest_DragonBox64
    PUBLIC :: BenchTest_RealDP_ToString
    PUBLIC :: BenchTest_RealDP_FromString

    PRIVATE          ! by default, hide all data and routines except those declared explicitly
    
!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
	! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Real64_ToChar(Number, IsScientific) RESULT(cStr)
            IMPORT
            REAL(KIND=DP),     INTENT(IN)  :: Number
            LOGICAL, OPTIONAL, INTENT(IN)  :: IsScientific
            CHARACTER(LEN=:), ALLOCATABLE  :: cStr
        END FUNCTION
        FUNCTION Real64_FromChar(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            IMPORT
            CHARACTER(LEN=*),                        INTENT(IN)    :: cStr
            INTEGER(KIND=I4B),             OPTIONAL, INTENT(IN)    :: ParseOpt
            LOGICAL,                       OPTIONAL, INTENT(OUT)   :: ErrFlag
            CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg
            REAL(KIND=DP)                                          :: Number
        END FUNCTION
    END INTERFACE

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!------------------------------------------------------------------------------
!
!                       TESTING ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE RoundTripTest_RealDP()

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
    REAL(KIND=DP), ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)           :: TestNum
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++           RoundTripTest_RealDP Started.               +++++'
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
            CALL Generate_R64_SubNormal(TotNum, RVal)
        CASE (2)
            ! normal (double) range
            CALL Generate_R64_Normal(TotNum, RVal)
        CASE (3)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, -308, -201, RVal)
        CASE (4)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, -200, -39, RVal)
        CASE (5)
            ! single range
            CALL Generate_R64_ExpRange(TotNum, -38, -1, RVal)
        CASE (6)
            ! single range
            CALL Generate_R64_ZeroToOne(TotNum, RVal)
        CASE (7)
            ! single range
            CALL Generate_R64_ExpRange(TotNum, 1, 38, RVal)
        CASE (8)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, 39, 200, RVal)
        CASE (9)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, 201, 308, RVal)
        END SELECT
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++                   Internal Write                       +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        ! test internal writes with READ statement and FromString functions
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE List-DR statement', ListDr)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE G-Dest (G0) statement', G_Desc)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE G-Dest (G0.16) statement', G_Desc, 0, 16)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE G-Dest (G0.17) statement', G_Desc, 0, 17)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE E-Dest (E0.16E3) statement', E_Desc, 0, 16, 3)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE E-Dest (E0.17E3) statement', E_Desc, 0, 17, 3)
        ! test ToChar/ToString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++           ToString_Conversion - General Format         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real64_ToChar(TotNum, FStr, 'Dragonbox - General', RealDP_ToString_DragonBox, GeneralFmt)
        CALL Test_Real64_ToChar(TotNum, FStr, 'Ryu       - General', RealDP_ToString_Ryu,       GeneralFmt)
        CALL Test_Real64_ToChar(TotNum, FStr, 'Schubfach - General', RealDP_ToString_Schubfach, GeneralFmt)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++        ToString_Conversion - Scientific Format         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real64_ToChar(TotNum, FStr, 'Dragonbox - Scientific', RealDP_ToString_DragonBox, ScientificFmt)
        CALL Test_Real64_ToChar(TotNum, FStr, 'Ryu       - Scientific', RealDP_ToString_Ryu,       ScientificFmt)
        CALL Test_Real64_ToChar(TotNum, FStr, 'Schubfach - Scientific', RealDP_ToString_Schubfach, ScientificFmt)
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

    SUBROUTINE Test_Real64_ToChar(TotalLoop, OutStr, TestName, TestFunc, IsScientific)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check real-tochar conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: TotalLoop
        TYPE(FString),     INTENT(OUT)  :: OutStr(TotalLoop)
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        LOGICAL,           INTENT(IN)   :: IsScientific
        PROCEDURE(Real64_ToChar)       :: TestFunc
    
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
    END SUBROUTINE Test_Real64_ToChar

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
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'FastFloat', RealDP_FromString_FastFloat)
        SELECT CASE (TestNum)
        CASE (2, 4:9)
            ! for subnormal and normal with Exp < -2000, LibC is very very very slow for "200000" test data
            !   (i.e. when Eisel-Lemire is not valid)
            WRITE(*,*) 'Check LibC'
            CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'LibC     ', RealDP_FromString_LibC)
        END SELECT
        WRITE(*,*) 'Check YY'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'YY       ', RealDP_FromString_YY)
        WRITE(*,*) 'Check Lemire'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'Lemire   ', RealDP_FromString_Lemire)
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
        REAL(KIND=DP)                   :: ROut, AccErr, RelErr
        CHARACTER(LEN=:), ALLOCATABLE   :: TestName
    
    !** FLOW:

        ! initialize
        Count = 0
        AccErr = 0.0D0
        
        DO I = 1, TotalLoop
            READ(OutStr(I)%cStr, FMT=*, IOSTAT=IErr) ROut
            IF (RVal(I) /= 0.0D0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
            IF (RelErr > EPSILON(1.0D0)*10.0D0) Count = Count + 1
        END DO
        TestName = 'ToString: ' // TestName1 // '   FromString: Internal Read'
        IF (Count /= 0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Err Count = ', Count
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSEIF (AccErr /= 0.0D0) THEN
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
        PROCEDURE(Real64_FromChar)     :: TestFunc

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)               :: Count, I
        REAL(KIND=DP)                   :: ROut, AccErr, RelErr
        CHARACTER(LEN=:), ALLOCATABLE   :: TestName
    
    !** FLOW:

        ! initialize
        Count = 0
        AccErr = 0.0D0
        
        DO I = 1, TotalLoop
            ROut = TestFunc(OutStr(I)%cStr, FPlusNum)
            IF (RVal(I) /= 0.0D0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
            IF (RelErr > EPSILON(1.0D0)*10.0D0) Count = Count + 1
        END DO
        TestName = 'ToString: ' // TestName1 // '   FromString: ' // TestName2
        IF (Count /= 0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Err Count = ', Count
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSEIF (AccErr /= 0.0D0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        END IF
        
        RETURN
    END SUBROUTINE Check_FromString_Results

    !--------------------------------------------------------------------------

END SUBROUTINE RoundTripTest_RealDP

!******************************************************************************

SUBROUTINE RoundTripTest_DragonBox64()

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform the so-called round-trip test to check the correctness of write-read cycle of
    !   various combinations of "ToString_DragonBox" function and "READ/FromString" functions.
    ! This routine is actually a subset of the "RoundTripTest_RealDP" routine.  It is used to
    !   identify and illustrate the problem in the "internal read" (i.e. when the shortest-
    !   representation decimal string produced by the "ToString" function is at the midpoint
    !   of RVal(I) and its (either lower or upper) neighbor).

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
    
!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! na
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER    :: TotNum = 200000
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(FString), ALLOCATABLE  :: FStr(:)
    REAL(KIND=DP), ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)           :: TestNum
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++           RoundTripTest_DragonBox64 Started.          +++++'
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
            CALL Generate_R64_SubNormal(TotNum, RVal)
        CASE (2)
            ! normal (double) range
            CALL Generate_R64_Normal(TotNum, RVal)
        CASE (3)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, -308, -201, RVal)
        CASE (4)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, -200, -39, RVal)
        CASE (5)
            ! single range
            CALL Generate_R64_ExpRange(TotNum, -38, -1, RVal)
        CASE (6)
            ! single range
            CALL Generate_R64_ZeroToOne(TotNum, RVal)
        CASE (7)
            ! single range
            CALL Generate_R64_ExpRange(TotNum, 1, 38, RVal)
        CASE (8)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, 39, 200, RVal)
        CASE (9)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, 201, 308, RVal)
        END SELECT
        ! test ToChar/ToString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++           ToString_Conversion - General Format         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real64_ToChar(TotNum, FStr, 'Dragonbox - General', RealDP_ToString_DragonBox, GeneralFmt)
!        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
!        WRITE(*,*) '+++++        ToString_Conversion - Scientific Format         +++++'
!        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
!        CALL Test_Real64_ToChar(TotNum, FStr, 'Dragonbox - Scientific', RealDP_ToString_DragonBox, ScientificFmt)
        WRITE(*,*) ' '
        PAUSE
    END DO
    
    RETURN

CONTAINS

    SUBROUTINE Test_Real64_ToChar(TotalLoop, OutStr, TestName, TestFunc, IsScientific)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check real-tochar conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: TotalLoop
        TYPE(FString),     INTENT(OUT)  :: OutStr(TotalLoop)
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        LOGICAL,           INTENT(IN)   :: IsScientific
        PROCEDURE(Real64_ToChar)       :: TestFunc
    
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
    END SUBROUTINE Test_Real64_ToChar

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
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'FastFloat', RealDP_FromString_FastFloat)
        SELECT CASE (TestNum)
        CASE (2, 4:9)
            ! for subnormal and normal with Exp < -2000, LibC is very very very slow for "200000" test data
            !   (i.e. when Eisel-Lemire is not valid)
            WRITE(*,*) 'Check LibC'
            CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'LibC     ', RealDP_FromString_LibC)
        END SELECT
        WRITE(*,*) 'Check YY'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'YY       ', RealDP_FromString_YY)
        WRITE(*,*) 'Check Lemire'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'Lemire   ', RealDP_FromString_Lemire)
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
        REAL(KIND=DP)                   :: ROut, AccErr, RelErr
        CHARACTER(LEN=:), ALLOCATABLE   :: TestName
    
    !** FLOW:

        ! initialize
        Count = 0
        AccErr = 0.0D0
        
        DO I = 1, TotalLoop
            READ(OutStr(I)%cStr, FMT=*, IOSTAT=IErr) ROut
            IF (RVal(I) /= 0.0D0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
            IF (RelErr > EPSILON(1.0D0)*10.0D0) Count = Count + 1
            IF (RelErr /= 0.0D0) THEN
                OPEN(UNIT=999, FILE='Internal Read Debug.Txt', POSITION='APPEND' )
                ! IMPORTANT NOTE:
                ! For a combination of the "internal read" and a (any) "ToString" function it appears
                !   that the internal read will not return the same REAL number (i.e. ROut /= RVal(I))
                !   when the shortest-representation decimal string produced by the "ToString" function
                !   is at the midpoint of RVal(I) and its (either lower or upper) neighbor.  This means
                !   that for these cases, the internal read will return the neighbor of RVal(I) instead.
                WRITE(999,*)                    'OutStr  = ', OutStr(I)%cStr
                WRITE(999,'(A10, 1X, E0.17E3)') 'RVal    = ', RVal(I)
                WRITE(999,'(A10, 1X, E0.17E3)') 'ROut-IR = ', ROut
                WRITE(999,'(A10, 1X, E0.17E3)') 'ROut-FF = ', RealDP_FromString_FastFloat(OutStr(I)%cStr)
                WRITE(999,'(A10, 1X, E0.17E3)') 'ROut-LC = ', RealDP_FromString_LibC(OutStr(I)%cStr)
                WRITE(999,'(A10, 1X, E0.17E3)') 'ROut-YY = ', RealDP_FromString_YY(OutStr(I)%cStr)
                WRITE(999,'(A10, 1X, E0.17E3)') 'ROut-LM = ', RealDP_FromString_Lemire(OutStr(I)%cStr)
                WRITE(999,*) '-------------------------------------------------------------'
                CLOSE(999)
            END IF
        END DO
        TestName = 'ToString: ' // TestName1 // '   FromString: Internal Read'
        IF (Count /= 0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Err Count = ', Count
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSEIF (AccErr /= 0.0D0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSE
            WRITE(*,*) TestName // ' - Success'
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
        PROCEDURE(Real64_FromChar)     :: TestFunc

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)               :: Count, I
        REAL(KIND=DP)                   :: ROut, AccErr, RelErr
        CHARACTER(LEN=:), ALLOCATABLE   :: TestName
    
    !** FLOW:

        ! initialize
        Count = 0
        AccErr = 0.0D0
        
        DO I = 1, TotalLoop
            ROut = TestFunc(OutStr(I)%cStr, FPlusNum)
            IF (RVal(I) /= 0.0D0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
            IF (RelErr > EPSILON(1.0D0)*10.0D0) Count = Count + 1
        END DO
        TestName = 'ToString: ' // TestName1 // '   FromString: ' // TestName2
        IF (Count /= 0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Err Count = ', Count
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSEIF (AccErr /= 0.0D0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSE
            WRITE(*,*) TestName // ' - Success'
        END IF
        
        RETURN
    END SUBROUTINE Check_FromString_Results

    !--------------------------------------------------------------------------

END SUBROUTINE RoundTripTest_DragonBox64

!******************************************************************************

SUBROUTINE BenchTest_RealDP_ToString()

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
    REAL(KIND=DP),      ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)                :: K
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++       BenchTest_RealDP_ToString Started.              +++++'
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
            CALL Generate_R64_SubNormal(TotNum, RVal)
        CASE (2)
            ! normal (double) range
            CALL Generate_R64_Normal(TotNum, RVal)
        CASE (3)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, -308, -201, RVal)
        CASE (4)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, -200, -39, RVal)
        CASE (5)
            ! single range
            CALL Generate_R64_ExpRange(TotNum, -38, -1, RVal)
        CASE (6)
            ! single range
            CALL Generate_R64_ZeroToOne(TotNum, RVal)
        CASE (7)
            ! single range
            CALL Generate_R64_ExpRange(TotNum, 1, 38, RVal)
        CASE (8)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, 39, 200, RVal)
        CASE (9)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, 201, 308, RVal)
        END SELECT
        ! test internal writes
        CALL Test_Internal_Write('WRITE List-DR statement', '*')
        CALL Test_Internal_Write('WRITE G-Dest (G0) statement', '(G0)')
        CALL Test_Internal_Write('WRITE G-Dest (G0.16) statement', '(G0.16)')
        CALL Test_Internal_Write('WRITE G-Dest (G0.17) statement', '(G0.17)')
        CALL Test_Internal_Write('WRITE E-Dest (E0.16E3) statement', '(E0.16E3)')
        CALL Test_Internal_Write('WRITE E-Dest (E0.17E3) statement', '(E0.17E3)')
        ! test ToChar/ToString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++           String_Conversion - General Format           +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real64_ToChar('DummyCall - General', RealDP_ToString_DragonBox, IsScientific=FalseVal)
        CALL Test_Real64_ToChar('Dragonbox - General', RealDP_ToString_DragonBox, IsScientific=FalseVal)
        CALL Test_Real64_ToChar('Ryu       - General', RealDP_ToString_Ryu,       IsScientific=FalseVal)
        CALL Test_Real64_ToChar('Schubfach - General', RealDP_ToString_Schubfach, IsScientific=FalseVal)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++        String_Conversion - Scientific Format           +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real64_ToChar('Dragonbox - Scientific', RealDP_ToString_DragonBox, IsScientific=TrueVal)
        CALL Test_Real64_ToChar('Ryu       - Scientific', RealDP_ToString_Ryu,       IsScientific=TrueVal)
        CALL Test_Real64_ToChar('Schubfach - Scientific', RealDP_ToString_Schubfach, IsScientific=TrueVal)
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

    SUBROUTINE Test_Real64_ToChar(TestName, TestFunc, IsScientific)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check real-tochar conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        LOGICAL, OPTIONAL, INTENT(IN)   :: IsScientific
        PROCEDURE(Real64_ToChar)        :: TestFunc
    
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
    END SUBROUTINE Test_Real64_ToChar

    !--------------------------------------------------------------------------

    SUBROUTINE Check_Write_Results(Loop, TestName)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-write conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)   :: Loop
        CHARACTER(LEN=*),   INTENT(IN)   :: TestName
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: IErr
        INTEGER(KIND=I4B)   :: I
        REAL(KIND=DP)       :: ROut, AccErr, RelErr
    
    !** FLOW:

        ! initialize
        AccErr = 0.0D0
        
        DO I = 1, Loop
            READ(CStr(I), FMT=*, IOSTAT=IErr) ROut
            IF (RVal(I) /= 0.0D0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
        END DO
        IF (AccErr /= 0.0D0) THEN
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
        ! To check R64-to-string conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)   :: Loop
        CHARACTER(LEN=*),   INTENT(IN)   :: TestName
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: I
        REAL(KIND=DP)       :: ROut, AccErr, RelErr
    
    !** FLOW:

        ! initialize
        AccErr = 0.0D0
        
        DO I = 1, Loop
            ROut = RealDP_FromString_FastFloat(FStr(I)%cStr)
            IF (RVal(I) /= 0.0D0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
        END DO
        IF (AccErr /= 0.0D0) THEN
            WRITE(*,*) '          ' // TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSE
            WRITE(*,*) '          ' // TestName // ' - SUCCESS'
        END IF
        
        RETURN
    END SUBROUTINE Check_Q2S_Results

    !--------------------------------------------------------------------------

END SUBROUTINE BenchTest_RealDP_ToString

!******************************************************************************

SUBROUTINE BenchTest_RealDP_FromString()

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
    REAL(KIND=DP),      ALLOCATABLE  :: RRef(:)
    REAL(KIND=DP),      ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)                :: K, L, IErr
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++       BenchTest_RealDP_FromString Started.            +++++'
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
            CALL Generate_R64_SubNormal(TotNum, RRef)
        CASE (2)
            ! normal (double) range
            CALL Generate_R64_Normal(TotNum, RRef)
        CASE (3)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, -308, -201, RRef)
        CASE (4)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, -200, -39, RRef)
        CASE (5)
            ! single range
            CALL Generate_R64_ExpRange(TotNum, -38, -1, RRef)
        CASE (6)
            ! single range
            CALL Generate_R64_ZeroToOne(TotNum, RRef)
        CASE (7)
            ! single range
            CALL Generate_R64_ExpRange(TotNum, 1, 38, RRef)
        CASE (8)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, 39, 200, RRef)
        CASE (9)
            ! beyond single range
            CALL Generate_R64_ExpRange(TotNum, 201, 308, RRef)
        END SELECT
        ! test internal reads
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++       String_Conversion - READ from WRITE E0.17E3      +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        DO L = 1, TotNum
            WRITE(CStr(L), FMT='(E0.17E3)', IOSTAT=IErr) RRef(L)
            FStr(L)%cStr = TRIM(ADJUSTL(CStr(L)))
        END DO
        CALL Test_Internal_Read('READ from WRITE E-Dest (E0.17E3)')
        CALL Test_Real64_FromChar('FastFloat - WRITE E-Dest', RealDP_FromString_FastFloat, FPlusNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real64_FromChar('LibC      - WRITE E-Dest', RealDP_FromString_LibC,      FPlusNum)
        END SELECT
        CALL Test_Real64_FromChar('YY        - WRITE E-Dest', RealDP_FromString_YY,        FPlusNum)
        CALL Test_Real64_FromChar('Lemire    - WRITE E-Dest', RealDP_FromString_Lemire,    FPlusNum)
        ! test FromChar/FromString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++       String_Conversion - Parse Fortran Number         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        ! generate input string
        DO L = 1, TotNum
            FStr(L)%cStr = RealDP_ToString_DragonBox(RRef(L))
        END DO
        CALL Test_Real64_FromChar('FastFloat - Fortran', RealDP_FromString_FastFloat, FortNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real64_FromChar('LibC      - Fortran', RealDP_FromString_LibC,      FortNum)
        END SELECT
        CALL Test_Real64_FromChar('YY        - Fortran', RealDP_FromString_YY,        FortNum)
        CALL Test_Real64_FromChar('Lemire    - Fortran', RealDP_FromString_Lemire,    FortNum)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++    String_Conversion - Parse FortranPlus Number        +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real64_FromChar('FastFloat - FortPlus', RealDP_FromString_FastFloat, FPlusNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real64_FromChar('LibC      - FortPlus', RealDP_FromString_LibC,      FPlusNum)
        END SELECT
        CALL Test_Real64_FromChar('YY        - FortPlus', RealDP_FromString_YY,        FPlusNum)
        CALL Test_Real64_FromChar('Lemire    - FortPlus', RealDP_FromString_Lemire,    FPlusNum)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++        String_Conversion - Parse JSON Number           +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real64_FromChar('FastFloat - JSON', RealDP_FromString_FastFloat, JsonNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real64_FromChar('LibC      - JSON', RealDP_FromString_LibC,      JsonNum)
        END SELECT
        CALL Test_Real64_FromChar('YY        - JSON', RealDP_FromString_YY,        JsonNum)
        CALL Test_Real64_FromChar('Lemire    - JSON', RealDP_FromString_Lemire,    JsonNum)
        WRITE(*,*) ' '
        PAUSE
    END DO
    
    RETURN

CONTAINS

    SUBROUTINE Test_Internal_Read(TestName)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-read conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
    
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

    SUBROUTINE Test_Real64_FromChar(TestName, TestFunc, ParseOpt)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check from-string conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),   INTENT(IN)  :: TestName
        PROCEDURE(Real64_FromChar)      :: TestFunc
        INTEGER(KIND=I4B),  INTENT(IN)  :: ParseOpt

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
    END SUBROUTINE Test_Real64_FromChar

    !--------------------------------------------------------------------------

    SUBROUTINE Check_Results(Loop, TestName)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-read / from-string conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)   :: Loop
        CHARACTER(LEN=*),   INTENT(IN)   :: TestName
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: IErr
        INTEGER(KIND=I4B)   :: I
        REAL(KIND=DP)       :: AccErr, RelErr
    
    !** FLOW:

        ! initialize
        AccErr = 0.0D0
        
        DO I = 1, Loop
            IF (RRef(I) /= 0.0D0) THEN
                RelErr = ABS((RRef(I)-RVal(I))/RRef(I))
            ELSE
                RelErr = ABS((RRef(I)-RVal(I)))
            END IF
            AccErr = AccErr + RelErr
        END DO
        IF (AccErr /= 0.0D0) THEN
            WRITE(*,*) '          ' // TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSE
            WRITE(*,*) '          ' // TestName // ' - SUCCESS'
        END IF
        
        RETURN
    END SUBROUTINE Check_Results

    !--------------------------------------------------------------------------

END SUBROUTINE BenchTest_RealDP_FromString

!------------------------------------------------------------------------------
!
!                       RANDOM-NUMBER GENERATION ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE Generate_R64_SubNormal(N, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the subnormal range

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)     :: N            ! number of random numbers
    REAL(KIND=DP),      INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I
    INTEGER(KIND=I8B)   :: RawFP64
    TYPE(Mt64RNG)       :: RNG
    
!** FLOW
    
    WRITE(*,*) '-------------------------------------------------------------'
    WRITE(*,*) 'Generate random  64-bit real numbers in the subnormal range. '
    WRITE(*,*) '-------------------------------------------------------------'

    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate 64-bit unsigned integer in the subnormal range
        RawFP64    = RNG%NextLong(MinSubnormal, MaxSubnormal)
        ! convert to 64-bit floating point number
        RandNum(I) = RawFP_ToDouble(RawFP64)
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R64_SubNormal

!******************************************************************************

SUBROUTINE Generate_R64_Normal(N, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the normal range

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)     :: N            ! number of random numbers   
    REAL(KIND=DP),      INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I
    INTEGER(KIND=I8B)   :: RawFP64
    TYPE(Mt64RNG)       :: RNG
    
!** FLOW
    
    WRITE(*,*) '----------------------------------------------------------'
    WRITE(*,*) 'Generate random  64-bit real numbers in the normal range. '
    WRITE(*,*) '----------------------------------------------------------'

    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate 64-bit unsigned integer in the normal range
        RawFP64   = RNG%NextLong(MinNormal, MaxNormal)
        ! convert to 64-bit floating point number
        RandNum(I) = RawFP_ToDouble(RawFP64)
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R64_Normal

!******************************************************************************

SUBROUTINE Generate_R64_ExpRange(N, ExpLo, ExpHi, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the specified exponent range
    ! where ExpHi must be greater than ExpLo and both variables must be
    ! in the applicable (normal) exponent range [-308, 308]

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)     :: N            ! number of random numbers
    INTEGER(KIND=I4B), INTENT(IN)     :: ExpLo        ! lower bound of exponent range
    INTEGER(KIND=I4B), INTENT(IN)     :: ExpHi        ! upper bound of exponent range
    REAL(KIND=DP),     INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! exponent bounds for 'normal' range
    INTEGER(KIND=I4B), PARAMETER  :: Exponent_UppBound =  308
    INTEGER(KIND=I4B), PARAMETER  :: Exponent_LowBound = -308     ! lowest normal
    REAL(KIND=DP),     PARAMETER  :: TenDouble = 10.0D0

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I, ExpLow, ExpUpp, Exp
    REAL(KIND=DP)       :: Rand
    TYPE(Mt64RNG)       :: RNG
    
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
    WRITE(*,*) 'Generate random  64-bit real numbers in the specified exponent range. '
    WRITE(*,*) 'ExpLow = ', ExpLow, ' and ExpUpp = ', ExpUpp
    WRITE(*,*) '----------------------------------------------------------------------'
    
    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate the random exponent in the valid range
        Exp = RNG%NextInteger(ExpLow, ExpUpp)
        ! generate the random real number in [0, 1) range
        Rand = RNG%NextDouble()
        ! set the current random number
        RandNum(I) = Rand*(TenDouble**Exp)
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R64_ExpRange

!******************************************************************************

SUBROUTINE Generate_R64_ZeroToOne(N, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the range [0.0, 1.0)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)     :: N            ! number of random numbers
    REAL(KIND=DP),      INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I
    TYPE(Mt64RNG)       :: RNG
    
!** FLOW
    
    WRITE(*,*) '--------------------------------------------------------------'
    WRITE(*,*) 'Generate random  64-bit real numbers in the range [0.0, 1.0). '
    WRITE(*,*) '--------------------------------------------------------------'
    
    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate the random real number in [0, 1) range
        RandNum(I) = RNG%NextDouble()
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R64_ZeroToOne

!******************************************************************************

FUNCTION RawFP_ToDouble(RawVal) RESULT(RealVal)

!** PURPOSE OF THIS SUBROUTINE:
! To convert a raw binary floating point number into
    ! its equivalent real number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)   :: RawVal
    REAL(KIND=DP)                   :: RealVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: IntVal
    REAL(KIND=DP)       :: FloatVal
    EQUIVALENCE(IntVal, FloatVal)

!** FLOW
    
    IntVal  = RawVal
    RealVal = FloatVal

    RETURN

END FUNCTION RawFP_ToDouble

!******************************************************************************

END MODULE ModTest_RealDP_CharConv

!******************************************************************************
