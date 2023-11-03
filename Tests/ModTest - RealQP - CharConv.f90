
MODULE ModTest_RealQP_CharConv

!** PURPOSE OF THIS MODULE:
    ! This module contains routines that test functions from 'ModBase_RealQP_CharConv' module.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_UInt128
    USE ModBase_RealQP_CharConv
    USE ModBase_Integer_ToChar, ONLY: ToChar => I32_ToChar_CC
    USE Class_Mt64RNG
    USE Class_Timer
    USE Class_ProgressBar

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: RoundTripTest_RealQP
    PUBLIC :: RoundTripTest_DragonBox128
    PUBLIC :: BenchTest_RealQP_ToString
    PUBLIC :: BenchTest_RealQP_FromString
    PUBLIC :: GetWriteFormat
    PUBLIC :: FString

    PRIVATE          ! by default, hide all data and routines except those declared explicitly
    
!** MODULE PARAMETERS:
    INTEGER(KIND=I4B), PUBLIC, PARAMETER    :: ListDr = 1
    INTEGER(KIND=I4B), PUBLIC, PARAMETER    :: G_Desc = 2
    INTEGER(KIND=I4B), PUBLIC, PARAMETER    :: E_Desc = 3
    LOGICAL,           PUBLIC, PARAMETER    :: GeneralFmt    = FalseVal
    LOGICAL,           PUBLIC, PARAMETER    :: ScientificFmt = TrueVal

!** DERIVED TYPE DEFINITIONS
	TYPE FString
        CHARACTER(LEN=:), ALLOCATABLE  :: cStr
    END TYPE FString

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Real128_ToChar(Number, IsScientific) RESULT(cStr)
            IMPORT
            REAL(KIND=QP),     INTENT(IN)  :: Number
            LOGICAL, OPTIONAL, INTENT(IN)  :: IsScientific
            CHARACTER(LEN=:), ALLOCATABLE  :: cStr
        END FUNCTION
        FUNCTION Real128_FromChar(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            IMPORT
            CHARACTER(LEN=*),                        INTENT(IN)    :: cStr
            INTEGER(KIND=I4B),             OPTIONAL, INTENT(IN)    :: ParseOpt
            LOGICAL,                       OPTIONAL, INTENT(OUT)   :: ErrFlag
            CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg
            REAL(KIND=QP)                                          :: Number
        END FUNCTION
    END INTERFACE

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!------------------------------------------------------------------------------
!
!                       TESTING ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE RoundTripTest_RealQP()

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
    REAL(KIND=QP), ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)           :: TestNum
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++           RoundTripTest_RealQP Started.               +++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    
    ! allocate working variables
    ALLOCATE(FStr(TotNum))
    ALLOCATE(RVal(TotNum))
    
    DO TestNum = 1, 9
        ! generate random numbers
        SELECT CASE (TestNum)
        CASE (1)
            ! subnormal range
            CALL Generate_R128_SubNormal(TotNum, RVal)
        CASE (2)
            ! normal (quad) range
            CALL Generate_R128_Normal(TotNum, RVal)
        CASE (3)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, -4932, -2001, RVal)
        CASE (4)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, -2000, -309, RVal)
        CASE (5)
            ! double range
            CALL Generate_R128_ExpRange(TotNum, -308, -1, RVal)
        CASE (6)
            ! double range
            CALL Generate_R128_ZeroToOne(TotNum, RVal)
        CASE (7)
            ! double range
            CALL Generate_R128_ExpRange(TotNum, 1, 308, RVal)
        CASE (8)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, 309, 2000, RVal)
        CASE (9)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, 2001, 4932, RVal)
        END SELECT
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++                   Internal Write                       +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        ! test internal writes with READ statement and FromString functions
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE List-DR statement', ListDr)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE G-Dest (G0) statement', G_Desc)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE G-Dest (G0.35) statement', G_Desc, 0, 35)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE G-Dest (G0.36) statement', G_Desc, 0, 36)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE E-Dest (E0.35E4) statement', E_Desc, 0, 35, 4)
        CALL Test_Internal_Write(TotNum, FStr, 'WRITE E-Dest (E0.36E4) statement', E_Desc, 0, 36, 4)
        ! test ToChar/ToString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++           ToString_Conversion - General Format         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real128_ToChar(TotNum, FStr, 'Dragonbox - General', RealQP_ToString_DragonBox, GeneralFmt)
        CALL Test_Real128_ToChar(TotNum, FStr, 'Ryu       - General', RealQP_ToString_Ryu,       GeneralFmt)
        CALL Test_Real128_ToChar(TotNum, FStr, 'Schubfach - General', RealQP_ToString_Schubfach, GeneralFmt)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++        ToString_Conversion - Scientific Format         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real128_ToChar(TotNum, FStr, 'Dragonbox - Scientific', RealQP_ToString_DragonBox, ScientificFmt)
        CALL Test_Real128_ToChar(TotNum, FStr, 'Ryu       - Scientific', RealQP_ToString_Ryu,       ScientificFmt)
        CALL Test_Real128_ToChar(TotNum, FStr, 'Schubfach - Scientific', RealQP_ToString_Schubfach, ScientificFmt)
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
        CHARACTER(LEN=60)               :: CStr
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
                    IF (PRESENT(Digits)) THEN
                        SELECT CASE (Digits)
                        CASE (:35)
                            ! for Digits <= 35, run-time format is O.K. for IFort compiler 2020.0.164
                            WRITE(CStr, FMT=Fmt, IOSTAT=IErr) RVal(Indx)
                        CASE (36)
                            ! for Digits > 35, it must be compile-time for IFort compiler 2020.0.164
                            ! otherwise, the compiler will produce the undesirable '******' results
                            WRITE(CStr, FMT='(G0.36)', IOSTAT=IErr) RVal(Indx)
                        CASE (37)
                            WRITE(CStr, FMT='(G0.37)', IOSTAT=IErr) RVal(Indx)
                        CASE (38)
                            WRITE(CStr, FMT='(G0.38)', IOSTAT=IErr) RVal(Indx)
                        CASE (39)
                            WRITE(CStr, FMT='(G0.39)', IOSTAT=IErr) RVal(Indx)
                        END SELECT
                    ELSE
                        WRITE(CStr, FMT=Fmt, IOSTAT=IErr) RVal(Indx)
                    END IF
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

    SUBROUTINE Test_Real128_ToChar(TotalLoop, OutStr, TestName, TestFunc, IsScientific)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check real-tochar conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: TotalLoop
        TYPE(FString),     INTENT(OUT)  :: OutStr(TotalLoop)
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        LOGICAL,           INTENT(IN)   :: IsScientific
        PROCEDURE(Real128_ToChar)       :: TestFunc
    
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
    END SUBROUTINE Test_Real128_ToChar

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
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'FastFloat', RealQP_FromString_FastFloat)
        SELECT CASE (TestNum)
        CASE (2, 4:9)
            ! for subnormal and normal with Exp < -2000, LibC is very very very slow for "200000" test data
            !   (i.e. when Eisel-Lemire is not valid)
            WRITE(*,*) 'Check LibC'
            CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'LibC     ', RealQP_FromString_LibC)
        END SELECT
        WRITE(*,*) 'Check YY'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'YY       ', RealQP_FromString_YY)
        WRITE(*,*) 'Check Lemire'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'Lemire   ', RealQP_FromString_Lemire)
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
        REAL(KIND=QP)                   :: ROut, AccErr, RelErr
        CHARACTER(LEN=:), ALLOCATABLE   :: TestName
    
    !** FLOW:

        ! initialize
        Count = 0
        AccErr = 0.0Q0
        
        DO I = 1, TotalLoop
            READ(OutStr(I)%cStr, FMT=*, IOSTAT=IErr) ROut
            IF (RVal(I) /= 0.0Q0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
            IF (RelErr > EPSILON(1.0Q0)*10.0Q0) Count = Count + 1
!            IF (RelErr /= 0.0Q0) THEN
                ! IMPORTANT NOTE:
                ! For a combination of the "internal read" and a (any) "ToString" function it appears
                !   that the internal read will not return the same REAL number (i.e. ROut /= RVal(I))
                !   when the shortest-representation decimal string produced by the "ToString" function
                !   is at the midpoint of RVal(I) and its (either lower or upper) neighbor.  This means
                !   that for these cases, the internal read will return the neighbor of RVal(I) instead.
!                WRITE(*,*)                    'OutStr  = ', OutStr(I)%cStr
!                WRITE(*,'(A10, 1X, E0.36E4)') 'RVal    = ', RVal(I)
!                WRITE(*,'(A10, 1X, E0.36E4)') 'ROut-IR = ', ROut
!                WRITE(*,'(A10, 1X, E0.36E4)') 'ROut-FF = ', RealQP_FromString_FastFloat(OutStr(I)%cStr)
!                WRITE(*,'(A10, 1X, E0.36E4)') 'ROut-LC = ', RealQP_FromString_LibC(OutStr(I)%cStr)
!                WRITE(*,'(A10, 1X, E0.36E4)') 'ROut-YY = ', RealQP_FromString_YY(OutStr(I)%cStr)
!                WRITE(*,'(A10, 1X, E0.36E4)') 'ROut-LM = ', RealQP_FromString_Lemire(OutStr(I)%cStr)
!                WRITE(*,*) '-------------------------------------------------------------'
!            END IF
        END DO
        TestName = 'ToString: ' // TestName1 // '   FromString: Internal Read'
        IF (Count /= 0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Err Count = ', Count
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSEIF (AccErr /= 0.0Q0) THEN
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
        PROCEDURE(Real128_FromChar)     :: TestFunc

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)               :: Count, I
        REAL(KIND=QP)                   :: ROut, AccErr, RelErr
        CHARACTER(LEN=:), ALLOCATABLE   :: TestName
    
    !** FLOW:

        ! initialize
        Count = 0
        AccErr = 0.0Q0
        
        DO I = 1, TotalLoop
            ROut = TestFunc(OutStr(I)%cStr, FPlusNum)
            IF (RVal(I) /= 0.0Q0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
            IF (RelErr > EPSILON(1.0Q0)*10.0Q0) Count = Count + 1
        END DO
        TestName = 'ToString: ' // TestName1 // '   FromString: ' // TestName2
        IF (Count /= 0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Err Count = ', Count
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSEIF (AccErr /= 0.0Q0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        END IF
        
        RETURN
    END SUBROUTINE Check_FromString_Results

    !--------------------------------------------------------------------------

END SUBROUTINE RoundTripTest_RealQP

!******************************************************************************

SUBROUTINE RoundTripTest_DragonBox128()

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform the so-called round-trip test to check the correctness of write-read cycle of
    !   various combinations of "ToString_DragonBox" function and "READ/FromString" functions.
    ! This routine is actually a subset of the "RoundTripTest_RealQP" routine.  It is used to
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
    REAL(KIND=QP), ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)           :: TestNum
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++       RoundTripTest_DragonBox128 Started.             +++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    
    ! allocate working variables
    ALLOCATE(FStr(TotNum))
    ALLOCATE(RVal(TotNum))
    
    DO TestNum = 1, 9
        ! generate random numbers
        SELECT CASE (TestNum)
        CASE (1)
            ! subnormal range
            CALL Generate_R128_SubNormal(TotNum, RVal)
        CASE (2)
            ! normal (quad) range
            CALL Generate_R128_Normal(TotNum, RVal)
        CASE (3)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, -4932, -2001, RVal)
        CASE (4)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, -2000, -309, RVal)
        CASE (5)
            ! double range
            CALL Generate_R128_ExpRange(TotNum, -308, -1, RVal)
        CASE (6)
            ! double range
            CALL Generate_R128_ZeroToOne(TotNum, RVal)
        CASE (7)
            ! double range
            CALL Generate_R128_ExpRange(TotNum, 1, 308, RVal)
        CASE (8)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, 309, 2000, RVal)
        CASE (9)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, 2001, 4932, RVal)
        END SELECT
        ! test ToChar/ToString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++           ToString_Conversion - General Format         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real128_ToChar(TotNum, FStr, 'Dragonbox - General', RealQP_ToString_DragonBox, GeneralFmt)
!        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
!        WRITE(*,*) '+++++        ToString_Conversion - Scientific Format         +++++'
!        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
!        CALL Test_Real128_ToChar(TotNum, FStr, 'Dragonbox - Scientific', RealQP_ToString_DragonBox, ScientificFmt)
        WRITE(*,*) ' '
        PAUSE
    END DO
    
    RETURN

CONTAINS

    SUBROUTINE Test_Real128_ToChar(TotalLoop, OutStr, TestName, TestFunc, IsScientific)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check real-tochar conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B), INTENT(IN)   :: TotalLoop
        TYPE(FString),     INTENT(OUT)  :: OutStr(TotalLoop)
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        LOGICAL,           INTENT(IN)   :: IsScientific
        PROCEDURE(Real128_ToChar)       :: TestFunc
    
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
    END SUBROUTINE Test_Real128_ToChar

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
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'FastFloat', RealQP_FromString_FastFloat)
        SELECT CASE (TestNum)
        CASE (2, 4:9)
            ! for subnormal and normal with Exp < -2000, LibC is very very very slow for "200000" test data
            !   (i.e. when Eisel-Lemire is not valid)
            WRITE(*,*) 'Check LibC'
            CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'LibC     ', RealQP_FromString_LibC)
        END SELECT
        WRITE(*,*) 'Check YY'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'YY       ', RealQP_FromString_YY)
        WRITE(*,*) 'Check Lemire'
        CALL Check_FromString_Results(TotalLoop, OutStr, TestName, 'Lemire   ', RealQP_FromString_Lemire)
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
        REAL(KIND=QP)                   :: ROut, AccErr, RelErr
        CHARACTER(LEN=:), ALLOCATABLE   :: TestName
    
    !** FLOW:

        ! initialize
        Count = 0
        AccErr = 0.0Q0
        
        DO I = 1, TotalLoop
            READ(OutStr(I)%cStr, FMT=*, IOSTAT=IErr) ROut
            IF (RVal(I) /= 0.0Q0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
            IF (RelErr > EPSILON(1.0Q0)*10.0Q0) Count = Count + 1
            IF (RelErr /= 0.0Q0) THEN
                OPEN(UNIT=999, FILE='Internal Read Debug.Txt', POSITION='APPEND' )
                ! IMPORTANT NOTE:
                ! For a combination of the "internal read" and a (any) "ToString" function it appears
                !   that the internal read will not return the same REAL number (i.e. ROut /= RVal(I))
                !   when the shortest-representation decimal string produced by the "ToString" function
                !   is at the midpoint of RVal(I) and its (either lower or upper) neighbor.  This means
                !   that for these cases, the internal read will return the neighbor of RVal(I) instead.
                WRITE(999,*)                    'OutStr  = ', OutStr(I)%cStr
                WRITE(999,'(A10, 1X, E0.36E4)') 'RVal    = ', RVal(I)
                WRITE(999,'(A10, 1X, E0.36E4)') 'ROut-IR = ', ROut
                WRITE(999,'(A10, 1X, E0.36E4)') 'ROut-FF = ', RealQP_FromString_FastFloat(OutStr(I)%cStr)
                WRITE(999,'(A10, 1X, E0.36E4)') 'ROut-LC = ', RealQP_FromString_LibC(OutStr(I)%cStr)
                WRITE(999,'(A10, 1X, E0.36E4)') 'ROut-YY = ', RealQP_FromString_YY(OutStr(I)%cStr)
                WRITE(999,'(A10, 1X, E0.36E4)') 'ROut-LM = ', RealQP_FromString_Lemire(OutStr(I)%cStr)
                WRITE(999,*) '-------------------------------------------------------------'
                CLOSE(999)
            END IF
        END DO
        TestName = 'ToString: ' // TestName1 // '   FromString: Internal Read'
        IF (Count /= 0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Err Count = ', Count
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSEIF (AccErr /= 0.0Q0) THEN
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
        PROCEDURE(Real128_FromChar)     :: TestFunc

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)               :: Count, I
        REAL(KIND=QP)                   :: ROut, AccErr, RelErr
        CHARACTER(LEN=:), ALLOCATABLE   :: TestName
    
    !** FLOW:

        ! initialize
        Count = 0
        AccErr = 0.0Q0
        
        DO I = 1, TotalLoop
            ROut = TestFunc(OutStr(I)%cStr, FPlusNum)
            IF (RVal(I) /= 0.0Q0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
            IF (RelErr > EPSILON(1.0Q0)*10.0Q0) Count = Count + 1
        END DO
        TestName = 'ToString: ' // TestName1 // '   FromString: ' // TestName2
        IF (Count /= 0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Err Count = ', Count
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSEIF (AccErr /= 0.0Q0) THEN
            WRITE(*,*) TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSE
            WRITE(*,*) TestName // ' - Success'
        END IF
        
        RETURN
    END SUBROUTINE Check_FromString_Results

    !--------------------------------------------------------------------------

END SUBROUTINE RoundTripTest_DragonBox128

!******************************************************************************

SUBROUTINE BenchTest_RealQP_ToString()

!** PURPOSE OF THIS SUBROUTINE:
    ! To test performance of Real_ToString functions.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
    
!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! na
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: TotNum = 200000
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=60),  ALLOCATABLE  :: CStr(:)
    TYPE(FString),      ALLOCATABLE  :: FStr(:)
    REAL(KIND=QP),      ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)                :: K
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++       BenchTest_RealQP_ToString Started.              +++++'
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
            ! subnormal range
            CALL Generate_R128_SubNormal(TotNum, RVal)
        CASE (2)
            ! normal (quad) range
            CALL Generate_R128_Normal(TotNum, RVal)
        CASE (3)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, -4932, -2001, RVal)
        CASE (4)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, -2000, -309, RVal)
        CASE (5)
            ! double range
            CALL Generate_R128_ExpRange(TotNum, -308, -1, RVal)
        CASE (6)
            ! double range
            CALL Generate_R128_ZeroToOne(TotNum, RVal)
        CASE (7)
            ! double range
            CALL Generate_R128_ExpRange(TotNum, 1, 308, RVal)
        CASE (8)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, 309, 2000, RVal)
        CASE (9)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, 2001, 4932, RVal)
        END SELECT
        ! test internal writes
        CALL Test_Internal_Write('WRITE List-DR statement', '*')
        CALL Test_Internal_Write('WRITE G-Dest (G0) statement', '(G0)')
        CALL Test_Internal_Write('WRITE G-Dest (G0.35) statement', '(G0.35)')
        CALL Test_Internal_Write('WRITE G-Dest (G0.36) statement', '(G0.36)')
        CALL Test_Internal_Write('WRITE E-Dest (E0.35E4) statement', '(E0.35E4)')
        CALL Test_Internal_Write('WRITE E-Dest (E0.36E4) statement', '(E0.36E4)')
        ! test ToChar/ToString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++           String_Conversion - General Format           +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real128_ToChar('DummyCall - General', RealQP_ToString_DragonBox, IsScientific=FalseVal)
        CALL Test_Real128_ToChar('Dragonbox - General', RealQP_ToString_DragonBox, IsScientific=FalseVal)
        CALL Test_Real128_ToChar('Ryu       - General', RealQP_ToString_Ryu,       IsScientific=FalseVal)
        CALL Test_Real128_ToChar('Schubfach - General', RealQP_ToString_Schubfach, IsScientific=FalseVal)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++        String_Conversion - Scientific Format           +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real128_ToChar('Dragonbox - Scientific', RealQP_ToString_DragonBox, IsScientific=TrueVal)
        CALL Test_Real128_ToChar('Ryu       - Scientific', RealQP_ToString_Ryu,       IsScientific=TrueVal)
        CALL Test_Real128_ToChar('Schubfach - Scientific', RealQP_ToString_Schubfach, IsScientific=TrueVal)
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
        ELSEIF (Fmt == '(G0.36)') THEN
            CALL Clock%Start()
            DO J = 1, OutLoop
                DO I = 1, InLoop
                    WRITE(CStr(Indx), FMT='(G0.36)', IOSTAT=IErr) RVal(Indx)
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
        CALL Check_Write_Results(TotNum, TestName, Fmt)

        RETURN
    END SUBROUTINE Test_Internal_Write

    !--------------------------------------------------------------------------

    SUBROUTINE Test_Real128_ToChar(TestName, TestFunc, IsScientific)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check real-tochar conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        LOGICAL, OPTIONAL, INTENT(IN)   :: IsScientific
        PROCEDURE(Real128_ToChar)       :: TestFunc
    
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
    END SUBROUTINE Test_Real128_ToChar

    !--------------------------------------------------------------------------

    SUBROUTINE Check_Write_Results(Loop, TestName, Fmt)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-write conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)   :: Loop
        CHARACTER(LEN=*),   INTENT(IN)   :: TestName
        CHARACTER(LEN=*),   INTENT(IN)   :: Fmt
        
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: IErr
        INTEGER(KIND=I4B)   :: I
        REAL(KIND=QP)       :: ROut, AccErr, RelErr
        CHARACTER(LEN=60)   :: EStr
    
    !** FLOW:

        ! initialize
        AccErr = 0.0Q0
        
        DO I = 1, Loop
            READ(CStr(I), FMT=*, IOSTAT=IErr) ROut
            IF (RVal(I) /= 0.0Q0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
        END DO
        IF (AccErr /= 0.0Q0) THEN
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
        ! To check R128-to-string conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)   :: Loop
        CHARACTER(LEN=*),   INTENT(IN)   :: TestName
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: I
        REAL(KIND=QP)       :: ROut, AccErr, RelErr
    
    !** FLOW:

        ! initialize
        AccErr = 0.0Q0
        
        DO I = 1, Loop
            ROut = RealQP_FromString_FastFloat(FStr(I)%cStr)
            IF (RVal(I) /= 0.0Q0) THEN
                RelErr = ABS((RVal(I)-ROut)/RVal(I))
            ELSE
                RelErr = ABS((RVal(I)-ROut))
            END IF
            AccErr = AccErr + RelErr
        END DO
        IF (AccErr /= 0.0Q0) THEN
            WRITE(*,*) '          ' // TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSE
            WRITE(*,*) '          ' // TestName // ' - SUCCESS'
        END IF
        
        RETURN
    END SUBROUTINE Check_Q2S_Results

    !--------------------------------------------------------------------------

END SUBROUTINE BenchTest_RealQP_ToString

!******************************************************************************

SUBROUTINE BenchTest_RealQP_FromString()

!** PURPOSE OF THIS SUBROUTINE:
    ! To test performance and correctness (write-read cycle) of Real_FromString functions.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
    
!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! na
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: TotNum = 200000
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=50),  ALLOCATABLE  :: CStr(:)
    TYPE(FString),      ALLOCATABLE  :: FStr(:)
    REAL(KIND=QP),      ALLOCATABLE  :: RRef(:)
    REAL(KIND=QP),      ALLOCATABLE  :: RVal(:)
    INTEGER(KIND=I4B)                :: K, L, IErr
    
!** FLOW:

    WRITE(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    WRITE(*,*) '+++++                                                       +++++'
    WRITE(*,*) '+++++       BenchTest_RealQP_FromString Started.            +++++'
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
            CALL Generate_R128_SubNormal(TotNum, RRef)
        CASE (2)
            ! normal (quad) range
            CALL Generate_R128_Normal(TotNum, RRef)
        CASE (3)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, -4932, -2001, RRef)
        CASE (4)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, -2000, -309, RRef)
        CASE (5)
            ! double range
            CALL Generate_R128_ExpRange(TotNum, -308, -1, RRef)
        CASE (6)
            ! double range
            CALL Generate_R128_ZeroToOne(TotNum, RRef)
        CASE (7)
            ! double range
            CALL Generate_R128_ExpRange(TotNum, 1, 308, RRef)
        CASE (8)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, 309, 2000, RRef)
        CASE (9)
            ! beyond double range
            CALL Generate_R128_ExpRange(TotNum, 2001, 4932, RRef)
        END SELECT
        ! test internal reads
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++       String_Conversion - READ from WRITE E0.36E4      +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        DO L = 1, TotNum
            WRITE(CStr(L), FMT='(E0.36E4)', IOSTAT=IErr) RRef(L)
            FStr(L)%cStr = TRIM(ADJUSTL(CStr(L)))
        END DO
        CALL Test_Internal_Read('READ from WRITE E-Dest (E0.36E4)', TotNum)
        CALL Test_Real128_FromChar('FastFloat - WRITE E-Dest', RealQP_FromString_FastFloat, FPlusNum, TotNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real128_FromChar('LibC      - WRITE E-Dest', RealQP_FromString_LibC,      FPlusNum, TotNum)
        CASE DEFAULT
            ! for subnormal and normal with Exp < -2000, LibC is very slow (i.e. when Eisel-Lemire is not valid)
            CALL Test_Real128_FromChar('LibC      - WRITE E-Dest', RealQP_FromString_LibC,      FPlusNum, TotNum/2500)
        END SELECT
        CALL Test_Real128_FromChar('YY        - WRITE E-Dest', RealQP_FromString_YY,        FPlusNum, TotNum)
        CALL Test_Real128_FromChar('Lemire    - WRITE E-Dest', RealQP_FromString_Lemire,    FPlusNum, TotNum)
        ! test FromChar/FromString functions
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++       String_Conversion - Parse Fortran Number         +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        ! generate input string for FromChar/FromString functions
        DO L = 1, TotNum
            FStr(L)%cStr = RealQP_ToString_DragonBox(RRef(L))
        END DO
        CALL Test_Real128_FromChar('FastFloat - Fortran', RealQP_FromString_FastFloat, FortNum, TotNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real128_FromChar('LibC      - Fortran', RealQP_FromString_LibC,      FortNum, TotNum)
        CASE DEFAULT
            ! for subnormal and normal with Exp < -2000, LibC is very slow (i.e. when Eisel-Lemire is not valid)
            CALL Test_Real128_FromChar('LibC      - Fortran', RealQP_FromString_LibC,      FortNum, TotNum/2500)
        END SELECT
        CALL Test_Real128_FromChar('YY        - Fortran', RealQP_FromString_YY,        FortNum, TotNum)
        CALL Test_Real128_FromChar('Lemire    - Fortran', RealQP_FromString_Lemire,    FortNum, TotNum)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++    String_Conversion - Parse FortranPlus Number        +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real128_FromChar('FastFloat - FortPlus', RealQP_FromString_FastFloat, FPlusNum, TotNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real128_FromChar('LibC      - FortPlus', RealQP_FromString_LibC,      FPlusNum, TotNum)
        CASE DEFAULT
            ! for subnormal and normal with Exp < -2000, LibC is very slow (i.e. when Eisel-Lemire is not valid)
            CALL Test_Real128_FromChar('LibC      - FortPlus', RealQP_FromString_LibC,      FPlusNum, TotNum/2500)
        END SELECT
        CALL Test_Real128_FromChar('YY        - FortPlus', RealQP_FromString_YY,        FPlusNum, TotNum)
        CALL Test_Real128_FromChar('Lemire    - FortPlus', RealQP_FromString_Lemire,    FPlusNum, TotNum)
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        WRITE(*,*) '+++++        String_Conversion - Parse JSON Number           +++++'
        WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        CALL Test_Real128_FromChar('FastFloat - JSON', RealQP_FromString_FastFloat, JsonNum, TotNum)
        SELECT CASE (K)
        CASE (2, 4:9)
            CALL Test_Real128_FromChar('LibC      - JSON', RealQP_FromString_LibC,      JsonNum, TotNum)
        CASE DEFAULT
            ! for subnormal and normal with Exp < -2000, LibC is very slow (i.e. when Eisel-Lemire is not valid)
            CALL Test_Real128_FromChar('LibC      - JSON', RealQP_FromString_LibC,      JsonNum, TotNum/2500)
        END SELECT
        CALL Test_Real128_FromChar('YY        - JSON', RealQP_FromString_YY,        JsonNum, TotNum)
        CALL Test_Real128_FromChar('Lemire    - JSON', RealQP_FromString_Lemire,    JsonNum, TotNum)
        WRITE(*,*) ' '
        PAUSE
    END DO
    
    RETURN

CONTAINS

    SUBROUTINE Test_Internal_Read(TestName, TestNum)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-read conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        INTEGER(KIND=I4B), INTENT(IN)   :: TestNum
    
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
        InLoop = TestNum / OutLoop
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

    SUBROUTINE Test_Real128_FromChar(TestName, TestFunc, ParseOpt, TestNum)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check from-string conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CHARACTER(LEN=*),  INTENT(IN)   :: TestName
        PROCEDURE(Real128_FromChar)     :: TestFunc
        INTEGER(KIND=I4B),  INTENT(IN)  :: ParseOpt
        INTEGER(KIND=I4B),  INTENT(IN)  :: TestNum

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
        InLoop = TestNum / OutLoop
        Indx = 1

        CALL Bar%Setup(Progress_Delimited, Prefix = "Progress: ", Show = Progress_Value_Max, &
                        MaxVal = REAL(OutLoop, KIND=SP))
        
        ! perform bench test
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
        CALL Check_Results(TestNum, TestName)

        RETURN
    END SUBROUTINE Test_Real128_FromChar

    !--------------------------------------------------------------------------

    SUBROUTINE Check_Results(Loop, TestName)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check internal-read / from-string conversion results.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),  INTENT(IN)   :: Loop
        CHARACTER(LEN=*),   INTENT(IN)   :: TestName
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)   :: I
        REAL(KIND=QP)       :: AccErr, RelErr
    
    !** FLOW:

        ! initialize
        AccErr = 0.0Q0
        
        DO I = 1, Loop
            IF (RRef(I) /= 0.0Q0) THEN
                RelErr = ABS((RRef(I)-RVal(I))/RRef(I))
            ELSE
                RelErr = ABS((RRef(I)-RVal(I)))
            END IF
            AccErr = AccErr + RelErr
        END DO
        IF (AccErr /= 0.0Q0) THEN
            WRITE(*,*) '          ' // TestName // ' - FAIL'
            WRITE(*,*) '          Acc Error = ', AccErr
        ELSE
            WRITE(*,*) '          ' // TestName // ' - SUCCESS'
        END IF
        
        RETURN
    END SUBROUTINE Check_Results

    !--------------------------------------------------------------------------

END SUBROUTINE BenchTest_RealQP_FromString

!------------------------------------------------------------------------------
!
!                       RANDOM-NUMBER GENERATION ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE Generate_R128_SubNormal(N, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the subnormal range

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)     :: N            ! number of random numbers
    REAL(KIND=QP),     INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I
    TYPE(UInt128)       :: RawFP128
    TYPE(Mt64RNG)       :: RNG
    
!** FLOW
    
    WRITE(*,*) '-------------------------------------------------------------'
    WRITE(*,*) 'Generate random  128-bit real numbers in the subnormal range.'
    WRITE(*,*) '-------------------------------------------------------------'

    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate 128-bit unsigned integer in the subnormal range
        RawFP128   = RNG%NextU128(MinSubnormal, MaxSubnormal)
        ! convert to 128-bit floating point number
        RandNum(I) = RawFP_ToQuad(RawFP128)
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R128_SubNormal

!******************************************************************************

SUBROUTINE Generate_R128_Normal(N, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the normal range

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)     :: N            ! number of random numbers
    REAL(KIND=QP),     INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I
    TYPE(UInt128)       :: RawFP128
    TYPE(Mt64RNG)       :: RNG
    
!** FLOW
    
    WRITE(*,*) '----------------------------------------------------------'
    WRITE(*,*) 'Generate random  128-bit real numbers in the normal range.'
    WRITE(*,*) '----------------------------------------------------------'

    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate 128-bit unsigned integer in the normal range
        RawFP128   = RNG%NextU128(MinNormal, MaxNormal)
        ! convert to 128-bit floating point number
        RandNum(I) = RawFP_ToQuad(RawFP128)
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R128_Normal

!******************************************************************************

SUBROUTINE Generate_R128_ExpRange(N, ExpLo, ExpHi, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the specified exponent range
    ! where ExpHi must be greater than ExpLo and both variables must be
    ! in the applicable (normal) exponent range [-4932, 4932]

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)     :: N            ! number of random numbers
    INTEGER(KIND=I4B), INTENT(IN)     :: ExpLo        ! lower bound of exponent range
    INTEGER(KIND=I4B), INTENT(IN)     :: ExpHi        ! upper bound of exponent range
    REAL(KIND=QP),     INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! exponent bounds for 'normal' range
    INTEGER(KIND=I4B), PARAMETER  :: Exponent_UppBound =  4932
    INTEGER(KIND=I4B), PARAMETER  :: Exponent_LowBound = -4932    ! lowest normal
    REAL(KIND=QP),     PARAMETER  :: TenQuad = 10.0Q0

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I, ExpLow, ExpUpp, Exp
    REAL(KIND=QP)       :: Rand
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
    WRITE(*,*) 'Generate random  128-bit real numbers in the specified exponent range.'
    WRITE(*,*) 'ExpLow = ', ExpLow, ' and ExpUpp = ', ExpUpp
    WRITE(*,*) '----------------------------------------------------------------------'
    
    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate the random exponent in the valid range
        Exp = RNG%NextInteger(ExpLow, ExpUpp)
        ! generate the random real number in [0, 1) range
        Rand = RNG%NextQuad()
        ! set the current random number
        RandNum(I) = Rand*(TenQuad**Exp)
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R128_ExpRange

!******************************************************************************

SUBROUTINE Generate_R128_ZeroToOne(N, RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate (positive) random numbers in the range [0.0, 1.0)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)     :: N            ! number of random numbers
    REAL(KIND=QP),     INTENT(OUT)    :: RandNum(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: I
    TYPE(Mt64RNG)       :: RNG
    
!** FLOW
    
    WRITE(*,*) '--------------------------------------------------------------'
    WRITE(*,*) 'Generate random  128-bit real numbers in the range [0.0, 1.0).'
    WRITE(*,*) '--------------------------------------------------------------'
    
    ! initialize the random-number generator
    CALL RNG%Initialize()
    
    ! generate random numbers
    DO I = 1, N
        ! generate the random real number in [0, 1) range
        RandNum(I) = RNG%NextQuad()
    END DO
    
    RETURN
        
END SUBROUTINE Generate_R128_ZeroToOne

!******************************************************************************

FUNCTION RawFP_ToQuad(RawVal) RESULT(RealVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a raw binary floating point number into
    ! its equivalent real number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: RawVal
    REAL(KIND=QP)               :: RealVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    LOGICAL, PARAMETER :: Little_Endian = (TRANSFER([1_I1B, 0_I1B, 0_I1B, 0_I1B], 0_I4B) == 1_I4B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: IntVal(2)
    REAL(KIND=QP)       :: FloatVal
    EQUIVALENCE(IntVal, FloatVal)

!** FLOW
    
    IF (Little_Endian) THEN
        ! little-endian order
        IntVal(2) = RawVal%High
        IntVal(1) = RawVal%Low
        RealVal   = FloatVal
    ELSE
        ! big-endian order
        IntVal(1) = RawVal%High
        IntVal(2) = RawVal%Low
        RealVal   = FloatVal
    END IF

    RETURN

END FUNCTION RawFP_ToQuad

!******************************************************************************

FUNCTION GetWriteFormat(WriteFormat, Width, Digits, Exponent) RESULT(Fmt)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a string format used for internal-write conversions.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)           :: WriteFormat
    INTEGER(KIND=I4B), INTENT(IN), OPTIONAL :: Width
    INTEGER(KIND=I4B), INTENT(IN), OPTIONAL :: Digits
    INTEGER(KIND=I4B), INTENT(IN), OPTIONAL :: Exponent
    CHARACTER(LEN=:), ALLOCATABLE           :: Fmt
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)               :: WVal, DVal, EVal
    CHARACTER(LEN=:), ALLOCATABLE   :: W, D, E
    
!** FLOW:
        
    SELECT CASE (WriteFormat)
    CASE (ListDr)
        Fmt = '*'
    CASE (G_Desc)
        ! set WVal
        WVal = 0
        IF (PRESENT(Width)) WVal = Width
        IF (WVal < 0) WVal = 0
        ! set DVal
        DVal = 0
        IF (PRESENT(Digits)) DVal = Digits
        IF (DVal < 0) DVal = 0
        ! set EVal
        EVal = 0
        IF (PRESENT(Exponent)) EVal = Exponent
        IF (EVal < 0) EVal = 0
        ! set Fmt
        IF (WVal == 0) THEN
            ! only G0 or G0.d is applicable so ignore EVal
            D = ToChar(DVal)
            Fmt = '(G0.' // D // ')'
        ELSE
            IF (WVal < DVal + EVal + 5) WVal = DVal + EVal + 5
            W = ToChar(WVal)
            D = ToChar(DVal)
            E = ToChar(EVal)
            IF (EVal == 0) THEN
                Fmt = '(G' // W // '.' // D // ')'
            ELSE
                Fmt = '(G' // W // '.' // D // 'E' // E // ')'
            END IF
        END IF
    CASE (E_Desc)
        ! set WVal
        WVal = 0
        IF (PRESENT(Width)) WVal = Width
        IF (WVal < 0) WVal = 0
        ! set DVal
        DVal = 0
        IF (PRESENT(Digits)) DVal = Digits
        IF (DVal < 0) DVal = 0
        ! set EVal
        EVal = 0
        IF (PRESENT(Exponent)) EVal = Exponent
        IF (EVal < 0) EVal = 0
        ! set Fmt
        IF ((WVal > 0).AND.(WVal < DVal + EVal + 5)) WVal = DVal + EVal + 5
        W = ToChar(WVal)
        D = ToChar(DVal)
        IF (EVal == 0) THEN
            Fmt = '(E' // W // '.' // D // ')'
        ELSE
            E = ToChar(EVal)
            Fmt = '(E' // W // '.' // D // 'E' // E // ')'
        END IF
    END SELECT

    RETURN
    
END FUNCTION GetWriteFormat

!--------------------------------------------------------------------------

END MODULE ModTest_RealQP_CharConv

!******************************************************************************
