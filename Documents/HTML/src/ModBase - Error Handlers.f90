
MODULE ModBase_Error_Handlers

!^ **PURPOSE OF THIS MODULE**:  
    ! contains routines and parameters for error handling tasks

!** USE STATEMENTS:
    USE ModBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! error parameters
    PUBLIC :: ErrNone
    PUBLIC :: ErrWarning
    PUBLIC :: ErrSevere
    PUBLIC :: ErrFatal
    PUBLIC :: ErrFilename
    ! error message procedures
    PUBLIC :: DisplayMessage
    PUBLIC :: DisplayWarningError
    PUBLIC :: DisplaySevereError
    PUBLIC :: DisplayFatalError
    PUBLIC :: DisplayContinueError
    PUBLIC :: CloseErrorFile
    PUBLIC :: CloseMiscOpenFiles
    PUBLIC :: SetStopOnError
    PUBLIC :: AbortProgram

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! parameters for error level
	INTEGER(KIND=I1B), PARAMETER    :: ErrNone    = 0   !! no error level
    INTEGER(KIND=I1B), PARAMETER    :: ErrWarning = 1   !! warning error level
    INTEGER(KIND=I1B), PARAMETER    :: ErrSevere  = 2   !! severe error level
    INTEGER(KIND=I1B), PARAMETER    :: ErrFatal   = 3   !! fatal error level
    ! name of error file
    CHARACTER(LEN=*),  PARAMETER    :: ErrFilename = 'ErrorLog.txt'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    LOGICAL     :: StopOnError = FalseVal
    LOGICAL     :: FatalError  = FalseVal

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES or FUNCTIONS:

!**************************************************************************************

SUBROUTINE DisplayErrorMessage(ErrorMessage,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To display the error message on the "standard error output" unit
    !   or the indicated file unit number if specified.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(LEN=*),            INTENT(IN)     :: ErrorMessage
    INTEGER(KIND=I4B), OPTIONAL, INTENT(INOUT)  :: OutUnit

!** SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(LEN=*), PARAMETER :: ErrorFormat = '(2X,A)'

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL             :: FileOpened
    LOGICAL             :: Existing
    INTEGER(KIND=I4B)   :: IOS
    INTEGER(KIND=I4B)   :: ErrorUnit

!** FLOW

    IF (PRESENT(OutUnit)) THEN

        ! first, check whether the specified unit number is connected to an opened file
        INQUIRE(UNIT=OutUnit, OPENED=FileOpened, IOStat=IOS)

        IF (FileOpened.AND.(IOS == 0)) THEN

            ! simply write message to the specified file unit number
            WRITE(OutUnit, ErrorFormat) TRIM(ErrorMessage)

        ELSE

            ! open file
            OPEN(OutUnit, File='User_Error.Txt')

            ! then write message to the specified file unit number
            WRITE(OutUnit, ErrorFormat) TRIM(ErrorMessage)

        END IF

    ELSE

        ! first, check whether the General Error Report file is opened
        INQUIRE(FILE=ErrFilename, EXIST=Existing, OPENED=FileOpened, IOStat=IOS)

        IF (FileOpened.AND.(IOS == 0)) THEN

            ! get existing unit number for the opened file
            INQUIRE(FILE=ErrFilename, NUMBER=ErrorUnit)

            ! then write message to the General Error Report file
            WRITE(ErrorUnit, ErrorFormat) TRIM(ErrorMessage)

        ELSE

            ! open file
            IF (Existing) THEN
                OPEN(NEWUNIT=ErrorUnit, FILE=ErrFilename, POSITION='APPEND')
                ! write separation indicator to the General Error Report file
                WRITE(ErrorUnit, '(A)') '------------------------------------------------------------'
                WRITE(ErrorUnit, '(A)') ''
            ELSE
                OPEN(NEWUNIT=ErrorUnit, FILE=ErrFilename)
                ! write heading to the General Error Report file
                WRITE(ErrorUnit, '(A)') '  XPFC - General Error Report'
                WRITE(ErrorUnit, '(A)') '  Program/Library Version = 1.0'
                WRITE(ErrorUnit, '(A)') '------------------------------------------------------------'
                WRITE(ErrorUnit, '(A)') ''
            END IF

            ! then write message to the General Error Report file
            WRITE(ErrorUnit, ErrorFormat) TRIM(ErrorMessage)

        END IF

    ENDIF

    RETURN

END SUBROUTINE DisplayErrorMessage

!******************************************************************************

SUBROUTINE DisplayWarningError(ErrorMessage,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To display a 'Warning Error' message.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(LEN=*),            INTENT(IN)     :: ErrorMessage
    INTEGER(KIND=I4B), OPTIONAL, INTENT(INOUT)  :: OutUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL DisplayErrorMessage(' ++ WARNING ++ : ' // ErrorMessage, OutUnit)

    RETURN

END SUBROUTINE DisplayWarningError

!******************************************************************************

SUBROUTINE DisplayFatalError(ErrorMessage,OutUnit)

!** URPOSE OF THIS SUBROUTINE:
    !^ To display a 'Fatal Error' message.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(LEN=*),            INTENT(IN)     :: ErrorMessage
    INTEGER(KIND=I4B), OPTIONAL, INTENT(INOUT)  :: OutUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! display message
    CALL DisplayErrorMessage(' ++ FATAL ++ : ' // ErrorMessage, OutUnit)

    ! set flag
    FatalError = TrueVal

    RETURN

END SUBROUTINE DisplayFatalError

!******************************************************************************

SUBROUTINE DisplaySevereError(ErrorMessage,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To display a 'Severe Error' message.

	IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(LEN=*),            INTENT(IN)     :: ErrorMessage
    INTEGER(KIND=I4B), OPTIONAL, INTENT(INOUT)  :: OutUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

	CALL DisplayErrorMessage(' ++ SEVERE ++ : ' // ErrorMessage, OutUnit)

	RETURN

END SUBROUTINE DisplaySevereError

!******************************************************************************

SUBROUTINE DisplayContinueError(ErrorMessage,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To display a 'Continued Error' message.

	IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(LEN=*),            INTENT(IN)     :: ErrorMessage
    INTEGER(KIND=I4B), OPTIONAL, INTENT(INOUT)  :: OutUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! display message
	CALL DisplayErrorMessage(' +++++ : ' // ErrorMessage, OutUnit)

    ! check error flags
    IF (StopOnError.AND.FatalError) THEN
        CALL AbortProgram(OutUnit)
    ELSE
        ! reset flag
        FatalError = FalseVal
    END IF

	RETURN

END SUBROUTINE DisplayContinueError

!******************************************************************************

SUBROUTINE DisplayMessage(Message,OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To display a (informative) message on designated output Files.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(LEN=*),            INTENT(IN)     :: Message
    INTEGER(KIND=I4B), OPTIONAL, INTENT(INOUT)  :: OutUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL DisplayErrorMessage(Message, OutUnit)

  RETURN

END SUBROUTINE DisplayMessage

!******************************************************************************

SUBROUTINE CloseErrorFile

!** PURPOSE OF THIS SUBROUTINE:
    !^ This subroutine closes the general error file.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL             :: FileOpened
    INTEGER(KIND=I4B)   :: IOS
    INTEGER(KIND=I4B)   :: ErrorUnit

!** FLOW

    INQUIRE(FILE=ErrFilename, OPENED=FileOpened, IOStat=IOS)

    IF (FileOpened.AND.(IOS == 0)) THEN

        ! get existing unit number for the opened file
        INQUIRE(FILE=ErrFilename, NUMBER=ErrorUnit)

        ! then close the General Error Report file
        CLOSE(ErrorUnit)

    END IF

    RETURN

END SUBROUTINE CloseErrorFile

!******************************************************************************

SUBROUTINE AbortProgram(OutUnit)

!** PURPOSE OF THIS SUBROUTINE:
    !^ This subroutine causes the program to halt due to a fatal error.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DEFINITIONS:
	INTEGER(KIND=I4B), OPTIONAL, INTENT(INOUT)  :: OutUnit

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=:), ALLOCATABLE   :: StopMessage

!** FLOW

	StopMessage = 'XPFC Terminated -- Fatal Error(s) Detected.'
	CALL DisplayMessage(StopMessage, OutUnit)
	CALL CloseMiscOpenFiles()
	STOP 'Program Terminated -- Error(s) Detected.'

	RETURN

END SUBROUTINE AbortProgram

!******************************************************************************

SUBROUTINE SetStopOnError(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set StopOnError flag (module variable).

	IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DEFINITIONS:
	LOGICAL, INTENT(IN) :: Flag     ! true if requesting termination of the program due to fatal error(s)

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

	StopOnError = Flag

	RETURN

END SUBROUTINE SetStopOnError

!******************************************************************************

SUBROUTINE CloseMiscOpenFiles

!** PURPOSE OF THIS SUBROUTINE:
    !^ This subroutine scans potential Unit numbers and closes
    !   any that are still open.

!** METHODOLOGY EMPLOYED:
    ! Use INQUIRE to determine if file is open.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

!** SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER(KIND=I4B), PARAMETER    :: MaxUnitNumber = 1000

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL             :: EXIST, OPENED
    INTEGER(KIND=I4B)   :: UnitNumber
    INTEGER(KIND=I4B)   :: IOS

!** FLOW

    DO UnitNumber = 1, MaxUnitNumber
        INQUIRE(Unit=UnitNumber, EXIST=EXIST,  OPENED=OPENED, IOStat=IOS)
        IF (EXIST.AND.OPENED.AND.(IOS == 0)) CLOSE(UnitNumber)
    END DO

    RETURN

END SUBROUTINE CloseMiscOpenFiles

!******************************************************************************

END MODULE ModBase_Error_Handlers

!******************************************************************************
