
MODULE Class_Timer

!** PURPOSE OF THIS MODULE:
    ! This module contains a 'Timer' class that can be used to measure clock/cpu time.

!** USE STATEMENTS:
    USE ModBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Timer, SECONDS, MILLI_SEC, MICRO_SEC, NANO_SEC

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! parameters for timer state
    INTEGER(KIND=I4B), PARAMETER  :: STATE_RUNNING = 1
    INTEGER(KIND=I4B), PARAMETER  :: STATE_PAUSED  = 2
    INTEGER(KIND=I4B), PARAMETER  :: STATE_STOPPED = 3
    ! parameters for elapsed time unit
    INTEGER(KIND=I4B), PARAMETER  :: SECONDS   = 1
    INTEGER(KIND=I4B), PARAMETER  :: MILLI_SEC = 2
    INTEGER(KIND=I4B), PARAMETER  :: MICRO_SEC = 3
    INTEGER(KIND=I4B), PARAMETER  :: NANO_SEC  = 4

!** DERIVED TYPE DEFINITIONS
    TYPE Timer
        PRIVATE
        INTEGER(KIND=I8B)     :: Frequency
        INTEGER(KIND=I8B)     :: StartTime
        INTEGER(KIND=I8B)     :: TimePaused
        INTEGER(KIND=I8B)     :: PauseTime
        INTEGER(KIND=I8B)     :: StopTime
        INTEGER(KIND=I4B)     :: State
    CONTAINS
        PROCEDURE   :: Start        => Timer_Start
        PROCEDURE   :: Stop         => Timer_Stop
        PROCEDURE   :: Pause        => Timer_Pause
        PROCEDURE   :: Resume       => Timer_Resume
        PROCEDURE   :: ElapsedTime  => Timer_ElapsedTime
    END TYPE Timer

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES or FUNCTIONS:

SUBROUTINE Timer_Start(Clock)

!** PURPOSE OF THIS SUBROUTINE:
    ! To start (or reset) the timer.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Timer), INTENT(OUT)   :: Clock

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! initialize timer components
    Clock%Frequency  = 0_I8B
    Clock%StartTime  = 0_I8B
    Clock%TimePaused = 0_I8B
    Clock%PauseTime  = 0_I8B
    Clock%StopTime   = 0_I8B
    Clock%State      = STATE_RUNNING

    ! call system clock to get current clock count and the count rate
    CALL SYSTEM_CLOCK(Count=Clock%StartTime, Count_Rate=Clock%Frequency)

    RETURN

END SUBROUTINE Timer_Start

!******************************************************************************

SUBROUTINE Timer_Stop(Clock)

!** PURPOSE OF THIS SUBROUTINE:
    ! To stop the timer.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Timer), INTENT(INOUT) :: Clock

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (Clock%State == STATE_RUNNING) THEN
        CALL SYSTEM_CLOCK(Count=Clock%StopTime)
    ELSEIF (Clock%State == STATE_PAUSED) THEN
        Clock%StopTime = Clock%PauseTime
    END IF

    Clock%State = STATE_STOPPED

    RETURN

END SUBROUTINE Timer_Stop

!******************************************************************************

SUBROUTINE Timer_Pause(Clock)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pause the timer.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Timer), INTENT(INOUT) :: Clock

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (Clock%State == STATE_RUNNING) THEN
        CALL SYSTEM_CLOCK(Count=Clock%PauseTime)
        Clock%State = STATE_PAUSED
    END IF

    RETURN

END SUBROUTINE Timer_Pause

!******************************************************************************

SUBROUTINE Timer_Resume(Clock)

!** PURPOSE OF THIS SUBROUTINE:
    ! To resume the timer.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Timer), INTENT(INOUT) :: Clock

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: CurTime, TimePaused

! FLOW

    IF (Clock%State == STATE_PAUSED) THEN
        CALL SYSTEM_CLOCK(Count=CurTime)
        TimePaused = CurTime - Clock%PauseTime
        Clock%TimePaused = Clock%TimePaused + TimePaused
        Clock%State = STATE_RUNNING
    END IF

    RETURN

END SUBROUTINE Timer_Resume

!******************************************************************************

FUNCTION Timer_ElapsedTime(Clock, Unit) RESULT(Time)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get elapsed time of the timer.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Timer), INTENT(INOUT) :: Clock
    INTEGER(KIND=I4B), OPTIONAL :: Unit     ! unit of the elapsed time
    REAL(KIND=DP)               :: Time     ! elapsed time

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: Counts    ! elapsed counts

! FLOW

    ! stop the clock if it has not yet been stopped
    IF (.NOT.(Clock%State /= STATE_STOPPED)) CALL Clock%Stop()
    
    ! get the elapsed counts/ticks
    Counts = (Clock%StopTime - Clock%StartTime - Clock%TimePaused)
    
    ! get the elapsed time in seconds
    Time = REAL(Counts, KIND=DP)/REAL(Clock%Frequency, KIND=DP)
    
    ! get the elapsed time according to the specified unit
    IF (PRESENT(Unit)) THEN
        SELECT CASE (Unit)
        CASE (MILLI_SEC)
            Time = Time*1000.0_DP
        CASE (MICRO_SEC)
            Time = Time*1000000.0_DP
        CASE (NANO_SEC)
            Time = Time*1000000000.0_DP
        CASE DEFAULT
            ! for seconds, do nothing
        END SELECT
    END IF

    RETURN

END FUNCTION Timer_ElapsedTime

!******************************************************************************

END MODULE Class_Timer
    
!******************************************************************************
