
MODULE Class_ProgressBar

!** PURPOSE OF THIS MODULE:
    ! This module contains a 'ProgressBar' class that can be used to display a progress bar
    ! in the console, so that the user can see the progress of a long calculation or operation.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ISO_C_BINDING,      ONLY: C_Carriage_Return
    USE ISO_FORTRAN_ENV,    ONLY: Output_Unit

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: ProgressBar

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! parameters for the progress bar style
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: Progress_Keep       = 0
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: Progress_Continuous = 1
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: Progress_Cyclic     = 2
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: Progress_Delimited  = 3
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: Progress_Rotating   = 4
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: Progress_NoCursor   = 5
    ! parameters for options to show the value
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: Progress_None       = 0
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: Progress_Value      = 1
    INTEGER(KIND=I4B), PARAMETER, PUBLIC  :: Progress_Value_Max  = 2

!** DERIVED TYPE DEFINITIONS
    TYPE ProgressBar
        PRIVATE
        ! options
        CHARACTER(LEN=1)                :: LeadChar  = '-'
        CHARACTER(LEN=1)                :: TrailChar = '>'
        CHARACTER(LEN=:), ALLOCATABLE   :: PrefixStr
        CHARACTER(LEN=:), ALLOCATABLE   :: SuffixStr
        CHARACTER(LEN=:), ALLOCATABLE   :: FormatValue
        CHARACTER(LEN=:), ALLOCATABLE   :: FormatBoth
        INTEGER(KIND=I4B)               :: ShowValue = Progress_None
        ! parameters
        REAL(KIND=SP)                   :: MaxValue    = 100.0
        INTEGER(KIND=I4B)               :: Style       = -1
        INTEGER(KIND=I4B)               :: Width       = 50
        INTEGER(KIND=I4B)               :: Position    =  0
        INTEGER(KIND=I4B)               :: Direction   =  1
        CHARACTER(LEN=4) :: CyclicChars = '-\|/'
    CONTAINS
        PROCEDURE, PRIVATE  :: ProgressBar_Next
        PROCEDURE, PRIVATE  :: ProgressBar_Value
        PROCEDURE, PRIVATE  :: Reset        => ProgressBar_Reset
        PROCEDURE           :: Finish       => ProgressBar_Finish
        PROCEDURE           :: Setup        => ProgressBar_Setup
        GENERIC             :: Update       => ProgressBar_Next, &
                                               ProgressBar_Value
    END TYPE ProgressBar

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES or FUNCTIONS:

SUBROUTINE ProgressBar_Reset(Bar)

!** PURPOSE OF THIS SUBROUTINE:
    ! To reset all options to the default values

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ProgressBar), INTENT(INOUT)   :: Bar

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Bar%Position  =  0
    Bar%Direction =  1
    Bar%Style     = -1
    Bar%Width     = 50
    Bar%MaxValue  = 100.0

    Bar%LeadChar    = '-'
    Bar%TrailChar   = '*'
    Bar%PrefixStr   = ''
    Bar%SuffixStr   = ''
    Bar%FormatValue = '(F8.2)'
    Bar%FormatBoth  = '(F8.2,''/'',F8.2)'
    Bar%ShowValue   = Progress_None

    RETURN

END SUBROUTINE ProgressBar_Reset

!******************************************************************************

SUBROUTINE ProgressBar_Finish(Bar)

!** PURPOSE OF THIS SUBROUTINE:
    ! To move the cursor to the next line

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ProgressBar), INTENT(INOUT)   :: Bar

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Bar%Position  =  0
    Bar%Direction =  1

    WRITE(Output_Unit, '(A)') ''

    RETURN

END SUBROUTINE ProgressBar_Finish

!******************************************************************************

SUBROUTINE ProgressBar_Setup(Bar, Style, MaxVal, Leading, Trailing, Prefix, &
                             Suffix, Show, Width, FormatValue, FormatBoth)

!** PURPOSE OF THIS SUBROUTINE:
    ! To set the style and other parameters for the progress bar

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ProgressBar),          INTENT(INOUT)  :: Bar
    INTEGER(KIND=I4B),           INTENT(IN)     :: Style
    REAL(KIND=SP),     OPTIONAL, INTENT(IN)     :: MaxVal
    CHARACTER(LEN=*),  OPTIONAL, INTENT(IN)     :: Leading
    CHARACTER(LEN=*),  OPTIONAL, INTENT(IN)     :: Trailing
    CHARACTER(LEN=*),  OPTIONAL, INTENT(IN)     :: Prefix
    CHARACTER(LEN=*),  OPTIONAL, INTENT(IN)     :: Suffix
    INTEGER(KIND=I4B), OPTIONAL, INTENT(IN)     :: Show
    INTEGER(KIND=I4B), OPTIONAL, INTENT(IN)     :: Width
    CHARACTER(LEN=*),  OPTIONAL, INTENT(IN)     :: FormatValue
    CHARACTER(LEN=*),  OPTIONAL, INTENT(IN)     :: FormatBoth
    ! Style         What style for the progress bar, note: progrees_keep allows you to change settings and keep others
    ! MaxVal        Scale for values passed to progress_bar_value (calculate the length)
    ! Leading       Character to use before the cursor
    ! Trailing      Character to use to show the Position of the cursor
    ! Prefix        Text to be shown before the progress bar
    ! Suffix        Text to be shown after the progress bar
    ! Show          Option to show the actual values (none, value and maximum value)
    ! Width         Width of the area for the cursor
    ! FormatValue   Format to be used for the value (without the maximum)
    ! FormatBoth    Format to be used for the value and the maximum

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    !
    ! Check style
    !
    IF (ALL(Style /= [Progress_Keep, Progress_Continuous, Progress_Cyclic, &
                      Progress_Delimited, Progress_Rotating, Progress_NoCursor])) THEN
        Bar%Style = Progress_Continuous
    ELSE
        IF (Style /= Progress_Keep) THEN
            CALL Bar%Reset()
            Bar%Style = Style
        END IF
    END IF

    !
    ! Store the various values
    !
    IF (PRESENT(MaxVal))      Bar%MaxValue = MaxVal
    IF (PRESENT(Leading))     Bar%LeadChar = leading
    IF (PRESENT(Trailing))    Bar%TrailChar = Trailing
    IF (PRESENT(Show))        Bar%ShowValue = Show
    IF (PRESENT(Width))       Bar%Width = Width
    IF (PRESENT(FormatValue)) Bar%FormatValue = FormatValue
    IF (PRESENT(FormatBoth))  Bar%FormatBoth = FormatBoth
    IF (PRESENT(Prefix)) THEN
        Bar%PrefixStr = Prefix
    ELSE
        Bar%PrefixStr = ''
    END IF
    IF (PRESENT(Suffix)) THEN
        Bar%SuffixStr = Suffix
    ELSE
        Bar%SuffixStr = ''
    END IF

    Bar%Position  =  0
    Bar%Direction =  1

    RETURN

END SUBROUTINE ProgressBar_Setup

!******************************************************************************

SUBROUTINE ProgressBar_Next(Bar)

!** PURPOSE OF THIS SUBROUTINE:
    ! To simply write the next character
    ! Note: Only applicable if the progress bar does not show a value.
    !       If it should show a value, simply return.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ProgressBar), INTENT(INOUT)   :: Bar

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (Bar%ShowValue /= Progress_None) RETURN

    SELECT CASE (Bar%Style)
    CASE (Progress_Continuous)
        WRITE(Output_Unit, '(A)', ADVANCE = 'NO') Bar%TrailChar
        FLUSH(Output_Unit)
    CASE (Progress_Cyclic)
        IF (Bar%Position == 0) Bar%Direction = 1
        IF (Bar%Position == Bar%Width) THEN
            Bar%Direction = -1
            Bar%Position  = Bar%Position + Bar%Direction
        END IF
        WRITE(Output_Unit, '(6A)', ADVANCE = 'NO') Bar%PrefixStr,       &
            REPEAT(Bar%LeadChar, MAX(0, Bar%Position)), Bar%TrailChar,  &
            REPEAT(Bar%LeadChar, MAX(0, Bar%Width - Bar%Position - 1)), &
            Bar%SuffixStr, C_Carriage_Return
        FLUSH(Output_Unit)
        Bar%Position = Bar%Position + Bar%Direction
    CASE (Progress_Delimited)
        IF (Bar%Position < Bar%Width) THEN
            WRITE(Output_Unit, '(6A)', ADVANCE = 'NO') Bar%PrefixStr,       &
                REPEAT(Bar%LeadChar, MAX(0, Bar%Position)), Bar%TrailChar,  &
                REPEAT(' ', MAX(0, Bar%Width - Bar%Position - 1)),          &
                Bar%SuffixStr, C_Carriage_Return
            FLUSH(Output_Unit)
            Bar%Position = Bar%Position + 1
        END IF
    CASE (Progress_Rotating)
        Bar%Position = 1 + MOD(Bar%Position, 4)
        WRITE(Output_Unit, '(4A)', ADVANCE = 'NO') Bar%PrefixStr, &
            Bar%CyclicChars(Bar%Position:Bar%Position), Bar%SuffixStr, C_Carriage_Return
        FLUSH(Output_Unit)
    CASE DEFAULT
        ! Simply ignore this possibility
    END SELECT

    RETURN

END SUBROUTINE ProgressBar_Next

!******************************************************************************

SUBROUTINE ProgressBar_Value(Bar, Value)

!** PURPOSE OF THIS SUBROUTINE:
    ! To show the progress bar based on value

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ProgressBar), INTENT(INOUT)   :: Bar
    REAL(KIND=SP),      INTENT(IN)      :: Value    ! the value to be used for the Position of the cursor

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=80)    :: Value_String

! FLOW
    
    SELECT CASE (Bar%ShowValue)
    CASE (Progress_None)
        RETURN
    CASE (Progress_Value)
        WRITE(Value_String, Bar%FormatValue) Value
    CASE (Progress_Value_Max)
        WRITE(Value_String, Bar%FormatBoth) Value, Bar%MaxValue
    CASE DEFAULT
        ! Simply ignore this possibility
    END SELECT

    SELECT CASE (Bar%Style)
    CASE (Progress_Delimited)
        Bar%Position = Bar%Width * MAX(0.0, MIN(Value, Bar%MaxValue)) / Bar%MaxValue
        WRITE(Output_Unit, '(7A)', ADVANCE = 'NO') Bar%PrefixStr,       &
            REPEAT(Bar%LeadChar, MAX(0, Bar%Position)), Bar%TrailChar,  &
            REPEAT(' ',          MAX(0, Bar%Width - Bar%Position - 1)), &
            Bar%SuffixStr, TRIM(Value_String), C_Carriage_Return
        FLUSH(Output_Unit)

    CASE (Progress_NoCursor)
        WRITE(Output_Unit, '(5A)', ADVANCE = 'NO') Bar%PrefixStr, &
            TRIM(Value_String), C_Carriage_Return
    CASE DEFAULT
        ! Simply ignore this possibility
    END SELECT

    RETURN

END SUBROUTINE ProgressBar_Value

!******************************************************************************

END MODULE Class_ProgressBar
    
!******************************************************************************
