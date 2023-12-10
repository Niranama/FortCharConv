 
MODULE ModBase_Common

!^ **PURPOSE OF THIS MODULE**:  
    ! contains parameters and derived types commonly used

!** USE STATEMENTS:
    USE, INTRINSIC :: ISO_FORTRAN_ENV,  ONLY: INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
    USE, INTRINSIC :: ISO_C_BINDING,    ONLY: C_PTR, C_NULL_PTR

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By default, all parameters which are placed in this data-only
                    ! module should be available to other modules and routines.
                    ! Thus, all parameters in this module must be PUBLIC.

!** MODULE PARAMETERS:
    ! symbolic names for kind types of 8-, 4-, 2-, and 1-byte integers:
    INTEGER,          PARAMETER :: I8B = INT64  !! kind for 64-bit or 8-byte integer
	INTEGER,          PARAMETER :: I4B = INT32  !! kind for 32-bit or 4-byte integer
	INTEGER,          PARAMETER :: I2B = INT16  !! kind for 16-bit or 2-byte integer
	INTEGER,          PARAMETER :: I1B = INT8   !! kind for  8-bit or 1-byte integer
    ! symbolic names for kind types of quadruple-, double-, and single-precision reals:
	INTEGER,          PARAMETER :: QP = REAL128 !! kind for 128-bit floating point (real) number
	INTEGER,          PARAMETER :: DP = REAL64  !! kind for  64-bit floating point (real) number
	INTEGER,          PARAMETER :: SP = REAL32  !! kind for  32-bit floating point (real) number
    ! true and false values
    LOGICAL,          PARAMETER :: TrueVal  = .TRUE.    !! 
    LOGICAL,          PARAMETER :: FalseVal = .FALSE.    !! 
    ! kinds of numeric precision
    INTEGER,          PARAMETER :: IP = I4B         !! kind of indices
    INTEGER,          PARAMETER :: FP = DP          !! kind of default floating point (real) number
    ! angle unit flag
    INTEGER,          PARAMETER :: Degree = 1    !! 
	INTEGER,          PARAMETER :: Radian = 2    !! 
    ! commonly used numeric constants (i.e. whole real number)
	REAL(KIND=FP),    PARAMETER     :: Zero	        = 0.0_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: One	        = 1.0_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Two	        = 2.0_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Three	    = 3.0_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Four	        = 4.0_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Five	        = 5.0_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Six	        = 6.0_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: Seven        = 7.0_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: Eight        = 8.0_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: Nine	        = 9.0_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: Ten	        = 10.0_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: Hundred	    = 100.0_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: Thousand     = 1000.0_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: Million      = 1000000.0_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: Billion      = 1000000000.0_FP    !! 
    ! common fractions
	REAL(KIND=FP),    PARAMETER     :: Quater	    = 0.25_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: Half	        = 0.5_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: ThreeQuater  = 0.75_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: OneThird     = One/Three    !! 
    REAL(KIND=FP),    PARAMETER     :: TwoThird     = Two/Three    !! 
    ! frequently used mathematical constants (with precision to spare):
	REAL(KIND=FP),    PARAMETER     :: Pi           = 3.141592653589793238462643383279502884197169399375105820974944592307_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: PiOvr2       = Half * Pi    !! 
	REAL(KIND=FP),    PARAMETER     :: Pi3Ovr2      = 1.5_FP * Pi    !! 
	REAL(KIND=FP),    PARAMETER     :: TwoPi        = Two * Pi    !! 
    ! defined tolerance value
	REAL(KIND=FP),    PARAMETER     :: Zero01       = 1.0E-1_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero02       = 1.0E-2_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero03       = 1.0E-3_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero04       = 1.0E-4_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero05       = 1.0E-5_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero06       = 1.0E-6_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero07       = 1.0E-7_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero08       = 1.0E-8_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero09       = 1.0E-9_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero10       = 1.0E-10_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero11       = 1.0E-11_FP    !! 
    REAL(KIND=FP),    PARAMETER     :: Zero12       = 1.0E-12_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero13       = 1.0E-13_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero14       = 1.0E-14_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero15       = 1.0E-15_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero16       = 1.0E-16_FP    !! 
	REAL(KIND=FP),    PARAMETER     :: Zero17       = 1.0E-17_FP    !! 
    ! machine dependent parameters
    REAL(KIND=FP),    PARAMETER     :: MachineEps   = EPSILON(One)      !! machine epsilon
    REAL(KIND=FP),    PARAMETER     :: Small        = TINY(One)         !! the smallest positive number
    REAL(KIND=FP),    PARAMETER     :: Large        = HUGE(One)         !! the largest positive number
    REAL(KIND=FP),    PARAMETER     :: SqrtEps      = SQRT(MachineEps)  !! square root of MachineEps
    ! huge (maximum) numbers for intrinsic types used to prevent overflow
    INTEGER(KIND=I1B),PARAMETER     :: Huge_I1B = HUGE(1_I1B)       !! = 127
    INTEGER(KIND=I2B),PARAMETER     :: Huge_I2B = HUGE(1_I2B)       !! = 32767
    INTEGER(KIND=I4B),PARAMETER     :: Huge_I4B = HUGE(1_I4B)       !! = 2147483647
    INTEGER(KIND=I8B),PARAMETER     :: Huge_I8B = HUGE(1_I8B)       !! = 9223372036854775807
    REAL(KIND=SP),    PARAMETER     :: Huge_RSP = HUGE(1.0_SP)      !! = 3.4028235E+38
    REAL(KIND=DP),    PARAMETER     :: Huge_RDP = HUGE(1.0_DP)      !! = 1.797693134862316E+308
    REAL(KIND=QP),    PARAMETER     :: Huge_RQP = HUGE(1.0_QP)      !! = 1.189731495357231765085759326628007E+4932
    ! tiny (positive minimum) numbers for floating point types used to prevent underflow (normal range)
    REAL(KIND=SP),    PARAMETER     :: Tiny_RSP = TINY(1.0_SP)      !! = 1.1754944E-38
    REAL(KIND=DP),    PARAMETER     :: Tiny_RDP = TINY(1.0_DP)      !! = 2.225073858507201E-308
    REAL(KIND=QP),    PARAMETER     :: Tiny_RQP = TINY(1.0_QP)      !! = 3.362103143112093506262677817321753E-4932
    ! machine epsilon numbers for floating point types used to check accuracy tolerance
    REAL(KIND=SP),    PARAMETER     :: Eps_RSP = EPSILON(1.0_SP)    !! = 1.1920929E-07
    REAL(KIND=DP),    PARAMETER     :: Eps_RDP = EPSILON(1.0_DP)    !! = 2.220446049250313E-016
    REAL(KIND=QP),    PARAMETER     :: Eps_RQP = EPSILON(1.0_QP)    !! = 1.925929944387235853055977942584927E-0034
    ! miscellaneous
    CHARACTER(LEN=*), PARAMETER :: LibName = "XPFLib"               !! library name

!** DERIVED TYPE DEFINITIONS
    TYPE Equation
        !^ an equation type that can be used to parse strings/texts expressing
        !   mathematical expressions to 'FunctionParser' so that a system of
        !   equations can be evaluated at runtime.
        INTEGER(KIND=IP)                :: NEQ          !! number of equations
        INTEGER(KIND=IP)                :: NVR          !! number of variables
        CHARACTER(LEN=256), ALLOCATABLE :: EQText(:)    !! texts expressing equations
        CHARACTER(LEN=30),  ALLOCATABLE :: VarText(:)   !! texts expressing variable names
        REAL(KIND=FP),      ALLOCATABLE :: Values(:)    !! values of variables
    END TYPE Equation
    TYPE UserParam
        !! a user parameter type used to modernize legacy code
        INTEGER(KIND=IP)                :: NR   !! number of real parameters
        INTEGER(KIND=IP)                :: NI   !! number of integer parameters
        REAL(KIND=FP),    ALLOCATABLE   :: RPar(:)  !! real parameters
        INTEGER(KIND=IP), ALLOCATABLE   :: IPar(:)  !! integer parameters
    END TYPE UserParam
	TYPE WorkSpace
        !! a workspace type used to modernize legacy code
        INTEGER(KIND=IP)                :: LRW      !! number of real workspace variables
        INTEGER(KIND=IP)                :: LIW      !! number of integer workspace variables
        REAL(KIND=FP),    ALLOCATABLE   :: RVar(:)  !! real workspace variables
        INTEGER(KIND=IP), ALLOCATABLE   :: IVar(:)  !! integer workspace variables
    END TYPE WorkSpace
	TYPE SharePar(NR,NI)
        !! a share parameter type used to modernize common block of legacy code
        INTEGER(KIND=IP), LEN   :: NR,NI        !! number of real and integer parameters
        REAL(KIND=FP)           :: RPar(NR)     !! real parameters
        INTEGER(KIND=IP)        :: IPar(NI)     !! integer parameters
    END TYPE SharePar
    TYPE SaveVar(NR,NI,NL)
        !! a saved variable type used to modernize legacy code
        INTEGER(KIND=IP), LEN   :: NR,NI,NL     !! number of real, integer and logical parameters
        REAL(KIND=FP)           :: RVar(NR)     !! number of real parameters
        INTEGER(KIND=IP)        :: IVar(NI)     !! number of integer parameters
        LOGICAL                 :: LVar(NL)     !! number of logical parameters
    END TYPE SaveVar
	TYPE, BIND(C) :: Container
        !! container type that utilize 'c' pointer (C_PTR) type
        TYPE(C_PTR)     :: Store = C_NULL_PTR
    END TYPE Container
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

END MODULE ModBase_Common

!******************************************************************************
