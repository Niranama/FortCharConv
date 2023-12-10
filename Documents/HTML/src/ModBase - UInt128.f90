
MODULE ModBase_UInt128

!^ **PURPOSE OF THIS MODULE**:  
    !	This module contains a derived type and basic operations for a 128-bit unsigned integer.  
    !  The application interface (API) follows Fortran intrinsic integer types as close as practical.
    !  However, since Fortran does not have an unsigned integer type, those operations that differ
    !  between unsigned and signed integers use API similar to those used in 'ModBase_UIntUtil' module.  
    !  ***Important Note***:  
    !   (1) For arithmetic operations, various types of unsigned integer types (32-, 64- and 128-bit)
    !   are allowed.  However, the use of signed and unsigned integers in the same operation is NOT
    !   allowed.  Signed integer types must be explicitly converted to unsigned types before using
    !   in the arithmetic operations.  
    !   (2) For comparison and bitwise operations that require two input arguments, both arguments must
    !   only be the 128-bit unsigned integer type.  All other types must be explicitly converted to
    !   this type before using in the comparison and bitwise operations.  
    !  
!^ **REFERENCES**:  
    !	[1] [Absl's Numeric Library](https://github.com/abseil/abseil-cpp/tree/master/absl/numeric)  
    !	[2] [Fast 128-bit math library for Java](https://github.com/martint/int128/)  
    !	[3] [Extended precision integer C++ library](https://github.com/chfast/intx)

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Error_Handlers
    USE ModBase_SIntUtil
    USE ModBase_UIntUtil
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE, INTRINSIC :: ISO_FORTRAN_ENV,  ONLY: OUTPUT_UNIT

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type + constructor
    PUBLIC :: UInt128
    ! assignment (only applicable for unsigned integers)
    PUBLIC :: ASSIGNMENT(=)
    ! conversion
    PUBLIC :: ToI32, ToI64, ToR32, ToR64, ToR128
    PUBLIC :: ToDecString, ToHexString
    ! comparison
    PUBLIC :: OPERATOR(==), OPERATOR(/=)
    PUBLIC :: OPERATOR(.ULT.), OPERATOR(.ULE.)
    PUBLIC :: OPERATOR(.UGT.), OPERATOR(.UGE.)
    PUBLIC :: CompareUnsigned
    ! arithmetic
    PUBLIC :: OPERATOR(+), OPERATOR(-)
    PUBLIC :: OPERATOR(*), OPERATOR(.UDIV.)
    PUBLIC :: Increment, Decrement, Add, Subtract
    PUBLIC :: Multiply, Divide, UMOD, UDivMod
    ! bitwise (general)
    PUBLIC :: SHIFTL, SHIFTR, ISHFT, ISHFTC
    PUBLIC :: IOR, IEOR, IAND, NOT, LEADZ, TRAILZ
    PUBLIC :: POPCNT, POPPAR, IBSET, IBCLR
    PUBLIC :: IBCHNG, BTEST, IBITS
    PUBLIC :: MoveBits  ! == MVBITS
    ! bitwise (specialized)
    PUBLIC :: ShiftLOnce, ShiftROnce
    PUBLIC :: ShiftL64, ShiftR64
    PUBLIC :: ShiftL63Down, ShiftR63Down
    PUBLIC :: ShiftL64Up, ShiftR64Up
    ! auxiliary
    PUBLIC :: Display

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! module name
    CHARACTER(LEN=*),   PARAMETER :: ModName = 'ModBase_UInt128'
    ! unsigned limit parameters
    INTEGER(KIND=I8B),  PARAMETER :: MaxU64 = INT(Z'FFFFFFFFFFFFFFFF', KIND=I8B)    ! max unsigned 64-bit
    INTEGER(KIND=I8B),  PARAMETER :: MinU64 = INT(Z'0000000000000000', KIND=I8B)    ! min unsigned 64-bit
    INTEGER(KIND=I8B),  PARAMETER :: MaxU32 = INT(Z'00000000FFFFFFFF', KIND=I8B)    ! max unsigned 32-bit
    INTEGER(KIND=I8B),  PARAMETER :: MinU32 = INT(Z'0000000000000000', KIND=I8B)    ! min unsigned 32-bit
    ! signed limit parameters
    INTEGER(KIND=I8B),  PARAMETER :: MaxI64 = INT(Z'7FFFFFFFFFFFFFFF', KIND=I8B)    ! max signed 64-bit
    INTEGER(KIND=I8B),  PARAMETER :: MinI64 = INT(Z'8000000000000000', KIND=I8B)    ! min signed 64-bit
    INTEGER(KIND=I8B),  PARAMETER :: MaxI32 = INT(Z'000000007FFFFFFF', KIND=I8B)    ! max signed 32-bit
    INTEGER(KIND=I8B),  PARAMETER :: MinI32 = INT(Z'0000000080000000', KIND=I8B)    ! min signed 32-bit
    ! miscellaneous
    INTEGER(KIND=I8B),  PARAMETER :: Mask32 = MaxU32
    INTEGER(KIND=I8B),  PARAMETER :: TopBit = SHIFTL(1_I8B, 63)
    LOGICAL,             PARAMETER :: AsUnsigned = TrueVal
    INTEGER(KIND=I4B),  PARAMETER :: RecTable(0:255) = [                    &
             2045,  2037,  2029,  2021,  2013,  2005,  1998,  1990, &
             1983,  1975,  1968,  1960,  1953,  1946,  1938,  1931, &
             1924,  1917,  1910,  1903,  1896,  1889,  1883,  1876, &
             1869,  1863,  1856,  1849,  1843,  1836,  1830,  1824, &
             1817,  1811,  1805,  1799,  1792,  1786,  1780,  1774, &
             1768,  1762,  1756,  1750,  1745,  1739,  1733,  1727, &
             1722,  1716,  1710,  1705,  1699,  1694,  1688,  1683, &
             1677,  1672,  1667,  1661,  1656,  1651,  1646,  1641, &
             1636,  1630,  1625,  1620,  1615,  1610,  1605,  1600, &
             1596,  1591,  1586,  1581,  1576,  1572,  1567,  1562, &
             1558,  1553,  1548,  1544,  1539,  1535,  1530,  1526, &
             1521,  1517,  1513,  1508,  1504,  1500,  1495,  1491, &
             1487,  1483,  1478,  1474,  1470,  1466,  1462,  1458, &
             1454,  1450,  1446,  1442,  1438,  1434,  1430,  1426, &
             1422,  1418,  1414,  1411,  1407,  1403,  1399,  1396, &
             1392,  1388,  1384,  1381,  1377,  1374,  1370,  1366, &
             1363,  1359,  1356,  1352,  1349,  1345,  1342,  1338, &
             1335,  1332,  1328,  1325,  1322,  1318,  1315,  1312, &
             1308,  1305,  1302,  1299,  1295,  1292,  1289,  1286, &
             1283,  1280,  1276,  1273,  1270,  1267,  1264,  1261, &
             1258,  1255,  1252,  1249,  1246,  1243,  1240,  1237, &
             1234,  1231,  1228,  1226,  1223,  1220,  1217,  1214, &
             1211,  1209,  1206,  1203,  1200,  1197,  1195,  1192, &
             1189,  1187,  1184,  1181,  1179,  1176,  1173,  1171, &
             1168,  1165,  1163,  1160,  1158,  1155,  1153,  1150, &
             1148,  1145,  1143,  1140,  1138,  1135,  1133,  1130, &
             1128,  1125,  1123,  1121,  1118,  1116,  1113,  1111, &
             1109,  1106,  1104,  1102,  1099,  1097,  1095,  1092, &
             1090,  1088,  1086,  1083,  1081,  1079,  1077,  1074, &
             1072,  1070,  1068,  1066,  1064,  1061,  1059,  1057, &
             1055,  1053,  1051,  1049,  1047,  1044,  1042,  1040, &
             1038,  1036,  1034,  1032,  1030,  1028,  1026,  1024]

!** DERIVED TYPE DEFINITIONS
    !# a 128-bit unsigned integer type where the base of its components is 2**64.  
    TYPE UInt128
        INTEGER(KIND=I8B)   :: High !! upper 64 bits treated as unsigned integer
        INTEGER(KIND=I8B)   :: Low  !! lower 64 bits treated as unsigned integer
    END TYPE UInt128

!** MODULE PARAMETERS (PART 2):
    !# 128-bit unsigned parameter with maximum value
    TYPE(UInt128), PARAMETER, PUBLIC :: MaxU128   = UInt128(MaxU64, MaxU64)
    !# 128-bit unsigned parameter with minimum value
    TYPE(UInt128), PARAMETER, PUBLIC :: MinU128   = UInt128(MinU64, MinU64)
    !# 128-bit unsigned parameter with value of one
    TYPE(UInt128), PARAMETER, PUBLIC :: OneU128   = UInt128(MinU64, 1_I8B)
    !# 128-bit unsigned parameter with value of zero
    TYPE(UInt128), PARAMETER, PUBLIC :: ZeroU128  = MinU128
    !# Used to cast a 64-bit integer to a 128bit integer without getting unwanted sign extension
    TYPE(UInt128), PARAMETER, PUBLIC :: MASKI64   = UInt128(MinU64, MaxU64)

!** INTERFACE DEFINITIONS:
	!-----------------------------------------------
    !----- 	        conversion operations 	   -----
	!-----------------------------------------------
    INTERFACE ASSIGNMENT(=)
        !^ **Operator Overload**: ASSIGNMENT(=)  
        !  **Purpose**:  To convert between a 128-bit unsigned integer and
        !   other unsigned integers (32- and 64-bit integers)  
        !  **Usage**:  
        !   --->    U128 = OtherType
        MODULE PROCEDURE U128_From_U32
        MODULE PROCEDURE U128_From_U64
        MODULE PROCEDURE U128_To_U32
        MODULE PROCEDURE U128_To_U64
    END INTERFACE
    INTERFACE UInt128
        !^ **Constructor Interface**: UInt128  
        !  **Purpose**:  To construct a 128-bit unsigned integer from
        !   other Fortran intrinsic types  
        !  **Usage**:  
        !   --->    U128 = UInt128(OtherType)
        MODULE PROCEDURE I32_To_U128
        MODULE PROCEDURE I64_To_U128
        MODULE PROCEDURE R32_To_U128
        MODULE PROCEDURE R64_To_U128
        MODULE PROCEDURE R128_To_U128
        MODULE PROCEDURE DecString_To_U128
    END INTERFACE
    INTERFACE ToI32
        !^ **Function Interface**: ToI32  
        !  **Purpose**:  To convert a 128-bit unsigned integer to a 
        !   32-bit signed integer  
        !  **Usage**:  
        !   --->    I32 = ToI32(U128)
        MODULE PROCEDURE I32_From_U128
    END INTERFACE
    INTERFACE ToI64
        !^ **Function Interface**: ToI64  
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a 64-bit signed integer  
        !  **Usage**:  
        !   --->    I64 = ToI64(U128)
        MODULE PROCEDURE I64_From_U128
    END INTERFACE
    INTERFACE ToR32
        !^ **Function Interface**: ToR32  
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a 32-bit floating point (real) number  
        !  **Usage**:  
        !   --->    R32 = ToR32(U128)
        MODULE PROCEDURE R32_From_U128
    END INTERFACE
    INTERFACE ToR64
        !^ **Function Interface**: ToR64  
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a 64-bit floating point (real) number  
        !  **Usage**:  
        !   --->    R64 = ToR64(U128)
        MODULE PROCEDURE R64_From_U128
    END INTERFACE
    INTERFACE ToR128
        !^ **Function Interface**: ToR128  
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a 128-bit floating point (real) number  
        !  **Usage**:  
        !   --->    R128 = ToR128(U128)
        MODULE PROCEDURE R128_From_U128
    END INTERFACE
    INTERFACE ToDecString
        !^ **Function Interface**: ToDecString  
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a decimal string  
        !  **Usage**:  
        !   --->    Str = ToDecString(U128)
        MODULE PROCEDURE DecString_From_U128
    END INTERFACE
    INTERFACE ToHexString
        !^ **Function Interface**: ToHexString  
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a hexadecimal string  
        !  **Usage**:  
        !   --->    Str = ToHexString(U128)
        MODULE PROCEDURE HexString_From_U128
    END INTERFACE
	!-----------------------------------------------
    !----- 		    comparison operations	       -----
	!-----------------------------------------------
    INTERFACE OPERATOR(==)
        !^ **Operator Overload**: OPERATOR(==)  
        !  **Purpose**:  To check if values of two 128-bit unsigned integers are equal  
        !   return .TRUE. if both values are equal; otherwise return .FALSE.  
        !  **Usage**:  
        !   --->    Flag = LHS == RHS  
        !   --->    IF (LHS .EQ. RHS) DoSomething
        MODULE PROCEDURE U128_Equal
    END INTERFACE
    INTERFACE OPERATOR(/=)
        !^ **Operator Overload**: OPERATOR(/=)  
        !  **Purpose**:  To check if values of two 128-bit unsigned integers are not equal  
        !   return .TRUE. if both values are NOT equal; otherwise return .FALSE.  
        !  **Usage**:  
        !   --->    Flag = LHS /= RHS  
        !   --->    IF (LHS .NE. RHS) DoSomething
        MODULE PROCEDURE U128_NotEqual
    END INTERFACE
    INTERFACE OPERATOR(.ULT.)
        !^ **Operator Overload**: OPERATOR(.ULT.)  
        !  **Purpose**:  To check if the LHS value is less than the RHS value  
        !   return .TRUE. if LHS < RHS; otherwise return .FALSE.  
        !  **Usage**:  
        !   --->    Flag = LHS .ULT. RHS  
        !   --->    IF (LHS .ULT. RHS) DoSomething
        MODULE PROCEDURE U128_LessThan
    END INTERFACE
    INTERFACE OPERATOR(.ULE.)
        !^ **Operator Overload**: OPERATOR(.ULE.)  
        !  **Purpose**:  To check if the LHS value is less than or equal to the RHS value  
        !   return .TRUE. if LHS <= RHS; otherwise return .FALSE.  
        !  **Usage**:  
        !   --->    Flag = LHS .ULE. RHS  
        !   --->    IF (LHS .ULE. RHS) DoSomething
        MODULE PROCEDURE U128_LessEqual
    END INTERFACE
    INTERFACE OPERATOR(.UGT.)
        !^ **Operator Overload**: OPERATOR(.UGT.)  
        !  **Purpose**:  To check if the LHS value is greater than the RHS value  
        !   return .TRUE. if LHS > RHS; otherwise return .FALSE.  
        !  **Usage**:  
        !   --->    Flag = LHS .UGT. RHS  
        !   --->    IF (LHS .UGT. RHS) DoSomething
        MODULE PROCEDURE U128_GreaterThan
    END INTERFACE
    INTERFACE OPERATOR(.UGE.)
        !^ **Operator Overload**: OPERATOR(.UGE.)  
        !  **Purpose**:  To check if the LHS value is greater than or equal to the RHS value  
        !   return .TRUE. if LHS >= RHS; otherwise return .FALSE.  
        !  **Usage**:  
        !   --->    Flag = LHS .UGE. RHS  
        !   --->    IF (LHS .UGE. RHS) DoSomething
        MODULE PROCEDURE U128_GreaterEqual
    END INTERFACE
    INTERFACE CompareUnsigned
        !^ **Function Interface**: CompareUnsigned  
        !  **Purpose**:  To compare two 128-bit unsigned integers and return  
        !   -1 if LHS < RHS  
        !    0 if LHS == RHS  
        !    1 if LHS > RHS  
        !  **Usage**:  
        !   --->    Flag = CompareUnsigned(LHS, RHS)  
        !   --->    IF (CompareUnsigned(LHS, RHS) /= 0) DoSomething
        MODULE PROCEDURE U128_Compare
    END INTERFACE
	!-----------------------------------------------
    !----- 		 arithmetic operations          -----
	!-----------------------------------------------
    INTERFACE OPERATOR(+)
        !^ **Operator Overload**: OPERATOR(+)  
        !  **Purpose**:  To perform a summation of two unsigned integers
        !   (at least one of which is a 128-bit unsigned integer) or
        !   to add a unary plus sign to a 128-bit unsigned integer
        !   (which has no effect on the unsigned integer)  
        !  **Usage**:  
        !   --->    OUTPUT = +INPUT  
        !   --->    OUTPUT = FIRST_IN + SECOND_IN
        MODULE PROCEDURE U128_UnaryPlus
        MODULE PROCEDURE U128_Plus_U128
        MODULE PROCEDURE U128_Plus_U32
        MODULE PROCEDURE U32_Plus_U128
        MODULE PROCEDURE U128_Plus_U64
        MODULE PROCEDURE U64_Plus_U128
    END INTERFACE
    INTERFACE OPERATOR(-)
        !^ **Operator Overload**: OPERATOR(-)  
        !  **Purpose**:  To perform a subtraction of two unsigned integers
        !   (at least one of which is a 128-bit unsigned integer) or
        !   to perform a negation of a 128-bit unsigned integer   
        !  **Usage**:  
        !   --->    OUTPUT = -INPUT  
        !   --->    OUTPUT = FIRST_IN - SECOND_IN  
        !  ***Important Note***:  For subtraction of unsigned integers, value of FIRST_IN
        !   must always be greater than SECOND_IN.  Otherwise, value of OUTPUT
        !   is NOT valid.
        MODULE PROCEDURE U128_Negate
        MODULE PROCEDURE U128_Minus_U128
        MODULE PROCEDURE U128_Minus_U32
        MODULE PROCEDURE U32_Minus_U128
        MODULE PROCEDURE U128_Minus_U64
        MODULE PROCEDURE U64_Minus_U128
    END INTERFACE
    INTERFACE OPERATOR(*)
        !^ **Operator Overload**: OPERATOR( * )  
        !  **Purpose**:  To perform a multiplication of two unsigned integers  
        !   (at least one of which is a 128-bit unsigned integer)   
        !  **Usage**:  
        !   --->    OUTPUT = FIRST_IN * SECOND_IN
        MODULE PROCEDURE U128_Multiply_U128
        MODULE PROCEDURE U128_Multiply_U32
        MODULE PROCEDURE U32_Multiply_U128
        MODULE PROCEDURE U128_Multiply_U64
        MODULE PROCEDURE U64_Multiply_U128
    END INTERFACE
    INTERFACE OPERATOR(.UDIV.)
        !^ **Operator Overload**: OPERATOR(.UDIV.)  
        !  **Purpose**:  To return the quotient of a division of two unsigned integers,
        !   where the dividend (numerator) is a 128-bit unsigned integer and the
        !   divisor (denominator) can be 32-, 64- or 128-bit unsigned integer  
        !  **Usage**:  
        !   --->    QUOT = NUMER .UDIV. DENOM
        MODULE PROCEDURE U128_Divide_U32
        MODULE PROCEDURE U128_Divide_U64
        MODULE PROCEDURE U128_Divide_U128
    END INTERFACE
    INTERFACE UMOD
        !^ **Function Interface**: UMOD  
        !  **Purpose**:  To return the remainder of a division of two unsigned integers,
        !   where the dividend (numerator) is a 128-bit unsigned integer and the
        !   divisor (denominator) can be 32-, 64- or 128-bit unsigned integer  
        !  **Usage**:  
        !   --->    REM = UMOD(NUMER, DENOM)
        MODULE PROCEDURE U128_Mod_U32
        MODULE PROCEDURE U128_Mod_U64
        MODULE PROCEDURE U128_Mod_U128
    END INTERFACE
    INTERFACE UDivMod
        !^ **Subroutine Interface**: UDivMod  
        !  **Purpose**:  To perform a division of two unsigned integers (where the
        !   dividend (numerator) is a 128-bit unsigned integer and the divisor
        !   (denominator) can be 32-, 64- or 128-bit unsigned integer) and
        !   to return both the quotient and the remainder  
        !  **Usage**:  
        !   --->    CALL UDivMod(NUMER, DENOM, QUOT, REM)
        MODULE PROCEDURE U128_DivMod_U32
        MODULE PROCEDURE U128_DivMod_U64
        MODULE PROCEDURE U128_DivMod_U128
    END INTERFACE
    INTERFACE Increment
        !^ **Subroutine Interface**: Increment  
        !  **Purpose**:  To increase value of a 128-bit unsigned integer by one  
        !  **Usage**:  
        !   --->    CALL Increment(U128)
        MODULE PROCEDURE U128_Increment
    END INTERFACE
    INTERFACE Decrement
        !^ **Subroutine Interface**: Decrement  
        !  **Purpose**:  To decrease value of a 128-bit unsigned integer by one  
        !  **Usage**:  
        !   --->    CALL Decrement(U128)
        MODULE PROCEDURE U128_Decrement
    END INTERFACE
    INTERFACE Add
        !^ **Subroutine Interface**: Add  
        !  **Purpose**:  To add an unsigned integer to a 128-bit unsigned integer  
        !  **Usage**:  
        !   --->    CALL Add(This, Other)
        MODULE PROCEDURE U128_Add_U32
        MODULE PROCEDURE U128_Add_U64
        MODULE PROCEDURE U128_Add_U128
    END INTERFACE
    INTERFACE Subtract
        !^ **Subroutine Interface**: Subtract  
        !  **Purpose**:  To subtract an unsigned integer from a 128-bit unsigned integer  
        !  **Usage**:  
        !   --->    CALL Subtract(This, Other)  
        !  ***Important Note***:  For subtraction of unsigned integers, value of This
        !   must always be greater than Other.  Otherwise, value of the returned
        !   This is NOT valid.
        MODULE PROCEDURE U128_Subtract_U32
        MODULE PROCEDURE U128_Subtract_U64
        MODULE PROCEDURE U128_Subtract_U128
    END INTERFACE
    INTERFACE Multiply
        !^ **Subroutine Interface**: Multiply  
        !  **Purpose**:  To multiply a 128-bit unsigned integer by an unsigned integer  
        !  **Usage**:  
        !   --->    CALL Multiply(This, Other)
        MODULE PROCEDURE U128_Times_U32
        MODULE PROCEDURE U128_Times_U64
        MODULE PROCEDURE U128_Times_U128
    END INTERFACE
    INTERFACE Divide
        !^ **Subroutine Interface**: Divide  
        !  **Purpose**:  To divide a 128-bit unsigned integer by an unsigned integer  
        !  **Usage**:  
        !   --->    CALL Divide(This, Other)
        MODULE PROCEDURE U128_Over_U32
        MODULE PROCEDURE U128_Over_U64
        MODULE PROCEDURE U128_Over_U128
    END INTERFACE
	!-----------------------------------------------
    !----- 		    bitwise operations 		   -----
	!-----------------------------------------------
    INTERFACE ShiftLOnce
        !^ **Function Interface**: ShiftLOnce  
        !  **Purpose**:  To perform logical left shift by 1  
        !  **Usage**:  
        !   --->    OUT = ShiftLOnce(IN)
        MODULE PROCEDURE U128_ShiftLeftOnce
    END INTERFACE
    INTERFACE ShiftROnce
        !^ **Function Interface**: ShiftROnce  
        !  **Purpose**:  To perform logical right shift by 1  
        !  **Usage**:  
        !   --->    OUT = ShiftROnce(IN)
        MODULE PROCEDURE U128_ShiftRightOnce
    END INTERFACE
    INTERFACE ShiftL64
        !^ **Function Interface**: ShiftL64  
        !  **Purpose**:  To perform logical left shift by 64  
        !  **Usage**:  
        !   --->    OUT = ShiftL64(IN)
        MODULE PROCEDURE U128_ShiftLeft64
    END INTERFACE
    INTERFACE ShiftR64
        !^ **Function Interface**: ShiftR64  
        !  **Purpose**:  To perform logical right shift by 64  
        !  **Usage**:  
        !   --->    OUT = ShiftR64(IN)
        MODULE PROCEDURE U128_ShiftRight64
    END INTERFACE
    INTERFACE ShiftL63Down
        !^ **Function Interface**: ShiftL63Down  
        !  **Purpose**:  To perform logical left shift by 63 or less  
        !  **Usage**:  
        !   --->    OUT = ShiftL63Down(IN, 11)
        MODULE PROCEDURE U128_ShiftLeft63Down
    END INTERFACE
    INTERFACE ShiftR63Down
        !^ **Function Interface**: ShiftR63Down  
        !  **Purpose**:  To perform logical right shift by 63 or less  
        !  **Usage**:  
        !   --->    OUT = ShiftR63Down(IN, 53)
        MODULE PROCEDURE U128_ShiftRight63Down
    END INTERFACE
    INTERFACE ShiftL64Up
        !^ **Function Interface**: ShiftL64Up  
        !  **Purpose**:  To perform logical left shift by 64 or more (<= 128)  
        !  **Usage**:  
        !   --->    OUT = ShiftL64Up(IN, 111)
        MODULE PROCEDURE U128_ShiftLeft64Up
    END INTERFACE
    INTERFACE ShiftR64Up
        !^ **Function Interface**: ShiftR64Up  
        !  **Purpose**:  To perform logical right shift by 64 or more (<= 128)  
        !  **Usage**:  
        !   --->    OUT = ShiftR64Up(IN, 84)
        MODULE PROCEDURE U128_ShiftRight64Up
    END INTERFACE
    INTERFACE SHIFTL
        !^ **Function Interface**: SHIFTL  
        !  **Purpose**:  To perform logical left shift with 0 <= ShiftPos <= 128  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = SHIFTL(IN, 127)
        MODULE PROCEDURE U128_ShiftLeft
    END INTERFACE
    INTERFACE SHIFTR
        !^ **Function Interface**: SHIFTR  
        !  **Purpose**:  To perform logical right shift with 0 <= ShiftPos <= 128  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = SHIFTR(IN, 33)
        MODULE PROCEDURE U128_ShiftRight
    END INTERFACE
    INTERFACE ISHFT
        !^ **Function Interface**: ISHFT  
        !  **Purpose**:  To perform logical shift with -128 <= ShiftPos <= 128  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = ISHFT(IN, 53)    ! a logical left shift by 53  
        !   --->    OUT = ISHFT(IN, -24)   ! a logical right shift by 24
        MODULE PROCEDURE U128_ShiftLogical
    END INTERFACE
    INTERFACE ISHFTC
        !^ **Function Interface**: ISHFTC  
        !  **Purpose**:  To perform circular shift with -128 <= ShiftPos <= 128  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = ISHFTC(IN, 53)    ! a circular left shift by 53  
        !   --->    OUT = ISHFTC(IN, -24)   ! a circular right shift by 24
        MODULE PROCEDURE U128_Rotate
    END INTERFACE
    INTERFACE NOT
        !^ **Function Interface**: NOT  
        !  **Purpose**:  To return the bitwise logical complement of the input  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = NOT(IN)
        MODULE PROCEDURE U128_Not
    END INTERFACE
    INTERFACE IOR
        !^ **Function Interface**: IOR  
        !  **Purpose**:  To perform an inclusive OR on corresponding bits of the input  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = IOR(LHSIN, RHSIN)
        MODULE PROCEDURE U128_Ior
    END INTERFACE
    INTERFACE IEOR
        !^ **Function Interface**: IEOR  
        !  **Purpose**:  To perform an exclusive OR on corresponding bits of the input  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = IEOR(LHSIN, RHSIN)
        MODULE PROCEDURE U128_Ieor
    END INTERFACE
    INTERFACE IAND
        !^ **Function Interface**: IAND  
        !  **Purpose**:  To perform a logical AND on corresponding bits of the input  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = IAND(LHSIN, RHSIN)
        MODULE PROCEDURE U128_Iand
    END INTERFACE
    INTERFACE LEADZ
        !^ **Function Interface**: LEADZ  
        !  **Purpose**:  To count the number of leading zero bits of the input  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    NumLZ = LEADZ(INPUT)
        MODULE PROCEDURE U128_LeadingZeros
    END INTERFACE
    INTERFACE TRAILZ
        !^ **Function Interface**: TRAILZ  
        !  **Purpose**:  To count the number of trailing zero bits of the input  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    NumTZ = TRAILZ(INPUT)
        MODULE PROCEDURE U128_TrailingZeros
    END INTERFACE
    INTERFACE POPCNT
        !^ **Function Interface**: POPCNT  
        !  **Purpose**:  To count the number of 1 bits in the input  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    NumBits = POPCNT(INPUT)
        MODULE PROCEDURE U128_Count1Bits
    END INTERFACE
    INTERFACE POPPAR
        !^ **Function Interface**: POPPAR  
        !  **Purpose**:  To determine the parity of the input  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    NumPar = POPPAR(INPUT)
        MODULE PROCEDURE U128_Parity
    END INTERFACE
    INTERFACE IBSET
        !^ **Function Interface**: IBSET  
        !  **Purpose**:  To set the bit at the specified position to 1  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = IBSET(IN, Pos)
        MODULE PROCEDURE U128_SetBit
    END INTERFACE
    INTERFACE IBCLR
        !^ **Function Interface**: IBCLR  
        !  **Purpose**:  To set the bit at the specified position to 0  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = IBCLR(IN, Pos)
        MODULE PROCEDURE U128_ClearBit
    END INTERFACE
    INTERFACE IBCHNG
        !^ **Function Interface**: IBCHNG  
        !  **Purpose**:  To reverse the bit at the specified position  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = IBCHNG(IN, Pos)
        MODULE PROCEDURE U128_FlipBit
    END INTERFACE
    INTERFACE BTEST
        !^ **Function Interface**: BTEST  
        !  **Purpose**:  To check whether the bit at the specified position is 0 (False) or 1 (True)  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    Flag = BTEST(IN, Pos)
        MODULE PROCEDURE U128_TestBit
    END INTERFACE
    INTERFACE IBITS
        !^ **Function Interface**: IBITS  
        !  **Purpose**:  To extract a sequence of bits according to the specified input  
        !   (For more information, see detailed explanation of the intrinsic function)  
        !  **Usage**:  
        !   --->    OUT = IBITS(IN, Pos, Len)
        MODULE PROCEDURE U128_ExtractBits
    END INTERFACE
    INTERFACE MoveBits
        !^ **Subroutine Interface**: MoveBits  
        !  **Purpose**:  To copy a sequence of bits (a bit field) from one location to another  
        !   (For more information, see detailed explanation of the intrinsic subroutine 'MVBITS')  
        !  **Usage**:  
        !   --->    CALL MoveBits(InVal, InPos, Len, OutVal, OutPos)
        MODULE PROCEDURE U128_MoveBits
    END INTERFACE
	!-----------------------------------------------
    !----- 	        Auxiliary Routine 	       -----
	!-----------------------------------------------
    INTERFACE Display
        !^ **Subroutine Interface**: Display  
        !  **Purpose**:  To write/display the 'UInt128' object to the screen (or the specified unit)  
        !  **Usage**:  
        !   To display (unsigned) value of U128 as a decimal string to the screen  
        !   --->    CALL Display(U128)  
        !   To display (unsigned) value of U128 as a decimal string to the output logical unit  
        !   --->    CALL Display(U128, 11)  
        !   To display (unsigned) value of U128 as a decimal string to the output logical unit  
        !   with input/output status and message  
        !   --->    CALL Display(U128, 11, IOStat, IOMsg)  
        !   To display (signed) values of components of U128 as a decimal string to the screen  
        !   --->    CALL Display(U128, ShowComponent=.TRUE.)  
        !   To display (unsigned) value of U128 as a decimal string to the screen with a prefix string  
        !   --->    CALL Display(U128, Prefix='Unsigned value of U128')
        MODULE PROCEDURE U128_Write
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!------------------------------------------------------------------------------
!
!                           ASSIGNMENT ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE U128_From_U32(U128, U32)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 32-bit integer number to an unsigned 128-bit integer number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(OUT)   :: U128
    INTEGER(KIND=I4B),  INTENT(IN)    :: U32      !! number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U128 = UInt128(MinU64, ToUnsignedLong(U32))

    RETURN

END SUBROUTINE U128_From_U32

!******************************************************************************

SUBROUTINE U128_From_U64(U128, U64)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 64-bit integer number to an unsigned 128-bit integer number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(OUT)   :: U128
    INTEGER(KIND=I8B),  INTENT(IN)    :: U64      !! number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U128 = UInt128(MinU64, U64)

    RETURN

END SUBROUTINE U128_From_U64

!******************************************************************************

SUBROUTINE U128_To_U32(U32, U128)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 128-bit integer number to an unsigned 32-bit integer number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(OUT)     :: U32      !! number treated as unsigned
    TYPE(UInt128),      INTENT(IN)      :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U32 = INT(U128%Low, KIND=I4B)

    RETURN

END SUBROUTINE U128_To_U32

!******************************************************************************

SUBROUTINE U128_To_U64(U64, U128)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 128-bit integer number to an unsigned 64-bit integer number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(OUT)   :: U64      !! number treated as unsigned
    TYPE(UInt128),      INTENT(IN)    :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U64 = U128%Low

    RETURN

END SUBROUTINE U128_To_U64

!------------------------------------------------------------------------------
!
!                           CONSTRUCTOR ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION I32_To_U128(I32, AsUnsigned) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 32-bit integer number to an unsigned 128-bit integer number
    !  or to convert an unsigned 32-bit integer number to an unsigned 128-bit integer
    !  number if the specified flag is present and true.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B), INTENT(IN)   :: I32          !! number treated as signed (default)
    LOGICAL, OPTIONAL, INTENT(IN)   :: AsUnsigned   !! if present and true, number treated as unsigned
    TYPE(UInt128)                   :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (PRESENT(AsUnsigned)) THEN
        IF (AsUnsigned) THEN
            ! number treated as unsigned
            U128 = UInt128(MinU64, ToUnsignedLong(I32))
            RETURN
        END IF
    END IF
    ! number treated as signed
    IF (I32 < 0) THEN
        U128 = UInt128(MaxU64, INT(I32, KIND=I8B))
    ELSE
        U128 = UInt128(MinU64, INT(I32, KIND=I8B))
    END IF

    RETURN

END FUNCTION I32_To_U128

!******************************************************************************

FUNCTION I64_To_U128(I64, AsUnsigned) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a signed 64-bit integer number to an unsigned 128-bit integer number
    !! or to convert an unsigned 64-bit integer number to an unsigned 128-bit integer
    !! number if the specified flag is present and true.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)  :: I64          !! number treated as signed (default)
    LOGICAL, OPTIONAL, INTENT(IN)  :: AsUnsigned   !! if present and true, number treated as unsigned
    TYPE(UInt128)                  :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (PRESENT(AsUnsigned)) THEN
        IF (AsUnsigned) THEN
            ! number treated as unsigned
            U128 = UInt128(MinU64, I64)
            RETURN
        END IF
    END IF
    ! number treated as signed
    IF (I64 < 0_I8B) THEN
        U128 = UInt128(MaxU64, I64)
    ELSE
        U128 = UInt128(MinU64, I64)
    END IF

    RETURN

END FUNCTION I64_To_U128

!******************************************************************************

FUNCTION R32_To_U128(R32) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 32-bit floating point number to an unsigned 128-bit integer number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=SP), INTENT(IN)   :: R32
    TYPE(UInt128)               :: U128

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: Mask = INT(Z'000000FF', KIND=I4B)  ! 255
    INTEGER(KIND=I4B), PARAMETER  :: C1   = SHIFTL(1, 23)               ! 2**23
    INTEGER(KIND=I4B), PARAMETER  :: C2   = C1 - 1                      ! 2**23 - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: Exp
    INTEGER(KIND=I4B)   :: IBits
    REAL(KIND=SP)       :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R32 is NaN or cannot fit into U128.
    IF (.NOT.IEEE_IS_FINITE (R32)) THEN
        CALL DisplaySevereError('Message from Routine '//'R32_To_U128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R32 is NOT finite.')
        U128 = MaxU128
        RETURN
    ELSEIF (R32 <= -1.0_SP) THEN
        CALL DisplaySevereError('Message from Routine '//'R32_To_U128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R32 < U128Min.')
        U128 = MinU128
        RETURN
    ELSEIF (R32 >= 2.0_SP**128) THEN
        CALL DisplaySevereError('Message from Routine '//'R32_To_U128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R32 > U128Max.')
        U128 = MaxU128
        RETURN
    ELSEIF (R32 < 0.0_SP) THEN
        U128 = ZeroU128
        RETURN
    END IF

    ! transfer bits from real to integer
    RBits = R32
    ! determine exponent bits
    Exp = IAND(SHIFTR(IBits, 23), Mask) - 150   ! 150 = 127 + 23
    ! determine significand bits
    IBits = IOR(IAND(IBits, C2), C1)
    ! convert and add exponent bits
    ! => U128 = UInt128(MinU64, IBits)
    ! => U128 = ISHFT(U128, Exp)
    IF (Exp < 0) THEN
        Exp = -Exp
        IF (Exp >= 64) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSE
            U128 = UInt128(MinU64, SHIFTR(INT(IBits, KIND=I8B), Exp))
        END IF
    ELSE
        IF (Exp >= 128) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSEIF (Exp >= 64) THEN
            U128 = UInt128(SHIFTL(INT(IBits, KIND=I8B), Exp - 64), MinU64)
        ELSE
            U128 = UInt128(SHIFTR(INT(IBits, KIND=I8B), 64 - Exp), SHIFTL(INT(IBits, KIND=I8B), Exp))
        END IF
    END IF

    RETURN

END FUNCTION R32_To_U128

!******************************************************************************

FUNCTION R64_To_U128(R64) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 64-bit floating point number to an unsigned 128-bit integer number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=DP), INTENT(IN)   :: R64
    TYPE(UInt128)               :: U128

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I8B), PARAMETER  :: Mask = INT(Z'00000000000007FF', KIND=I8B)  ! 2047
    INTEGER(KIND=I8B), PARAMETER  :: C1   = SHIFTL(1_I8B, 52)                   ! 2**52
    INTEGER(KIND=I8B), PARAMETER  :: C2   = C1 - 1_I8B                          ! 2**52 - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: Exp
    INTEGER(KIND=I8B)   :: IBits
    REAL(KIND=DP)       :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R64 is NaN or cannot fit into U128.
    IF (.NOT.IEEE_IS_FINITE (R64)) THEN
        CALL DisplaySevereError('Message from Routine '//'R64_To_U128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R64 is NOT finite.')
        U128 = MaxU128
        RETURN
    ELSEIF (R64 <= -1.0_DP) THEN
        CALL DisplaySevereError('Message from Routine '//'R64_To_U128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R64 < U128Min.')
        U128 = MinU128
        RETURN
    ELSEIF (R64 >= 2.0_DP**128) THEN
        CALL DisplaySevereError('Message from Routine '//'R64_To_U128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R64 > U128Max.')
        U128 = MaxU128
        RETURN
    ELSEIF (R64 < 0.0_DP) THEN
        U128 = ZeroU128
        RETURN
    END IF

    ! transfer bits from real to integer
    RBits = R64
    ! determine exponent bits
    Exp = INT(IAND(SHIFTR(IBits, 52), Mask), KIND=I4B) - 1075   ! 1075 = 1023 + 52
    ! determine significand bits
    IBits = IOR(IAND(IBits, C2), C1)
    ! convert and add exponent bits
    ! => U128 = UInt128(MinU64, IBits)
    ! => U128 = ISHFT(U128, Exp)
    IF (Exp < 0) THEN
        Exp = -Exp
        IF (Exp >= 64) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSE
            U128 = UInt128(MinU64, SHIFTR(IBits, Exp))
        END IF
    ELSE
        IF (Exp >= 128) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSEIF (Exp >= 64) THEN
            U128 = UInt128(SHIFTL(IBits, Exp - 64), MinU64)
        ELSE
            U128 = UInt128(SHIFTR(IBits, 64 - Exp), SHIFTL(IBits, Exp))
        END IF
    END IF

    RETURN

END FUNCTION R64_To_U128

!******************************************************************************

FUNCTION R128_To_U128(R128) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a 128-bit floating point number to an unsigned 128-bit integer number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    REAL(KIND=QP), INTENT(IN)   :: R128
    TYPE(UInt128)               :: U128

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: Mask  = INT(Z'00007FFF', KIND=I4B)     ! 32767
    INTEGER(KIND=I8B), PARAMETER  :: C1(2) = [ 0_I8B, 281474976710656_I8B]  ! 2**112 = SHIFTL(1, 112)
    INTEGER(KIND=I8B), PARAMETER  :: C2(2) = [-1_I8B, 281474976710655_I8B]  ! 2**112 -1 = SHIFTL(1, 112) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: Exp
    INTEGER(KIND=I8B)   :: ExpL
    INTEGER(KIND=I8B)   :: IBits(2)
    REAL(KIND=QP)       :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R128 is NaN or cannot fit into U128.
    IF (.NOT.IEEE_IS_FINITE (R128)) THEN
        CALL DisplaySevereError('Message from Routine '//'R128_To_U128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R128 is NOT finite.')
        U128 = MaxU128
        RETURN
    ELSEIF (R128 <= -1.0_QP) THEN
        CALL DisplaySevereError('Message from Routine '//'R128_To_U128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R128 < U128Min.')
        U128 = MinU128
        RETURN
    ELSEIF (R128 >= 2.0_QP**128) THEN
        CALL DisplaySevereError('Message from Routine '//'R128_To_U128'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Undefined behavior: R128 > U128Max.')
        U128 = MaxU128
        RETURN
    ELSEIF (R128 < 0.0_QP) THEN
        U128 = ZeroU128
        RETURN
    END IF

    ! transfer bits from real to integer (assumming little-endian order)
    ! (if big-endian order, just switch IBits(1) and IBits(2))
    RBits = R128
    ! determine exponent bits
    ExpL = SHIFTR(IBits(2), 48)                         ! 48 = 112-64
    Exp  = IAND(INT(ExpL, KIND=I4B), Mask) - 16495      ! 16495 = 16383 + 112
    ! convert and add exponent bits
    U128 = UInt128(IOR(IAND(IBits(2), C2(2)), C1(2)), IOR(IAND(IBits(1), C2(1)), C1(1)))
    IF (Exp < 0) THEN
        Exp = -Exp
        ! perform right shift
        IF (Exp >= 128) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSEIF (Exp >= 64) THEN
            U128%Low  = SHIFTR(U128%High, Exp - 64)
            U128%High = MinU64
        ELSE
            U128%Low  = IOR(SHIFTR(U128%Low, Exp), SHIFTL(U128%High, 64 - Exp))
            U128%High = SHIFTR(U128%High, Exp)
        END IF
    ELSE
        ! perform left shift
        IF (Exp >= 128) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSEIF (Exp >= 64) THEN
            U128%High = SHIFTL(U128%Low, Exp - 64)
            U128%Low  = MinU64
        ELSE
            U128%High = IOR(SHIFTL(U128%High, Exp), SHIFTR(U128%Low, 64 - Exp))
            U128%Low  = SHIFTL(U128%Low, Exp)
        END IF
    END IF

    RETURN

END FUNCTION R128_To_U128

!******************************************************************************

FUNCTION DecString_To_U128(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert a decimal string to an unsigned 128-bit integer value.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CHARACTER(LEN=*), TARGET,                INTENT(IN)    :: cStr     !! character string
    LOGICAL,                       OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    TYPE(UInt128)                                          :: Number   !! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    INTEGER(KIND=I4B), PARAMETER  :: A0           = IACHAR('0')
    INTEGER(KIND=I4B), PARAMETER  :: A4           = IACHAR('4')
    INTEGER(KIND=I4B), PARAMETER  :: A9           = IACHAR('9')
    INTEGER(KIND=I4B), PARAMETER  :: MaxDigitI32  = 10
    INTEGER(KIND=I4B), PARAMETER  :: MaxDigitI64  = 19
    INTEGER(KIND=I4B), PARAMETER  :: MaxDigitU128 = 39
    CHARACTER(LEN=*),  PARAMETER  :: MaxStr       = '340282366920938463463374607431768211455'
    INTEGER(KIND=I8B), PARAMETER  :: Mask32       = MaxU32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)                       :: Indx, StrLen, DigitLen
    INTEGER(KIND=I4B)                       :: NumDigit
    INTEGER(KIND=I4B)                       :: IStart, IndxP7
    CHARACTER(LEN=1), POINTER               :: CurChr
    LOGICAL                                 :: Overflow
    CHARACTER(LEN=:), ALLOCATABLE,  TARGET  :: CurStr
    LOGICAL                                 :: ErrorFlag  ! true if input is not invalid
    CHARACTER(LEN=:), ALLOCATABLE           :: ErrorMsg   ! message if input is not invalid
    INTEGER(KIND=I4B)                       :: I32Val
    INTEGER(KIND=I8B)                       :: I64Val
    CHARACTER(LEN=8)                        :: wStr
    INTEGER(KIND=I8B)                       :: wVal
    EQUIVALENCE(wStr, wVal)

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
            Number = MinU128
            RETURN
        END IF
    END IF

    ! check for sign
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a minus sign is not allowed for an unsigned integer.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
    Number = ZeroU128
    IF (cStr(Indx:Indx) == '0') THEN
        ! the first digit is zero so loop through the following
        ! characters until a non-zero character is found
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= '0') EXIT
            Indx = Indx + 1
        END DO
        ! if only zero digits encountered, return
        IF (Indx > StrLen) RETURN
    END IF

    ! compute the length of digits
    DigitLen = StrLen - Indx + 1

    ! return quickly if possible
    IF (DigitLen < MaxDigitI32) THEN
        I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
        IF (ErrorFlag) THEN
            Number = MinU128
        ELSE
            Number = UInt128(I32Val)
        END IF
        IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
        IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        RETURN
    ELSEIF (DigitLen < MaxDigitI64) THEN
        I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
        IF (ErrorFlag) THEN
            Number = MinU128
        ELSE
            Number = UInt128(I64Val)
        END IF
        IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
        IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        RETURN
    END IF

    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        IndxP7 = Indx + 7
        DO WHILE (IndxP7 <= StrLen)
            wStr = cStr(Indx:IndxP7)
            IF (Is8Digits(WVal)) THEN
                ! process 8 digits at once
                ! => Number = Number*100000000_I8B + Parse8Digits(wVal)
                CALL MulAddU64(Number, 100000000_I8B, Parse8Digits(wVal))
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            Indx   = Indx + 8
            IndxP7 = Indx + 7
        END DO
        IF (Indx <= StrLen) THEN
            CurChr => cStr(Indx:Indx)
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                ! => Number = Number*10 + (IACHAR(CurChr)-A0)
                CALL MulAddU32(Number, 10, (IACHAR(CurChr)-A0))
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinU128
                    RETURN
                END IF
            END DO
        END IF
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinU128
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitU128) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitU128) THEN
        ! value might be in the applicable range so check overflow
        CurStr = cStr(IStart:StrLen)
        Overflow = FalseVal
        DO Indx = 1, MaxDigitU128
            CurChr => CurStr(Indx:Indx)
            IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                EXIT
            ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                Overflow = TrueVal
                EXIT
            END IF
        END DO
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is too large.'
        Number = MaxU128
    END IF

    RETURN

    CONTAINS

    FUNCTION Parse8Digits(InVal) RESULT(OutVal)

!DIR$ ATTRIBUTES FORCEINLINE :: Parse8Digits

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

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutVal = SHIFTR(IAND(SHIFTR(IAND(SHIFTR(IAND(InVal, K1)*M1, 8), K2)*M2, 16), K3)*M3, 32)

        RETURN

    END FUNCTION Parse8Digits

!******************************************************************************

    FUNCTION Is8Digits(InVal) RESULT(Flag)

!DIR$ ATTRIBUTES FORCEINLINE :: Is8Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether we can process eight digits immediately

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN) :: InVal
        LOGICAL                       :: Flag

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER  :: C1 = INT(Z'F0F0F0F0F0F0F0F0', KIND=I8B)
        INTEGER(KIND=I8B), PARAMETER  :: C2 = INT(Z'3333333333333333', KIND=I8B)
        INTEGER(KIND=I8B), PARAMETER  :: C3 = INT(Z'0606060606060606', KIND=I8B)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        Flag = IOR(IAND(InVal, C1), SHIFTR(IAND((InVal + C3), C1), 4)) ==  C2

        RETURN

    END FUNCTION Is8Digits

!******************************************************************************

    SUBROUTINE MulAddU64(U128, Mul, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply the given number by 'Mul' and then add 'Add' to it.
        ! (i.e. to set  U128 = U128*Mul + Add)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(UInt128),      INTENT(INOUT) :: U128
        INTEGER(KIND=I8B),  INTENT(IN)    :: Mul
        INTEGER(KIND=I8B),  INTENT(IN)    :: Add

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: X_Lo, Y_Lo, Y_Hi
        INTEGER(KIND=I8B)     :: Lo_Lo, Cross, MulLo

    !** FLOW

        ! perform multiplication
        X_Lo = IAND(Mul, Mask32)
        Y_Lo = IAND(U128%Low, Mask32)
        Y_Hi = SHIFTR(U128%Low, 32)
        Lo_Lo = X_Lo*Y_Lo
        Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi
        MulLo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
        U128%High = U128%High*Mul + SHIFTR(Cross, 32)

        ! perform addition
        U128%Low = MulLo + Add
        IF (IEOR(U128%Low, MinI64) < IEOR(MulLo, MinI64)) U128%High = U128%High + 1_I8B

        RETURN

    END SUBROUTINE MulAddU64

!******************************************************************************

    SUBROUTINE MulAddU32(U128, Mul, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply the given number by 'Mul' and then add 'Add' to it.
        ! (i.e. to set  U128 = U128*Mul + Add)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(UInt128),      INTENT(INOUT) :: U128
        INTEGER(KIND=I4B),  INTENT(IN)    :: Mul
        INTEGER(KIND=I4B),  INTENT(IN)    :: Add

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: X_Lo, Y_Lo, Y_Hi
        INTEGER(KIND=I8B)     :: Lo_Lo, Cross, MulLo

    !** FLOW

        ! perform multiplication
        X_Lo = IAND(INT(Mul, KIND=I8B), Mask32)
        Y_Lo = IAND(U128%Low, Mask32)
        Y_Hi = SHIFTR(U128%Low, 32)
        Lo_Lo = X_Lo*Y_Lo
        Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi
        MulLo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
        U128%High = U128%High*Mul + SHIFTR(Cross, 32)

        ! perform addition
        U128%Low = MulLo + Add
        IF (IEOR(U128%Low, MinI64) < IEOR(MulLo, MinI64)) U128%High = U128%High + 1_I8B

        RETURN

    END SUBROUTINE MulAddU32

!******************************************************************************

END FUNCTION DecString_To_U128

!------------------------------------------------------------------------------
!
!                           CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION I32_From_U128(U128) RESULT(I32)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 128-bit integer number to a signed 32-bit integer number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: U128
    INTEGER(KIND=I4B)            :: I32      !! number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I32 = INT(U128%Low, KIND=I4B)

    RETURN

END FUNCTION I32_From_U128

!******************************************************************************

FUNCTION I64_From_U128(U128) RESULT(I64)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 128-bit integer number to a signed 64-bit integer number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: U128
    INTEGER(KIND=I8B)            :: I64      !! number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I64 = U128%Low

    RETURN

END FUNCTION I64_From_U128

!******************************************************************************

FUNCTION R32_From_U128(U128) RESULT(R32)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 128-bit integer number to a 32-bit floating point number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)     :: U128
    REAL(KIND=SP)                 :: R32

!** SUBROUTINE PARAMETER DECLARATIONS:
    REAL(KIND=SP),     PARAMETER  :: TwoPow64 = 2.0_SP**64
    INTEGER(KIND=I4B), PARAMETER  :: TwoPow23 = SHIFTL(1, 23)
    INTEGER(KIND=I4B), PARAMETER  :: Mask     = INT(Z'000000FF', KIND=I4B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: S, Exp
    INTEGER(KIND=I4B)   :: IBits
    REAL(KIND=SP)       :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    IF (U128%High == 0_I8B) THEN
        ! convert directly and return quickly
        R32 = U64_To_R32(U128%Low)
        RETURN
    END IF

    S = LEADZ(U128%High)
    ! Mask out the 24 MSBits
    ! Also, the leading bit is implicit so cancel it out to get the significand
    IF (S <= 40) THEN
        IBits = IEOR(INT(SHIFTR(U128%High, 40-S), KIND=I4B), TwoPow23)
    ELSE
        ! S-40 == additional bits we need
        IBits = IEOR(INT(IOR(SHIFTL(U128%High, S-40), SHIFTR(U128%Low, 104-S)), KIND=I4B), TwoPow23)
    END IF
    ! get the binary exponent
    Exp = IAND(254-S, Mask)         ! 254 = 64 + 64 + 127 - 1

    ! Add the exponent
    IBits = IOR(IBits, SHIFTL(Exp, 23))

    ! transfer output (RBits mapped to IBits using equivalence)
    R32 = RBits

    RETURN

CONTAINS

    FUNCTION U64_To_R32(LongVal) RESULT(SingleVal)

!DIR$ ATTRIBUTES FORCEINLINE :: U64_To_R32

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 32-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN) :: LongVal      ! integer number treated as unsigned one
        REAL(KIND=SP)                 :: SingleVal    ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_I8B) THEN
            SingleVal = REAL(LongVal, KIND=SP)
        ELSE
            SingleVal = TwoPow64 + REAL(LongVal, KIND=SP)
        END IF

        RETURN

    END FUNCTION U64_To_R32

!******************************************************************************

END FUNCTION R32_From_U128

!******************************************************************************

FUNCTION R64_From_U128(U128) RESULT(R64)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 128-bit integer number to a 64-bit floating point number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)     :: U128
    REAL(KIND=DP)                 :: R64

!** SUBROUTINE PARAMETER DECLARATIONS:
    REAL(KIND=DP),     PARAMETER  :: TwoPow64 = 2.0_DP**64
    INTEGER(KIND=I8B), PARAMETER  :: TwoPow52 = SHIFTL(1_I8B, 52)
    INTEGER(KIND=I4B), PARAMETER  :: Mask     = INT(Z'000007FF', KIND=I4B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: S
    INTEGER(KIND=I8B)   :: Exp
    INTEGER(KIND=I8B)   :: IBits
    REAL(KIND=DP)       :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    IF (U128%High == 0_I8B) THEN
        R64 = U64_To_R64(U128%Low)
        RETURN
    END IF

    S = LEADZ(U128%High)
    ! Mask out the 53 MSBits
    ! Also, the leading bit is implicit so cancel it out to get the significand
    IF (S <= 11) THEN
        IBits = IEOR(SHIFTR(U128%High, 11-S), TwoPow52)
    ELSE
        ! S-11 == additional bits we need
        IBits = IEOR(IOR(SHIFTL(U128%High, S-11), SHIFTR(U128%Low, 75-S)), TwoPow52)
    END IF
    ! get the binary exponent
    Exp = INT(IAND(1150-S, Mask), KIND=I8B)        ! 1150 = 64 + 64 + 1023 - 1

    ! Add the exponent
    IBits = IOR(IBits, SHIFTL(Exp, 52))

    ! transfer output (RBits mapped to IBits using equivalence)
    R64 = RBits

    RETURN

CONTAINS

    FUNCTION U64_To_R64(LongVal) RESULT(DoubleVal)

!DIR$ ATTRIBUTES FORCEINLINE :: U64_To_R64

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 64-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN) :: LongVal      ! integer number treated as unsigned one
        REAL(KIND=DP)                 :: DoubleVal    ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_I8B) THEN
            DoubleVal = REAL(LongVal, KIND=DP)
        ELSE
            DoubleVal = TwoPow64 + REAL(LongVal, KIND=DP)
        END IF

        RETURN

    END FUNCTION U64_To_R64

!******************************************************************************

END FUNCTION R64_From_U128

!******************************************************************************

FUNCTION R128_From_U128(U128) RESULT(R128)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 128-bit integer number to a 128-bit floating point number.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)     :: U128
    REAL(KIND=QP)                 :: R128

!** SUBROUTINE PARAMETER DECLARATIONS:
    REAL(KIND=QP),     PARAMETER  :: TwoPow64     = 2.0_QP**64
    INTEGER(KIND=I8B), PARAMETER  :: TwoPow112(2) = [0_I8B, 281474976710656_I8B]  ! SHIFTL(1, 112)
    INTEGER(KIND=I4B), PARAMETER  :: Mask         = INT(Z'00007FFF', KIND=I4B)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)   :: S, Shift
    INTEGER(KIND=I8B)   :: Exp
    INTEGER(KIND=I8B)   :: IBits(2)
    REAL(KIND=QP)       :: RBits
    EQUIVALENCE(IBits, RBits)

!** FLOW

    IF (U128%High == 0_I8B) THEN
        R128 = U64_To_R128(U128%Low)
        RETURN
    END IF

    S = LEADZ(U128%High)
    IF (S >= 15) THEN
        R128 = U64_To_R128(U128%Low) + U64_To_R128(U128%High)*TwoPow64
        RETURN
    END IF

    ! Mask out the 113 MSBits (assumming little-endian order)
    ! (if big-endian order, just switch IBits(1) and IBits(2))
    Shift = 15 - S
    IBits(2) = SHIFTR(U128%High, Shift)
    IBits(1) = IOR(SHIFTR(U128%Low, Shift), SHIFTL(U128%High, 64-Shift))

    ! get the binary exponent
    Exp = INT(IAND(16510-S, Mask), KIND=I8B)   ! 16510 = 64 + 64 + 16383 - 1

    ! The leading bit is implicit, cancel it out to get the significand
    ! and also add the exponent
    IBits(1) = IEOR(IOR(SHIFTR(U128%Low, Shift), SHIFTL(U128%High, 64-Shift)), TwoPow112(1))
    IBits(2) = IOR(IEOR(SHIFTR(U128%High, Shift), TwoPow112(2)), SHIFTL(Exp, 48))   ! 48 = 112 - 64

    ! transfer output (RBits mapped to IBits using equivalence)
    ! For big-endian machine, we must swap IBits(1) and IBits(2) before the assigment.
    ! => Tmp = IBits(1); IBits(1) = IBits(2); IBits(2) = Tmp
    R128 = RBits

    RETURN

CONTAINS

    FUNCTION U64_To_R128(LongVal) RESULT(QuadVal)

!DIR$ ATTRIBUTES FORCEINLINE :: U64_To_R128

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 128-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN) :: LongVal  ! integer number treated as unsigned one
        REAL(KIND=QP)                 :: QuadVal  ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_I8B) THEN
            QuadVal = REAL(LongVal, KIND=QP)
        ELSE
            QuadVal = TwoPow64 + REAL(LongVal, KIND=QP)
        END IF

        RETURN

    END FUNCTION U64_To_R128

!******************************************************************************

END FUNCTION R128_From_U128

!******************************************************************************

FUNCTION DecString_From_U128(U128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 128-bit integer number to a decimal string.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)       :: U128
    CHARACTER(LEN=:), ALLOCATABLE   :: Str

!** SUBROUTINE PARAMETER DECLARATIONS:
    CHARACTER(LEN=1), PARAMETER  :: Char1Digit(0:9) = [  &
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    CHARACTER(LEN=2), PARAMETER  :: Char2Digits(0:99) = [           &
        '00', '01', '02', '03', '04', '05', '06', '07', '08', '09', &
        '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', &
        '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', &
        '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', &
        '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', &
        '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', &
        '60', '61', '62', '63', '64', '65', '66', '67', '68', '69', &
        '70', '71', '72', '73', '74', '75', '76', '77', '78', '79', &
        '80', '81', '82', '83', '84', '85', '86', '87', '88', '89', &
        '90', '91', '92', '93', '94', '95', '96', '97', '98', '99']
    CHARACTER(LEN=4), PARAMETER  :: Char4Digits(0:9999) = [                             &
        '0000', '0001', '0002', '0003', '0004', '0005', '0006', '0007', '0008', '0009', &
        '0010', '0011', '0012', '0013', '0014', '0015', '0016', '0017', '0018', '0019', &
        '0020', '0021', '0022', '0023', '0024', '0025', '0026', '0027', '0028', '0029', &
        '0030', '0031', '0032', '0033', '0034', '0035', '0036', '0037', '0038', '0039', &
        '0040', '0041', '0042', '0043', '0044', '0045', '0046', '0047', '0048', '0049', &
        '0050', '0051', '0052', '0053', '0054', '0055', '0056', '0057', '0058', '0059', &
        '0060', '0061', '0062', '0063', '0064', '0065', '0066', '0067', '0068', '0069', &
        '0070', '0071', '0072', '0073', '0074', '0075', '0076', '0077', '0078', '0079', &
        '0080', '0081', '0082', '0083', '0084', '0085', '0086', '0087', '0088', '0089', &
        '0090', '0091', '0092', '0093', '0094', '0095', '0096', '0097', '0098', '0099', &
        '0100', '0101', '0102', '0103', '0104', '0105', '0106', '0107', '0108', '0109', &
        '0110', '0111', '0112', '0113', '0114', '0115', '0116', '0117', '0118', '0119', &
        '0120', '0121', '0122', '0123', '0124', '0125', '0126', '0127', '0128', '0129', &
        '0130', '0131', '0132', '0133', '0134', '0135', '0136', '0137', '0138', '0139', &
        '0140', '0141', '0142', '0143', '0144', '0145', '0146', '0147', '0148', '0149', &
        '0150', '0151', '0152', '0153', '0154', '0155', '0156', '0157', '0158', '0159', &
        '0160', '0161', '0162', '0163', '0164', '0165', '0166', '0167', '0168', '0169', &
        '0170', '0171', '0172', '0173', '0174', '0175', '0176', '0177', '0178', '0179', &
        '0180', '0181', '0182', '0183', '0184', '0185', '0186', '0187', '0188', '0189', &
        '0190', '0191', '0192', '0193', '0194', '0195', '0196', '0197', '0198', '0199', &
        '0200', '0201', '0202', '0203', '0204', '0205', '0206', '0207', '0208', '0209', &
        '0210', '0211', '0212', '0213', '0214', '0215', '0216', '0217', '0218', '0219', &
        '0220', '0221', '0222', '0223', '0224', '0225', '0226', '0227', '0228', '0229', &
        '0230', '0231', '0232', '0233', '0234', '0235', '0236', '0237', '0238', '0239', &
        '0240', '0241', '0242', '0243', '0244', '0245', '0246', '0247', '0248', '0249', &
        '0250', '0251', '0252', '0253', '0254', '0255', '0256', '0257', '0258', '0259', &
        '0260', '0261', '0262', '0263', '0264', '0265', '0266', '0267', '0268', '0269', &
        '0270', '0271', '0272', '0273', '0274', '0275', '0276', '0277', '0278', '0279', &
        '0280', '0281', '0282', '0283', '0284', '0285', '0286', '0287', '0288', '0289', &
        '0290', '0291', '0292', '0293', '0294', '0295', '0296', '0297', '0298', '0299', &
        '0300', '0301', '0302', '0303', '0304', '0305', '0306', '0307', '0308', '0309', &
        '0310', '0311', '0312', '0313', '0314', '0315', '0316', '0317', '0318', '0319', &
        '0320', '0321', '0322', '0323', '0324', '0325', '0326', '0327', '0328', '0329', &
        '0330', '0331', '0332', '0333', '0334', '0335', '0336', '0337', '0338', '0339', &
        '0340', '0341', '0342', '0343', '0344', '0345', '0346', '0347', '0348', '0349', &
        '0350', '0351', '0352', '0353', '0354', '0355', '0356', '0357', '0358', '0359', &
        '0360', '0361', '0362', '0363', '0364', '0365', '0366', '0367', '0368', '0369', &
        '0370', '0371', '0372', '0373', '0374', '0375', '0376', '0377', '0378', '0379', &
        '0380', '0381', '0382', '0383', '0384', '0385', '0386', '0387', '0388', '0389', &
        '0390', '0391', '0392', '0393', '0394', '0395', '0396', '0397', '0398', '0399', &
        '0400', '0401', '0402', '0403', '0404', '0405', '0406', '0407', '0408', '0409', &
        '0410', '0411', '0412', '0413', '0414', '0415', '0416', '0417', '0418', '0419', &
        '0420', '0421', '0422', '0423', '0424', '0425', '0426', '0427', '0428', '0429', &
        '0430', '0431', '0432', '0433', '0434', '0435', '0436', '0437', '0438', '0439', &
        '0440', '0441', '0442', '0443', '0444', '0445', '0446', '0447', '0448', '0449', &
        '0450', '0451', '0452', '0453', '0454', '0455', '0456', '0457', '0458', '0459', &
        '0460', '0461', '0462', '0463', '0464', '0465', '0466', '0467', '0468', '0469', &
        '0470', '0471', '0472', '0473', '0474', '0475', '0476', '0477', '0478', '0479', &
        '0480', '0481', '0482', '0483', '0484', '0485', '0486', '0487', '0488', '0489', &
        '0490', '0491', '0492', '0493', '0494', '0495', '0496', '0497', '0498', '0499', &
        '0500', '0501', '0502', '0503', '0504', '0505', '0506', '0507', '0508', '0509', &
        '0510', '0511', '0512', '0513', '0514', '0515', '0516', '0517', '0518', '0519', &
        '0520', '0521', '0522', '0523', '0524', '0525', '0526', '0527', '0528', '0529', &
        '0530', '0531', '0532', '0533', '0534', '0535', '0536', '0537', '0538', '0539', &
        '0540', '0541', '0542', '0543', '0544', '0545', '0546', '0547', '0548', '0549', &
        '0550', '0551', '0552', '0553', '0554', '0555', '0556', '0557', '0558', '0559', &
        '0560', '0561', '0562', '0563', '0564', '0565', '0566', '0567', '0568', '0569', &
        '0570', '0571', '0572', '0573', '0574', '0575', '0576', '0577', '0578', '0579', &
        '0580', '0581', '0582', '0583', '0584', '0585', '0586', '0587', '0588', '0589', &
        '0590', '0591', '0592', '0593', '0594', '0595', '0596', '0597', '0598', '0599', &
        '0600', '0601', '0602', '0603', '0604', '0605', '0606', '0607', '0608', '0609', &
        '0610', '0611', '0612', '0613', '0614', '0615', '0616', '0617', '0618', '0619', &
        '0620', '0621', '0622', '0623', '0624', '0625', '0626', '0627', '0628', '0629', &
        '0630', '0631', '0632', '0633', '0634', '0635', '0636', '0637', '0638', '0639', &
        '0640', '0641', '0642', '0643', '0644', '0645', '0646', '0647', '0648', '0649', &
        '0650', '0651', '0652', '0653', '0654', '0655', '0656', '0657', '0658', '0659', &
        '0660', '0661', '0662', '0663', '0664', '0665', '0666', '0667', '0668', '0669', &
        '0670', '0671', '0672', '0673', '0674', '0675', '0676', '0677', '0678', '0679', &
        '0680', '0681', '0682', '0683', '0684', '0685', '0686', '0687', '0688', '0689', &
        '0690', '0691', '0692', '0693', '0694', '0695', '0696', '0697', '0698', '0699', &
        '0700', '0701', '0702', '0703', '0704', '0705', '0706', '0707', '0708', '0709', &
        '0710', '0711', '0712', '0713', '0714', '0715', '0716', '0717', '0718', '0719', &
        '0720', '0721', '0722', '0723', '0724', '0725', '0726', '0727', '0728', '0729', &
        '0730', '0731', '0732', '0733', '0734', '0735', '0736', '0737', '0738', '0739', &
        '0740', '0741', '0742', '0743', '0744', '0745', '0746', '0747', '0748', '0749', &
        '0750', '0751', '0752', '0753', '0754', '0755', '0756', '0757', '0758', '0759', &
        '0760', '0761', '0762', '0763', '0764', '0765', '0766', '0767', '0768', '0769', &
        '0770', '0771', '0772', '0773', '0774', '0775', '0776', '0777', '0778', '0779', &
        '0780', '0781', '0782', '0783', '0784', '0785', '0786', '0787', '0788', '0789', &
        '0790', '0791', '0792', '0793', '0794', '0795', '0796', '0797', '0798', '0799', &
        '0800', '0801', '0802', '0803', '0804', '0805', '0806', '0807', '0808', '0809', &
        '0810', '0811', '0812', '0813', '0814', '0815', '0816', '0817', '0818', '0819', &
        '0820', '0821', '0822', '0823', '0824', '0825', '0826', '0827', '0828', '0829', &
        '0830', '0831', '0832', '0833', '0834', '0835', '0836', '0837', '0838', '0839', &
        '0840', '0841', '0842', '0843', '0844', '0845', '0846', '0847', '0848', '0849', &
        '0850', '0851', '0852', '0853', '0854', '0855', '0856', '0857', '0858', '0859', &
        '0860', '0861', '0862', '0863', '0864', '0865', '0866', '0867', '0868', '0869', &
        '0870', '0871', '0872', '0873', '0874', '0875', '0876', '0877', '0878', '0879', &
        '0880', '0881', '0882', '0883', '0884', '0885', '0886', '0887', '0888', '0889', &
        '0890', '0891', '0892', '0893', '0894', '0895', '0896', '0897', '0898', '0899', &
        '0900', '0901', '0902', '0903', '0904', '0905', '0906', '0907', '0908', '0909', &
        '0910', '0911', '0912', '0913', '0914', '0915', '0916', '0917', '0918', '0919', &
        '0920', '0921', '0922', '0923', '0924', '0925', '0926', '0927', '0928', '0929', &
        '0930', '0931', '0932', '0933', '0934', '0935', '0936', '0937', '0938', '0939', &
        '0940', '0941', '0942', '0943', '0944', '0945', '0946', '0947', '0948', '0949', &
        '0950', '0951', '0952', '0953', '0954', '0955', '0956', '0957', '0958', '0959', &
        '0960', '0961', '0962', '0963', '0964', '0965', '0966', '0967', '0968', '0969', &
        '0970', '0971', '0972', '0973', '0974', '0975', '0976', '0977', '0978', '0979', &
        '0980', '0981', '0982', '0983', '0984', '0985', '0986', '0987', '0988', '0989', &
        '0990', '0991', '0992', '0993', '0994', '0995', '0996', '0997', '0998', '0999', &
        '1000', '1001', '1002', '1003', '1004', '1005', '1006', '1007', '1008', '1009', &
        '1010', '1011', '1012', '1013', '1014', '1015', '1016', '1017', '1018', '1019', &
        '1020', '1021', '1022', '1023', '1024', '1025', '1026', '1027', '1028', '1029', &
        '1030', '1031', '1032', '1033', '1034', '1035', '1036', '1037', '1038', '1039', &
        '1040', '1041', '1042', '1043', '1044', '1045', '1046', '1047', '1048', '1049', &
        '1050', '1051', '1052', '1053', '1054', '1055', '1056', '1057', '1058', '1059', &
        '1060', '1061', '1062', '1063', '1064', '1065', '1066', '1067', '1068', '1069', &
        '1070', '1071', '1072', '1073', '1074', '1075', '1076', '1077', '1078', '1079', &
        '1080', '1081', '1082', '1083', '1084', '1085', '1086', '1087', '1088', '1089', &
        '1090', '1091', '1092', '1093', '1094', '1095', '1096', '1097', '1098', '1099', &
        '1100', '1101', '1102', '1103', '1104', '1105', '1106', '1107', '1108', '1109', &
        '1110', '1111', '1112', '1113', '1114', '1115', '1116', '1117', '1118', '1119', &
        '1120', '1121', '1122', '1123', '1124', '1125', '1126', '1127', '1128', '1129', &
        '1130', '1131', '1132', '1133', '1134', '1135', '1136', '1137', '1138', '1139', &
        '1140', '1141', '1142', '1143', '1144', '1145', '1146', '1147', '1148', '1149', &
        '1150', '1151', '1152', '1153', '1154', '1155', '1156', '1157', '1158', '1159', &
        '1160', '1161', '1162', '1163', '1164', '1165', '1166', '1167', '1168', '1169', &
        '1170', '1171', '1172', '1173', '1174', '1175', '1176', '1177', '1178', '1179', &
        '1180', '1181', '1182', '1183', '1184', '1185', '1186', '1187', '1188', '1189', &
        '1190', '1191', '1192', '1193', '1194', '1195', '1196', '1197', '1198', '1199', &
        '1200', '1201', '1202', '1203', '1204', '1205', '1206', '1207', '1208', '1209', &
        '1210', '1211', '1212', '1213', '1214', '1215', '1216', '1217', '1218', '1219', &
        '1220', '1221', '1222', '1223', '1224', '1225', '1226', '1227', '1228', '1229', &
        '1230', '1231', '1232', '1233', '1234', '1235', '1236', '1237', '1238', '1239', &
        '1240', '1241', '1242', '1243', '1244', '1245', '1246', '1247', '1248', '1249', &
        '1250', '1251', '1252', '1253', '1254', '1255', '1256', '1257', '1258', '1259', &
        '1260', '1261', '1262', '1263', '1264', '1265', '1266', '1267', '1268', '1269', &
        '1270', '1271', '1272', '1273', '1274', '1275', '1276', '1277', '1278', '1279', &
        '1280', '1281', '1282', '1283', '1284', '1285', '1286', '1287', '1288', '1289', &
        '1290', '1291', '1292', '1293', '1294', '1295', '1296', '1297', '1298', '1299', &
        '1300', '1301', '1302', '1303', '1304', '1305', '1306', '1307', '1308', '1309', &
        '1310', '1311', '1312', '1313', '1314', '1315', '1316', '1317', '1318', '1319', &
        '1320', '1321', '1322', '1323', '1324', '1325', '1326', '1327', '1328', '1329', &
        '1330', '1331', '1332', '1333', '1334', '1335', '1336', '1337', '1338', '1339', &
        '1340', '1341', '1342', '1343', '1344', '1345', '1346', '1347', '1348', '1349', &
        '1350', '1351', '1352', '1353', '1354', '1355', '1356', '1357', '1358', '1359', &
        '1360', '1361', '1362', '1363', '1364', '1365', '1366', '1367', '1368', '1369', &
        '1370', '1371', '1372', '1373', '1374', '1375', '1376', '1377', '1378', '1379', &
        '1380', '1381', '1382', '1383', '1384', '1385', '1386', '1387', '1388', '1389', &
        '1390', '1391', '1392', '1393', '1394', '1395', '1396', '1397', '1398', '1399', &
        '1400', '1401', '1402', '1403', '1404', '1405', '1406', '1407', '1408', '1409', &
        '1410', '1411', '1412', '1413', '1414', '1415', '1416', '1417', '1418', '1419', &
        '1420', '1421', '1422', '1423', '1424', '1425', '1426', '1427', '1428', '1429', &
        '1430', '1431', '1432', '1433', '1434', '1435', '1436', '1437', '1438', '1439', &
        '1440', '1441', '1442', '1443', '1444', '1445', '1446', '1447', '1448', '1449', &
        '1450', '1451', '1452', '1453', '1454', '1455', '1456', '1457', '1458', '1459', &
        '1460', '1461', '1462', '1463', '1464', '1465', '1466', '1467', '1468', '1469', &
        '1470', '1471', '1472', '1473', '1474', '1475', '1476', '1477', '1478', '1479', &
        '1480', '1481', '1482', '1483', '1484', '1485', '1486', '1487', '1488', '1489', &
        '1490', '1491', '1492', '1493', '1494', '1495', '1496', '1497', '1498', '1499', &
        '1500', '1501', '1502', '1503', '1504', '1505', '1506', '1507', '1508', '1509', &
        '1510', '1511', '1512', '1513', '1514', '1515', '1516', '1517', '1518', '1519', &
        '1520', '1521', '1522', '1523', '1524', '1525', '1526', '1527', '1528', '1529', &
        '1530', '1531', '1532', '1533', '1534', '1535', '1536', '1537', '1538', '1539', &
        '1540', '1541', '1542', '1543', '1544', '1545', '1546', '1547', '1548', '1549', &
        '1550', '1551', '1552', '1553', '1554', '1555', '1556', '1557', '1558', '1559', &
        '1560', '1561', '1562', '1563', '1564', '1565', '1566', '1567', '1568', '1569', &
        '1570', '1571', '1572', '1573', '1574', '1575', '1576', '1577', '1578', '1579', &
        '1580', '1581', '1582', '1583', '1584', '1585', '1586', '1587', '1588', '1589', &
        '1590', '1591', '1592', '1593', '1594', '1595', '1596', '1597', '1598', '1599', &
        '1600', '1601', '1602', '1603', '1604', '1605', '1606', '1607', '1608', '1609', &
        '1610', '1611', '1612', '1613', '1614', '1615', '1616', '1617', '1618', '1619', &
        '1620', '1621', '1622', '1623', '1624', '1625', '1626', '1627', '1628', '1629', &
        '1630', '1631', '1632', '1633', '1634', '1635', '1636', '1637', '1638', '1639', &
        '1640', '1641', '1642', '1643', '1644', '1645', '1646', '1647', '1648', '1649', &
        '1650', '1651', '1652', '1653', '1654', '1655', '1656', '1657', '1658', '1659', &
        '1660', '1661', '1662', '1663', '1664', '1665', '1666', '1667', '1668', '1669', &
        '1670', '1671', '1672', '1673', '1674', '1675', '1676', '1677', '1678', '1679', &
        '1680', '1681', '1682', '1683', '1684', '1685', '1686', '1687', '1688', '1689', &
        '1690', '1691', '1692', '1693', '1694', '1695', '1696', '1697', '1698', '1699', &
        '1700', '1701', '1702', '1703', '1704', '1705', '1706', '1707', '1708', '1709', &
        '1710', '1711', '1712', '1713', '1714', '1715', '1716', '1717', '1718', '1719', &
        '1720', '1721', '1722', '1723', '1724', '1725', '1726', '1727', '1728', '1729', &
        '1730', '1731', '1732', '1733', '1734', '1735', '1736', '1737', '1738', '1739', &
        '1740', '1741', '1742', '1743', '1744', '1745', '1746', '1747', '1748', '1749', &
        '1750', '1751', '1752', '1753', '1754', '1755', '1756', '1757', '1758', '1759', &
        '1760', '1761', '1762', '1763', '1764', '1765', '1766', '1767', '1768', '1769', &
        '1770', '1771', '1772', '1773', '1774', '1775', '1776', '1777', '1778', '1779', &
        '1780', '1781', '1782', '1783', '1784', '1785', '1786', '1787', '1788', '1789', &
        '1790', '1791', '1792', '1793', '1794', '1795', '1796', '1797', '1798', '1799', &
        '1800', '1801', '1802', '1803', '1804', '1805', '1806', '1807', '1808', '1809', &
        '1810', '1811', '1812', '1813', '1814', '1815', '1816', '1817', '1818', '1819', &
        '1820', '1821', '1822', '1823', '1824', '1825', '1826', '1827', '1828', '1829', &
        '1830', '1831', '1832', '1833', '1834', '1835', '1836', '1837', '1838', '1839', &
        '1840', '1841', '1842', '1843', '1844', '1845', '1846', '1847', '1848', '1849', &
        '1850', '1851', '1852', '1853', '1854', '1855', '1856', '1857', '1858', '1859', &
        '1860', '1861', '1862', '1863', '1864', '1865', '1866', '1867', '1868', '1869', &
        '1870', '1871', '1872', '1873', '1874', '1875', '1876', '1877', '1878', '1879', &
        '1880', '1881', '1882', '1883', '1884', '1885', '1886', '1887', '1888', '1889', &
        '1890', '1891', '1892', '1893', '1894', '1895', '1896', '1897', '1898', '1899', &
        '1900', '1901', '1902', '1903', '1904', '1905', '1906', '1907', '1908', '1909', &
        '1910', '1911', '1912', '1913', '1914', '1915', '1916', '1917', '1918', '1919', &
        '1920', '1921', '1922', '1923', '1924', '1925', '1926', '1927', '1928', '1929', &
        '1930', '1931', '1932', '1933', '1934', '1935', '1936', '1937', '1938', '1939', &
        '1940', '1941', '1942', '1943', '1944', '1945', '1946', '1947', '1948', '1949', &
        '1950', '1951', '1952', '1953', '1954', '1955', '1956', '1957', '1958', '1959', &
        '1960', '1961', '1962', '1963', '1964', '1965', '1966', '1967', '1968', '1969', &
        '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', &
        '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989', &
        '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', &
        '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', &
        '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', &
        '2020', '2021', '2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', &
        '2030', '2031', '2032', '2033', '2034', '2035', '2036', '2037', '2038', '2039', &
        '2040', '2041', '2042', '2043', '2044', '2045', '2046', '2047', '2048', '2049', &
        '2050', '2051', '2052', '2053', '2054', '2055', '2056', '2057', '2058', '2059', &
        '2060', '2061', '2062', '2063', '2064', '2065', '2066', '2067', '2068', '2069', &
        '2070', '2071', '2072', '2073', '2074', '2075', '2076', '2077', '2078', '2079', &
        '2080', '2081', '2082', '2083', '2084', '2085', '2086', '2087', '2088', '2089', &
        '2090', '2091', '2092', '2093', '2094', '2095', '2096', '2097', '2098', '2099', &
        '2100', '2101', '2102', '2103', '2104', '2105', '2106', '2107', '2108', '2109', &
        '2110', '2111', '2112', '2113', '2114', '2115', '2116', '2117', '2118', '2119', &
        '2120', '2121', '2122', '2123', '2124', '2125', '2126', '2127', '2128', '2129', &
        '2130', '2131', '2132', '2133', '2134', '2135', '2136', '2137', '2138', '2139', &
        '2140', '2141', '2142', '2143', '2144', '2145', '2146', '2147', '2148', '2149', &
        '2150', '2151', '2152', '2153', '2154', '2155', '2156', '2157', '2158', '2159', &
        '2160', '2161', '2162', '2163', '2164', '2165', '2166', '2167', '2168', '2169', &
        '2170', '2171', '2172', '2173', '2174', '2175', '2176', '2177', '2178', '2179', &
        '2180', '2181', '2182', '2183', '2184', '2185', '2186', '2187', '2188', '2189', &
        '2190', '2191', '2192', '2193', '2194', '2195', '2196', '2197', '2198', '2199', &
        '2200', '2201', '2202', '2203', '2204', '2205', '2206', '2207', '2208', '2209', &
        '2210', '2211', '2212', '2213', '2214', '2215', '2216', '2217', '2218', '2219', &
        '2220', '2221', '2222', '2223', '2224', '2225', '2226', '2227', '2228', '2229', &
        '2230', '2231', '2232', '2233', '2234', '2235', '2236', '2237', '2238', '2239', &
        '2240', '2241', '2242', '2243', '2244', '2245', '2246', '2247', '2248', '2249', &
        '2250', '2251', '2252', '2253', '2254', '2255', '2256', '2257', '2258', '2259', &
        '2260', '2261', '2262', '2263', '2264', '2265', '2266', '2267', '2268', '2269', &
        '2270', '2271', '2272', '2273', '2274', '2275', '2276', '2277', '2278', '2279', &
        '2280', '2281', '2282', '2283', '2284', '2285', '2286', '2287', '2288', '2289', &
        '2290', '2291', '2292', '2293', '2294', '2295', '2296', '2297', '2298', '2299', &
        '2300', '2301', '2302', '2303', '2304', '2305', '2306', '2307', '2308', '2309', &
        '2310', '2311', '2312', '2313', '2314', '2315', '2316', '2317', '2318', '2319', &
        '2320', '2321', '2322', '2323', '2324', '2325', '2326', '2327', '2328', '2329', &
        '2330', '2331', '2332', '2333', '2334', '2335', '2336', '2337', '2338', '2339', &
        '2340', '2341', '2342', '2343', '2344', '2345', '2346', '2347', '2348', '2349', &
        '2350', '2351', '2352', '2353', '2354', '2355', '2356', '2357', '2358', '2359', &
        '2360', '2361', '2362', '2363', '2364', '2365', '2366', '2367', '2368', '2369', &
        '2370', '2371', '2372', '2373', '2374', '2375', '2376', '2377', '2378', '2379', &
        '2380', '2381', '2382', '2383', '2384', '2385', '2386', '2387', '2388', '2389', &
        '2390', '2391', '2392', '2393', '2394', '2395', '2396', '2397', '2398', '2399', &
        '2400', '2401', '2402', '2403', '2404', '2405', '2406', '2407', '2408', '2409', &
        '2410', '2411', '2412', '2413', '2414', '2415', '2416', '2417', '2418', '2419', &
        '2420', '2421', '2422', '2423', '2424', '2425', '2426', '2427', '2428', '2429', &
        '2430', '2431', '2432', '2433', '2434', '2435', '2436', '2437', '2438', '2439', &
        '2440', '2441', '2442', '2443', '2444', '2445', '2446', '2447', '2448', '2449', &
        '2450', '2451', '2452', '2453', '2454', '2455', '2456', '2457', '2458', '2459', &
        '2460', '2461', '2462', '2463', '2464', '2465', '2466', '2467', '2468', '2469', &
        '2470', '2471', '2472', '2473', '2474', '2475', '2476', '2477', '2478', '2479', &
        '2480', '2481', '2482', '2483', '2484', '2485', '2486', '2487', '2488', '2489', &
        '2490', '2491', '2492', '2493', '2494', '2495', '2496', '2497', '2498', '2499', &
        '2500', '2501', '2502', '2503', '2504', '2505', '2506', '2507', '2508', '2509', &
        '2510', '2511', '2512', '2513', '2514', '2515', '2516', '2517', '2518', '2519', &
        '2520', '2521', '2522', '2523', '2524', '2525', '2526', '2527', '2528', '2529', &
        '2530', '2531', '2532', '2533', '2534', '2535', '2536', '2537', '2538', '2539', &
        '2540', '2541', '2542', '2543', '2544', '2545', '2546', '2547', '2548', '2549', &
        '2550', '2551', '2552', '2553', '2554', '2555', '2556', '2557', '2558', '2559', &
        '2560', '2561', '2562', '2563', '2564', '2565', '2566', '2567', '2568', '2569', &
        '2570', '2571', '2572', '2573', '2574', '2575', '2576', '2577', '2578', '2579', &
        '2580', '2581', '2582', '2583', '2584', '2585', '2586', '2587', '2588', '2589', &
        '2590', '2591', '2592', '2593', '2594', '2595', '2596', '2597', '2598', '2599', &
        '2600', '2601', '2602', '2603', '2604', '2605', '2606', '2607', '2608', '2609', &
        '2610', '2611', '2612', '2613', '2614', '2615', '2616', '2617', '2618', '2619', &
        '2620', '2621', '2622', '2623', '2624', '2625', '2626', '2627', '2628', '2629', &
        '2630', '2631', '2632', '2633', '2634', '2635', '2636', '2637', '2638', '2639', &
        '2640', '2641', '2642', '2643', '2644', '2645', '2646', '2647', '2648', '2649', &
        '2650', '2651', '2652', '2653', '2654', '2655', '2656', '2657', '2658', '2659', &
        '2660', '2661', '2662', '2663', '2664', '2665', '2666', '2667', '2668', '2669', &
        '2670', '2671', '2672', '2673', '2674', '2675', '2676', '2677', '2678', '2679', &
        '2680', '2681', '2682', '2683', '2684', '2685', '2686', '2687', '2688', '2689', &
        '2690', '2691', '2692', '2693', '2694', '2695', '2696', '2697', '2698', '2699', &
        '2700', '2701', '2702', '2703', '2704', '2705', '2706', '2707', '2708', '2709', &
        '2710', '2711', '2712', '2713', '2714', '2715', '2716', '2717', '2718', '2719', &
        '2720', '2721', '2722', '2723', '2724', '2725', '2726', '2727', '2728', '2729', &
        '2730', '2731', '2732', '2733', '2734', '2735', '2736', '2737', '2738', '2739', &
        '2740', '2741', '2742', '2743', '2744', '2745', '2746', '2747', '2748', '2749', &
        '2750', '2751', '2752', '2753', '2754', '2755', '2756', '2757', '2758', '2759', &
        '2760', '2761', '2762', '2763', '2764', '2765', '2766', '2767', '2768', '2769', &
        '2770', '2771', '2772', '2773', '2774', '2775', '2776', '2777', '2778', '2779', &
        '2780', '2781', '2782', '2783', '2784', '2785', '2786', '2787', '2788', '2789', &
        '2790', '2791', '2792', '2793', '2794', '2795', '2796', '2797', '2798', '2799', &
        '2800', '2801', '2802', '2803', '2804', '2805', '2806', '2807', '2808', '2809', &
        '2810', '2811', '2812', '2813', '2814', '2815', '2816', '2817', '2818', '2819', &
        '2820', '2821', '2822', '2823', '2824', '2825', '2826', '2827', '2828', '2829', &
        '2830', '2831', '2832', '2833', '2834', '2835', '2836', '2837', '2838', '2839', &
        '2840', '2841', '2842', '2843', '2844', '2845', '2846', '2847', '2848', '2849', &
        '2850', '2851', '2852', '2853', '2854', '2855', '2856', '2857', '2858', '2859', &
        '2860', '2861', '2862', '2863', '2864', '2865', '2866', '2867', '2868', '2869', &
        '2870', '2871', '2872', '2873', '2874', '2875', '2876', '2877', '2878', '2879', &
        '2880', '2881', '2882', '2883', '2884', '2885', '2886', '2887', '2888', '2889', &
        '2890', '2891', '2892', '2893', '2894', '2895', '2896', '2897', '2898', '2899', &
        '2900', '2901', '2902', '2903', '2904', '2905', '2906', '2907', '2908', '2909', &
        '2910', '2911', '2912', '2913', '2914', '2915', '2916', '2917', '2918', '2919', &
        '2920', '2921', '2922', '2923', '2924', '2925', '2926', '2927', '2928', '2929', &
        '2930', '2931', '2932', '2933', '2934', '2935', '2936', '2937', '2938', '2939', &
        '2940', '2941', '2942', '2943', '2944', '2945', '2946', '2947', '2948', '2949', &
        '2950', '2951', '2952', '2953', '2954', '2955', '2956', '2957', '2958', '2959', &
        '2960', '2961', '2962', '2963', '2964', '2965', '2966', '2967', '2968', '2969', &
        '2970', '2971', '2972', '2973', '2974', '2975', '2976', '2977', '2978', '2979', &
        '2980', '2981', '2982', '2983', '2984', '2985', '2986', '2987', '2988', '2989', &
        '2990', '2991', '2992', '2993', '2994', '2995', '2996', '2997', '2998', '2999', &
        '3000', '3001', '3002', '3003', '3004', '3005', '3006', '3007', '3008', '3009', &
        '3010', '3011', '3012', '3013', '3014', '3015', '3016', '3017', '3018', '3019', &
        '3020', '3021', '3022', '3023', '3024', '3025', '3026', '3027', '3028', '3029', &
        '3030', '3031', '3032', '3033', '3034', '3035', '3036', '3037', '3038', '3039', &
        '3040', '3041', '3042', '3043', '3044', '3045', '3046', '3047', '3048', '3049', &
        '3050', '3051', '3052', '3053', '3054', '3055', '3056', '3057', '3058', '3059', &
        '3060', '3061', '3062', '3063', '3064', '3065', '3066', '3067', '3068', '3069', &
        '3070', '3071', '3072', '3073', '3074', '3075', '3076', '3077', '3078', '3079', &
        '3080', '3081', '3082', '3083', '3084', '3085', '3086', '3087', '3088', '3089', &
        '3090', '3091', '3092', '3093', '3094', '3095', '3096', '3097', '3098', '3099', &
        '3100', '3101', '3102', '3103', '3104', '3105', '3106', '3107', '3108', '3109', &
        '3110', '3111', '3112', '3113', '3114', '3115', '3116', '3117', '3118', '3119', &
        '3120', '3121', '3122', '3123', '3124', '3125', '3126', '3127', '3128', '3129', &
        '3130', '3131', '3132', '3133', '3134', '3135', '3136', '3137', '3138', '3139', &
        '3140', '3141', '3142', '3143', '3144', '3145', '3146', '3147', '3148', '3149', &
        '3150', '3151', '3152', '3153', '3154', '3155', '3156', '3157', '3158', '3159', &
        '3160', '3161', '3162', '3163', '3164', '3165', '3166', '3167', '3168', '3169', &
        '3170', '3171', '3172', '3173', '3174', '3175', '3176', '3177', '3178', '3179', &
        '3180', '3181', '3182', '3183', '3184', '3185', '3186', '3187', '3188', '3189', &
        '3190', '3191', '3192', '3193', '3194', '3195', '3196', '3197', '3198', '3199', &
        '3200', '3201', '3202', '3203', '3204', '3205', '3206', '3207', '3208', '3209', &
        '3210', '3211', '3212', '3213', '3214', '3215', '3216', '3217', '3218', '3219', &
        '3220', '3221', '3222', '3223', '3224', '3225', '3226', '3227', '3228', '3229', &
        '3230', '3231', '3232', '3233', '3234', '3235', '3236', '3237', '3238', '3239', &
        '3240', '3241', '3242', '3243', '3244', '3245', '3246', '3247', '3248', '3249', &
        '3250', '3251', '3252', '3253', '3254', '3255', '3256', '3257', '3258', '3259', &
        '3260', '3261', '3262', '3263', '3264', '3265', '3266', '3267', '3268', '3269', &
        '3270', '3271', '3272', '3273', '3274', '3275', '3276', '3277', '3278', '3279', &
        '3280', '3281', '3282', '3283', '3284', '3285', '3286', '3287', '3288', '3289', &
        '3290', '3291', '3292', '3293', '3294', '3295', '3296', '3297', '3298', '3299', &
        '3300', '3301', '3302', '3303', '3304', '3305', '3306', '3307', '3308', '3309', &
        '3310', '3311', '3312', '3313', '3314', '3315', '3316', '3317', '3318', '3319', &
        '3320', '3321', '3322', '3323', '3324', '3325', '3326', '3327', '3328', '3329', &
        '3330', '3331', '3332', '3333', '3334', '3335', '3336', '3337', '3338', '3339', &
        '3340', '3341', '3342', '3343', '3344', '3345', '3346', '3347', '3348', '3349', &
        '3350', '3351', '3352', '3353', '3354', '3355', '3356', '3357', '3358', '3359', &
        '3360', '3361', '3362', '3363', '3364', '3365', '3366', '3367', '3368', '3369', &
        '3370', '3371', '3372', '3373', '3374', '3375', '3376', '3377', '3378', '3379', &
        '3380', '3381', '3382', '3383', '3384', '3385', '3386', '3387', '3388', '3389', &
        '3390', '3391', '3392', '3393', '3394', '3395', '3396', '3397', '3398', '3399', &
        '3400', '3401', '3402', '3403', '3404', '3405', '3406', '3407', '3408', '3409', &
        '3410', '3411', '3412', '3413', '3414', '3415', '3416', '3417', '3418', '3419', &
        '3420', '3421', '3422', '3423', '3424', '3425', '3426', '3427', '3428', '3429', &
        '3430', '3431', '3432', '3433', '3434', '3435', '3436', '3437', '3438', '3439', &
        '3440', '3441', '3442', '3443', '3444', '3445', '3446', '3447', '3448', '3449', &
        '3450', '3451', '3452', '3453', '3454', '3455', '3456', '3457', '3458', '3459', &
        '3460', '3461', '3462', '3463', '3464', '3465', '3466', '3467', '3468', '3469', &
        '3470', '3471', '3472', '3473', '3474', '3475', '3476', '3477', '3478', '3479', &
        '3480', '3481', '3482', '3483', '3484', '3485', '3486', '3487', '3488', '3489', &
        '3490', '3491', '3492', '3493', '3494', '3495', '3496', '3497', '3498', '3499', &
        '3500', '3501', '3502', '3503', '3504', '3505', '3506', '3507', '3508', '3509', &
        '3510', '3511', '3512', '3513', '3514', '3515', '3516', '3517', '3518', '3519', &
        '3520', '3521', '3522', '3523', '3524', '3525', '3526', '3527', '3528', '3529', &
        '3530', '3531', '3532', '3533', '3534', '3535', '3536', '3537', '3538', '3539', &
        '3540', '3541', '3542', '3543', '3544', '3545', '3546', '3547', '3548', '3549', &
        '3550', '3551', '3552', '3553', '3554', '3555', '3556', '3557', '3558', '3559', &
        '3560', '3561', '3562', '3563', '3564', '3565', '3566', '3567', '3568', '3569', &
        '3570', '3571', '3572', '3573', '3574', '3575', '3576', '3577', '3578', '3579', &
        '3580', '3581', '3582', '3583', '3584', '3585', '3586', '3587', '3588', '3589', &
        '3590', '3591', '3592', '3593', '3594', '3595', '3596', '3597', '3598', '3599', &
        '3600', '3601', '3602', '3603', '3604', '3605', '3606', '3607', '3608', '3609', &
        '3610', '3611', '3612', '3613', '3614', '3615', '3616', '3617', '3618', '3619', &
        '3620', '3621', '3622', '3623', '3624', '3625', '3626', '3627', '3628', '3629', &
        '3630', '3631', '3632', '3633', '3634', '3635', '3636', '3637', '3638', '3639', &
        '3640', '3641', '3642', '3643', '3644', '3645', '3646', '3647', '3648', '3649', &
        '3650', '3651', '3652', '3653', '3654', '3655', '3656', '3657', '3658', '3659', &
        '3660', '3661', '3662', '3663', '3664', '3665', '3666', '3667', '3668', '3669', &
        '3670', '3671', '3672', '3673', '3674', '3675', '3676', '3677', '3678', '3679', &
        '3680', '3681', '3682', '3683', '3684', '3685', '3686', '3687', '3688', '3689', &
        '3690', '3691', '3692', '3693', '3694', '3695', '3696', '3697', '3698', '3699', &
        '3700', '3701', '3702', '3703', '3704', '3705', '3706', '3707', '3708', '3709', &
        '3710', '3711', '3712', '3713', '3714', '3715', '3716', '3717', '3718', '3719', &
        '3720', '3721', '3722', '3723', '3724', '3725', '3726', '3727', '3728', '3729', &
        '3730', '3731', '3732', '3733', '3734', '3735', '3736', '3737', '3738', '3739', &
        '3740', '3741', '3742', '3743', '3744', '3745', '3746', '3747', '3748', '3749', &
        '3750', '3751', '3752', '3753', '3754', '3755', '3756', '3757', '3758', '3759', &
        '3760', '3761', '3762', '3763', '3764', '3765', '3766', '3767', '3768', '3769', &
        '3770', '3771', '3772', '3773', '3774', '3775', '3776', '3777', '3778', '3779', &
        '3780', '3781', '3782', '3783', '3784', '3785', '3786', '3787', '3788', '3789', &
        '3790', '3791', '3792', '3793', '3794', '3795', '3796', '3797', '3798', '3799', &
        '3800', '3801', '3802', '3803', '3804', '3805', '3806', '3807', '3808', '3809', &
        '3810', '3811', '3812', '3813', '3814', '3815', '3816', '3817', '3818', '3819', &
        '3820', '3821', '3822', '3823', '3824', '3825', '3826', '3827', '3828', '3829', &
        '3830', '3831', '3832', '3833', '3834', '3835', '3836', '3837', '3838', '3839', &
        '3840', '3841', '3842', '3843', '3844', '3845', '3846', '3847', '3848', '3849', &
        '3850', '3851', '3852', '3853', '3854', '3855', '3856', '3857', '3858', '3859', &
        '3860', '3861', '3862', '3863', '3864', '3865', '3866', '3867', '3868', '3869', &
        '3870', '3871', '3872', '3873', '3874', '3875', '3876', '3877', '3878', '3879', &
        '3880', '3881', '3882', '3883', '3884', '3885', '3886', '3887', '3888', '3889', &
        '3890', '3891', '3892', '3893', '3894', '3895', '3896', '3897', '3898', '3899', &
        '3900', '3901', '3902', '3903', '3904', '3905', '3906', '3907', '3908', '3909', &
        '3910', '3911', '3912', '3913', '3914', '3915', '3916', '3917', '3918', '3919', &
        '3920', '3921', '3922', '3923', '3924', '3925', '3926', '3927', '3928', '3929', &
        '3930', '3931', '3932', '3933', '3934', '3935', '3936', '3937', '3938', '3939', &
        '3940', '3941', '3942', '3943', '3944', '3945', '3946', '3947', '3948', '3949', &
        '3950', '3951', '3952', '3953', '3954', '3955', '3956', '3957', '3958', '3959', &
        '3960', '3961', '3962', '3963', '3964', '3965', '3966', '3967', '3968', '3969', &
        '3970', '3971', '3972', '3973', '3974', '3975', '3976', '3977', '3978', '3979', &
        '3980', '3981', '3982', '3983', '3984', '3985', '3986', '3987', '3988', '3989', &
        '3990', '3991', '3992', '3993', '3994', '3995', '3996', '3997', '3998', '3999', &
        '4000', '4001', '4002', '4003', '4004', '4005', '4006', '4007', '4008', '4009', &
        '4010', '4011', '4012', '4013', '4014', '4015', '4016', '4017', '4018', '4019', &
        '4020', '4021', '4022', '4023', '4024', '4025', '4026', '4027', '4028', '4029', &
        '4030', '4031', '4032', '4033', '4034', '4035', '4036', '4037', '4038', '4039', &
        '4040', '4041', '4042', '4043', '4044', '4045', '4046', '4047', '4048', '4049', &
        '4050', '4051', '4052', '4053', '4054', '4055', '4056', '4057', '4058', '4059', &
        '4060', '4061', '4062', '4063', '4064', '4065', '4066', '4067', '4068', '4069', &
        '4070', '4071', '4072', '4073', '4074', '4075', '4076', '4077', '4078', '4079', &
        '4080', '4081', '4082', '4083', '4084', '4085', '4086', '4087', '4088', '4089', &
        '4090', '4091', '4092', '4093', '4094', '4095', '4096', '4097', '4098', '4099', &
        '4100', '4101', '4102', '4103', '4104', '4105', '4106', '4107', '4108', '4109', &
        '4110', '4111', '4112', '4113', '4114', '4115', '4116', '4117', '4118', '4119', &
        '4120', '4121', '4122', '4123', '4124', '4125', '4126', '4127', '4128', '4129', &
        '4130', '4131', '4132', '4133', '4134', '4135', '4136', '4137', '4138', '4139', &
        '4140', '4141', '4142', '4143', '4144', '4145', '4146', '4147', '4148', '4149', &
        '4150', '4151', '4152', '4153', '4154', '4155', '4156', '4157', '4158', '4159', &
        '4160', '4161', '4162', '4163', '4164', '4165', '4166', '4167', '4168', '4169', &
        '4170', '4171', '4172', '4173', '4174', '4175', '4176', '4177', '4178', '4179', &
        '4180', '4181', '4182', '4183', '4184', '4185', '4186', '4187', '4188', '4189', &
        '4190', '4191', '4192', '4193', '4194', '4195', '4196', '4197', '4198', '4199', &
        '4200', '4201', '4202', '4203', '4204', '4205', '4206', '4207', '4208', '4209', &
        '4210', '4211', '4212', '4213', '4214', '4215', '4216', '4217', '4218', '4219', &
        '4220', '4221', '4222', '4223', '4224', '4225', '4226', '4227', '4228', '4229', &
        '4230', '4231', '4232', '4233', '4234', '4235', '4236', '4237', '4238', '4239', &
        '4240', '4241', '4242', '4243', '4244', '4245', '4246', '4247', '4248', '4249', &
        '4250', '4251', '4252', '4253', '4254', '4255', '4256', '4257', '4258', '4259', &
        '4260', '4261', '4262', '4263', '4264', '4265', '4266', '4267', '4268', '4269', &
        '4270', '4271', '4272', '4273', '4274', '4275', '4276', '4277', '4278', '4279', &
        '4280', '4281', '4282', '4283', '4284', '4285', '4286', '4287', '4288', '4289', &
        '4290', '4291', '4292', '4293', '4294', '4295', '4296', '4297', '4298', '4299', &
        '4300', '4301', '4302', '4303', '4304', '4305', '4306', '4307', '4308', '4309', &
        '4310', '4311', '4312', '4313', '4314', '4315', '4316', '4317', '4318', '4319', &
        '4320', '4321', '4322', '4323', '4324', '4325', '4326', '4327', '4328', '4329', &
        '4330', '4331', '4332', '4333', '4334', '4335', '4336', '4337', '4338', '4339', &
        '4340', '4341', '4342', '4343', '4344', '4345', '4346', '4347', '4348', '4349', &
        '4350', '4351', '4352', '4353', '4354', '4355', '4356', '4357', '4358', '4359', &
        '4360', '4361', '4362', '4363', '4364', '4365', '4366', '4367', '4368', '4369', &
        '4370', '4371', '4372', '4373', '4374', '4375', '4376', '4377', '4378', '4379', &
        '4380', '4381', '4382', '4383', '4384', '4385', '4386', '4387', '4388', '4389', &
        '4390', '4391', '4392', '4393', '4394', '4395', '4396', '4397', '4398', '4399', &
        '4400', '4401', '4402', '4403', '4404', '4405', '4406', '4407', '4408', '4409', &
        '4410', '4411', '4412', '4413', '4414', '4415', '4416', '4417', '4418', '4419', &
        '4420', '4421', '4422', '4423', '4424', '4425', '4426', '4427', '4428', '4429', &
        '4430', '4431', '4432', '4433', '4434', '4435', '4436', '4437', '4438', '4439', &
        '4440', '4441', '4442', '4443', '4444', '4445', '4446', '4447', '4448', '4449', &
        '4450', '4451', '4452', '4453', '4454', '4455', '4456', '4457', '4458', '4459', &
        '4460', '4461', '4462', '4463', '4464', '4465', '4466', '4467', '4468', '4469', &
        '4470', '4471', '4472', '4473', '4474', '4475', '4476', '4477', '4478', '4479', &
        '4480', '4481', '4482', '4483', '4484', '4485', '4486', '4487', '4488', '4489', &
        '4490', '4491', '4492', '4493', '4494', '4495', '4496', '4497', '4498', '4499', &
        '4500', '4501', '4502', '4503', '4504', '4505', '4506', '4507', '4508', '4509', &
        '4510', '4511', '4512', '4513', '4514', '4515', '4516', '4517', '4518', '4519', &
        '4520', '4521', '4522', '4523', '4524', '4525', '4526', '4527', '4528', '4529', &
        '4530', '4531', '4532', '4533', '4534', '4535', '4536', '4537', '4538', '4539', &
        '4540', '4541', '4542', '4543', '4544', '4545', '4546', '4547', '4548', '4549', &
        '4550', '4551', '4552', '4553', '4554', '4555', '4556', '4557', '4558', '4559', &
        '4560', '4561', '4562', '4563', '4564', '4565', '4566', '4567', '4568', '4569', &
        '4570', '4571', '4572', '4573', '4574', '4575', '4576', '4577', '4578', '4579', &
        '4580', '4581', '4582', '4583', '4584', '4585', '4586', '4587', '4588', '4589', &
        '4590', '4591', '4592', '4593', '4594', '4595', '4596', '4597', '4598', '4599', &
        '4600', '4601', '4602', '4603', '4604', '4605', '4606', '4607', '4608', '4609', &
        '4610', '4611', '4612', '4613', '4614', '4615', '4616', '4617', '4618', '4619', &
        '4620', '4621', '4622', '4623', '4624', '4625', '4626', '4627', '4628', '4629', &
        '4630', '4631', '4632', '4633', '4634', '4635', '4636', '4637', '4638', '4639', &
        '4640', '4641', '4642', '4643', '4644', '4645', '4646', '4647', '4648', '4649', &
        '4650', '4651', '4652', '4653', '4654', '4655', '4656', '4657', '4658', '4659', &
        '4660', '4661', '4662', '4663', '4664', '4665', '4666', '4667', '4668', '4669', &
        '4670', '4671', '4672', '4673', '4674', '4675', '4676', '4677', '4678', '4679', &
        '4680', '4681', '4682', '4683', '4684', '4685', '4686', '4687', '4688', '4689', &
        '4690', '4691', '4692', '4693', '4694', '4695', '4696', '4697', '4698', '4699', &
        '4700', '4701', '4702', '4703', '4704', '4705', '4706', '4707', '4708', '4709', &
        '4710', '4711', '4712', '4713', '4714', '4715', '4716', '4717', '4718', '4719', &
        '4720', '4721', '4722', '4723', '4724', '4725', '4726', '4727', '4728', '4729', &
        '4730', '4731', '4732', '4733', '4734', '4735', '4736', '4737', '4738', '4739', &
        '4740', '4741', '4742', '4743', '4744', '4745', '4746', '4747', '4748', '4749', &
        '4750', '4751', '4752', '4753', '4754', '4755', '4756', '4757', '4758', '4759', &
        '4760', '4761', '4762', '4763', '4764', '4765', '4766', '4767', '4768', '4769', &
        '4770', '4771', '4772', '4773', '4774', '4775', '4776', '4777', '4778', '4779', &
        '4780', '4781', '4782', '4783', '4784', '4785', '4786', '4787', '4788', '4789', &
        '4790', '4791', '4792', '4793', '4794', '4795', '4796', '4797', '4798', '4799', &
        '4800', '4801', '4802', '4803', '4804', '4805', '4806', '4807', '4808', '4809', &
        '4810', '4811', '4812', '4813', '4814', '4815', '4816', '4817', '4818', '4819', &
        '4820', '4821', '4822', '4823', '4824', '4825', '4826', '4827', '4828', '4829', &
        '4830', '4831', '4832', '4833', '4834', '4835', '4836', '4837', '4838', '4839', &
        '4840', '4841', '4842', '4843', '4844', '4845', '4846', '4847', '4848', '4849', &
        '4850', '4851', '4852', '4853', '4854', '4855', '4856', '4857', '4858', '4859', &
        '4860', '4861', '4862', '4863', '4864', '4865', '4866', '4867', '4868', '4869', &
        '4870', '4871', '4872', '4873', '4874', '4875', '4876', '4877', '4878', '4879', &
        '4880', '4881', '4882', '4883', '4884', '4885', '4886', '4887', '4888', '4889', &
        '4890', '4891', '4892', '4893', '4894', '4895', '4896', '4897', '4898', '4899', &
        '4900', '4901', '4902', '4903', '4904', '4905', '4906', '4907', '4908', '4909', &
        '4910', '4911', '4912', '4913', '4914', '4915', '4916', '4917', '4918', '4919', &
        '4920', '4921', '4922', '4923', '4924', '4925', '4926', '4927', '4928', '4929', &
        '4930', '4931', '4932', '4933', '4934', '4935', '4936', '4937', '4938', '4939', &
        '4940', '4941', '4942', '4943', '4944', '4945', '4946', '4947', '4948', '4949', &
        '4950', '4951', '4952', '4953', '4954', '4955', '4956', '4957', '4958', '4959', &
        '4960', '4961', '4962', '4963', '4964', '4965', '4966', '4967', '4968', '4969', &
        '4970', '4971', '4972', '4973', '4974', '4975', '4976', '4977', '4978', '4979', &
        '4980', '4981', '4982', '4983', '4984', '4985', '4986', '4987', '4988', '4989', &
        '4990', '4991', '4992', '4993', '4994', '4995', '4996', '4997', '4998', '4999', &
        '5000', '5001', '5002', '5003', '5004', '5005', '5006', '5007', '5008', '5009', &
        '5010', '5011', '5012', '5013', '5014', '5015', '5016', '5017', '5018', '5019', &
        '5020', '5021', '5022', '5023', '5024', '5025', '5026', '5027', '5028', '5029', &
        '5030', '5031', '5032', '5033', '5034', '5035', '5036', '5037', '5038', '5039', &
        '5040', '5041', '5042', '5043', '5044', '5045', '5046', '5047', '5048', '5049', &
        '5050', '5051', '5052', '5053', '5054', '5055', '5056', '5057', '5058', '5059', &
        '5060', '5061', '5062', '5063', '5064', '5065', '5066', '5067', '5068', '5069', &
        '5070', '5071', '5072', '5073', '5074', '5075', '5076', '5077', '5078', '5079', &
        '5080', '5081', '5082', '5083', '5084', '5085', '5086', '5087', '5088', '5089', &
        '5090', '5091', '5092', '5093', '5094', '5095', '5096', '5097', '5098', '5099', &
        '5100', '5101', '5102', '5103', '5104', '5105', '5106', '5107', '5108', '5109', &
        '5110', '5111', '5112', '5113', '5114', '5115', '5116', '5117', '5118', '5119', &
        '5120', '5121', '5122', '5123', '5124', '5125', '5126', '5127', '5128', '5129', &
        '5130', '5131', '5132', '5133', '5134', '5135', '5136', '5137', '5138', '5139', &
        '5140', '5141', '5142', '5143', '5144', '5145', '5146', '5147', '5148', '5149', &
        '5150', '5151', '5152', '5153', '5154', '5155', '5156', '5157', '5158', '5159', &
        '5160', '5161', '5162', '5163', '5164', '5165', '5166', '5167', '5168', '5169', &
        '5170', '5171', '5172', '5173', '5174', '5175', '5176', '5177', '5178', '5179', &
        '5180', '5181', '5182', '5183', '5184', '5185', '5186', '5187', '5188', '5189', &
        '5190', '5191', '5192', '5193', '5194', '5195', '5196', '5197', '5198', '5199', &
        '5200', '5201', '5202', '5203', '5204', '5205', '5206', '5207', '5208', '5209', &
        '5210', '5211', '5212', '5213', '5214', '5215', '5216', '5217', '5218', '5219', &
        '5220', '5221', '5222', '5223', '5224', '5225', '5226', '5227', '5228', '5229', &
        '5230', '5231', '5232', '5233', '5234', '5235', '5236', '5237', '5238', '5239', &
        '5240', '5241', '5242', '5243', '5244', '5245', '5246', '5247', '5248', '5249', &
        '5250', '5251', '5252', '5253', '5254', '5255', '5256', '5257', '5258', '5259', &
        '5260', '5261', '5262', '5263', '5264', '5265', '5266', '5267', '5268', '5269', &
        '5270', '5271', '5272', '5273', '5274', '5275', '5276', '5277', '5278', '5279', &
        '5280', '5281', '5282', '5283', '5284', '5285', '5286', '5287', '5288', '5289', &
        '5290', '5291', '5292', '5293', '5294', '5295', '5296', '5297', '5298', '5299', &
        '5300', '5301', '5302', '5303', '5304', '5305', '5306', '5307', '5308', '5309', &
        '5310', '5311', '5312', '5313', '5314', '5315', '5316', '5317', '5318', '5319', &
        '5320', '5321', '5322', '5323', '5324', '5325', '5326', '5327', '5328', '5329', &
        '5330', '5331', '5332', '5333', '5334', '5335', '5336', '5337', '5338', '5339', &
        '5340', '5341', '5342', '5343', '5344', '5345', '5346', '5347', '5348', '5349', &
        '5350', '5351', '5352', '5353', '5354', '5355', '5356', '5357', '5358', '5359', &
        '5360', '5361', '5362', '5363', '5364', '5365', '5366', '5367', '5368', '5369', &
        '5370', '5371', '5372', '5373', '5374', '5375', '5376', '5377', '5378', '5379', &
        '5380', '5381', '5382', '5383', '5384', '5385', '5386', '5387', '5388', '5389', &
        '5390', '5391', '5392', '5393', '5394', '5395', '5396', '5397', '5398', '5399', &
        '5400', '5401', '5402', '5403', '5404', '5405', '5406', '5407', '5408', '5409', &
        '5410', '5411', '5412', '5413', '5414', '5415', '5416', '5417', '5418', '5419', &
        '5420', '5421', '5422', '5423', '5424', '5425', '5426', '5427', '5428', '5429', &
        '5430', '5431', '5432', '5433', '5434', '5435', '5436', '5437', '5438', '5439', &
        '5440', '5441', '5442', '5443', '5444', '5445', '5446', '5447', '5448', '5449', &
        '5450', '5451', '5452', '5453', '5454', '5455', '5456', '5457', '5458', '5459', &
        '5460', '5461', '5462', '5463', '5464', '5465', '5466', '5467', '5468', '5469', &
        '5470', '5471', '5472', '5473', '5474', '5475', '5476', '5477', '5478', '5479', &
        '5480', '5481', '5482', '5483', '5484', '5485', '5486', '5487', '5488', '5489', &
        '5490', '5491', '5492', '5493', '5494', '5495', '5496', '5497', '5498', '5499', &
        '5500', '5501', '5502', '5503', '5504', '5505', '5506', '5507', '5508', '5509', &
        '5510', '5511', '5512', '5513', '5514', '5515', '5516', '5517', '5518', '5519', &
        '5520', '5521', '5522', '5523', '5524', '5525', '5526', '5527', '5528', '5529', &
        '5530', '5531', '5532', '5533', '5534', '5535', '5536', '5537', '5538', '5539', &
        '5540', '5541', '5542', '5543', '5544', '5545', '5546', '5547', '5548', '5549', &
        '5550', '5551', '5552', '5553', '5554', '5555', '5556', '5557', '5558', '5559', &
        '5560', '5561', '5562', '5563', '5564', '5565', '5566', '5567', '5568', '5569', &
        '5570', '5571', '5572', '5573', '5574', '5575', '5576', '5577', '5578', '5579', &
        '5580', '5581', '5582', '5583', '5584', '5585', '5586', '5587', '5588', '5589', &
        '5590', '5591', '5592', '5593', '5594', '5595', '5596', '5597', '5598', '5599', &
        '5600', '5601', '5602', '5603', '5604', '5605', '5606', '5607', '5608', '5609', &
        '5610', '5611', '5612', '5613', '5614', '5615', '5616', '5617', '5618', '5619', &
        '5620', '5621', '5622', '5623', '5624', '5625', '5626', '5627', '5628', '5629', &
        '5630', '5631', '5632', '5633', '5634', '5635', '5636', '5637', '5638', '5639', &
        '5640', '5641', '5642', '5643', '5644', '5645', '5646', '5647', '5648', '5649', &
        '5650', '5651', '5652', '5653', '5654', '5655', '5656', '5657', '5658', '5659', &
        '5660', '5661', '5662', '5663', '5664', '5665', '5666', '5667', '5668', '5669', &
        '5670', '5671', '5672', '5673', '5674', '5675', '5676', '5677', '5678', '5679', &
        '5680', '5681', '5682', '5683', '5684', '5685', '5686', '5687', '5688', '5689', &
        '5690', '5691', '5692', '5693', '5694', '5695', '5696', '5697', '5698', '5699', &
        '5700', '5701', '5702', '5703', '5704', '5705', '5706', '5707', '5708', '5709', &
        '5710', '5711', '5712', '5713', '5714', '5715', '5716', '5717', '5718', '5719', &
        '5720', '5721', '5722', '5723', '5724', '5725', '5726', '5727', '5728', '5729', &
        '5730', '5731', '5732', '5733', '5734', '5735', '5736', '5737', '5738', '5739', &
        '5740', '5741', '5742', '5743', '5744', '5745', '5746', '5747', '5748', '5749', &
        '5750', '5751', '5752', '5753', '5754', '5755', '5756', '5757', '5758', '5759', &
        '5760', '5761', '5762', '5763', '5764', '5765', '5766', '5767', '5768', '5769', &
        '5770', '5771', '5772', '5773', '5774', '5775', '5776', '5777', '5778', '5779', &
        '5780', '5781', '5782', '5783', '5784', '5785', '5786', '5787', '5788', '5789', &
        '5790', '5791', '5792', '5793', '5794', '5795', '5796', '5797', '5798', '5799', &
        '5800', '5801', '5802', '5803', '5804', '5805', '5806', '5807', '5808', '5809', &
        '5810', '5811', '5812', '5813', '5814', '5815', '5816', '5817', '5818', '5819', &
        '5820', '5821', '5822', '5823', '5824', '5825', '5826', '5827', '5828', '5829', &
        '5830', '5831', '5832', '5833', '5834', '5835', '5836', '5837', '5838', '5839', &
        '5840', '5841', '5842', '5843', '5844', '5845', '5846', '5847', '5848', '5849', &
        '5850', '5851', '5852', '5853', '5854', '5855', '5856', '5857', '5858', '5859', &
        '5860', '5861', '5862', '5863', '5864', '5865', '5866', '5867', '5868', '5869', &
        '5870', '5871', '5872', '5873', '5874', '5875', '5876', '5877', '5878', '5879', &
        '5880', '5881', '5882', '5883', '5884', '5885', '5886', '5887', '5888', '5889', &
        '5890', '5891', '5892', '5893', '5894', '5895', '5896', '5897', '5898', '5899', &
        '5900', '5901', '5902', '5903', '5904', '5905', '5906', '5907', '5908', '5909', &
        '5910', '5911', '5912', '5913', '5914', '5915', '5916', '5917', '5918', '5919', &
        '5920', '5921', '5922', '5923', '5924', '5925', '5926', '5927', '5928', '5929', &
        '5930', '5931', '5932', '5933', '5934', '5935', '5936', '5937', '5938', '5939', &
        '5940', '5941', '5942', '5943', '5944', '5945', '5946', '5947', '5948', '5949', &
        '5950', '5951', '5952', '5953', '5954', '5955', '5956', '5957', '5958', '5959', &
        '5960', '5961', '5962', '5963', '5964', '5965', '5966', '5967', '5968', '5969', &
        '5970', '5971', '5972', '5973', '5974', '5975', '5976', '5977', '5978', '5979', &
        '5980', '5981', '5982', '5983', '5984', '5985', '5986', '5987', '5988', '5989', &
        '5990', '5991', '5992', '5993', '5994', '5995', '5996', '5997', '5998', '5999', &
        '6000', '6001', '6002', '6003', '6004', '6005', '6006', '6007', '6008', '6009', &
        '6010', '6011', '6012', '6013', '6014', '6015', '6016', '6017', '6018', '6019', &
        '6020', '6021', '6022', '6023', '6024', '6025', '6026', '6027', '6028', '6029', &
        '6030', '6031', '6032', '6033', '6034', '6035', '6036', '6037', '6038', '6039', &
        '6040', '6041', '6042', '6043', '6044', '6045', '6046', '6047', '6048', '6049', &
        '6050', '6051', '6052', '6053', '6054', '6055', '6056', '6057', '6058', '6059', &
        '6060', '6061', '6062', '6063', '6064', '6065', '6066', '6067', '6068', '6069', &
        '6070', '6071', '6072', '6073', '6074', '6075', '6076', '6077', '6078', '6079', &
        '6080', '6081', '6082', '6083', '6084', '6085', '6086', '6087', '6088', '6089', &
        '6090', '6091', '6092', '6093', '6094', '6095', '6096', '6097', '6098', '6099', &
        '6100', '6101', '6102', '6103', '6104', '6105', '6106', '6107', '6108', '6109', &
        '6110', '6111', '6112', '6113', '6114', '6115', '6116', '6117', '6118', '6119', &
        '6120', '6121', '6122', '6123', '6124', '6125', '6126', '6127', '6128', '6129', &
        '6130', '6131', '6132', '6133', '6134', '6135', '6136', '6137', '6138', '6139', &
        '6140', '6141', '6142', '6143', '6144', '6145', '6146', '6147', '6148', '6149', &
        '6150', '6151', '6152', '6153', '6154', '6155', '6156', '6157', '6158', '6159', &
        '6160', '6161', '6162', '6163', '6164', '6165', '6166', '6167', '6168', '6169', &
        '6170', '6171', '6172', '6173', '6174', '6175', '6176', '6177', '6178', '6179', &
        '6180', '6181', '6182', '6183', '6184', '6185', '6186', '6187', '6188', '6189', &
        '6190', '6191', '6192', '6193', '6194', '6195', '6196', '6197', '6198', '6199', &
        '6200', '6201', '6202', '6203', '6204', '6205', '6206', '6207', '6208', '6209', &
        '6210', '6211', '6212', '6213', '6214', '6215', '6216', '6217', '6218', '6219', &
        '6220', '6221', '6222', '6223', '6224', '6225', '6226', '6227', '6228', '6229', &
        '6230', '6231', '6232', '6233', '6234', '6235', '6236', '6237', '6238', '6239', &
        '6240', '6241', '6242', '6243', '6244', '6245', '6246', '6247', '6248', '6249', &
        '6250', '6251', '6252', '6253', '6254', '6255', '6256', '6257', '6258', '6259', &
        '6260', '6261', '6262', '6263', '6264', '6265', '6266', '6267', '6268', '6269', &
        '6270', '6271', '6272', '6273', '6274', '6275', '6276', '6277', '6278', '6279', &
        '6280', '6281', '6282', '6283', '6284', '6285', '6286', '6287', '6288', '6289', &
        '6290', '6291', '6292', '6293', '6294', '6295', '6296', '6297', '6298', '6299', &
        '6300', '6301', '6302', '6303', '6304', '6305', '6306', '6307', '6308', '6309', &
        '6310', '6311', '6312', '6313', '6314', '6315', '6316', '6317', '6318', '6319', &
        '6320', '6321', '6322', '6323', '6324', '6325', '6326', '6327', '6328', '6329', &
        '6330', '6331', '6332', '6333', '6334', '6335', '6336', '6337', '6338', '6339', &
        '6340', '6341', '6342', '6343', '6344', '6345', '6346', '6347', '6348', '6349', &
        '6350', '6351', '6352', '6353', '6354', '6355', '6356', '6357', '6358', '6359', &
        '6360', '6361', '6362', '6363', '6364', '6365', '6366', '6367', '6368', '6369', &
        '6370', '6371', '6372', '6373', '6374', '6375', '6376', '6377', '6378', '6379', &
        '6380', '6381', '6382', '6383', '6384', '6385', '6386', '6387', '6388', '6389', &
        '6390', '6391', '6392', '6393', '6394', '6395', '6396', '6397', '6398', '6399', &
        '6400', '6401', '6402', '6403', '6404', '6405', '6406', '6407', '6408', '6409', &
        '6410', '6411', '6412', '6413', '6414', '6415', '6416', '6417', '6418', '6419', &
        '6420', '6421', '6422', '6423', '6424', '6425', '6426', '6427', '6428', '6429', &
        '6430', '6431', '6432', '6433', '6434', '6435', '6436', '6437', '6438', '6439', &
        '6440', '6441', '6442', '6443', '6444', '6445', '6446', '6447', '6448', '6449', &
        '6450', '6451', '6452', '6453', '6454', '6455', '6456', '6457', '6458', '6459', &
        '6460', '6461', '6462', '6463', '6464', '6465', '6466', '6467', '6468', '6469', &
        '6470', '6471', '6472', '6473', '6474', '6475', '6476', '6477', '6478', '6479', &
        '6480', '6481', '6482', '6483', '6484', '6485', '6486', '6487', '6488', '6489', &
        '6490', '6491', '6492', '6493', '6494', '6495', '6496', '6497', '6498', '6499', &
        '6500', '6501', '6502', '6503', '6504', '6505', '6506', '6507', '6508', '6509', &
        '6510', '6511', '6512', '6513', '6514', '6515', '6516', '6517', '6518', '6519', &
        '6520', '6521', '6522', '6523', '6524', '6525', '6526', '6527', '6528', '6529', &
        '6530', '6531', '6532', '6533', '6534', '6535', '6536', '6537', '6538', '6539', &
        '6540', '6541', '6542', '6543', '6544', '6545', '6546', '6547', '6548', '6549', &
        '6550', '6551', '6552', '6553', '6554', '6555', '6556', '6557', '6558', '6559', &
        '6560', '6561', '6562', '6563', '6564', '6565', '6566', '6567', '6568', '6569', &
        '6570', '6571', '6572', '6573', '6574', '6575', '6576', '6577', '6578', '6579', &
        '6580', '6581', '6582', '6583', '6584', '6585', '6586', '6587', '6588', '6589', &
        '6590', '6591', '6592', '6593', '6594', '6595', '6596', '6597', '6598', '6599', &
        '6600', '6601', '6602', '6603', '6604', '6605', '6606', '6607', '6608', '6609', &
        '6610', '6611', '6612', '6613', '6614', '6615', '6616', '6617', '6618', '6619', &
        '6620', '6621', '6622', '6623', '6624', '6625', '6626', '6627', '6628', '6629', &
        '6630', '6631', '6632', '6633', '6634', '6635', '6636', '6637', '6638', '6639', &
        '6640', '6641', '6642', '6643', '6644', '6645', '6646', '6647', '6648', '6649', &
        '6650', '6651', '6652', '6653', '6654', '6655', '6656', '6657', '6658', '6659', &
        '6660', '6661', '6662', '6663', '6664', '6665', '6666', '6667', '6668', '6669', &
        '6670', '6671', '6672', '6673', '6674', '6675', '6676', '6677', '6678', '6679', &
        '6680', '6681', '6682', '6683', '6684', '6685', '6686', '6687', '6688', '6689', &
        '6690', '6691', '6692', '6693', '6694', '6695', '6696', '6697', '6698', '6699', &
        '6700', '6701', '6702', '6703', '6704', '6705', '6706', '6707', '6708', '6709', &
        '6710', '6711', '6712', '6713', '6714', '6715', '6716', '6717', '6718', '6719', &
        '6720', '6721', '6722', '6723', '6724', '6725', '6726', '6727', '6728', '6729', &
        '6730', '6731', '6732', '6733', '6734', '6735', '6736', '6737', '6738', '6739', &
        '6740', '6741', '6742', '6743', '6744', '6745', '6746', '6747', '6748', '6749', &
        '6750', '6751', '6752', '6753', '6754', '6755', '6756', '6757', '6758', '6759', &
        '6760', '6761', '6762', '6763', '6764', '6765', '6766', '6767', '6768', '6769', &
        '6770', '6771', '6772', '6773', '6774', '6775', '6776', '6777', '6778', '6779', &
        '6780', '6781', '6782', '6783', '6784', '6785', '6786', '6787', '6788', '6789', &
        '6790', '6791', '6792', '6793', '6794', '6795', '6796', '6797', '6798', '6799', &
        '6800', '6801', '6802', '6803', '6804', '6805', '6806', '6807', '6808', '6809', &
        '6810', '6811', '6812', '6813', '6814', '6815', '6816', '6817', '6818', '6819', &
        '6820', '6821', '6822', '6823', '6824', '6825', '6826', '6827', '6828', '6829', &
        '6830', '6831', '6832', '6833', '6834', '6835', '6836', '6837', '6838', '6839', &
        '6840', '6841', '6842', '6843', '6844', '6845', '6846', '6847', '6848', '6849', &
        '6850', '6851', '6852', '6853', '6854', '6855', '6856', '6857', '6858', '6859', &
        '6860', '6861', '6862', '6863', '6864', '6865', '6866', '6867', '6868', '6869', &
        '6870', '6871', '6872', '6873', '6874', '6875', '6876', '6877', '6878', '6879', &
        '6880', '6881', '6882', '6883', '6884', '6885', '6886', '6887', '6888', '6889', &
        '6890', '6891', '6892', '6893', '6894', '6895', '6896', '6897', '6898', '6899', &
        '6900', '6901', '6902', '6903', '6904', '6905', '6906', '6907', '6908', '6909', &
        '6910', '6911', '6912', '6913', '6914', '6915', '6916', '6917', '6918', '6919', &
        '6920', '6921', '6922', '6923', '6924', '6925', '6926', '6927', '6928', '6929', &
        '6930', '6931', '6932', '6933', '6934', '6935', '6936', '6937', '6938', '6939', &
        '6940', '6941', '6942', '6943', '6944', '6945', '6946', '6947', '6948', '6949', &
        '6950', '6951', '6952', '6953', '6954', '6955', '6956', '6957', '6958', '6959', &
        '6960', '6961', '6962', '6963', '6964', '6965', '6966', '6967', '6968', '6969', &
        '6970', '6971', '6972', '6973', '6974', '6975', '6976', '6977', '6978', '6979', &
        '6980', '6981', '6982', '6983', '6984', '6985', '6986', '6987', '6988', '6989', &
        '6990', '6991', '6992', '6993', '6994', '6995', '6996', '6997', '6998', '6999', &
        '7000', '7001', '7002', '7003', '7004', '7005', '7006', '7007', '7008', '7009', &
        '7010', '7011', '7012', '7013', '7014', '7015', '7016', '7017', '7018', '7019', &
        '7020', '7021', '7022', '7023', '7024', '7025', '7026', '7027', '7028', '7029', &
        '7030', '7031', '7032', '7033', '7034', '7035', '7036', '7037', '7038', '7039', &
        '7040', '7041', '7042', '7043', '7044', '7045', '7046', '7047', '7048', '7049', &
        '7050', '7051', '7052', '7053', '7054', '7055', '7056', '7057', '7058', '7059', &
        '7060', '7061', '7062', '7063', '7064', '7065', '7066', '7067', '7068', '7069', &
        '7070', '7071', '7072', '7073', '7074', '7075', '7076', '7077', '7078', '7079', &
        '7080', '7081', '7082', '7083', '7084', '7085', '7086', '7087', '7088', '7089', &
        '7090', '7091', '7092', '7093', '7094', '7095', '7096', '7097', '7098', '7099', &
        '7100', '7101', '7102', '7103', '7104', '7105', '7106', '7107', '7108', '7109', &
        '7110', '7111', '7112', '7113', '7114', '7115', '7116', '7117', '7118', '7119', &
        '7120', '7121', '7122', '7123', '7124', '7125', '7126', '7127', '7128', '7129', &
        '7130', '7131', '7132', '7133', '7134', '7135', '7136', '7137', '7138', '7139', &
        '7140', '7141', '7142', '7143', '7144', '7145', '7146', '7147', '7148', '7149', &
        '7150', '7151', '7152', '7153', '7154', '7155', '7156', '7157', '7158', '7159', &
        '7160', '7161', '7162', '7163', '7164', '7165', '7166', '7167', '7168', '7169', &
        '7170', '7171', '7172', '7173', '7174', '7175', '7176', '7177', '7178', '7179', &
        '7180', '7181', '7182', '7183', '7184', '7185', '7186', '7187', '7188', '7189', &
        '7190', '7191', '7192', '7193', '7194', '7195', '7196', '7197', '7198', '7199', &
        '7200', '7201', '7202', '7203', '7204', '7205', '7206', '7207', '7208', '7209', &
        '7210', '7211', '7212', '7213', '7214', '7215', '7216', '7217', '7218', '7219', &
        '7220', '7221', '7222', '7223', '7224', '7225', '7226', '7227', '7228', '7229', &
        '7230', '7231', '7232', '7233', '7234', '7235', '7236', '7237', '7238', '7239', &
        '7240', '7241', '7242', '7243', '7244', '7245', '7246', '7247', '7248', '7249', &
        '7250', '7251', '7252', '7253', '7254', '7255', '7256', '7257', '7258', '7259', &
        '7260', '7261', '7262', '7263', '7264', '7265', '7266', '7267', '7268', '7269', &
        '7270', '7271', '7272', '7273', '7274', '7275', '7276', '7277', '7278', '7279', &
        '7280', '7281', '7282', '7283', '7284', '7285', '7286', '7287', '7288', '7289', &
        '7290', '7291', '7292', '7293', '7294', '7295', '7296', '7297', '7298', '7299', &
        '7300', '7301', '7302', '7303', '7304', '7305', '7306', '7307', '7308', '7309', &
        '7310', '7311', '7312', '7313', '7314', '7315', '7316', '7317', '7318', '7319', &
        '7320', '7321', '7322', '7323', '7324', '7325', '7326', '7327', '7328', '7329', &
        '7330', '7331', '7332', '7333', '7334', '7335', '7336', '7337', '7338', '7339', &
        '7340', '7341', '7342', '7343', '7344', '7345', '7346', '7347', '7348', '7349', &
        '7350', '7351', '7352', '7353', '7354', '7355', '7356', '7357', '7358', '7359', &
        '7360', '7361', '7362', '7363', '7364', '7365', '7366', '7367', '7368', '7369', &
        '7370', '7371', '7372', '7373', '7374', '7375', '7376', '7377', '7378', '7379', &
        '7380', '7381', '7382', '7383', '7384', '7385', '7386', '7387', '7388', '7389', &
        '7390', '7391', '7392', '7393', '7394', '7395', '7396', '7397', '7398', '7399', &
        '7400', '7401', '7402', '7403', '7404', '7405', '7406', '7407', '7408', '7409', &
        '7410', '7411', '7412', '7413', '7414', '7415', '7416', '7417', '7418', '7419', &
        '7420', '7421', '7422', '7423', '7424', '7425', '7426', '7427', '7428', '7429', &
        '7430', '7431', '7432', '7433', '7434', '7435', '7436', '7437', '7438', '7439', &
        '7440', '7441', '7442', '7443', '7444', '7445', '7446', '7447', '7448', '7449', &
        '7450', '7451', '7452', '7453', '7454', '7455', '7456', '7457', '7458', '7459', &
        '7460', '7461', '7462', '7463', '7464', '7465', '7466', '7467', '7468', '7469', &
        '7470', '7471', '7472', '7473', '7474', '7475', '7476', '7477', '7478', '7479', &
        '7480', '7481', '7482', '7483', '7484', '7485', '7486', '7487', '7488', '7489', &
        '7490', '7491', '7492', '7493', '7494', '7495', '7496', '7497', '7498', '7499', &
        '7500', '7501', '7502', '7503', '7504', '7505', '7506', '7507', '7508', '7509', &
        '7510', '7511', '7512', '7513', '7514', '7515', '7516', '7517', '7518', '7519', &
        '7520', '7521', '7522', '7523', '7524', '7525', '7526', '7527', '7528', '7529', &
        '7530', '7531', '7532', '7533', '7534', '7535', '7536', '7537', '7538', '7539', &
        '7540', '7541', '7542', '7543', '7544', '7545', '7546', '7547', '7548', '7549', &
        '7550', '7551', '7552', '7553', '7554', '7555', '7556', '7557', '7558', '7559', &
        '7560', '7561', '7562', '7563', '7564', '7565', '7566', '7567', '7568', '7569', &
        '7570', '7571', '7572', '7573', '7574', '7575', '7576', '7577', '7578', '7579', &
        '7580', '7581', '7582', '7583', '7584', '7585', '7586', '7587', '7588', '7589', &
        '7590', '7591', '7592', '7593', '7594', '7595', '7596', '7597', '7598', '7599', &
        '7600', '7601', '7602', '7603', '7604', '7605', '7606', '7607', '7608', '7609', &
        '7610', '7611', '7612', '7613', '7614', '7615', '7616', '7617', '7618', '7619', &
        '7620', '7621', '7622', '7623', '7624', '7625', '7626', '7627', '7628', '7629', &
        '7630', '7631', '7632', '7633', '7634', '7635', '7636', '7637', '7638', '7639', &
        '7640', '7641', '7642', '7643', '7644', '7645', '7646', '7647', '7648', '7649', &
        '7650', '7651', '7652', '7653', '7654', '7655', '7656', '7657', '7658', '7659', &
        '7660', '7661', '7662', '7663', '7664', '7665', '7666', '7667', '7668', '7669', &
        '7670', '7671', '7672', '7673', '7674', '7675', '7676', '7677', '7678', '7679', &
        '7680', '7681', '7682', '7683', '7684', '7685', '7686', '7687', '7688', '7689', &
        '7690', '7691', '7692', '7693', '7694', '7695', '7696', '7697', '7698', '7699', &
        '7700', '7701', '7702', '7703', '7704', '7705', '7706', '7707', '7708', '7709', &
        '7710', '7711', '7712', '7713', '7714', '7715', '7716', '7717', '7718', '7719', &
        '7720', '7721', '7722', '7723', '7724', '7725', '7726', '7727', '7728', '7729', &
        '7730', '7731', '7732', '7733', '7734', '7735', '7736', '7737', '7738', '7739', &
        '7740', '7741', '7742', '7743', '7744', '7745', '7746', '7747', '7748', '7749', &
        '7750', '7751', '7752', '7753', '7754', '7755', '7756', '7757', '7758', '7759', &
        '7760', '7761', '7762', '7763', '7764', '7765', '7766', '7767', '7768', '7769', &
        '7770', '7771', '7772', '7773', '7774', '7775', '7776', '7777', '7778', '7779', &
        '7780', '7781', '7782', '7783', '7784', '7785', '7786', '7787', '7788', '7789', &
        '7790', '7791', '7792', '7793', '7794', '7795', '7796', '7797', '7798', '7799', &
        '7800', '7801', '7802', '7803', '7804', '7805', '7806', '7807', '7808', '7809', &
        '7810', '7811', '7812', '7813', '7814', '7815', '7816', '7817', '7818', '7819', &
        '7820', '7821', '7822', '7823', '7824', '7825', '7826', '7827', '7828', '7829', &
        '7830', '7831', '7832', '7833', '7834', '7835', '7836', '7837', '7838', '7839', &
        '7840', '7841', '7842', '7843', '7844', '7845', '7846', '7847', '7848', '7849', &
        '7850', '7851', '7852', '7853', '7854', '7855', '7856', '7857', '7858', '7859', &
        '7860', '7861', '7862', '7863', '7864', '7865', '7866', '7867', '7868', '7869', &
        '7870', '7871', '7872', '7873', '7874', '7875', '7876', '7877', '7878', '7879', &
        '7880', '7881', '7882', '7883', '7884', '7885', '7886', '7887', '7888', '7889', &
        '7890', '7891', '7892', '7893', '7894', '7895', '7896', '7897', '7898', '7899', &
        '7900', '7901', '7902', '7903', '7904', '7905', '7906', '7907', '7908', '7909', &
        '7910', '7911', '7912', '7913', '7914', '7915', '7916', '7917', '7918', '7919', &
        '7920', '7921', '7922', '7923', '7924', '7925', '7926', '7927', '7928', '7929', &
        '7930', '7931', '7932', '7933', '7934', '7935', '7936', '7937', '7938', '7939', &
        '7940', '7941', '7942', '7943', '7944', '7945', '7946', '7947', '7948', '7949', &
        '7950', '7951', '7952', '7953', '7954', '7955', '7956', '7957', '7958', '7959', &
        '7960', '7961', '7962', '7963', '7964', '7965', '7966', '7967', '7968', '7969', &
        '7970', '7971', '7972', '7973', '7974', '7975', '7976', '7977', '7978', '7979', &
        '7980', '7981', '7982', '7983', '7984', '7985', '7986', '7987', '7988', '7989', &
        '7990', '7991', '7992', '7993', '7994', '7995', '7996', '7997', '7998', '7999', &
        '8000', '8001', '8002', '8003', '8004', '8005', '8006', '8007', '8008', '8009', &
        '8010', '8011', '8012', '8013', '8014', '8015', '8016', '8017', '8018', '8019', &
        '8020', '8021', '8022', '8023', '8024', '8025', '8026', '8027', '8028', '8029', &
        '8030', '8031', '8032', '8033', '8034', '8035', '8036', '8037', '8038', '8039', &
        '8040', '8041', '8042', '8043', '8044', '8045', '8046', '8047', '8048', '8049', &
        '8050', '8051', '8052', '8053', '8054', '8055', '8056', '8057', '8058', '8059', &
        '8060', '8061', '8062', '8063', '8064', '8065', '8066', '8067', '8068', '8069', &
        '8070', '8071', '8072', '8073', '8074', '8075', '8076', '8077', '8078', '8079', &
        '8080', '8081', '8082', '8083', '8084', '8085', '8086', '8087', '8088', '8089', &
        '8090', '8091', '8092', '8093', '8094', '8095', '8096', '8097', '8098', '8099', &
        '8100', '8101', '8102', '8103', '8104', '8105', '8106', '8107', '8108', '8109', &
        '8110', '8111', '8112', '8113', '8114', '8115', '8116', '8117', '8118', '8119', &
        '8120', '8121', '8122', '8123', '8124', '8125', '8126', '8127', '8128', '8129', &
        '8130', '8131', '8132', '8133', '8134', '8135', '8136', '8137', '8138', '8139', &
        '8140', '8141', '8142', '8143', '8144', '8145', '8146', '8147', '8148', '8149', &
        '8150', '8151', '8152', '8153', '8154', '8155', '8156', '8157', '8158', '8159', &
        '8160', '8161', '8162', '8163', '8164', '8165', '8166', '8167', '8168', '8169', &
        '8170', '8171', '8172', '8173', '8174', '8175', '8176', '8177', '8178', '8179', &
        '8180', '8181', '8182', '8183', '8184', '8185', '8186', '8187', '8188', '8189', &
        '8190', '8191', '8192', '8193', '8194', '8195', '8196', '8197', '8198', '8199', &
        '8200', '8201', '8202', '8203', '8204', '8205', '8206', '8207', '8208', '8209', &
        '8210', '8211', '8212', '8213', '8214', '8215', '8216', '8217', '8218', '8219', &
        '8220', '8221', '8222', '8223', '8224', '8225', '8226', '8227', '8228', '8229', &
        '8230', '8231', '8232', '8233', '8234', '8235', '8236', '8237', '8238', '8239', &
        '8240', '8241', '8242', '8243', '8244', '8245', '8246', '8247', '8248', '8249', &
        '8250', '8251', '8252', '8253', '8254', '8255', '8256', '8257', '8258', '8259', &
        '8260', '8261', '8262', '8263', '8264', '8265', '8266', '8267', '8268', '8269', &
        '8270', '8271', '8272', '8273', '8274', '8275', '8276', '8277', '8278', '8279', &
        '8280', '8281', '8282', '8283', '8284', '8285', '8286', '8287', '8288', '8289', &
        '8290', '8291', '8292', '8293', '8294', '8295', '8296', '8297', '8298', '8299', &
        '8300', '8301', '8302', '8303', '8304', '8305', '8306', '8307', '8308', '8309', &
        '8310', '8311', '8312', '8313', '8314', '8315', '8316', '8317', '8318', '8319', &
        '8320', '8321', '8322', '8323', '8324', '8325', '8326', '8327', '8328', '8329', &
        '8330', '8331', '8332', '8333', '8334', '8335', '8336', '8337', '8338', '8339', &
        '8340', '8341', '8342', '8343', '8344', '8345', '8346', '8347', '8348', '8349', &
        '8350', '8351', '8352', '8353', '8354', '8355', '8356', '8357', '8358', '8359', &
        '8360', '8361', '8362', '8363', '8364', '8365', '8366', '8367', '8368', '8369', &
        '8370', '8371', '8372', '8373', '8374', '8375', '8376', '8377', '8378', '8379', &
        '8380', '8381', '8382', '8383', '8384', '8385', '8386', '8387', '8388', '8389', &
        '8390', '8391', '8392', '8393', '8394', '8395', '8396', '8397', '8398', '8399', &
        '8400', '8401', '8402', '8403', '8404', '8405', '8406', '8407', '8408', '8409', &
        '8410', '8411', '8412', '8413', '8414', '8415', '8416', '8417', '8418', '8419', &
        '8420', '8421', '8422', '8423', '8424', '8425', '8426', '8427', '8428', '8429', &
        '8430', '8431', '8432', '8433', '8434', '8435', '8436', '8437', '8438', '8439', &
        '8440', '8441', '8442', '8443', '8444', '8445', '8446', '8447', '8448', '8449', &
        '8450', '8451', '8452', '8453', '8454', '8455', '8456', '8457', '8458', '8459', &
        '8460', '8461', '8462', '8463', '8464', '8465', '8466', '8467', '8468', '8469', &
        '8470', '8471', '8472', '8473', '8474', '8475', '8476', '8477', '8478', '8479', &
        '8480', '8481', '8482', '8483', '8484', '8485', '8486', '8487', '8488', '8489', &
        '8490', '8491', '8492', '8493', '8494', '8495', '8496', '8497', '8498', '8499', &
        '8500', '8501', '8502', '8503', '8504', '8505', '8506', '8507', '8508', '8509', &
        '8510', '8511', '8512', '8513', '8514', '8515', '8516', '8517', '8518', '8519', &
        '8520', '8521', '8522', '8523', '8524', '8525', '8526', '8527', '8528', '8529', &
        '8530', '8531', '8532', '8533', '8534', '8535', '8536', '8537', '8538', '8539', &
        '8540', '8541', '8542', '8543', '8544', '8545', '8546', '8547', '8548', '8549', &
        '8550', '8551', '8552', '8553', '8554', '8555', '8556', '8557', '8558', '8559', &
        '8560', '8561', '8562', '8563', '8564', '8565', '8566', '8567', '8568', '8569', &
        '8570', '8571', '8572', '8573', '8574', '8575', '8576', '8577', '8578', '8579', &
        '8580', '8581', '8582', '8583', '8584', '8585', '8586', '8587', '8588', '8589', &
        '8590', '8591', '8592', '8593', '8594', '8595', '8596', '8597', '8598', '8599', &
        '8600', '8601', '8602', '8603', '8604', '8605', '8606', '8607', '8608', '8609', &
        '8610', '8611', '8612', '8613', '8614', '8615', '8616', '8617', '8618', '8619', &
        '8620', '8621', '8622', '8623', '8624', '8625', '8626', '8627', '8628', '8629', &
        '8630', '8631', '8632', '8633', '8634', '8635', '8636', '8637', '8638', '8639', &
        '8640', '8641', '8642', '8643', '8644', '8645', '8646', '8647', '8648', '8649', &
        '8650', '8651', '8652', '8653', '8654', '8655', '8656', '8657', '8658', '8659', &
        '8660', '8661', '8662', '8663', '8664', '8665', '8666', '8667', '8668', '8669', &
        '8670', '8671', '8672', '8673', '8674', '8675', '8676', '8677', '8678', '8679', &
        '8680', '8681', '8682', '8683', '8684', '8685', '8686', '8687', '8688', '8689', &
        '8690', '8691', '8692', '8693', '8694', '8695', '8696', '8697', '8698', '8699', &
        '8700', '8701', '8702', '8703', '8704', '8705', '8706', '8707', '8708', '8709', &
        '8710', '8711', '8712', '8713', '8714', '8715', '8716', '8717', '8718', '8719', &
        '8720', '8721', '8722', '8723', '8724', '8725', '8726', '8727', '8728', '8729', &
        '8730', '8731', '8732', '8733', '8734', '8735', '8736', '8737', '8738', '8739', &
        '8740', '8741', '8742', '8743', '8744', '8745', '8746', '8747', '8748', '8749', &
        '8750', '8751', '8752', '8753', '8754', '8755', '8756', '8757', '8758', '8759', &
        '8760', '8761', '8762', '8763', '8764', '8765', '8766', '8767', '8768', '8769', &
        '8770', '8771', '8772', '8773', '8774', '8775', '8776', '8777', '8778', '8779', &
        '8780', '8781', '8782', '8783', '8784', '8785', '8786', '8787', '8788', '8789', &
        '8790', '8791', '8792', '8793', '8794', '8795', '8796', '8797', '8798', '8799', &
        '8800', '8801', '8802', '8803', '8804', '8805', '8806', '8807', '8808', '8809', &
        '8810', '8811', '8812', '8813', '8814', '8815', '8816', '8817', '8818', '8819', &
        '8820', '8821', '8822', '8823', '8824', '8825', '8826', '8827', '8828', '8829', &
        '8830', '8831', '8832', '8833', '8834', '8835', '8836', '8837', '8838', '8839', &
        '8840', '8841', '8842', '8843', '8844', '8845', '8846', '8847', '8848', '8849', &
        '8850', '8851', '8852', '8853', '8854', '8855', '8856', '8857', '8858', '8859', &
        '8860', '8861', '8862', '8863', '8864', '8865', '8866', '8867', '8868', '8869', &
        '8870', '8871', '8872', '8873', '8874', '8875', '8876', '8877', '8878', '8879', &
        '8880', '8881', '8882', '8883', '8884', '8885', '8886', '8887', '8888', '8889', &
        '8890', '8891', '8892', '8893', '8894', '8895', '8896', '8897', '8898', '8899', &
        '8900', '8901', '8902', '8903', '8904', '8905', '8906', '8907', '8908', '8909', &
        '8910', '8911', '8912', '8913', '8914', '8915', '8916', '8917', '8918', '8919', &
        '8920', '8921', '8922', '8923', '8924', '8925', '8926', '8927', '8928', '8929', &
        '8930', '8931', '8932', '8933', '8934', '8935', '8936', '8937', '8938', '8939', &
        '8940', '8941', '8942', '8943', '8944', '8945', '8946', '8947', '8948', '8949', &
        '8950', '8951', '8952', '8953', '8954', '8955', '8956', '8957', '8958', '8959', &
        '8960', '8961', '8962', '8963', '8964', '8965', '8966', '8967', '8968', '8969', &
        '8970', '8971', '8972', '8973', '8974', '8975', '8976', '8977', '8978', '8979', &
        '8980', '8981', '8982', '8983', '8984', '8985', '8986', '8987', '8988', '8989', &
        '8990', '8991', '8992', '8993', '8994', '8995', '8996', '8997', '8998', '8999', &
        '9000', '9001', '9002', '9003', '9004', '9005', '9006', '9007', '9008', '9009', &
        '9010', '9011', '9012', '9013', '9014', '9015', '9016', '9017', '9018', '9019', &
        '9020', '9021', '9022', '9023', '9024', '9025', '9026', '9027', '9028', '9029', &
        '9030', '9031', '9032', '9033', '9034', '9035', '9036', '9037', '9038', '9039', &
        '9040', '9041', '9042', '9043', '9044', '9045', '9046', '9047', '9048', '9049', &
        '9050', '9051', '9052', '9053', '9054', '9055', '9056', '9057', '9058', '9059', &
        '9060', '9061', '9062', '9063', '9064', '9065', '9066', '9067', '9068', '9069', &
        '9070', '9071', '9072', '9073', '9074', '9075', '9076', '9077', '9078', '9079', &
        '9080', '9081', '9082', '9083', '9084', '9085', '9086', '9087', '9088', '9089', &
        '9090', '9091', '9092', '9093', '9094', '9095', '9096', '9097', '9098', '9099', &
        '9100', '9101', '9102', '9103', '9104', '9105', '9106', '9107', '9108', '9109', &
        '9110', '9111', '9112', '9113', '9114', '9115', '9116', '9117', '9118', '9119', &
        '9120', '9121', '9122', '9123', '9124', '9125', '9126', '9127', '9128', '9129', &
        '9130', '9131', '9132', '9133', '9134', '9135', '9136', '9137', '9138', '9139', &
        '9140', '9141', '9142', '9143', '9144', '9145', '9146', '9147', '9148', '9149', &
        '9150', '9151', '9152', '9153', '9154', '9155', '9156', '9157', '9158', '9159', &
        '9160', '9161', '9162', '9163', '9164', '9165', '9166', '9167', '9168', '9169', &
        '9170', '9171', '9172', '9173', '9174', '9175', '9176', '9177', '9178', '9179', &
        '9180', '9181', '9182', '9183', '9184', '9185', '9186', '9187', '9188', '9189', &
        '9190', '9191', '9192', '9193', '9194', '9195', '9196', '9197', '9198', '9199', &
        '9200', '9201', '9202', '9203', '9204', '9205', '9206', '9207', '9208', '9209', &
        '9210', '9211', '9212', '9213', '9214', '9215', '9216', '9217', '9218', '9219', &
        '9220', '9221', '9222', '9223', '9224', '9225', '9226', '9227', '9228', '9229', &
        '9230', '9231', '9232', '9233', '9234', '9235', '9236', '9237', '9238', '9239', &
        '9240', '9241', '9242', '9243', '9244', '9245', '9246', '9247', '9248', '9249', &
        '9250', '9251', '9252', '9253', '9254', '9255', '9256', '9257', '9258', '9259', &
        '9260', '9261', '9262', '9263', '9264', '9265', '9266', '9267', '9268', '9269', &
        '9270', '9271', '9272', '9273', '9274', '9275', '9276', '9277', '9278', '9279', &
        '9280', '9281', '9282', '9283', '9284', '9285', '9286', '9287', '9288', '9289', &
        '9290', '9291', '9292', '9293', '9294', '9295', '9296', '9297', '9298', '9299', &
        '9300', '9301', '9302', '9303', '9304', '9305', '9306', '9307', '9308', '9309', &
        '9310', '9311', '9312', '9313', '9314', '9315', '9316', '9317', '9318', '9319', &
        '9320', '9321', '9322', '9323', '9324', '9325', '9326', '9327', '9328', '9329', &
        '9330', '9331', '9332', '9333', '9334', '9335', '9336', '9337', '9338', '9339', &
        '9340', '9341', '9342', '9343', '9344', '9345', '9346', '9347', '9348', '9349', &
        '9350', '9351', '9352', '9353', '9354', '9355', '9356', '9357', '9358', '9359', &
        '9360', '9361', '9362', '9363', '9364', '9365', '9366', '9367', '9368', '9369', &
        '9370', '9371', '9372', '9373', '9374', '9375', '9376', '9377', '9378', '9379', &
        '9380', '9381', '9382', '9383', '9384', '9385', '9386', '9387', '9388', '9389', &
        '9390', '9391', '9392', '9393', '9394', '9395', '9396', '9397', '9398', '9399', &
        '9400', '9401', '9402', '9403', '9404', '9405', '9406', '9407', '9408', '9409', &
        '9410', '9411', '9412', '9413', '9414', '9415', '9416', '9417', '9418', '9419', &
        '9420', '9421', '9422', '9423', '9424', '9425', '9426', '9427', '9428', '9429', &
        '9430', '9431', '9432', '9433', '9434', '9435', '9436', '9437', '9438', '9439', &
        '9440', '9441', '9442', '9443', '9444', '9445', '9446', '9447', '9448', '9449', &
        '9450', '9451', '9452', '9453', '9454', '9455', '9456', '9457', '9458', '9459', &
        '9460', '9461', '9462', '9463', '9464', '9465', '9466', '9467', '9468', '9469', &
        '9470', '9471', '9472', '9473', '9474', '9475', '9476', '9477', '9478', '9479', &
        '9480', '9481', '9482', '9483', '9484', '9485', '9486', '9487', '9488', '9489', &
        '9490', '9491', '9492', '9493', '9494', '9495', '9496', '9497', '9498', '9499', &
        '9500', '9501', '9502', '9503', '9504', '9505', '9506', '9507', '9508', '9509', &
        '9510', '9511', '9512', '9513', '9514', '9515', '9516', '9517', '9518', '9519', &
        '9520', '9521', '9522', '9523', '9524', '9525', '9526', '9527', '9528', '9529', &
        '9530', '9531', '9532', '9533', '9534', '9535', '9536', '9537', '9538', '9539', &
        '9540', '9541', '9542', '9543', '9544', '9545', '9546', '9547', '9548', '9549', &
        '9550', '9551', '9552', '9553', '9554', '9555', '9556', '9557', '9558', '9559', &
        '9560', '9561', '9562', '9563', '9564', '9565', '9566', '9567', '9568', '9569', &
        '9570', '9571', '9572', '9573', '9574', '9575', '9576', '9577', '9578', '9579', &
        '9580', '9581', '9582', '9583', '9584', '9585', '9586', '9587', '9588', '9589', &
        '9590', '9591', '9592', '9593', '9594', '9595', '9596', '9597', '9598', '9599', &
        '9600', '9601', '9602', '9603', '9604', '9605', '9606', '9607', '9608', '9609', &
        '9610', '9611', '9612', '9613', '9614', '9615', '9616', '9617', '9618', '9619', &
        '9620', '9621', '9622', '9623', '9624', '9625', '9626', '9627', '9628', '9629', &
        '9630', '9631', '9632', '9633', '9634', '9635', '9636', '9637', '9638', '9639', &
        '9640', '9641', '9642', '9643', '9644', '9645', '9646', '9647', '9648', '9649', &
        '9650', '9651', '9652', '9653', '9654', '9655', '9656', '9657', '9658', '9659', &
        '9660', '9661', '9662', '9663', '9664', '9665', '9666', '9667', '9668', '9669', &
        '9670', '9671', '9672', '9673', '9674', '9675', '9676', '9677', '9678', '9679', &
        '9680', '9681', '9682', '9683', '9684', '9685', '9686', '9687', '9688', '9689', &
        '9690', '9691', '9692', '9693', '9694', '9695', '9696', '9697', '9698', '9699', &
        '9700', '9701', '9702', '9703', '9704', '9705', '9706', '9707', '9708', '9709', &
        '9710', '9711', '9712', '9713', '9714', '9715', '9716', '9717', '9718', '9719', &
        '9720', '9721', '9722', '9723', '9724', '9725', '9726', '9727', '9728', '9729', &
        '9730', '9731', '9732', '9733', '9734', '9735', '9736', '9737', '9738', '9739', &
        '9740', '9741', '9742', '9743', '9744', '9745', '9746', '9747', '9748', '9749', &
        '9750', '9751', '9752', '9753', '9754', '9755', '9756', '9757', '9758', '9759', &
        '9760', '9761', '9762', '9763', '9764', '9765', '9766', '9767', '9768', '9769', &
        '9770', '9771', '9772', '9773', '9774', '9775', '9776', '9777', '9778', '9779', &
        '9780', '9781', '9782', '9783', '9784', '9785', '9786', '9787', '9788', '9789', &
        '9790', '9791', '9792', '9793', '9794', '9795', '9796', '9797', '9798', '9799', &
        '9800', '9801', '9802', '9803', '9804', '9805', '9806', '9807', '9808', '9809', &
        '9810', '9811', '9812', '9813', '9814', '9815', '9816', '9817', '9818', '9819', &
        '9820', '9821', '9822', '9823', '9824', '9825', '9826', '9827', '9828', '9829', &
        '9830', '9831', '9832', '9833', '9834', '9835', '9836', '9837', '9838', '9839', &
        '9840', '9841', '9842', '9843', '9844', '9845', '9846', '9847', '9848', '9849', &
        '9850', '9851', '9852', '9853', '9854', '9855', '9856', '9857', '9858', '9859', &
        '9860', '9861', '9862', '9863', '9864', '9865', '9866', '9867', '9868', '9869', &
        '9870', '9871', '9872', '9873', '9874', '9875', '9876', '9877', '9878', '9879', &
        '9880', '9881', '9882', '9883', '9884', '9885', '9886', '9887', '9888', '9889', &
        '9890', '9891', '9892', '9893', '9894', '9895', '9896', '9897', '9898', '9899', &
        '9900', '9901', '9902', '9903', '9904', '9905', '9906', '9907', '9908', '9909', &
        '9910', '9911', '9912', '9913', '9914', '9915', '9916', '9917', '9918', '9919', &
        '9920', '9921', '9922', '9923', '9924', '9925', '9926', '9927', '9928', '9929', &
        '9930', '9931', '9932', '9933', '9934', '9935', '9936', '9937', '9938', '9939', &
        '9940', '9941', '9942', '9943', '9944', '9945', '9946', '9947', '9948', '9949', &
        '9950', '9951', '9952', '9953', '9954', '9955', '9956', '9957', '9958', '9959', &
        '9960', '9961', '9962', '9963', '9964', '9965', '9966', '9967', '9968', '9969', &
        '9970', '9971', '9972', '9973', '9974', '9975', '9976', '9977', '9978', '9979', &
        '9980', '9981', '9982', '9983', '9984', '9985', '9986', '9987', '9988', '9989', &
        '9990', '9991', '9992', '9993', '9994', '9995', '9996', '9997', '9998', '9999']

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)       :: BufLen, Top, I, J
    CHARACTER(LEN=41)       :: Buffer
    TYPE(UInt128)           :: Copy
    INTEGER(KIND=I8B)       :: Tmp

!** FLOW

    IF (U128 == ZeroU128) THEN
        Str = '0'
        RETURN
    END IF

    BufLen = 41
    FORALL (I=1:BufLen) Buffer(I:I) = '0'
    Top  = BufLen
    Copy = U128
    DO
        J = Top
        Tmp = DivModBy10Pow18(Copy)
        CALL Write_1_To_18_Digits(Tmp, Buffer, Top)
        IF ((Copy%High == 0).AND.(Copy%Low == 0)) THEN
            EXIT
        ELSE
            Top = J - 18
        END IF
    END DO
    Str = Buffer(Top+1:BufLen)

    RETURN

    CONTAINS

    SUBROUTINE Write_1_To_18_Digits(Number, cStr, Indx)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an integer number to character string

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B),   INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B),   INTENT(INOUT)    :: Indx     ! current index to the character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I4B), PARAMETER  :: MaxLen = 19
        INTEGER(KIND=I8B), PARAMETER  :: Div1E8 = 100000000_I8B
        ! multiplier and shift for 19 digits and divisor of 1.0E8
        INTEGER(KIND=I8B), PARAMETER  :: M90 = INT(Z'ABCC77118461CEFD', KIND=I8B)
        INTEGER(KIND=I4B), PARAMETER  :: S90 = 90 - 64
        ! multiplier for 11 digits and divisor of 1.0E8
        INTEGER(KIND=I8B), PARAMETER  :: M64 = INT(Z'0000002AF31DC462', KIND=I8B)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CHARACTER(LEN=MaxLen)   :: wStr
        INTEGER(KIND=I8B)       :: PosNum
        INTEGER(KIND=I8B)       :: NxtNum, RemNum
        INTEGER(KIND=I4B)       :: Start

    !** FLOW

        ! start the conversion
        IF (Number >= 1000000000_I8B) THEN
            ! compute NxtNum = Number/100000000
            NxtNum = SHIFTR(UMul128_Upper64(Number, M90), S90)
            ! compute RemNum = MOD(Number, 100000000)
            RemNum = Number - NxtNum*Div1E8
            ! convert the remainder to a working string
            CALL Write_8_Digits(INT(RemNum, KIND=I4B), wStr(12:19))

            PosNum = NxtNum
            IF (PosNum > Div1E8) THEN
                ! compute NxtNum = PosNum/100000000
                NxtNum = UMul128_Upper64(PosNum, M64)
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
            DO I = MaxLen, Start, -1
                cStr(Indx:Indx) = wStr(I:I)
                Indx = Indx - 1
            END DO
        ELSE
            CALL Write_1_To_9_Digits(INT(Number, KIND=I4B), cStr, Indx)
        END IF

        RETURN

    END SUBROUTINE Write_1_To_18_Digits

    !******************************************************************************

    SUBROUTINE Write_1_To_9_Digits(Number, cStr, Indx)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_1_To_9_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an integer number to character string

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),   INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr     ! character string
        INTEGER(KIND=I4B),   INTENT(INOUT)    :: Indx     ! current index to the character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I4B), PARAMETER :: MaxLen = 10
        INTEGER(KIND=I4B), PARAMETER :: ShiftPos = 45
        INTEGER(KIND=I8B), PARAMETER :: Multiplier = INT(Z'00000000D1B71759', KIND=I8B)
        INTEGER(KIND=I4B), PARAMETER :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CHARACTER(LEN=MaxLen)   :: wStr     ! working string
        INTEGER(KIND=I4B)       :: PosNum   ! positive number (working number)
        INTEGER(KIND=I4B)       :: NxtNum   ! next round of positive number
        INTEGER(KIND=I4B)       :: RemNum   ! remainder number
        INTEGER(KIND=I4B)       :: Start, Finish

    !** FLOW

        ! start the conversion
        IF (Number >= 10000) THEN
            ! compute the next round of working number
            NxtNum = INT(SHIFTR(Number*Multiplier, ShiftPos), KIND=I4B)     ! NxtNum = Number/10000
            ! compute the remainder
            RemNum = Number - NxtNum*Divisor                                ! RemNum = MOD(Number, 10000)
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
        ELSE
            Start  = 1
            IF (Number < 100) THEN
                wStr(1:2) = Char2Digits(Number)
                Finish = 2
                IF (wStr(Start:Start) == '0') Start = 2
            ELSE
                wStr(1:4) = Char4Digits(Number)
                Finish = 4
                IF (wStr(Start:Start) == '0') Start = 2
            END IF
        END IF

        ! transfer to output string
        DO I = Finish, Start, -1
            cStr(Indx:Indx) = wStr(I:I)
            Indx = Indx - 1
        END DO

        RETURN

    END SUBROUTINE Write_1_To_9_Digits

    !******************************************************************************

    SUBROUTINE Write_8_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I4B),   INTENT(IN)       :: Number   ! number
        CHARACTER(LEN=*),    INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! multiplier and shift for 4-byte integer and divisor of 10000
        INTEGER(KIND=I4B), PARAMETER  :: ShiftPos = 45
        INTEGER(KIND=I8B), PARAMETER  :: Multiplier = INT(Z'00000000D1B71759', KIND=I8B)
        INTEGER(KIND=I4B), PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: NxtNum, RemNum

    !** FLOW

        ! compute NxtNum = PosNum/10000
        NxtNum = INT(SHIFTR(Number*Multiplier, ShiftPos), KIND=I4B)
        ! compute RemNum = MOD(PosNum, 10000)
        RemNum = Number - NxtNum*Divisor
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
        INTEGER(KIND=I4B)     :: NxtNum, RemNum

    !** FLOW

        IF (Number < 10000) THEN
            IF (Number < 100) THEN
                cStr(7:8) = Char2Digits(Number)
                Start = 7
                IF (cStr(Start:Start) == '0') Start = 8
            ELSE
                cStr(5:8) = Char4Digits(Number)
                Start = 5
                IF (cStr(Start:Start) == '0') Start = 6
            END IF
        ELSE
            ! compute NxtNum = Number/10000
            NxtNum = INT(SHIFTR(Number*Multiplier, ShiftPos), KIND=I4B)
            ! compute RemNum = MOD(Number, 10000)
            RemNum = Number - NxtNum*Divisor
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

    FUNCTION DivModBy10Pow18(DivQuot) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division DivQuot / Divisor where the Divisor is equal to 10**18

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(UInt128), INTENT(INOUT)  :: DivQuot    ! on entry, the dividend
                                                        ! on exit, the quotient
        INTEGER(KIND=I8B)              :: Remainder	! the remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B), PARAMETER  :: LSh   = 4                                  ! = LEADZ(Divisor)
        INTEGER(KIND=I8B), PARAMETER  :: Denom = INT(Z'DE0B6B3A76400000', KIND=I8B) ! = SHIFTL(Divisor, LSh)
        INTEGER(KIND=I8B), PARAMETER  :: V     = INT(Z'2725DD1D243ABA0E', KIND=I8B) ! = Reciprocal_2By1(Denom)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)         :: RSh
        INTEGER(KIND=I8B)         :: NumerHi, NumerLo
!        INTEGER(KIND=I8B)         :: DenomHi, DenomLo
        INTEGER(KIND=I8B)         :: NumerEx, RshMask
        INTEGER(KIND=I8B)         :: R1, R2
!        INTEGER(KIND=I8B)         :: LHS, RHS

    !** FLOW

        IF (DivQuot%High == 0_I8B) THEN
            IF ((DivQuot%Low > 0_I8B).AND.(DivQuot%Low < 1000000000000000000_I8B)) THEN
                Remainder   = DivQuot%Low
                DivQuot%Low = 0_I8B
                RETURN
            END IF
        END IF
        RSh = 64 - LSh
        RShMask = -1_I8B
        NumerLo = SHIFTL(DivQuot%Low, LSh)
        NumerHi = IOR(SHIFTL(DivQuot%High, LSh), IAND(SHIFTR(DivQuot%Low, RSh), RShMask))
        NumerEx = IAND(SHIFTR(DivQuot%High, RSh), RShMask)

        CALL UDivRem_2By1(NumerEx, NumerHi, Denom, V, DivQuot%High, R1)
        CALL UDivRem_2By1(R1, NumerLo, Denom, V, DivQuot%Low, R2)
        Remainder = SHIFTR(R2, LSh)

        RETURN

    END FUNCTION DivModBy10Pow18

    !**************************************************************************

    SUBROUTINE UDivRem_2By1(UHi, ULo, D, V, Q, R)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To performm 128-bit integer division by 64-bit integer

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)     :: UHi, ULo, D, V
        INTEGER(KIND=I8B), INTENT(OUT)    :: Q, R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER  :: MinInt64 = INT(Z'8000000000000000', KIND=I8B)   ! -9223372036854775808

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: QHi, QLo, NewLo

    !** FLOW

        ! Q128 = V*UHi
        CALL UMul128(V, UHi, QHi, QLo)

        ! Q128 = Q128 + U128
        NewLo = QLo + ULo
        IF (IEOR(NewLo, MinInt64) < IEOR(QLo, MinInt64)) THEN
            QHi = QHi + UHi + 1_I8B
        ELSE
            QHi = QHi + UHi
        END IF
        QLo = NewLo

        QHi = QHi + 1_I8B

        R = ULo - QHi*D

        IF (IEOR(R, MinInt64) > IEOR(QLo, MinInt64)) THEN
            QHi = QHi - 1_I8B
            R = R + D
        END IF

        IF (IEOR(R, MinInt64) >= IEOR(D, MinInt64)) THEN
            QHi = QHi + 1_I8B
            R = R - D
        END IF
        Q = QHi

        RETURN

    END SUBROUTINE UDivRem_2By1

    !**************************************************************************

END FUNCTION DecString_From_U128

!******************************************************************************

FUNCTION HexString_From_U128(U128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 128-bit integer number to a hexadecimal string.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)       :: U128
    CHARACTER(LEN=:), ALLOCATABLE   :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    !  na

!** FLOW

    ! for hexadecimal, division and mod of UInt128 is not needed so it is simpler and faster
    IF (U128%High /= 0_I8B) THEN
        BLOCK
            CHARACTER(LEN=:), ALLOCATABLE      :: LowHex
            CHARACTER(LEN=16)    :: LowStr
            LowStr = '0000000000000000'
            LowHex = U64_ToHexStr(U128%Low)
            IF (LEN(LowHex) > 0) LowStr(17-LEN(LowHex):16) = LowHex
            Str = U64_ToHexStr(U128%High) // LowStr
        END BLOCK
    ELSE
        Str = U64_ToHexStr(U128%Low)
    END IF

    RETURN

CONTAINS

    FUNCTION I64_ToHexStr(Number) RESULT(cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an integer number to a hexadecimal string

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: Number   ! number
        CHARACTER(LEN=:), ALLOCATABLE   :: cStr     ! hexadecimal string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I4B),  PARAMETER  :: MaxLen = 16
        INTEGER(KIND=I8B),  PARAMETER  :: Base   = 16_I8B
        INTEGER(KIND=I4B),  PARAMETER  :: Shift  = 4
        CHARACTER(LEN=1),   PARAMETER  :: NumStr(0:15) = ['0', '1', '2', '3', '4', '5', '6', '7', &
                                                          '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        CHARACTER(LEN=MaxLen)   :: wStr     ! working string
        INTEGER(KIND=I8B)       :: PosNum   ! positive number (working number)
        INTEGER(KIND=I8B)       :: CurNum   ! current (saved) working number
        INTEGER(KIND=I8B)       :: RemNum   ! remainder number
        INTEGER(KIND=I4B)       :: Indx

    !** FLOW

        ! check whether the number is zero
        IF (Number == 0_I8B) THEN
            cStr = '0'
            RETURN
        END IF
        IF (Number < 0_I8B) THEN
            IF (Number == MinI64) THEN
                cStr = '-8000000000000000'
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
            PosNum = SHIFTR(PosNum, Shift)
            ! compute the remainder
            RemNum = CurNum - SHIFTL(PosNum, Shift)
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

    END FUNCTION I64_ToHexStr

!******************************************************************************

    FUNCTION U64_ToHexStr(Number) RESULT(cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned integer number to a hexadecimal string

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)   :: Number   ! number treated as unsigned one
        CHARACTER(LEN=:), ALLOCATABLE   :: cStr     ! hexadecimal string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I4B), PARAMETER    :: Shift = 4

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)               :: Quotient, Remainder
        CHARACTER(LEN=:), ALLOCATABLE   :: QuotStr,  RemStr

    !** FLOW

        IF (Number >= 0_I8B) THEN
            cStr = I64_ToHexStr(Number)
        ELSE
            Quotient  = SHIFTR(Number, Shift)
            Remainder = Number - SHIFTL(Quotient, Shift)
            QuotStr = I64_ToHexStr(Quotient)
            RemStr  = I64_ToHexStr(Remainder)
            cStr    = QuotStr // RemStr
        END IF

        RETURN

    END FUNCTION U64_ToHexStr

!******************************************************************************

END FUNCTION HexString_From_U128

!------------------------------------------------------------------------------
!
!                           comparison ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION U128_Equal(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two Uint128 objects are equal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: LHS
    TYPE(UInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (LHS%High == RHS%High).AND.(LHS%Low == RHS%Low)

    RETURN

END FUNCTION U128_Equal

!******************************************************************************

FUNCTION U128_NotEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether two Uint128 objects are NOT equal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: LHS
    TYPE(UInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (LHS%High /= RHS%High).OR.(LHS%Low /= RHS%Low)

    RETURN

END FUNCTION U128_NotEqual

!******************************************************************************

FUNCTION U128_LessThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the LHS Uint128 object is less than the RHS Uint128 object.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: LHS
    TYPE(UInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) < IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (IEOR(LHS%High, MinI64) < IEOR(RHS%High, MinI64))
    END IF

    RETURN

END FUNCTION U128_LessThan

!******************************************************************************

FUNCTION U128_LessEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the LHS Uint128 object is less than or equal to the RHS Uint128 object.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: LHS
    TYPE(UInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) <= IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (IEOR(LHS%High, MinI64) <= IEOR(RHS%High, MinI64))
    END IF

    RETURN

END FUNCTION U128_LessEqual

!******************************************************************************

FUNCTION U128_GreaterThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the LHS Uint128 object is greater than the RHS Uint128 object.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: LHS
    TYPE(UInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) > IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (IEOR(LHS%High, MinI64) > IEOR(RHS%High, MinI64))
    END IF

    RETURN

END FUNCTION U128_GreaterThan

!******************************************************************************

FUNCTION U128_GreaterEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the LHS Uint128 object is greater than or equal to the RHS Uint128 object.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: LHS
    TYPE(UInt128), INTENT(IN)   :: RHS
    LOGICAL                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) >= IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (IEOR(LHS%High, MinI64) >= IEOR(RHS%High, MinI64))
    END IF

    RETURN

END FUNCTION U128_GreaterEqual

!******************************************************************************

FUNCTION U128_Compare(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS objects.  
    !   -> return -1 if LHS < RHS  
    !   -> return  0 if LHS == RHS  
    !   -> return +1 if LHS > RHS

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: LHS
    TYPE(UInt128), INTENT(IN)   :: RHS
    INTEGER(KIND=I4B)           :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)   :: ULHS, URHS

!** FLOW

    ULHS = IEOR(LHS%High, MinI64)
    URHS = IEOR(RHS%High, MinI64)
    IF (ULHS < URHS) THEN
        Flag = -1
    ELSEIF (ULHS > URHS) THEN
        Flag = +1
    ELSE
        ULHS = IEOR(LHS%Low, MinI64)
        URHS = IEOR(RHS%Low, MinI64)
        IF (ULHS < URHS) THEN
            Flag = -1
        ELSEIF (ULHS > URHS) THEN
            Flag = +1
        ELSE
            Flag = 0
        END IF
    END IF

    RETURN

END FUNCTION U128_Compare

!------------------------------------------------------------------------------
!
!                           BITWISE ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION U128_ShiftLeftOnce(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical left shift by 1.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: InVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IOR(SHIFTL(InVal%High, 1), SHIFTR(InVal%Low, 63))
    OutVal%Low  = SHIFTL(InVal%Low, 1)

    RETURN

END FUNCTION U128_ShiftLeftOnce

!******************************************************************************

FUNCTION U128_ShiftRightOnce(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical right shift by 1.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: InVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTR(InVal%High, 1)
    OutVal%Low  = IOR(SHIFTR(InVal%Low, 1), SHIFTL(InVal%High, 63))

    RETURN

END FUNCTION U128_ShiftRightOnce

!******************************************************************************

FUNCTION U128_ShiftLeft64(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical left shift by 64.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: InVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = InVal%Low
    OutVal%Low  = 0_I8B

    RETURN

END FUNCTION U128_ShiftLeft64

!******************************************************************************

FUNCTION U128_ShiftRight64(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical right shift by 64

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: InVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = 0_I8B
    OutVal%Low  = InVal%High

    RETURN

END FUNCTION U128_ShiftRight64

!******************************************************************************

FUNCTION U128_ShiftLeft63Down(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical left shift by 63 or less.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos     !! 0 <= ShiftPos < 64
    TYPE(UInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IOR(SHIFTL(InVal%High, ShiftPos), SHIFTR(InVal%Low, 64 - ShiftPos))
    OutVal%Low  = SHIFTL(InVal%Low, ShiftPos)

    RETURN

END FUNCTION U128_ShiftLeft63Down

!******************************************************************************

FUNCTION U128_ShiftRight63Down(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical right shift by 63 or less.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos     !! 0 <= ShiftPos < 64
    TYPE(UInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTR(InVal%High, ShiftPos)
    OutVal%Low  = IOR(SHIFTR(InVal%Low, ShiftPos), SHIFTL(InVal%High, 64 - ShiftPos))

    RETURN

END FUNCTION U128_ShiftRight63Down

!******************************************************************************

FUNCTION U128_ShiftLeft64Up(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical left shift by 64 or more.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos     !! 64 < ShiftPos <= 128
    TYPE(UInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTL(InVal%Low, ShiftPos - 64)
    OutVal%Low  = 0_I8B

    RETURN

END FUNCTION U128_ShiftLeft64Up

!******************************************************************************

FUNCTION U128_ShiftRight64Up(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical right shift by 64 or more.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos     !! 64 < ShiftPos <= 128
    TYPE(UInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = 0_I8B
    OutVal%Low  = SHIFTR(InVal%High, ShiftPos - 64)

    RETURN

END FUNCTION U128_ShiftRight64Up

!******************************************************************************

FUNCTION U128_ShiftLogical(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical (left or right) shift of the UInt128 object.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos !! -128 <= ShiftPos <= 128;  
                                                !! -> positive, the shift is to the left;  
                                                !! -> negative, the shift is to the right
    TYPE(UInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ShiftPos < 0) THEN
        OutVal = SHIFTR(InVal, -ShiftPos)
    ELSE
        OutVal = SHIFTL(InVal, ShiftPos)
    END IF

    RETURN

END FUNCTION U128_ShiftLogical

!******************************************************************************

FUNCTION U128_ShiftLeft(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical left shift of the UInt128 object.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos     !! 0 <= ShiftPos <= 128
    TYPE(UInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ShiftPos < 0) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_ShiftLeft'//' in Module '//ModName//'.')
        CALL DisplayContinueError('ShiftPos must be nonnegative number')
    ELSEIF (ShiftPos == 0) THEN
        OutVal = InVal
    ELSEIF (ShiftPos >= 128) THEN
        OutVal = ZeroU128
    ELSEIF (ShiftPos >= 64) THEN
        OutVal%High = SHIFTL(InVal%Low, ShiftPos - 64)
        OutVal%Low  = 0_I8B
    ELSE
        OutVal%High = IOR(SHIFTL(InVal%High, ShiftPos), SHIFTR(InVal%Low, 64 - ShiftPos))
        OutVal%Low  = SHIFTL(InVal%Low, ShiftPos)
    END IF

    RETURN

END FUNCTION U128_ShiftLeft

!******************************************************************************

FUNCTION U128_ShiftRight(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform logical right shift of the UInt128 object.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos     !! 0 <= ShiftPos <= 128
    TYPE(UInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ShiftPos < 0) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_ShiftRight'//' in Module '//ModName//'.')
        CALL DisplayContinueError('ShiftPos must be nonnegative number')
    ELSEIF (ShiftPos == 0) THEN
        OutVal = InVal
    ELSEIF (ShiftPos >= 128) THEN
        OutVal = ZeroU128
    ELSEIF (ShiftPos >= 64) THEN
        OutVal%High = 0_I8B
        OutVal%Low  = SHIFTR(InVal%High, ShiftPos - 64)
    ELSE
        OutVal%High = SHIFTR(InVal%High, ShiftPos)
        OutVal%Low  = IOR(SHIFTR(InVal%Low, ShiftPos), SHIFTL(InVal%High, 64 - ShiftPos))
    END IF

    RETURN

END FUNCTION U128_ShiftRight

!******************************************************************************

FUNCTION U128_Rotate(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform a circular shift of the rightmost bits.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)  :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)  :: ShiftPos !! -128 <= ShiftPos <= 128;  
                                                !! -> positive, the shift is to the left;  
                                                !! -> negative, the shift is to the right
    TYPE(UInt128)                   :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)    :: LeftShift

!** FLOW

    IF (ShiftPos == 0) THEN
        OutVal = InVal
        RETURN
    ELSEIF (ABS(ShiftPos) == 128) THEN
        OutVal = ZeroU128
        RETURN
    ELSEIF (ABS(ShiftPos) > 128) THEN
        LeftShift = MOD(ShiftPos, 128)
    ELSE
        LeftShift = ShiftPos
    END IF
    IF (LeftShift < 0) LeftShift = 128 + LeftShift
    OutVal = IOR(SHIFTL(InVal, LeftShift), SHIFTR(InVal, 128 - LeftShift))

    RETURN

END FUNCTION U128_Rotate

!******************************************************************************

FUNCTION U128_Not(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To return the bitwise logical complement of the UInt128 object.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: InVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = NOT(InVal%High)
    OutVal%Low  = NOT(InVal%Low)

    RETURN

END FUNCTION U128_Not

!******************************************************************************

FUNCTION U128_Ior(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform an inclusive OR on corresponding bits of the UInt128 objects.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: LhsVal
    TYPE(UInt128), INTENT(IN)    :: RhsVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IOR(LhsVal%High, RhsVal%High)
    OutVal%Low  = IOR(LhsVal%Low, RhsVal%Low)

    RETURN

END FUNCTION U128_Ior

!******************************************************************************

FUNCTION U128_Iand(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform a logical AND on corresponding bits of the UInt128 objects.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: LhsVal
    TYPE(UInt128), INTENT(IN)    :: RhsVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IAND(LhsVal%High, RhsVal%High)
    OutVal%Low  = IAND(LhsVal%Low, RhsVal%Low)

    RETURN

END FUNCTION U128_Iand

!******************************************************************************

FUNCTION U128_Ieor(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform an exclusive OR on corresponding bits of the UInt128 objects.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: LhsVal
    TYPE(UInt128), INTENT(IN)    :: RhsVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IEOR(LhsVal%High, RhsVal%High)
    OutVal%Low  = IEOR(LhsVal%Low, RhsVal%Low)

    RETURN

END FUNCTION U128_Ieor

!******************************************************************************

FUNCTION U128_LeadingZeros(U128) RESULT(NumLZ)

!** PURPOSE OF THIS SUBROUTINE:
    !! To count the number of leading zero bits.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: U128
    INTEGER(KIND=I4B)            :: NumLZ

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (U128%High == 0_I8B) THEN
        NumLZ = LEADZ(U128%Low) + 64
    ELSE
        NumLZ = LEADZ(U128%High)
    END IF

    RETURN

END FUNCTION U128_LeadingZeros

!******************************************************************************

FUNCTION U128_TrailingZeros(U128) RESULT(NumTZ)

!** PURPOSE OF THIS SUBROUTINE:
    !! To count the number of trailing zero bits.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: U128
    INTEGER(KIND=I4B)            :: NumTZ

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (U128%Low == 0_I8B) THEN
        NumTZ = TRAILZ(U128%High) + 64
    ELSE
        NumTZ = TRAILZ(U128%Low)
    END IF

    RETURN

END FUNCTION U128_TrailingZeros

!******************************************************************************

FUNCTION U128_Count1Bits(U128) RESULT(NumBits)

!** PURPOSE OF THIS SUBROUTINE:
    !! To count the number of 1 bits in the specified input.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: U128
    INTEGER(KIND=I4B)            :: NumBits

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    NumBits = POPCNT(U128%Low) + POPCNT(U128%High)

    RETURN

END FUNCTION U128_Count1Bits

!******************************************************************************

FUNCTION U128_Parity(U128) RESULT(ParNum)

!** PURPOSE OF THIS SUBROUTINE:
    !! To determine the parity of the specified input.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: U128
    INTEGER(KIND=I4B)            :: ParNum

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! ParNum = IAND(POPCNT(U128), 1)
    ParNum = POPPAR(U128%Low) + POPPAR(U128%High)
    IF (ParNum == 2) ParNum = 0

    RETURN

END FUNCTION U128_Parity

!******************************************************************************

FUNCTION U128_SetBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the bit at the specified position to 1.  
    !   For more detail, see explanation of elemental intrinsic function 'IBSET'.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: Pos
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Pos < 0).OR.(Pos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_SetBit'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos must be between 0 and 127.')
        OutVal = ZeroU128
        RETURN
    END IF

    IF (Pos < 64) THEN
        OutVal%Low  = IBSET(InVal%Low, Pos)
        OutVal%High = InVal%High
    ELSE
        OutVal%Low  = InVal%Low
        OutVal%High = IBSET(InVal%High, Pos-64)
    END IF

    RETURN

END FUNCTION U128_SetBit

!******************************************************************************

FUNCTION U128_ClearBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the bit at the specified position to 0.  
    !   For more detail, see explanation of elemental intrinsic function 'IBCLR'.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: Pos
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Pos < 0).OR.(Pos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_ClearBit'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos must be between 0 and 127.')
        OutVal = ZeroU128
        RETURN
    END IF

    IF (Pos < 64) THEN
        OutVal%Low  = IBCLR(InVal%Low, Pos)
        OutVal%High = InVal%High
    ELSE
        OutVal%Low  = InVal%Low
        OutVal%High = IBCLR(InVal%High, Pos-64)
    END IF

    RETURN

END FUNCTION U128_ClearBit

!******************************************************************************

FUNCTION U128_FlipBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reverse the bit at the specified position.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: Pos
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: HiPos

!** FLOW

    IF ((Pos < 0).OR.(Pos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_FlipBit'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos must be between 0 and 127.')
        OutVal = ZeroU128
        RETURN
    END IF

    IF (Pos < 64) THEN
        IF (BTEST(InVal%Low, Pos)) THEN
            ! clear bit
            OutVal%Low = IBCLR(InVal%Low, Pos)
        ELSE
            ! set bit
            OutVal%Low = IBSET(InVal%Low, Pos)
        END IF
        OutVal%High = InVal%High
    ELSE
        HiPos = Pos-64
        IF (BTEST(InVal%High, HiPos)) THEN
            ! clear bit
            OutVal%High = IBCLR(InVal%High, HiPos)
        ELSE
            ! set bit
            OutVal%High = IBSET(InVal%High, HiPos)
        END IF
        OutVal%Low = InVal%Low
    END IF

    RETURN

END FUNCTION U128_FlipBit

!******************************************************************************

FUNCTION U128_TestBit(U128, Pos) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the bit at the specified position is 0 (False) or 1 (True).  
    !   For more detail, see explanation of elemental intrinsic function 'BTEST'.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: U128
    INTEGER(KIND=I4B),  INTENT(IN)    :: Pos
    LOGICAL                           :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Pos < 0).OR.(Pos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_TestBit'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos must be between 0 and 127.')
        Flag = .FALSE.
        RETURN
    END IF

    IF (Pos < 64) THEN
        Flag = BTEST(U128%Low, Pos)
    ELSE
        Flag = BTEST(U128%High, Pos-64)
    END IF

    RETURN

END FUNCTION U128_TestBit

!******************************************************************************

FUNCTION U128_ExtractBits(InVal, Pos, Len) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To extract a sequence of bits according to the specified input.  
    !   For more detail, see explanation of elemental intrinsic function 'IBITS'.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: Pos
    INTEGER(KIND=I4B),  INTENT(IN)    :: Len
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: Len1, Len2, Len3

!** FLOW

! first, check input validity
    IF (Len < 0) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_ExtractBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Len must be nonnegative.')
        OutVal = ZeroU128
        RETURN
    ELSEIF (Len == 0) THEN
        OutVal = ZeroU128
        RETURN
    ELSEIF ((Pos < 0).OR.(Pos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_ExtractBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos must be between 0 and 127.')
        OutVal = ZeroU128
        RETURN
    ELSEIF (Pos + Len > 128) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_ExtractBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Pos + Len > 128.')
        OutVal = ZeroU128
        RETURN
    END IF

    OutVal = ZeroU128
    IF (Pos < 64) THEN
        IF (Pos + Len <= 64) THEN
            ! bit fields are in only lower elements of both input and output
            CALL MVBITS(InVal%Low, Pos, Len, OutVal%Low, 0)
        ELSE
            IF (Len <= 64) THEN
                ! bit fields are in both lower and upper elements of input
                ! but only in lower element of output
                Len1 = 64-Pos
                Len2 = Len - Len1
                CALL MVBITS(InVal%Low,  Pos, Len1, OutVal%Low,    0)
                CALL MVBITS(InVal%High,   0, Len2, OutVal%Low, Len1)
            ELSE
                ! bit fields are in lower and upper elements of both input and output
                Len1 = 64-Pos           ! Input%Low  -> Output%Low
                Len2 = 64-Len1          ! Input%High -> Output%Low
                Len3 = Len-(Len1+Len2)  ! Input%High -> Output%High
                CALL MVBITS(InVal%Low,   Pos, Len1, OutVal%Low,     0)
                CALL MVBITS(InVal%High,    0, Len2, OutVal%Low,  Len1)
                CALL MVBITS(InVal%High, Len2, Len3, OutVal%High,    0)
            END IF
        END IF
    ELSE
        ! one of the simplest cases where bit fields are in upper element of input
        ! and in lower element of output
        CALL MVBITS(InVal%High, Pos-64, Len, OutVal%Low, 0)
    END IF

    RETURN

END FUNCTION U128_ExtractBits

!******************************************************************************

SUBROUTINE U128_MoveBits(InVal, InPos, Len, OutVal, OutPos)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy a sequence of bits (a bit field) from one location to another.  
    !   For more detail, see explanation of elemental intrinsic subroutine 'MVBITS'.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: InVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: InPos
    INTEGER(KIND=I4B),  INTENT(IN)    :: Len
    TYPE(UInt128),      INTENT(INOUT) :: OutVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: OutPos

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)        :: Len1, Len2, Len3

!** FLOW

    ! first, check input validity
    IF (Len < 0) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_MoveBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Len must be nonnegative.')
        RETURN
    ELSEIF (Len == 0) THEN
        RETURN
    ELSEIF ((InPos < 0).OR.(InPos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_MoveBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('InPos must be between 0 and 127.')
        RETURN
    ELSEIF ((OutPos < 0).OR.(OutPos > 127)) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_MoveBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('OutPos must be between 0 and 127.')
        RETURN
    ELSEIF (InPos + Len > 128) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_MoveBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('InPos + Len > 128.')
        RETURN
    ELSEIF (OutPos + Len > 128) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_MoveBits'//' in Module '//ModName//'.')
        CALL DisplayContinueError('OutPos + Len > 128.')
        RETURN
    END IF

    IF (InPos < 64) THEN
        IF (InPos + Len <= 64) THEN
            IF (OutPos < 64) THEN
                IF (OutPos + Len <= 64) THEN
                    ! one of the simplest cases where bit fields are in lower elements
                    CALL MVBITS(InVal%Low, InPos, Len, OutVal%Low, OutPos)
                ELSE
                    ! bit fields are in lower element of input but in both lower and
                    ! upper elements of output
                    Len1 = 64-OutPos
                    Len2 = Len - Len1
                    CALL MVBITS(InVal%Low, InPos,      Len1, OutVal%Low,  OutPos)
                    CALL MVBITS(InVal%Low, InPos+Len1, Len2, OutVal%High,      0)
                END IF
            ELSE
                ! one of the simplest cases where bit fields are in lower element of input
                ! and upper element of output, respectively
                CALL MVBITS(InVal%Low, InPos, Len, OutVal%High, OutPos-64)
            END IF
        ELSE
            IF (OutPos < 64) THEN
                IF (OutPos + Len <= 64) THEN
                    ! bit fields are in both lower and upper element of input but
                    ! only in lower element of output
                    Len1 = 64-InPos
                    Len2 = Len - Len1
                    CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%Low,      OutPos)
                    CALL MVBITS(InVal%High,     0, Len2, OutVal%Low, OutPos+Len1)
                ELSE
                    ! the most complicated cases where bit fields are in lower
                    ! and upper elements of both input and output
                    IF (InPos == OutPos) THEN
                        Len1 = 64-InPos         ! Input%Low  -> Output%Low
                        Len2 = Len-Len1         ! Input%High -> Output%High
                        CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%Low,  OutPos)
                        CALL MVBITS(InVal%High,     0, Len2, OutVal%High,      0)
                    ELSEIF (InPos < OutPos) THEN
                        Len1 = 64-OutPos        ! Input%Low  -> Output%Low
                        Len2 = 64-(InPos+Len1)  ! Input%Low  -> Output%High
                        Len3 = Len-(Len1+Len2)  ! Input%High -> Output%High
                        CALL MVBITS(InVal%Low,      InPos,  Len1, OutVal%Low,  OutPos)
                        CALL MVBITS(InVal%Low,  InPos+Len1, Len2, OutVal%High,      0)
                        CALL MVBITS(InVal%High,          0, Len3, OutVal%High,   Len2)
                    ELSE
                        Len1 = 64-InPos         ! Input%Low  -> Output%Low
                        Len2 = 64-(OutPos+Len1) ! Input%High -> Output%Low
                        Len3 = Len-(Len1+Len2)  ! Input%High -> Output%High
                        CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%Low,       OutPos)
                        CALL MVBITS(InVal%High,     0, Len2, OutVal%Low,  OutPos+Len1)
                        CALL MVBITS(InVal%High,  Len2, Len3, OutVal%High,           0)
                    END IF
                END IF
            ELSE
                ! bit fields are in both lower and upper element of input but
                ! only in upper element of output
                Len1 = 64-InPos
                Len2 = Len - Len1
                CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%High,      OutPos-64)
                CALL MVBITS(InVal%High,     0, Len2, OutVal%High, OutPos-64+Len1)
            END IF
        END IF
    ELSE
        IF (OutPos < 64) THEN
            IF (OutPos + Len <= 64) THEN
                ! one of the simplest cases where bit fields are in upper element of input
                ! and lower element of output, respectively
                CALL MVBITS(InVal%High, InPos-64, Len, OutVal%Low, OutPos)
            ELSE
                ! bit fields are in upper element of input but in both lower and
                ! upper elements of output
                Len1 = 64-OutPos
                Len2 = Len - Len1
                CALL MVBITS(InVal%High, InPos-64,      Len1, OutVal%Low,  OutPos)
                CALL MVBITS(InVal%High, InPos-64+Len1, Len2, OutVal%High,      0)
            END IF
        ELSE
            ! one of the simplest cases where bit fields are in upper elements
            CALL MVBITS(InVal%High, InPos-64, Len, OutVal%High, OutPos-64)
        END IF
    END IF

    RETURN

END SUBROUTINE U128_MoveBits

!------------------------------------------------------------------------------
!
!                           ARITHMETIC ROUTINES
!
!------------------------------------------------------------------------------

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    ADDITION OPERATIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION U128_UnaryPlus(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To return result of the unary plus sign of the Uint128 object.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: InVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!

!** FLOW

    OutVal = InVal

    RETURN

END FUNCTION U128_UnaryPlus

!******************************************************************************

SUBROUTINE U128_Increment(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !! To increase value of the input by 1.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(INOUT) :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!

!** FLOW

    IF (Val%Low == -1_I8B) THEN
        Val%High = Val%High + 1_I8B
        Val%Low  = 0_I8B
    ELSE
        Val%Low  = Val%Low + 1_I8B
    END IF

    RETURN

END SUBROUTINE U128_Increment

!******************************************************************************

SUBROUTINE U128_Add_U32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform addition:  This = This + Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I4B),  INTENT(IN)    :: Other      !! value treated as unsigned integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry, OutLo

!** FLOW

    CALL AddU64(This%Low, ToUnsignedLong(Other), 0_I8B, OutLo, Carry)
    This%Low  = OutLo
    This%High = This%High + Carry

    RETURN

END SUBROUTINE U128_Add_U32

!******************************************************************************

SUBROUTINE U128_Add_U64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform addition:  This = This + Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I8B),  INTENT(IN)    :: Other      !! value treated as unsigned integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry, OutLo

!** FLOW

    CALL AddU64(This%Low, Other, 0_I8B, OutLo, Carry)
    This%Low  = OutLo
    This%High = This%High + Carry

    RETURN

END SUBROUTINE U128_Add_U64

!******************************************************************************

SUBROUTINE U128_Add_U128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform addition:  This = This + Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(INOUT) :: This
    TYPE(UInt128), INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry, OutLo, OutHi

!** FLOW

    CALL AddU64(This%Low, Other%Low, 0_I8B, OutLo, Carry)
    CALL AddU64(This%High, Other%High, Carry, OutHi)
    This%Low  = OutLo
    This%High = OutHi

    RETURN

END SUBROUTINE U128_Add_U128

!******************************************************************************

FUNCTION U128_Plus_U32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform addition:  OutVal = LhsVal + RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: RhsVal     !! value treated as unsigned integer
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, ToUnsignedLong(RhsVal), 0_I8B, OutVal%Low, Carry)
    OutVal%High = LhsVal%High + Carry

    RETURN

END FUNCTION U128_Plus_U32

!******************************************************************************

FUNCTION U32_Plus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform addition:  OutVal = LhsVal + RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)    :: LhsVal     !! value treated as unsigned integer
    TYPE(UInt128),      INTENT(IN)    :: RhsVal
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL AddU64(ToUnsignedLong(LhsVal), RhsVal%Low, 0_I8B, OutVal%Low, Carry)
    OutVal%High = RhsVal%High + Carry

    RETURN

END FUNCTION U32_Plus_U128

!******************************************************************************

FUNCTION U128_Plus_U64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform addition:  OutVal = LhsVal + RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I8B),  INTENT(IN)    :: RhsVal     !! value treated as unsigned integer
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, RhsVal, 0_I8B, OutVal%Low, Carry)
    OutVal%High = LhsVal%High + Carry

    RETURN

END FUNCTION U128_Plus_U64

!******************************************************************************

FUNCTION U64_Plus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform addition:  OutVal = LhsVal + RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)    :: LhsVal     !! value treated as unsigned integer
    TYPE(UInt128),      INTENT(IN)    :: RhsVal
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL AddU64(LhsVal, RhsVal%Low, 0_I8B, OutVal%Low, Carry)
    OutVal%High = RhsVal%High + Carry

    RETURN

END FUNCTION U64_Plus_U128

!******************************************************************************

FUNCTION U128_Plus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform addition:  OutVal = LhsVal + RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: LhsVal
    TYPE(UInt128), INTENT(IN)    :: RhsVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, RhsVal%Low, 0_I8B, OutVal%Low, Carry)
    CALL AddU64(LhsVal%High, RhsVal%High, Carry, OutVal%High)

    RETURN

END FUNCTION U128_Plus_U128

!******************************************************************************

PURE SUBROUTINE AddU64(X, Y, CarryIn, Sum, CarryOut)

!DIR$ ATTRIBUTES INLINE :: AddU64

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the sum with carry of X, Y and CarryIn: Sum = X + Y + CarryIn.  
    !   The carry input must be 0 or 1; otherwise the behavior is undefined.  
    !   The carry output is guaranteed to be 0 or 1.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)     :: X, Y, CarryIn      !! values treated as unsigned integers
    INTEGER(KIND=I8B), INTENT(OUT)    :: Sum, CarryOut      !! values treated as unsigned integers
    OPTIONAL                          :: CarryOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Sum = X + Y + CarryIn
    ! The sum will overflow if both top bits are set (x & y) or if one of them
    ! is (x | y), and a carry from the lower place happened. If such a carry
    ! happens, the top bit will be 1 + 0 + 1 = 0 (&^ sum).
    IF (PRESENT(CarryOut)) THEN
        CarryOut = SHIFTR(IOR(IAND(X, Y), IAND(IOR(X, Y), NOT(Sum))), 63)
    END IF

    RETURN

END SUBROUTINE AddU64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  SUBTRACTION OPERATIONS                  ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION U128_Negate(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To negate the Uint128 object.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: InVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = NOT(InVal%High)
    IF (InVal%Low == 0_I8B) OutVal%High = OutVal%High + 1_I8B
    OutVal%Low = NOT(InVal%Low) + 1_I8B

    RETURN

END FUNCTION U128_Negate

!******************************************************************************

SUBROUTINE U128_Decrement(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !! To decrease value of the input by 1.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(INOUT) :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!

!** FLOW

    IF (Val%Low == 0_I8B) THEN
        Val%High = Val%High - 1_I8B
        Val%Low  = -1_I8B
    ELSE
        Val%Low  = Val%Low - 1_I8B
    END IF

    RETURN

END SUBROUTINE U128_Decrement

!******************************************************************************

SUBROUTINE U128_Subtract_U32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform subtraction:  This = This - Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I4B),  INTENT(IN)    :: Other      !! value treated as unsigned integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow, OutLo

!** FLOW

    CALL SubU64(This%Low, ToUnsignedLong(Other), 0_I8B, OutLo, Borrow)
    This%Low  = OutLo
    This%High = This%High - Borrow

    RETURN

END SUBROUTINE U128_Subtract_U32

!******************************************************************************

SUBROUTINE U128_Subtract_U64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform subtraction:  This = This - Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I8B),  INTENT(IN)    :: Other      !! value treated as unsigned integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow, OutLo

!** FLOW

    CALL SubU64(This%Low, Other, 0_I8B, OutLo, Borrow)
    This%Low  = OutLo
    This%High = This%High - Borrow

    RETURN

END SUBROUTINE U128_Subtract_U64

!******************************************************************************

SUBROUTINE U128_Subtract_U128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform subtraction:  This = This - Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(INOUT) :: This
    TYPE(UInt128), INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow, OutLo, OutHi

!** FLOW

    CALL SubU64(This%Low, Other%Low, 0_I8B, OutLo, Borrow)
    CALL SubU64(This%High, Other%High, Borrow, OutHi)
    This%Low  = OutLo
    This%High = OutHi

    RETURN

END SUBROUTINE U128_Subtract_U128

!******************************************************************************

FUNCTION U128_Minus_U32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform subtraction:  OutVal = LhsVal - RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: RhsVal     !! value treated as unsigned integer
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, ToUnsignedLong(RhsVal), 0_I8B, OutVal%Low, Borrow)
    OutVal%High = LhsVal%High - Borrow

    RETURN

END FUNCTION U128_Minus_U32

!******************************************************************************

FUNCTION U32_Minus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform subtraction:  OutVal = LhsVal - RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)    :: LhsVal     !! value treated as unsigned integer
    TYPE(UInt128),      INTENT(IN)    :: RhsVal
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow

!** FLOW

    CALL SubU64(ToUnsignedLong(LhsVal), RhsVal%Low, 0_I8B, OutVal%Low, Borrow)
    OutVal%High = -(RhsVal%High + Borrow)

    RETURN

END FUNCTION U32_Minus_U128

!******************************************************************************

FUNCTION U128_Minus_U64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform subtraction:  OutVal = LhsVal - RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I8B),  INTENT(IN)    :: RhsVal     !! value treated as unsigned integer
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, RhsVal, 0_I8B, OutVal%Low, Borrow)
    OutVal%High = LhsVal%High - Borrow

    RETURN

END FUNCTION U128_Minus_U64

!******************************************************************************

FUNCTION U64_Minus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform subtraction:  OutVal = LhsVal - RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)    :: LhsVal     !! value treated as unsigned integer
    TYPE(UInt128),      INTENT(IN)    :: RhsVal
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow

!** FLOW

    CALL SubU64(LhsVal, RhsVal%Low, 0_I8B, OutVal%Low, Borrow)
    OutVal%High = -(RhsVal%High + Borrow)

    RETURN

END FUNCTION U64_Minus_U128

!******************************************************************************

FUNCTION U128_Minus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform subtraction:  OutVal = LhsVal - RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: LhsVal
    TYPE(UInt128), INTENT(IN)    :: RhsVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, RhsVal%Low, 0_I8B, OutVal%Low, Borrow)
    CALL SubU64(LhsVal%High, RhsVal%High, Borrow, OutVal%High)

    RETURN

END FUNCTION U128_Minus_U128

!******************************************************************************

PURE SUBROUTINE SubU64(X, Y, BorrowIn, Diff, BorrowOut)

!DIR$ ATTRIBUTES INLINE :: SubU64

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the difference of X, Y and BorrowIn: Diff = X - Y - BorrowIn.  
    !   The borrow input must be 0 or 1; otherwise the behavior is undefined.  
    !   The borrow output is guaranteed to be 0 or 1.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B), INTENT(IN)     :: X, Y, BorrowIn     !! values treated as unsigned integers
    INTEGER(KIND=I8B), INTENT(OUT)    :: Diff, BorrowOut    !! values treated as unsigned integers
    OPTIONAL                          :: BorrowOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Diff = X - Y - BorrowIn
    ! The difference will underflow if the top bit of x is not set and the top
    ! bit of y is set (^x & y) or if they are the same (^(x ^ y)) and a Borrow
    ! from the lower place happens. If that Borrow happens, the result will be
    ! 1 - 1 - 1 = 0 - 0 - 1 = 1 (& diff).
    IF (PRESENT(BorrowOut)) THEN
        BorrowOut = SHIFTR(IOR(IAND(NOT(X), Y), IAND(NOT(IEOR(X, Y)), Diff)), 63)
    END IF

    RETURN

END SUBROUTINE SubU64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                 MULTIPLICATION OPERATIONS                ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE U128_Times_U32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform multiplication:  This = This * Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I4B),  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: X_Lo, Y_Lo, Y_Hi
    INTEGER(KIND=I8B)     :: Lo_Lo, Cross

!** FLOW

    X_Lo = IAND(INT(Other, KIND=I8B), Mask32)
    Y_Lo = IAND(This%Low, Mask32)
    Y_Hi = SHIFTR(This%Low, 32)
    Lo_Lo = X_Lo*Y_Lo
    Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi

    This%High = This%High*Other + SHIFTR(Cross, 32)
    This%Low  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))

    RETURN

END SUBROUTINE U128_Times_U32

!******************************************************************************

SUBROUTINE U128_Times_U64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform multiplication:  This = This * Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(INOUT) :: This
    INTEGER(KIND=I8B),  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry, ProductLow

!** FLOW

    CALL UMul128(This%Low, Other, Carry, ProductLow)
    This%High = This%High*Other + Carry
    This%Low  = ProductLow

    RETURN

END SUBROUTINE U128_Times_U64

!******************************************************************************

SUBROUTINE U128_Times_U128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform multiplication:  This = This * Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(INOUT) :: This
    TYPE(UInt128), INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry, ProductLow

!** FLOW

    CALL UMul128(This%Low, Other%Low, Carry, ProductLow)
    This%High = This%Low*Other%High + This%High*Other%Low + Carry
    This%Low  = ProductLow

    RETURN

END SUBROUTINE U128_Times_U128

!******************************************************************************

FUNCTION U32_Multiply_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform multiplication:  OutVal = LhsVal * RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I4B),  INTENT(IN)    :: LhsVal
    TYPE(UInt128),      INTENT(IN)    :: RhsVal
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: X_Lo, Y_Lo, Y_Hi
    INTEGER(KIND=I8B)     :: Lo_Lo, Cross

!** FLOW

    X_Lo = IAND(INT(LhsVal, KIND=I8B), Mask32)
    Y_Lo = IAND(RhsVal%Low, Mask32)
    Y_Hi = SHIFTR(RhsVal%Low, 32)
    Lo_Lo = X_Lo*Y_Lo
    Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi

    OutVal%High = LhsVal*RhsVal%High + SHIFTR(Cross, 32)
    OutVal%Low  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))

    RETURN

END FUNCTION U32_Multiply_U128

!******************************************************************************

FUNCTION U128_Multiply_U32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform multiplication:  OutVal = LhsVal * RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I4B),  INTENT(IN)    :: RhsVal
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: X_Lo, Y_Lo, Y_Hi
    INTEGER(KIND=I8B)     :: Lo_Lo, Cross

!** FLOW

    X_Lo = IAND(INT(RhsVal, KIND=I8B), Mask32)
    Y_Lo = IAND(LhsVal%Low, Mask32)
    Y_Hi = SHIFTR(LhsVal%Low, 32)
    Lo_Lo = X_Lo*Y_Lo
    Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi

    OutVal%High = RhsVal*LhsVal%High + SHIFTR(Cross, 32)
    OutVal%Low  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))

    RETURN

END FUNCTION U128_Multiply_U32

!******************************************************************************

FUNCTION U64_Multiply_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform multiplication:  OutVal = LhsVal * RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    INTEGER(KIND=I8B),  INTENT(IN)    :: LhsVal
    TYPE(UInt128),      INTENT(IN)    :: RhsVal
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL UMul128(LhsVal, RhsVal%Low, Carry, OutVal%Low)
    OutVal%High = LhsVal*RhsVal%High + Carry

    RETURN

END FUNCTION U64_Multiply_U128

!******************************************************************************

FUNCTION U128_Multiply_U64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform multiplication:  OutVal = LhsVal * RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: LhsVal
    INTEGER(KIND=I8B),  INTENT(IN)    :: RhsVal
    TYPE(UInt128)                     :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL UMul128(LhsVal%Low, RhsVal, Carry, OutVal%Low)
    OutVal%High = LhsVal%High*RhsVal + Carry

    RETURN

END FUNCTION U128_Multiply_U64

!******************************************************************************

FUNCTION U128_Multiply_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform multiplication:  OutVal = LhsVal * RhsVal.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: LhsVal
    TYPE(UInt128), INTENT(IN)    :: RhsVal
    TYPE(UInt128)                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I8B)     :: Carry

!** FLOW

    CALL UMul128(LhsVal%Low, RhsVal%Low, Carry, OutVal%Low)
    OutVal%High = LhsVal%Low*RhsVal%High + LhsVal%High*RhsVal%Low + Carry

    RETURN

END FUNCTION U128_Multiply_U128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++             DIVISION/MODULATION OPERATIONS               ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE U128_DivMod_U32(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.  
    !   And, to return both quotient and remainder.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I4B),  INTENT(IN)    :: Divisor
    TYPE(UInt128),      INTENT(OUT)   :: Quotient
    TYPE(UInt128),      INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END SUBROUTINE U128_DivMod_U32

!******************************************************************************

SUBROUTINE U128_DivMod_U64(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.  
    !   And, to return both quotient and remainder.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I8B),  INTENT(IN)    :: Divisor
    TYPE(UInt128),      INTENT(OUT)   :: Quotient
    TYPE(UInt128),      INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END SUBROUTINE U128_DivMod_U64

!******************************************************************************

SUBROUTINE U128_DivMod_U128(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.  
    !   And, to return both quotient and remainder.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: Dividend
    TYPE(UInt128), INTENT(IN)    :: Divisor
    TYPE(UInt128), INTENT(OUT)   :: Quotient
    TYPE(UInt128), INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Dividend%High < 0_I8B) THEN
        CALL U128_DivMod_Intx(Dividend, Divisor, Quotient, Remainder)
    ELSE
        CALL U128_DivMod_Java(Dividend, Divisor, Quotient, Remainder)
    END IF

    RETURN

END SUBROUTINE U128_DivMod_U128

!******************************************************************************

SUBROUTINE U128_Over_U32(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform division:  This = This / Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),               INTENT(INOUT)    :: This
    INTEGER(KIND=I4B),           INTENT(IN)       :: Other
    INTEGER(KIND=I4B), OPTIONAL, INTENT(OUT)      :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)    :: Dividend, Rem

!** FLOW

    Dividend = This
    CALL U128_DivMod_U128(Dividend, UInt128(Other, AsUnsigned), This, Rem)
    IF (PRESENT(Remainder)) Remainder = Rem

    RETURN

END SUBROUTINE U128_Over_U32

!******************************************************************************

SUBROUTINE U128_Over_U64(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform division:  This = This / Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),               INTENT(INOUT)    :: This
    INTEGER(KIND=I8B),           INTENT(IN)       :: Other
    INTEGER(KIND=I8B), OPTIONAL, INTENT(OUT)      :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)    :: Dividend, Rem

!** FLOW

    Dividend = This
    CALL U128_DivMod_U128(Dividend, UInt128(Other, AsUnsigned), This, Rem)
    IF (PRESENT(Remainder)) Remainder = Rem

    RETURN

END SUBROUTINE U128_Over_U64

!******************************************************************************

SUBROUTINE U128_Over_U128(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform division:  This = This / Other.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),           INTENT(INOUT)   :: This
    TYPE(UInt128),           INTENT(IN)      :: Other
    TYPE(UInt128), OPTIONAL, INTENT(OUT)     :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)    :: Dividend, Rem

!** FLOW

    Dividend = This
    IF (PRESENT(Remainder)) THEN
        CALL U128_DivMod_U128(Dividend, Other, This, Remainder)
    ELSE
        CALL U128_DivMod_U128(Dividend, Other, This, Rem)
    END IF

    RETURN

END SUBROUTINE U128_Over_U128

!******************************************************************************

FUNCTION U128_Divide_U32(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform division:  Quotient = Dividend / Divisor.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I4B),  INTENT(IN)    :: Divisor
    TYPE(UInt128)                     :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)    :: Remainder

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END FUNCTION U128_Divide_U32

!******************************************************************************

FUNCTION U128_Divide_U64(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform division:  Quotient = Dividend / Divisor.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I8B),  INTENT(IN)    :: Divisor
    TYPE(UInt128)                     :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)    :: Remainder

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END FUNCTION U128_Divide_U64

!******************************************************************************

FUNCTION U128_Divide_U128(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform division:  Quotient = Dividend / Divisor.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: Dividend
    TYPE(UInt128), INTENT(IN)    :: Divisor
    TYPE(UInt128)                :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)    :: Remainder

!** FLOW

    CALL U128_DivMod_U128(Dividend, Divisor, Quotient, Remainder)

    RETURN

END FUNCTION U128_Divide_U128

!******************************************************************************

FUNCTION U128_Mod_U32(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform modulation:  Remainder = Dividend MOD Divisor.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I4B),  INTENT(IN)    :: Divisor
    TYPE(UInt128)                     :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)    :: Quotient

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END FUNCTION U128_Mod_U32

!******************************************************************************

FUNCTION U128_Mod_U64(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform modulation:  Remainder = Dividend MOD Divisor.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)    :: Dividend
    INTEGER(KIND=I8B),  INTENT(IN)    :: Divisor
    TYPE(UInt128)                     :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)    :: Quotient

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END FUNCTION U128_Mod_U64

!******************************************************************************

FUNCTION U128_Mod_U128(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform modulation:  Remainder = Dividend MOD Divisor.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: Dividend
    TYPE(UInt128), INTENT(IN)    :: Divisor
    TYPE(UInt128)                :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)    :: Quotient

!** FLOW

    CALL U128_DivMod_U128(Dividend, Divisor, Quotient, Remainder)

    RETURN

END FUNCTION U128_Mod_U128

!******************************************************************************

SUBROUTINE U128_DivMod_Java(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division of two UInt128 objects (Dividend / Divisor)
    !   and return both the quotient and the remainder.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: Dividend
    TYPE(UInt128), INTENT(IN)    :: Divisor
    TYPE(UInt128), INTENT(OUT)   :: Quotient
    TYPE(UInt128), INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: NumerLZ
    INTEGER(KIND=I4B)     :: DenomLZ
    INTEGER(KIND=I4B)     :: DenomTZ
    INTEGER(KIND=I4B)     :: CompFlag

!** FLOW

    IF (Divisor == ZeroU128) THEN
        ! division by zero
        Quotient  = ZeroU128
        Remainder = ZeroU128
        CALL DisplaySevereError('Message from Routine '//'U128_DivMod_Java'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The divisor must not be zero.')
        RETURN
    END IF

    CompFlag = CompareU128(Dividend%High, Dividend%Low, Divisor%High, Divisor%Low)
    IF (CompFlag < 0) THEN
        ! divisor > dividend
        Quotient  = ZeroU128
        Remainder = Dividend
        RETURN
    ELSEIF (CompFlag == 0) THEN
        ! divisor == dividend
        Quotient  = OneU128
        Remainder = ZeroU128
        RETURN
    END IF

    NumerLZ = LEADZ(Dividend)
    DenomLZ = LEADZ(Divisor)
    DenomTZ = TRAILZ(Divisor)

    IF (DenomLZ == 128) THEN
        CALL DisplaySevereError('Message from Routine '//'U128_DivMod_Java'//' in Module '//ModName//'.')
        CALL DisplayContinueError('Divide by zero.')
        RETURN
    ELSEIF (IOR(Dividend%High, Divisor%High) == 0_I8B) THEN
        ! dividend and divisor fit in an unsigned
        CALL UDivMod(Dividend%Low, Divisor%Low, Quotient%Low, Remainder%Low)
        Quotient%High  = 0_I8B
        Remainder%High = 0_I8B
        RETURN
    ELSEIF (DenomLZ == 127) THEN
        ! divisor is 1
        Quotient  = Dividend
        Remainder = ZeroU128
        RETURN
    ELSEIF ((DenomTZ + DenomLZ) == 127) THEN
        ! only one bit set (i.e., power of 2), so just shift
        Quotient  = SHIFTR(Dividend, DenomTZ)
        Remainder = IAND(Dividend, Divisor - OneU128)
        RETURN
    END IF

    IF ((DenomLZ - NumerLZ) > 15) THEN
        ! fast divide when the values differ by this many orders of magnitude
        CALL FastDivision(Dividend, Divisor, Quotient, Remainder)
    ELSE
        CALL BinaryDivision(Dividend, Divisor, NumerLZ, DenomLZ, Quotient, Remainder)
    END IF

    RETURN

CONTAINS

    SUBROUTINE BinaryDivision(Numerator, Denominator, NumerLZ, DenomLZ, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division of two SInt128 objects (Dividend / Divisor)
        ! using binary-division algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(UInt128),      INTENT(IN)   :: Numerator, Denominator
        INTEGER(KIND=I4B),  INTENT(IN)   :: NumerLZ, DenomLZ
        TYPE(UInt128),      INTENT(OUT)  :: Quotient
        TYPE(UInt128),      INTENT(OUT)  :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: Shift
        INTEGER(KIND=I8B)     :: NumerHi, NumerLo, DenomHi, DenomLo
        INTEGER(KIND=I8B)     :: QuotHi, QuotLo

    !** FLOW

        ! initialize
        NumerHi = Numerator%High
        NumerLo = Numerator%Low

        ! shift should always be posivite since the routine is only called
        ! when divisor magnitude is less than dividen magnitude
        Shift   = DenomLZ - NumerLZ
        IF (Shift >= 64) THEN
            DenomHi = SHIFTL(Denominator%Low, Shift - 64)
            DenomLo = 0_I8B
        ELSE
            DenomHi = IOR(SHIFTL(Denominator%High, Shift), SHIFTR(Denominator%Low, 64 - Shift))
            DenomLo = SHIFTL(Denominator%Low, Shift)
        END IF
        QuotHi = 0_I8B
        QuotLo = 0_I8B

        DO
            ! quotient = SHIFTL(quotient, 1)
            QuotHi = IOR(SHIFTL(QuotHi, 1), SHIFTR(QuotLo, 63))
            QuotLo = SHIFTL(QuotLo, 1)

            ! if (dividend >= divisor)
            IF (CompareU128(NumerHi, NumerLo, DenomHi, DenomLo) >= 0) THEN
                ! dividend = dividend - divisor
                IF (IEOR(NumerLo, MinI64) < IEOR(DenomLo, MinI64)) THEN
                    NumerHi = NumerHi - DenomHi - 1_I8B
                    NumerLo = NumerLo - DenomLo
                ELSE
                    NumerHi = NumerHi - DenomHi
                    NumerLo = NumerLo - DenomLo
                END IF

                ! quotient = IOR(quotient, 1)
                QuotLo = IOR(QuotLo, 1_I8B)
            END IF

            ! divisor = SHIFTR(divisor, 1)
            DenomLo = IOR(SHIFTR(DenomLo, 1), SHIFTL(SHIFTL(DenomHi, 1), 62))
            DenomHi = SHIFTR(DenomHi, 1)

            IF (Shift == 0) EXIT
            Shift = Shift - 1
        END DO

        ! set output
        Quotient%High  = QuotHi
        Quotient%Low   = QuotLo
        Remainder%High = NumerHi
        Remainder%Low  = NumerLo

        RETURN

    END SUBROUTINE BinaryDivision

!******************************************************************************

    SUBROUTINE FastDivision(Dividend, Divisor, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division of two SInt128 objects (Dividend / Divisor)
        ! using fast-division algorithm(s)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(UInt128), INTENT(IN)    :: Dividend
        TYPE(UInt128), INTENT(IN)    :: Divisor
        TYPE(UInt128), INTENT(OUT)   :: Quotient
        TYPE(UInt128), INTENT(OUT)   :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: NLZ
        INTEGER(KIND=I8B)     :: NumHi, NumLo
        INTEGER(KIND=I8B)     :: V1Hi, U1Hi, U1Lo
        INTEGER(KIND=I8B)     :: QLo, RLo

    !** FLOW

        IF (Divisor%High == 0_I8B) THEN
            IF (CompareUnsigned(Dividend%High, Divisor%Low) < 0) THEN
                CALL DivideU128ByU64(Dividend%High, Dividend%Low, Divisor%Low, &
                                    Quotient%Low, Remainder%Low)
                Quotient%High  = 0_I8B
                Remainder%High = 0_I8B
            ELSE
                CALL UDivMod(Dividend%High, Divisor%Low, Quotient%High, NumHi)
                NumLo = Dividend%Low
                CALL DivideU128ByU64(NumHi, NumLo, Divisor%Low, Quotient%Low, Remainder%Low)
                Remainder%High = 0_I8B
            END IF
        ELSE
            NLZ = LEADZ(Divisor%High)
            ! v1 = divisor << nlz
            IF (NLZ < 64) THEN
                V1Hi = IOR(SHIFTL(Divisor%High, NLZ), SHIFTR(Divisor%Low, 64 - NLZ))
            ELSE
                V1Hi = SHIFTL(Divisor%Low, NLZ - 64)
            END IF
            ! u1 = dividend >>> 1
            U1Lo = IOR(SHIFTR(Dividend%Low, 1), SHIFTL(SHIFTL(Dividend%High, 1), 62))
            U1Hi = SHIFTR(Dividend%High, 1)
            CALL DivideU128ByU64(U1Hi, U1Lo, V1Hi, QLo, RLo)
            ! q1 = q1 >>> (63 - nlz)
            QLo  = SHIFTR(QLo, 63 - NLZ)
            ! if (q1 /= 0)
            IF (QLo /= 0_I8B) QLo = QLo - 1_I8B
            ! r = dividend - q1 * divisor
            Remainder = Dividend - (Divisor * QLo)
            Quotient%High = 0_I8B
            IF (CompareU128(Remainder%High, Remainder%Low, Divisor%High, Divisor%Low) >= 0) THEN
                ! quotient++
                QLo = QLo + 1_I8B
                IF (QLo == 0_I8B) Quotient%High = 1_I8B
                ! remainder -= divisor
                CALL Subtract(Remainder, Divisor)
            END IF
            Quotient%Low = QLo
        END IF

        RETURN

    END SUBROUTINE FastDivision

!******************************************************************************

    SUBROUTINE DivideU128ByU64(DividendHi, DividendLo, Divisor, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division of 128-bit unsigned integer by 64-bit unsigned integer
        ! and return quotient and remainder as 64-bit unsigned integers
        ! This routine is only applicable for cases where the divisor is greater
        !   than the upper halft of the dividend (i.e. Divisor .UGT. DividendHi).
        ! Note: This routine is based on 'divlu' of Hacker's delight and its derivative
        !       (division routine in Fast 128-bit math library for Java)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)     :: DividendHi, DividendLo
        INTEGER(KIND=I8B), INTENT(IN)     :: Divisor
        INTEGER(KIND=I8B), INTENT(OUT)    :: Quotient
        INTEGER(KIND=I8B), INTENT(OUT)    :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        INTEGER(KIND=I8B), PARAMETER  :: Mask = INT(Z'00000000FFFFFFFF', KIND=I8B)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I4B)     :: Shift
        INTEGER(KIND=I8B)     :: NHi, NLo         ! N -> numerator
        INTEGER(KIND=I8B)     :: LoHi, LoLo
        INTEGER(KIND=I8B)     :: RHat, UHat
        INTEGER(KIND=I8B)     :: Denom, DHi, DLo  ! D -> denominator
        INTEGER(KIND=I8B)     :: QHi, QLo

    !** FLOW

        ! initialize
        Denom = Divisor
        NHi   = DividendHi
        NLo   = DividendLo
        Shift = LEADZ(Denom)

        IF (Shift /= 0) THEN
            Denom = SHIFTL(Denom, Shift)
            NHi = IOR(SHIFTL(NHi, Shift), SHIFTR(NLo, 64 - Shift))
            NLo = SHIFTL(NLo, Shift)
        END IF

        DHi  = SHIFTR(Denom, 32)
        DLo  = IAND(Denom, Mask)
        LoHi = SHIFTR(NLo, 32)
        LoLo = IAND(NLo, Mask)

        ! Compute NHi quotient digit.
        CALL UDivMod(NHi, DHi, QHi, RHat)

        ! qhat >>> 32 == qhat > base
        DO WHILE ((SHIFTR(QHi, 32) /= 0_I8B).OR. &
                  (IEOR(QHi * DLo, MinI64) > IEOR(IOR(SHIFTL(RHat, 32), LoHi), MinI64)))
            QHi = QHi - 1_I8B
            RHat = RHat + DHi
            IF (SHIFTR(RHat, 32) /= 0_I8B) EXIT
        END DO

        UHat = IOR(SHIFTL(NHi, 32), LoHi) - QHi * Denom

        ! Compute NLo quotient digit.
        CALL UDivMod(UHat, DHi, QLo, RHat)

        DO WHILE ((SHIFTR(QLo, 32) /= 0_I8B).OR.   &
                  (IEOR(QLo * DLo, MinI64) > IEOR(IOR(SHIFTL(RHat, 32), LoLo), MinI64)))
            QLo = QLo - 1
            RHat = RHat + DHi
            IF (SHIFTR(RHat, 32) /= 0_I8B) EXIT
        END DO

        Quotient  = IOR(SHIFTL(QHi, 32), QLo)
        Remainder = SHIFTR(IOR(SHIFTL(UHat, 32), LoLo) - QLo * Denom, Shift)

        RETURN

    END SUBROUTINE DivideU128ByU64

!******************************************************************************

    FUNCTION CompareU128(LhsHi, LhsLo, RhsHi, RhsLo) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compare LHS and RHS where both numbers are treated as unsigned.
        ! - return -1 if LHS < RHS
        ! - return  0 if LHS == RHS
        ! - return +1 if LHS > RHS

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN) :: LhsHi, LhsLo, RhsHi, RhsLo
        INTEGER(KIND=I4B)             :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: ULHS, URHS

    !** FLOW

        ULHS = IEOR(LhsHi, MinI64)
        URHS = IEOR(RhsHi, MinI64)
        IF (ULHS < URHS) THEN
            Flag = -1
        ELSEIF (ULHS > URHS) THEN
            Flag = +1
        ELSE
            Flag = 0
        END IF

        IF (Flag == 0) THEN
            ULHS = IEOR(LhsLo, MinI64)
            URHS = IEOR(RhsLo, MinI64)
            IF (ULHS < URHS) THEN
                Flag = -1
            ELSEIF (ULHS > URHS) THEN
                Flag = +1
            ELSE
                Flag = 0
            END IF
        END IF

        RETURN

    END FUNCTION CompareU128

!******************************************************************************

END SUBROUTINE U128_DivMod_Java

!******************************************************************************

SUBROUTINE U128_DivMod_IntX(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division of two UInt128 objects (Dividend / Divisor)
    !   and return both the quotient and the remainder.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)    :: Dividend
    TYPE(UInt128), INTENT(IN)    :: Divisor
    TYPE(UInt128), INTENT(OUT)   :: Quotient
    TYPE(UInt128), INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTEGER(KIND=I4B)     :: LSh, RSh
    INTEGER(KIND=I8B)     :: NumerHi, NumerLo, DenomHi, DenomLo
    INTEGER(KIND=I8B)     :: NumerEx, Denom, RshMask
    INTEGER(KIND=I8B)     :: V, R1, R2, Q1, Q2, LHS, RHS

!** FLOW

    IF (Divisor == ZeroU128) THEN
        ! division by zero
        Quotient  = ZeroU128
        Remainder = ZeroU128
        CALL DisplaySevereError('Message from Routine '//'U128_DivMod_IntX'//' in Module '//ModName//'.')
        CALL DisplayContinueError('The divisor must not be zero.')
        RETURN
    END IF

    IF (Divisor%High == 0) THEN
        LSh = LEADZ(Divisor%Low)
        IF (LSh == 0) THEN
            RSh = 0
            RShMask = 0_I8B
        ELSE
            RSh = 64 - LSh
            RShMask = -1_I8B
        END IF
        Denom   = SHIFTL(Divisor%Low, LSh)
        NumerLo = SHIFTL(Dividend%Low, LSh)
        NumerHi = IOR(SHIFTL(Dividend%High, LSh), IAND(SHIFTR(Dividend%Low, RSh), RShMask))
        NumerEx = IAND(SHIFTR(Dividend%High, RSh), RShMask)

        V = Reciprocal_2By1(Denom)
        CALL UDivRem_2By1(NumerEx, NumerHi, Denom, V, Q1, R1)
        CALL UDivRem_2By1(R1, NumerLo, Denom, V, Q2, R2)
        Quotient%High = Q1
        Quotient%Low  = Q2
        Remainder%High = 0_I8B
        Remainder%Low  = SHIFTR(R2, LSh)
        RETURN
    END IF

    IF (IEOR(Divisor%High, MinI64) > IEOR(Dividend%High, MinI64)) THEN
        ! divisor > dividend
        Quotient  = ZeroU128
        Remainder = Dividend
        RETURN
    END IF

    LSh = LEADZ(Divisor%High)
    IF (LSh == 0) THEN
        IF (IEOR(Divisor%High, MinI64) < IEOR(Dividend%High, MinI64)) THEN
            LHS = 1_I8B
        ELSE
            LHS = 0_I8B
        END IF
        IF (IEOR(Divisor%Low, MinI64) <= IEOR(Dividend%Low, MinI64)) THEN
            RHS = 1_I8B
        ELSE
            RHS = 0_I8B
        END IF
        Quotient%High = 0_I8B
        Quotient%Low  = IOR(LHS, RHS)
        IF (Quotient%Low  == 0_I8B) THEN
            Remainder = Dividend
        ELSE
            ! Remainder = Dividend - Divisor
            Remainder%Low  = Dividend%Low - Divisor%Low
            IF (IEOR(Dividend%Low, MinI64) < IEOR(Divisor%Low, MinI64)) THEN
                Remainder%High = Dividend%High - Divisor%High - 1_I8B
            ELSE
                Remainder%High = Dividend%High - Divisor%High
            END IF
        END IF
        RETURN
    END IF

    RSh = 64 - LSh

    DenomLo = SHIFTL(Divisor%Low, LSh)
    DenomHi = IOR(SHIFTL(Divisor%High, LSh), SHIFTR(Divisor%Low, RSh))
    NumerLo = SHIFTL(Dividend%Low, LSh)
    NumerHi = IOR(SHIFTL(Dividend%High, LSh), SHIFTR(Dividend%Low, RSh))
    NumerEx = SHIFTR(Dividend%High, RSh)

    V = Reciprocal_3By2(DenomHi, DenomLo)
    CALL UDivRem_3By2(NumerEx, NumerHi, NumerLo, DenomHi, DenomLo, V, Q2, R1, R2)

    Quotient%High = 0_I8B
    Quotient%Low  = Q2
    Remainder%High = SHIFTR(R1, LSh)
    Remainder%Low  = IOR(SHIFTR(R2, LSh), SHIFTL(R1, 64 - LSh))

    RETURN

    CONTAINS

    FUNCTION Reciprocal_2By1(D) RESULT(R)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute the reciprocal (2**128 - 1) / D - 2**64 for normalized D
        ! based on Algorithm 2 from "Improved division by invariant integers".

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN) :: D
        INTEGER(KIND=I8B)             :: R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: D0, D9, D40, D63, E, T
        INTEGER(KIND=I8B)     :: PHi, PLo, V0, V1, V2, V3

    !** FLOW

        D9  = SHIFTR(D, 55)
        V0  = INT(RecTable(INT(D9, KIND=I4B) - 256), KIND=I8B)

        D40 = SHIFTR(D, 24) + 1
        V1  = SHIFTL(V0, 11) - SHIFTR((V0 * V0) * D40, 40) - 1_I8B

        V2  = SHIFTL(V1, 13) + SHIFTR(V1 * (INT(Z'1000000000000000', KIND=I8B) - V1 * D40), 47)

        D0  = IAND(D, 1_I8B)
        D63 = SHIFTR(D, 1) + D0     ! ceil(D/2)
        E   = IAND(SHIFTR(V2, 1), (0_I8B - D0)) - V2 * D63
        CALL UMul128(V2, E, PHi, PLo)
        V3  = SHIFTR(PHi, 1) + SHIFTL(V2, 31)

        CALL UMul128(V3, D, PHi, PLo)
        T = PLo + D
        IF (IEOR(T, MinI64) < IEOR(PLo, MinI64)) PHi = PHi + 1
        R = V3 - PHi - D

        RETURN

    END FUNCTION Reciprocal_2By1

    !******************************************************************************

    FUNCTION Reciprocal_3By2(DHi, DLo) RESULT(R)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute the reciprocal (2**128 - 1) / D - 2**64 for normalized D
        ! based on Algorithm 2 from "Improved division by invariant integers".

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN) :: DHi, DLo
        INTEGER(KIND=I8B)             :: R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: V, P, THi, TLo

    !** FLOW

        V = Reciprocal_2By1(DHi)
        P = DHi * V
        P = P + DLo
        IF (IEOR(P, MinI64) < IEOR(DLo, MinI64)) THEN
            V = V - 1_I8B
            IF (IEOR(P, MinI64) >= IEOR(DHi, MinI64)) THEN
                V = V - 1_I8B
                P = P - DHi
            END IF
            P = P - DHi
        END IF

        CALL UMul128(V, DLo, THi, TLo)

        P = P + THi
        IF (IEOR(P, MinI64) < IEOR(THi, MinI64)) THEN
            V = V - 1_I8B
            IF (IEOR(P, MinI64) >= IEOR(DHi, MinI64)) THEN
                IF ((IEOR(P,   MinI64) >  IEOR(DHi, MinI64)).OR.&
                    (IEOR(TLo, MinI64) >= IEOR(DLo, MinI64))) V = V - 1_I8B
            END IF
        END IF
        R = V

        RETURN

    END FUNCTION Reciprocal_3By2

    !******************************************************************************

    SUBROUTINE UDivRem_2By1(UHi, ULo, D, V, Q, R)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To performm 128-bit integer division by 64-bit integer

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)     :: UHi, ULo, D, V
        INTEGER(KIND=I8B), INTENT(OUT)    :: Q, R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: QHi, QLo, NewLo

    !** FLOW

        ! Q128 = V*UHi
        CALL UMul128(V, UHi, QHi, QLo)

        ! Q128 = Q128 + U128
        NewLo = QLo + ULo
        IF (IEOR(NewLo, MinI64) < IEOR(QLo, MinI64)) THEN
            QHi = QHi + UHi + 1_I8B
        ELSE
            QHi = QHi + UHi
        END IF
        QLo = NewLo

        QHi = QHi + 1_I8B

        R = ULo - QHi*D

        IF (IEOR(R, MinI64) > IEOR(QLo, MinI64)) THEN
            QHi = QHi - 1_I8B
            R = R + D
        END IF

        IF (IEOR(R, MinI64) >= IEOR(D, MinI64)) THEN
            QHi = QHi + 1_I8B
            R = R - D
        END IF
        Q = QHi

        RETURN

    END SUBROUTINE UDivRem_2By1

    !******************************************************************************

    SUBROUTINE UDivRem_3By2(U2, U1, U0, DHi, DLo, V, Q, RHi, RLo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To performm 128-bit integer division by 64-bit integer

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        INTEGER(KIND=I8B), INTENT(IN)     :: U2, U1, U0, DHi, DLo, V
        INTEGER(KIND=I8B), INTENT(OUT)    :: Q, RHi, RLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        INTEGER(KIND=I8B)     :: QHi, QLo, NewLo, R1
        INTEGER(KIND=I8B)     :: THi, TLo, SHi, SLo
        LOGICAL    :: Flag

    !** FLOW

        ! Q128 = V*U2
        CALL UMul128(V, U2, QHi, QLo)

        ! Q128 = Q128 + UInt128(U2, U1)
        NewLo = QLo + U1
        IF (IEOR(NewLo, MinI64) < IEOR(QLo, MinI64)) THEN
            QHi = QHi + U2 + 1_I8B
        ELSE
            QHi = QHi + U2
        END IF
        QLo = NewLo

        R1 = U1 - QHi * DHi

        ! T128 = DLo*QHi
        CALL UMul128(DLo, QHi, THi, TLo)

        ! R128 = UInt128(R1, U0) - T128 - D128
        SLo  = U0 - TLo
        IF (IEOR(U0, MinI64) < IEOR(TLo, MinI64)) THEN
            SHi = R1 - THi - 1_I8B
        ELSE
            SHi = R1 - THi
        END IF
        RLo  = SLo - DLo
        IF (IEOR(SLo, MinI64) < IEOR(DLo, MinI64)) THEN
            RHi = SHi - DHi - 1_I8B
        ELSE
            RHi = SHi - DHi
        END IF

        R1 = RHi

        QHi = QHi + 1_I8B

        IF (R1 .UGE. QLo) THEN
            QHi = QHi - 1_I8B
            ! R128 = R128 + D128
            NewLo = RLo + DLo
            IF (IEOR(NewLo, MinI64) < IEOR(RLo, MinI64)) THEN
                RHi = RHi + DHi + 1_I8B
            ELSE
                RHi = RHi + DHi
            END IF
            RLo = NewLo
        END IF

        IF (RHi == DHi) THEN
            Flag = (IEOR(RLo, MinI64) >= IEOR(DLo, MinI64))
        ELSE
            Flag = (IEOR(RHi, MinI64) >= IEOR(DHi, MinI64))
        END IF
        IF (Flag) THEN
            QHi = QHi + 1_I8B
            ! R128 = R128 - D128
            NewLo = RLo - DLo
            IF (IEOR(RLo, MinI64) < IEOR(DLo, MinI64)) THEN
                RHi = RHi - DHi - 1_I8B
            ELSE
                RHi = RHi - DHi
            END IF
            RLo = NewLo
        END IF

        Q = QHi

        RETURN

    END SUBROUTINE UDivRem_3By2

!******************************************************************************

END SUBROUTINE U128_DivMod_IntX

!------------------------------------------------------------------------------
!
!                           AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE U128_Write(U128, Unit, IOStat, IOMsg, ShowComponent, Prefix)

!** PURPOSE OF THIS SUBROUTINE:
    !! To write 'UInt128' object to the screen (or the specified unit).

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),                INTENT(IN)     :: U128             !! UInt128 object
    INTEGER(KIND=I4B),  OPTIONAL, INTENT(IN)     :: Unit            !! output logical unit
    INTEGER(KIND=I4B),  OPTIONAL, INTENT(OUT)    :: IOStat          !! io stat
    CHARACTER(LEN=*),   OPTIONAL, INTENT(OUT)    :: IOMsg           !! io message
    LOGICAL,            OPTIONAL, INTENT(IN)     :: ShowComponent
    !^ flag indicating whether to show components or not  
    ! if flag is present and true, write compoents of the object  
    ! otherwise, write the object as a decimal string
    CHARACTER(LEN=*),   OPTIONAL, INTENT(IN)     :: Prefix          !! prefix string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    LOGICAL                         :: AsString
    INTEGER(KIND=I4B)               :: OutUnit
    INTEGER(KIND=I4B)               :: IO_Stat
    CHARACTER(LEN=128)              :: IO_Msg
    CHARACTER(LEN=:), ALLOCATABLE   :: DispStr

!** FLOW

    ! set defaults
    OutUnit  = OUTPUT_UNIT
    AsString = TrueVal

    ! check optional input
    IF (PRESENT(ShowComponent)) THEN
        IF (ShowComponent) AsString = FalseVal
    END IF
    IF (PRESENT(Unit)) OutUnit = Unit

    ! write the object
    IF (AsString) THEN
        IF (PRESENT(Prefix)) THEN
            DispStr = Prefix // ToDecString(U128)
        ELSE
            DispStr = ' U128 = ' // ToDecString(U128)
        END IF
        WRITE(UNIT=OutUnit, FMT='(A)', IOSTAT=IO_Stat, IOMSG=IO_Msg) DispStr
    ELSE
        DispStr = '-: '
        IF (PRESENT(Prefix)) DispStr = Prefix
        WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
              DispStr // 'High value = ', U128%High
        WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
              DispStr // 'Low value = ', U128%Low
    END IF

    ! return output if requested
    IF (PRESENT(IOStat)) IOStat = IO_Stat
    IF (PRESENT(IOMsg))  IOMsg  = IO_Msg

    RETURN

END SUBROUTINE U128_Write

!******************************************************************************

END MODULE ModBase_UInt128

!******************************************************************************
