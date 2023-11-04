# FortCharConv
Fortran Character String-Number Conversion

FortCharConv is a Fortran libray that provides routines to convert between number and character string.

Types of the number that can be converted include 32-bit and 64-bit integers and 32-bit, 64-bit and 128-bit floating-point numbers (reals)

Files are organized into three main folders.

The 'Cores' folder contains five core files that perform the string-number conversions.
	- 'ModBase - Integer - FromChar' file provides routines to convert a decimal string into an integer value.
	- 'ModBase - Integer - ToChar' file provides routines to convert an integer value into a decimal string.
	- 'ModBase - RealSP - CharConv' file provides routines to convert between a 32-bit real value and a string.
	- 'ModBase - RealDP - CharConv' file provides routines to convert between a 64-bit real value and a string.
	- 'ModBase - RealQP - CharConv' file provides routines to convert between a 128-bit real value and a string.
Another file ('ModBase - Tables - CharConv') provides a module that contains look-up tables used by those five core modules.

The 'Basics' folder contains auxiliary files that provide support to the core routines.  For those interested in implementing 'unsigned integer' in Fortran,
'ModBase - UIntUtil' file provides routines that perform logical and arithmetic operations for unsigned integers. Note that those logical and arithmetic operations that are NOT provides are those that are applicable to both signed and unsigned integers.  The file also provides various utility routines relating to unsigned integers.  As an example of how to implement 'unsigned integer' in Fortran, 'ModBase - UInt128' file contains a module that provide a derived type and various operations for an unsigned 128-bit integer.

The 'Tests' folder contains files that perform verification and bench tests.  The 'Tests' folder also contains a subfolder named 'Supports' that provides auxiliary files that are required by the test modules.

No separate documentation is provided.  See detailed explanations of algoritms implemented and usage of routines in the source code.

The following list provides compilation order of the library.
- ModBase - Common
- ModBase - Error Handlers
- ModBase - SIntUtil
- ModBase - UIntUtil
- ModBase - UInt128
- ModBase - SInt128
- ModBase_Tables_CharConv
- ModBase - Integer - FromChar
- ModBase_Integer_ToChar
- ModBase_RealSP_CharConv
- ModBase_RealDP_CharConv
- ModBase_RealQP_CharConv

The following list provides compilation order of test files of the library.
- Class - Timer
- Class - ProgressBar
- Class - BaseRNG
- SubClass - Rng Auxiliary
- SubClass - Rng Ziggurat
- Class - IntegerRNG
- Class - LongRNG
- Class - Mt32RNG
- Class - Mt64RNG
- ModTest - RealQP - CharConv
- ModTest - RealDP - CharConv
- ModTest - RealSP - CharConv

License:
The source code of this library can be licensed under either of Apache, Boost or MIT license.  See 'Note on Licences'.