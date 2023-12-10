## FortCharConv:  Fortran Character String-Number Conversion

FortCharConv is a Fortran libray that provides routines to convert between number and character string.  Types of the number that can be converted include 32-bit and 64-bit integers and 32-bit, 64-bit and 128-bit floating-point numbers (reals).

### Source Code:

Files are organized into three main folders.

The **Cores** folder contains six core files that provides the string-number conversions.

- *ModBase - Integer - FromChar* file contains a module that  provides routines to convert a decimal string into an integer value.

- *ModBase - Integer - ToChar* file contains a module that provides routines to convert an integer value into a decimal string.

- *ModBase - RealSP - CharConv* file contains a module that provides routines to convert between a 32-bit real value and a decimal string.

- *ModBase - RealDP - CharConv* file contains a module that provides routines to convert between a 64-bit real value and a decimal string.

- *ModBase - RealQP - CharConv* file contains a module that provides routines to convert between a 128-bit real value and a decimal string. 

- *ModBase - Tables - CharConv* fiel contains a module that contains look-up tables used by the other five modules.

The **Basics** folder contains auxiliary files that provide support to the core routines.  For those interested in implementing ***unsigned integer*** in Fortran, the *ModBase - UIntUtil* file contains a module that provides routines to perform various tasks relating to unsigned integers.  As an illustration of how *unsigned integer* can be implemented in Fortran, the *ModBase - UInt128* file contains a module that provides a derived type and various operations for an unsigned 128-bit integer.

The **Tests** folder contains files (modules) that perform verification and bench tests.  The folder also contains a subfolder named **Supports** that provides auxiliary files that are required by the test modules.

### Documentation:

Use FORD program to automatically generate the documentation of *basic* and *core* routines via 'DocGen.md'.

### Compilation:

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

### License:

The source code of this library can be licensed under either of Apache, Boost or MIT license.  See *Note on Licences*.
