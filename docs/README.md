# jj\_big\_number_64
Eiffel library for manipulating arbitrarily large numbers.  It implements addition, subtraction,  multiplication and division.

To used the example ecf files, checkout the library to a location identified by the envirnment variable "JJ_GITHUB".

### Demo/Test/Timing
The [demo](../demo) program and the [timing tests](../timing) use the Eiffel_GMP library, which depends on [The GNU Multiple Precision Arithmetic Library](https://gmplib.org), in order to compare results.  The demo program uses an override of class ROUTINE which changes the export status of feature *closed\_operands*, making the output routines in the test class easier to program.

The [timing tests](../timing) tests depend on the [JJ\_TEMPORAL](http://github.com/boxer41a/jj_temporal) classes.


### Timing Tests
Directory "timing" contains a class that compares execution times for this library to the execution times of corresponding routines in "eiffel_gmp" (an Eiffel binding to the GNU Multiple Precision Arithmetic Library).

In my ecf files I used environment variable "JJ_OTHER" to identify the file-system location for non-eiffel libraries or eiffel libraries that depend on non-eiffel libraries.