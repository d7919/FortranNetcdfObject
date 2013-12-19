FortranNetcdfObject
===================

A collection of routines to provide a simplified object orientated approach to creating and writing simple netcdf files.

The file nc_test_prog.f90 contains an example usage.

The routines have not been well tested yet and there may stil be bugs to fix

##Compilation
To build the netcdf object one needs to compile in the following order:

1. nc_utils
2. netcdf_dim
3. netcdf_var
4. netcdf_file
5. *optional* nc_test_prog


##Todo
* Add in attributes.
* Add support for parallel io.
* Generalise treatment of unlimited dimensions.
* Rationalise repeated code.
* Add some error checking.
* Better error message handling.
* Preprocessor directives?
* Convert logical_to_int to an elemental function.
* The ability to populate a file object from an existing file (i.e. to read in data).
* Add a simple makefile to produce a library which can be linked.
