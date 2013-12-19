FortranNetcdfObject
===================

A collection of routines to provide a simplified object orientated approach to creating and writing simple netcdf files.

The file nc_test_prog.f90 contains an example usage.

The routines have not been well tested yet and there may still be bugs to fix

##Compilation
To build the netcdf object one needs to compile in the following order:

1. nc_utils
2. netcdf_dim
3. netcdf_var
4. netcdf_file
5. *optional* nc_test_prog

##Basic usage
This is probably best illustrated by the test program provided in nc_test_prog.
The basic procedure can be thought of as:

1. Create an instance of the ncdf_file_type type. This is done in the declaration phase.
2. Initialise the object with a call to the `init` bound procedure. This takes optional arguments ndim and nvar, these can be set if known to avoid some small overheads later on when adding variables and dimensions. It doesn't matter if these values are too small as we automatically extend the required storage when needed.
3. Set the file name by calling the `set_name` bound procedure.
4. Define the dimensions. This involves a call to `create_dim` for each dimensions, passing a name and either the size of the dimension or `unlimited=.true.` for the unlimited dimension. (Note, currently we only support one unlimited dimension per file object).
5. Define the variables by calling `create_var` for each. We need to provide a name and some indication of type (this can either be through a `type_str` or `type_int`, the str approach is recommended). We should also define the dimensions of the variable here by passing a character array giving the dimension names. If this is not provided the variable is assumed to be scalar. Trailing blanks are ignored in dimension names.
6. Create the file and commit the dimension and variable definitions to file by calling `create_and_define`.
7. Exit definition mode by calling `enddef`
8. Write fixed data from start of simulation using `write_var`, which takes the variable name and data.
9. When writing data to the unlimited dimension we want to do so for a single part of the unlimited dimension at once. Use `increment_unlim` to increment the unlimited dimension index (note the user can supply the increment, which could be negative). Use `set_unlim` to specify the current unlimited dimension index. Note we can optionally force a sync/flush of the data by calling `flush_file`.
10. Write any final data.
11. Close the file by calling `close_file`
12. Tidy up/free memory by calling `free`

##Todo
* Add in attributes.
* Add support for parallel io.
* Generalise treatment of unlimited dimensions.
* Rationalise repeated code.
* Add some error checking.
* Better error message handling.
* Preprocessor directives?
* The ability to populate a file object from an existing file (i.e. to read in data).
* Add a simple makefile to produce a library which can be linked.
