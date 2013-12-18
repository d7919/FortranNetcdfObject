!TODO:
! Attributes
! Error checking
! Preprocessor directives
module netcdf_var
  !<DD> A module containing the netcdf variable type definition
  !and associated routines.
  !</DD>

  use netcdf_dim, only: dim_max_name_len
  use nc_utils, only: kind_nf
  implicit none
  public :: ncdf_var_type, var_max_name_len
  private
  integer,parameter :: var_max_name_len=200 !Longest name allowed

  type :: ncdf_var_type
     !Could have an array of attributes here
     integer :: id !The id to use in calls to netcdf routines
     integer(kind_nf) :: type !The variable type
     character(len=var_max_name_len) :: name
     character(len=dim_max_name_len),dimension(:),allocatable :: dim_names
     !Meta data
     integer :: ndim
     logical :: is_complex
   contains
     private
     procedure,public :: init !Basic initialisation
     procedure,public :: free !Free up memory
  end type ncdf_var_type
contains
  !>Initialise the netcdf variable object
  subroutine init(self,name,dims,type_int,type_str,complx)
    use nc_utils, only : Type_Str_To_Int
    use netcdf_dim, only: dim_complex_name
    implicit none
    class(ncdf_var_type), intent(inout) :: self
    character(len=*), intent(in) :: name
    character(len=*),dimension(:),intent(in) :: dims
    integer,intent(in),optional :: type_int
    character(len=*),intent(in),optional :: type_str
    logical, intent(in), optional :: complx
    logical :: my_complex
    integer :: tmp,ndim,i,ct
    !Initialise the complex state
    my_complex=.false.
    if(present(complx)) my_complex=complx

    !Set the data type
    if(present(type_int))then
       self%type=type_int
    elseif(present(type_str))then
       self%type=Type_Str_To_Int(type_str,my_complex)
    else
       self%type=-10
    endif
    self%is_complex=my_complex

    !Count not empty dimensions
    ndim=0
    tmp=size(dims)
    do i=1,tmp
       if(trim(dims(i)).ne.'') ndim=ndim+1
    enddo

    !If we're complex then we need to add in a new dimension to 
    !represent the two components as netcdf doesn't have a native
    !complex data type
    if(self%is_complex) ndim=ndim+1

    !Initialise variables
    self%name=trim(name)
    self%ndim=ndim

    !If we have dimensions then save the names
    if(ndim.gt.0)then
       allocate(self%dim_names(ndim))
       ct=0
       !Add the complex dimension if required
       if(self%is_complex) then
          ct=ct+1
          self%dim_names(ct)=dim_complex_name
       endif

       do i=1,tmp
          if(trim(dims(i)).ne.'')then
             ct=ct+1
             self%dim_names(ct)=trim(dims(i))
          endif
       enddo
    endif
  end subroutine init

  !Free objects
  subroutine free(self)
    implicit none
    class(ncdf_var_type), intent(inout) :: self
    if(allocated(self%dim_names)) deallocate(self%dim_names)
  end subroutine free
end module netcdf_var
