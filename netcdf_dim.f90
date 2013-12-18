!TODO:
! Error checking
! Preprocessor directives
module netcdf_dim
  !<DD> A module containing the netcdf dimension type definition
  !and associated routines.
  !</DD>
  use netcdf, only: NF90_UNLIMITED
  implicit none
  public :: ncdf_dim_type, dim_max_name_len, dim_complex_name
  private
  integer,parameter :: dim_max_name_len=6 !Longest name allowed
  character(len=2),parameter :: dim_complex_name='ri' !What do we call the 
  !special dimension used to hold real and imaginary components?

  type :: ncdf_dim_type
     integer :: id !The id to use in calls to netcdf routines
     integer :: size !The dimension size
     character(len=dim_max_name_len) :: name
   contains
     private
     procedure,public :: init !Basic initialisation
     procedure,public :: free !Free up memory
     procedure,public :: is_unlimited !Is this an unlimited dimension?
     procedure,public :: get_size !How big is this dimension
  end type ncdf_dim_type

contains
  !>Initialise the netcdf dimension object
  subroutine init(self,name,size,unlimited)
    implicit none
    class(ncdf_dim_type), intent(inout) :: self
    character(len=*), intent(in) :: name
    integer,intent(in),optional :: size 
    logical,intent(in),optional :: unlimited
    logical :: size_set
    !Initialise variables
    self%name=trim(name)
    size_set=.false.

    !Set the size, if we specify unlimited
    !then we disregard the size variable
    if(present(unlimited))then
       if(unlimited) then 
          self%size=NF90_UNLIMITED
          size_set=.true.
       endif
    endif
    if(.not.size_set)then
       if(present(size))then
          self%size=size
          size_set=.true.
       else
          !Perhaps we should stop here, at the very least print an error
          print*,"Error: Attempted to make a dimension without a size, setting to unlimited but could cause problems."
          self%size=NF90_UNLIMITED
       endif
    endif
  end subroutine init

  !Free objects
  subroutine free(self)
    implicit none
    class(ncdf_dim_type), intent(inout) :: self
    !Nothing to do for this object at the moment
  end subroutine free

  !Check if this is an unlimited dimension
  function is_unlimited(self)
    implicit none
    class(ncdf_dim_type), intent(in) :: self
    logical :: is_unlimited
    is_unlimited=(self%size.eq.NF90_UNLIMITED)
  end function is_unlimited

  !Check dimension length
  function get_size(self,ncid)
    use netcdf, only: nf90_inquire_dimension
    implicit none
    class(ncdf_dim_type), intent(in) :: self
    integer, intent(in) :: ncid
    integer :: get_size,status
    status=nf90_inquire_dimension(ncid,self%id,len=get_size)
  end function get_size
end module netcdf_dim
