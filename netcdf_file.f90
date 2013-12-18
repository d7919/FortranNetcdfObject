!TODO:
! Error checking
! Preprocessor directives
module netcdf_file
  use netcdf_dim, only: ncdf_dim_type
  use netcdf_var, only: ncdf_var_type
  use netcdf, only: nf90_put_var
  implicit none
  public :: ncdf_file_type, file_max_name_len
  private
  integer, parameter :: file_max_name_len=200 !The maximum allowed filename length
  integer :: ndim_expand=5 !How much we increase self%dims when required
  integer :: nvar_expand=5 !How much we increase self%vars when required

  !<DD> A module containing the netcdf file type definition
  !and associated routines.
  !</DD>

  interface logical_to_int
     module procedure :: log2int_s,log2int_1,log2int_2,log2int_3
     module procedure :: log2int_4,log2int_5,log2int_6,log2int_7
  end interface logical_to_int

  type :: ncdf_file_type
     type(ncdf_dim_type), dimension(:), allocatable :: dims !The dimensions
     type(ncdf_var_type), dimension(:), allocatable :: vars !The variables
     integer :: id !The file id to use in calls to netcdf routines
     character(len=file_max_name_len) :: name
     !Meta data
     integer :: unlim_dim=1
     integer :: ndim, nvar
     logical :: def_mode, opened
   contains
     private
     procedure,public :: init !Basic initialisation
     procedure,public :: free !Free up memory
     procedure,public :: set_name !Set the filename
     procedure,public :: create_and_define !Make the file and define variables/dims
     procedure,public :: enddef !Exit definition mode
     procedure,public :: close_file !Close the file
     procedure,public :: create_dim !Creates and adds a dimension
     procedure,public :: create_var !Creates and adds a variable
     procedure,public :: add_dim !Attach a premade dimension
     procedure,public :: add_var !Attach a premade variable
     procedure :: write_comp_s,write_comp_1,write_comp_2,write_comp_3,write_comp_4,write_comp_5,write_comp_6,write_real_s,write_real_1,write_real_2,write_real_3,write_real_4,write_real_5,write_real_6,write_real_7,write_int_s,write_int_1,write_int_2,write_int_3,write_int_4,write_int_5,write_int_6,write_int_7,write_char_s,write_char_1,write_char_2,write_char_3,write_char_4,write_char_5,write_char_6,write_char_7,write_logi_s,write_logi_1,write_logi_2,write_logi_3,write_logi_4,write_logi_5,write_logi_6,write_logi_7
     generic,public::write_var=>write_comp_s,write_comp_1,write_comp_2,write_comp_3,write_comp_4,write_comp_5,write_comp_6,write_real_s,write_real_1,write_real_2,write_real_3,write_real_4,write_real_5,write_real_6,write_real_7,write_int_s,write_int_1,write_int_2,write_int_3,write_int_4,write_int_5,write_int_6,write_int_7,write_char_s,write_char_1,write_char_2,write_char_3,write_char_4,write_char_5,write_char_6,write_char_7,write_logi_s,write_logi_1,write_logi_2,write_logi_3,write_logi_4,write_logi_5,write_logi_6,write_logi_7
     procedure :: define_dim !Defines a passed dimension in the file
     procedure :: define_var !Defines a passed variable in the file
     procedure :: has_dim !Check if we have a named dim
     procedure :: has_var !Check if we have a named var
     procedure :: get_dim_id !Get the dimension id corresponding to a named dim
     procedure :: get_dim_index !Get the index corresponding to a named dimension
     procedure :: get_var_index !Get the index corresponding to a named variable
     procedure :: enable_complex !Creates the complex dimension
     procedure,public :: flush_file !Flush the output
     procedure,public :: increment_unlim !Increments the size of the unlimited dimension 
  end type ncdf_file_type

contains
  !>Initialise the netcdf file object
  subroutine init(self,ndim,nvar)
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    integer, intent(in), optional :: ndim,nvar

    !If we've passed a dim/var number then preallocate
    !else we need either a linked list or temporaries.
    if(present(ndim)) allocate(self%dims(ndim))
    if(present(nvar)) allocate(self%vars(nvar))

    !Initialise variables
    call self%set_name("input.out.nc")
    self%ndim=0
    self%nvar=0
  end subroutine init

  !Free objects
  subroutine free(self)
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    integer :: i
    if(allocated(self%dims))then
       do i=1,self%ndim
          call self%dims(i)%free
       enddo
       deallocate(self%dims)
    endif
    if(allocated(self%vars))then
       do i=1,self%nvar
          call self%vars(i)%free
       enddo
       deallocate(self%vars)
    endif
  end subroutine free

  !Make the netcdf file, define the dimensions and variables
  subroutine create_and_define(self)
    use netcdf, only: NF90_CLOBBER, nf90_create
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    integer :: status, i
    !Open the file
    status=nf90_create(trim(self%name),NF90_CLOBBER,self%id)

    !Set the def mod
    self%def_mode=.true.

    !Now define the dimensions
    do i=1,self%ndim
       status=self%define_dim(self%dims(i))
    enddo

    !Now define the variables
    do i=1,self%nvar
       status=self%define_var(self%vars(i))
    enddo
  end subroutine create_and_define
  
  !Close the file
  subroutine close_file(self)
    use netcdf, only: nf90_close
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    integer :: status
    status=nf90_close(self%id)
  end subroutine close_file

  !Set the filename
  subroutine set_name(self,name)
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    character(len=*), intent(in) :: name
    self%name=trim(name)
    self%opened=.false.
    self%def_mode=.false.
  end subroutine set_name

  !Exit def mode
  subroutine enddef(self)
    use netcdf, only: nf90_enddef
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    integer :: status
    if(.not.self%def_mode) return
    status=nf90_enddef(self%id)
    self%def_mode=.false.
  end subroutine enddef

  !Make the special dimension used for complex variables
  subroutine enable_complex(self)
    use netcdf_dim, only: dim_complex_name
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    if(self%has_dim(dim_complex_name)) return
    call self%create_dim(name=dim_complex_name,size=2)
  end subroutine enable_complex

  subroutine increment_unlim(self,inc)
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    integer, intent(in), optional :: inc
    if(present(inc))then
       self%unlim_dim=self%unlim_dim+inc
    else
       self%unlim_dim=self%unlim_dim+1
    endif
  end subroutine increment_unlim

  !Flush the output
  subroutine flush_file(self)
    use netcdf, only: nf90_sync
    implicit none
    class(ncdf_file_type), intent(in) :: self
    integer :: status
    status=nf90_sync(self%id)
  end subroutine flush_file

  !/////////////////////
  !// DIMENSION RELATED 
  !/////////////////////

  !Define a dimension
  function define_dim(self,dim)
    use netcdf, only: nf90_def_dim
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    type(ncdf_dim_type), intent(inout) :: dim
    integer :: define_dim
    !Note dim%size could be NF90_UNLIMITED
    define_dim=nf90_def_dim(self%id,trim(dim%name),dim%size,dim%id)
  end function define_dim

  !>Attach a dimension
  subroutine add_dim(self,dim)
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    type(ncdf_dim_type), intent(in) :: dim
    type(ncdf_dim_type), dimension(:), allocatable :: dim_tmp
    integer :: sz
    !First check if we already have this dim, if so exit
    if(self%has_dim(dim%name)) return

    !Now check if self%dim is allocated, if not allocate it
    if(.not.allocated(self%dims)) allocate(self%dims(ndim_expand))
    
    !Increment dimension count
    self%ndim=self%ndim+1

    !Get size of self%dims and see if we need to make it
    !bigger, if we used a linked list we could avoid this
    sz=size(self%dims)
    !If we have more dims than available space we need to expand
    if(self%ndim>sz)then
       !Make temp storage
       allocate(dim_tmp(self%ndim-1))
       dim_tmp=self%dims

       !Expand internal storage
       deallocate(self%dims)
       allocate(self%dims(self%ndim+ndim_expand))

       !Replace data
       self%dims(1:self%ndim-1)=dim_tmp
    endif

    !Store dim
    self%dims(self%ndim)=dim
  end subroutine add_dim

  !>See if we have the current named dimension
  function has_dim(self,name)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    logical :: has_dim
    integer :: i
    has_dim=.false.
    !loop until we find a match. This is probably not the
    !most efficient way to do this
    do i=1,self%ndim
       if(trim(self%dims(i)%name).eq.trim(name)) then
          has_dim=.true.
          exit
       endif
    enddo
  end function has_dim

  !Find the dimension id corresponding to dimension name
  function get_dim_id(self,name)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*),intent(in) :: name
    integer :: get_dim_id, i
    get_dim_id=-100
    do i=1,self%ndim
       if(trim(self%dims(i)%name).eq.trim(name)) then
          get_dim_id=self%dims(i)%id
          exit
       endif
    enddo
    if(get_dim_id.eq.-100) then
       print*,"ERROR: No id found for dimension '",trim(name),"' -- Probably not added yet --> Have to halt."
       stop
    endif
  end function get_dim_id

  !Make a dimension and attach it to the file object
  subroutine create_dim(self,name,size,unlimited)
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    character(len=*), intent(in) :: name
    integer,intent(in),optional :: size 
    logical,intent(in),optional :: unlimited
    type(ncdf_dim_type) :: tmp
    !Initialise dimension
!<DD>
!NOTE: Size and unlimited are optional, I think the following statement is ok
!in cases where these variables are provided AND where they are not but this may
!be something which needs testing/changing
!</DD>
    call tmp%init(name=name,size=size,unlimited=unlimited)

    !Now add it to the file
    call self%add_dim(dim=tmp)

    !Now free the tmp dim
    call tmp%free
  end subroutine create_dim

  !Find the index of the dim corresponding to dimension name
  function get_dim_index(self,name)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*),intent(in) :: name
    integer :: get_dim_index, i
    get_dim_index=-100
    do i=1,self%ndim
       if(trim(self%dims(i)%name).eq.trim(name)) then
          get_dim_index=i
          exit
       endif
    enddo
    if(get_dim_index.eq.-100) then
       print*,"ERROR: No dimension found with name '",trim(name),"' --> Have to halt."
       stop
    endif
  end function get_dim_index

  !/////////////////////
  !// VARIABLE RELATED 
  !/////////////////////

  !Define a variable
  function define_var(self,var)
    use netcdf, only: nf90_def_var
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    type(ncdf_var_type), intent(inout) :: var
    integer :: define_var,i
    integer,dimension(:),allocatable :: dim_id
    !Not all variables have dimensions so we need to check
    !if this variable has dims or not.
    if(var%ndim.gt.0)then
       !Make array containing dim ids
       allocate(dim_id(var%ndim))
       do i=1,var%ndim
          dim_id(i)=self%get_dim_id(var%dim_names(i))
       enddo
       define_var=nf90_def_var(self%id,var%name,var%type,dim_id,var%id)
       deallocate(dim_id)
    else
       define_var=nf90_def_var(self%id,var%name,var%type,var%id)
    endif
    !Note we should loop over var attributes and attach them here
  end function define_var

  !>Attach a variable
  subroutine add_var(self,var)
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    type(ncdf_var_type), intent(in) :: var
    type(ncdf_var_type), dimension(:), allocatable :: var_tmp
    integer :: sz
    !First check if we already have this var, if so exit
    if(self%has_var(var%name)) return

    !Now check if self%dim is allocated, if not allocate it
    if(.not.allocated(self%vars)) allocate(self%vars(nvar_expand))
    
    !Increment dimension count
    self%nvar=self%nvar+1

    !Get size of self%dims and see if we need to make it
    !bigger, if we used a linked list we could avoid this
    sz=size(self%vars)
    !If we have more dims than available space we need to expand
    if(self%nvar>sz)then
       !Make temp storage
       allocate(var_tmp(self%nvar-1))
       var_tmp=self%vars

       !Expand internal storage
       deallocate(self%vars)
       allocate(self%vars(self%nvar+nvar_expand))

       !Replace data
       self%vars(1:self%nvar-1)=var_tmp
    endif

    !Store var
    self%vars(self%nvar)=var
  end subroutine add_var

  !>See if we have the current named variable
  function has_var(self,name)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    logical :: has_var
    integer :: i
    has_var=.false.
    !loop until we find a match. This is probably not the
    !most efficient way to do this
    do i=1,self%nvar
       if(trim(self%vars(i)%name).eq.trim(name)) then
          has_var=.true.
          exit
       endif
    enddo
  end function has_var

  !Find the index of the var corresponding to variable name
  function get_var_index(self,name)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*),intent(in) :: name
    integer :: get_var_index, i
    get_var_index=-100
    do i=1,self%nvar
       if(trim(self%vars(i)%name).eq.trim(name)) then
          get_var_index=i
          exit
       endif
    enddo
    if(get_var_index.eq.-100) then
       print*,"ERROR: No variable found with name '",trim(name),"' --> Have to halt."
       stop
    endif
  end function get_var_index

  !Make a variable and attach it to the file object
  subroutine create_var(self,name,dims,type_int,type_str,complx)
    use netcdf_dim, only: dim_max_name_len
    implicit none
    class(ncdf_file_type), intent(inout) :: self
    type(ncdf_var_type) :: tmp
    character(len=*), intent(in) :: name
    character(len=*),dimension(:),intent(in),optional :: dims
    integer,intent(in),optional :: type_int
    character(len=*),intent(in),optional :: type_str
    logical, intent(in), optional :: complx
    character(len=dim_max_name_len),dimension(:),allocatable::loc_dim
    !Handle the case where we don't have dimensions
    if(present(dims)) then
       allocate(loc_dim(size(dims)))
       loc_dim=dims
    else
       allocate(loc_dim(1))
       loc_dim(1)=''
    endif
    
    !Initialise variable
!<DD>
!NOTE: Several args are optional, I think the following statement is ok
!in cases where these variables are provided AND where they are not but this may
!be something which needs testing/changing
!</DD>
    call tmp%init(name=name,dims=loc_dim,type_int=type_int,&
         type_str=type_str,complx=complx)

    !Deallocate
    deallocate(loc_dim)

    !Now add it to the file
    call self%add_var(var=tmp)

    !Enable complex if variable is complex
    if(tmp%is_complex) call self%enable_complex

    !Now free the tmp var
    call tmp%free
  end subroutine create_var

  !/////////////////////
  !// WRITE VAR RELATED
  !/////////////////////

  !Tedious duplicatation of write routines for different types and ranks

  !##########
  !##COMPLEX
  !##########

  subroutine write_comp_s(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    complex, intent(in) :: dat
    real,dimension(2) :: rdat
    !Convert dat to rdat
    rdat(1)=dble(dat) ; rdat(2)=aimag(dat)

    !Pass data onto variable write routine
    call self%write_var(name,rdat)
  end subroutine write_comp_s

  subroutine write_comp_1(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    complex,dimension(:), intent(in) :: dat
    real,dimension(:,:),allocatable :: rdat
    !Convert dat to rdat
    allocate(rdat(2,size(dat,1)))
    rdat(1,:)=dble(dat) ; rdat(2,:)=aimag(dat)

    !Pass data onto variable write routine
    call self%write_var(name,rdat)
    deallocate(rdat)
  end subroutine write_comp_1

  subroutine write_comp_2(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    complex,dimension(:,:), intent(in) :: dat
    real,dimension(:,:,:),allocatable :: rdat
    !Convert dat to rdat
    allocate(rdat(2,size(dat,1),size(dat,2)))
    rdat(1,:,:)=dble(dat) ; rdat(2,:,:)=aimag(dat)

    !Pass data onto variable write routine
    call self%write_var(name,rdat)
    deallocate(rdat)
  end subroutine write_comp_2

  subroutine write_comp_3(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    complex,dimension(:,:,:), intent(in) :: dat
    real,dimension(:,:,:,:),allocatable :: rdat
    !Convert dat to rdat
    allocate(rdat(2,size(dat,1),size(dat,2),size(dat,3)))
    rdat(1,:,:,:)=dble(dat) ; rdat(2,:,:,:)=aimag(dat)

    !Pass data onto variable write routine
    call self%write_var(name,rdat)
    deallocate(rdat)
  end subroutine write_comp_3

  subroutine write_comp_4(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    complex,dimension(:,:,:,:), intent(in) :: dat
    real,dimension(:,:,:,:,:),allocatable :: rdat
    integer :: v_index, status
    !Convert dat to rdat
    allocate(rdat(2,size(dat,1),size(dat,2),size(dat,3),size(dat,4)))
    rdat(1,:,:,:,:)=dble(dat) ; rdat(2,:,:,:,:)=aimag(dat)

    !Pass data onto variable write routine
    call self%write_var(name,rdat)
    deallocate(rdat)
  end subroutine write_comp_4

  subroutine write_comp_5(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    complex,dimension(:,:,:,:,:), intent(in) :: dat
    real,dimension(:,:,:,:,:,:),allocatable :: rdat
    !Convert dat to rdat
    allocate(rdat(2,size(dat,1),size(dat,2),size(dat,3),size(dat,4),size(dat,5)))
    rdat(1,:,:,:,:,:)=dble(dat) ; rdat(2,:,:,:,:,:)=aimag(dat)

    !Pass data onto variable write routine
    call self%write_var(name,rdat)
    deallocate(rdat)
  end subroutine write_comp_5

  subroutine write_comp_6(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    complex,dimension(:,:,:,:,:,:), intent(in) :: dat
    real,dimension(:,:,:,:,:,:,:),allocatable :: rdat
    !Convert dat to rdat
    allocate(rdat(2,size(dat,1),size(dat,2),size(dat,3),size(dat,4),size(dat,5),size(dat,6)))
    rdat(1,:,:,:,:,:,:)=dble(dat) ; rdat(2,:,:,:,:,:,:)=aimag(dat)

    !Pass data onto variable write routine
    call self%write_var(name,rdat)
    deallocate(rdat)
  end subroutine write_comp_6

  !##########
  !##REAL
  !##########

  subroutine write_real_s(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    real, intent(in) :: dat
    integer :: v_index,status
    integer :: dim_in=size(shape(dat)),nd
    logical :: with_start
    !Get variable index
    v_index=self%get_var_index(name)

    !Validate data (note this is only valid for scalars)
    nd=self%vars(v_index)%ndim

    !Work out if we want start or not
    with_start=.false.
    if(nd.ne.dim_in)then
       if(self%dims(self%get_dim_id(self%vars(v_index)%dim_names(1)))%is_unlimited())then
          with_start=.true.
       endif
    endif

    !Put variable
    if(with_start)then
       status=nf90_put_var(self%id,self%vars(v_index)%id,(/dat/),start=(/self%unlim_dim/),count=(/1/))
    else
       status=nf90_put_var(self%id,self%vars(v_index)%id,dat)
    endif
  end subroutine write_real_s

  subroutine write_real_1(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    real,dimension(:), intent(in) :: dat
    real :: tdat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp
    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_real_1

  subroutine write_real_2(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    real,dimension(:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0

    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_real_2

  subroutine write_real_3(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    real,dimension(:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_real_3

  subroutine write_real_4(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    real,dimension(:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_real_4

  subroutine write_real_5(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    real,dimension(:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_real_5

  subroutine write_real_6(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    real,dimension(:,:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_real_6

  subroutine write_real_7(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    real,dimension(:,:,:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_real_7

  !##########
  !##INTEGER
  !##########

  subroutine write_int_s(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    integer, intent(in) :: dat
    integer :: v_index,status
    integer :: dim_in=size(shape(dat)),nd
    logical :: with_start
    !Get variable index
    v_index=self%get_var_index(name)

    !Validate data (note this is only valid for scalars)
    nd=self%vars(v_index)%ndim

    !Work out if we want start or not
    with_start=.false.
    if(nd.ne.dim_in)then
       if(self%dims(self%get_dim_id(self%vars(v_index)%dim_names(1)))%is_unlimited())then
          with_start=.true.
       endif
    endif

    !Put variable
    if(with_start)then
       status=nf90_put_var(self%id,self%vars(v_index)%id,(/dat/),start=(/self%unlim_dim/),count=(/1/))
    else
       status=nf90_put_var(self%id,self%vars(v_index)%id,dat)
    endif
  end subroutine write_int_s

  subroutine write_int_1(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    integer,dimension(:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_int_1

  subroutine write_int_2(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    integer,dimension(:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_int_2

  subroutine write_int_3(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    integer,dimension(:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_int_3

  subroutine write_int_4(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    integer,dimension(:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_int_4

  subroutine write_int_5(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    integer,dimension(:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_int_5

  subroutine write_int_6(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    integer,dimension(:,:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_int_6

  subroutine write_int_7(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    integer,dimension(:,:,:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_int_7

  !##########
  !##CHARACTER
  !##########

  subroutine write_char_s(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: dat
    integer :: v_index,status
    integer :: dim_in=size(shape(dat)),nd
    logical :: with_start
    !Get variable index
    v_index=self%get_var_index(name)

    !Validate data (note this is only valid for scalars)
    nd=self%vars(v_index)%ndim

    !Work out if we want start or not
    with_start=.false.
    if(nd.ne.dim_in)then
       if(self%dims(self%get_dim_id(self%vars(v_index)%dim_names(1)))%is_unlimited())then
          with_start=.true.
       endif
    endif

    !Put variable
    if(with_start)then
       status=nf90_put_var(self%id,self%vars(v_index)%id,(/dat/),start=(/self%unlim_dim/),count=(/1/))
    else
       status=nf90_put_var(self%id,self%vars(v_index)%id,dat)
    endif
  end subroutine write_char_s

  subroutine write_char_1(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=*),dimension(:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=self%dims(d_index)%get_size(self%id)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_char_1

  subroutine write_char_2(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=*),dimension(:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=len(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=self%dims(d_index)%get_size(self%id)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_char_2

  subroutine write_char_3(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=*),dimension(:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=self%dims(d_index)%get_size(self%id)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_char_3

  subroutine write_char_4(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=*),dimension(:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=self%dims(d_index)%get_size(self%id)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_char_4

  subroutine write_char_5(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=*),dimension(:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=self%dims(d_index)%get_size(self%id)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_char_5

  subroutine write_char_6(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=*),dimension(:,:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=self%dims(d_index)%get_size(self%id)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_char_6

  subroutine write_char_7(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=*),dimension(:,:,:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=self%dims(d_index)%get_size(self%id)
       endif
    enddo
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,dat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_char_7

  !##########
  !##LOGICAL
  !##########

  subroutine write_logi_s(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    logical, intent(in) :: dat
    integer :: v_index,status
    integer :: dim_in=size(shape(dat)),nd
    logical :: with_start
    integer :: idat
    !Get variable index
    v_index=self%get_var_index(name)

    !Validate data (note this is only valid for scalars)
    nd=self%vars(v_index)%ndim

    !Work out if we want start or not
    with_start=.false.
    if(nd.ne.dim_in)then
       if(self%dims(self%get_dim_id(self%vars(v_index)%dim_names(1)))%is_unlimited())then
          with_start=.true.
       endif
    endif

    !Convert logical to integer
    call logical_to_int(dat,idat)

    !Put variable
    if(with_start)then
       status=nf90_put_var(self%id,self%vars(v_index)%id,(/idat/),start=(/self%unlim_dim/),count=(/1/))
    else
       status=nf90_put_var(self%id,self%vars(v_index)%id,idat)
    endif
  end subroutine write_logi_s

  subroutine write_logi_1(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    logical,dimension(:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp
    integer, dimension(:), allocatable :: idat
    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    allocate(idat(shp(1)))
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo


    !Convert logical to integer
    call logical_to_int(dat,idat)
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,idat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_logi_1

  subroutine write_logi_2(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    logical,dimension(:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp
    integer, dimension(:,:), allocatable :: idat
    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    allocate(idat(shp(1),shp(2)))
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Convert logical to integer
    call logical_to_int(dat,idat)

    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,idat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_logi_2

  subroutine write_logi_3(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    logical,dimension(:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp
    integer, dimension(:,:,:), allocatable :: idat
    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    allocate(idat(shp(1),shp(2),shp(3)))
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
        
    !Convert logical to integer
    call logical_to_int(dat,idat)

    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,idat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_logi_3

  subroutine write_logi_4(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    logical,dimension(:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp
    integer, dimension(:,:,:,:), allocatable :: idat

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    allocate(idat(shp(1),shp(2),shp(3),shp(4)))
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
        
    !Convert logical to integer
    call logical_to_int(dat,idat)

    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,idat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_logi_4

  subroutine write_logi_5(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    logical,dimension(:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp
    integer, dimension(:,:,:,:,:), allocatable :: idat
    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    allocate(idat(shp(1),shp(2),shp(3),shp(4),shp(5)))
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
        
    !Convert logical to integer
    call logical_to_int(dat,idat)

    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,idat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_logi_5

  subroutine write_logi_6(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    logical,dimension(:,:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp
    integer, dimension(:,:,:,:,:,:), allocatable :: idat

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    allocate(idat(shp(1),shp(2),shp(3),shp(4),shp(5),shp(6)))
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
    
    !Convert logical to integer
    call logical_to_int(dat,idat)
    
    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,idat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_logi_6

  subroutine write_logi_7(self,name,dat)
    implicit none
    class(ncdf_file_type), intent(in) :: self
    character(len=*), intent(in) :: name
    logical,dimension(:,:,:,:,:,:,:), intent(in) :: dat
    integer :: v_index,d_index,status,nd,i,ct
    integer :: dim_in=size(shape(dat))
    integer, dimension(:),allocatable :: starts,counts,shp
    integer, dimension(:,:,:,:,:,:,:),allocatable :: idat

    !Get variable index
    v_index=self%get_var_index(name)

    !Initialise starts and counts array
    nd=self%vars(v_index)%ndim
    allocate(starts(nd),counts(nd),shp(dim_in))
    starts=1
    shp=shape(dat)
    allocate(idat(shp(1),shp(2),shp(3),shp(4),shp(5),shp(6),shp(7)))
    ct=0
    !Set the counts and handle the unlimited dimension
    do i=1,nd
       d_index=self%get_dim_index(self%vars(v_index)%dim_names(i))
       !If this is an unlimited dimension then we need to change start and counts
       if(self%dims(d_index)%is_unlimited())then
          counts(i)=1
          starts(i)=self%unlim_dim !<DD>TO FIX
       else
          ct=ct+1
          counts(i)=shp(ct)
       endif
    enddo
        
    !Convert logical to integer
    call logical_to_int(dat,idat)

    !Put variable
    status=nf90_put_var(self%id,self%vars(v_index)%id,idat,start=starts,count=counts)

    !Tidy
    deallocate(starts,counts)
  end subroutine write_logi_7
  
  !/////////////////
  !//UTILS
  !/////////////////
  subroutine log2int_s(dat,idat)
    implicit none
    logical, intent(in) :: dat
    integer, intent(out) :: idat
    if(dat)then
       idat=1
    else
       idat=0
    endif
  end subroutine log2int_s

  subroutine log2int_1(dat,idat)
    implicit none
    logical, dimension(:), intent(in) :: dat
    integer, dimension(:), intent(out) :: idat
    where(dat)
       idat=1
    elsewhere
       idat=0
    end where
  end subroutine log2int_1

  subroutine log2int_2(dat,idat)
    implicit none
    logical, dimension(:,:), intent(in) :: dat
    integer, dimension(:,:), intent(out) :: idat
    where(dat)
       idat=1
    elsewhere
       idat=0
    end where
  end subroutine log2int_2

  subroutine log2int_3(dat,idat)
    implicit none
    logical, dimension(:,:,:), intent(in) :: dat
    integer, dimension(:,:,:), intent(out) :: idat
    where(dat)
       idat=1
    elsewhere
       idat=0
    end where
  end subroutine log2int_3

  subroutine log2int_4(dat,idat)
    implicit none
    logical, dimension(:,:,:,:), intent(in) :: dat
    integer, dimension(:,:,:,:), intent(out) :: idat
    where(dat)
       idat=1
    elsewhere
       idat=0
    end where
  end subroutine log2int_4

  subroutine log2int_5(dat,idat)
    implicit none
    logical, dimension(:,:,:,:,:), intent(in) :: dat
    integer, dimension(:,:,:,:,:), intent(out) :: idat
    where(dat)
       idat=1
    elsewhere
       idat=0
    end where
  end subroutine log2int_5

  subroutine log2int_6(dat,idat)
    implicit none
    logical, dimension(:,:,:,:,:,:), intent(in) :: dat
    integer, dimension(:,:,:,:,:,:), intent(out) :: idat
    where(dat)
       idat=1
    elsewhere
       idat=0
    end where
  end subroutine log2int_6

  subroutine log2int_7(dat,idat)
    implicit none
    logical, dimension(:,:,:,:,:,:,:), intent(in) :: dat
    integer, dimension(:,:,:,:,:,:,:), intent(out) :: idat
    where(dat)
       idat=1
    elsewhere
       idat=0
    end where
  end subroutine log2int_7
end module netcdf_file
