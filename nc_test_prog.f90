program tester
  use netcdf_file, only: ncdf_file_type
  implicit none
  type(ncdf_file_type) :: My_Output
  real,dimension(:,:),allocatable :: phi
  complex,dimension(:,:),allocatable :: cphi
  real,dimension(:),allocatable :: xx,yy,t
  integer :: nx=9,ny=26, ix,iy,i,nl=15
  character(len=200),dimension(2) :: mesg
  logical,dimension(:),allocatable :: logi
  !Now initialise the file
  call My_Output%init(ndim=2,nvar=5)
  call My_Output%set_name(name="test.nc")

  !Now define the dimensions
  call My_Output%create_dim(name='x',size=nx) !Note the dimension name can be longer than 1 char
  call My_Output%create_dim(name='y',size=ny) !but makes it tricky to use array constructors as
  call My_Output%create_dim(name='c',size=200)!it's not possible to use strings of different length
  call My_Output%create_dim(name='j  ',size=2)  !i.e. (/'c200','j'/) is not valid, though (/'c200','j   '/)
  call My_Output%create_dim(name='l',size=nl) !should be and the library should ignore the blanks.
  call My_Output%create_dim(name='t',unlimited=.true.)

  !Now define the variables
  call My_Output%create_var(name='phi',dims=(/'x','y'/),type_str='real')
  call My_Output%create_var(name='cphi',dims=(/'x','y'/),type_str='complex')
  call My_Output%create_var(name='phit',dims=(/'t'/),type_str='real')
  call My_Output%create_var(name='phit_x',dims=(/'x','t'/),type_str='real')
  call My_Output%create_var(name='x',dims=(/'x'/),type_str='real')
  call My_Output%create_var(name='y',dims=(/'y'/),type_str='real')
  call My_Output%create_var(name='logi',dims=(/'l'/),type_str='logical')
  call My_Output%create_var(name='nproc',type_str='int')
  call My_Output%create_var(name='mesg',dims=(/'c','j'/),type_str='char')
  call My_Output%create_var(name='mesg_short',dims=(/'c'/),type_str='char')
  
  !Create and define file
  call My_Output%create_and_define

  !Exit def mode
  call My_Output%enddef

  !Make data
  allocate(xx(nx),yy(ny),phi(nx,ny),logi(nl))
  do ix=1,nx
     xx(ix)=0.1*ix
  enddo
  do iy=1,ny
     yy(iy)=0.5*iy
  enddo
  do iy=1,nl
     logi(iy)=(1.eq.mod(iy,2))
  enddo
  do iy=1,ny
     do ix=1,nx
        phi(ix,iy)=sqrt(xx(ix)*yy(iy))
     enddo
  enddo
  cphi=cmplx(0,1.0)*phi

  !Now write the fixed data
  call My_Output%write_var('phi',phi)
  call My_Output%write_var('cphi',cphi)
  call My_Output%write_var('x',xx)
  call My_Output%write_var('logi',logi)
  call My_Output%write_var('y',yy)
  call My_Output%write_var('nproc',26)
  mesg(1)="Hello this is a message!"
  mesg(2)="Line 2"
  call My_Output%write_var('mesg',mesg)
  call My_Output%write_var('mesg_short',"Testing 2")
  
  !Demonstrate incremental writing of unlimited dimension
  do i=1,nx
     call My_Output%write_var('phit',i*1.001)
     call My_Output%write_var('phit_x',xx+i*1.0)
     call My_Output%increment_unlim !This advances the unlimited dimension
     if(mod(i,10).eq.0) call My_Output%flush_file() !Flush the output
  enddo
  call My_Output%set_unlim(1) !This sets the unlimited dimension value
  call My_Output%write_var('phit',-1.001)

  !Finish up
  call My_Output%close_file
  call My_Output%free
end program tester
