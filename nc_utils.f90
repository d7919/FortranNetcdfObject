! Copyright (C) 2013 David Dickinson

! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

!TODO:
! Preprocessor directives
! Error handling
module nc_utils

  !<DD> A module to provide utility routines for netcdf operations
  !</DD>
  use netcdf, only: NF90_NOERR

  implicit none
  public :: Type_Str_To_Int, init_precision, kind_nf, netcdf_real
  public :: netcdf_byte, netcdf_char, netcdf_int
  private

!Setup the netcdf integer kind
  integer, parameter :: kind_nf = kind (NF90_NOERR)

  !Following used to cope with different precisions etc
  integer(kind_nf) :: netcdf_real, netcdf_int, netcdf_char, netcdf_byte

  !See if we've initialised
  logical :: initialised=.false.
contains
  !Convert a string to at netcdf type integer
  function Type_Str_To_Int(StrIn,complx)
    implicit none
    character(len=*), intent(in) :: StrIn
    logical,intent(out),optional :: complx
    integer :: Type_Str_To_Int
    !Initialise if needed
    if(.not.initialised) call init_precision

    !Initialise complex state
    if(present(complx)) complx=.false.
    !Note could do with converting to fixed case
    select case(trim(StrIn))
    case('real','r','R')
       Type_Str_To_Int=netcdf_real
    case('complex','c','C')
       Type_Str_To_Int=netcdf_real
       if(present(complx)) complx=.true.
    case('integer','int','i','I')
       Type_Str_To_Int=netcdf_int
    case('char','character','string','s','S')
       Type_Str_To_Int=netcdf_char
    case('byte','b','B')
       Type_Str_To_Int=netcdf_byte
    case('logical','l','L')
       Type_Str_To_Int=netcdf_int
    case default
       !Could do with better error handling here
       print*,"ERROR: Invalid variable type string --> Setting to real for now"
       Type_Str_To_Int=netcdf_real
    end select
  end function Type_Str_To_Int

  !Setup the netcdf type integers
  subroutine init_precision
    use netcdf, only: NF90_FLOAT,NF90_DOUBLE,NF90_INT,NF90_CHAR,NF90_BYTE
    implicit none
    integer, parameter :: kind_rs=selected_real_kind(p=6),kind_rd=selected_real_kind(p=12)
    double precision, parameter :: djnk=1.0000000000000000000000123456789
    real, parameter :: jnk=djnk 
    !Reals
    if ( (kind(jnk)==kind_rs) .or. (kind_rs==kind_rd) ) then
       netcdf_real = NF90_FLOAT
    else if (kind(jnk)==kind_rd) then
       netcdf_real = NF90_DOUBLE
    else
       write (6,'("ERROR: precision mismatch in get_netcdf_code_precision")')
    end if
    !Ints
    netcdf_int=NF90_INT !Could be NF90_SHORT etc, note netcdf4 allows 64 bit INT
    !Chars
    netcdf_char=NF90_CHAR
    !Byte
    netcdf_byte=NF90_BYTE

    !Set initialised state
    initialised=.true.
  end subroutine init_precision
end module nc_utils
