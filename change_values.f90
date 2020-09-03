program change_values

  implicit none

  character(len=5) :: infname='in.nc'

  integer :: fid, vid
  integer :: exitcode, io
  integer, external :: ncopn, ncvid
  integer, external :: ncvdef, ncdid
  integer, external :: nccre, ncddef
  integer, external :: nf_create
  integer :: dummy

  integer, parameter :: ncnowrit = 0
  integer, parameter :: ncwrite = 1
  integer, parameter :: ncclobber = 0
  integer, parameter :: ncbyte = 1
  integer, parameter :: ncchar = 2
  integer, parameter :: ncshort =3
  integer, parameter :: ncint = 4
  integer, parameter :: ncfloat = 5
  integer, parameter :: ncdouble = 6
  integer, parameter :: nf_64bit_offset = 512

  real(kind=8), allocatable :: dinvar1d(:)

  integer, allocatable :: readstart(:)
  integer, allocatable :: readcount(:)

  integer :: ii
  
  write(*,*) 'Starting direct modification of values...'
  
  ! Hard coded allocated space
  allocate(dinvar1d(1460))
  allocate(readstart(1))
  allocate(readcount(1))
  readstart=1
  readcount=1460
  
  ! Open a netcdf file
  fid = ncopn(infname, ncwrite, exitcode)
  
  ! Read the values of a particular variable
  vid = ncvid(fid, 'TIME', exitcode)
  call ncvgt(fid,vid,readstart,readcount,dinvar1d,exitcode)
  
  ! Shift those values by a given amount
  do ii = 1,1460
     dinvar1d(ii) = dinvar1d(ii) - 11132.5
  enddo
  
  ! Write those values back to the original file
  call ncvpt(fid,vid,readstart,readcount,dinvar1d,exitcode)
  
  ! Close input file
  call ncclos(fid,exitcode)
   
  ! Done
  
end program change_values
