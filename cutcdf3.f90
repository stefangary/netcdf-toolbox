!------------------------------------------

       program cutcdf3

       implicit none

!-----------------------------------------
! This program will input a single
! model snapshot netcdf file and output
! a netcdf file that contains
! a smaller window into the data from
! the source file.  Thus, we CUT netCDF.
! A subset of grid nodes are copied
! directly from the big input file
! to a new, smaller subset file.  This
! is why we work primarily in index
! units => working directly with
! grid nodes is more robust.  No
! attempt is made to work with lon,
! lat, depth units.
!
! Fill values will be retained
! as they are present in the original
! source files.  Thus, there is no error
! checking to determine if the given
! values in the file are meaningful,
! simply direct copying from the specified
! subset of the file.
!
! Stefan Gary, Oct 2008.
!
! Version 2: Added option of time-limiting
! index values [lmin,lmax] to be specified
! on the command line rather than only
! hard coded in the user defined variables
! below.
!
! Version 3: Added -Dbigfile for large
! output files and added ability to handle
! additional type of attribute for compatibility
! with VIKING20 output.  Added compiler flags for
! the domain so this program can be
! compiled separately for each region to
! extract.  Ideally add command line
! i/j/k domain limits, but preproc flags
! will do for now.
! orca025_na------------GLBB2011/2012
! orca05_na-------------GLBB2011
! viking20_spg----------
! viking20_davis_str----ATLAS test domain
! viking20_rockall_bank-ATLAS test domain
! viking20_condor_seamt-ATLAS test domain
! viking20_fsc----------Kamila Walicka PhD
! 
!-----------------INPUTS------------------
! The shell script cutcdf will compile
! this code and create a symbolic link
! to the input file.  The name will
! be "infile.cdf".  Then, this program
! will be executed.  The script may
! then rename the output file and
! continue this process for many time
! steps.
!
! Below, the user must specify the
! domain over which to cut the netcdf
! input file using the index values.
!
! The user has the option of specifying
! the time indices [lmin,lmax] at the
! command line invokation of this program.
! This is done so that cutcdf may be
! used (along with a shell script) to
! split a single file of many time steps
! into many files of single time steps.
! If no time domain indeces are specified
! in the command line, then default
! values are used; these are specified
! by [lmin, lmax] alongiside the other
! domain index limits.  The command line
! options following the invokation of this
! program MUST be integers; otherwise,
! a fortran runtime error is generated.
!
!----------------OUTPUTS-----------------
! The output is a file, "outfile.cdf"
! that contains all the variables,
! attributes, and dimensions of the
! source file except that the spatial
! extent has been cut down according to
! the user-specified indicies.
!----------------------------------------
!
!=========User Defined Variables=========

! Names of the input and output files
! (symbolic links created before
! executing this program).
       character(len=10) :: infname  = 'infile.cdf'
       character(len=11) :: outfname = 'outfile.cdf'

! Define the extent of the data to
! copy in index units.  This program
! assumes that the first four dimensions
! of the netcdf file are x, y, z, t
! (in that order) corresponding to
! index counts of i, j, k, l.

#ifdef orca025_na
       integer :: imin = 823
       integer :: imax = 1144

       integer :: jmin = 510
       integer :: jmax = 860

       integer :: kmin = 1
       integer :: kmax = 46
#endif

#ifdef orca05_na
       integer :: imin = 400
       integer :: imax = 600

       integer :: jmin = 250
       integer :: jmax = 450

       integer :: kmin = 1
       integer :: kmax = 46
#endif

#ifdef viking20_spg
! SPG   |  450 | 1650 |   1  | 1200 |   46 |
       integer :: imin = 450
       integer :: imax = 1650

       integer :: jmin = 1
       integer :: jmax = 1200

       integer :: kmin = 1
       integer :: kmax = 46
#endif

#ifdef viking20_davis_str
! Davis*|  430 |  585 | 750  |  975 |   46 |
       integer :: imin = 430
       integer :: imax = 585

       integer :: jmin = 750
       integer :: jmax = 975

       integer :: kmin = 1
       integer :: kmax = 46
#endif

#ifdef viking20_rockall_bank
! RT/HP | 1125 | 1520 | 490  |  900 |   46 |
! RBank*| 1257 | 1354 | 644  |  715 |   46 |

       integer :: imin = 1257
       integer :: imax = 1354

       integer :: jmin = 644
       integer :: jmax = 715

       integer :: kmin = 1
       integer :: kmax = 46
#endif

#ifdef rhp
       integer :: imin = 1
       integer :: imax = 25

       integer :: jmin = 1
       integer :: jmax = 25

       integer :: kmin = 1
       integer :: kmax = 46
#endif

#ifdef viking20_condor_seamt
! Condr*| 1015 | 1076 | 143  |  222 |   46 |
 
       integer :: imin = 1015
       integer :: imax = 1076

       integer :: jmin = 143
       integer :: jmax = 222

       integer :: kmin = 1
       integer :: kmax = 46
#endif

#ifdef viking20_fsc
! FSC*  | 1390 | 1575 | 750  |  950 |   46 |

       integer :: imin = 1390
       integer :: imax = 1575

       integer :: jmin = 750
       integer :: jmax = 950

       integer :: kmin = 1
       integer :: kmax = 46
#endif

       ! (Default values: take the 1st tstep)
       integer :: lmin = 1
       integer :: lmax = 1

!======END of USER defined variables======

!-------Declare Other Variables----------

!---Domain----
! Maximum width (i), length(j), depth (k), and time (i)
       integer :: im, jm, km, lm

! Subset width (i), length (j), depth (k) and time (l)
       integer :: is, js, ks, ls

!----Counter variables----
       integer :: i, j, k, l

!-------Declarations: netcdf file io--------
! Out-file-id, In-file-id, error return, io
       integer :: ofid, ifid, exitcode, io
       integer, external :: ncopn, ncvid
       integer, external :: ncvdef, ncdid
       integer, external :: nccre, ncddef
       integer, external :: nf_create
       integer :: dummy

! Parameters as defined in netcdf.inc
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

       real, parameter :: fill_real = 9.9692099683868690e+36
       integer, parameter :: fill_int = -2147483647

! Variables defining basic information about
! each of the components of the file (dims,
! vars, and atts).

!----General File Parameters obtained from ncinq----

       ! Number of DIMensionS in the file
       integer :: ndims

       ! Number of VARibleS in the file
       integer :: nvars   

       ! Number of Global ATTributeS in the file
       integer :: ngatts

       ! ID of recording (unlimited) DIMension
       integer :: recdim

!----Dimension Parameters obtained from ncdinq-------
       
       ! Dimension ID number - assumed
       ! to cycle from 1 to ndims for the infile.
       integer, allocatable :: idid(:)

       ! List of outfile dimension IDs
       integer, allocatable :: odid(:)

       ! List of Dimension sizes for infile
       integer, allocatable :: idsize(:) 
       
       ! List of Dimension sizes for outfile
       integer, allocatable :: odsize(:)

       ! Dimension names (same for in and outfiles)
       character(len=20), allocatable :: dimname(:)

       ! Dimension type
       ! 1 = longitude type
       ! 2 = latitude type
       ! 3 = depth type
       ! 4 = time type
       ! 5 = holder (dimension length of 1,
       !             used for variables w/
       !             less than 4 dimensions)
       integer, allocatable :: dimtype(:)

!----Variable Parameters obtrained from ncvinq----

       ! list of Infile Variable ID numbers
       integer, allocatable :: ivid(:)

       ! list of Outfile Variable ID numbers
       integer, allocatable :: ovid(:)

       ! VARiable NAME (same for in and outfiles)
       character(len=20), allocatable :: varname(:)
       
       ! VARiable TYPE (int, real, etc.)
       integer, allocatable :: vartype(:)
       
       ! total Number of DIMensionS per Variable
       integer, allocatable :: nvdims(:)
       
       ! list of DIMension id's that apply to
       ! a variable.  For the creation of variables,
       ! different lengths of the vdims vectors
       ! are necessary depending on the shape
       ! of the variable.  We limit the number
       ! of dimensions per variable to 4 max.
       integer :: vdims1(1)
       integer :: vdims2(2)
       integer :: vdims3(3)
       integer :: vdims4(4)
       integer :: vdims(10)

       ! Store the dimensions corresponding
       ! to all variables in this list.
       ! i = the dimensions in the
       !     variable (rows for each var.)
       ! j = the number of variables.
       ! Values of zero in this array designate
       ! unused (possible) dimensions.  VarIDs
       ! correspond to the input file, not the
       ! output file because it's easier to
       ! reference the output varID from the
       ! input varID than vice-versa.
       integer,allocatable :: allvdims(:,:)

       ! Number of ATTributeS for each Var
       integer, allocatable :: nvatts(:)

!----Attribute Parameters obtained from ncanam and ncainq----

       ! ATTribute NUMber
       integer :: attnum

       ! ATTribute NAME
       character(len=20) :: attname

       ! ATTribute TYPE (int, real, etc.)
       integer :: atttype

       ! LENgth of ATTRIBUTE
       ! If it is a character type, then
       ! this is the number of letters.
       integer :: attlen
       
       ! ATTribute VALue if real NUMber
       real :: attvalnum
       real(kind=8) :: attvaldouble

       ! ATTribute VALue if STRing (character)
       character(len=35):: attvalstr	

!----------------DATA Storage------------- 
! An array for holding the data read from
! the file, IN VARiable with the naming
! convention <format>invar<shape> where:
! format = <b|c|s|i|f|d> corresponding
!          to the netcdf data types byte,
!          char, short, int, float, and
!          double.
! shape  = <1d|2d|3d|4d> corresponding to
!          the number of dimensions for
!          the variable.
!
! Declare an array for each shape and each
! possible variable type. In general, kind = 1
! is an 8-bit packet = "1 word", kind = 2
! requires twice as much space = "2 words"
! or 16-bits.  Similarly, kind = 4 and
! kind = 8 are 32 and 64 bits, respectively.
! The system default (declaring real or
! integer without any kind specifier) is
! kind = 4 for both integers and reals.

       ! 8 bit integers = ncbyte
       integer(kind = 1), allocatable :: binvar1d(:)
       integer(kind = 1), allocatable :: binvar2d(:,:)
       integer(kind = 1), allocatable :: binvar3d(:,:,:)
       integer(kind = 1), allocatable :: binvar4d(:,:,:,:)

       ! 16 bit integers = ncshort
       integer(kind = 2), allocatable :: sinvar1d(:)
       integer(kind = 2), allocatable :: sinvar2d(:,:)
       integer(kind = 2), allocatable :: sinvar3d(:,:,:)
       integer(kind = 2), allocatable :: sinvar4d(:,:,:,:)

       ! 32 bit integers = ncint  (system default)
       integer, allocatable :: iinvar1d(:)
       integer, allocatable :: iinvar2d(:,:)
       integer, allocatable :: iinvar3d(:,:,:)
       integer, allocatable :: iinvar4d(:,:,:,:)

       ! 32 bit reals = ncfloat   (system default)
       real, allocatable :: rinvar1d(:)
       real, allocatable :: rinvar2d(:,:)
       real, allocatable :: rinvar3d(:,:,:)
       real, allocatable :: rinvar4d(:,:,:,:)

       ! 64 bit reals = ncdouble
       real(kind=8), allocatable :: dinvar1d(:)
       real(kind=8), allocatable :: dinvar2d(:,:)
       real(kind=8), allocatable :: dinvar3d(:,:,:)
       real(kind=8), allocatable :: dinvar4d(:,:,:,:)


! Vectors that note where to read
! and write in the netcdf file,
! READ STart, READ COunt, and WRITe
! STart in 1D, 2D, 3D, and 4D
! versions.  Note that read count
! will the same as write count since
! only the data we seek to cut
! out are read from the input.
       integer :: readst1d(1)
       integer :: readco1d(1)
       integer :: writst1d(1)

       integer :: readst2d(1,2)
       integer :: readco2d(1,2)
       integer :: writst2d(1,2)

       integer :: readst3d(1,3)
       integer :: readco3d(1,3)
       integer :: writst3d(1,3)

       integer :: readst4d(1,4)
       integer :: readco4d(1,4)
       integer :: writst4d(1,4)

!----Variables for command line args-----

       integer :: num_command_arg
       character(len=4) :: arg_lmin, arg_lmax
       integer :: arg_len

!----------------------------------------

! Read values from the command line, and
! if present, copy them to [lmin,lmax].
! Only the time domain is specified from
! the command line.
       num_command_arg = command_argument_count()

       if(num_command_arg .lt. 2) then
          write(*,*) ' Warning: less than 2 command line args.'
          write(*,*) ' Staying with default [lmin,lmax].'
       elseif(num_command_arg .gt. 2) then
          write(*,*) ' Warning: more than 2 command line args.'
          write(*,*) ' Staying with default [lmin,lmax].'
       else
#ifdef verbose
          write(*,*) ' Two command line arguments found.'
          write(*,*) ' Proceeding to update [lmin,lmax].'
#endif
          ! For the first argument:
          call get_command_argument(1, arg_lmin, arg_len, exitcode)
          if (exitcode .gt. 0) then
             write(*,*) '  Error in retrieving argument!'
             stop
          elseif(exitcode .eq. -1) then
             write(*,*) '  Warning: argument truncated while passed!'
             stop
          elseif(exitcode .eq. 0) then
#ifdef verbose
             write(*,*) '  No problems in passing argument.'
#endif
          else
             write(*,*) '  Unknown exitcode from get_command_argument!'
             stop
          endif

          ! For the second argument:
          call get_command_argument(1, arg_lmax, arg_len, exitcode)
          if (exitcode .gt. 0) then
             write(*,*) '  Error in retrieving argument!'
             stop
          elseif(exitcode .eq. -1) then
             write(*,*) '  Warning: argument truncated while passed!'
             stop
          elseif(exitcode .eq. 0) then
#ifdef verbose
             write(*,*) '  No problems in passing argument.'
#endif
          else
             write(*,*) '  Unknown exitcode from get_command_argument!'
             stop
          endif

          ! Convert the arguments (strings) to integers
          read(arg_lmin,'(i4)') lmin
          read(arg_lmax,'(i4)') lmax
       endif
!----Done reading from the command line----

! Initial error checking of index limits:
       if(imin .gt. imax) then
          write(*,*) 'Warning: imin > imax, flipping values.'
          i = imax
          imax = imin
          imin = i
       endif

       if(imin .lt. 1) then
          write(*,*) 'Warning: imin < 1, setting to 1.'
          imin = 1
       endif
       
       if(jmin .gt. jmax) then
          write(*,*) 'Warning: jmin > jmax, flipping values.'
          i = jmax
          jmax = jmin
          jmin = i
       endif

       if(jmin .lt. 1) then
          write(*,*) 'Warning: jmin < 1, setting to 1.'
          jmin = 1
       endif

       if(kmin .gt. kmax) then
          write(*,*) 'Warning: kmin > kmax, flipping values.'
          i = kmax
          kmax = kmin
          kmin = i
       endif

       if(kmin .lt. 1) then
          write(*,*) 'Warning: kmin < 1, setting to 1.'
          kmin = 1
       endif

       if(lmin .gt. lmax) then
          write(*,*) 'Warning: lmin > lmax, flipping values.'
          i = lmax
          lmax = lmin
          lmin = i
       endif

       if(lmin .lt. 1) then
          write(*,*) 'Warning: lmin < 1, setting to 1.'
          lmin = 1
       endif

       ! Compute number of subset grid nodes.
       is = imax - imin + 1
       js = jmax - jmin + 1
       ks = kmax - kmin + 1
       ls = lmax - lmin + 1

#ifdef verbose       
       write(*,'(a25,i6,a3,i6,a3,i6,a3,i6)')&
             ' Subset grid size will be ',&
             is,' x ',js,' x ',ks,' x ',ls
#endif

! Open the input file, no write mode.
       ifid = ncopn(infname, ncnowrit, exitcode)
#ifdef verbose
       if(exitcode .eq. 0) then
          write(*,*) 'Infile opened.'
       else
          write(*,*) 'Unable to open infile.'
          stop
       endif
       write(6,*)' '
#endif

! Create the output file.  Clobber mode will
! overwrite any existing file w/ same name.
#ifdef bigfile
       dummy = nf_create(outfname,nf_64bit_offset,ofid)
#else
       ofid = nccre(outfname, ncclobber, exitcode)
#endif
#ifdef verbose
       if(exitcode .eq. 0) then
          write(*,*)' Output file created, in define mode.'
       else
          write(*,*)' Error in creating output file.'
          stop
       endif
#endif

! Get basic information about the file
       call ncinq(ifid, ndims, nvars, ngatts,&
                  recdim, exitcode)
#ifdef verbose
       if(exitcode .eq. 0) then
          write(*,*) 'Sucessful ncinq.'
       else
          write(*,*) 'Unable to inquire infile.'
          stop
       endif
#endif

! Print basic information to screen
#ifdef verbose
        write(6,*)'---------File Info---------'
        write(6,'(a,i6)')' Number of dimensions: ',ndims
        write(6,'(a,i6)')' Number of variables:  ',nvars
        write(6,'(a,i6)')' Num global attrib.:   ',ngatts
        write(6,'(a,i6)')' ID of unlimited dim:  ',recdim
        write(6,*)' '
#endif

! Get information about each dimension of input file
! and add these dimensions, in cut form, to the
! output file.
        allocate(idid(ndims))
        allocate(odid(ndims))
        allocate(idsize(ndims))
        allocate(odsize(ndims))
        allocate(dimname(ndims))
        allocate(dimtype(ndims))

!For each dimension:
        do i = 1,ndims
           ! NOTE: here we assume that the dimension IDs
           ! the original file increase monotonically
           ! from 1 to ndims.  This is consistent with
           ! the NetCDF documentation and I don't see
           ! any way around this assumption.
           idid(i) = i
           call ncdinq(ifid, idid(i), dimname(i), idsize(i), exitcode)
#ifdef verbose
           if(exitcode .eq. 0) then
              write(*,*) ' Sucessful inquiry into dimensions.'
           else
              write(*,*) ' Cannot inquire into dimensions.'
              stop
           endif
           write(*,*) '-------------------------------'
           write(*,*) ' For dimension: ',trim(dimname(i))
           write(*,*) ' Dimension ID:  ',idid(i)
           write(*,*) ' Original size: ',idsize(i)
#endif

! Determine the type of the dimension
! (lon, lat, dep, time, holder) by string
! comparisions with the dimname.  The index
! function:
!
! result = index(string, substring)
!
! will return the non-zero integer index of
! the start of substring within string.  If
! substring is not present, then result
! will be zero.  First we check for holder
! dimensions (becasue index can be confused
! between the ORCA holder dim x_a and the
! real dimension x, for example).

           if(idsize(i) .eq. 1) then
              ! This dim is a holder dim and will
              ! not be cut down (keeps size of 1).
              dimtype(i) = 5
              odsize(i) = 1
#ifdef verbose
              write(*,*) ' Holder dimension.'
              write(*,*) ' New size: ',odsize(i)
#endif
           elseif((index(dimname(i), 'Lon').ne.0).or.&
                  (index(dimname(i), 'lon').ne.0).or.&
                  (index(dimname(i), 'LON').ne.0).or.&
                  (index(dimname(i), 'X')  .ne.0).or.&
                  (index(dimname(i), 'x')  .ne.0)) then
              ! We have a longitude type dimension.
              ! It will be cut down to size determined
              ! by [imin, imax]
              dimtype(i) = 1
              odsize(i) = is

              ! Test whether the new dimension size is
              ! too large.
              if(odsize(i) .gt. idsize(i)) then
                 write(*,*) ' WARNING: output size > input size!'
                 write(*,*) ' Output size: ',odsize(i)
                 write(*,*) ' Need to readjust [imin,imax].'
                 stop
              elseif(imax .gt. idsize(i)) then
                 write(*,*) ' WARNING: imax > input size!'
                 write(*,*) ' imax: ',imax
                 write(*,*) ' Need to readjust [imin,imax].'
                 stop
              endif
#ifdef verbose
              write(*,*) ' Longitude dimension.'
              write(*,*) ' New size: ',odsize(i)
#endif
           elseif((index(dimname(i), 'Lat').ne.0).or.&
                  (index(dimname(i), 'lat').ne.0).or.&
                  (index(dimname(i), 'LAT').ne.0).or.&
                  (index(dimname(i), 'Y')  .ne.0).or.&
                  (index(dimname(i), 'y')  .ne.0)) then
              ! We have a latitude type dimension.
              ! It will be cut down to size determined
              ! by [jmin, jmax]
              dimtype(i) = 2
              odsize(i) = js

              ! Test whether the new dimension size is
              ! too large.
              if(odsize(i) .gt. idsize(i)) then
                 write(*,*) ' WARNING: output size > input size!'
                 write(*,*) ' Output size: ',odsize(i)
                 write(*,*) ' Need to readjust [jmin,jmax].'
                 stop
              elseif(jmax .gt. idsize(i)) then
                 write(*,*) ' WARNING: jmax > input size!'
                 write(*,*) ' jmax: ',jmax
                 write(*,*) ' Need to readjust [jmin,jmax].'
                 stop
              endif
#ifdef verbose
              write(*,*) ' Latitude dimension.'
              write(*,*) ' New size: ',odsize(i)
#endif
           elseif((index(dimname(i), 'dep').ne.0).or.&
                  (index(dimname(i), 'Dep').ne.0).or.&
                  (index(dimname(i), 'DEP').ne.0).or.&
                  (index(dimname(i), 'Z')  .ne.0).or.&
                  (index(dimname(i), 'z')  .ne.0)) then
              ! We have a depth type dimension.
              ! It will be cut down to size determined
              ! by [kmin, kmax]
              dimtype(i) = 3
              odsize(i) = ks

              ! Test whether the new dimension size is
              ! too large.
              if(odsize(i) .gt. idsize(i)) then
                 write(*,*) ' WARNING: output size > input size!'
                 write(*,*) ' Output size: ',odsize(i)
                 write(*,*) ' Need to readjust [kmin,kmax].'
                 stop
              elseif(kmax .gt. idsize(i)) then
                 write(*,*) ' WARNING: kmax > input size!'
                 write(*,*) ' kmax: ',kmax
                 write(*,*) ' Need to readjust [kmin,kmax].'
                 stop
              endif
#ifdef verbose
              write(*,*) ' Depth dimension.'
              write(*,*) ' New size: ',odsize(i)
#endif
           elseif((index(dimname(i), 'tim').ne.0).or.&
                  (index(dimname(i), 'Tim').ne.0).or.&
                  (index(dimname(i), 'TIM').ne.0))then
              ! We have a time type dimension.
              ! It will be cut down to size determined
              ! by [lmin, lmax]
              dimtype(i) = 4
              odsize(i) = ls

              ! Test whether the new dimension size is
              ! too large.
              if(odsize(i) .gt. idsize(i)) then
                 write(*,*) ' WARNING: output size > input size!'
                 write(*,*) ' Output size: ',odsize(i)
                 write(*,*) ' Need to readjust [lmin,lmax].'
                 stop
              elseif(lmax .gt. idsize(i)) then
                 write(*,*) ' WARNING: lmax > input size!'
                 write(*,*) ' lmax: ',lmax
                 write(*,*) ' Need to readjust [lmin,lmax].'
                 stop
              endif
#ifdef verbose
              write(*,*) ' Time dimension.'
              write(*,*) ' New size: ',odsize(i)
#endif
           endif
           
           ! Copy over this dimension to the output file
           odid(i) = ncddef(ofid, trim(dimname(i)),&
                            odsize(i), exitcode)
#ifdef verbose
           if(exitcode .eq. 0) then
              write(*,*) ' Dimension creation/copy sucessful.'
           else
              write(*,*) ' Dimension creation error.'
              stop
           endif
           write(*,*) ' Output dim ID: ',odid(i)
#endif
        enddo

! Get information about the variables of the file and
! create similar, but cut, variables in the output.
        allocate(ivid(nvars))
        allocate(ovid(nvars))
        allocate(varname(nvars))
        allocate(vartype(nvars))
        allocate(nvdims(nvars))
        allocate(nvatts(nvars))
        allocate(allvdims(4,nvars))

        ! Initialize allvdims to zero (unused dimensions)
        allvdims = 0

! For each variable,
        do i = 1,nvars
           ! NOTE: Here, as with the dimensions, I
           ! assume that the dimension ID's of the
           ! original file increase monotonically
           ! from 1 to nvars.
           ivid(i) = i
           call ncvinq(ifid,ivid(i),varname(i),&
                       vartype(i),nvdims(i),vdims,&
                       nvatts(i),exitcode)
#ifdef verbose
           write(*,*) '-------------------------------'
           write(*,*)' Var Name: ',trim(varname(i))
           write(*,*)' Var Type: ',vartype(i)
           write(*,*)' N Var Dims: ',nvdims(i)
           write(*,*)' N Var Atts: ',nvatts(i)
           write(*,*)' Var ID: ',ivid(i)
#endif
           ! Check that there are no 5D variables
           if(nvdims(i) .gt. 4) then
              write(*,*) ' WARNING: More than 4D!'
              stop
           endif

           ! Load information about the dimensions
           ! into the allvdims array.  For each dim,
           ! write its (infile) ID.
           do j = 1,nvdims(i)
              allvdims(j,i) = vdims(j)
           enddo

#ifdef verbose
           write(*,*) ' '
           write(*,*) 'with dimensions: '
           ! Print the name of each relevant
           ! dimension for this variable.
           do j = 1,nvdims(i)
              write(*,'(a,a,a,i3)') ' Dim name: ',&
                                    trim(dimname(vdims(j))),&
                                    '   IN dimID: ',idid(vdims(j))
           enddo
#endif

           ! Assign the dimension ids to a
           ! shape-specific vector to create the
           ! same variables in the output file.
           ! Then create the new variable in the
           ! output file.  Use the outfile dim
           ! IDs, referenced by the infile dim
           ! IDs (vdims).  Although the two should
           ! be the same - this provides a small
           ! additional generality that, indeed,
           ! we are referencing the correct dims
           ! in the input and output.
           if(nvdims(i) .eq. 1) then
              vdims1(1) = odid(vdims(1))
              ovid(i) = ncvdef(ofid,trim(varname(i)),&
                               vartype(i),nvdims(i),&
                               vdims1,exitcode)
#ifdef sverbose
              write(*,*) ' OUT dimID:',vdims1(1)
              if(exitcode .eq. 0) then
                 write(*,*) ' Variable shape copy OK.'
              else
                 write(*,*) ' Error creating new varaible.'
                 stop
              endif
#endif
           elseif(nvdims(i) .eq. 2) then
              vdims2(1) = odid(vdims(1))
              vdims2(2) = odid(vdims(2))
              ovid(i) = ncvdef(ofid,trim(varname(i)),&
                               vartype(i),nvdims(i),&
                               vdims2,exitcode)
#ifdef sverbose
              write(*,*) ' OUT dimID:',vdims2(1)
              write(*,*) ' OUT dimID:',vdims2(2)
              if(exitcode .eq. 0) then
                 write(*,*) ' Variable shape copy OK.'
              else
                 write(*,*) ' Error creating new varaible.'
                 stop
              endif
#endif
           elseif(nvdims(i) .eq. 3) then
              vdims3(1) = odid(vdims(1))
              vdims3(2) = odid(vdims(2))
              vdims3(3) = odid(vdims(3))
              ovid(i) = ncvdef(ofid,trim(varname(i)),&
                               vartype(i),nvdims(i),&
                               vdims3,exitcode)
#ifdef sverbose
              write(*,*) ' OUT dimID:',vdims3(1)
              write(*,*) ' OUT dimID:',vdims3(2)
              write(*,*) ' OUT dimID:',vdims3(3)
              if(exitcode .eq. 0) then
                 write(*,*) ' Variable shape copy OK.'
              else
                 write(*,*) ' Error creating new varaible.'
                 stop
              endif
#endif
           elseif(nvdims(i) .eq. 4) then
              vdims4(1) = odid(vdims(1))
              vdims4(2) = odid(vdims(2))
              vdims4(3) = odid(vdims(3))
              vdims4(4) = odid(vdims(4))
              ovid(i) = ncvdef(ofid,trim(varname(i)),&
                               vartype(i),nvdims(i),&
                               vdims4,exitcode)
#ifdef sverbose
              write(*,*) ' OUT dimID:',vdims4(1)
              write(*,*) ' OUT dimID:',vdims4(2)
              write(*,*) ' OUT dimID:',vdims4(3)
              write(*,*) ' OUT dimID:',vdims4(4)
              if(exitcode .eq. 0) then
                 write(*,*) ' Variable shape copy OK.'
              else
                 write(*,*) ' Error creating new varaible.'
                 stop
              endif
#endif
           endif
           
           ! Copy over the attributes corresponding
           ! to this variable.  For each attribute,
#ifdef sverbose
           write(*,*) '    Variable attributes:'
#endif
           do j = 1,nvatts(i)

              attnum = j
              ! Find the name of the attribute from the number.
              ! Attribute numbers are not as solid as attribute
              ! names, so netCDF standard is to use attribute
              ! name for all ID'ing.
              call ncanam(ifid,ivid(i),attnum,attname,exitcode)

              ! Get information about the attribute so that we
              ! what type of data it is.
              call ncainq(ifid,ivid(i),attname,atttype,attlen,exitcode)
#ifdef sverbose
              write(*,*)'    Name: ',trim(attname)
              write(*,*)'    Type: ',atttype
              write(*,*)'    Len:  ',attlen
#endif
              ! Get the value of the attribute
              if(atttype .eq. ncchar) then
                 ! Define an empty string to be overwritten.
                 attvalstr = '                                   '
                 call ncagtc(ifid,ivid(i),attname,&
                             attvalstr,attlen,exitcode)
#ifdef sverbose
                 write(*,*)'     Value: ',trim(attvalstr)
#endif
                 call ncaptc(ofid,ovid(ivid(i)),attname,&
                             atttype,attlen,attvalstr,exitcode)
#ifdef sverbose
                 if(exitcode .eq. 0) then
                    write(*,*)'     Attribute created Ok.'
                 else
                    write(*,*)'     Error creating attribute.'
                    stop
                 endif
#endif
              elseif ( atttype .eq. ncfloat ) then
                 ! the attribute value is a 32 bit real
                 call ncagt(ifid,ivid(i),attname,&
                            attvalnum,exitcode)
#ifdef sverbose
                 write(*,*)'     Value: ',attvalnum
#endif
                 call ncapt(ofid,ovid(ivid(i)),attname,&
                            atttype,attlen,attvalnum,exitcode)
#ifdef sverbose
                 if(exitcode .eq. 0) then
                    write(*,*)'     Attribute created Ok.'
                 else
                    write(*,*)'     Error creating attribute.'
                    stop
                 endif
#endif
              elseif ( atttype .eq. ncdouble ) then
                 ! the attribute value is a 64 real
                 call ncagt(ifid,ivid(i),attname,&
                            attvaldouble,exitcode)
#ifdef sverbose
                 write(*,*)'     Value: ',attvaldouble
#endif
                 call ncapt(ofid,ovid(ivid(i)),attname,&
                            atttype,attlen,attvaldouble,exitcode)
#ifdef sverbose
                 if(exitcode .eq. 0) then
                    write(*,*)'     Attribute created Ok.'
                 else
                    write(*,*)'     Error creating attribute.'
                    stop
                 endif
#endif
              else
                 write(*,*) 'ERROR: Unsupported attribute type!'
                 exit
              endif
           enddo
        enddo

        ! We are done defining the output file
        call ncendf(ofid, exitcode)
#ifdef verbose
        if(exitcode .eq. 0) then
           write(*,*) ' Outfile out of define mode - ready to write.'
        else
           write(*,*) ' Error exiting define mode.'
           stop
        endif
#endif

!--------------Copy Variable Values into output file------------------

        ! For each variable,
        do i = 1,nvars

#ifdef verbose
           write(*,*)'----COPYING ',trim(varname(i)),'----'
#endif
           ! Determine the readstart and readcount
           ! vectors that control which data is read.
           ! Allocate space in the array to hold
           ! the variable's values.  Variable used
           ! depends on the variable's shape and
           ! type, hence all the special cases.
           ! Then, read the cut version of
           ! the variable from the input file.
           ! Write the cut version to the output
           ! file.  Finally, Deallocate the holder.
           
           if (nvdims(i) .eq. 1) then

              ! Test if lon, lat, dep, time or hold dim
              if (dimtype(allvdims(1,i)) .eq. 1) then
                 ! Longitude type dimension => use [imin,imax]
                 readst1d(1) = imin
                 readco1d(1) = is
                 writst1d(1) = 1

              elseif (dimtype(allvdims(1,i)) .eq. 2) then
                 ! Latitude type dimension => use [jmin,jmax]
                 readst1d(1) = jmin
                 readco1d(1) = js
                 writst1d(1) = 1

              elseif (dimtype(allvdims(1,i)) .eq. 3) then
                 ! Depth type dimension => use [kmin,kmax]
                 readst1d(1) = kmin
                 readco1d(1) = ks
                 writst1d(1) = 1

              elseif (dimtype(allvdims(1,i)) .eq. 4) then
                 ! Time type dimension => use [lmin,lmax]
                 readst1d(1) = lmin
                 readco1d(1) = ls
                 writst1d(1) = 1

              elseif (dimtype(allvdims(1,i)) .eq. 5) then
                 ! Holder type dimension - unlikely here,
                 ! included for completeness.
                 readst1d(1) = 1
                 readco1d(1) = 1
                 writst1d(1) = 1

              else
                 write(*,*) 'Error: Unknown dimension type.'
                 stop
              endif

              ! Allocate the storage array according to
              ! the space specs determined above and the
              ! variable type.  Read in the variable,
              ! and write the variable to output. 
              ! First, test the variable type:
              if (vartype(i) .eq. ncchar) then
                 write(*,*) 'Error: Cannot write large scale ncchar var.'
                 stop
              elseif (vartype(i) .eq. ncbyte) then

                 allocate(binvar1d(readco1d(1)))
                 call ncvgt(ifid, ivid(i), readst1d, readco1d, binvar1d, exitcode)
                 call ncvpt(ofid, ovid(i), writst1d, readco1d, binvar1d, exitcode)
                 deallocate(binvar1d)

              elseif (vartype(i) .eq. ncint) then

                 allocate(iinvar1d(readco1d(1)))
                 call ncvgt(ifid, ivid(i), readst1d, readco1d, iinvar1d, exitcode)
                 call ncvpt(ofid, ovid(i), writst1d, readco1d, iinvar1d, exitcode)
                 deallocate(iinvar1d)

              elseif (vartype(i) .eq. ncshort) then

                 allocate(sinvar1d(readco1d(1)))
                 call ncvgt(ifid, ivid(i), readst1d, readco1d, sinvar1d, exitcode)
                 call ncvpt(ofid, ovid(i), writst1d, readco1d, sinvar1d, exitcode)
                 deallocate(sinvar1d)

              elseif (vartype(i) .eq. ncfloat) then

                 allocate(rinvar1d(readco1d(1)))
                 call ncvgt(ifid, ivid(i), readst1d, readco1d, rinvar1d, exitcode)
                 call ncvpt(ofid, ovid(i), writst1d, readco1d, rinvar1d, exitcode)
                 deallocate(rinvar1d)

              elseif (vartype(i) .eq. ncdouble) then

                 allocate(dinvar1d(readco1d(1)))
                 call ncvgt(ifid, ivid(i), readst1d, readco1d, dinvar1d, exitcode)
                 call ncvpt(ofid, ovid(i), writst1d, readco1d, dinvar1d, exitcode)
                 deallocate(dinvar1d)

              else
                 write(*,*) 'Error: Unknown variable type.'
                 stop
              endif

           elseif(nvdims(i) .eq. 2) then

              ! For each dimension of the variable,
              do j = 1,nvdims(i)
                 ! Test if lon, lat, dep, time or hold dim
                 if (dimtype(allvdims(j,i)) .eq. 1) then
                    ! Longitude type dimension => use [imin,imax]
                    readst2d(1,j) = imin
                    readco2d(1,j) = is
                    writst2d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 2) then
                    ! Latitude type dimension => use [jmin,jmax]
                    readst2d(1,j) = jmin
                    readco2d(1,j) = js
                    writst2d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 3) then
                    ! Depth type dimension => use [kmin,kmax]
                    readst2d(1,j) = kmin
                    readco2d(1,j) = ks
                    writst2d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 4) then
                    ! Time type dimension => use [lmin,lmax]
                    readst2d(1,j) = lmin
                    readco2d(1,j) = ls
                    writst2d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 5) then
                    ! Holder type dimension - unlikely here,
                    ! included for completeness.
                    readst2d(1,j) = 1
                    readco2d(1,j) = 1
                    writst2d(1,j) = 1

                 else
                    write(*,*) 'Error: Unknown dimension type.'
                    stop
                 endif
              enddo

              ! Allocate the storage array according to
              ! the space specs determined above and the
              ! variable type.  Read in the variable,
              ! and write the variable to output. 
              ! First, test the variable type:
              if (vartype(i) .eq. ncchar) then
                 write(*,*) 'Error: Cannot write large scale ncchar var.'
                 stop
              elseif (vartype(i) .eq. ncbyte) then

                 allocate(binvar2d(readco2d(1,1),readco2d(1,2)))
                 call ncvgt(ifid, ivid(i), readst2d, readco2d, binvar2d, exitcode)
                 call ncvpt(ofid, ovid(i), writst2d, readco2d, binvar2d, exitcode)
                 deallocate(binvar2d)

              elseif (vartype(i) .eq. ncint) then

                 allocate(iinvar2d(readco2d(1,1),readco2d(1,2)))
                 call ncvgt(ifid, ivid(i), readst2d, readco2d, iinvar2d, exitcode)
                 call ncvpt(ofid, ovid(i), writst2d, readco2d, iinvar2d, exitcode)
                 deallocate(iinvar2d)

              elseif (vartype(i) .eq. ncshort) then

                 allocate(sinvar2d(readco2d(1,1),readco2d(1,2)))
                 call ncvgt(ifid, ivid(i), readst2d, readco2d, sinvar2d, exitcode)
                 call ncvpt(ofid, ovid(i), writst2d, readco2d, sinvar2d, exitcode)
                 deallocate(sinvar2d)

              elseif (vartype(i) .eq. ncfloat) then

                 allocate(rinvar2d(readco2d(1,1),readco2d(1,2)))
                 call ncvgt(ifid, ivid(i), readst2d, readco2d, rinvar2d, exitcode)
                 call ncvpt(ofid, ovid(i), writst2d, readco2d, rinvar2d, exitcode)
                 deallocate(rinvar2d)

              elseif (vartype(i) .eq. ncdouble) then

                 allocate(dinvar2d(readco2d(1,1),readco2d(1,2)))
                 call ncvgt(ifid, ivid(i), readst2d, readco2d, dinvar2d, exitcode)
                 call ncvpt(ofid, ovid(i), writst2d, readco2d, dinvar2d, exitcode)
                 deallocate(dinvar2d)
              else
                 write(*,*) 'Error: Unknown variable type.'
                 stop
              endif

           elseif(nvdims(i) .eq. 3) then
              ! For each dimension of the variable,
              do j = 1,nvdims(i)
                 ! Test if lon, lat, dep, time or hold dim
                 if (dimtype(allvdims(j,i)) .eq. 1) then
                    ! Longitude type dimension => use [imin,imax]
                    readst3d(1,j) = imin
                    readco3d(1,j) = is
                    writst3d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 2) then
                    ! Latitude type dimension => use [jmin,jmax]
                    readst3d(1,j) = jmin
                    readco3d(1,j) = js
                    writst3d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 3) then
                    ! Depth type dimension => use [kmin,kmax]
                    readst3d(1,j) = kmin
                    readco3d(1,j) = ks
                    writst3d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 4) then
                    ! Time type dimension => use [lmin,lmax]
                    readst3d(1,j) = lmin
                    readco3d(1,j) = ls
                    writst3d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 5) then
                    ! Holder type dimension - unlikely here,
                    ! included for completeness.
                    readst3d(1,j) = 1
                    readco3d(1,j) = 1
                    writst3d(1,j) = 1

                 else
                    write(*,*) 'Error: Unknown dimension type.'
                    stop
                 endif
              enddo

              ! Allocate the storage array according to
              ! the space specs determined above and the
              ! variable type.  Read in the variable,
              ! and write the variable to output. 
              ! First, test the variable type:
              if (vartype(i) .eq. ncchar) then
                 write(*,*) 'Error: Cannot write large scale ncchar var.'
                 stop
              elseif (vartype(i) .eq. ncbyte) then

                 allocate(binvar3d(readco3d(1,1),readco3d(1,2),readco3d(1,3)))
                 call ncvgt(ifid, ivid(i), readst3d, readco3d, binvar3d, exitcode)
                 call ncvpt(ofid, ovid(i), writst3d, readco3d, binvar3d, exitcode)
                 deallocate(binvar3d)

              elseif (vartype(i) .eq. ncint) then

                 allocate(iinvar3d(readco3d(1,1),readco3d(1,2),readco3d(1,3)))
                 call ncvgt(ifid, ivid(i), readst3d, readco3d, iinvar3d, exitcode)
                 call ncvpt(ofid, ovid(i), writst3d, readco3d, iinvar3d, exitcode)
                 deallocate(iinvar3d)

              elseif (vartype(i) .eq. ncshort) then

                 allocate(sinvar3d(readco3d(1,1),readco3d(1,2),readco3d(1,3)))
                 call ncvgt(ifid, ivid(i), readst3d, readco3d, sinvar3d, exitcode)
                 call ncvpt(ofid, ovid(i), writst3d, readco3d, sinvar3d, exitcode)
                 deallocate(sinvar3d)

              elseif (vartype(i) .eq. ncfloat) then

                 allocate(rinvar3d(readco3d(1,1),readco3d(1,2),readco3d(1,3)))
                 call ncvgt(ifid, ivid(i), readst3d, readco3d, rinvar3d, exitcode)
                 call ncvpt(ofid, ovid(i), writst3d, readco3d, rinvar3d, exitcode)
                 deallocate(rinvar3d)

              elseif (vartype(i) .eq. ncdouble) then

                 allocate(dinvar3d(readco3d(1,1),readco3d(1,2),readco3d(1,3)))
                 call ncvgt(ifid, ivid(i), readst3d, readco3d, dinvar3d, exitcode)
                 call ncvpt(ofid, ovid(i), writst3d, readco3d, dinvar3d, exitcode)
                 deallocate(dinvar3d)
              else
                 write(*,*) 'Error: Unknown variable type.'
                 stop
              endif
   
           elseif(nvdims(i) .eq. 4) then
              ! For each dimension of the variable,
              do j = 1,nvdims(i)
                 ! Test if lon, lat, dep, time or hold dim
                 if (dimtype(allvdims(j,i)) .eq. 1) then
                    ! Longitude type dimension => use [imin,imax]
                    readst4d(1,j) = imin
                    readco4d(1,j) = is
                    writst4d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 2) then
                    ! Latitude type dimension => use [jmin,jmax]
                    readst4d(1,j) = jmin
                    readco4d(1,j) = js
                    writst4d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 3) then
                    ! Depth type dimension => use [kmin,kmax]
                    readst4d(1,j) = kmin
                    readco4d(1,j) = ks
                    writst4d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 4) then
                    ! Time type dimension => use [lmin,lmax]
                    readst4d(1,j) = lmin
                    readco4d(1,j) = ls
                    writst4d(1,j) = 1

                 elseif (dimtype(allvdims(j,i)) .eq. 5) then
                    ! Holder type dimension - unlikely here,
                    ! included for completeness.
                    readst4d(1,j) = 1
                    readco4d(1,j) = 1
                    writst4d(1,j) = 1

                 else
                    write(*,*) 'Error: Unknown dimension type.'
                    stop
                 endif
              enddo

              ! Allocate the storage array according to
              ! the space specs determined above and the
              ! variable type.  Read in the variable,
              ! and write the variable to output. 
              ! First, test the variable type:
              if (vartype(i) .eq. ncchar) then
                 write(*,*) 'Error: Cannot write large scale ncchar var.'
                 stop
              elseif (vartype(i) .eq. ncbyte) then

                 allocate(binvar4d(readco4d(1,1),readco4d(1,2),&
                                   readco4d(1,3),readco4d(1,4)))
                 call ncvgt(ifid, ivid(i), readst4d, readco4d, binvar4d, exitcode)
                 call ncvpt(ofid, ovid(i), writst4d, readco4d, binvar4d, exitcode)
                 deallocate(binvar4d)

              elseif (vartype(i) .eq. ncint) then

                 allocate(iinvar4d(readco4d(1,1),readco4d(1,2),&
                                   readco4d(1,3),readco4d(1,4)))
                 call ncvgt(ifid, ivid(i), readst4d, readco4d, iinvar4d, exitcode)
                 call ncvpt(ofid, ovid(i), writst4d, readco4d, iinvar4d, exitcode)
                 deallocate(iinvar4d)

              elseif (vartype(i) .eq. ncshort) then

                 allocate(sinvar4d(readco4d(1,1),readco4d(1,2),&
                                   readco4d(1,3),readco4d(1,4)))
                 call ncvgt(ifid, ivid(i), readst4d, readco4d, sinvar4d, exitcode)
                 call ncvpt(ofid, ovid(i), writst4d, readco4d, sinvar4d, exitcode)
                 deallocate(sinvar4d)

              elseif (vartype(i) .eq. ncfloat) then

                 allocate(rinvar4d(readco4d(1,1),readco4d(1,2),&
                                   readco4d(1,3),readco4d(1,4)))
                 call ncvgt(ifid, ivid(i), readst4d, readco4d, rinvar4d, exitcode)
                 call ncvpt(ofid, ovid(i), writst4d, readco4d, rinvar4d, exitcode)
                 deallocate(rinvar4d)

              elseif (vartype(i) .eq. ncdouble) then

                 allocate(dinvar4d(readco4d(1,1),readco4d(1,2),&
                                   readco4d(1,3),readco4d(1,4)))
                 call ncvgt(ifid, ivid(i), readst4d, readco4d, dinvar4d, exitcode)
                 call ncvpt(ofid, ovid(i), writst4d, readco4d, dinvar4d, exitcode)
                 deallocate(dinvar4d)
              else
                 write(*,*) 'Error: Unknown variable type.'
                 stop
              endif

           else
              write(*,*) 'Error: Unknown number of dimensions.'
              stop
           endif
#ifdef verbose
           if(exitcode .eq. 0) then
              write(*,*) ' Variable written to file OK.'
           else
              write(*,*) ' Error in writing variable.'
           endif
#endif
        enddo

        ! Close the input file
        call ncclos(ifid, exitcode)
#ifdef verbose
        if(exitcode.eq.0) then
           write(*,*)' Input file closed.'
        else
           write(*,*)' Problem closing input file.'
           stop
        endif
#endif

        ! Close the output file
        call ncclos(ofid, exitcode)
#ifdef verbose
        if(exitcode.eq.0) then
           write(*,*)' Output file closed.'
        else
           write(*,*)' Problem closing output file.'
           stop
        endif
#endif

        ! Write values to the reference vector dimensions.
!        write(6,*)' Writing lon, lat, depth, date vectors to output.'
!        call ncvpt(ofid,lonvid,1,imw,lon,exitcode)
!        call ncvpt(ofid,latvid,1,jmw,lat,exitcode)
!        call ncvpt(ofid,depvid,1,1,depth,exitcode)
!        call ncvpt(ofid,datvid,1,numfiles,date,exitcode)

! Allocate space for the input and output arrays
!       allocate(vin((imw),(jmw),1,1))
!       allocate(vout((imw),(jmw),1,numfiles))

! Set the readstart and readcount values,
! constant for all data files since we
! are reading the same window in each file.
!       readst(1,1) = ilow
!       readst(1,2) = jlow
!       readst(1,3) = kin
!       readst(1,4) = 1

!       readct(1,1) = imw
!       readct(1,2) = jmw
!       readct(1,3) = 1
!       readct(1,4) = 1

! For each infile:
!       do n = 1,numfiles
          ! Compute the file number
!          filenum = 1000 + n

          ! Determine the infile name
!          write(infname,'(a,i4,a)')infroot,filenum,infext
!          write(6,'(a,a,a,i4)')' Opening file: ',infname,
!     &                         ' out of: ',numfiles

          ! Open the file n
!          ifid = ncopn(infname, ncnowrit, exitcode)

          ! Get the variable id given the vnam
          ! specified by the user.
!          ivid = ncvid(ifid, vnam, exitcode)
          
          ! Read the source values from the infile
!          call ncvgt(ifid, ivid, readst, readct, vin, exitcode)

          ! Copy the source values into the
          ! variable for writing into the output file.
!          do j = 1,(jhigh-jlow+1)
!             do i = 1,(ihigh-ilow+1)
!                vout(i,j,1,n) = vin(i,j,1,1)
!             enddo
!          enddo

          ! Close the current infile
!          call ncclos(ifid, exitcode)

          ! End of the file counting loop
!       enddo

       ! Write the result to the output file

!       writest(1,1) = 1
!       writest(1,2) = 1
!       writest(1,3) = 1
!       writest(1,4) = 1

!       writect(1,1) = imw
!       writect(1,2) = jmw
!       writect(1,3) = 1
!       writect(1,4) = numfiles

!       write(6,*)' Writing window time series to output file...'
!       call ncvpt(ofid,ovid,writest,writect,vout,exitcode)
!       if(exitcode .eq. 0) write(6,*)'   Write complete.'

       ! Close and save the output netcdf file
!       call ncclos(ofid, exitcode)
!       if(exitcode.eq.0) write(6,*)' Output file closed.'

!       write(6,*)'End of wintser'

       end program cutcdf3

!------------------------------------------------
