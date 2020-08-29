!------------------------------------------

       program avgcdf

       implicit none

!-----------------------------------------
! This program will input a series of
! model snapshot netcdf files and output
! a netcdf file that contains the
! average of the the data from the source
! files and another file that contains the
! variance about the mean. Thus, we
! AVeraGe the netCDF.
!
! We assume that all the input files
! are identical.  All the file dimensions,
! shapes, variables, and attributes of
! the output files will be copied directly
! from the first file.
!
! No attempt is made to modify any of
! the parameters of the file - the shapes
! of all variables are preserved and
! all are averaged accordingly.
! Therefore, this is not a strict time
! average - if the variables have time
! dimensions, then they will be averaged
! across time as well as space.
!
! Fill values will be checked during
! the averaging process according to the
! missing_value or _Fill_Value variable
! attributes given in the respective cdf
! input files.
!
! - - - - - - - - - - - - - - - - - - - -
! Stefan Gary, Jan 2010.
!
! Added the capability to average over
! ncshort variables as well.  Short integers
! are automatically converted to reals
! so that the averaging is done with real
! numbers and not integers to reduce
! truncation errors and strange stuff with
! integer arithmetic, etc. Sept. 2013.
!
! Added the capability to average over
! ncint variabiles as well.  They are
! converted to reals and averaged as reals,
! and then converted back to integers.
! Currently, the only variable types this
! system is cabable of are: ncdouble, ncfloat,
! ncshort, and ncint.  Oct. 2015.
!
! Added ncbyte variables, Jan. 2019.  Same
! approach as with the ncint, ncshort.       
!
! Cludge to ignore SeaWiFS "palette" variables.
! Added longer character names for NSIDC variables.
! Cludge to skip over character variables.
!       
!-----------------INPUTS------------------
! The shell script avgcdf will control all
! the inputs to this program.  The list of
! symbolic links is used by this program
! to count the number of files to be
! averaged.  On the command line invokation,
! the user must specify the number of files
! to be averaged.
!
! All the input files will be read into the
! program by the symbolic links, which
! should be named infile<num>.cdf where
! num is a monotonically increasing and
! integer between 10001 and 19999.
!
!----------------OUTPUTS-----------------
! The output is in two files:
! avgfile.cdf => mean values
! varfile.cdf => variance values
!
! All the variables properties,
! attributes, and dimensions of the
! source files are retained in each of
! the outfiles.
!
! Note that the variance is compted by
! dividing the sum of the squares by the
! number of data points, n, not n-1.
!
! There are two (optional) preprocessor
! flags:
! -D verbose : prints lots of intermediate
!              information to screen like
!              variable sizes and ID's, etc.
!
! -D bigfile : output is in netcdf 64bit
!              offset (without this flag,
!              output is in netcdf classic).
!              Also, to reduce RAM usage,
!              the variable averages are
!              computed by time step for all
!              of the 4D variables.
!
!----------------------------------------
!
!=========User Defined Variables=========
!
! None.
!
!======END of USER defined variables=====
!
!----------------File Naming-------------
! Names of the input files are divided
! into parts.
       character(len=6) :: infnamebase = 'infile'
       character(len=5) :: infnamenumb
       character(len=4) :: infnameextn = '.cdf'
       character(len=15) :: infname

! Output file names
       character(len=11) :: avgoutfname = 'avgfile.cdf'
       character(len=11) :: varoutfname = 'varfile.cdf'

!-------------------Domain-----------------
! Maximum width (i), length(j), depth (k), and time (i)
       integer :: im, jm, km, lm

!---------------Counter variables----------

! Dimension counters
       integer :: i, j, k, l, lc, l_max

! File counting
       integer :: n, nfiles, filenum
       integer, parameter :: filenumbase = 10000

! Variable counting
       integer :: v

!-------Declarations: netcdf file io--------
! Out-file-id, In-file-id, error return, io
       integer :: avgofid, varofid
       integer :: ifid, exitcode, io
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

       integer(kind=1) :: fill_byte, missing_byte
       integer(kind=1) :: min_byte, max_byte
       
       integer(kind=2) :: fill_shrt, missing_shrt
       integer(kind=2) :: min_shrt, max_shrt

       integer :: fill_intg, missing_intg
       integer :: min_intg, max_intg

       real :: fill_real, missing_real
       real :: min_real, max_real

       real(kind=8) :: fill_dble, missing_dble
       real(kind=8) :: min_dble, max_dble

       logical :: lfill, lmissing, lminv, lmaxv, l_valid
       logical :: lcoord, lnovar

! Variables defining basic information about
! each of the components of the file (dims,
! vars, and atts).

!----General File Parameters obtained from ncinq----

       ! Number of DIMensionS in the file
       integer :: ndims
       integer :: ndims_cur

       ! Number of VARibleS in the file
       integer :: nvars   
       integer :: nvars_cur

       ! Number of Global ATTributeS in the file
       integer :: ngatts
       integer :: ngatts_cur

       ! ID of recording (unlimited) DIMension
       integer :: recdim
       integer :: recdim_cur

!----Dimension Parameters obtained from ncdinq-------
       
       ! Dimension ID number - assumed
       ! to cycle from 1 to ndims for the infile.
       integer, allocatable :: idid(:)

       ! List of outfile dimension IDs
       integer, allocatable :: aodid(:)
       integer, allocatable :: vodid(:)

       ! Dimension sizes (same for in and out files)
       integer, allocatable :: dimsize(:) 
       
       ! Dimension names (same for in and outfiles)
       ! Updated from 20 because some NSIDC files require longer names.
       character(len=50), allocatable :: dimname(:)

!----Variable Parameters obtrained from ncvinq----

       ! list of Infile Variable ID numbers
       integer, allocatable :: ivid(:)
       integer :: vid

       ! list of Outfile Variable ID numbers
       integer, allocatable :: aovid(:)
       integer, allocatable :: vovid(:)

       ! var name (same for in and outfiles)
       ! Updated from 20 because some NSIDC files require longer names.
       character(len=50), allocatable :: varname(:)
       character(len=50) :: varname_cur
       character(len=50) :: varn

       ! var type (int, real, etc., same for in and out)
       integer, allocatable :: vartype(:)
       integer :: vart

       ! num dims per var (same for in and out)
       integer, allocatable :: nvdims(:)
       integer :: nvdim

       ! list of DIMension id's that apply to
       ! a variable.  We limit the number
       ! of dimensions per variable to 4 max.
       integer :: ivdims(4)
       integer, allocatable :: aovdims(:)
       integer, allocatable :: vovdims(:)

       ! Array of dimensions for each variable.
       ! 1st = variable
       ! 2nd = dimension index
       integer, allocatable :: dvmap(:,:)

       ! Number of ATTributeS for each Var
       integer, allocatable :: nvatts(:)
       integer :: nvats

!----Attribute Parameters obtained from ncanam and ncainq----

       ! ATTribute NUMber
       integer :: attnum

       ! ATTribute NAME
       ! Updated from 20 because some NSIDC files require longer names.
       character(len=50) :: attname

       ! ATTribute TYPE (int, real, etc.)
       integer :: atttype

       ! LENgth of ATTRIBUTE
       ! If it is a character type, then
       ! this is the number of letters.
       integer :: attlen
       
       ! ATTribute VALue if real NUMber
       real :: attvalnum

       ! ATTribute VALue if STRing (character)
       character(len=35):: attvalstr	

!----------------DATA Loading------------- 
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
       
!-------------Data Storage-----------------
! Similarly to the data loading arrays, we
! need to hold the running mean and the count
! of points for each variable.
! ao => average out
! vo => variance out
! co => counter out

       ! 8 bit integers = ncbyte
       ! See 16 bit integers below.
       integer(kind=1), allocatable :: baovar1d(:)
       integer(kind=1), allocatable :: baovar2d(:,:)
       integer(kind=1), allocatable :: baovar3d(:,:,:)
       integer(kind=1), allocatable :: baovar4d(:,:,:,:)

       integer(kind=1), allocatable :: bvovar1d(:)
       integer(kind=1), allocatable :: bvovar2d(:,:)
       integer(kind=1), allocatable :: bvovar3d(:,:,:)
       integer(kind=1), allocatable :: bvovar4d(:,:,:,:)
       
       ! 16 bit integers = ncshort
       ! DO NOT STORE INTERMEDIATE FIELDS
       ! AS INTEGERS.  ALL INTEGERS ARE
       ! AUTOMATICALLY CONVERTED TO REALS
       ! AND AVERAGING IS DONE ON REALS
       ! (TO PREVENT TRUNCATION).  AFTER
       ! ALL AVERAGING, CONVERT BACK TO
       ! INTEGERS FOR WRITING TO FILE.
       integer(kind=2), allocatable :: saovar1d(:)
       integer(kind=2), allocatable :: saovar2d(:,:)
       integer(kind=2), allocatable :: saovar3d(:,:,:)
       integer(kind=2), allocatable :: saovar4d(:,:,:,:)

       integer(kind=2), allocatable :: svovar1d(:)
       integer(kind=2), allocatable :: svovar2d(:,:)
       integer(kind=2), allocatable :: svovar3d(:,:,:)
       integer(kind=2), allocatable :: svovar4d(:,:,:,:)
       
       ! 32 bit integers = ncint (system default)
       integer, allocatable :: iaovar1d(:)
       integer, allocatable :: iaovar2d(:,:)
       integer, allocatable :: iaovar3d(:,:,:)
       integer, allocatable :: iaovar4d(:,:,:,:)

       integer, allocatable :: ivovar1d(:)
       integer, allocatable :: ivovar2d(:,:)
       integer, allocatable :: ivovar3d(:,:,:)
       integer, allocatable :: ivovar4d(:,:,:,:)

       ! 32 bit reals = ncfloat   (system default)
       real, allocatable :: raovar1d(:)
       real, allocatable :: raovar2d(:,:)
       real, allocatable :: raovar3d(:,:,:)
       real, allocatable :: raovar4d(:,:,:,:)

       real, allocatable :: rvovar1d(:)
       real, allocatable :: rvovar2d(:,:)
       real, allocatable :: rvovar3d(:,:,:)
       real, allocatable :: rvovar4d(:,:,:,:)

       ! 64 bit reals = ncdouble
       real(kind=8), allocatable :: daovar1d(:)
       real(kind=8), allocatable :: daovar2d(:,:)
       real(kind=8), allocatable :: daovar3d(:,:,:)
       real(kind=8), allocatable :: daovar4d(:,:,:,:)

       real(kind=8), allocatable :: dvovar1d(:)
       real(kind=8), allocatable :: dvovar2d(:,:)
       real(kind=8), allocatable :: dvovar3d(:,:,:)
       real(kind=8), allocatable :: dvovar4d(:,:,:,:)

       ! Counter arrays, always integers
       integer, allocatable :: covar1d(:)
       integer, allocatable :: covar2d(:,:)
       integer, allocatable :: covar3d(:,:,:)
       integer, allocatable :: covar4d(:,:,:,:)

!------------------------------------------

! Vectors that note where to read
! and write in the netcdf file,
! READ STart, READ COunt, and WRITe
! STart.
       integer, allocatable :: readstart(:)
       integer, allocatable :: readcount(:)
       integer, allocatable :: writstart(:)
       integer, allocatable :: writcount(:)

! Variables for command line args
       integer :: num_command_arg
       character(len=5) :: arg_nfiles
       integer :: arg_len

!----------------------------------------
#ifdef verbose
       write(*,*) 'Starting avgcdf...'
#endif
       ! Default value of valid variable flag.
       l_valid = .false.
!----------------------------------------
#ifdef verbose
       write(*,*) 'Creating output files...'
#endif
#ifdef bigfile
       avgofid = 42
       dummy = nf_create(avgoutfname,nf_64bit_offset,avgofid)
       varofid = 43
       dummy = nf_create(varoutfname,nf_64bit_offset,varofid)
#else
       avgofid = nccre(avgoutfname,ncclobber,exitcode)
       varofid = nccre(varoutfname, ncclobber,exitcode)
#endif

!----------------------------------------
#ifdef verbose
       write(*,*) 'Reading number of input files from command line...'
#endif
       ! Count number of command line arguments.
       num_command_arg = command_argument_count()
       
       if(num_command_arg .lt. 1) then
          write(*,*) 'Warning: less than 1 command line arg. Crash.'
          stop
       elseif(num_command_arg .gt. 1) then
          write(*,*) 'Warning: more than 1 command line args. Crash.'
          stop
       else
          call get_command_argument(1, arg_nfiles, arg_len, exitcode)
          if (exitcode .gt. 0) then
             write(*,*) 'Error in retrieving argument!'
             stop
          elseif(exitcode .eq. -1) then
             write(*,*) 'Warning: argument truncated while passed!'
             stop
          elseif(exitcode .eq. 0) then
#ifdef verbose
             write(*,*) 'Successful read from command line.'
#endif
          else
             write(*,*) 'Unknown exitcode from get_command_argument!'
             stop
          endif

          ! Convert the argument (string) to integer
          read(arg_nfiles,'(i5)') nfiles
          write(*,*) 'Starting avgcdf over ',nfiles,' input files.'
          
       endif
       
!----------------------------------------
#ifdef verbose
       write(*,*) 'Opening the first file...'
#endif
       filenum = filenumbase + 1

       ! Determine the infile name
       write(infname,'(a,i5,a)')infnamebase,filenum,infnameextn
#ifdef verbose
       write(*,'(a,a)')' Opening file: ',infname
#endif
       ! Open the file
       ifid = ncopn(infname, ncnowrit, exitcode)

!----------------------------------------
#ifdef verbose
       write(*,*) 'Getting/copying dimension info...'
       write(*,*) ' '
#endif
       ! Get basic information about the input file
       call ncinq(ifid,ndims,nvars,ngatts,recdim,exitcode)
#ifdef verbose
       ! Print basic information to screen
       write(6,*) '   ---------File Info---------'
       write(6,'(a,i6)') '   Number of dimensions: ',ndims
       write(6,'(a,i6)') '   Number of variables:  ',nvars
       write(6,'(a,i6)') '   Num global attrib.:   ',ngatts
       write(6,'(a,i6)') '   ID of unlimited dim:  ',recdim
       write(6,*)' '
#endif
       ! Allocate space to hold info for each dimension.
       allocate(idid(ndims))
       allocate(aodid(ndims))
       allocate(vodid(ndims))
       allocate(dimsize(ndims))
       allocate(dimname(ndims))

       ! For each dimension:
       do v = 1,ndims
          ! NOTE: here we assume that the dimension IDs
          ! the original file increase monotonically
          ! from 1 to ndims.  This is consistent with
          ! the NetCDF documentation and I don't see
          ! any way around this assumption.
          idid(v) = v
          call ncdinq(ifid,idid(v),dimname(v),dimsize(v),exitcode)
#ifdef verbose
          write(*,*) '   -------------------------------'
          write(*,*) '   Dimension Name: ',trim(dimname(v))
          write(*,*) '   Dimension ID:   ',idid(v)
          write(*,*) '   Dimension Size: ',dimsize(v)
#endif
          ! Copy this dimension to the output files
          aodid(v) = ncddef(avgofid,trim(dimname(v)),dimsize(v),exitcode)
#ifdef verbose
          if(exitcode .eq. 0) then
             write(*,*) '   Dimension copy to avg file OK, ID: ',aodid(v)
          else
             write(*,*) '   Dimension creation error.'
             stop
          endif
#endif
          vodid(v) = ncddef(varofid,trim(dimname(v)),dimsize(v),exitcode)
#ifdef verbose
          if(exitcode .eq. 0) then
             write(*,*) '   Dimension copy to var file OK, ID: ',vodid(v)
          else
             write(*,*) '   Dimension creation error.'
             stop
          endif
          write(*,*) ' '
#endif
       enddo

!----------------------------------------
#ifdef verbose
       write(*,*) 'Copying variable info...'
       write(*,*) ' '
#endif
       allocate(ivid(nvars))
       allocate(aovid(nvars))
       allocate(vovid(nvars))
       allocate(varname(nvars))
       allocate(vartype(nvars))
       allocate(nvdims(nvars))
       allocate(nvatts(nvars))
       allocate(dvmap(nvars,ndims))
       dvmap = 0

       ! For each variable
       do v = 1,nvars
          ! NOTE: Here, as with the dimensions, I
          ! assume that the variable ID's of the
          ! original file increase monotonically
          ! from 1 to nvars.
          ivid(v) = v
          call ncvinq(ifid,ivid(v),varname(v),vartype(v),&
               nvdims(v),ivdims,nvatts(v),exitcode)
#ifdef verbose
          write(*,*) '   -------------------------------'
          write(*,*) '   Var Name  : ',trim(varname(v))
          write(*,*) '   Var Type  : ',vartype(v)
          write(*,*) '   N Var Dims: ',nvdims(v)
          write(*,*) '   N Var Atts: ',nvatts(v)
          write(*,*) '   Var ID    : ',ivid(v)
#endif
          ! Check that there are no 5D variables
          if(nvdims(v) .gt. 4) then
             write(*,*) '   WARNING: More than 4D!'
             stop
          endif

          ! We want to ignore this variable, so continue
          ! onto the next loop iteration.
          if ( index(trim(varname(v)),'palette') .ne. 0) then
#ifdef verbose             
             write(*,*) 'Ignoring variable!'
#endif
             cycle
          endif

          ! Print the name of each relevant
          ! dimension for this variable.
#ifdef verbose
          write(*,*) ' '
          write(*,*) '   with dimensions: '
          do j = 1,nvdims(v)
             write(*,*) '      ',trim(dimname(ivdims(j)))
          enddo
#endif
!----------------------------------------

          ! Fill in yes (1) or no (0, default) if this
          ! variable depends on each dimension.

          do j = 1,ndims

             ! Check if this dimension is present
             ! in the variable.
             do k = 1,nvdims(v)
                if(ivdims(k) .eq. idid(j) ) then
                   dvmap(v,j) = 1
                   exit
                endif
             enddo

          enddo

!----------------------------------------
          
          ! Create the list of dimension IDs for output.
          allocate(aovdims(nvdims(v)))
          allocate(vovdims(nvdims(v)))
          aovdims = -1
          vovdims = -1

          ! For each element in ivdims
          do k = 1,nvdims(v)

             ! Cycle through all possible dimensions
             ! to find the dimension we seek.
             do j = 1,ndims

                if( ivdims(k) .eq. idid(j) ) then

                   ! This dimension is in this variable.
                   aovdims(k) = aodid(j)
                   vovdims(k) = vodid(j)

                endif
             enddo
          enddo

          write(*,*) ' ivdims: ',ivdims
          write(*,*) ' aovdims: ',aovdims
          write(*,*) ' vovdims: ',vovdims

!----------------------------------------

          aovid(v) = ncvdef(avgofid,trim(varname(v)),&
               vartype(v),nvdims(v),aovdims,exitcode)
          if(exitcode .eq. 0) then
             write(*,*) '   Variable shape copy to avg file OK.'
          else
             write(*,*) 'Error creating new varaible in avg file.'
             stop
          endif

          vovid(v) = ncvdef(varofid,trim(varname(v)),&
               vartype(v),nvdims(v),vovdims,exitcode)
          if(exitcode .eq. 0) then
             write(*,*) '   Variable shape copy to var file OK.'
          else
             write(*,*) ' Error creating new varaible in var file.'
             stop
          endif
       
          deallocate(aovdims)
          deallocate(vovdims)

!----------------------------------------
#ifdef verbose
          ! Copy over the attributes corresponding
          ! to this variable.
          write(*,*) ' '
          write(*,*) '   with attributes:'
          write(*,*) ' '
#endif
          do j = 1,nvatts(v)

             attnum = j
             ! Find the name of the attribute from the number.
             ! Attribute numbers are not as solid as attribute
             ! names, so netCDF standard is to use attribute
             ! name for all ID'ing.
             call ncanam(ifid,ivid(v),attnum,attname,exitcode)

             ! Get information about the attribute so that we
             ! what type of data it is.
             call ncainq(ifid,ivid(v),attname,atttype,attlen,exitcode)
#ifdef verbose
             write(*,*) '      -------------------------------'
             write(*,*) '      Att Name: ',trim(attname)
             write(*,*) '      Att Type: ',atttype
             write(*,*) '      Att Len : ',attlen
#endif
             call ncacpy(ifid,ivid(v),trim(attname),avgofid,aovid(v),exitcode)
             if(exitcode .eq. 0) then
                write(*,*)'      Attribute created in avg file OK.'
             else
                write(*,*)'      Error creating attribute in avg file.'
                stop
             endif

             call ncacpy(ifid,ivid(v),trim(attname),varofid,vovid(v),exitcode)
             if(exitcode .eq. 0) then
                write(*,*)'      Attribute created in var file OK.'
             else
                write(*,*)'      Error creating attribute in var file.'
                stop
             endif
#ifdef verbose
             write(*,*) ' '
#endif
          enddo
       enddo

!----------------------------------------
#ifdef verbose
       write(*,*) 'Done defining output files.'
#endif
       call ncclos(ifid,exitcode)
       if(exitcode .eq. 0) then
          !write(*,*) 'First file closed.'
       else
          write(*,*) 'Cannot close first file!'
          stop
       endif

       call ncendf(avgofid, exitcode)
       if(exitcode .eq. 0) then
          !write(*,*) 'Average outfile ready to write.'
       else
          write(*,*) 'Error exiting define mode in avg file.'
          stop
       endif

       call ncendf(varofid, exitcode)
       if(exitcode .eq. 0) then
          !write(*,*) 'Variance outfile ready to write.'
       else
          write(*,*) 'Error exiting define mode in var file.'
          stop
       endif

!----------------------------------------
#ifdef verbose
       write(*,*) '-------------Starting to compute averages...---------------'
       write(*,*) ' '
#endif
       ! For each variable,
       do v = 1,nvars
#ifdef verbose
          write(*,*) '=========== Averaging ',trim(varname(v)),' ==========='
#endif
          ! We want to ignore this variable, so continue
          ! onto the next loop iteration.
          if ( index(trim(varname(v)),'palette') .ne. 0) then
#ifdef verbose             
             write(*,*) 'Ignoring variable!'
#endif
             cycle
          endif
          
#ifdef bigfile
          ! Loop over each element along the
          ! 4th dimension of the variable,
          ! if present.  If variable has less
          ! than 4 dimensions, then this loop
          ! collapses to a single list of
          ! executions.  The counter lc
          ! stands for "l current".
          if ( nvdims(v) .lt. 4 ) then
             l_max = 1
          else
             l_max = dimsize(4)
          endif
          do lc = 1,l_max
#ifdef verbose
             if ( nvdims(v) .eq. 4 ) write(*,*) '  Averaging big file 4th dimension ',lc,' of ',l_max,'...'
#endif             
#endif
             ! Loop over each input file,
             do n = 1,nfiles
                !----------------------------------------
                ! Determine the current infile name
                filenum = filenumbase + n
                write(infname,'(a,i5,a)')infnamebase,filenum,infnameextn
#ifdef verbose
                write(*,'(a,a)')'   Opening file: ',infname
#endif
                ! Open the current infile
                ifid = ncopn(infname,ncnowrit,exitcode)
                if(exitcode .eq. 0) then
                   !write(*,*) '   Infile opened.'
                else
                   write(*,*) '   Unable to open infile.'
                   stop
                endif

                !----------------------------------------
                ! Check that the variable we seek is present
                ! in this infile.

                ! (1) Count the number of variables in the
                ! current infile.
                call ncinq(ifid,ndims_cur,nvars_cur,ngatts_cur,recdim_cur,exitcode)

                ! (2) Test that we have the same number of
                ! variables as the first file we used to
                ! define the output files.
                lnovar = .true.
                if( (nvars_cur .eq. nvars) .and. (ndims_cur .eq. ndims) ) then
                   ! We have, most likely, the same file
                   ! structure, so proceed as normal.
                   lnovar = .false.
                else
                   ! Check whether the missing variable is
                   ! the one we're trying to average now.
                   do i = 1,nvars_cur
                      call ncvinq(ifid,i,varn,vart,nvdim,ivdims,nvats,exitcode)
                      if( (index(varn,varname(v)) .ne. 0) .and. &
                           (len_trim(varn) .eq. len_trim(varname(v))) ) then
                         ! The variable is present
                         lnovar = .false.
                      else
                         ! The variable is not present
                      endif
                   enddo
                endif
                
                if( lnovar ) then
                   ! We do not have the variable we
                   ! seek in this file.  Skip file.
                   write(*,*) varname(v),' is not present. Skipping file!'
                elseif ( vartype(v) .eq. ncchar ) then
                   ! This variable is a character!
                   write(*,*) varname(v),' is a character.  Skipping file!'
                else
                   ! The variable we seek is present,
                   ! so proceed with the averaging.

                   !----------------------------------------

                   ! Get variable ID with this variable name
                   vid = ncvid(ifid,trim(varname(v)),exitcode)
                   if(exitcode .eq. 0) then
                      !write(*,*) '   Got variable ID for infile.'
                   else
                      write(*,*) '   Unable to get infile VID.'
                      stop
                   endif

                   !----------------------------------------
             
                   ! For the first snapshot, set index bounds
                   ! and allocate space.  Also, do this only for
                   ! the first 4th dimension time step of the
                   ! first big file.
#ifdef bigfile
                   if( (n .eq. 1).and.(lc .eq. 1) ) then
#else
                   if( n .eq. 1) then
#endif
#ifdef verbose
                      write(*,*) '   Setting index bounds...'
#endif
                      !----------------------------------------
                      ! Get variable information
                      call ncvinq(ifid,vid,varn,vart,nvdim,ivdims,nvats,exitcode)
                      if(exitcode .eq. 0) then
                         !write(*,*) '   Got variable info.'
                      else
                         write(*,*) '   Cannot get variable info. Crash.'
                         stop
                      endif

                      !----------------------------------------

                      ! Error checking
                      if(vart .ne. vartype(v)) then
                         write(*,*) '   WARNING: Inconsistent variable type!'
                         stop
                      endif

                      if(nvdim .ne. nvdims(v)) then
                         write(*,*) '   WARNING: Inconsistent # dimensions!'
                         stop
                      endif

                      if(nvats .ne. nvatts(v)) then
                         write(*,*) '   WARNING: Inconsistent # attributes!'
                         stop
                      endif

                      !----------------------------------------

                      ! Allocate space dependent on the shape.
                      allocate(readstart(nvdim))
                      allocate(readcount(nvdim))
                      allocate(writstart(nvdim))
                      allocate(writcount(nvdim))
                      l_valid = .true.
                      
                      !----------------------------------------
                      readstart = 1
                      writstart = 1
                      ! For each element in list of dimensions
                      do k = 1,nvdim
                         ! Cycle through all possible dimensions
                         ! to find the dimension we seek.
                         do j = 1,ndims
                            if( ivdims(k) .eq. idid(j) ) then
                               ! This dimension is in this variable.
                               readcount(k) = dimsize(j)
                               writcount(k) = dimsize(j)    
                            endif
                         enddo
                      enddo
                
                      !----------------------------------------
                      ! Assign index limits
                      im = readcount(1)
                      if( nvdim .ge. 2 ) jm = readcount(2)
                      if( nvdim .ge. 3 ) km = readcount(3)
                      if( nvdim .eq. 4 ) lm = readcount(4)
                   
                      !----------------------------------------
                      write(*,*) ' Readstart: ',readstart
                      write(*,*) ' Readcount: ',readcount
                      write(*,*) ' Writstart: ',writstart
                      write(*,*) ' Writcount: ',writcount
                      !----------------------------------------

                      !----------------------------------------
                      ! Allocate space for holders and counter.
                      ! Initialize holders and counter.
                      if( (vart .eq. ncfloat) .and. (nvdim .gt. 0) ) then

                         if( nvdim .eq. 1 ) then
                            allocate(rinvar1d(1:im))
                            allocate(raovar1d(1:im))
                            allocate(rvovar1d(1:im))
                            allocate(covar1d(1:im))
                            rinvar1d = 0.0
                            raovar1d = 0.0
                            rvovar1d = 0.0
                            covar1d = 0
                         
                         elseif( nvdim .eq. 2 ) then
                            allocate(rinvar2d(1:im,1:jm))
                            allocate(raovar2d(1:im,1:jm))
                            allocate(rvovar2d(1:im,1:jm))
                            allocate(covar2d(1:im,1:jm))
                            rinvar2d = 0.0
                            raovar2d = 0.0
                            rvovar2d = 0.0
                            covar2d = 0

                         elseif( nvdim .eq. 3 ) then
                            allocate(rinvar3d(1:im,1:jm,1:km))
                            allocate(raovar3d(1:im,1:jm,1:km))
                            allocate(rvovar3d(1:im,1:jm,1:km))
                            allocate(covar3d(1:im,1:jm,1:km))
                            rinvar3d = 0.0
                            raovar3d = 0.0
                            rvovar3d = 0.0
                            covar3d = 0

                         elseif( nvdim .eq. 4 ) then
#ifdef bigfile
                            ! For large files, we will proceed
                            ! time step by time step, so keep
                            ! size of the 4th dimension to 1.
                            allocate(rinvar4d(1:im,1:jm,1:km,1))
                            allocate(raovar4d(1:im,1:jm,1:km,1))
                            allocate(rvovar4d(1:im,1:jm,1:km,1))
                            allocate(covar4d(1:im,1:jm,1:km,1))
#else
                            ! For smaller files, we can allocate
                            ! as much space as is needed in all
                            ! dimensions.
                            allocate(rinvar4d(1:im,1:jm,1:km,1:lm))
                            allocate(raovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(rvovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(covar4d(1:im,1:jm,1:km,1:lm))
#endif
                            rinvar4d = 0.0
                            raovar4d = 0.0
                            rvovar4d = 0.0
                            covar4d = 0
                         else
                            write(*,*) '   Unknown number of dimensions! Crash.'
                            stop
                         endif

                      elseif( (vart .eq. ncdouble) .and. (nvdim .gt. 0) ) then
                      
                         if( nvdim .eq. 1 ) then
                            allocate(dinvar1d(1:im))
                            allocate(daovar1d(1:im))
                            allocate(dvovar1d(1:im))
                            allocate(covar1d(1:im))
                            dinvar1d = 0.0
                            daovar1d = 0.0
                            dvovar1d = 0.0
                            covar1d = 0
                         
                         elseif( nvdim .eq. 2 ) then
                            allocate(dinvar2d(1:im,1:jm))
                            allocate(daovar2d(1:im,1:jm))
                            allocate(dvovar2d(1:im,1:jm))
                            allocate(covar2d(1:im,1:jm))
                            dinvar2d = 0.0
                            daovar2d = 0.0
                            dvovar2d = 0.0
                            covar2d = 0
                            
                         elseif( nvdim .eq. 3 ) then
                            allocate(dinvar3d(1:im,1:jm,1:km))
                            allocate(daovar3d(1:im,1:jm,1:km))
                            allocate(dvovar3d(1:im,1:jm,1:km))
                            allocate(covar3d(1:im,1:jm,1:km))
                            dinvar3d = 0.0
                            daovar3d = 0.0
                            dvovar3d = 0.0
                            covar3d = 0
                         
                         elseif( nvdim .eq. 4 ) then
#ifdef bigfile
                            ! See notes above for nvdim .eq. 4
                            ! for the single precision variable
                            ! size allocation.
                            allocate(dinvar4d(1:im,1:jm,1:km,1))
                            allocate(daovar4d(1:im,1:jm,1:km,1))
                            allocate(dvovar4d(1:im,1:jm,1:km,1))
                            allocate(covar4d(1:im,1:jm,1:km,1))
#else
                            allocate(dinvar4d(1:im,1:jm,1:km,1:lm))
                            allocate(daovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(dvovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(covar4d(1:im,1:jm,1:km,1:lm))
#endif
                            dinvar4d = 0.0
                            daovar4d = 0.0
                            dvovar4d = 0.0
                            covar4d = 0
                         else
                            write(*,*) '   Unknown number of dimensions! Crash.'
                            stop
                         endif

                      elseif ( (vart .eq. ncshort) .and. (nvdim .gt. 0) ) then

                         ! NOTE THAT REAL VARIABLES
                         ! are used to store the integers
                         ! during the averaging process.
                         ! The saovar*d variables are purely
                         ! for writing at the end.

                         if( nvdim .eq. 1 ) then
                            allocate(rinvar1d(1:im))
                            allocate(raovar1d(1:im))
                            allocate(rvovar1d(1:im))
                            allocate(covar1d(1:im))
                            rinvar1d = 0.0
                            raovar1d = 0.0
                            rvovar1d = 0.0
                            covar1d = 0
                         
                            allocate(sinvar1d(1:im))
                            allocate(saovar1d(1:im))
                            allocate(svovar1d(1:im))
                            sinvar1d = 0.0
                            saovar1d = 0.0
                            svovar1d = 0.0

                         elseif( nvdim .eq. 2 ) then
                            allocate(rinvar2d(1:im,1:jm))
                            allocate(raovar2d(1:im,1:jm))
                            allocate(rvovar2d(1:im,1:jm))
                            allocate(covar2d(1:im,1:jm))
                            rinvar2d = 0.0
                            raovar2d = 0.0
                            rvovar2d = 0.0
                            covar2d = 0
                            
                            allocate(sinvar2d(1:im,1:jm))
                            allocate(saovar2d(1:im,1:jm))
                            allocate(svovar2d(1:im,1:jm))

                         elseif( nvdim .eq. 3 ) then
                            allocate(rinvar3d(1:im,1:jm,1:km))
                            allocate(raovar3d(1:im,1:jm,1:km))
                            allocate(rvovar3d(1:im,1:jm,1:km))
                            allocate(covar3d(1:im,1:jm,1:km))
                            rinvar3d = 0.0
                            raovar3d = 0.0
                            rvovar3d = 0.0
                            covar3d = 0
                         
                            allocate(sinvar3d(1:im,1:jm,1:km))
                            allocate(saovar3d(1:im,1:jm,1:km))
                            allocate(svovar3d(1:im,1:jm,1:km))

                         elseif( nvdim .eq. 4 ) then
#ifdef bigfile
                            ! See notes above for nvdim .eq. 4
                            ! for the single precision variable
                            ! size allocation.
                            allocate(rinvar4d(1:im,1:jm,1:km,1))
                            allocate(raovar4d(1:im,1:jm,1:km,1))
                            allocate(rvovar4d(1:im,1:jm,1:km,1))
                            allocate(covar4d(1:im,1:jm,1:km,1))

                            allocate(sinvar4d(1:im,1:jm,1:km,1))
                            allocate(saovar4d(1:im,1:jm,1:km,1))
                            allocate(svovar4d(1:im,1:jm,1:km,1))
#else
                            allocate(rinvar4d(1:im,1:jm,1:km,1:lm))
                            allocate(raovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(rvovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(covar4d(1:im,1:jm,1:km,1:lm))

                            allocate(sinvar4d(1:im,1:jm,1:km,1:lm))
                            allocate(saovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(svovar4d(1:im,1:jm,1:km,1:lm))
#endif
                            rinvar4d = 0.0
                            raovar4d = 0.0
                            rvovar4d = 0.0
                            covar4d = 0

                         else
                            write(*,*) '   Unknown number of dimensions! Crash.'
                            stop
                         endif

                      elseif ( (vart .eq. ncbyte) .and. (nvdim .gt. 0) ) then
                         
                         ! NOTE THAT REAL VARIABLES
                         ! are used to store the integers
                         ! during the averaging process.
                         ! The saovar*d variables are purely
                         ! for writing at the end.

                         if( nvdim .eq. 1 ) then
                            allocate(rinvar1d(1:im))
                            allocate(raovar1d(1:im))
                            allocate(rvovar1d(1:im))
                            allocate(covar1d(1:im))
                            rinvar1d = 0.0
                            raovar1d = 0.0
                            rvovar1d = 0.0
                            covar1d = 0
                         
                            allocate(binvar1d(1:im))
                            allocate(baovar1d(1:im))
                            allocate(bvovar1d(1:im))
                            binvar1d = 0.0
                            baovar1d = 0.0
                            bvovar1d = 0.0

                         elseif( nvdim .eq. 2 ) then
                            allocate(rinvar2d(1:im,1:jm))
                            allocate(raovar2d(1:im,1:jm))
                            allocate(rvovar2d(1:im,1:jm))
                            allocate(covar2d(1:im,1:jm))
                            rinvar2d = 0.0
                            raovar2d = 0.0
                            rvovar2d = 0.0
                            covar2d = 0
                            
                            allocate(binvar2d(1:im,1:jm))
                            allocate(baovar2d(1:im,1:jm))
                            allocate(bvovar2d(1:im,1:jm))

                         elseif( nvdim .eq. 3 ) then
                            allocate(rinvar3d(1:im,1:jm,1:km))
                            allocate(raovar3d(1:im,1:jm,1:km))
                            allocate(rvovar3d(1:im,1:jm,1:km))
                            allocate(covar3d(1:im,1:jm,1:km))
                            rinvar3d = 0.0
                            raovar3d = 0.0
                            rvovar3d = 0.0
                            covar3d = 0
                         
                            allocate(binvar3d(1:im,1:jm,1:km))
                            allocate(baovar3d(1:im,1:jm,1:km))
                            allocate(bvovar3d(1:im,1:jm,1:km))

                         elseif( nvdim .eq. 4 ) then
#ifdef bigfile
                            ! See notes above for nvdim .eq. 4
                            ! for the single precision variable
                            ! size allocation.
                            allocate(rinvar4d(1:im,1:jm,1:km,1))
                            allocate(raovar4d(1:im,1:jm,1:km,1))
                            allocate(rvovar4d(1:im,1:jm,1:km,1))
                            allocate(covar4d(1:im,1:jm,1:km,1))

                            allocate(binvar4d(1:im,1:jm,1:km,1))
                            allocate(baovar4d(1:im,1:jm,1:km,1))
                            allocate(bvovar4d(1:im,1:jm,1:km,1))
#else
                            allocate(rinvar4d(1:im,1:jm,1:km,1:lm))
                            allocate(raovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(rvovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(covar4d(1:im,1:jm,1:km,1:lm))

                            allocate(binvar4d(1:im,1:jm,1:km,1:lm))
                            allocate(baovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(bvovar4d(1:im,1:jm,1:km,1:lm))
#endif
                            rinvar4d = 0.0
                            raovar4d = 0.0
                            rvovar4d = 0.0
                            covar4d = 0

                         else
                            write(*,*) '   Unknown number of dimensions! Crash.'
                            stop
                         endif


                      elseif ( (vart .eq. ncint) .and. (nvdim .gt. 0) ) then

                         ! NOTE THAT REAL VARIABLES
                         ! are used to store the integers
                         ! during the averaging process.
                         ! The iaovar*d variables are purely
                         ! for writing at the end.

                         if( nvdim .eq. 1 ) then
                            allocate(rinvar1d(1:im))
                            allocate(raovar1d(1:im))
                            allocate(rvovar1d(1:im))
                            allocate(covar1d(1:im))
                            rinvar1d = 0.0
                            raovar1d = 0.0
                            rvovar1d = 0.0
                            covar1d = 0
                         
                            allocate(iinvar1d(1:im))
                            allocate(iaovar1d(1:im))
                            allocate(ivovar1d(1:im))
                            iinvar1d = 0.0
                            iaovar1d = 0.0
                            ivovar1d = 0.0

                         elseif( nvdim .eq. 2 ) then
                            allocate(rinvar2d(1:im,1:jm))
                            allocate(raovar2d(1:im,1:jm))
                            allocate(rvovar2d(1:im,1:jm))
                            allocate(covar2d(1:im,1:jm))
                            rinvar2d = 0.0
                            raovar2d = 0.0
                            rvovar2d = 0.0
                            covar2d = 0
                            
                            allocate(iinvar2d(1:im,1:jm))
                            allocate(iaovar2d(1:im,1:jm))
                            allocate(ivovar2d(1:im,1:jm))

                         elseif( nvdim .eq. 3 ) then
                            allocate(rinvar3d(1:im,1:jm,1:km))
                            allocate(raovar3d(1:im,1:jm,1:km))
                            allocate(rvovar3d(1:im,1:jm,1:km))
                            allocate(covar3d(1:im,1:jm,1:km))
                            rinvar3d = 0.0
                            raovar3d = 0.0
                            rvovar3d = 0.0
                            covar3d = 0
                         
                            allocate(iinvar3d(1:im,1:jm,1:km))
                            allocate(iaovar3d(1:im,1:jm,1:km))
                            allocate(ivovar3d(1:im,1:jm,1:km))

                         elseif( nvdim .eq. 4 ) then
#ifdef bigfile
                            ! See notes above for nvdim .eq. 4
                            ! for the single precision variable
                            ! size allocation.
                            allocate(rinvar4d(1:im,1:jm,1:km,1))
                            allocate(raovar4d(1:im,1:jm,1:km,1))
                            allocate(rvovar4d(1:im,1:jm,1:km,1))
                            allocate(covar4d(1:im,1:jm,1:km,1))

                            allocate(iinvar4d(1:im,1:jm,1:km,1))
                            allocate(iaovar4d(1:im,1:jm,1:km,1))
                            allocate(ivovar4d(1:im,1:jm,1:km,1))
#else
                            allocate(rinvar4d(1:im,1:jm,1:km,1:lm))
                            allocate(raovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(rvovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(covar4d(1:im,1:jm,1:km,1:lm))

                            allocate(iinvar4d(1:im,1:jm,1:km,1:lm))
                            allocate(iaovar4d(1:im,1:jm,1:km,1:lm))
                            allocate(ivovar4d(1:im,1:jm,1:km,1:lm))
#endif
                            rinvar4d = 0.0
                            raovar4d = 0.0
                            rvovar4d = 0.0
                            covar4d = 0

                         else
                            write(*,*) '   Unknown number of dimensions! Crash.'
                            stop
                         endif
                      else
                         write(*,*) '   Unprepared to deal with this var type. SKIP.'
                         !stop
                      endif

                      !----------------------------------------

                      ! Check which filtering attributes are available.
                      lmissing = .false.
                      lfill = .false.
                      lminv = .false.
                      lmaxv = .false.

                      ! Loop over each possible attribute.
                      do j = 1,nvats
                         ! Find the name of the attribute from the number.
                         call ncanam(ifid,vid,j,attname,exitcode)
                         
                         ! Check for missing_value attribute
                         if( index(attname,'missing_value') .ne. 0) lmissing = .true.
                      
                         ! Check for _FillValue attribute
                         if( index(attname,'_FillValue') .ne. 0) lfill = .true.
                      
                         ! Check for min_value attribute
                         if( index(attname,'valid_min') .ne. 0) lminv = .true.
                      
                         ! Check for max_value attribute
                         if( index(attname,'valid_max') .ne. 0) lmaxv = .true.
                      enddo

                      !----------------------------------------                   
                      ! Assign filtering attributes (if available)
                      if( lmissing ) then
                         call ncainq(ifid,vid,'missing_value',atttype,attlen,exitcode)
                      
                         if( atttype .eq. ncfloat ) then
                            call ncagt(ifid,vid,'missing_value',missing_real,exitcode)
                         endif
                         
                         if( atttype .eq. ncdouble ) then
                            call ncagt(ifid,vid,'missing_value',missing_dble,exitcode)
                         endif

                         if( atttype .eq. ncshort ) then
                            call ncagt(ifid,vid,'missing_value',missing_shrt,exitcode)
                            missing_real = float(missing_shrt)
                         endif

                         if( atttype .eq. ncbyte ) then
                            call ncagt(ifid,vid,'missing_value',missing_byte,exitcode)
                            missing_real = float(missing_byte)
                         endif

                         if( atttype .eq. ncint ) then
                            call ncagt(ifid,vid,'missing_value',missing_intg,exitcode)
                            missing_real = float(missing_intg)
                         endif

                      endif
                   
                      if( lfill ) then
                         call ncainq(ifid,vid,'_FillValue',atttype,attlen,exitcode)
                         
                         if( atttype .eq. ncfloat ) then
                            call ncagt(ifid,vid,'_FillValue',fill_real,exitcode)
                         endif
                         
                         if( atttype .eq. ncdouble ) then
                            call ncagt(ifid,vid,'_FillValue',fill_dble,exitcode)
                         endif

                         if( atttype .eq. ncshort ) then
                            call ncagt(ifid,vid,'_FillValue',fill_shrt,exitcode)
                            fill_real = float(fill_shrt)
                         endif

                         if( atttype .eq. ncbyte ) then
                            call ncagt(ifid,vid,'_FillValue',fill_byte,exitcode)
                            fill_real = float(fill_byte)
                         endif

                         if( atttype .eq. ncint ) then
                            call ncagt(ifid,vid,'_FillValue',fill_intg,exitcode)
                            fill_real = float(fill_intg)
                         endif

                      endif

                      if( lminv ) then
                         call ncainq(ifid,vid,'valid_min',atttype,attlen,exitcode)
                         
                         if( atttype .eq. ncfloat ) then
                            call ncagt(ifid,vid,'valid_min',min_real,exitcode)
                         endif
                         
                         if( atttype .eq. ncdouble ) then
                            call ncagt(ifid,vid,'valid_min',min_dble,exitcode)
                         endif

                         if( atttype .eq. ncshort ) then
                            call ncagt(ifid,vid,'valid_min',min_shrt,exitcode)
                            min_real = float(min_shrt)
                         endif

                         if( atttype .eq. ncbyte ) then
                            call ncagt(ifid,vid,'valid_min',min_byte,exitcode)
                            min_real = float(min_byte)
                         endif

                         if( atttype .eq. ncint ) then
                            call ncagt(ifid,vid,'valid_min',min_intg,exitcode)
                            min_real = float(min_intg)
                         endif

                      endif
                      
                      if( lmaxv ) then
                         call ncainq(ifid,vid,'valid_max',atttype,attlen,exitcode)
                         
                         if( atttype .eq. ncfloat ) then
                            call ncagt(ifid,vid,'valid_max',max_real,exitcode)
                         endif
                         
                         if( atttype .eq. ncdouble ) then
                            call ncagt(ifid,vid,'valid_max',max_dble,exitcode)
                         endif

                         if( atttype .eq. ncshort ) then
                            call ncagt(ifid,vid,'valid_max',max_shrt,exitcode)
                            max_real = float(max_shrt)
                         endif

                         if( atttype .eq. ncbyte ) then
                            call ncagt(ifid,vid,'valid_max',max_byte,exitcode)
                            max_real = float(max_byte)
                         endif

                         if( atttype .eq. ncint ) then
                            call ncagt(ifid,vid,'valid_max',max_intg,exitcode)
                            max_real = float(max_intg)
                         endif

                      endif
                   
                      !----------------------------------------
                      ! If we have missing but no fill values (ORCA)
                      ! then set fill = missing.
                      if ( lmissing ) then
                         if ( lfill ) then
                            ! Do nothing to modify current
                            ! fill values.
                         else
                            fill_real = missing_real
                            fill_dble = missing_dble
                         endif
                      endif
                   endif
                   !--------End of reading variable info-------
#ifdef bigfile
                   ! For big files, need to change read
                   ! and write start and counts because 
                   ! we will be averaging time step by 
                   ! time step (each increment along 4th
                   ! dimension, which is usually time).
                   ! These read_ and writ_ values
                   ! will change as we loop over time steps.
                   if( nvdim .eq. 4 ) then
                      readstart(4) = lc
                      writstart(4) = lc
                      readcount(4) = 1
                      writcount(4) = 1
                   endif

                   ! Also, need to reinitialize the 
                   ! online averages, variances, and
                   ! counters to zero.  Otherwise, the
                   ! result is the average over the
                   ! course of the year!  This only
                   ! needs to be done for the 4D
                   ! variables.  Note that this is
                   ! done only when the file number
                   ! is equal to 1 (starting the
                   ! averaging all over again).
                   if( (nvdim .eq. 4).and.(n .eq. 1) ) then
#ifdef verbose
                      write(*,*) '  Zeroing out the online avgs, etc...'
#endif
                      if ( (vart .eq. ncfloat) .or. (vart .eq. ncshort) .or. &
                           (vart .eq. ncint) .or. (vart .eq. ncbyte) ) then
                         rinvar4d = 0.0
                         raovar4d = 0.0
                         rvovar4d = 0.0
                      elseif ( vart .eq. ncdouble ) then
                         dinvar4d = 0.0
                         daovar4d = 0.0
                         dvovar4d = 0.0
                      endif
                      covar4d = 0
                   endif
#endif                   
                   !-----------------Load data-----------------
                   if( vartype(v) .eq. ncfloat ) then
                   
                      if( nvdims(v) .eq. 1 ) then
                         call ncvgt(ifid,vid,readstart,readcount,rinvar1d,exitcode)
                      elseif( nvdims(v) .eq. 2 ) then
                         call ncvgt(ifid,vid,readstart,readcount,rinvar2d,exitcode)
                      elseif( nvdims(v) .eq. 3 ) then
                         call ncvgt(ifid,vid,readstart,readcount,rinvar3d,exitcode)
                      elseif( nvdims(v) .eq. 4 ) then
                         call ncvgt(ifid,vid,readstart,readcount,rinvar4d,exitcode)
                      elseif( nvdims(v) .eq. 0 ) then
                         write(*,*) '   No dimensions for this variable.  Skip.'
                      else
                         write(*,*) '    Unknown number of dimensions! Crash.'
                         stop
                      endif
                   
                   elseif( vartype(v) .eq. ncdouble ) then
                   
                      if( nvdims(v) .eq. 1 ) then
                         call ncvgt(ifid,vid,readstart,readcount,dinvar1d,exitcode)
                      elseif( nvdims(v) .eq. 2 ) then
                         call ncvgt(ifid,vid,readstart,readcount,dinvar2d,exitcode)
                      elseif( nvdims(v) .eq. 3 ) then
                         call ncvgt(ifid,vid,readstart,readcount,dinvar3d,exitcode)
                      elseif( nvdims(v) .eq. 4 ) then
                         call ncvgt(ifid,vid,readstart,readcount,dinvar4d,exitcode)
                      elseif( nvdims(v) .eq. 0 ) then
                         write(*,*) '   No dimensions for this variable.  Skip.'
                      else
                         write(*,*) '   Unknown number of dimensions! Crash.'
                         stop
                      endif

                   elseif( vartype(v) .eq. ncshort ) then
                   
                      ! Get integer data from the file.
                      ! Immediately copy and convert this
                      ! integer data to real data.
                      if( nvdims(v) .eq. 1 ) then
                         call ncvgt(ifid,vid,readstart,readcount,sinvar1d,exitcode)
                         rinvar1d = float(sinvar1d)
                      elseif( nvdims(v) .eq. 2 ) then
                         call ncvgt(ifid,vid,readstart,readcount,sinvar2d,exitcode)
                         rinvar2d = float(sinvar2d)
                      elseif( nvdims(v) .eq. 3 ) then
                         call ncvgt(ifid,vid,readstart,readcount,sinvar3d,exitcode)
                         rinvar3d = float(sinvar3d)
                      elseif( nvdims(v) .eq. 4 ) then
                         call ncvgt(ifid,vid,readstart,readcount,sinvar4d,exitcode)
                         rinvar4d = float(sinvar4d)
                      elseif( nvdims(v) .eq. 0 ) then
                         write(*,*) '   No dimensions for this variable.  Skip.'
                      else
                         write(*,*) '   Unknown number of dimensions! Crash.'
                         stop
                      endif

                   elseif( vartype(v) .eq. ncbyte ) then
                   
                      ! Get integer data from the file.
                      ! Immediately copy and convert this
                      ! integer data to real data.
                      if( nvdims(v) .eq. 1 ) then
                         call ncvgt(ifid,vid,readstart,readcount,binvar1d,exitcode)
                         rinvar1d = float(binvar1d)
                      elseif( nvdims(v) .eq. 2 ) then
                         call ncvgt(ifid,vid,readstart,readcount,binvar2d,exitcode)
                         rinvar2d = float(binvar2d)
                      elseif( nvdims(v) .eq. 3 ) then
                         call ncvgt(ifid,vid,readstart,readcount,binvar3d,exitcode)
                         rinvar3d = float(binvar3d)
                      elseif( nvdims(v) .eq. 4 ) then
                         call ncvgt(ifid,vid,readstart,readcount,binvar4d,exitcode)
                         rinvar4d = float(binvar4d)
                      elseif( nvdims(v) .eq. 0 ) then
                         write(*,*) '   No dimensions for this variable.  Skip.'
                      else
                         write(*,*) '   Unknown number of dimensions! Crash.'
                         stop
                      endif

                   elseif( vartype(v) .eq. ncint ) then
                   
                      ! Get integer data from the file.
                      ! Immediately copy and convert this
                      ! integer data to real data.
                      if( nvdims(v) .eq. 1 ) then
                         call ncvgt(ifid,vid,readstart,readcount,iinvar1d,exitcode)
                         rinvar1d = float(iinvar1d)
                      elseif( nvdims(v) .eq. 2 ) then
                         call ncvgt(ifid,vid,readstart,readcount,iinvar2d,exitcode)
                         rinvar2d = float(iinvar2d)
                      elseif( nvdims(v) .eq. 3 ) then
                         call ncvgt(ifid,vid,readstart,readcount,iinvar3d,exitcode)
                         rinvar3d = float(iinvar3d)
                      elseif( nvdims(v) .eq. 4 ) then
                         call ncvgt(ifid,vid,readstart,readcount,iinvar4d,exitcode)
                         rinvar4d = float(iinvar4d)
                      elseif( nvdims(v) .eq. 0 ) then
                         write(*,*) '   No dimensions for this variable.  Skip.'
                      else
                         write(*,*) '   Unknown number of dimensions! Crash.'
                         stop
                      endif

                   else
                      write(*,*) '   Unprepared to deal with this var type. SKIP.'
                      !stop
                   endif
                
                   if(exitcode .eq. 0) then
                      !write(*,*) '   Read variable from infile.'
                   else
                      write(*,*) '   Error reading variable from infile. Crash.'
                      stop
                   endif
                   !----------Done loading Data-------------

                   !----------------------------------------
                   ! Update the running sum and counter
                   ! provided data is clean.  Two different
                   ! verions: one for data with fill or
                   ! missing values, and one for data which
                   ! doesn't (explicitly) have fill or missing
                   ! values.

                   if( lmissing .or. lfill ) then
                      ! We check for missing or fill values.

                      if( (vartype(v) .eq. ncfloat) .or. (vartype(v) .eq. ncshort) .or. &
                           (vartype(v) .eq. ncint) .or. (vartype(v) .eq. ncbyte) ) then
                         if( nvdims(v) .eq. 1 ) then
                            do i = 1,im
                               if ( ( rinvar1d(i) .eq. missing_real ) .or. &
                                    ( rinvar1d(i) .eq. fill_real ) ) then
                                  ! Skip value in averaging.
                               else
                                  ! Include value in averaging.
                                  call olavgvar(rinvar1d(i),covar1d(i),raovar1d(i),rvovar1d(i))
                               endif
                            enddo
                         elseif( nvdims(v) .eq. 2 ) then
                            do j = 1,jm
                               do i = 1,im
                                  if ( ( rinvar2d(i,j) .eq. missing_real ) .or. &
                                       ( rinvar2d(i,j) .eq. fill_real ) ) then
                                     ! Skip value in averaging.
                                  else
                                     ! Include value in averaging.
                                     call olavgvar(rinvar2d(i,j),covar2d(i,j),raovar2d(i,j),rvovar2d(i,j))
                                  endif
                               enddo
                            enddo
                         elseif( nvdims(v) .eq. 3 ) then
                            do k = 1,km
                               do j = 1,jm
                                  do i = 1,im
                                     if ( ( rinvar3d(i,j,k) .eq. missing_real ) .or. &
                                          ( rinvar3d(i,j,k) .eq. fill_real ) ) then
                                        ! Skip value in averaging.
                                     else
                                        ! Include value in averaging.
                                        call olavgvar(rinvar3d(i,j,k),covar3d(i,j,k),raovar3d(i,j,k),rvovar3d(i,j,k))
                                     endif
                                  enddo
                               enddo
                            enddo
                         elseif( nvdims(v) .eq. 4 ) then
#ifdef bigfile
                            do k = 1,km
                               do j = 1,jm
                                  do i = 1,im
                                     if ( ( rinvar4d(i,j,k,1) .eq. missing_real ) .or. &
                                          ( rinvar4d(i,j,k,1) .eq. fill_real ) ) then
                                        ! Skip value in averaging.
                                     else
                                        ! Include value in averaging.
                                        call olavgvar(rinvar4d(i,j,k,1),covar4d(i,j,k,1),raovar4d(i,j,k,1),rvovar4d(i,j,k,1))
                                     endif
                                  enddo
                               enddo
                            enddo
#else
                            do l = 1,lm
                               do k = 1,km
                                  do j = 1,jm
                                     do i = 1,im
                                        if ( ( rinvar4d(i,j,k,l) .eq. missing_real ) .or. &
                                             ( rinvar4d(i,j,k,l) .eq. fill_real ) ) then
                                           ! Skip value in averaging.
                                        else
                                           ! Include value in averaging.
                                           call olavgvar(rinvar4d(i,j,k,l),covar4d(i,j,k,l),raovar4d(i,j,k,l),rvovar4d(i,j,k,l))
                                        endif
                                     enddo
                                  enddo
                               enddo
                            enddo
#endif
                         elseif( nvdims(v) .eq. 0 ) then
                            write(*,*) '   No dimensions for this variable.  Skip.'
                         else
                            write(*,*) '   Unknown number of dimensions! Crash.'
                            stop
                         endif
                      
                      elseif( vartype(v) .eq. ncdouble ) then
                         if( nvdims(v) .eq. 1 ) then
                            do i = 1,im
                               if ( ( dinvar1d(i) .eq. missing_dble ) .or. &
                                    ( dinvar1d(i) .eq. fill_dble ) ) then
                                  ! Skip value in averaging.
                               else
                                  ! Include value in averaging.
                                  call olavgvar_dble(dinvar1d(i),covar1d(i),daovar1d(i),dvovar1d(i))
                               endif
                            enddo
                         elseif( nvdims(v) .eq. 2 ) then
                            do j = 1,jm
                               do i = 1,im
                                  if ( ( dinvar2d(i,j) .eq. missing_dble ) .or. &
                                       ( dinvar2d(i,j) .eq. fill_dble ) ) then
                                     ! Skip value in averaging.
                                  else
                                     ! Include value in averaging.
                                     call olavgvar_dble(dinvar2d(i,j),covar2d(i,j),daovar2d(i,j),dvovar2d(i,j))
                                  endif
                               enddo
                            enddo
                         elseif( nvdims(v) .eq. 3 ) then
                            do k = 1,km
                               do j = 1,jm
                                  do i = 1,im
                                     if ( ( dinvar3d(i,j,k) .eq. missing_dble ) .or. &
                                          ( dinvar3d(i,j,k) .eq. fill_dble ) ) then
                                        ! Skip value in averaging.
                                     else
                                        ! Include value in averaging.
                                        call olavgvar_dble(dinvar3d(i,j,k),covar3d(i,j,k),daovar3d(i,j,k),dvovar3d(i,j,k))
                                     endif
                                  enddo
                               enddo
                            enddo
                         elseif( nvdims(v) .eq. 4 ) then
#ifdef bigfile
                            do k = 1,km
                               do j = 1,jm
                                  do i = 1,im
                                     if ( ( dinvar4d(i,j,k,1) .eq. missing_dble ) .or. &
                                          ( dinvar4d(i,j,k,1) .eq. fill_dble ) ) then
                                        ! Skip value in averaging.
                                     else
                                        ! Include value in averaging.
                                        call olavgvar_dble(dinvar4d(i,j,k,1),covar4d(i,j,k,1),daovar4d(i,j,k,1),dvovar4d(i,j,k,1))
                                     endif
                                  enddo
                               enddo
                            enddo
#else
                            do l = 1,lm
                               do k = 1,km
                                  do j = 1,jm
                                     do i = 1,im
                                        if ( ( dinvar4d(i,j,k,l) .eq. missing_dble ) .or. &
                                             ( dinvar4d(i,j,k,l) .eq. fill_dble ) ) then
                                           ! Skip value in averaging.
                                        else
                                           ! Include value in averaging.
                                           call olavgvar_dble(dinvar4d(i,j,k,l),&
                                                covar4d(i,j,k,l),daovar4d(i,j,k,l),dvovar4d(i,j,k,l))
                                        endif
                                     enddo
                                  enddo
                               enddo
                            enddo
#endif
                         elseif( nvdims(v) .eq. 0 ) then
                            write(*,*) '   No dimensions for this variable.  Skip.'
                         else
                            write(*,*) '   Unknown number of dimensions! Crash.'
                            stop
                         endif
                      
                      else
                         write(*,*) '   Unprepared to deal with this var type. SKIP.'
                         !stop
                      endif
                   
                   else
                      ! No missing or fill value check is possible.
                      ! Perform averaging over all values.
                      if( (vartype(v) .eq. ncfloat) .or. (vartype(v) .eq. ncshort) .or. &
                           (vartype(v) .eq. ncint) .or. (vartype(v) .eq. ncbyte) ) then
                         if( nvdims(v) .eq. 1 ) then
                            do i = 1,im
                               call olavgvar(rinvar1d(i),covar1d(i),raovar1d(i),rvovar1d(i))                 
                            enddo
                         elseif( nvdims(v) .eq. 2 ) then
                            do j = 1,jm
                               do i = 1,im
                                  call olavgvar(rinvar2d(i,j),covar2d(i,j),raovar2d(i,j),rvovar2d(i,j))
                               enddo
                            enddo
                         elseif( nvdims(v) .eq. 3 ) then
                            do k = 1,km
                               do j = 1,jm
                                  do i = 1,im
                                     call olavgvar(rinvar3d(i,j,k),covar3d(i,j,k),raovar3d(i,j,k),rvovar3d(i,j,k))
                                  enddo
                               enddo
                            enddo
                         elseif( nvdims(v) .eq. 4 ) then
#ifdef bigfile
                            do k = 1,km
                               do j = 1,jm
                                  do i = 1,im
                                     call olavgvar(rinvar4d(i,j,k,1),covar4d(i,j,k,1),raovar4d(i,j,k,1),rvovar4d(i,j,k,1))
                                  enddo
                               enddo
                            enddo
#else
                            do l = 1,lm
                               do k = 1,km
                                  do j = 1,jm
                                     do i = 1,im
                                        call olavgvar(rinvar4d(i,j,k,l),covar4d(i,j,k,l),raovar4d(i,j,k,l),rvovar4d(i,j,k,l))
                                     enddo
                                  enddo
                               enddo
                            enddo
#endif
                         elseif( nvdims(v) .eq. 0 ) then
                            write(*,*) '   No dimensions for this variable.  Skip.'
                         else
                            write(*,*) '   Unknown number of dimensions! Crash.'
                            stop
                         endif
                      
                      elseif( vartype(v) .eq. ncdouble ) then
                         if( nvdims(v) .eq. 1 ) then
                            do i = 1,im
                               call olavgvar_dble(dinvar1d(i),covar1d(i),daovar1d(i),dvovar1d(i))
                            enddo
                         elseif( nvdims(v) .eq. 2 ) then
                            do j = 1,jm
                               do i = 1,im
                                  call olavgvar_dble(dinvar2d(i,j),covar2d(i,j),daovar2d(i,j),dvovar2d(i,j))
                               enddo
                            enddo
                         elseif( nvdims(v) .eq. 3 ) then
                            do k = 1,km
                               do j = 1,jm
                                  do i = 1,im
                                     call olavgvar_dble(dinvar3d(i,j,k),covar3d(i,j,k),daovar3d(i,j,k),dvovar3d(i,j,k))
                                  enddo
                               enddo
                            enddo
                         elseif( nvdims(v) .eq. 4 ) then
#ifdef bigfile
                            do k = 1,km
                               do j = 1,jm
                                  do i = 1,im
                                     call olavgvar_dble(dinvar4d(i,j,k,1),covar4d(i,j,k,1),daovar4d(i,j,k,1),dvovar4d(i,j,k,1))
                                  enddo
                               enddo
                            enddo
#else
                            do l = 1,lm
                               do k = 1,km
                                  do j = 1,jm
                                     do i = 1,im
                                        call olavgvar_dble(dinvar4d(i,j,k,l),covar4d(i,j,k,l),daovar4d(i,j,k,l),dvovar4d(i,j,k,l))
                                     enddo
                                  enddo
                               enddo
                            enddo
#endif
                         elseif( nvdims(v) .eq. 0 ) then
                            write(*,*) '   No dimensions for this variable.  Skip.'
                         else
                            write(*,*) '   Unknown number of dimensions! Crash.'
                            stop
                         endif
                      else
                         write(*,*) '   Unprepared to deal with this var type. SKIP.'
                         !stop
                      endif ! End of variable type check
                   endif   ! End of check of presence of missing or fill values.
                endif  ! End of variable presence check.
#ifdef verbose
                write(*,*) '   Closing infile...'
#endif
                call ncclos(ifid,exitcode)
                if(exitcode .eq. 0) then
                   !write(*,*) '   Infile closed.'
                else
                   write(*,*) '   Unable to close infile. Crash'
                   stop
                endif
                
                !----------------------------------------
             enddo ! End loop over input files.
             !----------------------------------------
             ! (1) Final computations for normalizing the
             !     variance.

             ! (2) Checking for fill/missing values.
             !
             !     If the variable has missing or fill
             !     values, any values with zero counts
             !     will be set as missing values in both
             !     the mean and variance fields.
             !
             !     If the variable does not have (explicit)
             !     missing values, then zero counts result
             !     in zero variances and no change to avg.
             if( (vartype(v) .eq. ncfloat) .or. (vartype(v) .eq. ncshort) .or. &
                  (vartype(v) .eq. ncint) .or. (vartype(v) .eq. ncbyte) ) then

                if( nvdims(v) .eq. 1 ) then
                   do i = 1,im
                      if( covar1d(i) .eq. 0 ) then
                         ! Flag the variance and average fields
                         if( lmissing .or. lfill ) then
                            rvovar1d(i) = missing_real
                            raovar1d(i) = missing_real
                         else
                            rvovar1d(i) = 0.0
                         endif
                      else
                         ! Compute the variance.
                         rvovar1d(i) = rvovar1d(i)/float(covar1d(i))
                      endif
                   enddo
                elseif( nvdims(v) .eq. 2 ) then
                   do j = 1,jm
                      do i = 1,im
                         if( covar2d(i,j) .eq. 0 ) then
                            ! Flag the variance and average fields
                            if( lmissing .or. lfill ) then
                               rvovar2d(i,j) = missing_real
                               raovar2d(i,j) = missing_real
                            else
                               rvovar2d(i,j) = 0.0
                            endif
                         else
                            ! Compute the variance.
                            rvovar2d(i,j) = rvovar2d(i,j)/float(covar2d(i,j))
                         endif
                      enddo
                   enddo
                elseif( nvdims(v) .eq. 3 ) then
                   do k = 1,km
                      do j = 1,jm
                         do i = 1,im
                            if( covar3d(i,j,k) .eq. 0 ) then
                               ! Flag the variance and average fields
                               if( lmissing .or. lfill ) then
                                  rvovar3d(i,j,k) = missing_real
                                  raovar3d(i,j,k) = missing_real
                               else
                                  rvovar3d(i,j,k) = 0.0
                               endif
                            else
                               ! Compute the variance.
                               rvovar3d(i,j,k) = rvovar3d(i,j,k)/float(covar3d(i,j,k))
                            endif
                         enddo
                      enddo
                   enddo
                elseif( nvdims(v) .eq. 4 ) then
#ifdef bigfile
                   do k = 1,km
                      do j = 1,jm
                         do i = 1,im
                            if( covar4d(i,j,k,1) .eq. 0 ) then
                               ! Flag the variance and average fields
                               if( lmissing .or. lfill ) then
                                  rvovar4d(i,j,k,1) = missing_real
                                  raovar4d(i,j,k,1) = missing_real
                               else
                                  rvovar4d(i,j,k,1) = 0.0
                               endif
                            else
                               ! Compute the variance.
                               rvovar4d(i,j,k,1) = rvovar4d(i,j,k,1)/float(covar4d(i,j,k,1))
                            endif
                         enddo
                      enddo
                   enddo
#else
                   do l = 1,lm
                      do k = 1,km
                         do j = 1,jm
                            do i = 1,im
                               if( covar4d(i,j,k,l) .eq. 0 ) then
                                  ! Flag the variance and average fields
                                  if( lmissing .or. lfill ) then
                                     rvovar4d(i,j,k,l) = missing_real
                                     raovar4d(i,j,k,l) = missing_real
                                  else
                                     rvovar4d(i,j,k,l) = 0.0
                                  endif
                               else
                                  ! Compute the variance.
                                  rvovar4d(i,j,k,l) = rvovar4d(i,j,k,l)/float(covar4d(i,j,k,l))
                               endif
                            enddo
                         enddo
                      enddo
                   enddo
#endif
                elseif( nvdims(v) .eq. 0 ) then
                   write(*,*) '   No dimensions for this variable.  Skip.'
                else
                   write(*,*) '    Unknown number of dimensions! Crash.'
                   stop
                endif

             elseif( vartype(v) .eq. ncdouble ) then
                if( nvdims(v) .eq. 1 ) then
                   do i = 1,im
                      if( covar1d(i) .eq. 0 ) then
                         ! Flag the variance and average fields
                         if( lmissing .or. lfill ) then
                            dvovar1d(i) = missing_dble
                            daovar1d(i) = missing_dble
                         else
                            dvovar1d(i) = 0.0
                         endif
                      else
                         ! Compute the variance.
                         dvovar1d(i) = dvovar1d(i)/dble(covar1d(i))
                      endif
                   enddo
                elseif( nvdims(v) .eq. 2 ) then
                   do j = 1,jm
                      do i = 1,im
                         if( covar2d(i,j) .eq. 0 ) then
                            ! Flag the variance and average fields
                            if( lmissing .or. lfill ) then
                               dvovar2d(i,j) = missing_dble
                               daovar2d(i,j) = missing_dble
                            else
                               dvovar2d(i,j) = 0.0
                            endif
                         else
                            ! Compute the variance.
                            dvovar2d(i,j) = dvovar2d(i,j)/dble(covar2d(i,j))
                         endif
                      enddo
                   enddo
                elseif( nvdims(v) .eq. 3 ) then
                   do k = 1,km
                      do j = 1,jm
                         do i = 1,im
                            if( covar3d(i,j,k) .eq. 0 ) then
                               ! Flag the variance and average fields
                               if( lmissing .or. lfill ) then
                                  dvovar3d(i,j,k) = missing_dble
                                  daovar3d(i,j,k) = missing_dble
                               else
                                  dvovar3d(i,j,k) = 0.0
                               endif
                            else
                               ! Compute the variance.
                               dvovar3d(i,j,k) = dvovar3d(i,j,k)/dble(covar3d(i,j,k))
                            endif
                         enddo
                      enddo
                   enddo
                elseif( nvdims(v) .eq. 4 ) then
#ifdef bigfile
                   do k = 1,km
                      do j = 1,jm
                         do i = 1,im
                            if( covar4d(i,j,k,1) .eq. 0 ) then
                               ! Flag the variance and average fields
                               if( lmissing .or. lfill ) then
                                  dvovar4d(i,j,k,1) = missing_dble
                                  daovar4d(i,j,k,1) = missing_dble
                               else
                                  dvovar4d(i,j,k,1) = 0.0
                               endif
                            else
                               ! Compute the variance.
                               dvovar4d(i,j,k,1) = dvovar4d(i,j,k,1)/dble(covar4d(i,j,k,1))
                            endif
                         enddo
                      enddo
                   enddo
#else
                   do l = 1,lm
                      do k = 1,km
                         do j = 1,jm
                            do i = 1,im
                               if( covar4d(i,j,k,l) .eq. 0 ) then
                                  ! Flag the variance and average fields
                                  if( lmissing .or. lfill ) then
                                     dvovar4d(i,j,k,l) = missing_dble
                                     daovar4d(i,j,k,l) = missing_dble
                                  else
                                     dvovar4d(i,j,k,l) = 0.0
                                  endif
                               else
                                  ! Compute the variance.
                                  dvovar4d(i,j,k,l) = dvovar4d(i,j,k,l)/dble(covar4d(i,j,k,l))
                               endif
                            enddo
                         enddo
                      enddo
                   enddo
#endif
                elseif( nvdims(v) .eq. 0 ) then
                   write(*,*) '   No dimensions for this variable.  Skip.'
                else
                   write(*,*) '   Unknown number of dimensions! Crash.'
                   stop
                endif
             else
                write(*,*) '   Unprepared to deal with this var type. SKIP.'
                !stop
             endif

             !----------------------------------------
#ifdef verbose
             write(*,*) '   Writing mean and variance to output...'
#endif
             ! Note that during the writing process, we
             ! check for any variable names with the
             ! text:
             !
             ! longitude, latitude, _lon, _lat, LONGITUDE,
             ! LATITUDE, Longitude, Latitude, depth, DEPTH,
             ! Depth, time, TIME, or Time
             ! 
             ! If we come across such a variable, instead
             ! of writing the variance of the variable to
             ! the variance output file, we write the
             ! mean value of the variable.  This is done
             ! so that the coordinate systems are
             ! preserved in the variance output files.
             
             ! Test for coordinate variable
             lcoord = .false.
             if(  (index(varname(v), 'Longitude').ne.0) .or. &
                  (index(varname(v), 'longitude').ne.0) .or. &
                  (index(varname(v), 'LONGITUDE').ne.0) .or. &
                  (index(varname(v), '_LON').ne.0) .or. &
                  (index(varname(v), '_Lon').ne.0) .or. &
                  (index(varname(v), '_lon').ne.0) .or. &
                  (index(varname(v), 'Latitude').ne.0) .or. &
                  (index(varname(v), 'latitude').ne.0) .or. &
                  (index(varname(v), 'LATITUDE').ne.0) .or. &
                  (index(varname(v), '_LAT').ne.0) .or. &
                  (index(varname(v), '_Lat').ne.0) .or. &
                  (index(varname(v), '_lat').ne.0) .or. &
                  (index(varname(v), 'Depth').ne.0) .or. &
                  (index(varname(v), 'depth').ne.0) .or. &
                  (index(varname(v), 'DEPTH').ne.0) .or. &
                  (index(varname(v), '_DEP').ne.0) .or. &
                  (index(varname(v), '_Dep').ne.0) .or. &
                  (index(varname(v), '_dep').ne.0) .or. &
                  (index(varname(v), 'Time').ne.0) .or. &
                  (index(varname(v), 'time').ne.0) .or. &
                  (index(varname(v), 'TIME').ne.0) .or. &
                  (index(varname(v), '_TIM').ne.0) .or. &
                  (index(varname(v), '_Tim').ne.0) .or. &
                  (index(varname(v), '_tim').ne.0) ) lcoord = .true.

             if( vartype(v) .eq. ncfloat ) then
                if( nvdims(v) .eq. 1 ) then
                   call ncvpt(avgofid,aovid(v),writstart,writcount,raovar1d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      call ncvpt(varofid,vovid(v),writstart,writcount,raovar1d,exitcode)
                      !write(*,*) ' NOTE: Coordinate Variable!'
                   else
                      call ncvpt(varofid,vovid(v),writstart,writcount,rvovar1d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 2 ) then
                   call ncvpt(avgofid,aovid(v),writstart,writcount,raovar2d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      call ncvpt(varofid,vovid(v),writstart,writcount,raovar2d,exitcode)
                      !write(*,*) ' NOTE: Coordinate Variable!'
                   else
                      call ncvpt(varofid,vovid(v),writstart,writcount,rvovar2d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 3 ) then
                   call ncvpt(avgofid,aovid(v),writstart,writcount,raovar3d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      call ncvpt(varofid,vovid(v),writstart,writcount,raovar3d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      call ncvpt(varofid,vovid(v),writstart,writcount,rvovar3d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 4 ) then
                   call ncvpt(avgofid,aovid(v),writstart,writcount,raovar4d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      call ncvpt(varofid,vovid(v),writstart,writcount,raovar4d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      call ncvpt(varofid,vovid(v),writstart,writcount,rvovar4d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 0 ) then
                   write(*,*) '   No dimensions for this variable.  Skip.'
                else
                   write(*,*) ' Unknown number of dimensions! Crash.'
                   stop
                endif
             
             elseif( vartype(v) .eq. ncdouble ) then
                if( nvdims(v) .eq. 1 ) then
                   call ncvpt(avgofid,aovid(v),writstart,writcount,daovar1d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      call ncvpt(varofid,vovid(v),writstart,writcount,daovar1d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      call ncvpt(varofid,vovid(v),writstart,writcount,dvovar1d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 2 ) then
                   call ncvpt(avgofid,aovid(v),writstart,writcount,daovar2d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      call ncvpt(varofid,vovid(v),writstart,writcount,daovar2d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      call ncvpt(varofid,vovid(v),writstart,writcount,dvovar2d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 3 ) then
                   call ncvpt(avgofid,aovid(v),writstart,writcount,daovar3d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      call ncvpt(varofid,vovid(v),writstart,writcount,daovar3d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      call ncvpt(varofid,vovid(v),writstart,writcount,dvovar3d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 4 ) then
                   call ncvpt(avgofid,aovid(v),writstart,writcount,daovar4d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      call ncvpt(varofid,vovid(v),writstart,writcount,daovar4d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      call ncvpt(varofid,vovid(v),writstart,writcount,dvovar4d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 0 ) then
                   write(*,*) '   No dimensions for this variable.  Skip.'
                else
                   write(*,*) ' Unknown number of dimensions! Crash.'
                   stop
                endif

             elseif( vartype(v) .eq. ncshort ) then
                ! Convert to short integers
                if( nvdims(v) .eq. 1 ) then
                   saovar1d = int(anint(raovar1d),2)
                   call ncvpt(avgofid,aovid(v),writstart,writcount,saovar1d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      saovar1d = int(anint(raovar1d),2)
                      call ncvpt(varofid,vovid(v),writstart,writcount,saovar1d,exitcode)
                      !write(*,*) ' NOTE: Coordinate Variable!'
                   else
                      svovar1d = int(anint(rvovar1d),2)
                      call ncvpt(varofid,vovid(v),writstart,writcount,svovar1d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 2 ) then
                   saovar2d = int(anint(raovar2d),2)
                   call ncvpt(avgofid,aovid(v),writstart,writcount,saovar2d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      saovar2d = int(anint(raovar2d),2)
                      call ncvpt(varofid,vovid(v),writstart,writcount,saovar2d,exitcode)
                      !write(*,*) ' NOTE: Coordinate Variable!'
                   else
                      svovar2d = int(anint(rvovar2d),2)
                      call ncvpt(varofid,vovid(v),writstart,writcount,svovar2d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 3 ) then
                   saovar3d = int(anint(raovar3d),2)
                   call ncvpt(avgofid,aovid(v),writstart,writcount,saovar3d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      saovar3d = int(anint(raovar3d),2)
                      call ncvpt(varofid,vovid(v),writstart,writcount,saovar3d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      svovar3d = int(anint(rvovar3d),2)
                      call ncvpt(varofid,vovid(v),writstart,writcount,svovar3d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 4 ) then
                   saovar4d = int(anint(raovar4d),2)
                   call ncvpt(avgofid,aovid(v),writstart,writcount,saovar4d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      saovar4d = int(anint(raovar4d),2)
                      call ncvpt(varofid,vovid(v),writstart,writcount,saovar4d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      svovar4d = int(anint(rvovar4d),2)
                      call ncvpt(varofid,vovid(v),writstart,writcount,svovar4d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 0 ) then
                   write(*,*) '   No dimensions for this variable.  Skip.'
                else
                   write(*,*) ' Unknown number of dimensions! Crash.'
                   stop
                endif

             elseif( vartype(v) .eq. ncbyte ) then
                ! Convert to byte integers
                if( nvdims(v) .eq. 1 ) then
                   baovar1d = int(anint(raovar1d),1)
                   call ncvpt(avgofid,aovid(v),writstart,writcount,baovar1d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      baovar1d = int(anint(raovar1d),1)
                      call ncvpt(varofid,vovid(v),writstart,writcount,baovar1d,exitcode)
                      !write(*,*) ' NOTE: Coordinate Variable!'
                   else
                      bvovar1d = int(anint(rvovar1d),1)
                      call ncvpt(varofid,vovid(v),writstart,writcount,bvovar1d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 2 ) then
                   baovar2d = int(anint(raovar2d),1)
                   call ncvpt(avgofid,aovid(v),writstart,writcount,baovar2d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      baovar2d = int(anint(raovar2d),1)
                      call ncvpt(varofid,vovid(v),writstart,writcount,baovar2d,exitcode)
                      !write(*,*) ' NOTE: Coordinate Variable!'
                   else
                      bvovar2d = int(anint(rvovar2d),1)
                      call ncvpt(varofid,vovid(v),writstart,writcount,bvovar2d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 3 ) then
                   baovar3d = int(anint(raovar3d),1)
                   call ncvpt(avgofid,aovid(v),writstart,writcount,baovar3d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      baovar3d = int(anint(raovar3d),1)
                      call ncvpt(varofid,vovid(v),writstart,writcount,baovar3d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      bvovar3d = int(anint(rvovar3d),1)
                      call ncvpt(varofid,vovid(v),writstart,writcount,bvovar3d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 4 ) then
                   baovar4d = int(anint(raovar4d),1)
                   call ncvpt(avgofid,aovid(v),writstart,writcount,baovar4d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      baovar4d = int(anint(raovar4d),1)
                      call ncvpt(varofid,vovid(v),writstart,writcount,baovar4d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      bvovar4d = int(anint(rvovar4d),1)
                      call ncvpt(varofid,vovid(v),writstart,writcount,bvovar4d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 0 ) then
                   write(*,*) '   No dimensions for this variable.  Skip.'
                else
                   write(*,*) ' Unknown number of dimensions! Crash.'
                   stop
                endif

             elseif( vartype(v) .eq. ncint ) then
                ! Convert to short integers
                if( nvdims(v) .eq. 1 ) then
                   iaovar1d = int(anint(raovar1d))
                   call ncvpt(avgofid,aovid(v),writstart,writcount,iaovar1d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      iaovar1d = int(anint(raovar1d))
                      call ncvpt(varofid,vovid(v),writstart,writcount,iaovar1d,exitcode)
                      !write(*,*) ' NOTE: Coordinate Variable!'
                   else
                      ivovar1d = int(anint(rvovar1d))
                      call ncvpt(varofid,vovid(v),writstart,writcount,ivovar1d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 2 ) then
                   iaovar2d = int(anint(raovar2d))
                   call ncvpt(avgofid,aovid(v),writstart,writcount,iaovar2d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      iaovar2d = int(anint(raovar2d))
                      call ncvpt(varofid,vovid(v),writstart,writcount,iaovar2d,exitcode)
                      !write(*,*) ' NOTE: Coordinate Variable!'
                   else
                      ivovar2d = int(anint(rvovar2d))
                      call ncvpt(varofid,vovid(v),writstart,writcount,ivovar2d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 3 ) then
                   iaovar3d = int(anint(raovar3d))
                   call ncvpt(avgofid,aovid(v),writstart,writcount,iaovar3d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      iaovar3d = int(anint(raovar3d))
                      call ncvpt(varofid,vovid(v),writstart,writcount,iaovar3d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      ivovar3d = int(anint(rvovar3d))
                      call ncvpt(varofid,vovid(v),writstart,writcount,ivovar3d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 4 ) then
                   iaovar4d = int(anint(raovar4d))
                   call ncvpt(avgofid,aovid(v),writstart,writcount,iaovar4d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                   if(lcoord) then
                      iaovar4d = int(anint(raovar4d))
                      call ncvpt(varofid,vovid(v),writstart,writcount,iaovar4d,exitcode)
                      !write(*,*) ' NOTE: Coordiante Variable!'
                   else
                      ivovar4d = int(anint(rvovar4d))
                      call ncvpt(varofid,vovid(v),writstart,writcount,ivovar4d,exitcode)
                   endif
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Variance written to file.'
                   else
                      write(*,*) '   Problem writing variance. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 0 ) then
                   write(*,*) '   No dimensions for this variable.  Skip.'
                else
                   write(*,*) ' Unknown number of dimensions! Crash.'
                   stop
                endif

             else
                write(*,*) ' Unprepared to deal with this var type. SKIP.'
                !stop
             endif
             
#ifdef bigfile
          enddo
          ! Information on this increment of the
          ! 4th dimension has now been written.
          ! Loop over the other index values of
          ! the 4th dimension before proceeding
          ! to the next variable.  All index
          ! values and allocations are already
          ! taken care of.
#endif

          !----------------------------------------
          ! Clear memory
          if ( l_valid ) then
             deallocate(readstart)
             deallocate(readcount)
             deallocate(writstart)
             deallocate(writcount)
             l_valid = .false.
          endif
          
          if( vartype(v) .eq. ncfloat ) then
             if( nvdims(v) .eq. 1 ) then
                deallocate(rinvar1d)
                deallocate(raovar1d)
                deallocate(rvovar1d)
                deallocate(covar1d)
             elseif( nvdims(v) .eq. 2 ) then
                deallocate(rinvar2d)
                deallocate(raovar2d)
                deallocate(rvovar2d)
                deallocate(covar2d)
             elseif( nvdims(v) .eq. 3 ) then
                deallocate(rinvar3d)
                deallocate(raovar3d)
                deallocate(rvovar3d)
                deallocate(covar3d)
             elseif( nvdims(v) .eq. 4 ) then
                deallocate(rinvar4d)
                deallocate(raovar4d)
                deallocate(rvovar4d)
                deallocate(covar4d)
             elseif( nvdims(v) .eq. 0 ) then
                write(*,*) '   No dimensions for this variable.  Skip deallocation.'
             else
                write(*,*) '   Unknown number of dimensions! Crash.'
                stop
             endif
          elseif( vartype(v) .eq. ncdouble ) then
             if( nvdims(v) .eq. 1 ) then
                deallocate(dinvar1d)
                deallocate(daovar1d)
                deallocate(dvovar1d)
                deallocate(covar1d)
             elseif( nvdims(v) .eq. 2 ) then
                deallocate(dinvar2d)
                deallocate(daovar2d)
                deallocate(dvovar2d)
                deallocate(covar2d)
             elseif( nvdims(v) .eq. 3 ) then
                deallocate(dinvar3d)
                deallocate(daovar3d)
                deallocate(dvovar3d)
                deallocate(covar3d)
             elseif( nvdims(v) .eq. 4 ) then
                deallocate(dinvar4d)
                deallocate(daovar4d)
                deallocate(dvovar4d)
                deallocate(covar4d)
             elseif( nvdims(v) .eq. 0 ) then
                write(*,*) '   No dimensions for this variable.  Skip deallocation.'
             else
                write(*,*) '   Unknown number of dimensions! Crash.'
                stop
             endif
             
          elseif( vartype(v) .eq. ncshort ) then
             if( nvdims(v) .eq. 1 ) then
                deallocate(sinvar1d)
                deallocate(saovar1d)
                deallocate(svovar1d)
                deallocate(covar1d)

                deallocate(rinvar1d)
                deallocate(raovar1d)
                deallocate(rvovar1d)

             elseif( nvdims(v) .eq. 2 ) then
                deallocate(sinvar2d)
                deallocate(saovar2d)
                deallocate(svovar2d)
                deallocate(covar2d)

                deallocate(rinvar2d)
                deallocate(raovar2d)
                deallocate(rvovar2d)

             elseif( nvdims(v) .eq. 3 ) then
                deallocate(sinvar3d)
                deallocate(saovar3d)
                deallocate(svovar3d)
                deallocate(covar3d)

                deallocate(rinvar3d)
                deallocate(raovar3d)
                deallocate(rvovar3d)

             elseif( nvdims(v) .eq. 4 ) then
                deallocate(sinvar4d)
                deallocate(saovar4d)
                deallocate(svovar4d)
                deallocate(covar4d)

                deallocate(rinvar4d)
                deallocate(raovar4d)
                deallocate(rvovar4d)

             elseif( nvdims(v) .eq. 0 ) then
                write(*,*) '   No dimensions for this variable.  Skip deallocation.'

             else
                write(*,*) '   Unknown number of dimensions! Crash.'
                stop
             endif
             
          elseif( vartype(v) .eq. ncbyte ) then
             if( nvdims(v) .eq. 1 ) then
                deallocate(binvar1d)
                deallocate(baovar1d)
                deallocate(bvovar1d)
                deallocate(covar1d)

                deallocate(rinvar1d)
                deallocate(raovar1d)
                deallocate(rvovar1d)

             elseif( nvdims(v) .eq. 2 ) then
                deallocate(binvar2d)
                deallocate(baovar2d)
                deallocate(bvovar2d)
                deallocate(covar2d)

                deallocate(rinvar2d)
                deallocate(raovar2d)
                deallocate(rvovar2d)

             elseif( nvdims(v) .eq. 3 ) then
                deallocate(binvar3d)
                deallocate(baovar3d)
                deallocate(bvovar3d)
                deallocate(covar3d)

                deallocate(rinvar3d)
                deallocate(raovar3d)
                deallocate(rvovar3d)

             elseif( nvdims(v) .eq. 4 ) then
                deallocate(binvar4d)
                deallocate(baovar4d)
                deallocate(bvovar4d)
                deallocate(covar4d)

                deallocate(rinvar4d)
                deallocate(raovar4d)
                deallocate(rvovar4d)

             elseif( nvdims(v) .eq. 0 ) then
                write(*,*) '   No dimensions for this variable.  Skip deallocation.'

             else
                write(*,*) '   Unknown number of dimensions! Crash.'
                stop
             endif
             
          elseif( vartype(v) .eq. ncint ) then
             if( nvdims(v) .eq. 1 ) then
                deallocate(iinvar1d)
                deallocate(iaovar1d)
                deallocate(ivovar1d)
                deallocate(covar1d)

                deallocate(rinvar1d)
                deallocate(raovar1d)
                deallocate(rvovar1d)

             elseif( nvdims(v) .eq. 2 ) then
                deallocate(iinvar2d)
                deallocate(iaovar2d)
                deallocate(ivovar2d)
                deallocate(covar2d)

                deallocate(rinvar2d)
                deallocate(raovar2d)
                deallocate(rvovar2d)

             elseif( nvdims(v) .eq. 3 ) then
                deallocate(iinvar3d)
                deallocate(iaovar3d)
                deallocate(ivovar3d)
                deallocate(covar3d)

                deallocate(rinvar3d)
                deallocate(raovar3d)
                deallocate(rvovar3d)

             elseif( nvdims(v) .eq. 4 ) then
                deallocate(iinvar4d)
                deallocate(iaovar4d)
                deallocate(ivovar4d)
                deallocate(covar4d)

                deallocate(rinvar4d)
                deallocate(raovar4d)
                deallocate(rvovar4d)

             elseif( nvdims(v) .eq. 0 ) then
                write(*,*) '   No dimensions for this variable.  Skip deallocation.'

             else
                write(*,*) '   Unknown number of dimensions! Crash.'
                stop
             endif

          else
             write(*,*) '   Unprepared to deal with this var type. SKIP.'
             !stop
          endif
#ifdef verbose
          write(*,*) ' '
          !write(*,*) ' '
#endif
!------------------------------------------------

       enddo ! End loop over variables.

!------------------------------------------------

        ! Close the output files
        call ncclos(avgofid, exitcode)
        if(exitcode .eq. 0) then
           !write(*,*)'   Average output file closed.'
        else
           write(*,*)'   Problem closing avg output file.'
           stop
        endif

        call ncclos(varofid, exitcode)
        if(exitcode .eq. 0) then
           !write(*,*)'   Variance output file closed.'
        else
           write(*,*)'   Problem closing var output file.'
           stop
        endif

!------------------------------------------------
#ifdef verbose
        write(6,*) ' End of avgcdf.'
#endif
      end program avgcdf

!------------------------------------------------

!---------------------------------------------------------

        subroutine olavgvar(new_val,counter,store_avg,store_var)

!---------------------------------------------------------
! This subroutine implements an online averaging algorithm
! by incorporating each new_val into the updated variable
! store_avg.  The variance is also computed in the variable
! store_var.  The variable counter is needed to keep track
! of the number of points that are incorporated into the
! average (i.e. not fill values).
!
!--------------------Technical Details--------------------
! Use an online algorithm based on Knuth, D. E. (1998).
! _The_Art_of_Computer_Programming_, vol. 2:
! Seminumerical Algorithms, 3rd Ed. p232.
! Boston: Addison-Wesley.  ...which I found out
! about on wikipedia, of course!
! http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
! See research notes for verification of algorithm.
! In the testing of this code, it is clear that the
! subtraction in the averaging does not cause the
! loss of digits for the numbers I'm using.
!
! After looping (and pushing the stack), the last step is
! to normalize the variance (store_var) by n (otherwise,
! our var values represent the sum of the squares of the
! differences between each value and its mean), but not
! the variance.  This must be done separately.
!
! In addition to checking for fill_values, we also check
! for NaN's.  This is a non-negligable increase in the
! computational cost.
!---------------------------------------------------------

          implicit none

          ! Declare local variables
          real, intent(in) :: new_val
          integer, intent(inout) :: counter
          real, intent(inout) :: store_avg
          real, intent(inout) :: store_var
          real(kind=8) :: delta
          
          integer :: new_count
          real(kind=8) :: dnew_val
          real(kind=8) :: dstore_avg
          real(kind=8) :: dstore_var
        
          new_count = counter + 1
             
          ! Convert to doubles
          dnew_val = dble(new_val)
          dstore_avg = dble(store_avg)
          dstore_var = dble(store_var)
          
          delta = dnew_val - dstore_avg
             
          store_avg = (real(counter)*store_avg + new_val)/real(new_count)
          counter = new_count
          
          dstore_avg = dble(store_avg)
          
          store_var = sngl(dstore_var + delta*(dnew_val - dstore_avg))
          
          return
        end subroutine olavgvar
!---------------------------------------------------------

!---------------------------------------------------------

        subroutine olavgvar_dble(new_val,counter,store_avg,store_var)

!---------------------------------------------------------
!
! Same as olavgvar except double IO.
!
!---------------------------------------------------------

          implicit none

          ! Declare local variables
          real(kind=8), intent(in) :: new_val
          integer, intent(inout) :: counter
          real(kind=8), intent(inout) :: store_avg
          real(kind=8), intent(inout) :: store_var

          real(kind=8) :: delta          
          integer :: new_count
        
          new_count = counter + 1
          
          delta = new_val - store_avg
          
          store_avg = (dble(counter)*store_avg + new_val)/dble(new_count)
          
          counter = new_count
          
          store_var = store_var + delta*(new_val - store_avg)
          
          return
        end subroutine olavgvar_dble
!---------------------------------------------------------
