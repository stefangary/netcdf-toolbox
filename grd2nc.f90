!------------------------------------------

       program grd2nc

       implicit none

!-----------------------------------------
! This program will input a .grd file
! created by GMT and output an .nc file
! that can be read by NOAA Ferret.       
! 
! While GMT .grd files are netcdf files,
! there is something about their format
! (perhaps variable name) that gives
! ferret trouble.
!
! No attempt is made to modify any of
! the parameters of the file or change
! any values/fills/etc.      
!
! - - - - - - - - - - - - - - - - - - - -

! This is an inital draft so no command line options:
! 1) link input file to in.grd
! 2) output file out.nc is created (will CLOBBER!)       
 
!----------------File Naming-------------
       character(len=6) :: infname='in.grd'
       character(len=6) :: outfname = 'out.nc'

!-------------------Domain-----------------
! Maximum width (i), length(j), depth (k), and time (i)
       integer :: im, jm, km, lm

!---------------Counter variables----------

! Dimension counters
       integer :: i, j, k, l, lc, l_max

! Variable counting
       integer :: v

!-------Declarations: netcdf file io--------
! Out-file-id, In-file-id, error return, io
       integer :: ofid
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
       
!------------------------------------------

! Vectors that note where to read
! and write in the netcdf file,
! READ STart, READ COunt, and WRITe
! STart.
       integer, allocatable :: readstart(:)
       integer, allocatable :: readcount(:)
       integer, allocatable :: writstart(:)
       integer, allocatable :: writcount(:)

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
       ofid = 42
       dummy = nf_create(outfname,nf_64bit_offset,ofid)
#else
       ofid = nccre(outfname,ncclobber,exitcode)
#endif

!----------------------------------------
       
!----------------------------------------
#ifdef verbose
       write(*,*) 'Opening input file...'
#endif
       !filenum = filenumbase + 1

       ! Determine the infile name
       !write(infname,'(a,i5,a)')infnamebase,filenum,infnameextn
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
          aodid(v) = ncddef(ofid,trim(dimname(v)),dimsize(v),exitcode)
#ifdef verbose
          if(exitcode .eq. 0) then
             write(*,*) '   Dimension copy to avg file OK, ID: ',aodid(v)
          else
             write(*,*) '   Dimension creation error.'
             stop
          endif
#endif
       enddo

!----------------------------------------
#ifdef verbose
       write(*,*) 'Copying variable info...'
       write(*,*) ' '
#endif
       allocate(ivid(nvars))
       allocate(aovid(nvars))
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
          aovdims = -1

          ! For each element in ivdims
          do k = 1,nvdims(v)

             ! Cycle through all possible dimensions
             ! to find the dimension we seek.
             do j = 1,ndims

                if( ivdims(k) .eq. idid(j) ) then

                   ! This dimension is in this variable.
                   aovdims(k) = aodid(j)

                endif
             enddo
          enddo

          write(*,*) ' ivdims: ',ivdims
          write(*,*) ' aovdims: ',aovdims

!----------------------------------------

          aovid(v) = ncvdef(ofid,trim(varname(v)),&
               vartype(v),nvdims(v),aovdims,exitcode)
          if(exitcode .eq. 0) then
             write(*,*) '   Variable shape copy to avg file OK.'
          else
             write(*,*) 'Error creating new variable in out file.'
             stop
          endif

          deallocate(aovdims)

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
             call ncacpy(ifid,ivid(v),trim(attname),ofid,aovid(v),exitcode)
             if(exitcode .eq. 0) then
                write(*,*)'      Attribute created in avg file OK.'
             else
                write(*,*)'      Error creating attribute in output file.'
                stop
             endif
#ifdef verbose
             write(*,*) ' '
#endif
          enddo
       enddo

!----------------------------------------
#ifdef verbose
       write(*,*) 'Done defining output file.'
#endif
       call ncclos(ifid,exitcode)
       if(exitcode .eq. 0) then
          write(*,*) 'Input file closed.'
       else
          write(*,*) 'Cannot close input file!'
          stop
       endif

       call ncendf(ofid, exitcode)
       if(exitcode .eq. 0) then
          !write(*,*) 'Average outfile ready to write.'
       else
          write(*,*) 'Error exiting define mode in avg file.'
          stop
       endif

!----------------------------------------
#ifdef verbose
       write(*,*) '-------------Starting to work...---------------'
       write(*,*) ' '
#endif
       ! For each variable,
       do v = 1,nvars
#ifdef verbose
          write(*,*) '=========== Working on ',trim(varname(v)),' ==========='
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
             if ( nvdims(v) .eq. 4 ) write(*,*) '  Working on big file 4th dimension ',lc,' of ',l_max,'...'
#endif             
#endif

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
                
                ! Set index bounds
                ! and allocate space.  Also, do this only for
                ! the first 4th dimension time step of the
                ! first big file.
#ifdef bigfile
                if(lc .eq. 1) then
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
                         rinvar1d = 0.0
                         
                      elseif( nvdim .eq. 2 ) then
                         allocate(rinvar2d(1:im,1:jm))
                         rinvar2d = 0.0

                      elseif( nvdim .eq. 3 ) then
                         allocate(rinvar3d(1:im,1:jm,1:km))
                         rinvar3d = 0.0
                         
                      elseif( nvdim .eq. 4 ) then
#ifdef bigfile
                         ! For large files, we will proceed
                         ! time step by time step, so keep
                         ! size of the 4th dimension to 1.
                         allocate(rinvar4d(1:im,1:jm,1:km,1))
#else
                         ! For smaller files, we can allocate
                         ! as much space as is needed in all
                         ! dimensions.
                         allocate(rinvar4d(1:im,1:jm,1:km,1:lm))
#endif
                         rinvar4d = 0.0
                      else
                         write(*,*) '   Unknown number of dimensions! Crash.'
                         stop
                      endif
                      
                   elseif( (vart .eq. ncdouble) .and. (nvdim .gt. 0) ) then
                      
                      if( nvdim .eq. 1 ) then
                         allocate(dinvar1d(1:im))
                         dinvar1d = 0.0
                         
                      elseif( nvdim .eq. 2 ) then
                         allocate(dinvar2d(1:im,1:jm))
                         dinvar2d = 0.0
                         
                      elseif( nvdim .eq. 3 ) then
                         allocate(dinvar3d(1:im,1:jm,1:km))
                         dinvar3d = 0.0
                         
                      elseif( nvdim .eq. 4 ) then
#ifdef bigfile
                         ! See notes above for nvdim .eq. 4
                         ! for the single precision variable
                         ! size allocation.
                         allocate(dinvar4d(1:im,1:jm,1:km,1))
#else
                         allocate(dinvar4d(1:im,1:jm,1:km,1:lm))
#endif
                         dinvar4d = 0.0
                      else
                         write(*,*) '   Unknown number of dimensions! Crash.'
                         stop
                      endif
                      
                   elseif ( (vart .eq. ncshort) .and. (nvdim .gt. 0) ) then
                      
                      if( nvdim .eq. 1 ) then
                         allocate(sinvar1d(1:im))
                         sinvar1d = 0.0
                         
                      elseif( nvdim .eq. 2 ) then
                         allocate(sinvar2d(1:im,1:jm))
                         sinvar2d = 0.0
                         
                      elseif( nvdim .eq. 3 ) then
                         allocate(sinvar3d(1:im,1:jm,1:km))
                         sinvar3d = 0.0
                         
                      elseif( nvdim .eq. 4 ) then
#ifdef bigfile
                         ! See notes above for nvdim .eq. 4
                         ! for the single precision variable
                         ! size allocation.
                         allocate(sinvar4d(1:im,1:jm,1:km,1))
#else
                         allocate(sinvar4d(1:im,1:jm,1:km,1:lm))
#endif
                         sinvar4d = 0.0
                      else
                         write(*,*) '   Unknown number of dimensions! Crash.'
                         stop
                      endif
                      
                   elseif ( (vart .eq. ncbyte) .and. (nvdim .gt. 0) ) then
                      if( nvdim .eq. 1 ) then
                         allocate(binvar1d(1:im))
                         binvar1d = 0.0
                         
                      elseif( nvdim .eq. 2 ) then
                         allocate(binvar2d(1:im,1:jm))
                         binvar2d = 0.0
                         
                      elseif( nvdim .eq. 3 ) then
                         allocate(binvar3d(1:im,1:jm,1:km))
                         binvar3d = 0.0
                         
                      elseif( nvdim .eq. 4 ) then
#ifdef bigfile
                         ! See notes above for nvdim .eq. 4
                         ! for the single precision variable
                         ! size allocation.
                         allocate(binvar4d(1:im,1:jm,1:km,1))
#else
                         allocate(binvar4d(1:im,1:jm,1:km,1:lm))
#endif
                         binvar4d = 0.0
                      else
                         write(*,*) '   Unknown number of dimensions! Crash.'
                         stop
                      endif
                      
                   elseif ( (vart .eq. ncint) .and. (nvdim .gt. 0) ) then
                      if( nvdim .eq. 1 ) then
                         allocate(iinvar1d(1:im))
                         iinvar1d = 0.0
                         
                      elseif( nvdim .eq. 2 ) then
                         allocate(iinvar2d(1:im,1:jm))
                         iinvar2d = 0.0
                         
                      elseif( nvdim .eq. 3 ) then
                         allocate(iinvar3d(1:im,1:jm,1:km))
                         iinvar3d = 0.0
                         
                      elseif( nvdim .eq. 4 ) then
#ifdef bigfile
                         ! See notes above for nvdim .eq. 4
                         ! for the single precision variable
                         ! size allocation.
                         allocate(iinvar4d(1:im,1:jm,1:km,1))
#else
                         allocate(iinvar4d(1:im,1:jm,1:km,1:lm))
#endif
                         iinvar4d = 0.0
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
#ifdef bigfile                   
                endif
#endif
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
                   call ncvpt(ofid,aovid(v),writstart,writcount,rinvar1d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 2 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,rinvar2d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 3 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,rinvar3d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 4 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,rinvar4d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
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
                   call ncvpt(ofid,aovid(v),writstart,writcount,dinvar1d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 2 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,dinvar2d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 3 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,dinvar3d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 4 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,dinvar4d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
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
                   call ncvpt(ofid,aovid(v),writstart,writcount,sinvar1d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 2 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,sinvar2d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 3 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,sinvar3d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 4 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,sinvar4d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
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
                   call ncvpt(ofid,aovid(v),writstart,writcount,binvar1d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 2 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,binvar2d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 3 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,binvar3d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 4 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,binvar4d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
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
                   call ncvpt(ofid,aovid(v),writstart,writcount,iinvar1d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 2 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,iinvar2d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 3 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,iinvar3d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
                      stop
                   endif
                elseif( nvdims(v) .eq. 4 ) then
                   call ncvpt(ofid,aovid(v),writstart,writcount,iinvar4d,exitcode)
                   if( exitcode .eq. 0 ) then
                      !write(*,*) '   Average written to file.'
                   else
                      write(*,*) '   Problem writing average. Crash.'
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

          endif ! lnovar - WORKING HERE
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
             elseif( nvdims(v) .eq. 2 ) then
                deallocate(rinvar2d)
             elseif( nvdims(v) .eq. 3 ) then
                deallocate(rinvar3d)
             elseif( nvdims(v) .eq. 4 ) then
                deallocate(rinvar4d)
             elseif( nvdims(v) .eq. 0 ) then
                write(*,*) '   No dimensions for this variable.  Skip deallocation.'
             else
                write(*,*) '   Unknown number of dimensions! Crash.'
                stop
             endif
          elseif( vartype(v) .eq. ncdouble ) then
             if( nvdims(v) .eq. 1 ) then
                deallocate(dinvar1d)
             elseif( nvdims(v) .eq. 2 ) then
                deallocate(dinvar2d)
             elseif( nvdims(v) .eq. 3 ) then
                deallocate(dinvar3d)
             elseif( nvdims(v) .eq. 4 ) then
                deallocate(dinvar4d)
             elseif( nvdims(v) .eq. 0 ) then
                write(*,*) '   No dimensions for this variable.  Skip deallocation.'
             else
                write(*,*) '   Unknown number of dimensions! Crash.'
                stop
             endif
             
          elseif( vartype(v) .eq. ncshort ) then
             if( nvdims(v) .eq. 1 ) then
                deallocate(sinvar1d)

             elseif( nvdims(v) .eq. 2 ) then
                deallocate(sinvar2d)

             elseif( nvdims(v) .eq. 3 ) then
                deallocate(sinvar3d)

             elseif( nvdims(v) .eq. 4 ) then
                deallocate(sinvar4d)

             elseif( nvdims(v) .eq. 0 ) then
                write(*,*) '   No dimensions for this variable.  Skip deallocation.'

             else
                write(*,*) '   Unknown number of dimensions! Crash.'
                stop
             endif
             
          elseif( vartype(v) .eq. ncbyte ) then
             if( nvdims(v) .eq. 1 ) then
                deallocate(binvar1d)

             elseif( nvdims(v) .eq. 2 ) then
                deallocate(binvar2d)

             elseif( nvdims(v) .eq. 3 ) then
                deallocate(binvar3d)

             elseif( nvdims(v) .eq. 4 ) then
                deallocate(binvar4d)

             elseif( nvdims(v) .eq. 0 ) then
                write(*,*) '   No dimensions for this variable.  Skip deallocation.'

             else
                write(*,*) '   Unknown number of dimensions! Crash.'
                stop
             endif
             
          elseif( vartype(v) .eq. ncint ) then
             if( nvdims(v) .eq. 1 ) then
                deallocate(iinvar1d)

             elseif( nvdims(v) .eq. 2 ) then
                deallocate(iinvar2d)

             elseif( nvdims(v) .eq. 3 ) then
                deallocate(iinvar3d)

             elseif( nvdims(v) .eq. 4 ) then
                deallocate(iinvar4d)

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
        call ncclos(ofid, exitcode)
        if(exitcode .eq. 0) then
           !write(*,*)'   Average output file closed.'
        else
           write(*,*)'   Problem closing output file.'
           stop
        endif

!------------------------------------------------
#ifdef verbose
        write(6,*) ' End of grd2nc.'
#endif
      end program grd2nc

!------------------------------------------------
