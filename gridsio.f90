! This software is distributed under the
! terms of the GNU General Public License
! v3 or any later version.
! Copyright Stefan Gary, 2020.
!------------------------------------------

!------------------------------------------
! All parameters, variables, and external
! function declarations specific to I/O
! of netCDF files are grouped here.

      module netcdfio

        implicit none

        ! External functions
        integer, external :: nccre
        integer, external :: ncopn
        integer, external :: ncdid
        integer, external :: ncvid
        integer, external :: ncddef
        integer, external :: ncvdef
        integer, external :: nf_create

        ! Constant values as defined in netcdf.inc
        integer, parameter :: ncbyte = 1
        integer, parameter :: ncchar = 2
        integer, parameter :: ncshort = 3
        integer, parameter :: ncint = 4
        integer, parameter :: ncfloat = 5
        integer, parameter :: ncdouble = 6 
        integer, parameter :: ncnowrit = 0
        integer, parameter :: ncwrite = 1
        integer, parameter :: ncclobber = 0
        integer, parameter :: nf_64bit_offset = 512

        ! File IDs
        integer :: infid, exitcode, io
        integer :: outfid1,outfid2

        ! Name of the vector that notes where to
        ! write in the netcdf file.
        integer :: readstart(4)
        integer :: readcount(4)
        integer :: writestart(3)
        integer :: writecount(3)

        integer :: readst1d(1)
        integer :: readct1d(1)
        integer :: readst2d(2)
        integer :: readct2d(2)
        integer :: readst3d(3)
        integer :: readct3d(3)
        integer :: readst4d(4)
        integer :: readct4d(4)

        integer :: writest1d(1)
        integer :: writect1d(1)
        integer :: writest2d(2)
        integer :: writect2d(2)
        integer :: writest3d(3)
        integer :: writect3d(3)
        integer :: writest4d(4)
        integer :: writect4d(4)

        ! Variable ids for the input file
        integer :: uvid, vvid, wvid, tvid
        integer :: svid, hvid, cvid, kvid
        integer :: xvid, yvid, zvid

        !-------File Naming Conventions------
        ! Base names of the links made to the
        ! files for input:
        character(len=6) :: infroot = 'infile'
        character(len=4) :: infext = '.cdf'
        character(len=16) :: infname
        
        ! Name of the output file
        character(len=7) :: outfname = 'out.cdf'
        character(len=8)   :: ofnaddt = 'addt.cdf'
#ifdef uavgvar
        character(len=8) :: avgoutfname = 'uavg.cdf'
        character(len=8) :: varoutfname = 'uvar.cdf'
#endif
#ifdef tavgvar
        character(len=8) :: avgoutfname = 'tavg.cdf'
        character(len=8) :: varoutfname = 'tvar.cdf'
#endif
#ifdef qeddy
        character(len=9) :: eddoutfname = 'qeddy.cdf'
        character(len=9) :: divoutfname = 'div_q.cdf'
#endif
#ifdef zeddy
        character(len=9) :: eddoutfname = 'zeddy.cdf'
        character(len=9) :: divoutfname = 'div_z.cdf'
#endif
        !------OUTPUT FILE STRUCTURE------
        ! Two or three or four dimensions
        ! in each output variable
        integer :: vdims1d(1)
        integer :: vdims2d(2)
        integer :: vdims3d(3)
        integer :: vdims4d(4)

        ! _2 for second file output
        integer :: vdims1d2(1)
        integer :: vdims2d2(2)
        integer :: vdims3d2(3)
        integer :: vdims4d2(4)

        ! NetCDF output dimension IDs
        integer :: londid, latdid
        integer :: depdid, timdid
        integer :: deptdid, depwdid

        ! NetCDF output dim/variable ID
        integer :: lontvid, lattvid
        integer :: lonuvid, latuvid
        integer :: lonvvid, latvvid
        integer :: lonfvid, latfvid
        integer :: deptvid, depwvid
        integer :: ddptvid, ddpwvid
        integer :: timvid

        integer :: dxtvid, dytvid
        integer :: dxuvid, dyuvid
        integer :: dxvvid, dyvvid
        integer :: dxfvid, dyfvid

           ! _2 for second file output.
        integer :: lontvid2, lattvid2
        integer :: lonuvid2, latuvid2
        integer :: lonvvid2, latvvid2
        integer :: lonfvid2, latfvid2
        integer :: deptvid2, depwvid2

        ! NetCDF output dimension names
        character(len=1) :: londnam = 'x'
        character(len=1) :: latdnam = 'y'
        character(len=1) :: depdnam = 'z'
        character(len=2) :: deptdnam = 'zt'
        character(len=2) :: depwdnam = 'zw'
        character(len=4) :: timdnam = 'time'
        
        ! NetCDF output (plotting) dim/variable names
        character(len=4) :: lontvnam = 'lont'
        character(len=4) :: lattvnam = 'latt'
        character(len=4) :: lonuvnam = 'lonu'
        character(len=4) :: latuvnam = 'latu'
        character(len=4) :: lonvvnam = 'lonv'
        character(len=4) :: latvvnam = 'latv'
        character(len=4) :: lonfvnam = 'lonf'
        character(len=4) :: latfvnam = 'latf'
        character(len=4) :: deptvnam = 'dept'
        character(len=4) :: depwvnam = 'depw'
        character(len=3) :: ddptvnam = 'dzt'
        character(len=3) :: ddpwvnam = 'dzw'
        character(len=4) :: timevnam = 'time'
        character(len=3) :: dxtvnam = 'dxt'
        character(len=3) :: dytvnam = 'dyt'
        character(len=3) :: dxuvnam = 'dxu'
        character(len=3) :: dyuvnam = 'dyu'
        character(len=3) :: dxvvnam = 'dxv'
        character(len=3) :: dyvvnam = 'dyv'
        character(len=3) :: dxfvnam = 'dxf'
        character(len=3) :: dyfvnam = 'dyf'

      end module netcdfio
!------------------------------------------

!------------------------------------------

      module load_data
      
        implicit none

      contains

        ! All functions for loading grids:
        ! load_flame_grid
        ! load_orca05_grid
        ! load_orca025_grid
        ! load_storm_grid
        ! load_storm_cut_grid
        ! wipe_grid

        ! Functions for loading data
        ! Allow for loading subregions
        ! of the full model fields.
        ! Infield common variables must
        ! be allocated first and extents
        ! are set by readcount and
        ! readstart.

        ! More generalized code: hide model differences
        ! within subroutines instead of having separate
        ! subroutines to do each operation for each
        ! model.  Below is the list of subroutines
        ! necessary for generality.
        ! load_uv
        ! load_uvw
        ! compute_w
        ! load_t
        ! load_s
        ! load_tracer
        ! load_h

        ! Other functions to write for generality:
        ! load_cut_grid (or perhaps cut_grid)
        ! load_grid
        ! wipe_grid
        ! flip_j()  (perhaps best in ops2d)
        ! flip_i()  (perhaps best in ops2d)

        ! ORIGINAL FUNCTIONS FOR LOADING DATA
        ! Still included for backward compatibility
        ! load_flame_data
        ! load_flame_cut_data
        ! load_orca_data
        ! load_orca_tracer
        ! load_storm_cut_data

!----------------------------------------------------------
        subroutine wipe_grid
!----------------------------------------------------------
! This  subroutine will wipe out the grid and topography
! variables defining the model grid so that a cut
! grid can be loaded later.
!----------------------------------------------------------

          use grids
          implicit none

#ifdef verbose
          write(*,*) ' Wiping model grid...'
#endif
#if defined (orca025) || defined (orca050) || defined (mmflame) || defined (d3flame) || defined (orca025_global) || defined (viking20_cut) || defined (viking20_full)
          deallocate(kmt,kmf)
          deallocate(xu,xv,xf)
          deallocate(yu,yv,yf)
          deallocate(dxt,dxu,dxv,dxf)
          deallocate(dyt,dyu,dyv,dyf)
#endif
#if defined (orca025) || defined (orca050) || defined (orca025_global) || defined (viking20_cut) || defined (viking20_full)
          deallocate(umask,vmask,tmask,fmask)
          deallocate(dzt,dzw)
#endif
#ifdef storm
          deallocate(depto)
          deallocate(dzt)
#endif
#ifdef storm_cdo
          deallocate(depto)
#endif
          ! The grid vars below are always loaded
          ! for all three model frameworks.  The
          ! grid vars above are loaded only for
          ! ORCA and FLAME.
          deallocate(xt,yt)
          !deallocate(zt,zw)
          return
        end subroutine wipe_grid
!----------------------------------------------------------

!---------------------------------------------------------
        subroutine flip_j(var,dg)
!---------------------------------------------------------
! This subroutine will flip the grid in the j direction.
! This is used for STORM data which is stored upside
! down (increasing j is decreasing latitude).
!
! The direction of the velocity field does not need to
! change - it is just how the lines are stored.
!
!---------------------------------------------------------

          implicit none

          ! Declare inputs and output
          integer, dimension(2), intent(in) :: dg
          real, dimension(dg(1),dg(2)), intent(inout) :: var
          real, allocatable :: tmp(:,:)

          ! Local array bounds
          integer :: im,jm

          ! Local counters
          integer :: i,j,o

          im = dg(1)
          jm = dg(2)
          allocate(tmp(im,jm))
          ! Copy original variable
          do j = 1,jm
             do i = 1,im
                tmp(i,j) = var(i,j)
             enddo
          enddo
          ! Wipe out original variable
          var = 0.0
          do j = 1,jm
             ! Compute the row transform
             ! For j = 1, o = jm
             ! For j = 2, o = jm-1
             ! For j = jm, o = 1
             ! For j = jm-1, o = 2
             ! For j = jm/2 - 1, o = jm/2 + 2
             ! For j = jm/2, o = jm/2 + 1
             ! For j = jm/2 + 1, o = jm/2
             o = jm-j+1
             do i = 1,im
                var(i,j) = tmp(i,o)
             enddo
          enddo
          deallocate(tmp)
          return
        end subroutine flip_j
!---------------------------------------------------------

#if defined (mmflame) || defined (d3flame)
!----------------------------------------------------------
        subroutine load_flame_grid
!----------------------------------------------------------
          use params
          use grids
         
          implicit none

! This subroutine reads information about the
! FLAME model grid from the binary files kmt.dta
! and grid.dta.  The grid variables were declared
! in the grids module and allocated here.
!
! FLAME u,v,t,s variables are all on the same
! levels (i.e. located by zt and dzt) but they
! are on staggered grids in the horizontal with
! u,v on one grid and t,s on the other grid
! (B-grid convention - from the perspective of
! a C-grid, FLAME t,s are on the C-t-grid and 
! FLAME u,v are on the C-f-grid).  In order to
! convert from one grid system to the other,
! we declare and allocate *local* B-grid
! variables for reading the input files.  Then,
! we copy those variables to the globally declared
! (but allocated here) C-grid variables.

          !------Local variables------
          integer :: idimuf, jdim, kdim, io
          integer :: i, j, k

          !---B-grid Variables (_b)---
          ! Horizontal locations of tracer and
          ! velocity grid nodes (units degrees)
          real,allocatable :: xtb(:),xub(:)
          real,allocatable :: ytb(:),yub(:)

          ! Horizontal spacing between grid nodes
          real,allocatable :: dxtb(:),dxub(:)
          real,allocatable :: dytb(:),dyub(:)

          ! Conversion/Scaling/Mapping factors
          real :: pi, deg2rad, deg2met, eradius
          real,allocatable :: phiub(:),phitb(:)
          real,allocatable :: cosub(:),costb(:)

          !------Allocate memory for the C-grid------
          ! Variables declared in grids module.
          allocate(kmt(imt,jmt))
          allocate(kmf(imt,jmt))
          allocate(xt(imt,jmt),yt(imt,jmt)) 
          allocate(xu(imt,jmt),yu(imt,jmt)) 
          allocate(xv(imt,jmt),yv(imt,jmt)) 
          allocate(xf(imt,jmt),yf(imt,jmt)) 
          allocate(dxt(imt,jmt),dyt(imt,jmt))
          allocate(dxu(imt,jmt),dyu(imt,jmt))
          allocate(dxv(imt,jmt),dyv(imt,jmt))
          allocate(dxf(imt,jmt),dyf(imt,jmt))
          allocate(zt(0:km+1),zw(0:km))
          allocate(dzt(0:km+1),dzw(0:km))

          !------Allocate memory for the B-grid-----
          ! Variables declared here, local to this subroutine.
          allocate(xtb(imt),ytb(jmt)) 
          allocate(xub(imt),yub(jmt)) 
          allocate(dxtb(imt),dytb(jmt))
          allocate(dxub(imt),dyub(jmt))
          allocate(phiub(jmt),phitb(jmt))
          allocate(cosub(jmt),costb(jmt))

#ifdef verbose
          write(*,*) ' Loading FLAME grid information...'
#endif

!-----Read in topography information-----
! The orginal kmt.dta file is big-endian.
! Also, specify that the file is a binary
! file (unformatted), all we want to do is
! read, it already exists, and that we
! will have sequential (rather than direct)
! access.
          io = 44
          open(unit=io, &
               file='kmt.dta', &
               form='unformatted', &
               action='read', &
               convert='big_endian', &
               status='old', &
               access='sequential')

! Skip over the header of the file, read
! number of grid nodes in each water column,
! and close the file. One might be able to
! force this with -frecord-marker=4 flag?
          read(io)
          read(io)
          read(io)
          read(io)
          read(io) kmt
          close(io)

! Force the rightmost column and top row to
! be all zeros.  This is done so that all
! f-grid points that are not completely
! surrounded by t-grid points are set null.
          do j=1,jmt
             kmf(imt,j) = 0
          enddo

          do i=1,imt
             kmf(i,jmt) = 0
          enddo

! Scan through all topography points and
! assign the minimum of the surrounding
! values to kmf. Minimum value means less
! depth, so only the f-grid nodes that are
! completely surrounded (in 3D) are non-zero.
          do j=1,jmt-1
             do i=1,imt-1
                kmf(i,j) = min(kmt(i,j), &
                               kmt(i+1,j), &
                               kmt(i,j+1), &
                               kmt(i+1,j+1))
             enddo
          enddo

!-----End of topography information-----

!-----Load grid spacing and node locations-----
! Open the binary FLAME grid information file.
          io=45
          open(unit=io, &
               file='grid.dta', &
               form='unformatted', &
               action='read', &
               convert='big_endian', &
               status='old', &
               access='sequential')

! Skip over the first two lines in the header. 
          read(io)
          read (io) idimuf, jdim, kdim

! Read grid spacing and node positions and close the file.
          read (io) (dxtb(i),i=1,imt), &
                    (dytb(j),j=1,jmt), &
                    (dxub(i),i=1,imt), &
                    (dyub(j),j=1,jmt), &
                    (dzt(k),k=1,km), &
                    (dzw(k),k=0,km), &
                    (xtb(i),i=1,imt), &
                    (xub(i),i=1,imt), &
                    (ytb(j),j=1,jmt), &
                    (yub(j),j=1,jmt), &
                    (zt(k),k=1,km), &
                    (zw(k),k=1,km)
          close(unit=io)

! Convert absolute degreees of longitude to
! degrees longitude E and W.  Negative values
! are degrees west of the prime meridian.
          xub = xub - 360.0
          xtb = xtb - 360.0

!-----Fill up z_ and dz_-----
! zt(0) is 5m above the surface
! zw(0) is the surface
! zt(1) is in the middle of the surf grid box
! zw(km) is the bottom of the deepest possible t-
! zt(km+1) is a point always below ocean floor
! Picture:
!-------------------------
!
!  zt(0)=-5m,  dzt(0)=10m    -------------------
!
!~~~~~surface~rigid~lid~~~~~~zw(0)=0, dzw(0)=10m
!
!  zt(1)=5m,  dzt(1)=10m     -------------------
!
!------------------------    zw(1)=10m, dzw(1)=10m
!
!  zt(2)=15m,  dzt(2)=10.44  -------------------
!
!------------------------    zw(2)=20.44, dzw(2)=10.88
!
!  zt(3)=25.88m, ...         -------------------
!
!
!                            -------------------
!
!  ----------------------    zw(44)=5250, dzw(44)=250
!
!  zt(45)=5375, dzt(45)=250  -------------------
!
!~~~bottom last t-grid cell = last w-level node~~~zw(45)=5500, dzw(45)=125~~~
!
!  zt(46)=5625, dzt(46)=250  -------------------
!========================================================
          zt(0) = (-1) * zt(1)
          dzt(0) = dzt(1)
          zw(0) = 0.0
          dzw(0) = dzw(1)
          dzt(km+1) = dzt(km)
          zt(km+1) = zt(km) + dzt(km)

! Convert zt, zw, dzw, dzt from units of cm to m:
          zt = zt/100
          zw = zw/100
          dzw = dzw/100
          dzt = dzt/100

!-----Conversion factors-----
! Compute values of latitude in units of radians
! and compute the Mercator map scaling factor
! (cosine of latitude).
          pi = 4.0*atan(1.0)          ! atan(1.0) = pi/4
          deg2rad = pi/180.0          ! degrees to radians
          eradius = 6370.e03          ! Earth radius [m]
          deg2met = eradius*deg2rad   ! half circumference/180 degrees
                                      ! => meters per degree at equator

! For each row of latitude in the model domain,
! convert degrees latitude to radians latitude
! for both the u and t grids.  Also, compute
! the cosine of each latitudinal location.
          do j=1,jmt
             phitb(j) = ytb(j)*deg2rad
             phiub(j) = yub(j)*deg2rad
             cosub(j) = cos(phiub(j))
             costb(j) = cos(phitb(j))
          enddo

!------B-grid -> C-grid node position------
! Locations retain angular units (lon,lat).
! Horizontal values only - z-levels are
! identical in B or C grids.
!
!    V    F--yub
!
!    T    U--ytb
!    |    |
!   xtb  xub

          do j = 1,jmt
             do i = 1,imt
                xt(i,j) = xtb(i)
                xu(i,j) = xub(i)
                xv(i,j) = xtb(i)
                xf(i,j) = xub(i)
                
                yt(i,j) = ytb(j)
                yu(i,j) = ytb(j)
                yv(i,j) = yub(j)
                yf(i,j) = yub(j)
             enddo
          enddo

!-----B-grid -> C-grid node spacing-----
! Spacing in [m], not angluar units.
          do j = 1,jmt
             do i = 1,imt
                ! Mercator spacing - boxes to
                ! the north are more closely
                ! packed zonally. Meridional
                ! spacings do not change zonally.

                ! <B|C>-t-grid
                dxt(i,j) = dxtb(i)*costb(j)*deg2met
                dyt(i,j) = dytb(j)*deg2met
                
                ! B-u-grid = C-f-grid
                dxf(i,j) = dxub(i)*cosub(j)*deg2met
                dyf(i,j) = dyub(j)*deg2met
                
                ! Since the grid spacing along lines of
                ! longitude is uniform, we can simply
                ! copy over the zonal grid spacing from
                ! the B-t-grid to the C-u-grid and from
                ! the B-f-grid to the C-v-grid.
                dxu(i,j) = dxt(i,j)
                dyu(i,j) = dyt(i,j)
                dxv(i,j) = dxf(i,j)
                dyv(i,j) = dyf(i,j)
             enddo
          enddo

          return
        end subroutine load_flame_grid
!----------------------------------------------------------

!----------------------------------------------------------
        subroutine cut_flame_grid
!----------------------------------------------------------
! This subroutine will cut the existing FLAME grid to a
! smaller size stored in temporary variables, deallocate
! the full grid variables, reallocate the variables to
! the cut grid dimensions, and the fill in the values.
! The vertical spacing is not changed at all since FLAME
! does not have partial depths.
!----------------------------------------------------------
          use params
          use grids
          use netcdfio

          implicit none

          ! Start locations
          integer :: ii,jj

          ! Stide variables
          integer :: im,jm

          ! Current variables
          integer :: ic,jc

          ! Counter variables
          integer :: i,j

          ! Local temporary variables
          real, allocatable ::  kmt_l(:,:)
          real, allocatable ::  kmf_l(:,:)
          real, allocatable ::  xt_l(:,:)
          real, allocatable ::  yt_l(:,:)
          real, allocatable ::  dxt_l(:,:)
          real, allocatable ::  dyt_l(:,:)
          real, allocatable ::  xu_l(:,:)
          real, allocatable ::  yu_l(:,:)
          real, allocatable ::  dxu_l(:,:)
          real, allocatable ::  dyu_l(:,:)
          real, allocatable ::  xv_l(:,:)
          real, allocatable ::  yv_l(:,:)
          real, allocatable ::  dxv_l(:,:)
          real, allocatable ::  dyv_l(:,:)
          real, allocatable ::  xf_l(:,:)
          real, allocatable ::  yf_l(:,:)
          real, allocatable ::  dxf_l(:,:)
          real, allocatable ::  dyf_l(:,:)

          ! Set the start and stride based on readstart
          ! and readcount
          ii = readstart(1)
          jj = readstart(2)
          im = readcount(1)
          jm = readcount(2)

          ! Temporary variables
          allocate(kmt_l(im,jm))
          allocate(kmf_l(im,jm))
          allocate(xt_l(im,jm),yt_l(im,jm)) 
          allocate(xu_l(im,jm),yu_l(im,jm)) 
          allocate(xv_l(im,jm),yv_l(im,jm)) 
          allocate(xf_l(im,jm),yf_l(im,jm)) 
          allocate(dxt_l(im,jm),dyt_l(im,jm))
          allocate(dxu_l(im,jm),dyu_l(im,jm))
          allocate(dxv_l(im,jm),dyv_l(im,jm))
          allocate(dxf_l(im,jm),dyf_l(im,jm))

          ! Copy from full grid to temporary variables
          jc = 1
          do j = jj,jj+jm-1
             ic = 1
             do i = ii,ii+im-1
                !write(*,*) '(ic,jc) = ',ic,jc,' (i,j) = ',i,j
                kmt_l(ic,jc) = kmt(i,j)
                kmf_l(ic,jc) = kmf(i,j)
                xt_l(ic,jc) = xt(i,j)
                yt_l(ic,jc) = yt(i,j)
                dxt_l(ic,jc) = dxt(i,j)
                dyt_l(ic,jc) = dyt(i,j)
                xu_l(ic,jc) = xu(i,j)
                yu_l(ic,jc) = yu(i,j)
                dxu_l(ic,jc) = dxu(i,j)
                dyu_l(ic,jc) = dyu(i,j)
                xv_l(ic,jc) = xv(i,j)
                yv_l(ic,jc) = yv(i,j)
                dxv_l(ic,jc) = dxv(i,j)
                dyv_l(ic,jc) = dyv(i,j)
                xf_l(ic,jc) = xf(i,j)
                yf_l(ic,jc) = yf(i,j)
                dxf_l(ic,jc) = dxf(i,j)
                dyf_l(ic,jc) = dyf(i,j)
                ic = ic + 1
             enddo
             jc = jc + 1
          enddo

          ! Deallocate the full grid variables
          deallocate(kmt,kmf)
          deallocate(xt,yt,dxt,dyt)
          deallocate(xu,yu,dxu,dyu)
          deallocate(xv,yv,dxv,dyv)
          deallocate(xf,yf,dxf,dyf)

          !------Allocate memory for the cut C-grid------
          ! Variables declared in grids module.
          allocate(kmt(im,jm))
          allocate(kmf(im,jm))
          allocate(xt(im,jm),yt(im,jm)) 
          allocate(xu(im,jm),yu(im,jm)) 
          allocate(xv(im,jm),yv(im,jm)) 
          allocate(xf(im,jm),yf(im,jm)) 
          allocate(dxt(im,jm),dyt(im,jm))
          allocate(dxu(im,jm),dyu(im,jm))
          allocate(dxv(im,jm),dyv(im,jm))
          allocate(dxf(im,jm),dyf(im,jm))

          !--------------Copy temp variables to perm--------
          do j = 1,jm
             do i = 1,im
                kmt(i,j) = kmt_l(i,j)
                kmf(i,j) = kmf_l(i,j)
                xt(i,j) = xt_l(i,j)
                yt(i,j) = yt_l(i,j)
                dxt(i,j) = dxt_l(i,j)
                dyt(i,j) = dyt_l(i,j)
                xu(i,j) = xu_l(i,j)
                yu(i,j) = yu_l(i,j)
                dxu(i,j) = dxu_l(i,j)
                dyu(i,j) = dyu_l(i,j)
                xv(i,j) = xv_l(i,j)
                yv(i,j) = yv_l(i,j)
                dxv(i,j) = dxv_l(i,j)
                dyv(i,j) = dyv_l(i,j)
                xf(i,j) = xf_l(i,j)
                yf(i,j) = yf_l(i,j)
                dxf(i,j) = dxf_l(i,j)
                dyf(i,j) = dyf_l(i,j)
             enddo
          enddo

          return
        end subroutine cut_flame_grid
!----------------------------------------------------------

!----------------------------------------------------------
        subroutine load_flame_time_axis
!----------------------------------------------------------
! This subroutine will load the flame time axis, similarly
! to the loading of the xy grid.  
!----------------------------------------------------------
          use grids
          implicit none

          integer :: io, line_count, year, month, day
          integer :: i,j

          ! Read in FLAME file date
          ! look up table.  First scoll
          ! through the lines to find
          ! the number of snapshots,
          ! allocate space, and then
          ! read the list of years,
          ! months and days into
          ! time_axis_<y|m|d>.

          write(*,*) ' Loading FLAME time axis...'

          io = 42
          open(io,file='FLAME.ymd',status='old',form='formatted')
          do line_count = 1,9999999
             read(io,'(i4,x,i2,x,i2)',end=33)
          enddo
33        line_count = line_count - 1

          ! Clean up and close.
          rewind(io)
          close(io)

          write(*,*) ' Found ',line_count,' time steps.'

          ! Allocate space for the y,m,d list
          allocate(time_axis_y(line_count))
          allocate(time_axis_m(line_count))
          allocate(time_axis_d(line_count))
          time_axis_len = line_count

          ! Initialize lookup table
          date_lookup = 0

          open(io,file='FLAME.ymd',status='old',form='formatted')
          do i = 1,line_count
             read(io,'(i4,x,i2,x,i2)') year,month,day
             time_axis_y(i) = real(year)
             time_axis_m(i) = real(month)
             time_axis_d(i) = real(day)

             if (date_lookup(year,month) .eq. 0) then
                ! We have reached the first instance
                ! of this year, month combination.
                ! Log the result in the table so that
                ! subsequent checks do not overwrite
                ! this index.
                date_lookup(year,month) = i
             endif
          enddo

          ! Verify
#ifdef verbose
          !do i = 1990,2004
          !   do j = 1,12
          !      write(*,*) i,j,date_lookup(i,j)
          !   enddo
          !enddo
#endif
          ! Clean up and close.
          rewind(io)
          close(io)

          write(*,*) ' Done reading model date lookup table.'

          return
        end subroutine load_flame_time_axis
!----------------------------------------------------------
#endif
#if defined (orca025) || defined (orca025_global) || defined (viking20_cut) || defined (viking20_full) 
!----------------------------------------------------------
        subroutine load_orca025_grid
!----------------------------------------------------------
! This  subroutine will load the grid and topography
! variables defining the ORCA025 model grid.
! These values are read from the files
! ORCA025_mesh_hgr.nc
! ORCA025_mesh_zgr.nc
! ORCA025_mask.nc
!----------------------------------------------------------

          use params
          use netcdfio
          use grids

          implicit none

          ! Local holder value due to input format
          ! of ORCA grid variables.  NOTE: some
          ! variables are double precision.
          real, allocatable :: hold4d(:,:,:,:)
          real(kind=8), allocatable :: dhold4d(:,:,:,:)
          real, allocatable :: hold5d(:,:,:,:,:)
          real(kind=8), allocatable :: dhold5d(:,:,:,:,:)

          ! Local counters
          integer :: i, j, k, n, nvar2read, vid

          ! List of variable names to load
          character(len=7), allocatable :: varnames(:)

#ifdef verbose
          write(*,*) ' Loading ORCA025 model grid...'
#endif
          !------Allocate memory for grid-------
          ! These variables were declared in the
          ! grids module.  See comments there
          ! for definitions and meanings.
          allocate(kmt(imt,jmt))
          allocate(kmf(imt,jmt))
          allocate(umask(imt,jmt,km))
          allocate(vmask(imt,jmt,km))
          allocate(tmask(imt,jmt,km))
          allocate(fmask(imt,jmt,km))
          allocate(xt(imt,jmt),yt(imt,jmt))
          allocate(xu(imt,jmt),yu(imt,jmt))
          allocate(xv(imt,jmt),yv(imt,jmt))
          allocate(xf(imt,jmt),yf(imt,jmt))
          allocate(dxt(imt,jmt),dyt(imt,jmt))
          allocate(dxu(imt,jmt),dyu(imt,jmt))
          allocate(dxv(imt,jmt),dyv(imt,jmt))
          allocate(dxf(imt,jmt),dyf(imt,jmt))
          allocate(zt(km),zw(km))
          allocate(dzt(imt,jmt,km),dzw(imt,jmt,km))

          ! Assign indices to readstart (unchanged).
          readstart(1) = 1
          readstart(2) = 1
          readstart(3) = 1
          readstart(4) = 1
          
          ! Last value of readcount (time index) is also unchanged.
          readcount(4) = 1
          
          ! Open depth defining file
          infid = ncopn('ORCA025_mesh_zgr.nc', ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)'Sucessful opening of ORCA025_mesh_zgr.nc.'
#endif
          ! The values to be read are 1D, depth only.
          readcount(1) = 1
          readcount(2) = 1
          readcount(3) = km
          
          ! Allocate holder variable accordingly
          allocate(hold4d(1,1,km,1))
          
          ! We have two variables of this type to read in
          nvar2read = 2
          
          ! Assign the names
          allocate(varnames(2))
          varnames(1) = 'gdept'
          varnames(2) = 'gdepw'

          ! Allocate holder variable
          allocate(hold5d(1,1,km,1,nvar2read))
          hold5d = 0.0
          
          ! For each variable to read,
          do n = 1,nvar2read
             
             ! Get variable ID and read topography information
             hold4d = 0.0
             vid = ncvid(infid, trim(varnames(n)), exitcode)
             call ncvgt(infid, vid, readstart, readcount, hold4d, exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*)' Read ',trim(varnames(n))
#endif
             ! Copy value to storage array
             do k = 1,km
                hold5d(1,1,k,1,n) = hold4d(1,1,k,1)
             enddo
          enddo
      
          ! Split the storage array into variables:
          do k = 1,km
             zt(k) = hold5d(1,1,k,1,1)
             zw(k) = hold5d(1,1,k,1,2)
          enddo

          deallocate(hold4d, hold5d, varnames)

          ! NOTE: No automation for 3D spacing variables
          ! because there are only two and the memory
          ! costs would be large.
       
          ! The spacing values are 3D, resize:
          readcount(1) = imt
          readcount(2) = jmt
          readcount(3) = km
          allocate(hold4d(imt,jmt,km,1))
       
          ! Get variable ID and read topography information
          hold4d = 0.0
          vid = ncvid(infid, 'e3t', exitcode)
          call ncvgt(infid,vid,readstart,readcount,hold4d,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read e3t.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   dzt(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo

          ! Get variable ID and read topography information
          hold4d = 0.0
          vid = ncvid(infid, 'e3w', exitcode)
          call ncvgt(infid, vid, readstart, readcount, hold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read e3w.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   dzw(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo

          deallocate(hold4d)
          
          ! Close the file
          call ncclos(infid, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) ' Closed ORCA025_mesh_zgr.nc.'
#endif
          ! Open the horizontal defining file
          infid = ncopn('ORCA025_mesh_hgr.nc', ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)'Sucessful opening of ORCA025_mesh_hgr.nc.'
#endif
          !---HORIZONTAL NODE LOCATIONS and SCALE FACTORS---
          !---The values to be read are all 2D---
          readcount(1) = imt
          readcount(2) = jmt
          readcount(3) = 1
          allocate(dhold4d(imt,jmt,1,1))

          ! There are 16 variables of this type
          nvar2read = 16
      
          ! Assign the list of names
          allocate(varnames(16))
          varnames(1) = 'glamt'
          varnames(2) = 'gphit'
          varnames(3) = 'glamu'
          varnames(4) = 'gphiu'
          varnames(5) = 'glamv'
          varnames(6) = 'gphiv'
          varnames(7) = 'glamf'
          varnames(8) = 'gphif'
          varnames(9) = 'e1t'
          varnames(10)= 'e2t'
          varnames(11)= 'e1u'
          varnames(12)= 'e2u'
          varnames(13)= 'e1v'
          varnames(14)= 'e2v'
          varnames(15)= 'e1f'
          varnames(16)= 'e2f'
      
          ! Define size of storage array
          allocate(dhold5d(imt,jmt,1,1,16))
          dhold5d = 0.0
      
          ! For each variable to read,
          do n = 1,nvar2read
             
             ! Get variable ID and read information
             dhold4d = 0.0
             vid = ncvid(infid, trim(varnames(n)), exitcode)
             call ncvgt(infid, vid, readstart, readcount, dhold4d, exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*)' Read ',trim(varnames(n))
#endif
             ! Copy value to long term storage array
             do j = 1,jmt
                do i = 1,imt
                   dhold5d(i,j,1,1,n) = dhold4d(i,j,1,1)
                enddo
             enddo
          enddo
      
          ! Split the storage array into variables
          do j = 1,jmt
             do i = 1,imt
                xt(i,j) = dhold5d(i,j,1,1,1)
                yt(i,j) = dhold5d(i,j,1,1,2)
                xu(i,j) = dhold5d(i,j,1,1,3)
                yu(i,j) = dhold5d(i,j,1,1,4)
                xv(i,j) = dhold5d(i,j,1,1,5)
                yv(i,j) = dhold5d(i,j,1,1,6)
                xf(i,j) = dhold5d(i,j,1,1,7)
                yf(i,j) = dhold5d(i,j,1,1,8)
                dxt(i,j) = dhold5d(i,j,1,1,9)
                dyt(i,j) = dhold5d(i,j,1,1,10)
                dxu(i,j) = dhold5d(i,j,1,1,11)
                dyu(i,j) = dhold5d(i,j,1,1,12)
                dxv(i,j) = dhold5d(i,j,1,1,13)
                dyv(i,j) = dhold5d(i,j,1,1,14)
                dxf(i,j) = dhold5d(i,j,1,1,15)
                dyf(i,j) = dhold5d(i,j,1,1,16)
             enddo
          enddo

          deallocate(dhold4d, dhold5d, varnames)

          ! Close the file
          call ncclos(infid, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) ' Closed ORCA025_mesh_hgr.nc'
#endif
          ! Open the mask file
          infid = ncopn('ORCA025_mask.nc', ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Sucessful opening of ORCA025_mask.nc.'
#endif
          ! The mask values are 3D
          readcount(1) = imt
          readcount(2) = jmt
          readcount(3) = km
          allocate(hold4d(imt,jmt,km,1))       
          
          ! Read in the t-grid mask
          hold4d = 0.0
          vid = ncvid(infid, 'tmask', exitcode)
          call ncvgt(infid, vid, readstart, readcount, hold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read tmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   tmask(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the u-grid mask
          hold4d = 0.0
          vid = ncvid(infid, 'umask', exitcode)
          call ncvgt(infid, vid, readstart, readcount, hold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read umask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   umask(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the v-grid mask
          hold4d = 0.0
          vid = ncvid(infid, 'vmask', exitcode)
          call ncvgt(infid, vid, readstart, readcount, hold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read vmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   vmask(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the f-grid mask
          hold4d = 0.0
          vid = ncvid(infid, 'fmask', exitcode)
          call ncvgt(infid, vid, readstart, readcount, hold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read fmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   fmask(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          deallocate(hold4d)
          
          ! Close the file
          call ncclos(infid, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Closed ORCA025_mask.nc'
#endif
          ! Build km_ from the _mask information
          ! (this is necessary for a standardized
          ! bottom check for both FLAME and ORCA
          ! in the isopycnal subroutine.  The result
          ! here is the same as loading the mbathy
          ! variable in mesh_zgr.nc.
          kmt = 0
          kmf = 0
          do j = 1,jmt
             do i = 1,imt
                ! Compute the sum down this column
                do k = 1,km
                   kmt(i,j) = kmt(i,j) + tmask(i,j,k)
                   kmf(i,j) = kmf(i,j) + fmask(i,j,k)
                enddo
             enddo
          enddo
          
          return
        end subroutine load_orca025_grid
!----------------------------------------------------------

!----------------------------------------------------------
        subroutine load_orca025_cut_grid
!----------------------------------------------------------
! This subroutine will load the grid but cut according to
! readstart and readcount from:
! ORCA025_mesh_hgr.nc
! ORCA025_mesh_zgr.nc
! ORCA025_mask.nc
!----------------------------------------------------------

          use params
          use netcdfio
          use grids

          implicit none

          ! Local holder value due to input format
          ! of ORCA grid variables.  NOTE: some
          ! variables are double precision.
          real, allocatable :: hold4d(:,:,:,:)
          real(kind=8), allocatable :: dhold4d(:,:,:,:)
          real, allocatable :: hold5d(:,:,:,:,:)
          real(kind=8), allocatable :: dhold5d(:,:,:,:,:)

          ! Local counters
          integer :: i, j, k, n, nvar2read, vid

          ! Local storage of the cut grid extent
          integer :: ii,jj,im,jm

          ! List of variable names to load
          character(len=7), allocatable :: varnames(:)

#ifdef verbose
          write(*,*) ' Loading ORCA025 model cut grid...'
#endif
          ii = readstart(1)
          jj = readstart(2)
          im = readcount(1)
          jm = readcount(2)

          !------Allocate memory for grid-------
          ! These variables were declared in the
          ! grids module.  See comments there
          ! for definitions and meanings.
          allocate(kmt(im,jm))
          allocate(kmf(im,jm))
          allocate(umask(im,jm,km))
          allocate(vmask(im,jm,km))
          allocate(tmask(im,jm,km))
          allocate(fmask(im,jm,km))
          allocate(xt(im,jm),yt(im,jm))
          allocate(xu(im,jm),yu(im,jm))
          allocate(xv(im,jm),yv(im,jm))
          allocate(xf(im,jm),yf(im,jm))
          allocate(dxt(im,jm),dyt(im,jm))
          allocate(dxu(im,jm),dyu(im,jm))
          allocate(dxv(im,jm),dyv(im,jm))
          allocate(dxf(im,jm),dyf(im,jm))
          !allocate(zt(km),zw(km))  !Not reloaded b/c just fine
          allocate(dzt(im,jm,km),dzw(im,jm,km))

          ! Open depth defining file
          infid = ncopn('ORCA025_mesh_zgr.nc', ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)'Sucessful opening of ORCA025_mesh_zgr.nc.'
#endif
          ! Skip reloading of zt and zw
          ! Below load dzt and dzw.
          allocate(hold4d(im,jm,km,1))

          ! Get variable ID, read topography information,
          ! and copy to long-term storage variable.
          hold4d = 0.0
          vid = ncvid(infid, 'e3t', exitcode)
          call ncvgt(infid,vid,readstart,readcount,hold4d,exitcode)
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   dzt(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo

          hold4d = 0.0
          vid = ncvid(infid, 'e3w', exitcode)
          call ncvgt(infid,vid,readstart,readcount,hold4d,exitcode)
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   dzw(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo
          deallocate(hold4d)
          
          ! Close the file
          call ncclos(infid, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) ' Closed ORCA025_mesh_zgr.nc.'
#endif
          ! Filter thicknesses
          do k = 1,km
             do j = 1,jm
                 do i = 1,im
                    if ((dzt(i,j,k).lt.0.0).or.(isnan(dzt(i,j,k))).or.(dzt(i,j,k).gt.1000.0)) dzt(i,j,k) = 0.0
                 enddo
              enddo
           enddo

          ! Open the horizontal defining file
          infid = ncopn('ORCA025_mesh_hgr.nc', ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)'Sucessful opening of ORCA025_mesh_hgr.nc.'
#endif
          !---HORIZONTAL NODE LOCATIONS and SCALE FACTORS---
          !---The values to be read are all 2D---
          allocate(dhold4d(im,jm,1,1))

          ! There are 16 variables of this type
          nvar2read = 16
      
          ! Assign the list of names
          allocate(varnames(16))
          varnames(1) = 'glamt'
          varnames(2) = 'gphit'
          varnames(3) = 'glamu'
          varnames(4) = 'gphiu'
          varnames(5) = 'glamv'
          varnames(6) = 'gphiv'
          varnames(7) = 'glamf'
          varnames(8) = 'gphif'
          varnames(9) = 'e1t'
          varnames(10)= 'e2t'
          varnames(11)= 'e1u'
          varnames(12)= 'e2u'
          varnames(13)= 'e1v'
          varnames(14)= 'e2v'
          varnames(15)= 'e1f'
          varnames(16)= 'e2f'
      
          ! Define size of storage array
          allocate(dhold5d(im,jm,1,1,16))
          dhold5d = 0.0

          readst4d(1) = readstart(1)
          readst4d(2) = readstart(2)
          readst4d(3) = 1
          readst4d(4) = 1
      
          readct4d(1) = readcount(1)
          readct4d(2) = readcount(2)
          readct4d(3) = 1
          readct4d(4) = 1

          ! For each variable to read,
          do n = 1,nvar2read
             
! WORKING HERE NEED TO BE CAREFUL WITH READ INDECES

             ! Get variable ID and read information
             dhold4d = 0.0
             vid = ncvid(infid, trim(varnames(n)), exitcode)
             call ncvgt(infid,vid,readst4d,readct4d,dhold4d,exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*)' Read ',trim(varnames(n))
#endif
             ! Copy value to long term storage array
             do j = 1,jm
                do i = 1,im
                   dhold5d(i,j,1,1,n) = dhold4d(i,j,1,1)
                enddo
             enddo
          enddo
      
          ! Split the storage array into variables
          do j = 1,jm
             do i = 1,im
                xt(i,j) = dhold5d(i,j,1,1,1)
                yt(i,j) = dhold5d(i,j,1,1,2)
                xu(i,j) = dhold5d(i,j,1,1,3)
                yu(i,j) = dhold5d(i,j,1,1,4)
                xv(i,j) = dhold5d(i,j,1,1,5)
                yv(i,j) = dhold5d(i,j,1,1,6)
                xf(i,j) = dhold5d(i,j,1,1,7)
                yf(i,j) = dhold5d(i,j,1,1,8)
                dxt(i,j) = dhold5d(i,j,1,1,9)
                dyt(i,j) = dhold5d(i,j,1,1,10)
                dxu(i,j) = dhold5d(i,j,1,1,11)
                dyu(i,j) = dhold5d(i,j,1,1,12)
                dxv(i,j) = dhold5d(i,j,1,1,13)
                dyv(i,j) = dhold5d(i,j,1,1,14)
                dxf(i,j) = dhold5d(i,j,1,1,15)
                dyf(i,j) = dhold5d(i,j,1,1,16)
             enddo
          enddo

          deallocate(dhold4d, dhold5d, varnames)

          ! Close the file
          call ncclos(infid, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) ' Closed ORCA025_mesh_hgr.nc'
#endif
          ! Open the mask file
          infid = ncopn('ORCA025_mask.nc', ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Sucessful opening of ORCA025_mask.nc.'
#endif
          allocate(hold4d(im,jm,km,1))       
          
          ! Read in the t-grid mask
          hold4d = 0.0
          vid = ncvid(infid,'tmask',exitcode)
          call ncvgt(infid,vid,readstart,readcount,hold4d,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read tmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   tmask(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the u-grid mask
          hold4d = 0.0
          vid = ncvid(infid, 'umask', exitcode)
          call ncvgt(infid,vid,readstart,readcount,hold4d,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read umask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   umask(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the v-grid mask
          hold4d = 0.0
          vid = ncvid(infid,'vmask',exitcode)
          call ncvgt(infid,vid,readstart,readcount,hold4d,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read vmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   vmask(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the f-grid mask
          hold4d = 0.0
          vid = ncvid(infid, 'fmask', exitcode)
          call ncvgt(infid,vid,readstart,readcount,hold4d,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read fmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   fmask(i,j,k) = hold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          deallocate(hold4d)
          
          ! Close the file
          call ncclos(infid, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Closed ORCA025_mask.nc'
#endif
          ! Build km_ from the _mask information
          ! (this is necessary for a standardized
          ! bottom check for both FLAME and ORCA
          ! in the isopycnal subroutine.  The result
          ! here is the same as loading the mbathy
          ! variable in mesh_zgr.nc.
          kmt = 0
          kmf = 0
          do j = 1,jm
             do i = 1,im
                ! Compute the sum down this column
                do k = 1,km
                   kmt(i,j) = kmt(i,j) + tmask(i,j,k)
                   kmf(i,j) = kmf(i,j) + fmask(i,j,k)
                enddo
             enddo
          enddo
          return
        end subroutine load_orca025_cut_grid

!----------------------------------------------------------
!----------------------------------------------------------
        subroutine load_orca_mesh_mask
!----------------------------------------------------------
          use params
          use netcdfio
          use grids

          implicit none
          
!----------------------------------------------------------
! This  subroutine will load the grid and topography
! variables defining an ORCA model grid.
! These values are read from the file mesh_mask.nc
! This should work for both ORCA025 and ORCA05 mesh masks.
!----------------------------------------------------------

          ! Local holder value due to input format
          ! of ORCA grid variables.  NOTE: some
          ! variables are integers or double precision.
          
          ! For masks - no automation
          integer(kind=1), allocatable :: ihold4d(:,:,:,:)

          ! For g<lam|phi><t|u|v|f>
          real, allocatable :: rhold3d(:,:,:)
          real, allocatable :: rhold4d(:,:,:,:)
          
          ! For e<1|2><t|u|v|f>
          real(kind=8), allocatable :: dhold3d(:,:,:)
          real(kind=8), allocatable :: dhold4d(:,:,:,:)

          ! For e3<t,u,v,f>
          ! Use dhold4d declared above - no automation.
          
          ! For gdep<t|w>_0 - no automation.
          real(kind=8), allocatable :: dhold2d(:,:)
          
          ! Local counters
          integer :: i, j, k, n, nvar2read, vid
          
          ! List of variable names to load
          character(len=7), allocatable :: varnames(:)
          
          ! The 4d equivalents were already declared.
          !integer :: readst2d(1,2)
          !integer :: readco2d(1,2)
#ifdef verbose
          write(*,*) ' Loading ORCA model grid...'
#endif
          !------Allocate memory for grid-------
          ! These variables were declared in the
          ! grids module.  See comments there
          ! for definitions and meanings.
          allocate(kmt(imt,jmt))
          allocate(kmf(imt,jmt))
          allocate(umask(imt,jmt,km))
          allocate(vmask(imt,jmt,km))
          allocate(tmask(imt,jmt,km))
          allocate(fmask(imt,jmt,km))
          allocate(xt(imt,jmt),yt(imt,jmt))
          allocate(xu(imt,jmt),yu(imt,jmt))
          allocate(xv(imt,jmt),yv(imt,jmt))
          allocate(xf(imt,jmt),yf(imt,jmt))
          allocate(dxt(imt,jmt),dyt(imt,jmt))
          allocate(dxu(imt,jmt),dyu(imt,jmt))
          allocate(dxv(imt,jmt),dyv(imt,jmt))
          allocate(dxf(imt,jmt),dyf(imt,jmt))
          allocate(zt(km),zw(km))
          allocate(dzt(imt,jmt,km),dzw(imt,jmt,km))

          ! Assign indices to readstart (unchanged).
          readst4d(1) = 1
          readst4d(2) = 1
          readst4d(3) = 1
          readst4d(4) = 1
          
          readst2d(1) = 1
          readst2d(2) = 1
          
          readst3d(1) = 1
          readst3d(2) = 1
          readst3d(3) = 1

          ! Last value of readcount (time index) is also unchanged.
          readct4d(4) = 1

          ! Open mesh mask file
          infid = ncopn('mesh_mask.nc', ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)'Opened mesh_mask.nc.'
#endif
          !------READ LEVEL LOCATIONS------
          ! The values to be read are 1D, depth only.
          readct2d(1) = km
          readct2d(2) = 1
      
          ! Allocate holder variable accordingly
          allocate(dhold2d(km,1))
          
          ! Get variable ID and read topography information
          dhold2d = 0.0
          vid = ncvid(infid,'gdept_0', exitcode)
          call ncvgt(infid, vid, readst2d, readct2d, dhold2d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read gdept_0'
#endif
          do k = 1,km
             zt(k) = dhold2d(k,1)
          enddo
          
          dhold2d = 0.0
          vid = ncvid(infid,'gdepw_0', exitcode)
          call ncvgt(infid, vid, readst2d, readct2d, dhold2d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read gdepw_0'
#endif
          do k = 1,km
             zw(k) = dhold2d(k,1)
          enddo
          deallocate(dhold2d)

          !-----READ LEVEL SPACING INFORMATION-----
          ! The spacing values are 3D, resize:
          readct4d(1) = imt
          readct4d(2) = jmt
          readct4d(3) = km
          allocate(dhold4d(imt,jmt,km,1))
       
          ! Get variable ID and read topography information
          dhold4d = 0.0
          vid = ncvid(infid, 'e3t', exitcode)
          call ncvgt(infid,vid,readst4d,readct4d,dhold4d,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read e3t.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   dzt(i,j,k) = dhold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Get variable ID and read topography information
          dhold4d = 0.0
          vid = ncvid(infid, 'e3w', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, dhold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read e3w.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   dzw(i,j,k) = dhold4d(i,j,k,1)
                enddo
             enddo
          enddo
          deallocate(dhold4d)
          
          !------HORIZONTAL NODE LOCATIONS------
          readct3d(1) = imt
          readct3d(2) = jmt
          readct3d(3) = 1
          allocate(rhold3d(imt,jmt,1))

          ! There are 8 variables of this type
          nvar2read = 8
          
          ! Assign the list of names
          allocate(varnames(nvar2read))
          varnames(1) = 'glamt'
          varnames(2) = 'gphit'
          varnames(3) = 'glamu'
          varnames(4) = 'gphiu'
          varnames(5) = 'glamv'
          varnames(6) = 'gphiv'
          varnames(7) = 'glamf'
          varnames(8) = 'gphif'
      
          ! Define size of storage array
          allocate(rhold4d(imt,jmt,1,8))
          rhold4d = 0.0
      
          ! For each variable to read,
          do n = 1,nvar2read
             
             ! Get variable ID and read information
             rhold3d = 0.0
             vid = ncvid(infid, trim(varnames(n)), exitcode)
             call ncvgt(infid, vid, readst3d, readct3d, rhold3d, exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*)' Read ',trim(varnames(n))
#endif
             ! Copy value to long term storage array
             do j = 1,jmt
                do i = 1,imt
                   rhold4d(i,j,1,n) = rhold3d(i,j,1)
                enddo
             enddo
          enddo
          
          ! Split the storage array into variables
          do j = 1,jmt
             do i = 1,imt
                xt(i,j) = rhold4d(i,j,1,1)
                yt(i,j) = rhold4d(i,j,1,2)
                xu(i,j) = rhold4d(i,j,1,3)
                yu(i,j) = rhold4d(i,j,1,4)
                xv(i,j) = rhold4d(i,j,1,5)
                yv(i,j) = rhold4d(i,j,1,6)
                xf(i,j) = rhold4d(i,j,1,7)
                yf(i,j) = rhold4d(i,j,1,8)
             enddo
          enddo
          deallocate(rhold3d,rhold4d)
          
          !-----READ HORIZONTAL GRID SPACING------
          ! These values are the same as node locations
          ! except stored as double precision.  Yeaghhth.
          allocate(dhold4d(imt,jmt,1,nvar2read))
          allocate(dhold3d(imt,jmt,1))
          dhold4d = 0.0
          varnames(1)= 'e1t'
          varnames(2)= 'e2t'
          varnames(3)= 'e1u'
          varnames(4)= 'e2u'
          varnames(5)= 'e1v'
          varnames(6)= 'e2v'
          varnames(7)= 'e1f'
          varnames(8)= 'e2f'
          
          ! For each variable to read,
          do n = 1,nvar2read
             
             ! Get variable ID and read information
             dhold3d = 0.0
             vid = ncvid(infid, trim(varnames(n)), exitcode)
             call ncvgt(infid, vid, readst3d, readct3d, dhold3d, exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*)' Read ',trim(varnames(n))
#endif
             ! Copy value to long term storage array
             do j = 1,jmt
                do i = 1,imt
                   dhold4d(i,j,1,n) = dhold3d(i,j,1)
                enddo
             enddo
          enddo
          
          ! Split the storage array into variables
          do j = 1,jmt
             do i = 1,imt
                dxt(i,j) = dhold4d(i,j,1,1)
                dyt(i,j) = dhold4d(i,j,1,2)
                dxu(i,j) = dhold4d(i,j,1,3)
                dyu(i,j) = dhold4d(i,j,1,4)
                dxv(i,j) = dhold4d(i,j,1,5)
                dyv(i,j) = dhold4d(i,j,1,6)
                dxf(i,j) = dhold4d(i,j,1,7)
                dyf(i,j) = dhold4d(i,j,1,8)
             enddo
          enddo

          deallocate(dhold3d, dhold4d, varnames)
          
          !------MASK VALUES------
          ! The mask values are 4D in file, but 3D at end
          ! They are also stored as ncbyte for ORCA05.
          readcount(1) = imt
          readcount(2) = jmt
          readcount(3) = km
          allocate(ihold4d(imt,jmt,km,1))       

          ! Read in the t-grid mask
          ihold4d = 0
          vid = ncvid(infid, 'tmask', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read tmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   tmask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the u-grid mask
          ihold4d = 0.0
          vid = ncvid(infid, 'umask', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read umask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   umask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the v-grid mask
          ihold4d = 0.0
          vid = ncvid(infid, 'vmask', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read vmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   vmask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the f-grid mask
          ihold4d = 0.0
          vid = ncvid(infid, 'fmask', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read fmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   fmask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          deallocate(ihold4d)
      
          ! Close the file
          call ncclos(infid, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Closed mesh_mask.nc'
#endif
          ! Build kmt from the tmask information
          ! (this is necessary for a standardized
          ! bottom check for both FLAME and ORCA
          ! in the isopycnal subroutine.  The result
          ! here is the same as loading the mbathy
          ! variable in mesh_zgr.nc.
          kmt = 0
          kmf = 0
          do j = 1,jmt
             do i = 1,imt
                ! Compute the sum down this column
                do k = 1,km
                   kmt(i,j) = kmt(i,j) + tmask(i,j,k)
                   kmf(i,j) = kmf(i,j) + fmask(i,j,k)
                enddo
             enddo
          enddo
          
          return
        end subroutine load_orca_mesh_mask
!----------------------------------------------------------
!----------------------------------------------------------
        subroutine load_orca_mesh_mask_e3uv
!----------------------------------------------------------
          use params
          use netcdfio
          use grids

          implicit none
          
!----------------------------------------------------------
! This  subroutine will load the grid and topography
! variables defining an ORCA model grid for e3u and e3v
! only.  This is done to reduce memory use since these
! variables are not always needed.
! These values are read from the file mesh_mask.nc
! This should work for both ORCA025 and ORCA05 mesh masks.
!----------------------------------------------------------

          ! For e3<t,u,v,f>
          real(kind=8), allocatable :: dhold4d(:,:,:,:)
          
          ! Local counters
          integer :: i, j, k, n, nvar2read, vid
          
          ! List of variable names to load
          character(len=7), allocatable :: varnames(:)
          
#ifdef verbose
          write(*,*) ' Loading ORCA model grid e3u and e3v...'
#endif
          !------Allocate memory for grid-------
          ! These variables were declared in the
          ! grids module.  See comments there
          ! for definitions and meanings.
          allocate(e3u(imt,jmt,km),e3v(imt,jmt,km))

          ! Assign indices to readstart (unchanged).
          readst4d(1) = 1
          readst4d(2) = 1
          readst4d(3) = 1
          readst4d(4) = 1
          
          readst2d(1) = 1
          readst2d(2) = 1
          
          readst3d(1) = 1
          readst3d(2) = 1
          readst3d(3) = 1

          ! Last value of readcount (time index) is also unchanged.
          readct4d(4) = 1

          ! Open mesh mask file
          infid = ncopn('mesh_mask.nc', ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)'Opened mesh_mask.nc.'
#endif
          !-----READ LEVEL SPACING INFORMATION-----
          ! The spacing values are 3D, resize:
          readct4d(1) = imt
          readct4d(2) = jmt
          readct4d(3) = km
          allocate(dhold4d(imt,jmt,km,1))
       
          ! Get variable ID and read topography information
          dhold4d = 0.0
          vid = ncvid(infid, 'e3u', exitcode)
          call ncvgt(infid,vid,readst4d,readct4d,dhold4d,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read e3u.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   e3u(i,j,k) = dhold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Get variable ID and read topography information
          dhold4d = 0.0
          vid = ncvid(infid, 'e3v', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, dhold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read e3v.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   e3v(i,j,k) = dhold4d(i,j,k,1)
                enddo
             enddo
          enddo
          deallocate(dhold4d)
          
          ! Close the file
          call ncclos(infid, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Closed mesh_mask.nc'
#endif
          return
        end subroutine load_orca_mesh_mask_e3uv
!----------------------------------------------------------
!----------------------------------------------------------
        subroutine load_orca_cut_mesh_mask
!----------------------------------------------------------
          use params
          use netcdfio
          use grids

          implicit none
          
!----------------------------------------------------------
! This  subroutine will load the grid and topography
! variables defining an ORCA model grid based on the
! extents defined by the global variables readstart
! and readcount.
! These values are read from the file mesh_mask.nc
! This should work for both ORCA025 and ORCA05 mesh masks.
!----------------------------------------------------------

          ! Local holder value due to input format
          ! of ORCA grid variables.  NOTE: some
          ! varaibles are integers or double precision.
          
          ! For masks
          integer(kind=1), allocatable :: ihold4d(:,:,:,:)

          ! For g<lam|phi><t|u|v|f>
          real, allocatable :: rhold3d(:,:,:)
          real, allocatable :: rhold4d(:,:,:,:)
          
          ! For e<1|2|3><t|u|v|f>
          real(kind=8), allocatable :: dhold3d(:,:,:)
          real(kind=8), allocatable :: dhold4d(:,:,:,:)

          ! Local counters
          integer :: i, j, k, n, nvar2read, vid
          
          ! Local storage of cut grid domain
          integer :: ii, jj, im, jm

          ! List of variable names to load
          character(len=7), allocatable :: varnames(:)
          
#ifdef verbose
          write(*,*) ' Loading ORCA model grid...'
#endif

          ii = readstart(1)
          jj = readstart(2)
          im = readcount(1)
          jm = readcount(2)

          !------Allocate memory for grid-------
          ! These variables were declared in the
          ! grids module.  See comments there
          ! for definitions and meanings.
          allocate(kmt(im,jm))
          allocate(kmf(im,jm))
          allocate(umask(im,jm,km))
          allocate(vmask(im,jm,km))
          allocate(tmask(im,jm,km))
          allocate(fmask(im,jm,km))
          allocate(xt(im,jm),yt(im,jm))
          allocate(xu(im,jm),yu(im,jm))
          allocate(xv(im,jm),yv(im,jm))
          allocate(xf(im,jm),yf(im,jm))
          allocate(dxt(im,jm),dyt(im,jm))
          allocate(dxu(im,jm),dyu(im,jm))
          allocate(dxv(im,jm),dyv(im,jm))
          allocate(dxf(im,jm),dyf(im,jm))
          allocate(dzt(im,jm,km),dzw(im,jm,km))

          ! Open mesh mask file
          infid = ncopn('mesh_mask.nc', ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)'Opened mesh_mask.nc.'
#endif
          !------READ LEVEL LOCATIONS------
          ! Unneccessary because zt and zw
          ! never change regardless of the
          ! specified 2D domain.  zt and zw
          ! are NOT cleared during wipe_grid.


          !-----READ LEVEL SPACING INFORMATION-----
          ! The spacing values are 3D, resize:
          readst4d(1) = ii
          readst4d(2) = jj
          readst4d(3) = 1
          readst4d(4) = 1

          readct4d(1) = im
          readct4d(2) = jm
          readct4d(3) = km
          readct4d(4) = 1

          allocate(dhold4d(im,jm,km,1))
          dhold4d = 0.0
       
          ! Get variable ID and read topography information
          vid = ncvid(infid, 'e3t', exitcode)
          call ncvgt(infid,vid,readst4d,readct4d,dhold4d,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read e3t.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   dzt(i,j,k) = dhold4d(i,j,k,1)
                enddo
             enddo
          enddo
          dhold4d = 0.0

          ! Get variable ID and read topography information
          vid = ncvid(infid, 'e3w', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, dhold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read e3w.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   dzw(i,j,k) = dhold4d(i,j,k,1)
                enddo
             enddo
          enddo
          deallocate(dhold4d)
          
          !------HORIZONTAL NODE LOCATIONS------
          readst3d(1) = ii
          readst3d(2) = jj
          readst3d(3) = 1

          readct3d(1) = im
          readct3d(2) = jm
          readct3d(3) = 1

          allocate(rhold3d(im,jm,1))
          rhold3d = 0.0

          ! There are 8 variables of this type
          nvar2read = 8
          
          ! Assign the list of names
          allocate(varnames(nvar2read))
          varnames(1) = 'glamt'
          varnames(2) = 'gphit'
          varnames(3) = 'glamu'
          varnames(4) = 'gphiu'
          varnames(5) = 'glamv'
          varnames(6) = 'gphiv'
          varnames(7) = 'glamf'
          varnames(8) = 'gphif'
      
          ! Define size of storage array
          allocate(rhold4d(im,jm,1,nvar2read))
          rhold4d = 0.0
      
          ! For each variable to read,
          do n = 1,nvar2read
             
             ! Get variable ID and read information
             vid = ncvid(infid, trim(varnames(n)), exitcode)
             call ncvgt(infid, vid, readst3d, readct3d, rhold3d, exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*)' Read ',trim(varnames(n))
#endif
             ! Copy value to long term storage array
             do j = 1,jm
                do i = 1,im
                   rhold4d(i,j,1,n) = rhold3d(i,j,1)
                enddo
             enddo
             rhold3d = 0.0

          enddo
          
          ! Split the storage array into variables
          do j = 1,jm
             do i = 1,im
                xt(i,j) = rhold4d(i,j,1,1)
                yt(i,j) = rhold4d(i,j,1,2)
                xu(i,j) = rhold4d(i,j,1,3)
                yu(i,j) = rhold4d(i,j,1,4)
                xv(i,j) = rhold4d(i,j,1,5)
                yv(i,j) = rhold4d(i,j,1,6)
                xf(i,j) = rhold4d(i,j,1,7)
                yf(i,j) = rhold4d(i,j,1,8)
             enddo
          enddo
          deallocate(rhold3d,rhold4d)
          
          !-----READ HORIZONTAL GRID SPACING------
          ! These values are the same as node locations
          ! except stored as double precision.  Yeaghhth.
          allocate(dhold4d(im,jm,1,nvar2read))
          allocate(dhold3d(im,jm,1))
          dhold3d = 0.0
          dhold4d = 0.0
          varnames(1) = 'e1t'
          varnames(2) = 'e2t'
          varnames(3) = 'e1u'
          varnames(4) = 'e2u'
          varnames(5) = 'e1v'
          varnames(6) = 'e2v'
          varnames(7) = 'e1f'
          varnames(8) = 'e2f'
          
          ! For each variable to read,
          do n = 1,nvar2read
             
             ! Get variable ID and read information
             vid = ncvid(infid, trim(varnames(n)), exitcode)
             call ncvgt(infid, vid, readst3d, readct3d, dhold3d, exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*)' Read ',trim(varnames(n))
#endif
             ! Copy value to long term storage array
             do j = 1,jm
                do i = 1,im
                   dhold4d(i,j,1,n) = dhold3d(i,j,1)
                enddo
             enddo
             dhold3d = 0.0

          enddo
          
          ! Split the storage array into variables
          do j = 1,jm
             do i = 1,im
                dxt(i,j) = dhold4d(i,j,1,1)
                dyt(i,j) = dhold4d(i,j,1,2)
                dxu(i,j) = dhold4d(i,j,1,3)
                dyu(i,j) = dhold4d(i,j,1,4)
                dxv(i,j) = dhold4d(i,j,1,5)
                dyv(i,j) = dhold4d(i,j,1,6)
                dxf(i,j) = dhold4d(i,j,1,7)
                dyf(i,j) = dhold4d(i,j,1,8)
             enddo
          enddo

          deallocate(dhold3d, dhold4d, varnames)
          
          !------MASK VALUES------
          ! The mask values are 4D in file, but 3D at end
          ! They are also stored as ncbyte.
          readst4d(1) = ii
          readst4d(2) = jj
          readst4d(3) = 1
          readst4d(4) = 1

          readct4d(1) = im
          readct4d(2) = jm
          readct4d(3) = km
          readct4d(4) = 1

          allocate(ihold4d(im,jm,km,1))       
          ihold4d = 0

          ! Read in the t-grid mask
          vid = ncvid(infid, 'tmask', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read tmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   tmask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          ihold4d = 0

          ! Read in the u-grid mask
          vid = ncvid(infid, 'umask', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read umask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   umask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          ihold4d = 0.0

          ! Read in the v-grid mask
          vid = ncvid(infid, 'vmask', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read vmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   vmask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          ihold4d = 0.0
          
          ! Read in the f-grid mask
          vid = ncvid(infid, 'fmask', exitcode)
          call ncvgt(infid, vid, readst4d, readct4d, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read fmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jm
                do i = 1,im
                   fmask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          deallocate(ihold4d)
      
          ! Close the file
          call ncclos(infid, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Closed mesh_mask.nc'
#endif
          ! Build kmt from the tmask information
          ! (this is necessary for a standardized
          ! bottom check for both FLAME and ORCA
          ! in the isopycnal subroutine.  The result
          ! here is the same as loading the mbathy
          ! variable in mesh_zgr.nc.
          kmt = 0
          kmf = 0
          do j = 1,jm
             do i = 1,im
                ! Compute the sum down this column
                do k = 1,km
                   kmt(i,j) = kmt(i,j) + tmask(i,j,k)
                   kmf(i,j) = kmf(i,j) + fmask(i,j,k)
                enddo
             enddo
          enddo
          
          return
        end subroutine load_orca_cut_mesh_mask
!----------------------------------------------------------
#endif
#ifdef orca050
!----------------------------------------------------------
        subroutine load_orca05_grid
!----------------------------------------------------------
          use params
          use netcdfio
          use grids

          implicit none
          
! This  subroutine will load the grid and topography
! variables defining the ORCA05 model grid.
! These values are read from the file
! ORCA05_mesh_mask.nc
!----------------------------------------------------------

          ! Local holder value due to input format
          ! of ORCA grid variables.  NOTE: some
          ! varaibles are integers or double precision.
          
          ! For masks - no automation
          integer(kind=1), allocatable :: ihold4d(:,:,:,:)

          ! For g<lam|phi><t|u|v|f>
          real, allocatable :: rhold3d(:,:,:)
          real, allocatable :: rhold4d(:,:,:,:)
          
          ! For e<1|2><t|u|v|f>
          real(kind=8), allocatable :: dhold3d(:,:,:)
          real(kind=8), allocatable :: dhold4d(:,:,:,:)

          ! For e3<t,u,v,f>
          ! Use dhold4d declared above - no automation.
          
          ! For gdep<t|w>_0 - no automation.
          real(kind=8), allocatable :: dhold2d(:,:)
          
          ! Local counters
          integer :: i, j, k, n, nvar2read, vid
          
          ! List of variable names to load
          character(len=7), allocatable :: varnames(:)
          
#ifdef verbose
          write(*,*) ' Loading ORCA05 model grid...'
#endif
          !------Allocate memory for grid-------
          ! These variables were declared in the
          ! grids module.  See comments there
          ! for definitions and meanings.
          allocate(kmt(imt,jmt))
          allocate(kmf(imt,jmt))
          allocate(umask(imt,jmt,km))
          allocate(vmask(imt,jmt,km))
          allocate(tmask(imt,jmt,km))
          allocate(fmask(imt,jmt,km))
          allocate(xt(imt,jmt),yt(imt,jmt))
          allocate(xu(imt,jmt),yu(imt,jmt))
          allocate(xv(imt,jmt),yv(imt,jmt))
          allocate(xf(imt,jmt),yf(imt,jmt))
          allocate(dxt(imt,jmt),dyt(imt,jmt))
          allocate(dxu(imt,jmt),dyu(imt,jmt))
          allocate(dxv(imt,jmt),dyv(imt,jmt))
          allocate(dxf(imt,jmt),dyf(imt,jmt))
          allocate(zt(km),zw(km))
          allocate(dzt(imt,jmt,km),dzw(imt,jmt,km))

          ! Assign indices to readstart (unchanged).
          readstart(1) = 1
          readstart(2) = 1
          readstart(3) = 1
          readstart(4) = 1
          
          readst2d(1) = 1
          readst2d(2) = 1
          
          readst3d(1) = 1
          readst3d(2) = 1
          readst3d(3) = 1

          ! Last value of readcount (time index) is also unchanged.
          readcount(4) = 1

          ! Open mesh mask file
          infid = ncopn('ORCA05_mesh_mask.nc', ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)'Sucessful opening of ORCA05_mesh_mask.nc.'
#endif
          !------READ LEVEL LOCATIONS------
          ! The values to be read are 1D, depth only.
          readct2d(1) = km
          readct2d(2) = 1
      
          ! Allocate holder variable accordingly
          allocate(dhold2d(km,1))
          
          ! Get variable ID and read topography information
          dhold2d = 0.0
          vid = ncvid(infid,'gdept_0', exitcode)
          call ncvgt(infid, vid, readst2d, readct2d, dhold2d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read gdept_0'
#endif
          do k = 1,km
             zt(k) = dhold2d(k,1)
          enddo
          
          dhold2d = 0.0
          vid = ncvid(infid,'gdepw_0', exitcode)
          call ncvgt(infid, vid, readst2d, readct2d, dhold2d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read gdepw_0'
#endif
          do k = 1,km
             zw(k) = dhold2d(k,1)
          enddo
          deallocate(dhold2d)

          !-----READ LEVEL SPACING INFORMATION-----
          ! The spacing values are 3D, resize:
          readcount(1) = imt
          readcount(2) = jmt
          readcount(3) = km
          allocate(dhold4d(imt,jmt,km,1))
       
          ! Get variable ID and read topography information
          dhold4d = 0.0
          vid = ncvid(infid, 'e3t', exitcode)
          call ncvgt(infid,vid,readstart,readcount,dhold4d,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read e3t.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   dzt(i,j,k) = dhold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Get variable ID and read topography information
          dhold4d = 0.0
          vid = ncvid(infid, 'e3w', exitcode)
          call ncvgt(infid, vid, readstart, readcount, dhold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read e3w.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   dzw(i,j,k) = dhold4d(i,j,k,1)
                enddo
             enddo
          enddo
          deallocate(dhold4d)
          
          !------HORIZONTAL NODE LOCATIONS------
          readct3d(1) = imt
          readct3d(2) = jmt
          readct3d(3) = 1
          allocate(rhold3d(imt,jmt,1))

          ! There are 8 variables of this type
          nvar2read = 8
          
          ! Assign the list of names
          allocate(varnames(nvar2read))
          varnames(1) = 'glamt'
          varnames(2) = 'gphit'
          varnames(3) = 'glamu'
          varnames(4) = 'gphiu'
          varnames(5) = 'glamv'
          varnames(6) = 'gphiv'
          varnames(7) = 'glamf'
          varnames(8) = 'gphif'
      
          ! Define size of storage array
          allocate(rhold4d(imt,jmt,1,8))
          rhold4d = 0.0
      
          ! For each variable to read,
          do n = 1,nvar2read
             
             ! Get variable ID and read information
             rhold3d = 0.0
             vid = ncvid(infid, trim(varnames(n)), exitcode)
             call ncvgt(infid, vid, readst3d, readct3d, rhold3d, exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*)' Read ',trim(varnames(n))
#endif
             ! Copy value to long term storage array
             do j = 1,jmt
                do i = 1,imt
                   rhold4d(i,j,1,n) = rhold3d(i,j,1)
                enddo
             enddo
          enddo
          
          ! Split the storage array into variables
          do j = 1,jmt
             do i = 1,imt
                xt(i,j) = rhold4d(i,j,1,1)
                yt(i,j) = rhold4d(i,j,1,2)
                xu(i,j) = rhold4d(i,j,1,3)
                yu(i,j) = rhold4d(i,j,1,4)
                xv(i,j) = rhold4d(i,j,1,5)
                yv(i,j) = rhold4d(i,j,1,6)
                xf(i,j) = rhold4d(i,j,1,7)
                yf(i,j) = rhold4d(i,j,1,8)
             enddo
          enddo
          deallocate(rhold3d,rhold4d)
          
          !-----READ HORIZONTAL GRID SPACING------
          ! These values are the same as node locations
          ! except stored as double precision.  Yeaghhth.
          allocate(dhold4d(imt,jmt,1,nvar2read))
          allocate(dhold3d(imt,jmt,1))
          dhold4d = 0.0
          varnames(1) = 'e1t'
          varnames(2)= 'e2t'
          varnames(3)= 'e1u'
          varnames(4)= 'e2u'
          varnames(5)= 'e1v'
          varnames(6)= 'e2v'
          varnames(7)= 'e1f'
          varnames(8)= 'e2f'
          
          ! For each variable to read,
          do n = 1,nvar2read
             
             ! Get variable ID and read information
             dhold3d = 0.0
             vid = ncvid(infid, trim(varnames(n)), exitcode)
             call ncvgt(infid, vid, readst3d, readct3d, dhold3d, exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*)' Read ',trim(varnames(n))
#endif
             ! Copy value to long term storage array
             do j = 1,jmt
                do i = 1,imt
                   dhold4d(i,j,1,n) = dhold3d(i,j,1)
                enddo
             enddo
          enddo
          
          ! Split the storage array into variables
          do j = 1,jmt
             do i = 1,imt
                dxt(i,j) = dhold4d(i,j,1,1)
                dyt(i,j) = dhold4d(i,j,1,2)
                dxu(i,j) = dhold4d(i,j,1,3)
                dyu(i,j) = dhold4d(i,j,1,4)
                dxv(i,j) = dhold4d(i,j,1,5)
                dyv(i,j) = dhold4d(i,j,1,6)
                dxf(i,j) = dhold4d(i,j,1,7)
                dyf(i,j) = dhold4d(i,j,1,8)
             enddo
          enddo

          deallocate(dhold3d, dhold4d, varnames)
          
          !------MASK VALUES------
          ! The mask values are 4D in file, but 3D at end
          ! They are also stored as ncbyte for ORCA05.
          readcount(1) = imt
          readcount(2) = jmt
          readcount(3) = km
          allocate(ihold4d(imt,jmt,km,1))       

          ! Read in the t-grid mask
          ihold4d = 0
          vid = ncvid(infid, 'tmask', exitcode)
          call ncvgt(infid, vid, readstart, readcount, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read tmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   tmask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the u-grid mask
          ihold4d = 0.0
          vid = ncvid(infid, 'umask', exitcode)
          call ncvgt(infid, vid, readstart, readcount, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read umask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   umask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the v-grid mask
          ihold4d = 0.0
          vid = ncvid(infid, 'vmask', exitcode)
          call ncvgt(infid, vid, readstart, readcount, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read vmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   vmask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          ! Read in the f-grid mask
          ihold4d = 0.0
          vid = ncvid(infid, 'fmask', exitcode)
          call ncvgt(infid, vid, readstart, readcount, ihold4d, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Read fmask.'
#endif
          ! Copy value to long term storage array
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   fmask(i,j,k) = ihold4d(i,j,k,1)
                enddo
             enddo
          enddo
          
          deallocate(ihold4d)
      
          ! Close the file
          call ncclos(infid, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*)' Closed ORCA05_mesh_mask.nc'
#endif
          ! Build kmt from the tmask information
          ! (this is necessary for a standardized
          ! bottom check for both FLAME and ORCA
          ! in the isopycnal subroutine.  The result
          ! here is the same as loading the mbathy
          ! variable in mesh_zgr.nc.
          kmt = 0
          kmf = 0
          do j = 1,jmt
             do i = 1,imt
                ! Compute the sum down this column
                do k = 1,km
                   kmt(i,j) = kmt(i,j) + tmask(i,j,k)
                   kmf(i,j) = kmf(i,j) + fmask(i,j,k)
                enddo
             enddo
          enddo
          
          return
        end subroutine load_orca05_grid
!----------------------------------------------------------
#endif
#if defined (storm) || defined (storm_cdo)
!----------------------------------------------------------
        subroutine load_storm_grid
!----------------------------------------------------------
          use params
          use netcdfio
          use grids

          implicit none
          
! This  subroutine will load the grid and topography
! variables defining the STORM model grid.
! These values are read from the file storm_grid.nc
!----------------------------------------------------------

          ! TO SAVE SPACE, NOT ALL GRID VALUES
          ! ARE LOADED HERE.  ADD AS NEEDED.

          ! For ofcdf, we need to be able to:
          ! interp from u-grid to t-grid
          ! interp from v-grid to t-grid
          ! rotate u,v on the t-grid
          ! define t-grid bathymetry
          ! The rest of ofcdf uses its
          ! own internally generated grids
          ! on the section of interest.
          !
          ! Therefore, we only need to load
          ! the bathymetry and locations of
          ! the t grid nodes.
          !
          !Variables that need to be read in:
          !xt, yt, depto (depth of t-grid nodes).

          ! Local counters
          integer :: i, j, k, vid
          
          ! Make local lists of readcount and readstart
          ! The 4d equivalents were already declared.
          !integer :: readst2d(2)
          !integer :: readco2d(2)

          ! Local grid size
          integer :: dg(2)
          real, allocatable :: hold4d(:,:,:,:)
          real(kind=8), allocatable :: hold1d(:)
          !=====================================

          dg(1) = imt
          dg(2) = jmt

          ! Single precision final results
          allocate(xt(imt,jmt))
          allocate(yt(imt,jmt))
          xt = 0.0
          yt = 0.0

!          allocate(xu(imt,jmt))
!          allocate(yu(imt,jmt))
!          xu = 0.0
!          yu = 0.0

!          allocate(xv(imt,jmt))
!          allocate(yv(imt,jmt))
!          xv = 0.0
!          yv = 0.0

          allocate(depto(imt,jmt,1,1))
          depto = 0.0
#ifdef storm_cdo
          ! CDO rotated grids are enormous,
          ! no need to load dzt yet.
#else
          allocate(dzt(imt,jmt,km))
          allocate(hold4d(imt,jmt,km,1))
#endif
          ! Hard coded definition of
          ! temperature depth levels.
          allocate(zt(1:km))
          zt(1) = 6.0
          zt(2) = 17.0
          zt(3) = 27.0
          zt(4) = 37.0
          zt(5) = 47.0
          zt(6) = 57.0
          zt(7) = 67.0
          zt(8) = 77.5
          zt(9) = 88.5
          zt(10) = 100.0
          zt(11) = 112.5
          zt(12) = 125.5
          zt(13) = 139.0
          zt(14) = 153.0
          zt(15) = 167.5
          zt(16) = 183.0
          zt(17) = 199.0
          zt(18) = 215.5
          zt(19) = 233.0
          zt(20) = 251.5
          zt(21) = 271.0
          zt(22) = 291.5
          zt(23) = 312.5
          zt(24) = 334.0
          zt(25) = 357.0
          zt(26) = 381.5
          zt(27) = 407.0
          zt(28) = 433.5
          zt(29) = 461.0
          zt(30) = 489.5
          zt(31) = 519.5
          zt(32) = 551.0
          zt(33) = 584.0
          zt(34) = 618.5
          zt(35) = 654.5
          zt(36) = 692.5
          zt(37) = 732.0
          zt(38) = 773.0
          zt(39) = 816.0
          zt(40) = 861.0
          zt(41) = 908.0
          zt(42) = 957.0
          zt(43) = 1008.5
          zt(44) = 1062.5
          zt(45) = 1119.0
          zt(46) = 1178.0
          zt(47) = 1239.5
          zt(48) = 1304.0
          zt(49) = 1371.5
          zt(50) = 1442.0
          zt(51) = 1516.0
          zt(52) = 1593.5
          zt(53) = 1674.5
          zt(54) = 1759.5
          zt(55) = 1848.5
          zt(56) = 1941.5
          zt(57) = 2038.5
          zt(58) = 2140.0
          zt(59) = 2246.0
          zt(60) = 2356.5
          zt(61) = 2472.5
          zt(62) = 2594.0
          zt(63) = 2721.0
          zt(64) = 2854.0
          zt(65) = 2993.0
          zt(66) = 3138.5
          zt(67) = 3290.5
          zt(68) = 3449.5
          zt(69) = 3616.0
          zt(70) = 3790.0
          zt(71) = 3972.0
          zt(72) = 4162.5
          zt(73) = 4362.0
          zt(74) = 4570.5
          zt(75) = 4788.5
          zt(76) = 5016.5
          zt(77) = 5255.0
          zt(78) = 5504.5
          zt(79) = 5765.5
          zt(80) = 6038.5

          ! Sketch of z-levels and indexing:
          ! Note that this indexing is shifted by
          ! one k-unit compared to the indexing
          ! in the MPIOM documentation.  There,
          ! zw(1) = 0.0 at the surface.
          !
          !----------------zw(0) = 0.0 = surface
          !
          !++++++++++++++++zt(1) = 6.0 (center of the zt(1) grid box)
          !
          !----------------zw(1) = 12.0 (bottom of the zt(1) grid box)
          !
          !++++++++++++++++zt(2) = 17.0
          !
          !----------------zw(2) = 22.0
          !
          !...

          ! Determine the depths of
          ! the w-levels
          allocate(zw(0:km))
          zw(0) = 0.0          
          do k = 1,km
             zw(k) = 2.0*zt(k) - zw(k-1)
          enddo

          ! Check that the reconstruction was
          ! correct: the rule is that the
          ! t-grid levels lie in the middle
          ! of the w-grid levels.
          do k=1,km
             if ( (zw(k)+zw(k-1))/2.0 .ne. zt(k) ) then
                write(*,*) 'ERROR: Inconsistent reconstruction of zw!'
                write(*,*) 'zw(k) = ',zw(k)
                write(*,*) 'zw(k-1)=',zw(k-1)
                write(*,*) 'zt(k) = ',zt(k)
                stop
             endif
          enddo

          ! Compute the spacing of the w-levels
          ! (which, dzw(k) is also the bottom
          ! of the k-th temperature box).
          allocate(dzw(1,1,km))
          do k = 1,km
             dzw(1,1,k) = zw(k) - zw(k-1)
          enddo

          ! Open data file
          infid = ncopn('storm_grid.nc', ncnowrit, exitcode)
          if (exitcode .eq. 0) write(*,*) 'Opened STORM grid file.'
#ifdef storm_cdo
          ! All grids are co-located. Uniform grids
          ! are referenced with 1D vectors, need to
          ! build 2D position mesh.
          readst1d(1) = 1
          readct1d(1) = imt
          allocate(hold1d(imt))
          vid = ncvid(infid,'lon',exitcode)
          call ncvgt(infid,vid,readst1d,readct1d,hold1d,exitcode)
          do j = 1,jmt
             do i = 1,imt
                if ( hold1d(i) .le. 180.0 ) then
                   ! Just copy degrees east
                   xt(i,j) = real(hold1d(i))
                else
                   ! Convert to degrees west
                   xt(i,j) = real(hold1d(i)) - 360.0
                endif
             enddo
          enddo
          deallocate(hold1d)

          readct1d(1) = jmt
          allocate(hold1d(jmt))
          vid = ncvid(infid,'lat',exitcode)
          call ncvgt(infid,vid,readst1d,readct1d,hold1d,exitcode)
          do j = 1,jmt
             do i = 1,imt
                yt(i,j) = real(hold1d(j))
             enddo
          enddo
          deallocate(hold1d)

          readst4d(1) = 1
          readst4d(2) = 1
          readst4d(3) = 1
          readst4d(4) = 1
          readct4d(1) = imt
          readct4d(2) = jmt
          readct4d(3) = 1
          readct4d(4) = 1
          vid = ncvid(infid,'DEPTO',exitcode)
          call ncvgt(infid,vid,readst4d,readct4d,depto,exitcode)
          if (exitcode .eq. 0) write(*,*) 'Got depth.'
#else
          ! Unrotated storm grids
          !===============T-grid==============
          ! Assign reading starts and counts
          readst2d(1) = 1
          readst2d(2) = 1
          readct2d(1) = imt
          readct2d(2) = jmt

          vid = ncvid(infid,'LON', exitcode)
          call ncvgt(infid, vid, readst2d, readct2d, xt, exitcode)
          if (exitcode .eq. 0) write(*,*) 'Got longitude.'

          vid = ncvid(infid,'LAT', exitcode)
          call ncvgt(infid, vid, readst2d, readct2d, yt, exitcode)
          if (exitcode .eq. 0) write(*,*) 'Got latitude.'

          readstart(1) = 1
          readstart(2) = 1
          readstart(3) = 1
          readstart(4) = 1
          readcount(1) = imt
          readcount(2) = jmt
          readcount(3) = 1
          readcount(4) = 1
          vid = ncvid(infid,'DEPTO', exitcode)
          call ncvgt(infid, vid, readstart, readcount, depto, exitcode)
          if (exitcode .eq. 0) write(*,*) 'Got depth.'

          readcount(3) = km
          vid = ncvid(infid,'DDPO',exitcode)
          call ncvgt(infid,vid,readstart,readcount,hold4d,exitcode)
          if(exitcode .eq. 0) write(*,*) 'Got thickness.'
#endif
          ! Close file
          call ncclos(infid, exitcode)
          if (exitcode .eq. 0) write(*,*) 'Closed grid file.'
#ifdef flipj
          write(*,*) 'Flipping grid...'
          call flip_j(xt,dg)
          call flip_j(yt,dg)
          call flip_j(depto(:,:,1,1),dg)
#ifdef storm
          do k = 1,km
             call flip_j(hold4d(:,:,k,1),dg)
          enddo
#endif
#endif
#ifdef storm
          write(*,*) 'Copy and check node thicknesses...'
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   if ( (hold4d(i,j,k,1) .gt. 0.0).and. &
                        (hold4d(i,j,k,1) .lt. 1000.0) ) then
                      dzt(i,j,k) = hold4d(i,j,k,1)
                   else
                      dzt(i,j,k) = 0.0
                   endif
                enddo
             enddo
          enddo
          deallocate(hold4d)
          write(*,*) 'Done loading STORM grid.'
#endif
#ifdef storm_cdo
          write(*,*) 'Done loading STORM_CDO grid.'
#endif
          return
        end subroutine load_storm_grid
!----------------------------------------------------------

!----------------------------------------------------------
        subroutine load_storm_cut_grid
!----------------------------------------------------------
! This subroutine will load the partial cell depths
! from the storm model grid based on the (i,j)
! cut domain.  This is similar to load_storm_cut_data().
!----------------------------------------------------------

          use params
          use netcdfio
          use grids

          implicit none

          integer :: vid
          integer :: dg(2)
          integer :: i,j,k
          real, allocatable :: hold4d(:,:,:,:)
          real(kind=8), allocatable :: hold1d(:)
          real, allocatable :: tmp(:,:)

          ! Make local lists of readcount and readstart
          ! The 4d equivalents were already declared.
          !integer :: readst2d(2)
          !integer :: readco2d(2)

          dg(1) = readcount(1)
          dg(2) = readcount(2)

          ! NOT ALL VARIABLES ARE RELOADED
          ! HERE.  ADD AS NEEDED.
          
          allocate(xt(readcount(1),readcount(2)))
          allocate(yt(readcount(1),readcount(2)))
          allocate(dzt(readcount(1),readcount(2),readcount(3)))
          allocate(depto(readcount(1),readcount(2),1,1))
          allocate(hold4d(readcount(1),readcount(2),readcount(3),1))
          !allocate(tmp(readcount(1),readcount(2)))
          depto = 0.0
          xt = 0.0
          yt = 0.0
          dzt = 0.0
          hold4d = 0.0

          infid = ncopn('storm_grid.nc', ncnowrit, exitcode)
          if (exitcode .eq. 0) write(*,*) 'Opened STORM grid file.'

#ifdef storm_cdo
          ! All grids are co-located. Uniform grids
          ! are referenced with 1D vectors, need to
          ! build 2D position mesh.
          readst1d(1) = readstart(1)
          readct1d(1) = readcount(1)
          allocate(hold1d(readcount(1)))
          vid = ncvid(infid,'lon',exitcode)
          call ncvgt(infid,vid,readst1d,readct1d,hold1d,exitcode)
          do j = 1,readcount(2)
             do i = 1,readcount(1)
                if ( hold1d(i) .le. 180.0 ) then
                   ! Copy degrees east
                   xt(i,j) = real(hold1d(i))
                else
                   ! Convert to degrees west
                   xt(i,j) = real(hold1d(i)) - 360.0
                endif
             enddo
          enddo
          !do i = 1,readcount(1)
          !   write(*,*) hold1d(i)
          !enddo
          deallocate(hold1d)

          readst1d(1) = readstart(2)
          readct1d(1) = readcount(2)
          allocate(hold1d(readcount(2)))
          vid = ncvid(infid,'lat',exitcode)
          call ncvgt(infid,vid,readst1d,readct1d,hold1d,exitcode)
          do j = 1,readcount(2)
             do i = 1,readcount(1)
                yt(i,j) = hold1d(j)
             enddo
          enddo
          !do j = 1,readcount(2)
          !   write(*,*) hold1d(j)
          !enddo
          deallocate(hold1d)

          readst4d(1) = readstart(1)
          readst4d(2) = readstart(2)
          readst4d(3) = 1
          readst4d(4) = 1
          readct4d(1) = readcount(1)
          readct4d(2) = readcount(2)
          readct4d(3) = 1
          readct4d(4) = 1
          vid = ncvid(infid,'DEPTO',exitcode)
          call ncvgt(infid,vid,readst4d,readct4d,depto,exitcode)
          if (exitcode .eq. 0) write(*,*) 'Got depth.'

          readct4d(3) = readcount(3)
          vid = ncvid(infid,'DDPO',exitcode)
          call ncvgt(infid,vid,readst4d,readct4d,hold4d,exitcode)
          if (exitcode .eq. 0) write(*,*) 'Got thickness.'
#else
          !===============T-grid==============
          ! i-index does not change because
          ! no flipping in i-direction.
          readst2d(1) = readstart(1)
#ifdef flipj
          ! Flipping grids - from flip_j, recall
          ! o = jm-j+1
          readst2d(2) = jmt - (readstart(2) + readcount(2) - 1) + 1
#else
          ! Unflipped grids - just start
          ! reading at desired location.
          readst2d(2) = readstart(2)
#endif
          ! Readcount does not change based
          ! on flipping because the size of
          ! the grid is the same.
          readct2d(1) = readcount(1)
          readct2d(2) = readcount(2)

          vid = ncvid(infid,'LON', exitcode)
          call ncvgt(infid,vid,readst2d,readct2d,xt,exitcode)
          if (exitcode .eq. 0) write(*,*) 'Got longitude.'
#ifdef flipj
          call flip_j(xt,dg)
#endif

          vid = ncvid(infid,'LAT', exitcode)
          call ncvgt(infid,vid,readst2d,readct2d,yt,exitcode)
          if (exitcode .eq. 0) write(*,*) 'Got latitude.'
#ifdef flipj
          call flip_j(yt,dg)
#endif
          readst4d(1) = readst2d(1)
          readst4d(2) = readst2d(2)
          readst4d(3) = 1
          readst4d(4) = 1
          readct4d(1) = readct2d(1)
          readct4d(2) = readct2d(2)
          readct4d(3) = 1
          readct4d(4) = 1
          vid = ncvid(infid,'DEPTO',exitcode)
          call ncvgt(infid,vid,readst4d,readct4d,depto,exitcode)
          if (exitcode .eq. 0) write(*,*) 'Got depth.'
#ifdef flipj
          call flip_j(depto(:,:,1,1),dg)
#endif

          readct4d(3) = readcount(3)
#ifdef flipj
          write(*,*) '-----------------------------------------------'
          write(*,*) 'In flipped domain, reading:'
          write(*,*) 'i: ',readst4d(1),' to ',readst4d(1) + readct4d(1) - 1,' stride ',readct4d(1)
          write(*,*) 'j: ',readst4d(2),' to ',readst4d(2) + readct4d(2) - 1,' stride ',readct4d(2)
          write(*,*) 'k: ',readst4d(3),' to ',readst4d(3) + readct4d(3) - 1,' stride ',readct4d(3)
          write(*,*) 'l: ',readst4d(4),' to ',readst4d(4) + readct4d(4) - 1,' stride ',readct4d(4)
          write(*,*) '-----------------------------------------------'
          write(*,*) 'In original domain, corresponds to:'
          write(*,*) 'i: ',readstart(1),' to ',readstart(1)+readcount(1)-1,' stride ',readcount(1)
          write(*,*) 'j: ',readstart(2),' to ',readstart(2)+readcount(2)-1,' stride ',readcount(2)
          write(*,*) 'k: ',readstart(3),' to ',readstart(3)+readcount(3)-1,' stride ',readcount(3)
          write(*,*) 'l: ',readstart(4),' to ',readstart(4)+readcount(4)-1,' stride ',readcount(4)
          write(*,*) '-----------------------------------------------'
#endif
          vid = ncvid(infid,'DDPO',exitcode)
          call ncvgt(infid,vid,readst4d,readct4d,hold4d,exitcode)
          if (exitcode .eq. 0) write(*,*) 'Got thicknesses.'
#ifdef flipj
          do k = 1,km
             call flip_j(hold4d(:,:,k,1),dg)
          enddo
#endif
#endif
          call ncclos(infid, exitcode)

          ! Copy and filter thicknesses
          do k = 1,readcount(3)
             do j = 1,readcount(2)
                 do i = 1,readcount(1)
                    if ( (hold4d(i,j,k,1).lt.0.0).or. &
                         (isnan(hold4d(i,j,k,1))).or. &
                         (hold4d(i,j,k,1).gt.1000.0) ) then
                       dzt(i,j,k) = 0.0
                    else
                       dzt(i,j,k) = hold4d(i,j,k,1)
                    endif
                 enddo
              enddo
           enddo
           deallocate(hold4d)
          return
        end subroutine load_storm_cut_grid
!----------------------------------------------------------
#endif
!----------------------------------------------------------
        subroutine find_ij(xpi,ypi,ipo,jpo,mode)
!----------------------------------------------------------
! This subroutine will input the coordinates of a single
! 2D point, (xpi,ypi).  The fractional indicial location
! of the given 2D point will be found and returned (ipo,
! jpo).   The mode specifies which grid (T, F, U, or V)
! we are searching.  This program is generalized to work
! well with a curvilinear grid (ORCA025 or STORM)
! so it CANNOT be implemented with a binary search
! because we are not tied to lines of constant
! i or j.
!----------------------------------------------------------

          use params
          use grids

          implicit none

          !======Variables that are passed======
          real, intent(in) :: xpi,ypi
          character(len=1), intent(in) :: mode
          real, intent(out) :: ipo,jpo

          !======Internal variables=======

          ! Mode flags
          logical :: lt,lf,lu,lv

          ! Temp holder for grids
          real :: xg(imt,jmt), yg(imt,jmt)

          ! Distance map
          real :: distance(imt,jmt)

          ! Location of min value
          integer :: ij(2)

          ! Conversion factor meters of
          ! arc length to degrees
          real :: m2deg

          ! Radius of the Earth [m]
          real :: eradius

          ! Pi = 3.14159...
          real :: pi

          ! Local counters
          integer :: i,j

          !=======Start=======
#ifdef super_verbose
          write(*,*) ' Starting find_ij...'
#endif
          lt = .false.
          lf = .false.
          lu = .false.
          lv = .false.

          eradius = 6370.e03
          pi = 4.0*atan(1.0)   ! atan(1.0) = pi/4
          m2deg = 180.0/(pi*eradius)

          ! Check that the input mode is correct
          if ( index(mode,'T') .ne. 0 ) then
#ifdef super_verbose
             write(*,*) 'We are searching the t-grid.'
#endif
             lt = .true.

             ! Load T grid to temp holders
             do j = 1,jmt
                do i = 1,imt
#if defined (mmflame) || defined (d3flame) || defined (storm) || defined (storm_cdo)
                   xg(i,j) = xt(i,j)
                   yg(i,j) = yt(i,j)
#endif
#if defined (orca050) || defined (orca025) || defined (viking20_cut) || defined (viking20_full)
                   xg(i,j) = real(xt(i,j))
                   yg(i,j) = real(yt(i,j))
#endif
                enddo
             enddo

          elseif ( index(mode,'F') .ne. 0 ) then
             ! We are searching the f grid
             lf = .true.

             ! Load F grid to temp holders
             do j = 1,jmt
                do i = 1,imt
#if defined (mmflame) || defined (d3flame) || defined (storm) || defined (storm_cdo)
                   xg(i,j) = xf(i,j)
                   yg(i,j) = yf(i,j)
#endif
#if defined (orca050) || defined (orca025)
                   xg(i,j) = real(xf(i,j))
                   yg(i,j) = real(yf(i,j))
#endif
                enddo
             enddo

          elseif ( index(mode,'U') .ne. 0 ) then
             ! We are searching the u grid
             lu = .true.

             ! Load U grid to temp holders
             do j = 1,jmt
                do i = 1,imt
#if defined (mmflame) || defined (d3flame) || defined (storm) || defined (storm_cdo)
                   xg(i,j) = xu(i,j)
                   yg(i,j) = yu(i,j)
#endif
#if defined (orca050) || defined (orca025)
                   xg(i,j) = real(xu(i,j))
                   yg(i,j) = real(yu(i,j))
#endif
                enddo
             enddo

          elseif ( index(mode,'V') .ne. 0 ) then
             ! We are searching the v grid
             lv = .true.

             ! Load V grid to temp holders
             do j = 1,jmt
                do i = 1,imt
#if defined (mmflame) || defined (d3flame) || defined (storm) || defined (storm_cdo)
                   xg(i,j) = xv(i,j)
                   yg(i,j) = yv(i,j)
#endif
#if defined (orca050) || defined (orca025)
                   xg(i,j) = real(xv(i,j))
                   yg(i,j) = real(yv(i,j))
#endif
                enddo
             enddo

          else
             write(*,*) 'ERROR in find_ij: Unexpected mode value: ',trim(mode)
             write(*,*) 'Must be one of T, F, U, or V.'
             stop
          endif          

          ! Compute a 2D distance map between each coord.
          ! in the grid and the given point.
#ifdef super_verbose
          write(*,*) ' Computing distance map...'
#endif
          do j = 1,jmt
             do i = 1,imt
                ! One could add a sqrt here, but not
                ! necessary, and it just adds compute
                ! time.
                distance(i,j) = (xpi - xg(i,j))**2.0 + &
                                (ypi - yg(i,j))**2.0
             enddo
          enddo

          ! Find the minimum value, and its indicial
          ! coordinates, in the distance map.
          ij = minloc(abs(distance))
          i = ij(1)
          j = ij(2)

          ! If the closest point in the grid lies is
          ! on the edge of the domain, check that the
          ! given point is within the domain.
          if ( (i .eq. 1) .and. (xpi .lt. xg(i,j)) ) then
             write(*,*) ' ERROR in find_ij: Point is beyond western edge.'
             stop
          elseif ( (i .eq. imt) .and. (xpi .gt. xg(i,j)) ) then
             write(*,*) ' ERROR in find_ij: Point is beyond eastern edge.'
             stop
#ifdef testing_unflipped
             ! STORM is different from ORCA and FLAME
             ! in that decreasing j means increasing
             ! latitude.  The velocities are still in
             ! the right direction (+ => northward)
             ! but the whole grid has been flipped.
          elseif ( (j .eq. 1) .and. (ypi .gt. yg(i,j)) ) then
             write(*,*) ' ERROR in find_ij: Point is beyond northern edge.'
             stop
          elseif ( (j .eq. jmt) .and. (ypi .lt. yg(i,j)) ) then
             write(*,*) ' ERROR in find_ij: Point is beyond southern edge.'
             stop
#else
             ! ORCA and FLAME output is "typical,"
             ! that is, increasing j generally means
             ! increasing latitude.
          elseif ( (j .eq. 1) .and. (ypi .lt. yg(i,j)) ) then
             write(*,*) ' ERROR in find_ij: Point is beyond southern edge.'
             stop
          elseif ( (j .eq. jmt) .and. (ypi .gt. yg(i,j)) ) then
             write(*,*) ' ERROR in find_ij: Point is beyond northern edge.'
             stop
#endif
          else
#ifdef super_verbose
             write(*,*) ' Point is within domain.'
#endif
          endif

          ! Test which quadrant the given point lies
          ! relative to the minimum point in the
          ! distance map.
          !
          !  2 | 1
          !    |
          ! ---x---
          !    | 
          !  3 | 4
          !
          ! Compute the additional fractional location
          ! of the point in the grid.  Note special cases
          ! due to STORM flip.

!=========================NEW CODE====================================
! This newer code should generate an identical result to the old
! code below.  However, things are grouped differently to make it
! more readable.
          if ( xpi .ge. xg(i,j) ) then
             ! We are in Q1 or Q4.
             ipo = i + (xpi - xg(i,j))/(xg(i+1,j)-xg(i,j))
          elseif ( xpi .le. xg(i,j) ) then
             ! We are in Q2 or Q3.
             ipo = i + (xpi - xg(i,j))/(xg(i,j)-xg(i-1,j))
          else
             write(*,*) 'ERROR: Cannot place point in x!'
             stop
          endif

          if ( ypi .ge. yg(i,j) ) then
             ! We are in Q1 or Q2
#ifdef testing_unflipped
             ! j must decrease (while FLAME and ORCA j increases).
             ! numerator is +
             ! denominator is + because decreasing j is increasing latitude.
             ! Also, the j-1 row is the row north of the j row.

             ! Was my best shot...
             !jpo = j - (ypi - yg(i,j))/(yg(i,j-1)-yg(i,j))

             ! Another try - see below - WORKING HERE
             !jpo = j + (ypi - yg(i,j))/(yg(i,j-1)-yg(i,j))

             ! Another try
             jpo = j - abs((ypi - yg(i,j))/(yg(i,j-1)-yg(i,j)))

             ! Explicitly test for j increase
             if ( jpo .le. j ) then
                ! We're ok
             else
                write(*,*) 'ERROR: J got larger in Q1 or Q2.'
             endif
#else
             ! j must increase: numerator is +,
             !                  denominator is +
             jpo = j + (ypi - yg(i,j))/(yg(i,j+1)-yg(i,j))
#endif
          elseif ( ypi .le. yg(i,j) ) then
#ifdef testing_unflipped
             ! j must increase (while in FLAME and ORCA j decreases).
             ! numerator is -
             ! denominator is + because increasing j is decreasing latitude.
             ! Also, the j+1 row is the row south of the j row.

             ! Was my best shot...
             !jpo = j - (ypi - yg(i,j))/(yg(i,j)-yg(i,j+1))

             ! Another try - see above - WORKING HERE
             !jpo = j + (ypi - yg(i,j))/(yg(i,j)-yg(i,j+1))

             ! Another try
             jpo = j + abs((ypi - yg(i,j))/(yg(i,j) - yg(i,j+1)))

             ! Explicitly test for j increase
             if ( jpo .ge. j ) then
                ! We're ok
             else
                write(*,*) 'ERROR: J got smaller in Q3 or Q4.'
             endif
#else
             ! j must decrease: numerator is -,
             !                  denominator is +.
             jpo = j + (ypi - yg(i,j))/(yg(i,j)-yg(i,j-1))
#endif
          else
             write(*,*) 'ERROR: Cannot place point in y!'
          endif

!=========================OLD CODE====================================
!          if ( (xpi .ge. xg(i,j)) .and. (ypi .ge. yg(i,j)) ) then
             ! We are in Q1
!             ipo = i + (xpi - xg(i,j))/(xg(i+1,j)-xg(i,j))
!#ifdef storm
             ! j must decrease (while FLAME and ORCA j increases).
             ! 
!             jpo = j - (ypi - yg(i,j))/(yg(i,j-1)-yg(i,j))
!#else
!             jpo = j + (ypi - yg(i,j))/(yg(i,j+1)-yg(i,j))
!#endif
!
!          elseif ( (xpi .le. xg(i,j)) .and. (ypi .ge. yg(i,j)) ) then
             ! We are in Q2
!             ipo = i + (xpi - xg(i,j))/(xg(i,j)-xg(i-1,j))
!#ifdef storm
!             jpo = j - (ypi - yg(i,j))/(yg(i,j-1)-yg(i,j))
!#else
!             jpo = j + (ypi - yg(i,j))/(yg(i,j+1)-yg(i,j))
!#endif
!          elseif ( (xpi .le. xg(i,j)) .and. (ypi .le. yg(i,j)) ) then
             ! We are in Q3
!             ipo = i + (xpi - xg(i,j))/(xg(i,j)-xg(i-1,j))
!#ifdef storm
!             jpo = j + (ypi - yg(i,j))/(yg(i,j)-yg(i,j+1))
!#else
!             jpo = j + (ypi - yg(i,j))/(yg(i,j)-yg(i,j-1))
!#endif
!          elseif ( (xpi .ge. xg(i,j)) .and. (ypi .le. yg(i,j)) ) then
             ! We are in Q4
!             ipo = i + (xpi - xg(i,j))/(xg(i+1,j)-xg(i,j))
!#ifdef storm
!             jpo = j + (ypi - yg(i,j))/(yg(i,j)-yg(i,j+1))
!#else
!             jpo = j + (ypi - yg(i,j))/(yg(i,j)-yg(i,j-1))
!#endif
!          else
!             write(*,*) ' ERROR in find_ij: Cannot place point in quadrant!'
!             stop
!          endif
#ifdef super_verbose
          ! Print output information to screen
          write(*,*) ' Input coordiantes: (',xpi,',',ypi,')'
          write(*,*) ' Closest indices: (',i,',',j,')'
          write(*,*) ' Value on grid: (',xg(i,j),',',yg(i,j),')'
          write(*,*) ' Fractional indices: (',ipo,',',jpo,')'
          write(*,*) ' ------------find_ij is done--------------'
#endif
          return
        end subroutine find_ij
!----------------------------------------------------------

!----------------------------------------------------------
        subroutine find_ij_on_t_quick(xpi,ypi,ipo,jpo)
!----------------------------------------------------------
! This subroutine will input the coordinates of a single
! 2D point, (xpi,ypi).  The fractional indicial location
! of the given 2D point will be found and returned (ipo,
! jpo).   The mode specifies which grid (T, F, U, or V)
! we are searching.  This program is generalized to work
! well with a curvilinear grid (ORCA025 or STORM)
! so it CANNOT be implemented with a binary search
! because we are not tied to lines of constant
! i or j.
!
! The _on_t_quick version outputs ipo and jpo as INTEGERS
! and does not compute the fractional values, rather
! only the closest point is found.  This makes the run
! time a bit quicker if all you want is to find out
! which grid node you are in.
!
! Also, the mode option is removed removing additional
! if checks for T,U,V grids.  This is specific to the T-grid.
!
! Also, ipo and jpo are set to 0 if not in the model domain
! (instead of just crashing) so the calling routine can deal
! with it.  Useful for trajectory processing for masked traj.
! points.  
!----------------------------------------------------------

          use params
          use grids

          implicit none

          !======Variables that are passed======
          real, intent(in) :: xpi,ypi
          integer, intent(out) :: ipo,jpo

          !======Internal variables=======

          ! Distance map
          real :: distance(imt,jmt)

          ! Local grid values
          real, allocatable :: xg(:,:)
          real, allocatable :: yg(:,:)

          ! Location of min value
          integer :: ij(2)

          ! Conversion factor meters of
          ! arc length to degrees
          real :: m2deg

          ! Radius of the Earth [m]
          real :: eradius

          ! Pi = 3.14159...
          real :: pi

          ! Local counters
          integer :: i,j

          !=======Start=======
#ifdef super_verbose
          write(*,*) ' Starting find_ij_on_t_quick...'
#endif
          eradius = 6370.e03
          pi = 4.0*atan(1.0)   ! atan(1.0) = pi/4
          m2deg = 180.0/(pi*eradius)

          allocate(xg(imt,jmt))
          allocate(yg(imt,jmt))
          do j = 1,jmt
             do i = 1,imt
#if defined (mmflame) || defined (d3flame) || defined (storm) || defined (storm_cdo)
                xg(i,j) = xt(i,j)
                yg(i,j) = yt(i,j)
#endif
#if defined (orca050) || defined (orca025) || defined (viking20_cut) || defined (viking20_full)
                xg(i,j) = real(xt(i,j))
                yg(i,j) = real(yt(i,j))
#endif
             enddo
          enddo

          ! Compute a 2D distance map between each coord.
          ! in the grid and the given point.
#ifdef super_verbose
          write(*,*) ' Computing distance map...'
#endif
          do j = 1,jmt
!$OMP PARALLEL SHARED(j) PRIVATE(i)
!$OMP DO
             do i = 1,imt
                ! One could add a sqrt here, but not
                ! necessary, and it just adds compute
                ! time.
                ! TESTING: changed **2 to abs
                distance(i,j) = abs(xpi - xg(i,j)) + &
                                abs(ypi - yg(i,j))
             enddo
!$OMP END DO
!$OMP END PARALLEL
          enddo

          ! Find the minimum value, and its indicial
          ! coordinates, in the distance map.
          ij = minloc(distance)
          ipo = ij(1)
          jpo = ij(2)

          ! If the closest point in the grid lies is
          ! on the edge of the domain, check that the
          ! given point is within the domain.
          if ( (ipo .eq. 1) .and. (xpi .lt. xg(i,j)) ) then
             ipo = 0
             jpo = 0
          elseif ( (ipo .eq. imt) .and. (xpi .gt. xg(i,j)) ) then
             ipo = 0
             jpo = 0
#ifdef testing_unflipped
             ! STORM is different from ORCA and FLAME
             ! in that decreasing j means increasing
             ! latitude.  The velocities are still in
             ! the right direction (+ => northward)
             ! but the whole grid has been flipped.
          elseif ( (jpo .eq. 1) .and. (ypi .gt. yg(i,j)) ) then
             ipo = 0
             jpo = 0
          elseif ( (jpo .eq. jmt) .and. (ypi .lt. yg(i,j)) ) then
             ipo = 0
             jpo = 0
#else
             ! ORCA and FLAME output is "typical,"
             ! that is, increasing j generally means
             ! increasing latitude.
          elseif ( (jpo .eq. 1) .and. (ypi .lt. yg(i,j)) ) then
             ipo = 0
             jpo = 0
          elseif ( (jpo .eq. jmt) .and. (ypi .gt. yg(i,j)) ) then
             ipo = 0
             jpo = 0
#endif
          else
#ifdef super_verbose
             write(*,*) ' Point is within domain.'
#endif
          endif

#ifdef super_verbose
          ! Print output information to screen
          write(*,*) ' Input coordiantes: (',xpi,',',ypi,')'
          write(*,*) ' Closest indices: (',ipo,',',jpo,')'
          write(*,*) ' Value on grid: (',xg(i,j),',',yg(i,j),')'
          write(*,*) ' ------------find_ij_on_t_quick is done--------------'
#endif
          return
        end subroutine find_ij_on_t_quick
!----------------------------------------------------------

!----------------------------------------------------------
        subroutine find_ijw(xpi,ypi,ipo,jpo,wpo,mode)
!----------------------------------------------------------
! This subroutine will input the coordinates of a single
! 2D point, (xpi,ypi).  The fractional indicial location
! of the given 2D point will be found and returned (ipo,
! jpo).   The mode specifies which grid (T, F, U, or V)
! we are searching.  This program is generalized to work
! well with a curvilinear grid (ORCA025 or STORM)
! so it CANNOT be implemented with a binary search
! because we are not tied to lines of constant
! i or j.
!
! This subroutine is different from find_ij because
! the relative contribution of each point (the weights
! used in interpolation) are also computed.
!----------------------------------------------------------

          use params
          use grids

          implicit none

          !======Variables that are passed======
          real, intent(in) :: xpi,ypi
          character(len=1), intent(in) :: mode
          real, intent(out) :: ipo,jpo
          real, intent(out) :: wpo(2,2)

          !======Internal variables=======

          ! Mode flags
          logical :: lt,lf,lu,lv

          ! Temp holder for grids
          real :: xg(imt,jmt), yg(imt,jmt)

          ! Distance map
          real :: distance(imt,jmt)

          ! Location of min value
          integer :: ij(2)

          ! Conversion factor meters of
          ! arc length to degrees
          real :: m2deg

          ! Radius of the Earth [m]
          real :: eradius

          ! Pi = 3.14159...
          real :: pi

          ! Local counters
          integer :: i,j
          real :: td,tw

          !=======Start=======
#ifdef super_verbose
          write(*,*) ' Starting find_ijw...'
#endif
          lt = .false.
          lf = .false.
          lu = .false.
          lv = .false.

          eradius = 6370.e03
          pi = 4.0*atan(1.0)   ! atan(1.0) = pi/4
          m2deg = 180.0/(pi*eradius)

          ! Check that the input mode is correct
          if ( index(mode,'T') .ne. 0 ) then
#ifdef super_verbose
             write(*,*) 'We are searching the t-grid.'
#endif
             lt = .true.

             ! Load T grid to temp holders
             do j = 1,jmt
                do i = 1,imt
#if defined (mmflame) || defined (d3flame) || defined (storm) || defined (storm_cdo)
                   xg(i,j) = xt(i,j)
                   yg(i,j) = yt(i,j)
#endif
#if defined (orca050) || defined (orca025)
                   xg(i,j) = real(xt(i,j))
                   yg(i,j) = real(yt(i,j))
#endif
                enddo
             enddo
          elseif ( index(mode,'F') .ne. 0 ) then
             ! We are searching the f grid
             lf = .true.

             ! Load F grid to temp holders
             do j = 1,jmt
                do i = 1,imt
#if defined (mmflame) || defined (d3flame) || defined (storm) || defined (storm_cdo)
                   xg(i,j) = xf(i,j)
                   yg(i,j) = yf(i,j)
#endif
#if defined (orca050) || defined (orca025)
                   xg(i,j) = real(xf(i,j))
                   yg(i,j) = real(yf(i,j))
#endif
                enddo
             enddo
          elseif ( index(mode,'U') .ne. 0 ) then
             ! We are searching the u grid
             lu = .true.

             ! Load U grid to temp holders
             do j = 1,jmt
                do i = 1,imt
#if defined (mmflame) || defined (d3flame) || defined (storm) || defined (storm_cdo)
                   xg(i,j) = xu(i,j)
                   yg(i,j) = yu(i,j)
#endif
#if defined (orca050) || defined (orca025)
                   xg(i,j) = real(xu(i,j))
                   yg(i,j) = real(yu(i,j))
#endif
                enddo
             enddo
          elseif ( index(mode,'V') .ne. 0 ) then
             ! We are searching the v grid
             lv = .true.

             ! Load V grid to temp holders
             do j = 1,jmt
                do i = 1,imt
#if defined (mmflame) || defined (d3flame) || defined (storm) || defined (storm_cdo)
                   xg(i,j) = xv(i,j)
                   yg(i,j) = yv(i,j)
#endif
#if defined (orca050) || defined (orca025)
                   xg(i,j) = real(xv(i,j))
                   yg(i,j) = real(yv(i,j))
#endif
                enddo
             enddo
          else
             write(*,*) 'ERROR in find_ij: Unexpected mode value: ',trim(mode)
             write(*,*) 'Must be one of T, F, U, or V.'
             stop
          endif          

          ! Compute a 2D distance map between each coord.
          ! in the grid and the given point.
#ifdef super_verbose
          write(*,*) ' Computing distance map...'
#endif
          do j = 1,jmt
             do i = 1,imt
                ! No square root needed here.  Later...
                distance(i,j) = (xpi - xg(i,j))**2.0 + &
                                (ypi - yg(i,j))**2.0
             enddo
          enddo

          ! Find the minimum value, and its indicial
          ! coordinates, in the distance map.
          ij = minloc(abs(distance))
          i = ij(1)
          j = ij(2)

          ! If the closest point in the grid lies is
          ! on the edge of the domain, check that the
          ! given point is within the domain.
          if ( (i .eq. 1) .and. (xpi .lt. xg(i,j)) ) then
             write(*,*) ' ERROR in find_ijw: Point is beyond western edge.'
             write(*,*) ' xpi = ',xpi
             write(*,*) ' xg(i,j) = ',xg(i,j)
             stop
          elseif ( (i .eq. imt) .and. (xpi .gt. xg(i,j)) ) then
             write(*,*) ' ERROR in find_ijw: Point is beyond eastern edge.'
             stop
          elseif ( (j .eq. 1) .and. (ypi .lt. yg(i,j)) ) then
             write(*,*) ' ERROR in find_ijw: Point is beyond southern edge.'
             stop
          elseif ( (j .eq. jmt) .and. (ypi .gt. yg(i,j)) ) then
             write(*,*) ' ERROR in find_ijw: Point is beyond northern edge.'
             stop
          else
#ifdef super_verbose
             write(*,*) ' Point is within domain.'
#endif
          endif

          ! Test which quadrant the given point lies
          ! relative to the minimum point in the
          ! distance map.
          !
          !  2 | 1
          !    |
          ! ---x---
          !    | 
          !  3 | 4
          !
          ! Compute the additional fractional location
          ! of the point in the grid.
          if ( (xpi .ge. xg(i,j)) .and. &
               (ypi .ge. yg(i,j)) ) then
             ! We are in Q1
             ipo = i + (xpi - xg(i,j))/(xg(i+1,j  )-xg(i  ,j  ))
             jpo = j + (ypi - yg(i,j))/(yg(i  ,j+1)-yg(i  ,j  ))

             wpo(1,1) = sqrt(distance(i  ,j  ))
             wpo(2,1) = sqrt(distance(i+1,j  ))
             wpo(1,2) = sqrt(distance(i  ,j+1))
             wpo(2,2) = sqrt(distance(i+1,j+1))
             
             !write(*,*) wpo(1,1),wpo(2,1),wpo(1,2),wpo(2,2)

          elseif ( (xpi .lt. xg(i,j)) .and. &
                   (ypi .ge. yg(i,j)) ) then
             ! We are in Q2.
             ipo = i + (xpi - xg(i,j))/(xg(i  ,j  )-xg(i-1,j  ))
             jpo = j + (ypi - yg(i,j))/(yg(i  ,j+1)-yg(i  ,j  ))

             wpo(1,1) = sqrt(distance(i-1,j  ))
             wpo(2,1) = sqrt(distance(i  ,j  ))
             wpo(1,2) = sqrt(distance(i-1,j+1))
             wpo(2,2) = sqrt(distance(i  ,j+1))

             !write(*,*) wpo(1,1),wpo(2,1),wpo(1,2),wpo(2,2)

          elseif ( (xpi .lt. xg(i,j)) .and. &
                   (ypi .lt. yg(i,j)) ) then
             ! We are in Q3
             ipo = i + (xpi - xg(i,j))/(xg(i  ,j  )-xg(i-1,j  ))
             jpo = j + (ypi - yg(i,j))/(yg(i  ,j  )-yg(i  ,j-1))

             wpo(1,1) = sqrt(distance(i-1,j-1))
             wpo(2,1) = sqrt(distance(i  ,j-1))
             wpo(1,2) = sqrt(distance(i-1,j  ))
             wpo(2,2) = sqrt(distance(i  ,j  ))

             !write(*,*) wpo(1,1),wpo(2,1),wpo(1,2),wpo(2,2)

          elseif ( (xpi .ge. xg(i,j)) .and. &
                   (ypi .lt. yg(i,j)) ) then
             ! We are in Q4
             ipo = i + (xpi - xg(i,j))/(xg(i+1,j  )-xg(i  ,j  ))
             jpo = j + (ypi - yg(i,j))/(yg(i  ,j  )-yg(i  ,j-1))

             wpo(1,1) = sqrt(distance(i  ,j-1))
             wpo(2,1) = sqrt(distance(i+1,j-1))
             wpo(1,2) = sqrt(distance(i  ,j  ))
             wpo(2,2) = sqrt(distance(i+1,j  ))

             !write(*,*) wpo(1,1),wpo(2,1),wpo(1,2),wpo(2,2)

          else
             write(*,*) 'ERROR: Cannot place point!'
             stop
          endif

          ! Normalize distance to get nondim weights
          ! Total distance is in numerator, so for
          ! small distances, the weight is large and
          ! for large distances the weight is small.
          td = wpo(1,1) + wpo(1,2) + wpo(2,1) + wpo(2,2)
          !write(*,*) td
          if ( wpo(1,1) .eq. 0.0 ) then
             wpo = 0.0
             wpo(1,1) = 1.0
          elseif ( wpo(1,2) .eq. 0.0 ) then
             wpo = 0.0
             wpo(1,2) = 1.0
          elseif ( wpo(2,1) .eq. 0.0 ) then
             wpo = 0.0
             wpo(2,1) = 1.0
          elseif ( wpo(2,2) .eq. 0.0 ) then
             wpo = 0.0
             wpo(2,2) = 1.0
          else
             wpo(1,1) = td/wpo(1,1)
             wpo(1,2) = td/wpo(1,2)
             wpo(2,1) = td/wpo(2,1)
             wpo(2,2) = td/wpo(2,2)
          endif

          ! Finally, normalize all the weights
          ! so they fit from zero to one.
          tw = wpo(1,1) + wpo(1,2) + wpo(2,1) + wpo(2,2)
          wpo(1,1) = wpo(1,1)/tw
          wpo(1,2) = wpo(1,2)/tw
          wpo(2,1) = wpo(2,1)/tw
          wpo(2,2) = wpo(2,2)/tw

          !tw = wpo(1,1) + wpo(1,2) + wpo(2,1) + wpo(2,2)
          !if (wpo(1,1)+wpo(1,2)+wpo(2,1)+wpo(2,2).ne.1.0) then
          !   write(*,*) 'ERROR: Sum of weights not 1.0.'
          !   write(*,*) tw
          !endif
#ifdef super_verbose
          ! Print output information to screen
          write(*,*) ' Input coordiantes: (',xpi,',',ypi,')'
          write(*,*) ' Closest indices: (',i,',',j,')'
          write(*,*) ' Value on grid: (',xg(i,j),',',yg(i,j),')'
          write(*,*) ' Fractional indices: (',ipo,',',jpo,')'
          write(*,*) ' Weights: ',wpo(1,1),wpo(1,2),wpo(2,1),wpo(2,2)
          write(*,*) ' ------------find_ijw is done--------------'
#endif
          return
        end subroutine find_ijw
!----------------------------------------------------------

!----------------------------------------------------------
        subroutine load_wind_stress(filenumber)
!----------------------------------------------------------
! This subroutine will load wind stresses from FLAME, ORCA,
! and STORM models.  WORKS FOR STORM_CDO BUT NOT FOR THE
! ORIGINAL STORM GRIDS.
!----------------------------------------------------------

          use params
          use netcdfio
          use infields
          use grids
      
          implicit none

          ! Local counters
          integer :: filenumber, i, j
#if defined (d3flame)
          character(len=12) :: taufnam
#else
          character(len=16) :: taufnam
#endif
          integer :: tau_fid
          integer :: tauxvid, tauyvid
#if defined (mmflame) || defined (d3flame)
          !character(len=4) :: tauxnam = 'TAUX'
          !character(len=4) :: tauynam = 'TAUY'
          character(len=4) :: tauxnam = 'taux'
          character(len=4) :: tauynam = 'tauy'
#endif
#if defined (orca050) || defined (orca025) || defined (orca025_global) || defined (viking20_cut) || defined (viking20_full)
          character(len=8) :: tauxnam = 'sozotaux'
          character(len=8) :: tauynam = 'sometauy'
#endif
#if defined (storm) || defined (storm_cdo)
          character(len=3) :: tauxnam = 'txo'
          character(len=3) :: tauynam = 'tye'
#endif
          ! Load wind stress data using same
          ! grid node (i,j) as when loading uvts.
#if defined (mmflame) || defined (d3flame) || defined (orca025)
          readst3d(1) = readstart(1)
          readst3d(2) = readstart(2)
          readst3d(3) = 1
          readct3d(1) = readcount(1)
          readct3d(2) = readcount(2)
          readct3d(3) = 1
#endif
#if defined (storm_cdo)
          readst4d(1) = readstart(1)
          readst4d(2) = readstart(2)
          readst4d(3) = 1
          readst4d(4) = filenumber - 10000
          
          readct4d(1) = readcount(1)
          readct4d(2) = readcount(2)
          readct4d(3) = 1
          readct4d(4) = 1
#endif
          !---------------TAUX----------------
#if defined (mmflame)
          write(taufnam,'(a6,i5,a5)') 'infile',filenumber,'F.cdf'
#endif
#ifdef d3flame
          write(taufnam,'(a3,i5,a4)') 'mm_',filenumber,'.cdf'
#endif
#if defined (orca050) || defined (orca025) || defined (orca025_global) || defined (viking20_cut) || defined (viking20_full)
          write(taufnam,'(a6,i5,a5)') 'infile',filenumber,'U.cdf'
#endif
#if defined (storm_cdo)
          write(taufnam,'(a6,i5,a5)') 'infile',10001,'F.cdf'
#endif
          tau_fid = ncopn(taufnam,ncnowrit,exitcode)
          tauxvid = ncvid(tau_fid,tauxnam,exitcode)
#if defined (storm_cdo)
          call ncvgt(tau_fid,tauxvid,readst4d,readct4d,tau_xi,exitcode)
#else
          call ncvgt(tau_fid,tauxvid,readst3d,readct3d,tau_xi,exitcode)
#endif
          call ncclos(tau_fid,exitcode)

          !---------------TAUY----------------
#if defined (orca050) || defined (orca025) || defined (orca025_global) || defined (viking20_cut) || defined (viking20_full)
          write(taufnam,'(a6,i5,a5)') 'infile',filenumber,'V.cdf'
#endif
          tau_fid = ncopn(taufnam,ncnowrit,exitcode)
          tauyvid = ncvid(tau_fid,tauynam,exitcode)
#if defined (storm_cdo)
          call ncvgt(tau_fid,tauyvid,readst4d,readct4d,tau_yi,exitcode)
#else
          call ncvgt(tau_fid,tauyvid,readst3d,readct3d,tau_yi,exitcode)
#endif
          call ncclos(tau_fid, exitcode)

          !------------Sanity Check-----------
          do j = 1,readcount(2)
             do i = 1,readcount(1)
                if ( (tau_xi(i,j,1) .lt. tau_high) .and. &
                     (tau_xi(i,j,1) .gt. tau_low) .and. &
#if defined (mmflame) || defined (d3flame)
                     (kmf(i,j) .gt. 0) ) then
#endif
#if defined (orca025) || defined (orca050) || defined (orca025_global) || defined (viking20_cut) || defined (viking20_full)
                     (umask(i,j,1) .eq. 1) ) then
#endif
#if defined (storm) || defined (storm_cdo)
                     (depto(i,j,1,1) .gt. 0) ) then
#endif
                else
                   tau_xi(i,j,1) = fill_real
                endif
                
                if ( (tau_yi(i,j,1) .lt. tau_high) .and. &
                     (tau_yi(i,j,1) .gt. tau_low)  .and. &
#if defined (mmflame) || defined (d3flame)
                     (kmf(i,j) .gt. 0) ) then
#endif
#if defined (orca025) || defined (orca050) || defined (orca025_global) || defined (viking20_cut) || defined (viking20_full)
                     (vmask(i,j,1) .eq. 1) ) then
#endif
#if defined (storm) || defined (storm_cdo)
                     (depto(i,j,1,1) .gt. 0) ) then
#endif
                   ! Value is OK, do nothing.
                else
                   tau_yi(i,j,1) = fill_real
                endif
             enddo
          enddo
          return
        end subroutine load_wind_stress
!----------------------------------------------------------
        
#if defined (mmflame) || defined (d3flame)
!----------------------------------------------------------

        subroutine load_flame_data(filenumber)

          use params
          use netcdfio
          use infields
          use grids

          implicit none

          ! Local counters
          integer :: filenumber, i, j, k

          ! Intermediate values used in interpolation
          real :: left, right, top, bot

          !--------------------------------------

          ! Initialize (wipe out) infile fields
          ui = 0.0
          vi = 0.0
          ti = 0.0
          si = 0.0
          wi = 0.0
          
          ! Note that there is an "avgin" compiler option
          ! which takes into account the difference file
          ! structure if we have a 3D input file that was
          ! created by eddycdf_3d (tavgvar, uavgvar).
#ifdef avgin
          ! >>>>> This is for 3d average files <<<<<
          ! No provision is made for GM90 since 3d avg
          ! files are likely to be computed only for
          ! ORCA025, which does not have GM90.

          !----Zonal, meridional, and vertical velocities----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'U',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          uvid = ncvid(infid, uvnam, exitcode)
          vvid = ncvid(infid, vvnam, exitcode)
          wvid = ncvid(infid, wvnam, exitcode)
          call ncvgt(infid, uvid, readstart, readcount, ui, exitcode)
          call ncvgt(infid, vvid, readstart, readcount, vi, exitcode)
#if defined (uavgvar) || defined (qeddy) || defined (zeddy)
          call ncvgt(infid, wvid, readstart, readcount, wi, exitcode)
#endif
          call ncclos(infid,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
#if defined (tavgvar) || defined (qeddy)
          !----Temperature and salinity----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          tvid = ncvid(infid, tvnam, exitcode)
          svid = ncvid(infid, svnam, exitcode)
          call ncvgt(infid, tvid, readstart, readcount, ti, exitcode)
          call ncvgt(infid, svid, readstart, readcount, si, exitcode)
          call ncclos(infid,exitcode)
#endif
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
#else
          ! >>>>>> STANDARD VERSION OF load_flame_data <<<<<<
          ! Determine the infile name.  Add "f" because
          ! FLAME input file.
          write(infname,'(a,i5,a,a)')infroot,filenumber,'F',infext
          write(6,'(a,a)')' Opening file: ',infname
          
          ! Open the file n
          infid = ncopn(infname, ncnowrit, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) '   No errors opening file.'
#endif
          ! Get the variable ids from the infile:
          uvid = ncvid(infid, uvnam, exitcode)
          vvid = ncvid(infid, vvnam, exitcode)
          tvid = ncvid(infid, tvnam, exitcode)
          svid = ncvid(infid, svnam, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) '   Got variable IDs.'
#endif
          ! Read the source values from the infile
          call ncvgt(infid, uvid, readstart, readcount, ui, exitcode)
          call ncvgt(infid, vvid, readstart, readcount, vi, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) '   Done reading (u,v).'
#endif
          ! Convert the source file values to correct units, velocity
          ! in d3flame files is in cm/s -> change to m/s to be consistent
          ! with mmflame and all ORCA files.  In the process, mask out
          ! dry velocity boxes with fill_values based on kmf and the
          ! max and min acceptable values for the velocities.
#ifdef verbose
          write(*,*) '   Converting (u,v)...'
#endif
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   if(ui(i,j,k,1) .eq. fill_real) then
                      ! Do nothing here - keep fill
                      ! If the value is NaN, test results in F
                   else
                      ! Convert cm/s -> m/s and filter
#ifdef d3flame
                      ui(i,j,k,1) = ui(i,j,k,1)/100.0
#endif
                      
                      if( (kmf(i,j).ge.k).and. &
                           (ui(i,j,k,1).gt.ulow).and. &
                           (ui(i,j,k,1).lt.uhigh) ) then
                         ! Do nothing - keep the value
                         ! If the value is NaN, test is false
                      else
                         ui(i,j,k,1) = fill_real
                      endif
                   endif
                   
                   if(vi(i,j,k,1) .eq. fill_real) then
                      ! Do nothing here - keep fill
                      ! If the value is NaN, test results in F
                   else
                      ! Convert cm/s -> m/s and filter
#ifdef d3flame
                      vi(i,j,k,1) = vi(i,j,k,1)/100.0
#endif
                      if( (kmf(i,j).ge.k).and. &
                           (vi(i,j,k,1).gt.vlow).and. &
                           (vi(i,j,k,1).lt.vhigh) ) then
                         ! Do nothing - keep the value
                         ! If the value is NaN, test is false
                      else
                         vi(i,j,k,1) = fill_real
                      endif
                   endif
                   
                enddo
             enddo
          enddo
#if defined (uavgvar) || defined (qeddy) || defined (zeddy)
          !----Compute Vertical Velocity directly onto t-grid----
#ifdef verbose
          write(*,*) '   Computing vertical velocity...'
#endif
          ! (1) Horizontal divergence:
          ! For each point in the water column and each column
          do k = 1,km
             do j = 2,jmt
                do i = 2,imt
                   !----Compute the vertical velocity----
                   ! Integrate divergence:
                   ! dw/dz = -du/dx - dv/dy
                   ! and so:
                   ! w = w0 - dz(du/dx + dv/dy)
                   !
                   ! w is computed on the t-grid because we are
                   ! using (i,j) and all the points (subtracting 1,
                   ! not adding).  The depth of each w is computed
                   ! at the depths of w-grid values.
                   !
                   ! Find the net horizontal flux though the half
                   ! the grid box that surrounds the w/t point
                   ! (hence the factor of 1/2 with each flux).
                   !
                   !          ^           ^
                   !          |           |
                   !          -------------
                   !                             
                   !      (i-1,j)       (i,j)     
                   !   |      X            X      | =>
                   !   |                          |
                   !   |                          |
                   !   |             o            |
                   !   |                          |
                   !   |                          |
                   !   |       X            X     | =>
                   !        (i-1,j-1)     (i,j-1) 
                   !                             
                   !           --------------
                   !
                   ! Then, divide the net input by the area
                   ! of the surrounded t/w grid box.
                   
                   ! Check for fill values in the velocity field
                   if(ui(i,j,k,1) .eq. fill_real .or. &
                        ui(i,j-1,k,1) .eq. fill_real .or. &
                        ui(i-1,j,k,1) .eq. fill_real .or. &
                        ui(i-1,j-1,k,1) .eq. fill_real .or. &
                        vi(i,j,k,1) .eq. fill_real .or. &
                        vi(i,j-1,k,1) .eq. fill_real .or. &
                        vi(i-1,j,k,1) .eq. fill_real .or. &
                        vi(i-1,j-1,k,1) .eq. fill_real) then
                      wi(i,j,k,1) = fill_real
                   else
                      wi(i,j,k,1) =((ui(i,j,k,1)*dyu(i,j) +  &
                           ui(i,j-1,k,1)*dyu(i,j-1) -  &
                           ui(i-1,j,k,1)*dyu(i-1,j) -  &
                           ui(i-1,j-1,k,1)*dyu(i-1,j-1) +  &
                           vi(i,j,k,1)*dxu(i,j) +  &
                           vi(i-1,j,k,1)*dxu(i-1,j) -  &
                           vi(i,j-1,k,1)*dxu(i,j-1) -  &
                           vi(i-1,j-1,k,1)*dxu(i-1,j-1)) &
                           /(2.0*dxt(i,j)*dyt(i,j)))*dzt(k)
                   endif
                enddo
             enddo
          enddo
          
          ! Fill in the blank values
          do k = 1,km
             do j = 1,jmt
                wi(1,j,k,1) = fill_real
             enddo
             do i = 1,imt
                wi(i,1,k,1) = fill_real
             enddo
          enddo
#ifdef verbose
          write(*,*) '     Finished computing horizontal divergence.'
#endif
          ! (2) Vertical integration:
          ! Sum up each point in the water column at each (i,j).
          ! Note that wi(:,:,0,1) was initialized to 0.0 and is
          ! never reassigned afterward.
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   if (k.lt.kmt(i,j)) then
                      ! We are above the ocean floor, so integrate
                      ! the divergence by summing up the velocities
                      if(wi(i,j,k,1) .eq. fill_real) then
                         ! Do not include this point in sum
                      else
                         wi(i,j,k,1) =  wi(i,j,k,1) + wi(i,j,k-1,1)
                      endif
                   else
                      ! We are on the ocean floor, fill values
                      wi(i,j,k,1) = fill_real
                   endif
                enddo
             enddo
          enddo
#ifdef verbose
          write(*,*) '     Finished vertical integration.'
#endif
#endif
! End of calctype check (uavgvar, qeddy, zeddy)
          
#if defined (tavgvar) || defined (qeddy)
          ! Read in salinity and temperature
          call ncvgt(infid, tvid, readstart, readcount, ti, exitcode)
          call ncvgt(infid, svid, readstart, readcount, si, exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) '   Read (t,s).'
          write(*,*) '   Filtering the input data...'
#endif
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   if( (ti(i,j,k,1).gt.tlow).and. &
                        (ti(i,j,k,1).lt.thigh).and. &
                        (kmt(i,j).ge.k) ) then
                      ! Do nothing, the value is good
                   else
                      ti(i,j,k,1) = fill_real
                   endif
                   
                   if( (si(i,j,k,1).gt.slow).and. &
                        (si(i,j,k,1).lt.shigh).and. &
                        (kmt(i,j).ge.k) ) then
                      ! Do nothing, the value is good
                   else
                      si(i,j,k,1) = fill_real
                   endif
                enddo
             enddo
          enddo
#endif
! End of calctype check (tavgvar, qeddy)
          ! Close the input file
          call ncclos(infid, exitcode)
#ifdef vebose
          if(exitcode .eq. 0) write(*,*) 'Input file closed.'      
#endif
#endif
!----End of avgin loop block for standard load_flame_data-----
          
          ! Operations on 2D data: SSH/Lid pressure
          hi = fill_real

#ifdef verbose
          write(*,*) ' Done loading snapshot.  Exiting load_flame_data.'
#endif
          return
        end subroutine load_flame_data
!----------------------------------------------------------

!----------------------------------------------------------

      subroutine load_flame_cut_data(filenumber)

      use params
      use netcdfio
      use infields
      use grids

      implicit none

      ! Local counters
      integer :: filenumber, i, j, k

      ! Intermediate values used in interpolation
      real :: left, right, top, bot

!--------------------------------------

      ! Determine the infile name.
      write(infname,'(a,i5,a,a)')infroot,filenumber,'F',infext
#ifdef verbose
      write(6,'(a,a)')' Opening file: ',infname
#endif
      ! Open the file
      infid = ncopn(infname,ncnowrit,exitcode)

#ifdef verbose
      if(exitcode .eq. 0) write(*,*) '   Getting variable IDs...'
#endif
      uvid = ncvid(infid,uvnam,exitcode)
      vvid = ncvid(infid,vvnam,exitcode)
      tvid = ncvid(infid,tvnam,exitcode)
      svid = ncvid(infid,svnam,exitcode)

      ! Read the source values from the infile
#ifdef verbose
      write(*,*) '   Reading (s,t)...'
#endif
      ! Read in salinity and temperature
      call ncvgt(infid,svid,readstart,readcount,si,exitcode)
      call ncvgt(infid,tvid,readstart,readcount,ti,exitcode)
      
#ifdef verbose
      write(*,*) '   Filtering (s,t)...'
#endif
      do k = 1,readcount(3)
         do j = 1,readcount(2)
            do i = 1,readcount(1)
               if((ti(i,j,k,1).gt.tlow).and. &
                  (ti(i,j,k,1).lt.thigh).and. &
                  (kmt(i,j).ge.k)) then
                  ! WORKING HERE
                  ! ACK ACK ACK ACK!
                  ! CLUDGE FOR OFCDF BECAUSE HAD NOT YET WRITTEN
                  ! CUT_FLAME_GRID
                  !(kmt((readstart(1)+i-1),(readstart(2)+j-1)).ge.k)) then
                  ! Do nothing, the value is good
               else
                  ti(i,j,k,1) = fill_real
               endif

               if((si(i,j,k,1).gt.slow).and. &
                  (si(i,j,k,1).lt.shigh).and. &
                  (kmt(i,j).ge.k)) then
                  ! WORKING HERE
                  ! ACK ACK ACK ACK!
                  ! CLUDGE
                  !(kmt((readstart(1)+i-1),(readstart(2)+j-1)).ge.k)) then
                  ! Do nothing, the value is good
               else
                  si(i,j,k,1) = fill_real
               endif
            enddo
         enddo
      enddo

#ifdef verbose
      if(exitcode .eq. 0) write(*,*) '   Reading (u,v)...'
#endif
      call ncvgt(infid,uvid,readstart,readcount,ui,exitcode)
      call ncvgt(infid,vvid,readstart,readcount,vi,exitcode)

      ! Convert the source file values to correct units, velocity
      ! in d3flame files is in cm/s -> change to m/s to be consistent
      ! with mmflame and all ORCA files.
#ifdef d3flame
#ifdef verbose
      write(*,*) '   Converting (u,v)...'
#endif
      ui = ui/100.0
      vi = vi/100.0
#endif
#ifdef verbose
      write(*,*) '   Filtering (u,v)...'
#endif
      do k = 1,readcount(3)
         do j = 1,readcount(2)
            do i = 1,readcount(1)
               if((ui(i,j,k,1).gt.ulow).and. &
                  (ui(i,j,k,1).lt.uhigh).and. &
                  (kmf(i,j).ge.k)) then
                  ! WORKING HERE
                  ! ACK ACK ACK ACK!
                  ! CLUDGE
                  !(kmf((readstart(1)+i-1),(readstart(2)+j-1)).ge.k)) then
                  ! Do nothing - keep the value
                  ! If the value is NaN, test is false
               else
                  ui(i,j,k,1) = 0.0
               endif

               if((vi(i,j,k,1).gt.vlow).and. &
                  (vi(i,j,k,1).lt.vhigh).and. &
                  (kmf(i,j).ge.k))  then
                  ! WORKING HERE
                  ! ACK ACK ACK ACK!
                  ! CLUDGE
                  !(kmf((readstart(1)+i-1),(readstart(2)+j-1)).ge.k)) then
                  ! Do nothing - keep the value
                  ! If the value is NaN, test is false
               else
                  vi(i,j,k,1) = 0.0
               endif

            enddo
         enddo
      enddo

      ! Close the input file
      call ncclos(infid, exitcode)
#ifdef vebose
      if(exitcode .eq. 0) write(*,*) 'Input file closed.'      
#endif
#ifdef verbose
      write(*,*) ' Done loading snapshot.  Exiting load_flame_data.'
#endif
      return
      end subroutine load_flame_cut_data
!----------------------------------------------------------

!----------------------------------------------------------

      subroutine load_flame_tracer(filenumber)

!----------------------------------------------------------
! This subroutine is the FLAME parallel to load_orca_tracer
! and is intended to be used when only T and S data are
! needed from each snapshot.  Note a difference in the
! expected input file name.
!----------------------------------------------------------

        use params
        use netcdfio
        use infields
        use grids

        implicit none

        ! Local counters
        integer :: filenumber, i, j, k

        ! Initialize (wipe out) infile fields
        ti = 0.0
        si = 0.0

        ! Determine the infile name.  Add "F" because
        ! FLAME input file and we're opening only tracer
        ! (T and S) information.
        write(infname,'(a,i5,a,a)')infroot,filenumber,'F',infext
#ifdef verbose
        write(6,'(a,a)')' Opening file: ',infname
#endif
        ! Open the file n
        infid = ncopn(infname, ncnowrit, exitcode)
#ifdef verbose
        if (exitcode .eq. 0) write(*,*) ' File opened OK.'
#endif
        ! Get the variable ids from the infile:
        tvid = ncvid(infid, tvnam, exitcode)
        svid = ncvid(infid, svnam, exitcode)
#ifdef verbose
        if(exitcode .eq. 0) write(*,*) ' Got variable IDs.'
#endif
        ! Read in salinity and temperature
        call ncvgt(infid,tvid,readstart,readcount,ti,exitcode)
        call ncvgt(infid,svid,readstart,readcount,si,exitcode)
#ifdef verbose
        if(exitcode .eq. 0) write(*,*) ' Read (t,s).'
        write(*,*) ' Closing input file...'
#endif
        call ncclos(infid, exitcode)
#ifdef verbose
        if(exitcode .eq. 0) write(*,*) ' File closed OK.'
#endif
        return
      end subroutine load_flame_tracer
!----------------------------------------------------------

!----------------------------------------------------------

      subroutine load_flame_clim_tracer(filenumber)

!----------------------------------------------------------
! This subroutine is the FLAME parallel to load_orca_tracer
! and is intended to be used when only T and S data are
! needed from each snapshot.  Note a difference in the
! expected input file name.
!
! This routine is hard coded to load climatology files
! built from the FLAME monthly mean files!
!----------------------------------------------------------

        use params
        use netcdfio
        use infields
        use grids

        implicit none

        ! Local counters
        integer :: filenumber, i, j, k

        ! Initialize (wipe out) infile fields
        ti = 0.0
        si = 0.0

        ! Determine the infile name.  Add "C" because
        ! FLAME input file and we're opening only tracer
        ! (T and S) information.
        write(infname,'(a,i5,a,a)')infroot,filenumber,'C',infext
#ifdef verbose
        write(6,'(a,a)')' Opening file: ',infname
#endif
        ! Open the file n
        infid = ncopn(infname, ncnowrit, exitcode)
#ifdef verbose
        if (exitcode .eq. 0) write(*,*) ' File opened OK.'
#endif
        ! Get the variable ids from the infile:
        tvid = ncvid(infid, 'temp', exitcode)
        svid = ncvid(infid, 'salt', exitcode)
#ifdef verbose
        if(exitcode .eq. 0) write(*,*) ' Got variable IDs.'
#endif
        ! Read in salinity and temperature
        call ncvgt(infid,tvid,readstart,readcount,ti,exitcode)
        call ncvgt(infid,svid,readstart,readcount,si,exitcode)
#ifdef verbose
        if(exitcode .eq. 0) write(*,*) ' Read (t,s).'
        write(*,*) ' Closing input file...'
#endif
        call ncclos(infid, exitcode)
#ifdef verbose
        if(exitcode .eq. 0) write(*,*) ' File closed OK.'
#endif

#ifdef verbose
        write(*,*) '   Filtering (s,t)...'
#endif
        do k = 1,readcount(3)
           do j = 1,readcount(2)
              do i = 1,readcount(1)
                 if((ti(i,j,k,1).gt.tlow).and. &
                      (ti(i,j,k,1).lt.thigh).and. &
                      (kmt(i,j).ge.k)) then
                    ! Do nothing the value is good.
                 else
                    ti(i,j,k,1) = fill_real
                 endif

                 if((si(i,j,k,1).gt.slow).and. &
                      (si(i,j,k,1).lt.shigh).and. &
                      (kmt(i,j).ge.k)) then
                    ! Do nothing, the value is good
                 else
                    si(i,j,k,1) = fill_real
                 endif
              enddo
           enddo
        enddo

        return
      end subroutine load_flame_clim_tracer
!----------------------------------------------------------

!----------------------------------------------------------

      subroutine load_flame_temp(filenumber)

! This subroutine is the FLAME parallel to load_orca_tracer
! and is intended to be used when only T data are
! needed from each snapshot.
!
! NOTE! THIS WAS SPECIFICALLY MODIFIED TO WORK WITH LAG_VAR
! SO THE TRAJECTORY CODE CONVENTION OF K = 0:KM IS USED HERE
! INSTEAD OF THE CONVENTION OF K = 1:KM AS IS USED ELSEWHERE.
! This will cause segmentation faults if this routine is
! used with the programs that assume k = 1:km.  This also
! applies to load_flame_salt, below.

        use params
        use netcdfio
        use infields
        use grids

        implicit none

        ! Local counters
        integer :: filenumber, i, j, k

        ! Allocate space for temporary holder
        allocate(ci(readcount(1),readcount(2),readcount(3),1))

        ! Initialize (wipe out) infile fields
        ci = 0.0

        write(infname,'(a,i5,a,a)')infroot,filenumber,'F',infext
#ifdef verbose
        write(6,'(a,a)')' Opening file: ',infname
#endif
        ! Open the file n
        infid = ncopn(infname, ncnowrit, exitcode)

        ! Get the variable ids from the infile:
        tvid = ncvid(infid, tvnam, exitcode)
#ifdef verbose
        if(exitcode .eq. 0) write(*,*) '   Got variable ID.'
#endif
        ! Read in temperature
        call ncvgt(infid,tvid,readstart,readcount,ci,exitcode)
#ifdef verbose
        if(exitcode .eq. 0) write(*,*) '   Read temp.'
#endif
        ! Assign values to surface
        do j = 1,readcount(2)
           do i = 1,readcount(1)
              ti(i,j,0,1) = ci(i,j,1,1)
           enddo
        enddo
        ! Assign values to volume
        do k = 1,readcount(3)
           do j = 1,readcount(2)
              do i = 1,readcount(1)
                 ! No additional filtering done here.
                 ti(i,j,k,1) = ci(i,j,k,1)
              enddo
           enddo
        enddo
        deallocate(ci)

        ! Close the input file
        call ncclos(infid, exitcode)
#ifdef vebose
        if(exitcode .eq. 0) write(*,*) 'Input file closed.'      
#endif
      end subroutine load_flame_temp
!----------------------------------------------------------

!----------------------------------------------------------

      subroutine load_flame_salt(filenumber)

! This subroutine is the FLAME parallel to load_orca_tracer
! and is intended to be used when only S data are
! needed from each snapshot.
!
! !!!!SEE NOTE FOR LOAD_FLAME_TEMP!!!!
        use params
        use netcdfio
        use infields
        use grids

        implicit none

        ! Local counters
        integer :: filenumber, i, j, k

        ! Allocate space for temporary holder
        allocate(ci(readcount(1),readcount(2),readcount(3),1))

        ! Initialize (wipe out) infile fields
        ci = 0.0

        write(infname,'(a,i5,a,a)')infroot,filenumber,'F',infext
#ifdef verbose
        write(6,'(a,a)')' Opening file: ',infname
#endif
        ! Open the file n
        infid = ncopn(infname, ncnowrit, exitcode)

        ! Get the variable ids from the infile:
        svid = ncvid(infid, svnam, exitcode)
#ifdef verbose
        if(exitcode .eq. 0) write(*,*) '   Got variable ID.'
#endif
        ! Read in temperature
        call ncvgt(infid,svid,readstart,readcount,ci,exitcode)
#ifdef verbose
        if(exitcode .eq. 0) write(*,*) '   Read salt.'
#endif
        ! Assign values to surface
        do j = 1,readcount(2)
           do i = 1,readcount(1)
              si(i,j,0,1) = ci(i,j,1,1)
           enddo
        enddo
        ! Assign values to volume
        do k = 1,readcount(3)
           do j = 1,readcount(2)
              do i = 1,readcount(1)
                 ! No additional filtering done here.
                 si(i,j,k,1) = ci(i,j,k,1)
              enddo
           enddo
        enddo
        deallocate(ci)

        ! Close the input file
        call ncclos(infid, exitcode)
#ifdef vebose
        if(exitcode .eq. 0) write(*,*) 'Input file closed.'      
#endif
      end subroutine load_flame_salt
!----------------------------------------------------------
#endif

#if defined (storm) || defined (storm_cdo)
!----------------------------------------------------------
      subroutine load_storm_cut_data(filenumber)

      use params
      use netcdfio
      use infields
      use grids

      implicit none

      ! Input file number
      integer :: filenumber

      ! Local dummy file name
      integer :: dummy_file_num

      ! Local counter
      integer :: i,j,k
      integer :: dg(2)
      real, allocatable :: tmp(:,:)

      ! STORM data may not be time step separated.
      ! In that case, the "filenumber" is really
      ! the fourth index in the readstart variable.
      if ( filenumber .gt. 10000 ) then
         readstart(4) = filenumber-10000
         dummy_file_num = 10001
         write(*,*) 'Set readstart to timestep ',readstart(4)
      else
         dummy_file_num = filenumber
      endif

      readst4d(1) = readstart(1)
#ifdef flipj
      readst4d(2) = jmt - (readstart(2) + readcount(2) - 1) + 1
#else
      readst4d(2) = readstart(2)
#endif
      readst4d(3) = 1
      readst4d(4) = readstart(4)
      readct4d(1) = readcount(1)
      readct4d(2) = readcount(2)
      readct4d(3) = readcount(3)
      readct4d(4) = 1
      dg(1) = readcount(1)
      dg(2) = readcount(2)

      !---------------U-velocity----------------
      write(infname,'(a,i5,a,a)')infroot,dummy_file_num,'U',infext
#ifdef verbose
      write(6,'(a,a)')' Opening file: ',infname
#endif
      infid = ncopn(infname,ncnowrit,exitcode)
      uvid = ncvid(infid,uvnam,exitcode)
      call ncvgt(infid,uvid,readst4d,readct4d,ui,exitcode)
#ifdef flipj
      do k = 1,readcount(3)
         call flip_j(ui(:,:,k,1),dg)
      enddo
#endif
      call ncclos(infid, exitcode)

      !---------------V-velocity----------------
      write(infname,'(a,i5,a,a)')infroot,dummy_file_num,'V',infext
#ifdef verbose
      write(6,'(a,a)')' Opening file: ',infname
#endif
      infid = ncopn(infname,ncnowrit,exitcode)
      vvid = ncvid(infid,vvnam,exitcode)
      call ncvgt(infid,vvid,readst4d,readct4d,vi,exitcode)
#ifdef flipj
      do k = 1,readcount(3)
         call flip_j(vi(:,:,k,1),dg)
      enddo
#endif
      call ncclos(infid, exitcode)

      !---------------Temperature----------------
      write(infname,'(a,i5,a,a)')infroot,dummy_file_num,'T',infext
#ifdef verbose
      write(6,'(a,a)')' Opening file: ',infname
#endif
      infid = ncopn(infname,ncnowrit,exitcode)
      tvid = ncvid(infid,tvnam,exitcode)
      call ncvgt(infid,tvid,readst4d,readct4d,ti,exitcode)
#ifdef flipj
      do k = 1,readcount(3)
         call flip_j(ti(:,:,k,1),dg)
      enddo
#endif
      call ncclos(infid, exitcode)

      !---------------Salinity----------------
      write(infname,'(a,i5,a,a)')infroot,dummy_file_num,'S',infext
#ifdef verbose
      write(6,'(a,a)')' Opening file: ',infname
#endif
      infid = ncopn(infname,ncnowrit,exitcode)
      svid = ncvid(infid,svnam,exitcode)
      call ncvgt(infid,svid,readst4d,readct4d,si,exitcode)
#ifdef flipj
      do k = 1,readcount(3)
         call flip_j(si(:,:,k,1),dg)
      enddo
#endif
      call ncclos(infid,exitcode)

#ifdef verbose
      write(*,*) '   Done reading u,v,t,s.'
      write(*,*) '   Filtering u,v,t,s with sanity check...'
#endif
      do k = 1,readcount(3)
         do j = 1,readcount(2)
            do i = 1,readcount(1)
               if((ti(i,j,k,1).gt.tlow).and. &
                  (ti(i,j,k,1).lt.thigh).and. &
                  (dzt(i,j,k) .gt. 0.0)) then
                  ! Do nothing, the value is good
               else
                  ti(i,j,k,1) = fill_real
               endif

               if((si(i,j,k,1).gt.slow).and. &
                  (si(i,j,k,1).lt.shigh).and. &
                  (dzt(i,j,k) .gt. 0.0)) then
                  ! Do nothing, the value is good
               else
                  si(i,j,k,1) = fill_real
               endif

               if((ui(i,j,k,1).gt.ulow).and. &
                  (ui(i,j,k,1).lt.uhigh)) then
                  ! Do nothing, the value is good
               else
                  ui(i,j,k,1) = 0.0
               endif

               if((vi(i,j,k,1).gt.vlow).and. &
                  (vi(i,j,k,1).lt.vhigh)) then
                  ! Do nothing, the value is good
               else
                  vi(i,j,k,1) = 0.0
               endif
            enddo
         enddo
      enddo
#ifdef verbose
      write(*,*) '   Done filtering u,v,t,s.'
#endif
      return

    end subroutine load_storm_cut_data
!----------------------------------------------------------
#endif

#if defined (orca050) || defined (orca025) || defined (orca025_global) || defined (viking20_cut) || defined (viking20_full)
!----------------------------------------------------------

      subroutine load_orca_data(filenumber)

          use params
          use netcdfio
          use infields
          use grids

          implicit none
          
          ! Local counters
          integer :: filenumber, i, j, k
          
          ! Local holders for f-grid to t-grid interpolation
          real :: left, right
          
          !--------------------------------------
#ifdef verbose
          write(*,*) ' Starting load_orca_data...'
#endif
          ! Initialize (wipe out) infile fields
          ti = 0.0
          si = 0.0
          ui = 0.0
          vi = 0.0
          wi = 0.0
          hi = 0.0

          ! For each block, we do the following:
          ! (1) Determine the infile name.
          ! (2) Open the file n.
          ! (3) Get the variable ids from the infile.
          ! (4) Read the source values from the infile.
          ! (5) Close the infile.
          
          ! Note that there is an "avgin" compiler option
          ! which takes into account the difference file
          ! structure if we have a 3D input file that was
          ! created by eddycdf_3d (tavgvar, uavgvar).
#ifdef avgin
          ! >>>>> This is for 3d average files <<<<<
          ! No provision is made for GM90 since 3d avg
          ! files are likely to be computed only for
          ! ORCA025, which does not have GM90.

          !----Zonal, meridional, and vertical velocities----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'U',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          uvid = ncvid(infid, uvnam, exitcode)
          vvid = ncvid(infid, vvnam, exitcode)
          wvid = ncvid(infid, wvnam, exitcode)
          call ncvgt(infid, uvid, readstart, readcount, ui, exitcode)
          call ncvgt(infid, vvid, readstart, readcount, vi, exitcode)
#if defined (uavgvar) || defined (qeddy) || defined (zeddy)
          call ncvgt(infid, wvid, readstart, readcount, wi, exitcode)
#endif
          call ncclos(infid,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
#if defined (tavgvar) || defined (qeddy)
          !----Temperature and salinity----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          tvid = ncvid(infid, tvnam, exitcode)
          svid = ncvid(infid, svnam, exitcode)
          call ncvgt(infid, tvid, readstart, readcount, ti, exitcode)
          call ncvgt(infid, svid, readstart, readcount, si, exitcode)
          call ncclos(infid,exitcode)
#endif
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
#else
          ! >>>>> This is the standard version of load_orca_data <<<<<

          !----Zonal Velocity----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'U',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          uvid = ncvid(infid, uvnam, exitcode)
          call ncvgt(infid, uvid, readstart, readcount, ui, exitcode)
#ifdef gm90
          ! Get the eddy-induced velocity and combine with velocity
          svid = ncvid(infid, 'vozoeivu', exitcode)
          call ncvgt(infid, svid, readstart, readcount, si, exitcode)
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   if(  ui(i,j,k,1) .gt. uhigh .or. &
                        ui(i,j,k,1) .lt. ulow .or. &
                        si(i,j,k,1) .gt. uhigh .or. &
                        si(i,j,k,1) .lt. ulow ) then
                      ui(i,j,k,1) = fill_real
                   else
                      ui(i,j,k,1) = ui(i,j,k,1) + si(i,j,k,1)
                   endif
                enddo
             enddo
          enddo
          si = 0.0
#endif
          call ncclos(infid,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
          !----Meridional Velocity----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'V',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          vvid = ncvid(infid, vvnam, exitcode)
          call ncvgt(infid, vvid, readstart, readcount, vi, exitcode)
#ifdef gm90
          ! Get the eddy-induced velocity and combine with velocity
          svid = ncvid(infid, 'vomeeivv', exitcode)
          call ncvgt(infid, svid, readstart, readcount, si, exitcode)
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   if(  vi(i,j,k,1) .gt. uhigh .or. &
                        vi(i,j,k,1) .lt. ulow .or. &
                        si(i,j,k,1) .gt. uhigh .or. &
                        si(i,j,k,1) .lt. ulow ) then
                      vi(i,j,k,1) = fill_real
                   else
                      vi(i,j,k,1) = vi(i,j,k,1) + si(i,j,k,1)
                   endif
                enddo
             enddo
          enddo
          si = 0.0
#endif
          call ncclos(infid,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
#if defined (uavgvar) || defined (qeddy) || defined (zeddy)
          !----Vertical Velocity----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'W',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          wvid = ncvid(infid, wvnam, exitcode)
          call ncvgt(infid, wvid, readstart, readcount, wi, exitcode)
#ifdef gm90
          ! Get the eddy-induced velocity and combine with velocity
          svid = ncvid(infid, 'voveeivw', exitcode)
          call ncvgt(infid, svid, readstart, readcount, si, exitcode)
          do k = 1,km
             do j = 1,jmt
                do i = 1,imt
                   if(  wi(i,j,k,1) .gt. whigh .or. &
                        wi(i,j,k,1) .lt. wlow .or. &
                        si(i,j,k,1) .gt. whigh .or. &
                        si(i,j,k,1) .lt. wlow ) then
                      wi(i,j,k,1) = fill_real
                   else
                      wi(i,j,k,1) = wi(i,j,k,1) + si(i,j,k,1)
                   endif
                enddo
             enddo
          enddo
          si = 0.0
#endif
          call ncclos(infid,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
#endif
! End of whether or not to load w
#if defined (tavgvar) || defined (qeddy)
          !----Temperature and Salinity----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          tvid = ncvid(infid, tvnam, exitcode)
          svid = ncvid(infid, svnam, exitcode)
          hvid = ncvid(infid, hvnam, exitcode)
          call ncvgt(infid, tvid, readstart, readcount, ti, exitcode)
          call ncvgt(infid, svid, readstart, readcount, si, exitcode)
          call ncvgt(infid, hvid, writest3d, writect3d, hi, exitcode)
          call ncclos(infid,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
#endif
! End of whether or not to load T,S.
#endif
! End of avgin or standard data loading.
          ! Filter and mask out (w/ fill values) t,s,u,v,w (3d) fields
          do k = 1,km
!             write(*,*) k
             do j = 1,jmt
                do i = i,imt
                   
                   if( (ui(i,j,k,1).lt.uhigh).and. &
                        (ui(i,j,k,1).gt.ulow).and. &
                        (umask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      ui(i,j,k,1) = fill_real
                   endif
                   
                   if( (vi(i,j,k,1).lt.vhigh).and. &
                        (vi(i,j,k,1).gt.vlow).and. &
                        (vmask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      vi(i,j,k,1) = fill_real
                   endif
#if defined (uavgvar) || defined (qeddy) || defined (zeddy)
                   if( (wi(i,j,k,1).lt.whigh).and. &
                        (wi(i,j,k,1).gt.wlow).and. &
                        (tmask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      wi(i,j,k,1) = fill_real
                   endif
#endif
#if defined (tavgvar) || defined (qeddy)
                   if( (ti(i,j,k,1).lt.thigh).and. &
                        (ti(i,j,k,1).gt.tlow).and. &
                        (tmask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      ti(i,j,k,1) = fill_real
                   endif
                   
                   if( (si(i,j,k,1).lt.shigh).and. &
                        (si(i,j,k,1).gt.slow).and. &
                        (tmask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      si(i,j,k,1) = fill_real
                   endif
#endif
                enddo
             enddo
          enddo

          ! Filter and mask out (w/ fill values) h (2d) fields
          do j = 1,jmt
             do i = 1,imt
#if defined (tavgvar) || defined (qeddy)
                if( (hi(i,j,1).lt.hhigh).and. &
                    (hi(i,j,1).gt.hlow).and. &
                    (tmask(i,j,1).eq.1) ) then
                   ! Do nothing, value is good
                else
                   hi(i,j,1) = fill_real
                endif
#endif
             enddo
          enddo

#ifdef verbose
          write(*,*) '   All variables filtered.  Exit load_orca_data.'
#endif
          return     
        end subroutine load_orca_data
!----------------------------------------------------------

!----------------------------------------------------------
        subroutine load_orca_cut_data(filenumber)
!----------------------------------------------------------
! This subroutine will load data from the ORCA models
! cut to the limits set by readstart and readcount.
!----------------------------------------------------------
          use params
          use netcdfio
          use infields
          use grids

          implicit none
          
          ! Local counters
          integer :: filenumber, i, j, k
          
          ! Local holders for f-grid to t-grid interpolation
          real :: left, right
          
          !--------------------------------------
#ifdef verbose
          write(*,*) ' Starting load_orca_cut_data...'
#endif
          ! Initialize (wipe out) infile fields
          ti = 0.0
          si = 0.0
          ui = 0.0
          vi = 0.0
          !wi = 0.0
          !hi = 0.0 ! See load_orca_cut_ssh
#ifdef verbose
          write(*,*) ' Infields zeroed out.'
#endif
          ! For each block, we do the following:
          ! (1) Determine the infile name.
          ! (2) Open the file n.
          ! (3) Get the variable ids from the infile.
          ! (4) Read the source values from the infile.
          ! (5) Close the infile.
          
          !----Zonal Velocity----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'U',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          uvid = ncvid(infid, uvnam, exitcode)
          call ncvgt(infid,uvid,readstart,readcount,ui,exitcode)
          call ncclos(infid,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'U file closed ok'
#endif
          !----Meridional Velocity----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'V',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          vvid = ncvid(infid, vvnam, exitcode)
          call ncvgt(infid,vvid,readstart,readcount,vi,exitcode)
          call ncclos(infid,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'V file closed ok'
#endif
          !----Temperature and Salinity----
          write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          tvid = ncvid(infid, tvnam, exitcode)
          svid = ncvid(infid, svnam, exitcode)
          hvid = ncvid(infid, hvnam, exitcode)
          call ncvgt(infid,tvid,readstart,readcount,ti,exitcode)
          call ncvgt(infid,svid,readstart,readcount,si,exitcode)
          call ncclos(infid,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
          ! Filter and mask out (w/ fill values) t,s,u,v,w (3d) fields
          do k = 1,readcount(3)
             do j = 1,readcount(2)
                do i = 1,readcount(1)                   
                   if( (ui(i,j,k,1).lt.uhigh).and. &
                        (ui(i,j,k,1).gt.ulow).and. &
                        (umask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      !ui(i,j,k,1) = fill_real
                      ui(i,j,k,1) = 0.0
                   endif
                   if( (vi(i,j,k,1).lt.vhigh).and. &
                        (vi(i,j,k,1).gt.vlow).and. &
                        (vmask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      !vi(i,j,k,1) = fill_real
                      vi(i,j,k,1) = 0.0
                   endif
                   if( (ti(i,j,k,1).lt.thigh).and. &
                        (ti(i,j,k,1).gt.tlow).and. &
                        (tmask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      ti(i,j,k,1) = fill_real
                   endif                   
                   if( (si(i,j,k,1).lt.shigh).and. &
                        (si(i,j,k,1).gt.slow).and. &
                        (tmask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      si(i,j,k,1) = fill_real
                   endif
                enddo
             enddo
          enddo
#ifdef verbose
          write(*,*) '   All variables filtered.  Exit load_orca_cut_data.'
#endif
          return
        end subroutine load_orca_cut_data
!----------------------------------------------------------

!----------------------------------------------------------
        subroutine load_orca_cut_data_etc(filenumber,uvtsdqa)
!----------------------------------------------------------
! This subroutine will load data from the ORCA models
! cut to the limits set by readstart and readcount.
!
! uvtsdqa = 1, 2, 3, 4 for loading just u, v, t, or s since
! these fields are the largest. 5, 6, 7 are for SSH, MLD, SST.
!
! 
!----------------------------------------------------------
          use params
          use netcdfio
          use infields
          use grids

          implicit none
          
          ! Local counters
          integer :: filenumber, i, j, k
          integer :: uvtsdqa

          ! Local holders for f-grid to t-grid interpolation
          real :: left, right

          !--------------------------------------
#ifdef verbose
          write(*,*) ' Starting load_orca_cut_data_etc...'
#endif
          ! Initialize (wipe out) infile fields
          if ( uvtsdqa .eq. 3 ) then
             ti = 0.0
          elseif ( uvtsdqa .eq. 4 ) then
             si = 0.0
          elseif ( uvtsdqa .eq. 1 ) then
             ui = 0.0
          elseif ( uvtsdqa .eq. 2 ) then
             vi = 0.0
          elseif ( uvtsdqa .eq. 5 ) then
             hi = 0.0
          elseif ( uvtsdqa .eq. 6 ) then
             mldi = 0.0
          elseif ( uvtsdqa .eq. 7 ) then
             ssti = 0.0
          else
             write(*,*) ' load_orca_cut_data_uvts error, uvts unrecognized!'
             return
          endif
#ifdef verbose
          write(*,*) ' Infields zeroed out.'
#endif
          ! For each block, we do the following:
          ! (1) Determine the infile name.
          ! (2) Open the file n.
          ! (3) Get the variable ids from the infile.
          ! (4) Read the source values from the infile.
          ! (5) Close the infile.
          if ( uvtsdqa .eq. 1 ) then
             !----Zonal Velocity----
             write(infname,'(a,i5,a,a)')infroot,filenumber,'U',infext
             write(6,'(a,a)')' Opening file: ',infname
             infid = ncopn(infname, ncnowrit, exitcode)
             uvid = ncvid(infid, uvnam, exitcode)
             call ncvgt(infid,uvid,readstart,readcount,ui,exitcode)
             call ncclos(infid,exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*) 'U file closed ok'
#endif
             ! Filter and mask out (w/ fill values) t,s,u,v,w (3d) fields
             do k = 1,readcount(3)
                do j = 1,readcount(2)
                   do i = 1,readcount(1)                   
                      if( (ui(i,j,k,1).lt.uhigh).and. &
                           (ui(i,j,k,1).gt.ulow).and. &
                           (umask(i,j,k).eq.1) ) then
                         ! Do nothing, value is good
                      else
                         ui(i,j,k,1) = fill_real
                         !ui(i,j,k,1) = 0.0
                      endif
                   enddo
                enddo
             enddo
          elseif ( uvtsdqa .eq. 2 ) then
             !----Meridional Velocity----
             write(infname,'(a,i5,a,a)')infroot,filenumber,'V',infext
             write(6,'(a,a)')' Opening file: ',infname
             infid = ncopn(infname, ncnowrit, exitcode)
             vvid = ncvid(infid, vvnam, exitcode)
             call ncvgt(infid,vvid,readstart,readcount,vi,exitcode)
             call ncclos(infid,exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*) 'V file closed ok'
#endif
             ! Filter and mask out (w/ fill values) t,s,u,v,w (3d) fields
             do k = 1,readcount(3)
                do j = 1,readcount(2)
                   do i = 1,readcount(1)
                      if( (vi(i,j,k,1).lt.vhigh).and. &
                           (vi(i,j,k,1).gt.vlow).and. &
                           (vmask(i,j,k).eq.1) ) then
                         ! Do nothing, value is good
                      else
                         vi(i,j,k,1) = fill_real
                         !vi(i,j,k,1) = 0.0
                      endif
                   enddo
                enddo
             enddo
          elseif ( uvtsdqa .eq. 3 ) then
             !----Temperature and Salinity----
             write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext
             write(6,'(a,a)')' Opening file: ',infname
             infid = ncopn(infname, ncnowrit, exitcode)
             tvid = ncvid(infid, tvnam, exitcode)
             call ncvgt(infid,tvid,readstart,readcount,ti,exitcode)
             call ncclos(infid,exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
             do k = 1,readcount(3)
                do j = 1,readcount(2)
                   do i = 1,readcount(1)
                      if( (ti(i,j,k,1).lt.thigh).and. &
                           (ti(i,j,k,1).gt.tlow).and. &
                           (tmask(i,j,k).eq.1) ) then
                         ! Do nothing, value is good
                      else
                         ti(i,j,k,1) = fill_real
                      endif
                   enddo
                enddo
             enddo

          elseif ( uvtsdqa .eq. 4 ) then
             write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext
             write(6,'(a,a)')' Opening file: ',infname
             infid = ncopn(infname, ncnowrit, exitcode)
             svid = ncvid(infid, svnam, exitcode)
             call ncvgt(infid,svid,readstart,readcount,si,exitcode)
             call ncclos(infid,exitcode)
#ifdef verbose
             if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
             do k = 1,readcount(3)
                do j = 1,readcount(2)
                   do i = 1,readcount(1)
                      if( (si(i,j,k,1).lt.shigh).and. &
                           (si(i,j,k,1).gt.slow).and. &
                           (tmask(i,j,k).eq.1) ) then
                         ! Do nothing, value is good
                      else
                         si(i,j,k,1) = fill_real
                      endif
                   enddo
                enddo
             enddo
          elseif ( uvtsdqa .eq. 5 ) then
             write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext
             write(6,'(a,a)')' Opening file: ',infname
             infid = ncopn(infname, ncnowrit, exitcode)
             hvid = ncvid(infid, hvnam, exitcode)
             ! Assign reading interval
             readst3d(1) = readstart(1)
             readst3d(2) = readstart(2)
             readst3d(3) = readstart(4)
             readct3d(1) = readcount(1)
             readct3d(2) = readcount(2)
             readct3d(3) = readcount(4)

             ! Get values
             call ncvgt(infid,hvid,readst3d,readct3d,hi,exitcode)
          
             call ncclos(infid,exitcode)

             ! Filter
             do j = 1,readcount(2)
                do i = 1,readcount(1)
                   if( (hi(i,j,1).lt.hhigh).and. &
                        (hi(i,j,1).gt.hlow).and. &
                        (tmask(i,j,1).eq.1) ) then
                         ! Do nothing, value is good
                   else
                      hi(i,j,1) = fill_real
                   endif
                enddo
             enddo

#ifdef verbose
             if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
          elseif ( uvtsdqa .eq. 6 ) then
             write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext
             write(6,'(a,a)')' Opening file: ',infname
             infid = ncopn(infname, ncnowrit, exitcode)
             hvid = ncvid(infid, mldvnam, exitcode)
             ! Assign reading interval
             readst3d(1) = readstart(1)
             readst3d(2) = readstart(2)
             readst3d(3) = readstart(4)
             readct3d(1) = readcount(1)
             readct3d(2) = readcount(2)
             readct3d(3) = readcount(4)

             ! Get values
             call ncvgt(infid,hvid,readst3d,readct3d,mldi,exitcode)

             call ncclos(infid,exitcode)

             ! Filter
             do j = 1,readcount(2)
                do i = 1,readcount(1)
                   if( (mldi(i,j,1).lt.mldhigh).and. &
                        (mldi(i,j,1).gt.mldlow).and. &
                        (tmask(i,j,1).eq.1) ) then
                         ! Do nothing, value is good
                   else
                      mldi(i,j,1) = fill_real
                   endif
                enddo
             enddo
#ifdef verbose
             if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
          elseif ( uvtsdqa .eq. 7 ) then
             write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext
             write(6,'(a,a)')' Opening file: ',infname
             infid = ncopn(infname, ncnowrit, exitcode)
             hvid = ncvid(infid, sstvnam, exitcode)
             ! Assign reading interval
             readst3d(1) = readstart(1)
             readst3d(2) = readstart(2)
             readst3d(3) = readstart(4)
             readct3d(1) = readcount(1)
             readct3d(2) = readcount(2)
             readct3d(3) = readcount(4)

             ! Get values
             call ncvgt(infid,hvid,readst3d,readct3d,ssti,exitcode)

             call ncclos(infid,exitcode)

             ! Filter
             do j = 1,readcount(2)
                do i = 1,readcount(1)
                   if( (ssti(i,j,1).lt.thigh).and. &
                        (ssti(i,j,1).gt.tlow).and. &
                        (tmask(i,j,1).eq.1) ) then
                         ! Do nothing, value is good
                   else
                      ssti(i,j,1) = fill_real
                   endif
                enddo
             enddo
#ifdef verbose
             if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
          endif
#ifdef verbose
          write(*,*) '   All variables filtered.  Exit load_orca_cut_data_etc.'
#endif
          return
        end subroutine load_orca_cut_data_etc
!----------------------------------------------------------

!----------------------------------------------------------
        subroutine load_orca_ssh(filenumber)
!----------------------------------------------------------
! This subroutine will load ssh data from the ORCA models
! cut to the limits set by readstart and readcount.
!----------------------------------------------------------
          use params
          use netcdfio
          use infields
          use grids

          implicit none
          
          ! Local counters
          integer :: filenumber, i, j, k, l
          
          !--------------------------------------
#ifdef verbose
          write(*,*) ' Starting load_orca_ssh...'
#endif
          ! Initialize (wipe out) infile fields
          hi = 0.0
#ifdef verbose
          write(*,*) ' Infields zeroed out.'
#endif
          ! Build input file name
          write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext

          ! Open input file
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)

          ! Get variable ID
          hvid = ncvid(infid, hvnam, exitcode)

          ! Assign reading interval
          readst3d(1) = readstart(1)
          readst3d(2) = readstart(2)
          readst3d(3) = readstart(4)
          readct3d(1) = readcount(1)
          readct3d(2) = readcount(2)
          readct3d(3) = readcount(4)

          ! Get values
          call ncvgt(infid,hvid,readst3d,readct3d,hi,exitcode)
          
          ! Close input file
          call ncclos(infid,exitcode)
#ifdef verbose
          if(exitcode .eq. 0) write(*,*) 'File closed ok'
#endif
          ! Filter and mask out (w/ fill values) h (2d) fields
          do l = 1,readct3d(3)
             do j = 1,readct3d(2)
                do i = 1,readct3d(1)
                   if( (hi(i,j,l).lt.hhigh).and. &
                        (hi(i,j,l).gt.hlow).and. &
                        (tmask(i,j,1).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      hi(i,j,l) = fill_real
                   endif
                enddo
             enddo
          enddo
#ifdef verbose
          write(*,*) ' Exit load_orca_ssh.'
#endif
          return
        end subroutine load_orca_ssh
!----------------------------------------------------------

!----------------------------------------------------------

        subroutine load_orca_tracer(filenumber)

!----------------------------------------------------------
! This subroutine is geared for just opening t,s,c from two
! separate data filed.  Used primarily in cfccore.f90.
!
! WARNING: CURRENTLY, T and S are commented out. Loads
! CFC only.
!----------------------------------------------------------

          use params
          use netcdfio
          use infields
          use grids

          implicit none
          
          ! Local counters
          integer :: filenumber, i, j, k
          
          !--------------------------------------
#ifdef verbose
          write(*,*) ' Starting load_orca_tracers...'
#endif
          ! Initialize (wipe out) infile fields
          !ti = 0.0
          !si = 0.0
          ci = 0.0

          ! For each block, we do the following:
          ! (1) Determine the infile name.
          ! (2) Open the file n.
          ! (3) Get the variable ids from the infile.
          ! (4) Read the source values from the infile.
          ! (5) Close the infile.
          
          !----Temperature and Salinity----
          !write(infname,'(a,i5,a,a)')infroot,filenumber,'T',infext
          !write(6,'(a,a)')' Opening file: ',infname
          !infid = ncopn(infname, ncnowrit, exitcode)
          !tvid = ncvid(infid, tvnam, exitcode)
          !svid = ncvid(infid, svnam, exitcode)
          !call ncvgt(infid, tvid, readstart, readcount, ti, exitcode)
          !call ncvgt(infid, svid, readstart, readcount, si, exitcode)
          !call ncclos(infid,exitcode)

          !----Tracer----------------------
          write(infname,'(a,i5,a,a)')infroot,filenumber,'C',infext
          write(6,'(a,a)')' Opening file: ',infname
          infid = ncopn(infname, ncnowrit, exitcode)
          cvid = ncvid(infid, cvnam, exitcode)
          call ncvgt(infid, cvid, readstart, readcount, ci, exitcode)
          call ncclos(infid,exitcode)

          ! Filter and mask out (w/ fill values) t,s,c (3d) fields
          do k = 1,km
             write(*,*) k
             do j = 1,jmt
                do i = i,imt
                   
                   !if( (ti(i,j,k,1).lt.thigh).and. &
                   !     (ti(i,j,k,1).gt.tlow).and. &
                   !     (tmask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   !else
                   !   ti(i,j,k,1) = fill_real
                   !endif
                   
                   !if( (si(i,j,k,1).lt.shigh).and. &
                   !     (si(i,j,k,1).gt.slow).and. &
                   !     (tmask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   !else
                   !   si(i,j,k,1) = fill_real
                   !endif

                   if( (ci(i,j,k,1).lt.chigh).and. &
                        (ci(i,j,k,1).gt.clow).and. &
                        (tmask(i,j,k).eq.1) ) then
                      ! Do nothing, value is good
                   else
                      ci(i,j,k,1) = fill_real
                   endif

                enddo
             enddo
          enddo
#ifdef verbose
          write(*,*) '   All variables filtered.  Exit load_orca_data.'
#endif
          return     
        end subroutine load_orca_tracer
!----------------------------------------------------------
#endif  

      end module load_data
!----------------------------------------------------------

