#!/bin/csh -f
#------------------------------------------------------------------------------
set srcdir = ./
#set ariane_dir = /home/sa03sg/src/vistraj/ariane_tools
#set vistraj_dir = /home/sa03sg/src/vistraj
set netcdfdir = /usr/local/lib
#set modtyp = mmflame

#echo Compiling the basics
#gfortran -xf95-cpp-input -D${modtyp} -Dverbose -Dqeddy -c ${srcdir}/gridsio.f90 -o gridsio.o
#gfortran -xf95-cpp-input -D${modtyp} -Dverbose         -c ${srcdir}/ops2d_cut.f90 -o ops2d_cut.o
#gfortran -xf95-cpp-input -D${modtyp} -Dverbose         -c ${srcdir}/ops2d.f90 -o ops2d.o
#gfortran -xf95-cpp-input -D${modtyp} -Dverbose         -c ${srcdir}/basicfun.f90 -o basicfun.o
#gfortran -xf95-cpp-input                               -c ${srcdir}/tcdfio.f90 -o tcdfio.o

#echo Compiling and linking ofcdf
#gfortran -xf95-cpp-input -D${modtyp} -Dverbose -Dinterp_frac -c ${srcdir}/ofcdf_11.f90 -o ofcdf.o
#gfortran gridsio.o ops2d_cut.o basicfun.o ofcdf.o -o ofcdf.x -L${netcdfdir} -lnetcdff

#echo Compiling and linking lag_cent
#gfortran -xf95-cpp-input -D${modtyp} -Dverbose               -c ${srcdir}/lag_cent.f90 -o lag_cent.o
#gfortran gridsio.o tcdfio.o basicfun.o lag_cent.o -o lag_cent.x -L${netcdfdir} -lnetcdff

#echo Compiling and linking tcdfhist
#gfortran -xf95-cpp-input             -Dverbose               -c ${srcdir}/tcdfhist.f90 -o tcdfhist.o
#gfortran gridsio.o tcdfio.o basicfun.o tcdfhist.o -o tcdfhist.x -L${netcdfdir} -lnetcdff

#echo Compiling and linking tcdfsort
# THIS CODE NEEDS TO BE MERGED WITH TCDFIO STANDARD
# Old, unmerged version.
#gfortran tcdfsort11a.f90 -o tcdfsort.x -L${netcdfdir} -lnetcdff
#gfortran -c ${srcdir}/tcdfsort12.f90 -o tcdfsort.o
#gfortran gridsio.o tcdfio.o basicfun.o tcdfsort.o -o tcdfsort.x -L${netcdfdir} -lnetcdff

#echo Compiling and linking flame_grid_2_cdf...
#gfortran ${srcdir}/flame_grid_2_cdf.f90 -o flame_grid_2_cdf.x gridsio.o -L${netcdfdir} -lnetcdff

echo compiling and linking change_values...
gfortran -fallow-argument-mismatch -O ${srcdir}/change_values.f90 -o change_values.x -L/usr/local/lib -lnetcdff

echo Compiling and linking avgcdf...
gfortran -xf95-cpp-input -Dverbose -Dbigfile -fallow-argument-mismatch -O ${srcdir}/avgcdf.f90 -o avgcdf_verbose_bigfile.x -L/usr/local/lib -lnetcdff

echo Compiling and linking grd2nc...
#gfortran -xf95-cpp-input -Dverbose -Dbigfile -O ${srcdir}/grd2nc.f90 -o grd2nc_verbose_bigfile.x -L/usr/local/lib -lnetcdff
gfortran -xf95-cpp-input -Dverbose -fallow-argument-mismatch -O ${srcdir}/grd2nc.f90 -o grd2nc.x -L/usr/local/lib -lnetcdff

#echo Compiling and linking FLAME_wind_stress_curl...
#gfortran -xf95-cpp-input -Dverbose -c ${srcdir}/FLAME_wind_stress_curl.f90 -o flame_wind_stress_curl.o
#gfortran gridsio.o ops2d_cut.o flame_wind_stress_curl.o -o flame_wind_stress_curl.x -L/usr/local/netcdf_3/lib -lnetcdf

#echo Compiling and linking CORE_uv2tau...
#gfortran -c ${srcdir}/ncar_ocean_fluxes.2007_08_01.f90 -o ncar_ocean_fluxes.o
#gfortran -xf95-cpp-input             -Dverbose               -c ${srcdir}/CORE_uv2tau.f90 -o CORE_uv2tau.o
#gfortran gridsio.o basicfun.o CORE_uv2tau.o ops2d_cut.o -o CORE_uv2tau.x -L${netcdfdir} -lnetcdff

#echo Compiling and linking cutcdf...
#gfortran -xf95-cpp-input -Dbigfile -Drhp ${srcdir}/cutcdf3.f90 -o cutcdf.x -L${netcdfdir} -lnetcdff

#echo Compiling and linking get_launch_depth...
#rm -f gridsio.o
#gfortran -xf95-cpp-input -Dviking20_cut -Dverbose -c ${srcdir}/gridsio.f90 -o gridsio.o
#gfortran -xf95-cpp-input -Dviking20_cut -c ${ariane_dir}/get_launch_depth.f90 -o get_launch_depth.o
#gfortran gridsio.o get_launch_depth.o -o get_launch_depth.x -L${netcdfdir} -lnetcdff

#echo Compiling and linking get_traj_water_depth...
#rm -f gridsio.o
#gfortran -xf95-cpp-input -Dviking20_cut -Dverbose -c ${srcdir}/gridsio.f90 -o gridsio.o 
#-fopenmp
#gfortran -xf95-cpp-input -Dviking20_cut -c ${ariane_dir}/get_traj_water_depth.f90 -o get_traj_water_depth.o
#-fopenmp
#gfortran gridsio.o tcdfio.o get_traj_water_depth.o -o get_traj_water_depth.x -L${netcdfdir} -lnetcdff
#-fopenmp

#echo Compiling and linking ariane2tcdf...
#gfortran -xf95-cpp-input -Dclip_seamounts -c ${vistraj_dir}/convert/ariane2tcdf_3.f90 -o ariane2tcdf.o
#gfortran gridsio.o basicfun.o tcdfio.o ariane2tcdf.o -o ariane2tcdf.x -L${netcdfdir} -lnetcdff

# get_w
#rm -f gridsio.o
#echo Compiling and linking get_w...
#gfortran -xf95-cpp-input -Dorca025_global -Dverbose -Dzeddy -c ${srcdir}/gridsio.f90 -o gridsio.o 
#gfortran -c ${srcdir}/get_w.f90 -o get_w.o
#gfortran gridsio.o get_w.o -o get_w.x -L${netcdfdir} -lnetcdff
