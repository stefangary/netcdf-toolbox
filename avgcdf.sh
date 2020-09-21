#!/bin/tcsh

set prefix = infile
set suffix = .cdf

if ( 1 == 0 ) then
    @ filecount = 10000
    foreach file ( /media/sfg/CKP_BKUP_2/AVHRR/*.nc )
	echo $file
	@ filecount = $filecount + 1    
	ln -sv $file ./${prefix}${filecount}${suffix}
    end

    @ filecount = $filecount - 10000

    avgcdf_verbose_bigfile.x $filecount

    rm -rf ${prefix}*${suffix}
else
    set years_to_use = (1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017)
    foreach year ( $years_to_use )

	@ filecount = 10000
	foreach file ( /media/sfg/CKP_BKUP_2/AVHRR/*_${year}??.nc )
	    @ filecount = $filecount + 1
	    ln -sv $file ./${prefix}${filecount}${suffix}
	end

	@ filecount = $filecount - 10000

	avgcdf_verbose_bigfile.x $filecount

	rm -rf ${prefix}*${suffix}

	mv avgfile.cdf Monthly_mean_Rrs_AVHRR_visible_avg_${year}.nc
	mv varfile.cdf Monthly_mean_Rrs_AVHRR_visible_var_${year}.nc

	gzip -1v *.nc

	mv *.nc.gz /opt/data_archive/
    end
end
