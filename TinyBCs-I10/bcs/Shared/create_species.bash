#!/bin/bash -x

ORIGFILE=/discover/nobackup/ltakacs/bcs/Icarus/Shared/pchem.species.CMIP-5.1870-2097.z_91x72.nc4


BEGYEAR=1999
ENDYEAR=2000

#BEGYEAR=1992
#ENDYEAR=2013

NUMYEARS=$((ENDYEAR-BEGYEAR+1))

FINALFILE=pchem.species.CMIP-5.$BEGYEAR-$ENDYEAR.z_91x72.nc4

cdo selyear,$BEGYEAR/$ENDYEAR $ORIGFILE temp1.nc4

ncatted -a climYears,global,m,i,$NUMYEARS -a begClimYear,global,m,i,$BEGYEAR -a endClimYear,global,m,i,$ENDYEAR temp1.nc4 temp2.nc4

ncks -L 1 temp2.nc4 $FINALFILE

rm temp1.nc4 temp2.nc4
