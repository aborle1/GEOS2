#!/bin/bash -x

ORIGDIR=/discover/nobackup/projects/gmao/share/gmao_ops/fvInput/g5gcm/moist/L72

CURRDIR=$(pwd)

for file in $(ls $ORIGDIR)
do
   cdo -L sellonlatbox,-180,180,-90,90 -remapbil,r90x47 $ORIGDIR/$file $CURRDIR/$file
done

