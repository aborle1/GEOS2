#!/bin/sh

#########

if [[ $# -ne 1 ]]
then
   echo "$0 requires one argument"
   echo ""
   echo "   The argument is the prefix directory under"
   echo "   which are the etc/, bin/, include/, etc., "
   echo "   directories."
   exit 1
fi

PREFIX=$1
ETCDIR="$1/etc"

VERSION_RC_FILE="$ETCDIR/versions.rc"

if [[ -e $VERSION_RC_FILE ]]
then
   /bin/rm $VERSION_RC_FILE
fi

touch $VERSION_RC_FILE

echo_version () {
   echo "$1:$2 #Version Location: $3" >> $VERSION_RC_FILE
}

#########

JPEG_VERSION_LOC="jpeg/jversion.h"
JPEG_VERSION=$(awk '/JVERSION/ {print $3, $4}' $JPEG_VERSION_LOC | sed 's/"//g')
echo_version jpeg "$JPEG_VERSION" "$JPEG_VERSION_LOC"

ZLIB_VERSION_LOC="zlib/zlib.h"
ZLIB_VERSION=$(awk '/#define ZLIB_VERSION/ {print $3}' $ZLIB_VERSION_LOC | sed 's/"//g')
echo_version zlib "$ZLIB_VERSION" "$ZLIB_VERSION_LOC"

SZLIB_VERSION_LOC="szlib/src/szlib.h"
SZLIB_VERSION=$(awk '/#define SZLIB_VERSION/ {print $3}' $SZLIB_VERSION_LOC | sed 's/"//g')
echo_version szlib "$SZLIB_VERSION" "$SZLIB_VERSION_LOC"

CURL_VERSION_LOC="curl/include/curl/curlver.h"
CURL_VERSION=$(awk '/#define LIBCURL_VERSION / {print $3}' $CURL_VERSION_LOC | sed 's/"//g')
echo_version curl "$CURL_VERSION" "$CURL_VERSION_LOC"

HDF4_VERSION_LOC="hdf4/README.txt"
HDF4_VERSION=$(cat $HDF4_VERSION_LOC | head -1 | awk '{print $3}')
echo_version hdf4 "$HDF4_VERSION" "$HDF4_VERSION_LOC"

HDF5_VERSION_LOC="hdf5/README.txt"
HDF5_VERSION=$(cat $HDF5_VERSION_LOC | head -1 | awk '{print $3}')
echo_version hdf5 "$HDF5_VERSION" "$HDF5_VERSION_LOC"

H5EDIT_VERSION_LOC="h5edit/README"
H5EDIT_VERSION=$(cat $H5EDIT_VERSION_LOC | head -1 | awk '{print $3}')
echo_version h5edit "$H5EDIT_VERSION" "$H5EDIT_VERSION_LOC"

NETCDF_VERSION_LOC="netcdf/configure"
NETCDF_VERSION=$(awk -F= '/^PACKAGE_VERSION=/ {print $2}' $NETCDF_VERSION_LOC | sed "s/'//g")
echo_version netcdf "$NETCDF_VERSION" "$NETCDF_VERSION_LOC"

NETCDF_FORTRAN_VERSION_LOC="netcdf-fortran/configure"
NETCDF_FORTRAN_VERSION=$(awk -F= '/^PACKAGE_VERSION=/ {print $2}' $NETCDF_FORTRAN_VERSION_LOC | sed "s/'//g")
echo_version netcdf-fortran "$NETCDF_FORTRAN_VERSION" "$NETCDF_FORTRAN_VERSION_LOC"

UDUNITS2_VERSION_LOC="udunits2/configure"
UDUNITS2_VERSION=$(awk -F= '/^PACKAGE_VERSION=/ {print $2}' $UDUNITS2_VERSION_LOC | sed "s/'//g")
echo_version udunits2 "$UDUNITS2_VERSION" "$UDUNITS2_VERSION_LOC"

NCO_VERSION_LOC="nco/doc/VERSION"
NCO_VERSION=$(cat ${NCO_VERSION_LOC})
echo_version nco "$NCO_VERSION" "$NCO_VERSION_LOC"

CDO_VERSION_LOC="cdo/configure"
CDO_VERSION=$(awk -F= '/^PACKAGE_VERSION=/ {print $2}' $CDO_VERSION_LOC | sed "s/'//g")
echo_version cdo "$CDO_VERSION" "$CDO_VERSION_LOC"

ESMF_VERSION_LOC="esmf/src/Infrastructure/Util/include/ESMC_Macros.h"
ESMF_VERSION=$(awk '/ESMF_VERSION_STRING/ {print $3}' $ESMF_VERSION_LOC | sed 's/"//g')
echo_version esmf "$ESMF_VERSION" "$ESMF_VERSION_LOC"

PFUNIT_VERSION_LOC="pFUnit/VERSION"
PFUNIT_VERSION=$(cat ${PFUNIT_VERSION_LOC})
echo_version pFUnit "$PFUNIT_VERSION" "$PFUNIT_VERSION_LOC"

HDFEOS_VERSION_LOC="hdfeos/src/EHapi.c"
HDFEOS_VERSION=$(awk '/HDFEOSVERSION1/ {print $3}' $HDFEOS_VERSION_LOC | sed 's/"//g')
echo_version hdfeos "$HDFEOS_VERSION" "$HDFEOS_VERSION_LOC"

UUID_VERSION_LOC="uuid/uuid_vers.h"
UUID_VERSION=$(uuid/shtool version -l c -d short $UUID_VERSION_LOC)
echo_version uuid "$UUID_VERSION" "$UUID_VERSION_LOC"

CMOR_VERSION_LOC="cmor/configure"
CMOR_VERSION=$(awk -F= '/^PACKAGE_VERSION=/ {print $2}' $CMOR_VERSION_LOC | sed "s/'//g")
echo_version cmor "$CMOR_VERSION" "$CMOR_VERSION_LOC"

HDFEOS5_VERSION_LOC="hdfeos5/include/HE5_HdfEosDef.h"
HDFEOS5_VERSION=$(awk '/HE5_HDFEOSVERSION/ {print $3}' $HDFEOS5_VERSION_LOC | sed 's/"//g')
echo_version hdfeos5 "$HDFEOS5_VERSION" "$HDFEOS5_VERSION_LOC"

SDPTK_VERSION_LOC="SDPToolkit/include/PGS_SMF.h"
SDPTK_VERSION=$(awk '/define PGSd_TOOLKIT_MAJOR_VERSION/ {print $3}' $SDPTK_VERSION_LOC | sed 's/"//g')
echo_version SDPToolkit "$SDPTK_VERSION" "$SDPTK_VERSION_LOC"
