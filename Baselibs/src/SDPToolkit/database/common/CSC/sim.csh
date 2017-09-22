#! /bin/csh -f

# This script creates a file called utcpole.bogus based on the existing file
# utcpole.dat but with all dates in the file utcpole.dat increased by 3000
# days.  This is a quick way to generate a file for use with the SDP Toolkit
# that will allow simulated processing of times in the near future (which are
# not normally supported).

# !!!! WARNING !!!! WARNING !!!! WARNING !!!! WARNING !!!! WARNING !!!!

# The data file generated by this script is ONLY for use in testing software
# functionality at future dates.  It should NOT be used for processing data or
# doing prediction work of any kind.  Results obtained using data from the file
# utcpole.bogus should be considered completely inaccurate.

# Users are strongly advised to NOT rename the resulting output file of this
# script to utcpole.dat.  The preferred method of accessing this file is to
# replace the line in the PCFs of USERS INTERESTED IN USING THESE SIMULATED
# TIMES that refers to utcpole.dat with a reference to utcpole.bogus.
# Example
# change: 10401|utcpole.dat|~/database/common/CSC||||1 to:
# 10401|utcpole.bogus|~/database/common/CSC||||1

if ( $?PGSHOME ) then
    set input_file=${PGSHOME}/database/common/CSC/utcpole.dat
    set output_file=${PGSHOME}/database/common/CSC/utcpole.bogus
else
    set input_file=utcpole.dat
    set output_file=utcpole.bogus
endif

if ( ! -f $input_file ) then
    echo ""
    echo unable to locate input file':' $input_file ... aborting
    echo ""
endif

sed "s/^54/57/" $input_file | \
sed "s/^53/56/" | \
sed "s/^52/55/" | \
sed "s/^51/54/" | \
sed "s/^50/53/" | \
sed "s/^49/52/" | \
sed "s/^48/51/" | \
sed "s/^47/50/" | \
sed "s/^46/49/" | \
sed "s/^45/48/" | \
sed "s/^44/47/" | \
sed "s/^43/46/" | \
sed "s/^42/45/" | \
sed "s/^41/44/" | \
sed "s/^40/43/" | \
sed "s/^39/42/" | \
sed "s/^38/41/" | \
sed "s/^37/40/" >! $output_file
