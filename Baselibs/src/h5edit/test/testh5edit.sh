#! /bin/sh
#
# Copyright by The HDF Group.
# All rights reserved.
#
# Tests for the h5edit tool

TESTNAME=h5edit
EXIT_SUCCESS=0
EXIT_FAILURE=1

# Atomicity option test codes to indicate if backup file should pre and/or
# post exists when the tool is run. These are interpreted as bit fields.
BACKUP_EXIST_PRE=010		# pre-exist only
BACKUP_EXIST_POST=001		# post--exist only
BACKUP_EXIST_POST_DIFF=101	# post--exist only; different from original
BACKUP_EXIST_PRE_POST=011	# pre-exist and post-exist
BACKUP_EXIST_PRE_POST_DIFF=111	# pre-exist and post-exist; different from original
BACKUP_EXIST_NONE=000		# neither

H5EDIT=h5edit               # The tool name
H5EDIT_BIN=`pwd`/../src/$H5EDIT    # The path of the h5edit binary

DIFF='diff -c'
NLINES=20		    # Max. lines of output to display if test fails
H5DUMP=h5dump


TESTFILE=tmptestfile.h5	    # name of temporary HDF5 testfile
BACKUPFILE=.${TESTFILE}.bck # name of backup datafile
nerrors=0
verbose=yes
h5haveexitcode=yes	    # default is yes

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
    srcdir=.
fi
test -d ./testfiles || mkdir ./testfiles

# RUNSERIAL is used. Check if it can return exit code from executable correctly.
if [ -n "$RUNSERIAL_NOEXITCODE" ]; then
    echo "***Warning*** Serial Exit Code is not passed back to shell correctly."
    echo "***Warning*** Exit code checking is skipped."
    h5haveexitcode=no
fi

# Print a banner message
# $* the message
BANNER() {
    echo ==================================
    echo "$@"
    echo ==================================
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING() {
    SPACES="                                                               "
    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

# Print a "SKIP" message to show test is skipped.
SKIP() {
    TESTING $@
    echo  " -SKIP-"
}


# Copy datafile ($1) from testfile directory ($srcdir/testfiles) to
# testfile ($2) for testing. If $2 is not given, assume current directory and
# use same datafile name.
# Will not copy if testfile exists and is the same as datafile. This prevents
# copying datafile to itself.
COPYFILE() {
    datafile="$1"		# datafile
    testfile="$2"		# testfile
    if [ -z "$datafile" ]; then
	echo "COPYFILE: datafile argument missing"
	exit $EXIT_FAILURE
    fi
    if [ ! -r $datafile ]; then
	echo "COPYFILE: datafile not readable"
	exit $EXIT_FAILURE
    fi
    if [ -z "$testfile" ]; then
	# copy to current directory using same file name
	datafile=./`basename $datafile`
    fi
    # check if testfile exists and is the same
    $DIFF $datafile $testfile > /dev/null 2>&1
    if [ $? != 0 ]; then
	cp $datafile $testfile
    fi
    return 0
}


# Dump an HDF5 by using h5dump to display its content.
# But filter out the first line which contains the file
# name.  Therefore two different HDF5 files having
# the same content will produce the same output, par
# the file name.
# $1 is the file name
DUMP()
{
    $H5DUMP $1 | sed -e 1d
}


# Run a test and print PASS or *FAIL*. For now, if h5edit can complete
# with exit status 0, consider it pass. If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display up to $NLINES
# lines of the actual output from the test.  The actual output is not
# removed if $HDF5_NOCLEANUP has a non-zero value.
# Arguments:
# $1 -- Expected output filename to use, found in $srcdir/testfiles directory.
#    If it is "nocheck", no need to verify the output.
# $2 -- Expected exit code.
# $3 -- Backup_Exist_Code show if backup file should pre and/or post exist,
#       and whether it should be the same as or different from the original
#       file (POST-only).
# $4 and on -- argument for the h5edit tool.
#
H5EDITTEST()
{
    # Initial datafile is correct until found otherwise.
    data_correct=yes
    xTESTFILE=testfiles/$TESTFILE
    xBACKUPFILE=testfiles/$BACKUPFILE

    # file to hold dumps of original and new datafiles plus the backup file if
    # exists.
    xdump_orig=testfiles/dump_orig
    xdump_new=testfiles/dump_new
    xdump_backup=testfiles/dump_bak

    # Parse in the options.
    # Do we want to match output
    if [ $1 = "nocheck" ]; then
	nocheckfile="yes"
    else
    	nocheckfile=
    fi
    expect="$srcdir/testfiles/$1"
    h5ref="$srcdir/testfiles/`basename $1 .edit`.ref"
    result="./testfiles/`basename $1 .edit`.result"
    actual="./testfiles/`basename $1 .edit`.out"
    actual_err="./testfiles/`basename $1 .edit`.err"
    shift

    # What should be the tool exit code?
    retvalexpect=$1
    shift

    # What should the Backup_Exist_Code (BEC) be?
    BEC=$1
    shift
    # If backup file should pre-exist, create it. This is used to test
    # the tool would not overwrite pre-existing backup file.
    # If backup should exist afterward, save the test file to check
    # against original
    # If backup file should not exist, remove it before running the test
    # to make sure it wasn't left over from previous test.
    #FIXME: this is bash; doesn't work on bourne-only Solaris systems
    #bit1=${BEC:2:1}
    #bit2=${BEC:1:1}
    #bit3=${BEC:0:1}
    bit1=`expr "$BEC" : '[01][01]\([01]\)'`
    bit2=`expr "$BEC" : '[01]\([01]\)[01]'`
    bit3=`expr "$BEC" : '\([01]\)[01][01]'`

    if [ 1 = $bit2 ] ; then
	# create a dummy backup file
	touch $xBACKUPFILE
    else
	# Remove any backup file left from previous tests.
	rm -f $xBACKUPFILE
    fi
    if [ 1 = $bit1 ] ; then
	# Expect backup file after tool ends.
	# Save a dump of the original datafile
        DUMP $xTESTFILE > $xdump_orig
    fi
#    case "$BEC" in 
#	$BACKUP_EXIST_PRE | $BACKUP_EXIST_PRE_POST)
#	    # create a dummy backup file
#	    touch $xBACKUPFILE
#	    ;;
#	$BACKUP_EXIST_POST)
#	    # Expect backup file after tool ends.
#	    # Save a dump of the original datafile
#            DUMP $xTESTFILE > $xdump_orig
#	    # Remove any backup file left from previous tests.
#	    rm -f $xBACKUPFILE
#	    ;;
#	*)  # Remove any backup file left from previous tests.
#	    rm -f $xBACKUPFILE
#	    ;;
#    esac

    # Run test.
    # Stderr is included in stdout so that the diff can detect
    # any unexpected output from that stream too.
    TESTING $H5EDIT $@
    (
	cd testfiles
	$RUNSERIAL $H5EDIT_BIN "$@"
    ) >$actual 2>$actual_err 
    exitcode=$?
    cat $actual_err >> $actual 

    # check for existence of backup file
    case "$BEC" in 
	$BACKUP_EXIST_PRE | $BACKUP_EXIST_PRE_POST)
	    # pre-exist backup file should still exist
	    if [ ! -f $xBACKUPFILE ]; then
		echo "*failed*"
		nerrors="`expr $nerrors + 1`"
		if [ yes = "$verbose" ]; then
		    echo "pre-exist backup file ($xBACKUPFILE) does not exist any more"
		fi
	    fi
	    ;;
	 $BACKUP_EXIST_POST | $BACKUP_EXIST_POST_DIFF) 
	    # expect backup file after tool ends
	    if [ ! -f $xBACKUPFILE ]; then
		echo "*failed*"
		nerrors="`expr $nerrors + 1`"
		if [ yes = "$verbose" ]; then
		    echo "Expected backup file ($xBACKUPFILE) not found"
		fi
	    else
		# Dump a copy of the backup file and the new datafile
		DUMP $xBACKUPFILE > $xdump_backup
		DUMP $xTESTFILE > $xdump_new
		# Determine whether the backup dump is the same as the original dump.
		# That shows the backup file is either a complete backup of the original
                # or in the case of incremental atomicity if it should be different.
		$DIFF $xdump_orig $xdump_backup > /dev/null 2>&1
                if [ "$BEC" = "$BACKUP_EXIST_POST_DIFF" ]; then
                    diff=1;
                else
                    diff=0;
                fi
		if [ "$?" != "$diff" ]; then
		    echo "*failed*"
		    nerrors="`expr $nerrors + 1`"
		    if [ yes = "$verbose" ]; then
			echo backup file is in wrong state compared to original datafile
		    fi
		fi
		# skip the diffing for now
		#$DIFF $xdump_orig $xdump_new > /dev/null 2>&1
		#if [ "$?" -eq 0 ]; then
		#    echo new datafile is same as the original datafile
		#fi
	    fi
	    ;;
	$BACKUP_EXIST_NONE)
	    # expect no backup file after tool ends
	    if [ -f $xBACKUPFILE ]; then
		echo "*failed*"
		nerrors="`expr $nerrors + 1`"
		if [ yes = "$verbose" ]; then
		    echo "Found unexpected backup file ($xBACKUPFILE)"
		fi
	    fi
	    ;;
    esac

    # Verify expected exitcode.
    if [ $h5haveexitcode = 'yes' -a $exitcode -ne $retvalexpect ]; then
	echo "*FAILED*"
	nerrors="`expr $nerrors + 1`"
	if [ yes = "$verbose" ]; then
	    echo "test returned with exit code $exitcode"
	    echo "test output: (up to $NLINES lines)"
	    head -$NLINES $actual
	    echo "***end of test output***"
	    echo ""
	fi
    elif test "$nocheckfile" != ""; then
	# not checking against any expected file
        echo " PASSED"
    else
	# Otherwise, compare the output to the expected file
        # if there's a datafile, dump it to a text file for diff against the expected file.
        if [ -f testfiles/$TESTFILE ]; then
            $H5DUMP testfiles/$TESTFILE > $result
            $DIFF $h5ref $result > /dev/null 2>&1
            if [ "$?" != 0 ]; then
                data_correct=no
            fi
        fi
        if test -f $expect && test -f $actual; then
            $DIFF $expect $actual > /dev/null 2>&1
            if [ "$?" = "0" ]; then
                if [ "$data_correct" = "no" ]; then
                        echo "*FAILED*"
        	        echo "    Expected result differs from actual result"
        	        nerrors="`expr $nerrors + 1`"
    	            test yes = "$verbose" && $DIFF $h5ref $result |sed 's/^/    /' \
                                          && echo ""
                else
                    echo " PASSED"
                    if [ -f testfiles/$TESTFILE ]; then
                       rm testfiles/$TESTFILE
                    fi
                fi
            else
                echo " *FAILED*"
                echo "    Expected output differs from actual output"
                nerrors="`expr $nerrors + 1`"
                test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
                if [ "$data_correct" = "no" ]; then
    	        echo "    Expected result differs from actual result"
    	        test yes = "$verbose" && $DIFF $h5ref $result |sed 's/^/    /' \
                                      && echo ""
                fi           
            fi
        else
            echo " *FAILED*"
            test yes = "$verbose" && echo "$expect not found" && echo ""
            nerrors="`expr $nerrors + 1`"
        fi
    fi

    # Clean up output file
    if test -z "$HDF5_NOCLEANUP"; then
        rm -f $xTESTFILE $xBACKUPFILE
        rm -f $xdump_orig $xdump_new $xdump_backup
	rm -f $actual $actual_err $result
    fi
}

##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

# test the help syntax - valid
BANNER Verify -h, --help and --version
H5EDITTEST help-1.edit 0 $BACKUP_EXIST_NONE "-h"
H5EDITTEST help-1.edit 0 $BACKUP_EXIST_NONE "--help"

# test data file not in current directory (i.e., it has a path component)
# remove test files afterward
BANNER Make sure we can specify a data file in a different directory
test -d ./testfiles/more || mkdir ./testfiles/more
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/more/$TESTFILE
H5EDITTEST nocheck 0 \
    $BACKUP_EXIST_NONE \
    -c "DELETE /g1/g1.1/dset1.1.1 attr1;" more/$TESTFILE

if test -z "$HDF5_NOCLEANUP"; then
        rm -rf testfiles/more
fi

# test version information
H5EDITTEST version.edit 0 $BACKUP_EXIST_NONE "--version"

# test the help syntax - incomplete 
BANNER Verify rejecting bad options
H5EDITTEST missing-command.edit 1 $BACKUP_EXIST_NONE ""
H5EDITTEST missing-command-arg.edit 1 $BACKUP_EXIST_NONE "-c"
H5EDITTEST missing-command-arg.edit 1 $BACKUP_EXIST_NONE "--command"
H5EDITTEST missing-command-file-arg.edit 1 $BACKUP_EXIST_NONE "--command-file"
H5EDITTEST missing-command.edit 1 $BACKUP_EXIST_NONE "--dryrun"
H5EDITTEST unknownoption-1.edit 1 $BACKUP_EXIST_NONE "-help"

# test the help syntax - invalid
H5EDITTEST unknownoption-2.edit  1 $BACKUP_EXIST_NONE "-x"
H5EDITTEST unknownoption-3.edit  1 $BACKUP_EXIST_NONE "--bogus"
H5EDITTEST missing-command.edit  1 $BACKUP_EXIST_NONE "%@bogus_command@%"
H5EDITTEST missing-command.edit 1 $BACKUP_EXIST_NONE '`pwd`'

# non-existent command file
H5EDITTEST bad-command-file.edit 1 $BACKUP_EXIST_NONE --command-file ./no_such_file.h5

# test simple legal commands
BANNER Test DELETE commands
# Copy the datafile and test; 5.2.3.1
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST delete-atts12-dset111.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "DELETE /g1/g1.1/dset1.1.1 attr1; \
        DELETE DATASET /g1/g1.1/dset1.1.1 attr2;" \
    $TESTFILE

# Repeat with single combined target-object/attribute syntax.
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST delete-atts12-dset111.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "DELETE /g1/g1.1/dset1.1.1/attr1; \
        DELETE DATASET /g1/g1.1/dset1.1.1/attr2;" \
    $TESTFILE

# Test DELETE from Group
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST delete-atts12-grrt.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "DELETE GROUP / attr1; DELETE GROUP / attr2;" $TESTFILE

# Repeat with single combined target-object/attribute syntax.
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST delete-atts12-grrt.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "DELETE /attr1; DELETE /attr2;" $TESTFILE

BANNER Test RENAME commands
# Copy the datafile and test
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST rename-atts12-dset111.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "RENAME /g1/g1.1/dset1.1.1 attr1 attr1_new; \
        RENAME DATASET /g1/g1.1/dset1.1.1 attr2 attr2_new;" \
    $TESTFILE

# Test RENAME Group attributes
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST rename-atts12-grrt.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "RENAME GROUP / attr1 attr1_new; RENAME GROUP / attr2 attr2_new;" $TESTFILE

# Repeat with single combined target-object/attribute syntax.
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST rename-atts12-dset111.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "RENAME /g1/g1.1/dset1.1.1/attr1 attr1_new; \
        RENAME DATASET /g1/g1.1/dset1.1.1/attr2 attr2_new;" \
    $TESTFILE

# Repeat GROUP attributes test with single combined target-object/attribute syntax.
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST rename-atts12-grrt.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "RENAME GROUP /attr1 attr1_new; RENAME GROUP /attr2 attr2_new;" $TESTFILE

# Test bad rename commands; expected to fail
# rename from nonexisting attribute
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST rename-bogusattr.edit 1 \
    $BACKUP_EXIST_NONE \
    --atomic no -c "RENAME GROUP /g1 attrbogus attr_new;" $TESTFILE

# rename from group with no GROUP keyword
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST rename-nogroupkeyword.edit 1 \
    $BACKUP_EXIST_NONE \
    --atomic no -c "RENAME /g1 attr2 attr2renamed;" $TESTFILE

# rename with bad destination parameter
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST rename-baddest.edit 1 \
    $BACKUP_EXIST_NONE \
    --atomic no -c "RENAME GROUP /g1 attr2 /g2;" $TESTFILE

# Test CREATE Command
BANNER Test CREATE commands
# Test CREATE SCALAR integer attibutes
# This also checks the validity of the keywords DATATYPE, DATASPACE, DATA.
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-newatts-I3264BE.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE /g1/g1.1/dset1.1.1 new_attrI32BE {DATATYPE H5T_STD_I32BE DATASPACE SCALAR DATA {32}} ; \
	CREATE /g1/g1.1/dset1.1.1 new_attrI64BE {DATATYPE H5T_STD_I64BE DATASPACE SCALAR DATA {64}} ;" \
    $TESTFILE

# Test CREATE SCALAR float attibutes
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-newatts-F32BE64LE.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE /g1/g1.1/dset1.1.1 new_attrF32BE { H5T_IEEE_F32BE SCALAR {32.32}} ; \
	CREATE /g1/g1.1/dset1.1.1 new_attrF32LE { H5T_IEEE_F32LE SCALAR {64.64}} ;" \
    $TESTFILE

# Repeat with single combined target-object/attribute syntax.
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-newatts-F32BE64LE.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE /g1/g1.1/dset1.1.1/new_attrF32BE { H5T_IEEE_F32BE SCALAR {32.32}} ; \
	CREATE /g1/g1.1/dset1.1.1/new_attrF32LE { H5T_IEEE_F32LE SCALAR {64.64}} ;" \
    $TESTFILE

# Test CREATE Simple Atributes for integer and float attributes
# This also checks the validity of the keyword SIMPLE.
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-newatts-F64LEI32BE.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE /g1/g1.1/dset1.1.1 newattrI32BE {H5T_STD_I32BE SIMPLE ( 1, 2, 3) {11, 12, 21, 22, 31,32}}; \
CREATE /g1/g1.1/dset1.1.1 newattrF64LE {H5T_IEEE_F64LE SIMPLE (2, 2, 2 ) {0.1E1, 0.2E1, -1.1e2, -1.2e2, 2.1E-3, 2.2E-3, -3.1E-3, -3.2E-2}} ;" $TESTFILE

# Test Command file containing the same CREATE commands as above.
# Should have the same result as above.
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
COPYFILE $srcdir/testfiles/t_comm_file1 testfiles/t_comm_file1
H5EDITTEST create-newatts-F64LEI32BE.edit 0 \
    $BACKUP_EXIST_NONE \
    --command-file t_comm_file1 $TESTFILE

# Test CREATE SCALAR dataspace attributes with integer datatypes
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-intattsscalar.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE GROUP / newattrI8LE {H5T_STD_I8LE SCALAR {8}} ; CREATE GROUP /g1 newattrI8BE {H5T_STD_I8BE SCALAR {8}} ; CREATE GROUP /g1/g1.1 newattrI16LE {H5T_STD_I16LE SCALAR {16}} ; CREATE /g1/g1.1/dset1.1.1 newattrI16BE {H5T_STD_I16BE SCALAR {16}} ; CREATE /g1/g1.1/dset1.1.2 newattrI32LE {H5T_STD_I32LE SCALAR {32}} ; CREATE GROUP /g1/g1.2 newattrI32BE {H5T_STD_I32BE SCALAR {32}} ; CREATE GROUP /g1/g1.2/g1.2.1 newattrI64LE {H5T_STD_I64LE SCALAR {64}} ; CREATE /g2/dset2.2 newattrI64BE {H5T_STD_I64BE SCALAR {64}} ; " $TESTFILE

# Test CREATE SIMPLE dataspace attributes with integer datatypes
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-intattssimple.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE GROUP /g1/g1.1 newattrI8LE {H5T_STD_I8LE ( 1, 2, 3) {2, 4, 8, 16, 32, 64}} ; CREATE /g1/g1.1/dset1.1.1 newattrI8BE {H5T_STD_I8BE (2, 2, 2 ) {-2, 0, 2, 4, 8, 16, 32, 64}} ; CREATE /g1/g1.1/dset1.1.2 newattrI16LE {H5T_STD_I16LE ( 1, 2, 3)  {64, 128, 256, 512, 1024, 2048}} ; CREATE GROUP /g1/g1.2 newattrI16BE {H5T_STD_I16BE (2, 2, 2 )  {64, 128, 256, 512, 1024, 2048, 4096, 8192}} ; CREATE GROUP /g1/g1.2/g1.2.1 newattrI32LE {H5T_STD_I32LE ( 1, 2, 3)  {2048, 4096, 8192, 16384, 32768, 65536}} ; CREATE GROUP /g2 newattrI32BE {H5T_STD_I32BE (2, 2, 2 )  {2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144}} ;CREATE /g2/dset2.1 newattrI64LE {H5T_STD_I64LE ( 1, 2, 3)  {65536, 131072, 262144, 524288, 1048576, 2097152}} ; CREATE /g2/dset2.2 newattrI64BE {H5T_STD_I64BE (2, 2, 2 )  {65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608}} ; " $TESTFILE

# Test CREATE SCALAR dataspace attributes with unsigned integer datatypes
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-unsignedattsscalar.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE GROUP / newattrU8LE {H5T_STD_U8LE SCALAR {8}} ; CREATE GROUP /g1 newattrU8BE {H5T_STD_U8BE SCALAR {8}} ; CREATE GROUP /g1/g1.1 newattrU16LE {H5T_STD_U16LE SCALAR {16}} ; CREATE /g1/g1.1/dset1.1.1 newattrU16BE {H5T_STD_U16BE SCALAR {16}} ; CREATE /g1/g1.1/dset1.1.2 newattrU32LE {H5T_STD_U32LE SCALAR {32}} ; CREATE GROUP /g1/g1.2 newattrU32BE {H5T_STD_U32BE SCALAR {32}} ;CREATE GROUP /g1/g1.2/g1.2.1 newattrU64LE {H5T_STD_U64LE SCALAR {64}} ; CREATE /g2/dset2.2 newattrU64BE {H5T_STD_U64BE SCALAR {64}} ; " $TESTFILE

# Test CREATE unsigned integer datatypes
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-unsignedattssimple.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE GROUP /g1/g1.1 newattrU8LE {H5T_STD_U8LE ( 1, 2, 3)  {2, 4, 8, 16, 32, 64}} ; CREATE /g1/g1.1/dset1.1.1 newattrU8BE {H5T_STD_U8BE (2, 2, 2 )  {0, 2, 4, 8, 16, 32, 64, 128}} ; CREATE /g1/g1.1/dset1.1.2 newattrU16LE {H5T_STD_U16LE ( 1, 2, 3)  {64, 128, 256, 512, 1024, 2048}} ; CREATE GROUP /g1/g1.2 newattrU16BE {H5T_STD_U16BE (2, 2, 2 )  {64, 128, 256, 512, 1024, 2048, 4096, 8192}} ; CREATE GROUP /g1/g1.2/g1.2.1 newattrU32LE {H5T_STD_U32LE ( 1, 2, 3)  {2048, 4096, 8192, 16384, 32768, 65536}} ; CREATE GROUP /g2 newattrU32BE {H5T_STD_U32BE (2, 2, 2 )  {2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144}} ;CREATE /g2/dset2.1 newattrU64LE {H5T_STD_U64LE ( 1, 2, 3)  {65536, 131072, 262144, 524288, 1048576, 2097152}} ; CREATE /g2/dset2.2 newattrU64BE {H5T_STD_U64BE (2, 2, 2 )  {65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608}} ; " $TESTFILE

# Test CREATE SCALAR dataspace attributes with float datatypes
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-floatattsscalar.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE /g1/g1.1/dset1.1.1 newattrF32BE {H5T_IEEE_F32BE SCALAR {32.32}} ; CREATE GROUP /g1/g1.2 newattrF64BE {H5T_IEEE_F64BE SCALAR {64.64}} ; CREATE /g1/g1.1/dset1.1.1 newattrF32LE {H5T_IEEE_F32LE SCALAR {32.32}} ; CREATE GROUP /g1/g1.2 newattrF64LE {H5T_IEEE_F64LE SCALAR {64.64}} ;" $TESTFILE

# Test CREATE dataspace attributes with float datatypes
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-floatattssimple.edit 0  \
    $BACKUP_EXIST_NONE \
    -c "CREATE /g1/g1.1/dset1.1.1 newattrF32BE {H5T_IEEE_F32BE ( 1, 2, 3)  {11.1111, 12.1212, 21.2121, 22.2222, 31.3131, 32.3232}}; CREATE GROUP /g1/g1.2 newattrF32LE {H5T_IEEE_F32LE ( 1, 2, 3)  {11.1111, 12.1212, 21.2121, 22.2222, 31.3131, 32.3232}}; CREATE /g1/g1.1/dset1.1.1 newattrF64BE {H5T_IEEE_F64BE (2, 2, 2 )  {0.1E1, 0.2E1, -1.1e2, -1.2e2, 2.1E-3, 2.2E-3, -3.1E-3, -3.2E-2}} ; CREATE GROUP /g1/g1.2 newattrF64LE {H5T_IEEE_F64LE (2, 2, 2 )  {0.1E1, 0.2E1, -1.1e2, -1.2e2, 2.1E-3, 2.2E-3, -3.1E-3, -3.2E-2}} ;" $TESTFILE

# Test CREATE SCALAR and STRING type attributes
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-strings.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE /g1/g1.1/dset1.1.1 newattrScalarString {H5T_STRING { STRSIZE 15 } SCALAR  {'scalar string'}}; CREATE /g1/g1.1/dset1.1.1 newattrArrayString {H5T_STRING { STRSIZE 10 } ( 3 )  {'a', 'string', 'array'}}; " $TESTFILE

BANNER Test COPY command
#test COPY to another attribute name in same dataset/group, four parameters
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST copy-attr-object-same.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "COPY / attr1 / attr1copy;" $TESTFILE

#test COPY to another attribute name in same dataset/group, two parameters
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST copy-attr-object-same.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "COPY /attr1 /attr1copy;" $TESTFILE

#test COPY to another attribute name in same dataset/group, two parameters, implied target object
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST copy-attr-object-same.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "COPY /attr1 attr1copy;" $TESTFILE

#test COPY from one dataset/group to another, different attribute name, four parameters
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST copy-attr-object-different.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "COPY / attr1 /g2/dset2.1 attr1copy;" $TESTFILE

#test COPY from one dataset/group to another, different attribute name, two parameters
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST copy-attr-object-different.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "COPY /attr1 /g2/dset2.1/attr1copy;" $TESTFILE

#test COPY from one dataset/group to another, same attribute name, four parameters
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST copy-attr-object-different-same.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "COPY / attr1 /g2/dset2.1 attr1;" $TESTFILE

#test COPY from one dataset/group to another, different attribute name, two parameters
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST copy-attr-object-different-same.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "COPY /attr1 /g2/dset2.1/attr1;" $TESTFILE

#test COPY from one dataset/group to another, leading slash attribute name
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST copy-attr-object-different2.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "COPY /attr1 /g2/dset2.1//attr1copy;" $TESTFILE

#test COPY from one dataset/group to another, different form of leading slash attribute name
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST copy-attr-object-different2.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "COPY /attr1 /g2/dset2.1'/attr1copy';" $TESTFILE

#tests for COPY that should fail
#test COPY to same attribute name in same dataset/group, four parameters
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST nocheck 1 \
    $BACKUP_EXIST_POST \
    -c "COPY / attr1 / attr1;" $TESTFILE

#test COPY to same attribute name in same dataset/group, two parameters
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST nocheck 1 \
    $BACKUP_EXIST_NONE \
    -atomic no -c "COPY /attr1 /attr1;" $TESTFILE

#test COPY to same attribute name in same dataset/group, two parameters, implied target object
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST nocheck 1 \
    $BACKUP_EXIST_NONE \
    -atomic no -c "COPY /attr1 attr1;" $TESTFILE

#test COPY with three parameters
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST nocheck 1 \
    $BACKUP_EXIST_NONE \
    -atomic no -c "COPY /attr1  / attr2;" $TESTFILE

#test COPY with fully specified parameters
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST nocheck 1 \
    $BACKUP_EXIST_NONE \
    -atomic no -c "COPY GROUP / ATTRIBUTE attr1  GROUP / ATTRIBUTE attr2;" $TESTFILE

# test MODIFY commands
BANNER Test MODIFY commands
# Test MODIFY SCALAR integer attibutes
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST modify-atts12-dset111.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "MODIFY /g1/g1.1/dset1.1.1 attr1 {\
	    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, \
	    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, \
	    21, 22, 23, 24, 25, 26, 27}; \
	MODIFY /attr2 {11, 13, 17, 19}; \
	MODIFY /g1/g1.1/dset1.1.1/attr2 {\
	    27, 26, 25, 24, 23, 22, 21, \
	    20, 19, 18, 17, 16, 15, 14, 13, 12, 11, \
	    10, 9, 8, 7, 6, 5, 4, 3, 2, 1}; \
	" \
    $TESTFILE
# Test MODIFY STRING attibutes
# Has to create the string ttributes first because the test datafile does
# not have any string attributes.
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST modify-strings.edit 0 \
    $BACKUP_EXIST_NONE \
    -c "CREATE /g1/g1.1/dset1.1.1 newattrScalarString {H5T_STRING { STRSIZE 15 } SCALAR  {'scalar string'}}; \
    CREATE /g1/g1.1/dset1.1.1/newattrArrayString {H5T_STRING { STRSIZE 10 } ( 3 )  {'a', 'string', 'array'}}; \
    MODIFY /g1/g1.1/dset1.1.1 newattrScalarString {'single string'}; \
    MODIFY /g1/g1.1/dset1.1.1/newattrArrayString {'array', 'of', 'strings'}; \
    " $TESTFILE

# The following commands should fail.  They should return 1 with error messages.
BANNER Verify it rejects bad commands.
# Test CREATE Command for existing attribute
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-existingattr.edit 1 \
    $BACKUP_EXIST_POST \
    -c "CREATE GROUP / attr1 {H5T_STD_I8BE (1, 10) { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 }} ;" $TESTFILE

# Test CREATE command for non-exiting object
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST create-attr-missingdset.edit 1 \
    $BACKUP_EXIST_POST \
    -c "CREATE /g1/bogusdset1.1 attr1 {H5T_STD_I8BE (1, 10) { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 }} ;" $TESTFILE

# Test DELETE command for non-existing attribute
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST delete-bogusattr.edit 1 \
    $BACKUP_EXIST_POST \
    -c "DELETE /g1/g1.1/dset1.1.1 bogusattr;" $TESTFILE

# Test DELETE command for non-existing object
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST delete-bogusgroup.edit 1 \
    $BACKUP_EXIST_POST \
    -c "DELETE GROUP /g1/g1.1000 attr1;" $TESTFILE
 
# Test atomicity option.
# 1. missing level argument.
# 2. bad level argument.
# 3. Inc level, Yes level, No level.
BANNER Verify --atomic option
H5EDITTEST missing-atomic-arg.edit 1 $BACKUP_EXIST_NONE --atomic
H5EDITTEST bad-atomic-arg.edit 1 $BACKUP_EXIST_NONE --atomic bad

# The following commands will fail. Will verify --atomic can produce a backup file.
# /attr1 exists, but /attr_no_such does not.
# Verify default --atomic is yes. 5.1.1 & 5.2.1
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST nocheck 1 \
    $BACKUP_EXIST_POST \
    -c "DELETE /attr1; DELETE /attr_no_such;" $TESTFILE
# Use --atomic yes. Should get the same result.
# Check resulting file against test file. 5.2.1.4
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST delete-atomic-yes-fail.edit 1 \
    $BACKUP_EXIST_POST \
    --atomic yes \
    -c "DELETE /attr1; DELETE /attr_no_such;" $TESTFILE
# Use --atomic inc.
# 5.2.3.2
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST nocheck 1 \
    $BACKUP_EXIST_POST \
    --atomic inc \
    -c "DELETE /attr1; DELETE /attr_no_such;" $TESTFILE
# Use --atomic no. Should not find any backup file. 5.2.2.1
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST delete-atomic-no-good.edit 0 \
    $BACKUP_EXIST_NONE \
    --atomic no \
    -c "DELETE /attr1;" $TESTFILE
# Use --atomic no. Should not find any backup file.
# add check against created file. 5.2.2.2
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST delete-atomic-no-fail.edit 1 \
    $BACKUP_EXIST_NONE \
    --atomic no \
    -c "DELETE /attr1; DELETE /attr_no_such;" $TESTFILE

# Use --atomic inc. Same as atomic no but backup file should exist.
# add check against created file (5.2.3.2.7)
COPYFILE $srcdir/testfiles/datafile1.h5 testfiles/$TESTFILE
H5EDITTEST delete-atomic-no-fail.edit 1 \
    $BACKUP_EXIST_POST \
    --atomic inc \
    -c "DELETE /attr1; DELETE /attr_no_such;" $TESTFILE

#=======================
# Post results and exit.
#=======================
BANNER Results
if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi
