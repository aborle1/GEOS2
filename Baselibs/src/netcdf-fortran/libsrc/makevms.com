$! --------------------------------------------------------------------------
$! For making FTEST.EXE on VMS
$! --------------------------------------------------------------------------
$!
$! $Id$
$
$ ccc := cc /opt/nodebug/nolist/include=([-.src])
$
$ ccc JACKETS.C
$ fort FTEST.FOR
$
$ link/nodebug/notraceback/exec=FTEST.exe -
    ftest.obj, -
    jackets.obj, -
    [---.lib]netcdf.olb/lib, -
    sys$input/opt
	sys$library:vaxcrtl.exe/share
$
$ run ftest
$
