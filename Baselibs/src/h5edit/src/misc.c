/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of h5edit. The full h5edit copyright notice, including  *
 * terms governing use, modification, and redistribution, is contained in    *
 * the file COPYING, which can be found at the root of the source code       *
 * distribution tree. If you do not have access to this file, you may        *
 * request a copy from help@hdfgroup.org.                                    *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#include <h5edit.h>

/* Local variables */
static int h5edit_debug = 0;	    /* debug setting, default no */

/* A glocal cheat variable */
char *backupname_g = NULL;


/* Initialize the tool.
 * Setup debug mode.
 * Register backup virtual file driver.
 * Return: void
 *   If error encountered during init, will report and leave(EXIT_FAILURE).
 */
void
init(void)
{
    set_debug();
    /* register the backup VFD */
    if (H5FD_backup_init() < 0 ){
	fprintf(stderr, "Failed to initialze Backup VFD\n");
	leave(EXIT_FAILURE);
    }
}


/* Terminate the tool.
 * If leave_code is EXIT_SUCCESS (normal exit), close commfile (if open) and
 * the datafile, close and remove the backup file, then exit the tool with
 * EXIT_SUCCESS.
 * If leave_code is EXIT_FAILURE (errors encounter), close commfile (if open)
 * but not closing the datafile. Close the backup file and do not remove it
 * so that it can be available for recovery. Then exit the tool with
 * EXIT_FAILURE.
 */
void
leave(int leave_code){
     herr_t dfclosecode;
     int exitcode=leave_code;

    /* close all opened files */
    /* Command file */
    if (commfile_g != NULL)
    	HDfclose(commfile_g);
    /* Data file */
    if (datafile_g >= 0){
	if ((dfclosecode=H5Fclose(datafile_g)) < 0){
	    fprintf(stderr, "Failed to close HDF5 datafile\n");
	    /* remember the error, need to continue to close the backup file */
	    exitcode=EXIT_FAILURE;
	}else{
	}
	datafile_g=-1;
    }

    /* if atomicity is on, close backup file and the datafile readonly handle */
    if (atomic_level_g != No_atomic){
	/* If exitcode is success, remove backup file; else leave it
	 * for recovery. */
	if (exitcode==EXIT_SUCCESS){
if (get_debug())
    fprintf(stderr, "removing backup file %s\n", backupname_g);
	    HDremove(backupname_g);
	}
	HDfree(backupname_g);
    }

    /* quit tool*/
    exit(exitcode);
}


/* Parse command line options.
 * Return if all is okay;
 *     otherwise, print error message and leave(EXIT_FAILURE)
 *     which does not return.
 */
void
parse_option(int ac, const char** av)
{
    int argno;
    const char *s = NULL;

    /* parse in options */
    for (argno = 1; argno < ac && '-' == av[argno][0]; argno++){
	if (streq(av[argno], "--help") || streq(av[argno], "-h")){
	    usage();
	    leave(EXIT_SUCCESS);
	}else if(streq(av[argno], "--dryrun")){
	    dryrun_g = TRUE;
	}else if(streq(av[argno], "--command") || streq(av[argno], "-c")){
	    if (commfilename_g || command_g){	/* --commfile or --commfile option given already */
		fprintf(stderr, "%s: only one of --command or --command-file is allowed\n",
		    progname_g);
		usage();
		leave(EXIT_FAILURE);
	    }
	    if ((argno + 1) >= ac){
		fprintf(stderr, "%s: --command argument missing\n",
		    progname_g);
		usage();
		leave(EXIT_FAILURE);
	    }
	    command_g = av[++argno];
	}else if(streq(av[argno], "--command-file")){
	    if (commfilename_g || command_g){	/* --commfile or --commfile option given already */
		fprintf(stderr, "%s: only one of --command or --command-file is allowed\n",
		    progname_g);
		usage();
		leave(EXIT_FAILURE);
	    }
	    if ((argno + 1) >= ac){
		fprintf(stderr, "%s: --command-file argument missing\n",
		    progname_g);
		usage();
		leave(EXIT_FAILURE);
	    }
	    commfilename_g = av[++argno];
	    if ((commfile_g=opencommfile(commfilename_g))==NULL){
		perror(progname_g);
		fprintf(stderr, "%s: could not open command file (%s)\n",
		    progname_g, commfilename_g);
		usage();
		leave(EXIT_FAILURE);
	    }
	}else if(streq(av[argno], "--atomic")){
	    if ((argno + 1) >= ac){
		fprintf(stderr, "%s: --atomic argument missing\n",
		    progname_g);
		usage();
		leave(EXIT_FAILURE);
	    }
	    /* assign atomic level */
	    ++argno;
	    if(streq(av[argno], "inc")){
		atomic_level_g = Inc_atomic;
	    }else if(streq(av[argno], "no")){
		atomic_level_g = No_atomic;
	    }else if(streq(av[argno], "yes")){
		atomic_level_g = Yes_atomic;
	    }else{	/* unrecognized atomic level */
		fprintf(stderr, "%s: unrecognized atomic level (%s)\n",
		    progname_g, av[argno]);
		usage();
		leave(EXIT_FAILURE);
	    }
	}else if(streq(av[argno], "--version")){
	    print_version();
	    leave(EXIT_SUCCESS);
	} else {
	    fprintf(stderr, "%s: Unknown option (%s)\n",
		progname_g, av[argno]);
	    usage();
	    leave(EXIT_FAILURE);
	}
    } /* end for */

    /* check if command or command-file is provided. */
    if (command_g==NULL && commfilename_g==NULL){
	fprintf(stderr, "%s: missing command argument\n", progname_g);
	usage();
	leave(EXIT_FAILURE);
    }

    /* look for datafile argument */
    if (argno >= ac){
	fprintf(stderr, "%s: missing datafile argument\n", progname_g);
	usage();
	leave(EXIT_FAILURE);
    }
    datafilename_g = av[argno++];
    
    if ((opendatafile(datafilename_g)) < 0){
	perror(progname_g);
	fprintf(stderr, "%s: could not open data file (%s)\n",
	    progname_g, datafilename_g);
	usage();
	leave(EXIT_FAILURE);
    }

    /* Turn off HDF5's automatic error printing. */
    /* Should have an option to allow automatic error printing for debugging. */
    if (H5Eset_auto2(H5E_DEFAULT, NULL, NULL) < 0){
	fprintf(stderr, "%s: failed to turn off automatic error printing\n", progname_g);
	leave(EXIT_FAILURE);
    }

    return;
}

void
usage(void)
{
    fprintf(stderr, "usage:\n");
    fprintf(stderr, "%s [-h | --help] h5file\n", progname_g);
    fprintf(stderr, "%s [OPTIONS] h5file\n", progname_g);
    fprintf(stderr, 
	"OPTIONS\n" \
	"   -h, --help\n"
	"       Print a usage message and exit\n"
	"   --atomic level:\n"
	"       Specifies the atomic level:\n"
	"       yes: This is the default. It means the changes must be done as all or\n"
	"            nothing. The original data file is restored in case of any command\n"
	"            failures.\n"
	"       no:  No atomicity is desired. Do as much changes as possible.\n"
	"       inc: Atomicity of changes at individual command level is desired, not\n"
	"            the entire execution.\n"
	"   -c command, --command command:\n"
	"       Specify an H5edit command to apply to the file h5file.\n"
	"   --command-file commfile\n"
	"       Specify a command file, commfile, that contains H5edit commands written\n"
	"       in the H5edit Command Language, to apply to the file h5file.\n"
	"   --dryrun\n"
	"       (To be supported in future implementation)\n"
	"       Just check the syntax of the H5edit commands against the HDF5 file\n"
	"       without making the actual changes to the HDF5 file.\n"
	"   --version\n"
	"       Print the version information of h5edit.\n"
	);
    fprintf(stderr, "\n");

} /* usage() */


/* In the future, data file is open with atomicity in mind. For now, just
 * H5Fopen it.
 * Return 0 succeess, otherwise -1.
 */
int opendatafile(const char *name)
{
    int ret_code=0;
    hid_t fapl = H5P_DEFAULT;

    /* setup to use backup VFD if atomic is choosen */
    if (atomic_level_g != No_atomic){
	/* it is either Yes_atomic or Inc_atomic */
	if ((fapl=H5Pcreate(H5P_FILE_ACCESS))<0){
	    fprintf(stderr, "%s: could not H5Pcreate\n", progname_g);
	    leave(EXIT_FAILURE);
	}
	if (H5Pset_fapl_backup(fapl)<0){
	    fprintf(stderr, "%s: could not H5Pset_fapl_backup\n", progname_g);
	    leave(EXIT_FAILURE);
	}
    }

    if ((datafile_g=H5Fopen(name, H5F_ACC_RDWR, fapl))<0){
	fprintf(stderr, "%s: could not open data file (%s)\n",
		progname_g, name);
	leave(EXIT_FAILURE);
    };
    H5Pclose(fapl);

    return(ret_code);
}

/* Open the command file */
FILE *opencommfile(const char *name)
{
    FILE *ret_code;

    if ((ret_code=HDfopen(name, "r"))==NULL){
	fprintf(stderr, "%s: could not open command file (%s)\n",
		progname_g, name);
	leave(EXIT_FAILURE);
    };
    return(ret_code);
}

/*-------------------------------------------------------------------------
 * Execute h5edit commands.
 * For now, no return value--always succeed.
 * If error is encountered, it would have gone to exit function leave()
 * and exit from there.
 # This sets up the input stream, first from command-file if given.  If not,
 * setup from command string.
 */
void execute_command(void)
{
    /* setup command file input if given */
    if (commfile_g != NULL){
	h5edit_commfile = commfile_g;
	h5edit_command = NULL;
	h5edit_input_len = -1;
    } else {
	h5edit_commfile = NULL;
	h5edit_command = command_g;
	h5edit_input_len = strlen(h5edit_command);
    }

    if(H5EDITyyparse() != 0)
        goto quit;

    /* Normal return. */
    /* Keeping this in case we want to distinguish it from quit */
    h5edit_commfile = NULL;
    h5edit_command = NULL;
    h5edit_input_len = -1;
    return;

quit:
    h5edit_commfile = NULL;
    h5edit_command = NULL;
    h5edit_input_len = -1;
    return;
}


/* Set debug varilable if environment variable $H5EDIT_DEBUG is set */
void
set_debug(void)
{
    if (HDgetenv("H5EDIT_DEBUG") != NULL){
	h5edit_debug = 1;
	fprintf(stderr, "debug set to 1\n");
    }
}

/* return if debug is on.
 * TRUE (not 0) if set;
 * FALSE (0) otherwise.
 */
int
get_debug(void)
{
    return(h5edit_debug);
}

    
/* Print the version information of h5edit */
void
print_version(void)
{
    fprintf(stdout, H5EDIT_VER_INFO "\n");
}


/*==============================
 * Attribute definition routines
 *==============================*/
/* reset the attribute definition structure
 * Return void. Assuming always succeed. */
void
reset_attribute_def(attribute_def_p_t attr_def)
{
    /* sanity check */
    HDassert(attr_def);

    /* close type id */
    if (attr_def->type_id >= 0)
	H5Tclose(attr_def->type_id);
    attr_def->type_id = -1;
    attr_def->type_class = H5T_NO_CLASS;
    attr_def->type_str_size = 0;

    /* close space id */
    if (attr_def->space_id >= 0)
	H5Sclose(attr_def->space_id);
    attr_def->space_id = -1;
    attr_def->space_class = H5S_NO_CLASS;

    /* reset data_size */
    attr_def->data_size = -1;
}

/* retrieve the definitions of an attribute */
int
get_attribute_def(const char *attribute_name, const char *group_name,
    const char *dataset_name, attribute_def_p_t attr_def)
{
    hid_t attribute_id = -1;
    const char *objectname;

    /* sanity check */
    HDassert((group_name || dataset_name));
    HDassert(attribute_name);
    HDassert(attr_def);

    objectname = dataset_name ? dataset_name : group_name;
    if ((attribute_id=H5Aopen_by_name(datafile_g, objectname, attribute_name, H5P_DEFAULT, H5P_DEFAULT)) < 0){
	fprintf(stderr, "failed to open attribute %s/%s\n", objectname, attribute_name);
	goto error;
    }

    /* get attribute type class */
    if ((attr_def->type_id=H5Aget_type(attribute_id)) < 0){
	fprintf(stderr, "failed to get attribute type\n");
	goto error;
    }
    if ((attr_def->type_class=H5Tget_class(attr_def->type_id)) < 0){
	fprintf(stderr, "failed to get attribute type class\n");
	goto error;
    }
    if (get_debug()){
	    fprintf(stderr, "attribute type class is %d\n", attr_def->type_class);
    }
    if (H5T_STRING==attr_def->type_class){
	/* need to retrieve the string size */
	if ((attr_def->type_str_size = H5Tget_size(attr_def->type_id)) < 1) {
	    fprintf(stderr, "string size must be positive but got (%d)\n", attr_def->type_str_size);
	    goto error;
	}
	if (get_debug()){
	    fprintf(stderr, "attribute string size is %d\n", attr_def->type_str_size);
	}
    }

    /* get attribute space info */
    if ((attr_def->space_id=H5Aget_space(attribute_id)) < 0){
	fprintf(stderr, "failed to get attribute space\n");
	goto error;
    }
    if ((attr_def->space_class=H5Sget_simple_extent_type(attr_def->space_id)) < 0){
	fprintf(stderr, "failed to get attribute space class\n");
	goto error;
    }
    switch (attr_def->space_class){
	case H5S_SIMPLE: {
	    hsize_t x_dims[H5S_MAX_RANK];
	    int	x_ndims;
	    int	i;
	    /* get dimension sizes */
	    if ((x_ndims=H5Sget_simple_extent_ndims(attr_def->space_id)) < 0){
		fprintf(stderr, "failed to get ndims\n");
		goto error;
	    }
	    if (x_ndims > H5S_MAX_RANK){
		fprintf(stderr, "ndims(%d) is larger than maximum(%d)\n", x_ndims, H5S_MAX_RANK);
		goto error;
	    }
	    if ((H5Sget_simple_extent_dims(attr_def->space_id, x_dims, NULL)) < 0){
		fprintf(stderr, "failed to get dimension sizes\n");
		goto error;
	    }
	    /* calculate data size */
	    attr_def->data_size = x_dims[0];
	    for (i=1; i < x_ndims; i++)
		attr_def->data_size *= x_dims[i];

	    if (get_debug()){
		fprintf(stderr, "ndims=%d, dims=(%d", x_ndims, (int)x_dims[0]);
		for (i=1; i < x_ndims; i++)
		    fprintf(stderr, ", %d", (int)x_dims[i]);
		fprintf(stderr, "), data_size=%d\n", attr_def->data_size);
	    }
	    break;
	}
	case H5S_SCALAR:
	    attr_def->data_size = 1;
	    if (get_debug()){
		fprintf(stderr, "Space type is H5S_SCALAR and size is %d\n", attr_def->data_size);
	    }
	    break;
	case H5S_NULL:
	    fprintf(stderr, "Space type is H5S_NULL, not supported yet\n");
	    goto error;
	default:
	    fprintf(stderr, "unknown space class(%d)\n", attr_def->space_class);
	    goto error;
    }

    /* close attribute */
    if (H5Aclose(attribute_id) < 0){
	fprintf(stderr, "Could not close attribute\n");
	goto error;
    }

    /* All is well. Return success. */
    return(0);

error:
    /* clean up */
    if (attribute_id >= 0){
	if (H5Aclose(attribute_id) < 0){
	    fprintf(stderr, "Could not close attribute\n");
	    /* continue since this returns -1 eventually */
	}
    }

    /* return failed */
    return(-1);
}
