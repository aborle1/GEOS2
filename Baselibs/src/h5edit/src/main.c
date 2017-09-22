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

/* Global variables definitions */
/* option flags */
int	dryrun_g = FALSE;	/* dryrun default off */
atomic_level_t	atomic_level_g = Inc_atomic;	/* atomic level, default Inc. */
						/* For now it is same as 
						 * Yes_atomic.
						 */
/* data and command file name */
const char	*datafilename_g = NULL;
const char	*commfilename_g = NULL;
const char	*command_g = NULL;
const char	*progname_g;	/* program name */
hid_t		datafile_g = -1;
FILE		*commfile_g = NULL;

/* For Lex and Yacc */
int	h5edit_input_len;
const char	*h5edit_command;
FILE	*h5edit_commfile;

int
main(int ac, const char** av){

    if((progname_g=strrchr(av[0], '/')))
	progname_g++;
    else
	progname_g = av[0];

    init();

    parse_option(ac, av);
    execute_command();
    leave(EXIT_SUCCESS);
}
