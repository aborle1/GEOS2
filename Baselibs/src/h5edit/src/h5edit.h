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
#include <h5edit_config.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <private_definition.h>
#include <hdf5.h>
#include <hdf5_hl.h>
#include <assert.h>
#include "H5FDbackup.h"

/* Macro definitions */
#define EXIT_FAILURE	1
#define EXIT_SUCCESS	0
#ifndef	FALSE
    #define FALSE	0
#endif
#ifndef TRUE
    #define TRUE	!FALSE
#endif
#define streq(s1, s2)	!HDstrcmp(s1, s2)

/* Version information of the tool. It consists of:
 * Major version: increases whenever there are major feature changes;
 * Minor version: increases whenever there are feature changes;
 * Release:       increases whenever officially released.
 */
#define H5EDIT_VER_MAJOR    1
#define H5EDIT_VER_MINOR    3
#define H5EDIT_VER_RELEASE  1
#define H5EDIT_VER_SUBRELEASE ""
#define H5EDIT_VER_INFO		"H5edit version 1.3.1, released on 2014-11-14"


/* Global types declaration. */
typedef	enum object_t {No_type, Dataset_type, Group_type} object_t;
typedef	enum atomic_level_t {No_atomic, Yes_atomic, Inc_atomic} atomic_level_t;

/* structure for attribute definition */
typedef struct attribute_def_t {
    hid_t type_id;			/* init to -1 */
    H5T_class_t	type_class;		/* init to H5T_NO_CLASS */
    size_t	type_str_size;		/* init to 0 */
    hid_t space_id;			/* init to -1 */
    H5S_class_t	space_class;		/* init to H5S_NO_CLASS */
    int		data_size;		/* init to -1 */
} attribute_def_t;
typedef attribute_def_t * attribute_def_p_t;     /* pointer to attribute definition */


/* Global variables declaration. */
/* Option flags */
extern	atomic_level_t	atomic_level_g;
extern	int		dryrun_g;
extern	const char	*datafilename_g;
extern	const char	*commfilename_g;
extern	const char	*command_g;
extern	const char	*progname_g;
extern	hid_t		datafile_g;
extern	FILE		*commfile_g;

/* other global variables */
/* A glocal cheat variable */
extern char *backupname_g;

/* lex and yacc definitions */
extern int	h5edit_input_len;
extern const char	*h5edit_command;
extern FILE	*h5edit_commfile;
extern int	H5EDITyyparse(void);

/* Prototypes */
void init(void);
void leave(int ret);
void usage(void);
void parse_option(int ac, const char** av);
hid_t opendatafile(const char *name);
FILE *opencommfile(const char *name);
void execute_command(void);
void set_debug(void);
int get_debug(void);
void reset_attribute_def(attribute_def_p_t attr_def);
int get_attribute_def(const char *attribute_name, const char *group_name,
    const char *dataset_name, attribute_def_p_t attr_def);
int create_attribute(const char *attribute_name, const char *group_name,
    const char *dataset_name, hid_t type_id, hid_t space_id, void *data);
int delete_attribute(const char *attribute_name, const char *group_name,
    const char *dataset_name);
int rename_attribute(const char *attribute_name, const char *group_name,
    const char *dataset_name, const char *attribute_name_new);
int copy_attribute(char *src_object_name, char *src_attribute_name,
    char *dst_object_name, char *dst_attribute_name );
int modify_attribute(const char *attribute_name, const char *group_name,
    const char *dataset_name, attribute_def_p_t attr_def, void *data);
void print_version(void);
int split_final_quoted_string( char *pathname, char **trim_pathname, char **trim_attr );
hid_t H5FD_backup_init(void);
