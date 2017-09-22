/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * This file was adapted from the parse.y of the HDF5 code.              *
 * This is used to generate the parse.c by				     *
 *   yacc -pH5EDITyy -o parse.c -d parse.y			     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

%{
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

#include<h5edit.h>

extern int yylex();
extern int yyerror(char *);

#define STACK_SIZE      16
#define DATA_SIZE_MAX	512	/* max number of data points */

/* types definitions */
/* nothing yet */

/* variables */
/* Attribute name and handle */
char		*attribute_name = NULL;
char		*attribute_name_new = NULL;
char		*attribute_name_old = NULL;
hid_t		attribute_id = -1;
char		*attribute_type_name = NULL;
hid_t		attribute_type_id = -1;
hid_t		attribute_space_id = -1;
/*
H5T_class_t	attribute_type_class = H5T_NO_CLASS;
H5S_class_t	attribute_space_class = H5S_NO_CLASS;
*/
H5T_class_t	data_value_class = H5T_NO_CLASS;
struct arr_info	attribute_dims;		/* attribute dimension */
attribute_def_t attr_def_old={H5T_NO_CLASS, H5S_NO_CLASS, -1};	/* old attribute defintions */

/* names for COPY command */
char            *destination_attribute_name = NULL;
char            *source_object_name = NULL;
char            *destination_object_name = NULL;
char            *source_attribute_part = NULL;
char            *source_object_part = NULL;
char            *destination_attribute_part = NULL;
char            *destination_object_part = NULL;
char            *copy_param2_name = NULL;

		/* attribute data buffer */
/*void		*attribute_data = NULL; */
		/* this approach does not work for compound types */
union {
    int		data_int[DATA_SIZE_MAX];	/* attribute integer data */
    double	data_float[DATA_SIZE_MAX];	/* attribute float data */
    char	data_string[DATA_SIZE_MAX];	/* attribute string data */
    char	data_char[DATA_SIZE_MAX];	/* for SCALAR string data */
} attribute_data;
/*int		attribute_data_needed = -1;*/
int		attribute_data_gotten = -1;
		/* points to available string data buffer */
char*		attribute_data_pt = attribute_data.data_string;



/* Dataset name and handle */
char	*group_name = NULL;
char	*dataset_name = NULL;
hid_t	dataset_id = -1;

/*structure for array type information*/
struct arr_info {
    hsize_t	dims[H5S_MAX_RANK];     /*size of each dimension, limited to 32 dimensions*/
    int		ndims;                  /*number of dimensions*/
    hbool_t	is_dim;                 /*flag to lexer for dimension*/
};

int h5edit_csindex = -1;                /*pointer to the top of compound stack*/

/*stack for nested array type*/
struct arr_info arr_stack[STACK_SIZE];
int h5edit_asindex = -1;               /*pointer to the top of array stack*/ 


%}
%union {
    int   ival;         /*for integer token*/
    double fval;	/*for floating point token */
    char  *sval;        /*for name string*/
}

/* reserved command keyword tokens */
%token <ival> CREATE_KWORD DELETE_KWORD RENAME_KWORD COPY_KWORD MODIFY_KWORD
%token <ival> ATTRIBUTE_KWORD DATASET_KWORD GROUP_KWORD
%token <ival> DATATYPE_KWORD DATASPACE_KWORD DATA_KWORD
%token <sval> IDENTIFIER
%token <sval> PATHNAME
%token <sval> QUOTED_STRING
%token <sval> DQUOTED_STRING
%token <sval> EOSTMT_TOKEN
%token <sval> WSPACE		/* White spaces */

/* Data types */
%token <sval> INTEGER_TYPE_TOKEN FLOAT_TYPE_TOKEN

%token <ival> H5T_STD_I8BE_TOKEN H5T_STD_I8LE_TOKEN H5T_STD_I16BE_TOKEN  H5T_STD_I16LE_TOKEN
%token <ival> H5T_STD_I32BE_TOKEN H5T_STD_I32LE_TOKEN H5T_STD_I64BE_TOKEN H5T_STD_I64LE_TOKEN
%token <ival> H5T_STD_U8BE_TOKEN H5T_STD_U8LE_TOKEN H5T_STD_U16BE_TOKEN  H5T_STD_U16LE_TOKEN
%token <ival> H5T_STD_U32BE_TOKEN H5T_STD_U32LE_TOKEN H5T_STD_U64BE_TOKEN H5T_STD_U64LE_TOKEN
%token <ival> H5T_NATIVE_CHAR_TOKEN H5T_NATIVE_SCHAR_TOKEN H5T_NATIVE_UCHAR_TOKEN 
%token <ival> H5T_NATIVE_SHORT_TOKEN H5T_NATIVE_USHORT_TOKEN H5T_NATIVE_INT_TOKEN H5T_NATIVE_UINT_TOKEN 
%token <ival> H5T_NATIVE_LONG_TOKEN H5T_NATIVE_ULONG_TOKEN H5T_NATIVE_LLONG_TOKEN H5T_NATIVE_ULLONG_TOKEN

%token <ival> H5T_IEEE_F32BE_TOKEN H5T_IEEE_F32LE_TOKEN H5T_IEEE_F64BE_TOKEN H5T_IEEE_F64LE_TOKEN
%token <ival> H5T_NATIVE_FLOAT_TOKEN H5T_NATIVE_DOUBLE_TOKEN H5T_NATIVE_LDOUBLE_TOKEN

%token <ival> H5T_STRING_TOKEN STRSIZE_TOKEN STRPAD_TOKEN CSET_TOKEN CTYPE_TOKEN H5T_VARIABLE_TOKEN
%token <ival> H5T_STR_NULLTERM_TOKEN H5T_STR_NULLPAD_TOKEN H5T_STR_SPACEPAD_TOKEN 
%token <ival> H5T_CSET_ASCII_TOKEN H5T_CSET_UTF8_TOKEN H5T_C_S1_TOKEN H5T_FORTRAN_S1_TOKEN

%token <ival> H5T_COMPOUND_TOKEN
%token <ival> H5T_ARRAY_TOKEN
%token <ival> H5T_VLEN_TOKEN

%token <sval> STRING_CONST
%token <ival> INTEGER_CONST
%token <fval> FLOAT_CONST
%token <ival> '{' '}' '[' ']' '(' ')' ':' ','

/* data space tokens */
%token <ival> SCALAR_TOKEN SIMPLE_TOKEN NULL_TOKEN

%start		editstmts	/* editstmts */

%%
editstmts	:	/*empty*/
		|	editstmts editstmt
		;

editstmt	: 	editcmd eostmt
			/* One command executed. Do end of statement actions. */
			{
			    if (get_debug()) fprintf(stderr, "=====end of command.=====\n");
			    if (atomic_level_g == Inc_atomic){
			        if (H5Fflush(datafile_g, H5F_SCOPE_GLOBAL) < 0){
			           fprintf(stderr, "%s: failed to flush the data file\n",
			                   progname_g);
			           leave(EXIT_FAILURE);
			        }
				if (get_debug()) fprintf(stderr, "backup file updated.\n");
			    }

			}
		;

editcmd		:	create_cmd 
		|	delete_cmd 
		|	rename_cmd 
		|	copy_cmd 
		|	modify_cmd 
		;

create_cmd	:	/* two kinds of create commands of differnt syntax, create_cmd1 and create_cmd2 */
			create_cmd1
		|	create_cmd2
		;

delete_cmd	:	/* two kinds of delete commands of differnt syntax, delete_cmd1 and delete_cmd2 */
			delete_cmd1
		|	delete_cmd2
		;

copy_cmd        :       /* only one form of syntax so far, but allow for a second (e.g., identical
                         attribute name, or combined object/attribute */
                        copy_cmd1
                |       copy_cmd2
                ;

modify_cmd	:	/* two kinds of modify commands of differnt syntax, modify_cmd1 and modify_cmd2 */
			/* both syntax are combined into one rule. */
			modify_cmd1
		;

create_cmd1	:	/* create command with a single combined targetobject/attribute */
			CREATE_KWORD targetobject attribute_definition
			{
			    if (get_debug()) fprintf(stderr, "found create command syntax 1\n");
			    /* error check */
			    if (!dataset_name && !group_name){
				fprintf(stderr, "neither dataset nor group name is provided for CREATE command\n");
				leave(EXIT_FAILURE);
			    }
			    /* Execute the CREATE command */
			    /* Sending NULL for attribute_name because it is not defined. */
			    if (0 != create_attribute(NULL, group_name, dataset_name,
				attribute_type_id, attribute_space_id, &attribute_data)){
				CMD_ERROR("CREATE Attribute command failed");
			    }
			    /* Cleanup */
			    if (group_name){
				HDfree(group_name);
				group_name = NULL;
			    }
			    if (dataset_name){
				HDfree(dataset_name);
				dataset_name = NULL;
			    }
			    attr_def_old.data_size = -1;

			    if (attribute_space_id >= 0){
				H5Sclose(attribute_space_id);
				attribute_space_id = -1;
			    }
			    if (attribute_type_id >= 0){
				H5Tclose(attribute_type_id);
				attribute_type_id = -1;
			    }
			    attr_def_old.type_class = H5T_NO_CLASS;
			}
		;

create_cmd2	:	CREATE_KWORD targetobject attributename attribute_definition
			{
			    if (get_debug()) fprintf(stderr, "found create command\n");
			    /* error check */
			    if (!attribute_name){
				fprintf(stderr, "attribute name missing from CREATE command\n");
				leave(EXIT_FAILURE);
			    }
			    if (!dataset_name && !group_name){
				fprintf(stderr, "neither dataset nor group name is provided for CREATE command\n");
				leave(EXIT_FAILURE);
			    }
			    /* Execute the CREATE command */
			    if (0 != create_attribute(attribute_name, group_name, dataset_name,
				attribute_type_id, attribute_space_id, &attribute_data)){
				CMD_ERROR("CREATE Attribute command failed");
			    }
			    /* Cleanup */
			    HDfree(attribute_name);
			    attribute_name = NULL;
			    if (group_name){
				HDfree(group_name);
				group_name = NULL;
			    }
			    if (dataset_name){
				HDfree(dataset_name);
				dataset_name = NULL;
			    }
			    attr_def_old.data_size = -1;

			    if (attribute_space_id >= 0){
				H5Sclose(attribute_space_id);
				attribute_space_id = -1;
			    }
			    if (attribute_type_id >= 0){
				H5Tclose(attribute_type_id);
				attribute_type_id = -1;
			    }
			    attr_def_old.type_class = H5T_NO_CLASS;
			}
		;

delete_cmd1	:	/* delete command with a single combined targetobject/attribute */
			DELETE_KWORD targetobject
			{
			    if (get_debug()) fprintf(stderr, "found delete command syntax 1\n");
			    /* error check */
			    if (!dataset_name && !group_name){
				fprintf(stderr, "neither dataset nor group name is provided for DELETE command\n");
				leave(EXIT_FAILURE);
			    }
			    /* Execute the DELETE command */
			    /* Sending NULL for attribute_name because it is not defined. */
			    if (0 != delete_attribute(NULL, group_name, dataset_name)){
				CMD_ERROR("DELETE Attribute command failed");
			    }
			    /* Cleanup */
			    if (group_name){
				HDfree(group_name);
				group_name = NULL;
			    }
			    if (dataset_name){
				HDfree(dataset_name);
				dataset_name = NULL;
			    }
			}
		;

delete_cmd2	:	/* delete command with separated targetobject and attribute */
			DELETE_KWORD targetobject attributename
			{
			    if (get_debug()) fprintf(stderr, "found delete command\n");
			    /* error check */
			    if (!attribute_name){
				fprintf(stderr, "attribute name missing from DELETE command\n");
				leave(EXIT_FAILURE);
			    }
			    if (!dataset_name && !group_name){
				fprintf(stderr, "neither dataset nor group name is provided for DELETE command\n");
				leave(EXIT_FAILURE);
			    }
			    /* Execute the DELETE command */
			    if (0 != delete_attribute(attribute_name, group_name, dataset_name)){
				CMD_ERROR("DELETE Attribute command failed");
			    }
			    /* Cleanup */
			    HDfree(attribute_name);
			    attribute_name = NULL;
			    if (group_name){
				HDfree(group_name);
				group_name = NULL;
			    }
			    if (dataset_name){
				HDfree(dataset_name);
				dataset_name = NULL;
			    }
			}
		;

       /* copy command with combined object and attribute (two compound parameters) */
copy_cmd2       :
                        COPY_KWORD sourceobjectname copyparam2
                        {
			    if (get_debug()) fprintf(stderr, "found copy2 command\n");
                            if (!source_object_name){
                                fprintf(stderr, "source object is missing for COPY command\n");
                                leave(EXIT_FAILURE);
                            }
                            if (!copy_param2_name){
                                fprintf(stderr, "destination is missing for COPY command\n");
                                leave(EXIT_FAILURE);
                            }
                            /* split  source object and attribute names */
                            source_attribute_part = split_object_component( source_object_name,
					&source_object_part );
                            if (!source_object_part){
                                fprintf(stderr, "source object name couldn't be found for COPY command\n");
                                leave(EXIT_FAILURE);
                            }
                            if (!source_attribute_part){
                                fprintf(stderr, "source attribute name couldn't be found for COPY command\n");
                                leave(EXIT_FAILURE);
                            }
                            /* split destination object and attribute names */
                            destination_attribute_part = split_object_component( copy_param2_name,
					&destination_object_part );
                            if (!destination_object_part){
                                fprintf(stderr, "destination object name couldn't be found for COPY command\n");
                                leave(EXIT_FAILURE);
                            }
			    /* in this case we assume no destination object so use the source object */
                            if (!destination_attribute_part){
				destination_attribute_part = copy_param2_name;
				destination_object_part = source_object_part;
                            }
                            if (0 != copy_attribute(source_object_part, source_attribute_part,
                                    destination_object_part, destination_attribute_part)){
                                CMD_ERROR("COPY Attribute command failed");
                            }
                            HDfree(source_object_name);
                            /*HDfree(source_object_part);*/
                            /*HDfree(source_attribute_part);*/
                            source_object_name = NULL;
                            HDfree(copy_param2_name);
                            /*HDfree(destination_object_part);*/
                            /*HDfree(destination_attribute_part);*/
                            copy_param2_name = NULL;
                        }
                ;

copy_cmd1       :       /* copy command with separated source and destination object and attribute (four parameters) */
                        COPY_KWORD sourceobjectname copyparam2 destinationobjectname destinationattributename
                        {
                            if (get_debug()) fprintf(stderr, "found copy1 command\n");
                            /* error check */
                            if (!source_object_name){
                                fprintf(stderr, "source object is missing for COPY command\n");
                                leave(EXIT_FAILURE);
                            }
                            if (!copy_param2_name){
                                fprintf(stderr, "source attribute name missing from COPY command\n");
                                leave(EXIT_FAILURE);
                            }
                            if (!destination_object_name){
                                fprintf(stderr, "destination object is missing for COPY command\n");
                                leave(EXIT_FAILURE);
                            }
                            if (!destination_attribute_name){
                                fprintf(stderr, "destination attribute name missing from COPY command\n");
                                leave(EXIT_FAILURE);
                            }
                            /* Execute the COPY command */
                            if (0 != copy_attribute(source_object_name, copy_param2_name, 
                                    destination_object_name, destination_attribute_name)){
                                CMD_ERROR("COPY Attribute command failed");
                            }
                            /* Cleanup */
                            HDfree(source_object_name);
                            HDfree(copy_param2_name);
                            HDfree(destination_object_name);
                            HDfree(destination_attribute_name);
                            source_object_name = NULL;
                            destination_object_name = NULL;
                        }
                ;

modify_cmd1	:	/* modify command accepts two kinds of syntax, modify_cmd1 and modify_cmd2 */
			MODIFY_KWORD targetobject_attribute
			{
			    if (get_debug()) fprintf(stderr, "got attribute_name(%s)\n", attribute_name);
			    /* copy the attribute name to old attribute name and reset
			       attributename. */
			    if (attribute_name_old != NULL){
				fprintf(stderr, "Already got a old attribute name\n");
				leave(EXIT_FAILURE);
			    }
			    attribute_name_old = attribute_name;
			    attribute_name = NULL;
			    /* Either groupname or datasetname should be known by now */
			    if (!dataset_name && !group_name){
				fprintf(stderr, "neither dataset nor group name is provided for attribute\n");
				leave(EXIT_FAILURE);
			    }
			    /* retrieve the definitions of the old attribute */
			    if (0 != get_attribute_def(attribute_name_old, group_name, dataset_name, &attr_def_old)){
				fprintf(stderr, "failed to get the definition of old attribute %s\n", attribute_name_old);
				leave(EXIT_FAILURE);
			    }
			}
			data_stmt
			{
			    if (get_debug()) fprintf(stderr, "found modify command syntax 1\n");
			    /* error check */
			    if (!dataset_name && !group_name){
				fprintf(stderr, "neither dataset nor group name is provided for MODIFY command\n");
				leave(EXIT_FAILURE);
			    }
			    if (get_debug()) fprintf(stderr, "found modify command syntax 1 second part\n");
			    /* Execute the MODIFY command */
			    /* Sending NULL for attribute_name because it is not defined. */
			    if (0 != modify_attribute(attribute_name_old, group_name, dataset_name,
				&attr_def_old, &attribute_data)){
				CMD_ERROR("MODIFY Attribute command failed");
			    }
			    /* Cleanup */
			    HDfree(attribute_name_old);
			    attribute_name_old = NULL;
			    if (group_name){
				HDfree(group_name);
				group_name = NULL;
			    }
			    if (dataset_name){
				HDfree(dataset_name);
				dataset_name = NULL;
			    }
			    reset_attribute_def(&attr_def_old);
			}
		;

attributename	:	objectname
			{if (get_debug()) fprintf(stderr, "got attribute name(%s)\n", H5EDITyylval.sval);
			    /* save the attribute name */
			    if (attribute_name != NULL){
				fprintf(stderr, "Already got an attribute name\n");
				leave(EXIT_FAILURE);
			    }
			    if ((attribute_name = HDstrdup(H5EDITyylval.sval)) == NULL){
				fprintf(stderr, "strdup failed\n");
				leave(EXIT_FAILURE);
			    }
			}
		;

copyparam2	:	objectname 
			{if (get_debug()) fprintf(stderr, "got copy_param2 name(%s)\n", H5EDITyylval.sval);
			    if (copy_param2_name != NULL){
                                fprintf(stderr, "Already got second param name\n");
                                leave(EXIT_FAILURE);
                            }
                            if ((copy_param2_name = HDstrdup(H5EDITyylval.sval)) == NULL){
                                fprintf(stderr, "strdup failed\n");
                                leave(EXIT_FAILURE);
                            }
			}
		;

attrname_old	:	attributename
			{
			    if (get_debug()) fprintf(stderr, "got attrname_old(%s)\n", attribute_name);
			    /* copy the attribute name to old attribute name and reset
			       attributename. */
			    if (attribute_name_old != NULL){
				fprintf(stderr, "Already got a old attribute name\n");
				leave(EXIT_FAILURE);
			    }
			    attribute_name_old = attribute_name;
			    attribute_name = NULL;
			    /* Either groupname or datasetname should be known by now */
			    if (!dataset_name && !group_name){
				fprintf(stderr, "neither dataset nor group name is provided for attribute\n");
				leave(EXIT_FAILURE);
			    }
			}
                ;

attrname_new	:	attributename
			{
			    if (get_debug()) fprintf(stderr, "got attrname_new(%s)\n", attribute_name);
			    /* copy the attribute name to new attribute name and reset
			       attributename. */
			    if (attribute_name_new != NULL){
				fprintf(stderr, "Already got a new attribute name\n");
				leave(EXIT_FAILURE);
			    }
			    attribute_name_new = attribute_name;
			    attribute_name = NULL;
			}
                ;

sourceobjectname:       PATHNAME
                        {if (get_debug()) fprintf(stderr, "got source object name(%s)\n", H5EDITyylval.sval);
                            /* save the source object name */
                            if (source_object_name != NULL){
                                fprintf(stderr, "Already got a source object name\n");
                                leave(EXIT_FAILURE);
                            }
                            if ((source_object_name = HDstrdup(H5EDITyylval.sval)) == NULL){
                                fprintf(stderr, "strdup failed\n");
                                leave(EXIT_FAILURE);
                            }
                        }
                ;

destinationobjectname   :       PATHNAME
                        {if (get_debug()) fprintf(stderr, "got destination object name(%s)\n", H5EDITyylval.sval);
                            /* save the destination object name */
                            if (destination_object_name != NULL){
                                fprintf(stderr, "Already got a destination object name\n");
                                leave(EXIT_FAILURE);
                            }
                            if ((destination_object_name = HDstrdup(H5EDITyylval.sval)) == NULL){
                                fprintf(stderr, "strdup failed\n");
                                leave(EXIT_FAILURE);
                            }
                        }
                ;

destinationattributename        :      objectname
                        {if (get_debug()) fprintf(stderr, "got destination attribute name(%s)\n", H5EDITyylval.sval);
                            /* save the destination attribute name */
                            if (destination_attribute_name != NULL){
                                fprintf(stderr, "Already got a destination attribute name\n");
                                leave(EXIT_FAILURE);
                            }
                            if ((destination_attribute_name = HDstrdup(H5EDITyylval.sval)) == NULL){
                                fprintf(stderr, "strdup failed\n");
                                leave(EXIT_FAILURE);
                            }
                        }
                ;


targetobject	:	datasetname
		|	DATASET_KWORD datasetname
		|	GROUP_KWORD	groupname
		;

datasetname	:	objectname
			{if (get_debug()) fprintf(stderr, "got dataset name(%s)\n", H5EDITyylval.sval);
			    /* save the dataset name */
			    if (dataset_name != NULL){
				fprintf(stderr, "Already got an dataset name\n");
				leave(EXIT_FAILURE);
			    }
			    if ((dataset_name = HDstrdup(H5EDITyylval.sval)) == NULL){
				fprintf(stderr, "strdup failed\n");
				leave(EXIT_FAILURE);
			    }
			}
		;

groupname	:	objectname
			{if (get_debug()) fprintf(stderr, "got group name(%s)\n", H5EDITyylval.sval);
			    /* save the group name */
			    if (group_name != NULL){
				fprintf(stderr, "Already got an group name\n");
				leave(EXIT_FAILURE);
			    }
			    if ((group_name = HDstrdup(H5EDITyylval.sval)) == NULL){
				fprintf(stderr, "strdup failed\n");
				leave(EXIT_FAILURE);
			    }
			}
		;

objectname	:	IDENTIFIER
		|	PATHNAME
		|	QUOTED_STRING
		|	DQUOTED_STRING
		;

eostmt		:	EOSTMT_TOKEN
		;

/* Attribute Definition */

targetobject_attribute	:	/* targetobject_attribute have two syntax. */
				targetobject_attribute_def
			|	targetobject attribute_def
			;

targetobject_attribute_def : 	targetobject
			{
			    if (get_debug()) fprintf(stderr, "found targetobject_attribute_def\n");
			    /* retrieve the definitions of the old attribute */
			    if (0 != split_object_attribute(group_name, dataset_name)){
				fprintf(stderr, "failed to split object_attribute names\n");
				leave(EXIT_FAILURE);
			    }
			}
		;

attribute_def	:	attributename
			{
			    /* nothing yet */
			    if (get_debug()) fprintf(stderr, "in attribute_def\n");
			}
			;

attribute_definition : "{"
			attribute_body_definition
			"}"
			{ if (get_debug()) fprintf(stderr, "found attribute definition\n");

			}
		;

attribute_body_definition:
			datatype_stmt dataspace_stmt data_stmt
		|	datatype_stmt                data_stmt
		|		      dataspace_stmt data_stmt
		|	                             data_stmt
		;

datatype_stmt	:	DATATYPE_KWORD datatype_definition
		|	datatype_definition
		;

datatype_definition:	datatype_class
		;

datatype_class	:	integer_class
		|	floatpoint_class
		|	string_class
		;

string_class	:	H5T_STRING_TOKEN "{" string_type_definition "}"
			{
			    if (get_debug()) fprintf(stderr, "found string type\n");
			    if (attr_def_old.type_class != H5T_NO_CLASS){
				fprintf(stderr, "Already has an attribute data type\n");
				leave(EXIT_FAILURE);
			    }
			    attr_def_old.type_class = H5T_STRING;
			    /* for now, assume it is null-terminated strings */
			    if ((attribute_type_id=H5Tcopy(H5T_C_S1)) < 0){
				fprintf(stderr, "Attribute string type create failure\n");
				leave(EXIT_FAILURE);
			    }
			    if (H5Tset_size(attribute_type_id,attr_def_old.type_str_size) < 0){
				fprintf(stderr, "Attribute string size set failure\n");
				leave(EXIT_FAILURE);
			    }
			    if (H5Tset_strpad(attribute_type_id, H5T_STR_NULLTERM) < 0){
				fprintf(stderr, "Attribute string pad set failure\n");
				leave(EXIT_FAILURE);
			    }
			}
		;

string_type_definition : string_size_definition
			{
			    if (get_debug()) fprintf(stderr, "found string size\n");
			}
		;

string_size_definition : STRSIZE_TOKEN INTEGER_CONST
			{   /* Must test value range before assign it to str_zize */
			    /* which is size_t type which is unsigned. */
			    if (get_debug()) fprintf(stderr, "String size is (%d)\n",
				    H5EDITyylval.ival);
			    if (H5EDITyylval.ival <= 0){
				fprintf(stderr, "String size must be positive (%d)\n",
				    H5EDITyylval.ival);
				leave(EXIT_FAILURE);
			    }
			    attr_def_old.type_str_size = H5EDITyylval.ival;
			}
		;

string_const	:	QUOTED_STRING
		|	DQUOTED_STRING
		;

integer_class	:	INTEGER_TYPE_TOKEN
			{
			    if (get_debug()) fprintf(stderr, "found integer type(%s)\n",
				H5EDITyylval.sval);
			    if (attr_def_old.type_class != H5T_NO_CLASS){
				fprintf(stderr, "Already has an attribute data type\n");
				leave(EXIT_FAILURE);
			    }
			    attr_def_old.type_class = H5T_INTEGER;
			    if ((attribute_type_id=H5LTtext_to_dtype(H5EDITyylval.sval, H5LT_DDL)) < 0){
				fprintf(stderr, "Attribute type create failure\n");
				leave(EXIT_FAILURE);
			    }
			}
		;

floatpoint_class :	FLOAT_TYPE_TOKEN
			{
			    if (get_debug()) fprintf(stderr, "found float type(%s)\n",
				H5EDITyylval.sval);
			    if (attr_def_old.type_class != H5T_NO_CLASS){
				fprintf(stderr, "Already has an attribute data type\n");
				leave(EXIT_FAILURE);
			    }
			    attr_def_old.type_class = H5T_FLOAT;
			    if ((attribute_type_id=H5LTtext_to_dtype(H5EDITyylval.sval, H5LT_DDL)) < 0){
				fprintf(stderr, "Attribute type create failure\n");
				leave(EXIT_FAILURE);
			    }
			}
		;

dataspace_stmt	:	DATASPACE_KWORD dataspace_definition
		|	dataspace_definition
		;

dataspace_definition:
			dataspace_simple
		|	dataspace_scalar
		|	dataspace_null
		;

dataspace_scalar:	SCALAR_TOKEN
			{
			    if (get_debug()) fprintf(stderr, "found scalar dataspace\n");
			    attr_def_old.space_class = H5S_SCALAR;
			    if (attribute_space_id != -1){
				fprintf(stderr, "Already has an attribute space\n");
				leave(EXIT_FAILURE);
			    }
			    if ((attribute_space_id=H5Screate(H5S_SCALAR)) < 0){
				fprintf(stderr, "Attribute space create failure\n");
				leave(EXIT_FAILURE);
			    }
			    attr_def_old.data_size = 1;
			}
		;

dataspace_simple:	SIMPLE_TOKEN dataspace_dims_list
		|	dataspace_dims_list
		;

dataspace_dims_list:

			{
			    if (get_debug()) fprintf(stderr, "looking for dimlist\n");
			    attr_def_old.space_class = H5S_SIMPLE;
			    /* reset attribute dimension information */
			    HDmemset(&attribute_dims, 0, sizeof(attribute_dims));
			}

			"(" dim_firstone dims_list ")"
			{
			    if (get_debug()){
				int i;
				fprintf(stderr, "found simple dataspace\n");
				fprintf(stderr, "rank=%d:\n", attribute_dims.ndims);
				for (i=0; i<attribute_dims.ndims; i++){
				    fprintf(stderr, "%ld\n", (long) attribute_dims.dims[i]);
				}
			    }
			    if (attribute_space_id != -1){
				fprintf(stderr, "Already has an attribute space\n");
				leave(EXIT_FAILURE);
			    }
			    if ((attribute_space_id=H5Screate_simple(attribute_dims.ndims,
				    attribute_dims.dims, NULL)) < 0 ){
				fprintf(stderr, "Attribute space create failure\n");
				leave(EXIT_FAILURE);
			    }
			    /* Calculate number of data elements needed */
			    attr_def_old.data_size = attribute_dims.dims[0];
			    {   int i;
				for (i=1; i < attribute_dims.ndims; i++){
				    attr_def_old.data_size *= attribute_dims.dims[i];
				}
				if (get_debug()) fprintf(stderr, "attribute data needed=%d\n",
					attr_def_old.data_size);
			    }
			    /* AKC: I don't like the coding here. */
			    /* It is repeating code.  Need to fix it later. */
			    /* Default datatype is Native Float */
			    if ( attr_def_old.type_class == H5T_NO_CLASS){
				attr_def_old.type_class = H5T_FLOAT;
				if ((attribute_type_id=H5Tcopy(H5T_NATIVE_FLOAT)) < 0){
				    fprintf(stderr, "Attribute default type create failure\n");
				    leave(EXIT_FAILURE);
				}
			    }
			    switch (attr_def_old.type_class){
			    case H5T_INTEGER:
			    case H5T_FLOAT:
				if (attr_def_old.data_size > DATA_SIZE_MAX){
				    fprintf(stderr,
					"Too many attribute data needed(%d). "
					"Maximum is %d\n",
					attr_def_old.data_size, DATA_SIZE_MAX);
				    leave(EXIT_FAILURE);
				}
				break;
			    case H5T_STRING:
				if ((attr_def_old.data_size*attr_def_old.type_str_size) > sizeof(attribute_data)){
				    fprintf(stderr,
					"Too much attribute string data needed(%d). "
					"Maximum is %d\n",
					(int)attr_def_old.data_size*attr_def_old.type_str_size,
					(int)sizeof(attribute_data));
				    leave(EXIT_FAILURE);
				}
				break;
			    default:
				fprintf(stderr, "Unsupported data type(%d)\n",
				    attr_def_old.type_class);
				leave(EXIT_FAILURE);
			    }
			}
		;

dim_firstone	:	INTEGER_CONST
			{   /* first dimension */
			    if (H5EDITyylval.ival <=0){
				fprintf(stderr, "Dimension must be positive. Got %d.\n",
				    H5EDITyylval.ival);
				leave(EXIT_FAILURE);
			    }
			    attribute_dims.dims[attribute_dims.ndims] = H5EDITyylval.ival;
			    attribute_dims.ndims++;
			}
		;

dims_list	:	/* empty */
		|	dims_list dim_more
		;

dim_more	:	"," INTEGER_CONST
			{   /* more dimensions */
			    if (attribute_dims.ndims >= H5S_MAX_RANK){
				fprintf(stderr, "Too many dimensions. Maximun is %d\n",
				    H5S_MAX_RANK);
				leave(EXIT_FAILURE);
			    }
			    if (H5EDITyylval.ival <=0){
				fprintf(stderr, "Dimension must be positive. Got %d.\n",
				    H5EDITyylval.ival);
				leave(EXIT_FAILURE);
			    }
			    attribute_dims.dims[attribute_dims.ndims] = H5EDITyylval.ival;
			    attribute_dims.ndims++;
			}
		;

dataspace_null	:	NULL_TOKEN
		;

			
data_stmt	:	DATA_KWORD data_definition
		|	data_definition
		;

data_definition :	"{"
			{
			    /* get the data buffer ready for new data */
			    if (get_debug()) fprintf(stderr, "found attribute data\n");
			    attribute_data_gotten = 0;
			    attribute_data_pt = attribute_data.data_string;
			    /* set attribute definition to default values if not set yet. */
			    if ( attr_def_old.type_class == H5T_NO_CLASS){
				/* Default datatype is Native Float */
				attr_def_old.type_class = H5T_FLOAT;
				if ((attribute_type_id=H5Tcopy(H5T_NATIVE_FLOAT)) < 0){
				    fprintf(stderr, "Attribute default type create failure\n");
				    leave(EXIT_FAILURE);
				}
				/* Default dataspace is SCALAR */
				if (attribute_space_id == -1){
				    attr_def_old.space_class = H5S_SCALAR;
				    if ((attribute_space_id=H5Screate(H5S_SCALAR)) < 0){
					fprintf(stderr, "Attribute default space create failure\n");
					leave(EXIT_FAILURE);
				    }
				    attr_def_old.data_size = 1;
				}
			    }
			}
			data_firstval data_list
			{
			    if (attribute_data_gotten < attr_def_old.data_size){
				fprintf(stderr, "Fewer data than needed. "
				    "Need %d but got %d data only\n",
				    attr_def_old.data_size, attribute_data_gotten);
				leave(EXIT_FAILURE);
			    }
			}
			"}"
			{ /* no action */
			}
		;

data_firstval	:	data_val
		;

data_list	:	/* empty */
		|	data_list data_more
		;

data_more	:	"," data_val
		;

data_val	:	INTEGER_CONST
			{
			    if (attribute_data_gotten >= attr_def_old.data_size){
				fprintf(stderr, "More data than needed. Only %d are needed.\n",
				    attr_def_old.data_size);
				leave(EXIT_FAILURE);
			    }
			    if (attr_def_old.type_class != H5T_INTEGER){
				fprintf(stderr, "Gotten incorrect type of data\n");
				leave(EXIT_FAILURE);
			    }
			    attribute_data.data_int[attribute_data_gotten++] = H5EDITyylval.ival;
			}

		|	FLOAT_CONST
			{
			    if (attribute_data_gotten >= attr_def_old.data_size){
				fprintf(stderr, "More data than needed. Only %d are needed.\n",
				    attr_def_old.data_size);
				leave(EXIT_FAILURE);
			    }
			    if (attr_def_old.type_class != H5T_FLOAT){
				fprintf(stderr, "Gotten incorrect type of data\n");
				leave(EXIT_FAILURE);
			    }
			    attribute_data.data_float[attribute_data_gotten++] = H5EDITyylval.fval;
			}
		|	string_const
			{
			    if (attribute_data_gotten >= attr_def_old.data_size){
				fprintf(stderr, "More data than needed. Only %d are needed.\n",
				    attr_def_old.data_size);
				leave(EXIT_FAILURE);
			    }
			    if (attr_def_old.type_class != H5T_STRING){
				fprintf(stderr, "Gotten incorrect type of data\n");
				leave(EXIT_FAILURE);
			    }
			    switch (attr_def_old.space_class) {
			    case H5S_SCALAR:
				/* this works for scalar only. Copy the
				 * string_const to attribute_data. */
				if (attribute_data_gotten > 0 ){
				    fprintf(stderr, "More data than needed. Only %d are needed.\n",
					attr_def_old.data_size);
				    leave(EXIT_FAILURE);
				}
				HDstrncpy(attribute_data_pt, H5EDITyylval.sval, attr_def_old.type_str_size);
				/* move data pointer forward */
				attribute_data_pt += attr_def_old.type_str_size;
				attribute_data_gotten++;
				break;
			    case H5S_SIMPLE:
				/* This should work for SIMPLE space */
				/* this is not working
				fprintf(stderr, "not able to support string type of SIMPLE space\n");
				leave(EXIT_FAILURE);
				*/
				    HDstrncpy(attribute_data_pt, H5EDITyylval.sval, attr_def_old.type_str_size);
				    /* move data pointer forward */
				    attribute_data_pt += attr_def_old.type_str_size;
				attribute_data_gotten++;
				break;
			    default:
				fprintf(stderr, "Do not know how to store string_const "
				    "of this kind of data space(%d)\n",
				    attr_def_old.space_class);
				leave(EXIT_FAILURE);
			    }
			}
		;


rename_cmd	:	/* two kinds of rename commands of different syntax, rename_cmd1 and rename_cmd2 */
			rename_cmd1
		|	rename_cmd2
		;

rename_cmd1	:	/* rename command with an old targetobject/attribute and a new attribute name */
			RENAME_KWORD targetobject attrname_old attrname_new
			{
			    if (get_debug()){
				fprintf(stderr, "found rename command 1\n");
				fprintf(stderr, "attribute_name_old=%s, group_name=%s, dataset_name=%s, attribute_name_new=%s\n",
				    (attribute_name_old?attribute_name_old:"null"),
				    (group_name?group_name:"null"),
				    (dataset_name?dataset_name:"null"),
				    (attribute_name_new?attribute_name_new:"null")
				);
			    }
			    /* error check */
			    if (!attribute_name_old){
				fprintf(stderr, "old attribute name missing from RENAME command 1\n");
				leave(EXIT_FAILURE);
			    }
			    if (!attribute_name_new){
				fprintf(stderr, "new attribute name missing from RENAME command 1\n");
				leave(EXIT_FAILURE);
			    }
			    if (!dataset_name && !group_name){
				fprintf(stderr, "neither dataset nor group name is provided for RENAME command 1\n");
				leave(EXIT_FAILURE);
			    }
			    /* Execute the RENAME command */
			    if (0 != rename_attribute(attribute_name_old, group_name, dataset_name, attribute_name_new)){
				CMD_ERROR("RENAME Attribute command failed");
			    }
			    /* Cleanup */
			    HDfree(attribute_name_old);
			    attribute_name_old = NULL;
			    HDfree(attribute_name_new);
			    attribute_name_new = NULL;
			    if (group_name){
				HDfree(group_name);
				group_name = NULL;
			    }
			    if (dataset_name){
				HDfree(dataset_name);
				dataset_name = NULL;
			    }
			    if (get_debug()) fprintf(stderr, "finished rename command 1\n");
			}
		;

rename_cmd2	:	/* rename command with a single combined old targetobject/attribute and a new attribute name */
			RENAME_KWORD targetobject attrname_new
			{
			    if (get_debug()){
				fprintf(stderr, "found rename command 2\n");
				fprintf(stderr, "attribute_name_old=%s, group_name=%s, dataset_name=%s, attribute_name_new=%s\n",
				    (attribute_name_old?attribute_name_old:"null"),
				    (group_name?group_name:"null"),
				    (dataset_name?dataset_name:"null"),
				    (attribute_name_new?attribute_name_new:"null")
				);
			    }
			    /* error check */
			    if (!attribute_name_new){
				fprintf(stderr, "new attribute name missing from RENAME command 2\n");
				leave(EXIT_FAILURE);
			    }
			    if (!dataset_name && !group_name){
				fprintf(stderr, "neither dataset nor group name is provided for RENAME command 2\n");
				leave(EXIT_FAILURE);
			    }
			    /* Execute the RENAME command */
			    if (0 != rename_attribute(NULL, group_name, dataset_name, attribute_name_new)){
				CMD_ERROR("RENAME Attribute command failed");
			    }
			    /* Cleanup */
			    HDfree(attribute_name_new);
			    attribute_name_new = NULL;
			    if (group_name){
				HDfree(group_name);
				group_name = NULL;
			    }
			    if (dataset_name){
				HDfree(dataset_name);
				dataset_name = NULL;
			    }
			    if (get_debug()) fprintf(stderr, "finished rename command 2\n");
			}
		;

%%

/* subroutine section */
char* strrstr ( const char* str1, const char* str2 );

/**
* Strip the final identifier (legal attributname) from a pathname.
* This allows us to deal with a combined objectpath/attributename
* construct.
*
* Input: the pathname
* Output: the (potential, if trimmed) object name
* Result: the (potential, if trimmed) attribute name
*/
char * split_object_component ( char *pathname, char **trim_pathname )
{
    char *split_ptr = NULL;    /* pointer to split point */
    char *split_ptr0 = NULL;   /* temporary pointer to split point */
    char *attr_ptr = NULL;     /* pointer to final object segment */

    /* split at final occurrence of double slash. */
    *trim_pathname = HDstrdup( pathname );
    if (split_ptr = strrstr( *trim_pathname, "//" )) {
        *split_ptr++ = '\0';
        attr_ptr = split_ptr;
        if (get_debug()) fprintf( stderr, "Split: doubleslash\n" );
    }
    /*  split at penultimate occurrence of single or double quote; remove quotes */
    else if (split_final_quoted_string(pathname,trim_pathname,&attr_ptr)) {
        if (get_debug()) fprintf( stderr, "Split: quotes\n" );
        /* don't need to do anything else */
    }
    /* split at final occurrence of slash. */
    else if (split_ptr = strrchr( (const char*)*trim_pathname, '/' )) {
        *split_ptr++ = '\0';
        if (get_debug()) fprintf( stderr, "split: final slash\n" );
        attr_ptr = split_ptr;
        /* pathological case: root object */
        if (*trim_pathname[0] == '\0') {
            *trim_pathname = HDstrdup( "/" );
        }
    }
    else
        if (get_debug()) fprintf( stderr, "split: none found!\n" );

    if (get_debug())
        fprintf( stderr, "Object part: '%s'; Attr part: '%s'\n", *trim_pathname, attr_ptr );

    return attr_ptr;
}

/*
* if final token is a quoted string, return true and set return variables
* to reflect the object and attribute parts.
*/
int split_final_quoted_string( char *pathname, char **trim_pathname, char **trim_attr ) 
{
    char    *trimptr = NULL;
    char    final = 0;
    int     pos = -1;

    /* can't be a non-zero-length quoted string */
    if (!pathname || (strlen(pathname) < 3)) return 0;

    /* final character has to be a single- or double-quote */
    final = pathname[strlen(pathname)-1];
    if (((char)final == '"') || ((char)final == '\'')) {
        if (get_debug()) fprintf( stderr, "Got a quote: %c\n", final );
        pos = strlen(*trim_pathname)-1;
        if (get_debug()) fprintf( stderr, "   final position: %d\n", pos );
        /* *trim_pathname[strlen(*trim_pathname)-1] = '\0'; *//* remove the trailing quote */
        (*trim_pathname)[pos] = '\0'; /* remove the trailing quote */
        if (get_debug()) fprintf( stderr, "   removed trailing quote\n" );
        if (trimptr = strrchr( *trim_pathname, final )) {    /* found first quote */
            if (get_debug()) fprintf( stderr, "   found startf of token\n" );
            *trimptr++ = '\0';
            if (get_debug()) fprintf( stderr, "   split string\n" );
            *trim_attr = trimptr;
            if (get_debug()) fprintf( stderr, "   tweaking attr ptr\n" );
            return 1;
        }
        else {
            /* FIXME: restore trim_pathname */
            if (get_debug()) fprintf( stderr, "   repairing pathname\n" );
            (*trim_pathname)[strlen(*trim_pathname)] = (char)final;
        }
    }

    /* return false */
    return 0;
}

/*
* Find last (rightmost) occurrence of string str2 in string str1.
*/
char* strrstr ( const char* str1, const char* str2 )
{
  char* strp;
  int len1, len2;

  len2 = strlen(str2);
  if(len2==0)
    return (char*)str1;

  len1 = strlen(str1);
  if(len1 - len2 <= 0)
    return 0;
   
  strp = (char*)(str1 + len1 - len2);
  while(strp != str1)
  {
    if(*strp == *str2)
    {
      if(strncmp(strp,str2,len2)==0)
        return strp;
    }
    strp--;
  }
  return 0;
}

/* split a object/attribute name into object and attribute names */
int
split_object_attribute(char *group_name, char *dataset_name)
{
/*
    hid_t attribute_id = -1;
    hid_t type_id = -1;
    hid_t space_id = -1;
*/
    char *objectname, *t_attrname;
    char *t_objectname=NULL;
    int   t_attributenamesize=-1;


    /* sanity check */
    HDassert((group_name || dataset_name));
    HDassert(!attribute_name);

    objectname = dataset_name ? dataset_name : group_name;
    /* attribute_name is not given. Retrieve it from the objectname */
    if ((t_attrname=HDstrrchr(objectname, '/')) == NULL){
	fprintf(stderr, "Cannot extract attribute name from target-object-name %s",
	    objectname);
	goto error;
    };
    t_attributenamesize=HDstrlen(t_attrname);	/* size includes null terminator */
    if (NULL==(attribute_name=HDmalloc(t_attributenamesize))){
	fprintf(stderr, "memory allocation failed\n");
	goto error;
    }
    /* copy 1 byte after the last slash found */
    HDstrcpy(attribute_name, t_attrname+1);

    /* adjust objectname to remove the attribute part. */
    if (objectname == t_attrname){
	/* the last slash is actually the only slash => this is an attribute of the root group.
	 * Need to preserve the only slash. */
	 *(++t_attrname) = '\0';
    }else{
	/* turn the found slash into a null terminator */
	*t_attrname = '\0';
    }

    /* All is well. Return success. */
    return(0);

error:
    /* clean up */
    /* nothing to clean yet. */

    /* return failed */
    return(-1);
}
