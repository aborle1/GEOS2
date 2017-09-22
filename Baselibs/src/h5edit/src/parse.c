#ifndef lint
static char const 
yyrcsid[] = "$FreeBSD: src/usr.bin/yacc/skeleton.c,v 1.28 2000/01/17 02:04:06 bde Exp $";
#endif
#include <stdlib.h>
#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define YYLEX yylex()
#define YYEMPTY -1
#define yyclearin (yychar=(YYEMPTY))
#define yyerrok (yyerrflag=0)
#define YYRECOVERING() (yyerrflag!=0)
static int yygrowstack();
#define yyparse H5EDITyyparse
#define yylex H5EDITyylex
#define yyerror H5EDITyyerror
#define yychar H5EDITyychar
#define yyval H5EDITyyval
#define yylval H5EDITyylval
#define yydebug H5EDITyydebug
#define yynerrs H5EDITyynerrs
#define yyerrflag H5EDITyyerrflag
#define yyss H5EDITyyss
#define yyssp H5EDITyyssp
#define yyvs H5EDITyyvs
#define yyvsp H5EDITyyvsp
#define yylhs H5EDITyylhs
#define yylen H5EDITyylen
#define yydefred H5EDITyydefred
#define yydgoto H5EDITyydgoto
#define yysindex H5EDITyysindex
#define yyrindex H5EDITyyrindex
#define yygindex H5EDITyygindex
#define yytable H5EDITyytable
#define yycheck H5EDITyycheck
#define yyname H5EDITyyname
#define yyrule H5EDITyyrule
#define yysslim H5EDITyysslim
#define yystacksize H5EDITyystacksize
#define YYPREFIX "H5EDITyy"
#line 8 "parse.y"
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


#line 93 "parse.y"
typedef union {
    int   ival;         /*for integer token*/
    double fval;	/*for floating point token */
    char  *sval;        /*for name string*/
} YYSTYPE;
#line 134 "parse.c"
#define YYERRCODE 256
#define CREATE_KWORD 257
#define DELETE_KWORD 258
#define RENAME_KWORD 259
#define COPY_KWORD 260
#define MODIFY_KWORD 261
#define ATTRIBUTE_KWORD 262
#define DATASET_KWORD 263
#define GROUP_KWORD 264
#define DATATYPE_KWORD 265
#define DATASPACE_KWORD 266
#define DATA_KWORD 267
#define IDENTIFIER 268
#define PATHNAME 269
#define QUOTED_STRING 270
#define DQUOTED_STRING 271
#define EOSTMT_TOKEN 272
#define WSPACE 273
#define INTEGER_TYPE_TOKEN 274
#define FLOAT_TYPE_TOKEN 275
#define H5T_STD_I8BE_TOKEN 276
#define H5T_STD_I8LE_TOKEN 277
#define H5T_STD_I16BE_TOKEN 278
#define H5T_STD_I16LE_TOKEN 279
#define H5T_STD_I32BE_TOKEN 280
#define H5T_STD_I32LE_TOKEN 281
#define H5T_STD_I64BE_TOKEN 282
#define H5T_STD_I64LE_TOKEN 283
#define H5T_STD_U8BE_TOKEN 284
#define H5T_STD_U8LE_TOKEN 285
#define H5T_STD_U16BE_TOKEN 286
#define H5T_STD_U16LE_TOKEN 287
#define H5T_STD_U32BE_TOKEN 288
#define H5T_STD_U32LE_TOKEN 289
#define H5T_STD_U64BE_TOKEN 290
#define H5T_STD_U64LE_TOKEN 291
#define H5T_NATIVE_CHAR_TOKEN 292
#define H5T_NATIVE_SCHAR_TOKEN 293
#define H5T_NATIVE_UCHAR_TOKEN 294
#define H5T_NATIVE_SHORT_TOKEN 295
#define H5T_NATIVE_USHORT_TOKEN 296
#define H5T_NATIVE_INT_TOKEN 297
#define H5T_NATIVE_UINT_TOKEN 298
#define H5T_NATIVE_LONG_TOKEN 299
#define H5T_NATIVE_ULONG_TOKEN 300
#define H5T_NATIVE_LLONG_TOKEN 301
#define H5T_NATIVE_ULLONG_TOKEN 302
#define H5T_IEEE_F32BE_TOKEN 303
#define H5T_IEEE_F32LE_TOKEN 304
#define H5T_IEEE_F64BE_TOKEN 305
#define H5T_IEEE_F64LE_TOKEN 306
#define H5T_NATIVE_FLOAT_TOKEN 307
#define H5T_NATIVE_DOUBLE_TOKEN 308
#define H5T_NATIVE_LDOUBLE_TOKEN 309
#define H5T_STRING_TOKEN 310
#define STRSIZE_TOKEN 311
#define STRPAD_TOKEN 312
#define CSET_TOKEN 313
#define CTYPE_TOKEN 314
#define H5T_VARIABLE_TOKEN 315
#define H5T_STR_NULLTERM_TOKEN 316
#define H5T_STR_NULLPAD_TOKEN 317
#define H5T_STR_SPACEPAD_TOKEN 318
#define H5T_CSET_ASCII_TOKEN 319
#define H5T_CSET_UTF8_TOKEN 320
#define H5T_C_S1_TOKEN 321
#define H5T_FORTRAN_S1_TOKEN 322
#define H5T_COMPOUND_TOKEN 323
#define H5T_ARRAY_TOKEN 324
#define H5T_VLEN_TOKEN 325
#define STRING_CONST 326
#define INTEGER_CONST 327
#define FLOAT_CONST 328
#define SCALAR_TOKEN 329
#define SIMPLE_TOKEN 330
#define NULL_TOKEN 331
const short H5EDITyylhs[] = {                                        -1,
    0,    0,    1,    2,    2,    2,    2,    2,    4,    4,
    5,    5,    7,    7,    8,    9,   10,   11,   12,   14,
   13,   25,   15,   18,   20,   27,   28,   19,   21,   22,
   16,   16,   16,   29,   30,   26,   26,   26,   26,    3,
   23,   23,   31,   32,   17,   33,   33,   33,   33,   34,
   34,   36,   37,   37,   37,   40,   41,   42,   43,   43,
   38,   39,   35,   35,   44,   44,   44,   46,   45,   45,
   49,   48,   50,   51,   51,   52,   47,   24,   24,   55,
   57,   53,   54,   56,   56,   59,   58,   58,   58,    6,
    6,   60,   61,
};
const short H5EDITyylen[] = {                                         2,
    0,    2,    2,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    3,    4,    2,    3,    3,
    5,    0,    4,    1,    1,    1,    1,    1,    1,    1,
    1,    2,    2,    1,    1,    1,    1,    1,    1,    1,
    1,    2,    1,    1,    3,    3,    2,    2,    1,    2,
    1,    1,    1,    1,    1,    4,    1,    2,    1,    1,
    1,    1,    2,    1,    1,    1,    1,    1,    2,    1,
    0,    5,    1,    0,    2,    2,    1,    2,    1,    0,
    0,    6,    1,    0,    2,    2,    1,    1,    1,    1,
    1,    4,    3,
};
const short H5EDITyydefred[] = {                                      1,
    0,    0,    0,    0,    0,    0,    2,    0,    4,    5,
    6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
   90,   91,    0,    0,   36,   37,   38,   39,    0,   34,
   31,    0,    0,   28,    0,    0,   22,   41,   40,    3,
   32,   35,   33,    0,   16,    0,   24,   19,    0,    0,
   93,    0,   25,   44,   42,    0,    0,    0,    0,   61,
   62,    0,   80,   68,   71,   77,   49,    0,    0,    0,
   51,   52,   53,   54,   55,   64,   65,   66,   67,   70,
    0,   79,   17,   27,   92,   29,    0,   23,   50,   63,
   78,    0,    0,   69,   45,   47,    0,   48,    0,   21,
   30,    0,    0,   57,   59,   60,   87,   88,   89,   84,
   83,   46,   73,   74,   58,   56,    0,    0,    0,    0,
   85,   72,    0,   75,   86,   82,   76,
};
const short H5EDITyydgoto[] = {                                       1,
    7,    8,   40,    9,   10,   11,   12,   13,   14,   15,
   16,   17,   18,   19,   20,   29,   45,   46,   35,   52,
   87,  100,   37,   67,   56,   30,   50,   51,   31,   43,
   38,   55,   68,   69,   70,   71,   72,   73,   74,   75,
  103,  104,  109,   76,   77,   78,   79,   80,   81,  114,
  118,  124,   82,  110,   93,  117,  120,  111,  121,   21,
   22,
};
const short H5EDITyysindex[] = {                                      0,
 -228, -244, -244, -244, -257, -244,    0, -238,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0, -227, -227,    0,    0,    0,    0, -115,    0,
    0, -227, -227,    0, -227, -227,    0,    0,    0,    0,
    0,    0,    0, -123,    0,  -76,    0,    0,    0, -227,
    0, -213,    0,    0,    0, -122, -259, -276,  -66,    0,
    0,  -65,    0,    0,    0,    0,    0,  -64, -120, -122,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   19,    0,    0,    0,    0,    0, -227,    0,    0,    0,
    0, -251, -265,    0,    0,    0, -122,    0, -263,    0,
    0, -261,  -58,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   24,    8, -265,  -56,
    0,    0, -256,    0,    0,    0,    0,
};
const short H5EDITyyrindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0, -202,    0,    0,    0, -119,    0,    0,    0,    0,
    0,    0,    0,   32,    0,    0,    0,    0, -232,    0,
    0, -199,    0,    0,    0,    0,    0,   32,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   32,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  -51,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,
};
const short H5EDITyygindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   42,   29,  -15,    0,    0,
    0,    0,    0,  -47,    0,  -22,    0,   26,   54,    0,
    0,    0,    0,    0,    9,   22,    0,    0,    0,    0,
    0,    0,    0,   23,    0,    0,    0,   15,    0,    0,
    0,    0,   25,    0,    0,    0,    0,  -37,    0,    0,
    0,
};
#define YYTABLESIZE 211
const short H5EDITyytable[] = {                                      63,
   63,   42,   63,   43,  105,  106,   47,   44,   88,   47,
   47,   34,   53,   47,   60,   61,   48,   49,   23,   24,
   54,   96,   98,   25,   26,   27,   28,   47,    2,    3,
    4,    5,    6,   39,   84,   26,   26,   26,   26,   27,
   25,   26,   27,   28,   32,   33,   44,   36,  122,  112,
   62,  123,   64,   65,   66,   86,   63,   92,   99,  102,
   95,  107,  108,  113,  101,  115,  116,  119,  126,   18,
  127,   71,   20,   81,   83,   85,   41,   97,   89,   94,
   90,  125,    0,   91,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   57,   58,   59,   59,   58,   59,   43,    0,    0,
   60,   61,   25,   26,   27,   28,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   62,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   64,   65,   66,   64,   65,
   66,
};
const short H5EDITyycheck[] = {                                     123,
  123,   24,  123,  123,  270,  271,   29,  123,   56,   32,
   33,  269,   35,   36,  274,  275,   32,   33,  263,  264,
   36,   69,   70,  268,  269,  270,  271,   50,  257,  258,
  259,  260,  261,  272,   50,  268,  269,  270,  271,  272,
  268,  269,  270,  271,    3,    4,  123,    6,   41,   97,
  310,   44,  329,  330,  331,  269,  123,  123,   40,  311,
  125,  327,  328,  327,   87,  327,  125,   44,  125,  272,
  327,   40,  272,  125,   46,   50,   23,   69,   57,   65,
   58,  119,   -1,   59,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  265,  266,  267,  267,  266,  267,  267,   -1,   -1,
  274,  275,  268,  269,  270,  271,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  310,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  329,  330,  331,  329,  330,
  331,
};
#define YYFINAL 1
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 331
#if YYDEBUG
const char * const H5EDITyyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,"'('","')'",0,0,"','",0,0,0,0,0,0,0,0,0,0,0,0,0,"':'",0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'['",0,"']'",0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'{'",0,"'}'",0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
"CREATE_KWORD","DELETE_KWORD","RENAME_KWORD","COPY_KWORD","MODIFY_KWORD",
"ATTRIBUTE_KWORD","DATASET_KWORD","GROUP_KWORD","DATATYPE_KWORD",
"DATASPACE_KWORD","DATA_KWORD","IDENTIFIER","PATHNAME","QUOTED_STRING",
"DQUOTED_STRING","EOSTMT_TOKEN","WSPACE","INTEGER_TYPE_TOKEN",
"FLOAT_TYPE_TOKEN","H5T_STD_I8BE_TOKEN","H5T_STD_I8LE_TOKEN",
"H5T_STD_I16BE_TOKEN","H5T_STD_I16LE_TOKEN","H5T_STD_I32BE_TOKEN",
"H5T_STD_I32LE_TOKEN","H5T_STD_I64BE_TOKEN","H5T_STD_I64LE_TOKEN",
"H5T_STD_U8BE_TOKEN","H5T_STD_U8LE_TOKEN","H5T_STD_U16BE_TOKEN",
"H5T_STD_U16LE_TOKEN","H5T_STD_U32BE_TOKEN","H5T_STD_U32LE_TOKEN",
"H5T_STD_U64BE_TOKEN","H5T_STD_U64LE_TOKEN","H5T_NATIVE_CHAR_TOKEN",
"H5T_NATIVE_SCHAR_TOKEN","H5T_NATIVE_UCHAR_TOKEN","H5T_NATIVE_SHORT_TOKEN",
"H5T_NATIVE_USHORT_TOKEN","H5T_NATIVE_INT_TOKEN","H5T_NATIVE_UINT_TOKEN",
"H5T_NATIVE_LONG_TOKEN","H5T_NATIVE_ULONG_TOKEN","H5T_NATIVE_LLONG_TOKEN",
"H5T_NATIVE_ULLONG_TOKEN","H5T_IEEE_F32BE_TOKEN","H5T_IEEE_F32LE_TOKEN",
"H5T_IEEE_F64BE_TOKEN","H5T_IEEE_F64LE_TOKEN","H5T_NATIVE_FLOAT_TOKEN",
"H5T_NATIVE_DOUBLE_TOKEN","H5T_NATIVE_LDOUBLE_TOKEN","H5T_STRING_TOKEN",
"STRSIZE_TOKEN","STRPAD_TOKEN","CSET_TOKEN","CTYPE_TOKEN","H5T_VARIABLE_TOKEN",
"H5T_STR_NULLTERM_TOKEN","H5T_STR_NULLPAD_TOKEN","H5T_STR_SPACEPAD_TOKEN",
"H5T_CSET_ASCII_TOKEN","H5T_CSET_UTF8_TOKEN","H5T_C_S1_TOKEN",
"H5T_FORTRAN_S1_TOKEN","H5T_COMPOUND_TOKEN","H5T_ARRAY_TOKEN","H5T_VLEN_TOKEN",
"STRING_CONST","INTEGER_CONST","FLOAT_CONST","SCALAR_TOKEN","SIMPLE_TOKEN",
"NULL_TOKEN",
};
const char * const H5EDITyyrule[] = {
"$accept : editstmts",
"editstmts :",
"editstmts : editstmts editstmt",
"editstmt : editcmd eostmt",
"editcmd : create_cmd",
"editcmd : delete_cmd",
"editcmd : rename_cmd",
"editcmd : copy_cmd",
"editcmd : modify_cmd",
"create_cmd : create_cmd1",
"create_cmd : create_cmd2",
"delete_cmd : delete_cmd1",
"delete_cmd : delete_cmd2",
"copy_cmd : copy_cmd1",
"copy_cmd : copy_cmd2",
"modify_cmd : modify_cmd1",
"create_cmd1 : CREATE_KWORD targetobject attribute_definition",
"create_cmd2 : CREATE_KWORD targetobject attributename attribute_definition",
"delete_cmd1 : DELETE_KWORD targetobject",
"delete_cmd2 : DELETE_KWORD targetobject attributename",
"copy_cmd2 : COPY_KWORD sourceobjectname copyparam2",
"copy_cmd1 : COPY_KWORD sourceobjectname copyparam2 destinationobjectname destinationattributename",
"$$1 :",
"modify_cmd1 : MODIFY_KWORD targetobject_attribute $$1 data_stmt",
"attributename : objectname",
"copyparam2 : objectname",
"attrname_old : attributename",
"attrname_new : attributename",
"sourceobjectname : PATHNAME",
"destinationobjectname : PATHNAME",
"destinationattributename : objectname",
"targetobject : datasetname",
"targetobject : DATASET_KWORD datasetname",
"targetobject : GROUP_KWORD groupname",
"datasetname : objectname",
"groupname : objectname",
"objectname : IDENTIFIER",
"objectname : PATHNAME",
"objectname : QUOTED_STRING",
"objectname : DQUOTED_STRING",
"eostmt : EOSTMT_TOKEN",
"targetobject_attribute : targetobject_attribute_def",
"targetobject_attribute : targetobject attribute_def",
"targetobject_attribute_def : targetobject",
"attribute_def : attributename",
"attribute_definition : '{' attribute_body_definition '}'",
"attribute_body_definition : datatype_stmt dataspace_stmt data_stmt",
"attribute_body_definition : datatype_stmt data_stmt",
"attribute_body_definition : dataspace_stmt data_stmt",
"attribute_body_definition : data_stmt",
"datatype_stmt : DATATYPE_KWORD datatype_definition",
"datatype_stmt : datatype_definition",
"datatype_definition : datatype_class",
"datatype_class : integer_class",
"datatype_class : floatpoint_class",
"datatype_class : string_class",
"string_class : H5T_STRING_TOKEN '{' string_type_definition '}'",
"string_type_definition : string_size_definition",
"string_size_definition : STRSIZE_TOKEN INTEGER_CONST",
"string_const : QUOTED_STRING",
"string_const : DQUOTED_STRING",
"integer_class : INTEGER_TYPE_TOKEN",
"floatpoint_class : FLOAT_TYPE_TOKEN",
"dataspace_stmt : DATASPACE_KWORD dataspace_definition",
"dataspace_stmt : dataspace_definition",
"dataspace_definition : dataspace_simple",
"dataspace_definition : dataspace_scalar",
"dataspace_definition : dataspace_null",
"dataspace_scalar : SCALAR_TOKEN",
"dataspace_simple : SIMPLE_TOKEN dataspace_dims_list",
"dataspace_simple : dataspace_dims_list",
"$$2 :",
"dataspace_dims_list : $$2 '(' dim_firstone dims_list ')'",
"dim_firstone : INTEGER_CONST",
"dims_list :",
"dims_list : dims_list dim_more",
"dim_more : ',' INTEGER_CONST",
"dataspace_null : NULL_TOKEN",
"data_stmt : DATA_KWORD data_definition",
"data_stmt : data_definition",
"$$3 :",
"$$4 :",
"data_definition : '{' $$3 data_firstval data_list $$4 '}'",
"data_firstval : data_val",
"data_list :",
"data_list : data_list data_more",
"data_more : ',' data_val",
"data_val : INTEGER_CONST",
"data_val : FLOAT_CONST",
"data_val : string_const",
"rename_cmd : rename_cmd1",
"rename_cmd : rename_cmd2",
"rename_cmd1 : RENAME_KWORD targetobject attrname_old attrname_new",
"rename_cmd2 : RENAME_KWORD targetobject attrname_new",
};
#endif
#if YYDEBUG
#include <stdio.h>
#endif
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 10000
#define YYMAXDEPTH 10000
#endif
#endif
#define YYINITSTACKSIZE 200
int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short *yyss;
short *yysslim;
YYSTYPE *yyvs;
int yystacksize;
#line 1106 "parse.y"

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
#line 682 "parse.c"
/* allocate initial stack or double stack size, up to YYMAXDEPTH */
static int yygrowstack()
{
    int newsize, i;
    short *newss;
    YYSTYPE *newvs;

    if ((newsize = yystacksize) == 0)
        newsize = YYINITSTACKSIZE;
    else if (newsize >= YYMAXDEPTH)
        return -1;
    else if ((newsize *= 2) > YYMAXDEPTH)
        newsize = YYMAXDEPTH;
    i = yyssp - yyss;
    newss = yyss ? (short *)realloc(yyss, newsize * sizeof *newss) :
      (short *)malloc(newsize * sizeof *newss);
    if (newss == NULL)
        return -1;
    yyss = newss;
    yyssp = newss + i;
    newvs = yyvs ? (YYSTYPE *)realloc(yyvs, newsize * sizeof *newvs) :
      (YYSTYPE *)malloc(newsize * sizeof *newvs);
    if (newvs == NULL)
        return -1;
    yyvs = newvs;
    yyvsp = newvs + i;
    yystacksize = newsize;
    yysslim = yyss + newsize - 1;
    return 0;
}

#define YYABORT goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab

#ifndef YYPARSE_PARAM
#if defined(__cplusplus) || __STDC__
#define YYPARSE_PARAM_ARG void
#define YYPARSE_PARAM_DECL
#else	/* ! ANSI-C/C++ */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif	/* ANSI-C/C++ */
#else	/* YYPARSE_PARAM */
#ifndef YYPARSE_PARAM_TYPE
#define YYPARSE_PARAM_TYPE void *
#endif
#if defined(__cplusplus) || __STDC__
#define YYPARSE_PARAM_ARG YYPARSE_PARAM_TYPE YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else	/* ! ANSI-C/C++ */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL YYPARSE_PARAM_TYPE YYPARSE_PARAM;
#endif	/* ANSI-C/C++ */
#endif	/* ! YYPARSE_PARAM */

int
yyparse (YYPARSE_PARAM_ARG)
    YYPARSE_PARAM_DECL
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register const char *yys;

    if ((yys = getenv("YYDEBUG")))
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    if (yyss == NULL && yygrowstack()) goto yyoverflow;
    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if ((yyn = yydefred[yystate])) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yyssp >= yysslim && yygrowstack())
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;
#if defined(lint) || defined(__GNUC__)
    goto yynewerror;
#endif
yynewerror:
    yyerror("syntax error");
#if defined(lint) || defined(__GNUC__)
    goto yyerrlab;
#endif
yyerrlab:
    ++yynerrs;
yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yysslim && yygrowstack())
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 3:
#line 149 "parse.y"
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
break;
case 16:
#line 193 "parse.y"
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
break;
case 17:
#line 230 "parse.y"
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
break;
case 18:
#line 273 "parse.y"
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
break;
case 19:
#line 299 "parse.y"
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
break;
case 20:
#line 331 "parse.y"
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
break;
case 21:
#line 381 "parse.y"
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
break;
case 22:
#line 417 "parse.y"
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
break;
case 23:
#line 439 "parse.y"
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
break;
case 24:
#line 469 "parse.y"
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
break;
case 25:
#line 483 "parse.y"
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
break;
case 26:
#line 496 "parse.y"
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
break;
case 27:
#line 515 "parse.y"
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
break;
case 28:
#line 529 "parse.y"
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
break;
case 29:
#line 543 "parse.y"
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
break;
case 30:
#line 557 "parse.y"
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
break;
case 34:
#line 577 "parse.y"
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
break;
case 35:
#line 591 "parse.y"
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
break;
case 43:
#line 621 "parse.y"
{
			    if (get_debug()) fprintf(stderr, "found targetobject_attribute_def\n");
			    /* retrieve the definitions of the old attribute */
			    if (0 != split_object_attribute(group_name, dataset_name)){
				fprintf(stderr, "failed to split object_attribute names\n");
				leave(EXIT_FAILURE);
			    }
			}
break;
case 44:
#line 632 "parse.y"
{
			    /* nothing yet */
			    if (get_debug()) fprintf(stderr, "in attribute_def\n");
			}
break;
case 45:
#line 641 "parse.y"
{ if (get_debug()) fprintf(stderr, "found attribute definition\n");

			}
break;
case 56:
#line 666 "parse.y"
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
break;
case 57:
#line 690 "parse.y"
{
			    if (get_debug()) fprintf(stderr, "found string size\n");
			}
break;
case 58:
#line 696 "parse.y"
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
break;
case 61:
#line 714 "parse.y"
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
break;
case 62:
#line 730 "parse.y"
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
break;
case 68:
#line 756 "parse.y"
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
break;
case 71:
#line 777 "parse.y"
{
			    if (get_debug()) fprintf(stderr, "looking for dimlist\n");
			    attr_def_old.space_class = H5S_SIMPLE;
			    /* reset attribute dimension information */
			    HDmemset(&attribute_dims, 0, sizeof(attribute_dims));
			}
break;
case 72:
#line 785 "parse.y"
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
break;
case 73:
#line 852 "parse.y"
{   /* first dimension */
			    if (H5EDITyylval.ival <=0){
				fprintf(stderr, "Dimension must be positive. Got %d.\n",
				    H5EDITyylval.ival);
				leave(EXIT_FAILURE);
			    }
			    attribute_dims.dims[attribute_dims.ndims] = H5EDITyylval.ival;
			    attribute_dims.ndims++;
			}
break;
case 76:
#line 868 "parse.y"
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
break;
case 80:
#line 893 "parse.y"
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
break;
case 81:
#line 918 "parse.y"
{
			    if (attribute_data_gotten < attr_def_old.data_size){
				fprintf(stderr, "Fewer data than needed. "
				    "Need %d but got %d data only\n",
				    attr_def_old.data_size, attribute_data_gotten);
				leave(EXIT_FAILURE);
			    }
			}
break;
case 82:
#line 927 "parse.y"
{ /* no action */
			}
break;
case 87:
#line 942 "parse.y"
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
break;
case 88:
#line 956 "parse.y"
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
break;
case 89:
#line 969 "parse.y"
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
break;
case 92:
#line 1021 "parse.y"
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
break;
case 93:
#line 1067 "parse.y"
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
break;
#line 1717 "parse.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yyssp, yystate);
#endif
    if (yyssp >= yysslim && yygrowstack())
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
yyoverflow:
    yyerror("yacc stack overflow");
yyabort:
    return (1);
yyaccept:
    return (0);
}
