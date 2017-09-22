/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * This file was adapted from the H5LTanaylze.l of the HDF5 code.            *
 * This is used to generate the anaylze.c by                                 *
 *   lex -PH5EDITyy -oanalyze.c analyze.l				     *
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

#include <stdlib.h>
#include <string.h>
#include <h5edit.h>
#include "parse.h"

static int H5EDIT_yyinput(char *, int);
#undef YY_INPUT
#define YY_INPUT(b, r, ms) (r=H5EDIT_yyinput(b, ms))
#define token(x)        (int)x

/* Save these for debug purpose. */
#ifdef  YY_BUF_SIZE
#undef  YY_BUF_SIZE
#endif
#define YY_BUF_SIZE        100    /*Define read buffer to be 256K*/

extern char *h5edit_myinput;
extern int  h5edit_input_len;

#define STACK_SIZE      16

/*variables for compound type*/
struct cmpd_info {
    hid_t       id;
    hbool_t         is_field;
    hbool_t         first_memb;
};
extern struct cmpd_info h5edit_cmpd_stack[STACK_SIZE];
extern int h5edit_csindex;

/*variables for array type*/
struct arr_info {
    hsize_t             dims[H5S_MAX_RANK];
    int                 ndim;
    hbool_t             is_dim;
};
extern struct arr_info arr_stack[STACK_SIZE];
extern int h5edit_asindex;

/*variables for enumerate type*/
extern hbool_t     h5edit_is_enum;
extern hbool_t     h5edit_is_enum_memb;

/*variables for string type*/
extern hbool_t h5edit_is_str_size;

/*variables for opaque type*/
extern hbool_t h5edit_is_opq_size;
extern hbool_t h5edit_is_opq_tag;

static hbool_t        first_quote = 1;

/* For Lex and Yacc */
/*int  input_len;
char *myinput;*/
    
%}

/* short hand definitions */
/* digits */
digit	[0-9]
/* letters */
L	[a-z][A-Z]
/* Upper case letters */
UL	[A-Z]
/* Lower case letters */
LL	[a-z]
/* White spaces: space, tab, or newline */
WB	[ \t\n]
/* Non white space */
NWB	[^ \t\n]
/* No single quote */
NQUOTE  [^']
/* double quote mark */
DQ	\"
/* No double quote */
NDQUOTE [^"]

%%

CREATE		{return token(CREATE_KWORD);}
DELETE		{return token(DELETE_KWORD);}
RENAME		{return token(RENAME_KWORD);}
COPY		{return token(COPY_KWORD);}
MODIFY		{return token(MODIFY_KWORD);}
ATTRIBUTE	{return token(ATTRIBUTE_KWORD);}
DATASET		{return token(DATASET_KWORD);}
GROUP		{return token(GROUP_KWORD);}
DATA		{return token(DATA_KWORD);}
DATATYPE	{return token(DATATYPE_KWORD);}
DATASPACE	{return token(DATASPACE_KWORD);}

H5T_STD_I8BE     {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_I8LE     {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_I16BE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_I16LE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_I32BE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_I32LE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_I64BE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_I64LE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}

H5T_STD_U8BE     {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_U8LE     {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_U16BE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_U16LE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_U32BE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_U32LE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_U64BE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_STD_U64LE    {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}

H5T_NATIVE_CHAR  {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_NATIVE_SCHAR {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_NATIVE_UCHAR {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_NATIVE_SHORT {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_NATIVE_USHORT  {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_NATIVE_INT   {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_NATIVE_UINT  {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_NATIVE_LONG  {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_NATIVE_ULONG {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_NATIVE_LLONG   {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}
H5T_NATIVE_ULLONG  {H5EDITyylval.sval = yytext; return token(INTEGER_TYPE_TOKEN);}

H5T_IEEE_F32BE   {H5EDITyylval.sval = yytext; return token(FLOAT_TYPE_TOKEN);}
H5T_IEEE_F32LE   {H5EDITyylval.sval = yytext; return token(FLOAT_TYPE_TOKEN);}
H5T_IEEE_F64BE   {H5EDITyylval.sval = yytext; return token(FLOAT_TYPE_TOKEN);}
H5T_IEEE_F64LE   {H5EDITyylval.sval = yytext; return token(FLOAT_TYPE_TOKEN);}
H5T_NATIVE_FLOAT   {H5EDITyylval.sval = yytext; return token(FLOAT_TYPE_TOKEN);}
H5T_NATIVE_DOUBLE  {H5EDITyylval.sval = yytext; return token(FLOAT_TYPE_TOKEN);}
H5T_NATIVE_LDOUBLE {H5EDITyylval.sval = yytext; return token(FLOAT_TYPE_TOKEN);}

H5T_STRING	{return token(H5T_STRING_TOKEN);}
STRSIZE		{return token(STRSIZE_TOKEN);}

SIMPLE		{return token(SIMPLE_TOKEN);}
SCALAR		{return token(SCALAR_TOKEN);}
NULL		{return token(NULL_TOKEN);}

[a-zA-Z][a-zA-Z0-9_/]*	{ H5EDITyylval.sval = yytext;
		    if (get_debug()) fprintf(stderr, "Lex: found an identifier(%s)\n",
		    yytext); return(IDENTIFIER);}

";"		{ if (get_debug()) fprintf(stderr, "Lex: end of statment\n"); return(EOSTMT_TOKEN);}

"/"[^ \t;]*	{ H5EDITyylval.sval = yytext;
		    if (get_debug()) fprintf(stderr, "Lex: found a pathname(%s)\n",
		    yytext); return(PATHNAME);}

-?([0-9]+) { H5EDITyylval.ival = atoi(yytext);
		    if (get_debug()) fprintf(stderr, "Lex: found a number(%s), value=%d\n",
		    yytext, H5EDITyylval.ival); return(INTEGER_CONST);}

-?(([0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?) {
		    H5EDITyylval.fval = atof(yytext);
		    if (get_debug()) fprintf(stderr, "Lex: found a float value(%s), value=%f\n",
			yytext, H5EDITyylval.fval);
		    return(FLOAT_CONST);}

'({NQUOTE}|'')+' { if (get_debug()) fprintf(stderr, "Lex: found a quoted strings(%s)\n",
		    yytext);
		    if (get_debug()) fprintf(stderr, "Lex: yyleng is %d\n", yyleng);
		    /* need to stripe the leading and ending quotes */
		    *(yytext+yyleng-1) = '\0';      /* turn ending quote into null */
		    H5EDITyylval.sval = yytext+1; /* pass the leading quote */
		    return(QUOTED_STRING);
		}

{DQ}({NDQUOTE}|{DQ}{DQ})+{DQ} {
		    if (get_debug()) fprintf(stderr, "Lex: found a double quoted strings(%s)\n",
			yytext);
		    if (get_debug()) fprintf(stderr, "Lex: yyleng is %d\n", yyleng);
		    /* need to stripe the leading and ending double quotes */
		    *(yytext+yyleng-1) = '\0';      /* turn ending quote into null */
		    H5EDITyylval.sval = yytext+1; /* pass the leading quote */
		    return(DQUOTED_STRING);
		}

"{"              {return token('{');}
"}"              {return token('}');}
"["              {return token('[');}
"]"              {return token(']');}
"("              {return token('(');}
")"              {return token(')');}
":"              {return token(':');}
","              {return token(',');}

[ \t\n]		/* ignore all white spaces (space, TAB, newline) */

.		{/* anything unexpected */
		fprintf(stderr, "unexpected input=%0x\n", *(yytext));}

%%
/* Try to read input from command file (h5edit_commfile) if provided.
 * Otherwise, try get it from command string (h5edit_command).
 * Return value: Positive if it is successful;
 *    0 if there is no more input;
 *    negative if error.
 */
static int H5EDIT_yyinput(char *buf, int max_size)
{
    int ret = 0;

    if (h5edit_commfile != NULL){
	/* read it from command file */
	ret = (int)fread(buf, (size_t) 1, (size_t) max_size, h5edit_commfile);
    }else{
	/* read it from command string */
	if (h5edit_input_len > 0 ) {
	    /* There are more available input */
	    if (h5edit_input_len <= max_size){
		memcpy(buf, h5edit_command, h5edit_input_len); 
		ret = h5edit_input_len;
		/* no more input to give next time */
		h5edit_input_len = 0;
		h5edit_command = NULL; 
	    }else{
		/* give at most max_size input */
		memcpy(buf, h5edit_command, max_size);
		ret = max_size;
		/* decrease available input by max_size */
		h5edit_input_len -= max_size;
		h5edit_command += max_size;
	    }
	}
    }

    return ret;
}

int H5EDITyyerror(char *msg)
{
   fprintf(stderr, "h5edit analyser ERROR: %s at \"%s\".\n", msg, yytext);
}

int H5EDITyywrap()
{
    if (get_debug())
	fprintf(stderr, "Lex: end of input\n");
    return(1);
}
