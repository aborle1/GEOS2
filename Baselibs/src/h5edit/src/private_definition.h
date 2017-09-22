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
/* private definitions of Standard functions */
#include <string.h>

/* macro short hands */
#define DEBUG			get_debug()
#define CMD_ERROR(mesg)		fprintf(stderr, mesg "\n"); \
				leave(EXIT_FAILURE)

/* Standard functions */
#define HDfclose(F)		fclose(F)
#define HDfopen(S,M)		fopen(S,M)
#define HDstrcmp(s1, s2)	strcmp(s1, s2)
#define HDstrrchr(s, c)		strrchr(s, c)
#define HDstrdup(s)		strdup(s)
#define HDstrcpy(s1, s2)	strcpy(s1, s2)
#define HDstrncpy(s1, s2, N)	strncpy(s1, s2, N)
#define HDmalloc(s)		malloc(s)
#define HDfree(s)		free(s)
#define HDgetenv(s)		getenv(s)
#define HDassert(s)		assert(s)
#define HDmemset(X,C,Z)		memset(X,C,Z)
#define HDperror(s)		perror(s)
#define HDstrlen(s)		strlen(s)
#define HDaccess(s,c)		access(s,c)
#define HDmemcpy(X,C,Z)		memcpy(X,C,Z)
#define HDremove(F)		remove(F)
#define HDfread(B,N,Z,F)	fread(B,N,Z,F)
#define HDfwrite(B,N,Z,F)	fwrite(B,N,Z,F)
#define HDrewind(F)		rewind(F)
