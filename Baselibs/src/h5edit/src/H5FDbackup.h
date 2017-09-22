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

/*
 * Purpose:	The header file for the backup driver.
 */
#ifndef H5FDbackup_H
#define H5FDbackup_H

#include "H5Ipublic.h"
#include "private_definition.h"

#define H5FD_BACKUP	(H5FD_backup_init())

/* Definitions of structs used to manage segment-indexing. */
typedef struct H5FD_backup_segment_t {
    int segment_factor;		/* 2^segment_factor = setment_size. */
    int segment_size;		/* size of the segments in the file. Power of 2 and less than 2GB for now. */
    int segment_array_size;	/* size of segment array */
    uint8_t *segment_array;	/* pointer to segment array */
} H5FD_backup_segment_t;


#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5FD_backup_init(void);
H5_DLL void H5FD_backup_term(void);
H5_DLL herr_t H5Pset_fapl_backup(hid_t fapl_id);

#ifdef __cplusplus
}
#endif

#endif
