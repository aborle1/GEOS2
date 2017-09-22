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
 * This module is modeled after the HDF5 stdio driver module.
 */

/* Programmer:  Albert Cheng
 *              Feb 1, 2014
 *
 * Purpose: The Backup virtual file driver supports the backup protocol.
 *          It bases on the STDIO driver.
 *
 * Modifications:
 *	Added the segment management routines for backup performance.
 *      Albert Cheng, Jun 25, 2014.
 *
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "hdf5.h"
#include "H5FDbackup.h"

#ifdef H5_HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef H5_HAVE_WIN32_API
/* The following two defines must be before any windows headers are included */
#define WIN32_LEAN_AND_MEAN    /* Exclude rarely-used stuff from Windows headers */
#define NOGDI                  /* Exclude Graphic Display Interface macros */

#include <windows.h>
#include <io.h>

/* This is not defined in the Windows header files */
#ifndef F_OK
#define F_OK 00
#endif

#endif

#ifdef MAX
#undef MAX
#endif /* MAX */
#define MAX(X,Y)  ((X)>(Y)?(X):(Y))

/* local size definitions */
#define	UPDATESIZE	(1<<15)		/* 2^15 == 32KB */

/* The driver identification number, initialized at runtime */
static hid_t H5FD_BACKUP_g = 0;

/* A glocal cheat variable */
extern char *backupname_g;

/* The maximum number of bytes which can be written in a single I/O operation */
static size_t H5_BACKUP_MAX_IO_BYTES_g = (size_t)-1;

/* File operations */
typedef enum {
    H5FD_BACKUP_OP_UNKNOWN=0,
    H5FD_BACKUP_OP_READ=1,
    H5FD_BACKUP_OP_WRITE=2,
    H5FD_BACKUP_OP_SEEK=3
} H5FD_backup_file_op;

/* The description of a file belonging to this driver. The 'eoa' and 'eof'
 * determine the amount of hdf5 address space in use and the high-water mark
 * of the file (the current size of the underlying Unix file). The 'pos'
 * value is used to eliminate file position updates when they would be a
 * no-op. Unfortunately we've found systems that use separate file position
 * indicators for reading and writing so the lseek can only be eliminated if
 * the current operation is the same as the previous operation.  When opening
 * a file the 'eof' will be set to the current file size, 'eoa' will be set
 * to zero, 'pos' will be set to H5F_ADDR_UNDEF (as it is when an error
 * occurs), and 'op' will be set to H5F_OP_UNKNOWN.
 */
typedef struct H5FD_backup_t {
    H5FD_t      pub;            /* public stuff, must be first      */
    FILE        *fp;            /* the file handle                  */
    int         fd;             /* file descriptor (for truncate)   */
    haddr_t     eoa;            /* end of allocated region          */
    haddr_t     eof;            /* end of file; current file size   */
    haddr_t     pos;            /* current file I/O position        */
    unsigned    write_access;   /* Flag to indicate the file was opened with write access */
    H5FD_backup_file_op op;  /* last operation */
    FILE	*backup_fp;     /* file handle of the backup file   */
    char	*backup_name;	/* name of the backup file          */
    FILE	*ronly_fp;      /* readonly handle of the data file */
    H5FD_backup_segment_t *segment_struct; /* segment index structure   */

#ifndef H5_HAVE_WIN32_API
    /* On most systems the combination of device and i-node number uniquely
     * identify a file.  Note that Cygwin, MinGW and other Windows POSIX
     * environments have the stat function (which fakes inodes)
     * and will use the 'device + inodes' scheme as opposed to the
     * Windows code further below.
     */
    dev_t           device;     /* file device number   */
#ifdef H5_VMS
    ino_t           inode[3];   /* file i-node number   */
#else
    ino_t           inode;      /* file i-node number   */
#endif /* H5_VMS */
#else
    /* Files in windows are uniquely identified by the volume serial
     * number and the file index (both low and high parts).
     *
     * There are caveats where these numbers can change, especially
     * on FAT file systems.  On NTFS, however, a file should keep
     * those numbers the same until renamed or deleted (though you
     * can use ReplaceFile() on NTFS to keep the numbers the same
     * while renaming).
     *
     * See the MSDN "BY_HANDLE_FILE_INFORMATION Structure" entry for
     * more information.
     *
     * http://msdn.microsoft.com/en-us/library/aa363788(v=VS.85).aspx
     */
    DWORD           nFileIndexLow;
    DWORD           nFileIndexHigh;
    DWORD           dwVolumeSerialNumber;
    
    HANDLE          hFile;      /* Native windows file handle */
#endif  /* H5_HAVE_WIN32_API */
} H5FD_backup_t;

/* Use similar structure as in H5private.h by defining Windows stuff first. */
#ifdef H5_HAVE_WIN32_API
#ifndef H5_HAVE_MINGW
    #define file_fseek      _fseeki64
    #define file_offset_t   __int64
    #define file_ftruncate  _chsize_s   /* Supported in VS 2005 or newer */
    #define file_ftell      _ftelli64
#endif /* H5_HAVE_MINGW */
#endif /* H5_HAVE_WIN32_API */

/* Use file_xxx to indicate these are local macros, avoiding confusing 
 * with the global HD_xxx macros. 
 * Assume fseeko, which is POSIX standard, is always supported; 
 * but prefer to use fseeko64 if supported. 
 */
#ifndef file_fseek
    #ifdef H5_HAVE_FSEEKO64
        #define file_fseek      fseeko64
        #define file_offset_t   off64_t
        #define file_ftruncate  ftruncate64
        #define file_ftell      ftello64
    #else
        #define file_fseek      fseeko
        #define file_offset_t   off_t
        #define file_ftruncate  ftruncate
        #define file_ftell      ftello
    #endif /* H5_HAVE_FSEEKO64 */
#endif /* file_fseek */

/* These macros check for overflow of various quantities.  These macros
 * assume that file_offset_t is signed and haddr_t and size_t are unsigned.
 *
 * ADDR_OVERFLOW:  Checks whether a file address of type `haddr_t'
 *      is too large to be represented by the second argument
 *      of the file seek function.
 *
 * SIZE_OVERFLOW:  Checks whether a buffer size of type `hsize_t' is too
 *      large to be represented by the `size_t' type.
 *
 * REGION_OVERFLOW:  Checks whether an address and size pair describe data
 *      which can be addressed entirely by the second
 *      argument of the file seek function.
 */
/* adding for windows NT filesystem support. */
#define MAXADDR (((haddr_t)1<<(8*sizeof(file_offset_t)-1))-1)
#define ADDR_OVERFLOW(A)  (HADDR_UNDEF==(A) || ((A) & ~(haddr_t)MAXADDR))
#define SIZE_OVERFLOW(Z)  ((Z) & ~(hsize_t)MAXADDR)
#define REGION_OVERFLOW(A,Z)  (ADDR_OVERFLOW(A) || SIZE_OVERFLOW(Z) || \
    HADDR_UNDEF==(A)+(Z) || (file_offset_t)((A)+(Z))<(file_offset_t)(A))

/* Prototypes */
static H5FD_t *H5FD_backup_open(const char *name, unsigned flags,
                 hid_t fapl_id, haddr_t maxaddr);
static herr_t H5FD_backup_close(H5FD_t *lf);
static int H5FD_backup_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t H5FD_backup_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_backup_alloc(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size);
static haddr_t H5FD_backup_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_backup_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD_backup_get_eof(const H5FD_t *_file);
static herr_t  H5FD_backup_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle);
static herr_t H5FD_backup_read(H5FD_t *lf, H5FD_mem_t type, hid_t fapl_id, haddr_t addr,
                size_t size, void *buf);
static herr_t H5FD_backup_write(H5FD_t *lf, H5FD_mem_t type, hid_t fapl_id, haddr_t addr,
                size_t size, const void *buf);
static herr_t H5FD_backup_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing);
static herr_t H5FD_backup_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static int H5FD_backup_updatebackfile(H5FD_backup_t *file);
static FILE *H5FD_backup_createbackfile(H5FD_backup_t *file, const char *name);
static int H5FD_backup_updatebackfile(H5FD_backup_t *file);
static int H5FD_backup_init_segments(H5FD_backup_t *file);
static int H5FD_backup_mark_segments(const H5FD_backup_t *file, const haddr_t ADDR, const size_t SIZE);

static const H5FD_class_t H5FD_backup_g = {
    "backup",                    /* name         */
    MAXADDR,                    /* maxaddr      */
    H5F_CLOSE_WEAK,             /* fc_degree    */
    NULL,                       /* sb_size      */
    NULL,                       /* sb_encode    */
    NULL,                       /* sb_decode    */
    0,                          /* fapl_size    */
    NULL,                       /* fapl_get     */
    NULL,                       /* fapl_copy    */
    NULL,                       /* fapl_free    */
    0,                          /* dxpl_size    */
    NULL,                       /* dxpl_copy    */
    NULL,                       /* dxpl_free    */
    H5FD_backup_open,            /* open         */
    H5FD_backup_close,           /* close        */
    H5FD_backup_cmp,             /* cmp          */
    H5FD_backup_query,           /* query        */
    NULL,                       /* get_type_map */
    H5FD_backup_alloc,           /* alloc        */
    NULL,                       /* free         */
    H5FD_backup_get_eoa,         /* get_eoa      */
    H5FD_backup_set_eoa,         /* set_eoa      */
    H5FD_backup_get_eof,         /* get_eof      */
    H5FD_backup_get_handle,      /* get_handle   */
    H5FD_backup_read,            /* read         */
    H5FD_backup_write,           /* write        */
    H5FD_backup_flush,           /* flush        */
    H5FD_backup_truncate,        /* truncate     */
    NULL,                       /* lock         */
    NULL,                       /* unlock       */
    H5FD_FLMAP_DICHOTOMY	/* fl_map       */
};


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_init
 *
 * Purpose:  Initialize this driver by registering the driver with the
 *    library.
 *
 * Return:  Success:  The driver ID for the backup driver.
 *
 *    Failure:  Negative.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_backup_init(void)
{
    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if (H5I_VFL!=H5Iget_type(H5FD_BACKUP_g))
        H5FD_BACKUP_g = H5FDregister(&H5FD_backup_g);
    return H5FD_BACKUP_g;
} /* end H5FD_backup_init() */


/*---------------------------------------------------------------------------
 * Function:  H5FD_backup_term
 *
 * Purpose:  Shut down the VFD
 *
 * Returns:     None
 *
 * Programmer:  Quincey Koziol
 *              Friday, Jan 30, 2004
 *
 *---------------------------------------------------------------------------
 */
void
H5FD_backup_term(void)
{
    /* Reset VFL ID */
    H5FD_BACKUP_g = 0;

    return;
} /* end H5FD_backup_term() */


/*-------------------------------------------------------------------------
 * Function:  H5Pset_fapl_backup
 *
 * Purpose:  Modify the file access property list to use the H5FD_BACKUP
 *    driver defined in this source file.  There are no driver
 *    specific properties.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_backup(hid_t fapl_id)
{
    static const char *func = "H5FDset_fapl_backup";  /*for error reporting*/

    /*NO TRACE*/

    if (get_debug())
	fprintf(stderr, "calling H5Pset_fapl_backup\n");

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if(0 == H5Pisa_class(fapl_id, H5P_FILE_ACCESS))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_PLIST, H5E_BADTYPE, "not a file access property list", -1)

    return H5Pset_driver(fapl_id, H5FD_BACKUP, NULL);
} /* end H5Pset_fapl_backup() */


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_open
 *
 * Purpose:  Create and/or opens a Standard C file as an HDF5 file.
 *
 * Errors:
 *  IO  CANTOPENFILE    File doesn't exist and CREAT wasn't
 *                      specified.
 *  IO  CANTOPENFILE    fopen() failed.
 *  IO  FILEEXISTS      File exists but CREAT and EXCL were
 *                      specified.
 *
 * Return:
 *      Success:    A pointer to a new file data structure. The
 *                  public fields will be initialized by the
 *                  caller, which is always H5FD_open().
 *
 *      Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_backup_open( const char *name, unsigned flags, hid_t fapl_id,
    haddr_t maxaddr)
{
    FILE                *f = NULL;
    unsigned            write_access = 0;           /* File opened with write access? */
    H5FD_backup_t        *file = NULL;
    static const char   *func = "H5FD_backup_open";  /* Function Name for error reporting */
#ifdef H5_HAVE_WIN32_API
    struct _BY_HANDLE_FILE_INFORMATION fileinfo;
#else /* H5_HAVE_WIN32_API */
    struct stat         sb;
#endif  /* H5_HAVE_WIN32_API */

    /* Sanity check on file offsets */
    assert(sizeof(file_offset_t) >= sizeof(size_t));

    if (get_debug())
	fprintf(stderr, "calling H5FD_backup_open\n");

    /* Quiet compiler */
    fapl_id = fapl_id;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Check arguments */
    if (!name || !*name)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_BADVALUE, "invalid file name", NULL)
    if (0 == maxaddr || HADDR_UNDEF == maxaddr)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_BADRANGE, "bogus maxaddr", NULL)
    if (ADDR_OVERFLOW(maxaddr))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_OVERFLOW, "maxaddr too large", NULL)

    /* Tentatively open file in read-only mode, to check for existence */
    if(flags & H5F_ACC_RDWR)
        f = fopen(name, "rb+");
    else
        f = fopen(name, "rb");

    if(!f) {
        /* File doesn't exist */
        if(flags & H5F_ACC_CREAT) {
            assert(flags & H5F_ACC_RDWR);
            f = fopen(name, "wb+");
            write_access = 1;     /* Note the write access */
        }
        else
            H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_CANTOPENFILE, "file doesn't exist and CREAT wasn't specified", NULL)
    } else if(flags & H5F_ACC_EXCL) {
        /* File exists, but EXCL is passed.  Fail. */
        assert(flags & H5F_ACC_CREAT);
        fclose(f);
        H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_FILEEXISTS, "file exists but CREAT and EXCL were specified", NULL)
    } else if(flags & H5F_ACC_RDWR) {
        if(flags & H5F_ACC_TRUNC)
            f = freopen(name, "wb+", f);
        write_access = 1;     /* Note the write access */
    } /* end if */
    /* Note there is no need to reopen if neither TRUNC nor EXCL are specified,
     * as the tentative open will work */

    if(!f)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_CANTOPENFILE, "fopen failed", NULL)

    /* Build the return value */
    if(NULL == (file = (H5FD_backup_t *)calloc((size_t)1, sizeof(H5FD_backup_t)))) {
        fclose(f);
        H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_NOSPACE, "memory allocation failed", NULL)
    } /* end if */
    file->fp = f;
    file->op = H5FD_BACKUP_OP_SEEK;
    file->pos = HADDR_UNDEF;
    file->write_access = write_access;    /* Note the write_access for later */
    if(file_fseek(file->fp, (file_offset_t)0, SEEK_END) < 0) {
        file->op = H5FD_BACKUP_OP_UNKNOWN;
    } else {
        file_offset_t x = file_ftell(file->fp);
        assert (x >= 0);
        file->eof = (haddr_t)x;
    }

    /* Get the file descriptor (needed for truncate and some Windows information) */
#ifdef H5_HAVE_WIN32_API
    file->fd = _fileno(file->fp);
#else /* H5_HAVE_WIN32_API */
    file->fd = fileno(file->fp);
#endif /* H5_HAVE_WIN32_API */
    if(file->fd < 0) {
        free(file);
        fclose(f);
        H5Epush_ret(func, H5E_ERR_CLS, H5E_FILE, H5E_CANTOPENFILE, "unable to get file descriptor", NULL);
    } /* end if */


#ifdef H5_HAVE_WIN32_API
    file->hFile = (HANDLE)_get_osfhandle(file->fd);
    if(INVALID_HANDLE_VALUE == file->hFile) {
        free(file);
        fclose(f);
        H5Epush_ret(func, H5E_ERR_CLS, H5E_FILE, H5E_CANTOPENFILE, "unable to get Windows file handle", NULL);
    } /* end if */

    if(!GetFileInformationByHandle((HANDLE)file->hFile, &fileinfo)) {
        free(file);
        fclose(f);
        H5Epush_ret(func, H5E_ERR_CLS, H5E_FILE, H5E_CANTOPENFILE, "unable to get Windows file descriptor information", NULL);
    } /* end if */

    file->nFileIndexHigh = fileinfo.nFileIndexHigh;
    file->nFileIndexLow = fileinfo.nFileIndexLow;
    file->dwVolumeSerialNumber = fileinfo.dwVolumeSerialNumber;
#else /* H5_HAVE_WIN32_API */
    if(fstat(file->fd, &sb) < 0) {
        free(file);
        fclose(f);
        H5Epush_ret(func, H5E_ERR_CLS, H5E_FILE, H5E_BADFILE, "unable to fstat file", NULL)
    } /* end if */
    file->device = sb.st_dev;
#ifdef H5_VMS
    file->inode[0] = sb.st_ino[0];
    file->inode[1] = sb.st_ino[1];
    file->inode[2] = sb.st_ino[2];
#else /* H5_VMS */
    file->inode = sb.st_ino;
#endif /* H5_VMS */
#endif /* H5_HAVE_WIN32_API */

    /* generate a backup copy the opened file */
    file->backup_fp=H5FD_backup_createbackfile(file, name);
    /* NEED TO CHECK IF THIS IS A DUPLICATED FILE */
    /* also open a readonly handle of the opened file for backup copying later. */
    if (NULL==(file->ronly_fp=fopen(name, "r"))){
        H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_CANTOPENFILE, "readonly handle open failed", NULL)
    };

    /* update the backup file initially */
    if (H5FD_backup_init_segments(file) < 0)
	H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_WRITEERROR, "backup segment array init failed", NULL)
    if (H5FD_backup_updatebackfile(file) < 0)
	H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_WRITEERROR, "update backup failed", NULL)

    return (H5FD_t*)file;
} /* end H5FD_backup_open() */


/*-------------------------------------------------------------------------
 * Function:  H5F_backup_close
 *
 * Purpose:  Closes a file.
 *
 * Errors:
 *    IO    CLOSEERROR  Fclose failed.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_backup_close(H5FD_t *_file)
{
    H5FD_backup_t  *file = (H5FD_backup_t*)_file;
    static const char *func = "H5FD_backup_close";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if (fclose(file->fp) < 0)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_CLOSEERROR, "fclose failed", -1)

    /* close backup file */
    FILE	*ronly_fp;      /* readonly handle of the data file */
    if (fclose(file->ronly_fp) < 0)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_CLOSEERROR, "fclose ronly_fp failed", -1)
    if (fclose(file->backup_fp) < 0)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_CLOSEERROR, "fclose backup failed", -1)
    free(file->backup_name);

    free(file);

    return 0;
} /* end H5FD_backup_close() */


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_cmp
 *
 * Purpose:  Compares two files belonging to this driver using an
 *    arbitrary (but consistent) ordering.
 *
 * Return:
 *      Success:    A value like strcmp()
 *
 *      Failure:    never fails (arguments were checked by the caller).
 *
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_backup_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_backup_t  *f1 = (const H5FD_backup_t*)_f1;
    const H5FD_backup_t  *f2 = (const H5FD_backup_t*)_f2;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

#ifdef H5_HAVE_WIN32_API
    if(f1->dwVolumeSerialNumber < f2->dwVolumeSerialNumber) return -1;
    if(f1->dwVolumeSerialNumber > f2->dwVolumeSerialNumber) return 1;

    if(f1->nFileIndexHigh < f2->nFileIndexHigh) return -1;
    if(f1->nFileIndexHigh > f2->nFileIndexHigh) return 1;

    if(f1->nFileIndexLow < f2->nFileIndexLow) return -1;
    if(f1->nFileIndexLow > f2->nFileIndexLow) return 1;
#else /* H5_HAVE_WIN32_API */
#ifdef H5_DEV_T_IS_SCALAR
    if(f1->device < f2->device) return -1;
    if(f1->device > f2->device) return 1;
#else /* H5_DEV_T_IS_SCALAR */
    /* If dev_t isn't a scalar value on this system, just use memcmp to
     * determine if the values are the same or not.  The actual return value
     * shouldn't really matter...
     */
    if(memcmp(&(f1->device),&(f2->device),sizeof(dev_t)) < 0) return -1;
    if(memcmp(&(f1->device),&(f2->device),sizeof(dev_t)) > 0) return 1;
#endif /* H5_DEV_T_IS_SCALAR */
#ifdef H5_VMS
    if(memcmp(&(f1->inode), &(f2->inode), 3 * sizeof(ino_t)) < 0) return -1;
    if(memcmp(&(f1->inode), &(f2->inode), 3 * sizeof(ino_t)) > 0) return 1;
#else /* H5_VMS */
    if(f1->inode < f2->inode) return -1;
    if(f1->inode > f2->inode) return 1;
#endif /* H5_VMS */
#endif /* H5_HAVE_WIN32_API */

    return 0;
} /* H5FD_backup_cmp() */


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_query
 *
 * Purpose:  Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:  Success:  non-negative
 *
 *    Failure:  negative
 *
 * Programmer:  Quincey Koziol
 *              Friday, August 25, 2000
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_backup_query(const H5FD_t *_f, unsigned long *flags /* out */)
{
    /* Quiet the compiler */
    _f=_f;

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags = 0;
        *flags|=H5FD_FEAT_AGGREGATE_METADATA; /* OK to aggregate metadata allocations */
        *flags|=H5FD_FEAT_ACCUMULATE_METADATA; /* OK to accumulate metadata for faster writes */
        *flags|=H5FD_FEAT_DATA_SIEVE;       /* OK to perform data sieving for faster raw data reads & writes */
        *flags|=H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
    }

    return 0;
} /* end H5FD_backup_query() */


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_alloc
 *
 * Purpose:     Allocates file memory. If fseeko isn't available, makes
 *              sure the file size isn't bigger than 2GB because the
 *              parameter OFFSET of fseek is of the type LONG INT, limiting
 *              the file size to 2GB.
 *
 * Return:
 *      Success:    Address of new memory
 *
 *      Failure:    HADDR_UNDEF
 *
 * Programmer:  Raymond Lu
 *              30 March 2007
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_backup_alloc(H5FD_t *_file, H5FD_mem_t /*UNUSED*/ type, hid_t /*UNUSED*/ dxpl_id, hsize_t size)
{
    H5FD_backup_t    *file = (H5FD_backup_t*)_file;
    haddr_t         addr;

    /* Quiet compiler */
    type = type;
    dxpl_id = dxpl_id;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Compute the address for the block to allocate */
    addr = file->eoa;

    /* Check if we need to align this block */
    if(size >= file->pub.threshold) {
        /* Check for an already aligned block */
        if((addr % file->pub.alignment) != 0)
            addr = ((addr / file->pub.alignment) + 1) * file->pub.alignment;
    } /* end if */

    file->eoa = addr + size;

    return addr;
} /* end H5FD_backup_alloc() */


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_get_eoa
 *
 * Purpose:  Gets the end-of-address marker for the file. The EOA marker
 *           is the first address past the last byte allocated in the
 *           format address space.
 *
 * Return:  Success:  The end-of-address marker.
 *
 *    Failure:  HADDR_UNDEF
 *
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_backup_get_eoa(const H5FD_t *_file, H5FD_mem_t /*UNUSED*/ type)
{
    const H5FD_backup_t *file = (const H5FD_backup_t *)_file;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Quiet compiler */
    type = type;

    return file->eoa;
} /* end H5FD_backup_get_eoa() */


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_set_eoa
 *
 * Purpose:  Set the end-of-address marker for the file. This function is
 *    called shortly after an existing HDF5 file is opened in order
 *    to tell the driver where the end of the HDF5 data is located.
 *
 * Return:  Success:  0
 *
 *    Failure:  Does not fail
 *
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_backup_set_eoa(H5FD_t *_file, H5FD_mem_t /*UNUSED*/ type, haddr_t addr)
{
    H5FD_backup_t  *file = (H5FD_backup_t*)_file;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Quiet the compiler */
    type = type;

    file->eoa = addr;

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_get_eof
 *
 * Purpose:  Returns the end-of-file marker, which is the greater of
 *    either the Unix end-of-file or the HDF5 end-of-address
 *    markers.
 *
 * Return:  Success:  End of file address, the first address past
 *        the end of the "file", either the Unix file
 *        or the HDF5 file.
 *
 *    Failure:  HADDR_UNDEF
 *
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_backup_get_eof(const H5FD_t *_file)
{
    const H5FD_backup_t  *file = (const H5FD_backup_t *)_file;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    return MAX(file->eof, file->eoa);
} /* end H5FD_backup_get_eof() */


/*-------------------------------------------------------------------------
 * Function:       H5FD_backup_get_handle
 *
 * Purpose:        Returns the file handle of backup file driver.
 *
 * Returns:        Non-negative if succeed or negative if fails.
 *
 * Programmer:     Raymond Lu
 *                 Sept. 16, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_backup_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle)
{
    H5FD_backup_t       *file = (H5FD_backup_t *)_file;
    static const char  *func = "H5FD_backup_get_handle";  /* Function Name for error reporting */

    /* Quiet the compiler */
    fapl = fapl;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    *file_handle = &(file->fp);
    if(*file_handle == NULL)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_WRITEERROR, "get handle failed", -1)

    return 0;
} /* end H5FD_backup_get_handle() */


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_read
 *
 * Purpose:  Reads SIZE bytes beginning at address ADDR in file LF and
 *    places them in buffer BUF.  Reading past the logical or
 *    physical end of file returns zeros instead of failing.
 *
 * Errors:
 *    IO    READERROR  fread failed.
 *    IO    SEEKERROR  fseek failed.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_backup_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
    void *buf/*out*/)
{
    H5FD_backup_t    *file = (H5FD_backup_t*)_file;
    static const char *func = "H5FD_backup_read";  /* Function Name for error reporting */

    /* Quiet the compiler */
    type = type;
    dxpl_id = dxpl_id;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Check for overflow */
    if (HADDR_UNDEF==addr)
        H5Epush_ret (func, H5E_ERR_CLS, H5E_IO, H5E_OVERFLOW, "file address overflowed", -1)
    if (REGION_OVERFLOW(addr, size))
        H5Epush_ret (func, H5E_ERR_CLS, H5E_IO, H5E_OVERFLOW, "file address overflowed", -1)
    if((addr + size) > file->eoa)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_OVERFLOW, "file address overflowed", -1)

    /* Check easy cases */
    if (0 == size)
        return 0;
    if ((haddr_t)addr >= file->eof) {
        memset(buf, 0, size);
        return 0;
    }

    /* Seek to the correct file position. */
    if (!(file->op == H5FD_BACKUP_OP_READ || file->op == H5FD_BACKUP_OP_SEEK) ||
            file->pos != addr) {
        if (file_fseek(file->fp, (file_offset_t)addr, SEEK_SET) < 0) {
            file->op = H5FD_BACKUP_OP_UNKNOWN;
            file->pos = HADDR_UNDEF;
            H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_SEEKERROR, "fseek failed", -1)
        }
        file->pos = addr;
    }

    /* Read zeros past the logical end of file (physical is handled below) */
    if (addr + size > file->eof) {
        size_t nbytes = (size_t) (addr + size - file->eof);
        memset((unsigned char *)buf + size - nbytes, 0, nbytes);
        size -= nbytes;
    }

    /* Read the data.  Since we're reading single-byte values, a partial read
     * will advance the file position by N.  If N is zero or an error
     * occurs then the file position is undefined.
     */
    while(size > 0) {

        size_t bytes_in        = 0;    /* # of bytes to read       */
        size_t bytes_read      = 0;    /* # of bytes actually read */
        size_t item_size       = 1;    /* size of items in bytes */

        if(size > H5_BACKUP_MAX_IO_BYTES_g)
            bytes_in = H5_BACKUP_MAX_IO_BYTES_g;
        else
            bytes_in = size;

        bytes_read = fread(buf, item_size, bytes_in, file->fp);

        if(0 == bytes_read && ferror(file->fp)) { /* error */
            file->op = H5FD_BACKUP_OP_UNKNOWN;
            file->pos = HADDR_UNDEF;
            H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_READERROR, "fread failed", -1)
        } /* end if */
        
        if(0 == bytes_read && feof(file->fp)) {
            /* end of file but not end of format address space */
            memset((unsigned char *)buf, 0, size);
            break;
        } /* end if */
        
        size -= bytes_read;
        addr += (haddr_t)bytes_read;
        buf = (char *)buf + bytes_read;
    } /* end while */

    /* Update the file position data. */
    file->op = H5FD_BACKUP_OP_READ;
    file->pos = addr;

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_write
 *
 * Purpose:  Writes SIZE bytes from the beginning of BUF into file LF at
 *    file address ADDR.
 *
 * Errors:
 *    IO    SEEKERROR   fseek failed.
 *    IO    WRITEERROR  fwrite failed.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_backup_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t ADDR,
    size_t SIZE, const void *buf)
{
    H5FD_backup_t    *file = (H5FD_backup_t*)_file;
    static const char *func = "H5FD_backup_write";  /* Function Name for error reporting */
    haddr_t addr = ADDR;
    size_t size = SIZE;

    /* Quiet the compiler */
    dxpl_id = dxpl_id;
    type = type;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Check for overflow conditions */
    if (HADDR_UNDEF == addr)
        H5Epush_ret (func, H5E_ERR_CLS, H5E_IO, H5E_OVERFLOW, "file address overflowed", -1)
    if (REGION_OVERFLOW(addr, size))
        H5Epush_ret (func, H5E_ERR_CLS, H5E_IO, H5E_OVERFLOW, "file address overflowed", -1)
    if (addr+size > file->eoa)
        H5Epush_ret (func, H5E_ERR_CLS, H5E_IO, H5E_OVERFLOW, "file address overflowed", -1)

    /* Seek to the correct file position. */
    if ((file->op != H5FD_BACKUP_OP_WRITE && file->op != H5FD_BACKUP_OP_SEEK) ||
                file->pos != addr) {
        if (file_fseek(file->fp, (file_offset_t)addr, SEEK_SET) < 0) {
            file->op = H5FD_BACKUP_OP_UNKNOWN;
            file->pos = HADDR_UNDEF;
            H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_SEEKERROR, "fseek failed", -1)
        }
        file->pos = addr;
    }

    /* Write the buffer.  On successful return, the file position will be
     * advanced by the number of bytes read.  On failure, the file position is
     * undefined.
     */
    while(size > 0) {

        size_t bytes_in        = 0;    /* # of bytes to write  */
        size_t bytes_wrote     = 0;    /* # of bytes written   */
        size_t item_size       = 1;    /* size of items in bytes */

        if(size > H5_BACKUP_MAX_IO_BYTES_g)
            bytes_in = H5_BACKUP_MAX_IO_BYTES_g;
        else
            bytes_in = size;

        bytes_wrote = fwrite(buf, item_size, bytes_in, file->fp);

        if(bytes_wrote != bytes_in || (0 == bytes_wrote && ferror(file->fp))) { /* error */
            file->op = H5FD_BACKUP_OP_UNKNOWN;
            file->pos = HADDR_UNDEF;
            H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_WRITEERROR, "fwrite failed", -1)
        } /* end if */
        
        assert(bytes_wrote > 0);
        assert((size_t)bytes_wrote <= size);

        size -= bytes_wrote;
        addr += (haddr_t)bytes_wrote;
        buf = (const char *)buf + bytes_wrote;
    }

    /* Update seek optimizing data. */
    file->op = H5FD_BACKUP_OP_WRITE;
    file->pos = addr;

    /* Update EOF if necessary */
    if (file->pos > file->eof)
        file->eof = file->pos;

    if (H5FD_backup_mark_segments(file, ADDR, SIZE) < 0){
	H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_WRITEERROR, "fwrite failed", -1)
    }

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_flush
 *
 * Purpose:  Makes sure that all data is on disk.
 *
 * Errors:
 *    IO    SEEKERROR     fseek failed.
 *    IO    WRITEERROR    fflush or fwrite failed.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_backup_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing)
{
    H5FD_backup_t  *file = (H5FD_backup_t*)_file;
    static const char *func = "H5FD_backup_flush";  /* Function Name for error reporting */

    /* Quiet the compiler */
    dxpl_id = dxpl_id;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Only try to flush the file if we have write access */
    if(file->write_access) {
        if(!closing) {
            if(fflush(file->fp) < 0)
                H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_WRITEERROR, "fflush failed", -1)

            /* Reset last file I/O information */
            file->pos = HADDR_UNDEF;
            file->op = H5FD_BACKUP_OP_UNKNOWN;

	    /* update the backup file */
	    if (H5FD_backup_updatebackfile(file) < 0)
                H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_WRITEERROR, "update backup failed", -1)
        } /* end if */
    } /* end if */

    return 0;
} /* end H5FD_backup_flush() */


/*-------------------------------------------------------------------------
 * Function:  H5FD_backup_truncate
 *
 * Purpose:  Makes sure that the true file size is the same (or larger)
 *    than the end-of-address.
 *
 * Errors:
 *    IO    SEEKERROR     fseek failed.
 *    IO    WRITEERROR    fflush or fwrite failed.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *    Thursday, January 31, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_backup_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing)
{
    H5FD_backup_t  *file = (H5FD_backup_t*)_file;
    static const char *func = "H5FD_backup_truncate";  /* Function Name for error reporting */

    /* Quiet the compiler */
    dxpl_id = dxpl_id;
    closing = closing;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Only try to flush the file if we have write access */
    if(file->write_access) {
        /* Makes sure that the true file size is the same as the end-of-address. */
        if(file->eoa != file->eof) {

#ifdef H5_HAVE_WIN32_API
            LARGE_INTEGER   li;         /* 64-bit (union) integer for SetFilePointer() call */
            DWORD           dwPtrLow;   /* Low-order pointer bits from SetFilePointer()
                                         * Only used as an error code here.
                                         */
            DWORD           dwError;    /* DWORD error code from GetLastError() */
            BOOL            bError;     /* Boolean error flag */

            /* Reset seek offset to beginning of file, so that file isn't re-extended later */
            rewind(file->fp);

            /* Windows uses this odd QuadPart union for 32/64-bit portability */
            li.QuadPart = (__int64)file->eoa;

            /* Extend the file to make sure it's large enough.
             *
             * Since INVALID_SET_FILE_POINTER can technically be a valid return value
             * from SetFilePointer(), we also need to check GetLastError().
             */
            dwPtrLow = SetFilePointer(file->hFile, li.LowPart, &li.HighPart, FILE_BEGIN);
            if(INVALID_SET_FILE_POINTER == dwPtrLow) {
                dwError = GetLastError();
                if(dwError != NO_ERROR )
                    H5Epush_ret(func, H5E_ERR_CLS, H5E_FILE, H5E_FILEOPEN, "unable to set file pointer", -1)
            }
            
            bError = SetEndOfFile(file->hFile);
            if(0 == bError)
                H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_SEEKERROR, "unable to truncate/extend file properly", -1)
#else /* H5_HAVE_WIN32_API */
            /* Reset seek offset to beginning of file, so that file isn't re-extended later */
            rewind(file->fp);

            /* Truncate file to proper length */
            if(-1 == file_ftruncate(file->fd, (file_offset_t)file->eoa))
                H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_SEEKERROR, "unable to truncate/extend file properly", -1)
#endif /* H5_HAVE_WIN32_API */

            /* Update the eof value */
            file->eof = file->eoa;

            /* Reset last file I/O information */
            file->pos = HADDR_UNDEF;
            file->op = H5FD_BACKUP_OP_UNKNOWN;
        } /* end if */
    } /* end if */
    else {
        /* Double-check for problems */
        if(file->eoa > file->eof)
            H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_TRUNCATED, "eoa > eof!", -1)
    } /* end else */

    return 0;
} /* end H5FD_backup_truncate() */

/* Create backup file from data file.
 * Use stdio API for file access.
 * Check backup file does not exist before creating it.
 * This only creates an empty backup file, no content is copied yet.
 */
static FILE *
H5FD_backup_createbackfile(H5FD_backup_t *file, const char *name)
{
    FILE *ret_code=NULL;
    int   name_len, backupname_len;
    char  *backupname;
    int   head_len, tail_len;
    char *pt;

    /* No arg check by assuming the name argument is proper since
     * this is called after name has been opened as a proper HDF5 file. */
    /* Get length of filename and create backup file name as:
     * ".XYZ.bck". The filename should be the tail part after the rightmost
     * slash.
     */
    name_len = HDstrlen(name);
    backupname_len = name_len + 6; /* dot+name+".bck"+terminator */
    if (NULL==(backupname=HDmalloc(backupname_len))){
	fprintf(stderr, "backupname memory allocation failed\n");
	return(NULL);
    }
    /* Find the right most '/'. */
    if (NULL!=(pt=HDstrrchr(name, '/'))){
	head_len = pt-name+1;
	HDmemcpy(backupname, name, head_len);
    }
    else{
	head_len = 0;
    };
    tail_len = name_len - head_len;
    *(backupname+head_len) = '.';
    HDmemcpy((backupname+head_len+1), name+head_len, tail_len);
    HDstrcpy((backupname+name_len+1), ".bck");
#ifndef DEBUG
fprintf(stderr, "H5FD_backup_createbackfile: name=%s, backupname=%s\n", name, backupname);
#endif
    if (HDaccess(backupname, F_OK) == 0) {
	fprintf(stderr, "backup file (%s) already exists.\n",
		backupname);
	return(NULL);
    }
    if (NULL==(ret_code=fopen(backupname, "wb"))){
	HDperror("fopen:");
	fprintf(stderr, "failed to create backup file (%s)\n",
		backupname);
	return(NULL);
    };
    file->backup_name = backupname;
    /* cheat */
    backupname_g = HDstrdup(file->backup_name);
    return(ret_code);
}

/* Update backup file.
 * Do an H5Fflush() to update the datafile.
 * Then copy the datafile to the backup file.
 * (future enhancement: will copy only parts of the datafile
 * that have been modified.)
 * Return: 0 success; -1 otherwise.
 */
static int
H5FD_backup_updatebackfile(H5FD_backup_t *file)
{
    int		ret_code=0;
    static const char *func = "H5FD_backup_updatebackfile";  /* Function Name for error reporting */
    static char	*buffer=NULL;
    size_t	bytesread;
    H5FD_backup_segment_t *seg_struct;
    int		i;
    uint8_t	*pt_segement_array;
    
    /* sanity checks */
    assert(file);

    seg_struct = file->segment_struct;
    if (!buffer){
	if (NULL==(buffer=HDmalloc(seg_struct->segment_size))){
	    fprintf(stderr, "buffer memory allocation failed\n");
	    return(-1);
	}
    }

    /* rewind both data and backup files */
    HDrewind(file->ronly_fp);
    HDrewind(file->backup_fp);
    /* walk through the segement array and flush only dirty segments */
    pt_segement_array = seg_struct->segment_array;
    for (i=0; i < seg_struct->segment_array_size; i++, pt_segement_array++){
	if (*pt_segement_array){
	    /* position both files to the right offset */
	    if ((file_fseek(file->ronly_fp, (file_offset_t)i*seg_struct->segment_size, SEEK_SET) < 0) ||
		(file_fseek(file->backup_fp, (file_offset_t)i*seg_struct->segment_size, SEEK_SET) < 0))
	    {
		H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_SEEKERROR, "fseek failed", -1)
	    }
	    if ((bytesread=HDfread(buffer, 1, seg_struct->segment_size, file->ronly_fp)) < 0){
		H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_SEEKERROR, "fread failed", -1)
	    }
	    if (HDfwrite(buffer, 1, bytesread, file->backup_fp) != bytesread){
		H5Epush_ret(func, H5E_ERR_CLS, H5E_IO, H5E_SEEKERROR, "fwrite failed", -1)
	    }
	    /* clear the segement */
	    *pt_segement_array = 0;
	}
    }
    /* free the buffer space */
    HDfree(buffer);
    buffer=NULL;

    /* flush backupfile */
    fflush(file->backup_fp);
    return(ret_code);
}

/*
 * These are functions to manage the segment array for more efficient backup.
 * The copying would take up extra I/O time and copying entire file for
 * every small changes being flushed would be wasteful. A speed up would be
 * to implement an algorithm to copy only parts of the file that have been
 * updated. The data file is “divided” into “segments” such as 1MB
 * each in size. Update in a segment is recorded and only dirty segments are
 * copied to the backup copy.
 * Implementation limit: for now, assume there is only 1 file to be backup.
 * Eventually, it should be one struct per file.
 */

/* H5FD_backup_init_segments
 * Allocate a segment array as big as 110% of current file size. The 10% is for
 * potential file growth in the future.
 * Return: 0 success; -1 otherwise.
 */
static int
H5FD_backup_init_segments(H5FD_backup_t *file)
{
    H5FD_backup_segment_t *seg_struct;
    static const char *func = "H5FD_backup_init_segments";  /* Function Name for error reporting */

    /* sanity checks */
    assert(file);
    assert((file->segment_struct == 0));

    /* allocate and initialize a segment index struct */
    if (NULL == (seg_struct = (H5FD_backup_segment_t *)calloc((size_t)1, sizeof(H5FD_backup_segment_t)))) {
        H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_NOSPACE, "memory allocation failed", -1)
    } /* end if */
    file->segment_struct = seg_struct;

    seg_struct->segment_factor = 20;	/* equal to 1MB */
    seg_struct->segment_size = 1<<seg_struct->segment_factor;
    seg_struct->segment_array_size = (file->eof >> seg_struct->segment_factor) + 1;
#if 0
printf("factor=%d, size=%d, array_size=%d, eof=%lld\n",
    seg_struct->segment_factor,
    seg_struct->segment_size,
    seg_struct->segment_array_size,
    (long long)file->eof);
#endif
    if (NULL == (seg_struct->segment_array = (uint8_t *)calloc((size_t)seg_struct->segment_array_size, sizeof(uint8_t)))) {
        H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_NOSPACE, "memory allocation failed", -1)
    } /* end if */

    /* mark all segments dirty so that since none has been backed yet. */
    HDmemset(seg_struct->segment_array, 1, (size_t)seg_struct->segment_array_size);

    /* return success */
    return(0);
}

/* H5FD_backup_mark_segments
 * Mark segments that has been written. 
 * Return: 0 success; -1 otherwise.
 */
static int
H5FD_backup_mark_segments(const H5FD_backup_t *file, const haddr_t ADDR, const size_t SIZE)
{
    H5FD_backup_segment_t *seg_struct;
    static const char *func = "H5FD_backup_mark_segments";  /* Function Name for error reporting */
    int	begin_seg_id, end_seg_id;	/* zero based, inclusive */

    /* sanity checks */
    assert(file);
    assert((ADDR >= 0));
    assert((SIZE > 0));		/* SIZE should +ve */

    seg_struct = file->segment_struct;

    /* calculate beginning and ending index in the segment_array */
    begin_seg_id = ADDR >> seg_struct->segment_factor;
    end_seg_id = (ADDR + SIZE) >> seg_struct->segment_factor;
#if 0
printf("first ADDR=%lld, SIZE=%lld, segment_factor=%d, begin_seg_id=%d, end_seg_id=%d\n",
    (long long) ADDR,
    (long long) SIZE,
    seg_struct->segment_factor,
    begin_seg_id,
    end_seg_id
    );
#endif

    /* check if new size is larger than current segment array */
    if (end_seg_id >= seg_struct->segment_array_size){
	/* use recalloc to expand the segment array and preserve existing segment information */
	if (NULL == (seg_struct->segment_array = (uint8_t *)calloc((size_t)(end_seg_id + 1), sizeof(uint8_t)))) {
	    H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_NOSPACE, "memory allocation failed", -1)
	}
	seg_struct->segment_array_size = end_seg_id + 1;
    }

    /* mark all written segments */
    HDmemset(seg_struct->segment_array + begin_seg_id, 1, (size_t)(end_seg_id-begin_seg_id+1));

    /* return success */
    return(0);
}

