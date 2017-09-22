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

#ifndef SUCCEED
#define SUCCEED    0
#endif
#ifndef FAIL
#define FAIL    (-1)
#endif

/* Attribute copy function declaration */
static herr_t attribute_copy_by_name(hid_t src_loc_id, const char *src_attr_name, hid_t dst_loc_id, const char *dst_attr_name);

/* Copy an attribute from an object (dataset or group) to another
 * Return 0 if all okay; otherwise -1.
 */
int
copy_attribute(
    char *src_object_name,
    char *src_attribute_name,
    char *dst_object_name,
    char *dst_attribute_name )
{
    hid_t src_object_id = -1;
    hid_t dst_object_id = -1;

    int         ret_value = 0;

    /* FIXME: Turn on HDF5's automatic error printing. */
/****
    if (H5Eset_auto2(H5E_DEFAULT, (H5E_auto2_t)H5Eprint2, stderr) < 0){
        fprintf(stderr, "%s: failed to turn off automatic error printing\n", progname_g);
        leave(EXIT_FAILURE);
    }
****/

    /* sanity check */
    HDassert(datafile_g>=0);

    /* open source and destination objects and call the renaming function */
    src_object_id = H5Oopen( datafile_g, src_object_name, H5P_DEFAULT );
    if (src_object_id <= 0) {
        fprintf( stderr, "Couldn't open object %s\n", src_object_name );
        ret_value = -1;
        goto done; 
    }

    if (strcmp(src_object_name,dst_object_name)) {
        dst_object_id = H5Oopen( datafile_g, dst_object_name, H5P_DEFAULT );
        if (dst_object_id <= 0) {
            fprintf( stderr, "Couldn't open object %s\n", dst_object_name );
            ret_value = -1;
            goto done; 
        }
    }
    else {
        dst_object_id = src_object_id;
    }

    ret_value = attribute_copy_by_name( src_object_id, src_attribute_name, dst_object_id, dst_attribute_name);
    if (ret_value < 0) {
        H5Eprint( H5E_DEFAULT,stderr );
        goto done;
    }

done:
    /* clean up */
    if ((src_object_id >= 0) && (src_object_id != dst_object_id)){
	if (H5Oclose(src_object_id) < 0){
	    fprintf(stderr, "Could not close source object\n");
        }
    }
    if (dst_object_id >= 0){
	if (H5Oclose(dst_object_id) < 0){
	    fprintf(stderr, "Could not close destination object\n");
        }
    }

    /*fprintf( stderr, "copy_cmd: returning value %d\n", ret_value );*/
    return(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    attribute_copy_by_name
 PURPOSE
    Copy an attribute
 USAGE
    hid_t attribute_copy_by_name(src_loc_id, src_attr_name, dst_loc_id, dst_attr_name)
        hid_t src_loc_id;       IN: Object (dataset or group) of the attribute to copy from
        const char *src_attr_name;  IN: Name of attribute to copy
        hid_t dst_loc_id;       IN: Object (dataset or group) of the attribute to copy to
        const char *dst_attr_name;  IN: Name of attribute to locate and open
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
    This function will copy an existing attribute, specified with 'src_loc_id/src_attr_name'
    to a new location, specified with 'dst_loc_id/dst_attr_name'. The dataspace and datatype
    of the destination attribute will be identical to that of the source attribute. Note that
    the copy will fail if both the locations and attribute names are identical.

--------------------------------------------------------------------------*/
static herr_t attribute_copy_by_name(hid_t src_loc_id, const char *src_attr_name,
        hid_t dst_loc_id, const char *dst_attr_name)
{
		
    hid_t       attr_src_id;
    hid_t       attr_dst_id;
    hid_t       ds_id;
    hid_t       dt_id;
    herr_t      ret_value = SUCCEED;
    H5A_info_t  ainfo;

    void        *buffer;
    const char  *self_name = ".";

    /* open the source attribute */
    attr_src_id = H5Aopen_by_name( src_loc_id, self_name, src_attr_name, H5P_DEFAULT, H5P_DEFAULT);
    if (attr_src_id <= 0) {
        ret_value = FAIL;
        goto done;
    }

    /* get the attribute type */
    dt_id = H5Aget_type( attr_src_id );
    if (dt_id <= 0) {
        ret_value = FAIL;
        goto done;
    }

    /* get the attribute dataspace */
    ds_id = H5Aget_space( attr_src_id );
    if (ds_id <= 0) {
        ret_value = FAIL;
        goto done;
    }

    /* get attribute info */
    ret_value = H5Aget_info( attr_src_id, &ainfo );
    if (ds_id <= 0) {
        ret_value = FAIL;
        goto done;
    }

    /* allocate a buffer for the attribute value */
    buffer = malloc( ainfo.data_size );
    if (buffer == NULL) {
        ret_value = FAIL;
        goto done;
    }

    /* create the destination attribute */
    attr_dst_id = H5Acreate(dst_loc_id, dst_attr_name, dt_id, ds_id, H5P_DEFAULT, H5P_DEFAULT);
    if (attr_dst_id < 0) {
        ret_value = FAIL;
        goto done;
    }

    /* read the old attribute and write the new one */
    ret_value = H5Aread( attr_src_id, dt_id, buffer );
    if (ret_value < 0) {
        ret_value = FAIL;
        goto done;
    }
    ret_value = H5Awrite( attr_dst_id, dt_id, buffer );
    if (ret_value < 0) {
        ret_value = FAIL;
        goto done;
    }

    /*fprintf( stderr, "attribute_copy_by_name: returning value %d\n", SUCCEED );*/
    /* if no errors */
    return SUCCEED;

    /* if error occurred */
done:
    /*fprintf( stderr, "attribute_copy_by_name: returning value %d\n", ret_value );*/
    return ret_value;
}
