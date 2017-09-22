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

/* Modify an attribute in a group or dataset.
 * If attribute_name is not given (==NULL), extract it from the given
 * group or dataset name.
 * Return 0 if all okay; otherwise -1.
 */
int
modify_attribute(
    const char *attribute_name,
    const char *group_name,
    const char *dataset_name,
    attribute_def_p_t attr_def,
    void *data			/* attribte data */
    )
{
    hid_t object_id = -1;
    hid_t attribute_id = -1;
    hid_t memory_type_id = -1;;
    const char *objectname = NULL;

    /* sanity check */
    HDassert((group_name || dataset_name));
    HDassert(attribute_name);
    HDassert(datafile_g>=0);

    /* open the target object */
    objectname = dataset_name ? dataset_name : group_name;
    if ((object_id=H5Oopen(datafile_g, objectname, H5P_DEFAULT)) < 0){
	fprintf(stderr, "1failed to open target object %s\n", objectname);
	goto error;
    }

    /* modify the attribute */
    if ((attribute_id=H5Aopen(object_id, attribute_name, H5P_DEFAULT)) < 0){
	fprintf(stderr, "failed to open attribute %s\n", attribute_name);
	goto error;
    }
    /* write the attribute data */
    switch (attr_def->type_class){
	case H5T_INTEGER:
	    memory_type_id = H5Tcopy(H5T_NATIVE_INT);
	    break;
	case H5T_FLOAT:
	    memory_type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
	    break;
	case H5T_STRING:
	    /* For now, one null-terminated strings are supported.
	     * Memory is the same as the file data type.
	     * Just duplicate the type_id.
	     */
	    memory_type_id = H5Tcopy(attr_def->type_id);
	    break;
	default:
	    fprintf(stderr, "cannot write data of unsupported attribute type(%d)\n",
		attr_def->type_class);
	    goto error;
    } /* end of switch */
    if (H5Awrite(attribute_id, memory_type_id, data) < 0){
	fprintf(stderr, "failed to write data of attribute %s\n",
	    attribute_name);
	goto error;
    }

    /* close memory type, attribute, dataset and leave */
    if (H5Tclose(memory_type_id) < 0){
	fprintf(stderr, "Could not close memory type id\n");
	goto error;
    }
    memory_type_id = -1;

    if (H5Aclose(attribute_id) < 0){
	fprintf(stderr, "Could not close attribute\n");
	goto error;
    }
    attribute_id = -1;

    if (H5Oclose(object_id) < 0){
	fprintf(stderr, "Could not close dataset\n");
	goto error;
    }
    object_id = -1;

    /* Command done. Return success. */
    return(0);

error:
    /* clean up */
    /* attribute should not be opened but close it anyway. */
    if (attribute_id >=0){
	if (H5Aclose(attribute_id) < 0){
	    fprintf(stderr, "Could not close attribute\n");
	    return(-1);
	}
    }
    if (memory_type_id >=0){
	if (H5Tclose(memory_type_id) < 0){
	    fprintf(stderr, "Could not close memory_type_id\n");
	    return(-1);
	}
    }
    if (object_id >= 0){
	if (H5Oclose(object_id) < 0){
	    fprintf(stderr, "Could not close dataset\n");
	    return(-1);
	}
    }
    return(-1);
}
