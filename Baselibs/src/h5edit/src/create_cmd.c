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

/* Create an attribute in a group or dataset.
 * If attribute_name is not given (==NULL), extract it from the given
 * group or dataset name.
 * Return 0 if all okay; otherwise -1.
 */
int
create_attribute(
    const char *attribute_name,
    const char *group_name,
    const char *dataset_name,
    hid_t type_id,		/* attribute type */
    hid_t space_id,		/* attribute space */
    void *data			/* attribte data */
    )
{
    hid_t object_id = -1;
    hid_t attribute_id = -1;
    H5T_class_t type_class;
    hid_t memory_type_id = -1;;
    int	retval = -1;	/* assume it will fail */
    char *attr_name = NULL;	/* local pointer of the attr_name */
    char *target_obj_name = NULL;	/* local point of the target object name */
    object_t attribute_type = No_type;

    /* sanity check */
    HDassert((group_name || dataset_name));
    HDassert(datafile_g>=0);

    /* If attribute name is given, assign it to the local attribute name pointer; */
    /* else extract it from the group or dataset name. */
    if (attribute_name){
	attr_name = attribute_name;
	/* open the target object */
	/* try dataset_name first */
	if (dataset_name){
	    if ((object_id=H5Dopen2(datafile_g, dataset_name, H5P_DEFAULT)) < 0){
		fprintf(stderr, "failed to open dataset %s\n", dataset_name);
		goto error;
	    }
	    attribute_type = Dataset_type;	/* delete a dataset attribute */
	}else{
	    if ((object_id=H5Gopen2(datafile_g, group_name, H5P_DEFAULT)) < 0){
		fprintf(stderr, "failed to open group %s\n", group_name);
		goto error;
	    }
	    attribute_type = Group_type;	/* delete a group attribute */
	}
    } else{
	/* copy group or dataset name into target_obj_name and then extract attribute name from it. */
	if ((target_obj_name = HDstrdup(dataset_name ? dataset_name: group_name)) == NULL){
	    fprintf(stderr, "strdup failed\n");
	    goto error;
	};
	if ((attr_name=HDstrrchr(target_obj_name, '/')) == NULL){
	    fprintf(stderr, "Cannot extract attribute name from target-object-name %s",
		target_obj_name);
	    goto error;
	};
	/* put a NULL where the / separator is to terminate the target object name;
	   Then move attr_name pointer down one byte. */
	*attr_name++ = '\0';

	/* First try open the target object as a dataset */
	if ((object_id=H5Dopen2(datafile_g, target_obj_name, H5P_DEFAULT)) >= 0){
	    attribute_type = Dataset_type;	/* delete a dataset attribute */
	} else{
	    /* Try open it as a group */
	    /* If the target object is root group "/", it becomes "\0". */
	    /* In that case, hardcode in "/". */
	    if ((object_id=H5Gopen2(datafile_g, (*target_obj_name?target_obj_name:"/"), H5P_DEFAULT)) < 0){
		fprintf(stderr, "failed to open group %s\n", target_obj_name);
		goto error;
	    }
	    attribute_type = Group_type;	/* delete a group attribute */
	}
    }

    /* create the attribute */
    if ((attribute_id=H5Acreate2(object_id, attr_name, type_id, space_id,
			    H5P_DEFAULT, H5P_DEFAULT)) < 0){
	fprintf(stderr, "failed to create attribute %s\n", attribute_name);
	goto error;
    }
    /* write the attribute data */
    switch (type_class = H5Tget_class(type_id)){
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
	    memory_type_id = H5Tcopy(type_id);
	    break;
	default:
	    fprintf(stderr, "cannot write data of unsupported attribute type(%d)\n",
		type_class);
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
	return(-1);
    }
    if (H5Aclose(attribute_id) < 0){
	fprintf(stderr, "Could not close attribute\n");
	return(-1);
    }

    /* close the target object ID before returning */
    if (object_id >= 0){
	switch (attribute_type) {
	    case Dataset_type:
		if (H5Dclose(object_id) < 0){
		    fprintf(stderr, "Could not close dataset\n");
		    return(-1);
		}
		break;
	    case Group_type:
		if (H5Gclose(object_id) < 0){
		    fprintf(stderr, "Could not close group\n");
		    return(-1);
		}
		break;
	    default:
	    	/* Unexpected type, return error */
		fprintf(stderr,
		    "Could not close object because of unknown attribute type(%d)\n",
		    attribute_type);
		return(-1);
	}
	object_id = -1;
    }
    return(0);

error:
    /* clean up */
    if (object_id >= 0){
	switch (attribute_type) {
	    case Dataset_type:
		if (H5Dclose(object_id) < 0){
		    fprintf(stderr, "Could not close dataset\n");
		    return(-1);
		}
		break;
	    case Group_type:
		if (H5Gclose(object_id) < 0){
		    fprintf(stderr, "Could not close group\n");
		    return(-1);
		}
		break;
	    default:
	    	/* Unexpected type, return error */
		fprintf(stderr,
		    "Could not close object because of unknown attribute type(%d)\n",
		    attribute_type);
		return(-1);
	}
	object_id = -1;
    }
    return(-1);
}
