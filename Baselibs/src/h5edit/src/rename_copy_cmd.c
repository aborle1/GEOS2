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

/* Rename an attribute from a group or dataset.
 * If attribute_name is not given (==NULL), extract it from the given
 * group or dataset name.
 * Return 0 if all okay; otherwise -1.
 */
int
rename_attribute(
    const char *attribute_name,
    const char *group_name,
    const char *dataset_name,
    const char *attribute_name_new)
{
    hid_t object_id = -1;
    int	retval = -1;	/* assume it will fail */
    char *attr_name = NULL;	/* local pointer of the attr_name */
    char *target_obj_name = NULL;	/* local point of the target object name */
    object_t attribute_type = No_type;


    if (get_debug()) {
	fprintf(stderr, "Debug: enter rename_attribute:\n"
	    "Debug: attribute_name=%s, group_name=%s, dataset_name=%s, attribute_name_new=%s\n",
	    (attribute_name?attribute_name:"null"),
	    (group_name?group_name:"null"),
	    (dataset_name?dataset_name:"null"),
	    (attribute_name_new?attribute_name_new:"null")
	);
    }
    /* sanity check */
    HDassert((group_name || dataset_name));
    HDassert(attribute_name_new);
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
    }else{
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

    /* Rename the attribute */
    if (H5Arename(object_id, attr_name, attribute_name_new) < 0){
	fprintf(stderr, "failed to rename attribute from old name %s to new name\n",
	    attr_name, attribute_name_new);
	goto error;
    }

    /* Free target_obj_name space if it is allocated */
    if (target_obj_name)
	HDfree(target_obj_name);

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
    if (target_obj_name)
	HDfree(target_obj_name);

    return(-1);
}
