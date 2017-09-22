/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:   
        PGS_MET_SetAttr
 
DESCRIPTION:
	Enables the user to set the values of metadata parameters 
	
AUTHOR: 
        Alward N. Siyyid/ EOSL
        Carol S. W. Tsai / Space Applications Corporation
        Abe Taaheri / Emergent Information Technologies, Inc.

HISTORY:
        18-MAY-95      ANS     Initial version
	01-JUN-95      ANS     Post code inpection updates
	13-July-95     ANS     Improved Fortran example
	20-July-1995    ANS     Fixed DR ECSed01012
	20-July-1995    ANS     Fixed DR ECSed01021
	25-July-1995	ANS	Fixed DR ECSed01033
	10-Aug-1995     ANS     chnaged some typos in the prolog
	17-Feb_1996	ANS	begun modification for tk5+
        08-Apr-1997     CSWT    Added code for generating error messages when user set 
                                attributes for Data_Location is DAAC, DSS, or PD
        21-Oct-1997     CSWT    Added code to generate the error message if the
                                Data Location is MCF and user attends to set up the
                                attribute value that already been set up
        06-Dec-1997     CSWT    Added code to check if the length of the character 
                                string to be set up for the attribute value is greater   
                                than the PGSd_MET_MAX_STRING_SET_L (255), a constant    
                                that defined for the maximun amount of the length of  
                                the character string. A error status message will  
                                be generated if the length is over the maximun 
                                maximun amount of 255.   
                                Added code to check if the element size of the array 
                                of the character string to be set up for the attribute 
                                value is greater than the PGSd_MET_MAX_ARRAY_ELEMENT_SIZE  
                                (1000), a constant that defined for the maximun amount   
                                of the size of the array element. A error status message 
                                will be generated if the length is over the maximun 
                                maximun amount of 1000.   
        15-Jun-1998     CSWT    Modified code to remove the VALUE Node only for Data Location 
                                is PGE so that the new value could be inserted (This change
                                is for NCR ECSed12896 about MET: PSAs not passed to .met if 
                                CLASS="M" in MCF)
	07-Jul-1999	RM	updated for TSF functionality
        21-Jan-2001     AT      Increased PGSd_MET_MAX_ARRAY_ELEMENT_SIZE to
                                2000 in the PGS_MET.h and PGS_MET.f

END_FILE_PROLOG
*******************************************************************************/

/* include files */

#include "PGS_MET.h"
#include "PGS_TSF.h"
#include <errno.h>
#include <limits.h>
#include <math.h>

/* odl include files */

#include <CUC/odldef.h>
#include <CUC/odlinter.h>

/***************************************************************************
BEGIN_PROLOG:

TITLE: 
        Accesses the metadata parameters and sets them to user defined values
  
NAME:  
        PGS_MET_SetAttr()

SYNOPSIS:
C:
        #include "PGS_MET.h"

	PGSt_SMF_status
	PGS_MET_SetAttr(
			PGSt_MET_handle    mdHandle,
			char               *attrNameStr,
			void               *attrValue)

FORTRAN:
         include "PGS_MET_13.f"
	 include "PGS_MET.f" 
	 include "PGS_SMF.h"
         integer function pgs_met_setattr(mdHandle, attrNameStr, attrValue) 

	 character*   mdHandle
         character*      attrName
	 'user defined'  attrValue
   
DESCRIPTION:
	Metadata file is first initialized into memory and some of the parameters 
	are automatically set (if data location is defined as MCF or PCF) and some are set by the user using PGS_MET_SetAttr().
	The values can be of following types and there array counterparts

	PGSt_integer, PGSt_double, PGSt_real, char * (string)
INPUTS:
        Name            Description            Units   	Min	Max
        ----            -----------            -----   	---     ---
	mdHandle	metadata group		none    N/A     N/A	
			in MCF
        attrNameStr	name.class of parameter none    N/A     N/A
	attrValue       value of attribute to  none    N/A     N/A
			be inserted		
OUTPUTS:
	None

RETURNS:   
   	PGS_S_SUCCESS			
	PGSMET_E_NO_INITIALIZATION	Metadata file is not initialized
	PGSMET_E_NESTED_OBJECTS		Object descriptions enclosing related objects must not be enclosed themselevs
					by other objects
	PGSMET_E_ODL_MEM_ALLOC		ODL routine failed to allocate memory
	PGSMET_E_PARENT_GROUP		Multiple objects must have enclosing groups around them
	PGSMET_E_CLASS_PARAMETER	Container object must also have class parameter defined
	PGSMET_E_METADATA_CHILD		MetaData Objects are not allowed to enclose other objects 
	PGSMET_W_NOT_MULTIPLE		Object is not supposed to be multiple therefore resetting the value
					The user may have given a class with the metadata name
	PGSMET_E_ILLEGAL_TYPE		Illegal type definition for metadata <attrName>. It should be a string
	PGSMET_E_NO_DEFINITION		Unable to obtain <attr> of metadata <parameter>
					Either type or numval not defined
	PGSMET_E_ILLEGAL_NUMVAL		Illegal NUMVAL definition for metadata <attrName>. It should be an integer
	PGSMET_E_DD_UNKNOWN_PARM	The requested parameter <parameter name> 
					could not be found in <agg node>	
	PGSMET_E_NEW_ODL_DATA_ERR	Unable to create a new odl <parameter>, 
					probably due to lack of memeory
	PGSMET_E_INV_DATATYPE		Invalid data type definition in MCF for parameter <name>
	PGS_MET_E_ILLEGAL_HANDLE	Handle is illegal. Check that initialization has taken place
        PGSMET_E_INVALID_LOCATION       Invalid data location for setting attribute value
	PGSTSF_E_GENERAL_FAILURE	problem in TSF code

EXAMPLES:
C:
	This is just an extraction of the call from a full example given in PGS_MET_Init() prolog.

	ret = PGS_MET_SetAttr(handles[INVENTORYMETADATA],  "SizeMBECSDataGranule", &ival);
	ret = PGS_MET_SetAttr(handles[INVENTORYMETADATA],  "RangeBeginningDateTime", &datetime);
	ret = PGS_MET_SetAttr(handles[INVENTORYMETADATA],  "EastBoundingCoordinate", &dval);
	ret = PGS_MET_SetAttr(handles[INVENTORYMETADATA],  "ZoneIdentifier", &ivals);
	ret = PGS_MET_SetAttr(handles[INVENTORYMETADATA],  "LocalityValue", &svals);

FORTRAN:

	This is just an extraction of the call from a full example given in PGS_MET_Init() prolog.

C	Note the way _i for integer, _d for double and _s for strings are used 
C	at the end of the function name. This is necessary because fortran 
C	compiler would complain about type conflicts if a generic name
C	is used
C	Set various values

	ival(1) = 3
	result = pgs_met_setattr_i(groups(INVENTORYMETADATA), "SizeMBECSDataGranule", ival)
	datetime = "1989-04-11T12:30:45.7Z"
	result = pgs_met_setattr_s(groups(INVENTORYMETADATA),  "RangeBeginningDateTime", datetime)
	dval(1) = 203.2
	result = pgs_met_setattr_d(groups(INVENTORYMETADATA),  "EastBoundingCoordinate", dval)
	do 11 i = 1,6
                ivals(i) = i
 11     continue
	ivals(6) = PGSd_MET_INT_MAX
	result = pgs_met_setattr_i(groups(INVENTORYMETADATA),  "ZoneIdentifier", ivals)
	svals(1) = "string 1"
	svals(2) = "string 2"
	svals(3) = "string 3"
        svals(4) = PGSd_MET_STR_END
	result = pgs_met_setattr_s(groups(INVENTORYMETADATA),  "LocalityValue", svals)
	if(result.NE.PGS_S_SUCCESS) then
               print *, "SetAttr failed. See Logstatus for details"
	endif

NOTES:
        Users can not set attribute values for Data Location is DAAC, DSS, or PD, a error 
        message will be generated if users incidently set up attributes for any of those 
        Data Locations above. 

	It is very important that variable string pointers are used for string manipulations.
        This is because void interface is used. For e.g. the following piece of code would give
        an error or unexpected results:

        .
        .
        char a[100];
        .
        .
        strcpy(a, "MODIS");
        retVal = PGS_MET_SetAttr(mdHandles[GROUP_GRANULE_DATA], "SATELLITE_NAME", a);
        retVal = PGS_MET_SetAttr(mdHandles[GROUP_GRANULE_DATA], "SATELLITE_NAME", &a);

        The first call is wrong because the routine expects char** but cannot force it
        because of void interface. The second call is wrong too because of the declaration
        of 'a' which is a constant pointer, i.e. it would always point to the same
        location in memory of 100 bytes. Only the following construct will work with
        the routine in which the srting pointer is declared as a variable

        char *a = "MODIS"
        .
        .
        retVal = PGS_MET_SetAttr(mdHandles[GROUP_GRANULE_DATA], "SATELLITE_NAME", &a);

        The above discussion is also true for arrays of strings. For e.g. the following
        is not allowed for the same reasons as above

        .
        .
        char a[10][100];
        .
        .
        strcpy(a[0], "MODIS");
        retVal = PGS_MET_SetAttr(mdHandles[GROUP_GRANULE_DATA], "SATELLITE_NAME", &a[0]);

        while the following is acceptable

        .
        .
        char *a[10];
        .
        .
        a[0] = "MODIS";
        retVal = PGS_MET_SetAttr(mdHandles[GROUP_GRANULE_DATA], "SATELLITE_NAME", &a[0]);

	Another important point is that there may be cases where metadata name is shared.
        For e.g. there could be a metadata attribute called LATITUDE defining sub-satellite point
        and there could be another giving grid reference. In such cases the tool distinguishes 
        between the two using the CLASS of the metadata which is part of the input name string.
        For e.g. The above mentioned latitudes can be represented as follows:

        attrNameStr = "LATITUDE.GRID"
        attrNameStr = "LATITUDE.SATELLITE"

        where GRID and SATELLITE are the two classes respectively.

        The CLASS field is optional and is needed only under the aforementioned circumstances. 

	See Note on Multiplicity for tk5+
 
REQUIREMENTS:
        PGSTK-0290 PGSTK-0410 PGSTK-380

DETAILS:
	The tool provides a void interface through which different types of metadata 
        can be set. The types supported are:
                PGSt_integer
		PGSt_uinteger
                PGSt_double
                string
        and their arrays counterparts. There is a small price to pay regarding 'strings'
        where 'void' interfaces are concerned which is explained in the 'notes' section
        above.

	Addendum on tk5+

	There has been a number of ways in which additional functionality has been introduced for tk5+:
 
1       Multiplicity:
		In tk5, a CLASS statement was introduced so that metadata objects with the same name
could be distinguished from each other in the ODL tree. This functionality is retained in tk5+. However
user only needs to declare the metadata object only once with an indication that this metadata object
can have multiple instances. This allows the user to declare and manipulate "arrays" of metadata
objects. This new facility now also implies that all the metadata objects within a master group in the MCF
must have unique names.

eg.   in MCF	
	
GROUP = GPolygonContainerGroup
 
OBJECT = GPolygonContainerObject
 
        Data_Location= "NONE" / necessary to i.d. a non-functional container object /
        CLASS = "M"
        Mandatory = "TRUE"
 
        OBJECT = ExclusionGRingFlag
                Data_Location= "PGE"
                CLASS = "M"
                TYPE = "STRING"
                NUM_VAL = 1
                Mandatory = TRUE
        END_OBJECT = ExclusionGRingFlag

/ for each of the following objects there are at least 3 elements in each array /
 
        OBJECT = GRingPointLatitude
                Data_Location = "PGE"
                CLASS = "M"
                TYPE = "DOUBLE"
                NUM_VAL = 5             / an array  of max size n , where n is at least 3 /
                Mandatory = TRUE
        END_OBJECT = GRingPointLatitude
 
        OBJECT = GRingPointLongitude
                Data_Location = "PGE"
                CLASS = "M"
                TYPE = "DOUBLE"
                NUM_VAL = 1
                Mandatory = TRUE
        END_OBJECT = GRingPointLongitude

        OBJECT = GRingPointSequenceNo
                Data_Location  = "PGE"
                CLASS = "M"
                TYPE = "STRING"
                NUM_VAL = 1
                Mandatory = TRUE
        END_OBJECT = GRingPointSequenceNo
 
END_OBJECT = GPolygonContainerObject
 
END_GROUP = GPolygonContainerGroup

HDFHeader

  GROUP                  = GPOLYGONCONTAINERGROUP
 
    OBJECT                 = GPOLYGONCONTAINEROBJECT
      CLASS                = "1"
 
      OBJECT                 = EXCLUSIONGRINGFLAG
        NUM_VAL              = 1
        VALUE                = "NOT SET"
      END_OBJECT             = EXCLUSIONGRINGFLAG

      OBJECT                 = GRINGPOINTLATITUDE
        NUM_VAL              = 5
        VALUE                = (1.000000, 2.000000, 3.000000,
                                4.000000, 5.000000)
      END_OBJECT             = GRINGPOINTLATITUDE
 
      OBJECT                 = GRINGPOINTLONGITUDE
        NUM_VAL              = 1
        VALUE                = "NOT SET"
      END_OBJECT             = GRINGPOINTLONGITUDE
 
      OBJECT                 = GRINGPOINTSEQUENCENO
        NUM_VAL              = 1
        VALUE                = "31"
      END_OBJECT             = GRINGPOINTSEQUENCENO
 
    END_OBJECT             = GPOLYGONCONTAINEROBJECT
 
    OBJECT                 = GPOLYGONCONTAINEROBJECT
      CLASS                = "2"
 
      OBJECT                 = EXCLUSIONGRINGFLAG
        NUM_VAL              = 1
        VALUE                = "NOT SET"
      END_OBJECT             = EXCLUSIONGRINGFLAG

      OBJECT                 = GRINGPOINTLATITUDE
        NUM_VAL              = 5
        VALUE                = "NOT SET"
      END_OBJECT             = GRINGPOINTLATITUDE
 
      OBJECT                 = GRINGPOINTLONGITUDE
        NUM_VAL              = 1
        VALUE                = "NOT SET"
      END_OBJECT             = GRINGPOINTLONGITUDE

      OBJECT                 = GRINGPOINTSEQUENCENO
        NUM_VAL              = 1
        VALUE                = "33"
      END_OBJECT             = GRINGPOINTSEQUENCENO
 
    END_OBJECT             = GPOLYGONCONTAINEROBJECT
 
  END_GROUP              = GPOLYGONCONTAINERGROUP
 
2       Nested metadata:
		There are certain metadata objects (gring plygons) which are always described as a group of related
metadata. To allow such groups to stay together in the MCF and the ODL tree, nested metadata objects can now also
be defined in the MCF. These are conveniently called "Container Objects" in the MCF with related metadata as its child
members. The child members are set individually as before. The container object does not have a value since it defines
a concept and not an entity.

eg. see the example above

In the case of multiple container objects (there could be more than one instances of gring polygons), when a call to
set a value of one of the child metadata objects is made, it is the container object which is duplicated with a different
class creating instances of all the child members. It is the users responsibility to set their values as well with
subsequent call.

eg. See the example above
3       Array manipulation:
                Tk5 imposed a restriction that metadata objects with values defined as arrays must be set with all
the elements filled. This restriction is now lifted. Instead the user is now allowed to fill the array partially if desired.
The NUM_VAL field in the MCF now describes the maximum number of values. Metadata tools now recognize the end of the data
array as follows:

	TYPE            END VALUE
 
        PGSt_integer    INT_MAX
        PGSt_uinteger   UINT_MAX
        PGSt_double     DBL_MAX
        char *  NULL
 
These values are defined in the limits.h and floats.h. Its analogous to null terminated strings defined as char[] arrays.

	FORTRAN users:
 
        Use PGSd_MET_INT_MAX, PGSd_MET_DBL_MAX and PGSd_MET_STR_END respectively.

eg. see routine example
 
Caution:
        If there are more data values than the maximum declared an error will be issued.

GLOBALS:
	PGSg_MET_MasterNode

FILES:
	None
FUNCTIONS_CALLED:
	PGS_MET_NameAndClass
	PGS_SMF_SetStaticMsg
	PGS_MET_ErrorMsg
	PGS_MET_GetDDAttr
	PGS_MET_CheckRange
	FindGroup
	FindObject
	ParentObject
	NextSubObject
	ParentAggregate
	PasteAggregate
	NewParameter
	CopyAggregate
	ODLConvertString
	ODLConverInteger
	ODLConverReal
	RemoveParameter
	RemoveValue
	ODLConvertString
	ODLConvertReal
	ODLConvertInteger
	NewValue
	FindParameter
	FirstValue
	PGS_TSF_GetTSFMaster
	PGS_SMF_TestErrorLevel

END_PROLOG:
***************************************************************************/


PGSt_SMF_status
PGS_MET_SetAttr(			    /* Sets metadata attribute values
					     */
             PGSt_MET_handle  	mdHandle,   /* metadata group within MCF containing the parameter */
             char 		*attrNameStr,  /* Parameter attr to be set */
             void 		*attrValue) /* Attribute value buffer with
                                             * the value to be set
                                             */
{
	AGGREGATE		mdGroupNode = NULL;
	AGGREGATE		containerParent = NULL;
	AGGREGATE               parentObject = NULL;
	AGGREGATE               baseNode = NULL;
	AGGREGATE               copyObject = NULL;
	AGGREGATE               parentAgg = NULL;
	AGGREGATE               subsubObject = NULL;
	PARAMETER		mdParmNode = NULL;	/* parameter attribute requested */
	PARAMETER		newParmNode = NULL;
	PARAMETER               classParam = NULL;
	PARAMETER               valueParameter = NULL;
	PARAMETER               typeParameter = NULL;
	PARAMETER               numParameter = NULL;
	PARAMETER               parentAggParameter = NULL;
        PARAMETER               locNode = NULL;
        PARAMETER               valuenode = NULL;
        PARAMETER               pgeParameter  = NULL;

        OBJECT                  mdNode = NULL;        /* parameter node */

	VALUE			mdValueNode = NULL;	/* value of the attribute */
	VALUE                   classValue = NULL;
	VALUE                   classValueNode = NULL;
	VALUE                   valueNode = NULL;
	VALUE                   NodeValue = NULL;
        VALUE                   locValue = NULL;
        VALUE                   pgeValue = NULL;
	VALUE_DATA		valueData;

	PGSt_integer            loopFlag = 0;
	PGSt_integer            loopCount = 0;
	char *			errInserts[PGSd_MET_MAX_ERR_INSERTS] = {NULL}; 
							/* Dynamic strings inserted in 
							 * error messages
							 */
	char *			funcName = "PGS_MET_SetAttr";
	char  			mdType[PGSd_MET_DD_STRING_L] = "NULL";
	PGSt_integer 		mdNumOfVal = 0;
	char 			intStr[10];
	char 			floatStr[DBL_DIG+sizeof("-0.E-999")];
	char			doubleStr[DBL_DIG+sizeof("-0.E-999")];
	char                    attrClass[PGSd_MET_CLASS_L]  = ""; /* attribute class */
        char                    attrName[PGSd_MET_NAME_L]  = ""; /* attribute Name */
	char **			inString = {NULL};	/* pointer used to place values in void buffer */
	PGSt_integer *		inInteger;	/* pointer used to place values in void buffer */
	PGSt_double *		inDouble;	/* pointer used to place values in void buffer */
	PGSt_real *		inFloat;	/* pointer used to place values in void buffer */

	PGSt_integer            mcfNumber = 0;
	char *                  mcfNumPtr = NULL;
        char *                  errPtr = NULL;
        char                    locMdHandle[PGSd_MET_GROUP_NAME_L] = "";
	char *                  ptr2;
	PGSt_integer            numdig;

#ifdef _PGS_THREADSAFE
    PGSt_SMF_status retTSF;
    AGGREGATE *PGSg_MET_MasterNode;
    PGSt_TSF_MasterStruct *masterTSF;

    /* get struct containing keys */
    retTSF = PGS_TSF_GetTSFMaster(&masterTSF);
    if (PGS_SMF_TestErrorLevel(retTSF))
    {
        return PGSTSF_E_GENERAL_FAILURE;
    }

    /* set local from TSD value */
    PGSg_MET_MasterNode = (AGGREGATE *) pthread_getspecific(
                                  masterTSF->keyArray[PGSd_TSF_KEYMETMASTERNODE]);
#else
	extern AGGREGATE	PGSg_MET_MasterNode[];
#endif
	
	/* clear the errno for the ODL routines */

        if (errno == ERANGE)
        {
                errno = 0;
        }
        /* find out the MCF number contained in the mdHandle argument*/

	if(mdHandle != NULL)
        {
                mcfNumPtr = strrchr(mdHandle, '#');
        }
        if(mcfNumPtr == NULL)
        {
                (void) PGS_SMF_SetStaticMsg(PGSMET_E_ILLEGAL_HANDLE, funcName);
                return(PGSMET_E_ILLEGAL_HANDLE);
        }

	mcfNumPtr++;
        mcfNumber = (PGSt_integer)strtol(mcfNumPtr, &errPtr, 10);
        if(mcfNumber < 0 || mcfNumber > PGSd_MET_NUM_OF_MCF || errPtr == mcfNumPtr)
        {
                (void) PGS_SMF_SetStaticMsg(PGSMET_E_ILLEGAL_HANDLE, funcName);
                return(PGSMET_E_ILLEGAL_HANDLE);
        }
	/* if master node is null then initialization has not taken  place */

        if(PGSg_MET_MasterNode[mcfNumber] == NULL)
        {
                (void) PGS_SMF_SetStaticMsg(PGSMET_E_NO_INITIALIZATION, funcName);
                return(PGSMET_E_NO_INITIALIZATION);
        }

	/* now make a local copy of the mdHandle */

        strcpy(locMdHandle, mdHandle);
        mcfNumPtr = strrchr(locMdHandle, '#');
        *mcfNumPtr = '\0';

	/* separate out the parm name and class */

        (void) PGS_MET_NameAndClass(attrNameStr, attrName, attrClass);

/* Begin changes for tk5+ 

		check if the object is multiple by checking the comment
		check if the object is a first by checking the class. it should be = M
		check if the parent is also an object for nested objects
		do nothing if not multiple
		create a copy of the parent if exists else crate a copy of the object

 End changes for tk5+ */
			
	   
#ifdef _PGS_THREADSAFE
        /* calling COTS - lock here */
        retTSF = PGS_TSF_LockIt(PGSd_TSF_LOCKODL);
        if (PGS_SMF_TestErrorLevel(retTSF))
        {
            return PGSTSF_E_GENERAL_FAILURE;
        }
#endif

	/* locate the group node */
	mdGroupNode = FindGroup(PGSg_MET_MasterNode[mcfNumber], locMdHandle);

#ifdef _PGS_THREADSAFE
        /* re-set TSD value */
        pthread_setspecific(masterTSF->keyArray[PGSd_TSF_KEYMETMASTERNODE],
                             PGSg_MET_MasterNode);
#endif

	mdNode = FindObject(mdGroupNode, attrName, attrClass);
/* adding code to check Data Location (CSWT) */

	if(mdNode == NULL && attrClass != NULL && *attrClass != '\0')
	{
/* Begin changes for tk5+ */
		/* there is a possibility that the object is a multiple
		   and therefore should be handled accordingly but in this
		   case class should not be NULL or null string
		*/

		mdNode = FindObject(mdGroupNode, attrName, NULL);
		if(mdNode != NULL) /* confirming that it might be a multiple */
		{
			/* confirm that it is a multiple . appl1 field is set to one */
			if(mdNode->appl1 == (long) PGSd_MET_MULTI_FLAG)
			{
				/* determine if its the firsttime or not
				   if first then simply set the class to the given value */
				/* make a copy of the aggregate */

				/* if the object belongs to a container object
                                   then all the objects in the container object has the same class */
                                parentObject = ParentObject(mdNode);
                                if(parentObject == NULL) /* object is not in a container */
                                {
					parentObject = mdNode;
				}
				else
				{
					/* it looks like a container object, however container object
					   must not have any parent objects */
					containerParent = ParentObject(parentObject);
					if(containerParent != NULL) /* error error error */
					{
					
                                                /* more than one level in the object hierarchy is being used which is not allowed */
                                                /* error message is:
                                                "Object descriptions enclosing related objects must not be enclosed themselevs 
						 by othe objects" */
 
                                                (void) PGS_MET_ErrorMsg(PGSMET_E_NESTED_OBJECTS,
                                                funcName, 0, errInserts);
#ifdef _PGS_THREADSAFE
                                                /* unlock - do not check return - 
                                                   we want to let user know about
                                                   previous error */
                                                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                                                return(PGSMET_E_NESTED_OBJECTS);
					}
				}
				if(strcmp(mdNode->objClass, PGSd_MET_MULTIPLE_FLAG) != 0)
                                {	
					copyObject = CopyAggregate(parentObject);
					if(copyObject == NULL)
                                        {
                                                /* this means that memory allocation has failed within ODL */
                                                /* error message is:
                                                "ODL routine failed to allocate memory" */
 
                                                (void) PGS_MET_ErrorMsg(PGSMET_E_ODL_MEM_ALLOC,
                                                funcName, 0, errInserts);
#ifdef _PGS_THREADSAFE
                                                /* unlock - do not check return - 
                                                   we want to let user know about
                                                   previous error */
                                                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                                                return(PGSMET_E_ODL_MEM_ALLOC);
                                        }
				}
				/* test that the object has an enclosing group statement which is not the MASTER GROUP */

				parentAgg = ParentAggregate(parentObject);
                                if(parentAgg->kind != KA_GROUP)
                                {
                                        /* error message is:
                                        "Multiple objects must have enclosing groups around them" */
 
                                        (void) PGS_MET_ErrorMsg(PGSMET_E_PARENT_GROUP,
                                        funcName, 0, errInserts);
					copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                                        /* unlock - do not check return - 
                                           we want to let user know about
                                           previous error */
                                        PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                                        return(PGSMET_E_PARENT_GROUP);
                                }
				else
				{
					parentAggParameter = FindParameter(parentAgg, PGSd_MET_GROUP_TYPE_STR);
					if(parentAggParameter != NULL)
					{
						/* error message is:
                                        	"Multiple objects must have enclosing groups around them" */
 
                                        	(void) PGS_MET_ErrorMsg(PGSMET_E_PARENT_GROUP,
                                        	funcName, 0, errInserts);
						copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                                               /* unlock - do not check return - 
                                                  we want to let user know about
                                                  previous error */
                                                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                                	        return(PGSMET_E_PARENT_GROUP);
					}
				}
				if(copyObject != NULL)
				{
					parentObject = PasteAggregate(parentAgg, copyObject);
				}
						
				/* set the class parameter(s) to the given class */
				baseNode = parentObject;
				do
				{
					classParam = FindParameter(parentObject, PGSd_MET_CLASS_STR);
					if(classParam == NULL)
					{
						/* error message is:
                                                "Container object must also have class parameter defined" */
 
                                                (void) PGS_MET_ErrorMsg(PGSMET_E_CLASS_PARAMETER,
                                                funcName, 0, errInserts);
						copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                                               /* unlock - do not check return - 
                                                  we want to let user know about
                                                  previous error */
                                                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                                                return(PGSMET_E_CLASS_PARAMETER);
					}
					classValue = FirstValue(classParam);
					classValue = RemoveValue(classValue);
					valueData = ODLConvertString(attrClass, strlen(attrClass)); 
					if(valueData.valid != 1)
                			{
                        			/* this means that memory allocation has failed within ODL */
                       				/* error message is:
                        			"ODL routine failed to allocate memory" */

                 				(void) PGS_MET_ErrorMsg(PGSMET_E_ODL_MEM_ALLOC,
                                        	funcName, 0, errInserts);
						copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                                               /* unlock - do not check return - 
                                                  we want to let user know about
                                                  previous error */
                                                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                        			return(PGSMET_E_ODL_MEM_ALLOC);
                			}
					classValueNode = NewValue(classParam, &valueData);
                			if(classValueNode == NULL)
                			{
                        			errInserts[0] = "value node";

      						/* error message is:
               					"Unable to create a new odl <value>, probably due to lack of memeory */

      						(void) PGS_MET_ErrorMsg(PGSMET_E_NEW_ODL_DATA_ERR,
                               			funcName, 1, errInserts);
						copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                                               /* unlock - do not check return - 
                                                  we want to let user know about
                                                  previous error */
                                                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
               					return(PGSMET_E_NEW_ODL_DATA_ERR);
       					}

					/* remove value only for Data Location is PGE so that new value could be inserted later */
					valueParameter = FindParameter(parentObject, PGSd_MET_ATTR_VALUE_STR);
					pgeParameter = FindParameter(parentObject, PGSd_MET_DATA_LOC_STR);
					if(pgeParameter)
					{
						pgeValue = FirstValue(pgeParameter);
						
					}

					if(strcmp(pgeValue->item.value.string, PGSd_MET_SET_BY_PGE) == 0)
					{
						if(valueParameter != NULL)
						{
							valueParameter = RemoveParameter(valueParameter);
							valueParameter = NULL;
						}
					}
						
					if(parentObject->objClass != NULL)
                                        {
                                                free(parentObject->objClass);
                                                parentObject->objClass = NULL;
                                                parentObject->objClass = (char *) malloc(strlen(classValueNode->item.value.string) + 1);
                                        }
					strcpy(parentObject->objClass, classValueNode->item.value.string);
					parentObject = NextSubObject(baseNode, parentObject);
					/* check that subobjects does not have subobjects of their own */
					subsubObject = NextSubObject(parentObject, parentObject);
					if(subsubObject != NULL)
					{
                                                /* more than one level in the object hierarchy is being used which is not allowed */
                                                /* error message is:
                                                "MetaData Objects are not allowed to enclose other objects */
 
                                                (void) PGS_MET_ErrorMsg(PGSMET_E_METADATA_CHILD,
                                                funcName, 0, errInserts);
						copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                                               /* unlock - do not check return - 
                                                  we want to let user know about
                                                  previous error */
                                                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                                                return(PGSMET_E_METADATA_CHILD);
                                        }	
				}
				while(parentObject != NULL);
				/* set the mdNode to the newly created object */
				mdNode = FindObject(baseNode, attrName, attrClass);
			}
			else /* its not a multiple although class is given */
			{
				/* simply issue a warning and treat the node as if resetting a value */
				/* warning message is:
                                                "Object is not supposed to be multiple therefore resetting the value */
 
                                                (void) PGS_MET_ErrorMsg(PGSMET_W_NOT_MULTIPLE,
                                                funcName, 0, errInserts);
			}
		}
/* End changes for tk5+ */
			
        }
	if(mdNode == NULL) /* object is still not found so isuue an error and return */
        {
               errInserts[0] = attrName;
	       if(mdGroupNode != NULL)
	       {
               		errInserts[1] = mdGroupNode->name;
	       }
	       else
	       {
			errInserts[1] = "NULL";
	       }
 
               if(attrName == (char *) NULL)
               {
                        errInserts[0] = "NULL";
               }
 
               /* error message is:
               "The requested parameter <parameter name> could not be found in <agg node>" */
 
               (void) PGS_MET_ErrorMsg(PGSMET_E_DD_UNKNOWN_PARM,
                                    funcName, 2, errInserts);
		copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                /* unlock - do not check return - we want to let user know about
                   previous error */
                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
               return(PGSMET_E_DD_UNKNOWN_PARM);
        }
        locNode = FindParameter(mdNode, PGSd_MET_DATA_LOC_STR);
        locValue = FirstValue(locNode);
        
        /* Based on the requirment of writing Metadat only for Data_Location is PCF, 
         * PGE, MCF, or TK, error messages will be generated when user tring to set
         * up attributes for DAAC, DSS, or DP 
         */
 
        if(strcmp(locValue->item.value.string, PGSd_MET_SET_BY_DP) == 0)
        {
           errInserts[0] = locValue->item.value.string;
           /* error message is:
              "Invalid data location for setting attribute value" */
           (void) PGS_MET_ErrorMsg(PGSMET_E_INVALID_LOCATION,funcName, 1, errInserts);
#ifdef _PGS_THREADSAFE
           /* unlock - do not check return - we want to let user know about
              previous error */
           PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
           return(PGSMET_E_INVALID_LOCATION);
        }
        else if(strcmp(locValue->item.value.string, PGSd_MET_SET_BY_DAAC) == 0)
        {
           errInserts[0] = locValue->item.value.string;
           /* error message is:
              "Invalid data location for setting attribute value" */
           (void) PGS_MET_ErrorMsg(PGSMET_E_INVALID_LOCATION,funcName, 1, errInserts);
#ifdef _PGS_THREADSAFE
           /* unlock - do not check return - we want to let user know about
              previous error */
           PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
           return(PGSMET_E_INVALID_LOCATION);
        }
        else if(strcmp(locValue->item.value.string, PGSd_MET_SET_BY_DSS) == 0)
        {
           errInserts[0] = locValue->item.value.string;
           /* error message is:
              "Invalid data location for setting attribute value" */
           (void) PGS_MET_ErrorMsg(PGSMET_E_INVALID_LOCATION,funcName, 1, errInserts);
#ifdef _PGS_THREADSAFE
           /* unlock - do not check return - we want to let user know about
              previous error */
           PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
           return(PGSMET_E_INVALID_LOCATION);
        }
        else if(strcmp(locValue->item.value.string, PGSd_MET_SET_BY_MCF) == 0)
        {
           valuenode = FindParameter(mdNode, PGSd_MET_ATTR_VALUE_STR);
           NodeValue = FirstValue(valuenode);
           if(valuenode != NULL && (NodeValue->item.type == TV_STRING || 
                                    NodeValue->item.type == TV_DATE ||
                                    NodeValue->item.type == TV_TIME ||
                                    NodeValue->item.type == TV_DATE_TIME ||  
                                    NodeValue->item.type == TV_REAL || 
                                    NodeValue->item.type == TV_INTEGER))
           {
              errInserts[0] = locValue->item.value.string;
              /* error message is:
                 "Attribute value set up in the data locateion MCF is not allowable to be written over" */
              (void) PGS_MET_ErrorMsg(PGSMET_E_SET_ERR,funcName, 1, errInserts);
#ifdef _PGS_THREADSAFE
           /* unlock - do not check return - we want to let user know about
              previous error */
           PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
              return(PGSMET_E_SET_ERR);
           }
        }

	/* create a new parameter node. This will be a copy of the metadata in 
	 * question and is given the new value. It will replace the original
	 * after every thing is accomplished successfully 
	 */
		
	newParmNode = NewParameter(mdNode, KP_ATTRIBUTE, PGSd_MET_NEW_VALUE_STR);
		
	if(newParmNode == NULL)
        {
               	errInserts[0] = "parameter";

 	        /* error message is:
                "Unable to create a new odl <parameter>, probably due to lack of memeory */

 	        (void) PGS_MET_ErrorMsg(PGSMET_E_NEW_ODL_DATA_ERR,
                                     funcName, 1, errInserts);
		copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                /* unlock - do not check return - we want to let user know about
                   previous error */
                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
               	return(PGSMET_E_NEW_ODL_DATA_ERR);
        }
		
/* Begin changes for tk5+ */

	typeParameter = FindParameter(mdNode, PGSd_MET_ATTR_TYPE_STR);	
        if(typeParameter == NULL)
	{
                errInserts[0] = PGSd_MET_ATTR_TYPE_STR;
                errInserts[1] = attrName;

                /* error message is:
                "Unable to obtain \
                 <attr> of metadata <parameter>" */

                (void) PGS_MET_ErrorMsg(PGSMET_E_NO_DEFINITION,
                                        funcName, 2, errInserts);
		(void) RemoveParameter(newParmNode);
		copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                /* unlock - do not check return - we want to let user know about
                   previous error */
                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                return(PGSMET_E_NO_DEFINITION);
        }
	valueNode = FirstValue(typeParameter);

	if(valueNode->item.type == TV_STRING || valueNode->item.type == TV_SYMBOL)
	{
        	strcpy(mdType, valueNode->item.value.string);
	}
	else
	{
                errInserts[0] = attrName;
 
                /* error message is:
                "Illegal type definition for metadata <attrName>. It should be a string" */
 
                (void) PGS_MET_ErrorMsg(PGSMET_E_ILLEGAL_TYPE,
                                        funcName, 1, errInserts);
                (void) RemoveParameter(newParmNode);
		copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                /* unlock - do not check return - we want to let user know about
                   previous error */
                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                return(PGSMET_E_ILLEGAL_TYPE);
        }
	numParameter = FindParameter(mdNode, PGSd_MET_ATTR_NUMOFVAL_STR);
        if(numParameter == NULL)
        {
                errInserts[0] = PGSd_MET_ATTR_NUMOFVAL_STR;
                errInserts[1] = attrName;
 
                /* error message is:
                "Unable to obtain \
                 <attr> of metadata <parameter>" */
 
                (void) PGS_MET_ErrorMsg(PGSMET_E_NO_DEFINITION,
                                        funcName, 2, errInserts);
                (void) RemoveParameter(newParmNode);
		copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                /* unlock - do not check return - we want to let user know about
                   previous error */
                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                return(PGSMET_E_NO_DEFINITION);
        }
	valueNode = FirstValue(numParameter);
        if(valueNode->item.type == TV_INTEGER)
        {
                mdNumOfVal = (PGSt_integer)valueNode->item.value.integer.number;
        }
        else
        {
                errInserts[0] = attrName;
 
                /* error message is:
                "Illegal NUMVAL definition for metadata <attrName>. It should be an integer " */
 
                (void) PGS_MET_ErrorMsg(PGSMET_E_ILLEGAL_NUMVAL,
                                        funcName, 1, errInserts);
                (void) RemoveParameter(newParmNode);
		copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                /* unlock - do not check return - we want to let user know about
                   previous error */
                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                return(PGSMET_E_ILLEGAL_NUMVAL);
        }

 /*  End changes to tk5+ */

/* Initialize various type pointers to point to the start of the void data space 
 * this is where this routines differs from the original routine. Fortran strings
 * have trailing blanks which are automatically handled by the cfortran.h. In this
 * particular case, because the void interface is used, anther type called STRVOID
 * was defined in the cfortran.h which would pass the length of the string as a
 * hidden variable through the wrapper routine. Once the length of the string is known
 * the question of trailing blanks is handled by this routine when manipulating strings
 */

	inString = (char **) attrValue;
	inInteger = (PGSt_integer *) attrValue;
	inFloat = (PGSt_real *) attrValue;
	inDouble = (PGSt_double *) attrValue;

/* set appropriate values to the New parameter fields */

	if(mdNumOfVal > 1)
	{
		newParmNode->value_kind = KV_SEQUENCE;
	}
	else
	{
		newParmNode->value_kind = KV_SCALAR;
	}

/* Begin tk5+ changes

	value count and columns should now be updated in the loop
	if maximum is exceeded then error should be returned
   End tk5+ changes
*/

	newParmNode->value_count = (long)0;
	newParmNode->columns = (short)0;
	newParmNode->rows = (short)1;
	
/* once onto the value nodes, simply fill the buffer with values from the 
 * value nodes according to the type of data
 */

/* Begin tk5+ changes
 
	loop now should be while loop 
	add datatypes for datetime and create date and time objects 
	delete range check

   End tk5+ changes
*/
	loopFlag = 0;
	loopCount = 0;
	if(mdNumOfVal == 1)
	{
		loopFlag = 1;
	}
	do
	{
		loopCount = loopCount + 1;
		if(strcmp(mdType, PGSd_MET_STRING_STR) == 0 || strcmp(mdType, PGSd_MET_DATETIME_STR) == 0 ||
		   strcmp(mdType, PGSd_MET_DATE_STR) == 0 || strcmp(mdType, PGSd_MET_TIME_STR) == 0)
		{
                        if(loopCount == 1 && strlen(*inString) > PGSd_MET_MAX_STRING_SET_L)
                        {
                                errInserts[0] = attrName;
                                /* error message is:
                                "The length of character string to be set for attribute value can 
                                not exceed PGSd_MET_MAX_STRING_SET_L(255)" */
                                (void) PGS_MET_ErrorMsg(PGSMET_E_ILLEGAL_LENGTH,
                                                        funcName, 1, errInserts);
                                (void) RemoveParameter(newParmNode);
                                copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                                /* unlock - do not check return - 
                                   we want to let user know about
                                   previous error */
                                PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                                return(PGSMET_E_ILLEGAL_LENGTH);
                        }
		        else valueData = ODLConvertString(*inString, strlen(*inString));

			inString++;
			if(loopCount != mdNumOfVal)
			{
				if(*inString == NULL || strcmp(*inString, "") == 0)
				{
					loopFlag = 1;
				}
			}
			else
			{
                                if(mdNumOfVal > PGSd_MET_MAX_ARRAY_ELEMENT_SIZE  &&
                                   *inString != NULL)
                                {
                                        errInserts[0] = attrName;
                                        /* error message is:
                                        "The size of array element of the attribute value assigned for the
                                        attribute name can not exceed PGSd_MET_MAX_ARRAY_ELEMENT_SIZE(1000)" */
                                        (void) PGS_MET_ErrorMsg(PGSMET_E_ARRAY_ELEMENT_SIZE,
                                                                funcName, 1, errInserts);
                                        loopFlag = 1;
                                        (void) RemoveParameter(newParmNode);
                                        copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                                        /* unlock - do not check return - 
                                           we want to let user know about
                                           previous error */
                                        PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                                        return(PGSMET_E_ARRAY_ELEMENT_SIZE);
                                }
                                else if(mdNumOfVal > PGSd_MET_MAX_ARRAY_ELEMENT_SIZE  &&
                                        *inString == NULL )
                                {
                                        errInserts[0] = attrName;
                                        /* error message is:
                                        "The size of array element of the attribute value assigned for the
                                        attribute name can not exceed PGSd_MET_MAX_ARRAY_ELEMENT_SIZE(1000)" */
                                        (void) PGS_MET_ErrorMsg(PGSMET_E_ARRAY_ELEMENT_SIZE,
                                                                funcName, 1, errInserts);
                                        loopFlag = 1;
                                        (void) RemoveParameter(newParmNode);
                                        copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                                        /* unlock - do not check return - 
                                           we want to let user know about
                                           previous error */
                                        PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                                        return(PGSMET_E_ARRAY_ELEMENT_SIZE);
                                }
                                loopFlag = 1;
			}
		}
		else if(strcmp(mdType, PGSd_MET_INTEGER_STR) ==0 || strcmp(mdType, PGSd_MET_UINTEGER_STR) ==0)
		{
			sprintf(intStr, "%d", *inInteger);
                        valueData = ODLConvertInteger(intStr, strlen(intStr));
			inInteger++;
			if(loopCount != mdNumOfVal)
			{
				if(strcmp(mdType, PGSd_MET_UINTEGER_STR) ==0)
				{
					if(*((PGSt_uinteger *)inInteger) == UINT_MAX)
					{
						loopFlag = 1;
					}
				}
				else
				{
					if(*inInteger == INT_MAX)
					{
						loopFlag = 1;
                        		}
				}
			}
			else
                        {
                                loopFlag = 1;
                        }
        	}
		else if(strcmp(mdType, PGSd_MET_FLOAT_STR) == 0)
		{
               		sprintf(floatStr, "%e", *inFloat);
                       	valueData = ODLConvertReal(floatStr, strlen(floatStr));
			inFloat++;
			if(loopCount != mdNumOfVal)
                        {
				if(*inFloat > (FLT_MAX - FLT_EPSILON))
                        	{
                                	loopFlag = 1;
                        	}
			}
			else
                        {
                                loopFlag = 1;
                        }
		}
		else if(strcmp(mdType, PGSd_MET_DOUBLE_STR) == 0)
		{
		      if(fabs(*inDouble) > 1000.)
			{
			  sprintf(doubleStr, "%.*g", DBL_DIG, *inDouble);
			  numdig=strlen(doubleStr);
			  if(numdig > DBL_DIG)
			    {
			      numdig = DBL_DIG;
			    }
			  if(numdig < 6) numdig = 6;
			  sprintf(doubleStr, "%.*e", numdig, *inDouble);
			}
		      else
			{
			  sprintf(doubleStr, "%.*g", DBL_DIG, *inDouble);
			  ptr2 = strrchr(doubleStr, '.');
			  if(ptr2 ==NULL)
			    {
			      strcat(doubleStr,".");
			    }
			}
			valueData = ODLConvertReal(doubleStr, strlen(doubleStr));
			inDouble++;
			if(loopCount != mdNumOfVal)
                        {
                                if(*inDouble >= DBL_MAX)
                                {
                                        loopFlag = 1;
                                }
                        }
			else
                        {
                                loopFlag = 1;
                        }
                }
		else
		{
			errInserts[0] = attrName;

                        /* error message is:
                        "Invalid data type definition in MCF for parameter <name>" */

                        (void) PGS_MET_ErrorMsg(PGSMET_E_INV_DATATYPE,
                        funcName, 1, errInserts);
			(void) RemoveParameter(newParmNode);
			copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                        /* unlock - do not check return - 
                           we want to let user know about
                           previous error */
                        PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                        return(PGSMET_E_INV_DATATYPE);
                }	
		if(valueData.valid != 1)
                {
			/* this means that memory allocation has failed within ODL */
                       /* error message is:
                        "ODL routine failed to allocate memory" */

                       (void) PGS_MET_ErrorMsg(PGSMET_E_ODL_MEM_ALLOC,
                                             funcName, 0, errInserts);
			(void) RemoveParameter(newParmNode);
			copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                        /* unlock - do not check return - 
                           we want to let user know about
                           previous error */
                        PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                        return(PGSMET_E_ODL_MEM_ALLOC);
                }

	/* attach the new value to the parameter node */

		mdValueNode = NewValue(newParmNode, &valueData);
		if(mdValueNode == NULL)
        	{
                	errInserts[0] = "value node";

 	               /* error message is:
        	        "Unable to create a new odl <value>, probably due to lack of memeory */

 	               (void) PGS_MET_ErrorMsg(PGSMET_E_NEW_ODL_DATA_ERR,
        	                             funcName, 1, errInserts);
			(void) RemoveParameter(newParmNode);
			copyObject = RemoveAggregate(copyObject);
#ifdef _PGS_THREADSAFE
                        /* unlock - do not check return - 
                           we want to let user know about
                           previous error */
                        PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif
                	return(PGSMET_E_NEW_ODL_DATA_ERR);
        	}
        	newParmNode->columns++;			
	}
	while(loopFlag == 0);
	
	/* find and remove the old parameter "VALUE" */
	
	mdParmNode = FindParameter(mdNode, PGSd_MET_ATTR_VALUE_STR);
	if(mdParmNode != NULL)
	{
		mdParmNode = RemoveParameter(mdParmNode);
	}

#ifdef _PGS_THREADSAFE
        retTSF = PGS_TSF_UnlockIt(PGSd_TSF_LOCKODL);
#endif

	/* rename the new parameter as the value parameter */
	strcpy(newParmNode->name, PGSd_MET_ATTR_VALUE_STR);

	return(PGS_S_SUCCESS);
}
