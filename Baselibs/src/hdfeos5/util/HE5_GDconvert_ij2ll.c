/*********************************************************
HE5_GDconver_ij2ll.c--

This function converts pixel coordinates i,j into lat/lon for a grid in 
an hdf-eos5 file. Once installed executable will be in hdfeos5/bin/<brand> 
directory. Followings are how to run the executible, and the outcome:

a) ./HE5_GDconver_ij2ll <input_hdf_file_name> <input_grid_name>

   will write ASCII output for i,j,lat,lon onto STDOUT for whole grid.

b) ./HE5_GDconvert_ij2ll <input_hdf_file_name> <input_grid_name> <output_file_name_template> <-a>
   will write ASCII output for i,j,lat,lon into a file.

c) ./HE5_GDconvert_ij2ll <input_hdf_file_name> <input_grid_name> <output_file_name_template> <-b>
   will write 3 output files with names constructed using output_file_name_template:
       1. An ascii file containing info for input file and grid.
       2. A binary file containing latitudes (64bit flaot data), 
          a row follows another row for the grid
       3. A binary file containing longitudes (64bit flaot data), 
          a row follows another row for the grid

d) ./HE5_GDconvert_ij2ll <input_hdf_file_name> <input_grid_name> STDOUT <-a> <i j>
   will write ASCII output for i,j,lat,lon onto STDOUT for a single pair.

User may type ./HE5_GDconvert_ij2ll to see what the usage is. Also typing a
dummy grid name will return error, specifying what the valid grid names 
are in the hdf file.

Author--
Abe Taaheri, Raytheon IIS

Dates--
2/20/2007   AT  First Programming 

*********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <math.h>
#include "HE5_HdfEosDef.h" 
#include "cproj.h"
#include "proj.h"

#define COMMAND_STRING      1024

typedef struct
{
  char inputFile[COMMAND_STRING];
  char gridName[COMMAND_STRING];
  char outputFile_nxny[COMMAND_STRING];
  char outputFile_lat[COMMAND_STRING];
  char outputFile_lon[COMMAND_STRING];
  char outFileFlag[COMMAND_STRING];
  long RowCol[2];
}
CommandArgument;

CommandArgument Com;

void CommandLineUsage()
{
    fprintf( stderr, "\tUsage (ASCII output onto STDOUT):\n\t./HE5_GDconvert_ij2ll <input_hdf_file_name> <input_grid_name>\n" );

    fprintf( stderr, " OR\n");
    fprintf( stderr, "\tUsage (ASCII output into a file):\n\t./HE5_GDconvert_ij2ll <input_hdf_file_name> <input_grid_name> <output_file_name_template> <-a>\n" );
 
    fprintf( stderr, " OR\n");
    fprintf( stderr, "\tUsage (for Binary output into a file):\n\t./HE5_GDconvert_ij2ll <input_hdf_file_name> <input_grid_name> <output_file_name_template> <-b>\n" );

    fprintf( stderr, " OR\n");
    fprintf( stderr, "\tUsage (lat/lon for a given (row col)):\n\t./HE5_GDconvert_ij2ll <input_hdf_file_name> <input_grid_name> STDOUT <-a> <i j>\n" );
 }
/* get: 
   input hdf file
   Grid name to process
   Output ASCII filename
*/
herr_t commandLineReader(int argc, char *argv[], CommandArgument *Com )
{
  char      *errbuf = NULL;/* Error message buffer */


  /* Allocate memory for error buffer */
  /* -------------------------------- */
  errbuf  = (char *)calloc( HE5_HDFE_ERRBUFSIZE, sizeof(char));
  if(errbuf == NULL)
    {
      H5Epush(__FILE__, "commandLineReader", __LINE__, H5E_RESOURCE, 
	      H5E_NOSPACE, "Cannot allocate memory to error buffer.");
      HE5_EHprint("Error: Cannot allocate memory to error buffer, occured", 
		  __FILE__, __LINE__);
      return(FAIL);
    }

  if ( argc < 3 || argc > 7 || argc == 6) /* display usage in case of no or 
					  insufficient arguments */
    {
      /* user need help read the command line option, it is not error */
      CommandLineUsage();
      exit(-1); 
    }
  
  /* read command line parameters */
  if(argc ==  7)
    {/* will write a lat/lon pair ASCII output to stdout */ 
      strcpy(Com->inputFile, argv[1]);
      strcpy(Com->gridName, argv[2]);
      strcpy(Com->outputFile_nxny, "");
      strcpy(Com->outputFile_lat, "");
      strcpy(Com->outputFile_lon, "");
      strcpy(Com->outFileFlag, argv[4]);
      Com->RowCol[0] = atol(argv[5]);
      Com->RowCol[1] = atol(argv[6]);
      if( Com->RowCol[0] < 0 || Com->RowCol[1] < 0)
	{
	  sprintf(errbuf, "Row or Column numbers cannot be negative.\n");
	  H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_ARGS, 
		  H5E_BADVALUE, errbuf);
	  HE5_EHprint(errbuf, __FILE__, __LINE__);
	  return (-1);
	}
      free(errbuf);
      return 0;
    }
  else if (argc ==  5)
    {/* will write (rows X cols) ASCII or BINARY output to output file */ 
      strcpy(Com->inputFile, argv[1]);
      strcpy(Com->gridName, argv[2]);
      strcpy(Com->outputFile_nxny, argv[3]);
      strcpy(Com->outputFile_lat, argv[3]);
      strcpy(Com->outputFile_lon, argv[3]);
      strcat(Com->outputFile_nxny,"_ASCII");
      strcat(Com->outputFile_lat,"_lat");
      strcat(Com->outputFile_lon,"_lon");
      strcpy(Com->outFileFlag, argv[4]);
      Com->RowCol[0] = -1;
      Com->RowCol[1] = -1;
      free(errbuf);
      return 0;
    }
  else if(argc ==  4)
    {/* will write (rows X cols) ASCII output to output file */ 
      strcpy(Com->inputFile, argv[1]);
      strcpy(Com->gridName, argv[2]);
      strcpy(Com->outputFile_nxny, argv[3]);
      strcpy(Com->outputFile_lat, argv[3]);
      strcpy(Com->outputFile_lon, argv[3]);
      strcat(Com->outputFile_nxny,"_ASCII");
      strcat(Com->outputFile_lat,"_lat");
      strcat(Com->outputFile_lon,"_lon");
      strcpy(Com->outFileFlag, "-a");
      Com->RowCol[0] = -1;
      Com->RowCol[1] = -1;
      free(errbuf);
      return 0;
    }
  else if (argc ==  3)
    { /* will write (rows X cols) ASCII output to stdout */ 
      strcpy(Com->inputFile, argv[1]);
      strcpy(Com->gridName, argv[2]);
      strcpy(Com->outputFile_nxny, "");
      strcpy(Com->outputFile_lat, "");
      strcpy(Com->outputFile_lon, "");
      strcpy(Com->outFileFlag, "-a");
      Com->RowCol[0] = -1;
      Com->RowCol[1] = -1;
      free(errbuf);
      return 0;
    }
  free(errbuf);
  return 0;
}     


/* Separating string */

void separating_String(char *stringlist, long *nstring, char *strings[], 
		       char strs[])
{
  char  *astring = NULL;
    
  *nstring=0;
  astring=strtok(stringlist, strs);
  while (astring!=NULL)
  {
    strings[*nstring]=astring;  
    astring=strtok(NULL, strs);
    *nstring=*nstring+1;
  } 
}



herr_t main(int argc, char *argv[])
{
  herr_t     status = 0;
  int        i, j, jx, jy,k, is;
  int        Singl_latlon = 0; /* get the lat/lon for whole grid */
  hid_t      gdfid, gdid[10];
  long       xdimsize, ydimsize;
  long       ngrid;
  int        grid_not_found;
  char       gridlist[1000], tmp_gridlist[1000],*grids[20];
  int        projcode=0; 
  int        zonecode=0;
  double     projparam[16]={0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		            0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
			    0.0, 0.0, 0.0, 0.0};
  int        spherecode =0;
  long       strsize;
  int        pixregcode, origincode;
  double     upleft[2], lowright[2];
  double     lowleft[2], upright[2];
  long       npnts;
  double    *lats = NULL; 
  double    *lons = NULL;
  long      *rows = NULL; 
  long      *cols = NULL;
  int        v1;

  FILE *outfile_nxny = NULL;  /* Pointer for ASCII outfile file for rows,cols*/
  FILE *outfile_lat = NULL;   /* Pointer for outfile file for lats*/
  FILE *outfile_lon = NULL;   /* Pointer for outfile file for lons */
  char      *errbuf = NULL;   /* Error message buffer */
  char projection[64];

  /* Allocate memory for error buffer */
  /* -------------------------------- */
  errbuf  = (char *)calloc( HE5_HDFE_ERRBUFSIZE, sizeof(char));
  if(errbuf == NULL)
    {
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_RESOURCE, 
	      H5E_NOSPACE, "Cannot allocate memory to error buffer.");
      HE5_EHprint("Error: Cannot allocate memory to error buffer, occured", 
		  __FILE__, __LINE__);
      return(FAIL);
    }

 /* process command-line arguments (to get parameter filename) */
  status = commandLineReader(argc, argv, &Com);
    if (status == -1)
    {
      status = -1;
      sprintf(errbuf, "Problem with command line entries.\n");
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_ARGS, 
	      H5E_BADVALUE,errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      free(errbuf);
      return(status);
    }
  gdfid=HE5_GDopen((&Com)->inputFile, H5F_ACC_RDONLY);
  
  if (gdfid == -1)
    {
      status = -1;
      sprintf(errbuf, "The file %s cannot be opened.\n",(&Com)->inputFile);
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FILE, 
	      H5E_CANTOPENFILE, errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      free(errbuf);
      return(status);
    }
  
	  
  /*
    Inquire grid
  */
  
  ngrid=HE5_GDinqgrid((&Com)->inputFile, gridlist, &strsize);
  if (ngrid<1)
    {
      HE5_GDclose(gdfid);
      status = -1;
      sprintf(errbuf, "No grid exists in the file %s.\n",(&Com)->inputFile);
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FUNC, 
	      H5E_CANTINIT, errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      free(errbuf);
      return(status);
    }
  strcpy(tmp_gridlist, gridlist);
  separating_String(tmp_gridlist, &ngrid, grids, ",");

  /* check for  the desired grid */
  grid_not_found = 0;
  for (i=0; i<ngrid; i++)
    {
      if(strcmp((&Com)->gridName,grids[i]) !=0)
	{
	  if(i == (ngrid -1))
	    {
	      grid_not_found = 1;
	      break;
	    }
	  else
	    {
	      continue;
	    }
	}
      else
	{
	  is =i;
	  break;
	}
    }
  
  if(grid_not_found == 1)
    {
      status = -1;
      sprintf(errbuf, "The file %s does not contain grid %s. The grids in this file are: %s\n",(&Com)->inputFile, (&Com)->gridName,gridlist);
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FUNC, 
	      H5E_CANTINIT, errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      HE5_GDclose(gdfid);
      free(errbuf);
      return(status);
    }
  
  gdid[is]=HE5_GDattach(gdfid, grids[is]);
  
  status=HE5_GDprojinfo(gdid[is], &projcode, &zonecode, &spherecode, 
			projparam);
  
  if (status==-1)
    {
      sprintf(errbuf, "No projection information for grid %s.\n",gdid[is]);
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FUNC, 
	      H5E_CANTINIT, errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      HE5_GDdetach(gdid[is]);
      HE5_GDclose(gdfid);
      free(errbuf);
      return(status);
    }

    /* set grid INFO */

    switch (projcode)
      {
      case HE5_GCTP_GEO:
	strcpy(projection,"GEOGRAPHIC");
	break;
      case HE5_GCTP_UTM:
	strcpy(projection,"UNIVERSAL TRANSVERSE MERCATOR");
	break;
      case HE5_GCTP_LAMCC:
 	strcpy(projection,"LAMBERT CONFORMAL CONIC");
	break;
      case HE5_GCTP_PS:
	strcpy(projection,"POLAR STREOGRAPHIC");
	break;
      case HE5_GCTP_POLYC:
	strcpy(projection,"POLYCONIC");
	break;
      case HE5_GCTP_TM:
	strcpy(projection,"TRANSVERSE MERCATOR");
	break;
      case HE5_GCTP_LAMAZ:
	strcpy(projection,"LAMBERT AZIMUTHAL");
	break;
      case HE5_GCTP_HOM:
	strcpy(projection,"HOTIN OBLIQUE MERCATOR");
	break;
      case HE5_GCTP_SOM:
	strcpy(projection,"SPACE OBLIQUE MERCATOR");
	break;
      case HE5_GCTP_GOOD:
	strcpy(projection,"INTERRPTED GOODE");
	break;
      case HE5_GCTP_SPCS:
	strcpy(projection,"STATE PLANE");	
	break;
      case 99:
	strcpy(projection,"INTEGERIZED SINUSOIDAL");
	break;
      case HE5_GCTP_SNSOID:
	strcpy(projection,"SINUSOIDAL");
	break;
      case HE5_GCTP_MERCAT:
	strcpy(projection,"MERCATOR");
	break;
      case HE5_GCTP_ALBERS:
	strcpy(projection,"ALBERS CONICAL EQUAL AREA");
	break;
	
      default:
 	strcpy(projection,"NOT SUPPORTED");
	fprintf(stdout,"Projection type does not exist.");
      }

  status=HE5_GDpixreginfo(gdid[is], &pixregcode);
  if (status==-1)
    {
      sprintf(errbuf, "No pixel rgistration code information for grid %s, assuming the default HE5_HDFE_CENTER.\n",gdid[is]);
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FUNC, 
	      H5E_CANTINIT, errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      pixregcode = HE5_HDFE_CENTER;
    }
  else
    {
      if(pixregcode == HE5_HDFE_CORNER)
	{
	  status=HE5_GDorigininfo(gdid[is], &origincode);
	  if (status==-1)
	    {
	      sprintf(errbuf, "No origin information for grid %s, assuming the default HE5_HDFE_GD_UL.\n",gdid[is]);
	      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FUNC, 
		      H5E_CANTINIT, errbuf);
	      HE5_EHprint(errbuf, __FILE__, __LINE__);
	      origincode = HE5_HDFE_GD_UL;
	    }
	}
    }

  status=HE5_GDgridinfo(gdid[is], &xdimsize, &ydimsize, upleft, lowright);
  if (status==-1)
    {
      sprintf(errbuf, "No grid information for grid %s.\n",gdid[is]);
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FUNC, 
	      H5E_CANTINIT, errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      HE5_GDdetach(gdid[is]);
      HE5_GDclose(gdfid);
      free(errbuf);
      return(status);
    }



  /* lat/lon for other two corners UR and LL */
  /* these lat/los are in DMS format for GEO projection, and
     meters for others.
  */

  upright[0] = lowright[0];
  upright[1] = upleft[1];
  lowleft[0] = upleft[0];
  lowleft[1] = lowright[1];

  /* we now have upper left, Lower right corners, xdim, and ydim  
     lets get now the lat/lon for every i/j */

  if((&Com)->RowCol[0] >= 0 && (&Com)->RowCol[1] >= 0)
    {/* check to see if they are valid numbers */
      if((&Com)->RowCol[0] > (ydimsize-1))
	{
	  status = -1;
	  sprintf(errbuf, "Requestrd Row number %ld invalid. Should be less than %d.\n",(&Com)->RowCol[0], ydimsize);
	  H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_ARGS, 
		  H5E_BADVALUE, errbuf);
	  HE5_EHprint(errbuf, __FILE__, __LINE__);
	  HE5_GDdetach(gdid[is]);
	  HE5_GDclose(gdfid);
	  free(errbuf);
	  return(status);
	}

      if((&Com)->RowCol[1] > (xdimsize-1))
	{
	  status = -1;
	  sprintf(errbuf, "Requestrd Column number %ld invalid. Should be less than %d.\n",(&Com)->RowCol[1],xdimsize);
	  H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_ARGS, 
		  H5E_BADVALUE, errbuf);
	  HE5_EHprint(errbuf, __FILE__, __LINE__);
	  HE5_GDdetach(gdid[is]);
	  HE5_GDclose(gdfid);
	  free(errbuf);
	  return(status);
	}
      Singl_latlon = 1;/* get the lat/lon for a single pixel in the grid */
    }
  /* testing
  printf("Rows: ydimsize=%d\n",ydimsize);
  printf("cols: xdimsize=%d\n",xdimsize);
  printf("Row#: %d\n",(&Com)->RowCol[0]);
  printf("col#: %d\n",(&Com)->RowCol[1]);
  */
  if(Singl_latlon == 1)
    {
      npnts = 1;
    }
  else
    {
      npnts = xdimsize * ydimsize;
    }
      
  lons = (double *) malloc(npnts*sizeof(double));
  if(lons == NULL) 
    {
      sprintf(errbuf, "memory problem allocating space to lons....\n");
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FILE, 
	      H5E_NOSPACE, errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      HE5_GDdetach(gdid[is]);
      HE5_GDclose(gdfid);
      free(errbuf);
      return(status);
    }
  
  lats = (double *) malloc(npnts*sizeof(double));
  if(lats == NULL) 
    {
      sprintf(errbuf, "memory problem allocating space to lats....\n");
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FILE, 
	      H5E_NOSPACE, errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      HE5_GDdetach(gdid[is]);
      HE5_GDclose(gdfid);
      free(errbuf);
      return(status);
    }
  
  rows = (long *) malloc(npnts*sizeof(long));
  if(rows == NULL) 
    {
      sprintf(errbuf, "memory problem allocating space to rows....\n");
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FILE, 
	      H5E_NOSPACE, errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      HE5_GDdetach(gdid[is]);
      HE5_GDclose(gdfid);
      free(errbuf);
      return(status);
    }
  
  cols = (long *) malloc(npnts*sizeof(long));
  if(cols == NULL) 
    {
      sprintf(errbuf, "memory problem allocating space to cols....\n");
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FILE, 
	      H5E_NOSPACE, errbuf);
      HE5_EHprint(errbuf, __FILE__, __LINE__);
      HE5_GDdetach(gdid[is]);
      HE5_GDclose(gdfid);
      free(errbuf);
      return(status);
    }
  
  if(Singl_latlon == 1)
    {
      rows[0] = (long) (&Com)->RowCol[0];
      cols[0] = (long) (&Com)->RowCol[1];
    }
  else
    {
      /* LOAD UP row and col arrays for use by HE5_GDconvert_ij2ll */
      /* ----------------------------------------------*/
      k = 0;   /*  used for counting */
      
      for( jy = 0; jy < ydimsize; jy++ )   /* LOOP for each row     */
	{
	  for( jx = 0; jx < xdimsize; jx++ )   /* LOOP for each column  */
	    {  
	      rows[k] = jy;
	      cols[k] = jx;
	      k++;
	    }
	}
    }

  fprintf(stdout,"\n...calculating lat/lon values for grid...please wait... \n");

  /* Get lat/lon values of grid for given i,j values*/
  /* ---------------------------------------------- */
  status  =  HE5_GDij2ll(projcode, zonecode, projparam,
		     spherecode, xdimsize, ydimsize,
		     upleft, lowright,
		     npnts, rows, cols,
		     lons, lats, pixregcode, origincode);

  if(status == -1)
    {
      free(rows);
      rows = NULL;
      free(cols);
      cols = NULL;
      free(lons);
      lons = NULL;
      free(lats);
      lats = NULL;

      sprintf(errbuf, "Problem converting i,j to lat,lon in grid %s.\n",(&Com)->gridName);
      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FUNC, 
	      H5E_CANTINIT, errbuf);

      HE5_EHprint(errbuf, __FILE__, __LINE__);
      HE5_GDdetach(gdid[is]);
      HE5_GDclose(gdfid);
      free(errbuf);
      return(status);
    }
  else
    {
      /* open output ASCII/Binary file and print results to it */
      /* Or just print it stdout                               */

      if(strcmp((&Com)->outputFile_nxny, "") != 0) /* Put output into a file */
	{
	  if(strcmp((&Com)->outFileFlag,"-b") == 0) /*open in binary mode*/
	    {
	      outfile_nxny=fopen((&Com)->outputFile_nxny, "w" );
	      outfile_lat=fopen((&Com)->outputFile_lat, "wb" );
	      outfile_lon=fopen((&Com)->outputFile_lon, "wb" );
	    }
	  else if(strcmp((&Com)->outFileFlag,"-a") == 0) /*open in ascii mode*/
	    {
	      outfile_nxny=fopen((&Com)->outputFile_nxny, "w" );
	    }
	  else /*open in ascii mode*/
	    {
	      sprintf(errbuf, "The flag for creating ASCII/Binary output file %s is -a or -b. \nAssuming -a : ASCII output file will be created.\n");
	      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_ARGS, 
		      H5E_BADVALUE, errbuf);
	      HE5_EHprint(errbuf, __FILE__, __LINE__);
	      outfile_nxny=fopen((&Com)->outputFile_nxny, "w" );
	    }

	  if(outfile_nxny == NULL)
	    {
	      free(rows);
	      rows = NULL;
	      free(cols);
	      cols = NULL;
	      free(lons);
	      lons = NULL;
	      free(lats);
	      lats = NULL;
	      status = -1;
	      sprintf(errbuf, "The file %s cannot be opened.\n",(&Com)->outputFile_nxny);
	      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FILE, 
		      H5E_CANTOPENFILE, errbuf);
	      HE5_EHprint(errbuf, __FILE__, __LINE__);
	      HE5_GDdetach(gdid[is]);
	      HE5_GDclose(gdfid);
	      free(errbuf);
	      if(outfile_lat == NULL ) fclose(outfile_lat);
	      if(outfile_lon == NULL ) fclose(outfile_lon);
	      return(status);
	    }
	  else if(outfile_lat == NULL && 
		  (strcmp((&Com)->outFileFlag,"-b") == 0))
	    {
	      free(rows);
	      rows = NULL;
	      free(cols);
	      cols = NULL;
	      free(lons);
	      lons = NULL;
	      free(lats);
	      lats = NULL;
	      status = -1;
	      sprintf(errbuf, "The file %s cannot be opened.\n",
		      (&Com)->outputFile_lat);
	      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FILE, 
		      H5E_CANTOPENFILE, errbuf);
	      HE5_EHprint(errbuf, __FILE__, __LINE__);
	      HE5_GDdetach(gdid[is]);
	      HE5_GDclose(gdfid);
	      free(errbuf);
	      fclose(outfile_nxny);
	      if(outfile_lon == NULL ) fclose(outfile_lon);
	      return(status);
	    }
	  else if(outfile_lon == NULL && 
		  (strcmp((&Com)->outFileFlag,"-b") == 0))
	    {
	      free(rows);
	      rows = NULL;
	      free(cols);
	      cols = NULL;
	      free(lons);
	      lons = NULL;
	      free(lats);
	      lats = NULL;
	      status = -1;
	      sprintf(errbuf, "The file %s cannot be opened.\n",
		      (&Com)->outputFile_lon);
	      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, H5E_FILE, 
		      H5E_CANTOPENFILE, errbuf);
	      HE5_EHprint(errbuf, __FILE__, __LINE__);
	      HE5_GDdetach(gdid[is]);
	      HE5_GDclose(gdfid);
	      free(errbuf);
	      fclose(outfile_nxny);
	      fclose(outfile_lat);
	      return(status);
	    }
	  else
	    {
	      if(strcmp((&Com)->outFileFlag,"-b") == 0) /* binary data */
		{
		  fprintf(stdout,"The output binary file contains 64bit float data for Latitude and  Longitude of the desired grid: {n=(%d * %d) 64bit float data; First row at the beginning, Last row at the end}\n\n", ydimsize,xdimsize);

		  /* 
		     write lat/lon to binary file in dataset format:
		     (ydimsize Rows) X (xdimsize cloums)
		  */
		  status = 0;
		  fprintf(outfile_nxny,"Input file: %s\nGrid: %s\nNumber of rows in the grid = %d\nNumber of columns in the grid = %d\nThe output binary files containing latitudes and longitudes:\n\t%s\n\t%s\n\n Each file contains %d X %d of 64bit float data; First row at the beginning, Last row at the end of the file.\nProjection:  %s\nProjection Parameters: ( ", (&Com)->inputFile, (&Com)->gridName, ydimsize, xdimsize, (&Com)->outputFile_lat, (&Com)->outputFile_lon, ydimsize, xdimsize, projection);
		  for( v1=0; v1<13; v1++) 
		    {
		      fprintf(outfile_nxny,"  %lf",projparam[v1]);
		    }
		  fprintf(outfile_nxny," )");
		  if(projcode == HE5_GCTP_UTM)
		    {
		      fprintf(outfile_nxny,"Zonecode: %d\n\n", zonecode);
		    }
		  else
		    {
		      fprintf(outfile_nxny,"\n\n");
		    }

		  if(status != -1 && 
		     fwrite(lats, sizeof lats[0], npnts, outfile_lat) != npnts)
		    {
		      status = -1;
		      sprintf(errbuf, "Error writing latitudes to binary output.\n");
		      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, 
			      H5E_DATASET, H5E_WRITEERROR, errbuf);
		      HE5_EHprint(errbuf, __FILE__, __LINE__);
		    }
		  if(status != -1 && 
		     fwrite(lons, sizeof lons[0], npnts, outfile_lon) != npnts)
		    {
		      status = -1;
		      sprintf(errbuf, "Error writing longitudes to binary output.\n");
		      H5Epush(__FILE__, "HE5_GDconvert_ij2ll", __LINE__, 
			      H5E_DATASET, H5E_WRITEERROR, errbuf);
		      HE5_EHprint(errbuf, __FILE__, __LINE__);
		    }
		  if(status == -1)
		    {
		      fflush(outfile_nxny);
		      if(strcmp((&Com)->outFileFlag,"-b") == 0) 
			fflush(outfile_lat);
		      if(strcmp((&Com)->outFileFlag,"-b") == 0) 
			fflush(outfile_lon);
		      free(rows);
		      rows = NULL;
		      free(cols);
		      cols = NULL;
		      free(lons);
		      lons = NULL;
		      free(lats);
		      lats = NULL;
		      HE5_GDdetach(gdid[is]);
		      HE5_GDclose(gdfid);
		      free(errbuf);
		      if(outfile_nxny == NULL) fclose(outfile_nxny);
		      if(outfile_lat == NULL && 
			 (strcmp((&Com)->outFileFlag,"-b") == 0)) 
			fclose(outfile_lat);
		      if(outfile_lon == NULL && 
			 (strcmp((&Com)->outFileFlag,"-b") == 0)) 
			fclose(outfile_lon);
		      return(-1);
		    }
		}
	      else/* ascii data to output file */
		{
		  fprintf(stdout,"Number of rows =%d\n",ydimsize);
 		  fprintf(stdout,"Number of cols =%d\n",xdimsize);
 		  fprintf(stdout,"Projection: %s\nProjection Parameters: ( ",projection);
		  for( v1=0; v1<13; v1++) 
		    {
		      fprintf(stdout,"  %lf",projparam[v1]);
		    }
		  fprintf(stdout," )");
		  if(projcode == HE5_GCTP_UTM)
		    {
		      fprintf(stdout,"Zonecode: %d\n", zonecode);
		    }
		  else
		    {
		      fprintf(stdout,"\n");
		    }

		  fprintf(stdout,"\n");
		  fprintf(stdout,"Order of paramters written to output file:\n");
		  fprintf(stdout,"row column Latitude Longitude\n");
		  fprintf(outfile_nxny,"Input file: %s\n",(&Com)->inputFile);
		  fprintf(outfile_nxny,"Grid: %s\n",(&Com)->gridName);
		  fprintf(outfile_nxny,"Number of rows =%d\n",ydimsize);
 		  fprintf(outfile_nxny,"Number of cols =%d\n",xdimsize);
 		  fprintf(outfile_nxny,"Projection: %s\nProjection Parameters: ( ",projection);
		  for( v1=0; v1<13; v1++) 
		    {
		      fprintf(outfile_nxny,"  %lf",projparam[v1]);
		    }
		  fprintf(outfile_nxny," )");
		  if(projcode == HE5_GCTP_UTM)
		    {
		      fprintf(outfile_nxny,"Zonecode: %d\n\n", zonecode);
		    }
		  else
		    {
		      fprintf(outfile_nxny,"\n\n");
		    }

		  for (k=0; k<npnts; k++)
		    {
		      fprintf(outfile_nxny,"%d %d %lf %lf\n",
			      rows[k], cols[k], lats[k], lons[k]);
		    }
		}
	    }

	  fflush(outfile_nxny);
	  if(strcmp((&Com)->outFileFlag,"-b") == 0) fflush(outfile_lat);
	  if(strcmp((&Com)->outFileFlag,"-b") == 0) fflush(outfile_lon);
	  free(rows);
	  rows = NULL;
	  free(cols);
	  cols = NULL;
	  free(lons);
	  lons = NULL;
	  free(lats);
	  lats = NULL;
	  HE5_GDdetach(gdid[is]);
	  HE5_GDclose(gdfid);
	  free(errbuf);
	  if(outfile_nxny == NULL) fclose(outfile_nxny);
	  if(outfile_lat == NULL  && 
	     (strcmp((&Com)->outFileFlag,"-b") == 0)) fclose(outfile_lat);
	  if(outfile_lon == NULL  && 
	     (strcmp((&Com)->outFileFlag,"-b") == 0)) fclose(outfile_lon);
	  return(0);
	}
      else
	{
	  /* write output to STDOUT */
	  fprintf(stdout,"row column Latitude Longitude\n");
	  for (k=0; k<npnts; k++)
	    {
	      fprintf(stdout,"%d %d %lf %lf\n",rows[k], cols[k], 
		      lats[k], lons[k]);
	    }
	  
	  fflush(stdout);
	  free(rows);
	  rows = NULL;
	  free(cols);
	  cols = NULL;
	  free(lons);
	  lons = NULL;
	  free(lats);
	  lats = NULL;
	  HE5_GDdetach(gdid[is]);
	  HE5_GDclose(gdfid);
	  free(errbuf);
	  return(0);
	}
    }
}

