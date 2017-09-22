/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */

#include <HE5_HdfEosDef.h>

  typedef struct 
	{
	  double    Time;
	  float     Conc[4];
	  char      Spc[8];
	} CmpData_1;


  typedef struct 
	{
	  char      Label[8];
	  double    Lon;
	  double    Lat;
	  int       Date;
	  char      Id[8];
	} CmpData_2;

  typedef struct
	{
          double      Time;
          float       Rain;
          float       Temp;
          char        Id[8];
	} CmpData_3;

  typedef struct
        {
          char        Label[10];
          int         Date;
          int         Weight;
          char        Id[8];
        } CmpData_4;

  typedef struct
	{
	  double      Time;
	  double      Lon;
	  double      Lat;
	  float       Rain;
	  float       Temp;
	  char        Id[8];
	} CmpData_5;


#define NAMEBUF_SIZE 100

int main()
{
  FILE            *fp;

  herr_t          status = FAIL;

  int             n, date, wgt, IntAttr = 9999;

  hid_t           ptfid = FAIL, PTid = FAIL;

  hsize_t         count[1];
  
  size_t          datasize = 0;
  
  float           conc[4], rain, temp, flt = -7.5;

  double          time, lon, lat;

  char            spc[8], desc[16], id[ 2 ];    

  char            namebuf[NAMEBUF_SIZE];

  char            *srcdir;


  CmpData_1 datbuf_1[20];
  CmpData_2 datbuf_2[5];
  CmpData_3 datbuf_3[25];
  CmpData_4 datbuf_4[5];
  CmpData_5 datbuf_5[30];

  /* Open the HDF-EOS file, "Point.h5" */
  /* --------------------------------- */
  ptfid = HE5_PTopen("Point.h5", H5F_ACC_RDWR);

  /* Write to Simple Point */
  /* --------------------- */
  PTid = HE5_PTattach(ptfid, "Simple Point");

  /* Open input data file */
  /* -------------------- */

  /* Set up path if the input file is in another directory */
  srcdir = getenv("srcdir");
  if(srcdir != NULL && (strlen(srcdir) + strlen("simple.txt") + 2) <= NAMEBUF_SIZE)
  {  
    strcpy(namebuf, srcdir);
    strcat(namebuf, "/simple.txt");
  }
  else
  {
    strcpy(namebuf, "simple.txt");
  }

  fp = fopen(namebuf, "rb");
  n = 0;
  while(fscanf(fp, "%lf %f %f %f %f %s", &time, &conc[0], &conc[1], &conc[2], &conc[3], spc) != -1)
    {
 	  datbuf_1[n].Time    = time;
	  datbuf_1[n].Conc[0] = conc[0];
	  datbuf_1[n].Conc[1] = conc[1];
	  datbuf_1[n].Conc[2] = conc[2];
	  datbuf_1[n].Conc[3] = conc[3];
	  memmove(datbuf_1[n].Spc, spc, sizeof(char)*strlen(spc));
	  datbuf_1[n].Spc[strlen(spc)] = 0;
	  n++;
    }

  fclose(fp);
    
  datasize = (size_t)sizeof(CmpData_1);
  count[0] = n;

  status = HE5_PTwritelevel(PTid, 0, count, &datasize, datbuf_1);
  printf("Status returned by HE5_PTwritelevel() :   %d \n", status);
    
  status = HE5_PTdetach(PTid);
  printf("Status returned by HE5_PTdetach() :       %d \n", status);
    
  /* Write to Fixed Buoy Point */
  /* ------------------------- */
  PTid = HE5_PTattach(ptfid, "FixedBuoy Point");


  /* Write First (0th) Level */
  /* ----------------------- */

  /* Set up path if the input file is in another directory */
  srcdir = getenv("srcdir");
  if(srcdir != NULL && (strlen(srcdir) + strlen("simple.txt") + 2) <= NAMEBUF_SIZE)
  {  
    strcpy(namebuf, srcdir);
    strcat(namebuf, "/fixedBuoy0.txt");
  }
  else
  {
    strcpy(namebuf, "fixedBuoy0.txt");
  }

  fp = fopen(namebuf, "r");

  n = 0;
  while(fscanf(fp, "%s %lf %lf %d  %s", desc, &lon, &lat, &date, id) != -1)
    {
	  strcpy(datbuf_2[n].Label, desc);
 	  datbuf_2[n].Lon     = lon;
 	  datbuf_2[n].Lat     = lat;
 	  datbuf_2[n].Date    = date;
 	  memmove(datbuf_2[n].Id, id, sizeof(char)*strlen(id));
	  datbuf_2[n].Id[strlen(id)] = 0;
	  n++;
    }

  fclose(fp);
    
  datasize = (size_t)sizeof(CmpData_2);
  count[0] = n;

  status = HE5_PTwritelevel(PTid, 0, count, &datasize, datbuf_2);
  printf("Status returned by HE5_PTwritelevel() :   %d \n", status);


  /* Write Second (1th) Level */
  /* ------------------------ */

  /* Set up path if the input file is in another directory */
  srcdir = getenv("srcdir");
  if(srcdir != NULL && (strlen(srcdir) + strlen("simple.txt") + 2) <= NAMEBUF_SIZE)
  {  
    strcpy(namebuf, srcdir);
    strcat(namebuf, "/fixedBuoy1.txt");
  }
  else
  {
    strcpy(namebuf, "fixedBuoy1.txt");
  }

  fp = fopen(namebuf, "r");

  n = 0;
  while(fscanf(fp, "%lf %f %f %s", &time, &rain, &temp, id) != -1)
    {
	  datbuf_3[n].Time = time;
	  datbuf_3[n].Rain = rain;
	  datbuf_3[n].Temp = temp;
	  memmove(datbuf_3[n].Id, id, sizeof(char)*strlen(id));
	  datbuf_3[n].Id[strlen(id)] = 0;
	  n++;
    }

  fclose(fp);
    
  datasize = (size_t)sizeof(CmpData_3);
  count[0] = n;

  status = HE5_PTwritelevel(PTid, 1, count, &datasize, datbuf_3);
  printf("Status returned by HE5_PTwritelevel() :   %d \n", status);

  count[0] = 1;
  status = HE5_PTwriteattr(PTid, "GlobalAttr_Integer", H5T_NATIVE_INT, count, &IntAttr);
  printf("Status returned by HE5_PTwriteattr() :    %d \n", status);

  status = HE5_PTdetach(PTid);
  printf("Status returned by HE5_PTdetach() :       %d \n", status);


  /* Write to Floating Buoy Point */
  /* ---------------------------- */
  PTid = HE5_PTattach(ptfid, "FloatBuoy Point");


  /* Write First (0th) Level */
  /* ----------------------- */

  /* Set up path if the input file is in another directory */
  srcdir = getenv("srcdir");
  if(srcdir != NULL && (strlen(srcdir) + strlen("simple.txt") + 2) <= NAMEBUF_SIZE)
  {  
    strcpy(namebuf, srcdir);
    strcat(namebuf, "/floatBuoy0.txt");
  }
  else
  {
    strcpy(namebuf, "floatBuoy0.txt");
  }

  fp = fopen(namebuf, "r");

  n = 0;
  while(fscanf(fp, "%s %d %d %s", desc, &date, &wgt, id) != -1)
    {
	  strcpy(datbuf_4[n].Label, desc);
	  datbuf_4[n].Date   = date;
	  datbuf_4[n].Weight = wgt;
	  memmove(datbuf_4[n].Id, id, sizeof(char)*strlen(id));
      datbuf_4[n].Id[strlen(id)] = 0;
	  n++;
    }
  fclose(fp);
    
  datasize = (size_t)sizeof(CmpData_4);
  count[0] = n;

  status = HE5_PTwritelevel(PTid, 0, count, &datasize, datbuf_4);
  printf("Status returned by HE5_PTwritelevel() :   %d \n", status);

  /* Write Second (1th) Level */
  /* ------------------------ */

  /* Set up path if the input file is in another directory */
  srcdir = getenv("srcdir");
  if(srcdir != NULL && (strlen(srcdir) + strlen("simple.txt") + 2) <= NAMEBUF_SIZE)
  {  
    strcpy(namebuf, srcdir);
    strcat(namebuf, "/floatBuoy1.txt");
  }
  else
  {
    strcpy(namebuf, "floatBuoy1.txt");
  }

  fp = fopen(namebuf, "r");

  n = 0;
  while(fscanf(fp, "%lf %lf %lf %f %f %s", &time, &lon, &lat, &rain, &temp, id) != -1)
    {
	  datbuf_5[n].Time = time;
	  datbuf_5[n].Lon  = lon;
	  datbuf_5[n].Lat  = lat;
	  datbuf_5[n].Rain = rain;
	  datbuf_5[n].Temp = temp;
	  memmove(datbuf_5[n].Id, id,sizeof(char)*strlen(id));
      datbuf_5[n].Id[strlen(id)] = 0;
	  n++;
    }
  fclose(fp);

  datasize = (size_t)sizeof(CmpData_5);
  count[0] = n; 
  status = HE5_PTwritelevel(PTid, 1, count, &datasize, datbuf_5);
  printf("Status returned by HE5_PTwritelevel() :   %d \n", status);
 
  count[0] = 1;
  status = HE5_PTwriteattr(PTid, "GlobalAttr", H5T_NATIVE_FLOAT, count, &flt);
  printf("Status returned by HE5_PTwriteattr() :    %d \n", status);
    
  status = HE5_PTdetach(PTid);
  printf("Status returned by HE5_PTdetach() :       %d \n", status);

  status = HE5_PTclose(ptfid);
  printf("Status returned by HE5_PTclose() :        %d \n", status);

  return 0;
}






