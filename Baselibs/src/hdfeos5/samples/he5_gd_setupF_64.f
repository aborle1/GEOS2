c     In this code we open an HDF-EOS file and create UTM, Polar 
c     Stereographic, and Geographic grids within the file
c     =========================================================

      program     he5_gd_setupF_64
 
      implicit    none

      include     'hdfeos5.inc'

      integer     status, gdfid, i 
      integer     he5_gdcreate, he5_gdopen 
      integer     he5_gddefdim, he5_gddefproj
      integer     he5_gddeforigin
      integer     he5_gddetach, he5_gdclose
      integer     gdid, gdid2, gdid3
      integer     zonecode, spherecode

      integer*8   xdim, ydim, dim

      real*8      uplft(2), lowrgt(2)
      real*8      projparm(16), he5_ehconvang

c     Open the HDF-EOS grid file, "grid.he5"
c     -------------------------------------
      gdfid = he5_gdopen('grid.he5',HE5F_ACC_TRUNC)
      write(*,*) 'File ID returned by he5_gdopen():  ',gdfid
    
c     Create UTM Grid     
c     ---------------

c --------------------------------------------------- c
c  Region is bounded by 54 E and 60 E longitudes and  c
c  20 N and 30 N latitudes. UTM Zone: 40.             c
c  Use default spheriod (Clarke 1866: spherecode = 0) c
c                                                     c
c  Grid into 120 bins along x-axis and 200 bins       c
c  along y-axis  (approx 3' by 3' bins).              c
c --------------------------------------------------- c

      zonecode   = 40
      spherecode = 0

c    Upper Left and Lower Right points in meters
c    -------------------------------------------
      uplft(1)  =   210584.50041
      uplft(2)  =  3322395.95445
      lowrgt(1) =  813931.10959
      lowrgt(2) = 2214162.53278

      xdim = 120
      ydim = 200

c     Define GCTP Projection Parameters
c     ---------------------------------
      do i=1,16
         projparm(i) = 0.d0
      enddo
    
      gdid   = he5_gdcreate(gdfid, "UTMGrid", 
     1     xdim, ydim, uplft, lowrgt)
      write(*,*) 'Grid ID returned by he5_gdcreate():  ',gdid

      status = he5_gddefproj(gdid,HE5_GCTP_UTM,
     1     zonecode,spherecode,projparm)
      write(*,*) 'Status returned by he5_gddefproj():  ',status
         
c     Define "Time" Dimension
c     -----------------------
      dim    = 10
      status = he5_gddefdim(gdid, "Time", dim)
      write(*,*) 'Status returned by he5_gddefdim():  ',status
     
c     Create polar stereographic grid 
c     -------------------------------
    
c --------------------------------------------------- c
c   Northern Hemisphere (True Scale at 90 N, 0        c
c   Longitude below pole).                            c
c   Use International 1967 spheriod (spherecode = 3)  c
c   Grid into 100 bins along x-axis and y-axis.       c
c --------------------------------------------------- c
      
      xdim = 100
      ydim = 100
      
      spherecode = 3
      
c     Set Longitude below pole & true scale in DDDMMMSSS.SSS format)
c     --------------------------------------------------------------
      projparm(5) = 0.0
      projparm(6) = 90000000.00

c     Use default boundaries for Polar Stereographic (hemisphere)
c     -----------------------------------------------------------
      uplft(1)    =  0
      uplft(2)    =  0
      lowrgt(1)   =  0
      lowrgt(2)   =  0

      zonecode   = 0
      
      gdid2  = he5_gdcreate(gdfid,"PolarGrid",
     1     xdim,ydim,uplft,lowrgt)
      write(*,*) 'Grid ID returned by he5_gdcreate():  ',gdid2

      status = he5_gddefproj(gdid2,HE5_GCTP_PS,
     1     zonecode,spherecode,projparm)
      write(*,*) 'Status returned by he5_gddefproj():  ',status

      status = he5_gddeforigin(gdid2, HE5_HDFE_GD_LR)
      write(*,*) 'Status returned by he5_gddeforigin():  ',status
            
c     Define "Bands" Dimension
c     ------------------------
      dim    = 3
      status = he5_gddefdim(gdid2, "Bands", dim)
      write(*,*) 'Status returned by he5_gddefdim():  ',status
            
c     Create geographic (linear scale) grid
c     -------------------------------------

c --------------------------------------------------- c 
c   0-15 degrees longitude, 20-30 degrees latitude    c
c --------------------------------------------------- c

      xdim = 60
      ydim = 40

      uplft(1)  = he5_ehconvAng(0.d0, HE5_HDFE_DEG_DMS)
      uplft(2)  = he5_ehconvAng(30.d0,HE5_HDFE_DEG_DMS)
      lowrgt(1) = he5_ehconvAng(15.d0,HE5_HDFE_DEG_DMS)
      lowrgt(2) = he5_ehconvAng(20.d0,HE5_HDFE_DEG_DMS)

      do i=1,16
         projparm(i) = 0.d0
      enddo
      
      zonecode   = 0
      spherecode = 0

      gdid3 =he5_gdcreate(gdfid,"GEOGrid",
     1     xdim,ydim,uplft,lowrgt)
      write(*,*) 'Grid ID returned by he5_gdcreate():  ',gdid3

      status=he5_gddefproj(gdid3,HE5_GCTP_GEO,
     1     zonecode,spherecode,projparm)
      write(*,*) 'Status returned by he5_gddefproj():  ',status

c     Detach from the created grids
c     -----------------------------
      status = he5_gddetach(gdid)
      write(*,*) 'Status returned by he5_gddetach():  ',status

      status = he5_gddetach(gdid2)
      write(*,*) 'Status returned by he5_gddetach():  ',status

      status = he5_gddetach(gdid3)
      write(*,*) 'Status returned by he5_gddetach():  ',status
          
c     Finally, close the grid file
c     ----------------------------
      status = he5_gdclose(gdfid)
      write(*,*) 'Status returned by he5_gdclose():  ',status

      stop
      end





