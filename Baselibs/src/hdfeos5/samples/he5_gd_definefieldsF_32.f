      program   he5_gd_definefldsF_32

      implicit  none

      include   'hdfeos5.inc'

      integer   status
      integer   he5_gdopen
      integer   he5_gddeffld
      integer   he5_gdsetfill
      integer   he5_gddeftle
      integer   he5_gddefcomp
      integer   he5_gdattach
      integer   he5_gddetach
      integer   he5_gdclose
      integer   gdfid, gdid1, gdid2  

      integer   tilecode, tilerank
      integer*4 tiledims(2)
      integer   compparm(1)
      
      real*4    fillval1, fillval2
	
      fillval1 = -7.0
      fillval2 = -9999.0

c     Open HDF-EOS file "grid.he5" 
c     ----------------------------	  
	  gdfid = he5_gdopen("grid.he5",HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_gdopen():  ',gdfid

c     Attach to the UTM grid
c     ----------------------
	  gdid1 = he5_gdattach(gdfid, "UTMGrid")
      write(*,*) 'Grid ID returned by he5_gdattach():  ',gdid1

c     Define Fields
c     -------------	  
      status = he5_gddeffld(gdid1,"Pollution",
     1     "XDim,YDim,Time"," ", HE5T_NATIVE_FLOAT, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status

      status = he5_gdsetfill(gdid1,"Pollution",
     1     HE5T_NATIVE_FLOAT,fillval1)
      write(*,*) 'Status returned by he5_gdsetfill():  ',status
	
                tilerank = 2
                tiledims(1) = 60
                tiledims(2) = 100
                compparm(1) = 6

      status = he5_gddeftle(gdid1,HE5_HDFE_TILE,tilerank,tiledims)
      write(*,*) 'Status returned by he5_gddeftle(): ',status

      status = he5_gddefcomp(gdid1,HE5_HDFE_COMP_DEFLATE,compparm)
      write(*,*) 'Status returned by he5_gddefcomp():  ',status

      status = he5_gddeffld(gdid1, "Vegetation", "XDim,YDim",
     1     " ", HE5T_NATIVE_FLOAT, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status


c     Attach to the POLAR grid
c     ------------------------
	  gdid2 = he5_gdattach(gdfid, "PolarGrid")
      write(*,*) 'Grid ID returned by he5_gdattach():  ',gdid2

      
c     Define Fields
c     -------------	  
      status = he5_gddeffld(gdid2, "Temperature", "XDim,YDim",
     1     " ", HE5T_NATIVE_FLOAT, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status

      status = he5_gddeffld(gdid2, "Pressure", "XDim,YDim",
     1     " ", HE5T_NATIVE_FLOAT, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status


c     Set fill value for "Pressure" field
c     -----------------------------------
      status = he5_gdsetfill(gdid2,"Pressure",
     1     HE5T_NATIVE_FLOAT,fillval2)
      write(*,*) 'Status returned by he5_gdsetfill():  ',status

c     Define Fields
c     -------------	  
      status = he5_gddeffld(gdid2, "Soil Dryness", "XDim,YDim",
     1     " ", HE5T_NATIVE_FLOAT, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status

      
      status = he5_gddeffld(gdid2, "Spectra", "XDim,YDim,Bands",
     1     " ", HE5T_NATIVE_DOUBLE, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status


c     Detach from the grids
c     ---------------------
	  status = he5_gddetach(gdid1)
      write(*,*) 'Status returned by he5_gddetach():  ',status

	  status = he5_gddetach(gdid2)
      write(*,*) 'Status returned by he5_gddetach():  ',status

      
c     Close the file
c     --------------
	  status = he5_gdclose(gdfid)
      write(*,*) 'Status returned by he5_gdclose():  ',status

	  stop
	  end
      
      





