      program   he5_gd_definefieldsF_64
  
      implicit  none

      include   'hdfeos5.inc'
     
	  integer   status, he5_gddeffld, he5_gdsetfill
	  integer   he5_gdattach, he5_gddetach, he5_gdclose
          integer   he5_gddeftle, he5_gddefcomp
	  integer   gdfid, gdid1, gdid2, he5_gdopen 
    
          integer   tilecode, tilerank
          integer*8 tiledims(2)
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
      status = he5_gddeffld(gdid1, "Pollution", 
     1"XDim,YDim,Time"," ", HE5T_NATIVE_FLOAT, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status
      
      status = he5_gdsetfill(gdid1,"Pollution",
     1HE5T_NATIVE_FLOAT,fillval1)
      write(*,*) 'Status returned by he5_gdsetfill():  ',status
	
                tilerank = 2
                tiledims(1) = 60
                tiledims(2) = 100
                compparm(1) = 6
 
      status = he5_gddeftle(gdid1,HE5_HDFE_TILE,tilerank,tiledims)
      write(*,*) 'Status returned by he5_gddeftle(): ',status
 
      status = he5_gddefcomp(gdid1,HE5_HDFE_COMP_DEFLATE,compparm)
      write(*,*) 'Status returned by he5_gddefcomp():  ',status

      status = he5_gddeffld(gdid1, "Vegetation",
     1"XDim,YDim"," ", HE5T_NATIVE_FLOAT, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status

c     Attach to the POLAR grid
c     ------------------------
	  gdid2 = he5_gdattach(gdfid, "PolarGrid")
      write(*,*) 'Grid ID returned by he5_gdattach():  ',gdid2

c     Define Fields
c     -------------	  
      status = he5_gddeffld(gdid2, "Temperature",
     1"XDim,YDim"," ", HE5T_NATIVE_FLOAT, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status

      status = he5_gddeffld(gdid2, "Pressure", 
     1"XDim,YDim"," ", HE5T_NATIVE_FLOAT, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status

c     Set fill value for "Pressure" field
c     -----------------------------------
      status = he5_gdsetfill(gdid2,"Pressure",
     1HE5T_NATIVE_FLOAT,fillval2)
      write(*,*) 'Status returned by he5_gdsetfill():  ',status

c     Define Fields
c     -------------	  
      status = he5_gddeffld(gdid2, "Soil Dryness", 
     1"XDim,YDim"," ", HE5T_NATIVE_FLOAT, 0)
      write(*,*) 'Status returned by he5_gddeffld():  ',status
      
      status = he5_gddeffld(gdid2, "Spectra", 
     1"XDim,YDim,Bands"," ", HE5T_NATIVE_DOUBLE, 0)
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




