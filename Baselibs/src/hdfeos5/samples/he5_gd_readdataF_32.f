      program       he5_gd_readdataF_32

      implicit      none

      include       'hdfeos5.inc'

	  integer       status
	  integer       gdfid 
      integer       gdid
	  integer       he5_gdopen
      integer       he5_gdattach
      integer       he5_gdrdfld
      integer       he5_gdrdattr
      integer       he5_gddetach 
	  integer       he5_gdclose

	  integer*4     start(2)
      integer*4     stride(2)
      integer*4     count(2)

	  real*4        f
      real*4        veg(120,200)

      integer       FAIL
      parameter     (FAIL=-1)

c     Open HDF-EOS "grid.he5" file
c     -----------------------------
	  gdfid = he5_gdopen("grid.he5",HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_gdopen():  ',gdid
	  
	  if (gdfid .ne. FAIL) then 
		 
c        Attach to the UTM grid 
c        ----------------------
		 gdid = he5_gdattach(gdfid, "UTMGrid")
         write(*,*) 'Grid ID returned by he5_gdattach():  ',gdid
		 
		 if (gdid .ne. FAIL) then			
			start(1)  = 0
			start(2)  = 0
			stride(1) = 1
			stride(2) = 1
			count(1)  = 120
			count(2)  = 200

c           Read the data from "Vegetation" field
c           -------------------------------------
			status = he5_gdrdfld(gdid,"Vegetation",
     1           start,stride,count,veg)
            write(*,*) 'Status returned by he5_gdrdfld():  ',status


c           Read global attribute "float"
c           -----------------------------	      
			status = he5_gdrdattr(gdid, "float", f)
            write(*,*) 'Status returned by he5_gdrdattr():  ',status
			write(*,*) 'global attribute value: ', f
		 endif
	  endif

c     Detach from the grid
c     --------------------	  
	  status = he5_gddetach(gdid)
      write(*,*) 'Status returned by he5_gddetach():  ',status

c     Close the file
c     --------------
	  status = he5_gdclose(gdfid)
      write(*,*) 'Status returned by he5_gdclose():  ',status
	  
	  stop
	  end




