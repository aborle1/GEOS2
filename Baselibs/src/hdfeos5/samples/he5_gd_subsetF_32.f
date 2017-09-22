c     In this example we will (1) open the "grid.he5" HDF-EOS file
c     (2) attach to the "PolarGrid" grid, and (3) subset data from 
c     the "Temperature" field.
c     ============================================================
 
      program      he5_gd_subsetF_32

      implicit     none

      include      'hdfeos5.inc'

      integer      status
      integer      gdfid
      integer      gdid
      integer      rgid
      integer      he5_gdopen
      integer      he5_gdattach
      integer      he5_gdreginfo
      integer      he5_gddefboxreg
      integer      he5_gdextreg
      integer      he5_gddetach
      integer      he5_gdclose
      integer      rk
      integer      nt

      integer*4    dims(8)
      integer*4    size

      real*8       cornerlon(2)
      real*8       cornerlat(2)
      real*8       upleft(2)
      real*8       lowright(2)

      real*4       datbuf(100*100)
	
      integer      FAIL
      parameter    (FAIL=-1)

c     Open the HDF-EOS grid file, "grid.he5"
c     -------------------------------------- 
      gdfid = he5_gdopen("grid.he5",HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_gdopen():  ',gdfid

      if (gdfid .NE. FAIL) then

c        Attach to the POLAR grid
c        ------------------------
         gdid = he5_gdattach(gdfid, "PolarGrid")
         write(*,*) 'Grid ID returned by he5_gdattach():  ',gdid

         if (gdid .NE. FAIL) then
            cornerlon(1) = 0.
            cornerlat(1) = 90.
            cornerlon(2) = 90.
            cornerlat(2) = 0.

c           Define box region
c           -----------------
            rgid   = he5_gddefboxreg(gdid,cornerlon, 
     1           cornerlat)
            write(*,*) 'Region ID returned by he5_gddefboxreg():  ',
     1           rgid


c           Get region information
c           ----------------------            
            status = he5_gdreginfo(gdid,rgid,"Temperature",
     1           nt,rk, dims, size, upleft, lowright)
            write(*,*) dims(1), dims(2), rk, nt

c           Extract region
c           --------------
            status = he5_gdextreg(gdid,rgid,"Temperature",
     1           datbuf)
            write(*,*) 'Status returned by he5_gdextreg():  ',status

         endif
         
c        Detach from the grid
c        --------------------
         status = he5_gddetach(gdid)
         write(*,*) 'Status returned by he5_gddetach():  ',status

c        Close the file
c        --------------
         status = he5_gdclose(gdfid)
         write(*,*) 'Status returned by he5_gdclose():  ',status
         
      endif
      stop
      end
	



