c
c	  In this program we (1) open the "swath.he5" HDF-EOS file, 
c     (2) attach to the "Swath1" swath, and (3) define the 
c     external data field "ExtData".
c
      program     he5_sw_defexternalfldF_32

      implicit    none
 
      include     'hdfeos5.inc'

      integer     status
      integer     he5_swopen
      integer     he5_swattach
      integer     he5_swdefdfld
      integer     he5_swdetach
      integer     he5_swclose
      integer     he5_swsetxdat
      integer     swfid, swid

      integer*4   offset(3), size(3)

      integer     FAIL
      parameter   (FAIL=-1)


c     Open the HDF-EOS file, "swath.he5" using "READ/WRITE" access code
c     ----------------------------------------------------------------
      swfid = he5_swopen("swath.he5", HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_swopen():  ',swfid
      if (swfid .NE. FAIL) then

      swid = he5_swattach(swfid, "Swath1")
      write(*,*) 'Swath ID returned by he5_swattach():  ',swid
      if (swid .NE. FAIL) then

      offset(1) = 0 
      size(1)   = 40

      offset(2) = 40
      size(2)   = 80

      offset(3) = 80
      size(3)   = 120

      status = he5_swsetxdat(swid, "extf1.dat,extf2.dat,extf3.dat",
     1offset, size)
      write(*,*) 'Status returned by he5_swsetxdat():  ',status
      
      status = he5_swdefdfld(swid, "ExtData", "ExtDim",
     1" ", HE5T_NATIVE_INT, 0)
      write(*,*) 'Status returned by he5_swdefdfld():  ',status

      endif
      endif

c     Detach from the swath
c     ---------------------      
      status = he5_swdetach(swid)
      write(*,*) 'Status returned by he5_swdetach():  ',status

c     Close the file
c     --------------      
      status = he5_swclose(swfid)
      write(*,*) 'Status returned by he5_swclose():  ',status

      stop
      end





