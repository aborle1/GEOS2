!     In this program we (1) open the "za.he5" HDF-EOS file, 
!     (2) attach to the "ZA1" za, and (3) define the za fields
!     ========================================================

      program     he5_za_definefieldsF_32

      implicit    none

      include     'hdfeos5.inc'

      integer     status
      integer     he5_zaopen
      integer     he5_zaattach
      integer     he5_zadefine
      integer     he5_zadefchunk
      integer     he5_zadefcomp
      integer     he5_zadetach
      integer     he5_zaclose
      integer     zafid, zaid

      integer     chunkrank
      integer*4   chunkdims(3)
      integer     compparm(1)

      integer     FAIL
      parameter   (FAIL=-1)

!     Open the HDF-EOS file, "za.he5" using "READ/WRITE" access code
!     --------------------------------------------------------------
      zafid = he5_zaopen("za.he5",HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_zaopen():  ',zafid 
      if (zafid .NE. FAIL) then

         zaid = he5_zaattach(zafid, "ZA1")
         write(*,*) 'ZA ID  returned by he5_zaattach():  ',zaid 
         if (zaid .NE. FAIL) then
            
!     Define Data fields
!     ------------------
            
            status = he5_zadefine(zaid, "Density", 
     1           "MyTrack1"," ", HE5T_NATIVE_FLOAT)
            write(*,*) 'Status returned by he5_zadefine():  ',status 

            status = he5_zadefine(zaid, "Temperature",
     1           "MyTrack2,MyTrack1",
     2           " ", HE5T_NATIVE_FLOAT)	      
            write(*,*) 'Status returned by he5_zadefine():  ',status 
	      
            status = he5_zadefine(zaid, "Pressure", 
     1           "Res2xtr,Res2tr",
     2           " ", HE5T_NATIVE_FLOAT)
            write(*,*) 'Status returned by he5_zadefine():  ',status 
	      
                chunkrank = 3
                chunkdims(1) = 20
                chunkdims(2) = 40
                chunkdims(3) = 15
                compparm(1) = 6
 
            status = he5_zadefchunk(zaid,chunkrank,chunkdims)
            write(*,*) 'Status returned by he5_zadefchunk(): ',status
 
            status = he5_zadefcomp(zaid,HE5_HDFE_COMP_DEFLATE,compparm)
            write(*,*) 'Status returned by he5_zadefcomp():  ',status

            status = he5_zadefine(zaid, "Spectra", 
     1           "Res2xtr,Res2tr,Bands",
     2           " ", HE5T_NATIVE_DOUBLE)	      
            write(*,*) 'Status returned by he5_zadefine():  ',status 



         endif
      endif
      
!     Detach from the za
!     ------------------      
      status = he5_zadetach(zaid)
      write(*,*) 'Status returned by he5_zadetach():  ',status 
      
!     Close the file
!     --------------      
      status = he5_zaclose(zafid)
      write(*,*) 'Status returned by he5_zaclose():  ',status 

      stop
      end





