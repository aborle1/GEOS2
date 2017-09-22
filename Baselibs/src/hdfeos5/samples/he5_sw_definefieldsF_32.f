c	  In this program we (1) open the "swath.he5" HDF-EOS file, (2)
c	  attach to the "Swath1" swath, and (3) define the swath fields
c     =============================================================

      program     he5_sw_definefieldsF_32

      implicit    none

      include     'hdfeos5.inc'

      integer     status
      integer     he5_swopen
      integer     he5_swattach
      integer     he5_swsetfill
      integer     he5_swdefgfld
      integer     he5_swdefdfld
      integer     he5_prdefine
      integer     he5_swdefchunk
      integer     he5_swdefcomp
      integer     he5_swdetach
      integer     he5_swclose
      integer     swfid, swid

      integer     chunkrank
      integer*4   chunkdims(3) 
      integer     compparm(1)

      integer     FAIL
      parameter   (FAIL=-1)

c     Open the HDF-EOS file, "swath.he5" using "READ/WRITE" access code
c     -----------------------------------------------------------------
      swfid = he5_swopen("swath.he5",HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_swopen():  ',swfid 
      if (swfid .NE. FAIL) then

         swid = he5_swattach(swfid, "Swath1")
         write(*,*) 'Swath ID  returned by he5_swattach():  ',swid 
         if (swid .NE. FAIL) then
            
c     Define Geolocation and Data fields
c     ----------------------------------
c     ---------------------------------------------------------------
c     We define six fields.  The first three, "Time", "Longitude"
c	  and "Latitude" are geolocation fields and thus we use the
c	  geolocation dimensions "GeoTrack" and "GeoXtrack" in the field
c	  definitions.  We also must specify the data type using the
c	  standard HDF data type codes.  In this example the geolocation
c	  are 4-byte (32 bit) floating point numbers.
c     
c	  The next three fields are data fields.  Note that either
c	  geolocation or data dimensions can be used. 
c     ---------------------------------------------------------------
            
            status = he5_swdefgfld(swid, "Time", "GeoTrack", 
     1           " ",HE5T_NATIVE_DOUBLE, 0)
            write(*,*) 'Status returned by he5_swdefgfld():  ',status 

            status = he5_swdefgfld(swid, "Longitude", 
     1           "GeoXtrack,GeoTrack",
     2           " ", HE5T_NATIVE_FLOAT, 0)
            write(*,*) 'Status returned by he5_swdefgfld():  ',status 

            status = he5_swdefgfld(swid, "Latitude",
     1           "GeoXtrack,GeoTrack",
     2           " ", HE5T_NATIVE_FLOAT, 0)
            write(*,*) 'Status returned by he5_swdefgfld():  ',status 

            status = he5_swdefdfld(swid, "Density",
     1           "GeoTrack"," ", HE5T_NATIVE_FLOAT, 0)
            write(*,*) 'Status returned by he5_swdefdfld():  ',status

            status = he5_swdefdfld(swid, "Temperature",
     1           "GeoXtrack,GeoTrack",
     2           " ", HE5T_NATIVE_FLOAT, 0)	      
            write(*,*) 'Status returned by he5_swdefdfld():  ',status 
	      
            status = he5_swdefdfld(swid, "Pressure", 
     1           "Res2xtr,Res2tr",
     2           " ", HE5T_NATIVE_FLOAT, 0)
            write(*,*) 'Status returned by he5_swdefdfld():  ',status 
	      
		chunkrank = 3
                chunkdims(1) = 20
                chunkdims(2) = 40
                chunkdims(3) = 15
                compparm(1) = 6

            status = he5_swdefchunk(swid,chunkrank,chunkdims)
            write(*,*) 'Status returned by he5_swdefchunk(): ',status

            status = he5_swdefcomp(swid,HE5_HDFE_COMP_DEFLATE,compparm)
            write(*,*) 'Status returned by he5_swdefcomp():  ',status

            status = he5_swdefdfld(swid, "Spectra", 
     1           "Res2xtr,Res2tr,Bands",
     2           " ", HE5T_NATIVE_DOUBLE, 0)	      
            write(*,*) 'Status returned by he5_swdefdfld():  ',status 


c     Define Profile Field
c     -----------------------  
            status = he5_prdefine(swid, "Profile-2000", 
     1           "ProfDim", " ", HE5T_NATIVE_INT)
            write(*,*) 'Status returned by he5_prdefine():  ',status


c     Define Appendable Field
c     -----------------------  
c     status = he5_swdefdfld(swid, "Count", "GeoTrack",
c     1"Unlim", HE5T_NATIVE_INT, 0)	      
            
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





