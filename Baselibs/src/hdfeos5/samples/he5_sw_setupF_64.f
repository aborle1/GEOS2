c     In this program we (1) open an HDF-EOS file, (2) create the 
c     swath interface, and (3) define the swath field dimensions
c     ===========================================================

      program            he5_sw_setupF_64

      implicit           none

      include            'hdfeos5.inc'
      include            'hdfeos5_64.inc'

      integer            status
      integer            he5_swopen
      integer            he5_swcreate
      integer            he5_swdefdim
      integer            he5_swdefmap
      integer            he5_swdefimap
      integer            he5_swdetach
      integer            he5_swclose
      integer            swfid, swid

      integer*8          dtrack, extdata
      integer*8          offset, incr
      integer*8          indx(12)

      data indx  /0,1,3,6,7,8,11,12,14,24,32,39/

c     Open the HDF-EOS file, "swath.he5" using "TRUNC" file access code 
c     -----------------------------------------------------------------
      swfid = he5_swopen("swath.he5",HE5F_ACC_TRUNC)
      write(*,*) 'File ID returned by he5_swopen():  ',swfid

c     Create the swath, "Swath1", within the file 
c     -------------------------------------------
      swid = he5_swcreate(swfid, "Swath1")
      write(*,*) 'Swath ID returned by he5_swcreate():  ',swid
    
c     Define Geolocation and Data dimensions
c     --------------------------------------
c     ----------------------------------------------------------------------
c     Typically, many fields within a swath share the same dimension. The
c     swath interface therefore provides a way of defining dimensions that
c     will then be used to define swath fields.  A dimension is defined with
c     a name and a size and is connected to the particular swath through the
c     swath id.  In this example, we define the geo- location track and
c     cross track dimensions with size 20 and 10 respectively and two
c     dimensions corresponding to these but with twice the resolution.
c     Also, we define "Bands" and "unlimited" dimensions.
c     ----------------------------------------------------------------------
      dtrack = 73
      status = he5_swdefdim(swid, "GeoTrack", dtrack)
      write(*,*) 'Status returned by he5_swdefdim():  ',status

      dtrack = 10
      status = he5_swdefdim(swid, "GeoXtrack", dtrack)
      write(*,*) 'Status returned by he5_swdefdim():  ',status

      dtrack = 40
      status = he5_swdefdim(swid, "Res2tr", dtrack)
      write(*,*) 'Status returned by he5_swdefdim():  ',status

      dtrack = 20
      status = he5_swdefdim(swid, "Res2xtr", dtrack)
      write(*,*) 'Status returned by he5_swdefdim():  ',status

      dtrack = 15
      status = he5_swdefdim(swid, "Bands", dtrack)
      write(*,*) 'Status returned by he5_swdefdim():  ',status

      dtrack = 12
      status = he5_swdefdim(swid, "IndxTrack", dtrack)
      write(*,*) 'Status returned by he5_swdefdim():  ',status

      dtrack = 4
      status = he5_swdefdim(swid, "ProfDim", dtrack)
      write(*,*) 'Status returned by he5_swdefdim():  ',status
      
c     Define "External" dimension
c     ---------------------------
      extdata = 60
      status = he5_swdefdim(swid, "ExtDim", extdata)
      write(*,*) 'Status returned by he5_swdefdim():  ',status
     
c     Define Unlimited (appendable) dimension
c     ---------------------------------------
      status = he5_swdefdim(swid, "Unlim", HE5S_UNLIMITED_F_64)
      write(*,*) 'Status returned by he5_swdefdim():  ',status

c     ----------------------------------------------------------------------
c     Once the dimensions are defined, the relationship (mapping)between the
c     geolocation dimensions, such as track and cross track, and the data
c     dimensions, must be established.  This is done through the SWdefdimmap
c     routine.  It takes as input the swath id, the names of the dimensions
c     designating the geolocation and data dimensions, respectively, and the
c     offset and increment defining the relation.
c     
c     In the first example we relate the "GeoTrack" and "Res2tr" dimensions
c     with an offset of 0 and an increment of 2.  Thus the ith element of
c     "Geotrack" corresponds to the 2 * ith element of "Res2tr".
c   
c     In the second example, the ith element of "GeoXtrack" corresponds to 
c     the 2 * ith + 1 element of "Res2xtr".
c     -----------------------------------------------------------------------  
      
c     Define dimension mappings
c     -------------------------
      offset = 0
      incr   = 2
      status = he5_swdefmap(swid, "GeoTrack", "Res2tr",
     1offset, incr)
      write(*,*) 'Status returned by he5_swdefmap():  ',status

      offset = 1
      status = he5_swdefmap(swid, "GeoXtrack", "Res2xtr", 
     1offset, incr)
      write(*,*) 'Status returned by he5_swdefmap():  ',status


c     Define indexed dimension mapping
c     --------------------------------
      status = he5_swdefimap(swid, "IndxTrack", "Res2tr", 
     1indx)
      write(*,*) 'Status returned by he5_swdefimap():  ',status

c     Detach from the swath
c     ---------------------
      status = he5_swdetach(swid)
      write(*,*) 'Status returned by he5_swdetach():  ',status

c     Close the swath file
c     --------------------
      status = he5_swclose(swfid)
      write(*,*) 'Status returned by he5_swclose():  ',status

      stop
      end








