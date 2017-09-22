c     In this program we (1) open the "swath.he5" file, (2) attach to
c     the "Swath1" swath, and (3) write data to the "Longitude", 
c     "Longitude" and "Spectra" fields

      program        he5_sw_writedataF_32
      
      implicit       none

      include        'hdfeos5.inc'

      integer        status
      integer        he5_swopen 
      integer        he5_swattach 
      integer        he5_swwrfld
      integer        he5_swwrattr
      integer        he5_swwrlattr
      integer        he5_swwrgattr
      integer        he5_ehwrglatt
      integer        he5_swsetalias
      integer        he5_swdetach
      integer        he5_swclose
      integer        he5_prwrite
      integer        swfid, swid 
      integer        buffer(250)
      integer        counter
      integer        i, j, k 
      integer        itrack

      integer*4      attr(4)
      integer*4      attr0(4)
      integer*4      track
      integer*4      start(3)
      integer*4      stride(3)
      integer*4      count(3)
      integer*4      len(4)
      integer*4      datasize
      real           attr1(4)
      character*10    attr4
      character*100   attr5

      real           lng(10)
      real           lat(10)
 
      real*8         plane(800)
      real*8         tme(20)

      integer        FAIL
      parameter      (FAIL=-1)	


c     Set longitude values along the cross track
c     ------------------------------------------
      do i=1,10
		 lng(i) = i-1.0
      enddo
	 
c     Open HDF-EOS file, "swath.he5"
c     ------------------------------
      swfid = he5_swopen("swath.he5", HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_swopen():  ',swfid

      if (swfid .NE. FAIL) then
		 swid = he5_swattach(swfid, "Swath1")      
         write(*,*) 'Swath ID returned by he5_swattach():  ',swid

		 if (swid .NE. FAIL) then
            
c     Write data starting at the beginning of each cross track
c     --------------------------------------------------------
			start(1)  = 0
			stride(1) = 1
			stride(2) = 1
			count(1)  = 10
			count(2)  = 1

c     Loop through all the tracks, incrementing the track starting
c     position by one each time
c     ------------------------------------------------------------
			do track = 1,20
			   start(2) = track - 1
			   status = he5_swwrfld(swid,"Longitude",start,
     1              stride,count,lng)

			   do itrack = 1,10
				  lat(itrack) = track
			   enddo
			   status = he5_swwrfld(swid,"Latitude",start,
     1              stride,count,lat) 
			enddo
            
			do i = 1,20
			   tme(i) = 34574087.3 + 84893.2*(i-1)
			enddo
            
			start(1)  = 0
			stride(1) = 1
			count(1)  = 20
	     
			status = he5_swwrfld(swid, "Time", start, stride,
     1           count, tme)

                        start(1)  = 0
                        stride(1) = 1
                        stride(2) = 1
                        count(1)  = 10
                        count(2)  = 1

                        do track = 1,20
                           start(2) = track - 1
			status = he5_swwrfld(swid,"Temperature",start,
     1              stride,count,lng)
                        enddo

			status = he5_swsetalias(swid,"Temperature","L2gpValue")
			status = he5_swsetalias(swid,"Latitude","Lat,L")
            
c     Write Spectra one plane at a time 
c     Value is 100 * track index + band index (0-based)
c     -------------------------------------------------
			start(1)  = 0
			start(2)  = 0
			count(1)  = 20
			count(2)  = 40
			count(3)  = 1
			stride(3) = 1
	    
			do i=1,15
			   start(3) = i - 1
			   do j=1,40
				  do k=1,20
					 plane((j-1)*20+k) = (j-1)*100 + i-1
				  enddo
			   enddo
			   status = he5_swwrfld(swid,"Spectra",start,
     1              stride,count,plane)
			enddo

c     Populate data buffer and write data to the Profile Field
c     --------------------------------------------------------
      datasize = 0
      counter  = 0
      do i=1,4

         len(i)    = i*25
         datasize  = datasize + len(i)
         do j=1,(25*i)
            counter = counter + 1
            buffer(counter) = (i)*1000 + j - 1
         enddo
      enddo

      start(1)  = 0
      count(1)  = 4
      stride(1) = 1

      status = he5_prwrite(swid,"Profile-2000",start,stride,count,
     1datasize,len,buffer)
      write(*,*) 'Status returned by he5_prwrite():  ',status
		            
c     Write User defined Attribute
c     ----------------------------
			attr(1) = 3
			attr(2) = 5
			attr(3) = 7
			attr(4) = 11
            count(1) = 4
			status = he5_swwrattr(swid,"TestAttr",HE5T_NATIVE_INT,
     1           count,attr)
	write(*,*) 'Status returned by he5_swwrattr():  ',status

            
c     Write Global Attribute
c     ----------------------
			attr4 = "ABCDEFGH"
			count(1) = 8
			status = he5_swwrattr(swid,"GLOBAL_CHAR_ATTR",HE5T_NATIVE_CHAR,
	1	count,attr4)
	write(*,*) 'Status returned by he5_swwrattr():  ',status

			attr4 = "111"
			count(1) = 3
			status = he5_swwrgattr(swid,"GLOBAL_CHAR_ATTR_1",HE5T_NATIVE_CHAR,
	1	count,attr4)
	write(*,*) 'Status returned by he5_swwrgattr():  ',status

			attr4 = "222222"
			count(1) = 6
			status = he5_ehwrglatt(swfid,"GLOBAL_CHAR_ATTR_2",HE5T_NATIVE_CHAR,
	1	count,attr4)
	write(*,*) 'Status returned by he5_ehwrglatt():  ',status

c     Write Local Attribute
c     ---------------------
                     attr0(1) = 100
                     attr0(2) = 200
                     attr0(3) = 300
                     attr0(4) = 400
               count(1) = 4
			status = he5_swwrlattr(swid,"Density","LocalAttribute_1",
     1		 HE5T_NATIVE_INT,count,attr0)
	write(*,*) 'Status returned by he5_swwrlattr():  ',status
                     
			attr5 = "abababababababababab"
			count(1) = 20
			status = he5_swwrlattr(swid,"Density","LocalAttribute_0",
	1           HE5T_NATIVE_CHAR,count,attr5)
	write(*,*) 'Status returned by he5_swwrlattr():  ',status

                 attr1(1) = 1.111111
                 attr1(2) = 2.222222
                 attr1(3) = 3.333333
                 attr1(4) = 4.444444
           count(1) = 4
			status = he5_swwrlattr(swid,"Longitude","LocalAttribute_2",
     1		HE5T_NATIVE_REAL,count,attr1)
	write(*,*) 'Status returned by he5_swwrlattr():  ',status

                 
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


















