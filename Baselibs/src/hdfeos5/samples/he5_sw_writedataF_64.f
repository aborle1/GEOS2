c  In this program we (1) open the "swath.he5" file, (2) attach to
c  the "Swath1" swath, and (3) write data to the "Longitude", 
c  "Longitude" and "Spectra" fields

      program        he5_sw_writedataF_64

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
      integer        he5_swdetach
      integer        he5_swclose
      integer        he5_prwrite
      integer        swfid, SWid 
      integer        buffer(250)
      integer        counter
      integer        i, j , k
      integer        itrack

      integer*4      attr(4)
      integer*8      track
      integer*8      start(3)
      integer*8      stride(3)
      integer*8      count(3)
      integer*8      len(4)
      integer*8      datasize
      character*10    attr4
      character*100   attr5

      character*4    charfield(10)

      real           lng(10)
      real           lat(10)
 
      real*8         plane(800)
      real*8         tme(73)

      integer        FAIL
      parameter      (FAIL=-1)
      
c     Set longitude values along the cross track
c     ------------------------------------------
      do i=1,10
		 lng(i) = i-1.0
      enddo

      charfield = (/'aaaa','b  b','cc  ','dddd','ee e',
     1              'ffff','gggg','hh  ','i   ','jjjj'/)
	 
c     Open HDF-EOS file, "swath.he5"
c     ------------------------------
      swfid = he5_swopen("swath.he5", HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_swopen():  ',swfid

      if (swfid .NE. FAIL) then
		 SWid = he5_swattach(swfid, "Swath1")
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

			do i = 1,73
			   tme(i) = 345740878.3 + 848936.2*(i-1)
			enddo
            
			start(1)  = 0
			stride(1) = 1
			count(1)  = 73
	     
			status = he5_swwrfld(swid, "Time", start, stride,
     1           count, tme)

			start(1) = 0
                        stride(1) = 1
                        count(1)  = 10
                        status = he5_swwrfld(swid, "test_string", start, 
     1           stride, count, charfield)

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
			attr(1)  = 3
			attr(2)  = 5
			attr(3)  = 7
			attr(4)  = 11
            count(1) = 4
			status = he5_swwrattr(swid,"TestAttr",HE5T_NATIVE_INT,
     1           count,attr)

c     Write Global Attribute
c     ----------------------
			attr4 = "ABCDEFGH"
			count(1) = 8
			status = he5_swwrattr(swid,"GLOBAL_CHAR_ATTR",HE5T_NATIVE_CHAR,
	1	count,attr4)

			attr4 = "111"
			count(1) = 3		
			status = he5_swwrgattr(swid,"GLOBAL_CHAR_ATTR_1",HE5T_NATIVE_CHAR,
	1	count,attr4)

			attr4 = "222222"
			count(1) = 6
			status = he5_ehwrglatt(swfid,"GLOBAL_CHAR_ATTR_2",HE5T_NATIVE_CHAR,
	1	count,attr4)

c     Write Local Attribute
c     ---------------------
			attr5 = "abababababababababab"
			count(1) = 20
			status = he5_swwrlattr(swid,"Density","LocalAttribute_0",
	1	HE5T_NATIVE_CHAR,count,attr5)

	      
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









