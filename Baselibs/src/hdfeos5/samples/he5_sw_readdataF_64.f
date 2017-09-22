c     In this program we (1) open the "swath.he5" file, (2) attach to
c     the "Swath1" swath, and (3) read data from the "Longitude" field
c     ================================================================

      program            he5_sw_readdataF_64

      implicit           none

      include            'hdfeos5.inc'

      integer            status
      integer            he5_swopen
      integer            he5_swattach
      integer            he5_swrdfld
      integer            he5_swrdattr
      integer            he5_prread
      integer            he5_swdetach
      integer            he5_swclose
      integer            swfid, swid
      integer            buffer_out(250)
      integer            i,j,j1
      integer            element1(25)
      integer            element2(50)
      integer            element3(75)
      integer            element4(100)

      real*4             lng(10,20)

      integer*4          attr(4)

      integer*8          start(2)
      integer*8          stride(2)
      integer*8          count(2)
      integer*8          len(4)
      character*4        charfield(10)

      integer            FAIL
      parameter          (FAIL=-1)

    
c     Open HDF-EOS swath file, "swath.he5"
c     ------------------------------------
	  swfid = he5_swopen("swath.he5",HE5F_ACC_RDONLY)
      write(*,*) 'File ID returned by he5_swopen():  ',swfid

	  if (swfid .NE. FAIL) then
		 swid = he5_swattach(swfid, "Swath1")
         write(*,*) 'Swath ID returned by he5_swattach():  ',swid

		 if (swid .NE. FAIL) then
			
	      
c     Read the entire Longitude field
c     -------------------------------
			start(1)  = 0
			start(2)  = 0
			stride(1) = 1
			stride(2) = 1
			count(1)  = 10
			count(2)  = 20
            
			status = he5_swrdfld(swid,"Longitude",
     1           start,stride,count,lng)

			do i=1,20
			   do j=1,10
				  write(*,*)'i j Longitude ',i,j,lng(j,i)
			   enddo
			enddo

                        start(1)  = 0
                        stride(1) = 1
                        count(1)  = 10
                        status = he5_swrdfld(swid,"test_string",
     1           start,stride,count,charfield)

                        do i=1,10
                                  write(*,*)'i = ',i,charfield(i)
                        enddo

c     Read data from the Profile
c     --------------------------
			start(1)  = 0
			stride(1) = 1
			count(1)  = 4
 
      status = he5_prread(swid,"Profile-2000",start,stride,count,
     1len,buffer_out)
      write(*,*) '   '
      write(*,*) 'Status returned from he5_prread:  ',status

c     Display the Profile data
c     ------------------------
      do i=1,4
         write(*,*) 'len(',i,'):  ',len(i)
      enddo

      write(*,*) '   '
      write(*,*) 'buffer_out:  '
      write(*,*)  buffer_out
      write(*,*) '   '

      j = 0
      do i=1,25
        element1(i) = buffer_out(i)      
        j = j + 1
      enddo  
      write(*,*) '1st element: '
      write(*,*) element1
      write(*,*) '   '

      j1 = j
      do i=1,50
        element2(i) = buffer_out(j1 + i)      
        j = j + 1
      enddo  
      write(*,*) '2nd element: '
      write(*,*) element2
      write(*,*) '   '

      j1 = j
      do i=1,75
        element3(i) = buffer_out(j1 + i)      
        j = j + 1
      enddo  
      write(*,*) '3rd element: '
      write(*,*) element3
      write(*,*) '   '

      j1 = j
      do i=1,100
        element4(i) = buffer_out(j1 + i)      
        j = j + 1
      enddo  
      write(*,*) '4th element: '
      write(*,*) element4
      write(*,*) '   '
            
c     Read Attribute
c     --------------	      
			status = he5_swrdattr(swid, "TestAttr", attr)
			do i=1,4
			   write(*,*) 'Attribute Element', i, ':', attr(i)
			enddo	      
			
		 endif
	  endif
      
c     Detach from swath
c     -----------------
	  status = he5_swdetach(swid)
      write(*,*) 'Status returned from he5_swdetach:  ',status
      
c     Close the file
c     --------------
	  status = he5_swclose(swfid)
      write(*,*) 'Status returned from he5_swclose:  ',status
      
	  stop
	  end









