!  In this program we (1) open the "za.he5" file, (2) attach to
!  the "ZA1" za, and (3) write data to the "Spectra" field 

      program        he5_za_writedataF_64

      implicit       none

      include        'hdfeos5.inc'

      integer        status
      integer        he5_zaopen 
      integer        he5_zaattach 
      integer        he5_zawrite
      integer        he5_zawrattr
      integer        he5_zadetach
      integer        he5_zaclose
      integer        zafid, zaid 
      integer        i, j , k

      integer*4      attr(4)
      integer*8      start(3)
      integer*8      stride(3)
      integer*8      count(3)
 
      real*8         plane(800)

      integer        FAIL
      parameter      (FAIL=-1)
      
	 
!     Open HDF-EOS file, "za.he5"
!     ---------------------------
      zafid = he5_zaopen("za.he5", HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_zaopen():  ',zafid

      if (zafid .NE. FAIL) then
		 zaid = he5_zaattach(zafid, "ZA1")
         write(*,*) 'ZA ID returned by he5_zaattach():  ',zaid

		 if (zaid .NE. FAIL) then

!     Write Spectra one plane at a time 
!     Value is 100 * track index + band index (0-based)
!     -------------------------------------------------
			start(1)  = 0
			start(2)  = 0
			count(1)  = 20
			count(2)  = 40
			count(3)  = 1
			stride(1) = 1
			stride(2) = 1
			stride(3) = 1
	    
			do i=1,15
			   start(3) = i - 1
			   do j=1,40
				  do k=1,20
					 plane((j-1)*20+k) = (j-1)*100 + i-1
				  enddo
			   enddo
			   status = he5_zawrite(zaid,"Spectra",start,
     1              stride,count,plane)
			enddo
	write(*,*) 'Status returned by he5_zawrite(): ',status

			
!     Write User defined Attribute
!     ----------------------------
			attr(1)  = 3
			attr(2)  = 5
			attr(3)  = 7
			attr(4)  = 11
            count(1) = 4
			status = he5_zawrattr(zaid,"TestAttr",HE5T_NATIVE_INT,
     1           count,attr)
	      
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









