!     In this program we (1) open the "za.he5" file, (2) attach to
!     the "ZA1" za, and (3) read data from the "Spectra" field
!     ============================================================

      program            he5_za_readdataF_32

      implicit           none

      include            'hdfeos5.inc'

      integer            status
      integer            he5_zaopen
      integer            he5_zaattach
      integer            he5_zaread
      integer            he5_zardattr
      integer            he5_zadetach
      integer            he5_zaclose
      integer            zafid, zaid
      integer            i,j,k

      integer*4          attr(4)

      integer*4          start(3)
      integer*4          stride(3)
      integer*4          count(3)
      
      real*8		 plane(800)

      integer            FAIL
      parameter          (FAIL=-1)
      
!     Open HDF-EOS za file, "za.he5"
!     ------------------------------
	  zafid = he5_zaopen("za.he5",HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_zaopen():  ',zafid

	  if (zafid .NE. FAIL) then
		 zaid = he5_zaattach(zafid, "ZA1")
         write(*,*) 'ZA ID returned by he5_zaattach():  ',zaid

		 if (zaid .NE. FAIL) then
			
	      
!     Read the entire Spectra field
!     -----------------------------
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
                           status = he5_zaread(zaid,"Spectra",start,
     1              stride,count,plane)
                        enddo


!     Read Attribute
!     --------------	      
			status = he5_zardattr(zaid, "TestAttr", attr)
			do i=1,4
			   write(*,*) 'Attribute Element', i, ':', attr(i)
			enddo	      
			
		 endif
	  endif
      
!    Detach from the za
!    ------------------
	  status = he5_zadetach(zaid)
      write(*,*) 'Status returned from he5_zadetach:  ',status

!     Close the file
!     --------------
	  status = he5_zaclose(zafid)
      write(*,*) 'Status returned from he5_zaclose:  ',status

	  stop
	  end

















