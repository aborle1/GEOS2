!
! ----------------------------------------------------------------------------
! |    Copyright (C) 2002   Emergent IT Inc. and Raytheon Systems Company    |
! ----------------------------------------------------------------------------
!

      program            he5_za_wrexternaldataF_32

      implicit           none
 
      include            'hdfeos5.inc'

      integer            i
      integer            status
      integer            he5_zaopen
      integer            he5_zaattach
      integer            he5_zawrite
      integer            he5_ehrdwrfile
      integer            he5_zadetach
      integer            he5_zaclose
      integer            zafid, zaid

      integer            data_out_1(100)
      integer            data_out_2(100)
      integer            data_out_3(100)
      integer            data(60)

      integer*4          start(2)
      integer*4          stride(2)
      integer*4          count(2)
      integer*4          num_elements

      integer            FAIL
      parameter          (FAIL=-1)

!.....Use this for writing (only) to an existing file
      integer            WRITE_EXISTING_ONLY
      parameter          (WRITE_EXISTING_ONLY=0)

!.....Use this for reading (only) from an existing file
      integer            READ_ONLY
      parameter          (READ_ONLY=1)

!.....Use this for creating and writing to a file
      integer            WRITE_NEW
      parameter          (WRITE_NEW=2)


!     Create the new data buffer
!     --------------------------
      do 10 i=1,60
         data(i) = 9999
 10   continue
      
!     Open HDF-EOS za file, "za.he5"
!     ------------------------------
	  zafid = he5_zaopen("za.he5", HE5F_ACC_RDWR)
      write(*,*) 'File ID returned from he5_zaopen():  ',zafid

	  if (zafid .NE. FAIL) then
		 zaid = he5_zaattach(zafid, "ZA1")
         write(*,*) 'ZA ID returned from he5_zaattach():  ',zaid

		 if (zaid .NE. FAIL) then
			
!     Write new data to the external files 
!     ------------------------------------
      start(1)  = 0
      stride(1) = 1
      count(1)  = 60

      status = he5_zawrite(zaid,"ExtData",
     1     start,stride,count,data)
      write(*,*) 'Status returned from he5_zawrite():  ',status

      num_elements = 100

      status = he5_ehrdwrfile("extf1.dat",READ_ONLY,
     1HE5T_NATIVE_INT,num_elements,data_out_1)
      write(*,*) 'Status returned from he5_ehrdwrfile:   ',status

      status = he5_ehrdwrfile("extf2.dat",READ_ONLY,
     1HE5T_NATIVE_INT,num_elements,data_out_2)
      write(*,*) 'Status returned from he5_ehrdwrfile:   ',status

      status = he5_ehrdwrfile("extf3.dat",READ_ONLY,
     1HE5T_NATIVE_INT,num_elements,data_out_3)
      write(*,*) 'Status returned from he5_ehrdwrfile:   ',status

      write(*,*)'External file extf1.dat:'
      write(*,*) data_out_1
      write(*,*) '  '

      write(*,*)'External file extf2.dat:'
      write(*,*) data_out_2
      write(*,*) '  '

      write(*,*)'External file extf3.dat:'
      write(*,*) data_out_3
      write(*,*) '  '

		 endif
	  endif
	
!     Detach from za
!     --------------
	  status = he5_zadetach(zaid)
      write(*,*) 'Status returned from he5_zadetach():  ',status

!     Close the file
!     --------------
	  status = he5_zaclose(zafid)
      write(*,*) 'Status returned from he5_zaclose():  ',status
      
	  stop
	  end









