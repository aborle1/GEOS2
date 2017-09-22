!
! ----------------------------------------------------------------------------
! |    Copyright (C) 2002   Emergent IT Inc. and Raytheon Systems Company    |
! ----------------------------------------------------------------------------
!

      program            he5_za_rdexternaldataF_32

      implicit           none

      include            'hdfeos5.inc'

      integer            i
      integer            status
      integer            he5_zaopen
      integer            he5_zaattach
      integer            he5_zaread
      integer            he5_zagetxdat
      integer            he5_ehrdwrfile
      integer            he5_zadetach
      integer            he5_zaclose
      integer            zafid, zaid
      integer            nfiles
      integer            datain1(100)
      integer            datain2(100)
      integer            datain3(100)
      integer            dataout(60)

      integer*4          start(2)
      integer*4          stride(2)
      integer*4          count(2)
      integer*4          offset(3)
      integer*4          size(3)
      integer*4          nmlgth
      integer*4          num_elements

      character          filelist*256

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



!     Create the external data sets
!     -----------------------------
      do 10 i=1,100

         datain1(i) = 1000+i
         datain2(i) = 2000+i
         datain3(i) = 3000+i        
         
 10   continue

      num_elements = 100
      
      status = he5_ehrdwrfile("extf1.dat",WRITE_NEW,
     1HE5T_NATIVE_INT,num_elements,datain1)
      write(*,*) 'Status returned from he5_ehrdwrfile:   ',status

      status = he5_ehrdwrfile("extf2.dat",WRITE_NEW,
     1HE5T_NATIVE_INT,num_elements,datain2)
      write(*,*) 'Status returned from he5_ehrdwrfile:   ',status

      status = he5_ehrdwrfile("extf3.dat",WRITE_NEW,
     1HE5T_NATIVE_INT,num_elements,datain3)
      write(*,*) 'Status returned from he5_ehrdwrfile:   ',status

!     Open HDF-EOS za file, "za.he5"
!     ------------------------------
	  zafid = he5_zaopen("za.he5", HE5F_ACC_RDWR)
      write(*,*) 'File ID returned from he5_zaopen():  ',zafid

	  if (zafid .NE. FAIL) then
		 zaid = he5_zaattach(zafid, "ZA1")
         write(*,*) 'ZA ID returned from he5_zaattach():  ',zaid

		 if (zaid .NE. FAIL) then
			
!     Read the external data field 
!     ----------------------------
      start(1)  = 0
      stride(1) = 1
      count(1)  = 60

      status = he5_zaread(zaid,"ExtData",
     1     start,stride,count,dataout)
      write(*,*) 'Status returned from he5_zaread():  ',status


!     Display the data
!     ----------------
      do i=1,60
         write(*,*)'External data',dataout(i)
      enddo

      nmlgth = 48

!     Get the number of external files
!     --------------------------------     
      nfiles = he5_zagetxdat(zaid,"ExtData",nmlgth,
     1     filelist,offset,size)

      write(*,*) 'Number of external files',nfiles
      write(*,*) 'External files:',filelist
      do i=1,nfiles
         write(*,*) 'offsets:',offset(i)
         write(*,*) 'sizes:',size(i)
      enddo

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












