c
c ----------------------------------------------------------------------------
c |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
c ----------------------------------------------------------------------------
c

      program            he5_sw_rdexternaldataF_32

      implicit           none

      include            'hdfeos5.inc'

      integer            i
      integer            status
      integer            he5_swopen
      integer            he5_swattach
      integer            he5_swrdfld
      integer            he5_swgetxdat
      integer            he5_ehrdwrfile
      integer            he5_swdetach
      integer            he5_swclose
      integer            swfid, swid
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

c.....Use this for writing (only) to an existing file
      integer            WRITE_EXISTING_ONLY
      parameter          (WRITE_EXISTING_ONLY=0)

c.....Use this for reading (only) from an existing file
      integer            READ_ONLY
      parameter          (READ_ONLY=1)

c.....Use this for creating and writing to a file
      integer            WRITE_NEW
      parameter          (WRITE_NEW=2)



c     Create the external data sets
c     -----------------------------
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

c     Open HDF-EOS swath file, "swath.he5"
c     -----------------------------------
	  swfid = he5_swopen("swath.he5", HE5F_ACC_RDWR)
      write(*,*) 'File ID returned from he5_swopen():  ',swfid

	  if (swfid .NE. FAIL) then
		 swid = he5_swattach(swfid, "Swath1")
         write(*,*) 'Swath ID returned from he5_swattach():  ',swid

		 if (swid .NE. FAIL) then
			
c     Read the external data field 
c     ----------------------------
      start(1)  = 0
      stride(1) = 1
      count(1)  = 60

      status = he5_swrdfld(swid,"ExtData",
     1     start,stride,count,dataout)
      write(*,*) 'Status returned from he5_swrdfld():  ',status


c     Display the data
c     ----------------
      do i=1,60
         write(*,*)'External data',dataout(i)
      enddo

      nmlgth = 48

c     Get the number of external files
c     --------------------------------     
      nfiles = he5_swgetxdat(swid,"ExtData",nmlgth,
     1     filelist,offset,size)

      write(*,*) 'Number of external files',nfiles
      write(*,*) 'External files:',filelist
      do i=1,nfiles
         write(*,*) 'offsets:',offset(i)
         write(*,*) 'sizes:',size(i)
      enddo

		 endif
	  endif
	
c     Detach from swath
c     -----------------
	  status = he5_swdetach(swid)
      write(*,*) 'Status returned from he5_swdetach():  ',status

c     Close the file
c     --------------
	  status = he5_swclose(swfid)
      write(*,*) 'Status returned from he5_swclose():  ',status
      
	  stop
	  end












