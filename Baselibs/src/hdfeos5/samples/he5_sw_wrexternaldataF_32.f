c
c ----------------------------------------------------------------------------
c |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
c ----------------------------------------------------------------------------
c

      program            he5_sw_wrexternaldataF_32

      implicit           none
 
      include            'hdfeos5.inc'

      integer            i
      integer            status
      integer            he5_swopen
      integer            he5_swattach
      integer            he5_swwrfld
      integer            he5_ehrdwrfile
      integer            he5_swdetach
      integer            he5_swclose
      integer            swfid, swid

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

c.....Use this for writing (only) to an existing file
      integer            WRITE_EXISTING_ONLY
      parameter          (WRITE_EXISTING_ONLY=0)

c.....Use this for reading (only) from an existing file
      integer            READ_ONLY
      parameter          (READ_ONLY=1)

c.....Use this for creating and writing to a file
      integer            WRITE_NEW
      parameter          (WRITE_NEW=2)


c     Create the new data buffer
c     --------------------------
      do 10 i=1,60
         data(i) = 9999
 10   continue
      
c     Open HDF-EOS swath file, "swath.he5"
c     ------------------------------------
	  swfid = he5_swopen("swath.he5", HE5F_ACC_RDWR)
      write(*,*) 'File ID returned from he5_swopen():  ',swfid

	  if (swfid .NE. FAIL) then
		 swid = he5_swattach(swfid, "Swath1")
         write(*,*) 'Swath ID returned from he5_swattach():  ',swid

		 if (swid .NE. FAIL) then
			
c     Write new data to the external files 
c     ------------------------------------
      start(1)  = 0
      stride(1) = 1
      count(1)  = 60

      status = he5_swwrfld(swid,"ExtData",
     1     start,stride,count,data)
      write(*,*) 'Status returned from he5_swrdfld():  ',status

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









