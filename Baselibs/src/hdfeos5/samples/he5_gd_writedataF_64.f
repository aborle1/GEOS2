      program     he5_gd_writedataF_64
 
      implicit    none

      include     'hdfeos5.inc'

	  integer     i, j, status
      integer     he5_gdopen
      integer     he5_gdattach
      integer     he5_gdwrfld
	  integer     he5_gdwrattr
      integer     he5_gdwrgattr
      integer     he5_ehwrglatt
      integer     he5_gdwrlattr
      integer     he5_gddetach
      integer     he5_gdclose
	  integer     gdfid, gdid

	  integer*8   start(2),stride(2),count(2)

	  real*4      f,veg(120,200),temp(100,100)
      character*10    attr4
      character*100   attr5

      integer     FAIL
      parameter   (FAIL=-1)


c     Create data buffers 
c     -------------------      
	  do i=1,200
		 do j=1,120
			veg(j,i) = 10 + i
		 enddo
	  enddo
	  
	  do i=1,100
		 do j=1,100
			temp(j,i) = 100*(i-1) + j
		 enddo
	  enddo

c     Open HDF-EOS file "grid.he5" 
c     ---------------------------      
	  gdfid = he5_gdopen("grid.he5",HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_gdopen():  ',gdfid

	  if (gdfid .ne. FAIL) then

c        Attach to the UTM grid
c        ----------------------         
		 gdid = he5_gdattach(gdfid, "UTMGrid")
         write(*,*) 'Grid ID returned by he5_gdattach():  ',gdid

		 if (gdid .ne. FAIL) then
			start(1)  = 0
			start(2)  = 0
			stride(1) = 1
			stride(2) = 1
			count(1)  = 120
			count(2)  = 200

c           Write data to the field "Vegetation"
c           ------------------------------------
			status = he5_gdwrfld(gdid,"Vegetation",
     1           start,stride,count,veg)
            write(*,*) 'Status returned by he5_gdwrfld():  ',status
      
			f        = 1
			count(1) = 1

c           Write global attribute "float"
c           ------------------------------
			status = he5_gdwrattr(gdid,"float",
     1      HE5T_NATIVE_FLOAT,count,f)
            write(*,*) 'Status returned by he5_gdwrattr():  ',status

		attr4 = "ABCDEFGH"
		count(1) = 8
		status = he5_gdwrattr(gdid,"GLOBAL_CHAR_ATTR",HE5T_NATIVE_CHAR,
	1	count,attr4)
	write(*,*) 'Status returned by he5_gdwrattr():  ',status

		attr4 = "111"
		count(1) = 3
		status = he5_gdwrgattr(gdid,"GLOBAL_CHAR_ATTR_1",HE5T_NATIVE_CHAR,
	1	count,attr4)
	write(*,*) 'Status returned by he5_gdwrgattr():  ',status

		attr4 = "222222"
		count(1) = 6
		status = he5_ehwrglatt(gdfid,"GLOBAL_CHAR_ATTR_2",HE5T_NATIVE_CHAR,
	1	count,attr4)
	write(*,*) 'Status returned by he5_ehwrglatt():  ',status

		attr5 = "abcdefgh111111111111"
		count(1) = 20
		status = he5_gdwrlattr(gdid,"Vegetation","LocalAttribute_0",
	1	HE5T_NATIVE_CHAR,count,attr5)
	write(*,*) 'Status returned by he5_gdwrlattr():  ',status

		 endif
	  endif

c     Detach from the grid
c     --------------------	  
	  status = he5_gddetach(gdid)
      write(*,*) 'Status returned by he5_gddetach():  ',status

c     Attach to the POLAR grid 
c     ------------------------	  
	  gdid = he5_gdattach(gdfid, "PolarGrid")
      write(*,*) 'Grid ID returned by he5_gdattach():  ',gdid

	  if (gdid .ne. FAIL) then
		 start(1)  = 0
		 start(2)  = 0
		 stride(1) = 1
		 stride(2) = 1
		 count(1)  = 100
		 count(2)  = 100

c        Write data to the "Temperature" field
c        -------------------------------------         
		 status = he5_gdwrfld(gdid,"Temperature",
     1        start,stride,count,temp)
         write(*,*) 'Status returned by he5_gdwrfld():  ',status

	  endif

c     Detach from the grid
c     --------------------
	  status = he5_gddetach(gdid)
      write(*,*) 'Status returned by he5_gddetach():  ',status

c     Close the file 
c     --------------	  
	  status = he5_gdclose(gdfid)
      write(*,*) 'Status returned by he5_gdclose():  ',status

	  stop
	  end
