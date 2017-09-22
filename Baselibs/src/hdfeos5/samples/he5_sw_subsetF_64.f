c     In this program we (1) open the "swath.he5" HDF-EOS file
c     (2) attach to the "Swath1" swath, and (3) subset data from 
c     the "Spectra" field
c     ==========================================================

      program     he5_sw_subsetF_64

      implicit    none

      include     'hdfeos5.inc'

	  integer     status
	  integer     i
	  integer     he5_swopen
      integer     he5_swattach
	  integer     he5_swextper
	  integer     he5_swperinfo
      integer     he5_swreginfo
	  integer     he5_swdefboxreg
      integer     he5_swdeftmeper
      integer     he5_swextreg
      integer     he5_swdetach
      integer     he5_swclose
	  integer     swfid
      integer     swid
	  integer     rank
	  integer     ntype
	  integer     regionid
	  integer     periodid

	  integer*8   dims(8)
	  integer*8   size

	  real*8      cornerlon(2)
	  real*8      cornerlat(2)
	  real*8      datbuf(40,20,15)
	  real*8      tmebuf(20)
	  real*8      t1
	  real*8      t2

	  integer     FAIL
      parameter   (FAIL=-1)

c     Open HDF-EOS swath file, "swath.he5"
c     ------------------------------------
	  swfid = he5_swopen("swath.he5",HE5F_ACC_RDONLY)
      write(*,*) 'File ID returned by he5_swopen():  ',swfid

      if (swfid .NE. FAIL) then
		 swid = he5_swattach(swfid, "Swath1")
         write(*,*) 'Swath ID returned by he5_swattach():  ',swid

		 if (swid .NE. FAIL) then
			cornerlon(1) = 3.
			cornerlat(1) = 5.
			cornerlon(2) = 7.
			cornerlat(2) = 12.
            
c     Define box region 
c     -----------------
			regionid = he5_swdefboxreg(swid,cornerlon,
     1           cornerlat,HE5_HDFE_MIDPOINT)
            write(*,*) 'Region ID returned by he5_swdefboxreg():  ',
     1           regionid

			write(*,*) regionid,swid
            
			status = he5_swreginfo(swid,regionid,"Spectra",ntype,
     1           rank,dims,size)
            write(*,*) 'Status returned by he5_swreginfo():  ',
     1           status
			write(*,*) dims(1), dims(2), dims(3), rank, ntype, size
	      
c     Extract region data
c     -------------------
			status = he5_swextreg(swid,regionid,"Spectra",
     1           HE5_HDFE_INTERNAL,datbuf)
            write(*,*) 'Status returned by he5_swextreg():  ',
     1           status
            
c     Time Subsetting
c     ---------------
			t1  = 352324870.2d0
			t2  = 366098980.1d0
			periodid = he5_swdeftmeper(swid,t1,t2,HE5_HDFE_MIDPOINT)
            write(*,*) 'Period ID returned by he5_swdeftmeper():  ',
     1           periodid

			write(*,*) 'Time Subset: ', periodid,swid
            
			status = he5_swperinfo(swid,periodid,"Time",ntype,rank,
     1           dims,size)
	        write(*,*) 'Status returned by he5_swperinfo():  ',
     1           status 
            write(*,*) 'Time Subset: ', rank, dims(1), size
            
c     Extract Time data
c     -----------------
			status = he5_swextper(swid,periodid,"Time",
     1           HE5_HDFE_INTERNAL,tmebuf)
             write(*,*) 'Status returned by he5_swextper():  ',
     1           status            

			do 10 i=1,size/8
			   write(*,*) i, tmebuf(i)
 10         continue
            
		 endif

c     Detach from swath
c     -----------------		 
		 status = he5_swdetach(swid)
         write(*,*) 'Status returned by he5_swdetach():  ',status         

c     Close the file
c     --------------
		 status = he5_swclose(swfid)
         write(*,*) 'Status returned by he5_swclose():  ',status             

      endif
      
      stop
      end
	



