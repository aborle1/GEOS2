      program       he5_pt_readdataF_32

      implicit      none

      include      'hdfeos5.inc'

      integer       status
      integer       ptfid 
      integer       ptid
      integer       he5_ptopen
      integer       he5_ptattach
      integer       he5_ptreadlevel
      integer       he5_ptlevelinfo 
      integer       he5_ptnrecs
      integer       he5_ptnlevels
      integer       he5_ptnfields
      integer       he5_ptc2fort
      integer       he5_ptinqattrs
      integer       he5_ptreadattr
      integer       he5_ptdetach 
      integer       he5_ptclose
      integer       i
      integer       nflds
      integer       level
      integer       arr_rank
      integer       datatype
      integer       dtype(3)
      integer       attr
      integer       nrecs
      integer       nlevels

      integer*4     dimens(2)
      integer*4     datasize
      integer*4     rank_tt(3)
      integer*4     offset_tt(3)
      integer*4     dtype_tt(3)
      integer*4     dim_sizes_tt(3)
      integer*4     nattr
      integer*4     strbufsize

      character*80  levelname
      character*80  fieldname(3)
      character*80  attrname
      character*240 fieldlist
      character*240 attrlist
      character*8   spec(15)

      real*4        con(4,15)
      real*4        outcon(15,4)
      real*8        time(15)


c     Open the HDF-EOS point file, "point.he5"
c     ----------------------------------------
      ptfid = he5_ptopen('point.he5',HE5F_ACC_RDONLY)
      write(*,*) 'File ID returned by he5_ptopen():  ',ptfid

c     Read Simple Point 
c     ----------------- 
      ptid = he5_ptattach(ptfid, "Simple Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

c     Get level information 
c     --------------------- 
      level = 0

      status = he5_ptlevelinfo(ptid, level, levelname, rank_tt,
     1fieldlist, dim_sizes_tt, datasize, offset_tt, dtype_tt)
      write(*,*) 'Status returned by he5_ptlevelinfo(): ',status

c     Get the number of records in level
c     ---------------------------------- 
      level = 0
      nrecs = he5_ptnrecs(ptid, level)
      print *,'Number of records in level: ', nrecs

      nlevels = he5_ptnlevels(ptid)
      print *,'Number of levels in Point data set: ', nlevels

      nflds = he5_ptnfields(ptid, level, fieldlist, strbufsize)
      print *,'Number of fields in level: ', nflds

c     Populate input information structure 
c     ------------------------------------ 
      dtype(1)     =  HE5T_NATIVE_DOUBLE
      dtype(2)     =  HE5T_NATIVE_FLOAT
      dtype(3)     =  HE5T_NATIVE_CHAR

c     Read the level data and print out
c     --------------------------------- 
      fieldname(1)  = 'Time'

      status = he5_ptreadlevel(ptid, 0, fieldname(1), 
     1dtype(1), time)
      write(*,*) 'Status returned by he5_ptreadlevel():  ',status

      write(*,*) 'time array:    '
      do i = 1,nrecs
        print *,time(i)
      end do

      fieldname(2)  = 'Concentration'

      status = he5_ptreadlevel(ptid, 0, fieldname(2),  
     1dtype(2), con)  
      write(*,*) 'Status returned by he5_ptreadlevel():  ',status

c     Convert 'C' array to Fortran order
c     ---------------------------------- 
      dimens(1) = 15
      dimens(2) = 4
      arr_rank  = 2
      datatype  = HE5T_NATIVE_FLOAT

      status  = he5_ptc2fort(dimens, arr_rank, datatype, con, 
     1outcon)
      write(*,*) 'Status returned by he5_ptc2fort():  ',status

      write(*,*) 'outcon array:    '
      do i = 1,nrecs
        print *,outcon(i,1),outcon(i,2),outcon(i,3),outcon(i,4) 
      end do

      fieldname(3)  = 'Species'

      status = he5_ptreadlevel(ptid, 0, fieldname(3), 
     1dtype(3), spec)  
      write(*,*) 'Status returned by he5_ptreadlevel():  ',status

      write(*,*) 'spec array:    '
      do i = 1,nrecs
        print *,spec(i)
      end do

c.....Close out the point interface
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status

c     Read FixedBuoy Point 
c     -------------------- 
      ptid = he5_ptattach(ptfid, "FixedBuoy Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

c     Global Attributes
c     -----------------
      attrname       = 'GlobalAttribute_int'

      print *,'   '
      print *,'Global Attribute:   '
      nattr   = he5_ptinqattrs(ptid,attrlist,strbufsize)
      print *,'Number of attributes:  ',nattr
      print *,'Attribute list:  ',attrlist
      print *,'Size (in bytes) of attribute list:  ',strbufsize

      status   = he5_ptreadattr(ptid,attrname,attr)
      write(*,*) 'Status returned by he5_ptreadattr():  ',status
      print *,'Attribute value:    ',attr

c.....Close out the point interface
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status

      status = he5_ptclose(ptfid)
      write(*,*) 'Status returned by he5_ptclose():  ',status


      stop
      end






