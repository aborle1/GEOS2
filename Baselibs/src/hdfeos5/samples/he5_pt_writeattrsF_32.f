      program       he5_pt_writeattrsF_32

      implicit      none

      include       'hdfeos5.inc'

      integer       status
      integer       ptfid 
      integer       ptid
      integer       he5_ptopen
      integer       he5_ptattach
      integer       he5_ptwriteattr
      integer       he5_ptwritegrpattr
      integer       he5_ptwritelocattr
      integer       he5_ptdetach 
      integer       he5_ptclose
      integer       attr,attr1(5)
      integer       attr2(5)
      integer       ntype

      integer*4     fortcount(8)

      real*4        flt      

      character*80  attrname,levelname


c     Open the HDF point file, "point.he5"
c     -----------------------------------
      ptfid = he5_ptopen('point.he5',HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_ptopen():  ',ptfid

c     Read Simple Point 
c     ----------------- 
      ptid = he5_ptattach(ptfid, "FixedBuoy Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

c     Write attributes to "Fixed Buoy Point"
c     --------------------------------------
      attrname       = 'GlobalAttribute_int'
      ntype          = HE5T_NATIVE_INT
      fortcount(1)   = 1
      attr           = 9999
             
      status   = he5_ptwriteattr(ptid,attrname,ntype,fortcount,
     1attr)
      write(*,*) 'Status returned by he5_ptwriteattr():  ',status

      attrname       = 'GroupAttribute'
      ntype          = HE5T_NATIVE_INT
      fortcount(1)   = 5
      attr1(1)       = 1
      attr1(2)       = 2
      attr1(3)       = 3
      attr1(4)       = 4
      attr1(5)       = 5

      status   = he5_ptwritegrpattr(ptid,attrname,ntype,fortcount,
     1attr1)
      write(*,*) 'Status returned by he5_ptwritegrpattr():  ',status

      levelname      = 'Observations'
      attrname       = 'LocalAttribute'
      ntype          = HE5T_NATIVE_INT
      fortcount(1)   = 5
      attr2(1)       = 10
      attr2(2)       = 20
      attr2(3)       = 30
      attr2(4)       = 40
      attr2(5)       = 50

      status   = he5_ptwritelocattr(ptid,levelname,attrname,
     1ntype,fortcount,attr2)
      write(*,*) 'Status returned by he5_ptwritelocattr():  ',status

c.....Close out the point interface
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status


c     Write attributes to "Fixed Buoy Point"
c     --------------------------------------
      ptid = he5_ptattach(ptfid, "FixedBuoy Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

      attrname       = 'GlobalAttribute_float'
      ntype          = HE5T_NATIVE_FLOAT
      fortcount(1)   = 1
      flt            = -7.5

      status   = he5_ptwriteattr(ptid,attrname,
     1ntype,fortcount,flt)
      write(*,*) 'Status returned by he5_ptwriteattr():  ',status


c.....Close out the point interface
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status

      status = he5_ptclose(ptfid)
      write(*,*) 'Status returned by he5_ptclose():  ',status

      stop
      end





