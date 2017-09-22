      program       he5_pt_definelevelsF_32

      implicit      none

      include      'hdfeos5.inc'

      integer       status
      integer       ptfid 
      integer       ptid
      integer       he5_ptopen
      integer       he5_ptattach
      integer       he5_ptdeflevel 
      integer       he5_ptdeflinkage 
      integer       he5_ptdetach 
      integer       he5_ptclose
      integer       rank_1(3)
      integer       rank_2(5)
      integer       rank_3(4)
      integer       rank_4(4)
      integer       rank_5(6)
      integer       dtype_1(3)
      integer       dtype_2(5)
      integer       dtype_3(4)
      integer       dtype_4(4)
      integer       dtype_5(6)
      integer       array_1(3)
      integer       array_2(5)
      integer       array_3(4)
      integer       array_4(4)
      integer       array_5(6)

      integer*4     dims_1(3,1)
      integer*4     dims_2(5,1)
      integer*4     dims_3(4,1)
      integer*4     dims_4(4,1)
      integer*4     dims_5(6,1)

      character*240 fieldlist1
      character*240 fieldlist2
      character*240 fieldlist3
      character*240 fieldlist4
      character*240 fieldlist5
      character*80  levelname
      character*20  parent
      character*20  child
      character*20  linkfield

c     Open the HDF point file, "point.he5"
c     -----------------------------------
      ptfid = he5_ptopen('point.he5',HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_ptopen():  ',ptfid

c     Read Simple Point 
c     ----------------- 
      ptid = HE5_PTattach(ptfid, "Simple Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid


c     Populate input information structure 
c     ------------------------------------ 
      levelname = 'Sensor'

      rank_1(1)  = 1
      rank_1(2)  = 1
      rank_1(3)  = 1

      fieldlist1 = 'Time,Concentration,Species'

      dtype_1(1)     =  HE5T_NATIVE_DOUBLE
      dtype_1(2)     =  HE5T_NATIVE_FLOAT
      dtype_1(3)     =  HE5T_NATIVE_CHAR

      array_1(1)     =  0
      array_1(2)     =  1
      array_1(3)     =  1

      dims_1(1,1)    =  1
      dims_1(2,1)    =  4
      dims_1(3,1)    =  8

      status = he5_ptdeflevel(ptid, levelname, rank_1, fieldlist1,
     1dims_1, dtype_1, array_1)
      write(*,*) 'Status returned by he5_ptdeflevel():  ',status

c     Close out the point interface
c     -----------------------------
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status

c     Read Fixed Buoy Point 
c     --------------------- 
      ptid = HE5_PTattach(ptfid, "FixedBuoy Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

c     Populate input information structure 
c     ------------------------------------ 
      levelname = 'Desc-Loc'

      rank_2(1)  = 1
      rank_2(2)  = 1
      rank_2(3)  = 1
      rank_2(4)  = 1
      rank_2(5)  = 1

      fieldlist2 = 'Label,Longitude,Latitude,DeployDate,ID'

      dtype_2(1)     =  HE5T_NATIVE_CHAR
      dtype_2(2)     =  HE5T_NATIVE_DOUBLE
      dtype_2(3)     =  HE5T_NATIVE_DOUBLE
      dtype_2(4)     =  HE5T_NATIVE_INT
      dtype_2(5)     =  HE5T_NATIVE_CHAR

      array_2(1)  = 1
      array_2(2)  = 0
      array_2(3)  = 0
      array_2(4)  = 0
      array_2(5)  = 1

      dims_2(1,1)      =  8
      dims_2(2,1)      =  1
      dims_2(3,1)      =  1
      dims_2(4,1)      =  1
      dims_2(5,1)      =  8

      status = he5_ptdeflevel(ptid, levelname, rank_2, fieldlist2,
     1dims_2, dtype_2, array_2)
      write(*,*) 'Status returned by he5_ptdeflevel():  ',status


c     Populate input information structure 
c     ------------------------------------ 
      levelname = 'Observations'

      rank_3(1)  = 1
      rank_3(2)  = 1
      rank_3(3)  = 1
      rank_3(4)  = 1

      fieldlist3 = 'Time,Rainfall,Temperature,ID'

      dtype_3(1)  =  HE5T_NATIVE_DOUBLE
      dtype_3(2)  =  HE5T_NATIVE_FLOAT
      dtype_3(3)  =  HE5T_NATIVE_FLOAT
      dtype_3(4)  =  HE5T_NATIVE_CHAR

      array_3(1)  = 0
      array_3(2)  = 0
      array_3(3)  = 0
      array_3(4)  = 1

      dims_3(1,1) =  1
      dims_3(2,1) =  1
      dims_3(3,1) =  1
      dims_3(4,1) =  8

      status = he5_ptdeflevel(ptid, levelname, rank_3, fieldlist3,
     1dims_3, dtype_3, array_3)
      write(*,*) 'Status ID returned by he5_ptdeflevel():  ',status

      parent    = 'Desc-Loc'
      child     = 'Observations'
      linkfield = 'ID'

      status = he5_ptdeflinkage(ptid, parent, child, linkfield)
      write(*,*) 'Status ID returned by he5_ptdeflinkage():  ',status

c     Close out the point interface
c     -----------------------------
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status

c     Read Floating Buoy Point 
c     --------------------- 
      ptid = HE5_PTattach(ptfid, "FloatBuoy Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

c     Populate input information structure 
c     ------------------------------------ 
      levelname = 'Description'

      rank_4(1)  = 1
      rank_4(2)  = 1
      rank_4(3)  = 1
      rank_4(4)  = 1

      fieldlist4 = 'Label,DeployDate,Weight,ID'

      dtype_4(1)  =  HE5T_NATIVE_CHAR
      dtype_4(2)  =  HE5T_NATIVE_INT
      dtype_4(3)  =  HE5T_NATIVE_INT
      dtype_4(4)  =  HE5T_NATIVE_CHAR

      array_4(1)  = 1
      array_4(2)  = 0
      array_4(3)  = 0
      array_4(4)  = 1

      dims_4(1,1) =  8
      dims_4(2,1) =  1
      dims_4(3,1) =  1
      dims_4(4,1) =  8

      status = he5_ptdeflevel(ptid, levelname, rank_4, fieldlist4,
     1dims_4, dtype_4, array_4)
      write(*,*) 'Status returned by he5_ptdeflevel():  ',status

c     Populate input information structure 
c     ------------------------------------ 
      levelname = 'Measurements'

      rank_5(1)  = 1
      rank_5(2)  = 1
      rank_5(3)  = 1
      rank_5(4)  = 1
      rank_5(5)  = 1
      rank_5(6)  = 1

      fieldlist5 = 'Time,Longitude,Latitude,Rainfall,Temperature,ID'

      dtype_5(1)     =  HE5T_NATIVE_DOUBLE
      dtype_5(2)     =  HE5T_NATIVE_DOUBLE
      dtype_5(3)     =  HE5T_NATIVE_DOUBLE
      dtype_5(4)     =  HE5T_NATIVE_FLOAT
      dtype_5(5)     =  HE5T_NATIVE_FLOAT
      dtype_5(6)     =  HE5T_NATIVE_CHAR

      array_5(1)  = 0
      array_5(2)  = 0
      array_5(3)  = 0
      array_5(4)  = 0
      array_5(5)  = 0
      array_5(6)  = 1

      dims_5(1,1)      =  1
      dims_5(2,1)      =  1
      dims_5(3,1)      =  1
      dims_5(4,1)      =  1
      dims_5(5,1)      =  1
      dims_5(6,1)      =  8

      status = he5_ptdeflevel(ptid, levelname, rank_5, fieldlist5,
     1dims_5, dtype_5, array_5)
      write(*,*) 'Status returned by he5_ptdeflevel():  ',status

      parent    = 'Description'
      child     = 'Measurements'
      linkfield = 'ID'

      status = he5_ptdeflinkage(ptid, parent, child, linkfield)
      write(*,*) 'Status ID returned by he5_ptdeflinkage():  ',status

c     Close out the point interface
c     -----------------------------
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status

      status = he5_ptclose(ptfid)
      write(*,*) 'Status returned by he5_ptclose():  ',status

      stop
      end










