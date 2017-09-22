      program       he5_pt_writedataF_32

      implicit      none

      include       'hdfeos5.inc'

      integer       status
      integer       ptfid 
      integer       ptid
      integer       he5_ptopen
      integer       he5_ptattach
      integer       he5_ptwritelevel 
      integer       he5_ptfort2c
      integer       he5_ptwrbckptr
      integer       he5_ptwrfwdptr
      integer       he5_ptwriteattr 
      integer       he5_ptdetach 
      integer       he5_ptclose
      integer       i
      integer       rank, datatype
      integer       attr

      integer*4     n
      integer*4     count(1)
      integer*4     dimens(2)
      integer*4     fortcount(8),ntype

      real*4        flt      

      character*80  fieldname,attrname

c.....used by Simple Point
      real*8        time_tt
      real*8        time(15)
      real*4        concentration_tt(4)
      real*4        conc(15,4)
      real*4        outconc(4,15)
      character*8   spc_tt
      character*8   spc(15)

c.....used by FixedBuoy Point - Level 0
      character*8   desc_tt
      character*8   desc(3)
      real*8        lon_tt
      real*8        lon(3)
      real*8        lat_tt
      real*8        lat(3)
      integer*4     date_tt
      integer*4     date(3)
      character*8   id_tt
      character*8   id(3)

c.....used by FixedBuoy Point - Level 1
      real*8        time3_tt
      real*8        time3(20)
      real*4        rain_tt
      real*4        rain(20)
      real*4        temp_tt
      real*4        temp(20)
      character*8   id3_tt
      character*8   id3(20)

c.....used by FloatBuoy Point - Level 0
      character*8   desc4_tt
      character*8   desc4(3)
      integer*4     date4_tt
      integer*4     date4(3)
      integer*4     wgt_tt
      integer*4     wgt(3)
      character*8   id4_tt
      character*8   id4(3)

c.....used by FloatBuoy Point - Level 1
      real*8        time5_tt
      real*8        time5(25)
      real*8        lon5_tt
      real*8        lon5(25)
      real*8        lat5_tt
      real*8        lat5(25)
      real*4        rain5_tt
      real*4        rain5(25)
      real*4        temp5_tt
      real*4        temp5(25)
      character*8   id5_tt
      character*8   id5(25)


c     Open the HDF point file, "point.he5"
c     -----------------------------------
      ptfid = he5_ptopen('point.he5',HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_ptopen():  ',ptfid

c     Do Simple Point 
c     --------------- 
      ptid = HE5_PTattach(ptfid, "Simple Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid


c     Read Simple Point 
c     ----------------- 
      open(unit=1, file='simple.txt', status='OLD')

      n = 0
      do 10 i=1,1000
         read(1, 110, end=100) time_tt, concentration_tt(1), 
     1                                  concentration_tt(2), 
     2                                  concentration_tt(3),
     3                                  concentration_tt(4), 
     4                                  spc_tt
         time(i)     = time_tt
         conc(i,1)   = concentration_tt(1)
         conc(i,2)   = concentration_tt(2)
         conc(i,3)   = concentration_tt(3)
         conc(i,4)   = concentration_tt(4)
         spc(i)      = spc_tt

         n = n + 1
   10 continue

  100 close(unit=1)
  110 format(F13.1,F6.2,F6.2,F6.2,F6.2,2X,A8)

      count(1) = n

      fieldname      = 'Time'
      datatype       = HE5T_NATIVE_DOUBLE

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, time)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status
   
c.....Convert array to 'C' order
      dimens(1) = 15
      dimens(2) = 4
      rank      = 2
      datatype  = HE5T_NATIVE_FLOAT

      status    = he5_ptfort2c(dimens, rank, datatype, conc, 
     1outconc)
      write(*,*) 'Status returned by he5_ptfort2c():  ',status

      fieldname      = 'Concentration'

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, outconc)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status


      fieldname      = 'Species'
      datatype       = HE5T_NATIVE_CHAR

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, spc)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

c.....Close out the point interface
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status

c     Do FixedBuoy Point 
c     ------------------ 
      ptid = HE5_PTattach(ptfid, "FixedBuoy Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

c     Read FixedBuoy Point 
c     -------------------- 
      open(unit=1, file='fixedBuoy0.txt', status='OLD')

      n = 0
      do 20 i=1,1000
         read(1, 210, end=200) desc_tt, lon_tt, lat_tt, date_tt, 
     1id_tt
         desc(i)     = desc_tt
         lon(i)      = lon_tt
         lat(i)      = lat_tt
         date(i)     = date_tt
         id(i)       = id_tt

         n = n + 1
   20 continue

  200 close(unit=1)
  210 format(A8,F13.7,F13.7,I7,1X,A8)

      count(1) = n

      fieldname      = 'Label'
      datatype       = HE5T_NATIVE_CHAR

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, desc)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'Longitude'
      datatype       = HE5T_NATIVE_DOUBLE

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, lon)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'Latitude'
      datatype       = HE5T_NATIVE_DOUBLE

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, lat)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'DeployDate'
      datatype       = HE5T_NATIVE_INT

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, date)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'ID'
      datatype       = HE5T_NATIVE_CHAR

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, id)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

c     Read FixedBuoy Point - Level 1
c     ------------------------------ 
      open(unit=1, file='fixedBuoy1.txt', status='OLD')

      n = 0
      do 30 i=1,1000
         read(1, 310, end=300) time3_tt, rain_tt, temp_tt, id3_tt 
         time3(i)     = time3_tt
         rain(i)      = rain_tt
         temp(i)      = temp_tt
         id3(i)       = id3_tt

         n = n + 1
   30 continue

  300 close(unit=1)
  310 format(F13.2,F8.1,F8.2,3X,A8)

      count(1) = n

      fieldname      = 'Time'
      datatype       = HE5T_NATIVE_DOUBLE

      status = he5_ptwritelevel(ptid, 1, count, fieldname, 
     1datatype, time3)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'Rainfall'
      datatype       = HE5T_NATIVE_FLOAT

      status = he5_ptwritelevel(ptid, 1, count, fieldname, 
     1datatype, rain)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'Temperature'
      datatype       = HE5T_NATIVE_FLOAT

      status = he5_ptwritelevel(ptid, 1, count, fieldname, 
     1datatype, temp)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'ID'
      datatype       = HE5T_NATIVE_CHAR

      status = he5_ptwritelevel(ptid, 1, count, fieldname, 
     1datatype, id3)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

c.....Write forward and backward pointers
      status = he5_ptwrbckptr(ptid,1)
      write(*,*) 'Status returned by he5_ptwrbckptr():  ',status

      status = he5_ptwrfwdptr(ptid,1)
      write(*,*) 'Status returned by he5_ptwrfwdptr():  ',status

c     Write attributes to "Fixed Buoy Point"
c     --------------------------------------
      attrname       = 'GlobalAttribute_int'
      ntype          = HE5T_NATIVE_INT
      fortcount(1)   = 1
      attr           = 9999 

      status   = he5_ptwriteattr(ptid,attrname,ntype,fortcount,
     1attr)
      write(*,*) 'Status returned by he5_ptwriteattr():  ',status

c.....Close out the point interface
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status


c     Do FloatBuoy Point 
c     ------------------ 
      ptid = HE5_PTattach(ptfid, "FloatBuoy Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

c     Read FloatBuoy Point - Level 0
c     ------------------------------ 
      open(unit=1, file='floatBuoy0.txt', status='OLD')

      n = 0
      do 40 i=1,1000
         read(1, 410, end=400) desc4_tt, date4_tt, wgt_tt, id4_tt 
         desc4(i)     = desc4_tt
         date4(i)     = date4_tt
         wgt(i)       = wgt_tt
         id4(i)       = id4_tt

         n = n + 1
   40 continue

  400 close(unit=1)
  410 format(A8,I8,I7,2X,A8)

      count(1) = n

      fieldname      = 'Label'
      datatype       = HE5T_NATIVE_CHAR

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, desc4)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'DeployDate'
      datatype       = HE5T_NATIVE_INT

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, date4)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'Weight'
      datatype       = HE5T_NATIVE_INT

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, wgt)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'ID'
      datatype       = HE5T_NATIVE_CHAR

      status = he5_ptwritelevel(ptid, 0, count, fieldname, 
     1datatype, id4)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

c     Read FixedBuoy Point - Level 1
c     ------------------------------ 
      open(unit=1, file='floatBuoy1.txt', status='OLD')

      n = 0
      do 50 i=1,1000
         read(1, 510, end=500) time5_tt, lon5_tt, lat5_tt,  
     1rain5_tt,temp5_tt,id5_tt
         time5(i)     = time5_tt
         lon5(i)      = lon5_tt
         lat5(i)      = lat5_tt
         rain5(i)     = rain5_tt
         temp5(i)     = temp5_tt
         id5(i)       = id5_tt

         n = n + 1
   50 continue

  500 close(unit=1)
  510 format(F13.1,F13.6,F13.6,F8.1,F8.2,3X,A8)

      count(1) = n

      fieldname      = 'Time'
      datatype       = HE5T_NATIVE_DOUBLE

      status = he5_ptwritelevel(ptid, 1, count, fieldname, 
     1datatype, time5)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'Longitude'
      datatype       = HE5T_NATIVE_DOUBLE

      status = he5_ptwritelevel(ptid, 1, count, fieldname, 
     1datatype, lon5)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'Latitude'
      datatype       = HE5T_NATIVE_DOUBLE

      status = he5_ptwritelevel(ptid, 1, count, fieldname, 
     1datatype, lat5)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'Rainfall'
      datatype       = HE5T_NATIVE_FLOAT

      status = he5_ptwritelevel(ptid, 1, count, fieldname, 
     1datatype, rain5)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'Temperature'
      datatype       = HE5T_NATIVE_FLOAT

      status = he5_ptwritelevel(ptid, 1, count, fieldname, 
     1datatype, temp5)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

      fieldname      = 'ID'
      datatype       = HE5T_NATIVE_CHAR

      status = he5_ptwritelevel(ptid, 1, count, fieldname, 
     1datatype, id5)
      write(*,*) 'Status returned by he5_ptwritelevel():  ',status

c.....Write forward and backward pointers
      status = he5_ptwrbckptr(ptid,1)
      write(*,*) 'Status returned by he5_ptwrbckptr():  ',status

      status = he5_ptwrfwdptr(ptid,1)
      write(*,*) 'Status returned by he5_ptwrfwdptr():  ',status

c     Write attributes to "Float Buoy Point"
c     --------------------------------------
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



