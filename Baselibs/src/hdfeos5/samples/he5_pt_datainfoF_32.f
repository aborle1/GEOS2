      program       he5_pt_datainfoF_32

      implicit      none

      include       'hdfeos5.inc'

      integer       status
      integer       ptfid 
      integer       ptid       
      integer       he5_ptopen
      integer       he5_ptattach
      integer       he5_ptinqdatatype
      integer       he5_ptdetach 
      integer       he5_ptclose
      integer       dtype
      integer       classid
      integer       order
      integer       fieldgroup

      integer*4     size

      character*1   null_char_0
      character*80  fieldname
      character*80  attrname

      null_char_0  = '0'

c     Open the HDF point file, "point.he5"
c     ------------------------------------
      ptfid = he5_ptopen('point.he5',HE5F_ACC_RDONLY)
      write(*,*) 'File ID returned by he5_ptopen():  ',ptfid

c     Read Simple Point 
c     ----------------- 
      ptid = he5_ptattach(ptfid, "FixedBuoy Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

      fieldgroup = HE5_HDFE_DATAGROUP
      fieldname  = 'Observations'

      status = he5_ptinqdatatype(ptid,fieldname,null_char_0,fieldgroup,
     1dtype,classid,order,size)
      print *,'Status returned from he5_ptinqdatatype():  ',status
      print *,'datatype:     ',dtype
      print *,'class ID:     ',classid
      print *,'order:        ',order
      print *,'size:         ',size


      fieldgroup   = HE5_HDFE_ATTRGROUP
      attrname     = 'GlobalAttribute_int'

      status = he5_ptinqdatatype(ptid,null_char_0,attrname,fieldgroup,
     1dtype,classid,order,size)
      print *,'Status returned from he5_ptinqdatatype():  ',status
      print *,'datatype:     ',dtype
      print *,'class ID:     ',classid
      print *,'order:        ',order
      print *,'size:         ',size


      fieldgroup   = HE5_HDFE_GRPATTRGROUP
      attrname     = 'GroupAttribute'

      status = he5_ptinqdatatype(ptid,null_char_0,attrname,fieldgroup,
     1dtype,classid,order,size)
      print *,'Status returned from he5_ptinqdatatype():  ',status
      print *,'datatype:     ',dtype
      print *,'class ID:     ',classid
      print *,'order:        ',order
      print *,'size:         ',size


      fieldname    = 'Observations'
      fieldgroup   = HE5_HDFE_LOCATTRGROUP
      attrname     = 'LocalAttribute'

      status = he5_ptinqdatatype(ptid,fieldname,attrname,fieldgroup,
     1dtype,classid,order,size)
      print *,'Status returned from he5_ptinqdatatype():  ',status
      print *,'datatype:     ',dtype
      print *,'class ID:     ',classid
      print *,'order:        ',order
      print *,'size:         ',size


c     Close out the point interface
c     -----------------------------
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status

      status = he5_ptclose(ptfid)
      write(*,*) 'Status returned by he5_ptclose():  ',status

      stop
      end


 


