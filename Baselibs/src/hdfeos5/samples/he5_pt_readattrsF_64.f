      program       he5_pt_readattrsF_64

      implicit      none

      include       'hdfeos5.inc'

      integer       status
      integer       ptfid 
      integer       ptid
      integer       he5_ptopen
      integer       he5_ptattach
      integer       he5_ptinqattrs
      integer       he5_ptinqgrpattrs
      integer       he5_ptinqlocattrs
      integer       he5_ptattrinfo
      integer       he5_ptgrpattrinfo
      integer       he5_ptlocattrinfo
      integer       he5_ptreadattr
      integer       he5_ptreadgrpattr
      integer       he5_ptreadlocattr
      integer       he5_ptdetach 
      integer       he5_ptclose
      integer       attr
      integer       attr1(5)
      integer       attr2(5)
      integer       ntype

      integer*8     sz
      integer*8     strbufsize
      integer*8     nattr

      real*4        flt      

      character*1   null_char_0
      character*80  attrname
      character*80  levelname
      character*240 attrlist

      null_char_0  = '0'

c     Open the HDF-EOS point file, "point.he5"
c     ----------------------------------------
      ptfid = he5_ptopen('point.he5',HE5F_ACC_RDONLY)
      write(*,*) 'File ID returned by he5_ptopen():  ',ptfid

c     Read FixedBuoy Point 
c     -------------------- 
      ptid = he5_ptattach(ptfid, "FixedBuoy Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

c     Global Attributes
c     -----------------
      attrname       = 'GlobalAttribute_int'
             
      print *,'Global Attribute:   '
      status   = he5_ptreadattr(ptid,attrname,attr)
      write(*,*) 'Status returned by he5_ptreadattr():  ',status
      print *,'Attribute value:    ',attr

     
      nattr   = he5_ptinqattrs(ptid,null_char_0,strbufsize)
      print *,'Number of attributes:  ',nattr
      print *,'Size (in bytes) of attribute list:  ',strbufsize

      nattr   = he5_ptinqattrs(ptid,attrlist,strbufsize)
      print *,'Number of attributes:  ',nattr
      print *,'Attribute list:  ',attrlist

      attrname       = 'GlobalAttribute_int'

      status   = he5_ptattrinfo(ptid,attrname,ntype,sz)
      write(*,*) 'Status returned by he5_ptattrinfo():  ',status
      print *,'Data type of attribute:        ',ntype
      print *,'Number of attribute elements:  ',sz

      print *,'   '
      attrname       = 'GlobalAttribute_float'
      print *,'attrname:    ',attrname

      status   = he5_ptattrinfo(ptid,attrname,ntype,sz)
      write(*,*) 'Status returned by he5_ptattrinfo():  ',status
      print *,'Data type of attribute:        ',ntype
      print *,'Number of attribute elements:  ',sz

      status   = he5_ptreadattr(ptid,attrname,flt)
      write(*,*) 'Status returned by he5_ptreadattr():  ',status
      print *,'Attribute value:    ',flt

      attrname       = 'GroupAttribute'

c     Global Attributes
c     -----------------
      print *, 'Group Attribute:'
      status = he5_ptreadgrpattr(ptid,attrname,attr1)
      print *,'Status returned by he5_ptreadgrpattr():  ',status
      print *,'Attribute values:  ',attr1


      nattr = he5_ptinqgrpattrs(ptid,null_char_0,strbufsize)
      print *,'Number of attributes:               ',nattr
      print *,'Size (in bytes) of attribute list:  ',strbufsize
      nattr = he5_ptinqgrpattrs(ptid,attrlist,strbufsize)
      print *,'Attribute list:  ',attrlist

      attrname       = 'GroupAttribute'

      status   = he5_ptgrpattrinfo(ptid,attrname,ntype,sz)
      write(*,*) 'Status returned by he5_ptgrpattrinfo():  ',status
      print *,'Data type of attribute:        ',ntype
      print *,'Number of attribute elements:  ',sz
     
c     Local Attributes
c     ----------------
      print *, 'Local Attribute:'

      attrname       = 'LocalAttribute'
      levelname      = 'Observations'

      status = he5_ptreadlocattr(ptid,levelname,attrname,attr2)
      print *,'Status returned by he5_ptreadlocattr():  ',status
      print *,'Attribute values:  ',attr2

      nattr = he5_ptinqlocattrs(ptid,levelname,null_char_0,strbufsize)
      print *,'Status returned by he5_ptinqlocattrs():  ',status
      print *,'Number of attributes:               ',nattr
      print *,'Size (in bytes) of attribute list:  ',strbufsize

      nattr = he5_ptinqlocattrs(ptid,levelname,attrlist,strbufsize)
      print *,'Attribute list:  ',attrlist

      status = he5_ptlocattrinfo(ptid,levelname,attrname,ntype,sz)
      print *,'Data type of attribute:        ',ntype
      print *,'Number of attribute elements:  ',sz


c.....Close out the point interface
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status

      status = he5_ptclose(ptfid)
      write(*,*) 'Status returned by he5_ptclose():  ',status

      stop
      end








