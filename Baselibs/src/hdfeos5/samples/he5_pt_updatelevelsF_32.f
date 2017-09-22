      program       he5_pt_updatelevelsF_32

      implicit      none

      include       'hdfeos5.inc'

      integer       status
      integer       ptfid 
      integer       ptid
      integer       level
      integer       he5_ptopen
      integer       he5_ptattach
      integer       he5_ptupdatelevel 
      integer       he5_ptdetach 
      integer       he5_ptclose
      integer       dtype(3)

      integer*4     recs(32)
      integer*4     nrec

      real*4        conc_tt(4)

      real*8        time_tt

      character*8   spc_tt
      character*80  fieldname


c     Open the HDF point file, "point.he5"
c     ------------------------------------
      ptfid = he5_ptopen('point.he5',HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_ptopen():  ',ptfid

c     Read Simple Point 
c     ----------------- 
      ptid = HE5_PTattach(ptfid, "Simple Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid

      dtype(1)     =  HE5T_NATIVE_DOUBLE
      dtype(2)     =  HE5T_NATIVE_FLOAT
      dtype(3)     =  HE5T_NATIVE_CHAR

      nrec      = 1
      recs(1)   = 0

      level     = 0

      fieldname = 'Concentration'

      conc_tt(1)   = 1.11
      conc_tt(2)   = 2.22
      conc_tt(3)   = 3.33
      conc_tt(4)   = 4.44

      status = he5_ptupdatelevel(ptid, level, fieldname, nrec,
     1recs, dtype(2), conc_tt)
      write(*,*) 'Status returned by he5_ptupdatelevel():  ',status

      fieldname = 'Time'

      time_tt   = 13131313.0

      status = he5_ptupdatelevel(ptid, level, fieldname, nrec,
     1recs, dtype(1), time_tt)
      write(*,*) 'Status returned by he5_ptupdatelevel():  ',status

      fieldname = 'Species'

      spc_tt    = 'AM'

      status = he5_ptupdatelevel(ptid, level, fieldname, nrec,
     1recs, dtype(3), spc_tt)
      write(*,*) 'Status returned by he5_ptupdatelevel():  ',status


c.....Close out the point interface
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status


      status = he5_ptclose(ptfid)
      write(*,*) 'Status returned by he5_ptclose():  ',status


      stop
      end














