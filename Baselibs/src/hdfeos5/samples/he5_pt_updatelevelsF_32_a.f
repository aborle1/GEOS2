



      program       he5_pt_updatelevelsF_32

      integer       status
      integer       ptfid 
      integer       ptid
      integer       level
      integer       rank, datatype

      integer*4     recs(32)
      integer*4     nrec

      integer*4     dimens(2)
      integer*4     num_elements

      real*4        concentration_tt(4)
      real*4        conc(15,4)
      real*4        conc_tt(4)
      real*4        outconc(4,15)
      real*8        time(15)
      real*8        time_tt

      character*8   spc(15)
      character*8   spc_tt

      character*80  fieldname

      integer       datasize_time
      integer       datasize_conc
      integer       datasize_spc
      integer       dtype(3)

      integer       he5_ptopen
      integer       he5_ptattach
      integer       he5_ptupdatelevel 
      integer       he5_ptdetach 
      integer       he5_ptclose


c     Open the HDF-EOS point file, "point.he5"
c     ----------------------------------------
      ptfid = he5_ptopen('point.he5',HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_ptopen():  ',ptfid

c     Read Simple Point 
c     ----------------- 
      ptid = HE5_PTattach(ptfid, "Simple Point")
      write(*,*) 'Point ID returned by he5_ptattach():  ',ptid


      open(unit=1, file='simple_a.txt', status='OLD')

      n = 0
      do 10 i=1,1000
         read(1, *, end=100) time_tt, concentration_tt(1), 
     1                                concentration_tt(2), 
     2                                concentration_tt(3),
     3                                concentration_tt(4), 
     4                                spc_tt
         time(i)     = time_tt
         conc(i,1)   = concentration_tt(1)
         conc(i,2)   = concentration_tt(2)
         conc(i,3)   = concentration_tt(3)
         conc(i,4)   = concentration_tt(4)
         spc(i)      = spc_tt

         n = n + 1
   10 continue

  100 close(unit=1)


      print *,'n = ',n
      do i = 1,n
        print *,time(i)
        print *,conc(i,1),conc(i,2),conc(i,3),conc(i,4)
        print *,spc(i)
      end do

c.....specify the storage size of each atomic field
      datasize_time  = 8
      datasize_conc  = 4*4
      datasize_spc   = 8*1

      dtype(1)     =  HE5T_NATIVE_DOUBLE
      dtype(2)     =  HE5T_NATIVE_FLOAT
      dtype(3)     =  HE5T_NATIVE_CHAR


cc... One way that works
      nrec      = 0
      recs(1)   = 0

cc... Another way that works 
cc      nrec      = n
cc      do i = 1,n
cc         recs(i)   = i-1
cc      end do

      level     = 0

      fieldname = 'Concentration'

cc      conc_tt(1)   = 1.11
cc      conc_tt(2)   = 2.22
cc      conc_tt(3)   = 3.33
cc      conc_tt(4)   = 4.44


c.....Convert array to 'C' order
      dimens(1) = 15
      dimens(2) = 4
      rank      = 2
      datatype  = HE5T_NATIVE_FLOAT

      num_elements   = he5_ptfort2c(dimens, rank, datatype, conc, 
     1outconc)
      write(*,*) 'num_elements returned by he5_ptfort2c():  ',num_elements

c      status = he5_ptupdatelevel(ptid, level, fieldname, nrec,
c     1recs, datasize_conc, dtype(2), conc)
      status = he5_ptupdatelevel(ptid, level, fieldname, nrec,
     1recs, datasize_conc, dtype(2), outconc)
      write(*,*) 'Status returned by he5_ptupdatelevel():  ',status

cc      print *,nrec
cc      print *,recs

      fieldname = 'Time'

cc      time_tt   = 13131313.0

      status = he5_ptupdatelevel(ptid, level, fieldname, nrec,
     1recs, datasize_time, dtype(1), time)
      write(*,*) 'Status returned by he5_ptupdatelevel():  ',status

cc      print *,nrec
cc      print *,recs

      fieldname = 'Species'

cc      spc_tt    = 'AM'

      status = he5_ptupdatelevel(ptid, level, fieldname, nrec,
     1recs, datasize_spc, dtype(3), spc)
      write(*,*) 'Status returned by he5_ptupdatelevel():  ',status

cc      print *,nrec
cc      print *,recs



c.....Close out the point interface
      status = he5_ptdetach(ptid)
      write(*,*) 'Status returned by he5_ptdetach():  ',status


      status = he5_ptclose(ptfid)
      write(*,*) 'Status returned by he5_ptclose():  ',status


      stop
      end














