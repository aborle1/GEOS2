      program     he5_pt_setupF_64

      implicit    none

      include     'hdfeos5.inc'

      integer     status 
      integer     ptfid 
      integer     ptid1
      integer     ptid2
      integer     ptid3

      integer     he5_ptopen
      integer     he5_ptcreate
      integer     he5_ptdetach
      integer     he5_ptclose

c --------------------------------------------------------------------------- c
c     We first open the HDF-EOS point file, "point.he5". Because this file    c
c     does not already exist, we use the "TRUNC" file access code in the      c
c     open statement.  The PTopen routine returns the point file id, ptfid,   c
c     which is used to identify the file in subsequent routines in the        c
c     library.                                                                c
c --------------------------------------------------------------------------- c

c     Open the HDF-EOS point file, "point.he5"
c     ----------------------------------------
      ptfid = he5_ptopen('point.he5',HE5F_ACC_TRUNC)
      write(*,*) 'File ID returned by he5_ptopen():  ',ptfid
     
      ptid1 = he5_ptcreate(ptfid, "Simple Point")
      write(*,*) 'Point ID returned by he5_ptcreate():   ',ptid1
      ptid2 = he5_ptcreate(ptfid, "FixedBuoy Point")
      write(*,*) 'Point ID returned by he5_ptcreate():   ',ptid2
      ptid3 = he5_ptcreate(ptfid, "FloatBuoy Point")
      write(*,*) 'Point ID returned by he5_ptcreate():   ',ptid3

c --------------------------------------------------------------------------- c
c     We now close the point interface with the he5_ptdetach routine.         c
c     This step is necessary to properly store the point information within   c
c     the file.                                                               c
c --------------------------------------------------------------------------- c

      status = he5_ptdetach(ptid1)
      write(*,*) 'Status returned by he5_ptdetach():  ',status
      status = he5_ptdetach(ptid2)
      write(*,*) 'Status returned by he5_ptdetach():  ',status
      status = he5_ptdetach(ptid3)
      write(*,*) 'Status returned by he5_ptdetach():  ',status

      status = he5_ptclose(ptfid)
      write(*,*) 'Status returned by he5_ptclose():  ',status

      stop
      end








