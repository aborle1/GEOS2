!     In this program we (1) open an HDF-EOS file, (2) create the 
!     za interface, and (3) define the za field dimensions
!     ===========================================================
      
      program    he5_za_setupF_32

      implicit   none

      include    'hdfeos5.inc'

      integer    status
      integer    he5_zaopen
      integer    he5_zacreate
      integer    he5_zadefdim
      integer    he5_zadetach
      integer    he5_zaclose
      integer    zafid, zaid

      integer*4  dtrack, extdata


!     Open the HDF-EOS file, "za.he5" using "TRUNC" file access code 
!     --------------------------------------------------------------
      zafid = he5_zaopen('za.he5',HE5F_ACC_TRUNC)
      write(*,*) 'File ID returned by he5_zaopen():  ',zafid

!     Create the za, "ZA1", within the file 
!     -------------------------------------
      zaid = he5_zacreate(zafid, "ZA1")
      write(*,*) 'ZA ID returned by he5_zacreate():  ',zaid
    
!     Define Data dimensions
!     ----------------------
      dtrack = 20
      status = he5_zadefdim(zaid, "MyTrack1", dtrack)
      write(*,*) 'Status returned by he5_zadefdim():  ',status

      dtrack = 10
      status = he5_zadefdim(zaid, "MyTrack2", dtrack)
      write(*,*) 'Status returned by he5_zadefdim():  ',status

      dtrack = 40
      status = he5_zadefdim(zaid, "Res2tr", dtrack)
      write(*,*) 'Status returned by he5_zadefdim():  ',status

      dtrack = 20
      status = he5_zadefdim(zaid, "Res2xtr", dtrack)
      write(*,*) 'Status returned by he5_zadefdim():  ',status

      dtrack = 15
      status = he5_zadefdim(zaid, "Bands", dtrack)
      write(*,*) 'Status returned by he5_zadefdim():  ',status

      dtrack = 12
      status = he5_zadefdim(zaid, "IndxTrack", dtrack)
      write(*,*) 'Status returned by he5_zadefdim():  ',status

!     Define "External" dimension
!     ---------------------------
      extdata = 60
      status = he5_zadefdim(zaid, "ExtDim", extdata)
      write(*,*) 'Status returned by he5_zadefdim():  ',status

!     Define Unlimited (appendable) dimension
!     ---------------------------------------
      status = he5_zadefdim(zaid, "Unlim", HE5S_UNLIMITED_F)
      write(*,*) 'Status returned by he5_zadefdim():  ',status

!     Detach from the za
!     ------------------
      status = he5_zadetach(zaid)
      write(*,*) 'Status returned by he5_zadetach():  ',status

!     Close the za file
!     -----------------
      status = he5_zaclose(zafid)
      write(*,*) 'Status returned by he5_zaclose():  ',status

      stop
      end











































