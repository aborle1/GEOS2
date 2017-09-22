!
!   In this program we (1) open the "za.he5" HDF-EOS file, (2)
!   attach to the "ZA1" za, and (3) define the external data 
!   field "ExtData".
!

      program     he5_za_defexternalfldF_64

      implicit    none
 
      include     'hdfeos5.inc'

      integer     status
      integer     he5_zaopen
      integer     he5_zaattach
      integer     he5_zadefine
      integer     he5_zadetach
      integer     he5_zaclose
      integer     he5_zasetxdat
      integer     zafid, zaid

      integer*8   offset(3), size(3)

      integer     FAIL
      parameter   (FAIL=-1)


!     Open the HDF-EOS file, "za.he5" using "READ/WRITE" access code
!     --------------------------------------------------------------
      zafid = he5_zaopen("za.he5", HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_zaopen():  ',zafid
      if (zafid .NE. FAIL) then

      zaid = he5_zaattach(zafid, "ZA1")
      write(*,*) 'ZA ID returned by he5_zaattach():  ',zaid
      if (zaid .NE. FAIL) then

      offset(1) = 0 
      size(1)   = 40

      offset(2) = 40
      size(2)   = 80

      offset(3) = 80
      size(3)   = 120

      status = he5_zasetxdat(zaid, "extf1.dat,extf2.dat,extf3.dat",
     1offset, size)
      write(*,*) 'Status returned by he5_zasetxdat():  ',status
      
      status = he5_zadefine(zaid, "ExtData", "ExtDim",
     1" ", HE5T_NATIVE_INT)
      write(*,*) 'Status returned by he5_zadefine():  ',status

      endif
      endif

!     Detach from the za
!     ------------------      
      status = he5_zadetach(zaid)
      write(*,*) 'Status returned by he5_zadetach():  ',status

!     Close the file
!     --------------      
      status = he5_zaclose(zafid)
      write(*,*) 'Status returned by he5_zaclose():  ',status

      stop
      end









