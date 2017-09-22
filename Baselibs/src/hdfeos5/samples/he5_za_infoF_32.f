!	  In this program we retrieve (1) information about the
!	  dimensions, (2) the za fields 

          program         he5_za_infoF_32

          implicit        none

          include         'hdfeos5.inc'

	  integer         i
          integer         status
	  integer         zafid, zaid
	  integer         he5_zaopen
	  integer         he5_zaattach
	  integer         he5_zainfo
          integer         he5_zachunkinfo
	  integer         he5_zadetach	  
	  integer         he5_zaclose
	  integer         rank(32)
	  integer         ntype(32)
	  integer         rk
	  integer         nt

	  integer*4       he5_zainqdims
          integer*4       he5_zainquire
          integer*4	  he5_zadiminfo
	  integer*4       ndims
	  integer*4       nflds
	  integer*4       dims(32)
	  integer*4       dimsize

	  character*72    dimname
          character*72    dimlist
          character*72    maxdimlst
          character*72    fieldlist

          integer         chunkrank
          integer*4       chunkdims(32)

	  integer         FAIL
	  parameter       (FAIL=-1)

      
!     Open the "za.he5" file for "read only" access
!     ---------------------------------------------
	  zafid = he5_zaopen("za.he5", HE5F_ACC_RDONLY)
      write(*,*) 'File ID returned by he5_zaopen():  ',zafid

	  if (zafid .NE. FAIL) then
	   
!     Attach the za
!     -------------
		 zaid = he5_zaattach(zafid, "ZA1")
         write(*,*) 'ZA ID returned by he5_zaattach():  ',zaid

		 if (zaid .NE. FAIL) then
		
!     Inquire Dimensions
!     ------------------			
			ndims = he5_zainqdims(zaid, dimname, dims)
			write(*,*) 'Dimension list: ', dimname
			do i = 1,ndims
			   write(*,*) 'dim size: ', dims(i)
			enddo
			write(*,*)	      
						
!     Inquire Data Fields	      
!     -------------------			
			nflds = he5_zainquire(zaid, fieldlist, rank, ntype)
			write(*,*) 'Data Fieldlist: ', fieldlist
			do i=1,nflds
			   write(*,*) 'field rank & datatype: ',rank(i),ntype(i)
			enddo
			write(*,*) 
			
!	  Get info on "MyTrack1" dim
!         --------------------------			
	      dimsize = he5_zadiminfo(zaid, "MyTrack1")
	      write(*,*) 'Size of MyTrack1: ', dimsize
	      write(*,*)
	      

!	  Get info on "Spectra" Field
!         ---------------------------		  
	      status = he5_zainfo(zaid,"Spectra",rk,dims,nt,
     1         dimlist,maxdimlst)
	      write(*,*) 'Spectra Rank: ', rk
	      write(*,*) 'Spectra NumberType: ', nt
	      write(*,*) 'Spectra Dimlist: ', dimlist
	      write(*,*) 'Spectra Max Dimlist: ', maxdimlst
	      do i=1,rk
			 write(*,*) 'Dimension ',i,dims(i)
	      enddo

              status = he5_zachunkinfo(zaid,"Spectra",chunkrank,
     1         chunkdims)
              write(*,*) 'Spectra Chunking Rank: ', chunkrank
              do i=1,chunkrank
                         write(*,*) 'Chunking Dimension ',i,chunkdims(i)
              enddo
		  
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








