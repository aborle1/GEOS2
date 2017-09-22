c	In this program we retrieve (1) information about the
c	dimensions, (2) the dimension mappings (geolocation relations), 
c	and (3) the swath fields 

      program         he5_sw_infoF_64

      implicit        none

      include         'hdfeos5.inc'

	  integer         i
          integer         status
	  integer         swfid, swid
	  integer         he5_swopen
	  integer         he5_swattach
	  integer         he5_swfldinfo
          integer         he5_swattrinfo
	  integer         he5_swmapinfo
          integer         he5_swchunkinfo
	  integer         he5_swdetach	  
	  integer         he5_swclose
	  integer         rank(32)
	  integer         ntype(32)
	  integer         rk
          integer         nt

	  integer*8       he5_swinqdims
	  integer*8       he5_swinqmaps
          integer*8       he5_swinqgflds
          integer*8       he5_swinqdflds
	  integer*8       he5_swdiminfo
	  integer*8       he5_swimapinfo
	  integer*8       he5_swinqimaps
          integer*8       he5_swinqgfldalias
	  integer*8       offset
	  integer*8       incr
	  integer*8       ndims
	  integer*8       nmaps
	  integer*8       nflds
	  integer*8       dims(32)
	  integer*8       off(32)
	  integer*8       inc(32)
	  integer*8       sizes(8)
	  integer*8       indx(32)
	  integer*8       dimsize
          integer*8       n
          integer*8       count

	  character*72    dimname
	  character*72    dimmap
          character*72    dimlist
          character*72    maxdimlst
          character*72    fieldlist

          integer*8       nfldalias
          integer*8       strbufsize
 
          character*72    fldaliases

          integer         chunkrank
          integer*8       chunkdims(32)

	  
	  integer         FAIL
	  parameter       (FAIL=-1)

c     Open the "swath.he5" file for "read only" access
c     ------------------------------------------------
	  swfid = he5_swopen("swath.he5",HE5F_ACC_RDONLY)
      write(*,*) 'File ID returned by he5_swopen():  ',swfid

	  if (swfid .NE. FAIL) then
	   
c     Attach the swath
c     ----------------
		 swid = he5_swattach(swfid, "Swath1")
         write(*,*) 'Swath ID returned by he5_swattach():  ',swid

		 if (swid .NE. FAIL) then
		
c     Inquire Dimensions
c     ------------------			
			ndims = he5_swinqdims(swid, dimname, dims)
			write(*,*) 'Dimension list: ', dimname
			do i = 1,ndims
			   write(*,*) 'dim size: ', dims(i)
			enddo
			write(*,*)	      
            
c     Inquire Dimension Mappings
c     --------------------------
			nmaps = he5_swinqmaps(swid, dimmap, off, inc)
			write(*,*) 'Dimension map: ', dimmap
			do i = 1,nmaps
			   write(*,*) 'offset increment: ', off(i), inc(i)
			enddo
			write(*,*)

c     Inquire Indexed Dimension Mappings
c     ----------------------------------
			nmaps = he5_swinqimaps(swid, dimmap, sizes)
			write(*,*) 'Index Dimension map: ', dimmap
			do i=1,nmaps
			   write(*,*) 'sizes: ', sizes(i)
			enddo
			write(*,*)	      
			
c     Inquire Geolocation Fields
c     --------------------------
			nflds = he5_swinqgflds(swid, fieldlist, rank, ntype)
			write(*,*) 'Geolocation fieldlist: ', fieldlist
			do i=1,nflds
			   write(*,*) 'field rank & datatype: ',rank(i),ntype(i)
			enddo
			write(*,*) 
	      
c     Inquire Data Fields	      
c     -------------------			
			nflds = he5_swinqdflds(swid, fieldlist, rank, ntype)
			write(*,*) 'Data Fieldlist: ', fieldlist
			do i=1,nflds
			   write(*,*) 'field rank & datatype: ',rank(i),ntype(i)
			enddo
			write(*,*) 

        nfldalias = he5_swinqgfldalias(swid,fldaliases,strbufsize)
        write(*,*) 'Fields & aliases list: ',fldaliases
        write(*,*) 'Length of list is ',strbufsize

			
c     Get info on "GeoTrack" dim
c     --------------------------			
	      dimsize = he5_swdiminfo(swid, "GeoTrack")
	      write(*,*) 'Size of GeoTrack: ', dimsize
	      write(*,*)
	      

c     Get info on "GeoTrack/Res2tr" mapping
c     -------------------------------------		  
	      status = he5_swmapinfo(swid,"GeoTrack","Res2tr",
     1         offset,incr)	      
	      write(*,*) 'Mapping Offset: ', offset
	      write(*,*) 'Mapping Increment: ', incr
	      write(*,*)


c     Get info on "IndxTrack/Res2tr" indexed mapping
c     ----------------------------------------------		  
	      n = he5_swimapinfo(swid, "IndxTrack", "Res2tr", indx)
	      do i=1,n
			 write(*,*) 'Index Mapping Entry ', i, indx(i)
	      enddo
	      write(*,*)
	      
c	  Get info on "Longitude" Field
c         -----------------------------		  
	      status = he5_swfldinfo(swid,"Longitude",rk,dims,nt,
     1         dimlist,maxdimlst)
	      write(*,*) 'Longitude Rank: ', rk
	      write(*,*) 'Longitude NumberType: ', nt
	      write(*,*) 'Longitude Dimlist: ', dimlist
	      write(*,*) 'Longitude Max Dimlist: ', maxdimlst
	      do i=1,rk
			 write(*,*) 'Dimension ',i,dims(i)
	      enddo

              status = he5_swfldinfo(swid,"test_string",rk,dims,nt,
     1         dimlist,maxdimlst)
              write(*,*) 'test_string: Rank: ', rk
              write(*,*) 'test_string: NumberType: ', nt
              write(*,*) 'test_string: Dimlist: ', dimlist
              write(*,*) 'test_string: Max Dimlist: ', maxdimlst
              do i=1,rk
                         write(*,*) 'Dimension ',i,dims(i)
              enddo

			status = he5_swattrinfo(swid,"GLOBAL_CHAR_ATTR",nt,count)
		write(*,*) 'GLOBAL_CHAR_ATTR NumberType: ', nt
		write(*,*) 'GLOBAL_CHAR_ATTR size: ', count

              status = he5_swchunkinfo(swid,"Spectra",chunkrank,
     1         chunkdims)
              write(*,*) 'Spectra Chunking Rank: ', chunkrank
              do i=1,chunkrank
                         write(*,*) 'Chunking Dimension ',i,chunkdims(i)
              enddo

		  
	   endif
	  endif
	  
c     Detach from swath
c     -----------------
	  status = he5_swdetach(swid)
      write(*,*) 'Status returned by he5_swdetach():  ',status

c     Close the file
c     --------------
	  status = he5_swclose(swfid)
      write(*,*) 'Status returned by he5_swclose():  ',status

	  stop
	  end








