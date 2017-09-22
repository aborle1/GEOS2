      program       he5_gd_infoF_64

      implicit      none

      include       'hdfeos5.inc'

	  integer       status
	  integer       i,n
      integer       he5_gdopen
      integer       he5_gdattach
      integer       he5_gdfldinfo
      integer       he5_gdprojinfo
      integer       he5_gdgridinfo
      integer       he5_gdinqflds
      integer       he5_gdtileinfo
      integer       he5_gddetach
      integer       he5_gdclose
      integer       gdfid
      integer       gdid1
      integer       gdid2
      integer       nflds  
      integer       rk(32)
      integer       nt(32)
      integer       spherecode
      integer       projcode
      integer       zonecode

      integer*8     he5_gddiminfo
      integer*8     he5_gdinqdims
      integer*8     he5_gdnentries
      integer*8     ndim
      integer*8     dims(32)
      integer*8     xdimsize
      integer*8     ydimsize
      integer*8     dimsize 

	  integer*8     strbufsize 
    
      real*8        upleftpt(2)
      real*8        lowrightpt(2)
      real*8        projparm(13)
	  
      character*72  dimname
      character*72  mxdimname
      character*72  fieldlist
	  
      integer       tilecode
      integer       tilerank
      integer*8     tiledims(32)

      integer       FAIL
      parameter     (FAIL=-1)

c     Open HDF-EOS "grid.he5" file
c     -----------------------------	  
      gdfid = he5_gdopen('grid.he5',HE5F_ACC_RDWR)
      write(*,*) 'File ID returned by he5_gdopen():  ',gdfid

      if (gdfid .ne. FAIL) then
		 
c     Attach to the UTM and POLAR grids
c     ---------------------------------
      gdid1 = he5_gdattach(gdfid, 'UTMGrid')
      write(*,*) 'Grid ID returned by he5_gdattach():  ',gdid1

      gdid2 = he5_gdattach(gdfid, 'PolarGrid')
      write(*,*) 'Grid ID returned by he5_gdattach():  ',gdid2

c     Inquire dimensions
c     ------------------		 
      ndim = he5_gdinqdims(gdid1, dimname, dims)
      write(*,*) 'Dimension list (UTMGrid): ',dimname
      do i=1,ndim
         write(*,*) 'dim size: ', dims(i)
      enddo
		 
      ndim = he5_gdinqdims(gdid2, dimname, dims)
      write(*,*) 'Dimension list (PolarGrid): ',dimname
      do i=1,ndim
         write(*,*) 'dim size: ', dims(i)
      enddo

c     Get the sizes of certain dimensions
c        -----------------------------------         
      dimsize = he5_gddiminfo(gdid1, 'Time')
      write(*,*) 'Size of "Time" Array: ', dimsize
		 
      dimsize = he5_gddiminfo(gdid2, 'Bands')
      write(*,*) 'Size of "Bands" Array: ', dimsize
	
c     Get grid parameters
c     -------------------
      status = he5_gdgridinfo(gdid1,xdimsize,ydimsize,
     1upleftpt,lowrightpt)
      write(*,*)'X dim size, Y dim size (UTMGrid): ',
     1xdimsize,ydimsize
	   
      write(*,*) 'Up left pt (UTMGrid): ',
     1upleftpt(1),upleftpt(2)
      write(*,*) 'Low right pt (UTMGrid): ',
     1lowrightpt(1),lowrightpt(2)
	   
      status = he5_gdgridinfo(gdid2,xdimsize,ydimsize,
     1upleftpt,lowrightpt)
      write(*,*) 'X dim size, Y dim size (PolarGrid): ',
     1xdimsize,ydimsize
		 
      write(*,*) 'Up left pt (PolarGrid): ',
     1upleftpt(1),upleftpt(2)
      write(*,*) 'Low right pt (PolarGrid): ',
     1lowrightpt(1),lowrightpt(2)

c     Get projection parameters
c     -------------------------		 
      status = he5_gdprojinfo(gdid1,projcode,zonecode,
     1     spherecode,projparm)
      write(*,*) 'projcode,zonecode (UTMGrid): ',
     1     projcode,zonecode
      write(*,*) 'spherecode (UTMGrid): ', spherecode	
	   
      status = he5_gdprojinfo(gdid2,projcode,zonecode,
     1     spherecode,projparm)
      write(*,*) 'projcode (PolarGrid): ', projcode
      write(*,*) 'spherecode (PolarGrid): ', spherecode
      do i=1,13
         write(*,*) 'Projection Parameter: ',i,projparm(i)
      enddo
         
c     Get information about fields
c     ----------------------------
      nflds = he5_gdinqflds(gdid1, fieldlist, rk, nt)
      if (nflds .ne. 0) then
         write(*,*) 'Data fields (UTMGrid): ', fieldlist
         do i=1,nflds
            write(*,*) 'rank type: ',rk(i),nt(i)
         enddo
      endif
	         
      status = he5_gdtileinfo(gdid1,"Vegetation",tilecode,tilerank,
     1         tiledims)
              write(*,*) 'Vegetation Tile Code: ', tilecode
              write(*,*) 'Vegetation Tile Rank: ', tilerank
              do i=1,tilerank
                         write(*,*) 'Tile Dimension ',i,tiledims(i)
              enddo

      nflds = he5_gdinqflds(gdid2, fieldlist, rk, nt)
      if (nflds .ne. 0) then
         write(*,*) 'Data fields (PolarGrid): ',fieldlist
         do i=1,nflds
            write(*,*) 'rank type: ',rk(i),nt(i)
         enddo
      endif	
      
      status = he5_gdfldinfo(gdid2,'Spectra',rk,dims,nt,
     1     dimname,mxdimname)
      write(*,*) 'Spectra rank dims: ',rk(1)
      write(*,*) 'Spectra dim names: ',dimname
      write(*,*) 'Spectra max. dim names: ',mxdimname
      do i=1,rk(1)
         write(*,*) 'Spectra dims: ',i,dims(i)
      enddo
         
c     Get number of grid dimensions and dim. list length
c     --------------------------------------------------
      n = he5_gdnentries(gdid1, HE5_HDFE_NENTDIM, strbufsize)
      write(*,*) 'Number of dimension entries (UTMGrid): ', n
      write(*,*) 'Length of Dimension List (UTMGrid): ', 
     1     strbufsize
	   
c     Get number of data fields and field list length
c     -----------------------------------------------
      n = he5_gdnentries(gdid1, HE5_HDFE_NENTDFLD, 
     1     strbufsize)
      write(*,*) 'Number of data fields (UTMGrid): ', n
      write(*,*) 'Length of Field List (UTMGrid): ', 
     1     strbufsize
        
      endif
	  
c     Detach from the grids
c     ---------------------
      status = he5_gddetach(gdid1)
      write(*,*) 'Status returned by he5_gddetach():  ',status

      status = he5_gddetach(gdid2)
      write(*,*) 'Status returned by he5_gddetach():  ',status     

c     Close the file
c     --------------
      status = he5_gdclose(gdfid)
      write(*,*) 'Status returned by he5_gdclose():  ',status  

      stop
      end

























































































































































