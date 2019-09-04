 SUBROUTINE readpa(vdata,lens,IXM,IYM,IZM,nx,ny,nz,filename,V_scal)
c
c    reading SIMION potential array, dimensions: nx--nz (max: IXM--IXZ)
c    note : in de do-loop worden z en x verwisseld!!!
c	    de randen worden (later) niet gedifferentieerd
c
      IMPLICIT REAL*8 (a-h,o-z)
      real*8  fnew
      character*8 fchar
      equiVALENCE (fnew,fchar)
      character*128 filename
      integer IXM,IYM,IZM
      real*8 vdata(IXM,IYM,IZM),V_scal
      logical lens(IXM,IYM,IZM) 
      integer nx,ny,nz

      inunit=33
      open(unit=inunit,file=filename,status='old'
     .     ,form='UNFORMATTED',access='stream')
      inquire(unit=inunit,form=fchar)
      write (*,*) fchar
c
      write(*,*) ''
      write(*,*) 'READING FILE',filename
c
c    Read the *.PA SIMION file:
c
      call readhead(inunit,imode,isym,fmaxv,nx,ny,nz,mirror)
      do k=1,nz
 	do j=1,ny
	  do i=1,nx
	     do l=1,8 
               read(inunit,end=50,err=50) fchar(l:l)
             end do
c             call shuffle(fchar(1:1),8)
             vdata(i,j,k)=V_scal*fnew
             if (fnew.ge.fmaxv) then
               vdata(i,j,k)=V_scal*(fnew-2.0*fmaxv)
               lens(i,j,k)=.true.
             end if
           end do
	 end do
      end do
      close(inunit)
      return
50    write(*,*) 'error reading'
      end
