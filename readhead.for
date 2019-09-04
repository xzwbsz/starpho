c Subroutines:
c
c	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c	        READHEAD
c	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
      	subroutine readhead(inunit,imode,isym,fmaxv,nx,ny,nz,mirror)
      	IMPLICIT REAL*8 (a-h,o-z)
      	integer inew
      	real*8  fnew
      	character*4 ichar
      	character*8 fchar
      	equiVALENCE (inew,ichar)
      	equiVALENCE (fnew,fchar)
c
      	do j=1,4
		write(*,*) '1'
         	read(inunit,end=50,err=50) ichar(j:j)
		write(*,*) '2'
      	end do
      	imode=inew
      	do j=1,4
         	read(inunit,end=50,err=50) ichar(j:j)
      	end do
      	isym=inew
      	do j=1,8
         	read(inunit,end=50,err=50) fchar(j:j)
      	end do
      	fmaxv=fnew
      	do j=1,4
         	read(inunit,end=50,err=50) ichar(j:j)
      	end do
      	nx=inew
      	do j=1,4
         	read(inunit,end=50,err=50) ichar(j:j)
      	end do
      	ny=inew
      	do j=1,4
         	read(inunit,end=50,err=50) ichar(j:j)
      	end do
      	nz=inew
      	do j=1,4
         	read(inunit,end=50,err=50) ichar(j:j)
      	end do
      	mirror=inew
      	return
50    	write(*,*) 'error in readhead'
        stop
      	end 
 
