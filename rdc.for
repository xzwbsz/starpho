 subroutine rdc(vdata,lens,IXM,IYM,IZM,ibegin,jbegin,kbegin,ni,nj,
     >nk,voltage,IXR,IYR,IZR)
c
c     Reducing potential array to the size needed fro calculation decelerator
c
	integer i,j,k,ibegin,jbegin,kbegin
	integer IXM,IYM,IZM,IXR,IYR,IZR
	real*8 vdata(IXM,IYM,IZM),voltage(IXR,IYR,IZR)
	logical lens(IXM,IYM,IZM)
	real*8 Vmin,Vmax
c
	Vmax=-1D40 
      	Vmin=+1D40
c
         do i=1,ni
	    do j=1,nj
	      do k=1,nk
c	        
		voltage(i,j,k)=vdata(i+ibegin,j+jbegin,k+kbegin)
c
c         Write position of electrodes
c
c		if (lens(i+ibegin,j+jbegin,k+kbegin)) then
c		  write(*,*) 'ELECTRODE AT',i+ibegin,j+jbegin,
c     .                                   k+kbegin
c		  write(*,*) 'VOLTAGE',voltage(i,j,k),i,j,k
c		endif
c
		if(voltage(i,j,k).gt.Vmax) then
		  Vmax=voltage(i,j,k)
		endif
c
		if(voltage(i,j,k).lt.Vmin) then
		  Vmin=voltage(i,j,k)
		endif
c	
	      end do
	    end do
	  end do 
c
	  write(*,*) ''
	  write(*,*) 'ARRAY REDUCED TO ',ni,nj,nk
	  write(*,*) ''
	  write(*,*) 'MAX. ',Vmax, 'V'
	  write(*,*) 'MIN. ',Vmin, 'V'     
c
	return
	end subroutine rdc(vdata,lens,IXM,IYM,IZM,ibegin,jbegin,kbegin,ni,nj,
     >nk,voltage,IXR,IYR,IZR)
c