	program bin
c
c	Conversion of output of deceler(col(B)/(92))*3657ator program (flight times, position
c	velocity, phase, hit) to flight time profiles.
c	Last modification:	sept-2007
c
c
c
	IMPLICIT none
c
	integer i,n,n_write,n_mol,nbin,inunit,xbin,hitw
        integer xbin_begin,xbin_end
	integer IX,IY
	parameter(IX=10000, IY=25)
	integer n_tot(IY),n_sel(IY),n_out(IY),n_hit(IY),n_hit_last(IY)
        integer n_right(IY)
	integer j
c
c
	real*8 minT,maxT,lim1_min,lim1_max,binsize,ddety,ddetz
	real*8 lim2_min,lim2_max,laserwidth,binsizeV1,binsizeV2
	real*8 T(IY)
	real*8 select1,select2
	real*8 tof(IY),ph(IY),x(IY),y(IY),z(IY)
	real*8 vx(IY),vy(IY),vz(IY),theta,test,pi
	real*8 dump2, dump3, dump4, dump5, dump6, dump7, dump8, dump9
c
	character*30 filename
	real*8 TOF_profile(10000,IY),hit(IY),idummy(IY)
	real*8 minV1,maxV1,V1_profile(10000,IY)
	real*8 minV2,maxV2,V2_profile(10000,IY)
c

        pi = 3.1415
c
c	Initializing parameters	
c

	minT =   0.0e-3
	maxT =   10.0e-3
c
	lim1_min = 0e-3
	lim1_max = 1000e-3
c
c	lim2_min =  0
c	lim2_max =  1000
c	lim2_max =  1000e-3
c
	lim2_min = 0.0e-3
	lim2_max = 1000.0e-3 
c
	nbin =  10000		! Number of intervals for time binning
c
	hitw =  0		! preferential hit
c
        laserwidth = 1e-3
	ddety = 5.0e-3		! Size detection area
        ddetz = 5.0e-3
c
	binsize = (maxT-minT)/dble(nbin)
c
c
	inunit=34
	filename='../output/ff.dat'
	write(*,*) ''
	write(*,*) 'READING Parameters from: ',filename
	open(unit=inunit,file=filename)
	read(inunit,*) n_mol
	read(inunit,*) n_write
	write(*,*) 'number of molecules flown= ',n_mol
	write(*,*) 'nunber of lines per molecule=', n_write
	write(*,*) 'Selection criteria:'
	write(*,*) '1.  min: ',lim1_min,' max: ',lim1_max
	write(*,*) '2.  min: ',lim2_min,' max: ',lim2_max
c
c
	do n=1,n_mol

	  if( mod(n,50000).eq.0) then
	    write(*,*) 'n_mol read ',n
	  endif
c
	  do i=1,n_write
	    read(inunit,*) idummy(i),tof(i),x(i),y(i),z(i),
     .                           vx(i),vy(i),vz(i),hit(i)
            T(i)  =  tof(i)
          enddo
c	              
	  select1    =  tof(4)
	  select2    =  x(2)
c
c
c
c
	  do i=1,n_write
	    xbin=dint((T(i)-minT+(binsize/2))/binsize)
            xbin_begin=
     .         dint((T(i)-(laserwidth/vx(i))-minT+(binsize/2))/binsize)
            xbin_end=
     .         dint((T(i)+(laserwidth/vx(i))-minT+(binsize/2))/binsize)
	    if(xbin.ge.1.and.xbin.le.nbin) then
	      if(hit(i).eq.hitw) then
		 if((select1.gt.lim1_min.and.select1.lt.lim1_max).and.
     .              (select2.gt.lim2_min.and.select2.lt.lim2_max).and.
     .              (abs(y(i)).lt.ddety.and.abs(z(i)).lt.ddetz)) then
                    do xbin=xbin_begin,xbin_end
	              TOF_profile(xbin,i) = TOF_profile(xbin,i)+1. 
                    enddo
	         n_sel(i) = n_sel(i) + 1
		 endif
c
 	         n_right(i) = n_right(i) + 1
c	    
	      endif
	    endif
	  end do 	! loop over number of written output lines per mol.
c
	end do
c
c 
c
c
	write(*,*) n_write
         do n=1,nbin
	     write(200+n_write,*) (minT+dfloat(n)*binsize)*1000,
     .                   TOF_profile(n,n_write) 
	  end do
c	
c 
c
	close(inunit)
c
	stop 'normal termination'
	end
