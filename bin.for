	program bin
c
c	Conversion of output of deceler(col(B)/(92))*3657ator program (flight times, position
c	velocity, phase, hit) to flight time profiles.
c	Last modification:	sept-2007
c
c	By Dr. Dongdong Zhang
c
	IMPLICIT none
c
	integer i,n,n_write,n_mol,nbin,inunit,xbin,hitw
	integer xbin_begin,xbin_end
	integer IX,IY
	parameter(IX=100000, IY=25)
	integer n_tot(IY),n_sel(IY),n_out(IY),n_hit(IY),n_hit_last(IY)
	integer n_right(IY)
	integer j
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
	real*8 TOF_profile(100000,IY),hit(IY),idummy(IY)
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
	laserwidth = 4e-3
	ddety = 5.0e-3		! Size detection area
	ddetz = 5.0e-3
c
	binsize = (maxT-minT)/dble(nbin)
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
	    	if((hit(i).eq.hitw).or.(hit(i).eq.2)) then         
               	if((select1.gt.lim1_min.and.select1.lt.lim1_max).and.
     .              (select2.gt.lim2_min.and.select2.lt.lim2_max)) then
		 			write(500+i,*) x(i),vx(i) 
                 	write(400+i,*) y(i),vy(i) 
                 	write(300+i,*) z(i),vz(i)
c		 write(600+i,*) vy(i),vz(i)
c		 write(700+i,*) vx(i),vz(i)
c		 write(800+i,*) vx(i),vy(i)
c		 write(900+i,*) y(i),z(i)
c		 write(1000+i,*) x(i),z(i)
c		 write(1100+i,*) x(i),y(i)
               	endif
			endif
		enddo
c
c  		do i=1,n_write  
c		hit(0)=hit(1)
c	    	if(hit(i).eq.1.and.hit(i-1).eq.0) then         
c    			if((select1.gt.lim1_min.and.select1.lt.lim1_max).and.
c     .              (select2.gt.lim2_min.and.select2.lt.lim2_max)) then
c		 			write(510+i,*) x(i),vx(i) 
c             		write(410+i,*) y(i),vy(i) 
c           		write(310+i,*) z(i),vz(i)
c		 write(610+i,*) vy(i),vz(i)
c		 write(710+i,*) vx(i),vz(i)
c		 write(810+i,*) vx(i),vy(i)
c		 write(910+i,*) y(i),z(i)
c		 write(1010+i,*) x(i),z(i)
c		 write(1110+i,*) x(i),y(i)
c              	endif
c       	endif
c     	enddo
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
  		minV1 = 1230
	  	maxV1 = 1270
	  	binsizeV1 = (maxV1-minV1)/dfloat(nbin)
c
     	minV2 = -10
	  	maxV2 = 10
	  	binsizeV2 = (maxV2-minV2)/dfloat(nbin)         
c
	  	do i=1,n_write
	    	xbin=dint((vx(i)-minV1+(binsizeV1/2))/binsizeV1)
	   		if(xbin.ge.1.and.xbin.le.nbin) then
	     		if(hit(i).eq.hitw) then
		 			if((select1.gt.lim1_min.and.select1.lt.lim1_max).and.
     .              (select2.gt.lim2_min.and.select2.lt.lim2_max).and.
     .              (abs(y(i)).lt.ddety.and.abs(z(i)).lt.ddetz)) then
	              		V1_profile(xbin,i) = V1_profile(xbin,i)+1. 
		 			endif	    
	      		endif
	    	endif
	 	end do 	! loop over number of written output lines per mol.
c
	  	do i=1,n_write
	    	xbin=dint((vy(i)-minV2+(binsizeV2/2))/binsizeV2)
	    	if(xbin.ge.1.and.xbin.le.nbin) then
	      		if(hit(i).eq.hitw) then
		 			if((select1.gt.lim1_min.and.select1.lt.lim1_max).and.
     .              (select2.gt.lim2_min.and.select2.lt.lim2_max).and.
     .              (abs(y(i)).lt.ddety.and.abs(z(i)).lt.ddetz)) then
	              		V2_profile(xbin,i) = V2_profile(xbin,i)+1. 
		 			endif	    
	      		endif
	    	endif
	  	end do 	! loop over number of written output lines per mol.
c
c
c
	end do		! loop over number of molecules
c
c 	do i=1,n_write
c		do n=1,nbin
c       	write(100+i,*) (minV1+dfloat(n)*binsizeV1),
c     .                   V1_profile(n,i)
c  		enddo
c	enddo  
c
c 	do i=1,n_write
c	  	do n=1,nbin
c      		write(800+i,*) (minV2+dfloat(n)*binsizeV2),
c     .                   V2_profile(n,i)
c  		enddo
c	enddo  
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
