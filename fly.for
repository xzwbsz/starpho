	program fly
c
c       This program simulates the trajectories of molecules through a 
c       hexapole and Stark decelerator. A molecular packet with a definable
c       initial position and velocity spread is generated with a random
c       generator. 
c       
c       Last modification: Sat 02 Jun 2012 02:44:15 PM CEST 
c		By Dongdong Zhnag
c
c       Note: Changing dimensions of acceleration array must be done both in the 
c       main program AND in the subroutine F!!!
c
c
c	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c       Decleration of variables
c	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	IMPLICIT none
c	
	character*128 dummychar 
c	! used to cump some variables from the timeseq inputfile
	character*128 filename,filename1,filename2,arg 
c	! required for the input of the timeseq and burstfile
	integer iarg,iargc,icm,indx ! used in the setup-part of the program
c	The following array is used for the decelerator-part of the potential:
	integer IXMAX,IYMAX,IZMAX
	PARAMETER(IXMAX=200,IYMAX=200,IZMAX=200)
  	real*8 accx(IXMAX,IYMAX,IZMAX)
  	real*8 accy(IXMAX,IYMAX,IZMAX)
	real*8 accz(IXMAX,IYMAX,IZMAX)         
	real*8 gu		! gridunits
	integer ni,nj,nk
	integer nt,ntotal	! the number of stages
 	integer nb,ne,nd	! beginning and end of the array that is used
	common /basis/ accx,accy,accz
	common /basis/  gu
  	common /basis/ ni,nj,nk
	common /basis/ nt,ntotal
   	common /basis/ nb,ne,nd
c
	real*8 trigBU
c
	real*8 x0,vx0,deltat,trig_offset,deltavx,fact,x_offset
	real*8 finalx,finaly,finalz,finaltof
	real*8 r,theta,vr,vtheta
	real*8 deltar,deltavr,delta_transverse, delta_long
	real*8 pseudoT
	real*8 mint,maxt
	real*8 minv,maxv
	real*8 dummy
	real*8 LA,LB,LC,L1,L2,L3,L4
c
	integer n,n_mol,outunit
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c       INTEGRATION PACKAGE
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  	INTEGER NEQ,LRW,LIW
	PARAMETER (NEQ=6, LRW=33+7*NEQ, LIW=34)
	REAL*8 YSTART(NEQ)
 	REAL*8 RWORK(LRW)
  	INTEGER IWORK(LIW)
  	REAL*8  TSTART,T,TEND, TWANT
 	INTEGER INFO(15)
  	INTEGER IDID
   	REAL*8 RTOL, ATOL
	REAL*8 RPAR(10)
 	INTEGER IPAR(10)
 	EXTERNAL DDERKF
  	EXTERNAL F, HEX
	common /nico1/ TSTART,YSTART,TWANT,INFO,RTOL,
     .            ATOL,IDID,RWORK,RPAR,IPAR
c
	REAL*8 random
	EXTERNAL random
	EXTERNAL random_package
	EXTERNAL fake_distribution
	INTEGER seed
	INTEGER*4 time
c
	integer i,ii,j,k,inunit, i_start
 	common /stage/ i
	integer dummyi,dummyj,dummyk
	real*8  dummyx
c
	integer MAXTIMINGS
	PARAMETER(MAXTIMINGS=9000)	
	real*8 t2jump(MAXTIMINGS),TOFFSET,Thexapole,Thexapole_off
	integer m
	character*6 statusBU(MAXTIMINGS)
	common t2jump,m,statusBU,TOFFSET,
     .       Thexapole,Thexapole_off
c
	real*8 l_exc_dec,l_stage,W_stage,l_exc_det
	real*8 l_dec_det,l_sk_dec,r_sk,l_x0_sk,phase,l_nozzle_hex
 	real*8 l_exc_hex, L_hex
	common /dimensions/ l_exc_dec, l_exc_hex, L_hex, W_stage
c
	real*8 mu,Vhex,Lambda,mass,r_hex,B
	common /hexapole/ Vhex,r_hex
	common /molecule/ mu,mass,Lambda,B
c
	real*8 deltatof 
  	real*8 l_w,v,t_l
	integer tofoutput
	real*8 deltatof_slow
	real*8 deltatof_fast
c
	real*8 hexapole_begin
	real*8 hexapole_end
	real*8 decelerator_begin
	real*8 decelerator_end
	common /arraydimensions/ hexapole_begin,hexapole_end,
     .     decelerator_begin,decelerator_end
	real*8 tof,x,y,z,vx,vy,vz
	character*128 ptu	!query replace in potential_to_use
	character*8 hexapole
	character*8 pulse
	integer hit,p,lastline
  	common /nico0/ tof,x,y,z,vx,vy,vz,
     .                 ptu,hit
 	common /countlines/ p, lastline
	integer odd
	common /oddd/ odd   ! put in nico0
	character*500 t2s_str
	integer time2save(200), nbt2save
	common /time2saveblock/ time2save, nbt2save
	EXTERNAL INTEGRATION
	EXTERNAL checkhit
	integer now(3),start(3)
	integer myseed,iseed,hms
c
c
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c		SETUP
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
	if (iargc().eq.0) then
		write(*,*) 'Usage: fly_trap.exe -i1 "input timeseq file" '
	 	write(*,*) '                    -i2 "input BURST file" '
		stop
	else
		icm=0
  		iarg = 0
       	indx = iargc()
   		do while (iarg .lt. indx)
	  		iarg = iarg + 1 
	    	call getarg(iarg,arg)
            if (arg .eq. '-i1') then
          		iarg = iarg + 1
         		call getarg(iarg,filename1)
        	elseif (arg.eq. '-i2') then
                iarg = iarg + 1
                call getarg(iarg,filename2)
        	end if
		end do
	end if
c
	write(*,*) ''
	write(*,*) 'READING Parameters from disc'
c
	inunit=34
	write(*,*) ''
	write(*,*) 'READING: ',filename1
	open(unit=inunit,file=filename1)
	read(inunit,*) dummychar
	read(inunit,*) dummychar
	read(inunit,*) dummychar
	read(inunit,*) ni	!Dimensions acceleration array 
	read(inunit,*) nj
	read(inunit,*) nk
c 	Omit first and last data points; used only for fitting purposes:
	read(inunit,*) nb
	read(inunit,*) ne
	nd = ne-nb	
c 	Read dimensions
	read(inunit,*) gu	!#gridunits/meter
c
  	read(inunit,*) dummychar        !comment lines
  	read(inunit,*) dummychar
 	read(inunit,*) dummychar
c
  	read(inunit,*) mu          ! dipole moment molecule in Debye
	mu=mu*3.3356D-30        ! mu in Cm
  	read(inunit,*) Vhex        ! hexapole voltage in kV
 	Vhex=Vhex*1000D0        ! Vhex in V
 	read(inunit,*) Lambda      ! Lambda doublet splitting in GHz
  	read(inunit,*) r_hex       ! inner radius hexapole (m)
  	read(inunit,*) B           ! effective value of MK/J(J+1)
c
	read(inunit,*) dummychar        !comment lines
  	read(inunit,*) dummychar
  	read(inunit,*) dummychar
c
	read(inunit,*) r_sk	!radius skimmer
	read(inunit,*) l_x0_sk	!Distance center package to skimmer
	read(inunit,*) LB	!Dist. skimmer-hexapole
 	l_nozzle_hex=l_x0_sk+LB
  	l_exc_hex=l_x0_sk+LB
 	LB = l_x0_sk + LB
c
  	read(inunit,*) hexapole      !hexapole installed?
	read(inunit,*) LC	!hexapole (not used)
 	read(inunit,*) L1	!hexapole (used)
c  	L_hex = L1
  	read(inunit,*) L2	!hexapole (not used)
  	L_hex = LC+L1+L2
  	read(inunit,*) L3	!hexapole-first stage
c
	if(hexapole.eq.'yes') then
		l_exc_dec  =  LB+LC+L1+L2+L3
	else
		l_exc_dec = LB
	endif	 	
c
	l_stage = (nd/gu)	!Length of single stage
	W_stage=(nj-3)/(2*gu)	!Half the distance between opposing rods 
  	read(inunit,*) l_dec_det   !Distance decelerator to detection 
 	read(inunit,*) nt		!Number of stages (physical size)
	read(inunit,*) dummy	        !number of stages actually used		
 	read(inunit,*) dummy            !overtone number
  	ntotal=nt
	l_exc_det=l_exc_dec+(nt*l_stage)+l_dec_det
c
 	write(*,*) 'l_exc_dec:', l_exc_dec
 	write(*,*) 'l_dec_det:', l_dec_det
 	write(*,*) 'length of single stage:', l_stage
	write(*,*) 'number of stages (geometrical):', ntotal
	write(*,*) 'Distance to decelerator:',l_exc_dec
	write(*,*) 'Distance to detector:   ', l_exc_det
c
	read(inunit,*) dummychar		!comment lines
 	read(inunit,*) dummychar
 	read(inunit,*) dummychar
c 	Parameters used in calculation
  	read(inunit,*) dummy       !velocity synchronous molecule
	read(inunit,*) dummy       !stepsize integration
  	read(inunit,*) dummy       !phase
  	read(inunit,*) mass        !mass
  	mass = mass*1.6605402D-27  ! mass in kg
c
 	read(inunit,*) dummychar	!comment lines
  	read(inunit,*) dummychar
  	read(inunit,*) dummychar
c
	read(inunit,*) n_mol	!number of molecules flown
 	read(inunit,*) trigBU   !trigger time difference diss. and burst unit
	trigBU=trigBU/1.0e6
  	read(inunit,*) pulse    !beam or block pulse
	read(inunit,*) trig_offset	!time offset if block pulse(mus)
	trig_offset=trig_offset*1d-6
	read(inunit,*) x_offset	 !position offset if block pulse (mm)
	x_offset=x_offset*1d-3
  	read(inunit,*) delta_long   !longitudinal extension (block only!)
  	delta_long = delta_long*1D-3
   	read(inunit,*) delta_transverse   !radial extension (block only!)
	delta_transverse = delta_transverse*1D-3
	read(inunit,*) deltar	!extention in radial direction 
  	deltar = deltar*1D-3
	read(inunit,*) deltat	!pulse width (mm)
	deltat=deltat*1D-3	!pulse width (m)
	read(inunit,*) vx0	!average velocity incoming package
	read(inunit,*) deltavx	!rel. velocity spread incoming package
	deltavx=deltavx*vx0
	read(inunit,*) deltavr	!perp. vel. spread perp. beam axis/avg velocity 
	deltavr=deltavr*vx0
c
	close(inunit)
c	XXXXXXXXXXXXXXXXXXXXXXXX END OF READING PARAMETERS XXXXXXXXXXXXXX 
c
	write(*,*) 'Vhex                  : ',Vhex
	write(*,*) 'mass                  : ',mass
	write(*,*) 'Lambda                : ',Lambda
	write(*,*) 'mu                    : ',mu
c
	write(*,*) 'l_exc_hex             : ',l_exc_hex
	write(*,*) 'L_hex                 : ',L_hex
c
	write(*,*) 'n_mol                 : ',n_mol
	write(*,*) '# stages used         : ',ntotal
	write(*,*) 'Pulse length (mm)     : ',deltat
	write(*,*) 'Ext. rad. dir.(m)     : ',deltar
	write(*,*) 'center vel. (m/s)     : ',vx0	
	write(*,*) 'vel. spread (m/s)     : ',deltavx
	write(*,*) 'lim. radial vel. (m/s): ',deltavr
c

 	x0      = 0
c	x0      = 0.10922		!x=0 at excitation region
c
c 	End reading parameters
c
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c			INPUT FILES 
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c	acc file s
c
	filename= filename2
c	****************************************
	call read_T2jump(filename,ntotal,trigBU)   !read in BURST file
c	****************************************
c
	write(*,*) ''
	write(*,*) 'READING acceleration files'
c
	filename='../output/outax.dat'
c	****************************************************************
	call read_acceleration(filename,accx,IXMAX,IYMAX,IZMAX,ni,nj,nk)
c	****************************************************************
	filename='../output/outay.dat'
c	****************************************************************
	call read_acceleration(filename,accy,IXMAX,IYMAX,IZMAX,ni,nj,nk)
c	****************************************************************
	filename='../output/outaz.dat'
c	****************************************************************
	call read_acceleration(filename,accz,IXMAX,IYMAX,IZMAX,ni,nj,nk)
c	****************************************************************
c
	write(*,*) 'INPUT FILES READ'
c
c	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c               OUTPUT FILES
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
	filename='../output/ff.dat                 '
	write(*,*) ''
	write(*,*) 'output written to ', filename
c
	outunit = 0
	open(unit = outunit,file = filename)
c
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c               INITIALISATION
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
c 	Set begin and end of the different potential arrays
c 	What counts here is where which array is used, not physical sizes
c 	First the timngs are checked, 
c	and only in the force where the molecules actually are.
c
	hexapole_begin = l_exc_hex
	hexapole_end = l_exc_hex + L_hex
	decelerator_begin = l_exc_dec
	decelerator_end = l_exc_dec+(nt*l_stage)
c
	write(*,*) 'STARTING CALCULATION'
c
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c			INITIALISATION
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
	maxv = -1.0d60
	minv =  1.0d60
c
	maxt = -1.0d60
	mint =  1.0d60
c
c 	seed = time()    !required for random generator
c 	call srand(seed)    !required for random generator
c	***********************
	call init_random_seed()
c	***********************
c	*****************
c	call itime(start)
c	*****************
c
c
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c		START LOOP OVER MOLECULES 
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

c
c 	p=0
c
	do n=1,n_mol+2  !Loop for number of molecules to be flown
c                   +2 because the first 2 molecules are used to
c					write n_mol and p (see below) 	
c
		if( mod(n,1000).eq.0) then   !screen output during run
			write(*,*) 'done:',n
		endif			
c
		if(n.eq.1) then   !(mis)use first molecule to write this line
			write(outunit,*) n_mol
	   		write(*,*) 'total number of molecules:', n_mol
	  	endif
c
		if(n.eq.2) then  !(mis)use second molecule to write p
			write(outunit,*) p  !number of output lines per molecule
	   		write(*,*) 'output lines', p
		endif   
c
		p=0   !number of output lines per molecule
c 		write(*,*) 'output lines', p
c
		hit = 0		!four hits: 
					!hit=0: No problem
					!hit=1: Crash into electrodes
					!hit=2: Hit at end-aperture decelerator
					!hit=3: No entrance to decelerator
c
c 		Generate the molecules:
c	
        if(pulse.eq.'beam') then
	    	x0 = 0
	    	trig_offset = 0
c	    *************************************************************
	    	call random_package(x0,deltat,trig_offset,deltar,vx0,deltavx,
     .        deltavr,r_sk,l_x0_sk,W_stage,l_exc_dec)	    
c	    *************************************************************
		else
	    	if(pulse.eq.'block') then
	      		x0 = x_offset
c	      *****************************************************
	      		call fake_distribution(x0,delta_long,trig_offset,
     .	        delta_transverse,
     .	        vx0,deltavx,deltavr,r_sk,l_x0_sk,W_stage,l_exc_dec)
c	      *****************************************************
	    	endif    
   		endif
c 
c	*********************
        call writeoutput(n,0)
c	*********************
	   
c 	some initialization for integration routines
c
		do ii=1,15
	   		INFO(ii)=0
	  	enddo
	  	ATOL = 1D-6
	   	RTOL = 1D-6
c       
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c      	START LOOP OVER SWITCH TIMES
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
		tof=trig_offset
	   	i_start=1
	   	i=1
	   	do while(i.le.m)   !find first switch time for integration
			if (t2jump(i).lt.tof) then
                i_start = i
			endif
	     	i=i+1 	     
	   	enddo
c	
           	i = i_start
c	   
	   	do while((i.le.m-1))
c
	     	ptu='freeflight'  !assume free flight
c
	     	if(statusBU(i).eq.'0x0001') then
				ptu='hexapole'
	     	endif
c
	     	if(statusBU(i).eq.'0x2000') then
				ptu='decelerator_vertical'
	     	endif
          	if(statusBU(i).eq.'0x1000') then
				ptu='decelerator_horizontal'
          	endif
c
	  		if(statusBU(i).eq.'0x0200') then
				ptu='decelerator_vertical'
	     	endif
         	if(statusBU(i).eq.'0x0100') then
				ptu='decelerator_horizontal'
         	endif
c
	   		if(statusBU(i).eq.'0x0010') then
				ptu='decelerator_vertical'
	     	endif
          	if(statusBU(i).eq.'0x0020') then
				ptu='decelerator_horizontal'
	     	endif
c
          	if(statusBU(i).eq.'0x0000') then
				ptu='freeflight'
       		endif
c
c	     *****************************
	     call integration(t2jump(i+1))	!integrate to next switch time
c	     *****************************
c	     ***************
	     call checkhit()  !check for crash into electrodes
c	     ***************
c	     ***************************************************
	     call check_min_max(vx,maxv,minv,finaltof,maxt,mint)     	  
c	     ***************************************************
c
             if(i.eq.1) then  !write outputline at switch time i (first stage)
	       call writeoutput(n,i)
	     endif  
c
c
             if(i.eq.(m-1)) then  !write outputline at switch time i (last stage)
	       call writeoutput(n,i)
	     endif  
c
	     i=i+1
c
	   end do   !End loop switching times
c
	   call fly_to_detector(tof,W_stage,l_exc_det,decelerator_end,
     .                                            x,y,z,vx,vy,vz,hit)
	   call writeoutput(n,m+1) !write outputline (detector) 
	end do	 !End loop number of molecules flown
c
c
	close(outunit)
	close(tofoutput)
c
	write(*,*) 'final time of flight',finaltof
	write(*,*) 'MIN Vx',minv
	write(*,*) 'MAX Vx',maxv
	write(*,*) 'MIN tof', mint
	write(*,*) 'MAX tof', maxt
	write(*,*) 'total number of switching times:',m
	write(*,*) 'starting point', i_start
c
	if((trigBU*vx0.lt.(l_nozzle_hex-0.001)).or.
     .      	(trigBU*vx0.gt.(l_nozzle_hex+0.001))) then
  		write(*,*) 'WARNING: Incoupling time does not match!!!'
 	endif
c
	write(*,*) ''
c
1001	stop 'normal termination'
	end
c	
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c 	XXX         END OF PROGRAM                          XXX
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
c
c
c
c
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c 	XXX         WRITE OUTPUT                            XXX
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
	subroutine writeoutput(n,j)
	implicit none
	real*8 tof,x,y,z,vx,vy,vz
	character*128 ptu
	integer hit,p, lastline
	common /nico0/ tof,x,y,z,vx,vy,vz,
     .                 ptu,hit
	common /countlines/ p, lastline    
c
	integer n
	integer j
	integer outunit
c
        outunit = 0
c 	write output line in file ff.dat: 
c 	(start generating output only after molecule #2:
c 	use molecule #1 and #2 to count number of output lines 
c 	per molecule that is very helpful in the binning program)
c     

	if(n.gt.2) then
		write(outunit,*)j,tof,x,y,z,vx,vy,vz,hit
  	endif
c
	p = p+1    !increase number of lines 
c
	return
	end subroutine
c
c
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c 	XXX         CHECK_HIT                               XXX
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
c 	This subroutine checks if the molecule crashes into 
c 	electrodes. If so, it gets hit=1
c
	subroutine checkhit()
	IMPLICIT none
c
	real*8 tof,x,y,z,vx,vy,vz
 	character*128 ptu
	integer hit
 	common /nico0/ tof,x,y,z,vx,vy,vz,
     .                 ptu,hit
	integer odd
  	common /oddd/ odd	
	real*8 mu,Vhex,Lambda,mass,r_hex,B
 	common /hexapole/ Vhex,r_hex
  	real*8 l_exc_dec,l_stage,W_stage,l_exc_det
  	real*8 l_dec_det,l_sk_dec,r_sk,l_x0_sk,phase
  	real*8 l_exc_hex, L_hex
 	common /dimensions/ l_exc_dec, l_exc_hex, L_hex, W_stage
c
 	real*8 hexapole_begin
	real*8 hexapole_end
  	real*8 decelerator_begin
 	real*8 decelerator_end
 	common /arraydimensions/ hexapole_begin,hexapole_end,
     .     decelerator_begin,decelerator_end

c
c 	hexapole
	if( (x.gt.hexapole_begin).and.(x.lt.hexapole_end)      
     .       .and.((y**2 + z**2).gt.r_hex**2)  ) then	  
		hit = 1
	endif
c
c 	region between hexapole and decelerator: r_hex also here the limit
	if( (x.gt.hexapole_end).and.(x.lt.decelerator_begin)      
     .       .and.((y**2 + z**2).gt.r_hex**2)  ) then	  
	   	hit = 1
	endif

c 	decelerator 
	if( (x.gt.decelerator_begin)
     .      .and.(x.lt.decelerator_end)) then
	   	if(   (abs(y).gt.(1.05*W_stage))
     .       .or.(abs(z).gt.(1.05*W_stage))) then
c if(   (abs(y).gt.(5e-3))
c .       .or.(abs(z).gt.(5e-3))) then
	      	hit = 1
	   	endif
	endif

	return
	end subroutine
c
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c XXX         INTEGRATION                             XXX
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
c In this subroutine the real integration is done. 
c Depending on the status of the BURST file, the routine
c calls the correct Force subroutine to integrate.
c
	subroutine integration(t2reach)
	IMPLICIT none
c
	real*8 t2reach
c
	real*8 tof,x,y,z,vx,vy,vz
	character*128 ptu
 	integer hit
	common /nico0/ tof,x,y,z,vx,vy,vz,
     .                 ptu,hit
  	integer odd
 	common /oddd/ odd
c
	integer i
	common /stage/ i	! nico0
c 	integration variables
	INTEGER NEQ,LRW,LIW
	PARAMETER (NEQ=6, LRW=33+7*NEQ, LIW=34)
  	REAL*8 YSTART(NEQ)
  	REAL*8 RWORK(LRW)
  	INTEGER IWORK(LIW)
 	REAL*8  TSTART,T,TEND, TWANT
   	INTEGER INFO(15)
   	INTEGER IDID
  	REAL*8 RTOL, ATOL
   	REAL*8 RPAR(10)
  	INTEGER IPAR(10)
	EXTERNAL DDERKF
 	EXTERNAL F, HEX
	common /nico1/ TSTART,YSTART,TWANT,INFO,RTOL,
     .            ATOL,IDID,RWORK,RPAR,IPAR
c
	real*8 temp
        
c
	TSTART = tof
	TWANT  = t2reach
c
	if((ptu.eq.'freeflight').and.(hit.eq.0)) then
  		x = x + vx*(TWANT-TSTART)
     	y = y + vy*(TWANT-TSTART)
     	z = z + vz*(TWANT-TSTART)
	   tof = TWANT
   	else                   ! end treatment freeflight
c
		YSTART(1) = x		!inital conditions for integration
		YSTART(2) = y
		YSTART(3) = z
		YSTART(4) = vx
		YSTART(5) = vy
		YSTART(6) = vz
c       
	if(ptu.eq.'decelerator_vertical') then
	  	odd=1
	endif
	if((ptu.eq.'decelerator_horizontal')) then 
c 	exchange y and z, i.e., rotate by 90��
		temp = YSTART(2)
     	YSTART(2) = YSTART(3)
     	YSTART(3) = temp
     	temp = YSTART(5)
      	YSTART(5) = YSTART(6)
     	YSTART(6) = temp
     	odd=-1
	endif
c
	if((ptu.eq.'hexapole').and.(hit.eq.0)) then
c	   ***************************************************
	   call DDERKF(HEX, NEQ, TSTART, YSTART, TWANT, INFO,
     .   RTOL, ATOL, IDID, RWORK, LRW, IWORK, LIW, RPAR, IPAR)
c	   ***************************************************
	endif
c
c 	if(((ptu.eq.'decelerator_vertical')
c .  .or.(ptu.eq.'decelerator_horizontal'))) then
c 		write(*,*) 'text'
c 		call DDERKF(F, NEQ, TSTART, YSTART, TWANT, INFO,
c .   RTOL, ATOL, IDID, RWORK, LRW, IWORK, LIW, RPAR, IPAR)
c 	endif	 
c
c
	if(((ptu.eq.'decelerator_vertical')
     .  .or.(ptu.eq.'decelerator_horizontal')).and.(hit.eq.0)) then
c	  ****************************************************
		call DDERKF(F, NEQ, TSTART, YSTART, TWANT, INFO,
     .   RTOL, ATOL, IDID, RWORK, LRW, IWORK, LIW, RPAR, IPAR)
c	  ******************************************************
	endif	 
c
	if(hit.eq.0) then  !increase tof only if molecule has not crashed
     	tof = TWANT
	endif
c	  
	INFO(1)=0
c 	if(IDID.ne.2) then
c  		IDID=2 iff the integration went OK to the TEND
c  		write(*,*) 'ERROR T in the ',ptu
c 	endif
c
c 	Return coordinates to main program
c
	x = YSTART(1)
	y = YSTART(2)
	z = YSTART(3)
	vx = YSTART(4)
	vy = YSTART(5)
	vz = YSTART(6)
c
	if(ptu.eq.'decelerator_horizontal') then 
c 	reexchange x and z...
		temp = y
      	y = z
       	z = temp
	   	temp = vy
     	vy = vz
     	vz = temp
	endif
c
	endif			!not freeflight
	return
	end subroutine
c	
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c 	XXX         HEXAPOLE FORCE                          XXX
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
	subroutine HEX(T, Y0, YP,RPAR,IPAR)
	IMPLICIT none
c	
	real*8 T, Y0(*), YP(*)		!YP = F(T, Y0)
c
	REAL*8 RPAR
	INTEGER IPAR
	real*8 Vhex, r_hex
	common /hexapole/Vhex,r_hex
c
 	real*8 hexapole_begin
   	real*8 hexapole_end
  	real*8 decelerator_begin
  	real*8 decelerator_end
  	common /arraydimensions/ hexapole_begin,hexapole_end,
     .     decelerator_begin,decelerator_end
	real*8 mu,mass,Lambda,B
	common /molecule/mu,mass,Lambda,B
c
    real*8 tof,x,y,z,vx,vy,vz
 	character*128 ptu
 	integer hit
 	common /nico0/ tof,x,y,z,vx,vy,vz,
     .                 ptu,hit
c	
	real*8 enfrac
	real*8 k
	real*8 r
c
c
	k = (3.D0*mu*Vhex/r_hex**3)*B
	r = sqrt(Y0(2)*Y0(2) + Y0(3)*Y0(3))
	enfrac = (6.63D-34)*Lambda*1.0D9/k
c	
c 	check position
	if (    (r.lt.r_hex)
     .     .and.(Y0(1).gt.hexapole_begin)
     .     .and.(Y0(1).lt.(hexapole_end))) then  
	   	YP(4) = 0.0D0
	   	YP(5) = -k/mass*(Y0(2))/sqrt(1 + (enfrac/(r**2))**2)
	   	YP(6) = -k/mass*(Y0(3))/sqrt(1 + (enfrac/(r**2))**2) 
	else
	   	YP(4) = 0.0D0
	   	YP(5) = 0.0D0
	   	YP(6) = 0.0D0
	endif	
c       
	YP(1) = Y0(4)
	YP(2) = Y0(5)
	YP(3) = Y0(6)	
c
	return
	end subroutine
c
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c 	XXX         DECELERATOR/BUNCHER FORCE               XXX
c 	XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
	SUBROUTINE F(T,Y0,YP, RPAR, IPAR)
	IMPLICIT none
c	
	REAL*8 	T
	REAL*8  Y0(*),YP(*)
c	
	REAL*8 RPAR
	INTEGER IPAR
c
	REAL*8  YP1,YPN
c
	integer IXMAX,IYMAX,IZMAX
	PARAMETER(IXMAX=200,IYMAX=200,IZMAX=200)
   	real*8 accx(IXMAX,IYMAX,IZMAX)
	real*8 accy(IXMAX,IYMAX,IZMAX)
   	real*8 accz(IXMAX,IYMAX,IZMAX)
 	real*8 gu
  	integer ni,nj,nk
	common /basis/ accx,accy,accz
  	common /basis/  gu
  	common /basis/ ni,nj,nk
c
  	integer nt,ntotal
  	common /basis/ nt,ntotal
c
  	integer nb,ne,nd
  	common /basis/ nb,ne,nd
c
  	real*8 xx,yy,zz
c
	real*8 g0i,g0j,g0k	
c
   	integer i
  	common /stage/ i	! put in nico0call 
c
	real*8 l_exc_dec, l_exc_hex, L_hex, W_stage
	common /dimensions/ l_exc_dec, l_exc_hex, L_hex, W_stage
c
	integer odd
   	common /oddd/ odd
c
  	integer xi,yj,zk,xit,xiactual
c
c 	new variables
c
  	real*8 tof,x,y,z,vx,vy,vz
 	character*128 ptu
 	integer hit
  	common /nico0/ tof,x,y,z,vx,vy,vz,
     .                 ptu,hit
c
  	real*8 hexapole_begin
  	real*8 hexapole_end
  	real*8 decelerator_begin
 	real*8 decelerator_end
 	common /arraydimensions/ hexapole_begin,hexapole_end,
     .     decelerator_begin,decelerator_end
c
c 	check longitudinal position
	if((Y0(1).gt.decelerator_begin).and.
     .	   (Y0(1).lt.decelerator_end)) then
c
		g0i = -decelerator_begin
		g0j = dfloat(nj+1)/(2.*gu)
		g0k = dfloat(nk+1)/(2.*gu)
c
		xi = dint((Y0(1)+g0i)*gu) 
		yj = dint((Y0(2)+g0j)*gu)
		zk = dint((Y0(3)+g0k)*gu)
c
c
		if(((xi.ge.0).and.(xi.le.ntotal*nd)).and.
     .	   ((yj.ge.1).and.(yj.le.nj)).and.
     .	   ((zk.ge.1).and.(zk.le.nk)))   then
c
c
	  		xx = (Y0(1)+g0i)*gu - dfloat(xi)
	  		yy = (Y0(2)+g0j)*gu - dfloat(yj)
	  		zz = (Y0(3)+g0k)*gu - dfloat(zk)
c
c	
			xiactual=xi  
c	  
	  		if(odd.eq.-1) then
	    		xi=xi+nd
  	  		endif
c
        	xit=xi
c 
			if ( (xiactual.le.nt*nd) ! molecule in decelerator spatially
     .  .and.(i-3.le.nt) ) then  ! test in time crap!!!
c
	  			xi = mod(xit,(2*nd))   
          		if(xi.lt.nd) then  
c
  	    		xi = xi+nb
c
	YP(4)=((1.0 - xx)*(1.0 - yy)*(1.0 - zz)*accx(xi,yj,zk)
     .         +(xx)      *(1.0 - yy)*(1.0 - zz)*accx(xi+1,yj,zk)
     .         +(1.0 - xx)*(yy)*      (1.0 - zz)*accx(xi,yj+1,zk)
     .         +(xx)      *(yy)*      (1.0 - zz)*accx(xi+1,yj+1,zk)
     .         +(1.0 - xx)*(1.0 - yy)*(zz)      *accx(xi,yj,zk+1) 
     .         +(xx)      *(1.0 - yy)*(zz)      *accx(xi+1,yj,zk+1) 
     .         +(1.0 - xx)*(yy)      *(zz)      *accx(xi,yj+1,zk+1) 
     .         +(xx)      *(yy)      *(zz)      *accx(xi+1,yj+1,zk+1))
c
	YP(5)=((1.0 - xx)*(1.0 - yy)*(1.0 - zz)*accy(xi,yj,zk)
     .         +(xx)      *(1.0 - yy)*(1.0 - zz)*accy(xi+1,yj,zk)
     .         +(1.0 - xx)*(yy)*      (1.0 - zz)*accy(xi,yj+1,zk)
     .         +(xx)      *(yy)*      (1.0 - zz)*accy(xi+1,yj+1,zk)
     .         +(1.0 - xx)*(1.0 - yy)*(zz)      *accy(xi,yj,zk+1) 
     .         +(xx)      *(1.0 - yy)*(zz)      *accy(xi+1,yj,zk+1) 
     .         +(1.0 - xx)*(yy)      *(zz)      *accy(xi,yj+1,zk+1) 
     .         +(xx)      *(yy)      *(zz)      *accy(xi+1,yj+1,zk+1))
c
	YP(6)=((1.0 - xx)*(1.0 - yy)*(1.0 - zz)*accz(xi,yj,zk)
     .         +(xx)      *(1.0 - yy)*(1.0 - zz)*accz(xi+1,yj,zk)
     .         +(1.0 - xx)*(yy)*      (1.0 - zz)*accz(xi,yj+1,zk)
     .         +(xx)      *(yy)*      (1.0 - zz)*accz(xi+1,yj+1,zk)
     .         +(1.0 - xx)*(1.0 - yy)*(zz)      *accz(xi,yj,zk+1) 
     .         +(xx)      *(1.0 - yy)*(zz)      *accz(xi+1,yj,zk+1) 
     .         +(1.0 - xx)*(yy)      *(zz)      *accz(xi,yj+1,zk+1) 
     .         +(xx)      *(yy)      *(zz)      *accz(xi+1,yj+1,zk+1))
c
          		else ! if(xi.ge.nd) 
c
            		xi = nb + (nd - (xi-nd)) !(2nd-xi)+nb
c
        YP(4)=-((1.0 - xx)*(1.0 - yy)*(1.0 - zz)*accx(xi,yj,zk)
     .         +(xx)      *(1.0 - yy)*(1.0 - zz)*accx(xi-1,yj,zk)
     .         +(1.0 - xx)*(yy)*      (1.0 - zz)*accx(xi,yj+1,zk)
     .         +(xx)      *(yy)*      (1.0 - zz)*accx(xi-1,yj+1,zk)
     .         +(1.0 - xx)*(1.0 - yy)*(zz)      *accx(xi,yj,zk+1)
     .         +(xx)      *(1.0 - yy)*(zz)      *accx(xi-1,yj,zk+1)
     .         +(1.0 - xx)*(yy)      *(zz)      *accx(xi,yj+1,zk+1)
     .         +(xx)      *(yy)      *(zz)      *accx(xi-1,yj+1,zk+1))
c
        YP(5)=((1.0 - xx)*(1.0 - yy)*(1.0 - zz)*accy(xi,yj,zk)
     .         +(xx)      *(1.0 - yy)*(1.0 - zz)*accy(xi-1,yj,zk)
     .         +(1.0 - xx)*(yy)*      (1.0 - zz)*accy(xi,yj+1,zk)
     .         +(xx)      *(yy)*      (1.0 - zz)*accy(xi-1,yj+1,zk)
     .         +(1.0 - xx)*(1.0 - yy)*(zz)      *accy(xi,yj,zk+1)
     .         +(xx)      *(1.0 - yy)*(zz)      *accy(xi-1,yj,zk+1)
     .         +(1.0 - xx)*(yy)      *(zz)      *accy(xi,yj+1,zk+1)
     .         +(xx)      *(yy)      *(zz)      *accy(xi-1,yj+1,zk+1))
c
        YP(6)=((1.0 - xx)*(1.0 - yy)*(1.0 - zz)*accz(xi,yj,zk)
     .         +(xx)      *(1.0 - yy)*(1.0 - zz)*accz(xi-1,yj,zk)
     .         +(1.0 - xx)*(yy)*      (1.0 - zz)*accz(xi,yj+1,zk)
     .         +(xx)      *(yy)*      (1.0 - zz)*accz(xi-1,yj+1,zk)
     .         +(1.0 - xx)*(1.0 - yy)*(zz)      *accz(xi,yj,zk+1)
     .         +(xx)      *(1.0 - yy)*(zz)      *accz(xi-1,yj,zk+1)
     .         +(1.0 - xx)*(yy)      *(zz)      *accz(xi,yj+1,zk+1)
     .         +(xx)      *(yy)      *(zz)      *accz(xi-1,yj+1,zk+1))
c
          		endif ! xi.lt.nd test  
c
				else !test in time and position

         YP(4) = 0
         YP(5) = 0
         YP(6) = 0
c
				endif !test in time and position decelerator  
c
				else !big test on xi,yj,zk
c	  
	  YP(4) = 0
	  YP(5) = 0
	  YP(6) = 0
c
		if( (((yj.lt.1).or.(yj.gt.nj)).or.
     .	       ((zk.lt.1).or.(zk.gt.nk))).and.
     .         ((xi.le.ntotal*nd).and.(xi.ge.0)) )   then
c
	    hit = 1
c
		endif
c
c	
	endif  !big test on xi,yj,zk
c		
	YP(1) = Y0(4)
	YP(2) = Y0(5)
	YP(3) = Y0(6)
c
c end test molecule between decelerator_begin and buncher_end
	else
	   YP(1) = Y0(4)
	   YP(2) = Y0(5)
	   YP(3) = Y0(6)
	   YP(4) = 0D0
	   YP(5) = 0D0
	   YP(6) = 0D0
	endif	
c
	RETURN
	END SUBROUTINE
c
c
c
c
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c XXX         MY rgauss                               XXX
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
c	This subroutine generates the package of molecules
c
        real*8 function myrgauss (xmean, sd)
	IMPLICIT none
c       
        real*8 xmean,sd,rn
	integer I
	call random_number(rn)
        myrgauss = -6.0
        DO 10 I=1,12
	   call random_number(rn)
           myrgauss = myrgauss + rn
 10	continue
        myrgauss = xmean + sd*myrgauss
        return
	end
c
c
c
c Routine to generate a position and velocity distribution for a package
c of molecules. Gaussian along x direction (beam axis, width det. by deltat),
c block along y,z direction. Velocity: gaussian along x, block along y,z
c direction. vx, vy, vz, y and z are determined for a cross section of the beam 
c at postion x0=0; if a molecule with these values can pass c the skimmer, the 
c random time is determined, together with x, y, z for this timing. 
c
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c XXX                  RANDOM PACKAGE                          XXX
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  
        subroutine random_package(x0,deltat,trig_offset,deltar,vx0,
     .             deltavx,deltavr,r_sk,
     .             l_x0_sk,W_stage,l_exc_dec)
        IMPLICIT none
c
	real*8 x0,deltat,trig_offset,deltar
	real*8 t,r,rn
	real*8 vx0,deltavx,deltavr,theta,vr,vtheta
	real*8 r_sk,l_x0_sk,W_stage,l_exc_dec
	real*8 d0,tan_phi,r_max,vr_max
	real*8 A,B,EPS,ETA
	real*8 V
c
        real*8 tof,x,y,z,vx,vy,vz
        character*128 ptu
        integer hit
        common /nico0/ tof,x,y,z,vx,vy,vz,ptu,hit
c
c REAL*8 random
c EXTERNAL random          
c REAL*8 RANDOMN, errorf
c EXTERNAL C05ADF,ERRORF
c COMMON /ERROR/ RANDOMN
c	  
	REAL*8 MYRGAUSS
c       
	x=0D0			! defaults to enter do-while loop
	vx=vx0
	y=1D60
	vy=0D0
	z=1D60
	vz=0D0
c
	do while(((y+vy*(l_x0_sk)/vx)**2+
     .   (z+vz*(l_x0_sk)/vx)**2).gt.(r_sk*r_sk))
c
c Only molecules passing through skimmer are used. 
c 
c "Gaussian" for vx:
c
	  vx = myrgauss(vx0,deltavx/2D0)   !HWHM should be used here
c "Block" for y,z:
	  call random_number(rn)
	  r      = deltar      * sqrt(rn)
	  call random_number(rn)
	  theta  = 2*3.14159 *rn
	  y  = r * sin(theta)
	  z  = r * cos(theta)
c "Block" distribution for vy,vz!!
          call random_number(rn)
	  vr      = deltavr * sqrt(rn)
          call random_number(rn)
	  vtheta  = 2.* 3.14159 *(rn)
	  vy = vr * sin(vtheta)
	  vz = vr * cos(vtheta)
	end do
c
c"Gaussian" for t (and x):
c
c       t = myrgauss(trig_offset,deltat/2D0)
c 	x=vx*t
c	y=y+vy*t
c	z=z+vz*t
c
c Fixed value for t: time of dissociation:
c
        t = trig_offset
c
c "Gaussian" for x in dissociation:
c
	x = myrgauss(0D0,deltat/2D0)  !HWHM should be used
c
	return
	end subroutine	  
c
c
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c XXXXX     RANDOM PACKET; FAKE DITRIBUTION           XXX
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
        subroutine fake_distribution(x0,delta_long,trig_offset,
     .	       delta_transverse,
     .  	vx0,deltavx,deltavr,r_sk,
     .             l_x0_sk,W_stage,l_exc_dec)
        IMPLICIT none
c
	real*8 x0,delta_long,trig_offset,delta_transverse
	real*8 t,r,rn
	real*8 vx0,deltavx,deltavr,theta,vr,vtheta
	real*8 r_sk,l_x0_sk,W_stage,l_exc_dec
	real*8 d0,tan_phi,r_max,vr_max
	real*8 A,B,EPS,ETA
	real*8 V
c
        real*8 tof,x,y,z,vx,vy,vz
        character*128 ptu
        integer hit
        common /nico0/ tof,x,y,z,vx,vy,vz,ptu,hit
c
	REAL*8 random
	EXTERNAL random
c
	REAL*8 RANDOMN, errorf
	EXTERNAL C05ADF,ERRORF
	COMMON /ERROR/ RANDOMN
c	  
	REAL*8 MYRGAUSS
c       
	x=0D0			! defaults to enter do-while loop
	vx=vx0
	y=1D60
	vy=0D0
	z=1D60
	vz=0D0
c 
c
c "Block" for x,y,z:
        call random_number(rn)
c	write(*,*) rn
        x  = x0 + delta_long*(rn-0.5)
        call random_number(rn)   
c	write(*,*) rn
	y= delta_transverse*(rn-0.5)
        call random_number(rn) 
c	write(*,*) rn
	z  = delta_transverse*(rn-0.5)
c
c "Block" distribution for vx,vy,vz
c
        call random_number(rn)
c	write(*,*) rn
        vx = vx0 + deltavx*(rn-0.5)
        call random_number(rn)
c	write(*,*) rn
	vy = deltavr*(rn-0.5)
        call random_number(rn)
c	write(*,*) rn
	vz = deltavr*(rn-0.5)
c
c Fixed value for t
c
        tof = trig_offset
c


	return
	end subroutine	  
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c XXX         READ ACCELERATION                       XXX
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
	subroutine read_acceleration(filename2,acceleration,IXM,IYM,IZM,
     .  ni,nj,nk)
	IMPLICIT none
c
c Reading acceleration file (x-z direction) from disc
c
	character*128 filename2
	integer IXM,IYM,IZM
	real*8 acceleration(IXM,IYM,IZM)
	integer ni,nj,nk,inunit,i,j,k,dummyi,dummyj,dummyk
c	
	inunit=0
	open(unit=inunit,file=filename2)
	read(inunit,*) ni,nj,nk
	write(*,*) 'Dimensions array ',filename2,ni,nj,nk
	do i=1,ni
	  do j=1,nj
	    do k=1,nk
	     read(inunit,*) dummyi,dummyj,dummyk,acceleration(i,j,k)
            end do
	  end do
	end do 
c
c
	close(inunit)
	return
	end subroutine
c
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c XXX         READ TIME TO JUMP                       XXX
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
c Read in the BURST file. The subroutine recognizes 
c the format and length of the BURST file.
c
	subroutine read_T2jump(filename,ntotal,trigBU)
	IMPLICIT none
	character*128 filename
	integer ntotal,m
	integer dummyi,inunit
	integer MAXTIMINGS
        PARAMETER(MAXTIMINGS=9000)
	real*8 t2jump(MAXTIMINGS),TOFFSET,Thexapole,Thexapole_off,trigBU
        character*6 statusBU(MAXTIMINGS)
        common t2jump,m,statusBU,TOFFSET,
     .       Thexapole,Thexapole_off
c
	integer i,j,k,ii,aaa
	real*8 dump
	character*1 dumpc
	integer zero
c
	zero=ichar('0')
	inunit=1
	open(unit = inunit,file = filename)
c
        j=0
        k=0
        dumpc='#'
        do while (dumpc.eq.'#')   !count number of lines
           read(inunit,*) dumpc
	   j=j+1
	   if(dumpc.eq.'[') then
	      k=j    !count number of comment lines
           endif
	enddo
	write(*,*) 'first j :',j
	write(*,*) 'first k :',k
	do while (dumpc.ne.'#')
	   read(inunit,*) dumpc
	   j=j+1
        enddo
	write(*,*) 'second j :',j
        close(inunit)
c
	inunit=1
	open(unit = inunit,file = filename)
c
	write(*,*) ''
	write(*,*) 'READING Switch-timesequence: ',filename
c
        do i=1,k
	  read(inunit,*) dumpc
	enddo
c
        m = j-k-1  !number of lines containing switch times
	i=1
	write(*,*) 'write out m valve :',m
        do i=1,m   ! read in all switch times;
	   read(inunit,*) t2jump(i), statusBU(i)
	   t2jump(i) = ((t2jump(i)-1010)*1.0e-9)+trigBU  
c		! add incoupling time to all
c				switch times and correct for 1.01 mus		     
	enddo
c
        close(inunit)
	return
	end subroutine
c
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c XXX         CHECK MIN MAX                           XXX
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
c This subroutine keeps track of the highest and lowest 
c velocity etc. that has occured
c
	subroutine check_min_max(vx,maxv,minv,finaltof,maxt,mint)
	IMPLICIT none
c
	real*8 vx,maxv,minv
	real*8 finaltof,maxt,mint
c
c
	if(vx.gt.maxv) then
	  maxv = vx
	endif 
	if(vx.lt.minv) then
	  minv = vx
	endif
c
	if(finaltof.gt.maxt) then
	  maxt = finaltof
	endif 
	if(finaltof.lt.mint) then
	  mint = finaltof
	endif  
	return
	end subroutine
c

c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c                          Fly to detector
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
	subroutine fly_to_detector(tof,W_stage,l_exc_det,
     .                               decelerator_end,x,y,z,vx,vy,vz,hit)
	IMPLICIT none
	real*8 tof,W_stage,l_exc_det,decelerator_end
	real*8 x,y,z,vx,vy,vz
	integer hit
c       
c        if (hit.eq.0) then
c	  tof = tof +    (l_exc_det-x)/vx       
c	  y   = y   + vy*(l_exc_det-x)/vx
c	  z   = z   + vz*(l_exc_det-x)/vx
c	  x   = l_exc_det
c	endif  

        if (hit.eq.0) then
	  if (x.lt.decelerator_end) then
	     tof = tof + (decelerator_end-x)/vx   !fly to end decelerator first
	     y   = y   + vy*(decelerator_end-x)/vx
	     z   = z   + vz*(decelerator_end-x)/vx
	     x   = decelerator_end
             if ((abs(y).gt.W_stage).or.(abs(z).gt.W_stage)) then
	        hit = 1
	     endif	 
          endif             
          tof = tof +    (l_exc_det-x)/vx  ! fly to detector     
          y   = y   + vy*(l_exc_det-x)/vx
	  z   = z   + vz*(l_exc_det-x)/vx
	  x   = l_exc_det
	endif  
c
c	
	return
	end subroutine
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c               RANDOM
c XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c
         SUBROUTINE init_random_seed()
c IMPLICIT none
           INTEGER :: i, n, clock
           INTEGER, DIMENSION(:), ALLOCATABLE :: seed
c
           CALL RANDOM_SEED(size=n)
           ALLOCATE(seed(n))
c
           CALL SYSTEM_CLOCK(COUNT=clock)
c
           seed = clock + 37 * (/ (i - 1, i = 1, n) /)
           CALL RANDOM_SEED(PUT = seed)
c
           DEALLOCATE(seed)
         END SUBROUTINE
c	
