module LinearSearchTools
	use tensor_type
	implicit none
	real*8,private,parameter::zero_number=1d-16
	real*8,private,parameter::InfinityNumber=1d100
contains
	!*************************************************************
	!*************************************************************
	!                 linear search
	!*************************************************************

	!linear search NOTE:
		!  Linear search: f(x)=f(P+x*d) is a function of x
		!                 g(x)= vec{g(P+x*d)} \cdot \vec{d}
		!                 g(P+x*d) is a vector but  g(x) not
		!
		! 1. input x0, it is a random number at the first time
		! 2. input         f(P)    --->  f(0)
		!                  g(P)    --->  g(0)
		! 3. calculate   f(P+x0*d) --->  f(x0)
		! 4. calculate g(P+x0*d)*d --->  g(x0)
		! 5. suppose f(x)=a*x^3+b*x^2+c*x+d ,use x=0,f(0),g(0),x0,f(x0),g(x0)
		!   solve a,b,c,d
		! 6. When we have a,b,c,d min f(x)=a*x^3+b*x^2+c*x+d--->output x1
		!   1). f'(x)=3*a*x^2+2*b*x+c=0 ==> x1
		!   2). if Delta= (2*b)^2-4*(3*a)*c <0, no root of f'(x)=0,
		!      use quadratic interpolation
		!   3).Quadratic interpolation: 
		!      suppose g(x)=kx+b, use x=0,g(0),x0,g(x0) solve k and b
		!      x1=-b/k
		! 7. calculate   f(P+x1*d) --->  f(x1)
		! 8. calculate g(P+x1*d)*d --->  g(x1)
		! 9. Now there are 3 piont
		!     xa =  0     xb =  x0     xc =  x1
		!     fa = f(0)   fb = f(x0)   fc = f(x1)
		!     ga = g(0)   gb = g(x0)   gb = g(x1)
		!   1).if ga*gb>0, use Quadratic interpolation, output x2
		!    (1).suppose g(x)=kx+b, use xa,ga,xb,gb solve k and b
		!      x2=-b/k
		!    (2). calculate   f(P+x2*d) --->  f(x2)
		!    (3). calculate g(P+x2*d)*d --->  g(x2)
		!    (4). if fa*fc<0 keep fa ,drop fb
		!            xb=xc,fb=fc,gb=gc
		!            xc=x2,fc=f2,gc=g2
		!        else keep fb ,drop fa
		!           xa=xc,fa=fc,ga=gc
		!            xc=x2,fc=f2,gc=g2
		!          if all point on the sameside,drop xb   
		!    (5).  go to 9.
		!     
		!   2). if ga*gb<0, use cubic interpolation
		!     (1). suppose g(x)=a*x^2+b*x+c and f(x)=\int_0^x g(t)dt=a/3*x^3+b/2*x^2+c*x
		!     (2). xa,ga,xb,gb,xc,gc to solve a,b,c in g(x)=a*x^2+b*x+c
		!     (3). g(x2)=0 find two x2, choose the one that min f(x) as output x2
		!     (4). calculate   f(P+x2*d) --->  f(x2)
		!     (5). calculate g(P+x2*d)*d --->  g(x2)
		!     (6). if fa*fc<0 keep fa ,drop fb
		!            xb=xc,fb=fc,gb=gc
		!            xc=x2,fc=f2,gc=g2
		!        else keep fb ,drop fa
		!           xa=xc,fa=fc,ga=gc
		!            xc=x2,fc=f2,gc=g2
		!       if all point on the sameside,drop xb   
		!     (7). go to 9.
		! 10. repete 9 about LinearSearch_max_running time, output x
		! 11. new P is P+x*d, x will be input x0 in next Linear search circle
		!*************************************************************
		!*************************************************************

	!solvefx:
		!f(x)=ax^3+bx^2+cx+d
		!g(x)=3ax^2+2bx+c
		!input f(0),g(0),f(x1),g(x1),x1==>output a,b,c,d

	subroutine solvefx(a,b,c,d,f0,g0,f1,g1,x1)!use in LinearSearch_third_point
		real*8,intent(inout)::a,b,c,d
		real*8,intent(in)::f0,g0,f1,g1,x1
		real*8::x1_3,temp
		x1_3=x1*x1*x1
		c=g0
		d=f0
		a= ((g1+c)*x1-2d0*(f1-d))/x1_3
		b= (g1-c)/(2d0*x1)-1.5d0*x1*a
		return
	end subroutine

	!zerofx
		!f(x)=ax^3+bx^2+cx+d
		!input a, b, c, d ,find minf(x)
		!outx should be larger than 0
		!output outx=min(x1,x2)

	subroutine zerofx(outx,a,b,c,d,Delta)!use in LinearSearch_third_point
		real*8,intent(in)::a,b,c,d
		real*8,intent(inout)::outx,Delta
		real*8::x1,x2,f1,f2
		!Delta=4d0*b*b-12*a*c
		if(Delta.lt.zero_number) then
			write(*,*)"ERROR, should call zerolinearfx"
			stop
		end if
		if(dabs(a).le.zero_number)then!f(x)=bx^2+cx+d
			if(dabs(b).le.zero_number) then !f(x)=cx+d
				write(*,*)"ERROR in zerofx"
				stop
			end if
			outx=-c/(2d0*b)
			return
		end if
		Delta=dsqrt(Delta)
		x1=(-2d0*b+Delta)/(6d0*a)
		x2=(-2d0*b-Delta)/(6d0*a)
		f1=a*x1*x1*x1+b*x1*x1+c*x1+d
		f2=a*x2*x2*x2+b*x2*x2+c*x2+d
		if(x1.le.x2)then
			outx=x1
			if(outx.le.0) outx=x2
		else
			outx=x2
			if(outx.le.0) outx=x1
		end if
		return
	end subroutine

	!LinearSearch_third_point:
		!input two point
		!x0=0,f0,g0,x1,f1,g1
		!find the next point
		!suppose f(x)=ax^3+bx^2+cx+d,then g(x)=3ax^2+2bx+c
		!input x0=0,f0,g0,x1,f1,g1 can determin a,b,c,d
		!Then find xmin,g(xmin)=0
		!If there is no root in the equation g(x)=0
		!Then regard (0,g0) and (x1,g1) as two point on a line g(x)=kx+b
		!Then find xmin,g(xmin)=0

	subroutine LinearSearch_third_point(outx,f0,g0,f1,g1,x1)
		real*8,intent(inout)::outx
		real*8,intent(in)::f0,g0,f1,g1,x1
		real*8::Delta,a,b,c,d
		call solvefx(a,b,c,d,f0,g0,f1,g1,x1)
		Delta=4d0*b*b-12*a*c
		if(Delta.lt.zero_number) then
			call zerolinearfx(outx,0d0,g0,x1,g1)
			return
		end if
		call zerofx(outx,a,b,c,d,Delta)
		return
	end subroutine

	!zerolinearfx:
		!g(x)=kx+b
		!input x1,g1,x2,g2
		!g(x0)=0,output x0

	subroutine zerolinearfx(outx,x1,g1,x2,g2,stopflag)
		real*8,intent(inout)::outx
		real*8,intent(in)::x1,g1,x2,g2
		logical,optional,intent(inout)::stopflag
		real*8::k,b
		k=(g1-g2)/(x1-x2)
		if(dabs(k).le.zero_number)then
			if(present(stopflag)) then
				stopflag=.true.
				return
			else
				write(*,*)"ERROR in zerolinearfx,k=0"
				write(*,*)x1,g1
				write(*,*)x2,g2
				stop
			end if
		end if
		b=g1- (x1*k)
		outx=-b/k
		return
	end subroutine

	!LinearSearch_fouth_point:
		!on output,fc,gc are useless
		!on input, outx and xc can be the same variable,but other cannot
		!
		!if (ga*gb) >0
		! 
		! use b and c ==> d .because c may be obtain from a,b, if use a and b to get d,  c and d may be the same,go wrong
		! a,b,c keep two point
		!   if(ga*gc<0) keep a and c
		!   if(gb*gc<0) keep b and c
		!   ga * gb is larger than 0,so other case is ga ,gb ,gc are the same sign
		!     in this case ,drop b, keep a ,c
		!If ga,gb,gc are the same sign, use Quadratic interpolation,otherwhile use cubic interpolation

	subroutine LinearSearch_fouth_point(outx,stopflag,xa,fa,ga,xb,fb,gb,xc_,fc,gc,max_x)
		real*8,intent(inout)::outx,xa,fa,ga,xb,fb,gb,max_x
		logical,intent(inout)::stopflag
		real*8,intent(in)::xc_,fc,gc
		real*8::xc
		real*8::a,b,c,Delta
		logical::Quadraticflag
		!Quadraticflag,Quadratic interpolation
		xc=xc_
		if(ga*gb.ge.0d0) then
			call zerolinearfx(outx,xa,ga,xc,gc,stopflag)
			if(stopflag.or.(outx.le.0d0))then
			 	call choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
			 	stopflag=.true.
			 	return
			end if
			fb=fc
			gb=gc
			xb=xc
			return
		end if
		call solvefx2(a,b,c,xa,ga,xb,gb,xc,gc)
		if(dabs(a).le.zero_number)then!g(x)=bx+c
			if(dabs(b).le.zero_number) then
				call choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
			else
				outx=-c/b
			end if
			stopflag=.true.
			if(outx.le.0d0) call choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
			return
		end if
		Delta=b*b-4.*a*c!g(x)=ax^2+bx+c
		if( (Delta.ge.0) .and.(Delta.lt.InfinityNumber)) then
			call zerolinearfx2(outx,a,b,c,Delta)
			if(outx.le.0d0) then
				stopflag=.true.
				call choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
			end if
			if(ga*gc.gt.0d0) then
				ga=gc
				fa=fc
				xa=xc
			end if
			if(gb*gc.gt.0d0) then
				gb=gc
				fb=fc
				xb=xc
			end if
		else! if the g is too small or x are too close,Delta=NAM
			stopflag=.true.
			call choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
		end if
		return
	end subroutine

	!choose_fx:
		!outx should be larger than 0	
		!outx=min(xa,xb,xc)

	subroutine choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
		real*8,intent(in)::xa,xb,xc,fa,fb,fc,max_x
		real*8,intent(out)::outx
		real*8::f
		outx=xa
		f=fa
		if(outx.ge.xb) then	
			if(xb.ge.0) then
			 outx=xb
			 f=xb
			end if
		end if
		if(outx.ge.xc) then	
			if(xc.ge.0) then
			 outx=xc
			 f=xc
			end if
		end if
		if(outx.le.0d0)then
			outx=max_x
		end if
		return

		!outx=xa
		!f=fa
		!if(f.ge.fb) then	
		!	if(xb.ge.0) then
		!	 outx=xb
		!	 f=xb
		!	end if
		!end if
		!if(f.ge.fc) then	
		!	if(xc.ge.0) then
		!	 outx=xc
		!	 f=xc
		!	end if
		!end if
		!if(outx.le.0d0)then
		!	outx=max_x
		!end if
		!return
	end subroutine

	!solvefx2
		!g(x)=ax^2+bx+c
		!output a,b,c

	subroutine solvefx2(a,b,c,x1,g1,x2,g2,x3,g3)
		real*8,intent(inout)::a,b,c
		real*8,intent(in)::g1,x1,g2,x2,g3,x3
		real*8::temp1,x1_2,x2_2,x3_2
		x1_2=x1*x1
		x2_2=x2*x2
		x3_2=x3*x3
		temp1= (x2_2-x1_2)/(x3_2-x1_2)
		b=(temp1*(g3-g1)-g2+g1)/(temp1*(x3-x1)-x2+x1)
		temp1= (x2-x1)/(x3-x1)
		a=(temp1*(g3-g1)-g2+g1)/(temp1*(x3_2-x1_2)-x2_2+x1_2)
		c=g3-x3_2*a-x3*b
		return
	end subroutine

	!zerolinearfx2
		!g(x)=ax^2+bx+c ==>f(x)=int_0^x g(t)dt
		!output minf(x) and outx should be larger than 0
		!output x=min(x1,x2)

	subroutine zerolinearfx2(outx,a,b,c,Delta)
		real*8,intent(in)::a,b,c
		real*8,intent(out)::outx,Delta
		real*8::x1,x2,f1,f2
		Delta=dsqrt(Delta)
		if(dabs(a).le.zero_number)then!g(x)=bx+c
			write(*,*)"ERROR in zerolinearfx2"
			stop
		end if
		x1=(-b+Delta)/(2d0*a)
		x2=(-b-Delta)/(2d0*a)
		f1=(a*x1*x1*x1/3d0)+(b*x1*x1*0.5d0)+c*x1
		f2=(a*x2*x2*x2/3d0)+(b*x2*x2*0.5d0)+c*x2
		if(x1.le.x2)then
			outx=x1
			if(outx.le.0) outx=x2
		else
			outx=x2
			if(outx.le.0) outx=x1
		end if
		return
	end subroutine

	!*************************************************************
	!*************************************************************
	!                 linear search2
	!   use the least square method 
	!    f(x)=a*x^3+b*x^2+c*x+d 
	!    g(x)=3*a*x^2+2*b*x+c
	!   if there are two point: x0=0 f0 g0 and x1 f1 g1
	!       / 0       0     0   1 \
	!       | 0       0     1   0 |
	!   M=  | x1^3   x1^2  x1   1 |
	!       \3*x1^2  2*x1   1   0 /
	!   
	!     / a \
	!   A=| b |
	!     | c |
	!     \ d /
	!
	!     / f0 \
	!   B=| g0 |
	!     | f1 |
	!     \ g1 /
	!
	!then  A=(M^T*M)^(-1)*(M^T*B)
	!   if there are more than two point: x0=0 f0 g0 ,..., xi fi gi
	!       / 0       0     0   1 \
	!       | 0       0     1   0 |
	!       |                     |
	!   M=  | ...                 |
	!       |                     |
	!       | xi^3   xi^2  xi   1 |
	!       \3*xi^2  2*xi   1   0 /
	!   
	!     / a \
	!   A=| b |
	!     | c |
	!     \ d /
	!
	!     / f0 \
	!   B=| g0 |
	!     | f1 |
	!     \ g1 /
	!
	!then  A=(M^T*M)^(-1)*(M^T*B)
 	!*************************************************************

 	!LSM: least square method 

 	subroutine LSM(outA,Allx,Allf,Allg)
 		type(Tensor),intent(inout)::outA
 		real*8,intent(in)::Allx(:),Allf(:),Allg(:)
 		type(Tensor)::M,B
 		integer::len,i,sizeAllx
 		real*8,pointer::Mp(:,:),Bp(:)
 		sizeAllx=size(Allx)
 		len=sizeAllx*2
 		call outA%empty()
 		call M%allocate([len,4],'real*8')
 		call B%allocate([len],'real*8')
 		call M%pointer(Mp)
 		call B%pointer(Bp)
 		do i=1,sizeAllx
 			Mp(i,:)=[Allx(i)*Allx(i)*Allx(i),Allx(i)*Allx(i),Allx(i),1d0]
 			Bp(i)=Allf(i)
 		end do

 		do i=1,sizeAllx
 			Mp(sizeAllx+i,:)=[3d0*Allx(i)*Allx(i),2d0*Allx(i),1d0,0d0]
 			Bp(sizeAllx+i)=Allg(i)
 		end do

 		outA=(.inv.((.H.M)*M))* ((.H.M)*B)
 		return
 	end subroutine


 	!gx_root:
 		!g(x)=ax^2+bx+c 
		!find x of g(x)=0 and outx should be larger than 0
		!output x=min(x1,x2)

 	subroutine gx_root(outx,inA)
 		real*8,intent(inout)::outx
 		type(Tensor),intent(in)::inA
 		real*8::Delta,x1,x2,a,b,c,d
 		a=inA%di(1)
 		b=inA%di(2)
 		c=inA%di(3)
 		d=inA%di(4)
 		Delta=b*b-4d0*a*c
		if(Delta.lt.zero_number) then
			outx=-b/(2d0*a)
			return
		end if
		Delta=dsqrt(Delta)
		x1=(-b+Delta)/(2d0*a)
		x2=(-b-Delta)/(2d0*a)
		if(x1.le.x2)then
			outx=x1
			if(outx.le.0) outx=x2
		else
			outx=x2
			if(outx.le.0) outx=x1
		end if
 	end subroutine

 	subroutine runLSM(outx,Allx,Allf,Allg)
 		real*8,intent(in)::Allx(:),Allf(:),Allg(:)
 		real*8,intent(inout)::outx
 		type(Tensor)::A
 		call LSM(A,Allx,Allf,Allg)
 		call gx_root(outx,A)
 		return
 	end subroutine


end module
