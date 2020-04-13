!
!                   _ooOoo_
!                  o8888888o
!                  88" . "88
!                  (| -_- |)
!                  O\  =  /O
!               ____/`---'\____
!             .'  \\|     |//  `.
!            /  \\|||  :  |||//  \
!           /  _||||| -:- |||||-  \
!           |   | \\\  -  /// |   |
!           | \_|  ''\---/''  |   |
!           \  .-\__  `-`  ___/-. /
!         ___`. .'  /--.--\  `. . __
!      ."" '<  `.___\_<|>_/___.'  >'"".
!     | | :  `- \`.;`\ _ /`;.`/ - ` : | |
!     \  \ `-.   \_ __\ /__ _/   .-` /  /
!======`-.____`-.___\_____/___.-`____.-'======
!                   `=---='
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!       Buddha blessed , no BUG 
! Report bugs of the package to sj.dong@outlook.com


! DO NOT FINISHED the eigen value that near s0, when input the subroutine of H*H*phi
! This part finished in the version of eigvalue-3.1


module eigen_value
	use Tensor_type
	use Tools
	implicit none
	private

	


	public::eigenvalue
	type eigenvalue
		integer::maxitr=300
		integer::ncv=-1
		integer::maxnvc=100
		real(kind=8)::tol=0.0
		integer::print_num=0
		procedure(H_phi_subroutine),pointer,NOPASS::H_phi=>default_H_phi
		procedure(HH_phi_subroutine),pointer,NOPASS::HH_phi=>default_HH_phi
	contains
		procedure,public::set_maxnvc
		procedure,public::set_ncv
		procedure,public::set_maxitr
		procedure,public::set_Hphi
		procedure,public::set_HHphi
		procedure,public::set_print_num
		generic,public::set_tol=>set_tol1,set_tol2
		procedure::set_tol1,set_tol2
		procedure::eigen_all,eigen_max_min,eigen_max_min_phi
		procedure::eigen_ss0,eigen_ds0,eigen_cs0,eigen_zs0
		procedure::eigen_max_min_phi1_para_array,eigen_max_min_phi1_para_array2,eigen_max_min_phi1_parainfo_array
		generic,public::eig =>eigen_all,eigen_max_min,eigen_max_min_phi,&
		                      eigen_ss0,eigen_ds0,eigen_cs0,eigen_zs0,&
		                      eigen_max_min_phi1_para_array,eigen_max_min_phi1_para_array2,eigen_max_min_phi1_parainfo_array

	end type eigenvalue

	interface
		 subroutine H_phi_subroutine(parameter,phi)
		 	import Tensor
			type(Tensor)::parameter(:)
			type(Tensor)::phi
		end subroutine H_phi_subroutine
	end interface

	interface
		 subroutine HH_phi_subroutine(s0,parameter,phi)
		 	import Tensor
		 	real*8::s0
			type(Tensor)::parameter(:)
			type(Tensor)::phi
		end subroutine HH_phi_subroutine
	end interface


	interface eigenvalue
		procedure constructor1
		procedure constructor2
		procedure constructor3
		procedure constructor4
	end interface

	real*8::print_time1,print_time2
	integer::print_step=0
	integer::print_per_step=0


contains
	subroutine reset_print_time(num)
		integer,intent(in)::num
		print_per_step=num
		call cpu_time(print_time1)
		print_step=0
	end subroutine

	subroutine check_time()
		character(len=100)::chartime,systemtime
		integer::values(8)
		print_step=print_step+1
		if(mod(print_step,print_per_step).eq.0)then
			call cpu_time(print_time2)
			call system_time(print_time2-print_time1,chartime) 
			call date_and_time(VALUES=values)
			systemtime=values(1)+'-'+values(2)+'-'+values(3)+'-'+values(5)+':'+values(6)+':'+values(7)
			call writemess(systemtime+'. Step='+print_step+'. Running time='+chartime,'-')
		end if
	end subroutine

	subroutine set_print_num(eig,num)
		class(eigenvalue),intent(inout)::eig
		integer,intent(in)::num
		eig%print_num=num
		return
	end subroutine
	subroutine default_H_phi(parameter,phi)
		type(Tensor)::parameter(:)
		type(Tensor)::phi
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call writemess('@@@                 ERROR                   @@@  ')
		call writemess('@@@ DO NOT set the subroutine of phi=H*phi  @@@  ')
		call writemess('@@@                                         @@@  ')
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call error_stop
	end subroutine


	subroutine default_HH_phi(s0,parameter,phi)
		real*8::s0
		type(Tensor)::parameter(:)
		type(Tensor)::phi
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call writemess('@@@                 ERROR                     @@@  ')
		call writemess('@@@ DO NOT set the subroutine of phi=H*H*phi  @@@  ')
		call writemess('@@@                                           @@@  ')
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call error_stop
		return
	end subroutine

	subroutine set_Hphi(eig,Hphi)
		class(eigenvalue),intent(inout)::eig
		procedure(H_phi_subroutine)::Hphi
		eig%H_phi=>Hphi
		return
	end subroutine

	subroutine set_HHphi(eig,HHphi)
		class(eigenvalue),intent(inout)::eig
		procedure(HH_phi_subroutine)::HHphi
		eig%HH_phi=>HHphi
		return
	end subroutine


	type(eigenvalue) function constructor1(tol,ncv,maxitr,maxnvc)result(Res)
		integer,intent(in)::ncv,maxitr,maxnvc
		class(*),intent(in)::tol
		select type(tol)
			type is (integer)
				Res%tol=tol
			type is (real(kind=4))
				Res%tol=tol
			type is (real(kind=8))
				Res%tol=tol
		end select
		Res%ncv=ncv
		Res%maxitr=maxitr
		Res%maxnvc=maxnvc
		return
	end function
	type(eigenvalue) function constructor2(tol,ncv,maxitr)result(Res)
		integer,intent(in)::ncv,maxitr
		class(*),intent(in)::tol
		select type(tol)
			type is (integer)
				Res%tol=tol
			type is (real(kind=4))
				Res%tol=tol
			type is (real(kind=8))
				Res%tol=tol
		end select
		Res%ncv=ncv
		Res%maxitr=maxitr
		return
	end function
	type(eigenvalue) function constructor3(tol,ncv)result(Res)
		integer,intent(in)::ncv
		class(*),intent(in)::tol
		select type(tol)
			type is (integer)
				Res%tol=tol
			type is (real(kind=4))
				Res%tol=tol
			type is (real(kind=8))
				Res%tol=tol
		end select
		Res%ncv=ncv
		return
	end function
	type(eigenvalue) function constructor4(tol)result(Res)
		class(*),intent(in)::tol
		select type(tol)
			type is (integer)
				Res%tol=tol
			type is (real(kind=4))
				Res%tol=tol
			type is (real(kind=8))
				Res%tol=tol
		end select
		return
	end function
	subroutine set_maxnvc(eiger,maxnvc)
		class(eigenvalue),intent(inout)::eiger
		integer,intent(in)::maxnvc
		eiger%maxnvc=maxnvc
		return
	end subroutine
	subroutine set_ncv(eiger,ncv)
		class(eigenvalue),intent(inout)::eiger
		integer,intent(in)::ncv
		eiger%ncv=ncv
		return
	end subroutine
	subroutine set_maxitr(eiger,maxitr)
		class(eigenvalue),intent(inout)::eiger
		integer,intent(in)::maxitr
		eiger%maxitr=maxitr
		return
	end subroutine
	subroutine set_tol1(eiger,tol)
		class(eigenvalue),intent(inout)::eiger
		real(kind=8),intent(in)::tol
		eiger%tol=tol
		return
	end subroutine
	subroutine set_tol2(eiger,tol)
		class(eigenvalue),intent(inout)::eiger
		real(kind=4),intent(in)::tol
		eiger%tol=tol
		return
	end subroutine


	function eigen_all(eiger,H,outvex) result(res)
		class(eigenvalue),intent(in)::eiger
		type(Tensor),allocatable::res(:)
		type(Tensor),intent(in) ::H
		logical,optional,intent(in)::outvex
		if(present(outvex).and.outvex)then
			allocate(res(2))
			call H%eigRoutine(res(1),res(2))
		else
			allocate(res(1))
			call H%eigRoutine(res(1))
		end if
		return
	end function

	!********************************************************
		!		the max of min eigen value
		!		on output,a Tensor link,link:[eigenvalue]->[eigenvector]
		!		link.i.2 is a matrix of eigenvector , and the columns of which is the approximate 
		!	eigenvectors (Ritz vectors) corresponding eigenvalue
		!	ncv number of lonzcos vector	
		!	if outvector=.false. no eigenvectors
		!				WHICH   Character*2.  (INPUT)
		!          'LM' -> want the NEV eigenvalues of largest magnitude.
		!          'SM' -> want the NEV eigenvalues of smallest magnitude.
		!          'LR' -> want the NEV eigenvalues of largest real part.
		!          'SR' -> want the NEV eigenvalues of smallest real part.
		!          'LI' -> want the NEV eigenvalues of largest imaginary part.
		!          'SI' -> want the NEV eigenvalues of smallest imaginary part.


	!********************************************************
	!********************************************************
	!        input the matrix
	!********************************************************
	!********************************************************

	function eigen_max_min(eiger,T,WHICH,num_of_eig,outvector)result(Res)
		type(Tensor),allocatable::Res(:)
		class(eigenvalue),intent(in)::eiger
		type(Tensor),intent(in)::T
		Character*2,intent(in)::WHICH
		logical,optional,intent(in)::outvector
		integer,intent(in)::num_of_eig
		integer::ncv,n
		if(T%getRank().ne.2) then
			write(*,*)"ERROR in dimension of input Tensor,eigen"
			write(*,*)"stop"
			stop
		end if
		n=T.dim.1
		if(eiger%ncv.le.0)then
			ncv=min((2+num_of_eig+n)/2,eiger%maxnvc)
		else
			ncv=eiger%ncv
		end if
		if(n.ne.(T.dim.2)) then
		   write(*,*)"ERROR in dimension of input Tensor,eigen"
		   write(*,*)"Should be a matrix of n * n"
			write(*,*)"stop"
			stop
		end if
		select case(T%getType())
			case(5)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_max_min1_com8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,WHICH,T,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigen_max_min1_com8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,WHICH,T,Res(1)) 
				end if
			case(4)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_max_min1_com4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,WHICH,T,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigen_max_min1_com4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,WHICH,T,Res(1)) 
				end if
			case(3)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_max_min1_real8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,WHICH,T,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigen_max_min1_real8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,WHICH,T,Res(1)) 
				end if
			case(2)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_max_min1_real4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,WHICH,T,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigen_max_min1_real4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,WHICH,T,Res(1)) 
				end if
			case default
				write(*,*)"Do not finished the part of integer"
				stop
		end select
		return
	end function
	subroutine eigen_max_min1_com8(print_num,n,num_of_eig,ncv,tol,maxitr,WHICH,T,eigval,eigvec) 
		integer,intent(in)::n,num_of_eig,ncv,maxitr,print_num
		real(kind=8),intent(in)::tol
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::T
		type(Tensor),intent(inout)::eigval
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		complex(kind=8),allocatable::v(:,:),resid(:),d(:)
		complex(kind=8),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		logical::continu
		real(kind=8),allocatable::rwork(:)
		logical,allocatable::selework(:)
		complex(kind=8)::s0
		integer::i,j,lworkl
		complex(kind=8),allocatable::Tdatacom8(:,:)
		outvector=present(eigvec)
		call copyTensor(Tdatacom8,T)
		lworkl = 3*ncv**2+5*ncv
		allocate(workd(3*n))
		allocate(workev(2*ncv))
		allocate(workl(lworkl))
		allocate(rwork(ncv))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(d(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.
		
		if(print_num.gt.0) call reset_print_time(print_num)
		do while(continu)
			call znaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,rwork,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call ZGEMV('N',n,n,dcmplx(1d0,0d0),Tdatacom8,n,workd(ipntr(1):ipntr(1)+n-1),&
						1,dcmplx(0d0,0d0),workd(ipntr(2):ipntr(2)+n-1),1)
			else
				continu=.false.
			end if
			if(print_num.gt.0) call check_time()
		end do

		if(print_num.gt.0) call writemess('')

		if(info.ne.0)then
			write(*,*)"znaupd do not success"
			write(*,*)info
			stop
		end if
		s0=0
		call zneupd(outvector,'A',selework,d,v,n,s0,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,rwork,info)
		if(info.ne.0)then
			write(*,*)"zneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=d(1:num_of_eig)
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	subroutine eigen_max_min1_com4(print_num,n,num_of_eig,ncv,tol,maxitr,WHICH,T,eigval,eigvec) 
		integer,intent(in)::n,num_of_eig,ncv,maxitr,print_num
		real(kind=4),intent(in)::tol
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::T
		type(Tensor),intent(inout)::eigval
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		complex(kind=4),allocatable::v(:,:),resid(:),d(:)
		complex(kind=4),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		integer,allocatable::ipiv(:)
		logical::continu
		real(kind=4),allocatable::rwork(:)
		logical,allocatable::selework(:)
		complex(kind=4)::s0
		integer::i,j,lworkl
		complex(kind=4),allocatable::Tdatacom4(:,:)
		outvector=present(eigvec)
		call copyTensor(Tdatacom4,T)
		lworkl = 3*ncv**2+5*ncv
		allocate(workd(3*n))
		allocate(workev(2*ncv))
		allocate(workl(lworkl))
		allocate(ipiv(n))
		allocate(rwork(ncv))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(d(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.
		
		if(print_num.gt.0) call reset_print_time(print_num)

		do while(continu)
			call cnaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,rwork,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call CGEMV('N',n,n,cmplx(1,kind=4),Tdatacom4,n,workd(ipntr(1):ipntr(1)+n-1),&
						1,cmplx(0,kind=4),workd(ipntr(2):ipntr(2)+n-1),1)
			else
				continu=.false.
			end if
			if(print_num.gt.0) call check_time()
		end do

		if(print_num.gt.0) call writemess('')

		if(info.ne.0)then
			write(*,*)"cnaupd do not success"
			write(*,*)info
			stop
		end if
		s0=0
		call cneupd(outvector,'A',selework,d,v,n,s0,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,rwork,info)
		if(info.ne.0)then
			write(*,*)"cneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=d(1:num_of_eig)
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	
	subroutine eigen_max_min1_real8(print_num,n,num_of_eig,ncv,tol,maxitr,WHICH,T,eigval,eigvec) 
		integer,intent(in)::n,num_of_eig,ncv,maxitr,print_num
		real(kind=8),intent(in)::tol
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::T
		type(Tensor),intent(inout)::eigval
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		real(kind=8),allocatable::v(:,:),resid(:),dr(:),di(:)
		real(kind=8),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		logical::continu
		real(kind=8),allocatable::rwork(:)
		logical,allocatable::selework(:)
		real(kind=8)::s0r,s0i
		integer::i,j,lworkl
		real(kind=8),allocatable::Tdatareal8(:,:)
		outvector=present(eigvec)
		call copyTensor(Tdatareal8,T)
		lworkl = 3*ncv**2+6*ncv
		allocate(workd(3*n))
		allocate(workev(3*ncv))
		allocate(workl(lworkl))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(dr(num_of_eig+1))
		allocate(di(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.

		if(print_num.gt.0) call reset_print_time(print_num)

		do while(continu)
			call dnaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call DGEMV('N',n,n,1d0,Tdatareal8,n,workd(ipntr(1):ipntr(1)+n-1),&
						1,0d0,workd(ipntr(2):ipntr(2)+n-1),1)
			else
				continu=.false.
			end if
			if(print_num.gt.0) call check_time()
		end do

		if(print_num.gt.0) call writemess('')

		if(info.ne.0)then
			write(*,*)"dnaupd do not success"
			write(*,*)info
			stop
		end if
		s0r=0
		s0i=0
		call dneupd(outvector,'A',selework,dr,di,v,n,s0r,s0i,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,info)
		if(info.ne.0)then
			write(*,*)"dneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=dcmplx(dr(1:num_of_eig),di(1:num_of_eig))
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	subroutine eigen_max_min1_real4(print_num,n,num_of_eig,ncv,tol,maxitr,WHICH,T,eigval,eigvec) 
		integer,intent(in)::n,num_of_eig,ncv,maxitr,print_num
		real(kind=4),intent(in)::tol
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::T
		type(Tensor),intent(inout)::eigval
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		real(kind=4),allocatable::v(:,:),resid(:),dr(:),di(:)
		real(kind=4),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		logical::continu
		real(kind=4),allocatable::rwork(:)
		logical,allocatable::selework(:)
		real(kind=4)::s0r,s0i
		integer::i,j,lworkl
		real(kind=4),allocatable::Tdatareal4(:,:)
		outvector=present(eigvec)
		call copyTensor(Tdatareal4,T)
		lworkl = 3*ncv**2+6*ncv
		allocate(workd(3*n))
		allocate(workev(3*ncv))
		allocate(workl(lworkl))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(dr(num_of_eig+1))
		allocate(di(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.

		if(print_num.gt.0) call reset_print_time(print_num)

		do while(continu)
			call snaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call SGEMV('N',n,n,1.,Tdatareal4,n,workd(ipntr(1):ipntr(1)+n-1),&
						1,0.,workd(ipntr(2):ipntr(2)+n-1),1)
			else
				continu=.false.
			end if

			if(print_num.gt.0) call check_time()

		end do

		if(print_num.gt.0) call writemess('')

		if(info.ne.0)then
			write(*,*)"snaupd do not success"
			write(*,*)info
			stop
		end if
		s0r=0
		s0i=0
		call sneupd(outvector,'A',selework,dr,di,v,n,s0r,s0i,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,info)
		if(info.ne.0)then
			write(*,*)"sneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=cmplx(dr(1:num_of_eig),di(1:num_of_eig),kind=4)
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine


	!********************************************************
	!********************************************************
	!        input the subroutine of H_phi
	!********************************************************
	!********************************************************

	function eigen_max_min_phi(eiger,parameter,WHICH,num_of_eig,outvector)result(Res)
		type(Tensor),allocatable::Res(:)
		type(Tensor),intent(inout)::parameter(:)
		class(eigenvalue),intent(in)::eiger
		Character*2,intent(in)::WHICH
		logical,optional,intent(in)::outvector
		integer,intent(in)::num_of_eig
		integer::ncv,n
		type(Tensor)::T
		call eiger%H_phi(parameter,T)
		n=T%getTotalData()
		if(eiger%ncv.le.0)then
			ncv=min((2+num_of_eig+n)/2,eiger%maxnvc)
		else
			ncv=eiger%ncv
		end if
		select case(T%getType())
			case(5)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_com8(eiger,parameter,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_com8(eiger,parameter,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1)) 
				end if
			case(4)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_com4(eiger,parameter,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_com4(eiger,parameter,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,Res(1))  
				end if
			case(3)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_real8(eiger,parameter,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_real8(eiger,parameter,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1))  
				end if
			case(2)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_real4(eiger,parameter,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_real4(eiger,parameter,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,Res(1))  
				end if
			case default
				write(*,*)"Do not finished the part of integer"
				stop
		end select
		return
	end function

	subroutine eigenvalue_H_phi_com8(eiger,parameter,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		type(Tensor),intent(inout)::parameter(:)
		type(eigenvalue),intent(in)::eiger
		Character*2,intent(in)::WHICH
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=8),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		complex(kind=8),allocatable::v(:,:),resid(:),d(:)
		complex(kind=8),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		logical::continu
		real(kind=8),allocatable::rwork(:)
		logical,allocatable::selework(:)
		complex(kind=8)::s0
		integer::i,j,lworkl,print_num
		print_num=eiger%print_num
		outvector=present(eigvec)
     	lworkl = 3*ncv**2+5*ncv
		allocate(workd(3*n))
		allocate(workev(2*ncv))
		allocate(workl(lworkl))
		allocate(rwork(ncv))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(d(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.


		if(print_num.gt.0) call reset_print_time(print_num)

		do while(continu)
			call znaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,rwork,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call eiger%H_phi(parameter,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if

			if(print_num.gt.0) call check_time()

		end do

		if(print_num.gt.0) call writemess('')

		if(info.ne.0)then
			write(*,*)"znaupd do not success"
			write(*,*)info
			stop
		end if
		s0=0
		call zneupd(outvector,'A',selework,d,v,n,s0,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,rwork,info)
		if(info.ne.0)then
			write(*,*)"zneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=d(1:num_of_eig)
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	
	subroutine eigenvalue_H_phi_com4(eiger,parameter,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		type(Tensor),intent(inout)::parameter(:)
		type(eigenvalue),intent(in)::eiger
		Character*2,intent(in)::WHICH
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=4),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		complex(kind=4),allocatable::v(:,:),resid(:),d(:)
		complex(kind=4),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		logical::continu
		real(kind=4),allocatable::rwork(:)
		logical,allocatable::selework(:)
		complex(kind=4)::s0
		integer::i,j,lworkl,print_num
		print_num=eiger%print_num
		outvector=present(eigvec)
      	lworkl = 3*ncv**2+5*ncv
		allocate(workd(3*n))
		allocate(workev(2*ncv))
		allocate(workl(lworkl))
		allocate(rwork(ncv))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(d(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.

		if(print_num.gt.0) call reset_print_time(print_num)

		do while(continu)
			call cnaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,rwork,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call eiger%H_phi(parameter,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if

			if(print_num.gt.0) call check_time()

		end do

		if(print_num.gt.0) call writemess('')


		if(info.ne.0)then
			write(*,*)"cnaupd do not success"
			write(*,*)info
			stop
		end if
		s0=0
		call cneupd(outvector,'A',selework,d,v,n,s0,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,rwork,info)
		if(info.ne.0)then
			write(*,*)"cneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=d(1:num_of_eig)
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	subroutine eigenvalue_H_phi_real8(eiger,parameter,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		type(Tensor),intent(inout)::parameter(:)
		type(eigenvalue),intent(in)::eiger
		Character*2,intent(in)::WHICH
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=8),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		real(kind=8),allocatable::v(:,:),resid(:),dr(:),di(:)
		real(kind=8),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		integer,allocatable::ipiv(:)
		logical::continu
		real(kind=8),allocatable::rwork(:)
		logical,allocatable::selework(:)
		real(kind=8)::s0r,s0i
		integer::i,j,lworkl,print_num
		print_num=eiger%print_num
		outvector=present(eigvec)
      	lworkl = 3*ncv**2+6*ncv
		allocate(workd(3*n))
		allocate(workev(3*ncv))
		allocate(workl(lworkl))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(dr(num_of_eig+1))
		allocate(di(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.

		if(print_num.gt.0)then
			call reset_print_time(print_num)
		end if

		do while(continu)
			call dnaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call eiger%H_phi(parameter,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if

			if(print_num.gt.0) call check_time()

		end do
		if(print_num.gt.0) call writemess('')

		if(info.ne.0)then
			write(*,*)"dnaupd do not success"
			write(*,*)info
			stop
		end if
		s0r=0
		s0i=0
		call dneupd(outvector,'A',selework,dr,di,v,n,s0r,s0i,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,info)
		if(info.ne.0)then
			write(*,*)"dneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=dcmplx(dr(1:num_of_eig),di(1:num_of_eig))
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	subroutine eigenvalue_H_phi_real4(eiger,parameter,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		type(Tensor),intent(inout)::parameter(:)
		type(eigenvalue),intent(in)::eiger
		Character*2,intent(in)::WHICH
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=4),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		real(kind=4),allocatable::v(:,:),resid(:),dr(:),di(:)
		real(kind=4),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		integer,allocatable::ipiv(:)
		logical::continu
		real(kind=4),allocatable::rwork(:)
		logical,allocatable::selework(:)
		real(kind=4)::s0r,s0i
		integer::i,j,lworkl,print_num
		print_num=eiger%print_num
		outvector=present(eigvec)
      	lworkl = 3*ncv**2+6*ncv
		allocate(workd(3*n))
		allocate(workev(3*ncv))
		allocate(workl(lworkl))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(dr(num_of_eig+1))
		allocate(di(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.

		if(print_num.gt.0) call reset_print_time(print_num)

		do while(continu)
			call snaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call eiger%H_phi(parameter,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if

			if(print_num.gt.0) call check_time()

		end do

		if(print_num.gt.0) call writemess('')


		if(info.ne.0)then
			write(*,*)"snaupd do not success"
			write(*,*)info
			stop
		end if
		s0r=0
		s0i=0
		call sneupd(outvector,'A',selework,dr,di,v,n,s0r,s0i,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,info)
		if(info.ne.0)then
			write(*,*)"sneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=cmplx(dr(1:num_of_eig),di(1:num_of_eig))
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine

	!********************************************************
	!********************************************************
	!		eigen value near some value, say s0
	!********************************************************
	!********************************************************



	function eigen_ss0(eiger,T,s0,num_of_eig,outvector)result(Res)
		type(Tensor),allocatable::Res(:)
		class(eigenvalue),intent(in)::eiger
		type(Tensor),intent(in)::T
		real*4,intent(in)::s0
		complex*16::zs0
		complex*8::cs0
		logical,optional,intent(in)::outvector
		integer,intent(in)::num_of_eig
		integer::ncv,n
		if(T%getRank().ne.2) then
			write(*,*)"ERROR in dimension of input Tensor,eigen"
			write(*,*)"stop"
			stop
		end if
		n=T.dim.1
		if(eiger%ncv.le.0)then
			ncv=min((2+num_of_eig+n)/2,eiger%maxnvc)
		else
			ncv=eiger%ncv
		end if
		if(n.ne.(T.dim.2)) then
		   write(*,*)"ERROR in dimension of input Tensor,eigen"
		   write(*,*)"Should be a matrix of n * n"
			write(*,*)"stop"
			stop
		end if
		zs0=s0
		cs0=s0
		select case(T%getType())
			case(5)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_s0_com8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,zs0,T,Res(1),Res(2))
				else
					allocate(Res(1))
					call eigen_s0_com8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,zs0,T,Res(1))
				end if
			case(4)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_s0_com4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,cs0,T,Res(1),Res(2))
				else
					allocate(Res(1))
					call eigen_s0_com4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,cs0,T,Res(1))
				end if
			case default
				write(*,*)"Do not finished the part of integer,real"
				stop
		end select
		return
	end function
	function eigen_ds0(eiger,T,s0,num_of_eig,outvector)result(Res)
		type(Tensor),allocatable::Res(:)
		class(eigenvalue),intent(in)::eiger
		type(Tensor),intent(in)::T
		real*8,intent(in)::s0
		complex*16::zs0
		complex*8::cs0
		logical,optional,intent(in)::outvector
		integer,intent(in)::num_of_eig
		integer::ncv,n
		if(T%getRank().ne.2) then
			write(*,*)"ERROR in dimension of input Tensor,eigen"
			write(*,*)"stop"
			stop
		end if
		n=T.dim.1
		if(eiger%ncv.le.0)then
			ncv=min((2+num_of_eig+n)/2,eiger%maxnvc)
		else
			ncv=eiger%ncv
		end if
		if(n.ne.(T.dim.2)) then
		   write(*,*)"ERROR in dimension of input Tensor,eigen"
		   write(*,*)"Should be a matrix of n * n"
			write(*,*)"stop"
			stop
		end if
		zs0=s0
		cs0=s0
		select case(T%getType())
			case(5)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_s0_com8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,zs0,T,Res(1),Res(2))
				else
					allocate(Res(1))
					call eigen_s0_com8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,zs0,T,Res(1))
				end if
			case(4)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_s0_com4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,cs0,T,Res(1),Res(2))
				else
					allocate(Res(1))
					call eigen_s0_com4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,cs0,T,Res(1))
				end if
			case default
				write(*,*)"Do not finished the part of integer,real"
				stop
		end select
		return
	end function
	function eigen_cs0(eiger,T,s0,num_of_eig,outvector)result(Res)
		type(Tensor),allocatable::Res(:)
		class(eigenvalue),intent(in)::eiger
		type(Tensor),intent(in)::T
		complex*8,intent(in)::s0
		complex*16::zs0
		complex*8::cs0
		logical,optional,intent(in)::outvector
		integer,intent(in)::num_of_eig
		integer::ncv,n
		if(T%getRank().ne.2) then
			write(*,*)"ERROR in dimension of input Tensor,eigen"
			write(*,*)"stop"
			stop
		end if
		n=T.dim.1
		if(eiger%ncv.le.0)then
			ncv=min((2+num_of_eig+n)/2,eiger%maxnvc)
		else
			ncv=eiger%ncv
		end if
		if(n.ne.(T.dim.2)) then
		   write(*,*)"ERROR in dimension of input Tensor,eigen"
		   write(*,*)"Should be a matrix of n * n"
			write(*,*)"stop"
			stop
		end if
		zs0=s0
		cs0=s0
		select case(T%getType())
			case(5)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_s0_com8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,zs0,T,Res(1),Res(2))
				else
					allocate(Res(1))
					call eigen_s0_com8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,zs0,T,Res(1))
				end if
			case(4)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_s0_com4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,cs0,T,Res(1),Res(2))
				else
					allocate(Res(1))
					call eigen_s0_com4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,cs0,T,Res(1))
				end if
			case default
				write(*,*)"Do not finished the part of integer,real"
				stop
		end select
		return
	end function
	function eigen_zs0(eiger,T,s0,num_of_eig,outvector)result(Res)
		type(Tensor),allocatable::Res(:)
		class(eigenvalue),intent(in)::eiger
		type(Tensor),intent(in)::T
		complex*16,intent(in)::s0
		complex*16::zs0
		complex*8::cs0
		logical,optional,intent(in)::outvector
		integer,intent(in)::num_of_eig
		integer::ncv,n
		if(T%getRank().ne.2) then
			write(*,*)"ERROR in dimension of input Tensor,eigen"
			write(*,*)"stop"
			stop
		end if
		n=T.dim.1
		if(eiger%ncv.le.0)then
			ncv=min((2+num_of_eig+n)/2,eiger%maxnvc)
		else
			ncv=eiger%ncv
		end if
		if(n.ne.(T.dim.2)) then
		   write(*,*)"ERROR in dimension of input Tensor,eigen"
		   write(*,*)"Should be a matrix of n * n"
			write(*,*)"stop"
			stop
		end if
		zs0=s0
		cs0=s0
		select case(T%getType())
			case(5)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_s0_com8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,zs0,T,Res(1),Res(2))
				else
					allocate(Res(1))
					call eigen_s0_com8(eiger%print_num,n,num_of_eig,ncv,eiger%tol,eiger%maxitr,zs0,T,Res(1))
				end if
			case(4)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigen_s0_com4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,cs0,T,Res(1),Res(2))
				else
					allocate(Res(1))
					call eigen_s0_com4(eiger%print_num,n,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,cs0,T,Res(1))
				end if
			case default
				write(*,*)"Do not finished the part of integer,real"
				stop
		end select
		return
	end function
	subroutine eigen_s0_com8(print_num,n,num_of_eig,ncv,tol,maxitr,s0,T,eigval,eigvec)
	 	type(Tensor),intent(in)::T
		type(Tensor),intent(inout)::eigval
		type(Tensor),optional,intent(inout)::eigvec
		complex*16,intent(in)::s0
		integer,intent(in)::num_of_eig,ncv,n,maxitr,print_num
		real*8,intent(in)::tol
		logical::outvector
		complex*16,allocatable::A(:),v(:,:),resid(:),d(:),Cdata(:),workdata(:)
		complex*16,allocatable::workd(:),workev(:),workl(:)
		
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		integer,allocatable::ipiv(:)
		logical::continu
		real*8,allocatable::rwork(:)
		logical,allocatable::selework(:)
		integer,allocatable::IPIV2(:)
		integer::icouter,info2
		complex*16,allocatable::C0(:,:),C(:,:)
		integer::i,j,lworkl,n2
		type(Tensornode),pointer::res_eigvalue,res_eigenvector
		outvector=(present(eigvec))
		n2=n*n
		allocate(C0(n,n))
		allocate(C(n,n))
		C0=T
		do i=1,n
			C0(i,i)=C0(i,i)-s0
		end do
		lworkl = 3*ncv**2+5*ncv
		allocate(workd(3*n))
		allocate(workev(2*ncv))
		allocate(workl(lworkl))
		allocate(ipiv(n))
		allocate(rwork(ncv))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(d(num_of_eig+1))
		allocate(IPIV2(n))
		allocate(workdata(n))
		allocate(selework(ncv))
		!tol    = 0.0 
		ido    = 0
		info   = 0
		info2=0
				
		ishfts = 1
		mode   = 3
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.

		if(print_num.gt.0) call reset_print_time(print_num)


		do while(continu)
			call znaupd(ido,'I',n,'LM',num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,rwork,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call zcopy ( n, workd(ipntr(1)),1, workd(ipntr(2)), 1)
				call zcopy ( n2, C0,1, C, 1)
				call ZGESV(n,1,C,n,IPIV2,workd(ipntr(2)),n,info2)
			else
				continu=.false.
			end if

			if(print_num.gt.0) call check_time()

		end do

		if(print_num.gt.0) call writemess('')

		if(info.ne.0)then
			write(*,*)"znaupd do not success"
			write(*,*)info
			stop
		end if
		call zneupd(outvector,'A',selework,d,v,n,s0,workev,'I',n,'LM',num_of_eig,tol,resid,ncv,v,n,iparam,&
					ipntr,workd,workl,lworkl,rwork,info)
		if(info.ne.0)then
			write(*,*)"zneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=d(1:num_of_eig)
		if(outvector) eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	subroutine eigen_s0_com4(print_num,n,num_of_eig,ncv,tol,maxitr,s0,T,eigval,eigvec)
	 	type(Tensor),intent(in)::T
		type(Tensor),intent(inout)::eigval
		type(Tensor),optional,intent(inout)::eigvec
		complex*8,intent(in)::s0
		integer,intent(in)::num_of_eig,ncv,n,maxitr,print_num
		real*4,intent(in)::tol
		logical::outvector
		complex*8,allocatable::A(:),v(:,:),resid(:),d(:),Cdata(:),workdata(:)
		complex*8,allocatable::workd(:),workev(:),workl(:)
		
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		integer,allocatable::ipiv(:)
		logical::continu
		real*4,allocatable::rwork(:)
		logical,allocatable::selework(:)
		integer,allocatable::IPIV2(:)
		integer::icouter,info2
		complex*8,allocatable::C0(:,:),C(:,:)
		integer::i,j,lworkl,n2
		type(Tensornode),pointer::res_eigvalue,res_eigenvector
		outvector=(present(eigvec))
		n2=n*n
		allocate(C0(n,n))
		allocate(C(n,n))
		C0=T
		do i=1,n
			C0(i,i)=C0(i,i)-s0
		end do
		lworkl = 3*ncv**2+5*ncv
		allocate(workd(3*n))
		allocate(workev(2*ncv))
		allocate(workl(lworkl))
		allocate(ipiv(n))
		allocate(rwork(ncv))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(d(num_of_eig+1))
		allocate(IPIV2(n))
		allocate(workdata(n))
		allocate(selework(ncv))
		!tol    = 0.0 
		ido    = 0
		info   = 0
		info2=0
				
		ishfts = 1
		mode   = 3
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.

		if(print_num.gt.0) call reset_print_time(print_num)


		do while(continu)
			call cnaupd(ido,'I',n,'LM',num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,rwork,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call ccopy ( n, workd(ipntr(1)),1, workd(ipntr(2)), 1)
				call ccopy ( n2, C0,1, C, 1)
				call CGESV(n,1,C,n,IPIV2,workd(ipntr(2)),n,info2)
			else
				continu=.false.
			end if

			if(print_num.gt.0) call check_time()

		end do

		if(print_num.gt.0) call writemess('')
		
		if(info.ne.0)then
			write(*,*)"cnaupd do not success"
			write(*,*)info
			stop
		end if
		call cneupd(outvector,'A',selework,d,v,n,s0,workev,'I',n,'LM',num_of_eig,tol,resid,ncv,v,n,iparam,&
					ipntr,workd,workl,lworkl,rwork,info)
		if(info.ne.0)then
			write(*,*)"cneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=d(1:num_of_eig)
		if(outvector) eigvec=v(:,1:num_of_eig)
		return
	end subroutine


	!********************************************************
	!********************************************************
	!		The old version funciton 
	!********************************************************
	!********************************************************
	
	function eigen_max_min_phi1_para_array(eiger,H_phi,para,WHICH,num_of_eig,outvector)result(Res)
		type(Tensor),allocatable::Res(:)
		class(eigenvalue),intent(in)::eiger
		type(Tensor),intent(in)::para(:)
		external::H_phi
		Character*2,intent(in)::WHICH
		logical,optional,intent(in)::outvector
		integer,intent(in)::num_of_eig
		integer::ncv,n
		type(Tensor)::T
		call H_phi(para,T)
		n=T%getTotalData()
		if(eiger%ncv.le.0)then
			ncv=min((2+num_of_eig+n)/2,eiger%maxnvc)
		else
			ncv=eiger%ncv
		end if
		select case(T%getType())
			case(5)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_para_com8(H_phi,para,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_para_com8(H_phi,para,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1)) 
				end if
			case(4)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_para_com4(H_phi,para,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_para_com4(H_phi,para,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,Res(1))  
				end if
			case(3)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_para_real8(H_phi,para,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_para_real8(H_phi,para,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1))  
				end if
			case(2)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_para_real4(H_phi,para,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_para_real4(H_phi,para,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,Res(1))  
				end if
			case default
				write(*,*)"Do not finished the part of integer"
				stop
		end select
		return
	end function
	function eigen_max_min_phi1_para_array2(eiger,H_phi,para,WHICH,num_of_eig,outvector)result(Res)
		type(Tensor),allocatable::Res(:)
		class(eigenvalue),intent(in)::eiger
		type(Tensor),intent(in)::para
		external::H_phi
		Character*2,intent(in)::WHICH
		logical,optional,intent(in)::outvector
		integer,intent(in)::num_of_eig
		integer::ncv,n
		type(Tensor)::T
		call H_phi(para,T)
		n=T%getTotalData()
		if(eiger%ncv.le.0)then
			ncv=min((2+num_of_eig+n)/2,eiger%maxnvc)
		else
			ncv=eiger%ncv
		end if
		select case(T%getType())
			case(5)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_para_com8(H_phi,(/para/),T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_para_com8(H_phi,(/para/),T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1)) 
				end if
			case(4)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_para_com4(H_phi,(/para/),T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_para_com4(H_phi,(/para/),T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,Res(1))  
				end if
			case(3)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_para_real8(H_phi,(/para/),T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_para_real8(H_phi,(/para/),T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1))  
				end if
			case(2)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_para_real4(H_phi,(/para/),T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_para_real4(H_phi,(/para/),T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,Res(1))  
				end if
			case default
				write(*,*)"Do not finished the part of integer"
				stop
		end select
		return
	end function
	
	function eigen_max_min_phi1_parainfo_array(eiger,H_phi,para,parainfo,WHICH,num_of_eig,outvector)result(Res)
		type(Tensor),allocatable::Res(:)
		class(eigenvalue),intent(in)::eiger
		type(Tensor),intent(in)::para(:),parainfo
		external::H_phi
		Character*2,intent(in)::WHICH
		logical,optional,intent(in)::outvector
		integer,intent(in)::num_of_eig
		integer::ncv,n
		type(Tensor)::T
		call H_phi(para,parainfo,T)
		n=T%getTotalData()
		if(eiger%ncv.le.0)then
			ncv=min((2+num_of_eig+n)/2,eiger%maxnvc)
		else
			ncv=eiger%ncv
		end if
		select case(T%getType())
			case(5)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_parainfo_com8(H_phi,para,parainfo,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_parainfo_com8(H_phi,para,parainfo,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1)) 
				end if
			case(4)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_parainfo_com4(H_phi,para,parainfo,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_parainfo_com4(H_phi,para,parainfo,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,Res(1))  
				end if
			case(3)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_parainfo_real8(H_phi,para,parainfo,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_parainfo_real8(H_phi,para,parainfo,T,n,WHICH,num_of_eig,ncv,eiger%tol,eiger%maxitr,Res(1))  
				end if
			case(2)
				if(present(outvector).and.outvector)then
					allocate(Res(2))
					call eigenvalue_H_phi_parainfo_real4(H_phi,para,parainfo,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr&
																	,Res(1),Res(2)) 
				else
					allocate(Res(1))
					call eigenvalue_H_phi_parainfo_real4(H_phi,para,parainfo,T,n,WHICH,num_of_eig,ncv,real(eiger%tol,kind=4),eiger%maxitr,Res(1))  
				end if
			case default
				write(*,*)"Do not finished the part of integer"
				stop
		end select
		return
	end function
	
	subroutine eigenvalue_H_phi_para_com8(H_phi,para,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		external::H_phi
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::para(*)
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=8),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		complex(kind=8),allocatable::v(:,:),resid(:),d(:)
		complex(kind=8),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		logical::continu
		real(kind=8),allocatable::rwork(:)
		logical,allocatable::selework(:)
		complex(kind=8)::s0
		integer::i,j,lworkl
		outvector=present(eigvec)
      lworkl = 3*ncv**2+5*ncv
		allocate(workd(3*n))
		allocate(workev(2*ncv))
		allocate(workl(lworkl))
		allocate(rwork(ncv))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(d(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.
		do while(continu)
			call znaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,rwork,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call H_phi(para,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if
		end do
		if(info.ne.0)then
			write(*,*)"znaupd do not success"
			write(*,*)info
			stop
		end if
		s0=0
		call zneupd(outvector,'A',selework,d,v,n,s0,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,rwork,info)
		if(info.ne.0)then
			write(*,*)"zneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=d(1:num_of_eig)
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	subroutine eigenvalue_H_phi_parainfo_com8(H_phi,para,parainfo,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		external::H_phi
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::para(*),parainfo
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=8),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		complex(kind=8),allocatable::v(:,:),resid(:),d(:)
		complex(kind=8),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		logical::continu
		real(kind=8),allocatable::rwork(:)
		logical,allocatable::selework(:)
		complex(kind=8)::s0
		integer::i,j,lworkl
		outvector=present(eigvec)
      lworkl = 3*ncv**2+5*ncv
		allocate(workd(3*n))
		allocate(workev(2*ncv))
		allocate(workl(lworkl))
		allocate(rwork(ncv))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(d(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.
		do while(continu)
			call znaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,rwork,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call H_phi(para,parainfo,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if
		end do
		if(info.ne.0)then
			write(*,*)"znaupd do not success"
			write(*,*)info
			stop
		end if
		s0=0
		call zneupd(outvector,'A',selework,d,v,n,s0,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,rwork,info)
		if(info.ne.0)then
			write(*,*)"zneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=d(1:num_of_eig)
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	subroutine eigenvalue_H_phi_para_com4(H_phi,para,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		external::H_phi
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::para(*)
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=4),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		complex(kind=4),allocatable::v(:,:),resid(:),d(:)
		complex(kind=4),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		logical::continu
		real(kind=4),allocatable::rwork(:)
		logical,allocatable::selework(:)
		complex(kind=4)::s0
		integer::i,j,lworkl
		outvector=present(eigvec)
      lworkl = 3*ncv**2+5*ncv
		allocate(workd(3*n))
		allocate(workev(2*ncv))
		allocate(workl(lworkl))
		allocate(rwork(ncv))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(d(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.
		do while(continu)
			call cnaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,rwork,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call H_phi(para,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if
		end do
		if(info.ne.0)then
			write(*,*)"cnaupd do not success"
			write(*,*)info
			stop
		end if
		s0=0
		call cneupd(outvector,'A',selework,d,v,n,s0,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,rwork,info)
		if(info.ne.0)then
			write(*,*)"cneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=d(1:num_of_eig)
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	
	subroutine eigenvalue_H_phi_parainfo_com4(H_phi,para,parainfo,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		external::H_phi
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::para(*),parainfo
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=4),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		complex(kind=4),allocatable::v(:,:),resid(:),d(:)
		complex(kind=4),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		logical::continu
		real(kind=4),allocatable::rwork(:)
		logical,allocatable::selework(:)
		complex(kind=4)::s0
		integer::i,j,lworkl
		outvector=present(eigvec)
      lworkl = 3*ncv**2+5*ncv
		allocate(workd(3*n))
		allocate(workev(2*ncv))
		allocate(workl(lworkl))
		allocate(rwork(ncv))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(d(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.
		do while(continu)
			call cnaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,rwork,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call H_phi(para,parainfo,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if
		end do
		if(info.ne.0)then
			write(*,*)"cnaupd do not success"
			write(*,*)info
			stop
		end if
		s0=0
		call cneupd(outvector,'A',selework,d,v,n,s0,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,rwork,info)
		if(info.ne.0)then
			write(*,*)"cneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=d(1:num_of_eig)
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	
	subroutine eigenvalue_H_phi_para_real8(H_phi,para,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		external::H_phi
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::para(*)
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=8),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		real(kind=8),allocatable::v(:,:),resid(:),dr(:),di(:)
		real(kind=8),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		integer,allocatable::ipiv(:)
		logical::continu
		real(kind=8),allocatable::rwork(:)
		logical,allocatable::selework(:)
		real(kind=8)::s0r,s0i
		integer::i,j,lworkl
		outvector=present(eigvec)
      lworkl = 3*ncv**2+6*ncv
		allocate(workd(3*n))
		allocate(workev(3*ncv))
		allocate(workl(lworkl))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(dr(num_of_eig+1))
		allocate(di(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.
		do while(continu)
			call dnaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call H_phi(para,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if
		end do
		if(info.ne.0)then
			write(*,*)"dnaupd do not success"
			write(*,*)info
			stop
		end if
		s0r=0
		s0i=0
		call dneupd(outvector,'A',selework,dr,di,v,n,s0r,s0i,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,info)
		if(info.ne.0)then
			write(*,*)"dneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=dcmplx(dr(1:num_of_eig),di(1:num_of_eig))
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	
	subroutine eigenvalue_H_phi_parainfo_real8(H_phi,para,parainfo,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		external::H_phi
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::para(*),parainfo
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=8),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		real(kind=8),allocatable::v(:,:),resid(:),dr(:),di(:)
		real(kind=8),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		integer,allocatable::ipiv(:)
		logical::continu
		real(kind=8),allocatable::rwork(:)
		logical,allocatable::selework(:)
		real(kind=8)::s0r,s0i
		integer::i,j,lworkl
		outvector=present(eigvec)
      lworkl = 3*ncv**2+6*ncv
		allocate(workd(3*n))
		allocate(workev(3*ncv))
		allocate(workl(lworkl))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(dr(num_of_eig+1))
		allocate(di(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.
		do while(continu)
			call dnaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call H_phi(para,parainfo,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if
		end do
		if(info.ne.0)then
			write(*,*)"dnaupd do not success"
			write(*,*)info
			stop
		end if
		s0r=0
		s0i=0
		call dneupd(outvector,'A',selework,dr,di,v,n,s0r,s0i,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,info)
		if(info.ne.0)then
			write(*,*)"dneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=dcmplx(dr(1:num_of_eig),di(1:num_of_eig))
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	
	subroutine eigenvalue_H_phi_para_real4(H_phi,para,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		external::H_phi
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::para(*)
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=4),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		real(kind=4),allocatable::v(:,:),resid(:),dr(:),di(:)
		real(kind=4),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		integer,allocatable::ipiv(:)
		logical::continu
		real(kind=4),allocatable::rwork(:)
		logical,allocatable::selework(:)
		real(kind=4)::s0r,s0i
		integer::i,j,lworkl
		outvector=present(eigvec)
      lworkl = 3*ncv**2+6*ncv
		allocate(workd(3*n))
		allocate(workev(3*ncv))
		allocate(workl(lworkl))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(dr(num_of_eig+1))
		allocate(di(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.
		do while(continu)
			call snaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call H_phi(para,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if
		end do
		if(info.ne.0)then
			write(*,*)"snaupd do not success"
			write(*,*)info
			stop
		end if
		s0r=0
		s0i=0
		call sneupd(outvector,'A',selework,dr,di,v,n,s0r,s0i,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,info)
		if(info.ne.0)then
			write(*,*)"sneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=cmplx(dr(1:num_of_eig),di(1:num_of_eig))
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine
	
	subroutine eigenvalue_H_phi_parainfo_real4(H_phi,para,parainfo,phi,n,WHICH,num_of_eig,ncv,tol,maxitr,eigval,eigvec) 
		external::H_phi
		Character*2,intent(in)::WHICH
		type(Tensor),intent(in)::para(*),parainfo
		integer,intent(in)::num_of_eig,ncv,maxitr,n
		real(kind=4),intent(in)::tol
		type(Tensor),intent(inout)::eigval,phi
		type(Tensor),optional,intent(inout)::eigvec
		logical::outvector
		real(kind=4),allocatable::v(:,:),resid(:),dr(:),di(:)
		real(kind=4),allocatable::workd(:),workev(:),workl(:)
		integer::ido,info,ishfts,mode,iparam(11), ipntr(14)
		integer,allocatable::ipiv(:)
		logical::continu
		real(kind=4),allocatable::rwork(:)
		logical,allocatable::selework(:)
		real(kind=4)::s0r,s0i
		integer::i,j,lworkl
		outvector=present(eigvec)
      lworkl = 3*ncv**2+6*ncv
		allocate(workd(3*n))
		allocate(workev(3*ncv))
		allocate(workl(lworkl))
		allocate(v(n,ncv))
		allocate(resid(n))
		allocate(dr(num_of_eig+1))
		allocate(di(num_of_eig+1))
		allocate(selework(ncv))
		ido    = 0
		info   = 0
		ishfts = 1
		mode   = 1
		iparam(1) = ishfts 
		iparam(3) = maxitr 
		iparam(7) = mode 	 
		continu=.true.
		do while(continu)
			call snaupd(ido,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,lworkl,info)
			if (ido .eq. -1 .or. ido .eq. 1 ) then
				call phi%setValue(workd(ipntr(1):ipntr(1)+n-1))
				call H_phi(para,parainfo,phi)
				workd(ipntr(2):)=phi
			else
				continu=.false.
			end if
		end do
		if(info.ne.0)then
			write(*,*)"snaupd do not success"
			write(*,*)info
			stop
		end if
		s0r=0
		s0i=0
		call sneupd(outvector,'A',selework,dr,di,v,n,s0r,s0i,workev,'I',n,WHICH,num_of_eig,tol,resid,ncv,v,n,iparam,&
						ipntr,workd,workl,lworkl,info)
		if(info.ne.0)then
			write(*,*)"sneupd do not success"
			write(*,*)info
			stop
		end if
		eigval=cmplx(dr(1:num_of_eig),di(1:num_of_eig))
		if(outvector)	eigvec=v(:,1:num_of_eig)
		return
	end subroutine

end module








