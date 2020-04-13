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

module ParityTool
	use QuantumNumber_Type
	use SymDimension_typede
	use Tools
	use Tensor_type
	implicit none
	private



	
	public::reverseParityRule
	public::ParityNonZeroElement
	public::PRule4
	public::ParityfuseOrder
	public::ifParityIndex4
	public::PParity1
	public::checkParityRule

	public::ParityRule
	interface ParityRule
		module procedure PRule1
		module procedure PRule2
		module procedure PRule3
	end interface


	public::ParityQuantumNumber
	interface ParityQuantumNumber
		module procedure QuantumNumberParity
		module procedure QuantumNumberParity2
		module procedure QuantumNumberParity3
		module procedure QuantumNumberParity4
	end interface

	public::PifParity
	interface PifParity
		module procedure ifParityIndex1
		module procedure ifParityIndex2
		module procedure ifParityIndex3
	end interface

	public::PQuanNumParity
	interface PQuanNumParity
		module procedure PParity2
	end interface

	
	
	
	
contains
	
	
	logical function PRule1(Q1,Q2,Q3,R1,R2,R3)
		real*4,intent(in)::Q1,Q2,Q3
		integer,optional,intent(in)::R1,R2,R3
		if(Q1.le.0)then
			PRule1=(Q2*Q3).le.0
		else
			PRule1=(Q2*Q3).gt.0
		end if
		return
	end function
	logical function PRule2(Q,R)
		real*4,intent(in)::Q(:)
		integer,optional,intent(in)::R(:)
		real*4::temp
		integer::i
		temp=Q(1)
		do i=2,size(Q)
			temp=temp*Q(i)
		end do
		PRule2=temp.gt.0
		return
	end function
	
	logical function PRule3(dimen,indices)
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::indices(:)
		real*4::Q(size(indices))
		integer::R(size(indices))
		integer::i
		real*4::temp
		do i=1,size(indices)
			Q(i)=dimen%getQN(i,indices(i))
		end do
		temp=Q(1)
		do i=2,size(Q)
			temp=temp*Q(i)
		end do
		PRule3=temp.gt.0
		return
	end function

	 subroutine PRule4(Res,dimen,indices,length)
	 	logical,intent(inout)::Res
		integer,intent(in)::length
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::indices(length)
		real*4::Q(length)
		integer::R(length)
		integer::i
		real*4::temp
		do i=1,size(indices)
			Q(i)=dimen%getQN(i,indices(i))
		end do
		temp=Q(1)
		do i=2,size(Q)
			temp=temp*Q(i)
		end do
		Res=temp.gt.0
		return
	end subroutine


	type(QuanNum) function QuantumNumberParity(deg)result(Parity)
		integer,intent(in)::deg(2)
		call Parity%setRule(0)
		call Parity%setQN((/-1.,1./))
		call Parity%setDeg(Deg)
		return
	end function
	type(QuanNum) function QuantumNumberParity2(deg,Arrow)result(Parity)
		integer,intent(in)::deg(2),Arrow
		call Parity%setFermiArrow(Arrow)
		call Parity%setQN((/-1.,1./))
		call Parity%setDeg(Deg)
		return
	end function
	type(QuanNum) function QuantumNumberParity3(QN,deg)result(Parity)
		real*4::QN
		integer,intent(in)::deg
		call Parity%setRule(0)
		call Parity%setQN((/QN/))
		call Parity%setDeg(1,Deg)
		return
	end function
	type(QuanNum) function QuantumNumberParity4(QN,deg,Arrow)result(Parity)
		real*4::QN
		integer,intent(in)::deg,Arrow
		call Parity%setFermiArrow(Arrow)
		call Parity%setQN((/QN/))
		call Parity%setDeg(1,Deg)
		return
	end function

	
!outQ=Q1+Q2

	subroutine ParityfuseOrder(Res,order,Q1,Q2,newRule_)
		type(QuanNum),intent(inout)::Res
		type(QuanNum),intent(in)::Q1,Q2
		type(Tensor),intent(inout)::order
		integer,intent(in)::newRule_
		call QuanFuseOrder(order,Res,Q1,Q2)
		return
	end subroutine


	subroutine QuanFuseOrder(order,outQ,Q1,Q2)
		type(QuanNum),intent(inout)::outQ
		type(QuanNum),intent(in)::Q1,Q2
		type(Tensor),intent(inout)::order
		integer::i,j,k,deg(2),deg1,deg2,degstart,maxi,maxj,maxk
		real*4::outQN,QN1,QN2
		logical::goon,deg_not_zero_flag
		maxj=Q2%getQNlength()
		maxk=Q1%getQNlength()
		maxi=max(maxj,maxk)
		if(maxi.eq.1)then
			call QuanFuseOrder_one_QN(order,outQ,Q1,Q2)
			return
		end if
		call outQ%setRule(Q1%getRule())
		call outQ%setQN((/-1.,1./))
		deg=0
		call order%empty()
		do i=1,maxi
			degstart=0
			do j=1,maxj
				do k=1,maxk
					outQN=outQ%getQN(i)
					QN1=Q1%getQN(k)
					QN2=Q2%getQN(j)
					goon=PRule1(outQN,QN1,QN2)
					if(goon)then
						deg1=Q1%getDeg(k)
						deg2=Q2%getDeg(j)
						deg_not_zero_flag=(deg1.ne.0).and.(deg2.ne.0)
						if(deg_not_zero_flag)then
							deg(i)=deg(i)+deg1*deg2
							if(order%getFlag())then
								call order%addrow(Tensor( (/i,k,j,deg1,deg2,degstart+1,deg(i)/)))
								degstart=deg(i)
							else
								order=(/i,k,j,deg1,deg2,1,deg(i)/)
								degstart=deg(i)
							end if
						end if
					end if
				end do
			end do
		end do
		call outQ%setDeg(Deg)
		if(order%getRank().eq.1)call order%resetDim((/1,order%getTotalData()/))
		return
	end subroutine
	subroutine QuanFuseOrder_one_QN(order,outQ,Q1,Q2)
		type(QuanNum),intent(inout)::outQ
		type(QuanNum),intent(in)::Q1,Q2
		type(Tensor),intent(inout)::order
		integer::i,j,k,deg,deg1,deg2
		real*4::outQN,QN1,QN2
		logical::deg_not_zero_flag
		QN1=Q1%getQN(1)
		QN2=Q2%getQN(1)
		if(QN1*QN2.gt.0)then
			outQN=1.
		else
			outQN=-1.
		end if
		call outQ%setRule(1)
		call outQ%setQN((/outQN/))
		deg=0
		deg1=Q1%getDeg(1)
		deg2=Q2%getDeg(1)
		deg_not_zero_flag=(deg1.ne.0).and.(deg2.ne.0)
		if(deg_not_zero_flag)then
			deg=deg1*deg2
			order=(/1,1,1,deg1,deg2,1,deg/)
		end if
		call outQ%setDeg(1,Deg)
		call order%resetDim((/1,order%getTotalData()/))
		return
	end subroutine
		

	subroutine ParityNonZeroElement(Res,dimen)
		type(Tensor),intent(inout)::Res
		type(SymDimension),intent(in)::dimen
		call ParityNonZeroElementParity(Res,dimen)
		if(Res%getRank().eq.1)then
			call Res%resetDim((/1,Res%getTotalData()/))
		end if
		return
	end subroutine
	subroutine ParityNonZeroElementParity(Res,dimen)
		type(Tensor),intent(inout)::Res
		type(SymDimension),intent(in)::dimen
		integer,allocatable::indices(:),maxdim(:),mindim(:)
		integer::Rank
		logical::goon,rulefit
		real*4,allocatable::QN(:)
		rank=dimen%getRank()
		allocate(indices(rank))
		allocate(maxdim(rank))
		allocate(mindim(rank))
		allocate(QN(rank))
		mindim=1
		indices=mindim
		maxdim=dimen%dim()
		goon=.true.
		call Res%empty()
		do while(goon)
			QN=dimen%QNDim(indices)
			rulefit=Prule2(QN)
			if(rulefit)then
				if(Res%getFlag())then
					call Res%addrow(Tensor(indices))
				else
					Res=indices
				end if
			end if
			goon=inde_counter(indices,mindim,maxdim,1)
		end do
		return
	end subroutine
	

	subroutine pParity1(Res,dimen,ith,jth)
		integer,intent(inout)::Res
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::ith,jth
		real*4::QN
		QN=Dimen%getQN(ith,jth)
		if(QN.gt.0)then
			Res=1
		else
			Res=-1
		end if
		return
	end subroutine

	logical function ifParityIndex1(dimen,ith,jth)
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::ith,jth
		real*4::QN
		QN=Dimen%getQN(ith,jth)
		if(QN.gt.0)then
			ifParityIndex1=.true.
		else
			ifParityIndex1=.false.
		end if
		return
	end function

	logical function ifParityIndex2(dimen,vec,ith,jth)
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::vec(:)
		integer,intent(in)::ith,jth
		real*4,allocatable::QN(:)
		integer::rank
		rank=dimen%getRank()
		if(rank.ne.size(vec))then
			call writemess("ERROR in calculate the parity",-1)
			call error_stop
		end if
		allocate(QN(rank))
		QN=Dimen%getQN(vec)
		if(product(QN(ith:jth) ).gt.0)then
			ifParityIndex2=.true.
		else
			ifParityIndex2=.false.
		end if
		return
	end function

	logical function ifParityIndex3(dimen,vec)
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::vec(:)
		real*4,allocatable::QN(:)
		integer::rank
		rank=dimen%getRank()
		if(rank.ne.size(vec))then
			call writemess("ERROR in calculate the parity",-1)
			call error_stop
		end if
		allocate(QN(rank))
		QN=Dimen%getQN(vec)
		if(product(QN).gt.0)then
			ifParityIndex3=.true.
		else
			ifParityIndex3=.false.
		end if
		return
	end function

	subroutine ifParityIndex4(Res,dimen,vec,ith,jth,rank)
	logical,intent(inout)::REs
		integer,intent(in)::ith,jth,rank
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::vec(rank)
		real*4::QN(rank)
		if(rank.ne.size(vec))then
			call writemess("ERROR in calculate the parity",-1)
			call error_stop
		end if
		QN=Dimen%getQN(vec)
		if(product(QN(ith:jth) ).gt.0)then
			Res=.true.
		else
			Res=.false.
		end if
		return
	end subroutine

	function PParity2(dimen,vec)result(Parity2)
		integer,allocatable::Parity2(:)
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::vec(:)
		real*4,allocatable::QN(:)
		integer::rank,i
		rank=dimen%getRank()
		if(rank.ne.size(vec))then
			call writemess("ERROR in calculate the parity",-1)
			call error_stop
		end if
		allocate(QN(rank))
		allocate(Parity2(rank))
		QN=Dimen%getQN(vec)
		do i=1,rank
			if(QN(i).gt.0)then
				Parity2(i)=1
			else
				Parity2(i)=-1
			end if
		end do
		return
	end function

	subroutine checkParityRule(Rule1,Rule2,lenname1,lenname2)
		integer,intent(in)::Rule1,Rule2
		character(len=*),intent(in)::lenname1,lenname2
		return
	end subroutine

	subroutine reverseParityRule(T,dimen,LD1,LD2)
		integer,intent(in)::LD1,LD2
		Type(Tensor),intent(inout)::T(LD1,LD2)
		Type(Symdimension),intent(inout)::dimen
		return
	end subroutine
	

end module

