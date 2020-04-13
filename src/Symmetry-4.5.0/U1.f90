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

module U1Tool
	use QuantumNumber_Type
	use SymDimension_typede
	use Tools
	use Tensor_type
	implicit none
	private


	public::checkU1Rule
	public::reverseU1Rule
	public::U1NonZeroElement
	public::U1Rule4
	public::U1fuseOrder
	public::U1ifParity4
	public::U1Parity1
	public::reverseU1RuleRoutine



	public::U1QuanNumParity
	interface U1QuanNumParity
		module procedure U1Parity2
	end interface


	public::U1ifParity
	interface U1ifParity
		module procedure U1ifParity1
		module procedure U1ifParity2
		module procedure U1ifParity3
	end interface

	public::U1rule
	interface U1rule
		module procedure U1rule1
		module procedure U1rule2
		module procedure U1rule3
	end interface


	public::U1quantumnumber
	interface U1quantumnumber
		module procedure U1QuantumNumber0
		module procedure U1QuantumNumber0_
		module procedure U1QuantumNumber0_2
		module procedure U1QuantumNumber1
		module procedure U1QuantumNumber2
		module procedure U1QuantumNumber3
		module procedure U1QuantumNumber1_
		module procedure U1QuantumNumber2_
		module procedure U1QuantumNumber3_
	end interface
	
	
	
contains
	
	
	logical function U1Rule1(Q1,Q2,Q3,R1,R2,R3)
		real*4,intent(in)::Q1,Q2,Q3
		integer,intent(in)::R1,R2,R3
		U1Rule1=((Q1*R1)+(Q2*R2)+(Q3*R3)).equ.0e0
		return
	end function
	logical function U1Rule2(Q,R)
		real*4,intent(in)::Q(:)
		integer,intent(in)::R(:)
		real*4::temp
		integer::i
		temp=Q(1)*R(1)
		do i=2,size(Q)
			temp=temp+(Q(i)*R(i))
		end do
		U1Rule2=temp.equ.0e0
		return
	end function
	
	logical function U1Rule3(dimen,indices)
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::indices(:)
		real*4::Q(size(indices))
		integer::R(size(indices))
		integer::i
		real*4::temp
		do i=1,size(indices)
			Q(i)=dimen%getQN(i,indices(i))
			R(i)=dimen%getRule(i)
		end do
		temp=Q(1)*R(1)
		do i=2,size(Q)
			temp=temp+(Q(i)*R(i))
		end do
		U1Rule3=temp.equ.0e0
		return
	end function

	subroutine  U1Rule4(Res,dimen,indices,length)
		logical,intent(inout)::Res
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::length
		integer,intent(in)::indices(length)
		real*4::Q(length)
		integer::R(length)
		integer::i
		real*4::temp
		do i=1,size(indices)
			Q(i)=dimen%getQN(i,indices(i))
			R(i)=dimen%getRule(i)
		end do
		temp=Q(1)*R(1)
		do i=2,size(Q)
			temp=temp+(Q(i)*R(i))
		end do
		Res=temp.equ.0e0
		return
	end subroutine



	type(QuanNum) function U1QuantumNumber0(QN)
		real*4,intent(in)::QN(:)
		call U1QuantumNumber0%setQN(QN)
		return
	end function

	type(QuanNum) function U1QuantumNumber0_(QN1,QN2)
		real*4,intent(in)::QN1,QN2
		real*4,allocatable::QN(:)
		integer::lenQN,i
		if(QN1.gt.QN2)then
			call writemess('ERROR in input QN1,QN2, QN1 > QN2')
			call error_stop
		end if
		lenQN=int(anint(QN2))-int(anint(QN1))+1
		allocate(QN(lenQN))
		do i=1,lenQN
			QN(i)=QN1+(i-1)
		end do		
		call U1QuantumNumber0_%setQN(QN)
		return
	end function
	type(QuanNum) function U1QuantumNumber0_2(QN1)
		real*4,intent(in)::QN1
		call U1QuantumNumber0_2%setQN([QN1])
		return
	end function
	
	type(QuanNum) function U1QuantumNumber1(QN,deg)
		integer,intent(in)::deg(:)
		real*4,intent(in)::QN(:)
		call U1QuantumNumber1%setQN(QN)
		call U1QuantumNumber1%setDeg(Deg)
		return
	end function
	type(QuanNum) function U1QuantumNumber1_(QN1,QN2,deg)result(U1QuantumNumber1)
		integer,intent(in)::deg(:)
		real*4,intent(in)::QN1,QN2
		real*4,allocatable::QN(:)
		integer::lenQN,i
		if(QN1.gt.QN2)then
			call writemess('ERROR in input QN1,QN2, QN1 > QN2')
			call error_stop
		end if
		lenQN=int(anint(QN2))-int(anint(QN1))+1
		if(lenQN.ne.size(deg))then
			call writemess('ERROR in input deg, size(deg) .ne. lenQN')
			call error_stop
		end if
		allocate(QN(lenQN))
		do i=1,lenQN
			QN(i)=QN1+(i-1)
		end do
		call U1QuantumNumber1%setQN(QN)
		call U1QuantumNumber1%setDeg(Deg)
		return
	end function
	type(QuanNum) function U1QuantumNumber2(QN,deg,rule)
		integer,intent(in)::deg(:),rule
		real*4,intent(in)::QN(:)
		call U1QuantumNumber2%setQN(QN)
		call U1QuantumNumber2%setDeg(Deg)
		call U1QuantumNumber2%setRule(rule)
		return
	end function
	type(QuanNum) function U1QuantumNumber2_(QN1,QN2,deg,rule)result(U1QuantumNumber2)
		integer,intent(in)::deg(:),rule
		real*4,intent(in)::QN1,QN2
		real*4,allocatable::QN(:)
		integer::lenQN,i
		if(QN1.gt.QN2)then
			call writemess('ERROR in input QN1,QN2, QN1 > QN2')
			call error_stop
		end if
		lenQN=int(anint(QN2))-int(anint(QN1))+1
		if(lenQN.ne.size(deg))then
			call writemess('ERROR in input deg, size(deg) .ne. lenQN')
			call error_stop
		end if
		allocate(QN(lenQN))
		do i=1,lenQN
			QN(i)=QN1+(i-1)
		end do
		call U1QuantumNumber2%setQN(QN)
		call U1QuantumNumber2%setDeg(Deg)
		call U1QuantumNumber2%setRule(rule)
		return
	end function
	type(QuanNum) function U1QuantumNumber3(QN,deg,rule,arrow)
		integer,intent(in)::deg(:),rule,arrow
		real*4,intent(in)::QN(:)
		call U1QuantumNumber3%setQN(QN)
		call U1QuantumNumber3%setDeg(Deg)
		call U1QuantumNumber3%setRule(rule)
		call U1QuantumNumber3%setFermiArrow(arrow)
		return
	end function
	type(QuanNum) function U1QuantumNumber3_(QN1,QN2,deg,rule,arrow)result(U1QuantumNumber3)
		integer,intent(in)::deg(:),rule,arrow
		real*4,intent(in)::QN1,QN2
		real*4,allocatable::QN(:)
		integer::lenQN,i
		if(QN1.gt.QN2)then
			call writemess('ERROR in input QN1,QN2, QN1 > QN2')
			call error_stop
		end if
		lenQN=int(anint(QN2))-int(anint(QN1))+1
		if(lenQN.ne.size(deg))then
			call writemess('ERROR in input deg, size(deg) .ne. lenQN')
			call error_stop
		end if
		allocate(QN(lenQN))
		do i=1,lenQN
			QN(i)=QN1+(i-1)
		end do
		call U1QuantumNumber3%setQN(QN)
		call U1QuantumNumber3%setDeg(Deg)
		call U1QuantumNumber3%setRule(rule)
		call U1QuantumNumber3%setFermiArrow(arrow)
		return
	end function
	
!outQ=Q1+Q2

	subroutine U1fuseOrder(Res,order,Q1,Q2,newRule_)
		type(QuanNum),intent(inout)::Res
		type(QuanNum),intent(in)::Q1,Q2
		type(Tensor),intent(inout)::order
		integer,intent(in)::newRule_
		integer::newRule
		real*4::meanQN1,meanQN2
		type(QuanNum)::nonZeroQ1,nonZeroQ2
		integer::flag
		flag=0
		if(Q1%ifzeroDeg())then
			nonZeroQ1=Q1!%trimZeroDegQN()
			flag=flag+1
		end if
		if(Q2%ifzeroDeg())then
			nonZeroQ2=Q2!%trimZeroDegQN()
			flag=flag+10
		end if
		select case(flag)
			case (0)
				meanQN1=real(Q1%getmaxQN()+Q1%getminQN())/2.
				meanQN2=real(Q2%getmaxQN()+Q2%getminQN())/2.
				if(meanQN1.ge.meanQN2)then
					newRule=Q1%GetRule()
				else
					newRule=Q2%GetRule()
				end if
				if(newRule_.ne.0)then
					newRule=newRule_
				end if
				call QuanFuseOrder(order,Res,Q1,Q2,newRule)
			case (1)
				meanQN1=real(nonZeroQ1%getmaxQN()+nonZeroQ1%getminQN())/2.
				meanQN2=real(Q2%getmaxQN()+Q2%getminQN())/2.
				if(meanQN1.ge.meanQN2)then
					newRule=nonZeroQ1%GetRule()
				else
					newRule=Q2%GetRule()
				end if
				if(newRule_.ne.0)then
					newRule=newRule_
				end if
				call QuanFuseOrder(order,Res,nonZeroQ1,Q2,newRule)
			case (10)
				meanQN1=real(Q1%getmaxQN()+Q1%getminQN())/2.
				meanQN2=real(nonZeroQ2%getmaxQN()+nonZeroQ2%getminQN())/2.
				if(meanQN1.ge.meanQN2)then
					newRule=Q1%GetRule()
				else
					newRule=nonZeroQ2%GetRule()
				end if
				if(newRule_.ne.0)then
					newRule=newRule_
				end if
				call QuanFuseOrder(order,Res,Q1,nonZeroQ2,newRule)
			case (11)
				meanQN1=real(nonZeroQ1%getmaxQN()+nonZeroQ1%getminQN())/2.
				meanQN2=real(nonZeroQ2%getmaxQN()+nonZeroQ2%getminQN())/2.
				if(meanQN1.ge.meanQN2)then
					newRule=nonZeroQ1%GetRule()
				else
					newRule=nonZeroQ2%GetRule()
				end if
				if(newRule_.ne.0)then
					newRule=newRule_
				end if
				call QuanFuseOrder(order,Res,nonZeroQ1,nonZeroQ2,newRule)
		end select
		return
	end subroutine

	subroutine QuanFuseOrder(order,outQ,Q1,Q2,newRule)
		type(QuanNum),intent(inout)::outQ
		type(QuanNum),intent(in)::Q1,Q2
		integer,intent(in)::newRule
		type(Tensor),intent(inout)::order
		integer::i,j,k,deg1,deg2,degstart,maxi,maxj,maxk
		integer::rule1,rule2
		real*4::outQN,QN1,QN2,minQN,maxQN
		logical::goon,deg_not_zero_flag
		real*4,allocatable::newQN(:)
		integer,allocatable::deg(:)
		real*4::tempQN(4)
		maxj=Q2%getQNlength()
		maxk=Q1%getQNlength()
		rule1=-1*Q1%GetRule()
		rule2=-1*Q2%GetRule()
		tempQN(1)=Q1%GetQN(1)*rule1+Q2%GetQN(1)*rule2
		tempQN(2)=Q1%GetQN(1)*rule1+Q2%GetQN(maxj)*rule2
		tempQN(3)=Q1%GetQN(maxk)*rule1+Q2%GetQN(1)*rule2
		tempQN(4)=Q1%GetQN(maxk)*rule1+Q2%GetQN(maxj)*rule2
		if(newRule.gt.0)then
			tempQN=-1*tempQN
		endif
		minQN=minval(tempQN)
		maxQN=maxval(tempQN)


		if(maxQN.lt.minQN)then
			call writemess('ERROR maxQN and minQN ,rule of newQN='+newRule,-1)
			call Q1%print()
			call Q2%print()
			call error_stop()
		end if

		maxi=int(anint(maxQN))-int(anint(minQN))+1
		allocate(newQN(maxi))
		allocate(deg(maxi))
		do i=1,maxi
			newQN(i)=minQN+(i-1)
		end do
		if(newQN(maxi).nequ.maxQN)then
			call writemess('ERROR in QuanFuseOrder,1')
			call error_stop
		end if

		if(maxi.eq.1)then
			call QuanFuseOrder_one_QN(order,outQ,Q1,Q2,newRule)
			return
		end if

		call outQ%setQN(newQN)
		call outQ%setRule(newRule)
		
		deg=0
		call order%empty()
		do i=1,maxi
			degstart=0
			do j=1,maxj
				do k=1,maxk
					outQN=outQ%getQN(i)
					QN1=Q1%getQN(k)
					QN2=Q2%getQN(j)
					goon=U1Rule1(outQN,QN1,QN2,newRule,rule1,rule2)
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


	subroutine QuanFuseOrder_one_QN(order,outQ,Q1,Q2,newRule)
		type(QuanNum),intent(inout)::outQ
		type(QuanNum),intent(in)::Q1,Q2
		type(Tensor),intent(inout)::order
		integer,intent(in)::newRule
		integer::i,j,k,deg,deg1,deg2
		real*4::outQN,QN1,QN2
		logical::deg_not_zero_flag
		integer::rule1,rule2
		QN1=Q1%getQN(1)
		QN2=Q2%getQN(1)
		rule1=-Q1%GetRule()
		rule2=-Q2%GetRule()
		outQN=Q1%GetQN(1)*rule1+Q2%GetQN(1)*rule2
		if(newRule.gt.0)then
			outQN=-outQN
		end if
		call outQ%setRule(newRule)
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
		

	subroutine U1NonZeroElement(Res,dimen)
		type(SymDimension),intent(in)::dimen
		type(Tensor),intent(inout)::Res
		call U1NonZeroElementU1(Res,dimen)
		if(Res%getRank().eq.1)then
			call Res%resetDim((/1,Res%getTotalData()/))
		end if
		return
	end subroutine
	subroutine U1NonZeroElementU1(Res,dimen)
		type(Tensor),intent(inout)::Res
		type(SymDimension),intent(in)::dimen
		integer,allocatable::indices(:),maxdim(:),mindim(:),allrule(:)
		integer::Rank
		logical::goon,rulefit
		real*4,allocatable::QN(:)
		rank=dimen%getRank()
		allocate(indices(rank))
		allocate(maxdim(rank))
		allocate(mindim(rank))
		allocate(QN(rank))
		allocate(allrule(rank))
		mindim=1
		indices=mindim
		maxdim=dimen%dim()
		goon=.true.
		call Res%empty()
		allrule=Dimen%GetRule()
		do while(goon)
			QN=dimen%QNDim(indices)
			rulefit=U1Rule2(QN,allrule)
			if(rulefit)then
				if(Res%getFlag())then
					call Res%addrow(Tensor(indices))
				else
					Res=indices
				end if
			end if
			goon=inde_counter(indices,mindim,maxdim,1)
		end do
		deallocate(indices)
		deallocate(maxdim)
		deallocate(mindim)
		deallocate(QN)
		deallocate(allrule)
		return
	end subroutine
	

	subroutine U1Parity1(Res,dimen,ith,jth)
		integer,intent(inout)::Res
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::ith,jth
		integer::QN
		QN=int(anint(Dimen%getQN(ith,jth)))
		if(mod(QN,2).eq.0)then
			Res=1
		else
			Res=-1
		end if
		return
	end subroutine

	logical function U1ifParity1(dimen,ith,jth)
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::ith,jth
		integer::QN
		QN=int(anint(Dimen%getQN(ith,jth)))
		if(mod(QN,2).eq.0)then
			U1ifParity1=.true.
		else
			U1ifParity1=.false.
		end if
		return
	end function

	logical function U1ifParity3(dimen,vec,ith,jth)
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::vec(:)
		integer,intent(in)::ith,jth
		integer,allocatable::QN(:)
		integer::rank
		rank=dimen%getRank()
		if(rank.ne.size(vec))then
			call writemess("ERROR in calculate the parity",-1)
			call error_stop
		end if
		allocate(QN(rank))
		QN=int(anint(Dimen%getQN(vec)))
		if(mod(sum(QN(ith:jth)),2).eq.0)then
			U1ifParity3=.true.
		else
			U1ifParity3=.false.
		end if
		return
	end function

	subroutine U1ifParity4(REs,dimen,vec,ith,jth,rank)
		logical,intent(inout)::REs
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::rank
		integer,intent(in)::vec(rank)
		integer,intent(in)::ith,jth
		integer::QN(rank)
		if(rank.ne.size(vec))then
			call writemess("ERROR in calculate the parity",-1)
			call error_stop
		end if
		QN=int(anint(Dimen%getQN(vec)))
		if(mod(sum(QN(ith:jth)),2).eq.0)then
			REs=.true.
		else
			REs=.false.
		end if
		return
	end subroutine

	logical function U1ifParity2(dimen,vec)
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::vec(:)
		integer,allocatable::QN(:)
		integer::rank
		rank=dimen%getRank()
		if(rank.ne.size(vec))then
			call writemess("ERROR in calculate the parity",-1)
			call error_stop
		end if
		allocate(QN(rank))
		QN=int(anint(Dimen%getQN(vec)))
		if(mod(sum(QN),2).eq.0)then
			U1ifParity2=.true.
		else
			U1ifParity2=.false.
		end if
		return
	end function

	 function U1Parity2(dimen,vec)result(Res)
		integer,allocatable::Res(:)
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::vec(:)
		integer,allocatable::QN(:)
		integer::rank,i
		rank=dimen%getRank()
		if(rank.ne.size(vec))then
			call writemess("ERROR in calculate the parity",-1)
			call error_stop
		end if
		allocate(QN(rank))
		allocate(Res(rank))
		QN=int(anint(Dimen%getQN(vec)))
		do i=1,rank
			if(mod(QN(i),2).eq.0)then
				Res(i)=1
			else
				Res(i)=-1
			end if
		end do
		return
	end function

	subroutine checkU1Rule(Rule1,Rule2,lenname1,lenname2)
		integer,intent(in)::Rule1,Rule2
		character(len=*),intent(in)::lenname1,lenname2
		if(rule1*rule2.ge.0)then
			call writemess('ERROR in U(1) Symmetry Rule',-1)
			call writemess('Ruel1='+rule1+',Rule2='+rule2)
			call writemess('lenname1='+lenname1+',lenname2='+lenname2)
			call error_stop
		end if
		return
	end subroutine

	subroutine reverseU1Rule(inoutT,inoutdimen,LD1,LD2)
		integer,intent(in)::LD1,LD2
		Type(Tensor),intent(inout)::inoutT(LD1,LD2)
		Type(Symdimension),intent(inout)::inoutdimen
		type(Tensor),allocatable::TData(:,:)
		integer::i,j,m,n
		integer::rule,arrow
		real*4,allocatable::QN(:)
		integer,allocatable::deg(:)
		type(QuanNum)::qunNum
		type(SymDimension)::dimen
		if(inoutdimen%getRank().ne.2)then
			call writemess('The input should be a matrix in reverseU1Rule ')
			call error_stop
		end if
		if(.not.inoutdimen%out_sample_flag())then
			call writemess('ERROR in  reverseU1Rule, one can not fuse the tensor')
			call inoutdimen%print
			call writemess('Split the dimension:')
			call inoutdimen%split()
			call inoutdimen%print
			call error_stop 
		end if
		m=LD1
		n=LD2
		allocate(TData(m,n))
		do j=1,n
			do i=1,m
				TData(i,j)=inoutT(i,n-j+1)
			end do
		end do
		n=inoutdimen%getQNlength(2)
		allocate(QN(n))
		allocate(deg(n))
		do i=1,n
			QN(i)=inoutdimen%getQN(2,n-i+1)
			if((  QN(i).nequ.(0e0) ) )QN(i)=-1*QN(i)
			deg(i)=inoutdimen%getdeg(2,n-i+1)
		end do
		rule=-1*inoutdimen%getRule(2)
		Arrow=inoutdimen%getFermiArrow(2)
		qunNum=U1quantumnumber(QN,deg,rule,Arrow)
		dimen=[inoutdimen%Quantumnumber(1),qunNum]
		if(inoutdimen%getnameFlag().ne.0)then
			do i=1,inoutdimen%getRank()
				call dimen%setName(i,inoutdimen%getName(i))
			end do
		end if
		inoutdimen=dimen
		do j=1,n
			do i=1,m
				inoutT(i,j)=TData(i,j)
			end do
		end do


		deallocate(TData)
		deallocate(QN)
		deallocate(deg)
		return
	end subroutine


	subroutine reverseU1RuleRoutine(dimen)
		Type(Symdimension),intent(inout)::dimen
		integer::i,rank
		rank=dimen%getRank()
		do i=1,rank
			call dimen%setRule(i,-1*dimen%getRule(i))
		end do
		return
	end subroutine
	

end module

