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

module QuantumNumber_Type
	use Tools
	use mpi
	use Tensor_type
	implicit none
	private
	
	public::QuanNum
	type QuanNum!Symmetry quantum number
		integer::lenOfQuanNum=0
		real*4,allocatable::QuanNum(:)
		integer,allocatable::degeneracy(:)
		integer::rule=0! rule=1 or -1  in U(1) Sin=Sout==> Sin-Sout=0==>Sin*rule(Sin)+Sout*rule(Sout)=0
		               ! rule=1        in parity symmetry, no used
		               ! rule=0 no symmetry
		integer::FermiArrow=0! in for the fermi tensor network
	contains	
		procedure,public::getQNlength=>getlenOfQuanNum
		procedure,public::SetQN=>setQuanNumQN
		generic,public::SetDeg=>setQuanNumDeg,setQuanNumDegi
		procedure,public::SetRule=>setQuanNumRule
		procedure,public::empty => emptyQuanNum
		procedure,public::deallocate => deallocateQuanNum
		procedure,public::GetRule=>getQuanNumRule
		procedure,public::GetFermiArrow=>getQuanNumArrow
		generic,public::GetDeg => getQuanNumDeg,getAllQuanNumDeg,getQuanNumDegQ
		generic,public::GetQN =>getQuanNumQN,getQuanNumAllQN
		procedure,public::getIndex=>outQuanNumIndex
		procedure,public::outDeg =>outAllQuanNumDeg
		procedure,public::outQN =>outAllQuanNumQN
		procedure,public::outDegeneracy =>outAllQuanNumDeg
		procedure,public::outQuanNum =>outAllQuanNumQN
		procedure,public::print =>QuanNumPrint
		procedure,public::printQN =>QuanNumPrintQN
		procedure,public::read =>QuanNumRead
		procedure,public::setFermiArrow=>setQuanNumArrow
		procedure,public::getMaxQN
		procedure,public::getMinQN
		procedure,public::ifzeroDeg
		procedure,public::TrimZeroDegQN
		procedure,public::trimZeroDeg
		procedure,public::trimZeroDegIndex
		procedure,public::killZeroDeg
		procedure,public::NonZeroQNindex
		procedure,public::NonZeroDegQN
		procedure::setQuanNumDeg
		procedure::setQuanNumDegi
		procedure::getQuanNumDeg
		procedure::getQuanNumDegQ
		procedure::getAllQuanNumDeg
		procedure::getQuanNumQN,getQuanNumAllQN
	end type QuanNum
	
	
	public::assignment(=)
	interface assignment(=)
		module procedure QuanNumInitialization!QuanNum=QuanNum
		module procedure QuanNumInitializationArray!QuanNum(:)=QuanNum(:)
	end interface
	
	public::allocateCheck
	interface allocateCheck! if size(A)<lenA then allocate A,else do nothing
		module procedure allocateCheck_QuanNum
	end interface
	

	interface QuanNum
		procedure constructor_scal
		procedure constructor_scal_deg
		procedure constructor_array
		procedure constructor_array_deg
	end interface
	
	public::MPI_send_QuanNum,MPI_BCAST_QuanNum
contains
	type(QuanNUm) function constructor_scal(QN)result(Res)
		real*4,intent(in)::QN
		call Res%setQN([QN])
		return
	end function
	type(QuanNUm) function constructor_scal_deg(QN,deg)result(Res)
		real*4,intent(in)::QN
		integer,intent(in)::deg
		call Res%setQN([QN],[deg])
		return
	end function
	type(QuanNUm) function constructor_array(QN)result(Res)
		real*4,intent(in)::QN(:)
		call Res%setQN(QN)
		return
	end function
	type(QuanNUm) function constructor_array_deg(QN,deg)result(Res)
		real*4,intent(in)::QN(:)
		integer,intent(in)::deg(:)
		call Res%setQN(QN,deg)
		return
	end function

	logical function ifzeroDeg(QN)
		class(QuanNum),intent(in)::QN
		integer::i
		ifzeroDeg=.false.
		do i=1,QN%getQNlength()
			if(QN%getDeg(i).eq.0)then
				ifzeroDeg=.true.
				return
			end if
		end do
		return
	end function 	
	subroutine TrimZeroDegQN(QN)
		class(QuanNum),intent(inout)::QN
		real*4,allocatable::Qnum(:)
		integer,allocatable::deg(:)
		integer::oldlenQN,Rule,Arrow,lenQN
		integer::ith,jth
		logical::goon
		if(QN%ifzeroDeg())then
			oldlenQN=QN%getQNlength()
			goon=.true.
			ith=0
			do while(goon)
				ith=ith+1
				if(QN%getDeg(ith).ne.0)goon=.false.
				if(ith.eq.oldlenQN)goon=.false.
			end do
			jth=oldlenQN+1
			goon=.true.
			do while(goon)
				jth=jth-1
				if(QN%getDeg(jth).ne.0)goon=.false.
				if(jth.eq.1)goon=.false.
			end do
			Rule=QN%getRule()
			Arrow=QN%getFermiArrow()
			lenQN=jth-ith+1
			if(lenQN.le.0)then
				call writemess('ERROR in TrimZeroDegQN, QuantumNumber.f90')
				call QN%print()
				call error_stop
			end if
			allocate(Qnum(lenQN))
			allocate(deg(lenQN))
			Qnum=QN%QuanNum(ith:jth)
			deg=QN%degeneracy(ith:jth)
			call QN%empty()
			call QN%setQN(Qnum)
			call QN%setDeg(deg)
			call QN%setRule(Rule)
			call QN%setFermiArrow(Arrow)
		end if
		return
	end subroutine
	type(QuanNum) function trimZeroDeg(QN)
		class(QuanNum),intent(in)::QN
		real*4,allocatable::Qnum(:)
		integer,allocatable::deg(:)
		integer::oldlenQN,Rule,Arrow,lenQN
		integer::ith,jth
		logical::goon
		if(QN%ifzeroDeg())then
			oldlenQN=QN%getQNlength()
			goon=.true.
			ith=0
			do while(goon)
				ith=ith+1
				if(QN%getDeg(ith).ne.0)goon=.false.
				if(ith.eq.oldlenQN)goon=.false.
			end do
			jth=oldlenQN+1
			goon=.true.
			do while(goon)
				jth=jth-1
				if(QN%getDeg(jth).ne.0)goon=.false.
				if(jth.eq.1)goon=.false.
			end do
			Rule=QN%getRule()
			Arrow=QN%getFermiArrow()
			lenQN=jth-ith+1
			allocate(Qnum(lenQN))
			allocate(deg(lenQN))
			Qnum=QN%QuanNum(ith:jth)
			deg=QN%degeneracy(ith:jth)
			call trimZeroDeg%empty()
			call trimZeroDeg%setQN(Qnum)
			call trimZeroDeg%setDeg(deg)
			call trimZeroDeg%setRule(Rule)
			call trimZeroDeg%setFermiArrow(Arrow)
		else
			trimZeroDeg=QN
		end if
		return
	end function
	function trimZeroDegIndex(QN)result(Degindex)
		integer,allocatable::Degindex(:)
		class(QuanNum),intent(in)::QN
		logical::goon
		integer::oldlenQN
		allocate(Degindex(2))
		oldlenQN=QN%getQNlength()
		Degindex(1)=0
		goon=.true.
		do while(goon)
			Degindex(1)=Degindex(1)+1
			if(QN%getDeg(Degindex(1)).ne.0)goon=.false.
			if(Degindex(1).eq.oldlenQN)goon=.false.
		end do
		Degindex(2)=oldlenQN+1
		goon=.true.
		do while(goon)
			Degindex(2)=Degindex(2)-1
			if(QN%getDeg(Degindex(2)).ne.0)goon=.false.
			if(Degindex(2).eq.1)goon=.false.
		end do
		return
	end function

	subroutine killZeroDeg(QN)
		class(QuanNum),intent(inout)::QN
		real*4,allocatable::Qnum(:)
		integer,allocatable::deg(:)
		integer::oldlenQN,Rule,Arrow,lenQN
		integer::i,ith
		logical,allocatable::nonZeroElement(:)
		if(QN%ifzeroDeg())then
			oldlenQN=QN%getQNlength()
			allocate(nonZeroElement(oldlenQN))
			lenQN=0
			do i=1,oldlenQN
				if(QN%getDeg(i).ne.0)then
					lenQN=lenQN+1
					nonZeroElement(i)=.true.
				else
					nonZeroElement(i)=.false.
				end if
			end do
			Rule=QN%getRule()
			Arrow=QN%getFermiArrow()
			if(lenQN.eq.0)then
				call writemess('ERROR in killZeroDeg, QuantumNumber.f90')
				call QN%print()
				call error_stop
			end if
			allocate(Qnum(lenQN))
			allocate(deg(lenQN))
			ith=0
			do i=1,oldlenQN
				if(nonZeroElement(i))then
					ith=ith+1
					Qnum(ith)=QN%QuanNum(i)
					deg(ith)=QN%degeneracy(i)
				end if
			end do
			call QN%empty()
			call QN%setQN(Qnum)
			call QN%setDeg(deg)
			call QN%setRule(Rule)
			call QN%setFermiArrow(Arrow)
		end if
		return
	end subroutine
	function NonZeroQNindex(QN)
		type(Tensor)::NonZeroQNindex
		class(QuanNum),intent(in)::QN
		integer::oldlenQN,Rule,Arrow,lenQN
		integer::i
		integer,allocatable::indexData(:)
		oldlenQN=QN%getQNlength()
		allocate(indexData(oldlenQN))
		lenQN=0
		do i=1,oldlenQN
			if(QN%getDeg(i).ne.0)then
				lenQN=lenQN+1
				indexData(lenQN)=i
			end if
		end do
		NonZeroQNindex=indexData(1:lenQN)
	end function
	type(QuanNum) function  NonZeroDegQN(QN)
		class(QuanNum),intent(in)::QN
		real*4,allocatable::Qnum(:)
		integer,allocatable::deg(:)
		integer::oldlenQN,Rule,Arrow,lenQN
		integer::i,ith
		logical,allocatable::nonZeroElement(:)
		if(QN%ifzeroDeg())then
			oldlenQN=QN%getQNlength()
			allocate(nonZeroElement(oldlenQN))
			lenQN=0
			do i=1,oldlenQN
				if(QN%getDeg(i).ne.0)then
					lenQN=lenQN+1
					nonZeroElement(i)=.true.
				else
					nonZeroElement(i)=.false.
				end if
			end do
			Rule=QN%getRule()
			Arrow=QN%getFermiArrow()
			if(lenQN.eq.0)then
				call writemess('ERROR in killZeroDeg, QuantumNumber.f90')
				call QN%print()
				call error_stop
			end if
			allocate(Qnum(lenQN))
			allocate(deg(lenQN))
			ith=0
			do i=1,oldlenQN
				if(nonZeroElement(i))then
					ith=ith+1
					Qnum(ith)=QN%QuanNum(i)
					deg(ith)=QN%degeneracy(i)
				end if
			end do
			call NonZeroDegQN%empty()
			call NonZeroDegQN%setQN(Qnum)
			call NonZeroDegQN%setDeg(deg)
			call NonZeroDegQN%setRule(Rule)
			call NonZeroDegQN%setFermiArrow(Arrow)
		else
			NonZeroDegQN=QN
		end if
		return
	end function

	subroutine allocateCheck_QuanNum(A,lenA)
		type(QuanNum),allocatable,intent(inout) ::A(:)
		integer::lenA
		if(allocated(A)) then
			if(size(A).lt.lenA) then
				deallocate(A)
				allocate(A(lenA))
			end if
		else
			allocate(A(lenA))
		end if
		return
	end subroutine
	
! subroutine and function for QuanNum
	integer function getlenOfQuanNum(QN)
		class(QuanNum),intent(in) :: QN
		getlenOfQuanNum=QN%lenOfQuanNum
		return
	end function
	integer function getQuanNumRule(QN)
		class(QuanNum),intent(in) :: QN
		getQuanNumRule=QN%Rule
		return
	end function
	integer function getQuanNumArrow(QN)
		class(QuanNum),intent(in) :: QN
		getQuanNumArrow=QN%FermiArrow
		return
	end function

	real*4 function getMaxQN(QN)
		class(QuanNum),intent(in) :: QN
		if(QN%lenOfQuanNum.eq.0)then
			call writemess('There is no data in the type(QuanNum)')
			call error_stop
		end if
		getMaxQN=maxval(QN%QuanNum(1:QN%lenOfQuanNum))
		return
	end function
	
	real*4 function getMinQN(QN)
		class(QuanNum),intent(in) :: QN
		if(QN%lenOfQuanNum.eq.0)then
			call writemess('There is no data in the type(QuanNum)')
			call error_stop
		end if
		getMinQN=minval(QN%QuanNum(1:QN%lenOfQuanNum))
		return
	end function

	
	subroutine QuanNumInitialization(inoutQN,inQN)
		type(QuanNum),intent(inout) ::inoutQN
		type(QuanNum),intent(in) :: inQN
		inoutQN%lenOfQuanNum=inQN%lenOfQuanNum
		inoutQN%rule=inQN%rule
		if(inoutQN%lenOfQuanNum.eq.0)return
		call allocateCheck(inoutQN%QuanNum,inQN%lenOfQuanNum)
		inoutQN%QuanNum(1:inoutQN%lenOfQuanNum)=inQN%QuanNum(1:inQN%lenOfQuanNum)
		call allocateCheck(inoutQN%degeneracy,inQN%lenOfQuanNum)
		inoutQN%degeneracy(1:inoutQN%lenOfQuanNum)=inQN%degeneracy(1:inQN%lenOfQuanNum)
		inoutQN%FermiArrow=inQN%FermiArrow
		return
	end subroutine
	subroutine QuanNumInitializationArray(inoutQN,inQN)
		type(QuanNum),intent(inout) ::inoutQN(:)
		type(QuanNum),intent(in) :: inQN(:)
		integer::length,i
		length=size(inQN)
		if(size(inoutQN).lt.length)then
			write(*,*)"ERROR in assignment of two type(QuanNum) array "
			write(*,*)"Q1(:)=Q2(:),size(Q1)<size(Q2)"
			write(*,*)size(inoutQN),length
			call error_stop()
		end if
		do i=1,length
			inoutQN(i)=inQN(i)
		end do
	end subroutine
	subroutine emptyQuanNum(inoutQN)
		class(QuanNum),intent(inout) ::inoutQN
		if(deallocate_memory_flag)then
			call deallocateQuanNum(inoutQN)
			return
		end if
		inoutQN%lenOfQuanNum=0
		inoutQN%rule=0
		inoutQN%FermiArrow=0
		return
	end subroutine
	subroutine deallocateQuanNum(inoutQN)
		class(QuanNum),intent(inout) ::inoutQN
		inoutQN%lenOfQuanNum=0
		inoutQN%rule=0
		inoutQN%FermiArrow=0
		if(allocated(inoutQN%QuanNum)) then
			deallocate(inoutQN%QuanNum)
		end if
		if(allocated(inoutQN%degeneracy)) then
			deallocate(inoutQN%degeneracy)
		end if
		return
	end subroutine
	
	subroutine setQuanNumQN(QN,QuantumNumber,degeneracy)
		class(QuanNum),intent(inout)::QN
		real*4,intent(in)::QuantumNumber(:)
		integer,intent(in),optional::degeneracy(:)
		QN%lenOfQuanNum=size(QuantumNumber)
		call allocateCheck(QN%QuanNum,QN%lenOfQuanNum)
		QN%QuanNum(1:QN%lenOfQuanNum)=QuantumNumber
		call allocateCheck(QN%degeneracy,QN%lenOfQuanNum)
		if(present(degeneracy))then
			if(size(degeneracy).ne.QN%lenOfQuanNum)then
				call writemess("ERROR of degeneracy in setting quantum number and degeneracy to type(QuanNum)")
				call error_stop()
			end if
			QN%degeneracy(1:QN%lenOfQuanNum)=degeneracy
		else
			QN%degeneracy(1:QN%lenOfQuanNum)=1
		end if
		return
	end subroutine
	subroutine setQuanNumDeg(QN,degeneracy)
		class(QuanNum),intent(inout)::QN
		integer,intent(in)::degeneracy(:)
		if(QN%lenOfQuanNum.eq.0)then
			call writemess("ERROR of degeneracy in setting  degeneracy to type(QuanNum)")
			call writemess("One should set quantum number  first")
			call error_stop()
		end if
		call allocateCheck(QN%degeneracy,QN%lenOfQuanNum)
		if(size(degeneracy).ne.QN%lenOfQuanNum)then
			call writemess("ERROR of degeneracy in setting  degeneracy to type(QuanNum)")
			call error_stop()
		end if
		QN%degeneracy(1:QN%lenOfQuanNum)=degeneracy
		return
	end subroutine
	subroutine setQuanNumDegi(QN,ith,degeneracy)
		class(QuanNum),intent(inout)::QN
		integer,intent(in)::degeneracy,ith
		if(QN%lenOfQuanNum.eq.0)then
			call writemess("ERROR of degeneracy in setting  degeneracy to type(QuanNum)")
			call writemess("One should set quantum number  first")
			call error_stop()
		end if
		call allocateCheck(QN%degeneracy,QN%lenOfQuanNum)
		if(ith.gt.QN%lenOfQuanNum)then
			call writemess("ERROR of degeneracy in setting  degeneracy to type(QuanNum)")
			call error_stop()
		end if
		if(degeneracy.lt.0)then
			call writemess('ERROR in setting the degeneracy to a quantum number')
			call writemess(' the degeneracy should be larger than 0. you input degeneracy='+degeneracy)
			call error_stop
		end if
		QN%degeneracy(ith)=degeneracy
		return
	end subroutine
	
	subroutine setQuanNumRule(QN,Rule)
		class(QuanNum),intent(inout)::QN
		integer,intent(in)::Rule
		QN%Rule=Rule
		return
	end subroutine
	subroutine setQuanNumArrow(QN,Arrow)
		class(QuanNum),intent(inout)::QN
		integer,intent(in)::Arrow
		QN%FermiArrow=Arrow
		return
	end subroutine

!Get the degeneracy
	integer function getQuanNumDeg(QN,ith)
		class(QuanNum),intent(in)::QN
		integer,intent(in)::ith
		if(ith.gt.QN%getQNlength())then
			call writemess("The input index is larger then the length of the degeneracy of Quantum Number")
			call error_stop()
		end if
		getQuanNumDeg=QN%degeneracy(ith)
		return
	end function
	integer function getQuanNumDegQ(QN,Qith)
		class(QuanNum),intent(in)::QN
		real*4,intent(in)::Qith
		integer::ith
		ith=QN%GetIndex(Qith)
		if(ith.gt.QN%getQNlength())then
			call writemess("The input index is larger then the length of the degeneracy of Quantum Number")
			call error_stop()
		end if
		if(ith.eq.0)then
			call writemess("Can not Find the Quantum Number="+Qith)
			call error_stop()
		end if
		getQuanNumDegQ=QN%degeneracy(ith)
		return
	end function
	function getAllQuanNumDeg(QN)
		integer,allocatable::getAllQuanNumDeg(:)
		class(QuanNum),intent(in)::QN
		integer::length
		length=QN%getQNlength()
		allocate(getAllQuanNumDeg(length))
		getAllQuanNumDeg=QN%degeneracy(1:length)
		return
	end function
!Get the Quantum Number
	real*4 function getQuanNumQN(QN,ith)
		class(QuanNum),intent(in)::QN
		integer,intent(in)::ith
		if(ith.gt.QN%getQNlength())then
			call writemess("The input index is larger then the length of the Quantum Number")
			call error_stop()
		end if
		getQuanNumQN=QN%QuanNum(ith)
		return
	end function
	function getQuanNumAllQN(QN)
		real*4,allocatable::getQuanNumAllQN(:)
		class(QuanNum),intent(in)::QN
		integer::length
		length=QN%getQNlength()
		allocate(getQuanNumAllQN(length))
		getQuanNumAllQN=QN%QuanNum(1:length)
		return
	end function
	
	subroutine outAllQuanNumDeg(QN,outQN)
		class(QuanNum),intent(in)::QN
		integer,intent(inout)::outQN(:)
		outQN=QN%degeneracy(1:QN%lenOfQuanNum)
		return
	end subroutine
	subroutine outAllQuanNumQN(QN,outQN)
		class(QuanNum),intent(in)::QN
		real*4,intent(inout)::outQN(:)
		outQN=QN%QuanNum(1:QN%lenOfQuanNum)
		return
	end subroutine
	
	integer function outQuanNumIndex(QN,Qnumber)
		class(QuanNum),intent(in)::QN
		real*4,intent(in)::Qnumber
		integer::i
		outQuanNumIndex=0
		do i=1,QN%lenOfQuanNum
			if(abs(QN%QuanNum(i)-Qnumber).le.1e-7)then
				outQuanNumIndex=i
				exit
			end if
		end do
		return
	end function
		
	
	subroutine QuanNumPrint(na,uni)
		class(QuanNum),intent(in)::na
		integer,optional,intent(in)::uni
		character(len=5000)::words
		integer::i
		if(present(uni))then
			if(na%lenOfQuanNum.eq.0)then
				write(uni,*)"F , Do not Set Quantum Number yet"
				return
			else
				write(uni,*)"T , data of Quantum Number"
			end if
			write(uni,*)"rule",na%rule
			write(uni,*)"Arrow",na%FermiArrow
			write(uni,*)"lenOfQuanNum",na%lenOfQuanNum
			write(uni,*)"QN_i:"
			write(uni,*)na%QuanNum(1:na%lenOfQuanNum)
			write(uni,*)"degeneracy:"
			write(uni,*)na%degeneracy(1:na%lenOfQuanNum)
		else
			if(na%lenOfQuanNum.eq.0)then
			 call writemess("Do not Set Quantum Number yet",-1)
			 return
			end if
			call writemess("rule:"+na%rule,-1)
			call writemess("fermi-arrow:"+na%FermiArrow,-1)
			call writemess("QN_i :",-1)
			words=na%QuanNum(1)
			do i=2,na%lenOfQuanNum
				words=words+(' ,'+(' '+na%QuanNum(i)))
			end do
			call writemess(words,-1)
			call writemess("degeneracy:",-1)
			words=na%degeneracy(1)
			do i=2,na%lenOfQuanNum
				words=words+(' ,'+(' '+na%degeneracy(i)))
			end do
			call writemess(words,-1)
			call writemess("------",-1)
		end if
		return
	end subroutine
	subroutine QuanNumPrintQN(na,inw,uni)
		class(QuanNum),intent(in)::na
		character(len=*),intent(in)::inw
		integer,optional,intent(in)::uni
		character(len=5000)::words
		integer::i
		if(present(uni))then
			if(na%lenOfQuanNum.eq.0)then
				write(uni,*)"F , Do not Set Quantum Number yet"
				return
			end if
			words=inw+" QN:"
			words=words+(' '+na%QuanNum(1))
			do i=2,na%lenOfQuanNum
				words=words+(' ,'+(' '+na%QuanNum(i)))
			end do
			write(uni,*)trim(adjustl(words))
			words="    Deg:"
			words=words+(' '+na%degeneracy(1))
			do i=2,na%lenOfQuanNum
				words=words+(' ,'+(' '+na%degeneracy(i)))
			end do
			write(uni,*)trim(words)
			write(uni,*)"-------------------------"
		else
			if(na%lenOfQuanNum.eq.0)then
			 call writemess("Do not Set Quantum Number yet",-1)
			 return
			end if
			words=inw+" QN:"
			words=words+(' '+na%QuanNum(1))
			do i=2,na%lenOfQuanNum
				words=words+(' ,'+(' '+na%QuanNum(i)))
			end do
			call writemess(words,-1)
			words="    Deg:"
			words=words+(' '+na%degeneracy(1))
			do i=2,na%lenOfQuanNum
				words=words+(' ,'+(' '+na%degeneracy(i)))
			end do
			call writemess(words,-1)
			call writemess("-------------------------",-1)
		end if
		return
	end subroutine
	subroutine QuanNumRead(na,uni)
		class(QuanNum),intent(inout)::na
		integer,intent(in)::uni
		CHARACTER(len=50)::notused
		logical::flag
		read(uni,*)flag
		if(.not.Flag)then
			call na%empty()
			return
		end if
		read(uni,*)notused,na%rule
		read(uni,*)notused,na%FermiArrow
		read(uni,*)notused,na%lenOfQuanNum
		call allocateCheck(na%QuanNum,na%lenOfQuanNum)
		read(uni,*)notused
		read(uni,*)na%QuanNum(1:na%lenOfQuanNum)
		read(uni,*)notused
		call allocateCheck(na%degeneracy,na%lenOfQuanNum)
		read(uni,*)na%degeneracy(1:na%lenOfQuanNum)
	end subroutine
	
	
	
	

!**********************************************************************
!**********************************************************************
!	the code below is for MPI
!**********************************************************************
		
	subroutine MPI_send_QuanNum(Q1,Q2,ID1,ID2,ierr,MPIcommon)
		type(QuanNum),intent(in)::Q1
		type(QuanNum),intent(inout)::Q2
		integer,optional,intent(in)::MPIcommon
		integer,intent(in)::ID1,ID2
		integer::ierr
		integer::proID,proNum,tag,istatus(MPI_STATUS_SIZE),mpi_comm
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		tag=1
		call mpi_comm_rank(mpi_comm,proID,ierr)
		call mpi_comm_size(mpi_comm,proNum,ierr )
		if(present(MPIcommon))then
			if((ID1.ge.proNum).or.(ID2.ge.proNum))return
		end if
		
		if(ID1.eq.ID2) return !The same cpu, do nothing
		
		if((proID.ne.ID1).and.(proID.ne.ID2)) return!The proID do not send or recv, return
		
!************************   lenOfQuanNum   *************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Q1%lenOfQuanNum,1,MPI_INTEGER,ID2,tag,mpi_comm,ierr)
			if(Q1%lenOfQuanNum.eq.0) return
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Q2%lenOfQuanNum,1,MPI_INTEGER,ID1,tag,mpi_comm,istatus,ierr)
				if(Q2%lenOfQuanNum.eq.0) then
					Q2%Rule=0
					return
				else
					call allocateCheck(Q2%QuanNum,Q2%lenOfQuanNum)
					call allocateCheck(Q2%degeneracy,Q2%lenOfQuanNum)
					!allocate(Q2%QuanNum(Q2%lenOfQuanNum))
					!allocate(Q2%degeneracy(Q2%lenOfQuanNum))
				end if
		end if
!*************************   rule   **************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Q1%rule,1,MPI_INTEGER,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Q2%rule,1,MPI_INTEGER,ID1,tag,mpi_comm,istatus,ierr)
		end if
!*************************   FermiArrow   **************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Q1%FermiArrow,1,MPI_INTEGER,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Q2%FermiArrow,1,MPI_INTEGER,ID1,tag,mpi_comm,istatus,ierr)
		end if
!*************************   QuanNum   **************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Q1%QuanNum(1:Q1%lenOfQuanNum),Q1%lenOfQuanNum,MPI_REAL,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Q2%QuanNum(1:Q2%lenOfQuanNum),Q2%lenOfQuanNum,MPI_REAL,ID1,tag,mpi_comm,istatus,ierr)
		end if
!*************************   degeneracy   **************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Q1%degeneracy(1:Q1%lenOfQuanNum),Q1%lenOfQuanNum,MPI_INTEGER,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Q2%degeneracy(1:Q2%lenOfQuanNum),Q2%lenOfQuanNum,MPI_INTEGER,ID1,tag,mpi_comm,istatus,ierr)
		end if		
		return
	end subroutine
	
	subroutine MPI_BCAST_QuanNum(QN,ID,ierr,MPIcommon)
		type(QuanNum),intent(inout)::QN
		integer,intent(in)::ID
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,istatus(MPI_STATUS_SIZE),mpi_comm
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		tag=1
		call mpi_comm_rank(mpi_comm,proID,ierr)
		call mpi_comm_size(mpi_comm,proNum,ierr )
		
		if(present(MPIcommon))then
			if(ID.ge.proNum)return
		end if
		
!************************   lenOfQuanNum   *************************************************		
		call MPI_BCAST(QN%lenOfQuanNum,1,MPI_INTEGER,ID,mpi_comm,ierr)	
		if(QN%lenOfQuanNum.eq.0) then
			QN%Rule=0
			return
		else
			if(proid.ne.ID)then
				call allocateCheck(QN%QuanNum,QN%lenOfQuanNum)
				call allocateCheck(QN%degeneracy,QN%lenOfQuanNum)
			end if
		end if
!*************************   rule   **************************************************		
		call MPI_BCAST(QN%rule,1,MPI_INTEGER,ID,mpi_comm,ierr)	
!*************************   FermiArrow   **************************************************		
		call MPI_BCAST(QN%FermiArrow,1,MPI_INTEGER,ID,mpi_comm,ierr)	
!*************************   QuanNum   **************************************************		
		call MPI_BCAST(QN%QuanNum(1:QN%lenOfQuanNum),QN%lenOfQuanNum,MPI_REAL,ID,mpi_comm,ierr)	
!*************************   degeneracy   **************************************************	
		call MPI_BCAST(QN%degeneracy(1:QN%lenOfQuanNum),QN%lenOfQuanNum,MPI_INTEGER,ID,mpi_comm,ierr)	
		return
	end subroutine
	
		

end module





