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

module SymDimension_typede
	use dimension_typede
	use QuantumNumber_Type
	use Tools
	use mpi
	use Tensor_type
	implicit none
	private
	
	
	public::SymDimension
	type,extends (Dimension) :: SymDimension
		type(QuanNum),allocatable :: QN(:)
		logical::QNflag=.false.
	contains
		procedure,public::getQNlength=>getlenOfSymDimQuanNum
		generic,public::SetQN=>setSymDimensionQN,setSymDimQuanNum
		generic,public::SetRule=>setSymDimensionRule,setAllSymDimensionRule,setSymDimensionRule_char
		generic,public::SetFermiArrow=>setSymDimensionArrow,setSymDimensionArrow_char,setAllSymDimensionArrow
		procedure,public::setDeg=>setSymDimensionDeg
		procedure,public::getQNflag
		procedure,public::empty => emptyDimension
		procedure,public::deallocate=>cleanDimension
		procedure::getMaxQN1,getMaxQN2
		procedure::getMinQN1,getMinQN2
		generic,public::getMaxQN=>getMaxQN1,getMaxQN2
		generic,public::getMinQN=>getMinQN1,getMinQN2

		
		generic,public::GetRule=>getSymDimRule,getSymDimAllRule,getSymDimRuleChar
		generic,public::getFermiArrow=>getFermiArrow0,getFermiArrowChar,getAllFermiArrow

		
		generic,public::Getindex =>outSymDimIndex,outSymDimALLIndex
		!In U(1), dimension is [2,2,3],maxQN is 0.5,0.5,1
		!         input [-0.5,0.5,0] output [1,2,2]
		!In parity dimension is [2,2,2]
		!        input [-1,1,1] output [1,2,2]
		!If no such quantum number,return 0
		! example input [-1,5.,1] output [1,0,2]
		
		generic,public::GetDeg =>getSymSymDimensionDeg,outAllSymDimDegFun,getSymSymDimensionDegVec,getSymSymDimensionDegQVec,&
		         getSymSymDimensionDegQN
		procedure,public::outDegeneracy =>outAllSymDimQN!The same as GetDeg, but it is a subroutine
		!GetDeg(i,j):output the dimension%QN(i)%degeneracy(j)
		!GetDeg(i):output the dimension%QN(i)%degeneracy
		generic,public::GetQN =>getSymSymDimensionQN,outAllSymDimQNFun,getSymSymDimensionQNVec!get quannum number, output array real*4 
		procedure,public::outQuanNum =>outAllSymDimQN!The same as GetQN, but it is a subroutine
		
		!GetQN(i,j):output the dimension%QN(i)%QuanNum(j)
		!GetQN(i):output the dimension%QN(i)%QuanNum
		generic,public::QuantumNumber=>getSymSymDimensionQuanNum,getSymSymDimensionQuanNumChar!get quannum number, output type(QuanNum)
		
		
		procedure,public::DegDim=>getSymSymDimensionDegVec!DegDim( (/1,2,3/) ),output the degeneracy of dimension (/1,2,3/)
		generic,public::DegDimension=>getDeg_dimension,getDeg_dimensionQ!The same as DegDim,but it output a type(Dimension) so as the TensorName in it
		procedure,public::QNDim=>getSymSymDimensionQNVec!QNDim( (/1,2,3/) ),output the Quantum number of dimension (/1,2,3/)
		
		generic,public::NonSymIndex=>QN2nonSyminde_one,QN2nonSyminde_one2,QN2nonSyminde,QN2nonSyminde2
		procedure,public::RNDim=>RNDimRoutine
		!U(1) symmetry
		!|S,rs>,S=-1,0,1,degeneracy: 1,2,1
		!in the symmetry base
		! |-1,1>,|0,1>,|0,2>,|1,1>
		!transfrom to non-symmety index will be
		! |1>  , |2>  ,|3>  , |4>
		!input s, output imin and imax
		! example,input s=0,output imin=2,imax=3, that is output a array of (2,3)
		! example,input s=1,output imin=4,imax=4, that is output a array of (4,4)

		!generic,public::QuanNum=>outSymDimQuanNum,outAllSymDimQuanNum
		procedure,public::print=>Dprint
		procedure,public::info=>Dprint2
		procedure,public::read=>readdimension
		procedure,public::SPermute=>Dimpermute!Can not overwrite because the data type of output
		procedure,public::outAllName
		procedure,public::getAllName=>outAllName
		procedure,public::ifzeroDeg
		procedure::index2QNinfo1,index2QNinfo2
		generic,public::index2QNinfo=>index2QNinfo1,index2QNinfo2
		procedure::getSymSymDimensionDegVec
		procedure::getSymSymDimensionDegQN
		procedure::setSymDimensionQN
		procedure::setSymDimQuanNum
		procedure::setSymDimensionRule
		procedure::setAllSymDimensionRule
		procedure::setSymDimensionRule_char
		procedure::getSymDimRule
		procedure::getSymDimAllRule
		procedure::getSymDimRuleChar
		procedure::outSymDimIndex
		procedure::outSymDimALLIndex
		procedure::getSymSymDimensionDeg
		procedure::outAllSymDimDegFun
		procedure::getSymSymDimensionQN
		procedure::outAllSymDimQNFun
		procedure::getSymSymDimensionQuanNum
		procedure::getSymSymDimensionQuanNumChar
		procedure::QN2nonSyminde_one
		procedure::QN2nonSyminde_one2
		procedure::QN2nonSyminde
		procedure::QN2nonSyminde2
		procedure::outSymDimQuanNum
		procedure::outAllSymDimQuanNum
		procedure::getFermiArrow0,getFermiArrowChar,getAllFermiArrow
		procedure::setSymDimensionArrow,setSymDimensionArrow_char,setAllSymDimensionArrow
		procedure::getSymSymDimensionDegQVec
		procedure::getDeg_dimension,getDeg_dimensionQ
		procedure::getSymSymDimensionQNVec
	end type SymDimension
	
	public::assignment(=)
	interface assignment(=)
		module procedure QuanNumInitialization2!Symdimension=QuanNum
		module procedure SymDimInitialization!Symdimension=Symdimension
		module procedure SymDimInitialization2!Symdimension=integer(:)
		module procedure SymDimInitialization3!Symdimension=dimension
		module procedure SymDimInitialization4!dimension=Symdimension
		module procedure SymDimInitialization5!Symdimension= (/ QuanNum /)
	end interface
	public::operator(.fuse.)
	interface operator(.fuse.)!overwrite the function in type(Dimension)
		module procedure fuseDimension_val
		module procedure fuseDimension_vec
	end interface
	public::operator(.split.)!overwrite the function in type(Dimension)
	interface operator(.split.)
		module procedure splitDimension2
		module procedure splitDimension3
		module procedure splitDimensionAll
	end interface
	public::operator(.sub.)!overwrite the function in type(Dimension)
	interface operator(.sub.)
		module procedure getSubDim2
		module procedure getSubDim3
		module procedure getSubDim4
		module procedure getSubDim2_name
	end interface
	public::operator(.subdim.)!overwrite the function in type(Dimension)
	interface operator(.subdim.)
		module procedure getSubDim2
		module procedure getSubDim3
		module procedure getSubDim4
		module procedure getSubDim2_name
	end interface
	
	
	public::operator(+)
	interface operator(+)
		module procedure Dimadd
		module procedure DimaddQuanNum
		module procedure QuanNumAddDim
		module procedure QuanNumAddQuanNum
	end interface
	
	public::sdimpermute_forwards,Sdimpermute_backwards,Sdimpermute_forwards_index,Sdimpermute_backwards_index
	public::MPI_send_SymDimension,MPI_BCAST_SymDimension
contains

	logical function ifzeroDeg(dimen)
		class(SymDimension),intent(in)::dimen
		integer::i,dimi
		ifzeroDeg=.false.
		if(.not.dimen%getQNflag())return
		do dimi=1,dimen%getRank()
			do i=1,dimen%getQNlength(dimi)
				if(dimen%getDeg(dimi,i).eq.0)then
					ifzeroDeg=.true.
					return
				end if
			end do
		end do
		return
	end function 	
	
	real*4 function getMaxQN1(Dimen,ith)result(getMaxQN)
		class(SymDimension),intent(in) :: Dimen
		integer,intent(in)::ith
		if(.not.Dimen%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		getMaxQN=Dimen%QN(ith)%getMaxQN()
		return
	end function
	real*4 function getMaxQN2(Dimen,legname)result(getMaxQN)
		class(SymDimension),intent(in) :: Dimen
		character(len=*),intent(in)::legname
		integer::ith
		if(.not.Dimen%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		ith=Dimen%FindOrder(legname)
		getMaxQN=Dimen%QN(ith)%getMaxQN()
		return
	end function
	
	real*4 function getMinQN1(Dimen,ith)result(getMinQN)
		class(SymDimension),intent(in) :: Dimen
		integer,intent(in)::ith
		if(.not.Dimen%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		getMinQN=Dimen%QN(ith)%getMinQN()
		return
	end function

	real*4 function getMinQN2(Dimen,legname)result(getMinQN)
		class(SymDimension),intent(in) :: Dimen
		character(len=*),intent(in)::legname
		integer::ith
		if(.not.Dimen%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		ith=Dimen%FindOrder(legname)
		getMinQN=Dimen%QN(ith)%getMinQN()
		return
	end function


	subroutine index2QNinfo_ith(maxDeg,outQN,outDegi,inindex)
		integer,intent(in)::maxDeg(:)
		integer,intent(inout)::outQN,outDegi
		integer,intent(in)::inindex
		integer::lenQN,i,Qni,degi
		lenQN=size(maxDeg)
		outQN=1
		degi=0
		do i=1,inindex
			degi=degi+1
			if(degi.gt.maxDeg(outQN))then
				outQN=outQN+1
				degi=1
			end if
			if(Qni.gt.lenQN)then
				call writemess('ERROR, the index is larger the max index of the dimension')
				call error_stop
			end if
			outDegi=degi
		end do
		return
	end subroutine


	subroutine index2QNinfo1(Dimen,outQN,outDegi,inindex)
		class(SymDimension),intent(in)::Dimen
		integer,intent(inout)::outQN(:),outDegi(:)
		integer,intent(in)::inindex(:)
		integer::i,rank
		rank=Dimen%getRank()
		if(size(outQN).lt.rank)then
			call writemess(' The length of array can not store the output data')
			call writemess('size(outQN)='+size(outQN))
			call writemess('the length output data='+rank)
			call error_stop
		end if
		if(size(outDegi).lt.rank)then
			call writemess(' The length of array can not store the output data')
			call writemess('size(outDegi)='+size(outDegi))
			call writemess('the length output data='+rank)
			call error_stop
		end if
		do i=1,rank
			call index2QNinfo_ith(Dimen%getDeg(i),outQN(i),outDegi(i),inindex(i))
		end do
		return
	end subroutine

	subroutine index2QNinfo2(Dimen,outQN,outDegi,inindex)
		class(SymDimension),intent(in)::Dimen
		integer,intent(inout)::outDegi(:)
		real*4,intent(inout)::outQN(:)
		integer::iQN
		integer,intent(in)::inindex(:)
		integer::i,rank
		rank=Dimen%getRank()
		if(size(outQN).lt.rank)then
			call writemess(' The length of array can not store the output data')
			call writemess('size(outQN)='+size(outQN))
			call writemess('the length output data='+rank)
			call error_stop
		end if
		if(size(outDegi).lt.rank)then
			call writemess(' The length of array can not store the output data')
			call writemess('size(outDegi)='+size(outDegi))
			call writemess('the length output data='+rank)
			call error_stop
		end if
		do i=1,rank
			call index2QNinfo_ith(Dimen%getDeg(i),iQN,outDegi(i),inindex(i))
			outQN(i)=Dimen%getQN(i,iQN)
		end do
		return
	end subroutine

	
	
! subroutines and funcitons for SymDimension

	integer function getSymDimRule(Dimen,ith)
		class(SymDimension),intent(in) :: Dimen
		integer,intent(in)::ith
		if(.not.Dimen%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		if(ith.gt.Dimen%outlenDimData())then
			if(Dimen%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting Rule of the dimension")
			call error_stop()
		end if
		getSymDimRule=Dimen%QN(ith)%getRule()
		return
	end function

	integer function getFermiArrow0(Dimen,ith)
		class(SymDimension),intent(in) :: Dimen
		integer,intent(in)::ith
		if(.not.Dimen%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		if(ith.gt.Dimen%outlenDimData())then
			if(Dimen%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting fermi-arrow of the dimension")
			call error_stop()
		end if
		getFermiArrow0=Dimen%QN(ith)%GetFermiArrow()
		return
	end function


	integer function getSymDimRuleChar(Dimen,w)
		class(SymDimension),intent(in) :: Dimen
		character(len=*),intent(in)::w
		integer::ith
		if(.not.Dimen%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		ith=Dimen%FindOrder(w)
		if(ith.gt.Dimen%outlenDimData())then
			if(Dimen%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting Rule of the dimension")
			call error_stop()
		end if
		getSymDimRuleChar=Dimen%QN(ith)%getRule()
		return
	end function
	integer function getFermiArrowChar(Dimen,w)
		class(SymDimension),intent(in) :: Dimen
		character(len=*),intent(in)::w
		integer::ith
		if(.not.Dimen%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		ith=Dimen%FindOrder(w)
		if(ith.gt.Dimen%outlenDimData())then
			if(Dimen%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting fermi-arrow of the dimension")
			call error_stop()
		end if
		getFermiArrowChar=Dimen%QN(ith)%GetFermiArrow()
		return
	end function

	function getSymDimAllRule(Dimen)
		integer,allocatable::getSymDimAllRule(:)
		class(SymDimension),intent(in) :: Dimen
		integer::i,rank
		rank=Dimen%outlenDimData()
		allocate(getSymDimAllRule(rank))
		do i=1,rank
			getSymDimAllRule(i)=Dimen%QN(i)%getRule()
		end do
		return
	end function
	function getAllFermiArrow(Dimen)
		integer,allocatable::getAllFermiArrow(:)
		class(SymDimension),intent(in) :: Dimen
		integer::i,rank
		rank=Dimen%outlenDimData()
		allocate(getAllFermiArrow(rank))
		do i=1,rank
			getAllFermiArrow(i)=Dimen%QN(i)%getRule()
		end do
		return
	end function
	
	
	
	integer function outSymDimIndex(Dimen,ith,Qnumber)
		class(SymDimension),intent(in) :: Dimen
		integer,intent(in)::ith
		real*4,intent(in)::Qnumber
		outSymDimIndex=Dimen%QN(ith)%getIndex(Qnumber)
		return
	end function
	
	function outSymDimAllIndex(Dimen,Qnumber)
		integer,allocatable::outSymDimAllIndex(:)
		class(SymDimension),intent(in) :: Dimen
		real*4,intent(in)::Qnumber(:)
		integer::I,lendata
		lendata=Dimen%outlendimData()
		if(size(Qnumber).ne.lendata)then
			call writemess("in getting Index of the Symdimension when inputting quantum number")
			call error_stop()
		end if
		allocate(outSymDimAllIndex(lendata))
		do i=1,lendata
			outSymDimAllIndex(i)=Dimen%QN(i)%getIndex(Qnumber(i))
		end do
		return
	end function

	
	integer function getlenOfSymDimQuanNum(dimen,ith)
		class(SymDimension),intent(in) ::dimen
		integer,intent(in)::ith
		if(.not.Dimen%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		if(ith.gt.Dimen%outlenDimData())then
			if(Dimen%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting length Of Quantum Number of the dimension")
			call error_stop()
		end if
		getlenOfSymDimQuanNum=dimen%QN(ith)%getQNlength()
		return
	end function
	logical function getQNflag(Symdim)
		class(SymDimension),intent(in) ::Symdim
		getQNflag=Symdim%QNflag
		return
	end function
	subroutine SymDimInitialization(inoutQN,inQN)
		type(SymDimension),intent(inout) ::inoutQN
		type(SymDimension),intent(in) :: inQN
		integer::i,lenDimData
		inoutQN%Dimension=inQN%Dimension
		inoutQN%QNflag=inQN%QNflag
		if(inoutQN%QNflag)then
			lenDimData=inQN%outlenDimData()
			call allocateCheck(inoutQN%QN,lenDimData)
			do i=1,lenDimData
				inoutQN%QN(i)=inQN%QN(i)
			end do
		end if
		return
	end subroutine
	subroutine SymDimInitialization2(inoutQN,dimdata)
		type(SymDimension),intent(inout) ::inoutQN
		integer,intent(in) :: dimdata(:)
		integer::i,lenDimData
		inoutQN%dimension=dimdata
		inoutQN%QNflag=.false.
		return
	end subroutine
	subroutine SymDimInitialization3(inoutQN,inQN)
		type(SymDimension),intent(inout) ::inoutQN
		type(Dimension),intent(in) :: inQN
		integer::i,lenDimData
		inoutQN%Dimension=inQN
		inoutQN%QNflag=.false.
		return
	end subroutine
	subroutine SymDimInitialization4(inoutQN,inQN)
		type(Dimension),intent(inout) ::inoutQN
		type(SymDimension),intent(in) :: inQN
		integer::i,lenDimData
		inoutQN=inQN%dimension
		return
	end subroutine
	subroutine SymDimInitialization5(inoutQN,inQN)
		type(SymDimension),intent(inout) ::inoutQN
		type(QuanNum),intent(in) :: inQN(:)
		integer::i,lenDimData
		integer,allocatable::dimdata(:)
		lenDimData=size(inQN)
		allocate(dimdata(lenDimData))
		do i=1,lenDimData
			dimdata(i)=inQN(i)%getQNlength()
		end do
		call allocatecheck(inoutQN%QN,lenDimData)
		inoutQN%dimension=dimdata
		inoutQN%QN=inQN
		inoutQN%QNFlag=.true.
		return
	end subroutine
	subroutine QuanNumInitialization2(dimen,inQN)
		type(SymDimension),intent(inout) ::dimen
		type(QuanNum),intent(in) :: inQN
		integer::l
		l=inQN%getQNlength()
		dimen=(/l/)
		call dimen%setQN(1,inQN)
		return
	end subroutine
	subroutine emptyDimension(Dimen)!overwrite the subroutine in dimension.f90
		class(SymDimension),intent(inout) ::Dimen
		integer::i
		if(allocated(Dimen%QN))then
			do i=1,size(Dimen%QN)
				call Dimen%QN(i)%empty()
			end do
		end if
		call Dimen%dimension%empty()
		Dimen%QNflag=.false.
		return
	end subroutine
	subroutine cleanDimension(Dimen)!overwrite the subroutine in dimension.f90
		class(SymDimension),intent(inout) ::Dimen
		integer::i
		if(allocated(Dimen%QN))then
			do i=1,size(Dimen%QN)
				call Dimen%QN(i)%deallocate()
			end do
			deallocate(Dimen%QN)
		end if
		call Dimen%dimension%deallocate()
		Dimen%QNflag=.false.
		return
	end subroutine
	subroutine setSymDimensionQN(inoutQN,ith,QuantumNumber,degeneracy)
		class(SymDimension),intent(inout) ::inoutQN
		integer,intent(in)::ith
		real*4,intent(in)::QuantumNumber(:)
		integer,intent(in),optional::degeneracy(:)
		integer::dimi,lenQN
		if(ith.gt.inoutQN%outlenDimData())then
			if(inoutQN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess('ERROR in SETTING Quantum Number')
			call writemess('The index is larger than the length of the dimension')
			call error_stop()
		end if
		if(.not.inoutQN%getQNflag())call allocateCheck(inoutQN%QN,inoutQN%outlenDimData())
		dimi=inoutQN%dim(ith)
		lenQN=size(QuantumNumber)
		if(dimi.ne.lenQN)then
			call writemess("The length of Quantum Number do not match the dimension when setting the QuantumNumber to SymDimension")
			call writemess("The length of Quantum Number that is setting to the ith dimension should be equal to the ith diemsnion")
			call error_stop()
		end if
		call inoutQN%QN(ith)%SetQN(QuantumNumber,degeneracy)
		inoutQN%QNflag=.true.
		return
	end subroutine
	subroutine setSymDimQuanNum(inoutQN,ith,QuantumNumber)
		class(SymDimension),intent(inout) ::inoutQN
		integer,intent(in)::ith
		type(QuanNum),intent(in)::QuantumNumber
		integer::dimi,lenQN
		if(ith.gt.inoutQN%outlenDimData())then
			if(inoutQN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess('ERROR in SETTING Quantum Number')
			call writemess('The index is larger than the length of the dimension')
			call error_stop()
		end if
		if(.not.inoutQN%getQNflag())call allocateCheck(inoutQN%QN,inoutQN%outlenDimData())
		dimi=inoutQN%dim(ith)
		lenQN=QuantumNumber%getQNlength()
		if(dimi.ne.lenQN)then
			call writemess("The length of Quantum Number do not match the dimension when setting the QuantumNumber to SymDimension")
			call writemess("The length of Quantum Number that is setting to the ith dimension should be equal to the ith diemsnion")
			call error_stop()
		end if
		inoutQN%QN(ith)=QuantumNumber
		inoutQN%QNflag=.true.
		return
	end subroutine
	subroutine setSymDimensionRule(inoutQN,ith,rule)
		class(SymDimension),intent(inout) ::inoutQN
		integer,intent(in)::ith,rule
		if(ith.gt.inoutQN%outlenDimData())then
			if(inoutQN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if 
			call writemess('ERROR in SETTING Quantum Number')
			call writemess('The index is larger than the length of the dimension')
			call error_stop()
		end if
		if(.not.inoutQN%getQNflag())then
			call writemess("One should set the Quantum Number before setting the symmetry rule")
			call error_stop()
		end if
		call inoutQN%QN(ith)%SetRule(rule)
		return
	end subroutine
	subroutine setSymDimensionArrow(inoutQN,ith,Arrow)
		class(SymDimension),intent(inout) ::inoutQN
		integer,intent(in)::ith,Arrow
		if(ith.gt.inoutQN%outlenDimData())then
			if(inoutQN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if 
			call writemess('ERROR in SETTING fermi-arrow')
			call writemess('The index is larger than the length of the dimension')
			call error_stop()
		end if
		if(.not.inoutQN%getQNflag())then
			call writemess("One should set the Quantum Number before setting the fermi-arrow")
			call error_stop()
		end if
		call inoutQN%QN(ith)%SetFermiArrow(Arrow)
		return
	end subroutine
	subroutine setSymDimensionArrow_char(inoutQN,w,Arrow)
		class(SymDimension),intent(inout) ::inoutQN
		character(len=*),intent(in)::w
		integer,intent(in)::Arrow
		integer::ith
		ith=inoutQN%FindOrder(w)
		if(ith.gt.inoutQN%outlenDimData())then
			if(inoutQN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if 
			call writemess('ERROR in SETTING fermi-arrow')
			call writemess('The index is larger than the length of the dimension')
			call error_stop()
		end if
		if(.not.inoutQN%getQNflag())then
			call writemess("One should set the Quantum Number before setting the fermi-arrow")
			call error_stop()
		end if
		call inoutQN%QN(ith)%SetFermiArrow(Arrow)
		return
	end subroutine
	subroutine setSymDimensionRule_char(inoutQN,w,rule)
		class(SymDimension),intent(inout) ::inoutQN
		character(len=*),intent(in)::w
		integer,intent(in)::rule
		integer::ith
		ith=inoutQN%FindOrder(w)
		if(ith.gt.inoutQN%outlenDimData())then
			if(inoutQN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if 
			call writemess('ERROR in SETTING Quantum Number')
			call writemess('The index is larger than the length of the dimension')
			call error_stop()
		end if
		if(.not.inoutQN%getQNflag())then
			call writemess("One should set the Quantum Number before setting the symmetry rule")
			call error_stop()
		end if
		call inoutQN%QN(ith)%SetRule(rule)
		return
	end subroutine
	subroutine setAllSymDimensionRule(inoutQN,rule)
		class(SymDimension),intent(inout) ::inoutQN
		integer,intent(in)::rule(:)
		integer::i
		if(inoutQN%outlenDimData().eq.0)then
			call writemess('There is no Data in the Symdimension, initial first before setting rule')
			call error_stop()
		end if 
		if(size(rule).lt.inoutQN%outlenDimData())then
			call writemess('ERROR in SETTING Quantum rule')
			call writemess('The length of input rule is smaller than the length of the dimension')
			call error_stop()
		end if
		if(.not.inoutQN%getQNflag())then
			call writemess("One should set the Quantum Number before setting the symmetry rule")
			call error_stop()
		end if
		do i=1,inoutQN%outlenDimData()
			call inoutQN%QN(i)%SetRule(rule(i))
		end do
		return
	end subroutine
	subroutine setAllSymDimensionArrow(inoutQN,Arrow)
		class(SymDimension),intent(inout) ::inoutQN
		integer,intent(in)::Arrow(:)
		integer::i
		if(inoutQN%outlenDimData().eq.0)then
			call writemess('There is no Data in the Symdimension, initial first before setting fermi-arrow')
			call error_stop()
		end if 
		if(size(Arrow).lt.inoutQN%outlenDimData())then
			call writemess('ERROR in SETTING fermi-arrow')
			call writemess('The length of  input fermi-arrow is smaller than the length of the dimension')
			call error_stop()
		end if
		if(.not.inoutQN%getQNflag())then
			call writemess("One should set the Quantum Number before setting the fermi-arrow")
			call error_stop()
		end if
		do i=1,inoutQN%outlenDimData()
			call inoutQN%QN(i)%SetFermiArrow(Arrow(i))
		end do
		return
	end subroutine

	subroutine setSymDimensionDeg(inoutQN,ith,deg)
		class(SymDimension),intent(inout) ::inoutQN
		integer,intent(in)::ith
		integer,intent(in)::deg(:)
		if(ith.gt.inoutQN%outlenDimData())then
			if(inoutQN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess('ERROR in SETTING Quantum Number')
			call writemess('The index is larger than the length of the dimension')
			call error_stop()
		end if
		if(.not.inoutQN%getQNflag())then
			call writemess("One should set the Quantum Number before setting the degenerace")
			call error_stop()
		end if
		call inoutQN%QN(ith)%setDeg(deg)
		inoutQN%QNflag=.true.
		return
	end subroutine
	
	
	integer function getSymSymDimensionDeg(QN,ith,jth)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::ith,jth
		if(.not.QN%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		if(ith.gt.QN%outlenDimData())then
			if(QN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting the degeneracy Of Quantum Number of the dimension")
			call error_stop()
		end if
		getSymSymDimensionDeg=QN%QN(ith)%getDeg(jth)
		return
	end function

	integer function getSymSymDimensionDegQN(QN,ith,jth)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::ith
		real*4,intent(in)::jth
		if(.not.QN%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		if(ith.gt.QN%outlenDimData())then
			if(QN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting the degeneracy Of Quantum Number of the dimension")
			call error_stop()
		end if
		getSymSymDimensionDegQN=QN%QN(ith)%getDeg(jth)
		return
	end function
	
	function getSymSymDimensionDegVec(QN,vec)
		integer,allocatable::getSymSymDimensionDegVec(:)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::vec(:)
		integer::lenvec,i
		if(.not.QN%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		lenvec=size(vec)
		if(lenvec.ne.QN%getRank())then
			call writemess("ERROR in Getting the degeneracy")
			call error_stop
		end if
		allocate(getSymSymDimensionDegVec(lenvec))
		do i=1,lenvec
			getSymSymDimensionDegVec(i)=QN%QN(i)%getDeg(vec(i))
		end do
		return
	end function

	function getSymSymDimensionDegQVec(QN,vec)
		integer,allocatable::getSymSymDimensionDegQVec(:)
		class(SymDimension),intent(in)::QN
		real*4,intent(in)::vec(:)
		integer::lenvec,i
		if(.not.QN%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		lenvec=size(vec)
		if(lenvec.ne.QN%getRank())then
			call writemess("ERROR in Getting the degeneracy")
			call error_stop
		end if
		allocate(getSymSymDimensionDegQVec(lenvec))
		do i=1,lenvec
			getSymSymDimensionDegQVec(i)=QN%QN(i)%getDeg(vec(i))
		end do
		return
	end function


	type(Dimension) function getDeg_dimension(QN,vec)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::vec(:)
		integer::NameFlag,i
		getDeg_dimension=getSymSymDimensionDegVec(QN,vec)
		NameFlag=QN%outNameFlag()
		if(NameFlag.eq.0)return
		do i=1,QN%outlenDimData()
			call getDeg_dimension%setName(i,QN%outName(i))
		end do
	end function
	type(Dimension) function getDeg_dimensionQ(QN,vec)
		class(SymDimension),intent(in)::QN
		real*4,intent(in)::vec(:)
		integer::NameFlag,i
		getDeg_dimensionQ=getSymSymDimensionDegQVec(QN,vec)
		NameFlag=QN%outNameFlag()
		if(NameFlag.eq.0)return
		do i=1,QN%outlenDimData()
			call getDeg_dimensionQ%setName(i,QN%outName(i))
		end do
	end function
		
		
		
	real*4 function getSymSymDimensionQN(QN,ith,jth)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::ith,jth
		if(.not.QN%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		if(ith.gt.QN%outlenDimData())then
			if(QN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting the Quantum Number of the dimension")
			call error_stop()
		end if
		getSymSymDimensionQN=QN%QN(ith)%getQN(jth)
		return
	end function
	
	function getSymSymDimensionQNVec(QN,vec)
		real*4,allocatable::getSymSymDimensionQNVec(:)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::vec(:)
		integer::lenvec,i
		lenvec=size(vec)
		if(lenvec.ne.QN%getRank())then
			call writemess("ERROR in Getting the Quantum Number ")
			call writemess('lenvec='+lenvec+',QN%getRank()='+QN%getRank())
			call error_stop
		end if
		allocate(getSymSymDimensionQNVec(lenvec))
		do i=1,lenvec
			getSymSymDimensionQNVec(i)=QN%QN(i)%GetQN(vec(i))
		end do
		return
	end function
	type(QuanNum) function getSymSymDimensionQuanNum(QN,ith)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::ith
		if(ith.gt.QN%outlenDimData())then
			call writemess("ERROR in Getting the Quantum Number ")
			call writemess('ith='+ith+',QN%getRank()='+QN%getRank())
			call error_stop
		end if
		getSymSymDimensionQuanNum=QN%QN(ith)
		return
	end function
	type(QuanNum) function getSymSymDimensionQuanNumChar(QN,charmane)result(getSymSymDimensionQuanNum)
		class(SymDimension),intent(in)::QN
		character(len=*),intent(in)::charmane
		integer::ith
		ith=QN%FindOrder(charmane)
		if(ith.gt.QN%outlenDimData())then
			call writemess("ERROR in Getting the Quantum Number ")
			call writemess('ith='+ith+',QN%getRank()='+QN%getRank())
			call error_stop
		end if
		if(ith.eq.0)then
			call writemess("No this name in getting quantum number ")
			call error_stop
		end if
		getSymSymDimensionQuanNum=QN%QN(ith)
		return
	end function
	subroutine outAllSymDimDeg(QN,ith,output)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::ith
		integer,allocatable,intent(inout)::output(:)
		if(.not.QN%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		if(ith.gt.QN%outlenDimData())then
			if(QN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting the degeneracy Of Quantum Number of the dimension")
			call error_stop()
		end if
		call QN%QN(ith)%outDegeneracy(output)
		return
	end subroutine
	function outAllSymDimDegfun(QN,ith)result(outAllSymDimDeg)
		integer,allocatable::outAllSymDimDeg(:)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::ith
		integer::leng
		if(.not.QN%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		if(ith.gt.QN%outlenDimData())then	
			if(QN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting the degeneracyof Quantum Number of the dimension")
			call error_stop()
		end if
		leng=QN%getQNlength(ith)
		allocate(outAllSymDimDeg(leng))
		call QN%QN(ith)%outDegeneracy(outAllSymDimDeg)
		return
	end function
	subroutine outAllSymDimQN(QN,ith,output)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::ith
		real*4,allocatable,intent(inout)::output(:)
		if(.not.QN%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		if(ith.gt.QN%outlenDimData())then
			if(QN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting the Quantum Number of the dimension")
			call error_stop()
		end if
		call QN%QN(ith)%outQuanNum(output)
		return
	end subroutine
	function outAllSymDimQNfun(QN,ith)result(outAllSymDimQN)
		real*4,allocatable::outAllSymDimQN(:)
		class(SymDimension),intent(in)::QN
		integer,intent(in)::ith
		integer::leng
		if(.not.QN%getQNflag())then
			call writemess("There is no Quantum Number")
			call error_stop()
		end if
		if(ith.gt.QN%outlenDimData())then
			if(QN%outlenDimData().eq.0)then
				call writemess('There is no Data in the Symdimension')
				call error_stop()
			end if
			call writemess("The input index is larger then the length of the dimension")
			call writemess("in getting the Quantum Number of the dimension")
			call error_stop()
		end if
		leng=QN%getQNlength(ith)
		allocate(outAllSymDimQN(leng))
		call QN%QN(ith)%outQuanNum(outAllSymDimQN)
		return
	end function
	type(QuanNum) function outSymDimQuanNum(Dimen,ith)
		class(SymDimension),intent(in)::Dimen
		integer,intent(in)::ith
		outSymDimQuanNum=Dimen%QN(ith)
		return
	end function
	function outALLSymDimQuanNum(Dimen)
		type(QuanNum),allocatable::outALLSymDimQuanNum(:)
		class(SymDimension),intent(in)::Dimen
		allocate(outALLSymDimQuanNum(Dimen%outlenDimData()))
		outALLSymDimQuanNum=Dimen%QN
		return
	end function
	
	subroutine getSubDimQN(Dimen,ith,QN,outlen)
		type(SymDimension),intent(in)::Dimen
		integer,intent(in)::ith
		integer,intent(in)::outlen
		type(QuanNum),intent(inout)::QN(outlen)
		integer::boundary(2),i,Dlen,k
		call Dimen%getSubDimboundary(ith,boundary)
		Dlen=boundary(2)-boundary(1)
		if(Dlen.ne.outlen)then
			call writemess("ERROR in getSubDimQN,SymDimension.f90")
			call error_stop()
		end if
		k=1
		do i=boundary(1)+1,boundary(2)
			QN(k)=Dimen%QN(i)
			k=k+1
		end do
		return
	end subroutine
		
	
	subroutine Dprint(Dimen,uni)!overwrite the subroutine in dimension.f90
		class(SymDimension),intent(in) ::Dimen
		integer,optional,intent(in)::uni
		integer::i
		character*500::w
		character*100::w2
		call Dimen%Dimension%print(uni)
		if(present(uni))then
			if(Dimen%QNflag)then
				w='The Symmetry Rule of the dimension are:'
				write(uni,*)trim(w)
				w=Dimen%getRule(1)
				do i=2,Dimen%getRank()
					w=w+','+Dimen%getRule(i)
				end do
				write(uni,*)trim(w)
				w='The fermi-arrow of the dimension are:'
				write(uni,*)trim(w)
				w=Dimen%getFermiArrow(1)
				do i=2,Dimen%getRank()
					w=w+','+Dimen%getFermiArrow(i)
				end do
				write(uni,*)trim(w)
			else
				write(uni,*)"Do not Set the Quantum Numbers and their degeneracies"
			end if
		else
			if(Dimen%QNflag)then
				w='The Symmetry Rule of the dimension are:'
				call writemess(w,-1)
				w=Dimen%getRule(1)
				do i=2,Dimen%getRank()
					w=w+','+Dimen%getRule(i)
				end do
				call writemess(w,-1)
				w='The fermi-arrow of the dimension are:'
				call writemess(w,-1)
				w=Dimen%getFermiArrow(1)
				do i=2,Dimen%getRank()
					w=w+','+Dimen%getFermiArrow(i)
				end do
				call writemess(w,-1)
			else
				call writemess("Do not Set the Quantum Numbers and their degeneracies",-1)
			end if
		end if
		do i=1,Dimen%getRank()
			if(Dimen%getnameFlag().ne.0)then
				w2=Dimen%getName(i)
			else
				w2=i
			end if
			call Dimen%QN(i)%printQN(w2,uni)
		end do
		return
	end subroutine
	
	subroutine Dprint2(Dimen,uni)!overwrite the subroutine in dimension.f90
		class(SymDimension),intent(in) ::Dimen
		integer,optional,intent(in)::uni
		integer::i
		call Dimen%dimension%info(uni)
		if(present(uni))then
			write(uni,*)"QNflag ",Dimen%QNflag
			if(.not.Dimen%QNflag)return
			write(uni,*)"Below are the Quantum Numbers and their degeneracies"
			do i=1,Dimen%outlenDimData()
				write(uni,*)"index: ",i
				call Dimen%QN(i)%print(uni)
			end do
			write(uni,*)"-----"
		else
			call writemess("QNflag:"+Dimen%QNflag,-1)
			if(.not.Dimen%QNflag)return
			call writemess("Below are the Quantum Numbers and their degeneracies",-1)
			do i=1,Dimen%outlenDimData()
				call writemess("index: "+i,-1)
				call Dimen%QN(i)%print()
			end do
			call writemess("------",-1)
		end if
		return
	end subroutine
	
	subroutine readdimension(dimen,uni)
		class(Symdimension),intent(inout)::dimen
		integer,intent(in)::uni
		character*50::notused
		integer::i,j,k,lenQuanNum,rule
		integer,allocatable::degeneracy(:)
		real*4,allocatable::QuanNum(:)
		real*4::maxQN
		call dimen%dimension%read(uni)
		read(uni,*)notused,Dimen%QNflag
		if(.not.Dimen%QNflag)return
		read(uni,*)notused
		call allocateCheck(Dimen%QN,Dimen%outlenDimData())
		do i=1,dimen%outlenDimData()
			read(uni,*)notused
			call Dimen%QN(i)%read(uni)
		end do
		read(uni,*)notused
		return
	end subroutine
	
!*******  function or subroutine for name   **************	
!	return the inde  dimension	,outpout in a type(dimenison)
	type(Symdimension) function  getSubDim2(Dimen,inde)
		type(Symdimension),intent(in) :: Dimen
		integer,intent(in) :: inde
		integer::i,j,k,Dlen,boundary(2)
		getSubDim2%dimension=Dimen%dimension.sub.inde
		if(.not.Dimen%QNflag)return
		call Dimen%getSubDimboundary(inde,boundary)
		Dlen=boundary(2)-boundary(1)
		if(Dlen.ne.getSubDim2%dimension%outlenDimData())then
			call writemess("ERROR in getSubDim2,SymDimension.f90")
			call error_stop()
		end if
		call allocateCheck(getSubDim2%QN,Dlen)
		getSubDim2%QNflag=.true.
		k=1
		do i=boundary(1)+1,boundary(2)
			getSubDim2%QN(k)=Dimen%QN(i)
			k=k+1
		end do
		return
	end function
	
	
	type(Symdimension) function  getSubDim3(Dimen,inde) 
		type(Symdimension),intent(in) :: Dimen
		integer,intent(in) :: inde(2)
		integer::ith(2),Dlen
		getSubDim3%dimension=Dimen%dimension.sub.inde
		if(.not.Dimen%QNflag)return
		getSubDim3%QNflag=.true.
		call getSubDim3_index_routine(ith,Dimen%Dimension,inde) 
		Dlen=ith(2)-ith(1)+1
		allocate(getSubDim3%QN(Dlen))
		getSubDim3%QN=Dimen%QN(ith(1):ith(2))
		return
	end function
	type(Symdimension) function  getSubDim4(Dimen)
		type(Symdimension),intent(in) :: Dimen
		getSubDim4=Dimen
		return
	end function
	type(Symdimension) function  getSubDim2_name(Dimen,w)
		type(Symdimension),intent(in) :: Dimen
		CHARACTER(len=*),intent(in)::w
		integer::inde
		inde=Dimen%FindOrder(w)
		getSubDim2_name=getSubDim2(Dimen,inde)
		return
	end function
		
!**************** fuse   ****************
!		combine two index of the Tensor,which is con_index and con_index+1
	type(SymDimension) function fuseDimension_val(Dimen,inde)result(fuseDim)!overwrite
		integer,intent(in) :: inde
		type(SymDimension),intent(in) :: Dimen
		fuseDim=dimen%Dimension.fuse.(inde)
		fuseDim%QNflag=dimen%QNflag
		if(fuseDim%QNflag)then
			allocate(fuseDim%QN(fuseDim%outlenDimData()))
			fuseDim%QN=dimen%QN
		end if
		return
	end function
	type(SymDimension) function fuseDimension_vec(dimen,vector)result(fuseDim)
		integer,intent(in) ::vector(2)
		type(SymDimension),intent(in) :: dimen
		fuseDim=dimen%Dimension.fuse.vector
		fuseDim%QNflag=dimen%QNflag
		if(fuseDim%QNflag)then
			allocate(fuseDim%QN(fuseDim%outlenDimData()))
			fuseDim%QN=dimen%QN
		end if
		return
	end function		
	type(SymDimension) function splitDimension2(dimen,vector)result(splitDimension)
		type(SymDimension),intent(in) :: dimen
		integer,intent(in) ::vector(2)
		splitDimension=dimen%Dimension.split.vector
		splitDimension%QNflag=dimen%QNflag
		if(splitDimension%QNflag)then
			allocate(splitDimension%QN(splitDimension%outlenDimData()))
			splitDimension%QN=dimen%QN
		end if
		return
	end function
			
	type(SymDimension) function splitDimension3(dimen,de_index)result(splitDimension)
		type(SymDimension),intent(in) :: dimen
		integer,intent(in) :: de_index
		splitDimension=dimen%Dimension.split.de_index
		splitDimension%QNflag=dimen%QNflag
		if(splitDimension%QNflag)then
			allocate(splitDimension%QN(splitDimension%outlenDimData()))
			splitDimension%QN=dimen%QN
		end if
		return
	end function
			
	type(SymDimension) function splitDimensionAll(Dimen)result(splitDimension)
		type(SymDimension),intent(in) :: Dimen
		splitDimension=.split.dimen%Dimension
		splitDimension%QNflag=dimen%QNflag
		if(splitDimension%QNflag)then
			allocate(splitDimension%QN(splitDimension%outlenDimData()))
			splitDimension%QN=dimen%QN
		end if
		return
	end function	
	
	
	
	type(SymDimension) function Dimpermute(dimen,v)
		class(SymDimension),intent(in) :: Dimen
		integer,intent(in):: v(:)
		integer::i,datalen,subDlen,k
		type(QuanNum),allocatable :: QN(:)
		Dimpermute%Dimension=dimen%Dimpermute(v)
		if(.not.dimen%getQNFlag()) return
		datalen=dimen%outlenDimData()
		allocate(Dimpermute%QN(datalen))
		Dimpermute%QNflag=dimen%QNflag
		
		if(dimen%out_sample_flag())then
			do i=1,datalen
				Dimpermute%QN(i)=dimen%QN(v(i))
			end do
			return
		end if
		k=1
		do i=1,Dimen%getRank()
			subDlen=dimen%getsubDimlen(v(i))
			call allocateCheck(QN,subDlen)
			call getSubDimQN(Dimen,v(i),QN,subDlen)
			Dimpermute%QN(k:subDlen+k-1)=QN(1:subDlen)
			k=k+subDlen
		end do
		return
	end function	
	! order is [ith,1,2,3...]
	subroutine  SDimpermute_forwards(outdim,dimen,ith)
		type(SymDimension),intent(inout)::outdim
		type(SymDimension),intent(in) :: Dimen
		integer,intent(in):: ith
		integer,allocatable::order(:)
		integer::length,i
		length=Dimen%getRank()
		allocate(order(length))
		order(1)=ith
		do i=2,ith
			order(i)=i-1
		end do
		do i=ith+1,length
			order(i)=i
		end do
		outdim=Dimen%spermute(order)
		return
	end subroutine
! order is [1,2,3...,n,ith]
	subroutine SDimpermute_backwards(outdim,dimen,ith)
		type(SymDimension),intent(inout)::outdim
		type(SymDimension),intent(in) :: Dimen
		integer,intent(in):: ith
		integer,allocatable::order(:)
		integer::length,i
		length=Dimen%getRank()
		allocate(order(length))
		order(length)=ith
		do i=1,ith-1
			order(i)=i
		end do
		do i=ith,length-1
			order(i)=i+1
		end do
		outdim=Dimen%spermute(order)
		return
	end subroutine
! oeder is [2,3,4,...,ith,1,ith+1,...]
	subroutine SDimpermute_forwards_index(outdim,dimen,ith)
		type(SymDimension),intent(inout)::outdim
		type(SymDimension),intent(in) :: Dimen
		integer,intent(in):: ith
		integer,allocatable::order(:)
		integer::length,i
		length=Dimen%getRank()
		allocate(order(length))
		order(ith)=1
		do i=1,ith-1
			order(i)=i+1
		end do
		do i=ith+1,length
			order(i)=i
		end do
		outdim=Dimen%spermute(order)
		return
	end subroutine
	! oeder is [1,2,3,4,...,ith,n,ith+1,...,n-1]
	subroutine SDimpermute_backwards_index(outdim,dimen,ith)
		type(SymDimension),intent(inout)::outdim
		type(SymDimension),intent(in) :: Dimen
		integer,intent(in):: ith
		integer,allocatable::order(:)
		integer::length,i
		length=Dimen%getRank()
		allocate(order(length))
		order(ith)=length
		do i=1,ith-1
			order(i)=i
		end do
		do i=ith+1,length
			order(i)=i-1
		end do
		outdim=Dimen%spermute(order)
		return
	end subroutine

!*******  function or subroutine for name   **************	
	
	
	type(Tensor) function outAllName(dimen,w,typ)
		class(SymDimension),intent(in)::dimen
		character(len=*),intent(in)::w
		character(len=*),intent(in),optional::typ
		logical::goon
		character(len=max_len_of_char_in_TData),allocatable::outchar(:)
		goon=dimen%Dimension%outSomedimensionName(outchar,w,typ)
		if(goon)then
			outAllName=outchar
		else
			call outAllName%empty()
		end if
		return
	end function
	
	
	type(SymDimension) function  DimAdd(Dimen,Dimen2)
		type(SymDimension),intent(in) :: Dimen,Dimen2
		integer::l1,l2
		DimAdd%Dimension=Dimen%Dimension+Dimen2%Dimension
		if(Dimen%getQNflag() .and. Dimen2%getQNflag())then
			l1=Dimen%outlenDimData()
			l2=Dimen2%outlenDimData()
			allocate(DimAdd%QN(l1+l2))
			DimAdd%QN(1:l1)=Dimen%QN(1:l1)
			DimAdd%QN(l1+1:)=Dimen2%QN(1:l2)
			DimAdd%QNflag=.true.
			return
		end if
		if( (.not.Dimen%getQNflag()) .and. (.not.Dimen2%getQNflag()))then
			DimAdd%QNflag=.false.
			return
		end if
		call writemess('ERRO in SymDimension + SymDimension')
		call writemess('There is no Quantum Number in one or both Symdimension')
		call error_stop()
	end function
	type(SymDimension) function  DimAddQuanNum(Dimen,Quan)result(DimAdd)
		type(SymDimension),intent(in) :: Dimen
		type(QuanNum),intent(in)::Quan
		integer::l1,l2
		l2=Quan%getQNlength()
		if(l2.eq.0)then
			call writemess(' There is no Data in the Quantun number when dim1 + QN')
			call error_stop
		end if
		DimAdd%dimension=Dimen%dimension+(/l2/)
		if(Dimen%getQNFlag())then
			l1=Dimen%outlenDimData()
			allocate(DimAdd%QN(l1+1))
			DimAdd%QN(1:l1)=Dimen%QN(1:l1)
			DimAdd%QN(l1+1)=Quan
			DimAdd%QNflag=.true.
			return
		end if
		call writemess('ERRO in SymDimension + SymDimension')
		call writemess('There is no Quantum Number in one or both Symdimension')
		call error_stop()
	end function
	type(SymDimension) function  QuanNumAddDim(Quan,Dimen)result(DimAdd)
		type(QuanNum),intent(in)::Quan
		type(SymDimension),intent(in) :: Dimen
		integer::l1,l2
		l1=Quan%getQNlength()
		if(l1.eq.0)then
			call writemess(' There is no Data in the Quantun number when QN + dim')
			call error_stop
		end if
		DimAdd%dimension=(/l1/) + Dimen%dimension
		if(Dimen%getQNFlag())then
			l2=Dimen%outlenDimData()
			allocate(DimAdd%QN(l2+1))
			DimAdd%QN(1)=Quan
			DimAdd%QN(2:l2+1)=Dimen%QN(1:l2)
			DimAdd%QNflag=.true.
			return
		end if
		call writemess('ERRO in SymDimension + SymDimension')
		call writemess('There is no Quantum Number in one or both Symdimension')
		call error_stop()
	end function	
	type(SymDimension) function  QuanNumAddQuanNum(Quan1,Quan2)result(DimAdd)
		type(QuanNum),intent(in)::Quan1,Quan2
		integer::l1,l2
		l1=Quan1%getQNlength()
		l2=Quan2%getQNlength()
		if(l1.eq.0)then
			call writemess(' There is no Data in the Quantun number when QN + QN')
			call error_stop
		end if
		if(l2.eq.0)then
			call writemess(' There is no Data in the Quantun number when QN + QN')
			call error_stop
		end if
		DimAdd=(/l1,l2/)
		allocate(DimAdd%QN(2))
		DimAdd%QN(1)=Quan1
		DimAdd%QN(2)=Quan2
		DimAdd%QNflag=.true.
		return
	end function	
	
	
	subroutine RNDimRoutine(Dimen) !ouverwrite
		class(SymDimension),intent(inout) :: Dimen
		integer::i,lenD,lenNewD
		integer,allocatable::Dimindex(:)
		type(QuanNum),allocatable::QN(:)
		call writemess('Do no finsihed this part of killLeg')
		call error_stop
		if(.not.if_original_dim(dimen)) then
			write(*,*)"ERROR IN RNDim,in Diemnsion.f90"
			call error_stop()
		end if
		lenD=dimen%outLenDimData()
		allocate(Dimindex(lenD))
		lenNewD=0
		do i=1,lenD
			if(Dimen%dim(i).ne.1)then
				lenNewD=lenNewD+1
				Dimindex(lenNewD)=i
			end if
		end do
		if(.not.Dimen%QNflag)return
		if(lenNewD.eq.0)then
			Dimen%QNflag=.false.
			deallocate(Dimen%QN)
			return
		end if
		allocate(QN(lenNewD))
		do i=1,lenNewD
			QN(i)=dimen%QN(Dimindex(i))
		end do
		deallocate(Dimen%QN)
		allocate(Dimen%QN(lenNewD))
		Dimen%QN=QN
		call Dimen%Dimension%RNDim()
		if(size(QN).ne.Dimen%outlenDimData())then
			call writemess('ERROR in killLeg in SymDimension',-1)
			call error_stop
		end if
		return
	end subroutine
	
	subroutine RNDimRoutineint(Dimen,notkillleg,killFlag) !ouverwrite
		class(SymDimension),intent(inout) :: Dimen
		integer,intent(in)::notkillleg
		character(len=*),intent(in),optional::killFlag
		integer::i,lenD,lenNewD
		integer,allocatable::Dimindex(:)
		type(QuanNum),allocatable::QN(:)
		call writemess('Do no finsihed this part of killLeg')
		call error_stop
		if(.not.if_original_dim(dimen)) then
			write(*,*)"ERROR IN RNDim,in Diemnsion.f90"
			call error_stop()
		end if
		lenD=dimen%outLenDimData()
		allocate(Dimindex(lenD))
		lenNewD=0
		if(present(killFlag))then
			if(killFlag.equ.'kill')then
				do i=1,lenD
					if(i.ne.notkillleg)then
						lenNewD=lenNewD+1
						Dimindex(lenNewD)=i
					end if
				end do
			else
				do i=1,lenD
					if((Dimen%dim(i).ne.1).or.(i.eq.notkillleg))then
						lenNewD=lenNewD+1
						Dimindex(lenNewD)=i
					end if
				end do
			end if
		else
			do i=1,lenD
				if((Dimen%dim(i).ne.1).or.(i.eq.notkillleg))then
					lenNewD=lenNewD+1
					Dimindex(lenNewD)=i
				end if
			end do
		end if
		call Dimen%Dimension%killLeg()
		if(.not.Dimen%QNflag)return
		if(lenNewD.eq.0)then
			Dimen%QNflag=.false.
			deallocate(Dimen%QN)
			return
		end if
		allocate(QN(lenNewD))
		do i=1,lenNewD
			QN(i)=dimen%QN(Dimindex(i))
		end do
		deallocate(Dimen%QN)
		allocate(Dimen%QN(lenNewD))
		Dimen%QN=QN
		if(size(QN).ne.Dimen%outlenDimData())then
			call writemess('ERROR in killLeg in SymDimension',-1)
			call error_stop
		end if
		return
	end subroutine



!U(1) symmetry
!|S,rs>,S=-1,0,1,degeneracy: 1,2,1
!in the symmetry base
! |-1,1>,|0,1>,|0,2>,|1,1>
!transfrom to non-symmety index will be
! |1>  , |2>  ,|3>  , |4>
!input s, output imin and imax
! example,input s=0,output imin=2,imax=3, that is output a array of (2,3)
! example,input s=1,output imin=4,imax=4, that is output a array of (4,4)
!
! in Parity
! |p,d>, p=+1,-1. degeneracy are 3 for -1 and 4 for 1
! in the symmetry base
! |-1,1>,|-1,2>,|-1,3>,|1,1>,|1,2>,|1,3>,|1,4>
!transfrom to non-symmety index will be
! |1>  , |2>  ,|3>  , |4> ,  |5>,  |6>,   |7>
! input p=1, output imin=4,imax=7, that is output a array of (4,7)
!
	function QN2nonSyminde_one(dimen,ith,inQN)result(vec)
		integer,allocatable::vec(:)
		class(SymDimension),intent(in)::dimen
		real*4,intent(in)::inQN
		integer,intent(in)::ith
		integer::imin,imax
		integer::i
		allocate(vec(2))
		if(.not.dimen%if_original_dim())then
			write(*,*)"The SymDimension should be in its original SymDimension,QN2nonSyminde"
			stop
		end if
		imin=1
		do i=1,dimen%getQNlength(ith)
			if(abs(dimen%getQN(ith,i)-inQN).gt.1d-15)then
				imin=imin+dimen%getDeg(ith,i)
			else
				imax=dimen%getDeg(ith,i)
				exit
			end if
		end do
		imax=imax+imin-1
		vec=(/imin,imax/)
		return
	end function
!U(1) symmetry
!|S,rs>,S=-1,0,1,degeneracy: 1,2,1
!in the symmetry base
! |-1,1>,|0,1>,|0,2>,|1,1>
!transfrom to non-symmety index will be
! |1>  , |2>  ,|3>  , |4>
!input i,where s are store in order, i is the index of s, output imin and imax
! example,input i=2, which mean s(2)=0,output imin=2,imax=3
! example,input i=3, which mean s(3)=1,output imin=4,imax=4
	function QN2nonSyminde_one2(dimen,ith,inde)result(vec)
		integer,allocatable::vec(:)
		class(SymDimension),intent(in)::dimen
		integer,intent(in)::inde
		integer,intent(in)::ith
		integer::imin,imax
		integer::i
		allocate(vec(2))
		if(.not.dimen%if_original_dim())then
			write(*,*)"The SymDimension should be in its original SymDimension,QN2nonSyminde"
			stop
		end if
		imin=1
		do i=1,inde-1
			imin=imin+dimen%getDeg(ith,i)
		end do
		imax=dimen%getDeg(ith,inde)+imin-1
		vec=(/imin,imax/)
		return
	end function
	
!|S,rs>,S=-1,0,1,degeneracy: 1,2,1
!in the symmetry base
! |-1,1>,|0,1>,|0,2>,|1,1>
!transfrom to non-symmety index will be
! |1>  , |2>  ,|3>  , |4>
!input s, output imin and imax,thay are array
! example,dimen=[2,2,3],QN=[0.5,0.5,1],degeneracy=[(1,1),(1,1),(1,2,1)]
! input [0.5,-0.5,0] output imin:[1,2,2] imax:[1,2,3] there store in a array
!    / 1 2 2 \
!    \ 1 2 3 /
	function QN2nonSyminde(dimen,QN) result(vec)
		integer,allocatable::vec(:,:)
		class(SymDimension),intent(in)::dimen
		real,intent(in)::QN(:)
		integer::i,lendim,temp(2)
		lendim=dimen%outlendimdata()
		allocate(vec(2,lendim))
		do i=1,dimen%getRank()
			temp=dimen%NonSymIndex(i,QN(i))
			vec(1,i)=temp(1)
			vec(2,i)=temp(2)
		end do
		return
	end function
	function QN2nonSyminde2(dimen,ith) result(vec)
		integer,allocatable::vec(:,:)
		class(SymDimension),intent(in)::dimen
		integer,intent(in)::ith(:)
		integer::i,lendim,temp(2)
		lendim=dimen%outlendimdata()
		allocate(vec(2,lendim))
		do i=1,dimen%getRank()
			temp=dimen%NonSymIndex(i,ith(i))
			vec(1,i)=temp(1)
			vec(2,i)=temp(2)
		end do
		return
	end function
	
	

!**********************************************************************
!**********************************************************************
!	the code below is for MPI
!**********************************************************************
		
	
	
	subroutine MPI_send_SymDimension(Dimen1,Dimen2,ID1,ID2,ierr,MPIcommon)
		type(SymDimension),intent(in)::Dimen1
		type(SymDimension),intent(inout)::Dimen2
		integer,intent(in)::ID1,ID2
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,istatus(MPI_STATUS_SIZE),mpi_comm,lendata,i
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
		
		call MPI_send_Dimension(Dimen1%Dimension,Dimen2%Dimension,ID1,ID2,ierr,MPIcommon)
!**************************flag************************************************				
		if(proID.eq.ID1) then
			call mpi_send(Dimen1%QNflag,1,MPI_logical,ID2,tag,MPI_Comm,ierr)
			if(.not.Dimen1%QNflag)then
				return
			else
				lendata=Dimen1%outlenDimData()
			end if
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Dimen2%QNflag,1,MPI_logical,ID1,tag,MPI_Comm,istatus,ierr)
			if(.not.Dimen2%QNflag)then
				return
			else
				lendata=Dimen2%outlenDimData()
				call allocateCheck(Dimen2%QN,lendata)
				!allocate(Dimen2%QN(lendata))
			end if
		end if		
		do i=1,lendata
			call MPI_send_QuanNum(Dimen1%QN(i),Dimen2%QN(i),ID1,ID2,ierr,MPIcommon)
		end do
		return
	end subroutine
	
	subroutine MPI_BCAST_SymDimension(Dimen,ID,ierr,MPIcommon)
		type(SymDimension),intent(inout)::Dimen
		integer,intent(in)::ID
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,istatus(MPI_STATUS_SIZE),mpi_comm,lendata,i
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
		
		call MPI_BCAST_Dimension(Dimen%Dimension,ID,ierr,MPIcommon)
		
		call MPI_BCAST(Dimen%QNflag,1,MPI_logical,ID,mpi_comm,ierr)		
		if(Dimen%QNflag)then
			lendata=Dimen%outlenDimData()
			if(proId.ne.ID)then
				call allocateCheck(Dimen%QN,lendata)
			end if
		else
			return
		end if
		do i=1,lendata
			call MPI_BCAST_QuanNum(Dimen%QN(i),ID,ierr,MPIcommon)
		end do
		return
	end subroutine
		

end module










