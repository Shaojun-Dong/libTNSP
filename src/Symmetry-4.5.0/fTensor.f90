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


!
!  It can be speed up when the tensor is to fuse into a matrix
!
!    for exampe  if symmetry rule of T is [ -1, |-1,-1]
!    fuse the fist leg    -->  [-1,-1,-1]
!    fuse the 2 and 3 leg -->  [-1,-1]
!    then as the tensor is not diag, so the code will call reverseSymmetryRule(T)
!
!   if do as following:
!    fuse the fist leg    -->  [-1,-1,-1]
!    fuse the 2 and 3 leg,and set the rule as 1 -->  [-1,1]
!	 then as the tensor is  diag, so the code will NOT call reverseSymmetryRule(t)
!


module fermiTensor
	use SymTensor_type
	use SymDimension_typede
	use Tensor_type
	use Dimension_typede
	use Tools
	use mpi
	use QuantumNumber_Type
	implicit none
	private

	logical::mointer_order_flag=.true.
	integer::fermi_factor=-1
	!if mointer_order_flag=.false. Do not moniter the rule=0
	!
	
	public::fTensor
	type,extends (SymTensor) :: fTensor
		
	contains

		generic,public::SVD=>fSVDRoutine,fSVDRoutineNum,fSVDRoutineNumMaxValue,fSVDRoutineMaxValue,&
					fSVDNameRoutine,fSVDNameNumRoutine,fSVDNameNumMaxValueRoutine,fSVDNameMaxValueRoutine&
					,fSVDNameNumRoutineLeft,fSVDNameRoutineLeft,&
					fSVDNameNumMaxValueRoutineLeft,fSVDNameMaxValueRoutineLeft,fSVDNameRoutineLeft2
		procedure::fSVDRoutine,fSVDRoutineNum,fSVDRoutineNumMaxValue,fSVDRoutineMaxValue,&
					fSVDNameRoutine,fSVDNameNumRoutine,fSVDNameNumMaxValueRoutine,fSVDNameMaxValueRoutine&
					,fSVDNameNumRoutineLeft,fSVDNameRoutineLeft,fSVDNameNumMaxValueRoutineLeft&
					,fSVDNameMaxValueRoutineLeft,fSVDNameRoutineLeft2

		generic,public::SVDkill=>fSVDRoutine_kill_inData,fSVDRoutineNum_kill_inData,fSVDRoutineNumMaxValue_kill_inData&
						,fSVDRoutineMaxValue_kill_inData,fSVDNameRoutine_kill_inData,fSVDNameNumRoutine_kill_inData,&
						fSVDNameNumMaxValueRoutine_kill_inData,fSVDNameMaxValueRoutine_kill_inData&
					    ,fSVDNameNumRoutineLeft_kill_inData,fSVDNameRoutineLeft_kill_inData,&
						fSVDNameNumMaxValueRoutineLeft_kill_inData,fSVDNameMaxValueRoutineLeft_kill_inData,&
						fSVDNameRoutineLeft2_kill_inData
		procedure::fSVDRoutine_kill_inData,fSVDRoutineNum_kill_inData,fSVDRoutineNumMaxValue_kill_inData&
						,fSVDRoutineMaxValue_kill_inData,fSVDNameRoutine_kill_inData,fSVDNameNumRoutine_kill_inData,&
						fSVDNameNumMaxValueRoutine_kill_inData,fSVDNameMaxValueRoutine_kill_inData&
					    ,fSVDNameNumRoutineLeft_kill_inData,fSVDNameRoutineLeft_kill_inData,&
						fSVDNameNumMaxValueRoutineLeft_kill_inData,fSVDNameMaxValueRoutineLeft_kill_inData,&
						fSVDNameRoutineLeft2_kill_inData



        !************************************************************************************
        !************************************************************************************
		!                              Old Function 
		!
		generic,public::SVDfRoutine=>fSVDNameNumMaxValueOld,fSVDRoutine,fSVDRoutineNum,fSVDNameNumRoutineLeft
		procedure::fSVDNameNumMaxValueOld
		generic,public::SVDfTensor=>SVDNameNumOld
		!generic,public::randomfTensor=>set_random_SymTensor_all,set_random_SymTensor_val,set_random_SymTensor_vec
		!generic,public::setfValue=>set_Symelement_vec,set_Symelement_val
		generic,public::QRfTensor=>QRfTensor_name,QRfTensor_name2
		generic,public::LQfTensor=>LQfTensor_name,LQfTensor_name2
		generic,public::QRfRoutine=>fQRdecomposition,fQRRoutineNameLeft,fQRRoutineName
		generic,public::LQfRoutine=>fLQdecomposition,fLQRoutineNameLeft,fLQRoutineName
		procedure::SVDNameNumOld
		procedure::QRfTensor_name,QRfTensor_name2
		procedure::LQfTensor_name,LQfTensor_name2
		!
		!************************************************************************************
        !************************************************************************************


		generic,public::QR=>fQRdecomposition,fQRRoutineNameLeft,fQRRoutineName
		procedure::fQRdecomposition,fQRRoutineNameLeft,fQRRoutineName
		generic,public::LQ=>fLQdecomposition,fLQRoutineNameLeft,fLQRoutineName
		procedure::fLQdecomposition,fLQRoutineNameLeft,fLQRoutineName

		generic,public::QRkill=>fQRdecomposition_kill_inData,fQRRoutineNameLeft_kill_inData,fQRRoutineName_kill_inData
		procedure::fQRdecomposition_kill_inData,fQRRoutineNameLeft_kill_inData,fQRRoutineName_kill_inData
		generic,public::LQkill=>fLQdecomposition_kill_inData,fLQRoutineNameLeft_kill_inData,fLQRoutineName_kill_inData
		procedure::fLQdecomposition_kill_inData,fLQRoutineNameLeft_kill_inData,fLQRoutineName_kill_inData




		generic,public::FermiSplit=>FermiSplitfTensor2,FermiSplitfTensor
		generic,public::FermiFuse=>FermiFusefTensor,FermiFusefTensor2,FermiFusefTensor3,FermiFusefTensor4&
							,FermiFusefTensorRule,FermiFusefTensorRule2,FermiFusefTensorRule3,FermiFusefTensorRule4



		procedure,public::reorder=>DimOrder
		! reorder the Tensor, order the leg as the order in allTensorName(:)
		procedure,public::ReverseFermiArrow=>Reverse_Fermi_Rule_specify3

		generic,public::contract=>fcontract_name_routine,fcontract_name_routine1,fcontract_name_routine2,&
						fcontract_name_routine4,fcontract_name_routine5,fcontract_name_routine6
		procedure::fcontract_name_routine,fcontract_name_routine1,fcontract_name_routine2
		procedure::fcontract_name_routine4,fcontract_name_routine5,fcontract_name_routine6
		procedure::contract_name_ownlegs_routine,contract_ownlegs_routine

		procedure::fermi_element_vec,fermi_element_vec2,fermi_element_vec3
		generic,public::fi=>fermi_element_vec,fermi_element_vec2,fermi_element_vec3

		procedure,public::Notfermi_permute
		procedure::permutefo_routine
		procedure::permutefo_name_routine
		procedure::permutefo_vec_routine
		procedure::permutefo_vec_name_routine
		procedure::permutefo_Tensor_routine
		procedure::permutation_routine
		procedure::permutation_name_routine
		procedure::permutation_Tensor_routine
		procedure::permuteback_routine
		procedure::permuteback_name_routine
		procedure::permuteback_vec_routine
		procedure::permuteback_vec_name_routine
		procedure::permuteback_Tensor_routine
		procedure::FermiSplitfTensor
		procedure::FermiSplitfTensor2
		procedure::FermiFusefTensor
		procedure::FermiFusefTensor2
		procedure::FermiFusefTensor3
		procedure::FermiFusefTensor4
		procedure::FermiFusefTensorRule
		procedure::FermiFusefTensorRule2
		procedure::FermiFusefTensorRule3
		procedure::FermiFusefTensorRule4
	end type fTensor
	
	public::Reverse_Fermi_Rule
	interface Reverse_Fermi_Rule
		module procedure Reverse_Fermi_Rule_specify
		module procedure Reverse_Fermi_Rule_specify2
		module procedure Reverse_Fermi_Rule_specify3
		module procedure Reverse_Fermi_Rule_specify4
		module procedure Reverse_Fermi_Rule_specify5
		module procedure Reverse_Fermi_Rule1
	end interface

	public::ReverseFermiArrow
	interface ReverseFermiArrow
		module procedure Reverse_Fermi_Rule_specify
		module procedure Reverse_Fermi_Rule_specify2
		module procedure Reverse_Fermi_Rule_specify3
		module procedure Reverse_Fermi_Rule_specify4
		module procedure Reverse_Fermi_Rule_specify5
		module procedure Reverse_Fermi_Rule_specify6
		module procedure Reverse_Fermi_Rule_specify7
		module procedure Reverse_Fermi_Rule1
	end interface
	
	public::un_set_mointer_order_flag
	public::set_mointer_order_flag
	
	
	
	public::expm
	interface expm
		module procedure  expmfTensor
	end interface
	
	public::operator(.pf.)
	interface operator(.pf.)
		module procedure FermiPermuteForWard
		module procedure FermiPermuteForWard_vec
		module procedure FermiPermuteForWard_name
		module procedure FermiPermuteForWard_name_vec
		module procedure FermiPermuteForWard_Tensor
	end interface
	public::operator(.pb.)
	interface operator(.pb.)
		module procedure FermiPermuteBackWard
		module procedure FermiPermuteBackWard_vec
		module procedure FermiPermuteBackWard_name
		module procedure FermiPermuteBackWard_name_vec
		module procedure FermiPermuteBackWard_Tensor
	end interface
	public::operator(.p.)
	interface operator(.p.)
		module procedure FermiPermutation
		module procedure FermiPermutation_name
		module procedure FermiPermutation_Tensor
	end interface
	
	
	public::contract
	interface contract
		module procedure contract_noName
		module procedure contract_noName2
		module procedure contract_Name!In put dimension as character
		module procedure contract_Name2
		module procedure fcontract_name_ownlegs
		module procedure fcontract_ownlegs
	end interface

	public::operator(*)
	interface operator(*)
		module procedure ProductTensor
		module procedure multiply_number_real8
		module procedure multiply_number_com4
		module procedure multiply_number_real4
		
	end interface
	public::operator(/)
	interface operator(/)
		module procedure divide_real8
		module procedure divide_real4
		module procedure divide_Tensor
	end interface
	
	public::operator(+)
	interface operator(+)
		module procedure add_fTensor
	end interface
	
	public::operator(-)
	interface operator(-)
		module procedure minu_fTensor
	end interface
	
	public::operator(.H.)
	interface operator(.H.)
		module procedure Htranspose! Htranspose, and the symmetry rule will go inverse
	end interface
	public::operator(.Hn.)
	interface operator(.Hn.)
		module procedure Htranspose2! Htranspose, and the symmetry rule will go inverse,and Rename all TensorName
	end interface
	
	public::operator(.subdim.)!overwrite the function in type(Dimension)
	interface operator(.subdim.)
		module procedure getSubDim2
		module procedure getSubDim3
		module procedure getSubDim4
		module procedure getSubDim2_name
	end interface
	
	public::operator(.con.)
	interface operator(.con.)
		module procedure conjugate! conjugate
	end interface
	
	
	
	public::operator(.kron.)!direct Product,support any rank tensor and keep their TensorName,see more in help/operator
	interface operator(.kron.)
		module procedure directProductTensor
	end interface
	
	public::operator(.x.)
	interface operator(.x.)! dot product conjugating the first vector,The Tensor will be regard as a vector
		module procedure TdotTensor
	end interface
	public::operator(.sx.)
	interface operator(.sx.)
		module procedure sdotTensor
	end interface
	public::operator(.dx.)
	interface operator(.dx.)
		module procedure ddotTensor
	end interface
	public::operator(.cx.)
	interface operator(.cx.)
		module procedure cdotTensor
	end interface
	public::operator(.zx.)
	interface operator(.zx.)
		module procedure zdotTensor
	end interface


	public::operator(.dot.)
	interface operator(.dot.)! dot product DO NOTconjugat the first vector,The Tensor will be regard as a vector
		module procedure TdotTensor
	end interface
	public::operator(.sdot.)
	interface operator(.sdot.)
		module procedure sdotTensor
	end interface
	public::operator(.ddot.)
	interface operator(.ddot.)
		module procedure ddotTensor
	end interface
	public::operator(.cdot.)
	interface operator(.cdot.)
		module procedure cdotTensor
	end interface
	public::operator(.zdot.)
	interface operator(.zdot.)
		module procedure zdotTensor
	end interface
	
	public::assignment(=)
	interface assignment(=)
		module procedure assignmentTen
		module procedure assignmentreal4
		module procedure assignmentreal8
		module procedure assignmentcom4
		module procedure assignmentcom8
		module procedure fTensorToSymTensor
		module procedure fTensorToTensor
		module procedure SymTensorTofTensor!Do not check the Symmetry rule
		module procedure TensorTofTensor!Do not check the Symmetry rule
		module procedure assignmentfTensorArray
	end interface
	
	public::MPI_BCAST_fTensor,MPI_send_fTensor
	public::MPI_SUM_fTensor!Do not check the input fTensor
	public::set_fermi_symmetry
	interface MPI_SUM_fTensor
		module procedure MPI_SUM_fTensor1
	end interface


contains

	subroutine set_fermi_symmetry(group)
		character(len=*),intent(in)::group
		call set_symmetry(group)
		return
	end subroutine

	subroutine set_fermi_factor(factor)
		integer,intent(in)::factor
		fermi_factor=factor
		call writemess('Set fermi_factor as '+(' '+factor))
		return
	end subroutine

	
	subroutine assignmentTen(T,T2)
		type(fTensor),intent(inout) ::T
		type(fTensor),intent(in) :: T2
		T%SymTensor=T2%SymTensor
		return
	end subroutine
	subroutine assignmentreal4(r,T2)
		real*4,intent(inout)::r
		type(fTensor),intent(in) :: T2
		r=T2%SymTensor
		return
	end subroutine
	subroutine assignmentreal8(r,T2)
		real*8,intent(inout)::r
		type(fTensor),intent(in) :: T2
		r=T2%SymTensor
		return
	end subroutine
	subroutine assignmentcom4(r,T2)
		complex(kind=4),intent(inout)::r
		type(fTensor),intent(in) :: T2
		r=T2%SymTensor
		return
	end subroutine
	subroutine assignmentcom8(r,T2)
		complex(kind=8),intent(inout)::r
		type(fTensor),intent(in) :: T2
		r=T2%SymTensor
		return
	end subroutine
	subroutine fTensorToSymTensor(T,T2)
		type(SymTensor),intent(inout) ::T
		type(fTensor),intent(in) :: T2
		T=T2%SymTensor
		return
	end subroutine
	subroutine fTensorToTensor(T,T2)
		type(Tensor),intent(inout) ::T
		type(fTensor),intent(in) :: T2
		T=T2%SymTensor
		return
	end subroutine
	subroutine SymTensorTofTensor(T,T2)
		type(fTensor),intent(inout) ::T
		type(SymTensor),intent(in) :: T2
		T%SymTensor=T2
		return
	end subroutine
	subroutine TensorTofTensor(T,T2)
		type(fTensor),intent(inout) ::T
		type(Tensor),intent(in) :: T2
		if(.not.T%getFlag())then
			call writemess('Allocate fTensor before setting value')
			call error_stop()
		end if
		T%SymTensor=T2
		return
	end subroutine

	subroutine assignmentfTensorArray(T,T2)
		type(fTensor),intent(inout) ::T(:)
		type(fTensor),intent(in) :: T2(:)
		integer::length,i
		length=size(T2)
		if(size(T).lt.length)then
			write(*,*)"ERROR in assignment of two fTensor array "
			write(*,*)"T1(:)=T2(:),size(T1)<size(T2)"
			write(*,*)size(T),length
			call error_stop()
		end if
		do i=1,length
			T(i)=T2(i)
		end do
		return
	end subroutine
	
	
	type(Symdimension) function  getSubDim2(T,inde)
		type(fTensor),intent(in) :: T
		integer,intent(in)::inde
		getSubDim2=T%SymDimension.subdim.inde
		return
	end function
	type(Symdimension) function  getSubDim3(T,inde)
		type(fTensor),intent(in) :: T
		integer,intent(in)::inde(2)
		getSubDim3=T%SymDimension.subdim.inde
		return
	end function
	type(Symdimension) function  getSubDim4(T)
		type(fTensor),intent(in) :: T
		getSubDim4=T%SymDimension
		return
	end function
	type(Symdimension) function  getSubDim2_name(T,w)
		type(fTensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::w
		integer::inde
		inde=T%FindOrder(w)
		getSubDim2_name=T%SymDimension.subdim.inde
		return
	end function
	
	
	
	
	
	subroutine Notfermi_permute(fT,index)
		class(fTensor),intent(inout)::fT
		integer,intent(in)::index(:)
		call fT%SymTensor%permute(index)
		return
	end subroutine
	
	type(fTensor) function fermi_element_vec(fH,QNindex,Degindex)result(subfH)
		class(fTensor),intent(in)::fH
		integer,intent(in)::QNindex(:),Degindex(:)
		type(Tensor)::Hi
		integer::rank,i
		real*8::Hdata
		type(QuanNum),allocatable::QN(:)
		Hi=fH%i(QNindex)
		Hdata=Hi%i(Degindex)
		rank=fH%getRank()
		allocate(QN(rank))
		do i=1,rank
			call QN(i)%setQN([fH%getQN(i,QNindex(i))])
			call QN(i)%setDeg([1])
			call QN(i)%setRule(fH%getRule(i))
			call QN(i)%setFermiArrow(fH%getFermiArrow(i))
		end do
		call subfH%allocate(QN,fH%getType())
		call subfH%setValue(1,Tensor([Hdata]))
		do i=1,rank
			call subfH%setName(i,fH%getName(i))
		end do
		return
	end function

	type(fTensor) function fermi_element_vec2(fH,index)result(subfH)
		class(fTensor),intent(in)::fH
		integer,intent(in)::index(:)
		integer,allocatable::QNindex(:),Degindex(:)
		type(Tensor)::Hi
		integer::rank,i
		real*8::Hdata
		type(QuanNum),allocatable::QN(:)
		rank=fH%getRank()
		allocate(QNindex(rank))
		allocate(Degindex(rank))
		call fH%index2QNinfo(QNindex,Degindex,index)
		!subfH2= subfH(fH,QNindex,Degindex)
		Hi=fH%i(QNindex)
		Hdata=Hi%i(Degindex)
		allocate(QN(rank))
		do i=1,rank
			call QN(i)%setQN([fH%getQN(i,QNindex(i))])
			call QN(i)%setDeg([1])
			call QN(i)%setRule(fH%getRule(i))
			call QN(i)%setFermiArrow(fH%getFermiArrow(i))
		end do
		call subfH%allocate(QN,fH%getType())
		call subfH%setValue(1,Tensor([Hdata]))
		do i=1,rank
			call subfH%setName(i,fH%getName(i))
		end do
		return
	end function

	type(fTensor) function fermi_element_vec3(fH,QNindex,Degindex)result(subfH)
		class(fTensor),intent(in)::fH
		integer,intent(in)::Degindex(:)
		real*4,intent(in)::QNindex(:)
		type(Tensor)::Hi
		integer::rank,i
		real*8::Hdata
		type(QuanNum),allocatable::QN(:)
		Hi=fH%i(QNindex)
		Hdata=Hi%i(Degindex)
		rank=fH%getRank()
		allocate(QN(rank))
		do i=1,rank
			call QN(i)%setQN([QNindex(i)])
			call QN(i)%setDeg([1])
			call QN(i)%setRule(fH%getRule(i))
			call QN(i)%setFermiArrow(fH%getFermiArrow(i))
		end do
		call subfH%allocate(QN,fH%getType())
		call subfH%setValue(1,Tensor([Hdata]))
		do i=1,rank
			call subfH%setName(i,fH%getName(i))
		end do
		return
	end function
	
	
	
	type(fTensor) function FermiPermutation(T,newOrder)
		type(fTensor),intent(in) :: T
		integer,intent(in)::newOrder(:)
		integer,allocatable ::inde(:)
		integer::lenOrder,i,j
		lenorder=size(newOrder)-1
		allocate(inde(lenorder))
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		FermiPermutation=T
		do i=lenorder,1,-1
			call permutefo_routine(FermiPermutation,inde(i))
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		return
	end function
	type(fTensor) function FermiPermutation_name(T,newOrderchar)result(Res)
		type(fTensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::newOrderchar(:)
		integer,allocatable::newOrder(:)
		integer::i
		allocate(newOrder(size(newOrderchar)))
		newOrder=T%FindOrder(newOrderchar)
		Res= FermiPermutation(T,newOrder)
		return
	end function
	type(fTensor) function FermiPermutation_Tensor(T,Order)result(Res)
		type(fTensor),intent(in) :: T
		type(Tensor),intent(in)::Order
		select case(Order%getType())
			case (1)
				Res=FermiPermutation(T,Order%ii())
			case (7)
				Res=FermiPermutation_name(T,Order%ai())
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()
		end select
		return
	end function
	subroutine permutation_Tensor_routine(T,Tenorder)
		class(fTensor),intent(inout)::T
		type(Tensor),intent(in)::Tenorder
		character(len=max_len_of_char_in_TData),allocatable::indechar(:)
		integer,allocatable::indeint(:)
		select case (Tenorder%getType())
			case (1)
				allocate(indeint(Tenorder%getTotalData()))
				indeint=Tenorder
				call permutation_routine(T,indeint)
			case (7)
				allocate(indechar(Tenorder%getTotalData()))
				indechar=Tenorder
				call permutation_name_routine(T,indechar)
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()	
		end select
		return
	end subroutine

	subroutine permutefo_routine(T,inde)
		class(fTensor),intent(inout)::T
		integer,intent(in)::inde
		call T%SymTensor%forward(inde)
		call T%external(externalForWard,Tensor(inde))
		return
	end subroutine
	
	subroutine permutefo_name_routine(T,indechar)
		class(fTensor),intent(inout)::T
		character(len=*),intent(in)::indechar
		integer::ith,sizeindechar,i,lenintindex
		character(len=50)::indexname
		integer,allocatable::charToInt(:)
		if(long_Name_logi(indechar))then
			ith=T%FindOrder(indechar)
			call T%SymTensor%forward(indechar)
			call T%external(externalForWard,Tensor(ith))
		else
			sizeindechar=T%getRank()
			allocate(charToInt(sizeindechar))
			lenintindex=0
			do i=1,sizeindechar
				indexname=T%outTensorName(i)
				if(indechar.equ.indexname)then
					lenintindex=lenintindex+1
					charToInt(lenintindex)=i
				end if
			end do
			call permutefo_vec_routine(T,charToInt(1:lenintindex))
		end if
		return
	end subroutine
	
	subroutine permutefo_vec_routine(T,vec_)
		class(fTensor),intent(inout)::T
		integer,intent(in)::vec_(:)
		integer::lenVec,i,j
		integer::vec(size(vec_))
		lenVec=size(vec_)
		vec=vec_
		do i=lenVec,1,-1
			call permutefo_routine(T,vec(i))
			do j=1,i-1
				if(vec(j).lt.vec(i))then
					vec(j)=vec(j)+1
				end if
			end do
		end do
		return
	end subroutine
	
	subroutine permutefo_vec_name_routine(T,indechar)
		class(fTensor),intent(inout)::T
		character(len=*),intent(in)::indechar(:)
		integer::i
		do i=size(indechar),1,-1
			call permutefo_name_routine(T,indechar(i))
		end do
		return
	end subroutine

	Type(fTensor) function FermiPermuteForWard(T,ith)Result(Res)
		Type(fTensor),intent(in)::T
		integer,intent(in)::ith
		Res%SymTensor=T%SymTensor.pf.ith
		call Res%external(externalForWard,Tensor(ith))
		return
	end function
	
	Type(fTensor) function FermiPermuteForWard_vec(T,vec)Result(Res)
		Type(fTensor),intent(in)::T
		integer,intent(in)::vec(:)
		integer::i
		Res%SymTensor=T%SymTensor
		call permutefo_vec_routine(Res,vec)
		return
	end function
	Type(fTensor) function FermiPermuteForWard_name(T,a)Result(Res)
		Type(fTensor),intent(in)::T
		character(len=*),intent(in)::a
		Res%SymTensor=T%SymTensor
		call permutefo_name_routine(Res,a)
		return
	end function
	Type(fTensor) function FermiPermuteForWard_name_vec(T,a)Result(Res)
		Type(fTensor),intent(in)::T
		character(len=*),intent(in)::a(:)
		integer::i,ith
		Res%SymTensor=T%SymTensor
		call permutefo_vec_name_routine(Res,a)
		return
	end function
	
	type(fTensor) function FermiPermuteForWard_Tensor(T,Tenorder)result(permutefo)
		type(fTensor),intent(in)::T
		type(Tensor),intent(in)::Tenorder
		character(len=max_len_of_char_in_TData),allocatable::indechar(:)
		integer,allocatable::indeint(:)
		select case (Tenorder%getType())
			case (1)
				allocate(indeint(Tenorder%getTotalData()))
				indeint=Tenorder
				permutefo=FermiPermuteForWard_vec(T,indeint)
			case (7)
				allocate(indechar(Tenorder%getTotalData()))
				indechar=Tenorder
				permutefo=FermiPermuteForWard_name_vec(T,indechar)
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()	
		end select
		return
	end function
	subroutine permutefo_Tensor_routine(T,Tenorder)
		class(fTensor),intent(inout)::T
		type(Tensor),intent(in)::Tenorder
		character(len=max_len_of_char_in_TData),allocatable::indechar(:)
		integer,allocatable::indeint(:)
		select case (Tenorder%getType())
			case (1)
				allocate(indeint(Tenorder%getTotalData()))
				indeint=Tenorder
				call permutefo_vec_routine(T,indeint)
			case (7)
				allocate(indechar(Tenorder%getTotalData()))
				indechar=Tenorder
				call permutefo_vec_name_routine(T,indechar)
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()	
		end select
		return
	end subroutine
	
	subroutine externalForWard(block,lenofblock,dimen,info)
		integer::lenofblock
		type(Tensor)::block(lenofblock)
		Type(Tensor)::info
		Type(SymDimension)::dimen
		integer,allocatable::indices(:),maxinde(:),mininde(:)
		logical::goon,ifp
		integer::i,indexEnd,rank,iQN
		indexEnd=info%ii(1)
		if(indexEnd.eq.1)return
		rank=dimen%getRank()
		allocate(indices(rank))
		allocate(mininde(rank))
		allocate(maxinde(rank))
		maxinde=dimen%dim()
		indices=1
		mininde=1
		goon=.true.
		i=1
		do while (goon)
			if(block(i)%getFlag())then
				call QaunNumParity(iQN,dimen,1,indices(1) )
				if(iQN .eq.(-1))then
					call ifParity(ifp,dimen,indices,1,indexEnd,rank)
					if(ifp) then
						block(i)=fermi_factor*block(i)
					end if
				end if
			end if
			i=i+1
			goon=inde_counter(indices,mininde,maxinde,1)
		end do
		return
	end subroutine
	
	
	
	subroutine permuteback_routine(T,inde)
		class(fTensor),intent(inout)::T
		integer,intent(in)::inde
		call T%SymTensor%Backward(inde)
		call T%external(externalBackWard,Tensor(inde))
		return
	end subroutine
	
	subroutine permuteback_name_routine(T,indechar)
		class(fTensor),intent(inout)::T
		character(len=*),intent(in)::indechar
		integer::inde,i,sizeinde,lenintindex,j
		character*50::indexname
		integer,allocatable::indes(:)
		if(long_Name_logi(indechar))then
			inde=T%FindOrder(indechar)
			call T%SymTensor%Backward(inde)
			call T%external(externalBackWard,Tensor(inde))
		else
			sizeinde=T%getRank()
			allocate(indes(sizeinde))
			lenintindex=0
			do i=1,sizeinde
				indexname=T%outTensorName(i)
				if(indechar.equ.indexname)then
					lenintindex=lenintindex+1
					indes(lenintindex)=i
				end if
			end do
			call permuteback_vec_routine(T,indes(1:lenintindex))
		end if
		return
	end subroutine
	
	subroutine permuteback_vec_routine(T,vec_)
		class(fTensor),intent(inout)::T
		integer,intent(in)::vec_(:)
		integer::vec(size(vec_)),lenVec,i,j
		lenVec=size(vec_)
		vec=vec_
		do i=1,lenVec
			call permuteback_routine(T,vec(i))
			do j=lenVec,i+1,-1
				if(vec(j).gt.vec(i))then
					vec(j)=vec(j)-1
				end if
			end do
		end do
		return
	end subroutine
	
	subroutine permuteback_vec_name_routine(T,indechar)
		class(fTensor),intent(inout)::T
		character(len=*),intent(in)::indechar(:)
		integer::vec(size(indechar)),rank
		vec=T%FindOrder(indechar)
		call permuteback_vec_routine(T,vec)
		return
	end subroutine
	
	
	Type(fTensor) function FermiPermuteBackWard(T,ith)Result(Res)
		type(fTensor),intent(in)::T
		integer,intent(in)::ith
		Res%SymTensor=T%SymTensor.pb.ith
		call Res%external(externalBackWard,Tensor(ith))
		return
	end function
	Type(fTensor) function FermiPermuteBackWard_name(T,a)Result(Res)
		type(fTensor),intent(in)::T
		character(len=*),intent(in)::a
		integer::ith
		Res%SymTensor=T%SymTensor
		call permuteback_name_routine(Res,a)
		return
	end function
	Type(fTensor) function FermiPermuteBackWard_name_vec(T,a)Result(Res)
		type(fTensor),intent(in)::T
		character(len=*),intent(in)::a(:)
		integer::i,ith
		Res%SymTensor=T%SymTensor
		call permuteback_vec_name_routine(Res,a)
		return
	end function
	Type(fTensor) function FermiPermuteBackWard_vec(T,vec)Result(Res)
		type(fTensor),intent(in)::T
		integer,intent(in)::vec(:)
		integer::i
		Res%SymTensor=T%SymTensor
		call permuteback_vec_routine(Res,vec)
		return
	end function
	
	type(fTensor) function FermiPermuteBackWard_Tensor(T,Tenorder)result(permutefo)
		type(fTensor),intent(in)::T
		type(Tensor),intent(in)::Tenorder
		character(len=max_len_of_char_in_TData),allocatable::indechar(:)
		integer,allocatable::indeint(:)
		select case (Tenorder%getType())
			case (1)
				allocate(indeint(Tenorder%getTotalData()))
				indeint=Tenorder
				permutefo=FermiPermuteBackWard_vec(T,indeint)
			case (7)
				allocate(indechar(Tenorder%getTotalData()))
				indechar=Tenorder
				permutefo=FermiPermuteBackWard_name_vec(T,indechar)
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()	
		end select
		return
	end function
	subroutine permuteback_Tensor_routine(T,Tenorder)
		class(fTensor),intent(inout)::T
		type(Tensor),intent(in)::Tenorder
		character(len=max_len_of_char_in_TData),allocatable::indechar(:)
		integer,allocatable::indeint(:)
		select case (Tenorder%getType())
			case (1)
				allocate(indeint(Tenorder%getTotalData()))
				indeint=Tenorder
				call permuteback_vec_routine(T,indeint)
			case (7)
				allocate(indechar(Tenorder%getTotalData()))
				indechar=Tenorder
				call permuteback_vec_name_routine(T,indechar)
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()	
		end select
		return
	end subroutine
	subroutine externalBackWard(block,lenofblock,dimen,info)
		integer::lenofblock
		type(Tensor)::block(lenofblock),info
		Type(SymDimension)::dimen
		integer,allocatable::indices(:),maxinde(:),mininde(:)
		logical::goon,ifp
		integer::i,rank,indexStart,iQN
		indexStart=info%ii(1)
		rank=dimen%getRank()
		if(indexStart.eq.rank)return
		allocate(indices(rank))
		allocate(mininde(rank))
		allocate(maxinde(rank))
		maxinde=dimen%dim()
		indices=1
		mininde=1
		goon=.true.
		i=1
		do while (goon)
			if(block(i)%getFlag())then
				call QaunNumParity(iQN,dimen,rank,indices(rank))
				if(iQN.eq.(-1))then
					call ifParity(ifp,dimen,indices,indexStart,rank,rank)
					if(ifp)then
						block(i)=fermi_factor*block(i)
					end if
				end if
			end if
			i=i+1
			goon=inde_counter(indices,mininde,maxinde,1)
		end do
		return
	end subroutine
	
	subroutine permutation_routine(T,newOrder)
		class(fTensor),intent(inout) :: T
		integer,intent(in)::newOrder(:)
		integer,allocatable ::inde(:)
		integer::lenOrder,i,j
		lenorder=size(newOrder)-1
		allocate(inde(lenorder))
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		do i=lenorder,1,-1
			 call permutefo_routine(T,inde(i))
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		return
	end subroutine
	subroutine permutation_name_routine(T,newOrderchar)
		class(fTensor),intent(inout) :: T
		CHARACTER(len=*),intent(in)::newOrderchar(:)
		integer,allocatable::newOrder(:)
		integer,allocatable ::inde(:)
		integer::lenOrder,i,j
		allocate(newOrder(size(newOrderchar)))
		newOrder=T%FindOrder(newOrderchar)
		lenorder=size(newOrder)-1
		allocate(inde(lenorder))
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		do i=lenorder,1,-1
			call permutefo_routine(T,inde(i))
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		return
	end subroutine
	
	
!************************************************************************
	type(fTensor) function ProductTensor (T1,T2)
		type(fTensor),intent(in) :: T1,T2
		call ProductTensor%ProductTensorRoutine(T1%SymTensor,T2%SymTensor)
		return
	end function
	
	type(fTensor) function divide_real8(T1,num) result(Res)
		type(fTensor),intent(in) :: T1
		real(kind=8),intent(in) ::   num
		Res%SymTensor=T1%SymTensor/num
		return
	end function
	type(fTensor) function divide_real4(T1,num) result(Res)
		type(fTensor),intent(in) :: T1
		real(kind=4),intent(in) ::   num
		Res%SymTensor=T1%SymTensor/num
		return
	end function
	type(fTensor) function divide_Tensor(T1,num) result(Res)
		type(fTensor),intent(in) :: T1
		type(Tensor),intent(in) ::   num
		Res%SymTensor=T1%SymTensor/num
		return
	end function
	
	type(fTensor) function add_fTensor(T1,T2) result(Res)
		type(fTensor),intent(in) :: T1,T2
		Res%SymTensor=T1%SymTensor+T2%SymTensor
		return
	end function
	
	type(fTensor) function minu_fTensor(T1,T2) result(Res)
		type(fTensor),intent(in) :: T1,T2
		Res%SymTensor=T1%SymTensor-T2%SymTensor
		return
	end function
	
	type(fTensor) function multiply_number_real8(T1,num) result(Res)
		type(fTensor),intent(in) :: T1
		real(kind=8),intent(in) ::   num
		Res%SymTensor=T1%SymTensor*num
		return
	end function
	type(fTensor) function multiply_number_real4(T1,num) result(Res)
		type(fTensor),intent(in) :: T1
		real(kind=4),intent(in) ::   num
		Res%SymTensor=T1%SymTensor*num
		return
	end function
	type(fTensor) function multiply_number_com4(T1,num) result(Res)
		type(fTensor),intent(in) :: T1
		complex(kind=4),intent(in) ::   num
		Res%SymTensor=T1%SymTensor*num
		return
	end function



!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  contract
!
!**************************************************************************************************************
!**************************************************************************************************************	



! T(:,col)=P(col)*T(:,col) ,input Tensor(:,:)
!where P(col)=+-1 , the parity of col

	subroutine contract_sgin_order_subroutine(T,LD1,LD2,col)
		integer,intent(in)::LD1,LD2,col
		type(Tensor),intent(inout) :: T(LD1,LD2)
		integer::i
		do i=1,LD1
			if(T(i,col)%getFlag())T(i,col)=fermi_factor*T(i,col)
		end do
		return
	end subroutine

! T(:,ith,:)=P(ith)*T(:,ith,:) ,input Tensor(:,:,:)
!where P(col)=+-1 , the parity of col

	subroutine contract_sgin_order_3dim_subroutine(T,LD1,LD2,LD3,ith)
		integer,intent(in)::LD1,LD2,LD3,ith
		type(Tensor),intent(inout) :: T(LD1,LD2,LD3)
		integer::i,j
		do j=1,LD3
			do i=1,LD1
				if(T(i,ith,j)%getFlag())T(i,ith,j)=fermi_factor*T(i,ith,j)
			end do
		end do
		return
	end subroutine

! T(row,:)=P(row)*T(row,:) ,input Tensor(:,:)
!where P(row)=+-1 , the parity of row

	subroutine contract_sgin_order_row_subroutine(T,LD1,LD2,row)
		integer,intent(in)::LD1,LD2,row
		type(Tensor),intent(inout) :: T(LD1,LD2)
		integer::i
		do i=1,LD2
			if(T(row,i)%getFlag())T(row,i)=fermi_factor*T(row,i)
		end do
		return
	end subroutine
!
! T(:,col)=P(col)*T(:,col) ,input fTensor
!where P(col)=+-1 , the parity of col

	subroutine externalContractSginOrder(block,lenofblock,dimen)
		integer::lenofblock
		type(Tensor)::block(lenofblock)
		Type(SymDimension)::dimen
		integer::col,LD1,LD2,i,rank,iQN
		rank=dimen%getRank()
		LD2=dimen%dim(rank)
		LD1=1
		do i=1,rank-1
			LD1=LD1*dimen%dim(i)
		end do
		do col=1,LD2
			call QaunNumParity(iQN,dimen,rank,col)
			if(iQN.eq.(-1))then
				call contract_sgin_order_subroutine(block,LD1,LD2,col)
			end if
		end do
		return
	end subroutine
! T(row,:)=P(row)*T(row,:) ,input fTensor
!where P(row)=+-1 , the parity of row

	subroutine externalContractSginOrderRow(block,lenofblock,dimen)
		integer::lenofblock
		type(Tensor)::block(lenofblock)
		Type(SymDimension)::dimen
		integer::row,LD1,LD2,i,rank,iQN
		rank=dimen%getRank()
		LD1=dimen%dim(1)
		LD2=1
		do i=2,rank
			LD2=LD2*dimen%dim(i)
		end do
		do row=1,LD1
			call QaunNumParity(iQN,dimen,1,row)
			if(iQN.eq.(-1))then
				call contract_sgin_order_row_subroutine(block,LD1,LD2,row)
			end if
		end do
		return
	end subroutine
!
! T(:,ith,:)=P(ith)*T(:,ith,:) ,input fTensor
!where P(ith)=+-1 , the parity of ith
!routine(T%Block,T%getTotalData(),T%SymDimension,T2)

	subroutine externalContractSginOrderDim(block,lenofblock,dimen,Info)
		integer::lenofblock
		type(Tensor)::block(lenofblock)
		Type(SymDimension)::dimen
		type(Tensor)::Info
		integer::ith,LD1,LD2,LD3,i,rank,ithleg,iQN
		ithleg=Info%ii(1)
		rank=dimen%getRank()
		if(ithleg.eq.1)then
			call externalContractSginOrderRow(block,lenofblock,dimen)
			return
		end if
		if(ithleg.eq.rank)then
			call externalContractSginOrder(block,lenofblock,dimen)
			return
		end if
		LD1=dimen%dim(1)
		do i=2,ithleg-1
			LD1=LD1*dimen%dim(i)
		end do
		LD2=dimen%dim(ithleg)
		LD3=dimen%dim(ithleg+1)
		do i=ithleg+2,rank
			LD3=LD3*dimen%dim(i)
		end do

		do ith=1,dimen%dim(ithleg)
			call QaunNumParity(iQN,dimen,ithleg,ith)
			if(iQN.eq.(-1))then
				call contract_sgin_order_3dim_subroutine(block,LD1,LD2,LD3,ith)
			end if
		end do
		return
	end subroutine	
!
! [T1]---->---[T2]
! change to
! [T1]----<---[T2]
!
!modify the rule of last leg of T1 and the first leg of T2
!
!	

	subroutine Reverse_Fermi_Rule1(T1,T2)
		type(fTensor),intent(inout)::T1,T2
		integer::rank1
		if(T1%getTotalBlock().gt.T2%getTotalBlock())then
			call T2%external(externalContractSginOrderRow)
		else
			call T1%external(externalContractSginOrder)
		end if
		rank1=T1%getRank()
		call T1%setFermiArrow(rank1,-1*T1%getFermiArrow(rank1))
		call T2%setFermiArrow(1,-1*T2%getFermiArrow(1))
		return
	end subroutine
	
	subroutine Reverse_Fermi_Rule_specify(T1,T2,row)
		character*1,intent(in)::row
		type(fTensor),intent(inout)::T1,T2
		integer::rank1
		if(row.eq.'r')then
			call T2%external(externalContractSginOrderRow)
		else if(row.eq.'c')then
			call T1%external(externalContractSginOrder)
		else
			call writemess('ERROR in Reverse_Fermi_Rule, input parameter,row='+row,-1)
			call writemess('row="r", change the row of T2',-1)
			call writemess('row="c", change the col of T1',-1)
			call error_stop()
		end if
		rank1=T1%getRank()
		call T1%setFermiArrow(rank1,-1*T1%getFermiArrow(rank1))
		call T2%setFermiArrow(1,-1*T2%getFermiArrow(1))
		return
	end subroutine
	
	subroutine Reverse_Fermi_Rule_specify2(T1,ith1,T2,ith2)
		integer,intent(in)::ith1,ith2
		type(fTensor),intent(inout)::T1,T2
		if(T1%getTotalBlock().gt.T2%getTotalBlock())then
			call T2%external(externalContractSginOrderDim,Tensor((/ith2/)))
		else
			call T1%external(externalContractSginOrderDim,Tensor((/ith1/)))
		end if
		call T1%setFermiArrow(ith1,-1*T1%getFermiArrow(ith1))
		call T2%setFermiArrow(ith2,-1*T2%getFermiArrow(ith2))
		return
	end subroutine
	subroutine Reverse_Fermi_Rule_specify3(T1,ith1)
		integer,intent(in)::ith1
		class(fTensor),intent(inout)::T1
		call T1%external(externalContractSginOrderDim,Tensor((/ith1/)))
		call T1%setFermiArrow(ith1,-1*T1%getFermiArrow(ith1))
		return
	end subroutine

	subroutine Reverse_Fermi_Rule_specify4(T1,name1,T2,name2)
		character(len=*),intent(in)::name1,name2
		type(fTensor),intent(inout)::T1,T2
		integer::ith1,ith2
		ith1=T1%FindOrder(name1)
		ith2=T2%FindOrder(name2)
		if(T1%getTotalBlock().gt.T2%getTotalBlock())then
			call T2%external(externalContractSginOrderDim,Tensor((/ith2/)))
		else
			call T1%external(externalContractSginOrderDim,Tensor((/ith1/)))
		end if
		call T1%setFermiArrow(ith1,-1*T1%getFermiArrow(ith1))
		call T2%setFermiArrow(ith2,-1*T2%getFermiArrow(ith2))
		return
	end subroutine

	subroutine Reverse_Fermi_Rule_specify5(T1,name1)
		character(len=*),intent(in)::name1
		class(fTensor),intent(inout)::T1
		integer::ith1
		ith1=T1%FindOrder(name1)
		call T1%external(externalContractSginOrderDim,Tensor((/ith1/)))
		call T1%setFermiArrow(ith1,-1*T1%getFermiArrow(ith1))
		return
	end subroutine

	subroutine Reverse_Fermi_Rule_specify6(T1,name1,T2,name2,T1orT2)
		character(len=*),intent(in)::name1,name2
		type(fTensor),intent(inout)::T1,T2
		logical,intent(in)::T1orT2
		integer::ith1,ith2
		ith1=T1%FindOrder(name1)
		ith2=T2%FindOrder(name2)
		if(T1orT2)then
			call T1%external(externalContractSginOrderDim,Tensor((/ith1/)))
		else
			call T2%external(externalContractSginOrderDim,Tensor((/ith2/)))
		end if
		call T1%setFermiArrow(ith1,-1*T1%getFermiArrow(ith1))
		call T2%setFermiArrow(ith2,-1*T2%getFermiArrow(ith2))
		return
	end subroutine

	subroutine Reverse_Fermi_Rule_specify7(T1,ith1,T2,ith2,T1orT2)
		integer,intent(in)::ith1,ith2
		type(fTensor),intent(inout)::T1,T2
		logical,intent(in)::T1orT2
		if(T1orT2)then
			call T1%external(externalContractSginOrderDim,Tensor((/ith1/)))
		else
			call T2%external(externalContractSginOrderDim,Tensor((/ith2/)))
		end if
		call T1%setFermiArrow(ith1,-1*T1%getFermiArrow(ith1))
		call T2%setFermiArrow(ith2,-1*T2%getFermiArrow(ith2))
		return
	end subroutine


	subroutine monitor_check(rule1,rule2)
		integer,intent(in)::rule1,rule2
		if(mointer_order_flag)then
			if(rule1*rule2.ge.0)then
				call writemess('ERROR in fermi-arrow',-1)
				write(*,*)rule1,rule2
				call error_stop
			end if
		else
			if(rule1*rule2.gt.0)then
				call writemess('ERROR in fermi-arrow',-1)
					write(*,*)rule1,rule2
				call error_stop
			end if
		end if
		return
	end subroutine
	
	subroutine un_set_mointer_order_flag()
		mointer_order_flag=.false.
	end subroutine
	subroutine set_mointer_order_flag()
		mointer_order_flag=.true.
	end subroutine

!fermi-arrow=1     mean the leg is <a| :  --->----[A]
!fermi-arrow=-1 	mean the leg is |b> :  ---<----[B]
!
!   the contraction should be    <a|b>
!   if not do the Correction
!      __         __
!     |  |       |  |
!     |A |--->---|B |
!     |  |--->---|  | 
!     |  |---<---|  |
!     |__|       |__|
!
!the fermi-arrow of A are   -1,  -1,  1
!the fermi-arrow of B are    1,   1, -1
!The first two leg will not cause the SignCorrect
! the last leg, fermi-arrow of A is 1 and fermi-arrow of B is -1, it cause SignCorrect
!
!
!If fermi-arrow=0 , will not monitor the order

	subroutine RuleSignCorrect(T1,T1_,T1Name,T2,T2_,T2Name,lenName)
		type(fTensor),intent(inout)::T1,T2
		type(fTensor),intent(in)::T1_,T2_
		character(len=*),intent(in)::T2Name(:),T1Name(:)
		integer,intent(in)::lenName
		integer::i,rank
		integer::rule1,rule2,SymRule1,SymRule2
		T2=T2_.pf.T2Name(1:lenName)
		rank=T1_%getRank()
		call T1%empty()
		do i=lenName,1,-1
			if(T1%getFlag())then
				call T1%backWard(T1Name(i))
			else
				T1=T1_.pb.T1Name(i)
			end if
			rule1=T1%getFermiArrow(rank)
			rule2=T2%getFermiArrow(i)
			call monitor_check(rule1,rule2)
			SymRule1=T1%getRule(rank)
			SymRule2=T2%getRule(i)
			call checkSymmetryRule(SymRule1,SymRule2,T1%getName(rank),T2%getName(i))
			if((rule2.lt.0).and.(rule1.gt.0))then
				call T1%external(externalContractSginOrder)
			end if
		end do
		call T1%SymTensor%backWard(T1Name(1:lenName))
		return
	end subroutine
	subroutine RuleSignCorrectInout1(T1,T1_,T1Name,T2,T2Name,lenName)!the subroutine will change the order of legs in T2, will not change T1_
		type(fTensor),intent(inout)::T1,T2
		type(fTensor),intent(in)::T1_
		character(len=*),intent(in)::T2Name(:),T1Name(:)
		integer,intent(in)::lenName
		integer::i,rank
		integer::rule1,rule2,SymRule1,SymRule2
		call T2%forward(T2Name(1:lenName))
		rank=T1_%getRank()
		call T1%empty()
		do i=lenName,1,-1
			if(T1%getFlag())then
				call T1%backWard(T1Name(i))
			else
				T1=T1_.pb.T1Name(i)
			end if
			rule1=T1%getFermiArrow(rank)
			rule2=T2%getFermiArrow(i)
			call monitor_check(rule1,rule2)
			SymRule1=T1%getRule(rank)
			SymRule2=T2%getRule(i)
			call checkSymmetryRule(SymRule1,SymRule2,T1%getName(rank),T2%getName(i))
			if((rule2.lt.0).and.(rule1.gt.0))then
				call T1%external(externalContractSginOrder)
			end if
		end do
		call T1%SymTensor%backWard(T1Name(1:lenName))
		return
	end subroutine
	subroutine RuleSignCorrectInout2(T1,T1Name,T2,T2Name,lenName)!the subroutine will change the order of legs in T2, and destroy T1
		type(fTensor),intent(inout)::T1,T2
		character(len=*),intent(in)::T2Name(:),T1Name(:)
		integer,intent(in)::lenName
		integer::i,rank
		integer::rule1,rule2,SymRule1,SymRule2
		call T2%forward(T2Name(1:lenName))
		rank=T1%getRank()
		do i=lenName,1,-1
			call T1%backWard(T1Name(i))
			rule1=T1%getFermiArrow(rank)
			rule2=T2%getFermiArrow(i)
			call monitor_check(rule1,rule2)
			SymRule1=T1%getRule(rank)
			SymRule2=T2%getRule(i)
			call checkSymmetryRule(SymRule1,SymRule2,T1%getName(rank),T2%getName(i))
			if((rule2.lt.0).and.(rule1.gt.0))then
				call T1%external(externalContractSginOrder)
			end if
		end do
		call T1%SymTensor%backWard(T1Name(1:lenName))
		return
	end subroutine
	subroutine RuleSignCorrectInout3(T1,T1Name,T2,T2Name,lenName)!the subroutine will change the order of legs in T1, and destroy T2
		type(fTensor),intent(inout)::T1,T2
		character(len=*),intent(in)::T2Name(:),T1Name(:)
		integer,intent(in)::lenName
		integer::i,rank,firsti
		integer::rule1,rule2,SymRule1,SymRule2
		call T1%backward(T1Name(1:lenName))
		rank=T1%getRank()
		firsti=rank-lenName
		do i=1,lenName
			call T2%forWard(T2Name(i))
			rule1=T1%getFermiArrow(firsti+i)
			rule2=T2%getFermiArrow(1)
			call monitor_check(rule1,rule2)
			SymRule1=T1%getRule(firsti+i)
			SymRule2=T2%getRule(1)
			call checkSymmetryRule(SymRule1,SymRule2,T1%getName(firsti+i),T2%getName(1))
			if((rule2.lt.0).and.(rule1.gt.0))then
				call T2%external(externalContractSginOrderRow)
			end if
		end do
		call T2%SymTensor%forWard(T2Name(1:lenName))
		return
	end subroutine
	subroutine RuleSignCorrect_int(T1,T1_,T1index_,T2,T2_,T2index,lenindex)
		type(fTensor),intent(inout)::T1,T2
		type(fTensor),intent(in)::T1_,T2_
		integer,intent(in)::T1index_(:),T2index(:)
		integer,intent(in)::lenindex
		integer::i,rank,indices(lenindex),j,T1index(lenindex)
		integer::rule1,rule2,SymRule1,SymRule2
		T2=T2_.pf.T2index(1:lenindex)
		rank=T1_%getRank()
		call T1%empty()
		do i=1,lenindex
			indices(i)=rank-i+1
		end do
		T1index=T1index_(1:lenindex)
		do i=lenindex,1,-1
			if(T1%getFlag())then
				call T1%backWard(T1index(i))
			else
				T1=T1_.pb.T1index(i)
			end if
			do j=i-1,1,-1
				if(T1index(j).gt.T1index(i))then
					T1index(j)=T1index(j)-1
				end if
			end do
			rule1=T1%getFermiArrow(rank)
			rule2=T2%getFermiArrow(i)
			call monitor_check(rule1,rule2)
			SymRule1=T1%getRule(rank)
			SymRule2=T2%getRule(i)
			call checkSymmetryRule(SymRule1,SymRule2,T1%getName(rank),T2%getName(i))
			if((rule2.lt.0).and.(rule1.gt.0))then
				call T1%external(externalContractSginOrder)
			end if
		end do
		call T1%SymTensor%backWard(indices)
		return
	end subroutine
	
	subroutine RuleSignCorrect2(T1,T1_,T1Name,T2,T2_,T2Name)
		type(fTensor),intent(inout)::T1,T2
		type(fTensor),intent(in)::T1_,T2_
		character(len=*),intent(in)::T2Name,T1Name
		integer::i,rank
		integer::rule1,rule2,SymRule1,SymRule2
		T1=T1_.pb.T1Name
		T2=T2_.pf.T2Name
		rule1=T1%getFermiArrow(T1%getrank())
		rule2=T2%getFermiArrow(1)
		call monitor_check(rule1,rule2)
		SymRule1=T1%getRule(T1%getrank())
		SymRule2=T2%getRule(1)
		call checkSymmetryRule(SymRule1,SymRule2,T1%getName(T1%getrank()),T2%getName(1))
		if((rule2.lt.0).and.(rule1.gt.0))then
			call T1%external(externalContractSginOrder)
		end if
		return
	end subroutine
	
	subroutine RuleSignCorrect_int2(T1,T1_,T1index,T2,T2_,T2index)
		type(fTensor),intent(inout)::T1,T2
		type(fTensor),intent(in)::T1_,T2_
		integer,intent(in)::T1index,T2index
		integer::i,rank
		integer::rule1,rule2,SymRule1,SymRule2
		T1=T1_.pb.T1index
		T2=T2_.pf.T2index
		rule1=T1%getFermiArrow(T1%getrank())
		rule2=T2%getFermiArrow(1)
		call monitor_check(rule1,rule2)
		SymRule1=T1%getRule(T1%getrank())
		SymRule2=T2%getRule(1)
		call checkSymmetryRule(SymRule1,SymRule2,T1%getName(T1%getrank()),T2%getName(1))
		if((rule2.lt.0).and.(rule1.gt.0))then
			call T1%external(externalContractSginOrder)
		end if
		return
	end subroutine
	
!******************  contract  *********************
!	T1:[i1,i2,i3,i4,i5,i6,i7,i8]
!	T2:[j1,j2,j3,j4,j5,j6,j7,j8,j9,j10]
!	i1=(/5,1,2/)
!	i2=(/10,3,5/)
!	then the result will be T1'*T2'
!	where ,
!	T1'=[i3,i4,i6,i7,i8,(i5*i1*i2)]
!	T2'=[(j10*j3*j5),j1,j2,j4,j6,j7,j8,j9]
!  The sign of T1' will be determine by T1, T1 permute to (/2,1,5/) with fermi rule **********NOTE here not (/5,1,2/) *************
!         And the permute to [i3,i4,i6,i7,i8,(i5*i1*i2)] non-fermi rule
!  T2' will be determine by T1, T1 permute to (/10,3,5/)  with fermi rule 
!  example   
!       C1*C2 |11> = C1*C2 * C1^+ * C2^+ |0> = -C2*C1 * C1^+ * C2^+ |0> = -|00>
!     The order of C1*C2 shoud reoder as C2*C1
!     but the data store in memery is C1*C2
!     so the function permute to C2*C1 with fermi rule, and then permute to C1*C2 with non-fermi rule
!
! 	input Tensor should be in its original dimenison,there is no contract on it
!	if present len_of_contract, len_of_contract specify the length of  i1, and i2

	type(fTensor) function contract_noName(T1_,i1,T2_,i2,len_of_contract) result(T)
		type(fTensor),intent(in) :: T1_,T2_
		integer,intent(in) :: i1(:),i2(:)
		integer,optional,intent(in)::len_of_contract(2)
		type(fTensor) :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first fTensor, when contracting fTensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second fTensor, when contracting fTensor',-1)
			call error_stop()
		end if
		rank1=T1_%getrank()
		rank2=T2_%getrank()
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call RuleSignCorrect_int(T1,T1_,i1,T2,T2_,i2,leni1)
		call T1%fuse(rank1-leni1+1,rank1)
		call T2%fuse(1,leni2)
		T=T1 * T2
		return
	end function
	type(fTensor) function contract_noName2(T1_,i1,T2_,i2) result(T)
		type(fTensor),intent(in) :: T1_,T2_
		type(fTensor) :: T1,T2
		integer,intent(in) :: i1,i2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first fTensor, when contracting fTensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second fTensor, when contracting fTensor',-1)
			call error_stop()
		end if
		call RuleSignCorrect_int2(T1,T1_,i1,T2,T2_,i2)
		T=T1*T2
		return
	end function
	type(fTensor) function contract_name(T1_,name1,T2_,name2,len_of_contract) result(T)
		type(fTensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name1(:),name2(:)
		integer,optional,intent(in)::len_of_contract(2)
		type(fTensor) :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first fTensor, when contracting fTensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second fTensor, when contracting fTensor',-1)
			call error_stop()
		end if
		if(.not.(T1_%if_original_dim().and.T2_%if_original_dim())) then
			write(*,*)"ERROR in contract"
			write(*,*)"stop"
			call error_stop()
		end if
		rank1=T1_%getrank()
		rank2=T2_%getrank()
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(name1))
			leni2=min(len_of_contract(2),size(name2))
		else
			leni1=size(name1)
			leni2=size(name2)
		end if
		call RuleSignCorrect(T1,T1_,name1,T2,T2_,name2,leni1)
		call T1%fuse(rank1-leni1+1,rank1)
		call T2%fuse(1,leni2)
		T=T1 * T2
		return
	end function
	type(fTensor) function contract_name2(T1_,name1,T2_,name2) result(T)
		type(fTensor),intent(in) :: T1_,T2_
		type(fTensor) :: T1,T2
		character(len=*),intent(in)::name1,name2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first fTensor, when contracting fTensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second fTensor, when contracting fTensor',-1)
			call error_stop()
		end if
		if(.not.(T1_%if_original_dim().and.T2_%if_original_dim())) then
			write(*,*)"ERROR in contract"
			write(*,*)"stop"
			call error_stop()
		end if
		call RuleSignCorrect2(T1,T1_,name1,T2,T2_,name2)
		T=T1*T2
		return
	end function

	
	subroutine fcontract_name_routine(T,T1,name1,T2,name2,len_of_contract) 
		class(fTensor),target::T
		type(fTensor),target:: T1,T2
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2
		class(fTensor),pointer::pT
		type(fTensor),pointer::pT1,pT2
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1%SymDimension).and.if_original_dim(T2%SymDimension))) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		pT=>T
		pT1=>T1
		pT2=>T2
		if(associated(pT,pT1).or.associated(pT,pT2).or.associated(pT1,pT2))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract(A,[names],B,[names])')
			call writemess('T, A and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT1=>null()
		pT2=>null()

		i1=T1%FindOrder(name1)
		i2=T2%FindOrder(name2)
		rank1=T1%Getrank()
		rank2=T2%Getrank()
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(name1))
			leni2=min(len_of_contract(2),size(name2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call RuleSignCorrectinout1(T,T1,name1,T2,name2,leni1)
		call T%fuse(rank1-leni1+1,rank1)
		call T2%fuse(1,leni2)
		T=T*T2  
		call T1%split( )
		call T2%split( )
		return
	end subroutine
	
	subroutine fcontract_name_routine1(T,name1,T2,name2,len_of_contract) 
		class(fTensor),target::T
		type(fTensor),target:: T2
		character(len=*),intent(in)::name1(:),name2(:)
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2
		class(fTensor),pointer::pT
		type(fTensor),pointer::pT2
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T%SymDimension).and.if_original_dim(T2%SymDimension))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if

		pT=>T
		pT2=>T2
		if(associated(pT,pT2))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract([names],B,[names])')
			call writemess('T and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT2=>null()
		rank1=T%getRank()
		rank2=T2%getRank()
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(name1))
			leni2=min(len_of_contract(2),size(name2))
		else
			leni1=size(name1)
			leni2=size(name2)
		end if
		
		call RuleSignCorrectinout2(T,name1,T2,name2,leni1)
		call T%fuse(rank1-leni1+1,rank1)
		call T2%fuse(1,leni2)
		T=T*T2
		call T2%split()
		return
	end subroutine
	subroutine fcontract_name_routine2(T,T1,name1,name2,len_of_contract) 
		class(fTensor),target::T
		type(fTensor),target:: T1
		character(len=*),intent(in)::name1(:),name2(:)
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2
		class(fTensor),pointer::pT
		type(fTensor),pointer::pT1
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1%SymDimension).and.if_original_dim(T%SymDimension))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if

		pT=>T
		pT1=>T1
		if(associated(pT,pT1))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract(A,[names],[names])')
			call writemess('T and A can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT1=>null()
		rank1=T1%getrank()
		rank2=T%getrank()
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(name1))
			leni2=min(len_of_contract(2),size(name2))
		else
			leni1=size(name1)
			leni2=size(name2)
		end if
		call RuleSignCorrectinout3(T1,name1,T,name2,leni1)
		call T1%fuse(rank1-leni1+1,rank1)
		call T%fuse(1,leni2)
		T=T1*T
		call T1%split( )
		return
	end subroutine
	subroutine fcontract_name_routine4(T,T1,name1,T2,name2) 
		class(fTensor),target::T
		type(fTensor),target:: T1,T2
		character(len=*),intent(in)::name1,name2
		class(fTensor),pointer::pT
		type(fTensor),pointer::pT1,pT2
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1%SymDimension).and.if_original_dim(T2%SymDimension))) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		pT=>T
		pT1=>T1
		pT2=>T2
		if(associated(pT,pT1).or.associated(pT,pT2).or.associated(pT1,pT2))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract(A,[names],B,[names])')
			call writemess('T, A and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT1=>null()
		pT2=>null()
		call RuleSignCorrectinout1(T,T1,[name1],T2,[name2],1)
		T=T*T2  
		return
	end subroutine
	subroutine fcontract_name_routine5(T,name1,T2,name2) 
		class(fTensor),target::T
		type(fTensor),target:: T2
		character(len=*),intent(in)::name1,name2
		class(fTensor),pointer::pT
		type(fTensor),pointer::pT2
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T2%SymDimension))) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		pT=>T
		pT2=>T2
		if(associated(pT,pT2))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract(A,[names],B,[names])')
			call writemess('T, A and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT2=>null()
		call RuleSignCorrectinout2(T,[name1],T2,[name2],1)
		T=T*T2  
		return
	end subroutine
	subroutine fcontract_name_routine6(T,T1,name1,name2) 
		class(fTensor),target::T
		type(fTensor),target :: T1
		character(len=*),intent(in)::name1,name2
		class(fTensor),pointer::pT
		type(fTensor),pointer::pT1
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T1%SymDimension)) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		pT=>T
		pT1=>T1
		if(associated(pT,pT1))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract(name1,B,name2)')
			call writemess('T and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT1=>null()
		call RuleSignCorrectinout3(T1,[name1],T,[name2],1)
		T=T1*T
	end subroutine


	subroutine contract_name_ownlegs_routine(T,name1,name2) 
		class(fTensor)::T
		character(len=*),intent(in)::name1,name2
		type(fTensor)::tempT
		type(SymDimension)::NewDimen
		integer::rank,i,k,classtype,dim1
		integer::rule1,rule2,SymRule1,SymRule2
		type(Tensor),pointer::Tenp(:,:,:),Newp(:)
		type(Tensor)::blockTensor
		integer,pointer::totalBlock
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(T%if_original_dim())) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		rank=T%getRank()
		if(rank.eq.2)then
			call writemess(' Do not finsihed the code of rank=2, for T%contract(name1,name2)')
			call error_stop
		end if

		rule1=T%getFermiArrow(name1)
		rule2=T%getFermiArrow(name2)
		call monitor_check(rule1,rule2)
		SymRule1=T%getRule(name1)
		SymRule2=T%getRule(name2)
		call checkSymmetryRule(SymRule1,SymRule2,name1,name2)
		if(rule1.lt.0)then
			tempT=T.pf.name2
			call tempT%forward(name1)
		else
			tempT=T.pf.name1
			call tempT%forward(name2)
		end if


		dim1=tempT%dim(1)
		if(dim1.ne.tempT%dim(2))then
			call writemess(' ERROR in contract(name1,name2), dimension')
			call error_stop
		end if
		NewDimen=tempT.subdim.[3,rank]
		classtype=T%getType()
		call T%empty()
		call T%allocate(NewDimen,classtype)
		call tempT%fuse(3,rank)
		call tempT%pointer(Tenp)
		call T%pointer(Newp)
		call T%point2TotalBlock(totalBlock)
		do k=1,T%getTotalData()
			do i=1,dim1
				if(Tenp(i,i,k)%getFlag())then
					blockTensor=contract(Tenp(i,i,k),1,2)
					call blockTensor%split()
					if(Newp(k)%getFlag())then
						Newp(k)=Newp(k)+blockTensor
					else
						Newp(k)=blockTensor
					end if
				end if
			end do
			if(Newp(k)%getFlag())totalBlock=totalBlock+1
		end do
		return
	end subroutine

	subroutine contract_ownlegs_routine(T,ith1,ith2) 
		class(fTensor)::T
		integer,intent(in)::ith1,ith2
		type(fTensor)::tempT
		type(SymDimension)::NewDimen
		integer::rank,i,k,classtype,dim1
		integer::rule1,rule2,SymRule1,SymRule2
		type(Tensor),pointer::Tenp(:,:,:),Newp(:)
		type(Tensor)::blockTensor
		integer,pointer::totalBlock
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(T%if_original_dim())) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		rank=T%getRank()
		if(rank.eq.2)then
			call writemess(' Do not finsihed the code of rank=2, for T%contract(name1,name2)')
			call error_stop
		end if

		rule1=T%getFermiArrow(ith1)
		rule2=T%getFermiArrow(ith2)
		call monitor_check(rule1,rule2)
		SymRule1=T%getRule(ith1)
		SymRule2=T%getRule(ith2)
		call checkSymmetryRule(SymRule1,SymRule2,T%getName(ith1),T%getName(ith2))
		if(rule1.lt.0)then
			tempT=T.pf.ith2
			call tempT%forward(ith1)
		else
			tempT=T.pf.ith1
			call tempT%forward(ith2)
		end if

		dim1=tempT%dim(1)
		if(dim1.ne.tempT%dim(2))then
			call writemess(' ERROR in contract(name1,name2), dimension')
			call error_stop
		end if
		NewDimen=tempT.subdim.[3,rank]
		classtype=T%getType()
		call T%empty()
		call T%allocate(NewDimen,classtype)
		call tempT%fuse(3,rank)
		call tempT%pointer(Tenp)
		call T%pointer(Newp)
		call T%point2TotalBlock(totalBlock)
		do k=1,T%getTotalData()
			do i=1,dim1
				if(Tenp(i,i,k)%getFlag())then
					blockTensor=contract(Tenp(i,i,k),1,2)
					call blockTensor%split()
					if(Newp(k)%getFlag())then
						Newp(k)=Newp(k)+blockTensor
					else
						Newp(k)=blockTensor
					end if
				end if
			end do
			if(Newp(k)%getFlag())totalBlock=totalBlock+1
		end do
		return
	end subroutine

	type(fTensor) function fcontract_name_ownlegs(Tin,name1,name2) Result(Res)
		type(fTensor),intent(in)::Tin
		character(len=*),intent(in)::name1,name2
		type(fTensor)::tempT
		type(SymDimension)::NewDimen
		integer::rank,i,k,classtype,dim1
		integer::rule1,rule2,SymRule1,SymRule2
		type(Tensor),pointer::Tenp(:,:,:),Newp(:)
		type(Tensor)::blockTensor
		integer,pointer::totalBlock
		if(.not.Tin%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(Tin%if_original_dim())) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		rank=Tin%getRank()
		if(rank.eq.2)then
			call writemess(' Do not finsihed the code of rank=2, for T%contract(name1,name2)')
			call error_stop
		end if
		rule1=Tin%getFermiArrow(name1)
		rule2=Tin%getFermiArrow(name2)
		call monitor_check(rule1,rule2)
		SymRule1=Tin%getRule(name1)
		SymRule2=Tin%getRule(name2)
		call checkSymmetryRule(SymRule1,SymRule2,name1,name2)
		if(rule1.lt.0)then
			tempT=Tin.pf.name2
			call tempT%forward(name1)
		else
			tempT=Tin.pf.name1
			call tempT%forward(name2)
		end if


		dim1=tempT%dim(1)
		if(dim1.ne.tempT%dim(2))then
			call writemess(' ERROR in contract(name1,name2), dimension')
			call error_stop
		end if
		NewDimen=tempT.subdim.[3,rank]
		classtype=Tin%getType()
		call Res%empty()
		call Res%allocate(NewDimen,classtype)
		call tempT%fuse(3,rank)
		call tempT%pointer(Tenp)
		call Res%pointer(Newp)
		call Res%point2TotalBlock(totalBlock)
		do k=1,Res%getTotalData()
			do i=1,dim1
				if(Tenp(i,i,k)%getFlag())then
					blockTensor=contract(Tenp(i,i,k),1,2)
					call blockTensor%split()
					if(Newp(k)%getFlag())then
						Newp(k)=Newp(k)+blockTensor
					else
						Newp(k)=blockTensor
					end if
				end if
			end do
			if(Newp(k)%getFlag())totalBlock=totalBlock+1
		end do
		return
	end function
	type(fTensor) function fcontract_ownlegs(Tin,ith1,ith2) Result(Res)
		type(fTensor),intent(in)::Tin
		integer,intent(in)::ith1,ith2
		type(fTensor)::tempT
		type(SymDimension)::NewDimen
		integer::rank,i,k,classtype,dim1
		integer::rule1,rule2,SymRule1,SymRule2
		type(Tensor),pointer::Tenp(:,:,:),Newp(:)
		type(Tensor)::blockTensor
		integer,pointer::totalBlock
		if(.not.Tin%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(Tin%if_original_dim())) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		rank=Tin%getRank()
		if(rank.eq.2)then
			call writemess(' Do not finsihed the code of rank=2, for T%contract(name1,name2)')
			call error_stop
		end if
		rule1=Tin%getFermiArrow(ith1)
		rule2=Tin%getFermiArrow(ith2)
		call monitor_check(rule1,rule2)
		SymRule1=Tin%getRule(ith1)
		SymRule2=Tin%getRule(ith2)
		call checkSymmetryRule(SymRule1,SymRule2,Tin%getName(ith1),Tin%getName(ith2))
		if(rule1.lt.0)then
			tempT=Tin.pf.ith2
			call tempT%forward(ith1)
		else
			tempT=Tin.pf.ith1
			call tempT%forward(ith2)
		end if


		dim1=tempT%dim(1)
		if(dim1.ne.tempT%dim(2))then
			call writemess(' ERROR in contract(name1,name2), dimension')
			call error_stop
		end if
		NewDimen=tempT.subdim.[3,rank]
		classtype=Tin%getType()
		call Res%empty()
		call Res%allocate(NewDimen,classtype)
		call tempT%fuse(3,rank)
		call tempT%pointer(Tenp)
		call Res%pointer(Newp)
		call Res%point2TotalBlock(totalBlock)
		do k=1,Res%getTotalData()
			do i=1,dim1
				if(Tenp(i,i,k)%getFlag())then
					blockTensor=contract(Tenp(i,i,k),1,2)
					call blockTensor%split()
					if(Newp(k)%getFlag())then
						Newp(k)=Newp(k)+blockTensor
					else
						Newp(k)=blockTensor
					end if
				end if
			end do
			if(Newp(k)%getFlag())totalBlock=totalBlock+1
		end do
		return
	end function
!*********************************************
 
	type(fTensor) function Htranspose(T)
		type(fTensor),intent(in) :: T
		integer :: rank,i,charlen
		integer,allocatable::indices(:)
		character(len=max_len_of_char_in_TData)::Tname
		rank=T%getRank()
		allocate(indices(rank))
		do i=1,rank
			indices(i)=rank-i+1
		end do
		Htranspose%SymTensor=(.con.T%SymTensor).p.indices
		do i=1,rank
			call Htranspose%setFermiArrow(i,-1*Htranspose%getFermiArrow(i))
		end do
		call reverseRule(Htranspose%SymDimension)
		return
	end function
	
	type(fTensor) function Htranspose2(T)result(Htranspose)
		type(fTensor),intent(in) :: T
		integer :: rank,i,charlen
		integer,allocatable::indices(:)
		character(len=max_len_of_char_in_TData)::Tname
		rank=T%getRank()
		allocate(indices(rank))
		do i=1,rank
			indices(i)=rank-i+1
		end do
		Htranspose%SymTensor=(.con.T%SymTensor).p.indices
		do i=1,rank
			call Htranspose%setFermiArrow(i,-1*Htranspose%getFermiArrow(i))
		end do
		call reverseRule(Htranspose%SymDimension)
		do i=1,rank
			Tname=Htranspose%getName(i)
			charlen=len(trim(Tname))
			if(Tname(charlen:charlen).eq.dag_mark) then
				call Htranspose%setName(i,Tname(1:charlen-1))
			else
				call Htranspose%setName(i,Tname+dag_mark)
			end if
		end do
		return
	end function
	
	type(fTensor) function conjugate(T)
		type(fTensor),intent(in) :: T
		integer :: i
		conjugate%SymTensor=.con.T%SymTensor
		return
	end function
	
	
	
	
	type(fTensor) function expmfTensor(T)
		type(fTensor),intent(in) :: T
		expmfTensor%SymTensor=expm(T%SymTensor)
		return
	end function
	
	type(fTensor) function FermiFusefTensorRule(B,outDim,Order,row,NewRule)result(FermiFusefTensor)
		class(fTensor),intent(in)::B
		type(SymDimension),intent(inout)::outDim
		integer,intent(in)::NewRule
		type(Tensor),intent(inout)::Order
		logical,optional,intent(in)::row
		integer::i,rank
		type(QuanNum)::NewQuanNum
		if(row)then
			outDim=B%SymDimension.subdim.[1,2]
			call fuseOrder(NewQuanNum,order,B%quantumnumber(1),B%quantumnumber(2),NewRule)
			FermiFusefTensor=B%QuanFuse(NewQuanNum,Order,.true.)
		else
			rank=B%getRank()
			i=rank-1
			outDim=B%SymDimension.subdim.[i,rank]
			call fuseOrder(NewQuanNum,order,B%quantumnumber(i),B%quantumnumber(rank),NewRule)
			FermiFusefTensor=B.pb.i
			call FermiFusefTensor%SymTensor%backward(i)
			FermiFusefTensor=FermiFusefTensor%QuanFuse(NewQuanNum,Order,.false.)
		end if
		return
	end function


	type(fTensor) function FermiFusefTensor(B,outDim,Order,row)
		class(fTensor),intent(in)::B
		type(SymDimension),intent(inout)::outDim
		type(Tensor),intent(inout)::Order
		logical,intent(in)::row
		integer::i,rank
		type(QuanNum)::NewQuanNum
		integer::arrow1,arrow2,rule1,rule2
		if(row)then
			arrow1=B%getFermiArrow(1)
			arrow2=B%getFermiArrow(2)
			if(arrow1.ne.arrow2)then
				call writemess(' The fermi-arrows of the fusing legs are not the same')
				call error_stop
			end if
			rule1=B%getRule(1)
			rule2=B%getRule(2)
			if(rule1.ne.rule2)then
				call writemess(' The symmetry of the fusing legs are not the same')
				call error_stop
			end if
			outDim=B%SymDimension.subdim.[1,2]
			call fuseOrder(NewQuanNum,order,B%quantumnumber(1),B%quantumnumber(2),rule1)
			call NewQuanNum%setFermiArrow(arrow1)
			if(arrow1.eq.(-1))then
				FermiFusefTensor=B.pf.2
				call FermiFusefTensor%SymTensor%forward(2)
				FermiFusefTensor=FermiFusefTensor%QuanFuse(NewQuanNum,Order,.true.)
			else
				FermiFusefTensor=B%QuanFuse(NewQuanNum,Order,.true.)
			end if
		else
			rank=B%getRank()
			i=rank-1
			arrow1=B%getFermiArrow(i)
			arrow2=B%getFermiArrow(rank)
			if(arrow1.ne.arrow2)then
				call writemess(' The fermi-arrows of the fusing legs are not the same')
				call error_stop
			end if
			rule1=B%getRule(i)
			rule2=B%getRule(rank)
			if(rule1.ne.rule2)then
				call writemess(' The symmetry of the fusing legs are not the same')
				call error_stop
			end if
			outDim=B%SymDimension.subdim.[i,rank]
			call fuseOrder(NewQuanNum,order,B%quantumnumber(i),B%quantumnumber(rank),rule1)
			call NewQuanNum%setFermiArrow(arrow1)
			if(arrow1.eq.(-1))then
				FermiFusefTensor=B.pb.i
				call FermiFusefTensor%SymTensor%backward(i)
				FermiFusefTensor=FermiFusefTensor%QuanFuse(NewQuanNum,Order,.false.)
			else
				FermiFusefTensor=B%QuanFuse(NewQuanNum,Order,.false.)
			end if
		end if
		return
	end function

	

	
	
	type(fTensor) function FermiFusefTensor2(B,row)result(FermiFusefTensor)
		class(fTensor),intent(in)::B
		type(Tensor)::Order
		logical,intent(in)::row
		integer::i,rank,rule
		type(QuanNum)::NewQuanNum
		integer::arrow1,arrow2,rule1,rule2
		if(row)then
			arrow1=B%getFermiArrow(1)
			arrow2=B%getFermiArrow(2)
			if(arrow1.ne.arrow2)then
				call writemess(' The fermi-arrows of the fusing legs are not the same')
				call error_stop
			end if
			rule1=B%getRule(1)
			rule2=B%getRule(2)
			if(rule1.ne.rule2)then
				call writemess(' The symmetry of the fusing legs are not the same')
				call error_stop
			end if
			call fuseOrder(NewQuanNum,order,B%quantumnumber(1),B%quantumnumber(2),rule1)
			call NewQuanNum%setFermiArrow(arrow1)
			if(arrow1.eq.(-1))then
				FermiFusefTensor=B.pf.2
				call FermiFusefTensor%SymTensor%forward(2)
				FermiFusefTensor=FermiFusefTensor%QuanFuse(NewQuanNum,Order,.true.)
			else
				FermiFusefTensor=B%QuanFuse(NewQuanNum,Order,.true.)
			end if
		else
			rank=B%getRank()
			i=rank-1
			arrow1=B%getFermiArrow(i)
			arrow2=B%getFermiArrow(rank)
			if(arrow1.ne.arrow2)then
				call writemess(' The fermi-arrows of the fusing legs are not the same')
				call error_stop
			end if
			rule1=B%getRule(i)
			rule2=B%getRule(rank)
			if(rule1.ne.rule2)then
				call writemess(' The symmetry of the fusing legs are not the same')
				call error_stop
			end if
			call fuseOrder(NewQuanNum,order,B%quantumnumber(i),B%quantumnumber(rank),rule1)
			call NewQuanNum%setFermiArrow(arrow1)
			if(arrow1.eq.(-1))then
				FermiFusefTensor=B.pb.i
				call FermiFusefTensor%SymTensor%backward(i)
				FermiFusefTensor=FermiFusefTensor%QuanFuse(NewQuanNum,Order,.false.)
			else
				FermiFusefTensor=B%QuanFuse(NewQuanNum,Order,.false.)
			end if
		end if
		return
	end function

	type(fTensor) function FermiFusefTensorRule2(B,row,NewRule)result(FermiFusefTensor)
		class(fTensor),intent(in)::B
		type(Tensor)::Order
		integer,intent(in)::NewRule
		logical,intent(in)::row
		integer::i,rank,rule
		type(QuanNum)::NewQuanNum
		integer::arrow1,arrow2
		if(row)then
			arrow1=B%getFermiArrow(1)
			arrow2=B%getFermiArrow(2)
			if(arrow1.ne.arrow2)then
				call writemess(' The fermi-arrows of the fusing legs are not the same')
				call error_stop
			end if
			call fuseOrder(NewQuanNum,order,B%quantumnumber(1),B%quantumnumber(2),NewRule)
			call NewQuanNum%setFermiArrow(arrow1)
			if(arrow1.eq.(-1))then
				FermiFusefTensor=B.pf.2
				call FermiFusefTensor%SymTensor%forward(2)
				FermiFusefTensor=FermiFusefTensor%QuanFuse(NewQuanNum,Order,.true.)
			else
				FermiFusefTensor=B%QuanFuse(NewQuanNum,Order,.true.)
			end if
		else
			rank=B%getRank()
			i=rank-1
			arrow1=B%getFermiArrow(i)
			arrow2=B%getFermiArrow(rank)
			if(arrow1.ne.arrow2)then
				call writemess(' The fermi-arrows of the fusing legs are not the same')
				call error_stop
			end if
			call fuseOrder(NewQuanNum,order,B%quantumnumber(i),B%quantumnumber(rank),NewRule)
			call NewQuanNum%setFermiArrow(arrow1)
			if(arrow1.eq.(-1))then
				FermiFusefTensor=B.pb.i
				call FermiFusefTensor%SymTensor%backward(i)
				FermiFusefTensor=FermiFusefTensor%QuanFuse(NewQuanNum,Order,.false.)
			else
				FermiFusefTensor=B%QuanFuse(NewQuanNum,Order,.false.)
			end if
		end if
		return
	end function
	



!The order of fusing all legs	 will store in outOrder
!orderinfo store the size(row number) of each fusing order
!example, fusing 4 leg, totaldata of orderinfo will be 3
!  fusing leg1 and leg2,result leg_1
!  order is
!     1 2 1 1 1 1 1
!     1 2 2 1 1 1 1
! outOrder is 
!     1 2 1 1 1 1 1
!     1 2 2 1 1 1 1
! outOrder(3)=2---> there are two row of data
!
!  fusing leg_1 and leg3,result leg_2
!  order is
!     2 2 1 1 1 1 1
! outOrder is 
!     2 2 1 1 1 1 1 ---> this is the neworder
!     1 2 1 1 1 1 1
!     1 2 2 1 1 1 1
! outOrder(2)=1---> there is 1 row of data
!
!  fusing leg_2 and leg4,result leg_3
!  order is
!     2 1 1 1 1 1 1
!     1 2 1 1 1 1 1
! outOrder is 
!     2 1 1 1 1 1 1 ---> this is the neworder
!     1 2 1 1 1 1 1 ---> this is the neworder
!     2 2 1 1 1 1 1 
!     1 2 1 1 1 1 1
!     1 2 2 1 1 1 1
! outOrder(1)=2---> there are 2 row of data
!
!When split data, read the outOrder, get the subTensor of order to split

	type(fTensor) function FermiFusefTensor3(B,ith,outDimen,outOrder,orderinfo,row)result(T)
		class(fTensor),intent(in)::B
		integer,intent(in)::ith
		type(SymDimension),intent(inout)::outDimen
		type(Tensor),intent(inout)::outOrder,orderinfo
		logical,intent(in)::row
		integer::i,rank,j,rankV
		type(Tensor)::order
		type(SymDimension)::dimen
		call orderinfo%empty()
		if(row)then	
			if(ith.gt.1)then
				call orderinfo%allocate((/ith-1/),'integer')
				T=FermiFusefTensor(B,dimen,order,.true.)
				outDimen=dimen
				outOrder=order
				if(order%getRank().eq.0)then
					call writemess('ERROR in FermiFusefTensor3',-1)
					call error_stop
				end if
				if(order%getRank().eq.1)then
					call orderinfo%setValue(ith-1,1)
				else
					call orderinfo%setValue(ith-1,order%dim(1))
				end if
				do i=2,ith-1
					T=FermiFusefTensor(T,dimen,order,.true.)
					outDimen=outDimen+dimen
					if(order%getRank().eq.1)then
						call orderinfo%setValue(ith-i,1)
					else
						call orderinfo%setValue(ith-i,order%dim(1))
					end if
					outOrder=order.Rpaste.outOrder
					!call outOrder%paste(order,.true.)
				end do
			else
				T=B
			end if
		else
			rank=B%getRank()
			rankV=rank-ith+1
			if(rankV.gt.1)then
				call orderinfo%allocate((/rankV-1/),'integer')
				T=FermiFusefTensor(B,dimen,order,.false.)
				outDimen=dimen
				outOrder=order
				if(order%getRank().eq.0)then
					call writemess('ERROR in FermiFusefTensor3',-1)
					call error_stop
				end if
				if(order%getRank().eq.1)then
					call orderinfo%setValue(rankV-1,1)
				else
					call orderinfo%setValue(rankV-1,order%dim(1))
				end if
				do i=2,rankV-1
					T=FermiFusefTensor(T,dimen,order,.false.)
					outDimen=outDimen+dimen
					if(order%getRank().eq.1)then
						call orderinfo%setValue(rankV-i,1)
					else
						call orderinfo%setValue(rankV-i,order%dim(1))
					end if
					outOrder=order.Rpaste.outOrder
					!call outOrder%paste(order,.true.)
				end do
			else
				T=B
			end if
		end if
		
		return
	end function

	type(fTensor) function FermiFusefTensorRule3(B,ith,outDimen,outOrder,orderinfo,row,NewRule)result(T)
		class(fTensor),intent(in)::B
		integer,intent(in)::ith,NewRule
		type(SymDimension),intent(inout)::outDimen
		type(Tensor),intent(inout)::outOrder,orderinfo
		logical,intent(in)::row
		integer::i,rank,j,rankV
		type(Tensor)::order
		type(SymDimension)::dimen
		call orderinfo%empty()
		if(row)then	
			if(ith.gt.1)then
				call orderinfo%allocate((/ith-1/),'integer')
				T=FermiFusefTensorRule(B,dimen,order,.true.,NewRule)
				outDimen=dimen
				outOrder=order
				if(order%getRank().eq.0)then
					call writemess('ERROR in FermiFusefTensor3',-1)
					call error_stop
				end if
				if(order%getRank().eq.1)then
					call orderinfo%setValue(ith-1,1)
				else
					call orderinfo%setValue(ith-1,order%dim(1))
				end if
				do i=2,ith-1
					T=FermiFusefTensorRule(T,dimen,order,.true.,NewRule)
					outDimen=outDimen+dimen
					if(order%getRank().eq.1)then
						call orderinfo%setValue(ith-i,1)
					else
						call orderinfo%setValue(ith-i,order%dim(1))
					end if
					outOrder=order.Rpaste.outOrder
					!call outOrder%paste(order,.true.)
				end do
			else
				T=B
			end if
		else
			rank=B%getRank()
			rankV=rank-ith+1
			if(rankV.gt.1)then
				call orderinfo%allocate((/rankV-1/),'integer')
				T=FermiFusefTensorRule(B,dimen,order,.false.,NewRule)
				outDimen=dimen
				outOrder=order
				if(order%getRank().eq.0)then
					call writemess('ERROR in FermiFusefTensor3',-1)
					call error_stop
				end if
				if(order%getRank().eq.1)then
					call orderinfo%setValue(rankV-1,1)
				else
					call orderinfo%setValue(rankV-1,order%dim(1))
				end if
				do i=2,rankV-1
					T=FermiFusefTensorRule(T,dimen,order,.false.,NewRule)
					outDimen=outDimen+dimen
					if(order%getRank().eq.1)then
						call orderinfo%setValue(rankV-i,1)
					else
						call orderinfo%setValue(rankV-i,order%dim(1))
					end if
					outOrder=order.Rpaste.outOrder
				end do
			else
				T=B
			end if
		end if
		
		return
	end function
	
	type(fTensor) function FermiFusefTensor4(B,ith,row)result(T)
		class(fTensor),intent(in)::B
		integer,intent(in)::ith
		logical,intent(in)::row
		integer::i,rank,j,rankV
		integer,allocatable::index(:)
		if(row)then	
			if(ith.gt.1)then
				T=FermiFusefTensor2(B,.true.)
				do i=2,ith-1
					T=FermiFusefTensor2(T,.true.)
				end do
			else
				T=B
			end if
		else
			rank=B%getRank()
			rankV=rank-ith+1
			if(rankV.gt.2)then
				allocate(index(rankV))
				do i=ith,rank
					index(i-ith+1)=i
				end do
				T=B.pf.index
				T=FermiFusefTensor2(T,.true.)
				do i=2,rankV-1
					T=FermiFusefTensor2(T,.true.)
				end do
				call T%backward(1)
			else if(rankV.eq.2)then
				T=FermiFusefTensor2(B,.false.)
			else
				T=B
			end if
		end if
		
		return
	end function

	type(fTensor) function FermiFusefTensorRule4(B,ith,row,NewRule)result(T)
		class(fTensor),intent(in)::B
		integer,intent(in)::ith,NewRule
		logical,intent(in)::row
		integer::i,rank,j,rankV
		integer,allocatable::index(:)
		if(row)then	
			if(ith.gt.1)then
				T=FermiFusefTensorRule2(B,.true.,NewRule)
				do i=2,ith-1
					T=FermiFusefTensorRule2(T,.true.,NewRule)
				end do
			else
				T=B
			end if
		else
			rank=B%getRank()
			rankV=rank-ith+1
			if(rankV.gt.2)then
				allocate(index(rankV))
				do i=ith,rank
					index(i-ith+1)=i
				end do
				T=B.pf.index
				T=FermiFusefTensorRule2(T,.true.,NewRule)
				do i=2,rankV-1
					T=FermiFusefTensorRule2(T,.true.,NewRule)
				end do
				call T%backward(1)
			else if(rankV.eq.2)then
				T=FermiFusefTensorRule2(B,.false.,NewRule)
			else
				T=B
			end if
		end if
		
		return
	end function
	
	
	
	type(fTensor) function FermiSplitfTensor(B,NewQuanDimen,Order,row)
		class(fTensor),intent(in)::B
		type(SymDimension),intent(in)::NewQuanDimen
		type(QuanNum)::NewQ1,NewQ2
		type(Tensor),intent(in)::Order
		logical,intent(in)::row
		integer::i,arrow
		NewQ1=NewQuanDimen%QuantumNumber(1)
		NewQ2=NewQuanDimen%QuantumNumber(2)
		if(row)then
			FermiSplitfTensor=B%QuanSplit(NewQ1,NewQ2,Order,.true.)
			arrow=FermiSplitfTensor%getFermiArrow(1)
			if(arrow.eq.(-1))then
				call FermiSplitfTensor%SymTensor%forward(2)
				call FermiSplitfTensor%forward(2)
			end if
		else
			i=B%getrank()
			FermiSplitfTensor=B%QuanSplit(NewQ1,NewQ2,Order,.false.)
			arrow=FermiSplitfTensor%getFermiArrow(i+1)
			if(arrow.eq.(-1))then
				call FermiSplitfTensor%SymTensor%backward(i)
				call FermiSplitfTensor%backward(i)
			end if
		end if

		if(NewQuanDimen%outNameFlag().eq.1)then
			if(row)then
				call FermiSplitfTensor%setName(1,NewQuanDimen%outName(1))
				call FermiSplitfTensor%setName(2,NewQuanDimen%outName(2))
			else
				call FermiSplitfTensor%setName(FermiSplitfTensor%getRank()-1,NewQuanDimen%outName(1))
				call FermiSplitfTensor%setName(FermiSplitfTensor%getRank(),NewQuanDimen%outName(2))
			end if
		end if
		return
	end function
	type(fTensor) function FermiSplitfTensor2(B,inDimen,inOrder,orderinfo,row)result(T)
		class(fTensor),intent(in)::B
		type(SymDimension),intent(in)::inDimen
		type(Tensor),intent(in)::inOrder,orderinfo
		logical,intent(in)::row
		integer::i,rank,j,rankV,orderindex,istart,iend
		type(Tensor)::order
		type(SymDimension)::dimen
		rank=inDimen%getRank()
		if(rank.eq.0)then
			T=B
			return
		end if
		if(rank.lt.2)then
			call writemess('ERROR in Split fTensor, input parameter error',-1)
			call error_stop()
		end if
		orderindex=1
		dimen=inDimen.subdim.[rank-1,rank]
		istart=1
		iend=orderinfo%ii(orderindex)
		order=inOrder%subTensor((/-1,istart,iend/))
		istart=iend+1
		T=FermiSplitfTensor(B,dimen,Order,row)
		do i=rank-2,2,-2
			j=i-1
			orderindex=orderindex+1
			dimen=inDimen.subdim.[i-1,i]
			iend=istart+orderinfo%ii(orderindex)-1
			order=inOrder%subTensor((/-1,istart,iend/))
			istart=iend+1
			T=FermiSplitfTensor(T,dimen,order,row)
		end do
		return
	end function

	
	
	
	subroutine fSVDRoutine(T,U,S,V,QuanNumCut)
		class(fTensor),intent(in)::T
		type(fTensor),intent(inout)::U,S,V
		type(QuanNum),optional,intent(in)::QuanNumCut
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,QuanNumCut)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		return
	end subroutine
	subroutine fSVDRoutine_kill_inData(T,U,S,V,QuanNumCut)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		type(QuanNum),optional,intent(in)::QuanNumCut
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,QuanNumCut)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		return
	end subroutine
	subroutine fSVDRoutineNum(T,U,S,V,NumSave)
		class(fTensor),intent(in)::T
		type(fTensor),intent(inout)::U,S,V
		integer,intent(in)::NumSave
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,NumSave)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		return
	end subroutine
	subroutine fSVDRoutineNum_kill_inData(T,U,S,V,NumSave)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		integer,intent(in)::NumSave
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,NumSave)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		return
	end subroutine

	subroutine fSVDRoutineNumMaxValue(T,U,S,V,minNum,NumSave,maxValue,VType,outNumSave,outMaxValue)
		class(fTensor),intent(in)::T
		type(fTensor),intent(inout)::U,S,V
		integer,intent(in)::minNum
		integer,intent(in)::NumSave
		real*8,intent(in)::maxValue
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::VType
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,minNum,NumSave,maxValue,VType,outNumSave,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		return
	end subroutine
	subroutine fSVDRoutineNumMaxValue_kill_inData(T,U,S,V,minNum,NumSave,maxValue,VType,outNumSave,outMaxValue)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		integer,intent(in)::minNum
		integer,intent(in)::NumSave
		real*8,intent(in)::maxValue
		character(len=*),intent(in)::VType
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,minNum,NumSave,maxValue,VType,outNumSave,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		return
	end subroutine

	subroutine fSVDRoutineMaxValue(T,U,S,V,maxValue,VType,outMaxValue)
		class(fTensor),intent(in)::T
		type(fTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue
		character(len=*),intent(in)::VType
		real*8,intent(inout),optional::outMaxValue
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,maxValue,VType,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		return
	end subroutine
	subroutine fSVDRoutineMaxValue_kill_inData(T,U,S,V,maxValue,VType,outMaxValue)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue
		character(len=*),intent(in)::VType
		real*8,intent(inout),optional::outMaxValue
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,maxValue,VType,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		return
	end subroutine

	subroutine fSVDNameRoutine(Tin,U,S,V,nameU,nameV,QuanNumCut)
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::U,S,V
		type(QuanNum),optional,intent(in)::QuanNumCut
		character(len=*)::nameU,nameV
		type(fTensor)::T
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Tin%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(Tin%outTensorName(i).equ.nameU) rankU=rankU+1
			if(Tin%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU.le.rankV)then
			T=Tin.pf.nameU
		else
			T=Tin.pb.nameV
		end if
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in SVDcutoff_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,QuanNumCut)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 
	subroutine fSVDNameRoutine_kill_inData(T,U,S,V,nameU,nameV,QuanNumCut)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		type(QuanNum),optional,intent(in)::QuanNumCut
		character(len=*)::nameU,nameV
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=T%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(T%outTensorName(i).equ.nameU) rankU=rankU+1
			if(T%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in SVDcutoff_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		if(rankU.le.rankV)then
			call T%forward(nameU)
		else
			call T%backWard(nameV)
		end if
		
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,QuanNumCut)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 


	subroutine fSVDNameNumRoutine(Tin,U,S,V,nameU,nameV,QuanNumCut)
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::U,S,V
		integer,intent(in)::QuanNumCut
		character(len=*)::nameU,nameV
		type(fTensor)::T
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Tin%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(Tin%outTensorName(i).equ.nameU) rankU=rankU+1
			if(Tin%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU.le.rankV)then
			T=Tin.pf.nameU
		else
			T=Tin.pb.nameV
		end if
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in SVDcutoff_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,QuanNumCut)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 
	subroutine fSVDNameNumRoutine_kill_inData(T,U,S,V,nameU,nameV,QuanNumCut)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		integer,intent(in)::QuanNumCut
		character(len=*)::nameU,nameV
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=T%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(T%outTensorName(i).equ.nameU) rankU=rankU+1
			if(T%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in SVDcutoff_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		if(rankU.le.rankV)then
			call T%forward(nameU)
		else
			call T%backward(nameV)
		end if
		
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,QuanNumCut)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine


	subroutine fSVDNameNumMaxValueRoutine(Tin,U,S,V,nameU,nameV,minNum,NumSave,&
						maxValue,Vtype,outNumSave,outMaxValue)
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::U,S,V
		integer,intent(in)::minNum
		integer,intent(in)::NumSave
		real*8,intent(in)::maxValue
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::Vtype
		character(len=*),intent(in)::nameU,nameV
		type(fTensor)::T
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Tin%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(Tin%outTensorName(i).equ.nameU) rankU=rankU+1
			if(Tin%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU.le.rankV)then
			T=Tin.pf.nameU
		else
			T=Tin.pb.nameV
		end if
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in SVDcutoff_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,minNum,NumSave,maxValue,VType,outNumSave,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	subroutine fSVDNameNumMaxValueRoutine_kill_inData(T,U,S,V,nameU,nameV,minNum,NumSave,&
						maxValue,Vtype,outNumSave,outMaxValue)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		integer,intent(in)::minNum
		integer,intent(in)::NumSave
		real*8,intent(in)::maxValue
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::Vtype
		character(len=*),intent(in)::nameU,nameV
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=T%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(T%outTensorName(i).equ.nameU) rankU=rankU+1
			if(T%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in SVDcutoff_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		if(rankU.le.rankV)then
			call T%forward(nameU)
		else
			call T%backward(nameV)
		end if
		
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVDKill(U%SymTensor,S%SymTensor,V%SymTensor,minNum,NumSave,maxValue,VType,outNumSave,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	subroutine fSVDNameMaxValueRoutine(Tin,U,S,V,nameU,nameV,maxValue,Vtype,outMaxValue)
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue
		character(len=*),intent(in)::Vtype
		character(len=*),intent(in)::nameU,nameV
		real*8,intent(inout),optional::outMaxValue
		type(fTensor)::T
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Tin%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(Tin%outTensorName(i).equ.nameU) rankU=rankU+1
			if(Tin%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU.le.rankV)then
			T=Tin.pf.nameU
		else
			T=Tin.pb.nameV
		end if
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in SVDcutoff_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,maxValue,VType,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	subroutine fSVDNameMaxValueRoutine_kill_inData(T,U,S,V,nameU,nameV,maxValue,Vtype,outMaxValue)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue
		character(len=*),intent(in)::Vtype
		character(len=*),intent(in)::nameU,nameV
		real*8,intent(inout),optional::outMaxValue
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=T%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(T%outTensorName(i).equ.nameU) rankU=rankU+1
			if(T%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in SVDcutoff_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in SVDcutoff_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		if(rankU.le.rankV)then
			call T%forward(nameU)
		else
			call T%backward(nameV)
		end if
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,maxValue,VType,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 




	subroutine fSVDNameRoutineLeft(Tin,U,S,V,SVDname,NumSave,leftFlag)
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::U,S,V
		type(QuanNum),intent(in)::NumSave
		character(len=*)::SVDname(:)
		logical,intent(in)::leftFlag
		type(fTensor)::T
		integer::lenSVDname,rank,lenLeft
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Tin%getRank()
		lenSVDname=size(SVDname)
		if(lenSVDname.ge.rank)then
			call writemess("ERROR in SVDNameNumRoutine, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(leftFlag)then
			T=Tin.pf.SVDname
			lenLeft=lenSVDname
		else
			T=Tin.pb.SVDname
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,NumSave)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)

		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	subroutine fSVDNameRoutineLeft_kill_inData(T,U,S,V,SVDname,NumSave,leftFlag)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		type(QuanNum),intent(in)::NumSave
		character(len=*)::SVDname(:)
		logical,intent(in)::leftFlag
		integer::lenSVDname,rank,lenLeft
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=T%getRank()
		lenSVDname=size(SVDname)
		if(lenSVDname.ge.rank)then
			call writemess("ERROR in SVDNameNumRoutine, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(leftFlag)then
			call T%forward(SVDname)
			lenLeft=lenSVDname
		else
			call T%backward(SVDname)
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,NumSave)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)

		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	subroutine fSVDNameRoutineLeft2(Tin,U,S,V,SVDname,leftFlag)
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::U,S,V
		character(len=*)::SVDname(:)
		logical,intent(in)::leftFlag
		type(fTensor)::T
		integer::lenSVDname,rank,lenLeft
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Tin%getRank()
		lenSVDname=size(SVDname)
		if(lenSVDname.ge.rank)then
			call writemess("ERROR in SVDNameNumRoutine, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(leftFlag)then
			T=Tin.pf.SVDname
			lenLeft=lenSVDname
		else
			T=Tin.pb.SVDname
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)

		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 
	subroutine fSVDNameRoutineLeft2_kill_inData(T,U,S,V,SVDname,leftFlag)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		character(len=*)::SVDname(:)
		logical,intent(in)::leftFlag
		integer::lenSVDname,rank,lenLeft
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=T%getRank()
		lenSVDname=size(SVDname)
		if(lenSVDname.ge.rank)then
			call writemess("ERROR in SVDNameNumRoutine, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(leftFlag)then
			call T%forward(SVDname)
			lenLeft=lenSVDname
		else
			call T%backward(SVDname)
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)

		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine


	subroutine fSVDNameNumRoutineLeft(Tin,U,S,V,SVDname,NumSave,leftFlag)
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::U,S,V
		integer,intent(in)::NumSave
		character(len=*)::SVDname(:)
		logical,intent(in)::leftFlag
		type(fTensor)::T
		integer::lenSVDname,rank,lenLeft
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Tin%getRank()
		lenSVDname=size(SVDname)
		if(lenSVDname.ge.rank)then
			call writemess("ERROR in SVDNameNumRoutine, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(leftFlag)then
			T=Tin.pf.SVDname
			lenLeft=lenSVDname
		else
			T=Tin.pb.SVDname
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,NumSave)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)

		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 
	subroutine fSVDNameNumRoutineLeft_kill_inData(T,U,S,V,SVDname,NumSave,leftFlag)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		integer,intent(in)::NumSave
		character(len=*)::SVDname(:)
		logical,intent(in)::leftFlag
		integer::lenSVDname,rank,lenLeft
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=T%getRank()
		lenSVDname=size(SVDname)
		if(lenSVDname.ge.rank)then
			call writemess("ERROR in SVDNameNumRoutine, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(leftFlag)then
			call T%forward(SVDname)
			lenLeft=lenSVDname
		else
			call T%backward(SVDname)
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,NumSave)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)

		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 





	subroutine fSVDNameNumMaxValueRoutineLeft(Tin,U,S,V,SVDname,minNum,NumSave,maxValue,Vtype,&
						leftFlag,outNumSave,outMaxValue)
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue
		character(len=*),intent(in)::Vtype
		integer,intent(in)::minNum
		integer,intent(in)::NumSave
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::SVDname(:)
		logical,intent(in)::leftFlag
		type(fTensor)::T
		integer::lenSVDname,rank,lenLeft
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Tin%getRank()
		lenSVDname=size(SVDname)
		if(lenSVDname.ge.rank)then
			call writemess("ERROR in SVDNameNumRoutine, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(leftFlag)then
			T=Tin.pf.SVDname
			lenLeft=lenSVDname
		else
			T=Tin.pb.SVDname
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,minNum,NumSave,maxValue,Vtype,outNumSave,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)


		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 
	subroutine fSVDNameNumMaxValueRoutineLeft_kill_inData(T,U,S,V,SVDname,minNum,NumSave,maxValue,Vtype,&
					leftFlag,outNumSave,outMaxValue)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue
		character(len=*),intent(in)::Vtype
		integer,intent(in)::minNum
		integer,intent(in)::NumSave
		character(len=*),intent(in)::SVDname(:)
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		logical,intent(in)::leftFlag
		integer::lenSVDname,rank,lenLeft
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=T%getRank()
		lenSVDname=size(SVDname)
		if(lenSVDname.ge.rank)then
			call writemess("ERROR in SVDNameNumRoutine, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(leftFlag)then
			call T%forward(SVDname)
			lenLeft=lenSVDname
		else
			call T%backward(SVDname)
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,minNum,NumSave,maxValue,Vtype,outNumSave,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)


		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 


	subroutine fSVDNameMaxValueRoutineLeft(Tin,U,S,V,SVDname,maxValue,Vtype,leftFlag,outMaxValue)
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue
		character(len=*),intent(in)::Vtype
		character(len=*),intent(in)::SVDname(:)
		logical,intent(in)::leftFlag
		real*8,intent(inout),optional::outMaxValue
		type(fTensor)::T
		integer::lenSVDname,rank,lenLeft
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Tin%getRank()
		lenSVDname=size(SVDname)
		if(lenSVDname.ge.rank)then
			call writemess("ERROR in SVDNameNumRoutine, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(leftFlag)then
			T=Tin.pf.SVDname
			lenLeft=lenSVDname
		else
			T=Tin.pb.SVDname
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVD(U%SymTensor,S%SymTensor,V%SymTensor,maxValue,Vtype,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)


		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 
	subroutine fSVDNameMaxValueRoutineLeft_kill_inData(T,U,S,V,SVDname,maxValue,Vtype,leftFlag,outMaxValue)
		class(fTensor),intent(inout)::T
		type(fTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue
		character(len=*),intent(in)::Vtype
		character(len=*),intent(in)::SVDname(:)
		logical,intent(in)::leftFlag
		real*8,intent(inout),optional::outMaxValue
		integer::lenSVDname,rank,lenLeft
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=T%getRank()
		lenSVDname=size(SVDname)
		if(lenSVDname.ge.rank)then
			call writemess("ERROR in SVDNameNumRoutine, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(leftFlag)then
			call T%forward(SVDname)
			lenLeft=lenSVDname
		else
			call T%backward(SVDname)
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%SVDkill(U%SymTensor,S%SymTensor,V%SymTensor,maxValue,Vtype,outMaxValue)
		call U%setFermiArrow(2,-1)
		call S%setFermiArrow(1,1)
		call S%setFermiArrow(2,-1)
		call V%setFermiArrow(1,1)


		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V%SymTensor)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	

	
	
	subroutine fQRdecomposition(T,Q,R) 
		class(fTensor),intent(in)::T
		type(fTensor),intent(inout)::Q,R
		call T%SymTensor%QR(Q%SymTensor,R%SymTensor)
		call Q%setFermiArrow(2,-1)
		call R%setFermiArrow(1,1)
		return
	end subroutine

	subroutine fQRdecomposition_kill_inData(Q,R) 
		class(fTensor),intent(inout)::Q
		type(fTensor),intent(inout)::R
		call Q%SymTensor%QRkill(R%SymTensor)
		call Q%setFermiArrow(2,-1)
		call R%setFermiArrow(1,1)
		return
	end subroutine
	
	subroutine fQRRoutineNameLeft(Tin,Q,R,QRName,LeftFlag_)
		class(fTensor),intent(in)::Tin
		character(len=*),intent(in)::QRName(:)
		logical,optional,intent(in)::LeftFlag_
		type(fTensor),intent(inout)::Q,R
		logical::LeftFlag,reverFlag
		integer::lenQRName,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		Type(fTensor)::T
		integer::lenLeft
		if(present(LeftFlag_))then
			LeftFlag=LeftFlag_
		else
			LeftFlag=.true.
		end if
		rank=Tin%getRank()
		lenQRName=size(QRName)
		if(lenQRName.ge.rank)then
			call writemess("ERROR in QRfTensor, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(LeftFlag)then
			T=Tin.pf.QRName
			lenLeft=lenQRName
		else
			T=Tin.pb.QRName
			lenLeft=rank-lenQRName
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%QR(Q%SymTensor,R%SymTensor) 
		call Q%setFermiArrow(2,-1)
		call R%setFermiArrow(1,1)
		if(Tin%getNameFlag().ne.0)then
			call Q%setName(2,'QR.Q')
			call R%setName(1,'QR.R')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(R%SymTensor)
		end if
		Q=Q%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		R=R%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine
	subroutine fQRRoutineNameLeft_kill_inData(Q,R,QRName,LeftFlag_)
		class(fTensor),intent(inout)::Q
		character(len=*),intent(in)::QRName(:)
		logical,optional,intent(in)::LeftFlag_
		type(fTensor),intent(inout)::R
		logical::LeftFlag,reverFlag
		integer::lenQRName,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		integer::lenLeft
		if(present(LeftFlag_))then
			LeftFlag=LeftFlag_
		else
			LeftFlag=.true.
		end if
		rank=Q%getRank()
		lenQRName=size(QRName)
		if(lenQRName.ge.rank)then
			call writemess("ERROR in QRfTensor, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(LeftFlag)then
			call Q%forward(QRName)
			lenLeft=lenQRName
		else
			call Q%backWard(QRName)
			lenLeft=rank-lenQRName
		end if
		Q=Q%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		Q=Q%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(Q%getRule(1)*Q%getRule(2).ge.0)then
			call reverseSymmetryRule(Q%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call Q%SymTensor%QRkill(R%SymTensor) 
		call Q%setFermiArrow(2,-1)
		call R%setFermiArrow(1,1)
		if(Q%getNameFlag().ne.0)then
			call Q%setName(2,'QR.Q')
			call R%setName(1,'QR.R')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(R%SymTensor)
		end if
		Q=Q%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		R=R%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine



	subroutine fQRRoutineName(Tin,Q,R,nameU,nameV)
		character(len=*),intent(in)::nameU,nameV
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::Q,R
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		Type(fTensor)::T
		logical::reverFlag
		rank=Tin%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(Tin%outTensorName(i).equ.nameU) rankU=rankU+1
			if(Tin%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in QRRoutineName",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call Tin%diminfo()
			call writemess(nameU+','+nameV,-1)
			call error_stop()
		end if
		if(rankU.le.rankV)then
			T=Tin.pf.nameU
		else
			T=Tin.pb.nameV
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in QRRoutineName,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in QRRoutineName,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%QR(Q%SymTensor,R%SymTensor) 
		call Q%setFermiArrow(2,-1)
		call R%setFermiArrow(1,1)
		if(Tin%getNameFlag().ne.0)then
			call Q%setName(2,'QR.Q')
			call R%setName(1,'QR.R')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(R%SymTensor)
		end if
		Q=Q%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		R=R%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine

	subroutine fQRRoutineName_kill_inData(Q,R,nameU,nameV)
		class(fTensor),intent(inout)::Q
		character(len=*),intent(in)::nameU,nameV
		type(fTensor),intent(inout)::R
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Q%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(Q%outTensorName(i).equ.nameU) rankU=rankU+1
			if(Q%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in QRRoutineName",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call Q%diminfo()
			call writemess(nameU+','+nameV,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in QRRoutineName,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in QRRoutineName,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		if(rankU.le.rankV)then
			call Q%forward(nameU)
		else
			call Q%backward(nameV)
		end if
		
		Q=Q%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		Q=Q%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(Q%getRule(1)*Q%getRule(2).ge.0)then
			call reverseSymmetryRule(Q%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call Q%SymTensor%QRkill(R%SymTensor) 
		call Q%setFermiArrow(2,-1)
		call R%setFermiArrow(1,1)
		if(Q%getNameFlag().ne.0)then
			call Q%setName(2,'QR.Q')
			call R%setName(1,'QR.R')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(R%SymTensor)
		end if
		Q=Q%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		R=R%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine



	subroutine fLQdecomposition(T,L,Q) 
		class(fTensor),intent(in)::T
		type(fTensor),intent(inout)::L,Q
		call T%SymTensor%LQ(L%SymTensor,Q%SymTensor)
		call L%setFermiArrow(2,-1)
		call Q%setFermiArrow(1,1)
		return
	end subroutine

	subroutine fLQdecomposition_kill_inData(Q,L) 
		class(fTensor),intent(inout)::Q
		type(fTensor),intent(inout)::L
		call Q%SymTensor%LQkill(L%SymTensor)
		call L%setFermiArrow(2,-1)
		call Q%setFermiArrow(1,1)
		return
	end subroutine


	subroutine fLQRoutineNameLeft(Tin,L,Q,QRName,LeftFlag_)
		character(len=*),intent(in)::QRName(:)
		logical,optional,intent(in)::LeftFlag_
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::L,Q
		logical::LeftFlag,reverFlag
		integer::lenQRName,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		Type(fTensor)::T
		integer::lenLeft
		if(present(LeftFlag_))then
			LeftFlag=LeftFlag_
		else
			LeftFlag=.true.
		end if
		rank=Tin%getRank()
		lenQRName=size(QRName)
		if(lenQRName.ge.rank)then
			call writemess("ERROR in LQ SymTensor, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(LeftFlag)then
			T=Tin.pf.QRName
			lenLeft=lenQRName
		else
			T=Tin.pb.QRName
			lenLeft=rank-lenQRName
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%LQ(L%SymTensor,Q%SymTensor) 
		call L%setFermiArrow(2,-1)
		call Q%setFermiArrow(1,1)
		if(Tin%getNameFlag().ne.0)then
			call L%setName(2,'LQ.L')
			call Q%setName(1,'LQ.Q')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(Q%SymTensor)
		end if
		L=L%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		Q=Q%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine

	subroutine fLQRoutineNameLeft_kill_inData(Q,L,QRName,LeftFlag_)
		class(fTensor),intent(inout)::Q
		character(len=*),intent(in)::QRName(:)
		logical,optional,intent(in)::LeftFlag_
		type(fTensor),intent(inout)::L
		logical::LeftFlag,reverFlag
		integer::lenQRName,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		integer::lenLeft
		if(present(LeftFlag_))then
			LeftFlag=LeftFlag_
		else
			LeftFlag=.true.
		end if
		rank=Q%getRank()
		lenQRName=size(QRName)
		if(lenQRName.ge.rank)then
			call writemess("ERROR in LQ SymTensor, input name larger than the rank of the Tensor",-1)
			call error_stop()
		end if
		if(LeftFlag)then
			call Q%forward(QRName)
			lenLeft=lenQRName
		else
			call Q%backward(QRName)
			lenLeft=rank-lenQRName
		end if
		Q=Q%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		Q=Q%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(Q%getRule(1)*Q%getRule(2).ge.0)then
			call reverseSymmetryRule(Q%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call Q%SymTensor%LQKill(L%SymTensor) 
		call L%setFermiArrow(2,-1)
		call Q%setFermiArrow(1,1)
		if(Q%getNameFlag().ne.0)then
			call L%setName(2,'LQ.L')
			call Q%setName(1,'LQ.Q')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(Q%SymTensor)
		end if
		L=L%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		Q=Q%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine

	subroutine fLQRoutineName(Tin,L,Q,nameU,nameV)
		character(len=*),intent(in)::nameU,nameV
		type(fTensor),intent(inout)::L,Q
		class(fTensor),intent(in)::Tin
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		Type(fTensor)::T
		logical::reverFlag
		rank=Tin%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(Tin%outTensorName(i).equ.nameU) rankU=rankU+1
			if(Tin%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in LQ SymTensor_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call Tin%diminfo()
			call writemess(nameU+','+nameV,-1)
			call error_stop()
		end if
		if(rankU.le.rankV)then
			T=Tin.pf.nameU
		else
			T=Tin.pb.nameV
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in LQ SymTensor_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in LQ SymTensor_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call T%SymTensor%LQ(L%SymTensor,Q%SymTensor) 
		call L%setFermiArrow(2,-1)
		call Q%setFermiArrow(1,1)
		if(Tin%getNameFlag().ne.0)then
			call L%setName(2,'LQ.L')
			call Q%setName(1,'LQ.Q')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(Q%SymTensor)
		end if
		L=L%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		Q=Q%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine

	subroutine fLQRoutineName_kill_inData(Q,L,nameU,nameV)
		class(fTensor),intent(inout)::Q
		character(len=*),intent(in)::nameU,nameV
		type(fTensor),intent(inout)::L
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		logical::reverFlag
		rank=Q%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(Q%outTensorName(i).equ.nameU) rankU=rankU+1
			if(Q%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in LQ SymTensor_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call Q%diminfo()
			call writemess(nameU+','+nameV,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in LQ SymTensor_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in LQ SymTensor_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		if(rankU.le.rankV)then
			call Q%forward(nameU)
		else
			call Q%backward(nameV)
		end if
		
		Q=Q%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		Q=Q%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(Q%getRule(1)*Q%getRule(2).ge.0)then
			call reverseSymmetryRule(Q%SymTensor)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call Q%SymTensor%LQkill(L%SymTensor) 
		call L%setFermiArrow(2,-1)
		call Q%setFermiArrow(1,1)
		if(Q%getNameFlag().ne.0)then
			call L%setName(2,'LQ.L')
			call Q%setName(1,'LQ.Q')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(Q%SymTensor)
		end if
		L=L%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		Q=Q%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine
	
	
	
		
	
	type(fTensor) function directProductTensor(T1,T2)result(Res)
		type(fTensor),intent(in) :: T1,T2
		Res%SymTensor=T1%SymTensor.kron.T2%SymTensor
		return
	end function
	
	
	type(Tensor) function TdotTensor(phi1,phi2)result(dotTensor)
		Type(fTensor),intent(in)::phi1,phi2
		dotTensor=phi1%SymTensor.x.phi2%SymTensor
		RETURN
	end function
	real function sdotTensor(phi1,phi2)result(dotTensor)
		Type(fTensor),intent(in)::phi1,phi2
		dotTensor=phi1%SymTensor.sx.phi2%SymTensor
		RETURN
	end function
	real*8 function ddotTensor(phi1,phi2)result(dotTensor)
		Type(fTensor),intent(in)::phi1,phi2
		dotTensor=phi1%SymTensor.dx.phi2%SymTensor
		RETURN
	end function
	complex(kind=4) function cdotTensor(phi1,phi2)result(dotTensor)
		Type(fTensor),intent(in)::phi1,phi2
		dotTensor=phi1%SymTensor.cx.phi2%SymTensor
		RETURN
	end function
	complex(kind=8) function zdotTensor(phi1,phi2)result(dotTensor)
		Type(fTensor),intent(in)::phi1,phi2
		dotTensor=phi1%SymTensor.zx.phi2%SymTensor
		RETURN
	end function


	type(Tensor) function TdotUTensor(phi1,phi2)result(dotTensor)
		Type(fTensor),intent(in)::phi1,phi2
		dotTensor=phi1%SymTensor.dot.phi2%SymTensor
		RETURN
	end function
	real function sdotUTensor(phi1,phi2)result(dotTensor)
		Type(fTensor),intent(in)::phi1,phi2
		dotTensor=phi1%SymTensor.sdot.phi2%SymTensor
		RETURN
	end function
	real*8 function ddotUTensor(phi1,phi2)result(dotTensor)
		Type(fTensor),intent(in)::phi1,phi2
		dotTensor=phi1%SymTensor.ddot.phi2%SymTensor
		RETURN
	end function
	complex(kind=4) function cdotUTensor(phi1,phi2)result(dotTensor)
		Type(fTensor),intent(in)::phi1,phi2
		dotTensor=phi1%SymTensor.cdot.phi2%SymTensor
		RETURN
	end function
	complex(kind=8) function zdotUTensor(phi1,phi2)result(dotTensor)
		Type(fTensor),intent(in)::phi1,phi2
		dotTensor=phi1%SymTensor.zdot.phi2%SymTensor
		RETURN
	end function
	
	


! reorder the Tensor, order the leg as the order in allTensorName(:)

	subroutine  DimOrder(inoutT,allTensorName)
		class(fTensor),intent(inout)::inoutT
		character(len=*),intent(in)::allTensorName(:)
		character(len=max_len_of_char_in_TData),allocatable :: TensorName(:)
		character(len=max_len_of_char_in_TData),allocatable :: Names(:)
		integer::rank,i
		if(.not.inoutT%getFlag())then
			call writemess('There is no data in the fTensor',-1)
			call error_stop
		endif
		rank=inoutT%getRank()
		allocate(TensorName(rank))
		allocate(Names(rank))
		do i=1,rank
			TensorName(i)=inoutT%outTensorName(i)
			Names(i)=inoutT%getName(i)
		end do
		call NameOrder_sort(TensorName,Names,allTensorName,rank)
		call inoutT%permute(Names)
		return
	end subroutine

!Do not Check if There is no such name

	subroutine NameOrder_sort(TensorName,inoutName,allTensorName,n)
		integer,intent(in)::n
		character(len=*),intent(in)::allTensorName(:)
		character(len=*),intent(inout) :: inoutName(n),TensorName(n)
		character(len=max_len_of_char_in_TData) :: temp
		integer :: i,j
		do i=1,n-1
		 do j=i+1,n
			  if ( checkOrder(TensorName(i),TensorName(j),allTensorName) ) then
					temp = inoutName(i)
					inoutName(i) = inoutName(j)
					inoutName(j) = temp
					temp = TensorName(i)
					TensorName(i) = TensorName(j)
					TensorName(j) = temp
			  endif
		 enddo
		enddo
		return
	end subroutine


	logical function checkOrder(name1,name2,allTensorName)
		character(len=*),intent(in)::name1,name2,allTensorName(:)
		integer::i,lenName
		lenName=size(allTensorName)
		do i=1,lenName
			if(name1.equ.allTensorName(i))then
				checkOrder=.false.
				return
			end if
			if(name2.equ.allTensorName(i))then
				checkOrder=.true.
				return
			end if
		end do
		call writemess('ERROR in checkOrder',-1)
		call error_stop
	end function


!************************************************************************************
!************************************************************************************
!                              Old Function 
!
	subroutine fSVDNameNumMaxValueOld(Tin,U,S,V,maxValue,Vtype,SVDname,minNum,NumSave,leftFlag)
		class(fTensor),intent(in)::Tin
		type(fTensor),intent(inout)::U,S,V
		real*8,intent(inout)::maxValue
		character(len=*),intent(in)::Vtype
		integer,intent(in)::minNum
		integer,intent(inout)::NumSave
		character(len=*),intent(in)::SVDname(:)
		logical,intent(in)::leftFlag
		call fSVDNameNumMaxValueRoutineLeft(Tin,U,S,V,SVDname,minNum,NumSave,maxValue,Vtype,leftFlag)
	end subroutine

	function SVDNameNumOld(Tin,nameU,nameV,NumSave)Result(SVDName)
		type(fTensor),allocatable::SVDName(:)
		class(fTensor),intent(in)::Tin
		integer,intent(in)::NumSave
		character(len=*),intent(in)::nameU,nameV
		allocate(SVDName(3))
		call fSVDNameNumRoutine(Tin,SVDName(1),SVDName(2),SVDName(3),nameU,nameV,NumSave)
	end function


	function QRfTensor_name(Tin,nameU,nameV)result(Res)
		type(fTensor),allocatable::Res(:)
		character(len=*),intent(in)::nameU,nameV
		class(fTensor),intent(in)::Tin
		allocate(Res(2))
		call fQRRoutineName(Tin,Res(1),Res(2),nameU,nameV)
		return
	end function

	function QRfTensor_name2(Tin,QRName,LeftFlag_)result(Res)
		type(fTensor),allocatable::Res(:)
		character(len=*),intent(in)::QRName(:)
		logical,optional,intent(in)::LeftFlag_
		class(fTensor),intent(in)::Tin
		allocate(Res(2))
		call fQRRoutineNameLeft(Tin,Res(1),Res(2),QRName,LeftFlag_)
		return
	end function

	function LQfTensor_name(Tin,nameU,nameV)result(Res)
		type(fTensor),allocatable::Res(:)
		character(len=*),intent(in)::nameU,nameV
		class(fTensor),intent(in)::Tin
		allocate(Res(2))
		call fLQRoutineName(Tin,Res(1),Res(2),nameU,nameV)
		return
	end function


	function LQfTensor_name2(Tin,LQName,LeftFlag_)result(Res)
		type(fTensor),allocatable::Res(:)
		character(len=*),intent(in)::LQName(:)
		logical,optional,intent(in)::LeftFlag_
		class(fTensor),intent(in)::Tin
		allocate(Res(2))
		call fLQRoutineNameLeft(Tin,Res(1),Res(2),LQName,LeftFlag_)
		return
	end function








!**********************************************************************
!**********************************************************************
!	the code below is for MPI
!**********************************************************************

	subroutine MPI_send_fTensor(Ten1,Ten2,ID1,ID2,ierr,MPIcommon)
		type(fTensor),intent(in)::Ten1
		type(fTensor),intent(inout)::Ten2
		integer,intent(in)::ID1,ID2
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		call MPI_send_SymTensor(Ten1%SymTensor,Ten2%SymTensor,ID1,ID2,ierr,MPIcommon)
		return
	end subroutine
	subroutine MPI_BCAST_fTensor(Ten1,ID,ierr,MPIcommon)
		type(fTensor),intent(inout)::Ten1
		integer,intent(in)::ID
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		call MPI_BCAST_SymTensor(Ten1%SymTensor,ID,ierr,MPIcommon)
		return
	end subroutine



	subroutine MPI_SUM_fTensor1(inoutTensor,ierr,MPIcommon)
		type(fTensor),intent(inout)::inoutTensor
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		call MPI_SUM_SymTensor(inoutTensor%SymTensor,ierr,MPIcommon)
		return
	end subroutine






















end module
