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
!SymTensor=Tensoe may error in some case, becase the relation of quantum number and the index
!
!The Tensor product will not check the quantum number. It regard the Tensor(the block) as matrix and
! make used of the matrix-matrix product.
!
! The code will do nothing on the fermi-arrow in the symdimension
!

module SymTensor_type
	use QuantumNumber_Type
	use SymDimension_typede
	use Tensor_type
	use Dimension_typede
	use Tools
	use U1Tool
	use ParityTool
	use mpi
	implicit none
	private

	integer,public::default_Row_Rule_=1
	integer,public::default_Col_Rule_=-1

	abstract INTERFACE
	  SUBROUTINE checkSymmetryRuleExternal(Rule1,Rule2,lenname1,lenname2)
		integer,intent(in)::Rule1,Rule2
		character(len=*),intent(in)::lenname1,lenname2
	  END SUBROUTINE checkSymmetryRuleExternal
	END INTERFACE
	procedure(checkSymmetryRuleExternal),public,pointer::checkSymmetryRule=>defaultcheckSymmetryRule


	abstract INTERFACE
	  SUBROUTINE reverseRuleDataRoutine(T,dimen,LD1,LD2)
	  	import :: Tensor
	  	import :: Symdimension
		integer,intent(in)::LD1,LD2
		Type(Tensor),intent(inout)::T(LD1,LD2)
		Type(Symdimension),intent(inout)::dimen
	  END SUBROUTINE reverseRuleDataRoutine
	END INTERFACE
	procedure(reverseRuleDataRoutine),public,pointer::reverseRuleData=>defaultreverseSymmetryRule


	abstract INTERFACE
	  SUBROUTINE reverseRuleRoutine(dimen)
	  	import :: Symdimension
		Type(Symdimension),intent(inout)::dimen
	  END SUBROUTINE reverseRuleRoutine
	END INTERFACE
	procedure(reverseRuleRoutine),public,pointer::reverseRule=>defaultReverseRule

	abstract interface
		subroutine NonZeroElementExternal(Res,dimen)
			import :: Tensor
	  		import :: Symdimension
	  		type(Tensor),intent(inout)::Res
			type(SymDimension),intent(in)::dimen
		end subroutine NonZeroElementExternal
	end interface
	procedure(NonZeroElementExternal),public,pointer::NonZeroElement=>defaultNonZeroElement

	abstract interface
		subroutine RuleExternal(Res,dimen,indices,length)
	  		import :: Symdimension
	  		logical,intent(inout)::Res
			type(SymDimension),intent(in)::dimen
			integer,intent(in)::length
			integer,intent(in)::indices(length)
		end subroutine RuleExternal
	end interface
	procedure(RuleExternal),public,pointer::Rule=>defaultRule


	abstract interface
		subroutine fuseOrderExternal(Res,order,Q1,Q2,newRule_)
			import :: Tensor
	  		import :: QuanNum
	  		type(QuanNum),intent(inout)::Res
			type(QuanNum),intent(in)::Q1,Q2
			type(Tensor),intent(inout)::order
			integer,intent(in)::newRule_
		end subroutine fuseOrderExternal
	end interface
	procedure(fuseOrderExternal),public,pointer::fuseOrder=>defaultfuseOrder

	abstract interface
		subroutine ifParityExternal(Res,dimen,vec,ith,jth,rank)
			import :: Symdimension
			type(SymDimension),intent(in)::dimen
			logical,intent(inout)::Res
			integer,intent(in)::rank
			integer,intent(in)::vec(rank)
			integer,intent(in)::ith,jth
		end subroutine ifParityExternal
	end interface
	procedure(ifParityExternal),public,pointer::ifParity=>defaultifParity

	abstract interface
		 subroutine QaunNumParityExternal(Res,dimen,ith,jth)
			import :: Symdimension
			integer,intent(inout)::Res
			type(SymDimension),intent(in)::dimen
			integer,intent(in)::ith,jth
		end subroutine QaunNumParityExternal
	end interface
	procedure(QaunNumParityExternal),public,pointer::QaunNumParity=>defaultQaunNumParity
	
	character(len=20),private::Symmetry_type='null'

	public::SymTensor
	type,extends (SymDimension) :: SymTensor
		integer,private :: totalblock=0!The number of the total partitioned Tensors,that is the non-zero block
		integer,private :: totalData=0!size(block)
		type(Tensor),private,allocatable::block(:)!the partitioned Tensor
		integer,private :: rank=0!length of Dimension. SymDimension%GetRank()
		logical,private :: flag=.false. !if flag=true ,it means there are data in SymTensor
		integer,private::classType=default_classtype_in_Tensor ! Should be the same type of block
						           !if classType=1, integer
						           !if classType=2, real(4)
						           !if classType=3, real(8)
						           !if classType=4, complex(4)
						           !if classType=5, complex(8)
						           !if classType=6, logical
						           !if classType=7, character
		logical,private::DynamicClass=default_DynamicClass_in_Tensor! Should be the same type of block
	contains
		generic,public::getTotalData =>getTotalData0,getTotalData_vec,getTotalData_val
		procedure,public::getTotalblock
		procedure,public::getRank => DimSize !overwrite the function of type(Dimension)
		generic,public::getFlag =>getFlag0,getFlag_vec,getFlag_val
		procedure,public::getclassType
		procedure,public::getType
		procedure,public::Dynamic =>setDynamic
		procedure,public::Static=>setStatic
		procedure,public::ifDynamic
		procedure,public::SymmetryCheck
		procedure,public::ifSymmetry
		generic,public::resetDim =>resetDimension2,resetDimension3
		generic,public::reset_dim_no_check =>reset_dim_no_check1,reset_dim_no_check2
		procedure::reset_dim_no_check1,reset_dim_no_check2
		procedure,public::deallocate=>cleanDimension!overwrite the function of type(Dimension)
		procedure,public::deallocateBlock=>cleanBlock!Only deallocate the Tensor of block
		procedure,public::empty => emptyDimension!overwrite the function of type(Dimension)
		generic,public::setType =>setclassType,setclassType2!Set the data of Tensor and set it as a Static Tensor
		procedure,public::RNDim=>RNDimRoutine
		!call T1%allocate(T2), T1 will have the same dimension of T2 and will allocate the Block in T1 as that in T2
		generic,public::allocate => allocateTensor1,allocateTensor2,allocateTensor3,&
												allocateTensor4,allocateTensor5,allocateTensor6,&
												allocateTensor7,allocateTensor9,&
												allocateTensor13,allocateTensor14,allocateTensor15


		generic,public::random=>set_random_Tensor_val,set_random_Tensor_vec,set_random_Tensor_all	
		generic,public::SymRandom=>set_random_SymTensor_all,set_random_SymTensor_val,set_random_SymTensor_vec
		procedure::set_random_SymTensor_all,set_random_SymTensor_val,set_random_SymTensor_vec


		generic,public::allocateBlock=>allocateBlock1,allocateBlockAll,allocateBlock2,allocateBlock3
		generic,public::allocateSymBlock=>allocateSymBlock1,allocateSymBlock2,allocateSymBlock3,allocatefSymBlockAll
		procedure::allocateSymBlock1,allocateSymBlock2,allocateSymBlock3,allocatefSymBlockAll
		
		generic,public::i=>out_element1,out_element_vec,out_element_QN
		procedure::Sym_element_vec,Sym_element_vec2,Sym_element_vec3
		generic,public::symi=>Sym_element_vec,Sym_element_vec2,Sym_element_vec3

		generic,public::setValue=>set_element_vec,set_element_val
		generic,public::setSymValue=>set_Symelement_vec,set_Symelement_val
		procedure::set_Symelement_vec,set_Symelement_val
		procedure,public::zero=>set_zero_Tensor
		procedure,public::emptyBlock



		generic,public::SVD=>SVDRoutine,SVDRoutineNum,SVDRoutineNumMaxValue,SVDRoutineMaxValue,&
					SVDNameRoutine,SVDNameNumRoutine,SVDNameNumMaxValueRoutine,SVDNameMaxValueRoutine&
					,SVDNameNumRoutineLeft,SVDNameRoutineLeft,SVDNameNumMaxValueRoutineLeft&
					,SVDNameRoutineLeft2,SVDNameMaxValueRoutineLeft
		procedure::SVDRoutine,SVDRoutineNum,SVDRoutineNumMaxValue,SVDRoutineMaxValue,&
					SVDNameRoutine,SVDNameNumRoutine,SVDNameNumMaxValueRoutine,SVDNameMaxValueRoutine&
					,SVDNameNumRoutineLeft,SVDNameRoutineLeft,SVDNameNumMaxValueRoutineLeft&
					,SVDNameRoutineLeft2,SVDNameMaxValueRoutineLeft

		generic,public::SVDKill=>SVDRoutine_kill_inData,SVDRoutineNum_kill_inData,SVDRoutineNumMaxValue_kill_inData,&
					SVDRoutineMaxValue_kill_inData,SVDNameRoutine_kill_inData,SVDNameNumRoutine_kill_inData,&
					SVDNameNumMaxValueRoutine_kill_inData,SVDNameMaxValueRoutine_kill_inData&
					,SVDNameNumRoutineLeft_kill_inData,SVDNameRoutineLeft_kill_inData,SVDNameNumMaxValueRoutineLeft_kill_inData&
					,SVDNameRoutineLeft2_kill_inData,SVDNameMaxValueRoutineLeft_kill_inData
		procedure::SVDRoutine_kill_inData,SVDRoutineNum_kill_inData,SVDRoutineNumMaxValue_kill_inData,&
					SVDRoutineMaxValue_kill_inData,SVDNameRoutine_kill_inData,SVDNameNumRoutine_kill_inData,&
					SVDNameNumMaxValueRoutine_kill_inData,SVDNameMaxValueRoutine_kill_inData&
					,SVDNameNumRoutineLeft_kill_inData,SVDNameRoutineLeft_kill_inData,SVDNameNumMaxValueRoutineLeft_kill_inData&
					,SVDNameRoutineLeft2_kill_inData,SVDNameMaxValueRoutineLeft_kill_inData
					



		procedure,public::eigSymTensor
		procedure,public::print=>Dprint
		generic,public::writeinfo=>Tprint_file1,Tprint_file2
		generic,public::write=>Tprint_file1,Tprint_file2
		procedure,public::read=>readdimension
		generic,public::printBlock=>PrintBlock1,PrintBlock2,PrintBlock3
		generic,public::diminfo=>DimPrint1,DimPrint2,DimPrint3
		generic,public::Blockdiminfo=>DimBlockPrint1,DimBlockPrint2,DimBlockPrint3



		procedure,public::fuse=>fuseDimension!overwrite
		procedure,public::split=>splitDimension!overwrite

		generic,public::QuanFuse=>QuantumFuseFast,QuantumFuseDimFast
		generic,public::QuanSplit=>QuantumSplitFast,QuantumSplitDimFast
		generic,public::SymSplit=>FermiSplitSymTensor2,FermiSplitSymTensor
		generic,public::SymFuse=>SymFuseTensor,SymFuseTensor2,SymFuseTensor3,SymFuseTensor4&
								,SymFuseTensorRule,SymFuseTensorRule2,SymFuseTensorRule3,SymFuseTensorRule4
		procedure::FermiSplitSymTensor2,FermiSplitSymTensor
		procedure::SymFuseTensor,SymFuseTensor2,SymFuseTensor3,SymFuseTensor4&
					,SymFuseTensorRule,SymFuseTensorRule2,SymFuseTensorRule3,SymFuseTensorRule4
									

		
		generic,public::permute=>permutation_routine,permutation_Name_routine,permutation_Tensor_routine
		
		generic,public::forward=>permutefo_routine,permutefo_name_routine,permutefo_vec_routine,permutefo_vec_Name_routine,&
										permutefo_Tensor_routine
		generic,public::backward=>permuteback_routine,permuteback_name_routine,permuteback_vec_routine,permuteback_vec_name_routine,&
											permuteback_Tensor_routine


		generic,public::external=>operation_on_SymTensor1,operation_on_SymTensor2,operation_on_SymTensor3
		generic,public::pointer=>Tpointer,Tpointer2,Tpointer3,Tpointer_,Tpointer2_
		procedure::Tpointer,Tpointer2,Tpointer3,Tpointer_,Tpointer2_
		procedure,public::point2TotalBlock,point2TotalData



		generic,public::QR=>QRdecomposition,QRRoutineNameLeft,QRRoutineName
		procedure::QRdecomposition,QRRoutineNameLeft,QRRoutineName
		generic,public::LQ=>LQdecomposition,LQRoutineNameLeft,LQRoutineName
		procedure::LQdecomposition,LQRoutineNameLeft,LQRoutineName

		generic,public::QRkill=>QRdecomposition_kill_inData,QRRoutineNameLeft_kill_inData,QRRoutineName_kill_inData
		procedure::QRdecomposition_kill_inData,QRRoutineNameLeft_kill_inData,QRRoutineName_kill_inData
		generic,public::LQKill=>LQdecomposition_kill_inData,LQRoutineNameLeft_kill_inData,LQRoutineName_kill_inData
		procedure::LQdecomposition_kill_inData,LQRoutineNameLeft_kill_inData,LQRoutineName_kill_inData



		generic,public::eye=>eye_Tensor2,eye_Tensor6
		procedure,public::isZero=>isZeroSymTensor
		procedure,public::SymCheck=>checkSymTensorData
		generic,public::max=>TmaxElement,TmaxminElement
		generic,public::dmax=>dmaxElement,dmaxminElement
		procedure,public::trace=>traceSymTensor
		procedure,public::strace=>straceSymTensor
		procedure,public::dtrace=>dtraceSymTensor
		procedure,public::ctrace=>ctraceSymTensor
		procedure,public::ztrace=>ztraceSymTensor
		procedure,public::checkClassType
		!	subSymTensor(inde):
		!		inde=[-1,inde_min,inde_max] output data(inde_min:inde_max,:)
		!	or[-2,inde_min,inde_max],data(:,inde_min:inde_max)
		!	or[-3,inde_min,inde_max],data(inde_min:inde_max)
		!	or [-1,inde_row] [-2,inde_col],output row or col
		!	or [inde1_min,inde1_max,inde2_min,inde2_max] output data(inde1_min:inde1_max,inde2_min:inde2_max)
		! The dimension of the output SymTensor is the same as input T
		!	subSymTensor(indexi,degi,row) or subSymTensor(indexi,degi,row,keepQN)
		!  subSymTensor(indexi,degi,row) 

		!       row='r'
		!              The SubSymTensor of the SymT(indexi,:)
		!                  only output the degenercy of degi
		!                  delete the first quantum number
		!       row='c'
		!              The SubSymTensor of the SymT(:,indexi)
		!                  only output the degenercy of degi
		!                  delete the last quantum number
		!  if input keepQN=.true.
		!       do not delete the quantum number, the degenercy of the quantum number is
		!                 [0,0,0,1,0,0,] ---> the degi element is 1, others  are 0
		!

		generic,public::subSymTensor=>subSymTensor1,subSymTensor2,subSymTensor3

		generic,public::isnan=>isNaNSymTensor1,isNaNSymTensor2,isNaNSymTensor3
		procedure,public::norm=>normSymTensor
		procedure,public::inorm=>inormSymTensor
		procedure,public::snorm=>snormSymTensor
		procedure,public::dnorm=>dnormSymTensor
		procedure,public::cnorm=>cnormSymTensor
		procedure,public::znorm=>znormSymTensor
		procedure,public::norm2=>norm2SymTensor
		procedure,public::inorm2=>inorm2SymTensor
		procedure,public::snorm2=>snorm2SymTensor
		procedure,public::dnorm2=>dnorm2SymTensor
		procedure,public::cnorm2=>cnorm2SymTensor
		procedure,public::znorm2=>znorm2SymTensor
		procedure,public::ProductTensorRoutine

		generic,public::contract=>contract_name_routine,contract_name_routine1,contract_name_routine2,&
											contract_name_routine4,contract_name_routine5,contract_name_routine6,&
											contract_name_ownlegs_routine,contract_ownlegs_routine
		procedure::contract_name_routine,contract_name_routine1,contract_name_routine2
		procedure::contract_name_routine4,contract_name_routine5,contract_name_routine6
		procedure::contract_name_ownlegs_routine,contract_ownlegs_routine
		
		
		procedure,public::sum=>TsumSymTensor
		procedure,public::isum=>isumSymTensor
		procedure,public::ssum=>ssumSymTensor
		procedure,public::dsum=>dsumSymTensor
		procedure,public::csum=>csumSymTensor
		procedure,public::zsum=>zsumSymTensor
		procedure,public::ZeroBlockindex
		procedure,public::emptyZeroBlock
		procedure,public::trimSymDim
		procedure,public::trimDim
		procedure,public::killZeroDeg=>killZeroDegDim


		procedure::isNaNSymTensor1
		procedure::isNaNSymTensor2
		procedure::isNaNSymTensor3
		
		procedure::getTotalData0
		procedure::getTotalData_vec
		procedure::getTotalData_val
		procedure::getFlag0
		procedure::getFlag_vec
		procedure::getFlag_val
		procedure::resetDimension
		procedure::resetDimension2
		procedure::resetDimension3
		procedure::setclassType
		procedure::setclassType2
		procedure::allocateTensor1
		procedure::allocateTensor2
		procedure::allocateTensor3
		procedure::allocateTensor4
		procedure::allocateTensor5
		procedure::allocateTensor6
		procedure::allocateTensor7
		procedure::allocateTensor9
		procedure::allocateTensor13
		procedure::allocateTensor14
		procedure::allocateTensor15		
		procedure::set_random_Tensor_val
		procedure::set_random_Tensor_vec
		procedure::set_random_Tensor_all		
		procedure::allocateBlock1
		procedure::allocateBlockAll
		procedure::allocateBlock2
		procedure::allocateBlock3
		procedure::out_element1
		procedure::out_element_vec
		procedure::out_element_QN
		procedure::set_element_vec
		procedure::set_element_val!As the dimension of the block is fix by the degeneracy of the symDimension
		                                           !This subroutine will ignore the input Tensor dimension
		procedure::Tprint_file1
		procedure::Tprint_file2
		procedure::PrintBlock1
		procedure::PrintBlock2
		procedure::PrintBlock3
		procedure::DimPrint1
		procedure::DimPrint2
		procedure::DimPrint3
		procedure::DimBlockPrint1
		procedure::DimBlockPrint2
		procedure::DimBlockPrint3
		procedure::QuantumFusefast
		procedure::QuantumFuseDimfast
		procedure::QuantumSplitfast
		procedure::QuantumSplitDimfast
		procedure::permutation_routine
		procedure::permutation_Name_routine
		procedure::permutation_Tensor_routine
		procedure::permutefo_routine
		procedure::permutefo_name_routine
		procedure::permutefo_vec_routine
		procedure::permutefo_vec_Name_routine
		procedure::permutefo_Tensor_routine
		procedure::permuteback_routine
		procedure::permuteback_name_routine
		procedure::permuteback_vec_routine
		procedure::permuteback_vec_name_routine
		procedure::permuteback_Tensor_routine
		procedure::operation_on_SymTensor1
		procedure::operation_on_SymTensor2
		procedure::operation_on_SymTensor3
		procedure::eye_Tensor2
		procedure::eye_Tensor6
		procedure::dmaxElement
		procedure::dmaxminElement
		procedure::TmaxElement
		procedure::TmaxminElement
		procedure::subSymTensor1
		procedure::subSymTensor2
		procedure::subSymTensor3
	end type SymTensor


	interface check_Contact
		module procedure  check_Contact_routine1
		module procedure  check_Contact_routine2
		module procedure  check_Contact_routine3
		module procedure  check_Contact_routine4
		module procedure  check_Contact_routine5
		module procedure  check_Contact_routine6	
	end interface
	



	public::dmax
	interface dmax
		module procedure  dmaxElement
		module procedure  dmaxminElement
	end interface
	
	public::max
	interface max
		module procedure  TmaxElement
		module procedure  TmaxminElement
	end interface
	
	public::expm
	interface expm
		module procedure  expmSymTensor
	end interface
	
	public::operator(/)
	interface operator(/)
		module procedure divide_int
		module procedure divide_real8
		module procedure divide_real4
		module procedure divide_Tensor
	end interface
	
	
	public::operator(+)
	interface operator(+)
		module procedure add_SymTensor
	end interface
	
	public::operator(-)
	interface operator(-)
		module procedure minu_SymTensor
	end interface
	
	public::operator(*)
	interface operator(*)
		module procedure ProductTensor
		module procedure multiply_number_real8
		module procedure multiply_number_com4
		module procedure multiply_number_com8
		module procedure multiply_number_real4
		module procedure multiply_number_real8_
		module procedure multiply_number_com4_
		module procedure multiply_number_com8_
		module procedure multiply_number_real4_
	end interface
	
	public::operator(.con.)
	interface operator(.con.)
		module procedure conjugate! conjugate
	end interface
	public::operator(.H.)
	interface operator(.H.)
		module procedure Htranspose! conjugateTranspose! one or two dimension case
											! If input rank=1,the result is the same as conjugate
	end interface
	
	public::operator(.Hn.)
	interface operator(.Hn.)
		module procedure Htranspose2!
	end interface
	
	
	public::operator(.p.)
	interface operator(.p.)
		module procedure permute_rank2 !permute the tensor whose rank is 2,if rank=1 ,do nothing
		module procedure permute_rank3 !permute the tensor whose rank is 3
		module procedure permutation	 !permute the tensor of any rank,give the new order of the dimension
												 !		If operate on a big Tensor,use permute_rank2 or permute_rank3,
												 !	 they are faster.if rank>3,use contract to reshape
		module procedure permutation_name!input a character(:) as the new order
		module procedure permutation_Tensor
	end interface
	public::operator(.pf.)
	interface operator(.pf.)
		module procedure permutefo!permute the inde index to the first
										  !T_{1,2,3,..,i,..,n},permutefo(T,i)=_{i,1,2,3,..,i-1,i+1,..,n}
										  !T_{1,2,3,..,j,..,i,.,k,...,n},permutefo_vec(T,(/i,j,k/))=_{i,j,k,1,2,3,...,n}
		module procedure permutefo_vec
		module procedure permutefo_name
		module procedure permutefo_vec_name
		module procedure permutefo_Tensor
	end interface
	public::operator(.pb.)
	interface operator(.pb.)!	T_{1,2,3,..,i,..,n},permuteback(T,i)=_{1,2,3,..,i-1,i+1,..,n,i}
									!!T_{1,2,3,..,j,..,i,.,k,...,n},permuteback_vec(T,(/i,j,k/))=_{1,2,3,...,n,i,j,k}
		module procedure permuteback
		module procedure permuteback_vec
		module procedure permuteback_name
		module procedure permuteback_vec_name
		module procedure permuteback_Tensor
	end interface
	!	public::operator(.pi.)
	!	interface operator(.pi.)!T_{1,2,3,..,i,..,n},permuteInde(T,i)=_{2,3,..,i,1,i+1,..,n}
	!		module procedure permuteInde
	!		module procedure permuteInde_name
	!	end interface
	!	public::operator(.pbi.)
	!	interface operator(.pbi.)!T_{1,2,3,..,i,..,n},permutebackInde(T,i)=_{1,2,3,..,i-1,n,i,i+1,..,n-1}
	!		module procedure permutebackInde
	!		module procedure permutebackInde_name
	!	end interface	

	public::operator(.fuse.)
	interface operator(.fuse.)
		module procedure fuseTensor_val
		module procedure fuseTensor_vec
	end interface
	
	public::operator(.split.)
	interface operator(.split.)
		module procedure Tensorsplit2
		module procedure Tensorsplit3
		module procedure TensorsplitAll
	end interface


	public::contract
	interface contract
		module procedure contract_Same_name
		module procedure contract_noName
		module procedure contract_noName2
		module procedure contract_Name!In put dimension as character
		module procedure contract_Name2
		module procedure contract_name_int
		module procedure contract_name_int2
		module procedure contract_int_name
		module procedure contract_int_name2
		module procedure contract_name_ownlegs
		module procedure contract_ownlegs
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
	interface operator(.dot.)! dot product DO NOT conjugating the first vector,The Tensor will be regard as a vector
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
	
	public::operator(.subdim.)!overwrite the function in type(Dimension)
	interface operator(.subdim.)
		module procedure getSubDim2
		module procedure getSubDim3
		module procedure getSubDim4
		module procedure getSubDim2_name
	end interface
		
	public::assignment(=)
	interface assignment(=)
		module procedure assignmentTen
		module procedure assignmentreal4
		module procedure assignmentreal8
		module procedure assignmentcom4
		module procedure assignmentcom8
		module procedure SymTensorToTensor
		module procedure TensorToSymTensor
		module procedure assignmentSymTensorArray
	end interface
	
	public::MPI_BCAST_SymTensor,MPI_send_SymTensor
	public::MPI_SUM_SymTensor
	interface MPI_SUM_SymTensor!Do not check the input SymTensor
		module procedure MPI_SUM_SymTensor1
	end interface
	
	public::reverseSymmetryRule,set_symmetry

	public:: set_checkSymmetryRule_subroutine
	public:: set_reverseRuleData_subroutine
	public:: set_NonZeroElement_subroutine
	public:: set_Rule_subroutine
	public:: set_fuseOrder_subroutine
	public:: set_ifParity_subroutine
	public:: set_QaunNumParity_subroutine
	public:: out_Tensor_Symmetry_type

	!When split the Tensor, if the Tensor is 0, one should not store it
	!   / 0 \
	!   |---|   ---> split to two Tensor: 0  and B. Howevey one should not store the 0 (should ne updata)
	!   \ B /
	!Do not set the name to the block , so should modify the function and routine call by name
	! All operation of SymDimension should be overwrite, because one should do the same operaction on the block(:)


contains

	function out_Tensor_Symmetry_type()
		character(len=characterlen)::out_Tensor_Symmetry_type
		out_Tensor_Symmetry_type=Symmetry_type
	end function


	subroutine defaultcheckSymmetryRule(Rule1,Rule2,lenname1,lenname2)
		integer,intent(in)::Rule1,Rule2
		character(len=*),intent(in)::lenname1,lenname2
	end subroutine

	subroutine defaultreverseSymmetryRule(T,dimen,LD1,LD2)
		integer,intent(in)::LD1,LD2
		Type(Tensor),intent(inout)::T(LD1,LD2)
		Type(Symdimension),intent(inout)::dimen
		call error_mess()
	end subroutine

	subroutine defaultReverseRule(dimen)
		Type(Symdimension),intent(inout)::dimen
		return
	end subroutine
	

	subroutine defaultNonZeroElement(Res,dimen)
		type(Tensor),intent(inout)::Res
		type(SymDimension),intent(in)::dimen
		call error_mess()
	end subroutine
	 subroutine defaultRule(Res,dimen,indices,length)
	 	logical,intent(inout)::Res
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::length
		integer,intent(in)::indices(length)
		call error_mess()
	end subroutine

	subroutine defaultfuseOrder(Res,order,Q1,Q2,newRule_)
		type(QuanNum),intent(inout)::Res
		type(QuanNum),intent(in)::Q1,Q2
		type(Tensor),intent(inout)::order
		integer,intent(in)::newRule_
		call error_mess()
	end subroutine
	subroutine defaultifParity(Res,dimen,vec,ith,jth,rank)
		logical,intent(inout)::Res
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::rank
		integer,intent(in)::vec(rank)
		integer,intent(in)::ith,jth
		call error_mess()
	end subroutine

	subroutine defaultQaunNumParity(REs,dimen,ith,jth)
		integer,intent(inout)::Res
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::ith,jth
		call error_mess()
	end subroutine

	subroutine set_checkSymmetryRule_subroutine(func)
		procedure(checkSymmetryRuleExternal)::func
		checkSymmetryRule=>func
		call writemess('Set the checkSymmetryRule subroutine')
		Symmetry_type='Other'
	end subroutine

	subroutine set_reverseRuleData_subroutine(func)
		procedure(reverseRuleDataRoutine)::func
		reverseRuleData=>func
		call writemess('Set the reverseRuleData subroutine')
		Symmetry_type='Other'
	end subroutine

	subroutine set_NonZeroElement_subroutine(func)
		procedure(NonZeroElementExternal)::func
		NonZeroElement=>func
		call writemess('Set the NonZeroElement subroutine')
		Symmetry_type='Other'
	end subroutine

	subroutine set_Rule_subroutine(func)
		procedure(RuleExternal)::func
		Rule=>func
		call writemess('Set the Rule subroutine')
		Symmetry_type='Other'
	end subroutine
	subroutine set_reverseRule_subroutine(func)
		procedure(reverseRuleRoutine)::func
		reverseRule=>func
		call writemess('Set the reverseRule subroutine')
		Symmetry_type='Other'
	end subroutine

	subroutine set_fuseOrder_subroutine(func)
		procedure(fuseOrderExternal)::func
		fuseOrder=>func
		call writemess('Set the fuseOrder subroutine')
		Symmetry_type='Other'
	end subroutine

	subroutine set_ifParity_subroutine(func)
		procedure(ifParityExternal)::func
		ifParity=>func
		call writemess('Set the ifParity subroutine')
		Symmetry_type='Other'
	end subroutine

	subroutine set_QaunNumParity_subroutine(func)
		procedure(QaunNumParityExternal)::func
		QaunNumParity=>func
		call writemess('Set the QaunNumParity subroutine')
		Symmetry_type='Other'
	end subroutine




	subroutine error_mess()
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call writemess('@@@                 ERROR                   @@@  ')
		call writemess('@@@ You have not set the symmetry group yet @@@  ')
		call writemess('@@@                                         @@@  ')
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call writemess('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ')
		call error_stop
	end subroutine
	subroutine set_symmetry(group)
		character(len=*),intent(in)::group
		if(Symmetry_type.nequ.'null')then
			call writemess(' The package has a symmetry group of ')
			call writemess(' Symmetry_type='+Symmetry_type)
			call writemess(' Can not reset the Symmetry group')
			call error_stop
		end if
		call writemess('   ')
		call writemess(' Set the symmetry group as '+(' '+group))
		call writemess('   ')
		if(group.equ.'U(1)')then
			checkSymmetryRule=>checkU1Rule
			reverseRuleData=>reverseU1Rule
			NonZeroElement=>U1NonZeroElement
			Rule=>U1rule4
			fuseOrder=>U1fuseOrder
			ifParity=>U1ifParity4
			QaunNumParity=>U1Parity1
			reverseRule=>reverseU1RuleRoutine
			Symmetry_type='U(1)'
		else if(group.equ.'Parity')then
			checkSymmetryRule=>checkParityRule
			reverseRuleData=>reverseParityRule
			NonZeroElement=>ParityNonZeroElement
			Rule=>PRule4
			fuseOrder=>ParityfuseOrder
			ifParity=>ifParityIndex4
			QaunNumParity=>pParity1
			Symmetry_type='Parity'
		else
			call writemess(' Only support the symmetry group of U(1) or Parity')
			call writemess(' However you can setup your onw symmetry group by:')
			call writemess(' call set_checkSymmetryRule_subroutine(externalFunction)')
			call writemess(' call set_reverseRuleData_subroutine(externalFunction)')
			call writemess(' call set_NonZeroElement_subroutine(externalFunction)')
			call writemess(' call set_Rule_subroutine(externalFunction)')
			call writemess(' call set_fuseOrder_subroutine(externalFunction)')
			call writemess(' call set_ifParity_subroutine(externalFunction)')
			call writemess(' call set_QaunNumParity_subroutine(externalFunction)')
			call error_stop
		end if
		return
	end subroutine


	subroutine reverseSymmetryRule(T)
		type(SymTensor),intent(inout)::T
		call reverseRuleData(T%Block,T%SymDimension,T%dim(1),T%dim(2))
		return
	end subroutine
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


	integer function getTotalData0(ST)
		class(SymTensor),intent(in)::ST
		getTotalData0=ST%totalData
		return
	end function

	integer function getTotalData_val(ST,ith)
		class(SymTensor),intent(in)::ST
		integer,intent(in)::ith
		getTotalData_val=ST%block(ith)%getTotalData()
		return
	end function

	integer function getTotalData_vec(ST,indices)
		class(SymTensor),intent(in)::ST
		integer,intent(in)::indices(:)
		integer::inde
		inde=addressToIndes(ST%dim(),indices)
		getTotalData_vec=ST%block(inde)%getTotalData()
		return
	end function
	
	integer function getTotalblock(ST)
		class(SymTensor),intent(in)::ST
		getTotalblock=ST%totalblock
		return
	end function
	
	integer	function DimSize(Dimen)
		class(SymTensor),intent(in) :: Dimen
		DimSize=Dimen%rank
		return
	end function
	
	logical function getFlag0(ST)
		class(SymTensor),intent(in)::ST
		getFlag0=ST%flag
		return
	end function

	logical function getFlag_val(ST,ith)
		class(SymTensor),intent(in)::ST
		integer,intent(in)::ith
		getFlag_val=ST%block(ith)%getFlag()
		return
	end function

	logical function getFlag_vec(ST,indices)
		class(SymTensor),intent(in)::ST
		integer,intent(in)::indices(:)
		integer::inde
		inde=addressToIndes(ST%dim(),indices)
		getFlag_vec=ST%block(inde)%getFlag()
		return
	end function
	
	character(len=20) function getclassType(T)
		class(SymTensor),intent(in) :: T
		getclassType=out_data_class_type(T%classType)
		return
	end function

	character(len=20) function out_data_class_type(classType)result(classtypechar)
		integer,intent(in) ::classType
		select case(classType)
			case (1)
				classtypechar='integer'
			case (2)
				classtypechar='real(kind=4)'
			case (3)
				classtypechar='real(kind=8)'
			case (4)
				classtypechar='complex(kind=4)'
			case (5)
				classtypechar='complex(kind=8)'
			case (6)
				classtypechar='logical'
			case (7)
				classtypechar='character'
			case default 
				write(*,*)"no such case"
				stop
		end 	select
		return
	end function

	integer function select_data_type_char(indata)result(select_data_type)
		character(len=*),intent(in) ::indata
		if(indata.equ.'integer') then
			select_data_type=1
			return
		end if
		if((indata.equ.'real*4').or.(indata.equ.'real(kind=4)').or.(indata.equ.'real')) then
			select_data_type=2
			return
		end if
		if((indata.equ.'real*8').or.(indata.equ.'real(kind=8)').or.(indata.equ.'double')) then
			select_data_type=3
			return
		end if
		if((indata.equ.'complex*8').or.(indata.equ.'complex(kind=4)').or.(indata.equ.'complex')) then
			select_data_type=4
			return
		end if
		if((indata.equ.'complex*16').or.(indata.equ.'complex(kind=8)')) then
			select_data_type=5
			return
		end if
		if(indata.equ.'logical') then
			select_data_type=6
			return
		end if
		if(indata.equ.'character') then
			select_data_type=7
			return
		end if
		write(*,*)"ERROR type"
		call error_stop()
		return
	end function
	
	integer function getType(T)
		class(SymTensor),intent(in) :: T
		getType=T%classType
	end function	
	
	!******************  setDynamic  ***********

	subroutine setDynamic(T)
		class(SymTensor),intent(inout) :: T
		integer::i
		integer::classtype
		T%DynamicClass=.true.
		classtype=T%getType()
		if(T%getFlag())then
			do i=1,T%getTotalData()
				call T%block(i)%setType(classtype)
				call T%block(i)%Dynamic()
			end do
		end if
		return
	end subroutine

	!******************  setStatic  ***********

	subroutine setStatic(T)
		class(SymTensor),intent(inout) :: T
		integer::i,classtype
		T%DynamicClass=.false.
		classtype=T%getType()
		if(T%getFlag())then
			do i=1,T%getTotalData()
				call T%block(i)%setType(classtype)
				call T%block(i)%Static()
			end do
		end if
		return
	end subroutine

	!*******************  ifDynamic   *********

	logical function ifDynamic(T)
		class(SymTensor),intent(in) :: T
		ifDynamic=T%DynamicClass
		return
	end function

	logical function ifSymmetry(T)
		class(SymTensor),intent(in)::T
		integer::i,rank
		integer,allocatable::Tdim(:),Maxdim(:)
		rank=T%GetRank()
		allocate(TDim(rank))
		allocate(Maxdim(rank))
		Maxdim=T%dim()
		ifSymmetry=.true.
		do i=1,T%getTotalData()
			if(T%getFlag(i))then
				call IndesToaddress(Maxdim,Tdim,i)	
				call rule(ifSymmetry,T%SymDimension,TDim,rank)
				if(.not.ifSymmetry)return
			end if
		end do
	end function

	subroutine SymmetryCheck(T)
		class(SymTensor),intent(in)::T
		integer::i,rank
		logical::checkFlag
		integer,allocatable::Tdim(:),Maxdim(:)
		rank=T%GetRank()
		allocate(TDim(rank))
		allocate(Maxdim(rank))
		Maxdim=T%dim()
		checkFlag=.true.
		do i=1,T%getTotalData()
			if(T%getFlag(i))then
				call IndesToaddress(Maxdim,Tdim,i)	
				call rule(checkFlag,T%SymDimension,TDim,rank)
				if(.not.checkFlag)then
					call writemess(' The Tensor is not a Symmetry Tensor')
					call error_stop
				end if
			end if
		end do
	end subroutine

	type(Tensor) function ZeroBlockindex(T)
		class(SymTensor),intent(in)::T
		integer::i
		integer,allocatable::Tdim(:),vec(:)
		allocate(Tdim(T%getRank()))
		allocate(vec(T%getRank()))
		call ZeroBlockindex%empty()
		TDim=T%dim()
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				if(T%Block(i)%iszero())then
					call IndesToaddress(TDim,vec,i)
					if(ZeroBlockindex%getFlag())then
						call ZeroBlockindex%addrow( Tensor(vec) )
					else
						ZeroBlockindex=vec
					end if
				end if
			end if
		end do
		return
	end function

	subroutine point2TotalBlock(T,ip)
		class(SymTensor),target,intent(in)::T
		integer,pointer::ip
		ip=>T%TotalBlock
		return
	end subroutine
	subroutine point2TotalData(T,ip)
		class(SymTensor),target,intent(in)::T
		integer,pointer::ip
		ip=>T%TotalData
		return
	end subroutine

	subroutine checkClassType(T)
		class(SymTensor),intent(inout)::T
		integer::i,Ttype
		Ttype=T%getType()
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				if(T%Block(i)%getType().ne.Ttype)then
					call writemess(' ERROR in the class type in SymTensor with its block',-1)
					call error_stop
				end if
			end if
		end do
		return
	end subroutine

	subroutine emptyZeroBlock(T)
		class(SymTensor),intent(inout)::T
		integer::i
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				if(T%Block(i)%iszero())then
					call T%Block(i)%deallocate()
					T%totalBlock=T%totalBlock-1
				end if
			end if
		end do
		if(T%totalBlock.lt.0)then
			call writemess(' ERROR in emptyZeroBlock',-1)
			call error_stop
		end if
		return
	end subroutine


	!
	!Check
	!The degeneracies and the dimension of the block should be the same
	! TensorName should not be set in the block

	subroutine checkSymTensorData(T)
		class(SymTensor),intent(in)::T
		integer::i,rank
		integer,allocatable::deg(:),dimen(:),Tdimen(:),deg2(:)
		logical::flag
		if(.not.T%getFlag())return
		allocate(deg(T%getRank()))
		allocate(dimen(T%getRank()))
		allocate(Tdimen(T%getRank()))
		allocate(deg2(T%getRank()))
		Tdimen=T%dim()
		rank=T%getRank()
		do i=1,T%getTotalData()
			if(T%block(i)%getFlag())then
				call IndesToaddress(Tdimen,dimen,i)
				deg=T%degDim(dimen)
				deg2=T%block(i)%dim()
				if(.not.(deg.equ.deg2))then
					call writemess('The degeneracies conflit with dimension of the block')
					call error_stop()
				end if
				if(T%block(i)%outNAmeFlag().ne.0)then
					call writemess('There is TensorName in the dimension of the block')
					call error_stop()
				end if
				call Rule(flag,T%SymDimension,dimen,rank)
				if(.not.flag)then
					call writemess('ERROR Symmetry Rule, please check the data in the SymDimension')
					call error_stop
				end if
			end if
		end do
		
		return
	end subroutine


	logical function isNaNSymTensor1(T)result(Res)
		class(SymTensor),intent(in)::T
		integer::i
		Res=.false.
		do i=1,T%getTotalData()
			if(T%block(i)%getFlag())then
				Res=T%block(i)%isnan()
				if(Res)return
			end if
		end do
		return
	end function
	logical function isNaNSymTensor2(T,ith)result(Res)
		class(SymTensor),intent(in)::T
		integer,intent(in)::ith
		Res=T%block(ith)%isnan()
		return
	end function
	logical function isNaNSymTensor3(T,vec)result(Res)
		class(SymTensor),intent(in)::T
		integer,intent(in)::vec(:)
		type(Tensor)::temp
		temp=T%i(vec)
		Res=temp%isnan()
		return
	end function



	type(SymTensor) function trimSymDim(A,ith)
		class(SymTensor),intent(inout)::A
		integer,intent(in)::ith
		type(QuanNum)::Q1,Q2
		integer::m(2),n(2),i,j
		type(Tensor),pointer::Ap(:,:)
		logical::goon
		if(A%getRank().ne.2)then
			call writemess('trimSymDim only work on matrix (rank=2)')
			call error_stop
		end if
		if(.not.A%ifZeroDeg())then
			trimSymDim=A
			return
		end if
		if(ith.eq.1)then
			m=Q1%trimZeroDegIndex()
			n=[1,Q2%getQNlength()]
			call Q1%TrimZeroDegQN()
		else if(ith.eq.2)then
			m=[1,Q1%getQNlength()]
			n=Q2%trimZeroDegIndex()
			call Q2%TrimZeroDegQN()
		else  if(ith.eq.-1)then
			m=Q1%trimZeroDegIndex()
			n=Q2%trimZeroDegIndex()
			call Q1%TrimZeroDegQN()
			call Q2%TrimZeroDegQN()
		else
			call writemess('ERROR in trimDim')
			call error_stop
		end if
		call trimSymDim%allocate([Q1,Q2],A%getType())
		call A%pointer(Ap,m,n)
		do i=1,trimSymDim%dim(1)
			do j=1,trimSymDim%dim(2)
				if(Ap(i,j)%getFlag())then
					call trimSymDim%setValue([i,j],Ap(i,j))
				end if
			end do
		end do
		Ap=>null()
		return
	end function

	subroutine trimDim(A,ith)
		class(SymTensor),target,intent(inout)::A
		integer,intent(in)::ith
		type(QuanNum)::Q1,Q2
		integer::m(2),n(2),i,j,Adim(2),classtype
		type(Tensor),pointer::Ap(:,:)
		type(Tensor),target,allocatable::AData(:,:)
		logical::goon
		if(A%getRank().ne.2)then
			call writemess('trimDim only work on matrix (rank=2)')
			call error_stop
		end if
		if(.not.A%ifZeroDeg())return
		Q1=A%quantumnumber(1)
		Q2=A%quantumnumber(2)
		if(ith.eq.1)then
			m=Q1%trimZeroDegIndex()
			n=[1,Q2%getQNlength()]
			call Q1%TrimZeroDegQN()
		else if(ith.eq.2)then
			m=[1,Q1%getQNlength()]
			n=Q2%trimZeroDegIndex()
			call Q2%TrimZeroDegQN()
		else  if(ith.eq.-1)then
			m=Q1%trimZeroDegIndex()
			n=Q2%trimZeroDegIndex()
			call Q1%TrimZeroDegQN()
			call Q2%TrimZeroDegQN()
		else
			call writemess('ERROR in trimDim')
			call error_stop
		end if
		Adim(1)=m(2)-m(1)+1
		Adim(2)=n(2)-n(1)+1
		allocate(AData(Adim(1),Adim(2)))
		call A%pointer(Ap,m,n)
		do j=1,Adim(2)
			do i=1,Adim(1)
				AData(i,j)=Ap(i,j)
			end do
		end do
		Ap=>null()
		classtype=A%getType()
		call A%empty()
		call A%allocate([Q1,Q2],classtype)
		do i=1,A%dim(1)
			do j=1,A%dim(2)
				if(AData(i,j)%getFlag())then
					call A%setValue([i,j],AData(i,j))
				end if
			end do
		end do
		
		return
	end subroutine

	subroutine killZeroDegDim(A,ith)
		class(SymTensor),target,intent(inout)::A
		integer,intent(in)::ith
		type(QuanNum)::Q1,Q2
		integer::i,j,Adim(2),classtype
		type(Tensor)::m,n
		type(Tensor),pointer::Ap(:,:)
		type(Tensor),target,allocatable::AData(:,:)
		logical::goon
		if(A%getRank().ne.2)then
			call writemess('trimDim only work on matrix (rank=2)')
			call error_stop
		end if
		if(.not.A%ifZeroDeg())return
		Q1=A%quantumnumber(1)
		Q2=A%quantumnumber(2)
		if(ith.eq.1)then
			m=Q1%NonZeroQNindex()
			call n%allocate([Q2%getQNlength()],'integer')
			do i=1,Q2%getQNlength()
				call n%setValue(i,i)
			end do
			call Q1%killZeroDeg()
		else if(ith.eq.2)then
			call m%allocate([Q1%getQNlength()],'integer')
			do i=1,Q1%getQNlength()
				call m%setValue(i,i)
			end do
			n=Q2%NonZeroQNindex()
			call Q2%killZeroDeg()
		else  if(ith.eq.-1)then
			m=Q1%NonZeroQNindex()
			n=Q2%NonZeroQNindex()
			call Q1%killZeroDeg()
			call Q2%killZeroDeg()
		else
			call writemess('ERROR in trimDim')
			call error_stop
		end if
		Adim(1)=m%getTotalData()
		Adim(2)=n%getTotalData()
		allocate(AData(Adim(1),Adim(2)))
		call A%pointer(Ap)
		do j=1,Adim(2)
			do i=1,Adim(1)
				AData(i,j)=Ap(m%ii(i),n%ii(j))
			end do
		end do
		Ap=>null()
		classtype=A%getType()
		call A%empty()
		call A%allocate([Q1,Q2],classtype)
		do i=1,A%dim(1)
			do j=1,A%dim(2)
				if(AData(i,j)%getFlag())then
					call A%setValue([i,j],AData(i,j))
				end if
			end do
		end do
		
		return
	end subroutine

	!*****************  deallocate ****************

	subroutine cleanDimension(Dimen)!overwrite the subroutine in Symdimension.f90
		class(SymTensor),intent(inout) ::Dimen
		integer::i
		call Dimen%SymDimension%deallocate()
		Dimen%Flag=.false.
		Dimen%DynamicClass=default_DynamicClass_in_Tensor	
		Dimen%classType=default_classtype_in_Tensor	
		Dimen%rank=0
		Dimen%totalblock=0
		Dimen%totalData=0
		if(allocated(Dimen%block))then
			do i=1,size(Dimen%block)
				call Dimen%block(i)%deallocate()
			end do
			deallocate(Dimen%block)
		end if
		return
	end subroutine 
	subroutine cleanBlock(Dimen)
		class(SymTensor),intent(inout) ::Dimen
		integer::i
		if(allocated(Dimen%block))then
			do i=1,size(Dimen%block)
				call Dimen%block(i)%deallocate()
			end do
		end if
		Dimen%totalblock=0
		return
	end subroutine 
	subroutine emptyDimension(Dimen)!overwrite the subroutine in Symdimension.f90
		class(SymTensor),intent(inout) ::Dimen
		integer::i
		if(deallocate_memory_flag)then
			call cleanDimension(Dimen)
			return
		end if
		call Dimen%SymDimension%empty()
		Dimen%Flag=.false.
		Dimen%DynamicClass=default_DynamicClass_in_Tensor	
		Dimen%classType=default_classtype_in_Tensor	
		Dimen%rank=0
		Dimen%totalblock=0
		Dimen%totalDAta=0
		if(allocated(Dimen%block))then
			do i=1,size(Dimen%block)
				call Dimen%block(i)%deallocate()
			end do
		end if
		return
	end subroutine

	subroutine emptyBlock(T)
		class(SymTensor),intent(inout)::T
		integer::i
		if(.not.T%getflag()) then
			return
		end if
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				call T%Block(i)%deallocate()
			end if
		end do
		T%totalblock=0
		return
	end subroutine

	!*********************  setclassType	 **********************	

	subroutine setclassType(T,classType_)
		class(SymTensor),intent(inout) :: T
		character(len=*),intent(in)::classType_
		integer::	classtype,i
		if(T%getflag())then
			write(*,*)" Can not set type to a allocated Tensor"
			call error_stop()
		end if
		classtype=select_data_type_char(classtype_)
		if(deallocate_memory_flag)then
			if(T%classtype.ne.classtype) call T%deallocateBlock()
		end if
		T%classType=classType
		T%DynamicClass=.false.
		if(T%getFlag())then
			do i=1,T%getTotalData()
				call T%block(i)%setType(classtype)
			end do
		end if
		return
	end subroutine
	subroutine setclassType2(T,classType)
		class(SymTensor),intent(inout) :: T
		integer,intent(in)::classType
		integer::i
		if(T%getFlag())then
			write(*,*)" Can not set type to a allocated Tensor"
			call error_stop()
		end if
		if(deallocate_memory_flag)then
			if(T%classtype.ne.classtype) call T%deallocateBlock()
		end if
		T%classType=classType
		T%DynamicClass=.false.
		if(T%getFlag())then
			do i=1,T%getTotalData()
				call T%block(i)%setType(classtype)
			end do
		end if
		return
	end subroutine
	subroutine resetDimension(dimen,DimData)
		class(SymTensor),intent(inout) ::Dimen
		integer,intent(in) :: DimData(:)
		if(product(DimData).ne.dimen%getTotaldata()) then
			call writemess("ERROR in resetdim",-1)
			call error_stop()
		end if
		call dimen%Dimension%resetDim(DimData)
		dimen%rank=size(DimData)
		return
	end subroutine
	subroutine resetDimension2(Ten,dimen)
		class(SymTensor),intent(inout)::Ten
		type(Symdimension),intent(in)::dimen
		if(dimen%size().ne.Ten%getTotaldata()) then
			call writemess("ERROR in resetdim2",-1)
			call writemess(dimen%size()+','+Ten%getTotaldata(),-1)
			call error_stop()
		end if
		Ten%SymDimension=dimen
		Ten%rank=dimen%getRank()
		return
	end subroutine	
	subroutine resetDimension3(Ten,dimen)
		class(SymTensor),intent(inout)::Ten
		type(dimension),intent(in)::dimen
		if(dimen%size().ne.Ten%getTotaldata()) then
			call writemess("ERROR in resetdim2",-1)
			call writemess(dimen%size()+','+Ten%getTotaldata(),-1)
			call error_stop()
		end if
		Ten%SymDimension=dimen
		Ten%rank=dimen%getRank()
		return
	end subroutine		

	subroutine reset_dim_no_check1(Ten,dimen)
		class(SymTensor),intent(inout)::Ten
		type(dimension),intent(in)::dimen
		integer::i
		Ten%SymDimension=dimen
		Ten%rank=dimen%getRank()
		Ten%totalData=dimen%size()
		Ten%totalBlock=0
		do i=1,Ten%totalData
			if(Ten%Block(i)%getFlag())then
				Ten%totalBlock=Ten%totalBlock+1
			end if
		end do
		return
	end subroutine	
	subroutine reset_dim_no_check2(Ten,dimen)
		class(SymTensor),intent(inout)::Ten
		type(SymDimension),intent(in)::dimen
		integer::i
		Ten%SymDimension=dimen
		Ten%rank=dimen%getRank()
		Ten%totalData=dimen%size()
		Ten%totalBlock=0
		do i=1,Ten%totalData
			if(Ten%Block(i)%getFlag())then
				Ten%totalBlock=Ten%totalBlock+1
			end if
		end do
		return
	end subroutine	

	subroutine RNDimRoutine(Dimen) !ouverwrite
		class(SymTensor),intent(inout) :: Dimen
		call Dimen%SymDimension%RNDim()
		Dimen%rank=Dimen%SymDimension%getRank()
	end subroutine

	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                    assignment    
	!
	!**************************************************************************************************************
	!**************************************************************************************************************

	subroutine assignmentTen(T,T2)
		type(SymTensor),intent(inout) ::T
		type(SymTensor),intent(in) :: T2
		integer::i,length,typede,oldType
		logical::notifDynamic
		if(.not.T2%getflag())then
			call T%empty()
			return
		end if
		typede=T2%getType()
		T%rank=T2%rank
		T%SymDimension=T2%SymDimension
		length=T2%gettotalData()
		notifDynamic=.not.T%ifDynamic()
		oldType=T%getType()
		if(T%ifDynamic())then
			if(deallocate_memory_flag)then
				if(T%getType().ne.typede) call T%deallocateBlock()
			end if
			T%classType=typede
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=T2%TotalBlock
		T%flag=T2%flag
		do i=1,length
			if(T2%Block(i)%getFlag())then
				if(notifDynamic)then
					if(oldType.ne.T%Block(i)%getType())then
						call T%Block(i)%setType(oldType)
					end if
				end if
				T%Block(i)=T2%Block(i)
			else
				call T%Block(i)%deallocate()
			end if
		end do
		return
	end subroutine



	subroutine assignmentreal4(r,T2)
		real*4,intent(inout)::r
		integer::i
		type(SymTensor),intent(in) :: T2
		if(.not.T2%getFlag())then
			r=0
			return
		end if
		if(T2%getTotalBlock().gt.1)then
			call writemess('Can not copy the SymTensor to real')
			call writemess('totalBlock of SymTensor is not equal to 1')
			call T2%diminfo()
			call error_stop
		end if
		if(T2%getTotalBlock().eq.0)then
			r=0
			return
		end if
		r=0
		do i=1,T2%getTotalData()
			if(T2%block(i)%getFlag())then
				r=T2%block(i)
				return
			end if
		end do
		return
	end subroutine
	subroutine assignmentreal8(r,T2)
		real*8,intent(inout)::r
		integer::i
		type(SymTensor),intent(in) :: T2
		if(.not.T2%getFlag())then
			r=0
			return
		end if
		if(T2%getTotalBlock().gt.1)then
			call writemess('Can not copy the SymTensor to real')
			call writemess('totalBlock of SymTensor is not equal to 1')
			call T2%diminfo()
			call error_stop
		end if
		if(T2%getTotalBlock().eq.0)then
			r=0
			return
		end if
		r=0
		do i=1,T2%getTotalData()
			if(T2%block(i)%getFlag())then
				r=T2%block(i)
				return
			end if
		end do
		return
	end subroutine
	subroutine assignmentcom4(r,T2)
		complex(kind=4),intent(inout)::r
		integer::i
		type(SymTensor),intent(in) :: T2
		if(.not.T2%getFlag())then
			call writemess('Can not copy the SymTensor to complex')
			call writemess('There is no data in the SymTensor')
			call T2%diminfo()
			call error_stop
		end if
		if(T2%getTotalBlock().ne.1)then
			call writemess('Can not copy the SymTensor to complex')
			call writemess('totalBlock of SymTensor is not equal to 1')
			call T2%diminfo()
			call error_stop
		end if
		r=0
		do i=1,T2%getTotalData()
			if(T2%block(i)%getFlag())then
				r=T2%block(i)
				return
			end if
		end do
		return
	end subroutine
	subroutine assignmentcom8(r,T2)
		complex(kind=8),intent(inout)::r
		integer::i
		type(SymTensor),intent(in) :: T2
		if(.not.T2%getFlag())then
			call writemess('Can not copy the SymTensor to complex')
			call writemess('There is no data in the SymTensor')
			call T2%diminfo()
			call error_stop
		end if
		if(T2%getTotalBlock().ne.1)then
			call writemess('Can not copy the SymTensor to complex')
			call writemess('totalBlock of SymTensor is not equal to 1')
			call T2%diminfo()
			call error_stop
		end if
		r=0
		do i=1,T2%getTotalData()
			if(T2%block(i)%getFlag())then
				r=T2%block(i)
				return
			end if
		end do
		return
	end subroutine





	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                    allocateTensor    
	!
	!**************************************************************************************************************
	!**************************************************************************************************************

	subroutine allocateTensor1(T,dimen,typede)
		class(SymTensor),intent(inout) ::T
		type(SymDimension),intent(in)::dimen
		integer,intent(in)::typede
		integer::length
		if(T%getflag())then
			call writemess("Can not allocate to a allocated Tensor",-1)
			call error_stop
		end if
		T%rank=dimen%getRank()
		T%SymDimension=dimen
		length=dimen%size()
		if(length.le.0) then
			call writemess("ERROR in allocateTensor",-1)
			call writemess("length="+length,-1)
			call dimen%print()
			call error_stop()
		end if
		if(T%ifDynamic())then
			if(deallocate_memory_flag)then
				if(T%getType().ne.typede) call T%deallocateBlock()
			end if
			call T%setType(typede)
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=0
		call T%Static()
		return
	end subroutine

	subroutine allocateTensor2(T,dimen,typede)
		class(SymTensor),intent(inout) ::T
		integer,intent(in)::dimen(:)
		integer,intent(in)::typede
		integer::length
		if(T%getflag())then
			write(*,*)"Can not allocate to a allocated Tensor"
			stop
		end if
		T%rank=size(dimen)
		T%SymDimension=dimen
		length=product(dimen)
		if(length.le.0) then
			write(*,*)"ERROR in allocateTensor"
			call error_stop()
		end if
		if(T%ifDynamic())then
			if(deallocate_memory_flag)then
				if(T%getType().ne.typede) call T%deallocateBlock()
			end if
			call T%setType(typede)
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=0
		call T%Static()
	!	call allocateData(T%TData,length,typede,.true.)
		return
	end subroutine



	subroutine allocateTensor3(T,T2,typede)
		class(SymTensor),intent(inout) ::T
		class(SymTensor),intent(in) ::T2
		integer,intent(in)::typede
		integer::length,i
		if(T%getflag())then
			call writemess("Can not allocate to a allocated Tensor",-1)
			call error_stop
		end if
		if(.not.T2%getflag())then
			call T%empty()
			return
		end if
		T%rank=T2%rank
		T%SymDimension=T2%SymDimension
		length=T2%gettotalData()
		if(T%ifDynamic())then
			if(deallocate_memory_flag)then
				if(T%getType().ne.typede) call T%deallocateBlock()
			end if
			call T%setType(typede)
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=T2%totalBlock
		call T%Static()
		do i=1,length
			if(T2%Block(i)%getFlag())then
				call T%block(i)%allocate(T2%Block(i),typede)
			else
				call T%block(i)%deallocate()
				call T%block(i)%setType(typede)
			end if
		end do
		return
	end subroutine


	subroutine allocateTensor4(T,dimen,typede)
		class(SymTensor),intent(inout) ::T
		type(Symdimension),intent(in)::dimen
		character(len=*),intent(in)::typede
		integer::length
		if(T%getflag())then
			call writemess("Can not allocate to a allocated Tensor",-1)
			call error_stop
		end if
		T%rank=dimen%getRank()
		T%SymDimension=dimen
		length=dimen%size()
		if(length.le.0) then
			call writemess("ERROR in allocateTensor",-1)
			call writemess("length="+length,-1)
			call dimen%print()
			call error_stop()
		end if
		if(T%ifDynamic())then
			if(deallocate_memory_flag)then
				if(T%getClassType().ne.typede) call T%deallocateBlock()
			end if
			call T%setType(typede)
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=0
		call T%Static()
		return
	end subroutine


	subroutine allocateTensor5(T,dimen,typede)
		class(SymTensor),intent(inout) ::T
		integer,intent(in)::dimen(:)
		character(len=*),intent(in)::typede
		integer::length
		if(T%getflag())then
			write(*,*)"Can not allocate to a allocated Tensor"
			stop
		end if
		T%rank=size(dimen)
		T%SymDimension=dimen
		length=product(dimen)
		if(length.le.0) then
			write(*,*)"ERROR in allocateTensor"
			call error_stop()
		end if
		if(T%ifDynamic())then
			if(deallocate_memory_flag)then
				if(T%getClassType().ne.typede) call T%deallocateBlock()
			end if
			call T%setType(typede)
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=0
		call T%Static()
		!call allocateData(T%TData,length,typede,.true.)
		return
	end subroutine


	subroutine allocateTensor6(T,T2,typede)
		class(SymTensor),intent(inout) ::T
		class(SymTensor),intent(in) ::T2
		character(len=*),intent(in)::typede
		integer::length,i
		if(T%getflag())then
			call writemess("Can not allocate to a allocated Tensor",-1)
			call error_stop
		end if
		if(.not.T2%getflag())then
			call T%empty()
			return
		end if
		T%rank=T2%rank
		T%SymDimension=T2%SymDimension
		length=T2%gettotalData()
		if(T%ifDynamic())then
			if(deallocate_memory_flag)then
				if(T%getClassType().ne.typede) call T%deallocateBlock()
			end if
			call T%setType(typede)
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=T2%totalBlock
		call T%Static()
		do i=1,length
			if(T2%Block(i)%getFlag())then
				call T%block(i)%allocate(T2%Block(i),typede)
			else
				call T%block(i)%deallocate()
				call T%block(i)%setType(typede)
			end if
		end do
		return
	end subroutine

	subroutine allocateTensor7(T,dimen)
		class(SymTensor),intent(inout) ::T
		type(Symdimension),intent(in)::dimen
		integer::length
		if(T%getflag())then
			call writemess("Can not allocate to a allocated Tensor",-1)
			call error_stop
		end if
		T%rank=dimen%getRank()
		T%SymDimension=dimen
		length=dimen%size()
		if(length.le.0) then
			call writemess("ERROR in allocateTensor",-1)
			call writemess("length="+length,-1)
			call dimen%print()
			call error_stop()
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=0
		return
	end subroutine

	subroutine allocateTensor9(T,T2)
		class(SymTensor),intent(inout) ::T
		class(SymTensor),intent(in) ::T2
		integer::length,i
		if(T%getflag())then
			call writemess("Can not allocate to a allocated Tensor",-1)
			call error_stop
		end if
		if(.not.T2%getflag())then
			call T%empty()
			return
		end if
		T%rank=T2%rank
		T%SymDimension=T2%SymDimension
		length=T2%gettotalData()
		if(T%ifDynamic())then
			if(deallocate_memory_flag)then
				if(T%getType().ne.T2%GetType()) call T%deallocateBlock()
			end if
			call T%setType(T2%GetType())
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=T2%TotalBlock
		do i=1,T2%getToTalData()
			if(T2%block(i)%getFlag())then
				call T%Block(i)%allocate(T2%block(i))
			else
				call T%block(i)%deallocate()
			end if
		end do
		return
	end subroutine

	subroutine allocateTensor13(T,QN)
		class(SymTensor),intent(inout) ::T
		type(QuanNum),intent(in)::QN(:)
		integer::length
		if(T%getflag())then
			call writemess("Can not allocate to a allocated Tensor",-1)
			call error_stop
		end if
		T%SymDimension=QN
		T%rank=T%SymDimension%getRank()
		length=T%SymDimension%size()
		if(length.le.0) then
			call writemess("ERROR in allocateTensor",-1)
			call writemess("Quantum number length="+length,-1)
			call T%print()
			call error_stop()
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=0
		return
	end subroutine

	subroutine allocateTensor14(T,QN,typede)
		class(SymTensor),intent(inout) ::T
		type(QuanNum),intent(in)::QN(:)
		character(len=*),intent(in)::typede
		integer::length
		if(T%getflag())then
			call writemess("Can not allocate to a allocated Tensor",-1)
			call error_stop
		end if
		T%SymDimension=QN
		T%rank=T%SymDimension%getRank()
		length=T%SymDimension%size()
		if(length.le.0) then
			call writemess("ERROR in allocateTensor",-1)
			call writemess("Quantum number length="+length,-1)
			call t%print()
			call error_stop()
		end if
		if(T%ifDynamic())then
			if(deallocate_memory_flag)then
				if(T%getClassType().ne.typede) call T%deallocateBlock()
			end if
			call T%setType(typede)
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=0
		call T%Static()
		return
	end subroutine

	subroutine allocateTensor15(T,QN,typede)
		class(SymTensor),intent(inout) ::T
		type(QuanNum),intent(in)::QN(:)
		integer,intent(in)::typede
		integer::length
		if(T%getflag())then
			call writemess("Can not allocate to a allocated Tensor",-1)
			call error_stop
		end if
		T%SymDimension=QN
		T%rank=T%SymDimension%getRank()
		length=T%SymDimension%size()
		if(length.le.0) then
			call writemess("ERROR in allocateTensor",-1)
			call writemess("Quantum number length="+length,-1)
			call t%print()
			call error_stop()
		end if
		if(T%ifDynamic())then
			if(deallocate_memory_flag)then
				if(T%getType().ne.typede) call T%deallocateBlock()
			end if
			call T%setType(typede)
		end if
		call allocateCheck(T%block,length)
		T%flag=.true.
		T%TotalData=length
		T%TotalBlock=0
		call T%Static()
		return
	end subroutine



	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                    allocate Block    
	!
	!**************************************************************************************************************
	!**************************************************************************************************************


	subroutine allocateBlock_subroutine1(T,i,LD1,dimen,classType,Dynamic)
		integer::i,LD1
		type(Tensor),intent(inout)::T(LD1)
		integer,intent(in)::dimen(:)
		integer,intent(in)::classType
		logical,intent(in)::Dynamic
		call T(i)%Dynamic()
		call T(i)%allocate(dimen,classType)
		if(.not.Dynamic) call T(i)%Static()
		return
	end subroutine
	subroutine allocateBlock_subroutine2(T,i,j,LD1,LD2,dimen,classType,Dynamic)
		integer::i,j,LD1,LD2
		type(Tensor),intent(inout)::T(LD1,LD2)
		integer,intent(in)::dimen(:)
		integer,intent(in)::classType
		logical,intent(in)::Dynamic
		call T(i,j)%Dynamic()
		call T(i,j)%allocate(dimen,classType)
		if(.not.Dynamic) call T(i,j)%Static()
		return
	end subroutine


	subroutine allocateBlock_subroutine3(T,i,j,k,LD1,LD2,LD3,dimen,classType,Dynamic)
		integer::i,j,k,LD1,LD2,LD3
		type(Tensor),intent(inout)::T(LD1,LD2,LD3)
		integer,intent(in)::dimen(:)
		integer,intent(in)::classType
		logical,intent(in)::Dynamic
		call T(i,j,k)%Dynamic()
		call T(i,j,k)%allocate(dimen,classType)
		if(.not.Dynamic) call T(i,j,k)%Static()
	end subroutine
	

	subroutine allocateBlock_subroutine4(T,i,j,k,l,LD1,LD2,LD3,LD4,dimen,classType,Dynamic)
		integer::i,j,k,l,LD1,LD2,LD3,LD4
		type(Tensor),intent(inout)::T(LD1,LD2,LD3,LD4)
		integer,intent(in)::dimen(:)
		integer,intent(in)::classType
		logical,intent(in)::Dynamic
		call T(i,j,k,l)%Dynamic()
		call T(i,j,k,l)%allocate(dimen,classType)
		if(.not.Dynamic) call T(i,j,k,l)%Static()
	end subroutine		



	subroutine allocateBlock1(T,vec)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::vec(:)
		integer::classType
		integer,allocatable::dimen(:)
		integer::lenvec,LD1,LD2,LD3,LD4,inde,i
		integer,allocatable::Tdim(:)
		lenvec=size(vec)
		allocate(dimen(T%getRank()))
		dimen=T%DegDim(vec)
		classType=T%getType()
		do i=1,lenvec!The degeneracy is 0, do not allocate block
			if(dimen(i).eq.0)return
		end do
		select case (lenvec)
			case (1)
				if(vec(1).gt.T%getTotalData())then
					call writemess("index is larger than the totaldata of SymTensor")
					call error_stop()
				end if
				call allocateBlock_subroutine1(T%Block,vec(1),T%GetTotalData(),dimen,classType,T%ifDynamic())
				T%totalblock=T%totalblock+1
				return
			case (2)
				LD1=T.dim.1
				LD2=T.dim.2
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2) )Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+') and Dim=('+LD1+','+LD2+')')
					call error_stop()
				end if
				call allocateBlock_subroutine2(T%Block,vec(1),vec(2),LD1,LD2,dimen,classType,T%ifDynamic())
				T%totalblock=T%totalblock+1
				return
			case (3)
				LD1=T.dim.1
				LD2=T.dim.2
				LD3=T.dim.3
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2) .or. (vec(3).gt.LD3))Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+','+vec(3)+') and Dim=('+LD1+','+LD2+','+LD3+')')
					call error_stop()
				end if
				call allocateBlock_subroutine3(T%Block,vec(1),vec(2),vec(3),LD1,LD2,LD3,dimen,classType,T%ifDynamic())
				T%totalblock=T%totalblock+1
				return
			case (4)
				LD1=T.dim.1
				LD2=T.dim.2
				LD3=T.dim.3
				LD4=T.dim.4
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2) .or. (vec(3).gt.LD3).or. (vec(4).gt.LD4) )Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+','+vec(3)+','+vec(4)+') and Dim=('+LD1+','+LD2+','+LD3+','+LD4+')')
					call error_stop()
				end if
				call allocateBlock_subroutine4(T%Block,vec(1),vec(2),vec(3),vec(4),LD1,LD2,LD3,LD4,dimen,classType,T%ifDynamic())
				T%totalblock=T%totalblock+1
				return
			case default
				allocate(Tdim(T%getRank()) )
				TDim=T%dim()
				inde=addressToIndes(Tdim,vec)
				if(inde.gt.T%getTotalData())then
					call writemess("index is larger than the totaldata of SymTensor")
					call error_stop()
				end if
				call allocateBlock_subroutine1(T%Block,inde,T%GetTotalData(),dimen,classType,T%ifDynamic())
				T%totalblock=T%totalblock+1
				return
		end select
	end subroutine
	subroutine allocateBlockAll(T)
		class(SymTensor),intent(inout)::T
		integer::i,totaldata,classType,rank,j
		logical::ifDynamic
		integer,allocatable::vec(:),TDim(:),dimen(:)
		logical::goon
		totaldata=T%GetTotalData()
		ifDynamic=T%ifDynamic()
		classType=T%getType()
		allocate(TDim(T%getRank()))
		allocate(dimen(T%getRank()))
		allocate(vec(T%getRank()))
		TDim=T%dim()
		rank=T%getRank()
		do i=1,totaldata
			call IndesToaddress(TDim,vec,i)
			dimen=T%DegDim(vec)
			goon=.true.
			do j=1,rank!The degeneracy is 0, do not allocate block
				if(dimen(j).eq.0)then
					goon=.false.
					exit
				end if
			end do
			if(goon)then
				call allocateBlock_subroutine1(T%block,i,totaldata,dimen,classType,ifDynamic)
				T%totalblock=T%totalblock+1
			end if
		end do
		return
	end subroutine
	
	subroutine allocateBlock2(T,element)
		class(SymTensor),intent(inout)::T
		type(Tensor),intent(in)::element
		integer::I
		integer,allocatable::vec(:)
		if(element%getType().ne.1)then
			call writemess("When allocate SymTensor, one should input the non-zero element indices as a integer Tensor")
			call error_stop()
		end if
		allocate(vec(element%dim(2)))
		do i=1,element%dim(1)
			vec=element%subTensor((/-1,i/))
			call allocateBlock1(T,vec)
		end do
		return
	end subroutine
	subroutine allocateBlock3(T,ith)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::ith
		integer::totaldata,classType,rank,j
		logical::ifDynamic
		integer,allocatable::vec(:),TDim(:),dimen(:)
		logical::goon
		ifDynamic=T%ifDynamic()
		classType=T%getType()
		allocate(TDim(T%getRank()))
		allocate(dimen(T%getRank()))
		allocate(vec(T%getRank()))
		TDim=T%dim()
		call IndesToaddress(TDim,vec,ith)
		dimen=T%DegDim(vec)
		do j=1,rank!The degeneracy is 0, do not allocate block
			if(dimen(j).eq.0)then
				goon=.false.
				exit
			end if
		end do
		if(goon)then
			call allocateBlock_subroutine1(T%block,ith,totaldata,dimen,classType,ifDynamic)
				T%totalblock=T%totalblock+1
		end if
		return
	end subroutine

	subroutine allocateSymBlock1(T,vec)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::vec(:)
		logical::goon
		integer::i
		character*500,allocatable::w
		real*4,allocatable::qnum(:)
		call rule(goon,T%SymDimension,vec,T%GetRank())
		if(goon)then
			call T%allocateBlock(vec)
			return
		end if
		call writemess('Can not allocate the element of a SymTensor')
		allocate(w)
		w='The indices are: ('
		do i=1,size(vec)-1
			w=w+vec(i)+','
		end do
		w=w+vec(i)+')'
		call writemess(w)
		allocate(qnum(size(vec)))
		qnum=T%QNDim(vec)
		w='The quantum number are: ('
		do i=1,size(vec)-1
			w=w+qnum(i)+','
		end do
		w=w+qnum(i)+')'
		call writemess(w)
		call error_stop()
	end 	subroutine
	subroutine allocateSymBlock2(T,element)
		class(SymTensor),intent(inout)::T
		type(Tensor),intent(in)::element
		integer,allocatable::Adim(:)
		if(element.gt.T%getTotalData())Then
			write(*,*)"Index is larger than the len of SymTensor"
			call error_stop()
		end if
		allocate(Adim(T%getRank()))
		call IndesToaddress(T%dim(),Adim,element%ii(1))
		call allocateSymBlock1(T,Adim)
		return
	end subroutine
	subroutine allocateSymBlock3(T,element)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::element
		integer,allocatable::Adim(:)
		if(element.gt.T%getTotalData())Then
			write(*,*)"Index is larger than the len of SymTensor"
			call error_stop()
		end if
		allocate(Adim(T%getRank()))
		call IndesToaddress(T%dim(),Adim,element)
		call allocateSymBlock1(T,Adim)
		return
	end subroutine

	subroutine allocatefSymBlockAll(T)
		class(SymTensor),intent(inout)::T
		type(Tensor)::elementindex
		call NonZeroElement(elementindex,T%symDimension)
		if(.not.elementindex%getFlag())then
			call T%diminfo()
			call writemess('There is no non-zero block')
			call error_stop
		end if
		call T%allocateBlock(elementindex)
		return
	end subroutine














	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                    get dimension   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************






	type(Symdimension) function  getSubDim2(T,inde)
		type(SymTensor),intent(in) :: T
		integer,intent(in)::inde
		getSubDim2=T%SymDimension.subdim.inde
		return
	end function
	type(Symdimension) function  getSubDim3(T,inde)
		type(SymTensor),intent(in) :: T
		integer,intent(in)::inde(2)
		getSubDim3=T%SymDimension.subdim.inde
		return
	end function
	type(Symdimension) function  getSubDim4(T)
		type(SymTensor),intent(in) :: T
		getSubDim4=T%SymDimension
		return
	end function
	type(Symdimension) function  getSubDim2_name(T,w)
		type(SymTensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::w
		integer::inde
		inde=T%FindOrder(w)
		getSubDim2_name=T%SymDimension.subdim.inde
		return
	end function






	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                    random SymTensor   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************

	subroutine random_SymTensor_block_i_subroutine2(T,i,j,LD1,LD2,region)
		integer::i,j,LD1,LD2
		type(Tensor),intent(inout)::T(LD1,LD2)
		real*8,optional,intent(in)::region(*)
		if(T(i,j)%getFlag())then
			call T(i,j)%random()
		else
			call writemess("Do not allocate the Tensor in block")
			call error_stop
		end if
		return
	end subroutine

	!T(i,j,k)=random Tensor

	subroutine random_SymTensor_block_i_subroutine3(T,i,j,k,LD1,LD2,LD3,region)
		integer::i,j,k,LD1,LD2,LD3
		type(Tensor),intent(inout)::T(LD1,LD2,LD3)
		real*8,optional,intent(in)::region(*)
		if(T(i,j,k)%getFlag())then
			call T(i,j,k)%random()
		else
			call writemess("Do not allocate the Tensor in block")
			call error_stop
		end if
		return
	end subroutine

	!T(i,j,k,l)=random Tensor

	subroutine random_SymTensor_block_i_subroutine4(T,i,j,k,l,LD1,LD2,LD3,LD4,region)
		integer::i,j,k,l,LD1,LD2,LD3,LD4
		type(Tensor),intent(inout)::T(LD1,LD2,LD3,LD4)
		real*8,optional,intent(in)::region(*)
		if(T(i,j,k,l)%getFlag())then
			call T(i,j,k,l)%random()
		else
			call writemess("Do not allocate the Tensor in block")
			call error_stop
		end if
		return
	end subroutine	



	subroutine set_random_Tensor_all(T,region)
		class(SymTensor),intent(inout)::T
		real*8,optional,intent(in)::region(2)
		integer::I
		if(.not.T%getflag()) then
			write(*,*)"Do not allocate SymTensor yet"
			call error_stop()
		end if
		do i=1,T%getTotalData()
			if(T%block(i)%getFlag())then
				call T%block(i)%random(region)
			end if
		end do
		return
	end subroutine
	subroutine set_random_Tensor_val(T,ith,region)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::ith
		real*8,optional,intent(in)::region(2)
		if(.not.T%getflag()) then
			write(*,*)"Do not allocate SymTensor yet"
			call error_stop()
		end if
		if(ith.gt.T%getTotalData())Then
			write(*,*)"Index is larger than the len of SymTensor"
			call error_stop()
		end if
		if(T%block(ith)%getFlag())then
			call T%block(ith)%random(region)
		else
			call writemess("Do not allocate the Tensor in block")
			call error_stop
		end if
		return
	end subroutine
	subroutine set_random_Tensor_vec(T,vec,region)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::vec(:)
		real*8,optional,intent(in)::region(2)
		integer::lenvec,LD1,LD2,LD3,LD4,inde
		integer,allocatable::Tdim(:)
		if(.not.T%getflag()) then
			return
		end if
		lenvec=size(vec)
		select case (lenvec)
			case (1)
				if(vec(1).gt.T%getTotalData())then
					call writemess("index is larger than the totaldata of SymTensor")
					call error_stop()
				end if
				call set_random_Tensor_val(T,vec(1),region)
				return
			case (2)
				LD1=T.dim.1
				LD2=T.dim.2
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2))Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+') and Dim=('+LD1+','+LD2+')')
				end if
				call random_SymTensor_block_i_subroutine2(T%block,vec(1),vec(2),LD1,LD2,region)
				return
			case (3)
				LD1=T.dim.1
				LD2=T.dim.2
				LD3=T.dim.3
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2) .or. (vec(3).gt.LD3) )Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+','+vec(3)+') and Dim=('+LD1+','+LD2+','+LD3+')')
					call error_stop()
				end if
				call random_SymTensor_block_i_subroutine3(T%block,vec(1),vec(2),vec(3),LD1,LD2,LD3,region)
				return
			case (4)
				LD1=T.dim.1
				LD2=T.dim.2
				LD3=T.dim.3
				LD4=T.dim.4
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2) .or. (vec(3).gt.LD3).or. (vec(4).gt.LD4) )Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+','+vec(3)+','+vec(4)+') and Dim=('+LD1+','+LD2+','+LD3+','+LD4+')')
					call error_stop()
				end if
				call random_SymTensor_block_i_subroutine4(T%block,vec(1),vec(2),vec(3),vec(4),LD1,LD2,LD3,LD4,region)
				return
			case default
				allocate(Tdim(T%getRank()) )
				TDim=T%dim()
				inde=addressToIndes(Tdim,vec)
				if(inde.gt.T%getTotalData())then
					call writemess("index is larger than the totaldata of SymTensor")
					call error_stop()
				end if
				call set_random_Tensor_val(T,inde,region)
				return
		end select
		return
	end subroutine


	subroutine set_random_SymTensor_all(T,region)
		class(SymTensor),intent(inout)::T
		real*8,optional,intent(in)::region(2)
		integer::I
		type(Tensor)::elementindex
		call NonZeroElement(elementindex,T%symDimension)
		if(.not.elementindex%getFlag())then
			call T%diminfo()
			call writemess('There is no non-zero block')
			call error_stop
		end if
		call T%allocateBlock(elementindex)
		call T%random(region)
		return
	end subroutine
	subroutine set_random_SymTensor_vec(T,vec,region)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::vec(:)
		real*8,optional,intent(in)::region(2)
		logical::goon
		integer::i
		character*500,allocatable::w
		real*4,allocatable::qnum(:)
		call rule(goon,T%SymDimension,vec,T%getRank())
		if(goon)then
			call T%random(vec,region)
			return
		end if
		call writemess('Can not set random value to the element of a SymTensor')
		allocate(w)
		w='The indices are: ('
		do i=1,size(vec)-1
			w=w+vec(i)+','
		end do
		w=w+vec(i)+')'
		call writemess(w)
		allocate(qnum(size(vec)))
		qnum=T%QNDim(vec)
		w='The quantum number are: ('
		do i=1,size(vec)-1
			w=w+qnum(i)+','
		end do
		w=w+qnum(i)+')'
		call writemess(w)
		call error_stop()
	end subroutine
	subroutine set_random_SymTensor_val(T,ith,region)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::ith
		real*8,optional,intent(in)::region(2)
		integer,allocatable::Adim(:)
		if(ith.gt.T%getTotalData())Then
			write(*,*)"Index is larger than the len of SymTensor"
			call error_stop()
		end if
		allocate(Adim(T%getRank()))
		call IndesToaddress(T%dim(),Adim,ith)
		call set_random_SymTensor_vec(T,Adim,region)
		return
	end subroutine

	integer function addressToIndes(Tdim,Adim)
		integer,intent(in) :: Adim(:),Tdim(:)
		integer :: i,Dimlen
		Dimlen=size(TDim)
		if(Dimlen.eq.1) then
			addressToIndes=Adim(1)
			return
		end if
		addressToIndes=0
		do i=Dimlen,2,-1
			addressToIndes=addressToIndes+(Adim(i)-1)*product(TDim(1:(i-1)))
		end do
		addressToIndes=addressToIndes+Adim(1)
		return 
	end function	



	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                    get data    
	!
	!**************************************************************************************************************
	!**************************************************************************************************************


	! Out element of block

	type(Tensor) function out_element1(T,ith) result(element)
		class(SymTensor),intent(in)::T
		integer,intent(in)::ith
		integer::i
		if(.not.T%getFlag())then
			call writemess("There is no data in the SymTensor")
			call error_stop()
		end if
		if(ith.gt.T%getTotalData())then
			call writemess("index is larger than the totaldata of SymTensor")
			call error_stop()
		end if
		if(T%getTotalBlock().eq.0)then
			call writemess("There is no block in the SymTensor")
			call error_stop()
		end if
		if(ith.eq.-1)then
			if(T%getTotalBlock().gt.1)then
				call writemess("There are more than 1 blocks in the SymTensor")
				call error_stop()
			end if
			do i=1,T%getTotalData()
				if(T%block(i)%getFlag())then
					element=T%block(i)
					return
				end if
			end do
			return
		end if
		element=T%block(ith)
		return
	end function
	
	type(Tensor) function out_element_QN(T,vec) result(element)
		class(SymTensor),intent(in)::T
		real*4,intent(in)::vec(:)
		integer::lenvec,LD1,LD2,LD3,LD4,inde
		integer,allocatable::Tdim(:)
		if(.not.T%getFlag())then
			call writemess("There is no data in the SymTensor")
			call error_stop()
		end if
		allocate(TDim(T%getRank()))
		TDim=T%Getindex(vec)
		element=out_element_vec(T,TDim)
		return
	end function
	type(Tensor) function out_element_vec(T,vec) result(element)
		class(SymTensor),intent(in)::T
		integer,intent(in)::vec(:)
		integer::lenvec,LD1,LD2,LD3,LD4,inde
		integer,allocatable::Tdim(:)
		if(.not.T%getFlag())then
			call writemess("There is no data in the SymTensor")
			call error_stop()
		end if
		if(T%getTotalBlock().eq.0)then
			call writemess("There is no block in the SymTensor")
			call error_stop()
		end if
		lenvec=size(vec)
		select case (lenvec)
			case (1)
				if(vec(1).gt.T%getTotalData())then
					call writemess("index is larger than the totaldata of SymTensor")
					call error_stop()
				end if
				element=T%block(vec(1))
				return
			case (2)
				LD1=T.dim.1
				LD2=T.dim.2
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2))Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+') and Dim=('+LD1+','+LD2+')')
				end if
				call out_element_subroutine2(T%block,vec(1),vec(2),LD1,LD2,element)
				return
			case (3)
				LD1=T.dim.1
				LD2=T.dim.2
				LD3=T.dim.3
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2) .or. (vec(3).gt.LD3) )Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+','+vec(3)+') and Dim=('+LD1+','+LD2+','+LD3+')')
					call error_stop()
				end if
				call out_element_subroutine3(T%block,vec(1),vec(2),vec(3),LD1,LD2,LD3,element)
				return
			case (4)
				LD1=T.dim.1
				LD2=T.dim.2
				LD3=T.dim.3
				LD4=T.dim.4
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2) .or. (vec(3).gt.LD3).or. (vec(4).gt.LD4) )Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+','+vec(3)+','+vec(4)+') and Dim=('+LD1+','+LD2+','+LD3+','+LD4+')')
					call error_stop()
				end if
				call out_element_subroutine4(T%block,vec(1),vec(2),vec(3),vec(4),LD1,LD2,LD3,LD4,element)
				return
			case default
				allocate(Tdim(T%getRank()) )
				TDim=T%dim()
				inde=addressToIndes(Tdim,vec)
				if(inde.gt.T%getTotalData())then
					call writemess("index is larger than the totaldata of SymTensor")
					call error_stop()
				end if
				element=T%block(inde)
				return
		end select
		return
	end function

	type(SymTensor) function Sym_element_vec(fH,QNindex,Degindex)result(subfH)
		class(SymTensor),intent(in)::fH
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

	type(SymTensor) function Sym_element_vec2(fH,index)result(subfH)
		class(SymTensor),intent(in)::fH
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

	type(SymTensor) function Sym_element_vec3(fH,QNindex,Degindex)result(subfH)
		class(SymTensor),intent(in)::fH
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




	subroutine out_element_subroutine2(block,i,j,LD1,LD2,inoutT)
		integer,intent(in)::i,j,LD1,LD2
		type(Tensor)::block(LD1,LD2),inoutT
		inoutT=block(i,j)
		return
	end subroutine
	subroutine out_element_subroutine3(block,i,j,k,LD1,LD2,LD3,inoutT)
		integer,intent(in)::i,j,k,LD1,LD2,LD3
		type(Tensor)::block(LD1,LD2,LD3),inoutT
			inoutT=block(i,j,k)
		return
	end subroutine
	subroutine out_element_subroutine4(block,i,j,k,l,LD1,LD2,LD3,LD4,inoutT)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		type(Tensor)::block(LD1,LD2,LD3,LD4),inoutT
			inoutT=block(i,j,k,l)
		return
	end subroutine


	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                    set value      
	!
	!**************************************************************************************************************
	!**************************************************************************************************************

	subroutine set_zero_Tensor(T)
		class(SymTensor),intent(inout)::T
		integer::i
		if(.not.T%getflag()) then
			return
		end if
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				call T%Block(i)%zero()
			end if
		end do
		return
	end subroutine



	subroutine set_element_val(T,vec,element) 
		class(SymTensor),intent(inout)::T
		integer,intent(in)::vec
		type(Tensor),intent(in)::element
		integer,allocatable::Tdim(:)
		logical::newblockFlag
		integer,allocatable::dimen(:)
		if(.not.T%getflag()) then
			call writemess("Do not allocate SymTensor yet")
			call error_stop()
		end if
		if(vec.gt.T%getTotalData())then
			call writemess("index is larger than the totaldata of SymTensor")
			call error_stop()
		end if
		allocate(Tdim(T%getRank()))
		call IndesToaddress(T%dim(),Tdim,vec)
		allocate(dimen(T%getRank()))
		dimen=T%SymDimension%DegDim(Tdim)
		if(.not.T%block(vec)%getFlag())then
			T%TotalBlock=T%TotalBlock+1
			call T%block(vec)%allocate(dimen,T%getType())
		end if
		call T%block(vec)%setValue(1,product(dimen),element)
		return
	end subroutine

	subroutine set_element_vec(T,vec,element) 
		class(SymTensor),intent(inout)::T
		integer,intent(in)::vec(:)
		type(Tensor),intent(in)::element
		integer::lenvec,LD1,LD2,LD3,LD4,inde
		integer,allocatable::Tdim(:)
		integer::newblockFlag
		integer,allocatable::dimen(:)
		if(.not.T%getflag()) then
			call writemess("Do not allocate SymTensor yet")
			call error_stop()
		end if
		lenvec=size(vec)
		select case (lenvec)
			case (1)
				if(vec(1).gt.T%getTotalData())then
					call writemess("index is larger than the totaldata of SymTensor")
					call error_stop()
				end if
				if(.not.element%getFlag())then
					if(T%block(vec(1))%getFlag()) then
						call T%block(vec(1))%deallocate()
						T%TotalBlock=T%TotalBlock-1
					end if
					return
				end if
				allocate(dimen(1))
				dimen=T%SymDimension%DegDim(vec)
				if(.not.T%block(vec(1))%getFlag())then
					T%TotalBlock=T%TotalBlock+1
					call T%block(vec(1))%allocate(dimen,T%getType())
				end if
				call T%block(vec(1))%setValue(1,dimen(1),element)
				return
			case (2)
				LD1=T.dim.1
				LD2=T.dim.2
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2))Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+') and Dim=('+LD1+','+LD2+')')
				end if
				call set_element_subroutine2(T%block,vec(1),vec(2),LD1,LD2,element,T%SymDimension,T%getType(),newblockFlag)
				T%TotalBlock=T%TotalBlock+newblockFlag
				return
			case (3)
				LD1=T.dim.1
				LD2=T.dim.2
				LD3=T.dim.3
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2) .or. (vec(3).gt.LD3) )Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+','+vec(3)+') and Dim=('+LD1+','+LD2+','+LD3+')')
					call error_stop()
				end if
				call set_element_subroutine3(T%block,vec(1),vec(2),vec(3),LD1,LD2,LD3,element,T%SymDimension&
																		,T%getType(),newblockFlag)
				T%TotalBlock=T%TotalBlock+newblockFlag
				return
			case (4)
				LD1=T.dim.1
				LD2=T.dim.2
				LD3=T.dim.3
				LD4=T.dim.4
				if( (vec(1).gt.LD1) .or. (vec(2).gt.LD2) .or. (vec(3).gt.LD3).or. (vec(4).gt.LD4) )Then
					call writemess("Index is larger than the len of SymTensor")
					call writemess("index=("+vec(1)+','+vec(2)+','+vec(3)+','+vec(4)+') and Dim=('+LD1+','+LD2+','+LD3+','+LD4+')')
					call error_stop()
				end if
				call set_element_subroutine4(T%block,vec(1),vec(2),vec(3),vec(4),LD1,LD2,LD3,LD4,element,T%SymDimension&
																		,T%getType(),newblockFlag)
				T%TotalBlock=T%TotalBlock+newblockFlag
				return
			case default
				allocate(Tdim(T%getRank()) )
				TDim=T%dim()
				inde=addressToIndes(Tdim,vec)
				if(inde.gt.T%getTotalData())then
					call writemess("index is larger than the totaldata of SymTensor")
					call error_stop()
				end if
				if(.not.element%getFlag())then
					if(T%block(inde)%getFlag()) then
						call T%block(inde)%deallocate()
						T%TotalBlock=T%TotalBlock-1
					end if
					return
				end if
				
				allocate(dimen(T%getRank()))
				dimen=T%SymDimension%DegDim(vec)
				if(.not.T%block(inde)%getFlag())then
					T%TotalBlock=T%TotalBlock+1
					call T%block(inde)%allocate(dimen,T%getType())
				end if
				call T%block(inde)%setValue(1,product(dimen),element)
				return
				return
		end select
		return
	end subroutine
	
	subroutine set_element_subroutine2(block,i,j,LD1,LD2,inoutT,Sdimen,classtype,newblockFlag)
		integer,intent(in)::i,j,LD1,LD2
		type(Tensor)::block(LD1,LD2),inoutT
		integer,intent(inout)::newblockFlag
		type(SymDimension),intent(in)::Sdimen
		integer,intent(in)::classtype
		integer::dimen(2)
		newblockFlag=0
		if(.not.inoutT%getFlag())then
			if(block(i,j)%getFlag())newblockFlag=-1
			call block(i,j)%deallocate()
			return
		end if
		dimen=Sdimen%DegDim((/i,j/))
		if(.not.block(i,j)%getFlag())then
			newblockFlag=1
			call block(i,j)%allocate(dimen,classtype)
		end if
		call block(i,j)%setValue(1,product(dimen),inoutT)
		return
	end subroutine
	subroutine set_element_subroutine3(block,i,j,k,LD1,LD2,LD3,inoutT,Sdimen,classtype,newblockFlag)
		integer,intent(in)::i,j,k,LD1,LD2,LD3
		type(Tensor)::block(LD1,LD2,LD3),inoutT
		integer,intent(inout)::newblockFlag
		type(SymDimension),intent(in)::Sdimen
		integer,intent(in)::classtype
		integer::dimen(3)
		newblockFlag=0
		if(.not.inoutT%getFlag())then
			if(block(i,j,k)%getFlag())newblockFlag=-1
			call block(i,j,k)%deallocate()
			return
		end if
		dimen=Sdimen%DegDim((/i,j,k/))
		if(.not.block(i,j,k)%getFlag())then
			newblockFlag=1
			call block(i,j,k)%allocate(dimen,classtype)
		end if
		call block(i,j,k)%setValue(1,product(dimen),inoutT)
		return
	end subroutine
	subroutine set_element_subroutine4(block,i,j,k,l,LD1,LD2,LD3,LD4,inoutT,Sdimen,classtype,newblockFlag)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		type(Tensor)::block(LD1,LD2,LD3,LD4),inoutT
		integer,intent(inout)::newblockFlag
		type(SymDimension),intent(in)::Sdimen
		integer,intent(in)::classtype
		integer::dimen(4)
		newblockFlag=0
		if(.not.inoutT%getFlag())then
			if(block(i,j,k,l)%getFlag())newblockFlag=-1
			call block(i,j,k,l)%deallocate()
			return
		end if
		dimen=Sdimen%DegDim((/i,j,k,l/))
		if(.not.block(i,j,k,l)%getFlag())then
			newblockFlag=1
			call block(i,j,k,l)%allocate(dimen,classtype)
		end if
		call block(i,j,k,l)%setValue(1,product(dimen),inoutT)
		return
	end subroutine



	subroutine set_Symelement_vec(T,vec,element) 
		class(symTensor),intent(inout)::T
		integer,intent(in)::vec(:)
		type(Tensor),intent(in)::element
		logical::goon
		integer::i
		character*500,allocatable::w
		real*4,allocatable::qnum(:)
		call rule(goon,T%SymDimension,vec,T%getRank())
		if(goon)then
			call T%setValue(vec,element)
			return
		end if
		call writemess('Can not set the value to the element of a SymTensor')
		allocate(w)
		w='The indices are: ('
		do i=1,size(vec)-1
			w=w+vec(i)+','
		end do
		w=w+vec(i)+')'
		call writemess(w)
		allocate(qnum(size(vec)))
		qnum=T%QNDim(vec)
		w='The quantum number are: ('
		do i=1,size(vec)-1
			w=w+qnum(i)+','
		end do
		w=w+qnum(i)+')'
		call writemess(w)
		call error_stop()
	end 	subroutine
	subroutine set_Symelement_val(T,vec,element) 
		class(SymTensor),intent(inout)::T
		integer,intent(in)::vec
		type(Tensor),intent(in)::element
		integer,allocatable::Adim(:)
		if(vec.gt.T%getTotalData())Then
			write(*,*)"Index is larger than the len of SymTensor"
			call error_stop()
		end if
		allocate(Adim(T%getRank()))
		call IndesToaddress(T%dim(),Adim,vec)
		call set_Symelement_vec(T,Adim,element) 
		return
	end subroutine
	
	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                    Fuse and split      
	!
	!**************************************************************************************************************
	!**************************************************************************************************************

	!**************** fuse   ****************
	!		combine two index of the Tensor,which is con_index and con_index+1

	type(SymTensor) function fuseTensor_val(T,inde)result(fuseT)
		integer,intent(in) :: inde
		type(SymTensor),intent(in) :: T
		integer::i
		fuseT=T
		call fuseT%SymDimension%fuse(inde)
		fuseT%rank=fuseT%rank-1
		do i=1,fuseT%getTotalData()
			if(fuseT%Block(i)%getFlag())then
				call fuseT%Block(i)%fuse(inde)
			end if
		end do
		return
	end function
	type(SymTensor) function fuseTensor_vec(T,vector)result(fuseT)
		integer,intent(in) ::vector(2)
		type(SymTensor),intent(in) :: T
		integer::i
		fuseT=T
		call fuseT%fuse(vector(1),vector(2))
		fuseT%rank=fuseT%SymDimension%getRank()
		do i=1,fuseT%getTotalData()
			if(fuseT%Block(i)%getFlag())then
				call fuseT%Block(i)%fuse(vector(1),vector(2))
			end if
		end do
		return
	end function		
	type(SymTensor) function Tensorsplit2(T,vector)result(splitT)
		type(SymTensor),intent(in) :: T
		integer,intent(in) ::vector(2)
		integer::i
		splitT=T
		call splitT%split(vector(1),vector(2))
		splitT%rank=splitT%SymDimension%getRank()
		do i=1,splitT%getTotalData()
			if(splitT%Block(i)%getFlag())then
				call splitT%Block(i)%split(vector(1),vector(2))
			end if
		end do
		return
	end function
			
	type(SymTensor) function Tensorsplit3(T,de_index)result(splitT)
		type(SymTensor),intent(in) :: T
		integer,intent(in) :: de_index
		integer::i
		splitT=T
		call splitT%split(de_index)
		splitT%rank=splitT%SymDimension%getRank()
		do i=1,splitT%getTotalData()
			if(splitT%Block(i)%getFlag())then
				call splitT%Block(i)%split(de_index)
			end if
		end do
		return
	end function
			
	type(SymTensor) function TensorsplitAll(T)result(splitT)
		type(SymTensor),intent(in) :: T
		integer::i
		splitT=T
		call splitT%split()
		splitT%rank=splitT%SymDimension%getRank()
		do i=1,splitT%getTotalData()
			if(splitT%Block(i)%getFlag())then
				call splitT%Block(i)%split()
			end if
		end do
		return
	end function	
	subroutine fuseDimension(dimen,index1,index2)!overwite
		class(SymTensor),intent(inout) :: dimen
		integer,intent(in) :: index1
		integer,optional,intent(in)::index2
		integer ::i
		call dimen%Dimension%fuse(index1,index2)
		dimen%rank=dimen%SymDimension%getRank()
		do i=1,dimen%getTotalData()
			if(dimen%Block(i)%getFlag())then
				call dimen%Block(i)%fuse(index1,index2)
			end if
		end do
		return
	end subroutine		    
	subroutine splitDimension(Dimen,de_index,inde)
		class(SymTensor),intent(inout) :: Dimen
		integer,optional,intent(in) :: de_index
		integer,optional,intent(in)::inde
		integer::i
		call dimen%Dimension%split(de_index,inde)
		dimen%rank=dimen%Dimension%getRank()
		do i=1,dimen%getTotalData()
			if(dimen%Block(i)%getFlag())then
				call dimen%Block(i)%split(de_index,inde)
			end if
		end do
		return
	end subroutine	












	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                              Quantum Number   Fuse and split      
	!
	!**************************************************************************************************************
	!**************************************************************************************************************



	!
	!
	!QuanNum1+QuanNum2=NewQuanNum
	!
	!order of codes for do
	! outQ=Q1+Q2 
	!  first order Q1 and then Q2 at last outQ
	!Parity example:
	!order of codes for do
	! outQ      Q1  Q2 
	!  -1       -1  -1   false
	!  -1       +1  -1
	!  -1       -1  +1
	!  -1       +1  +1   false
	!  +1       -1  -1
	!  +1       +1  -1   false
	!  +1       -1  +1   false
	!  +1       +1  +1
	!
	!The U(1) symmetry
	!the rule for outQ Q1 Q2 are -1 1 1
	! outQ      Q1   Q2 
	! -1.5      -1  -0.5
	! -1.5       0  -0.5 false
	! -1.5       1  -0.5 false
	! -1.5      -1   0.5 false
	! -1.5       0   0.5 false
	! -1.5       1   0.5 false
	!
	! -0.5      -1  -0.5 false
	! -0.5       0  -0.5 
	! -0.5       1  -0.5 false
	! -0.5      -1   0.5 
	! -0.5       0   0.5 false
	! -0.5       1   0.5 false
	!
	!  0.5      -1  -0.5 false
	!  0.5       0  -0.5 false
	!  0.5       1  -0.5 
	!  0.5      -1   0.5 false
	!  0.5       0   0.5 
	!  0.5       1   0.5 false
	!
	!  1.5      -1  -0.5 false
	!  1.5       0  -0.5 false
	!  1.5       1  -0.5 false
	!  1.5      -1   0.5 false
	!  1.5       0   0.5 false
	!  1.5       1   0.5 

	!order(i,:)=( outi,Q1i,Q2i,degQ1,degQ2,degstart,DegEnd)
	!do not store the data of degQ1=0 or degQ2=0.

	type(SymTensor) function QuantumFuseFast(B,NewQuanNum,Order,row)REsult(QuantumFuse)!ERROR
		class(SymTensor),intent(in)::B
		type(QuanNum),intent(in)::NewQuanNum
		type(Tensor),intent(in)::Order
		logical,intent(in)::row
		if(row)then
			call QuantumFuseRowFast(QuantumFuse,B,NewQuanNum,Order)
		else
			call QuantumFuseColFast(QuantumFuse,B,NewQuanNum,order)
		end if
		return
	end function
	
	type(SymTensor) function QuantumFuseDimFast(B,NewQuanDimen,Order,row)Result(QuantumFuseDim)!ERROR
		class(SymTensor),intent(in)::B
		type(SymDimension),intent(in)::NewQuanDimen
		type(QuanNum)::NewQuanNum
		type(Tensor),intent(in)::Order
		logical,intent(in)::row
		
		NewQuanNum=NewQuanDimen%QuantumNumber(1)
		if(row)then
			call QuantumFuseRowFast(QuantumFuseDim,B,NewQuanNum,Order)
		else
			call QuantumFuseColFast(QuantumFuseDim,B,NewQuanNum,order)
		end if
		if(NewQuanDimen%outNameFlag().eq.1)then
			if(row)then
				call QuantumFuseDim%setName(1,NewQuanDimen%outName(1))
			else
				call QuantumFuseDim%setName(QuantumFuseDim%getRank(),NewQuanDimen%outName(1))
			end if
		end if
		return
	end function
			
	
	character(len=max_len_of_char_in_TData) function newname_in_fuse(name1,name2) result(Res)
		character(len=*),intent(in)::name1,name2
		Res=(name1.subL.indexsymbol)+(name2.subL.indexsymbol)
		Res=Res+indexsymbol+(name1.subR.indexsymbol)+(name2.subR.indexsymbol)
		return
	end function

	subroutine QuantumFuseRowFast(A,B,NewQuanNum,Order)
		type(SymTensor),intent(inout)::A
		class(SymTensor),intent(in)::B
		type(QuanNum),intent(in)::NewQuanNum
		type(Tensor),intent(in)::Order
		type(SymDimension)::newDim
		integer::i,ia,jb,kb,rowStart,rowend,degB1,degB2,NewDeg
		integer::Adim1,Adim2,rank,lenorder
		integer::Bdim1,Bdim2,Bdim3,newblock
		integer,allocatable::deg(:),indices(:),AAlldim(:)
		logical::goon,flag
		call A%empty()
		rank=B%getRank()
		if(rank.eq.1)then
			A=B
			return
		end if
		lenorder=Order%dim(1)
		if(order%getRank().eq.1)lenorder=1
		if(rank.eq.2)then
			newDim=NewQuanNum
			if(check_same_name_flag) then
				if(B%getNameFlag().ne.0)then
					call newDim%setName(1,newname_in_fuse(B%getName(1),B%getName(2)))
				end if
			end if
			call A%allocate(newDim,B%getType())
			A%totalBlock=0
			Adim1=A%dim(1)
			Bdim1=B%dim(1)
			Bdim2=B%dim(2)
			do i=1,lenorder
				ia=Order%ii((/i,1/))
				jb=Order%ii((/i,2/))
				kb=Order%ii((/i,3/))
				degB1=Order%ii((/i,4/))
				degB2=Order%ii((/i,5/))
				rowStart=Order%ii((/i,6/))
				rowend=Order%ii((/i,7/))
				NewDeg=NewQuanNum%getDeg(ia)
				!B(jb,kb) store in A(ia)
				!The row_Start to row_end data is B(jb,kb)
				call paste_blockfast2(A%block,ia,Adim1,B%block,jb,kb,Bdim1,Bdim2,rowStart,rowEnd,NewDeg,newblock)
				A%totalBlock=A%totalBlock+newblock
			end do
			return
		end if
		newDim=NewQuanNum + (B%SymDimension.subdim.[3,rank])
		if(check_same_name_flag) then
			if(B%getNameFlag().ne.0)then
				call newDim%setName(1,newname_in_fuse(B%getName(1),B%getName(2)))
			end if
		end if
		Adim2=1
		do i=3,rank
			Adim2=Adim2*B%dim(i)
		end do
		call A%allocate(newDim,B%getType())
		A%totalBlock=0
		Adim1=A%dim(1)
		Bdim1=B%dim(1)
		Bdim2=B%dim(2)
		Bdim3=Adim2
		do i=1,lenorder
			ia=Order%ii((/i,1/))
			jb=Order%ii((/i,2/))
			kb=Order%ii((/i,3/))
			degB1=Order%ii((/i,4/))
			degB2=Order%ii((/i,5/))
			rowStart=Order%ii((/i,6/))
			rowend=Order%ii((/i,7/))
			NewDeg=NewQuanNum%getDeg(ia)
			!B(jb,kb,:) store in A(ia,:)
			!The row_Start to row_end data is B(jb,kb)
			call paste_blockfast(A%block,ia,Adim1,Adim2,B%block,jb,kb,Bdim1,Bdim2,Bdim3,rowStart,rowEnd,NewDeg,newblock)
			A%totalBlock=A%totalBlock+newblock
		end do
		return
	end subroutine

	!A(rowStart:rowEnd,:)=B(:,:)
	!inoput A should be rank 2 or 1
	!       B should be rank 2 
	!there are some sitiation
	!1. rowStart=1
	!       A=B
	! reset the dimension of A
	!2. rowStart= A%dim(1)+1
	!                / A \
	!                |---|
	!                \ B /
	!3.rowStart > A%dim(1)+1 and there are data in A
	!                / A \
	!                |---|
	!                | 0 |
	!                |---|
	!                \ B /
	!4.rowStart > A%dim(1)+1 and there is no data in A
	!                / 0 \
	!                |---|
	!                \ B /
	! when input A is nor rank=2 , the subroutine will fuse it
	!
	
	subroutine paste_blockfast2(A,ia,LDA1,B,jb,kb,LDB1,LDB2,rowStart,rowEnd,Deg,newblock)
		integer,intent(in)::ia,jb,kb,LDA1,LDB1,LDB2,rowStart,rowEnd,Deg
		type(Tensor),intent(inout)::A(LDA1)
		type(Tensor),intent(in)::B(LDB1,LDB2)
		integer,intent(inout)::newblock
		logical::flag
		integer::rankB,totalB
		newblock=0
		if(B(jb,kb)%getFlag())then
			rankB=B(jb,kb)%getRank()
			if(rankB.gt.2)then
				call writemess("ERROR in fuse Tensor, 1")
				write(*,*)rankB
				call error_stop()
			end if
			if(.not.A(ia)%getFlag())then
				newblock=newblock+1
				call A(ia)%allocate([Deg],B(jb,kb)%getType())
				call A(ia)%zero()
			endif
			totalB=B(jb,kb)%getTotalData()
			call A(ia)%setValue([rowStart,rowEnd],B(jb,kb).fuse.(/1,2/),[1,totalB])
		end if
		return
	end subroutine
	subroutine paste_blockfast(A,ia,LDA1,LDA2,B,jb,kb,LDB1,LDB2,LDB3,rowStart,rowEnd,Deg,newblock)
		integer,intent(in)::ia,jb,kb,LDA1,LDA2,LDB1,LDB2,LDB3,rowStart,rowEnd,Deg
		type(Tensor),intent(inout)::A(LDA1,LDA2)
		type(Tensor),intent(in)::B(LDB1,LDB2,LDB3)
		integer,intent(inout)::newblock
		integer::j,rankB,ii,Bdim1,Bdim2
		logical::flag
		type(dimension)::dimen
		type(Tensor)::temp
		newblock=0
		do j=1,LDA2
			if(B(jb,kb,j)%getFlag())then
				rankB=B(jb,kb,j)%getRank()
				temp=B(jb,kb,j).fuse.(/1,2/)
				call temp%fuse(2,temp%getRank())
				dimen=[Deg]+(B(jb,kb,j).subdim.[3,rankB])
				if(.not.A(ia,j)%getFlag())then
					newblock=newblock+1
					call A(ia,j)%allocate(dimen,B(jb,kb,j)%getType())
					call A(ia,j)%zero()
				endif
				call A(ia,j)%fuse(2,A(ia,j)%getRank())
				Bdim1=temp%dim(1)
				Bdim2=temp%dim(2)
				call A(ia,j)%setValue([rowStart,rowEnd],[1,Bdim2],temp,[1,Bdim1],[1,Bdim2])
				call A(ia,j)%resetDim(dimen)
			end if
		end do
		return
	end subroutine
	
	

	
	!order(i,:)=( outi,Q1i,Q2i,degQ1,degQ2,degstart,DegEnd)

	subroutine QuantumFuseColFast(A,B,NewQuanNum,order)
		type(SymTensor),intent(inout)::A
		type(SymTensor),intent(in)::B
		type(QuanNum),intent(in)::NewQuanNum
		type(Tensor),intent(in)::order
		type(SymDimension)::newDim
		integer::i,ia,jb,kb,colStart,colend,degB1,degB2,NewDeg
		integer::Adim1,Adim2,Bdim1,Bdim2,Bdim3
		integer::newblock,rank,lenorder
		logical::goon,flag
		integer,allocatable::deg(:),indices(:),AAlldim(:)
		call A%empty()
		rank=B%getRank()
		if(rank.eq.1)then
			A=B
			return
		end if
		lenorder=Order%dim(1)
		if(order%getRank().eq.1)lenorder=1
		if(rank.eq.2)then
			newDim=NewQuanNum
			if(check_same_name_flag) then
				if(B%getNameFlag().ne.0)then
					call newDim%setName(rank-1,newname_in_fuse(B%getName(rank-1),B%getName(rank)))
				end if
			end if
			call A%allocate(newDim,B%getType())
			A%totalBlock=0
			Adim1=A%dim(1)
			Bdim1=B%dim(1)
			Bdim2=B%dim(2)
			do i=1,lenorder
				ia=Order%ii((/i,1/))
				jb=Order%ii((/i,2/))
				kb=Order%ii((/i,3/))
				degB1=Order%ii((/i,4/))
				degB2=Order%ii((/i,5/))
				colStart=Order%ii((/i,6/))
				colend=Order%ii((/i,7/))
				NewDeg=NewQuanNum%getDeg(ia)
				call pasteCol_blockFast2(A%block,ia,Adim1,B%block,jb,kb,Bdim1,Bdim2,colStart,colEnd,NewDeg,newblock)
				A%totalBlock=A%totalBlock+newblock
			end do
			return
		end if
		newDim=(B%SymDimension.subdim.[1,rank-2]) + NewQuanNum
		if(check_same_name_flag) then
			if(B%getNameFlag().ne.0)then
				call newDim%setName(rank-1,newname_in_fuse(B%getName(rank-1),B%getName(rank)))
			end if
		end if
		Bdim1=B%dim(1)
		do i=2,rank-2
			Bdim1=Bdim1*B%dim(i)
		end do
		Bdim2=B%dim(rank-1)
		Bdim3=B%dim(rank)
		call A%allocate(newDim,B%getType())
		Adim1=Bdim1
		Adim2=A%dim(2)
		A%totalBlock=0
		do i=1,lenorder
			ia=Order%ii((/i,1/))
			jb=Order%ii((/i,2/))
			kb=Order%ii((/i,3/))
			degB1=Order%ii((/i,4/))
			degB2=Order%ii((/i,5/))
			colStart=Order%ii((/i,6/))
			colend=Order%ii((/i,7/))
			NewDeg=NewQuanNum%getDeg(ia)
			!B(jb,kb,:) store in A(ia,:)
			call pasteCol_blockfast(A%block,ia,Adim1,Adim2,B%block,jb,kb,Bdim1,Bdim2,Bdim3,colStart,colEnd,NewDeg,newblock)
			A%totalBlock=A%totalBlock+newblock
		end do
		return
	end subroutine


	!A(:,colStart:colEnd)=B(:,:)
	!inoput A should be rank 2 or 1
	!       B should be rank 2 or 1
	!there are some sitiation
	!1. colStart=1
	!       A=B
	!       reset the dimension of A
	!2. colStart= A%dim(2)+1
	!                /   |   \
	!                | A | B |
	!                \   |   /
	!3.colStart > A%dim(2)+1 and there are data in A
	!                /   |     |   \
	!                | A |  0  | B |
	!                \   |     |   /
	!4.colStart > A%dim(2)+1 and there is no data in A
	!                /   |   \
	!                | 0 | B |
	!                \   |   /
	!
	!


	subroutine pasteCol_blockfast2(A,ia,LDA1,B,jb,kb,LDB1,LDB2,colStart,colEnd,Deg,newblock)
		integer,intent(in)::ia,jb,kb,LDA1,LDB1,LDB2,colStart,colEnd,Deg
		type(Tensor),intent(inout)::A(LDA1)
		type(Tensor),intent(in)::B(LDB1,LDB2)
		integer,intent(inout)::newblock
		logical::flag
		integer::rankB,totalB
		type(dimension)::dimen
		newblock=0
		if(B(jb,kb)%getFlag())then
			rankB=B(jb,kb)%getRank()
			if(rankB.gt.2)then
				call writemess("ERROR in fuse Tensor, 2")
				write(*,*)rankB
				call error_stop()
			end if
			if(.not.A(ia)%getFlag())then
				newblock=newblock+1
				call A(ia)%allocate([Deg],B(jb,kb)%getType())
				call A(ia)%zero()
			endif
			totalB=B(jb,kb)%getTotalData()
			call A(ia)%setValue([colStart,colEnd],B(jb,kb).fuse.[1,2],[1,totalB])
		end if
		return
	end subroutine
	
	subroutine pasteCol_blockfast(A,ia,LDA1,LDA2,B,jb,kb,LDB1,LDB2,LDB3,colStart,colEnd,Deg,newblock)
		integer,intent(in)::ia,jb,kb,LDA1,LDA2,LDB1,LDB2,LDB3,colStart,colEnd,Deg
		type(Tensor),intent(inout)::A(LDA1,LDA2)
		type(Tensor),intent(in)::B(LDB1,LDB2,LDB3)
		integer,intent(inout)::newblock
		integer::i,rankB,Bdim1,Bdim2
		logical::flag
		type(dimension)::dimen
		type(TEnsor)::temp
		newblock=0
		do i=1,LDA1
			if(B(i,jb,kb)%getFlag())then
				rankB=B(i,jb,kb)%getRank()
				if(rankB.eq.1)then
					call writemess('ERROR in past col in SymTensor.f90')
					call error_stop
				end if
				temp=B(i,jb,kb).fuse.(/rankB-1,rankB/)
				if(temp%getRank().gt.2)call temp%fuse(1,temp%getRank()-1)
				dimen=(B(i,jb,kb).subdim.[1,rankB-2]) + [Deg]
				if(.not.A(i,ia)%getFlag())then
					newblock=newblock+1
					call A(i,ia)%allocate(dimen,B(i,jb,kb)%getType())
					call A(i,ia)%zero()
				endif
				Bdim1=temp%dim(1)
				Bdim2=temp%dim(2)
				call A(i,ia)%fuse(1,A(i,ia)%getRank()-1)
				call A(i,ia)%setValue([1,Bdim1],[colStart,colEnd],temp,[1,Bdim1],[1,Bdim2])
				call A(i,ia)%resetDim(dimen)
			end if
		end do
		return
	end subroutine



	!incase of this situation
	!                /   |   \
	!                | A | 0 |
	!                \   |   /
	!or
	!                / A \
	!                |---|
	!                \ 0 /

	subroutine checkAddDim(A,deg,row)
		type(Tensor),intent(inout)::A
		integer,intent(in)::deg(:)
		logical,intent(in)::row
		type(Tensor)::temp
		type(Dimension)::dimen
		integer::l,dim1,dim2,checkdim,i,rank
		rank=A%getRank()
		if(row)then
			dim1=A%Dim(1)
			if(deg(1).ne.dim1)then
				dim2=1
				dimen=(/deg(1)/)+ (A.subdim.[2,rank])
			!	dimen=(/deg(1)/)
				do i=2,rank
					dim2=dim2*A%dim(i)
			!		dimen=dimen+(A.subdim.i)
				end do
			
				l=deg(1)-dim1
				call temp%allocate((/l,dim2/),A%getType())
				call temp%zero()
				call A%fuse(2,rank)
				call A%paste(temp,.true.)
				call A%resetdim(dimen)
			end if
		else
			dim2=A%Dim(rank)
			if(deg(rank).ne.dim2)then
				dim1=A%dim(1)
				dimen=A.subdim.[1,rank-1]
			!	dimen=A.subdim.1
				do i=2,rank-1
					dim1=dim1*A%dim(i)
			!		dimen=dimen+(A.subdim.i)
				end do
				dimen=dimen+(/deg(rank)/)
				l=deg(rank)-dim2
				call temp%allocate((/dim1,l/),A%getType())
				call temp%zero()
				call A%fuse(1,rank-1)
				call A%paste(temp,.false.)
				call A%resetdim(dimen)
			end if
		end if
		do i=1,rank
			if(A%dim(i).ne.deg(i))then
				call writemess("ERROR in checkAddDim,SymTensor.f90")
				write(*,*)A%dim()
				write(*,*)row
				write(*,*)deg
				call error_stop
			end if
		end do
		return
	end subroutine
		
	

	!
	!
	!QuanNum1+QuanNum2=NewQuanNum
	!
	!read the add in Symmetry_tool in SymDimension.f90
	!order of codes for do
	! outQ=Q1+Q2 
	!  first order Q1 and then Q2 at lase outQ
	!Parity example:
	!order of codes for do
	! outQ      Q1  Q2 
	!  -1       -1  -1   false
	!  -1       +1  -1
	!  -1       -1  +1
	!  -1       +1  +1   false
	!  +1       -1  -1
	!  +1       +1  -1   false
	!  +1       -1  +1   false
	!  +1       +1  +1
	!
	!The U(1) symmetry
	!the rule for outQ Q1 Q2 are -1 1 1
	! outQ      Q1   Q2 
	! -1.5      -1  -0.5
	! -1.5       0  -0.5 false
	! -1.5       1  -0.5 false
	! -1.5      -1   0.5 false
	! -1.5       0   0.5 false
	! -1.5       1   0.5 false
	!
	! -0.5      -1  -0.5 false
	! -0.5       0  -0.5 
	! -0.5       1  -0.5 false
	! -0.5      -1   0.5 
	! -0.5       0   0.5 false
	! -0.5       1   0.5 false
	!
	!  0.5      -1  -0.5 false
	!  0.5       0  -0.5 false
	!  0.5       1  -0.5 
	!  0.5      -1   0.5 false
	!  0.5       0   0.5 
	!  0.5       1   0.5 false
	!
	!  1.5      -1  -0.5 false
	!  1.5       0  -0.5 false
	!  1.5       1  -0.5 false
	!  1.5      -1   0.5 false
	!  1.5       0   0.5 false
	!  1.5       1   0.5 

	
	type(SymTensor) function QuantumSplitFast(B,NewQ1,NewQ2,Order,row)Result(QuantumSplit)!ERROR
		class(SymTensor),intent(in)::B
		type(QuanNum),intent(in)::NewQ1,NewQ2
		type(Tensor),intent(in)::Order
		logical,intent(in)::row
		if(row)then
			call QuantumSplitRowFast(QuantumSplit,B,NewQ1,NewQ2,Order)
		else
			call QuantumSplitColFast(QuantumSplit,B,NewQ1,NewQ2,order)
		end if
		return
	end function
	type(SymTensor) function QuantumSplitDimFast(B,NewQuanDimen,Order,row)Result(QuantumSplitDim)!ERROR
		class(SymTensor),intent(in)::B
		type(SymDimension),intent(in)::NewQuanDimen
		type(QuanNum)::NewQ1,NewQ2
		type(Tensor),intent(in)::Order
		logical,intent(in)::row
		NewQ1=NewQuanDimen%QuantumNumber(1)
		NewQ2=NewQuanDimen%QuantumNumber(2)
		if(row)then
			call QuantumSplitRowFast(QuantumSplitDim,B,NewQ1,NewQ2,Order)
		else
			call QuantumSplitColFast(QuantumSplitDim,B,NewQ1,NewQ2,order)
		end if
		if(NewQuanDimen%outNameFlag().eq.1)then
			if(row)then
				call QuantumSplitDim%setName(1,NewQuanDimen%outName(1))
				call QuantumSplitDim%setName(2,NewQuanDimen%outName(2))
			else
				call QuantumSplitDim%setName(QuantumSplitDim%getRank()-1,NewQuanDimen%outName(1))
				call QuantumSplitDim%setName(QuantumSplitDim%getRank(),NewQuanDimen%outName(2))
			end if
		end if
		return
	end function
	
	
	
	subroutine QuantumSplitRowFast(A,B,NewQ1,NewQ2,order)
		type(SymTensor),intent(inout)::A
		type(SymTensor),intent(in)::B
		type(QuanNum),intent(in)::NewQ1,NewQ2
		type(Tensor),intent(in)::order
		type(QuanNum)::QuanNumber
		type(SymDimension)::newDim
		integer::i,ib,ja,ia,rowStart,rowend,deg(2),rank
		integer::Adim1,Adim2,Adim3,Bdim1,Bdim2
		integer::newblock,lenorder
		logical::goon
		call A%empty()
		rank=B%getRank()
		lenorder=Order%dim(1)
		if(order%getRank().eq.1)lenorder=1
		if(rank.eq.1)then
			newDim=(/NewQ1,NewQ2/)
			call A%allocate(newDim,B%getType())
			Adim1=A%dim(1)
			Adim2=A%dim(2)
			Bdim1=B%dim(1)
			A%totalBlock=0
			do i=1,lenorder
				ib=Order%ii((/i,1/))
				ia=Order%ii((/i,2/))
				ja=Order%ii((/i,3/))
				deg(1)=Order%ii((/i,4/))
				deg(2)=Order%ii((/i,5/))
				rowStart=Order%ii((/i,6/))
				rowend=Order%ii((/i,7/))
				!A(ia,ja) read  from B(ib), start with rowStart and with rowend
				!The row_Start to row_end data is B(jb,kb)
				call  split_blockFast2(A%block,ia,ja,Adim1,Adim2,B%block,ib,Bdim1,rowstart,rowend,deg,newblock)
				A%totalBlock=A%totalBlock+newblock
			end do
			return
		end if
		newDim=(/NewQ1,NewQ2/) 
		newDim=newDim + (B%SymDimension.subdim.[2,rank])
		Adim3=1
		do i=2,rank
			Adim3=Adim3*B%dim(i)
		end do
		call A%allocate(newDim,B%getType())
		Adim1=A%dim(1)
		Adim2=A%dim(2)
		Bdim1=B%dim(1)
		Bdim2=Adim3
		A%totalBlock=0
		do i=1,lenorder
			ib=Order%ii((/i,1/))
			ia=Order%ii((/i,2/))
			ja=Order%ii((/i,3/))
			deg(1)=Order%ii((/i,4/))
			deg(2)=Order%ii((/i,5/))
			rowStart=Order%ii((/i,6/))
			rowend=Order%ii((/i,7/))
			!A(ia,ja,:) read  from B(ib,:), start with rowStart and with rowend
			!The rowStart to rowend data is A(ia,ja,:)
			call  split_blockFast(A%block,ia,ja,Adim1,Adim2,Adim3,B%block,ib,Bdim1,Bdim2,rowstart,rowend,deg,newblock)
			A%totalBlock=A%totalBlock+newblock
		end do
		return
	end subroutine
	subroutine split_blockFast2(A,ia,ja,LDA1,LDA2,B,ib,LDB1,row_start,row_end,deg,newblock)
		integer,intent(in)::row_start,row_end,ia,ja,LDA1,LDA2,LDB1,ib,deg(2)
		integer,intent(inout)::newblock
		type(Tensor),intent(inout)::A(LDA1,LDA2)
		type(Tensor),intent(in)::B(LDB1)
		type(Dimension)::dimen
		integer::rankB,i
		newblock=0
		if(B(ib)%getFlag() )then
			rankB=B(ib)%getRank()
			if(rankB.ne.1)then
				call writemess('ERROR in split data in SymTensor.f90')
				call error_stop
			end if
			dimen=deg
			call A(ia,ja)%allocate(dimen,B(ib)%getType())
			call A(ia,ja)%fuse(1,2)
			call A(ia,ja)%setValue([1,A(ia,ja)%getTotalData()],B(ib),[row_start,row_end])
			call A(ia,ja)%resetDim(dimen)
			newblock=newblock+1
		end if
		return
	end subroutine
	subroutine split_blockFast(A,ia,ja,LDA1,LDA2,LDA3,B,ib,LDB1,LDB2,row_start,row_end,deg,newblock)
		integer,intent(in)::row_start,row_end,ia,ja,LDA1,LDA2,LDA3,LDB1,LDB2,ib,deg(2)
		integer,intent(inout)::newblock
		type(Tensor),intent(inout)::A(LDA1,LDA2,LDA3)
		type(Tensor),intent(in)::B(LDB1,LDB2)
		integer::k,rankB,i,Adim1,Adim2
		type(Dimension)::dimen
		newblock=0
		do k=1,LDA3
			if(B(ib,k)%getFlag() )then
				rankB=B(ib,k)%getRank()
				dimen=deg+(B(ib,k).subDim.[2,rankB])
				call A(ia,ja,k)%allocate(dimen,B(ib,k)%getType())
				call A(ia,ja,k)%fuse(1,2)
				call A(ia,ja,k)%fuse(2,A(ia,ja,k)%getRank())
				Adim1=A(ia,ja,k)%dim(1)
				Adim2=A(ia,ja,k)%dim(2)
				if(rankB.gt.2)then
					call A(ia,ja,k)%setValue([1,Adim1],[1,Adim2],B(ib,k).fuse.[2,rankB],[row_start,row_end],[1,Adim2])
				else
					call A(ia,ja,k)%setValue([1,Adim1],[1,Adim2],B(ib,k),[row_start,row_end],[1,Adim2])
				end if
				call A(ia,ja,k)%resetDim(dimen)
				newblock=newblock+1
			end if
		end do
		return
	end subroutine
	
	

	subroutine QuantumSplitColFast(A,B,NewQ1,NewQ2,order)
		type(SymTensor),intent(inout)::A
		type(SymTensor),intent(in)::B
		type(QuanNum),intent(in)::NewQ1,NewQ2
		type(Tensor),intent(in)::order
		type(QuanNum)::QuanNumber
		type(SymDimension)::newDim
		integer::i,ja,ka,jb,colStart,colend,deg(2),rank
		integer::Adim1,Adim2,Adim3,Bdim1,Bdim2
		integer::newblock,lenorder
		logical::goon
		rank=B%getRank()
		call A%empty()
		lenorder=Order%dim(1)
		if(order%getRank().eq.1)lenorder=1
		if(rank.eq.1)then
			newDim=(/NewQ1,NewQ2/)
			call A%allocate(newDim,B%getType())
			Adim1=A%dim(1)
			Adim2=A%dim(2)
			Bdim1=B%dim(1)
			A%totalBlock=0
			do i=1,lenorder
				jb=Order%ii((/i,1/))
				ja=Order%ii((/i,2/))
				ka=Order%ii((/i,3/))
				deg(1)=Order%ii((/i,4/))
				deg(2)=Order%ii((/i,5/))
				colStart=Order%ii((/i,6/))
				colEnd=Order%ii((/i,7/))
				call splitCol_blockFast2(A%block,ja,ka,Adim1,Adim2,B%block,jb,Bdim1,colstart,colend,deg,newblock)
				A%totalBlock=A%totalBlock+newblock
			end do
			return
		end if
		newDim=(B%SymDimension.subdim.[1,rank-1])+NewQ1+NewQ2
		Bdim1=B%dim(1)
		do i=2,rank-1
			Bdim1=Bdim1*B%dim(i)
		end do
		Bdim2=B%dim(rank)
		call A%allocate(newDim,B%getType())
		Adim1=Bdim1
		Adim2=A%dim(rank)
		Adim3=A%dim(rank+1)
		A%totalBlock=0
		do i=1,lenorder
			jb=Order%ii((/i,1/))
			ja=Order%ii((/i,2/))
			ka=Order%ii((/i,3/))
			deg(1)=Order%ii((/i,4/))
			deg(2)=Order%ii((/i,5/))
			colStart=Order%ii((/i,6/))
			colEnd=Order%ii((/i,7/))
			!A(:,ja,ka) read  from B(:,jb), start with colStart and with colEnd
			!The colStart to colEnd data is A(:,ja,ka) 
			call splitCol_blockFast(A%block,ja,ka,Adim1,Adim2,Adim3,B%block,jb,Bdim1,Bdim2,colStart,colEnd,deg,newblock)
			A%totalBlock=A%totalBlock+newblock
		end do
		return
	end subroutine
	subroutine splitCol_blockFast2(A,ja,ka,LDA1,LDA2,B,jb,LDB1,colstart,colend,deg,newblock)
		integer,intent(in)::colstart,colend,ja,ka,jb,LDA1,LDA2,LDB1,deg(2)
		type(Tensor),intent(inout)::A(LDA1,LDA2)
		type(Tensor),intent(in)::B(LDB1)
		integer,intent(inout)::newblock
		integer::rankB
		newblock=0
		if(B(jb)%getFlag())then
			rankB=B(jb)%getRank()
			if(rankB.ne.1)then
				call writemess('ERROR in split col1 data, in SymTensor.f90')
				call error_stop()
			end if
			call A(ja,ka)%allocate(deg,B(jb)%getType())
			call A(ja,ka)%fuse(1)
			call A(ja,ka)%setValue([1,A(ja,ka)%getTotalData()],B(jb),[colStart,colEnd])
			call A(ja,ka)%resetDim(deg)
			newblock=newblock+1
		end if
		return
	end subroutine
	!A(:,ja,ka) read  from B(:,jb),
	subroutine splitCol_blockFast(A,ja,ka,LDA1,LDA2,LDA3,B,jb,LDB1,LDB2,colStart,colEnd,deg,newblock)
		integer,intent(in)::ja,ka,jb,LDA1,LDA2,LDA3,LDB1,LDB2,colStart,colEnd,deg(2)
		type(Tensor),intent(inout)::A(LDA1,LDA2,LDA3)
		type(Tensor),intent(in)::B(LDB1,LDB2)
		integer,intent(inout)::newblock
		integer::i,rankB,k,Adim1,Adim2
		type(Dimension)::dimen
		newblock=0
		do i=1,LDB1
			if(B(i,jb)%getFlag())then
				rankB=B(i,jb)%getRank()
				if(rankB.eq.1)then
					call writemess('ERROR in split col data, in SymTensor.f90')
					call error_stop()
				end if
				dimen= (B(i,jb).subDim.[1,rankB-1]) +deg
				call A(i,ja,ka)%allocate(dimen,B(i,jb)%getType())
				call A(i,ja,ka)%fuse(rankB,rankB+1)
				if(rankB.gt.2)then
					call A(i,ja,ka)%fuse(1,rankB-1)
				end if
				if(A(i,ja,ka)%getRank().ne.2)then
					call writemess('ERROR in split col data 2, in SymTensor.f90')
					call error_stop()
				end if
				Adim1=A(i,ja,ka)%dim(1)
				Adim2=A(i,ja,ka)%dim(2)
				if(rankB.gt.2)then
					call A(i,ja,ka)%setValue([1,Adim1],[1,Adim2],B(i,jb).fuse.(/1,rankB-1/),[1,Adim1],[colStart,colEnd])
				else
					call A(i,ja,ka)%setValue([1,Adim1],[1,Adim2],B(i,jb),[1,Adim1],[colStart,colEnd])
				end if
				call A(i,ja,ka)%resetDim(dimen)
				newblock=newblock+1
			end if
		end do
		return
	end subroutine















	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                     Quantum Number   Fuse and split   use the symmetry rule   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************



	type(SymTensor) function SymFuseTensorRule(B,outDim,Order,row,NewRule)result(SymFuseTensor)
		class(SymTensor),intent(in)::B
		type(SymDimension),intent(inout)::outDim
		integer,intent(in)::NewRule
		type(Tensor),intent(inout)::Order
		logical,intent(in)::row
		integer::i,rank
		type(QuanNum)::NewQuanNum
		if(row)then
			outDim=B%SymDimension.subdim.[1,2]
			call fuseOrder(NewQuanNum,order,B%quantumnumber(1),B%quantumnumber(2),NewRule)
			SymFuseTensor=B%QuanFuse(NewQuanNum,Order,.true.)
		else
			rank=B%getRank()
			i=rank-1
			outDim=B%SymDimension.subdim.[i,rank]
			call fuseOrder(NewQuanNum,order,B%quantumnumber(i),B%quantumnumber(rank),NewRule)
			SymFuseTensor=B%QuanFuse(NewQuanNum,Order,.false.)
		end if
		return
	end function


	type(SymTensor) function SymFuseTensor(B,outDim,Order,row)
		class(SymTensor),intent(in)::B
		type(SymDimension),intent(inout)::outDim
		type(Tensor),intent(inout)::Order
		logical,optional,intent(in)::row
		integer::i,rank,rule1,rule2
		type(QuanNum)::NewQuanNum
		if(row)then
			rule1=B%getRule(1)
			rule2=B%getRule(2)
			if(rule1.ne.rule2)then
				call writemess(' The symmetry of the fusing legs are not the same')
				call error_stop
			end if

			outDim=B%SymDimension.subdim.[1,2]
			call fuseOrder(NewQuanNum,order,B%quantumnumber(1),B%quantumnumber(2),rule1)
			SymFuseTensor=B%QuanFuse(NewQuanNum,Order,.true.)
		else
			rank=B%getRank()
			i=rank-1
			rule1=B%getRule(i)
			rule2=B%getRule(rank)
			if(rule1.ne.rule2)then
				call writemess(' The symmetry of the fusing legs are not the same')
				call error_stop
			end if
			outDim=B%SymDimension.subdim.[i,rank]
			call fuseOrder(NewQuanNum,order,B%quantumnumber(i),B%quantumnumber(rank),rule1)
			SymFuseTensor=	B%QuanFuse(NewQuanNum,Order,.false.)
		end if
		return
	end function

	

	
	type(SymTensor) function SymFuseTensor2(B,row)result(SymFuseTensor)
		class(SymTensor),intent(in)::B
		type(Tensor)::Order
		logical,optional,intent(in)::row
		integer::i,rank,rule1,rule2
		type(QuanNum)::NewQuanNum
		if(row)then
			rule1=B%getRule(1)
			rule2=B%getRule(2)
			if(rule1.ne.rule2)then
				call writemess(' The symmetry of the fusing legs are not the same')
				call error_stop
			end if
			call fuseOrder(NewQuanNum,order,B%quantumnumber(1),B%quantumnumber(2),rule1)
			SymFuseTensor=B%QuanFuse(NewQuanNum,Order,.true.)
		else
			rank=B%getRank()
			i=rank-1
			rule1=B%getRule(i)
			rule2=B%getRule(rank)
			if(rule1.ne.rule2)then
				call writemess(' The symmetry of the fusing legs are not the same')
				call error_stop
			end if
			call fuseOrder(NewQuanNum,order,B%quantumnumber(i),B%quantumnumber(rank),rule1)
			SymFuseTensor=B%QuanFuse(NewQuanNum,Order,.false.)
		end if
		return
	end function

	type(SymTensor) function SymFuseTensorRule2(B,row,NewRule)result(SymFuseTensor)
		class(SymTensor),intent(in)::B
		type(Tensor)::Order
		integer,intent(in)::NewRule
		logical,intent(in)::row
		integer::i,rank
		type(QuanNum)::NewQuanNum
		if(row)then
			call fuseOrder(NewQuanNum,order,B%quantumnumber(1),B%quantumnumber(2),NewRule)
			SymFuseTensor=B%QuanFuse(NewQuanNum,Order,.true.)
		else
			rank=B%getRank()
			i=rank-1
			call fuseOrder(NewQuanNum,order,B%quantumnumber(i),B%quantumnumber(rank),NewRule)
			SymFuseTensor=B%QuanFuse(NewQuanNum,Order,.false.)
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

	type(SymTensor) function SymFuseTensor3(B,ith,outDimen,outOrder,orderinfo,row)result(T)
		class(SymTensor),intent(in)::B
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
				T=SymFuseTensor(B,dimen,order,.true.)
				outDimen=dimen
				outOrder=order
				if(order%getRank().eq.0)then
					call writemess('ERROR in SymFuseTensor3',-1)
					call error_stop
				end if
				if(order%getRank().eq.1)then
					call orderinfo%setValue(ith-1,1)
				else
					call orderinfo%setValue(ith-1,order%dim(1))
				end if
				do i=2,ith-1
					T=SymFuseTensor(T,dimen,order,.true.)
					outDimen=outDimen+dimen
					if(order%getRank().eq.1)then
						call orderinfo%setValue(ith-i,1)
					else
						call orderinfo%setValue(ith-i,order%dim(1))
					end if
					outOrder=order.Rpaste.outOrder
				end do
			else
				T=B
			end if
		else
			rank=B%getRank()
			rankV=rank-ith+1
			if(rankV.gt.1)then
				call orderinfo%allocate((/rankV-1/),'integer')
				T=SymFuseTensor(B,dimen,order,.false.)
				outDimen=dimen
				outOrder=order
				if(order%getRank().eq.0)then
					call writemess('ERROR in SymFuseTensor3',-1)
					call error_stop
				end if
				if(order%getRank().eq.1)then
					call orderinfo%setValue(rankV-1,1)
				else
					call orderinfo%setValue(rankV-1,order%dim(1))
				end if
				do i=2,rankV-1
					T=SymFuseTensor(T,dimen,order,.false.)
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
	type(SymTensor) function SymFuseTensorRule3(B,ith,outDimen,outOrder,orderinfo,row,NewRule)result(T)
		class(SymTensor),intent(in)::B
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
				T=SymFuseTensorRule(B,dimen,order,.true.,NewRule)
				outDimen=dimen
				outOrder=order
				if(order%getRank().eq.0)then
					call writemess('ERROR in SymFuseTensor3',-1)
					call error_stop
				end if
				if(order%getRank().eq.1)then
					call orderinfo%setValue(ith-1,1)
				else
					call orderinfo%setValue(ith-1,order%dim(1))
				end if
				do i=2,ith-1
					T=SymFuseTensorRule(T,dimen,order,.true.,NewRule)
					outDimen=outDimen+dimen
					if(order%getRank().eq.1)then
						call orderinfo%setValue(ith-i,1)
					else
						call orderinfo%setValue(ith-i,order%dim(1))
					end if
					outOrder=order.Rpaste.outOrder
				end do
			else
				T=B
			end if
		else
			rank=B%getRank()
			rankV=rank-ith+1
			if(rankV.gt.1)then
				call orderinfo%allocate((/rankV-1/),'integer')
				T=SymFuseTensorRule(B,dimen,order,.false.,NewRule)
				outDimen=dimen
				outOrder=order
				if(order%getRank().eq.0)then
					call writemess('ERROR in SymFuseTensor3',-1)
					call error_stop
				end if
				if(order%getRank().eq.1)then
					call orderinfo%setValue(rankV-1,1)
				else
					call orderinfo%setValue(rankV-1,order%dim(1))
				end if
				do i=2,rankV-1
					T=SymFuseTensorRule(T,dimen,order,.false.,NewRule)
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
	
	type(SymTensor) function SymFuseTensor4(B,ith,row)result(T)
		class(SymTensor),intent(in)::B
		integer,intent(in)::ith
		logical,intent(in)::row
		integer::i,rank,j,rankV
		integer,allocatable::index(:)
		if(row)then	
			if(ith.gt.1)then
				T=SymFuseTensor2(B,.true.)
				do i=2,ith-1
					T=SymFuseTensor2(T,.true.)
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
				T=SymFuseTensor2(T,.true.)
				do i=2,rankV-1
					T=SymFuseTensor2(T,.true.)
				end do
				call T%backward(1)
			else if(rankV.eq.2)then
				T=SymFuseTensor2(B,.false.)
			else
				T=B
			end if
		end if
		
		return
	end function
	type(SymTensor) function SymFuseTensorRule4(B,ith,row,NewRule)result(T)
		class(SymTensor),intent(in)::B
		integer,intent(in)::ith,NewRule
		logical,intent(in)::row
		integer::i,rank,j,rankV
		integer,allocatable::index(:)
		if(row)then	
			if(ith.gt.1)then
				T=SymFuseTensorRule2(B,.true.,NewRule)
				do i=2,ith-1
					T=SymFuseTensorRule2(T,.true.,NewRule)
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
				T=SymFuseTensorRule2(T,.true.,NewRule)
				do i=2,rankV-1
					T=SymFuseTensorRule2(T,.true.,NewRule)
				end do
				call T%backward(1)
			else if(rankV.eq.2)then
				T=SymFuseTensorRule2(B,.false.,NewRule)
			else
				T=B
			end if
		end if
		
		return
	end function
	
	
	
	type(SymTensor) function FermiSplitSymTensor(B,NewQuanDimen,Order,row)
		class(SymTensor),intent(in)::B
		type(SymDimension),intent(in)::NewQuanDimen
		type(QuanNum)::NewQ1,NewQ2
		type(Tensor),intent(in)::Order
		logical,optional,intent(in)::row
		integer::i
		NewQ1=NewQuanDimen%QuantumNumber(1)
		NewQ2=NewQuanDimen%QuantumNumber(2)
		if(row)then
			FermiSplitSymTensor=B%QuanSplit(NewQ1,NewQ2,Order,.true.)
		else
			i=B%getrank()
			FermiSplitSymTensor=B%QuanSplit(NewQ1,NewQ2,Order,.false.)
		end if
		if(NewQuanDimen%outNameFlag().eq.1)then
			if(row)then
				call FermiSplitSymTensor%setName(1,NewQuanDimen%outName(1))
				call FermiSplitSymTensor%setName(2,NewQuanDimen%outName(2))
			else
				call FermiSplitSymTensor%setName(FermiSplitSymTensor%getRank()-1,NewQuanDimen%outName(1))
				call FermiSplitSymTensor%setName(FermiSplitSymTensor%getRank(),NewQuanDimen%outName(2))
			end if
		end if
		return
	end function
	type(SymTensor) function FermiSplitSymTensor2(B,inDimen,inOrder,orderinfo,row)result(T)
		class(SymTensor),intent(in)::B
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
			call writemess('ERROR in Split SymTensor, input parameter error',-1)
			call error_stop()
		end if
		orderindex=1
		dimen=inDimen.subdim.[rank-1,rank]
		istart=1
		iend=orderinfo%ii(orderindex)
		order=inOrder%subTensor((/-1,istart,iend/))
		istart=iend+1
		T=FermiSplitSymTensor(B,dimen,Order,row)
		do i=rank-2,2,-2
			j=i-1
			orderindex=orderindex+1
			dimen=inDimen.subdim.[i-1,i]
			iend=istart+orderinfo%ii(orderindex)-1
			order=inOrder%subTensor((/-1,istart,iend/))
			istart=iend+1
			T=FermiSplitSymTensor(T,dimen,order,row)
		end do
		return
	end function




	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                      + - * /   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************



	!**************************************************************************

	type(SymTensor) function divide_real8(T1,num) result(Res)
		type(SymTensor),intent(in) :: T1
		real(kind=8),intent(in) ::   num
		real*8::oneOverNum
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (/) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),3)
		oneOverNum=1d0/num
		call Res%allocate(T1,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*oneOverNum
			end if
		end do
		return
	end function
	type(SymTensor) function divide_real4(T1,num) result(Res)
		type(SymTensor),intent(in) :: T1
		real(kind=4),intent(in) ::   num
		integer::i,classtype
		real*8::oneOverNum
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (/) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),3)
		call Res%allocate(T1,classtype)
		oneOverNum=1d0/num
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*oneOverNum
			end if
		end do
		return
	end function

	type(SymTensor) function divide_int(T1,num) result(Res)
		type(SymTensor),intent(in) :: T1
		integer,intent(in) ::   num
		integer::i,classtype
		real*8::oneOverNum
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (/) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),3)
		call Res%allocate(T1,classtype)
		oneOverNum=1d0/dble(num)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*oneOverNum
			end if
		end do
		return
	end function
	type(SymTensor) function divide_Tensor(T1,num) result(Res)
		type(SymTensor),intent(in) :: T1
		type(Tensor),intent(in) ::   num
		integer::i,classtype
		real*8::oneOverNum
		if(num%getTotalData().ne.1)then
			call writemess('ERROR in SymTensor/Tensor, Tensor should be regard as a value not a Tensor')
			call error_stop()
		end if
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (/) ', -1)
			call error_stop
			return
		end if
		classtype=select_type_in_add_minu(T1%getType(),3)
		oneOverNum=1d0/(num%di(1))
		call Res%allocate(T1,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*oneOverNum
			end if
		end do
		return
	end function
	
	
	type(SymTensor) function multiply_number_real8(T1,num) result(Res)
		type(SymTensor),intent(in) :: T1
		real(kind=8),intent(in) ::   num
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (*) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),3)
		call Res%allocate(T1,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*num
			end if
		end do
		return
	end function

	type(SymTensor) function multiply_number_real8_(num,T1) result(Res)
		type(SymTensor),intent(in) :: T1
		real(kind=8),intent(in) ::   num
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (*) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),3)
		call Res%allocate(T1,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*num
			end if
		end do
		return
	end function

	type(SymTensor) function multiply_number_real4(T1,num) result(Res)
		type(SymTensor),intent(in) :: T1
		real(kind=4),intent(in) ::   num
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (*) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),2)
		call Res%allocate(T1,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*num
			end if
		end do
		return
	end function

	type(SymTensor) function multiply_number_real4_(num,T1) result(Res)
		type(SymTensor),intent(in) :: T1
		real(kind=4),intent(in) ::   num
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (*) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),2)
		call Res%allocate(T1,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*num
			end if
		end do
		return
	end function


	type(SymTensor) function multiply_number_com4(T1,num) result(Res)
		type(SymTensor),intent(in) :: T1
		complex(kind=4),intent(in) ::   num
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (*) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),4)
		call Res%allocate(T1,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*num
			end if
		end do
		return
	end function
	type(SymTensor) function multiply_number_com4_(num,T1) result(Res)
		type(SymTensor),intent(in) :: T1
		complex(kind=4),intent(in) ::   num
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (*) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),4)
		call Res%allocate(T1,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*num
			end if
		end do
		return
	end function

	type(SymTensor) function multiply_number_com8(T1,num) result(Res)
		type(SymTensor),intent(in) :: T1
		complex(kind=8),intent(in) ::   num
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (*) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),5)
		call Res%allocate(T1,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*num
			end if
		end do
		return
	end function
	type(SymTensor) function multiply_number_com8_(num,T1) result(Res)
		type(SymTensor),intent(in) :: T1
		complex(kind=8),intent(in) ::   num
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in SymTensor, (*) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),5)
		call Res%allocate(T1,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				Res%block(i)=T1%block(i)*num
			end if
		end do
		return
	end function
	
	type(SymTensor) function add_SymTensor(T1,T2) result(Res)
		type(SymTensor),intent(in) :: T1,T2
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in the first SymTensor, (+) ', -1)
			call error_stop
		end if
		if(.not.T2%getflag())then
			call writemess('There is no data in the second SymTensor, (+) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),T2%getType())
		call Res%allocate(T1%SymDimension,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				if(T2%block(i)%getFlag())then
					Res%block(i)=T1%block(i)+T2%block(i)
				else
					Res%block(i)=T1%block(i)
				end if
				Res%totalBlock=Res%totalBlock+1
			else 
				if(T2%block(i)%getFlag())then
					Res%block(i)=T2%block(i)
					Res%totalBlock=Res%totalBlock+1
				end if
			end if
		end do
		return
	end function
	
	type(SymTensor) function minu_SymTensor(T1,T2) result(Res)
		type(SymTensor),intent(in) :: T1,T2
		integer::i,classtype
		if(.not.T1%getflag())then
			call writemess('There is no data in the first SymTensor, (-) ', -1)
			call error_stop
		end if
		if(.not.T2%getflag())then
			call writemess('There is no data in the second SymTensor, (-) ', -1)
			call error_stop
		end if
		classtype=select_type_in_add_minu(T1%getType(),T2%getType())
		call Res%allocate(T1%SymDimension,classtype)
		do i=1,T1%getTotalData()
			if(T1%block(i)%getFlag())then
				if(T2%block(i)%getFlag())then
					Res%block(i)=T1%block(i)-T2%block(i)
				else
					Res%block(i)=T1%block(i)
				end if
				Res%totalBlock=Res%totalBlock+1
			else 
				if(T2%block(i)%getFlag())then
					Res%block(i)=(-1)*T2%block(i)
					Res%totalBlock=Res%totalBlock+1
				end if
			end if
		end do
		return
	end function


	subroutine SZGEMM(Row,Col,M,N,K,A,LDRA,LDCA,B,LDRB,LDCB,C,LDRC,LDCC,totalblock)!ZGEMM for block data,Not finished but it could run
	  	INTEGER,intent(in)::M,N,k,LDRA,LDCA,LDRB,LDCB,LDRC,LDCC
	  	integer,intent(in)::Row(3),Col(3)
	  	type(Tensor),target,intent(in)::A(ldra,*),B(ldrb,*)
	  	type(Tensor),target,intent(inout)::C(ldrc,*)
	  	integer,intent(inout)::totalblock
	  	integer::i,j,l,rowA,colA,rowB,colB,rowC,colC
	  	integer::RA,CA,RB,CB,RC,CC
	  	type(Tensor),pointer::Cp,Ap,Bp
	  	totalblock=0
	  	rowA=Row(1)-1
	  	rowB=Row(2)-1
	  	rowC=Row(3)-1
	  	colA=Col(1)-1
	  	colB=Col(2)-1
	  	colC=Col(3)-1
	  	do j=1,N!do1
	  		CB=colB+j
	  		CC=ColC+j
	  		do i=1,M!do2
	  			RA=rowA+i
	  			RC=rowC+i
	  			Cp=>C(RC,CC)
	  			do l=1,k!do3
	  				CA=colA+l
	  				RB=rowB+l
	  				Ap=>A(RA,CA)
	  				Bp=>B(RB,CB)
	  				if(Ap%getFlag().and.Bp%getFlag())then
	  					call Cp%ProductTensorRoutine(Ap,Bp,1,1)
	  				end if
	  			end do!do3
	  			if(Cp%getflag()) totalblock=totalblock+1
	  		end do!do2
	  	end do!do1
	end subroutine


	type(Tensor) FUNCTION Szdotu(N,ZX,INCX,ZY,INCY)!zdotu for block data, not conjugating the first vector
		integer,intent(in)::INCX,INCY,N
		type(Tensor),intent(in)::ZX(*),ZY(*)
		integer::i,ix,iy
		Szdotu=(0.,0.)
		if(n.le.0) return
		if(incx.eq.1 .and. incy.eq.1) then
			do i=1,n
				if(zx(i)%getflag().and.zy(i)%getFlag()) then
					Szdotu = Szdotu + ( zx(i) .dot. zy(i) )
				end if
			end do
		else
			ix=1
			iy=1
			IF (incx.LT.0) ix = (-n+1)*incx + 1
			IF (incy.LT.0) iy = (-n+1)*incy + 1
			DO i = 1,n
				if(zx(ix)%getflag().and.zy(iy)%getFlag()) then
					Szdotu = Szdotu + (zx(ix) .dot. zy(iy))
				end if
				ix = ix + incx
				iy = iy + incy
			end do
		end if
		return
	end FUNCTION


	subroutine ProductTensorRoutine(Res,T1,T2)
		class(SymTensor),target,intent(inout) :: Res
		type(SymTensor),target,intent(in) :: T1,T2
		integer::rank1,rank2,flag,T1m,T1n,T2m,T2n,T1l,T2l
		type(SymDimension)::D1,D2,newD
		integer::i,classtype,row(3),col(3),totalblock
		logical::NoBlock
		class(SymTensor),pointer::Resp,T1p,T2p
		row=1
		col=1
		if(.not.T1%getflag()) then
			call writemess("ERROR in (*)",-1)
			call writemess("Tensor is no data in the first Tensor",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		if(.not.T2%getflag()) then
			call writemess("ERROR in (*)",-1)
			call writemess("Tensor is no data in the second Tensor",-1)
			call writemess("stop",-1)
			call error_stop()
		end if	

		Resp=>Res
		T1p=>T1
		T2p=>T2
		if(associated(Resp,T1p).or.associated(Resp,T2p))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call Res%ProductTensorRoutine(Res,T1,T2,alpha,beta)')
			call writemess('Res and T1, or Res and T2, can not be a same variable')
			call error_stop
		end if
		Resp=>null()
		T1p=>null()
		T2p=>null()
		call Res%empty()

		NoBlock=(T1%getTotalBlock().eq.0).or.(T2%getTotalBlock().eq.0)
		
		rank1=T1%getRank()
		rank2=T2%getRank()
		D1=T1%SymDimension
		D2=T2%SymDimension
		
		if((rank1.eq.1).and.(rank2.eq.1)) then
			flag=1
		else if((rank1.eq.1).and.(rank2.ge.2)) then
			flag=2
		else if((rank1.ge.2).and.(rank2.eq.1)) then
			flag=3	
		else if((rank1.ge.2).and.(rank2.ge.2)) then
			flag=4
		else
			call writemess("ERROR in ProductTensor"+rank1+','+rank2,-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T1%getType(),T2%getType())
		select case (flag)
			case (1)
				T1m=T1.dim.1
				T2n=T2.dim.1
				if(T1m.ne.T2n) then
					call writemess("ERROR in ProductTensor,case 1,stop",-1)
					call error_stop()
				end if
				call Res%allocate((/1/),classtype)
				if(NoBlock)then
					Res%totalblock=0
				else
					Res%block(1)=Szdotu(T1m,T1%Block,1,T2%block,1)
					Res%totalblock=1	
				end if
			case (2)
				newD=D2.sub.[2,rank2]
				D2=D2%fuseIndex(2,rank2)
				T1m=1
				T1n=D1.i.1
				T2m=D2.i.1
				T2n=D2.i.2
				call Res%allocate(newD,classtype)
				if(NoBlock)then
					Res%totalblock=0
				else
					call SZGEMM(row,col,T1m,T2n,T1n,T1%BLOCK,T1m,T1n,&
						T2%BLOCK,T2m,T2n,Res%BLOCK,T1m,T2n,totalblock)
					Res%totalblock=totalblock
				end if
		case (3)
				newD=D1.sub.[1,rank1-1]
				D1=D1%fuseIndex(1,rank1-2)
				T1m=D1.i.1
				T1n=D1.i.2
				T2m=D2.i.1
				T2n=1
				if((D2.i.1) .ne. T1n) then
					call writemess("ERROR in ProductTensor,case 3,stop",-1)
					call error_stop()
				end if
				call Res%allocate(newD,classtype)
				if(NoBlock)then
					Res%totalblock=0
				else
					call SZGEMM(row,col,T1m,T2n,T1n,T1%BLOCK,T1m,T1n,&
						T2%BLOCK,T2m,T2n,Res%BLOCK,T1m,T2n,totalblock)
					Res%totalblock=totalblock
				end if
		case (4)
				newD=D1.sub.[1,rank1-1]
				D1=D1%fuseIndex(1,rank1-2)
				newD=newD+(D2.sub.[2,rank2])
				D2=D2%fuseIndex(2,rank2)
				if((D1.i.2).ne.(D2.i.1)) then
					call writemess("error ProductTensor,dimension",-1)
					call error_stop()
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				T2m=D2.i.1
				T2n=D2.i.2
				call Res%allocate(newD,classtype)
				if(NoBlock)then
					Res%totalblock=0
				else
					call SZGEMM(row,col,T1m,T2n,T1n,T1%BLOCK,T1m,T1n,&
						T2%BLOCK,T2m,T2n,Res%BLOCK,T1m,T2n,totalblock)
					Res%totalblock=totalblock
				end if

		case default 
			call writemess("ERROR in ProductTensor,no such data",-1)
			call error_stop()
		end 	select
		if(ProductTensor_output_check_flag)call Res%checkClassType()
		return
	end subroutine

	subroutine ProductTensor_output_check(T1,T2)
		type(SymTensor),intent(in) :: T1,T2
		type(SymDimension)::dim1,dim2
		integer::i
		dim1=T1%SymDimension.subdim.(T1%getRank())
		dim2=T2%SymDimension.subdim.1
		call dim1%split()
		call dim2%split()
		do i=1,dim1%getRank()
			if(abs(dim1%GetQN(i,1)-dim2%GetQN(i,1)) .gt.1e-7)then
				open(unit=4321,file='_ERROR_PRODUCT'+output_ProID+'dat',status='replace')
				write(4321,*)'ERROR when doing product(*),the dimension error'
				write(4321,*)'The quantum number of the'+(' '+i)+'th dimension are'
				write(4321,*)dim1%GetQN(i,1)+','+dim2%GetQN(i,1)
				write(4321,*)'The Names of the'+(' '+i)+'th dimension are'
				write(4321,*)dim1%getName(i)+','+dim1%getName(i)
				call dim1%print(4321)
				call dim2%print(4321)
				call error_stop()
			end if
		end do
		return
	end subroutine


	type(SymTensor) function ProductTensor (T1,T2)
		type(SymTensor),intent(in) :: T1,T2
		integer::rank1,rank2,flag,T1m,T1n,T2m,T2n,T1l,T2l
		type(SymDimension)::D1,D2,newD
		integer::i,classtype,row(3),col(3),totalblock
		logical::NoBlock
		row=1
		col=1
		if(.not.T1%getflag()) then
			call writemess("ERROR in (*)",-1)
			call writemess("Tensor is no data in the first Tensor",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		if(.not.T2%getflag()) then
			call writemess("ERROR in (*)",-1)
			call writemess("Tensor is no data in the second Tensor",-1)
			call writemess("stop",-1)
			call error_stop()
		end if	
		rank1=T1%getRank()
		rank2=T2%getRank()
		D1=T1%SymDimension
		D2=T2%SymDimension
		if((T2%dim(1).lt.2).and.ProductTensor_output_check_flag)then
			call  ProductTensor_output_check(T1,T2)
		end if
		if((rank1.eq.1).and.(rank2.eq.1)) then
			flag=1
		else if((rank1.eq.1).and.(rank2.ge.2)) then
			flag=2
		else if((rank1.ge.2).and.(rank2.eq.1)) then
			flag=3	
		else if((rank1.ge.2).and.(rank2.ge.2)) then
			flag=4
		else
			write(*,*)"ERROR in ProductTensor",rank1,rank2
			call error_stop()
		end if
		NoBlock=(T1%getTotalBlock().eq.0).or.(T2%getTotalBlock().eq.0)
		classtype=select_type_in_add_minu(T1%getType(),T2%getType())
		select case (flag)
			case (1)
				T1m=T1.dim.1
				T2n=T2.dim.1
				if(T1m.ne.T2n) then
					call writemess("ERROR in ProductTensor,case 1,stop",-1)
					call error_stop()
				end if
				call ProductTensor%allocate((/1/),classtype)
				if(NoBlock)then
					ProductTensor%totalblock=0
				else
					ProductTensor%block(1)=Szdotu(T1m,T1%Block,1,T2%block,1)
					ProductTensor%totalblock=1
				end if
			case (2)
				newD=D2.sub.[2,rank2]
				D2=D2%fuseIndex(2,rank2)
				T1m=1
				T1n=D1.i.1
				T2m=D2.i.1
				T2n=D2.i.2
				call ProductTensor%allocate(newD,classtype)
				if(NoBlock)then
					ProductTensor%totalblock=0
				else
					call SZGEMM(row,col,T1m,T2n,T1n,T1%BLOCK,T1m,T1n,&
						T2%BLOCK,T2m,T2n,ProductTensor%BLOCK,T1m,T2n,totalblock)
					ProductTensor%totalblock=totalblock
				end if
		case (3)
				newD=D1.sub.[1,rank1-1]
				D1=D1%fuseIndex(1,rank1-2)
				T1m=D1.i.1
				T1n=D1.i.2
				T2m=D2.i.1
				T2n=1
				if((D2.i.1) .ne. T1n) then
					call writemess("ERROR in ProductTensor,case 3,stop",-1)
					call error_stop()
				end if
				call ProductTensor%allocate(newD,classtype)
				if(NoBlock)then
					ProductTensor%totalblock=0
				else
					call SZGEMM(row,col,T1m,T2n,T1n,T1%BLOCK,T1m,T1n,&
						T2%BLOCK,T2m,T2n,ProductTensor%BLOCK,T1m,T2n,totalblock)
					ProductTensor%totalblock=totalblock
				end if
		case (4)
				newD=D1.sub.[1,rank1-1]
				D1=D1%fuseIndex(1,rank1-2)
				newD=newD+(D2.sub.[2,rank2])
				D2=D2%fuseIndex(2,rank2)
				if((D1.i.2).ne.(D2.i.1)) then
					call writemess("error ProductTensor,dimension",-1)
					call error_stop()
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				T2m=D2.i.1
				T2n=D2.i.2
				call ProductTensor%allocate(newD,classtype)
				if(NoBlock)then
					ProductTensor%totalblock=0
				else
					call SZGEMM(row,col,T1m,T2n,T1n,T1%BLOCK,T1m,T1n,&
						T2%BLOCK,T2m,T2n,ProductTensor%BLOCK,T1m,T2n,totalblock)
					ProductTensor%totalblock=totalblock
				end if
		case default 
			call writemess("ERROR in ProductTensor,no such data",-1)
			call error_stop()
		end 	select
		if(ProductTensor_output_check_flag)call ProductTensor%checkClassType()
		return
	end function


	integer function select_type_in_add_minu(classtype1,classtype2)result(Res)
		integer,intent(in)::classtype1,classtype2
		integer::flag
		if(classtype1.eq.classtype2)then
			Res=classtype1
			return
		end if
		flag=10*classtype1+classtype2
		select case(flag)
		!int+classtype ---> classtype
			case (12)!int,real4
				Res=2
			case (13)!int,real8
				Res=3
			case (14)!int,compelx(kind=4)
				Res=4
			case (15)!int,compelx(kind=8)
				Res=5
			case (17)!int,character
				Res=7
		!real4+classtype ---> max{2,classtype}
			case (21)!real(kind=4),int
				Res=2
			case (23)!real(kind=4),real8
				Res=3
			case (24)!real(kind=4),compelx(kind=4)
				Res=4
			case (25)!real(kind=4),compelx(kind=8)
				Res=5
			case (27)!real(kind=4),character
				Res=7
		!depend on 		classtype
			case (31)!real(kind=8),int
				Res=3
			case (32)!real(kind=8),real4
				Res=3
			case (34)!real(kind=8),compelx(kind=4)
				Res=5
			case (35)!real(kind=8),compelx(kind=8)
				Res=5
			case (37)!real(kind=8),character
				Res=7
		!depend on 		classtype		
			case (41)!compelx(kind=4),int
				Res=4
			case (42)!compelx(kind=4),real4
				Res=4
			case (43)!compelx(kind=4),real8
				Res=5
			case (45)!compelx(kind=4),compelx(kind=8)
				Res=5
			case (47)!compelx(kind=4),character 
				Res=7
		!complex*16+classtype ---> max{5,classtype}	
			case (51)!compelx(kind=8),int
				Res=5
			case (52)!compelx(kind=8),real4
				Res=5
			case (53)!compelx(kind=8),real8
				Res=5
			case (54)!compelx(kind=8),compelx(kind=4)
				Res=5
			case (57)!compelx(kind=8),character
				Res=7
		!character+classtype ---> max{7,classtype}		
			case (71)!character,int
				Res=7
			case (72)!character,real4
				Res=7
			case (73)!character),real8
				Res=7
			case (74)!character,real4
				Res=7
			case (75)!character),real8
				Res=7
			case (76)!character,logical 
				Res=7
			case default
				write(*,*)"ERROR, no such type in select"
				write(*,*)"flag=",flag
				call error_stop
		end select
		return
	end function







	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                     copy to Tensor   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************

	subroutine SymTensorToTensor(Res,SymT)
		type(Tensor),intent(inout)::Res
		type(SymTensor),intent(in)::SymT
		type(SymDimension)::Dimen
		integer,allocatable::deg(:),inde(:),alldim(:),minmaxindex(:,:)
		integer::Degi,i,rank,classtype,ii,dim1,dim2
		logical::goon
		type(Tensor)::temp1,temp2
		if(.not.Dimen%if_original_dim())then
			call writemess("ERROR in Tensor=SymTensor ")
			call error_stop()
		end if
		if(SymT%getTotalData().eq.1)then
			Res=SymT%Block(1)
			return
		end if
		rank=SymT%getRank()
		classtype=SymT%getType()
		call Res%empty()
		call Res%settype(classtype)
			allocate(deg(rank))
			do i=1,rank
				deg(i)=sum(SymT%GetDeg(i))
			end do
			if(product(deg).eq.0)return
			call Res%allocate(deg,SymT%getType())
			call Res%zero()
			allocate(inde(rank))
			allocate(alldim(rank))
			allocate(minmaxindex(2,rank))
			alldim=SymT%dim()
			do i=1,SymT%getTotalData()
				if(SymT%Block(i)%getFlag())then
					call IndesToaddress(alldim,inde,i)
					minmaxindex=SymT%NonSymIndex(inde)
					call StoreData(Res,SymT%Block(i),minmaxindex(1,:),minmaxindex(2,:))
				end if
			end do
			
		if(SymT%outNameFlag().eq.0) return
		if(SymT%outNameFlag().eq.1)then
		 do i=1,rank
		 	call Res%setName(i,SymT%outName(i))
		 end do
		 return
		end if
		if(SymT%outNameFlag().eq.2)then
		 call writemess('ERROR, Do no finished the part of  integer name in Tensor=SymTensor')
		 call error_stop()
		end if
	end subroutine
	
	!inoutT(indexstart:indexend)=inoutT(minindeT:maxTinde)
	!or
	!inoutT(indexstart:indexend)=inoutT

	subroutine StoreData(inoutT,T,indexstart,indexend,minindeT_,maxTinde)
		type(Tensor),intent(inout)::inoutT
		type(Tensor),intent(in)::T
		integer,intent(in)::indexstart(:),indexend(:)
		integer,optional,intent(in)::minindeT_(:),maxTinde(:)
		integer,allocatable::indeinoutT(:),indeT(:),dimT(:),minindeT(:)
		logical::goon,goon2
		allocate(indeinoutT(size(indexstart)))
		allocate(indeT(size(indexstart)))
		allocate(dimT(size(indexstart)))
		allocate(minindeT(size(indexstart)))
		indeinoutT=indexstart
		if(present(minindeT_))then
			minindeT=minindeT_
			dimT=maxTinde
		else
			minindeT=1
			dimT=T%dim()
		end if
		indeT=minindeT
		goon=.true.
		do while(goon)
			call inoutT%setValue(indeinoutT,T%i(indeT))
			goon2=inde_counter(indeT,minindeT,dimT,1)
			goon=inde_counter(indeinoutT,indexstart,indexend,1)
			if(goon.neqv.goon2)then
				call writemess("ERROR in StoreData,SymTensor")
				write(*,*)indeT,"|",minindeT,"|",dimT
				write(*,*)indeinoutT,"|",indexstart,"|",indexend
				call error_Stop()
			end if
		end do
		return
	end subroutine
	



	
	
	subroutine TensorToSymTensor(Res,T)
		type(SymTensor),intent(inout)::Res
		type(Tensor),intent(in)::T
		integer,allocatable::inde(:),alldim(:),minmaxindex(:,:),indexstart(:),indexend(:)
		integer::Degi,i,rank,classtype,ii,dim1,dim2
		logical::goon
		type(Tensor)::temp1,temp2
		if(.not.Res%getFlag())then
			call writemess("ERROR in SymTensor=Tensor,the SymTensor should be allocate ")
			call error_stop()
		endif
		rank=Res%getRank()
		classtype=T%getType()
		if(classtype.ne.Res%getType())then
			call writemess("ERROR in SymTensor=Tensor, data type do not match")
			call writemess('dataype'+classtype+','+Res%getType())
			call error_stop()
		end if
			allocate(inde(rank))
			allocate(alldim(rank))
			allocate(indexstart(rank))
			allocate(indexend(rank))
			allocate(minmaxindex(2,rank))
			alldim=Res%dim()
			indexstart=1
			do i=1,Res%getTotalData()
				if(Res%Block(i)%getFlag())then
					call IndesToaddress(alldim,inde,i)
					minmaxindex=Res%NonSymIndex(inde)
					indexend=Res%Block(i)%dim()
					!!inoutT(indexstart:indexend)=inoutT(minindeT:maxTinde)
					call StoreData(Res%Block(i),T,indexstart,indexend,minmaxindex(1,:),minmaxindex(2,:))
				end if
			end do
			
		if(T%outNameFlag().eq.0) return
		if(T%outNameFlag().eq.1)then
		 do i=1,rank
		 	call Res%setName(i,T%outName(i))
		 end do
		 return
		end if
		if(T%outNameFlag().eq.2)then
		 call writemess('ERROR, Do no finished the part of  integer name in Tensor=SymTensor')
		 call error_stop()
		end if
	end subroutine


	subroutine assignmentSymTensorArray(T,T2)
		type(SymTensor),intent(inout) ::T(:)
		type(SymTensor),intent(in) :: T2(:)
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



	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                     print   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************

	subroutine Dprint(Dimen,uni)!overwrite the subroutine in dimension.f90
		class(SymTensor),intent(in) ::Dimen
		integer,optional,intent(in)::uni
		type(Tensor)::T
		T=Dimen
		call T%print( )
		return
	end subroutine	
	
	subroutine Tprint_file1(T,words,uni,printType)!overwrite the subroutine in dimension.f90
		class(SymTensor),intent(in) ::T
		CHARACTER(len=*),intent(in)::words
		integer,intent(in)::uni
		CHARACTER(len=*),optional,intent(in)::printType
		integer::i,checki
		CHARACTER(len=20)::classTypeChar
		write(uni,*)"=================="
		write(uni,*)"readable data"
		write(uni,*)trim(words)
		write(uni,*)'Flag',T%getFlag()
		if(.not.T%getFlag())then
			write(uni,*)"There is no data"
			return
		end if
		classTypeChar=T%getclassType()
		if(T%ifDynamic())then
			write(uni,*)'Dynamic class SymTensor,data type is:'
			write(uni,*)classTypeChar
		else
			write(uni,*)'Static class SymTensor,data type is:'
			write(uni,*)classTypeChar
		end if
		write(uni,*) "The rank of the SymTensor is"
		write(uni,*) T%getRank()
		write(uni,*) "The length of  data of the SymTensor is"
		write(uni,*) T%getTotalData()
		write(uni,*) "The length of  block of the SymTensor is"
		write(uni,*) T%getTotalBlock()
		write(uni,*) "Data of block in the SymTensor:"
		checki=0
		do i=1,T%getTotalData()
			if(T%block(i)%getFlag())then
				write(uni,*)i
				call T%block(i)%writeinfo(uni,printType)
				checki=checki+1
			end if
		end do
		if(checki.ne.T%getTotalBlock())then
			call writemess("ERROR in TotalBlock of SymTensor,T%getTotalBlock()="+T%getTotalBlock())
			call writemess("checki="+checki)
			call error_stop
		end if
		call T%SymDimension%info(uni)
		write(uni,*) "***END***"
		write(uni,*) ""
		return
	end subroutine

	subroutine Tprint_file2(T,uni,printType)
		class(SymTensor),intent(in) :: T
		integer,intent(in)::uni
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		integer::i,checki
		write(uni,*)"=================="
		write(uni,*)"readable data"
		write(uni,*)"-"
		write(uni,*)'Flag',T%getFlag()
		if(.not.T%getFlag())then
			write(uni,*)"There is no data"
			return
		end if
		classTypeChar=T%getclassType()
		if(T%ifDynamic())then
			write(uni,*)'Dynamic class SymTensor,data type is:'
			write(uni,*)classTypeChar
		else
			write(uni,*)'Static class SymTensor,data type is:'
			write(uni,*)classTypeChar
		end if
		write(uni,*) "The rank of the SymTensor is"
		write(uni,*) T%getRank()
		write(uni,*) "The length of  data of the SymTensor is"
		write(uni,*) T%getTotalData()
		write(uni,*) "The length of  block of the SymTensor is"
		write(uni,*) T%getTotalBlock()
		write(uni,*) "Data of block in the SymTensor:"
		checki=0
		do i=1,T%getTotalData()
			if(T%block(i)%getFlag())then
				write(uni,*)i
				call T%block(i)%writeinfo(uni,printType)
				checki=checki+1
			end if
		end do
		if(checki.ne.T%getTotalBlock())then
			write(*,*)"ERROR in TotalBlock of SymTensor,T%getTotalBlock() is wroing"
			call error_stop
		end if
		call T%SymDimension%info(uni)
		write(uni,*) "***END of SymTensor***"
		return
	end subroutine

	subroutine readdimension(dimen,uni)
		class(SymTensor),intent(inout) :: dimen
		integer,intent(in)::uni
		CHARACTER(len=50)::notused,classTypeChar
		integer::rank,TotalData,i,ith,TotalBlock
		logical::Flag
		read(uni,*)notused
		read(uni,*)notused
		
		if(notused.ne.'readable')then
			call writemess("error in reading SymTensor")
			call error_stop()
		end if
		read(uni,*)notused
		read(uni,*)notused,Flag
		if(.not.Flag)then
			read(uni,*)notused
			call dimen%empty()
			return
		end if
		read(uni,*)notused
		read(uni,*)classTypeChar
		call dimen%empty()
		call dimen%setType(classTypeChar)
		if(notused.eq.'Dynamic')then
			call dimen%Dynamic()
		else if(notused.eq.'Static')then
			call dimen%Static()
		else
			call writemess("ERROR in read SymTensof,NO.2")
			call error_stop
		end if
		
		read(uni,*) notused
		read(uni,*) rank
		read(uni,*) notused
		read(uni,*) TotalData
		read(uni,*) notused
		read(uni,*) TotalBlock
		read(uni,*) notused
		call allocatecheck(dimen%block,TotalData)
		dimen%totalBlock=TotalBlock
		dimen%TotalData=TotalData
		dimen%rank=rank
		do i=1,TotalBlock
				read(uni,*)ith
				call dimen%block(ith)%read(uni)
		end do
		call dimen%SymDimension%read(uni)
		dimen%Flag=.true.
		read(uni,*) notused
		
		return
	end subroutine

	subroutine PrintBlock1(Dimen,vec)!
		class(SymTensor),intent(in) ::Dimen
		integer,intent(in)::vec(:)
		integer::i
		if(.not.Dimen%getFlag())then
			call writemess("There is no Data")
			return
		end if
		i=addressToIndes(Dimen%dim(),vec)
		call Dimen%block(i)%print( )
		return
	end subroutine	
	subroutine PrintBlock3(Dimen,vec)
		class(SymTensor),intent(in) ::Dimen
		integer,intent(in)::vec
		if(.not.Dimen%getFlag())then
			call writemess("There is no Data",-1)
			return
		end if
		call Dimen%block(vec)%print( )
		return
	end subroutine	
	subroutine PrintBlock2(Dimen)
		class(SymTensor),intent(in) ::Dimen
		integer::i,rank
		integer,allocatable::inde(:),maxdim(:)
		real*4,allocatable::qn(:)
		if(.not.Dimen%getFlag())then
			call writemess("There is no Data",-1)
			return
		end if
		rank=Dimen%getRank()
			write(*,*)"************ Start  *****************"
			write(*,*)"The non-zero block are"
		allocate(maxdim(rank))
		allocate(inde(rank))
		allocate(qn(rank))
		maxdim=Dimen%dim()
		do i=1,Dimen%GettotalData()
			if(Dimen%Block(i)%getFlag())then
				call IndesToaddress(maxdim,inde,i)
				qn=Dimen%QNDim(inde)
					write(*,*)"quantum number:",qn
				call Dimen%block(i)%print( )
			end if
		end do
			write(*,*)"total Block is:",Dimen%getTotalblock()
			write(*,*)"************ END  *****************"
			write(*,*)" "
		
		return
	end subroutine	

	subroutine DimPrint1(T,uni)
		class(SymTensor),intent(in) ::T
		integer,optional,intent(in)::uni
		integer::i
		character*500::w
		if(.not.T%getFlag())then
			call writemess("There is no Data",-1)
			return
		end if
		if(present(uni))then
			write(uni,*)' '
			write(uni,*)"************ Start  *****************"
		else
			call writemess( ' ')
			call writemess("************ Start  *****************")
		end if
		call T%SymDimension%print(uni)
		if(present(uni))then
			write(uni,*)"total Block is:",T%getTotalblock()
			write(uni,*)("************ END  *****************")
			write(uni,*)( ' ')
		else
			call writemess("total Block is:"+T%getTotalblock(),-1)
			call writemess("************ END  *****************")
			call writemess( ' ')
		end if
		
		return
	end subroutine

	subroutine DimBlockPrint1(T,vec,uni)
		class(SymTensor),intent(in) ::T
		integer,intent(in)::vec(:)
		integer,optional,intent(in)::uni
		type(Tensor)::temp
		if(.not.T%getFlag())then
			call writemess("There is no Data",-1)
			return
		end if
		temp=T%i(vec)
		call temp%Diminfo(uni)
		return
	end subroutine

	subroutine DimBlockPrint2(T,ith,uni)
		class(SymTensor),intent(in) ::T
		integer,intent(in)::ith
		integer,optional,intent(in)::uni
		integer,allocatable::vec(:),dimen(:)
		character(len=500)::words
		integer::i
		if(.not.T%getFlag())then
			call writemess("There is no Data",-1)
			return
		end if
		allocate(dimen(T%getRank()))
		allocate(vec(T%getRank()))
		call IndesToaddress(dimen,vec,ith)
		if(present(uni))then
			write(uni,*)"The dimension infotmation of block index:",vec
		else
			words=' '+vec(1)
			do i=2,size(vec)
				words=words+(' ,'+(' '+vec(i)))
			end do
			call writemess("The dimension infotmation of block index:"+words,-1)
		end if
		call T%block(ith)%diminfo(uni)
		return
	end subroutine

	subroutine DimBlockPrint3(T,flag)
		class(SymTensor),intent(in) ::T
		logical,optional,intent(in)::flag
		integer,allocatable::vec(:),dimen(:),rule(:)
		integer::ith,i
		real*4,allocatable::qn(:)
		character*50::na
		character*500::w
		type(QuanNum)::QNum
		if(.not.T%getFlag())then
			call writemess("There is no Data",-1)
			return
		end if
		allocate(dimen(T%getRank()))
		allocate(vec(T%getRank()))
		allocate(qn(T%getRank()))
		allocate(rule(T%getRank()))
		dimen=T%dim()
		call T%Dimension%print()
		rule=T%GetRule()
		call writemess("The symmetry rule are",-1)
		w=' '+rule(1)
		do i=2,size(rule)
			w=w+(' ,'+(' '+rule(i)))
		end do
		call writemess(w,-1)
		call writemess("The fermi-arrow are",-1)
		rule=T%GetFermiArrow()
		w=' '+rule(1)
		do i=2,size(rule)
			w=w+(' ,'+(' '+rule(i)))
		end do
		call writemess(w,-1)
		call writemess("The degeneracies of the Quantum Number are",-1)
		do ith=1,T%getRank()
			if(T%outNameFlag().ne.0)then
				w='Name:'+T%getName(ith)+','
				call writemess(w,-1)
			else
				w=''
			end if
			QNum=T%quantumnumber(ith)
			call QNum%print()
			call writemess("---",-1)
		end do
		call writemess(" ",-1)
		do ith=1,T%getTotalData()
			if(T%block(ith)%getFlag())then
				call IndesToaddress(dimen,vec,ith)
					w=' '+vec(1)
					do i=2,size(vec)
						w=w+(' ,'+(' '+vec(i)))
					end do
					call writemess("The dimension infotmation of block index:"+w,-1)
					qn=T%QNDim(vec)
					w=' '+qn(1)
					do i=2,size(qn)
						w=w+(' ,'+(' '+qn(i)))
					end do
					call writemess('The quantum number:'+w,-1)
					call writemess("--",-1)
				if(present(flag).and.flag)call T%block(ith)%diminfo( )
			end if
		end do
		return
	end subroutine
	
	subroutine DimPrint2(T,QNflag)
		class(SymTensor),intent(in) ::T
		logical,intent(in)::QNflag
		if(.not.T%getFlag())then
			call writemess("There is no Data",-1)
			return
		end if
		if(QNflag)then
			call T%SymDimension%print( )
		else
			call T%Dimension%print( )
		end if
		call writemess("total Block is:"+T%getTotalblock(),-1)
		return
	end subroutine

	subroutine DimPrint3(T,uni,QNflag)
		class(SymTensor),intent(in) ::T
		integer,intent(in)::uni
		logical,intent(in)::QNflag
		if(.not.T%getFlag())then
			call writemess("There is no Data",-1)
			return
		end if
		if(QNflag)then
			call T%SymDimension%print(uni)
		else
			call T%Dimension%print(uni)
		end if
		write(uni,*)"total Block is:",T%getTotalblock()
		return
	end subroutine


	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                     Other function   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************

	type(Tensor) function traceSymTensor(T)
		class(SymTensor),intent(in)::T
		integer::rank,i
		type(Tensor),pointer::temp(:,:)
		if(T%getTotalData().eq.1)then
			traceSymTensor=T%i(1)
			return
		end if
		rank=T%getRank()
		if(rank.ne.2) then
			call writemess("error in trace",-1)
			call writemess("input Tensor should be a matrix",-1)
			call error_stop()
		end if
		if((T.dim.1).ne.(T.dim.2)) then
			call writemess("error in trace",-1)
			call writemess("input Tensor should be a matrix",-1)
			call writemess((T.dim.1)+','+(T.dim.2),-1)
			call error_stop()
		end if
		call traceSymTensor%empty()
		call T%pointer(temp)
		do i=1,T%dim(1)
			if(T%getFlag([i,i]))then
				if(traceSymTensor%getFlag())then
					traceSymTensor=traceSymTensor+temp(i,i)%trace()
				else
					traceSymTensor=temp(i,i)%trace()
				end if
			endif
		end do
		temp=>null()
		return
	end function

	real*8 function dtraceSymTensor(T)
		class(SymTensor),intent(in)::T
		integer::rank,i
		type(Tensor),pointer::temp(:,:)
		if(T%getTotalData().eq.1)then
			dtraceSymTensor=T%i(1)
			return
		end if
		rank=T%getRank()
		if(rank.ne.2) then
			call writemess("error in trace",-1)
			call writemess("input Tensor should be a matrix",-1)
			call error_stop()
		end if
		if((T.dim.1).ne.(T.dim.2)) then
			call writemess("error in trace",-1)
			call writemess("input Tensor should be a matrix",-1)
			call writemess((T.dim.1)+','+(T.dim.2),-1)
			call error_stop()
		end if
		dtraceSymTensor=0
		call T%pointer(temp)
		do i=1,T%dim(1)
			if(T%getFlag([i,i]))then
				dtraceSymTensor=dtraceSymTensor+temp(i,i)%dtrace()
			endif
		end do
		temp=>null()
		return
	end function

	real*4 function straceSymTensor(T)
		class(SymTensor),intent(in)::T
		integer::rank,i
		type(Tensor),pointer::temp(:,:)
		if(T%getTotalData().eq.1)then
			straceSymTensor=T%i(1)
			return
		end if
		rank=T%getRank()
		if(rank.ne.2) then
			call writemess("error in trace",-1)
			call writemess("input Tensor should be a matrix",-1)
			call error_stop()
		end if
		if((T.dim.1).ne.(T.dim.2)) then
			call writemess("error in trace",-1)
			call writemess("input Tensor should be a matrix",-1)
			call writemess((T.dim.1)+','+(T.dim.2),-1)
			call error_stop()
		end if
		straceSymTensor=0
		call T%pointer(temp)
		do i=1,T%dim(1)
			if(T%getFlag([i,i]))then
				straceSymTensor=straceSymTensor+temp(i,i)%strace()
			endif
		end do
		temp=>null()
		return
	end function

	complex*8 function ctraceSymTensor(T)
		class(SymTensor),intent(in)::T
		integer::rank,i
		type(Tensor),pointer::temp(:,:)
		if(T%getTotalData().eq.1)then
			ctraceSymTensor=T%i(1)
			return
		end if
		rank=T%getRank()
		if(rank.ne.2) then
			call writemess("error in trace",-1)
			call writemess("input Tensor should be a matrix",-1)
			call error_stop()
		end if
		if((T.dim.1).ne.(T.dim.2)) then
			call writemess("error in trace",-1)
			call writemess("input Tensor should be a matrix",-1)
			call writemess((T.dim.1)+','+(T.dim.2),-1)
			call error_stop()
		end if
		ctraceSymTensor=0
		call T%pointer(temp)
		do i=1,T%dim(1)
			if(T%getFlag([i,i]))then
				ctraceSymTensor=ctraceSymTensor+temp(i,i)%ctrace()
			endif
		end do
		temp=>null()
		return
	end function

	complex*16 function ztraceSymTensor(T)
		class(SymTensor),intent(in)::T
		integer::rank,i
		type(Tensor),pointer::temp(:,:)
		if(T%getTotalData().eq.1)then
			ztraceSymTensor=T%i(1)
			return
		end if
		rank=T%getRank()
		if(rank.ne.2) then
			call writemess("error in trace",-1)
			call writemess("input Tensor should be a matrix",-1)
			call error_stop()
		end if
		if((T.dim.1).ne.(T.dim.2)) then
			call writemess("error in trace",-1)
			call writemess("input Tensor should be a matrix",-1)
			call writemess((T.dim.1)+','+(T.dim.2),-1)
			call error_stop()
		end if
		ztraceSymTensor=0
		call T%pointer(temp)
		do i=1,T%dim(1)
			if(T%getFlag([i,i]))then
				ztraceSymTensor=ztraceSymTensor+temp(i,i)%ztrace()
			endif
		end do
		temp=>null()
		return
	end function



	
	logical function isZeroSymTensor(T)
		class(SymTensor),intent(in)::T
		integer::i
		isZeroSymTensor=.true.
		do i=1,T%getTotalData()
			if(T%getFlag(i))then
				if(.not.T%block(i)%isZero())then
					isZeroSymTensor=.false.
					return
				end if
			end if
		end do
		return
	end function
	
	type(SymTensor) function expmSymTensor(T)
		type(SymTensor),intent(in)::T
		type(SymTensor)::U,V
		TYPE(SymTensor)::temp
		integer::i,j,dimen(2)
		real*8::a
		if(T%getRank().ne.2)then
			call writemess('ERROR in expm SymTensor')
			call error_stop()
		end if
		call expmSymTensor%setType(T%getType())
		temp=T
		expmSymTensor=T
		a=1
		do i=2,99999
			temp=temp*T
			if(temp%isZero())exit
			temp=temp/dble(i)
			expmSymTensor=expmSymTensor+temp
		end do
		do i=1,T%dim(1)
			dimen=T%DegDim((/i,i/))
			if(T%getFlag((/i,i/)))then
				call expmSymTensor%setValue((/i,i/),expmSymTensor%i((/i,i/))+eye(dimen(1),dimen(2)))
			else
				if(product(dimen).ne.0)call expmSymTensor%setValue((/i,i/),eye(dimen(1),dimen(2)))
			end if
		end do
		if(ProductTensor_output_check_flag)call expmSymTensor%checkClassType()
		return
	end function
	
	function eigSymTensor(H,outvector)Result(Res)
		type(SymTensor),allocatable::Res(:)
		class(SymTensor),intent(in)::H
		logical,intent(in),optional::outvector
		integer::i,j,classtype,classtype2
		integer::ii,deg(2)
		real*4::Q1,Q2
		type(Tensor)::temp(2)
		if(H%getRank().ne.2)then
			call writemess('ERROR in eig SymTensor')
			call error_stop()
		end if
		classtype2=H%getType()
		select case(classtype2)
			case (1)
				classtype=4
			case (2)
				classtype=4
			case (3)
				classtype=5
			case (4)
				classtype=4
			case (5)
				classtype=5
		end select
		if(present(outvector).and.outvector)then
			allocate(Res(2))
			call Res(1)%setType(classType)
			call Res(2)%setType(classType2)
			call Res(1)%allocate(H%SymDimension)
			call Res(2)%allocate(H%SymDimension)
		else
			allocate(Res(1))
			call Res(1)%setType(classType)
			call Res(1)%allocate(H%SymDimension)
		endif 
		do j=1,H%dim(2)
			do i=1,H%dim(1)
				Q1=H%getQN(1,i)
				Q2=H%getQN(2,j)
				if(H%getFlag((/i,j/)))then
					if(abs(Q1-Q2).le.1d-7)then
						ii=addressToIndes(H%dim(),(/i,j/))
						temp=H%Block(ii)%eigTensor(outvector)
						Res(1)%block(ii)=eye(temp(1))
						Res(1)%totalBlock=Res(1)%totalBlock+1
						if(present(outvector).and.outvector)then
							Res(2)%block(ii)=temp(2)
							Res(2)%totalBlock=Res(2)%totalBlock+1
						end if
					else
						call writemess('ERROR in eig SymTensor, input Tensor is not symTensor')
						call error_stop()
					end if
				end if
			end do
		end do
		return
	end function
	
	subroutine eye_Tensor2(T)
		class(SymTensor),intent(inout)::T
		integer::i,rank,deg(2),dim1,dim2
		rank=T%getRank()
		if(rank.ne.2)then
			call writemess("ERROR in set eye in SymTensor,input should be a matrix")
			call error_stop()
		end if
		dim1=T%dim(1)
		dim2=T%dim(2)
		if(dim1.ne.dim2)then
			call writemess("ERROR in set eye element in SymTensor,input should be a N*N matrix")
			call error_stop()
		end if
		call T%emptyBlock()
		do i=1,min(dim1,dim2)
			deg=T%DegDim((/i,i/))
			call T%setvalue((/i,i/),eye(deg(1),deg(2)))
		end do
		return
	end subroutine
	
	subroutine eye_Tensor6(T,dimen,classtype)
		class(SymTensor),intent(inout)::T
		character(len=*),intent(in)::classtype
		type(Symdimension),intent(in)::dimen
		call T%empty()
		call T%allocate(dimen,classtype)
		call eye_Tensor2(T)
		return
	end subroutine



	type(Tensor) function normSymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		call normTensor%empty()
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				if(normTensor%getFlag())then
					normTensor=normTensor+T%Block(i)%norm2()
				else
					normTensor=T%Block(i)%norm2()
				end if
			end if
		end do
		normTensor=sqrt(normTensor)
		return
	end 	function

	integer function inormSymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		normTensor=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
					normTensor=normTensor+T%Block(i)%inorm2()
			end if
		end do
		normTensor=sqrt(real(normTensor))
		return
	end 	function

	real(kind=4) function snormSymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		normTensor=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
					normTensor=normTensor+T%Block(i)%snorm2()
			end if
		end do
		normTensor=sqrt(normTensor)
		return
	end 	function

	real(kind=8) function dnormSymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		normTensor=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
					normTensor=normTensor+T%Block(i)%dnorm2()
			end if
		end do
		normTensor=dsqrt(normTensor)
		return
	end 	function
	complex(kind=4) function cnormSymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		normTensor=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
					normTensor=normTensor+T%Block(i)%cnorm2()
			end if
		end do
		normTensor=sqrt(real(normTensor))
		return
	end 	function
	complex(kind=8) function znormSymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		normTensor=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
					normTensor=normTensor+T%Block(i)%znorm2()
			end if
		end do
		normTensor=dsqrt(dble(normTensor))
		return
	end 	function
	
	type(Tensor) function norm2SymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		call normTensor%empty()
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				if(normTensor%getFlag())then
					normTensor=normTensor+T%Block(i)%norm2()
				else
					normTensor=T%Block(i)%norm2()
				end if
			end if
		end do
		return
	end 	function
	
	integer function inorm2SymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		normTensor=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				normTensor=normTensor+T%Block(i)%inorm2()
			end if
		end do
		return
	end 	function
	real*4 function snorm2SymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		normTensor=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				normTensor=normTensor+T%Block(i)%snorm2()
			end if
		end do
		return
	end 	function
	real*8 function dnorm2SymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		normTensor=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				normTensor=normTensor+T%Block(i)%dnorm2()
			end if
		end do
		return
	end 	function
	complex*8 function cnorm2SymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		normTensor=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				normTensor=normTensor+T%Block(i)%cnorm2()
			end if
		end do
		return
	end 	function
	complex*16 function znorm2SymTensor(T)result(normTensor)
		class(SymTensor),intent(in)::T
		integer::i
		normTensor=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				normTensor=normTensor+T%Block(i)%znorm2()
			end if
		end do
		return
	end 	function




	Type(Tensor) function TmaxElement(T)
		class(SymTensor),intent(in)::T
		integer::i
		logical::first
		real*8::temp
		do i=1,T%getTotalData()
			if(T%block(i)%getFlag())then
				if(first)then
					TmaxElement=T%block(i)%max()
					first=.false.
				else
					temp=T%block(i)%max()
					if(temp.gt.TmaxElement) TmaxElement=temp
				end if
			end if
		end do
		return
	end function
	
	Type(Tensor) function TmaxminElement(T,maxminflag)
		class(SymTensor),intent(in)::T
		character(len=4),intent(in)::maxminflag
		integer::i,j
		type(Tensor)::temp
		call temp%allocate((/T%getTotalBlock()/),T%getType())
		j=1
		do i=1,T%getTotalData()
			if(T%block(i)%getFlag())then
				call temp%setValue(j,T%block(i)%max(maxminflag))
				j=j+1
			end if
		end do
		TmaxminElement=temp%max(maxminflag)
		return
	end function
	
	real*8 function dmaxElement(T)
		class(SymTensor),intent(in)::T
		integer::i
		logical::first
		real*8::temp
		do i=1,T%getTotalData()
			if(T%block(i)%getFlag())then
				if(first)then
					dmaxElement=dmax(T%block(i))
					first=.false.
				else
					temp=dmax(T%block(i))
					if(temp.gt.dmaxElement) dmaxElement=temp
				end if
			end if
		end do
		return
	end function
	
	real*8 function dmaxminElement(T,maxminflag)
		class(SymTensor),intent(in)::T
		character(len=4),intent(in)::maxminflag
		integer::i,j
		type(Tensor)::temp
		call temp%allocate((/T%getTotalBlock()/),'real*8')
		j=1
		do i=1,T%getTotalData()
			if(T%block(i)%getFlag())then
				call temp%setValue(j,dmax(T%block(i),maxminflag))
				j=j+1
			end if
		end do
		dmaxminElement=temp%dmax(maxminflag)
		return
	end function
	
	type(Tensor) function TsumSymTensor(T)Result(Res)
		class(SymTensor),intent(in)::T
		integer::i
		call Res%empty()
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				if(Res%getFlag())then
					Res=Res+T%Block(i)%sum()
				else
					Res=T%Block(i)%sum()
				end if
			end if
		end do
		return
	end function
	real*8 function dsumSymTensor(T)Result(Res)
		class(SymTensor),intent(in)::T
		integer::i
		Res=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				Res=Res+T%Block(i)%dsum()
			end if
		end do
		return
	end function
	real*4 function ssumSymTensor(T)Result(Res)
		class(SymTensor),intent(in)::T
		integer::i
		Res=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				Res=Res+T%Block(i)%ssum()
			end if
		end do
		return
	end function
	complex*8 function csumSymTensor(T)Result(Res)
		class(SymTensor),intent(in)::T
		integer::i
		Res=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				Res=Res+T%Block(i)%csum()
			end if
		end do
		return
	end function
	complex*16 function zsumSymTensor(T)Result(Res)
		class(SymTensor),intent(in)::T
		integer::i
		Res=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				Res=Res+T%Block(i)%zsum()
			end if
		end do
		return
	end function
	integer function isumSymTensor(T)Result(Res)
		class(SymTensor),intent(in)::T
		integer::i
		Res=0
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				Res=Res+T%Block(i)%isum()
			end if
		end do
		return
	end function
	







	
	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                     pointer   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************








	subroutine operation_on_SymTensor1(T,routine,T2)
		class(SymTensor),intent(inout)::T
		class(SymDimension),optional,intent(inout)::T2
		external::routine
		if(Present(T2))then
			call routine(T%Block,T%getTotalData(),T%SymDimension,T2)
		else
			call routine(T%Block,T%getTotalData(),T%SymDimension)
		end if
		return
	end subroutine
	subroutine operation_on_SymTensor2(T,routine,T2)
		class(SymTensor),intent(inout)::T
		Type(Tensor),intent(in)::T2
		external::routine
		call routine(T%Block,T%getTotalData(),T%SymDimension,T2)
		return
	end subroutine
	subroutine operation_on_SymTensor3(T,routine,T2)
		class(SymTensor),intent(inout)::T
		class(Tensor),intent(in)::T2(:)
		external::routine
		call routine(T%Block,T%getTotalData(),T%SymDimension,T2,size(T2))
		return
	end subroutine
	
	subroutine TpointerToDataSubroutine(p,indata,lenData)
		integer,intent(in)::lenData
		type(Tensor),intent(in),target::indata(lenData)
		type(Tensor),pointer,intent(inout)::p(:)
		p=>indata(1:lenData)
		return
	end subroutine 
	subroutine TpointerToDataSubroutine_(p,indata,ith,lenData)
		integer,intent(in)::lenData
		type(Tensor),intent(in),target::indata(lenData)
		type(Tensor),pointer,intent(inout)::p(:)
		integer,intent(in)::ith(2)
		p=>indata(ith(1):ith(2))
		return
	end subroutine 
	subroutine TpointerToDataSubroutine2(p,indata,i,j,dimen)
		integer,intent(in)::i(2),j(2),dimen(2)
		type(Tensor),intent(in),target::indata(dimen(1),dimen(2))
		type(Tensor),pointer,intent(inout)::p(:,:)
		p=>indata(i(1):i(2),j(1):j(2))
		return
	end subroutine 
	subroutine TpointerToDataSubroutine3(p,indata,i,j,k,dimen)
		integer,intent(in)::i(2),j(2),k(2),dimen(3)
		type(Tensor),intent(in),target::indata(dimen(1),dimen(2),dimen(3))
		type(Tensor),pointer,intent(inout)::p(:,:,:)
		p=>indata(i(1):i(2),j(1):j(2),k(1):k(2))
		return
	end subroutine 
	subroutine Tpointer(T,p)
		class(SymTensor),intent(in)::T
		type(Tensor),pointer,intent(inout)::p(:)
		if(.not.T%getFlag())then
			call writemess('There is no data in SymTensor',-1)
			call error_stop
		end if
		call TpointerToDataSubroutine(p,T%block,T%totalData)
		return
	end subroutine
	subroutine Tpointer_(T,p,ith)
		class(SymTensor),intent(in)::T
		type(Tensor),pointer,intent(inout)::p(:)
		integer,intent(in)::ith(2)
		if(.not.T%getFlag())then
			call writemess('There is no data in SymTensor',-1)
			call error_stop
		end if
		if( (ith(1).le.0).or.(ith(1).gt.ith(2)))then
			call writemess('ERROR in symT%pointer(p,ith)')
			call writemess('Error in ith ')
			call error_stop
		end if
		if(ith(2).gt.T%getTotalData())then
			call writemess('ERROR in symT%pointer(p,ith)')
			call writemess('Error in ith(2) >totalData')
			call error_stop
		end if
		call TpointerToDataSubroutine_(p,T%block,ith,T%getTotalData())
		return
	end subroutine
	subroutine Tpointer2(T,p)
		class(SymTensor),intent(in)::T
		type(Tensor),pointer,intent(inout)::p(:,:)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the SymTensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call TpointerToDataSubroutine2(p,T%block,[1,d(1)],[1,d(2)],d)
		return
	end subroutine
	subroutine Tpointer2_(T,p,ith,jth)
		class(SymTensor),intent(in)::T
		type(Tensor),pointer,intent(inout)::p(:,:)
		integer,intent(in)::ith(2),jth(2)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the SymTensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		if( (ith(1).le.0).or.(ith(1).gt.ith(2)))then
			call writemess('ERROR in symT%pointer(p,ith,jth)')
			call writemess('Error in ith=('+ith(1)+','+ith(2)+')')
			call error_stop
		end if
		if( (jth(1).le.0).or.(jth(1).gt.jth(2)))then
			call writemess('ERROR in symT%pointer(p,ith,jth)')
			call writemess('Error in jth=('+jth(1)+','+jth(2)+')')
			call error_stop
		end if
		if(ith(2).gt.T%dim(1))then
			call writemess('ERROR in symT%pointer(p,ith,jth)')
			call writemess('Error in ith(2) >T%dim(1)')
			call error_stop
		end if
		if(jth(2).gt.T%dim(2))then
			call writemess('ERROR in symT%pointer(p,ith,jth)')
			call writemess('Error in jth(2) >T%dim(2)')
			call error_stop
		end if
		d=T%dim()
		call TpointerToDataSubroutine2(p,T%block,[ith(1),ith(2)],[jth(1),jth(2)],d)
		return
	end subroutine
	subroutine Tpointer3(T,p)
		class(SymTensor),intent(in)::T
		type(Tensor),pointer,intent(inout)::p(:,:,:)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the SymTensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call TpointerToDataSubroutine3(p,T%block,[1,d(1)],[1,d(2)],[1,d(3)],d)
		return
	end subroutine




	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                     SVD   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************










	subroutine SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum,QuanNumCut)
		type(QuanNum),intent(in)::quanNum1,quanNum2
		type(QuanNum),optional,intent(in)::QuanNumCut
		type(QuanNum),intent(inout)::NewQuanNum
		real*4::QN
		integer::i,deg,ith,jth,deg1,deg2
		if(present(QuanNumCut))then
			NewQuanNum=QuanNumCut
			do i=1,NewQuanNum%getQNlength()
				QN=NewQuanNum%getQN(i)
				deg=NewQuanNum%getDeg(i)
				ith=quanNum1%getIndex(QN)
				jth=quanNum2%getIndex(QN)
				if(ith.ne.0)then
					deg1=quanNum1%getDeg(ith)
				else
					deg1=deg
				end if
				if(jth.ne.0)then
					deg2=quanNum2%getDeg(jth)
				else
					deg2=deg
				end if
				deg=min(deg1,deg2,deg)
				call NewQuanNum%setDeg(i,deg)
			end do
			return
		end if
		call choosSameQN(NewQuanNum,quanNum1,quanNum2)

		return
	end subroutine


	subroutine choosSameQN(outQN,QN1,QN2)
		type(QuanNum),intent(in)::QN1,QN2
		type(QuanNum),intent(inout)::outQN
		real*4,allocatable::Q(:)
		real*4::Qi
		integer,allocatable::Alldeg(:)
		integer::maxlen,lenQ,ith,i
		maxlen=max(QN1%getQNlength(),QN2%getQNlength())
		allocate(Q(maxlen))
		allocate(Alldeg(maxlen))
		lenQ=0
		do i=1,QN1%getQNlength()
			Qi=QN1%getQN(i)
			ith=QN2%getIndex(Qi)
			if(ith.ne.0)then
				lenQ=lenQ+1
				Q(lenQ)=Qi
				Alldeg(lenQ)=min(QN1%getDeg(i),QN2%getDeg(ith))
			end if
		end do
		if(lenQ.eq.0)then
			call writemess(' ERROR in choosSameQN, there is no Quantum numbers that are the same')
			call QN1%print()
			call QN2%print()
			call error_stop
		end if
		call outQN%setQN(Q(1:lenQ))
		call outQN%setDeg(Alldeg(1:lenQ))
		return
	end subroutine


	subroutine SVDRoutine(T,U,S,V,QuanNumCut)
		class(SymTensor),intent(in)::T
		type(SymTensor),intent(inout)::U,S,V
		type(QuanNum),optional,intent(in)::QuanNumCut
		integer::i,j,clastype
		type(SymDimension)::Dim1,dim2,sdim
		type(QuanNum)::NewQuanNum,quanNum1,quanNum2
		real*4::Q1,Q2,Q3,newQ
		integer::degcut,Tdim1,Tdim2,Sdim1,indexs
		logical::flag
		if(T%getRank().ne.2)then 
			call writemess("ERROR in SVD on SymTensor",-1)
			call error_stop()
		end if
		if(T%getTotalBlock().eq.0)then 
			call writemess("ERROR in SVD on SymTensor",-1)
			call writemess("There is no block in the input Tensor",-1)
			call error_stop()
		end if
		quanNum1=T%QuantumNumber(1)
		quanNum2=T%QuantumNumber(2)
		call SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum,QuanNumCut)
		call NewQuanNum%setRule(quanNum2%getRule())
		Dim1=(/quanNum1,NewQuanNum/)
		sdim=NewQuanNum
		call NewQuanNum%setRule(quanNum1%getRule())
		Dim2=(/NewQuanNum,quanNum2/)
		sdim=NewQuanNum+sdim
		clastype=T%getType()
		call U%empty()
		call S%empty()
		call V%empty()
		call U%allocate(Dim1,clastype)
		select case(clastype)
			case(5)
				call S%allocate(Sdim,3)
			case(3)
				call S%allocate(Sdim,3)
			case default
				call S%allocate(Sdim,2)
		end select
		call V%allocate(Dim2,clastype)
		Tdim1=T%dim(1)
		Tdim2=T%dim(2)
		Sdim1=Sdim%dim(1)
		do j=1,Tdim2
			do i=1,Tdim1
				call SVD_subroutine_Flag(Flag,T%block,i,j,Tdim1,Tdim2)
				if(Flag)then
					Q1=T%getQN(1,i)
					Q2=T%getQN(2,j)
					if(Q1.ne.Q2)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in SVD on SymTensor",-1)
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call T%diminfo()
						call error_stop()
					end if
					indexs=NewQuanNum%getIndex(Q1)
					if(indexs.ne.0)then
						degcut=NewQuanNum%getDeg(indexs)
						call SVD_subroutine(T%block,U%block,S%block,V%block,degcut,Tdim1,Tdim2,sdim1,i,j,indexs)
						U%TotalBlock=U%TotalBlock+1
						S%TotalBlock=S%TotalBlock+1
						V%TotalBlock=V%TotalBlock+1
					end if
				end if
			end do
		end do
		if(T%outNameFlag().ne.0)then
			call U%setName(1,T%outName(1))
			call V%setName(2,T%outName(2))
		end if
		return
	end subroutine

	subroutine SVDRoutine_kill_inData(T,U,S,V,QuanNumCut)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
		type(QuanNum),optional,intent(in)::QuanNumCut
		integer::i,j,clastype
		type(SymDimension)::Dim1,dim2,sdim
		type(QuanNum)::NewQuanNum,quanNum1,quanNum2
		real*4::Q1,Q2,Q3,newQ
		integer::degcut,Tdim1,Tdim2,Sdim1,indexs
		logical::flag
		if(T%getRank().ne.2)then 
			call writemess("ERROR in SVD on SymTensor",-1)
			call error_stop()
		end if
		if(T%getTotalBlock().eq.0)then 
			call writemess("ERROR in SVD on SymTensor",-1)
			call writemess("There is no block in the input Tensor",-1)
			call error_stop()
		end if
		quanNum1=T%QuantumNumber(1)
		quanNum2=T%QuantumNumber(2)
		call SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum,QuanNumCut)
		call NewQuanNum%setRule(quanNum2%getRule())
		Dim1=(/quanNum1,NewQuanNum/)
		sdim=NewQuanNum
		call NewQuanNum%setRule(quanNum1%getRule())
		Dim2=(/NewQuanNum,quanNum2/)
		sdim=NewQuanNum+sdim
		clastype=T%getType()
		call U%empty()
		call S%empty()
		call V%empty()
		call U%allocate(Dim1,clastype)
		select case(clastype)
			case(5)
				call S%allocate(Sdim,3)
			case(3)
				call S%allocate(Sdim,3)
			case default
				call S%allocate(Sdim,2)
		end select
		call V%allocate(Dim2,clastype)
		Tdim1=T%dim(1)
		Tdim2=T%dim(2)
		Sdim1=Sdim%dim(1)
		do j=1,Tdim2
			do i=1,Tdim1
				call SVD_subroutine_Flag(Flag,T%block,i,j,Tdim1,Tdim2)
				if(Flag)then
					Q1=T%getQN(1,i)
					Q2=T%getQN(2,j)
					if(Q1.ne.Q2)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in SVD on SymTensor",-1)
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call T%diminfo()
						call error_stop()
					end if
					indexs=NewQuanNum%getIndex(Q1)
					if(indexs.ne.0)then
						degcut=NewQuanNum%getDeg(indexs)
						call SVD_subroutine_kill_inData(T%block,U%block,S%block,V%block,degcut,Tdim1,Tdim2,sdim1,i,j,indexs)
						U%TotalBlock=U%TotalBlock+1
						S%TotalBlock=S%TotalBlock+1
						V%TotalBlock=V%TotalBlock+1
					end if
				end if
			end do
		end do
		if(T%outNameFlag().ne.0)then
			call U%setName(1,T%outName(1))
			call V%setName(2,T%outName(2))
		end if
		return
	end subroutine


	subroutine SVD_subroutine(A,U,S,V,cut,LDA1,LDA2,LDS,indexi,indexj,indexs)
		integer,intent(in)::cut,LDA1,LDA2,LDS,indexi,indexj,indexs
		type(Tensor),intent(in)::A(LDA1,LDA2)
		type(Tensor),intent(inout)::U(LDA1,LDS),S(LDS,LDS),V(LDS,LDA2)
		type(Tensor)::SS
		if(A(indexi,indexj)%getTotalData().eq.1)then
			call U(indexi,indexs)%setType(A(indexi,indexj)%getType())
			call V(indexs,indexj)%setType(A(indexi,indexj)%getType())
			if(A(indexi,indexj).gt.0)then
				S(indexs,indexs)=A(indexi,indexj)
				U(indexi,indexs)=1
				V(indexs,indexj)=1
			else
				S(indexs,indexs)=(-1)*A(indexi,indexj)
				U(indexi,indexs)=-1
				V(indexs,indexj)=1
			end if
			call S(indexs,indexs)%resetdim((/1,1/))
			call U(indexi,indexs)%resetdim((/1,1/))
			call V(indexs,indexj)%resetdim((/1,1/))
		else
			call A(indexi,indexj)%SVDroutine(U(indexi,indexs),ss,V(indexs,indexj),cut)
			if(.not.ifset_SVD_S_matrix_flag())then
				S(indexs,indexs)=eye(ss)
			else
				S(indexs,indexs)=ss
			end if
			
		end if
		if(S(indexs,indexs)%dim(1).ne.cut)then
			call writemess("ERROR in SVD on SymTensor",-1)
			call writemess("The degeneracy error",-1)
			write(*,*)S(indexs,indexs)%dim(1),cut
			call error_stop()
		end if
		return
	end subroutine
	subroutine SVD_subroutine_kill_inData(A,U,S,V,cut,LDA1,LDA2,LDS,indexi,indexj,indexs)
		integer,intent(in)::cut,LDA1,LDA2,LDS,indexi,indexj,indexs
		type(Tensor),intent(inout)::A(LDA1,LDA2)
		type(Tensor),intent(inout)::U(LDA1,LDS),S(LDS,LDS),V(LDS,LDA2)
		type(Tensor)::SS
		if(A(indexi,indexj)%getTotalData().eq.1)then
			call U(indexi,indexs)%setType(A(indexi,indexj)%getType())
			call V(indexs,indexj)%setType(A(indexi,indexj)%getType())
			if(A(indexi,indexj).gt.0)then
				S(indexs,indexs)=A(indexi,indexj)
				U(indexi,indexs)=1
				V(indexs,indexj)=1
			else
				S(indexs,indexs)=(-1)*A(indexi,indexj)
				U(indexi,indexs)=-1
				V(indexs,indexj)=1
			end if
			call S(indexs,indexs)%resetdim((/1,1/))
			call U(indexi,indexs)%resetdim((/1,1/))
			call V(indexs,indexj)%resetdim((/1,1/))
		else
			call A(indexi,indexj)%SVDKill(U(indexi,indexs),ss,V(indexs,indexj),cut)
			if(.not.ifset_SVD_S_matrix_flag())then
				S(indexs,indexs)=eye(ss)
			else
				S(indexs,indexs)=ss
			end if
		end if
		if(S(indexs,indexs)%dim(1).ne.cut)then
			call writemess("ERROR in SVD on SymTensor",-1)
			call writemess("The degeneracy error",-1)
			write(*,*)S(indexs,indexs)%dim(1),cut
			call error_stop()
		end if
		return
	end subroutine

	subroutine SVD_subroutine_Flag(Flag,A,i,j,LDA1,LDA2)
		integer,intent(in)::i,j,LDA1,LDA2
		type(Tensor),intent(in)::A(LDA1,LDA2)
		logical,intent(inout)::Flag
		Flag=A(i,j)%getFlag()
		return
	end subroutine




	subroutine SVDRoutineNum(T,U,S,V,NumSave_)
		class(SymTensor),intent(in)::T
		type(SymTensor),intent(inout)::U,S,V
		integer,intent(in)::NumSave_
		integer::i,j,clastype,NumSave
		type(SymDimension)::Dim1,dim2,sdim
		type(QuanNum)::NewQuanNum,quanNum1,quanNum2
		real*4::Q1,Q2,Q3,newQ
		integer::Tdim1,Tdim2,Sdim1,indexs
		logical::flag,cutFlag
		integer,allocatable::newSDeg(:)
		if(T%getRank().ne.2)then 
			call writemess("ERROR in SVD on SymTensor",-1)
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			call writemess('ERROR in SVD cut number',-1)
			call error_stop
		end if
		if(T%getTotalBlock().le.0)then
			call writemess('The is no block in the Tensor',-1)
			call T%diminfo()
			call error_stop
		end if
		if(NumSave_.le.0)then
			NumSave=-NumSave_
			cutFlag=.false.
		else
			NumSave=NumSave_
			cutFlag=.true.
		end if
		quanNum1=T%QuantumNumber(1)
		quanNum2=T%QuantumNumber(2)
		call SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum)
		call NewQuanNum%setRule(quanNum2%getRule())
		Dim1=(/quanNum1,NewQuanNum/)
		sdim=NewQuanNum
		call NewQuanNum%setRule(quanNum1%getRule())
		Dim2=(/NewQuanNum,quanNum2/)
		sdim=NewQuanNum+sdim
		clastype=T%getType()
		call U%empty()
		call S%empty()
		call V%empty()
		call U%allocate(Dim1,clastype)
		select case(clastype)
			case(5)
				call S%allocate(Sdim,3)
			case(3)
				call S%allocate(Sdim,3)
			case default
				call S%allocate(Sdim,2)
		end select
		call V%allocate(Dim2,clastype)
		Tdim1=T%dim(1)
		Tdim2=T%dim(2)
		Sdim1=Sdim%dim(1)
		do j=1,Tdim2
			do i=1,Tdim1
				call SVD_subroutine_Flag(Flag,T%block,i,j,Tdim1,Tdim2)
				if(Flag)then
					Q1=T%getQN(1,i)
					Q2=T%getQN(2,j)
					if(Q1.ne.Q2)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in SVD on SymTensor",-1)
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call T%diminfo()
						call error_stop()
					end if
					indexs=NewQuanNum%getIndex(Q1)
					if(indexs.ne.0)then
						call SVD_subroutine_S(T%block,U%block,S%block,V%block,Tdim1,Tdim2,sdim1,i,j,indexs)
						U%TotalBlock=U%TotalBlock+1
						S%TotalBlock=S%TotalBlock+1
						V%TotalBlock=V%TotalBlock+1
					end if
				end if
			end do
		end do
		if(cutFlag)then
			allocate(NewSDeg(NewQuanNum%getQNlength()))
			call SVDSavingSingleValueDeg(S%Block,NewSDeg,NumSave,Sdim1,Sdim1)
			call SVD_DiagS(S%block,Sdim1,Sdim1)
			call set_and_cut_col(U,NewSDeg)
			call set_and_cut_deg(S,NewSDeg,3)
			call set_and_cut_deg(V,NewSDeg,1)
			call U%killZeroDeg(2)
			call s%killZeroDeg(-1)
			call V%killZeroDeg(1)
		else
			call SVD_DiagS(S%block,Sdim1,Sdim1)
		end if
		if(T%outNameFlag().ne.0)then
			call U%setName(1,T%outName(1))
			call V%setName(2,T%outName(2))
		end if
		if(ProductTensor_output_check_flag)then
			call U%checkClassType()
			call s%checkClassType()
			call V%checkClassType()
		end if
		return
	end subroutine

	subroutine SVDRoutineNum_kill_inData(T,U,S,V,NumSave_)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
		integer,intent(in)::NumSave_
		integer::i,j,clastype,NumSave
		type(SymDimension)::Dim1,dim2,sdim
		type(QuanNum)::NewQuanNum,quanNum1,quanNum2
		real*4::Q1,Q2,Q3,newQ
		integer::Tdim1,Tdim2,Sdim1,indexs
		logical::flag,cutFlag
		integer,allocatable::newSDeg(:)
		if(T%getRank().ne.2)then 
			call writemess("ERROR in SVD on SymTensor",-1)
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			call writemess('ERROR in SVD cut number',-1)
			call error_stop
		end if
		if(T%getTotalBlock().le.0)then
			call writemess('The is no block in the Tensor',-1)
			call T%diminfo()
			call error_stop
		end if
		if(NumSave_.le.0)then
			NumSave=-NumSave_
			cutFlag=.false.
		else
			NumSave=NumSave_
			cutFlag=.true.
		end if
		quanNum1=T%QuantumNumber(1)
		quanNum2=T%QuantumNumber(2)
		call SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum)
		call NewQuanNum%setRule(quanNum2%getRule())
		Dim1=(/quanNum1,NewQuanNum/)
		sdim=NewQuanNum
		call NewQuanNum%setRule(quanNum1%getRule())
		Dim2=(/NewQuanNum,quanNum2/)
		sdim=NewQuanNum+sdim
		clastype=T%getType()
		call U%empty()
		call S%empty()
		call V%empty()
		call U%allocate(Dim1,clastype)
		select case(clastype)
			case(5)
				call S%allocate(Sdim,3)
			case(3)
				call S%allocate(Sdim,3)
			case default
				call S%allocate(Sdim,2)
		end select
		call V%allocate(Dim2,clastype)
		Tdim1=T%dim(1)
		Tdim2=T%dim(2)
		Sdim1=Sdim%dim(1)
		do j=1,Tdim2
			do i=1,Tdim1
				call SVD_subroutine_Flag(Flag,T%block,i,j,Tdim1,Tdim2)
				if(Flag)then
					Q1=T%getQN(1,i)
					Q2=T%getQN(2,j)
					if(Q1.ne.Q2)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in SVD on SymTensor",-1)
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call T%diminfo()
						call error_stop()
					end if
					indexs=NewQuanNum%getIndex(Q1)
					if(indexs.ne.0)then
						call SVD_subroutine_S_kill_inData(T%block,U%block,S%block,V%block,Tdim1,Tdim2,sdim1,i,j,indexs)
						U%TotalBlock=U%TotalBlock+1
						S%TotalBlock=S%TotalBlock+1
						V%TotalBlock=V%TotalBlock+1
					end if
				end if
			end do
		end do
		if(cutFlag)then
			allocate(NewSDeg(NewQuanNum%getQNlength()))
			call SVDSavingSingleValueDeg(S%Block,NewSDeg,NumSave,Sdim1,Sdim1)
			call SVD_DiagS(S%block,Sdim1,Sdim1)
			call set_and_cut_col(U,NewSDeg)
			call set_and_cut_deg(S,NewSDeg,3)
			call set_and_cut_deg(V,NewSDeg,1)
			call U%killZeroDeg(2)
			call s%killZeroDeg(-1)
			call V%killZeroDeg(1)
		else
			call SVD_DiagS(S%block,Sdim1,Sdim1)
		end if
		if(T%outNameFlag().ne.0)then
			call U%setName(1,T%outName(1))
			call V%setName(2,T%outName(2))
		end if
		if(ProductTensor_output_check_flag)then
			call U%checkClassType()
			call s%checkClassType()
			call V%checkClassType()
		end if
		return
	end subroutine

	subroutine SVD_subroutine_S(A,U,S,V,LDA1,LDA2,LDS,indexi,indexj,indexs)
		integer,intent(in)::LDA1,LDA2,LDS,indexi,indexj,indexs
		type(Tensor),intent(in)::A(LDA1,LDA2)
		type(Tensor),intent(inout)::U(LDA1,LDS),S(LDS,LDS),V(LDS,LDA2)
		type(Tensor)::SS
		if(A(indexi,indexj)%getTotalData().eq.1)then
			call U(indexi,indexs)%setType(A(indexi,indexj)%getType())
			call V(indexs,indexj)%setType(A(indexi,indexj)%getType())
			if(A(indexi,indexj).gt.0)then
				S(indexs,indexs)=A(indexi,indexj)
				U(indexi,indexs)=1
				V(indexs,indexj)=1
			else
				S(indexs,indexs)=(-1)*A(indexi,indexj)
				U(indexi,indexs)=-1
				V(indexs,indexj)=1
			end if
			call U(indexi,indexs)%resetdim((/1,1/))
			call V(indexs,indexj)%resetdim((/1,1/))
			call S(indexs,indexs)%resetdim([1])
		else
			call A(indexi,indexj)%SVDroutine(U(indexi,indexs),S(indexs,indexs),V(indexs,indexj))
		end if
		return
	end subroutine
	subroutine SVD_subroutine_S_kill_inData(A,U,S,V,LDA1,LDA2,LDS,indexi,indexj,indexs)
		integer,intent(in)::LDA1,LDA2,LDS,indexi,indexj,indexs
		type(Tensor),intent(inout)::A(LDA1,LDA2)
		type(Tensor),intent(inout)::U(LDA1,LDS),S(LDS,LDS),V(LDS,LDA2)
		type(Tensor)::SS
		if(A(indexi,indexj)%getTotalData().eq.1)then
			call U(indexi,indexs)%setType(A(indexi,indexj)%getType())
			call V(indexs,indexj)%setType(A(indexi,indexj)%getType())
			if(A(indexi,indexj).gt.0)then
				S(indexs,indexs)=A(indexi,indexj)
				U(indexi,indexs)=1
				V(indexs,indexj)=1
			else
				S(indexs,indexs)=(-1)*A(indexi,indexj)
				U(indexi,indexs)=-1
				V(indexs,indexj)=1
			end if
			call U(indexi,indexs)%resetdim((/1,1/))
			call V(indexs,indexj)%resetdim((/1,1/))
			call S(indexs,indexs)%resetdim([1])
		else
			call A(indexi,indexj)%SVDKill(U(indexi,indexs),S(indexs,indexs),V(indexs,indexj))
		end if
		return
	end subroutine

	subroutine SVDSavingSingleValueDeg(S,Newdeg,NumSave_,LD1,LD2)
		integer,intent(in)::LD1,LD2
		type(Tensor),intent(in)::S(LD1,LD2)
		integer,intent(in)::NumSave_
		integer,intent(inout)::Newdeg(:)
		integer::i,j,k,Total,NumSave,lenNewdeg,k2
		real*8,allocatable,target::AllSdata(:)
		integer,allocatable::AllSindex(:)
		real*8,pointer::dp(:),AllSdatapointer(:)
		if(ifset_SVD_S_matrix_flag())then
			call SVDSavingSingleValueDeg2(S,Newdeg,NumSave_,LD1,LD2)
			return
		end if
		Total=0
		do i=1,LD1
			if(S(i,i)%getFlag())Total=Total+S(i,i)%getTotalData()
		end do
		if(Total.eq.0)then
			call writemess('ERROR in SVDSavingSingleValueDeg',-1)
			call error_stop
		end if
		NumSave=min(NumSave_,total)
		allocate(AllSdata(Total))
		allocate(AllSindex(Total))
		k=0
		do i=1,LD1
			if(S(i,i)%getFlag())then
				do j=1,S(i,i)%getTotalData()
					k=k+1
					if(k.gt.Total)then
						call writemess('ERROR in SVDSavingSingleValueDeg',-1)
						call error_stop
					end if
					AllSdata(k)=S(i,i)%di(j)
					AllSindex(k)=i
				end do
			end if
		end do
		call sortDataReal8(AllSdata,AllSindex)
		Newdeg=0
		lenNewdeg=size(Newdeg)
		do i=1,NumSave
			if(AllSindex(i).gt.lenNewdeg)then
				call writemess('ERROR in SVDSavingSingleValueDeg',-1)
				call error_stop
			end if
			Newdeg(AllSindex(i))=Newdeg(AllSindex(i))+1
		end do
		return
	end subroutine

	subroutine SVDSavingSingleValueDeg2(S,Newdeg,NumSave_,LD1,LD2)!use when one has set  call set_SVD_S_matrix_flag()
		integer,intent(in)::LD1,LD2
		type(Tensor),intent(in)::S(LD1,LD2)
		integer,intent(in)::NumSave_
		integer,intent(inout)::Newdeg(:)
		integer::i,j,k,Total,NumSave,lenNewdeg
		real*8,allocatable::AllSdata(:)
		integer,allocatable::AllSindex(:)
		Total=0
		do i=1,LD1
			if(S(i,i)%getFlag())Total=Total+S(i,i)%dim(1)
		end do
		if(Total.eq.0)then
			call writemess('ERROR in SVDSavingSingleValueDeg',-1)
			call error_stop
		end if
		NumSave=min(NumSave_,total)
		allocate(AllSdata(Total))
		allocate(AllSindex(Total))
		k=0
		do i=1,LD1
			if(S(i,i)%getFlag())then
				do j=1,S(i,i)%dim(1)
					k=k+1
					if(k.gt.Total)then
						call writemess('ERROR in SVDSavingSingleValueDeg',-1)
						call error_stop
					end if
					AllSdata(k)=S(i,i)%di([j,j])
					AllSindex(k)=i
				end do
			end if
		end do
		call sortDataReal8(AllSdata,AllSindex)
		Newdeg=0
		lenNewdeg=size(Newdeg)
		do i=1,NumSave
			if(AllSindex(i).gt.lenNewdeg)then
				call writemess('ERROR in SVDSavingSingleValueDeg',-1)
				call error_stop
			end if
			Newdeg(AllSindex(i))=Newdeg(AllSindex(i))+1
		end do
		return
	end subroutine




	subroutine SVD_DiagS(S,LD1,LD2)
		integer,intent(in)::LD1,LD2
		type(Tensor),intent(inout)::S(LD1,LD2)
		integer::i
		logical::diag
		diag=ifset_SVD_S_matrix_flag()
		if(diag)return
		do i=1,LD1
			if(S(i,i)%getFlag())then
				S(i,i)=eye(S(i,i))
			end if
		end do
		return
	end subroutine

	subroutine set_and_cut_deg(ST,deg,ith)
		use OtherFunction
		type(SymTensor),intent(inout)::ST
		integer,intent(in)::deg(:),ith
		integer::i,j,newdim(2),totalBlock
		type(Tensor),pointer::Tp(:,:)
		if(ST%getRank().ne.2)then 
			call writemess("ERROR in set_and_cut_deg",-1)
			call error_stop()
		end if
		if((ith.le.0).or.(ith.gt.3))then
			call writemess("ERROR in set_and_cut_deg",-1)
			call error_stop()
		end if
		if(ith.eq.3)then
			call ST%setDeg(1,deg)
			call ST%setDeg(2,deg)
		else
			call ST%setDeg(ith,deg)
		end if
		call ST%pointer(Tp)
		totalBlock=0
		do j=1,ST%dim(2)
			do i=1,ST%dim(1)
				if(Tp(i,j)%getFlag())then
					newdim=ST%GetDeg([i,j])
					Tp(i,j)=resetMatrixDim(Tp(i,j),newdim(1),newdim(2))
					if(Tp(i,j)%getFlag())totalBlock=totalBlock+1
				end if
			end do
		end do
		ST%totalBlock=totalBlock
		return
	end  subroutine

	subroutine set_and_cut_col(ST,deg)
		use OtherFunction
		type(SymTensor),intent(inout)::ST
		integer,intent(in)::deg(:)
		integer::i,j,newdim(2),totalBlock
		type(Tensor),pointer::Tp(:,:)
		if(ST%getRank().ne.2)then 
			call writemess("ERROR in set_and_cut_col",-1)
			call error_stop()
		end if
		call ST%setDeg(2,deg)
		call ST%pointer(Tp)
		totalBlock=0
		do j=1,ST%dim(2)
			do i=1,ST%dim(1)
				if(Tp(i,j)%getFlag())then
					if(.not.Tp(i,j)%if_simple_dimension())then
						call writemess('set_and_cut_col only work on simple dimension')
						call error_stop 
					end if
					newdim=ST%GetDeg([i,j])
					if((newdim(1).eq.0).or.(newdim(2).eq.0))then
						call Tp(i,j)%deallocate()
					else
						call Tp(i,j)%reset_dim_no_check(newdim)
					end if
					if(Tp(i,j)%getFlag())totalBlock=totalBlock+1
				end if
			end do
		end do
		ST%totalBlock=totalBlock
		return
	end subroutine



	subroutine SVDRoutineNumMaxValue(T,U,S,V,minNum,NumSave_,maxValue_,VType,outNumSave,outMaxValue)
		class(SymTensor),intent(in)::T
		type(SymTensor),intent(inout)::U,S,V
		integer,intent(in)::minNum
		integer,intent(in)::NumSave_
		real*8,intent(in)::maxValue_
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		integer::NumSave
		real*8::maxValue
		character(len=*),intent(in)::VType
		integer::i,j,clastype
		type(SymDimension)::Dim1,dim2,sdim
		type(QuanNum)::NewQuanNum,quanNum1,quanNum2
		real*4::Q1,Q2,Q3,newQ
		integer::Tdim1,Tdim2,Sdim1,indexs
		logical::flag
		integer,allocatable::newSDeg(:)
		NumSave=NumSave_
		maxValue=maxValue_
		if(T%getRank().ne.2)then 
			call writemess("ERROR in SVD on SymTensor",-1)
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			call writemess('ERROR in SVD cut number',-1)
			call error_stop
		end if
		if(T%getTotalBlock().le.0)then
			call writemess('The is no block in the Tensor',-1)
			call T%diminfo()
			call error_stop
		end if
		quanNum1=T%QuantumNumber(1)
		quanNum2=T%QuantumNumber(2)
		call SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum)
		call NewQuanNum%setRule(quanNum2%getRule())
		Dim1=(/quanNum1,NewQuanNum/)
		sdim=NewQuanNum
		call NewQuanNum%setRule(quanNum1%getRule())
		Dim2=(/NewQuanNum,quanNum2/)
		sdim=NewQuanNum+sdim
		clastype=T%getType()
		call U%empty()
		call S%empty()
		call V%empty()
		call U%allocate(Dim1,clastype)
		select case(clastype)
			case(5)
				call S%allocate(Sdim,3)
			case(3)
				call S%allocate(Sdim,3)
			case default
				call S%allocate(Sdim,2)
		end select
		call V%allocate(Dim2,clastype)
		Tdim1=T%dim(1)
		Tdim2=T%dim(2)
		Sdim1=Sdim%dim(1)
		do j=1,Tdim2
			do i=1,Tdim1
				call SVD_subroutine_Flag(Flag,T%block,i,j,Tdim1,Tdim2)
				if(Flag)then
					Q1=T%getQN(1,i)
					Q2=T%getQN(2,j)
					if(Q1.ne.Q2)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in SVD on SymTensor",-1)
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call error_stop()
					end if
					indexs=NewQuanNum%getIndex(Q1)
					if(indexs.ne.0)then
						call SVD_subroutine_S(T%block,U%block,S%block,V%block,Tdim1,Tdim2,sdim1,i,j,indexs)
						U%TotalBlock=U%TotalBlock+1
						S%TotalBlock=S%TotalBlock+1
						V%TotalBlock=V%TotalBlock+1
					end if
				end if
			end do
		end do
		allocate(NewSDeg(NewQuanNum%getQNlength()))
		call SVDSavingSingleValueDegValue(S%Block,NewSDeg,minNum,NumSave,maxValue,VType,Sdim1,Sdim1)
		call SVD_DiagS(S%block,Sdim1,Sdim1)
		call set_and_cut_col(U,NewSDeg)
		call set_and_cut_deg(S,NewSDeg,3)
		call set_and_cut_deg(V,NewSDeg,1)
		call U%killZeroDeg(2)
		call s%killZeroDeg(-1)
		call V%killZeroDeg(1)
		if(T%outNameFlag().ne.0)then
			call U%setName(1,T%outName(1))
			call V%setName(2,T%outName(2))
		end if
		if(ProductTensor_output_check_flag)then
			call U%checkClassType()
			call s%checkClassType()
			call V%checkClassType()
		end if
		if(present(outNumSave))outNumSave=NumSave
		if(present(outmaxValue))outmaxValue=maxValue
		return
	end subroutine

	subroutine SVDRoutineNumMaxValue_kill_inData(T,U,S,V,minNum,NumSave_,maxValue_,VType,outNumSave,outMaxValue)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
		integer,intent(in)::minNum
		integer,intent(in)::NumSave_
		real*8,intent(in)::maxValue_
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		integer::NumSave
		real*8::maxValue
		character(len=*),intent(in)::VType
		integer::i,j,clastype
		type(SymDimension)::Dim1,dim2,sdim
		type(QuanNum)::NewQuanNum,quanNum1,quanNum2
		real*4::Q1,Q2,Q3,newQ
		integer::Tdim1,Tdim2,Sdim1,indexs
		logical::flag
		integer,allocatable::newSDeg(:)
		NumSave=NumSave_
		maxValue=maxValue_
		if(T%getRank().ne.2)then 
			call writemess("ERROR in SVD on SymTensor",-1)
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			call writemess('ERROR in SVD cut number',-1)
			call error_stop
		end if
		if(T%getTotalBlock().le.0)then
			call writemess('The is no block in the Tensor',-1)
			call T%diminfo()
			call error_stop
		end if
		quanNum1=T%QuantumNumber(1)
		quanNum2=T%QuantumNumber(2)
		call SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum)
		call NewQuanNum%setRule(quanNum2%getRule())
		Dim1=(/quanNum1,NewQuanNum/)
		sdim=NewQuanNum
		call NewQuanNum%setRule(quanNum1%getRule())
		Dim2=(/NewQuanNum,quanNum2/)
		sdim=NewQuanNum+sdim
		clastype=T%getType()
		call U%empty()
		call S%empty()
		call V%empty()
		call U%allocate(Dim1,clastype)
		select case(clastype)
			case(5)
				call S%allocate(Sdim,3)
			case(3)
				call S%allocate(Sdim,3)
			case default
				call S%allocate(Sdim,2)
		end select
		call V%allocate(Dim2,clastype)
		Tdim1=T%dim(1)
		Tdim2=T%dim(2)
		Sdim1=Sdim%dim(1)
		do j=1,Tdim2
			do i=1,Tdim1
				call SVD_subroutine_Flag(Flag,T%block,i,j,Tdim1,Tdim2)
				if(Flag)then
					Q1=T%getQN(1,i)
					Q2=T%getQN(2,j)
					if(Q1.ne.Q2)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in SVD on SymTensor",-1)
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call error_stop()
					end if
					indexs=NewQuanNum%getIndex(Q1)
					if(indexs.ne.0)then
						call SVD_subroutine_S_kill_inData(T%block,U%block,S%block,V%block,Tdim1,Tdim2,sdim1,i,j,indexs)
						U%TotalBlock=U%TotalBlock+1
						S%TotalBlock=S%TotalBlock+1
						V%TotalBlock=V%TotalBlock+1
					end if
				end if
			end do
		end do
		allocate(NewSDeg(NewQuanNum%getQNlength()))
		call SVDSavingSingleValueDegValue(S%Block,NewSDeg,minNum,NumSave,maxValue,VType,Sdim1,Sdim1)
		call SVD_DiagS(S%block,Sdim1,Sdim1)
		call set_and_cut_col(U,NewSDeg)
		call set_and_cut_deg(S,NewSDeg,3)
		call set_and_cut_deg(V,NewSDeg,1)
		call U%killZeroDeg(2)
		call s%killZeroDeg(-1)
		call V%killZeroDeg(1)
		if(T%outNameFlag().ne.0)then
			call U%setName(1,T%outName(1))
			call V%setName(2,T%outName(2))
		end if
		if(ProductTensor_output_check_flag)then
			call U%checkClassType()
			call s%checkClassType()
			call V%checkClassType()
		end if
		if(present(outNumSave))outNumSave=NumSave
		if(present(outmaxValue))outmaxValue=maxValue
		return
	end subroutine

	subroutine SVDRoutineMaxValue(T,U,S,V,maxValue_,VType,outMaxValue)
		class(SymTensor),intent(in)::T
		type(SymTensor),intent(inout)::U,S,V
		integer::minNum
		integer::NumSave
		real*8,intent(in)::maxValue_
		real*8,intent(inout),optional::outMaxValue
		real*8::maxValue
		character(len=*),intent(in)::VType
		integer::i,j,clastype
		type(SymDimension)::Dim1,dim2,sdim
		type(QuanNum)::NewQuanNum,quanNum1,quanNum2
		real*4::Q1,Q2,Q3,newQ
		integer::Tdim1,Tdim2,Sdim1,indexs
		logical::flag
		integer,allocatable::newSDeg(:)
		minNum=1
		maxValue=maxValue_
		if(T%getRank().ne.2)then 
			call writemess("ERROR in SVD on SymTensor",-1)
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			call writemess('ERROR in SVD cut number',-1)
			call error_stop
		end if
		if(T%getTotalBlock().le.0)then
			call writemess('The is no block in the Tensor',-1)
			call T%diminfo()
			call error_stop
		end if
		NumSave=min(sum(T%GetDeg(1)),sum(T%GetDeg(2)))
		quanNum1=T%QuantumNumber(1)
		quanNum2=T%QuantumNumber(2)
		call SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum)
		call NewQuanNum%setRule(quanNum2%getRule())
		Dim1=(/quanNum1,NewQuanNum/)
		sdim=NewQuanNum
		call NewQuanNum%setRule(quanNum1%getRule())
		Dim2=(/NewQuanNum,quanNum2/)
		sdim=NewQuanNum+sdim
		clastype=T%getType()
		call U%empty()
		call S%empty()
		call V%empty()
		call U%allocate(Dim1,clastype)
		select case(clastype)
			case(5)
				call S%allocate(Sdim,3)
			case(3)
				call S%allocate(Sdim,3)
			case default
				call S%allocate(Sdim,2)
		end select
		call V%allocate(Dim2,clastype)
		Tdim1=T%dim(1)
		Tdim2=T%dim(2)
		Sdim1=Sdim%dim(1)
		do j=1,Tdim2
			do i=1,Tdim1
				call SVD_subroutine_Flag(Flag,T%block,i,j,Tdim1,Tdim2)
				if(Flag)then
					Q1=T%getQN(1,i)
					Q2=T%getQN(2,j)
					if(Q1.ne.Q2)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in SVD on SymTensor",-1)
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call error_stop()
					end if
					indexs=NewQuanNum%getIndex(Q1)
					if(indexs.ne.0)then
						call SVD_subroutine_S(T%block,U%block,S%block,V%block,Tdim1,Tdim2,sdim1,i,j,indexs)
						U%TotalBlock=U%TotalBlock+1
						S%TotalBlock=S%TotalBlock+1
						V%TotalBlock=V%TotalBlock+1
					end if
				end if
			end do
		end do
		allocate(NewSDeg(NewQuanNum%getQNlength()))
		call SVDSavingSingleValueDegValue(S%Block,NewSDeg,minNum,NumSave,maxValue,VType,Sdim1,Sdim1)
		call SVD_DiagS(S%block,Sdim1,Sdim1)
		call set_and_cut_col(U,NewSDeg)
		call set_and_cut_deg(S,NewSDeg,3)
		call set_and_cut_deg(V,NewSDeg,1)
		call U%killZeroDeg(2)
		call s%killZeroDeg(-1)
		call V%killZeroDeg(1)
		if(T%outNameFlag().ne.0)then
			call U%setName(1,T%outName(1))
			call V%setName(2,T%outName(2))
		end if
		if(ProductTensor_output_check_flag)then
			call U%checkClassType()
			call s%checkClassType()
			call V%checkClassType()
		end if
		if(present(outmaxValue))outmaxValue=maxValue
		return
	end subroutine

	subroutine SVDRoutineMaxValue_kill_inData(T,U,S,V,maxValue_,VType,outMaxValue)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
		integer::minNum
		integer::NumSave
		real*8,intent(in)::maxValue_
		real*8,intent(inout),optional::outMaxValue
		real*8::maxValue
		character(len=*),intent(in)::VType
		integer::i,j,clastype
		type(SymDimension)::Dim1,dim2,sdim
		type(QuanNum)::NewQuanNum,quanNum1,quanNum2
		real*4::Q1,Q2,Q3,newQ
		integer::Tdim1,Tdim2,Sdim1,indexs
		logical::flag
		integer,allocatable::newSDeg(:)
		minNum=1
		maxValue=maxValue_
		if(T%getRank().ne.2)then 
			call writemess("ERROR in SVD on SymTensor",-1)
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			call writemess('ERROR in SVD cut number',-1)
			call error_stop
		end if
		if(T%getTotalBlock().le.0)then
			call writemess('The is no block in the Tensor',-1)
			call T%diminfo()
			call error_stop
		end if
		NumSave=min(sum(T%GetDeg(1)),sum(T%GetDeg(2)))
		quanNum1=T%QuantumNumber(1)
		quanNum2=T%QuantumNumber(2)
		call SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum)
		call NewQuanNum%setRule(quanNum2%getRule())
		Dim1=(/quanNum1,NewQuanNum/)
		sdim=NewQuanNum
		call NewQuanNum%setRule(quanNum1%getRule())
		Dim2=(/NewQuanNum,quanNum2/)
		sdim=NewQuanNum+sdim
		clastype=T%getType()
		call U%empty()
		call S%empty()
		call V%empty()
		call U%allocate(Dim1,clastype)
		select case(clastype)
			case(5)
				call S%allocate(Sdim,3)
			case(3)
				call S%allocate(Sdim,3)
			case default
				call S%allocate(Sdim,2)
		end select
		call V%allocate(Dim2,clastype)
		Tdim1=T%dim(1)
		Tdim2=T%dim(2)
		Sdim1=Sdim%dim(1)
		do j=1,Tdim2
			do i=1,Tdim1
				call SVD_subroutine_Flag(Flag,T%block,i,j,Tdim1,Tdim2)
				if(Flag)then
					Q1=T%getQN(1,i)
					Q2=T%getQN(2,j)
					if(Q1.ne.Q2)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in SVD on SymTensor",-1)
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call error_stop()
					end if
					indexs=NewQuanNum%getIndex(Q1)
					if(indexs.ne.0)then
						call SVD_subroutine_S_kill_inData(T%block,U%block,S%block,V%block,Tdim1,Tdim2,sdim1,i,j,indexs)
						U%TotalBlock=U%TotalBlock+1
						S%TotalBlock=S%TotalBlock+1
						V%TotalBlock=V%TotalBlock+1
					end if
				end if
			end do
		end do
		allocate(NewSDeg(NewQuanNum%getQNlength()))
		call SVDSavingSingleValueDegValue(S%Block,NewSDeg,minNum,NumSave,maxValue,VType,Sdim1,Sdim1)
		call SVD_DiagS(S%block,Sdim1,Sdim1)
		call set_and_cut_col(U,NewSDeg)
		call set_and_cut_deg(S,NewSDeg,3)
		call set_and_cut_deg(V,NewSDeg,1)
		call U%killZeroDeg(2)
		call s%killZeroDeg(-1)
		call V%killZeroDeg(1)
		if(T%outNameFlag().ne.0)then
			call U%setName(1,T%outName(1))
			call V%setName(2,T%outName(2))
		end if
		if(ProductTensor_output_check_flag)then
			call U%checkClassType()
			call s%checkClassType()
			call V%checkClassType()
		end if
		if(present(outmaxValue))outmaxValue=maxValue
		return
	end subroutine

	subroutine SVDSavingSingleValueDegValue(S,Newdeg,minNumSave,NumSave,maxValue,VType,LD1,LD2)
		integer,intent(in)::LD1,LD2,minNumSave
		type(Tensor),intent(in)::S(LD1,LD2)
		integer,intent(inout)::NumSave
		character(len=*),intent(in)::VType
		real*8,intent(inout)::maxValue
		integer,intent(inout)::Newdeg(:)
		integer::i,j,k,Total,lenNewdeg,k2
		real*8,allocatable,target::AllSdata(:)
		integer,allocatable::AllSindex(:)
		real*8::sumValue,tempr
		real*8,pointer::dp(:),AllSdatapointer(:)
		Total=0
		do i=1,LD1
			if(S(i,i)%getFlag())Total=Total+S(i,i)%getTotalData()
		end do
		if(Total.eq.0)then
			call writemess('ERROR in SVDSavingSingleValueDegValue',-1)
			call error_stop
		end if
		NumSave=min(NumSave,total)
		allocate(AllSdata(Total))
		allocate(AllSindex(Total))
		k=0
		do i=1,LD1
			if(S(i,i)%getFlag())then
				do j=1,S(i,i)%getTotalData()
					k=k+1
					if(k.gt.Total)then
						call writemess('ERROR in SVDSavingSingleValueDegValue',-1)
						call error_stop
					end if
					AllSdata(k)=S(i,i)%di(j)
					AllSindex(k)=i
				end do
			end if
		end do
		call sortDataReal8(AllSdata,AllSindex)
		Newdeg=0
		sumValue=0
		Total=0
		lenNewdeg=size(Newdeg)
		tempr=1d0/AllSdata(1)
		do i=1,NumSave
			Total=Total+1
			if(AllSindex(i).gt.lenNewdeg)then
				call writemess('ERROR in SVDSavingSingleValueDegValue',-1)
				call error_stop
			end if
			Newdeg(AllSindex(i))=Newdeg(AllSindex(i))+1
			if(VType.equ.'sum2')then
				sumValue=sumValue+(AllSdata(i)*AllSdata(i))
				if((sumValue.gt.maxValue).and.(i.ge.minNumSave))then
					sumValue=AllSdata(max(i-1,1))
					Total=Total-1
					Newdeg(AllSindex(i))=Newdeg(AllSindex(i))-1
					exit
				end if
			else if(VType.equ.'1_n') then
				sumValue=abs(AllSdata(i)*tempr)
				if( ( sumValue.le.maxValue).and.(i.ge.minNumSave)) then
					sumValue=AllSdata(max(i-1,1))
					Total=Total-1
					Newdeg(AllSindex(i))=Newdeg(AllSindex(i))-1
					exit
				end if
			else if(VType.equ.'1n') then
				sumValue=abs(AllSdata(i)*tempr)
				if( ( sumValue.le.maxValue).and.(i.ge.minNumSave)) then
					sumValue=AllSdata(max(i-1,1))
					Total=Total-1
					Newdeg(AllSindex(i))=Newdeg(AllSindex(i))-1
					exit
				end if
			else if(VType.equ.'sum')then
				sumValue=sumValue+AllSdata(i)
				if( (sumValue.gt.maxValue).and.(i.ge.minNumSave))then
					sumValue=AllSdata(max(i-1,1))
					Total=Total-1
					Newdeg(AllSindex(i))=Newdeg(AllSindex(i))-1
					exit
				end if
			else if(VType.equ.'min')then
				sumValue=AllSdata(i)
				if( (sumValue.le.maxValue).and.(i.ge.minNumSave))then
					sumValue=AllSdata(max(i-1,1))
					Total=Total-1
					Newdeg(AllSindex(i))=Newdeg(AllSindex(i))-1
					exit
				end if
			else
				call writemess('No such case in SVD',-1)
				call writemess('VType='+VType,-1)
				call error_stop
			end if
		end do
		NumSave=Total
		maxValue=sumValue
		return
	end subroutine




	subroutine sortDataReal8(a,inde)
		real*8,intent(inout) :: a(:)
		integer,intent(inout):: inde(:)
		real*8 :: temp
		integer :: i,j,n,tempi
		n=size(a)
		do i=1,n-1
			 do j=i+1,n
				  if (a(i) .lt. a(j)) then
						temp = a(i)
						tempi=inde(i)
						a(i) = a(j)
						inde(i)=inde(j)
						a(j) = temp
						inde(j)=tempi
				  endif
			 enddo
		enddo
		return
	end subroutine





	subroutine SVDNameRoutine(Tin,U,S,V,nameU,nameV,QuanNumCut)
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::U,S,V
		type(QuanNum),optional,intent(in)::QuanNumCut
		character(len=*)::nameU,nameV
		type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutine(T,U,S,V,QuanNumCut)
		if(reverFlag)then
			call reverseSymmetryRule(V)
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
	subroutine SVDNameRoutine_kill_inData(T,U,S,V,nameU,nameV,QuanNumCut)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutine_kill_inData(T,U,S,V,QuanNumCut)
		if(reverFlag)then
			call reverseSymmetryRule(V)
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

	subroutine SVDNameNumRoutine(Tin,U,S,V,nameU,nameV,QuanNumCut)
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::U,S,V
		integer,intent(in)::QuanNumCut
		character(len=*)::nameU,nameV
		type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineNum(T,U,S,V,QuanNumCut)
		if(reverFlag)then
			call reverseSymmetryRule(V)
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

	subroutine SVDNameNumRoutine_kill_inData(T,U,S,V,nameU,nameV,QuanNumCut)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
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
			call T%backWard(nameV)
		end if
		T=T%SymFuse(rankU,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)
		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineNum_kill_inData(T,U,S,V,QuanNumCut)
		if(reverFlag)then
			call reverseSymmetryRule(V)
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

	subroutine SVDNameNumMaxValueRoutine(Tin,U,S,V,nameU,nameV,minNum,NumSave_,&
		                    maxValue_,Vtype,outNumSave,outMaxValue)
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::U,S,V
		integer,intent(in)::minNum
		integer,intent(in)::NumSave_
		real*8,intent(in)::maxValue_
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::Vtype
		character(len=*)::nameU,nameV
		type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineNumMaxValue(T,U,S,V,minNum,NumSave_,maxValue_,VType,outNumSave,outMaxValue)
		if(reverFlag)then
			call reverseSymmetryRule(V)
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
	subroutine SVDNameNumMaxValueRoutine_kill_inData(T,U,S,V,nameU,nameV,minNum,NumSave_,&
		                             maxValue_,Vtype,outNumSave,outMaxValue)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
		integer,intent(in)::minNum
		integer,intent(in)::NumSave_
		real*8,intent(in)::maxValue_
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::Vtype
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineNumMaxValue_kill_inData(T,U,S,V,minNum,NumSave_,maxValue_,VType,outNumSave,outMaxValue)
		if(reverFlag)then
			call reverseSymmetryRule(V)
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

	subroutine SVDNameMaxValueRoutine(Tin,U,S,V,nameU,nameV,maxValue_,Vtype,outMaxValue)
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue_
		character(len=*),intent(in)::Vtype
		real*8,intent(inout),optional::outMaxValue
		character(len=*)::nameU,nameV
		type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineMaxValue(T,U,S,V,maxValue_,VType,outMaxValue)
		if(reverFlag)then
			call reverseSymmetryRule(V)
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
	subroutine SVDNameMaxValueRoutine_kill_inData(T,U,S,V,nameU,nameV,maxValue_,Vtype,outMaxValue)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue_
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::Vtype
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineMaxValue_kill_inData(T,U,S,V,maxValue_,VType,outMaxValue)
		if(reverFlag)then
			call reverseSymmetryRule(V)
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


	subroutine SVDNameNumRoutineLeft(Tin,U,S,V,SVDname,NumSave,leftFlag)
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::U,S,V
		integer,intent(in)::NumSave
		character(len=*)::SVDname(:)
		logical,intent(in)::leftFlag
		type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineNum(T,U,S,V,NumSave)

		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	subroutine SVDNameNumRoutineLeft_kill_inData(T,U,S,V,SVDname,NumSave,leftFlag)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
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
			call T%backWard(SVDname)
			lenLeft=rank-lenSVDname
		end if
		T=T%SymFuse(lenLeft,dimen(1),order(1),orderinfo(1),.true.,default_Row_Rule_)
		T=T%SymFuse(2,dimen(2),order(2),orderinfo(2),.false.,default_Col_Rule_)

		if(T%getRule(1)*T%getRule(2).ge.0)then
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineNum_kill_inData(T,U,S,V,NumSave)

		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	subroutine SVDNameRoutineLeft(Tin,U,S,V,SVDname,NumSave,leftFlag)
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::U,S,V
		type(QuanNum),intent(in)::NumSave
		character(len=*)::SVDname(:)
		logical,intent(in)::leftFlag
		type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutine(T,U,S,V,NumSave)

		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 
	subroutine SVDNameRoutineLeft_kill_inData(T,U,S,V,SVDname,NumSave,leftFlag)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutine_kill_inData(T,U,S,V,NumSave)

		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	subroutine SVDNameRoutineLeft2(Tin,U,S,V,SVDname,leftFlag)
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::U,S,V
		character(len=*)::SVDname(:)
		logical,intent(in)::leftFlag
		type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutine(T,U,S,V)

		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine

	subroutine SVDNameRoutineLeft2_kill_inData(T,U,S,V,SVDname,leftFlag)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutine_kill_inData(T,U,S,V)

		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine


	subroutine SVDNameNumMaxValueRoutineLeft(Tin,U,S,V,SVDname,minNum,NumSave_,maxValue_,Vtype,&
		                               leftFlag,outNumSave,outMaxValue)
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue_
		integer,intent(in)::NumSave_
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::Vtype
		integer,intent(in)::minNum
		character(len=*),intent(in)::SVDname(:)
		logical,intent(in)::leftFlag
		type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineNumMaxValue(T,U,S,V,minNum,NumSave_,maxValue_,VType,outNumSave,outMaxValue)

		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 
	subroutine SVDNameNumMaxValueRoutineLeft_kill_inData(T,U,S,V,SVDname,minNum,NumSave_,maxValue_,&
		                                     Vtype,leftFlag,outNumSave,outMaxValue)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue_
		integer,intent(in)::NumSave_
		integer,optional,intent(inout)::outNumSave
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::Vtype
		integer,intent(in)::minNum
		character(len=*),intent(in)::SVDname(:)
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineNumMaxValue_kill_inData(T,U,S,V,minNum,NumSave_,maxValue_,VType,outNumSave,outMaxValue)

		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	subroutine SVDNameMaxValueRoutineLeft(Tin,U,S,V,SVDname,maxValue_,Vtype,leftFlag,outMaxValue)
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue_
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::Vtype
		character(len=*),intent(in)::SVDname(:)
		logical,intent(in)::leftFlag
		type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineMaxValue(T,U,S,V,maxValue_,VType,outMaxValue)

		if(Tin%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 

	subroutine SVDNameMaxValueRoutineLeft_kill_inData(T,U,S,V,SVDname,maxValue_,Vtype,leftFlag,outMaxValue)
		class(SymTensor),intent(inout)::T
		type(SymTensor),intent(inout)::U,S,V
		real*8,intent(in)::maxValue_
		real*8,intent(inout),optional::outMaxValue
		character(len=*),intent(in)::Vtype
		character(len=*),intent(in)::SVDname(:)
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call SVDRoutineMaxValue_kill_inData(T,U,S,V,maxValue_,VType,outMaxValue)

		if(T%getNameFlag().ne.0)then
			call U%setName(2,'SVD.U')
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
			call V%setName(1,'SVD.V')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(V)
		end if
		U=U%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		V=V%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
		return
	end subroutine 















	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                     QR   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************










	subroutine QRdecomposition(T,Q,R) 
		class(SymTensor),intent(in)::T
		type(SymTensor),intent(inout)::Q,R
		type(Symdimension)::Qdimen,Rdimen
		integer::M,N,K,classtype,indexs,i,j,deg,deg1,deg2
		type(QuanNum)::NewQuanNum,quanNum1,quanNum2
		real*4::Q1,Q2
		if(T%getRank().ne.2) then
			call writemess("ERROR in QR decomposition",-1)
			call writemess("input SymTensor should be a matrix",-1)
			call error_stop()
		endif
		if(T%getTotalBlock().eq.0) then
			call writemess("ERROR in QR decomposition",-1)
			call writemess("There is no block in the SymTensor",-1)
			call error_stop()
		endif
		M=T%dim(1)
		N=T%dim(2)
		quanNum1=T%QuantumNumber(1)
		quanNum2=T%QuantumNumber(2)
		call SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum)
		classtype=T%getType()
		call NewQuanNum%setRule(quanNum2%getRule())
		Qdimen=(T%SymDimension.subdim.1)+NewQuanNum
		call NewQuanNum%setRule(quanNum1%getRule())
		Rdimen=NewQuanNum+(T%SymDimension.subdim.2)
		call Q%empty()
		call R%empty()
		call Q%allocate(Qdimen,classtype)
		call R%allocate(Rdimen,classtype)
		K=R%dim(1)
		do j=1,N
			do i=1,M
				if(T%getFlag((/i,j/)))then
					Q1=T%getQN(1,i)
					Q2=T%getQN(2,j)
					if(abs(Q1-Q2).ge.default_zero_real_number)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in QR on SymTensor",-1)
						call T%diminfo()
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call error_stop()
					end if
					indexs=NewQuanNum%getIndex(Q1)
					if(indexs.eq.0)then
						call writemess("ERROR in QR on SymTensor, NO.2",-1)
						call T%diminfo()
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call error_stop()
					end if
					call QR_subroutine(T%block,Q%block,R%block,M,N,K,i,j,indexs) 
					Q%TotalBlock=Q%TotalBlock+1
					R%TotalBlock=R%TotalBlock+1
				end if
			end do
		end do
		if(ProductTensor_output_check_flag)then
			call Q%checkClassType()
			call R%checkClassType()
		end if
		return
	end subroutine


	subroutine choosSmallDeg(inoutQN,QN)
		type(QuanNum),intent(in)::QN
		type(QuanNum),intent(inout)::inoutQN
		real*4::Qi
		integer::deg,ith,i
		do i=1,inoutQN%getQNlength()
			Qi=inoutQN%getQN(i)
			ith=QN%getIndex(Qi)
			if(ith.ne.0)then
				deg=min(inoutQN%getDeg(i),QN%getDeg(ith))
				call inoutQN%setDeg(i,deg)
			end if
		end do
		return
	end subroutine

	subroutine QRdecomposition_kill_inData(Q,R) 
		class(SymTensor),intent(inout)::Q
		type(SymTensor),intent(inout)::R
		type(Symdimension)::Rdimen
		integer::M,N,K,classtype,indexs,i,j,deg,deg1,deg2
		type(QuanNum)::quanNum1,quanNum2
		type(symDimension)::SymDim
		real*4::Q1,Q2
		if(Q%getRank().ne.2) then
			call writemess("ERROR in QR decomposition",-1)
			call writemess("input SymTensor should be a matrix",-1)
			call error_stop()
		endif
		if(Q%getTotalBlock().eq.0) then
			call writemess("ERROR in QR decomposition",-1)
			call writemess("There is no block in the SymTensor",-1)
			call error_stop()
		endif
		M=Q%dim(1)
		N=Q%dim(2)
		quanNum2=Q%QuantumNumber(2)
		quanNum1=quanNum2
		classtype=Q%getType()
		call choosSmallDeg(quanNum1,Q%QuantumNumber(1))
		SymDim=(Q.subdim.1)+quanNum1
		call quanNum1%setRule(Q%getRule(1))
		Rdimen=quanNum1+(Q.subdim.2)
		call R%empty()
		call R%allocate(Rdimen,classtype)
		call Q%reset_dim_no_check(SymDim)
		K=R%dim(1)
		do j=1,N
			do i=1,M
				if(Q%getFlag((/i,j/)))then
					Q1=Q%getQN(1,i)
					Q2=Q%getQN(2,j)
					if(abs(Q1-Q2).ge.default_zero_real_number)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in QR on SymTensor",-1)
						call Q%diminfo()
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call error_stop()
					end if
					indexs=quanNum1%getIndex(Q1)
					if(indexs.eq.0)then
						call writemess("ERROR in QR on SymTensor, NO.2",-1)
						call Q%diminfo()
						call writemess("The quantum number are:"+Q1+' , '+Q2,-1)
						call error_stop()
					end if
					call QR_subroutine_kill_inData(Q%block,R%block,M,N,K,i,j,indexs) 
					R%TotalBlock=R%TotalBlock+1
				end if
			end do
		end do
		if(ProductTensor_output_check_flag)then
			call Q%checkClassType()
			call R%checkClassType()
		end if
		return
	end subroutine


	!T = QR

	subroutine QR_subroutine(T,Q,R,M,N,MN,i,j,k) 
		integer,intent(in)::M,N,MN,i,j,k
		type(Tensor),intent(in)::T(M,N)
		type(Tensor),intent(inout)::Q(M,MN),R(MN,N)
		Q(i,k)=T(i,j)
		call Q(i,k)%QRkill(R(k,j))
		return
	end subroutine

	subroutine QR_subroutine_kill_inData(Q,R,M,N,MN,i,j,k) 
		integer,intent(in)::M,N,MN,i,j,k
		type(Tensor),intent(inout)::Q(M,MN)
		type(Tensor),intent(inout)::R(MN,N)
		call Q(i,j)%QRkill(R(k,j))
		return
	end subroutine


	subroutine QRRoutineNameLeft(Tin,Q,R,QRName,LeftFlag_)
		character(len=*),intent(in)::QRName(:)
		logical,optional,intent(in)::LeftFlag_
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::Q,R
		logical::LeftFlag,reverFlag
		integer::lenQRName,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		Type(SymTensor)::T
		integer::lenLeft
		if(present(LeftFlag_))then
			LeftFlag=LeftFlag_
		else
			LeftFlag=.true.
		end if
		rank=Tin%getRank()
		lenQRName=size(QRName)
		if(lenQRName.ge.rank)then
			call writemess("ERROR in QRSymTensor, input name larger than the rank of the Tensor",-1)
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call QRdecomposition(T,Q,R) 
		if(Tin%getNameFlag().ne.0)then
			call Q%setName(2,'QR.Q')
			call R%setName(1,'QR.R')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(R)
		end if
		Q=Q%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		R=R%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine
	subroutine QRRoutineNameLeft_kill_inData(Q,R,QRName,LeftFlag_)
		class(SymTensor),intent(inout)::Q
		character(len=*),intent(in)::QRName(:)
		logical,optional,intent(in)::LeftFlag_
		type(SymTensor),intent(inout)::R
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
			call writemess("ERROR in QRSymTensor, input name larger than the rank of the Tensor",-1)
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
			call reverseSymmetryRule(Q)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call QRdecomposition_kill_inData(Q,R) 
		if(Q%getNameFlag().ne.0)then
			call Q%setName(2,'QR.Q')
			call R%setName(1,'QR.R')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(R)
		end if
		Q=Q%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		R=R%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine


	subroutine QRRoutineName(Tin,Q,R,nameU,nameV)
		character(len=*),intent(in)::nameU,nameV
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::Q,R
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		Type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call QRdecomposition(T,Q,R) 
		if(Tin%getNameFlag().ne.0)then
			call Q%setName(2,'QR.Q')
			call R%setName(1,'QR.R')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(R)
		end if
		Q=Q%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		R=R%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine

	subroutine QRRoutineName_kill_inData(Q,R,nameU,nameV)
		character(len=*),intent(in)::nameU,nameV
		class(SymTensor),intent(inout)::Q
		type(SymTensor),intent(inout)::R
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
			call reverseSymmetryRule(Q)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call QRdecomposition_kill_inData(Q,R) 
		if(Q%getNameFlag().ne.0)then
			call Q%setName(2,'QR.Q')
			call R%setName(1,'QR.R')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(R)
		end if
		Q=Q%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		R=R%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine


















	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                     LQ   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************









	subroutine LQdecomposition(T,L,Q) 
		class(SymTensor),intent(in)::T
		type(SymTensor),intent(inout)::L,Q
		type(Symdimension)::Ldimen,Qdimen
		integer::M,N,K,classtype,indexs,i,j,deg,deg1,deg2
		type(QuanNum)::NewQuanNum,quanNum1,quanNum2
		real*4::Q1,Q2
		if(T%getRank().ne.2) then
			call writemess("ERROR in LQ decomposition",-1)
			call writemess("input SymTensor should be a matrix",-1)
			call error_stop()
		endif
		if(T%getTotalBlock().eq.0) then
			call writemess("ERROR in LQ decomposition",-1)
			call writemess("There is no block in the SymTensor",-1)
			call error_stop()
		endif
		M=T%dim(1)
		N=T%dim(2)
		quanNum1=T%QuantumNumber(1)
		quanNum2=T%QuantumNumber(2)
		call SelectQuantumNumber(quanNum1,quanNum2,NewQuanNum)
		classtype=T%getType()
		call NewQuanNum%setRule(quanNum2%getRule())
		Ldimen=(T%SymDimension.subdim.1)+NewQuanNum
		call NewQuanNum%setRule(quanNum1%getRule())
		Qdimen=NewQuanNum+(T%SymDimension.subdim.2)
		call L%empty()
		call Q%empty()
		call L%allocate(Ldimen,classtype)
		call Q%allocate(Qdimen,classtype)
		K=Q%dim(1)
		do j=1,N
			do i=1,M
				if(T%getFlag((/i,j/)))then
					Q1=T%getQN(1,i)
					Q2=T%getQN(2,j)
					if(abs(Q1-Q2).ge.default_zero_real_number)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in LQ on SymTensor")
						call writemess("The quantum number are:"+Q1+' , '+Q2)
						call error_stop()
					end if
					indexs=NewQuanNum%getIndex(Q1)
					if(indexs.eq.0)then
						call writemess("ERROR in QR on SymTensor, NO.2")
						call writemess("The quantum number are:"+Q1+' , '+Q2)
						call error_stop()
					end if
					call LQ_subroutine(T%block,L%block,Q%block,M,N,K,i,j,indexs) 
					L%TotalBlock=L%TotalBlock+1
					Q%TotalBlock=Q%TotalBlock+1
				end if
			end do
		end do
		if(ProductTensor_output_check_flag)then
			call L%checkClassType()
			call Q%checkClassType()
		end if
		return
	end subroutine

	subroutine LQdecomposition_kill_inData(Q,L) 
		class(SymTensor),intent(inout)::Q
		type(SymTensor),intent(inout)::L
		type(Symdimension)::Ldimen,Qdimen
		integer::M,N,K,classtype,indexs,i,j,deg,deg1,deg2
		type(QuanNum)::quanNum1
		real*4::Q1,Q2
		type(SymTensor)::T
		if(Q%getRank().ne.2) then
			call writemess("ERROR in LQ decomposition",-1)
			call writemess("input SymTensor should be a matrix",-1)
			call error_stop()
		endif
		if(Q%getTotalBlock().eq.0) then
			call writemess("ERROR in LQ decomposition",-1)
			call writemess("There is no block in the SymTensor",-1)
			call error_stop()
		endif
		M=Q%dim(1)
		N=Q%dim(2)
		classtype=Q%getType()

		quanNum1=Q%quantumnumber(1)
		call choosSmallDeg(quanNum1,Q%QuantumNumber(2))
		Qdimen=quanNum1+(Q.subdim.2)

		Ldimen=(Q.subdim.1)+quanNum1
		call Ldimen%setRule(2,Q%getRule(2))



		call L%empty()
		call L%allocate(Ldimen,classtype)
		call Q%reset_dim_no_check(Qdimen)

		K=L%dim(2)
		do j=1,N
			do i=1,M
				if(Q%getFlag([i,j]))then
					Q1=Q%getQN(1,i)
					Q2=Q%getQN(2,j)
					if(abs(Q1-Q2).ge.default_zero_real_number)then ! U(1), S1=1.5 S2=0.5, the diag mement is not (i,i) but (i+1,i)
						call writemess("ERROR in LQ on SymTensor")
						call writemess("The quantum number are:"+Q1+' , '+Q2)
						call error_stop()
					end if
					indexs=quanNum1%getIndex(Q1)
					if(indexs.eq.0)then
						call writemess("ERROR in LQ on SymTensor, NO.2")
						call writemess("The quantum number are:"+Q1+' , '+Q2)
						call Q%diminfo
						call error_stop()
					end if
					call LQ_subroutine_kill_inData(L%block,Q%block,M,N,K,i,j,indexs) 
					L%TotalBlock=L%TotalBlock+1
				end if
			end do
		end do
		if(ProductTensor_output_check_flag)then
			call L%checkClassType()
			call Q%checkClassType()
		end if
		return
	end subroutine


	!T = QR

	subroutine LQ_subroutine(T,L,Q,M,N,MN,i,j,k) 
		integer,intent(in)::M,N,MN,i,j,k
		type(Tensor),intent(in)::T(M,N)
		type(Tensor),intent(inout)::L(M,MN),Q(MN,N)
		Q(k,j)=T(i,j)
		call Q(k,j)%LQkill(L(i,k))
		return
	end subroutine	

	subroutine LQ_subroutine_kill_inData(L,Q,M,N,MN,i,j,k) 
		integer,intent(in)::M,N,MN,i,j,k
		type(Tensor),intent(inout)::Q(MN,N)
		type(Tensor),intent(inout)::L(M,MN)
		call Q(i,j)%LQkill(L(i,k))
		return
	end subroutine	


	subroutine LQRoutineNameLeft(Tin,L,Q,QRName,LeftFlag_)
		character(len=*),intent(in)::QRName(:)
		logical,optional,intent(in)::LeftFlag_
		class(SymTensor),intent(in)::Tin
		type(SymTensor),intent(inout)::L,Q
		logical::LeftFlag,reverFlag
		integer::lenQRName,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		Type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call LQdecomposition(T,L,Q) 
		if(Tin%getNameFlag().ne.0)then
			call L%setName(2,'LQ.L')
			call Q%setName(1,'LQ.Q')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(Q)
		end if
		L=L%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		Q=Q%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine

	subroutine LQRoutineNameLeft_kill_inData(Q,L,QRName,LeftFlag_)
		class(SymTensor),intent(inout)::Q
		character(len=*),intent(in)::QRName(:)
		logical,optional,intent(in)::LeftFlag_
		type(SymTensor),intent(inout)::L
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
			call reverseSymmetryRule(Q)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call LQdecomposition_kill_inData(Q,L)
		if(Q%getNameFlag().ne.0)then
			call L%setName(2,'LQ.L')
			call Q%setName(1,'LQ.Q')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(Q)
		end if
		L=L%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		Q=Q%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine


	subroutine LQRoutineName(Tin,L,Q,nameU,nameV)
		character(len=*),intent(in)::nameU,nameV
		type(SymTensor),intent(inout)::L,Q
		class(SymTensor),intent(in)::Tin
		integer::rankU,rankV,i,j,rank
		type(Tensor)::order(2),orderinfo(2)
		type(SymDimension)::dimen(2)
		Type(SymTensor)::T
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
			call reverseSymmetryRule(T)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call LQdecomposition(T,L,Q) 
		if(Tin%getNameFlag().ne.0)then
			call L%setName(2,'LQ.L')
			call Q%setName(1,'LQ.Q')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(Q)
		end if
		L=L%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		Q=Q%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine

	subroutine LQRoutineName_kill_inData(Q,L,nameU,nameV)
		class(SymTensor),intent(inout)::Q
		character(len=*),intent(in)::nameU,nameV
		type(SymTensor),intent(inout)::L
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
			call reverseSymmetryRule(Q)
			reverFlag=.true.
		else
			reverFlag=.false.
		end if
		call LQdecomposition_kill_inData(Q,L)
		if(Q%getNameFlag().ne.0)then
			call L%setName(2,'LQ.L')
			call Q%setName(1,'LQ.Q')
		end if
		if(reverFlag)then
			call reverseSymmetryRule(Q)
		end if
		L=L%SymSplit(dimen(1),order(1),orderinfo(1),.true.)
		Q=Q%SymSplit(dimen(2),order(2),orderinfo(2),.false.)
	end subroutine






	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                     Permutation   
	!
	!**************************************************************************************************************
	!**************************************************************************************************************


	!permutation_data*(Res,block,index_not_permute,dimens): block should be set value before calling
	!                                                       block should be the data to be permute   
	!                                                       dimens is the dimension of block
	!permutation_data*_inout(Res,block,index_not_permute,dimens): block should not be set value before calling
	!                                                             Res should be the data to be   permute
	!                                                             dimens is the dimension of Res


	type(SymTensor) function permute_rank3(T1,index_not_permute)result(Res)
		type(SymTensor),intent(in) :: T1
		integer,intent(in) ::   index_not_permute
		integer::dimen(3),lenD,i
		type(SymDimension)::newdim
		if(T1%getrank().ne.3) then
			write(*,*)"ERROR in permute_rank3 of SymTensor"
			write(*,*)"stop"
			call error_stop()
			return
		end if
		select case(index_not_permute)
			case(1)
				newdim=T1%SymDimension%Spermute((/1,3,2/))
				call Res%allocate(newdim,T1%getType())
				dimen=T1%dim()
				call Sympermutation_rank3_data1(Res%block,T1%block,dimen(1),dimen(2),dimen(3))
			case(2)
				newdim=T1%SymDimension%Spermute((/3,2,1/))
				call Res%allocate(newdim,T1%getType())
				dimen=T1%dim()
				call Sympermutation_rank3_data2(Res%block,T1%block,dimen(1),dimen(2),dimen(3))
			case(3)
				newdim=T1%SymDimension%Spermute((/2,1,3/))
				call Res%allocate(newdim,T1%getType())
				dimen=T1%dim()
				call Sympermutation_rank3_data3(Res%block,T1%block,dimen(1),dimen(2),dimen(3))
		end select
		do i=1,Res%GettotalData()
			if(Res%block(i)%getflag()) then
				Res%block(i)=Res%block(i).p.index_not_permute
			end if
		end do
		Res%TotalBlock=T1%TotalBlock
		return
	 end function
	 
	 type(SymTensor) function permute_rank2 (T)result(Res)
		type(SymTensor),intent(in) :: T
		integer::I
		type(SymDimension)::newdim
		if(T%getrank().eq.1) then
			Res=T
			return
		end if
		if(T%getrank().gt.2) then
			write(*,*)"ERROR in Dpermute_rank2"
			write(*,*)"stop"
			call error_stop()
			return
		end if
		newdim=T%SymDimension%Spermute((/2,1/))
		call Res%allocate(newdim,T%getType())
		call Sympermutation_rank2_data(Res%block,T%Block,T%dim(1),T%dim(2))
		do i=1,Res%GettotalData()
			if(Res%block(i)%getflag()) then
				Res%block(i)=.p.Res%block(i)
			end if
		end do
		Res%TotalBlock=T%TotalBlock
		return
	end function 
	subroutine permutefo_data_inout(Res,block,inde,dimen)!Block is working memery, do not need to set value
	                                                     ! Block should be allocate memery before calling
	                                                     !Do nothing to the data in the Res%Block
	                                                     !Res should be the data to be 
		type(SymTensor),intent(inout) :: Res  
		type(SymDimension),intent(inout) ::dimen
		type(SymDimension)::newdim
		type(Tensor),intent(inout)::block(:)
		integer,intent(in)::inde
		integer::rank,num
		integer::oper(2,3)
		rank=dimen%Getrank()
		if(inde.gt.rank) then
			call writemess("ERROR in function permutefo_data",-1)
			call writemess("index>rank",-1)
			call error_stop()
		end if
		if(inde.le.0) then
			call writemess("ERROR in function permutefo_data",-1)
			call writemess("index="+inde,-1)
			call error_stop()
		end if
		if(inde.eq.1) then
			return
		end if
		call SDimpermute_forwards(newdim,dimen,inde)
		if(inde.eq.rank) then
			dimen=dimen%fuseIndex(1,rank-2)
			call permutation_data2_inout(Res,block,dimen)
			dimen=newdim
			return
		end if
		num=inde-2
		dimen=dimen%fuseIndex(1,num)
		num=rank-3
		dimen=dimen%fuseIndex(3,num)
		call permutation_data3_inout(Res,block,3,dimen)
		dimen=newdim
		return
	end subroutine	 
	subroutine permuteback_data_inout(Res,block,inde,dimen)
		type(SymTensor),intent(inout) :: Res
		type(SymDimension),intent(inout) ::dimen
		type(Tensor),intent(inout)::block(:)
		integer,intent(in)::inde
		type(SymDimension)::newdim
		integer::rank,num
		integer::oper(2,3)
		rank=dimen%Getrank()
		if(inde.eq.rank) then
			return
		end if
		if(inde.le.0) then
			write(*,*)"ERROR in function permuteback_data"
			write(*,*)"index=",inde
			call error_stop()
		end if
		call SDimpermute_backwards(newdim,dimen,inde)
		if(inde.eq.1) then
			dimen=dimen%fuseIndex(2,rank-2)
			call permutation_data2_inout(Res,block,dimen)
			dimen=newdim
			return
		end if
		num=inde-2
		dimen=dimen%fuseIndex(1,num)
		num=rank-3
		dimen=dimen%fuseIndex(3,num)
		call permutation_data3_inout(Res,block,1,dimen)
		dimen=newdim
		return
	end subroutine
	 subroutine permutation_data3_inout(Res,block,index_not_permute,dimens)!Block is working memery, do not need to set value
	                                                     ! Block should be allocate memery before calling
	                                                     !Do nothing to the data in the Res%Block
		integer,intent(in) ::   index_not_permute
		type(SymTensor),intent(inout) :: Res
		type(Tensor),intent(inout)::block(:)
		type(SymDimension),intent(inout) ::dimens
		integer::dimen(3)
		dimen=dimens
		block=Res%block(1:Res%getTotalData())
		if(index_not_permute.eq.1) then
			call Sympermutation_rank3_data1(Res%Block,Block,dimen(1),dimen(2),dimen(3))
			return
		end if
		if(index_not_permute.eq.2) then
			call Sympermutation_rank3_data2(Res%Block,Block,dimen(1),dimen(2),dimen(3))
			return
		end if
		if(index_not_permute.eq.3) then
			call Sympermutation_rank3_data3(Res%Block,Block,dimen(1),dimen(2),dimen(3))
			return
		end if
	 end subroutine
	subroutine permutation_data2_inout(Res,block,dimens)
		type(SymTensor),intent(inout) :: Res
		type(Tensor),intent(inout) :: block(:)
		type(SymDimension),intent(inout) ::dimens
		integer::dimen(2)
		if(dimens%Getrank().eq.1) then
			return
		end if
		dimen=dimens
		block=Res%Block(1:Res%getTotalData())
		call Sympermutation_rank2_data(Res%Block,block,dimen(1),dimen(2))
		return
	end subroutine	
	subroutine Sympermutation_rank3_data1(outdata,indata,LD1,LD2,LD3)
		integer,intent(in)::LD1,LD2,LD3
		type(Tensor),intent(inout)::outdata(LD1,LD3,LD2)
		type(Tensor),intent(in)::indata(LD1,LD2,LD3)
		integer::i,j,k
		do j=1,LD3
			do k=1,LD2
				outdata(:,j,k)=indata(:,k,j)
			end do
		end do
		return
	end subroutine
	
	subroutine Sympermutation_rank3_data2(outdata,indata,LD1,LD2,LD3)
		integer,intent(in)::LD1,LD2,LD3
		type(Tensor),intent(inout)::outdata(LD3,LD2,LD1)
		type(Tensor),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD2
			outdata(:,i,:)=transpose(indata(:,i,:))
		end do
		return
	end subroutine
	
	subroutine Sympermutation_rank3_data3(outdata,indata,LD1,LD2,LD3)
		integer,intent(in)::LD1,LD2,LD3
		type(Tensor),intent(inout)::outdata(LD2,LD1,LD3)
		type(Tensor),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD3
			outdata(:,:,i)=transpose(indata(:,:,i))
		end do
		return
	end subroutine	
	subroutine Sympermutation_rank2_data(outdata,indata,LD1,LD2)
		integer,intent(in)::LD1,LD2
		type(Tensor),intent(inout)::outdata(LD2,LD1)
		type(Tensor),intent(in)::indata(LD1,LD2)
		outdata=transpose(indata)
		return
	end subroutine	

	
	
	
	
	
	
	
	type(SymTensor) function permutation(T,newOrder)
		type(SymTensor),intent(in) :: T
		integer,intent(in)::newOrder(:)
		integer,allocatable ::inde(:)
		integer::lenOrder,i,j
		type(Tensor),allocatable::temp(:)
		type(SymDimension)::dimen		
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutation)"
			call error_stop()
		end if
		lenorder=size(newOrder)-1
		allocate(inde(lenorder))
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		permutation=T
		allocate(temp(T%getTotalData()))
		do i=lenorder,1,-1
			call permutefo_data_inout(permutation,temp,inde(i),permutation%SymDimension)
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		do i=1,permutation%GettotalData()
			if(permutation%block(i)%getflag()) then
				call permutation%block(i)%permute(newOrder)
			end if
		end do
		return
	end function
	subroutine permutation_routine(T,newOrder)
		class(SymTensor),intent(inout) :: T
		integer,intent(in)::newOrder(:)
		integer,allocatable ::inde(:)
		integer::lenOrder,i,j
		type(Tensor),allocatable::temp(:)
		type(SymDimension)::dimen		
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutation)"
			call error_stop()
		end if
		lenorder=size(newOrder)-1
		allocate(inde(lenorder))
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		allocate(temp(T%getTotalData()))
		do i=lenorder,1,-1
			call permutefo_data_inout(T,temp,inde(i),T%SymDimension)
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		do i=1,T%GettotalData()
			if(T%block(i)%getflag()) then
				call T%block(i)%permute(newOrder)
			end if
		end do
		return
	end subroutine
	
	type(SymTensor) function permutation_name(T,newOrderchar)result(permutation)
		type(SymTensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::newOrderchar(:)
		integer,allocatable::newOrder(:)
		integer,allocatable ::inde(:)
		integer::lenOrder,i,j
		type(SymDimension)::dimen		
		type(Tensor),allocatable::temp(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutation)"
			call error_stop()
		end if
		allocate(newOrder(size(newOrderchar)))
		newOrder=T%FindOrder(newOrderchar)
		lenorder=size(newOrder)-1
		allocate(inde(lenorder))
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		permutation=T
		allocate(temp(T%getTotalData()))
		do i=lenorder,1,-1
			call permutefo_data_inout(permutation,temp,inde(i),permutation%SymDimension)
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		do i=1,permutation%GettotalData()
			if(permutation%block(i)%getflag()) then
				call permutation%block(i)%permute(newOrder)
			end if
		end do
		return
	end function
	
	subroutine permutation_name_routine(T,newOrderchar)
		class(SymTensor),intent(inout) :: T
		CHARACTER(len=*),intent(in)::newOrderchar(:)
		integer,allocatable::newOrder(:)
		integer,allocatable ::inde(:)
		integer::lenOrder,i,j
		type(SymDimension)::dimen	
		type(Tensor),allocatable::temp(:)	
		if(.not.T%getflag())then
			write(*,*)"There is no data in the SymTensor,(permutation)"
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			write(*,*)"split Symdimension before calling permuation on name"
			call error_stop()
		end if
		allocate(newOrder(size(newOrderchar)))
		newOrder=T%FindOrder(newOrderchar)
		lenorder=size(newOrder)-1
		allocate(inde(lenorder))
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		allocate(temp(T%getTotalData()))
		do i=lenorder,1,-1
			call permutefo_data_inout(T,temp,inde(i),T%SymDimension)
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		do i=1,T%GettotalData()
			if(T%block(i)%getflag()) then
				call T%block(i)%permute(newOrder)
			end if
		end do
		return
	end subroutine
	type(SymTensor) function permutation_Tensor(T,Order)result(Res)
		type(SymTensor),intent(in) :: T
		type(Tensor),intent(in)::Order
		integer,allocatable ::inde(:)
		character(len=max_len_of_char_in_TData),allocatable ::charinde(:)
		select case(Order%getType())
			case (1)
				allocate(inde(Order%getTotalData()))
				Res=permutation(T,inde)
			case (7)
				allocate(charinde(Order%getTotalData()))
				Res=permutation_name(T,charinde)
			case default
				call writemess('error in permutation, the data type of order')
				call error_Stop()
		end select
		return
	end function
	subroutine permutation_Tensor_routine(T,Tenorder)
		class(SymTensor),intent(inout)::T
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
				call writemess('error in permutation, the data type of order')
				call error_Stop()	
		end select
		return
	end subroutine
	
	
	type(SymTensor) function permutefo(T,inde)
		type(SymTensor),intent(in)::T
		integer,intent(in)::inde
		integer::rank,i
		type(Tensor),allocatable::Temp(:)
		permutefo=T
		allocate(temp(T%getTotalData()))
		call permutefo_Data_inout(permutefo,Temp,inde,permutefo%SYmDimension)
		do i=1,permutefo%GettotalData()
			if(permutefo%block(i)%getflag()) then
				call permutefo%block(i)%forward(inde)
			end if
		end do
		return
	end function
	subroutine permutefo_routine(T,inde)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::inde
		INTEGER::i
		type(Tensor),allocatable::temp(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutefo_vec)"
			call error_stop()
		end if
		allocate(temp(T%getTotalData()))
		call permutefo_data_inout(T,temp,inde,T%SymDimension)
		do i=1,T%GettotalData()
			if(T%block(i)%getflag()) then
				call T%block(i)%forward(inde)
			end if
		end do
		return
	end subroutine
	type(SymTensor) function permutefo_name(T,indechar)result(permutefo)
		type(SymTensor),intent(in)::T
		character(len=*),intent(in)::indechar
		integer::inde
		integer::rank,i
		type(Tensor),allocatable::Temp(:)
		rank=T%getRank()
		if(long_Name_logi(indechar))then
			inde=T%FindOrder(indechar)
			permutefo=T
			allocate(temp(T%getTotalData()))
			call permutefo_data_inout(permutefo,temp,inde,permutefo%SymDimension) 
			do i=1,permutefo%GettotalData()
				if(permutefo%block(i)%getflag()) then
					call permutefo%block(i)%forward(inde)
				end if
			end do
			return
		else
			permutefo=T
			call permutefo_name_routine(permutefo,indechar)
			return
		end if
	end function
	subroutine permutefo_name_routine(T,indechar)
		class(SymTensor),intent(inout)::T
		integer::inde,i,j,lenintindex,sizeindechar
		character(len=len_of_Name)::indexname
		character(len=*),intent(in)::indechar
		integer,allocatable::charToInt(:)
		type(Tensor),allocatable::temp(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the SymTensor,(permutefo_vec)"
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			write(*,*)"split dimension before calling permuation on name"
			call error_stop()
		end if
		if(long_Name_logi(indechar))then
			inde=T%FindOrder(indechar)
			allocate(temp(T%getTotalData()))
			call permutefo_data_inout(T,temp,inde,T%SymDimension)
			do j=1,T%GettotalData()
				if(T%block(j)%getflag()) then
					call T%block(j)%forward(inde)
				end if
			end do
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
			do j=1,T%GettotalData()
				if(T%block(j)%getflag()) then
					call T%block(j)%forward(charToInt(:lenintindex))
				end if
			end do
			
			i=T%getRank()
			allocate(temp(T%getTotalData()))
			do inde=T%getRank(),1,-1
				indexname=T%outTensorName(i)
				if(indechar.equ.indexname)then
					call permutefo_data_inout(T,temp,i,T%SymDimension)
				else
					i=i-1
				end if
			end do
		
			
		end if
		return
	end subroutine
!*****************permutefo******************************
!		T_{1,2,3,..,j,..,i,.,k,...,n},permutefo(T,(/i,j,k/))=_{i,j,k,1,2,3,...,n}
!
	type(SymTensor) function permutefo_vec(T,vec_)
		type(SymTensor),intent(in)::T
		integer,intent(in)::vec_(:)
		integer::lenVec,i,j
		type(Tensor),allocatable::temp(:)
		integer::vec(size(vec_))
		if(.not.T%getflag())then
			write(*,*)"There is no data in the SymTensor,(permutefo_vec)"
			call error_stop()
		end if
		lenVec=size(vec_)
		vec=vec_
		permutefo_vec=T
		allocate(temp(T%getTotalData()))
		do i=lenVec,1,-1
			call permutefo_data_inout(permutefo_vec,temp,vec(i),permutefo_vec%SymDimension)
			do j=1,i-1
				if(vec(j).lt.vec(i))then
					vec(j)=vec(j)+1
				end if
			end do
		end do
		do i=1,permutefo_vec%GettotalData()
			if(permutefo_vec%block(i)%getflag()) then
				call permutefo_vec%block(i)%forward(vec_)
			end if
		end do
		return
	end function
	subroutine permutefo_vec_routine(T,vec_)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::vec_(:)
		integer::lenVec,i,j
		integer::vec(size(vec_))
		type(Tensor),allocatable::temp(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutefo_vec)"
			call error_stop()
		end if
		lenVec=size(vec_)
		vec=vec_
		allocate(temp(T%getTotalData()))
		do i=lenVec,1,-1
			call permutefo_data_inout(T,temp,vec(i),T%SymDimension)
			do j=1,i-1
				if(vec(j).lt.vec(i))then
					vec(j)=vec(j)+1
				end if
			end do
		end do
		do i=1,T%GettotalData()
			if(T%block(i)%getflag()) then
				call T%block(i)%forward(vec_)
			end if
		end do
		return
	end subroutine
	
	type(SymTensor) function permutefo_vec_name(T,indechar)result(permutefo_vec)
		type(SymTensor),intent(in)::T
		character(len=*),intent(in)::indechar(:)
		integer::lenVec,i,j
		integer::vec(size(indechar)),vec0(size(indechar))
		type(Tensor),allocatable::temp(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutefo_vec_name)"
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			write(*,*)"split dimension before calling permuation on name"
			call error_stop()
		end if
		lenVec=size(indechar)
		vec=T%FindOrder(indechar)
		vec0=vec
		permutefo_vec=T
		allocate(temp(T%getTotalData()))
		do i=lenVec,1,-1
			call permutefo_data_inout(permutefo_vec,temp,vec(i),permutefo_vec%SymDimension)
			do j=1,i-1
				if(vec(j).lt.vec(i))then
					vec(j)=vec(j)+1
				end if
			end do
		end do
		do i=1,permutefo_vec%GettotalData()
			if(permutefo_vec%block(i)%getflag()) then
				call permutefo_vec%block(i)%forward(vec0)
			end if
		end do
		
		return
	end function
	subroutine permutefo_vec_name_routine(T,indechar)
		class(SymTensor),intent(inout)::T
		character(len=*),intent(in)::indechar(:)
		integer::lenVec,i,j
		integer::vec(size(indechar)),vec0(size(indechar))
		type(Tensor),allocatable::temp(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutefo_vec_name)"
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			write(*,*)"split dimension before calling permuation on name"
			call error_stop()
		end if
		lenVec=size(indechar)
		vec=T%FindOrder(indechar)
		vec0=vec
		allocate(temp(T%getTotalData()))
		do i=lenVec,1,-1
			call permutefo_data_inout(T,temp,vec(i),T%SymDimension)
			do j=1,i-1
				if(vec(j).lt.vec(i))then
					vec(j)=vec(j)+1
				end if
			end do
		end do
		
		do i=1,T%GettotalData()
			if(T%block(i)%getflag()) then
				call T%block(i)%forward(vec0)
			end if
		end do
		
		return
	end subroutine
	
	type(SymTensor) function permutefo_Tensor(T,Tenorder)result(permutefo)
		type(SymTensor),intent(in)::T
		type(Tensor),intent(in)::Tenorder
		character(len=max_len_of_char_in_TData),allocatable::indechar(:)
		integer,allocatable::indeint(:)
		select case (Tenorder%getType())
			case (1)
				allocate(indeint(Tenorder%getTotalData()))
				indeint=Tenorder
				permutefo=permutefo_vec(T,indeint)
			case (7)
				allocate(indechar(Tenorder%getTotalData()))
				indechar=Tenorder
				permutefo=permutefo_vec_name(T,indechar)
			case default
				call writemess('error in permutation, the data type of order')
				call error_Stop()	
		end select
		return
	end function
	subroutine permutefo_Tensor_routine(T,Tenorder)
		class(SymTensor),intent(inout)::T
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
				call writemess('error in permutation, the data type of order')
				call error_Stop()	
		end select
		return
	end subroutine
	
	!*****************permuteback******************************
	!		T_{1,2,3,..,i,..,n},permuteback(T,i)=_{1,2,3,..,i-1,i+1,..,n,i}
	!
	type(SymTensor) function permuteback(T,inde)
		type(SymTensor),intent(in)::T
		integer,intent(in)::inde
		integer::rank,i
		type(Tensor),allocatable::temp(:)
		permuteback=T
		allocate(temp(T%getTotalData()))
		call permuteback_data_inout(permuteback,temp,inde,permuteback%SymDimension)
		do i=1,permuteback%GettotalData()
			if(permuteback%block(i)%getflag()) then
				call permuteback%block(i)%backward(inde)
			end if
		end do
		return
	end function
	subroutine permuteback_routine(T,inde)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::inde
		type(Tensor),allocatable::temp(:)
		integer::i
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permuteback_vec)"
			call error_stop()
		end if
		allocate(temp(T%getTotalData()))
		call permuteback_data_inout(T,temp,inde,T%SymDimension)
		do i=1,T%GettotalData()
			if(T%block(i)%getflag()) then
				call T%block(i)%backward(inde)
			end if
		end do
		return
	end subroutine
	
	subroutine permuteback_name_routine(T,indechar)
		class(SymTensor),intent(inout)::T
		character(len=*),intent(in)::indechar
		integer::inde,i,sizeinde,lenintindex,j
		character(len=len_of_Name)::indexname
		integer,allocatable::indes(:)
		type(Tensor),allocatable::Temp(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permuteback_vec)"
			call error_stop()
		end if
		if(.not.T%if_original_dim())then
			write(*,*)"split dimension before calling permuation on name"
			call error_stop()
		end if
		if(long_Name_logi(indechar))then
			inde=T%FindOrder(indechar)
			allocate(Temp(T%getTotalData()))
			call permuteback_data_inout(T,Temp,inde,T%SymDimension)
			do i=1,T%GettotalData()
				if(T%block(i)%getflag()) then
					call T%block(i)%backward(inde)
				end if
			end do
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
			do j=1,T%GettotalData()
				if(T%block(j)%getflag()) then
					call T%block(j)%backward(indes(:lenintindex))
				end if
			end do
			
			
			i=1
			allocate(Temp(T%getTotalData()))
			do inde=1,T%getRank()
				indexname=T%outTensorName(i)
				if(indechar.equ.indexname)then
					call permuteback_data_inout(T,Temp,i,T%SymDimension)
				else
					i=i+1
				end if
			end do
		end if
		return
	end subroutine
	type(SymTensor) function permuteback_name(T,indechar)
		type(SymTensor),intent(in)::T
		character(len=*),intent(in)::indechar
		integer::inde
		if(long_Name_logi(indechar))then
			inde=T%FindOrder(indechar)
			permuteback_name=permuteback(T,inde)
			return
		end if
		permuteback_name=T
		call permuteback_name_routine(permuteback_name,indechar)
		return
	end function
	!*****************permuteback******************************
	!		T_{1,2,3,..,i,..,n},permuteback(T,i)=_{1,2,3,..,i-1,i+1,..,n,i}
	!
	type(SymTensor) function permuteback_vec(T,vec_)
		type(SymTensor),intent(in)::T
		integer,intent(in)::vec_(:)
		integer::rank,lenVec,i,j
		integer::vec(size(vec_))
		type(Tensor),allocatable::Temp(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permuteback_vec)"
			call error_stop()
		end if
		rank=T%getRank()
		lenVec=size(vec_)
		vec=vec_
		permuteback_vec=T
		allocate(temp(T%getTotalData()))
		do i=1,lenVec
			call permuteback_data_inout(permuteback_vec,temp,vec(i),permuteback_vec%SymDimension)
			do j=lenVec,i+1,-1
				if(vec(j).gt.vec(i))then
					vec(j)=vec(j)-1
				end if
			end do
		end do
		do j=1,permuteback_vec%GettotalData()
			if(permuteback_vec%block(j)%getflag()) then
				call permuteback_vec%block(j)%backward(vec_)
			end if
		end do
		
		return
	end function
	
	type(SymTensor) function permuteback_vec_name(T,indechar)
		type(SymTensor),intent(in)::T
		character(len=*),intent(in)::indechar(:)
		integer::vec(size(indechar))
		vec=T%FindOrder(indechar)
		permuteback_vec_name=permuteback_vec(T,vec)
		return
	end function
	
	subroutine permuteback_vec_routine(T,vec_)
		class(SymTensor),intent(inout)::T
		integer,intent(in)::vec_(:)
		integer::rank,lenVec,i,j
		integer::vec(size(vec_))
		type(Tensor),allocatable::temp(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permuteback_vec)"
			call error_stop()
		end if
		rank=T%getRank()
		lenVec=size(vec_)
		vec=vec_
		allocate(temp(T%getTotalData()))
		do i=1,lenVec
			call permuteback_data_inout(T,temp,vec(i),T%SymDimension)
			do j=lenVec,i+1,-1
				if(vec(j).gt.vec(i))then
					vec(j)=vec(j)-1
				end if
			end do
		end do
		do j=1,T%GettotalData()
			if(T%block(j)%getflag()) then
				call T%block(j)%backward(vec_)
			end if
		end do
		return
	end subroutine
	subroutine permuteback_vec_name_routine(T,indechar)
		class(SymTensor),intent(inout)::T
		character(len=*),intent(in)::indechar(:)
		integer::vec(size(indechar)),rank
		rank=T%getRank()
		vec=T%FindOrder(indechar)
		call permuteback_vec_routine(T,vec)
		return
	end subroutine
	
	type(SymTensor) function permuteback_Tensor(T,Tenorder)result(permutefo)
		type(SymTensor),intent(in)::T
		type(Tensor),intent(in)::Tenorder
		character(len=max_len_of_char_in_TData),allocatable::indechar(:)
		integer,allocatable::indeint(:)
		select case (Tenorder%getType())
			case (1)
				allocate(indeint(Tenorder%getTotalData()))
				indeint=Tenorder
				permutefo=permuteback_vec(T,indeint)
			case (7)
				allocate(indechar(Tenorder%getTotalData()))
				indechar=Tenorder
				permutefo=permuteback_vec_name(T,indechar)
			case default
				call writemess('error in permutation, the data type of order')
				call error_Stop()	
		end select
		return
	end function
	subroutine permuteback_Tensor_routine(T,Tenorder)
		class(SymTensor),intent(inout)::T
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
				call writemess('error in permutation, the data type of order')
				call error_Stop()	
		end select
		return
	end subroutine



	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                  contract
	!
	!**************************************************************************************************************
	!**************************************************************************************************************	
	!******************  contract  *********************

	!	T1:[i1,i2,i3,i4,i5,i6,i7,i8]
	!	T2:[j1,j2,j3,j4,j5,j6,j7,j8,j9,j10]
	!	i1=(/5,1,2/)
	!	i2=(10,3,5,6)
	!	then the result will be T1'*T2'
	!	where ,
	!	T1'=[i3,i4,i6,i7,i8,(i5*i1*i2)]
	!	T2'=[(j10*j3*j5*j6),j1,j2,j4,j7,j8,j9]
	!	output T1'*T2
	! 	input Tensor should be in its original dimenison,there is no contract on it
	!	if present len_of_contract, len_of_contract specify the length of  i1, and i2
	

	subroutine check_Contact_routine1(T1,ith1,T2,ith2)
		type(SymTensor),intent(in)::T1,T2
		integer,intent(in)::ith1,ith2
		integer::SymRule1,SymRule2
		SymRule1=T1%getRule(ith1)
		SymRule2=T2%getRule(ith2)
		call checkSymmetryRule(SymRule1,SymRule2,T1%getName(ith1),T2%getName(ith2))
		return
	end subroutine
	subroutine check_Contact_routine2(T1,ith1,T2,ith2)
		type(SymTensor),intent(in)::T1,T2
		integer,intent(in)::ith1(:),ith2(:)
		integer::i,SymRule1,SymRule2
		do i=1,size(ith1)
			SymRule1=T1%getRule(ith1(i))
			SymRule2=T2%getRule(ith2(i))
			call checkSymmetryRule(SymRule1,SymRule2,T1%getName(ith1(i)),T2%getName(ith2(i)))
		end do
		return
	end subroutine
	subroutine check_Contact_routine3(T1,ith1,T2,ith2)
		type(SymTensor),intent(in)::T1,T2
		character(len=*),intent(in)::ith1,ith2
		integer::SymRule1,SymRule2
		SymRule1=T1%getRule(ith1)
		SymRule2=T2%getRule(ith2)
		call checkSymmetryRule(SymRule1,SymRule2,ith1,ith2)
		return
	end subroutine
	subroutine check_Contact_routine4(T1,ith1,T2,ith2)
		type(SymTensor),intent(in)::T1,T2
		character(len=*),intent(in)::ith1(:),ith2(:)
		integer::i,SymRule1,SymRule2
		do i=1,size(ith1)
			SymRule1=T1%getRule(ith1(i))
			SymRule2=T2%getRule(ith2(i))
			call checkSymmetryRule(SymRule1,SymRule2,ith1(i),ith2(i))
		end do
		return
	end subroutine
	subroutine check_Contact_routine5(T,ith1,ith2)
		type(SymTensor),intent(in)::T
		integer,intent(in)::ith1,ith2
		integer::SymRule1,SymRule2
		SymRule1=T%getRule(ith1)
		SymRule2=T%getRule(ith2)
		call checkSymmetryRule(SymRule1,SymRule2,T%getName(ith1),T%getName(ith2))
		return
	end subroutine
	subroutine check_Contact_routine6(T,ith1,ith2)
		type(SymTensor),intent(in)::T
		character(len=*),intent(in)::ith1,ith2
		integer::SymRule1,SymRule2
		SymRule1=T%getRule(ith1)
		SymRule2=T%getRule(ith2)
		call checkSymmetryRule(SymRule1,SymRule2,ith1,ith2)
		return
	end subroutine

	type(SymTensor) function contract_noName(T1_,i1,T2_,i2,len_of_contract) result(T)
		type(SymTensor),intent(in) :: T1_,T2_
		integer,intent(in) :: i1(:),i2(:)
		integer,optional,intent(in)::len_of_contract(2)
		type(SymTensor) :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		rank1=T1_%rank
		rank2=T2_%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call check_Contact(T1_,i1(1:leni1),T2_,i2(1:leni2))
		T1=T1_.pb.i1(1:leni1)
		call T1%fuse(rank1-leni1+1,rank1)
		T2=T2_.pf.i2(1:leni2)
		call T2%fuse(1,leni2)
		T=T1 * T2
		return
	end function
	type(SymTensor) function contract_noName2(T1_,i1,T2_,i2) result(T)
		type(SymTensor),intent(in) :: T1_,T2_
		integer,intent(in) :: i1,i2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		call check_Contact(T1_,i1,T2_,i2)
		T = (T1_.pb.i1) * (T2_.pf.i2)
		return
	end function
	type(SymTensor) function contract_name(T1_,name1,T2_,name2,len_of_contract) result(T)
		type(SymTensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		type(SymTensor) :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.(T1_%if_original_dim().and.T2_%if_original_dim())) then
			write(*,*)"ERROR in Tenproduct"
			write(*,*)"stop"
			call error_stop()
		end if
		i1=T1_%FindOrder(name1)
		i2=T2_%FindOrder(name2)
		rank1=T1_%rank
		rank2=T2_%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call check_Contact(T1_,i1(1:leni1),T2_,i2(1:leni2))
		T1=T1_.pb.i1(1:leni1)
		call T1%fuse(rank1-leni1+1,rank1)
		T2=T2_.pf.i2(1:leni2)
		call T2%fuse(1,leni2)
		T=T1 * T2
		return
	end function
	type(SymTensor) function contract_name2(T1_,name1,T2_,name2) result(T)
		type(SymTensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name1,name2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.(T1_%if_original_dim().and.T2_%if_original_dim())) then
			write(*,*)"ERROR in Tenproduct"
			write(*,*)"stop"
			call error_stop()
		end if
		call check_Contact(T1_,name1,T2_,name2)
		T= (T1_.pb.name1) * (T2_.pf.name2)
		return
	end function

	!******************  contract the same names  *********************	

	subroutine find_same_name(inname1,inname2,SameName,lenofname)
		character(len=*),intent(in)::inname1(:),inname2(:)
		character(len=*),intent(inout)::SameName(:)
		integer,intent(inout)::lenofname
		integer::i,j,k
		k=1
		lenofname=0
		do i=1,size(inname1)
			do j=1,size(inname2)
				if(inname1(i).equ.inname2(j))then
					SameName(k)=inname1(i)
					lenofname=lenofname+1
					k=k+1
				end if
			end do
		end do
		return
	end subroutine

	type(SymTensor) function contract_Same_name(T1_,T2_) result(T)
		type(SymTensor),intent(in) :: T1_,T2_
		type(SymTensor)::T1,T2
		character(len=len_of_Name+len_of_Name),allocatable::Samename(:),name1(:),name2(:)
		integer::lenofname,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.(T1_%if_original_dim().and.T2_%if_original_dim())) then
			write(*,*)"ERROR in Tenproduct"
			write(*,*)"stop"
			call error_stop()
		end if
		rank1=T1_%getRank()
		rank2=T2_%getRank()
		allocate(name1(rank1))
		allocate(name2(rank2))
		allocate(Samename(max(rank1,rank2) ))
		name1=.iname.T1_
		name2=.iname.T2_
		call find_same_name(name1,name2,SameName,lenofname) 
		call check_Contact(T1_,SameName(1:lenofname),T2_,SameName(1:lenofname))
		T1=T1_.pb.SameName(1:lenofname)
		call T1%fuse(rank1-lenofname+1,rank1)
		T2=T2_.pf.SameName(1:lenofname)
		call T2%fuse(1,lenofname)
		T=T1 * T2
		return
	end function
	type(SymTensor) function contract_name_int(T1_,name1,T2_,i2,len_of_contract) result(T)
		type(SymTensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name1(:)
		integer,intent(in)::i2(:)
		integer :: i1(size(name1))
		integer,optional,intent(in)::len_of_contract(2)
		type(SymTensor) :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T1_%if_original_dim()) then
			write(*,*)"ERROR in Tenproduct"
			write(*,*)"stop"
			call error_stop()
		end if
		i1=T1_%FindOrder(name1)
		rank1=T1_%rank
		rank2=T2_%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call check_Contact(T1_,i1(1:leni1),T2_,i2(1:leni2))
		T1=T1_.pb.i1(1:leni1)
		call T1%fuse(rank1-leni1+1,rank1)
		T2=T2_.pf.i2(1:leni2)
		call T2%fuse(1,leni2)
		T=T1 * T2
		return
	end function
	type(SymTensor) function contract_name_int2(T1_,name1,T2_,i2) result(T)
		type(SymTensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name1
		integer,intent(in)::i2
		integer::i1
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T1_%if_original_dim()) then
			write(*,*)"ERROR in Tenproduct"
			write(*,*)"stop"
			call error_stop()
		end if
		i1=T1_%FindOrder(name1)
		call check_Contact(T1_,i1,T2_,i2)
		T = (T1_.pb.name1) * (T2_.pf.i2)
		return
	end function

	type(SymTensor) function contract_int_name(T1_,i1,T2_,name2,len_of_contract) result(T)
		type(SymTensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name2(:)
		integer,intent(in)::i1(:)
		integer,optional,intent(in)::len_of_contract(2)
		integer :: i2(size(name2))
		type(SymTensor) :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%if_original_dim()) then
			write(*,*)"ERROR in Tenproduct"
			write(*,*)"stop"
			call error_stop()
		end if
		i2=T2_%FindOrder(name2)
		rank1=T1_%rank
		rank2=T2_%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call check_Contact(T1_,i1(1:leni1),T2_,i2(1:leni2))
		T1=T1_.pb.i1(1:leni1)
		call T1%fuse(rank1-leni1+1,rank1)
		T2=T2_.pf.i2(1:leni2)
		call T2%fuse(1,leni2)
		T=T1 * T2
		return
	end function
	type(SymTensor) function contract_int_name2(T1_,i1,T2_,name2) result(T)
		type(SymTensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name2
		integer,intent(in)::i1
		integer::i2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second SymTensor, when contracting SymTensor')
			call error_stop()
		end if
		if(.not.T2_%if_original_dim()) then
			write(*,*)"ERROR in Tenproduct"
			write(*,*)"stop"
			call error_stop()
		end if
		i2=T2_%FindOrder(name2)
		call check_Contact(T1_,i1,T2_,i2)
		T = (T1_.pb.i1) * (T2_.pf.name2)
		return
	end function
	
	subroutine contract_name_routine(T,T1,name1,T2,name2,len_of_contract) 
		class(SymTensor),target::T
		type(SymTensor),target:: T1,T2
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2
		class(SymTensor),pointer::pT
		type(SymTensor),pointer::pT1,pT2
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(T1%if_original_dim().and.T2%if_original_dim())) then
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
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call check_Contact(T1,i1(1:leni1),T2,i2(1:leni2))


		call T1%backward(i1(1:leni1))
		call T1%fuse(rank1-leni1+1,rank1)
		call T2%forward(i2(1:leni2))
		call T2%fuse(1,leni2)
		call T%ProductTensorRoutine(T1 , T2  )
		call T1%split( )
		call T2%split( )
		return
	end subroutine
	
	subroutine contract_name_routine1(T,name1,T2,name2,len_of_contract) 
		class(SymTensor),target::T
		type(SymTensor),target :: T2
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2
		class(SymTensor),pointer::pT
		type(SymTensor),pointer::pT2
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(T%if_original_dim().and.T2%if_original_dim())) then
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
		i1=T%FindOrder(name1)
		i2=T2%FindOrder(name2)
		rank1=T%getRank()
		rank2=T2%getRank()
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call check_Contact(T,i1(1:leni1),T2,i2(1:leni2))
		call T%backward(i1(1:leni1))
		call T%fuse(rank1-leni1+1,rank1)
		call T2%forward(i2(1:leni2))
		call T2%fuse(1,leni2)
		T=T*T2
		call T2%split()
		return
	end subroutine
	subroutine contract_name_routine2(T,T1,name1,name2,len_of_contract) 
		class(SymTensor),target::T
		type(SymTensor),target :: T1
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2
		class(SymTensor),pointer::pT
		type(SymTensor),pointer::pT1
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(T1%if_original_dim().and.T%if_original_dim())) then
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
		i1=T1%FindOrder(name1)
		i2=T%FindOrder(name2)
		rank1=T1%getrank()
		rank2=T%getrank()
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call check_Contact(T1,i1(1:leni1),T,i2(1:leni2))
		call T1%backward(i1(1:leni1))
		call T1%fuse(rank1-leni1+1,rank1)
		call T%forward(i2(1:leni2))
		call T%fuse(1,leni2)
		T=T1*T
		call T1%split( )
		return
	end subroutine
	subroutine contract_name_routine4(T,T1,name1,T2,name2) 
		class(SymTensor),target::T
		type(SymTensor),target:: T1,T2
		character(len=*),intent(in)::name1,name2
		class(SymTensor),pointer::pT
		type(SymTensor),pointer::pT1,pT2
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(T1%if_original_dim().and.T2%if_original_dim())) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		pT=>T
		pT1=>T1
		pT2=>T2
		if(associated(pT,pT1).or.associated(pT,pT2).or.associated(pT1,pT2))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract(A,name1,B,name2)')
			call writemess('T, A and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT1=>null()
		pT2=>null()
		call check_Contact(T1,name1,T2,name2)
		call T1%backward(name1)
		call T2%forward(name2)
		call T%ProductTensorRoutine(T1 , T2  )
		return
	end subroutine
	
	subroutine contract_name_routine5(T,name1,T2,name2) 
		class(SymTensor),target::T
		type(SymTensor),target:: T2
		character(len=*),intent(in)::name1,name2
		class(SymTensor),pointer::pT
		type(SymTensor),pointer::pT2
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(T2%if_original_dim())) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		pT=>T
		pT2=>T2
		if(associated(pT,pT2))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract(name1,B,name2)')
			call writemess('T and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT2=>null()
		call check_Contact(T,name1,T2,name2)
		call T%backward(name1)
		call T2%forward(name2)
		T=T*T2
		return
	end subroutine
	
	subroutine contract_name_routine6(T,T1,name1,name2) 
		class(SymTensor),target::T
		type(SymTensor),target:: T1
		character(len=*),intent(in)::name1,name2
		class(SymTensor),pointer::pT
		type(SymTensor),pointer::pT1
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(T1%if_original_dim())) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		pT=>T
		pT1=>T1
		if(associated(pT,pT1))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract(A,name1,name2)')
			call writemess('T and A can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT1=>null()
		call check_Contact(T1,name1,T,name2)
		call T1%backward(name1)
		call T%forward(name2)
		T=T1*T
		return
	end subroutine


	subroutine contract_name_ownlegs_routine(T,name1,name2) 
		class(SymTensor)::T
		character(len=*),intent(in)::name1,name2
		type(SymTensor)::tempT
		type(SymDimension)::NewDimen
		integer::rank,i,k,classtype,dim1
		type(Tensor),pointer::Tenp(:,:,:),Newp(:)
		type(Tensor)::blockTensor
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
		call check_Contact(T,name1,name2)
		if(rank.eq.2)then
			call writemess(' Do not finsihed the code of rank=2, for T%contract(name1,name2)')
			call error_stop
		end if
		tempT=T.pf.name1
		call tempT%forward(name2)
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
			if(Newp(k)%getFlag())T%TotalBlock=T%TotalBlock+1
		end do
		return
	end subroutine

	subroutine contract_ownlegs_routine(T,ith1,ith2) 
		class(SymTensor)::T
		integer,intent(in)::ith1,ith2
		type(SymTensor)::tempT
		type(SymDimension)::NewDimen
		integer::rank,i,k,classtype,dim1
		type(Tensor),pointer::Tenp(:,:,:),Newp(:)
			type(Tensor)::blockTensor
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
		call check_Contact(T,ith1,ith2)
		if(rank.eq.2)then
			call writemess(' Do not finsihed the code of rank=2, for T%contract(name1,name2)')
			call error_stop
		end if
		tempT=T.pf.ith1
		call tempT%forward(ith2)
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
			if(Newp(k)%getFlag())T%TotalBlock=T%TotalBlock+1
		end do
		return
	end subroutine

	type(SymTensor) function contract_name_ownlegs(Tin,name1,name2) Result(Res)
		type(SymTensor),intent(in)::Tin
		character(len=*),intent(in)::name1,name2
		type(SymTensor)::tempT
		type(SymDimension)::NewDimen
		integer::rank,i,k,classtype,dim1
		type(Tensor),pointer::Tenp(:,:,:),Newp(:)
			type(Tensor)::blockTensor
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
		call check_Contact(Tin,name1,name2)
		if(rank.eq.2)then
			call writemess(' Do not finsihed the code of rank=2, for T%contract(name1,name2)')
			call error_stop
		end if
		tempT=Tin.pf.name1
		call tempT%forward(name2)
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
			if(Newp(k)%getFlag())Res%TotalBlock=Res%TotalBlock+1
		end do
		return
	end function
	type(SymTensor) function contract_ownlegs(Tin,ith1,ith2) Result(Res)
		type(SymTensor),intent(in)::Tin
		integer,intent(in)::ith1,ith2
		type(SymTensor)::tempT
		type(SymDimension)::NewDimen
		integer::rank,i,k,classtype,dim1
		type(Tensor),pointer::Tenp(:,:,:),Newp(:)
			type(Tensor)::blockTensor
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
		call check_Contact(Tin,ith1,ith2)
		if(rank.eq.2)then
			call writemess(' Do not finsihed the code of rank=2, for T%contract(name1,name2)')
			call error_stop
		end if
		tempT=Tin.pf.ith1
		call tempT%forward(ith2)
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
			if(Newp(k)%getFlag())Res%TotalBlock=Res%TotalBlock+1
		end do
		return
	end function

















	!*********************************************

	
	
	type(SymTensor) function directProductTensor(T1,T2)result(Res)
		type(SymTensor),intent(in) :: T1,T2
		type(SymDimension)::newDim
		integer::classtype,i,j,k
		newDim=T1%SymDimension+T2%SymDimension
		classtype=select_type_in_add_minu(T1%getType(),T2%getType())
		call Res%allocate(newDim,classtype)
		k=1
		do j=1,T2%getTotalData()
			do i=1,T1%getTotalData()
				if(T1%block(i)%getFlag().and.T2%block(j)%getFlag())then
					Res%block(k)=T1%block(i).kron.T2%block(j)
					Res%TotalBlock=Res%TotalBlock+1
				end if
				k=k+1
			end do
		end do
		return
	end function
	
	


	type(Tensor) function TdotTensor(phi1,phi2)result(dotTensor)
		Type(SymTensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		integer::i
		do i=1,phi1%getTotalData()
			if(phi1%block(i)%getFlag().and.phi2%block(i)%getFlag())then
				if(dotTensor%getFlag())then
					dotTensor=dotTensor+(phi1%block(i).x.phi2%block(i))
				else
					dotTensor=phi1%block(i).x.phi2%block(i)
				end if
			end if
		end do
		RETURN
	end function
	real*4 function sdotTensor(phi1,phi2)result(dotTensor)
		Type(SymTensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		integer::i
		dotTensor=0.
		do i=1,phi1%getTotalData()
			if(phi1%block(i)%getFlag().and.phi2%block(i)%getFlag())then
				dotTensor=dotTensor+(phi1%block(i).sx.phi2%block(i))
			end if
		end do
		RETURN
	end function
	real*8 function ddotTensor(phi1,phi2)result(dotTensor)
		Type(SymTensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		integer::i
		dotTensor=0.
		do i=1,phi1%getTotalData()
			if(phi1%block(i)%getFlag().and.phi2%block(i)%getFlag())then
				dotTensor=dotTensor+(phi1%block(i).dx.phi2%block(i))
			end if
		end do
		RETURN
	end function
	complex*8 function cdotTensor(phi1,phi2)result(dotTensor)
		Type(SymTensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		integer::i
		dotTensor=0.
		do i=1,phi1%getTotalData()
			if(phi1%block(i)%getFlag().and.phi2%block(i)%getFlag())then
				dotTensor=dotTensor+(phi1%block(i).cx.phi2%block(i))
			end if
		end do
		RETURN
	end function
	
	complex*16 function zdotTensor(phi1,phi2)result(dotTensor)
		Type(SymTensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		integer::i
		dotTensor=0.
		do i=1,phi1%getTotalData()
			if(phi1%block(i)%getFlag().and.phi2%block(i)%getFlag())then
				dotTensor=dotTensor+(phi1%block(i).zx.phi2%block(i))
			end if
		end do
		RETURN
	end function
	
	type(Tensor) function TdotUTensor(phi1,phi2)result(dotTensor)
		Type(SymTensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		integer::i
		do i=1,phi1%getTotalData()
			if(phi1%block(i)%getFlag().and.phi2%block(i)%getFlag())then
				if(dotTensor%getFlag())then
					dotTensor=dotTensor+(phi1%block(i).dot.phi2%block(i))
				else
					dotTensor=phi1%block(i).dot.phi2%block(i)
				end if
			end if
		end do
		RETURN
	end function
	real*4 function sdotUTensor(phi1,phi2)result(dotTensor)
		Type(SymTensor),intent(in)::phi1,phi2
		integer::i
		dotTensor=0.
		do i=1,phi1%getTotalData()
			if(phi1%block(i)%getFlag().and.phi2%block(i)%getFlag())then
				dotTensor=dotTensor+(phi1%block(i).sdot.phi2%block(i))
			end if
		end do
		RETURN
	end function
	real*8 function ddotUTensor(phi1,phi2)result(dotTensor)
		Type(SymTensor),intent(in)::phi1,phi2
		integer::i
		dotTensor=0.
		do i=1,phi1%getTotalData()
			if(phi1%block(i)%getFlag().and.phi2%block(i)%getFlag())then
				dotTensor=dotTensor+(phi1%block(i).ddot.phi2%block(i))
			end if
		end do
		RETURN
	end function
	complex*8 function cdotUTensor(phi1,phi2)result(dotTensor)
		Type(SymTensor),intent(in)::phi1,phi2
		integer::i
		dotTensor=0.
		do i=1,phi1%getTotalData()
			if(phi1%block(i)%getFlag().and.phi2%block(i)%getFlag())then
				dotTensor=dotTensor+(phi1%block(i).cdot.phi2%block(i))
			end if
		end do
		RETURN
	end function
	complex*16 function zdotUTensor(phi1,phi2)result(dotTensor)
		Type(SymTensor),intent(in)::phi1,phi2
		integer::i
		dotTensor=0.
		do i=1,phi1%getTotalData()
			if(phi1%block(i)%getFlag().and.phi2%block(i)%getFlag())then
				dotTensor=dotTensor+(phi1%block(i).zdot.phi2%block(i))
			end if
		end do
		RETURN
	end function

	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                  H transpose
	!
	!**************************************************************************************************************
	!**************************************************************************************************************	

	type(SymTensor) function Htranspose(T)
		type(SymTensor),intent(in) :: T
		integer::rank,m,n,i
		type(dimension)::dimen
		integer,allocatable::indices(:)
		rank=T%getrank()
		if(rank.eq.1) then
			Htranspose=conjugate(T)
		else if(rank.eq.2) then
			Htranspose=.p.conjugate(T)
		else
			allocate(indices(rank))
			do i=1,rank
				indices(i)=rank-i+1
			end do
			Htranspose=conjugate(T).p.indices
		end if
		call reverseRule(Htranspose%SymDimension)
		return
	end function
	
	type(SymTensor) function Htranspose2(T)result(Htranspose)
		type(SymTensor),intent(in) :: T
		integer :: rank,i,charlen
		integer,allocatable::indices(:)
		character(len=max_len_of_char_in_TData)::Tname
		rank=T%getRank()
		allocate(indices(rank))
		do i=1,rank
			indices(i)=rank-i+1
		end do
		Htranspose=(.con.T).p.indices
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

	type(SymTensor) function conjugate(T)
		type(SymTensor),intent(in) :: T
		integer :: i
		conjugate=T
		do i=1,T%getTotalData()
			if(T%Block(i)%getFlag())then
				conjugate%block(i)=.con.T%block(i)
			end if
		end do
		return
	end function



	!**************************************************************************************************************
	!**************************************************************************************************************
	!
	!                                  SubTensor
	!
	!**************************************************************************************************************
	!**************************************************************************************************************	
		
	!		inde=[-1,inde_min,inde_max] output data(inde_min:inde_max,:)
	!	or[-2,inde_min,inde_max],data(:,inde_min:inde_max)
	!	or[-3,inde_min,inde_max],data(inde_min:inde_max)
	!	or [-1,inde_row] [-2,inde_col],output row or col
	!	or [inde1_min,inde1_max,inde2_min,inde2_max] output data(inde1_min:inde1_max,inde2_min:inde2_max)

	! The dimension of the output SymTensor is the same as input T

	type(SymTensor) function subSymTensor1(T,inde)	result(subSymTensor)
		class(SymTensor),intent(in) ::T
		integer,intent(in)::inde(:)
		integer::dim1,dim2,i,j
		logical::newBlock
		if(T%getRank().gt.2)then
			write(*,*)"error in subSymTensor,only matrix or vector is allowed"
			call error_stop()
		end if
		dim1=T.dim.1
		dim2=T.dim.2
		call subSymTensor%allocate(T%SymDimension,T%getType())
		if(size(inde).eq.4) then
			if(inde(2).gt.dim1)then
				write(*,*)"subSymTensor,error.1"
				call error_stop()
			end if
			if(inde(4).gt.dim2)then
				write(*,*)"subSymTensor,error.2"
				call error_stop()
			end if
			do i=inde(1),inde(2)
				do j=inde(3),inde(4)
					call subSymTensor_store_block2(subSymTensor%block,T%block,dim1,dim2,i,j,newBlock)
					if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
				end do
			end do
			return
		end if
		if(size(inde).eq.2) then
			
			select case (inde(1))
				case (-1)!output row
					if(inde(2).gt.dim1)then
						write(*,*)"subSymTensor,error.3"
						call error_stop()
					end if
					do j=1,dim2
						call subSymTensor_store_block2(subSymTensor%block,T%block,dim1,dim2,inde(2),j,newBlock)
						if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
					end do
				case (-2)!
					if(inde(2).gt.dim2)then
						write(*,*)"subSymTensor,error.4"
						call error_stop()
					end if
					do i=1,dim1
						call subSymTensor_store_block2(subSymTensor%block,T%block,dim1,dim2,i,inde(2),newBlock)
						if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
					end do
				case default 
					write(*,*) "no such case in subSymTensor"
					write(*,*)inde
					call error_stop()
			end 	select
			return
		end if
		if(size(inde).ne.3) then
			write(*,*) "no such case in subSymTensor"
			write(*,*) "length of inde is ",size(inde)
			write(*,*)inde
			call error_stop()
		end if
		select case (inde(1))
			case (-1)!output row
				if(inde(3).gt.dim1)then
					write(*,*)"subSymTensor,error.3"
					call error_stop()
				end if
				do i=inde(2),inde(3)
					do j=1,dim2
						call subSymTensor_store_block2(subSymTensor%block,T%block,dim1,dim2,i,j,newBlock)
						if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
					end do
				end do
			case (-2)!
				if(inde(3).gt.dim2)then
					write(*,*)"subSymTensor,error.4"
					call error_stop()
				end if
				do j=inde(2),inde(3)
					do i=1,dim1
						call subSymTensor_store_block2(subSymTensor%block,T%block,dim1,dim2,i,j,newBlock)
						if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
					end do
				end do
			case (-3)!
				do j=inde(2),inde(3)
					subSymTensor%block(j)=T%block(j)
					if(subSymTensor%block(j)%getFlag())subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
				end do
			case default 
				write(*,*) "no such case in subSymTensor"
				write(*,*)inde
				call error_stop()
		end 	select
		return
	end function
		
	subroutine subSymTensor_store_block2(block,inblock,LD1,LD2,i,j,newBlock)
		integer,intent(in)::LD1,LD2,i,j
		type(Tensor),intent(in)::inblock(LD1,LD2)
		type(Tensor),intent(inout)::block(LD1,LD2)
		logical,intent(inout)::newBlock
		block(i,j)=inblock(i,j)
		newBlock=inblock(i,j)%getFlag()
		return
	end subroutine
	
		
	type(SymTensor) function subSymTensor2(T,indexi,degi,row,keepQN_)result(subSymTensor)
		class(SymTensor),intent(in) ::T
		integer,intent(in)::indexi,degi
		character,intent(in)::row
		logical,optional,intent(in)::keepQN_
		logical::keepQN,newBlock
		type(SymDimension)::NewDim,tempDim
		type(QuanNum)::NewQN
		integer,allocatable::NewQNDeg(:)
		integer::i,rank,LD1,LD2,lDlengh,SLD1,SLD2
		real*4::QN
		if(.not.T%if_original_dim()) then
			write(*,*)"ERROR in subSymTensor2"
			write(*,*)"stop"
			call error_stop()
		end if
		if(present(keepQN_))then
			keepQN=keepQN_
		else
			keepQN=.false.
		end if
		rank=T%GetRank()
		if(rank.lt.2)then
			write(*,*) "ERROR in subSymTensor2"
			call error_stop()
		endif
		if(row.eq.'r')then
			if(keepQN)then
				QN=T%GetQN(1,indexi)
				call NewQN%setRule(T%getRule(1))
				call NewQN%setFermiArrow(T%getFermiArrow(1))
				call NewQN%setQN((/QN/))
				call NewQN%setDeg(1,1)
				NewDim=(/NewQN/)
				if(T%outNameFlag().eq.1)then
					call NewDim%setName(1,T%outName(1))
				end if
				NewDim=NewDim+(T%SymDimension.subdim.2)
			else
				NewDim=T%SymDimension.subdim.2
			end if
			do i=3,rank
				NewDim=NewDim+(T%SymDimension.subdim.i)
			end do
		else
			NewDim=T%SymDimension.subdim.1
			do i=2,rank-1
				NewDim=NewDim+(T%SymDimension.subdim.i)
			end do
			if(keepQN)then
				QN=T%GetQN(rank,indexi)
				call NewQN%setRule(T%getRule(rank))
				call NewQN%setFermiArrow(T%getFermiArrow(rank))
				call NewQN%setQN((/QN/))
				call NewQN%setDeg(1,1)
				tempDim=(/NewQN/)
				if(T%outNameFlag().eq.1)then
					call tempDim%setName(1,T%outName(rank))
				end if
				NewDim=NewDim+tempDim
			end if
		end if
		
		call subSymTensor%allocate(NewDim,T%getType())
		tempDim=T%SymDimension
		if(keepQN)then
			if(row.eq.'r')then
				call tempDim%fuse(2,rank)
				LD1=tempDim%dim(1)
				LD2=tempDim%dim(2)
				SLD1=subSymTensor%dim(1)
				SLD2=subSymTensor%dim(2)
				do i=3,subSymTensor%getRank()
					SLD2=SLD2*subSymTensor%dim(i)
				end do
				do i=1,LD2
					call subSymTensor_store_subTen2(subSymTensor%block,SLD1,SLD2,T%block,LD1,LD2,indexi,i,(/-1,degi/),newBlock)
					if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
				end do
			else
				call tempDim%fuse(1,rank-1)
				LD1=tempDim%dim(1)
				LD2=tempDim%dim(2)
				SLD1=subSymTensor%dim(1)
				do i=2,subSymTensor%getRank()-1
					SLD1=SLD1*subSymTensor%dim(i)
				end do
				SLD2=subSymTensor%dim(subSymTensor%getRank())
				do i=1,LD1
					call subSymTensor_store_subTen2(subSymTensor%block,SLD1,SLD2,T%block,LD1,LD2,i,indexi,(/-2,degi/),newBlock)
					if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
				end do
			end if
		
		else
			if(row.eq.'r')then
				call tempDim%fuse(2,rank)
				LD1=tempDim%dim(1)
				LD2=tempDim%dim(2)
				lDlengh=subSymTensor%getTotalData()
				do i=1,LD2
					call subSymTensor_store_subTen1(subSymTensor%block,lDlengh,i,T%block,LD1,LD2,indexi,i,(/-1,degi/),newBlock)
					if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
				end do
			else
				call tempDim%fuse(1,rank-1)
				LD1=tempDim%dim(1)
				LD2=tempDim%dim(2)
				lDlengh=subSymTensor%getTotalData()
				do i=1,LD1
					call subSymTensor_store_subTen1(subSymTensor%block,lDlengh,i,T%block,LD1,LD2,i,indexi,(/-2,degi/),newBlock)
					if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
				end do
			end if
		
		end if
		
		return
	end function
	
	
	subroutine subSymTensor_store_subTen2(block,LD1,LD2,inblock,inLD1,inLD2,i,j,indices,newBlock)
		integer,intent(in)::LD1,LD2,inLD1,inLD2,i,j,indices(2)
		type(Tensor),intent(in)::inblock(inLD1,inLD2)
		type(Tensor),intent(inout)::block(LD1,LD2)
		logical,intent(inout)::newBlock
		integer::ii,rank,blockindex(2)
		type(Dimension)::newdim
		if(indices(1).eq.(-1))then
			blockindex=(/1,j/)
		else
			blockindex=(/i,1/)
		end if
		if(inblock(i,j)%getFlag())then
			block(blockindex(1),blockindex(2))=inblock(i,j)
			newBlock=.true.
		else
			newBlock=.false.
			return
		end if
		rank=inblock(i,j)%getRank()
		if(indices(1).eq.(-1))then
			call block(blockindex(1),blockindex(2))%fuse(2,rank)
			newdim=(/1/)
			do ii=2,rank
				newdim=newdim+(inblock(i,j).subdim.ii)
			end do
		else
			call block(blockindex(1),blockindex(2))%fuse(1,rank-1)
			newdim=inblock(i,j).subdim.1
			do ii=2,rank-1
				newdim=newdim+(inblock(i,j).subdim.ii)
			end do
			newdim=newdim+(/1/)
		end if
		block(blockindex(1),blockindex(2))=block(blockindex(1),blockindex(2))%subTensor(indices) 
		call block(blockindex(1),blockindex(2))%resetdim(newdim)
		return
	end subroutine
	
	subroutine subSymTensor_store_subTen1(block,lenblock,k,inblock,LD1,LD2,i,j,indices,newBlock)
		integer,intent(in)::LD1,LD2,lenblock,i,j,k,indices(2)
		type(Tensor),intent(in)::inblock(LD1,LD2)
		type(Tensor),intent(inout)::block(lenblock)
		logical,intent(inout)::newBlock
		integer::ii,rank
		type(Dimension)::newdim
		if(inblock(i,j)%getFlag())then
			block(k)=inblock(i,j)
			newBlock=.true.
		else
			newBlock=.false.
			return
		end if
		rank=inblock(i,j)%getRank()
		if(indices(1).eq.-1)then
			call block(k)%fuse(2,rank)
			newdim=inblock(i,j).subdim.2
			do ii=3,rank
				newdim=newdim+(inblock(i,j).subdim.ii)
			end do
		else
			call block(k)%fuse(1,rank-1)
			newdim=inblock(i,j).subdim.1
			do ii=2,rank-1
				newdim=newdim+(inblock(i,j).subdim.ii)
			end do
		end if
		block(k)=block(k)%subTensor(indices) 
		call block(k)%resetdim(newdim)
		return
	end subroutine
	
	type(SymTensor) function subSymTensor3(T,QN,degi,row,keepQN_)result(subSymTensor)
		class(SymTensor),intent(in) ::T
		real*4,intent(in)::QN
		integer,intent(in)::degi
		character,intent(in)::row
		logical,optional,intent(in)::keepQN_
		logical::keepQN,newBlock
		type(SymDimension)::NewDim,tempDim
		type(QuanNum)::NewQN
		integer,allocatable::NewQNDeg(:)
		integer::i,rank,LD1,LD2,lDlengh,SLD1,SLD2,indexi
		if(.not.T%if_original_dim()) then
			write(*,*)"ERROR in subSymTensor2"
			write(*,*)"stop"
			call error_stop()
		end if
		if(present(keepQN_))then
			keepQN=keepQN_
		else
			keepQN=.false.
		end if
		rank=T%GetRank()
		if(rank.lt.2)then
			write(*,*) "ERROR in subSymTensor2"
			call error_stop()
		endif
		if(row.eq.'r')then
			if(keepQN)then
				indexi=T%Getindex(1,QN)
				call NewQN%setRule(T%getRule(1))
				call NewQN%setfermiArrow(T%getFermiArrow(1))
				call NewQN%setQN((/QN/))
				call NewQN%setDeg(1,1)
				NewDim=(/NewQN/)
				if(T%outNameFlag().eq.1)then
					call NewDim%setName(1,T%outName(1))
				end if
				NewDim=NewDim+(T%SymDimension.subdim.2)
			else
				NewDim=T%SymDimension.subdim.2
			end if
			do i=3,rank
				NewDim=NewDim+(T%SymDimension.subdim.i)
			end do
		else
			NewDim=T%SymDimension.subdim.1
			do i=2,rank-1
				NewDim=NewDim+(T%SymDimension.subdim.i)
			end do
			if(keepQN)then
				indexi=T%Getindex(rank,QN)
				call NewQN%setRule(T%getRule(rank))
				call NewQN%setFermiArrow(T%getFermiArrow(rank))
				call NewQN%setQN((/QN/))
				call NewQN%setDeg(1,1)
				tempDim=(/NewQN/)
				if(T%outNameFlag().eq.1)then
					call tempDim%setName(1,T%outName(rank))
				end if
				NewDim=NewDim+tempDim
			end if
		end if
		
		call subSymTensor%allocate(NewDim,T%getType())
		tempDim=T%SymDimension
		if(keepQN)then
			if(row.eq.'r')then
				call tempDim%fuse(2,rank)
				LD1=tempDim%dim(1)
				LD2=tempDim%dim(2)
				SLD1=subSymTensor%dim(1)
				SLD2=subSymTensor%dim(2)
				do i=3,subSymTensor%getRank()
					SLD2=SLD2*subSymTensor%dim(i)
				end do
				do i=1,LD2
					call subSymTensor_store_subTen2(subSymTensor%block,SLD1,SLD2,T%block,LD1,LD2,indexi,i,(/-1,degi/),newBlock)
					if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
				end do
			else
				call tempDim%fuse(1,rank-1)
				LD1=tempDim%dim(1)
				LD2=tempDim%dim(2)
				SLD1=subSymTensor%dim(1)
				do i=2,subSymTensor%getRank()-1
					SLD1=SLD1*subSymTensor%dim(i)
				end do
				SLD2=subSymTensor%dim(subSymTensor%getRank())
				do i=1,LD1
					call subSymTensor_store_subTen2(subSymTensor%block,SLD1,SLD2,T%block,LD1,LD2,i,indexi,(/-2,degi/),newBlock)
					if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
				end do
			end if
		
		else
			if(row.eq.'r')then
				call tempDim%fuse(2,rank)
				LD1=tempDim%dim(1)
				LD2=tempDim%dim(2)
				lDlengh=subSymTensor%getTotalData()
				do i=1,LD2
					call subSymTensor_store_subTen1(subSymTensor%block,lDlengh,i,T%block,LD1,LD2,indexi,i,(/-1,degi/),newBlock)
					if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
				end do
			else
				call tempDim%fuse(1,rank-1)
				LD1=tempDim%dim(1)
				LD2=tempDim%dim(2)
				lDlengh=subSymTensor%getTotalData()
				do i=1,LD1
					call subSymTensor_store_subTen1(subSymTensor%block,lDlengh,i,T%block,LD1,LD2,i,indexi,(/-2,degi/),newBlock)
					if(newBlock)subSymTensor%TotalBlock=subSymTensor%TotalBlock+1
				end do
			end if
		
		end if
		
		return
	end function
	



	!**********************************************************************
	!**********************************************************************
	!	the code below is for MPI
	!**********************************************************************

	subroutine MPI_send_SymTensor(Ten1,Ten2,ID1,ID2,ierr,MPIcommon)
		type(SymTensor),intent(in)::Ten1
		type(SymTensor),intent(inout)::Ten2
		integer,intent(in)::ID1,ID2
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,len2,istatus(MPI_STATUS_SIZE),mpi_comm,i,totaldata
		logical::blockflag
		tag=1
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		call mpi_comm_rank(mpi_comm,proID,ierr)
		call mpi_comm_size(mpi_comm,proNum,ierr )
		if(proNum.eq.1)return
		if(present(MPIcommon))then
			if((ID1.ge.proNum).or.(ID2.ge.proNum))return
		end if
		
		if(ID1.eq.ID2) return !The same cpu, do nothing
		
		if((proID.ne.ID1).and.(proID.ne.ID2)) return!The proID do not send or recv, return
		!***************************flag*********************************************		
		if(proID.eq.ID1) then
			call mpi_send(Ten1%flag,1,MPI_logical,ID2,tag,MPI_Comm,ierr)
			if(.not.Ten1%flag)then
				return
			end if
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Ten2%flag,1,MPI_logical,ID1,tag,MPI_Comm,istatus,ierr)
			if(.not.Ten2%flag)then
				call Ten2%empty()
				return
			end if
		end if
		!****************************totalData**********************************************		
		if(proID.eq.ID1) then
			call mpi_send(Ten1%totalData,1,MPI_integer,ID2,tag,MPI_Comm,ierr)
			totaldata=Ten1%totalData
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Ten2%totalData,1,MPI_integer,ID1,tag,MPI_Comm,istatus,ierr)
			totaldata=Ten2%totalData
		end if
		!****************************totalblock**********************************************		
		if(proID.eq.ID1) then
			call mpi_send(Ten1%totalblock,1,MPI_integer,ID2,tag,MPI_Comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Ten2%totalblock,1,MPI_integer,ID1,tag,MPI_Comm,istatus,ierr)
		end if		
		!****************************classtype**********************************************		
		if(proID.eq.ID1) then
			call mpi_send(Ten1%classtype,1,MPI_integer,ID2,tag,MPI_Comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Ten2%classtype,1,MPI_integer,ID1,tag,MPI_Comm,istatus,ierr)
		end if
		!****************************DynamicClass**********************************************	
		if(proID.eq.ID1) then
			call mpi_send(Ten1%DynamicClass,1,MPI_logical,ID2,tag,MPI_Comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Ten2%DynamicClass,1,MPI_logical,ID1,tag,MPI_Comm,istatus,ierr)
		end if
		!************************SymDimension************************************************		
		call MPI_send_SymDimension(Ten1%SymDimension,Ten2%SymDimension,ID1,ID2,ierr,MPIcommon)
		!*************************rank***************************************************			
		if(proID.eq.ID1) then
			call mpi_send(Ten1%rank,1,MPI_integer,ID2,tag,MPI_Comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Ten2%rank,1,MPI_integer,ID1,tag,MPI_Comm,istatus,ierr)
		end if
		!*************************block***************************************************	
		if(proID.eq.ID2) then
			call AllocateCheck(Ten2%block,totaldata)	
		end if
		do i=1,totaldata
			if(proID.eq.ID1) then
				blockflag=Ten1%block(i)%getFlag()
				call mpi_send(blockflag,1,MPI_logical,ID2,tag,MPI_Comm,ierr)
			end if
			if(proID.eq.ID2) then
				call mpi_recv(blockflag,1,MPI_logical,ID1,tag,MPI_Comm,istatus,ierr)
			end if
			if(blockflag)then
				call MPI_send_Tensor(Ten1%block(i),Ten2%block(i),ID1,ID2,ierr,MPIcommon)
			else
				if(proID.eq.ID2) call Ten2%block(i)%deallocate() 
			end if
		end do
		return
	end subroutine
	subroutine MPI_BCAST_SymTensor(Ten1,ID,ierr,MPIcommon)
		type(SymTensor),intent(inout)::Ten1
		integer,intent(in)::ID
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,len2,istatus(MPI_STATUS_SIZE),mpi_comm,i,totaldata
		logical::blockflag
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		
		tag=1
		call mpi_comm_rank(mpi_comm,proID,ierr)
		call mpi_comm_size(mpi_comm,proNum,ierr )
		if(proNum.eq.1)return
		if(present(MPIcommon))then
			if(ID.ge.proNum)return
		end if
		!******************************flag***********************************************			
		call MPI_BCAST(Ten1%flag,1,MPI_logical,ID,mpi_comm,ierr)
		if(.not.Ten1%flag) then
			call Ten1%empty()
			return
		end if
		!**************************totalData*********************************************		
		call MPI_BCAST(Ten1%totalData,1,MPI_integer,ID,mpi_comm,ierr)
		totaldata=Ten1%totalData
		!**************************totalblock*********************************************		
		call MPI_BCAST(Ten1%totalblock,1,MPI_integer,ID,mpi_comm,ierr)
		!**************************DynamicClass*********************************************
		call MPI_BCAST(Ten1%DynamicClass,1,MPI_logical,ID,mpi_comm,ierr)
		!**************************classtype*********************************************		
		call MPI_BCAST(Ten1%classtype,1,MPI_integer,ID,mpi_comm,ierr)
		!***************************rank**************************************************		
		call MPI_BCAST(Ten1%rank,1,MPI_integer,ID,mpi_comm,ierr)	
		!***************************SymDimension**************************************************		
		call MPI_BCAST_SymDimension(Ten1%SymDimension,ID,ierr,MPIcommon)
		!*************************block***************************************************	
		if(proID.ne.ID)then
			call allocateCheck(Ten1%block,totaldata)
		end if	
		do i=1,totaldata
			if(proID.eq.ID) then
				blockflag=Ten1%block(i)%getFlag()
			end if
			call MPI_BCAST(blockflag,1,MPI_logical,ID,mpi_comm,ierr)
			if(blockflag)then
				call MPI_BCAST_Tensor(Ten1%block(i),ID,ierr,MPIcommon)
			else
				call Ten1%block(i)%deallocate()
			end if
		end do
	end subroutine
	
	subroutine MPI_SUM_SymTensor1(inoutTensor,ierr,MPIcommon)
		type(SymTensor),intent(inout)::inoutTensor
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::classtype,proID,proNum,mpi_comm,i,tag,istatus(MPI_STATUS_SIZE),j
		integer::totalData,tempsum
		logical::goonFlag,ALLgoonFlag
		character*20::typechar
		type(Tensor)::temp,check
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		totalData=inoutTensor%totalData
		call MPI_BCAST(totalData,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=totalData.eq.inoutTensor%totalData
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('ERROR in Sum SymTensor MPI')
			call error_stop
		end if
		call mpi_comm_rank(mpi_comm,proID,ierr)
		call mpi_comm_size(mpi_comm,proNum,ierr )
		call temp%empty()
		call check%allocate([proNum],'integer')
		do i=1,totalData
			goonFlag=inoutTensor%block(i)%getFlag()
			call check%zero()
			if(goonFlag)call check%setValue(proID+1,1)
			call MPI_SUM_Tensor(check,ierr)
			tempsum=check%isum()
			if(tempsum.eq.proNum)then
				call MPI_SUM_Tensor(inoutTensor%block(i),ierr,MPIcommon)
			else if(tempsum.ne.0)then
				do j=1,proNum-1
					call MPI_send_Tensor(inoutTensor%block(i),temp,j,0,ierr)
					if(proID.eq.0)then
						if(inoutTensor%block(i)%getFlag())then
							if(temp%getFlag())then
								inoutTensor%block(i)=inoutTensor%block(i)+temp
							end if
						else
							if(temp%getFlag())inoutTensor%block(i)=temp
						end if
					end if
				end do
				call MPI_BCAST_Tensor(inoutTensor%block(i),0,ierr,MPIcommon) 
			end if
		end do
		return
	end subroutine





































end module
