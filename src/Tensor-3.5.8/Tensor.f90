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
!************************************************************
!************* START OF Tensor **********************
!************************************************************
!*****************     worning    *******************
!			1.when link1=link2,and want link1 unchange while operate on link2,use
!		copylink but not (=)
!			2.The code is fortran90 version, gfortran4.8.4.
!			3.to compile the code one should link the files to lapack and blas.
!			4.Send email to sj.dong@outlook.com to report any bugs.
!
!
!
!
!1.  allocateTensor
!2.  dallocateTensor
!3.  assignment to Tensor
!4.  assignment to array
!5 . Type Overloading
!6.  print Tensor 
!7.  get  dimension   data
!8.  TensorName
!9.  element
!10.  generate data
!11. modify element in Tensor
!12. + - * /
!13. overwrite int real dble cmplx dcmplx aimag char
!14. max or min element in Tensor
!15. operaction on dimension
!16. permutation
!17. contract
!18. useful function
!19. lapack function (SVD, linear equation inverse and so on)
!20. Tensorlink and Tensornode
!21. MPI function

module Tensor_type
	use TData_module
	use Dimension_typede
	use Contract_real8_Tools
	use mpi
	use Tools
	implicit none
	private

	logical::SVD_S_matrix_flag=.false.
	character(len=1)::array_character_divider='|'
	
	public::Tensor
	type,extends (TData) :: Tensor
		private
		type(dimension) :: TenDim
		integer:: rank=0
	contains
		generic,public::info =>Tprint1,Tprint2,Tprint3,Tprint4
		generic,public::print =>TMprint1,TMprint2,TMprint3,TMprint4
		generic,public::diminfo=>TDprint1,TDprint2,TDprint3
		
		generic,public::writeinfo =>Tprint_file1,Tprint_file2!output in external file
		generic,public::write =>Tprint_file1,Tprint_file2!output in external file
		generic,public::printFile =>TMprint_file1,TMprint_file2!output in external file
		
		procedure,public::read =>Tread_file
		generic,public::readData =>Tread_data
		procedure,public::empty =>emptyTensor
		procedure,public::deallocate =>cleanTensor
		generic,public::allocate => allocateTensor1,allocateTensor2,allocateTensor3,&
												allocateTensor4,allocateTensor5,allocateTensor6,&
												allocateTensor7,allocateTensor8,allocateTensor9
		generic,public::Reallocate => allocatedTensor1,allocatedTensor2,allocatedTensor3,&
												allocatedTensor4,allocatedTensor5,allocatedTensor6,&
												allocatedTensor7,allocatedTensor8,allocatedTensor9										
		
		generic,public::dim=> getTenDim_i,getAllTenDim,getTenDim_namei
		
		generic,public::dimension=>getTenSubDim,getTenSubDim_name,getTenDim
		procedure,public::getRank
		procedure,public::outNameFlag
		
		
		generic,public::setName =>setTensorName2,setTensorName3,setTensorName4,setTensorName5,&
											setTensorName6,setTensorName7,setTensorName8,setTensorName9
		
		generic,public::outTensorName=>outTensorName1,outAllTensorName,outTensorNamechar
		generic,public::outName=>outIndexName,outAllIndexName
		generic,public::getName=>outIndexName,outAllIndexName,outTensorNamechar
		procedure,public::outDimName
		procedure,public::outAllName
		procedure,public::getAllName=>outAllName
		
		generic,public::resetdim =>resetdim1,resetdim2
		procedure,public::cleanName =>cleanTensorName

		generic,public::reset_dim_no_check =>reset_dim_no_check2,reset_dim_no_check1
		procedure::reset_dim_no_check2,reset_dim_no_check1
		
		generic,public::Nameorder =>TenNameorder1,TenNameorder2,TenNameorder3,TenNameorder4
		generic,public::Findorder =>TenFindorder1,TenFindorder2,TenFindorder3
		procedure,public::RNdim=>RNTensorDim!input dimension [1,1,2,1,3,1,1,4,1] ,output[2,3,4]	
		generic,public::killLeg =>RNTensorDimint,RNTensorDimchar,RNTensorDim
		procedure::RNTensorDimint,RNTensorDimchar,RNTensorDim
		procedure,public::fuse =>fuseTensor
		procedure,public::split =>splitTensor
		
		generic,public::dimOperation =>dimOperation1,dimOperation2
		generic,public::permute=>permutation_routine,permutation_Name_routine,permutation_Tensor_routine
		generic,public::forward=>permutefo_routine,permutefo_name_routine,permutefo_vec_routine,permutefo_vec_Name_routine&
										,permutefo_Tensor_routine
		generic,public::backward=>permuteback_routine,permuteback_name_routine,permuteback_vec_routine&
										,permuteback_vec_name_routine,permuteback_Tensor_routine
										
		!procedure,public::sub=>subTen
		procedure::subTen,subTen2,subTen2_Name,subTen3,subTen3_Name
		generic,public::subTensor=>subTen,subTen2,subTen2_Name,subTen3,subTen3_Name
!		T%subTen(inde):inde=[-1,inde_min,inde_max] output data(inde_min:inde_max,:)
!			or[-2,inde_min,inde_max],data(:,inde_min:inde_max)
!	      or[-3,inde_min,inde_max],data(inde_min:inde_max)
!			or [-1,inde_row] [-2,inde_col],output row or col
!			or [inde1_min,inde1_max,inde2_min,inde2_max] output data(inde1_min:inde1_max,inde2_min:inde2_max)
		procedure,public::addcol=>combinationCol_subroutine
!combinationCol:
!			T1 :a [...,l,m,n] matrix
!			T2 :a [...,l,m,n] matrix
!			combination(T1,T2):a [...,l,m,n,2] matrix
!			or 
!			T1 :a [...,m,n,l] matrix
!			T2 :a [...,m,n] matrix
!			combination(T1,T2):a [...,m,n,l+1] matrix
		procedure,public::addrow=>combinationrow_subroutine
!combinationrow
!			T1 :a [l,m,n,...] matrix
!			T2 :a [l,m,n,...] matrix
!			combinationrow(T1,T2):a [2,l,m,n,...] matrix
!			or 
!			T1 :a [l,m,n,...] matrix
!			T2 :a [m,n,...] matrix
!			combinationrow(T1,T2):a [l+1,m,n,...] matrix	
		procedure,public::paste=>pasteTensorSubroutine
!call T1%pasteTensorSubroutine(T2,.true.)
!			T1 :a [l,m,n,...] matrix
!			T2 :a [l,m,n,...] matrix
!			pasteTensor(T1,T2):a [2*l,m,n,...] matrix
!        [1--->l, m, n,...] is T1
!			[l+1--->2*l, m, n,...] is T2
!        /   \
!        | T1 |
!        |----|
!        | T2 |
!        \    /
!pasteTensor(T1,T2,.false.)
!			T1 :a [...,m,n,l] matrix
!			T2 :a [...,m,n,l] matrix
!			pasteTensor(T1,T2):a [...,m,n,2*l] matrix
!        [... , m, n, 1--->l] is T1
!			[... , m, n, l+1--->2*l] is T2
!        /         \
!        | T1 | T2 |
!        \         /
		generic,public::i => TElement,TElement2
		generic,public::ii => iElement,iElement2,ielementAll
		generic,public::si => sElement,sElement2,selementAll
		generic,public::di => dElement,dElement2,delementAll
		generic,public::ci => cElement,cElement2,celementAll
		generic,public::zi => zElement,zElement2,zelementAll
		generic,public::li => lElement,lElement2,lelementAll
		generic,public::ai => aElement,aElement2,aelementAll
		
		generic,public::max => TmaxElement,TmaxminElement
		generic,public::imax => intmaxElement,intmaxminElement
		generic,public::smax => realmaxElement,realmaxminElement
		generic,public::dmax => dblemaxElement,dblemaxminElement
		generic,public::cmax => cmplxmaxElement,cmplxmaxminElement
		generic,public::zmax => dcmplxmaxElement,dcmplxmaxminElement
		
		generic,public::min => TminElement,TmaxminElement
		generic,public::imin => intminElement,intmaxminElement
		generic,public::smin => realminElement,realmaxminElement
		generic,public::dmin => dbleminElement,dblemaxminElement
		generic,public::cmin => cmplxminElement,cmplxmaxminElement
		generic,public::zmin => dcmplxminElement,dcmplxmaxminElement
		!maxminflag=
		!'maxa': max abs 
		!'mina': min abs 
		!'maxr': max real
		!'minr': min real
		!'maxi': 0(not com) or max imag
		!'mini': 0(not com) or min imag
		procedure,public::maxmin => dcmplxmaxminElement
		procedure,public::imaxmin => intmaxminElement
		procedure,public::smaxmin => realmaxminElement
		procedure,public::dmaxmin => dblemaxminElement
		procedure,public::cmaxmin => cmplxmaxminElement
		procedure,public::zmaxmin => dcmplxmaxminElement
		
		
		procedure,public::sum =>sumTensorT
		procedure,public::isum =>sumTensori
		procedure,public::ssum =>sumTensors
		procedure,public::dsum =>sumTensord
		procedure,public::csum =>sumTensorc
		procedure,public::zsum =>sumTensorz
		
		generic,public::random =>set_random_Tensor,set_random_Tensor_region4,set_random_Tensor_regioni
		procedure,public::zero=>set_zero_Tensor
		!Tdata(:)=a            setvalue(a)                -----set_value_Tensor_*
		!Tdata(i)=a            setvalue(i,a)              -----modifyTen_val_*
		!Tdata((/i,j,k/))=a    setvalue((/i,j,k/),a)      -----modifyTen_val1_*
		!Tdata(i)=T.i.1            setvalue(i,T)   T is a Tensor             -----modifyTen_val_*
		!Tdata((/i,j,k/))=T.i.1    setvalue((/i,j,k/),T)  T is a Tensor      -----modifyTen_val1_*
		!Tdata(:)=a(*)         setvalue(a) , a is a array -----store_*
		!Tdata(i1,i2)=a(*)     setvalue(i1,i2,a), a is a array -----storeSome_*
		!Tdata(i1,i2)=T        setvalue(i1,i2,T), T is a Tensor -----storeSome_Tensor*
		!Tdata(i1:i2)=a(i1':i2') a is a Tensor or array  --- modify_Some_Data1_*
		!Tdata(i1:i2,j1:j2)=a(i1':i2',j1':j2') a is a Tensor or array  --- modify_Some_Data2_*
		!To be finished
		!Tdata(i1:i2,j1:j2,k1:k2)=a(i1':i2',j1':j2',k1':k2')
		!Tdata(i1:i2,j1:j2,k1:k2,l1:l2)=a(i1':i2',j1':j2',k1':k2',l1':l2')
		generic,public::setValue =>modifyTen_val_int,modifyTen_val_real4,modifyTen_val_real8,modifyTen_val_com4,&
											modifyTen_val_com8,modifyTen_val_logi,modifyTen_val_char,&
											modifyTen_val1_int,modifyTen_val1_real4,modifyTen_val1_real8,modifyTen_val1_com4,&
											modifyTen_val1_com8,modifyTen_val1_logi,modifyTen_val1_char,modifyTen_val_Tensor,&
											modifyTen_val1_Tensor,&
											set_value_Tensor_int,set_value_Tensor_real4,set_value_Tensor_real8,set_value_Tensor_com4,&
											set_value_Tensor_com8,set_value_Tensor_logi,set_value_Tensor_char, store_T,&
											store_int,store_real4,store_real8,store_com4,store_com8,store_logi,store_char,&
											storeSome_int,storeSome_real4,storeSome_real8,storeSome_com4,storeSome_com8,storeSome_logi,storeSome_char,&
											storeSome_Tensor,&
											modify_Some_Data1_int,modify_Some_Data1_real4,modify_Some_Data1_real8,modify_Some_Data1_com4,&
											modify_Some_Data1_com8,modify_Some_Data1_char,modify_Some_Data1_logi,modify_Some_Data1_Tensor,&
											modify_Some_Data2_int,modify_Some_Data2_real4,modify_Some_Data2_real8,modify_Some_Data2_com4,&
											modify_Some_Data2_com8,modify_Some_Data2_char,modify_Some_Data2_logi,modify_Some_Data2_Tensor,&
											modify_Some_Data2_Tensor2
											
!			norm2(Tr_):return  <phi|phi>
!			norm(Tr):return  sqrt(<phi|phi>)
		procedure,public::norm => TnormTensor
		procedure,public::inorm => inormTensor
		procedure,public::snorm => snormTensor
		procedure,public::dnorm => dnormTensor
		procedure,public::cnorm => cnormTensor
		procedure,public::znorm => znormTensor
		procedure,public::norm2 => Tnorm2Tensor
		procedure,public::inorm2 => inorm2Tensor
		procedure,public::snorm2 => snorm2Tensor
		procedure,public::dnorm2 => dnorm2Tensor
		procedure,public::cnorm2 => cnorm2Tensor
		procedure,public::znorm2 => znorm2Tensor
		
		procedure,public::trace  => TtraceTensor
		procedure,public::itrace => itraceTensor
		procedure,public::strace => straceTensor
		procedure,public::dtrace => dtraceTensor
		procedure,public::ctrace => ctraceTensor
		procedure,public::ztrace => ztraceTensor
		
		
		generic,public::SVDlink => SVD_link,SVD_name_link
		generic,public::SVDTensor    => SVDTensor_noname,SVDTensor_name,SVDTensor_Leg1,SVDTensor_Leg2,SVDTensor_Leg1_
		generic,public::SVDroutine     => SVDcutoff,SVDcutoff_name,SVDTensor_Leg1_Routine,SVDTensor_Leg2_Routine,SVDTensor_Leg1_Routine_
		generic,public::SVD     =>SVDcutoff,SVDcutoff_name,SVDTensor_Leg1_Routine,SVDTensor_Leg2_Routine,&
		 							SVDTensor_Leg1_Routine_,SVDNameRoutineLeft
		generic,public::SVDKill =>SVDcutoff_kill_inData,SVDcutoff_name_kill_inData,SVDTensor_Leg1_routine_kill_indata,&
		                        SVDNameRoutineLeft_kill_indata,SVDTensor_Leg1_Routine_kill_inData_,SVDTensor_Leg2_Routine_kill_inData
		procedure::SVDcutoff_kill_inData,SVDcutoff_name_kill_inData,SVDTensor_Leg1_routine_kill_indata,SVDNameRoutineLeft_kill_indata
		procedure::SVDTensor_Leg1_Routine_kill_inData_,SVDTensor_Leg2_Routine_kill_inData
		procedure::SVDTensor_Leg1,SVDTensor_Leg2,SVDTensor_Leg1_
		procedure::SVDTensor_Leg1_Routine,SVDTensor_Leg2_Routine,SVDTensor_Leg1_Routine_,SVDNameRoutineLeft
! 			T should be two dimension		
!			T=U*s*V^T
!			 on output,the order in the array is
! 			[U]->[s]->[V^T]
!			 T=res(1)*res(res(2))*res(3)
		procedure,public::QRlink => QR_link
		generic,public::QRTensor =>QRTensor_noName,QRTensor_Name,QRTensor_Leg1,QRTensor_Leg2,QRTensor_Leg1_
		generic,public::QRroutine     => QRdecomposition1,QRTensor_Leg1_Routine,QRTensor_Leg2_Routine
		generic,public::QR     => QRdecomposition1,QRTensor_Leg1_Routine,QRTensor_Leg2_Routine,QRRoutineNameLeft
		procedure::QRTensor_Leg1_Routine,QRTensor_Leg2_Routine,QRdecomposition1,QRRoutineNameLeft
		procedure::QRTensor_Leg1,QRTensor_Leg2,QRTensor_Leg1_
! 			T should be two dimension		
!			T=Q*R
!			 on output,the order in the array is
! 			[Q]->[R]
!			 T=res(1)*res(2)		
		

		generic,public::QRKill     => QRdecomposition_kill_inData,QRTensor_Leg1_Routine_kill_inData,&
											QRTensor_Leg2_Routine_kill_inData,QRRoutineNameLeft_kill_inData
		procedure::QRdecomposition_kill_inData,QRTensor_Leg1_Routine_kill_inData,&
											QRTensor_Leg2_Routine_kill_inData,QRRoutineNameLeft_kill_inData
		generic,public::LQKill     =>LQdecomposition_kill_inData,LQTensor_Leg2_Routine_kill_inData,&
											LQTensor_Leg1_Routine_kill_inData,LQRoutineNameLeft_kill_inData
		procedure::LQdecomposition_kill_inData,LQTensor_Leg2_Routine_kill_inData,&
											LQTensor_Leg1_Routine_kill_inData,LQRoutineNameLeft_kill_inData


		procedure,public::LQlink => LQ_link
		generic,public::LQTensor =>LQTensor_noName,LQTensor_Name,LQTensor_Leg1,LQTensor_Leg2,LQTensor_Leg1_
		generic,public::LQroutine     => LQdecomposition1,LQTensor_Leg2_Routine,LQTensor_Leg1_Routine
		generic,public::LQ     => LQdecomposition1,LQTensor_Leg2_Routine,LQTensor_Leg1_Routine,LQRoutineNameLeft
		procedure::LQTensor_Leg2_Routine,LQTensor_Leg2,LQTensor_Leg1_Routine,LQTensor_Leg1,LQdecomposition1,LQRoutineNameLeft
		procedure::LQTensor_Leg1_
! 			T should be two dimension		
!			T=L*Q
!			 on output,the order in the array is
! 			[L]->[Q]
!			 T=res(1)*res(2)	
		procedure,public::eiglink =>eig_link!check,only ok on sysmetry martix
		procedure,public::eigTensor!check,only ok on sysmetry martix
		procedure,public::eig =>eigTensor!check,only ok on sysmetry martix
		procedure,public::eigRoutine=>eigvalue
		procedure,public::linequ
		procedure,public::ifName
		procedure,public::if_simple_dimension
!A*X=B
!input A  and B
!output X
!if A^{-1} may not exit,input RCOND,
!perform SVD on A, Only keep  singular values S(i) <= RCOND*S(1) 
!if RCONDM<0 keep the S(i)>0
		procedure,public::invTensor=>inverseTensor
		
		generic,public::eye=> eye_Tensor2,eye_Tensor3,eye_Tensor4,eye_Tensor5,eye_Tensor6
		generic,public::which=>iwhichindex,swhichindex,dwhichindex,cwhichindex,zwhichindex,awhichindex,awhichindex2
		generic,public::sort=>sortTensor1,sortTensor2,sortTensor3,sortTensor4,sortTensor5,sortTensor6
		generic,public::pointer=>iTpointer2,sTpointer2,dTpointer2,cTpointer2,zTpointer2,lTpointer2,aTpointer2,&
								iTpointer2_,sTpointer2_,dTpointer2_,cTpointer2_,zTpointer2_,lTpointer2_,aTpointer2_,&
								iTpointer3,sTpointer3,dTpointer3,cTpointer3,zTpointer3,lTpointer3,aTpointer3,&
								iTpointer3_,sTpointer3_,dTpointer3_,cTpointer3_,zTpointer3_,lTpointer3_,aTpointer3_,&
								iTpointer4,sTpointer4,dTpointer4,cTpointer4,zTpointer4,lTpointer4,aTpointer4,&
								iTpointer4_,sTpointer4_,dTpointer4_,cTpointer4_,zTpointer4_,lTpointer4_,aTpointer4_
		procedure::iTpointer2,sTpointer2,dTpointer2,cTpointer2,zTpointer2,lTpointer2,aTpointer2
		procedure::iTpointer2_,sTpointer2_,dTpointer2_,cTpointer2_,zTpointer2_,lTpointer2_,aTpointer2_
		procedure::iTpointer3,sTpointer3,dTpointer3,cTpointer3,zTpointer3,lTpointer3,aTpointer3
		procedure::iTpointer3_,sTpointer3_,dTpointer3_,cTpointer3_,zTpointer3_,lTpointer3_,aTpointer3_
		procedure::iTpointer4,sTpointer4,dTpointer4,cTpointer4,zTpointer4,lTpointer4,aTpointer4
		procedure::iTpointer4_,sTpointer4_,dTpointer4_,cTpointer4_,zTpointer4_,lTpointer4_,aTpointer4_

		generic,public::enLargeTensor=>enlargeTensorReal8,enlargeTensorReal4,enlargeTensorInt,enlargeTensorAllReal8&
							,enlargeTensorAllReal4,enlargeTensorAllInt,&
							enlargeTensorReal8_name,enlargeTensorReal4_name,enlargeTensorInt_name
		generic,public::enLarge=>enlargeTensorReal8Routine,enlargeTensorReal4Routine,enlargeTensorIntRoutine,&
								enlargeTensorAllReal8Routine,enlargeTensorAllReal4Routine,enlargeTensorAllIntRoutine,&
								enlargeTensorReal8_nameRoutine,enlargeTensorReal4_nameRoutine,enlargeTensorInt_nameRoutine
		procedure::enlargeTensorReal8,enlargeTensorReal4,enlargeTensorInt
		procedure::enlargeTensorReal8_name,enlargeTensorReal4_name,enlargeTensorInt_name
		procedure::enlargeTensorReal8Routine,enlargeTensorReal4Routine,enlargeTensorIntRoutine
		procedure::enlargeTensorReal8_nameRoutine,enlargeTensorReal4_nameRoutine,enlargeTensorInt_nameRoutine
		procedure::enlargeTensorAllReal8Routine,enlargeTensorAllReal4Routine,enlargeTensorAllIntRoutine
		procedure::enlargeTensorAllReal8,enlargeTensorAllReal4,enlargeTensorAllInt
		procedure::ProductTensorRoutine1,ProductTensorRoutine2,ProductTensorRoutine3
		generic,public::ProductTensorRoutine=>ProductTensorRoutine1,ProductTensorRoutine2,ProductTensorRoutine3
		generic,public::contract=>contract_name_routine,contract_name_routine1,contract_name_routine2,&
											contract_name_routine4,contract_name_routine5,contract_name_routine6,&
											contract_name_ownlegs_routine,contract_ownlegs_routine
		procedure::contract_name_routine,contract_name_routine1,contract_name_routine2
		procedure::contract_name_routine4,contract_name_routine5,contract_name_routine6
		procedure::contract_name_ownlegs_routine,contract_ownlegs_routine
		
		procedure::Fast_contract_name_routine,Fast_contract_name_1leg_routine
		procedure::Fast_contract_name_routine1,Fast_contract_name_1leg_routine1
		procedure::Fast_contract_name_1leg_routine2,Fast_contract_name_routine2
		generic,public::FastContract=>Fast_contract_name_routine,Fast_contract_name_1leg_routine,&
			Fast_contract_name_routine1,Fast_contract_name_1leg_routine1,&
			Fast_contract_name_1leg_routine2,Fast_contract_name_routine2

		procedure::TenFindorder1,TenFindorder2,TenFindorder3
		procedure,public::if_original_dim=>if_original_dim_in_T
		procedure::sortTensor1
		procedure::sortTensor2
		procedure::sortTensor3
		procedure::sortTensor4
		procedure::sortTensor5
		procedure::sortTensor6
		procedure::QRTensor_noName
		procedure::QRTensor_Name
		procedure::LQTensor_noName
		procedure::LQTensor_Name
		procedure::Tread_data
		procedure::getTenSubDim
		procedure::getTenSubDim_name
		procedure::getTenDim
		procedure::eye_Tensor2
		procedure::eye_Tensor3
		procedure::eye_Tensor4
		procedure::eye_Tensor5
		procedure::eye_Tensor6
		procedure::iwhichindex
		procedure::swhichindex
		procedure::dwhichindex
		procedure::cwhichindex
		procedure::zwhichindex
		procedure::awhichindex
		procedure::awhichindex2
		procedure::SVD_link
		procedure::SVDTensor_noname
		procedure::SVDcutoff
		procedure::SVD_name_link
		procedure::SVDTensor_name
		procedure::SVDcutoff_name
		procedure::Tprint_file1
		procedure::Tprint_file2
		procedure::Tprint1
		procedure::Tprint2
		procedure::Tprint3
		procedure::Tprint4
		procedure::TMprint1
		procedure::TMprint2
		procedure::TMprint3
		procedure::TMprint4
		procedure::TMprint_file1
		procedure::TMprint_file2
		procedure::TDprint1
		procedure::TDprint2
		procedure::TDprint3
		procedure::allocateTensor1
		procedure::allocateTensor2
		procedure::allocateTensor3
		procedure::allocateTensor4
		procedure::allocateTensor5
		procedure::allocateTensor6
		procedure::allocateTensor7
		procedure::allocateTensor8
		procedure::allocateTensor9
		procedure::allocatedTensor1
		procedure::allocatedTensor2
		procedure::allocatedTensor3
		procedure::allocatedTensor4
		procedure::allocatedTensor5
		procedure::allocatedTensor6
		procedure::allocatedTensor7
		procedure::allocatedTensor8
		procedure::allocatedTensor9
		procedure::setTensorName2
		procedure::setTensorName3
		procedure::setTensorName4
		procedure::setTensorName5
		procedure::setTensorName6
		procedure::setTensorName7
		procedure::setTensorName8
		procedure::setTensorName9
		procedure::outIndexName
		procedure::outAllIndexName
		procedure::outTensorName1
		procedure::outAllTensorName
		procedure::outTensorNamechar
		procedure::resetdim1
		procedure::resetdim2
		procedure::TenNameorder1
		procedure::TenNameorder2
		procedure::TenNameorder3
		procedure::TenNameorder4
		procedure::dimOperation1
		procedure::dimOperation2
		procedure::permutation_routine
		procedure::permutation_Name_routine
		procedure::permutation_Tensor_routine
		procedure::permutefo_routine
		procedure::permutefo_name_routine
		procedure::permutefo_Tensor_routine
		procedure::permutefo_vec_routine
		procedure::permutefo_vec_Name_routine
		procedure::permuteback_routine
		procedure::permuteback_name_routine
		procedure::permuteback_vec_routine
		procedure::permuteback_vec_name_routine
		procedure::permuteback_Tensor_routine
		procedure::iElement
		procedure::sElement
		procedure::dElement
		procedure::cElement
		procedure::zElement
		procedure::lElement
		procedure::aElement
		procedure::iElement2
		procedure::sElement2
		procedure::dElement2
		procedure::cElement2
		procedure::zElement2
		procedure::lElement2
		procedure::aElement2
		procedure::ielementAll
		procedure::selementAll
		procedure::delementAll
		procedure::celementAll
		procedure::zelementAll
		procedure::lelementAll
		procedure::aelementAll
		procedure::TElement
		procedure::TElement2
		procedure::store_int
		procedure::store_real4
		procedure::store_real8
		procedure::store_com4
		procedure::store_com8
		procedure::store_logi
		procedure::store_char
		procedure:: store_T
		procedure::storeSome_int
		procedure::storeSome_real4
		procedure::storeSome_real8
		procedure::storeSome_com4
		procedure::storeSome_com8
		procedure::storeSome_logi
		procedure::storeSome_char
		procedure::storeSome_Tensor
		procedure::modify_Some_Data1_int
		procedure::modify_Some_Data1_real4
		procedure::modify_Some_Data1_real8
		procedure::modify_Some_Data1_com4
		procedure::modify_Some_Data1_com8
		procedure::modify_Some_Data1_char
		procedure::modify_Some_Data1_logi
		procedure::modify_Some_Data1_Tensor
		procedure::modify_Some_Data2_int
		procedure::modify_Some_Data2_real4
		procedure::modify_Some_Data2_real8
		procedure::modify_Some_Data2_com4
		procedure::modify_Some_Data2_com8
		procedure::modify_Some_Data2_char
		procedure::modify_Some_Data2_logi
		procedure::modify_Some_Data2_Tensor
		procedure::modify_Some_Data2_Tensor2
		
		procedure:: modifyTen_val_int
		procedure::modifyTen_val_real4
		procedure::modifyTen_val_real8
		procedure::modifyTen_val_com4
		procedure::modifyTen_val_com8
		procedure:: modifyTen_val_logi
		procedure::modifyTen_val_char
		procedure::modifyTen_val1_int
		procedure::modifyTen_val1_real4
		procedure::modifyTen_val1_real8
		procedure::modifyTen_val1_Tensor
		procedure:: modifyTen_val1_com4
		procedure::modifyTen_val1_com8
		procedure::modifyTen_val1_logi
		procedure::modifyTen_val1_char
		procedure::modifyTen_val_Tensor
		procedure::set_value_Tensor_int
		procedure::set_value_Tensor_real4
		procedure::set_value_Tensor_real8
		procedure::set_value_Tensor_com4
		procedure::set_value_Tensor_com8
		procedure::set_value_Tensor_logi
		procedure::set_value_Tensor_char
		procedure::set_random_Tensor
		procedure::set_random_Tensor_region4
		procedure::set_random_Tensor_regioni
		procedure::dcmplxmaxElement
		procedure::TmaxminElement
		procedure::TmaxElement
		procedure::TminElement
		procedure::intmaxElement
		procedure::realmaxElement
		procedure::dblemaxElement
		procedure:: cmplxmaxElement
		procedure::sumTensorT
		procedure::sumTensori
		procedure::sumTensors
		procedure::sumTensord
		procedure::sumTensorc
		procedure::sumTensorz
		
		procedure:: intminElement
		procedure::realminElement
		procedure:: dbleminElement
		procedure:: cmplxminElement
		procedure::dcmplxminElement
		!maxminflag=
		!'maxa': max abs
		!'mina': min abs
		!'maxr': max real
		!'minr': min real
		!'maxi': 0(not com) or max imag
		!'mini': 0(not com) or min imag
		procedure:: intmaxminElement
		procedure:: realmaxminElement
		procedure:: dblemaxminElement
		procedure::cmplxmaxminElement
		procedure::dcmplxmaxminElement
		procedure::getAllTenDim
		procedure::getTenDim_Namei!output the ith dimension of the Tensor, input the name of the dimension
													!If can not find , output 0
		procedure::getTenDim_i!output the i dimension of the Tensor,output an integer
!		procedure::getTenDim!output the Dimension of Tensor,return type(Dimension)
											!It can use to commit the assignment to a vector
											!vector=type(Dimension),if the vector is allocatable
	end type Tensor


!**************************************************************
!
!           WorkingMemory
!
!**************************************************************
	type(memory),save::WorkingMemory
	type(Tensor),target,save::WorkingTensor1,WorkingTensor2!use in contract
	type(dimension),target,save::Workingdimension4,Workingdimension5!!use in contract
	type(dimension),target,save::Workingdimension1,Workingdimension2,Workingdimension3!use in (*)
	
!
!--------------------------------------------------------------
!	


	public::Tensornode
	type,extends (Tensor) :: Tensornode
		private
		integer,allocatable:: indices(:)
		type(Tensornode),public,pointer :: next=>null()
	end type Tensornode
	
	public::Tensorlink
	type Tensorlink
		private
		integer :: length=0
		type(Tensornode),pointer  :: head
		type(Tensornode),pointer  :: Tend
		contains
			procedure,public::Tbackward =>push_backTen	
			procedure,public::Tforward=>push_fo
			procedure,public::forward =>push_foclass
			procedure,public::backward=>push_backclass
			generic,public::i =>Ti,TiI
			procedure::Ti
			procedure::TiI
			procedure,public::deallocate=>cleanlink
			procedure,public::modify=>modifylink
			procedure,public::linklength
			procedure,public::nodei!pointer to ith node
			procedure,public::addnode!add a empty node
			procedure,public::endnode!pointer to end node
			procedure,public::headnode!pointer to head node
			procedure,public::deletelink=>deletelinksubroutine
	end type Tensorlink	


! use in the old code
!**************************************************************************
!*********** SVD **************

!			link=SVD(T) or link=SVD(T,Ncut)
!				T should be two dimension
!				T=U*s*V^T,on output,the order in the link is:[U]->[s]->[V^T]
! 				T=(link.i.1)*(eye(link.i.2))*(link.i.3)
!
	public::SVDlink
	interface SVDlink
		module procedure SVD_link
		module procedure SVD_name_link
	end interface
	
!			svddecomposition(T,U,s,V),or svddecomposition(T,U,s,V,Ncut):
!			T should be two dimension	
! 				T=U*s*V^T,on output T=U*eye(s)*V	
!
	public::svddecompose
	interface svddecompose
		module procedure SVDTensor_noname
		module procedure SVDTensor_name
	end interface
	
	public::maxelement
	interface maxelement
		module procedure dcmplxmaxElement
	end interface
	public::minelement
	interface minelement
		module procedure dcmplxminElement
	end interface
!***********   interface for Tensorlink  and Tesorlist *************************	
	public::push_back
	interface push_back
		module procedure push_backclass
	end interface
	public::push_forward
	interface push_forward
		module procedure push_foclass
	end interface
	public::node_i
	interface node_i!(h,p,inde),return p,points to inde Tensor in the link or inde link in the list
		module procedure node_i_int
		module procedure node_i_vec
	end interface
	
	interface Tensor
		procedure constructor_int_scal
		procedure constructor_int_dim1
		procedure constructor_int_dim2
		procedure constructor_int_dim3
		procedure constructor_int_dim4
		
		procedure constructor_real4_scal
		procedure constructor_real4_dim1
		procedure constructor_real4_dim2
		procedure constructor_real4_dim3
		procedure constructor_real4_dim4
		
		procedure constructor_real8_scal
		procedure constructor_real8_dim1
		procedure constructor_real8_dim2
		procedure constructor_real8_dim3
		procedure constructor_real8_dim4
		
		procedure constructor_com4_scal
		procedure constructor_com4_dim1
		procedure constructor_com4_dim2
		procedure constructor_com4_dim3
		procedure constructor_com4_dim4
		
		procedure constructor_com8_scal
		procedure constructor_com8_dim1
		procedure constructor_com8_dim2
		procedure constructor_com8_dim3
		procedure constructor_com8_dim4
		
		procedure constructor_logi_scal
		procedure constructor_logi_dim1
		procedure constructor_logi_dim2
		procedure constructor_logi_dim3
		procedure constructor_logi_dim4
		
		procedure constructor_char_scal
		procedure constructor_char_dim1
		procedure constructor_char_dim2
		procedure constructor_char_dim3
		procedure constructor_char_dim4
	end interface
	
!		Print Tensor: 
!			Tprint(Tensor):print all the information of the Tensor
!			TDprint(Tensor):print Dimension
!			TMprint(Tesnor):Print as matrix 
!			Lprint(Tensorlink):print every indice of the Tensor in the link
!			LDprint(Tensorlink):print every dimension and indice of the Tensor in the link
	public::TMprint
	interface TMprint
		module procedure  TMprint1
		module procedure  TMprint2
		module procedure  TMprint3
		module procedure  TMprint4
	end interface
	public::Tprint
	interface Tprint
		module procedure  Tprint1
		module procedure  Tprint2
		module procedure  Tprint3
		module procedure  Tprint4
	end interface
	public::TDprint
	interface TDprint
		module procedure  TDprint1
		module procedure  TDprint2
		module procedure  TDprint3
	end interface
	
	public::MPI_send_Tensor
	interface MPI_send_Tensor
		module procedure  sent_Tensor
		module procedure  sent_Tensorlink
	end interface
	
	public::MPI_BCAST_Tensor
	interface MPI_BCAST_Tensor
		module procedure  BCAST_Tensor
		module procedure  BCAST_Tensorlink
	end interface
	
	public:: MPI_SUM_Tensor
	interface MPI_SUM_Tensor
		module procedure MPI_SUM_Tensor1
		module procedure MPI_SUM_Tensor2
	end interface
	public:: MPI_MAX_Tensor
	interface MPI_MAX_Tensor
		module procedure MPI_MAX_Tensor1
		module procedure MPI_MAX_Tensor2
	end interface
	public:: MPI_MIN_Tensor
	interface MPI_MIN_Tensor
		module procedure MPI_MIN_Tensor1
		module procedure MPI_MIN_Tensor2
	end interface

!allocate Tensor according to the dimen		
!allocate according to the value classType,if classType=0, use default_classtype_in_Tensor
!Can not apply to a allocated Tensor
!!	allocatedTensor will change the Dynamic of Tensor to .false.
!	public::allocateTensor
	interface allocateTensor
		module procedure  allocateTensor1
		module procedure  allocateTensor2
		module procedure  allocateTensor3
		module procedure  allocateTensor4
		module procedure  allocateTensor5
		module procedure  allocateTensor6
		module procedure  allocateTensor7
		module procedure  allocateTensor8
		module procedure  allocateTensor9
	end interface
!Can apply to a allocated Tensor	
!!	allocatedTensor will not change the Dynamic of Tensor
	public::allocatedTensor
	interface allocatedTensor
		module procedure  allocatedTensor1
		module procedure  allocatedTensor2
		module procedure  allocatedTensor3
		module procedure  allocatedTensor4
		module procedure  allocatedTensor5
		module procedure  allocatedTensor6!To be check
		module procedure  allocatedTensor7
		module procedure  allocatedTensor8
		module procedure  allocatedTensor9!To be check
	end interface
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
	public::contract
	interface contract
		module procedure contract_Same_name
		module procedure contract_noName
		module procedure contract_noName2
		module procedure contract_Name!In put dimension as character
		module procedure contract_Name2
		module procedure contract_Intname
		module procedure contract_Intname2
		module procedure contract_name_int
		module procedure contract_name_int2
		module procedure contract_intname_int
		module procedure contract_intname_int2
		module procedure contract_int_name
		module procedure contract_int_name2
		module procedure contract_int_intname
		module procedure contract_int_intname2
		module procedure contract_old
		module procedure contract_name_ownlegs
		module procedure contract_ownlegs
	end interface
	
	interface generate!it cost time
		module procedure generate_NoName1!generate a random Tensor 
		module procedure generate_NoName1_region4
		module procedure generate_NoName1_regioni
		module procedure generate_NoName2
		module procedure generate_NoName2_region4
		module procedure generate_NoName2_regioni
		module procedure generate_NoName3
		module procedure generate_NoName4
		module procedure generate_dim1
		module procedure generate_dim1_region4
		module procedure generate_dim1_regioni
		module procedure generate_dim2
		module procedure generate_dim2_region4
		module procedure generate_dim2_regioni
		module procedure generate_dim3
		module procedure generate_dim4
	end interface
	public::dimoperation
	interface dimoperation
		module procedure dimoperation1
		module procedure dimoperation2
	end interface
	public::zeroTen
	interface zeroTen!generate a Tensor with all element being 0d0
			module procedure zeroTen1
			module procedure zeroTen2
			module procedure zeroTen3
			module procedure zeroTen4
	end interface
	!public::modify
	interface modify
		module procedure modifyTen_val_int
		module procedure modifyTen_val_real4
		module procedure modifyTen_val_real8
		module procedure modifyTen_val_com4
		module procedure modifyTen_val_com8
		module procedure modifyTen_val_logi
		module procedure modifyTen_val_char
		module procedure modifyTen_val1_int
		module procedure modifyTen_val1_real4
		module procedure modifyTen_val1_real8
		module procedure modifyTen_val1_com4
		module procedure modifyTen_val1_com8
		module procedure modifyTen_val1_logi
		module procedure modifyTen_val1_char
		module procedure set_value_Tensor_int
		module procedure set_value_Tensor_real4
		module procedure set_value_Tensor_real8
		module procedure set_value_Tensor_com4
		module procedure set_value_Tensor_com8
		module procedure set_value_Tensor_logi
		module procedure set_value_Tensor_char
		module procedure store_int
		module procedure store_real4
		module procedure store_real8
		module procedure store_com4
		module procedure store_com8
		module procedure store_logi
		module procedure store_char
	end interface
	public::eye
	interface eye! some subroutine will conflict 
		module procedure  eye_int
		module procedure  eye_real4
		module procedure  eye_real8
		module procedure  eye_com4
		module procedure  eye_com8
		module procedure  eye_logi
		module procedure  eye_char
		module procedure  eye_Tensor
		module procedure  diag_int
		module procedure  identity_matrix_Tensor
		module procedure  diag_real4
		module procedure  diag_real8
		module procedure  diag_com4
		module procedure  diag_com8
		module procedure  diag_logi
		module procedure  diag_char
		module procedure  diag_type
	end interface
	!public::TenNameorder
	interface TenNameorder
		module procedure TenNameorder1
		module procedure TenNameorder2
		module procedure TenNameorder3
		module procedure TenNameorder4
	end interface
	!public::setTensorName
	interface setTensorName
		module procedure setTensorName2!set all the name in Tensor call setTensorName(T,'A') or setTensorName(T,'A12')
		module procedure setTensorName3!modify the name of ith index
		module procedure setTensorName4!modify the index whose name is old_w to new_w
		module procedure setTensorName5
		module procedure setTensorName6
		module procedure setTensorName7
		module procedure setTensorName8
		module procedure setTensorName9
	end interface
	public::conjg
	interface conjg
		module procedure conjugate! 
	end interface
	public::int
	interface int
		module procedure intTensor
	end interface
	public::real
	interface real
		module procedure realTensor
	end interface
	public::abs
	interface abs
		module procedure absTensor
	end interface
	public::dabs
	interface dabs
		module procedure dabsTensor
	end interface
	public::dble
	interface dble
		module procedure dbleTensor
	end interface
	public::cmplx
	interface cmplx
		module procedure cmplxTensor
		module procedure cmplxTensor2
	end interface
	public::dcmplx
	interface dcmplx
		module procedure dcmplxTensor
		module procedure dcmplxTensor2
	end interface
	public::char
	interface char
		module procedure  charTensor
	end interface
	public::aimag
	interface aimag
		module procedure  aimagTensor
	end interface
	public::dimag
	interface dimag
		module procedure  dimagTensor
	end interface
	public::max
	interface max
		module procedure  TmaxElement
		module procedure  TmaxminElement
	end interface
	public::imax
	interface imax
		module procedure  intmaxElement
		module procedure  intmaxminElement
	end interface
	public::smax
	interface smax
		module procedure  realmaxElement
		module procedure  realmaxminElement
	end interface
	public::dmax
	interface dmax
		module procedure  dblemaxElement
		module procedure  dblemaxminElement
	end interface
	public::cmax
	interface cmax
		module procedure  cmplxmaxElement
		module procedure  cmplxmaxminElement
	end interface
	public::zmax
	interface zmax
		module procedure  dcmplxmaxElement
		module procedure  dcmplxmaxminElement
	end interface
	public::maxval
	interface maxval
		module procedure  TmaxElement
		module procedure  TmaxminElement
	end interface
	public::min
	interface min
		module procedure  TminElement
		module procedure  TmaxminElement
	end interface
	public::imin
	interface imin
		module procedure  intminElement
		module procedure  intmaxminElement
	end interface
	public::smin
	interface smin
		module procedure  realminElement
		module procedure  realmaxminElement
	end interface
	public::dmin
	interface dmin
		module procedure  dbleminElement
		module procedure  dblemaxminElement
	end interface
	public::cmin
	interface cmin
		module procedure  cmplxminElement
		module procedure  cmplxmaxminElement
	end interface
	public::zmin
	interface zmin
		module procedure  dcmplxminElement
		module procedure  dcmplxmaxminElement
	end interface
	public::minval
	interface minval
		module procedure  TminElement
		module procedure  TmaxminElement
	end interface
	public::transpose
	interface transpose
		module procedure transposeTensor! conjugateTranspose! one or two dimension case
	end interface
	
	
	public::sum
	interface sum
		module procedure sumTensorT
	end interface
	public::isum
	interface isum
		module procedure sumTensori
	end interface
	public::ssum
	interface ssum
		module procedure sumTensors
	end interface
	public::dsum
	interface dsum
		module procedure sumTensord
	end interface
	public::csum
	interface csum
		module procedure sumTensorc
	end interface
	public::zsum
	interface zsum
		module procedure sumTensorz
	end interface
	
	
	public::isnan
	interface isnan
		module procedure isnanTensor
	end interface
	
	public::sqrt
	interface sqrt
		module procedure sqrt_Tensor
	end interface
	
	
	
	
	public::operator(.AddCol.)
	interface operator(.AddCol.)
		module procedure combinationCol
	end interface
	
	public::operator(.AddRow.)
	interface operator(.AddRow.)
		module procedure combinationRow
	end interface
!combinationCol:
!			T1 :a [...,l,m,n] matrix
!			T2 :a [...,l,m,n] matrix
!			combination(T1,T2):a [...,l,m,n,2] matrix
!			or 
!			T1 :a [...,m,n,l] matrix
!			T2 :a [...,m,n] matrix
!			combination(T1,T2):a [...,m,n,l+1] matrix
!combinationrow
!			T1 :a [l,m,n,...] matrix
!			T2 :a [l,m,n,...] matrix
!			combinationrow(T1,T2):a [2,l,m,n,...] matrix
!			or 
!			T1 :a [l,m,n,...] matrix
!			T2 :a [m,n,...] matrix
!			combinationrow(T1,T2):a [l+1,m,n,...] matrix	
	public::operator(.RPaste.)
	interface operator(.RPaste.)
		module procedure pasteTensorRow
	end interface
	public::operator(.CPaste.)
	interface operator(.CPaste.)
		module procedure pasteTensorCol
	end interface
!			T=T1.Rpaste.T2
!			T1 :a [l,m,n,...] matrix
!			T2 :a [l,m,n,...] matrix
!			pasteTensorRow(T1,T2):a [2*l,m,n,...] matrix
!        [1--->l, m, n,...] is T1
!			[l+1--->2*l, m, n,...] is T2
!        /   \
!        | T1 |
!        |----|
!        | T2 |
!        \    /
!			T=T1.Cpaste.T2
!pasteTensorCol(T1,T2,.false.)
!			T1 :a [...,m,n,l] matrix
!			T2 :a [...,m,n,l] matrix
!			pasteTensor(T1,T2):a [...,m,n,2*l] matrix
!        [... , m, n, 1--->l] is T1
!			[... , m, n, l+1--->2*l] is T2
!        /         \
!        | T1 | T2 |
!        \         /

	
	public::operator(.p.)
	interface operator(.p.)
		module procedure permute_rank2 !permute the tensor whose rank is 2,if rank=1 ,do nothing
		module procedure permute_rank3 !permute the tensor whose rank is 3
		module procedure permutation	 !permute the tensor of any rank,give the new order of the dimension
												 !		If operate on a big Tensor,use permute_rank2 or permute_rank3,
												 !	 they are faster.if rank>3,use contract to reshape
		module procedure permutation_name!input a character(:) as the new order
		module procedure permutation_Tensor ! if input 'A', goes worg, to be modify
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
	public::operator(.pi.)
	interface operator(.pi.)!T_{1,2,3,..,i,..,n},permuteInde(T,i)=_{2,3,..,i,1,i+1,..,n}
		module procedure permuteInde
		module procedure permuteInde_name
	end interface
	public::operator(.pbi.)
	interface operator(.pbi.)!T_{1,2,3,..,i,..,n},permutebackInde(T,i)=_{1,2,3,..,i-1,n,i,i+1,..,n-1}
		module procedure permutebackInde
		module procedure permutebackInde_name
	end interface	
	public::operator(.dim.)
	interface operator(.dim.)
		module procedure getTenDim_Namei!output the ith dimension of the Tensor, input the name of the dimension
													!If can not find , output 0
		module procedure getTenDim_i!output the i dimension of the Tensor,output an integer
		module procedure getAllTenDim!output the Dimension of Tensor,return array of integer
	end interface
	public::operator(.subDim.)
	interface operator(.subDim.)
		module procedure getTenSubDim!output the ith dimension of the Tensor,return type(Dimension)
		module procedure getTenSubDim3
		module procedure getTenSubDim_name
		module procedure getTenDim!output the Dimension of Tensor,return type(Dimension)
											!It can use to commit the assignment to a vector
											!vector=type(Dimension),if the vector is allocatable
	end interface
	
	public:: operator(.iname.)
	interface operator(.iname.)!the index name
		module procedure outIndexName
		module procedure outAllIndexName
	end interface
	public::operator(.iiname.)
	interface operator(.iiname.)!the index name
		module procedure outDimensionIntName
		module procedure outAllDimensionIntName
	end interface
	public::operator(.Tname.)
	interface operator(.Tname.)!the Tensor name
		module procedure outTensorName1
		module procedure outTensorNamecharFullname
		module procedure outAllTensorName
	end interface
	public::operator(.Tiname.)
	interface operator(.Tiname.)!the Tensor name
		module procedure outTensorIntName
		module procedure outAllTensorIntName
	end interface
	public::operator(.fuse.)
	interface operator(.fuse.)
		module procedure fuseTensor_val
		module procedure fuseTensor_vec
	end interface
	public::operator(.split.)
	interface operator(.split.)
		module procedure splitTensor2
		module procedure splitTensor3
		module procedure splitTensorAll
	end interface
	public::operator(.i.)
	interface operator(.i.)!
		module procedure TElement!output the data in the Tensor of Tim=(i,j),if rank<=4, the speed is the same as array,The program will check the input
		module procedure TElement2!output the data in the Tensor of inde
		module procedure Ti!output the i Tensor in the Tensorlink
		module procedure TiI!output the index Tensor in the Tensorlink,index is a vector
	end interface
	public::operator(.ii.)
	interface operator(.ii.)!
		module procedure ielementAll
		module procedure iElement!output the data in the Tensor of Tim=(i,j),if rank<=4, the speed is the same as array,The program will check the input
		module procedure iElement2!output the data in the Tensor of inde
	end interface
	public::operator(.si.)
	interface operator(.si.)!
		module procedure selementAll
		module procedure sElement!output the data in the Tensor of Tim=(i,j),if rank<=4, the speed is the same as array,The program will check the input
		module procedure sElement2!output the data in the Tensor of inde
	end interface
	public:: operator(.di.)
	interface operator(.di.)!
		module procedure delementAll
		module procedure dElement!output the data in the Tensor of Tim=(i,j),if rank<=4, the speed is the same as array,The program will check the input
		module procedure dElement2!output the data in the Tensor of inde
	end interface
	public::operator(.ci.)
	interface operator(.ci.)!
		module procedure celementAll
		module procedure cElement!output the data in the Tensor of Tim=(i,j),if rank<=4, the speed is the same as array,The program will check the input
		module procedure cElement2!output the data in the Tensor of inde
	end interface
	public::operator(.zi.)
	interface operator(.zi.)!
		module procedure zelementAll
		module procedure zElement!output the data in the Tensor of Tim=(i,j),if rank<=4, the speed is the same as array,The program will check the input
		module procedure zElement2!output the data in the Tensor of inde
	end interface
	public::operator(.li.)
	interface operator(.li.)!
		module procedure lelementAll
		module procedure lElement!output the data in the Tensor of Tim=(i,j),if rank<=4, the speed is the same as array,The program will check the input
		module procedure lElement2!output the data in the Tensor of inde
	end interface
	public::operator(.ai.)
	interface operator(.ai.)!
		module procedure aelementAll
		module procedure aElement!output the data in the Tensor of Tim=(i,j),if rank<=4, the speed is the same as array,The program will check the input
		module procedure aElement2!output the data in the Tensor of inde
	end interface
	public::operator(.mxx.)!direct Product,input two rank<=2 Tensor only
	interface operator(.mxx.)
		module procedure directProductM
	end interface
	public::operator(.xx.)!direct Product,input two rank<=2 Tensor only
	interface operator(.xx.)
		module procedure directProduct
	end interface
	public::operator(.kron.)!direct Product,support any rank tensor and keep their TensorName,see more in help/operator
	interface operator(.kron.)
		module procedure directProductTensor
	end interface
	public::operator(.x.)
	interface operator(.x.)! dot product conjugating the first vector,The Tensor will be regard as a vector
		module procedure TdotTensor
	end interface
	public::operator(.ix.)
	interface operator(.ix.)
		module procedure idotTensor
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
		module procedure TdotUTensor
	end interface
	public::operator(.idot.)
	interface operator(.idot.)
		module procedure idotUTensor
	end interface
	public::operator(.sdot.)
	interface operator(.sdot.)
		module procedure sdotUTensor
	end interface
	public::operator(.ddot.)
	interface operator(.ddot.)
		module procedure ddotUTensor
	end interface
	public::operator(.cdot.)
	interface operator(.cdot.)
		module procedure cdotUTensor
	end interface
	public::operator(.zdot.)
	interface operator(.zdot.)
		module procedure zdotUTensor
	end interface
	
	
	public::operator(.H.)
	interface operator(.H.)
		module procedure Htranspose
	end interface
	public::operator(.Hn.)
	interface operator(.Hn.)
		module procedure Htranspose2
	end interface
	public::operator(.T.)
	interface operator(.T.)
		module procedure transposeTensor! Transpose! one or two dimension case
	end interface
	
	public::operator(.inv.)
	interface operator(.inv.)
		module procedure inverse
		module procedure inverseTen
	end interface
	
	public::operator(.con.)
	interface operator(.con.)
		module procedure conjugate! conjugate
	end interface
	
	public::operator(+)
	interface operator(+)
		module procedure add
		module procedure add_int
		module procedure add_real4
		module procedure add_real8
		module procedure add_com4
		module procedure add_com8
		module procedure add_char
		module procedure add_int_
		module procedure add_real4_
		module procedure add_real8_
		module procedure add_com4_
		module procedure add_com8_
		module procedure add_char_
		module procedure connectlink!combine two Tensor link,the output will be a new link
	end interface
	public::operator(-)
	interface operator(-)
		module procedure minus
		module procedure minus_int
		module procedure minus_real4
		module procedure minus_real8
		module procedure minus_com4
		module procedure minus_com8
		module procedure minus_int_
		module procedure minus_real4_
		module procedure minus_real8_
		module procedure minus_com4_
		module procedure minus_com8_
	end interface
	public::operator(*)
	interface operator(*)
		module procedure multiply_number_int
		module procedure multiply_number_int_
		module procedure multiply_number_real4
		module procedure multiply_number_real4_
		module procedure multiply_number_real8
		module procedure multiply_number_real8_
		module procedure multiply_number_com4
		module procedure multiply_number_com4_
		module procedure multiply_number_com8
		module procedure multiply_number_com8_
		module procedure ProductTensor0
	end interface
	
	public::operator(/)
	interface operator(/)
		module procedure divide_Tensor
		module procedure int_divide_Tensor
		module procedure real4_divide_Tensor
		module procedure real8_divide_Tensor
		module procedure com4_divide_Tensor
		module procedure com8_divide_Tensor
		module procedure divide_num_int
		module procedure divide_num_real4
		module procedure divide_num_real8
		module procedure divide_num_com4
		module procedure divide_num_com8
	end interface
	
	
	public::copyTensor
	interface copyTensor!array=T ,array is a allocatable  and T are Tensor,array=T%Tensor_Data,used for any case,array will be allocate
		module procedure copyTensor_int_dim1_allocateble
		module procedure copyTensor_int_dim2_allocateble
		module procedure copyTensor_int_dim3_allocateble
		module procedure copyTensor_int_dim4_allocateble
		module procedure copyTensor_real4_dim1_allocateble
		module procedure copyTensor_real4_dim2_allocateble
		module procedure copyTensor_real4_dim3_allocateble
		module procedure copyTensor_real4_dim4_allocateble
		module procedure copyTensor_real8_dim1_allocateble
		module procedure copyTensor_real8_dim2_allocateble
		module procedure copyTensor_real8_dim3_allocateble
		module procedure copyTensor_real8_dim4_allocateble
		module procedure copyTensor_com4_dim1_allocateble
		module procedure copyTensor_com4_dim2_allocateble
		module procedure copyTensor_com4_dim3_allocateble
		module procedure copyTensor_com4_dim4_allocateble
		module procedure copyTensor_com8_dim1_allocateble
		module procedure copyTensor_com8_dim2_allocateble
		module procedure copyTensor_com8_dim3_allocateble
		module procedure copyTensor_com8_dim4_allocateble
		module procedure copyTensor_logi_dim1_allocateble
		module procedure copyTensor_logi_dim2_allocateble
		module procedure copyTensor_logi_dim3_allocateble
		module procedure copyTensor_logi_dim4_allocateble
		module procedure copyTensor_char_dim1_allocateble
		module procedure copyTensor_char_dim2_allocateble
		module procedure copyTensor_char_dim3_allocateble
		module procedure copyTensor_char_dim4_allocateble
	end interface
	
	
	public::assignment(=)
	interface assignment(=)
		module procedure assignmentTen!T1=T2 ,both T1 and T2 are Tensor
		module procedure assignmentTenNode
		module procedure assignmentNodeTen
		module procedure assignmentNodeNode
		module procedure assignmentdim_to_Ten
		module procedure assignmentTenarray_int1
		module procedure assignmentTenarray_int2
		module procedure assignmentTenarray_int3
		module procedure assignmentTenarray_int4
		module procedure assignmentTenarray_real4_1
		module procedure assignmentTenarray_real4_2
		module procedure assignmentTenarray_real4_3
		module procedure assignmentTenarray_real4_4
		module procedure assignmentTenarray_real8_1
		module procedure assignmentTenarray_real8_2
		module procedure assignmentTenarray_real8_3
		module procedure assignmentTenarray_real8_4
		module procedure assignmentTenarray_com4_1
		module procedure assignmentTenarray_com4_2
		module procedure assignmentTenarray_com4_3
		module procedure assignmentTenarray_com4_4
		module procedure assignmentTenarray_com8_1
		module procedure assignmentTenarray_com8_2
		module procedure assignmentTenarray_com8_3
		module procedure assignmentTenarray_com8_4
		module procedure assignmentTenarray_logi1
		module procedure assignmentTenarray_logi2
		module procedure assignmentTenarray_logi3
		module procedure assignmentTenarray_logi4
		module procedure assignmentTenarray_char1
		module procedure assignmentTenarray_char2
		module procedure assignmentTenarray_char3
		module procedure assignmentTenarray_char4
		
		module procedure copyTensor_int_dim1
		module procedure copyTensor_int_dim2
		module procedure copyTensor_int_dim3
		module procedure copyTensor_int_dim4
		module procedure copyTensor_real4_dim1
		module procedure copyTensor_real4_dim2
		module procedure copyTensor_real4_dim3
		module procedure copyTensor_real4_dim4
		module procedure copyTensor_real8_dim1
		module procedure copyTensor_real8_dim2
		module procedure copyTensor_real8_dim3
		module procedure copyTensor_real8_dim4
		module procedure copyTensor_com4_dim1
		module procedure copyTensor_com4_dim2
		module procedure copyTensor_com4_dim3
		module procedure copyTensor_com4_dim4
		module procedure copyTensor_com8_dim1
		module procedure copyTensor_com8_dim2
		module procedure copyTensor_com8_dim3
		module procedure copyTensor_com8_dim4
		module procedure copyTensor_logi_dim1
		module procedure copyTensor_logi_dim2
		module procedure copyTensor_logi_dim3
		module procedure copyTensor_logi_dim4
		module procedure copyTensor_char_dim1
		module procedure copyTensor_char_dim2
		module procedure copyTensor_char_dim3
		module procedure copyTensor_char_dim4
		
		
		module procedure assignmentTenNum_int
		module procedure assignmentTenNum_real4
		module procedure assignmentTenNum_real8
		module procedure assignmentTenNum_com4
		module procedure assignmentTenNum_com8
		module procedure assignmentTenNum_logi
		module procedure assignmentTenNum_char
		
		module procedure assignmentNumTen_int
		module procedure assignmentNumTen_real4
		module procedure assignmentNumTen_real8
		module procedure assignmentNumTen_com4
		module procedure assignmentNumTen_com8
		module procedure assignmentNumTen_logi
		module procedure assignmentNumTen_char
		
		module procedure assignmentTenArray!type(Tensor)::T1(len1),T2(len2),
!																			T1=T2,if len1<len2 call error_stop()
!																			if len1>len2 T1(1:len2)=T2,and no data in T1(len2+1:)
!																			That means getflag(T1(len2+1))=F
		module procedure assignmentTensorNodeArray
		module procedure copylinkhead
		module procedure linkToTenArray
		module procedure TenArrayToLink
		module procedure TenArrayToTensor
	end interface
	
	public::operator(.equ.)
	public::operator(.eq.)
	interface operator(.eq.)
		module procedure equal_of_Tensor
		module procedure T_eq_int
		module procedure T_eq_real4
		module procedure T_eq_real8
		module procedure int_eq_T
		module procedure real4_eq_T
		module procedure real8_eq_T
	end interface
	
	public::operator(.gt.)
	interface operator(.gt.)
		module procedure gt_of_Tensor
		module procedure T_gt_int
		module procedure T_gt_real4
		module procedure T_gt_real8
		module procedure int_gt_T
		module procedure real4_gt_T
		module procedure real8_gt_T
	end interface
	
	public::operator(.ge.)
	interface operator(.ge.)
		module procedure ge_of_Tensor
		module procedure T_ge_int
		module procedure T_ge_real4
		module procedure T_ge_real8
		module procedure int_ge_T
		module procedure real4_ge_T
		module procedure real8_ge_T
	end interface
	
	public::operator(.lt.)
	interface operator(.lt.)
		module procedure lt_of_Tensor
		module procedure T_lt_int
		module procedure T_lt_real4
		module procedure T_lt_real8
		module procedure int_lt_T
		module procedure real4_lt_T
		module procedure real8_lt_T
	end interface
	
	public::operator(.le.)
	interface operator(.le.)
		module procedure le_of_Tensor
		module procedure T_le_int
		module procedure T_le_real4
		module procedure T_le_real8
		module procedure int_le_T
		module procedure real4_le_T
		module procedure real8_le_T
	end interface
	public::writemess
	interface writemess
		module procedure writemess_Tensor
		module procedure writemess_Tensor_form
	end interface
	
	public::allocateCheck
	interface allocateCheck
		module procedure allocateCheck_Tensor
	end interface
!**********************************************************
!	Other Function or Subroutine:
!
!
!		get rank or totalData or flag
!			T%getTotalData
!			T%getflag
!
!
	public::set_SVD_S_matrix_flag!the output S matrix(single value matrox) of SVD will be a diag matrix,that is rank=2
	public::unset_SVD_S_matrix_flag!the output S matrix(single value matrox) of SVD will be a vector of all the single value, that is rank=1
	public::ifset_SVD_S_matrix_flag
	public::subTen
	
!
!		equal_of_Tensor:to judge if two Tensor are equak
!
!
!
!		QRlink:
!			make QR decompostion: A = QR, Q is an orthomomal matrix, and R is an upper triangle
!			The size of matrix A is M x N. the size of Q is M x min(M,N), R is  min(M,N) x N
!  		computes a QR factorization of a complex m by n matrix T  
!			A=(res.i.1)*(res.i.2)
!			ouput is a Tensorlink
!
!		LQlink:
!			make LQ decompostion: A = LQ, Q is an orthomomal matrix, and L is a lower triangle
!			matrix. The size of matrix A is M x N. the size of L is M x min(M,N), Q is  min(M,N) x N
!			A=(res.i.2)*(res.i.1)
!			ouput is a Tensorlink
!
!		QRdecompose
!			make QR decompostion: A = QR, Q is an orthomomal matrix, and R is an upper triangle
!			The size of matrix A is M x N. the size of Q is M x min(M,N), R is  min(M,N) x N
!  		computes a QR factorization of a complex m by n matrix T  
!			T=res(1)*res(2)
! 			the output is a array,if it is a array,T should be allocated
!
!
!		LQdecompose:
!			make LQ decompostion: A = LQ, Q is an orthomomal matrix, and L is a lower triangle
!  		matrix. The size of matrix A is M x N. the size of L is M x min(M,N), Q is  min(M,N) x N
!			T=res(2)*res(1)
!			the output is a array,if it is a array,T should be allocated
!
!
	public::linequ
!		linear equations
!			linequ:X=linequ(A,B),solve A*X=B,X could be a matrix
!A*X=B
!input A  and B
!output X
!if A^{-1} may not exit,input RCOND,
!perform SVD on A, Only keep  singular values S(i) <= RCOND*S(1) 
!if RCONDM<0 keep the S(i)>0
!
	public::pauli_matrix
!		pauli_matrix(Tx,Ty,Tz,num)
!
	public::expm
	interface expm
		module procedure  expmTensor
	end interface
!			return a Tensor e^H,
!
!
!
	public::combinationCol
!			T1 :a [...,l,m,n] matrix
!			T2 :a [...,l,m,n] matrix
!			combination(T1,T2):a [...,l,m,n,2] matrix
!			or 
!			T1 :a [...,m,n,l] matrix
!			T2 :a [...,m,n] matrix
!			combination(T1,T2):a [...,m,n,l+1] matrix
	public::combinationrow
!			T1 :a [l,m,n,...] matrix
!			T2 :a [l,m,n,...] matrix
!			combinationrow(T1,T2):a [2,l,m,n,...] matrix
!			or 
!			T1 :a [l,m,n,...] matrix
!			T2 :a [m,n,...] matrix
!			combinationrow(T1,T2):a [l+1,m,n,...] matrix			
!			
!		
!		SortTen:Bubble Sort,only rank=1 is allowed
!
!		Tensor index
	public::TenIndice
!			TenIndice(h,inde,indice):return the index of the inde Tensor.T_{1,1}->T_{1,2}->T_{1,3}
!				->T_{2,1}->T_{2,2},if input inde=2 => indice=[1,2].on entry the size of indice should
!				 be equal to the one in h
	public::TenIndiceLog
!			TenIndiceLog(h,inde,indice):if there no index in h,return .false..other while return inde
!				of the Tensor

!			lenOfIndicelist(h,inde,indice):return the index of the inde Tensorlink.T_{1,1}->T_{1,2}->T_{1,3}
!				->T_{2,1}->T_{2,2},if input inde=2 => indice=[1,2].on entry the size of indice should
!				 be equal to the one in h
!			linkIndiceLog(h,inde,indice):if there no index in h,return .false..other while return inde
!				of the Tensorlink
!
!		pointer subroutine
	public::ifnextnode,headnode,nextnode,endnode,addnode
!			ifnextnode:if the next node exict,p point to the next node,or p remain nochange
!			headnode:make the pointer points to the head of the link
!			nextnode:make the pointer points to the next node
!			endnode:make the pointer points to the end of the link
!			addnode:add a empty node to the link(then use a pointer to point to it)
!
!			ifnextlistnode:if the next node exict,p point to the next node,or p remain nochange(Tensorlist subroutine)
!			headlist:make the pointer points to the head of the list(Tensorlist subroutine)
!			nextlistnode:make the pointer points to the next node(Tensorlist subroutine)
!			endlist:make the pointer points to the end of the list(Tensorlist subroutine)
!			addlist:add a empty node to the link(then use a pointer to point to it)(Tensorlist subroutine)
!
	public::checklength,linklength,copylink
!		checklength(link):check and output length of the link,check if the length of the link is equal 
!				to the value in head of the link
!		checklistlength(list):check and output length of the list,check if the length of the list is equal 
!				to the value in head of the list
!
!		linklength(link):output length of the link
!		listlength(list):output length of the list
!
!		copylinkhead:copy the head of a link
!
!		modify(h,T,inde):modify the inde element of the link.The Tensor will be modify
!
!
	public::deletelink
!		deletelink(link_in,inde1,inde2):
!			link   :	T1->T2->...->T_{inde1-1}->T_inde1->T_{inde1+1}->....->T_{inde2-1}->T_inde2->T_{inde2+1}...->TN 
!		  result:		T1->T2->...->T_{inde1-1}->T_{inde1+1}....->T_{inde2-1}->->T_{inde2+1}...->TN 
!		  delete the inde1 to inde2 Tensor in the link,note that the deleted Tensor are include the Tensors of inde1 and inde2
!		deletelist(list_in,inde1,inde2):(Tensorlist subroutine)
!			list   :	T1->T2->...->T_{inde1-1}->T_inde1->T_{inde1+1}->....->T_{inde2-1}->T_inde2->T_{inde2+1}...->TN 
!		  result:		T1->T2->...->T_{inde1-1}->T_{inde1+1}....->T_{inde2-1}->->T_{inde2+1}...->TN 
!		  delete the inde1 to inde2 Tensorlink in the list,note that the deleted Tensorlink are include the Tensorlinks of inde1 and inde2
!
!		Some function about random number(in function.f90):
	public::randomnumber
	public::set_seed!set a random seed for the program,set_seed(idum),if idum=0,seed will generate by cpu
	public::out_randomseed!output the random seed of the program,seed=out_randomseed()
	public::out_and_set_seed!!set a random seed for the program,and then output the seed,seed=out_and_set_seed(),use to rebuild result
	public::set_writing_type!set_writing_type(chara,typ),chara=integer,real or some else,typ='(F25.8)','(I0)' or some else
	public::set_output_log_address!set the log file ,when use writemess
	public::set_output_log_unit!set the unit of log file ,when use writemess
	public::set_output_cpu!set the output cpu,when using writemess,by default , the output cpu is cpu0
	public::set_max_len_of_cha!define the max len of character when doing operation such as +
	
	public::initial_output_cpu_info!use in MPI program,(output_ProID_,output_ProNum_,output_Ierr_,MPICOMM),MPICOMM is optional
	public::set_output_cpu_info!use in MPI program,
	public::stop_program!use in MPI program
	public::error_stop
	public::outputmessTime
	public::Dimension,outTenNameLen,outDimNameLen,copydimension,if_original_dim
	public::BCAST_Tensor,sent_Tensor,BCAST_Dimension,sent_Dimension
	public::set_xgesdd_subroutine,set_xgesvd_subroutine
	public::set_array_character_divider,get_array_character_divider
!			randomnumber(): out put a random number from 0~1
!			set_seed(idum):idum a seed of integer,set the seed using for randomnumber,if call set_seed(0) of
!								Do not call this subroutine, the seed will be set by randomly.
!			out_randomseed():output the seed of the computer.
!			out_and_set_seed():set a seed randomly and output the seed. One should use it at the begining, and can repete
!								the result by call set_seed(idum) will the same idum at the begining.
!

!		warning function or testing function		
!			warning
!			warning2
!			NANwarning
!			NANwarning2
!			NANjudge
!			NANjudgev
!			check_zero_Ten
!
!
!		MPI function:
!			sent_Tensor(Ten1,Ten2,ID1,ID2,ierr,MPI_COMM):send the data of Ten1 in ID1 to Ten2 in ID2
!			BCAST_Tensor(Ten1,ID,ierr,MPI_COMM):BCAST The Ten1 in ID to every cpus
!			sent_Tensorlink(link1,link2,ID1,ID2,ierr,MPI_COMM):Send TensorLink,form link1 in ID1 to link2 in ID2
!			BCAST_Tensorlink(link1,ID,ierr,MPI_COMM):BCAST The link1 in ID to every CPUs
!
!			example
!				integer::ierr,proID,proNum
!				type(Tensor)::T1,T2
!				call initial_output_cpu_info(proID,proNum,ierr)
!				call sent_Tensor(T1,T2,0,1,ierr)  !T1 in cpu0 sent to cpu2 and store in T2
!				call BCAST_Tensor(T1,0,ierr) 		 !T1 in cpu 0 send to every cpus,store in T1 in other cpus
!				call stop_program()
!
!
!
!	Other operator ,see the note in file Dimension.f90
!
!			assignment(=):
!				assignment of the Dimension type;
!				Dimension=vector 
!					or
!				Dimension1=Dimension2 
!					or
!				vector=Dimension
!
!			operator(+):
!				Dimension1+Dimension2:[2,3]+[4,5,6]->[2,3,4,5,6] 
!				Dimension+vector 
!					 
!			operator(.sub.)
!				get the ith of dimension renturn dimenison tpye
!
!			operator(.i.)
!				get the ith of dimension data
!
!			operator(.equ.)
!				equal_of_array:If two array of one dimension are equal
!				equal_of_dim:If two type(dimension) are equal
!
!
!**********************************************************	
	public::Working_memory_report,Tensor_memory_report,Tensor_memory_length,dellocate_Tensor_memory


	interface modifyTen_val_class
		module procedure modifyTen_val_class_i
		module procedure modifyTen_val_class_s
		module procedure modifyTen_val_class_d
		module procedure modifyTen_val_class_c
		module procedure modifyTen_val_class_z
		module procedure modifyTen_val_class_l
		module procedure modifyTen_val_class_a
	end interface

contains
	function if_simple_dimension(T)
		logical::if_simple_dimension
		class(Tensor)::T
		if_simple_dimension=T%TenDim%if_simple_dimension()
		return
	end function
	subroutine set_SVD_S_matrix_flag()
		SVD_S_matrix_flag=.true.
		return
	end subroutine
	logical function ifset_SVD_S_matrix_flag()
		ifset_SVD_S_matrix_flag=SVD_S_matrix_flag
	end function
	subroutine unset_SVD_S_matrix_flag()
		SVD_S_matrix_flag=.false.
		return
	end subroutine
	subroutine dellocate_Tensor_memory()
		call WorkingMemory%deallocate()
		call dellocate_Dimension_memory
		call dellocate_TData_memory()
		call WorkingTensor1%deallocate()
		call WorkingTensor2%deallocate()
		call Workingdimension2%deallocate()
		call Workingdimension1%deallocate()
		call Workingdimension3%deallocate()
		return
	end subroutine
	subroutine Working_memory_report()
		integer::Tdatalength(7),Dimensionlength(7),Tensorlength(7)
		call Dimension_memory_length(Dimensionlength)
		call TData_memory_length(Tdatalength)
		call Tensor_memory_length(Tensorlength)
		Tensorlength=Tensorlength+Tdatalength+Dimensionlength
		call WorkingTensor1%TData_length(Dimensionlength)
		call WorkingTensor2%TData_length(Tdatalength)
		Tensorlength=Tensorlength+Tdatalength+Dimensionlength
		Tensorlength(1)=Tensorlength(1)+Workingdimension1%outlenDimData()+&
			Workingdimension2%outlenDimData()+Workingdimension3%outlenDimData()
		call writemess('The length of the memory used in Dimension, Tensor and TData are')
		call writemess('integer         :'+Tensorlength(1))
		call writemess('real(kind=4)    :'+Tensorlength(2))
		call writemess('real(kind=8)    :'+Tensorlength(3))
		call writemess('complex(kind=4) :'+Tensorlength(4))
		call writemess('complex(kind=8) :'+Tensorlength(5))
		call writemess('logical         :'+Tensorlength(6))
		call writemess('character       :'+Tensorlength(7))
			return
	end subroutine	
	subroutine Tensor_memory_report()
		call writemess('The memory used in Tensor are:')
		call WorkingMemory%print()
		call writemess(' ')
		call writemess('The length of the working Tensor are'+(WorkingTensor1%getTotalData()+&
			WorkingTensor2%getTotalData()))
		call writemess('The length of the working Dimension are'+(Workingdimension1%outlenDimData()+&
			Workingdimension2%outlenDimData()+Workingdimension3%outlenDimData()))
	end subroutine	
	subroutine Tensor_memory_length(length)
		integer,intent(inout)::length(:)
		call WorkingMemory%getlength(length)
	end subroutine	
	subroutine allocateCheck_Tensor(A,lenA)
		type(Tensor),allocatable,intent(inout) ::A(:)
		integer::lenA
		if(allocated(A)) then
			if(size(A).ltne.lenA) then
				deallocate(A)
				allocate(A(lenA))
			end if
		else
			allocate(A(lenA))
		end if
		return
	end subroutine
	
!*********************  getRank	 **********************
	integer function getRank(T)
		class(Tensor),intent(in) :: T
		getRank=T%rank
	end function
	integer function outNameFlag(T)
		class(Tensor),intent(in) :: T
		outNameFlag=T%TenDim%outNameFlag()
		return
	end function

	logical function if_original_dim_in_T(T)
		class(Tensor),intent(in) :: T
		if_original_dim_in_T=T%TenDim%if_original_dim()
	end function
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                    allocateTensor    
!
!**************************************************************************************************************
!**************************************************************************************************************

!allocate Tensor according to the dimen
	subroutine allocateTensor1(T,dimen,typede)
		class(Tensor),intent(inout) ::T
		type(dimension),intent(in)::dimen
		integer,intent(in)::typede
		integer::length
		if(getflag(T))then
			call writemess('Can not allocate to a allocated Tensor')
			call error_stop
		end if
		T%rank=dimen%getRank()
		T%TenDim=dimen
		length=dimen%size()
		if(length.le.0) then
			write(*,*)"ERROR in allocateTensor"
			call dimen%print()
			call error_stop()
		end if
		call allocateData(T%TData,length,typede,.true.)
		return
	end subroutine
!allocate Tensor according to the dimen
	subroutine allocateTensor2(T,dimen,typede)
		class(Tensor),intent(inout) ::T
		integer,intent(in)::dimen(:)
		integer,intent(in)::typede
		integer::length
		if(getflag(T))then
			call writemess('Can not allocate to a allocated Tensor')
			call error_stop
		end if
		T%rank=size(dimen)
		T%TenDim=dimen
		length=product(dimen)
		if(length.le.0) then
			write(*,*)"ERROR in allocateTensor"
			write(*,*)dimen
			call error_stop()
		end if
		call allocateData(T%TData,length,typede,.true.)
		return
	end subroutine
	subroutine allocateTensor3(T,T2,typede)
		class(Tensor),intent(inout) ::T
		class(Tensor),intent(in) ::T2
		integer,intent(in)::typede
		integer::length
		if(getflag(T))then
			call writemess('Can not allocate to a allocated Tensor')
			call error_stop
		end if
		if(.not.getflag(T2))then
			call emptyTensor(T)
			return
		end if
		T%rank=T2%rank
		T%TenDim=T2%TenDim
		length=gettotalData(T2)
		call allocateData(T%TData,length,typede,.true.)
		return
	end subroutine
	subroutine allocateTensor4(T,dimen,typede)
		class(Tensor),intent(inout) ::T
		type(dimension),intent(in)::dimen
		character(len=*),intent(in)::typede
		logical::deallocateflag
		integer::length
		if(getflag(T))then
			call writemess('Can not allocate to a allocated Tensor')
			call error_stop
		end if
		T%rank=dimen%getRank()
		T%TenDim=dimen
		length=dimen%size()
		if(length.le.0) then
			write(*,*)"ERROR in allocateTensor"
			call dimen%print()
			call error_stop()
		end if
		call allocateData(T%TData,length,typede,.true.)
		return
	end subroutine
!allocate Tensor according to the dimen
	subroutine allocateTensor5(T,dimen,typede)
		class(Tensor),intent(inout) ::T
		integer,intent(in)::dimen(:)
		character(len=*),intent(in)::typede
		logical::deallocateflag
		integer::length
		if(getflag(T))then
			call writemess('Can not allocate to a allocated Tensor')
			call error_stop
		end if
		T%rank=size(dimen)
		T%TenDim=dimen
		length=product(dimen)
		if(length.le.0) then
			write(*,*)"ERROR in allocateTensor"
			write(*,*)dimen
			call error_stop()
		end if
		call allocateData(T%TData,length,typede,.true.)
		return
	end subroutine
	subroutine allocateTensor6(T,T2,typede)
		class(Tensor),intent(inout) ::T
		class(Tensor),intent(in) ::T2
		character(len=*),intent(in)::typede
		integer::length
		if(getflag(T))then
			call writemess('Can not allocate to a allocated Tensor')
			call error_stop
		end if
		if(.not.getflag(T2))then
			call emptyTensor(T)
			return
		end if
		T%rank=T2%rank
		T%TenDim=T2%TenDim
		length=gettotalData(T2)
		call allocateData(T%TData,length,typede,.true.)
		return
	end subroutine
	subroutine allocateTensor7(T,dimen)
		class(Tensor),intent(inout) ::T
		type(dimension),intent(in)::dimen
		logical::deallocateflag
		integer::length
		if(getflag(T))then
			call writemess('Can not allocate to a allocated Tensor')
			call error_stop
		end if
		T%rank=dimen%getRank()
		T%TenDim=dimen
		length=dimen%size()
		if(length.le.0) then
			write(*,*)"ERROR in allocateTensor"
			call dimen%print()
			call error_stop()
		end if
		call allocateData(T%TData,length,.false.)
		return
	end subroutine
	subroutine allocateTensor8(T,dimen)
		class(Tensor),intent(inout) ::T
		integer,intent(in)::dimen(:)
		logical::deallocateflag
		integer::length
		if(getflag(T))then
			call writemess('Can not allocate to a allocated Tensor')
			call error_stop
		end if
		T%rank=size(dimen)
		T%TenDim=dimen
		length=product(dimen)
		if(length.le.0) then
			write(*,*)"ERROR in allocateTensor"
			write(*,*)dimen
			call error_stop()
		end if
		call allocateData(T%TData,length,.false.)
		return
	end subroutine
	subroutine allocateTensor9(T,T2)
		class(Tensor),intent(inout) ::T
		class(Tensor),intent(in) ::T2
		integer::length
		if(getflag(T))then
			call writemess('Can not allocate to a allocated Tensor')
			call error_stop
		end if
		if(.not.getflag(T2))then
			call emptyTensor(T)
			return
		end if
		T%rank=T2%rank
		T%TenDim=T2%TenDim
		length=gettotalData(T2)
		call allocateData(T%TData,length,T2%GetType(),.false.)
		return
	end subroutine
	
!	allocatedTensor will not change the Dynamic of Tensor
	subroutine allocatedTensor1(T,dimen,typede)
		class(Tensor),intent(inout) ::T
		type(dimension),intent(in)::dimen
		integer,intent(in)::typede
		logical::deallocateflag
		integer::length
		T%rank=dimen%getRank()
		T%TenDim=dimen
		length=dimen%size()
		if(length.le.0) then
			call writemess("ERROR in allocatedTensor of input dimension, total length=0",-1)
			call dimen%print()
			call error_stop()
		end if
		call allocateData(T%TData,length,typede)
		return
	end subroutine
!allocate Tensor according to the dimen
	subroutine allocatedTensor2(T,dimen,typede)
		class(Tensor),intent(inout) ::T
		integer,intent(in)::dimen(:)
		integer,intent(in)::typede
		logical::deallocateflag
		integer::length
		T%rank=size(dimen)
		T%TenDim=dimen
		length=product(dimen)
		if(length.le.0) then
			call writemess("ERROR in allocatedTensor of input dimension, total length=0",-1)
			write(*,*)dimen
			call error_stop()
		end if
		call allocateData(T%TData,length,typede)
		return
	end subroutine
	subroutine allocatedTensor3(T,T2,typede)
		class(Tensor),intent(inout) ::T
		class(Tensor),intent(in) ::T2
		integer,intent(in)::typede
		integer::length
		if(.not.getflag(T2))then
			call emptyTensor(T)
			return
		end if
		T%rank=T2%rank
		T%TenDim=T2%TenDim
		length=gettotalData(T2)
		call allocateData(T%TData,length,typede)
		return
	end subroutine
	subroutine allocatedTensor4(T,dimen,typede)
		class(Tensor),intent(inout) ::T
		type(dimension),intent(in)::dimen
		character(len=*),intent(in)::typede
		logical::deallocateflag
		integer::length
		T%rank=dimen%getRank()
		T%TenDim=dimen
		length=dimen%size()
		if(length.le.0) then
			write(*,*)"ERROR in allocatefTensor"
			call dimen%print()
			call error_stop()
		end if
		call allocateData(T%TData,length,typede)
		return
	end subroutine
!allocate Tensor according to the dimen
	subroutine allocatedTensor5(T,dimen,typede)
		class(Tensor),intent(inout) ::T
		integer,intent(in)::dimen(:)
		character(len=*),intent(in)::typede
		logical::deallocateflag
		integer::length
		T%rank=size(dimen)
		T%TenDim=dimen
		length=product(dimen)
		if(length.le.0) then
			call writemess("ERROR in allocatedTensor of input dimension, total length=0",-1)
			write(*,*)dimen
			call error_stop()
		end if
		call allocateData(T%TData,length,typede)
		return
	end subroutine
	subroutine allocatedTensor6(T,T2,typede)
		class(Tensor),intent(inout) ::T
		class(Tensor),intent(in) ::T2
		character(len=*),intent(in)::typede
		integer::length
		if(.not.getflag(T2))then
			call emptyTensor(T)
			return
		end if
		T%rank=T2%rank
		T%TenDim=T2%TenDim
		length=gettotalData(T2)
		call allocateData(T%TData,length,typede)
		return
	end subroutine
	subroutine allocatedTensor7(T,dimen)
		class(Tensor),intent(inout) ::T
		type(dimension),intent(in)::dimen
		logical::deallocateflag
		integer::length
		T%rank=dimen%getRank()
		T%TenDim=dimen
		length=dimen%size()
		if(length.le.0) then
			call writemess("ERROR in allocatedTensor of input dimension, total length=0",-1)
			call dimen%print()
			call error_stop()
		end if
		call allocateData(T%TData,length)
		return
	end subroutine
!allocate Tensor according to the dimen
	subroutine allocatedTensor8(T,dimen)
		class(Tensor),intent(inout) ::T
		integer,intent(in)::dimen(:)
		logical::deallocateflag
		integer::length
		T%rank=size(dimen)
		T%TenDim=dimen
		length=product(dimen)
		if(length.le.0) then
			call writemess("ERROR in allocatedTensor of input dimension, total length=0",-1)
			write(*,*)dimen
			call error_stop()
		end if
		call allocateData(T%TData,length)
		return
	end subroutine
	subroutine allocatedTensor9(T,T2)
		class(Tensor),intent(inout) ::T
		class(Tensor),intent(in) ::T2
		integer::length
		T%rank=T2%rank
		T%TenDim=T2%TenDim
		length=gettotalData(T2)
		call allocateData(T%TData,length,T2%getType())
		return
	end subroutine

!**************************************************************************************************************
!**************************************************************************************************************
!
!                                    deallocateTensor    
!
!**************************************************************************************************************
!**************************************************************************************************************
!*************** cleanTensor  *****************
!allocate memory
	subroutine cleanTensor(T)
		class(Tensor),intent(inout)::T
		T%rank=0
		call T%TenDim%deallocate()
		call cleanTData(T)
		select type(T)
			type is (Tensor)
				return
			type is (Tensornode)
				T%next=>null()
				if(allocated(T%indices))then
					deallocate(T%indices)
				end if
				return
		end select 
	end subroutine
!**************** emptyTensor *******************
!set it as a empty Tensor
	subroutine emptyTensor(T)
		class(Tensor),intent(inout)::T
		T%rank=0
		call T%TenDim%empty()
		call emptyTData(T)
		select type(T)
			type is (Tensor)
				return
			type is (Tensornode)
				T%next=>null()
				if(allocated(T%indices))then
					deallocate(T%indices)
				end if
				return
		end select 
		return
	end subroutine

!**************************************************************************************************************
!**************************************************************************************************************
!
!                                    assignment to Tensor
!
!**************************************************************************************************************
!**************************************************************************************************************	
	subroutine setindices_to_node(T,indices)
		class(TensorNode),intent(inout)::T
		integer,intent(in)::indices(:)
		integer::length
		length=size(indices)
		if(allocated(T%indices))then
			if(size(T%indices).ne.length)then
				deallocate(T%indices)
				allocate(T%indices(length))
			end if
		else
			allocate(T%indices(length))
		end if
		T%indices=indices
		return
	end subroutine
	subroutine assignmentTen(T,T2)
		type(Tensor),intent(inout) ::T
		type(Tensor),intent(in) :: T2
		if(.not.getFlag(T2))then
			call emptyTensor(T)
			return
		end if
		call allocatedTensor9(T,T2)
		call assignmentTData_routine(T%TData,T2%TData)
		return
		return
	end subroutine
	subroutine assignmentTenNode(T,T2)
		type(Tensor),intent(inout) ::T
		type(TensorNode),intent(in) :: T2
		T=T2%Tensor
		return
	end subroutine
	subroutine assignmentNodeTen(T,T2)
		type(TensorNode),intent(inout) ::T
		type(Tensor),intent(in) :: T2
		T%Tensor=T2
		T%next=>null()
		return
	end subroutine
	subroutine assignmentNodeNode(T,T2)
		type(TensorNode),intent(inout) ::T
		type(TensorNode),intent(in) :: T2
		T%Tensor=T2%Tensor
		T%next=>T2%next
		call setindices_to_node(T,T2%indices)
		return
	end subroutine
	subroutine assignmentdim_to_Ten(T,dimen)
		type(Tensor),intent(inout) ::T
		type(dimension),intent(in) :: dimen
		integer,allocatable::dimendata(:)
		call copydimension(dimendata,dimen)
		T=dimendata
		return
	end subroutine
	subroutine assignmentTenArray(T,T2)
		type(Tensor),intent(inout) ::T(:)
		type(Tensor),intent(in) :: T2(:)
		integer::length,i
		length=size(T2)
		if(size(T).lt.length)then
			write(*,*)"ERROR in assignment of two Tensor array "
			write(*,*)"T1(:)=T2(:),size(T1)<size(T2)"
			write(*,*)size(T),length
			call error_stop()
		end if
		do i=1,length
			T(i)=T2(i)
		end do
		return
	end subroutine
	subroutine assignmentTensorNodeArray(T,T2)
		type(Tensornode),intent(inout) ::T(:)
		type(Tensornode),intent(in) :: T2(:)
		integer::length,i
		length=size(T2)
		if(size(T).lt.length)then
			write(*,*)"ERROR in assignment of two Tensor array "
			write(*,*)"T1(:)=T2(:),size(T1)<size(T2)"
			write(*,*)size(T),length
			call error_stop()
		end if
		do i=1,length
			T(i)=T2(i)
		end do
		return
	end subroutine
	subroutine assignmentTenarray_int1(T,Tensor_data)
		type(Tensor),intent(inout)::T
		integer,intent(in)::Tensor_data(:)
		integer::length
		length=size(Tensor_data)
		call allocatedTensor(T,(/length/),1)
		call assignmentTData_int(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_int2(T,Tensor_data)
		type(Tensor),intent(inout)::T
		integer,intent(in)::Tensor_data(:,:)
		integer::length
		integer::dimen(2)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,1)
		call assignmentTData_int(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_int3(T,Tensor_data)
		type(Tensor),intent(inout)::T
		integer,intent(in)::Tensor_data(:,:,:)
		integer::length
		integer::dimen(3)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,1)
		call assignmentTData_int(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_int4(T,Tensor_data)
		type(Tensor),intent(inout)::T
		integer,intent(in)::Tensor_data(:,:,:,:)
		integer::length
		integer::dimen(4)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,1)
		call assignmentTData_int(T%TData,Tensor_data,length)
		return
	end subroutine


	subroutine assignmentTenarray_real4_1(T,Tensor_data)
		type(Tensor),intent(inout)::T
		real(kind=4),intent(in)::Tensor_data(:)
		integer::length
		length=size(Tensor_data)
		call allocatedTensor(T,(/length/),2)
		call assignmentTData_real4(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_real4_2(T,Tensor_data)
		type(Tensor),intent(inout)::T
		real(kind=4),intent(in)::Tensor_data(:,:)
		integer::length
		integer::dimen(2)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,2)
		call assignmentTData_real4(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_real4_3(T,Tensor_data)
		type(Tensor),intent(inout)::T
		real(kind=4),intent(in)::Tensor_data(:,:,:)
		integer::length
		integer::dimen(3)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,2)
		call assignmentTData_real4(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_real4_4(T,Tensor_data)
		type(Tensor),intent(inout)::T
		real(kind=4),intent(in)::Tensor_data(:,:,:,:)
		integer::length
		integer::dimen(4)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,2)
		call assignmentTData_real4(T%TData,Tensor_data,length)
		return
	end subroutine
	
	subroutine assignmentTenarray_real8_1(T,Tensor_data)
		type(Tensor),intent(inout)::T
		real(kind=8),intent(in)::Tensor_data(:)
		integer::length
		length=size(Tensor_data)
		call allocatedTensor(T,(/length/),3)
		call assignmentTData_real8(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_real8_2(T,Tensor_data)
		type(Tensor),intent(inout)::T
		real(kind=8),intent(in)::Tensor_data(:,:)
		integer::length
		integer::dimen(2)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,3)
		call assignmentTData_real8(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_real8_3(T,Tensor_data)
		type(Tensor),intent(inout)::T
		real(kind=8),intent(in)::Tensor_data(:,:,:)
		integer::length
		integer::dimen(3)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,3)
		call assignmentTData_real8(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_real8_4(T,Tensor_data)
		type(Tensor),intent(inout)::T
		real(kind=8),intent(in)::Tensor_data(:,:,:,:)
		integer::length
		integer::dimen(4)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,3)
		call assignmentTData_real8(T%TData,Tensor_data,length)
		return
	end subroutine
	

	subroutine assignmentTenarray_com4_1(T,Tensor_data)
		type(Tensor),intent(inout)::T
		complex(kind=4),intent(in)::Tensor_data(:)
		integer::length
		length=size(Tensor_data)
		call allocatedTensor(T,(/length/),4)
		call assignmentTData_com4(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_com4_2(T,Tensor_data)
		type(Tensor),intent(inout)::T
		complex(kind=4),intent(in)::Tensor_data(:,:)
		integer::length
		integer::dimen(2)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,4)
		call assignmentTData_com4(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_com4_3(T,Tensor_data)
		type(Tensor),intent(inout)::T
		complex(kind=4),intent(in)::Tensor_data(:,:,:)
		integer::length
		integer::dimen(3)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,4)
		call assignmentTData_com4(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_com4_4(T,Tensor_data)
		type(Tensor),intent(inout)::T
		complex(kind=4),intent(in)::Tensor_data(:,:,:,:)
		integer::length
		integer::dimen(4)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,4)
		call assignmentTData_com4(T%TData,Tensor_data,length)
		return
	end subroutine
	
	subroutine assignmentTenarray_com8_1(T,Tensor_data)
		type(Tensor),intent(inout)::T
		complex(kind=8),intent(in)::Tensor_data(:)
		integer::length
		length=size(Tensor_data)
		call allocatedTensor(T,(/length/),5)
		call assignmentTData_com8(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_com8_2(T,Tensor_data)
		type(Tensor),intent(inout)::T
		complex(kind=8),intent(in)::Tensor_data(:,:)
		integer::length
		integer::dimen(2)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,5)
		call assignmentTData_com8(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_com8_3(T,Tensor_data)
		type(Tensor),intent(inout)::T
		complex(kind=8),intent(in)::Tensor_data(:,:,:)
		integer::length
		integer::dimen(3)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,5)
		call assignmentTData_com8(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_com8_4(T,Tensor_data)
		type(Tensor),intent(inout)::T
		complex(kind=8),intent(in)::Tensor_data(:,:,:,:)
		integer::length
		integer::dimen(4)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,5)
		call assignmentTData_com8(T%TData,Tensor_data,length)
		return
	end subroutine	

	

	subroutine assignmentTenarray_logi1(T,Tensor_data)
		type(Tensor),intent(inout)::T
		logical,intent(in)::Tensor_data(:)
		integer::length
		length=size(Tensor_data)
		call allocatedTensor(T,(/length/),6)
		call assignmentTData_logi(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_logi2(T,Tensor_data)
		type(Tensor),intent(inout)::T
		logical,intent(in)::Tensor_data(:,:)
		integer::length
		integer::dimen(2)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,6)
		call assignmentTData_logi(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_logi3(T,Tensor_data)
		type(Tensor),intent(inout)::T
		logical,intent(in)::Tensor_data(:,:,:)
		integer::length
		integer::dimen(3)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,6)
		call assignmentTData_logi(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_logi4(T,Tensor_data)
		type(Tensor),intent(inout)::T
		logical,intent(in)::Tensor_data(:,:,:,:)
		integer::length
		integer::dimen(4)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,6)
		call assignmentTData_logi(T%TData,Tensor_data,length)
		return
	end subroutine
	
	subroutine assignmentTenarray_char1(T,Tensor_data)
		type(Tensor),intent(inout)::T
		character(len=*),intent(in)::Tensor_data(:)
		integer::length
		length=size(Tensor_data)
		call allocatedTensor(T,(/length/),7)
		call assignmentTData_char(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_char2(T,Tensor_data)
		type(Tensor),intent(inout)::T
		character(len=*),intent(in)::Tensor_data(:,:)
		integer::length
		integer::dimen(2)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,7)
		call assignmentTData_char(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_char3(T,Tensor_data)
		type(Tensor),intent(inout)::T
		character(len=*),intent(in)::Tensor_data(:,:,:)
		integer::length
		integer::dimen(3)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,7)
		call assignmentTData_char(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenarray_char4(T,Tensor_data)
		type(Tensor),intent(inout)::T
		character(len=*),intent(in)::Tensor_data(:,:,:,:)
		integer::length
		integer::dimen(4)
		length=size(Tensor_data)
		dimen=shape(Tensor_data)
		call allocatedTensor(T,dimen,7)
		call assignmentTData_char(T%TData,Tensor_data,length)
		return
	end subroutine
	subroutine assignmentTenNum_int(T,num)
		type(Tensor),intent(inout)::T
		integer,intent(in)::num
		T=(/num/)
		return
	end subroutine
	subroutine assignmentTenNum_real4(T,num)
		type(Tensor),intent(inout)::T
		real(kind=4),intent(in)::num
			T=(/num/)
		return
	end subroutine
	subroutine assignmentTenNum_real8(T,num)
		type(Tensor),intent(inout)::T
		real(kind=8),intent(in)::num
			T=(/num/)
		return
	end subroutine
	subroutine assignmentTenNum_com4(T,num)
		type(Tensor),intent(inout)::T
		complex(kind=4),intent(in)::num
			T=(/num/)
		return
	end subroutine
	subroutine assignmentTenNum_com8(T,num)
		type(Tensor),intent(inout)::T
		complex(kind=8),intent(in)::num
			T=(/num/)
		return
	end subroutine
	subroutine assignmentTenNum_logi(T,num)
		type(Tensor),intent(inout)::T
		logical,intent(in)::num
			T=(/num/)
		return
	end subroutine
	subroutine assignmentTenNum_char(T,num)
		type(Tensor),intent(inout)::T
		character(len=*),intent(in)::num
		T=(/num/)
		return
	end subroutine
	subroutine assignmentNumTen_int(val,T)
		integer,intent(inout)::val
		type(Tensor),intent(in)::T
		if(getTotalData(T).eq.1) then
			call assignment_int_Tdata_value(val,T%TData)
		else 
			write(*,*)"ERROR in assignment for Tensor to integer"
			call error_stop()
		end if
		return
	end subroutine
	subroutine assignmentNumTen_real4(val,T)
		real(kind=4),intent(inout)::val
		type(Tensor),intent(in)::T
		if(getTotalData(T).eq.1) then
			call assignment_real4_Tdata_value(val,T%TData)
		else if(.not.T%getFlag())then
			call writemess("ERROR in assignment for Tensor to real",-1)
			call writemess("The Tensor is a empty Tensor",-1)
			call error_stop()
		else
			call writemess("ERROR in assignment for Tensor to real",-1)
			call T%print()
			call error_stop()
		end if
		return
	end 	subroutine 
	subroutine assignmentNumTen_real8(val,T)
		real(kind=8),intent(inout)::val
		type(Tensor),intent(in)::T
		if(getTotalData(T).eq.1) then
			call assignment_real8_Tdata_value(val,T%TData)
		else if(.not.T%getFlag())then
			call writemess("ERROR in assignment for Tensor to real",-1)
			call writemess("The Tensor is a empty Tensor",-1)
			call error_stop()
		else
			call writemess("ERROR in assignment for Tensor to real",-1)
			call T%print()
			call error_stop()
		end if
		return
	end subroutine
	subroutine assignmentNumTen_com4(val,T)
		complex(kind=4),intent(inout)::val
		type(Tensor),intent(in)::T
		if(getTotalData(T).eq.1) then
			call assignment_com4_Tdata_value(val,T%TData)
		else
			call writemess("ERROR in assignment for Tensor to complex",-1)
			call error_stop()
		end if
		return
	end 	subroutine
	subroutine  assignmentNumTen_com8(val,T)
		complex(kind=8),intent(inout)::val
		type(Tensor),intent(in)::T
		if(getTotalData(T).eq.1) then
			call assignment_com8_Tdata_value(val,T%TData)
		else
			write(*,*)"ERROR in assignment for Tensor to complex"
			call error_stop()
		end if
		return
	end subroutine
	subroutine  assignmentNumTen_logi(val,T)
		logical,intent(inout)::val
		type(Tensor),intent(in)::T
		if(getTotalData(T).eq.1) then
			call assignment_logi_Tdata_value(val,T%TData)
		else
			write(*,*)"ERROR in assignment for Tensor to complex"
			call error_stop()
		end if
		return
	end subroutine
	subroutine  assignmentNumTen_char(val,T)
		character(len=*),intent(inout)::val
		type(Tensor),intent(in)::T
		if(getTotalData(T).eq.1) then
			call assignment_char_Tdata_value(val,T%TData)
		else
			write(*,*)"ERROR in assignment for Tensor to complex"
			call error_stop()
		end if
		return
	end subroutine
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                    assignment to array
!
!**************************************************************************************************************
!**************************************************************************************************************		
	subroutine copyTensor_int_dim1(Vec,T)
		integer,intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length
		length=getTotalData(T)
		if(size(Vec).lt.length) then
			write(*,*)"The array can not store the Data"
			write(*,*)"Length of the array",size(Vec)
			write(*,*)"Length of the Tensor",length
			call error_stop()
		end if
		call assignment_int_Tdata(vec,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_int_dim2(Mat,T)
		integer,intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		length=getTotalData(T)
		m=T.dim.1
		n=T.dim.2
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2)
			write(*,*)"size of the Tensor",m,n
			call error_stop()
		end if
		call assignment_int_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_int_dim3(Mat,T)
		integer,intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3)
			write(*,*)"size of the Tensor",m,n,l
			call error_stop()
		end if
		call assignment_int_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_int_dim4(Mat,T)
		integer,intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l).or.(size(Mat,4).ne.k)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3),size(Mat,4)
			write(*,*)"size of the Tensor",m,n,l,k
			call error_stop()
		end if
		call assignment_int_Tdata(Mat,T%TData,length)
		return
	end subroutine		
	
	

	subroutine copyTensor_real4_dim1(Vec,T)
		real(kind=4),intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length
		length=getTotalData(T)
		if(size(Vec).lt.length) then
			write(*,*)"The array can not store the Data"
			write(*,*)"Length of the array",size(Vec)
			write(*,*)"Length of the Tensor",length
			call error_stop()
		end if
		call assignment_real4_Tdata(vec,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_real4_dim2(Mat,T)
		real(kind=4),intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		length=getTotalData(T)
		m=T.dim.1
		n=T.dim.2
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2)
			write(*,*)"size of the Tensor",m,n
			call error_stop()
		end if
		call assignment_real4_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_real4_dim3(Mat,T)
		real(kind=4),intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3)
			write(*,*)"size of the Tensor",m,n,l
			call error_stop()
		end if
		call assignment_real4_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_real4_dim4(Mat,T)
		real(kind=4),intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l).or.(size(Mat,4).ne.k)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3),size(Mat,4)
			write(*,*)"size of the Tensor",m,n,l,k
			call error_stop()
		end if
		call assignment_real4_Tdata(Mat,T%TData,length)
		return
	end subroutine		
	
	
	
	
	subroutine copyTensor_real8_dim1(Vec,T)
		real(kind=8),intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length
		length=getTotalData(T)
		if(size(Vec).lt.length) then
			write(*,*)"The array can not store the Data"
			write(*,*)"Length of the array",size(Vec)
			write(*,*)"Length of the Tensor",length
			call error_stop()
		end if
		call assignment_real8_Tdata(vec,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_real8_dim2(Mat,T)
		real(kind=8),intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		length=getTotalData(T)
		m=T.dim.1
		n=T.dim.2
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2)
			write(*,*)"size of the Tensor",m,n
			call error_stop()
		end if
		call assignment_real8_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_real8_dim3(Mat,T)
		real(kind=8),intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3)
			write(*,*)"size of the Tensor",m,n,l
			call error_stop()
		end if
		call assignment_real8_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_real8_dim4(Mat,T)
		real(kind=8),intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l).or.(size(Mat,4).ne.k)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3),size(Mat,4)
			write(*,*)"size of the Tensor",m,n,l,k
			call error_stop()
		end if
		call assignment_real8_Tdata(Mat,T%TData,length)
		return
	end subroutine		
	
	
	
	subroutine copyTensor_com4_dim1(Vec,T)
		complex(kind=4),intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length
		length=getTotalData(T)
		if(size(Vec).lt.length) then
			write(*,*)"The array can not store the Data"
			write(*,*)"Length of the array",size(Vec)
			write(*,*)"Length of the Tensor",length
			call error_stop()
		end if
		call assignment_com4_Tdata(vec,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_com4_dim2(Mat,T)
		complex(kind=4),intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		length=getTotalData(T)
		m=T.dim.1
		n=T.dim.2
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2)
			write(*,*)"size of the Tensor",m,n
			call error_stop()
		end if
		call assignment_com4_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_com4_dim3(Mat,T)
		complex(kind=4),intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3)
			write(*,*)"size of the Tensor",m,n,l
			call error_stop()
		end if
		call assignment_com4_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_com4_dim4(Mat,T)
		complex(kind=4),intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l).or.(size(Mat,4).ne.k)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3),size(Mat,4)
			write(*,*)"size of the Tensor",m,n,l,k
			call error_stop()
		end if
		call assignment_com4_Tdata(Mat,T%TData,length)
		return
	end subroutine		
	
	
	
	
	subroutine copyTensor_com8_dim1(Vec,T)
		complex(kind=8),intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length
		length=getTotalData(T)
		if(size(Vec).lt.length) then
			write(*,*)"The array can not store the Data"
			write(*,*)"Length of the array",size(Vec)
			write(*,*)"Length of the Tensor",length
			call error_stop()
		end if
		call assignment_com8_Tdata(vec,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_com8_dim2(Mat,T)
		complex(kind=8),intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		length=getTotalData(T)
		m=T.dim.1
		n=T.dim.2
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2)
			write(*,*)"size of the Tensor",m,n
			call error_stop()
		end if
		call assignment_com8_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_com8_dim3(Mat,T)
		complex(kind=8),intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3)
			write(*,*)"size of the Tensor",m,n,l
			call error_stop()
		end if
		call assignment_com8_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_com8_dim4(Mat,T)
		complex(kind=8),intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l).or.(size(Mat,4).ne.k)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3),size(Mat,4)
			write(*,*)"size of the Tensor",m,n,l,k
			call error_stop()
		end if
		call assignment_com8_Tdata(Mat,T%TData,length)
		return
	end subroutine		
	
	
	subroutine copyTensor_logi_dim1(Vec,T)
		logical,intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length
		length=getTotalData(T)
		if(size(Vec).lt.length) then
			write(*,*)"The array can not store the Data"
			write(*,*)"Length of the array",size(Vec)
			write(*,*)"Length of the Tensor",length
			call error_stop()
		end if
		call assignment_logi_Tdata(vec,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_logi_dim2(Mat,T)
		logical,intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		length=getTotalData(T)
		m=T.dim.1
		n=T.dim.2
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2)
			write(*,*)"size of the Tensor",m,n
			call error_stop()
		end if
		call assignment_logi_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_logi_dim3(Mat,T)
		logical,intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3)
			write(*,*)"size of the Tensor",m,n,l
			call error_stop()
		end if
		call assignment_logi_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_logi_dim4(Mat,T)
		logical,intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l).or.(size(Mat,4).ne.k)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3),size(Mat,4)
			write(*,*)"size of the Tensor",m,n,l,k
			call error_stop()
		end if
		call assignment_logi_Tdata(Mat,T%TData,length)
		return
	end subroutine		
	
	
	subroutine copyTensor_char_dim1(Vec,T)
		character(len=*),intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length
		length=getTotalData(T)
		if(size(Vec).lt.length) then
			write(*,*)"The array can not store the Data"
			write(*,*)"Length of the array",size(Vec)
			write(*,*)"Length of the Tensor",length
			call error_stop()
		end if
		call assignment_char_Tdata(vec,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_char_dim2(Mat,T)
		character(len=*),intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		length=getTotalData(T)
		m=T.dim.1
		n=T.dim.2
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2)
			write(*,*)"size of the Tensor",m,n
			call error_stop()
		end if
		call assignment_char_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_char_dim3(Mat,T)
		character(len=*),intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3)
			write(*,*)"size of the Tensor",m,n,l
			call error_stop()
		end if
		call assignment_char_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_char_dim4(Mat,T)
		character(len=*),intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if((size(Mat,1).ne.m).or.(size(Mat,2).ne.n).or.(size(Mat,3).ne.l).or.(size(Mat,4).ne.k)) then
			write(*,*)"The array can not store the Data"
			write(*,*)"size of the array",size(Mat,1),size(Mat,2),size(Mat,3),size(Mat,4)
			write(*,*)"size of the Tensor",m,n,l,k
			call error_stop()
		end if
		call assignment_char_Tdata(Mat,T%TData,length)
		return
	end subroutine		
	
	
	
!                  store data      in Tensor, the input data can be any chape   
	subroutine store_T(T,Vec)
		class(Tensor),intent(inout) :: T
		class(Tensor),intent(in) ::Vec
		T%TData=Vec%TData
		return
	end subroutine	 
	subroutine store_int(T,Vec)
		class(Tensor),intent(inout) :: T
		integer,intent(in) ::Vec(*)
		integer::length
		length=getTotalData(T)
		call assignmentTData_int(T%TData,Vec,length)
		return
	end subroutine	
	
	subroutine storeSome_int(T,i1,i2,Vec)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::i1,i2
		integer,intent(in) ::Vec(*)
		integer::length
		length=i2-i1+1
		call assignmentSomeTData_int(T%TData,vec,length,i1,i2)
		return
	end subroutine	
	subroutine storeSome_Tensor(T,i1,i2,Vec)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::i1,i2
		class(Tensor),intent(in) ::Vec
		integer::length
		if(.not.Vec%getFlag())then
			call writemess("There is no data in input element, setValue(element)",-1)
			call error_stop()
		end if	
		length=i2-i1+1
		call assignmentSomeTData_T(T%TData,Vec%TData,length,i1,i2)
		return
	end subroutine	
	
	subroutine store_real4(T,Vec)
		class(Tensor),intent(inout) :: T
		real(kind=4),intent(in) ::Vec(*)
		integer::length
		length=getTotalData(T)
		call assignmentTData_real4(T%TData,Vec,length)
		return
	end subroutine	
	subroutine storeSome_real4(T,i1,i2,Vec)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::i1,i2
		real*4,intent(in) ::Vec(*)
		integer::length
		length=i2-i1+1
		call assignmentSomeTData_real4(T%TData,vec,length,i1,i2)
		return
	end subroutine	
	subroutine store_real8(T,Vec)
		class(Tensor),intent(inout) :: T
		real(kind=8),intent(in) ::Vec(*)
		integer::length
		length=getTotalData(T)
		call assignmentTData_real8(T%TData,Vec,length)
		return
	end subroutine	
	subroutine storeSome_real8(T,i1,i2,Vec)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::i1,i2
		real*8,intent(in) ::Vec(*)
		integer::length
		length=i2-i1+1
		call assignmentSomeTData_real8(T%TData,vec,length,i1,i2)
		return
	end subroutine	
	subroutine store_com4(T,Vec)
		class(Tensor),intent(inout) :: T
		complex(kind=4),intent(in) ::Vec(*)
		integer::length
		length=getTotalData(T)
		call assignmentTData_com4(T%TData,Vec,length)
		return
	end subroutine	
	subroutine storeSome_com4(T,i1,i2,Vec)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::i1,i2
		complex(kind=4),intent(in) ::Vec(*)
		integer::length
		length=i2-i1+1
		call assignmentSomeTData_com4(T%TData,vec,length,i1,i2)
		return
	end subroutine	
	subroutine store_com8(T,Vec)
		class(Tensor),intent(inout) :: T
		complex(kind=8),intent(in) ::Vec(*)
		integer::length
		length=getTotalData(T)
		call assignmentTData_com8(T%TData,Vec,length)
		return
	end subroutine	
	subroutine storeSome_com8(T,i1,i2,Vec)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::i1,i2
		complex(kind=8),intent(in) ::Vec(*)
		integer::length
		length=i2-i1+1
		call assignmentSomeTData_com8(T%TData,vec,length,i1,i2)
		return
	end subroutine	
	subroutine store_logi(T,Vec)
		class(Tensor),intent(inout) :: T
		logical,intent(in) ::Vec(*)
		integer::length
		length=getTotalData(T)
		call assignmentTData_logi(T%TData,Vec,length)
		return
	end subroutine	
	subroutine storeSome_logi(T,i1,i2,Vec)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::i1,i2
		logical,intent(in) ::Vec(*)
		integer::length
		length=i2-i1+1
		call assignmentSomeTData_logi(T%TData,vec,length,i1,i2)
		return
	end subroutine	
	subroutine store_char(T,Vec)
		class(Tensor),intent(inout) :: T
		character(len=*),intent(in) ::Vec(*)
		integer::length
		length=getTotalData(T)
		call assignmentTData_char(T%TData,Vec,length)
		return
	end subroutine	
	subroutine storeSome_char(T,i1,i2,Vec)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::i1,i2
		character(len=*),intent(in) ::Vec(*)
		integer::length
		length=i2-i1+1
		call assignmentSomeTData_char(T%TData,vec,length,i1,i2)
		return
	end subroutine	
	
	subroutine modify_chech_input(A,ia)
		class(Tensor)::A
		integer::ia(2)
		if(ia(1).gt.ia(2))then
			call writemess('ERROR in input parameter, when setvalue in Tensor.f90',-1)
			call writemess('ERROR:ia(1)>ia(2)'+',ia(1)='+ia(1)+',ia(2)='+ia(2),-1)
			call error_stop
		end if
		if(ia(1).le.0)then
			call writemess('ERROR in input parameter, when setvalue in Tensor.f90',-1)
			call writemess('ERROR:ia(1)<0'+',ia(1)='+ia(1))
			call error_stop
		end if
		if(ia(2).gt.A%getToTalData())then
			call writemess('ERROR in input parameter, when setvalue in Tensor.f90',-1)
			call writemess('ERROR:ia(2)>len of Tensor Data'+',ia(2)='+ia(2)+',TotalData='+A%getToTalData(),-1)
			call error_stop
		end if
		return
	end subroutine
	subroutine modify_chech_inputParameter(ia,lenA)
		integer::ia(2),lenA
		if(ia(1).gt.ia(2))then
			call writemess('ERROR in input parameter, when setvalue in Tensor.f90',-1)
			call writemess('ERROR:ia(1)>ia(2)'+',ia(1)='+ia(1)+',ia(2)='+ia(2),-1)
			call error_stop
		end if
		if(ia(1).le.0)then
			call writemess('ERROR in input parameter, when setvalue in Tensor.f90',-1)
			call writemess('ERROR:ia(1)<0'+',ia(1)='+ia(1),-1)
			call error_stop
		end if
		if(ia(2).gt.lenA)then
			call writemess('ERROR in input parameter, when setvalue in Tensor.f90',-1)
			call writemess('ERROR:ia(2)>len of lenA Data'+',ia(2)='+ia(2)+',TotalData='+lenA,-1)
			call error_stop
		end if
		return
	end subroutine
	subroutine modify_chech_inputindex(ia,ib)
		integer::ia(2),ib(2)
		if((ia(2)-ia(1)).ne.(ib(2)-ib(1)))then
			call writemess('ERROR in input parameter, when setvalue in Tensor.f90',-1)
			call writemess('ERROR: index do not match,ia(2)-ia(1)!=ib(2)-ib(1)',-1)
			call writemess('ia(1)='+ia(1),-1)
			call writemess('ia(2)='+ia(2),-1)
			call writemess('ib(1)='+ib(1),-1)
			call writemess('ib(2)='+ib(2),-1)
			call error_stop
		end if
		return
	end subroutine
	subroutine modify_chech_inputindex2(ia,ja,total)
		integer::ia(2),ja(2),total
		integer::m,n
		m=ia(2)-ia(1)+1
		n=ja(2)-ja(1)+1
		if((m*n).ne.total)then
			call writemess('ERROR in input parameter, when setvalue in Tensor.f90',-1)
			call writemess('ERROR: index do not match,(ia(2)-ia(1))*(ja(2)-ja(1))!=total',-1)
			call writemess('ia(1)='+ia(1),-1)
			call writemess('ia(2)='+ia(2),-1)
			call writemess('ja(1)='+ja(1),-1)
			call writemess('ja(2)='+ja(2),-1)
			call writemess('total='+total,-1)
			call error_stop
		end if
		return
	end subroutine
	subroutine modify_chech_inputindex1(ia,total)
		integer::ia(2),total
		integer::m
		m=ia(2)-ia(1)
		if(m.ne.total)then
			call writemess('ERROR in input parameter, when setvalue in Tensor.f90',-1)
			call writemess('ERROR: index do not match,(ia(2)-ia(1))!=total',-1)
			call writemess('ia(1)='+ia(1),-1)
			call writemess('ia(2)='+ia(2),-1)
			call writemess('total='+total,-1)
			call error_stop
		end if
		return
	end subroutine
	subroutine modify_Some_Data1_int(A,ia,B,ib)	
		class(Tensor),intent(inout) :: A
		integer,intent(in)::B(:)
		integer,intent(in)::ia(2),ib(2)
		integer::lenB
		call modify_chech_input(A,ia)
		lenB=size(B)
		call modify_chech_inputParameter(ib,lenB)
		call modify_chech_inputindex(ia,ib)
		call modify_Some_TData_class1(A%TData,A%GetTotalData(),ia,B,lenB,ib)
		return
	end subroutine
	subroutine modify_Some_Data1_real4(A,ia,B,ib)	
		class(Tensor),intent(inout) :: A
		real*4,intent(in)::B(:)
		integer,intent(in)::ia(2),ib(2)
		integer::lenB
		call modify_chech_input(A,ia)
		lenB=size(B)
		call modify_chech_inputParameter(ib,lenB)
		call modify_chech_inputindex(ia,ib)
		call modify_Some_TData_class1(A%TData,A%GetTotalData(),ia,B,lenB,ib)
		return
	end subroutine
	subroutine modify_Some_Data1_real8(A,ia,B,ib)	
		class(Tensor),intent(inout) :: A
		real*8,intent(in)::B(:)
		integer,intent(in)::ia(2),ib(2)
		integer::lenB
		call modify_chech_input(A,ia)
		lenB=size(B)
		call modify_chech_inputParameter(ib,lenB)
		call modify_chech_inputindex(ia,ib)
		call modify_Some_TData_class1(A%TData,A%GetTotalData(),ia,B,lenB,ib)
		return
	end subroutine
	subroutine modify_Some_Data1_com4(A,ia,B,ib)	
		class(Tensor),intent(inout) :: A
		complex*8,intent(in)::B(:)
		integer,intent(in)::ia(2),ib(2)
		integer::lenB
		call modify_chech_input(A,ia)
		lenB=size(B)
		call modify_chech_inputParameter(ib,lenB)
		call modify_chech_inputindex(ia,ib)
		call modify_Some_TData_class1(A%TData,A%GetTotalData(),ia,B,lenB,ib)
		return
	end subroutine
	subroutine modify_Some_Data1_com8(A,ia,B,ib)	
		class(Tensor),intent(inout) :: A
		complex*16,intent(in)::B(:)
		integer,intent(in)::ia(2),ib(2)
		integer::lenB
		call modify_chech_input(A,ia)
		lenB=size(B)
		call modify_chech_inputParameter(ib,lenB)
		call modify_chech_inputindex(ia,ib)
		call modify_Some_TData_class1(A%TData,A%GetTotalData(),ia,B,lenB,ib)
		return
	end subroutine
	subroutine modify_Some_Data1_logi(A,ia,B,ib)	
		class(Tensor),intent(inout) :: A
		logical,intent(in)::B(:)
		integer,intent(in)::ia(2),ib(2)
		integer::lenB
		call modify_chech_input(A,ia)
		lenB=size(B)
		call modify_chech_inputParameter(ib,lenB)
		call modify_chech_inputindex(ia,ib)
		call modify_Some_TData_class1(A%TData,A%GetTotalData(),ia,B,lenB,ib)
		return
	end subroutine
	subroutine modify_Some_Data1_char(A,ia,B,ib)	
		class(Tensor),intent(inout) :: A
		character(len=*),intent(in)::B(:)
		integer,intent(in)::ia(2),ib(2)
		integer::lenB
		call modify_chech_input(A,ia)
		lenB=size(B)
		call modify_chech_inputParameter(ib,lenB)
		call modify_chech_inputindex(ia,ib)
		call modify_Some_TData_class1(A%TData,A%GetTotalData(),ia,B,lenB,ib)
		return
	end subroutine
	subroutine modify_Some_Data1_Tensor(A,ia,B,ib)	
		class(Tensor),intent(inout) :: A
		type(Tensor),intent(in)::B
		integer,intent(in)::ia(2),ib(2)
		call modify_chech_input(A,ia)
		call modify_chech_input(B,ib)
		call modify_chech_inputindex(ia,ib)
		call modify_Some_TData_TData1(A%TData,A%GetTotalData(),ia,B%TData,A%GetTotalData(),ib)
		return
	end subroutine
	
	subroutine modify_Some_Data2_int(A,ia,ja,B,ib,jb)	
		class(Tensor),intent(inout) :: A
		integer,intent(in)::B(:,:)
		integer,intent(in)::ia(2),ja(2),ib(2),jb(2)
		integer::Bdim(2),Adim(2)
		if(A%getRank().ne.2)then
			call writemess('ERROR in SETTing value in Tensor.f90',-1)
			call writemess('The rank of the Tensor should be 2',-1)
			call error_stop
		end if
		Adim=A%dim()
		Bdim(1)=size(B,1)
		Bdim(2)=size(B,2)
		call modify_chech_inputParameter(ia,Adim(1))
		call modify_chech_inputParameter(ja,Adim(2))
		call modify_chech_inputParameter(ib,Bdim(1))
		call modify_chech_inputParameter(jb,Bdim(2))
		call modify_chech_inputindex(ia,ib)
		call modify_chech_inputindex(ja,jb)
		call modify_Some_TData_class2(A%TData,Adim,ia,ja,B,Bdim,ib,jb)
		return
	end subroutine
	subroutine modify_Some_Data2_real4(A,ia,ja,B,ib,jb)	
		class(Tensor),intent(inout) :: A
		real*4,intent(in)::B(:,:)
		integer,intent(in)::ia(2),ja(2),ib(2),jb(2)
		integer::Bdim(2),Adim(2)
		if(A%getRank().ne.2)then
			call writemess('ERROR in SETTing value in Tensor.f90',-1)
			call writemess('The rank of the Tensor should be 2',-1)
			call error_stop
		end if
		Adim=A%dim()
		Bdim(1)=size(B,1)
		Bdim(2)=size(B,2)
		call modify_chech_inputParameter(ia,Adim(1))
		call modify_chech_inputParameter(ja,Adim(2))
		call modify_chech_inputParameter(ib,Bdim(1))
		call modify_chech_inputParameter(jb,Bdim(2))
		call modify_chech_inputindex(ia,ib)
		call modify_chech_inputindex(ja,jb)
		call modify_Some_TData_class2(A%TData,Adim,ia,ja,B,Bdim,ib,jb)
		return
	end subroutine
	subroutine modify_Some_Data2_real8(A,ia,ja,B,ib,jb)	
		class(Tensor),intent(inout) :: A
		real*8,intent(in)::B(:,:)
		integer,intent(in)::ia(2),ja(2),ib(2),jb(2)
		integer::Bdim(2),Adim(2)
		if(A%getRank().ne.2)then
			call writemess('ERROR in SETTing value in Tensor.f90',-1)
			call writemess('The rank of the Tensor should be 2',-1)
			call error_stop
		end if
		Adim=A%dim()
		Bdim(1)=size(B,1)
		Bdim(2)=size(B,2)
		call modify_chech_inputParameter(ia,Adim(1))
		call modify_chech_inputParameter(ja,Adim(2))
		call modify_chech_inputParameter(ib,Bdim(1))
		call modify_chech_inputParameter(jb,Bdim(2))
		call modify_chech_inputindex(ia,ib)
		call modify_chech_inputindex(ja,jb)
		call modify_Some_TData_class2(A%TData,Adim,ia,ja,B,Bdim,ib,jb)
		return
	end subroutine
	subroutine modify_Some_Data2_com4(A,ia,ja,B,ib,jb)	
		class(Tensor),intent(inout) :: A
		complex*8,intent(in)::B(:,:)
		integer,intent(in)::ia(2),ja(2),ib(2),jb(2)
		integer::Bdim(2),Adim(2)
		if(A%getRank().ne.2)then
			call writemess('ERROR in SETTing value in Tensor.f90',-1)
			call writemess('The rank of the Tensor should be 2',-1)
			call error_stop
		end if
		Adim=A%dim()
		Bdim(1)=size(B,1)
		Bdim(2)=size(B,2)
		call modify_chech_inputParameter(ia,Adim(1))
		call modify_chech_inputParameter(ja,Adim(2))
		call modify_chech_inputParameter(ib,Bdim(1))
		call modify_chech_inputParameter(jb,Bdim(2))
		call modify_chech_inputindex(ia,ib)
		call modify_chech_inputindex(ja,jb)
		call modify_Some_TData_class2(A%TData,Adim,ia,ja,B,Bdim,ib,jb)
		return
	end subroutine
	subroutine modify_Some_Data2_com8(A,ia,ja,B,ib,jb)	
		class(Tensor),intent(inout) :: A
		complex*16,intent(in)::B(:,:)
		integer,intent(in)::ia(2),ja(2),ib(2),jb(2)
		integer::Bdim(2),Adim(2)
		if(A%getRank().ne.2)then
			call writemess('ERROR in SETTing value in Tensor.f90',-1)
			call writemess('The rank of the Tensor should be 2',-1)
			call error_stop
		end if
		Adim=A%dim()
		Bdim(1)=size(B,1)
		Bdim(2)=size(B,2)
		call modify_chech_inputParameter(ia,Adim(1))
		call modify_chech_inputParameter(ja,Adim(2))
		call modify_chech_inputParameter(ib,Bdim(1))
		call modify_chech_inputParameter(jb,Bdim(2))
		call modify_chech_inputindex(ia,ib)
		call modify_chech_inputindex(ja,jb)
		call modify_Some_TData_class2(A%TData,Adim,ia,ja,B,Bdim,ib,jb)
		return
	end subroutine
	subroutine modify_Some_Data2_logi(A,ia,ja,B,ib,jb)	
		class(Tensor),intent(inout) :: A
		logical,intent(in)::B(:,:)
		integer,intent(in)::ia(2),ja(2),ib(2),jb(2)
		integer::Bdim(2),Adim(2)
		if(A%getRank().ne.2)then
			call writemess('ERROR in SETTing value in Tensor.f90',-1)
			call writemess('The rank of the Tensor should be 2',-1)
			call error_stop
		end if
		Adim=A%dim()
		Bdim(1)=size(B,1)
		Bdim(2)=size(B,2)
		call modify_chech_inputParameter(ia,Adim(1))
		call modify_chech_inputParameter(ja,Adim(2))
		call modify_chech_inputParameter(ib,Bdim(1))
		call modify_chech_inputParameter(jb,Bdim(2))
		call modify_chech_inputindex(ia,ib)
		call modify_chech_inputindex(ja,jb)
		call modify_Some_TData_class2(A%TData,Adim,ia,ja,B,Bdim,ib,jb)
		return
	end subroutine
	subroutine modify_Some_Data2_char(A,ia,ja,B,ib,jb)	
		class(Tensor),intent(inout) :: A
		character(len=*),intent(in)::B(:,:)
		integer,intent(in)::ia(2),ja(2),ib(2),jb(2)
		integer::Bdim(2),Adim(2)
		if(A%getRank().ne.2)then
			call writemess('ERROR in SETTing value in Tensor.f90',-1)
			call writemess('The rank of the Tensor should be 2',-1)
			call error_stop
		end if
		Adim=A%dim()
		Bdim(1)=size(B,1)
		Bdim(2)=size(B,2)
		call modify_chech_inputParameter(ia,Adim(1))
		call modify_chech_inputParameter(ja,Adim(2))
		call modify_chech_inputParameter(ib,Bdim(1))
		call modify_chech_inputParameter(jb,Bdim(2))
		call modify_chech_inputindex(ia,ib)
		call modify_chech_inputindex(ja,jb)
		call modify_Some_TData_class2(A%TData,Adim,ia,ja,B,Bdim,ib,jb)
		return
	end subroutine
	subroutine modify_Some_Data2_Tensor(A,ia,ja,B,ib,jb)	
		class(Tensor),intent(inout) :: A
		Type(Tensor),intent(in)::B
		integer,intent(in)::ia(2),ja(2),ib(2),jb(2)
		integer::Bdim(2),Adim(2)
		if(A%getRank().ne.2)then
			call writemess('ERROR in SETTing value in Tensor.f90',-1)
			call writemess('The rank of the Tensor should be 2',-1)
			call error_stop
		end if
		if(B%getRank().ne.2)then
			call writemess('ERROR in SETTing value in Tensor.f90',-1)
			call writemess('The rank of the Tensor should be 2',-1)
			call error_stop
		end if
		Adim=A%dim()
		Bdim=B%dim()
		call modify_chech_inputParameter(ia,Adim(1))
		call modify_chech_inputParameter(ja,Adim(2))
		call modify_chech_inputParameter(ib,Bdim(1))
		call modify_chech_inputParameter(jb,Bdim(2))
		call modify_chech_inputindex(ia,ib)
		call modify_chech_inputindex(ja,jb)
		call modify_Some_TData_TData2(A%TData,Adim,ia,ja,B%TData,Bdim,ib,jb)
		return
	end subroutine
	
	subroutine modify_Some_Data2_Tensor2(A,ia,ja,B)	
		class(Tensor),intent(inout) :: A
		Type(Tensor),intent(in)::B
		integer,intent(in)::ia(2),ja(2)
		integer::Bdim(2),Adim(2),ib(2),jb(2),total
		if(A%getRank().ne.2)then
			call writemess('ERROR in SETTing value in Tensor.f90',-1)
			call writemess('The rank of the Tensor should be 2',-1)
			call error_stop
		end if
		Adim=A%dim()
		total=B%getTotalData()
		call modify_chech_inputParameter(ia,Adim(1))
		call modify_chech_inputParameter(ja,Adim(2))
		call modify_chech_inputindex2(ia,ja,total)
		Bdim(1)=ia(2)-ia(1)+1
		Bdim(2)=ja(2)-ja(1)+1
		ib(1)=1
		ib(2)=Bdim(1)
		jb(1)=1
		jb(2)=Bdim(2)
		call modify_Some_TData_TData2(A%TData,Adim,ia,ja,B%TData,Bdim,ib,jb)
		return
	end subroutine
	
!****************************************************************************8
!  copy to a allocatable array,
! allocate array and set value

	subroutine copyTensor_int_dim1_allocateble(Vec,T)
		integer,allocatable,intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length,lenvex
		length=getTotalData(T)
		if(allocated(Vec))then
			lenvex=size(Vec)
			if(lenvex.ne.length) then
				deallocate(Vec)
				allocate(Vec(length))
			end if
		else
			allocate(Vec(length))
		end if
		call assignment_int_Tdata(Vec,T%TData,length)
		return
	end subroutine
	
	!if rank=2,then copy the Tensor_Data to a mat
!Do not allocate the Tensor data
! mat is a matrix	
	subroutine copyTensor_int_dim2_allocateble(Mat,T)
		integer,allocatable,intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,lenMat1,lenMat2,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		length=getTotalData(T)
		m=T.dim.1
		n=T.dim.2
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			if((m.ne.lenMat1).or.(n.ne.lenMat2)) then
				deallocate(Mat)
				allocate(Mat(m,n))
			end if
		else
			allocate(Mat(m,n))
		end if
		call assignment_int_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_int_dim3_allocateble(Mat,T)
		integer,allocatable,intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,lenMat1,lenMat2,lenMat3,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l))
				end if
			end if
		else
			allocate(Mat(m,n,l))
		end if
		call assignment_int_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_int_dim4_allocateble(Mat,T)
		integer,allocatable,intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,lenMat1,lenMat2,lenMat3,lenMat4,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			lenMat4=size(Mat,4)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3).or.(k.ne.lenMat4)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l,k))
				end if
			end if
		else
			allocate(Mat(m,n,l,k))
		end if
		call assignment_int_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	
	
	subroutine copyTensor_real4_dim1_allocateble(Vec,T)
		real(kind=4),allocatable,intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length,lenvex
		length=getTotalData(T)
		if(allocated(Vec))then
			lenvex=size(Vec)
			if(lenvex.ne.length) then
				deallocate(Vec)
				allocate(Vec(length))
			end if
		else
			allocate(Vec(length))
		end if
		call assignment_real4_Tdata(Vec,T%TData,length)
		return
	end subroutine
!if rank=2,then copy the Tensor_Data to a mat
!Do not allocate the Tensor data
! mat is a matrix	
	subroutine copyTensor_real4_dim2_allocateble(Mat,T)
		real(kind=4),allocatable,intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,lenMat1,lenMat2,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			if((m.ne.lenMat1).or.(n.ne.lenMat2)) then
				deallocate(Mat)
				allocate(Mat(m,n))
			end if
		else
			allocate(Mat(m,n))
		end if
		call assignment_real4_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_real4_dim3_allocateble(Mat,T)
		real(kind=4),allocatable,intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,lenMat1,lenMat2,lenMat3,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l))
				end if
			end if
		else
			allocate(Mat(m,n,l))
		end if
		call assignment_real4_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_real4_dim4_allocateble(Mat,T)
		real(kind=4),allocatable,intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,lenMat1,lenMat2,lenMat3,lenMat4,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			lenMat4=size(Mat,4)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3).or.(k.ne.lenMat4)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l,k))
				end if
			end if
		else
			allocate(Mat(m,n,l,k))
		end if
		call assignment_real4_Tdata(Mat,T%TData,length)
		return
	end subroutine		
	
	


	subroutine copyTensor_real8_dim1_allocateble(Vec,T)
		real(kind=8),allocatable,intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length,lenvex
		length=getTotalData(T)
		if(allocated(Vec))then
			lenvex=size(Vec)
			if(lenvex.ne.length) then
				deallocate(Vec)
				allocate(Vec(length))
			end if
		else
			allocate(Vec(length))
		end if
		call assignment_real8_Tdata(Vec,T%TData,length)
		return
	end subroutine
!if rank=2,then copy the Tensor_Data to a mat
!Do not allocate the Tensor data
! mat is a matrix	
	subroutine copyTensor_real8_dim2_allocateble(Mat,T)
		real(kind=8),allocatable,intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,lenMat1,lenMat2,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			if((m.ne.lenMat1).or.(n.ne.lenMat2)) then
				deallocate(Mat)
				allocate(Mat(m,n))
			end if
		else
			allocate(Mat(m,n))
		end if
		call assignment_real8_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_real8_dim3_allocateble(Mat,T)
		real(kind=8),allocatable,intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,lenMat1,lenMat2,lenMat3,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l))
				end if
			end if
		else
			allocate(Mat(m,n,l))
		end if
		call assignment_real8_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_real8_dim4_allocateble(Mat,T)
		real(kind=8),allocatable,intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,lenMat1,lenMat2,lenMat3,lenMat4,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			lenMat4=size(Mat,4)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3).or.(k.ne.lenMat4)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l,k))
				end if
			end if
		else
			allocate(Mat(m,n,l,k))
		end if
		call assignment_real8_Tdata(Mat,T%TData,length)
		return
	end subroutine		







	subroutine copyTensor_com4_dim1_allocateble(Vec,T)
		complex(kind=4),allocatable,intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length,lenvex
		length=getTotalData(T)
		if(allocated(Vec))then
			lenvex=size(Vec)
			if(lenvex.ne.length) then
				deallocate(Vec)
				allocate(Vec(length))
			end if
		else
			allocate(Vec(length))
		end if
		call assignment_com4_Tdata(Vec,T%TData,length)
		return
	end subroutine
!if rank=2,then copy the Tensor_Data to a mat
!Do not allocate the Tensor data
! mat is a matrix	
	subroutine copyTensor_com4_dim2_allocateble(Mat,T)
		complex(kind=4),allocatable,intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,lenMat1,lenMat2,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			if((m.ne.lenMat1).or.(n.ne.lenMat2)) then
				deallocate(Mat)
				allocate(Mat(m,n))
			end if
		else
			allocate(Mat(m,n))
		end if
		call assignment_com4_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_com4_dim3_allocateble(Mat,T)
		complex(kind=4),allocatable,intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,lenMat1,lenMat2,lenMat3,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l))
				end if
			end if
		else
			allocate(Mat(m,n,l))
		end if
		call assignment_com4_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_com4_dim4_allocateble(Mat,T)
		complex(kind=4),allocatable,intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,lenMat1,lenMat2,lenMat3,lenMat4,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			lenMat4=size(Mat,4)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3).or.(k.ne.lenMat4)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l,k))
				end if
			end if
		else
			allocate(Mat(m,n,l,k))
		end if
		call assignment_com4_Tdata(Mat,T%TData,length)
		return
	end subroutine		
	
	


	subroutine copyTensor_com8_dim1_allocateble(Vec,T)
		complex(kind=8),allocatable,intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length,lenvex
		length=getTotalData(T)
		if(allocated(Vec))then
			lenvex=size(Vec)
			if(lenvex.ne.length) then
				deallocate(Vec)
				allocate(Vec(length))
			end if
		else
			allocate(Vec(length))
		end if
		call assignment_com8_Tdata(Vec,T%TData,length)
		return
	end subroutine
!if rank=2,then copy the Tensor_Data to a mat
!Do not allocate the Tensor data
! mat is a matrix	
	subroutine copyTensor_com8_dim2_allocateble(Mat,T)
		complex(kind=8),allocatable,intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,lenMat1,lenMat2,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			if((m.ne.lenMat1).or.(n.ne.lenMat2)) then
				deallocate(Mat)
				allocate(Mat(m,n))
			end if
		else
			allocate(Mat(m,n))
		end if
		call assignment_com8_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_com8_dim3_allocateble(Mat,T)
		complex(kind=8),allocatable,intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,lenMat1,lenMat2,lenMat3,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l))
				end if
			end if
		else
			allocate(Mat(m,n,l))
		end if
		call assignment_com8_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_com8_dim4_allocateble(Mat,T)
		complex(kind=8),allocatable,intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,lenMat1,lenMat2,lenMat3,lenMat4,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			lenMat4=size(Mat,4)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3).or.(k.ne.lenMat4)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l,k))
				end if
			end if
		else
			allocate(Mat(m,n,l,k))
		end if
		call assignment_com8_Tdata(Mat,T%TData,length)
		return
	end subroutine		
	
	subroutine copyTensor_logi_dim1_allocateble(Vec,T)
		logical,allocatable,intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length,lenvex
		length=getTotalData(T)
		if(allocated(Vec))then
			lenvex=size(Vec)
			if(lenvex.ne.length) then
				deallocate(Vec)
				allocate(Vec(length))
			end if
		else
			allocate(Vec(length))
		end if
		call assignment_logi_Tdata(Vec,T%TData,length)
		return
	end subroutine
!if rank=2,then copy the Tensor_Data to a mat
!Do not allocate the Tensor data
! mat is a matrix	
	subroutine copyTensor_logi_dim2_allocateble(Mat,T)
		logical,allocatable,intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,lenMat1,lenMat2,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			if((m.ne.lenMat1).or.(n.ne.lenMat2)) then
				deallocate(Mat)
				allocate(Mat(m,n))
			end if
		else
			allocate(Mat(m,n))
		end if
		call assignment_logi_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_logi_dim3_allocateble(Mat,T)
		logical,allocatable,intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,lenMat1,lenMat2,lenMat3,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l))
				end if
			end if
		else
			allocate(Mat(m,n,l))
		end if
		call assignment_logi_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_logi_dim4_allocateble(Mat,T)
		logical,allocatable,intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,lenMat1,lenMat2,lenMat3,lenMat4,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			lenMat4=size(Mat,4)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3).or.(k.ne.lenMat4)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l,k))
				end if
			end if
		else
			allocate(Mat(m,n,l,k))
		end if
		call assignment_logi_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	
	subroutine copyTensor_char_dim1_allocateble(Vec,T)
		character(len=max_len_of_char),allocatable,intent(inout) ::Vec(:)
		class(Tensor),intent(in) :: T
		integer::length,lenvex
		length=getTotalData(T)
		if(allocated(Vec))then
			lenvex=size(Vec)
			if(lenvex.ne.length) then
				deallocate(Vec)
				allocate(Vec(length))
			end if
		else
			allocate(Vec(length))
		end if
		call assignment_char_Tdata(Vec,T%TData,length)
		return
	end subroutine
!if rank=2,then copy the Tensor_Data to a mat
!Do not allocate the Tensor data
! mat is a matrix	
	subroutine copyTensor_char_dim2_allocateble(Mat,T)
		character(len=max_len_of_char),allocatable,intent(inout) ::Mat(:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,lenMat1,lenMat2,length
		if(getRank(T).ne.2)then
			write(*,*)"T is not a rank-2 Tensor"
			write(*,*)"Can not copy to a matrix"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			if((m.ne.lenMat1).or.(n.ne.lenMat2)) then
				deallocate(Mat)
				allocate(Mat(m,n))
			end if
		else
			allocate(Mat(m,n))
		end if
		call assignment_char_Tdata(Mat,T%TData,length)
		return
	end subroutine
	subroutine copyTensor_char_dim3_allocateble(Mat,T)
		character(len=max_len_of_char),allocatable,intent(inout) ::Mat(:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,lenMat1,lenMat2,lenMat3,length
		if(getRank(T).ne.3)then
			write(*,*)"T is not a rank-3 Tensor"
			write(*,*)"Can not copy to the array of dimension 3"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l))
				end if
			end if
		else
			allocate(Mat(m,n,l))
		end if
		call assignment_char_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	subroutine copyTensor_char_dim4_allocateble(Mat,T)
		character(len=max_len_of_char),allocatable,intent(inout) ::Mat(:,:,:,:)
		class(Tensor),intent(in) :: T
		integer::m,n,l,k,lenMat1,lenMat2,lenMat3,lenMat4,length
		if(getRank(T).ne.4)then
			write(*,*)"T is not a rank-4 Tensor"
			write(*,*)"Can not copy to the array of dimension 4"
			call error_stop()
		end if
		m=T.dim.1
		n=T.dim.2
		l=T.dim.3
		k=T.dim.4
		length=getTotalData(T)
		if(allocated(Mat))then
			lenMat1=size(Mat,1)
			lenMat2=size(Mat,2)
			lenMat3=size(Mat,3)
			lenMat4=size(Mat,4)
			if((m.ne.lenMat1).or.(n.ne.lenMat2).or.(l.ne.lenMat3).or.(k.ne.lenMat4)) then
				if(allocated(Mat))then
					deallocate(Mat)
					allocate(Mat(m,n,l,k))
				end if
			end if
		else
			allocate(Mat(m,n,l,k))
		end if
		call assignment_char_Tdata(Mat,T%TData,length)
		return
	end subroutine	
	
	
	
	
	!**************************************************************************************************************
!**************************************************************************************************************
!
!                                    Type Overloading  
!
!**************************************************************************************************************
!**************************************************************************************************************
	
!
	type(Tensor) function constructor_int_scal(val,dimen)result(Res)
		integer,intent(in)::val
		integer,optional,intent(in)::dimen(:)
		Res=val
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_int_dim1(vec,dimen)result(Res)
		integer,intent(in)::vec(:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_int_dim2(vec,dimen)result(Res)
		integer,intent(in)::vec(:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_int_dim3(vec,dimen)result(Res)
		integer,intent(in)::vec(:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_int_dim4(vec,dimen)result(Res)
		integer,intent(in)::vec(:,:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_real4_scal(val,dimen)result(Res)
		real(kind=4),intent(in)::val
		integer,optional,intent(in)::dimen(:)
		Res=val
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_real4_dim1(vec,dimen)result(Res)
		real(kind=4),intent(in)::vec(:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_real4_dim2(vec,dimen)result(Res)
		real(kind=4),intent(in)::vec(:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_real4_dim3(vec,dimen)result(Res)
		real(kind=4),intent(in)::vec(:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_real4_dim4(vec,dimen)result(Res)
		real(kind=4),intent(in)::vec(:,:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_real8_scal(val,dimen)result(Res)
		real(kind=8),intent(in)::val
		integer,optional,intent(in)::dimen(:)
		Res=val
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_real8_dim1(vec,dimen)result(Res)
		real(kind=8),intent(in)::vec(:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_real8_dim2(vec,dimen)result(Res)
		real(kind=8),intent(in)::vec(:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_real8_dim3(vec,dimen)result(Res)
		real(kind=8),intent(in)::vec(:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_real8_dim4(vec,dimen)result(Res)
		real(kind=8),intent(in)::vec(:,:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_com4_scal(val,dimen)result(Res)
		complex(kind=4),intent(in)::val
		integer,optional,intent(in)::dimen(:)
		Res=val
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_com4_dim1(vec,dimen)result(Res)
		complex(kind=4),intent(in)::vec(:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_com4_dim2(vec,dimen)result(Res)
		complex(kind=4),intent(in)::vec(:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_com4_dim3(vec,dimen)result(Res)
		complex(kind=4),intent(in)::vec(:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_com4_dim4(vec,dimen)result(Res)
		complex(kind=4),intent(in)::vec(:,:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_com8_scal(val,dimen)result(Res)
		complex(kind=8),intent(in)::val
		integer,optional,intent(in)::dimen(:)
		Res=val
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_com8_dim1(vec,dimen)result(Res)
		complex(kind=8),intent(in)::vec(:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_com8_dim2(vec,dimen)result(Res)
		complex(kind=8),intent(in)::vec(:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_com8_dim3(vec,dimen)result(Res)
		complex(kind=8),intent(in)::vec(:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_com8_dim4(vec,dimen)result(Res)
		complex(kind=8),intent(in)::vec(:,:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_logi_scal(val,dimen)result(Res)
		logical,intent(in)::val
		integer,optional,intent(in)::dimen(:)
		Res=val
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_logi_dim1(vec,dimen)result(Res)
		logical,intent(in)::vec(:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_logi_dim2(vec,dimen)result(Res)
		logical,intent(in)::vec(:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_logi_dim3(vec,dimen)result(Res)
		logical,intent(in)::vec(:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_logi_dim4(vec,dimen)result(Res)
		logical,intent(in)::vec(:,:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_char_scal0(val,dimen)result(Res)
		character(len=*),intent(in)::val
		integer,optional,intent(in)::dimen(:)
		Res=val
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	subroutine set_array_character_divider(cha)
		character(len=1),intent(in)::cha
		array_character_divider=cha
		return
	end subroutine
	function get_array_character_divider()
		character(len=1)::get_array_character_divider
		get_array_character_divider=array_character_divider
		return
	end function

	function constructor_char_scal(cha_,dimen)result(res)
		type(Tensor)::res
		character(len=*),intent(in)::cha_
		integer,optional,intent(in)::dimen(:)
		character(len=len(trim(adjustl(cha_))))::cha
		integer::TotalData,lenCha,i,j,ith,jth
		character(len=1)::w,divider,TensorType
		integer,pointer::ip(:)
		real*4,pointer::sp(:)
		real*8,pointer::dp(:)
		complex(kind=4),pointer::cp(:)
		complex(kind=8),pointer::zp(:)
		logical,pointer::lp(:)
		integer,allocatable::location(:)
		cha=trim(adjustl(cha_))
		lenCha=len(cha)
 		TotalData=1
 		divider=array_character_divider
 		TensorType=cha(1:1)
 		w=cha(2:2)
 		if(w.nequ.'=')then
 			Res=cha
			if(present(dimen))call Res%resetdim(dimen)
 			return
 		end if
 		do i=3,lenCha
 			w=cha(i:i)
 			if(w.equ.divider)TotalData=TotalData+1
 		end do
		allocate(location(TotalData+1))
		j=1
		location(1)=2
		do i=3,lenCha
			w=cha(i:i)
			if(w.equ.divider)then
				if(j.ge.(TotalData+1))then
					call writemess('ERROR in converting character for Tensor=character')
					call error_stop
				end if
				j=j+1
				location(j)=i
			end if
		end do
		if(j.ge.(TotalData+1))then
			call writemess('ERROR in converting character for Tensor=character')
			call error_stop
		end if
		j=j+1
		location(j)=lenCha+1
		select case(TensorType)
			case ('i')
				call Res%allocate([TotalData],'integer')
				call Res%pointer(ip)
				do i=1,TotalData
					ith=location(i)+1
					jth=location(i+1)-1
					read(cha(ith:jth),*)ip(i)
				end do
			case ('s')
				call Res%allocate([TotalData],'real*4')
				call Res%pointer(sp)
				do i=1,TotalData
					ith=location(i)+1
					jth=location(i+1)-1
					read(cha(ith:jth),*)sp(i)
				end do
			case ('d')
				call Res%allocate([TotalData],'real*8')
				call Res%pointer(dp)
				do i=1,TotalData
					ith=location(i)+1
					jth=location(i+1)-1
					read(cha(ith:jth),*)dp(i)
				end do
			case ('c')
				call writemess('DO NO finished this type, in Tensor=character')
				call error_stop
			case ('z')
				call writemess('DO NO finished this type, in Tensor=character')
				call error_stop
			case ('l')
				call Res%allocate([TotalData],'logical')
				call Res%pointer(lp)
				do i=1,TotalData
					ith=location(i)+1
					jth=location(i+1)-1
					read(cha(ith:jth),*)lp(i)
				end do
			case ('a')
				call Res%allocate([TotalData],'character')
				do i=1,TotalData
					ith=location(i)+1
					jth=location(i+1)-1
					call Res%setValue(i,cha(ith:jth))
				end do
			case default
				call writemess('ERROR in converting character for Tensor=character')
				call error_stop
		end select
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_char_dim1(vec,dimen)result(Res)
		character(len=*),intent(in)::vec(:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_char_dim2(vec,dimen)result(Res)
		character(len=*),intent(in)::vec(:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_char_dim3(vec,dimen)result(Res)
		character(len=*),intent(in)::vec(:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	type(Tensor) function constructor_char_dim4(vec,dimen)result(Res)
		character(len=*),intent(in)::vec(:,:,:,:)
		integer,optional,intent(in)::dimen(:)
		Res=Vec
		if(present(dimen))call Res%resetdim(dimen)
		return
	end function
	
	
	
	
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                    print Tensor    
!
!**************************************************************************************************************
!**************************************************************************************************************

	subroutine writemess_Tensor(mess,cpu_number)!overwrite writemess
		type(Tensor),intent(in)::mess
		integer,optional,intent(in)::cpu_number
		integer::i,j,k,totoal,rank
		integer,pointer::idata2(:,:),idata3(:,:,:),idata4(:,:,:,:)
		real*4,pointer::sdata2(:,:),sdata3(:,:,:),sdata4(:,:,:,:)
		real*8,pointer::ddata2(:,:),ddata3(:,:,:),ddata4(:,:,:,:)
		complex*8,pointer::cdata2(:,:),cdata3(:,:,:),cdata4(:,:,:,:)
		complex*16,pointer::zdata2(:,:),zdata3(:,:,:),zdata4(:,:,:,:)
		logical,pointer::ldata2(:,:),ldata3(:,:,:),ldata4(:,:,:,:)
		character(len=max_len_of_char)::w
		w=''
		if(.not.mess%getFlag())then
			call writemess('There is no data in the Tensor',cpu_number)
			return
		end if
		totoal=mess%getTotalData()
		if(totoal.eq.0)then
			call writemess('There is no data in the Tensor',cpu_number)
			return
		end if
		rank=mess%getRank()
		select case(mess%getType())
			case(1)
				select case(rank)
					case(1)
						call writemess(mess%ii(),cpu_number)
					case(2)
						call mess%pointer(idata2)
						do i=1,mess%dim(1)
							call writemess(idata2(i,:),cpu_number)
						end do
					case(3)
						call mess%pointer(idata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(idata3(i,:,j),cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(idata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(idata4(i,:,j,k),cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%ii(),cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(2)
				select case(rank)
					case(1)
						call writemess(mess%si(),cpu_number)
					case(2)
						call mess%pointer(sdata2)
						do i=1,mess%dim(1)
							call writemess(sdata2(i,:),cpu_number)
						end do
					case(3)
						call mess%pointer(sdata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(sdata3(i,:,j),cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(sdata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(sdata4(i,:,j,k),cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%si(),cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(3)
				select case(rank)
					case(1)
						call writemess(mess%di(),cpu_number)
					case(2)
						call mess%pointer(ddata2)
						do i=1,mess%dim(1)
							call writemess(ddata2(i,:),cpu_number)
						end do
					case(3)
						call mess%pointer(ddata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(ddata3(i,:,j),cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(ddata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(ddata4(i,:,j,k),cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%di(),cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(4)
				select case(rank)
					case(1)
						call writemess(mess%ci(),cpu_number)
					case(2)
						call mess%pointer(cdata2)
						do i=1,mess%dim(1)
							call writemess(cdata2(i,:),cpu_number)
						end do
					case(3)
						call mess%pointer(cdata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(cdata3(i,:,j),cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(cdata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(cdata4(i,:,j,k),cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%ci(),cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(5)
				select case(rank)
					case(1)
						call writemess(mess%zi(),cpu_number)
					case(2)
						call mess%pointer(zdata2)
						do i=1,mess%dim(1)
							call writemess(zdata2(i,:),cpu_number)
						end do
					case(3)
						call mess%pointer(zdata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(zdata3(i,:,j),cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(zdata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(zdata4(i,:,j,k),cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%zi(),cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(6)
				select case(rank)
					case(1)
						call writemess(mess%li(),cpu_number)
					case(2)
						call mess%pointer(ldata2)
						do i=1,mess%dim(1)
							call writemess(ldata2(i,:),cpu_number)
						end do
					case(3)
						call mess%pointer(ldata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(ldata3(i,:,j),cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(ldata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(ldata4(i,:,j,k),cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%li(),cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(7)
				do i=1,totoal-1
					w=w+mess%ai(i)+','
				end do
				w=w+mess%ai(totoal)
				call writemess(w,cpu_number)
		end select
		return
	end subroutine

	subroutine writemess_Tensor_form(mess,form,cpu_number)!overwrite writemess
		type(Tensor),intent(in)::mess
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		integer::i,j,k,totoal,rank
		integer,pointer::idata2(:,:),idata3(:,:,:),idata4(:,:,:,:)
		real*4,pointer::sdata2(:,:),sdata3(:,:,:),sdata4(:,:,:,:)
		real*8,pointer::ddata2(:,:),ddata3(:,:,:),ddata4(:,:,:,:)
		complex*8,pointer::cdata2(:,:),cdata3(:,:,:),cdata4(:,:,:,:)
		complex*16,pointer::zdata2(:,:),zdata3(:,:,:),zdata4(:,:,:,:)
		logical,pointer::ldata2(:,:),ldata3(:,:,:),ldata4(:,:,:,:)
		character(len=max_len_of_char)::w
		w=''
		if(.not.mess%getFlag())then
			call writemess('There is no data in the Tensor',cpu_number)
			return
		end if
		totoal=mess%getTotalData()
		if(totoal.eq.0)then
			call writemess('There is no data in the Tensor',cpu_number)
			return
		end if
		rank=mess%getRank()
		select case(mess%getType())
			case(1)
				select case(rank)
					case(1)
						call writemess(mess%ii(),form,cpu_number)
					case(2)
						call mess%pointer(idata2)
						do i=1,mess%dim(1)
							call writemess(idata2(i,:),form,cpu_number)
						end do
					case(3)
						call mess%pointer(idata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(idata3(i,:,j),form,cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(idata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(idata4(i,:,j,k),form,cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%ii(),form,cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(2)
				select case(rank)
					case(1)
						call writemess(mess%si(),form,cpu_number)
					case(2)
						call mess%pointer(sdata2)
						do i=1,mess%dim(1)
							call writemess(sdata2(i,:),form,cpu_number)
						end do
					case(3)
						call mess%pointer(sdata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(sdata3(i,:,j),form,cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(sdata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(sdata4(i,:,j,k),form,cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%si(),form,cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(3)
				select case(rank)
					case(1)
						call writemess(mess%di(),form,cpu_number)
					case(2)
						call mess%pointer(ddata2)
						do i=1,mess%dim(1)
							call writemess(ddata2(i,:),form,cpu_number)
						end do
					case(3)
						call mess%pointer(ddata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(ddata3(i,:,j),form,cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(ddata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(ddata4(i,:,j,k),form,cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%di(),form,cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(4)
				select case(rank)
					case(1)
						call writemess(mess%ci(),form,cpu_number)
					case(2)
						call mess%pointer(cdata2)
						do i=1,mess%dim(1)
							call writemess(cdata2(i,:),form,cpu_number)
						end do
					case(3)
						call mess%pointer(cdata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(cdata3(i,:,j),form,cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(cdata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(cdata4(i,:,j,k),form,cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%ci(),form,cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(5)
				select case(rank)
					case(1)
						call writemess(mess%zi(),form,cpu_number)
					case(2)
						call mess%pointer(zdata2)
						do i=1,mess%dim(1)
							call writemess(zdata2(i,:),form,cpu_number)
						end do
					case(3)
						call mess%pointer(zdata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(zdata3(i,:,j),form,cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(zdata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(zdata4(i,:,j,k),form,cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%zi(),form,cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(6)
				select case(rank)
					case(1)
						call writemess(mess%li(),cpu_number)
					case(2)
						call mess%pointer(ldata2)
						do i=1,mess%dim(1)
							call writemess(ldata2(i,:),cpu_number)
						end do
					case(3)
						call mess%pointer(ldata3)
						do j=1,mess%dim(3)
							call writemess('(*,*,'+j+')') 
							do i=1,mess%dim(2)
								call writemess(ldata3(i,:,j),cpu_number)
							end do
						end do
					case(4)
						call mess%pointer(ldata4)
						do k=1,mess%dim(4)
							do j=1,mess%dim(3)
								call writemess('(*,*,'+j+','+k+')') 
								do i=1,mess%dim(2)
									call writemess(ldata4(i,:,j,k),cpu_number)
								end do
							end do
						end do
					case default
						call writemess('The Tensor Data are:',cpu_number)
						call writemess(mess%li(),cpu_number)
						call writemess(mess%TenDim,cpu_number)
					end select
			case(7)
				do i=1,totoal-1
					w=w+mess%ai(i)+','
				end do
				w=w+mess%ai(totoal)
				call writemess(w,cpu_number)
		end select
		return
	end subroutine
		
	subroutine Tprint1(T)
		class(Tensor),intent(in) :: T
		CHARACTER(len=20)::classTypeChar
		write(*,*)"=================="
		write(*,*)"------------------"
		write(*,*)""
		if(getflag(T)) then
			classTypeChar=getclassType(T)
			if(ifDynamic(T))then
				write(*,*)'Dynamic class Tensor,data type is,',classTypeChar
			else
				write(*,*)'static class Tensor,data type is,',classTypeChar
			end if
			write(*,*) "The rank of the Tensor is"
			write(*,*) getRank(T)
			write(*,*) "The number of  data of the Tensor is"
			write(*,*) getTotalData(T)
			write(*,*) "The data of the Tensor is"
			call Tprintdata(T,0)
			call T%TenDim%print()
			write(*,*) "***end***"
			write(*,*) ""
		else
			write(*,*) "There is no data"
		end if
		return
	end subroutine
	subroutine Tprint2(T,words,printType)
		class(Tensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::words
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		write(*,*)"=================="
		write(*,*)"------------------"
		write(*,*)trim(words)
		if(getflag(T)) then
			classTypeChar=getclassType(T)
			if(ifDynamic(T))then
				write(*,*)'Dynamic class Tensor,data type is,',classTypeChar
			else
				write(*,*)'static class Tensor,data type is,',classTypeChar
			end if
			write(*,*) "The rank of the Tensor is"
			write(*,*) getRank(T)
			write(*,*) "The number of  data of the Tensor is"
			write(*,*) getTotalData(T)
			write(*,*) "The data of the Tensor is"
			call Tprintdata(T,0,printType)
			call T%TenDim%print()
			write(*,*) "***end***"
			write(*,*) ""
		else
			write(*,*) "There is no data"
		end if
		return
	end subroutine
	subroutine Tprint3(T,realpart,printType)
		class(Tensor),intent(in) :: T
		integer,intent(in)::realpart
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		write(*,*)"=================="
		write(*,*)"------------------"
		write(*,*)""
		if(getflag(T)) then
			classTypeChar=getclassType(T)
			if(ifDynamic(T))then
				write(*,*)'Dynamic class Tensor,data type is,',classTypeChar
			else
				write(*,*)'static class Tensor,data type is,',classTypeChar
			end if
			write(*,*) "The rank of the Tensor is"
			write(*,*) getRank(T)
			write(*,*) "The number of  data of the Tensor is"
			write(*,*) getTotalData(T)
			write(*,*) "The data of the Tensor is"
			call Tprintdata(T,realpart,printType)
			call T%TenDim%print()
			write(*,*) "***end***"
			write(*,*) ""
		else
			write(*,*) "There is no data"
		end if
		return
	end subroutine
	subroutine Tprint4(T,words,realpart,printType)
		class(Tensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::words
		CHARACTER(len=*),optional,intent(in)::printType
		integer,intent(in)::realpart
		CHARACTER(len=20)::classTypeChar
		write(*,*)"=================="
		write(*,*)"------------------"
		write(*,*)trim(words)
		if(getflag(T)) then
			classTypeChar=getclassType(T)
			if(ifDynamic(T))then
				write(*,*)'Dynamic class Tensor,data type is,',classTypeChar
			else
				write(*,*)'static class Tensor,data type is,',classTypeChar
			end if
			write(*,*) "The rank of the Tensor is"
			write(*,*) getRank(T)
			write(*,*) "The number of  data of the Tensor is"
			write(*,*) getTotalData(T)
			write(*,*) "The data of the Tensor is"
			call Tprintdata(T,realpart,printType)
			call T%TenDim%print()
			write(*,*) "***end***"
			write(*,*) ""
		else
			write(*,*) "There is no data"
		end if
		return
	end subroutine
	subroutine Tprint_file1(T,words,uni,printType)
		class(Tensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::words
		integer,intent(in)::uni
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		write(uni,*)"=================="
		write(uni,*)"readable data"
		write(uni,*)trim(words)
		if(getflag(T)) then
			classTypeChar=getclassType(T)
			if(ifDynamic(T))then
				write(uni,*)'Dynamic class Tensor,data type is:'
				write(uni,*)classTypeChar
			else
				write(uni,*)'Static class Tensor,data type is:'
				write(uni,*)classTypeChar
			end if
			write(uni,*) "The rank of the Tensor is"
			write(uni,*) getRank(T)
			write(uni,*) "The number of  data of the Tensor is"
			write(uni,*) getTotalData(T)
			if((T%getType().eq.4).or.(T%getType().eq.5))then
				write(uni,*) "The data of the Tensor is(real)"
				call Tprintdata_file(T,uni,1,printType)
				write(uni,*) "The data of the Tensor is(imag)"
				call Tprintdata_file(T,uni,2,printType)
			else
				write(uni,*) "The data of the Tensor is"
				call Tprintdata_file(T,uni,0,printType)
			end if
			call T%TenDim%info(uni)
			write(uni,*) "***END***"
			write(uni,*) ""
		else
			write(uni,*) "There is no data"
			write(uni,*)"END"
		end if
		return
	end subroutine
	subroutine Tprint_file2(T,uni,printType)
		class(Tensor),intent(in) :: T
		integer,intent(in)::uni
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		write(uni,*)"=================="
		write(uni,*)"readable data"
		write(uni,*)"-"
		if(getflag(T)) then
			classTypeChar=getclassType(T)
			if(ifDynamic(T))then
				write(uni,*)'Dynamic class Tensor,data type is:'
				write(uni,*)classTypeChar
			else
				write(uni,*)'Static class Tensor,data type is:'
				write(uni,*)classTypeChar
			end if
			write(uni,*) "The rank of the Tensor is"
			write(uni,*) getRank(T)
			write(uni,*) "The number of  data of the Tensor is"
			write(uni,*) getTotalData(T)
			if((T%getType().eq.4).or.(T%getType().eq.5))then
				write(uni,*) "The data of the Tensor is(real)"
				call Tprintdata_file(T,uni,1,printType)
				write(uni,*) "The data of the Tensor is(imag)"
				call Tprintdata_file(T,uni,2,printType)
			else
				write(uni,*) "The data of the Tensor is"
				call Tprintdata_file(T,uni,0,printType)
			end if
			call T%TenDim%info(uni)
			write(uni,*) "***END***"
			write(uni,*) ""
		else
			write(uni,*) "There is no data"
			write(uni,*)"END"
		end if
		return
	end subroutine
	
	subroutine Tread_file(T,uni)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::uni
		CHARACTER(len=20)::classTypeChar
		CHARACTER(len=50)::notused
		integer::rank,TotalData,i
		integer,allocatable::idata(:)
		real*4,allocatable::sdata(:),sdata2(:)
		real*8,allocatable::ddata(:),ddata2(:)
		character(len=max_len_of_char_in_TData),allocatable::adata(:)
		logical,allocatable::ldata(:)
		type(dimension)::dimen
		read(uni,*)notused
		read(uni,*)notused
		if(notused.ne.'readable')then
			call writemess("error in reading",-1)
			call error_stop()
		end if
		read(uni,*)notused
		read(uni,*)notused
		read(uni,*)classTypeChar
		call T%empty()
		if(classTypeChar.equ.'END')return
		call T%setType(classTypeChar)
		read(uni,*) notused
		read(uni,*) rank
		read(uni,*) notused
		read(uni,*) TotalData
		select case(T%getType())
			case(1)
				allocate(idata(TotalData))
				read(uni,*) notused
				read(uni,*)(idata(i),i=1,TotalData)
				T=idata
			case(2)
				allocate(sdata(TotalData))
				read(uni,*) notused
				read(uni,*)(sdata(i),i=1,TotalData)
				T=sdata
			case(3)
				allocate(ddata(TotalData))
				read(uni,*) notused
				read(uni,*)(ddata(i),i=1,TotalData)
				T=ddata
			case(4)
				allocate(sdata(TotalData))
				allocate(sdata2(TotalData))
				read(uni,*) notused
				read(uni,*)(sdata(i),i=1,TotalData)
				read(uni,*) notused
				read(uni,*)(sdata2(i),i=1,TotalData)
				T=cmplx(sdata,sdata2,kind=4)
			case(5)
				allocate(ddata(TotalData))
				allocate(ddata2(TotalData))
				read(uni,*) notused
				read(uni,*)(ddata(i),i=1,TotalData)
				read(uni,*) notused
				read(uni,*)(ddata2(i),i=1,TotalData)
				T=dcmplx(ddata,ddata2)
			case(6)
				allocate(ldata(TotalData))
				read(uni,*) notused
				read(uni,*)(ldata(i),i=1,TotalData)
				T=ldata
			case(7)
				allocate(adata(TotalData))
				read(uni,*) notused
				read(uni,*)(adata(i),i=1,TotalData)
				T=adata
		end select
		call dimen%read(uni)
		call T%resetDim(dimen)
		read(uni,*) notused
		return
	end subroutine
	
	subroutine Tread_data(T,uni)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::uni
		CHARACTER(len=50)::notused
		integer::rank,TotalData,i,dim1,dim2,j
		integer,pointer::idata(:,:),iidata(:)
		real*4,pointer::sdata(:,:),ssdata(:)
		real*8,pointer::ddata(:,:),dddata(:)
		character(len=max_len_of_char_in_TData),pointer::adata(:,:),aadata(:)
		logical,pointer::ldata(:,:),lldata(:)
		if(T%getRank().gt.2)then
			call writemess('ERROR in reading data, only allow for rank<=2 Tensor',-1)
			call error_stop
		end if
		if(T%getRank().eq.1)then
			TotalData=T%getTotalData()
			select case(T%getType())
				case(1)
					call T%pointer(iidata)
					read(uni,*)(iidata(i),i=1,TotalData)
					nullify(iidata)
				case(2)
					call T%pointer(ssdata)
					read(uni,*)(ssdata(i),i=1,TotalData)
					nullify(ssdata)
				case(3)
					call T%pointer(dddata)
					read(uni,*)(dddata(i),i=1,TotalData)
					nullify(dddata)
				case(4)
					call writemess('ERROR in reading data, Tensor',-1)
					call writemess('Do not finished for complex data yet',-1)
					call writemess('You can real two real data and combine them into a complex one',-1)
					call writemess('for example:A and B are real*4 Tensor.',-1)
					call writemess(' call A%readData(unit1).',-1)
					call writemess(' call B%readData(unit2).',-1)
					call writemess(' C=cmplex(A,B).',-1)
					call error_stop
				case(5)
					call writemess('ERROR in reading data, Tensor',-1)
					call writemess('Do not finished for complex data yet',-1)
					call writemess('You can real two real data and combine them into a complex one',-1)
					call writemess('for example:A and B are real*8 Tensor.',-1)
					call writemess(' call A%readData(unit1).',-1)
					call writemess(' call B%readData(unit2).',-1)
					call writemess(' C=dcmplex(A,B).',-1)
					call error_stop
				case(6)
					call T%pointer(lldata)
					read(uni,*)(lldata(i),i=1,TotalData)
					nullify(lldata)
				case(7)
					call T%pointer(aadata)
					read(uni,*)(aadata(i),i=1,TotalData)
					nullify(aadata)
			end select
			return
		endif
		dim1=T%dim(1)
		dim2=T%dim(2)
		TotalData=T%getTotalData()
		select case(T%getType())
			case(1)
				call T%pointer(idata)
				do i=1,dim1
					read(uni,*)(idata(i,j),j=1,dim2)
				end do
				nullify(idata)
			case(2)
				call T%pointer(sdata)
				do i=1,dim1
					read(uni,*)(sdata(i,j),j=1,dim2)
				end do
				nullify(sdata)
			case(3)
				call T%pointer(ddata)
				do i=1,dim1
					read(uni,*)(ddata(i,j),j=1,dim2)
				end do
				nullify(ddata)
			case(4)
				call writemess('ERROR in reading data, Tensor',-1)
				call writemess('Do not finished for complex data yet',-1)
				call writemess('You can real two real data and combine them into a complex one',-1)
				call writemess('for example:A and B are real*4 Tensor.',-1)
				call writemess(' call A%readData(unit1).',-1)
				call writemess(' call B%readData(unit2).',-1)
				call writemess(' C=cmplex(A,B).',-1)
				call error_stop
			case(5)
				call writemess('ERROR in reading data, Tensor',-1)
				call writemess('Do not finished for complex data yet',-1)
				call writemess('You can real two real data and combine them into a complex one',-1)
				call writemess('for example:A and B are real*8 Tensor.',-1)
				call writemess(' call A%readData(unit1).',-1)
				call writemess(' call B%readData(unit2).',-1)
				call writemess(' C=dcmplex(A,B).',-1)
				call error_stop
			case(6)
				call T%pointer(ldata)
					do i=1,dim1
						read(uni,*)(ldata(i,j),j=1,dim2)
					end do
					nullify(ldata)
			case(7)
				call T%pointer(adata)
				do i=1,dim1
					read(uni,*)(adata(i,j),j=1,dim2)
				end do
				nullify(adata)
		end select
		return
	end subroutine
	
	subroutine TMprint1(T)
		class(Tensor),intent(in) :: T
		CHARACTER(len=20)::classTypeChar
		integer,allocatable::dimen(:)
		write(*,*)"=================="
		write(*,*)"------------------"
		classTypeChar=getclassType(T)
		if(ifDynamic(T))then
			write(*,*)'Dynamic,',classTypeChar
		else
			write(*,*)'Static,',classTypeChar
		end if
		write(*,*) "*** START ***"
		if(.not.getFlag(T))then
			write(*,*)"There is no data"
			write(*,*) "*** END ***"
			return
		end if
		
		select case(T%rank)
			case(1)
				call Tprintdata(T,0)
				write(*,*) "*** END ***"
			case(2)
				allocate(dimen(2))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,0,T%rank,dimen)
				write(*,*) "*** END ***"
			case(3)
				allocate(dimen(3))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,0,T%rank,dimen)
				write(*,*) "*** END ***"
			case(4)
				allocate(dimen(4))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,0,T%rank,dimen)
				write(*,*) "*** END ***"
			case default
				write(*,*) "rank of the Tensor is large than 4"
				classTypeChar=getclassType(T)
				if(ifDynamic(T))then
					write(*,*)'Dynamic class Tensor,data type is,',classTypeChar
				else
					write(*,*)'static class Tensor,data type is,',classTypeChar
				end if
				write(*,*) "The data of the Tensor is"
				call Tprintdata(T,0)
				write(*,*) "***end***"
				write(*,*) "The dimension of the Tensor is"
				call T%TenDim%print()
				write(*,*) "The rank,total data are"
				write(*,*) getRank(T),getTotalData(T)
		end select
		return
	end subroutine
	
	subroutine TMprint2(T,words,printType)
		class(Tensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::words
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		integer,allocatable::dimen(:)
		write(*,*)"=================="
		write(*,*)"------------------"
		write(*,*)trim(words)
		classTypeChar=getclassType(T)
		if(ifDynamic(T))then
			write(*,*)'Dynamic,',classTypeChar
		else
			write(*,*)'Static,',classTypeChar
		end if
		write(*,*) "*** START ***"
		if(.not.getFlag(T))then
			write(*,*)"There is no data"
			write(*,*) "*** END ***"
			return
		end if
		
		select case(T%rank)
			case(1)
				call Tprintdata(T,0,printType)
				write(*,*) "*** END ***"
			case(2)
				allocate(dimen(2))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,0,T%rank,dimen,printType)
				write(*,*) "*** END ***"
			case(3)
				allocate(dimen(3))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,0,T%rank,dimen,printType)
				write(*,*) "*** END ***"
			case(4)
				allocate(dimen(4))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,0,T%rank,dimen,printType)
				write(*,*) "*** END ***"
			case default
				write(*,*) "rank of the Tensor is large than 4"
				classTypeChar=getclassType(T)
				if(ifDynamic(T))then
					write(*,*)'Dynamic class Tensor,data type is,',classTypeChar
				else
					write(*,*)'static class Tensor,data type is,',classTypeChar
				end if
				write(*,*) "The data of the Tensor is"
				call Tprintdata(T,0,printType)
				write(*,*) "***end***"
				write(*,*) "The dimension of the Tensor is"
				call T%TenDim%print()
				write(*,*) "The rank,total data are"
				write(*,*) getRank(T),getTotalData(T)
		end select
		return
	end subroutine
	subroutine TMprint3(T,realpart,printType)
		class(Tensor),intent(in) :: T
		integer,intent(in)::realpart
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		integer,allocatable::dimen(:)
		write(*,*)"=================="
		write(*,*)"------------------"
		classTypeChar=getclassType(T)
		if(ifDynamic(T))then
			write(*,*)'Dynamic,',classTypeChar
		else
			write(*,*)'Static,',classTypeChar
		end if
		write(*,*) "*** START ***"
		if(.not.getFlag(T))then
			write(*,*)"There is no data"
			write(*,*) "*** END ***"
			return
		end if
		
		select case(T%rank)
			case(1)
				call Tprintdata(T,realpart,printType)
				write(*,*) "*** END ***"
			case(2)
				allocate(dimen(2))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,realpart,T%rank,dimen,printType)
				write(*,*) "*** END ***"
			case(3)
				allocate(dimen(3))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,realpart,T%rank,dimen,printType)
				write(*,*) "*** END ***"
			case(4)
				allocate(dimen(4))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,realpart,T%rank,dimen,printType)
				write(*,*) "*** END ***"
			case default
				write(*,*) "rank of the Tensor is large than 4"
				classTypeChar=getclassType(T)
				if(ifDynamic(T))then
					write(*,*)'Dynamic class Tensor,data type is,',classTypeChar
				else
					write(*,*)'static class Tensor,data type is,',classTypeChar
				end if
				write(*,*) "The data of the Tensor is"
				call Tprintdata(T,realpart,printType)
				write(*,*) "***end***"
				write(*,*) "The dimension of the Tensor is"
				call T%TenDim%print()
				write(*,*) "The rank,total data are"
				write(*,*) getRank(T),getTotalData(T)
		end select
		return
	end subroutine
	subroutine TMprint4(T,words,realpart,printType)
		class(Tensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::words
		integer,intent(in)::realpart
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		integer,allocatable::dimen(:)
		write(*,*)"=================="
		write(*,*)"------------------"
		write(*,*)trim(words)
		classTypeChar=getclassType(T)
		if(ifDynamic(T))then
			write(*,*)'Dynamic,',classTypeChar
		else
			write(*,*)'Static,',classTypeChar
		end if
		write(*,*) "*** START ***"
		if(.not.getFlag(T))then
			write(*,*)"There is no data"
			write(*,*) "*** END ***"
			return
		end if
		
		select case(T%rank)
			case(1)
				call Tprintdata(T,realpart,printType)
				write(*,*) "*** END ***"
			case(2)
				allocate(dimen(2))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,realpart,T%rank,dimen,printType)
				write(*,*) "*** END ***"
			case(3)
				allocate(dimen(3))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,realpart,T%rank,dimen,printType)
				write(*,*) "*** END ***"
			case(4)
				allocate(dimen(4))
				dimen=.dim.T
				call Tprint_as_matrix(T%TData,realpart,T%rank,dimen,printType)
				write(*,*) "*** END ***"
			case default
				write(*,*) "rank of the Tensor is large than 4"
				classTypeChar=getclassType(T)
				if(ifDynamic(T))then
					write(*,*)'Dynamic class Tensor,data type is,',classTypeChar
				else
					write(*,*)'static class Tensor,data type is,',classTypeChar
				end if
				write(*,*) "The data of the Tensor is"
				call Tprintdata(T,realpart,printType)
				write(*,*) "***end***"
				write(*,*) "The dimension of the Tensor is"
				call T%TenDim%print()
				write(*,*) "The rank,total data are"
				write(*,*) getRank(T),getTotalData(T)
		end select
		return
	end subroutine
	
	subroutine TMprint_file1(T,words,uni,printType)
		class(Tensor),intent(in) :: T
		integer,intent(in)::uni
		CHARACTER(len=*),intent(in)::words
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		integer,allocatable::dimen(:)
		write(uni,*)"=================="
		write(uni,*)"------------------"
		write(uni,*)trim(words)
		classTypeChar=getclassType(T)
		if(ifDynamic(T))then
				write(uni,*)'Dynamic class Tensor,data type is,',classTypeChar
			else
				write(uni,*)'static class Tensor,data type is,',classTypeChar
			end if
		write(uni,*) "*** START ***"
		if(.not.getFlag(T))then
			write(uni,*)"There is no data"
			write(uni,*) "*** END ***"
			return
		end if
		
		select case(T%rank)
			case(1)
				call Tprintdata_file(T,uni,0,printType)
				write(uni,*) "*** END ***"
			case(2)
				allocate(dimen(2))
				dimen=.dim.T
				call Tprint_as_matrix_file(T%TData,uni,0,T%rank,dimen,printType)
				write(uni,*) "*** END ***"
			case(3)
				allocate(dimen(3))
				dimen=.dim.T
				call Tprint_as_matrix_file(T%TData,uni,0,T%rank,dimen,printType)
				write(uni,*) "*** END ***"
			case(4)
				allocate(dimen(4))
				dimen=.dim.T
				call Tprint_as_matrix_file(T%TData,uni,0,T%rank,dimen,printType)
				write(uni,*) "*** END ***"
			case default
				write(uni,*) "rank of the Tensor is large than 4"
				classTypeChar=getclassType(T)
				if(ifDynamic(T))then
					write(uni,*)'Dynamic class Tensor,data type is,',classTypeChar
				else
					write(uni,*)'static class Tensor,data type is,',classTypeChar
				end if
				write(uni,*) "The data of the Tensor is"
				call Tprintdata_file(T,uni,0,printType)
				write(uni,*) "***end***"
				write(uni,*) "The dimension of the Tensor is"
				call T%TenDim%print(uni)
				write(uni,*) "The rank,total data are"
				write(uni,*) getRank(T),getTotalData(T)
		end select
		return
	end subroutine
	subroutine TMprint_file2(T,uni,printType)
		class(Tensor),intent(in) :: T
		integer,intent(in)::uni
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		integer,allocatable::dimen(:)
		write(uni,*)"=================="
		write(uni,*)"------------------"
		classTypeChar=getclassType(T)
		if(ifDynamic(T))then
				write(uni,*)'Dynamic class Tensor,data type is,',classTypeChar
			else
				write(uni,*)'static class Tensor,data type is,',classTypeChar
			end if
		write(uni,*) "*** START ***"
		if(.not.getFlag(T))then
			write(uni,*)"There is no data"
			write(uni,*) "*** END ***"
			return
		end if
		
		select case(T%rank)
			case(1)
				call Tprintdata_file(T,uni,0,printType)
				write(uni,*) "*** END ***"
			case(2)
				allocate(dimen(2))
				dimen=.dim.T
				call Tprint_as_matrix_file(T%TData,uni,0,T%rank,dimen,printType)
				write(uni,*) "*** END ***"
			case(3)
				allocate(dimen(3))
				dimen=.dim.T
				call Tprint_as_matrix_file(T%TData,uni,0,T%rank,dimen,printType)
				write(uni,*) "*** END ***"
			case(4)
				allocate(dimen(4))
				dimen=.dim.T
				call Tprint_as_matrix_file(T%TData,uni,0,T%rank,dimen,printType)
				write(uni,*) "*** END ***"
			case default
				write(uni,*) "rank of the Tensor is large than 4"
				classTypeChar=getclassType(T)
				if(ifDynamic(T))then
					write(uni,*)'Dynamic class Tensor,data type is,',classTypeChar
				else
					write(uni,*)'static class Tensor,data type is,',classTypeChar
				end if
				write(uni,*) "The data of the Tensor is"
				call Tprintdata_file(T,uni,0,printType)
				write(uni,*) "***end***"
				write(uni,*) "The dimension of the Tensor is"
				call T%TenDim%print(uni)
				write(uni,*) "The rank,total data are"
				write(uni,*) getRank(T),getTotalData(T)
		end select
		return
	end subroutine
!********************* print dimension *********************    
	subroutine TDprint1(T,words,uni)
		class(Tensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::words
		integer,intent(in)::uni
		CHARACTER(len=20)::classTypeChar
		write(uni,*)"=================="
		write(uni,*)"------------------"
		write(uni,*)trim(words)
		if(getflag(T)) then!if1
			write(uni,*) "*** START ***"
			classTypeChar=getclassType(T)
			if(ifDynamic(T))then
				write(uni,*)'Dynamic class Tensor,data type is,',classTypeChar
			else
				write(uni,*)'static class Tensor,data type is,',classTypeChar
			end if
			write(uni,*) "The rank of the Tensor is"
			write(uni,*) T%rank
			write(uni,*) "The number of  data of the Tensor is"
			write(uni,*) getTotalData(T)
			call T%TenDim%print(uni)
			write(uni,*) "***end***"
			write(uni,*) ""
		else!if1
			write(uni,*) "There is no data"
		end if!if1
		return
	end subroutine
	subroutine TDprint2(T,words)
		class(Tensor),intent(in) :: T
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=20)::classTypeChar
		call writemess("==================",-1)
		call writemess("------------------",-1)
		call writemess(words)
		if(getflag(T)) then!if1
			call writemess("*** START ***",-1)
			classTypeChar=getclassType(T)
			if(ifDynamic(T))then
				call writemess('Dynamic class Tensor,data type is '+(' '+classTypeChar),-1)
			else
				call writemess('static class Tensor,data type is '+(' '+classTypeChar),-1)
			end if
			call writemess("The rank of the Tensor is",-1)
			call writemess(''+T%rank)
			call writemess("The number of  data of the Tensor is",-1)
			call writemess(''+getTotalData(T),-1)
			call T%TenDim%print()
			call writemess( "***end***",-1)
			call writemess("",-1)
		else!if1
			call writemess("There is no data",-1)
		end if!if1
		return
	end subroutine
	subroutine TDprint3(T,uni)
		class(Tensor),intent(in) :: T
		integer,intent(in)::uni
		CHARACTER(len=20)::classTypeChar
		write(uni,*)"=================="
		write(uni,*)"------------------"
		if(getflag(T)) then!if1
			write(uni,*) "*** START ***"
			classTypeChar=getclassType(T)
			if(ifDynamic(T))then
				write(uni,*)'Dynamic class Tensor,data type is,',classTypeChar
			else
				write(uni,*)'static class Tensor,data type is,',classTypeChar
			end if
			write(uni,*) "The rank of the Tensor is"
			write(uni,*) T%rank
			write(uni,*) "The number of  data of the Tensor is"
			write(uni,*) getTotalData(T)
			call T%TenDim%print(uni)
			write(uni,*) "***end***"
			write(uni,*) ""
		else!if1
			write(uni,*) "There is no data"
		end if!if1
		return
	end subroutine
	
	
	
	
	
	
	
	
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  get  dimension   data
!
!**************************************************************************************************************
!**************************************************************************************************************	
	integer function getTenDim_i(T,inde)
		class(Tensor),intent(in) :: T
		integer,intent(in) :: inde
		if(.not.T%getFlag())then
			call writemess('There is no data in the Tensor, when getting the ith dimension. ith='+inde,-1)
			call error_stop()
		end if
		getTenDim_i=T%TenDim.i.inde
		return
	end function
	integer function getTenDim_Namei(T,w)
		class(Tensor),intent(in) :: T
		character(len=*),intent(in)::w
		integer :: inde
		if(.not.T%getFlag())then
			call writemess('There is no data in the Tensor, when getting the dimension whose name is :'+w,-1)
			call error_stop()
		end if
		inde=T%TenDim%NameOrder(w)
		if(inde.eq.0)then
			getTenDim_Namei=0
			return
		end if
		getTenDim_Namei=T%TenDim.i.inde
		return
	end function
	type(Dimension) function getTenDim(T)
		class(Tensor),intent(in) :: T
		if(.not.T%getFlag())then
			call writemess('There is no data in the Tensor, when getting type(dimension)',-1)
			call error_stop()
		end if
		getTenDim=T%TenDim
		return
	end function
	function getAllTenDim(T)
		integer,allocatable::getAllTenDim(:)
		class(Tensor),intent(in) :: T
		if(.not.T%getFlag())then
			call writemess('There is no data in the Tensor, when getting dimension',-1)
			call error_stop()
		end if
		allocate(getAllTenDim(T%getRank()))
		getAllTenDim=T%TenDim
		return
	end function
	type(Dimension) function getTenSubDim(T,inde)
		class(Tensor),intent(in)::T
		integer,intent(in)::inde
		if(.not.T%getFlag())then
			call writemess('There is no data in the Tensor, when getting sub type(dimension)',-1)
			call error_stop()
		end if
		getTenSubDim=T%TenDim.sub.inde
		return
	end function
	type(Dimension) function getTenSubDim3(T,inde)
		class(Tensor),intent(in)::T
		integer,intent(in)::inde(2)
		if(.not.T%getFlag())then
			call writemess('There is no data in the Tensor, when getting sub type(dimension)',-1)
			call error_stop()
		end if
		getTenSubDim3=T%TenDim.sub.inde
		return
	end function
	type(Dimension) function getTenSubDim_name(T,w)
		class(Tensor),intent(in)::T
		CHARACTER(len=*),intent(in)::w
		integer::inde
		if(.not.T%getFlag())then
			call writemess('There is no data in the Tensor, when getting type(dimension)',-1)
			call error_stop()
		end if
		inde=TenNameorder(T,w)
		getTenSubDim_name=T%TenDim.sub.inde
		return
	end function	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                    TensorName    
!
!**************************************************************************************************************
!**************************************************************************************************************
	subroutine cleanTensorName(T)
		class(Tensor),intent(inout)::T
		call T%TenDim%cleanName()
		return
	end subroutine
	subroutine setTensorName2(T,TensorName)
		class(Tensor),intent(inout)::T
		CHARACTER(len=*),intent(in)::TensorName
		call T%TenDim%setName(TensorName)
		return
	end subroutine
	subroutine setTensorName3(T,ith,w)
		class(Tensor),intent(inout)::T
		integer,intent(in)::ith
		CHARACTER(len=*),intent(in)::w
		call T%TenDim%setName(ith,w)
		return
	end subroutine
	subroutine setTensorName4(T,oldw,neww)
		class(Tensor),intent(inout)::T
		CHARACTER(len=*),intent(in)::oldw,neww
		call T%TenDim%setName(oldw,neww)
		return
	end subroutine
	subroutine setTensorName5(T,TensorName)
		class(Tensor),intent(inout)::T
		integer,intent(in)::TensorName(:)
		call T%TenDim%setName(TensorName)
		return
	end subroutine
	subroutine setTensorName6(T,ith,TensorName)
		class(Tensor),intent(inout)::T
		integer,intent(in)::TensorName(:),ith
		call T%TenDim%setName(ith,TensorName)
		return
	end subroutine
	subroutine setTensorName7(T,ith,TensorName,DimenName)
		class(Tensor),intent(inout)::T
		integer,intent(in)::TensorName(:),DimenName(:),ith
		call T%TenDim%setName(ith,TensorName,DimenName)
		return
	end subroutine
	subroutine setTensorName8(T,oldTensorName,newTensorName)
		class(Tensor),intent(inout)::T
		integer,intent(in)::oldTensorName(:),newTensorName(:)
		call T%TenDim%setName(oldTensorName,newTensorName)
		return
	end subroutine
	subroutine setTensorName9(T,oldTensorName,oldDimenName,newTensorName,newDimenName)
		class(Tensor),intent(inout)::T
		integer,intent(in)::oldTensorName(:),newTensorName(:),oldDimenName(:),newDimenName(:)
		call T%TenDim%setName(oldTensorName,oldDimenName,newTensorName,newDimenName)
		return
	end subroutine
	
	CHARACTER(len=len_of_Name+len_of_Name) function outIndexName(T,ith)
		class(Tensor),intent(in)::T
		integer,intent(in)::ith
		outIndexName=T%TenDim%outName(ith)
		return
	end function
	CHARACTER(len=len_of_Name) function outDimName(T,ith)
		class(Tensor),intent(in)::T
		integer,intent(in)::ith
		outDimName=T%TenDim%outDimName(ith)
		return
	end function
	
	function outAllIndexName(T)
		CHARACTER(len=len_of_Name+len_of_Name),allocatable::outAllIndexName(:)
		class(Tensor),intent(in)::T
		integer::i
		allocate(outAllIndexName(T%rank))
		do i=1,T%rank
			outAllIndexName(i)=T%TenDim%outName(i)
		end do
		return
	end function
	CHARACTER(len=len_of_Name) function outTensorName1(T,ith)
		class(Tensor),intent(in)::T
		integer,intent(in)::ith
		outTensorName1=T%TenDim%outTensorName(ith)
		return
	end function
	CHARACTER(len=len_of_Name) function outTensorNamechar(T,w,typ)
		class(Tensor),intent(in)::T
		character(len=*),intent(in)::w
		character(len=*),intent(in),optional::typ
		outTensorNamechar=T%TenDim%outTensorName(w,typ)
		return
	end function
	CHARACTER(len=len_of_Name) function outTensorNamecharFullname(T,w)
		class(Tensor),intent(in)::T
		character(len=*),intent(in)::w
		outTensorNamecharFullname=T%TenDim%outTensorName(w)
		return
	end function
	
	function outTensorIntName(T,ith)
		integer,allocatable::outTensorIntName(:)
		class(Tensor),intent(in)::T
		integer,intent(in)::ith
		integer::TenNamelen
		TenNamelen=outTenNameLen()
		allocate(outTensorIntName(TenNamelen))
		outTensorIntName=T%TenDim%outTensorIntName(ith)
		return
	end function
	function outDimensionIntName(T,ith)
		integer,allocatable::outDimensionIntName(:)
		class(Tensor),intent(in)::T
		integer,intent(in)::ith
		integer::DimNamelen
		DimNamelen=outDimNameLen()
		allocate(outDimensionIntName(DimNamelen))
		outDimensionIntName=T%TenDim%outIntNameDim(ith)
		return
	end function
	function outAllTensorName(T)
		CHARACTER(len=len_of_Name),allocatable::outAllTensorName(:)
		class(Tensor),intent(in)::T
		integer::i
		allocate(outAllTensorName(T%rank))
		do i=1,T%rank
			outAllTensorName(i)=T%TenDim%outTensorName(i)
		end do
		return
	end function
	function outAllTensorIntName(T)
		integer,allocatable::outAllTensorIntName(:,:)
		class(Tensor),intent(in)::T
		integer::i,TenNamelen
		TenNamelen=outTenNameLen()
		allocate(outAllTensorIntName(T%rank,TenNamelen))
		do i=1,T%rank
			outAllTensorIntName(i,:)=T%TenDim%outTensorIntName(i)
		end do
		return
	end function
	function outAllDimensionintName(T)
		integer,allocatable::outAllDimensionintName(:,:)
		class(Tensor),intent(in)::T
		integer::i,DimNamelen
		DimNamelen=outDimNameLen()
		allocate(outAllDimensionintName(T%rank,DimNamelen))
		do i=1,T%rank
			outAllDimensionintName(i,:)=T%TenDim%outIntNameDim(i)
		end do
		return
	end function
	integer function TenNameorder1(T,w)
		class(Tensor),intent(in) :: T
		character(len=*),intent(in)::w
		integer :: inde
		TenNameorder1=T%TenDim%NameOrder(w)
		return
	end function
	integer function TenFindorder1(T,w)
		class(Tensor),intent(in) :: T
		character(len=*),intent(in)::w
		integer :: inde
		TenFindorder1=T%TenDim%FindOrder(w)
		return
	end function
	function TenNameorder2(T,w)
		integer,allocatable::TenNameorder2(:)
		class(Tensor),intent(in) :: T
		character(len=*),intent(in)::w(:)
		integer :: inde
		allocate(TenNameorder2(size(w)))
		TenNameorder2=T%TenDim%NameOrder(w)
		return
	end function
	function TenFindOrder2(T,w)
		integer,allocatable::TenFindOrder2(:)
		class(Tensor),intent(in) :: T
		character(len=*),intent(in)::w(:)
		integer :: inde
		allocate(TenFindOrder2(size(w)))
		TenFindOrder2=T%TenDim%FindOrder(w)
		return
	end function
	integer function TenNameorder3(T,TensorName,DimenName)
		class(Tensor),intent(in) :: T
		integer,intent(in)::TensorName(:),DimenName(:)
		integer :: inde
		TenNameorder3=T%TenDim%NameOrder(TensorName,DimenName)
		return
	end function
	integer function TenFindorder3(T,TensorName,DimenName)
		class(Tensor),intent(in) :: T
		integer,intent(in)::TensorName(:),DimenName(:)
		integer :: inde
		TenFindorder3=T%TenDim%FindOrder(TensorName,DimenName)
		return
	end function
	function TenNameorder4(T,TensorName,DimenName)
		class(Tensor),intent(in) :: T
		integer,allocatable::TenNameorder4(:)
		integer,intent(in)::TensorName(:,:),DimenName(:,:)
		allocate(TenNameorder4(size(TensorName,1)))		
		TenNameorder4=T%TenDim%NameOrder(TensorName,DimenName)
		return
	end function
	
	type(Tensor) function outAllName(T,w,typ)
		class(Tensor),intent(in)::T
		character(len=*),intent(in)::w
		character(len=*),intent(in),optional::typ
		logical::goon
		character(len=max_len_of_char_in_TData),allocatable::outchar(:)
		goon=T%TenDim%outSomedimensionName(outchar,w,typ)
		if(goon)then
			outAllName=outchar
		else
			call outAllName%empty()
		end if
		return
	end function

	logical function ifName(T,w,ch_)
		class(Tensor),intent(in) :: T
		character(len=*),intent(in)::w
		character(len=*),intent(in),optional::ch_
		ifName=T%TenDim%ifName(w,ch_)
		return
	end function
	
	
!input dimension [1,1,2,1,3,1,1,4,1]
!output dimenison [2,3,4]	
	subroutine RNTensorDim(T)
		class(Tensor),intent(inout)::T
		call T%TenDim%RNDim()
		T%rank=T%TenDim%getRank()
		return
	end subroutine	
	subroutine RNTensorDimint(T,notkillleg,killFlag)
		class(Tensor),intent(inout)::T
		integer,intent(in)::notkillleg
		character(len=*),intent(in),optional::killFlag
		call T%TenDim%killLeg(notkillleg,killFlag)
		T%rank=T%TenDim%getRank()
		return
	end subroutine	
	subroutine RNTensorDimChar(T,notkillleg,killFlag)
		class(Tensor),intent(inout)::T
		character(len=*),intent(in)::notkillleg
		character(len=*),intent(in),optional::killFlag
		call T%TenDim%killLeg(notkillleg,killFlag)
		T%rank=T%TenDim%getRank()
		return
	end subroutine	
	
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  element
!
!**************************************************************************************************************
!**************************************************************************************************************	
	integer function addressToIndes(T,Adim)
		class(Tensor),intent(in) :: T
		integer,intent(in) :: Adim(:)
		integer,allocatable::Tdim(:)
		integer :: i,Dimlen
		call copydimension(Tdim,T%TenDim)
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
	integer function iElement(T,Tdim)
		class(Tensor),intent(in) ::T
		integer,intent(in)::Tdim(:)
		integer::inde
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(T%rank.eq.1)then
			if(Tdim(1).gt.getTotalData(T))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_int(iElement,T%TData,Tdim,(/T.dim.1/),1)
			return
		end if
		if(T%rank.eq.2)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_int(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2/),2)
			return
		end if
		if(T%rank.eq.3)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt. (T.dim.2)).or. (Tdim(3).gt.(T.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_int(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3/),3)
			return
		end if
		if(T%rank.eq.4)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)).or.(Tdim(3).gt.(T.dim.3)).or.(Tdim(4).gt.(T.dim.4)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_int(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3,T.dim.4/),4)
			return
		end if
		inde=addressToIndes(T,Tdim)
		if(inde.gt.getTotalData(T))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
			call error_stop()
		end if
		call element_routine_int(iElement,T%TData,(/inde/),(/getTotalData(T)/),1)
		return
	end function		  
	real(kind=4) function sElement(T,Tdim)result(iElement)
		class(Tensor),intent(in) ::T
		integer,intent(in)::Tdim(:)
		integer::inde
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(T%rank.eq.1)then
			if(Tdim(1).gt.getTotalData(T))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_real4(iElement,T%TData,Tdim,(/T.dim.1/),1)
			return
		end if
		if(T%rank.eq.2)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_real4(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2/),2)
			return
		end if
		if(T%rank.eq.3)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt. (T.dim.2)).or. (Tdim(3).gt.(T.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_real4(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3/),3)
			return
		end if
		if(T%rank.eq.4)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)).or.(Tdim(3).gt.(T.dim.3)).or.(Tdim(4).gt.(T.dim.4)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_real4(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3,T.dim.4/),4)
			return
		end if
		inde=addressToIndes(T,Tdim)
		if(inde.gt.getTotalData(T))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
			call error_stop()
		end if
		call element_routine_real4(iElement,T%TData,(/inde/),(/getTotalData(T)/),1)
		return
	end function
	real(kind=8) function dElement(T,Tdim)result(iElement)
		class(Tensor),intent(in) ::T
		integer,intent(in)::Tdim(:)
		integer::inde
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(T%rank.eq.1)then
			if(Tdim(1).gt.getTotalData(T))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_real8(iElement,T%TData,Tdim,(/T.dim.1/),1)
			return
		end if
		if(T%rank.eq.2)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_real8(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2/),2)
			return
		end if
		if(T%rank.eq.3)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt. (T.dim.2)).or. (Tdim(3).gt.(T.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_real8(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3/),3)
			return
		end if
		if(T%rank.eq.4)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)).or.(Tdim(3).gt.(T.dim.3)).or.(Tdim(4).gt.(T.dim.4)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_real8(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3,T.dim.4/),4)
			return
		end if
		inde=addressToIndes(T,Tdim)
		if(inde.gt.getTotalData(T))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
			call error_stop()
		end if
		call element_routine_real8(iElement,T%TData,(/inde/),(/getTotalData(T)/),1)
		return
	end function		
	
	complex(kind=4) function cElement(T,Tdim)result(iElement)
		class(Tensor),intent(in) ::T
		integer,intent(in)::Tdim(:)
		integer::inde
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(T%rank.eq.1)then
			if(Tdim(1).gt.getTotalData(T))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_com4(iElement,T%TData,Tdim,(/T.dim.1/),1)
			return
		end if
		if(T%rank.eq.2)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_com4(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2/),2)
			return
		end if
		if(T%rank.eq.3)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt. (T.dim.2)).or. (Tdim(3).gt.(T.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_com4(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3/),3)
			return
		end if
		if(T%rank.eq.4)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)).or.(Tdim(3).gt.(T.dim.3)).or.(Tdim(4).gt.(T.dim.4)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_com4(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3,T.dim.4/),4)
			return
		end if
		inde=addressToIndes(T,Tdim)
		if(inde.gt.getTotalData(T))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
			call error_stop()
		end if
		call element_routine_com4(iElement,T%TData,(/inde/),(/getTotalData(T)/),1)
		return
	end function  
	complex(kind=8) function zElement(T,Tdim)result(iElement)
		class(Tensor),intent(in) ::T
		integer,intent(in)::Tdim(:)
		integer::inde
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(T%rank.eq.1)then
			if(Tdim(1).gt.getTotalData(T))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call writemess("you have input:"+Tdim(1))
				call error_stop()
			end if
			call element_routine_com8(iElement,T%TData,Tdim,(/T.dim.1/),1)
			return
		end if
		if(T%rank.eq.2)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call writemess("you have input:("+Tdim(1)+","+Tdim(2)+")")
				call writemess("dimension of T is input:("+(T.dim.1)+","+(T.dim.2)+")")
				call error_stop()
			end if
			call element_routine_com8(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2/),2)
			return
		end if
		if(T%rank.eq.3)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt. (T.dim.2)).or. (Tdim(3).gt.(T.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_com8(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3/),3)
			return
		end if
		if(T%rank.eq.4)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)).or.(Tdim(3).gt.(T.dim.3)).or.(Tdim(4).gt.(T.dim.4)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_com8(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3,T.dim.4/),4)
			return
		end if
		inde=addressToIndes(T,Tdim)
		if(inde.gt.getTotalData(T))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
			call error_stop()
		end if
		call element_routine_com8(iElement,T%TData,(/inde/),(/getTotalData(T)/),1)
		return
	end function  
	logical function lElement(T,Tdim)result(iElement)
		class(Tensor),intent(in) ::T
		integer,intent(in)::Tdim(:)
		integer::inde
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(T%rank.eq.1)then
			if(Tdim(1).gt.getTotalData(T))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_logi(iElement,T%TData,Tdim,(/T.dim.1/),1)
			return
		end if
		if(T%rank.eq.2)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_logi(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2/),2)
			return
		end if
		if(T%rank.eq.3)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt. (T.dim.2)).or. (Tdim(3).gt.(T.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_logi(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3/),3)
			return
		end if
		if(T%rank.eq.4)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)).or.(Tdim(3).gt.(T.dim.3)).or.(Tdim(4).gt.(T.dim.4)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_logi(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3,T.dim.4/),4)
			return
		end if
		inde=addressToIndes(T,Tdim)
		if(inde.gt.getTotalData(T))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
			call error_stop()
		end if
		call element_routine_logi(iElement,T%TData,(/inde/),(/getTotalData(T)/),1)
		return
	end function  
	
	character(len=max_len_of_char_in_TData) function aElement(T,Tdim)result(iElement)
		class(Tensor),intent(in) ::T
		integer,intent(in)::Tdim(:)
		integer::inde
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(T%rank.eq.1)then
			if(Tdim(1).gt.getTotalData(T))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_char(iElement,T%TData,Tdim,(/T.dim.1/),1)
			return
		end if
		if(T%rank.eq.2)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_char(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2/),2)
			return
		end if
		if(T%rank.eq.3)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt. (T.dim.2)).or. (Tdim(3).gt.(T.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_char(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3/),3)
			return
		end if
		if(T%rank.eq.4)then
			if( (Tdim(1).gt.(T.dim.1)) .or. (Tdim(2).gt.(T.dim.2)).or.(Tdim(3).gt.(T.dim.3)).or.(Tdim(4).gt.(T.dim.4)) )Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
				call error_stop()
			end if
			call element_routine_char(iElement,T%TData,Tdim,(/T.dim.1,T.dim.2,T.dim.3,T.dim.4/),4)
			return
		end if
		inde=addressToIndes(T,Tdim)
		if(inde.gt.getTotalData(T))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(T),-1)
			call error_stop()
		end if
		call element_routine_char(iElement,T%TData,(/inde/),(/getTotalData(T)/),1)
		return
	end function  
	type(Tensor) function TElement(T,Tdim)result(Element)
		class(Tensor),intent(in) ::T
		integer,intent(in)::Tdim(:)
		select case(T%getType())
			case (1)
				Element=T%ii(Tdim)
			case (2)
				Element=T%si(Tdim)
			case (3)
				Element=T%di(Tdim)	
			case (4)
				Element=T%ci(Tdim)
			case (5)
				Element=T%zi(Tdim)
			case (6)
				Element=T%li(Tdim)
			case (7)
				Element=T%ai(Tdim)
			case default
				call writemess("ERROR in TElement",-1)
				call error_stop()
		end select
		return
	end function
	type(Tensor) function TElement2(T,Tdim)result(Element)
		class(Tensor),intent(in) ::T
		integer,intent(in)::Tdim
		select case(T%getType())
			case (1)
				Element=T%ii(Tdim)
			case (2)
				Element=T%si(Tdim)
			case (3)
				Element=T%di(Tdim)	
			case (4)
				Element=T%ci(Tdim)
			case (5)
				Element=T%zi(Tdim)
			case (6)
				Element=T%li(Tdim)
			case (7)
				Element=T%ai(Tdim)
			case default
				call writemess("ERROR in TElement",-1)
				call error_stop()
		end select
		return
	end function
	integer function iElement2 (T,inde)result(Element)
		class(Tensor),intent(in) ::T
		integer,intent(in)::inde
		integer::length
		length=getTotalData(T)
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(inde.gt.length)then
			write(*,*)"index is larger then the length of the Tensor,(.i.)"
			call error_stop()
		end if
		call element_routine_int(Element,T%TData,(/inde/),(/length/),1)
		return
	end function
	real(kind=4) function sElement2 (T,inde)result(Element)
		class(Tensor),intent(in) ::T
		integer,intent(in)::inde
		integer::length
		length=getTotalData(T)
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(inde.gt.length)then
			write(*,*)"index is larger then the length of the Tensor,(.i.)"
			call error_stop()
		end if
		call element_routine_real4(Element,T%TData,(/inde/),(/length/),1)
		return
	end function
	real(kind=8) function dElement2 (T,inde)result(Element)
		class(Tensor),intent(in) ::T
		integer,intent(in)::inde
		integer::length
		length=getTotalData(T)
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(inde.gt.length)then
			write(*,*)"index is larger then the length of the Tensor,(.i.)"
			call error_stop()
		end if
		call element_routine_real8(Element,T%TData,(/inde/),(/length/),1)
		return
	end function
	complex(kind=4) function cElement2 (T,inde)result(Element)
		class(Tensor),intent(in) ::T
		integer,intent(in)::inde
		integer::length
		length=getTotalData(T)
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(inde.gt.length)then
			write(*,*)"index is larger then the length of the Tensor,(.i.)"
			call error_stop()
		end if
		call element_routine_com4(Element,T%TData,(/inde/),(/length/),1)
		return
	end function
	complex(kind=8) function zElement2 (T,inde)result(Element)
		class(Tensor),intent(in) ::T
		integer,intent(in)::inde
		integer::length
		length=getTotalData(T)
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(inde.gt.length)then
			write(*,*)"index is larger then the length of the Tensor,(.i.)"
			call error_stop()
		end if
		call element_routine_com8(Element,T%TData,(/inde/),(/length/),1)
		return
	end function
	logical function lElement2 (T,inde)result(Element)
		class(Tensor),intent(in) ::T
		integer,intent(in)::inde
		integer::length
		length=getTotalData(T)
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(inde.gt.length)then
			write(*,*)"index is larger then the length of the Tensor,(.i.)"
			call error_stop()
		end if
		call element_routine_logi(Element,T%TData,(/inde/),(/length/),1)
		return
	end function
	character(len=max_len_of_char_in_TData) function aElement2 (T,inde)result(Element)
		class(Tensor),intent(in) ::T
		integer,intent(in)::inde
		integer::length
		length=getTotalData(T)
		if(.not.getflag(T))then
			write(*,*)"There is no data in the Tensor,(.i.)"
			call error_stop()
		end if
		if(inde.gt.length)then
			write(*,*)"index is larger then the length of the Tensor,(.i.)"
			call error_stop()
		end if
		call element_routine_char(Element,T%TData,(/inde/),(/length/),1)
		return
	end function
	function ielementAll(T)result(Res)
		integer,allocatable::Res(:)
		class(Tensor),intent(in) ::T
		allocate(Res(T%getTotalData()))
		Res=T
		return
	end function
	function selementAll(T)result(Res)
		real*4,allocatable::Res(:)
		class(Tensor),intent(in) ::T
		allocate(Res(T%getTotalData()))
		Res=T
		return
	end function
	function delementAll(T)result(Res)
		real*8,allocatable::Res(:)
		class(Tensor),intent(in) ::T
		allocate(Res(T%getTotalData()))
		Res=T
		return
	end function
	function celementAll(T)result(Res)
		complex*8,allocatable::Res(:)
		class(Tensor),intent(in) ::T
		allocate(Res(T%getTotalData()))
		Res=T
		return
	end function
	function zelementAll(T)result(Res)
		complex*16,allocatable::Res(:)
		class(Tensor),intent(in) ::T
		allocate(Res(T%getTotalData()))
		Res=T
		return
	end function
	function lelementAll(T)result(Res)
		logical,allocatable::Res(:)
		class(Tensor),intent(in) ::T
		allocate(Res(T%getTotalData()))
		Res=T
		return
	end function
	function aelementAll(T)result(Res)
		character(len=max_len_of_char_in_TData),allocatable::Res(:)
		class(Tensor),intent(in) ::T
		allocate(Res(T%getTotalData()))
		Res=T
		return
	end function
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  generate data
!
!**************************************************************************************************************
!**************************************************************************************************************	
!!*********************  generate *********************
!		if T is allocated
	subroutine set_random_Tensor(T,region)
		class(Tensor),intent(inout)::T
		real*8,optional,intent(in)::region(*)
		if(.not.getflag(T)) then
			return
		end if
		call generate_random_data(T%TData,region)
		return
	end subroutine
	subroutine set_random_Tensor_region4(T,region)
		class(Tensor),intent(inout)::T
		real*4,intent(in)::region(*)
		if(.not.getflag(T)) then
			return
		end if
		call generate_random_data_region4(T%TData,region)
		return
	end subroutine
	subroutine set_random_Tensor_regioni(T,region)
		class(Tensor),intent(inout)::T
		integer,intent(in)::region(*)
		if(.not.getflag(T)) then
			return
		end if
		call generate_random_data_regioni(T%TData,region)
		return
	end subroutine
!*********************  generate *********************
!		generate a Tensor with random number
	type(Tensor) function generate_noName1(Tdim,region,classtype) result (T)
		integer,intent(in) :: Tdim(:)
		real*8,intent(in)::region(*)
		character(len=*),intent(in)::classtype
		call allocatedTensor(T,Tdim,classtype)
		call generate_random_data(T%TData,region)
		return
	end function	
	type(Tensor) function generate_noName1_region4(Tdim,region,classtype) result (T)
		integer,intent(in) :: Tdim(:)
		real*4,intent(in)::region(*)
		character(len=*),intent(in)::classtype
		call allocatedTensor(T,Tdim,classtype)
		call generate_random_data_region4(T%TData,region)
		return
	end function	
	type(Tensor) function generate_noName1_regioni(Tdim,region,classtype) result (T)
		integer,intent(in) :: Tdim(:)
		integer,intent(in)::region(*)
		character(len=*),intent(in)::classtype
		call allocatedTensor(T,Tdim,classtype)
		call generate_random_data_regioni(T%TData,region)
		return
	end function	
	type(Tensor) function generate_noName2(Tdim,region) result (T)
		integer,intent(in) :: Tdim(:)
		real*8,intent(in)::region(*)
		call allocatedTensor(T,Tdim)
		call generate_random_data(T%TData,region)
		return
	end function	
	type(Tensor) function generate_noName2_region4(Tdim,region) result (T)
		integer,intent(in) :: Tdim(:)
		real*4,intent(in)::region(*)
		call allocatedTensor(T,Tdim)
		call generate_random_data_region4(T%TData,region)
		return
	end function	
	type(Tensor) function generate_noName2_regioni(Tdim,region) result (T)
		integer,intent(in) :: Tdim(:)
		integer,intent(in)::region(*)
		call allocatedTensor(T,Tdim)
		call generate_random_data_regioni(T%TData,region)
		return
	end function	
	type(Tensor) function generate_noName3(Tdim,classtype) result (T)
		integer,intent(in) :: Tdim(:)
		character(len=*),intent(in)::classtype
		type(Tensor)::te
		call allocatedTensor(T,Tdim,classtype)
		call generate_random_data(T%TData)
		return
	end function
	type(Tensor) function generate_noName4(Tdim) result (T)
		integer,intent(in) :: Tdim(:)
		call allocatedTensor(T,Tdim)
		call generate_random_data(T%TData)
		return
	end function
	type(Tensor) function generate_dim1(Tdim,region,classtype) result (T)
		type(Dimension),intent(in) :: Tdim
		character(len=*),intent(in)::classtype
		real*8,intent(in)::region(*)
		call allocatedTensor(T,Tdim,classtype)
		call generate_random_data(T%TData,region)
		return
	end function
	type(Tensor) function generate_dim1_region4(Tdim,region,classtype) result (T)
		type(Dimension),intent(in) :: Tdim
		character(len=*),intent(in)::classtype
		real*4,intent(in)::region(*)
		call allocatedTensor(T,Tdim,classtype)
		call generate_random_data_region4(T%TData,region)
		return
	end function
	type(Tensor) function generate_dim1_regioni(Tdim,region,classtype) result (T)
		type(Dimension),intent(in) :: Tdim
		character(len=*),intent(in)::classtype
		integer,intent(in)::region(*)
		call allocatedTensor(T,Tdim,classtype)
		call generate_random_data_regioni(T%TData,region)
		return
	end function
	type(Tensor) function generate_dim2_region4(Tdim,region) result (T)
		type(Dimension),intent(in) :: Tdim
		real*4,intent(in)::region(*)
		call allocatedTensor(T,Tdim)
		call generate_random_data_region4(T%TData,region)
		return
	end function
	type(Tensor) function generate_dim2_regioni(Tdim,region) result (T)
		type(Dimension),intent(in) :: Tdim
		integer,intent(in)::region(*)
		call allocatedTensor(T,Tdim)
		call generate_random_data_regioni(T%TData,region)
		return
	end function
	type(Tensor) function generate_dim2(Tdim,region) result (T)
		type(Dimension),intent(in) :: Tdim
		real*8,intent(in)::region(*)
		call allocatedTensor(T,Tdim)
		call generate_random_data(T%TData,region)
		return
	end function
	type(Tensor) function generate_dim3(Tdim,classtype) result (T)
		type(Dimension),intent(in) :: Tdim
		character(len=*),intent(in)::classtype
		call allocatedTensor(T,Tdim,classtype)
		call generate_random_data(T%TData)
		return
	end function
	type(Tensor) function generate_dim4(Tdim) result (T)
		type(Dimension),intent(in) :: Tdim
		call allocatedTensor(T,Tdim)
		call generate_random_data(T%TData)
		return
	end function

!*********************  zeroTen *********************
!		generate a Tensor with all element is 0d0
	subroutine set_value_Tensor_class(T,value)
		class(Tensor),intent(inout)::T
		class(*),intent(in)::value
		if(.not.T%getflag()) then
			return
		end if
		select type(value)
			type is (integer)
				call set_all_data_int(T%TData,value)
			type is (real(kind=4))
				call set_all_data_real4(T%TData,value)
			type is (real(kind=8))
				call set_all_data_real8(T%TData,value)
			type is (complex(kind=4))
				call set_all_data_com4(T%TData,value)
			type is (complex(kind=8))
				call set_all_data_com8(T%TData,value)
			type is (logical)
				call set_all_data_logi(T%TData,value)
		end select
		return
	end subroutine
	subroutine set_zero_Tensor(T)
		class(Tensor),intent(inout)::T
		if(.not.T%getflag()) then
			return
		end if
		call set_all_data_int(T%TData,0)
		return
	end subroutine
	subroutine set_value_Tensor_int(T,value)
		class(Tensor),intent(inout)::T
		integer,intent(in)::value
		if(.not.T%getflag()) then
			return
		end if
		call set_all_data_int(T%TData,value)
		return
	end subroutine
	subroutine set_value_Tensor_real4(T,value)
		class(Tensor),intent(inout)::T
		real(kind=4),intent(in)::value
		if(.not.T%getflag()) then
			return
		end if
		call set_all_data_real4(T%TData,value)
		return
	end subroutine
	subroutine set_value_Tensor_real8(T,value)
		class(Tensor),intent(inout)::T
		real(kind=8),intent(in)::value
		if(.not.T%getflag()) then
			return
		end if
		call set_all_data_real8(T%TData,value)
		return
	end subroutine
	subroutine set_value_Tensor_com4(T,value)
		class(Tensor),intent(inout)::T
		complex(kind=4),intent(in)::value
		if(.not.T%getflag()) then
			return
		end if
		call set_all_data_com4(T%TData,value)
		return
	end subroutine
	subroutine set_value_Tensor_com8(T,value)
		class(Tensor),intent(inout)::T
		complex(kind=8),intent(in)::value
		if(.not.T%getflag()) then
			return
		end if
		call set_all_data_com8(T%TData,value)
		return
	end subroutine
	subroutine set_value_Tensor_logi(T,value)
		class(Tensor),intent(inout)::T
		logical,intent(in)::value
		if(.not.T%getflag()) then
			return
		end if
		call set_all_data_logi(T%TData,value)
		return
	end subroutine
	subroutine set_value_Tensor_char(T,value)
		class(Tensor),intent(inout)::T
		character(len=*),intent(in)::value
		if(.not.T%getflag()) then
			return
		end if
		call set_all_data_char(T%TData,value)
		return
	end subroutine
!*********************  zeroTen *********************
!		generate a Tensor with all element is 0d0
	type(Tensor) function zeroTen1(Tdim,classtype) result (T)
		integer,intent(in) :: Tdim(:)
		integer,optional,intent(in)::classtype
		if(present(classtype))then
			call allocatedTensor(T,Tdim,classtype)
		else
			call allocatedTensor(T,Tdim)
		end if
		call set_all_data_int(T%TData,0)
		return
	end function
!		generate a Tensor with all element is 0d0
	type(Tensor) function zeroTen2(Tdim,classtype) result (T)
		type(Dimension),intent(in) :: Tdim
		integer,optional,intent(in)::classtype
		if(present(classtype))then
			call allocatedTensor(T,Tdim,classtype)
		else
			call allocatedTensor(T,Tdim)
		end if
		call set_all_data_int(T%TData,0)
		return
	end function
	type(Tensor) function zeroTen3(Tdim,classtype) result (T)
		integer,intent(in) :: Tdim(:)
		character(len=*),intent(in)::classtype
			call allocatedTensor(T,Tdim,classtype)
		call set_all_data_int(T%TData,0)
		return
	end function
!		generate a Tensor with all element is 0d0
	type(Tensor) function zeroTen4(Tdim,classtype) result (T)
		type(Dimension),intent(in) :: Tdim
		character(len=*),intent(in)::classtype
		call allocateTensor(T,Tdim,classtype)
		call set_all_data_int(T%TData,0)
		return
	end function

	type(Tensor) function eye_int(s,dim1,dim2)
		integer,intent(in) :: s(:)
		integer,optional,intent(in) :: dim1,dim2
		integer :: i,m,n,lens
		lens=size(s)
		if(present(dim1))then
			m=dim1
			n=dim2
		else
			m=lens
			n=lens
		end if
		call allocateTensor(eye_int,(/m,n/),1)
		call eye_int%zero()
		do i=1,min(m,n,lens)
			call eye_int%setValue((/i,i/),s(i))
		end do
		return
	end function
	type(Tensor) function eye_real4(s,dim1,dim2)result(eye)
		real(kind=4),intent(in) :: s(:)
		integer,optional,intent(in) :: dim1,dim2
		integer :: i,m,n,lens
		lens=size(s)
		if(present(dim1))then
			m=dim1
			n=dim2
		else
			m=lens
			n=lens
		end if
		call allocateTensor(eye,(/m,n/),2)
		call eye%zero()
		do i=1,min(m,n,lens)
			call eye%setValue((/i,i/),s(i))
		end do
		return
	end function
	type(Tensor) function eye_real8(s,dim1,dim2)result(eye)
		real(kind=8),intent(in) :: s(:)
		integer,optional,intent(in) :: dim1,dim2
		integer :: i,m,n,lens
		lens=size(s)
		if(present(dim1))then
			m=dim1
			n=dim2
		else
			m=lens
			n=lens
		end if
		call allocateTensor(eye,(/m,n/),3)
		call eye%zero()
		do i=1,min(m,n,lens)
			call eye%setValue((/i,i/),s(i))
		end do
		return
	end function
	type(Tensor) function eye_com4(s,dim1,dim2)result(eye)
		complex(kind=4),intent(in) :: s(:)
		integer,optional,intent(in) :: dim1,dim2
		integer :: i,m,n,lens
		lens=size(s)
		if(present(dim1))then
			m=dim1
			n=dim2
		else
			m=lens
			n=lens
		end if
		call allocateTensor(eye,(/m,n/),4)
		call eye%zero()
		do i=1,min(m,n,lens)
			call eye%setValue((/i,i/),s(i))
		end do
		return
	end function
	type(Tensor) function eye_com8(s,dim1,dim2)result(eye)
		complex(kind=8),intent(in) :: s(:)
		integer,optional,intent(in) :: dim1,dim2
		integer :: i,m,n,lens
		lens=size(s)
		if(present(dim1))then
			m=dim1
			n=dim2
		else
			m=lens
			n=lens
		end if
		call allocateTensor(eye,(/m,n/),5)
		call eye%zero()
		do i=1,min(m,n,lens)
			call eye%setValue((/i,i/),s(i))
		end do
		return
	end function
	type(Tensor) function eye_logi(s,dim1,dim2)result(eye)
		logical,intent(in) :: s(:)
		integer,optional,intent(in) :: dim1,dim2
		integer :: i,m,n,lens
		lens=size(s)
		if(present(dim1))then
			m=dim1
			n=dim2
		else
			m=lens
			n=lens
		end if
		call allocateTensor(eye,(/m,n/),6)
		call eye%zero()
		do i=1,min(m,n,lens)
			call eye%setValue((/i,i/),s(i))
		end do
		return
	end function
	type(Tensor) function eye_char(s,dim1,dim2)result(eye)
		character(len=*),intent(in) :: s(:)
		integer,optional,intent(in) :: dim1,dim2
		integer :: i,m,n,lens
		lens=size(s)
		if(present(dim1))then
			m=dim1
			n=dim2
		else
			m=lens
			n=lens
		end if
		call allocateTensor(eye,(/m,n/),7)
		call eye%zero()
		do i=1,min(m,n,lens)
			call eye%setValue((/i,i/),s(i))
		end do
		return
	end function
	type(Tensor) function eye_Tensor(s,dim1,dim2)result(eye)
		class(Tensor),intent(in) :: s
		integer,optional,intent(in) :: dim1,dim2
		integer :: i,m,n,lens
		lens=getTotalData(s)
		if(s%getRank().ne.1)then
			call writemess('ERROR in eye(T), input T should be a vector(rank=1)')
			call s%diminfo()
			call error_stop
		end if
		if(present(dim1))then
			m=dim1
			n=dim2
		else
			m=lens
			n=lens
		end if
		call allocateTensor(eye,(/m,n/),getType(s))
		call eye%zero()
		do i=1,min(m,n,lens)
			call eye%setValue((/i,i/),s%zi((/i/)))
		end do
		return
	end function
	subroutine eye_Tensor2(T)
		class(Tensor),intent(inout)::T
		integer::i,rank
		rank=T%getRank()
		if(rank.ne.2)then
			call writemess("ERROR in set eye in Tensor,input should be a matrix",-1)
			call error_stop()
		end if
		call T%zero()
		do i=1,min(T%dim(1),T%dim(2))
			call T%setvalue((/i,i/),1)
		end do
		return
	end subroutine
	subroutine eye_Tensor3(T,m,n,classtype)
		class(Tensor),intent(inout)::T
		integer,intent(in)::m,n
		character(len=*),intent(in)::classtype
		integer::i
		call T%empty()
		call T%allocate((/m,n/),classtype)
		call T%zero()
		do i=1,min(T%dim(1),T%dim(2))
			call T%setvalue((/i,i/),1)
		end do
		return
	end subroutine
	subroutine eye_Tensor4(T,m,n,classtype)
		class(Tensor),intent(inout)::T
		integer,intent(in)::m,n,classtype
		integer::i
		call T%empty()
		call T%allocate((/m,n/),classtype)
		call T%zero()
		do i=1,min(T%dim(1),T%dim(2))
			call T%setvalue((/i,i/),1)
		end do
		return
	end subroutine
	subroutine eye_Tensor5(T,dimen,classtype)
		class(Tensor),intent(inout)::T
		character(len=*),intent(in)::classtype
		integer,intent(in)::dimen(2)
		call T%empty()
		call T%allocate(dimen,classtype)
		call eye_Tensor2(T)
		return
	end subroutine
	subroutine eye_Tensor6(T,dimen,classtype)
		class(Tensor),intent(inout)::T
		character(len=*),intent(in)::classtype
		type(dimension),intent(in)::dimen
		call T%empty()
		call T%allocate(dimen,classtype)
		call eye_Tensor2(T)
		return
	end subroutine
	
	type(Tensor) function diag_int(s,m,n)result(diag)
		integer,intent(in) :: s
		integer,intent(in) :: m,n
		integer :: i
		call allocateTensor(diag,(/m,n/),1)
		call diag%zero()
		do i=1,min(m,n)
			call diag%setValue((/i,i/),s)
		end do
		return
	end function
	type(Tensor) function identity_matrix_Tensor(m,n)result(diag)
		integer,intent(in) :: m,n
		integer :: i
		call allocateTensor(diag,(/m,n/),1)
		call diag%zero()
		do i=1,min(m,n)
			call diag%setValue((/i,i/),1)
		end do
		return
	end function
	type(Tensor) function diag_real4(s,m,n)result(diag)
		real(kind=4),intent(in) :: s
		integer,intent(in) :: m,n
		integer :: i
		call allocateTensor(diag,(/m,n/),2)
		call diag%zero()
		do i=1,min(m,n)
			call diag%setValue((/i,i/),s)
		end do
		return
	end function
	type(Tensor) function diag_real8(s,m,n)result(diag)
		real(kind=8),intent(in) :: s
		integer,intent(in) :: m,n
		integer :: i
		call allocateTensor(diag,(/m,n/),3)
		call diag%zero()
		do i=1,min(m,n)
			call diag%setValue((/i,i/),s)
		end do
		return
	end function
	type(Tensor) function diag_com4(s,m,n)result(diag)
		complex(kind=4),intent(in) :: s
		integer,intent(in) :: m,n
		integer :: i
		call allocateTensor(diag,(/m,n/),4)
		call diag%zero()
		do i=1,min(m,n)
			call diag%setValue((/i,i/),s)
		end do
		return
	end function
	type(Tensor) function diag_com8(s,m,n)result(diag)
		complex(kind=8),intent(in) :: s
		integer,intent(in) :: m,n
		integer :: i
		call allocateTensor(diag,(/m,n/),5)
		call diag%zero()
		do i=1,min(m,n)
			call diag%setValue((/i,i/),s)
		end do
		return
	end function
	type(Tensor) function diag_logi(s,m,n)result(diag)
		logical,intent(in) :: s
		integer,intent(in) :: m,n
		integer :: i
		call allocateTensor(diag,(/m,n/),6)
		call diag%zero()
		do i=1,min(m,n)
			call diag%setValue((/i,i/),s)
		end do
		return
	end function
	type(Tensor) function diag_char(s,m,n)result(diag)
		character(len=*),intent(in) :: s
		integer,intent(in) :: m,n
		integer :: i
		call allocateTensor(diag,(/m,n/),7)
		call diag%zero()
		do i=1,min(m,n)
			call diag%setValue((/i,i/),s)
		end do
		return
	end function
	type(Tensor) function diag_classtype(m,n,classtype)result(diag)
		integer,intent(in) :: m,n,classtype
		integer :: i
		call allocateTensor(diag,(/m,n/),classtype)
		call diag%zero()
		do i=1,min(m,n)
			call diag%setValue((/i,i/),1)
		end do
		return
	end function
	type(Tensor) function diag_type(m,n,classtype_)result(diag)
		integer,intent(in) :: m,n
		character(len=*),intent(in)::classtype_
		integer :: i,classtype
		classtype=select_data_type_char(classtype_)
		call allocateTensor(diag,(/m,n/),classtype)
		call diag%zero()
		do i=1,min(m,n)
			call diag%setValue((/i,i/),1)
		end do
		return
	end function




!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  modify element in Tensor
!
!**************************************************************************************************************
!**************************************************************************************************************

	subroutine modifyTen_val_class_i(Ten,dimen,val)!in will go wrong for character
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		integer,intent(in)::val
		integer::addre
		integer,allocatable::LDdimen(:)
		if(Ten%rank.eq.1) then
			if(dimen(1).gt.getTotalData(Ten))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(1))
			LDdimen=Ten.dim.1
			call modify_TData_class(Ten,dimen,LDdimen,1,val)
			return
		end if
		if(Ten%rank.eq.2) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt.(Ten.dim.2)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(2))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,2,val)
			return
		end if
		if(Ten%rank.eq.3) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(3))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,3,val)
			return
		end if
		if(Ten%rank.eq.4) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3))&
				.or.(dimen(4).gt.(Ten.dim.4)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(4))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,4,val)
			return
		end if
		addre=addressToIndes(Ten,dimen)
		if(addre.gt.getTotalData(Ten))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
			call error_stop()
		end if
		call modify_TData_class(Ten,(/addre/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine

	subroutine modifyTen_val_class_s(Ten,dimen,val)!in will go wrong for character
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		real*4,intent(in)::val
		integer::addre
		integer,allocatable::LDdimen(:)
		if(Ten%rank.eq.1) then
			if(dimen(1).gt.getTotalData(Ten))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(1))
			LDdimen=Ten.dim.1
			call modify_TData_class(Ten,dimen,LDdimen,1,val)
			return
		end if
		if(Ten%rank.eq.2) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt.(Ten.dim.2)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(2))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,2,val)
			return
		end if
		if(Ten%rank.eq.3) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(3))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,3,val)
			return
		end if
		if(Ten%rank.eq.4) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3))&
				.or.(dimen(4).gt.(Ten.dim.4)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(4))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,4,val)
			return
		end if
		addre=addressToIndes(Ten,dimen)
		if(addre.gt.getTotalData(Ten))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
			call error_stop()
		end if
		call modify_TData_class(Ten,(/addre/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine

	subroutine modifyTen_val_class_d(Ten,dimen,val)!in will go wrong for character
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		real*8,intent(in)::val
		integer::addre
		integer,allocatable::LDdimen(:)
		if(Ten%rank.eq.1) then
			if(dimen(1).gt.getTotalData(Ten))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(1))
			LDdimen=Ten.dim.1
			call modify_TData_class(Ten,dimen,LDdimen,1,val)
			return
		end if
		if(Ten%rank.eq.2) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt.(Ten.dim.2)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(2))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,2,val)
			return
		end if
		if(Ten%rank.eq.3) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(3))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,3,val)
			return
		end if
		if(Ten%rank.eq.4) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3))&
				.or.(dimen(4).gt.(Ten.dim.4)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(4))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,4,val)
			return
		end if
		addre=addressToIndes(Ten,dimen)
		if(addre.gt.getTotalData(Ten))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
			call error_stop()
		end if
		call modify_TData_class(Ten,(/addre/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine

	subroutine modifyTen_val_class_c(Ten,dimen,val)!in will go wrong for character
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		complex(kind=4),intent(in)::val
		integer::addre
		integer,allocatable::LDdimen(:)
		if(Ten%rank.eq.1) then
			if(dimen(1).gt.getTotalData(Ten))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(1))
			LDdimen=Ten.dim.1
			call modify_TData_class(Ten,dimen,LDdimen,1,val)
			return
		end if
		if(Ten%rank.eq.2) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt.(Ten.dim.2)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(2))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,2,val)
			return
		end if
		if(Ten%rank.eq.3) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(3))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,3,val)
			return
		end if
		if(Ten%rank.eq.4) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3))&
				.or.(dimen(4).gt.(Ten.dim.4)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(4))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,4,val)
			return
		end if
		addre=addressToIndes(Ten,dimen)
		if(addre.gt.getTotalData(Ten))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
			call error_stop()
		end if
		call modify_TData_class(Ten,(/addre/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine

	subroutine modifyTen_val_class_z(Ten,dimen,val)!in will go wrong for character
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		complex(kind=8),intent(in)::val
		integer::addre
		integer,allocatable::LDdimen(:)
		if(Ten%rank.eq.1) then
			if(dimen(1).gt.getTotalData(Ten))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(1))
			LDdimen=Ten.dim.1
			call modify_TData_class(Ten,dimen,LDdimen,1,val)
			return
		end if
		if(Ten%rank.eq.2) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt.(Ten.dim.2)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(2))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,2,val)
			return
		end if
		if(Ten%rank.eq.3) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(3))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,3,val)
			return
		end if
		if(Ten%rank.eq.4) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3))&
				.or.(dimen(4).gt.(Ten.dim.4)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(4))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,4,val)
			return
		end if
		addre=addressToIndes(Ten,dimen)
		if(addre.gt.getTotalData(Ten))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
			call error_stop()
		end if
		call modify_TData_class(Ten,(/addre/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine

	subroutine modifyTen_val_class_l(Ten,dimen,val)!in will go wrong for character
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		logical,intent(in)::val
		integer::addre
		integer,allocatable::LDdimen(:)
		if(Ten%rank.eq.1) then
			if(dimen(1).gt.getTotalData(Ten))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(1))
			LDdimen=Ten.dim.1
			call modify_TData_class(Ten,dimen,LDdimen,1,val)
			return
		end if
		if(Ten%rank.eq.2) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt.(Ten.dim.2)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(2))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,2,val)
			return
		end if
		if(Ten%rank.eq.3) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(3))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,3,val)
			return
		end if
		if(Ten%rank.eq.4) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3))&
				.or.(dimen(4).gt.(Ten.dim.4)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(4))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,4,val)
			return
		end if
		addre=addressToIndes(Ten,dimen)
		if(addre.gt.getTotalData(Ten))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
			call error_stop()
		end if
		call modify_TData_class(Ten,(/addre/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine

	subroutine modifyTen_val_class_a(Ten,dimen,val)!in will go wrong for character
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		character(len=*),intent(in)::val
		integer::addre
		integer,allocatable::LDdimen(:)
		if(Ten%rank.eq.1) then
			if(dimen(1).gt.getTotalData(Ten))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(1))
			LDdimen=Ten.dim.1
			call modify_TData_class(Ten,dimen,LDdimen,1,val)
			return
		end if
		if(Ten%rank.eq.2) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt.(Ten.dim.2)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(2))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,2,val)
			return
		end if
		if(Ten%rank.eq.3) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(3))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,3,val)
			return
		end if
		if(Ten%rank.eq.4) then
			if( (dimen(1).gt.(Ten.dim.1)) .or. (dimen(2).gt. (Ten.dim.2)).or. (dimen(3).gt.(Ten.dim.3))&
				.or.(dimen(4).gt.(Ten.dim.4)))Then
				call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
				call error_stop()
			end if
			allocate(LDdimen(4))
			LDdimen=.subdim.Ten
			call modify_TData_class(Ten,dimen,LDdimen,4,val)
			return
		end if
		addre=addressToIndes(Ten,dimen)
		if(addre.gt.getTotalData(Ten))Then
			call writemess("Index is larger than the len of Tensor,totalData="+getTotalData(Ten),-1)
			call error_stop()
		end if
		call modify_TData_class(Ten,(/addre/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine



	subroutine modifyTen_val_Tensor(Ten,dimen,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		type(Tensor),intent(in)::val
		if(val%getTotalData().ne.1)then
			call writemess("Do no finished this case, in modifyTen_val_Tensor",-1)
			call error_stop()
		end if
		if(.not.val%getFlag())then
			call writemess("There is no data in input element, setValue(element)",-1)
			call error_stop()
		end if	
		select case(val%getType())
			case (1)
				call modifyTen_val_class_i(Ten,dimen,val%ii(1))
			case (2)
				call modifyTen_val_class_s(Ten,dimen,val%si(1))	
			case (3)
				call modifyTen_val_class_d(Ten,dimen,val%di(1))
			case (4)
				call modifyTen_val_class_c(Ten,dimen,val%ci(1))
			case (5)
				call modifyTen_val_class_z(Ten,dimen,val%zi(1))
			case (6)
				call modifyTen_val_class_l(Ten,dimen,val%li(1))
			case (7)
				call modifyTen_val_class_a(Ten,dimen,val%ai(1))
			case default
				call writemess("ERROR in modifyTen_val_Tensor",-1)
				call error_stop()
		end select
		return
	end subroutine
	subroutine modifyTen_val_int(Ten,dimen,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		integer,intent(in)::val
		call modifyTen_val_class(Ten,dimen,val)
		return
	end subroutine
	subroutine modifyTen_val_real4(Ten,dimen,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		real(kind=4),intent(in)::val
		call modifyTen_val_class(Ten,dimen,val)
		return
	end subroutine
	subroutine modifyTen_val_real8(Ten,dimen,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		real(kind=8),intent(in)::val
		call modifyTen_val_class(Ten,dimen,val)
		return
	end subroutine
	subroutine modifyTen_val_com4(Ten,dimen,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		complex(kind=4),intent(in)::val
		call modifyTen_val_class(Ten,dimen,val)
		return
	end subroutine
	subroutine modifyTen_val_com8(Ten,dimen,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		complex(kind=8),intent(in)::val
		call modifyTen_val_class(Ten,dimen,val)
		return
	end subroutine
	subroutine modifyTen_val_logi(Ten,dimen,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		logical,intent(in)::val
		call modifyTen_val_class(Ten,dimen,val)
		return
	end subroutine
	subroutine modifyTen_val_char(Ten,dimen,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		character(len=*),intent(in)::val
		call modifyTen_val_class(Ten,dimen,val)
		return
	end subroutine

	
	subroutine modifyTen_val1_int(Ten,ith,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::ith
		integer,intent(in)::val
		call modify_TData_class(Ten,(/ith/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine
	subroutine modifyTen_val1_real4(Ten,ith,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::ith
		real(kind=4),intent(in)::val
		call modify_TData_class(Ten,(/ith/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine
	subroutine modifyTen_val1_real8(Ten,ith,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::ith
		real(kind=8),intent(in)::val
		call modify_TData_class(Ten,(/ith/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine
	subroutine modifyTen_val1_com4(Ten,ith,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::ith
		complex(kind=4),intent(in)::val
		call modify_TData_class(Ten,(/ith/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine
	subroutine modifyTen_val1_com8(Ten,ith,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::ith
		complex(kind=8),intent(in)::val
		call modify_TData_class(Ten,(/ith/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine
	subroutine modifyTen_val1_logi(Ten,ith,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::ith
		logical,intent(in)::val
		call modify_TData_class(Ten,(/ith/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine

	subroutine modifyTen_val1_char(Ten,ith,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::ith
		character(len=*),intent(in)::val
			call modify_TData_class(Ten,(/ith/),(/getTotalData(Ten)/),1,val)
		return
	end subroutine
	subroutine modifyTen_val1_Tensor(Ten,dimen,val)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen
		class(Tensor),intent(in)::val
		if(val%getTotalData().ne.1)then
			call writemess("Do no finished this case, in modifyTen_val_Tensor",-1)
			call error_stop()
		end if
		select case(val%getType())
			case (1)
				call modifyTen_val_class(Ten,(/dimen/),val%ii(1))
			case (2)
				call modifyTen_val_class(Ten,(/dimen/),val%si(1))	
			case (3)
				call modifyTen_val_class(Ten,(/dimen/),val%di(1))
			case (4)
				call modifyTen_val_class(Ten,(/dimen/),val%ci(1))
			case (5)
				call modifyTen_val_class(Ten,(/dimen/),val%zi(1))
			case (6)
				call modifyTen_val_class(Ten,(/dimen/),val%li(1))
			case (7)
				call modifyTen_val_class(Ten,(/dimen/),val%ai(1))
			case default
				call writemess("ERROR in modifyTen_val_Tensor",-1)
				call error_stop()
		end select
		return
	end subroutine
	
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  + - * /
!
!**************************************************************************************************************
!**************************************************************************************************************
!
! int + int --->int
! int + real4 --->rea4
! int + real8 --->rea8
! int + complex(kin=4) --->complex(kin=4)
! int + complex(kin=8) --->complex(kin=8)

! real4 + real4 --->rea4
! real4 + real8 --->rea8
! real4 + complex(kin=4) --->complex(kin=4)
! real4 + complex(kin=8) --->complex(kin=8)

! real8 + real8 --->rea8
! real8 + complex(kin=4) --->complex(kin=4)
! real8 + complex(kin=8) --->complex(kin=8)

! complex(kin=4) + complex(kin=4) --->complex(kin=4)
! complex(kin=4) + complex(kin=8) --->complex(kin=8)

	
	
	type(Tensor) function add(T1,T2)
		type(Tensor),intent(in) :: T1,T2
		type(Dimension)::dim1,dim2
		integer::classtype
		if((.not.T1%getflag()).or.(.not.T2%getflag()))then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		dim1=T1%TenDim
		dim2=T2%TenDim
		if(T1%getTotalData().ne.T2%getTotalData()) then
			call writemess("The totalData of T1 and T2 are not the same,(+)",-1)
			call writemess(T1%getTotalData()+','+T2%getTotalData(),-1)
			call writemess("The program will stop",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData(add%TData,T1%Tdata,T2%TData,1)
		return
	end function
	type(Tensor) function add_int(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		integer,intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,1)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_int(add%TData,T1%Tdata,num,1)
		return
	end function
	type(Tensor) function add_int_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		integer,intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,1)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_int_TData(add%TData,num,T1%Tdata,1)
		return
	end function
	type(Tensor) function add_real4(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		real(kind=4),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,2)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_real4(add%TData,T1%Tdata,num,1)
		return
	end function
	type(Tensor) function add_real4_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		real(kind=4),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,2)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_real4_TData(add%TData,num,T1%Tdata,1)
		return
	end function
	type(Tensor) function add_real8(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		real(kind=8),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,3)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_real8(add%TData,T1%Tdata,num,1)
		return
	end function
	type(Tensor) function add_real8_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		real(kind=8),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,3)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_real8_TData(add%TData,num,T1%Tdata,1)
		return
	end function
	type(Tensor) function add_com4(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		complex(kind=4),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,4)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_com4(add%TData,T1%Tdata,num,1)
		return
	end function
	type(Tensor) function add_com4_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		complex(kind=4),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,4)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_com4_TData(add%TData,num,T1%Tdata,1)
		return
	end function
	type(Tensor) function add_com8(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		complex(kind=8),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,5)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_com8(add%TData,T1%Tdata,num,1)
		return
	end function
	type(Tensor) function add_com8_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		complex(kind=8),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,5)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_com8_TData(add%TData,num,T1%Tdata,1)
		return
	end function
	type(Tensor) function add_char(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		character(len=*),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,7)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_char(add%TData,T1%Tdata,num,1)
		return
	end function
	type(Tensor) function add_char_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		character(len=*),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(+)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,7)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_char_TData(add%TData,num,T1%Tdata,1)
		return
	end function
	type(Tensor) function minus(T1,T2)
		type(Tensor),intent(in) :: T1,T2
		type(Dimension)::dim1,dim2
		integer::classtype
		if((.not.T1%getflag()).or.(.not.T2%getflag()))then
			call writemess("There is no data in the Tensor,(-)",-1)
			call error_stop()
		end if
		dim1=T1%TenDim
		dim2=T2%TenDim
		if(.not.(dim1.equ.dim2)) then
			call writemess("The dimension of T1 and T2 are not the same,in (-)")
			call writemess("The program will stop")
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		call allocateTensor(minus,T1%TenDim,classtype)
		call add_minu_TData(minus%TData,T1%Tdata,T2%TData,-1)
		return
	end function
	type(Tensor) function minus_int(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		integer,intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(-)")
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,1)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_int(add%TData,T1%Tdata,num,-1)
		return
	end function
	type(Tensor) function minus_int_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		integer,intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(-)")
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,1)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_int_TData(add%TData,num,T1%Tdata,-1)
		return
	end function
	type(Tensor) function minus_real4(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		real(kind=4),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(-)")
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,2)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_real4(add%TData,T1%Tdata,num,-1)
		return
	end function
	type(Tensor) function minus_real4_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		real(kind=4),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(-)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,2)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_real4_TData(add%TData,num,T1%Tdata,-1)
		return
	end function
	type(Tensor) function minus_real8(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		real(kind=8),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(-)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,3)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_real8(add%TData,T1%Tdata,num,-1)
		return
	end function
	type(Tensor) function minus_real8_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		real(kind=8),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(-)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,3)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_real8_TData(add%TData,num,T1%Tdata,-1)
		return
	end function
	type(Tensor) function minus_com4(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		complex(kind=4),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(-)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,4)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_com4(add%TData,T1%Tdata,num,-1)
		return
	end function
	type(Tensor) function minus_com4_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		complex(kind=4),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(-)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,4)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_com4_TData(add%TData,num,T1%Tdata,-1)
		return
	end function
	type(Tensor) function minus_com8(T1,num)result(add)
		type(Tensor),intent(in) :: T1
		complex(kind=8),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(-)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,5)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_TData_com8(add%TData,T1%Tdata,num,-1)
		return
	end function
	type(Tensor) function minus_com8_(num,T1)result(add)
		type(Tensor),intent(in) :: T1
		complex(kind=8),intent(in)::num
		type(Dimension)::dim1
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(-)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,5)
		call allocateTensor(add,T1%TenDim,classtype)
		call add_minu_com8_TData(add%TData,num,T1%Tdata,-1)
		return
	end function
	
	
	type(Tensor) function multiply_number_int(T1,num)result(Res)
		type(Tensor),intent(in) :: T1
		integer,intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(*)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,1)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_int(Res%TData,T1%TData,num)
		return
	end function
	type(Tensor) function multiply_number_real4(T1,num)result(Res)
		type(Tensor),intent(in) :: T1
		real(kind=4),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(*)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,2)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_real4(Res%TData,T1%TData,num)
		return
	end function
	type(Tensor) function multiply_number_real8(T1,num)result(Res)
		type(Tensor),intent(in) :: T1
		real(kind=8),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(*)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,3)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_real8(Res%TData,T1%TData,num)
		return
	end function
	type(Tensor) function multiply_number_com4(T1,num)result(Res)
		type(Tensor),intent(in) :: T1
		complex(kind=4),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(*)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,4)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_com4(Res%TData,T1%TData,num)
		return
	end function
	type(Tensor) function multiply_number_com8(T1,num)result(Res)
		type(Tensor),intent(in) :: T1
		complex(kind=8),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(*)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,5)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_com8(Res%TData,T1%TData,num)
		return
	end function
	
	type(Tensor) function multiply_number_int_(num,T1)result(Res)
		type(Tensor),intent(in) :: T1
		integer,intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(*)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,1)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_int(Res%TData,T1%TData,num)
		return
	end function
	type(Tensor) function multiply_number_real4_(num,T1)result(Res)
		type(Tensor),intent(in) :: T1
		real(kind=4),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(*)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,2)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_real4(Res%TData,T1%TData,num)
		return
	end function
	type(Tensor) function multiply_number_real8_(num,T1)result(Res)
		type(Tensor),intent(in) :: T1
		real(kind=8),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(*)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,3)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_real8(Res%TData,T1%TData,num)
		return
	end function
	type(Tensor) function multiply_number_com4_(num,T1)result(Res)
		type(Tensor),intent(in) :: T1
		complex(kind=4),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(*)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,4)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_com4(Res%TData,T1%TData,num)
		return
	end function
	type(Tensor) function multiply_number_com8_(num,T1)result(Res)
		type(Tensor),intent(in) :: T1
		complex(kind=8),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(*)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,5)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_com8(Res%TData,T1%TData,num)
		return
	end function



	type(Tensor) function divide_Tensor(T1,T) result(Res)
		type(Tensor),intent(in) :: T1
		type(Tensor),intent(in) :: T
		integer::classtype
		if(T%getTotalData().ne.1)then
			call writemess("ERROR in T1/T2, T2 should be lengh=1",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T1%TData,T%TData)
		call allocateTensor(Res,T1%TenDim,max(classtype,2))
		call TDatadivideTensor(Res%TData,T1%TData,T%TData)
		return
	end function
	type(Tensor) function int_divide_Tensor(num,T) result(Res)
		integer,intent(in) :: num
		type(Tensor),intent(in) :: T
		integer::classtype,Aclasstype
		if(T%getTotalData().ne.1)then
			call writemess("ERROR in number/T, T should be lengh=1",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T%TData,2)
		Aclasstype=1
		call allocateTensor(Res,T%TenDim,classtype)
		call TDatadivideTensor2(Res%TData,num,T%TData,Aclasstype)
		return
	end function
	type(Tensor) function real4_divide_Tensor(num,T) result(Res)
		real(kind=4),intent(in) :: num
		type(Tensor),intent(in) :: T
		integer::classtype,Aclasstype
		if(T%getTotalData().ne.1)then
			call writemess("ERROR in number/T, T should be lengh=1",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T%TData,2)
		Aclasstype=2
		call allocateTensor(Res,T%TenDim,classtype)
		call TDatadivideTensor2(Res%TData,num,T%TData,Aclasstype)
		return
	end function
	type(Tensor) function real8_divide_Tensor(num,T) result(Res)
		real(kind=8),intent(in) :: num
		type(Tensor),intent(in) :: T
		integer::classtype,Aclasstype
		if(T%getTotalData().ne.1)then
			call writemess("ERROR in number/T, T should be lengh=1",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T%TData,3)
		Aclasstype=3
		call allocateTensor(Res,T%TenDim,classtype)
		call TDatadivideTensor2(Res%TData,num,T%TData,Aclasstype)
		return
	end function
	type(Tensor) function com4_divide_Tensor(num,T) result(Res)
		complex(kind=4),intent(in) :: num
		type(Tensor),intent(in) :: T
		integer::classtype,Aclasstype
		if(T%getTotalData().ne.1)then
			call writemess("ERROR in number/T, T should be lengh=1",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T%TData,4)
		Aclasstype=4
		call allocateTensor(Res,T%TenDim,classtype)
		call TDatadivideTensor2(Res%TData,num,T%TData,Aclasstype)
		return
	end function
	type(Tensor) function com8_divide_Tensor(num,T) result(Res)
		complex(kind=8),intent(in) :: num
		type(Tensor),intent(in) :: T
		integer::classtype,Aclasstype
		if(T%getTotalData().ne.1)then
			call writemess("ERROR in number/T, T should be lengh=1",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T%TData,5)
		Aclasstype=5
		call allocateTensor(Res,T%TenDim,classtype)
		call TDatadivideTensor2(Res%TData,num,T%TData,Aclasstype)
		return
	end function
	type(Tensor) function divide_num_int(T1,num) result(Res)
		type(Tensor),intent(in) :: T1
		integer,intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(/)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,3)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_real8(Res%TData,T1%TData,1d0/dble(num))
		return
	end function
	type(Tensor) function divide_num_real4(T1,num) result(Res)
		type(Tensor),intent(in) :: T1
		real(kind=4),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(/)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,3)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_real8(Res%TData,T1%TData,1d0/dble(num))
		return
	end function
	type(Tensor) function divide_num_real8(T1,num) result(Res)
		type(Tensor),intent(in) :: T1
		real(kind=8),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(/)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,3)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_real8(Res%TData,T1%TData,1d0/num)
		return
	end function
	type(Tensor) function divide_num_com4(T1,num) result(Res)
		type(Tensor),intent(in) :: T1
		complex(kind=4),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(/)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,5)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_com8(Res%TData,T1%TData,dcmplx(1d0)/dcmplx(num))
		return
	end function
	type(Tensor) function divide_num_com8(T1,num) result(Res)
		type(Tensor),intent(in) :: T1
		complex(kind=8),intent(in) ::   num
		integer::classtype
		if(.not.T1%getflag())then
			call writemess("There is no data in the Tensor,(/)",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu2(T1%TData,5)
		call allocateTensor(Res,T1%TenDim,classtype)
		call TDatamultiply_number_com8(Res%TData,T1%TData,dcmplx(1.)/num)
		return
	end function

!**************** ProductTensor  ***************************
!		ProductTensor regard the last index of T1 and the first index
!	  of T2 as the dimenion for matrix-product,other index will be see
!	  as another dimenison.T1 and T2 can be any rank,but the last dimenion
!	  of T1 and the first diemsion of T2 should be equal.
	type(Tensor) function ProductTensor0 (T1,T2) result(ProductTensor)
		type(Tensor),intent(in) :: T1,T2
		integer::rank1,rank2,flag,T1m,T1n,T2m,T2n,T1l,T2l
		type(Dimension),pointer::D1,D2,newD
		integer::i,classtype,total1,total2
		if(.not.T1%getflag()) then
			write(*,*)"ERROR in (*)"
			write(*,*)"Tensor is no data is the first Tensor"
			write(*,*)"stop"
			call error_stop()
		end if
		if(.not.T2%getflag()) then
			write(*,*)"ERROR in (*)"
			write(*,*)"Tensor is no data is the second Tensor"
			write(*,*)"stop"
			call error_stop()
		end if	
		D1=>workingDimension1
		D2=>workingDimension2
		newD=>WorkingDimension3
		rank1=getRank(T1)
		rank2=getRank(T2)
		D1=.subDim.T1
		D2=.subDim.T2
		total1=T1%getTotalData()
		total2=T2%getTotalData()
		if((total1.eq.1).or.(total2.eq.1)) then
			flag=0
		else if((rank1.eq.1).and.(rank2.eq.1)) then
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
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		select case (flag)
			case (0)
				if((total1.eq.1).and.(total2.eq.1)) then!there is only one element in both T1 and T2
						if((rank1.eq.1).and.(rank2.eq.1))then!Number*number,(1) *(1)
							call allocateTensor(ProductTensor,T1%TenDim,classtype)
							call product_NumNum(ProductTensor%TData,T1%TData,T2%TData)	
							return
						else if((rank1.eq.1).and.(rank2.ne.1))then!Number*Tensor,(1) * (1,1,1)
							call allocateTensor(ProductTensor,T2%TenDim,classtype)
							call product_NumNum(ProductTensor%TData,T1%TData,T2%TData)	
							return
						else if((rank1.ne.1).and.(rank2.eq.1))then!Tensor*Number, (1,1,1) * (1)
							call allocateTensor(ProductTensor,T1%TenDim,classtype)
							call product_NumNum(ProductTensor%TData,T1%TData,T2%TData)	
							return
						else if((rank1.ne.1).and.(rank2.ne.1))then!Tensor*Tensor, (1,1,1) * (1,1)
							if(rank1.ge.2) then
								newD=D1.sub.[1,rank1-1]
							end if
							if(rank2.ge.2) then
								newD=newD+(D2.sub.[2,rank2])
							end if
							call allocateTensor(ProductTensor,newD,classtype)
							call product_NumNum(ProductTensor%TData,T1%TData,T2%TData)	
							return
						end if
				else if((total1.eq.1).and.(total2.ne.1)) then!there is only one element in both T1, but not T2
						if(rank1.eq.1)then!Number*Tensor,(1) *(3) or (1) *(2,3) 
							call allocateTensor(ProductTensor,T2%TenDim,classtype)
							call product_Mnum_dim1(ProductTensor%TData,T2%TData,T1%TData)	
							return
						else 
							if(rank2.eq.1)then!Tensor*number,(1,1) *(3)
								call writemess("ERROR in ProductTensor,case -1,stop",-1)
								call T1%diminfo('dimension of T1')
								call T2%diminfo('dimension of T2')
								call error_stop()
							else!Tensor*Tensor,(1,1) *(1,2,1,2)
								if(T2%dim(1).ne.1)then
									call writemess("ERROR in ProductTensor,case -2,stop",-1)
									call T1%diminfo('dimension of T1')
									call T2%diminfo('dimension of T2')
									call error_stop()
								end if
								if(rank1.ge.2) then
									newD=D1.sub.[1,rank1-1]
								end if
								if(rank2.ge.2) then
									newD=newD+(D2.sub.[2,rank2])
								end if
								call allocateTensor(ProductTensor,newD,classtype)
								call product_Mnum_dim1(ProductTensor%TData,T2%TData,T1%TData)	
								return
							end if
						end if
					else if((total1.ne.1).and.(total2.eq.1)) then!there is only one element in both T2, but not T1
							if(rank2.eq.1)then!Tensor*number,(3) *(1) or (2,3) *(1) 
								call allocateTensor(ProductTensor,T1%TenDim,classtype)
								call product_Mnum_dim1(ProductTensor%TData,T1%TData,T2%TData)	
								return
							else
								if(rank1.eq.1)then!Tensor*number,(3)*(1,1)
									call writemess("ERROR in ProductTensor,case -3,stop",-1)
									call T1%diminfo('dimension of T1')
									call T2%diminfo('dimension of T2')
									call error_stop()
								else!Tensor*Tensor,(1,2,2,1) *(1,1)
									if(T1%dim(rank1).ne.1)then
										call writemess("ERROR in ProductTensor,case -4,stop",-1)
										call T1%diminfo('dimension of T1')
										call T2%diminfo('dimension of T2')
										call error_stop()
									end if
									if(rank1.ge.2) then
										newD=D1.sub.[1,rank1-1]
									end if
									if(rank2.ge.2) then
										newD=newD+(D2.sub.[2,rank2])
									end if
									call allocateTensor(ProductTensor,newD,classtype)
									call product_Mnum_dim1(ProductTensor%TData,T1%TData,T2%TData)	
									return
								end if
							end if
					else
							call writemess("ERROR in ProductTensor,case -5,stop",-1)
							call T1%diminfo('dimension of T1')
							call T2%diminfo('dimension of T2')
							call error_stop()
					end if
			case (1)
				T1m=T1.dim.1
				T2n=T2.dim.1
				if(T1m.ne.T2n) then
					call writemess("ERROR in ProductTensor,case 1,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				call allocateTensor(ProductTensor,(/1/),classtype)
				call product_VV_dim1(ProductTensor%TData,T1%TData,T2%TData)	
				return
			case (2)
				if(rank2.ge.2) then
					newD=D2.sub.[2,rank2]
					D2=D2%fuseIndex(2,rank2)
				end if
				T2m=D2.i.1
				T2n=D2.i.2
				if((D1.i.1) .ne. T2m) then
					call writemess("ERROR in ProductTensor,case 2,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				call allocateTensor(ProductTensor,newD,classtype)
				if(T2n.eq.1)then![m]*[m,1]case
					call product_VV_dim1(ProductTensor%TData,T1%TData,T2%TData)	
				else
					call product_VM_dim1(ProductTensor%TData,T1%TData,T2%TData,T2m,T2n)	
				end if
				return
		case (3)
				if(rank1.ge.2) then
					newD=D1.sub.[1,rank1-1]
					D1=D1%fuseIndex(1,rank1-2)
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				if((D2.i.1) .ne. T1n) then
					call writemess("ERROR in ProductTensor,case 3,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				call allocateTensor(ProductTensor,newD,classtype)
				if(T1m.eq.1)then![1,m]*[m]case
					call product_VV_dim1(ProductTensor%TData,T1%TData,T2%TData)	
				else
					call product_MV_dim1(ProductTensor%TData,T1%TData,T2%TData,T1m,T1n)	
				end if
				return
		case (4)
				if(rank1.ge.2) then
					newD=D1.sub.[1,rank1-1]
					D1=D1%fuseIndex(1,rank1-2)
				end if
				if(rank2.ge.2) then
					newD=newD+(D2.sub.[2,rank2])
					D2=D2%fuseIndex(2,rank2)
				end if
				if((D1.i.2).ne.(D2.i.1)) then
					call writemess("ERROR in ProductTensor,case 4,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				T2m=D2.i.1
				T2n=D2.i.2
				call allocateTensor(ProductTensor,newD,classtype)
				if((T1m.eq.1).and.(T2n.eq.1))then![1,m] [m,1]
					call product_VV_dim1(ProductTensor%TData,T1%TData,T2%TData)	
				else if((T1m.eq.1).and.(T2n.ne.1))then![1,m] [m,n]
					call product_VM_dim1(ProductTensor%TData,T1%TData,T2%TData,T2m,T2n)	
				else if((T1m.ne.1).and.(T2n.eq.1))then![m,n] [n,1]
					call product_MV_dim1(ProductTensor%TData,T1%TData,T2%TData,T1m,T1n)	
				else
					call product_MM_dim1(ProductTensor%TData,T1%TData,T2%Tdata,T1m,T1n,T2n)	
				end if
				return
		case default 
			write(*,*) "ERROR in ProductTensor,no such data"
			call error_stop()
		end 	select
		return
	end function
	
	function ProductTensor_forContract (T1,T2) result(ProductTensor)
		type(Tensor)::ProductTensor
		type(Tensor),intent(in) :: T1,T2
		integer::rank1,rank2,flag,T1m,T1n,T2m,T2n,T1l,T2l
		type(Dimension),pointer::D1,D2,newD
		integer::i,classtype,total1,total2
		if(.not.T1%getflag()) then
			write(*,*)"ERROR in (*)"
			write(*,*)"Tensor is no data is the first Tensor"
			write(*,*)"stop"
			call error_stop()
		end if
		if(.not.T2%getflag()) then
			write(*,*)"ERROR in (*)"
			write(*,*)"Tensor is no data is the second Tensor"
			write(*,*)"stop"
			call error_stop()
		end if	
		D1=>workingDimension1
		D2=>workingDimension2
		newD=>WorkingDimension3
		rank1=getRank(T1)
		rank2=getRank(T2)
		D1=.subDim.T1
		D2=.subDim.T2
		total1=T1%getTotalData()
		total2=T2%getTotalData()
		if((total1.eq.1).or.(total2.eq.1)) then
			flag=0
		else if((rank1.eq.1).and.(rank2.eq.1)) then
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
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		select case (flag)
			case (0)
				if((total1.eq.1).and.(total2.eq.1)) then!there is only one element in both T1 and T2
						if((rank1.eq.1).and.(rank2.eq.1))then!Number*number,(1) *(1)
							call allocateTensor(ProductTensor,[1],classtype)
							call product_NumNum(ProductTensor%TData,T1%TData,T2%TData)	
							return
						else 
							if(rank1.ge.2) then
								newD=D1.sub.[1,rank1-1]
							end if
							if(rank2.ge.2) then
								newD=newD+(D2.sub.[2,rank2])
							end if
							call allocateTensor(ProductTensor,newD,classtype)
							call product_NumNum(ProductTensor%TData,T1%TData,T2%TData)	
							return
						end if
				else if((total1.eq.1).and.(total2.ne.1)) then!there is only one element in both T1, but not T2
							!Tensor*Tensor,(1,1) *(1,2,1,2) or (1) *(1,3) 
						if(T2%dim(1).ne.1)then
							call writemess("ERROR in ProductTensor,case -2,stop",-1)
							call T1%diminfo('dimension of T1')
							call T2%diminfo('dimension of T2')
							call error_stop()
						end if
						if(rank1.ge.2) then
							newD=D1.sub.[1,rank1-1]
						end if
						if(rank2.ge.2) then
							newD=newD+(D2.sub.[2,rank2])
						end if
						call allocateTensor(ProductTensor,newD,classtype)
						call product_Mnum_dim1(ProductTensor%TData,T2%TData,T1%TData)	
						return
					else if((total1.ne.1).and.(total2.eq.1)) then!there is only one element in both T2, but not T1
								!Tensor*Tensor,(1,2,2,1) *(1,1) or (2,1) *(1) 
						if(T1%dim(rank1).ne.1)then
							call writemess("ERROR in ProductTensor,case -4,stop",-1)
							call T1%diminfo('dimension of T1')
							call T2%diminfo('dimension of T2')
							call error_stop()
						end if
						if(rank1.ge.2) then
							newD=D1.sub.[1,rank1-1]
						end if
						if(rank2.ge.2) then
							newD=newD+(D2.sub.[2,rank2])
						end if
						call allocateTensor(ProductTensor,newD,classtype)
						call product_Mnum_dim1(ProductTensor%TData,T1%TData,T2%TData)	
						return
					else
							call writemess("ERROR in ProductTensor,case -5,stop",-1)
							call T1%diminfo('dimension of T1')
							call T2%diminfo('dimension of T2')
							call error_stop()
					end if
			case (1)
				T1m=T1.dim.1
				T2n=T2.dim.1
				if(T1m.ne.T2n) then
					call writemess("ERROR in ProductTensor,case 1,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				call allocateTensor(ProductTensor,(/1/),classtype)
				call product_VV_dim1(ProductTensor%TData,T1%TData,T2%TData)	
				return
			case (2)
				if(rank2.ge.2) then
					newD=D2.sub.[2,rank2]
					D2=D2%fuseIndex(2,rank2)
				end if
				T2m=D2.i.1
				T2n=D2.i.2
				if((D1.i.1) .ne. T2m) then
					call writemess("ERROR in ProductTensor,case 2,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				call allocateTensor(ProductTensor,newD,classtype)
				if(T2n.eq.1)then![m]*[m,1]case
					call product_VV_dim1(ProductTensor%TData,T1%TData,T2%TData)	
				else
					call product_VM_dim1(ProductTensor%TData,T1%TData,T2%TData,T2m,T2n)	
				end if
				return
		case (3)
				if(rank1.ge.2) then
					newD=D1.sub.[1,rank1-1]
					D1=D1%fuseIndex(1,rank1-2)
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				if((D2.i.1) .ne. T1n) then
					call writemess("ERROR in ProductTensor,case 3,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				call allocateTensor(ProductTensor,newD,classtype)
				if(T1m.eq.1)then![1,m]*[m]case
					call product_VV_dim1(ProductTensor%TData,T1%TData,T2%TData)	
				else
					call product_MV_dim1(ProductTensor%TData,T1%TData,T2%TData,T1m,T1n)	
				end if
				return
		case (4)
				if(rank1.ge.2) then
					newD=D1.sub.[1,rank1-1]
					D1=D1%fuseIndex(1,rank1-2)
				end if
				if(rank2.ge.2) then
					newD=newD+(D2.sub.[2,rank2])
					D2=D2%fuseIndex(2,rank2)
				end if
				if((D1.i.2).ne.(D2.i.1)) then
					call writemess("ERROR in ProductTensor,case 4,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				T2m=D2.i.1
				T2n=D2.i.2
				call allocateTensor(ProductTensor,newD,classtype)
				if((T1m.eq.1).and.(T2n.eq.1))then![1,m] [m,1]
					call product_VV_dim1(ProductTensor%TData,T1%TData,T2%TData)	
				else if((T1m.eq.1).and.(T2n.ne.1))then![1,m] [m,n]
					call product_VM_dim1(ProductTensor%TData,T1%TData,T2%TData,T2m,T2n)	
				else if((T1m.ne.1).and.(T2n.eq.1))then![m,n] [n,1]
					call product_MV_dim1(ProductTensor%TData,T1%TData,T2%TData,T1m,T1n)	
				else
					call product_MM_dim1(ProductTensor%TData,T1%TData,T2%Tdata,T1m,T1n,T2n)	
				end if
				return
		case default 
			write(*,*) "ERROR in ProductTensor,no such data"
			call error_stop()
		end 	select
		return
	end function
	
	subroutine ProductTensorRoutine1(Res,T1,T2,alpha,beta)! Res = alpha* T1 *  T2  + beta*Res
		class(Tensor),target,intent(inout)::Res
		class(Tensor),target,intent(in) :: T1,T2
		class(*),intent(in)::alpha,beta
		integer::rank1,rank2,flag,T1m,T1n,T2m,T2n,T1l,T2l
		type(Dimension),pointer::D1,D2,newD
		integer::i,classtype,total1,total2
		class(Tensor),pointer::Resp,T1p,T2p
		if(.not.T1%getflag()) then
			write(*,*)"ERROR in (*)"
			write(*,*)"Tensor is no data is the first Tensor"
			write(*,*)"stop"
			call error_stop()
		end if
		if(.not.T2%getflag()) then
			write(*,*)"ERROR in (*)"
			write(*,*)"Tensor is no data is the second Tensor"
			write(*,*)"stop"
			call error_stop()
		end if	
		
		Resp=>Res
		T1p=>T1
		T2p=>T2
		if(associated(Resp,T1p).or.associated(Resp,T2p))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%ProductTensorRoutine(Res,T1,T2,alpha,beta)')
			call writemess('Res and T1, or Res and T2, can not be a same variable')
			call error_stop
		end if
		Resp=>null()
		T1p=>null()
		T2p=>null()
		
		
		D1=>workingDimension1
		D2=>workingDimension2
		newD=>WorkingDimension3
		rank1=getRank(T1)
		rank2=getRank(T2)
		D1=.subDim.T1
		D2=.subDim.T2
		total1=T1%getTotalData()
		total2=T2%getTotalData()
		if((total1.eq.1).or.(total2.eq.1)) then
			flag=0
		else if((rank1.eq.1).and.(rank2.eq.1)) then
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
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		select case (flag)
			case (0)
				if((total1.eq.1).and.(total2.eq.1)) then!there is only one element in both T1 and T2
						if((rank1.eq.1).and.(rank2.eq.1))then!Number*number,(1) *(1)
							if(.not.Res%getFlag())then
								call allocateTensor(Res,[1],classtype)
								call Res%zero()
							end if
							call product_NumNum_par(Res%TData,T1%TData,T2%TData,alpha,beta)	
							return
						else !Tensor*Tensor, (1,1,1) * (1,1)
							if(rank1.ge.2) then
								newD=D1.sub.[1,rank1-1]
							end if
							if(rank2.ge.2) then
								newD=newD+(D2.sub.[2,rank2])
							end if
							if(.not.Res%getFlag())then
								call allocateTensor(Res,newD,classtype)
								call Res%zero()
							end if
							call product_NumNum_par(Res%TData,T1%TData,T2%TData,alpha,beta)		
							return
						end if
				else if((total1.eq.1).and.(total2.ne.1)) then!there is only one element in both T1, but not T2
					!Tensor*Tensor,(1,1) *(1,2,1,2) or (1) *(1,3) 
					if(T2%dim(1).ne.1)then
						call writemess("ERROR in ProductTensor,case -2,stop",-1)
						call T1%diminfo('dimension of T1')
						call T2%diminfo('dimension of T2')
						call error_stop()
					end if
					if(rank1.ge.2) then
						newD=D1.sub.[1,rank1-1]
					end if
					if(rank2.ge.2) then
						newD=newD+(D2.sub.[2,rank2])
					end if
					if(.not.Res%getFlag())then
						call allocateTensor(Res,newD,classtype)
						call Res%zero()
					end if
					call product_Mnum_dim1_par(Res%TData,T2%TData,T1%TData,alpha,beta)	
					return
				else if((total1.ne.1).and.(total2.eq.1)) then!there is only one element in both T2, but not T1
							!Tensor*Tensor,(1,2,2,1) *(1,1) or (2,1) *(1) 
					if(T1%dim(rank1).ne.1)then
						call writemess("ERROR in ProductTensor,case -4,stop",-1)
						call T1%diminfo('dimension of T1')
						call T2%diminfo('dimension of T2')
						call error_stop()
					end if
					if(rank1.ge.2) then
						newD=D1.sub.[1,rank1-1]
					end if
					if(rank2.ge.2) then
						newD=newD+(D2.sub.[2,rank2])
					end if
					if(.not.Res%getFlag())then
						call allocateTensor(Res,newD,classtype)
						call Res%zero()
					end if
					call product_Mnum_dim1_par(Res%TData,T1%TData,T2%TData,alpha,beta)	
					return
				else
						call writemess("ERROR in ProductTensor,case -5,stop",-1)
						call T1%diminfo('dimension of T1')
						call T2%diminfo('dimension of T2')
						call error_stop()
				end if
			case (1)
				T1m=T1.dim.1
				T2n=T2.dim.1
				if(T1m.ne.T2n) then
					call writemess("ERROR in ProductTensor,case 1,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				if(.not.Res%getFlag())then
					call allocateTensor(Res,(/1/),classtype)
					call Res%zero()
				end if
				call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,beta)	
				return
			case (2)
				if(rank2.ge.2) then
					newD=D2.sub.[2,rank2]
					D2=D2%fuseIndex(2,rank2)
				end if
				T2m=D2.i.1
				T2n=D2.i.2
				if((D1.i.1) .ne. T2m) then
					call writemess("ERROR in ProductTensor,case 2,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				if(.not.Res%getFlag()) then
					call allocateTensor(Res,newD,classtype)
					call Res%zero()
				end if
				if(T2n.eq.1)then![m]*[m,1]case
					call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,beta)
				else
					call product_VM_dim1_par(Res%TData,T1%TData,T2%TData,T2m,T2n,alpha,beta)	
				end if
				return
		case (3)
				if(rank1.ge.2) then
					newD=D1.sub.[1,rank1-1]
					D1=D1%fuseIndex(1,rank1-2)
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				if((D2.i.1) .ne. T1n) then
					call writemess("ERROR in ProductTensor,case 3,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				if(.not.Res%getFlag())then
					call allocateTensor(Res,newD,classtype)
					call Res%zero()
				end if
				if(T1m.eq.1)then![1,m]*[m]case
					call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,beta)
				else
					call product_MV_dim1_par(Res%TData,T1%TData,T2%TData,T1m,T1n,alpha,beta)
				end if
				return
		case (4)
				if(rank1.ge.2) then
					newD=D1.sub.[1,rank1-1]
					D1=D1%fuseIndex(1,rank1-2)
				end if
				if(rank2.ge.2) then
					newD=newD+(D2.sub.[2,rank2])
					D2=D2%fuseIndex(2,rank2)
				end if
				if((D1.i.2).ne.(D2.i.1)) then
					call writemess("ERROR in ProductTensor,case 4,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				T2m=D2.i.1
				T2n=D2.i.2
				if(.not.Res%getFlag())then
					call allocateTensor(Res,newD,classtype)
					call Res%zero()
				end if
				if((T1m.eq.1).and.(T2n.eq.1))then![1,m] [m,1]
					call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,beta)
				else if((T1m.eq.1).and.(T2n.ne.1))then![1,m] [m,n]
					call product_VM_dim1_par(Res%TData,T1%TData,T2%TData,T2m,T2n,alpha,beta)	
				else if((T1m.ne.1).and.(T2n.eq.1))then![m,n] [n,1]
					call product_MV_dim1_par(Res%TData,T1%TData,T2%TData,T1m,T1n,alpha,beta)	
				else
					call product_MM_dim1_par(Res%TData,T1%TData,T2%Tdata,T1m,T1n,T2n,alpha,beta)
				end if
				return
		case default 
			write(*,*) "ERROR in ProductTensor,no such data"
			call error_stop()
		end 	select
	end subroutine 
	
	
	subroutine ProductTensorRoutine2(Res,T1,T2,alpha)! Res = alpha* T1 *  T2
		class(Tensor),target,intent(inout)::Res
		class(Tensor),target,intent(in) :: T1,T2
		class(*),intent(in)::alpha
		integer::rank1,rank2,flag,T1m,T1n,T2m,T2n,T1l,T2l
		type(Dimension),pointer::D1,D2,newD
		integer::i,classtype,total1,total2
		class(Tensor),pointer::Resp,T1p,T2p
		if(.not.T1%getflag()) then
			write(*,*)"ERROR in (*)"
			write(*,*)"Tensor is no data is the first Tensor"
			write(*,*)"stop"
			call error_stop()
		end if
		if(.not.T2%getflag()) then
			write(*,*)"ERROR in (*)"
			write(*,*)"Tensor is no data is the second Tensor"
			write(*,*)"stop"
			call error_stop()
		end if	
		Resp=>Res
		T1p=>T1
		T2p=>T2
		if(associated(Resp,T1p).or.associated(Resp,T2p))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%ProductTensorRoutine(Res,T1,T2,alpha,beta)')
			call writemess('Res and T1, or Res and T2, can not be a same variable')
			call error_stop
		end if
		Resp=>null()
		T1p=>null()
		T2p=>null()
		D1=>workingDimension1
		D2=>workingDimension2
		newD=>WorkingDimension3
		call Res%empty()
		rank1=getRank(T1)
		rank2=getRank(T2)
		D1=.subDim.T1
		D2=.subDim.T2
		total1=T1%getTotalData()
		total2=T2%getTotalData()
		if((total1.eq.1).or.(total2.eq.1)) then
			flag=0
		else if((rank1.eq.1).and.(rank2.eq.1)) then
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
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		select case (flag)
			case (0)
				if((total1.eq.1).and.(total2.eq.1)) then!there is only one element in both T1 and T2
						if((rank1.eq.1).and.(rank2.eq.1))then!Number*number,(1) *(1)
							call allocateTensor(Res,[1],classtype)
							call product_NumNum_par(Res%TData,T1%TData,T2%TData,alpha,0)	
							return
						else !Tensor*Tensor, (1,1,1) * (1,1)
							if(rank1.ge.2) then
								newD=D1.sub.[1,rank1-1]
							end if
							if(rank2.ge.2) then
								newD=newD+(D2.sub.[2,rank2])
							end if
							call allocateTensor(Res,newD,classtype)
							call product_NumNum_par(Res%TData,T1%TData,T2%TData,alpha,0)		
							return
						end if
				else if((total1.eq.1).and.(total2.ne.1)) then!there is only one element in both T1, but not T2
							!Tensor*Tensor,(1,1) *(1,2,1,2)  or (1) *(1,3) 
					if(T2%dim(1).ne.1)then
						call writemess("ERROR in ProductTensor,case -2,stop",-1)
						call T1%diminfo('dimension of T1')
						call T2%diminfo('dimension of T2')
						call error_stop()
					end if
					if(rank1.ge.2) then
						newD=D1.sub.[1,rank1-1]
					end if
					if(rank2.ge.2) then
						newD=newD+(D2.sub.[2,rank2])
					end if
					call allocateTensor(Res,newD,classtype)
					call product_Mnum_dim1_par(Res%TData,T2%TData,T1%TData,alpha,0)	
					return
				else if((total1.ne.1).and.(total2.eq.1)) then!there is only one element in both T2, but not T1
							!Tensor*Tensor,(1,2,2,1) *(1,1) or (2,1) *(1) 
					if(T1%dim(rank1).ne.1)then
						call writemess("ERROR in ProductTensor,case -4,stop",-1)
						call T1%diminfo('dimension of T1')
						call T2%diminfo('dimension of T2')
						call error_stop()
					end if
					if(rank1.ge.2) then
						newD=D1.sub.[1,rank1-1]
					end if
					if(rank2.ge.2) then
						newD=newD+(D2.sub.[2,rank2])
					end if
					call allocateTensor(Res,newD,classtype)
					call product_Mnum_dim1_par(Res%TData,T1%TData,T2%TData,alpha,0)	
					return
				else
						call writemess("ERROR in ProductTensor,case -5,stop",-1)
						call T1%diminfo('dimension of T1')
						call T2%diminfo('dimension of T2')
						call error_stop()
				end if
			case (1)
				T1m=T1.dim.1
				T2n=T2.dim.1
				if(T1m.ne.T2n) then
					call writemess("ERROR in ProductTensor,case 1,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				call allocateTensor(Res,(/1/),classtype)
				call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,0)	
				return
			case (2)
				if(rank2.ge.2) then
					newD=D2.sub.[2,rank2]
					D2=D2%fuseIndex(2,rank2)
				end if
				T2m=D2.i.1
				T2n=D2.i.2
				if((D1.i.1) .ne. T2m) then
					call writemess("ERROR in ProductTensor,case 2,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				call allocateTensor(Res,newD,classtype)
				if(T2n.eq.1)then![m]*[m,1]case
					call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,0)
				else
					call product_VM_dim1_par(Res%TData,T1%TData,T2%TData,T2m,T2n,alpha,0)	
				end if
				return
		case (3)
				if(rank1.ge.2) then
					newD=D1.sub.[1,rank1-1]
					D1=D1%fuseIndex(1,rank1-2)
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				if((D2.i.1) .ne. T1n) then
					call writemess("ERROR in ProductTensor,case 3,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				call allocateTensor(Res,newD,classtype)
				if(T1m.eq.1)then![1,m]*[m]case
					call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,0)
				else
					call product_MV_dim1_par(Res%TData,T1%TData,T2%TData,T1m,T1n,alpha,0)
				end if
				return
		case (4)
				if(rank1.ge.2) then
					newD=D1.sub.[1,rank1-1]
					D1=D1%fuseIndex(1,rank1-2)
				end if
				if(rank2.ge.2) then
					newD=newD+(D2.sub.[2,rank2])
					D2=D2%fuseIndex(2,rank2)
				end if
				if((D1.i.2).ne.(D2.i.1)) then
					call writemess("ERROR in ProductTensor,case 4,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				T2m=D2.i.1
				T2n=D2.i.2
				call allocateTensor(Res,newD,classtype)
				if((T1m.eq.1).and.(T2n.eq.1))then![1,m] [m,1]
					call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,0)
				else if((T1m.eq.1).and.(T2n.ne.1))then![1,m] [m,n]
					call product_VM_dim1_par(Res%TData,T1%TData,T2%TData,T2m,T2n,alpha,0)	
				else if((T1m.ne.1).and.(T2n.eq.1))then![m,n] [n,1]
					call product_MV_dim1_par(Res%TData,T1%TData,T2%TData,T1m,T1n,alpha,0)	
				else
					call product_MM_dim1_par(Res%TData,T1%TData,T2%Tdata,T1m,T1n,T2n,alpha,0)
				end if
				return
		case default 
			write(*,*) "ERROR in ProductTensor,no such data"
			call error_stop()
		end 	select
	end subroutine 
	
	
	
!Not OK yet
	subroutine ProductTensorRoutine3(Res,T1,T2,alpha,beta,TRANSA,TRANSB)! Res = alpha*TRANSA( T1) * TRANSB( T2)  + beta*Res
		class(Tensor),target,intent(inout)::Res
		class(Tensor),target,intent(in) :: T1,T2
		character*1,intent(in)::TRANSA,TRANSB
		class(*),intent(in)::alpha,beta
		integer::rank1,rank2,flag,T1m,T1n,T2m,T2n,T1l,T2l
		type(Dimension),pointer::D1,D2,newD
		integer::i,classtype,total1,total2
		class(Tensor),pointer::Resp,T1p,T2p
		if(.not.T1%getflag()) then
			write(*,*)"ERROR in (*)"
			write(*,*)"Tensor is no data is the first Tensor"
			write(*,*)"stop"
			call error_stop()
		end if
		if(.not.T2%getflag()) then
			write(*,*)"ERROR in (*)"
			write(*,*)"Tensor is no data is the second Tensor"
			write(*,*)"stop"
			call error_stop()
		end if	
		
		Resp=>Res
		T1p=>T1
		T2p=>T2
		if(associated(Resp,T1p).or.associated(Resp,T2p))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%ProductTensorRoutine(Res,T1,T2,alpha,beta)')
			call writemess('Res and T1, or Res and T2, can not be a same variable')
			call error_stop
		end if
		Resp=>null()
		T1p=>null()
		T2p=>null()
		
		
		D1=>workingDimension1
		D2=>workingDimension2
		newD=>WorkingDimension3
		rank1=getRank(T1)
		rank2=getRank(T2)
		D1=.subDim.T1
		D2=.subDim.T2
		total1=T1%getTotalData()
		total2=T2%getTotalData()
		if((total1.eq.1).or.(total2.eq.1)) then
			flag=0
		else if((rank1.eq.1).and.(rank2.eq.1)) then
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
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		select case (flag)
			case (0)
				if((total1.eq.1).and.(total2.eq.1)) then!there is only one element in both T1 and T2
						if((rank1.eq.1).and.(rank2.eq.1))then!Number*number,(1) *(1)
							if(.not.Res%getFlag())then
								call allocateTensor(Res,[1],classtype)
								call Res%zero()
							end if
							call product_NumNum_par(Res%TData,T1%TData,T2%TData,alpha,beta)	
							return
						else !Tensor*Tensor, (1,1,1) * (1,1)
							if(rank1.ge.2) then
								newD=D1.sub.[1,rank1-1]
							end if
							if(rank2.ge.2) then
								newD=newD+(D2.sub.[2,rank2])
							end if
							if(.not.Res%getFlag())then
								call allocateTensor(Res,newD,classtype)
								call Res%zero()
							end if
							call product_NumNum_par(Res%TData,T1%TData,T2%TData,alpha,beta)		
							return
						end if
				else if((total1.eq.1).and.(total2.ne.1)) then!there is only one element in both T1, but not T2
							!Tensor*Tensor,(1,1) *(1,2,1,2) or (1) *(1,3) 
					if(T2%dim(1).ne.1)then
						call writemess("ERROR in ProductTensor,case -2,stop",-1)
						call T1%diminfo('dimension of T1')
						call T2%diminfo('dimension of T2')
						call error_stop()
					end if
					if(rank1.ge.2) then
						newD=D1.sub.[1,rank1-1]
					end if
					if(rank2.ge.2) then
						newD=newD+(D2.sub.[2,rank2])
					end if
					if(.not.Res%getFlag())then
						call allocateTensor(Res,newD,classtype)
						call Res%zero()
					end if
					call product_Mnum_dim1_par(Res%TData,T2%TData,T1%TData,alpha,beta)	
					return
				else if((total1.ne.1).and.(total2.eq.1)) then!there is only one element in both T2, but not T1
							!Tensor*Tensor,(1,2,2,1) *(1,1) or (2,1) *(1) 
					if(T1%dim(rank1).ne.1)then
						call writemess("ERROR in ProductTensor,case -4,stop",-1)
						call T1%diminfo('dimension of T1')
						call T2%diminfo('dimension of T2')
						call error_stop()
					end if
					if(rank1.ge.2) then
						newD=D1.sub.[1,rank1-1]
					end if
					if(rank2.ge.2) then
						newD=newD+(D2.sub.[2,rank2])
					end if
					if(.not.Res%getFlag())then
						call allocateTensor(Res,newD,classtype)
						call Res%zero()
					end if
					call product_Mnum_dim1_par(Res%TData,T1%TData,T2%TData,alpha,beta)	
					return
				else
						call writemess("ERROR in ProductTensor,case -5,stop",-1)
						call T1%diminfo('dimension of T1')
						call T2%diminfo('dimension of T2')
						call error_stop()
				end if
			case (1)
				T1m=T1.dim.1
				T2n=T2.dim.1
				if(T1m.ne.T2n) then
					call writemess("ERROR in ProductTensor,case 1,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				if(.not.Res%getFlag())then
					call allocateTensor(Res,(/1/),classtype)
					call Res%zero()
				end if
				call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,beta)	
				return
			case (2)
				if(rank2.ge.2) then
					newD=D2.sub.[2,rank2]
					D2=D2%fuseIndex(2,rank2)
				end if
				T2m=D2.i.1
				T2n=D2.i.2
				if((D1.i.1) .ne. T2m) then
					call writemess("ERROR in ProductTensor,case 2,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				if(.not.Res%getFlag()) then
					call allocateTensor(Res,newD,classtype)
					call Res%zero()
				end if
				if(T2n.eq.1)then![m]*[m,1]case
					call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,beta)
				else
					call product_VM_dim1_par(Res%TData,T1%TData,T2%TData,T2m,T2n,alpha,beta)	
				end if
				return
		case (3)
				if(rank1.ge.2) then
					newD=D1.sub.[1,rank1-1]
					D1=D1%fuseIndex(1,rank1-2)
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				if((D2.i.1) .ne. T1n) then
					call writemess("ERROR in ProductTensor,case 3,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				if(.not.Res%getFlag())then
					call allocateTensor(Res,newD,classtype)
					call Res%zero()
				end if
				if(T1m.eq.1)then![1,m]*[m]case
					call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,beta)
				else
					call product_MV_dim1_par(Res%TData,T1%TData,T2%TData,T1m,T1n,alpha,beta)
				end if
				return
		case (4)
				if(rank1.ge.2) then
					newD=D1.sub.[1,rank1-1]
					D1=D1%fuseIndex(1,rank1-2)
				end if
				if(rank2.ge.2) then
					newD=newD+(D2.sub.[2,rank2])
					D2=D2%fuseIndex(2,rank2)
				end if
				if((D1.i.2).ne.(D2.i.1)) then
					call writemess("ERROR in ProductTensor,case 4,stop",-1)
					call T1%diminfo('dimension of T1')
					call T2%diminfo('dimension of T2')
					call error_stop()
				end if
				T1m=D1.i.1
				T1n=D1.i.2
				T2m=D2.i.1
				T2n=D2.i.2
				if(.not.Res%getFlag())then
					call allocateTensor(Res,newD,classtype)
					call Res%zero()
				end if
				if((T1m.eq.1).and.(T2n.eq.1))then![1,m] [m,1]
					call product_VV_dim1_par(Res%TData,T1%TData,T2%TData,alpha,beta)
				else if((T1m.eq.1).and.(T2n.ne.1))then![1,m] [m,n]
					call product_VM_dim1_par(Res%TData,T1%TData,T2%TData,T2m,T2n,alpha,beta)	
				else if((T1m.ne.1).and.(T2n.eq.1))then![m,n] [n,1]
					call product_MV_dim1_parameter(Res%TData,T1%TData,T2%TData,T1m,T1n,alpha,beta,TRANSA)	
				else
					call product_MM_dim1_parameter(Res%TData,T1%TData,T2%Tdata,T1m,T1n,T2n,alpha,beta,TRANSA,TRANSB)
				end if
				return
		case default 
			write(*,*) "ERROR in ProductTensor,no such data"
			call error_stop()
		end 	select
	end subroutine 


!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  overwrite
!                          int real dble cmplx dcmplx aimag char
!
!**************************************************************************************************************
!**************************************************************************************************************
	type(Tensor) function intTensor(this)
		type(Tensor),intent(in)::this
		call allocateTensor(intTensor,this,1)
		call intTdata(intTensor%TData,this%TData)
		return
	end function
	type(Tensor) function realTensor(this)
		type(Tensor),intent(in)::this
		call allocateTensor(realTensor,this,2)
		call realTdata(realTensor%TData,this%TData)
		return
	end function
	type(Tensor) function absTensor(this)
		type(Tensor),intent(in)::this
		call allocateTensor(absTensor,this,2)
		call absTdata(absTensor%TData,this%TData)
		return
	end function
	type(Tensor) function dabsTensor(this)
		type(Tensor),intent(in)::this
		call allocateTensor(dabsTensor,this,3)
		call dabsTdata(dabsTensor%TData,this%TData)
		return
	end function
	type(Tensor) function dbleTensor(this)
		type(Tensor),intent(in)::this
		call allocateTensor(dbleTensor,this,3)
		call dbleTdata(dbleTensor%TData,this%TData)
		return
	end function
	type(Tensor) function cmplxTensor(this)
		type(Tensor),intent(in)::this
		call allocateTensor(cmplxTensor,this,4)
		call cmplxTdata(cmplxTensor%TData,this%TData)
		return
	end function
	type(Tensor) function dcmplxTensor(this)
		type(Tensor),intent(in)::this
		call allocateTensor(dcmplxTensor,this,5)
		call dcmplxTdata(dcmplxTensor%TData,this%TData)
		return
	end function
	type(Tensor) function aimagTensor(this)
		type(Tensor),intent(in)::this
		call allocateTensor(aimagTensor,this,2)
		call aimagTdata(aimagTensor%TData,this%TData)
		return
	end function
	type(Tensor) function dimagTensor(this)
		type(Tensor),intent(in)::this
		call allocateTensor(dimagTensor,this,3)
		call dimagTdata(dimagTensor%TData,this%TData)
		return
	end function
	type(Tensor) function cmplxTensor2(this1,this2)result(cmplxTensor)
		type(Tensor),intent(in)::this1,this2
		if(this1%gettype().gt.3)then
			call writemess("error in cmplx",-1)
			call error_stop()
		end if
		if(this2%getType().gt.3)then
			call writemess("error in cmplx",-1)
			call error_stop()
		end if
		if(this1%gettotalData().ne.this2%gettotalData())then
			call writemess("error in cmplx,totaldata",-1)
			call error_stop()
		end if
		call allocateTensor(cmplxTensor,this1,4)
		call cmplxTdata2(cmplxTensor%TData,this1%TData,this2%TData)
		return
	end function
	type(Tensor) function dcmplxTensor2(this1,this2)result(cmplxTensor)
		type(Tensor),intent(in)::this1,this2
		if(this1%gettype().gt.3)then
			call writemess("error in cmplx",-1)
			call error_stop()
		end if
		if(this2%getType().gt.3)then
			call writemess("error in cmplx",-1)
			call error_stop()
		end if
		if(this1%gettotalData().ne.this2%gettotalData())then
			call writemess("error in cmplx,totaldata",-1)
			call error_stop()
		end if
		call allocateTensor(cmplxTensor,this1,5)
		call dcmplxTdata2(cmplxTensor%TData,this1%TData,this2%TData)
		return
	end function
	type(Tensor) function charTensor(this)
		type(Tensor),target,intent(in)::this
		call allocateTensor(charTensor,this,7)
		if(this%getType().le.7)then
				call  charTdata(charTensor%TData,this%TData)
		else
			call writemess("ERROR in charTensor",-1)
			stop
		end if
		return
	end function

!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  max or min element in Tensor
!
!**************************************************************************************************************
!**************************************************************************************************************	

!*****************  maxElement  *****************
	integer function intmaxElement(T)
		class(Tensor),intent(in) :: T
		call intmaxTData(intmaxElement,T%TData)
		return
	end function
	real*4 function realmaxElement(T)
		class(Tensor),intent(in) :: T
		call real4_maxTData(realmaxElement,T%TData)
		return
	end function
	real*8 function dblemaxElement(T)
		class(Tensor),intent(in) :: T
		call real8_maxTData(dblemaxElement,T%TData)
		return
	end function
	complex(kind=4) function cmplxmaxElement(T)
		class(Tensor),intent(in) :: T
		call com4_maxTData(cmplxmaxElement,T%TData)
		return
	end function
	complex(kind=8) function dcmplxmaxElement(T)
		class(Tensor),intent(in) :: T
		call com8_maxTData(dcmplxmaxElement,T%TData)
		return
	end function
	type(Tensor) function TmaxElement(T)
		class(Tensor),intent(in) :: T
		select case(T%getType())
			case(1)
				TmaxElement=intmaxElement(T)
			case(2)
				TmaxElement=realmaxElement(T)
			case(3)
				TmaxElement=dblemaxElement(T)
			case(4)
				TmaxElement=cmplxmaxElement(T)
			case(5)
				TmaxElement=dcmplxmaxElement(T)
			case default
				call writemess("ERROR in type of input Tensor,(max)",-1)
				call error_stop()
		end select
		return
	end function
!*****************  minElement  *****************
	integer function intminElement(T)
		class(Tensor),intent(in) :: T
		call intminTData(intminElement,T%TData)
		return
	end function
	real*4 function realminElement(T)
		class(Tensor),intent(in) :: T
		call real4_minTData(realminElement,T%TData)
		return
	end function
	real*8 function dbleminElement(T)
		class(Tensor),intent(in) :: T
		call real8_minTData(dbleminElement,T%TData)
		return
	end function
	complex(kind=4) function cmplxminElement(T)
		class(Tensor),intent(in) :: T
		call com4_minTData(cmplxminElement,T%TData)
		return
	end function
	complex(kind=8) function dcmplxminElement(T)
		class(Tensor),intent(in) :: T
		call com8_minTData(dcmplxminElement,T%TData)
		return
	end function
	type(Tensor) function TminElement(T)
		class(Tensor),intent(in) :: T
		select case(T%getType())
			case(1)
				TminElement=intminElement(T)
			case(2)
				TminElement=realminElement(T)
			case(3)
				TminElement=dbleminElement(T)
			case(4)
				TminElement=cmplxminElement(T)
			case(5)
				TminElement=dcmplxminElement(T)
			case default
				call writemess("ERROR in type of input Tensor,(min)",-1)
				call error_stop()
		end select
		return
	end function
!*****************  maxminElement  *****************
			!maxminflag=
			!'maxa': max abs 
			!'mina': min abs 
			!'maxr': max real
			!'minr': min real
			!'maxi': 0(not com) or max imag
			!'mini': 0(not com) or min imag
	integer function intmaxminElement(T,maxminflag)result(Res)
		class(Tensor),intent(in) :: T
		character(len=4),intent(in)::maxminflag
		select case(maxminflag)
			case('maxa')
				call intmaxabsTData(Res,T%TData)
			case('mina')
				call intminabsTData(Res,T%TData)
			case('maxr')
				call intmaxrealTData(Res,T%TData)
			case('minr')
				call intminrealTData(Res,T%TData)
			case('maxi')
				call intmaximagTData(Res,T%TData)
			case('mini')
				call intminimagTData(Res,T%TData)
		end select
		return
	end function
	real(kind=4) function realmaxminElement(T,maxminflag)result(Res)
		class(Tensor),intent(in) :: T
		character(len=4),intent(in)::maxminflag
		select case(maxminflag)
			case('maxa')
				call real4_maxabsTData(Res,T%TData)
			case('mina')
				call real4_minabsTData(Res,T%TData)
			case('maxr')
				call real4_maxrealTData(Res,T%TData)
			case('minr')
				call real4_minrealTData(Res,T%TData)
			case('maxi')
				call real4_maximagTData(Res,T%TData)
			case('mini')
				call real4_minimagTData(Res,T%TData)
		end select
		return
	end function
	real(kind=8) function dblemaxminElement(T,maxminflag)result(Res)
		class(Tensor),intent(in) :: T
		character(len=4),intent(in)::maxminflag
		select case(maxminflag)
			case('maxa')
				call real8_maxabsTData(Res,T%TData)
			case('mina')
				call real8_minabsTData(Res,T%TData)
			case('maxr')
				call real8_maxrealTData(Res,T%TData)
			case('minr')
				call real8_minrealTData(Res,T%TData)
			case('maxi')
				call real8_maximagTData(Res,T%TData)
			case('mini')
				call real8_minimagTData(Res,T%TData)
		end select
		return
	end function
	complex(kind=4) function cmplxmaxminElement(T,maxminflag)result(Res)
		class(Tensor),intent(in) :: T
		character(len=4),intent(in)::maxminflag
		select case(maxminflag)
			case('maxa')
				call com4_maxabsTData(Res,T%TData)
			case('mina')
				call com4_minabsTData(Res,T%TData)
			case('maxr')
				call com4_maxrealTData(Res,T%TData)
			case('minr')
				call com4_minrealTData(Res,T%TData)
			case('maxi')
				call com4_maximagTData(Res,T%TData)
			case('mini')
				call com4_minimagTData(Res,T%TData)
		end select
		return
	end function
	complex(kind=8) function dcmplxmaxminElement(T,maxminflag)result(Res)
		class(Tensor),intent(in) :: T
		character(len=4),intent(in)::maxminflag
		select case(maxminflag)
			case('maxa')
				call com8_maxabsTData(Res,T%TData)
			case('mina')
				call com8_minabsTData(Res,T%TData)
			case('maxr')
				call com8_maxrealTData(Res,T%TData)
			case('minr')
				call com8_minrealTData(Res,T%TData)
			case('maxi')
				call com8_maximagTData(Res,T%TData)
			case('mini')
				call com8_minimagTData(Res,T%TData)
		end select
		return
	end function
	type(Tensor) function TmaxminElement(T,maxminflag)
		class(Tensor),intent(in) :: T
		character(len=4),intent(in)::maxminflag
		real*4::s
		real*8::D
		select case(T%getType())
			case(1)
				TmaxminElement=intmaxminElement(T,maxminflag)
			case(2)
				TmaxminElement=realmaxminElement(T,maxminflag)
			case(3)
				TmaxminElement=dblemaxminElement(T,maxminflag)
			case(4)
				s=cmplxmaxminElement(T,maxminflag)
				TmaxminElement=s
			case(5)
				d=dcmplxmaxminElement(T,maxminflag)
				TmaxminElement=d
			case default
				call writemess("ERROR in type of input Tensor,(maxmin)",-1)
				call error_stop()
		end select
		return
	end function
	
	
	integer function sumTensori(T)result(Res)
		class(Tensor),intent(in) :: T
		call sum_TDatai(Res,T%TData)
		return
	end function
	real*4 function sumTensors(T)result(Res)
		class(Tensor),intent(in) :: T
		call sum_TDatas(Res,T%TData)
		return
	end function
	real*8 function sumTensord(T)result(Res)
		class(Tensor),intent(in) :: T
		call sum_TDatad(Res,T%TData)
		return
	end function
	complex*8 function sumTensorc(T)result(Res)
		class(Tensor),intent(in) :: T
		call sum_TDatac(Res,T%TData)
		return
	end function
	complex*16 function sumTensorz(T)result(Res)
		class(Tensor),intent(in) :: T
		call sum_TDataz(Res,T%TData)
		return
	end function
	type(Tensor) function sumTensorT(T)result(Res)
		class(Tensor),intent(in) :: T
		integer::tempi
		real*4::temps
		real*8::tempd
		complex*8::tempc
		complex*16::tempz
		select case (T%getType())
			case (1)
				call sum_TDatai(tempi,T%TData)
				Res=tempi
			case (2)
				call sum_TDatas(temps,T%TData)
				Res=temps
			case (3)
				call sum_TDatad(tempd,T%TData)
				Res=tempd
			case (4)
				call sum_TDatac(tempc,T%TData)
				Res=tempc
			case (5)
				call sum_TDataz(tempz,T%TData)
				Res=tempz
		end select
		return
	end function
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  operaction on dimension
!
!**************************************************************************************************************
!**************************************************************************************************************	


! modify all the dimension in the Tensor
	subroutine resetdim1(Ten,dimen)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		if(product(dimen).ne.Ten%getTotaldata()) then
			call writemess("ERROR in resetdim1",-1)
			call writemess(product(dimen)+','+Ten%getTotaldata(),-1)
			call Ten%diminfo("")
			call writemess("stop",-1)
			call error_stop()
		end if
		Ten%TenDim=dimen
		Ten%rank=size(dimen)
		return
	end subroutine	
	subroutine resetdim2(Ten,dimen)
		class(Tensor),intent(inout)::Ten
		type(dimension),intent(in)::dimen
		if(dimen%size().ne.Ten%getTotaldata()) then
			call writemess("ERROR in resetdim2",-1)
			call writemess(dimen%size()+','+Ten%getTotaldata(),-1)
			call Ten%diminfo("")
			call writemess("stop",-1)
			call error_stop()
		end if
		Ten%TenDim=dimen
		Ten%rank=dimen%getRank()
		return
	end subroutine		
	subroutine reset_dim_no_check1(Ten,dimen)
		class(Tensor),intent(inout)::Ten
		type(dimension),intent(in)::dimen
		Ten%TenDim=dimen
		Ten%rank=dimen%getRank()
		call Ten%reset_total_Data_no_check(dimen%size())
		return
	end subroutine	
	subroutine reset_dim_no_check2(Ten,dimen)
		class(Tensor),intent(inout)::Ten
		integer,intent(in)::dimen(:)
		Ten%TenDim=dimen
		Ten%rank=size(dimen)
		call Ten%reset_total_Data_no_check(product(dimen))
		return
	end subroutine	
!**************** fuse   ****************
!		combine two index of the Tensor,which is con_index and con_index+1
	type(Tensor) function fuseTensor_val(T1,con_index)result(fuseTensor)
		integer,intent(in) :: con_index
		type(Tensor),intent(in) :: T1
		type(Dimension) ::newdim
		if(.not.T1%getFlag())then
			call fuseTensor%empty()
			return
		end if
		newdim=T1%TenDim%fuseIndex(con_index,1)
		call allocateTensor(fuseTensor,newdim,T1%getType())
		call assignmentTData(fuseTensor%TData,T1%TData)
		return
	end function
	type(Tensor) function fuseTensor_vec(T1,vector)result(fuseTensor)
		integer,intent(in) ::vector(2)
		type(Tensor),intent(in) :: T1
		type(Dimension) ::newdim
		integer ::num
		if(.not.T1%getFlag())then
			call fuseTensor%empty()
			return
		end if
		num=vector(2)-vector(1)
		newdim=T1%TenDim%fuseIndex(vector(1),num)
		call allocateTensor(fuseTensor,newdim,T1%getType())
		call assignmentTData(fuseTensor%TData,T1%TData)
		return
	end function		
!		combine two index of the Tensor,which is index1 index1+1,..,index2-1,index2
!		if index2 larger than rank,the function will contract the index of index1 -> rank
	subroutine fuseTensor(T1,index1,index2)
		class(Tensor),intent(inout) :: T1
		integer,intent(in) :: index1
		integer,optional,intent(in)::index2
		type(Dimension) ::newdim
		integer ::num
		if(.not.T1%getFlag())return
		if(present(index2))then
			num=index2-index1
		else
			num=1
		end if
		newdim=T1%TenDim%fuseIndex(index1,num)
		T1%rank=newDim%getRank()
		T1%TenDim=newdim
		return
	end subroutine		
!*****************  split  *****************
! decompose the de_index index of the Tensor into n(1),n(2)
!		for example the de_index index is (1,2,3,4,..inde,inde+1,...rank)
!		(1,2,3,4,..inde,inde+1,...rank)-->(1,2,3,4,..inde),(inde+1,...rank)		
!		if inde larger than rank ,the function will return no change	
	subroutine splitTensor(T1,de_index,inde)
		class(Tensor),intent(inout) :: T1
		integer,optional,intent(in) :: de_index
		integer,optional,intent(in)::inde
		type(Dimension) :: newDim
		if(.not.T1%getFlag())return
			
		if(present(de_index))then
			if(present(inde))then
				newDim=T1%TenDim%splitIndex(de_index,inde)
			else
				newDim=T1%TenDim%splitIndex(de_index,1)
			end if
		else
			newDim=T1%TenDim%splitIndex()
		end if
		T1%rank=newDim%getRank()
		T1%TenDim=newdim
		return
	end subroutine	
	type(Tensor) function splitTensor2(T1,vector)result(splitTensor)
		type(Tensor),intent(in) :: T1
		integer,intent(in) ::vector(2)
		integer:: de_index,inde
		type(Dimension) :: newDim
		if(.not.T1%getFlag())then
			call splitTensor%empty()
			return
		end if
		de_index=vector(1)
		inde=vector(2)
		newDim=T1%TenDim%splitIndex(de_index,inde)
		call allocateTensor(splitTensor,newdim,T1%getType())
		call assignmentTData(splitTensor%TData,T1%TData)
		return
	end function
			
	type(Tensor) function splitTensor3(T1,de_index)result(splitTensor)
		type(Tensor),intent(in) :: T1
		integer,intent(in) :: de_index
		type(Dimension) :: newDim
		if(.not.T1%getFlag())then
			call splitTensor%empty()
			return
		end if
		newDim=T1%TenDim%splitIndex(de_index,1)
		call allocateTensor(splitTensor,newdim,T1%getType())
		call assignmentTData(splitTensor%TData,T1%TData)
		return
	end function
			
	type(Tensor) function splitTensorAll(T1)result(splitTensor)
		type(Tensor),intent(in) :: T1
		integer:: i
		type(Dimension) :: newDim
		if(.not.T1%getFlag())then
			call splitTensor%empty()
			return
		end if
		newDim=T1%TenDim%splitIndex()
		call allocateTensor(splitTensor,newdim,T1%getType())
		call assignmentTData(splitTensor%TData,T1%TData)
		return
	end function	
!********************  dimOperations  ****************************
!		 dimOperations do the contract or decompose on the Tensor with
!	  no change on the Tensot_data in the Tensor.
!		Operar is a matrix,the first element of every row specify
!		 1:contracts
!		 2:decompose
!		 3:decomposeAll
!		other element of every row is input parameter of the function
	subroutine dimOperation2(T1,Operar)
		class(Tensor),intent(inout) :: T1
		integer,intent(in)::Operar(:,:)
		integer::i,j
		integer :: index1,index2
		integer ::num
		integer :: de_index,inde
		if(.not.T1%getflag())then
			write(*,*)"There is no data in the Tensor,(dimOperations)"
			call error_stop()
		end if
		do i=1,size(Operar,1)
			if(Operar(i,1).eq.1) then
				index1=Operar(i,2)
				index2=Operar(i,3)
				num=index2-index1
				T1%TenDim=T1%TenDim%fuseIndex(index1,num)
				T1%rank=T1%TenDim%GetRank()
			else if(Operar(i,1).eq.2) then
				de_index=Operar(i,2)
				inde=Operar(i,3)
				T1%TenDim=T1%TenDim%splitIndex(de_index,inde)
				T1%rank=T1%TenDim%GetRank()
			else if(Operar(i,1).eq.3) then
				T1%TenDim=T1%TenDim%splitIndex()
				T1%rank=T1%TenDim%GetRank()
			else
				write(*,*) "error in compose_decompse"
				call error_stop()
			end if
		end do
		return
	end subroutine
	subroutine dimOperation1(T1,Operar)
		class(Tensor),intent(inout) :: T1
		integer,intent(in)::Operar(:)
		integer::i,j
		integer :: index1,index2
		integer ::num
		integer :: de_index,inde
		if(.not.T1%getflag())then
			write(*,*)"There is no data in the Tensor,(dimOperations)"
			call error_stop()
		end if
		if(Operar(1).eq.1) then
			index1=Operar(2)
			index2=Operar(3)
			num=index2-index1
			T1%TenDim=T1%TenDim%fuseIndex(index1,num)
			T1%rank=T1%TenDim%GetRank()
		else if(Operar(1).eq.2) then
			de_index=Operar(2)
			inde=Operar(3)
			T1%TenDim=T1%TenDim%splitIndex(de_index,inde)
			T1%rank=T1%TenDim%GetRank()
		else if(Operar(1).eq.3) then
			T1%TenDim=T1%TenDim%splitIndex()
			T1%rank=T1%TenDim%GetRank()
		else
			write(*,*) "error in compose_decompse"
			call error_stop()
		end if
		return
	end subroutine
	
	

!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  permutation
!
!**************************************************************************************************************
!**************************************************************************************************************	
!ccccccccccccccccc permute_rank3   cccccccccccccccccc		
	type(Tensor) function permute_rank3(T1,index_not_permute)result(Res)
		type(Tensor),intent(in) :: T1
		integer,intent(in) ::   index_not_permute
		integer::dimen(3),lenD
		if(T1%rank.ne.3) then
			write(*,*)"ERROR in permute_rank3"
			write(*,*)"stop"
			call error_stop()
			return
		end if
		call allocateTensor(Res,T1,T1%getType())
		call permutation_data3(Res%TData,T1%TData,index_not_permute,Res%TenDim)
		select case(index_not_permute)
			case(1)
				Res%Tendim=T1%TenDim%Dimpermute((/1,3,2/))
			case(2)
				Res%Tendim=T1%TenDim%Dimpermute((/3,2,1/))
			case(3)
				Res%Tendim=T1%TenDim%Dimpermute((/2,1,3/))
		end select
		return
	 end function
!cccccccccccccccc permute_rank2   cccccccccccccccccc			 
	type(Tensor) function permute_rank2 (T)result(Res)
		type(Tensor),intent(in) :: T
		integer::dimen(2)
		if(T%rank.eq.1) then
			Res=T
			return
		end if
		if(T%rank.gt.2) then
			write(*,*)"ERROR in Dpermute_rank2"
			write(*,*)"stop"
			call error_stop()
			return
		end if
		call allocateTensor(Res,T,T%getType())
		call permutation_data2(Res%TData,T%TData,Res%TenDim)
		Res%Tendim=T%TenDim%Dimpermute((/2,1/))
		return
	end function 
!cccccccccccccccc permutation   cccccccccccccccccc
	type(Tensor) function permutation(T,newOrder)
		type(Tensor),intent(in) :: T
		integer,intent(in)::newOrder(:)
		integer,pointer ::inde(:)
		integer::lenOrder,i,j
		type(Dimension)::dimen		
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutation)",-1)
			call error_stop()
		end if
		call WorkingMemory%check('permutation')
		lenorder=size(newOrder)-1
		!allocate(inde(lenorder))
		call WorkingMemory%get_memory(inde,lenorder,'permutation')
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		permutation=T
		do i=lenorder,1,-1
			call permutefo_data_inout(permutation%TData,inde(i),permutation%TenDim)
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end function
	subroutine permutation_routine(T,newOrder)
		class(Tensor),intent(inout) :: T
		integer,intent(in)::newOrder(:)
		integer,pointer ::inde(:)
		integer::lenOrder,i,j
		type(Dimension)::dimen		
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutation)",-1)
			call error_stop()
		end if
		call WorkingMemory%check('permutation_routine')
		lenorder=size(newOrder)-1
		!allocate(inde(lenorder))
		call WorkingMemory%get_memory(inde,lenorder,'permutation_routine')
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		do i=lenorder,1,-1
			call permutefo_data_inout(T%TData,inde(i),T%TenDim)
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end subroutine
	type(Tensor) function permutation_name(T,newOrderchar)result(permutation)
		type(Tensor),intent(in) :: T
		CHARACTER(len=*),intent(in)::newOrderchar(:)
		integer,pointer::newOrder(:)
		integer,pointer ::inde(:)
		integer::lenOrder,i,j
		type(Dimension)::dimen		
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutation)",-1)
			call error_stop()
		end if
		call WorkingMemory%check('permutation_name')
		!allocate(newOrder(size(newOrderchar)))
		lenorder=size(newOrderchar)-1
		call WorkingMemory%allocate(1,lenorder+lenorder+2)
		!allocate(inde(lenorder))
		call WorkingMemory%get_memory(newOrder,lenorder+1,'permutation_name,1')
		call WorkingMemory%get_memory(inde,lenorder,'permutation_name,2')
		newOrder=T%TenDim%FindOrder(newOrderchar)
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		permutation=T
		do i=lenorder,1,-1
			call permutefo_data_inout(permutation%TData,inde(i),permutation%TenDim)
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end function
	
	type(Tensor) function permutation_Tensor(T,Order)result(permutation)
		type(Tensor),intent(in) :: T
		type(Tensor),intent(in)::Order
		integer,pointer ::inde(:)
		integer::lenOrder,i,j
		type(Dimension)::dimen		
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutation)",-1)
			call error_stop()
		end if
		call WorkingMemory%check('permutation_Tensor')
		lenOrder=Order%getTotalData()-1
		select case(Order%getType())
			case (1)
				!do i=1,lenOrder
				!	newOrder(i)=Order%ii(i)
				!end do
				allocate(inde(lenorder))
				call WorkingMemory%get_memory(inde,lenorder,'permutation_Tensor,1')
				do i=lenorder,1,-1
						inde(i)=Order%ii(i)
				end do
			case (7)
				!do i=1,lenOrder
				!	newOrder(i)=T%FindOrder(Order%ai(i))
				!end do
				allocate(inde(lenorder))
				call WorkingMemory%get_memory(inde,lenorder,'permutation_Tensor,2')
				do i=lenorder,1,-1
						inde(i)=T%FindOrder(Order%ai(i))
				end do
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()
		end select
		!lenorder=size(newOrder)-1
		!allocate(inde(lenorder))
		!do i=lenorder,1,-1
		!		inde(i)=newOrder(i)
		!end do
		permutation=T
		do i=lenorder,1,-1
			call permutefo_data_inout(permutation%TData,inde(i),permutation%TenDim)
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end function
	
	subroutine permutation_name_routine(T,newOrderchar)
		class(Tensor),intent(inout) :: T
		CHARACTER(len=*),intent(in)::newOrderchar(:)
		integer,pointer::newOrder(:)
		integer,pointer ::inde(:)
		integer::lenOrder,i,j
		type(Dimension)::dimen		
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutation)",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			call writemess("split dimension before calling permuation on name",-1)
			call error_stop()
		end if
		call WorkingMemory%check('permutation_name_routine')
		!allocate(newOrder(size(newOrderchar)))
		lenorder=size(newOrderchar)-1
		call WorkingMemory%allocate(1,lenorder+lenorder+2)
		call WorkingMemory%get_memory(newOrder,lenorder+1,'permutation_name_routine,1')
		!allocate(inde(lenorder))
		call WorkingMemory%get_memory(inde,lenorder,'permutation_name_routine,2')
		newOrder=T%TenDim%FindOrder(newOrderchar)
		do i=lenorder,1,-1
				inde(i)=newOrder(i)
		end do
		do i=lenorder,1,-1
			call permutefo_data_inout(T%TData,inde(i),T%TenDim)
			do j=1,i-1
				if(inde(j).lt.inde(i))then
					inde(j)=inde(j)+1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end subroutine
	subroutine permutation_Tensor_routine(T,Tenorder)
		class(Tensor),intent(inout)::T
		type(Tensor),intent(in)::Tenorder
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutefo_vec_name)",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			call writemess("split dimension before calling permuation on name",-1)
			call error_stop()
		end if
		select case (Tenorder%getType())
			case (1)
				call permutation_routine(T,Tenorder%ii())
			case (7)
				call permutation_name_routine(T,Tenorder%ai())
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()	
		end select
		return
	end subroutine
	
!*****************permutefo******************************
!		T_{1,2,3,..,i,..,n},permutefo(T,i)=_{i,1,2,3,..,i-1,i+1,..,n}
!
	type(Tensor) function permutefo(T,inde)
		type(Tensor),intent(in)::T
		integer,intent(in)::inde
		integer::rank
		rank=getRank(T)
		if(inde.gt.rank) then
			call writemess("ERROR in function permutefo",-1)
			call writemess(inde+','+rank,-1)
			call error_stop()
		end if
		if(inde.le.0) then
			call writemess("ERROR in function permutefo",-1)
			call writemess("index="+inde,-1)
			call error_stop()
		end if
		if(inde.eq.1) then
			permutefo=T
			return
		end if
		if(inde.eq.rank) then
			permutefo=T.fuse.(/1,rank-1/)
			permutefo=.p.permutefo
			call Dimpermute_forwards(permutefo%TenDim,T%TenDim,inde)
			permutefo%rank=permutefo%TenDim%GetRank()
			return
		end if
		permutefo=T.fuse.(/1,inde-1/)
		call permutefo%fuse(3,rank)
		permutefo=permutefo.p.3
		call Dimpermute_forwards(permutefo%TenDim,T%TenDim,inde)
		permutefo%rank=permutefo%TenDim%GetRank()
		return
	end function
	subroutine permutefo_routine(T,inde)
		class(Tensor),intent(inout)::T
		integer,intent(in)::inde
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutefo_vec)",-1)
			call error_stop()
		end if
		call permutefo_data_inout(T%TData,inde,T%TenDim)
		return
	end subroutine
	type(Tensor) function permutefo_name(T,indechar)result(permutefo)
		type(Tensor),intent(in)::T
		character(len=*),intent(in)::indechar
		integer::inde
		integer::rank
		rank=getRank(T)
		if(long_Name_logi(indechar))then
			inde=T%TenDim%FindOrder(indechar)
			if(inde.gt.rank) then
				call writemess("ERROR in function permutefo",-1)
				call writemess(inde+','+rank,-1)
				call error_stop()
			end if
			if(inde.le.0) then
				call writemess("ERROR in function permutefo_name,Can not find the name",-1)
				call writemess("stop",-1)
				call error_stop()
			end if
			if(.not.if_original_dim(T%Tendim))then
				call writemess("split dimension before calling permuation on name",-1)
				call error_stop()
			end if
			if(inde.eq.1) then
				permutefo=T
				return
			end if
			if(inde.eq.rank) then
				permutefo=T.fuse.(/1,rank-1/)
				permutefo=.p.permutefo
				call Dimpermute_forwards(permutefo%TenDim,T%TenDim,inde)
				permutefo%rank=permutefo%TenDim%GetRank()
				return
			end if
			permutefo=T.fuse.(/1,inde-1/)
			call permutefo%fuse(3,rank)
			permutefo=permutefo.p.3
			call Dimpermute_forwards(permutefo%TenDim,T%TenDim,inde)
			permutefo%rank=permutefo%TenDim%GetRank()
			return
		else
			permutefo=T
			call permutefo_name_routine(permutefo,indechar)
			return
		end if
	end function
	subroutine permutefo_name_routine(T,indechar)
		class(Tensor),intent(inout)::T
		integer::inde,i
		character(len=len_of_Name)::indexname
		character(len=*),intent(in)::indechar
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutefo_vec)",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			call writemess("split dimension before calling permuation on name",-1)
			call error_stop()
		end if
		if(long_Name_logi(indechar))then
			inde=T%TenDim%FindOrder(indechar)
			call permutefo_data_inout(T%TData,inde,T%TenDim)
		else
			i=T%getRank()
			do inde=T%getRank(),1,-1
				indexname=T%outTensorName(i)
				if(indechar.equ.indexname)then
					call permutefo_data_inout(T%TData,i,T%TenDim)
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
	type(Tensor) function permutefo_vec(T,vec_)
		type(Tensor),intent(in)::T
		integer,intent(in)::vec_(:)
		integer::lenVec,i,j
		integer::vec(size(vec_))
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutefo_vec)",-1)
			call error_stop()
		end if
		lenVec=size(vec_)
		vec=vec_
		permutefo_vec=T
		do i=lenVec,1,-1
			call permutefo_data_inout(permutefo_vec%TData,vec(i),permutefo_vec%TenDim)
			do j=1,i-1
				if(vec(j).lt.vec(i))then
					vec(j)=vec(j)+1
				end if
			end do
		end do
		return
	end function
	subroutine permutefo_vec_routine(T,vec_)
		class(Tensor),intent(inout)::T
		integer,intent(in)::vec_(:)
		integer::lenVec,i,j
		integer,pointer::vec(:)
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutefo_vec)",-1)
			call error_stop()
		end if
		call WorkingMemory%check('permutefo_vec_routine')
		lenVec=size(vec_)
		call WorkingMemory%get_memory(vec,lenVec,'permutefo_vec_routine')
		vec=vec_
		do i=lenVec,1,-1
			call permutefo_data_inout(T%TData,vec(i),T%TenDim)
			do j=1,i-1
				if(vec(j).lt.vec(i))then
					vec(j)=vec(j)+1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end subroutine
	type(Tensor) function permutefo_vec_name(T,indechar)result(permutefo_vec)
		type(Tensor),intent(in)::T
		character(len=*),intent(in)::indechar(:)
		integer::lenVec,i,j
		integer,pointer::vec(:)
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutefo_vec_name)",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			call writemess("split dimension before calling permuation on name",-1)
			call error_stop()
		end if
		call WorkingMemory%check('permutefo_vec_name')
		lenVec=size(indechar)
		call WorkingMemory%get_memory(vec,lenVec,'permutefo_vec_name')
		vec=T%TenDim%FindOrder(indechar)
		permutefo_vec=T
		do i=lenVec,1,-1
			call permutefo_data_inout(permutefo_vec%TData,vec(i),permutefo_vec%TenDim)
			do j=1,i-1
				if(vec(j).lt.vec(i))then
					vec(j)=vec(j)+1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end function
	
	type(Tensor) function permutefo_Tensor(T,Tenorder)result(permutefo)
		type(Tensor),intent(in)::T
		type(Tensor),intent(in)::Tenorder
		character(len=characterlen),pointer::indechar(:)
		integer,pointer::indeint(:)
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutefo_vec_name)",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			call writemess("split dimension before calling permuation on name",-1)
			call error_stop()
		end if
		call WorkingMemory%check('permutefo_Tensor')
		select case (Tenorder%getType())
			case (1)
				!allocate(indeint(Tenorder%getTotalData()))
				call WorkingMemory%get_memory(indeint,Tenorder%getTotalData(),'permutefo_Tensor,1')
				indeint=Tenorder
				permutefo=permutefo_vec(T,indeint)
			case (7)
				!allocate(indechar(Tenorder%getTotalData()))
				call WorkingMemory%get_memory(indechar,Tenorder%getTotalData(),'permutefo_Tensor,2')
				indechar=Tenorder
				permutefo=permutefo_vec_name(T,indechar)
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()	
		end select
		call WorkingMemory%free()
		return
	end function
	subroutine permutefo_vec_name_routine(T,indechar)
		class(Tensor),intent(inout)::T
		character(len=*),intent(in)::indechar(:)
		integer::lenVec,i,j
		integer,pointer::vec(:)
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutefo_vec_name)",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			call writemess("split dimension before calling permuation on name",-1)
			call error_stop()
		end if
		call WorkingMemory%check('permutefo_vec_name_routine')
		lenVec=size(indechar)
		call WorkingMemory%get_memory(vec,lenVec,'permutefo_vec_name_routine')
		vec=T%TenDim%FindOrder(indechar)
		do i=lenVec,1,-1
			call permutefo_data_inout(T%TData,vec(i),T%TenDim)
			do j=1,i-1
				if(vec(j).lt.vec(i))then
					vec(j)=vec(j)+1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end subroutine
	
	subroutine permutefo_Tensor_routine(T,Tenorder)
		class(Tensor),intent(inout)::T
		type(Tensor),intent(in)::Tenorder
		character(len=max_len_of_char_in_TData),pointer::indechar(:)
		integer,pointer::indeint(:)
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permutefo_vec_name)",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			call writemess("split dimension before calling permuation on name",-1)
			call error_stop()
		end if
		call WorkingMemory%check('permutefo_Tensor_routine')
		select case (Tenorder%getType())
			case (1)
				!allocate(indeint(Tenorder%getTotalData()))
				call WorkingMemory%get_memory(indeint,Tenorder%getTotalData(),'permutefo_Tensor_routine')
				indeint=Tenorder
				call permutefo_vec_routine(T,indeint)
			case (7)
				!allocate(indechar(Tenorder%getTotalData()))
				call WorkingMemory%get_memory(indechar,Tenorder%getTotalData(),'permutefo_Tensor_routine')
				indechar=Tenorder
				call permutefo_vec_name_routine(T,indechar)
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()	
		end select
		call WorkingMemory%free()
		return
	end subroutine
!*****************permuteback******************************
!		T_{1,2,3,..,i,..,n},permuteback(T,i)=_{1,2,3,..,i-1,i+1,..,n,i}
!
	type(Tensor) function permuteback(T,inde)
		type(Tensor),intent(in)::T
		integer,intent(in)::inde
		integer::rank
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permuteback)",-1)
			call error_stop()
		end if
		rank=getRank(T)
		if(inde.gt.rank) then
			call writemess("ERROR in function permuteback",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		if(inde.le.0) then
			call writemess("ERROR in function permuteback,Can not find the name",-1)
			call writemess("index"+'='+inde,-1)
			call error_stop()
		end if
		if(inde.eq.rank) then
			permuteback=T
			return
		end if
		if(inde.eq.1) then
			permuteback=T.fuse.(/2,rank/)
			permuteback=.p.permuteback
			call Dimpermute_backwards(permuteback%TenDim,T%TenDim,inde)
			permuteback%rank=permuteback%TenDim%GetRank()
			return
		end if
		permuteback=T.fuse.(/1,inde-1/)
		call permuteback%fuse(3,rank)
		permuteback=permuteback.p.1
		call Dimpermute_backwards(permuteback%TenDim,T%TenDim,inde)
		permuteback%rank=permuteback%TenDim%GetRank()
		return
	end function
	subroutine permuteback_routine(T,inde)
		class(Tensor),intent(inout)::T
		integer,intent(in)::inde
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permuteback_vec)",-1)
			call error_stop()
		end if
		call permuteback_data_inout(T%TData,inde,T%TenDim)
		return
	end subroutine
	subroutine permuteback_name_routine(T,indechar)
		class(Tensor),intent(inout)::T
		character(len=*),intent(in)::indechar
		integer::inde,i
		character(len=len_of_Name)::indexname
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permuteback_vec)",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			call writemess("split dimension before calling permuation on name",-1)
			call error_stop()
		end if
		if(long_Name_logi(indechar))then
			inde=T%TenDim%FindOrder(indechar)
			call permuteback_data_inout(T%TData,inde,T%TenDim)
		else
			i=1
			do inde=1,T%getRank()
				indexname=T%outTensorName(i)
				if(indechar.equ.indexname)then
					call permuteback_data_inout(T%TData,i,T%TenDim)
				else
					i=i+1
				end if
			end do
		end if
		return
	end subroutine
	type(Tensor) function permuteback_name(T,indechar)result(permuteback)
		type(Tensor),intent(in)::T
		character(len=*),intent(in)::indechar
		integer::inde
		integer::rank
		if(.not.T%getflag())then
			call writemess("There is no data in the Tensor,(permuteback_name)",-1)
			call error_stop()
		end if
		if(long_Name_logi(indechar))then
			if(.not.if_original_dim(T%Tendim))then
				call writemess("split dimension before calling permuation on name",-1)
				call error_stop()
			end if
			rank=getRank(T)
			inde=T%TenDim%FindOrder(indechar)
			if(inde.gt.rank) then
				call writemess("ERROR in function permuteback_name",-1)
				call writemess("stop",-1)
				call error_stop()
			end if
			if(inde.le.0) then
				call writemess("ERROR in function permuteback_name,Can not find the name",-1)
				call writemess("stop",-1)
				call error_stop()
			end if
			if(inde.eq.rank) then
				permuteback=T
				return
			end if
			if(inde.eq.1) then
				permuteback=T.fuse.(/2,rank/)
				permuteback=.p.permuteback
				call dimoperation(permuteback,(/3/))
				return
			end if
			permuteback=T.fuse.(/1,inde-1/)
			call permuteback%fuse(3,rank)
			permuteback=permuteback.p.1
			call dimoperation(permuteback,(/3/))
			return
		else
			permuteback=T
			call permuteback_name_routine(permuteback,indechar)
			return
		end if
		
	end function
!*****************permuteback******************************
!		T_{1,2,3,..,i,..,n},permuteback(T,i)=_{1,2,3,..,i-1,i+1,..,n,i}
!
	type(Tensor) function permuteback_vec(T,vec_)
		type(Tensor),intent(in)::T
		integer,intent(in)::vec_(:)
		integer::rank,lenVec,i,j
		integer,pointer::vec(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permuteback_vec)"
			call error_stop()
		end if
		call WorkingMemory%check('permuteback_vec')
		rank=getRank(T)
		lenVec=size(vec_)
		call WorkingMemory%get_memory(vec,lenVec,'permuteback_vec')
		vec=vec_
		permuteback_vec=T
		do i=1,lenVec
			call permuteback_data_inout(permuteback_vec%TData,vec(i),permuteback_vec%TenDim)
			do j=lenVec,i+1,-1
				if(vec(j).gt.vec(i))then
					vec(j)=vec(j)-1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end function
	type(Tensor) function permuteback_vec_name(T,indechar)result(permuteback_vec)
		type(Tensor),intent(in)::T
		character(len=*),intent(in)::indechar(:)
		integer::rank,lenVec,i,j
		integer,pointer::vec(:)
		type(Dimension)::dimen		
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permuteback_vec_name)"
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			write(*,*)"split dimension before calling permuation on name"
			call error_stop()
		end if
		call WorkingMemory%check('permuteback_vec_name')
		rank=getRank(T)
		lenVec=size(indechar)
		call WorkingMemory%get_memory(vec,lenVec,'permuteback_vec_name')
		dimen=T%TenDim
		vec=T%TenDim%FindOrder(indechar)
		permuteback_vec=T
		do i=1,lenVec
			call permuteback_data_inout(permuteback_vec%TData,vec(i),permuteback_vec%TenDim)
			do j=lenVec,i+1,-1
				if(vec(j).gt.vec(i))then
					vec(j)=vec(j)-1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end function
	
	type(Tensor) function permuteback_Tensor(T,Tenorder)result(permutefo)
		type(Tensor),intent(in)::T
		type(Tensor),intent(in)::Tenorder
		character(len=characterlen),pointer::indechar(:)
		integer,pointer::indeint(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutefo_vec_name)"
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			write(*,*)"split dimension before calling permuation on name"
			call error_stop()
		end if
		call WorkingMemory%check('permuteback_Tensor')
		select case (Tenorder%getType())
			case (1)
				!allocate(indeint(Tenorder%getTotalData()))
				call WorkingMemory%get_memory(indeint,Tenorder%getTotalData(),'permuteback_Tensor')
				indeint=Tenorder
				permutefo=permuteback_vec(T,indeint)
			case (7)
				!allocate(indechar(Tenorder%getTotalData()))
				call WorkingMemory%get_memory(indechar,Tenorder%getTotalData(),'permuteback_Tensor')
				indechar=Tenorder
				permutefo=permuteback_vec_name(T,indechar)
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()	
		end select
		call WorkingMemory%free()
		return
	end function
	
	subroutine permuteback_vec_routine(T,vec_)
		class(Tensor),intent(inout)::T
		integer,intent(in)::vec_(:)
		integer::rank,lenVec,i,j
		integer,pointer::vec(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permuteback_vec)"
			call error_stop()
		end if
		call WorkingMemory%check('permuteback_vec_routine')
		rank=getRank(T)
		lenVec=size(vec_)
		call WorkingMemory%get_memory(vec,lenVec,'permuteback_vec_routine')
		vec=vec_
		do i=1,lenVec
			call permuteback_data_inout(T%TData,vec(i),T%TenDim)
			do j=lenVec,i+1,-1
				if(vec(j).gt.vec(i))then
					vec(j)=vec(j)-1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end subroutine
	subroutine permuteback_vec_name_routine(T,indechar)
		class(Tensor),intent(inout)::T
		character(len=*),intent(in)::indechar(:)
		integer::rank,lenVec,i,j
		integer,pointer::vec(:)
		type(Dimension)::dimen		
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permuteback_vec_name)"
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			write(*,*)"split dimension before calling permuation on name"
			call error_stop()
		end if
		call WorkingMemory%check('permuteback_vec_name_routine')
		rank=getRank(T)
		lenVec=size(indechar)
		call WorkingMemory%get_memory(vec,lenVec,'permuteback_vec_name_routine')
		dimen=T%TenDim
		vec=T%TenDim%FindOrder(indechar)
		do i=1,lenVec
			call permuteback_data_inout(T%TData,vec(i),T%TenDim)
			do j=lenVec,i+1,-1
				if(vec(j).gt.vec(i))then
					vec(j)=vec(j)-1
				end if
			end do
		end do
		call WorkingMemory%free()
		return
	end subroutine
	
	subroutine permuteback_Tensor_routine(T,Tenorder)
		class(Tensor),intent(inout)::T
		type(Tensor),intent(in)::Tenorder
		character(len=characterlen),pointer::indechar(:)
		integer,pointer::indeint(:)
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutefo_vec_name)"
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			write(*,*)"split dimension before calling permuation on name"
			call error_stop()
		end if
		call WorkingMemory%check('permuteback_Tensor_routine')
		select case (Tenorder%getType())
			case (1)
				!allocate(indeint(Tenorder%getTotalData()))
				call WorkingMemory%get_memory(indeint,Tenorder%getTotalData(),'permuteback_Tensor_routine')
				indeint=Tenorder
				call permuteback_vec_routine(T,indeint)
			case (7)
				!allocate(indechar(Tenorder%getTotalData()))
				call WorkingMemory%get_memory(indechar,Tenorder%getTotalData(),'permuteback_Tensor_routine')
				indechar=Tenorder
				call permuteback_vec_name_routine(T,indechar)
			case default
				call writemess('error in permutation, the data type of order',-1)
				call error_Stop()	
		end select
		call WorkingMemory%free()
		return
	end subroutine
!****************** permuteInde**************************
!		T_{1,2,3,..,i,..,n},permuteInde(T,i)=_{2,3,..,i,1,i+1,..,n}
!
	type(Tensor) function permuteInde(T,inde)
		type(Tensor),intent(in)::T
		integer,intent(in)::inde
		integer::rank
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permuteInde)"
			call error_stop()
		end if
		rank=getRank(T)
		if(inde.gt.rank) then
			write(*,*)"ERROR in function permuteInde",inde,rank
			write(*,*)"stop"
			call error_stop()
		end if
		if(inde.le.0) then
			write(*,*)"ERROR in function permuteInde"
			write(*,*)"index",inde
			call error_stop()
		end if
		if(inde.eq.1) then
			permuteInde=T
			return
		end if
		if(inde.eq.rank) then
			permuteInde=T.fuse.(/2,rank/)
			permuteInde=.p.permuteInde
			call Dimpermute_forwards_index(permuteInde%TenDim,T%Tendim,inde)
			permuteInde%rank=permuteInde%TenDim%GetRank()
			return
		end if
		permuteInde=T.fuse.(/2,inde/)
		call permuteInde%fuse(3,rank)
		permuteInde=permuteInde.p.3
		call Dimpermute_forwards_index(permuteInde%TenDim,T%Tendim,inde)
		permuteInde%rank=permuteInde%TenDim%GetRank()
		return
	end function
	type(Tensor) function permuteInde_name(T,w)result(permuteInde)
		type(Tensor),intent(in)::T
		character(len=*),intent(in)::w
		integer::inde
		integer::rank
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permuteInde)"
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			write(*,*)"split dimension before calling permuation on name"
			call error_stop()
		end if
		rank=getRank(T)
		inde=T%TenDim%FindOrder(w)
		if(inde.gt.rank) then
			write(*,*)"ERROR in function permuteInde",inde,rank
			write(*,*)"stop"
			call error_stop()
		end if
		if(inde.le.0) then
			write(*,*)"ERROR in function permuteInde"
			write(*,*)"index",inde
			call error_stop()
		end if
		if(inde.eq.1) then
			permuteInde=T
			return
		end if
		if(inde.eq.rank) then
			permuteInde=T.fuse.(/2,rank/)
			permuteInde=.p.permuteInde
			call Dimpermute_forwards_index(permuteInde%TenDim,T%TenDim,inde)
			permuteInde%rank=permuteInde%TenDim%GetRank()
!			permuteInde%TenDim=permuteInde%TenDim%splitIndex()
!			permuteInde%rank=permuteInde%TenDim%GetRank()
			return
		end if
		permuteInde=T.fuse.(/2,inde/)
		call permuteInde%fuse(3,rank)
		permuteInde=permuteInde.p.3
		call Dimpermute_forwards_index(permuteInde%TenDim,T%TenDim,inde)
		permuteInde%rank=permuteInde%TenDim%GetRank()
	!	permuteInde%TenDim=permuteInde%TenDim%splitIndex()
	!	permuteInde%rank=permuteInde%TenDim%GetRank()
		return
	end function
!****************** permutebackInde**************************
!		T_{1,2,3,..,i,..,n},permutebackInde(T,i)=_{1,2,3,..,i-1,n,i,i+1,..,n-1}
!
	type(Tensor) function permutebackInde(T,inde)
		type(Tensor),intent(in)::T
		integer,intent(in)::inde
		integer::rank
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutebackInde)"
			call error_stop()
		end if
		rank=getRank(T)
		if(inde.gt.rank) then
			write(*,*)"ERROR in function permutebackInde",inde,rank
			write(*,*)"stop"
			call error_stop()
		end if
		if(inde.le.0) then
			write(*,*)"ERROR in function permutebackInde"
			write(*,*)"index",inde
			call error_stop()
		end if
		if(inde.eq.rank) then
			permutebackInde=T
			return
		end if
		if(inde.eq.1) then
			permutebackInde=T.fuse.(/1,rank-1/)
			permutebackInde=.p.permutebackInde
			call Dimpermute_backwards_index(permutebackInde%TenDim,T%TenDim,inde)
			permutebackInde%rank=permutebackInde%TenDim%GetRank()
			return
		end if
		permutebackInde=T.fuse.(/1,inde-1/)
		call permutebackInde%fuse(2,rank-inde+1)
		permutebackInde=permutebackInde.p.1
		call Dimpermute_backwards_index(permutebackInde%TenDim,T%TenDim,inde)
		permutebackInde%rank=permutebackInde%TenDim%GetRank()
		return
	end function
	type(Tensor) function permutebackInde_name(T,w)result(permutebackInde)
		type(Tensor),intent(in)::T
		character(len=*),intent(in)::w
		integer::inde
		integer::rank
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(permutebackInde)"
			call error_stop()
		end if
		if(.not.if_original_dim(T%Tendim))then
			write(*,*)"split dimension before calling permuation on name"
			call error_stop()
		end if
		rank=getRank(T)
		inde=T%TenDim%FindOrder(w)
		if(inde.gt.rank) then
			write(*,*)"ERROR in function permutebackInde",inde,rank
			write(*,*)"stop"
			call error_stop()
		end if
		if(inde.le.0) then
			write(*,*)"ERROR in function permutebackInde"
			write(*,*)"index",inde
			call error_stop()
		end if
		if(inde.eq.rank) then
			permutebackInde=T
			return
		end if
		if(inde.eq.1) then
			permutebackInde=T.fuse.(/1,rank-1/)
			permutebackInde=.p.permutebackInde
			call Dimpermute_backwards_index(permutebackInde%TenDim,T%TenDim,inde)
			permutebackInde%rank=permutebackInde%TenDim%GetRank()
			return
		end if
		permutebackInde=T.fuse.(/1,inde-1/)
		call permutebackInde%fuse(2,rank-inde+1)
		permutebackInde=permutebackInde.p.1
		call Dimpermute_backwards_index(permutebackInde%TenDim,T%TenDim,inde)
		permutebackInde%rank=permutebackInde%TenDim%GetRank()
		return
	end function
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
	type(Tensor) function contract_noName(T1_,i1,T2_,i2,len_of_contract) result(T)
		type(Tensor),intent(in) :: T1_,T2_
		integer,intent(in) :: i1(:),i2(:)
		integer,optional,intent(in)::len_of_contract(2)
		type(Tensor),pointer :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		T1=>WorkingTensor1
		T2=>WorkingTensor2
		!if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
		!	write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
		!	write(*,*)"stop"
		!	call error_stop()
		!end if
		rank1=T1_%rank
		rank2=T2_%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		T1=T1_.pb.i1(1:leni1)
		call dimoperation(T1,(/1,rank1-leni1+1,rank1/))
		T2=T2_.pf.i2(1:leni2)
		call dimoperation(T2,(/1,1,leni2/))
		call ProductTensorRoutine2(T,T1,T2,1) 
		return
	end function
	type(Tensor) function contract_noName2(T1_,i1,T2_,i2) result(T)
		type(Tensor),intent(in) :: T1_,T2_
		integer,intent(in) :: i1,i2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		!if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
		!	write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
		!	write(*,*)"stop"
		!	call error_stop()
		!end if
		call ProductTensorRoutine2(T, (T1_.pb.i1) , (T2_.pf.i2) ,1 )
		return
	end function
	type(Tensor) function contract_name(T1_,name1,T2_,name2,len_of_contract) result(T)
		type(Tensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		type(Tensor),pointer :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		T1=>WorkingTensor1
		T2=>WorkingTensor2
		i1=T1_%TenDim%FindOrder(name1)
		i2=T2_%TenDim%FindOrder(name2)
		rank1=T1_%rank
		rank2=T2_%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		T1=T1_.pb.i1(1:leni1)
		call dimoperation(T1,(/1,rank1-leni1+1,rank1/))
		T2=T2_.pf.i2(1:leni2)
		call dimoperation(T2,(/1,1,leni2/))
		call ProductTensorRoutine2(T, T1 , T2, 1)
		return
	end function
	type(Tensor) function contract_name2(T1_,name1,T2_,name2) result(T)
		type(Tensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name1,name2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		call ProductTensorRoutine2(T, (T1_.pb.name1) , (T2_.pf.name2) , 1)
		return
	end function
	
	
!******************  contract the same names  *********************	

	subroutine find_same_name(inname1,inname2,SameName,lenofname)
		character(len=*),intent(in)::inname1(:),inname2(:)
		character(len=*),intent(inout)::SameName(:)
		integer,intent(inout)::lenofname
		integer::i,j,k,sizeSameName
		k=0
		lenofname=0
		sizeSameName=size(SameName)
		do i=1,size(inname1)
			do j=1,size(inname2)
				if(inname1(i).equ.inname2(j))then
					k=k+1
					if(k.gt.sizeSameName)then
						call writemess('ERROR in finding the same name, maybe something wrong in the tensorname',-1)
						call writemess('You can diminfo to see if the tensor names are right')
						call writemess('It is not allow to have two or more same name in one tensor')
						call error_stop()
					end if
					SameName(k)=inname1(i)
					lenofname=lenofname+1
				end if
			end do
		end do
		return
	end subroutine
	
	type(Tensor) function contract_Same_name(T1_,T2_) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		type(Tensor),pointer::T1,T2
		character(len=len_of_Name+len_of_Name),allocatable::Samename(:),name1(:),name2(:)
		integer::lenofname,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		T1=>WorkingTensor1
		T2=>WorkingTensor2
		rank1=T1_%getRank()
		rank2=T2_%getRank()
		allocate(name1(rank1))
		allocate(name2(rank2))
		allocate(Samename(max(rank1,rank2) ))
		name1=.iname.T1_
		name2=.iname.T2_
		call find_same_name(name1,name2,SameName,lenofname) 
		T1=T1_.pb.SameName(1:lenofname)
		call dimoperation(T1,(/1,rank1-lenofname+1,rank1/))
		T2=T2_.pf.SameName(1:lenofname)
		call dimoperation(T2,(/1,1,lenofname/))
		call ProductTensorRoutine2(T, T1 , T2 , 1)
		return
	end function
	
	
	
	type(Tensor) function contract_Intname(T1_,name1,Nimname1,T2_,name2,Nimname2) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		integer,intent(in)::name1(:,:),name2(:,:),Nimname1(:,:),Nimname2(:,:)
		integer :: i1(size(name1,1)),i2(size(name2,1))
		type(Tensor) :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		i1=T1_%TenDim%NameOrder(name1,Nimname1)
		i2=T2_%TenDim%NameOrder(name2,Nimname2)
		rank1=T1_%rank
		rank2=T2_%rank
		leni1=size(i1)
		leni2=size(i2)
		T1=T1_.pb.i1
		call dimoperation(T1,(/1,rank1-leni1+1,rank1/))
		T2=T2_.pf.i2
		call dimoperation(T2,(/1,1,leni2/))
		call ProductTensorRoutine2(T, T1 , T2 , 1)
		return
	end function
	type(Tensor) function contract_Intname2(T1_,name1,Nimname1,T2_,name2,Nimname2) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		integer,intent(in)::name1(:),name2(:),Nimname1(:),Nimname2(:)
		integer :: i1,i2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		i1=T1_%TenDim%NameOrder(name1,Nimname1)
		i2=T2_%TenDim%NameOrder(name2,Nimname2)
		call ProductTensorRoutine2(T, (T1_.pb.i1) , (T2_.pf.i2) , 1 )
		return
	end function
	type(Tensor) function contract_int_name(T1_,i1,T2_,name2,len_of_contract) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name2(:)
		integer,intent(in)::i1(:)
		integer,optional,intent(in)::len_of_contract(2)
		integer :: i2(size(name2))
		type(Tensor),pointer :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T2_%TenDim)) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		T1=>WorkingTensor1
		T2=>WorkingTensor2
		i2=T2_%TenDim%FindOrder(name2)
		rank1=T1_%rank
		rank2=T2_%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		T1=T1_.pb.i1(1:leni1)
		call dimoperation(T1,(/1,rank1-leni1+1,rank1/))
		T2=T2_.pf.i2(1:leni2)
		call dimoperation(T2,(/1,1,leni2/))
		call ProductTensorRoutine2(T, T1 , T2 ,1)
		return
	end function
	type(Tensor) function contract_int_name2(T1_,i1,T2_,name2) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name2
		integer,intent(in)::i1
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T2_%TenDim)) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		call ProductTensorRoutine2(T, (T1_.pb.i1) , (T2_.pf.name2) , 1 )
		return
	end function
	type(Tensor) function contract_int_intname(T1_,i1,T2_,name2,DimName2) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		integer,intent(in)::i1(:),name2(:,:),DimName2(:,:)
		integer :: i2(size(name2,1))
		type(Tensor) :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		i2=T2_%TenDim%NameOrder(name2,DimName2)
		rank1=T1_%rank
		rank2=T2_%rank
		leni1=size(i1)
		leni2=size(i2)
		T1=T1_.pb.i1
		call dimoperation(T1,(/1,rank1-leni1+1,rank1/))
		T2=T2_.pf.i2
		call dimoperation(T2,(/1,1,leni2/))
		call ProductTensorRoutine2(T, T1 , T2 , 1)
		return
	end function
	type(Tensor) function contract_int_intname2(T1_,i1,T2_,name2,DimName2) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		integer,intent(in)::i1,name2(:),DimName2(:)
		integer :: i2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		i2=T2_%TenDim%NameOrder(name2,DimName2)
		call ProductTensorRoutine2(T,  (T1_.pb.i1) , (T2_.pf.i2) , 1 )
		return
	end function
	type(Tensor) function contract_name_int(T1_,name1,T2_,i2,len_of_contract) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name1(:)
		integer,intent(in)::i2(:)
		integer :: i1(size(name1))
		integer,optional,intent(in)::len_of_contract(2)
		type(Tensor),pointer :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T1_%TenDim)) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		T1=>WorkingTensor1
		T2=>WorkingTensor2
		i1=T1_%TenDim%FindOrder(name1)
		rank1=T1_%rank
		rank2=T2_%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		T1=T1_.pb.i1(1:leni1)
		call dimoperation(T1,(/1,rank1-leni1+1,rank1/))
		T2=T2_.pf.i2(1:leni2)
		call dimoperation(T2,(/1,1,leni2/))
		call ProductTensorRoutine2(T, T1 , T2 , 1 )
		return
	end function
	type(Tensor) function contract_name_int2(T1_,name1,T2_,i2) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		character(len=*),intent(in)::name1
		integer,intent(in)::i2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.if_original_dim(T1_%TenDim)) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		call ProductTensorRoutine2(T, (T1_.pb.name1) , (T2_.pf.i2) , 1 )
		return
	end function
	type(Tensor) function contract_intname_int(T1_,name1,DimName1,T2_,i2) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		integer,intent(in)::i2(:),name1(:,:),DimName1(:,:)
		integer :: i1(size(name1,1))
		type(Tensor) :: T1,T2
		integer::leni1,leni2,rank1,rank2
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		i1=T1_%TenDim%NameOrder(name1,DimName1)
		rank1=T1_%rank
		rank2=T2_%rank
		leni1=size(i1)
		leni2=size(i2)
		T1=T1_.pb.i1
		call dimoperation(T1,(/1,rank1-leni1+1,rank1/))
		T2=T2_.pf.i2
		call dimoperation(T2,(/1,1,leni2/))
		call ProductTensorRoutine2(T, T1 , T2 , 1 )
		return
	end function
	type(Tensor) function contract_intname_int2(T1_,name1,DimName1,T2_,i2) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		integer,intent(in)::i2,name1(:),DimName1(:)
		integer :: i1
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		i1=T1_%TenDim%NameOrder(name1,DimName1)
		call ProductTensorRoutine2(T, (T1_.pb.i1) , (T2_.pf.i2) , 1 )
		return
	end function
	type(Tensor) function contract_old(T1_,T2_,i1_,i2_) result(T)
		class(Tensor),intent(in) :: T1_,T2_
		integer,intent(in) :: i1_(:),i2_(:)
		integer :: i,j,k,D2,decompoD1,decompoD2!Di use for decompese
		type(Tensor) :: T1,T2
		integer :: decompoD1keep,decompoD2keep,rank1,&
			Tdim1(T1_%rank),Tdim2(T2_%rank),rank2,oper(2,3)
		integer::leni1,leni2,i1(size(i1_)),i2(size(i2_))
		if(.not.T1_%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2_%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1_%TenDim).and.if_original_dim(T2_%TenDim))) then
			write(*,*)"ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function"
			write(*,*)"stop"
			call error_stop()
		end if
		rank1=T1_%rank
		rank2=T2_%rank
		leni1=size(i1)
		leni2=size(i2)
		T1=T1_.pb.i1_(1)
		i1(1)=i1_(1)
		do j=2,leni1
			if(i1(1).gt.i1_(j)) then
				i1(j)=i1_(j)
			else
				i1(j)=i1_(j)-1
			end if
		end do
		
		T2=T2_.pf.i2_(leni2)
		i2(leni2)=i2_(leni2)
		do j=leni2-1,1,-1
			if(i2(leni2).gt.i2_(j)) then
				i2(j)=i2_(j)+1
			else
				i2(j)=i2_(j)
			end if
		end do
		!T1
		do i=2,leni1
		
			T1=T1.pb.i1(i)
			
			do j=i+1,leni1
				if(i1(i).gt.i1(j)) then
					i1(j)=i1(j)
				else
					i1(j)=i1(j)-1
				end if
			end do
			
		end do
		call dimoperation(T1,(/1,rank1-leni1+1,rank1/))
		!T2
		do i=leni2-1,1,-1
			T2=T2.pf.i2(i)
			
			do j=i-1,1,-1
				if(i2(i).gt.i2(j)) then
					i2(j)=i2(j)+1
				else
					i2(j)=i2(j)
				end if
			end do
		
		end do
		call dimoperation(T2,(/1,1,leni2/))
		call ProductTensorRoutine2(T, T1 , T2 , 1 )
		return
	end function

	subroutine contract_name_routine(T,T1,name1,T2,name2,len_of_contract) 
		class(Tensor),target::T
		class(Tensor),target :: T1,T2
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2
		class(Tensor),pointer::pT,pT1,pT2
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1%TenDim).and.if_original_dim(T2%TenDim))) then
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
		i1=T1%TenDim%FindOrder(name1)
		i2=T2%TenDim%FindOrder(name2)
		rank1=T1%rank
		rank2=T2%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call T1%backward(i1(1:leni1))
		call dimoperation(T1,(/1,rank1-leni1+1,rank1/))
		call T2%forward(i2(1:leni2))
		call dimoperation(T2,(/1,1,leni2/))
		call T%ProductTensorRoutine(T1 , T2 , 1 )
		call T1%split( )
		call T2%split( )
		return
	end subroutine
	subroutine contract_name_routine1(T,name1,T2,name2,len_of_contract) 
		class(Tensor),target::T
		class(Tensor),target :: T2
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2
		class(Tensor),pointer::pT,pT2
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T%TenDim).and.if_original_dim(T2%TenDim))) then
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
		i1=T%TenDim%FindOrder(name1)
		i2=T2%TenDim%FindOrder(name2)
		rank1=T%rank
		rank2=T2%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call T%backward(i1(1:leni1))
		call dimoperation(T,(/1,rank1-leni1+1,rank1/))
		call T2%forward(i2(1:leni2))
		call dimoperation(T2,(/1,1,leni2/))
		T=ProductTensor_forContract(T,T2)
		call T2%split()
		return
	end subroutine
	subroutine contract_name_routine2(T,T1,name1,name2,len_of_contract) 
		class(Tensor),target::T
		class(Tensor),target:: T1
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2
		class(Tensor),pointer::pT,pT1
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1%TenDim).and.if_original_dim(T%TenDim))) then
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
		i1=T1%TenDim%FindOrder(name1)
		i2=T%TenDim%FindOrder(name2)
		rank1=T1%rank
		rank2=T%rank
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call T1%backward(i1(1:leni1))
		call dimoperation(T1,(/1,rank1-leni1+1,rank1/))
		call T%forward(i2(1:leni2))
		call dimoperation(T,(/1,1,leni2/))
		T=ProductTensor_forContract(T1,T)
		call T1%split( )
		return
	end subroutine
	subroutine contract_name_routine4(T,T1,name1,T2,name2) 
		class(Tensor),target::T
		class(Tensor),target :: T1,T2
		character(len=*),intent(in)::name1,name2
		class(Tensor),pointer::pT,pT1,pT2
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1%TenDim).and.if_original_dim(T2%TenDim))) then
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
		call T1%backward(name1)
		call T2%forward(name2)
		call T%ProductTensorRoutine(T1 , T2 , 1 )
		return
	end subroutine
	subroutine contract_name_routine5(T,name1,T2,name2) 
		class(Tensor),target::T
		class(Tensor),target :: T2
		character(len=*),intent(in)::name1,name2
		class(Tensor),pointer::pT,pT2
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T2%TenDim))) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		pT=>T
		pT2=>T2
		if(associated(pT,pT2))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract(name1,B,name2)')
			call writemess('T, A and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT2=>null()
		call T%backward(name1)
		call T2%forward(name2)
		T=ProductTensor_forContract(T,T2)
		return
	end subroutine
	subroutine contract_name_routine6(T,T1,name1,name2) 
		class(Tensor),target::T
		class(Tensor),target:: T1
		character(len=*),intent(in)::name1,name2
		class(Tensor),pointer::pT,pT1
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1%TenDim))) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		pT=>T
		pT1=>T1
		if(associated(pT,pT1))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract(A,name1,name2)')
			call writemess('T, A and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT1=>null()
		call T1%backward(name1)
		call T%forward(name2)
		T=ProductTensor_forContract(T1,T)
		return
	end subroutine

	subroutine contract_name_ownlegs_routine(T,name1,name2) 
		class(Tensor)::T
		type(Tensor),pointer::pT
		character(len=*),intent(in)::name1,name2
		type(Dimension)::NewDimen
		integer::rank,classtype,dim1,i,k
		integer,pointer::idata(:,:,:),newidata(:)
		real*4,pointer::sdata(:,:,:),newsdata(:)
		real*8,pointer::ddata(:,:,:),newddata(:)
		complex*8,pointer::cdata(:,:,:),newcdata(:)
		complex*16,pointer::zdata(:,:,:),newzdata(:)
		if(.not.T%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T%TenDim))) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		rank=T%getRank()
		if(rank.eq.2)then
			T=T%trace()
			return
		end if
		pT=>WorkingTensor1
		pT=T.pf.name1
		call pT%forward(name2)
		dim1=pT%dim(1)
		if(dim1.ne.pT%dim(2))then
			call writemess(' ERROR in contract(name1,name2), dimension')
			call error_stop
		end if
		NewDimen=pT.subdim.[3,rank]
		classtype=T%getType()
		call T%empty()
		call T%allocate(NewDimen,classtype)
		call pT%fuse(3,rank)

		select case (classtype)
			case (1)
				call pT%pointer(idata)
				call T%pointer(newidata)
				do k=1,T%getTotalData()
					newidata(k)=0
					do i=1,dim1
						newidata(k)=newidata(k)+idata(i,i,k)
					end do
				end do
			case (2)
				call pT%pointer(sdata)
				call T%pointer(newsdata)
				do k=1,T%getTotalData()
					newsdata(k)=0
					do i=1,dim1
						newsdata(k)=newsdata(k)+sdata(i,i,k)
					end do
				end do
			case(3)
				call pT%pointer(ddata)
				call T%pointer(newddata)
				do k=1,T%getTotalData()
					newddata(k)=0
					do i=1,dim1
						newddata(k)=newddata(k)+ddata(i,i,k)
					end do
				end do
			case(4)
				call pT%pointer(cdata)
				call T%pointer(newcdata)
				do k=1,T%getTotalData()
					newcdata(k)=0
					do i=1,dim1
						newcdata(k)=newcdata(k)+cdata(i,i,k)
					end do
				end do
			case(5)
				call pT%pointer(zdata)
				call T%pointer(newzdata)
				do k=1,T%getTotalData()
					newzdata(k)=0
					do i=1,dim1
						newzdata(k)=newzdata(k)+zdata(i,i,k)
					end do
				end do
			case default
				call writemess(' ERROR in contract(name1,name2), clasee type')
				call error_stop
		end select
		return
	end subroutine
	subroutine contract_ownlegs_routine(T,ith1,ith2) 
		class(Tensor)::T
		type(Tensor),pointer::pT
		integer,intent(in)::ith1,ith2
		type(Dimension)::NewDimen
		integer::rank,classtype,dim1,i,k
		integer,pointer::idata(:,:,:),newidata(:)
		real*4,pointer::sdata(:,:,:),newsdata(:)
		real*8,pointer::ddata(:,:,:),newddata(:)
		complex*8,pointer::cdata(:,:,:),newcdata(:)
		complex*16,pointer::zdata(:,:,:),newzdata(:)
		if(.not.T%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		rank=T%getRank()
		if(rank.eq.2)then
			T=T%trace()
			return
		end if
		pT=>WorkingTensor1
		pT=T.pf.ith1
		call pT%forward(ith2)
		dim1=pT%dim(1)
		if(dim1.ne.pT%dim(2))then
			call writemess(' ERROR in contract(ith1,ith2), dimension')
			call error_stop
		end if
		NewDimen=pT.subdim.[3,rank]
		classtype=T%getType()
		call T%empty()
		call T%allocate(NewDimen,classtype)
		call pT%fuse(3,rank)

		select case (classtype)
			case (1)
				call pT%pointer(idata)
				call T%pointer(newidata)
				do k=1,T%getTotalData()
					newidata(k)=0
					do i=1,dim1
						newidata(k)=newidata(k)+idata(i,i,k)
					end do
				end do
			case (2)
				call pT%pointer(sdata)
				call T%pointer(newsdata)
				do k=1,T%getTotalData()
					newsdata(k)=0
					do i=1,dim1
						newsdata(k)=newsdata(k)+sdata(i,i,k)
					end do
				end do
			case(3)
				call pT%pointer(ddata)
				call T%pointer(newddata)
				do k=1,T%getTotalData()
					newddata(k)=0
					do i=1,dim1
						newddata(k)=newddata(k)+ddata(i,i,k)
					end do
				end do
			case(4)
				call pT%pointer(cdata)
				call T%pointer(newcdata)
				do k=1,T%getTotalData()
					newcdata(k)=0
					do i=1,dim1
						newcdata(k)=newcdata(k)+cdata(i,i,k)
					end do
				end do
			case(5)
				call pT%pointer(zdata)
				call T%pointer(newzdata)
				do k=1,T%getTotalData()
					newzdata(k)=0
					do i=1,dim1
						newzdata(k)=newzdata(k)+zdata(i,i,k)
					end do
				end do
			case default
				call writemess(' ERROR in contract(name1,name2), clasee type')
				call error_stop
		end select
		return
	end subroutine
	type(Tensor) function contract_ownlegs(Tin,ith1,ith2) Result(Res)
		type(Tensor),intent(in)::Tin
		type(Tensor),pointer::pT
		integer,intent(in)::ith1,ith2
		type(Dimension)::NewDimen
		integer::rank,classtype,dim1,i,k
		integer,pointer::idata(:,:,:),newidata(:)
		real*4,pointer::sdata(:,:,:),newsdata(:)
		real*8,pointer::ddata(:,:,:),newddata(:)
		complex*8,pointer::cdata(:,:,:),newcdata(:)
		complex*16,pointer::zdata(:,:,:),newzdata(:)
		if(.not.Tin%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		rank=Tin%getRank()
		if(rank.eq.2)then
			Res=Tin%trace()
			return
		end if
		pT=>WorkingTensor1
		pT=Tin.pf.ith1
		call pT%forward(ith2)
		dim1=pT%dim(1)
		if(dim1.ne.pT%dim(2))then
			call writemess(' ERROR in contract(ith1,ith2), dimension')
			call error_stop
		end if
		NewDimen=pT.subdim.[3,rank]
		classtype=Tin%getType()
		call Res%empty()
		call Res%allocate(NewDimen,classtype)
		call pT%fuse(3,rank)

		select case (classtype)
			case (1)
				call pT%pointer(idata)
				call Res%pointer(newidata)
				do k=1,Res%getTotalData()
					newidata(k)=0
					do i=1,dim1
						newidata(k)=newidata(k)+idata(i,i,k)
					end do
				end do
			case (2)
				call pT%pointer(sdata)
				call Res%pointer(newsdata)
				do k=1,Res%getTotalData()
					newsdata(k)=0
					do i=1,dim1
						newsdata(k)=newsdata(k)+sdata(i,i,k)
					end do
				end do
			case(3)
				call pT%pointer(ddata)
				call Res%pointer(newddata)
				do k=1,Res%getTotalData()
					newddata(k)=0
					do i=1,dim1
						newddata(k)=newddata(k)+ddata(i,i,k)
					end do
				end do
			case(4)
				call pT%pointer(cdata)
				call Res%pointer(newcdata)
				do k=1,Res%getTotalData()
					newcdata(k)=0
					do i=1,dim1
						newcdata(k)=newcdata(k)+cdata(i,i,k)
					end do
				end do
			case(5)
				call pT%pointer(zdata)
				call Res%pointer(newzdata)
				do k=1,Res%getTotalData()
					newzdata(k)=0
					do i=1,dim1
						newzdata(k)=newzdata(k)+zdata(i,i,k)
					end do
				end do
			case default
				call writemess(' ERROR in contract(name1,name2), clasee type')
				call error_stop
		end select
		return
	end function

	type(Tensor) function contract_name_ownlegs(Tin,name1,name2) Result(Res)
		type(Tensor),intent(in)::Tin
		type(Tensor),pointer::pT
		character(len=*),intent(in)::name1,name2
		type(Dimension)::NewDimen
		integer::rank,classtype,dim1,i,k
		integer,pointer::idata(:,:,:),newidata(:)
		real*4,pointer::sdata(:,:,:),newsdata(:)
		real*8,pointer::ddata(:,:,:),newddata(:)
		complex*8,pointer::cdata(:,:,:),newcdata(:)
		complex*16,pointer::zdata(:,:,:),newzdata(:)
		if(.not.Tin%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(Tin%TenDim))) then
			call writemess("ERROR in contract with TensorName, one can not fuse any legs of the Tensor to use this function",-1)
			call writemess("stop",-1)
			call error_stop()
		end if
		rank=Tin%getRank()
		if(rank.eq.2)then
			Res=Tin%trace()
			return
		end if
		pT=>WorkingTensor1
		pT=Tin.pf.name1
		call pT%forward(name2)
		dim1=pT%dim(1)
		if(dim1.ne.pT%dim(2))then
			call writemess(' ERROR in contract(ith1,ith2), dimension')
			call error_stop
		end if
		NewDimen=pT.subdim.[3,rank]
		classtype=Tin%getType()
		call Res%empty()
		call Res%allocate(NewDimen,classtype)
		call pT%fuse(3,rank)

		select case (classtype)
			case (1)
				call pT%pointer(idata)
				call Res%pointer(newidata)
				do k=1,Res%getTotalData()
					newidata(k)=0
					do i=1,dim1
						newidata(k)=newidata(k)+idata(i,i,k)
					end do
				end do
			case (2)
				call pT%pointer(sdata)
				call Res%pointer(newsdata)
				do k=1,Res%getTotalData()
					newsdata(k)=0
					do i=1,dim1
						newsdata(k)=newsdata(k)+sdata(i,i,k)
					end do
				end do
			case(3)
				call pT%pointer(ddata)
				call Res%pointer(newddata)
				do k=1,Res%getTotalData()
					newddata(k)=0
					do i=1,dim1
						newddata(k)=newddata(k)+ddata(i,i,k)
					end do
				end do
			case(4)
				call pT%pointer(cdata)
				call Res%pointer(newcdata)
				do k=1,Res%getTotalData()
					newcdata(k)=0
					do i=1,dim1
						newcdata(k)=newcdata(k)+cdata(i,i,k)
					end do
				end do
			case(5)
				call pT%pointer(zdata)
				call Res%pointer(newzdata)
				do k=1,Res%getTotalData()
					newzdata(k)=0
					do i=1,dim1
						newzdata(k)=newzdata(k)+zdata(i,i,k)
					end do
				end do
			case default
				call writemess(' ERROR in contract(name1,name2), clasee type')
				call error_stop
		end select
		return
	end function





	!*****************************************************************
	!          fast contraction
	!*****************************************************************

	subroutine Fast_contract_name_routine(T,T1,name1,T2,name2,len_of_contract) 
		class(Tensor),target::T
		class(Tensor),target :: T1,T2
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2,type1,type2,maxi,maxj,maxk,maxl,maxm
		integer,allocatable::dim1(:),dim2(:)
		real*8,pointer::Tdp(:),Adp(:),Bdp(:)
		type(Dimension)::NewDim
		class(Tensor),pointer::pT,pT1,pT2
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1%TenDim).and.if_original_dim(T2%TenDim))) then
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
		i1=T1%FindOrder(name1)
		i2=T2%FindOrder(name2)
		rank1=T1%getrank()
		rank2=T2%getrank()
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call T%empty()
		!only contract 1 leg
		if((leni1.eq.1).and.(leni2.eq.1))then

			if(i1(1).eq.1)then
				if(i2(1).eq.1)then
					NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[2,rank2])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=dim1(1)
					maxj=product(dim1(2:rank1))
					maxk=product(dim2(2:rank2))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							call contractRank22leg11(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				if(i2(1).eq.rank2)then
					NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,rank2-1])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=dim1(1)
					maxj=product(dim1(2:rank1))
					maxk=product(dim2(1:rank2-1))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							call contractRank22leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,i2(1)-1])+(T2.subDim.[i2(1)+1,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim2(1:i2(1)-1))
				maxj=dim2(i2(1))
				maxk=product(dim2(i2(1)+1:rank2))
				maxl=product(dim1(2:rank1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[l,i,k]=A[j,l]*B[i,j,k]
						call contractRank23leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if

			!********

			if(i1(1).eq.rank1)then
				if(i2(1).eq.1)then
					NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[2,rank2])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=product(dim1(1:rank1-1))
					maxj=dim1(rank1)
					maxk=product(dim2(2:rank2))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							! T[i,k]=A[i,j]*B[j,k]
							call contractRank22leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				if(i2(1).eq.rank2)then
					NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,rank2-1])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=product(dim1(1:rank1-1))
					maxj=dim1(rank1)
					maxk=product(dim2(1:rank2-1))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							! T[i,k]=A[i,j]*B[k,j]
							call contractRank22leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,i2(1)-1])+(T2.subDim.[i2(1)+1,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim2(1:i2(1)-1))
				maxj=dim2(i2(1))
				maxk=product(dim2(i2(1)+1:rank2))
				maxl=product(dim1(1:rank1-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[l,i,k]=A[l,j]*B[i,j,k]
						call contractRank23leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if

			!********

			if(i2(1).eq.1)then
				NewDim=(T1.subDim.[1,i1(1)-1])+(T1.subDim.[i1(1)+1,rank1])+(T2.subDim.[2,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:i1(1)-1))
				maxj=dim1(i1(1))
				maxk=product(dim1(i1(1)+1:rank1))
				maxl=product(dim2(2:rank2))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k,l]=A[i,j,k]*B[j,l]
						call contractRank32leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			if(i2(1).eq.rank2)then
				NewDim=(T1.subDim.[1,i1(1)-1])+(T1.subDim.[i1(1)+1,rank1])+(T2.subDim.[1,rank2-1])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:i1(1)-1))
				maxj=dim1(i1(1))
				maxk=product(dim1(i1(1)+1:rank1))
				maxl=product(dim2(1:rank2-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k,l]=A[i,j,k]*B[l,j]
						call contractRank32leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			NewDim=(T1.subDim.[1,i1(1)-1])+(T1.subDim.[i1(1)+1,rank1])&
			                 + (T2.subDim.[1,i2(1)-1])+(T2.subDim.[i2(1)+1,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim1(1:i1(1)-1))
			maxj=dim1(i1(1))
			maxk=product(dim1(i1(1)+1:rank1))
			maxl=product(dim2(1:i2(1)-1))
			maxm=product(dim2(i2(1)+1:rank2))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[i,k,l,m]=A[i,j,k]*B[l,j,m]
					call contractRank33leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl,maxm) 
				case default
					call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
					call error_stop
			end select
			return
		end if

		!contracting more than 1 legs
		call T1%forward(i1(1:leni1))
		call T2%forward(i2(1:leni2))
		NewDim=(T1.subDim.[leni1+1,rank1])+(T2.subDim.[leni2+1,rank2])
		allocate(dim1(rank1))
		allocate(dim2(rank2))
		dim1=T1%dim()
		dim2=T2%dim()
		maxi=product(dim1(1:leni1))
		maxj=product(dim1(leni1+1:rank1))
		maxk=product(dim2(leni2+1:rank2))
		type1=T1%getType()
		type2=T2%getType()
		if(type1.ne.type2)then
			call writemess('ERROR in Fast_contract_name_routine,1')
			call error_stop
		end if
		call T%allocate(NewDim,type1)
		select case (type1)
			case (3)
				call T%pointer(Tdp)
				call T1%pointer(Adp)
				call T2%pointer(Bdp)
				call contractRank22leg11(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
			case default
				call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
				call error_stop
		end select
		return
	end subroutine

	subroutine Fast_contract_name_1leg_routine(T,T1,name1,T2,name2) 
		class(Tensor),target::T
		class(Tensor),target :: T1,T2
		character(len=*),intent(in)::name1,name2
		integer :: i1,i2
		integer::rank1,rank2,type1,type2,maxi,maxj,maxk,maxl,maxm
		integer,allocatable::dim1(:),dim2(:)
		real*8,pointer::Tdp(:),Adp(:),Bdp(:)
		type(Dimension)::NewDim
		class(Tensor),pointer::pT,pT1,pT2
		if(.not.T1%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.(if_original_dim(T1%TenDim).and.if_original_dim(T2%TenDim))) then
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
		i1=T1%FindOrder(name1)
		i2=T2%FindOrder(name2)
		rank1=T1%getrank()
		rank2=T2%getrank()
		call T%empty()

		if(i1.eq.1)then
			if(i2.eq.1)then
				NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[2,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=dim1(1)
				maxj=product(dim1(2:rank1))
				maxk=product(dim2(2:rank2))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						call contractRank22leg11(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			if(i2.eq.rank2)then
				NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,rank2-1])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=dim1(1)
				maxj=product(dim1(2:rank1))
				maxk=product(dim2(1:rank2-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
					call T1%pointer(Adp)
						call T2%pointer(Bdp)
						call contractRank22leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,i2-1])+(T2.subDim.[i2+1,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim2(1:i2-1))
			maxj=dim2(i2)
			maxk=product(dim2(i2+1:rank2))
			maxl=product(dim1(2:rank1))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[l,i,k]=A[j,l]*B[i,j,k]
					call contractRank23leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
					call error_stop
			end select
			return
		end if

		!********

		if(i1.eq.rank1)then
			if(i2.eq.1)then
				NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[2,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:rank1-1))
				maxj=dim1(rank1)
				maxk=product(dim2(2:rank2))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k]=A[i,j]*B[j,k]
						call contractRank22leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			if(i2.eq.rank2)then
				NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,rank2-1])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:rank1-1))
				maxj=dim1(rank1)
				maxk=product(dim2(1:rank2-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k]=A[i,j]*B[k,j]
						call contractRank22leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
					call writemess('Do NO  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,i2-1])+(T2.subDim.[i2+1,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim2(1:i2-1))
			maxj=dim2(i2)
			maxk=product(dim2(i2+1:rank2))
			maxl=product(dim1(1:rank1-1))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[l,i,k]=A[l,j]*B[i,j,k]
					call contractRank23leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in ast_contract_name_rouine yet,')
					call error_stop
			end select
			return
		end if

		!********

		if(i2.eq.1)then
			NewDim=(T1.subDim.[1,i1-1])+(T1.subDim.[i1+1,rank1])+(T2.subDim.[2,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim1(1:i1-1))
			maxj=dim1(i1)
			maxk=product(dim1(i1+1:rank1))
			maxl=product(dim2(2:rank2))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[i,k,l]=A[i,j,k]*B[j,l]
					call contractRank32leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in Fat_contract_name_routie yet,')
					call error_stop
			end select
			return
		end if
		if(i2.eq.rank2)then
			NewDim=(T1.subDim.[1,i1-1])+(T1.subDim.[i1+1,rank1])+(T2.subDim.[1,rank2-1])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim1(1:i1-1))
			maxj=dim1(i1)
			maxk=product(dim1(i1+1:rank1))
			maxl=product(dim2(1:rank2-1))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[i,k,l]=A[i,j,k]*B[l,j]
					call contractRank32leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
					call error_stop
			end select
			return
		end if
		NewDim=(T1.subDim.[1,i1-1])+(T1.subDim.[i1+1,rank1])&
		                 + (T2.subDim.[1,i2-1])+(T2.subDim.[i2+1,rank2])
		allocate(dim1(rank1))
		allocate(dim2(rank2))
		dim1=T1%dim()
		dim2=T2%dim()
		maxi=product(dim1(1:i1-1))
		maxj=dim1(i1)
		maxk=product(dim1(i1+1:rank1))
		maxl=product(dim2(1:i2-1))
		maxm=product(dim2(i2+1:rank2))
		type1=T1%getType()
		type2=T2%getType()
		if(type1.ne.type2)then
			call writemess('ERROR in Fast_contract_name_routine,1')
			call error_stop
		end if
		call T%allocate(NewDim,type1)
		select case (type1)
			case (3)
				call T%pointer(Tdp)
				call T1%pointer(Adp)
				call T2%pointer(Bdp)
				! T[i,k,l,m]=A[i,j,k]*B[l,j,m]
				call contractRank33leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl,maxm) 
			case default
				call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
				call error_stop
		end select
		return
	end subroutine

	subroutine Fast_contract_name_routine1(T,name1,T2,name2,len_of_contract) 
		class(Tensor),target::T
		class(Tensor),target :: T2
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2,type1,type2,maxi,maxj,maxk,maxl,maxm
		integer,allocatable::dim1(:),dim2(:)
		real*8,pointer::Tdp(:),Adp(:),Bdp(:)
		type(Dimension)::NewDim
		type(Tensor),pointer::T1
		class(Tensor),pointer::pT,pT2
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
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

		T1=>WorkingTensor1
		T1=T
		i1=T1%FindOrder(name1)
		i2=T2%FindOrder(name2)
		rank1=T1%getrank()
		rank2=T2%getrank()
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call T%empty()
		if((leni1.eq.1).and.(leni2.eq.1))then

			if(i1(1).eq.1)then
				if(i2(1).eq.1)then
					NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[2,rank2])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=dim1(1)
					maxj=product(dim1(2:rank1))
					maxk=product(dim2(2:rank2))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							call contractRank22leg11(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				if(i2(1).eq.rank2)then
					NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,rank2-1])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=dim1(1)
					maxj=product(dim1(2:rank1))
					maxk=product(dim2(1:rank2-1))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							call contractRank22leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,i2(1)-1])+(T2.subDim.[i2(1)+1,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim2(1:i2(1)-1))
				maxj=dim2(i2(1))
				maxk=product(dim2(i2(1)+1:rank2))
				maxl=product(dim1(2:rank1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[l,i,k]=A[j,l]*B[i,j,k]
						call contractRank23leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if

			!********

			if(i1(1).eq.rank1)then
				if(i2(1).eq.1)then
					NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[2,rank2])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=product(dim1(1:rank1-1))
					maxj=dim1(rank1)
					maxk=product(dim2(2:rank2))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							! T[i,k]=A[i,j]*B[j,k]
							call contractRank22leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				if(i2(1).eq.rank2)then
					NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,rank2-1])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=product(dim1(1:rank1-1))
					maxj=dim1(rank1)
					maxk=product(dim2(1:rank2-1))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							! T[i,k]=A[i,j]*B[k,j]
							call contractRank22leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,i2(1)-1])+(T2.subDim.[i2(1)+1,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim2(1:i2(1)-1))
				maxj=dim2(i2(1))
				maxk=product(dim2(i2(1)+1:rank2))
				maxl=product(dim1(1:rank1-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[l,i,k]=A[l,j]*B[i,j,k]
						call contractRank23leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if

			!********

			if(i2(1).eq.1)then
				NewDim=(T1.subDim.[1,i1(1)-1])+(T1.subDim.[i1(1)+1,rank1])+(T2.subDim.[2,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:i1(1)-1))
				maxj=dim1(i1(1))
				maxk=product(dim1(i1(1)+1:rank1))
				maxl=product(dim2(2:rank2))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k,l]=A[i,j,k]*B[j,l]
						call contractRank32leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			if(i2(1).eq.rank2)then
				NewDim=(T1.subDim.[1,i1(1)-1])+(T1.subDim.[i1(1)+1,rank1])+(T2.subDim.[1,rank2-1])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:i1(1)-1))
				maxj=dim1(i1(1))
				maxk=product(dim1(i1(1)+1:rank1))
				maxl=product(dim2(1:rank2-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k,l]=A[i,j,k]*B[l,j]
						call contractRank32leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			NewDim=(T1.subDim.[1,i1(1)-1])+(T1.subDim.[i1(1)+1,rank1])&
			                 + (T2.subDim.[1,i2(1)-1])+(T2.subDim.[i2(1)+1,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim1(1:i1(1)-1))
			maxj=dim1(i1(1))
			maxk=product(dim1(i1(1)+1:rank1))
			maxl=product(dim2(1:i2(1)-1))
			maxm=product(dim2(i2(1)+1:rank2))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[i,k,l,m]=A[i,j,k]*B[l,j,m]
					call contractRank33leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl,maxm) 
				case default
					call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
					call error_stop
			end select
			return
		end if

		!contracting more than 1 legs
		call T1%forward(i1(1:leni1))
		call T2%forward(i2(1:leni2))
		NewDim=(T1.subDim.[leni1+1,rank1])+(T2.subDim.[leni2+1,rank2])
		allocate(dim1(rank1))
		allocate(dim2(rank2))
		dim1=T1%dim()
		dim2=T2%dim()
		maxi=product(dim1(1:leni1))
		maxj=product(dim1(leni1+1:rank1))
		maxk=product(dim2(leni2+1:rank2))
		type1=T1%getType()
		type2=T2%getType()
		if(type1.ne.type2)then
			call writemess('ERROR in Fast_contract_name_routine,1')
			call error_stop
		end if
		call T%allocate(NewDim,type1)
		select case (type1)
			case (3)
				call T%pointer(Tdp)
				call T1%pointer(Adp)
				call T2%pointer(Bdp)
				call contractRank22leg11(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
			case default
				call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
				call error_stop
		end select
		return
	end subroutine

	subroutine Fast_contract_name_1leg_routine1(T,name1,T2,name2) 
		class(Tensor),target::T
		class(Tensor),target :: T2
		character(len=*),intent(in)::name1,name2
		integer :: i1,i2
		integer::rank1,rank2,type1,type2,maxi,maxj,maxk,maxl,maxm
		integer,allocatable::dim1(:),dim2(:)
		real*8,pointer::Tdp(:),Adp(:),Bdp(:)
		type(Dimension)::NewDim
		type(Tensor),pointer::T1
		class(Tensor),pointer::pT,pT2
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()		
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
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

		T1=>WorkingTensor1
		T1=T
		i1=T1%FindOrder(name1)
		i2=T2%FindOrder(name2)
		rank1=T1%getrank()
		rank2=T2%getrank()
		call T%empty()
		if(i1.eq.1)then
			if(i2.eq.1)then
				NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[2,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=dim1(1)
				maxj=product(dim1(2:rank1))
				maxk=product(dim2(2:rank2))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						call contractRank22leg11(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			if(i2.eq.rank2)then
				NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,rank2-1])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=dim1(1)
				maxj=product(dim1(2:rank1))
				maxk=product(dim2(1:rank2-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
					call T1%pointer(Adp)
						call T2%pointer(Bdp)
						call contractRank22leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,i2-1])+(T2.subDim.[i2+1,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim2(1:i2-1))
			maxj=dim2(i2)
			maxk=product(dim2(i2+1:rank2))
			maxl=product(dim1(2:rank1))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[l,i,k]=A[j,l]*B[i,j,k]
					call contractRank23leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
					call error_stop
			end select
			return
		end if

		!********

		if(i1.eq.rank1)then
			if(i2.eq.1)then
				NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[2,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:rank1-1))
				maxj=dim1(rank1)
				maxk=product(dim2(2:rank2))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k]=A[i,j]*B[j,k]
						call contractRank22leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			if(i2.eq.rank2)then
				NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,rank2-1])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:rank1-1))
				maxj=dim1(rank1)
				maxk=product(dim2(1:rank2-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k]=A[i,j]*B[k,j]
						call contractRank22leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
					call writemess('Do NO  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,i2-1])+(T2.subDim.[i2+1,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim2(1:i2-1))
			maxj=dim2(i2)
			maxk=product(dim2(i2+1:rank2))
			maxl=product(dim1(1:rank1-1))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[l,i,k]=A[l,j]*B[i,j,k]
					call contractRank23leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in ast_contract_name_rouine yet,')
					call error_stop
			end select
			return
		end if

		!********

		if(i2.eq.1)then
			NewDim=(T1.subDim.[1,i1-1])+(T1.subDim.[i1+1,rank1])+(T2.subDim.[2,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim1(1:i1-1))
			maxj=dim1(i1)
			maxk=product(dim1(i1+1:rank1))
			maxl=product(dim2(2:rank2))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[i,k,l]=A[i,j,k]*B[j,l]
					call contractRank32leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in Fat_contract_name_routie yet,')
					call error_stop
			end select
			return
		end if
		if(i2.eq.rank2)then
			NewDim=(T1.subDim.[1,i1-1])+(T1.subDim.[i1+1,rank1])+(T2.subDim.[1,rank2-1])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim1(1:i1-1))
			maxj=dim1(i1)
			maxk=product(dim1(i1+1:rank1))
			maxl=product(dim2(1:rank2-1))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[i,k,l]=A[i,j,k]*B[l,j]
					call contractRank32leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
					call error_stop
			end select
			return
		end if
		NewDim=(T1.subDim.[1,i1-1])+(T1.subDim.[i1+1,rank1])&
		                 + (T2.subDim.[1,i2-1])+(T2.subDim.[i2+1,rank2])
		allocate(dim1(rank1))
		allocate(dim2(rank2))
		dim1=T1%dim()
		dim2=T2%dim()
		maxi=product(dim1(1:i1-1))
		maxj=dim1(i1)
		maxk=product(dim1(i1+1:rank1))
		maxl=product(dim2(1:i2-1))
		maxm=product(dim2(i2+1:rank2))
		type1=T1%getType()
		type2=T2%getType()
		if(type1.ne.type2)then
			call writemess('ERROR in Fast_contract_name_routine,1')
			call error_stop
		end if
		call T%allocate(NewDim,type1)
		select case (type1)
			case (3)
				call T%pointer(Tdp)
				call T1%pointer(Adp)
				call T2%pointer(Bdp)
				! T[i,k,l,m]=A[i,j,k]*B[l,j,m]
				call contractRank33leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl,maxm) 
			case default
				call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
				call error_stop
		end select
		return
	end subroutine

	subroutine Fast_contract_name_routine2(T,T1,name1,name2,len_of_contract) 
		class(Tensor),target::T
		class(Tensor),target :: T1
		character(len=*),intent(in)::name1(:),name2(:)
		integer :: i1(size(name1)),i2(size(name2))
		integer,optional,intent(in)::len_of_contract(2)
		integer::leni1,leni2,rank1,rank2,type1,type2,maxi,maxj,maxk,maxl,maxm
		integer,allocatable::dim1(:),dim2(:)
		real*8,pointer::Tdp(:),Adp(:),Bdp(:)
		type(Dimension)::NewDim
		type(Tensor),pointer::T2
		class(Tensor),pointer::pT,pT1
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T1%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		pT=>T
		pT1=>T1
		if(associated(pT,pT1))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract([names],B,[names])')
			call writemess('T and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT1=>null()

		T2=>WorkingTensor1
		T2=T
		i1=T1%FindOrder(name1)
		i2=T2%FindOrder(name2)
		rank1=T1%getrank()
		rank2=T2%getrank()
		if(present(len_of_contract))then
			leni1=min(len_of_contract(1),size(i1))
			leni2=min(len_of_contract(2),size(i2))
		else
			leni1=size(i1)
			leni2=size(i2)
		end if
		call T%empty()
		!only contract 1 leg
		if((leni1.eq.1).and.(leni2.eq.1))then

			if(i1(1).eq.1)then
				if(i2(1).eq.1)then
					NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[2,rank2])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=dim1(1)
					maxj=product(dim1(2:rank1))
					maxk=product(dim2(2:rank2))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							call contractRank22leg11(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				if(i2(1).eq.rank2)then
					NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,rank2-1])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=dim1(1)
					maxj=product(dim1(2:rank1))
					maxk=product(dim2(1:rank2-1))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							call contractRank22leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,i2(1)-1])+(T2.subDim.[i2(1)+1,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim2(1:i2(1)-1))
				maxj=dim2(i2(1))
				maxk=product(dim2(i2(1)+1:rank2))
				maxl=product(dim1(2:rank1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[l,i,k]=A[j,l]*B[i,j,k]
						call contractRank23leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if

			!********

			if(i1(1).eq.rank1)then
				if(i2(1).eq.1)then
					NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[2,rank2])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=product(dim1(1:rank1-1))
					maxj=dim1(rank1)
					maxk=product(dim2(2:rank2))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							! T[i,k]=A[i,j]*B[j,k]
							call contractRank22leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				if(i2(1).eq.rank2)then
					NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,rank2-1])
					allocate(dim1(rank1))
					allocate(dim2(rank2))
					dim1=T1%dim()
					dim2=T2%dim()
					maxi=product(dim1(1:rank1-1))
					maxj=dim1(rank1)
					maxk=product(dim2(1:rank2-1))
					type1=T1%getType()
					type2=T2%getType()
					if(type1.ne.type2)then
						call writemess('ERROR in Fast_contract_name_routine,1')
						call error_stop
					end if
					call T%allocate(NewDim,type1)
					select case (type1)
						case (3)
							call T%pointer(Tdp)
							call T1%pointer(Adp)
							call T2%pointer(Bdp)
							! T[i,k]=A[i,j]*B[k,j]
							call contractRank22leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
						case default
							call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
							call error_stop
					end select
					return
				end if
				NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,i2(1)-1])+(T2.subDim.[i2(1)+1,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim2(1:i2(1)-1))
				maxj=dim2(i2(1))
				maxk=product(dim2(i2(1)+1:rank2))
				maxl=product(dim1(1:rank1-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[l,i,k]=A[l,j]*B[i,j,k]
						call contractRank23leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if

			!********

			if(i2(1).eq.1)then
				NewDim=(T1.subDim.[1,i1(1)-1])+(T1.subDim.[i1(1)+1,rank1])+(T2.subDim.[2,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:i1(1)-1))
				maxj=dim1(i1(1))
				maxk=product(dim1(i1(1)+1:rank1))
				maxl=product(dim2(2:rank2))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k,l]=A[i,j,k]*B[j,l]
						call contractRank32leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			if(i2(1).eq.rank2)then
				NewDim=(T1.subDim.[1,i1(1)-1])+(T1.subDim.[i1(1)+1,rank1])+(T2.subDim.[1,rank2-1])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:i1(1)-1))
				maxj=dim1(i1(1))
				maxk=product(dim1(i1(1)+1:rank1))
				maxl=product(dim2(1:rank2-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k,l]=A[i,j,k]*B[l,j]
						call contractRank32leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			NewDim=(T1.subDim.[1,i1(1)-1])+(T1.subDim.[i1(1)+1,rank1])&
			                 + (T2.subDim.[1,i2(1)-1])+(T2.subDim.[i2(1)+1,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim1(1:i1(1)-1))
			maxj=dim1(i1(1))
			maxk=product(dim1(i1(1)+1:rank1))
			maxl=product(dim2(1:i2(1)-1))
			maxm=product(dim2(i2(1)+1:rank2))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[i,k,l,m]=A[i,j,k]*B[l,j,m]
					call contractRank33leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl,maxm) 
				case default
					call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
					call error_stop
			end select
			return
		end if

		!contracting more than 1 legs
		call T1%forward(i1(1:leni1))
		call T2%forward(i2(1:leni2))
		NewDim=(T1.subDim.[leni1+1,rank1])+(T2.subDim.[leni2+1,rank2])
		allocate(dim1(rank1))
		allocate(dim2(rank2))
		dim1=T1%dim()
		dim2=T2%dim()
		maxi=product(dim1(1:leni1))
		maxj=product(dim1(leni1+1:rank1))
		maxk=product(dim2(leni2+1:rank2))
		type1=T1%getType()
		type2=T2%getType()
		if(type1.ne.type2)then
			call writemess('ERROR in Fast_contract_name_routine,1')
			call error_stop
		end if
		call T%allocate(NewDim,type1)
		select case (type1)
			case (3)
				call T%pointer(Tdp)
				call T1%pointer(Adp)
				call T2%pointer(Bdp)
				call contractRank22leg11(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
			case default
				call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
				call error_stop
		end select
		return
	end subroutine

	subroutine Fast_contract_name_1leg_routine2(T,T1,name1,name2) 
		class(Tensor),target::T
		class(Tensor),target :: T1
		character(len=*),intent(in)::name1,name2
		integer :: i1,i2
		integer::rank1,rank2,type1,type2,maxi,maxj,maxk,maxl,maxm
		integer,allocatable::dim1(:),dim2(:)
		real*8,pointer::Tdp(:),Adp(:),Bdp(:)
		type(Dimension)::NewDim
		type(Tensor),pointer::T2
		class(Tensor),pointer::pT,pT1
		if(.not.T%getFlag())then
			call writemess('There is no data in the first Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		if(.not.T1%getFlag())then
			call writemess('There is no data in the second Tensor, when contracting Tensor',-1)
			call error_stop()
		end if
		pT=>T
		pT1=>T1
		if(associated(pT,pT1))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%contract([names],B,[names])')
			call writemess('T and B can not be a same variable')
			call error_stop
		end if
		pT=>null()
		pT1=>null()

		T2=>WorkingTensor2
		T2=T
		i1=T1%FindOrder(name1)
		i2=T2%FindOrder(name2)
		rank1=T1%getrank()
		rank2=T2%getrank()
		call T%empty()

		if(i1.eq.1)then
			if(i2.eq.1)then
				NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[2,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=dim1(1)
				maxj=product(dim1(2:rank1))
				maxk=product(dim2(2:rank2))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						call contractRank22leg11(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			if(i2.eq.rank2)then
				NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,rank2-1])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=dim1(1)
				maxj=product(dim1(2:rank1))
				maxk=product(dim2(1:rank2-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
					call T1%pointer(Adp)
						call T2%pointer(Bdp)
						call contractRank22leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			NewDim=(T1.subDim.[2,rank1])+(T2.subDim.[1,i2-1])+(T2.subDim.[i2+1,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim2(1:i2-1))
			maxj=dim2(i2)
			maxk=product(dim2(i2+1:rank2))
			maxl=product(dim1(2:rank1))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[l,i,k]=A[j,l]*B[i,j,k]
					call contractRank23leg12(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
					call error_stop
			end select
			return
		end if

		!********

		if(i1.eq.rank1)then
			if(i2.eq.1)then
				NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[2,rank2])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:rank1-1))
				maxj=dim1(rank1)
				maxk=product(dim2(2:rank2))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k]=A[i,j]*B[j,k]
						call contractRank22leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
						call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			if(i2.eq.rank2)then
				NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,rank2-1])
				allocate(dim1(rank1))
				allocate(dim2(rank2))
				dim1=T1%dim()
				dim2=T2%dim()
				maxi=product(dim1(1:rank1-1))
				maxj=dim1(rank1)
				maxk=product(dim2(1:rank2-1))
				type1=T1%getType()
				type2=T2%getType()
				if(type1.ne.type2)then
					call writemess('ERROR in Fast_contract_name_routine,1')
					call error_stop
				end if
				call T%allocate(NewDim,type1)
				select case (type1)
					case (3)
						call T%pointer(Tdp)
						call T1%pointer(Adp)
						call T2%pointer(Bdp)
						! T[i,k]=A[i,j]*B[k,j]
						call contractRank22leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk) 
					case default
					call writemess('Do NO  finished this part in Fast_contract_name_routine yet,')
						call error_stop
				end select
				return
			end if
			NewDim=(T1.subDim.[1,rank1-1])+(T2.subDim.[1,i2-1])+(T2.subDim.[i2+1,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim2(1:i2-1))
			maxj=dim2(i2)
			maxk=product(dim2(i2+1:rank2))
			maxl=product(dim1(1:rank1-1))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[l,i,k]=A[l,j]*B[i,j,k]
					call contractRank23leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in ast_contract_name_rouine yet,')
					call error_stop
			end select
			return
		end if

		!********

		if(i2.eq.1)then
			NewDim=(T1.subDim.[1,i1-1])+(T1.subDim.[i1+1,rank1])+(T2.subDim.[2,rank2])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim1(1:i1-1))
			maxj=dim1(i1)
			maxk=product(dim1(i1+1:rank1))
			maxl=product(dim2(2:rank2))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[i,k,l]=A[i,j,k]*B[j,l]
					call contractRank32leg21(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in Fat_contract_name_routie yet,')
					call error_stop
			end select
			return
		end if
		if(i2.eq.rank2)then
			NewDim=(T1.subDim.[1,i1-1])+(T1.subDim.[i1+1,rank1])+(T2.subDim.[1,rank2-1])
			allocate(dim1(rank1))
			allocate(dim2(rank2))
			dim1=T1%dim()
			dim2=T2%dim()
			maxi=product(dim1(1:i1-1))
			maxj=dim1(i1)
			maxk=product(dim1(i1+1:rank1))
			maxl=product(dim2(1:rank2-1))
			type1=T1%getType()
			type2=T2%getType()
			if(type1.ne.type2)then
				call writemess('ERROR in Fast_contract_name_routine,1')
				call error_stop
			end if
			call T%allocate(NewDim,type1)
			select case (type1)
				case (3)
					call T%pointer(Tdp)
					call T1%pointer(Adp)
					call T2%pointer(Bdp)
					! T[i,k,l]=A[i,j,k]*B[l,j]
					call contractRank32leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl) 
				case default
					call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
					call error_stop
			end select
			return
		end if
		NewDim=(T1.subDim.[1,i1-1])+(T1.subDim.[i1+1,rank1])&
		                 + (T2.subDim.[1,i2-1])+(T2.subDim.[i2+1,rank2])
		allocate(dim1(rank1))
		allocate(dim2(rank2))
		dim1=T1%dim()
		dim2=T2%dim()
		maxi=product(dim1(1:i1-1))
		maxj=dim1(i1)
		maxk=product(dim1(i1+1:rank1))
		maxl=product(dim2(1:i2-1))
		maxm=product(dim2(i2+1:rank2))
		type1=T1%getType()
		type2=T2%getType()
		if(type1.ne.type2)then
			call writemess('ERROR in Fast_contract_name_routine,1')
			call error_stop
		end if
		call T%allocate(NewDim,type1)
		select case (type1)
			case (3)
				call T%pointer(Tdp)
				call T1%pointer(Adp)
				call T2%pointer(Bdp)
				! T[i,k,l,m]=A[i,j,k]*B[l,j,m]
				call contractRank33leg22(Tdp,Adp,Bdp,Maxi,maxj,maxk,maxl,maxm) 
			case default
				call writemess('Do NOT  finished this part in Fast_contract_name_routine yet,')
				call error_stop
		end select
		return
	end subroutine





!		inde=[-1,inde_min,inde_max] output data(inde_min:inde_max,:)
!	or[-2,inde_min,inde_max],data(:,inde_min:inde_max)
!	or[-3,inde_min,inde_max],data(inde_min:inde_max)
!	or [-1,inde_row] [-2,inde_col],output row or col
!	or [inde1_min,inde1_max,inde2_min,inde2_max] output data(inde1_min:inde1_max,inde2_min:inde2_max)

!
! output col or row allow for rank>3 , the Tensor will regard as Matrix
	type(Tensor) function subTen(T,inde,keepdim)	
		class(Tensor),intent(in) ::T
		integer,intent(in)::inde(:)
		logical,optional,intent(in)::keepdim
		integer::dim1,dim2,i,rank,newDim1,newDim2
		type(dimension)::newdim
		rank=T%getRank()
		if(size(inde).eq.4) then
			if(getRank(T).gt.2)then
				write(*,*)"error in subTen,only matrix or vector is allowed"
				call error_stop()
			end if
			dim1=inde(2)-inde(1)+1
			dim2=inde(4)-inde(3)+1
			call allocateTensor(subTen,(/dim1,dim2/),T%getType())
			call subTen_TData_routine1(T%TData,subTen%TData,T.dim.1,T.dim.2,dim1,&
					dim2,inde(1),inde(2),inde(3),inde(4))
			return
		end if
		if(size(inde).eq.2) then
			dim1=T.dim.1
			dim2=T.dim.2
			select case (inde(1))
			
				case (-1)!output row
					if(getRank(T).gt.2)then
						write(*,*)"error in subTen,only matrix or vector is allowed"
						call error_stop()
					end if
					if(present(keepdim).and.keepdim)then
						call allocateTensor(subTen,[1,dim2],T%getType())
					else
						call allocateTensor(subTen,(/dim2/),T%getType())
					end if
					call subTen_TData_routine2(T%TData,subTen%TData,dim1,dim2,dim2,inde(2),.true.)
				case (-2)!
					dim1=T%dim(1)
					newdim=T.subdim.1
					do i=2,rank-1
						newdim=newdim+(T.subdim.i)
						dim1=dim1*T%dim(i)
					end do
					dim2=T%dim(rank)
					if(present(keepdim).and.keepdim)then
						call allocateTensor(subTen,newdim+[1],T%getType())
						if(newdim%outNameFlag().ne.0) call subTen%setName(rank,T%getName(rank))
					else
						call allocateTensor(subTen,newdim,T%getType())
					end if
					call subTen_TData_routine2(T%TData,subTen%TData,dim1,dim2,dim1,inde(2),.false.)
				case default 
					write(*,*) "no such case in subTen"
					write(*,*)inde
					call error_stop()
			end 	select
			return
		end if
		if(size(inde).ne.3) then
			write(*,*) "no such case in subTen"
			write(*,*) "length of inde is ",size(inde)
			write(*,*)inde
			call error_stop()
		end if
		select case (inde(1))
			case (-1)!output some rows
				newDim1=inde(3)-inde(2)+1
				newdim=(/newDim1/)
				newDim2=1
				do i=2,rank
					newdim=newdim+(T.subdim.i)
					newDim2=newDim2*T%dim(i)
				end do
				dim1=T.dim.1
				dim2=T%getTotalData()/dim1
				call allocateTensor(subTen,newdim,T%getType())
				call subTen_TData_routine3(T%TData,subTen%TData,dim1,dim2,newDim1,&
					newDim2,inde(2),inde(3),.true.)
			case (-2)!!output some cols
				newDim2=inde(3)-inde(2)+1
				newdim=T.subdim.1
				newDim1=T%dim(1)
				do i=2,rank-1
					newdim=newdim+(T.subdim.i)
					newDim1=newDim1*T%dim(i)
				end do
				newdim=newdim+(/newDim2/)
				dim2=T.dim.rank
				dim1=T%getTotalData()/dim2
				call allocateTensor(subTen,newdim,T%getType())
				call subTen_TData_routine3(T%TData,subTen%TData,dim1,dim2,newDim1,&
					newDim2,inde(2),inde(3),.false.)
			case (-3)!
				dim1=inde(3)-inde(2)+1
				call allocateTensor(subTen,(/dim1/),T%getType())
				call subTen_TData_routine1_(T%TData,subTen%TData,dim1,inde(2),inde(3))
			case default 
				write(*,*) "no such case in subTen"
				write(*,*)inde
				call error_stop()
		end 	select
		return
	end function
	subroutine subTenRoutine(outT,A,legi,legith,keepdim)
		type(Tensor),intent(inout)::outT
		type(Tensor),intent(in)::A
		integer,intent(in)::legi,legith
		logical,optional,intent(in)::keepdim
		integer::dim1,dim2,i,rank,newDim1,newDim2
		type(Tensor)::T
		type(dimension)::newdim
		if(A%getRank().le.1)then
			call writemess('ERROR in subTensor, the rank should be larger than or equal to 2')
			call error_stop
		end if
		T=A.pb.legi
		dim1=T%dim(1)
		rank=T%getRank()
		newdim=T.subdim.[1,rank-1]
		do i=2,rank-1
			dim1=dim1*T%dim(i)
		end do
		dim2=T%dim(rank)
		call outT%empty()
		call outT%allocate(newdim+[1],T%getType())
		if(newdim%outNameFlag().ne.0) call outT%setName(rank,T%getName(rank))
		call subTen_TData_routine2(T%TData,outT%TData,dim1,dim2,dim1,legith,.false.)
		outT=outT.pbi.legi
		if(present(keepdim).and.keepdim)return
		call outT%killLeg(legi,'kill') 
		return
	end subroutine
	type(Tensor) function subTen2(T,legi,legith,keepdim)
		class(Tensor),intent(in)::T
		integer,intent(in)::legi,legith
		logical,optional,intent(in)::keepdim
		call subTenRoutine(subTen2,T,legi,legith,keepdim)
		return
	end function
	type(Tensor) function subTen2_Name(T,legName,legith,keepdim)
		class(Tensor),intent(in)::T
		integer,intent(in)::legith
		character(len=*),intent(in)::legName
		logical,optional,intent(in)::keepdim
		call subTenRoutine(subTen2_Name,T,T%FindOrder(legName),legith,keepdim)
		return
	end function
	subroutine subTenRoutineLegs(outT,A,legi,legith)
		type(Tensor),intent(inout)::outT
		type(Tensor),intent(in)::A
		integer,intent(in)::legi,legith(2)
		integer::dim1,dim2,i,rank,newDim1,newDim2
		type(Tensor)::T
		type(dimension)::newdim
		if(A%getRank().le.1)then
			call writemess('ERROR in subTensor, the rank should be larger than or equal to 2')
			call error_stop
		end if
		T=A.pb.legi
		rank=T%getRank()
		newDim2=legith(2)-legith(1)+1
		if(newDim2.le.0)then
			call writemess('ERROR in subTensor, legith=('+legith(1)+','+legith(2)+')')
			call error_stop
		end if
		if(newDim2.gt.T%dim(rank))then
			call writemess('ERROR in subTensor, legith=('+legith(1)+','+legith(2)+')')
			call writemess('New dimension is larger than original one, dimen='+T%dim(rank))
			call error_stop
		end if
		newdim=T.subdim.[1,rank-1]
		newDim1=T%dim(1)
		do i=2,rank-1
			newDim1=newDim1*T%dim(i)
		end do
		newdim=newdim+[newDim2]
		dim2=T.dim.rank
		dim1=newDim1
		call outT%allocate(newdim,T%getType())
		if(newdim%outNameFlag().ne.0) call outT%setName(rank,T%getName(rank))
		call subTen_TData_routine3(T%TData,outT%TData,dim1,dim2,newDim1,&
			newDim2,legith(1),legith(2),.false.)
		outT=outT.pbi.legi
		return
	end subroutine
	type(Tensor) function subTen3(T,legi,legith)
		class(Tensor),intent(in)::T
		integer,intent(in)::legi,legith(2)
		call subTenRoutineLegs(subTen3,T,legi,legith)
		return
	end function
	type(Tensor) function subTen3_Name(T,legName,legith)
		class(Tensor),intent(in)::T
		integer,intent(in)::legith(2)
		character(len=*),intent(in)::legName
		call subTenRoutineLegs(subTen3_Name,T,T%FindOrder(legName),legith)
		return
	end function
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  useful function
!
!**************************************************************************************************************
!**************************************************************************************************************

	Type(Tensor) function enlargeTensorReal8(T,ithleg,newD,randomNumberScal)result(enlargeTensor)
		class(Tensor),intent(in)::T
		integer,intent(in)::newD,ithleg
		real*8,intent(in)::randomNumberScal
		integer::D,rank
		type(Dimension)::dimen
		rank=T%getRank()
		D=T%dim(ithleg)
		if(D.eq.newD)then
			enlargeTensor=T
			return
		end if
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.T%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,T%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if	
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call enlargeTensor%allocate(dimen,T%getType())
		if(T%ifDynamic())call enlargeTensor%Dynamic()
		call enlargeTensor%random([-randomNumberScal,randomNumberScal])
		call enlargeTensor%setValue(1,T%getTotalData(),T.pb.ithleg)
		enlargeTensor=enlargeTensor.pbi.ithleg
		return
	end function

	Type(Tensor) function enlargeTensorReal8_Name(T,nameleg,newD,randomNumberScal)result(enlargeTensor)
		class(Tensor),intent(in)::T
		integer,intent(in)::newD
		character(len=*),intent(in)::nameleg
		real*8,intent(in)::randomNumberScal
		integer::D,rank,ithleg
		type(Dimension)::dimen
		rank=T%getRank()
		ithleg=T%FindOrder(nameleg)
		D=T%dim(ithleg)
		if(D.eq.newD)then
			enlargeTensor=T
			return
		end if
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.T%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,T%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if	
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call enlargeTensor%allocate(dimen,T%getType())
		if(T%ifDynamic())call enlargeTensor%Dynamic()
		call enlargeTensor%random([-randomNumberScal,randomNumberScal])
		call enlargeTensor%setValue(1,T%getTotalData(),T.pb.ithleg)
		enlargeTensor=enlargeTensor.pbi.ithleg
		return
	end function

	Type(Tensor) function enlargeTensorAllReal8(T,newD,randomNumberScal)result(enlargeTensor)
		class(Tensor),intent(in)::T
		integer,intent(in)::newD(:)
		real*8,intent(in)::randomNumberScal
		integer::rank,i
		rank=T%getRank()
		enlargeTensor=enlargeTensorReal8(T,1,newD(1),randomNumberScal)
		do i=2,rank
			enlargeTensor=enlargeTensorReal8(enlargeTensor,i,newD(i),randomNumberScal)
		end do
		return
	end function
	subroutine enlargeTensorReal8Routine(inoutT,ithleg,newD,randomNumberScal)
		class(Tensor),intent(inout)::inoutT
		integer,intent(in)::newD,ithleg
		real*8,intent(in)::randomNumberScal
		integer::D,rank
		type(Dimension)::dimen
		type(Tensor)::T
		logical::ifDynamic
		D=inoutT%dim(ithleg)
		if(D.eq.newD)return
		T=inoutT
		ifDynamic=inoutT%ifDynamic()
		rank=inoutT%getRank()
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.inoutT%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.inoutT%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,inoutT%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if	
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call inoutT%empty()
		call inoutT%allocate(dimen,T%getType())
		if(ifDynamic)call inoutT%Dynamic()
		call inoutT%random([-randomNumberScal,randomNumberScal])
		call inoutT%setValue(1,T%getTotalData(),T.pb.ithleg)
		inoutT=inoutT.pbi.ithleg
		return
	end subroutine

	subroutine enlargeTensorReal8_NameRoutine(inoutT,Nameleg,newD,randomNumberScal)
		class(Tensor),intent(inout)::inoutT
		character(len=*),intent(in)::Nameleg
		integer,intent(in)::newD
		real*8,intent(in)::randomNumberScal
		integer::D,rank,ithleg
		type(Dimension)::dimen
		type(Tensor)::T
		logical::ifDynamic
		ithleg=inoutT%FindOrder(nameleg)
		D=inoutT%dim(ithleg)
		if(D.eq.newD)return
		T=inoutT
		ifDynamic=inoutT%ifDynamic()
		rank=inoutT%getRank()
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.inoutT%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.inoutT%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,inoutT%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if		
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call inoutT%empty()
		call inoutT%allocate(dimen,T%getType())
		if(ifDynamic)call inoutT%Dynamic()
		call inoutT%random([-randomNumberScal,randomNumberScal])
		call inoutT%setValue(1,T%getTotalData(),T.pb.ithleg)
		inoutT=inoutT.pbi.ithleg
		return
	end subroutine

	subroutine enlargeTensorAllReal8Routine(inoutT,newD,randomNumberScal)
		class(Tensor),intent(inout)::inoutT
		integer,intent(in)::newD(:)
		real*8,intent(in)::randomNumberScal
		integer::rank,i
		rank=inoutT%getRank()
		call enlargeTensorReal8Routine(inoutT,1,newD(1),randomNumberScal)
		do i=2,rank
			call enlargeTensorReal8Routine(inoutT,i,newD(i),randomNumberScal)
		end do
		return
	end subroutine
	Type(Tensor) function enlargeTensorReal4(T,ithleg,newD,randomNumberScal)result(enlargeTensor)
		class(Tensor),intent(in)::T
		integer,intent(in)::newD,ithleg
		real*4,intent(in)::randomNumberScal
		integer::D,rank
		type(Dimension)::dimen
		rank=T%getRank()
		D=T%dim(ithleg)
		if(D.eq.newD)then
			enlargeTensor=T
			return
		end if
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.T%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,T%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if	
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call enlargeTensor%allocate(dimen,T%getType())
		if(T%ifDynamic())call enlargeTensor%Dynamic()
		call enlargeTensor%random([-randomNumberScal,randomNumberScal])
		call enlargeTensor%setValue(1,T%getTotalData(),T.pb.ithleg)
		enlargeTensor=enlargeTensor.pbi.ithleg
		return
	end function
	Type(Tensor) function enlargeTensorReal4_name(T,nameleg,newD,randomNumberScal)result(enlargeTensor)
		class(Tensor),intent(in)::T
		integer,intent(in)::newD
		character(len=*),intent(in)::nameleg
		real*4,intent(in)::randomNumberScal
		integer::D,rank,ithleg
		type(Dimension)::dimen
		rank=T%getRank()
		ithleg=T%FindOrder(nameleg)
		D=T%dim(ithleg)
		if(D.eq.newD)then
			enlargeTensor=T
			return
		end if
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.T%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,T%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if		
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call enlargeTensor%allocate(dimen,T%getType())
		if(T%ifDynamic())call enlargeTensor%Dynamic()
		call enlargeTensor%random([-randomNumberScal,randomNumberScal])
		call enlargeTensor%setValue(1,T%getTotalData(),T.pb.ithleg)
		enlargeTensor=enlargeTensor.pbi.ithleg
		return
	end function
	Type(Tensor) function enlargeTensorAllReal4(T,newD,randomNumberScal)result(enlargeTensor)
		class(Tensor),intent(in)::T
		integer,intent(in)::newD(:)
		real*4,intent(in)::randomNumberScal
		integer::rank,i
		rank=T%getRank()
		enlargeTensor=enlargeTensorReal4(T,1,newD(1),randomNumberScal)
		do i=2,rank
			enlargeTensor=enlargeTensorReal4(enlargeTensor,i,newD(i),randomNumberScal)
		end do
		return
	end function
	subroutine enlargeTensorReal4Routine(inoutT,ithleg,newD,randomNumberScal)
		class(Tensor),intent(inout)::inoutT
		integer,intent(in)::newD,ithleg
		real*4,intent(in)::randomNumberScal
		integer::D,rank
		type(Dimension)::dimen
		type(Tensor)::T
		logical::ifDynamic
		D=inoutT%dim(ithleg)
		if(D.eq.newD)return
		T=inoutT
		ifDynamic=inoutT%ifDynamic()
		rank=T%getRank()
		
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.inoutT%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.inoutT%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,inoutT%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if	
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call inoutT%empty()
		call inoutT%allocate(dimen,T%getType())
		if(ifDynamic)call inoutT%Dynamic()
		call inoutT%random([-randomNumberScal,randomNumberScal])
		call inoutT%setValue(1,T%getTotalData(),T.pb.ithleg)
		inoutT=inoutT.pbi.ithleg
		return
	end subroutine
	subroutine enlargeTensorReal4_nameRoutine(inoutT,nameleg,newD,randomNumberScal)
		class(Tensor),intent(inout)::inoutT
		integer,intent(in)::newD
		character(len=*),intent(in)::nameleg
		real*4,intent(in)::randomNumberScal
		integer::D,rank,ithleg
		type(Dimension)::dimen
		type(Tensor)::T
		logical::ifDynamic
		ithleg=inoutT%FindOrder(nameleg)
		D=inoutT%dim(ithleg)
		if(D.eq.newD)return
		T=inoutT
		ifDynamic=inoutT%ifDynamic()
		rank=T%getRank()
		
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.inoutT%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.inoutT%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,inoutT%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if		
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call inoutT%empty()
		call inoutT%allocate(dimen,T%getType())
		if(ifDynamic)call inoutT%Dynamic()
		call inoutT%random([-randomNumberScal,randomNumberScal])
		call inoutT%setValue(1,T%getTotalData(),T.pb.ithleg)
		inoutT=inoutT.pbi.ithleg
		return
	end subroutine
	subroutine enlargeTensorAllReal4Routine(inoutT,newD,randomNumberScal)
		class(Tensor),intent(inout)::inoutT
		integer,intent(in)::newD(:)
		real*4,intent(in)::randomNumberScal
		integer::rank,i
		rank=inoutT%getRank()
		call enlargeTensorReal4Routine(inoutT,1,newD(1),randomNumberScal)
		do i=2,rank
			call enlargeTensorReal4Routine(inoutT,i,newD(i),randomNumberScal)
		end do
		return
	end subroutine
	Type(Tensor) function enlargeTensorInt(T,ithleg,newD,randomNumberScal)result(enlargeTensor)
		class(Tensor),intent(in)::T
		integer,intent(in)::newD,ithleg
		integer,intent(in)::randomNumberScal
		integer::D,rank
		type(Dimension)::dimen
		rank=T%getRank()
		D=T%dim(ithleg)
		if(D.eq.newD)then
			enlargeTensor=T
			return
		end if
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.T%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,T%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if	
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call enlargeTensor%allocate(dimen,T%getType())
		if(T%ifDynamic())call enlargeTensor%Dynamic()
		call enlargeTensor%random([-randomNumberScal,randomNumberScal])
		call enlargeTensor%setValue(1,T%getTotalData(),T.pb.ithleg)
		enlargeTensor=enlargeTensor.pbi.ithleg
		return
	end function
	Type(Tensor) function enlargeTensorInt_name(T,nameleg,newD,randomNumberScal)result(enlargeTensor)
		class(Tensor),intent(in)::T
		integer,intent(in)::newD
		character(len=*),intent(in)::nameleg
		integer,intent(in)::randomNumberScal
		integer::D,rank,ithleg
		type(Dimension)::dimen
		rank=T%getRank()
		ithleg=T%findOrder(nameleg)
		D=T%dim(ithleg)
		if(D.eq.newD)then
			enlargeTensor=T
			return
		end if
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.T%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.T%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,T%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if		
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call enlargeTensor%allocate(dimen,T%getType())
		if(T%ifDynamic())call enlargeTensor%Dynamic()
		call enlargeTensor%random([-randomNumberScal,randomNumberScal])
		call enlargeTensor%setValue(1,T%getTotalData(),T.pb.ithleg)
		enlargeTensor=enlargeTensor.pbi.ithleg
		return
	end function
	Type(Tensor) function enlargeTensorAllInt(T,newD,randomNumberScal)result(enlargeTensor)
		class(Tensor),intent(in)::T
		integer,intent(in)::newD(:)
		integer,intent(in)::randomNumberScal
		integer::rank,i
		rank=T%getRank()
		enlargeTensor=enlargeTensorInt(T,1,newD(1),randomNumberScal)
		do i=2,rank
			enlargeTensor=enlargeTensorInt(enlargeTensor,i,newD(i),randomNumberScal)
		end do
		return
	end function
	subroutine enlargeTensorIntRoutine(inoutT,ithleg,newD,randomNumberScal)
		class(Tensor),intent(inout)::inoutT
		integer,intent(in)::newD,ithleg
		integer,intent(in)::randomNumberScal
		integer::D,rank
		type(Dimension)::dimen
		type(Tensor)::T
		logical::ifDynamic
		D=inoutT%dim(ithleg)
		if(D.eq.newD)return
		T=inoutT
		ifDynamic=inoutT%ifDynamic()
		rank=T%getRank()
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.inoutT%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.inoutT%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,inoutT%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if	
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call inoutT%empty()
		call inoutT%allocate(dimen,T%getType())
		if(ifDynamic)call inoutT%Dynamic()
		call inoutT%random([-randomNumberScal,randomNumberScal])
		call inoutT%setValue(1,T%getTotalData(),T.pb.ithleg)
		inoutT=inoutT.pbi.ithleg
		return
	end subroutine
	subroutine enlargeTensorInt_nameRoutine(inoutT,nameleg,newD,randomNumberScal)
		class(Tensor),intent(inout)::inoutT
		integer,intent(in)::newD
		character(len=*),intent(in)::nameleg
		integer,intent(in)::randomNumberScal
		integer::D,rank,ithleg
		type(Dimension)::dimen
		type(Tensor)::T
		logical::ifDynamic
		ithleg=inoutT%findOrder(nameleg)
		D=inoutT%dim(ithleg)
		if(D.eq.newD)return
		T=inoutT
		ifDynamic=inoutT%ifDynamic()
		rank=T%getRank()
		if(D.gt.newD)then
			call writemess('The input dimension is smaller the one in the Tensor',-1)
			call writemess('ERROR in enlargeTensor',-1)
			call error_stop
		end if
		if(.not.inoutT%getFlag())then
			call writemess('There is no data in the tensor when enlarge D')
			call error_stop
		end if
		if(.not.inoutT%TenDim%if_simple_dimension())then
			call writemess('enlarge D work only on the simple dimension(you can not fuse the Tensor)')
			call error_stop
		end if
		if(rank.eq.1)then
			dimen= [newD] 
		else
			call dimpermute_backwards(dimen,inoutT%TenDim,ithleg)
			dimen=(dimen.subDim.[1,rank-1]) + [newD] 
		end if		
		if(T%outNameFlag().ne.0)then
			call dimen%setName(rank,T%getName(ithleg))
		end if
		call inoutT%empty()
		call inoutT%allocate(dimen,T%getType())
		if(ifDynamic)call inoutT%Dynamic()
		call inoutT%random([-randomNumberScal,randomNumberScal])
		call inoutT%setValue(1,T%getTotalData(),T.pb.ithleg)
		inoutT=inoutT.pbi.ithleg
		return
	end subroutine
	subroutine enlargeTensorAllIntRoutine(inoutT,newD,randomNumberScal)
		class(Tensor),intent(inout)::inoutT
		integer,intent(in)::newD(:)
		integer,intent(in)::randomNumberScal
		integer::rank,i
		rank=inoutT%getRank()
		call enlargeTensorIntRoutine(inoutT,1,newD(1),randomNumberScal)
		do i=2,rank
			call enlargeTensorIntRoutine(inoutT,i,newD(i),randomNumberScal)
		end do
		return
	end subroutine
	
	subroutine iTpointer2(T,p,i,j)
		class(Tensor),intent(in)::T
		integer,pointer,intent(inout)::p(:,:)
		integer,intent(in)::i(2),j(2)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.1)then
			call writemess('The type of Tensor is not  integer',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,d)
		return
	end subroutine
	subroutine iTpointer2_(T,p)
		class(Tensor),intent(in)::T
		integer,pointer,intent(inout)::p(:,:)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.1)then
			call writemess('The type of Tensor is not  integer',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],d)
		return
	end subroutine
	subroutine sTpointer2(T,p,i,j)
		class(Tensor),intent(in)::T
		real*4,pointer,intent(inout)::p(:,:)
		integer,intent(in)::i(2),j(2)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.2)then
			call writemess('The Tensor is of real*4, one should use the real*4 pointer',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,d)
		return
	end subroutine
	subroutine sTpointer2_(T,p)
		class(Tensor),intent(in)::T
		real*4,pointer,intent(inout)::p(:,:)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.2)then
			call writemess('The type of Tensor is not  real*4',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],d)
		return
	end subroutine
	subroutine dTpointer2(T,p,i,j)
		class(Tensor),intent(in)::T
		real*8,pointer,intent(inout)::p(:,:)
		integer,intent(in)::i(2),j(2)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.3)then
			call writemess('The type of Tensor is not  real*8',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,d)
		return
	end subroutine
	subroutine dTpointer2_(T,p)
		class(Tensor),intent(in)::T
		real*8,pointer,intent(inout)::p(:,:)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.3)then
			call writemess('The type of Tensor is not  real*8',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],d)
		return
	end subroutine
	subroutine cTpointer2(T,p,i,j)
		class(Tensor),intent(in)::T
		complex(kind=4),pointer,intent(inout)::p(:,:)
		integer,intent(in)::i(2),j(2)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.4)then
			call writemess('The type of Tensor is not  complex(kind=4)',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,d)
		return
	end subroutine
	subroutine cTpointer2_(T,p)
		class(Tensor),intent(in)::T
		complex(kind=4),pointer,intent(inout)::p(:,:)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.4)then
			call writemess('The type of Tensor is not  complex(kind=4)',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],d)
		return
	end subroutine
	subroutine zTpointer2(T,p,i,j)
		class(Tensor),intent(in)::T
		complex(kind=8),pointer,intent(inout)::p(:,:)
		integer,intent(in)::i(2),j(2)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.5)then
			call writemess('The type of Tensor is not  complex(kind=8)',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,d)
		return
	end subroutine
	subroutine zTpointer2_(T,p)
		class(Tensor),intent(in)::T
		complex(kind=8),pointer,intent(inout)::p(:,:)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.5)then
			call writemess('The type of Tensor is not  complex(kind=8)',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],d)
		return
	end subroutine
	subroutine lTpointer2(T,p,i,j)
		class(Tensor),intent(in)::T
		logical,pointer,intent(inout)::p(:,:)
		integer,intent(in)::i(2),j(2)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.6)then
			call writemess('The type of Tensor is not  logical',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,d)
		return
	end subroutine
	subroutine lTpointer2_(T,p)
		class(Tensor),intent(in)::T
		logical,pointer,intent(inout)::p(:,:)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.6)then
			call writemess('The type of Tensor is not  logical',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],d)
		return
	end subroutine
	subroutine aTpointer2(T,p,i,j)
		class(Tensor),intent(in)::T
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:,:)
		integer,intent(in)::i(2),j(2)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.7)then
			call writemess('The type of Tensor is not  character(len=max_len_of_char_in_TData)',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,d)
		return
	end subroutine
	subroutine aTpointer2_(T,p)
		class(Tensor),intent(in)::T
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:,:)
		integer::d(2)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.7)then
			call writemess('The type of Tensor is not  character(len=max_len_of_char_in_TData)',-1)
			call error_stop
		end if
		if(T%getRank().ne.2)then
			call writemess('The rank of the Tensor is not 2, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],d)
		return
	end subroutine
	
	
	subroutine iTpointer3(T,p,i,j,k)
		class(Tensor),intent(in)::T
		integer,pointer,intent(inout)::p(:,:,:)
		integer,intent(in)::i(2),j(2),k(2)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.1)then
			call writemess('The type of Tensor is not  integer',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,d)
		return
	end subroutine
	subroutine iTpointer3_(T,p)
		class(Tensor),intent(in)::T
		integer,pointer,intent(inout)::p(:,:,:)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.1)then
			call writemess('The type of Tensor is not  integer',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],d)
		return
	end subroutine
	
	subroutine sTpointer3(T,p,i,j,k)
		class(Tensor),intent(in)::T
		real*4,pointer,intent(inout)::p(:,:,:)
		integer,intent(in)::i(2),j(2),k(2)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.2)then
			call writemess('The type of Tensor is not  real*4',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,d)
		return
	end subroutine
	subroutine sTpointer3_(T,p)
		class(Tensor),intent(in)::T
		real*4,pointer,intent(inout)::p(:,:,:)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.2)then
			call writemess('The type of Tensor is not  real*4',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],d)
		return
	end subroutine
	subroutine dTpointer3(T,p,i,j,k)
		class(Tensor),intent(in)::T
		real*8,pointer,intent(inout)::p(:,:,:)
		integer,intent(in)::i(2),j(2),k(2)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.3)then
			call writemess('The type of Tensor is not  real*8',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,d)
		return
	end subroutine
	subroutine dTpointer3_(T,p)
		class(Tensor),intent(in)::T
		real*8,pointer,intent(inout)::p(:,:,:)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.3)then
			call writemess('The type of Tensor is not  real*8',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],d)
		return
	end subroutine
	
	subroutine cTpointer3(T,p,i,j,k)
		class(Tensor),intent(in)::T
		complex(kind=4),pointer,intent(inout)::p(:,:,:)
		integer,intent(in)::i(2),j(2),k(2)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.4)then
			call writemess('The type of Tensor is not  complex(kind=4)',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,d)
		return
	end subroutine
	subroutine cTpointer3_(T,p)
		class(Tensor),intent(in)::T
		complex(kind=4),pointer,intent(inout)::p(:,:,:)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.4)then
			call writemess('The type of Tensor is not  complex(kind=4)',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],d)
		return
	end subroutine
	
	subroutine zTpointer3(T,p,i,j,k)
		class(Tensor),intent(in)::T
		complex(kind=8),pointer,intent(inout)::p(:,:,:)
		integer,intent(in)::i(2),j(2),k(2)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.5)then
			call writemess('The type of Tensor is not  complex(kind=8)',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,d)
		return
	end subroutine
	subroutine zTpointer3_(T,p)
		class(Tensor),intent(in)::T
		complex(kind=8),pointer,intent(inout)::p(:,:,:)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.5)then
			call writemess('The type of Tensor is not  complex(kind=8)',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],d)
		return
	end subroutine
	
	subroutine lTpointer3(T,p,i,j,k)
		class(Tensor),intent(in)::T
		logical,pointer,intent(inout)::p(:,:,:)
		integer,intent(in)::i(2),j(2),k(2)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.6)then
			call writemess('The type of Tensor is not  logical',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,d)
		return
	end subroutine
	subroutine lTpointer3_(T,p)
		class(Tensor),intent(in)::T
		logical,pointer,intent(inout)::p(:,:,:)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.6)then
			call writemess('The type of Tensor is not  logical',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],d)
		return
	end subroutine
	
	subroutine aTpointer3(T,p,i,j,k)
		class(Tensor),intent(in)::T
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:,:,:)
		integer,intent(in)::i(2),j(2),k(2)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.7)then
			call writemess('The type of Tensor is not  character(len=max_len_of_char_in_TData)',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,d)
		return
	end subroutine
	subroutine aTpointer3_(T,p)
		class(Tensor),intent(in)::T
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:,:,:)
		integer::d(3)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.7)then
			call writemess('The type of Tensor is not  character(len=max_len_of_char_in_TData)',-1)
			call error_stop
		end if
		if(T%getRank().ne.3)then
			call writemess('The rank of the Tensor is not 3, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],d)
		return
	end subroutine

	subroutine iTpointer4(T,p,i,j,k,l)
		class(Tensor),intent(in)::T
		integer,pointer,intent(inout)::p(:,:,:,:)
		integer,intent(in)::i(2),j(2),k(2),l(2)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.1)then
			call writemess('The type of Tensor is not  integer',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,l,d)
		return
	end subroutine
	subroutine iTpointer4_(T,p)
		class(Tensor),intent(in)::T
		integer,pointer,intent(inout)::p(:,:,:,:)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.1)then
			call writemess('The type of Tensor is not  integer',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],[1,d(4)],d)
		return
	end subroutine

	subroutine sTpointer4(T,p,i,j,k,l)
		class(Tensor),intent(in)::T
		real*4,pointer,intent(inout)::p(:,:,:,:)
		integer,intent(in)::i(2),j(2),k(2),l(2)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.2)then
			call writemess('The type of Tensor is not  real*4',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,l,d)
		return
	end subroutine
	subroutine sTpointer4_(T,p)
		class(Tensor),intent(in)::T
		real*4,pointer,intent(inout)::p(:,:,:,:)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.2)then
			call writemess('The type of Tensor is not  real*4',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],[1,d(4)],d)
		return
	end subroutine
	
	subroutine dTpointer4(T,p,i,j,k,l)
		class(Tensor),intent(in)::T
		real*8,pointer,intent(inout)::p(:,:,:,:)
		integer,intent(in)::i(2),j(2),k(2),l(2)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.3)then
			call writemess('The type of Tensor is not  real*8',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,l,d)
		return
	end subroutine
	subroutine dTpointer4_(T,p)
		class(Tensor),intent(in)::T
		real*8,pointer,intent(inout)::p(:,:,:,:)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.3)then
			call writemess('The type of Tensor is not  real*8',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],[1,d(4)],d)
		return
	end subroutine

	subroutine cTpointer4(T,p,i,j,k,l)
		class(Tensor),intent(in)::T
		complex*8,pointer,intent(inout)::p(:,:,:,:)
		integer,intent(in)::i(2),j(2),k(2),l(2)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.4)then
			call writemess('The type of Tensor is not  complex(kind=4)',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,l,d)
		return
	end subroutine
	subroutine cTpointer4_(T,p)
		class(Tensor),intent(in)::T
		complex*8,pointer,intent(inout)::p(:,:,:,:)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.4)then
			call writemess('The type of Tensor is not  complex(kind=4)',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],[1,d(4)],d)
		return
	end subroutine
	subroutine zTpointer4(T,p,i,j,k,l)
		class(Tensor),intent(in)::T
		complex*16,pointer,intent(inout)::p(:,:,:,:)
		integer,intent(in)::i(2),j(2),k(2),l(2)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.5)then
			call writemess('The type of Tensor is not  complex(kind=8)',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,l,d)
		return
	end subroutine
	subroutine zTpointer4_(T,p)
		class(Tensor),intent(in)::T
		complex*16,pointer,intent(inout)::p(:,:,:,:)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.5)then
			call writemess('The type of Tensor is not  complex(kind=8)',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],[1,d(4)],d)
		return
	end subroutine
	subroutine lTpointer4(T,p,i,j,k,l)
		class(Tensor),intent(in)::T
		logical,pointer,intent(inout)::p(:,:,:,:)
		integer,intent(in)::i(2),j(2),k(2),l(2)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.6)then
			call writemess('The type of Tensor is not  logical',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,l,d)
		return
	end subroutine
	subroutine lTpointer4_(T,p)
		class(Tensor),intent(in)::T
		logical,pointer,intent(inout)::p(:,:,:,:)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.6)then
			call writemess('The type of Tensor is not  logical',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],[1,d(4)],d)
		return
	end subroutine
	subroutine aTpointer4(T,p,i,j,k,l)
		class(Tensor),intent(in)::T
		character(len=characterLen),pointer,intent(inout)::p(:,:,:,:)
		integer,intent(in)::i(2),j(2),k(2),l(2)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.7)then
			call writemess('The type of Tensor is not  character(len=characterLen)',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,i,j,k,l,d)
		return
	end subroutine
	subroutine aTpointer4_(T,p)
		class(Tensor),intent(in)::T
		character(len=characterLen),pointer,intent(inout)::p(:,:,:,:)
		integer::d(4)
		if(T%gettotalData().eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%getType().ne.7)then
			call writemess('The type of Tensor is not  character(len=characterLen)',-1)
			call error_stop
		end if
		if(T%getRank().ne.4)then
			call writemess('The rank of the Tensor is not 4, input error dimension pointer',-1)
			call error_stop
		end if
		d=T%dim()
		call T%pointerDim(p,[1,d(1)],[1,d(2)],[1,d(3)],[1,d(4)],d)
		return
	end subroutine
	

	subroutine sortTensor1(T)
		class(Tensor),intent(inout)::T
		logical::increase,realpart
		increase=.true.
		realpart=.true.
		call sortTData2(T%TData,increase,realpart)
		return
	end subroutine
	subroutine sortTensor2(T,indices)
		class(Tensor),intent(inout)::T
		type(Tensor),intent(inout)::indices
		logical::increase,realpart
		increase=.true.
		realpart=.true.
		call indices%empty()
		call indices%allocate([T%getTotalData()],'integer')
		call sortTData2(T%TData,increase,realpart)
		return
	end subroutine
	
	subroutine sortTensor3(T,increase)
		class(Tensor),intent(inout)::T
		logical,intent(in)::increase
		logical::realpart
		realpart=.true.
		call sortTData2(T%TData,increase,realpart)
		return
	end subroutine
	subroutine sortTensor4(T,indices,increase)
		class(Tensor),intent(inout)::T
		type(Tensor),intent(inout)::indices
		logical,intent(in)::increase
		logical::realpart
		realpart=.true.
		call indices%empty()
		call indices%allocate([T%getTotalData()],'integer')
		call sortTData2(T%TData,increase,realpart)
		return
	end subroutine
	
	subroutine sortTensor5(T,increase,realpart)
		class(Tensor),intent(inout)::T
		logical,intent(in)::increase,realpart
		call sortTData2(T%TData,increase,realpart)
		return
	end subroutine
	subroutine sortTensor6(T,indices,increase,realpart)
		class(Tensor),intent(inout)::T
		type(Tensor),intent(inout)::indices
		logical,intent(in)::increase,realpart
		call indices%empty()
		call indices%allocate([T%getTotalData()],'integer')
		call sortTData2(T%TData,increase,realpart)
		return
	end subroutine
	
	

	integer function iwhichindex(T,element)result(whichindex)
		class(Tensor),intent(in)::T
		integer,intent(in)::element
		if(.not.T%getflag())then
			whichindex=0
			return
		end if
		select case(T%getType())
			case (1)
				do whichindex=1,T%getTotalData()
					if(element.eq.T%ii(whichindex))return
				end do
			case default
				call writemess('The Tensor is integer, input type error, (which)',-1)
				call error_stop
		end select
		whichindex=0
		return
	end function
	integer function swhichindex(T,element)result(whichindex)
		class(Tensor),intent(in)::T
		real*4,intent(in)::element
		if(.not.T%getflag())then
			whichindex=0
			return
		end if
		select case(T%getType())
			case (2)
				do whichindex=1,T%getTotalData()
					if(abs(element-T%si(whichindex)).le. default_zero_real_number)return
				end do
			case default
				call writemess('The Tensor is real, input type error, (which)',-1)
				call error_stop
		end select
		whichindex=0
		return
	end function
	
	integer function dwhichindex(T,element)result(whichindex)
		class(Tensor),intent(in)::T
		real*8,intent(in)::element
		if(.not.T%getflag())then
			whichindex=0
			return
		end if
		select case(T%getType())
			case (3)
				do whichindex=1,T%getTotalData()
					if(dabs(element-T%di(whichindex)).le. default_zero_double_number)return
				end do
			case default
				call writemess('The Tensor is real*8, input type error, (which)',-1)
				call error_stop
		end select
		whichindex=0
		return
	end function
	
	integer function cwhichindex(T,element)result(whichindex)
		class(Tensor),intent(in)::T
		complex*8,intent(in)::element
		if(.not.T%getflag())then
			whichindex=0
			return
		end if
		select case(T%getType())
			case (4)
				do whichindex=1,T%getTotalData()
					if(cabs(element-T%ci(whichindex)).le. default_zero_real_number)return
				end do
			case default
				call writemess('The Tensor is complex(kind=4), input type error, (which)',-1)
				call error_stop
		end select
		whichindex=0
		return
	end function
	
	integer function zwhichindex(T,element)result(whichindex)
		class(Tensor),intent(in)::T
		complex*16,intent(in)::element
		if(.not.T%getflag())then
			whichindex=0
			return
		end if
		select case(T%getType())
			case (5)
				do whichindex=1,T%getTotalData()
					if(cdabs(element-T%zi(whichindex)).le. default_zero_double_number)return
				end do
			case default
				call writemess('The Tensor is complex(kind=4), input type error, (which)',-1)
				call error_stop
		end select
		whichindex=0
		return
	end function
	integer function awhichindex(T,element)result(whichindex)
		class(Tensor),intent(in)::T
		character(len=*),intent(in)::element
		if(.not.T%getflag())then
			whichindex=0
			return
		end if
		select case(T%getType())
			case (7)
				do whichindex=1,T%getTotalData()
					if(element.equ.T%ai(whichindex))return
				end do
			case default
				call writemess('The Tensor is character, input type error, (which)',-1)
				call error_stop
		end select
		whichindex=0
		return
	end function
	
	integer function awhichindex2(T,element,maxlen)result(whichindex)
		class(Tensor),intent(in)::T
		integer,intent(in)::maxlen
		character(len=*),intent(in)::element
		if(.not.T%getflag())then
			whichindex=0
			return
		end if
		select case(T%getType())
			case (7)
				do whichindex=1,maxlen
					if(element.equ.T%ai(whichindex))return
				end do
			case default
				call writemess('The Tensor is character, input type error, (which)',-1)
				call error_stop
		end select
		whichindex=0
		return
	end function
	

	
	type(Tensor) function sqrt_Tensor(T)result(Res)
		type(Tensor),intent(in) :: T
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(sqrt)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess("ONLY FOR Tensor with one element,(sqrt)!!",-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				Res=sqrt(T%si(1))
			case (2)
				Res=sqrt(T%si(1))
			case (3)
				Res=dsqrt(T%di(1))
			case (4)
				Res=csqrt(T%ci(1))
			case (5)
				Res=cdsqrt(T%zi(1))
			case default
				call writemess("ERROR in sqrt,error type",-1)
				call error_stop()
		end select
		return
	end function
				
	logical function isnanTensor(T)
		type(Tensor),intent(in)::T
		isnanTensor=isnanTData(T%Tdata)
		return
	end function
!***************** equal_of_Tensor *****************
	logical function equal_of_Tensor(T1,T2)
		type(Tensor),intent(in) :: T1,T2
		type(Tensor) :: T
		integer :: l,flag
		equal_of_Tensor=.true.
		if(.not.T1%getFlag())then
			call writemess('There is no data in T1,(.eq.)',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in T2,(.eq.)',-1)
			call error_stop()
		end if
		if(T1%rank.ne.T2%rank) then
			equal_of_Tensor=.false.
			return
		end if
		if(.not.(T1%TenDim.equ.T2%TenDim)) then
			equal_of_Tensor=.false.
			return
		end if
		flag=T1%getType()*10+T2%getType()
		select case(T1%getType())
			case (11)
				do l=1,T1%gettotalData()
					if(T1%ii(l).ne.T2%ii(l))then
						equal_of_Tensor=.false.
					return
					end if
				end do
			case default
				T=T1-T2
				if(T%dmax('maxa').gt.5e-8)then
					equal_of_Tensor=.false.
					return
				end if
				return
		end select
	end function
	logical function T_eq_int(T,num)
		type(Tensor),intent(in) :: T
		integer,intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.eq.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			T_eq_int=.false.
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).eq.num
			case (2)
				T_eq_int=T%si(1).eq.num
			case (3)
				T_eq_int=T%di(1).eq.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.eq.)!",-1)
				call error_stop()
		end select
		return
	end function
	logical function int_eq_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		integer,intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.eq.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			T_eq_int=.false.
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).eq.num
			case (2)
				T_eq_int=T%si(1).eq.num
			case (3)
				T_eq_int=T%di(1).eq.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.eq.)!",-1)
				call error_stop()
		end select
		return
	end function
	logical function T_eq_real4(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=4),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.eq.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			T_eq_int=.false.
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).eq.num
			case (2)
				T_eq_int=T%si(1).eq.num
			case (3)
				T_eq_int=T%di(1).eq.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.eq.)!",-1)
				call error_stop()
		end select
		return
	end function
	logical function real4_eq_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=4),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.eq.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			T_eq_int=.false.
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).eq.num
			case (2)
				T_eq_int=T%si(1).eq.num
			case (3)
				T_eq_int=T%di(1).eq.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.eq.)!",-1)
				call error_stop()
		end select
		return
	end function			
	logical function T_eq_real8(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=8),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.eq.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			T_eq_int=.false.
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).eq.num
			case (2)
				T_eq_int=T%si(1).eq.num
			case (3)
				T_eq_int=T%di(1).eq.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.eq.)!",-1)
				call error_stop()
		end select
		return
	end function
	logical function real8_eq_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=8),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.eq.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			T_eq_int=.false.
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).eq.num
			case (2)
				T_eq_int=T%si(1).eq.num
			case (3)
				T_eq_int=T%di(1).eq.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.eq.)!",-1)
				call error_stop()
		end select
		return
	end function		
	!use only there is one element and Tensor are real or integer
	logical function le_of_Tensor(T1,T2)
		type(Tensor),intent(in) :: T1,T2
		integer :: l
		if(.not.T1%getFlag())then
			call writemess('There is no data in T1,(.le.)',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in T2,(.le.)',-1)
			call error_stop()
		end if
		if(T1%gettotalData().gt.1)then
			call writemess("ONLY FOR Tensor with one element,(.le.)!!",-1)
			call error_stop()
		end if
		if(T1%gettotalData().gt.1)then
			call writemess("ONLY FOR Tensor with one element,(.le.)!!",-1)
			call error_stop()
		end if
		if(T1%getType().gt.3)then
			call writemess("ONLY FOR Tensor of integer or real,(.le.)!",-1)
			call error_stop()
		end if
		if(T2%getType().gt.3)then
			call writemess("ONLY FOR Tensor of integer or real,(.le.)!",-1)
			call error_stop()
		end if
		le_of_Tensor=T1%di(1).le.T2%di(1)
		return
	end function
	
	logical function T_le_int(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		integer,intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.le.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.le.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).le.num
			case (2)
				T_eq_int=T%si(1).le.num
			case (3)
				T_eq_int=T%di(1).le.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.le.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function int_le_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		integer,intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.le.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.le.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.le.T%ii(1)
			case (2)
				T_eq_int=num.le.T%si(1)
			case (3)
				T_eq_int=num.le.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.le.)!",-1)
				call error_stop()
		end select
		return
	end function
	
	logical function T_le_real4(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=4),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.le.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.le.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).le.num
			case (2)
				T_eq_int=T%si(1).le.num
			case (3)
				T_eq_int=T%di(1).le.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.le.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function real4_le_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=4),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.le.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.le.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.le.T%ii(1)
			case (2)
				T_eq_int=num.le.T%si(1)
			case (3)
				T_eq_int=num.le.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.le.)!",-1)
				call error_stop()
		end select
		return
	end function
	logical function T_le_real8(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=8),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.le.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.le.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).le.num
			case (2)
				T_eq_int=T%si(1).le.num
			case (3)
				T_eq_int=T%di(1).le.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.le.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function real8_le_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=8),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.le.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.le.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.le.T%ii(1)
			case (2)
				T_eq_int=num.le.T%si(1)
			case (3)
				T_eq_int=num.le.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.le.)!",-1)
				call error_stop()
		end select
		return
	end function
	
	logical function lt_of_Tensor(T1,T2)
		type(Tensor),intent(in) :: T1,T2
		integer :: l
		if(.not.T1%getFlag())then
			call writemess('There is no data in T1,(.lt.)',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in T2,(.lt.)',-1)
			call error_stop()
		end if
		if(T1%gettotalData().gt.1)then
			call writemess("ONLY FOR Tensor with one element,(.lt.)!!",-1)
			call error_stop()
		end if
		if(T1%gettotalData().gt.2)then
			call writemess("ONLY FOR Tensor with one element,(.lt.)!!",-1)
			call error_stop()
		end if
		if(T1%getType().gt.3)then
			call writemess("ONLY FOR Tensor of integer or real,(.lt.)!",-1)
			call error_stop()
		end if
		if(T2%getType().gt.3)then
			call writemess("ONLY FOR Tensor of integer or real,(.lt.)!",-1)
			call error_stop()
		end if
		lt_of_Tensor=T1%di(1).lt.T2%di(1)
		return
	end function
	
	logical function T_lt_int(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		integer,intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.lt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.lt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).lt.num
			case (2)
				T_eq_int=T%si(1).lt.num
			case (3)
				T_eq_int=T%di(1).lt.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.lt.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function int_lt_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		integer,intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.lt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.lt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.lt.T%ii(1)
			case (2)
				T_eq_int=num.lt.T%si(1)
			case (3)
				T_eq_int=num.lt.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.lt.)!",-1)
				call error_stop()
		end select
		return
	end function
	
	logical function T_lt_real4(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=4),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.lt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.lt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).lt.num
			case (2)
				T_eq_int=T%si(1).lt.num
			case (3)
				T_eq_int=T%di(1).lt.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.lt.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function real4_lt_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=4),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.lt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.lt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.lt.T%ii(1)
			case (2)
				T_eq_int=num.lt.T%si(1)
			case (3)
				T_eq_int=num.lt.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.lt.)!",-1)
				call error_stop()
		end select
		return
	end function
	logical function T_lt_real8(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=8),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.lt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.lt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).lt.num
			case (2)
				T_eq_int=T%si(1).lt.num
			case (3)
				T_eq_int=T%di(1).lt.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.lt.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function real8_lt_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=8),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.lt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.lt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.lt.T%ii(1)
			case (2)
				T_eq_int=num.lt.T%si(1)
			case (3)
				T_eq_int=num.lt.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.lt.)!",-1)
				call error_stop()
		end select
		return
	end function
	
	
	logical function ge_of_Tensor(T1,T2)
		type(Tensor),intent(in) :: T1,T2
		integer :: l
		if(.not.T1%getFlag())then
			call writemess('There is no data in T1,(.ge.)',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in T2,(.ge.)',-1)
			call error_stop()
		end if
		if(T1%gettotalData().gt.1)then
			call writemess("ONLY FOR Tensor with one element,(.ge.)!!",-1)
			call error_stop()
		end if
		if(T1%gettotalData().gt.2)then
			call writemess("ONLY FOR Tensor with one element,(.ge.)!!",-1)
			call error_stop()
		end if
		if(T1%getType().gt.3)then
			call writemess("ONLY FOR Tensor of integer or real,(.ge.)!",-1)
			call error_stop()
		end if
		if(T2%getType().gt.3)then
			call writemess("ONLY FOR Tensor of integer or real,(.ge.)!",-1)
			call error_stop()
		end if
		ge_of_Tensor=T1%di(1).ge.T2%di(1)
		return
	end function
	
	logical function T_ge_int(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		integer,intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.ge.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.ge.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).ge.num
			case (2)
				T_eq_int=T%si(1).ge.num
			case (3)
				T_eq_int=T%di(1).ge.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.ge.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function int_ge_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		integer,intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.ge.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.ge.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.ge.T%ii(1)
			case (2)
				T_eq_int=num.ge.T%si(1)
			case (3)
				T_eq_int=num.ge.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.ge.)!",-1)
				call error_stop()
		end select
		return
	end function
	
	logical function T_ge_real4(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=4),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.ge.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.ge.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).ge.num
			case (2)
				T_eq_int=T%si(1).ge.num
			case (3)
				T_eq_int=T%di(1).ge.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.ge.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function real4_ge_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=4),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.ge.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.ge.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.ge.T%ii(1)
			case (2)
				T_eq_int=num.ge.T%si(1)
			case (3)
				T_eq_int=num.ge.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.ge.)!",-1)
				call error_stop()
		end select
		return
	end function
	logical function T_ge_real8(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=8),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.ge.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.ge.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).ge.num
			case (2)
				T_eq_int=T%si(1).ge.num
			case (3)
				T_eq_int=T%di(1).ge.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.ge.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function real8_ge_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=8),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.ge.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.ge.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.ge.T%ii(1)
			case (2)
				T_eq_int=num.ge.T%si(1)
			case (3)
				T_eq_int=num.ge.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.ge.)!",-1)
				call error_stop()
		end select
		return
	end function
	
	logical function gt_of_Tensor(T1,T2)
		type(Tensor),intent(in) :: T1,T2
		integer :: l
		if(.not.T1%getFlag())then
			call writemess('There is no data in T1,(.gt.)',-1)
			call error_stop()
		end if
		if(.not.T2%getFlag())then
			call writemess('There is no data in T2,(.gt.)',-1)
			call error_stop()
		end if
		if(T1%gettotalData().gt.1)then
			call writemess("ONLY FOR Tensor with one element,(.gt.)!!",-1)
			call error_stop()
		end if
		if(T1%gettotalData().gt.2)then
			call writemess("ONLY FOR Tensor with one element,(.gt.)!!",-1)
			call error_stop()
		end if
		if(T1%getType().gt.3)then
			call writemess("ONLY FOR Tensor of integer or real,(.gt.)!",-1)
			call error_stop()
		end if
		if(T2%getType().gt.3)then
			call writemess("ONLY FOR Tensor of integer or real,(.gt.)!",-1)
			call error_stop()
		end if
		gt_of_Tensor=T1%di(1).gt.T2%di(1)
		return
	end function
	
	logical function T_gt_int(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		integer,intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.gt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.gt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).gt.num
			case (2)
				T_eq_int=T%si(1).gt.num
			case (3)
				T_eq_int=T%di(1).gt.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.gt.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function int_gt_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		integer,intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.gt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.gt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.gt.T%ii(1)
			case (2)
				T_eq_int=num.gt.T%si(1)
			case (3)
				T_eq_int=num.gt.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.gt.)!",-1)
				call error_stop()
		end select
		return
	end function
	
	logical function T_gt_real4(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=4),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.gt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.gt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).gt.num
			case (2)
				T_eq_int=T%si(1).gt.num
			case (3)
				T_eq_int=T%di(1).gt.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.gt.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function real4_gt_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=4),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.gt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.gt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.gt.T%ii(1)
			case (2)
				T_eq_int=num.gt.T%si(1)
			case (3)
				T_eq_int=num.gt.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.gt.)!",-1)
				call error_stop()
		end select
		return
	end function
	logical function T_gt_real8(T,num)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=8),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T1,(.gt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T1,(.gt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=T%ii(1).gt.num
			case (2)
				T_eq_int=T%si(1).gt.num
			case (3)
				T_eq_int=T%di(1).gt.num
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.gt.)!",-1)
				call error_stop()
		end select
		return
	end function	
	logical function real8_gt_T(num,T)result(T_eq_int)
		type(Tensor),intent(in) :: T
		real(kind=8),intent(in)::num
		if(.not.T%getFlag())then
			call writemess('There is no data in T2,(.gt.)',-1)
			call error_stop()
		end if
		if(T%gettotalData().gt.1)then
			call writemess('There is no data in T2,(.gt.)',-1)
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				T_eq_int=num.gt.T%ii(1)
			case (2)
				T_eq_int=num.gt.T%si(1)
			case (3)
				T_eq_int=num.gt.T%di(1)
			case default
				call writemess("ONLY FOR Tensor of integer or real,(.gt.)!",-1)
				call error_stop()
		end select
		return
	end function
!***************   dot   ******************
!		return <phi1|phi2>
!	
	integer function idotTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		if(.not.phi1%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		if(.not.phi2%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		N1=getTotalData(phi1)
		N2=getTotalData(phi2)
		if(N1.ne.N2) then
			write(*,*)"ERROR in dot"
			write(*,*)"Total number of input is"
			write(*,*)N1,N2
			call error_stop()
		end if
		call  product_dotc_int(dotTensor,phi1%TData,phi2%TData)	
		RETURN
	end function
	real(kind=4) function sdotTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		if(.not.phi1%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		if(.not.phi2%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		N1=getTotalData(phi1)
		N2=getTotalData(phi2)
		if(N1.ne.N2) then
			write(*,*)"ERROR in dot"
			write(*,*)"Total number of input is"
			write(*,*)N1,N2
			call error_stop()
		end if
		call  product_dotc_real4(dotTensor,phi1%TData,phi2%TData)	
		RETURN
	end function
	real(kind=8) function ddotTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		if(.not.phi1%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		if(.not.phi2%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		N1=getTotalData(phi1)
		N2=getTotalData(phi2)
		if(N1.ne.N2) then
			write(*,*)"ERROR in dot"
			write(*,*)"Total number of input is"
			write(*,*)N1,N2
			call error_stop()
		end if
		call  product_dotc_real8(dotTensor,phi1%TData,phi2%TData)	
		RETURN
	end function
	complex(kind=4) function cdotTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		if(.not.phi1%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		if(.not.phi2%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		N1=getTotalData(phi1)
		N2=getTotalData(phi2)
		if(N1.ne.N2) then
			write(*,*)"ERROR in dot"
			write(*,*)"Total number of input is"
			write(*,*)N1,N2
			call error_stop()
		end if
		call  product_dotc_com4(dotTensor,phi1%TData,phi2%TData)	
		RETURN
	end function
	complex(kind=8) function zdotTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		if(.not.phi1%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		if(.not.phi2%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		N1=getTotalData(phi1)
		N2=getTotalData(phi2)
		if(N1.ne.N2) then
			write(*,*)"ERROR in dot"
			write(*,*)"Total number of input is"
			write(*,*)N1,N2
			call error_stop()
		end if
		call  product_dotc_com8(dotTensor,phi1%TData,phi2%TData)	
		RETURN
	end function
	type(Tensor) function TdotTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		integer::i
		classtype=select_type_in_add_minu(phi1%TData,phi2%TData)
		select case(classtype)
			case(1)
				dotTensor=idotTensor(phi1,phi2)
			case(2)
				dotTensor=sdotTensor(phi1,phi2)
			case(3)
				dotTensor=ddotTensor(phi1,phi2)
			case(4)
				dotTensor=cdotTensor(phi1,phi2)
			case(5)
				dotTensor=zdotTensor(phi1,phi2)
			case default
				call writemess("ERROR in .x.",-1)
				call error_stop()
		end select
		RETURN
	end function


!***************   dot   ******************
!		return <phi1|phi2>, dot product DO NOT conjugating the first vector,The Tensor will be regard as a vector
!	
	integer function idotUTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		if(.not.phi1%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		if(.not.phi2%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		N1=getTotalData(phi1)
		N2=getTotalData(phi2)
		if(N1.ne.N2) then
			write(*,*)"ERROR in dot"
			write(*,*)"Total number of input is"
			write(*,*)N1,N2
			call error_stop()
		end if
		call  product_dot_int(dotTensor,phi1%TData,phi2%TData)	
		RETURN
	end function
	real(kind=4) function sdotUTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		if(.not.phi1%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		if(.not.phi2%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		N1=getTotalData(phi1)
		N2=getTotalData(phi2)
		if(N1.ne.N2) then
			write(*,*)"ERROR in dot"
			write(*,*)"Total number of input is"
			write(*,*)N1,N2
			call error_stop()
		end if
		call  product_dot_real4(dotTensor,phi1%TData,phi2%TData)	
		RETURN
	end function
	real(kind=8) function ddotUTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		if(.not.phi1%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		if(.not.phi2%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		N1=getTotalData(phi1)
		N2=getTotalData(phi2)
		if(N1.ne.N2) then
			write(*,*)"ERROR in dot"
			write(*,*)"Total number of input is"
			write(*,*)N1,N2
			call error_stop()
		end if
		call  product_dot_real8(dotTensor,phi1%TData,phi2%TData)	
		RETURN
	end function
	complex(kind=4) function cdotUTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		if(.not.phi1%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		if(.not.phi2%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		N1=getTotalData(phi1)
		N2=getTotalData(phi2)
		if(N1.ne.N2) then
			write(*,*)"ERROR in dot"
			write(*,*)"Total number of input is"
			write(*,*)N1,N2
			call error_stop()
		end if
		call  product_dot_com4(dotTensor,phi1%TData,phi2%TData)	
		RETURN
	end function
	complex(kind=8) function zdotUTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		if(.not.phi1%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		if(.not.phi2%getflag())then
			write(*,*)"There is no data in the Tensor,(dot)"
			call error_stop()
		end if
		N1=getTotalData(phi1)
		N2=getTotalData(phi2)
		if(N1.ne.N2) then
			write(*,*)"ERROR in dot"
			write(*,*)"Total number of input is"
			write(*,*)N1,N2
			call error_stop()
		end if
		call  product_dot_com8(dotTensor,phi1%TData,phi2%TData)	
		RETURN
	end function
	
	type(Tensor) function TdotUTensor(phi1,phi2)result(dotTensor)
		Type(Tensor),intent(in)::phi1,phi2
		integer::N1,N2,classtype
		integer::i
		classtype=select_type_in_add_minu(phi1%TData,phi2%TData)
		select case(classtype)
			case(1)
				dotTensor=idotUTensor(phi1,phi2)
			case(2)
				dotTensor=sdotUTensor(phi1,phi2)
			case(3)
				dotTensor=ddotUTensor(phi1,phi2)
			case(4)
				dotTensor=cdotUTensor(phi1,phi2)
			case(5)
				dotTensor=zdotUTensor(phi1,phi2)
			case default
				call writemess("ERROR in .dot.",-1)
				call error_stop()
		end select
		RETURN
	end function
	
	
!*****************  norm   *****************
!		return  sqrt(<phi|phi>	)
	integer function inormTensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		real*4::s
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		call norm_routine_real4(s,T%TData)
		normTensor=s
		return
	end function	
	real(kind=4) function snormTensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		call norm_routine_real4(normTensor,T%TData)
		return
	end function	
	real(kind=8) function dnormTensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		call norm_routine_real8(normTensor,T%TData)
		return
	end function	
	complex(kind=4) function cnormTensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		call norm_routine_com4(normTensor,T%TData)
		return
	end function	
	complex(kind=8) function znormTensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		call norm_routine_com8(normTensor,T%TData)
		return
	end function	
	type(Tensor) function TnormTensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		integer::i
		real*4::s
		real*8::d
		complex*8::c
		complex*16::z
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		select case(T%getType())
			case (1) 
				call norm_routine_real4(s,T%TData)
				normTensor=s
			case (2) 
				call norm_routine_real4(s,T%TData)
				normTensor=s
			case (3) 
				call norm_routine_real8(d,T%TData)
				normTensor=d
			case (4) 
				call norm_routine_com4(c,T%TData)
				normTensor=real(c,kind=4)
			case (5) 
				call norm_routine_com8(z,T%TData)
				normTensor=dble(z)
			case default
				call writemess("ERROR in norm",-1)
				call error_stop()
		end select
		return
	end function	

!*****************  norm2   *****************
!		return  <phi|phi>	
	integer function inorm2Tensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		call norm2_routine_int(normTensor,T%TData)
		return
	end function	
	real(kind=4) function snorm2Tensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		normTensor=snormTensor(T)
		normTensor=normTensor*normTensor
		return
	end function	
	real(kind=8) function dnorm2Tensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		normTensor=dnormTensor(T)
		normTensor=normTensor*normTensor
		return
	end function	
	complex(kind=4) function cnorm2Tensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		normTensor=cnormTensor(T)
		normTensor=normTensor*normTensor
		return
	end function	
	complex(kind=8) function znorm2Tensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		normTensor=znormTensor(T)
		normTensor=normTensor*normTensor
		return
	end function
	type(Tensor) function Tnorm2Tensor(T)result(normTensor)
		class(Tensor),intent(in) :: T
		if(.not.T%getflag())then
			write(*,*)"There is no data in the Tensor,(norm)"
			call error_stop()
		end if
		select case(T%getType())
			case (1)
				normTensor=inorm2Tensor(T)
			case default
			normTensor=TnormTensor(T)
			normTensor=normTensor*normTensor
		end select
		return
	end function
!***************  directProduct  *********************
	type(Tensor) function directProduct(T1,T2)
		type(Tensor),intent(in) :: T1,T2
		integer :: m1,n1,m2,n2,classtype
		type(Dimension):: D1,Dtemp
		if((getRank(T1).eq.1).and.(getRank(T2).eq.1)) then
			D1=T1%TenDim
			D1=D1+T2%TenDim
			classtype=select_type_in_add_minu(T1%TData,T2%TData)
			call allocateTensor(directProduct,D1,classtype)
			n1=T1.dim.1
			n2=T2.dim.1
			call directProduct1_routine(directProduct%TDAta,T1%TData,T2%TData,n1,n2)
			return
		end if
		if((getRank(T1).eq.2).and.(getRank(T2).eq.2)) then
			D1=T1%TenDim.sub.1
			Dtemp=T2%TenDim.sub.1
			D1=D1+Dtemp
			Dtemp=T1%TenDim.sub.2
			D1=D1+Dtemp
			Dtemp=T2%TenDim.sub.2
			D1=D1+Dtemp
			m1=T1.dim.1
			n1=T1.dim.2
			m2=T2.dim.1
			n2=T2.dim.2
			classtype=select_type_in_add_minu(T1%TData,T2%TData)
			call allocateTensor(directProduct,D1,classtype)
			call directProduct2_routine(directProduct%TData,T1%TData,T2%TData,m1,n1,m2,n2)
		else
			write(*,*)"The directProduct is just for matrix"
			call error_stop()
		end if
		return
	end function
	type(Tensor) function directProductM(T1,T2)result(directProduct)!return a matrix
		type(Tensor),intent(in) :: T1,T2
		integer :: m1,n1,m2,n2,classtype
		type(Dimension):: D1,Dtemp
		if((getRank(T1).eq.1).and.(getRank(T2).eq.1)) then
			D1=T1%TenDim
			D1=D1+T2%TenDim
			D1=D1%fuseIndex(1,1)
			classtype=select_type_in_add_minu(T1%TData,T2%TData)
			call allocateTensor(directProduct,D1,classtype)
			n1=T1.dim.1
			n2=T2.dim.1
			call directProduct1_routine(directProduct%TDAta,T1%TData,T2%TData,n1,n2)
			return
		end if
		if((getRank(T1).eq.2).and.(getRank(T2).eq.2)) then
			D1=T1%TenDim.sub.1
			Dtemp=T2%TenDim.sub.1
			D1=D1+Dtemp
			Dtemp=T1%TenDim.sub.2
			D1=D1+Dtemp
			Dtemp=T2%TenDim.sub.2
			D1=D1+Dtemp
			D1=D1%fuseIndex(1,1)
			D1=D1%fuseIndex(2,1)
			m1=T1.dim.1
			n1=T1.dim.2
			m2=T2.dim.1
			n2=T2.dim.2
			classtype=select_type_in_add_minu(T1%TData,T2%TData)
			call allocateTensor(directProduct,D1,classtype)
			call directProduct2_routine(directProduct%TData,T1%TData,T2%TData,m1,n1,m2,n2)
		else
			write(*,*)"The directProduct is just for matrix"
			call error_stop()
		end if
		return
	end function
	
	type(Tensor) function directProductTensor(T1,T2)result(Res)
		type(Tensor),intent(in) :: T1,T2
		type(Dimension)::newDim
		integer::classtype
		newDim=T1%TenDim+T2%TenDim
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		call Res%allocate(newDim,classtype)
		call directProductTensor_routine(Res%TData,T1%TData,T2%TData)
		return
	end function


!trace 
	type(Tensor) function TtraceTensor(T)result(trace)
		class(Tensor),intent(in) :: T
		integer::rank,i
		if(T%getTotalData().eq.1)then
			trace=T.zi.1
			return
		end if
		rank=getRank(T)
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
		select case(T%getType())
			case(1)
				trace=itraceTensor(T)
			case(2)
				trace=straceTensor(T)
			case(3)
				trace=dtraceTensor(T)
			case(4)
				trace=ctraceTensor(T)
			case(5)
				trace=ztraceTensor(T)
			case default
				call writemess("ERROR in trace",-1)
				call error_stop()
		end 	select
		return
	end function	
	complex*16 function ztraceTensor(T)result(trace)
		class(Tensor),intent(in) :: T
		integer::rank,i
		if(T%getTotalData().eq.1)then
			trace=T.zi.1
			return
		end if
		rank=getRank(T)
		if(rank.ne.2) then
			write(*,*)"error in trace"
			write(*,*)"input Tensor should be a matrix"
			call error_stop()
		end if
		if((T.dim.1).ne.(T.dim.2)) then
			write(*,*)"error in trace"
			write(*,*)"input Tensor should be a matrix"
			write(*,*)(T.dim.1),(T.dim.2)
			call error_stop()
		end if
		trace=dcmplx(0d0,0d0)
		do i=1,(T.dim.1)
			trace=trace+(T.zi.(/i,i/))
		end do
		return
	end function	
	complex*8 function ctraceTensor(T)result(trace)
		class(Tensor),intent(in) :: T
		integer::rank,i
		if(T%getTotalData().eq.1)then
			trace=T.ci.1
			return
		end if
		rank=getRank(T)
		if(rank.ne.2) then
			write(*,*)"error in trace"
			write(*,*)"input Tensor should be a matrix"
			call error_stop()
		end if
		if((T.dim.1).ne.(T.dim.2)) then
			write(*,*)"error in trace"
			write(*,*)"input Tensor should be a matrix"
			write(*,*)(T.dim.1),(T.dim.2)
			call error_stop()
		end if
		trace=cmplx(0d0,0d0)
		do i=1,(T.dim.1)
			trace=trace+(T.ci.(/i,i/))
		end do
		return
	end function	
	real(kind=8) function dtraceTensor(T)result(trace)
		class(Tensor),intent(in) :: T
		integer::rank,i
		if(T%getTotalData().eq.1)then
			trace=T.di.1
			return
		end if
		rank=getRank(T)
		if(rank.ne.2) then
			write(*,*)"error in trace"
			write(*,*)"input Tensor should be a matrix"
			call error_stop()
		end if
		if((T.dim.1).ne.(T.dim.2)) then
			write(*,*)"error in trace"
			write(*,*)"input Tensor should be a matrix"
			write(*,*)(T.dim.1),(T.dim.2)
			call error_stop()
		end if
		trace=0d0
		do i=1,(T.dim.1)
			trace=trace+(T.di.(/i,i/))
		end do
		return
	end function	
	real(kind=4) function straceTensor(T)result(trace)
		class(Tensor),intent(in) :: T
		integer::rank,i
		if(T%getTotalData().eq.1)then
			trace=T.si.1
			return
		end if
		rank=getRank(T)
		if(rank.ne.2) then
			write(*,*)"error in trace"
			write(*,*)"input Tensor should be a matrix"
			call error_stop()
		end if
		if((T.dim.1).ne.(T.dim.2)) then
			write(*,*)"error in trace"
			write(*,*)"input Tensor should be a matrix"
			write(*,*)(T.dim.1),(T.dim.2)
			call error_stop()
		end if
		trace=0.0
		do i=1,(T.dim.1)
			trace=trace+(T.si.(/i,i/))
		end do
		return
	end function	
	integer function itraceTensor(T)result(trace)
		class(Tensor),intent(in) :: T
		integer::rank,i
		if(T%getTotalData().eq.1)then
			trace=T.ii.1
			return
		end if
		rank=getRank(T)
		if(rank.ne.2) then
			write(*,*)"error in trace"
			write(*,*)"input Tensor should be a matrix"
			call error_stop()
		end if
		if((T.dim.1).ne.(T.dim.2)) then
			write(*,*)"error in trace"
			write(*,*)"input Tensor should be a matrix"
			write(*,*)(T.dim.1),(T.dim.2)
			call error_stop()
		end if
		trace=0
		do i=1,(T.dim.1)
			trace=trace+(T.ii.(/i,i/))
		end do
		return
	end function	

!*****************  Htranspose  *****************
	type(Tensor) function Htranspose(T)
		type(Tensor),intent(in) :: T
		integer::rank,m,n,i
		integer,allocatable::indices(:)
		type(dimension)::dimen
		rank=T%rank
		if(rank.eq.1) then
			m=T.dim. 1
			call allocateTensor(Htranspose,T,T%getType())
			call conjugate_subroutine(Htranspose%TData,T%TData)
		else if(rank.eq.2) then
			m=T.dim.1
			n=T.dim.2
			call allocateTensor(Htranspose, T%TenDim%Dimpermute((/2,1/)) ,T%getType())
			call Htranspose_subroutine(Htranspose%TData,T%TData,m,n)
		else
			allocate(indices(rank))
			do i=1,rank
				indices(rank-i+1)=i
			end do
			Htranspose=(.con.T).p.indices
		end if
		return
	end function
	type(Tensor) function Htranspose2(T)result(Htranspose)
		type(Tensor),intent(in) :: T
		integer :: rank,i,charlen
		integer,allocatable::indices(:)
		character(len=max_len_of_char_in_TData)::Tname
		rank=T%getRank()
		allocate(indices(rank))
		do i=1,rank
			indices(i)=rank-i+1
		end do
		Htranspose=.con.(T.p.indices)
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
!*****************  conjugate  *****************
	type(Tensor) function conjugate(T)
		type(Tensor),intent(in) :: T
		complex*16,allocatable :: newdata(:)
		integer :: m
		call allocateTensor(conjugate,T,T%getType())
		call conjugate_subroutine(conjugate%TData,T%TData)
		return
	end function
!transpose
	type(Tensor) function transposeTensor(T)
		type(Tensor),intent(in) :: T
		integer::rank,m,n
		type(dimension)::dimen
		rank=T%rank
		if(rank.eq.1) then
			transposeTensor=T
		else if(rank.eq.2) then
			m=T.dim.1
			n=T.dim.2
			call allocateTensor(transposeTensor, T%TenDim%Dimpermute((/2,1/)) ,T%getType())
			call transpose_subroutine(transposeTensor%TData,T%TData,m,n)
		else
			write(*,*) "Tensor should be 1 or 2 dimension"
			write(*,*) "ERROR in Htranspose,stop"
			call error_stop()
		end if
		return
	end function

!cccccccccccccccc	pauli_matrix  ccccccccccccccccccccccc
!	the output is sigma_i*num,where i=x,y,z
	subroutine pauli_matrix(Tx,Ty,Tz,num)
		type(Tensor) :: Tx,Ty,Tz
		class(*),optional,intent(in)::num
		complex*16::numr
		complex*16 :: x(2,2)
		complex*16 :: y(2,2)
		complex*16 :: z(2,2)
		complex*16::EE=(1d0,0d0)
		complex*16::II=(0d0,1d0)
		if(present(num)) then
			numr=dselect(num)
		else
			numr=dcmplx(1d0,0d0)
		end if
		x(1,1)=dcmplx(0,0)
		x(1,2)=EE*numr
		x(2,1)=EE*numr
		x(2,2)=dcmplx(0,0)

		y(1,1)=dcmplx(0,0)
		y(1,2)=-1d0*II*numr
		y(2,1)=II*numr
		y(2,2)=dcmplx(0,0)

		z(1,1)=EE*numr
		z(1,2)=dcmplx(0,0)
		z(2,1)=dcmplx(0,0)
		z(2,2)=-1d0*EE*numr
		Tx=x
		Ty=y
		Tz=z
		return
	end subroutine	
! return exp(H)
	 type(Tensor) function expmTensor(H)
		class(Tensor),intent(in) ::H
		type(Tensor)::temp
		real*8::a
		integer::i
		if(getRank(H).ne.2) then
			write(*,*)"ERROR in expm"
			write(*,*)"input Tensor should be a matrix"
			call error_stop()
		end if
		if(H%dim(1).ne.H%dim(2)) then
			write(*,*)"ERROR in expm"
			call error_stop()
		end if
		call expmTensor%setType(H%getType())
		temp=H
		expmTensor=H
		a=1
		do i=2,99999
			temp=temp*H
			if(temp%isZero())exit
			temp=temp/i
			expmTensor=expmTensor+temp
		end do
		expmTensor=expmTensor+eye(H%dim(1),H%dim(2))
		return
		
		
	!	integer :: hdim(2),i,j,N,lwork,info,SDIM
	!	type(dimension)::newHdim
	!	hdim(1)=H.dim.1
	!	hdim(2)=H.dim.2
	!	newHdim=H%TenDim
		
	!	N=hdim(1)
	!	select case(H%getType())
	!		case (1)
	!			call expm%setType('complex*8')
	!		case (2)
	!			call expm%setType('complex*8')	
	!		case (3)
	!			call expm%setType('complex*16')
	!		case (4)
	!			call expm%setType('complex*8')
	!		case (5)
	!			call expm%setType('complex*16')
		!end select
		!expm=H
		!call expm_TData_routine(expm%TData,N)
		return
	end function
	!H = vec*eye(val)*(vec**H)
	subroutine eigvalue(H,val,vec)
		class(Tensor),intent(in) ::H
		type(Tensor),intent(inout) ::val
		type(Tensor),optional,intent(inout)::vec
		integer::hdim(2)
		hdim(1)=H.dim.1
		hdim(2)=H.dim.2
		if(getRank(H).ne.2) then
			write(*,*)"ERROR in eng"
			write(*,*)"input Tensor should be a matrix"
			call error_stop()
		end if
		if(hdim(1).ne.hdim(2)) then
			write(*,*)"ERROR in eng"
			call error_stop()
		end if
		select case(H%getType())
			case (5)
				call allocatedTensor(val,(/hdim(1)/),5)
				if(present(vec))call allocatedTensor(vec,H%TenDim,5)
			case (4)
				call allocatedTensor(val,(/hdim(1)/),4)
				if(present(vec))call allocatedTensor(vec,H%TenDim,4)
			case (3)
				call allocatedTensor(val,(/hdim(1)/),5)
				if(present(vec))call allocatedTensor(vec,H%TenDim,3)
			case (2)
				call allocatedTensor(val,(/hdim(1)/),4)
				if(present(vec))call allocatedTensor(vec,H%TenDim,2)
			case (1)
				call allocatedTensor(val,(/hdim(1)/),4)
				if(present(vec))call allocatedTensor(vec,H%TenDim,2)
		end select
		call eigenvalue_TData_routine(H%TData,hdim(1),val%TData,vec%TData)
		return
	end subroutine	
	type(Tensorlink) function eig_link(T,outvex)	
		class(Tensor),target,intent(in)::T
		logical,optional,intent(in)::outvex
		type(Tensornode),pointer::val,vec
		call eig_link%addnode()
		if(present(outvex).and.outvex)then
			call eig_link%addnode()
			call eig_link%headnode(val)
			vec=>val%next
			call eigvalue(T,val%Tensor,vec%Tensor)
		else
			call eig_link%headnode(val)
			call eigvalue(T,val%Tensor)
		end if
		return
	end function
	
	function eigTensor(T,outvex)	result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),target,intent(in)::T
		logical,optional,intent(in)::outvex
		if(present(outvex).and.outvex)then
			allocate(res(2))
			call eigvalue(T,res(1),res(2))
		else
			allocate(res(1))
			call eigvalue(T,res(1))
		end if
		return
	end function
	
!**************   combination   ********************
!	T1 :a [...,m,n,l] matrix
!	T2 :a [...,m,n,l] matrix
!	combination(T1,T2):a [...,m,n,l,2] matrix
!or 
!	T1 :a [...,m,n,l] matrix
!	T2 :a [...,m,n] matrix
!	combination(T1,T2):a [...,m,n,l+1] matrix
	type(Tensor) function combinationCol(T1,T2)result(combination)
		type(Tensor),intent(in)::T1
		type(Tensor),intent(in)::T2
		integer,allocatable::dim1(:),dim2(:)
		integer::total,total1,i,dim_n,classtype
		type(Dimension)::newDim,dimen1,dimen2
		if(T1%rank.eq.T2%rank) then
			call copydimension(dim1,.subDim.T1)
			call copydimension(dim2,.subDim.T2)
			if(.not.(dim1.equ.dim2)) then
				write(*,*)"can not conbie two Tensor"
				call T1%diminfo('A')
				call T2%diminfo('B')
				write(*,*)"stop"
				call error_stop()
				return
			end if
			newDim=.subDim.T1
			newDim=newDim+(/2/)
			combination%TenDim=newDim
			classtype=select_type_in_add_minu(T1%TData,T2%TData)
			call allocateTensor(combination,newDim,classtype)
			call combinationCol_TData(combination%TData,T1%TData,T2%TData)
			return
		end if
		dimen1=.subDim.T1
		dimen1=dimen1%fuseIndex(1,T1%rank-2)
		dimen2=.subDim.T2
		dimen2=dimen2%fuseIndex(1,T2%rank)
		if(.not.((dimen1.sub.1).equ.dimen2)) then
			write(*,*)"can not conbie two Tensor"
			write(*,*)"stop2"
			call error_stop()
			return
		end if
		dim_n=dimen1.i.2
		newDim=(.subDim.T2)
		newDim=newDim+(/dim_n+1/)
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		call allocateTensor(combination,newDim,classtype)
		call combinationCol_TData(combination%TData,T1%TData,T2%TData)
		return
	end function
	subroutine combinationCol_subroutine(T1,T2)
		class(Tensor),intent(inout)::T1
		type(Tensor),intent(in)::T2
		type(Tensor)::combination
		integer,allocatable::dim1(:),dim2(:)
		integer::total,total1,i,dim_n,classtype
		type(Dimension)::newDim,dimen1,dimen2
		if(T1%rank.eq.T2%rank) then
			call copydimension(dim1,.subDim.T1)
			call copydimension(dim2,.subDim.T2)
			if(.not.(dim1.equ.dim2)) then
				write(*,*)"can not conbie two Tensor"
				call T1%diminfo('A')
				call T2%diminfo('B')
				write(*,*)"stop"
				call error_stop()
				return
			end if
			newDim=.subDim.T1
			newDim=newDim+(/2/)
			combination%TenDim=newDim
			classtype=select_type_in_add_minu(T1%TData,T2%TData)
			call allocateTensor(combination,newDim,classtype)
			call combinationCol_TData(combination%TData,T1%TData,T2%TData)
			T1=combination
			return
		end if
		dimen1=.subDim.T1
		dimen1=dimen1%fuseIndex(1,T1%rank-2)
		dimen2=.subDim.T2
		dimen2=dimen2%fuseIndex(1,T2%rank)
		if(.not.((dimen1.sub.1).equ.dimen2)) then
			write(*,*)"can not conbie two Tensor"
			write(*,*)"stop2"
			call error_stop()
			return
		end if
		dim_n=dimen1.i.2
		newDim=(.subDim.T2)
		newDim=newDim+(/dim_n+1/)
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		call allocateTensor(combination,newDim,classtype)
		call combinationCol_TData(combination%TData,T1%TData,T2%TData)
		T1=combination
		return
	end subroutine
!			T1 :a [l,m,n,...] matrix
!			T2 :a [l,m,n,...] matrix
!			combinationrow(T1,T2):a [2,l,m,n,...] matrix
!			or 
!			T1 :a [l,m,n,...] matrix
!			T2 :a [m,n,...] matrix
!			combinationrow(T1,T2):a [l+1,m,n,...] matrix				
	type(Tensor) function combinationrow(T1,T2)
		type(Tensor),intent(in)::T1
		type(Tensor),intent(in)::T2
		integer,allocatable::dim1(:),dim2(:)
		integer::total1,total2,i,dim_n,classtype
		type(Dimension)::newDim,dimen1,dimen2
		if(T1%rank.eq.T2%rank) then
			call copydimension(dim1,.subDim.T1)
			call copydimension(dim2,.subDim.T2)
			if(.not.(dim1.equ.dim2)) then
				write(*,*)"can not combinationrow two Tensor"
				write(*,*)dim1
				write(*,*)dim2
				write(*,*)"stop"
				call error_stop()
				return
			end if
			newDim=.subDim.T1
			newDim=(/2/)+newDim
			total1=T1%getTotalData()
			total2=T2%getTotalData()
			classtype=select_type_in_add_minu(T1%TData,T2%TData)
			call allocateTensor(combinationrow,newDim,classtype)
			call combinationRow_TData(combinationrow%TData,T1%TData,T2%TData,2,total1,1,1)
			return
		end if
		dimen1=.subDim.T1
		dimen1=dimen1%fuseIndex(2,T1%rank)
		dimen2=.subDim.T2
		dimen2=dimen2%fuseIndex(1,T2%rank)
		if(.not.((dimen1.sub.2).equ.dimen2)) then
			write(*,*)"can not combinationrow two Tensor"
			write(*,*)"stop2"
			call error_stop()
			return
		end if
		dim_n=dimen1.i.1
		total1=T1%getTotalData()
		total2=T2%getTotalData()
		newDim=(.subDim.T2)
		newDim=(/dim_n+1/)+newDim
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		call allocateTensor(combinationrow,newDim,classtype)
		call combinationRow_TData(combinationrow%TData,T1%TData,T2%TData,dim_n+1,total2,dim_n,1)
		return
	end function		
	
	subroutine combinationrow_subroutine(T1,T2)
		class(Tensor),intent(inout)::T1
		type(Tensor),intent(in)::T2
		type(Tensor)::combinationrow
		integer,allocatable::dim1(:),dim2(:)
		integer::total1,total2,i,dim_n,classtype
		type(Dimension)::newDim,dimen1,dimen2
		if(T1%rank.eq.T2%rank) then
			call copydimension(dim1,.subDim.T1)
			call copydimension(dim2,.subDim.T2)
			if(.not.(dim1.equ.dim2)) then
				write(*,*)"can not combinationrow two Tensor"
				write(*,*)dim1
				write(*,*)dim2
				write(*,*)"stop"
				call error_stop()
				return
			end if
			newDim=.subDim.T1
			newDim=(/2/)+newDim
			total1=T1%getTotalData()
			total2=T2%getTotalData()
			classtype=select_type_in_add_minu(T1%TData,T2%TData)
			call allocateTensor(combinationrow,newDim,classtype)
			call combinationRow_TData(combinationrow%TData,T1%TData,T2%TData,2,total1,1,1)
			T1=combinationrow
			return
		end if
		dimen1=.subDim.T1
		dimen1=dimen1%fuseIndex(2,T1%rank)
		dimen2=.subDim.T2
		dimen2=dimen2%fuseIndex(1,T2%rank)
		if(.not.((dimen1.sub.2).equ.dimen2)) then
			write(*,*)"can not combinationrow two Tensor"
			write(*,*)"stop2"
			call error_stop()
			return
		end if
		dim_n=dimen1.i.1
		total1=T1%getTotalData()
		total2=T2%getTotalData()
		newDim=(.subDim.T2)
		newDim=(/dim_n+1/)+newDim
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		call allocateTensor(combinationrow,newDim,classtype)
		call combinationRow_TData(combinationrow%TData,T1%TData,T2%TData,dim_n+1,total2,dim_n,1)
		T1=combinationrow
		return
	end subroutine	
	
!pasteTensor(T1,T2,.true.)
!			T1 :a [l,m,n,...] matrix
!			T2 :a [l,m,n,...] matrix
!			pasteTensor(T1,T2):a [2*l,m,n,...] matrix
!        [1--->l, m, n,...] is T1
!			[l+1--->2*l, m, n,...] is T2
!        /   \
!        | T1 |
!        |----|
!        | T2 |
!         \    /
!pasteTensor(T1,T2,.false.)
!			T1 :a [...,m,n,l] matrix
!			T2 :a [...,m,n,l] matrix
!			pasteTensor(T1,T2):a [...,m,n,2*l] matrix
!        [... , m, n, 1--->l] is T1
!			[... , m, n, l+1--->2*l] is T2
!        /         \
!        | T1 | T2 |
!        \         /
!        
	subroutine pasteTensorSubroutine(T1,T2,row)
		class(Tensor),intent(inout)::T1
		type(Tensor),intent(in)::T2
		logical,intent(in)::row
		integer::i,classtype,rank1,rank2,pasteDim1,pasteDim2,collen,newDim_i
		type(Dimension)::newDim
		type(Tensor)::pasteTensor
		if(.not.T2%getFlag())return
		rank1=T1%Getrank()
		rank2=T2%Getrank()
		if(rank1.ne.rank2) then
			call writemess('can not paste two Tensor,ranks are,'+rank1+','+rank2,-1)
			call error_stop()
		end if
		if(row)then
			if(rank1.eq.1)then
				!call writemess('Do not finsiehd this part,can not paste two Tensor,ranks are,'+rank1+','+rank2)
				!call error_stop()
				pasteDim1=1
				pasteDim2=1
				newDim_i=2
				collen=T1%getTotalData()
				newDim=(/2,collen/)
			else
				pasteDim1=T1%dim(1)
				pasteDim2=T2%dim(1)
				newDim_i=pasteDim1+pasteDim2
				newDim=(/newDim_i/)
				collen=1
				do i=2,rank1
					newDim=newDim+(T1.subDim.i)
					collen=collen*T1%dim(i)
					if(T1%dim(i).ne.T2%dim(i))then
						call writemess('can not paste two Tensor,in the ,'+i+'th dimension',-1)
						call writemess('T1%dim('+i+')='+T1%dim(i),-1)
						call writemess('T2%dim('+i+')='+T2%dim(i),-1)
						call error_stop()
					end if
				end do
			end if
			if(T1%outNameFlag().eq.1) then
				call newDim%setName(1,T1%outName(1))
			else if(T1%outNameFlag().eq.2)then
				call writemess("ERROR in pasteTensor,Tensor.f90",-1)
				call error_stop()
			end if
			classtype=select_type_in_add_minu(T1%TData,T2%TData)
			call allocateTensor(pasteTensor,newDim,classtype)
			call combinationRow_TData(pasteTensor%TData,T1%TData,T2%TData,newDim_i,collen,pasteDim1,pasteDim2)
		else
			pasteDim1=T1%dim(rank1)
			pasteDim2=T2%dim(rank2)
			newDim_i=pasteDim1+pasteDim2
			collen=1
			do i=1,rank1-1
				newDim=newDim+(T1.subDim.i)
				collen=collen*T1%dim(i)
				if(T1%dim(i).ne.T2%dim(i))then
					call writemess('can not paste two Tensor,in the ,'+i+'th dimension',-1)
					call writemess('T1%dim('+i+')='+T1%dim(i),-1)
					call writemess('T2%dim('+i+')='+T2%dim(i),-1)
					call error_stop()
				end if
			end do
			newDim=newDim+(/newDim_i/)
			if(T1%outNameFlag().eq.1) then
				call newDim%setName(rank1,T1%outName(rank1))
			else if(T1%outNameFlag().eq.2)then
				call writemess("ERROR in pasteTensor,Tensor.f90",-1)
				call error_stop()
			end if
			classtype=select_type_in_add_minu(T1%TData,T2%TData)
			call allocateTensor(pasteTensor,newDim,classtype)
			call combinationCol_TData(pasteTensor%TData,T1%TData,T2%TData)
		end if
		T1=pasteTensor
		return
	end subroutine
	
	type(Tensor) function pasteTensorRow(T1,T2)result(pasteTensor)
		type(Tensor),intent(in)::T1
		type(Tensor),intent(in)::T2
		integer::i,classtype,rank1,rank2,pasteDim1,pasteDim2,collen,newDim_i
		type(Dimension)::newDim
		rank1=T1%Getrank()
		rank2=T2%Getrank()
		if(rank1.ne.rank2) then
			call writemess('can not paste two Tensor,ranks are,'+rank1+','+rank2,-1)
			call error_stop()
		end if
		if(rank1.eq.1)then
			call writemess('Do not finsiehd this part,can not paste two Tensor,ranks are,'+rank1+','+rank2,-1)
			call error_stop()
		end if
		pasteDim1=T1%dim(1)
		pasteDim2=T2%dim(1)
		newDim_i=pasteDim1+pasteDim2
		newDim=(/newDim_i/)
		collen=1
		do i=2,rank1
			newDim=newDim+(T1.subDim.i)
			collen=collen*T1%dim(i)
			if(T1%dim(i).ne.T2%dim(i))then
				call writemess('can not paste two Tensor,in the ,'+i+'th dimension',-1)
				call writemess('T1%dim('+i+')='+T1%dim(i))
				call writemess('T2%dim('+i+')='+T2%dim(i))
				call error_stop()
			end if
		end do
		if(T1%outNameFlag().eq.1) then
			call newDim%setName(1,T1%outName(1))
		else if(T1%outNameFlag().eq.2)then
			call writemess("ERROR in pasteTensor,Tensor.f90",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		call allocateTensor(pasteTensor,newDim,classtype)
		call combinationRow_TData(pasteTensor%TData,T1%TData,T2%TData,newDim_i,collen,pasteDim1,pasteDim2)
		return
	end function
	
	type(Tensor) function pasteTensorCol(T1,T2)result(pasteTensor)
		type(Tensor),intent(in)::T1
		type(Tensor),intent(in)::T2
		integer::i,classtype,rank1,rank2,pasteDim1,pasteDim2,collen,newDim_i
		type(Dimension)::newDim
		rank1=T1%Getrank()
		rank2=T2%Getrank()
		if(rank1.ne.rank2) then
			call writemess('can not paste two Tensor,ranks are,'+rank1+','+rank2,-1)
			call error_stop()
		end if
		pasteDim1=T1%dim(rank1)
		pasteDim2=T2%dim(rank2)
		newDim_i=pasteDim1+pasteDim2
		collen=1
		do i=1,rank1-1
			newDim=newDim+(T1.subDim.i)
			collen=collen*T1%dim(i)
			if(T1%dim(i).ne.T2%dim(i))then
				call writemess('can not paste two Tensor,in the ,'+i+'th dimension',-1)
				call writemess('T1%dim('+i+')='+T1%dim(i),-1)
				call writemess('T2%dim('+i+')='+T2%dim(i),-1)
				call error_stop()
			end if
		end do
		newDim=newDim+(/newDim_i/)
		if(T1%outNameFlag().eq.1) then
			call newDim%setName(rank1,T1%outName(rank1))
		else if(T1%outNameFlag().eq.2)then
			call writemess("ERROR in pasteTensor,Tensor.f90",-1)
			call error_stop()
		end if
		classtype=select_type_in_add_minu(T1%TData,T2%TData)
		call allocateTensor(pasteTensor,newDim,classtype)
		call combinationCol_TData(pasteTensor%TData,T1%TData,T2%TData)
		return
	end function
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  lapack function
!
!**************************************************************************************************************
!**************************************************************************************************************	

	subroutine SVDcutoff(T,U,s,V,Ncut_) 
		class(Tensor),target,intent(in)::T
		type(Tensor),target,intent(inout)::U,s,V
		integer,optional,intent(in)::Ncut_
		integer::Ncut,info
		integer ::min_MN,m,n,classtype
		type(Dimension) :: T1Dim,T2Dim
		class(Tensor),pointer::Tp
		type(Tensor),pointer::Up,sp,Vp
		if(T%rank.ne.2) then
			if(.not.T%getflag()) then
				write(*,*)"ERROR in svd"
				write(*,*)"There is no data in the Tensor"
				write(*,*)"stop"
				call error_stop()
			end if
			write(*,*) "Input Tensor should be 2 dimension in svd"
			write(*,*)"stop"
			call error_stop()
		end if
		Tp=>T
		Up=>U
		sp=>s
		Vp=>V
		if(associated(Tp,Up).or.associated(Tp,sp).or.associated(Tp,Vp).or.associated(Up,sp).or.&
				associated(Up,Vp).or.associated(sp,Vp))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%SVDroutine(U,S,V,Ncut)',-1)
			call writemess('T, U, s and V can not be a same variable',-1)
			call error_stop
		end if
		Tp=>null()
		Up=>null()
		sp=>null()
		Vp=>null()
		
		m=T.dim.1
		n=T.dim.2
		min_MN=min(M,N)
		classtype=T%getType()
		if(present(Ncut_))then
			Ncut=Ncut_
			if(Ncut.gt.min_MN) Ncut=min_MN
			T1Dim=(/Ncut/)
			T1Dim=(T%TenDim.sub.1)+T1Dim
			T2Dim=(/Ncut/)
			T2Dim=T2Dim+(T%TenDim.sub.2)
			if(classtype.eq.1)then
				call allocatedTensor(U,T1Dim,2)
				call allocatedTensor(V,T2Dim,2)
			else
				call allocatedTensor(U,T1Dim,classtype)
				call allocatedTensor(V,T2Dim,classtype)
			end if
			select case(classtype)
				case(5)
					call allocatedTensor(s,(/Ncut/),3)
				case(3)
					call allocatedTensor(s,(/Ncut/),3)
				case default
					call allocatedTensor(s,(/Ncut/),2)
			end select
		else
			T1Dim=(/min_MN/)
			T1Dim=(T%TenDim.sub.1)+T1Dim
			T2Dim=(/min_MN/)
			T2Dim=T2Dim+(T%TenDim.sub.2)
			if(classtype.eq.1)then
				call allocatedTensor(U,T1Dim,2)
				call allocatedTensor(V,T2Dim,2)
			else
				call allocatedTensor(U,T1Dim,classtype)
				call allocatedTensor(V,T2Dim,classtype)
			end if
			select case(classtype)
				case(5)
					call allocatedTensor(s,(/min_MN/),3)
				case(3)
					call allocatedTensor(s,(/min_MN/),3)
				case default
					call allocatedTensor(s,(/min_MN/),2)
			end select
		end if
		call SVD_TData_routine(T%TData,U%TData,S%TData,V%TData,m,n,min_MN,Ncut_,info)
		if(info.ne.0) then
			call writemess('Error in svd ,info='+info,-1)
			call writemess('output The data in ./_SVD_ERROR_LOG.err',-1)
			open(unit=9991,file='./_SVD_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
			call T%writeinfo('The Matrix in SVD',9991)
			call U%writeinfo('The Matrix U in SVD',9991)
			call S%writeinfo('The Matrix S in SVD',9991)
			call V%writeinfo('The Matrix V in SVD',9991)
			close(9991)
			call error_stop()
		end if
		if(S%isnan())then!The number in S is less than U,V and T
			call writemess('Error in svd ,NAN ERROR',-1)
			call writemess('output The data in ./_SVD_ERROR_LOG.err',-1)
			open(unit=9991,file='./_SVD_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
			call T%writeinfo('The Matrix in SVD',9991)
			call U%writeinfo('The Matrix U in SVD',9991)
			call S%writeinfo('The Matrix S in SVD',9991)
			call V%writeinfo('The Matrix V in SVD',9991)
			close(9991)
			call error_stop()
		end if
		if(SVD_S_matrix_flag)S=eye(S)
		return
	end subroutine
	subroutine SVDcutoff_kill_inData(T,U,s,V,Ncut_) 
		class(Tensor),target,intent(inout)::T
		type(Tensor),target,intent(inout)::U,s,V
		integer,optional,intent(in)::Ncut_
		integer::Ncut,info
		integer ::min_MN,m,n,classtype
		type(Dimension) :: T1Dim,T2Dim
		class(Tensor),pointer::Tp
		type(Tensor),pointer::Up,sp,Vp
		if(deallocate_memory_flag)then
			call writemess(' The subroutine of SVDcutoff_kill_inData Can not use when deallocate_memory_flag=.true.',-1)
			call writemess(' One should use "call unset_deallocate_memory_flag()" and than use this subroutine',-1)
			call error_stop
		end if
		if(T%rank.ne.2) then
			if(.not.T%getflag()) then
				write(*,*)"ERROR in svd"
				write(*,*)"There is no data in the Tensor"
				write(*,*)"stop"
				call error_stop()
			end if
			write(*,*) "Input Tensor should be 2 dimension in svd"
			write(*,*)"stop"
			call error_stop()
		end if
		Tp=>T
		Up=>U
		sp=>s
		Vp=>V
		if(associated(Tp,Up).or.associated(Tp,sp).or.associated(Tp,Vp).or.associated(Up,sp).or.&
				associated(Up,Vp).or.associated(sp,Vp))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%SVDroutine(U,S,V,Ncut)',-1)
			call writemess('T, U, s and V can not be a same variable',-1)
			call error_stop
		end if
		Tp=>null()
		Up=>null()
		sp=>null()
		Vp=>null()
		
		m=T.dim.1
		n=T.dim.2
		min_MN=min(M,N)

		classtype=T%getType()
		T1Dim=(/min_MN/)
		T1Dim=(T%TenDim.sub.1)+T1Dim
		T2Dim=(/min_MN/)
		T2Dim=T2Dim+(T%TenDim.sub.2)
		if(classtype.eq.1)then
			call allocatedTensor(U,T1Dim,2)
			call allocatedTensor(V,T2Dim,2)
		else
			call allocatedTensor(U,T1Dim,classtype)
			call allocatedTensor(V,T2Dim,classtype)
		end if
		select case(classtype)
			case(5)
				call allocatedTensor(s,(/min_MN/),3)
			case(3)
				call allocatedTensor(s,(/min_MN/),3)
			case default
				call allocatedTensor(s,(/min_MN/),2)
		end select

		!if deallocate_memory_flag=.false. call U%empty() will not deallocate the momery of U
		! when trucate the data, just reset the info in dimension
		if(present(Ncut_))then
			Ncut=Ncut_
			if(Ncut.gt.min_MN) Ncut=min_MN
			T1Dim=(/Ncut/)
			T1Dim=(T%TenDim.sub.1)+T1Dim
			T2Dim=(/Ncut/)
			T2Dim=T2Dim+(T%TenDim.sub.2)
			call U%reset_dim_no_check(T1Dim)
			call V%reset_dim_no_check(T2Dim)
			call S%reset_dim_no_check([Ncut])
			!call U%empty()
			!call S%empty()
			!call V%empty()
			!if(classtype.eq.1)then
			!	call U%allocate(T1Dim,2)
			!	call V%allocate(T2Dim,2)
			!else
			!	call U%allocate(T1Dim,classtype)
			!	call V%allocate(T2Dim,classtype)
			!end if
			!select case(classtype)
			!	case(5)
			!		call S%allocate([Ncut],3)
			!	case(3)
			!		call S%allocate([Ncut],3)
			!	case default
			!		call S%allocate([Ncut],2)
			!end select
		end if
		call SVD_TData_routine_Kill_inData(T%TData,U%TData,S%TData,V%TData,m,n,min_MN,Ncut_,info)
		if(info.ne.0) then
			call writemess('Error in svd ,info='+info,-1)
			call writemess('output The data in ./_SVD_ERROR_LOG.err',-1)
			open(unit=9991,file='./_SVD_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
			call T%writeinfo('The Matrix in SVD',9991)
			call U%writeinfo('The Matrix U in SVD',9991)
			call S%writeinfo('The Matrix S in SVD',9991)
			call V%writeinfo('The Matrix V in SVD',9991)
			close(9991)
			call error_stop()
		end if
		if(S%isnan())then!The number in S is less than U,V and T
			call writemess('Error in svd ,NAN ERROR',-1)
			call writemess('output The data in ./_SVD_ERROR_LOG.err',-1)
			open(unit=9991,file='./_SVD_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
			call T%writeinfo('The Matrix in SVD',9991)
			call U%writeinfo('The Matrix U in SVD',9991)
			call S%writeinfo('The Matrix S in SVD',9991)
			call V%writeinfo('The Matrix V in SVD',9991)
			close(9991)
			call error_stop()
		end if
		if(SVD_S_matrix_flag)S=eye(S)
		return
	end subroutine
	subroutine SVDcutoff_name(inputT,U,s,V,nameU,nameV,Ncut_) 
		class(Tensor),target,intent(in)::inputT
		type(Tensor),target,intent(inout)::U,s,V
		type(Tensor)::T
		character(len=*),intent(in)::nameU,nameV
		integer,optional,intent(in)::Ncut_
		integer::Ncut,rankU,rankV,rank,info
		integer ::min_MN,m,n,classtype,i
		type(Dimension) :: T1Dim,T2Dim,Tdimension
		class(Tensor),pointer::Tp
		type(Tensor),pointer::Up,sp,Vp
		if(.not.inputT%getflag()) then
			write(*,*)"ERROR in svd"
			write(*,*)"There is no data in the Tensor"
			write(*,*)"stop"
			call error_stop()
		end if
		Tp=>inputT
		Up=>U
		sp=>s
		Vp=>V
		if(associated(Tp,Up).or.associated(Tp,sp).or.associated(Tp,Vp).or.associated(Up,sp).or.&
				associated(Up,Vp).or.associated(sp,Vp))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%SVDroutine(U,S,V,nameU,nameV,Ncut)')
			call writemess('T, U, s and V can not be a same variable')
			call error_stop
		end if
		Tp=>null()
		Up=>null()
		sp=>null()
		Vp=>null()
		
		T=inputT
		rank=T%getRank()
		rankU=0
		rankV=0
		call T%forward(nameU)
		do i=1,rank
			if(T%outTensorName(i).equ.nameU) rankU=rankU+1
			if(T%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in SVDcutoff_name",-1)
			call writemess(nameU+','+nameV,-1)
			call inputT%diminfo()
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
		call T%fuse(1,rankU)
		call T%fuse(2,rankV+1)
		m=T.dim.1
		n=T.dim.2
		min_MN=min(M,N)
		classtype=T%getType()
		if(present(Ncut_))then
			Ncut=Ncut_
			if(Ncut.gt.min_MN) Ncut=min_MN
			T1Dim=(/Ncut/)
			T1Dim=(T%TenDim.sub.1)+T1Dim
			T2Dim=(/Ncut/)
			T2Dim=T2Dim+(T%TenDim.sub.2)
			if(classtype.eq.1)then
				call allocatedTensor(U,T1Dim,2)
				call allocatedTensor(V,T2Dim,2)
			else
				call allocatedTensor(U,T1Dim,classtype)
				call allocatedTensor(V,T2Dim,classtype)
			end if
			select case(classtype)
				case(5)
					call allocatedTensor(s,(/Ncut/),3)
				case(3)
					call allocatedTensor(s,(/Ncut/),3)
				case default
					call allocatedTensor(s,(/Ncut/),2)
			end select
		else
			T1Dim=(/min_MN/)
			T1Dim=(T%TenDim.sub.1)+T1Dim
			T2Dim=(/min_MN/)
			T2Dim=T2Dim+(T%TenDim.sub.2)
			if(classtype.eq.1)then
				call allocatedTensor(U,T1Dim,2)
				call allocatedTensor(V,T2Dim,2)
			else
				call allocatedTensor(U,T1Dim,classtype)
				call allocatedTensor(V,T2Dim,classtype)
			end if
			select case(classtype)
				case(5)
					call allocatedTensor(s,(/min_MN/),3)
				case(3)
					call allocatedTensor(s,(/min_MN/),3)
				case default
					call allocatedTensor(s,(/min_MN/),2)
			end select
		end if
		call SVD_TData_routine(T%TData,U%TData,S%TData,V%TData,m,n,min_MN,Ncut_,info)
		if(info.ne.0) then
			call writemess('Error in svd ,info='+info,-1)
			call writemess('output The data in ./_SVD_ERROR_LOG.err',-1)
			open(unit=9991,file='./_SVD_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
			call T%writeinfo('The Matrix in SVD',9991)
			call U%writeinfo('The Matrix U in SVD',9991)
			call S%writeinfo('The Matrix S in SVD',9991)
			call V%writeinfo('The Matrix V in SVD',9991)
			close(9991)
			call error_stop()
		end if
		if(S%isnan())then!The number in S is less than U,V and T
			call writemess('Error in svd ,NAN ERROR',-1)
			call writemess('output The data in ./_SVD_ERROR_LOG.err',-1)
			open(unit=9991,file='./_SVD_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
			call T%writeinfo('The Matrix in SVD',9991)
			call U%writeinfo('The Matrix U in SVD',9991)
			call S%writeinfo('The Matrix S in SVD',9991)
			call V%writeinfo('The Matrix V in SVD',9991)
			close(9991)
			call error_stop()
		end if
		call U%split()
		call V%split()
		call U%setName(U%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			S=eye(S)
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
		end if
		call V%setName(1,'SVD.V')
		return
	end subroutine
	subroutine SVDcutoff_name_kill_inData(T,U,s,V,nameU,nameV,Ncut_) 
		class(Tensor),target,intent(inout)::T
		type(Tensor),target,intent(inout)::U,s,V
		character(len=*),intent(in)::nameU,nameV
		integer,optional,intent(in)::Ncut_
		integer::Ncut,rankU,rankV,rank,info
		integer ::min_MN,m,n,classtype,i
		type(Dimension) :: T1Dim,T2Dim,Tdimension
		class(Tensor),pointer::Tp
		type(Tensor),pointer::Up,sp,Vp
		if(.not.T%getflag()) then
			write(*,*)"ERROR in svd"
			write(*,*)"There is no data in the Tensor"
			write(*,*)"stop"
			call error_stop()
		end if
		Tp=>T
		Up=>U
		sp=>s
		Vp=>V
		if(associated(Tp,Up).or.associated(Tp,sp).or.associated(Tp,Vp).or.associated(Up,sp).or.&
				associated(Up,Vp).or.associated(sp,Vp))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%SVDroutine(U,S,V,nameU,nameV,Ncut)')
			call writemess('T, U, s and V can not be a same variable')
			call error_stop
		end if
		Tp=>null()
		Up=>null()
		sp=>null()
		Vp=>null()
		
		rank=T%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(T%outTensorName(i).equ.nameU) rankU=rankU+1
			if(T%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in SVDcutoff_name",-1)
			call writemess(nameU+','+nameV,-1)
			call T%diminfo()
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
		call T%fuse(1,rankU)
		call T%fuse(2,rankV+1)
		call SVDcutoff_kill_inData(T,U,s,V,Ncut_)
		call U%split()
		call V%split()
		call U%setName(U%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			S=eye(S)
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
		end if
		call V%setName(1,'SVD.V')
		return
	end subroutine
	type(Tensorlink) function SVD_link(T,Ncut_)	
		class(Tensor),intent(in)::T
		integer,optional,intent(in)::Ncut_
		type(Tensornode),pointer::U,S,V
		call SVD_link%addnode()
		call SVD_link%addnode()
		call SVD_link%addnode()
		call SVD_link%headnode(U)
		S=>U%next
		V=>S%next
		call SVDcutoff(T,U%Tensor,s%Tensor,V%Tensor,Ncut_) 
		return
	end function
	
	type(Tensorlink) function SVD_name_link(T,nameU,nameV,Ncut_) result(SVD_link)
		class(Tensor),intent(in)::T
		integer,optional,intent(in)::Ncut_
		character(len=*),intent(in)::nameU,nameV
		type(Tensornode),pointer::U,S,V
		call SVD_link%addnode()
		call SVD_link%addnode()
		call SVD_link%addnode()
		call SVD_link%headnode(U)
		S=>U%next
		V=>S%next
		call SVDcutoff_name(T,U%Tensor,s%Tensor,V%Tensor,nameU,nameV,Ncut_) 
		return
	end function
	
	function SVDTensor_noname(T,Ncut_)	result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		integer,optional,intent(in)::Ncut_
		allocate(res(3))
		call SVDcutoff(T,res(1),res(2),res(3),Ncut_) 
		return
	end function
	
	function SVDTensor_name(T,nameU,nameV,Ncut_)	result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		integer,optional,intent(in)::Ncut_
		character(len=*),intent(in)::nameU,nameV
		allocate(res(3))
		call SVDcutoff_name(T,res(1),res(2),res(3),nameU,nameV,Ncut_) 
		return
	end function
	
	function SVDTensor_Leg1(T,LegName,row,Ncut_)result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		integer,optional,intent(in)::Ncut_
		character*1,intent(in)::row
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		integer::lenName,rank
		allocate(res(3))
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row.equ.'r')then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call SVDcutoff(Temp,res(1),res(2),res(3),Ncut_)
		else if(row.equ.'c')then
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call SVDcutoff(Temp,res(1),res(2),res(3),Ncut_)
		else
			call writemess('DRROR in SVD, row='+row,-1)
			call writemess('row is r or c',-1)
			call error_stop
		end if
		call res(1)%split()
		call res(3)%split()

		call res(1)%setName(res(1)%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call res(2)%setName(1,'SVD.s1')
			call res(2)%setName(2,'SVD.s2')
		end if
		call res(3)%setName(1,'SVD.V')
		return
	end function
	subroutine SVDTensor_Leg1_routine(T,U,s,V,LegName,row,Ncut_)
		class(Tensor),intent(in)::T
		type(Tensor),intent(inout)::U,s,V
		integer,optional,intent(in)::Ncut_
		character*1,intent(in)::row
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		integer::lenName,rank
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row.equ.'r')then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call SVDcutoff(Temp,U,s,V,Ncut_)
		else if(row.equ.'c')then
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call SVDcutoff(Temp,U,s,V,Ncut_)
		else
			call writemess('DRROR in SVD, row='+row,-1)
			call writemess('row is r or c',-1)
			call error_stop
		end if
		call U%split()
		call V%split()

		call U%setName(U%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
		end if
		call V%setName(1,'SVD.V')
		return
	end subroutine

	subroutine SVDTensor_Leg1_routine_kill_indata(T,U,s,V,LegName,row,Ncut_)
		class(Tensor),intent(inout)::T
		type(Tensor),intent(inout)::U,s,V
		integer,optional,intent(in)::Ncut_
		character*1,intent(in)::row
		character(len=*),intent(in)::LegName(:)
		integer::lenName,rank
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row.equ.'r')then
			call T%forward(LegName)
			call T%fuse(1,lenName)
			call T%fuse(2,rank)
			call SVDcutoff_kill_inData(T,U,s,V,Ncut_)
		else if(row.equ.'c')then
			call T%Backward(LegName)
			call T%fuse(1,rank-lenName)
			call T%fuse(2,T%getRank())
			call SVDcutoff_kill_inData(T,U,s,V,Ncut_)
		else
			call writemess('DRROR in SVD, row='+row,-1)
			call writemess('row is r or c',-1)
			call error_stop
		end if
		call U%split()
		call V%split()
		call U%setName(U%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
		end if
		call V%setName(1,'SVD.V')
		return
	end subroutine

	subroutine SVDNameRoutineLeft(T,U,s,V,LegName,Ncut_,row)
		class(Tensor),intent(in)::T
		type(Tensor),intent(inout)::U,s,V
		integer,intent(in)::Ncut_
		logical,intent(in)::row
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		integer::lenName,rank
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row)then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call SVDcutoff(Temp,U,s,V,Ncut_)
		else 
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call SVDcutoff(Temp,U,s,V,Ncut_)
		end if
		call U%split()
		call V%split()

		call U%setName(U%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
		end if
		call V%setName(1,'SVD.V')
		return
	end subroutine

	subroutine SVDNameRoutineLeft_kill_inData(T,U,s,V,LegName,Ncut_,row)
		class(Tensor),intent(inout)::T
		type(Tensor),intent(inout)::U,s,V
		integer,intent(in)::Ncut_
		logical,intent(in)::row
		character(len=*),intent(in)::LegName(:)
		integer::lenName,rank
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row)then
			call T%forward(LegName)
			call T%fuse(1,lenName)
			call T%fuse(2,rank)
			call SVDcutoff_kill_inData(T,U,s,V,Ncut_)
		else 
			call T%Backward(LegName)
			call T%fuse(1,rank-lenName)
			call T%fuse(2,T%getRank())
			call SVDcutoff_kill_inData(T,U,s,V,Ncut_)
		end if
		call U%split()
		call V%split()

		call U%setName(U%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
		end if
		call V%setName(1,'SVD.V')
		return
	end subroutine
	
	function SVDTensor_Leg1_(T,LegName,Ncut_)result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		integer,optional,intent(in)::Ncut_
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		integer::lenName,rank
		allocate(res(3))
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		Temp=T.pf.LegName
		call Temp%fuse(1,lenName)
		call Temp%fuse(2,rank)
		call SVDcutoff(Temp,res(1),res(2),res(3),Ncut_)
		call res(1)%split()
		call res(3)%split()

		call res(1)%setName(res(1)%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call res(2)%setName(1,'SVD.s1')
			call res(2)%setName(2,'SVD.s2')
		end if
		call res(3)%setName(1,'SVD.V')
		return
	end function
	subroutine SVDTensor_Leg1_Routine_(T,U,s,V,LegName,Ncut_)
		class(Tensor),intent(in)::T
		type(Tensor),intent(inout)::U,s,V
		integer,optional,intent(in)::Ncut_
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		integer::lenName,rank
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		Temp=T.pf.LegName
		call Temp%fuse(1,lenName)
		call Temp%fuse(2,rank)
		call SVDcutoff(Temp,U,s,V,Ncut_)
		call U%split()
		call V%split()
		call U%setName(U%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
		end if
		call V%setName(1,'SVD.V')
		return
	end subroutine

	subroutine SVDTensor_Leg1_Routine_kill_inData_(T,U,s,V,LegName,Ncut_)
		class(Tensor),intent(inout)::T
		type(Tensor),intent(inout)::U,s,V
		integer,optional,intent(in)::Ncut_
		character(len=*),intent(in)::LegName(:)
		integer::lenName,rank
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		call T%forward(LegName)
		call T%fuse(1,lenName)
		call T%fuse(2,rank)
		call SVDcutoff_kill_inData(T,U,s,V,Ncut_)
		call U%split()
		call V%split()
		call U%setName(U%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
		end if
		call V%setName(1,'SVD.V')
		return
	end subroutine

	function SVDTensor_Leg2(T,LegNameRow,LegNameCol,Ncut_)result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		integer,optional,intent(in)::Ncut_
		character(len=*),intent(in)::LegNameRow(:),LegNameCol(:)
		type(Tensor)::Temp
		integer::lenName1,lenName2,rank
		allocate(res(3))
		lenName1=size(LegNameRow)
		lenName2=size(LegNameCol)
		rank=T%getRank()
		if((lenName1+lenName2).ne.rank)then
			call writemess('size(LegNameRow)='+lenName1+'size(LegNameCol)='+lenName2,-1)
			call writemess('rank='+rank,-1)
			call writemess('rank should be equal to size(LegNameRow)+size(LegNameCol)')
			call error_stop
		end if
		Temp=T.pf.LegNameRow
		!call Temp%backward(LegNameCol)
		call Temp%fuse(1,lenName1)
		call Temp%fuse(2,lenName2+1)
			call SVDcutoff(Temp,res(1),res(2),res(3),Ncut_)
		call res(1)%split()
		call res(3)%split()

		call res(1)%setName(res(1)%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call res(2)%setName(1,'SVD.s1')
			call res(2)%setName(2,'SVD.s2')
		end if
		call res(3)%setName(1,'SVD.V')
		return
	end function
	subroutine SVDTensor_Leg2_Routine(T,U,s,V,LegNameRow,LegNameCol,Ncut_)
		type(Tensor),intent(inout)::U,s,V
		class(Tensor),intent(in)::T
		integer,optional,intent(in)::Ncut_
		character(len=*),intent(in)::LegNameRow(:),LegNameCol(:)
		type(Tensor)::Temp
		integer::lenName1,lenName2,rank
		lenName1=size(LegNameRow)
		lenName2=size(LegNameCol)
		rank=T%getRank()
		if((lenName1+lenName2).ne.rank)then
			call writemess('size(LegNameRow)='+lenName1+'size(LegNameCol)='+lenName2,-1)
			call writemess('rank='+rank,-1)
			call writemess('rank should be equal to size(LegNameRow)+size(LegNameCol)')
			call error_stop
		end if
		Temp=T.pf.LegNameRow
		!call Temp%backward(LegNameCol)
		call Temp%fuse(1,lenName1)
		call Temp%fuse(2,lenName2+1)
			call SVDcutoff(Temp,U,s,V,Ncut_)
		call U%split()
		call V%split()

		call U%setName(U%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
		end if
		call V%setName(1,'SVD.V')
		return
	end subroutine
	subroutine SVDTensor_Leg2_Routine_kill_inData(T,U,s,V,LegNameRow,LegNameCol,Ncut_)
		type(Tensor),intent(inout)::U,s,V
		class(Tensor),intent(inout)::T
		integer,optional,intent(in)::Ncut_
		character(len=*),intent(in)::LegNameRow(:),LegNameCol(:)
		integer::lenName1,lenName2,rank
		lenName1=size(LegNameRow)
		lenName2=size(LegNameCol)
		rank=T%getRank()
		if((lenName1+lenName2).ne.rank)then
			call writemess('size(LegNameRow)='+lenName1+'size(LegNameCol)='+lenName2,-1)
			call writemess('rank='+rank,-1)
			call writemess('rank should be equal to size(LegNameRow)+size(LegNameCol)')
			call error_stop
		end if
		if(lenName1.le.lenName2)then
			call T%forward(LegNameRow)
		else
			call T%backWard(LegNameCol)
		end if
		call T%fuse(1,lenName1)
		call T%fuse(2,lenName2+1)
		call SVDcutoff_kill_inData(T,U,s,V,Ncut_)
		call U%split()
		call V%split()

		call U%setName(U%getRank(),'SVD.U')
		if(SVD_S_matrix_flag)then
			call S%setName(1,'SVD.s1')
			call S%setName(2,'SVD.s2')
		end if
		call V%setName(1,'SVD.V')
		return
	end subroutine

	!A*X=B
	!input A  and B
	!output X
	!if A^{-1} may not exit,input RCOND,
	!perform SVD on A, Only keep  singular values S(i) <= RCOND*S(1) 
	!if RCONDM<0 keep the S(i)>0

	type(Tensor) function  linequ(A,B,RCOND)
		class(Tensor),intent(in)::A
		type(Tensor),intent(in)::B
		class(*),intent(in),optional::RCOND
		integer::Na,Nb,Na2,classtype
		type(dimension)::Xdim
		Na=A.dim.1
		if(A%rank.ne.2)then
			write(*,*)"error in linequ,A should be a matrix"
			call error_stop()
		end if
		Na2=A.dim.2
		if(getRank(B).eq.1)then
			Nb=1
		else
			Nb=B.dim.2
		end if
		if(Na.ne.(B.dim.1)) then
			write(*,*)"error in linequ,dimension of A and B"
			call error_stop()
		end if
		classtype=select_type_in_add_minu(A%TData,B%TData)
		if(classtype.eq.1)classtype=2
		Xdim=(A%TenDim.sub.1)
		if(getRank(B).ne.1)then
			Xdim=Xdim+(B%TenDim.sub.2)
		end if
		call allocateTensor(linequ,Xdim,classtype)
		if(present(RCOND))then
			call linequ2_routine_TData(linequ%TData,A%TData,B%TData,Na,Na2,Nb,RCOND)
		else
			if(Na.ne.Na2) then
				write(*,*)"error in linequ,dimension of A"
				call error_stop()
			end if
			call linequ_routine_TData(linequ%TData,A%TData,B%TData,Na,Nb)
		end if
		return
	end function		

	!! The inverse of a matrix: the input tensor should be a square matrix 
	!A*X=E ==>X=A^-1
	type(Tensor) function inverse(T)
		type(Tensor),intent(in) :: T
		type(Tensor):: E
		integer :: M,N
		type(dimension)::Xdim
		if(getRank(T).ne.2) then
			write(*,*)"ERROR in calculating the inverse of a Tensor"
			write(*,*)"input Tensor should be a square matrix"
			call error_stop()
		endif
		M = T.dim.1
		N = T.dim.2
		if(M.ne.N) then
			write(*,*)"ERROR in calculating the inverse of a Tensor"
			write(*,*)"input Tensor should be a square matrix"
			call error_stop()
		endif
		call E%setType(T%getclassType())
		E=eye(M,N)
		inverse=linequ(T,E)
		call inverse%resetdim(T%TenDim)
		return
	end function	
	type(Tensor) function inverseTen(T,RCOND)result(inverse)
		type(Tensor),intent(in) :: T
		class(*),intent(in)::RCOND
		type(Tensor):: E
		integer :: M,N
		type(dimension)::Xdim
		if(getRank(T).ne.2) then
			write(*,*)"ERROR in calculating the inverse of a Tensor"
			write(*,*)"input Tensor should be a square matrix"
			call error_stop()
		endif
		M = T.dim.1
		N = T.dim.2
		if(M.ne.N) then
			write(*,*)"ERROR in calculating the inverse of a Tensor"
			write(*,*)"input Tensor should be a square matrix"
			call error_stop()
		endif
		call E%setType(T%getclassType())
		E=eye(M,N)
		inverse=linequ(T,E,RCOND)
		call inverse%resetdim(T%TenDim)
		return
	end function	
	
	type(Tensor) function inverseTensor(T,RCOND)result(inverse)
		class(Tensor),intent(in) :: T
		class(*),intent(in),optional::RCOND
		type(Tensor):: E
		integer :: M,N
		type(dimension)::Xdim
		if(getRank(T).ne.2) then
			write(*,*)"ERROR in calculating the inverse of a Tensor"
			write(*,*)"input Tensor should be a square matrix"
			call error_stop()
		endif
		M = T.dim.1
		N = T.dim.2
		if(M.ne.N) then
			write(*,*)"ERROR in calculating the inverse of a Tensor"
			write(*,*)"input Tensor should be a square matrix"
			call error_stop()
		endif
		call E%setType(T%getclassType())
		E=eye(M,N)
		inverse=linequ(T,E,RCOND)
		call inverse%resetdim(T%TenDim)
		return
	end function	
	
	subroutine LQdecomposition1(T,L,Q)
		class(Tensor),target,intent(in)::T
		type(Tensor),target,intent(inout)::L,Q
		Type(Tensor)::v,vv,identity,Tau
		type(Dimension)::dimen
		integer :: i,j,M,N,min_MN,classtype,INFO
		class(Tensor),pointer::Tp
		type(Tensor),pointer::Lp,Qp
		if(getRank(T).ne.2) then
			write(*,*)"ERROR in LQ decomposition"
			write(*,*)"input Tensor should be a matrix"
			call error_stop()
		endif
		Tp=>T
		Lp=>L
		Qp=>Q
		if(associated(Tp,Lp).or.associated(Tp,Qp).or.associated(Lp,Qp))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%LQTensor(L,Q)')
			call writemess('T, L and V can not be a same variable')
			call error_stop
		end if
		Tp=>null()
		Lp=>null()
		Qp=>null()
		
		M = T.dim.1
		N = T.dim.2
		min_MN=min(M,N)
		
		call L%empty()
		call Q%empty()
		classtype=max(2,T%getType())
		call L%setType(classtype)
		L=T
		call Tau%allocate((/min_MN/),classtype)
		INFO=999
		call TData_LQ(Tau%TData,L%TData,M,N,INFO)
		
		if(info.ne.0) then
			call writemess('Error in LQ decomposition ,info='+info,-1)
			call writemess('output The data in ./_LQ_ERROR_LOG.err',-1)
			open(unit=9991,file='./_LQ_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
			call T%writeinfo('The Matrix in LQ',9991)
			close(9991)
			call error_stop()
		end if
		call v%setType(classtype)
		call vv%setType(classtype)
		call identity%setType(classtype)
		if(M.gt.N)then
			!compute Q
			v=zeroTen((/N/))
			identity=eye(N,N)
			do i=1,min_MN
				if(i.ne.1)then
					call modify(v,i-1,0)
				end if
				call modify(v,i,dcmplx(1d0,0d0))
				do j=i+1,N
					call v%setValue((/j/),L.i.(/i,j/))
				end do
				vv=tau%i(i)*((.h.v).xx.v)
				if(i.eq.1)then
					Q=identity-vv
				else
					Q=Q*(identity-vv)
				end if
			end do
			Q=.h.Q
			Q=Q%subTensor((/-1,1,min_MN/))
			dimen=(/min_MN/)+(T%TenDim.sub.2)
			Q%TenDim=dimen
			!compute L
			do i=1,M
				do j=i+1,min_MN
					call L%setvalue((/i,j/),0)
				end do
			end do
			L=L%subTensor((/-2,1,min_MN/))
			dimen=(T%TenDim.sub.1)+(/min_MN/)
			L%TenDim=dimen
		else
			Q=L
			!compute L
			do i=1,M
				do j=i+1,min_MN
					call L%setvalue((/i,j/),0)
				end do
			end do
			L=L%subTensor((/-2,1,min_MN/))
			dimen=(T%TenDim.sub.1)+(/min_MN/)
			L%TenDim=dimen
			!compute Q
			INFO=999
			call TData_ORGLQ(Tau%TData,Q%TData,M,N,min_MN,INFO)
			dimen=(/min_MN/)+(T.subdim.2)
			Q%TenDim=dimen
			if(info.ne.0) then
				call writemess('Error in LQ decomposition ,info='+info,-1)
				call writemess('output The data in ./_LQ_ERROR_LOG.err',-1)
				open(unit=9991,file='./_LQ_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
				call T%writeinfo('The Matrix in LQ',9991)
				call Q%writeinfo('The Q Matrix in LQ',9991)
				call L%writeinfo('The L Matrix in LQ',9991)
				close(9991)
				call error_stop()
			end if
		end if
		return
	end subroutine

	subroutine LQdecomposition_kill_inData(Q,L)
		class(Tensor),target,intent(inout)::Q
		type(Tensor),target,intent(inout)::L
		Type(Tensor)::v,vv,identity,Tau
		type(Dimension)::dimen
		integer :: i,j,M,N,min_MN,classtype,INFO
		class(Tensor),pointer::Qp
		class(Tensor),pointer::Lp
		real*8,pointer::dp(:,:),Qdp(:,:)
		real*4,pointer::sp(:,:),Qsp(:,:)
		complex(kind=4),pointer::cp(:,:),Qcp(:,:)
		complex(kind=8),pointer::zp(:,:),Qzp(:,:)
		type(Dimension)::TenDim
		if(getRank(Q).ne.2) then
			write(*,*)"ERROR in LQ decomposition"
			write(*,*)"input Tensor should be a matrix"
			call error_stop()
		endif
		Lp=>L
		Qp=>Q
		if(associated(Lp,Qp))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%LQTensor(L,Q)')
			call writemess('T, L and V can not be a same variable')
			call error_stop
		end if
		Lp=>null()
		Qp=>null()
		TenDim=Q%TenDim
		M = Q.dim.1
		N = Q.dim.2
		min_MN=min(M,N)
		
		classtype=max(2,Q%getType())
		call Tau%allocate((/min_MN/),classtype)
		INFO=999
		call TData_LQ(Tau%TData,Q%TData,M,N,INFO)
		
		if(info.ne.0) then
			call writemess('Error in LQ decomposition ,info='+info,-1)
			call error_stop()
		end if
		
		call L%empty()
		call L%allocate((TenDim.sub.1)+(/min_MN/),classtype)
		


		!compute L
		select case(classtype)
			case(2)
				call L%pointer(sp)
				call Q%pointer(Qsp,[1,M],[1,min_MN])
				if(M.le.N)then
					do i=1,min_MN
						sp(i:M,i)=Qsp(i:M,i)
						if(i.gt.1)sp(1:i-1,i)=0
					end do
					INFO=999
					call TData_ORGLQ(Tau%TData,Q%TData,M,N,min_MN,INFO)
					call Q%reset_dim_no_check((/min_MN/)+(Tendim.subdim.2))
				else
					call scopy(Q%getTotalData(),Qsp,1,sp,1)
					do i=2,min_MN
						sp(1:i-1,i)=0
					end do
					INFO=999
					call TData_ORGLQ(Tau%TData,Q%TData,M,N,min_MN,INFO)
					call WorkingMemory%check()
					call WorkingMemory%get_memory(sp,min_MN,N)
					sp=Qsp(1:min_MN,1:N)
					call Q%reset_dim_no_check((/min_MN/)+(Tendim.subdim.2))
					call Q%pointer(Qsp)
					Qsp=sp
					call WorkingMemory%free()
				end if
			case(3)
				call L%pointer(dp)
				call Q%pointer(Qdp,[1,M],[1,min_MN])
				if(M.le.N)then
					do i=1,min_MN
						dp(i:M,i)=Qdp(i:M,i)
						if(i.gt.1)dp(1:i-1,i)=0
					end do
					INFO=999
					call TData_ORGLQ(Tau%TData,Q%TData,M,N,min_MN,INFO)
					call Q%reset_dim_no_check((/min_MN/)+(Tendim.subdim.2))
				else
					call dcopy(Q%getTotalData(),Qdp,1,dp,1)
					do i=2,min_MN
						dp(1:i-1,i)=0
					end do
					INFO=999
					call TData_ORGLQ(Tau%TData,Q%TData,M,N,min_MN,INFO)
					call WorkingMemory%check()
					call WorkingMemory%get_memory(dp,min_MN,N)
					dp=Qdp(1:min_MN,1:N)
					call Q%reset_dim_no_check((/min_MN/)+(Tendim.subdim.2))
					call Q%pointer(Qdp)
					Qdp=dp
					call WorkingMemory%free()
				end if
			case(4)
				call L%pointer(cp)
				call Q%pointer(Qcp,[1,M],[1,min_MN])
				if(M.le.N)then
					do i=1,min_MN
						cp(i:M,i)=Qcp(i:M,i)
						if(i.gt.1)cp(1:i-1,i)=0
					end do
					INFO=999
					call TData_ORGLQ(Tau%TData,Q%TData,M,N,min_MN,INFO)
					call Q%reset_dim_no_check((/min_MN/)+(Tendim.subdim.2))
				else
					call ccopy(Q%getTotalData(),Qcp,1,cp,1)
					do i=2,min_MN
						cp(1:i-1,i)=0
					end do
					INFO=999
					call TData_ORGLQ(Tau%TData,Q%TData,M,N,min_MN,INFO)
					call WorkingMemory%check()
					call WorkingMemory%get_memory(cp,min_MN,N)
					cp=Qcp(1:min_MN,1:N)
					call Q%reset_dim_no_check((/min_MN/)+(Tendim.subdim.2))
					call Q%pointer(Qcp)
					Qcp=cp
					call WorkingMemory%free()
				end if
			case(5)
				call L%pointer(zp)
				call Q%pointer(Qzp,[1,M],[1,min_MN])
				if(M.le.N)then
					do i=1,min_MN
						zp(i:M,i)=Qzp(i:M,i)
						if(i.gt.1)zp(1:i-1,i)=0
					end do
					INFO=999
					call TData_ORGLQ(Tau%TData,Q%TData,M,N,min_MN,INFO)
					call Q%reset_dim_no_check((/min_MN/)+(Tendim.subdim.2))
				else
					call zcopy(Q%getTotalData(),Qzp,1,zp,1)
					do i=2,min_MN
						zp(1:i-1,i)=0
					end do
					INFO=999
					call TData_ORGLQ(Tau%TData,Q%TData,M,N,min_MN,INFO)
					call WorkingMemory%check()
					call WorkingMemory%get_memory(zp,min_MN,N)
					zp=Qzp(1:min_MN,1:N)
					call Q%reset_dim_no_check((/min_MN/)+(Tendim.subdim.2))
					call Q%pointer(Qzp)
					Qzp=zp
					call WorkingMemory%free()
				end if
		end select
		if(info.ne.0) then
			call writemess('Error in LQ decomposition ,info='+info,-1)
			call error_stop()
		end if
		return
	end subroutine
	
	function LQTensor_noName(T)	result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),target,intent(in)::T
		allocate(res(2))
		call LQdecomposition1(T,Res(1),Res(2))
		return
	end function
	
	function LQTensor_name(inputT,nameU,nameV)	result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),target,intent(in)::inputT
		character(len=*),intent(in)::nameU,nameV
		type(Tensor)::T
		integer::rank,rankU,rankV,i
		T=inputT
		rank=T%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(T%outTensorName(i).equ.nameU) rankU=rankU+1
			if(T%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in LQTensor_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in LQTensor_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in LQTensor_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		call T%forward(nameU)
		call T%fuse(1,rankU)
		call T%fuse(2,rankV+1)
		allocate(res(2))
		call LQdecomposition1(T,Res(1),Res(2))
		call Res(1)%split()
		call Res(2)%split()

		call Res(1)%setName(Res(1)%getRank(),'LQ.L')
		call Res(2)%setName(1,'LQ.Q')
		return
	end function
	type(Tensorlink) function LQ_link(T)	
		class(Tensor),target,intent(in)::T
		type(Tensornode),pointer::L,Q
		call LQ_link%addnode()
		call LQ_link%addnode()
		call LQ_link%headnode(L)
		Q=>L%next
		call LQdecomposition1(T,L%Tensor,Q%Tensor)
		return
	end function
	
	function LQTensor_Leg1(T,LegName,row_)result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		character*1,optional,intent(in)::row_
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		character*1::row
		integer::lenName,rank
		if(present(row_))then
			row=row_
		else
			row='r'
		end if
		allocate(res(2))
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row.equ.'r')then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call LQdecomposition1(Temp,res(1),res(2))
		else if(row.equ.'c')then
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call LQdecomposition1(Temp,res(1),res(2))
		else
			call writemess('ERROR in LQ, row='+row,-1)
			call writemess('row is r or c',-1)
			call error_stop
		end if
		call res(1)%split()
		call res(2)%split()

		call Res(1)%setName(Res(1)%getRank(),'LQ.L')
		call Res(2)%setName(1,'LQ.Q')
		return
	end function
	function LQTensor_Leg1_(T,LegName,row_)result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		logical,intent(in)::row_
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		integer::lenName,rank
		allocate(res(2))
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row_)then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call LQdecomposition1(Temp,res(1),res(2))
		else 
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call LQdecomposition1(Temp,res(1),res(2))
		end if
		call res(1)%split()
		call res(2)%split()

		call Res(1)%setName(Res(1)%getRank(),'LQ.L')
		call Res(2)%setName(1,'LQ.Q')
		return
	end function
	
	subroutine LQTensor_Leg1_routine(T,L,Q,LegName,row_)
		class(Tensor),intent(in)::T
		type(Tensor),intent(inout)::L,Q
		character*1,optional,intent(in)::row_
		character(len=*),intent(in)::LegName(:)
		character*1::row
		type(Tensor)::Temp
		integer::lenName,rank
		if(present(row_))then
			row=row_
		else
			row='r'
		end if
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row.equ.'r')then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call LQdecomposition1(Temp,L,Q)
		else if(row.equ.'c')then
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call LQdecomposition1(Temp,L,Q)
		else
			call writemess('DRROR in LQ, row='+row,-1)
			call writemess('row is r or c',-1)
			call error_stop
		end if
		call L%split()
		call Q%split()

		call L%setName(L%getRank(),'LQ.L')
		call Q%setName(1,'LQ.Q')
		return
	end subroutine

	subroutine LQTensor_Leg1_routine_kill_inData(Q,L,LegName,row_)
		class(Tensor),intent(inout)::Q
		type(Tensor),intent(inout)::L
		character*1,optional,intent(in)::row_
		character(len=*),intent(in)::LegName(:)
		character*1::row
		type(Tensor)::Temp
		integer::lenName,rank
		if(present(row_))then
			row=row_
		else
			row='r'
		end if
		lenName=size(LegName)
		rank=Q%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row.equ.'r')then
			call Q%forward(LegName)
			call Q%fuse(1,lenName)
			call Q%fuse(2,rank)
			call LQdecomposition_kill_inData(Q,L)
		else if(row.equ.'c')then
			call Q%backward(LegName)
			call Q%fuse(1,rank-lenName)
			call Q%fuse(2,Q%getRank())
			call LQdecomposition_kill_inData(Q,L)
		else
			call writemess('DRROR in LQ, row='+row,-1)
			call writemess('row is r or c',-1)
			call error_stop
		end if
		call L%split()
		call Q%split()

		call L%setName(L%getRank(),'LQ.L')
		call Q%setName(1,'LQ.Q')
		return
	end subroutine

	subroutine LQRoutineNameLeft(T,L,Q,LegName,rowFlag)
		class(Tensor),intent(in)::T
		type(Tensor),intent(inout)::L,Q
		logical,intent(in)::rowFlag
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		integer::lenName,rank
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(rowFlag)then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call LQdecomposition1(Temp,L,Q)
		else 
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call LQdecomposition1(Temp,L,Q)
		end if
		call L%split()
		call Q%split()

		call L%setName(L%getRank(),'LQ.L')
		call Q%setName(1,'LQ.Q')
		return
	end subroutine
	subroutine LQRoutineNameLeft_kill_inData(Q,L,LegName,rowFlag)
		class(Tensor),intent(inout)::Q
		type(Tensor),intent(inout)::L
		logical,intent(in)::rowFlag
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		integer::lenName,rank
		lenName=size(LegName)
		rank=Q%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(rowFlag)then
			call Q%forward(LegName)
			call Q%fuse(1,lenName)
			call Q%fuse(2,rank)
			call LQdecomposition_kill_inData(Q,L)
		else 
			call Q%backward(LegName)
			call Q%fuse(1,rank-lenName)
			call Q%fuse(2,Q%getRank())
			call LQdecomposition_kill_inData(Q,L)
		end if
		call L%split()
		call Q%split()

		call L%setName(L%getRank(),'LQ.L')
		call Q%setName(1,'LQ.Q')
		return
	end subroutine

	function LQTensor_Leg2(T,LegNameRow,LegNameCol)result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		character(len=*),intent(in)::LegNameRow(:),LegNameCol(:)
		type(Tensor)::Temp
		integer::lenName1,lenName2,rank
		allocate(res(2))
		lenName1=size(LegNameRow)
		lenName2=size(LegNameCol)
		rank=T%getRank()
		if((lenName1+lenName2).ne.rank)then
			call writemess('size(LegNameRow)='+lenName1+'size(LegNameCol)='+lenName2,-1)
			call writemess('rank='+rank,-1)
			call writemess('rank should be equal to size(LegNameRow)+size(LegNameCol)')
			call error_stop
		end if
		Temp=T.pf.LegNameRow
		!call Temp%backward(LegNameCol)
		call Temp%fuse(1,lenName1)
		call Temp%fuse(2,lenName2+1)
		call LQdecomposition1(Temp,res(1),res(2))
		call res(1)%split()
		call res(2)%split()

		call Res(1)%setName(Res(1)%getRank(),'LQ.L')
		call Res(2)%setName(1,'LQ.Q')
		return
	end function
	
	subroutine LQTensor_Leg2_Routine(T,L,Q,LegNameRow,LegNameCol)
		type(Tensor),intent(inout)::L,Q
		class(Tensor),intent(in)::T
		character(len=*),intent(in)::LegNameRow(:),LegNameCol(:)
		type(Tensor)::Temp
		integer::lenName1,lenName2,rank
		lenName1=size(LegNameRow)
		lenName2=size(LegNameCol)
		rank=T%getRank()
		if((lenName1+lenName2).ne.rank)then
			call writemess('size(LegNameRow)='+lenName1+'size(LegNameCol)='+lenName2,-1)
			call writemess('rank='+rank,-1)
			call writemess('rank should be equal to size(LegNameRow)+size(LegNameCol)')
			call error_stop
		end if
		Temp=T.pf.LegNameRow
		!call Temp%backward(LegNameCol)
		call Temp%fuse(1,lenName1)
		call Temp%fuse(2,lenName2+1)
		call LQdecomposition1(Temp,L,Q)
		call L%split()
		call Q%split()

		call L%setName(L%getRank(),'LQ.L')
		call Q%setName(1,'LQ.Q')
		return
	end subroutine
	subroutine LQTensor_Leg2_Routine_kill_inData(Q,L,LegNameRow,LegNameCol)
		class(Tensor),intent(inout)::Q
		type(Tensor),intent(inout)::L
		character(len=*),intent(in)::LegNameRow(:),LegNameCol(:)
		type(Tensor)::Temp
		integer::lenName1,lenName2,rank
		lenName1=size(LegNameRow)
		lenName2=size(LegNameCol)
		rank=Q%getRank()
		if((lenName1+lenName2).ne.rank)then
			call writemess('size(LegNameRow)='+lenName1+'size(LegNameCol)='+lenName2,-1)
			call writemess('rank='+rank,-1)
			call writemess('rank should be equal to size(LegNameRow)+size(LegNameCol)')
			call error_stop
		end if
		if(lenName1.lt.lenName2)then
			call Q%forward(LegNameRow)
		else
			call Q%backward(LegNameCol)
		end if
		call Q%fuse(1,lenName1)
		call Q%fuse(2,lenName2+1)
		call LQdecomposition_kill_inData(Q,L)
		call L%split()
		call Q%split()

		call L%setName(L%getRank(),'LQ.L')
		call Q%setName(1,'LQ.Q')
		return
	end subroutine
	
	
	subroutine QRdecomposition1(T,Q,R) 
		class(Tensor),target,intent(in)::T
		type(Tensor),target,intent(inout)::Q,R
		Type(Tensor)::v,vv,identity,Tau
		type(Dimension)::dimen
		integer :: i,j,M,N,min_MN,classtype,INFO
		class(Tensor),pointer::Tp
		type(Tensor),pointer::Qp,Lp
		if(getRank(T).ne.2) then
			write(*,*)"ERROR in QR decomposition"
			write(*,*)"input Tensor should be a matrix"
			call error_stop()
		endif
		Tp=>T
		Lp=>Q
		Qp=>R
		if(associated(Tp,Lp).or.associated(Tp,Qp).or.associated(Lp,Qp))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%QRTensor(Q,R)')
			call writemess('T, Q and R can not be a same variable')
			call error_stop
		end if
		Tp=>null()
		Lp=>null()
		Qp=>null()
		M = T.dim.1
		N = T.dim.2
		min_MN=min(M,N)
		INFO=999
		classtype=max(2,T%getType())
		call Q%empty()
		call R%empty()
		call R%setType(classtype)
		R=T
		call Tau%allocate((/min_MN/),classtype)
		call TData_QR(Tau%TData,R%TData,M,N,INFO)
		if(info.ne.0) then
			call writemess('Error in QR decomposition ,info='+info,-1)
			call writemess('output The data in ./_QR_ERROR_LOG.err',-1)
			open(unit=9991,file='./_QR_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
			call T%writeinfo('The Matrix in QR',9991)
			close(9991)
			call error_stop()
		end if
		
		call v%setType(classtype)
		call vv%setType(classtype)
		call identity%setType(classtype)
		
		if((M.lt.N)) then
			!compute Q
			v=zeroTen((/M/))
			identity=eye(M,M)
			do i=1,min_MN
				if(i.ne.1)then
					call v%setValue(i-1,0)
				end if
				call v%setValue(i,1)
				do j=i+1,M
					call v%setValue((/j/),R.i.(/j,i/))
				end do
				vv=tau%i(i)*(v.xx.(.h.v))
				if(i.eq.1)then
					Q=identity-vv
				else
					Q=Q*(identity-vv)
				end if
			end do
			Q=Q%subTensor((/-2,1,min_MN/))
			dimen=(T%TenDim.sub.1)+(/min_MN/)
			Q%TenDim=dimen
			!compute R
			do i=1,min_MN
				do j=1,i-1
					call R%setvalue((/i,j/),0)
				end do
			end do
			R=R%subTensor((/-1,1,min_MN/))
			dimen=(/min_MN/)+(T%TenDim.sub.2)
			R%TenDim=dimen
		else
			!compute R
			Q=R
			do i=1,min_MN
				do j=1,i-1
					call R%setvalue((/i,j/),0)
				end do
			end do
			R=R%subTensor((/-1,1,min_MN/))
			dimen=(/min_MN/)+(T%TenDim.sub.2)
			R%TenDim=dimen
			!compute Q
			INFO=999
			call TData_ORGQR(Tau%TData,Q%TData,M,N,min_MN,INFO)
			dimen=(T.subdim.1)+(/min_MN/)
			Q%TenDim=dimen
			if(info.ne.0) then
				call writemess('Error in QR decomposition ,info='+info,-1)
				call writemess('output The data in ./_QR_ERROR_LOG.err',-1)
				open(unit=9991,file='./_QR_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
				call T%writeinfo('The Matrix in QR',9991)
				call Q%writeinfo('The Q Matrix in QR',9991)
				call R%writeinfo('The R Matrix in QR',9991)
				close(9991)
				call error_stop()
			end if
		end if
		RETURN
	end subroutine
	subroutine QRdecomposition_kill_inData(Q,R) 
		class(Tensor),target,intent(inout)::Q
		type(Tensor),target,intent(inout)::R
		Type(Tensor)::v,vv,identity,Tau
		type(Dimension)::dimen
		integer :: i,j,M,N,min_MN,MM,classtype,INFO
		type(Tensor),pointer::Rp
		class(Tensor),pointer::Qp
		real*8,pointer::dp(:,:),Qdp(:,:),dvp(:),dworkingvp(:),dtaup(:)
		real*4,pointer::sp(:,:),Qsp(:,:),svp(:),sworkingvp(:),staup(:)
		complex(kind=4),pointer::cp(:,:),Qcp(:,:),cvp(:),cworkingvp(:),ctaup(:)
		complex(kind=8),pointer::zp(:,:),Qzp(:,:),zvp(:),zworkingvp(:),ztaup(:)
		type(Dimension)::TenDim
		if(getRank(Q).ne.2) then
			call writemess("ERROR in QR decomposition",-1)
			call writemess("input Tensor should be a matrix",-1)
			call error_stop()
		endif
		TenDim=Q%TenDim
		Qp=>Q
		Rp=>R
		if(associated(Qp,Rp))then
			call writemess('input Tensors can not be the same variable',-1)
			call writemess('error in call T%QRTensor(Q,R)',-1)
			call writemess('T, Q and R can not be a same variable',-1)
			call error_stop
		end if
		Rp=>null()
		Qp=>null()
		M = Q.dim.1
		N = Q.dim.2
		min_MN=min(M,N)
		INFO=999
		classtype=max(2,Q%getType())
		call Tau%allocate((/min_MN/),classtype)
		call TData_QR(Tau%TData,Q%TData,M,N,INFO)
		if(info.ne.0) then
			call writemess('Error in QR decomposition ,info='+info,-1)
			call error_stop()
		end if
		
		call R%empty()
		call R%allocate((/min_MN/)+(TenDim.sub.2),classtype)
		
		select case(classtype)
			case(2)
				call R%pointer(sp)
				call Q%pointer(Qsp,[1,min_MN],[1,N])
				if(M.lt.N)then
					call scopy(Q%getTotalData(),Qsp,1,sp,1)
					do i=1,min_MN-1
						sp(i+1:min_MN,i)=0
					end do
				else
					do i=1,N
						sp(:i,i)=Qsp(:i,i)
						if(i.lt.N)sp(i+1:,i)=0
					end do
				end if
				
			case(3)
				call R%pointer(dp)
				call Q%pointer(Qdp,[1,min_MN],[1,N])
				if(M.lt.N)then
					call dcopy(Q%getTotalData(),Qdp,1,dp,1)
					do i=1,min_MN-1
						dp(i+1:min_MN,i)=0
					end do
				else
					do i=1,min_MN
						dp(:i,i)=Qdp(:i,i)
						if(i.lt.min_MN)dp(i+1:min_MN,i)=0
					end do
				end if
			case(4)
				call R%pointer(cp)
				call Q%pointer(Qcp,[1,min_MN],[1,N])
				if(M.lt.N)then
					call ccopy(Q%getTotalData(),Qcp,1,cp,1)
					do i=1,min_MN-1
						cp(i+1:min_MN,i)=0
					end do
				else
					do i=1,min_MN
						cp(:i,i)=Qcp(:i,i)
						if(i.lt.min_MN)cp(i+1:min_MN,i)=0
					end do
				end if
			case(5)
				call R%pointer(zp)
				call Q%pointer(Qzp,[1,min_MN],[1,N])
				if(M.lt.N)then
					call zcopy(Q%getTotalData(),Qzp,1,zp,1)
					do i=1,min_MN-1
						zp(i+1:min_MN,i)=0
					end do
				else
					do i=1,N
						zp(:i,i)=Qzp(:i,i)
						if(i.lt.N)zp(i+1:N,i)=0
					end do
				end if
				
		end select
		INFO=999
		call TData_ORGQR(Tau%TData,Q%TData,M,N,min_MN,INFO)
		call Q%reset_dim_no_check((TenDim.subdim.1)+[min_MN])
		if(info.ne.0) then
			call writemess('Error in QR decomposition ,info='+info,-1)
			call error_stop()
		end if
		RETURN
	end subroutine




	function QRTensor_noName(T)	result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),target,intent(in)::T
		allocate(res(2))
		call QRdecomposition1(T,Res(1),Res(2))
		return
	end function
	function QRTensor_name(inputT,nameU,nameV)	result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),target,intent(in)::inputT
		character(len=*),intent(in)::nameU,nameV
		type(Tensor)::T
		integer::rank,rankU,rankV,i
		T=inputT
		rank=T%getRank()
		rankU=0
		rankV=0
		do i=1,rank
			if(T%outTensorName(i).equ.nameU) rankU=rankU+1
			if(T%outTensorName(i).equ.nameV) rankV=rankV+1
		end do
		if(rankU+rankV.ne.rank) then
			call writemess("ERROR in QRTensor_name",-1)
			call writemess(rankU+','+rankV+','+rank,-1)
			call error_stop()
		end if
		if(rankU.eq.0) then
			call writemess("ERROR in QRTensor_name,no such name",-1)
			call writemess(nameU,-1)
			call error_stop()
		end if
		if(rankV.eq.0) then
			call writemess("ERROR in QRTensor_name,no such name",-1)
			call writemess(nameV,-1)
			call error_stop()
		end if
		call T%forward(nameU)
		call T%fuse(1,rankU)
		call T%fuse(2,rankV+1)
		allocate(res(2))
		call QRdecomposition1(T,Res(1),Res(2))
		call Res(1)%split()
		call Res(2)%split()

		call Res(1)%setName(Res(1)%getRank(),'QR.Q')
		call Res(2)%setName(1,'QR.R')
		return
	end function
	
	type(Tensorlink) function QR_link(T)	
		class(Tensor),target,intent(in)::T
		type(Tensornode),pointer::Q,R
		call QR_link%addnode()
		call QR_link%addnode()
		call QR_link%headnode(Q)
		R=>Q%next
		call QRdecomposition1(T,Q%Tensor,R%Tensor)
		return
	end function
	
	
	function QRTensor_Leg1(T,LegName,row_)result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		character*1,optional,intent(in)::row_
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		character*1::row
		integer::lenName,rank
		if(present(row_))then
			row=row_
		else
			row='r'
		end if
		allocate(res(2))
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row.equ.'r')then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call QRdecomposition1(Temp,res(1),res(2))
		else if(row.equ.'c')then
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call QRdecomposition1(Temp,res(1),res(2))
		else
			call writemess('ERROR in LQ, row='+row,-1)
			call writemess('row is r or c',-1)
			call error_stop
		end if
		call res(1)%split()
		call res(2)%split()

		call Res(1)%setName(Res(1)%getRank(),'QR.Q')
		call Res(2)%setName(1,'QR.R')
		return
	end function

	function QRTensor_Leg1_(T,LegName,row_)result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		logical,intent(in)::row_
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		integer::lenName,rank
		allocate(res(2))
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row_)then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call QRdecomposition1(Temp,res(1),res(2))
		else 
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call QRdecomposition1(Temp,res(1),res(2))
		end if
		call res(1)%split()
		call res(2)%split()

		call Res(1)%setName(Res(1)%getRank(),'QR.Q')
		call Res(2)%setName(1,'QR.R')
		return
	end function
	
	subroutine QRTensor_Leg1_routine(T,Q,R,LegName,row_)
		class(Tensor),intent(in)::T
		type(Tensor),intent(inout)::Q,R
		character*1,optional,intent(in)::row_
		character(len=*),intent(in)::LegName(:)
		character*1::row
		type(Tensor)::Temp
		integer::lenName,rank
		if(present(row_))then
			row=row_
		else
			row='r'
		end if
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row.equ.'r')then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call QRdecomposition1(Temp,Q,R)
		else if(row.equ.'c')then
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call QRdecomposition1(Temp,Q,R)
		else
			call writemess('DRROR in LQ, row='+row,-1)
			call writemess('row is r or c',-1)
			call error_stop
		end if
		call Q%split()
		call R%split()

		call Q%setName(Q%getRank(),'QR.Q')
		call R%setName(1,'QR.R')
		return
	end subroutine

	subroutine QRTensor_Leg1_routine_kill_indaTa(Q,R,LegName,row_)
		class(Tensor),intent(inout)::Q
		type(Tensor),intent(inout)::R
		character*1,optional,intent(in)::row_
		character(len=*),intent(in)::LegName(:)
		character*1::row
		integer::lenName,rank
		type(Dimension)::TenDim
		if(present(row_))then
			row=row_
		else
			row='r'
		end if
		lenName=size(LegName)
		rank=Q%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(row.equ.'r')then
			call Q%forward(LegName)
			call Q%fuse(1,lenName)
			call Q%fuse(2,rank)
			call QRdecomposition_kill_inData(Q,R)
		else if(row.equ.'c')then
			call Q%backWard(LegName)
			call Q%fuse(1,rank-lenName)
			call Q%fuse(2,Q%getRank())
			call QRdecomposition_kill_inData(Q,R)
		else
			call writemess('DRROR in LQ, row='+row,-1)
			call writemess('row is r or c',-1)
			call error_stop
		end if
		call Q%split()
		call R%split()

		call Q%setName(Q%getRank(),'QR.Q')
		call R%setName(1,'QR.R')
		return
	end subroutine

	subroutine QRRoutineNameLeft(T,Q,R,LegName,rowFlag)
		class(Tensor),intent(in)::T
		type(Tensor),intent(inout)::Q,R
		logical,intent(in)::rowFlag
		character(len=*),intent(in)::LegName(:)
		type(Tensor)::Temp
		integer::lenName,rank
		lenName=size(LegName)
		rank=T%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(rowFlag)then
			Temp=T.pf.LegName
			call Temp%fuse(1,lenName)
			call Temp%fuse(2,rank)
			call QRdecomposition1(Temp,Q,R)
		else 
			Temp=T.pb.LegName
			call Temp%fuse(1,rank-lenName)
			call Temp%fuse(2,Temp%getRank())
			call QRdecomposition1(Temp,Q,R)
		end if
		call Q%split()
		call R%split()

		call Q%setName(Q%getRank(),'QR.Q')
		call R%setName(1,'QR.R')
		return
	end subroutine

	subroutine QRRoutineNameLeft_kill_inData(Q,R,LegName,rowFlag)
		class(Tensor),intent(inout)::Q
		type(Tensor),intent(inout)::R
		logical,intent(in)::rowFlag
		character(len=*),intent(in)::LegName(:)
		integer::lenName,rank
		lenName=size(LegName)
		rank=Q%getRank()
		if(lenName.ge.rank)then
			call writemess('ERROR in the number of legs')
			call writemess('number of leg can not larger than of equal to the rank of the tensor')
			call error_stop
		end if
		if(rowFlag)then
			call Q%forward(LegName)
			call Q%fuse(1,lenName)
			call Q%fuse(2,rank)
			call QRdecomposition_kill_inData(Q,R)
		else 
			call Q%backward(LegName)
			call Q%fuse(1,rank-lenName)
			call Q%fuse(2,Q%getRank())
			call QRdecomposition_kill_inData(Q,R)
		end if
		call Q%split()
		call R%split()

		call Q%setName(Q%getRank(),'QR.Q')
		call R%setName(1,'QR.R')
		return
	end subroutine

	function QRTensor_Leg2(T,LegNameRow,LegNameCol)result(res)
		type(Tensor),allocatable::res(:)
		class(Tensor),intent(in)::T
		character(len=*),intent(in)::LegNameRow(:),LegNameCol(:)
		type(Tensor)::Temp
		integer::lenName1,lenName2,rank
		allocate(res(2))
		lenName1=size(LegNameRow)
		lenName2=size(LegNameCol)
		rank=T%getRank()
		if((lenName1+lenName2).ne.rank)then
			call writemess('size(LegNameRow)='+lenName1+'size(LegNameCol)='+lenName2,-1)
			call writemess('rank='+rank,-1)
			call writemess('rank should be equal to size(LegNameRow)+size(LegNameCol)')
			call error_stop
		end if
		Temp=T.pf.LegNameRow
		!call Temp%backward(LegNameCol)
		call Temp%fuse(1,lenName1)
		call Temp%fuse(2,lenName2+1)
		call QRdecomposition1(Temp,res(1),res(2))
		call res(1)%split()
		call res(2)%split()

		call Res(1)%setName(Res(1)%getRank(),'QR.Q')
		call Res(2)%setName(1,'QR.R')
		return
	end function
	
	subroutine QRTensor_Leg2_Routine(T,Q,R,LegNameRow,LegNameCol)
		type(Tensor),intent(inout)::Q,R
		class(Tensor),intent(in)::T
		character(len=*),intent(in)::LegNameRow(:),LegNameCol(:)
		type(Tensor)::Temp
		integer::lenName1,lenName2,rank
		lenName1=size(LegNameRow)
		lenName2=size(LegNameCol)
		rank=T%getRank()
		if((lenName1+lenName2).ne.rank)then
			call writemess('size(LegNameRow)='+lenName1+'size(LegNameCol)='+lenName2,-1)
			call writemess('rank='+rank,-1)
			call writemess('rank should be equal to size(LegNameRow)+size(LegNameCol)')
			call error_stop
		end if
		Temp=T.pf.LegNameRow
		!call Temp%backward(LegNameCol)
		call Temp%fuse(1,lenName1)
		call Temp%fuse(2,lenName2+1)
		call QRdecomposition1(Temp,Q,R)
		call Q%split()
		call R%split()

		call Q%setName(Q%getRank(),'QR.Q')
		call R%setName(1,'QR.R')

		return
	end subroutine

	subroutine QRTensor_Leg2_Routine_kill_inData(Q,R,LegNameRow,LegNameCol)
		class(Tensor),intent(inout)::Q
		type(Tensor),intent(inout)::R
		character(len=*),intent(in)::LegNameRow(:),LegNameCol(:)
		type(Tensor)::Temp
		integer::lenName1,lenName2,rank
		lenName1=size(LegNameRow)
		lenName2=size(LegNameCol)
		rank=Q%getRank()
		if((lenName1+lenName2).ne.rank)then
			call writemess('size(LegNameRow)='+lenName1+'size(LegNameCol)='+lenName2,-1)
			call writemess('rank='+rank,-1)
			call writemess('rank should be equal to size(LegNameRow)+size(LegNameCol)')
			call error_stop
		end if
		if(lenName1.lt.lenName2)then
			call Q%forward(LegNameRow)
		else
			call Q%backward(LegNameCol)
		end if
		call Q%fuse(1,lenName1)
		call Q%fuse(2,lenName2+1)
		call QRdecomposition_kill_inData(Q,R)
		call Q%split()
		call R%split()

		call Q%setName(Q%getRank(),'QR.Q')
		call R%setName(1,'QR.R')

		return
	end subroutine
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  Tensorlink and Tensornode 	  
!
!**************************************************************************************************************
!**************************************************************************************************************	



	!	add a Tensor to the end of the link
	!	there is no vector index in the  input Tensor
	subroutine push_backTen(h,T,indices)
		class(Tensorlink),intent(inout) ::h
		class(Tensor),TARGET,intent(in):: T
		type(Tensornode),pointer ::node
		integer,optional,intent(in) :: indices(:)
		select type(T)
			type is (Tensor)
				allocate(node)
				node=T
			type is (TensorNode)
				node=>T
				nullify(node%next)
		end select
		if(present(indices))then
			call setindices_to_node(node,indices)
		end if
		nullify(node%next)
		if(h%length.eq.0) then
			h%head=>node
			h%length=1
			h%Tend=>node
		else
			h%length=h%length+1
			h%Tend%next=>node
			h%Tend=>node
		end if
		return
	end subroutine
	!	add a Tensor to the begin of the link	
	!	there is no vector index in the  input Tensor
	subroutine push_fo(h,T,indices)
		class(Tensorlink),intent(inout) ::h
		class(Tensor),TARGET,intent(in):: T
		type(Tensornode),pointer ::node,p
		integer,optional,intent(in) :: indices(:)
		select type(T)
			type is (Tensor)
				allocate(node)
				node=T
			type is (TensorNode)
				node=>T
				nullify(node%next)
		end select
		if(present(indices))then
			call setindices_to_node(node,indices)
		end if
		nullify(node%next)
		if(h%length.eq.0) then
			h%head=>node
			h%length=1
			h%Tend=>node
		else
			p=>h%head
			h%head=>node
			node%next=>p
			h%length=h%length+1
		end if
		return
	end subroutine

	!	add a munber to the end of the link
	!	there is no vector index in the  input Tensor
	subroutine push_backclass(h,num,indices)
		class(Tensorlink),intent(inout) ::h
		class(*),intent(in) ::num
		type(Tensornode),pointer::Ten
		integer,optional,intent(in) :: indices(:)
		select type(num)
			type is (integer)
				allocate(Ten)
				Ten%Tensor=num
				call push_backTen(h,Ten,indices)
				return
			type is (real(kind=4))
				allocate(Ten)
				Ten%Tensor=num
				call push_backTen(h,Ten,indices)
				return
			type is (real(kind=8))
				allocate(Ten)
				Ten%Tensor=num
				call push_backTen(h,Ten,indices)
				return
			type is (complex(kind=4))
				allocate(Ten)
				Ten%Tensor=num
				call push_backTen(h,Ten,indices)
				return
			type is (complex(kind=8))
				allocate(Ten)
				Ten%Tensor=num
				call push_backTen(h,Ten,indices)
				return
			type is (logical)
				allocate(Ten)
				Ten%Tensor=num
				call push_backTen(h,Ten,indices)
				return
			type is (character(len=*))
				allocate(Ten)
				Ten%Tensor=num
				call push_backTen(h,Ten,indices)
				return
			class is (Tensor)
				call push_backTen(h,num,indices)
				return
			class default 
				write(*,*)"no such class"
				stop
		end 	select
	end subroutine
	!	add a compelx munber to the begin of the link	
	!	there is no vector index in the  input Tensor
	subroutine push_foclass(h,num,indices)
		class(Tensorlink),intent(inout) ::h
		class(*),intent(in) ::num
		type(Tensornode),pointer::Ten
		integer,optional,intent(in) :: indices(:)
		select type(num)
			type is (integer)
				allocate(Ten)
				Ten%Tensor=num
				call push_fo(h,Ten,indices)
				return
			type is (real(kind=4))
				allocate(Ten)
				Ten%Tensor=num
				call push_fo(h,Ten,indices)
				return
			type is (real(kind=8))
				allocate(Ten)
				Ten%Tensor=num
				call push_fo(h,Ten,indices)
				return
			type is (complex(kind=4))
				allocate(Ten)
				Ten%Tensor=num
				call push_fo(h,Ten,indices)
				return
			type is (complex(kind=8))
				allocate(Ten)
				Ten%Tensor=num
				call push_fo(h,Ten,indices)
				return
			type is (logical)
				allocate(Ten)
				Ten%Tensor=num
				call push_fo(h,Ten,indices)
				return
			type is (character(len=*))
				allocate(Ten)
				Ten%Tensor=num
				call push_fo(h,Ten,indices)
				return
			class is (Tensor)
				call push_fo(h,num,indices)
				return
			class default 
				write(*,*)"no such class"
				stop
		end 	select
	end subroutine
	!	return the inde Tensor in the link
	type(Tensor) function  Ti(h,inde)		
		class(Tensorlink),intent(in) :: h
		integer,intent(in) :: inde
		integer ::i
		type(Tensornode),pointer ::p
		if(h%length.lt.inde) then
			write(*,*)"ERROR in Ti"
		else
			p=>h%head
			do i=1,inde-1
				p=>p%next
			end do
			Ti=p
		end if
		return
	end  function
	!	return the inde Tensor in the link
	!	inde is vector	
	type(Tensor) function  TiI(h,inde)		
		class(Tensorlink),intent(in) :: h
		integer,intent(in) :: inde(:)
		integer ::i
		type(Tensornode),pointer ::p
		logical::continu
		continu=.true.
		if(lenOfIndice(h).ne.size(inde)) then
			write(*,*)"ERROR in TiI"
			write(*,*)"length of index of input"
			write(*,*)size(inde)
			write(*,*)"length of the index in the link is"
			write(*,*)lenOfIndice(h)
			call error_stop()
		else
			p=>h%head
			i=1
			do while (continu)
				if(inde.equ.p%indices) then
					TiI=p
					return
					continu=.false.
				end if
				p=>p%next
				i=i+1
				if(i.eq.h%length+1) then
					continu=.false.
				end if
			end do
		end if
		return
	end  function
	!	return the inde Tensor in the link
	!	inde is vector	
	! if no such Tensor,return .false.
	! else retrun .true.
	logical function  ifTiI(TiI,h,inde)		
		type(Tensorlink),intent(in) :: h
		type(Tensor),intent(out)::TiI
		integer,intent(in) :: inde(:)
		integer ::i
		type(Tensornode),pointer ::p
		logical::continu
		continu=.true.
		ifTiI=.false.
		if(lenOfIndice(h).ne.size(inde)) then
			write(*,*)"ERROR in TiI"
			write(*,*)"length of index of input"
			write(*,*)size(inde)
			write(*,*)"length of the index in the link is"
			write(*,*)lenOfIndice(h)
			call error_stop()
		else
			p=>h%head
			i=1
			do while (continu)
				if(inde.equ.p%indices) then
					TiI=p
					ifTiI=.true.
					return
					continu=.false.
				end if
				p=>p%next
				i=i+1
				if(i.eq.h%length+1) then
					continu=.false.
				end if
			end do
		end if
		return
	end  function
	!	return the inde Tensornode's address in the link
	!	on return,output is a pointer
	 subroutine  node_i_int(h,p,inde)
	 	type(Tensornode),pointer,intent(inout)::p
		type(Tensorlink),intent(in) :: h
		integer,intent(in) :: inde
		integer ::i
		if(h%length.lt.inde) then
			write(*,*)"ERROR in Ti"
		else
			p=>h%head
			do i=1,inde-1
				p=>p%next
			end do
		end if
		return
	end  subroutine
	!	return the inde Tensornode's address in the link
	!	inde is vector	
	!	on return,output is a pointer
	subroutine  node_i_vec(h,p,inde) 
		type(Tensorlink),intent(in) :: h
		integer,intent(in) :: inde(:)
		integer ::i
		type(Tensornode),pointer,intent(inout) ::p
		logical::continu
		continu=.true.
		if(lenOfIndice(h).ne.size(inde)) then
			write(*,*)"ERROR in TiI"
		else
		
			i=1
			p=>h%head
			if(inde.equ.p%indices) then
				return
			end if
			
			do while (continu)
				i=i+1
				p=>p%next
				if(inde.equ.p%indices) then
					return
				end if
				if(i.gt.h%length+1) then
					write(*,*)"ERROR in node_i_vec"
					call error_stop()
				end if
			end do
			
		end if
		return
	end  subroutine	
	! return the length of the index of the first Tesnor
	integer function lenOfIndice(h)
		type(Tensorlink),intent(in) :: h
		type(Tensornode),pointer ::p
		p=>h%head
		lenOfIndice=size(p%indices)
		return
	end function
	!	return the index of the inde Tensor
	!	T_{1,1}->T_{1,2}->T_{1,3}->T_{2,1}->T_{2,2}
	!	inde=2 => indice=[1,2]
	! on entry the size of indice should be equal to the one in h
	subroutine TenIndice(h,inde,indice)
		type(Tensorlink),intent(in) :: h
		integer,intent(in) :: inde
		integer,intent(inout) :: indice(:)
		integer :: i,lenOfind
		type(Tensornode),pointer ::p
		lenOfind=size(indice)
		if(lenOfind.ne.lenOfIndice(h)) then
			write(*,*) "Error in TenIndice"
			write(*,*) lenOfind,lenOfIndice(h)
			p=>h%head
			p=>p%Next
			write(*,*)p%indices
			write(*,*)"stop"
			call error_stop()
		end if
		if(h%length.lt.inde) then
			write(*,*)"ERROR in TenIndice"
		else
			p=>h%head
			do i=1,inde-1
				p=>p%next
			end do
			indice=p%indices
		end if
		return
	end subroutine

	! if the next node exict,p point to the next node,or p remain nochange
	logical function ifnextnode(p)
		type(Tensornode),pointer,intent(inout) ::p
		if(associated(p%next)) then
			ifnextnode=.true.
			p=>p%next
		else
			ifnextnode=.false.
		end if
	end function
	!	p is a pointer of Tensornode,p points to next
	subroutine nextnode(p)
		type(Tensornode),pointer,intent(inout) ::p
		if(associated(p%next)) then
			p=>p%next
		else
			write(*,*)"Error in nextnode,p is pointing to the end of the link"
			call error_stop()
		end if
	end subroutine
	!	p is a pointer of Tensornode,p points to head of the h	
	subroutine headnode(h,p)
		class(Tensorlink),intent(in)::h
		type(Tensornode),pointer,intent(inout) ::p
		p=>h%head
	end subroutine
	!	p is a pointer of Tensornode,p points to end of the h	
	subroutine endnode(h,p)
		class(Tensorlink),intent(in)::h
		type(Tensornode),pointer,intent(inout) ::p
		p=>h%Tend
	end subroutine
	!	add a empty node to the link
	subroutine addnode(h)
		class(Tensorlink),intent(inout)::h
		type(Tensornode),pointer ::p
		allocate(p)
		!call push_back(h,p)
		call h%Tbackward(p)
		return
	end subroutine
	subroutine nodei(h,p,ith)
		class(Tensorlink),intent(inout)::h
		type(Tensornode),pointer,intent(inout) ::p
		integer,intent(in)::ith
		integer::i
		p=>h%head
		do i=2,ith
			p=>p%next
		end do
		return
	end subroutine
		
		
	!	get the index of the inde Tensor,and return .true.
	!	T_{1,1}->T_{1,2}->T_{1,3}->T_{2,1}->T_{2,2}
	!	inde=2 => indice=[1,2]
	!	if there no index in h,return .false.
	logical function TenIndiceLog(h,inde,indice)
		type(Tensorlink),intent(in) :: h
		integer,intent(in) :: inde
		integer,allocatable,intent(out) :: indice(:)
		integer :: i
		type(Tensornode),pointer ::p
		if(h%length.lt.inde) then!if[2]
			write(*,*)"ERROR in TenIndiceLog"
		else!if[2]
			p=>h%head
			do i=1,inde-1
				p=>p%next
			end do
			if(allocated(p%indices)) then!if[3]
				TenIndiceLog=.true.
				allocate(indice(size(p%indices)))
				indice=p%indices
			else!if[3]
				TenIndiceLog=.false.
			end if!if[3]
		end if!if[2]
		return
	end function
	!check and output length of the link		
	!check if the length of the link is equal to the value in head of the link
	integer function Checklength(link)
		type(Tensorlink),intent(in) :: link
		type(Tensornode),pointer ::p
		integer ::num
		Checklength=link%length
		p=>link%head
		num=0
		do while(associated(p))
			p=>p%next
			num=num+1
		end do
		if(num.ne.Checklength)then
			write(*,*)"The length of link is",num
			write(*,*)"The value of length in link is" ,Checklength
			write(*,*)"Error in length of link"
			write(*,*)"stop !"
			call error_stop()
		end if
		return
	end function
	!	output length of the link		
	integer function linklength(link)
		class(Tensorlink),intent(in) :: link
		linklength=link%length
		return
	end function	
	
	!	modify the inde element of the link
	!	The Tensor will be modify
	subroutine modifylink(h,T,inde)
		class(Tensorlink),intent(inout):: h
		integer,intent(in) :: inde
		type(Tensor),intent(in):: T
		integer ::i
		type(Tensornode),pointer ::p
		if(h%length.lt.inde) then
			write(*,*)"ERROR in modify,length"
			else
			p=>h%head
			do i=1,inde-1
				p=>p%next
			end do
			p=T
		end if
		return
	end subroutine
	!	clean the link
	subroutine cleanlink(h)
		class(Tensorlink),intent(inout) :: h
		type(Tensornode),pointer ::p,p1
		if(h%length.eq.0) then!if[2]
			nullify(h%head)
			nullify(h%Tend)
			return
		end if!if[2]
		p=>h%head
		do while(associated(p))
			p1=>p%next
			call cleanTensor(p)
			deallocate(p)
			p=>p1
		end do
		h%length=0
		nullify(h%head)
		nullify(h%Tend)
		return
	end subroutine
	!	copy the link hin to hout
	!	hin will not change
	subroutine copylink(hout,hin)
		type(Tensorlink),intent(inout) :: hout
		type(Tensorlink),intent(in) :: hin
		integer :: i
		integer,allocatable::indes(:)
		logical :: logi
		call cleanlink(hout)
		do i=1,hin%length
			logi=TenIndiceLog(hin,i,indes)
			if(logi) then!if[2]
				!call push_back(hout,hin.i.i,indes)
				call hout%Tbackward(hin.i.i,indes)
			else
				!call push_back(hout,hin.i.i)
				call hout%Tbackward(hin.i.i)
			end if!if[2]
		end do
		return
	end subroutine
	subroutine copylinkhead(hout,hin)
		type(Tensorlink),intent(inout) :: hout
		type(Tensorlink),intent(in) :: hin
		integer :: i
		integer,allocatable::indes(:)
		logical :: logi
		!call cleanlink(hout)
		hout%head=>hin%head
		hout%Tend=>hin%Tend
		hout%length=hin%length
		return
	end subroutine
		
	!	connect two links:
	!			link1 :T1->T2->T4->T5...->TN
	!			link2: TT1->TT2->..->TTM
	!			result link will b2 T1->T2->..->TN->TT1->TT2->...->TTM
		type(Tensorlink) function connectlink(link1,link2)
			type(Tensorlink),intent(in) :: link1,link2
			type(Tensorlink) :: Templink
			type(Tensornode),pointer ::p
			call copylink(connectlink,link1)
			call copylink(Templink,link2)
			p=>connectlink%Tend
			p%next=>Templink%head
			connectlink%Tend=>Templink%Tend
			connectlink%length=connectlink%length+Templink%length
			return
		end function
	!***************  deletelink  **********************
	!	link   :	T1->T2->...->T_{inde1-1}->T_inde1->T_{inde1+1}->....->T_{inde2-1}->T_inde2->T_{inde2+1}...->TN 
	!  result:		T1->T2->...->T_{inde1-1}->T_{inde1+1}....->T_{inde2-1}->->T_{inde2+1}...->TN 
	!  delete the inde1 to inde2 Tensor in the link,note that the deleted Tensor are include the Tensors of inde1 and inde2

	type(Tensorlink) function deletelink(link_in,inde1,inde2) result(link)			
		type(Tensorlink),intent(in) :: link_in
		integer,intent(in)::inde1,inde2
		type(Tensorlink) :: Templink
		type(Tensornode),pointer ::p1,p2,p
		integer::i
		if(inde1.gt.link%length) then!if[2]
			write(*,*) "error"
			write(*,*) "inde1 larger than the length of link"
			write(*,*)"program will stop"
			call error_stop()
		end if!if[2]
		if(inde1.gt.inde2) then!if[2]
			write(*,*)"error"
			write(*,*) "inde1 should not larger than inde2"
			write(*,*)"program will stop"
			call error_stop()
		end if!if[3]
		call copylink(link,link_in)
		if(inde1.eq.1) then!if[4]
			if(inde2.eq.link%length) then!if[5]
				call cleanlink(link)
				return
			end if!if[5]
			p2=>link%head
			do i=1,inde2
				p=>p2
				p2=>p2%next
				call cleanTensor(p)
				deallocate(p)
			end do
			link%head=>p2
			link%length=link%length-(inde2-inde1)-1
			return
		end if!if[4]
		if(inde2.eq.link%length) then!if[6]
			p1=>link%head
			do i=2,inde1-1
				p1=>p1%next
			end do
			p2=>p1%next
			link%Tend=>p1
			p=>p1%next
			nullify(p1)
			do while(associated(p))
				p2=>p%next
				call cleanTensor(p)
				deallocate(p)
				p=>p2
			end do
			link%length=link%length-(inde2-inde1)-1
			return
		end if!if[6]
		p1=>link%head
		do i=2,inde1-1
			p1=>p1%next
		end do
		p2=>p1%next
		do i=inde1,inde2
			p=>p2
			p2=>p2%next
			call cleanTensor(p)
			deallocate(p)
		end do
		p1%next=>p2
		link%length=link%length-(inde2-inde1)-1
		return
	end function		
	
	subroutine deletelinksubroutine(link,inde1,inde2_)		
		class(Tensorlink),intent(inout) :: link
		integer,intent(in)::inde1
		integer,optional,intent(in)::inde2_
		integer::inde2
		type(Tensorlink) :: Templink
		type(Tensornode),pointer ::p1,p2,p
		integer::i
		if(inde1.gt.link%length) then!if[2]
			write(*,*) "error"
			write(*,*) "inde1 larger than the length of link"
			write(*,*)"program will stop"
			call error_stop()
		end if!if[2]
		if(present(inde2_))then
			inde2=inde2_
		else
			inde2=inde1
		end if
		if(inde1.gt.inde2) then!if[2]
			write(*,*)"error"
			write(*,*) "inde1 should not larger than inde2"
			write(*,*)"program will stop"
			call error_stop()
		end if!if[3]
		if(inde1.eq.1) then!if[4]
			if(inde2.eq.link%length) then!if[5]
				call cleanlink(link)
				return
			end if!if[5]
			p2=>link%head
			do i=1,inde2
				p=>p2
				p2=>p2%next
				call cleanTensor(p)
				deallocate(p)
			end do
			link%head=>p2
			link%length=link%length-(inde2-inde1)-1
			return
		end if!if[4]
		if(inde2.eq.link%length) then!if[6]
			p1=>link%head
			do i=2,inde1-1
				p1=>p1%next
			end do
			p2=>p1%next
			link%Tend=>p1
			p=>p1%next
			nullify(p1)
			do while(associated(p))
				p2=>p%next
				call cleanTensor(p)
				deallocate(p)
				p=>p2
			end do
			link%length=link%length-(inde2-inde1)-1
			return
		end if!if[6]
		p1=>link%head
		do i=2,inde1-1
			p1=>p1%next
		end do
		p2=>p1%next
		do i=inde1,inde2
			p=>p2
			p2=>p2%next
			call cleanTensor(p)
			deallocate(p)
		end do
		p1%next=>p2
		link%length=link%length-(inde2-inde1)-1
		return
	end subroutine		

	!Tensor=Tensorlink,if every element(Tensor) in the link is a one-element Tensor
	subroutine TenLink(Ten,link)
		type(Tensor),intent(inout)::Ten
		type(Tensorlink),intent(in)::link
		complex*16,allocatable::Tensordata(:)
		integer::Tenlen,i
		logical::goon
		type(Tensornode),pointer::p
		Tenlen=linklength(link)
		allocate(Tensordata(Tenlen))
		call headnode(link,p)
		do i=1,Tenlen
			if(getTotaldata(p).eq.1)then
				Tensordata(i)=p.i.1
			else
				write(*,*)"error in assignment of a link to Tensor"
			end if
			goon=ifnextnode(p)
		end do
		Ten=Tensordata
		nullify(p)
		return
	end subroutine
	
	subroutine Lprint(link)
		type(Tensorlink),intent(in) :: link
		integer::lenlink,i
		logical::goon,goon2
		integer,allocatable::indice(:)
		lenlink=Checklength(link)
		if(lenlink.eq.0) then
			write(*,*)"There is no Tensor in the link"
			return
		end if
		write(*,*) "length of the link is",lenlink
		goon=TenIndiceLog(link,1,indice)
		if(goon) then
			write(*,*)"indice of every Tensor in the link are"
			do i=1,lenlink
				goon2=TenIndiceLog(link,i,indice)
				if(goon2) then
					write(*,*)indice
				else
					write(*,*)"no indece"
				end if
			end do
		else
			goon=.true.
			do i=1,lenlink
				goon2=TenIndiceLog(link,i,indice)
				if(goon2) then
					write(*,*)"error in the link,there are some Tensor with indice,some are not"
					goon=.false.
				end if
			end do
		end if
		return
	end subroutine
	
	subroutine LDprint(link)
		type(Tensorlink),intent(in) :: link
		type(Tensornode),pointer::p
		integer::lenlink,i
		logical::goon
		integer,allocatable::indice(:)
		lenlink=Checklength(link)
		if(lenlink.eq.0) then
			write(*,*)"There is no Tensor in the link"
			return
		end if
		p=>link%head
		call p%TenDim%print()
		if(allocated(p%indices)) then
			write(*,*)"indice",p%indices
		end if
		i=1
		do while(associated(p%next))
			p=>p%next
			write(*,*)"----------------------------------",i
			i=i+1
			call p%TenDim%print()
			if(allocated(p%indices)) then
				write(*,*)"indice",p%indices
			end if
		end do
		write(*,*)"--------- End of the link -----------",i
		return
	end subroutine

	subroutine linkToTenArray(T,link)
		type(Tensor),intent(inout) ::T(:)
		class(Tensorlink),intent(in) :: link
		integer::length,i
		length=linklength(link)
		if(size(T).lt.length)then
			write(*,*)"ERROR in assignment oflink to Tensor array "
			write(*,*)"T1(:)=link,size(T1)<linklength(link)"
			write(*,*)size(T),length
			call error_stop()
		end if
		do i=1,length
			T(i)=link%i(i)
		end do
		return
	end subroutine

	subroutine TenArrayToLink(link,T)
		class(Tensorlink),intent(inout) :: link
		class(Tensor),intent(in) ::T(:)
		type(Tensornode),pointer::p
		integer::length,i
		length=size(T)
		p=>link%head
		do i=1,length
			if(getflag(T(i))) then
				if(associated(p))then
					p=T(i)
					p=>p%next
				else
					call push_backTen(link,T(i))
					nullify(p)
				end if
			end if
		end do
		return
	end subroutine
	
	subroutine TenArrayToTensor(outT,T)
		class(Tensor),intent(inout) :: outT
		class(Tensor),intent(in) ::T(:)
		integer::length,i,totaldata,classtype,i1,i2
		length=size(T)
		totaldata=T(1)%getTotalData()
		classtype=T(1)%getType()
		do i=2,length
			totaldata=totaldata+T(i)%getTotalData()
			if(classtype.lt.T(i)%getType())then
				classtype=T(i)%getType()
			end if
		end do
		call allocatedTensor(outT,(/totaldata/),classtype)
		i1=1
		i2=T(1)%getTotalData()
		do i=1,length
			call outT%setValue(i1,i2,T(i))
			i1=i2+1
			if(i.lt.length)i2=i2+T(i+1)%gettotalData()
		end do
		return
	end subroutine






!**********************************************************************
!**********************************************************************
!	the code below is for MPI
!**********************************************************************
	subroutine sent_Tensor(Ten1,Ten2,ID1,ID2,ierr,MPIcommon)
		type(Tensor),intent(in)::Ten1
		type(Tensor),intent(inout)::Ten2
		integer,intent(in)::ID1,ID2
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,len2,istatus(MPI_STATUS_SIZE),mpi_comm
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
!***************************data*********************************************		
		call sent_TData(Ten1%TData,Ten2%TData,ID1,ID2,ierr,MPIcommon)
		if((proID.eq.ID1).and.(.not.Ten1%getflag()))return
		if((proID.eq.ID2).and.(.not.Ten2%getflag()))then
			call Ten2%empty()
			return
		end if
!*************************rank***************************************************			
		if(proID.eq.ID1) then
			call mpi_send(Ten1%rank,1,MPI_integer,ID2,tag,MPI_Comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Ten2%rank,1,MPI_integer,ID1,tag,MPI_Comm,istatus,ierr)
		end if
!************************Dimension************************************************		
		call sent_Dimension(Ten1%TenDim,Ten2%TenDim,ID1,ID2,ierr,MPIcommon)
		return
	end subroutine


	subroutine BCAST_Tensor(Ten1,ID,ierr,MPIcommon)
		type(Tensor),intent(inout)::Ten1
		integer,intent(in)::ID
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,len2,istatus(MPI_STATUS_SIZE),mpi_comm
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
!***************************data*********************************************				
		call BCAST_TData(Ten1%TData,ID,ierr,MPIcommon)
		if(.not.Ten1%getflag()) then
			call Ten1%empty()
			return
		end if
!***************************rank**************************************************		
		call MPI_BCAST(Ten1%rank,1,MPI_integer,ID,mpi_comm,ierr)	
!**************************Dimension***********************************************		
			call BCAST_Dimension(Ten1%TenDim,ID,ierr,MPIcommon)
!**********************************************************************************	
		return
	end subroutine



	subroutine sent_Tensorlink(link1,link2,ID1,ID2,ierr,MPIcommon)
		type(Tensorlink),intent(in)::link1
		type(Tensorlink),intent(inout)::link2
		integer,intent(in)::ID1,ID2
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,linklen,indeLen,i,istatus(MPI_STATUS_SIZE),mpi_comm
		type(Tensor)::Ten
		integer,allocatable:: indice(:)
		logical::is_index
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		tag=1
		linklen=0
		call mpi_comm_rank(mpi_comm,proID,ierr)
		call mpi_comm_size(mpi_comm,proNum,ierr )
		if(proNum.eq.1)return
		if(present(MPIcommon))then
			if((ID1.ge.proNum).or.(ID2.ge.proNum))return
		end if
		
		if(ID1.eq.ID2) return !The some cpu, do nothing
		
		if((proID.ne.ID1).and.(proID.ne.ID2)) return!The proID do not sent or recv, return
		
		if(proID.eq.ID1) then
			linklen=linklength(link1)
			call mpi_send(linklen,1,MPI_integer,ID2,tag,mpi_comm,ierr)
			if(linklen.eq.0) then
				return
			end if
		end if
		if(proID.eq.ID2) then
			call mpi_recv(linklen,1,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
			if(linklen.eq.0) then
				call cleanlink(link2)
				return
			end if
			call cleanlink(link2)
		end if
!**********************************************************************************
		do i=1,linklen
			if(proID.eq.ID1) then
				is_index=TenIndiceLog(link1,i,indice)
				call mpi_send(is_index,1,MPI_logical,ID2,tag,mpi_comm,ierr)
			end if	
			if(proID.eq.ID2) then
				call mpi_recv(is_index,1,MPI_logical,ID1,tag,mpi_comm,istatus,ierr)
			end if
			if(is_index) then
				if(proID.eq.ID1) then
					indeLen=size(indice)
					call mpi_send(indeLen,1,MPI_integer,ID2,tag,mpi_comm,ierr)
				end if
				if(proID.eq.ID2) then
					call mpi_recv(indeLen,1,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
				end if
				if(proID.eq.ID1) then
					call mpi_send(indice,indeLen,MPI_integer,ID2,tag,mpi_comm,ierr)
				end if
				if(proID.eq.ID2) then
					if(allocated(indice))then
						if(indeLen.ne.size(indice))then
							deallocate(indice)
							allocate(indice(indeLen))
						end if
					else
						allocate(indice(indeLen))
					end if
					call mpi_recv(indice,indeLen,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
				end if
				if(proID.eq.ID1) then
					Ten=link1.i.i
				end if
				call sent_Tensor(Ten,Ten,ID1,ID2,ierr)
				if(proID.eq.ID2) then
					call push_back(link2,Ten,indice)
				end if
			else
				if(proID.eq.ID1) then
					Ten=link1.i.i
				end if
				call sent_Tensor(Ten,Ten,ID1,ID2,ierr)
				if(proID.eq.ID2) then
					call push_back(link2,Ten)
				end if
			end if
		end do
		return
	end subroutine

	subroutine BCAST_Tensorlink(link1,ID,ierr,MPIcommon)
		type(Tensorlink),intent(inout)::link1
		integer,intent(in)::ID
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,linklen,indeLen,i,istatus(MPI_STATUS_SIZE),mpi_comm
		type(Tensor)::Ten
		integer,allocatable:: indice(:)
		logical::is_index
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
		
		if(proID.eq.ID) then
			linklen=linklength(link1)
		else
			call cleanlink(link1)
		end if
		call MPI_BCAST(linklen,1,MPI_integer,ID,mpi_comm,ierr)
		if(linklen.eq.0) then
			call cleanlink(link1)
			return
		end if
!**********************************************************************************
		do i=1,linklen
			if(proID.eq.ID) then
				is_index=TenIndiceLog(link1,i,indice)
			end if
			call MPI_BCAST(is_index,1,MPI_logical,ID,mpi_comm,ierr)
			
			if(is_index) then
			
				if(proID.eq.ID) then
					indeLen=size(indice)
				end if
				call MPI_BCAST(indeLen,1,MPI_integer,ID,mpi_comm,ierr)
				if(proID.ne.ID) then
					if(allocated(indice))then
						if(indeLen.ne.size(indice))then
							deallocate(indice)
							allocate(indice(indeLen))
						end if
					else
						allocate(indice(indeLen))
					end if
				end if
				call MPI_BCAST(indice,indeLen,MPI_integer,ID,mpi_comm,ierr)
				if(proID.eq.ID) then
					Ten=link1.i.i
				end if
				call BCAST_Tensor(Ten,ID,ierr)
				if(proID.ne.ID) then
					call push_back(link1,Ten,indice)
				end if
				
			else
				if(proID.eq.ID) then
					Ten=link1.i.i
				end if
				call BCAST_Tensor(Ten,ID,ierr)
				if(proID.ne.ID) then
					call push_back(link1,Ten)
				end if
			end if
			
		end do
		return
	end subroutine

	subroutine MPI_SUM_Tensor1(inTensor,outTensor,ierr,MPIcommon)
		type(Tensor),intent(inout)::outTensor
		type(Tensor)::inTensor
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::classtype,proID,proNum,mpi_comm,i,tag,istatus(MPI_STATUS_SIZE)
		logical::goonFlag,ALLgoonFlag,Allempty
		character*20::typechar
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		classtype=inTensor%getType()
		call MPI_BCAST(classtype,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=.true.
		if(inTensor%getType().ne.classtype)goonFlag=.false.
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call mpi_comm_rank(mpi_comm,proID,ierr)
			call mpi_comm_size(mpi_comm,proNum,ierr )
			call writemess('The data type is not the same in every cpu when calling MPI_SUM_Tensor',-1)
			tag=1
			typechar=inTensor%getclassType()
			call writemess('The data type in CPU'+proID+'is classtype='+typechar,-1)
			do i=1,proNum-1
				if(proID.eq.i)call mpi_send(typechar,20,MPI_character,0,tag,mpi_comm,ierr)
				if(proID.eq.0)call mpi_recv(typechar,20,MPI_character,i,tag,mpi_comm,istatus,ierr)
				call writemess('The data type in CPU'+i+'is classtype='+typechar,-1)
			end do
			call error_stop()
		end if
		goonFlag=inTensor%getFlag()
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			if(inTensor%getFlag())then
				goonFlag=.false.
			else
				goonFlag=.true.
			end if
			call MPI_ALLREDUCE(goonFlag,Allempty,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
			if(Allempty)then
				call outTensor%empty()
				return
			end if
			call writemess('There are empty Tensor in some cpu, MPI_SUM_Tensor')
			call error_stop
		end if
		if(classtype.gt.5)then
			call writemess('The data type in Tensor can not be sum,the data type is classType='+inTensor%getclassType(),-1)
			call error_stop()
		end if
		call outTensor%empty()
		call outTensor%allocate(inTensor)
		call MPI_SUM_TData(inTensor%TData,outTensor%TData,ierr,MPIcommon)
		return
	end subroutine
	subroutine MPI_SUM_Tensor2(inoutTensor,ierr,MPIcommon)
		type(Tensor),intent(inout)::inoutTensor
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		type(Tensor)::temp
		integer::classtype,proID,proNum,mpi_comm,i,tag,istatus(MPI_STATUS_SIZE)
		logical::goonFlag,ALLgoonFlag,Allempty
		character*20::typechar
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		classtype=inoutTensor%getType()
		call MPI_BCAST(classtype,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=.true.
		if(inoutTensor%getType().ne.classtype)goonFlag=.false.
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call mpi_comm_rank(mpi_comm,proID,ierr)
			call mpi_comm_size(mpi_comm,proNum,ierr )
			call writemess('The data type is not the same in every cpu when calling MPI_SUM_Tensor',-1)
			tag=1
			typechar=inoutTensor%getclassType()
			call writemess('The data type in CPU'+proID+'is classtype='+typechar,-1)
			do i=1,proNum-1
				if(proID.eq.i)call mpi_send(typechar,20,MPI_character,0,tag,mpi_comm,ierr)
				if(proID.eq.0)call mpi_recv(typechar,20,MPI_character,i,tag,mpi_comm,istatus,ierr)
				call writemess('The data type in CPU'+i+'is classtype='+typechar,-1)
			end do
			call error_stop()
		end if
		
		goonFlag=inoutTensor%getFlag()
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			if(inoutTensor%getFlag())then
				goonFlag=.false.
			else
				goonFlag=.true.
			end if
			call MPI_ALLREDUCE(goonFlag,Allempty,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
			if(Allempty)then
				call inoutTensor%empty()
				return
			end if
			call writemess('There are empty Tensor in some cpu, MPI_SUM_Tensor')
			call error_stop
		end if
		
		if(classtype.gt.5)then
			call writemess('The data type in Tensor can not be sum,the data type is classType='+inoutTensor%getclassType(),-1)
			call error_stop()
		end if
		temp=inoutTensor
		call MPI_SUM_TData(temp%TData,inoutTensor%TData,ierr,MPIcommon)
		return
	end subroutine
	subroutine MPI_MAX_Tensor1(inTensor,outTensor,ierr,MPIcommon)
		type(Tensor),intent(inout)::outTensor
		type(Tensor)::inTensor
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::classtype,proID,proNum,mpi_comm,i,tag,istatus(MPI_STATUS_SIZE)
		logical::goonFlag,ALLgoonFlag
		character*20::typechar
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		classtype=inTensor%getType()
		call MPI_BCAST(classtype,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=.true.
		if(inTensor%getType().ne.classtype)goonFlag=.false.
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call mpi_comm_rank(mpi_comm,proID,ierr)
			call mpi_comm_size(mpi_comm,proNum,ierr )
			call writemess('The data type is not the same in every cpu when calling MPI_MAX_Tensor',-1)
			tag=1
			typechar=inTensor%getclassType()
			call writemess('The data type in CPU'+proID+'is classtype='+typechar,-1)
			do i=1,proNum-1
				if(proID.eq.i)call mpi_send(typechar,20,MPI_character,0,tag,mpi_comm,ierr)
				if(proID.eq.0)call mpi_recv(typechar,20,MPI_character,i,tag,mpi_comm,istatus,ierr)
				call writemess('The data type in CPU'+i+'is classtype='+typechar,-1)
			end do
			call error_stop()
		end if
		goonFlag=inTensor%getFlag()
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('There are empty Tensor in some cpu, MPI_MAX_Tensor')
			call error_stop
		end if
		if(classtype.ge.4)then
			call writemess('The data type in Tensor can not Find MAX,the data type is classType='+inTensor%getclassType(),-1)
			call error_stop()
		end if
		call outTensor%empty()
		call outTensor%allocate(inTensor)
		call MPI_MAX_TData(inTensor%TData,outTensor%TData,ierr,MPIcommon)
		return
	end subroutine
	subroutine MPI_MAX_Tensor2(inoutTensor,ierr,MPIcommon)
		type(Tensor),intent(inout)::inoutTensor
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		type(Tensor)::temp
		integer::classtype,proID,proNum,mpi_comm,i,tag,istatus(MPI_STATUS_SIZE)
		logical::goonFlag,ALLgoonFlag
		character*20::typechar
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		classtype=inoutTensor%getType()
		call MPI_BCAST(classtype,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=.true.
		if(inoutTensor%getType().ne.classtype)goonFlag=.false.
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call mpi_comm_rank(mpi_comm,proID,ierr)
			call mpi_comm_size(mpi_comm,proNum,ierr )
			call writemess('The data type is not the same in every cpu when calling MPI_MAX_Tensor',-1)
			tag=1
			typechar=inoutTensor%getclassType()
			call writemess('The data type in CPU'+proID+'is classtype='+typechar,-1)
			do i=1,proNum-1
				if(proID.eq.i)call mpi_send(typechar,20,MPI_character,0,tag,mpi_comm,ierr)
				if(proID.eq.0)call mpi_recv(typechar,20,MPI_character,i,tag,mpi_comm,istatus,ierr)
				call writemess('The data type in CPU'+i+'is classtype='+typechar,-1)
			end do
			call error_stop()
		end if
		
		goonFlag=inoutTensor%getFlag()
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('There are empty Tensor in some cpu, MPI_MAX_Tensor')
			call error_stop
		end if
		
		if(classtype.ge.4)then
			call writemess('The data type in Tensor can not Find MAX,the data type is classType='+inoutTensor%getclassType(),-1)
			call error_stop()
		end if
		temp=inoutTensor
		call MPI_MAX_TData(temp%TData,inoutTensor%TData,ierr,MPIcommon)
		return
	end subroutine
	subroutine MPI_MIN_Tensor1(inTensor,outTensor,ierr,MPIcommon)
		type(Tensor),intent(inout)::outTensor
		type(Tensor)::inTensor
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::classtype,proID,proNum,mpi_comm,i,tag,istatus(MPI_STATUS_SIZE)
		logical::goonFlag,ALLgoonFlag
		character*20::typechar
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		classtype=inTensor%getType()
		call MPI_BCAST(classtype,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=.true.
		if(inTensor%getType().ne.classtype)goonFlag=.false.
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call mpi_comm_rank(mpi_comm,proID,ierr)
			call mpi_comm_size(mpi_comm,proNum,ierr )
			call writemess('The data type is not the same in every cpu when calling MPI_MIN_Tensor',-1)
			tag=1
			typechar=inTensor%getclassType()
			call writemess('The data type in CPU'+proID+'is classtype='+typechar,-1)
			do i=1,proNum-1
				if(proID.eq.i)call mpi_send(typechar,20,MPI_character,0,tag,mpi_comm,ierr)
				if(proID.eq.0)call mpi_recv(typechar,20,MPI_character,i,tag,mpi_comm,istatus,ierr)
				call writemess('The data type in CPU'+i+'is classtype='+typechar,-1)
			end do
			call error_stop()
		end if
		goonFlag=inTensor%getFlag()
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('There are empty Tensor in some cpu, MPI_MIN_Tensor')
			call error_stop
		end if
		
		if(classtype.ge.4)then
			call writemess('The data type in Tensor can not Find MIN,the data type is classType='+inTensor%getclassType(),-1)
			call error_stop()
		end if
		call outTensor%empty()
		call outTensor%allocate(inTensor)
		call MPI_MIN_TData(inTensor%TData,outTensor%TData,ierr,MPIcommon)
		return
	end subroutine
	subroutine MPI_MIN_Tensor2(inoutTensor,ierr,MPIcommon)
		type(Tensor),intent(inout)::inoutTensor
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		type(Tensor)::temp
		integer::classtype,proID,proNum,mpi_comm,i,tag,istatus(MPI_STATUS_SIZE)
		logical::goonFlag,ALLgoonFlag
		character*20::typechar
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		classtype=inoutTensor%getType()
		call MPI_BCAST(classtype,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=.true.
		if(inoutTensor%getType().ne.classtype)goonFlag=.false.
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call mpi_comm_rank(mpi_comm,proID,ierr)
			call mpi_comm_size(mpi_comm,proNum,ierr )
			call writemess('The data type is not the same in every cpu when calling MPI_MIN_Tensor',-1)
			tag=1
			typechar=inoutTensor%getclassType()
			call writemess('The data type in CPU'+proID+'is classtype='+typechar,-1)
			do i=1,proNum-1
				if(proID.eq.i)call mpi_send(typechar,20,MPI_character,0,tag,mpi_comm,ierr)
				if(proID.eq.0)call mpi_recv(typechar,20,MPI_character,i,tag,mpi_comm,istatus,ierr)
				call writemess('The data type in CPU'+i+'is classtype='+typechar,-1)
			end do
			call error_stop()
		end if
		goonFlag=inoutTensor%getFlag()
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('There are empty Tensor in some cpu, MPI_MIN_Tensor')
			call error_stop
		end if
		if(classtype.ge.4)then
			call writemess('The data type in Tensor can not Find MIN,the data type is classType='+inoutTensor%getclassType(),-1)
			call error_stop()
		end if
		temp=inoutTensor
		call MPI_MIN_TData(temp%TData,inoutTensor%TData,ierr,MPIcommon)
		return
	end subroutine
















end module



module extends_memory_type
	use Tools
	use memory_type
	use Tensor_Type
	use Dimension_Typede
	implicit none
	private
	public::extendsMemory
	type,extends (memory) :: extendsMemory
		type(Tensor),pointer::Twork(:)
		type(Dimension),pointer::Dimwork(:)
		integer::TLength=0
		integer::Tith=0
		integer::DimLength=0
		integer::Dimith=0
	contains
		procedure::allocate_memory1,allocate_memory2
		procedure::deallocate_memory_All
		procedure,public::deallocate =>deallocate_memory_All
		procedure,public::free=>free_memory_All
		generic,public::get_memory=> Tget_memory,Dimget_memory,Tget_memory0,Dimget_memory0
		procedure::Tget_memory,Dimget_memory,Tget_memory0,Dimget_memory0
		procedure,public::print=>print_info
		procedure,public::getLength
	end type
contains
	integer function select_data_type_char(indata)result(select_data_type)
		character(len=*),intent(in) ::indata
		if(indata.equ.'Dimension') then
			select_data_type=8
			return
		end if
		if(indata.equ.'Tensor') then
			select_data_type=9
			return
		end if
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
		write(*,*)"ERROR type"
		call error_stop()
		return
	end function
	subroutine allocate_memory1(mem,dataType,length,w)
		class(extendsMemory),intent(inout)::mem
		character(len=*),intent(in)::dataType
		character(len=*),intent(in),optional::w
		integer,intent(in)::length
		integer::iType
		iType=select_data_type_char(dataType)
		select case(iType)
			case (7)
				if(mem%DimLength.lt.length)then
					if(mem%Dimith.eq.0)then
						if(associated(mem%Dimwork)) deallocate(mem%Dimwork)
						allocate(mem%Dimwork(length))
						mem%DimLength=length
					else
						call writemess('Can not reallocate extendsMemory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (8)
				if(mem%TLength.lt.length)then
					if(mem%Tith.eq.0)then
						if(associated(mem%Twork)) deallocate(mem%Twork)
						allocate(mem%Twork(length))
						mem%TLength=length
					else
						call writemess('Can not reallocate extendsMemory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case default
				call mem%memory%allocate(dataType,length,w)
		end select
		return
	end subroutine
	subroutine allocate_memory2(mem,iType,length,w)
		class(extendsMemory),intent(inout)::mem
		integer,intent(in)::length,iType
		character(len=*),intent(in),optional::w
		select case(iType)
			case (7)
				if(mem%DimLength.lt.length)then
					if(mem%Dimith.eq.0)then
						if(associated(mem%Dimwork)) deallocate(mem%Dimwork)
						allocate(mem%Dimwork(length))
						mem%DimLength=length
					else
						call writemess('Can not reallocate extendsMemory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (8)
				if(mem%TLength.lt.length)then
					if(mem%Tith.eq.0)then
						if(associated(mem%Twork)) deallocate(mem%Twork)
						allocate(mem%Twork(length))
						mem%TLength=length
					else
						call writemess('Can not reallocate extendsMemory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case default
				call mem%memory%allocate(iType,length,w)
		end select
		return
	end subroutine
	
	subroutine deallocate_memory_All(mem)
		class(extendsMemory),intent(inout)::mem
		if(associated(mem%Twork))deallocate(mem%Twork)	
		if(associated(mem%Dimwork))deallocate(mem%Dimwork)	
		mem%Tith=0
		mem%Dimith=0
		mem%TLength=0
		mem%DimLength=0
		call mem%memory%deallocate()
		return
	end subroutine
	subroutine free_memory_All(mem)
		class(extendsMemory),intent(inout)::mem
		if(deallocate_memory_flag)then
			call deallocate_memory_All(mem)
			return
		end if
		mem%Tith=0
		mem%Dimith=0
		call mem%memory%free()
		return
	end subroutine
	
	subroutine Tget_memory(mem,p,length,w)
		class(extendsMemory),target,intent(inout)::mem
		type(Tensor),intent(inout),pointer::P(:)
		integer,intent(in)::length
		character(len=*),intent(in),optional::w
		integer::ith
		mem%Flag=.true.
		ith=mem%Tith+length
		if(ith.gt.mem%TLength)then
			if(mem%DynamicLength)then
				call mem%allocate(9,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for Tensor is length='+mem%iLength)
				call error_stop
			end if
		end if
		p=>mem%Twork(mem%Tith+1:ith)
		mem%Tith=ith
		return
	end subroutine
	subroutine Tget_memory0(mem,p,w)
		class(extendsMemory),target,intent(inout)::mem
		type(Tensor),intent(inout),pointer::P
		character(len=*),intent(in),optional::w
		integer::ith,length
		length=1
		mem%Flag=.true.
		ith=mem%Tith+length
		if(ith.gt.mem%TLength)then
			if(mem%DynamicLength)then
				call mem%allocate(9,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for Tensor is length='+mem%iLength)
				call error_stop
			end if
		end if
		p=>mem%Twork(ith)
		mem%Tith=ith
		return
	end subroutine
	subroutine Dimget_memory(mem,p,length,w)
		class(extendsMemory),target,intent(inout)::mem
		type(Dimension),intent(inout),pointer::P(:)
		integer,intent(in)::length
		character(len=*),intent(in),optional::w
		integer::ith
		mem%Flag=.true.
		ith=mem%Dimith+length
		if(ith.gt.mem%DimLength)then
			if(mem%DynamicLength)then
				call mem%allocate(8,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for Dimension is length='+mem%iLength)
				call error_stop
			end if
		end if
		p=>mem%Dimwork(mem%Dimith+1:ith)
		mem%Dimith=ith
		return
	end subroutine
	subroutine Dimget_memory0(mem,p,w)
		class(extendsMemory),target,intent(inout)::mem
		type(Dimension),intent(inout),pointer::P
		character(len=*),intent(in),optional::w
		integer::ith,length
		length=1
		mem%Flag=.true.
		ith=mem%Dimith+length
		if(ith.gt.mem%DimLength)then
			if(mem%DynamicLength)then
				call mem%allocate(8,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for Dimension is length='+mem%iLength)
				call error_stop
			end if
		end if
		p=>mem%Dimwork(ith)
		mem%Dimith=ith
		return
	end subroutine
	subroutine print_info(mem)
		class(extendsMemory),intent(inout)::mem
		call writemess('The length of the memory are')
		call writemess('integer         :'+mem%iLength)
		call writemess('real(kind=4)    :'+mem%sLength)
		call writemess('real(kind=8)    :'+mem%dLength)
		call writemess('complex(kind=4) :'+mem%cLength)
		call writemess('complex(kind=8) :'+mem%zLength)
		call writemess('logical         :'+mem%lLength)
		call writemess('character       :'+mem%aLength)
		call writemess('Dimension       :'+mem%DimLength)
		call writemess('Tensor          :'+mem%TLength)
		return
	end subroutine
	subroutine getLength(mem,inoutlen)
		class(extendsMemory),intent(inout)::mem
		integer,intent(inout)::inoutlen(:)
		if(size(inoutlen).lt.9)then
			call writemess('ERROR in get length of the extendsMemory')
			call error_stop
		end if
		inoutlen(1)=mem%iLength
		inoutlen(2)=mem%sLength
		inoutlen(3)=mem%dLength
		inoutlen(4)=mem%cLength
		inoutlen(5)=mem%zLength
		inoutlen(6)=mem%lLength
		inoutlen(7)=mem%aLength
		inoutlen(8)=mem%DimLength
		inoutlen(9)=mem%TLength
		return
	end subroutine
end module












