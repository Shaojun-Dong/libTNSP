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
!************************************************************
!************* START OF Dimension *******************
!************************************************************

module Dimension_typede
	use Tools
	use mpi
	use memory_type
	implicit none
	private
	type(memory),private::WorkingMemory
	type DimensionName
		CHARACTER(len=len_of_Name)::TensorName
		CHARACTER(len=len_of_Name)::dimenName
	end type DimensionName
	type DimensionIntName
		integer::TensorName(len_of_intName_in_type_define)=0
		integer::dimenName(len_of_intName_in_type_define)=0
	end type DimensionIntName
	public::Dimension
	type Dimension
		integer,private :: boundarysize=0!The length of the boundary that is useful
		integer,private :: lenDimData=0!The length of the DimData that is useful,If it is 0, means no data, use it to tell if allocate dimension
		integer,private :: Dimsize=0!The Rank of The Tensor
		integer,allocatable,private :: boundary(:)!only boundary(1:boundarysize) are the usefull data
		integer,allocatable,private :: DimData(:)!only DimData(1:lenDimData) are the usefull data
		type(dimensionName),allocatable,private::DimName(:)!only DimName(1:lenDimData) are the usefull data
		type(DimensionIntName),allocatable,private::DimIntName(:)!only DimIntName(1:lenDimData) are the usefull data
		integer,private::nameflag=0!=0  means no name
		                           !=1  means there are names that is CHARACTER(DimName)
		                           !=2  means there are names that is integer(DimintName)
		logical,private::sample_dimension_flag=.true.!if true, only store DimData, DimName, DimIntName, nameflag and lenDimData
		                                             !When do not do the DimConstract, DimDecompose and
		                                             !no name, it gose faster.
	contains 
		generic,public::Nameorder => Nameorder2,Nameorder3,Nameorder4,Nameorder5,Nameorder6,&
											NameorderArray,intNameorderArray1,intNameorderArray2
		generic,public::FindOrder=> Nameorder2_check,Nameorder3_check,Nameorder4_check,Nameorder5_check,&
											NameorderArray_check
											
		generic,public::setName => setDimName0,integerDimName,setDimName1,setDimName2,setDimName3,setDimName4,setDimName5,setDimName6
		procedure,public::empty => emptyDimension
		procedure,public::deallocate=>cleanDimension
		procedure,public::cleanName=>cleanDimensionName
		generic,public::outTensorName=>outNameTen,outAllTensorName,outNameTenchar
		procedure,public::outSomedimensionName=>outAllNameTenChar
		procedure,public::outTensorIntName=>outIntNameTen
		procedure,public::outAllIntNameDim
		generic,public::outName=>outName1,outNameAll
		generic,public::getName=>outName1,outNameAll,outNameTenchar
		procedure,public::outDimName
		generic,public::dim=>Dim_i,DimName_i,AllDim
		procedure,public::outIntNameDim
		procedure,public::outDimintName
		procedure,public::RNDim=>RNDimRoutine
		generic,public::killLeg=>RNDimRoutine,RNDimRoutineint,RNDimRoutinechar
		procedure::RNDimRoutine,RNDimRoutineint,RNDimRoutinechar
		procedure,public::outlenDimData
		procedure,public::outDimData!output dimension%DimData
		procedure,public::print=>Dprint
		procedure,public::info=>Dprint2
		procedure,public::read=>readdimension
		procedure,public::getRank =>Dimsize!return the Dimsize of type(Dimension)
		procedure,public::size=>outtotalData!product(DimData)
		procedure,public::fuse=>fuseDimension
		procedure,public::split =>splitDimension
		generic,public::resetDim =>resetDimension
		procedure,public::fuseIndex=>DimConstract!no longer use
		generic,public::splitIndex=>DimDecompose,DimDecomposeAll!no longer use
		procedure,public::Dimpermute
		procedure,public::out_sample_flag=>out_simple_dimension_flag
		procedure,public::if_simple_dimension=>out_simple_dimension_flag
		procedure,public::getSubDimboundary
		procedure,public::getSubDimlen
		procedure,public::if_original_dim
		procedure,public::outNameFlag!=0  means no name
		                             !=1  means there are names that is CHARACTER(DimName)
		                             !=2  means there are names that is integer(DimintName)
		procedure,public::getNameFlag=>outNameFlag                            
     	procedure,public::Same_Name_Order_forwards
      	procedure,public::Same_Name_Order_backwards
      	procedure,public::ifName
		procedure::Dim_i
		procedure::outNameTen
		procedure::outAllTensorName
		procedure::outNameTenchar
		procedure::DimName_i
		procedure::AllDim                            
		procedure::resetDimension                             
		procedure::outName1
		procedure::outNameAll
		procedure::Nameorder2
		procedure::Nameorder3
		procedure::Nameorder4
		procedure::Nameorder5
		procedure::Nameorder6
		procedure::NameorderArray
		procedure::intNameorderArray1
		procedure::intNameorderArray2
		procedure::setDimName0
		procedure::integerDimName
		procedure::setDimName1
		procedure::setDimName2
		procedure::setDimName3
		procedure::setDimName4
		procedure::setDimName5
		procedure::setDimName6
		procedure::DimDecompose
		procedure::DimDecomposeAll
		procedure::Nameorder2_check,Nameorder3_check,Nameorder4_check,Nameorder5_check,&
											NameorderArray_check
	end type Dimension
	
	interface Nameinit
		module procedure Nameinit1
		module procedure Nameinit2
		module procedure intNameinit1
		module procedure intNameinit2
		module procedure intNameinit3
		module procedure intNameinit4
	end interface	
	
	!find the index,whose name is w	,output the order of it in the dimension
	!If can not find , output 0
	interface Nameorder
		module procedure Nameorder2!(dimen,character)
		module procedure Nameorder3!(dimen,name)
		module procedure Nameorder4
		module procedure Nameorder5
		module procedure Nameorder6
		module procedure NameorderArray!(dimen,character(:)),output a integer(:)
		module procedure intNameorderArray1!(dimen,integer(:),integer(:)),output a integer(:)
		module procedure intNameorderArray2!(dimen,DimensionIntName(:)),output a integer(:)
	end interface
	
!Find all the DimensionNames, whose indexname is oldname
!change them to newname
!If Cannot find it , do nothing
!If input 'A' 'B', all name such as 'A.1','A.2','A.as' will change to 'B.1','B.2','B.as'
!if input 'A.1' 'B.1',the name 'A.1' will change to 'B.1'
	interface setDimName
		module procedure setDimName0!(dimen,w),set a name to the dimenison
		module procedure integerDimName!(dimen,int(:)),set a name to the dimenison
		module procedure setDimName1
		module procedure setDimName2
		module procedure setDimName3
		module procedure setDimName4
		module procedure setDimName5
		module procedure setDimName6
	end interface
	public::assignment(=)
	interface assignment(=)
		module procedure DimInitialization!dimension=vector
		module procedure DimInitialization2
		module procedure copyDimToVec
		module procedure copyName!name1=name2
		module procedure copyintName!intname1=intname2
		module procedure copyNameArray!name1(:)=name2(:),willnot allocate name1
		module procedure copyintNameArray!intname1(:)=intname2(:),willnot allocate intname1
		module procedure charName!name=character
		module procedure Namechar!character=name
		module procedure intNamechar!character=intname
		module procedure charNameArray!name(:)=character(:)
		module procedure NamecharArray!character(:)=name(:)
		module procedure intNamecharArray!character(:)=intname(:)
	end interface
	
	public::operator(.fuse.)
	interface operator(.fuse.)
		module procedure fuseDimension_val
		module procedure fuseDimension_vec
	end interface
	
	public::operator(.split.)
	interface operator(.split.)
		module procedure splitDimension2
		module procedure splitDimension3
		module procedure splitDimensionAll
	end interface
	
	public::operator(+)
	interface operator(+)
		module procedure Dimadd
		module procedure DimAdd2
		module procedure DimAdd3
	end interface
	
	public::operator(.sub.)
	interface operator(.sub.)
		module procedure getSubDim2
		module procedure getSubDim3
		module procedure getSubDim2_name
		module procedure getSubDim2_intname2
	end interface
	public::operator(.subdim.)!The same the .sub.
	interface operator(.subdim.)
		module procedure getSubDim2
		module procedure getSubDim3
		module procedure getSubDim2_name
		module procedure getSubDim2_intname2
	end interface
	!getSubDim_intname(dimen,TensorName(:),DimenName(:))
	!!	return the inde  dimension	,outpout in a vector of the dimenison    
! If do the contract, onedimenison will have more than one value
! [2,3,4,5]	-->contract the 2,3 index -->[2,(3,4),5]!the dimension is 2,12,5
! then getSubDim(Dimen,2,dimenVec)==>dimenVec=[3,4]
!	subroutine getSubDim(Dimen,inde,dimenVec)
	public::operator(.i.)
	interface operator(.i.)
		module procedure Dim_i
		module procedure DimName_i
	end interface
	
	public::operator(.dim.)!The same the .i.
	interface operator(.dim.)
		module procedure Dim_i
		module procedure outDiemsnion
	end interface
	
	public:: operator(.iname.)
	interface operator(.iname.)!the index name
		module procedure outName1
		module procedure outNameAll
	end interface
	
	public::operator(.iiname.)
	interface operator(.iiname.)!the index name
		module procedure outIntNameDim
		module procedure outAllIntNameDim
	end interface
	public::operator(.Tname.)
	interface operator(.Tname.)!the Tensor name
		module procedure outNameTen
		module procedure outAllTensorName
	end interface
	
	public::operator(.equ.)
	interface operator(.equ.)
		module procedure equal_of_dim
		module procedure  equal_name1
		module procedure  equal_name2
		module procedure  equal_name3
		module procedure  equal_intname
	end interface
	
	public::MPI_send_Dimension
	interface MPI_send_Dimension
		module procedure  sent_Dimension
	end interface
	
	public::MPI_BCAST_Dimension
	interface MPI_BCAST_Dimension
		module procedure  BCAST_Dimension
	end interface
	
	interface allocateCheckName
		module procedure  allocateCheckName1
		module procedure  allocateCheckNameint
	end interface
!**********************************************************
!	Other Function or Subroutine:
!
!		set_sample_dimension_flag(dimen)
!
!		set_len_of_intName
!
		public::outTenNameLen!When use integer as the name of the Tensor, one use this function to get the length of Tensor
!
		public::outDimNameLen!When use integer as the name of the Tensor, one use this function to get the length of dimension
!
!		set_sample_dimension(dimen)
!
!		chartoName_log:input a character,output the two element of the 
!					type(DimensionName),example:input asd.ad output 'asd' and
!					 'ad', chartoName_log=.true.  ;  input asd output 'asd' ,
!					 '0'  and chartoName_log=.false.
!

		public::long_Name_logi
!		copyNameAllocate(outName,inName):outName(:)=inName(:),will allocate outName
!
!		copyintNameAllocate(outName,inName):outName(:)=inName(:),will allocate outName
!
!		printName:print the name in the dimension
!
!		Dimensioniniti_name(boundarysize,Dimsize,boundary,DimData,DimName): initialation a dimension with a name
!
!
!
!		outDimintName:output the Name of the ith dimension,output as integer(2,:)
!
!		getSubDimName:get the inde Name in the dimension,ouput in type(DimensionName)
!
!		getSubDimIntName:get the inde Name in the dimension,ouput in type(DimensionIntName)
!
!
		public::copydimension!:copy dimension to a array,it will allocate array
!
!		print the type(dimension):
!			Dprint
!			Dprint0
!			Dprint2
!
!
!		getSubDim_intname(dimen,TensorName(:),DimenName(:))
!			
!
!
!		Dimpermute:permutation of dimension	
!
		public::if_original_dim
!
!
		public::dimpermute_forwards,dimpermute_backwards,dimpermute_forwards_index,dimpermute_backwards_index
!
!
!		MPI function:
		public::sent_Dimension,BCAST_Dimension
!			sent_Dimension(dim1,dim2,ID1,ID2,ierr):send the data of dim1 in ID1 to dim2 in ID2
!			BCAST_Dimension(dim1,ID,ierr):BCAST The dim1 in ID to every cpus
!
!			example
!				integer::ierr,proID,proNum
!				type(Dimension)::T1,T2
!				call mpi_init(ierr)
!				call mpi_comm_rank(mpi_comm_world,proID,ierr)
!				call mpi_comm_size(mpi_comm_world,proNum,ierr ) 
!				call sent_Dimension(T1,T2,0,1,ierr)  !T1 in cpu0 sent to cpu2 and store in T2
!				call BCAST_Dimension(T1,0,ierr) 		 !T1 in cpu 0 send to every cpus,store in T1 in other cpus
!
!**********************************************************	
	public::writemess
	interface writemess
		module procedure writemess_dimension
	end interface
	
	
	public::getSubDim3_index_routine
	public::Dimension_memory_report,Dimension_memory_length,dellocate_Dimension_memory
contains

	subroutine dellocate_Dimension_memory()
		call WorkingMemory%deallocate()
		return
	end subroutine
	
	subroutine Dimension_memory_report()
		call writemess('The memory used in Dimension are:')
		call WorkingMemory%print()
		call writemess(' ')
	end subroutine	
	subroutine Dimension_memory_length(length)
		integer,intent(inout)::length(:)
		call WorkingMemory%getlength(length)
	end subroutine	
	
	subroutine writemess_dimension(Dimen,cpu_number)!overwrite writemess
		type(dimension),intent(in)::Dimen
		integer,optional,intent(in)::cpu_number
		character(len=max_len_of_char)::w
		CHARACTER*5000,allocatable::ws(:)
		integer,allocatable :: dimenVec(:)
		integer::i,maxdim,totoal
		CHARACTER*100::wlen,w2
		totoal=Dimen%LenDimData
		if(totoal.eq.0)then
			call writemess('There is no data in the dimension',cpu_number)
			return
		end if
		if(Dimen%sample_dimension_flag)then
			w='dimension:('
			do i=1,Dimen%LenDimData-1
				w=w+Dimen%DimData(i)+','
			end do
			w=w+Dimen%DimData(Dimen%LenDimData)+')'
			call writemess(w,cpu_number)
		else
			call copydimension(dimenVec,Dimen)
			w='dimension:('
			do i=1,size(dimenVec)-1
				w=w+dimenVec(i)+','
			end do
			w=w+dimenVec(size(dimenVec))+'),It is not original dimension'
			call writemess(w,cpu_number)
			w='original dimension:('
			do i=1,Dimen%LenDimData-1
				w=w+Dimen%DimData(i)+','
			end do
			w=w+Dimen%DimData(Dimen%LenDimData)+')'
			call writemess(w,cpu_number)
		end if
		if(Dimen%nameflag.eq.1)then
			allocate(ws(Dimen%lenDimData))
			ws=Dimen%DimName(1:Dimen%lenDimData)
			w="index Name are:"
			do i=1,Dimen%lenDimData
				w=w+','+ws(i)
			end do
			w=w+'.'
			call writemess(w,cpu_number)
		end if
		return
	end subroutine
	
	integer function outNameFlag(dimen)
		class(Dimension),intent(in)::dimen
		outNameFlag=dimen%NameFlag
		return
	end function
   subroutine set_len_of_intName(lenTenD,lendimN)
      integer,intent(in)::LenTenD,lendimN
      if(lenTenD.gt.len_of_intName_in_type_define)then 
         write(*,*)"The len_of_lenTenD is larger than the max length"
         call error_stop()
       end if
       if(lendimN.gt.len_of_intName_in_type_define)then 
         write(*,*)"The len_of_lenTenD is larger than the max length"
         call error_stop()
       end if
      len_of_intTensorName=lenTenD
      len_of_intDimenName=lendimN
      return
   end subroutine
   integer function outTenNameLen()
   	outTenNameLen=len_of_intTensorName
   	return
   end function
   integer function outDimNameLen()
   	outDimNameLen=len_of_intDimenName
   	return
   end function
	logical function out_simple_dimension_flag(dimen)
   	class(Dimension),intent(in)::dimen
		out_simple_dimension_flag=dimen%sample_dimension_flag
		return
	end function
	subroutine set_sample_dimension(dimen)!make the dimension as a sample_dimension, and kill dimensionName
   	type(Dimension),intent(inout)::dimen
   	integer::i
   	if(dimen%sample_dimension_flag) return
   	
   	do i=1,dimen%Dimsize
   		dimen%DimData(i)=dimen.i.i !Will not gose wrong,as the index of dimen.i.i will larger then dimen%DimData(i)
   	end do
   	dimen%LenDimData=dimen%Dimsize
		dimen%sample_dimension_flag=.true.
		dimen%Dimsize=0
		dimen%boundarysize=0
		Dimen%nameflag=0
		return
	end subroutine
	subroutine outDimension_boundry(dimen1,boundary,boundarysize,Dimsize)
   	type(Dimension),intent(in)::dimen1
   	integer,intent(inout)::boundarysize,Dimsize
   	integer,allocatable,intent(inout)::boundary(:)
   	integer::i
   	if(dimen1%sample_dimension_flag)then
			Dimsize=dimen1%lenDimData
			boundarysize=dimen1%lenDimData+1
			call allocateCheck(boundary,boundarysize)
			do i=1,boundarysize
				boundary(i)=i-1
			end do
		else
			Dimsize=dimen1%Dimsize
			boundarysize=dimen1%boundarysize
			call allocateCheck(boundary,boundarysize)
			do i=1,Dimen1%boundarysize
				boundary(i)=Dimen1%boundary(i)
			end do
		end if
		return
	end subroutine
   	
!*****************************************************
!  input A0 output 'A' , 0
!*******  function or subroutine for name   **************
	subroutine cleanDimensionName(Dimen)
		class(Dimension),intent(inout)::	Dimen
		if(allocated(Dimen%DimName))then
			deallocate(Dimen%DimName)
		end if
		if(allocated(Dimen%DimIntName))then
			deallocate(Dimen%DimIntName)
		end if
		Dimen%nameflag=0
		return
	end subroutine
!  input asd.ad output 'asd' and 'ad', chartoName_log=.true.
!	!  input asd output 'asd' , '0'  and chartoName_log=.false.
	logical function chartoName_log(TensorName,DimenName,w_)
		character(len=*),intent(in)::w_
		character(len=len(trim(adjustl(w_))))::w
		character(len=len_of_Name),intent(out)::DimenName,TensorName
		integer::lenw,lenw2
		w=trim(adjustl(w_))
		lenw=index(w,indexsymbol)
		lenw2=len(trim(w))
		if(lenw.eq.0)then!input asd
			chartoName_log=.false.
			TensorName=w
			DimenName='0'
			return
		end if
		chartoName_log=.true.
		!input 'A2.1'(if indexsymbol='.') 
		TensorName=w(1:lenw-1)
		DimenName=w(lenw+1:lenw2)
		return
	end function
!  input asd.ad .true.
!  else output .false.
	logical function long_Name_logi(w_)
		character(len=*),intent(in)::w_
		character(len=len(trim(adjustl(w_))))::w
		integer::lenw
		w=trim(adjustl(w_))
		lenw=index(w,indexsymbol)
		if(lenw.eq.0)then!input asd
			long_Name_logi=.false.
			return
		end if
		long_Name_logi=.true.
		return
	end function
!*******  function or subroutine for name   **************
	type(DimensionName) function Nameinit1(TensorName,DimenName)
		character(len=*),intent(in)::TensorName
		character(len=*),intent(in)::DimenName
		Nameinit1%TensorName=TensorName
		Nameinit1%DimenName=DimenName
		return
	end function
	type(DimensionName) function Nameinit2(TensorName,DimenName)
		character(len=*),intent(in)::TensorName
		integer,intent(in)::DimenName
		Nameinit2%TensorName=TensorName
		Nameinit2%DimenName=DimenName
		return
	end function
	type(DimensionIntName) function intNameinit1(TensorName,DimenName)
		integer,intent(in)::TensorName(:),DimenName(:)
		intNameinit1%TensorName(1:len_of_intTensorName)=TensorName(1:len_of_intTensorName)
		intNameinit1%DimenName(1:len_of_intDimenName)=DimenName(1:len_of_intDimenName)
		return
	end function 
	type(DimensionIntName) function intNameinit2(intDimname)
		integer,intent(in)::intDimname(:,:)
		intNameinit2%TensorName(1:len_of_intTensorName)=intDimname(1,1:len_of_intTensorName)
		intNameinit2%DimenName(1:len_of_intDimenName)=intDimname(2,1:len_of_intDimenName)
		return
	end function 
	type(DimensionIntName) function intNameinit3(TensorName,DimenName)
		integer,intent(in)::TensorName(:),DimenName
		intNameinit3%TensorName(1:len_of_intTensorName)=TensorName(1:len_of_intTensorName)
		intNameinit3%DimenName(1)=DimenName
		return
	end function 
	type(DimensionIntName) function intNameinit4(TensorName,DimenName)
		integer,intent(in)::TensorName,DimenName
		intNameinit4%TensorName(1)=TensorName
		intNameinit4%DimenName(1)=DimenName
		return
	end function 
		
!*******  function or subroutine for name   **************
	subroutine copyName(Nameout,Namein)
		type(DimensionName),intent(inout)::Nameout
		type(DimensionName),intent(in)::Namein
		Nameout%TensorName=Namein%TensorName
		Nameout%DimenName=Namein%DimenName
		return
	end subroutine		
	subroutine copyintName(Nameout,Namein)!the length of TensorName in Nameout or Namein are the same
		type(DimensionIntName),intent(inout)::Nameout
		type(DimensionIntName),intent(in)::Namein
		Nameout%TensorName=Namein%TensorName
		Nameout%DimenName=Namein%DimenName
		return
	end subroutine		
!*******  function or subroutine for name   **************

	subroutine copyNameArray(Nameout,Namein)
		type(DimensionName),intent(inout)::Nameout(:)
		type(DimensionName),intent(in)::Namein(:)
		integer::Lenout,lenIN,i
		lenIN=size(Namein)
		if(size(Nameout).lt.lenIN)then
			write(*,*)"ERROR in assignment of two Name array "
			write(*,*)"Name1(:)=Name2(:),size(Name1)<size(Name2)"
			write(*,*)size(Nameout),lenIN
			call error_stop()
		end if
		do i=1,lenIN
			Nameout(i)=Namein(i)
		end do
		return
	end subroutine		
	subroutine copyintNameArray(Nameout,Namein)
		type(DimensionIntName),intent(inout)::Nameout(:)
		type(DimensionIntName),intent(in)::Namein(:)
		integer::Lenout,lenIN,i
		lenIN=size(Namein)
		if(size(Nameout).lt.lenIN)then
			write(*,*)"ERROR in assignment of two intName array "
			write(*,*)"Name1(:)=Name2(:),size(Name1)<size(Name2)"
			write(*,*)size(Nameout),lenIN
			call error_stop()
		end if
		do i=1,lenIN
			Nameout(i)=Namein(i)
		end do
		return
	end subroutine	
!Name=w
!w='A','A1','A1.1'
	subroutine charName(outName,w)
		type(DimensionName),intent(inout)::outName
		character(len=*),intent(in)::w
		logical::flag
		flag=chartoName_log(outName%TensorName,outName%DimenName,w)
		return
	end subroutine
	subroutine Namechar(w,inName)
		type(DimensionName),intent(in)::inName
		character(len=*),intent(inout)::w
		w=inName%TensorName+indexsymbol+inName%DimenName
		return
	end subroutine
!example:inName is( [0,0,1][0,1,2])
!output character will be "0_0_1.0_1_2"
	subroutine intNamechar(w,inName)
		type(DimensionIntName),intent(in)::inName
		character(len=*),intent(inout)::w
		integer::i
		w=""
		do i=1,len_of_intTensorName
			w=w+inName%TensorName(i)
			if(i.ne.len_of_intTensorName) w=w+intNamesymbol
		end do
		w=w+indexsymbol
		do i=1,len_of_intDimenName
			w=w+inName%DimenName(i)
			if(i.ne.len_of_intDimenName) w=w+intNamesymbol
		end do
		return
	end subroutine
	
!	input array ,for example (/'A1.12','A1.2 ','A1.3 '/)
!	output a array of DimensionName
!	the array input should be the same length
!	'A1.12' length is 5, 'A1.2 ' is 5 too
!name(:)=w(:)
	 subroutine charNameArray(R,w)
	 	type(DimensionName),intent(inout)::R(:)
		character(len=*),intent(in)::w(:)
		integer::lenw,i
		lenw=size(w)
		if(size(R).lt.lenw)then
			write(*,*)"ERROR in assignment of two Name array "
			write(*,*)"R(:)=w(:),size(R)<size(w)"
			write(*,*)size(R),lenw
			call error_stop()
		end if
		do i=1,lenw
			R(i)=w(i)
		end do
		return
	end subroutine
!character(:)=DimensionName(:)
	subroutine NamecharArray(w,inName)
	 	type(DimensionName),intent(in)::inName(:)
		character(len=*),intent(inout)::w(:)
		integer::lenw,i
		lenw=size(inName)
		if(size(w).lt.lenw)then
			write(*,*)"ERROR in assignment of two Name array "
			write(*,*)"R(:)=w(:),size(R)<size(w)"
			write(*,*)size(w),lenw
			call error_stop()
		end if
		do i=1,lenw
			w(i)=inName(i)
		end do
		return
	end subroutine
	subroutine intNamecharArray(w,inName)
	 	type(DimensionIntName),intent(in)::inName(:)
		character(len=*),intent(inout)::w(:)
		integer::lenw,i
		lenw=size(inName)
		if(size(w).lt.lenw)then
			write(*,*)"ERROR in assignment of two Name array "
			write(*,*)"R(:)=w(:),size(R)<size(w)"
			write(*,*)size(w),lenw
			call error_stop()
		end if
		do i=1,lenw
			w(i)=inName(i)
		end do
		return
	end subroutine
!outName(:)=inName(:),will allocate outName	
	subroutine copyNameAllocate(outName,inName)
		type(DimensionName),allocatable,intent(inout)::outName(:)
		type(DimensionName),intent(in)::inName(:)
		integer::Lenout,lenIN,i
		lenIN=size(inName)
		if(allocated(outName))then
			if(size(outName).ne.lenIN)then
				deallocate(outName)
				allocate(outName(lenIN))
			end if
		else
			allocate(outName(lenIN))
		end if
		do i=1,lenIN
			outName(i)=inName(i)
		end do
	end subroutine
	subroutine copyintNameAllocate(outName,inName)
		type(DimensionIntName),allocatable,intent(inout)::outName(:)
		type(DimensionIntName),intent(in)::inName(:)
		integer::Lenout,lenIN,i
		lenIN=size(inName)
		if(allocated(outName))then
			if(size(outName).ne.lenIN)then
				deallocate(outName)
				allocate(outName(lenIN))
			end if
		else
			allocate(outName(lenIN))
		end if
		do i=1,lenIN
			outName(i)=inName(i)
		end do
	end subroutine
	
!*******  function or subroutine for name   **************
!set a name to the diemsnion
!w may be something like 'A' or 'A1'
	subroutine setDimName0(dimen,w)
		class(Dimension),intent(inout)::dimen
		character(len=*),intent(in)::w
		character(len=len_of_Name)::TensorName,DimenName
		integer::lenD,i
		logical::flag
		flag=chartoName_log(TensorName,DimenName,w)
		if(flag)then
			write(*,*)"ERROR in Set the name to the Tensor"
			write(*,*)"input:",w
			write(*,*)"could not contains the indexsymbol:",indexsymbol
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"The dimension should be in its original dimension"
			call error_stop()
		end if
		if(dimen%nameflag.eq.2) then
			write(*,*)"The Dimension have the name of integer"
			call error_stop()
		end if
		lenD=dimen%LenDimData
		call allocateCheckName(dimen%dimname,lenD)
		if(dimen%nameflag.eq.1)then!There are name in the dimension,rename
			do i=1,lenD
				dimen%dimname(i)%TensorName=TensorName
			end do
		else
			do i=1,lenD
				dimen%DimName(i)=Nameinit(TensorName,i)
			end do
			dimen%nameflag=1
		end if
		if(check_same_name_Flag)call check_same_name_in_dimension(dimen)
		return
	end subroutine
	subroutine integerDimName(dimen,TensorName)!set TensorName
		class(Dimension),intent(inout)::dimen
		integer,intent(in)::TensorName(:)
		integer::lenD,i
		if(.not.if_original_dim(dimen))then
			write(*,*)"The dimension should be in its original dimension"
			call error_stop()
		end if
		if(dimen%nameflag.eq.1) then
			write(*,*)"The Dimension have the name of character"
			call error_stop()
		end if
		lenD=dimen%LenDimData
		call allocateCheckName(dimen%DimintName,lenD)
		if(dimen%nameflag.eq.2)then!There are name in the dimension,rename
			do i=1,lenD
				dimen%DimintName(i)%TensorName=TensorName
			end do
		else
			do i=1,lenD
				dimen%DimintName(i)=Nameinit(TensorName,-i)
			end do
			dimen%nameflag=2
		end if
		return
	end subroutine

	
	subroutine printName(na)
		type(DimensionName),intent(in)::na
		character*1000::w
		w=na
		write(*,*)trim(adjustl(w))
		return
	end subroutine
	subroutine printIntName(na)
		type(DimensionName),intent(in)::na
		character*1000::w
		w=na
		write(*,*)trim(adjustl(w))
		return
	end subroutine
			
		
		
!***************   cleanDimension   *****************
	subroutine cleanDimension(Dimen)
		class(Dimension),intent(inout)::	Dimen
		Dimen%boundarysize=0
		Dimen%Dimsize=0
		Dimen%lenDimData=0
		Dimen%nameflag=0
		if(allocated(Dimen%boundary)) then
			deallocate(Dimen%boundary)
		end if
		if(allocated(Dimen%DimData)) then
			deallocate(Dimen%DimData)
		end if
		call cleanDimensionName(Dimen)
		Dimen%sample_dimension_flag=.true.
		return
	end subroutine
	subroutine emptyDimension(Dimen)!make dimension to a a empty dimension.but do not deallocate
		class(Dimension),intent(inout)::	Dimen
		if(deallocate_memory_flag)then
			call cleanDimension(Dimen)
			return
		end if
		Dimen%boundarysize=0
		Dimen%Dimsize=0
		Dimen%lenDimData=0
		Dimen%nameflag=0
		Dimen%sample_dimension_flag=.true.
		return
	end subroutine
	
	subroutine outDimData(dimen,DimData)
		class(Dimension),intent(in)::dimen
		integer,intent(inout)::DimData(:)
		DimData=dimen%DimData(1:dimen%lenDimData)
		return
	end subroutine
!****************print the information of dimension ****************************************	
	subroutine Dprint1(Dimen)
		type(Dimension),intent(in) ::Dimen
		integer,allocatable :: dimenVec(:)
		CHARACTER*5000,allocatable::w(:)
		integer::i
		write(*,*) "***   START   ***"
		write(*,*) Dimen%DimData(1:dimen%lenDimData)
		if(.not.Dimen%sample_dimension_flag) then
			if(Dimen%nameflag.eq.1)then
				allocate(w(Dimen%lenDimData))
				w=Dimen%DimName(1:Dimen%lenDimData)
				write(*,*)"index Name are"
				write(*,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
			end if
			if(Dimen%nameflag.eq.2)then
				allocate(w(Dimen%lenDimData))
				w=Dimen%DimintName(1:Dimen%lenDimData)
				write(*,*)"index Name are"
				write(*,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
			end if
			call copydimension(dimenVec,Dimen)
			write(*,*) "			--				"
			write(*,*)	dimenVec
		end if
		write(*,*) "***   END   ***"
	end subroutine
				
	subroutine Dprint0(Dimen)
		type(Dimension),intent(in) ::Dimen
		integer,allocatable :: dimenVec(:)
		CHARACTER*5000,allocatable::w(:)
		integer::i
		write(*,*) "***   Dimension Data    ***"
		write(*,*) Dimen%DimData(1:dimen%lenDimData)
		if(.not.Dimen%sample_dimension_flag)then
			call copydimension(dimenVec,Dimen)
			write(*,*)	dimenVec
			write(*,*) "***   Dimension END   ***"
		else
			write(*,*) "***   Dimension END   ***"
		end if
		if(Dimen%nameflag.eq.1)then
			allocate(w(Dimen%lenDimData))
			w=Dimen%DimName(1:Dimen%lenDimData)
			write(*,*)"index Name are"
			write(*,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
		end if
		if(Dimen%nameflag.eq.2)then
			allocate(w(Dimen%lenDimData))
			w=Dimen%DimintName(1:Dimen%lenDimData)
			write(*,*)"index Name are"
			write(*,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
		end if
	end subroutine
	subroutine Dprint(Dimen,uni)
		class(Dimension),intent(in) ::Dimen
		integer,optional,intent(in)::uni
		integer,allocatable :: dimenVec(:)
		CHARACTER*5000,allocatable::w(:)
		integer::i,maxdim
		CHARACTER*100::wlen,w2
		CHARACTER*5000::words
		if(present(uni))then
			write(uni,*) "***   Dimension Data    ***"
			if(Dimen%LenDimData.le.0)then
				write(uni,*) "***   There is no data in Dimension    ***"
				return
			end if
			if(Dimen%sample_dimension_flag)then
				maxdim=maxval(Dimen%DimData(1:Dimen%LenDimData))
				call outputform(w2,maxdim)
				wlen='('+(Dimen%LenDimData)+w2+')'
				write(uni,wlen)Dimen%DimData(1:Dimen%LenDimData)
			else
				call copydimension(dimenVec,Dimen)
				maxdim=maxval(dimenVec)
				call outputform(w2,maxdim)
				wlen='('+size(dimenVec)+w2+')'
				write(uni,wlen)	dimenVec
				write(uni,*)"It is not original dimension "
			end if
			write(uni,*) "***   Dimension END   ***"
			if(Dimen%nameflag.eq.1)then
				allocate(w(Dimen%lenDimData))
				w=Dimen%DimName(1:Dimen%lenDimData)
				write(uni,*)"index Name are"
				write(uni,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
			end if
			if(Dimen%nameflag.eq.2)then
				allocate(w(Dimen%lenDimData))
				w=Dimen%DimintName(1:Dimen%lenDimData)
				write(uni,*)"index Name are"
				write(uni,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
			end if
			write(uni,*) " "
		else
			call writemess("***   Dimension Data    ***",-1)
			if(Dimen%LenDimData.le.0)then
				call writemess( "***   There is no data in Dimension    ***",-1)
				return
			end if
			if(Dimen%sample_dimension_flag)then
				words=' '+Dimen%DimData(1)
				do i=2,Dimen%LenDimData
					words=words+(' ,'+(' '+Dimen%DimData(i)))
				end do
				call writemess(words,-1)
			else
				call copydimension(dimenVec,Dimen)
				words=' '+dimenVec(1)
				do i=2,size(dimenVec)
					words=words+(' ,'+(' '+dimenVec(i)))
				end do
				call writemess(words,-1)
				call writemess("It is not original dimension ",-1)
			end if
			call writemess( "***   Dimension END   ***",-1)
			if(Dimen%nameflag.eq.1)then
				allocate(w(Dimen%lenDimData))
				w=Dimen%DimName(1:Dimen%lenDimData)
				words=' '+w(1)
				do i=2,Dimen%LenDimData
					words=words+(' ,'+(' '+w(i)))
				end do
				call writemess("index Name are",-1)
				call writemess(words,-1)
			end if
			if(Dimen%nameflag.eq.2)then
				allocate(w(Dimen%lenDimData))
				w=Dimen%DimintName(1:Dimen%lenDimData)
				write(*,*)"index Name are"
				write(*,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
			end if
			write(*,*) " "
		end if
	end subroutine
	subroutine outputform(w,maxdim)
		integer,intent(in)::maxdim
		CHARACTER*100,intent(inout)::w
		integer::num
		num=log(real(maxdim))/log(10.)
		w='I'+(num+3)
		return
	end subroutine
		
				
	subroutine Dprint2(Dimen,uni)
		class(Dimension),intent(in) ::Dimen
		integer,optional,intent(in)::uni
		integer,allocatable :: dimenVec(:)
		CHARACTER*500,allocatable::w(:)
		integer::i
		if(present(uni))then
			write(uni,*) "***   Dimension Data    ***"
			write(uni,*)"sample_flag ",Dimen%sample_dimension_flag
			write(uni,*)"leng ",Dimen%LenDimData
			if(Dimen%LenDimData.le.0)then
				write(uni,*) "***   There is no data in Dimension    ***"
				return
			end if
			write(uni,*)Dimen%DimData(1:Dimen%LenDimData)
			if(.not.Dimen%sample_dimension_flag)then
				write(uni,*) "Dimsize"
				write(uni,*) Dimen%Dimsize
				write(uni,*) "boundarysize"
				write(uni,*) Dimen%boundarysize
				write(uni,*) "boundary"
				write(uni,*) Dimen%boundary(1:Dimen%boundarysize)
				call copydimension(dimenVec,Dimen)
				write(uni,*) "		Dimension   	"
				write(uni,*)	dimenVec
			end if
			write(uni,*)"nameflag ",Dimen%nameflag
			if(Dimen%nameflag.eq.1)then
				allocate(w(Dimen%lenDimData))
				w=Dimen%DimName(1:Dimen%lenDimData)
				write(uni,*)"index Name are"
				write(uni,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
			end if
			if(Dimen%nameflag.eq.2)then
				allocate(w(Dimen%lenDimData))
				w=Dimen%DimintName(1:Dimen%lenDimData)
				write(uni,*)"index Name are"
				write(uni,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
			end if
			write(uni,*) "***   END   ***"
		else
			write(*,*) "***   Dimension Data    ***"
			write(*,*)"sample_flag ",Dimen%sample_dimension_flag
			write(*,*)"leng ",Dimen%LenDimData
			if(Dimen%LenDimData.le.0)then
				write(uni,*) "***   There is no data in Dimension    ***"
				return
			end if
			write(*,*)Dimen%DimData(1:Dimen%LenDimData)
			if(.not.Dimen%sample_dimension_flag)then
				write(*,*) "Dimsize"
				write(*,*) Dimen%Dimsize
				write(*,*) "boundarysize"
				write(*,*) Dimen%boundarysize
				write(*,*) "boundary"
				write(*,*) Dimen%boundary(1:Dimen%boundarysize)
				call copydimension(dimenVec,Dimen)
				write(*,*) "		Dimension   	"
				write(*,*)	dimenVec
			end if
			write(*,*)"nameflag ",Dimen%nameflag
			if(Dimen%nameflag.eq.1)then
				allocate(w(Dimen%lenDimData))
				w=Dimen%DimName(1:Dimen%lenDimData)
				write(*,*)"index Name are"
				write(*,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
			end if
			if(Dimen%nameflag.eq.2)then
				allocate(w(Dimen%lenDimData))
				w=Dimen%DimintName(1:Dimen%lenDimData)
				write(*,*)"index Name are"
				write(*,*)(trim(adjustl(w(i)))//"  ",i=1,Dimen%lenDimData)
			end if
			write(*,*) "***   END   ***"
		end if
	end subroutine
	subroutine readdimension(dimen,uni)
		class(dimension),intent(inout)::dimen
		integer,intent(in)::uni
		logical::flag
		character*50::notused
		integer::lendimData,i,boundarysize,nameflag,Dimsize
		integer,allocatable::DimenData(:),boundary(:),dimnameint(:)
		character*50,allocatable::dimname(:)
		
		read(uni,*) notused
		read(uni,*)notused,flag
		read(uni,*)notused,lendimData
		if(lendimData.le.0)then
				read(uni,*) notused
				return
			end if
		allocate(DimenData(lendimData))
		read(uni,*)(DimenData(i),i=1,lendimData)
		if(.not.flag)then
			read(uni,*) notused
			read(uni,*)Dimsize
			read(uni,*) notused
			read(uni,*) boundarysize
			read(uni,*) notused
			allocate(boundary(boundarysize))
			read(uni,*)(boundary(i),i=1,boundarysize)
			read(uni,*) notused
			read(uni,*)	notused
			dimen=Dimensioniniti(boundarysize,Dimsize,boundary,DimenData)
		else
			dimen=DimenData
		end if
		read(uni,*)notused,nameflag
		if(nameflag.eq.1)then
			allocate(dimname(lendimData))
			read(uni,*)notused
			read(uni,*)(dimname(i),i=1,lendimData)
			do i=1,lendimData
				call dimen%setName(i,dimname(i),.true.)
			end do
		end if
		if(nameflag.eq.2)then
				call writemess("cannot read int name",-1)
				call error_stop()
		end if
		read(uni,*) notused
		return
	end subroutine
	
	type(Dimension) function Dimensioniniti(boundarysize,Dimsize,boundary,DimData)
		integer,intent(in) :: boundarysize
		integer,intent(in) :: Dimsize
		integer,intent(in) :: boundary(:)
		integer,intent(in) :: DimData(:)
		allocate(Dimensioniniti%boundary(size(boundary,1)))
		allocate(Dimensioniniti%DimData(size(DimData,1)))
		Dimensioniniti%boundary=boundary
		Dimensioniniti%DimData=DimData
		Dimensioniniti%boundarysize=boundarysize
		Dimensioniniti%Dimsize=Dimsize
		Dimensioniniti%lenDimData=size(DimData,1)
		Dimensioniniti%Nameflag=0
		Dimensioniniti%sample_dimension_flag=.false.
		return
	end function
	subroutine resetDimension(dimen,DimData)
		class(Dimension),intent(inout) ::Dimen
		integer,intent(in) :: DimData(:)
		if(size(DimData).ne.dimen%outlenDimData())then
			call writemess("Can not reset the dimension in type(Dimension)",-1)
			call error_stop()
		end if
		Dimen%DimData(1:Dimen%lenDimData)=DimData
		return
	end subroutine
	subroutine DimInitialization(Dimen,DimData)
		type(Dimension),intent(inout) ::Dimen
		integer,intent(in) :: DimData(:)
		integer::i
		Dimen%lenDimData=size(DimData)
		call allocateCheck(Dimen%DimData,Dimen%lenDimData)
		Dimen%DimData(1:Dimen%lenDimData)=DimData
		Dimen%sample_dimension_flag=.true.
		Dimen%nameflag=0
		return
	end subroutine
	subroutine DimInitialization2(Dimen,Dimen2)
		type(Dimension),intent(inout) ::Dimen
		type(Dimension),intent(in) ::Dimen2
		integer::lenDim,lenBoun,i
		lenDim=Dimen2%lenDimData
		if(lenDim.eq.0) then !no Data
			Dimen%Dimsize=0
			Dimen%boundarysize=0
			Dimen%lenDimData=0
			Dimen%nameflag=0
			Dimen%sample_dimension_flag=.true.
			return
		end if
		Dimen%lenDimData=lenDim
		call allocateCheck(Dimen%DimData , lenDim)
		Dimen%DimData(1:lenDim)=Dimen2%DimData(1:lenDim)
		Dimen%nameflag=Dimen2%nameflag
		Dimen%sample_dimension_flag=Dimen2%sample_dimension_flag
		if((Dimen%nameflag.eq.0).and.Dimen%sample_dimension_flag) return
		
		if(Dimen2%nameflag.eq.1)then
			call allocateCheckName(dimen%dimName,lenDim)
			do i=1,lenDim
				dimen%dimName(i)=dimen2%dimName(i)
			end do
		end if
		if(Dimen2%nameflag.eq.2)then
			call allocateCheckName(dimen%dimintName,lenDim)
			do i=1,lenDim
				dimen%dimintName(i)=dimen2%dimintName(i)
			end do
		end if
		if(.not.Dimen2%sample_dimension_flag) then
			lenBoun=Dimen2%boundarysize
			Dimen%boundarysize=lenBoun
			call allocateCheck(Dimen%boundary , lenBoun)
			Dimen%boundary(1:lenBoun)=Dimen2%boundary(1:lenBoun)
			Dimen%Dimsize=Dimen2%Dimsize
		end if
		return
	end subroutine
	integer	function DimSize(Dimen)
		class(Dimension),intent(in) :: Dimen
		if(Dimen%sample_dimension_flag)then
			DimSize=Dimen%LenDimData
		else
			DimSize=Dimen%dimSize
		end if
		return
	end function
	integer	function outlenDimData(Dimen)
		class(Dimension),intent(in) :: Dimen
		outlenDimData=Dimen%lenDimData
		return
	end function
!*******  function or subroutine for name   **************		
!output the TensorName of the ith dimension
	CHARACTER(len=len_of_Name) function outNameTen(dimen,ith)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in)::ith
		integer::i
		if(Dimen%nameflag.eq.0)then
			if(dimen%lenDimData.eq.0)then
				call writemess("There is no data in the dimension",-1)
			end if
			call writemess("There is no CHARACTER name in the dimension",-1)
			call error_stop()
		end if
		if(ith.gt.Dimen%lenDimData)then
			write(*,*)"The index is larger than the size of the name"
			write(*,*)ith,Dimen%lenDimData
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"NameID is use in original dimension"
			call error_stop()
		end if
		if(Dimen%nameflag.eq.1)then
			outNameTen=Dimen%DimName(ith)%TensorName
			return
		end if
		if(Dimen%nameflag.eq.2)then
			outNameTen=''
			do i=1,len_of_intTensorName
				outNameTen=outNameTen+Dimen%DimintName(ith)%TensorName(i)
				if(i.ne.len_of_intTensorName)outNameTen=outNameTen+intNamesymbol
			end do
			return
		end if
		write(*,*)"ERROR of nameFlag",Dimen%nameflag
		call error_stop()
	end function
	CHARACTER(len=len_of_Name) function outNameTenChar(dimen,w,ch_)
		class(Dimension),intent(in) :: Dimen
		character(len=*),intent(in)::w
		character(len=*),intent(in),optional::ch_
		character(len=10)::ch
		integer::i,check
		if(Dimen%nameflag.eq.0)then
			if(dimen%lenDimData.eq.0)then
				call writemess("There is no data in the dimension",-1)
			end if
			call writemess("There is no CHARACTER name in the dimension",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"NameID is use in original dimension"
			call error_stop()
		end if
		if(Dimen%nameflag.eq.1)then
			if(present(ch_))then
				ch=ch_
			else
				ch='fullname'
			end if
			check=0
			do i=1,dimen%getrank()
				if(w.equ.dimen%outDimName(i))then
					if(ch.equ.'Tensor')then
						outNameTenChar=Dimen%DimName(i)%TensorName
					else 
						outNameTenChar=Dimen%DimName(i)%TensorName+indexsymbol+Dimen%DimName(i)%dimenName
					end if
					check=check+1
				end if
			end do
			if(check.eq.1)return
			call writemess('Cannot find the name='+w+',dimension.f90',-1)
			call Dprint(dimen)
			call error_stop()
		end if
		write(*,*)"ERROR of nameFlag"
		call error_stop()
	end function
	logical function outAllNameTenChar(dimen,outchar,w,ch_)
		class(Dimension),intent(in) :: Dimen
		character(len=*),intent(in)::w
		character(len=*),intent(in),optional::ch_
		character(len=max_len_of_char_in_TData),allocatable,intent(inout)::outchar(:)
		character(len=10)::ch
		character(len=max_len_of_char_in_TData),allocatable::temp(:)
		integer::i,k
		if(Dimen%nameflag.eq.0)then
			if(dimen%lenDimData.eq.0)then
				call writemess("There is no data in the dimension",-1)
			end if
			call writemess("There is no CHARACTER name in the dimension",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			call writemess("NameID is use in original dimension",-1)
			call error_stop()
		end if
		if(Dimen%nameflag.eq.1)then
			if(present(ch_))then
				ch=ch_
			else
				ch='fullname'
			end if
			allocate(temp(dimen%getRank()))
			k=0
			do i=1,dimen%getrank()
				if(w.equ.dimen%outDimName(i))then
					if(ch.equ.'Tensor')then
						k=k+1
						temp(k)=Dimen%DimName(i)%TensorName
					else 
						k=k+1
						temp(k)=Dimen%DimName(i)%TensorName+indexsymbol+Dimen%DimName(i)%dimenName
					end if
				end if
			end do
			if(k.eq.0)then
				outAllNameTenChar=.false.
				return
			end if
			outAllNameTenChar=.true.
			allocate(outchar(k))
			outchar=temp(1:k)
			return
		end if
		write(*,*)"ERROR of nameFlag"
		call error_stop()
	end function

	logical function ifName(dimen,w,ch_)
		class(Dimension),intent(in) :: Dimen
		character(len=*),intent(in)::w
		character(len=*),intent(in),optional::ch_
		character(len=10)::ch
		integer::i
		if(Dimen%nameflag.eq.0)then
			if(dimen%lenDimData.eq.0)then
				call writemess("There is no data in the dimension",-1)
			end if
			call writemess("There is no CHARACTER name in the dimension",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			call writemess("NameID is use in original dimension",-1)
			call error_stop()
		end if
		if(Dimen%nameflag.eq.1)then
			if(present(ch_))then
				ch=ch_
			else
				ch='dimension'
			end if
			ifName=.true.
			if(long_Name_logi(w))then
				do i=1,dimen%getrank()
					if(w.equ.dimen%getName(i))return
				end do
			else
				if(ch.equ.'Tensor') then
					do i=1,dimen%getrank()
						if(w.equ.Dimen%DimName(i)%TensorName)return
					end do
				else
					do i=1,dimen%getrank()
						if(w.equ.Dimen%DimName(i)%dimenName)return
					end do
				end if
			end if
			ifName=.false.
			return
		end if
		write(*,*)"ERROR of nameFlag"
		call error_stop()
	end function

	
	CHARACTER(len=len_of_Name) function outDimName(dimen,ith)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in)::ith
		if(Dimen%nameflag.eq.0)then
			if(dimen%lenDimData.eq.0)then
				call writemess("There is no data in the dimension",-1)
			end if
			call writemess("There is no CHARACTER name in the dimension",-1)
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			call writemess("NameID is use in original dimension",-1)
			call error_stop()
		end if
		if(ith.gt.Dimen%lenDimData)then
			call writemess("The index is larger than the size of the name",-1)
			write(*,*)ith,Dimen%lenDimData
			call error_stop()
		end if
		if(Dimen%nameflag.eq.1)then
			outDimName=Dimen%DimName(ith)%dimenName
			return
		end if
		call writemess("ERROR of nameFlag",-1)
		call error_stop()
	end function
	function outAllTensorName(T)
		CHARACTER(len=len_of_Name),allocatable::outAllTensorName(:)
		class(Dimension),intent(in)::T
		integer::i
		allocate(outAllTensorName(T%outlenDimData()))
		do i=1,T%outlenDimData()
			outAllTensorName(i)=outNameTen(T,i)
		end do
		return
	end function
	function outIntNameTen(dimen,ith)
		integer::outIntNameTen(len_of_intTensorName)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in)::ith
		if(Dimen%nameflag.ne.2)then
			call writemess("There is no integer name in the dimension",-1)
			call error_stop()
		end if
		if(ith.gt.Dimen%LenDimData)then
			call writemess("The index is larger than the size of the name",-1)
			write(*,*)ith,Dimen%LenDimData
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			call writemess("NameID is use in original dimension",-1)
			call error_stop()
		end if
		outIntNameTen=Dimen%DimintName(ith)%TensorName(1:len_of_intTensorName)
		return
	end function
	function outIntNameDim(dimen,ith)
		integer::outIntNameDim(len_of_intDimenName)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in)::ith
		if(Dimen%nameflag.ne.2)then
			call writemess("There is no integer name in the dimension",-1)
			call error_stop()
		end if
		if(ith.gt.Dimen%LenDimData)then
			call writemess("The index is larger than the size of the name",-1)
			write(*,*)ith,Dimen%LenDimData
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			call writemess("NameID is use in original dimension",-1)
			call error_stop()
		end if
		outIntNameDim=Dimen%DimintName(ith)%DimenName(1:len_of_intDimenName)
		return
	end function
	function outAllIntNameDim(Dimen)
		integer,allocatable::outAllIntNameDim(:,:)
		class(Dimension),intent(in) :: Dimen
		integer::i,DimNamelen
		DimNamelen=outDimNameLen()
		allocate(outAllIntNameDim(Dimen%Getrank(),DimNamelen))
		do i=1,Dimen%Getrank()
			outAllIntNameDim(i,:)=Dimen%outIntNameDim(i)
		end do
		return
	end function
!*******  function or subroutine for name   **************		
!output the index of the ith dimension
	CHARACTER(len=len_of_Name+len_of_Name) function outName1(dimen,ith)result(outName)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in)::ith
		if(Dimen%nameflag.eq.0)then
			write(*,*)"There is no name in the dimension"
			call error_stop()
		end if
		if(ith.gt.size(Dimen%DimName))then
			write(*,*)"The index is larger than the size of the name"
			write(*,*)ith,size(Dimen%DimName)
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"indexID is use in original dimension"
			call error_stop()
		end if
		if(Dimen%nameflag.eq.1)then
			outName=Dimen%DimName(ith)
			return
		end if
		if(Dimen%nameflag.eq.2)then
			outName=Dimen%DimintName(ith)
			return
		end if
		write(*,*)"ERROR of nameFlag"
		call error_stop()
	end function
	function outNameAll(Dimen)
		CHARACTER(len=len_of_Name+len_of_Name),allocatable::outNameAll(:)
		class(Dimension),intent(in) :: Dimen
		integer::i,length
		length=Dimen%Getrank()
		allocate(outNameAll(length))
		do i=1,length
			outNameAll(i)=outName1(dimen,i)
		end do
		return
	end function
	function outDimintName(dimen,ith,TensorNameflag)
		integer,allocatable::outDimintName(:)
		class(Dimension),intent(in) :: Dimen
		logical,intent(in)::TensorNameflag
		integer,intent(in)::ith
		if(Dimen%nameflag.ne.2)then
			write(*,*)"There is no integer name in the dimension"
			call error_stop()
		end if
		if(ith.gt.size(Dimen%DimintName))then
			write(*,*)"The index is larger than the size of the name"
			write(*,*)ith,size(Dimen%DimintName)
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"NameID is use in original dimension"
			call error_stop()
		end if
		if(TensorNameflag)then
			allocate(outDimintName(len_of_intTensorName))
			outDimintName=Dimen%DimintName(ith)%TensorName(1:len_of_intTensorName)
		else
			allocate(outDimintName(len_of_intDimenName))
			outDimintName=Dimen%DimintName(ith)%DimenName(1:len_of_intDimenName)
		end if
		return
	end function
		
		
!*******  function or subroutine for name   **************		
!find the index,whose name is w	,output the order of it in the dimension
!If can not find , output 0
	integer function Nameorder2(dimen,w)
		class(Dimension),intent(in) :: Dimen
		CHARACTER(len=*),intent(in)::w
		integer::i
		type(DimensionName)::nam
		if(Dimen%nameflag.ne.1)then
			call writemess("There is no character name in the dimension,Nameorder2",-1)
			call writemess('nameflag='+Dimen%nameflag,-1)
			call error_stop()
		end if
		if(.not.long_Name_logi(w))then
			call writemess('input error, one should input name written as A'+indexsymbol+'B')
			call error_stop
		end if
		if(.not.if_original_dim(dimen))then
			call writemess("Nameorder2 is use in original dimension",-1)
			call error_stop()
		end if
		nam=w
		do i=1,dimen%lenDimData
			if(dimen%dimname(i).equ.nam)then
				Nameorder2=i
				return
			end if
		end do
		Nameorder2=0
		return
	end function
	integer function Nameorder2_check(dimen,w)
		class(Dimension),intent(in) :: Dimen
		CHARACTER(len=*),intent(in)::w
		integer::i
		type(DimensionName)::nam
		if(Dimen%nameflag.ne.1)then
			write(*,*)"There is no char name in the dimension,FindOrder"
			write(*,*)Dimen%nameflag
			call error_stop()
		end if
		if(.not.long_Name_logi(w))then
			call writemess('input error, one should input name written as A'+indexsymbol+'B')
			call error_stop
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"FindOrder is use in original dimension"
			call error_stop()
		end if
		nam=w
		do i=1,dimen%lenDimData
			if(dimen%dimname(i).equ.nam)then
				Nameorder2_check=i
				return
			end if
		end do
		call writemess('Can Not Find the name:'+w)
		call Dprint(dimen)
		call error_stop
		return
	end function
	integer function Nameorder3(dimen,dimname)
		class(Dimension),intent(in) :: Dimen
		integer::i
		type(DimensionName),intent(in)::dimname
		if(Dimen%nameflag.ne.1)then
			write(*,*)"There is no char name in the dimension,Nameorder3"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"Nameorder3 is use in original dimension"
			call error_stop()
		end if
		do i=1,dimen%lenDimData
			if(dimen%dimname(i).equ.dimname)then
				Nameorder3=i
				return
			end if
		end do
		Nameorder3=0
		return
	end function
	integer function Nameorder3_check(dimen,dimname)
		class(Dimension),intent(in) :: Dimen
		integer::i
		type(DimensionName),intent(in)::dimname
		if(Dimen%nameflag.ne.1)then
			write(*,*)"There is no char name in the dimension,FindOrder"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"FindOrder is use in original dimension"
			call error_stop()
		end if
		do i=1,dimen%lenDimData
			if(dimen%dimname(i).equ.dimname)then
				Nameorder3_check=i
				return
			end if
		end do
		call writemess('Can Not Find the name:')
		call dimen%print()
		return
	end function
	integer function Nameorder4(dimen,TensorName,dimenName)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in)::TensorName(:),dimenName(:)
		integer::i
		type(DimensionIntName)::testName 
		if(Dimen%nameflag.ne.2)then
			write(*,*)"There is no int name in the dimension,Nameorder4"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"Nameorder4 is use in original dimension"
			call error_stop()
		end if
		testName=Nameinit(TensorName,DimenName)
		do i=1,dimen%lenDimData
			if(dimen%dimintname(i).equ.testName)then
				Nameorder4=i
				return
			end if
		end do
		Nameorder4=0
		return
	end function
	integer function Nameorder4_check(dimen,TensorName,dimenName)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in)::TensorName(:),dimenName(:)
		integer::i
		type(DimensionIntName)::testName 
		if(Dimen%nameflag.ne.2)then
			write(*,*)"There is no int name in the dimension,Nameorder4"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"Nameorder4 is use in original dimension"
			call error_stop()
		end if
		testName=Nameinit(TensorName,DimenName)
		do i=1,dimen%lenDimData
			if(dimen%dimintname(i).equ.testName)then
				 Nameorder4_check=i
				return
			end if
		end do
		call writemess('Can Not Find the name:')
		call dimen%print()
		return
	end function
	integer function Nameorder5(dimen,TensoDimrName)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in)::TensoDimrName(:,:)
		integer::i
		type(DimensionIntName)::testName 
		if(Dimen%nameflag.ne.2)then
			write(*,*)"There is no int name in the dimension,Nameorder5"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"Nameorder5 is use in original dimension"
			call error_stop()
		end if
		testName=Nameinit(TensoDimrName)
		do i=1,dimen%lenDimData
			if(dimen%dimintname(i).equ.testName)then
				Nameorder5=i
				return
			end if
		end do
		Nameorder5=0
		return
	end function
	integer function Nameorder5_check(dimen,TensoDimrName)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in)::TensoDimrName(:,:)
		integer::i
		type(DimensionIntName)::testName 
		if(Dimen%nameflag.ne.2)then
			write(*,*)"There is no int name in the dimension,Nameorder5"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"Nameorder5 is use in original dimension"
			call error_stop()
		end if
		testName=Nameinit(TensoDimrName)
		do i=1,dimen%lenDimData
			if(dimen%dimintname(i).equ.testName)then
				Nameorder5_check=i
				return
			end if
		end do
		call writemess('Can Not Find the name:')
		call dimen%print()
		return
	end function
	
	integer function Nameorder6(dimen,TensoDimName)
		class(Dimension),intent(in) :: Dimen
		type(DimensionIntName),intent(in)::TensoDimName
		integer::i
		if(Dimen%nameflag.ne.2)then
			write(*,*)"There is no int name in the dimension,Nameorder5"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"Nameorder5 is use in original dimension"
			call error_stop()
		end if
		do i=1,dimen%lenDimData
			if(dimen%dimintname(i).equ.TensoDimName)then
				Nameorder6=i
				return
			end if
		end do
		Nameorder6=0
		return
	end function
	
	function NameorderArray(dimen,w)
		class(Dimension),intent(in) :: Dimen
		CHARACTER(len=*),intent(in)::w(:)
		integer::NameorderArray(size(w))
		integer::i,lenn
		if(Dimen%nameflag.ne.1)then
			write(*,*)"There is no char name in the dimension,NameorderArray"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"NameorderArray is use in original dimension"
			call error_stop()
		end if
		lenn=size(w)
		if(lenn.gt.dimen%lenDimData)then
			write(*,*)"ERROR i NameorderArray"
			write(*,*)lenn,dimen%Dimsize
			call error_stop()
		end if
		do i=1,lenn
			NameorderArray(i)=Nameorder(dimen,w(i))
		end do
		return
	end function
	
	function NameorderArray_check(dimen,w)
		class(Dimension),intent(in) :: Dimen
		CHARACTER(len=*),intent(in)::w(:)
		integer::NameorderArray_check(size(w))
		integer::i,lenn
		if(Dimen%nameflag.ne.1)then
			write(*,*)"There is no char name in the dimension,NameorderArray"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"NameorderArray is use in original dimension"
			call error_stop()
		end if
		lenn=size(w)
		if(lenn.gt.dimen%lenDimData)then
			write(*,*)"ERROR i NameorderArray"
			write(*,*)lenn,dimen%Dimsize
			call error_stop()
		end if
		do i=1,lenn
			NameorderArray_check(i)=Nameorder2_check(dimen,w(i))
		end do
		return
	end function
	
	function intNameorderArray1(dimen,TensorName,dimenName)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in)::TensorName(:,:),dimenName(:,:)
		integer::intNameorderArray1(size(TensorName,1))
		integer::i,lenn
		if(Dimen%nameflag.ne.2)then
			write(*,*)"There is no int name in the dimension,NameorderArray"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"intNameorderArray is use in original dimension"
			call error_stop()
		end if
		lenn=size(TensorName,1)
		if(lenn.gt.dimen%lenDimData)then
			write(*,*)"ERROR i intNameorderArray"
			write(*,*)lenn,dimen%Dimsize
			call error_stop()
		end if
		do i=1,lenn
			intNameorderArray1(i)=Nameorder(dimen,TensorName(i,:),dimenName(i,:))
		end do
		return
	end function
	function intNameorderArray2(dimen,TensorintName)
		class(Dimension),intent(in) :: Dimen
		type(DimensionIntName),intent(in)::TensorintName(:)
		integer::intNameorderArray2(size(TensorintName))
		integer::i,lenn
		if(Dimen%nameflag.ne.2)then
			write(*,*)"There is no int name in the dimension,NameorderArray"
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			write(*,*)"intNameorderArray is use in original dimension"
			call error_stop()
		end if
		lenn=size(TensorintName)
		if(lenn.gt.dimen%lenDimData)then
			write(*,*)"ERROR i intNameorderArray"
			write(*,*)lenn,dimen%Dimsize
			call error_stop()
		end if
		do i=1,lenn
			intNameorderArray2(i)=Nameorder6(dimen,TensorintName(i))
		end do
		return
	end function
!*******  function or subroutine for name   **************			
	logical function equal_name2(dimname,dimname2)
		type(DimensionName),intent(in)::dimname,dimname2
		if(trim(adjustl(dimname%TensorName)).ne.trim(adjustl(dimname2%TensorName)))then
			equal_name2=.false.
			return
		end if
		if(trim(adjustl(dimname%dimenName)).ne.trim(adjustl(dimname2%dimenName)))then
			equal_name2=.false.
			return
		end if
		equal_name2=.true.
		return
	end function
	logical function equal_name1(dimname,w)
		type(DimensionName),intent(in)::dimname
		CHARACTER(len=*),intent(in)::w
		type(DimensionName)::tempName
		tempName=w
		equal_name1=equal_name2(dimname,tempName)
		return
	end function
	logical function equal_name3(w,dimname)
		type(DimensionName),intent(in)::dimname
		CHARACTER(len=*),intent(in)::w
		type(DimensionName)::tempName
		tempName=w
		equal_name3=equal_name2(dimname,tempName)
		return
	end function	
	logical function equal_intname(name1,name2)
		type(DimensionIntName),intent(in)::name1,name2
		logical::lg1,lg2
		lg1=name1%TensorName(:len_of_intTensorName).equ.name2%TensorName(:len_of_intTensorName)
		lg2=name1%dimenName(:len_of_intDimenName).equ.name2%dimenName(:len_of_intDimenName)
		equal_intname=lg1.and.lg2
		return
	end function
	
!*******  function or subroutine for name   **************		
!Find all the DimensionNames, whose indexname is oldname
!change them to newname
!If Cannot find it , do nothing
	subroutine setDimName1(dimen,oldname,newname)
		class(Dimension),intent(inout) :: dimen
		CHARACTER(len=*),intent(in)::oldname,newname
		integer::i
		CHARACTER(len=len_of_Name)::oldTensorName,olddimenName
		CHARACTER(len=len_of_Name)::newTensorName,newdimenName
		logical::oldfullname,newfullname
		if(Dimen%nameflag.ne.1)then
			write(*,*)"There is no char name in the dimension,resetindexname"
			call error_stop()
		end if
		oldfullname=chartoName_log(oldTensorName,olddimenName,oldname)
		!(TensorName,DimenName,w_)
		newfullname=chartoName_log(newTensorName,newdimenName,newname)
		if(oldfullname.neqv.newfullname)then
			write(*,*)"ERROR in resetIndexname1"
			call error_stop()
		end if
		if(newfullname)then
			do i=1,Dimen%lenDimData
				if(dimen%dimname(i)%TensorName.equ.oldTensorName)then
					if(dimen%dimname(i)%dimenName.equ.olddimenName)then
						dimen%dimname(i)%TensorName=newTensorName
						dimen%dimname(i)%dimenName=newdimenName
					end if
				end if
			end do
		else
			do i=1,Dimen%lenDimData
				if(dimen%dimname(i)%TensorName.equ.oldTensorName)then
						dimen%dimname(i)%TensorName=newTensorName
				end if
			end do
		end if
		return
	end subroutine
!*******  function or subroutine for name   **************		
!set the NameID and Indexname of the ith index
	subroutine setDimName2(dimen,ith,newname,no_check)
		class(Dimension),intent(inout) :: dimen
		CHARACTER(len=*),intent(in)::newname
		integer,intent(in)::ith
		logical,optional,intent(in)::no_check
		CHARACTER(len=len_of_Name)::newTensorName,newdimenName
		logical::fullname
		integer::i,lenName
		if(ith.gt.dimen%LenDimData)then
			write(*,*)"The index is larger than the size of the name"
			write(*,*)ith,dimen%LenDimData
			call error_stop()
		end if
		if(.not.if_original_dim(dimen))then
			if(.not.present(no_check))then
				write(*,*)"setName should be use in original dimension(try call T%split() and setName)"
				call error_stop()
			end if
		end if
		fullname=chartoName_log(newTensorName,newdimenName,newname)
		if(Dimen%nameflag.eq.0)then!There is no name in dimension
			lenName=dimen%LenDimData
			!allocate(dimen%dimname(lenName))
			call allocateCheckName(dimen%dimname,lenName)
			do i=1,lenName
				dimen%dimname(i)%TensorName=newTensorName
				dimen%dimname(i)%dimenName=i
			end do
			if(fullname)then
				dimen%dimname(ith)%dimenName=newdimenName
			end if
			Dimen%nameflag=1
			return
		end if
		
		if(fullname)then
			dimen%dimname(ith)%TensorName=newTensorName
			dimen%dimname(ith)%dimenName=newdimenName
		else
			dimen%dimname(ith)%TensorName=newTensorName
		end if
		return
	end subroutine
!Find all the DimensionNames, whose indexname is oldname
!change them to newname
!If Cannot find it , do nothing
	subroutine setDimName3(dimen,oldnameTen,oldnamedim,newnameTen,newnameDim)
		class(Dimension),intent(inout) :: dimen
		integer,intent(in)::oldnameTen(:),oldnamedim(:)
		integer,intent(in)::newnameTen(:),newnameDim(:)
		integer::i
		type(DimensionIntName)::oldname,newName
		if(Dimen%nameflag.ne.2)then
			write(*,*)"There is no int name in the dimension,resetindexname"
			call error_stop()
		end if
		oldname=Nameinit(oldnameTen,oldnamedim)
		newname=Nameinit(newnameTen,newnameDim)
		do i=1,dimen%lenDimData
			if(dimen%dimintname(i).equ.oldname)then
				dimen%dimintname(i)=newName
			end if
		end do
		return
	end subroutine
	subroutine setDimName4(dimen,oldnameTen,newnameTen)
		class(Dimension),intent(inout) :: dimen
		integer,intent(in)::oldnameTen(:)
		integer,intent(in)::newnameTen(:)
		integer::i
		if(Dimen%nameflag.ne.2)then
			write(*,*)"There is no int name in the dimension,resetindexname"
			call error_stop()
		end if
		do i=1,dimen%lenDimData
			if(dimen%dimintname(i)%TensorName(:len_of_intTensorName).equ.oldnameTen)then
				dimen%dimintname(i)%TensorName(:len_of_intTensorName)=newnameTen
			end if
		end do
		return
	end subroutine
	subroutine setDimName5(dimen,ith,newnameTen) 
		class(Dimension),intent(inout) :: dimen
		integer,intent(in)::ith
		integer,intent(in)::newnameTen(:)
		integer::i
		if(Dimen%nameflag.eq.1)then
			write(*,*)"There is cahr name in the dimension,resetindexname"
			call error_stop()
		end if
		if(ith.gt.dimen%LenDimData)then
			write(*,*)"The index is larger than the size of the name"
			write(*,*)ith,dimen%LenDimData
			call error_stop()
		end if
		if(Dimen%nameflag.eq.2) then
			dimen%dimintname(ith)%TensorName(:len_of_intTensorName)=newnameTen
			return
		end if
		if(Dimen%nameflag.eq.0)then
			call allocateCheckName(dimen%dimintname,dimen%lenDimData)
			do i=1,dimen%lenDimData
				dimen%dimintname(i)=Nameinit(newnameTen,-i)
			end do
			Dimen%nameflag=2
		end if
		return
	end subroutine
	subroutine setDimName6(dimen,ith,newnameTen,newnameDim)
		class(Dimension),intent(inout) :: dimen
		integer,intent(in)::ith
		integer,intent(in)::newnameTen(:),newnameDim(:)
		integer::i
		if(Dimen%nameflag.eq.1)then
			write(*,*)"There is char name in the dimension,resetindexname"
			call error_stop()
		end if
		if(ith.gt.dimen%LenDimData)then
			write(*,*)"The index is larger than the size of the name"
			write(*,*)ith,dimen%LenDimData
			call error_stop()
		end if
		if(Dimen%nameflag.eq.2) then
			dimen%dimintname(ith)%TensorName(:len_of_intTensorName)=newnameTen
			dimen%dimintname(ith)%DimenName(:len_of_intDimenName)=newnameDim
			return
		end if
		if(Dimen%nameflag.eq.0)then
			call allocateCheckName(dimen%dimintname,dimen%lenDimData)
			do i=1,dimen%lenDimData
				if(i.eq.ith) then
					dimen%dimintname(i)=Nameinit(newnameTen,newnameDim)
				else
					dimen%dimintname(i)=Nameinit(newnameTen,-i)
				end if
			end do
			Dimen%nameflag=2
		end if
		return
	end subroutine
	
	integer function outtotalData(Dimen)
		class(Dimension),intent(in) :: Dimen
		outtotalData=product(Dimen%DimData(1:dimen%lenDimData))
		return
	end function				
!	return the inde dimension	,outpout in a integer
	integer function Dim_i(Dimen,inde)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in) :: inde
		integer :: i,D1
		if(Dimen%sample_dimension_flag)then
			if(inde.gt.dimen%LenDimData) then 
				write(*,*) "ERROR in getting the ith dimension"
				write(*,*)"stop"
				call error_stop()
			end if
			Dim_i=Dimen%DimData(inde)
			return
		end if
		
		if(inde.gt.dimen%Dimsize) then 
			write(*,*) "ERROR in getting the ith dimension"
			write(*,*)"stop"
			call error_stop()
		end if
		D1=1
		do i=Dimen%boundary(inde) + 1,Dimen%boundary(inde+1)
			D1=D1*Dimen%DimData(i)
		end do
		Dim_i=D1
	end function 
	function AllDim(Dimen)
		integer,allocatable::AllDim(:)
		class(Dimension),intent(in) :: Dimen
		allocate(AllDim(Dimen%getRank()))
		AllDim=Dimen
		return
	end function 
	type(Dimension) function outDiemsnion(Dimen)
		type(Dimension),intent(in) :: Dimen
		outDiemsnion=Dimen
		return
	end function
	integer function DimName_i(Dimen,w)
		class(Dimension),intent(in) :: Dimen
		character(len=*),intent(in)::w
		integer :: inde
		inde=Dimen%Nameorder(w)
		if(inde.eq.0)then
			DimName_i=0
			return
		end if
		DimName_i=Dim_i(Dimen,inde)
	end function 
	
	
!	return  all the  dimension	,outpout in a vector	
	subroutine copydimension(dimenVec,Dimen)
		integer,allocatable,intent(inout) :: dimenVec(:)
		class(Dimension),intent(in) :: Dimen
		integer :: i
		if(Dimen%sample_dimension_flag)then
			if(allocated(dimenVec)) then
				if(dimen%LenDimData.ne.size(dimenVec)) then 
					deallocate(dimenVec)
					allocate(dimenVec(dimen%LenDimData))
				end if
			else
				allocate(dimenVec(dimen%LenDimData))
			end if
			dimenVec=dimen%DimData(1:dimen%LenDimData)
			return
		end if
		
		if(allocated(dimenVec)) then
			if(dimen%DimSize.ne.size(dimenVec)) then 
				deallocate(dimenVec)
				allocate(dimenVec(dimen%DimSize))
			end if
		else
			allocate(dimenVec(dimen%DimSize))
		end if
		do i=1,Dimen%Dimsize
			dimenVec(i)=Dim_i(Dimen,i)
		end do
		return
	end 	subroutine		
	subroutine copyDimToVec(dimenVec,Dimen)
		integer,intent(inout) :: dimenVec(:)
		class(Dimension),intent(in) :: Dimen
		integer :: i
		if(Dimen%sample_dimension_flag)then
			if(size(dimenVec).lt.dimen%LenDimData)then
				write(*,*)"ERROR in assignment dimension to array "
				write(*,*)"array(:)=dimension(:),size(array)<size(dimension)"
				write(*,*)size(dimenVec),Dimen%LenDimData
				call error_stop()
			end if
			dimenVec=dimen%DimData(1:dimen%LenDimData)
			return
		end if
		
		if(size(dimenVec).lt.Dimen%Dimsize)then
			write(*,*)"ERROR in assignment dimension to array "
			write(*,*)"array(:)=dimension(:),size(array)<size(dimension)"
			write(*,*)size(dimenVec),Dimen%Dimsize
			call error_stop()
		end if
		do i=1,Dimen%Dimsize
			dimenVec(i)=Dim_i(Dimen,i)
		end do
	end 	subroutine

!	return the inde  dimension	,outpout in a vector of the dimenison    
! If do the fuse, onedimenison will have more than one value
! [2,3,4,5]	-->fuse the 2,3 index -->[2,(3,4),5]!the dimension is 2,12,5
! then getSubDim(Dimen,2,dimenVec)==>dimenVec=[3,4]
	subroutine getSubDim(Dimen,inde,dimenVec)
		type(Dimension),intent(in) :: Dimen
		integer,allocatable,intent(inout) :: dimenVec(:)
		integer,intent(in) :: inde
		integer::i,j,Dlen
		if(Dimen%sample_dimension_flag)then
			if(allocated(dimenVec)) then
				if(size(dimenVec).ne.1) then 
					deallocate(dimenVec)
					allocate(dimenVec(1))
				end if
			else
				allocate(dimenVec(1))
			end if
			dimenVec(1)=Dimen%DimData(inde)
			return
		end if
		
		Dlen=Dimen%boundary(inde+1)-Dimen%boundary(inde)
		if(inde.gt.dimen%dimSize) then 
			write(*,*) "ERROR in getSubDim"
			return
		end if
		if(allocated(dimenVec)) then
			if(Dlen.ne.size(dimenVec)) then 
				deallocate(dimenVec)
				allocate(dimenVec(Dlen))
			end if
		else
			allocate(dimenVec(Dlen))
		end if
		j=1
		do i=Dimen%boundary(inde) + 1,Dimen%boundary(inde+1)
			dimenVec(j)=Dimen%DimData(i)
			j=j+1
		end do
	end subroutine
	
	subroutine getSubDimboundary(Dimen,inde,dimenVec)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in) :: inde
		integer,intent(inout) :: dimenVec(2)
		integer::i,j,Dlen
		if(Dimen%out_sample_flag())then
			dimenVec(1)=inde-1
			dimenVec(2)=inde
		else
			dimenVec(1)=Dimen%boundary(inde)
			dimenVec(2)=Dimen%boundary(inde+1)
		end if
		return
	end subroutine
	
!*******  function or subroutine for name   **************	
	subroutine getSubDimName(Dimen,inde,dimenName)
		type(Dimension),intent(in) :: Dimen
		type(dimensionName),allocatable,intent(inout) :: dimenName(:)
		integer,intent(in) :: inde
		integer::i,j,Dlen
		if(Dimen%sample_dimension_flag)then
			if(inde.gt.dimen%LenDimData) then 
				write(*,*) "ERROR in getSubDimName"
				return
			end if
			if(allocated(dimenName)) then
				if(1.ne.size(dimenName)) then 
					deallocate(dimenName)
					allocate(dimenName(1))
				end if
			else
				allocate(dimenName(1))
			end if
			dimenName(1)=Dimen%DimName(inde)
			return
		end if
		
		Dlen=Dimen%boundary(inde+1)-Dimen%boundary(inde)
		if(inde.gt.dimen%dimSize) then 
			write(*,*) "ERROR in getSubDimName"
			return
		end if
		if(allocated(dimenName)) then
			if(Dlen.ne.size(dimenName)) then 
				deallocate(dimenName)
				allocate(dimenName(Dlen))
			end if
		else
			allocate(dimenName(Dlen))
		end if
		j=1
		do i=Dimen%boundary(inde) + 1,Dimen%boundary(inde+1)
			dimenName(j)=Dimen%DimName(i)
			j=j+1
		end do
	end subroutine
	subroutine getSubDimIntName(Dimen,inde,dimenintName)
		type(Dimension),intent(in) :: Dimen
		type(dimensionIntName),allocatable,intent(inout) :: dimenintName(:)
		integer,intent(in) :: inde
		integer::i,j,Dlen
		if(Dimen%sample_dimension_flag)then
			if(inde.gt.dimen%LenDimData) then 
				write(*,*) "ERROR in getSubDimIntName"
				return
			end if
			if(allocated(dimenintName)) then
				if(1.ne.size(dimenintName)) then 
					deallocate(dimenintName)
					allocate(dimenintName(1))
				end if
			else
				allocate(dimenintName(1))
			end if
			dimenintName(1)=Dimen%DimIntName(inde)
			return
		end if
		
		Dlen=Dimen%boundary(inde+1)-Dimen%boundary(inde)
		if(inde.gt.dimen%dimSize) then 
			write(*,*) "ERROR in getSubDimIntName"
			return
		end if
		if(allocated(dimenintName)) then
			if(Dlen.ne.size(dimenintName)) then 
				deallocate(dimenintName)
				allocate(dimenintName(Dlen))
			end if
		else
			allocate(dimenintName(Dlen))
		end if
		j=1
		do i=Dimen%boundary(inde) + 1,Dimen%boundary(inde+1)
			dimenintName(j)=Dimen%DimIntName(i)
			j=j+1
		end do
	end subroutine
!input dimension [1,1,2,1,3,1,1,4,1]
!output dimenison [2,3,4]
! if input [1,1,1,1,1]
!  ouput [1] without name
	type(Dimension) function RNDimFun(dimen) Result(RNDim)
		type(Dimension),intent(in) :: Dimen
		integer::i,lenD,lenNewD
		integer,allocatable::Dimindex(:)
		if(.not.if_original_dim(dimen)) then
			write(*,*)"ERROR IN RNDim,in Diemnsion.f90"
			call error_stop()
		end if
		lenD=dimen%LenDimData
		allocate(Dimindex(lenD))
		lenNewD=0
		do i=1,lenD
			if(Dimen%Dimdata(i).ne.1)then
				lenNewD=lenNewD+1
				Dimindex(lenNewD)=i
			end if
		end do
		if(lenNewD.eq.0)then
			RNDim=(/1/)
			return
		end if
			
		allocate(RNDim%DimData(lenNewD))
		do i=1,lenNewD
			RNDim%DimData(i)=dimen%DimData(Dimindex(i))
		end do
		RNDim%Dimsize=lenNewD
		RNDim%LenDimData=lenNewD
		RNDim%sample_dimension_flag=Dimen%sample_dimension_flag
		if(.not.Dimen%sample_dimension_flag)then
			RNDim%boundarysize=RNDim%Dimsize+1
			allocate(RNDim%boundary(RNDim%boundarysize))
			do i=1,RNDim%boundarysize
				RNDim%boundary(i)=i-1
			end do
		end if
		RNDim%NameFlag=dimen%NameFlag
		if(dimen%NameFlag.eq.1)then
			allocate(RNDim%DimName(lenNewD))
			do i=1,lenNewD
				RNDim%DimName(i)=dimen%DimName(Dimindex(i))
			end do
		end if
		if(dimen%NameFlag.eq.2)then
			allocate(RNDim%DimintName(lenNewD))
			do i=1,lenNewD
				RNDim%DimintName(i)=dimen%DimintName(Dimindex(i))
			end do
		end if
		return
	end function
	type(Dimension) function RNDimFunint(dimen,notkillleg,killFlag) Result(RNDim)
		type(Dimension),intent(in) :: Dimen
		integer,intent(in)::notkillleg
		character(len=*),intent(in),optional::killFlag
		integer::i,lenD,lenNewD
		integer,allocatable::Dimindex(:)
		if(.not.if_original_dim(dimen)) then
			write(*,*)"ERROR IN RNDim,in Diemnsion.f90"
			call error_stop()
		end if
		lenD=dimen%LenDimData
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
					if((Dimen%Dimdata(i).ne.1).or.(i.eq.notkillleg))then
						lenNewD=lenNewD+1
						Dimindex(lenNewD)=i
					end if
				end do
			end if
		else
			do i=1,lenD
				if((Dimen%Dimdata(i).ne.1).or.(i.eq.notkillleg))then
					lenNewD=lenNewD+1
					Dimindex(lenNewD)=i
				end if
			end do
		end if
		if(lenNewD.eq.0)then
			RNDim=(/1/)
			return
		end if
			
		allocate(RNDim%DimData(lenNewD))
		do i=1,lenNewD
			RNDim%DimData(i)=dimen%DimData(Dimindex(i))
		end do
		RNDim%Dimsize=lenNewD
		RNDim%LenDimData=lenNewD
		RNDim%sample_dimension_flag=Dimen%sample_dimension_flag
		RNDim%NameFlag=dimen%NameFlag
		if(dimen%NameFlag.eq.1)then
			allocate(RNDim%DimName(lenNewD))
			do i=1,lenNewD
				RNDim%DimName(i)=dimen%DimName(Dimindex(i))
			end do
		end if
		if(dimen%NameFlag.eq.2)then
			allocate(RNDim%DimintName(lenNewD))
			do i=1,lenNewD
				RNDim%DimintName(i)=dimen%DimintName(Dimindex(i))
			end do
		end if
		return
	end function
	type(Dimension) function RNDimFunchar(dimen,notkillleg,killFlag) Result(RNDim)
		type(Dimension),intent(in) :: Dimen
		character(len=*),intent(in)::notkillleg
		character(len=*),intent(in),optional::killFlag
		integer::i,lenD,lenNewD
		integer,allocatable::Dimindex(:)
		if(.not.if_original_dim(dimen)) then
			write(*,*)"ERROR IN RNDim,in Diemnsion.f90"
			call error_stop()
		end if
		lenD=dimen%LenDimData
		allocate(Dimindex(lenD))
		lenNewD=0
		if(present(killFlag))then
			if(killFlag.equ.'kill')then
				do i=1,lenD
					if(Dimen%getName(i).nequ.notkillleg)then
						lenNewD=lenNewD+1
						Dimindex(lenNewD)=i
					end if
				end do
			else
				do i=1,lenD
					if((Dimen%Dimdata(i).ne.1).or.(Dimen%getName(i).equ.notkillleg))then
						lenNewD=lenNewD+1
						Dimindex(lenNewD)=i
					end if
				end do
			end if
		else
			do i=1,lenD
				if((Dimen%Dimdata(i).ne.1).or.(Dimen%getName(i).equ.notkillleg))then
					lenNewD=lenNewD+1
					Dimindex(lenNewD)=i
				end if
			end do
		end if
		if(lenNewD.eq.0)then
			RNDim=(/1/)
			return
		end if
			
		allocate(RNDim%DimData(lenNewD))
		do i=1,lenNewD
			RNDim%DimData(i)=dimen%DimData(Dimindex(i))
		end do
		RNDim%Dimsize=lenNewD
		RNDim%LenDimData=lenNewD
		RNDim%sample_dimension_flag=Dimen%sample_dimension_flag
		RNDim%NameFlag=dimen%NameFlag
		if(dimen%NameFlag.eq.1)then
			allocate(RNDim%DimName(lenNewD))
			do i=1,lenNewD
				RNDim%DimName(i)=dimen%DimName(Dimindex(i))
			end do
		end if
		if(dimen%NameFlag.eq.2)then
			allocate(RNDim%DimintName(lenNewD))
			do i=1,lenNewD
				RNDim%DimintName(i)=dimen%DimintName(Dimindex(i))
			end do
		end if
		return
	end function
	
	subroutine RNDimRoutine(Dimen)
		class(Dimension),intent(inout) :: Dimen
		Dimen=RNDimFun(dimen)
		return
	end subroutine
	subroutine RNDimRoutineint(Dimen,notkillleg,killFlag)
		class(Dimension),intent(inout) :: Dimen
		integer,intent(in)::notkillleg
		character(len=*),intent(in),optional::killFlag
		Dimen=RNDimFunint(dimen,notkillleg,killFlag)
		return
	end subroutine
	subroutine RNDimRoutinechar(Dimen,notkillleg,killFlag)
		class(Dimension),intent(inout) :: Dimen
		character(len=*),intent(in)::notkillleg
		character(len=*),intent(in),optional::killFlag
		Dimen=RNDimFunchar(dimen,notkillleg,killFlag)
		return
	end subroutine
		
!*******  function or subroutine for name   **************	
!	return the inde  dimension	,outpout in a type(dimenison)
	type(Dimension) function  getSubDim2(Dimen,inde) 
		type(Dimension),intent(in) :: Dimen
		integer,intent(in) :: inde
		integer::boundarysize,boundary(2),Dimsize
		integer,allocatable :: Dimdata(:)
		integer::i,j,Dlen
		call getSubDim(Dimen,inde,Dimdata)
		Dlen=size(Dimdata)
		if(Dlen.eq.1) then
			allocate(getSubDim2%DimData(Dlen))
			getSubDim2%DimData=DimData
			getSubDim2%lenDimData=Dlen
			getSubDim2%sample_dimension_flag=.true.
		else
			boundary(1)=0
			boundary(2)=Dlen
			boundarysize=2
			getSubDim2=Dimensioniniti(boundarysize,1,boundary,DimData)
		end if
		getSubDim2%NameFlag=Dimen%NameFlag
		if(getSubDim2%NameFlag.eq.0) return
		if(Dimen%nameFlag.eq.1)then
			call getSubDimName(Dimen,inde,getSubDim2%dimName)
			return
		end if
		if(Dimen%nameFlag.eq.2)then
			call getSubDimIntName(Dimen,inde,getSubDim2%dimIntName)
			return
		end if
		write(*,*)"ERROR in getSubDim2,nameFlag"
		call error_stop()
	end function
	
	
	type(Dimension) function  getSubDim3(Dimen,inde) 
		type(Dimension),intent(in) :: Dimen
		integer,intent(in) :: inde(2)
		integer::boundarysize,Dimsize
		integer::i,j,Dlen,ith1,ith2
		if(Dimen%sample_dimension_flag)then
			Dlen=inde(2)-inde(1)+1
			allocate(getSubDim3%DimData(Dlen))
			getSubDim3%DimData=Dimen%DimData(inde(1):inde(2))
			getSubDim3%lenDimData=Dlen
			getSubDim3%sample_dimension_flag=.true.
			getSubDim3%NameFlag=Dimen%NameFlag
			if(getSubDim3%NameFlag.eq.0) return
			if(Dimen%nameFlag.eq.1) then
				allocate(getSubDim3%DimName(Dlen))
				getSubDim3%DimName=Dimen%DimName(inde(1):inde(2))
				return
			end if
		else
			ith1=Dimen%boundary(inde(1))+1
			ith2=Dimen%boundary(inde(2)+1)
			Dlen=ith2-ith1+1
			if((inde(2)-inde(1)+1).eq.Dlen)then
				allocate(getSubDim3%DimData(Dlen))
				getSubDim3%DimData=Dimen%DimData(ith1:ith2)
				getSubDim3%lenDimData=Dlen
				getSubDim3%sample_dimension_flag=.true.
				getSubDim3%NameFlag=Dimen%NameFlag
			else
				getSubDim3%sample_dimension_flag=.false.
				ith1=Dimen%boundary(inde(1))+1
				ith2=Dimen%boundary(inde(2)+1)
				Dlen=ith2-ith1+1
				allocate(getSubDim3%DimData(Dlen))
				getSubDim3%lenDimData=Dlen
				getSubDim3%DimData=Dimen%DimData(ith1:ith2)
				Dlen=inde(2)-inde(1)+2
				allocate(getSubDim3%boundary(Dlen))
				getSubDim3%boundary=Dimen%boundary(inde(1):inde(2)+1)
				getSubDim3%boundary=getSubDim3%boundary-getSubDim3%boundary(1)
				getSubDim3%boundarysize=Dlen
				getSubDim3%Dimsize=getSubDim3%boundarysize-1
				getSubDim3%NameFlag=Dimen%NameFlag
			end if
			if(getSubDim3%NameFlag.eq.0) return
			if(Dimen%nameFlag.eq.1)then
				Dlen=ith2-ith1+1
				allocate(getSubDim3%DimName(Dlen))
				getSubDim3%DimName=Dimen%DimName(ith1:ith2)
				return
			end if
		end if
		write(*,*)"ERROR in getSubDim3,nameFlag,intname is nolonger use"
		call error_stop()
	end function
	
	subroutine getSubDim3_index_routine(outindex,Dimen,inde) 
		integer,intent(inout)::outindex(2)
		type(Dimension),intent(in) :: Dimen
		integer,intent(in) :: inde(2)
		if(Dimen%sample_dimension_flag)then
			outindex=inde
			return
		end if
		outindex(1)=Dimen%boundary(inde(1))+1
		outindex(2)=Dimen%boundary(inde(2)+1)
		return
	end subroutine
	
	type(Dimension) function  getSubDim2_name(Dimen,w)
		type(Dimension),intent(in) :: Dimen
		CHARACTER(len=*),intent(in)::w
		integer::inde
		inde=Nameorder(Dimen,w)
		getSubDim2_name=getSubDim2(Dimen,inde)
		return
	end function
	type(Dimension) function  getSubDim_intname(Dimen,TensorName,DimenName)
		type(Dimension),intent(in) :: Dimen
		integer,intent(in)::TensorName(:)
		integer,intent(in)::DimenName(:)
		integer::inde
		inde=Nameorder(Dimen,TensorName,DimenName)
		getSubDim_intname=getSubDim2(Dimen,inde)
		return
	end function
	type(Dimension) function  getSubDim2_intname2(Dimen,TensorDimName)
		type(Dimension),intent(in) :: Dimen
		integer,intent(in)::TensorDimName(:,:)
		integer::inde
		inde=Nameorder(Dimen,TensorDimName)
		getSubDim2_intname2=getSubDim2(Dimen,inde)
		return
	end function
!*******  function or subroutine for name   **************	
! The type of input are both dimension	
!If dimen have a name but Dimen2 do not or Dimen2 have but not dimen
!then the name for the one with no name will be ['0',0,i]
	type(Dimension) function  DimAdd(Dimen,Dimen2)
		type(Dimension),intent(in) :: Dimen,Dimen2
		integer,allocatable::boundary1(:),boundary2(:)
		integer::boundarysize1,boundarysize2,Dimsize1,Dimsize2
		integer::i,j,l1,l2,templen
		l1=dimen%LenDimData
		l2=dimen2%LenDimData
		if(l1.eq.0)then
			call writemess(' There is no Data in the first dimension when dim1 + dim2')
			call error_stop
		end if
		if(l2.eq.0)then
			call writemess(' There is no Data in the second dimension when dim1 + dim2')
			call error_stop
		end if
		DimAdd%lenDimData=l1+l2
		allocate(DimAdd%Dimdata(l1+l2))
		DimAdd%Dimdata(:l1)=Dimen%DimData(1:Dimen%lenDimData)
		DimAdd%Dimdata(l1+1:)=Dimen2%DimData(1:Dimen2%lenDimData)
		if( (Dimen%nameFlag.eq.0) .and. (Dimen2%nameFlag.eq.0) ) then
			DimAdd%nameFlag=0
			if(Dimen%sample_dimension_flag.and.Dimen2%sample_dimension_flag)then
				DimAdd%sample_dimension_flag=.true.
				return
			end if
		end if
		if((Dimen%nameFlag.eq.0) .and. (Dimen2%nameFlag.eq.1))then
			allocate(DimAdd%DimName(l1+l2))
			do i=1,l1
				DimAdd%DimName(i)=Nameinit('0',i)
			end do
			DimAdd%DimName(l1+1:)=Dimen2%DimName(1:l2)
			DimAdd%nameFlag=1
		end if
		if((Dimen%nameFlag.eq.0) .and. (Dimen2%nameFlag.eq.2))then
			allocate(DimAdd%DimIntName(l1+l2))
			do i=1,l1
				DimAdd%DimIntName(i)=Nameinit(0,-i)
			end do
			DimAdd%DimIntName(l1+1:)=Dimen2%DimIntName(1:l2)
			DimAdd%nameFlag=2
		end if
		if((Dimen%nameFlag.eq.1) .and. (Dimen2%nameFlag.eq.1))then
			allocate(DimAdd%DimName(l1+l2))
			DimAdd%DimName(1:l1)=Dimen%DimName(1:l1)
			DimAdd%DimName(l1+1:)=Dimen2%DimName(1:l2)
			DimAdd%nameFlag=1
		end if
		if((Dimen%nameFlag.eq.1) .and. (Dimen2%nameFlag.eq.0))then
			allocate(DimAdd%DimName(l1+l2))
			DimAdd%DimName(1:l1)=Dimen%DimName(1:l1)
			do i=l1+1,l1+l2
				DimAdd%DimName(i)=Nameinit('0',i)
			end do
			DimAdd%nameFlag=1
		end if
		if((Dimen%nameFlag.eq.2) .and. (Dimen2%nameFlag.eq.2))then
			allocate(DimAdd%DimIntName(l1+l2))
			DimAdd%DimIntName(1:l1)=Dimen%DimIntName(1:l1)
			DimAdd%DimIntName(l1+1:)=Dimen2%DimIntName(1:l2)
			DimAdd%nameFlag=2
		end if
		if((Dimen%nameFlag.eq.2) .and. (Dimen2%nameFlag.eq.0))then
			allocate(DimAdd%DimIntName(l1+l2))
			DimAdd%DimIntName(1:l1)=Dimen%DimIntName(1:l1)
			do i=l1+1,l1+l2
				DimAdd%DimIntName(i)=Nameinit(0,-i)
			end do
			DimAdd%nameFlag=2
		end if
		
		if(check_same_name_Flag)then
			if(DimAdd%nameFlag.ne.0)call check_same_name_in_dimension(DimAdd)
		end if
		
		if(Dimen%sample_dimension_flag.and.Dimen2%sample_dimension_flag)then
			DimAdd%sample_dimension_flag=.true.
			return
		end if
		DimAdd%sample_dimension_flag=.false.
		call outDimension_boundry(dimen,boundary1,boundarysize1,Dimsize1)
		call outDimension_boundry(dimen2,boundary2,boundarysize2,Dimsize2)
		DimAdd%boundarysize=boundarysize1+boundarysize2-1
		allocate(DimAdd%boundary(DimAdd%boundarysize))
		do i=1,boundarysize1
			DimAdd%boundary(i)=boundary1(i)
		end do
		templen=boundary1(boundarysize1)
		do i=1,boundarysize2-1
			DimAdd%boundary(i+boundarysize1)=boundary2(i+1)+templen
		end do
		DimAdd%Dimsize=Dimsize1+Dimsize2
		return
	end function
! The type of input are one dimension and the other vector	
	type(Dimension) function  DimAdd2(Dimen,Dimenvec)
		type(Dimension),intent(in) :: Dimen
		integer,intent(in) :: Dimenvec(:)
		integer,allocatable::boundary(:)
		integer::boundarysize,Dimsize
		integer::i,j,l1,l2,tempLen
		l1=dimen%LenDimData
		l2=size(Dimenvec)
		if(l1.eq.0)then
			call writemess(' There is no Data in the first dimension when dim1 + vec(:)')
			call error_stop
		end if
		if(l2.eq.0)then
			call writemess(' There is no Data in the array when dim1 + vec(:)')
			call error_stop
		end if
		DimAdd2%lenDimData=l1+l2
		allocate(DimAdd2%Dimdata(l1+l2))
		DimAdd2%Dimdata(:l1)=Dimen%DimData(1:dimen%lenDimData)
		DimAdd2%Dimdata(l1+1:)=Dimenvec
		DimAdd2%nameFlag=Dimen%nameFlag
		DimAdd2%sample_dimension_flag=Dimen%sample_dimension_flag
		if((DimAdd2%nameFlag.eq.0).and.(DimAdd2%sample_dimension_flag)) return
				
			
		if(Dimen%nameFlag.eq.1)then
			allocate(DimAdd2%DimName(l1+l2))
			DimAdd2%DimName(1:l1)=Dimen%DimName(1:l1)
			do i=l1+1,l1+l2
				DimAdd2%DimName(i)=Nameinit('0',i)
			end do
		end if
		
		if(Dimen%nameFlag.eq.2)then
			allocate(DimAdd2%DimIntName(l1+l2))
			DimAdd2%DimIntName(1:l1)=Dimen%DimIntName(1:l1)
			do i=l1+1,l1+l2
				DimAdd2%DimIntName(i)=Nameinit(0,-i)
			end do
		end if
		if(check_same_name_Flag)then
			if(DimAdd2%nameFlag.ne.0)call check_same_name_in_dimension(DimAdd2)
		end if
		if(DimAdd2%sample_dimension_flag) return
		
		call outDimension_boundry(dimen,boundary,boundarysize,Dimsize)
		
		DimAdd2%boundarysize=boundarysize+size(Dimenvec)
		allocate(DimAdd2%boundary(DimAdd2%boundarysize))
		do i=1,boundarysize
			DimAdd2%boundary(i)=boundary(i)
		end do
		tempLen=boundary(boundarysize)
		do i=1,size(Dimenvec)
			DimAdd2%boundary(i+boundarysize)=i+tempLen
		end do
		DimAdd2%Dimsize=Dimsize+size(Dimenvec)
		return
	end function
	
	type(Dimension) function  DimAdd3(Dimenvec,Dimen)
		type(Dimension),intent(in) :: Dimen
		integer,intent(in) :: Dimenvec(:)
		type(Dimension)::Dimenvec_
		Dimenvec_=Dimenvec
		DimAdd3=DimAdd(Dimenvec_,Dimen)
	end function
	
	subroutine check_same_name_in_dimension(dimen)
		type(Dimension),intent(in) :: Dimen
		integer::i,j,rank
		character(len=max_len_of_char_in_TData)::na
		rank=dimen%getRank()
		do i=1,rank
			na=dimen%getName(i)
			do j=i+1,rank
				if(na.equ.dimen%getName(j))then
					call writemess('There are two legs with a same name',-1)
					call writemess('The name in the dimension can not be the same',-1)
					call writemess('The names are',-1)
					call writemess(na)
					call writemess(dimen%getName(j))
					call dimen%print()
					open(unit=1234,file='_ERROR_DIMENSION'+output_ProID+'.err',status='replace')
					call dimen%print(1234)
					write(1234,*)na
					close(unit=1234)
					call error_stop
				end if
			end do
		end do
	end subroutine
	
	integer function getSubDimlen(Dimen,inde)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in) :: inde
		if(Dimen%sample_dimension_flag)then
			getSubDimlen=1
			return
		end if
		getSubDimlen=Dimen%boundary(inde+1)-Dimen%boundary(inde)
	end function
!		1,2,3,4,..inde,inde+1,...rank-->1,2,3,4,..(inde*inde+1*..inde+num),...rank     	
!		if inde+num>boundarysize,it will constract all the index that larger than inde
	type(Dimension) function DimConstract(dimen,inde,num)
		class(Dimension)::Dimen
		integer,intent(in):: inde,num
		integer :: i,boundarysize,Dimsize
		integer,allocatable::boundary(:)
		if(Dimen%outlenDimdata().le.0)then
			call DimConstract%empty()
			return
		end if
		if(dimen%sample_dimension_flag)then
			if(inde.gt.dimen%LenDimData) then 
				write(*,*) "ERROR in DimConstract"
				write(*,*)inde,dimen%LenDimData
				call error_stop()
			end if
			if((num.le.0).or.(inde.eq.dimen%LenDimData)) then
				DimConstract=dimen
				return
			end if
		else
			if(inde.gt.dimen%Dimsize) then 
				write(*,*) "ERROR in DimConstract"
				write(*,*)inde,dimen%Dimsize
				call error_stop()
			end if
			if((num.le.0).or.(inde.eq.dimen%Dimsize)) then
				DimConstract=dimen
				return
			end if
		end if
		call outDimension_boundry(dimen,boundary,boundarysize,Dimsize)
		DimConstract%sample_dimension_flag=.false.
		if(inde+num.ge.boundarysize) then
			DimConstract%boundarysize=inde+1
			DimConstract%Dimsize=inde
			allocate(DimConstract%boundary(DimConstract%boundarysize))
			DimConstract%boundary(:inde)=boundary(:inde)
			DimConstract%boundary(inde+1)=boundary(boundarysize)
			DimConstract%LenDimData=dimen%LenDimData
			allocate(DimConstract%DimData(DimConstract%LenDimData))
			DimConstract%DimData=dimen%DimData(1:dimen%LenDimData)
			DimConstract%Nameflag=dimen%Nameflag
			if(dimen%Nameflag.eq.0) return
			if(dimen%Nameflag.eq.1)then
				call copyNameAllocate(DimConstract%DimName,dimen%DimName(1:Dimen%lenDimData))
				return
			end if
			if(dimen%Nameflag.eq.2)then
				call copyIntNameAllocate(DimConstract%DimIntName,dimen%DimIntName(1:Dimen%lenDimData))
				return
			end if
			write(*,*)"ERROR in DimConstract, name flag"
			call error_stop()
		end if
		DimConstract%boundarysize=boundarysize-num
		DimConstract%Dimsize=Dimsize-num
		allocate(DimConstract%boundary(DimConstract%boundarysize))
		DimConstract%LenDimData=dimen%LenDimData
		allocate(DimConstract%DimData(dimen%LenDimData))
		DimConstract%boundary(:inde)=boundary(:inde)
		do i=inde+1,DimConstract%boundarysize
			DimConstract%boundary(i)=boundary(i+num)
		end do
		DimConstract%DimData=dimen%DimData(1:dimen%LenDimData)
		DimConstract%Nameflag=dimen%Nameflag
		if(dimen%Nameflag.eq.0) return
		if(dimen%Nameflag.eq.1)then
			call copyNameAllocate(DimConstract%DimName,dimen%DimName(1:Dimen%lenDimData))
			return
		end if
		if(dimen%Nameflag.eq.2)then
			call copyIntNameAllocate(DimConstract%DimIntName,dimen%DimIntName(1:Dimen%lenDimData))
			return
		end if
		write(*,*)"ERROR in DimConstract, name flag"
		call error_stop()
	end function
 !**************** fuse   ****************
!		combine two index of the Tensor,which is con_index and con_index+1
	type(Dimension) function fuseDimension_val(Dimen,con_index)result(fuseDim)
		integer,intent(in) :: con_index
		type(Dimension),intent(in) :: Dimen
		fuseDim=DimConstract(dimen,con_index,1)
		return
	end function
	type(Dimension) function fuseDimension_vec(dimen,vector)result(fuseDim)
		integer,intent(in) ::vector(2)
		type(Dimension),intent(in) :: dimen
		integer ::num
		num=vector(2)-vector(1)
		fuseDim=DimConstract(dimen,vector(1),num)
		return
	end function		
!		combine two index of the Tensor,which is index1 index1+1,..,index2-1,index2
!		if index2 larger than rank,the function will contract the index of index1 -> rank
	subroutine fuseDimension(dimen,index1,index2)
		class(Dimension),intent(inout) :: dimen
		integer,intent(in) :: index1
		integer,optional,intent(in)::index2
		type(Dimension) ::newdim
		integer ::num
		if(present(index2))then
			num=index2-index1
		else
			num=1
		end if
		dimen=DimConstract(dimen,index1,num)
		return
	end subroutine		    	

     	
!		(inde,inde+1,..midindde,midindde+1,...rank)-->(inde,inde+1,..midindde),(midindde+1,...rank)	     		
!		if (inde,inde+1 ..midindde),	midindde larger then next elmemt,do nothing
!		for example:	
!			D=[2,2,(3,4,5),2,(3,4)],	boundary=[0,1,2,5,6,8]
!		DimDecompose(D,3,2)=[2,2,(3,4),5,2,(3,4)],boundary=[0,1,2,4,5,6,8]
!		DimDecompose(D,3,4) will do nothing
	type(Dimension) function DimDecompose(dimen,inde,midindde)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in):: inde,midindde
		integer :: i
		if(Dimen%outlenDimdata().le.0)then
			call DimDecompose%empty()
			return
		end if
		if(dimen%sample_dimension_flag)then
			write(*,*)"ERROR in DimDecompose,1"
			call error_stop()
		end if
		if(inde.gt.dimen%DimSize) then
			write(*,*)"ERROR in DimDecompose"
			call error_stop()
		end if
		if(dimen%boundary(inde) +midindde .ge.dimen%boundary(inde+1)) then
			DimDecompose=dimen
			return
		end if
		DimDecompose%LenDimData=dimen%LenDimData
		allocate(DimDecompose%DimData(dimen%LenDimData))
		DimDecompose%DimData=dimen%DimData(1:dimen%LenDimData)
		DimDecompose%Dimsize=dimen%Dimsize+1
		if(DimDecompose%LenDimData.eq.DimDecompose%Dimsize) then !It is sample dimension
			DimDecompose%sample_dimension_flag=.true.
			DimDecompose%Dimsize=0
			DimDecompose%boundarysize=0
		else
			DimDecompose%sample_dimension_flag=.false.
			DimDecompose%boundarysize=dimen%boundarysize+1
			allocate(DimDecompose%boundary(DimDecompose%boundarysize))
			DimDecompose%boundary(:inde)=dimen%boundary(:inde)
			DimDecompose%boundary(inde+1)=dimen%boundary(inde) + midindde
			do i=inde+1,DimDecompose%boundarysize-1
				DimDecompose%boundary(i+1)=dimen%boundary(i)
			end do
		end if
		DimDecompose%Nameflag=dimen%Nameflag
		if(dimen%Nameflag.eq.0) return
		if(dimen%Nameflag.eq.1)then
			call copyNameAllocate(DimDecompose%DimName,dimen%DimName(1:Dimen%lenDimData))
			return
		end if
		if(dimen%Nameflag.eq.2)then
			call copyIntNameAllocate(DimDecompose%DimIntName,dimen%DimIntName(1:Dimen%lenDimData))
			return
		end if
		write(*,*)"ERROR in DimDecompose, name flag"
		call error_stop()
		end function
		
		
!		Will not store boundary as it is sample dimension	
!		If dimen is already the simple type, do nothing	
	type(Dimension) function DimDecomposeAll(dimen)
		class(Dimension),intent(in) :: Dimen
		integer :: i
		if(Dimen%outlenDimdata().le.0)then
			call DimDecomposeAll%empty()
			return
		end if
		if(dimen%sample_dimension_flag)then
			DimDecomposeAll=dimen
			return
		end if
     	DimDecomposeAll%sample_dimension_flag=.true.
     	DimDecomposeAll%LenDimData=dimen%LenDimData
     	allocate(DimDecomposeAll%DimData(dimen%LenDimData))
		DimDecomposeAll%DimData=dimen%DimData(1:dimen%LenDimData)
		DimDecomposeAll%boundarysize=0
		DimDecomposeAll%Dimsize=0
		DimDecomposeAll%Nameflag=dimen%Nameflag
		if(dimen%Nameflag.eq.0) return
		if(dimen%Nameflag.eq.1)then
			call copyNameAllocate(DimDecomposeAll%DimName,dimen%DimName(1:Dimen%lenDimData))
			return
		end if
		if(dimen%Nameflag.eq.2)then
			call copyIntNameAllocate(DimDecomposeAll%DimIntName,dimen%DimIntName(1:Dimen%lenDimData))
			return
		end if
		write(*,*)"ERROR in DimDecomposeAll, name flag"
		call error_stop()
	end function


!*****************  split  *****************
! decompose the de_index index of the Tensor into n(1),n(2)
!		for example the de_index index is (1,2,3,4,..inde,inde+1,...rank)
!		(1,2,3,4,..inde,inde+1,...rank)-->(1,2,3,4,..inde),(inde+1,...rank)		
!		if inde larger than rank ,the function will return no change	
	subroutine splitDimension(Dimen,de_index,inde)
		class(Dimension),intent(inout) :: Dimen
		integer,optional,intent(in) :: de_index
		integer,optional,intent(in)::inde
		if(present(de_index))then
			if(present(inde))then
				Dimen=DimDecompose(dimen,de_index,inde)
			else
				Dimen=DimDecompose(dimen,de_index,1)
			end if
		else
			Dimen=DimDecomposeAll(Dimen)
		end if
		return
	end subroutine	
	type(Dimension) function splitDimension2(dimen,vector)result(splitDimension)
		type(Dimension),intent(in) :: dimen
		integer,intent(in) ::vector(2)
		integer:: de_index,inde
		de_index=vector(1)
		inde=vector(2)
		splitDimension=DimDecompose(dimen,de_index,inde)
		return
	end function
			
	type(Dimension) function splitDimension3(dimen,de_index)result(splitDimension)
		type(Dimension),intent(in) :: dimen
		integer,intent(in) :: de_index
		splitDimension=DimDecompose(dimen,de_index,1)
		return
	end function
			
	type(Dimension) function splitDimensionAll(Dimen)result(splitDimension)
		type(Dimension),intent(in) :: Dimen
		splitDimension=DimDecomposeAll(Dimen)
		return
	end function	
	 
    
     			
	type(Dimension) function Dimpermute(dimen,v)
		class(Dimension),intent(in) :: Dimen
		integer,intent(in):: v(Dimen%dimSize)
		type(DimensionName),allocatable::dimenName(:)
		type(DimensionIntName),allocatable::dimenIntName(:)
		integer::i,datalen,subDlen,k
		integer,allocatable :: dimenVec(:)
		datalen=dimen%LenDimData
		if(datalen.eq.0) then
			write(*,*)"There is no data in Dimension"
			call error_stop()
		end if
		Dimpermute%LenDimData=datalen
		allocate(Dimpermute%DimData(datalen))
		if(dimen%NameFlag.eq.1)then
			allocate(Dimpermute%DimName(datalen))
		end if
		if(dimen%NameFlag.eq.2)then
			allocate(Dimpermute%DimIntName(datalen))
		end if
		Dimpermute%Nameflag=dimen%NameFlag
		Dimpermute%sample_dimension_flag=dimen%sample_dimension_flag
		if(dimen%sample_dimension_flag)then
			do i=1,datalen
				if(dimen%NameFlag.eq.1)then
					Dimpermute%DimName(i)=dimen%DimName(v(i))
				end if
				if(dimen%NameFlag.eq.2)then
					Dimpermute%DimIntName(i)=dimen%DimIntName(v(i))
				end if
				Dimpermute%DimData(i)=dimen%DimData(v(i))
			end do
			return
		end if
		
		Dimpermute%Dimsize=Dimen%Dimsize
		Dimpermute%boundarysize=Dimen%boundarysize
		allocate(Dimpermute%boundary(Dimpermute%boundarysize))
		Dimpermute%boundary(1)=0
		k=1
		do i=1,Dimen%dimSize
			subDlen=getsubDimlen(dimen,v(i))
			Dimpermute%boundary(i+1)=Dimpermute%boundary(i)+subDlen
			call getSubDim(Dimen,v(i),dimenVec)
			Dimpermute%DimData(k:subDlen+k-1)=dimenVec
			if(dimen%NameFlag.eq.1)then
				call getSubDimName(Dimen,v(i),dimenName)
				Dimpermute%DimName(k:subDlen+k-1)=dimenName
			end if
			if(dimen%NameFlag.eq.2)then
				call getSubDimIntName(Dimen,v(i),dimenIntName)
				Dimpermute%DimIntName(k:subDlen+k-1)=dimenIntName
			end if
			k=k+subDlen
		end do
	end function
! order is [ith,1,2,3...]
	subroutine  Dimpermute_forwards(outdim,dimen,ith)
		type(Dimension),intent(inout)::outdim
		type(Dimension),intent(in) :: Dimen
		integer,intent(in):: ith
		integer,pointer::order(:)
		integer::length,i
		call WorkingMemory%Check()
		length=DimSize(dimen)
		!allocate(order(length))
		call WorkingMemory%get_memory(order,length)
		order(1)=ith
		do i=2,ith
			order(i)=i-1
		end do
		do i=ith+1,length
			order(i)=i
		end do
		outdim=Dimpermute(dimen,order)
		call WorkingMemory%free()
		return
	end subroutine
! order is [1,2,3...,n,ith]
	subroutine Dimpermute_backwards(outdim,dimen,ith)
		type(Dimension),intent(inout)::outdim
		type(Dimension),intent(in) :: Dimen
		integer,intent(in):: ith
		integer,pointer::order(:)
		integer::length,i
		call WorkingMemory%Check()
		length=DimSize(dimen)
		!allocate(order(length))
		call WorkingMemory%get_memory(order,length)
		order(length)=ith
		do i=1,ith-1
			order(i)=i
		end do
		do i=ith,length-1
			order(i)=i+1
		end do
		outdim=Dimpermute(dimen,order)
		call WorkingMemory%free()
		return
	end subroutine
! oeder is [2,3,4,...,ith,1,ith+1,...]
	subroutine Dimpermute_forwards_index(outdim,dimen,ith)
		type(Dimension),intent(inout)::outdim
		type(Dimension),intent(in) :: Dimen
		integer,intent(in):: ith
		integer,pointer::order(:)
		integer::length,i
		call WorkingMemory%Check()
		length=DimSize(dimen)
		!allocate(order(length))
		call WorkingMemory%get_memory(order,length)
		order(ith)=1
		do i=1,ith-1
			order(i)=i+1
		end do
		do i=ith+1,length
			order(i)=i
		end do
		outdim=Dimpermute(dimen,order)
		call WorkingMemory%free()
		return
	end subroutine
	! oeder is [1,2,3,4,...,ith,n,ith+1,...,n-1]
	subroutine Dimpermute_backwards_index(outdim,dimen,ith)
		type(Dimension),intent(inout)::outdim
		type(Dimension),intent(in) :: Dimen
		integer,intent(in):: ith
		integer,pointer::order(:)
		integer::length,i
		call WorkingMemory%Check()
		length=DimSize(dimen)
		!allocate(order(length))
		call WorkingMemory%get_memory(order,length)
		order(ith)=length
		do i=1,ith-1
			order(i)=i
		end do
		do i=ith+1,length
			order(i)=i-1
		end do
		outdim=Dimpermute(dimen,order)
		call WorkingMemory%free()
		return
	end subroutine

	logical function equal_of_dim(dim1,dim2)
		type(Dimension),intent(in) :: dim1,dim2
		integer,allocatable :: dimenVec1(:)
		integer,allocatable :: dimenVec2(:)
		call copydimension(dimenVec1,dim1)
		call copydimension(dimenVec2,dim2)
		equal_of_dim=dimenVec1.equ.dimenVec2
	end function
!************* equal_of_array *************
	logical function equal_of_array_old(a,b)result(equal_of_array)
		integer,intent(in) :: a(:),b(:)
		integer :: la,lb,i,l
		la=size(a)
		lb=size(b)
		equal_of_array=.false.
		if(la.eq.lb) then
			l=count(abs(a-b).gt.0)
			if(l.eq.0) then
				equal_of_array=.true.
			end if
		end if
		return
	end function
	logical function if_original_dim(dimen)
		class(Dimension),intent(in)::dimen
		if_original_dim=.true.
		if(dimen%sample_dimension_flag) return
		if(dimen%LenDimData.eq.dimen%Dimsize) return	
		if_original_dim=.false.
		return
	end function
	
	
	subroutine allocateCheckName1(A,lenA)
		type(DimensionName),allocatable,intent(inout)::A(:)
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
	subroutine allocateCheckNameint(A,lenA)
		type(DimensionintName),allocatable,intent(inout)::A(:)
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
	
	logical function Same_Name_Order_forwards(dimen,charorder)result(Res)
		class(dimension),intent(in)::dimen
		character(len=*),intent(in)::charorder(:)
		integer::i,lenchar
		lenchar=size(charorder)
		Res=.true.
		do i=1,lenchar
			if(dimen%getName(i).nequ.charorder(i))then
				Res=.false.
				return
			end if
		end do
		return
	end function
	logical function Same_Name_Order_Backwards(dimen,charorder)result(Res)
		class(dimension),intent(in)::dimen
		character(len=*),intent(in)::charorder(:)
		integer::i,lenchar,rank,j
		lenchar=size(charorder)
		rank=dimen%getRank()
		Res=.true.
		j=rank
		do i=lenchar,1,-1
			if(dimen%getName(j).nequ.charorder(i))then
				Res=.false.
				return
			end if
			j=j-1
		end do
		return
	end function
		




!**********************************************************************
!**********************************************************************
!	the code below is for MPI
!**********************************************************************
	subroutine sent_dimensionName(Name1,Name2,ID1,ID2,ierr,MPIcommon)
		type(DimensionName),intent(in)::Name1
		type(DimensionName),intent(inout)::Name2
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
		if(ID1.eq.ID2) return !The some cpu, do nothing
		
		if((proID.ne.ID1).and.(proID.ne.ID2)) return!The proID do not sent or recv, return
		
!************************   TensorName   *************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Name1%TensorName,len_of_Name,MPI_CHARACTER,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Name2%TensorName,len_of_Name,MPI_CHARACTER,ID1,tag,mpi_comm,istatus,ierr)
		end if
!*************************   DimenName   **************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Name1%DimenName,len_of_Name,MPI_CHARACTER,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Name2%DimenName,len_of_Name,MPI_CHARACTER,ID1,tag,mpi_comm,istatus,ierr)
		end if
		return
	end subroutine
	
	subroutine BCAST_DimensionName(DimenName,ID,ierr,MPIcommon)
		type(DimensionName),intent(inout)::DimenName
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
!************************   TensorName   *************************************************		
		call MPI_BCAST(DimenName%TensorName,len_of_Name,MPI_CHARACTER,ID,mpi_comm,ierr)	
!*************************   DimenName   **************************************************		
		call MPI_BCAST(DimenName%DimenName,len_of_Name,MPI_CHARACTER,ID,mpi_comm,ierr)	
		return
	end subroutine
	
	subroutine sent_dimensionIntName(Name1,Name2,ID1,ID2,ierr,MPIcommon)
		type(dimensionIntName),intent(in)::Name1
		type(dimensionIntName),intent(inout)::Name2
		integer,intent(in)::ID1,ID2
		integer::ierr
		integer,optional,intent(in)::MPIcommon
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
		
		if(ID1.eq.ID2) return !The some cpu, do nothing
		
		if((proID.ne.ID1).and.(proID.ne.ID2)) return!The proID do not sent or recv, return
		
!************************   TensorName   *************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Name1%TensorName,len_of_intTensorName,MPI_integer,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Name2%TensorName,len_of_intTensorName,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
		end if
!*************************   DimenName   **************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Name1%DimenName,len_of_intDimenName,MPI_integer,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Name2%DimenName,len_of_intDimenName,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
		end if
		return
	end subroutine
	
	subroutine BCAST_dimensionIntName(DimenName,ID,ierr,MPIcommon)
		type(dimensionIntName),intent(inout)::DimenName
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
		
!************************   TensorName   *************************************************		
		call MPI_BCAST(DimenName%TensorName,len_of_intTensorName,MPI_integer,ID,mpi_comm,ierr)	
!*************************   DimenName   **************************************************		
		call MPI_BCAST(DimenName%DimenName,len_of_intDimenName,MPI_integer,ID,mpi_comm,ierr)	
		return
	end subroutine
	
	subroutine sent_Dimension(Dimen1,Dimen2,ID1,ID2,ierr,MPIcommon)
		type(Dimension),intent(in)::Dimen1
		type(Dimension),intent(inout)::Dimen2
		integer,intent(in)::ID1,ID2
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,lenDimData,istatus(MPI_STATUS_SIZE),i,nameflag,mpi_comm
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		tag=1
		lenDimData=0
		call mpi_comm_rank(mpi_comm,proID,ierr)
		call mpi_comm_size(mpi_comm,proNum,ierr )
		
		if(present(MPIcommon))then
			if((ID1.ge.proNum).or.(ID2.ge.proNum))return
		end if
		
		
		if(ID1.eq.ID2) return !The some cpu, do nothing
		
		if((proID.ne.ID1).and.(proID.ne.ID2)) return!The proID do not sent or recv, return
		
!*********************   LenDimData   ******************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Dimen1%LenDimData,1,MPI_integer,ID2,tag,mpi_comm,ierr)
			if(Dimen1%LenDimData.eq.0) return !No date,return
			lenDimData=Dimen1%LenDimData
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Dimen2%LenDimData,1,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
			if(Dimen2%LenDimData.eq.0) then
				call emptyDimension(Dimen2)
				return !No date,return
			else
				call allocateCheck(Dimen2%DimData,Dimen2%LenDimData)
			end if
			lenDimData=Dimen2%LenDimData
		end if
!******************   DimData  ***********************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Dimen1%DimData(1:dimen1%lenDimData),dimen1%lenDimData,MPI_integer,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Dimen2%DimData(1:dimen2%lenDimData),dimen2%lenDimData,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
		end if		
!******************   NameFlag  ***********************************************************
		if(proID.eq.ID1) then
			call mpi_send(Dimen1%NameFlag,1,MPI_integer,ID2,tag,mpi_comm,ierr)
			nameflag=Dimen1%NameFlag
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Dimen2%NameFlag,1,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
			nameflag=Dimen2%NameFlag
			if(Dimen2%NameFlag.eq.1) then
				call allocateCheckName(Dimen2%DimName,dimen2%lenDimData)
			end if
			if(Dimen2%NameFlag.eq.2) then
				call allocateCheckName(Dimen2%DimintName,dimen2%lenDimData)
			end if
		end if
!******************   dimensionName  ***********************************************************
		if(nameflag.eq.1)then
			do i=1,lenDimData
				call sent_dimensionName(Dimen1%DimName(i),Dimen2%DimName(i),ID1,ID2,ierr)
			end do
		else if(nameflag.eq.2)then
			do i=1,lenDimData
				call sent_dimensionIntName(Dimen1%DimIntName(i),Dimen2%DimIntName(i),ID1,ID2,ierr)
			end do
		end if
!******************   sample_dimension_flag  ***********************************************************
		if(proID.eq.ID1) then
			call mpi_send(Dimen1%sample_dimension_flag,1,MPI_logical,ID2,tag,mpi_comm,ierr)
			if(Dimen1%sample_dimension_flag)return
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Dimen2%sample_dimension_flag,1,MPI_logical,ID1,tag,mpi_comm,istatus,ierr)
			if(Dimen2%sample_dimension_flag)return
		end if		

!***********************    Dimsize   ****************************************************				
		if(proID.eq.ID1) then
			call mpi_send(Dimen1%Dimsize,1,MPI_integer,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Dimen2%Dimsize,1,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
		end if
!*********************   boundarysize   ******************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Dimen1%boundarysize,1,MPI_integer,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Dimen2%boundarysize,1,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
			call allocateCheck(Dimen2%boundary,Dimen2%boundarysize)
		end if
!*********************   boundary   *******************************************************		
		if(proID.eq.ID1) then
			call mpi_send(Dimen1%boundary(1:Dimen1%boundarysize),Dimen1%boundarysize,MPI_integer,ID2,tag,mpi_comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Dimen2%boundary(1:Dimen2%boundarysize),Dimen2%boundarysize,MPI_integer,ID1,tag,mpi_comm,istatus,ierr)
		end if

		return
	end subroutine
	
	subroutine BCAST_Dimension(Dimen1,ID,ierr,MPIcommon)
		type(Dimension),intent(inout)::Dimen1
		integer,intent(in)::ID
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,istatus(MPI_STATUS_SIZE),i,mpi_comm
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
		
!*******************   LenDimData   **********************************************************		
		call MPI_BCAST(Dimen1%LenDimData,1,MPI_integer,ID,mpi_comm,ierr)	
		if(Dimen1%LenDimData.eq.0) then
			call emptyDimension(Dimen1)
			return !No date,return
		end if
		if(proID.ne.ID) then
			call allocateCheck(Dimen1%DimData,Dimen1%LenDimData)
		end if
		call MPI_BCAST(Dimen1%DimData(1:dimen1%lenDimData),Dimen1%LenDimData,MPI_integer,ID,mpi_comm,ierr)	
!******************   NameFlag  ***********************************************************
		call MPI_BCAST(Dimen1%NameFlag,1,MPI_logical,ID,mpi_comm,ierr)	
!******************   dimensionName  ***********************************************************
		if(Dimen1%NameFlag.eq.1)then
			if(proID.ne.ID) then
				call allocateCheckName(Dimen1%DimName,Dimen1%LenDimData)
			end if
			do i=1,Dimen1%LenDimData
				call BCAST_DimensionName(Dimen1%DimName(i),ID,ierr)
			end do
		end if
		if(Dimen1%NameFlag.eq.2)then
			if(proID.ne.ID) then
				call allocateCheckName(Dimen1%DimIntName,Dimen1%LenDimData)
			end if
			do i=1,Dimen1%LenDimData
				call BCAST_DimensionIntName(Dimen1%DimIntName(i),ID,ierr)
			end do
		end if
!******************   sample_dimension_flag  ***********************************************************
		call MPI_BCAST(Dimen1%sample_dimension_flag,1,MPI_logical,ID,mpi_comm,ierr)
		if(Dimen1%sample_dimension_flag) return
!*******************   boundarysize   **********************************************************		
		call MPI_BCAST(Dimen1%boundarysize,1,MPI_integer,ID,mpi_comm,ierr)	
!*********************   boundary   ***************************************************		
		if(proID.ne.ID) then
			call allocateCheck(Dimen1%boundary,Dimen1%boundarysize)
		end if
		call MPI_BCAST(Dimen1%boundary(1:Dimen1%boundarysize),Dimen1%boundarysize,MPI_integer,ID,mpi_comm,ierr)		
!********************   Dimsize   ****************************************************	
		call MPI_BCAST(Dimen1%Dimsize,1,MPI_integer,ID,mpi_comm,ierr)	
		return
	end subroutine
end module
!****************************************************
!*************** ENF OF Dimension *************
!****************************************************























