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

!1.  allocateTensor
!2.  dallocateTensor
!3.  assignment to Tensor
!4.  assignment to array
module TData_module
	use Tools
	use mpi
	use Dimension_typede
	use print_module
	use element_module
	use modify_module
	use permutation_module
	use memory_type
	implicit none
	type(memory),private::WorkingMemory
	
! if DynamicClass = .true. then the type of Tensor_data can change in the code, it can be integer,real*4,real*8,compelx*8,complex*16,logical
! if DynamicClass = .false. then  the type of Tensor_data can not change in the code
!example 
!	T=(/1.1,,2.2/)
!         The value in T is real*4,they are  1.10000 and 2.20000
!  T=(/3,4/)
!    if  DynamicClass = .true.
!				 The value in T is integer,they are  3 and 4
!    if DynamicClass = .false. 
!				 The value in T is real*4,they are  3.000000 and 4.00000

	type TData
		private
		integer,allocatable:: iData(:)
		real(kind=4),allocatable::sData(:)
		real(kind=8),allocatable::dData(:)
		complex(kind=4),allocatable::cData(:)
		complex(kind=8),public,allocatable::zData(:)
		logical,allocatable:: ldata(:)
		character(len=max_len_of_char_in_TData),allocatable:: adata(:)
		integer::classType=default_classtype_in_Tensor 
						           !if classType=1, integer
						           !if classType=2, real(4)
						           !if classType=3, real(8)
						           !if classType=4, complex(4)
						           !if classType=5, complex(8)
						           !if classType=6, logical
						           !if classType=7, character
		integer::totalData=0
		logical::DynamicClass=default_DynamicClass_in_Tensor
		logical::flag=.false.
	contains	
		procedure,public::printdata =>Tprintdata
		procedure,public::writedata=>Tprintdata_file!output in external file
		procedure,public::getTotalData
		procedure,public::reset_total_Data_no_check
		procedure,public::getFlag
		procedure,public::getclassType
		procedure,public::getType
		procedure,public::Dynamic =>setDynamic
		procedure,public::Static=>setStatic
		procedure,public::ifDynamic
		procedure,public::isnan=>isnanTData
		procedure,public::isOverFlow=>isOverflowTData
		procedure,public::isZero=>isZeroTData
		procedure,public::TData_length
		generic,public::setType =>setclassType,setclassType2
		generic,public::external =>operation_on_TData,operation_on_TData2,operation_on_TData3,operation_on_TData4
		procedure::setclassType
		procedure::setclassType2
		procedure::operation_on_TData
		procedure::operation_on_TData2
		procedure::operation_on_TData3
		procedure::operation_on_TData4
		
		generic,public::pointer=>ipointer,spointer,dpointer,cpointer,zpointer,lpointer,apointer,&
										ipointer_,spointer_,dpointer_,cpointer_,zpointer_,lpointer_,apointer_
		generic,public::pointerDim=>ipointer2,spointer2,dpointer2,cpointer2,zpointer2,lpointer2,apointer2,&
							ipointer3,spointer3,dpointer3,cpointer3,zpointer3,lpointer3,apointer3,&
							ipointer4,spointer4,dpointer4,cpointer4,zpointer4,lpointer4,apointer4
		procedure::ipointer,spointer,dpointer,cpointer,zpointer,lpointer,apointer
		procedure::ipointer2,spointer2,dpointer2,cpointer2,zpointer2,lpointer2,apointer2
		procedure::ipointer3,spointer3,dpointer3,cpointer3,zpointer3,lpointer3,apointer3
		procedure::ipointer4,spointer4,dpointer4,cpointer4,zpointer4,lpointer4,apointer4
		procedure::ipointer_,spointer_,dpointer_,cpointer_,zpointer_,lpointer_,apointer_
		
!example of external soubroutine,where class1 and class2 can be integer,real,complex,logical,character
!	subroutine testroutine(indata,leng,outdata,lenoutdata)
!	integer::leng,lenoutdata
!	class1::indata(leng)
!	class2::outdata(lenoutdata)
!   ...
!   ...
!	return
!end subroutine
!
! or
!
!	subroutine testroutine(indata,leng)
!	integer::leng
!	class1::indata(leng)
!   ...
!   ...
!	return
!end subroutine!
	end type TData
	interface allocateData
		module procedure allocateData1
		module procedure allocateData2
		module procedure allocateData3
	end interface
	interface select_type_in_add_minu
		module procedure select_type_in_add_minu1
		module procedure select_type_in_add_minu2
	end interface

	interface modify_TData_class
		module procedure modify_TData_class_i
		module procedure modify_TData_class_s
		module procedure modify_TData_class_d
		module procedure modify_TData_class_c
		module procedure modify_TData_class_z
		module procedure modify_TData_class_l
		module procedure modify_TData_class_a
	end interface
	

	interface modify_Some_TData_class1
		module procedure modify_Some_TData_class1_i
		module procedure modify_Some_TData_class1_s
		module procedure modify_Some_TData_class1_d
		module procedure modify_Some_TData_class1_c
		module procedure modify_Some_TData_class1_z
		module procedure modify_Some_TData_class1_l
		module procedure modify_Some_TData_class1_a
	end interface
	
	interface modify_Some_TData_class2
		module procedure modify_Some_TData_class2_i
		module procedure modify_Some_TData_class2_s
		module procedure modify_Some_TData_class2_d
		module procedure modify_Some_TData_class2_c
		module procedure modify_Some_TData_class2_z
		module procedure modify_Some_TData_class2_l
		module procedure modify_Some_TData_class2_a
	end interface

	interface modify_Some_TData_class3
		module procedure modify_Some_TData_class3_i
		module procedure modify_Some_TData_class3_s
		module procedure modify_Some_TData_class3_d
		module procedure modify_Some_TData_class3_c
		module procedure modify_Some_TData_class3_z
		module procedure modify_Some_TData_class3_l
		module procedure modify_Some_TData_class3_a
	end interface

	interface modify_Some_TData_class4
		module procedure modify_Some_TData_class4_i
		module procedure modify_Some_TData_class4_s
		module procedure modify_Some_TData_class4_d
		module procedure modify_Some_TData_class4_c
		module procedure modify_Some_TData_class4_z
		module procedure modify_Some_TData_class4_l
		module procedure modify_Some_TData_class4_a
	end interface

	
	abstract INTERFACE
	  SUBROUTINE SVD_TData_routine_Type(indata,U,S,V,m,n,min_MN,cut,info)
	  	import :: TData
		integer,intent(in)::min_MN
		type(TData),intent(in)::indata
		type(TData),intent(inout)::U,V
		type(TData),intent(inout)::s
		integer,intent(in),optional::cut,info
	  END SUBROUTINE SVD_TData_routine_Type
	END INTERFACE
	procedure(SVD_TData_routine_Type),public,pointer::SVD_TData_routine=>SVD_TData_routine_SVD

	abstract INTERFACE
	  SUBROUTINE SVD_TData_routine_kill_Type(indata,U,S,V,m,n,min_MN,cut,info)
	  	import :: TData
		integer,intent(in)::min_MN
		type(TData),intent(inout)::indata
		type(TData),intent(inout)::U,V
		type(TData),intent(inout)::s
		integer,intent(in),optional::cut,info
	  END SUBROUTINE SVD_TData_routine_kill_Type
	END INTERFACE
	procedure(SVD_TData_routine_kill_Type),public,&
	    pointer::SVD_TData_routine_Kill_inData=>SVD_TData_routine_SVD_Kill_inData


	
	public::TData_memory_report,TData_memory_length,dellocate_TData_memory
contains
	subroutine reset_total_Data_no_check(T,num)
		class(TData),intent(inout) ::T
		integer,intent(in)::num
		T%totalData=num
		return
	end subroutine
	subroutine set_xgesdd_subroutine()
		call writemess(' use the ?gesdd in svd decomposition')
		SVD_TData_routine=>SVD_TData_routine_SDD
		return
	end subroutine
	subroutine set_xgesvd_subroutine()
		call writemess(' use the ?gesvd in svd decomposition')
		SVD_TData_routine=>SVD_TData_routine_SVD
		return
	end subroutine
	subroutine dellocate_TData_memory()
		call WorkingMemory%deallocate()
		return
	end subroutine
	subroutine TData_memory_report()
		call writemess('The memory used in TData are:')
		call WorkingMemory%print()
		call writemess(' ')
	end subroutine	
	subroutine TData_memory_length(length)
		integer,intent(inout)::length(:)
		call WorkingMemory%getlength(length)
	end subroutine	
	subroutine TData_length(T,inoutlen)
		class(TData),intent(in)::T
		integer,intent(inout)::inoutlen(:)
		if(size(inoutlen).lt.7)then
			call writemess('ERROR in get length of the memory')
			call error_stop
		end if
		inoutlen(1:7)=0
		if(allocated(T%idata))inoutlen(1)=size(T%idata)
		if(allocated(T%sdata))inoutlen(2)=size(T%sdata)
		if(allocated(T%ddata))inoutlen(3)=size(T%ddata)
		if(allocated(T%cdata))inoutlen(4)=size(T%cdata)
		if(allocated(T%zdata))inoutlen(5)=size(T%zdata)
		if(allocated(T%ldata))inoutlen(6)=size(T%ldata)
		if(allocated(T%adata))inoutlen(7)=size(T%adata)
	end subroutine	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                    allocateTensor    
!
!**************************************************************************************************************
!**************************************************************************************************************
	subroutine allocateTData(T,length)
		type(TData),intent(inout) ::T
		integer,intent(in)::length
		select case(T%classType)
			case(1)
				call allocateCheck(T%iData,length)
			case(2)
				call allocateCheck(T%sData,length)
			case(3)
				call allocateCheck(T%dData,length)
			case(4)
				call allocateCheck(T%cData,length)
			case(5)
				call allocateCheck(T%zData,length)
			case(6)
				call allocateCheck(T%lData,length)
			case(7)
				call allocateCheck(T%aData,length)
			case default 
				write(*,*)"no such class in allocateData"
				stop
		end 	select
		return
	end subroutine
	subroutine deallocate_memory(T,classtype)
		type(TData),intent(inout) ::T
		integer,intent(in)::classtype
		select case(classType)
			case(1)
				if(allocated(T%iData))deallocate(T%iData)
			case(2)
				if(allocated(T%sData))deallocate(T%sData)
			case(3)
				if(allocated(T%dData))deallocate(T%dData)
			case(4)
				if(allocated(T%cData))deallocate(T%cData)
			case(5)
				if(allocated(T%zData))deallocate(T%zData)
			case(6)
				if(allocated(T%lData))deallocate(T%lData)
			case(7)
				if(allocated(T%aData))deallocate(T%aData)
			case default 
				write(*,*)"no such class in allocateData"
				stop
		end 	select
		return
	end subroutine
!if input 	change_Dynamic, allocateData will change DynamicClass
	subroutine allocateData1(T,length,classtype,change_Dynamic)
		type(TData),intent(inout) ::T
		integer,intent(in)::length
		integer,intent(in)::classtype
		logical,optional,intent(in)::change_Dynamic
		if(T%DynamicClass)then
			if(deallocate_memory_flag)then
				if(T%classtype.ne.classtype) call deallocate_memory(T,T%classtype)
			end if
			T%classtype=classtype
		end if
		call allocateTData(T,length)
		T%totalData=length
		T%flag=.true.
		if(present(change_Dynamic))T%DynamicClass=.false.
		return
	end subroutine
	subroutine allocateData2(T,length,classtype_,change_Dynamic)
		type(TData),intent(inout) ::T
		integer,intent(in)::length
		character(len=*),intent(in)::classtype_
		logical,optional,intent(in)::change_Dynamic
		integer::	classtype
		classtype=select_data_type_char(classtype_)
		if(T%DynamicClass)then
			if(deallocate_memory_flag)then
				if(T%classtype.ne.classtype) call deallocate_memory(T,T%classtype)
			end if
			T%classtype=classtype
		end if
		call allocateTData(T,length)
		T%totalData=length
		T%flag=.true.
		if(present(change_Dynamic))T%DynamicClass=.false.
		return
	end subroutine
	subroutine allocateData3(T,length,change_Dynamic)
		type(TData),intent(inout) ::T
		integer,intent(in)::length
		logical,optional,intent(in)::change_Dynamic
		integer::	classtype
		call allocateTData(T,length)
		T%totalData=length
		T%flag=.true.
		if(present(change_Dynamic))T%DynamicClass=.false.
		return
	end subroutine
		

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

!*********************  getTotalData	 **********************
	integer function getTotalData(T)
		class(TData),intent(in) :: T
		getTotalData=T%totalData
	end function	
!*********************  getFlag	 **********************
	logical function getFlag(T)
		class(TData),intent(in) :: T
		getFlag=T%flag
	end function	
!*********************  getclassType	 **********************	
	character(len=20) function getclassType(T)
		class(TData),intent(in) :: T
		getclassType=out_data_class_type(T%classType)
		return
	end function
	integer function getType(T)
		class(TData),intent(in) :: T
		getType=T%classType
	end function	
!*********************  setclassType	 **********************	
	subroutine setclassType(T,classType_)
		class(TData),intent(inout) :: T
		character(len=*),intent(in)::classType_
		integer::	classtype
		if(T%flag)then
			write(*,*)" Can not set type to a allocated Tensor"
			call error_stop()
		end if
		classtype=select_data_type_char(classtype_)
		if(deallocate_memory_flag)then
			if(T%classtype.ne.classtype) call deallocate_memory(T,T%classtype)
		end if
		T%classType=classType
		T%DynamicClass=.false.
		return
	end subroutine
	subroutine setclassType2(T,classType)
		class(TData),intent(inout) :: T
		integer,intent(in)::classType
		if(T%flag)then
			write(*,*)" Can not set type to a allocated Tensor"
			call error_stop()
		end if
		if(deallocate_memory_flag)then
			if(T%classtype.ne.classtype) call deallocate_memory(T,T%classtype)
		end if
		T%classType=classType
		T%DynamicClass=.false.
		return
	end subroutine
!******************  setDynamic  ***********
	subroutine setDynamic(T)
		class(TData),intent(inout) :: T
		T%DynamicClass=.true.
		return
	end subroutine
!******************  setStatic  ***********
	subroutine setStatic(T)
		class(TData),intent(inout) :: T
		T%DynamicClass=.false.
		return
	end subroutine
!*******************  ifDynamic   *********
	logical function ifDynamic(T)
		class(TData),intent(in) :: T
		ifDynamic=T%DynamicClass
		return
	end function
	
	subroutine cleanTData(T)
		class(TData),intent(inout)::T
		if(allocated(T%iData))deallocate(T%iData)
		if(allocated(T%sData))deallocate(T%sData)
		if(allocated(T%dData))deallocate(T%dData)
		if(allocated(T%cData))deallocate(T%cData)
		if(allocated(T%zData))deallocate(T%zData)
		if(allocated(T%lData))deallocate(T%lData)
		if(allocated(T%aData))deallocate(T%aData)
		T%classType=default_classtype_in_Tensor 
		T%totalData=0
		T%DynamicClass=default_DynamicClass_in_Tensor
		T%flag=.false.
		return
	end subroutine
	subroutine emptyTData(T)
		class(TData),intent(inout)::T
		if(deallocate_memory_flag)then
			call cleanTData(T)
			return
		end if
		T%classType=default_classtype_in_Tensor 
		T%totalData=0
		T%DynamicClass=default_DynamicClass_in_Tensor
		T%flag=.false.
		return
	end subroutine
			

!**************************************************************************************************************
!**************************************************************************************************************
!
!                                    assignment    
!
!**************************************************************************************************************
!**************************************************************************************************************
	subroutine assignmentTData_routine(T1,T2)
		class(TData),intent(inout) ::T1
		class(TData),intent(in) :: T2
		integer::flag
		flag=10*T1%classtype+T2%classtype
		if(T2%totalData.le.LAPACK_LENGTH)flag=-flag
		select case(flag)
			case (11)!int=int
				T1%idata=T2%idata(1:T2%totalData)
			case (12)!int=real4
				T1%idata=T2%sdata(1:T2%totalData)
			case (13)!int=real8
				T1%idata=T2%ddata(1:T2%totalData)
			case (14)!int=compelx(kind=4)
				T1%idata=T2%cdata(1:T2%totalData)
			case (15)!int=compelx(kind=8)
				T1%idata=T2%zdata(1:T2%totalData)
			case (-11)!int=int
				T1%idata=T2%idata(1:T2%totalData)
			case (-12)!int=real4
				T1%idata=T2%sdata(1:T2%totalData)
			case (-13)!int=real8
				T1%idata=T2%ddata(1:T2%totalData)
			case (-14)!int=compelx(kind=4)
				T1%idata=T2%cdata(1:T2%totalData)
			case (-15)!int=compelx(kind=8)
				T1%idata=T2%zdata(1:T2%totalData)
			case (16)!int=logical ,error
				write(*,*)"ERROR in assignment,int=logical"
				call error_stop
			case (17)!int=character error
				write(*,*)"ERROR in assignment,int=character"
				call error_stop
				
			case (21)!real(kind=4)=int
				call scopy (T2%TotalData, real(T2%idata(1:T2%TotalData),kind=4), 1, T1%sdata, 1)
			case (22)!real(kind=4)=real4
				call scopy (T2%TotalData,      T2%sdata        , 1, T1%sdata, 1)
			case (23)!real(kind=4)=real8
				call scopy (T2%TotalData, real(T2%ddata(1:T2%TotalData),kind=4), 1, T1%sdata, 1)
			case (24)!real(kind=4)=compelx(kind=4)
				call scopy (T2%TotalData, real(T2%cdata(1:T2%TotalData),kind=4), 1, T1%sdata, 1)
			case (25)!real(kind=4)=compelx(kind=8)
				call scopy (T2%TotalData, real(T2%zdata(1:T2%TotalData),kind=4), 1, T1%sdata, 1)
			case (26)!real(kind=4)=logical ,error
				write(*,*)"ERROR in assignment,real(kind=4)=logical"
				call error_stop
			case (27)!real(kind=4)=character error
				write(*,*)"ERROR in assignment,real(kind=4)=character"
				call error_stop	
			case (-21)!real(kind=4)=int
				T1%sdata=T2%idata(1:T2%TotalData)
			case (-22)!real(kind=4)=real4
				T1%sdata=T2%sdata(1:T2%TotalData)
			case (-23)!real(kind=4)=real8
				T1%sdata=T2%ddata(1:T2%TotalData)
			case (-24)!real(kind=4)=compelx(kind=4)
				T1%sdata=T2%cdata(1:T2%TotalData)
			case (-25)!real(kind=4)=compelx(kind=8)
				T1%sdata=T2%zdata(1:T2%TotalData)
			case (-26)!real(kind=4)=logical ,error
				write(*,*)"ERROR in assignment,real(kind=4)=logical"
				call error_stop
			case (-27)!real(kind=4)=character error
				write(*,*)"ERROR in assignment,real(kind=4)=character"
				call error_stop	
				
			case (31)!real(kind=8)=int
				call dcopy (T2%TotalData, dble(T2%idata(1:T2%TotalData)), 1, T1%ddata, 1)
			case (32)!real(kind=8)=real4
				call dcopy (T2%TotalData, dble(T2%sdata(1:T2%TotalData)), 1, T1%ddata, 1)
			case (33)!real(kind=8)=real8
				call dcopy (T2%TotalData,      T2%ddata                 , 1, T1%ddata, 1)
			case (34)!real(kind=8)=compelx(kind=4)
				call dcopy (T2%TotalData, dble(T2%cdata(1:T2%TotalData)), 1, T1%ddata, 1)
			case (35)!real(kind=8)=compelx(kind=8)
				call dcopy (T2%TotalData, dble(T2%zdata(1:T2%TotalData)), 1, T1%ddata, 1)
			case (36)!real(kind=8)=logical ,error
				write(*,*)"ERROR in assignment,real(kind=8)=logical"
				call error_stop
			case (37)!real(kind=8)=character error
				write(*,*)"ERROR in assignment,real(kind=8)=character"
				call error_stop	
			case (-31)!real(kind=8)=int
				T1%ddata=T2%idata(1:T2%TotalData)
			case (-32)!real(kind=8)=real4
				T1%ddata=T2%sdata(1:T2%TotalData)
			case (-33)!real(kind=8)=real8
				T1%ddata=T2%ddata(1:T2%TotalData)
			case (-34)!real(kind=8)=compelx(kind=4)
				T1%ddata=T2%cdata(1:T2%TotalData)
			case (-35)!real(kind=8)=compelx(kind=8)
				T1%ddata=T2%zdata(1:T2%TotalData)
			case (-36)!real(kind=8)=logical ,error
				write(*,*)"ERROR in assignment,real(kind=8)=logical"
				call error_stop
			case (-37)!real(kind=8)=character error
				write(*,*)"ERROR in assignment,real(kind=8)=character"
				call error_stop	
				
			case (41)!compelx(kind=4)=int
				call ccopy (T2%TotalData, cmplx(T2%idata(1:T2%TotalData),kind=4), 1, T1%cdata, 1)
			case (42)!compelx(kind=4)=real4
				call ccopy (T2%TotalData, cmplx(T2%sdata(1:T2%TotalData),kind=4), 1, T1%cdata, 1)
			case (43)!compelx(kind=4)=real8
				call ccopy (T2%TotalData, cmplx(T2%ddata(1:T2%TotalData),kind=4), 1, T1%cdata, 1)
			case (44)!compelx(kind=4)=compelx(kind=4)
				call ccopy (T2%TotalData,       T2%cdata                        , 1, T1%cdata, 1)
			case (45)!compelx(kind=4)=compelx(kind=8)
				call ccopy (T2%TotalData, cmplx(T2%zdata(1:T2%TotalData),kind=4), 1, T1%cdata, 1)
			case (46)!compelx(kind=4)=logical ,error
				write(*,*)"ERROR in assignment,compelx(kind=4)=logical"
				call error_stop
			case (47)!compelx(kind=4)=character error
				write(*,*)"ERROR in assignment,compelx(kind=4)=character"
				call error_stop	
			case (-41)!compelx(kind=4)=int
				T1%cdata=T2%idata(1:T2%TotalData)
			case (-42)!compelx(kind=4)=real4
				T1%cdata=T2%sdata(1:T2%TotalData)
			case (-43)!compelx(kind=4)=real8
				T1%cdata=T2%ddata(1:T2%TotalData)
			case (-44)!compelx(kind=4)=compelx(kind=4)
				T1%cdata=T2%cdata(1:T2%TotalData)
			case (-45)!compelx(kind=4)=compelx(kind=8)
				T1%cdata=T2%zdata(1:T2%TotalData)
			case (-46)!compelx(kind=4)=logical ,error
				write(*,*)"ERROR in assignment,compelx(kind=4)=logical"
				call error_stop
			case (-47)!compelx(kind=4)=character error
				write(*,*)"ERROR in assignment,compelx(kind=4)=character"
				call error_stop	
				
			case (51)!compelx(kind=8)=int
				call zcopy (T2%TotalData, dcmplx(T2%idata(1:T2%TotalData)), 1, T1%zdata, 1)
			case (52)!compelx(kind=8)=real4
				call zcopy (T2%TotalData, dcmplx(T2%sdata(1:T2%TotalData)), 1, T1%zdata, 1)
			case (53)!compelx(kind=8)=real8
				call zcopy (T2%TotalData, dcmplx(T2%ddata(1:T2%TotalData)), 1, T1%zdata, 1)
			case (54)!compelx(kind=8)=compelx(kind=4)
				call zcopy (T2%TotalData, dcmplx(T2%cdata(1:T2%TotalData)), 1, T1%zdata, 1)
			case (55)!compelx(kind=8)=compelx(kind=8)
				call zcopy (T2%TotalData, T2%zdata                        , 1, T1%zdata, 1)
			case (56)!compelx(kind=8)=logical ,error
				write(*,*)"ERROR in assignment,compelx(kind=8)=logical"
				call error_stop
			case (57)!compelx(kind=8)=character error
				write(*,*)"ERROR in assignment,compelx(kind=8)=character"
				call error_stop	
			case (-51)!compelx(kind=8)=int
				T1%zdata=T2%idata(1:T2%TotalData)
			case (-52)!compelx(kind=8)=real4
				T1%zdata=T2%sdata(1:T2%TotalData)
			case (-53)!compelx(kind=8)=real8
				T1%zdata=T2%ddata(1:T2%TotalData)
			case (-54)!compelx(kind=8)=compelx(kind=4)
				T1%zdata=T2%cdata(1:T2%TotalData)
			case (-55)!compelx(kind=8)=compelx(kind=8)
				T1%zdata=T2%zdata(1:T2%TotalData)
			case (-56)!compelx(kind=8)=logical ,error
				write(*,*)"ERROR in assignment,compelx(kind=8)=logical"
				call error_stop
			case (-57)!compelx(kind=8)=character error
				write(*,*)"ERROR in assignment,compelx(kind=8)=character"
				call error_stop	
				
			case (66)!logical=logical 
				T1%ldata=T2%ldata(1:T2%totalData)
				
			case (71)!character=int
				T1%adata=T2%idata(1:T2%totalData)
			case (72)!character=real4
				T1%adata=T2%sdata(1:T2%totalData)
			case (73)!character)=real8
				T1%adata=T2%ddata(1:T2%totalData)
			case (74)!character=real4
				T1%adata=T2%cdata(1:T2%totalData)
			case (75)!character)=real8
				T1%adata=T2%zdata(1:T2%totalData)
			case (76)!character=logical ,error
				T1%adata=T2%ldata(1:T2%totalData)
			case (77)!character=character 
				T1%adata=T2%adata(1:T2%totalData)
			case (-71)!character=int
				T1%adata=T2%idata(1:T2%totalData)
			case (-72)!character=real4
				T1%adata=T2%sdata(1:T2%totalData)
			case (-73)!character)=real8
				T1%adata=T2%ddata(1:T2%totalData)
			case (-74)!character=real4
				T1%adata=T2%cdata(1:T2%totalData)
			case (-75)!character)=real8
				T1%adata=T2%zdata(1:T2%totalData)
			case (-76)!character=logical ,error
				T1%adata=T2%ldata(1:T2%totalData)
			case (-77)!character=character 
				T1%adata=T2%adata(1:T2%totalData)
			case default
				write(*,*)"error in assignment"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignmentTData(T,T2)!T=T2
		type(TData),intent(inout) ::T
		type(TData),intent(in) :: T2
		if(.not.T2%flag)then
			call emptyTData(T)
			return
		end if
		call assignmentTData_routine(T,T2)
		return
	end subroutine
	
	subroutine assignmentSomeTData_T(T1,T,length,i1,i2)!T(i1:i2)=vec()
		type(TData),intent(inout) ::T1
		integer,intent(in)::i1,i2,length
		type(TData),intent(in)::T
		integer :: ivec(T%TotalData)
		real*4 :: svec(T%TotalData)
		real*8 :: dvec(T%TotalData)
		complex*8 :: cvec(T%TotalData)
		complex*16 :: zvec(T%TotalData)
		logical :: lvec(T%TotalData)
		character(len=max_len_of_char_in_TData) :: avec(T%TotalData)
		select case(T1%classType)
			case (1)
				call assignment_int_Tdata(ivec,T,T%TotalData)
				T1%idata(i1:i2)=ivec
			case (2)
				call assignment_real4_Tdata(svec,T,T%TotalData)
				call scopy (length, real(svec,kind=4), 1, T1%sdata(i1:i2), 1)
			case (3)
				call assignment_real8_Tdata(dvec,T,T%TotalData)
				call dcopy (length, dble(dvec), 1, T1%ddata(i1:i2), 1)
			case (4)
				call assignment_com4_Tdata(cvec,T,T%TotalData)
				call ccopy (length, cmplx(cvec,kind=4), 1, T1%cdata(i1:i2), 1)
			case (5)
				call assignment_com8_Tdata(zvec,T,T%TotalData)
				call zcopy (length, dcmplx(zvec), 1, T1%zdata(i1:i2), 1)
			case (6)
				call assignment_logi_Tdata(lvec,T,T%TotalData)
				T1%ldata(i1:i2)=lvec
			case (7)
				call assignment_char_Tdata(avec,T,T%TotalData)
				T1%adata(i1:i2)=avec
		end select
		return
	end subroutine
	
	subroutine assignmentTData_int(T1,vec,length)!T=vec
		type(TData),intent(inout) ::T1
		integer,intent(in)::length
		integer,intent(in) :: vec(length)
		select case(T1%classType)
			case (1)
				T1%idata=vec
			case (2)
				call scopy (length, real(vec,kind=4), 1, T1%sdata, 1)
			case (3)
				call dcopy (length, dble(vec), 1, T1%ddata, 1)
			case (4)
				call ccopy (length, cmplx(vec,kind=4), 1, T1%cdata, 1)
			case (5)
				call zcopy (length, dcmplx(vec), 1, T1%zdata, 1)
			case (6)
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
			case (7)
				T1%adata=vec
		end select
		return
	end subroutine
	subroutine assignmentSomeTData_int(T1,vec,length,i1,i2)!T(i1:i2)=vec()
		type(TData),intent(inout) ::T1
		integer,intent(in)::i1,i2,length
		integer,intent(in) :: vec(length)
		select case(T1%classType)
			case (1)
				T1%idata(i1:i2)=vec
			case (2)
				call scopy (length, real(vec,kind=4), 1, T1%sdata(i1:i2), 1)
			case (3)
				call dcopy (length, dble(vec), 1, T1%ddata(i1:i2), 1)
			case (4)
				call ccopy (length, cmplx(vec,kind=4), 1, T1%cdata(i1:i2), 1)
			case (5)
				call zcopy (length, dcmplx(vec), 1, T1%zdata(i1:i2), 1)
			case (6)
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
			case (7)
				T1%adata(i1:i2)=vec
		end select
		return
	end subroutine
	subroutine assignmentTData_real4(T1,vec,length)!T=vec
		type(TData),intent(inout) ::T1
		integer,intent(in)::length
		real(kind=4),intent(in) :: vec(length)
		select case(T1%classType)
			case (1)
				T1%idata=vec
			case (2)
				call scopy (length, vec, 1, T1%sdata, 1)
			case (3)
				call dcopy (length, dble(vec), 1, T1%ddata, 1)
			case (4)
				call ccopy (length, cmplx(vec,kind=4), 1, T1%cdata, 1)
			case (5)
				call zcopy (length, dcmplx(vec), 1, T1%zdata, 1)
			case (6)
				write(*,*)"ERROR in assignmentTData_int,logical=real"
				call error_stop
			case (7)
				T1%adata=vec
		end select
		return
	end subroutine
	subroutine assignmentSomeTData_real4(T1,vec,length,i1,i2)!T(i1:i2)=vec()
		type(TData),intent(inout) ::T1
		integer,intent(in)::length,i1,i2
		real(kind=4),intent(in) :: vec(length)
		select case(T1%classType)
			case (1)
				T1%idata(i1:i2)=vec
			case (2)
				call scopy (length, vec, 1, T1%sdata(i1:i2), 1)
			case (3)
				call dcopy (length, dble(vec), 1, T1%ddata(i1:i2), 1)
			case (4)
				call ccopy (length, cmplx(vec,kind=4), 1, T1%cdata(i1:i2), 1)
			case (5)
				call zcopy (length, dcmplx(vec), 1, T1%zdata(i1:i2), 1)
			case (6)
				write(*,*)"ERROR in assignmentTData_int,logical=real"
				call error_stop
			case (7)
				T1%adata(i1:i2)=vec
		end select
		return
	end subroutine
	subroutine assignmentTData_real8(T1,vec,length)!T=vec
		type(TData),intent(inout) ::T1
		integer,intent(in)::length
		real(kind=8),intent(in) :: vec(length)
		select case(T1%classType)
			case (1)
				T1%idata=vec
			case (2)
				call scopy (length, real(vec,kind=4), 1, T1%sdata, 1)
			case (3)
				call dcopy (length, vec, 1, T1%ddata, 1)
			case (4)
				call ccopy (length, cmplx(vec,kind=4), 1, T1%cdata, 1)
			case (5)
				call zcopy (length, dcmplx(vec), 1, T1%zdata, 1)
			case (6)
				write(*,*)"ERROR in assignmentTData_int,logical=real"
				call error_stop
			case (7)
				T1%adata=vec
		end select
		return
	end subroutine
	subroutine assignmentSomeTData_real8(T1,vec,length,i1,i2)!T(i1:i2)=vec()
		type(TData),intent(inout) ::T1
		integer,intent(in)::length,i1,i2
		real(kind=8),intent(in) :: vec(length)
		select case(T1%classType)
			case (1)
				T1%idata(i1:i2)=vec
			case (2)
				call scopy (length, real(vec,kind=4), 1, T1%sdata(i1:i2), 1)
			case (3)
				call dcopy (length, vec, 1, T1%ddata(i1:i2), 1)
			case (4)
				call ccopy (length, cmplx(vec,kind=4), 1, T1%cdata(i1:i2), 1)
			case (5)
				call zcopy (length, dcmplx(vec), 1, T1%zdata(i1:i2), 1)
			case (6)
				write(*,*)"ERROR in assignmentTData_int,logical=real"
				call error_stop
			case (7)
				T1%adata(i1:i2)=vec
		end select
		return
	end subroutine
	subroutine assignmentTData_com4(T1,vec,length)!T=vec
		type(TData),intent(inout) ::T1
		integer,intent(in)::length
		complex(kind=4),intent(in) :: vec(length)
		select case(T1%classType)
			case (1)
				T1%idata=vec
			case (2)
				call scopy (length, real(vec,kind=4), 1, T1%sdata, 1)
			case (3)
				call dcopy (length, dble(vec), 1, T1%ddata, 1)
			case (4)
				call ccopy (length, vec, 1, T1%cdata, 1)
			case (5)
				call zcopy (length, dcmplx(vec), 1, T1%zdata, 1)
			case (6)
				write(*,*)"ERROR in assignmentTData_int,logical=complex"
				call error_stop
			case (7)
				T1%adata=vec
		end select
		return
	end subroutine
	subroutine assignmentSomeTData_com4(T1,vec,length,i1,i2)!T(i1:i2)=vec()
		type(TData),intent(inout) ::T1
		integer,intent(in)::length,i1,i2
		complex(kind=4),intent(in) :: vec(length)
		select case(T1%classType)
			case (1)
				T1%idata(i1:i2)=vec
			case (2)
				call scopy (length, real(vec,kind=4), 1, T1%sdata(i1:i2), 1)
			case (3)
				call dcopy (length, dble(vec), 1, T1%ddata(i1:i2), 1)
			case (4)
				call ccopy (length, vec, 1, T1%cdata(i1:i2), 1)
			case (5)
				call zcopy (length, dcmplx(vec), 1, T1%zdata(i1:i2), 1)
			case (6)
				write(*,*)"ERROR in assignmentTData_int,logical=complex"
				call error_stop
			case (7)
				T1%adata(i1:i2)=vec
		end select
		return
	end subroutine
	subroutine assignmentTData_com8(T1,vec,length)!T=vec
		type(TData),intent(inout) ::T1
		integer,intent(in)::length
		complex(kind=8),intent(in) :: vec(length)
		select case(T1%classType)
			case (1)
				T1%idata=vec
			case (2)
				call scopy (length, real(vec,kind=4), 1, T1%sdata, 1)
			case (3)
				call dcopy (length, dble(vec), 1, T1%ddata, 1)
			case (4)
				call ccopy (length, cmplx(vec,kind=4), 1, T1%cdata, 1)
			case (5)
				call zcopy (length, vec, 1, T1%zdata, 1)
			case (6)
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
			case (7)
				T1%adata=vec
		end select
		return
	end subroutine	
	subroutine assignmentSomeTData_com8(T1,vec,length,i1,i2)!T(i1:i2)=vec()
		type(TData),intent(inout) ::T1
		integer,intent(in)::length,i1,i2
		complex(kind=8),intent(in) :: vec(length)
		select case(T1%classType)
			case (1)
				T1%idata(i1:i2)=vec
			case (2)
				call scopy (length, real(vec,kind=4), 1, T1%sdata(i1:i2), 1)
			case (3)
				call dcopy (length, dble(vec), 1, T1%ddata(i1:i2), 1)
			case (4)
				call ccopy (length, cmplx(vec,kind=4), 1, T1%cdata(i1:i2), 1)
			case (5)
				call zcopy (length, vec, 1, T1%zdata(i1:i2), 1)
			case (6)
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
			case (7)
				T1%adata(i1:i2)=vec
		end select
		return
	end subroutine
	subroutine assignmentTData_logi(T1,vec,length)!T=vec
		type(TData),intent(inout) ::T1
		integer,intent(in)::length
		logical,intent(in) :: vec(length)
		select case(T1%classType)
			case (6)
				T1%ldata=vec
			case (7)
				T1%adata=vec
			case default
				write(*,*)"ERROR in assignmentTData_logi"
				call error_stop
		end select
		return
	end subroutine	
	subroutine assignmentSomeTData_logi(T1,vec,length,i1,i2)!T(i1:i2)=vec()
		type(TData),intent(inout) ::T1
		integer,intent(in)::length,i1,i2
		logical,intent(in) :: vec(length)
		select case(T1%classType)
			case (6)
				T1%ldata(i1:i2)=vec
			case (7)
				T1%adata(i1:i2)=vec
			case default
				write(*,*)"ERROR in assignmentTData_logi"
				call error_stop
		end select
		return
	end subroutine	
	subroutine assignmentTData_char(T1,vec,length)!T=vec
		type(TData),intent(inout) ::T1
		integer,intent(in)::length
		character(len=*),intent(in) :: vec(length)
		select case(T1%classType)
			case (7)
				T1%adata=vec
			case default
				write(*,*)"ERROR in assignmentTData_logi"
				call error_stop
		end select
		return
	end subroutine	
	subroutine assignmentSomeTData_char(T1,vec,length,i1,i2)!T(i1:i2)=vec()
		type(TData),intent(inout) ::T1
		integer,intent(in)::length,i1,i2
		character(len=*),intent(in) :: vec(length)
		select case(T1%classType)
			case (7)
				T1%adata(i1:i2)=vec
			case default
				write(*,*)"ERROR in assignmentTData_logi"
				call error_stop
		end select
		return
	end subroutine	
	subroutine assignment_int_Tdata(vec,T1,length)!vec=T
		integer,intent(in)::length
		integer,intent(inout) :: vec(length)
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				vec=T1%idata(1:T1%totalData)
			case (2)
				vec=T1%sdata(1:T1%totalData)
			case (3)
				vec=T1%ddata(1:T1%totalData)
			case (4)
				vec=T1%cdata(1:T1%totalData)
			case (5)
				vec=T1%zdata(1:T1%totalData)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_real4_Tdata(vec,T1,length)!vec=T
		integer,intent(in)::length
		real(kind=4),intent(inout) :: vec(length)
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				call scopy (length, real(T1%idata,kind=4), 1, vec, 1)
			case (2)
				call scopy (length,      T1%sdata        , 1, vec, 1)
			case (3)
				call scopy (length, real(T1%ddata,kind=4), 1, vec, 1)
			case (4)
				call scopy (length, real(T1%cdata,kind=4), 1, vec, 1)
			case (5)
				call scopy (length, real(T1%zdata,kind=4), 1, vec, 1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_real8_Tdata(vec,T1,length)!vec=T
		integer,intent(in)::length
		real(kind=8),intent(inout) :: vec(length)
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				call dcopy (length, dble(T1%idata), 1, vec, 1)
			case (2)
				call dcopy (length, dble(T1%sdata), 1, vec, 1)
			case (3)
				call dcopy (length,      T1%ddata, 1, vec, 1)
			case (4)
				call dcopy (length, dble(T1%cdata), 1, vec, 1)
			case (5)
				call dcopy (length, dble(T1%zdata), 1, vec, 1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_com4_Tdata(vec,T1,length)!vec=T
		integer,intent(in)::length
		complex(kind=4),intent(inout) :: vec(length)
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				call ccopy (length, cmplx(T1%idata,kind=4), 1, vec, 1)
			case (2)
				call ccopy (length, cmplx(T1%sdata,kind=4), 1, vec, 1)
			case (3)
				call ccopy (length, cmplx(T1%ddata,kind=4), 1, vec, 1)
			case (4)
				call ccopy (length,       T1%cdata        , 1, vec, 1)
			case (5)
				call ccopy (length, cmplx(T1%zdata,kind=4), 1, vec, 1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_com8_Tdata(vec,T1,length)!vec=T
		integer,intent(in)::length
		complex(kind=8),intent(inout) :: vec(length)
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				call zcopy (length, dcmplx(T1%idata), 1, vec, 1)
			case (2)
				call zcopy (length, dcmplx(T1%sdata), 1, vec, 1)
			case (3)
				call zcopy (length, dcmplx(T1%ddata), 1, vec, 1)
			case (4)
				call zcopy (length, dcmplx(T1%cdata), 1, vec, 1)
			case (5)
				call zcopy (length,         T1%zdata, 1, vec, 1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_logi_Tdata(vec,T1,length)!vec=T
		integer,intent(in)::length
		logical,intent(inout) :: vec(length)
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (6)
				vec=T1%ldata(1:T1%totalData)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_char_Tdata(vec,T1,length)!vec=T
		integer,intent(in)::length
		character(len=*),intent(inout) :: vec(length)
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				vec=T1%idata(1:T1%totalData)
			case (2)
				vec=T1%sdata(1:T1%totalData)
			case (3)
				vec=T1%ddata(1:T1%totalData)
			case (4)
				vec=T1%cdata(1:T1%totalData)
			case (5)
				vec=T1%zdata(1:T1%totalData)
			case (6)
				vec=T1%ldata(1:T1%totalData)
			case (7)
				vec=T1%adata(1:T1%totalData)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	
	subroutine assignment_int_Tdata_value(vec,T1)!val=T,val is a scalar
		integer,intent(inout) :: vec
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				vec=T1%idata(1)
			case (2)
				vec=T1%sdata(1)
			case (3)
				vec=T1%ddata(1)
			case (4)
				vec=T1%cdata(1)
			case (5)
				vec=T1%zdata(1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_real4_Tdata_value(vec,T1)!val=T,val is a scalar
		real(kind=4),intent(inout) :: vec
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				vec=T1%idata(1)
			case (2)
				vec=T1%sdata(1)
			case (3)
				vec=T1%ddata(1)
			case (4)
				vec=T1%cdata(1)
			case (5)
				vec=T1%zdata(1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_real8_Tdata_value(vec,T1)!val=T,val is a scalar
		real(kind=8),intent(inout) :: vec
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				vec=T1%idata(1)
			case (2)
				vec=T1%sdata(1)
			case (3)
				vec=T1%ddata(1)
			case (4)
				vec=T1%cdata(1)
			case (5)
				vec=T1%zdata(1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_com4_Tdata_value(vec,T1)!val=T,val is a scalar
		complex(kind=4),intent(inout) :: vec
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				vec=T1%idata(1)
			case (2)
				vec=T1%sdata(1)
			case (3)
				vec=T1%ddata(1)
			case (4)
				vec=T1%cdata(1)
			case (5)
				vec=T1%zdata(1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_com8_Tdata_value(vec,T1)!val=T,val is a scalar
		complex(kind=8),intent(inout) :: vec
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				vec=T1%idata(1)
			case (2)
				vec=T1%sdata(1)
			case (3)
				vec=T1%ddata(1)
			case (4)
				vec=T1%cdata(1)
			case (5)
				vec=T1%zdata(1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_logi_Tdata_value(vec,T1)!val=T,val is a scalar
		logical,intent(inout) :: vec
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (6)
				vec=T1%ldata(1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	subroutine assignment_char_Tdata_value(vec,T1)!val=T,val is a scalar
		character(len=*),intent(inout) :: vec
		type(TData),intent(in) ::T1
		select case(T1%classType)
			case (1)
				vec=T1%idata(1)
			case (2)
				vec=T1%sdata(1)
			case (3)
				vec=T1%ddata(1)
			case (4)
				vec=T1%cdata(1)
			case (5)
				vec=T1%zdata(1)
			case (6)
				vec=T1%ldata(1)
			case (7)
				vec=T1%adata(1)
			case default
				write(*,*)"ERROR in assignmentTData_int,logical=int"
				call error_stop
		end select
		return
	end subroutine
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                    print Tensor    
!
!**************************************************************************************************************
!**************************************************************************************************************	
	subroutine Tprintdata_file(T,uni,realflag_,printType)
		class(TData),intent(in) :: T
		integer,intent(in)::uni
		integer,optional,intent(in)::realflag_
		CHARACTER(len=*),optional,intent(in)::printType
		integer::realflag
		if(present(realflag_))then
			realflag=realflag_
		else
			realflag=0
		end if
		if(getflag(T)) then
			select case(T%classType)
				case(1)
					call printDataMAtrix_int_dim1_unit(T%idata,T%TotalData,uni,printType)
				case(2)
					call printDataMAtrix_real4_dim1_unit(T%sData,T%TotalData,uni,printType)
				case(3)
					call printDataMAtrix_real8_dim1_unit(T%dData,T%TotalData,uni,printType)
				case(4)
					select case(realflag)
						case (0)
							call printDataMAtrix_com4_dim1_unit(T%cData,T%TotalData,uni,printType)
						case (1)
							call printDataMAtrix_real4_dim1_unit(real(T%cData,kind=4),T%TotalData,uni,printType)
						case (2)
							call printDataMAtrix_real4_dim1_unit(aimag(T%cData),T%TotalData,uni,printType)
					end select
				case(5)
					select case(realflag)
						case (0)
							call printDataMAtrix_com8_dim1_unit(T%zData,T%TotalData,uni,printType)
						case (1)
							call printDataMAtrix_real8_dim1_unit(dble(T%zData),T%TotalData,uni,printType)
						case (2)
							call printDataMAtrix_real8_dim1_unit(aimag(T%zData),T%TotalData,uni,printType)
					end select
				case(6)
					call printDataMAtrix_logi_dim1_unit(T%lData,T%TotalData,uni,printType)
				case(7)
					call printDataMAtrix_char_dim1_unit(T%aData,T%TotalData,uni,printType)
			end select
		else
			write(uni,*) "There is no data"
		end if
		return
	end subroutine
	subroutine Tprintdata(T,realflag_,printType)
		class(TData),intent(in) :: T
		integer,optional,intent(in)::realflag_
		CHARACTER(len=*),optional,intent(in)::printType
		integer::realflag
		if(present(realflag_))then
			realflag=realflag_
		else
			realflag=0
		end if
		if(getflag(T)) then
			select case(T%classType)
				case(1)
					call printDataMAtrix_int_dim1_no_unit(T%iData,T%TotalData,printType)
				case(2)
					call printDataMAtrix_real4_dim1_no_unit(T%sData,T%TotalData,printType)
				case(3)
					call printDataMAtrix_real8_dim1_no_unit(T%dData,T%TotalData,printType)
				case(4)
					select case(realflag)
						case (0)
							call printDataMAtrix_com4_dim1_no_unit(T%cData,T%TotalData,printType)
						case (1)
							call printDataMAtrix_real4_dim1_no_unit(real(T%cData,kind=4),T%TotalData,printType)
						case (2)
							call printDataMAtrix_real4_dim1_no_unit(aimag(T%cData),T%TotalData,printType)
					end select
				case(5)
					select case(realflag)
						case (0)
							call printDataMAtrix_com8_dim1_no_unit(T%zData,T%TotalData,printType)
						case (1)
							call printDataMAtrix_real8_dim1_no_unit(dble(T%zData),T%TotalData,printType)
						case (2)
							call printDataMAtrix_real8_dim1_no_unit(aimag(T%zData),T%TotalData,printType)
					end select
				case(6)
					call printDataMAtrix_logi_dim1_no_unit(T%lData,T%TotalData,printType)
				case(7)
					call printDataMAtrix_char_dim1_no_unit(T%aData,T%TotalData,printType)
			end select
		else
			write(*,*) "There is no data"
		end if
		return
	end subroutine	
	
	subroutine Tprint_as_matrix(T,realflag,rank,dimen,printType)
		type(TData),intent(in) :: T
		integer,intent(in)::realflag,rank
		integer,intent(in)::dimen(rank)
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		select case(rank)
			case(2)
				select case(T%classType)
					case(1)
						call printDataMAtrix_int_dim2_no_unit(T%iData,dimen(1),dimen(2),printType)
					case(2)
						call printDataMAtrix_real4_dim2_no_unit(T%sData,dimen(1),dimen(2),printType)
					case(3)
						call printDataMAtrix_real8_dim2_no_unit(T%dData,dimen(1),dimen(2),printType)
					case(4)
						select case(realflag)
							case (0)
								call printDataMAtrix_com4_dim2_no_unit(T%cData,dimen(1),dimen(2),printType)
							case (1)
								call printDataMAtrix_real4_dim2_no_unit(real(T%cData,kind=4),dimen(1),dimen(2),printType)
							case (2)
								call printDataMAtrix_real4_dim2_no_unit(aimag(T%cData),dimen(1),dimen(2),printType)
						end select
					case(5)
						select case(realflag)
							case (0)
								call printDataMAtrix_com8_dim2_no_unit(T%zData,dimen(1),dimen(2),printType)
							case (1)
								call printDataMAtrix_real8_dim2_no_unit(dble(T%zData),dimen(1),dimen(2),printType)
							case (2)
								call printDataMAtrix_real8_dim2_no_unit(aimag(T%zData),dimen(1),dimen(2),printType)
						end select
					case(6)
						call printDataMAtrix_logi_dim2_no_unit(T%lData,dimen(1),dimen(2),printType)
					case(7)
						call printDataMAtrix_char_dim2_no_unit(T%aData,dimen(1),dimen(2),printType)
				end select
			case(3)
				select case(T%classType)
					case(1)
						call printDataMAtrix_int_dim3_no_unit(T%iData,dimen(1),dimen(2),dimen(3),printType)
					case(2)
						call printDataMAtrix_real4_dim3_no_unit(T%sData,dimen(1),dimen(2),dimen(3),printType)
					case(3)
						call printDataMAtrix_real8_dim3_no_unit(T%dData,dimen(1),dimen(2),dimen(3),printType)
					case(4)
						select case(realflag)
							case (0)
								call printDataMAtrix_com4_dim3_no_unit(T%cData,dimen(1),dimen(2),dimen(3),printType)
							case (1)
								call printDataMAtrix_real4_dim3_no_unit(real(T%cData,kind=4),dimen(1),dimen(2),dimen(3),printType)
							case (2)
								call printDataMAtrix_real4_dim3_no_unit(aimag(T%cData),dimen(1),dimen(2),dimen(3),printType)
						end select
					case(5)
						select case(realflag)
							case (0)
								call printDataMAtrix_com8_dim3_no_unit(T%zData,dimen(1),dimen(2),dimen(3),printType)
							case (1)
								call printDataMAtrix_real8_dim3_no_unit(dble(T%zData),dimen(1),dimen(2),dimen(3),printType)
							case (2)
								call printDataMAtrix_real8_dim3_no_unit(aimag(T%zData),dimen(1),dimen(2),dimen(3),printType)
						end select
					case(6)
						call printDataMAtrix_logi_dim3_no_unit(T%lData,dimen(1),dimen(2),dimen(3),printType)
					case(7)
						call printDataMAtrix_char_dim3_no_unit(T%aData,dimen(1),dimen(2),dimen(3),printType)
				end select
			case(4)
				select case(T%classType)
					case(1)
						call printDataMAtrix_int_dim4_no_unit(T%iData,dimen(1),dimen(2),dimen(3),dimen(4),printType)
					case(2)
						call printDataMAtrix_real4_dim4_no_unit(T%sData,dimen(1),dimen(2),dimen(3),dimen(4),printType)
					case(3)
						call printDataMAtrix_real8_dim4_no_unit(T%dData,dimen(1),dimen(2),dimen(3),dimen(4),printType)
					case(4)
						select case(realflag)
							case (0)
								call printDataMAtrix_com4_dim4_no_unit(T%cData,dimen(1),dimen(2),dimen(3),dimen(4),printType)
							case (1)
								call printDataMAtrix_real4_dim4_no_unit(real(T%cData,kind=4),dimen(1),dimen(2),dimen(3),dimen(4),printType)
							case (2)
								call printDataMAtrix_real4_dim4_no_unit(aimag(T%cData),dimen(1),dimen(2),dimen(3),dimen(4),printType)
						end select
					case(5)
						select case(realflag)
							case (0)
								call printDataMAtrix_com8_dim4_no_unit(T%zData,dimen(1),dimen(2),dimen(3),dimen(4),printType)
							case (1)
								call printDataMAtrix_real8_dim4_no_unit(dble(T%zData),dimen(1),dimen(2),dimen(3),dimen(4),printType)
							case (2)
								call printDataMAtrix_real8_dim4_no_unit(aimag(T%zData),dimen(1),dimen(2),dimen(3),dimen(4),printType)
						end select
					case(6)
						call printDataMAtrix_logi_dim4_no_unit(T%lData,dimen(1),dimen(2),dimen(3),dimen(4),printType)
					case(7)
						call printDataMAtrix_char_dim4_no_unit(T%aData,dimen(1),dimen(2),dimen(3),dimen(4),printType)
				end select
			case default
				write(*,*) "ERROR in print"
				call error_stop
		end select
		return
	end subroutine
	subroutine Tprint_as_matrix_file(T,uni,realflag,rank,dimen,printType)
		type(TData),intent(in) :: T
		integer,intent(in)::uni
		integer,intent(in)::realflag,rank
		integer,intent(in)::dimen(rank)
		CHARACTER(len=*),optional,intent(in)::printType
		CHARACTER(len=20)::classTypeChar
		select case(rank)
			case(2)
				select case(T%classType)
					case(1)
						call printDataMAtrix_int_dim2_unit(T%iData,dimen(1),dimen(2),uni,printType)
					case(2)
						call printDataMAtrix_real4_dim2_unit(T%sData,dimen(1),dimen(2),uni,printType)
					case(3)
						call printDataMAtrix_real8_dim2_unit(T%dData,dimen(1),dimen(2),uni,printType)
					case(4)
						select case(realflag)
							case (0)
								call printDataMAtrix_com4_dim2_unit(T%cData,dimen(1),dimen(2),uni,printType)
							case (1)
								call printDataMAtrix_real4_dim2_unit(real(T%cData,kind=4),dimen(1),dimen(2),uni,printType)
							case (2)
								call printDataMAtrix_real4_dim2_unit(aimag(T%cData),dimen(1),dimen(2),uni,printType)
						end select
					case(5)
						select case(realflag)
							case (0)
								call printDataMAtrix_com8_dim2_unit(T%zData,dimen(1),dimen(2),uni,printType)
							case (1)
								call printDataMAtrix_real8_dim2_unit(dble(T%zData),dimen(1),dimen(2),uni,printType)
							case (2)
								call printDataMAtrix_real8_dim2_unit(aimag(T%zData),dimen(1),dimen(2),uni,printType)
						end select
					case(6)
						call printDataMAtrix_logi_dim2_unit(T%lData,dimen(1),dimen(2),uni,printType)
					case(7)
						call printDataMAtrix_char_dim2_unit(T%aData,dimen(1),dimen(2),uni,printType)
				end select
			case(3)
				select case(T%classType)
					case(1)
						call printDataMAtrix_int_dim3_unit(T%iData,dimen(1),dimen(2),dimen(3),uni,printType)
					case(2)
						call printDataMAtrix_real4_dim3_unit(T%sData,dimen(1),dimen(2),dimen(3),uni,printType)
					case(3)
						call printDataMAtrix_real8_dim3_unit(T%dData,dimen(1),dimen(2),dimen(3),uni,printType)
					case(4)
						select case(realflag)
							case (0)
								call printDataMAtrix_com4_dim3_unit(T%cData,dimen(1),dimen(2),dimen(3),uni,printType)
							case (1)
								call printDataMAtrix_real4_dim3_unit(real(T%cData,kind=4),dimen(1),dimen(2),dimen(3),uni,printType)
							case (2)
								call printDataMAtrix_real4_dim3_unit(aimag(T%cData),dimen(1),dimen(2),dimen(3),uni,printType)
						end select
					case(5)
						select case(realflag)
							case (0)
								call printDataMAtrix_com8_dim3_unit(T%zData,dimen(1),dimen(2),dimen(3),uni,printType)
							case (1)
								call printDataMAtrix_real8_dim3_unit(dble(T%zData),dimen(1),dimen(2),dimen(3),uni,printType)
							case (2)
								call printDataMAtrix_real8_dim3_unit(aimag(T%zData),dimen(1),dimen(2),dimen(3),uni,printType)
						end select
					case(6)
						call printDataMAtrix_logi_dim3_unit(T%lData,dimen(1),dimen(2),dimen(3),uni,printType)
					case(7)
						call printDataMAtrix_char_dim3_unit(T%aData,dimen(1),dimen(2),dimen(3),uni,printType)
				end select
			case(4)
				select case(T%classType)
					case(1)
						call printDataMAtrix_int_dim4_unit(T%iData,dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
					case(2)
						call printDataMAtrix_real4_dim4_unit(T%sData,dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
					case(3)
						call printDataMAtrix_real8_dim4_unit(T%dData,dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
					case(4)
						select case(realflag)
							case (0)
								call printDataMAtrix_com4_dim4_unit(T%cData,dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
							case (1)
								call printDataMAtrix_real4_dim4_unit(real(T%cData,kind=4),dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
							case (2)
								call printDataMAtrix_real4_dim4_unit(aimag(T%cData),dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
						end select
					case(5)
						select case(realflag)
							case (0)
								call printDataMAtrix_com8_dim4_unit(T%zData,dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
							case (1)
								call printDataMAtrix_real8_dim4_unit(dble(T%zData),dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
							case (2)
								call printDataMAtrix_real8_dim4_unit(aimag(T%zData),dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
						end select
					case(6)
						call printDataMAtrix_logi_dim4_unit(T%lData,dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
					case(7)
						call printDataMAtrix_char_dim4_unit(T%aData,dimen(1),dimen(2),dimen(3),dimen(4),uni,printType)
				end select
			case default
				write(uni,*) "ERROR in print"
				call error_stop
		end select
		return
	end subroutine

!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  element
!
!**************************************************************************************************************
!**************************************************************************************************************	
	subroutine element_routine_int(output,T,dimen,LDdimen,rank)
		type(TData),intent(in)::T
		integer,intent(inout)::output
		integer,intent(in)::dimen(:),LDdimen(:),rank
		if(rank.eq.1)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim1(dimen(1),T%idata,LDdimen(1)) 
				case (2)
					output=Element_subroutine_real4_dim1(dimen(1),T%sdata,LDdimen(1)) 
				case (3)
					output=Element_subroutine_real8_dim1(dimen(1),T%ddata,LDdimen(1)) 
				case (4)
					output=Element_subroutine_com4_dim1(dimen(1),T%cdata,LDdimen(1)) 
				case (5)
					output=Element_subroutine_com8_dim1(dimen(1),T%zdata,LDdimen(1)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.2)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim2(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2)) 
				case (2)
					output=Element_subroutine_real4_dim2(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2)) 
				case (3)
					output=Element_subroutine_real8_dim2(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2)) 
				case (4)
					output=Element_subroutine_com4_dim2(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2)) 
				case (5)
					output=Element_subroutine_com8_dim2(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.3)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim3(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (2)
					output=Element_subroutine_real4_dim3(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (3)
					output=Element_subroutine_real8_dim3(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (4)
					output=Element_subroutine_com4_dim3(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (5)
					output=Element_subroutine_com8_dim3(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.4)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (2)
					output=Element_subroutine_real4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (3)
					output=Element_subroutine_real8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (4)
					output=Element_subroutine_com4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (5)
					output=Element_subroutine_com8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		write(*,*)"ERROR in element_int"
		call error_stop
	end subroutine
	subroutine element_routine_real4(output,T,dimen,LDdimen,rank)
		type(TData),intent(in)::T
		real(kind=4),intent(inout)::output
		integer,intent(in)::dimen(:),LDdimen(:),rank
		if(rank.eq.1)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim1(dimen(1),T%idata,LDdimen(1)) 
				case (2)
					output=Element_subroutine_real4_dim1(dimen(1),T%sdata,LDdimen(1)) 
				case (3)
					output=Element_subroutine_real8_dim1(dimen(1),T%ddata,LDdimen(1)) 
				case (4)
					output=Element_subroutine_com4_dim1(dimen(1),T%cdata,LDdimen(1)) 
				case (5)
					output=Element_subroutine_com8_dim1(dimen(1),T%zdata,LDdimen(1)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.2)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim2(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2)) 
				case (2)
					output=Element_subroutine_real4_dim2(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2)) 
				case (3)
					output=Element_subroutine_real8_dim2(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2)) 
				case (4)
					output=Element_subroutine_com4_dim2(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2)) 
				case (5)
					output=Element_subroutine_com8_dim2(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.3)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim3(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (2)
					output=Element_subroutine_real4_dim3(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (3)
					output=Element_subroutine_real8_dim3(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (4)
					output=Element_subroutine_com4_dim3(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (5)
					output=Element_subroutine_com8_dim3(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.4)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (2)
					output=Element_subroutine_real4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (3)
					output=Element_subroutine_real8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (4)
					output=Element_subroutine_com4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (5)
					output=Element_subroutine_com8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		write(*,*)"ERROR in element_int"
		call error_stop
	end subroutine
	subroutine element_routine_real8(output,T,dimen,LDdimen,rank)
		type(TData),intent(in)::T
		real(kind=8),intent(inout)::output
		integer,intent(in)::dimen(:),LDdimen(:),rank
		if(rank.eq.1)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim1(dimen(1),T%idata,LDdimen(1)) 
				case (2)
					output=Element_subroutine_real4_dim1(dimen(1),T%sdata,LDdimen(1)) 
				case (3)
					output=Element_subroutine_real8_dim1(dimen(1),T%ddata,LDdimen(1)) 
				case (4)
					output=Element_subroutine_com4_dim1(dimen(1),T%cdata,LDdimen(1)) 
				case (5)
					output=Element_subroutine_com8_dim1(dimen(1),T%zdata,LDdimen(1)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.2)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim2(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2)) 
				case (2)
					output=Element_subroutine_real4_dim2(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2)) 
				case (3)
					output=Element_subroutine_real8_dim2(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2)) 
				case (4)
					output=Element_subroutine_com4_dim2(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2)) 
				case (5)
					output=Element_subroutine_com8_dim2(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.3)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim3(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (2)
					output=Element_subroutine_real4_dim3(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (3)
					output=Element_subroutine_real8_dim3(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (4)
					output=Element_subroutine_com4_dim3(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (5)
					output=Element_subroutine_com8_dim3(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.4)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (2)
					output=Element_subroutine_real4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (3)
					output=Element_subroutine_real8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (4)
					output=Element_subroutine_com4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (5)
					output=Element_subroutine_com8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		write(*,*)"ERROR in element_int"
		call error_stop
	end subroutine
	subroutine element_routine_com4(output,T,dimen,LDdimen,rank)
		type(TData),intent(in)::T
		complex(kind=4),intent(inout)::output
		integer,intent(in)::dimen(:),LDdimen(:),rank
		if(rank.eq.1)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim1(dimen(1),T%idata,LDdimen(1)) 
				case (2)
					output=Element_subroutine_real4_dim1(dimen(1),T%sdata,LDdimen(1)) 
				case (3)
					output=Element_subroutine_real8_dim1(dimen(1),T%ddata,LDdimen(1)) 
				case (4)
					output=Element_subroutine_com4_dim1(dimen(1),T%cdata,LDdimen(1)) 
				case (5)
					output=Element_subroutine_com8_dim1(dimen(1),T%zdata,LDdimen(1)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.2)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim2(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2)) 
				case (2)
					output=Element_subroutine_real4_dim2(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2)) 
				case (3)
					output=Element_subroutine_real8_dim2(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2)) 
				case (4)
					output=Element_subroutine_com4_dim2(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2)) 
				case (5)
					output=Element_subroutine_com8_dim2(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.3)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim3(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (2)
					output=Element_subroutine_real4_dim3(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (3)
					output=Element_subroutine_real8_dim3(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (4)
					output=Element_subroutine_com4_dim3(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (5)
					output=Element_subroutine_com8_dim3(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.4)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (2)
					output=Element_subroutine_real4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (3)
					output=Element_subroutine_real8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (4)
					output=Element_subroutine_com4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (5)
					output=Element_subroutine_com8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		write(*,*)"ERROR in element_int"
		call error_stop
	end subroutine
	subroutine element_routine_com8(output,T,dimen,LDdimen,rank)
		type(TData),intent(in)::T
		complex(kind=8),intent(inout)::output
		integer,intent(in)::dimen(:),LDdimen(:),rank
		if(rank.eq.1)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim1(dimen(1),T%idata,LDdimen(1)) 
				case (2)
					output=Element_subroutine_real4_dim1(dimen(1),T%sdata,LDdimen(1)) 
				case (3)
					output=Element_subroutine_real8_dim1(dimen(1),T%ddata,LDdimen(1)) 
				case (4)
					output=Element_subroutine_com4_dim1(dimen(1),T%cdata,LDdimen(1)) 
				case (5)
					output=Element_subroutine_com8_dim1(dimen(1),T%zdata,LDdimen(1)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.2)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim2(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2)) 
				case (2)
					output=Element_subroutine_real4_dim2(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2)) 
				case (3)
					output=Element_subroutine_real8_dim2(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2)) 
				case (4)
					output=Element_subroutine_com4_dim2(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2)) 
				case (5)
					output=Element_subroutine_com8_dim2(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.3)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim3(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (2)
					output=Element_subroutine_real4_dim3(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (3)
					output=Element_subroutine_real8_dim3(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (4)
					output=Element_subroutine_com4_dim3(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (5)
					output=Element_subroutine_com8_dim3(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.4)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (2)
					output=Element_subroutine_real4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (3)
					output=Element_subroutine_real8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (4)
					output=Element_subroutine_com4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (5)
					output=Element_subroutine_com8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		write(*,*)"ERROR in element_int"
		call error_stop
	end subroutine
	subroutine element_routine_logi(output,T,dimen,LDdimen,rank)
		type(TData),intent(in)::T
		logical,intent(inout)::output
		integer,intent(in)::dimen(:),LDdimen(:),rank
		if(rank.eq.1)then
			select case(T%classtype)
				case (6)
					output=Element_subroutine_logi_dim1(dimen(1),T%ldata,LDdimen(1)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.2)then
			select case(T%classtype)
				case (6)
					output=Element_subroutine_logi_dim2(dimen(1),dimen(2),T%ldata,LDdimen(1),LDdimen(2)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.3)then
			select case(T%classtype)
				case (6)
					output=Element_subroutine_logi_dim3(dimen(1),dimen(2),dimen(3),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.4)then
			select case(T%classtype)
				case (6)
					output=Element_subroutine_logi_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		write(*,*)"ERROR in element_int"
		call error_stop
	end subroutine
	subroutine element_routine_char(output,T,dimen,LDdimen,rank)
		type(TData),intent(in)::T
		character(len=*),intent(inout)::output
		integer,intent(in)::dimen(:),LDdimen(:),rank
		if(rank.eq.1)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim1(dimen(1),T%idata,LDdimen(1)) 
				case (2)
					output=Element_subroutine_real4_dim1(dimen(1),T%sdata,LDdimen(1)) 
				case (3)
					output=Element_subroutine_real8_dim1(dimen(1),T%ddata,LDdimen(1)) 
				case (4)
					output=Element_subroutine_com4_dim1(dimen(1),T%cdata,LDdimen(1)) 
				case (5)
					output=Element_subroutine_com8_dim1(dimen(1),T%zdata,LDdimen(1)) 
				case (6)
					output=Element_subroutine_logi_dim1(dimen(1),T%ldata,LDdimen(1)) 
				case (7)
					output=Element_subroutine_char_dim1(dimen(1),T%adata,LDdimen(1)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.2)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim2(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2)) 
				case (2)
					output=Element_subroutine_real4_dim2(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2)) 
				case (3)
					output=Element_subroutine_real8_dim2(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2)) 
				case (4)
					output=Element_subroutine_com4_dim2(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2)) 
				case (5)
					output=Element_subroutine_com8_dim2(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2)) 
				case (6)
					output=Element_subroutine_logi_dim2(dimen(1),dimen(2),T%ldata,LDdimen(1),LDdimen(2)) 
				case (7)
					output=Element_subroutine_char_dim2(dimen(1),dimen(2),T%adata,LDdimen(1),LDdimen(2)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.3)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim3(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (2)
					output=Element_subroutine_real4_dim3(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (3)
					output=Element_subroutine_real8_dim3(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3))
				case (4)
					output=Element_subroutine_com4_dim3(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (5)
					output=Element_subroutine_com8_dim3(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3))  
				case (6)
					output=Element_subroutine_logi_dim3(dimen(1),dimen(2),dimen(3),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case (7)
					output=Element_subroutine_char_dim3(dimen(1),dimen(2),dimen(3),T%adata,LDdimen(1),LDdimen(2),LDdimen(3)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		if(rank.eq.4)then
			select case(T%classtype)
				case (1)
					output=Element_subroutine_int_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (2)
					output=Element_subroutine_real4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (3)
					output=Element_subroutine_real8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (4)
					output=Element_subroutine_com4_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (5)
					output=Element_subroutine_com8_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (6)
					output=Element_subroutine_logi_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case (7)
					output=Element_subroutine_char_dim4(dimen(1),dimen(2),dimen(3),dimen(4),T%adata,LDdimen(1),LDdimen(2),LDdimen(3),LDdimen(4)) 
				case default
					write(*,*)"ERROR in element_int"
					call error_stop
			end select
			return
		end if
		write(*,*)"ERROR in element_int"
		call error_stop
	end subroutine
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  generate data in TData
!
!**************************************************************************************************************
!**************************************************************************************************************	
	subroutine generate_random_data(T,region)
		type(TData),intent(inout)::T
		real*8,optional,intent(in)::region(2)
		real*8 ::temp_real,temp_imag,minnum,maxnum,delmum,numr,numi
		complex*8::numc
		complex*16::numz
		integer::i,linchar,k,inum
		character(len=100)::w
		if(present(region))then
			minnum=region(1)
			maxnum=region(2)
		else
			minnum=0d0
			maxnum=1d0
		end if
		delmum=maxnum-minnum
		select case(T%classtype)
			case (1)
				do i=1,T%TotalData
					numr=randomnumber()
					numr=(delmum+1)*numr+minnum
					T%idata(i)=numr
				end do
			case (2)
				do i=1,T%TotalData
					numr=randomnumber()
					numr=delmum*numr+minnum
					T%sdata(i)=numr
				end do
			case (3)
				do i=1,T%TotalData
					numr=randomnumber()
					numr=delmum*numr+minnum
					T%ddata(i)=numr
				end do
			case (4)
				do i=1,T%TotalData
					temp_real=randomnumber()
					temp_imag=randomnumber()
					numc=cmplx(temp_real,temp_imag,kind=4)
					numc=(numc*delmum)+cmplx(minnum,minnum,kind=4)
					T%cdata(i)=numc
				end do
			case (5)
				do i=1,T%TotalData
					temp_real=randomnumber()
					temp_imag=randomnumber()
					numz=dcmplx(temp_real,temp_imag)
					numz=(numz*delmum)+dcmplx(minnum,minnum)
					T%zdata(i)=numz
				end do
			case (6)
				do i=1,T%TotalData
					numr=randomnumber()
					if(numr.le. 0.5d0) then
						T%ldata(i)=.true.
					else
						T%ldata(i)=.false.
					end if
				end do
			case (7)
				if(present(region))then
					linchar=region(1)
				else
					linchar=3
				end if
				maxnum=126
				minnum=32
				delmum=maxnum-minnum
				do i=1,T%TotalData
					w=''
					do k=1,linchar
						inum=(delmum+1)*randomnumber()+minnum
						w=w+char(inum)
					end do
					T%adata(i)=(trim(adjustl(w)))
				end do
			case default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine generate_random_data_region4(T,region)
		type(TData),intent(inout)::T
		real*4,optional,intent(in)::region(2)
		real*8 ::temp_real,temp_imag,minnum,maxnum,delmum,numr,numi
		complex*8::numc
		complex*16::numz
		integer::i,linchar,k,inum
		character(len=100)::w
		if(present(region))then
			minnum=region(1)
			maxnum=region(2)
		else
			minnum=0d0
			maxnum=1d0
		end if
		delmum=maxnum-minnum
		select case(T%classtype)
			case (1)
				do i=1,T%TotalData
					numr=randomnumber()
					numr=(delmum+1)*numr+minnum
					T%idata(i)=numr
				end do
			case (2)
				do i=1,T%TotalData
					numr=randomnumber()
					numr=delmum*numr+minnum
					T%sdata(i)=numr
				end do
			case (3)
				do i=1,T%TotalData
					numr=randomnumber()
					numr=delmum*numr+minnum
					T%ddata(i)=numr
				end do
			case (4)
				do i=1,T%TotalData
					numc=cmplx(randomnumber(),randomnumber(),kind=4)
					numc=(numc*delmum)+cmplx(minnum,minnum,kind=4)
					T%cdata(i)=numc
				end do
			case (5)
				do i=1,T%TotalData
					numz=dcmplx(randomnumber(),randomnumber())
					numz=(numz*delmum)+dcmplx(minnum,minnum)
					T%zdata(i)=numz
				end do
			case (6)
				do i=1,T%TotalData
					numr=randomnumber()
					if(numr.le. 0.5d0) then
						T%ldata(i)=.true.
					else
						T%ldata(i)=.false.
					end if
				end do
			case (7)
				if(present(region))then
					linchar=region(1)
				else
					linchar=3
				end if
				maxnum=126
				minnum=32
				delmum=maxnum-minnum
				do i=1,T%TotalData
					w=''
					do k=1,linchar
						inum=(delmum+1)*randomnumber()+minnum
						w=w+char(inum)
					end do
					T%adata(i)=(trim(adjustl(w)))
				end do
			case default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine generate_random_data_regioni(T,region)
		type(TData),intent(inout)::T
		integer,optional,intent(in)::region(2)
		real*8 ::temp_real,temp_imag,minnum,maxnum,delmum,numr,numi
		complex*8::numc
		complex*16::numz
		integer::i,linchar,k,inum
		character(len=100)::w
		if(present(region))then
			minnum=region(1)
			maxnum=region(2)
		else
			minnum=0d0
			maxnum=1d0
		end if
		delmum=maxnum-minnum
		select case(T%classtype)
			case (1)
				do i=1,T%TotalData
					numr=randomnumber()
					numr=(delmum+1)*numr+minnum
					T%idata(i)=numr
				end do
			case (2)
				do i=1,T%TotalData
					numr=randomnumber()
					numr=delmum*numr+minnum
					T%sdata(i)=numr
				end do
			case (3)
				do i=1,T%TotalData
					numr=randomnumber()
					numr=delmum*numr+minnum
					T%ddata(i)=numr
				end do
			case (4)
				do i=1,T%TotalData
					numc=cmplx(randomnumber(),randomnumber(),kind=4)
					numc=(numc*delmum)+cmplx(minnum,minnum,kind=4)
					T%cdata(i)=numc
				end do
			case (5)
				do i=1,T%TotalData
					numz=dcmplx(randomnumber(),randomnumber())
					numz=(numz*delmum)+dcmplx(minnum,minnum)
					T%zdata(i)=numz
				end do
			case (6)
				do i=1,T%TotalData
					numr=randomnumber()
					if(numr.le. 0.5d0) then
						T%ldata(i)=.true.
					else
						T%ldata(i)=.false.
					end if
				end do
			case (7)
				if(present(region))then
					linchar=region(1)
				else
					linchar=3
				end if
				maxnum=126
				minnum=32
				delmum=maxnum-minnum
				do i=1,T%TotalData
					w=''
					do k=1,linchar
						inum=(delmum+1)*randomnumber()+minnum
						w=w+char(inum)
					end do
					T%adata(i)=(trim(adjustl(w)))
				end do
			case default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine set_all_data_int(T,value)
		type(TData),intent(inout)::T
		integer,intent(in)::value
		integer::i
		select case(T%classtype)
			case (1)
				T%idata(1:T%TotalData)=value
			case (2)
				T%sdata(1:T%TotalData)=value
			case (3)
				T%ddata(1:T%TotalData)=value
			case (4)
				T%cdata(1:T%TotalData)=value
			case (5)
				T%zdata(1:T%TotalData)=value
			case (7)
				do i=1,T%TotalData
					T%adata(i)=value
				end do
			case default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine set_all_data_real4(T,value)
		type(TData),intent(inout)::T
		real(kind=4),intent(in)::value
		integer::i
		select case(T%classtype)
			case (1)
				T%idata(1:T%TotalData)=value
			case (2)
				T%sdata(1:T%TotalData)=value
			case (3)
				T%ddata(1:T%TotalData)=value
			case (4)
				T%cdata(1:T%TotalData)=value
			case (5)
				T%zdata(1:T%TotalData)=value
			case (7)
				do i=1,T%TotalData
					T%adata(i)=value
				end do
			case default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine set_all_data_real8(T,value)
		type(TData),intent(inout)::T
		real(kind=8),intent(in)::value
		integer::i
		select case(T%classtype)
			case (1)
				T%idata(1:T%TotalData)=value
			case (2)
				T%sdata(1:T%TotalData)=value
			case (3)
				T%ddata(1:T%TotalData)=value
			case (4)
				T%cdata(1:T%TotalData)=value
			case (5)
				T%zdata(1:T%TotalData)=value
			case (7)
				do i=1,T%TotalData
					T%adata(i)=value
				end do
			case default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine set_all_data_com4(T,value)
		type(TData),intent(inout)::T
		complex(kind=4),intent(in)::value
		integer::i
		select case(T%classtype)
			case (1)
				T%idata(1:T%TotalData)=value
			case (2)
				T%sdata(1:T%TotalData)=value
			case (3)
				T%ddata(1:T%TotalData)=value
			case (4)
				T%cdata(1:T%TotalData)=value
			case (5)
				T%zdata(1:T%TotalData)=value
			case default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine set_all_data_com8(T,value)
		type(TData),intent(inout)::T
		complex(kind=8),intent(in)::value
		integer::i
		select case(T%classtype)
			case (1)
				T%idata(1:T%TotalData)=value
			case (2)
				T%sdata(1:T%TotalData)=value
			case (3)
				T%ddata(1:T%TotalData)=value
			case (4)
				T%cdata(1:T%TotalData)=value
			case (5)
				T%zdata(1:T%TotalData)=value
			case default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine set_all_data_logi(T,value)
		type(TData),intent(inout)::T
		logical,intent(in)::value
		integer::i
		select case(T%classtype)
			case (6)
				T%ldata(1:T%TotalData)=value
			case (7)
				do i=1,T%TotalData
					T%adata(i)=value
				end do
			case default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine
	subroutine set_all_data_char(T,value)
		type(TData),intent(inout)::T
		character(len=*),intent(in)::value
		integer::i
		select case(T%classtype)
			case (7)
				do i=1,T%TotalData
					T%adata(i)=value
				end do
			case default 
				write(*,*)"no such class"
				stop
		end 	select
		return
	end subroutine

!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  modify element in Tensor
!
!**************************************************************************************************************
!**************************************************************************************************************

	subroutine modify_TData_class_i(T,dimen,LDdimen,rank,newdata)
		class(TData),intent(inout)::T
		integer,intent(in)::rank
		integer,intent(in)::dimen(rank),LDdimen(rank)
		integer,intent(in)::newdata
		select case(rank)
			case(1)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim1_int_i(dimen(1),T%idata,T%totalData,newdata)
					case(2)
						call modifyTen_val_dim1_real4_i(dimen(1),T%sdata,T%totalData,newdata)	
					case(3)
						call modifyTen_val_dim1_real8_i(dimen(1),T%ddata,T%totalData,newdata)	
					case(4)
						call modifyTen_val_dim1_com4_i(dimen(1),T%cdata,T%totalData,newdata)	
					case(5)
						call modifyTen_val_dim1_com8_i(dimen(1),T%zdata,T%totalData,newdata)	
					case(6)
						call modifyTen_val_dim1_logi_i(dimen(1),T%ldata,T%totalData,newdata)	
					case(7)
						call modifyTen_val_dim1_char_i(dimen(1),T%adata,T%totalData,newdata)	
				end select
			case(2)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim2_int_i(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2),newdata)
					case(2)
						call modifyTen_val_dim2_real4_i(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2),newdata)	
					case(3)
						call modifyTen_val_dim2_real8_i(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2),newdata)	
					case(4)
						call modifyTen_val_dim2_com4_i(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2),newdata)	
					case(5)
						call modifyTen_val_dim2_com8_i(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2),newdata)	
					case(6)
						call modifyTen_val_dim2_logi_i(dimen(1),dimen(2),T%ldata,LDdimen(1),LDdimen(2),newdata)	
					case(7)
						call modifyTen_val_dim2_char_i(dimen(1),dimen(2),T%adata,LDdimen(1),LDdimen(2),newdata)	
				end select
			case(3)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim3_int_i(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(2)
						call modifyTen_val_dim3_real4_i(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(3)
						call modifyTen_val_dim3_real8_i(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(4)
						call modifyTen_val_dim3_com4_i(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(5)
						call modifyTen_val_dim3_com8_i(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(6)
						call modifyTen_val_dim3_logi_i(dimen(1),dimen(2),dimen(3),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(7)
						call modifyTen_val_dim3_char_i(dimen(1),dimen(2),dimen(3),T%adata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
				end select
			case(4)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim4_int_i(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(2)
						call modifyTen_val_dim4_real4_i(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(3)
						call modifyTen_val_dim4_real8_i(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(4)
						call modifyTen_val_dim4_com4_i(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(5)
						call modifyTen_val_dim4_com8_i(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(6)
						call modifyTen_val_dim4_logi_i(dimen(1),dimen(2),dimen(3),dimen(4),T%ldata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(7)
						call modifyTen_val_dim4_char_i(dimen(1),dimen(2),dimen(3),dimen(4),T%adata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
				end select
			
		end select
		return
	end subroutine

	subroutine modify_TData_class_s(T,dimen,LDdimen,rank,newdata)
		class(TData),intent(inout)::T
		integer,intent(in)::rank
		integer,intent(in)::dimen(rank),LDdimen(rank)
		real(kind=4),intent(in)::newdata
		select case(rank)
			case(1)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim1_int_s(dimen(1),T%idata,T%totalData,newdata)
					case(2)
						call modifyTen_val_dim1_real4_s(dimen(1),T%sdata,T%totalData,newdata)	
					case(3)
						call modifyTen_val_dim1_real8_s(dimen(1),T%ddata,T%totalData,newdata)	
					case(4)
						call modifyTen_val_dim1_com4_s(dimen(1),T%cdata,T%totalData,newdata)	
					case(5)
						call modifyTen_val_dim1_com8_s(dimen(1),T%zdata,T%totalData,newdata)	
					case(6)
						call modifyTen_val_dim1_logi_s(dimen(1),T%ldata,T%totalData,newdata)	
					case(7)
						call modifyTen_val_dim1_char_s(dimen(1),T%adata,T%totalData,newdata)	
				end select
			case(2)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim2_int_s(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2),newdata)
					case(2)
						call modifyTen_val_dim2_real4_s(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2),newdata)	
					case(3)
						call modifyTen_val_dim2_real8_s(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2),newdata)	
					case(4)
						call modifyTen_val_dim2_com4_s(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2),newdata)	
					case(5)
						call modifyTen_val_dim2_com8_s(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2),newdata)	
					case(6)
						call modifyTen_val_dim2_logi_s(dimen(1),dimen(2),T%ldata,LDdimen(1),LDdimen(2),newdata)	
					case(7)
						call modifyTen_val_dim2_char_s(dimen(1),dimen(2),T%adata,LDdimen(1),LDdimen(2),newdata)	
				end select
			case(3)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim3_int_s(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(2)
						call modifyTen_val_dim3_real4_s(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(3)
						call modifyTen_val_dim3_real8_s(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(4)
						call modifyTen_val_dim3_com4_s(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(5)
						call modifyTen_val_dim3_com8_s(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(6)
						call modifyTen_val_dim3_logi_s(dimen(1),dimen(2),dimen(3),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(7)
						call modifyTen_val_dim3_char_s(dimen(1),dimen(2),dimen(3),T%adata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
				end select
			case(4)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim4_int_s(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(2)
						call modifyTen_val_dim4_real4_s(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(3)
						call modifyTen_val_dim4_real8_s(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(4)
						call modifyTen_val_dim4_com4_s(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(5)
						call modifyTen_val_dim4_com8_s(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(6)
						call modifyTen_val_dim4_logi_s(dimen(1),dimen(2),dimen(3),dimen(4),T%ldata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(7)
						call modifyTen_val_dim4_char_s(dimen(1),dimen(2),dimen(3),dimen(4),T%adata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
				end select
			
		end select
		return
	end subroutine

	subroutine modify_TData_class_d(T,dimen,LDdimen,rank,newdata)
		class(TData),intent(inout)::T
		integer,intent(in)::rank
		integer,intent(in)::dimen(rank),LDdimen(rank)
		real(kind=8),intent(in)::newdata
		select case(rank)
			case(1)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim1_int_d(dimen(1),T%idata,T%totalData,newdata)
					case(2)
						call modifyTen_val_dim1_real4_d(dimen(1),T%sdata,T%totalData,newdata)	
					case(3)
						call modifyTen_val_dim1_real8_d(dimen(1),T%ddata,T%totalData,newdata)	
					case(4)
						call modifyTen_val_dim1_com4_d(dimen(1),T%cdata,T%totalData,newdata)	
					case(5)
						call modifyTen_val_dim1_com8_d(dimen(1),T%zdata,T%totalData,newdata)	
					case(6)
						call modifyTen_val_dim1_logi_d(dimen(1),T%ldata,T%totalData,newdata)	
					case(7)
						call modifyTen_val_dim1_char_d(dimen(1),T%adata,T%totalData,newdata)	
				end select
			case(2)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim2_int_d(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2),newdata)
					case(2)
						call modifyTen_val_dim2_real4_d(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2),newdata)	
					case(3)
						call modifyTen_val_dim2_real8_d(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2),newdata)	
					case(4)
						call modifyTen_val_dim2_com4_d(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2),newdata)	
					case(5)
						call modifyTen_val_dim2_com8_d(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2),newdata)	
					case(6)
						call modifyTen_val_dim2_logi_d(dimen(1),dimen(2),T%ldata,LDdimen(1),LDdimen(2),newdata)	
					case(7)
						call modifyTen_val_dim2_char_d(dimen(1),dimen(2),T%adata,LDdimen(1),LDdimen(2),newdata)	
				end select
			case(3)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim3_int_d(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(2)
						call modifyTen_val_dim3_real4_d(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(3)
						call modifyTen_val_dim3_real8_d(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(4)
						call modifyTen_val_dim3_com4_d(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(5)
						call modifyTen_val_dim3_com8_d(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(6)
						call modifyTen_val_dim3_logi_d(dimen(1),dimen(2),dimen(3),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(7)
						call modifyTen_val_dim3_char_d(dimen(1),dimen(2),dimen(3),T%adata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
				end select
			case(4)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim4_int_d(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(2)
						call modifyTen_val_dim4_real4_d(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(3)
						call modifyTen_val_dim4_real8_d(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(4)
						call modifyTen_val_dim4_com4_d(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(5)
						call modifyTen_val_dim4_com8_d(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(6)
						call modifyTen_val_dim4_logi_d(dimen(1),dimen(2),dimen(3),dimen(4),T%ldata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(7)
						call modifyTen_val_dim4_char_d(dimen(1),dimen(2),dimen(3),dimen(4),T%adata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
				end select
			
		end select
		return
	end subroutine

	subroutine modify_TData_class_c(T,dimen,LDdimen,rank,newdata)
		class(TData),intent(inout)::T
		integer,intent(in)::rank
		integer,intent(in)::dimen(rank),LDdimen(rank)
		complex(kind=4),intent(in)::newdata
		select case(rank)
			case(1)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim1_int_c(dimen(1),T%idata,T%totalData,newdata)
					case(2)
						call modifyTen_val_dim1_real4_c(dimen(1),T%sdata,T%totalData,newdata)	
					case(3)
						call modifyTen_val_dim1_real8_c(dimen(1),T%ddata,T%totalData,newdata)	
					case(4)
						call modifyTen_val_dim1_com4_c(dimen(1),T%cdata,T%totalData,newdata)	
					case(5)
						call modifyTen_val_dim1_com8_c(dimen(1),T%zdata,T%totalData,newdata)	
					case(6)
						call modifyTen_val_dim1_logi_c(dimen(1),T%ldata,T%totalData,newdata)	
					case(7)
						call modifyTen_val_dim1_char_c(dimen(1),T%adata,T%totalData,newdata)	
				end select
			case(2)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim2_int_c(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2),newdata)
					case(2)
						call modifyTen_val_dim2_real4_c(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2),newdata)	
					case(3)
						call modifyTen_val_dim2_real8_c(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2),newdata)	
					case(4)
						call modifyTen_val_dim2_com4_c(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2),newdata)	
					case(5)
						call modifyTen_val_dim2_com8_c(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2),newdata)	
					case(6)
						call modifyTen_val_dim2_logi_c(dimen(1),dimen(2),T%ldata,LDdimen(1),LDdimen(2),newdata)	
					case(7)
						call modifyTen_val_dim2_char_c(dimen(1),dimen(2),T%adata,LDdimen(1),LDdimen(2),newdata)	
				end select
			case(3)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim3_int_c(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(2)
						call modifyTen_val_dim3_real4_c(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(3)
						call modifyTen_val_dim3_real8_c(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(4)
						call modifyTen_val_dim3_com4_c(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(5)
						call modifyTen_val_dim3_com8_c(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(6)
						call modifyTen_val_dim3_logi_c(dimen(1),dimen(2),dimen(3),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(7)
						call modifyTen_val_dim3_char_c(dimen(1),dimen(2),dimen(3),T%adata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
				end select
			case(4)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim4_int_c(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(2)
						call modifyTen_val_dim4_real4_c(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(3)
						call modifyTen_val_dim4_real8_c(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(4)
						call modifyTen_val_dim4_com4_c(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(5)
						call modifyTen_val_dim4_com8_c(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(6)
						call modifyTen_val_dim4_logi_c(dimen(1),dimen(2),dimen(3),dimen(4),T%ldata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(7)
						call modifyTen_val_dim4_char_c(dimen(1),dimen(2),dimen(3),dimen(4),T%adata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
				end select
			
		end select
		return
	end subroutine

	subroutine modify_TData_class_z(T,dimen,LDdimen,rank,newdata)
		class(TData),intent(inout)::T
		integer,intent(in)::rank
		integer,intent(in)::dimen(rank),LDdimen(rank)
		complex(kind=8),intent(in)::newdata
		select case(rank)
			case(1)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim1_int_z(dimen(1),T%idata,T%totalData,newdata)
					case(2)
						call modifyTen_val_dim1_real4_z(dimen(1),T%sdata,T%totalData,newdata)	
					case(3)
						call modifyTen_val_dim1_real8_z(dimen(1),T%ddata,T%totalData,newdata)	
					case(4)
						call modifyTen_val_dim1_com4_z(dimen(1),T%cdata,T%totalData,newdata)	
					case(5)
						call modifyTen_val_dim1_com8_z(dimen(1),T%zdata,T%totalData,newdata)	
					case(6)
						call modifyTen_val_dim1_logi_z(dimen(1),T%ldata,T%totalData,newdata)	
					case(7)
						call modifyTen_val_dim1_char_z(dimen(1),T%adata,T%totalData,newdata)	
				end select
			case(2)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim2_int_z(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2),newdata)
					case(2)
						call modifyTen_val_dim2_real4_z(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2),newdata)	
					case(3)
						call modifyTen_val_dim2_real8_z(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2),newdata)	
					case(4)
						call modifyTen_val_dim2_com4_z(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2),newdata)	
					case(5)
						call modifyTen_val_dim2_com8_z(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2),newdata)	
					case(6)
						call modifyTen_val_dim2_logi_z(dimen(1),dimen(2),T%ldata,LDdimen(1),LDdimen(2),newdata)	
					case(7)
						call modifyTen_val_dim2_char_z(dimen(1),dimen(2),T%adata,LDdimen(1),LDdimen(2),newdata)	
				end select
			case(3)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim3_int_z(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(2)
						call modifyTen_val_dim3_real4_z(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(3)
						call modifyTen_val_dim3_real8_z(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(4)
						call modifyTen_val_dim3_com4_z(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(5)
						call modifyTen_val_dim3_com8_z(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(6)
						call modifyTen_val_dim3_logi_z(dimen(1),dimen(2),dimen(3),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(7)
						call modifyTen_val_dim3_char_z(dimen(1),dimen(2),dimen(3),T%adata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
				end select
			case(4)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim4_int_z(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(2)
						call modifyTen_val_dim4_real4_z(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(3)
						call modifyTen_val_dim4_real8_z(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(4)
						call modifyTen_val_dim4_com4_z(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(5)
						call modifyTen_val_dim4_com8_z(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(6)
						call modifyTen_val_dim4_logi_z(dimen(1),dimen(2),dimen(3),dimen(4),T%ldata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(7)
						call modifyTen_val_dim4_char_z(dimen(1),dimen(2),dimen(3),dimen(4),T%adata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
				end select
			
		end select
		return
	end subroutine

	subroutine modify_TData_class_l(T,dimen,LDdimen,rank,newdata)
		class(TData),intent(inout)::T
		integer,intent(in)::rank
		integer,intent(in)::dimen(rank),LDdimen(rank)
		logical,intent(in)::newdata
		select case(rank)
			case(1)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim1_int_l(dimen(1),T%idata,T%totalData,newdata)
					case(2)
						call modifyTen_val_dim1_real4_l(dimen(1),T%sdata,T%totalData,newdata)	
					case(3)
						call modifyTen_val_dim1_real8_l(dimen(1),T%ddata,T%totalData,newdata)	
					case(4)
						call modifyTen_val_dim1_com4_l(dimen(1),T%cdata,T%totalData,newdata)	
					case(5)
						call modifyTen_val_dim1_com8_l(dimen(1),T%zdata,T%totalData,newdata)	
					case(6)
						call modifyTen_val_dim1_logi_l(dimen(1),T%ldata,T%totalData,newdata)	
					case(7)
						call modifyTen_val_dim1_char_l(dimen(1),T%adata,T%totalData,newdata)	
				end select
			case(2)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim2_int_l(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2),newdata)
					case(2)
						call modifyTen_val_dim2_real4_l(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2),newdata)	
					case(3)
						call modifyTen_val_dim2_real8_l(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2),newdata)	
					case(4)
						call modifyTen_val_dim2_com4_l(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2),newdata)	
					case(5)
						call modifyTen_val_dim2_com8_l(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2),newdata)	
					case(6)
						call modifyTen_val_dim2_logi_l(dimen(1),dimen(2),T%ldata,LDdimen(1),LDdimen(2),newdata)	
					case(7)
						call modifyTen_val_dim2_char_l(dimen(1),dimen(2),T%adata,LDdimen(1),LDdimen(2),newdata)	
				end select
			case(3)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim3_int_l(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(2)
						call modifyTen_val_dim3_real4_l(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(3)
						call modifyTen_val_dim3_real8_l(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(4)
						call modifyTen_val_dim3_com4_l(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(5)
						call modifyTen_val_dim3_com8_l(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(6)
						call modifyTen_val_dim3_logi_l(dimen(1),dimen(2),dimen(3),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(7)
						call modifyTen_val_dim3_char_l(dimen(1),dimen(2),dimen(3),T%adata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
				end select
			case(4)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim4_int_l(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(2)
						call modifyTen_val_dim4_real4_l(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(3)
						call modifyTen_val_dim4_real8_l(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(4)
						call modifyTen_val_dim4_com4_l(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(5)
						call modifyTen_val_dim4_com8_l(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(6)
						call modifyTen_val_dim4_logi_l(dimen(1),dimen(2),dimen(3),dimen(4),T%ldata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(7)
						call modifyTen_val_dim4_char_l(dimen(1),dimen(2),dimen(3),dimen(4),T%adata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
				end select
			
		end select
		return
	end subroutine

	subroutine modify_TData_class_a(T,dimen,LDdimen,rank,newdata)
		class(TData),intent(inout)::T
		integer,intent(in)::rank
		integer,intent(in)::dimen(rank),LDdimen(rank)
		character(len=*),intent(in)::newdata
		select case(rank)
			case(1)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim1_int_a(dimen(1),T%idata,T%totalData,newdata)
					case(2)
						call modifyTen_val_dim1_real4_a(dimen(1),T%sdata,T%totalData,newdata)	
					case(3)
						call modifyTen_val_dim1_real8_a(dimen(1),T%ddata,T%totalData,newdata)	
					case(4)
						call modifyTen_val_dim1_com4_a(dimen(1),T%cdata,T%totalData,newdata)	
					case(5)
						call modifyTen_val_dim1_com8_a(dimen(1),T%zdata,T%totalData,newdata)	
					case(6)
						call modifyTen_val_dim1_logi_a(dimen(1),T%ldata,T%totalData,newdata)	
					case(7)
						call modifyTen_val_dim1_char_a(dimen(1),T%adata,T%totalData,newdata)	
				end select
			case(2)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim2_int_a(dimen(1),dimen(2),T%idata,LDdimen(1),LDdimen(2),newdata)
					case(2)
						call modifyTen_val_dim2_real4_a(dimen(1),dimen(2),T%sdata,LDdimen(1),LDdimen(2),newdata)	
					case(3)
						call modifyTen_val_dim2_real8_a(dimen(1),dimen(2),T%ddata,LDdimen(1),LDdimen(2),newdata)	
					case(4)
						call modifyTen_val_dim2_com4_a(dimen(1),dimen(2),T%cdata,LDdimen(1),LDdimen(2),newdata)	
					case(5)
						call modifyTen_val_dim2_com8_a(dimen(1),dimen(2),T%zdata,LDdimen(1),LDdimen(2),newdata)	
					case(6)
						call modifyTen_val_dim2_logi_a(dimen(1),dimen(2),T%ldata,LDdimen(1),LDdimen(2),newdata)	
					case(7)
						call modifyTen_val_dim2_char_a(dimen(1),dimen(2),T%adata,LDdimen(1),LDdimen(2),newdata)	
				end select
			case(3)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim3_int_a(dimen(1),dimen(2),dimen(3),T%idata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(2)
						call modifyTen_val_dim3_real4_a(dimen(1),dimen(2),dimen(3),T%sdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(3)
						call modifyTen_val_dim3_real8_a(dimen(1),dimen(2),dimen(3),T%ddata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(4)
						call modifyTen_val_dim3_com4_a(dimen(1),dimen(2),dimen(3),T%cdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(5)
						call modifyTen_val_dim3_com8_a(dimen(1),dimen(2),dimen(3),T%zdata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
					case(6)
						call modifyTen_val_dim3_logi_a(dimen(1),dimen(2),dimen(3),T%ldata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)
					case(7)
						call modifyTen_val_dim3_char_a(dimen(1),dimen(2),dimen(3),T%adata,LDdimen(1),LDdimen(2),LDdimen(3),newdata)	
				end select
			case(4)
				select case (T%classtype)
					case(1)
						call modifyTen_val_dim4_int_a(dimen(1),dimen(2),dimen(3),dimen(4),T%idata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(2)
						call modifyTen_val_dim4_real4_a(dimen(1),dimen(2),dimen(3),dimen(4),T%sdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(3)
						call modifyTen_val_dim4_real8_a(dimen(1),dimen(2),dimen(3),dimen(4),T%ddata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(4)
						call modifyTen_val_dim4_com4_a(dimen(1),dimen(2),dimen(3),dimen(4),T%cdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(5)
						call modifyTen_val_dim4_com8_a(dimen(1),dimen(2),dimen(3),dimen(4),T%zdata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(6)
						call modifyTen_val_dim4_logi_a(dimen(1),dimen(2),dimen(3),dimen(4),T%ldata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
					case(7)
						call modifyTen_val_dim4_char_a(dimen(1),dimen(2),dimen(3),dimen(4),T%adata,LDdimen(1),LDdimen(2),&
																	LDdimen(3),LDdimen(4),newdata)
				end select
			
		end select
		return
	end subroutine




! !A(i:j)=B(i:j)

	subroutine modify_Some_TData_class1_i(T,Tdimen,ia,newdata,Ndimen,ib)
		integer,intent(in)::Tdimen,Ndimen,ia(2),ib(2)
		type(TData),intent(inout)::T
		integer,intent(in)::newdata(Ndimen)
		select case(T%classtype)
			case(1)
				call store_value_int_i(T%idata,Tdimen,ia,newdata,Ndimen,ib)
			case(2)
				call store_value_real4_i(T%sdata,Tdimen,ia,newdata,Ndimen,ib)
			case(3)
				call store_value_real8_i(T%ddata,Tdimen,ia,newdata,Ndimen,ib)
			case(4)
				call store_value_com4_i(T%cdata,Tdimen,ia,newdata,Ndimen,ib)
			case(5)
				call store_value_com8_i(T%zdata,Tdimen,ia,newdata,Ndimen,ib)
			case(6)	
				call store_value_logi_i(T%ldata,Tdimen,ia,newdata,Ndimen,ib)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class1_s(T,Tdimen,ia,newdata,Ndimen,ib)
		integer,intent(in)::Tdimen,Ndimen,ia(2),ib(2)
		type(TData),intent(inout)::T
		real(kind=4),intent(in)::newdata(Ndimen)
		select case(T%classtype)
			case(1)
				call store_value_int_s(T%idata,Tdimen,ia,newdata,Ndimen,ib)
			case(2)
				call store_value_real4_s(T%sdata,Tdimen,ia,newdata,Ndimen,ib)
			case(3)
				call store_value_real8_s(T%ddata,Tdimen,ia,newdata,Ndimen,ib)
			case(4)
				call store_value_com4_s(T%cdata,Tdimen,ia,newdata,Ndimen,ib)
			case(5)
				call store_value_com8_s(T%zdata,Tdimen,ia,newdata,Ndimen,ib)
			case(6)	
				call store_value_logi_s(T%ldata,Tdimen,ia,newdata,Ndimen,ib)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class1_d(T,Tdimen,ia,newdata,Ndimen,ib)
		integer,intent(in)::Tdimen,Ndimen,ia(2),ib(2)
		type(TData),intent(inout)::T
		real(kind=8),intent(in)::newdata(Ndimen)
		select case(T%classtype)
			case(1)
				call store_value_int_d(T%idata,Tdimen,ia,newdata,Ndimen,ib)
			case(2)
				call store_value_real4_d(T%sdata,Tdimen,ia,newdata,Ndimen,ib)
			case(3)
				call store_value_real8_d(T%ddata,Tdimen,ia,newdata,Ndimen,ib)
			case(4)
				call store_value_com4_d(T%cdata,Tdimen,ia,newdata,Ndimen,ib)
			case(5)
				call store_value_com8_d(T%zdata,Tdimen,ia,newdata,Ndimen,ib)
			case(6)	
				call store_value_logi_d(T%ldata,Tdimen,ia,newdata,Ndimen,ib)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class1_c(T,Tdimen,ia,newdata,Ndimen,ib)
		integer,intent(in)::Tdimen,Ndimen,ia(2),ib(2)
		type(TData),intent(inout)::T
		complex(kind=4),intent(in)::newdata(Ndimen)
		select case(T%classtype)
			case(1)
				call store_value_int_c(T%idata,Tdimen,ia,newdata,Ndimen,ib)
			case(2)
				call store_value_real4_c(T%sdata,Tdimen,ia,newdata,Ndimen,ib)
			case(3)
				call store_value_real8_c(T%ddata,Tdimen,ia,newdata,Ndimen,ib)
			case(4)
				call store_value_com4_c(T%cdata,Tdimen,ia,newdata,Ndimen,ib)
			case(5)
				call store_value_com8_c(T%zdata,Tdimen,ia,newdata,Ndimen,ib)
			case(6)	
				call store_value_logi_c(T%ldata,Tdimen,ia,newdata,Ndimen,ib)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class1_z(T,Tdimen,ia,newdata,Ndimen,ib)
		integer,intent(in)::Tdimen,Ndimen,ia(2),ib(2)
		type(TData),intent(inout)::T
		complex(kind=8),intent(in)::newdata(Ndimen)
		select case(T%classtype)
			case(1)
				call store_value_int_z(T%idata,Tdimen,ia,newdata,Ndimen,ib)
			case(2)
				call store_value_real4_z(T%sdata,Tdimen,ia,newdata,Ndimen,ib)
			case(3)
				call store_value_real8_z(T%ddata,Tdimen,ia,newdata,Ndimen,ib)
			case(4)
				call store_value_com4_z(T%cdata,Tdimen,ia,newdata,Ndimen,ib)
			case(5)
				call store_value_com8_z(T%zdata,Tdimen,ia,newdata,Ndimen,ib)
			case(6)	
				call store_value_logi_z(T%ldata,Tdimen,ia,newdata,Ndimen,ib)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class1_l(T,Tdimen,ia,newdata,Ndimen,ib)
		integer,intent(in)::Tdimen,Ndimen,ia(2),ib(2)
		type(TData),intent(inout)::T
		logical,intent(in)::newdata(Ndimen)
		select case(T%classtype)
			case(1)
				call store_value_int_l(T%idata,Tdimen,ia,newdata,Ndimen,ib)
			case(2)
				call store_value_real4_l(T%sdata,Tdimen,ia,newdata,Ndimen,ib)
			case(3)
				call store_value_real8_l(T%ddata,Tdimen,ia,newdata,Ndimen,ib)
			case(4)
				call store_value_com4_l(T%cdata,Tdimen,ia,newdata,Ndimen,ib)
			case(5)
				call store_value_com8_l(T%zdata,Tdimen,ia,newdata,Ndimen,ib)
			case(6)	
				call store_value_logi_l(T%ldata,Tdimen,ia,newdata,Ndimen,ib)
			case(7)	
				call store_value_char_l(T%adata,Tdimen,ia,newdata,Ndimen,ib)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class1_a(T,Tdimen,ia,newdata,Ndimen,ib)
		integer,intent(in)::Tdimen,Ndimen,ia(2),ib(2)
		type(TData),intent(inout)::T
		character(len=*),intent(in)::newdata(Ndimen)
		select case(T%classtype)
			case(1)
				call store_value_int_a(T%idata,Tdimen,ia,newdata,Ndimen,ib)
			case(2)
				call store_value_real4_a(T%sdata,Tdimen,ia,newdata,Ndimen,ib)
			case(3)
				call store_value_real8_a(T%ddata,Tdimen,ia,newdata,Ndimen,ib)
			case(4)
				call store_value_com4_a(T%cdata,Tdimen,ia,newdata,Ndimen,ib)
			case(5)
				call store_value_com8_a(T%zdata,Tdimen,ia,newdata,Ndimen,ib)
			case(6)	
				call store_value_logi_a(T%ldata,Tdimen,ia,newdata,Ndimen,ib)
			case(7)	
				call store_value_char_a(T%adata,Tdimen,ia,newdata,Ndimen,ib)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_TData1(T,Tdimen,ia,B,Ndimen,ib)
		integer,intent(in)::Tdimen,Ndimen,ia(2),ib(2)
		type(TData),intent(inout)::T
		type(TData),intent(in)::B
		select case(B%classtype)
			case(1)
				call modify_Some_TData_class1(T,Tdimen,ia,B%idata,Ndimen,ib)
			case(2)
				call modify_Some_TData_class1(T,Tdimen,ia,B%sdata,Ndimen,ib)
			case(3)
				call modify_Some_TData_class1(T,Tdimen,ia,B%ddata,Ndimen,ib)
			case(4)
				call modify_Some_TData_class1(T,Tdimen,ia,B%cdata,Ndimen,ib)
			case(5)
				call modify_Some_TData_class1(T,Tdimen,ia,B%zdata,Ndimen,ib)
			case(6)	
				call modify_Some_TData_class1(T,Tdimen,ia,B%ldata,Ndimen,ib)
			case(7)	
				call modify_Some_TData_class1(T,Tdimen,ia,B%adata,Ndimen,ib)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

! !A(i1:i2,j1:j2)=B(i1:i2,j1:j2)	

	subroutine modify_Some_TData_class2_i(T,TD,ia,ja,newdata,ND,ib,jb)
		integer,intent(in)::TD(2),ND(2),ia(2),ja(2),ib(2),jb(2)
		type(TData),intent(inout)::T
		integer,intent(in)::newdata(ND(1),ND(2))
		select case(T%classtype)
			case(1)
				call store_value2_int_i(T%idata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(2)
				call store_value2_real4_i(T%sdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(3)
				call store_value2_real8_i(T%ddata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(4)
				call store_value2_com4_i(T%cdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(5)
				call store_value2_com8_i(T%zdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(6)	
				call store_value2_logi_i(T%ldata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(7)	
				call store_value2_char_i(T%adata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class2_s(T,TD,ia,ja,newdata,ND,ib,jb)
		integer,intent(in)::TD(2),ND(2),ia(2),ja(2),ib(2),jb(2)
		type(TData),intent(inout)::T
		real(kind=4),intent(in)::newdata(ND(1),ND(2))
		select case(T%classtype)
			case(1)
				call store_value2_int_s(T%idata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(2)
				call store_value2_real4_s(T%sdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(3)
				call store_value2_real8_s(T%ddata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(4)
				call store_value2_com4_s(T%cdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(5)
				call store_value2_com8_s(T%zdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(6)	
				call store_value2_logi_s(T%ldata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(7)	
				call store_value2_char_s(T%adata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class2_d(T,TD,ia,ja,newdata,ND,ib,jb)
		integer,intent(in)::TD(2),ND(2),ia(2),ja(2),ib(2),jb(2)
		type(TData),intent(inout)::T
		real(kind=8),intent(in)::newdata(ND(1),ND(2))
		select case(T%classtype)
			case(1)
				call store_value2_int_d(T%idata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(2)
				call store_value2_real4_d(T%sdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(3)
				call store_value2_real8_d(T%ddata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(4)
				call store_value2_com4_d(T%cdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(5)
				call store_value2_com8_d(T%zdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(6)	
				call store_value2_logi_d(T%ldata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(7)	
				call store_value2_char_d(T%adata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class2_c(T,TD,ia,ja,newdata,ND,ib,jb)
		integer,intent(in)::TD(2),ND(2),ia(2),ja(2),ib(2),jb(2)
		type(TData),intent(inout)::T
		complex(kind=4),intent(in)::newdata(ND(1),ND(2))
		select case(T%classtype)
			case(1)
				call store_value2_int_c(T%idata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(2)
				call store_value2_real4_c(T%sdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(3)
				call store_value2_real8_c(T%ddata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(4)
				call store_value2_com4_c(T%cdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(5)
				call store_value2_com8_c(T%zdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(6)	
				call store_value2_logi_c(T%ldata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(7)	
				call store_value2_char_c(T%adata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class2_z(T,TD,ia,ja,newdata,ND,ib,jb)
		integer,intent(in)::TD(2),ND(2),ia(2),ja(2),ib(2),jb(2)
		type(TData),intent(inout)::T
		complex(kind=8),intent(in)::newdata(ND(1),ND(2))
		select case(T%classtype)
			case(1)
				call store_value2_int_z(T%idata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(2)
				call store_value2_real4_z(T%sdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(3)
				call store_value2_real8_z(T%ddata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(4)
				call store_value2_com4_z(T%cdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(5)
				call store_value2_com8_z(T%zdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(6)	
				call store_value2_logi_z(T%ldata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(7)	
				call store_value2_char_z(T%adata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class2_l(T,TD,ia,ja,newdata,ND,ib,jb)
		integer,intent(in)::TD(2),ND(2),ia(2),ja(2),ib(2),jb(2)
		type(TData),intent(inout)::T
		logical,intent(in)::newdata(ND(1),ND(2))
		select case(T%classtype)
			case(1)
				call store_value2_int_l(T%idata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(2)
				call store_value2_real4_l(T%sdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(3)
				call store_value2_real8_l(T%ddata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(4)
				call store_value2_com4_l(T%cdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(5)
				call store_value2_com8_l(T%zdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(6)	
				call store_value2_logi_l(T%ldata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(7)	
				call store_value2_char_l(T%adata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class2_a(T,TD,ia,ja,newdata,ND,ib,jb)
		integer,intent(in)::TD(2),ND(2),ia(2),ja(2),ib(2),jb(2)
		type(TData),intent(inout)::T
		character(len=8),intent(in)::newdata(ND(1),ND(2))
		select case(T%classtype)
			case(1)
				call store_value2_int_a(T%idata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(2)
				call store_value2_real4_a(T%sdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(3)
				call store_value2_real8_a(T%ddata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(4)
				call store_value2_com4_a(T%cdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(5)
				call store_value2_com8_a(T%zdata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(6)	
				call store_value2_logi_a(T%ldata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case(7)	
				call store_value2_char_a(T%adata,TD(1),TD(2),ia,ja,newdata,ND(1),ND(2),ib,jb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine
	
	subroutine modify_Some_TData_TData2(T,TD,ia,ja,B,ND,ib,jb)
		integer,intent(in)::TD(2),ND(2),ia(2),ja(2),ib(2),jb(2)
		type(TData),intent(inout)::T
		type(TData),intent(in)::B
		select case(T%classtype)
			case(1)
				select case(B%classtype)
					case(1)
						call store_value2_int_i(T%idata,TD(1),TD(2),ia,ja,B%idata,ND(1),ND(2),ib,jb)
					case(2)
						call store_value2_int_s(T%idata,TD(1),TD(2),ia,ja,B%sdata,ND(1),ND(2),ib,jb)
					case(3)
						call store_value2_int_d(T%idata,TD(1),TD(2),ia,ja,B%ddata,ND(1),ND(2),ib,jb)
					case(4)
						call store_value2_int_c(T%idata,TD(1),TD(2),ia,ja,B%cdata,ND(1),ND(2),ib,jb)
					case(5)
						call store_value2_int_z(T%idata,TD(1),TD(2),ia,ja,B%zdata,ND(1),ND(2),ib,jb)
					case default
						call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
						call error_stop()
				end select
				
			case(2)
				select case(B%classtype)
					case(1)
						call store_value2_real4_i(T%sdata,TD(1),TD(2),ia,ja,B%idata,ND(1),ND(2),ib,jb)
					case(2)
						call store_value2_real4_s(T%sdata,TD(1),TD(2),ia,ja,B%sdata,ND(1),ND(2),ib,jb)
					case(3)
						call store_value2_real4_d(T%sdata,TD(1),TD(2),ia,ja,B%ddata,ND(1),ND(2),ib,jb)
					case(4)
						call store_value2_real4_c(T%sdata,TD(1),TD(2),ia,ja,B%cdata,ND(1),ND(2),ib,jb)
					case(5)
						call store_value2_real4_z(T%sdata,TD(1),TD(2),ia,ja,B%zdata,ND(1),ND(2),ib,jb)
					case default
						call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
						call error_stop()
				end select
			
			case(3)
				select case(B%classtype)
					case(1)
						call store_value2_real8_i(T%ddata,TD(1),TD(2),ia,ja,B%idata,ND(1),ND(2),ib,jb)
					case(2)
						call store_value2_real8_s(T%ddata,TD(1),TD(2),ia,ja,B%sdata,ND(1),ND(2),ib,jb)
					case(3)
						call store_value2_real8_d(T%ddata,TD(1),TD(2),ia,ja,B%ddata,ND(1),ND(2),ib,jb)
					case(4)
						call store_value2_real8_c(T%ddata,TD(1),TD(2),ia,ja,B%cdata,ND(1),ND(2),ib,jb)
					case(5)
						call store_value2_real8_z(T%ddata,TD(1),TD(2),ia,ja,B%zdata,ND(1),ND(2),ib,jb)
					case default
						call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
						call error_stop()
				end select
			
			case(4)
				select case(B%classtype)
					case(1)
						call store_value2_com4_i(T%cdata,TD(1),TD(2),ia,ja,B%idata,ND(1),ND(2),ib,jb)
					case(2)
						call store_value2_com4_s(T%cdata,TD(1),TD(2),ia,ja,B%sdata,ND(1),ND(2),ib,jb)
					case(3)
						call store_value2_com4_d(T%cdata,TD(1),TD(2),ia,ja,B%ddata,ND(1),ND(2),ib,jb)
					case(4)
						call store_value2_com4_c(T%cdata,TD(1),TD(2),ia,ja,B%cdata,ND(1),ND(2),ib,jb)
					case(5)
						call store_value2_com4_z(T%cdata,TD(1),TD(2),ia,ja,B%zdata,ND(1),ND(2),ib,jb)
					case default
						call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
						call error_stop()
				end select	
			
			
			case(5)
				select case(B%classtype)
					case(1)
						call store_value2_com8_i(T%zdata,TD(1),TD(2),ia,ja,B%idata,ND(1),ND(2),ib,jb)
					case(2)
						call store_value2_com8_s(T%zdata,TD(1),TD(2),ia,ja,B%sdata,ND(1),ND(2),ib,jb)
					case(3)
						call store_value2_com8_d(T%zdata,TD(1),TD(2),ia,ja,B%ddata,ND(1),ND(2),ib,jb)
					case(4)
						call store_value2_com8_c(T%zdata,TD(1),TD(2),ia,ja,B%cdata,ND(1),ND(2),ib,jb)
					case(5)
						call store_value2_com8_z(T%zdata,TD(1),TD(2),ia,ja,B%zdata,ND(1),ND(2),ib,jb)
					case default
						call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
						call error_stop()
				end select	
				
			case(6)
				select case(B%classtype)
				case(6)
						call store_value2_logi_l(T%ldata,TD(1),TD(2),ia,ja,B%ldata,ND(1),ND(2),ib,jb)
					case default
						call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
						call error_stop()
				end select	
			
			case(7)
				select case(B%classtype)
					case(1)
						call store_value2_char_i(T%adata,TD(1),TD(2),ia,ja,B%idata,ND(1),ND(2),ib,jb)
					case(2)
						call store_value2_char_s(T%adata,TD(1),TD(2),ia,ja,B%sdata,ND(1),ND(2),ib,jb)
					case(3)
						call store_value2_char_d(T%adata,TD(1),TD(2),ia,ja,B%ddata,ND(1),ND(2),ib,jb)
					case(4)
						call store_value2_char_c(T%adata,TD(1),TD(2),ia,ja,B%cdata,ND(1),ND(2),ib,jb)
					case(5)
						call store_value2_char_z(T%adata,TD(1),TD(2),ia,ja,B%zdata,ND(1),ND(2),ib,jb)
					case(6)
						call store_value2_char_l(T%adata,TD(1),TD(2),ia,ja,B%ldata,ND(1),ND(2),ib,jb)
					case(7)
						call store_value2_char_a(T%adata,TD(1),TD(2),ia,ja,B%adata,ND(1),ND(2),ib,jb)
					case default
						call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
						call error_stop()
				end select	
				
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine
	
	
	
	
	
	
	
	subroutine modify_Some_TData_class3_i(T,TD,ia,ja,ka,newdata,ND,ib,jb,kb)
		integer,intent(in)::TD(3),ND(3),ia(2),ja(2),ka(2),ib(2),jb(2),kb(2)
		type(TData),intent(inout)::T
		integer,intent(in)::newdata(ND(1),ND(2),ND(3))
		select case(T%classtype)
			case(1)
				call store_value3_int_i(T%idata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(2)
				call store_value3_real4_i(T%sdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(3)
				call store_value3_real8_i(T%ddata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(4)
				call store_value3_com4_i(T%cdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(5)
				call store_value3_com8_i(T%zdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(6)	
				call store_value3_logi_i(T%ldata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(7)	
				call store_value3_char_i(T%adata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class3_s(T,TD,ia,ja,ka,newdata,ND,ib,jb,kb)
		integer,intent(in)::TD(3),ND(3),ia(2),ja(2),ka(2),ib(2),jb(2),kb(2)
		type(TData),intent(inout)::T
		real(kind=4),intent(in)::newdata(ND(1),ND(2),ND(3))
		select case(T%classtype)
			case(1)
				call store_value3_int_s(T%idata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(2)
				call store_value3_real4_s(T%sdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(3)
				call store_value3_real8_s(T%ddata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(4)
				call store_value3_com4_s(T%cdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(5)
				call store_value3_com8_s(T%zdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(6)	
				call store_value3_logi_s(T%ldata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(7)	
				call store_value3_char_s(T%adata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class3_d(T,TD,ia,ja,ka,newdata,ND,ib,jb,kb)
		integer,intent(in)::TD(3),ND(3),ia(2),ja(2),ka(2),ib(2),jb(2),kb(2)
		type(TData),intent(inout)::T
		real(kind=8),intent(in)::newdata(ND(1),ND(2),ND(3))
		select case(T%classtype)
			case(1)
				call store_value3_int_d(T%idata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(2)
				call store_value3_real4_d(T%sdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(3)
				call store_value3_real8_d(T%ddata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(4)
				call store_value3_com4_d(T%cdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(5)
				call store_value3_com8_d(T%zdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(6)	
				call store_value3_logi_d(T%ldata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(7)	
				call store_value3_char_d(T%adata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class3_c(T,TD,ia,ja,ka,newdata,ND,ib,jb,kb)
		integer,intent(in)::TD(3),ND(3),ia(2),ja(2),ka(2),ib(2),jb(2),kb(2)
		type(TData),intent(inout)::T
		complex(kind=4),intent(in)::newdata(ND(1),ND(2),ND(3))
		select case(T%classtype)
			case(1)
				call store_value3_int_c(T%idata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(2)
				call store_value3_real4_c(T%sdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(3)
				call store_value3_real8_c(T%ddata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(4)
				call store_value3_com4_c(T%cdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(5)
				call store_value3_com8_c(T%zdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(6)	
				call store_value3_logi_c(T%ldata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(7)	
				call store_value3_char_c(T%adata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class3_z(T,TD,ia,ja,ka,newdata,ND,ib,jb,kb)
		integer,intent(in)::TD(3),ND(3),ia(2),ja(2),ka(2),ib(2),jb(2),kb(2)
		type(TData),intent(inout)::T
		complex(kind=8),intent(in)::newdata(ND(1),ND(2),ND(3))
		select case(T%classtype)
			case(1)
				call store_value3_int_z(T%idata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(2)
				call store_value3_real4_z(T%sdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(3)
				call store_value3_real8_z(T%ddata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(4)
				call store_value3_com4_z(T%cdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(5)
				call store_value3_com8_z(T%zdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(6)	
				call store_value3_logi_z(T%ldata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(7)	
				call store_value3_char_z(T%adata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class3_l(T,TD,ia,ja,ka,newdata,ND,ib,jb,kb)
		integer,intent(in)::TD(3),ND(3),ia(2),ja(2),ka(2),ib(2),jb(2),kb(2)
		type(TData),intent(inout)::T
		logical,intent(in)::newdata(ND(1),ND(2),ND(3))
		select case(T%classtype)
			case(1)
				call store_value3_int_l(T%idata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(2)
				call store_value3_real4_l(T%sdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(3)
				call store_value3_real8_l(T%ddata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(4)
				call store_value3_com4_l(T%cdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(5)
				call store_value3_com8_l(T%zdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(6)	
				call store_value3_logi_l(T%ldata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(7)	
				call store_value3_char_l(T%adata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class3_a(T,TD,ia,ja,ka,newdata,ND,ib,jb,kb)
		integer,intent(in)::TD(3),ND(3),ia(2),ja(2),ka(2),ib(2),jb(2),kb(2)
		type(TData),intent(inout)::T
		character(len=*),intent(in)::newdata(ND(1),ND(2),ND(3))
		select case(T%classtype)
			case(1)
				call store_value3_int_a(T%idata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(2)
				call store_value3_real4_a(T%sdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(3)
				call store_value3_real8_a(T%ddata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(4)
				call store_value3_com4_a(T%cdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(5)
				call store_value3_com8_a(T%zdata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(6)	
				call store_value3_logi_a(T%ldata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case(7)	
				call store_value3_char_a(T%adata,TD(1),TD(2),TD(3),ia,ja,ka,newdata,ND(1),ND(2),ND(3),ib,jb,kb)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine
	
	subroutine modify_Some_TData_class4_i(T,TD,ia,ja,ka,la,newdata,ND,ib,jb,kb,lb)
		integer,intent(in)::TD(4),ND(4),ia(2),ja(2),ka(2),la(2),ib(2),jb(2),kb(2),lb(2)
		type(TData),intent(inout)::T
		integer,intent(in)::newdata(ND(1),ND(2),ND(3),ND(4))
		select case(T%classtype)
			case(1)
				call store_value4_int_i(T%idata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(2)
				call store_value4_real4_i(T%sdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(3)
				call store_value4_real8_i(T%ddata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(4)
				call store_value4_com4_i(T%cdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(5)
				call store_value4_com8_i(T%zdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(6)	
				call store_value4_logi_i(T%ldata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(7)	
				call store_value4_char_i(T%adata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class4_s(T,TD,ia,ja,ka,la,newdata,ND,ib,jb,kb,lb)
		integer,intent(in)::TD(4),ND(4),ia(2),ja(2),ka(2),la(2),ib(2),jb(2),kb(2),lb(2)
		type(TData),intent(inout)::T
		real(kind=4),intent(in)::newdata(ND(1),ND(2),ND(3),ND(4))
		select case(T%classtype)
			case(1)
				call store_value4_int_s(T%idata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(2)
				call store_value4_real4_s(T%sdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(3)
				call store_value4_real8_s(T%ddata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(4)
				call store_value4_com4_s(T%cdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(5)
				call store_value4_com8_s(T%zdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(6)	
				call store_value4_logi_s(T%ldata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(7)	
				call store_value4_char_s(T%adata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class4_d(T,TD,ia,ja,ka,la,newdata,ND,ib,jb,kb,lb)
		integer,intent(in)::TD(4),ND(4),ia(2),ja(2),ka(2),la(2),ib(2),jb(2),kb(2),lb(2)
		type(TData),intent(inout)::T
		real(kind=8),intent(in)::newdata(ND(1),ND(2),ND(3),ND(4))
		select case(T%classtype)
			case(1)
				call store_value4_int_d(T%idata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(2)
				call store_value4_real4_d(T%sdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(3)
				call store_value4_real8_d(T%ddata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(4)
				call store_value4_com4_d(T%cdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(5)
				call store_value4_com8_d(T%zdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(6)	
				call store_value4_logi_d(T%ldata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(7)	
				call store_value4_char_d(T%adata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class4_c(T,TD,ia,ja,ka,la,newdata,ND,ib,jb,kb,lb)
		integer,intent(in)::TD(4),ND(4),ia(2),ja(2),ka(2),la(2),ib(2),jb(2),kb(2),lb(2)
		type(TData),intent(inout)::T
		complex(kind=4),intent(in)::newdata(ND(1),ND(2),ND(3),ND(4))
		select case(T%classtype)
			case(1)
				call store_value4_int_c(T%idata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(2)
				call store_value4_real4_c(T%sdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(3)
				call store_value4_real8_c(T%ddata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(4)
				call store_value4_com4_c(T%cdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(5)
				call store_value4_com8_c(T%zdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(6)	
				call store_value4_logi_c(T%ldata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(7)	
				call store_value4_char_c(T%adata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class4_z(T,TD,ia,ja,ka,la,newdata,ND,ib,jb,kb,lb)
		integer,intent(in)::TD(4),ND(4),ia(2),ja(2),ka(2),la(2),ib(2),jb(2),kb(2),lb(2)
		type(TData),intent(inout)::T
		complex(kind=8),intent(in)::newdata(ND(1),ND(2),ND(3),ND(4))
		select case(T%classtype)
			case(1)
				call store_value4_int_z(T%idata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(2)
				call store_value4_real4_z(T%sdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(3)
				call store_value4_real8_z(T%ddata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(4)
				call store_value4_com4_z(T%cdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(5)
				call store_value4_com8_z(T%zdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(6)	
				call store_value4_logi_z(T%ldata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(7)	
				call store_value4_char_z(T%adata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class4_l(T,TD,ia,ja,ka,la,newdata,ND,ib,jb,kb,lb)
		integer,intent(in)::TD(4),ND(4),ia(2),ja(2),ka(2),la(2),ib(2),jb(2),kb(2),lb(2)
		type(TData),intent(inout)::T
		logical,intent(in)::newdata(ND(1),ND(2),ND(3),ND(4))
		select case(T%classtype)
			case(1)
				call store_value4_int_l(T%idata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(2)
				call store_value4_real4_l(T%sdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(3)
				call store_value4_real8_l(T%ddata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(4)
				call store_value4_com4_l(T%cdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(5)
				call store_value4_com8_l(T%zdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(6)	
				call store_value4_logi_l(T%ldata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(7)	
				call store_value4_char_l(T%adata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
				call error_stop()
		end select
		return
	end subroutine

	subroutine modify_Some_TData_class4_a(T,TD,ia,ja,ka,la,newdata,ND,ib,jb,kb,lb)
		integer,intent(in)::TD(4),ND(4),ia(2),ja(2),ka(2),la(2),ib(2),jb(2),kb(2),lb(2)
		type(TData),intent(inout)::T
		character(len=*),intent(in)::newdata(ND(1),ND(2),ND(3),ND(4))
		select case(T%classtype)
			case(1)
				call store_value4_int_a(T%idata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(2)
				call store_value4_real4_a(T%sdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(3)
				call store_value4_real8_a(T%ddata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(4)
				call store_value4_com4_a(T%cdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(5)
				call store_value4_com8_a(T%zdata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(6)	
				call store_value4_logi_a(T%ldata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case(7)	
				call store_value4_char_a(T%adata,TD(1),TD(2),TD(3),TD(4),ia,ja,ka,la,newdata,ND(1),ND(2),ND(3),ND(4),ib,jb,kb,la)
			case default
				call writemess('ERROR in classtype when set_value in Tdata.f90',-1)
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
	integer function select_type_in_add_minu_class_type(classtype1,classtype2)result(Res)
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
	integer function select_type_in_add_minu1(T1,T2)result(Res)
		type(TData),intent(in)::T1,T2
		Res=select_type_in_add_minu_class_type(T1%classtype,T2%classtype)
		return
	end function
	integer function select_type_in_add_minu2(T1,num)result(Res)
		type(TData),intent(in)::T1
		integer,intent(in)::num
		Res=select_type_in_add_minu_class_type(T1%classtype,num)
		return
	end function

!int+classtype ---> max{1,classtype}
	subroutine add_minu_TData_int(outTData,A,value,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		integer,intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(1)
				if(num.gt.0)then
					outTData%idata=A%idata(1:length)+value
				else
					outTData%idata=A%idata(1:length)-value
				end if
			case(2)
				if(num.gt.0)then
					outTData%sdata=A%sdata(1:length)+value
				else
					outTData%sdata=A%sdata(1:length)-value
				end if
			case(3)
				if(num.gt.0)then
					outTData%ddata=A%ddata(1:length)+value
				else
					outTData%ddata=A%ddata(1:length)-value
				end if
			case(4)
				if(num.gt.0)then
					outTData%cdata=A%cdata(1:length)+value
				else
					outTData%cdata=A%cdata(1:length)-value
				end if
			case(5)
				if(num.gt.0)then
					outTData%zdata=A%zdata(1:length)+value
				else
					outTData%zdata=A%zdata(1:length)-value
				end if
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%adata(i)+value
					end do
				end if
		end select
		return
	end subroutine
!real4+classtype ---> max{2,classtype}
	subroutine add_minu_TData_real4(outTData,A,value,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		real(kind=4),intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(1)
				if(num.gt.0)then
					outTData%sdata=A%idata(1:length)+value
				else
					outTData%sdata=A%idata(1:length)-value
				end if
			case(2)
				if(num.gt.0)then
					outTData%sdata=A%sdata(1:length)+value
				else
					outTData%sdata=A%sdata(1:length)-value
				end if
			case(3)
				if(num.gt.0)then
					outTData%ddata=A%ddata(1:length)+value
				else
					outTData%ddata=A%ddata(1:length)-value
				end if
			case(4)
				if(num.gt.0)then
					outTData%cdata=A%cdata(1:length)+value
				else
					outTData%cdata=A%cdata(1:length)-value
				end if
			case(5)
				if(num.gt.0)then
					outTData%zdata=A%zdata(1:length)+value
				else
					outTData%zdata=A%zdata(1:length)-value
				end if
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%adata(i)+value
					end do
				end if
		end select
		return
	end subroutine
!real8+classtype ---> depend on 3 and classtype
	subroutine add_minu_TData_real8(outTData,A,value,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		real(kind=8),intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(1)
				if(num.gt.0)then
					outTData%ddata=A%idata(1:length)+value
				else
					outTData%ddata=A%idata(1:length)-value
				end if
			case(2)
				if(num.gt.0)then
					outTData%ddata=A%sdata(1:length)+value
				else
					outTData%ddata=A%sdata(1:length)-value
				end if
			case(3)
				if(num.gt.0)then
					outTData%ddata=A%ddata(1:length)+value
				else
					outTData%ddata=A%ddata(1:length)-value
				end if
			case(4)
				if(num.gt.0)then
					outTData%zdata=A%cdata(1:length)+value
				else
					outTData%zdata=A%cdata(1:length)-value
				end if
			case(5)
				if(num.gt.0)then
					outTData%zdata=A%zdata(1:length)+value
				else
					outTData%zdata=A%zdata(1:length)-value
				end if
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%adata(i)+value
					end do
				end if
		end select
		return
	end subroutine
!com4+classtype ---> depend on 4 and classtype
	subroutine add_minu_TData_com4(outTData,A,value,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		complex(kind=4),intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(1)
				if(num.gt.0)then
					outTData%cdata=A%idata(1:length)+value
				else
					outTData%cdata=A%idata(1:length)-value
				end if
			case(2)
				if(num.gt.0)then
					outTData%cdata=A%sdata(1:length)+value
				else
					outTData%cdata=A%sdata(1:length)-value
				end if
			case(3)
				if(num.gt.0)then
					outTData%zdata=A%ddata(1:length)+value
				else
					outTData%zdata=A%ddata(1:length)-value
				end if
			case(4)
				if(num.gt.0)then
					outTData%cdata=A%cdata(1:length)+value
				else
					outTData%cdata=A%cdata(1:length)-value
				end if
			case(5)
				if(num.gt.0)then
					outTData%zdata=A%zdata(1:length)+value
				else
					outTData%zdata=A%zdata(1:length)-value
				end if
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%adata(i)+value
					end do
				end if
		end select
		return
	end subroutine
!complex*16+classtype ---> max{5,classtype}
	subroutine add_minu_TData_com8(outTData,A,value,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		complex(kind=8),intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(1)
				if(num.gt.0)then
					outTData%zdata=A%idata(1:length)+value
				else
					outTData%zdata=A%idata(1:length)-value
				end if
			case(2)
				if(num.gt.0)then
					outTData%zdata=A%sdata(1:length)+value
				else
					outTData%zdata=A%sdata(1:length)-value
				end if
			case(3)
				if(num.gt.0)then
					outTData%zdata=A%ddata(1:length)+value
				else
					outTData%zdata=A%ddata(1:length)-value
				end if
			case(4)
				if(num.gt.0)then
					outTData%zdata=A%cdata(1:length)+value
				else
					outTData%zdata=A%cdata(1:length)-value
				end if
			case(5)
				if(num.gt.0)then
					outTData%zdata=A%zdata(1:length)+value
				else
					outTData%zdata=A%zdata(1:length)-value
				end if
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%adata(i)+value
					end do
				end if
		end select
		return
	end subroutine
!character+classtype ---> max{7,classtype}	
	subroutine add_minu_TData_char(outTData,A,value,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		character(len=*),intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%adata(i)+value
					end do
				end if
			case(1)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%idata(i)+value
					end do
				end if
			case(2)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%sdata(i)+value
					end do
				end if
			case(3)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%ddata(i)+value
					end do
				end if
			case(4)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%cdata(i)+value
					end do
				end if
			case(5)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%zdata(i)+value
					end do
				end if
			case(6)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=A%ldata(i)+value
					end do
				end if
		end select
		return
	end subroutine
	subroutine add_minu_int_TData(outTData,value,A,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		integer,intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(1)
				if(num.gt.0)then
					outTData%idata=value+A%idata(1:length)
				else
					outTData%idata=value-A%idata(1:length)
				end if
			case(2)
				if(num.gt.0)then
					outTData%sdata=value+A%sdata(1:length)
				else
					outTData%sdata=value-A%sdata(1:length)
				end if
			case(3)
				if(num.gt.0)then
					outTData%ddata=value+A%ddata(1:length)
				else
					outTData%ddata=value-A%ddata(1:length)
				end if
			case(4)
				if(num.gt.0)then
					outTData%cdata=value+A%cdata(1:length)
				else
					outTData%cdata=value-A%cdata(1:length)
				end if
			case(5)
				if(num.gt.0)then
					outTData%zdata=value+A%zdata(1:length)
				else
					outTData%zdata=value-A%zdata(1:length)
				end if
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%adata(i)
					end do
				end if
		end select
		return
	end subroutine
	subroutine add_minu_real4_TData(outTData,value,A,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		real(kind=4),intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(1)
				if(num.gt.0)then
					outTData%sdata=value+A%idata(1:length)
				else
					outTData%sdata=value-A%idata(1:length)
				end if
			case(2)
				if(num.gt.0)then
					outTData%sdata=value+A%sdata(1:length)
				else
					outTData%sdata=value-A%sdata(1:length)
				end if
			case(3)
				if(num.gt.0)then
					outTData%ddata=value+A%ddata(1:length)
				else
					outTData%ddata=value-A%ddata(1:length)
				end if
			case(4)
				if(num.gt.0)then
					outTData%cdata=value+A%cdata(1:length)
				else
					outTData%cdata=value-A%cdata(1:length)
				end if
			case(5)
				if(num.gt.0)then
					outTData%zdata=value+A%zdata(1:length)
				else
					outTData%zdata=value-A%zdata(1:length)
				end if
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%adata(i)
					end do
				end if
		end select
		return
	end subroutine
	subroutine add_minu_real8_TData(outTData,value,A,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		real(kind=8),intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(1)
				if(num.gt.0)then
					outTData%ddata=value+A%idata(1:length)
				else
					outTData%ddata=value-A%idata(1:length)
				end if
			case(2)
				if(num.gt.0)then
					outTData%ddata=value+A%sdata(1:length)
				else
					outTData%ddata=value-A%sdata(1:length)
				end if
			case(3)
				if(num.gt.0)then
					outTData%ddata=value+A%ddata(1:length)
				else
					outTData%ddata=value-A%ddata(1:length)
				end if
			case(4)
				if(num.gt.0)then
					outTData%zdata=value+A%cdata(1:length)
				else
					outTData%zdata=value-A%cdata(1:length)
				end if
			case(5)
				if(num.gt.0)then
					outTData%zdata=value+A%zdata(1:length)
				else
					outTData%zdata=value-A%zdata(1:length)
				end if
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%adata(i)
					end do
				end if
		end select
		return
	end subroutine
	subroutine add_minu_com4_TData(outTData,value,A,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		complex(kind=4),intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(1)
				if(num.gt.0)then
					outTData%cdata=value+A%idata(1:length)
				else
					outTData%cdata=value-A%idata(1:length)
				end if
			case(2)
				if(num.gt.0)then
					outTData%cdata=value+A%sdata(1:length)
				else
					outTData%cdata=value-A%sdata(1:length)
				end if
			case(3)
				if(num.gt.0)then
					outTData%zdata=value+A%ddata(1:length)
				else
					outTData%zdata=value-A%ddata(1:length)
				end if
			case(4)
				if(num.gt.0)then
					outTData%cdata=value+A%cdata(1:length)
				else
					outTData%cdata=value-A%cdata(1:length)
				end if
			case(5)
				if(num.gt.0)then
					outTData%zdata=value+A%zdata(1:length)
				else
					outTData%zdata=value-A%zdata(1:length)
				end if
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%adata(i)
					end do
				end if
		end select
		return
	end subroutine
	subroutine add_minu_com8_TData(outTData,value,A,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		complex(kind=8),intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(1)
				if(num.gt.0)then
					outTData%zdata=value+A%idata(1:length)
				else
					outTData%zdata=value-A%idata(1:length)
				end if
			case(2)
				if(num.gt.0)then
					outTData%zdata=value+A%sdata(1:length)
				else
					outTData%zdata=value-A%sdata(1:length)
				end if
			case(3)
				if(num.gt.0)then
					outTData%zdata=value+A%ddata(1:length)
				else
					outTData%zdata=value-A%ddata(1:length)
				end if
			case(4)
				if(num.gt.0)then
					outTData%zdata=value+A%cdata(1:length)
				else
					outTData%zdata=value-A%cdata(1:length)
				end if
			case(5)
				if(num.gt.0)then
					outTData%zdata=value+A%zdata(1:length)
				else
					outTData%zdata=value-A%zdata(1:length)
				end if
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%adata(i)
					end do
				end if
		end select
		return
	end subroutine
	subroutine add_minu_char_TData(outTData,value,A,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A
		character(len=*),intent(in)::value
		integer,intent(in)::num
		integer::i,length
		length=A%totalData
		select case(A%classtype)
			case(7)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%adata(i)
					end do
				end if
			case(1)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%idata(i)
					end do
				end if
			case(2)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%sdata(i)
					end do
				end if
			case(3)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%ddata(i)
					end do
				end if
			case(4)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%cdata(i)
					end do
				end if
			case(5)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%zdata(i)
					end do
				end if
			case(6)
				if(num.gt.0)then
					do i=1,length
						outTData%adata(i)=value+A%ldata(i)
					end do
				end if
		end select
		return
	end subroutine
	
	subroutine add_minu_TData(outTData,A,B,num)
		type(TData),intent(inout)::outTData
		type(TData),intent(in)::A,B
		integer,intent(in)::num
		integer::flag,length,i
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				if(num.gt.0)then
					outTData%idata=A%idata(1:length)+B%idata(1:length)
				else
					outTData%idata=A%idata(1:length)-B%idata(1:length)
				end if
			case(12)
				call scopy(length,real(A%idata,kind=4),1,outTData%sdata,1)
				call saxpy (length, real(num,kind=4), B%sdata,1,outTData%sdata,1)
			case(13)
				call dcopy(length,dble(A%idata),1,outTData%ddata,1)
				call daxpy (length, dble(num), B%ddata,1,outTData%ddata,1)
			case(14)
				call ccopy(length,cmplx(A%idata,kind=4),1,outTData%cdata,1)
				call caxpy (length, cmplx(num,kind=4), B%cdata,1,outTData%cdata,1)
			case(15)
				call zcopy(length,dcmplx(A%idata),1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), B%zdata,1,outTData%zdata,1)
			case(17)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%idata(i)+B%adata(i)
					end do
				end if
			
			case(21)
				call scopy(length,A%sdata,1,outTData%sdata,1)
				call saxpy (length, real(num,kind=4), real(B%idata,kind=4),1,outTData%sdata,1)
			case(22)
				call scopy(length,A%sdata,1,outTData%sdata,1)
				call saxpy (length, real(num,kind=4), B%sdata,1,outTData%sdata,1)
			case(23)
				call dcopy(length,dble(A%sdata),1,outTData%ddata,1)
				call daxpy (length, dble(num), B%ddata,1,outTData%ddata,1)
			case(24)
				call ccopy(length,cmplx(A%sdata,kind=4),1,outTData%cdata,1)
				call caxpy (length, cmplx(num,kind=4), B%cdata,1,outTData%cdata,1)
			case(25)
				call zcopy(length,dcmplx(A%sdata),1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), B%zdata,1,outTData%zdata,1)
			case(27)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%sdata(i)+B%adata(i)
					end do
				end if
				
			case(31)
				call dcopy(length,A%ddata,1,outTData%ddata,1)
				call daxpy (length, dble(num), dble(B%idata),1,outTData%ddata,1)
			case(32)
				call dcopy(length,A%ddata,1,outTData%ddata,1)
				call daxpy (length, dble(num), dble(B%sdata),1,outTData%ddata,1)
			case(33)
				call dcopy(length,A%ddata,1,outTData%ddata,1)
				call daxpy (length, dble(num), B%ddata,1,outTData%ddata,1)
			case(34)
				call zcopy(length,dcmplx(A%ddata),1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), dcmplx(B%cdata),1,outTData%zdata,1)
			case(35)
				call zcopy(length,dcmplx(A%ddata),1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), B%zdata,1,outTData%zdata,1)
			case(37)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%ddata(i)+B%adata(i)
					end do
				end if
			
			case(41)
				call ccopy(length,A%cdata,1,outTData%cdata,1)
				call caxpy (length, cmplx(num,kind=4), cmplx(B%idata,kind=4),1,outTData%cdata,1)
			case(42)
				call ccopy(length,A%cdata,1,outTData%cdata,1)
				call caxpy (length, cmplx(num,kind=4), cmplx(B%sdata,kind=4),1,outTData%cdata,1)
			case(43)
				call zcopy(length,dcmplx(A%cdata),1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), dcmplx(B%ddata),1,outTData%zdata,1)
			case(44)
				call ccopy(length,A%cdata,1,outTData%cdata,1)
				call caxpy (length, cmplx(num,kind=4), B%cdata,1,outTData%cdata,1)
			case(45)
				call zcopy(length,dcmplx(A%cdata),1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), B%zdata,1,outTData%zdata,1)
			case(47)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%cdata(i)+B%adata(i)
					end do
				end if
			
			case(51)
				call zcopy(length,A%zdata,1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), dcmplx(B%idata),1,outTData%zdata,1)
			case(52)
				call zcopy(length,A%zdata,1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), dcmplx(B%sdata),1,outTData%zdata,1)
			case(53)
				call zcopy(length,A%zdata,1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), dcmplx(B%ddata),1,outTData%zdata,1)
			case(54)
				call zcopy(length,A%zdata,1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), dcmplx(B%cdata),1,outTData%zdata,1)
			case(55)
				call zcopy(length,A%zdata,1,outTData%zdata,1)
				call zaxpy (length, dcmplx(num), B%zdata,1,outTData%zdata,1)
			case(57)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%zdata(i)+B%adata(i)
					end do
				end if
			
			case(67)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%ldata(i)+B%adata(i)
					end do
				end if
				
			case(71)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%adata(i)+B%idata(i)
					end do
				end if
			case(72)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%adata(i)+B%sdata(i)
					end do
				end if
			case(73)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%adata(i)+B%ddata(i)
					end do
				end if
			case(74)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%adata(i)+B%cdata(i)
					end do
				end if
			case(75)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%adata(i)+B%zdata(i)
					end do
				end if
			case(76)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%adata(i)+B%ldata(i)
					end do
				end if
			case(77)
				if(num.gt.0) then
					do i=1,length
						outTData%adata(i)=A%adata(i)+B%adata(i)
					end do
				end if
			case default 
				write(*,*)"ERROR in + -"
				call error_stop()
		end select
		return
	end subroutine
	
	subroutine TDatadivideTensor(Res,A,B)
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A,B
		integer::length,flag
		length=A%TotalData
		flag=A%classtype*10+B%classtype
		select case(flag)
			case (11)
				Res%sdata=A%idata
				call sscal (length, 1./B%idata(1), Res%sdata, 1)
			case (12)
				call scopy(length,real(A%idata,kind=4),1,Res%sdata,1)
				call sscal (length, 1./B%sdata(1), Res%sdata, 1)
			case (13)
				call dcopy(length,dble(A%idata),1,Res%ddata,1)
				call dscal (length, 1d0/B%ddata(1), Res%ddata, 1)
			case (14)
				call ccopy(length,cmplx(A%idata,kind=4),1,Res%cdata,1)
				call cscal (length, cmplx(1.,0.)/B%cdata(1), Res%cdata, 1)
			case (15)
				call zcopy(length,dcmplx(A%idata),1,Res%zdata,1)
				call zscal (length, dcmplx(1.,0.)/B%zdata(1), Res%zdata, 1)
				
			case (21)
				call scopy(length,A%sdata,1,Res%sdata,1)
				call sscal (length, 1./real(B%idata(1),kind=4), Res%sdata, 1)
			case (22)
				call scopy(length,A%sdata,1,Res%sdata,1)
				call sscal (length, 1./B%sdata(1), Res%sdata, 1)
			case (23)
				call dcopy(length,dble(A%sdata),1,Res%ddata,1)
				call dscal (length, 1d0/B%ddata(1), Res%ddata, 1)
			case (24)
				call ccopy(length,cmplx(A%sdata,kind=4),1,Res%cdata,1)
				call cscal (length, cmplx(1.,0.)/B%cdata(1), Res%cdata, 1)
			case (25)
				call zcopy(length,dcmplx(A%sdata),1,Res%zdata,1)
				call zscal (length, dcmplx(1.,0.)/B%zdata(1), Res%zdata, 1)	
				
			case (31)
				call dcopy(length,A%ddata,1,Res%ddata,1)
				call dscal (length, 1d0/dble(B%idata(1)), Res%ddata, 1)
			case (32)
				call dcopy(length,A%ddata,1,Res%ddata,1)
				call dscal (length, 1d0/dble(B%sdata(1)), Res%ddata, 1)
			case (33)
				call dcopy(length,A%ddata,1,Res%ddata,1)
				call dscal (length, 1d0/B%ddata(1), Res%ddata, 1)
			case (34)
				call zcopy(length,dcmplx(A%ddata),1,Res%zdata,1)
				call zscal (length, dcmplx(1.,0.)/dcmplx(B%cdata(1)), Res%zdata, 1)
			case (35)
				call zcopy(length,dcmplx(A%ddata),1,Res%zdata,1)
				call zscal (length, dcmplx(1.,0.)/B%zdata(1), Res%zdata, 1)	
				
			case (41)
				call ccopy(length,A%cdata,1,Res%cdata,1)
				call cscal (length, cmplx(1,0,kind=4)/cmplx(B%idata(1),kind=4), Res%cdata, 1)
			case (42)
				call ccopy(length,A%cdata,1,Res%cdata,1)
				call cscal (length, cmplx(1,0,kind=4)/cmplx(B%sdata(1),kind=4), Res%cdata, 1)
			case (43)
				call zcopy(length,dcmplx(A%cdata),1,Res%zdata,1)
				call zscal (length, dcmplx(1d0/B%ddata(1)), Res%zdata, 1)	
			case (44)
				call ccopy(length,A%cdata,1,Res%cdata,1)
				call cscal (length, cmplx(1.,0.,kind=4)/B%cdata(1), Res%cdata, 1)
			case (45)
				call zcopy(length,dcmplx(A%cdata),1,Res%zdata,1)
				call zscal (length, dcmplx(1.,0.)/B%zdata(1), Res%zdata, 1)	
				
			case (51)
				call zcopy(length,A%zdata,1,Res%zdata,1)
				call zscal (length, dcmplx(1.,0.)/dcmplx(B%idata(1)), Res%zdata, 1)
			case (52)
				call zcopy(length,A%zdata,1,Res%zdata,1)
				call zscal (length, dcmplx(1./B%sdata(1)), Res%zdata, 1)
			case (53)
				call zcopy(length,A%zdata,1,Res%zdata,1)
				call zscal (length, dcmplx(1d0/B%ddata(1)), Res%zdata, 1)	
			case (54)
				call zcopy(length,A%zdata,1,Res%zdata,1)
				call zscal (length, dcmplx(1./B%cdata(1)), Res%zdata, 1)
			case (55)
				call zcopy(length,A%zdata,1,Res%zdata,1)
				call zscal (length, dcmplx(1.,0.)/B%zdata(1), Res%zdata, 1)	
				
		end select
		return
	end subroutine
	
	subroutine TDatadivideTensor2(Res,num,B,Aclasstype)
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: B
		class(*),intent(in)::num
		integer,intent(in)::Aclasstype
		integer::length,flag
		length=1
		flag=Aclasstype*10+B%classtype
		select type(num)
			type is (integer)
				call assignmentTData_int(Res,(/num/),1)
			type is (real(kind=4))
				call assignmentTData_real4(Res,(/num/),1)	
			type is (real(kind=8))
				call assignmentTData_real8(Res,(/num/),1)	
			type is (complex(kind=4))
				call assignmentTData_com4(Res,(/num/),1)
			type is (complex(kind=8))
				call assignmentTData_com8(Res,(/num/),1)
			class default
				write(*,*)"ERROR in num/Tensor"
		end select
		select case(flag)
			case (11)
				call sscal (length, 1./B%idata(1), Res%sdata, 1)
			case (12)
				call sscal (length, 1./B%sdata(1), Res%sdata, 1)
			case (13)
				call dscal (length, 1d0/B%ddata(1), Res%ddata, 1)
			case (14)
				call cscal (length, cmplx(1.,0.)/B%cdata(1), Res%cdata, 1)
			case (15)
				call zscal (length, dcmplx(1.,0.)/B%zdata(1), Res%zdata, 1)
				
			case (21)
				call sscal (length, 1./real(B%idata(1),kind=4), Res%sdata, 1)
			case (22)
				call sscal (length, 1./B%sdata(1), Res%sdata, 1)
			case (23)
				call dscal (length, 1d0/B%ddata(1), Res%ddata, 1)
			case (24)
				call cscal (length, cmplx(1.,0.)/B%cdata(1), Res%cdata, 1)
			case (25)
				call zscal (length, dcmplx(1.,0.)/B%zdata(1), Res%zdata, 1)	
				
			case (31)
				call dscal (length, 1d0/dble(B%idata(1)), Res%ddata, 1)
			case (32)
				call dscal (length, 1d0/dble(B%sdata(1)), Res%ddata, 1)
			case (33)
				call dscal (length, 1d0/B%ddata(1), Res%ddata, 1)
			case (34)
				call zscal (length, dcmplx(1.,0.)/dcmplx(B%cdata(1)), Res%zdata, 1)
			case (35)
				call zscal (length, dcmplx(1.,0.)/B%zdata(1), Res%zdata, 1)	
				
			case (41)
				call cscal (length, cmplx(1,0,kind=4)/cmplx(B%idata(1),kind=4), Res%cdata, 1)
			case (42)
				call cscal (length, cmplx(1,0,kind=4)/cmplx(B%sdata(1),kind=4), Res%cdata, 1)
			case (43)
				call zscal (length, dcmplx(1d0/B%ddata(1)), Res%zdata, 1)	
			case (44)
				call cscal (length, cmplx(1.,0.,kind=4)/B%cdata(1), Res%cdata, 1)
			case (45)
				call zscal (length, dcmplx(1.,0.)/B%zdata(1), Res%zdata, 1)	
				
			case (51)
				call zscal (length, dcmplx(1.,0.)/dcmplx(B%idata(1)), Res%zdata, 1)
			case (52)
				call zscal (length, dcmplx(1./B%sdata(1)), Res%zdata, 1)
			case (53)
				call zscal (length, dcmplx(1d0/B%ddata(1)), Res%zdata, 1)	
			case (54)
				call zscal (length, dcmplx(1./B%cdata(1)), Res%zdata, 1)
			case (55)
				call zscal (length, dcmplx(1.,0.)/B%zdata(1), Res%zdata, 1)	
				
		end select
		return
	end subroutine
	
	
	subroutine TDatamultiply_number_int(Res,A,num)
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A
		integer,intent(in) ::   num
		integer::length
		length=A%TotalData
		select case(A%classtype)
			case (1)
				Res%idata(1:length)=A%idata(1:length)*num
			case (2)
				call scopy(length,A%sdata,1,Res%sdata,1)
				call sscal (length, real(num,kind=4), Res%sdata, 1)
			case (3)
				call dcopy(length,A%ddata,1,Res%ddata,1)
				call dscal (length, dble(num), Res%ddata, 1)
			case (4)
				call ccopy(length,A%cdata,1,Res%cdata,1)
				call cscal (length, cmplx(num,kind=4), Res%cdata, 1)
			case (5)
				call zcopy(length,A%zdata,1,Res%zdata,1)
				call zscal (length, dcmplx(num), Res%zdata, 1)
		end select
		return
	end subroutine
	subroutine TDatamultiply_number_int_par(Res,A,num,alpha,beta)!Res=(A*num)*alpha+beta*Res
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A
		integer,intent(in) ::   num
		class(*),intent(in)::alpha,beta
		integer::length
		length=A%TotalData
		if(length.gt.Res%TotalData)then
			call writemess(' ERROR in TDatamultiply_number_real8_par in TData.f90')
			call error_stop
		end if
		select case(A%classtype)
			case (1)
				Res%idata(1:length)=(iselect(alpha)*(A%idata(1:length)*num))+(iselect(beta)*Res%idata(1:length))
			case (2)
				Res%sdata(1:length)=(sselect(alpha)*(A%sdata(1:length)*num))+(sselect(beta)*Res%sdata(1:length))
			case (3)
				Res%ddata(1:length)=(dselect(alpha)*(A%ddata(1:length)*num))+(dselect(beta)*Res%ddata(1:length))
			case (4)
				Res%cdata(1:length)=(cselect(alpha)*(A%cdata(1:length)*num))+(cselect(beta)*Res%cdata(1:length))
			case (5)
				Res%zdata(1:length)=(zselect(alpha)*(A%zdata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
		end select
		return
	end subroutine
	subroutine TDatamultiply_number_real4(Res,A,num)
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A
		real(kind=4),intent(in) ::   num
		integer::length
		length=A%TotalData
		select case(A%classtype)
			case (1)
				call scopy(length,real(A%idata,kind=4),1,Res%sdata,1)
				call sscal (length, num, Res%sdata, 1)
			case (2)
				call scopy(length,A%sdata,1,Res%sdata,1)
				call sscal (length, num, Res%sdata, 1)
			case (3)
				call dcopy(length,A%ddata,1,Res%ddata,1)
				call dscal (length, dble(num), Res%ddata, 1)
			case (4)
				call ccopy(length,A%cdata,1,Res%cdata,1)
				call cscal (length, cmplx(num,kind=4), Res%cdata, 1)
			case (5)
				call zcopy(length,A%zdata,1,Res%zdata,1)
				call zscal (length, dcmplx(num), Res%zdata, 1)
		end select
		return
	end subroutine
	subroutine TDatamultiply_number_real4_par(Res,A,num,alpha,beta)!Res=(A*num)*alpha+beta*Res
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A
		real*4,intent(in) ::   num
		class(*),intent(in)::alpha,beta
		integer::length
		length=A%TotalData
		if(length.gt.Res%TotalData)then
			call writemess(' ERROR in TDatamultiply_number_real8_par in TData.f90')
			call error_stop
		end if
		select case(A%classtype)
			case (1)
				Res%sdata(1:length)=(sselect(alpha)*(A%idata(1:length)*num))+(sselect(beta)*Res%sdata(1:length))
			case (2)
				Res%sdata(1:length)=(sselect(alpha)*(A%sdata(1:length)*num))+(sselect(beta)*Res%sdata(1:length))
			case (3)
				Res%ddata(1:length)=(dselect(alpha)*(A%ddata(1:length)*num))+(dselect(beta)*Res%ddata(1:length))
			case (4)
				Res%cdata(1:length)=(cselect(alpha)*(A%cdata(1:length)*num))+(cselect(beta)*Res%cdata(1:length))
			case (5)
				Res%zdata(1:length)=(zselect(alpha)*(A%zdata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
		end select
		return
	end subroutine
	subroutine TDatamultiply_number_real8(Res,A,num)
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A
		real(kind=8),intent(in) ::   num
		integer::length
		length=A%TotalData
		select case(A%classtype)
			case (1)
				call dcopy(length,dble(A%idata),1,Res%ddata,1)
				call dscal (length, num, Res%ddata, 1)
			case (2)
				call dcopy(length,dble(A%sdata),1,Res%ddata,1)
				call dscal (length, num, Res%ddata, 1)
			case (3)
				call dcopy(length,A%ddata,1,Res%ddata,1)
				call dscal (length, num, Res%ddata, 1)
			case (4)
				call zcopy(length,dcmplx(A%cdata),1,Res%zdata,1)
				call zscal (length, dcmplx(num), Res%zdata, 1)
			case (5)
				call zcopy(length,A%zdata,1,Res%zdata,1)
				call zscal (length, dcmplx(num), Res%zdata, 1)
		end select
		return
	end subroutine
	subroutine TDatamultiply_number_real8_par(Res,A,num,alpha,beta)!Res=(A*num)*alpha+beta*Res
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A
		real*8,intent(in) ::   num
		class(*),intent(in)::alpha,beta
		integer::length
		length=A%TotalData
		if(length.gt.Res%TotalData)then
			call writemess(' ERROR in TDatamultiply_number_real8_par in TData.f90')
			call error_stop
		end if
		select case(A%classtype)
			case (1)
				Res%ddata(1:length)=(dselect(alpha)*(A%idata(1:length)*num))+(dselect(beta)*Res%ddata(1:length))
			case (2)
				Res%ddata(1:length)=(dselect(alpha)*(A%sdata(1:length)*num))+(dselect(beta)*Res%ddata(1:length))
			case (3)
				Res%ddata(1:length)=(dselect(alpha)*(A%ddata(1:length)*num))+(dselect(beta)*Res%ddata(1:length))
			case (4)
				Res%zdata(1:length)=(zselect(alpha)*(A%cdata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
			case (5)
				Res%zdata(1:length)=(zselect(alpha)*(A%zdata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
		end select
		return
	end subroutine
	subroutine TDatamultiply_number_com4(Res,A,num)
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A
		complex(kind=4),intent(in) ::   num
		integer::length
		length=A%TotalData
		select case(A%classtype)
			case (1)
				call ccopy(length,cmplx(A%idata,kind=4),1,Res%cdata,1)
				call cscal (length, num, Res%cdata, 1)
			case (2)
				call ccopy(length,cmplx(A%sdata,kind=4),1,Res%cdata,1)
				call cscal (length, num, Res%cdata, 1)
			case (3)
				call zcopy(length,dcmplx(A%ddata),1,Res%zdata,1)
				call zscal (length, dcmplx(num), Res%zdata, 1)
			case (4)
				call ccopy(length,A%cdata,1,Res%cdata,1)
				call cscal (length, num, Res%cdata, 1)
			case (5)
				call zcopy(length,A%zdata,1,Res%zdata,1)
				call zscal (length, dcmplx(num), Res%zdata, 1)
		end select
		return
	end subroutine
	subroutine TDatamultiply_number_com4_par(Res,A,num,alpha,beta)!Res=(A*num)*alpha+beta*Res
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A
		complex(kind=4),intent(in) ::   num
		class(*),intent(in)::alpha,beta
		integer::length
		length=A%TotalData
		if(length.gt.Res%TotalData)then
			call writemess(' ERROR in TDatamultiply_number_real8_par in TData.f90')
			call error_stop
		end if
		select case(A%classtype)
			case (1)
				Res%cdata(1:length)=(cselect(alpha)*(A%idata(1:length)*num))+(cselect(beta)*Res%cdata(1:length))
			case (2)
				Res%cdata(1:length)=(cselect(alpha)*(A%sdata(1:length)*num))+(cselect(beta)*Res%cdata(1:length))
			case (3)
				Res%zdata(1:length)=(zselect(alpha)*(A%ddata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
			case (4)
				Res%cdata(1:length)=(cselect(alpha)*(A%cdata(1:length)*num))+(cselect(beta)*Res%cdata(1:length))
			case (5)
				Res%zdata(1:length)=(zselect(alpha)*(A%zdata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
		end select
		return
	end subroutine
	subroutine TDatamultiply_number_com8(Res,A,num)
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A
		complex(kind=8),intent(in) ::   num
		integer::length
		length=A%TotalData
		select case(A%classtype)
			case (1)
				call zcopy(length,dcmplx(A%idata),1,Res%zdata,1)
				call zscal (length, num, Res%zdata, 1)
			case (2)
				call zcopy(length,dcmplx(A%sdata),1,Res%zdata,1)
				call zscal (length, num, Res%zdata, 1)
			case (3)
				call zcopy(length,dcmplx(A%ddata),1,Res%zdata,1)
				call zscal (length, num, Res%zdata, 1)
			case (4)
				call zcopy(length,dcmplx(A%cdata),1,Res%zdata,1)
				call zscal (length, num, Res%zdata, 1)
			case (5)
				call zcopy(length,A%zdata,1,Res%zdata,1)
				call zscal (length, num, Res%zdata, 1)
		end select
		return
	end subroutine
	subroutine TDatamultiply_number_com8_par(Res,A,num,alpha,beta)!Res=(A*num)*alpha+beta*Res
		type(TData),intent(inout)::Res
		type(TData),intent(in) :: A
		complex(kind=8),intent(in) ::   num
		class(*),intent(in)::alpha,beta
		integer::length
		length=A%TotalData
		if(length.gt.Res%TotalData)then
			call writemess(' ERROR in TDatamultiply_number_real8_par in TData.f90')
			call error_stop
		end if
		select case(A%classtype)
			case (1)
				Res%zdata(1:length)=(zselect(alpha)*(A%idata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
			case (2)
				Res%zdata(1:length)=(zselect(alpha)*(A%sdata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
			case (3)
				Res%zdata(1:length)=(zselect(alpha)*(A%ddata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
			case (4)
				Res%zdata(1:length)=(zselect(alpha)*(A%cdata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
			case (5)
				Res%zdata(1:length)=(zselect(alpha)*(A%zdata(1:length)*num))+(zselect(beta)*Res%zdata(1:length))
		end select
		return
	end subroutine
!************************************************************************
!           vec*vec
!************************************************************************
	subroutine product_VV_dim1(R,A,B)	
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotu
		complex(kind=8),External::	zdotu
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R%idata=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R%sdata=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R%ddata=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R%cdata=cdotu(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R%zdata=zdotu(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R%sdata=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R%sdata=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R%ddata=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R%cdata=cdotu(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R%zdata=zdotu(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R%ddata=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R%ddata=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R%ddata=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R%zdata=zdotu(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R%zdata=zdotu(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R%cdata=cdotu(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R%cdata=cdotu(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R%zdata=zdotu(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R%cdata=cdotu(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R%zdata=zdotu(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R%zdata=zdotu(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R%zdata=zdotu(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R%zdata=zdotu(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R%zdata=zdotu(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R%zdata=zdotu(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine
	
	
	subroutine product_VV_dim1_par(R,A,B,alpha,beta)!Res=alpha*A*B+beta*Res
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		class(*),intent(in)::alpha,beta
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotu
		complex(kind=8),External::	zdotu
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R%idata=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))*iselect(alpha)+(R%idata*iselect(beta))
			case(12)
				R%sdata=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)*sselect(alpha)+(R%sdata*sselect(beta))
			case(13)
				R%ddata=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)*dselect(alpha)+(R%ddata*dselect(beta))
			case(14)
				R%cdata=cdotu(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)*cselect(alpha)+(R%cdata*cselect(beta))
			case(15)
				R%zdata=zdotu(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)*zselect(alpha)+(R%zdata*zselect(beta))
				
			case(21)
				R%sdata=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)*sselect(alpha)+(R%sdata*sselect(beta))
			case(22)
				R%sdata=sdot(length, A%sdata, 1, B%sdata, 1)*sselect(alpha)+(R%sdata*sselect(beta))
			case(23)
				R%ddata=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)*dselect(alpha)+(R%ddata*dselect(beta))
			case(24)
				R%cdata=cdotu(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)*cselect(alpha)+(R%cdata*cselect(beta))
			case(25)
				R%zdata=zdotu(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)*zselect(alpha)+(R%zdata*zselect(beta))
				
			case(31)
				R%ddata=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)*dselect(alpha)+(R%ddata*dselect(beta))
			case(32)
				R%ddata=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)*dselect(alpha)+(R%ddata*dselect(beta))
			case(33)
				R%ddata=ddot(length,  A%ddata, 1, B%ddata, 1)*dselect(alpha)+(R%ddata*dselect(beta))
			case(34)
				R%zdata=zdotu(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)*zselect(alpha)+(R%zdata*zselect(beta))
			case(35)
				R%zdata=zdotu(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)*zselect(alpha)+(R%zdata*zselect(beta))
			
			case(41)
				R%cdata=cdotu(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)*cselect(alpha)+(R%cdata*cselect(beta))
			case(42)
				R%cdata=cdotu(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)*cselect(alpha)+(R%cdata*cselect(beta))
			case(43)
				R%zdata=zdotu(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)*zselect(alpha)+(R%zdata*zselect(beta))
			case(44)
				R%cdata=cdotu(length, A%cdata, 1, B%cdata, 1)*cselect(alpha)+(R%cdata*cselect(beta))
			case(45)
				R%zdata=zdotu(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)*zselect(alpha)+(R%zdata*zselect(beta))
				
			case(51)
				R%zdata=zdotu(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)*zselect(alpha)+(R%zdata*zselect(beta))
			case(52)
				R%zdata=zdotu(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)*zselect(alpha)+(R%zdata*zselect(beta))
			case(53)
				R%zdata=zdotu(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)*zselect(alpha)+(R%zdata*zselect(beta))
			case(54)
				R%zdata=zdotu(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)*zselect(alpha)+(R%zdata*zselect(beta))
			case(55)
				R%zdata=zdotu(length, A%zdata, 1, B%zdata, 1)	*zselect(alpha)+(R%zdata*zselect(beta))
		end select
		return
	end subroutine
	


!************************************************************************
!           mat*vec
!************************************************************************
	subroutine matrix_mv_int(outdata,A,B,LD1,LD2,LD3,LD4)
		integer,intent(in)::LD1,LD2,LD3,LD4
		integer,intent(in)::A(LD1,LD2),B(LD3,LD4)
		integer,intent(inout)::outdata(LD1,LD4)
		outdata=matmul(A,B)
		return
	end subroutine
	subroutine matrix_mv_int_par(outdata,A,B,LD1,LD2,LD3,LD4,alpha,beta)
		integer,intent(in)::LD1,LD2,LD3,LD4
		integer,intent(in)::A(LD1,LD2),B(LD3,LD4)
		integer,intent(inout)::outdata(LD1,LD4)
		integer,intent(in)::alpha,beta
		outdata=alpha*matmul(A,B)+(beta*outdata)
		return
	end subroutine
	subroutine matrix_mv_real4(outdata,A,B,LD1,LD2,LD3,LD4)
		integer,intent(in)::LD1,LD2,LD3,LD4
		real(kind=4),intent(in)::A(LD1,LD2),B(LD3,LD4)
		real(kind=4),intent(inout)::outdata(LD1,LD4)
		outdata=matmul(A,B)
		return
	end subroutine
	subroutine matrix_mv_real8(outdata,A,B,LD1,LD2,LD3,LD4)
		integer,intent(in)::LD1,LD2,LD3,LD4
		real(kind=8),intent(in)::A(LD1,LD2),B(LD3,LD4)
		real(kind=8),intent(inout)::outdata(LD1,LD4)
		outdata=matmul(A,B)
		return
	end subroutine
	subroutine matrix_mv_com4(outdata,A,B,LD1,LD2,LD3,LD4)
		integer,intent(in)::LD1,LD2,LD3,LD4
		complex(kind=4),intent(in)::A(LD1,LD2),B(LD3,LD4)
		complex(kind=4),intent(inout)::outdata(LD1,LD4)
		outdata=matmul(A,B)
		return
	end subroutine
	subroutine matrix_mv_com8(outdata,A,B,LD1,LD2,LD3,LD4)
		integer,intent(in)::LD1,LD2,LD3,LD4
		complex(kind=8),intent(in)::A(LD1,LD2),B(LD3,LD4)
		complex(kind=8),intent(inout)::outdata(LD1,LD4)
		outdata=matmul(A,B)
		return
	end subroutine
	subroutine product_MV_dim1(R,A,B,LD1,LD2)	
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer,intent(in)::LD1,LD2
		integer::length,flag,lenB
		flag=10*A%classType+B%classType
		length=A%totalData
		lenB=B%totalData
		select case(flag)
			case(11)
				call matrix_mv_int(R%idata,A%idata,B%idata,LD1,LD2,LD2,1)
			case(12)
				call SGEMV('N', LD1, LD2, 1., real(A%idata(1:length),kind=4), LD1, B%sdata, 1, 0., R%sdata, 1)
			case(13)
				call DGEMV('N', LD1, LD2, 1d0, dble(A%idata(1:length)), LD1, B%ddata, 1, 0d0, R%ddata, 1)
			case(14)
				call CGEMV('N', LD1, LD2, cmplx(1,kind=4), cmplx(A%idata(1:length),kind=4), LD1, B%cdata, 1, cmplx(0,kind=4), R%cdata, 1)
			case(15)
				call ZGEMV('N', LD1, LD2, dcmplx(1), dcmplx(A%idata(1:length)), LD1, B%zdata, 1, dcmplx(0), R%zdata, 1)
				
			case(21)
				call SGEMV('N', LD1, LD2, 1., A%sdata, LD1, real(B%idata(1:lenB),kind=4), 1, 0., R%sdata, 1)
			case(22)
				call SGEMV('N', LD1, LD2, 1., A%sdata, LD1, B%sdata, 1, 0., R%sdata, 1)
			case(23)
				call DGEMV('N', LD1, LD2, 1d0, dble(A%sdata(1:length)), LD1, B%ddata, 1, 0d0, R%ddata, 1)
			case(24)
				call CGEMV('N', LD1, LD2, cmplx(1,kind=4), cmplx(A%sdata(1:length),kind=4), LD1, B%cdata, 1, cmplx(0,kind=4), R%cdata, 1)
			case(25)
				call ZGEMV('N', LD1, LD2, dcmplx(1), dcmplx(A%sdata(1:length)), LD1, B%zdata, 1, dcmplx(0), R%zdata, 1)
				
			case(31)
				call DGEMV('N', LD1, LD2, 1d0, A%ddata, LD1, dble(B%idata(1:lenB)), 1, 0d0, R%ddata, 1)
			case(32)
				call DGEMV('N', LD1, LD2, 1d0, A%ddata, LD1, dble(B%sdata(1:lenB)), 1, 0d0, R%ddata, 1)
			case(33)
				call DGEMV('N', LD1, LD2, 1d0, A%ddata, LD1, B%ddata, 1, 0d0, R%ddata, 1)
			case(34)
				call ZGEMV('N', LD1, LD2, dcmplx(1), dcmplx(A%ddata(1:length)), LD1, dcmplx(B%cdata(1:lenB)), 1, dcmplx(0), R%zdata, 1)
			case(35)
				call ZGEMV('N', LD1, LD2, dcmplx(1), dcmplx(A%ddata(1:length)), LD1, B%zdata, 1, dcmplx(0), R%zdata, 1)	
				
			case(41)
				call CGEMV('N', LD1, LD2, cmplx(1,kind=4), A%cdata, LD1, cmplx(B%idata(1:lenB),kind=4), 1, cmplx(0,kind=4), R%cdata, 1)
			case(42)
				call CGEMV('N', LD1, LD2, cmplx(1,kind=4), A%cdata, LD1, cmplx(B%sdata(1:lenB),kind=4), 1, cmplx(0,kind=4), R%cdata, 1)
			case(43)
				call ZGEMV('N', LD1, LD2, dcmplx(1),dcmplx(A%cdata(1:length)), LD1, dcmplx(B%ddata(1:lenB)), 1, dcmplx(0), R%zdata, 1)
			case(44)
				call CGEMV('N', LD1, LD2, cmplx(1,kind=4), A%cdata, LD1, B%cdata, 1, cmplx(0,kind=4), R%cdata, 1)
			case(45)
				call ZGEMV('N', LD1, LD2, dcmplx(1), dcmplx(A%cdata), LD1, B%zdata, 1, dcmplx(0), R%zdata, 1)	
				
			case(51)
				call ZGEMV('N', LD1, LD2, dcmplx(1), A%zdata, LD1, dcmplx(B%idata(1:lenB)), 1, dcmplx(0), R%zdata, 1)	
			case(52)
				call ZGEMV('N', LD1, LD2, dcmplx(1), A%zdata, LD1, dcmplx(B%sdata(1:lenB)), 1, dcmplx(0), R%zdata, 1)	
			case(53)
				call ZGEMV('N', LD1, LD2, dcmplx(1), A%zdata, LD1, dcmplx(B%ddata(1:lenB)), 1, dcmplx(0), R%zdata, 1)	
			case(54)
				call ZGEMV('N', LD1, LD2, dcmplx(1), A%zdata, LD1, dcmplx(B%cdata(1:lenB)), 1, dcmplx(0), R%zdata, 1)	
			case(55)
				call ZGEMV('N', LD1, LD2, dcmplx(1), A%zdata, LD1, B%zdata, 1, dcmplx(0), R%zdata, 1)	
		end select
		return
	end subroutine
	
	subroutine product_MV_dim1_par(R,A,B,LD1,LD2,alpha,beta)!Res=alpha*A*B+beta*Res
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		class(*),intent(in)::alpha,beta	
		integer,intent(in)::LD1,LD2
		integer::length,flag,lenB
		flag=10*A%classType+B%classType
		length=A%totalData
		lenB=B%totalData
		select case(flag)
			case(11)
				call matrix_mv_int_par(R%idata,A%idata,B%idata,LD1,LD2,LD2,1,iselect(alpha),iselect(beta))
			case(12)
				call SGEMV('N', LD1, LD2, sselect(alpha), real(A%idata(1:length),kind=4), LD1, B%sdata, 1, sselect(beta), R%sdata, 1)
			case(13)
				call DGEMV('N', LD1, LD2, dselect(alpha), dble(A%idata(1:length)), LD1, B%ddata, 1, dselect(beta), R%ddata, 1)
			case(14)
				call CGEMV('N', LD1, LD2, cselect(alpha), cmplx(A%idata(1:length),kind=4), LD1, B%cdata, 1, cselect(beta), R%cdata, 1)
			case(15)
				call ZGEMV('N', LD1, LD2, zselect(alpha), dcmplx(A%idata(1:length)), LD1, B%zdata, 1, zselect(beta), R%zdata, 1)
				
			case(21)
				call SGEMV('N', LD1, LD2, sselect(alpha), A%sdata, LD1, real(B%idata(1:lenB),kind=4), 1, sselect(beta), R%sdata, 1)
			case(22)
				call SGEMV('N', LD1, LD2, sselect(alpha), A%sdata, LD1, B%sdata, 1, sselect(beta), R%sdata, 1)
			case(23)
				call DGEMV('N', LD1, LD2, dselect(alpha), dble(A%sdata(1:length)), LD1, B%ddata, 1, dselect(beta), R%ddata, 1)
			case(24)
				call CGEMV('N', LD1, LD2, cselect(alpha), cmplx(A%sdata(1:length),kind=4), LD1, B%cdata, 1, cselect(beta), R%cdata, 1)
			case(25)
				call ZGEMV('N', LD1, LD2, zselect(alpha), dcmplx(A%sdata(1:length)), LD1, B%zdata, 1, zselect(beta), R%zdata, 1)
				
			case(31)
				call DGEMV('N', LD1, LD2, dselect(alpha), A%ddata, LD1, dble(B%idata(1:lenB)), 1, dselect(beta), R%ddata, 1)
			case(32)
				call DGEMV('N', LD1, LD2, dselect(alpha), A%ddata, LD1, dble(B%sdata(1:lenB)), 1, dselect(beta), R%ddata, 1)
			case(33)
				call DGEMV('N', LD1, LD2, dselect(alpha), A%ddata, LD1, B%ddata, 1, dselect(beta), R%ddata, 1)
			case(34)
				call ZGEMV('N', LD1, LD2, zselect(alpha), dcmplx(A%ddata(1:length)), LD1, dcmplx(B%cdata(1:lenB)), 1, zselect(beta), R%zdata, 1)
			case(35)
				call ZGEMV('N', LD1, LD2, zselect(alpha), dcmplx(A%ddata(1:length)), LD1, B%zdata, 1, zselect(beta), R%zdata, 1)	
				
			case(41)
				call CGEMV('N', LD1, LD2, cselect(alpha), A%cdata, LD1, cmplx(B%idata(1:lenB),kind=4), 1, cselect(beta), R%cdata, 1)
			case(42)
				call CGEMV('N', LD1, LD2, cselect(alpha), A%cdata, LD1, cmplx(B%sdata(1:lenB),kind=4), 1, cselect(beta), R%cdata, 1)
			case(43)
				call ZGEMV('N', LD1, LD2, zselect(alpha),dcmplx(A%cdata(1:length)), LD1, dcmplx(B%ddata(1:lenB)), 1, zselect(beta), R%zdata, 1)
			case(44)
				call CGEMV('N', LD1, LD2, cselect(alpha), A%cdata, LD1, B%cdata, 1, cselect(beta), R%cdata, 1)
			case(45)
				call ZGEMV('N', LD1, LD2, zselect(alpha), dcmplx(A%cdata), LD1, B%zdata, 1, zselect(beta), R%zdata, 1)	
				
			case(51)
				call ZGEMV('N', LD1, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%idata(1:lenB)), 1, zselect(beta), R%zdata, 1)	
			case(52)
				call ZGEMV('N', LD1, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%sdata(1:lenB)), 1, zselect(beta), R%zdata, 1)	
			case(53)
				call ZGEMV('N', LD1, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%ddata(1:lenB)), 1, zselect(beta), R%zdata, 1)	
			case(54)
				call ZGEMV('N', LD1, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%cdata(1:lenB)), 1, zselect(beta), R%zdata, 1)	
			case(55)
				call ZGEMV('N', LD1, LD2, zselect(alpha), A%zdata, LD1, B%zdata, 1, zselect(beta), R%zdata, 1)	
		end select
		return
	end subroutine
	subroutine product_MV_dim1_parameter(R,A,B,LD1,LD2,alpha,beta,TRANSA)!Res=alpha*A*B+beta*Res
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		class(*),intent(in)::alpha,beta	
		character*1,intent(in)::TRANSA
		integer,intent(in)::LD1,LD2
		integer::length,flag,lenB
		flag=10*A%classType+B%classType
		length=A%totalData
		lenB=B%totalData
		select case(flag)
			case(11)
				call matrix_mv_int_par(R%idata,A%idata,B%idata,LD1,LD2,LD2,1,iselect(alpha),iselect(beta))
			case(12)
				call SGEMV(TRANSA, LD1, LD2, sselect(alpha), real(A%idata(1:length),kind=4), LD1, B%sdata, 1, sselect(beta), R%sdata, 1)
			case(13)
				call DGEMV(TRANSA, LD1, LD2, dselect(alpha), dble(A%idata(1:length)), LD1, B%ddata, 1, dselect(beta), R%ddata, 1)
			case(14)
				call CGEMV(TRANSA, LD1, LD2, cselect(alpha), cmplx(A%idata(1:length),kind=4), LD1, B%cdata, 1, cselect(beta), R%cdata, 1)
			case(15)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha), dcmplx(A%idata(1:length)), LD1, B%zdata, 1, zselect(beta), R%zdata, 1)
				
			case(21)
				call SGEMV(TRANSA, LD1, LD2, sselect(alpha), A%sdata, LD1, real(B%idata(1:lenB),kind=4), 1, sselect(beta), R%sdata, 1)
			case(22)
				call SGEMV(TRANSA, LD1, LD2, sselect(alpha), A%sdata, LD1, B%sdata, 1, sselect(beta), R%sdata, 1)
			case(23)
				call DGEMV(TRANSA, LD1, LD2, dselect(alpha), dble(A%sdata(1:length)), LD1, B%ddata, 1, dselect(beta), R%ddata, 1)
			case(24)
				call CGEMV(TRANSA, LD1, LD2, cselect(alpha), cmplx(A%sdata(1:length),kind=4), LD1, B%cdata, 1, cselect(beta), R%cdata, 1)
			case(25)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha), dcmplx(A%sdata(1:length)), LD1, B%zdata, 1, zselect(beta), R%zdata, 1)
				
			case(31)
				call DGEMV(TRANSA, LD1, LD2, dselect(alpha), A%ddata, LD1, dble(B%idata(1:lenB)), 1, dselect(beta), R%ddata, 1)
			case(32)
				call DGEMV(TRANSA, LD1, LD2, dselect(alpha), A%ddata, LD1, dble(B%sdata(1:lenB)), 1, dselect(beta), R%ddata, 1)
			case(33)
				call DGEMV(TRANSA, LD1, LD2, dselect(alpha), A%ddata, LD1, B%ddata, 1, dselect(beta), R%ddata, 1)
			case(34)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha), dcmplx(A%ddata(1:length)), LD1, dcmplx(B%cdata(1:lenB)), 1, &
																zselect(beta), R%zdata, 1)
			case(35)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha), dcmplx(A%ddata(1:length)), LD1, B%zdata, 1, zselect(beta), R%zdata, 1)	
				
			case(41)
				call CGEMV(TRANSA, LD1, LD2, cselect(alpha), A%cdata, LD1, cmplx(B%idata(1:lenB),kind=4), 1, cselect(beta), R%cdata, 1)
			case(42)
				call CGEMV(TRANSA, LD1, LD2, cselect(alpha), A%cdata, LD1, cmplx(B%sdata(1:lenB),kind=4), 1, cselect(beta), R%cdata, 1)
			case(43)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha),dcmplx(A%cdata(1:length)), LD1, dcmplx(B%ddata(1:lenB)), 1, &
																zselect(beta), R%zdata, 1)
			case(44)
				call CGEMV(TRANSA, LD1, LD2, cselect(alpha), A%cdata, LD1, B%cdata, 1, cselect(beta), R%cdata, 1)
			case(45)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha), dcmplx(A%cdata), LD1, B%zdata, 1, zselect(beta), R%zdata, 1)	
				
			case(51)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%idata(1:lenB)), 1, zselect(beta), R%zdata, 1)	
			case(52)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%sdata(1:lenB)), 1, zselect(beta), R%zdata, 1)	
			case(53)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%ddata(1:lenB)), 1, zselect(beta), R%zdata, 1)	
			case(54)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%cdata(1:lenB)), 1, zselect(beta), R%zdata, 1)	
			case(55)
				call ZGEMV(TRANSA, LD1, LD2, zselect(alpha), A%zdata, LD1, B%zdata, 1, zselect(beta), R%zdata, 1)	
		end select
		return
	end subroutine
	
	subroutine product_VM_dim1(R,A,B,LD1,LD2)	
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer,intent(in)::LD1,LD2
		integer::flag,length,lenB
		flag=10*A%classType+B%classType
		length=A%totalData
		lenB=B%totalData
		select case(flag)
			case(11)
				call matrix_mv_int(R%idata,A%idata,B%idata,1,LD1,LD1,LD2)
			case(12)
				call SGEMV('T', LD1, LD2, 1., B%sdata, LD1, real(A%idata(1:length),kind=4), 1, 0., R%sdata, 1)
			case(13)
				call DGEMV('T', LD1, LD2, 1d0, B%ddata, LD1, dble(A%idata(1:length)), 1, 0d0, R%ddata, 1)
			case(14)
				call CGEMV('T', LD1, LD2, cmplx(1,kind=4), B%cdata, LD1, cmplx(A%idata(1:length),kind=4), 1, cmplx(0,kind=4), R%cdata, 1)
			case(15)
				call ZGEMV('T', LD1, LD2, dcmplx(1), B%zdata, LD1, dcmplx(A%idata(1:length)), 1, dcmplx(0), R%zdata, 1)
				
			case(21)
				call SGEMV('T', LD1, LD2, 1., real(B%idata(1:lenB),kind=4), LD1, A%sdata, 1, 0., R%sdata, 1)
			case(22)
				call SGEMV('T', LD1, LD2, 1., B%sdata, LD1, A%sdata, 1, 0., R%sdata, 1)
			case(23)
				call DGEMV('T', LD1, LD2, 1d0, B%ddata, LD1, dble(A%sdata(1:length)), 1, 0d0, R%ddata, 1)
			case(24)
				call CGEMV('T', LD1, LD2, cmplx(1,kind=4), B%cdata, LD1, cmplx(A%sdata(1:length),kind=4), 1, cmplx(0,kind=4), R%cdata, 1)
			case(25)
				call ZGEMV('T', LD1, LD2, dcmplx(1), B%zdata, LD1, dcmplx(A%sdata(1:length)), 1, dcmplx(0), R%zdata, 1)
				
			case(31)
				call DGEMV('T', LD1, LD2, 1d0, dble(B%idata(1:lenB)), LD1, A%ddata, 1, 0d0, R%ddata, 1)
			case(32)
				call DGEMV('T', LD1, LD2, 1d0, dble(B%sdata(1:lenB)), LD1, A%ddata, 1, 0d0, R%ddata, 1)
			case(33)
				call DGEMV('T', LD1, LD2, 1d0, B%ddata, LD1, A%ddata, 1, 0d0, R%ddata, 1)
			case(34)
				call ZGEMV('T', LD1, LD2, dcmplx(1), dcmplx(B%cdata(1:lenB)), LD1, dcmplx(A%ddata(1:length)), 1, dcmplx(0), R%zdata, 1)
			case(35)
				call ZGEMV('T', LD1, LD2, dcmplx(1), B%zdata, LD1, dcmplx(A%ddata(1:length)), 1, dcmplx(0), R%zdata, 1)	
				
			case(41)
				call CGEMV('T', LD1, LD2, cmplx(1,kind=4), cmplx(B%idata(1:lenB),kind=4), LD1, A%cdata, 1, cmplx(0,kind=4), R%cdata, 1)
			case(42)
				call CGEMV('T', LD1, LD2, cmplx(1,kind=4),cmplx(B%sdata(1:lenB),kind=4), LD1, A%cdata, 1, cmplx(0,kind=4), R%cdata, 1)
			case(43)
				call ZGEMV('T', LD1, LD2, dcmplx(1), dcmplx(B%ddata(1:lenB)), LD1, dcmplx(A%cdata(1:length)), 1, dcmplx(0), R%zdata, 1)
			case(44)
				call CGEMV('T', LD1, LD2, cmplx(1,kind=4), B%cdata, LD1, A%cdata, 1, cmplx(0,kind=4), R%cdata, 1)
			case(45)
				call ZGEMV('T', LD1, LD2, dcmplx(1), B%zdata, LD1, dcmplx(A%cdata(1:length)), 1, dcmplx(0), R%zdata, 1)		
				
			case(51)
				call ZGEMV('T', LD1, LD2, dcmplx(1), dcmplx(B%idata(1:lenB)), LD1, A%zdata, 1, dcmplx(0), R%zdata, 1)				
			case(52)
				call ZGEMV('T', LD1, LD2, dcmplx(1), dcmplx(B%sdata(1:lenB)), LD1, A%zdata, 1, dcmplx(0), R%zdata, 1)	
			case(53)
				call ZGEMV('T', LD1, LD2, dcmplx(1), dcmplx(B%ddata(1:lenB)), LD1, A%zdata, 1, dcmplx(0), R%zdata, 1)	
			case(54)
				call ZGEMV('T', LD1, LD2, dcmplx(1), dcmplx(B%cdata(1:lenB)), LD1, A%zdata, 1, dcmplx(0), R%zdata, 1)	
			case(55)
				call ZGEMV('T', LD1, LD2, dcmplx(1), B%zdata, LD1, A%zdata, 1, dcmplx(0), R%zdata, 1)		
		end select
		return
	end subroutine
	
	subroutine product_VM_dim1_par(R,A,B,LD1,LD2,alpha,beta)!Res=alpha*A*B+beta*Res
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		class(*),intent(in)::alpha,beta	
		integer,intent(in)::LD1,LD2
		integer::flag,length,lenB
		flag=10*A%classType+B%classType
		length=A%totalData
		lenB=B%totalData
		select case(flag)
			case(11)
				call matrix_mv_int_par(R%idata,A%idata,B%idata,1,LD1,LD1,LD2,iselect(alpha),iselect(beta))
			case(12)
				call SGEMV('T', LD1, LD2, sselect(alpha), B%sdata, LD1, real(A%idata(1:length),kind=4), 1, sselect(beta), R%sdata, 1)
			case(13)
				call DGEMV('T', LD1, LD2, dselect(alpha), B%ddata, LD1, dble(A%idata(1:length)), 1, dselect(beta), R%ddata, 1)
			case(14)
				call CGEMV('T', LD1, LD2, cselect(alpha), B%cdata, LD1, cmplx(A%idata(1:length),kind=4), 1, cselect(beta), R%cdata, 1)
			case(15)
				call ZGEMV('T', LD1, LD2, zselect(alpha), B%zdata, LD1, dcmplx(A%idata(1:length)), 1, zselect(beta), R%zdata, 1)
				
			case(21)
				call SGEMV('T', LD1, LD2, sselect(alpha), real(B%idata(1:lenB),kind=4), LD1, A%sdata, 1, sselect(beta), R%sdata, 1)
			case(22)
				call SGEMV('T', LD1, LD2, sselect(alpha), B%sdata, LD1, A%sdata, 1, sselect(beta), R%sdata, 1)
			case(23)
				call DGEMV('T', LD1, LD2, dselect(alpha), B%ddata, LD1, dble(A%sdata(1:length)), 1, dselect(beta), R%ddata, 1)
			case(24)
				call CGEMV('T', LD1, LD2, cselect(alpha), B%cdata, LD1, cmplx(A%sdata(1:length),kind=4), 1, cselect(beta), R%cdata, 1)
			case(25)
				call ZGEMV('T', LD1, LD2, zselect(alpha), B%zdata, LD1, dcmplx(A%sdata(1:length)), 1, zselect(beta), R%zdata, 1)
				
			case(31)
				call DGEMV('T', LD1, LD2, dselect(alpha), dble(B%idata(1:lenB)), LD1, A%ddata, 1, dselect(beta), R%ddata, 1)
			case(32)
				call DGEMV('T', LD1, LD2, dselect(alpha), dble(B%sdata(1:lenB)), LD1, A%ddata, 1, dselect(beta), R%ddata, 1)
			case(33)
				call DGEMV('T', LD1, LD2, dselect(alpha), B%ddata, LD1, A%ddata, 1, dselect(beta), R%ddata, 1)
			case(34)
				call ZGEMV('T', LD1, LD2, zselect(alpha), dcmplx(B%cdata(1:lenB)), LD1, dcmplx(A%ddata(1:length)), 1, zselect(beta), R%zdata, 1)
			case(35)
				call ZGEMV('T', LD1, LD2, zselect(alpha), B%zdata, LD1, dcmplx(A%ddata(1:length)), 1, zselect(beta), R%zdata, 1)	
				
			case(41)
				call CGEMV('T', LD1, LD2, cselect(alpha), cmplx(B%idata(1:lenB),kind=4), LD1, A%cdata, 1, cselect(beta), R%cdata, 1)
			case(42)
				call CGEMV('T', LD1, LD2, cselect(alpha),cmplx(B%sdata(1:lenB),kind=4), LD1, A%cdata, 1, cselect(beta), R%cdata, 1)
			case(43)
				call ZGEMV('T', LD1, LD2, zselect(alpha), dcmplx(B%ddata(1:lenB)), LD1, dcmplx(A%cdata(1:length)), 1, zselect(beta), R%zdata, 1)
			case(44)
				call CGEMV('T', LD1, LD2, cselect(alpha), B%cdata, LD1, A%cdata, 1, cselect(beta), R%cdata, 1)
			case(45)
				call ZGEMV('T', LD1, LD2, zselect(alpha), B%zdata, LD1, dcmplx(A%cdata(1:length)), 1, zselect(beta), R%zdata, 1)		
				
			case(51)
				call ZGEMV('T', LD1, LD2, zselect(alpha), dcmplx(B%idata(1:lenB)), LD1, A%zdata, 1, zselect(beta), R%zdata, 1)				
			case(52)
				call ZGEMV('T', LD1, LD2, zselect(alpha), dcmplx(B%sdata(1:lenB)), LD1, A%zdata, 1, zselect(beta), R%zdata, 1)	
			case(53)
				call ZGEMV('T', LD1, LD2, zselect(alpha), dcmplx(B%ddata(1:lenB)), LD1, A%zdata, 1, zselect(beta), R%zdata, 1)	
			case(54)
				call ZGEMV('T', LD1, LD2, zselect(alpha), dcmplx(B%cdata(1:lenB)), LD1, A%zdata, 1, zselect(beta), R%zdata, 1)	
			case(55)
				call ZGEMV('T', LD1, LD2, zselect(alpha), B%zdata, LD1, A%zdata, 1, zselect(beta), R%zdata, 1)		
		end select
		return
	end subroutine
!************************************************************************
!           mat*mat
!************************************************************************	
	subroutine SSSMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		real(kind=4),intent(inout)::R(M,N)
		real(kind=4),intent(in)::A(M,K),B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine DDSMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		real(kind=8),intent(inout)::R(M,N)
		real(kind=8),intent(in)::A(M,K)
		real(kind=4),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine DSDMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		real(kind=8),intent(inout)::R(M,N)
		real(kind=4),intent(in)::A(M,K)
		real(kind=8),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine DDDMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		real(kind=8),intent(inout)::R(M,N)
		real(kind=8),intent(in)::A(M,K),B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine CCSMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=4),intent(inout)::R(M,N)
		complex(kind=4),intent(in)::A(M,K)
		real(kind=4),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine CSCMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=4),intent(inout)::R(M,N)
		real(kind=4),intent(in)::A(M,K)
		complex(kind=4),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine CCCMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=4),intent(inout)::R(M,N)
		complex(kind=4),intent(in)::A(M,K),B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine ZCDMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=8),intent(inout)::R(M,N)
		complex(kind=4),intent(in)::A(M,K)
		real(kind=8),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine ZDCMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=8),intent(inout)::R(M,N)
		real(kind=8),intent(in)::A(M,K)
		complex(kind=4),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine ZCZMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=8),intent(inout)::R(M,N)
		complex(kind=4),intent(in)::A(M,K)
		complex(kind=8),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine ZZCMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=8),intent(inout)::R(M,N)
		complex(kind=8),intent(in)::A(M,K)
		complex(kind=4),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine ZZZMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=8),intent(inout)::R(M,N)
		complex(kind=8),intent(in)::A(M,K)
		complex(kind=8),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine ZSZMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=8),intent(inout)::R(M,N)
		real*4,intent(in)::A(M,K)
		complex(kind=8),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine ZZSMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=8),intent(inout)::R(M,N)
		complex(kind=8),intent(in)::A(M,K)
		real(kind=4),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine ZDZMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=8),intent(inout)::R(M,N)
		real*8,intent(in)::A(M,K)
		complex(kind=8),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	subroutine ZZDMM(R,A,B,M,K,N)
		integer,intent(in)::M,K,N
		complex(kind=8),intent(inout)::R(M,N)
		complex(kind=8),intent(in)::A(M,K)
		real(kind=8),intent(in)::B(K,N)
		R=matmul(A,B)
		return
	end subroutine
	
	
		
	subroutine product_MM_dim1(R,A,B,LD1,LD2,LD3)	
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer,intent(in)::LD1,LD2,LD3
		integer::flag,length,lenB
		flag=10*A%classType+B%classType
		length=A%totalData
		lenB=B%totalData
		if((length+lenB).le.LAPACK_LENGTH2)flag=-flag
		select case(flag)
			case(11)
				call matrix_mv_int(R%idata,A%idata,B%idata,LD1,LD2,LD2,LD3)
			case(12)
				call SGEMM('N', 'N', LD1, LD3, LD2, 1., real(A%idata(1:length),kind=4), LD1, B%sdata, LD2, 0., R%sdata, LD1)
			case(13)
				call DGEMM('N', 'N', LD1, LD3, LD2, 1d0, dble(A%idata(1:length)), LD1, B%ddata, LD2, 0d0, R%ddata, LD1)
			case(14)
				call CGEMM('N', 'N', LD1, LD3, LD2, cmplx(1,kind=4), cmplx(A%idata(1:length),kind=4), LD1, B%cdata, LD2,&
																																 cmplx(0d0,kind=4), R%cdata, LD1)
			case(15)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), dcmplx(A%idata(1:length)), LD1, B%zdata, LD2, dcmplx(0d0), R%zdata, LD1)
				
			case(21)
				call SGEMM('N', 'N', LD1, LD3, LD2, 1., A%sdata, LD1, real(B%idata(1:lenB),kind=4), LD2, 0., R%sdata, LD1)
			case(22)
				call SGEMM('N', 'N', LD1, LD3, LD2, 1., A%sdata, LD1, B%sdata, LD2, 0., R%sdata, LD1)
			case(23)
				call DGEMM('N', 'N', LD1, LD3, LD2, 1d0, dble(A%sdata(1:length)), LD1, B%ddata, LD2, 0d0, R%ddata, LD1)
			case(24)
				call CGEMM('N', 'N', LD1, LD3, LD2, cmplx(1,kind=4), cmplx(A%sdata(1:length),kind=4), LD1, B%cdata, LD2,&
																																 cmplx(0d0,kind=4), R%cdata, LD1)
			case(25)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), dcmplx(A%sdata(1:length)), LD1, B%zdata, LD2, dcmplx(0d0), R%zdata, LD1)
			
			case(31)
				call DGEMM('N', 'N', LD1, LD3, LD2, 1d0, A%ddata, LD1, dble(B%idata(1:lenB)), LD2, 0d0, R%ddata, LD1)
			case(32)
				call DGEMM('N', 'N', LD1, LD3, LD2, 1d0, A%ddata, LD1, dble(B%sdata(1:lenB)), LD2, 0d0, R%ddata, LD1)
			case(33)
				call DGEMM('N', 'N', LD1, LD3, LD2, 1d0, A%ddata, LD1, B%ddata, LD2, 0d0, R%ddata, LD1)
			case(34)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), dcmplx(A%ddata(1:length)), LD1, dcmplx(B%cdata(1:lenB)), LD2,&
																																			 dcmplx(0d0), R%zdata, LD1)
			case(35)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), dcmplx(A%ddata(1:length)), LD1, B%zdata, LD2, dcmplx(0d0), R%zdata, LD1)
			
			case(41)
				call CGEMM('N', 'N', LD1, LD3, LD2, cmplx(1,kind=4), A%cdata, LD1, cmplx(B%idata(1:lenB),kind=4), LD2,&
																																	 cmplx(0d0,kind=4), R%cdata, LD1)
			case(42)
				call CGEMM('N', 'N', LD1, LD3, LD2, cmplx(1,kind=4), A%cdata, LD1, cmplx(B%sdata(1:lenB),kind=4), LD2,&
																																		 cmplx(0d0,kind=4), R%cdata, LD1)
			case(43)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), dcmplx(A%cdata(1:length)), LD1, dcmplx(B%ddata(1:lenB)),&
																																		 LD2, dcmplx(0d0), R%zdata, LD1)
			case(44)
				call CGEMM('N', 'N', LD1, LD3, LD2, cmplx(1,kind=4), A%cdata, LD1, B%cdata, LD2, cmplx(0d0,kind=4), R%cdata, LD1)
			case(45)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), dcmplx(A%cdata(1:length)), LD1, B%zdata, LD2, dcmplx(0d0), R%zdata, LD1)
			
			case(51)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), A%zdata, LD1, dcmplx(B%idata(1:lenB)), LD2, dcmplx(0d0), R%zdata, LD1)
			case(52)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), A%zdata, LD1, dcmplx(B%sdata(1:lenB)), LD2, dcmplx(0d0), R%zdata, LD1)
			case(53)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), A%zdata, LD1, dcmplx(B%ddata(1:lenB)), LD2, dcmplx(0d0), R%zdata, LD1)
			case(54)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), A%zdata, LD1, dcmplx(B%cdata(1:lenB)), LD2, dcmplx(0d0), R%zdata, LD1)
			case(55)
				call ZGEMM('N', 'N', LD1, LD3, LD2, dcmplx(1), A%zdata, LD1, B%zdata, LD2, dcmplx(0d0), R%zdata, LD1)
			
			
			
			case(-11)
				call matrix_mv_int(R%idata,A%idata,B%idata,LD1,LD2,LD2,LD3)
			case(-12)
				call SSSMM(R%sdata,real(A%idata,kind=4),B%sdata,LD1,LD2,LD3)
			case(-13)
				call DSDMM(R%ddata,real(A%idata,kind=4),B%ddata,LD1,LD2,LD3)
			case(-14)
				call CSCMM(R%cdata,real(A%idata,kind=4),B%cdata,LD1,LD2,LD3)
			case(-15)
				call ZSZMM(R%zdata,real(A%idata,kind=4),B%zdata,LD1,LD2,LD3)
			case(-21)
				call SSSMM(R%sdata,A%sdata,real(B%idata,kind=4),LD1,LD2,LD3)
			case(-22)
				call SSSMM(R%sdata,A%sdata,B%sdata,LD1,LD2,LD3)
			case(-23)
				call DSDMM(R%ddata,A%sdata,B%ddata,LD1,LD2,LD3)
			case(-24)
				call CSCMM(R%cdata,A%sdata,B%cdata,LD1,LD2,LD3)
			case(-25)
				call ZSZMM(R%zdata,A%sdata,B%zdata,LD1,LD2,LD3)
			case(-31)
				call DDSMM(R%ddata,A%ddata,real(B%idata,kind=4),LD1,LD2,LD3)
			case(-32)
				call DDSMM(R%ddata,A%ddata,B%sdata,LD1,LD2,LD3)
			case(-33)
				call DDDMM(R%ddata,A%ddata,B%ddata,LD1,LD2,LD3)
			case(-34)
				call ZDCMM(R%zdata,A%ddata,B%cdata,LD1,LD2,LD3)
			case(-35)
				call ZDZMM(R%zdata,A%ddata,B%zdata,LD1,LD2,LD3)
			case(-41)
				call CCSMM(R%cdata,A%cdata,real(B%idata,kind=4),LD1,LD2,LD3)
			case(-42)
				call CCSMM(R%cdata,A%cdata,B%sdata,LD1,LD2,LD3)
			case(-43)
				call ZCDMM(R%zdata,A%cdata,B%ddata,LD1,LD2,LD3)
			case(-44)
				call CCCMM(R%cdata,A%cdata,B%cdata,LD1,LD2,LD3)
			case(-45)
				call ZCZMM(R%zdata,A%cdata,B%zdata,LD1,LD2,LD3)
			case(-51)
				call ZZSMM(R%zdata,A%zdata,real(B%idata,kind=4),LD1,LD2,LD3)
			case(-52)
				call ZZSMM(R%zdata,A%zdata,B%sdata,LD1,LD2,LD3)
			case(-53)
				call ZZDMM(R%zdata,A%zdata,B%ddata,LD1,LD2,LD3)
			case(-54)
				call ZZCMM(R%zdata,A%zdata,B%cdata,LD1,LD2,LD3)
			case(-55)
				call ZZZMM(R%zdata,A%zdata,B%zdata,LD1,LD2,LD3)
		end select
		return
	end subroutine
	
	subroutine product_MM_dim1_par(R,A,B,LD1,LD2,LD3,alpha,beta)!Res=alpha*A*B+beta*Res
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		class(*),intent(in)::alpha,beta	
		integer,intent(in)::LD1,LD2,LD3
		integer::flag,length,lenB
		flag=10*A%classType+B%classType
		length=A%totalData
		lenB=B%totalData
		if((length+lenB).le.LAPACK_LENGTH2)flag=-flag
		select case(flag)
			case(11)
				call matrix_mv_int_par(R%idata,A%idata,B%idata,LD1,LD2,LD2,LD3,iselect(alpha),iselect(beta))
			case(12)
				call SGEMM('N', 'N', LD1, LD3, LD2, sselect(alpha), real(A%idata(1:length),kind=4), LD1, B%sdata, LD2, sselect(beta)&
																																	, R%sdata, LD1)
			case(13)
				call DGEMM('N', 'N', LD1, LD3, LD2, dselect(alpha), dble(A%idata(1:length)), LD1, B%ddata, LD2, dselect(beta), R%ddata, LD1)
			case(14)
				call CGEMM('N', 'N', LD1, LD3, LD2, cselect(alpha), cmplx(A%idata(1:length),kind=4), LD1, B%cdata, LD2,&
																																 cselect(beta), R%cdata, LD1)
			case(15)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), dcmplx(A%idata(1:length)), LD1, B%zdata, LD2, zselect(beta), R%zdata, LD1)
				
			case(21)
				call SGEMM('N', 'N', LD1, LD3, LD2, sselect(alpha), A%sdata, LD1, real(B%idata(1:lenB),kind=4), LD2, sselect(beta)&
																																	, R%sdata, LD1)
			case(22)
				call SGEMM('N', 'N', LD1, LD3, LD2, sselect(alpha), A%sdata, LD1, B%sdata, LD2, sselect(beta), R%sdata, LD1)
			case(23)
				call DGEMM('N', 'N', LD1, LD3, LD2, dselect(alpha), dble(A%sdata(1:length)), LD1, B%ddata, LD2, dselect(beta), R%ddata, LD1)
			case(24)
				call CGEMM('N', 'N', LD1, LD3, LD2, cselect(alpha), cmplx(A%sdata(1:length),kind=4), LD1, B%cdata, LD2,&
																																 cselect(beta), R%cdata, LD1)
			case(25)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), dcmplx(A%sdata(1:length)), LD1, B%zdata, LD2, zselect(beta), R%zdata, LD1)
			
			case(31)
				call DGEMM('N', 'N', LD1, LD3, LD2, dselect(alpha), A%ddata, LD1, dble(B%idata(1:lenB)), LD2, dselect(beta), R%ddata, LD1)
			case(32)
				call DGEMM('N', 'N', LD1, LD3, LD2, dselect(alpha), A%ddata, LD1, dble(B%sdata(1:lenB)), LD2, dselect(beta), R%ddata, LD1)
			case(33)
				call DGEMM('N', 'N', LD1, LD3, LD2, dselect(alpha), A%ddata, LD1, B%ddata, LD2, dselect(beta), R%ddata, LD1)
			case(34)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), dcmplx(A%ddata(1:length)), LD1, dcmplx(B%cdata(1:lenB)), LD2,&
																																			 zselect(beta), R%zdata, LD1)
			case(35)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), dcmplx(A%ddata(1:length)), LD1, B%zdata, LD2, zselect(beta), R%zdata, LD1)
			
			case(41)
				call CGEMM('N', 'N', LD1, LD3, LD2, cselect(alpha), A%cdata, LD1, cmplx(B%idata(1:lenB),kind=4), LD2,&
																																	 cselect(beta), R%cdata, LD1)
			case(42)
				call CGEMM('N', 'N', LD1, LD3, LD2, cselect(alpha), A%cdata, LD1, cmplx(B%sdata(1:lenB),kind=4), LD2,&
																																		 cselect(beta), R%cdata, LD1)
			case(43)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), dcmplx(A%cdata(1:length)), LD1, dcmplx(B%ddata(1:lenB)),&
																																		 LD2, zselect(beta), R%zdata, LD1)
			case(44)
				call CGEMM('N', 'N', LD1, LD3, LD2, cselect(alpha), A%cdata, LD1, B%cdata, LD2, cselect(beta), R%cdata, LD1)
			case(45)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), dcmplx(A%cdata(1:length)), LD1, B%zdata, LD2, zselect(beta), R%zdata, LD1)
			
			case(51)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%idata(1:lenB)), LD2, zselect(beta), R%zdata, LD1)
			case(52)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%sdata(1:lenB)), LD2, zselect(beta), R%zdata, LD1)
			case(53)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%ddata(1:lenB)), LD2, zselect(beta), R%zdata, LD1)
			case(54)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), A%zdata, LD1, dcmplx(B%cdata(1:lenB)), LD2, zselect(beta), R%zdata, LD1)
			case(55)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zselect(alpha), A%zdata, LD1, B%zdata, LD2, zselect(beta), R%zdata, LD1)
			
			
			
			case default
			
				call writemess('ERROR in product_MM_dim1_par',-1)
				call error_stop
		end select
		return
	end subroutine
	
	
	subroutine product_MM_dim1_parameter(R,A,B,LD1,LD2,LD3,alpha,beta,TRANSA,TRANSB)!Res=alpha*A*B+beta*Res
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		class(*),intent(in)::alpha,beta	
		integer,intent(in)::LD1,LD2,LD3
		integer::flag,length,lenB
		character*1,intent(in)::TRANSA,TRANSB
		flag=10*A%classType+B%classType
		length=A%totalData
		lenB=B%totalData
		if((length+lenB).le.LAPACK_LENGTH2)flag=-flag
		select case(flag)
			case(11)
				call matrix_mv_int_par(R%idata,A%idata,B%idata,LD1,LD2,LD2,LD3,iselect(alpha),iselect(beta))
			case(12)
				call SGEMM(TRANSA,TRANSB, LD1, LD3, LD2, sselect(alpha), real(A%idata(1:length),kind=4), &
																				LD1, B%sdata, LD2, sselect(beta), R%sdata, LD1)
			case(13)
				call DGEMM(TRANSA,TRANSB, LD1, LD3, LD2, dselect(alpha), dble(A%idata(1:length))&
																				, LD1, B%ddata, LD2, dselect(beta), R%ddata, LD1)
			case(14)
				call CGEMM(TRANSA,TRANSB, LD1, LD3, LD2, cselect(alpha), cmplx(A%idata(1:length),kind=4)&
																				, LD1, B%cdata, LD2,cselect(beta), R%cdata, LD1)
			case(15)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha), dcmplx(A%idata(1:length))&
																				, LD1, B%zdata, LD2, zselect(beta), R%zdata, LD1)
			case(21)
				call SGEMM(TRANSA,TRANSB, LD1, LD3, LD2, sselect(alpha), A%sdata, LD1, real(B%idata(1:lenB),kind=4)&
																				, LD2, sselect(beta), R%sdata, LD1)
			case(22)
				call SGEMM(TRANSA,TRANSB, LD1, LD3, LD2, sselect(alpha), A%sdata, LD1, B%sdata, LD2, sselect(beta)&
																				, R%sdata, LD1)
			case(23)
				call DGEMM(TRANSA,TRANSB, LD1, LD3, LD2, dselect(alpha), dble(A%sdata(1:length)), LD1,&
																				 B%ddata, LD2, dselect(beta), R%ddata, LD1)
			case(24)
				call CGEMM(TRANSA,TRANSB, LD1, LD3, LD2, cselect(alpha), cmplx(A%sdata(1:length),kind=4)&
																				, LD1, B%cdata, LD2,cselect(beta), R%cdata, LD1)
			case(25)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha), dcmplx(A%sdata(1:length)),&
																				 LD1, B%zdata, LD2, zselect(beta), R%zdata, LD1)
			case(31)
				call DGEMM(TRANSA,TRANSB, LD1, LD3, LD2, dselect(alpha), A%ddata, LD1,&
																				dble(B%idata(1:lenB)), LD2, dselect(beta), R%ddata, LD1)
			case(32)
				call DGEMM(TRANSA,TRANSB, LD1, LD3, LD2, dselect(alpha), A%ddata, LD1,&
																				dble(B%sdata(1:lenB)), LD2, dselect(beta), R%ddata, LD1)
			case(33)
				call DGEMM(TRANSA,TRANSB, LD1, LD3, LD2, dselect(alpha), A%ddata, LD1, B%ddata, LD2, dselect(beta), R%ddata, LD1)
			case(34)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha), dcmplx(A%ddata(1:length)), LD1&
																				, dcmplx(B%cdata(1:lenB)), LD2,zselect(beta), R%zdata, LD1)
			case(35)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha), dcmplx(A%ddata(1:length))&
																				, LD1, B%zdata, LD2, zselect(beta), R%zdata, LD1)
			
			case(41)
				call CGEMM(TRANSA,TRANSB, LD1, LD3, LD2, cselect(alpha), A%cdata, LD1,&
																				 cmplx(B%idata(1:lenB),kind=4), LD2, cselect(beta), R%cdata, LD1)
			case(42)
				call CGEMM(TRANSA,TRANSB, LD1, LD3, LD2, cselect(alpha), A%cdata, LD1, &
																				cmplx(B%sdata(1:lenB),kind=4), LD2, cselect(beta), R%cdata, LD1)
			case(43)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha), dcmplx(A%cdata(1:length)), &
																	LD1, dcmplx(B%ddata(1:lenB)),LD2, zselect(beta), R%zdata, LD1)
			case(44)
				call CGEMM(TRANSA,TRANSB, LD1, LD3, LD2, cselect(alpha), A%cdata, LD1, B%cdata, LD2, cselect(beta), R%cdata, LD1)
			case(45)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha), dcmplx(A%cdata(1:length)),&
																	 LD1, B%zdata, LD2, zselect(beta), R%zdata, LD1)
			
			case(51)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha), A%zdata, LD1&
																	, dcmplx(B%idata(1:lenB)), LD2, zselect(beta), R%zdata, LD1)
			case(52)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha), A%zdata, LD1,&
																 dcmplx(B%sdata(1:lenB)), LD2, zselect(beta), R%zdata, LD1)
			case(53)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha), A%zdata, &
																	LD1, dcmplx(B%ddata(1:lenB)), LD2, zselect(beta), R%zdata, LD1)
			case(54)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha),&
																	 A%zdata, LD1, dcmplx(B%cdata(1:lenB)), LD2, zselect(beta), R%zdata, LD1)
			case(55)
				call ZGEMM(TRANSA,TRANSB, LD1, LD3, LD2, zselect(alpha), A%zdata, LD1, B%zdata, LD2, zselect(beta), R%zdata, LD1)
			
			
			
			case default
			
				call writemess('ERROR in product_MM_dim1_par',-1)
				call error_stop
		end select
		return
	end subroutine
	
!R=beta*R+alpha*(A*B)
	subroutine product_MM_dim1_num(R,beta,A,B,alpha,LD1,LD2,LD3)	
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		class(*),intent(in)::beta,alpha
		integer,intent(in)::LD1,LD2,LD3
		integer::flag,length,lenB
		integer::ibeta,ialpha
		real*4::sbeta,salpha
		real*8::dbeta,dalpha
		complex*8::cbeta,calpha
		complex*16::zbeta,zalpha
		flag=10*A%classType+B%classType
		length=A%totalData
		lenB=B%totalData
		select type(beta)
			type is (integer)
				ibeta=beta
			type is (real(kind=4))
				sbeta=beta
			type is (real(kind=8))
				dbeta=beta
			type is (complex(kind=4))
				cbeta=beta
			type is (complex(kind=8))
				zbeta=beta
		end select
		select type(alpha)
			type is (integer)
				ialpha=alpha
			type is (real(kind=4))
				salpha=alpha
			type is (real(kind=8))
				dalpha=alpha
			type is (complex(kind=4))
				calpha=alpha
			type is (complex(kind=8))
				zalpha=alpha
		end select
		
		select case(flag)
			case(11)
				call matrix_mv_int(R%idata,A%idata,B%idata,LD1,LD2,LD2,LD3)
			case(12)
				call SGEMM('N', 'N', LD1, LD3, LD2, salpha, real(A%idata(1:length),kind=4), LD1, B%sdata, LD2, sbeta, R%sdata, LD1)
			case(13)
				call DGEMM('N', 'N', LD1, LD3, LD2, dalpha, dble(A%idata(1:length)), LD1, B%ddata, LD2, dbeta, R%ddata, LD1)
			case(14)
				call CGEMM('N', 'N', LD1, LD3, LD2, calpha, cmplx(A%idata(1:length),kind=4), LD1, B%cdata, LD2,cbeta, R%cdata, LD1)
			case(15)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, dcmplx(A%idata(1:length)), LD1, B%zdata, LD2, zbeta, R%zdata, LD1)
				
			case(21)
				call SGEMM('N', 'N', LD1, LD3, LD2, salpha, A%sdata, LD1, real(B%idata(1:lenB),kind=4), LD2, sbeta, R%sdata, LD1)
			case(22)
				call SGEMM('N', 'N', LD1, LD3, LD2, salpha, A%sdata, LD1, B%sdata, LD2, sbeta, R%sdata, LD1)
			case(23)
				call DGEMM('N', 'N', LD1, LD3, LD2, dalpha, dble(A%sdata(1:length)), LD1, B%ddata, LD2, dbeta, R%ddata, LD1)
			case(24)
				call CGEMM('N', 'N', LD1, LD3, LD2, calpha, cmplx(A%sdata(1:length),kind=4), LD1, B%cdata, LD2,cbeta,R%cdata, LD1)
			case(25)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, dcmplx(A%sdata(1:length)), LD1, B%zdata, LD2, zbeta, R%zdata, LD1)
			
			case(31)
				call DGEMM('N', 'N', LD1, LD3, LD2, dalpha, A%ddata, LD1, dble(B%idata(1:lenB)), LD2, dbeta, R%ddata, LD1)
			case(32)
				call DGEMM('N', 'N', LD1, LD3, LD2, dalpha, A%ddata, LD1, dble(B%sdata(1:lenB)), LD2, dbeta, R%ddata, LD1)
			case(33)
				call DGEMM('N', 'N', LD1, LD3, LD2, dalpha, A%ddata, LD1, B%ddata, LD2, dbeta, R%ddata, LD1)
			case(34)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, dcmplx(A%ddata(1:length)), LD1, dcmplx(B%cdata(1:lenB)),&
																															 LD2,zbeta, R%zdata, LD1)
			case(35)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, dcmplx(A%ddata(1:length)), LD1, B%zdata, LD2, zbeta, R%zdata, LD1)
			
			case(41)
				call CGEMM('N', 'N', LD1, LD3, LD2, calpha, A%cdata, LD1, cmplx(B%idata(1:lenB),kind=4), LD2,cbeta, R%cdata, LD1)
			case(42)
				call CGEMM('N', 'N', LD1, LD3, LD2, calpha, A%cdata, LD1, cmplx(B%sdata(1:lenB),kind=4), LD2,cbeta, R%cdata, LD1)
			case(43)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, dcmplx(A%cdata(1:length)), LD1, dcmplx(B%ddata(1:lenB)),&
																																		 LD2, zbeta, R%zdata, LD1)
			case(44)
				call CGEMM('N', 'N', LD1, LD3, LD2, calpha, A%cdata, LD1, B%cdata, LD2, cbeta, R%cdata, LD1)
			case(45)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, dcmplx(A%cdata(1:length)), LD1, B%zdata, LD2, zbeta, R%zdata, LD1)
			
			case(51)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, A%zdata, LD1, dcmplx(B%idata(1:lenB)), LD2, zbeta, R%zdata, LD1)
			case(52)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, A%zdata, LD1, dcmplx(B%sdata(1:lenB)), LD2, zbeta, R%zdata, LD1)
			case(53)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, A%zdata, LD1, dcmplx(B%ddata(1:lenB)), LD2, zbeta, R%zdata, LD1)
			case(54)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, A%zdata, LD1, dcmplx(B%cdata(1:lenB)), LD2, zbeta, R%zdata, LD1)
			case(55)
				call ZGEMM('N', 'N', LD1, LD3, LD2, zalpha, A%zdata, LD1, B%zdata, LD2, zbeta, R%zdata, LD1)
		end select
		return
	end subroutine
!************************************************************************
!           mat*num
!************************************************************************
!there is  only one element in B
	subroutine product_Mnum_dim1(R,A,B)	
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		select case(B%classtype)
			case(1)
				call TDatamultiply_number_int(R,A,B%idata(1))
			case(2)
				call TDatamultiply_number_real4(R,A,B%sdata(1))
			case(3)
				call TDatamultiply_number_real8(R,A,B%ddata(1))
			case(4)
				call TDatamultiply_number_com4(R,A,B%cdata(1))
			case(5)
				call TDatamultiply_number_com8(R,A,B%zdata(1))
		end select
		return
	end subroutine
	subroutine product_Mnum_dim1_par(R,A,B,alpha,beta)	
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		class(*),intent(in)::alpha,beta
		select case(B%classtype)
			case(1)
				call TDatamultiply_number_int_par(R,A,B%idata(1),alpha,beta)
			case(2)
				call TDatamultiply_number_real4_par(R,A,B%sdata(1),alpha,beta)
			case(3)
				call TDatamultiply_number_real8_par(R,A,B%ddata(1),alpha,beta)
			case(4)
				call TDatamultiply_number_com4_par(R,A,B%cdata(1),alpha,beta)
			case(5)
				call TDatamultiply_number_com8_par(R,A,B%zdata(1),alpha,beta)
		end select
		return
	end subroutine
	
	subroutine product_NumNum(R,A,B)	
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag
		flag=10*A%classType+B%classType
		select case(flag)
			case(11)
				R%idata(1)=A%idata(1)*B%idata(1)
			case(12)
				R%sdata(1)=A%idata(1)*B%sdata(1)
			case(13)
				R%ddata(1)=A%idata(1)*B%ddata(1)
			case(14)
				R%cdata(1)=A%idata(1)*B%cdata(1)
			case(15)
				R%zdata(1)=A%idata(1)*B%zdata(1)
				
			case(21)
				R%sdata(1)=A%sdata(1)*B%idata(1)
			case(22)
				R%sdata(1)=A%sdata(1)*B%sdata(1)
			case(23)
				R%ddata(1)=A%sdata(1)*B%ddata(1)
			case(24)
				R%cdata(1)=A%sdata(1)*B%cdata(1)
			case(25)
				R%zdata(1)=A%sdata(1)*B%zdata(1)
					
			case(31)
				R%ddata(1)=A%ddata(1)*B%idata(1)
			case(32)
				R%ddata(1)=A%ddata(1)*B%sdata(1)
			case(33)
				R%ddata(1)=A%ddata(1)*B%ddata(1)
			case(34)
				R%zdata(1)=A%ddata(1)*B%cdata(1)
			case(35)
				R%zdata(1)=A%ddata(1)*B%zdata(1)
			
				
			case(41)
				R%cdata(1)=A%cdata(1)*B%idata(1)
			case(42)
				R%cdata(1)=A%cdata(1)*B%sdata(1)
			case(43)
				R%zdata(1)=A%cdata(1)*B%ddata(1)
			case(44)
				R%cdata(1)=A%cdata(1)*B%cdata(1)
			case(45)
				R%zdata(1)=A%cdata(1)*B%zdata(1)
				
			case(51)
				R%zdata(1)=A%zdata(1)*B%idata(1)
			case(52)
				R%zdata(1)=A%zdata(1)*B%sdata(1)
			case(53)
				R%zdata(1)=A%zdata(1)*B%ddata(1)
			case(54)
				R%zdata(1)=A%zdata(1)*B%cdata(1)
			case(55)
				R%zdata(1)=A%zdata(1)*B%zdata(1)
		end select
		return
	end subroutine
	
	subroutine product_NumNum_par(R,A,B,alpha,beta)!Res=alpha*A*B+beta*Res
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		class(*),intent(in)::alpha,beta
		integer::flag
		flag=10*A%classType+B%classType
		select case(flag)
			case(11)
				R%idata(1)=(iselect(alpha)*A%idata(1)*B%idata(1))+(iselect(beta)*R%idata(1))
			case(12)
				R%sdata(1)=(sselect(alpha)*A%idata(1)*B%sdata(1))+(sselect(beta)*R%sdata(1))
			case(13)
				R%ddata(1)=(dselect(alpha)*A%idata(1)*B%ddata(1))+(dselect(beta)*R%ddata(1))
			case(14)
				R%cdata(1)=(cselect(alpha)*A%idata(1)*B%cdata(1))+(cselect(beta)*R%cdata(1))
			case(15)
				R%zdata(1)=(zselect(alpha)*A%idata(1)*B%zdata(1))+(zselect(beta)*R%zdata(1))
				
			case(21)
				R%sdata(1)=(sselect(alpha)*A%sdata(1)*B%idata(1))+(sselect(beta)*R%sdata(1))
			case(22)
				R%sdata(1)=(sselect(alpha)*A%sdata(1)*B%sdata(1))+(sselect(beta)*R%sdata(1))
			case(23)
				R%ddata(1)=(dselect(alpha)*A%sdata(1)*B%ddata(1))+(dselect(beta)*R%ddata(1))
			case(24)
				R%cdata(1)=(cselect(alpha)*A%sdata(1)*B%cdata(1))+(cselect(beta)*R%cdata(1))
			case(25)
				R%zdata(1)=(zselect(alpha)*A%sdata(1)*B%zdata(1))+(zselect(beta)*R%zdata(1))
					
			case(31)
				R%ddata(1)=(dselect(alpha)*A%ddata(1)*B%idata(1))+(dselect(beta)*R%ddata(1))
			case(32)
				R%ddata(1)=(dselect(alpha)*A%ddata(1)*B%sdata(1))+(dselect(beta)*R%ddata(1))
			case(33)
				R%ddata(1)=(dselect(alpha)*A%ddata(1)*B%ddata(1))+(dselect(beta)*R%ddata(1))
			case(34)
				R%zdata(1)=(zselect(alpha)*A%ddata(1)*B%cdata(1))+(zselect(beta)*R%zdata(1))
			case(35)
				R%zdata(1)=(zselect(alpha)*A%ddata(1)*B%zdata(1))+(zselect(beta)*R%zdata(1))
			
				
			case(41)
				R%cdata(1)=(cselect(alpha)*A%cdata(1)*B%idata(1))+(cselect(beta)*R%cdata(1))
			case(42)
				R%cdata(1)=(cselect(alpha)*A%cdata(1)*B%sdata(1))+(cselect(beta)*R%cdata(1))
			case(43)
				R%zdata(1)=(zselect(alpha)*A%cdata(1)*B%ddata(1))+(zselect(beta)*R%zdata(1))
			case(44)
				R%cdata(1)=(cselect(alpha)*A%cdata(1)*B%cdata(1))+(cselect(beta)*R%cdata(1))
			case(45)
				R%zdata(1)=(zselect(alpha)*A%cdata(1)*B%zdata(1))+(zselect(beta)*R%zdata(1))
				
			case(51)
				R%zdata(1)=(zselect(alpha)*A%zdata(1)*B%idata(1))+(zselect(beta)*R%zdata(1))
			case(52)
				R%zdata(1)=(zselect(alpha)*A%zdata(1)*B%sdata(1))+(zselect(beta)*R%zdata(1))
			case(53)
				R%zdata(1)=(zselect(alpha)*A%zdata(1)*B%ddata(1))+(zselect(beta)*R%zdata(1))
			case(54)
				R%zdata(1)=(zselect(alpha)*A%zdata(1)*B%cdata(1))+(zselect(beta)*R%zdata(1))
			case(55)
				R%zdata(1)=(zselect(alpha)*A%zdata(1)*B%zdata(1))+(zselect(beta)*R%zdata(1))
		end select
		return
	end subroutine
	
	

!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  overwrite
!                          int real dble cmplx dcmplx aimag char
!
!**************************************************************************************************************
!**************************************************************************************************************
	subroutine intTdata(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		integer::lengA
		lengA=A%TotalData
		select case(A%classType)
			case(1)
				R%idata=A%idata(1:lengA)
			case(2)
				R%idata=A%sdata(1:lengA)
			case(3)
				R%idata=A%ddata(1:lengA)
			case(4)
				R%idata=A%cdata(1:lengA)
			case(5)
				R%idata=A%zdata(1:lengA)
		end select
		return
	end subroutine
	subroutine realTdata(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		integer::lengA
		lengA=A%TotalData
		select case(A%classType)
			case(1)
				R%sdata=A%idata(1:lengA)
			case(2)
				R%sdata=A%sdata(1:lengA)
			case(3)
				R%sdata=A%ddata(1:lengA)
			case(4)
				R%sdata=A%cdata(1:lengA)
			case(5)
				R%sdata=A%zdata(1:lengA)
		end select
		return
	end subroutine
	subroutine absTdata(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		integer::lengA
		lengA=A%TotalData
		select case(A%classType)
			case(1)
				R%sdata=iabs(A%idata(1:lengA))
			case(2)
				R%sdata=abs(A%sdata(1:lengA))	
			case(3)
				R%sdata=dabs(A%ddata(1:lengA))
			case(4)
				R%sdata=cabs(A%cdata(1:lengA))
			case(5)
				R%sdata=cdabs(A%zdata(1:lengA))
		end select
		return
	end subroutine
	subroutine dabsTdata(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		integer::lengA
		lengA=A%TotalData
		select case(A%classType)
			case(1)
				R%ddata=iabs(A%idata(1:lengA))
			case(2)
				R%ddata=abs(A%sdata(1:lengA))	
			case(3)
				R%ddata=dabs(A%ddata(1:lengA))
			case(4)
				R%ddata=cabs(A%cdata(1:lengA))
			case(5)
				R%ddata=cdabs(A%zdata(1:lengA))
		end select
		return
	end subroutine
	subroutine dbleTdata(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		integer::lengA
		lengA=A%TotalData
		select case(A%classType)
			case(1)
				R%ddata=A%idata(1:lengA)
			case(2)
				R%ddata=A%sdata(1:lengA)
			case(3)
				R%ddata=A%ddata(1:lengA)
			case(4)
				R%ddata=A%cdata(1:lengA)
			case(5)
				R%ddata=A%zdata(1:lengA)
		end select
		return
	end subroutine
	subroutine cmplxTdata(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		integer::lengA
		lengA=A%TotalData
		select case(A%classType)
			case(1)
				R%cdata=A%idata(1:lengA)
			case(2)
				R%cdata=A%sdata(1:lengA)	
			case(3)
				R%cdata=A%ddata(1:lengA)
			case(4)
				R%cdata=A%cdata(1:lengA)
			case(5)
				R%cdata=A%zdata(1:lengA)
		end select
		return
	end subroutine
	subroutine cmplxTdata2(R,A,B)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A,B
		integer::flag
		integer::lengA,lengB
		flag=10*A%classType+B%classType
		lengA=A%TotalData
		lengB=B%TotalData
		select case(flag)
			case(11)
				R%cdata=cmplx(A%idata(1:lengA),B%idata(1:lengB),kind=4)
			case(12)
				R%cdata=cmplx(A%idata(1:lengA),B%sdata(1:lengB),kind=4)	
			case(13)
				R%cdata=cmplx(A%idata(1:lengA),B%ddata(1:lengB),kind=4)
				
			case(21)
				R%cdata=cmplx(A%sdata(1:lengA),B%idata(1:lengB),kind=4)
			case(22)
				R%cdata=cmplx(A%sdata(1:lengA),B%sdata(1:lengB),kind=4)	
			case(23)
				R%cdata=cmplx(A%sdata(1:lengA),B%ddata(1:lengB),kind=4)
				
			case(31)
				R%cdata=cmplx(A%ddata(1:lengA),B%idata(1:lengB),kind=4)
			case(32)
				R%cdata=cmplx(A%ddata(1:lengA),B%sdata(1:lengB),kind=4)	
			case(33)
				R%cdata=cmplx(A%ddata(1:lengA),B%ddata(1:lengB),kind=4)
		end select
		return
	end subroutine
	subroutine dcmplxTdata(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
			integer::lengA
		lengA=A%TotalData
		select case(A%classType)
			case(1)
				R%zdata=A%idata(1:lengA)
			case(2)
				R%zdata=A%sdata(1:lengA)
			case(3)
				R%zdata=A%ddata(1:lengA)
			case(4)
				R%zdata=A%cdata(1:lengA)
			case(5)
				R%zdata=A%zdata(1:lengA)
		end select
		return
	end subroutine
	subroutine dcmplxTdata2(R,A,B)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A,B
		integer::flag
		integer::lengA,lengB
		lengA=A%TotalData
		lengB=B%TotalData
		flag=10*A%classType+B%classType
		select case(flag)
			case(11)
				R%zdata=dcmplx(A%idata(1:lengA),B%idata(1:lengB))
			case(12)
				R%zdata=dcmplx(A%idata(1:lengA),B%sdata(1:lengB))	
			case(13)
				R%zdata=dcmplx(A%idata(1:lengA),B%ddata(1:lengB))
				
			case(21)
				R%zdata=dcmplx(A%sdata(1:lengA),B%idata(1:lengB))
			case(22)
				R%zdata=dcmplx(A%sdata(1:lengA),B%sdata(1:lengB))	
			case(23)
				R%zdata=dcmplx(A%sdata(1:lengA),B%ddata(1:lengB))
				
			case(31)
				R%zdata=dcmplx(A%ddata(1:lengA),B%idata(1:lengB))
			case(32)
				R%zdata=dcmplx(A%ddata(1:lengA),B%sdata(1:lengB))	
			case(33)
				R%zdata=dcmplx(A%ddata(1:lengA),B%ddata(1:lengB))
		end select
		return
	end subroutine
	subroutine aimagTdata(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		integer::lengA
		lengA=A%TotalData
		select case(A%classType)
			case(1)
				R%sdata(1:R%totalData)=0
			case(2)
				R%sdata(1:R%totalData)=0
			case(3)
				R%sdata(1:R%totalData)=0
			case(4)
				R%sdata=aimag(A%cdata(1:lengA))
			case(5)
				R%sdata=aimag(A%zdata(1:lengA))
		end select
		return
	end subroutine
	subroutine dimagTdata(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
			integer::lengA
		lengA=A%TotalData
		select case(A%classType)
			case(1)
				R%ddata(1:R%totalData)=0
			case(2)
				R%ddata(1:R%totalData)=0
			case(3)
				R%ddata(1:R%totalData)=0
			case(4)
				R%ddata=aimag(A%cdata(1:lengA))
			case(5)
				R%ddata=aimag(A%zdata(1:lengA))
		end select
		return
	end subroutine
	subroutine charTdata(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		integer::lengA
		lengA=A%TotalData
		select case(A%classType)
			case(1)
				R%adata=A%idata(1:lengA)
			case(2)
				R%adata=A%sdata(1:lengA)
			case(3)
				R%adata=A%ddata(1:lengA)
			case(4)
				R%adata=A%cdata(1:lengA)
			case(5)
				R%adata=A%zdata(1:lengA)
			case(6)
				R%adata=A%ldata(1:lengA)
		end select
		return
	end subroutine
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  max or min element in TData
!
!**************************************************************************************************************
!**************************************************************************************************************	
			!maxminflag=
			!'maxa': max or max abs (complex data)
			!'mina': min or min abs (complex data)
			!'maxr': max real
			!'mina': min real
			!'maxi': 0(not com) or max imag
			!'mini': 0(not com) or min imag
	subroutine intmaxTData(outvalue,T)
		integer,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(T%idata(1:T%totalData))
			case (2)
				outvalue=maxval(T%sdata(1:T%totalData))
			case (3)
				outvalue=maxval(T%ddata(1:T%totalData))
			case (4)
				outvalue=maxval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine intminTData(outvalue,T)
		integer,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(T%idata(1:T%totalData))
			case (2)
				outvalue=minval(T%sdata(1:T%totalData))
			case (3)
				outvalue=minval(T%ddata(1:T%totalData))
			case (4)
				outvalue=minval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine intmaxrealTData(outvalue,T)
		integer,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(T%idata(1:T%totalData))
			case (2)
				outvalue=maxval(T%sdata(1:T%totalData))
			case (3)
				outvalue=maxval(T%ddata(1:T%totalData))
			case (4)
				outvalue=maxval(real(T%cdata(1:T%totalData),kind=4))
			case (5)
				outvalue=maxval(dble(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine intminrealTData(outvalue,T)
		integer,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(T%idata(1:T%totalData))
			case (2)
				outvalue=minval(T%sdata(1:T%totalData))
			case (3)
				outvalue=minval(T%ddata(1:T%totalData))
			case (4)
				outvalue=minval(real(T%cdata(1:T%totalData),kind=4))
			case (5)
				outvalue=minval(dble(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine intmaximagTData(outvalue,T)
		integer,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=0
			case (2)
				outvalue=0
			case (3)
				outvalue=0
			case (4)
				outvalue=maxval(aimag(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(dimag(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine intminimagTData(outvalue,T)
		integer,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=0
			case (2)
				outvalue=0
			case (3)
				outvalue=0
			case (4)
				outvalue=minval(aimag(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(dimag(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine intmaxabsTData(outvalue,T)
		integer,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(abs(T%idata(1:T%totalData)))
			case (2)
				outvalue=maxval(abs(T%sdata(1:T%totalData)))
			case (3)
				outvalue=maxval(dabs(T%ddata(1:T%totalData)))
			case (4)
				outvalue=maxval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine intminabsTData(outvalue,T)
		integer,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(abs(T%idata(1:T%totalData)))
			case (2)
				outvalue=minval(abs(T%sdata(1:T%totalData)))
			case (3)
				outvalue=minval(dabs(T%ddata(1:T%totalData)))
			case (4)
				outvalue=minval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	
	
	subroutine real4_maxTData(outvalue,T)
		real(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(T%idata(1:T%totalData))
			case (2)
				outvalue=maxval(T%sdata(1:T%totalData))
			case (3)
				outvalue=maxval(T%ddata(1:T%totalData))
			case (4)
				outvalue=maxval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real4_minTData(outvalue,T)
		real(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(T%idata(1:T%totalData))
			case (2)
				outvalue=minval(T%sdata(1:T%totalData))
			case (3)
				outvalue=minval(T%ddata(1:T%totalData))
			case (4)
				outvalue=minval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real4_maxrealTData(outvalue,T)
		real(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(T%idata(1:T%totalData))
			case (2)
				outvalue=maxval(T%sdata(1:T%totalData))
			case (3)
				outvalue=maxval(T%ddata(1:T%totalData))
			case (4)
				outvalue=maxval(real(T%cdata(1:T%totalData),kind=4))
			case (5)
				outvalue=maxval(dble(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real4_minrealTData(outvalue,T)
		real(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(T%idata(1:T%totalData))
			case (2)
				outvalue=minval(T%sdata(1:T%totalData))
			case (3)
				outvalue=minval(T%ddata(1:T%totalData))
			case (4)
				outvalue=minval(real(T%cdata(1:T%totalData),kind=4))
			case (5)
				outvalue=minval(dble(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real4_maximagTData(outvalue,T)
		real(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=0
			case (2)
				outvalue=0
			case (3)
				outvalue=0
			case (4)
				outvalue=maxval(aimag(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(dimag(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real4_minimagTData(outvalue,T)
		real(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=0
			case (2)
				outvalue=0
			case (3)
				outvalue=0
			case (4)
				outvalue=minval(aimag(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(dimag(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real4_maxabsTData(outvalue,T)
		real(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(abs(T%idata(1:T%totalData)))
			case (2)
				outvalue=maxval(abs(T%sdata(1:T%totalData)))
			case (3)
				outvalue=maxval(dabs(T%ddata(1:T%totalData)))
			case (4)
				outvalue=maxval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real4_minabsTData(outvalue,T)
		real(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(abs(T%idata(1:T%totalData)))
			case (2)
				outvalue=minval(abs(T%sdata(1:T%totalData)))
			case (3)
				outvalue=minval(dabs(T%ddata(1:T%totalData)))
			case (4)
				outvalue=minval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	
	
	subroutine real8_maxTData(outvalue,T)
		real(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(T%idata(1:T%totalData))
			case (2)
				outvalue=maxval(T%sdata(1:T%totalData))
			case (3)
				outvalue=maxval(T%ddata(1:T%totalData))
			case (4)
				outvalue=maxval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real8_minTData(outvalue,T)
		real(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(T%idata(1:T%totalData))
			case (2)
				outvalue=minval(T%sdata(1:T%totalData))
			case (3)
				outvalue=minval(T%ddata(1:T%totalData))
			case (4)
				outvalue=minval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real8_maxrealTData(outvalue,T)
		real(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(T%idata(1:T%totalData))
			case (2)
				outvalue=maxval(T%sdata(1:T%totalData))
			case (3)
				outvalue=maxval(T%ddata(1:T%totalData))
			case (4)
				outvalue=maxval(real(T%cdata(1:T%totalData),kind=4))
			case (5)
				outvalue=maxval(dble(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real8_minrealTData(outvalue,T)
		real(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(T%idata(1:T%totalData))
			case (2)
				outvalue=minval(T%sdata(1:T%totalData))
			case (3)
				outvalue=minval(T%ddata(1:T%totalData))
			case (4)
				outvalue=minval(real(T%cdata(1:T%totalData),kind=4))
			case (5)
				outvalue=minval(dble(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real8_maximagTData(outvalue,T)
		real(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=0
			case (2)
				outvalue=0
			case (3)
				outvalue=0
			case (4)
				outvalue=maxval(aimag(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(dimag(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real8_minimagTData(outvalue,T)
		real(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=0
			case (2)
				outvalue=0
			case (3)
				outvalue=0
			case (4)
				outvalue=minval(aimag(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(dimag(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real8_maxabsTData(outvalue,T)
		real(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(abs(T%idata(1:T%totalData)))
			case (2)
				outvalue=maxval(abs(T%sdata(1:T%totalData)))
			case (3)
				outvalue=maxval(dabs(T%ddata(1:T%totalData)))
			case (4)
				outvalue=maxval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine real8_minabsTData(outvalue,T)
		real(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(abs(T%idata(1:T%totalData)))
			case (2)
				outvalue=minval(abs(T%sdata(1:T%totalData)))
			case (3)
				outvalue=minval(dabs(T%ddata(1:T%totalData)))
			case (4)
				outvalue=minval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	
	
	subroutine com4_maxTData(outvalue,T)
		complex(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(T%idata(1:T%totalData))
			case (2)
				outvalue=maxval(T%sdata(1:T%totalData))
			case (3)
				outvalue=maxval(T%ddata(1:T%totalData))
			case (4)
				outvalue=maxval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com4_minTData(outvalue,T)
		complex(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(T%idata(1:T%totalData))
			case (2)
				outvalue=minval(T%sdata(1:T%totalData))
			case (3)
				outvalue=minval(T%ddata(1:T%totalData))
			case (4)
				outvalue=minval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com4_maxrealTData(outvalue,T)
		complex(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(T%idata(1:T%totalData))
			case (2)
				outvalue=maxval(T%sdata(1:T%totalData))
			case (3)
				outvalue=maxval(T%ddata(1:T%totalData))
			case (4)
				outvalue=maxval(real(T%cdata(1:T%totalData),kind=4))
			case (5)
				outvalue=maxval(dble(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com4_minrealTData(outvalue,T)
		complex(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(T%idata(1:T%totalData))
			case (2)
				outvalue=minval(T%sdata(1:T%totalData))
			case (3)
				outvalue=minval(T%ddata(1:T%totalData))
			case (4)
				outvalue=minval(real(T%cdata(1:T%totalData),kind=4))
			case (5)
				outvalue=minval(dble(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com4_maximagTData(outvalue,T)
		complex(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=0
			case (2)
				outvalue=0
			case (3)
				outvalue=0
			case (4)
				outvalue=maxval(aimag(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(dimag(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com4_minimagTData(outvalue,T)
		complex(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=0
			case (2)
				outvalue=0
			case (3)
				outvalue=0
			case (4)
				outvalue=minval(aimag(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(dimag(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com4_maxabsTData(outvalue,T)
		complex(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(abs(T%idata(1:T%totalData)))
			case (2)
				outvalue=maxval(abs(T%sdata(1:T%totalData)))
			case (3)
				outvalue=maxval(dabs(T%ddata(1:T%totalData)))
			case (4)
				outvalue=maxval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com4_minabsTData(outvalue,T)
		complex(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(abs(T%idata(1:T%totalData)))
			case (2)
				outvalue=minval(abs(T%sdata(1:T%totalData)))
			case (3)
				outvalue=minval(dabs(T%ddata(1:T%totalData)))
			case (4)
				outvalue=minval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	
	
	subroutine com8_maxTData(outvalue,T)
		complex(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(T%idata(1:T%totalData))
			case (2)
				outvalue=maxval(T%sdata(1:T%totalData))
			case (3)
				outvalue=maxval(T%ddata(1:T%totalData))
			case (4)
				outvalue=maxval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com8_minTData(outvalue,T)
		complex(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(T%idata(1:T%totalData))
			case (2)
				outvalue=minval(T%sdata(1:T%totalData))
			case (3)
				outvalue=minval(T%ddata(1:T%totalData))
			case (4)
				outvalue=minval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com8_maxrealTData(outvalue,T)
		complex(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(T%idata(1:T%totalData))
			case (2)
				outvalue=maxval(T%sdata(1:T%totalData))
			case (3)
				outvalue=maxval(T%ddata(1:T%totalData))
			case (4)
				outvalue=maxval(real(T%cdata(1:T%totalData),kind=4))
			case (5)
				outvalue=maxval(dble(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com8_minrealTData(outvalue,T)
		complex(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(T%idata(1:T%totalData))
			case (2)
				outvalue=minval(T%sdata(1:T%totalData))
			case (3)
				outvalue=minval(T%ddata(1:T%totalData))
			case (4)
				outvalue=minval(real(T%cdata(1:T%totalData),kind=4))
			case (5)
				outvalue=minval(dble(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com8_maximagTData(outvalue,T)
		complex(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=0
			case (2)
				outvalue=0
			case (3)
				outvalue=0
			case (4)
				outvalue=maxval(aimag(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(dimag(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com8_minimagTData(outvalue,T)
		complex(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=0
			case (2)
				outvalue=0
			case (3)
				outvalue=0
			case (4)
				outvalue=minval(aimag(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(dimag(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com8_maxabsTData(outvalue,T)
		complex(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=maxval(abs(T%idata(1:T%totalData)))
			case (2)
				outvalue=maxval(abs(T%sdata(1:T%totalData)))
			case (3)
				outvalue=maxval(dabs(T%ddata(1:T%totalData)))
			case (4)
				outvalue=maxval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=maxval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	subroutine com8_minabsTData(outvalue,T)
		complex(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=minval(abs(T%idata(1:T%totalData)))
			case (2)
				outvalue=minval(abs(T%sdata(1:T%totalData)))
			case (3)
				outvalue=minval(dabs(T%ddata(1:T%totalData)))
			case (4)
				outvalue=minval(abs(T%cdata(1:T%totalData)))
			case (5)
				outvalue=minval(cdabs(T%zdata(1:T%totalData)))
		end select
		return
	end subroutine
	
	
	subroutine sum_TDatai(outvalue,T)
		integer,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=sum(T%idata(1:T%totalData))
			case (2)
				outvalue=sum(T%sdata(1:T%totalData))
			case (3)
				outvalue=sum(T%ddata(1:T%totalData))
			case (4)
				outvalue=sum(T%cdata(1:T%totalData))
			case (5)
				outvalue=sum(T%zdata(1:T%totalData))
		end select
		return
	end subroutine
	subroutine sum_TDatas(outvalue,T)
		real*4,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=sum(T%idata(1:T%totalData))
			case (2)
				outvalue=sum(T%sdata(1:T%totalData))
			case (3)
				outvalue=sum(T%ddata(1:T%totalData))
			case (4)
				outvalue=sum(T%cdata(1:T%totalData))
			case (5)
				outvalue=sum(T%zdata(1:T%totalData))
		end select
		return
	end subroutine
	subroutine sum_TDatad(outvalue,T)
		real*8,intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=sum(T%idata(1:T%totalData))
			case (2)
				outvalue=sum(T%sdata(1:T%totalData))
			case (3)
				outvalue=sum(T%ddata(1:T%totalData))
			case (4)
				outvalue=sum(T%cdata(1:T%totalData))
			case (5)
				outvalue=sum(T%zdata(1:T%totalData))
		end select
		return
	end subroutine
	subroutine sum_TDatac(outvalue,T)
		complex(kind=4),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=sum(T%idata(1:T%totalData))
			case (2)
				outvalue=sum(T%sdata(1:T%totalData))
			case (3)
				outvalue=sum(T%ddata(1:T%totalData))
			case (4)
				outvalue=sum(T%cdata(1:T%totalData))
			case (5)
				outvalue=sum(T%zdata(1:T%totalData))
		end select
		return
	end subroutine
	subroutine sum_TDataz(outvalue,T)
		complex(kind=8),intent(inout)::outvalue
		type(TData),intent(in)::T
		select case(T%ClassType)
			case (1)
				outvalue=sum(T%idata(1:T%totalData))
			case (2)
				outvalue=sum(T%sdata(1:T%totalData))
			case (3)
				outvalue=sum(T%ddata(1:T%totalData))
			case (4)
				outvalue=sum(T%cdata(1:T%totalData))
			case (5)
				outvalue=sum(T%zdata(1:T%totalData))
		end select
		return
	end subroutine
	
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  permutation
!

!permutation_data2*,permutation_data3* will not change the dimension input
!permutefo_data* and permuteback_data* will change
!**************************************************************************************************************
!**************************************************************************************************************	
	subroutine permutation_data3_inout(Res,index_not_permute,dimens)
		integer,intent(in) ::   index_not_permute
		type(TData),intent(inout) :: Res
		type(Dimension),intent(inout) ::dimens
		integer,pointer::idata(:)
		real(kind=4),pointer::sdata(:)
		real(kind=8),pointer::ddata(:)
		complex(kind=4),pointer::cdata(:)
		complex(kind=8),pointer::zdata(:)
		logical,pointer::ldata(:)
		character(len=max_len_of_char_in_TData),pointer::adata(:)
		integer::dimen(3),lenD,classtype
		call WorkingMemory%check()
		dimen=dimens
		if(index_not_permute.eq.1) then
			lenD=dimen(1)
			select case (Res%classType)
				case (1)
					!allocate(idata(Res%TotalData))
					call WorkingMemory%get_memory(idata,Res%TotalData)
					idata=Res%idata
					call permutation_rank3_data1_int(Res%idata,idata,dimen(1),dimen(2),dimen(3),lenD)
				case (2)
					!allocate(sdata(Res%TotalData))
					call WorkingMemory%get_memory(sdata,Res%TotalData)
					call scopy(Res%TotalData,Res%sdata,1,sdata,1)
					call permutation_rank3_data1_real4(Res%sdata,sdata,dimen(1),dimen(2),dimen(3),lenD)
				case (3)
					!allocate(ddata(Res%TotalData))
					call WorkingMemory%get_memory(ddata,Res%TotalData)
					call dcopy(Res%TotalData,Res%ddata,1,ddata,1)
					call permutation_rank3_data1_real8(Res%ddata,ddata,dimen(1),dimen(2),dimen(3),lenD)
				case (4)
					!allocate(cdata(Res%TotalData))
					call WorkingMemory%get_memory(cdata,Res%TotalData)
					call ccopy(Res%TotalData,Res%cdata,1,cdata,1)
					call permutation_rank3_data1_com4(Res%cdata,cdata,dimen(1),dimen(2),dimen(3),lenD)
				case (5)
					!allocate(zdata(Res%TotalData))
					call WorkingMemory%get_memory(zdata,Res%TotalData)
					call zcopy(Res%TotalData,Res%zdata,1,zdata,1)
					call permutation_rank3_data1_com8(Res%zdata,zdata,dimen(1),dimen(2),dimen(3),lenD)
				case (6)
					!allocate(ldata(Res%TotalData))
					call WorkingMemory%get_memory(ldata,Res%TotalData)
					ldata=Res%ldata
					call permutation_rank3_data1_logi(Res%ldata,ldata,dimen(1),dimen(2),dimen(3),lenD)
				case (7)
					!allocate(adata(Res%TotalData))
					call WorkingMemory%get_memory(adata,Res%TotalData)
					adata=Res%adata
					call permutation_rank3_data1_char(Res%adata,adata,dimen(1),dimen(2),dimen(3),lenD)
			end select
			call WorkingMemory%free()
			return
		end if
		if(index_not_permute.eq.2) then
			lenD=dimen(1)*dimen(3)
			select case (Res%classType)
				case (1)
					!allocate(idata(Res%TotalData))
					call WorkingMemory%get_memory(idata,Res%TotalData)
					idata=Res%idata
					call permutation_rank3_data2_int(Res%idata,idata,dimen(1),dimen(2),dimen(3),lenD)
				case (2)
					!allocate(sdata(Res%TotalData))
					call WorkingMemory%get_memory(sdata,Res%TotalData)
					call scopy(Res%TotalData,Res%sdata,1,sdata,1)
					call permutation_rank3_data2_real4(Res%sdata,sdata,dimen(1),dimen(2),dimen(3),lenD)
				case (3)
					!allocate(ddata(Res%TotalData))
					call WorkingMemory%get_memory(ddata,Res%TotalData)
					call dcopy(Res%TotalData,Res%ddata,1,ddata,1)
					call permutation_rank3_data2_real8(Res%ddata,ddata,dimen(1),dimen(2),dimen(3),lenD)
				case (4)
					!allocate(cdata(Res%TotalData))
					call WorkingMemory%get_memory(cdata,Res%TotalData)
					call ccopy(Res%TotalData,Res%cdata,1,cdata,1)
					call permutation_rank3_data2_com4(Res%cdata,cdata,dimen(1),dimen(2),dimen(3),lenD)
				case (5)
					!allocate(zdata(Res%TotalData))
					call WorkingMemory%get_memory(zdata,Res%TotalData)
					call zcopy(Res%TotalData,Res%zdata,1,zdata,1)
					call permutation_rank3_data2_com8(Res%zdata,zdata,dimen(1),dimen(2),dimen(3),lenD)
				case (6)
					!allocate(ldata(Res%TotalData))
					call WorkingMemory%get_memory(ldata,Res%TotalData)
					ldata=Res%ldata
					call permutation_rank3_data2_logi(Res%ldata,ldata,dimen(1),dimen(2),dimen(3),lenD)
				case (7)
					!allocate(adata(Res%TotalData))
					call WorkingMemory%get_memory(adata,Res%TotalData)
					adata=Res%adata
					call permutation_rank3_data2_char(Res%adata,adata,dimen(1),dimen(2),dimen(3),lenD)
			end select
			call WorkingMemory%free()
			return
		end if
		if(index_not_permute.eq.3) then
			lenD=dimen(1)*dimen(2)
			select case (Res%classType)
				case (1)
					!allocate(idata(Res%TotalData))
					call WorkingMemory%get_memory(idata,Res%TotalData)
					idata=Res%idata
					call permutation_rank3_data3_int(Res%idata,idata,dimen(1),dimen(2),dimen(3),lenD)
				case (2)
					!allocate(sdata(Res%TotalData))
					call WorkingMemory%get_memory(sdata,Res%TotalData)
					call scopy(Res%TotalData,Res%sdata,1,sdata,1)
					call permutation_rank3_data3_real4(Res%sdata,sdata,dimen(1),dimen(2),dimen(3),lenD)
				case (3)
					!allocate(ddata(Res%TotalData))
					call WorkingMemory%get_memory(ddata,Res%TotalData)
					call dcopy(Res%TotalData,Res%ddata,1,ddata,1)
					call permutation_rank3_data3_real8(Res%ddata,ddata,dimen(1),dimen(2),dimen(3),lenD)
				case (4)
					!allocate(cdata(Res%TotalData))
					call WorkingMemory%get_memory(cdata,Res%TotalData)
					call ccopy(Res%TotalData,Res%cdata,1,cdata,1)
					call permutation_rank3_data3_com4(Res%cdata,cdata,dimen(1),dimen(2),dimen(3),lenD)
				case (5)
					!allocate(zdata(Res%TotalData))
					call WorkingMemory%get_memory(zdata,Res%TotalData)
					call zcopy(Res%TotalData,Res%zdata,1,zdata,1)
					call permutation_rank3_data3_com8(Res%zdata,zdata,dimen(1),dimen(2),dimen(3),lenD)
				case (6)
					!allocate(ldata(Res%TotalData))
					call WorkingMemory%get_memory(ldata,Res%TotalData)
					ldata=Res%ldata
					call permutation_rank3_data3_logi(Res%ldata,ldata,dimen(1),dimen(2),dimen(3),lenD)
				case (7)
					!allocate(adata(Res%TotalData))
					call WorkingMemory%get_memory(adata,Res%TotalData)
					adata=Res%adata
					call permutation_rank3_data3_char(Res%adata,adata,dimen(1),dimen(2),dimen(3),lenD)
			end select
			call WorkingMemory%free()
			return
		end if
	 end subroutine
	 
	 subroutine permutation_data3(Res,T,index_not_permute,dimens)
		integer,intent(in) ::   index_not_permute
		type(TData),intent(inout) :: Res
		type(TData),intent(in) :: T
		type(Dimension),intent(inout) ::dimens
		integer::dimen(3),lenD,classtype
		dimen=dimens
		if(index_not_permute.eq.1) then
			lenD=dimen(1)
			select case (T%classType)
				case (1)
					call permutation_rank3_data1_int(Res%idata,T%idata,dimen(1),dimen(2),dimen(3),lenD)
				case (2)
					call permutation_rank3_data1_real4(Res%sdata,T%sdata,dimen(1),dimen(2),dimen(3),lenD)
				case (3)
					call permutation_rank3_data1_real8(Res%ddata,T%ddata,dimen(1),dimen(2),dimen(3),lenD)
				case (4)
					call permutation_rank3_data1_com4(Res%cdata,T%cdata,dimen(1),dimen(2),dimen(3),lenD)
				case (5)
					call permutation_rank3_data1_com8(Res%zdata,T%zdata,dimen(1),dimen(2),dimen(3),lenD)
				case (6)
					call permutation_rank3_data1_logi(Res%ldata,T%ldata,dimen(1),dimen(2),dimen(3),lenD)
				case (7)
					call permutation_rank3_data1_char(Res%adata,T%adata,dimen(1),dimen(2),dimen(3),lenD)
			end select
			!dimens=Dimpermute(dimens,(/1,3,2/))
			return
		end if
		if(index_not_permute.eq.2) then
			lenD=dimen(1)
			select case (T%classType)
				case (1)
					call permutation_rank3_data2_int(Res%idata,T%idata,dimen(1),dimen(2),dimen(3),lenD)
				case (2)
					call permutation_rank3_data2_real4(Res%sdata,T%sdata,dimen(1),dimen(2),dimen(3),lenD)
				case (3)
					call permutation_rank3_data2_real8(Res%ddata,T%ddata,dimen(1),dimen(2),dimen(3),lenD)
				case (4)
					call permutation_rank3_data2_com4(Res%cdata,T%cdata,dimen(1),dimen(2),dimen(3),lenD)
				case (5)
					call permutation_rank3_data2_com8(Res%zdata,T%zdata,dimen(1),dimen(2),dimen(3),lenD)
				case (6)
					call permutation_rank3_data2_logi(Res%ldata,T%ldata,dimen(1),dimen(2),dimen(3),lenD)
				case (7)
					call permutation_rank3_data2_char(Res%adata,T%adata,dimen(1),dimen(2),dimen(3),lenD)
			end select
			!dimens=Dimpermute(dimens,(/3,2,1/))
			return
		end if
		if(index_not_permute.eq.3) then
			lenD=dimen(1)
			select case (T%classType)
				case (1)
					call permutation_rank3_data3_int(Res%idata,T%idata,dimen(1),dimen(2),dimen(3),lenD)
				case (2)
					call permutation_rank3_data3_real4(Res%sdata,T%sdata,dimen(1),dimen(2),dimen(3),lenD)
				case (3)
					call permutation_rank3_data3_real8(Res%ddata,T%ddata,dimen(1),dimen(2),dimen(3),lenD)
				case (4)
					call permutation_rank3_data3_com4(Res%cdata,T%cdata,dimen(1),dimen(2),dimen(3),lenD)
				case (5)
					call permutation_rank3_data3_com8(Res%zdata,T%zdata,dimen(1),dimen(2),dimen(3),lenD)
				case (6)
					call permutation_rank3_data3_logi(Res%ldata,T%ldata,dimen(1),dimen(2),dimen(3),lenD)
				case (7)
					call permutation_rank3_data3_char(Res%adata,T%adata,dimen(1),dimen(2),dimen(3),lenD)
			end select
			!dimens=Dimpermute(dimens,(/2,1,3/))
			return
		end if
	 end subroutine

	subroutine permutation_data2_inout(Res,dimens)
		type(TData),intent(inout) :: Res
		type(Dimension),intent(inout) ::dimens
		integer::dimen(2)
		integer,pointer::idata(:)
		real(kind=4),pointer::sdata(:)
		real(kind=8),pointer::ddata(:)
		complex(kind=4),pointer::cdata(:)
		complex(kind=8),pointer::zdata(:)
		logical,pointer::ldata(:)
		character(len=max_len_of_char_in_TData),pointer::adata(:)
		if(dimens%Getrank().eq.1) then
			return
		end if
		call WorkingMemory%check()
		dimen=dimens
		select case (Res%classType)
			case (1)
				!allocate(idata(Res%TotalData))
				call WorkingMemory%get_memory(idata,Res%TotalData)
				idata=Res%idata
				call permutation_rank2_data_int(Res%idata,iData,dimen(1),dimen(2))
			case (2)
				!allocate(sdata(Res%TotalData))
				call WorkingMemory%get_memory(sdata,Res%TotalData)
				call scopy(Res%TotalData,Res%sdata,1,sdata,1)
				call permutation_rank2_data_real4(Res%sdata,sData,dimen(1),dimen(2))
			case (3)
				!allocate(ddata(Res%TotalData))
				call WorkingMemory%get_memory(ddata,Res%TotalData)
				call dcopy(Res%TotalData,Res%ddata,1,ddata,1)
				call permutation_rank2_data_real8(Res%ddata,dData,dimen(1),dimen(2))
			case (4)
				!allocate(cdata(Res%TotalData))
				call WorkingMemory%get_memory(cdata,Res%TotalData)
				call ccopy(Res%TotalData,Res%cdata,1,cdata,1)
				call permutation_rank2_data_com4(Res%cdata,cData,dimen(1),dimen(2))
			case (5)
				!allocate(zdata(Res%TotalData))
				call WorkingMemory%get_memory(zdata,Res%TotalData)
				call zcopy(Res%TotalData,Res%zdata,1,zdata,1)
				call permutation_rank2_data_com8(Res%zdata,zData,dimen(1),dimen(2))
			case (6)
				!allocate(ldata(Res%TotalData))
				call WorkingMemory%get_memory(ldata,Res%TotalData)
				ldata=Res%ldata
				call permutation_rank2_data_logi(Res%ldata,lData,dimen(1),dimen(2))
			case (7)
				!allocate(idata(Res%TotalData))
				call WorkingMemory%get_memory(adata,Res%TotalData)
				adata=Res%adata
				call permutation_rank2_data_char(Res%adata,aData,dimen(1),dimen(2))
		end select
		call WorkingMemory%free()
		return
	end subroutine
	subroutine permutation_data2(Res,T,dimens)
		type(TData),intent(inout) :: Res
		type(TData),intent(in) :: T
		type(Dimension),intent(inout) ::dimens
		integer::dimen(2)
		dimen=dimens
		select case (T%classType)
			case (1)
				call permutation_rank2_data_int(Res%idata,T%iData,dimen(1),dimen(2))
			case (2)
				call permutation_rank2_data_real4(Res%sdata,T%sData,dimen(1),dimen(2))
			case (3)
				call permutation_rank2_data_real8(Res%ddata,T%dData,dimen(1),dimen(2))
			case (4)
				call permutation_rank2_data_com4(Res%cdata,T%cData,dimen(1),dimen(2))
			case (5)
				call permutation_rank2_data_com8(Res%zdata,T%zData,dimen(1),dimen(2))
			case (6)
				call permutation_rank2_data_logi(Res%ldata,T%lData,dimen(1),dimen(2))
			case (7)
				call permutation_rank2_data_char(Res%adata,T%aData,dimen(1),dimen(2))
		end select
		!dimens=Dimpermute(dimens,(/2,1/))
		return
	end subroutine

	subroutine permutefo_data_inout(Res,inde,dimen)
		type(TData),intent(inout) :: Res
		type(Dimension),intent(inout) ::dimen
		type(Dimension)::newdim
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
		call Dimpermute_forwards(newdim,dimen,inde)
		if(inde.eq.rank) then
			dimen=dimen%fuseIndex(1,rank-2)
			call permutation_data2_inout(Res,dimen)
			dimen=newdim
			return
		end if
		num=inde-2
		dimen=dimen%fuseIndex(1,num)
		num=rank-3
		dimen=dimen%fuseIndex(3,num)
		call permutation_data3_inout(Res,3,dimen)
		dimen=newdim
!		dimen=DimDecomposeAll(dimen)
		return
	end subroutine
	subroutine permutefo_data(Res,T,inde,dimen,indimen)
		type(TData),intent(inout) :: Res
		type(TData),intent(in) :: T
		type(Dimension),intent(inout) ::dimen
		type(Dimension),intent(in) ::indimen
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
			dimen=indimen
			return
		end if
		if(inde.eq.rank) then
			dimen=dimen%fuseIndex(1,rank-2)
			call permutation_data2(Res,T,dimen)
			call Dimpermute_forwards(dimen,indimen,inde)
!			dimen=DimDecomposeAll(dimen)
			return
		end if
		num=inde-2
		dimen=dimen%fuseIndex(1,num)
		num=rank-3
		dimen=dimen%fuseIndex(3,num)
		call permutation_data3(Res,T,3,dimen)
		call Dimpermute_forwards(dimen,indimen,inde)
!		dimen=DimDecomposeAll(dimen)
		return
	end subroutine
	subroutine permuteback_data_inout(Res,inde,dimen)
		type(TData),intent(inout) :: Res
		type(Dimension),intent(inout) ::dimen
		integer,intent(in)::inde
		type(Dimension)::newdim
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
		call Dimpermute_backwards(newdim,dimen,inde)
		if(inde.eq.1) then
			dimen=dimen%fuseIndex(2,rank-2)
			call permutation_data2_inout(Res,dimen)
			dimen=newdim
			return
		end if
		num=inde-2
		dimen=dimen%fuseIndex(1,num)
		num=rank-3
		dimen=dimen%fuseIndex(3,num)
		call permutation_data3_inout(Res,1,dimen)
		dimen=newdim
		return
	end subroutine
	subroutine permuteback_data(Res,T,inde,dimen,indimen)
		type(TData),intent(inout) :: Res
		type(TData),intent(in) :: T
		type(Dimension),intent(inout) ::dimen
		type(Dimension),intent(in) ::indimen
		integer,intent(in)::inde
		integer::rank,num
		integer::oper(2,3)
		rank=dimen%Getrank()
		if(inde.eq.rank) then
			dimen=indimen
			return
		end if
		if(inde.le.0) then
			write(*,*)"ERROR in function permuteback_data"
			write(*,*)"index=",inde
			call error_stop()
		end if
		if(inde.eq.1) then
			dimen=dimen%fuseIndex(2,rank-2)
			call permutation_data2(Res,T,dimen)
			call Dimpermute_backwards(dimen,indimen,inde)
			return
		end if
		num=inde-2
		dimen=dimen%fuseIndex(1,num)
		num=rank-3
		dimen=dimen%fuseIndex(3,num)
		call permutation_data3(Res,T,1,dimen)
		call Dimpermute_backwards(dimen,indimen,inde)
		return
	end subroutine
!**************************************************************************************************************
!**************************************************************************************************************
!
!                                  useful function
!
!**************************************************************************************************************
!**************************************************************************************************************	
	subroutine sortTData1(inoutData,outindices,increase,realpart)
		type(TData),intent(inout)::inoutData,outindices
		logical,intent(in)::increase,realpart
		select case(inoutData%classType)
			case(1)
				call sortData(inoutData%idata,outindices%idata,increase)
			case(2)
				call sortData(inoutData%sdata,outindices%idata,increase)
			case(3)
				call sortData(inoutData%ddata,outindices%idata,increase)
			case(4)
				call sortData(inoutData%cdata,outindices%idata,realpart,increase)
			case(5)
				call sortData(inoutData%zdata,outindices%idata,realpart,increase)
			case default
				call writemess('ERROR type in sort Tensor',-1)
				call error_stop
		end 	select
		return
	end subroutine
	subroutine sortTData2(inoutData,increase,realpart)
		type(TData),intent(inout)::inoutData
		logical,intent(in)::increase,realpart
		select case(inoutData%classType)
			case(1)
				call sortData(inoutData%idata,increase)
			case(2)
				call sortData(inoutData%sdata,increase)
			case(3)
				call sortData(inoutData%ddata,increase)
			case(4)
				call sortData(inoutData%cdata,realpart,increase)
			case(5)
				call sortData(inoutData%zdata,realpart,increase)
			case default
				call writemess('ERROR type in sort Tensor',-1)
				call error_stop
		end 	select
		return
	end subroutine
			

	logical function isnanTData(A)
		class(TData),intent(in)::A
		integer::i,totaldata
		totalData=A%totalData
		if(totalData.eq.0)then
			call writemess(" There is no data in Tensor, when checking if there is element is NAN",-1)
			call error_stop
		end if
		isnanTData=.false.
		select case(A%classType)
			case(1)
				do i=1,totalData
					if(isnan(real(A%idata(i))) )then
						isnanTData=.true.
						return
					end if
				end do
			case(2)
				do i=1,totalData
					if(isnan(A%sdata(i)) )then
						isnanTData=.true.
						return
					end if
				end do
			case(3)
				do i=1,totalData
					if(isnan(A%ddata(i)) )then
						isnanTData=.true.
						return
					end if
				end do
			case(4)
				do i=1,totalData
					if(isnan(real(A%cdata(i),kind=4)).or.isnan(aimag(A%cdata(i))) )then
						isnanTData=.true.
						return
					end if
				end do
			case(5)
				do i=1,totalData
					if(isnan(dble(A%zdata(i))).or.isnan(dimag(A%zdata(i))) )then
						isnanTData=.true.
						return
					end if
				end do
			case default
				call writemess(" ERROR in isnan",-1)
				call error_stop
		end select
		return
	end function
	
	
	logical function isOverflowTData(A)
		class(TData),intent(in)::A
		integer::i,totaldata
		totalData=A%totalData
		if(totalData.eq.0)then
			call writemess(" There is no data in Tensor, when checking if there is element Overflow",-1)
			call error_stop
		end if
		isOverflowTData=.false.
		select case(A%classType)
			case(1)
				do i=1,totalData
					if(abs(A%idata(i)).gt.default_max_integer_number )then
						isOverflowTData=.true.
						return
					end if
				end do
			case(2)
				do i=1,totalData
					if(abs(A%sdata(i)).gt.default_max_real_number)then
						isOverflowTData=.true.
						return
					end if
				end do
			case(3)
				do i=1,totalData
					if(dabs(A%ddata(i)).gt.default_max_double_number )then
						isOverflowTData=.true.
						return
					end if
				end do
			case(4)
				do i=1,totalData
					if((abs(real(A%cdata(i),kind=4)).gt.default_max_real_number).or.(abs(aimag(A%cdata(i)))&
												.gt.default_max_real_number ) )then
						isOverflowTData=.true.
						return
					end if
				end do
			case(5)
				do i=1,totalData
					if((dabs(dble(A%zdata(i))).gt.default_max_double_number ).or.(dabs(dimag(A%zdata(i))).gt.&
									default_max_double_number) )then
						isOverflowTData=.true.
						return
					end if
				end do
			case default
				call writemess(" ERROR in isOverflow,Tensor is not number type",-1)
				call error_stop
		end select
		return
	end function
	
	logical function isZeroTData(A)
		class(TData),intent(in)::A
		integer::i,totaldata
		totalData=A%totalData
		if(totalData.eq.0)then
			call writemess(" There is no data in Tensor, when checking if all the element is 0",-1)
			call error_stop
		end if
		isZeroTData=.true.
		select case(A%classType)
			case(1)
				do i=1,totalData
					if(A%idata(i).ne.0 )then
						isZeroTData=.false.
						return
					end if
				end do
			case(2)
				do i=1,totalData
					if(abs(A%sdata(i)).ge.default_zero_real_number)then
						isZeroTData=.false.
						return
					end if
				end do
			case(3)
				do i=1,totalData
					if(dabs(A%ddata(i)).ge.default_zero_double_number )then
						isZeroTData=.false.
						return
					end if
				end do
			case(4)
				do i=1,totalData
					if((abs(real(A%cdata(i),kind=4)).ge.default_zero_real_number).or.(abs(aimag(A%cdata(i)))&
												.ge.default_zero_real_number ) )then
						isZeroTData=.false.
						return
					end if
				end do
			case(5)
				do i=1,totalData
					if((dabs(dble(A%zdata(i))).ge.default_zero_double_number ).or.(dabs(dimag(A%zdata(i))).ge.&
									default_zero_double_number) )then
						isZeroTData=.false.
						return
					end if
				end do
			case default
				call writemess(" ERROR in isZeroTData,Tensor is not number type(integer, real or complex)",-1)
				call error_stop
		end select
		return
	end function

! dot product conjugating the first vector,The Tensor will be regard as a vector
	subroutine product_dotc_int(R,A,B)	
		integer,intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotc
		complex(kind=8),External::	zdotc
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R=cdotc(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R=zdotc(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R=cdotc(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R=zdotc(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R=zdotc(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R=zdotc(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R=cdotc(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R=cdotc(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R=zdotc(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R=cdotc(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R=zdotc(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R=zdotc(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R=zdotc(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R=zdotc(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R=zdotc(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R=zdotc(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine
	subroutine product_dotc_real4(R,A,B)	
		real(kind=4),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotc
		complex(kind=8),External::	zdotc
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R=cdotc(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R=zdotc(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R=cdotc(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R=zdotc(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R=zdotc(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R=zdotc(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R=cdotc(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R=cdotc(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R=zdotc(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R=cdotc(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R=zdotc(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R=zdotc(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R=zdotc(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R=zdotc(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R=zdotc(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R=zdotc(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine
	subroutine product_dotc_real8(R,A,B)	
		real(kind=8),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotc
		complex(kind=8),External::	zdotc
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R=cdotc(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R=zdotc(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R=cdotc(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R=zdotc(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R=zdotc(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R=zdotc(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R=cdotc(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R=cdotc(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R=zdotc(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R=cdotc(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R=zdotc(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R=zdotc(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R=zdotc(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R=zdotc(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R=zdotc(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R=zdotc(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine
	subroutine product_dotc_com4(R,A,B)	
		complex(kind=4),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotc
		complex(kind=8),External::	zdotc
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R=cdotc(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R=zdotc(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R=cdotc(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R=zdotc(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R=zdotc(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R=zdotc(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R=cdotc(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R=cdotc(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R=zdotc(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R=cdotc(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R=zdotc(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R=zdotc(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R=zdotc(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R=zdotc(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R=zdotc(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R=zdotc(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine
	subroutine product_dotc_com8(R,A,B)	
		complex(kind=8),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotc
		complex(kind=8),External::	zdotc
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R=cdotc(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R=zdotc(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R=cdotc(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R=zdotc(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R=zdotc(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R=zdotc(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R=cdotc(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R=cdotc(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R=zdotc(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R=cdotc(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R=zdotc(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R=zdotc(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R=zdotc(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R=zdotc(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R=zdotc(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R=zdotc(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine
	
	
	
	
	
	
! dot product DO NOT conjugating the first vector,The Tensor will be regard as a vector

	subroutine product_dot_int(R,A,B)	
		integer,intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotu
		complex(kind=8),External::	zdotu
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R=cdotu(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R=zdotu(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R=cdotu(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R=zdotu(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R=zdotu(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R=zdotu(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R=cdotu(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R=cdotu(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R=zdotu(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R=cdotu(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R=zdotu(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R=zdotu(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R=zdotu(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R=zdotu(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R=zdotu(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R=zdotu(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine
	subroutine product_dot_real4(R,A,B)	
		real(kind=4),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotu
		complex(kind=8),External::	zdotu
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R=cdotu(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R=zdotu(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R=cdotu(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R=zdotu(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R=zdotu(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R=zdotu(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R=cdotu(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R=cdotu(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R=zdotu(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R=cdotu(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R=zdotu(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R=zdotu(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R=zdotu(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R=zdotu(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R=zdotu(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R=zdotu(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine
	subroutine product_dot_real8(R,A,B)	
		real(kind=8),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotu
		complex(kind=8),External::	zdotu
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R=cdotu(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R=zdotu(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R=cdotu(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R=zdotu(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R=zdotu(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R=zdotu(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R=cdotu(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R=cdotu(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R=zdotu(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R=cdotu(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R=zdotu(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R=zdotu(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R=zdotu(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R=zdotu(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R=zdotu(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R=zdotu(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine
	subroutine product_dot_com4(R,A,B)	
		complex(kind=4),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotu
		complex(kind=8),External::	zdotu
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R=cdotu(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R=zdotu(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R=cdotu(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R=zdotu(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R=zdotu(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R=zdotu(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R=cdotu(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R=cdotu(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R=zdotu(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R=cdotu(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R=zdotu(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R=zdotu(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R=zdotu(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R=zdotu(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R=zdotu(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R=zdotu(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine
	subroutine product_dot_com8(R,A,B)	
		complex(kind=8),intent(inout)::R
		type(TData),intent(in)::A
		type(TData),intent(in)::B
		integer::flag,length
		real*4,External::	sdot
		real*8,External::	ddot
		complex(kind=4),External::	cdotu
		complex(kind=8),External::	zdotu
		flag=10*A%classType+B%classType
		length=A%totalData
		select case(flag)
			case(11)
				R=DOT_PRODUCT(A%idata(1:length),B%idata(1:length))
			case(12)
				R=sdot(length, real(A%idata(1:length),kind=4), 1, B%sdata, 1)
			case(13)
				R=ddot(length, dble(A%idata(1:length)), 1, B%ddata, 1)
			case(14)
				R=cdotu(length, cmplx(A%idata(1:length),kind=4), 1, B%cdata, 1)
			case(15)
				R=zdotu(length, dcmplx(A%idata(1:length)), 1, B%zdata, 1)
				
			case(21)
				R=sdot(length, A%sdata, 1, real(B%idata(1:length),kind=4), 1)
			case(22)
				R=sdot(length, A%sdata, 1, B%sdata, 1)
			case(23)
				R=ddot(length, dble(A%sdata(1:length)), 1, B%ddata, 1)
			case(24)
				R=cdotu(length, cmplx(A%sdata(1:length),kind=4), 1, B%cdata, 1)
			case(25)
				R=zdotu(length, dcmplx(A%sdata(1:length)), 1, B%zdata, 1)
				
			case(31)
				R=ddot(length, A%ddata, 1, dble(B%idata(1:length)), 1)
			case(32)
				R=ddot(length, A%ddata, 1, dble(B%sdata(1:length)), 1)
			case(33)
				R=ddot(length,  A%ddata, 1, B%ddata, 1)
			case(34)
				R=zdotu(length, dcmplx(A%ddata(1:length)), 1, dcmplx(B%cdata(1:length)), 1)
			case(35)
				R=zdotu(length, dcmplx(A%ddata(1:length)), 1, B%zdata, 1)	
			
			case(41)
				R=cdotu(length, A%cdata, 1, cmplx(B%idata(1:length),kind=4), 1)
			case(42)
				R=cdotu(length, A%cdata, 1, cmplx(B%sdata(1:length),kind=4), 1)
			case(43)
				R=zdotu(length, dcmplx(A%cdata(1:length)), 1, dcmplx(B%ddata(1:length)), 1)
			case(44)
				R=cdotu(length, A%cdata, 1, B%cdata, 1)
			case(45)
				R=zdotu(length, dcmplx(A%cdata(1:length)), 1, B%zdata, 1)	
				
			case(51)
				R=zdotu(length, A%zdata, 1, dcmplx(B%idata(1:length)), 1)
			case(52)
				R=zdotu(length, A%zdata, 1, dcmplx(B%sdata(1:length)), 1)
			case(53)
				R=zdotu(length, A%zdata, 1, dcmplx(B%ddata(1:length)), 1)
			case(54)
				R=zdotu(length, A%zdata, 1, dcmplx(B%cdata(1:length)), 1)
			case(55)
				R=zdotu(length, A%zdata, 1, B%zdata, 1)	
		end select
		return
	end subroutine

!************************************************************************
!           |vec|
!************************************************************************

	
	subroutine norm2_routine_int(outdata,A)
		integer,intent(inout)::outdata
		Type(TData),intent(in)::A
		real(kind=4),external::snrm2,scnrm2
		real(kind=8),external::dznrm2,dnrm2
		integer::length
		real*4::s
		real*8::d
		length=A%TotalData
		select case(A%classtype)
			case (1)
				outdata=DOT_PRODUCT(A%idata(1:length),A%idata(1:length))
			case (2)
				s=snrm2(length,A%sdata,1)
				outdata=s*s
			case (3)
				d=dnrm2(length,A%ddata,1)
				outdata=d*d
			case (4)
				s=scnrm2 (length,A%cdata,1)
				outdata=s*s
			case (5)
				d=dznrm2 (length,A%zData,1)
				outdata=d*d
		end select		
		return
	end subroutine
	subroutine norm_routine_real4(outdata,A)
		real(kind=4),intent(inout)::outdata
		Type(TData),intent(in)::A
		real(kind=4),external::snrm2,scnrm2
		real(kind=8),external::dznrm2,dnrm2
		integer::length
		length=A%TotalData
		select case(A%classtype)
			case (1)
				outdata=sqrt(real(DOT_PRODUCT(A%idata(1:length),A%idata(1:length))))
			case (2)
				outdata=snrm2(length,A%sdata,1)
			case (3)
				outdata=dnrm2(length,A%ddata,1)
			case (4)
				outdata=scnrm2 (length,A%cdata,1)
			case (5)
				outdata=dznrm2 (length,A%zData,1)
		end select		
		return
	end subroutine
	subroutine norm_routine_real8(outdata,A)
		real(kind=8),intent(inout)::outdata
		Type(TData),intent(in)::A
		real(kind=4),external::snrm2,scnrm2
		real(kind=8),external::dznrm2,dnrm2
		integer::length
		length=A%TotalData
		select case(A%classtype)
			case (1)
				outdata=sqrt(real(DOT_PRODUCT(A%idata(1:length),A%idata(1:length))))
			case (2)
				outdata=snrm2(length,A%sdata,1)
			case (3)
				outdata=dnrm2(length,A%ddata,1)
			case (4)
				outdata=scnrm2 (length,A%cdata,1)
			case (5)
				outdata=dznrm2 (length,A%zData,1)
		end select		
		return
	end subroutine
	subroutine norm_routine_com4(outdata,A)
		complex(kind=4),intent(inout)::outdata
		Type(TData),intent(in)::A
		real(kind=4),external::snrm2,scnrm2
		real(kind=8),external::dznrm2,dnrm2
		integer::length
		length=A%TotalData
		select case(A%classtype)
			case (1)
				outdata=sqrt(real(DOT_PRODUCT(A%idata(1:length),A%idata(1:length))))
			case (2)
				outdata=snrm2(length,A%sdata,1)
			case (3)
				outdata=dnrm2(length,A%ddata,1)
			case (4)
				outdata=scnrm2 (length,A%cdata,1)
			case (5)
				outdata=dznrm2 (length,A%zData,1)
		end select		
		return
	end subroutine
	subroutine norm_routine_com8(outdata,A)
		complex(kind=8),intent(inout)::outdata
		Type(TData),intent(in)::A
		real(kind=4),external::snrm2,scnrm2
		real(kind=8),external::dznrm2,dnrm2
		integer::length
		length=A%TotalData
		select case(A%classtype)
			case (1)
				outdata=sqrt(real(DOT_PRODUCT(A%idata(1:length),A%idata(1:length))))
			case (2)
				outdata=snrm2(length,A%sdata,1)
			case (3)
				outdata=dnrm2(length,A%ddata,1)
			case (4)
				outdata=scnrm2 (length,A%cdata,1)
			case (5)
				outdata=dznrm2 (length,A%zData,1)
		end select		
		return
	end subroutine
	
	subroutine operation_on_TData(T,routine,T2)
		class(TData),intent(inout)::T
		class(TData),optional,intent(inout)::T2
		external::routine
		integer::classtype
		if(present(T2))then
			select case(T%classtype)
				case (1)
					select case(T2%classtype)
						case (1)
							call routine(T%idata,T%TotalData,T2%idata,T2%TotalData)
						case (2)
							call routine(T%idata,T%TotalData,T2%sdata,T2%TotalData)
						case (3)
							call routine(T%idata,T%TotalData,T2%ddata,T2%TotalData)
						case (4)
							call routine(T%idata,T%TotalData,T2%cdata,T2%TotalData)
						case (5)
							call routine(T%idata,T%TotalData,T2%zdata,T2%TotalData)
						case (6)
							call routine(T%idata,T%TotalData,T2%ldata,T2%TotalData)
						case (7)
							call routine(T%idata,T%TotalData,T2%adata,T2%TotalData)
					end select
				case (2)
					select case(T2%classtype)
						case (1)
							call routine(T%sdata,T%TotalData,T2%idata,T2%TotalData)
						case (2)
							call routine(T%sdata,T%TotalData,T2%sdata,T2%TotalData)
						case (3)
							call routine(T%sdata,T%TotalData,T2%ddata,T2%TotalData)
						case (4)
							call routine(T%sdata,T%TotalData,T2%cdata,T2%TotalData)
						case (5)
							call routine(T%sdata,T%TotalData,T2%zdata,T2%TotalData)
						case (6)
							call routine(T%sdata,T%TotalData,T2%ldata,T2%TotalData)
						case (7)
							call routine(T%sdata,T%TotalData,T2%adata,T2%TotalData)
					end select
				case (3)
					select case(T2%classtype)
						case (1)
							call routine(T%ddata,T%TotalData,T2%idata,T2%TotalData)
						case (2)
							call routine(T%ddata,T%TotalData,T2%sdata,T2%TotalData)
						case (3)
							call routine(T%ddata,T%TotalData,T2%ddata,T2%TotalData)
						case (4)
							call routine(T%ddata,T%TotalData,T2%cdata,T2%TotalData)
						case (5)
							call routine(T%ddata,T%TotalData,T2%zdata,T2%TotalData)
						case (6)
							call routine(T%ddata,T%TotalData,T2%ldata,T2%TotalData)
						case (7)
							call routine(T%ddata,T%TotalData,T2%adata,T2%TotalData)
					end select
				case (4)
					select case(T2%classtype)
						case (1)
							call routine(T%cdata,T%TotalData,T2%idata,T2%TotalData)
						case (2)
							call routine(T%cdata,T%TotalData,T2%sdata,T2%TotalData)
						case (3)
							call routine(T%cdata,T%TotalData,T2%ddata,T2%TotalData)
						case (4)
							call routine(T%cdata,T%TotalData,T2%cdata,T2%TotalData)
						case (5)
							call routine(T%cdata,T%TotalData,T2%zdata,T2%TotalData)
						case (6)
							call routine(T%cdata,T%TotalData,T2%ldata,T2%TotalData)
						case (7)
							call routine(T%cdata,T%TotalData,T2%adata,T2%TotalData)
					end select
				case (5)
					select case(T2%classtype)
						case (1)
							call routine(T%zdata,T%TotalData,T2%idata,T2%TotalData)
						case (2)
							call routine(T%zdata,T%TotalData,T2%sdata,T2%TotalData)
						case (3)
							call routine(T%zdata,T%TotalData,T2%ddata,T2%TotalData)
						case (4)
							call routine(T%zdata,T%TotalData,T2%cdata,T2%TotalData)
						case (5)
							call routine(T%zdata,T%TotalData,T2%zdata,T2%TotalData)
						case (6)
							call routine(T%zdata,T%TotalData,T2%ldata,T2%TotalData)
						case (7)
							call routine(T%zdata,T%TotalData,T2%adata,T2%TotalData)
					end select
				case (6)
					select case(T2%classtype)
						case (1)
							call routine(T%ldata,T%TotalData,T2%idata,T2%TotalData)
						case (2)
							call routine(T%ldata,T%TotalData,T2%sdata,T2%TotalData)
						case (3)
							call routine(T%ldata,T%TotalData,T2%ddata,T2%TotalData)
						case (4)
							call routine(T%ldata,T%TotalData,T2%cdata,T2%TotalData)
						case (5)
							call routine(T%ldata,T%TotalData,T2%zdata,T2%TotalData)
						case (6)
							call routine(T%ldata,T%TotalData,T2%ldata,T2%TotalData)
						case (7)
							call routine(T%ldata,T%TotalData,T2%adata,T2%TotalData)
					end select
				case (7)
					select case(T2%classtype)
						case (1)
							call routine(T%adata,T%TotalData,T2%idata,T2%TotalData)
						case (2)
							call routine(T%adata,T%TotalData,T2%sdata,T2%TotalData)
						case (3)
							call routine(T%adata,T%TotalData,T2%ddata,T2%TotalData)
						case (4)
							call routine(T%adata,T%TotalData,T2%cdata,T2%TotalData)
						case (5)
							call routine(T%adata,T%TotalData,T2%zdata,T2%TotalData)
						case (6)
							call routine(T%adata,T%TotalData,T2%ldata,T2%TotalData)
						case (7)
							call routine(T%adata,T%TotalData,T2%adata,T2%TotalData)
					end select
				case default 
					write(*,*)"error in operation_on_TData"
					call error_stop()
			end select
		else
			select case(T%classtype)
				case (1)
					call routine(T%idata,T%TotalData)
				case (2)
					call routine(T%sdata,T%TotalData)	
				case (3)
					call routine(T%ddata,T%TotalData)
				case (4)
					call routine(T%cdata,T%TotalData)	
				case (5)
					call routine(T%zdata,T%TotalData)
				case (6)
					call routine(T%ldata,T%TotalData)
				case (7)
					call routine(T%adata,T%TotalData)
				case default 
					write(*,*)"error in operation_on_TData"
					call error_stop()
			end select
		end if
		return
	end subroutine
	
	subroutine operation_on_TData2(T,routine,dimen)
		class(TData),intent(inout)::T
		Type(Dimension),intent(inout)::dimen
		external::routine
		select case(T%classtype)
			case (1)
				call routine(T%idata,T%TotalData,dimen)
			case (2)
				call routine(T%sdata,T%TotalData,dimen)
			case (3)
				call routine(T%ddata,T%TotalData,dimen)
			case (4)
				call routine(T%cdata,T%TotalData,dimen)
			case (5)
				call routine(T%zdata,T%TotalData,dimen)
			case (6)
				call routine(T%ldata,T%TotalData,dimen)
			case (7)
				call routine(T%adata,T%TotalData,dimen)
			case default 
				write(*,*)"error in operation_on_TData"
				call error_stop()
		end select
		return
	end subroutine
	
	subroutine operation_on_TData3(T,routine,dimenT1,T2,dimenT2)
		class(TData),intent(inout)::T
		class(TData),intent(inout)::T2
		Type(Dimension),intent(inout)::dimenT1,dimenT2
		external::routine
		integer::classtype
		select case(T%classtype)
			case (1)
				select case(T2%classtype)
					case (1)
						call routine(T%idata,T%TotalData,dimenT1,T2%idata,T2%TotalData,dimenT2)
					case (2)
						call routine(T%idata,T%TotalData,dimenT1,T2%sdata,T2%TotalData,dimenT2)
					case (3)
						call routine(T%idata,T%TotalData,dimenT1,T2%ddata,T2%TotalData,dimenT2)
					case (4)
						call routine(T%idata,T%TotalData,dimenT1,T2%cdata,T2%TotalData,dimenT2)
					case (5)
						call routine(T%idata,T%TotalData,dimenT1,T2%zdata,T2%TotalData,dimenT2)
					case (6)
						call routine(T%idata,T%TotalData,dimenT1,T2%ldata,T2%TotalData,dimenT2)
					case (7)
						call routine(T%idata,T%TotalData,dimenT1,T2%adata,T2%TotalData,dimenT2)
				end select
			case (2)
				select case(T2%classtype)
					case (1)
						call routine(T%sdata,T%TotalData,dimenT1,T2%idata,T2%TotalData,dimenT2)
					case (2)
						call routine(T%sdata,T%TotalData,dimenT1,T2%sdata,T2%TotalData,dimenT2)
					case (3)
						call routine(T%sdata,T%TotalData,dimenT1,T2%ddata,T2%TotalData,dimenT2)
					case (4)
						call routine(T%sdata,T%TotalData,dimenT1,T2%cdata,T2%TotalData,dimenT2)
					case (5)
						call routine(T%sdata,T%TotalData,dimenT1,T2%zdata,T2%TotalData,dimenT2)
					case (6)
						call routine(T%sdata,T%TotalData,dimenT1,T2%ldata,T2%TotalData,dimenT2)
					case (7)
						call routine(T%sdata,T%TotalData,dimenT1,T2%adata,T2%TotalData,dimenT2)
				end select
			case (3)
				select case(T2%classtype)
					case (1)
						call routine(T%ddata,T%TotalData,dimenT1,T2%idata,T2%TotalData,dimenT2)
					case (2)
						call routine(T%ddata,T%TotalData,dimenT1,T2%sdata,T2%TotalData,dimenT2)
					case (3)
						call routine(T%ddata,T%TotalData,dimenT1,T2%ddata,T2%TotalData,dimenT2)
					case (4)
						call routine(T%ddata,T%TotalData,dimenT1,T2%cdata,T2%TotalData,dimenT2)
					case (5)
						call routine(T%ddata,T%TotalData,dimenT1,T2%zdata,T2%TotalData,dimenT2)
					case (6)
						call routine(T%ddata,T%TotalData,dimenT1,T2%ldata,T2%TotalData,dimenT2)
					case (7)
						call routine(T%ddata,T%TotalData,dimenT1,T2%adata,T2%TotalData,dimenT2)
				end select
			case (4)
				select case(T2%classtype)
					case (1)
						call routine(T%cdata,T%TotalData,dimenT1,T2%idata,T2%TotalData,dimenT2)
					case (2)
						call routine(T%cdata,T%TotalData,dimenT1,T2%sdata,T2%TotalData,dimenT2)
					case (3)
						call routine(T%cdata,T%TotalData,dimenT1,T2%ddata,T2%TotalData,dimenT2)
					case (4)
						call routine(T%cdata,T%TotalData,dimenT1,T2%cdata,T2%TotalData,dimenT2)
					case (5)
						call routine(T%cdata,T%TotalData,dimenT1,T2%zdata,T2%TotalData,dimenT2)
					case (6)
						call routine(T%cdata,T%TotalData,dimenT1,T2%ldata,T2%TotalData,dimenT2)
					case (7)
						call routine(T%cdata,T%TotalData,dimenT1,T2%adata,T2%TotalData,dimenT2)
				end select
			case (5)
				select case(T2%classtype)
					case (1)
						call routine(T%zdata,T%TotalData,dimenT1,T2%idata,T2%TotalData,dimenT2)
					case (2)
						call routine(T%zdata,T%TotalData,dimenT1,T2%sdata,T2%TotalData,dimenT2)
					case (3)
						call routine(T%zdata,T%TotalData,dimenT1,T2%ddata,T2%TotalData,dimenT2)
					case (4)
						call routine(T%zdata,T%TotalData,dimenT1,T2%cdata,T2%TotalData,dimenT2)
					case (5)
						call routine(T%zdata,T%TotalData,dimenT1,T2%zdata,T2%TotalData,dimenT2)
					case (6)
						call routine(T%zdata,T%TotalData,dimenT1,T2%ldata,T2%TotalData,dimenT2)
					case (7)
						call routine(T%zdata,T%TotalData,dimenT1,T2%adata,T2%TotalData,dimenT2)
				end select
			case (6)
				select case(T2%classtype)
					case (1)
						call routine(T%ldata,T%TotalData,dimenT1,T2%idata,T2%TotalData,dimenT2)
					case (2)
						call routine(T%ldata,T%TotalData,dimenT1,T2%sdata,T2%TotalData,dimenT2)
					case (3)
						call routine(T%ldata,T%TotalData,dimenT1,T2%ddata,T2%TotalData,dimenT2)
					case (4)
						call routine(T%ldata,T%TotalData,dimenT1,T2%cdata,T2%TotalData,dimenT2)
					case (5)
						call routine(T%ldata,T%TotalData,dimenT1,T2%zdata,T2%TotalData,dimenT2)
					case (6)
						call routine(T%ldata,T%TotalData,dimenT1,T2%ldata,T2%TotalData,dimenT2)
					case (7)
						call routine(T%ldata,T%TotalData,dimenT1,T2%adata,T2%TotalData,dimenT2)
				end select
			case (7)
				select case(T2%classtype)
					case (1)
						call routine(T%adata,T%TotalData,dimenT1,T2%idata,T2%TotalData,dimenT2)
					case (2)
						call routine(T%adata,T%TotalData,dimenT1,T2%sdata,T2%TotalData,dimenT2)
					case (3)
						call routine(T%adata,T%TotalData,dimenT1,T2%ddata,T2%TotalData,dimenT2)
					case (4)
						call routine(T%adata,T%TotalData,dimenT1,T2%cdata,T2%TotalData,dimenT2)
					case (5)
						call routine(T%adata,T%TotalData,dimenT1,T2%zdata,T2%TotalData,dimenT2)
					case (6)
						call routine(T%adata,T%TotalData,dimenT1,T2%ldata,T2%TotalData,dimenT2)
					case (7)
						call routine(T%adata,T%TotalData,dimenT1,T2%adata,T2%TotalData,dimenT2)
				end select
			case default 
				write(*,*)"error in operation_on_TData"
				call error_stop()
		end select
		return
	end subroutine
	subroutine operation_on_TData4(T,routine,dimenT1,T2)
		class(TData),intent(inout)::T
		class(TData),intent(inout)::T2
		Type(Dimension),intent(inout)::dimenT1
		external::routine
		integer::classtype
		select case(T%classtype)
			case (1)
				select case(T2%classtype)
					case (1)
						call routine(T%idata,T%TotalData,dimenT1,T2%idata,T2%TotalData)
					case (2)
						call routine(T%idata,T%TotalData,dimenT1,T2%sdata,T2%TotalData)
					case (3)
						call routine(T%idata,T%TotalData,dimenT1,T2%ddata,T2%TotalData)
					case (4)
						call routine(T%idata,T%TotalData,dimenT1,T2%cdata,T2%TotalData)
					case (5)
						call routine(T%idata,T%TotalData,dimenT1,T2%zdata,T2%TotalData)
					case (6)
						call routine(T%idata,T%TotalData,dimenT1,T2%ldata,T2%TotalData)
					case (7)
						call routine(T%idata,T%TotalData,dimenT1,T2%adata,T2%TotalData)
				end select
			case (2)
				select case(T2%classtype)
					case (1)
						call routine(T%sdata,T%TotalData,dimenT1,T2%idata,T2%TotalData)
					case (2)
						call routine(T%sdata,T%TotalData,dimenT1,T2%sdata,T2%TotalData)
					case (3)
						call routine(T%sdata,T%TotalData,dimenT1,T2%ddata,T2%TotalData)
					case (4)
						call routine(T%sdata,T%TotalData,dimenT1,T2%cdata,T2%TotalData)
					case (5)
						call routine(T%sdata,T%TotalData,dimenT1,T2%zdata,T2%TotalData)
					case (6)
						call routine(T%sdata,T%TotalData,dimenT1,T2%ldata,T2%TotalData)
					case (7)
						call routine(T%sdata,T%TotalData,dimenT1,T2%adata,T2%TotalData)
				end select
			case (3)
				select case(T2%classtype)
					case (1)
						call routine(T%ddata,T%TotalData,dimenT1,T2%idata,T2%TotalData)
					case (2)
						call routine(T%ddata,T%TotalData,dimenT1,T2%sdata,T2%TotalData)
					case (3)
						call routine(T%ddata,T%TotalData,dimenT1,T2%ddata,T2%TotalData)
					case (4)
						call routine(T%ddata,T%TotalData,dimenT1,T2%cdata,T2%TotalData)
					case (5)
						call routine(T%ddata,T%TotalData,dimenT1,T2%zdata,T2%TotalData)
					case (6)
						call routine(T%ddata,T%TotalData,dimenT1,T2%ldata,T2%TotalData)
					case (7)
						call routine(T%ddata,T%TotalData,dimenT1,T2%adata,T2%TotalData)
				end select
			case (4)
				select case(T2%classtype)
					case (1)
						call routine(T%cdata,T%TotalData,dimenT1,T2%idata,T2%TotalData)
					case (2)
						call routine(T%cdata,T%TotalData,dimenT1,T2%sdata,T2%TotalData)
					case (3)
						call routine(T%cdata,T%TotalData,dimenT1,T2%ddata,T2%TotalData)
					case (4)
						call routine(T%cdata,T%TotalData,dimenT1,T2%cdata,T2%TotalData)
					case (5)
						call routine(T%cdata,T%TotalData,dimenT1,T2%zdata,T2%TotalData)
					case (6)
						call routine(T%cdata,T%TotalData,dimenT1,T2%ldata,T2%TotalData)
					case (7)
						call routine(T%cdata,T%TotalData,dimenT1,T2%adata,T2%TotalData)
				end select
			case (5)
				select case(T2%classtype)
					case (1)
						call routine(T%zdata,T%TotalData,dimenT1,T2%idata,T2%TotalData)
					case (2)
						call routine(T%zdata,T%TotalData,dimenT1,T2%sdata,T2%TotalData)
					case (3)
						call routine(T%zdata,T%TotalData,dimenT1,T2%ddata,T2%TotalData)
					case (4)
						call routine(T%zdata,T%TotalData,dimenT1,T2%cdata,T2%TotalData)
					case (5)
						call routine(T%zdata,T%TotalData,dimenT1,T2%zdata,T2%TotalData)
					case (6)
						call routine(T%zdata,T%TotalData,dimenT1,T2%ldata,T2%TotalData)
					case (7)
						call routine(T%zdata,T%TotalData,dimenT1,T2%adata,T2%TotalData)
				end select
			case (6)
				select case(T2%classtype)
					case (1)
						call routine(T%ldata,T%TotalData,dimenT1,T2%idata,T2%TotalData)
					case (2)
						call routine(T%ldata,T%TotalData,dimenT1,T2%sdata,T2%TotalData)
					case (3)
						call routine(T%ldata,T%TotalData,dimenT1,T2%ddata,T2%TotalData)
					case (4)
						call routine(T%ldata,T%TotalData,dimenT1,T2%cdata,T2%TotalData)
					case (5)
						call routine(T%ldata,T%TotalData,dimenT1,T2%zdata,T2%TotalData)
					case (6)
						call routine(T%ldata,T%TotalData,dimenT1,T2%ldata,T2%TotalData)
					case (7)
						call routine(T%ldata,T%TotalData,dimenT1,T2%adata,T2%TotalData)
				end select
			case (7)
				select case(T2%classtype)
					case (1)
						call routine(T%adata,T%TotalData,dimenT1,T2%idata,T2%TotalData)
					case (2)
						call routine(T%adata,T%TotalData,dimenT1,T2%sdata,T2%TotalData)
					case (3)
						call routine(T%adata,T%TotalData,dimenT1,T2%ddata,T2%TotalData)
					case (4)
						call routine(T%adata,T%TotalData,dimenT1,T2%cdata,T2%TotalData)
					case (5)
						call routine(T%adata,T%TotalData,dimenT1,T2%zdata,T2%TotalData)
					case (6)
						call routine(T%adata,T%TotalData,dimenT1,T2%ldata,T2%TotalData)
					case (7)
						call routine(T%adata,T%TotalData,dimenT1,T2%adata,T2%TotalData)
				end select
			case default 
				write(*,*)"error in operation_on_TData"
				call error_stop()
		end select
		return
	end subroutine
	
	
	subroutine ipointerToDataSubroutine(p,indata,lenData)
		integer,intent(in)::lenData
		integer,intent(in),target::indata(lenData)
		integer,pointer,intent(inout)::p(:)
		p=>indata(1:lenData)
		return
	end subroutine
	subroutine spointerToDataSubroutine(p,indata,lenData)
		integer,intent(in)::lenData
		real*4,intent(in),target::indata(lenData)
		real*4,pointer,intent(inout)::p(:)
		p=>indata(1:lenData)
		return
	end subroutine
	subroutine dpointerToDataSubroutine(p,indata,lenData)
		integer,intent(in)::lenData
		real*8,intent(in),target::indata(lenData)
		real*8,pointer,intent(inout)::p(:)
		p=>indata(1:lenData)
		return
	end subroutine
	subroutine cpointerToDataSubroutine(p,indata,lenData)
		integer,intent(in)::lenData
		complex(kind=4),intent(in),target::indata(lenData)
		complex(kind=4),pointer,intent(inout)::p(:)
		p=>indata(1:lenData)
		return
	end subroutine
	subroutine zpointerToDataSubroutine(p,indata,lenData)
		integer,intent(in)::lenData
		complex(kind=8),intent(in),target::indata(lenData)
		complex(kind=8),pointer,intent(inout)::p(:)
		p=>indata(1:lenData)
		return
	end subroutine
	subroutine lpointerToDataSubroutine(p,indata,lenData)
		integer,intent(in)::lenData
		logical,intent(in),target::indata(lenData)
		logical,pointer,intent(inout)::p(:)
		p=>indata(1:lenData)
		return
	end subroutine
	subroutine apointerToDataSubroutine(p,indata,lenData)
		integer,intent(in)::lenData
		character(len=max_len_of_char_in_TData),intent(in),target::indata(lenData)
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:)
		p=>indata(1:lenData)
		return
	end subroutine
	
	subroutine ipointer(T,p)
		class(TData),intent(in)::T
		integer,pointer,intent(inout)::p(:)
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.1)then
			call writemess('The type of Tensor is not  integer',-1)
			call error_stop
		end if
		call ipointerToDataSubroutine(p,T%idata,T%totalData)
		return
	end subroutine
	subroutine ipointer_(T,p,i1i2)
		class(TData),intent(in)::T
		integer,pointer,intent(inout)::p(:)
		integer,intent(in)::i1i2(2)
		integer::length
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.1)then
			call writemess('The type of Tensor is not  integer',-1)
			call error_stop
		end if
		length=i1i2(2)-i1i2(1)+1
		if(length.gt.T%TotalData)then
			call writemess('ERROR in pointing integer, pointer lenght larger than totoaldata',-1)
			call error_stop
		end if
		call ipointerToDataSubroutine(p,T%idata(i1i2(1):i1i2(2)),length)
		return
	end subroutine
	subroutine spointer(T,p)
		class(TData),intent(in)::T
		real*4,pointer,intent(inout)::p(:)
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.2)then
			call writemess('The type of Tensor is not  real*4',-1)
			call error_stop
		end if
		call spointerToDataSubroutine(p,T%sdata,T%totalData)
		return
	end subroutine
	subroutine spointer_(T,p,i1i2)
		class(TData),intent(in)::T
		real*4,pointer,intent(inout)::p(:)
		integer,intent(in)::i1i2(2)
		integer::length
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.2)then
			call writemess('The type of Tensor is not  real*4',-1)
			call error_stop
		end if
		length=i1i2(2)-i1i2(1)+1
		if(length.gt.T%TotalData)then
			call writemess('ERROR in pointing real*4, pointer lenght larger than totoaldata',-1)
			call error_stop
		end if
		call spointerToDataSubroutine(p,T%sdata(i1i2(1):i1i2(2)),length)
		return
	end subroutine
	subroutine dpointer(T,p)
		class(TData),intent(in)::T
		real*8,pointer,intent(inout)::p(:)
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.3)then
			call writemess('The type of Tensor is not  real*8',-1)
			call error_stop
		end if
		call dpointerToDataSubroutine(p,T%ddata,T%totalData)
		return
	end subroutine
	subroutine dpointer_(T,p,i1i2)
		class(TData),intent(in)::T
		real*8,pointer,intent(inout)::p(:)
		integer,intent(in)::i1i2(2)
		integer::length
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.3)then
			call writemess('The type of Tensor is not  real*8',-1)
			call error_stop
		end if
		length=i1i2(2)-i1i2(1)+1
		if(length.gt.T%TotalData)then
			call writemess('ERROR in pointing real*8, pointer lenght larger than totoaldata',-1)
			call error_stop
		end if
		call dpointerToDataSubroutine(p,T%ddata(i1i2(1):i1i2(2)),length)
		return
	end subroutine
	subroutine cpointer(T,p)
		class(TData),intent(in)::T
		complex(kind=4),pointer,intent(inout)::p(:)
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.4)then
			call writemess('The type of Tensor is not  complex(kind=4)',-1)
			call error_stop
		end if
		call cpointerToDataSubroutine(p,T%cdata,T%totalData)
		return
	end subroutine
	subroutine cpointer_(T,p,i1i2)
		class(TData),intent(in)::T
		complex(kind=4),pointer,intent(inout)::p(:)
		integer,intent(in)::i1i2(2)
		integer::length
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.4)then
			call writemess('The type of Tensor is not  complex(kind=4)',-1)
			call error_stop
		end if
		length=i1i2(2)-i1i2(1)+1
		if(length.gt.T%TotalData)then
			call writemess('ERROR in pointing complex(kind=4), pointer lenght larger than totoaldata',-1)
			call error_stop
		end if
		call cpointerToDataSubroutine(p,T%cdata(i1i2(1):i1i2(2)),length)
		return
	end subroutine
	subroutine zpointer(T,p)
		class(TData),intent(in)::T
		complex(kind=8),pointer,intent(inout)::p(:)
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.5)then
			call writemess('The type of Tensor is not  complex(kind=8)',-1)
			call error_stop
		end if
		call zpointerToDataSubroutine(p,T%zdata,T%totalData)
		return
	end subroutine
	subroutine zpointer_(T,p,i1i2)
		class(TData),intent(in)::T
		complex(kind=8),pointer,intent(inout)::p(:)
		integer,intent(in)::i1i2(2)
		integer::length
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.5)then
			call writemess('The type of Tensor is not  complex(kind=8)',-1)
			call error_stop
		end if
		length=i1i2(2)-i1i2(1)+1
		if(length.gt.T%TotalData)then
			call writemess('ERROR in pointing complex(kind=8), pointer lenght larger than totoaldata',-1)
			call error_stop
		end if
		call zpointerToDataSubroutine(p,T%zdata(i1i2(1):i1i2(2)),length)
		return
	end subroutine
	subroutine lpointer(T,p)
		class(TData),intent(in)::T
		logical,pointer,intent(inout)::p(:)
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.6)then
			call writemess('The type of Tensor is not  logical',-1)
			call error_stop
		end if
		call lpointerToDataSubroutine(p,T%ldata,T%totalData)
		return
	end subroutine
	subroutine lpointer_(T,p,i1i2)
		class(TData),intent(in)::T
		logical,pointer,intent(inout)::p(:)
		integer,intent(in)::i1i2(2)
		integer::length
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.6)then
			call writemess('The type of Tensor is not  logical',-1)
			call error_stop
		end if
		length=i1i2(2)-i1i2(1)+1
		if(length.gt.T%TotalData)then
			call writemess('ERROR in pointing logical, pointer lenght larger than totoaldata',-1)
			call error_stop
		end if
		call lpointerToDataSubroutine(p,T%ldata(i1i2(1):i1i2(2)),length)
		return
	end subroutine
	subroutine apointer(T,p)
		class(TData),intent(in)::T
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:)
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.7)then
			call writemess('The type of Tensor is not  character(len=*)',-1)
			call error_stop
		end if
		call apointerToDataSubroutine(p,T%adata,T%totalData)
		return
	end subroutine
	subroutine apointer_(T,p,i1i2)
		class(TData),intent(in)::T
		integer,intent(in)::i1i2(2)
		integer::length
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:)
		if(T%totalData.eq.0)then
			call writemess('There is no data in the Tensor',-1)
			call error_stop
		end if
		if(T%classtype.ne.7)then
			call writemess('The type of Tensor is not  character(len=characterlen)',-1)
			call error_stop
		end if
		length=(i1i2(2)-i1i2(1)+1)
		if(length.gt.T%TotalData)then
			call writemess('ERROR in pointing character(len=characterlen), pointer lenght larger than totoaldata',-1)
			call error_stop
		end if
		call apointerToDataSubroutine(p,T%adata(i1i2(1):i1i2(2)),length)
		return
	end subroutine
	
	
	subroutine ipointerToDataSubroutine2(p,indata,ith,jth,dim1,dim2)
		integer,intent(in)::ith(2),jth(2),dim1,dim2
		integer,intent(in),target::indata(dim1,dim2)
		integer,pointer,intent(inout)::p(:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2))
		return
	end subroutine
	subroutine ipointer2(T,p,i,j,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),d(2)
		integer,pointer,intent(inout)::p(:,:)
		call ipointerToDataSubroutine2(p,T%idata,i,j,d(1),d(2))
		return
	end subroutine
		
		
	subroutine spointerToDataSubroutine2(p,indata,ith,jth,dim1,dim2)
		integer,intent(in)::ith(2),jth(2),dim1,dim2
		real*4,intent(in),target::indata(dim1,dim2)
		real*4,pointer,intent(inout)::p(:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2))
		return
	end subroutine
	subroutine spointer2(T,p,i,j,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),d(2)
		real*4,pointer,intent(inout)::p(:,:)
		call spointerToDataSubroutine2(p,T%sdata,i,j,d(1),d(2))
		return
	end subroutine
	
	subroutine dpointerToDataSubroutine2(p,indata,ith,jth,dim1,dim2)
		integer,intent(in)::ith(2),jth(2),dim1,dim2
		real*8,intent(in),target::indata(dim1,dim2)
		real*8,pointer,intent(inout)::p(:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2))
		return
	end subroutine
	subroutine dpointer2(T,p,i,j,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),d(2)
		real*8,pointer,intent(inout)::p(:,:)
		call dpointerToDataSubroutine2(p,T%ddata,i,j,d(1),d(2))
		return
	end subroutine
	
	subroutine cpointerToDataSubroutine2(p,indata,ith,jth,dim1,dim2)
		integer,intent(in)::ith(2),jth(2),dim1,dim2
		complex(kind=4),intent(in),target::indata(dim1,dim2)
		complex(kind=4),pointer,intent(inout)::p(:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2))
		return
	end subroutine
	subroutine cpointer2(T,p,i,j,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),d(2)
		complex(kind=4),pointer,intent(inout)::p(:,:)
		call cpointerToDataSubroutine2(p,T%cdata,i,j,d(1),d(2))
		return
	end subroutine
	
	subroutine zpointerToDataSubroutine2(p,indata,ith,jth,dim1,dim2)
		integer,intent(in)::ith(2),jth(2),dim1,dim2
		complex(kind=8),intent(in),target::indata(dim1,dim2)
		complex(kind=8),pointer,intent(inout)::p(:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2))
		return
	end subroutine
	subroutine zpointer2(T,p,i,j,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),d(2)
		complex(kind=8),pointer,intent(inout)::p(:,:)
		call zpointerToDataSubroutine2(p,T%zdata,i,j,d(1),d(2))
		return
	end subroutine
	
	subroutine lpointerToDataSubroutine2(p,indata,ith,jth,dim1,dim2)
		integer,intent(in)::ith(2),jth(2),dim1,dim2
		logical,intent(in),target::indata(dim1,dim2)
		logical,pointer,intent(inout)::p(:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2))
		return
	end subroutine
	subroutine lpointer2(T,p,i,j,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),d(2)
		logical,pointer,intent(inout)::p(:,:)
		call lpointerToDataSubroutine2(p,T%ldata,i,j,d(1),d(2))
		return
	end subroutine
	
	subroutine apointerToDataSubroutine2(p,indata,ith,jth,dim1,dim2)
		integer,intent(in)::ith(2),jth(2),dim1,dim2
		character(len=max_len_of_char_in_TData),intent(in),target::indata(dim1,dim2)
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2))
		return
	end subroutine
	subroutine apointer2(T,p,i,j,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),d(2)
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:,:)
		call apointerToDataSubroutine2(p,T%adata,i,j,d(1),d(2))
		return
	end subroutine
	
	subroutine ipointerToDataSubroutine3(p,indata,ith,jth,kth,dim1,dim2,dim3)
		integer,intent(in)::ith(2),jth(2),kth(2),dim1,dim2,dim3
		integer,intent(in),target::indata(dim1,dim2,dim3)
		integer,pointer,intent(inout)::p(:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2))
		return
	end subroutine
	subroutine ipointer3(T,p,i,j,k,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),d(3)
		integer,pointer,intent(inout)::p(:,:,:)
		call ipointerToDataSubroutine3(p,T%idata,i,j,k,d(1),d(2),d(3))
		return
	end subroutine
	
	subroutine spointerToDataSubroutine3(p,indata,ith,jth,kth,dim1,dim2,dim3)
		integer,intent(in)::ith(2),jth(2),kth(2),dim1,dim2,dim3
		real*4,intent(in),target::indata(dim1,dim2,dim3)
		real*4,pointer,intent(inout)::p(:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2))
		return
	end subroutine
	subroutine spointer3(T,p,i,j,k,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),d(3)
		real*4,pointer,intent(inout)::p(:,:,:)
		call spointerToDataSubroutine3(p,T%sdata,i,j,k,d(1),d(2),d(3))
		return
	end subroutine
	
	subroutine dpointerToDataSubroutine3(p,indata,ith,jth,kth,dim1,dim2,dim3)
		integer,intent(in)::ith(2),jth(2),kth(2),dim1,dim2,dim3
		real*8,intent(in),target::indata(dim1,dim2,dim3)
		real*8,pointer,intent(inout)::p(:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2))
		return
	end subroutine
	subroutine dpointer3(T,p,i,j,k,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),d(3)
		real*8,pointer,intent(inout)::p(:,:,:)
		call dpointerToDataSubroutine3(p,T%ddata,i,j,k,d(1),d(2),d(3))
		return
	end subroutine
	
	subroutine cpointerToDataSubroutine3(p,indata,ith,jth,kth,dim1,dim2,dim3)
		integer,intent(in)::ith(2),jth(2),kth(2),dim1,dim2,dim3
		complex(kind=4),intent(in),target::indata(dim1,dim2,dim3)
		complex(kind=4),pointer,intent(inout)::p(:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2))
		return
	end subroutine
	subroutine cpointer3(T,p,i,j,k,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),d(3)
		complex(kind=4),pointer,intent(inout)::p(:,:,:)
		call cpointerToDataSubroutine3(p,T%cdata,i,j,k,d(1),d(2),d(3))
		return
	end subroutine
	
	subroutine zpointerToDataSubroutine3(p,indata,ith,jth,kth,dim1,dim2,dim3)
		integer,intent(in)::ith(2),jth(2),kth(2),dim1,dim2,dim3
		complex(kind=8),intent(in),target::indata(dim1,dim2,dim3)
		complex(kind=8),pointer,intent(inout)::p(:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2))
		return
	end subroutine
	subroutine zpointer3(T,p,i,j,k,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),d(3)
		complex(kind=8),pointer,intent(inout)::p(:,:,:)
		call zpointerToDataSubroutine3(p,T%zdata,i,j,k,d(1),d(2),d(3))
		return
	end subroutine
	
	subroutine lpointerToDataSubroutine3(p,indata,ith,jth,kth,dim1,dim2,dim3)
		integer,intent(in)::ith(2),jth(2),kth(2),dim1,dim2,dim3
		logical,intent(in),target::indata(dim1,dim2,dim3)
		logical,pointer,intent(inout)::p(:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2))
		return
	end subroutine
	subroutine lpointer3(T,p,i,j,k,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),d(3)
		logical,pointer,intent(inout)::p(:,:,:)
		call lpointerToDataSubroutine3(p,T%ldata,i,j,k,d(1),d(2),d(3))
		return
	end subroutine
	
	subroutine apointerToDataSubroutine3(p,indata,ith,jth,kth,dim1,dim2,dim3)
		integer,intent(in)::ith(2),jth(2),kth(2),dim1,dim2,dim3
		character(len=max_len_of_char_in_TData),intent(in),target::indata(dim1,dim2,dim3)
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2))
		return
	end subroutine
	subroutine apointer3(T,p,i,j,k,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),d(3)
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:,:,:)
		call apointerToDataSubroutine3(p,T%adata,i,j,k,d(1),d(2),d(3))
		return
	end subroutine

	subroutine ipointerToDataSubroutine4(p,indata,ith,jth,kth,lth,dim1,dim2,dim3,dim4)
		integer,intent(in)::ith(2),jth(2),kth(2),lth(2),dim1,dim2,dim3,dim4
		integer,intent(in),target::indata(dim1,dim2,dim3,dim4)
		integer,pointer,intent(inout)::p(:,:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2),lth(1):lth(2))
		return
	end subroutine
	subroutine ipointer4(T,p,i,j,k,l,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),l(2),d(4)
		integer,pointer,intent(inout)::p(:,:,:,:)
		call ipointerToDataSubroutine4(p,T%idata,i,j,k,l,d(1),d(2),d(3),d(4))
		return
	end subroutine

	subroutine spointerToDataSubroutine4(p,indata,ith,jth,kth,lth,dim1,dim2,dim3,dim4)
		integer,intent(in)::ith(2),jth(2),kth(2),lth(2),dim1,dim2,dim3,dim4
		real(kind=4),intent(in),target::indata(dim1,dim2,dim3,dim4)
		real(kind=4),pointer,intent(inout)::p(:,:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2),lth(1):lth(2))
		return
	end subroutine
	subroutine spointer4(T,p,i,j,k,l,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),l(2),d(4)
		real(kind=4),pointer,intent(inout)::p(:,:,:,:)
		call spointerToDataSubroutine4(p,T%sdata,i,j,k,l,d(1),d(2),d(3),d(4))
		return
	end subroutine

	subroutine dpointerToDataSubroutine4(p,indata,ith,jth,kth,lth,dim1,dim2,dim3,dim4)
		integer,intent(in)::ith(2),jth(2),kth(2),lth(2),dim1,dim2,dim3,dim4
		real(kind=8),intent(in),target::indata(dim1,dim2,dim3,dim4)
		real(kind=8),pointer,intent(inout)::p(:,:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2),lth(1):lth(2))
		return
	end subroutine
	subroutine dpointer4(T,p,i,j,k,l,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),l(2),d(4)
		real(kind=8),pointer,intent(inout)::p(:,:,:,:)
		call dpointerToDataSubroutine4(p,T%ddata,i,j,k,l,d(1),d(2),d(3),d(4))
		return
	end subroutine

	subroutine cpointerToDataSubroutine4(p,indata,ith,jth,kth,lth,dim1,dim2,dim3,dim4)
		integer,intent(in)::ith(2),jth(2),kth(2),lth(2),dim1,dim2,dim3,dim4
		complex(kind=4),intent(in),target::indata(dim1,dim2,dim3,dim4)
		complex(kind=4),pointer,intent(inout)::p(:,:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2),lth(1):lth(2))
		return
	end subroutine
	subroutine cpointer4(T,p,i,j,k,l,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),l(2),d(4)
		complex(kind=4),pointer,intent(inout)::p(:,:,:,:)
		call cpointerToDataSubroutine4(p,T%cdata,i,j,k,l,d(1),d(2),d(3),d(4))
		return
	end subroutine

	subroutine zpointerToDataSubroutine4(p,indata,ith,jth,kth,lth,dim1,dim2,dim3,dim4)
		integer,intent(in)::ith(2),jth(2),kth(2),lth(2),dim1,dim2,dim3,dim4
		complex(kind=8),intent(in),target::indata(dim1,dim2,dim3,dim4)
		complex(kind=8),pointer,intent(inout)::p(:,:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2),lth(1):lth(2))
		return
	end subroutine
	subroutine zpointer4(T,p,i,j,k,l,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),l(2),d(4)
		complex(kind=8),pointer,intent(inout)::p(:,:,:,:)
		call zpointerToDataSubroutine4(p,T%zdata,i,j,k,l,d(1),d(2),d(3),d(4))
		return
	end subroutine

	subroutine lpointerToDataSubroutine4(p,indata,ith,jth,kth,lth,dim1,dim2,dim3,dim4)
		integer,intent(in)::ith(2),jth(2),kth(2),lth(2),dim1,dim2,dim3,dim4
		logical,intent(in),target::indata(dim1,dim2,dim3,dim4)
		logical,pointer,intent(inout)::p(:,:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2),lth(1):lth(2))
		return
	end subroutine
	subroutine lpointer4(T,p,i,j,k,l,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),l(2),d(4)
		logical,pointer,intent(inout)::p(:,:,:,:)
		call lpointerToDataSubroutine4(p,T%ldata,i,j,k,l,d(1),d(2),d(3),d(4))
		return
	end subroutine

	subroutine apointerToDataSubroutine4(p,indata,ith,jth,kth,lth,dim1,dim2,dim3,dim4)
		integer,intent(in)::ith(2),jth(2),kth(2),lth(2),dim1,dim2,dim3,dim4
		character(len=max_len_of_char_in_TData),intent(in),target::indata(dim1,dim2,dim3,dim4)
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:,:,:,:)
		p=>indata(ith(1):ith(2),jth(1):jth(2),kth(1):kth(2),lth(1):lth(2))
		return
	end subroutine
	subroutine apointer4(T,p,i,j,k,l,d)
		class(TData),intent(in)::T
		integer,intent(in)::i(2),j(2),k(2),l(2),d(4)
		character(len=max_len_of_char_in_TData),pointer,intent(inout)::p(:,:,:,:)
		call apointerToDataSubroutine4(p,T%adata,i,j,k,l,d(1),d(2),d(3),d(4))
		return
	end subroutine
	

!***************************************************************************
!                       directProduct
	subroutine directProduct2_int_routine(outdata,A,B,LDA1,LDA2,LDB1,LDB2)
		integer,intent(in)::LDA1,LDA2,LDB1,LDB2
		integer,intent(inout)::outdata(LDA1,LDB1,LDA2,LDB2)
		integer,intent(in)::A(LDA1,LDA2),B(LDB1,LDB2)
		integer::i,j,k,l
		do l=1,LDB2
			do k=1,LDA2
				do j=1,LDB1
					do i=1,LDA1
						outdata(i,j,k,l)=A(i,k)*B(j,l)
					end do
				end do
			end do
		end do
		return
	end subroutine
	subroutine directProduct2_real4_routine(outdata,A,B,LDA1,LDA2,LDB1,LDB2)
		integer,intent(in)::LDA1,LDA2,LDB1,LDB2
		real(kind=4),intent(inout)::outdata(LDA1,LDB1,LDA2,LDB2)
		real(kind=4),intent(in)::A(LDA1,LDA2),B(LDB1,LDB2)
		integer::i,j,k,l
		do l=1,LDB2
			do k=1,LDA2
				do j=1,LDB1
					do i=1,LDA1
						outdata(i,j,k,l)=A(i,k)*B(j,l)
					end do
				end do
			end do
		end do
		return
	end subroutine
	subroutine directProduct2_real8_routine(outdata,A,B,LDA1,LDA2,LDB1,LDB2)
		integer,intent(in)::LDA1,LDA2,LDB1,LDB2
		real(kind=8),intent(inout)::outdata(LDA1,LDB1,LDA2,LDB2)
		real(kind=8),intent(in)::A(LDA1,LDA2),B(LDB1,LDB2)
		integer::i,j,k,l
		do l=1,LDB2
			do k=1,LDA2
				do j=1,LDB1
					do i=1,LDA1
						outdata(i,j,k,l)=A(i,k)*B(j,l)
					end do
				end do
			end do
		end do
		return
	end subroutine
	subroutine directProduct2_com4_routine(outdata,A,B,LDA1,LDA2,LDB1,LDB2)
		integer,intent(in)::LDA1,LDA2,LDB1,LDB2
		complex(kind=4),intent(inout)::outdata(LDA1,LDB1,LDA2,LDB2)
		complex(kind=4),intent(in)::A(LDA1,LDA2),B(LDB1,LDB2)
		integer::i,j,k,l
		do l=1,LDB2
			do k=1,LDA2
				do j=1,LDB1
					do i=1,LDA1
						outdata(i,j,k,l)=A(i,k)*B(j,l)
					end do
				end do
			end do
		end do
		return
	end subroutine
	subroutine directProduct2_com8_routine(outdata,A,B,LDA1,LDA2,LDB1,LDB2)
		integer,intent(in)::LDA1,LDA2,LDB1,LDB2
		complex(kind=8),intent(inout)::outdata(LDA1,LDB1,LDA2,LDB2)
		complex(kind=8),intent(in)::A(LDA1,LDA2),B(LDB1,LDB2)
		integer::i,j,k,l
		do l=1,LDB2
			do k=1,LDA2
				do j=1,LDB1
					do i=1,LDA1
						outdata(i,j,k,l)=A(i,k)*B(j,l)
					end do
				end do
			end do
		end do
		return
	end subroutine
	subroutine directProduct2_routine(R,A,B,LDA1,LDA2,LDB1,LDB2)
		integer,intent(in)::LDA1,LDA2,LDB1,LDB2
		type(TData),intent(inout)::R
		type(TData),intent(in)::A,B
		integer::flag,lenA,lenB
		flag=10*A%classtype+B%classType
		lenA=A%TotalData
		lenB=B%TotalData
		select case(flag)
			case(11)
				call directProduct2_int_routine(R%idata,A%idata,B%idata,LDA1,LDA2,LDB1,LDB2)
			case(12)
				call directProduct2_real4_routine(R%sdata,real(A%idata(1:lenA),kind=4),B%sdata,LDA1,LDA2,LDB1,LDB2)
			case(13)
				call directProduct2_real8_routine(R%ddata,dble(A%idata(1:lenA)),B%ddata,LDA1,LDA2,LDB1,LDB2)
			case(14)
				call directProduct2_com4_routine(R%cdata,cmplx(A%idata(1:lenA),kind=4),B%cdata,LDA1,LDA2,LDB1,LDB2)
			case(15)
				call directProduct2_com8_routine(R%zdata,dcmplx(A%idata(1:lenA)),B%zdata,LDA1,LDA2,LDB1,LDB2)
			
			case(21)
				call directProduct2_real4_routine(R%sdata,A%sdata,real(B%idata(1:lenB),kind=4),LDA1,LDA2,LDB1,LDB2)
			case(22)
				call directProduct2_real4_routine(R%sdata,A%sdata,B%sdata,LDA1,LDA2,LDB1,LDB2)
			case(23)
				call directProduct2_real8_routine(R%ddata,dble(A%sdata(1:lenA)),B%ddata,LDA1,LDA2,LDB1,LDB2)
			case(24)
				call directProduct2_com4_routine(R%cdata,cmplx(A%sdata(1:lenA),kind=4),B%cdata,LDA1,LDA2,LDB1,LDB2)
			case(25)
				call directProduct2_com8_routine(R%zdata,dcmplx(A%sdata(1:lenA)),B%zdata,LDA1,LDA2,LDB1,LDB2)
				
			case(31)
				call directProduct2_real8_routine(R%ddata,A%ddata,dble(B%idata(1:lenB)),LDA1,LDA2,LDB1,LDB2)
			case(32)
				call directProduct2_real8_routine(R%ddata,A%ddata,dble(B%sdata(1:lenB)),LDA1,LDA2,LDB1,LDB2)
			case(33)
				call directProduct2_real8_routine(R%ddata,A%ddata,B%ddata,LDA1,LDA2,LDB1,LDB2)
			case(34)
				call directProduct2_com8_routine(R%zdata,dcmplx(A%ddata(1:lenA)),dcmplx(B%cdata(1:lenB)),LDA1,LDA2,LDB1,LDB2)
			case(35)
				call directProduct2_com8_routine(R%zdata,dcmplx(A%ddata(1:lenA)),B%zdata,LDA1,LDA2,LDB1,LDB2)
				
			case(41)
				call directProduct2_com4_routine(R%cdata,A%cdata,cmplx(B%idata(1:lenB),kind=4),LDA1,LDA2,LDB1,LDB2)
			case(42)
				call directProduct2_com4_routine(R%cdata,A%cdata,cmplx(B%sdata(1:lenB),kind=4),LDA1,LDA2,LDB1,LDB2)
			case(43)
				call directProduct2_com8_routine(R%zdata,dcmplx(A%cdata(1:lenA)),dcmplx(B%ddata(1:lenB)),LDA1,LDA2,LDB1,LDB2)
			case(44)
				call directProduct2_com4_routine(R%cdata,A%cdata,B%cdata,LDA1,LDA2,LDB1,LDB2)
			case(45)
				call directProduct2_com8_routine(R%zdata,dcmplx(A%cdata(1:lenA)),B%zdata,LDA1,LDA2,LDB1,LDB2)
				
			case(51)
				call directProduct2_com8_routine(R%zdata,A%zdata,dcmplx(B%idata(1:lenB)),LDA1,LDA2,LDB1,LDB2)
			case(52)
				call directProduct2_com8_routine(R%zdata,A%zdata,dcmplx(B%sdata(1:lenB)),LDA1,LDA2,LDB1,LDB2)
			case(53)
				call directProduct2_com8_routine(R%zdata,A%zdata,dcmplx(B%ddata(1:lenB)),LDA1,LDA2,LDB1,LDB2)
			case(54)
				call directProduct2_com8_routine(R%zdata,A%zdata,dcmplx(B%cdata(1:lenB)),LDA1,LDA2,LDB1,LDB2)
			case(55)
				call directProduct2_com8_routine(R%zdata,A%zdata,B%zdata,LDA1,LDA2,LDB1,LDB2)
		end select
		return
	end subroutine
	
	subroutine directProductTensor_routine(R,A,B)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A,B
		integer::flag,lenA,lenB
		integer::i,j,k
		flag=10*A%classtype+B%classType
		lenA=A%TotalData
		lenB=B%TotalData
		select case(flag)
			case(11)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%idata(k)=A%idata(i)*B%idata(j)
						k=k+1
					end do
				end do
			case(12)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%sdata(k)=A%idata(i)*B%sdata(j)
						k=k+1
					end do
				end do
			case(13)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%ddata(k)=A%idata(i)*B%ddata(j)
						k=k+1
					end do
				end do
			case(14)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%cdata(k)=A%idata(i)*B%cdata(j)
						k=k+1
					end do
				end do
			case(15)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%idata(i)*B%zdata(j)
						k=k+1
					end do
				end do
				
			case(21)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%sdata(k)=A%sdata(i)*B%idata(j)
						k=k+1
					end do
				end do
			case(22)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%sdata(k)=A%sdata(i)*B%sdata(j)
						k=k+1
					end do
				end do
			case(23)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%ddata(k)=A%sdata(i)*B%ddata(j)
						k=k+1
					end do
				end do
			case(24)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%cdata(k)=A%sdata(i)*B%cdata(j)
						k=k+1
					end do
				end do
			case(25)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%sdata(i)*B%zdata(j)
						k=k+1
					end do
				end do
			
			case(31)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%ddata(k)=A%ddata(i)*B%idata(j)
						k=k+1
					end do
				end do
			case(32)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%ddata(k)=A%ddata(i)*B%sdata(j)
						k=k+1
					end do
				end do
			case(33)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%ddata(k)=A%ddata(i)*B%ddata(j)
						k=k+1
					end do
				end do
			case(34)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%ddata(i)*B%cdata(j)
						k=k+1
					end do
				end do
			case(35)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%ddata(i)*B%zdata(j)
						k=k+1
					end do
				end do
			
			case(41)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%cdata(k)=A%cdata(i)*B%idata(j)
						k=k+1
					end do
				end do
			case(42)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%cdata(k)=A%cdata(i)*B%sdata(j)
						k=k+1
					end do
				end do
			case(43)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%cdata(i)*B%ddata(j)
						k=k+1
					end do
				end do
			case(44)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%cdata(k)=A%cdata(i)*B%cdata(j)
						k=k+1
					end do
				end do
			case(45)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%cdata(i)*B%zdata(j)
						k=k+1
					end do
				end do
				
			case(51)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%zdata(i)*B%idata(j)
						k=k+1
					end do
				end do
			case(52)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%zdata(i)*B%sdata(j)
						k=k+1
					end do
				end do
			case(53)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%zdata(i)*B%ddata(j)
						k=k+1
					end do
				end do
			case(54)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%zdata(i)*B%cdata(j)
						k=k+1
					end do
				end do
			case(55)
				k=1
				do j=1,lenB
					do i=1,lenA
						R%zdata(k)=A%zdata(i)*B%zdata(j)
						k=k+1
					end do
				end do	
		end select
		return
	end subroutine


	subroutine directProduct1_int_routine(outdata,A,B,m,n)
		integer,intent(in)::m,n
		integer,intent(inout)::outdata(m,n)
		integer,intent(in)::A(m),B(n)
		integer::k,l
		do l=1,n
			do k=1,m
				outdata(k,l)=A(k)*B(l)
			end do
		end do
		return
	end subroutine
	subroutine directProduct1_real4_routine(outdata,A,B,m,n)
		integer,intent(in)::m,n
		real(kind=4),intent(inout)::outdata(m,n)
		real(kind=4),intent(in)::A(m),B(n)
		integer::k,l
		do l=1,n
			do k=1,m
				outdata(k,l)=A(k)*B(l)
			end do
		end do
		return
	end subroutine
	subroutine directProduct1_real8_routine(outdata,A,B,m,n)
		integer,intent(in)::m,n
		real(kind=8),intent(inout)::outdata(m,n)
		real(kind=8),intent(in)::A(m),B(n)
		integer::k,l
		do l=1,n
			do k=1,m
				outdata(k,l)=A(k)*B(l)
			end do
		end do
		return
	end subroutine
	subroutine directProduct1_com4_routine(outdata,A,B,m,n)
		integer,intent(in)::m,n
		complex(kind=4),intent(inout)::outdata(m,n)
		complex(kind=4),intent(in)::A(m),B(n)
		integer::k,l
		do l=1,n
			do k=1,m
				outdata(k,l)=A(k)*B(l)
			end do
		end do
		return
	end subroutine
	subroutine directProduct1_com8_routine(outdata,A,B,m,n)
		integer,intent(in)::m,n
		complex(kind=8),intent(inout)::outdata(m,n)
		complex(kind=8),intent(in)::A(m),B(n)
		integer::k,l
		do l=1,n
			do k=1,m
				outdata(k,l)=A(k)*B(l)
			end do
		end do
		return
	end subroutine
	subroutine directProduct1_routine(R,A,B,LDA,LDB)
		integer,intent(in)::LDA,LDB
		type(TData),intent(inout)::R
		type(TData),intent(in)::A,B
		integer::flag,lenA,lenB
		flag=10*A%classtype+B%classType
		lenA=A%TotalData
		lenB=B%TotalData
		select case(flag)
			case(11)
				call directProduct1_int_routine(R%idata,A%idata,B%idata,LDA,LDB)
			case(12)
				call directProduct1_real4_routine(R%sdata,real(A%idata(1:lenA),kind=4),B%sdata,LDA,LDB)
			case(13)
				call directProduct1_real8_routine(R%ddata,dble(A%idata(1:lenA)),B%ddata,LDA,LDB)
			case(14)
				call directProduct1_com4_routine(R%cdata,cmplx(A%idata(1:lenA),kind=4),B%cdata,LDA,LDB)
			case(15)
				call directProduct1_com8_routine(R%zdata,dcmplx(A%idata(1:lenA)),B%zdata,LDA,LDB)
			
			case(21)
				call directProduct1_real4_routine(R%sdata,A%sdata,real(B%idata(1:lenB),kind=4),LDA,LDB)
			case(22)
				call directProduct1_real4_routine(R%sdata,A%sdata,B%sdata,LDA,LDB)
			case(23)
				call directProduct1_real8_routine(R%ddata,dble(A%sdata(1:lenA)),B%ddata,LDA,LDB)
			case(24)
				call directProduct1_com4_routine(R%cdata,cmplx(A%sdata(1:lenA),kind=4),B%cdata,LDA,LDB)
			case(25)
				call directProduct1_com8_routine(R%zdata,dcmplx(A%sdata(1:lenA)),B%zdata,LDA,LDB)
				
			case(31)
				call directProduct1_real8_routine(R%ddata,A%ddata,dble(B%idata(1:lenB)),LDA,LDB)
			case(32)
				call directProduct1_real8_routine(R%ddata,A%ddata,dble(B%sdata(1:lenB)),LDA,LDB)
			case(33)
				call directProduct1_real8_routine(R%ddata,A%ddata,B%ddata,LDA,LDB)
			case(34)
				call directProduct1_com8_routine(R%zdata,dcmplx(A%ddata(1:lenA)),dcmplx(B%cdata(1:lenB)),LDA,LDB)
			case(35)
				call directProduct1_com8_routine(R%zdata,dcmplx(A%ddata(1:lenA)),B%zdata,LDA,LDB)
				
			case(41)
				call directProduct1_com4_routine(R%cdata,A%cdata,cmplx(B%idata(1:lenB),kind=4),LDA,LDB)
			case(42)
				call directProduct1_com4_routine(R%cdata,A%cdata,cmplx(B%sdata(1:lenB),kind=4),LDA,LDB)
			case(43)
				call directProduct1_com8_routine(R%zdata,dcmplx(A%cdata(1:lenA)),dcmplx(B%ddata(1:lenB)),LDA,LDB)
			case(44)
				call directProduct1_com4_routine(R%cdata,A%cdata,B%cdata,LDA,LDB)
			case(45)
				call directProduct1_com8_routine(R%zdata,dcmplx(A%cdata(1:lenA)),B%zdata,LDA,LDB)
				
			case(51)
				call directProduct1_com8_routine(R%zdata,A%zdata,dcmplx(B%idata(1:lenB)),LDA,LDB)
			case(52)
				call directProduct1_com8_routine(R%zdata,A%zdata,dcmplx(B%sdata(1:lenB)),LDA,LDB)
			case(53)
				call directProduct1_com8_routine(R%zdata,A%zdata,dcmplx(B%ddata(1:lenB)),LDA,LDB)
			case(54)
				call directProduct1_com8_routine(R%zdata,A%zdata,dcmplx(B%cdata(1:lenB)),LDA,LDB)
			case(55)
				call directProduct1_com8_routine(R%zdata,A%zdata,B%zdata,LDA,LDB)
		end select
		return
	end subroutine
	
!Htranspose	
	subroutine Htranspose_subroutine_com8(outdata,indata,m,n)
		integer,intent(in)::m,n
		complex(kind=8),intent(inout) :: outdata(n,m)
		complex(kind=8),intent(in)::indata(m,n)
		outdata=transpose(dconjg(indata))
		return
	end subroutine
	subroutine Htranspose_subroutine_com4(outdata,indata,m,n)
		integer,intent(in)::m,n
		complex(kind=4),intent(inout) :: outdata(n,m)
		complex(kind=4),intent(in)::indata(m,n)
		outdata=transpose(conjg(indata))
		return
	end subroutine
	subroutine transpose_subroutine_com8(outdata,indata,m,n)
		integer,intent(in)::m,n
		complex(kind=8),intent(inout) :: outdata(n,m)
		complex(kind=8),intent(in)::indata(m,n)
		outdata=transpose(indata)
		return
	end subroutine
	subroutine transpose_subroutine_com4(outdata,indata,m,n)
		integer,intent(in)::m,n
		complex(kind=4),intent(inout) :: outdata(n,m)
		complex(kind=4),intent(in)::indata(m,n)
		outdata=transpose(indata)
		return
	end subroutine
	subroutine transpose_subroutine_real8(outdata,indata,m,n)
		integer,intent(in)::m,n
		real(kind=8),intent(inout) :: outdata(n,m)
		real(kind=8),intent(in)::indata(m,n)
		outdata=transpose(indata)
		return
	end subroutine
	subroutine transpose_subroutine_real4(outdata,indata,m,n)
		integer,intent(in)::m,n
		real(kind=4),intent(inout) :: outdata(n,m)
		real(kind=4),intent(in)::indata(m,n)
		outdata=transpose(indata)
		return
	end subroutine
	subroutine transpose_subroutine_int(outdata,indata,m,n)
		integer,intent(in)::m,n
		integer,intent(inout) :: outdata(n,m)
		integer,intent(in)::indata(m,n)
		outdata=transpose(indata)
		return
	end subroutine

	subroutine Htranspose_subroutine(R,A,m,n)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		integer,intent(in)::m,n
		select case(A%classType)
			case(1)
				call transpose_subroutine_int(R%idata,A%idata,m,n)
			case(2)
				call transpose_subroutine_real4(R%sdata,A%sdata,m,n)
			case(3)
				call transpose_subroutine_real8(R%ddata,A%ddata,m,n)
			case(4)
				call Htranspose_subroutine_com4(R%cdata,A%cdata,m,n)
			case(5)
				call Htranspose_subroutine_com8(R%zdata,A%zdata,m,n)
		end select
		return
	end subroutine
	subroutine transpose_subroutine(R,A,m,n)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		integer,intent(in)::m,n
		select case(A%classType)
			case(1)
				call transpose_subroutine_int(R%idata,A%idata,m,n)
			case(2)
				call transpose_subroutine_real4(R%sdata,A%sdata,m,n)
			case(3)
				call transpose_subroutine_real8(R%ddata,A%ddata,m,n)
			case(4)
				call transpose_subroutine_com4(R%cdata,A%cdata,m,n)
			case(5)
				call transpose_subroutine_com8(R%zdata,A%zdata,m,n)
		end select
		return
	end subroutine
	
	subroutine conjugate_subroutine(R,A)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		select case(A%classType)
			case(1)
				R%idata=A%idata(1:A%totalData)
			case(2)
				call scopy(A%TotalData, A%sData, 1, R%sdata, 1)
			case(3)
				call dcopy(A%TotalData, A%dData, 1, R%ddata, 1)
			case(4)
				R%cdata=conjg(A%cdata(1:A%totalData))
			case(5)
				R%zdata=dconjg(A%zdata(1:A%totalData))
		end select
		return
	end subroutine
!*******************************************************8
!			subTensor

	subroutine subTen_TData_routine1_(A,R,length,istart,iend)
		integer,intent(in)::istart,iend,length
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		select case(A%classType)
			case(1)
				R%idata=A%idata(istart:iend)
			case(2)
				call scopy (length, A%sdata(istart:iend), 1, R%sdata, 1)
			case(3)
				call dcopy (length, A%ddata(istart:iend), 1, R%ddata, 1)
			case(4)
				call ccopy (length, A%cdata(istart:iend) ,1, R%cdata, 1)
			case(5)
				call zcopy (length, A%zdata(istart:iend), 1, R%zdata, 1)
			case(6)
				R%ldata=A%ldata(istart:iend)
			case(7)
				R%adata=A%adata(istart:iend)
		end select
		return
	end subroutine
	
	subroutine subTen_TData_routine1(A,R,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		select case(A%classType)
			case(1)
				call  subTen_TData_routine1_int(A%idata,R%idata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
			case(2)
				call  subTen_TData_routine1_real4(A%sdata,R%sdata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
			case(3)
				call  subTen_TData_routine1_real8(A%ddata,R%ddata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
			case(4)
				call  subTen_TData_routine1_com4(A%cdata,R%cdata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
			case(5)
				call  subTen_TData_routine1_com8(A%zdata,R%zdata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
			case(6)
				call  subTen_TData_routine1_logi(A%ldata,R%ldata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
			case(7)
				call  subTen_TData_routine1_char(A%adata,R%adata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
		end select
		return
	end subroutine
	subroutine subTen_TData_routine2(A,R,TLD1,TLD2,OLD1,iindex,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,iindex
		logical,intent(in)::flag
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		select case(A%classType)
			case(1)
				call  subTen_TData_routine2_int(A%idata,R%idata,TLD1,TLD2,OLD1,iindex,flag)
			case(2)
				call  subTen_TData_routine2_real4(A%sdata,R%sdata,TLD1,TLD2,OLD1,iindex,flag)
			case(3)
				call  subTen_TData_routine2_real8(A%ddata,R%ddata,TLD1,TLD2,OLD1,iindex,flag)
			case(4)
				call  subTen_TData_routine2_com4(A%cdata,R%cdata,TLD1,TLD2,OLD1,iindex,flag)
			case(5)
				call  subTen_TData_routine2_com8(A%zdata,R%zdata,TLD1,TLD2,OLD1,iindex,flag)
			case(6)
				call  subTen_TData_routine2_logi(A%ldata,R%ldata,TLD1,TLD2,OLD1,iindex,flag)
			case(7)
				call  subTen_TData_routine2_char(A%adata,R%adata,TLD1,TLD2,OLD1,iindex,flag)
		end select
		return
	end subroutine
	subroutine subTen_TData_routine3(A,R,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend
		type(TData),intent(inout)::R
		type(TData),intent(in)::A
		logical,intent(in)::flag
		select case(A%classType)
			case(1)
				call  subTen_TData_routine3_int(A%idata,R%idata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
			case(2)
				call  subTen_TData_routine3_real4(A%sdata,R%sdata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
			case(3)
				call  subTen_TData_routine3_real8(A%ddata,R%ddata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
			case(4)
				call  subTen_TData_routine3_com4(A%cdata,R%cdata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
			case(5)
				call  subTen_TData_routine3_com8(A%zdata,R%zdata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
			case(6)
				call  subTen_TData_routine3_logi(A%ldata,R%ldata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
			case(7)
				call  subTen_TData_routine3_char(A%adata,R%adata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
		end select
		return
	end subroutine
	subroutine subTen_TData_routine1_com8(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend
		complex(kind=8),intent(in)::Tensordata(TLD1,TLD2)
		complex(kind=8),intent(inout)::outdata(OLD1,OLD2)
		outdata=Tensordata(istart:iend,jstart:jend)
		return
	end subroutine
	subroutine subTen_TData_routine1_com4(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend
		complex(kind=4),intent(in)::Tensordata(TLD1,TLD2)
		complex(kind=4),intent(inout)::outdata(OLD1,OLD2)
		outdata=Tensordata(istart:iend,jstart:jend)
		return
	end subroutine
	subroutine subTen_TData_routine1_real8(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend
		real(kind=8),intent(in)::Tensordata(TLD1,TLD2)
		real(kind=8),intent(inout)::outdata(OLD1,OLD2)
		outdata=Tensordata(istart:iend,jstart:jend)
		return
	end subroutine
	subroutine subTen_TData_routine1_real4(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend
		real(kind=4),intent(in)::Tensordata(TLD1,TLD2)
		real(kind=4),intent(inout)::outdata(OLD1,OLD2)
		outdata=Tensordata(istart:iend,jstart:jend)
		return
	end subroutine
	subroutine subTen_TData_routine1_int(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend
		integer,intent(in)::Tensordata(TLD1,TLD2)
		integer,intent(inout)::outdata(OLD1,OLD2)
		outdata=Tensordata(istart:iend,jstart:jend)
		return
	end subroutine
	subroutine subTen_TData_routine1_logi(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend
		logical,intent(in)::Tensordata(TLD1,TLD2)
		logical,intent(inout)::outdata(OLD1,OLD2)
		outdata=Tensordata(istart:iend,jstart:jend)
		return
	end subroutine
	subroutine subTen_TData_routine1_char(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend,jstart,jend
		character(len=*),intent(in)::Tensordata(TLD1,TLD2)
		character(len=*),intent(inout)::outdata(OLD1,OLD2)
		outdata=Tensordata(istart:iend,jstart:jend)
		return
	end subroutine
	
	subroutine subTen_TData_routine2_com8(Tensordata,outdata,TLD1,TLD2,OLD1,iindex,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,iindex
		complex(kind=8),intent(in)::Tensordata(TLD1,TLD2)
		complex(kind=8),intent(inout)::outdata(OLD1)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(iindex,:)
		else
			outdata=Tensordata(:,iindex)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine2_com4(Tensordata,outdata,TLD1,TLD2,OLD1,iindex,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,iindex
		complex(kind=4),intent(in)::Tensordata(TLD1,TLD2)
		complex(kind=4),intent(inout)::outdata(OLD1)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(iindex,:)
		else
			outdata=Tensordata(:,iindex)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine2_real8(Tensordata,outdata,TLD1,TLD2,OLD1,iindex,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,iindex
		real(kind=8),intent(in)::Tensordata(TLD1,TLD2)
		real(kind=8),intent(inout)::outdata(OLD1)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(iindex,:)
		else
			outdata=Tensordata(:,iindex)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine2_real4(Tensordata,outdata,TLD1,TLD2,OLD1,iindex,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,iindex
		real(kind=4),intent(in)::Tensordata(TLD1,TLD2)
		real(kind=4),intent(inout)::outdata(OLD1)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(iindex,:)
		else
			outdata=Tensordata(:,iindex)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine2_int(Tensordata,outdata,TLD1,TLD2,OLD1,iindex,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,iindex
		integer,intent(in)::Tensordata(TLD1,TLD2)
		integer,intent(inout)::outdata(OLD1)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(iindex,:)
		else
			outdata=Tensordata(:,iindex)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine2_logi(Tensordata,outdata,TLD1,TLD2,OLD1,iindex,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,iindex
		logical,intent(in)::Tensordata(TLD1,TLD2)
		logical,intent(inout)::outdata(OLD1)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(iindex,:)
		else
			outdata=Tensordata(:,iindex)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine2_char(Tensordata,outdata,TLD1,TLD2,OLD1,iindex,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,iindex
		character(len=*),intent(in)::Tensordata(TLD1,TLD2)
		character(len=*),intent(inout)::outdata(OLD1)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(iindex,:)
		else
			outdata=Tensordata(:,iindex)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine3_com8(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend
		complex(kind=8),intent(in)::Tensordata(TLD1,TLD2)
		complex(kind=8),intent(inout)::outdata(OLD1,OLD2)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(istart:iend,:)
		else
			outdata=Tensordata(:,istart:iend)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine3_com4(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend
		complex(kind=4),intent(in)::Tensordata(TLD1,TLD2)
		complex(kind=4),intent(inout)::outdata(OLD1,OLD2)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(istart:iend,:)
		else
			outdata=Tensordata(:,istart:iend)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine3_real8(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend
		real(kind=8),intent(in)::Tensordata(TLD1,TLD2)
		real(kind=8),intent(inout)::outdata(OLD1,OLD2)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(istart:iend,:)
		else
			outdata=Tensordata(:,istart:iend)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine3_real4(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend
		real(kind=4),intent(in)::Tensordata(TLD1,TLD2)
		real(kind=4),intent(inout)::outdata(OLD1,OLD2)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(istart:iend,:)
		else
			outdata=Tensordata(:,istart:iend)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine3_int(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend
		integer,intent(in)::Tensordata(TLD1,TLD2)
		integer,intent(inout)::outdata(OLD1,OLD2)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(istart:iend,:)
		else
			outdata=Tensordata(:,istart:iend)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine3_logi(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend
		logical,intent(in)::Tensordata(TLD1,TLD2)
		logical,intent(inout)::outdata(OLD1,OLD2)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(istart:iend,:)
		else
			outdata=Tensordata(:,istart:iend)
		end if
		return
	end subroutine
	subroutine subTen_TData_routine3_char(Tensordata,outdata,TLD1,TLD2,OLD1,OLD2,istart,iend,flag)
		integer,intent(in)::TLD1,TLD2,OLD1,OLD2,istart,iend
		character(len=*),intent(in)::Tensordata(TLD1,TLD2)
		character(len=*),intent(inout)::outdata(OLD1,OLD2)
		logical,intent(in)::flag
		if(flag)then
			outdata=Tensordata(istart:iend,:)
		else
			outdata=Tensordata(:,istart:iend)
		end if
		return
	end subroutine
	
	subroutine combinationCol_TData(R,A,B)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A,B
		integer::lengthA,lengthB
		lengthA=A%TotalData
		lengthB=B%TotalData
		select case(R%classtype)
			case(1)
				R%idata(:lengthA)=A%idata(1:lengthA)
				R%idata(lengthA+1:)=B%idata(1:lengthB)
			case(2)
				call assignment_real4_Tdata(R%sdata,A,lengthA)
				call assignment_real4_Tdata(R%sdata(lengthA+1:),B,lengthB)
			case(3)
				call assignment_real8_Tdata(R%ddata,A,lengthA)
				call assignment_real8_Tdata(R%ddata(lengthA+1:),B,lengthB)
			case(4)
				call assignment_com4_Tdata(R%cdata,A,lengthA)
				call assignment_com4_Tdata(R%cdata(lengthA+1:),B,lengthB)
			case(5)
				call assignment_com8_Tdata(R%zdata,A,lengthA)
				call assignment_com8_Tdata(R%zdata(lengthA+1:),B,lengthB)	
			case(6)
				call assignment_logi_Tdata(R%ldata,A,lengthA)
				call assignment_logi_Tdata(R%ldata(lengthA+1:),B,lengthB)		
			case(7)
				call assignment_char_Tdata(R%adata,A,lengthA)
				call assignment_char_Tdata(R%adata(lengthA+1:),B,lengthB)	
		end select
		return
	end subroutine
	subroutine combinationRow_TData(R,A,B,LDR,LDR2,LDA,LDB)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A,B
		integer,intent(in)::LDR,LDR2,LDA,LDB
		real(kind=4),allocatable::sdata(:)
		real(kind=8),allocatable::ddata(:)
		complex(kind=4),allocatable::cdata(:)
		complex(kind=8),allocatable::zdata(:),zdata2(:)
		character(len=max_len_of_char_in_TData),allocatable::adata(:)
		integer::lengthA,lengthB,comflag
		lengthA=A%TotalData
		lengthB=B%TotalData
		select case(R%classtype)
			case(1)
				call combinationRow_TData_int(R%idata,A%idata,B%idata,LDR,LDR2,LDA,LDB)
			case(2)
				if(A%classType.ne.R%classtype)then
					allocate(sdata(lengthA))
					call assignment_real4_Tdata(sdata,A,lengthA)
					call combinationRow_TData_real4(R%sdata,sdata,B%sdata,LDR,LDR2,LDA,LDB)
				else if(B%classType.ne.R%classtype) then
					allocate(sdata(lengthB))
					call assignment_real4_Tdata(sdata,B,lengthB)
					call combinationRow_TData_real4(R%sdata,A%sdata,sdata,LDR,LDR2,LDA,LDB)
				else
					call combinationRow_TData_real4(R%sdata,A%sdata,B%sdata,LDR,LDR2,LDA,LDB)
				end if
			case(3)
				if(A%classType.ne.R%classtype)then
					allocate(ddata(lengthA))
					call assignment_real8_Tdata(ddata,A,lengthA)
					call combinationRow_TData_real8(R%ddata,ddata,B%ddata,LDR,LDR2,LDA,LDB)
				else if(B%classType.ne.R%classtype) then
					allocate(ddata(lengthB))
					call assignment_real8_Tdata(ddata,B,lengthB)
					call combinationRow_TData_real8(R%ddata,A%ddata,ddata,LDR,LDR2,LDA,LDB)
				else
					call combinationRow_TData_real8(R%ddata,A%ddata,B%ddata,LDR,LDR2,LDA,LDB)
				end if
			case(4)
				if(A%classType.ne.R%classtype)then
					allocate(cdata(lengthA))
					call assignment_com4_Tdata(cdata,A,lengthA)
					call combinationRow_TData_com4(R%cdata,cdata,B%cdata,LDR,LDR2,LDA,LDB)
				else if(B%classType.ne.R%classtype) then
					allocate(cdata(lengthB))
					call assignment_com4_Tdata(cdata,B,lengthB)
					call combinationRow_TData_com4(R%cdata,A%cdata,cdata,LDR,LDR2,LDA,LDB)
				else
					call combinationRow_TData_com4(R%cdata,A%cdata,B%cdata,LDR,LDR2,LDA,LDB)
				end if
			case(5)
				comflag=0
				if(A%classType.ne.R%classtype)then
					allocate(zdata(lengthA))
					call assignment_com8_Tdata(zdata,A,lengthA)
					comflag=1
				end if
				if(B%classType.ne.R%classtype) then
					allocate(zdata2(lengthB))
					call assignment_com8_Tdata(zdata2,B,lengthB)
					if(comflag.eq.1)then
						comflag=3
					else
						comflag=2
					end if
				end if
				select case(comflag)
					case (0)
						call combinationRow_TData_com8(R%zdata,A%zdata,B%zdata,LDR,LDR2,LDA,LDB)
					case (1)
						call combinationRow_TData_com8(R%zdata,zdata,B%zdata,LDR,LDR2,LDA,LDB)
					case (2)
						call combinationRow_TData_com8(R%zdata,A%zdata,zdata2,LDR,LDR2,LDA,LDB)
					case (3)
						call combinationRow_TData_com8(R%zdata,zdata,zdata2,LDR,LDR2,LDA,LDB)
				end select
			case(6)
				call combinationRow_TData_logi(R%ldata,A%ldata,B%ldata,LDR,LDR2,LDA,LDB)
			case(7)
				if(A%classType.ne.R%classtype)then
					allocate(adata(lengthA))
					call assignment_char_Tdata(adata,A,lengthA)
					call combinationRow_TData_char(R%adata,adata,B%adata,LDR,LDR2,LDA,LDB)
				else if(B%classType.ne.R%classtype) then
					allocate(adata(lengthB))
					call assignment_char_Tdata(adata,B,lengthB)
					call combinationRow_TData_char(R%adata,A%adata,adata,LDR,LDR2,LDA,LDB)
				else
					call combinationRow_TData_char(R%adata,A%adata,B%adata,LDR,LDR2,LDA,LDB)
				end if
		end select
		return
	end subroutine
	subroutine combinationRow_TData_int(Rdata,Adata,Bdata,LDR,LDR2,LDA,LDB)
		integer,intent(in)::LDR,LDA,LDB,LDR2
		integer,intent(inout)::RData(LDR,LDR2)
		integer,intent(in)::AData(LDA,*)
		integer,intent(in)::BData(LDB,*)
		RData(1:LDA,:)=AData(:,:LDR2)
		RData(LDA+1:,:)=BData(:,:LDR2)
		return
	end subroutine
	subroutine combinationRow_TData_real4(Rdata,Adata,Bdata,LDR,LDR2,LDA,LDB)
		integer,intent(in)::LDR,LDA,LDB,LDR2
		real(kind=4),intent(inout)::RData(LDR,LDR2)
		real(kind=4),intent(in)::AData(LDA,*)
		real(kind=4),intent(in)::BData(LDB,*)
		RData(1:LDA,:)=AData(:,:LDR2)
		RData(LDA+1:,:)=BData(:,:LDR2)
		return
	end subroutine
	subroutine combinationRow_TData_real8(Rdata,Adata,Bdata,LDR,LDR2,LDA,LDB)
		integer,intent(in)::LDR,LDA,LDB,LDR2
		real(kind=8),intent(inout)::RData(LDR,LDR2)
		real(kind=8),intent(in)::AData(LDA,*)
		real(kind=8),intent(in)::BData(LDB,*)
		RData(1:LDA,:)=AData(:,:LDR2)
		RData(LDA+1:,:)=BData(:,:LDR2)
		return
	end subroutine
	subroutine combinationRow_TData_com4(Rdata,Adata,Bdata,LDR,LDR2,LDA,LDB)
		integer,intent(in)::LDR,LDA,LDB,LDR2
		complex(kind=4),intent(inout)::RData(LDR,LDR2)
		complex(kind=4),intent(in)::AData(LDA,*)
		complex(kind=4),intent(in)::BData(LDB,*)
		RData(1:LDA,:)=AData(:,:LDR2)
		RData(LDA+1:,:)=BData(:,:LDR2)
		return
	end subroutine
	subroutine combinationRow_TData_com8(Rdata,Adata,Bdata,LDR,LDR2,LDA,LDB)
		integer,intent(in)::LDR,LDA,LDB,LDR2
		complex(kind=8),intent(inout)::RData(LDR,LDR2)
		complex(kind=8),intent(in)::AData(LDA,*)
		complex(kind=8),intent(in)::BData(LDB,*)
		RData(1:LDA,:)=AData(:,:LDR2)
		RData(LDA+1:,:)=BData(:,:LDR2)
		return
	end subroutine
	subroutine combinationRow_TData_logi(Rdata,Adata,Bdata,LDR,LDR2,LDA,LDB)
		integer,intent(in)::LDR,LDA,LDB,LDR2
		logical,intent(inout)::RData(LDR,LDR2)
		logical,intent(in)::AData(LDA,*)
		logical,intent(in)::BData(LDB,*)
		RData(1:LDA,:)=AData(:,:LDR2)
		RData(LDA+1:,:)=BData(:,:LDR2)
		return
	end subroutine
	subroutine combinationRow_TData_char(Rdata,Adata,Bdata,LDR,LDR2,LDA,LDB)
		integer,intent(in)::LDR,LDA,LDB,LDR2
		character(len=*),intent(inout)::RData(LDR,LDR2)
		character(len=*),intent(in)::AData(LDA,*)
		character(len=*),intent(in)::BData(LDB,*)
		RData(1:LDA,:)=AData(:,:LDR2)
		RData(LDA+1:,:)=BData(:,:LDR2)
		return
	end subroutine
!**************************************************************************************************************
!
!                                  lapack function
!
!**************************************************************************************************************
!**************************************************************************************************************	


	subroutine SVD_TData_routine_SVD(indata,U,S,V,m,n,min_MN,cut,info)
		integer,intent(in)::min_MN
		type(TData),intent(in)::indata
		type(TData),intent(inout)::U,V
		type(TData),intent(inout)::s
		integer,intent(in),optional::cut,info
		complex*16,pointer :: zwork(:),zdata(:),uzdata(:,:),vzdata(:,:)
		complex*8,pointer :: cwork(:),cdata(:),ucdata(:,:),vcdata(:,:)
		real*8,pointer :: dwork(:),ddata(:),uddata(:,:),vddata(:,:),sdata8(:)
		real*4,pointer :: swork(:),sdata(:),usdata(:,:),vsdata(:,:),sdata4(:)
		integer m,n,lw,rwdim,i,j
		integer,pointer :: iw(:)
		real*8,pointer :: drw(:)
		real*4,pointer :: srw(:)
		integer :: ms_max,max_MN,length,cut2
		integer::totallengthofmemory
		call WorkingMemory%check()
		length=indata%TotalData
		max_MN=max(M,N)
		lw=min_MN*min_MN+2*min_MN+max_MN
		rwdim=min_MN*max(5*min_MN+7,2*max_MN+2*min_MN+1)
		select case(indata%classType)
			case (5)
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n
					call WorkingMemory%allocate(5,totallengthofmemory)
					totallengthofmemory=rwdim+min_MN
					call WorkingMemory%allocate(3,totallengthofmemory)
					call WorkingMemory%allocate(1,8*min_MN)
				end if
				!allocate(drw(rwdim))
				!allocate(iw(8*min_MN))
				!allocate(zwork(lw))
				!allocate(zdata(length))
				call WorkingMemory%get_memory(drw,rwdim)
				call WorkingMemory%get_memory(iw,8*min_MN)
				call WorkingMemory%get_memory(zwork,lw)
				call WorkingMemory%get_memory(zdata,length)
				call zcopy (length, indata%zdata, 1, zdata, 1)
				if(present(cut))then
					cut2=min(cut,min_MN)
					!allocate(uzdata(m,min_MN))
					!allocate(vzdata(min_MN,n))
					!allocate(sdata8(min_MN)) 
					call WorkingMemory%get_memory(uzdata,m,min_MN)
					call WorkingMemory%get_memory(vzdata,min_MN,n)
					call WorkingMemory%get_memory(sdata8,min_MN)
					call ZGESVD('S','S',m,n,zdata,m,sdata8,uzdata,m,vzdata,min_MN,zWORK,lw,drw,INFO)
					s%ddata=sdata8(1:cut2)
					call cut_off_routine_com8(u%zdata,m,cut2,uzdata,m,min_MN)
					call cut_off_routine_com8(V%zdata,cut2,n,Vzdata,min_MN,n)
				else
					call ZGESVD('S','S',m,n,zdata,m,S%ddata,U%zdata,m,V%zdata,min_MN,zWORK,lw,drw,INFO)
				end if
			case(4)
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n
					call WorkingMemory%allocate(4,totallengthofmemory)
					totallengthofmemory=rwdim+min_MN
					call WorkingMemory%allocate(2,totallengthofmemory)
					call WorkingMemory%allocate(1,8*min_MN)
				end if
				!allocate(cdata(length))
				!allocate(srw(rwdim))
				!allocate(iw(8*min_MN))
				!allocate(cwork(lw))
				call WorkingMemory%get_memory(cdata,length)
				call WorkingMemory%get_memory(srw,rwdim)
				call WorkingMemory%get_memory(iw,8*min_MN)
				call WorkingMemory%get_memory(cwork,lw)
				call ccopy (length, indata%cdata, 1, cdata, 1)
				if(present(cut))then
					cut2=min(cut,min_MN)
					!allocate(ucdata(m,min_MN))
					!allocate(vcdata(min_MN,n))
					!allocate(sdata4(min_MN)) 
					call WorkingMemory%get_memory(ucdata,m,min_MN)
					call WorkingMemory%get_memory(vcdata,min_MN,n)
					call WorkingMemory%get_memory(sdata4,min_MN)
					call CGESVD('S','S',m,n,cdata,m,sdata4,ucdata,m,vcdata,min_MN,cWORK,lw,srw,INFO)
					s%sdata=sdata4(1:cut2)
					call cut_off_routine_com4(u%cdata,m,cut2,ucdata,m,min_MN)
					call cut_off_routine_com4(V%cdata,cut2,n,Vcdata,min_MN,n)
				else
					call CGESVD('S','S',m,n,cdata,m,S%sdata,U%cdata,m,V%cdata,min_MN,cWORK,lw,srw,INFO)
				end if
			case(3)
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n+rwdim+min_MN
					call WorkingMemory%allocate(3,totallengthofmemory)
					call WorkingMemory%allocate(1,8*min_MN)
				end if
				call WorkingMemory%get_memory(ddata,length)
				call WorkingMemory%get_memory(drw,rwdim)
				call WorkingMemory%get_memory(iw,8*min_MN)
				call WorkingMemory%get_memory(dwork,lw)
				!allocate(ddata(length))
				!allocate(drw(rwdim))
				!allocate(iw(8*min_MN))
				!allocate(dwork(lw))
				call dcopy (length, indata%ddata, 1, ddata, 1)
				if(present(cut))then
					cut2=min(cut,min_MN)
					call WorkingMemory%get_memory(uddata,m,min_MN)
					call WorkingMemory%get_memory(vddata,min_MN,n)
					call WorkingMemory%get_memory(sdata8,min_MN)
					!allocate(uddata(m,min_MN))
					!allocate(vddata(min_MN,n))
					!allocate(sdata8(min_MN)) 
					call DGESVD('S','S',m,n,ddata,m,sdata8,uddata,m,vddata,min_MN,dWORK,lw,INFO)
					s%ddata=sdata8(1:cut2)
					call cut_off_routine_real8(u%ddata,m,cut2,uddata,m,min_MN)
					call cut_off_routine_real8(V%ddata,cut2,n,Vddata,min_MN,n)
				else
					call DGESVD('S','S',m,n,ddata,m,S%dData,U%dData,m,V%dData,min_MN,dWORK,lw,INFO)
				end if
			case(2)
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n+rwdim+min_MN
					call WorkingMemory%allocate(2,totallengthofmemory)
					call WorkingMemory%allocate(1,8*min_MN)
				end if
				call WorkingMemory%get_memory(sdata,length)
				call WorkingMemory%get_memory(srw,rwdim)
				call WorkingMemory%get_memory(iw,8*min_MN)
				call WorkingMemory%get_memory(swork,lw)
				!allocate(sdata(length))
				!allocate(srw(rwdim))
				!allocate(iw(8*min_MN))
				!allocate(swork(lw))
				call scopy (length, indata%sdata, 1, sdata, 1)
				if(present(cut))then
					cut2=min(cut,min_MN)
					call WorkingMemory%get_memory(usdata,m,min_MN)
					call WorkingMemory%get_memory(vsdata,min_MN,n)
					call WorkingMemory%get_memory(sdata4,min_MN)
					!allocate(usdata(m,min_MN))
					!allocate(vsdata(min_MN,n))
					!allocate(sdata4(min_MN)) 
					call SGESVD('S','S',m,n,sdata,m,sdata4,usdata,m,vsdata,min_MN,sWORK,lw,INFO)
					s%sdata=sdata4(1:cut2)
					call cut_off_routine_real4(u%sdata,m,cut2,usdata,m,min_MN)
					call cut_off_routine_real4(V%sdata,cut2,n,Vsdata,min_MN,n)
				else
					call SGESVD('S','S',m,n,sdata,m,S%sdata,U%sdata,m,V%sdata,min_MN,sWORK,lw,INFO)
				end if
			case(1)
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n+rwdim+min_MN
					call WorkingMemory%allocate(2,totallengthofmemory)
					call WorkingMemory%allocate(1,8*min_MN)
				end if
				call WorkingMemory%get_memory(sdata,length)
				call WorkingMemory%get_memory(srw,rwdim)
				call WorkingMemory%get_memory(iw,8*min_MN)
				call WorkingMemory%get_memory(swork,lw)
				!allocate(sdata(length))
				!allocate(srw(rwdim))
				!allocate(iw(8*min_MN))
				!allocate(swork(lw))
				sData=indata%idata
				if(present(cut))then
					cut2=min(cut,min_MN)
					call WorkingMemory%get_memory(usdata,m,min_MN)
					call WorkingMemory%get_memory(vsdata,min_MN,n)
					call WorkingMemory%get_memory(sdata4,min_MN)
					!allocate(usdata(m,min_MN))
					!allocate(vsdata(min_MN,n))
					!allocate(sdata4(min_MN)) 
					call SGESVD('S','S',m,n,sdata,m,sdata4,usdata,m,vsdata,min_MN,sWORK,lw,INFO)
					s%sdata=sdata4(1:cut2)
					call cut_off_routine_real4(u%sdata,m,cut2,usdata,m,min_MN)
					call cut_off_routine_real4(V%sdata,cut2,n,Vsdata,min_MN,n)
				else
					call SGESVD('S','S',m,n,sdata,m,S%sdata,U%sdata,m,V%sdata,min_MN,sWORK,lw,INFO)
				end if
		end select
		call WorkingMemory%free()
		return
	end subroutine

	subroutine SVD_TData_routine_SDD(indata,U,S,V,m,n,min_MN,cut,info)
		integer,intent(in)::min_MN
		type(TData),intent(in)::indata
		type(TData),intent(inout)::U,V
		type(TData),intent(inout)::s
		integer,intent(in),optional::cut,info
		complex*16,pointer :: zwork(:),zdata(:),uzdata(:,:),vzdata(:,:)
		complex*8,pointer :: cwork(:),cdata(:),ucdata(:,:),vcdata(:,:)
		real*8,pointer :: dwork(:),ddata(:),uddata(:,:),vddata(:,:),sdata8(:)
		real*4,pointer :: swork(:),sdata(:),usdata(:,:),vsdata(:,:),sdata4(:)
		integer m,n,lw,rwdim,i,j
		real*8,pointer :: drw(:)
		real*4,pointer :: srw(:)
		integer,pointer :: iwork(:)
		integer :: ms_max,max_MN,length,cut2
		integer::totallengthofmemory
		call WorkingMemory%check()
		length=indata%TotalData
		max_MN=max(M,N)
		select case(indata%classType)
			case (5)
				rwdim=5*min_MN*min_MN+7*min_MN
				lw=min_MN*min_MN+2*min_MN+max_MN
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n
					call WorkingMemory%allocate(5,totallengthofmemory)
					totallengthofmemory=rwdim+min_MN
					call WorkingMemory%allocate(3,totallengthofmemory)
					call WorkingMemory%allocate(1,16*min_MN)
				end if
				!allocate(drw(rwdim))
				!allocate(iw(8*min_MN))
				!allocate(zwork(lw))
				!allocate(zdata(length))
				call WorkingMemory%get_memory(drw,rwdim)
				call WorkingMemory%get_memory(zwork,lw)
				call WorkingMemory%get_memory(zdata,length)
				call WorkingMemory%get_memory(iwork,8*min_MN)
				call zcopy (length, indata%zdata, 1, zdata, 1)
				if(present(cut))then
					cut2=min(cut,min_MN)
					!allocate(uzdata(m,min_MN))
					!allocate(vzdata(min_MN,n))
					!allocate(sdata8(min_MN)) 
					call WorkingMemory%get_memory(uzdata,m,min_MN)
					call WorkingMemory%get_memory(vzdata,min_MN,n)
					call WorkingMemory%get_memory(sdata8,min_MN)
					call ZGESDD('S',m,n,zdata,m,sdata8,uzdata,m,vzdata,min_MN,zWORK,lw,drw,iwork,INFO)
					s%ddata=sdata8(1:cut2)
					call cut_off_routine_com8(u%zdata,m,cut2,uzdata,m,min_MN)
					call cut_off_routine_com8(V%zdata,cut2,n,Vzdata,min_MN,n)
				else
					call ZGESDD('S',m,n,zdata,m,S%ddata,U%zdata,m,V%zdata,min_MN,zWORK,lw,drw,iwork,INFO)
				end if
			case(4)
				rwdim=max(5*min_MN*min_MN+8*min_MN,2*max_MN*min_MN+2*min_MN*min_MN+min_MN)
				lw=min_MN*min_MN+4*min_MN
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n
					call WorkingMemory%allocate(4,totallengthofmemory)
					totallengthofmemory=rwdim+min_MN
					call WorkingMemory%allocate(2,totallengthofmemory)
					call WorkingMemory%allocate(1,16*min_MN)
				end if
				!allocate(cdata(length))
				!allocate(srw(rwdim))
				!allocate(iw(8*min_MN))
				!allocate(cwork(lw))
				call WorkingMemory%get_memory(cdata,length)
				call WorkingMemory%get_memory(srw,rwdim)
				call WorkingMemory%get_memory(cwork,lw)
				call WorkingMemory%get_memory(iwork,8*min_MN)
				call ccopy (length, indata%cdata, 1, cdata, 1)
				if(present(cut))then
					cut2=min(cut,min_MN)
					!allocate(ucdata(m,min_MN))
					!allocate(vcdata(min_MN,n))
					!allocate(sdata4(min_MN)) 
					call WorkingMemory%get_memory(ucdata,m,min_MN)
					call WorkingMemory%get_memory(vcdata,min_MN,n)
					call WorkingMemory%get_memory(sdata4,min_MN)
					call CGESDD('S',m,n,cdata,m,sdata4,ucdata,m,vcdata,min_MN,cWORK,lw,srw,iwork,INFO)
					s%sdata=sdata4(1:cut2)
					call cut_off_routine_com4(u%cdata,m,cut2,ucdata,m,min_MN)
					call cut_off_routine_com4(V%cdata,cut2,n,Vcdata,min_MN,n)
				else
					call CGESDD('S',m,n,cdata,m,S%sdata,U%cdata,m,V%cdata,min_MN,cWORK,lw,srw,iwork,INFO)
				end if
			case(3)
				lw=4*min_MN*min_MN+7*min_MN+1
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n+min_MN
					call WorkingMemory%allocate(3,totallengthofmemory)
					call WorkingMemory%allocate(1,16*min_MN)
				end if
				call WorkingMemory%get_memory(ddata,length)
				call WorkingMemory%get_memory(dwork,lw)
				call WorkingMemory%get_memory(iwork,8*min_MN)
				!allocate(ddata(length))
				!allocate(iw(8*min_MN))
				!allocate(dwork(lw))
				call dcopy (length, indata%ddata, 1, ddata, 1)
				if(present(cut))then
					cut2=min(cut,min_MN)
					call WorkingMemory%get_memory(uddata,m,min_MN)
					call WorkingMemory%get_memory(vddata,min_MN,n)
					call WorkingMemory%get_memory(sdata8,min_MN)
					!allocate(uddata(m,min_MN))
					!allocate(vddata(min_MN,n))
					!allocate(sdata8(min_MN)) 
					call DGESDD('S',m,n,ddata,m,sdata8,uddata,m,vddata,min_MN,dWORK,lw,iwork,INFO)
					s%ddata=sdata8(1:cut2)
					call cut_off_routine_real8(u%ddata,m,cut2,uddata,m,min_MN)
					call cut_off_routine_real8(V%ddata,cut2,n,Vddata,min_MN,n)
				else
					call DGESDD('S',m,n,ddata,m,S%dData,U%dData,m,V%dData,min_MN,dWORK,lw,iwork,INFO)
				end if
			case(2)
				lw=4*min_MN*min_MN+max(max(M,N),4*min(M,N)*min(M,N)+4*min(M,N))
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n+min_MN
					call WorkingMemory%allocate(2,totallengthofmemory)
					call WorkingMemory%allocate(1,16*min_MN)
				end if
				call WorkingMemory%get_memory(sdata,length)
				call WorkingMemory%get_memory(swork,lw)
				call WorkingMemory%get_memory(iwork,8*min_MN)
				!allocate(sdata(length))
				!allocate(srw(rwdim))
				!allocate(iw(8*min_MN))
				!allocate(swork(lw))
				call scopy (length, indata%sdata, 1, sdata, 1)
				if(present(cut))then
					cut2=min(cut,min_MN)
					call WorkingMemory%get_memory(usdata,m,min_MN)
					call WorkingMemory%get_memory(vsdata,min_MN,n)
					call WorkingMemory%get_memory(sdata4,min_MN)
					!allocate(usdata(m,min_MN))
					!allocate(vsdata(min_MN,n))
					!allocate(sdata4(min_MN)) 
					call SGESDD('S',m,n,sdata,m,sdata4,usdata,m,vsdata,min_MN,sWORK,lw,iwork,INFO)
					s%sdata=sdata4(1:cut2)
					call cut_off_routine_real4(u%sdata,m,cut2,usdata,m,min_MN)
					call cut_off_routine_real4(V%sdata,cut2,n,Vsdata,min_MN,n)
				else
					call SGESDD('S',m,n,sdata,m,S%sdata,U%sdata,m,V%sdata,min_MN,sWORK,lw,iwork,INFO)
				end if
			case(1)
				lw=4*min_MN*min_MN+max(max(M,N),4*min(M,N)*min(M,N)+4*min(M,N))
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n+min_MN
					call WorkingMemory%allocate(2,totallengthofmemory)
					call WorkingMemory%allocate(1,16*min_MN)
				end if
				call WorkingMemory%get_memory(sdata,length)
				call WorkingMemory%get_memory(swork,lw)
				call WorkingMemory%get_memory(iwork,8*min_MN)
				!allocate(sdata(length))
				!allocate(srw(rwdim))
				!allocate(iw(8*min_MN))
				!allocate(swork(lw))
				sData=indata%idata
				if(present(cut))then
					cut2=min(cut,min_MN)
					call WorkingMemory%get_memory(usdata,m,min_MN)
					call WorkingMemory%get_memory(vsdata,min_MN,n)
					call WorkingMemory%get_memory(sdata4,min_MN)
					!allocate(usdata(m,min_MN))
					!allocate(vsdata(min_MN,n))
					!allocate(sdata4(min_MN)) 
					call SGESVD('S',m,n,sdata,m,sdata4,usdata,m,vsdata,min_MN,sWORK,lw,iwork,INFO)
					s%sdata=sdata4(1:cut2)
					call cut_off_routine_real4(u%sdata,m,cut2,usdata,m,min_MN)
					call cut_off_routine_real4(V%sdata,cut2,n,Vsdata,min_MN,n)
				else
					call SGESVD('S',m,n,sdata,m,S%sdata,U%sdata,m,V%sdata,min_MN,sWORK,lw,iwork,INFO)
				end if
		end select
		call WorkingMemory%free()
		return
	end subroutine

	!if deallocate_memory_flag=.false. call U%empty() will not deallocate the momery of U
	! when trucate the data, just reset the info in dimension,for  example:
	!       call U%allocate([12],'real'), then the length of U%ddata is 12
	!       call U%empty, the program do not deallocate U%ddata, by reset the info in diemsnion
	!       call U%allocate([10],'real'), the length of  U%ddata is still  12, but the info in dimension is 10
	!

	subroutine SVD_TData_routine_SVD_Kill_inData(indata,U,S,V,m,n,min_MN,cut,info)
		integer,intent(in)::min_MN
		type(TData),intent(inout)::indata
		type(TData),intent(inout)::U,V
		type(TData),intent(inout)::s
		integer,intent(in),optional::cut,info
		complex*16,pointer :: zwork(:),vzdata(:,:)
		complex*8,pointer :: cwork(:),vcdata(:,:)
		real*8,pointer :: dwork(:),vddata(:,:)
		real*4,pointer :: swork(:),i2sdata(:),usdata(:,:),vsdata(:,:)
		integer m,n,lw,rwdim,i,j
		integer,pointer :: iw(:)
		real*8,pointer :: drw(:)
		real*4,pointer :: srw(:)
		integer :: ms_max,max_MN,length,cut2
		integer::totallengthofmemory
		call WorkingMemory%check()
		length=indata%TotalData
		max_MN=max(M,N)
		lw=min_MN*min_MN+2*min_MN+max_MN
		rwdim=min_MN*max(5*min_MN+7,2*max_MN+2*min_MN+1)
		select case(indata%classType)
			case (5)
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n
					call WorkingMemory%allocate(5,totallengthofmemory)
					totallengthofmemory=rwdim+min_MN
					call WorkingMemory%allocate(3,totallengthofmemory)
					call WorkingMemory%allocate(1,8*min_MN)
				end if
				!allocate(drw(rwdim))
				!allocate(iw(8*min_MN))
				!allocate(zwork(lw))
				call WorkingMemory%get_memory(drw,rwdim)
				call WorkingMemory%get_memory(iw,8*min_MN)
				call WorkingMemory%get_memory(zwork,lw)
				if(present(cut))then
					cut2=min(cut,min_MN)
					!allocate(uzdata(m,min_MN))
					!allocate(vzdata(min_MN,n))
					!allocate(sdata8(min_MN)) 
					if(size(u%zdata).lt.(m*min_MN))then
						call writemess('ERROR in SVD_TData_routine_SVD_Kill_inData,1')
						call error_stop
					end if
					if(size(S%ddata).lt.min_MN)then
						call writemess('ERROR in SVD_TData_routine_SVD_Kill_inData,2')
						call error_stop
					end if
					call WorkingMemory%get_memory(vzdata,min_MN,n)
					call ZGESVD('S','S',m,n,indata%zdata,m,s%ddata,u%zdata,m,vzdata,min_MN,zWORK,lw,drw,INFO)
					call cut_off_routine_com8(V%zdata,cut2,n,Vzdata,min_MN,n)
				else
					call ZGESVD('S','S',m,n,indata%zdata,m,S%ddata,U%zdata,m,V%zdata,min_MN,zWORK,lw,drw,INFO)
				end if
			case(4)
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n
					call WorkingMemory%allocate(4,totallengthofmemory)
					totallengthofmemory=rwdim+min_MN
					call WorkingMemory%allocate(2,totallengthofmemory)
					call WorkingMemory%allocate(1,8*min_MN)
				end if
				call WorkingMemory%get_memory(srw,rwdim)
				call WorkingMemory%get_memory(iw,8*min_MN)
				call WorkingMemory%get_memory(cwork,lw)
				if(present(cut))then
					cut2=min(cut,min_MN)
					if(size(u%cdata).lt.(m*min_MN))then
						call writemess('ERROR in SVD_TData_routine_SVD_Kill_inData,3')
						call error_stop
					end if
					if(size(S%sdata).lt.min_MN)then
						call writemess('ERROR in SVD_TData_routine_SVD_Kill_inData,4')
						call error_stop
					end if
					call WorkingMemory%get_memory(vcdata,min_MN,n)
					call CGESVD('S','S',m,n,indata%cdata,m,s%sdata,u%cdata,m,vcdata,min_MN,cWORK,lw,srw,INFO)
					call cut_off_routine_com4(V%cdata,cut2,n,Vcdata,min_MN,n)
				else
					call CGESVD('S','S',m,n,indata%cdata,m,S%sdata,U%cdata,m,V%cdata,min_MN,cWORK,lw,srw,INFO)
				end if
			case(3)
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n+rwdim+min_MN
					call WorkingMemory%allocate(3,totallengthofmemory)
					call WorkingMemory%allocate(1,8*min_MN)
				end if
				call WorkingMemory%get_memory(drw,rwdim)
				call WorkingMemory%get_memory(iw,8*min_MN)
				call WorkingMemory%get_memory(dwork,lw)
				if(present(cut))then
					if(size(u%ddata).lt.(m*min_MN))then
						call writemess('ERROR in SVD_TData_routine_SVD_Kill_inData,5')
						call error_stop
					end if
					if(size(S%ddata).lt.min_MN)then
						call writemess('ERROR in SVD_TData_routine_SVD_Kill_inData,6')
						call error_stop
					end if
					cut2=min(cut,min_MN)
					call WorkingMemory%get_memory(vddata,min_MN,n)
					call DGESVD('S','S',m,n,indata%ddata,m,s%ddata,u%ddata,m,vddata,min_MN,dWORK,lw,INFO)
					call cut_off_routine_real8(V%ddata,cut2,n,Vddata,min_MN,n)
				else
					call DGESVD('S','S',m,n,indata%ddata,m,S%dData,U%dData,m,V%dData,min_MN,dWORK,lw,INFO)
				end if
			case(2)
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n+rwdim+min_MN
					call WorkingMemory%allocate(2,totallengthofmemory)
					call WorkingMemory%allocate(1,8*min_MN)
				end if
				call WorkingMemory%get_memory(srw,rwdim)
				call WorkingMemory%get_memory(iw,8*min_MN)
				call WorkingMemory%get_memory(swork,lw)
				if(present(cut))then
					if(size(u%sdata).lt.(m*min_MN))then
						call writemess('ERROR in SVD_TData_routine_SVD_Kill_inData,7')
						call error_stop
					end if
					if(size(S%sdata).lt.min_MN)then
						call writemess('ERROR in SVD_TData_routine_SVD_Kill_inData,8')
						call error_stop
					end if
					cut2=min(cut,min_MN)
					call WorkingMemory%get_memory(vsdata,min_MN,n)
					call SGESVD('S','S',m,n,indata%sdata,m,s%sdata,u%sdata,m,vsdata,min_MN,sWORK,lw,INFO)
					call cut_off_routine_real4(V%sdata,cut2,n,Vsdata,min_MN,n)
				else
					call SGESVD('S','S',m,n,indata%sdata,m,S%sdata,U%sdata,m,V%sdata,min_MN,sWORK,lw,INFO)
				end if
			case(1)
				if(WorkingMemory%ifDynamic())then
					totallengthofmemory=length+lw+m*min_MN+min_MN*n+rwdim+min_MN
					call WorkingMemory%allocate(2,totallengthofmemory)
					call WorkingMemory%allocate(1,8*min_MN)
				end if
				call WorkingMemory%get_memory(i2sdata,length)
				call WorkingMemory%get_memory(srw,rwdim)
				call WorkingMemory%get_memory(iw,8*min_MN)
				call WorkingMemory%get_memory(swork,lw)
				i2sdata=indata%idata
				if(present(cut))then
					if(size(u%sdata).lt.(m*min_MN))then
						call writemess('ERROR in SVD_TData_routine_SVD_Kill_inData,9')
						call error_stop
					end if
					if(size(S%sdata).lt.min_MN)then
						call writemess('ERROR in SVD_TData_routine_SVD_Kill_inData,0')
						call error_stop
					end if
					cut2=min(cut,min_MN)
					call WorkingMemory%get_memory(vsdata,min_MN,n)
					call SGESVD('S','S',m,n,i2sdata,m,s%sdata,u%sdata,m,vsdata,min_MN,sWORK,lw,INFO)
					call cut_off_routine_real4(V%sdata,cut2,n,Vsdata,min_MN,n)
				else
					call SGESVD('S','S',m,n,i2sdata,m,S%sdata,U%sdata,m,V%sdata,min_MN,sWORK,lw,INFO)
				end if
		end select
		call WorkingMemory%free()
		return
	end subroutine
	
	subroutine cut_off_routine_real4(outdata,LDO1,LDO2,indata,LDI1,LDI2)
		integer,intent(in)::LDO1,LDO2,LDI1,LDI2
		real(kind=4),intent(inout)::outdata(LDO1,LDO2)
		real(kind=4),intent(in)::indata(LDI1,LDI2)
		outdata=indata(1:LDO1,1:LDO2)
		return
	end subroutine
	subroutine cut_off_routine_real8(outdata,LDO1,LDO2,indata,LDI1,LDI2)
		integer,intent(in)::LDO1,LDO2,LDI1,LDI2
		real(kind=8),intent(inout)::outdata(LDO1,LDO2)
		real(kind=8),intent(in)::indata(LDI1,LDI2)
		outdata=indata(1:LDO1,1:LDO2)
		return
	end subroutine
	subroutine cut_off_routine_com4(outdata,LDO1,LDO2,indata,LDI1,LDI2)
		integer,intent(in)::LDO1,LDO2,LDI1,LDI2
		complex(kind=4),intent(inout)::outdata(LDO1,LDO2)
		complex(kind=4),intent(in)::indata(LDI1,LDI2)
		outdata=indata(1:LDO1,1:LDO2)
		return
	end subroutine
	subroutine cut_off_routine_com8(outdata,LDO1,LDO2,indata,LDI1,LDI2)
		integer,intent(in)::LDO1,LDO2,LDI1,LDI2
		complex(kind=8),intent(inout)::outdata(LDO1,LDO2)
		complex(kind=8),intent(in)::indata(LDI1,LDI2)
		outdata=indata(1:LDO1,1:LDO2)
		return
	end subroutine

!************************************  expm *****************************************
	subroutine expm_TData_routine(Hdata,N)
		integer,intent(in)::N
		type(TData) ,intent(inout):: Hdata
		complex*16,allocatable::zexpHdata(:,:)
		complex*8,allocatable::cexpHdata(:,:)
		complex*8,allocatable :: ceigVal(:),ceigVec(:,:)
		complex*16,allocatable :: zeigVal(:),zeigVec(:,:)
		complex*8,ALLOCATABLE::cwork(:)
		complex*16,ALLOCATABLE::zwork(:)
		real*8,allocatable::drwork(:)
		real*4,allocatable::srwork(:)
		logical::bwork(N)
		integer :: i,j,lwork,info,SDIM
		select case(HData%classType)
			case (5)
				lwork=2*N
				allocate(zwork(lwork))
				allocate(zexpHdata(N,N))
				allocate(zeigVal(N))
				allocate(zeigVec(N,N))
				allocate(drwork(N))
				call ZGEES('V','N',1,N,Hdata%zdata,N,SDIM,zeigVal,zeigVec,N,zWORK,LWORK,dRWORK,BWORK,INFO)
				do i=1,N
					zeigVal(i)=cdexp(zeigVal(i))
				end do
				do i=1,N
					do j=1,N
						zexpHdata(i,j)=zeigVec(i,j)*zeigVal(j)
					end do
				end do
				zeigVec=dconjg(transpose(zeigVec))
				call ZGEMM('N', 'N', N, N, N, dcmplx(1d0,0d0), zexpHdata , N, zeigVec , N, dcmplx(0d0,0d0), Hdata%zdata, N)
			case(4)
				lwork=2*N
				allocate(cwork(lwork))
				allocate(cexpHdata(N,N))
				allocate(ceigVal(N))
				allocate(ceigVec(N,N))
				allocate(srwork(N))
				call CGEES('V','N',1,N,Hdata%cdata,N,SDIM,ceigVal,ceigVec,N,cWORK,LWORK,sRWORK,BWORK,INFO)
				do i=1,N
					ceigVal(i)=cexp(ceigVal(i))
				end do
				do i=1,N
					do j=1,N
						cexpHdata(i,j)=ceigVec(i,j)*ceigVal(j)
					end do
				end do
				ceigVec=conjg(transpose(ceigVec))
				call CGEMM('N', 'N', N, N, N, cmplx(1,kind=4), cexpHdata , N, ceigVec , N, cmplx(0,kind=4), Hdata%cdata, N)
			case default
				write(*,*)"do not finished real part yet"
				call error_stop()
		end select
		return
	end subroutine
!****************************************************************
!    eigenvalue
	subroutine eigenvalue_TData_routine(T,N,eigvalue,eigvector)
		integer,intent(in)::N
		type(TData) ,intent(inout):: eigvalue
		type(TData) ,optional,intent(inout)::eigvector
		type(TData) ,intent(in):: T
		complex(kind=8),allocatable::zHdata(:)
		complex(kind=4),allocatable::cHdata(:)
		real(kind=8),allocatable::dHdata(:)
		real(kind=4),allocatable::sHdata(:)
		
		complex(kind=8),allocatable::zeigVal(:)
		complex(kind=4),allocatable::ceigVal(:)
		real(kind=8),allocatable::deigValR(:),deigValI(:)
		real(kind=4),allocatable::seigValR(:),seigValI(:)
		
		complex(kind=8),allocatable::zeigVec(:)
		complex(kind=4),allocatable::ceigVec(:)
		real(kind=8),allocatable::deigVec(:)
		real(kind=4),allocatable::seigVec(:)
		
		complex(kind=8),allocatable::zwork(:)
		complex(kind=4),allocatable::cwork(:)
		real(kind=8),allocatable::dwork(:)
		real(kind=4),allocatable::swork(:)
		
		real(kind=8),allocatable::dRWORK(:)
		real(kind=4),allocatable::sRWORK(:)
		
		logical::bwork(N)
		integer::INFO,lwork,SDIM
		
		select case (T%classType)
			case(5)
				lwork=2*N
				allocate(zHdata(T%TotalData))
				allocate(zeigVal(N))
				allocate(dRWORK(N))
				allocate(zwork(lwork))
				if(present(eigvector))then
					allocate(zeigVec(T%TotalData))
					call zcopy(T%TotalData,T%zdata,1,zHdata,1)
					call ZGEES('V','N',1,N,zHdata,N,SDIM,zeigVal,zeigVec,N,zwork,LWORK,dRWORK,BWORK,INFO)
				else
					call zcopy(T%TotalData,T%zdata,1,zHdata,1)
					call ZGEES('N','N',1,N,zHdata,N,SDIM,zeigVal,zeigVec,N,zwork,LWORK,dRWORK,BWORK,INFO)
				end if
				if(info.ne.0) then
					write (*,*) "Error in eig ,info=",info
					call error_stop()
				end if
				call zcopy(N,zeigVal,1,eigvalue%zdata,1)
				if(present(eigvector))call zcopy(eigvector%TotalData,zeigVec,1,eigvector%Zdata,1)
			case(4)
				lwork=2*N
				allocate(cHdata(T%TotalData))
				allocate(ceigVal(N))
				allocate(sRWORK(N))
				allocate(cwork(lwork))
				if(present(eigvector))then
					allocate(ceigVec(T%TotalData))
					call ccopy(T%TotalData,T%cdata,1,cHdata,1)
					call CGEES('V','N',1,N,cHdata,N,SDIM,ceigVal,ceigVec,N,cwork,LWORK,sRWORK,BWORK,INFO)
				else
					call ccopy(T%TotalData,T%cdata,1,cHdata,1)
					call CGEES('N','N',1,N,cHdata,N,SDIM,ceigVal,ceigVec,N,cwork,LWORK,sRWORK,BWORK,INFO)
				end if
				if(info.ne.0) then
					write (*,*) "Error in eig ,info=",info
					call error_stop()
				end if
				call ccopy(N,ceigVal,1,eigvalue%cdata,1)
				if(present(eigvector))call ccopy(eigvector%TotalData,ceigVec,1,eigvector%cdata,1)
			case(3)
				lwork=3*N
				allocate(dHdata(T%TotalData))
				allocate(deigValR(N))
				allocate(deigValI(N))
				allocate(dRWORK(N))
				allocate(dwork(lwork))
				if(present(eigvector))then
					allocate(deigVec(T%TotalData))
					call dcopy(T%TotalData,T%ddata,1,dHdata,1)
					call DGEES('V','N',1,N,dHdata,N,SDIM,deigValR,deigValI,deigVec,N,dwork,LWORK,BWORK,INFO)
				else
					call dcopy(T%TotalData,T%ddata,1,dHdata,1)
					call DGEES('N','N',1,N,dHdata,N,SDIM,deigValR,deigValI,deigVec,N,dwork,LWORK,BWORK,INFO)
				end if
				if(info.ne.0) then
					write (*,*) "Error in eig ,info=",info
					call error_stop()
				end if
				call zcopy(N,dcmplx(deigValR,deigValI),1,eigvalue%zdata,1)
				if(present(eigvector))call dcopy(eigvector%TotalData,deigVec,1,eigvector%ddata,1)
			case(2)
				lwork=3*N
				allocate(sHdata(T%TotalData))
				allocate(seigValR(N))
				allocate(seigValI(N))
				allocate(sRWORK(N))
				allocate(swork(lwork))
				if(present(eigvector))then
					allocate(seigVec(T%TotalData))
					call scopy(T%TotalData,T%sdata,1,sHdata,1)
					call SGEES('V','N',1,N,sHdata,N,SDIM,seigValR,seigValI,seigVec,N,swork,LWORK,BWORK,INFO)
				else
					call scopy(T%TotalData,T%sdata,1,sHdata,1)
					call SGEES('N','N',1,N,sHdata,N,SDIM,seigValR,seigValI,seigVec,N,swork,LWORK,BWORK,INFO)
				end if
				if(info.ne.0) then
					write (*,*) "Error in eig ,info=",info
					call error_stop()
				end if
				call ccopy(N,cmplx(seigValR,seigValI,kind=4),1,eigvalue%cdata,1)
				if(present(eigvector))call scopy(eigvector%TotalData,seigVec,1,eigvector%sdata,1)
			case(1)
				lwork=3*N
				allocate(sHdata(T%TotalData))
				allocate(seigValR(N))
				allocate(seigValI(N))
				allocate(sRWORK(N))
				allocate(swork(lwork))
				if(present(eigvector))then
					allocate(seigVec(T%TotalData))
					sHdata=T%idata(1:T%TotalData)
					call SGEES('V','N',1,N,sHdata,N,SDIM,seigValR,seigValI,seigVec,N,swork,LWORK,sRWORK,BWORK,INFO)
				else
					sHdata=T%idata(1:T%TotalData)
					call SGEES('N','N',1,N,sHdata,N,SDIM,seigValR,seigValI,seigVec,N,swork,LWORK,sRWORK,BWORK,INFO)
				end if
				if(info.ne.0) then
					write (*,*) "Error in sig ,info=",info
					call error_stop()
				end if
				call ccopy(N,cmplx(seigValR,seigValI,kind=4),1,eigvalue%cdata,1)
				if(present(eigvector))call scopy(eigvector%TotalData,seigVec,1,eigvector%sdata,1)
		end select
		return
	end subroutine
	
!Solves a general system of linear equations
!A*X=B,find X
!X is a vector or a matrix,the dimension of which is the same as B
!on output,A will change and X is in B	
!the subroutine ZGESV will change A	
	subroutine linequ_routine_TData(R,A,B,Na,Nb)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A,B
		integer,intent(in)::Na,Nb
		integer,allocatable::IPIV(:)
		real(kind=4),allocatable::sdata(:)
		real(kind=8),allocatable::ddata(:)
		complex(kind=4),allocatable::cdata(:)
		complex(kind=8),allocatable::zdata(:)
		integer::INFO
		allocate(IPIV(Na))
		select case(R%classtype)
			case(2)
				allocate(sdata(A%totalData))
				call assignment_real4_Tdata(sdata,A,A%totalData)
				call assignment_real4_Tdata(R%sdata,B,B%totalData)
				call SGESV(Na,Nb,sdata,Na,IPIV,R%sdata,Na,INFO)
			case(3)
				allocate(ddata(A%totalData))
				call assignment_real8_Tdata(ddata,A,A%totalData)
				call assignment_real8_Tdata(R%ddata,B,B%totalData)
				call DGESV(Na,Nb,ddata,Na,IPIV,R%ddata,Na,INFO)
			case(4)
				allocate(cdata(A%totalData))
				call assignment_com4_Tdata(cdata,A,A%totalData)
				call assignment_com4_Tdata(R%cdata,B,B%totalData)
				call CGESV(Na,Nb,cdata,Na,IPIV,R%cdata,Na,INFO)	
			case(5)
				allocate(zdata(A%totalData))
				call assignment_com8_Tdata(zdata,A,A%totalData)
				call assignment_com8_Tdata(R%zdata,B,B%totalData)
				call ZGESV(Na,Nb,zdata,Na,IPIV,R%zdata,Na,INFO)	
		end select
		if(INFO.ne.0)then
			write(*,*)"ZGESV is not successful"
			write(*,*)"INFO",INFO
			call error_stop()
		end if
		return
	end subroutine
	
!A*X=B
!input A  and B
!output X
!X!=A^{-1} * B because A^{-1} may not exit
!X=(A^T * A)^{-1}* (A^T *B)
		subroutine linequ2_routine_TData(R,A,B,Na,Na2,Nb,RCOND_class)
		type(TData),intent(inout)::R
		type(TData),intent(in)::A,B
		integer,intent(in)::Na,Na2,Nb
		class(*),intent(in)::RCOND_class
		real*8::dRCOND
		real*4::sRCOND
		integer,allocatable::IPIV(:)
		real(kind=4),allocatable::sdata(:),sWORK(:),sS(:)
		real(kind=8),allocatable::ddata(:),dWORK(:),dS(:)
		complex(kind=4),allocatable::cdata(:),cWORK(:),cRWORK(:)
		complex(kind=8),allocatable::zdata(:),zWORK(:),zRWORK(:)
		integer::INFO,RANK,LWORK,maxMn
		select type(RCOND_class)
			type is (real(kind=4))
			 dRCOND=RCOND_class
			 sRCOND=RCOND_class
			type is (real(kind=8))
			 dRCOND=RCOND_class
			 sRCOND=RCOND_class
			type is (integer)
			 dRCOND=RCOND_class
			 sRCOND=RCOND_class
		end select
		allocate(IPIV(Na))
		maxMn=max(Na,Na2)
		LWORK=3*maxMn+MAX(2*maxMn,Nb,maxMn)+1
		select case(R%classtype)
			case(2)
				allocate(sdata(A%totalData))
				call assignment_real4_Tdata(sdata,A,A%totalData)
				call assignment_real4_Tdata(R%sdata,B,B%totalData)
				allocate(ss(min(Na,Na2)))
				allocate(sWORK(LWORK))
				call SGELSS(Na,Na2, Nb, sdata, Na, R%sdata, Na, sS, sRCOND,RANK, sWORK, LWORK, INFO )
			case(3)
				allocate(ddata(A%totalData))
				call assignment_real8_Tdata(ddata,A,A%totalData)
				call assignment_real8_Tdata(R%ddata,B,B%totalData)
				allocate(dS(min(Na,Na2)))
				allocate(dWORK(LWORK))
				call DGELSS(Na,Na2, Nb, ddata, Na, R%ddata, Na, dS, dRCOND,RANK, dWORK, LWORK, INFO )
			case(4)
				allocate(cdata(A%totalData))
				call assignment_com4_Tdata(cdata,A,A%totalData)
				call assignment_com4_Tdata(R%cdata,B,B%totalData)
				allocate(sS(min(Na,Na2)))
				allocate(cWORK(LWORK))
				allocate(cRWORK(5*min(Na,Na2)))
				call CGELSS(Na,Na2, Nb, cdata, Na, R%cdata, Na, sS, sRCOND,RANK,cWORK, LWORK,cRWORK, INFO )
			case(5)
				allocate(zdata(A%totalData))
				call assignment_com8_Tdata(zdata,A,A%totalData)
				call assignment_com8_Tdata(R%zdata,B,B%totalData)
				allocate(dS(min(Na,Na2)))
				allocate(zWORK(LWORK))
				allocate(zRWORK(max(5*min(Na,Na2)-4,1)))
				call ZGELSS(Na,Na2, Nb, zdata, Na, R%zdata, Na, dS, dRCOND,RANK,zWORK, LWORK,zRWORK, INFO )
		end select
		if(INFO.ne.0)then
			write(*,*)"?GELSS is not successful"
			write(*,*)"INFO",INFO
			call error_stop()
		end if
		return
	end subroutine
	
	subroutine TData_LQ(outTau,inoutTensor,M,N,INFO)
		type(TData),intent(inout)::outTau,inoutTensor
		integer,intent(in)::M,N
		integer,intent(inout)::INFO
		real(kind=4),pointer::swork(:)
		real(kind=8),pointer::dwork(:)
		complex(kind=4),pointer::cwork(:)
		complex(kind=8),pointer::zwork(:)
		integer::LWORK
		LWORK=2*max(M,N)
		call WorkingMemory%check()
		select case(inoutTensor%classtype)
			case(2)
				call WorkingMemory%get_memory(swork,LWORK)
				call SGELQF( M, N, inoutTensor%sdata, M, outTau%sdata, sWORK, LWORK, INFO )
			case(3)
				call WorkingMemory%get_memory(dwork,LWORK)
				call DGELQF( M, N, inoutTensor%ddata, M, outTau%ddata, dWORK, LWORK, INFO )
			case(4)
				call WorkingMemory%get_memory(cwork,LWORK)
				call CGELQF( M, N, inoutTensor%cdata, M, outTau%cdata, cWORK, LWORK, INFO )
			case(5)
				call WorkingMemory%get_memory(zwork,LWORK)
				call ZGELQF( M, N, inoutTensor%zdata, M, outTau%zdata, zWORK, LWORK, INFO )
		end select
		call WorkingMemory%free()
		return
	end subroutine
	
	subroutine TData_QR(outTau,inoutTensor,M,N,INFO)
		type(TData),intent(inout)::outTau,inoutTensor
		integer,intent(in)::M,N
		integer,intent(inout)::INFO
		real(kind=4),pointer::swork(:)
		real(kind=8),pointer::dwork(:)
		complex(kind=4),pointer::cwork(:)
		complex(kind=8),pointer::zwork(:)
		integer::LWORK
		LWORK=2*max(M,N)
		call WorkingMemory%check()
		select case(inoutTensor%classtype)
			case(2)
				call WorkingMemory%get_memory(swork,LWORK)
				call SGEQRF( M, N, inoutTensor%sdata, M, outTau%sdata, sWORK, LWORK, INFO )
			case(3)
				call WorkingMemory%get_memory(dwork,LWORK)
				call DGEQRF( M, N, inoutTensor%ddata, M, outTau%ddata, dWORK, LWORK, INFO )
			case(4)
				call WorkingMemory%get_memory(cwork,LWORK)
				call CGEQRF( M, N, inoutTensor%cdata, M, outTau%cdata, cWORK, LWORK, INFO )
			case(5)
				call WorkingMemory%get_memory(zwork,LWORK)
				call ZGEQRF( M, N, inoutTensor%zdata, M, outTau%zdata, zWORK, LWORK, INFO )
		end select
		call WorkingMemory%free()
		return
	end subroutine
	
	
	subroutine TData_ORGQR(Tau,T,M,N,minMN,INFO)
		type(TData),intent(inout)::T,Tau
		integer,intent(in)::M,N,minMN
		integer,intent(inout)::INFO
		real(kind=4),pointer::swork(:)
		real(kind=8),pointer::dwork(:)
		complex(kind=4),pointer::cwork(:)
		complex(kind=8),pointer::zwork(:)
		integer::LWORK
		LWORK=2*N
		call WorkingMemory%check()
		select case(T%classtype)
			case(2)
				call WorkingMemory%get_memory(swork,LWORK)
				call SORGQR( M, minMN, minMN, T%sdata, M, TAU%sdata, swork, LWORK, INFO )
			case(3)
				call WorkingMemory%get_memory(dwork,LWORK)
				call DORGQR( M, minMN, minMN, T%ddata, M, TAU%ddata, dwork, LWORK, INFO )
			case(4)
				call WorkingMemory%get_memory(cwork,LWORK)
				call CUNGQR( M, minMN, minMN, T%cdata, M, TAU%cdata, cwork, LWORK, INFO )
			case(5)
				call WorkingMemory%get_memory(zwork,LWORK)
				call ZUNGQR( M, minMN, minMN, T%zdata, M, TAU%zdata, zwork, LWORK, INFO )
		end select
		call WorkingMemory%free()
		return
	end subroutine
	
	subroutine TData_ORGLQ(Tau,T,M,N,minMN,INFO)
		type(TData),intent(inout)::T,Tau
		integer,intent(in)::M,N,minMN
		integer,intent(inout)::INFO
		real(kind=4),pointer::swork(:)
		real(kind=8),pointer::dwork(:)
		complex(kind=4),pointer::cwork(:)
		complex(kind=8),pointer::zwork(:)
		integer::LWORK
		LWORK=2*N
		call WorkingMemory%check()
		select case(T%classtype)
			case(2)
				call WorkingMemory%get_memory(swork,LWORK)
				call SORGLQ( minMN, N, minMN, T%sdata, M, TAU%sdata, swork, LWORK, INFO )
			case(3)
				call WorkingMemory%get_memory(dwork,LWORK)
				call DORGLQ( minMN, N, minMN, T%ddata, M, TAU%ddata, dwork, LWORK, INFO )
			case(4)
				call WorkingMemory%get_memory(cwork,LWORK)
				call CUNGLQ( minMN, N, minMN, T%cdata, M, TAU%cdata, cwork, LWORK, INFO )
			case(5)
				call WorkingMemory%get_memory(zwork,LWORK)
				call ZUNGLQ( minMN, N, minMN, T%zdata, M, TAU%zdata, zwork, LWORK, INFO )
		end select
		call WorkingMemory%free()
		return
	end subroutine
		
	

!**********************************************************************
!**********************************************************************
!	the code below is for MPI
!**********************************************************************
	subroutine sent_TData(Ten1,Ten2,ID1,ID2,ierr,MPIcommon)
		type(TData),intent(in)::Ten1
		type(TData),intent(inout)::Ten2
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
		if(present(MPIcommon))then
			if((ID1.ge.proNum).or.(ID2.ge.proNum))return
		end if
		
		if(ID1.eq.ID2) return !The some cpu, do nothing
		
		if((proID.ne.ID1).and.(proID.ne.ID2)) return!The proID do not sent or recv, return
!**************************flag************************************************				
		if(proID.eq.ID1) then
			call mpi_send(Ten1%flag,1,MPI_logical,ID2,tag,MPI_Comm,ierr)
			if(.not.Ten1%flag)then
				return
			end if
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Ten2%flag,1,MPI_logical,ID1,tag,MPI_Comm,istatus,ierr)
			if(.not.Ten2%flag)then
				return
			end if
		end if
!****************************totalData**********************************************		
		if(proID.eq.ID1) then
			call mpi_send(Ten1%totalData,1,MPI_integer,ID2,tag,MPI_Comm,ierr)
		end if
		if(proID.eq.ID2) then
			call mpi_recv(Ten2%totalData,1,MPI_integer,ID1,tag,MPI_Comm,istatus,ierr)
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
!**********************************************************************************	
		if(proID.eq.ID1) then
			select case(Ten1%classtype)
				case(1)
					call mpi_send(Ten1%idata(1:Ten1%totalData),Ten1%totalData,MPI_integer,ID2,tag,MPI_Comm,ierr)
				case(2)
					call mpi_send(Ten1%sdata(1:Ten1%totalData),Ten1%totalData,MPI_real,ID2,tag,MPI_Comm,ierr)
				case(3)
					call mpi_send(Ten1%ddata(1:Ten1%totalData),Ten1%totalData,MPI_double_precision,ID2,tag,MPI_Comm,ierr)
				case(4)
					call mpi_send(Ten1%cdata(1:Ten1%totalData),Ten1%totalData,MPI_complex,ID2,tag,MPI_Comm,ierr)
				case(5)
					call mpi_send(Ten1%zdata(1:Ten1%totalData),Ten1%totalData,MPI_double_complex,ID2,tag,MPI_Comm,ierr)
				case(6)
					call mpi_send(Ten1%ldata(1:Ten1%totalData),Ten1%totalData,MPI_logical,ID2,tag,MPI_Comm,ierr)
				case(7)
					call mpi_send(Ten1%adata(1:Ten1%totalData),max_len_of_char_in_TData*Ten1%totalData,&
																MPI_character,ID2,tag,MPI_Comm,ierr)
			end select
		end if
		if(proID.eq.ID2) then
			call allocateTData(Ten2,Ten2%totalData)
			select case(Ten2%classtype)
				case(1)
					call mpi_recv(Ten2%idata(1:Ten2%totalData),Ten2%totalData,MPI_integer,ID1,tag,MPI_Comm,istatus,ierr)
				case(2)
					call mpi_recv(Ten2%sdata(1:Ten2%totalData),Ten2%totalData,MPI_real,ID1,tag,MPI_Comm,istatus,ierr)
				case(3)
					call mpi_recv(Ten2%ddata(1:Ten2%totalData),Ten2%totalData,MPI_double_precision,ID1,tag,MPI_Comm,istatus,ierr)
				case(4)
					call mpi_recv(Ten2%cdata(1:Ten2%totalData),Ten2%totalData,MPI_complex,ID1,tag,MPI_Comm,istatus,ierr)
				case(5)
					call mpi_recv(Ten2%zdata(1:Ten2%totalData),Ten2%totalData,MPI_double_complex,ID1,tag,MPI_Comm,istatus,ierr)
				case(6)
					call mpi_recv(Ten2%ldata(1:Ten2%totalData),Ten2%totalData,MPI_logical,ID1,tag,MPI_Comm,istatus,ierr)
				case(7)
					call mpi_recv(Ten2%adata(1:Ten2%totalData),max_len_of_char_in_TData*Ten2%totalData,&
																		MPI_character,ID1,tag,MPI_Comm,istatus,ierr)
			end select
		end if
		return
	end subroutine
	
	subroutine BCAST_TData(Ten1,ID,ierr,MPIcommon)
		type(TData),intent(inout)::Ten1
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
		
		if(present(MPIcommon))then
			if(ID.ge.proNum)return
		end if
		
!******************************flag***********************************************			
		call MPI_BCAST(Ten1%flag,1,MPI_logical,ID,mpi_comm,ierr)
		if(.not.Ten1%flag) then
			return
		end if
!**************************totalData*********************************************		
		call MPI_BCAST(Ten1%totalData,1,MPI_integer,ID,mpi_comm,ierr)
!**************************DynamicClass*********************************************
		call MPI_BCAST(Ten1%DynamicClass,1,MPI_logical,ID,mpi_comm,ierr)
!**************************classtype*********************************************		
		call MPI_BCAST(Ten1%classtype,1,MPI_integer,ID,mpi_comm,ierr)
!**********************************************************************************	
		if(proID.ne.ID) then
			call allocateTData(Ten1,Ten1%totalData)
		end if
		select case(Ten1%classtype)
			case(1)
				call mpi_BCAST(Ten1%idata(1:Ten1%totalData),Ten1%totalData,MPI_integer,ID,MPI_Comm,ierr)
			case(2)
				call mpi_BCAST(Ten1%sdata(1:Ten1%totalData),Ten1%totalData,MPI_real,ID,MPI_Comm,ierr)
			case(3)
				call mpi_BCAST(Ten1%ddata(1:Ten1%totalData),Ten1%totalData,MPI_double_precision,ID,MPI_Comm,ierr)
			case(4)
				call mpi_BCAST(Ten1%cdata(1:Ten1%totalData),Ten1%totalData,MPI_complex,ID,MPI_Comm,ierr)
			case(5)
				call mpi_BCAST(Ten1%zdata(1:Ten1%totalData),Ten1%totalData,MPI_double_complex,ID,MPI_Comm,ierr)
			case(6)
				call mpi_BCAST(Ten1%ldata(1:Ten1%totalData),Ten1%totalData,MPI_logical,ID,MPI_Comm,ierr)
			case(7)
				call mpi_BCAST(Ten1%adata(1:Ten1%totalData),max_len_of_char_in_TData*Ten1%totalData,&
															MPI_character,ID,MPI_Comm,ierr) 
		end select
!**********************************************************************************	
		return
	end subroutine
	
	
	
	
	
	subroutine MPI_SUM_TData(inTData,outTData,ierr,MPIcommon)!Do not check input data and do no allocate memery
		type(TData),intent(inout)::outTData
		type(TData)::inTData
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,len2,istatus(MPI_STATUS_SIZE),mpi_comm
		integer::totalData
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		tag=1
		totalData=outTData%TotalData
		select case(inTData%classtype)
			case(1)
				call MPI_ALLREDUCE(inTData%idata(1:TotalData),outTData%idata(1:TotalData),&
											inTData%totalData,MPI_INTEGER,MPI_SUM,mpi_comm,ierr)
			case(2)
				call MPI_ALLREDUCE(inTData%sdata(1:TotalData),outTData%sdata(1:TotalData),&
											inTData%totalData,MPI_real,MPI_SUM,mpi_comm,ierr)
			case(3)
				call MPI_ALLREDUCE(inTData%ddata(1:TotalData),outTData%ddata(1:TotalData),&
											inTData%totalData,MPI_double_precision,MPI_SUM,mpi_comm,ierr)
			case(4)
				call MPI_ALLREDUCE(inTData%cdata(1:TotalData),outTData%cdata(1:TotalData),&
											inTData%totalData,MPI_complex,MPI_SUM,mpi_comm,ierr)
			case(5)
				call MPI_ALLREDUCE(inTData%zdata(1:TotalData),outTData%zdata(1:TotalData),&
											inTData%totalData,MPI_double_complex,MPI_SUM,mpi_comm,ierr)
		end select
		return
	end subroutine
	
	subroutine MPI_MAX_TData(inTData,outTData,ierr,MPIcommon)!Do not check input data and do no allocate memery
		type(TData),intent(inout)::outTData
		type(TData)::inTData
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,len2,istatus(MPI_STATUS_SIZE),mpi_comm,totalData
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		tag=1
		totalData=outTData%TotalData
		select case(inTData%classtype)
			case(1)
				call MPI_ALLREDUCE(inTData%idata(1:TotalData),outTData%idata(1:TotalData),&
											inTData%totalData,MPI_INTEGER,MPI_MAX,mpi_comm,ierr)
			case(2)
				call MPI_ALLREDUCE(inTData%sdata(1:TotalData),outTData%sdata(1:TotalData),&
											inTData%totalData,MPI_real,MPI_MAX,mpi_comm,ierr)
			case(3)
				call MPI_ALLREDUCE(inTData%ddata(1:TotalData),outTData%ddata(1:TotalData),&
											inTData%totalData,MPI_double_precision,MPI_MAX,mpi_comm,ierr)
			case default
				call writemess('ERROR in MAX Tensor')
				call error_stop
		end select
		return
	end subroutine
	
	subroutine MPI_MIN_TData(inTData,outTData,ierr,MPIcommon)!Do not check input data and do no allocate memery
		type(TData),intent(inout)::outTData
		type(TData)::inTData
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,len2,istatus(MPI_STATUS_SIZE),mpi_comm,totalData
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		tag=1
		totalData=outTData%TotalData
		select case(inTData%classtype)
			case(1)
				call MPI_ALLREDUCE(inTData%idata(1:TotalData),outTData%idata(1:TotalData),&
											inTData%totalData,MPI_INTEGER,MPI_MIN,mpi_comm,ierr)
			case(2)
				call MPI_ALLREDUCE(inTData%sdata(1:TotalData),outTData%sdata(1:TotalData),&
											inTData%totalData,MPI_real,MPI_MIN,mpi_comm,ierr)
			case(3)
				call MPI_ALLREDUCE(inTData%ddata(1:TotalData),outTData%ddata(1:TotalData),&
											inTData%totalData,MPI_double_precision,MPI_MIN,mpi_comm,ierr)
			case default
				call writemess('ERROR in MAX Tensor')
				call error_stop
		end select
		return
	end subroutine
		
	
	
	
	
	
	
	
	
	

end module
