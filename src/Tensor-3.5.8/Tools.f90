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
!         Buddha blessed , no BUG 
! Reany bugs of the TNSP to sj.dong@outlook.com
    
module Tools
	use mpi
	implicit none
	private
!********************************************
!Here define the commom data
	real*8,public,parameter::default_zero_double_number=1d-15
	real*4,public,parameter::default_zero_real_number=1e-7
	real*8,public,parameter::default_max_double_number=1d300! max double is +-1.79*10^308
	real*4,public,parameter::default_max_real_number=1e30! max real is +-3.4*10^38
	integer,public,parameter::default_max_integer_number=2107483647!max integer is -2^31 to 2^31-1
	integer,public,save::LAPACK_LENGTH=0!if the number of the data larger than LAPACK_LENGTH, call the lapack routine
	integer,public,save::LAPACK_LENGTH2=0


!     used in     Tensor.f90
	integer,public,parameter::default_classtype_in_Tensor=5
	logical,public,parameter::default_DynamicClass_in_Tensor=.true.
	integer,public,parameter::max_len_of_char_in_TData=100
	logical,public,save::deallocate_memory_flag=.false.
	integer,public,parameter::characterLen=max_len_of_char_in_TData
	character*1,public::dag_mark='+'

!     used in     Dimension.f90	
	CHARACTER*1,public,save::indexsymbol='.'
	CHARACTER*1,public,save::intNamesymbol='_'!!0_0_1.0_1_2
	integer,public,save::len_of_intTensorName=1
	integer,public,save::len_of_intDimenName=2
	integer,public,parameter::len_of_Name=20
	integer,public,parameter::len_of_intName_in_type_define=4!Cannot define using the value that is not parameter
	logical,public::check_same_name_flag=.false.
!     used in  this file
	integer,public,save::max_len_of_char=5000
	character(len=12)::formInt='(I0)'
	character(len=12)::formreal4='(F25.8)'
	character(len=12)::formreal8='(F50.16)'
	character(len=12)::formlarge='(ES20.10E5)'
!     used in     SymTensor.f90	
	logical,public::ProductTensor_output_check_flag=.false.
!********************************************
!***************END***********************

	logical,private::error_backtrace_Flag=.false.
	logical,private::rewriteFlag1=.false.
	logical,private::rewriteFlag2=.false.
	integer,private,save::randomseed
	logical,private,save::seed_flag=.false.
	integer,private,save::initial_randomseed=0
	integer,private,save::initial_mpi_randomseed_in_cpus=0
	!use for output mess
	integer,save,private::output_cpu_number=0!write into the output files
	integer,save,private::output_cpu_number2=0!print
	logical,save,private::log_flag=.false.!if false,there is no log,create a new file
	CHARACTER*100,private::log_address
	logical,save,private::out_log_flag=.false.!If true, write output on the log file
	logical,save,private::MPI_running=.false.!if true,that means there are more than 1 cpus running
	integer,save,private::		log_address_unit=9999
	logical,save,private::Time_calculater_start_flag=.true.
	integer,save,private::Time_calculater_TotalStep=1
	integer,save,private::Time_calculater_numOutput=1
	integer,save,private::Time_calculater_limit_time=0
	integer,save,private::persentCharLen=30
	integer,save,private::persentCharLen2=12
!****************************************************************************
	!	MPI parameter
	integer,public,save::output_ProID=0,output_ProNum=1,output_Ierr=1
	integer,private,parameter::IDmin=0
	!real*8,external::omp_get_wtime
	public::writemess
	interface writemess
		module procedure writemess_char
		module procedure writemess_char2
		module procedure writemess_char_form
		module procedure writemess_real
		module procedure writemess_real4
		module procedure writemess_logi
		module procedure writemess_int
		module procedure writemess_com4
		module procedure writemess_com8
		module procedure writemess_real_array
		module procedure writemess_real4_array
		module procedure writemess_logi_array
		module procedure writemess_int_array
		module procedure writemess_com4_array
		module procedure writemess_com8_array
		module procedure writemess_real_form
		module procedure writemess_real4_form
		module procedure writemess_int_form
		module procedure writemess_com4_form
		module procedure writemess_com8_form
		module procedure writemess_real_array_form
		module procedure writemess_real4_array_form
		module procedure writemess_int_array_form
		module procedure writemess_com4_array_form
		module procedure writemess_com8_array_form
	end interface	

	public::sortData
	interface sortData!Bubble Sort
		module procedure sortCom4
		module procedure sortCom4_
		module procedure sortCom8
		module procedure sortCom8_
		module procedure sortreal8
		module procedure sortreal8_
		module procedure sortreal4
		module procedure sortreal4_
		module procedure sortint
		module procedure sortint_
	end interface

	public::maxvalue
	interface maxvalue
		module procedure maxvalue_real
	end interface

	public::allocateCheck
	interface allocateCheck! if size(A)<lenA then allocate A,else do nothing
		module procedure allocateCheck_int
		module procedure allocateCheck_real4
		module procedure allocateCheck_real
		module procedure allocateCheck_com4
		module procedure allocateCheck_com
		module procedure allocateCheck_logi
		module procedure allocateCheck_char
	end interface
	
	public::operator(.ltne.)
	interface operator(.ltne.)
		module procedure lt_ne_fun
	end interface
	
	public::system_time
	interface system_time
		module procedure system_time1
		module procedure system_time2
	end interface
	

	public::set_lapack_length
	interface set_lapack_length
		module procedure set_lapack_length1
		module procedure set_lapack_length2
	end interface
	

	public::randomnumber
	interface randomnumber
		module procedure randomnumber1
		module procedure randomnumber2
		module procedure randomnumber3
		module procedure randomnumber4
	end interface
	

	public::System_time_calculater
	interface System_time_calculater
		module procedure System_time_calculater1!(stepi,totalstep,numOutput,firstnum),start from stepi=1
		module procedure System_time_calculater2
	end interface
	!reset_Time_calculator(totalstep,optional(numOutput))!inital the time calculater
	!Time_calculator()! it is the same as System_time_calculater
	!reset_Time_calculator_limit (totalstep,limit_time,optional(numOutput))!inital the time calculater_limit
	!time_calculator_limit ! if using time > time_calculator_limit_time, output .false. else output .true.
	
	public::operator(+)
	interface operator(+)
		module procedure charAdd
		module procedure charAddint
		module procedure intAddchar
		module procedure charAddreal
		module procedure realAddchar
		module procedure charAddreal4
		module procedure real4Addchar
		module procedure charAddlogi
		module procedure logiAddchar
		module procedure com8Addchar
		module procedure com4Addchar
		module procedure charAddcom4
		module procedure charAddcom8
	end interface

	
!cha='abcdefgh'
! cha.subL.'c'='ab'
! cha.subR.'c'='defgh'
! cha.sub.['c','g']='def'
	public::operator(.subL.)
	interface operator(.subL.)
		module procedure SubCharleft
	end interface

	public::operator(.subR.)
	interface operator(.subR.)
		module procedure SubCharRight
	end interface

	public::operator(.sub.)
	interface operator(.sub.)
		module procedure SubChar
	end interface

	public::initial_output_cpu_info,initial_mpi
	interface initial_mpi
		module procedure initial_output_cpu_info
	end interface


	public::initial_log,set_output_log_address
	interface initial_log
		module procedure set_output_log_address
	end interface

	public::set_MPI_log,set_output_MPI_log
	interface set_MPI_log
		module procedure set_output_MPI_log
	end interface


	public::set_error_pointer,set_error_backtrace
	interface set_error_pointer
		module procedure set_error_backtrace
	end interface


	public::set_seed
	interface set_seed
		module procedure set_seed1
		module procedure set_seed2
	end interface



	

	public::operator(.equ.)
	interface operator(.equ.)
		module procedure  equal_character
		module procedure  equal_real4
		module procedure  equal_real8
		module procedure  equal_com4
		module procedure  equal_com8
		module procedure  equal_of_array_real4
		module procedure  equal_of_array_real8
		module procedure  equal_of_array_com4
		module procedure  equal_of_array_com8
		module procedure  equal_of_array_char
		module procedure  equal_of_array
	end interface
	
	public::operator(.nequ.)
	interface operator(.nequ.)
		module procedure  nequal_character
		module procedure  nequal_real4
		module procedure  nequal_real8
		module procedure  nequal_com4
		module procedure  nequal_com8
	end interface
	
	public::assignment(=)
	interface assignment(=)
		module procedure charset
		module procedure charset_array
		module procedure charset_array2
		module procedure charsetreal4
		module procedure charsetreal4_array
		module procedure charsetreal4_array2
		module procedure charsetreal8
		module procedure charsetreal8_array
		module procedure charsetreal8_array2
		module procedure charsetcom4
		module procedure charsetcom4_array
		module procedure charsetcom4_array2
		module procedure charsetcom8
		module procedure charsetcom8_array
		module procedure charsetcom8_array2
		module procedure charsetlogi
		module procedure charsetlogi_array
		module procedure charsetlogi_array2
		module procedure charsetChar_array2
	end interface	

	public::unset_error_backtrace
	public::set_persent_Len,set_lapack_length1,set_lapack_length2
	public::set_writing_type,reset_writing_type,set_writing_type_scientific,unset_writing_type_scientific
	public::unset_check_dimension,set_check_dimension
	public::set_deallocate_memory_flag,unset_deallocate_memory_flag,set_check_dimension_no_use
	public::set_max_len_of_cha
	public::set_output_cpu_info
	public::set_output_log_unit,set_output_cpu,stop_program,error_stop

	public::inde_counter

	public::open_File,ifopen_File

	public::outputmessTime,reset_Time_calculator,Time_calculator,reset_Time_calculator_limit,time_calculator_limit

	public::out_randomseed,out_initial_randomseed,out_initial_mpi_randomseed,out_and_set_seed
	public::IndesToaddressRoutine,IndesToaddress
	public::iselect,sselect,dselect,cselect,zselect



!icopy(N,SX,INCX,SY,INCY) : BLAS scopy
contains
	subroutine set_error_backtrace()
		error_backtrace_Flag=.true.
		call writemess(' ')
		call writemess('#############   Set the error_backtrace  ##################')
		call writemess(' The error_backtrace can print the location of the bugs!')
		call writemess(' Add the code: ')
		call writemess('	 -g -static -ffpe-trap=invalid  ')
		call writemess(' when compiling your files ')
		call writemess(' Example: ')
		call writemess('    mpif90 -g -static -ffpe-trap=invalid test.f90 -o test')
		call writemess('###########################################################')
		call writemess(' ')
		return
	end subroutine
	subroutine unset_error_backtrace()
		error_backtrace_Flag=.false.
		call writemess(' unSet the error_backtrace')
	end subroutine
	subroutine set_persent_Len(length)
		integer,intent(in)::length
		persentCharLen=length
		persentCharLen2=persentCharLen/2-2
		return
	end subroutine
	subroutine set_lapack_length1(length)
		integer,intent(in)::length
		LAPACK_LENGTH=length
		LAPACK_LENGTH2=length+length
		return
	end subroutine
	subroutine set_lapack_length2(length1,length2)
		integer,intent(in)::length1,length2
		LAPACK_LENGTH=length1
		LAPACK_LENGTH2=length2
		return
	end subroutine
	
	subroutine set_writing_type(form,typ)
		character(len=*),intent(in)::form,typ
		if(trim(adjustl(typ)).eq.'integer') then
			formInt=form
			return
		end if
		if((trim(adjustl(typ)).eq.'real*4').or.(trim(adjustl(typ)).eq.'real(kind=4)')&
																.or.(trim(adjustl(typ)).eq.'real')) then
			formreal4=form
			return
		end if
		if((trim(adjustl(typ)).eq.'real*8').or.(trim(adjustl(typ)).eq.'real(kind=8)')&
																.or.(trim(adjustl(typ)).eq.'dble')) then
			formreal8=form
			return
		end if
		write(*,*)"ERROR in set_writemess_type"
		call error_stop()
	end subroutine
	subroutine reset_writing_type()
		formInt='(I0)'
		formreal4='(F25.8)'
		formreal8='(F50.16)'
		formlarge='(ES20.10E5)'
		return
	end subroutine
	
	subroutine unset_check_dimension()
		check_same_name_flag=.false.
		ProductTensor_output_check_flag=.false.
		call writemess('Do not check dimension Name')
		return
	end subroutine
	
	subroutine set_check_dimension()
		check_same_name_flag=.true.
		ProductTensor_output_check_flag=.true.
		call writemess('check dimension Name')
		return
	end subroutine
	
	subroutine set_deallocate_memory_flag()
		deallocate_memory_flag=.true.
		call writemess('deallocate memory after using them')
		return
	end subroutine
	subroutine unset_deallocate_memory_flag()
		deallocate_memory_flag=.false.
		call writemess('do not  deallocate memory after using them')
		return
	end subroutine
	subroutine set_check_dimension_no_use()
		check_same_name_flag=.true.
		ProductTensor_output_check_flag=.true.
		return
	end subroutine
	
	subroutine set_writing_type_scientific()
		formreal4=formlarge
		formreal8=formlarge
		return
	end subroutine
	subroutine unset_writing_type_scientific()
		formreal4='(F25.8)'
		formreal8='(F50.16)'
		return
	end subroutine
	subroutine set_max_len_of_cha(maxlen)
		integer,intent(in)::maxlen
		max_len_of_char=maxlen
		return
	end subroutine
	subroutine initial_output_cpu_info(id,num,ierr,MPICOMM)
		integer,intent(out)::id,num,ierr
		integer,optional,intent(inout)::MPICOMM
		call MPI_INIT(output_Ierr) !!MPI initializing
		if(present(MPICOMM)) then
			call MPI_Comm_rank(MPICOMM,output_ProID,output_Ierr) !!set MPI ranks for each process
	  		call MPI_Comm_size(MPICOMM, output_ProNum, output_Ierr) !!set MPI sizes
  		else
	  		call MPI_Comm_rank(MPI_COMM_WORLD,output_ProID,output_Ierr) !!set MPI ranks for each process
	  		call MPI_Comm_size(MPI_COMM_WORLD, output_ProNum, output_Ierr) !!set MPI sizes
	  	end if
	  	id=output_ProID
		num=output_ProNum
		ierr=output_Ierr
		if(output_ProNum.gt.1)then	
			MPI_running=.true.
		else
			MPI_running=.false.
		end if
		log_flag=.false.
		return
	end subroutine
	subroutine set_output_cpu_info(output_ProID_,output_ProNum_,output_Ierr_)
		integer,intent(in)::output_ProID_,output_ProNum_,output_Ierr_
		output_ProID=output_ProID_
		output_ProNum=output_ProNum_
		output_Ierr=output_Ierr_
		if(output_ProNum.gt.1)then	
			MPI_running=.true.
		else
			MPI_running=.false.
		end if
		log_flag=.false.
		return
	end subroutine
	subroutine set_output_log_address(address,notOverWrite)
		CHARACTER(len=*),intent(in)::address
		CHARACTER(len=*),optional,intent(in)::notOverWrite
		logical::alive
		log_address=address
		out_log_flag=.true.
		log_flag=.false.!replace the log_file
		if(present(notOverWrite))then
			if(notOverWrite.nequ.'overwrite')then
				inquire(file=address,exist=alive)
				log_flag=alive
			endif
		end if
		return
	end subroutine
	subroutine set_output_MPI_log(notOverWrite)
		CHARACTER(len=*),optional,intent(in)::notOverWrite
		logical::alive
		logical,save::first_flag=.true.
		if(.not.out_log_flag)then
			call writemess('Set the log address first by calling set_output_log_address(address,notOverWrite)')
			call error_stop
		end if
		if(first_flag)then
			first_flag=.false.
		else
			call writemess('The program have set the MPI log before')
			call error_stop
		end if
		call writemess('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
		call writemess('% Set log files for every cpu')
		call writemess('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
		if(output_ProID.eq.output_cpu_number)return
		log_address=log_address+output_ProID
		log_flag=.false.!replace the log_file
		output_cpu_number=output_ProID
		if(present(notOverWrite))then
			if(notOverWrite.nequ.'overwrite')then
				inquire(file=log_address,exist=alive)
				log_flag=alive
			endif
		end if
		call writemess('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
		call writemess('%  This is the output log of cpu'+output_ProID)
		call writemess('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
		call writemess(' ')
		call writemess(' ')
		call writemess(' ')
		return
	end subroutine
	
	subroutine set_output_log_unit(logunit)
		integer,intent(in)::logunit
		log_address_unit=logunit
		return
	end subroutine
	subroutine set_output_cpu(cpu)
		integer,intent(in)::cpu
		output_cpu_number=cpu
		output_cpu_number2=cpu
		return
	end subroutine
	subroutine stop_program()! no bug, stop
		if(MPI_running)then
			call MPI_FINALIZE( output_ierr )
			stop
		end if
		stop
	end subroutine
	subroutine error_stop()! bug , stop
		if(MPI_running)then
			if(seed_flag)call writemess('The random seed is'+initial_randomseed)
			if(seed_flag)call writemess('The random seed in cpu is'+initial_mpi_randomseed_in_cpus,-1)
			call writemess('    Running CPU number is   '+(' '+output_ProNum))
			call writemess('    All cups are going to stop   ')
			call writemess('    ')
			call outpicture()
			call sleep(2)
			if(error_backtrace_Flag) CALL BACKTRACE
			!call MPI_FINALIZE( output_ierr )
			stop
		end if
		if(seed_flag)call writemess('The random seed is,seed='+initial_randomseed)
		call outpicture()
		if(error_backtrace_Flag)CALL BACKTRACE 
		stop
	end subroutine	
	
	logical function lt_ne_fun(a,b)
		integer,intent(in)::a,b
		if(deallocate_memory_flag)then
			lt_ne_fun=a.ne.b
		else
			lt_ne_fun=a.lt.b
		end if
		return
	end function 
	
	subroutine allocateCheck_int(A,lenA)! if size(A)<lenA then allocate A,else do nothing
		integer,allocatable,intent(inout)::A(:)
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
	subroutine allocateCheck_real4(A,lenA)! if size(A)<lenA then allocate A,else do nothing
		real(kind=4),allocatable,intent(inout)::A(:)
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
	subroutine allocateCheck_real(A,lenA)! if size(A)<lenA then allocate A,else do nothing
		real*8,allocatable,intent(inout)::A(:)
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
	subroutine allocateCheck_com4(A,lenA)! if size(A)<lenA then allocate A,else do nothing
		complex(kind=4),allocatable,intent(inout)::A(:)
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
	subroutine allocateCheck_com(A,lenA)! if size(A)<lenA then allocate A,else do nothing
		complex(kind=8),allocatable,intent(inout)::A(:)
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
	subroutine allocateCheck_logi(A,lenA)! if size(A)<lenA then allocate A,else do nothing
		logical,allocatable,intent(inout)::A(:)
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
	subroutine allocateCheck_char(A,lenA)! if size(A)<lenA then allocate A,else do nothing
		character(len=*),allocatable,intent(inout)::A(:)
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

!!if[s1,s2,...,sn]=[max_s1,max_s2,..,max_sn] output false and return
!else output ture and do the code below
![s1,s2,...,sn]-->[s1+1,s2,..,sn],delta=1
!if s1+1>max_s1,s1=min_s1 and s2=s2+1
!if s2+1>max_s2,s2=min_s2 and s3=s3+1
!...
!inde are [s1,s2,...,sn]
!minindex are [min_s1,min_s2,..,min_sn]
!maxindex are [max_s1,max_s2,..,max_sn]
	logical function inde_counter(inde,minindex,maxindex,delta)
		integer,intent(inout)::inde(:)
		integer,intent(in)::minindex(:),maxindex(:),delta
		integer::indexlen,i
		indexlen=size(inde)
		if(equal_array_int(inde,maxindex)) then
			inde_counter=.false.
			return
		end if
		
		i=1
		inde_counter=.true.
		do i=1,indexlen
			inde(i)=inde(i)+delta
			if(inde(i).gt.maxindex(i))then
				inde(i)=minindex(i)
			else
				exit
			end if
		end do
		if(delta.gt.1)then
			inde_counter=.true.
			do i=1,indexlen
				inde_counter=inde_counter.and.(inde(i).gt.maxindex(i))
			end do
			inde_counter=.not.inde_counter
			if(.not.inde_counter)return
		end if
		return
	end function
	logical function equal_array_int(array1,array2)result(res)
		integer,intent(in)::array1(:),array2(:)
		integer::i,len1,len2
		res=.true.
		len1=size(array1)
		len2=size(array2)
		if(len1.ne.len2)then
			res=.false.
			return
		end if
		do i=1,len1
			if(array1(i).ne.array2(i))then
				res=.false.
				return
			end if
		end do
		return
	end function
	
	
	
	
	
	
!*****************  Bubble Sort  ****************
!inde output the order of the output
!sort the data form small to big
!if realpart=.true.,then sort base on the real part of the data
! else the imag part
	subroutine sortReal8(a,inde,increase)
		real*8,intent(inout) :: a(:)
		integer,intent(inout):: inde(:)
		logical,intent(in)::increase
		real*8 :: temp
		integer :: i,j,n,tempi
		n=size(a)
		do i=1,n
			inde(i)=i
		end do
		if(increase) then
			do i=1,n-1
				 do j=i+1,n
					  if (a(i) .gt. a(j)) then
							temp = a(i)
							tempi=inde(i)
							a(i) = a(j)
							inde(i)=inde(j)
							a(j) = temp
							inde(j)=tempi
					  endif
				 enddo
			enddo
		else
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
		end if
		return
	end subroutine
	
	subroutine sortReal8_(a,increase)
		real*8,intent(inout) :: a(:)
		logical,intent(in)::increase
		real*8 :: temp
		integer :: i,j,n
		n=size(a)
		if(increase) then
			do i=1,n-1
				 do j=i+1,n
					  if (a(i) .gt. a(j)) then
							temp = a(i)
							a(i) = a(j)
							a(j) = temp
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (a(i) .lt. a(j)) then
							temp = a(i)
							a(i) = a(j)
							a(j) = temp
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine
	
	subroutine sortReal4(a,inde,increase)
		real*4,intent(inout) :: a(:)
		integer,intent(inout):: inde(:)
		logical,intent(in)::increase
		real*4 :: temp
		integer :: i,j,n,tempi
		n=size(a)
		do i=1,n
			inde(i)=i
		end do
		if(increase) then
			do i=1,n-1
				 do j=i+1,n
					  if (a(i) .gt. a(j)) then
							temp = a(i)
							tempi=inde(i)
							a(i) = a(j)
							inde(i)=inde(j)
							a(j) = temp
							inde(j)=tempi
					  endif
				 enddo
			enddo
		else
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
		end if
		return
	end subroutine
	
	subroutine sortReal4_(a,increase)
		real*4,intent(inout) :: a(:)
		logical,intent(in)::increase
		real*4 :: temp
		integer :: i,j,n
		n=size(a)
		if(increase) then
			do i=1,n-1
				 do j=i+1,n
					  if (a(i) .gt. a(j)) then
							temp = a(i)
							a(i) = a(j)
							a(j) = temp
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (a(i) .lt. a(j)) then
							temp = a(i)
							a(i) = a(j)
							a(j) = temp
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine
	subroutine sortint(a,inde,increase)
		integer,intent(inout) :: a(:)
		integer,intent(inout):: inde(:)
		logical,intent(in)::increase
		integer :: temp
		integer :: i,j,n,tempi
		n=size(a)
		do i=1,n
			inde(i)=i
		end do
		if(increase) then
			do i=1,n-1
				 do j=i+1,n
					  if (a(i) .gt. a(j)) then
							temp = a(i)
							tempi=inde(i)
							a(i) = a(j)
							inde(i)=inde(j)
							a(j) = temp
							inde(j)=tempi
					  endif
				 enddo
			enddo
		else
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
		end if
		return
	end subroutine
	
	subroutine sortint_(a,increase)
		integer,intent(inout) :: a(:)
		logical,intent(in)::increase
		integer :: temp
		integer :: i,j,n
		n=size(a)
		if(increase) then
			do i=1,n-1
				 do j=i+1,n
					  if (a(i) .gt. a(j)) then
							temp = a(i)
							a(i) = a(j)
							a(j) = temp
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (a(i) .lt. a(j)) then
							temp = a(i)
							a(i) = a(j)
							a(j) = temp
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine
	
	subroutine sortCom8(a,inde,realpart,increase)
		complex*16,intent(inout) :: a(:)
		integer,intent(inout):: inde(:)
		logical,intent(in)::realpart,increase
		complex*16 :: temp
		integer :: i,j,n,tempi
		n=size(a)
		do i=1,n
			inde(i)=i
		end do
		if(increase) then
			call sort1(a,inde,realpart)
			return
		end if
		if(realpart) then
			do i=1,n-1
				 do j=i+1,n
					  if (dreal(a(i)) .lt. dreal(a(j))) then
						   temp = a(i)
						   tempi=inde(i)
						   a(i) = a(j)
						   inde(i)=inde(j)
						   a(j) = temp
						   inde(j)=tempi
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (aimag(a(i)) .lt. aimag(a(j))) then
						   temp = a(i)
						   tempi=inde(i)
						   a(i) = a(j)
						   inde(i)=inde(j)
						   a(j) = temp
						   inde(j)=tempi
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine
	subroutine sortCom8_(a,realpart,increase)
		complex*16,intent(inout) :: a(:)
		logical,intent(in)::realpart,increase
		complex*16 :: temp
		integer :: i,j,n
		n=size(a)
		if(increase) then
			call sort2(a,realpart)
			return
		end if
		if(realpart) then
			do i=1,n-1
				 do j=i+1,n
					  if (dreal(a(i)) .le. dreal(a(j))) then
						   temp = a(i)
						   a(i) = a(j)
						   a(j) = temp
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (aimag(a(i)) .le. aimag(a(j))) then
						   temp = a(i)
						   a(i) = a(j)
						   a(j) = temp
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine
	subroutine sortCom4(a,inde,realpart,increase)
		complex*8,intent(inout) :: a(:)
		integer,intent(inout):: inde(:)
		logical,intent(in)::realpart,increase
		complex*8 :: temp
		integer :: i,j,n,tempi
		n=size(a)
		do i=1,n
			inde(i)=i
		end do
		if(increase) then
			call sort3(a,inde,realpart)
			return
		end if
		if(realpart) then
			do i=1,n-1
				 do j=i+1,n
					  if (real(a(i),kind=4) .lt. real(a(j),kind=4)) then
						   temp = a(i)
						   tempi=inde(i)
						   a(i) = a(j)
						   inde(i)=inde(j)
						   a(j) = temp
						   inde(j)=tempi
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (aimag(a(i)) .lt. aimag(a(j))) then
						   temp = a(i)
						   tempi=inde(i)
						   a(i) = a(j)
						   inde(i)=inde(j)
						   a(j) = temp
						   inde(j)=tempi
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine
	subroutine sortCom4_(a,realpart,increase)
		complex*8,intent(inout) :: a(:)
		logical,intent(in)::realpart,increase
		complex*8 :: temp
		integer :: i,j,n
		n=size(a)
		if(increase) then
			call sort4(a,realpart)
			return
		end if
		if(realpart) then
			do i=1,n-1
				 do j=i+1,n
					  if (real(a(i),kind=4) .le. real(a(j),kind=4)) then
						   temp = a(i)
						   a(i) = a(j)
						   a(j) = temp
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (aimag(a(i)) .le. aimag(a(j)) )then
						   temp = a(i)
						   a(i) = a(j)
						   a(j) = temp
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine


	subroutine sort1(a,inde,realpart)
		complex*16,intent(inout) :: a(:)
		integer,intent(inout):: inde(:)
		logical,intent(in)::realpart
		complex*16 :: temp
		integer :: i,j,n,tempi
		n=size(a)
		do i=1,n
			inde(i)=i
		end do
		if(realpart) then
			do i=1,n-1
				 do j=i+1,n
					  if (dreal(a(i)) .gt. dreal(a(j))) then
						   temp = a(i)
						   tempi=inde(i)
						   a(i) = a(j)
						   inde(i)=inde(j)
						   a(j) = temp
						   inde(j)=tempi
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (aimag(a(i)) .gt. aimag(a(j))) then
						   temp = a(i)
						   tempi=inde(i)
						   a(i) = a(j)
						   inde(i)=inde(j)
						   a(j) = temp
						   inde(j)=tempi
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine
	
	subroutine sort2(a,realpart)
		complex*16,intent(inout) :: a(:)
		logical,intent(in)::realpart
		complex*16 :: temp
		integer :: i,j,n
		n=size(a)
		if(realpart) then
			do i=1,n-1
				 do j=i+1,n
					  if (dreal(a(i)) .gt. dreal(a(j))) then
						   temp = a(i)
						   a(i) = a(j)
						   a(j) = temp
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (aimag(a(i)) .gt. aimag(a(j))) then
						   temp = a(i)
						   a(i) = a(j)
						   a(j) = temp
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine
	subroutine sort3(a,inde,realpart)
		complex*8,intent(inout) :: a(:)
		integer,intent(inout):: inde(:)
		logical,intent(in)::realpart
		complex*8 :: temp
		integer :: i,j,n,tempi
		n=size(a)
		do i=1,n
			inde(i)=i
		end do
		if(realpart) then
			do i=1,n-1
				 do j=i+1,n
					  if (real(a(i),kind=4) .gt. real(a(j),kind=4)) then
						   temp = a(i)
						   tempi=inde(i)
						   a(i) = a(j)
						   inde(i)=inde(j)
						   a(j) = temp
						   inde(j)=tempi
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (aimag(a(i)) .gt. aimag(a(j))) then
						   temp = a(i)
						   tempi=inde(i)
						   a(i) = a(j)
						   inde(i)=inde(j)
						   a(j) = temp
						   inde(j)=tempi
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine
	
	subroutine sort4(a,realpart)
		complex*8,intent(inout) :: a(:)
		logical,intent(in)::realpart
		complex*8 :: temp
		integer :: i,j,n
		n=size(a)
		if(realpart) then
			do i=1,n-1
				 do j=i+1,n
					  if (real(a(i),kind=4) .gt. real(a(j),kind=4)) then
						   temp = a(i)
						   a(i) = a(j)
						   a(j) = temp
					  endif
				 enddo
			enddo
		else
			do i=1,n-1
				 do j=i+1,n
					  if (aimag(a(i)) .gt. aimag(a(j))) then
						   temp = a(i)
						   a(i) = a(j)
						   a(j) = temp
					  endif
				 enddo
			enddo
		end if
		return
	end subroutine



! find the max value of a
! the line element of a is the max
	subroutine maxvalue_real(a,line,maxel)
		real*8,intent(in)::a(:)
		real*8,intent(inout)::maxel
		integer,intent(inout)::line
		real*8 :: temp
		integer :: i,n
		n=size(a)
		temp=a(1)
		line=1
		do i=2,n
			if(a(i).gt.temp) then
				temp=a(i)
				line=i
			end if
		end do
		maxel=temp
		return
	end subroutine
	
		
!	character(len=(max(len(w1)+len(w2),max_len_of_char))) function charAdd(w1,w2)
	character(len=max_len_of_char) function charAdd(w1,w2)
		character(len=*),intent(in)::w1,w2
		if(len_trim(w1).eq.0)then
			charAdd=' '//(trim(w2))
			return
		end if
		charAdd=(trim(w1))//(trim(w2))
		!A='A'+'B'  					~~~~ using time:1d-2
		!A=(trim('A'))//(trim('B')) ~~~~ using time:1d-4
		return
	end function
	subroutine charset(w,inte)
		character(len=*),intent(inout)::w
		integer,intent(in)::inte
		integer(1)::temp(20),i,j,st
		integer::temp2
		if(inte.eq.0)then
			w='0'
			return
		else if (inte.gt.0)then
			temp2=inte
			w=''
			st=0
		else
			temp2=-inte
			w='-'
			st=1
		end if

		i=0
		do while(temp2.gt.0)
			i=i+1
			temp(i)=mod(temp2,10)
			temp2=temp2/10
		end do
		if(len(w).lt.i)then
			call writemess('The input character is too short to store the data. in character=integer')
			call error_stop
		end if
		do j=1,i
			w(st+j:st+j)=char(temp(i+1-j)+48)
		end do
		return
	end subroutine
	subroutine charset_array(w,inte)
		character(len=*),intent(inout)::w(:)
		integer,intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		if(size(w).lt.length)then
			write(*,*)"ERROR in charset_array"
			stop
		end if
		do i=1,length
			w(i)=inte(i)
		end do
		return
	end subroutine
	subroutine charset_array2(w,inte)
		character(len=*),intent(inout)::w
		integer,intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		w=inte(1)+','
		do i=2,length-1
			w=inte(i)+','
		end do
		w=inte(length)
		return
	end subroutine

	subroutine charsetreal4(str,inte)
		character(len=*),intent(inout)::str
		real*4,intent(in)::inte
		real(8)  :: num
		real(8) :: temp2
		integer :: appro,tenexp,tenexp2,lenint
		integer(1) :: temp(10),i,j,k,dotpos,st,len_not0
		real(8) :: compare(8)=[1d-2,1d-1,1d0,1d1,1d2,1d3,1d4,1d5]
		character(len=10)::tempchar

		num=inte

		if (num>0d0) then
			temp2=num*(1+1d-10)
			str=''
			st=0
		else if(num<0d0) then
			temp2=-num*(1+1d-10)
			str='-'
			st=1
		else
			str='0'
			return
		end if
		if(temp2>compare(size(compare)) .or. temp2<compare(1))then
			tenexp=0
			if(temp2>1)then
				do while(temp2>=10)
					tenexp=tenexp+1
					temp2=temp2/10
				end do
			else if (temp2<1)then
				do while(temp2<1)
					tenexp=tenexp-1
					temp2=temp2*10
				end do
			end if

			appro=nint(temp2*1d5)
			i=0
			do while(appro>0)
				i=i+1
				temp(i)=mod(appro,10)
				appro=appro/10
			end do

			k=0
			do while(temp(k+1)==0)
				k=k+1
			end do

			str(st+1:st+1)=char(temp(i)+48)
			st=st+1
			if(i-k>=2)then
				str(st+1:st+1)='.'
				do j=2,i-k
					str(st+j:st+j)=char(temp(i+1-j)+48)
				end do	
				st=st+i-k
			end if
			str(st+1:st+1)='E'
			tempchar=tenexp
			str(st+2:)=tempchar
		else
			dotpos=count(temp2>compare)
			appro=nint(temp2/compare(dotpos)*1d5)
			dotpos=dotpos-count(0.9>compare)

			i=0
			do while(appro>0)
				i=i+1
				temp(i)=mod(appro,10)
				appro=appro/10
			end do
			k=0
			do while(temp(k+1)==0)
				k=k+1
			end do

			if(dotpos<=0)then
				str(st+1:st+2)='0.'
				if(dotpos<0) str(st+3:st+4-dotpos)=repeat('0',-dotpos)
				st=st+2-dotpos
				do j=1,i-k
					str(st+j:st+j)=char(temp(i+1-j)+48)
				end do	
			else
				do j=1,dotpos
					str(st+j:st+j)=char(temp(i+1-j)+48)
				end do
				if(i-k>dotpos)then
					str(st+dotpos+1:st+dotpos+1)='.'
					do j=dotpos+1,i-k
						str(st+j+1:st+j+1)=char(temp(i+1-j)+48)
					end do	
				end if
			end if
		end if
		return
	end subroutine
	subroutine charsetreal4_array(w,inte)
		character(len=*),intent(inout)::w(:)
		real*4,intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		if(size(w).lt.length)then
			write(*,*)"ERROR in charsetreal4_array"
			stop
		end if
		do i=1,length
			w(i)=inte(i)
		end do
		return
	end subroutine
	
	subroutine charsetreal4_array2(w,inte)
		character(len=*),intent(inout)::w
		real*4,intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		w=inte(1)+','
		do i=2,length-1
			w=inte(i)+','
		end do
		w=inte(length)
		return
	end subroutine

	subroutine charsetreal8(str,num)
		character(len=*),intent(inout)::str
		real*8,intent(in)::num
		real(8) :: temp2
		integer :: appro,tenexp,tenexp2,lenint
		integer(1) :: temp(10),i,j,k,dotpos,st,len_not0
		real(8) :: compare(8)=[1d-2,1d-1,1d0,1d1,1d2,1d3,1d4,1d5]
		character(len=10)::tempchar

		if (num>0d0) then
			temp2=num*(1+1d-10)
			str=''
			st=0
		else if(num<0d0) then
			temp2=-num*(1+1d-10)
			str='-'
			st=1
		else
			str='0'
			return
		end if
		if(temp2>compare(size(compare)) .or. temp2<compare(1))then
			tenexp=0
			if(temp2>1)then
				do while(temp2>=10)
					tenexp=tenexp+1
					temp2=temp2/10
				end do
			else if (temp2<1)then
				do while(temp2<1)
					tenexp=tenexp-1
					temp2=temp2*10
				end do
			end if

			appro=nint(temp2*1d5)
			i=0
			do while(appro>0)
				i=i+1
				temp(i)=mod(appro,10)
				appro=appro/10
			end do

			k=0
			do while(temp(k+1)==0)
				k=k+1
			end do

			str(st+1:st+1)=char(temp(i)+48)
			st=st+1
			if(i-k>=2)then
				str(st+1:st+1)='.'
				do j=2,i-k
					str(st+j:st+j)=char(temp(i+1-j)+48)
				end do	
				st=st+i-k
			end if
			str(st+1:st+1)='E'
			tempchar=tenexp
			str(st+2:)=tempchar
		else
			dotpos=count(temp2>compare)
			appro=nint(temp2/compare(dotpos)*1d5)
			dotpos=dotpos-count(0.9>compare)

			i=0
			do while(appro>0)
				i=i+1
				temp(i)=mod(appro,10)
				appro=appro/10
			end do
			k=0
			do while(temp(k+1)==0)
				k=k+1
			end do

			if(dotpos<=0)then
				str(st+1:st+2)='0.'
				if(dotpos<0) str(st+3:st+4-dotpos)=repeat('0',-dotpos)
				st=st+2-dotpos
				do j=1,i-k
					str(st+j:st+j)=char(temp(i+1-j)+48)
				end do	
			else
				do j=1,dotpos
					str(st+j:st+j)=char(temp(i+1-j)+48)
				end do
				if(i-k>dotpos)then
					str(st+dotpos+1:st+dotpos+1)='.'
					do j=dotpos+1,i-k
						str(st+j+1:st+j+1)=char(temp(i+1-j)+48)
					end do	
				end if
			end if
		end if
		return
	end subroutine
	subroutine charsetreal8_array(w,inte)
		character(len=*),intent(inout)::w(:)
		real*8,intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		if(size(w).lt.length)then
			write(*,*)"ERROR in charsetreal8_array"
			stop
		end if
		do i=1,length
			w(i)=inte(i)
		end do
		return
	end subroutine
	subroutine charsetreal8_array2(w,inte)
		character(len=*),intent(inout)::w
		real*8,intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		w=inte(1)+','
		do i=2,length-1
			w=inte(i)+','
		end do
		w=inte(length)
		return
	end subroutine

	subroutine charsetcom4(w,inte)
		character(len=*),intent(inout)::w
		complex(kind=4),intent(in)::inte
		character*100::tempR,tempI
		real*4::inum
		tempR=real(inte)
		tempI=aimag(inte)
		inum=aimag(inte)
		if(inum.ge.0.)then
			w=(trim(adjustl(tempR)))//'+i'//(trim(adjustl(tempI)))
		else
			w=(trim(adjustl(tempR)))//'-i'//(trim(adjustl(tempI)))
		end if
		return
	end subroutine
	subroutine charsetcom4_array(w,inte)
		character(len=*),intent(inout)::w(:)
		complex(kind=4),intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		if(size(w).lt.length)then
			write(*,*)"ERROR in charsetreal8_array"
			stop
		end if
		do i=1,length
			w(i)=inte(i)
		end do
		return
	end subroutine
	subroutine charsetcom4_array2(w,inte)
		character(len=*),intent(inout)::w
		complex(kind=4),intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		w=inte(1)+','
		do i=2,length-1
			w=inte(i)+','
		end do
		w=inte(length)
		return
	end subroutine
	subroutine charsetcom8(w,inte)
		character(len=*),intent(inout)::w
		complex(kind=8),intent(in)::inte
		character*100::tempR,tempI
		real*8::inum
		tempR=real(inte)
		tempI=aimag(inte)
		inum=aimag(inte)
		if(inum.ge.0d0)then
			w=(trim(adjustl(tempR)))//'+i'//(trim(adjustl(tempI)))
		else
			w=(trim(adjustl(tempR)))//'-i'//(trim(adjustl(tempI)))
		end if
		return
	end subroutine
	subroutine charsetcom8_array(w,inte)
		character(len=*),intent(inout)::w(:)
		complex(kind=8),intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		if(size(w).lt.length)then
			write(*,*)"ERROR in charsetreal8_array"
			stop
		end if
		do i=1,length
			w(i)=inte(i)
		end do
		return
	end subroutine
	subroutine charsetcom8_array2(w,inte)
		character(len=*),intent(inout)::w
		complex(kind=8),intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		w=inte(1)+','
		do i=2,length-1
			w=inte(i)+','
		end do
		w=inte(length)
		return
	end subroutine
	subroutine charsetlogi(w,inte)
		character(len=*),intent(inout)::w
		logical,intent(in)::inte
		if(inte) then
			w='.true.'
		else
			w='.false.'
		end if
		return
	end subroutine
	subroutine charsetlogi_array(w,inte)
		character(len=*),intent(inout)::w(:)
		logical,intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		if(size(w).lt.length)then
			write(*,*)"ERROR in charsetlogi_array"
			stop
		end if
		do i=1,length
			w(i)=inte(i)
		end do
		return
	end subroutine
	subroutine charsetlogi_array2(w,inte)
		character(len=*),intent(inout)::w
		logical,intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		w=inte(1)+','
		do i=2,length-1
			w=inte(i)+','
		end do
		w=inte(length)
		return
	end subroutine
	subroutine charsetChar_array2(w,inte)
		character(len=*),intent(inout)::w
		character(len=*),intent(in)::inte(:)
		integer::i,length
		length=size(inte)
		w=inte(1)+','
		do i=2,length-1
			w=inte(i)+','
		end do
		w=inte(length)
		return
	end subroutine
	
!
!input inchar=ABCDEFG,cha=D, output the character befor D, output=ABC
!If inchar=ABCDEFG,cha=K,(there is no K in inchar), output=''
	character(len=max_len_of_char) function SubCharleft(inchar,cha)
		character(len=*),intent(in)::inchar
		character(len=1),intent(in)::cha
		integer::ith
		ith=index(inchar,cha)
		if(ith.eq.0)then
			SubCharleft=''
			return
		end if
		SubCharleft=inchar(1:ith-1)
		return
	end function
!
!input inchar=ABCDEFG,cha=D, output the character after D, output=EFG
!If inchar=ABCDEFG,cha=K,(there is no K in inchar), output=''
	character(len=max_len_of_char) function SubCharRight(inchar,cha)
		character(len=*),intent(in)::inchar
		character(len=1),intent(in)::cha
		integer::ith
		ith=index(inchar,cha)
		if(ith.eq.0)then
			SubCharRight=''
			return
		end if
		SubCharRight=inchar(ith+1:)
		return
	end function		
!
!input inchar=ABCDEFG,cha1=B,char2=F, output=CDE
!If on putput, output=''
	character(len=max_len_of_char) function SubChar(inchar,cha)
		character(len=*),intent(in)::inchar
		character(len=1),intent(in)::cha(2)
		integer::ith1,ith2
		ith1=index(inchar,cha(1))
		ith2=index(inchar,cha(2))
		if(ith1.eq.0)then
			SubChar=inchar(1:(ith2-1))
			return
		end if
		if(ith2.eq.0)then
			SubChar=inchar((ith1+1):)
			return
		end if
		if(ith1+1.gt.ith2-1)then
			call writemess('ERROR in getting sub character')
			call error_stop
		end if
		SubChar=inchar((ith1+1):(ith2-1))
		return
	end function		
	
!	character(len=(max(len(w1),max_len_of_char))) function charAddint(w1,inte)
	character(len=max_len_of_char) function charAddint(w1,inte)
		character(len=*),intent(in)::w1
		integer,intent(in)::inte
		character*100::temp
		temp=inte
		if(len(trim(w1)).eq.0)then
			charAddint=' '//(trim(adjustl(temp)))
			return
		end if
		charAddint=(trim(w1))//(trim(adjustl(temp)))
		return
	end function
!	character(len=(max(len(w1),max_len_of_char))) function intAddchar(inte,w1)
	character(len=max_len_of_char) function intAddchar(inte,w1)
		character(len=*),intent(in)::w1
		integer,intent(in)::inte
		character*100::temp
		temp=inte
		intAddchar=(trim(adjustl(temp)))//(trim(w1))
		return
	end function
	
	character(len=max_len_of_char) function charAddreal(w1,B)
		character(len=*),intent(in)::w1
		real*8,intent(in)::B
		character*100::temp
		temp=B
		if(len(trim(w1)).eq.0)then
			charAddreal=' '//(trim(adjustl(temp)))
			return
		end if
		charAddreal=(trim(w1))//(trim(adjustl(temp)))
		return
	end function
	
	character(len=max_len_of_char) function realAddchar(B,w1)
		character(len=*),intent(in)::w1
		real*8,intent(in)::B
		character*100::temp
		temp=B
		realAddchar=(trim(adjustl(temp)))//(trim(w1))
		return
	end function
	
	character(len=max_len_of_char) function real4Addchar(B,w1)
		character(len=*),intent(in)::w1
		real*4,intent(in)::B
		character*100::temp
		temp=B
		real4Addchar=(trim(adjustl(temp)))//(trim(w1))
		return
	end function
	character(len=max_len_of_char) function charAddreal4(w1,B)
		character(len=*),intent(in)::w1
		real*4,intent(in)::B
		character*100::temp
		temp=B
		if(len(trim(w1)).eq.0)then
			charAddreal4=' '//(trim(adjustl(temp)))
			return
		end if
		charAddreal4=(trim(w1))//(trim(adjustl(temp)))
		return
	end function
	character(len=max_len_of_char) function charAddcom4(w1,B)
		character(len=*),intent(in)::w1
		complex(kind=4),intent(in)::B
		character*100::temp
		temp=B
		if(len(trim(w1)).eq.0)then
			charAddcom4=' '//(trim(adjustl(temp)))
			return
		end if
		charAddcom4=(trim(w1))//(trim(adjustl(temp)))
		return
	end function
	character(len=max_len_of_char) function com4Addchar(B,w1)
		character(len=*),intent(in)::w1
		complex(kind=4),intent(in)::B
		character*100::temp
		temp=B
		com4Addchar=(trim(adjustl(temp)))//(trim(w1))		
		return
	end function
	character(len=max_len_of_char) function com8Addchar(B,w1)
		character(len=*),intent(in)::w1
		complex(kind=8),intent(in)::B
		character*100::temp
		temp=B
		com8Addchar=(trim(adjustl(temp)))//(trim(w1))
		return
	end function
	character(len=max_len_of_char) function charAddcom8(w1,B)
		character(len=*),intent(in)::w1
		complex(kind=8),intent(in)::B
		character*100::temp
		temp=B
		if(len(trim(w1)).eq.0)then
			charAddcom8=' '//(trim(adjustl(temp)))
			return
		end if
		charAddcom8=(trim(w1))//(trim(adjustl(temp)))
		return
	end function
	
	character(len=max_len_of_char) function logiAddchar(B,w1)
		character(len=*),intent(in)::w1
		logical,intent(in)::B
		if(B)then
			logiAddchar='.true.'//(trim(w1))
		else
			logiAddchar='.false.'//(trim(w1))
		end if
		return
	end function
	character(len=max_len_of_char) function charAddlogi(w1,B)
		character(len=*),intent(in)::w1
		logical,intent(in)::B
		if(B)then
			if(len(trim(w1)).eq.0)then
				charAddlogi='  .true.'
				return
			end if
			charAddlogi=(trim(w1))//'.true.'
		else
			if(len(trim(w1)).eq.0)then
				charAddlogi='  .false.'
				return
			end if
			charAddlogi=(trim(w1))//'.false.'
		end if
		return
	end function
	
	logical function equal_character(w1,w2)
		CHARACTER(len=*),intent(in)::w1
		CHARACTER(len=*),intent(in)::w2
		equal_character=trim(adjustl(w1)).eq.trim(adjustl(w2))
		return
	end function	
	logical function equal_of_array(a,b)
		integer,intent(in) :: a(:),b(:)
		integer :: la,lb,i
		la=size(a)
		lb=size(b)
		if(la.ne.lb)then
			equal_of_array=.false.
			return
		end if
		do i=1,la
			if(a(i).ne.b(i))then
				equal_of_array=.false.
				return
			end if
		end do
		equal_of_array=.true.
		return
	end function
	logical function equal_of_array_real4(a,b)result(equal_of_array)
		real*4,intent(in) :: a(:),b(:)
		integer :: la,lb,i,l
		la=size(a)
		lb=size(b)
		equal_of_array=.false.
		if(la.eq.lb) then
			l=count(abs(a-b).gt.default_zero_real_number)
			if(l.eq.0) then
				equal_of_array=.true.
			end if
		end if
		return
	end function
	logical function equal_of_array_real8(a,b)result(equal_of_array)
		real*8,intent(in) :: a(:),b(:)
		integer :: la,lb,i,l
		la=size(a)
		lb=size(b)
		equal_of_array=.false.
		if(la.eq.lb) then
			l=count(abs(a-b).gt.default_zero_double_number)
			if(l.eq.0) then
				equal_of_array=.true.
			end if
		end if
		return
	end function
	logical function equal_of_array_com4(a,b)result(equal_of_array)
		complex(kind=4),intent(in) :: a(:),b(:)
		integer :: la,lb,i,l1,l2
		la=size(a)
		lb=size(b)
		equal_of_array=.false.
		if(la.eq.lb) then
			l1=count(abs(real(a,kind=4)-real(b,kind=4)).gt.default_zero_real_number)
			l2=count(abs(aimag(a)-aimag(b)).gt.default_zero_real_number)
			if(l1.eq.0.and.l2.eq.0) then
				equal_of_array=.true.
			end if
		end if
		return
	end function
	logical function equal_of_array_com8(a,b)result(equal_of_array)
		complex(kind=8),intent(in) :: a(:),b(:)
		integer :: la,lb,i,l1,l2
		la=size(a)
		lb=size(b)
		equal_of_array=.false.
		if(la.eq.lb) then
			l1=count(abs(real(a,kind=8)-real(b,kind=8)).gt.default_zero_double_number)
			l2=count(abs(dimag(a)-dimag(b)).gt.default_zero_double_number)
			if(l1.eq.0.and.l2.eq.0) then
				equal_of_array=.true.
			end if
		end if
		return
	end function
	logical function equal_of_array_char(a,b)result(equal_of_array)
		character(len=*),intent(in) :: a(:),b(:)
		integer :: la,lb,i,l
		la=size(a)
		lb=size(b)
		if(la.ne.lb)then
			equal_of_array=.false.
			return
		end if
		do i=1,la
			if(equal_character(a(i),b(i)))then
				equal_of_array=.false.
				return
			end if
		end do
		equal_of_array=.true.
		return
	end function
	
	logical function equal_real4(a,b)result(equal)
		real*4,intent(in)::a,b
		equal=abs(a-b).lt.default_zero_real_number
		return
	end function
	
	logical function equal_real8(a,b)result(equal)
		real*8,intent(in)::a,b
		equal=abs(a-b).lt.default_zero_double_number
		return
	end function
	
	logical function equal_com4(a,b)result(equal)
		complex(kind=4),intent(in)::a,b
		complex(kind=4)::temp
		temp=abs(a-b)
		equal=real(a).lt.default_zero_real_number
		equal=equal.and. (aimag(a).lt.default_zero_real_number)
		return
	end function
	logical function equal_com8(a,b)result(equal)
		complex(kind=8),intent(in)::a,b
		complex(kind=8)::temp
		temp=abs(a-b)
		equal=dble(a).lt.default_zero_double_number
		equal=equal.and. (dimag(a).lt.default_zero_double_number)
		return
	end function
	
	
	logical function nequal_character(w1,w2)
		CHARACTER(len=*),intent(in)::w1
		CHARACTER(len=*),intent(in)::w2
		nequal_character=trim(adjustl(w1)).ne.trim(adjustl(w2))
		return
	end function
	logical function nequal_real4(a,b)result(equal)
		real*4,intent(in)::a,b
		equal=abs(a-b).ge.default_zero_real_number
		return
	end function
	
	logical function nequal_real8(a,b)result(equal)
		real*8,intent(in)::a,b
		equal=abs(a-b).ge.default_zero_double_number
		return
	end function
	
	logical function nequal_com4(a,b)result(equal)
		complex(kind=4),intent(in)::a,b
		complex(kind=4)::temp
		temp=abs(a-b)
		equal=real(a).ge.default_zero_real_number
		equal=equal.or. (aimag(a).ge.default_zero_real_number)
		return
	end function
	logical function nequal_com8(a,b)result(equal)
		complex(kind=8),intent(in)::a,b
		complex(kind=8)::temp
		temp=abs(a-b)
		equal=dble(a).ge.default_zero_double_number
		equal=equal.or. (dimag(a).ge.default_zero_double_number)
		return
	end function	
	
	subroutine openlog(not_write_cpu)
		logical,intent(in),optional::not_write_cpu
		logical :: alive
		if(present(not_write_cpu))then
			inquire(file=log_address,exist=alive)
			log_flag=.true.
			if(alive) then
				open(unit=log_address_unit,file=log_address,STATUS='old',POSITION='APPEND')
			else
				open(unit=log_address_unit,file=log_address,STATUS='REPLACE',POSITION='APPEND')
			end if
			return
		end if
		
		if(log_flag)then
			open(unit=log_address_unit,file=log_address,STATUS='old',POSITION='APPEND')
		else
			open(unit=log_address_unit,file=log_address,STATUS='REPLACE',POSITION='APPEND')
			log_flag=.true.
		end if
		return
	end subroutine
	subroutine closelog()
		close(unit=log_address_unit)
		return
	end subroutine


	subroutine open_File(uni,FileAddress,inSTATUS,inPOSITION)
		character(len=*),intent(in)::FileAddress
		character(len=*),intent(in)::inSTATUS
		integer,intent(in)::uni
		character(len=*),optional,intent(in)::inPOSITION
		logical::alive
		if(inSTATUS.equ.'old')then
			inquire(file=FileAddress,exist=alive)
			if(.not.alive)then


				call writemess(' ')
				call writemess('***** Cannot open the file of '+(' '+FileAddress)+' ******')
				call writemess(' ')
				call error_stop
			end if
		end if
		if(present(inPOSITION))then
			if(inPOSITION.equ.'end')then
				open(unit=uni,file=FileAddress,status=inSTATUS,POSITION='APPEND')
			else
				open(unit=uni,file=FileAddress,status=inSTATUS,POSITION=inPOSITION)
			end if
		else
			open(unit=uni,file=FileAddress,status=inSTATUS)
		end if
		return
	end subroutine
	logical function ifopen_File(uni,FileAddress,inSTATUS,inPOSITION)result(alive)
		character(len=*),intent(in)::FileAddress
		character(len=*),intent(in)::inSTATUS
		integer,intent(in)::uni
		character(len=*),optional,intent(in)::inPOSITION
		if(inSTATUS.equ.'old')then
			inquire(file=FileAddress,exist=alive)
			if(.not.alive)return
		end if
		alive=.true.
		if(present(inPOSITION))then
			if(inPOSITION.equ.'end')then
				open(unit=uni,file=FileAddress,status=inSTATUS,POSITION='APPEND')
			else
				open(unit=uni,file=FileAddress,status=inSTATUS,POSITION=inPOSITION)
			end if
		else
			open(unit=uni,file=FileAddress,status=inSTATUS)
		end if
		return
	end function



	subroutine writemess_char(mess,cpu_number)
		CHARACTER(len=*),intent(in)::mess
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) trim(mess)
					call closelog()
				end if
				if(output_proID.eq.output_cpu_number2)then
					if(rewriteFlag1)write(*,*)''
					write(*,*)trim( mess)
				end if
			end if
			if((cpu_number.lt.0).and.(output_proID.ne.output_cpu_number))then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) trim(mess+'   |   CPU'+output_proID)
					call closelog()
				end if
				if(output_proID.eq.output_cpu_number2)then
					if(rewriteFlag1)write(*,*)''
					write(*,*)trim(mess+'   |   CPU'+output_proID)
				end if
			else
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) trim(mess)
					call closelog()
				end if
				if(output_proID.eq.output_cpu_number2)then
					if(rewriteFlag1)write(*,*)''
					write(*,*)trim( mess)
				end if
			end if
			
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) trim(mess)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				if(rewriteFlag1)write(*,*)''
				write(*,*)trim( mess)
			end if
		end if
		rewriteFlag1=.false.
		rewriteFlag2=.false.
		return
	end subroutine
	subroutine writemess_char_form(mess,form_,cpu_number)
		CHARACTER(len=*),intent(in)::mess
		character(len=*),intent(in)::form_
		integer,optional,intent(in)::cpu_number
		character(len=max_len_of_char)::form,mess2
		logical::Flag,Flag2
		integer,save::lastlength=-1
		Flag=.true.
		Flag2=.false.
		if(form_.equ.'-')then
			form='(1a1,a,$)'
			if(lastlength.eq.-1)then
				lastlength=len_trim(mess)
			else
				lastlength=max(len_trim(mess),lastlength)
			end if
			Flag2=.true.
		else if(form_.equ.'+')then
			form='(1a1,a)'
			if(lastlength.eq.-1)then
				lastlength=len_trim(mess)
			else
				lastlength=max(len_trim(mess),lastlength)
			end if
		else if(form_.equ.'*')then
			form='(1a1,a,$)'
			lastlength=len_trim(mess)
			Flag2=.true.
		else if(form_.equ.'/')then
			form='(1a1,a)'
			lastlength=len_trim(mess)
		else
			form=form_
			Flag=.false.
		end if
		
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					if(Flag)then
						mess2=mess
						if(rewriteFlag2)BACKSPACE(UNIT=log_address_unit) 
						write(log_address_unit,*) mess2(1:lastlength)
					else
						write(log_address_unit,form) trim(mess)
					end if
					call closelog()
				end if
				if(output_proID.eq.output_cpu_number2)then
					if(Flag)then
						mess2=mess
						write(*,form)char(13),mess2(1:lastlength)
					else
						write(*,form)trim( mess)
					end if
				end if
			end if
			if((cpu_number.lt.0).and.(output_proID.ne.output_cpu_number))then
				if(out_log_flag)then
					call openlog(.true.)
					if(Flag)then
						mess2=trim(mess+'   |   CPU'+output_proID)
						lastlength=max(len_trim(mess2),lastlength)
						if(rewriteFlag2)BACKSPACE(UNIT=log_address_unit) 
						write(log_address_unit,*) mess2(1:lastlength)
					else
						write(log_address_unit,form)trim( mess)
					end if
					call closelog()
				end if
				if(output_proID.eq.output_cpu_number2)then
					if(Flag)then
						mess2=trim(mess+'   |   CPU'+output_proID)
						lastlength=max(len_trim(mess2),lastlength)
						write(*,form)char(13), mess2(1:lastlength)
					else
						write(*,form)trim(mess+'   |   CPU'+output_proID)
					end if
				end if
			else
				if(out_log_flag)then
					call openlog()
					if(Flag)then
						mess2=mess
						if(rewriteFlag2)BACKSPACE(UNIT=log_address_unit) 
						write(log_address_unit,*)mess2(1:lastlength)
					else
						write(log_address_unit,form) trim(mess)
					end if
					call closelog()
				end if
				if(output_proID.eq.output_cpu_number2)then
					if(Flag)then
						mess2=mess
						write(*,form)char(13),mess2(1:lastlength)
					else
						write(*,form)trim( mess)
					end if
				end if
			end if
			
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					if(Flag)then
						mess2=mess
						if(rewriteFlag2)BACKSPACE(UNIT=log_address_unit) 
						write(log_address_unit,*)mess2(1:lastlength)
					else
						write(log_address_unit,form) trim(mess)
					end if
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				if(Flag)then
					mess2=mess
					write(*,form)char(13),mess2(1:lastlength)
				else
					write(*,form)trim( mess)
				end if
			end if
		end if
		if(Flag)then
			if(Flag2)then
				rewriteFlag1=.true.
				rewriteFlag2=.true.
			else
				rewriteFlag1=.false.
			end if
		end if
		return
	end subroutine

	subroutine writemess_char2(noadjustl,mess,cpu_number)
		CHARACTER(len=*),intent(in)::mess
		integer,optional,intent(in)::cpu_number
		logical,intent(in)::noadjustl
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		end if
		return
	end subroutine
	
	subroutine writemess_real(mess,cpu_number)
		real*8,intent(in)::mess
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_real_array(mess,cpu_number)
		real*8,intent(in)::mess(:)
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_real_form(mess,form,cpu_number)
		real*8,intent(in)::mess
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,form)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,form) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_real_array_form(mess,form,cpu_number)
		real*8,intent(in)::mess(:)
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,form)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,form) mess
			end if
		end if
		return
	end subroutine




	subroutine writemess_real4(mess,cpu_number)
		real*4,intent(in)::mess
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_real4_array(mess,cpu_number)
		real*4,intent(in)::mess(:)
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_real4_form(mess,form,cpu_number)
		real*4,intent(in)::mess
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,form)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,form) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_real4_array_form(mess,form,cpu_number)
		real*4,intent(in)::mess(:)
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,form)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,form) mess
			end if
		end if
		return
	end subroutine



	subroutine writemess_int(mess,cpu_number)
		integer,intent(in)::mess
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_int_array(mess,cpu_number)
		integer,intent(in)::mess(:)
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_int_form(mess,form,cpu_number)
		integer,intent(in)::mess
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,form)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,form) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_int_array_form(mess,form,cpu_number)
		integer,intent(in)::mess(:)
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,form)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,form) mess
			end if
		end if
		return
	end subroutine



	subroutine writemess_com4(mess,cpu_number)
		complex*8,intent(in)::mess
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess+trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_com4_array(mess,cpu_number)
		complex*8,intent(in)::mess(:)
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_com4_form(mess,form,cpu_number)
		complex*8,intent(in)::mess
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess+trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,form)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,form) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_com4_array_form(mess,form,cpu_number)
		complex*8,intent(in)::mess(:)
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,form)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,form) mess
			end if
		end if
		return
	end subroutine

	subroutine writemess_com8(mess,cpu_number)
		complex*16,intent(in)::mess
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_com8_array(mess,cpu_number)
		complex*16,intent(in)::mess(:)
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_com8_form(mess,form,cpu_number)
		complex*16,intent(in)::mess
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,form)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,form) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_com8_array_form(mess,form,cpu_number)
		complex*16,intent(in)::mess(:)
		character(len=*),intent(in)::form
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,form)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,form) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,form) mess
			end if
		end if
		return
	end subroutine


	subroutine writemess_logi(mess,cpu_number)
		logical,intent(in)::mess
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('   |   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine
	subroutine writemess_logi_array(mess,cpu_number)
		logical,intent(in)::mess(:)
		integer,optional,intent(in)::cpu_number
		if(present(cpu_number))then
			if(output_proID.eq.cpu_number)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(cpu_number.lt.0)then
				if(out_log_flag)then
					call openlog(.true.)
					write(log_address_unit,*) mess,trim('|   CPU'+output_proID)
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number2)then
				write(*,*)mess
			end if
		else
			if(output_proID.eq.output_cpu_number)then
				if(out_log_flag)then
					call openlog()
					write(log_address_unit,*) mess
					call closelog()
				end if
			end if
			if(output_proID.eq.output_cpu_number)then
				write(*,*) mess
			end if
		end if
		return
	end subroutine

	
	subroutine outputmessTime(cputime)
		real*8,intent(in)::cputime
		integer::times,timem,timeh,timed,temp
		CHARACTER(10) :: cput
		Character(8) :: cpud 
		CHARACTER(5) :: cpuz  
		CHARACTER*50::w1,w2,w3
		if(output_ProID.eq.output_cpu_number) then
			if(cputime.lt.60) then
				times=cputime
				timem=0
				timeh=0
				timed=0
			else if( (cputime.ge.60).and.(cputime.lt.3600)) then
				timem=cputime/60
				times=cputime-timem*60
				timeh=0
				timed=0
			else if((cputime.ge.3600).and.(cputime.lt.86400)) then
				timeh=cputime/3600
				temp=cputime-timeh*3600
				timem=temp/60
				times=temp-timem*60
				timed=0
			else
				timed=cputime/86400
				temp=cputime-timed*86400
				timeh=temp/3600
				temp=temp-timeh*3600
				timem=temp/60
				times=temp-timem*60
			end if
			CALL DATE_AND_TIME(DATE=cpud,TIME=cput,ZONE=cpuz) 
			call writemess("now the time is :")
			w1=cpud
			w2=cput
			w3=trim(adjustl(w1))//" "//trim(adjustl(w2))
			call writemess(trim(adjustl(w3)))
			call system("date '+%D%n%c' ")
			
			call writemess("The time it cost up to now is")
			w1=timed
			w3=" "//trim(adjustl(w1))//"day,"
			w1=timeh
			w3=trim(adjustl(w3))//trim(adjustl(w1))//"hour,"
			w1=timem
			w3=trim(adjustl(w3))//trim(adjustl(w1))//"minute,"
			w1=times
			w3=trim(adjustl(w3))//trim(adjustl(w1))//"second."
			call writemess(trim(adjustl(w3)))
		end if
		return
	end subroutine	
	subroutine system_time1(cputime,times,timem,timeh,timed,chartime)
		real*8,intent(in)::cputime
		integer,intent(inout)::times,timem,timeh,timed
		character(len=*),intent(inout),optional::chartime
		integer::temp
		if(cputime.lt.60) then
			times=cputime
			timem=0
			timeh=0
			timed=0
			if(present(chartime))then
				chartime=times+'s'
			end if
		else if( (cputime.ge.60).and.(cputime.lt.3600)) then
			timem=cputime/60
			times=cputime-timem*60
			timeh=0
			timed=0
			if(present(chartime))then
				chartime=timem+'min,'+times+'s'
			end if
		else if((cputime.ge.3600).and.(cputime.lt.86400)) then
			timeh=cputime/3600
			temp=cputime-timeh*3600
			timem=temp/60
			times=temp-timem*60
			timed=0
			if(present(chartime))then
				chartime=timeh+'hour,'+timem+'min,'+times+'s'
			end if
		else
			timed=cputime/86400
			temp=cputime-timed*86400
			timeh=temp/3600
			temp=temp-timeh*3600
			timem=temp/60
			times=temp-timem*60
			if(present(chartime))then
				chartime=timed+'days,'+timeh+'hour,'+timem+'min,'+times+'s'
			end if
		end if
		return
	end subroutine
	subroutine system_time2(cputime,chartime)
		real*8,intent(in)::cputime
		character(len=*),intent(inout)::chartime
		integer::times,timem,timeh,timed
		integer::temp
		if(cputime.lt.60) then
			times=cputime
			timem=0
			timeh=0
			timed=0
			chartime=times+'s'
		else if( (cputime.ge.60).and.(cputime.lt.3600)) then
			timem=cputime/60
			times=cputime-timem*60
			timeh=0
			timed=0
			chartime=timem+'min,'+times+'s'
		else if((cputime.ge.3600).and.(cputime.lt.86400)) then
			timeh=cputime/3600
			temp=cputime-timeh*3600
			timem=temp/60
			times=temp-timem*60
			timed=0
			chartime=timeh+'hour,'+timem+'min,'+times+'s'
		else
			timed=cputime/86400
			temp=cputime-timed*86400
			timeh=temp/3600
			temp=temp-timeh*3600
			timem=temp/60
			times=temp-timem*60
			chartime=timed+'days,'+timeh+'hour,'+timem+'min,'+times+'s'
		end if
		return
	end subroutine
	
	subroutine reset_Time_calculator(totalstep,numOutput)
		integer,intent(in)::totalstep
		integer,intent(in),optional::numOutput
		Time_calculater_start_flag=.true.
		Time_calculater_TotalStep=totalstep
		if(present(numOutput))then
			Time_calculater_numOutput=numOutput
		else
			Time_calculater_numOutput=15
		end if
		call writemess('          --- reset Time calculator ---')
		return
	end subroutine
		
	
	subroutine Time_calculator(delta_step_)
		integer,intent(in),optional::delta_step_
		real*8,save::time1,time2
		integer,save::modi,stepi
		logical,save::first_write=.true.
		character*100::timechar,timechar2,systemtime
		real*8::persetpTime
		integer::remainStep,per
		integer::values(8)
		character*60::w
		integer::i,delta_step
		if(present(delta_step_))then
			delta_step=delta_step_
		else
			delta_step=1
		end if
		if(Time_calculater_start_flag)then
			!time1= omp_get_wtime()
			call cpu_time(time1)
			Time_calculater_start_flag=.false.
			first_write=.true.
			if(Time_calculater_numOutput.gt.Time_calculater_TotalStep)then
				modi=2
			else
				modi=Time_calculater_TotalStep/Time_calculater_numOutput
			end if
			call date_and_time(VALUES=values)
			systemtime=values(1)+'-'+values(2)+'-'+values(3)+'-'+values(5)+':'+values(6)+':'+values(7)
			call writemess('      ##########'+(' '+systemtime)+' ##########')
			stepi=max(0+delta_step,1)
			return
		end if
		stepi=max(stepi+delta_step,1)
		if(first_write)then
			if(stepi.eq.max(Time_calculater_TotalStep/800,20))then
				first_write=.false.
				!time2= omp_get_wtime()
				call cpu_time(time2)
				call system_time(time2-time1,timechar)
				per=dble(stepi)/dble(Time_calculater_TotalStep)*100.
				w=persentChar(per)
				persetpTime=(time2-time1)/stepi
				remainStep=Time_calculater_TotalStep-stepi
				call system_time(persetpTime*remainStep,timechar2)
				call date_and_time(VALUES=values)
				systemtime=values(1)+'-'+values(2)+'-'+values(3)+'-'+values(5)+':'+values(6)+':'+values(7)
				call writemess(w+'.'+systemtime+'. Using:'+timechar+'. Remain:'+timechar2,'-')
				return
			end if
		end if
		if(mod(stepi,modi).eq.0)then
			!time2= omp_get_wtime()
			call cpu_time(time2)
			call system_time(time2-time1,timechar)
			per=dble(stepi)/dble(Time_calculater_TotalStep)*100.
			w=persentChar(per)
			persetpTime=(time2-time1)/stepi
			remainStep=Time_calculater_TotalStep-stepi
			call system_time(persetpTime*remainStep,timechar2)
			call date_and_time(VALUES=values)
			systemtime=values(1)+'-'+values(2)+'-'+values(3)+'-'+values(5)+':'+values(6)+':'+values(7)
			call writemess(w+'.'+systemtime+'. Using:'+timechar+'. Remain:'+timechar2,'-')
		end if
		if(stepi.eq.Time_calculater_TotalStep)then
			!time2= omp_get_wtime()
			call cpu_time(time2)
			w=persentChar(100)
			call system_time(time2-time1,timechar)
			call writemess(w+'.Using time:'+timechar,'+')
			call date_and_time(VALUES=values)
			systemtime=values(1)+'-'+values(2)+'-'+values(3)+'-'+values(5)+':'+values(6)+':'+values(7)
			call writemess('      =========='+(' '+systemtime)+' ==========')
			call writemess(' ')
		end if
		return
	end subroutine
	
	subroutine reset_Time_calculator_limit(totalstep,limit_time,numOutput)
		integer,intent(in)::totalstep,limit_time
		integer,intent(in),optional::numOutput
		Time_calculater_start_flag=.true.
		Time_calculater_TotalStep=totalstep
		if(present(numOutput))then
			Time_calculater_numOutput=numOutput
		else
			Time_calculater_numOutput=15
		end if
		Time_calculater_limit_time=limit_time
		call writemess('          ------- reset Time calculator ------')
		call writemess('         limit_time='+limit_time+'s')
		return
	end subroutine
	
	logical function time_calculator_limit(delta_step_)Result(notstopFlag)
		integer,intent(in),optional::delta_step_
		real*8,save::time1,time2
		integer,save::modi,stepi
		logical,save::first_write=.true.
		character*100::timechar,timechar2,systemtime
		real*8::persetpTime
		integer::remainStep,per
		integer::values(8)
		character*60::w
		integer::i,delta_step
		notstopFlag=.true.
		if(present(delta_step_))then
			delta_step=delta_step_
		else
			delta_step=1
		end if
		if(Time_calculater_start_flag)then
			!time1= omp_get_wtime()
			call cpu_time(time1)
			Time_calculater_start_flag=.false.
			first_write=.true.
			if(Time_calculater_numOutput.gt.Time_calculater_TotalStep)then
				modi=2
			else
				modi=Time_calculater_TotalStep/Time_calculater_numOutput
			end if
			call date_and_time(VALUES=values)
			systemtime=values(1)+'-'+values(2)+'-'+values(3)+'-'+values(5)+':'+values(6)+':'+values(7)
			call writemess('      ##########'+(' '+systemtime)+' ##########')
			stepi=max(0+delta_step,1)
			return
		end if
		stepi=max(stepi+delta_step,1)
		if(first_write)then
			if(stepi.eq.max(Time_calculater_TotalStep/800,20))then
				first_write=.false.
				!time2= omp_get_wtime()
				call cpu_time(time2)
				call system_time(time2-time1,timechar)
				per=dble(stepi)/dble(Time_calculater_TotalStep)*100.
				w=persentChar(per)
				persetpTime=(time2-time1)/stepi
				remainStep=Time_calculater_TotalStep-stepi
				call system_time(persetpTime*remainStep,timechar2)
				call date_and_time(VALUES=values)
				systemtime=values(1)+'-'+values(2)+'-'+values(3)+'-'+values(5)+':'+values(6)+':'+values(7)
				call writemess(w+'.'+systemtime+'. Using:'+timechar+'. Remain:'+timechar2,'-')
				return
			end if
		end if
		if(mod(stepi,modi).eq.0)then
			!time2= omp_get_wtime()
			call cpu_time(time2)
			call system_time(time2-time1,timechar)
			per=dble(stepi)/dble(Time_calculater_TotalStep)*100.
			w=persentChar(per)
			persetpTime=(time2-time1)/stepi
			remainStep=Time_calculater_TotalStep-stepi
			call system_time(persetpTime*remainStep,timechar2)
			call date_and_time(VALUES=values)
			systemtime=values(1)+'-'+values(2)+'-'+values(3)+'-'+values(5)+':'+values(6)+':'+values(7)
			call writemess(w+'.'+systemtime+'. Using:'+timechar+'. Remain:'+timechar2,'-')
		end if
		if(stepi.eq.Time_calculater_TotalStep)then
			!time2= omp_get_wtime()
			call cpu_time(time2)
			w=persentChar(100)
			call system_time(time2-time1,timechar)
			call writemess(w+'.Using time:'+timechar,'+')
			call date_and_time(VALUES=values)
			systemtime=values(1)+'-'+values(2)+'-'+values(3)+'-'+values(5)+':'+values(6)+':'+values(7)
			call writemess('      =========='+(' '+systemtime)+' ==========')
			call writemess(' ')
		end if
		notstopFlag=(time2-time1).lt.Time_calculater_limit_time
		return
	end function
		
	
	subroutine System_time_calculater1(stepi,totalstep,numOutput,firstnum)
		integer,intent(in)::stepi,totalstep,numOutput
		integer,intent(in),optional::firstnum
		real*8,save::time1,time2
		integer,save::modi
		logical,save::first=.true.,first_write=.true.
		character*100::timechar,timechar2
		real*8::persetpTime
		integer::remainStep,per
		character*60::w
		integer::i
		if((stepi.eq.1).or.first)then
			!time1= omp_get_wtime()
			call cpu_time(time1)
			first=.false.
			if(numOutput.gt.totalstep)then
				modi=2
			else
				modi=totalstep/numOutput
			end if
			call writemess('=====================================')
			call writemess('output the running time for the loop:')
		end if
		if(first_write)then
			if(present(firstnum))then
				if(stepi.eq.firstnum)then
					first_write=.false.
					!time2= omp_get_wtime()
					call cpu_time(time2)
					call system_time(time2-time1,timechar)
					per=dble(stepi)/dble(totalstep)*100.
					w=persentChar(per)
					persetpTime=(time2-time1)/stepi
					remainStep=totalstep-stepi
					call system_time(persetpTime*remainStep,timechar2)
					call writemess(w+'Using time:'+timechar+'. Remaining time:'+timechar2,'-')
					return
				end if
			end if
			if(stepi.eq.max(totalstep/1000,20))then
				first_write=.false.
				!time2= omp_get_wtime()
				call cpu_time(time2)
				call system_time(time2-time1,timechar)
				per=dble(stepi)/dble(totalstep)*100.
				w=persentChar(per)
				persetpTime=(time2-time1)/stepi
				remainStep=totalstep-stepi
				call system_time(persetpTime*remainStep,timechar2)
				call writemess(w+'.Using time:'+timechar+'. Remaining time:'+timechar2,'-')
				return
			end if
		end if
		if(mod(stepi,modi).eq.0)then
			!time2= omp_get_wtime()
			call cpu_time(time2)
			call system_time(time2-time1,timechar)
			per=dble(stepi)/dble(totalstep)*100.
			w=persentChar(per)
			persetpTime=(time2-time1)/stepi
			remainStep=totalstep-stepi
			call system_time(persetpTime*remainStep,timechar2)
			call writemess(w+'.Using time:'+timechar+'. Remaining time:'+timechar2,'-')
		end if
		if(stepi.eq.totalstep)then
			!time2= omp_get_wtime()
			call cpu_time(time2)
			w=persentChar(100)
			call system_time(time2-time1,timechar)
			call writemess(w+'.Using time:'+timechar,'+')
			call writemess('=====================================')
			first=.true.
			first_write=.true.
		end if
		return
	end subroutine
	character(len=60) function persentChar(per_)
		integer,intent(in)::per_
		integer::i,lenchar,per
		character*3::perchar
		persentChar='['
		perchar=per_+'%'
		lenchar=len_trim(perchar)
		i=1
		per=per_*persentCharLen/100
		do while(i.le.per) 
			if(i.ne.persentCharLen2)then
				persentChar=persentChar+'*'
				i=i+1
			else
				persentChar=persentChar+perchar
				i=i+lenchar
			end if
		end do
		do while(i.le.persentCharLen) 
			if(i.ne.persentCharLen2)then
				persentChar=persentChar+'-'
				i=i+1
			else
				persentChar=persentChar+perchar
				i=i+lenchar
			end if
		end do
		persentChar=persentChar+']'
		return
	end function
		
	
	subroutine System_time_calculater2(stepi,totalstep,runningtime,remaintime,time1_)
		integer,intent(in)::stepi,totalstep
		character(len=*)::runningtime,remaintime
		real*8,intent(in),optional::time1_
		real*8,save::time1,time2
		integer,save::modi,first_stepi
		real*8::persetpTime
		integer::remainStep
		if(present(time1_))then
			time1=time1_
			first_stepi=stepi
			return
		end if
		call cpu_time(time2)
		!time2= omp_get_wtime()
		call system_time(time2-time1,runningtime)
		persetpTime=(time2-time1)/(stepi-first_stepi+1)
		remainStep=totalstep-stepi
		call system_time(persetpTime*remainStep,remaintime)
		return
	end subroutine
	
	
	subroutine outpicture()
		integer::ty
		ty=randomnumber(1,10)
		select case(ty)
			case (1)
				call outpicture1()
			case (2)
				call outpicture2()
			case (3)
				call outpicture3()
			case (4)
				call outpicture4()
			case (5)
				call outpicture5()
			case (6)
				call outpicture6()
			case (7)
				call outpicture7()
			case (8)
				call outpicture8()
			case (9)
				call outpicture9()
			case (10)
				call outpicture10()
		end select
		return
	end subroutine
	subroutine outpicture1()
		call writemess(.true.,'    ')
		call writemess(.true.,'                   _ooOoo_')
		call writemess(.true.,' _____________    o8888888o')
		call writemess(.true.,'|you have bugs|   88" . "88')
		call writemess(.true.,'|------------__\  (| -_- |)')
		call writemess(.true.,'                  O\  =  /O')
		call writemess(.true.,'               ____/`---`\\____')
		call writemess(.true.,'             .`  \\|     |//  `.')
		call writemess(.true.,'            /  \\|||  :  |||//  \')
		call writemess(.true.,'           /  _||||| -:- |||||-  \')
		call writemess(.true.,'           |   | \\\  -  /// |   |')
		call writemess(.true.,'           | \_|  ``\---/``  |   |')
		call writemess(.true.,'           \  .-\__  `-`  ___/-. /')
		call writemess(.true.,'         ___`. .`  /--.--\  `. . __')
		call writemess(.true.,'      ."" `<  `.___\_<|>_/___.`  >`"".')
		call writemess(.true.,'     | | :  `- \`.;`\ _ /`;.`/ - ` : | |')
		call writemess(.true.,'     \  \ `-.   \_ __\ /__ _/   .-` /  /')
		call writemess(.true.,'======`-.____`-.___\_____/___.-`____.-`======')
		call writemess(.true.,'                   `=---=`                    ')
		call writemess(.true.,'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^')
		call writemess(.true.,' Report any bugs of the TNSP to sj.dong@outlook.com     	     ' )
		call writemess(.true.,'    ')
	end subroutine 
	subroutine outpicture2()
	   call writemess(.true.,'    ')
	   call writemess(.true.,'                      \  /  ')
		call writemess(.true.,'                    ___\/__')
		call writemess(.true.,'                   /  ^   ^ \ ')
		call writemess(.true.,'                 /   (@) (@) \ ')
		call writemess(.true.,'                 / |     ,    \      _____________________')
		call writemess(.true.,'                |  U   /~~~\   |    ||                    |')
		call writemess(.true.,'                 \     `~~~    )    ||                    |')
		call writemess(.true.,'                _ /           /     ||                    |')
		call writemess(.true.,'               (  \   (```)   \     ||     ERROR          |  ')
		call writemess(.true.,'               `  `,  /  -`    \    ||                    |')
		call writemess(.true.,'============ /|=\   "   /  __|  |===||                    |==')
		call writemess(.true.,'            / |   \___/  ______/    ||____________________|')
		call writemess(.true.,' ________.,`   |                         __||_______|__')
		call writemess(.true.,'|you have bugs! |_________________________________________')
		call writemess(.true.,'|Report any bugs of the TNSP to sj.dong@outlook.com       |	     ' )
		call writemess(.true.,'`~-------------------------------------------------------~` ')
	end subroutine 
	subroutine outpicture3()
	   call writemess(.true.,'    ')
		call writemess(.true.,'        ___                   \  /')
		call writemess(.true.,'       /     \             ____\/____')
		call writemess(.true.,'      /        \          /           \')
		call writemess(.true.,'     |::     |   |      /  ::\::::/::: \')
		call writemess(.true.,'     |;;;;   U   |     /  < 0 >::< 0 > \')
		call writemess(.true.,'      |::;;;    |      |      (/\)     )   ')
		call writemess(.true.,'       |;;;;   ]        \            _/   ')
		call writemess(.true.,'        \:::: )        |  |            \ ')
		call writemess(.true.,'        /     \        | |     ME    | | . ')
		call writemess(.true.,'_______/        \_____,|,|,_________|_/__|\_______________')
		call writemess(.true.,'      |   YOU    \                       | \____________')
		call writemess(.true.,'_________________________________________( you have bugs|')
		call writemess(.true.,'|Report any bugs of the TNSP to sj.dong@outlook.com     |	     ' )
		call writemess(.true.,'`~-----------------------------------------------------~` ')
	end subroutine 
	subroutine outpicture4()
	   call writemess(.true.,'    ')
		call writemess(.true.,'______     ')
		call writemess(.true.,'___|__|         _________________________________  ')
		call writemess(.true.,'_|____|         |you have bugs!!                 |  ')
		call writemess(.true.,'___|__|--^\   __|Report any bugs of the TNSP to  |')
		call writemess(.true.,'_|____|w ` ) /_ | sj.dong@outlook.com !          |')
		call writemess(.true.,'___|__|  C=]    |________________________________|')
		call writemess(.true.,'_|____|=========')
		call writemess(.true.,'___|____|____|__|')
		call writemess(.true.,'_|____|____|____|')
		call writemess(.true.,'___|____|____|__|')
	end subroutine 
	subroutine outpicture5()
	   call writemess(.true.,'    ')
		call writemess(.true.,'     _     _')
		call writemess(.true.,'  __| |___| |__        _________________________________')
		call writemess(.true.,' |      _      |       |you have bugs!!                 |  ')
		call writemess(.true.,' |  __/   \__  |     __|Report any bugs of the TNSP to  |  ')
		call writemess(.true.,' |   0     0  ||   /    sj.dong@outlook.com !           | ')
		call writemess(.true.,' |            U|  /_____________________________________| ')
		call writemess(.true.,' |     _|_     |')
		call writemess(.true.,' |__        ___|')
		call writemess(.true.,'     |     |')
		call writemess(.true.,'     |     |____________')
		call writemess(.true.,'     |                 |__')
		call writemess(.true.,'     |                 |__}')
		call writemess(.true.,'     |_________________|' )
		call writemess(.true.,'       | | |       | | |')
		call writemess(.true.,'       | | |       | | |')
	end subroutine 
	subroutine outpicture6()
	   call writemess(.true.,'       /^\/^\ ')
		call writemess(.true.,'       ( -3-)')
		call writemess(.true.,' =====O=======O=====================')
		call writemess(.true.,' |you have bugs!!                  ||')
		call writemess(.true.,' |Report ONLY bugs of the package  ||')
		call writemess(.true.,' |to sj.dong@outlook.com !         ||')
		call writemess(.true.,' ===================================')
		call writemess(.true.,'    |  ||                  |  ||')
		call writemess(.true.,'    |__||                  |__|| ')
	end subroutine 
	subroutine outpicture7()
	   call writemess(.true.,'    ')
		call writemess(.true.,' _________________________________________________________')
		call writemess(.true.,' |you have bugs!!                                         |')
		call writemess(.true.,' |Report any bugs of the TNSP to sj.dong@outlook.com     !|')
		call writemess(.true.,' |  ------------------------------------------------------|')
		call writemess(.true.,' | /')
		call writemess(.true.,' |/__________')
		call writemess(.true.,'   /          \        ')
		call writemess(.true.,'  /        X X')
		call writemess(.true.,' | Y Y          \')
		call writemess(.true.,' | | |        oo |')
		call writemess(.true.,' | \_/       _/\_)')
		call writemess(.true.,' |          ___/ ')
		call writemess(.true.,'  \       / ' )
		call writemess(.true.,'   |  |  |')
		call writemess(.true.,'  (___)__)')
	end subroutine 
	subroutine outpicture8()
	   call writemess(.true.,'    ')
		call writemess(.true.,' ')
		call writemess(.true.,'      __ ')
		call writemess(.true.,'     #####                    ### ')
		call writemess(.true.,'    #########___---------____##### ')
		call writemess(.true.,'    ########                 ##### ')
		call writemess(.true.,'     ######                   ### ')
		call writemess(.true.,'      ####                     ## ')
		call writemess(.true.,'       ##                       \   ')
		call writemess(.true.,'       /        ##       ##      |  ')
		call writemess(.true.,'       |       #0#       #0#      | ')
		call writemess(.true.,'       |      ###         ###     | ')
		call writemess(.true.,'       |      #            ##     | ')
		call writemess(.true.,'       |             ####         | ')
		call writemess(.true.,'      /\         \    ##   /     / ')
		call writemess(.true.,'     /  \         \_______/     / ')
		call writemess(.true.,'    ###  \              \\     ## ')
		call writemess(.true.,'   ######################\\######## ')
		call writemess(.true.,' ############/|###########\\######## ')
		call writemess(.true.,'            / |            \\ ')
		call writemess(.true.,' ________.,`   |  ')
		call writemess(.true.,'|you have bugs! |_________________________________________')
		call writemess(.true.,'|Report any bugs of the TNSP to sj.dong@outlook.com       | ' )
		call writemess(.true.,'`~-------------------------------------------------------~` ')
	end subroutine 
	subroutine outpicture9()
	   call writemess(' ')
		call writemess('                      ________ ')
		call writemess('                    _/__|__|__\_ ')
		call writemess('                   /     _      \ ')
		call writemess('                  /_   _(_)_   __\ ')
		call writemess('                 ||_| |____o| |_|_| ')
		call writemess('             ____|================|___ ')
		call writemess('            |    |   __________   |   \ ')
		call writemess('            |    |  | WARNING! |  |    \ ')
		call writemess('            |   ||  | you      |  |\    \ ')
		call writemess('            |   ||  |  have    |  | \    \ ')
		call writemess('            |   ||  |    bugs  |  |  \    \ ')
		call writemess('            |   ||  |__________|  |   \    \ ')
		call writemess('            |   ||                |    \____\ ')
		call writemess('            |   ||________________|      | \ ')
		call writemess('            |___|      \     /           |  \ ')
		call writemess('             / \        \___/            |___\ ')
		call writemess('            /   \       /   \ ')
		call writemess('           /_____\     /_____\ ')
		call writemess('_____________________________________________________________ ')
		call writemess('|Report any bugs of the TNSP to sj.dong@outlook.com          |')
		call writemess('`~----------------------------------------------------------~`')
	end subroutine 
	subroutine outpicture10()
	   call writemess(' ')
		call writemess('                    |_| ')
		call writemess('                  _P   P_ ')
		call writemess('           \___  /  \|/  \  ___/ ')
		call writemess('               \/    |    \/ ')
		call writemess('                |  0 |   0| ')
		call writemess('             __/| 000|  00|\__ ')
		call writemess('            /   \  00|    /   \ ')
		call writemess('              __/\___|___/\__ ')
		call writemess('             /               \ ')
		call writemess(' _______________ ')
		call writemess('|you have bugs! |____________________________________________ ')
		call writemess('|Report any bugs of the TNSP to sj.dong@outlook.com          |')
		call writemess('`~----------------------------------------------------------~`')
	end subroutine 


!  Purpose: Generate seed for each process
!
!====================================================	
	subroutine seed_gen(myseed)
		implicit none
		integer::seed0,counter,myseed,i
		call system_clock(count=counter)
		seed0=-counter
		myseed= int(-5970*rand(seed0)) 
		initial_randomseed=myseed
		if(MPI_running)then
			seed0=myseed
			do i=1,output_proID+1
				myseed=int(-5970*rand(seed0)) 
			end do
			initial_mpi_randomseed_in_cpus=myseed
		end if
	end subroutine 
!=========================================================================
!  Purpose:   
!  "Minimal" random number generator of Park and Miller with
!  Bays-Durham shuffle and added safeguards. Returns a uniform
!  random deviate between 0.0 and 1.0 (exclusive of the end points).
!  Call with idum a negative integer to initialize; thereafter do
!  not alter idum between successive deviates in a sequence. RNMX
!  should approximate the largest floating point value that is 
!  less than 1.
!
!  Input: idum, the seed
!  Output:  random number between 0.0 and 1.0
!============================================================================
	real*8 function rand(idum)
		implicit none
		  
		integer::idum     
		
		integer ia, im, iq, ir, ntab, ndiv
		real am, eps, rnmx
		
		parameter (ia=16807, im=2147483647, am=1./im)
		parameter (iq=127773, ir=2836, ntab=32, ndiv=1+(im-1)/ntab)
		parameter (eps=1.2e-7, rnmx=1.-eps)  

		integer j, k, iv(ntab), iy
		save iv, iy

		data iv /ntab*0/, iy /0/

		if (idum.le.0 .or. iy.eq.0) then
		   idum = max(-idum,1)
		   do j = ntab+8, 1, -1
		      k = idum/iq
		      idum = ia*(idum-k*iq)-ir*k
		      if (idum .lt. 0) idum = idum + im
		      if (j .le. ntab) iv(j) = idum
		   enddo
		   iy = iv(1)
		endif

		k = idum/iq
		idum = ia*(idum-k*iq) - ir*k
		if (idum.lt.0) idum = idum + im
		j = 1 + iy/ndiv
		iy = iv(j)
		iv(j) = idum
		rand = min(am*iy,rnmx)
		return
	end function
	
	real*8 function randomnumber1()
		if(seed_flag) then
			randomnumber1=rand(randomseed)
		else
			call seed_gen(randomseed)
			seed_flag=.true.
			randomnumber1=rand(randomseed)
		end if
		return
	end function
	
	real*8 function randomnumber2(minr,maxr)
		real*8,intent(in)::minr,maxr
		randomnumber2=(maxr-minr)*randomnumber1()+minr
		return
	end function
	real*4 function randomnumber3(minr,maxr)
		real*4,intent(in)::minr,maxr
		randomnumber3=(maxr-minr)*randomnumber1()+minr
		return
	end function
	integer function randomnumber4(minr,maxr)
		integer,intent(in)::minr,maxr
		randomnumber4=(maxr-minr+1)*randomnumber1()+minr
		return
	end function
	
	subroutine set_seed1(idum)
		integer,intent(in)::idum
		integer::seed0,i
		if(idum.eq.0)then
			seed_flag=.false.
			return
		end if
		randomseed=idum
		initial_randomseed=randomseed
		if(MPI_running)then
			seed0=randomseed
			do i=1,output_proID+1
				randomseed=int(-5970*rand(seed0)) 
			end do
			initial_mpi_randomseed_in_cpus=randomseed
		end if
		seed_flag=.true.
		return
	end subroutine

	subroutine set_seed2(idum,id)
		integer,intent(in)::idum,id
		integer::seed0,i
		if(idum.eq.0)then
			seed_flag=.false.
			return
		end if
		randomseed=idum
		initial_randomseed=randomseed
		if(MPI_running)then
			call writemess(' ERROR in set_seed, can not run the MPI')
			call error_stop
		end if
		seed0=randomseed
		do i=1,id+1
			randomseed=int(-5970*rand(seed0)) 
		end do
		initial_mpi_randomseed_in_cpus=randomseed
		seed_flag=.true.
		return
	end subroutine


	integer function out_randomseed()
		out_randomseed=randomseed
	end function
	integer function out_initial_randomseed()
		out_initial_randomseed=initial_randomseed
	end function
	integer function out_initial_mpi_randomseed()
		out_initial_mpi_randomseed=initial_mpi_randomseed_in_cpus
	end function
	integer function out_and_set_seed()
		call seed_gen(randomseed)
		seed_flag=.true.
		out_and_set_seed=randomseed
	end function
	
	
	subroutine IndesToaddressRoutine(k,N,Num)
		integer,intent(inout)::k
		integer,intent(in)::N,Num
		integer::i
		logical ::goon
		goon=.true.
		if(N.lt.0)then
			call writemess("ERROR in IndesToaddressRoutine")
			call error_stop()
		end if
		k=1
		do while (goon)
			if((k-1)*N.ge.Num)then
				k=k-1
				goon=.false.
			else
				k=k+1
			end if
		end do
		return
	end subroutine
	subroutine IndesToaddress(TDim,Adim,inde_)
		integer,intent(in) :: TDim(:)!max dimension
		integer,intent(in) :: inde_
		integer,intent(inout)::Adim(:)
		integer :: i,lenDim,productdim,inde
		logical::goon
		lenDim=size(TDim)
		if(size(Adim).ne.lenDim) then
			write(*,*)"ERROR in IndesToaddress"
			call error_stop()
		end if
		inde=inde_
		do i=lenDim,2,-1
			productdim=product(TDim(1:i-1))
			call IndesToaddressRoutine(Adim(i),productdim,inde)
			inde=inde-((Adim(i)-1)*productdim)
		end do
		Adim(1)=inde
		return
	end subroutine		
	
	integer function iselect(num)result(Res)
		class(*),intent(in)::num
		select type(num)
			type is (integer)
				Res=num
			type is (real(kind=4))
				Res=num
			type is (real(kind=8))
				Res=num
			type is (complex(kind=4))
				Res=num
			type is (complex(kind=8))
				Res=num
			class default
				call writemess('ERROR in select type for class(*)',-1)
				call error_stop
		end select
		return
	end function
	real*4 function sselect(num)result(Res)
		class(*),intent(in)::num
		select type(num)
			type is (integer)
				Res=num
			type is (real(kind=4))
				Res=num
			type is (real(kind=8))
				Res=num
			type is (complex(kind=4))
				Res=num
			type is (complex(kind=8))
				Res=num
			class default
				call writemess('ERROR in select type for class(*)',-1)
				call error_stop
		end select
		return
	end function
	real*8 function dselect(num)result(Res)
		class(*),intent(in)::num
		select type(num)
			type is (integer)
				Res=num
			type is (real(kind=4))
				Res=num
			type is (real(kind=8))
				Res=num
			type is (complex(kind=4))
				Res=num
			type is (complex(kind=8))
				Res=num
			class default
				call writemess('ERROR in select type for class(*)',-1)
				call error_stop
		end select
		return
	end function
	complex(kind=4) function cselect(num)result(Res)
		class(*),intent(in)::num
		select type(num)
			type is (integer)
				Res=num
			type is (real(kind=4))
				Res=num
			type is (real(kind=8))
				Res=num
			type is (complex(kind=4))
				Res=num
			type is (complex(kind=8))
				Res=num
			class default
				call writemess('ERROR in select type for class(*)',-1)
				call error_stop
		end select
		return
	end function
	complex(kind=8) function zselect(num)result(Res)
		class(*),intent(in)::num
		select type(num)
			type is (integer)
				Res=num
			type is (real(kind=4))
				Res=num
			type is (real(kind=8))
				Res=num
			type is (complex(kind=4))
				Res=num
			type is (complex(kind=8))
				Res=num
			class default
				call writemess('ERROR in select type for class(*)',-1)
				call error_stop
		end select
		return
	end function
	
	
end module




module memory_type
	use Tools
	implicit none
	private
	
	public::memory
	type memory
		integer,pointer::iwork(:)
		real*4,pointer::swork(:)
		real*8,pointer::dwork(:)
		complex*8,pointer::cwork(:)
		complex*16,pointer::zwork(:)
		logical,pointer::lwork(:)
		character(len=characterLen),pointer::awork(:)
		integer::iLength=0
		integer::iith=0
		integer::sLength=0
		integer::sith=0
		integer::dLength=0
		integer::dith=0
		integer::cLength=0
		integer::cith=0
		integer::zLength=0
		integer::zith=0
		integer::lLength=0
		integer::lith=0
		integer::aLength=0
		integer::aith=0
		logical::DynamicLength=.true.
		logical::Flag=.false.
	contains
		procedure::allocate_memory1,allocate_memory2
		generic,public::allocate =>allocate_memory1,allocate_memory2
		procedure,public::deallocate =>deallocate_memory_All
		procedure::iget_memory,sget_memory,dget_memory,cget_memory,zget_memory,lget_memory,aget_memory
		procedure::iget_memory2,sget_memory2,dget_memory2,cget_memory2,zget_memory2
		generic,public::get_memory=> iget_memory,sget_memory,dget_memory,cget_memory,zget_memory,lget_memory,aget_memory,&
								iget_memory2,sget_memory2,dget_memory2,cget_memory2,zget_memory2
		procedure,public::free=>free_memory_All
		procedure,public::Dynamic
		procedure,public::Static
		procedure,public::print=>print_info
		procedure,public::getLength
		procedure,public::check=>check_memory
		procedure,public::ifDynamic
	end type
	
	
contains
	logical function getFlag(mem)
		class(memory),intent(inout)::mem
		getFlag=mem%flag
	end function
	logical function ifDynamic(mem)
		class(memory),intent(inout)::mem
		ifDynamic=mem%DynamicLength
	end function
	subroutine check_memory(mem,w)
		class(memory),intent(inout)::mem
		character(len=*),intent(in),optional::w
		if(mem%flag)then
			call writemess('some subroutine are using the memory ')
			if(present(w))then
				call writemess('The info of the error is ')
				call writemess(w)
			end if
			call error_stop()
		end if
		return
	end subroutine
	subroutine Dynamic(mem)
		class(memory),intent(inout)::mem
		mem%DynamicLength=.true.
	end subroutine
	subroutine Static(mem)
		class(memory),intent(inout)::mem
		mem%DynamicLength=.false.
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
	
	subroutine allocate_memory1(mem,dataType,length,w)
		class(memory),intent(inout)::mem
		character(len=*),intent(in)::dataType
		character(len=*),intent(in),optional::w
		integer,intent(in)::length
		integer::iType
		iType=select_data_type_char(dataType)
		select case(iType)
			case (1)
				if(mem%iLength.lt.length)then
					if(mem%iith.eq.0)then
						if(associated(mem%iwork)) deallocate(mem%iwork)
						allocate(mem%iwork(length))
						mem%iLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (2)
				if(mem%sLength.lt.length)then
					if(mem%sith.eq.0)then
						if(associated(mem%swork)) deallocate(mem%swork)
						allocate(mem%swork(length))
						mem%sLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (3)
				if(mem%dLength.lt.length)then
					if(mem%dith.eq.0)then
						if(associated(mem%dwork)) deallocate(mem%dwork)
						allocate(mem%dwork(length))
						mem%dLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (4)
				if(mem%cLength.lt.length)then
					if(mem%cith.eq.0)then
						if(associated(mem%cwork)) deallocate(mem%cwork)
						allocate(mem%cwork(length))
						mem%cLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (5)
				if(mem%zLength.lt.length)then
					if(mem%zith.eq.0)then
						if(associated(mem%zwork)) deallocate(mem%zwork)
						allocate(mem%zwork(length))
						mem%zLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (6)
				if(mem%lLength.lt.length)then
					if(mem%lith.eq.0)then
						if(associated(mem%lwork)) deallocate(mem%lwork)
						allocate(mem%lwork(length))
						mem%lLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (7)
				if(mem%aLength.lt.length)then
					if(mem%aith.eq.0)then
						if(associated(mem%awork)) deallocate(mem%awork)
						allocate(mem%awork(length))
						mem%aLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
		end select
	end subroutine
	
	subroutine allocate_memory2(mem,iType,length,w)
		class(memory),intent(inout)::mem
		integer,intent(in)::length,iType
		character(len=*),intent(in),optional::w
		select case(iType)
			case (1)
				if(mem%iLength.lt.length)then
					if(mem%iith.eq.0)then
						if(associated(mem%iwork)) deallocate(mem%iwork)
						allocate(mem%iwork(length))
						mem%iLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (2)
				if(mem%sLength.lt.length)then
					if(mem%sith.eq.0)then
						if(associated(mem%swork)) deallocate(mem%swork)
						allocate(mem%swork(length))
						mem%sLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (3)
				if(mem%dLength.lt.length)then
					if(mem%dith.eq.0)then
						if(associated(mem%dwork)) deallocate(mem%dwork)
						allocate(mem%dwork(length))
						mem%dLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (4)
				if(mem%cLength.lt.length)then
					if(mem%cith.eq.0)then
						if(associated(mem%cwork)) deallocate(mem%cwork)
						allocate(mem%cwork(length))
						mem%cLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (5)
				if(mem%zLength.lt.length)then
					if(mem%zith.eq.0)then
						if(associated(mem%zwork)) deallocate(mem%zwork)
						allocate(mem%zwork(length))
						mem%zLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (6)
				if(mem%lLength.lt.length)then
					if(mem%lith.eq.0)then
						if(associated(mem%lwork)) deallocate(mem%lwork)
						allocate(mem%lwork(length))
						mem%lLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case (7)
				if(mem%aLength.lt.length)then
					if(mem%aith.eq.0)then
						if(associated(mem%awork)) deallocate(mem%awork)
						allocate(mem%awork(length))
						mem%aLength=length
					else
						call writemess('Can not reallocate memory, some subroutine are using the memory!')
						if(present(w))then
							call writemess('The info of the error is ')
							call writemess(w)
						end if
						call error_stop()
					end if
				end if
			case default
				call writemess('ERRO input type in allocate memory')
				call error_stop
		end select
		return
	end subroutine
	
		
	subroutine iget_memory(mem,p,length,w)
		class(memory),target,intent(inout)::mem
		integer,intent(inout),pointer::P(:)
		integer,intent(in)::length
		character(len=*),intent(in),optional::w
		integer::ith
		mem%Flag=.true.
		ith=mem%iith+length
		if(ith.gt.mem%iLength)then
			if(mem%DynamicLength)then
				call mem%allocate(1,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for integer is length='+mem%iLength)
				call error_stop
			end if
		end if
		p=>mem%iwork(mem%iith+1:ith)
		mem%iith=ith
		return
	end subroutine
	subroutine iget_memory2(mem,p,m,n,w)
		class(memory),target,intent(inout)::mem
		integer,intent(inout),pointer::P(:,:)
		integer,intent(in)::m,n
		character(len=*),intent(in),optional::w
		integer::ith,length
		length=m*n
		mem%Flag=.true.
		ith=mem%iith+length
		if(ith.gt.mem%iLength)then
			if(mem%DynamicLength)then
				call mem%allocate(1,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for integer is length='+mem%iLength)
				call error_stop
			end if
		end if
		p(1:m,1:n)=>mem%iwork(mem%iith+1:ith)
		mem%iith=ith
		return
	end subroutine
	subroutine sget_memory(mem,p,length,w)
		class(memory),target,intent(inout)::mem
		real*4,intent(inout),pointer::P(:)
		integer,intent(in)::length
		character(len=*),intent(in),optional::w
		integer::ith
		mem%Flag=.true.
		ith=mem%sith+length
		if(ith.gt.mem%sLength)then
			if(mem%DynamicLength)then
				call mem%allocate(2,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for real*4 is length='+mem%sLength)
				call error_stop
			end if
		end if
		p=>mem%swork(mem%sith+1:ith)
		mem%sith=ith
		return
	end subroutine
	subroutine sget_memory2(mem,p,m,n,w)
		class(memory),target,intent(inout)::mem
		real*4,intent(inout),pointer::P(:,:)
		integer,intent(in)::m,n
		character(len=*),intent(in),optional::w
		integer::ith,length
		mem%Flag=.true.
		length=m*n
		ith=mem%sith+length
		if(ith.gt.mem%sLength)then
			if(mem%DynamicLength)then
				call mem%allocate(2,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for real*4 is length='+mem%sLength)
				call error_stop
			end if
		end if
		p(1:m,1:n)=>mem%swork(mem%sith+1:ith)
		mem%sith=ith
		return
	end subroutine
	subroutine dget_memory(mem,p,length,w)
		class(memory),target,intent(inout)::mem
		real*8,intent(inout),pointer::P(:)
		character(len=*),intent(in),optional::w
		integer,intent(in)::length
		integer::ith
		mem%Flag=.true.
		ith=mem%dith+length
		if(ith.gt.mem%dLength)then
			if(mem%DynamicLength)then
				call mem%allocate(3,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for real*8 is length='+mem%dLength)
				call error_stop
			end if
		end if
		p=>mem%dwork(mem%dith+1:ith)
		mem%dith=ith
		return
	end subroutine
	subroutine dget_memory2(mem,p,m,n,w)
		class(memory),target,intent(inout)::mem
		real*8,intent(inout),pointer::P(:,:)
		character(len=*),intent(in),optional::w
		integer,intent(in)::m,n
		integer::ith,length
		mem%Flag=.true.
		length=m*n
		ith=mem%dith+length
		if(ith.gt.mem%dLength)then
			if(mem%DynamicLength)then
				call mem%allocate(3,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for real*8 is length='+mem%dLength)
				call error_stop
			end if
		end if
		p(1:m,1:n)=>mem%dwork(mem%dith+1:ith)
		mem%dith=ith
		return
	end subroutine
	subroutine cget_memory(mem,p,length,w)
		class(memory),target,intent(inout)::mem
		complex*8,intent(inout),pointer::P(:)
		character(len=*),intent(in),optional::w
		integer,intent(in)::length
		integer::ith
		mem%Flag=.true.
		ith=mem%cith+length
		if(ith.gt.mem%cLength)then
			if(mem%DynamicLength)then
				call mem%allocate(4,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for complex*8 is length='+mem%cLength)
				call error_stop
			end if
		end if
		p=>mem%cwork(mem%cith+1:ith)
		mem%cith=ith
		return
	end subroutine
	subroutine cget_memory2(mem,p,m,n,w)
		class(memory),target,intent(inout)::mem
		character(len=*),intent(in),optional::w
		complex*8,intent(inout),pointer::P(:,:)
		integer,intent(in)::m,n
		integer::ith,length
		length=m*n
		mem%Flag=.true.
		ith=mem%cith+length
		if(ith.gt.mem%cLength)then
			if(mem%DynamicLength)then
				call mem%allocate(4,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for complex*8 is length='+mem%cLength)
				call error_stop
			end if
		end if
		p(1:m,1:n)=>mem%cwork(mem%cith+1:ith)
		mem%cith=ith
		return
	end subroutine
	subroutine zget_memory(mem,p,length,w)
		class(memory),target,intent(inout)::mem
		complex*16,intent(inout),pointer::P(:)
		character(len=*),intent(in),optional::w
		integer,intent(in)::length
		integer::ith
		mem%Flag=.true.
		ith=mem%zith+length
		if(ith.gt.mem%zLength)then
			if(mem%DynamicLength)then
				call mem%allocate(5,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for complex*16 is length='+mem%zLength)
				call error_stop
			end if
		end if
		p=>mem%zwork(mem%zith+1:ith)
		mem%zith=ith
		return
	end subroutine
	subroutine zget_memory2(mem,p,m,n,w)
		class(memory),target,intent(inout)::mem
		complex*16,intent(inout),pointer::P(:,:)
		character(len=*),intent(in),optional::w
		integer,intent(in)::m,n
		integer::ith,length
		length=m*n
		mem%Flag=.true.
		ith=mem%zith+length
		if(ith.gt.mem%zLength)then
			if(mem%DynamicLength)then
				call mem%allocate(5,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for complex*16 is length='+mem%zLength)
				call error_stop
			end if
		end if
		p(1:m,1:n)=>mem%zwork(mem%zith+1:ith)
		mem%zith=ith
		return
	end subroutine
	subroutine lget_memory(mem,p,length,w)
		class(memory),target,intent(inout)::mem
		logical,intent(inout),pointer::P(:)
		integer,intent(in)::length
		character(len=*),intent(in),optional::w
		integer::ith
		mem%Flag=.true.
		ith=mem%lith+length
		if(ith.gt.mem%lLength)then
			if(mem%DynamicLength)then
				call mem%allocate(6,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for logical is length='+mem%lLength)
				call error_stop
			end if
		end if
		p=>mem%lwork(mem%lith+1:ith)
		mem%lith=ith
		return
	end subroutine
	subroutine aget_memory(mem,p,length,w)
		class(memory),target,intent(inout)::mem
		character(len=characterlen),intent(inout),pointer::P(:)
		integer,intent(in)::length
		character(len=*),intent(in),optional::w
		integer::ith
		mem%Flag=.true.
		ith=mem%aith+length
		if(ith.gt.mem%aLength)then
			if(mem%DynamicLength)then
				call mem%allocate(7,ith,w)
			else
				call writemess('maximum memory limit reach')
				call writemess('memory for logical is length='+mem%aLength)
				call error_stop
			end if
		end if
		p=>mem%awork(mem%aith+1:ith)
		mem%aith=ith
		return
	end subroutine
	subroutine free_memory_All(mem)
		class(memory),intent(inout)::mem
		mem%Flag=.false.
		if(deallocate_memory_flag)then
			call deallocate_memory_All(mem)
			return
		end if
		mem%iith=0
		mem%sith=0
		mem%dith=0
		mem%cith=0
		mem%zith=0
		mem%lith=0	
		mem%aith=0
		return
	end subroutine
	subroutine deallocate_memory_All(mem)
		class(memory),intent(inout)::mem
		if(mem%Flag)then
			call writemess('Can not deallocate memory, there are some subroutine using the memory')
			call error_stop
		endif
		if(associated(mem%iwork))deallocate(mem%iwork)	
		if(associated(mem%swork))deallocate(mem%swork)	
		if(associated(mem%dwork))deallocate(mem%dwork)	
		if(associated(mem%cwork))deallocate(mem%cwork)	
		if(associated(mem%zwork))deallocate(mem%zwork)	
		if(associated(mem%lwork))deallocate(mem%lwork)	
		if(associated(mem%awork))deallocate(mem%awork)	
		mem%iith=0
		mem%sith=0
		mem%dith=0
		mem%cith=0
		mem%zith=0
		mem%lith=0	
		mem%aith=0
		mem%iLength=0
		mem%sLength=0
		mem%dLength=0
		mem%cLength=0
		mem%zLength=0
		mem%lLength=0	
		mem%aLength=0
		mem%Flag=.false.
		mem%DynamicLength=.true.
		return
	end subroutine
	subroutine print_info(mem)
		class(memory),intent(inout)::mem
		call writemess('The length of the memory are')
		call writemess('integer         :'+mem%iLength)
		call writemess('real(kind=4)    :'+mem%sLength)
		call writemess('real(kind=8)    :'+mem%dLength)
		call writemess('complex(kind=4) :'+mem%cLength)
		call writemess('complex(kind=8) :'+mem%zLength)
		call writemess('logical         :'+mem%lLength)
		call writemess('character       :'+mem%aLength)
		return
	end subroutine
	subroutine getLength(mem,inoutlen)
		class(memory),intent(inout)::mem
		integer,intent(inout)::inoutlen(:)
		if(size(inoutlen).lt.7)then
			call writemess('ERROR in get length of the memory')
			call error_stop
		end if
		inoutlen(1)=mem%iLength
		inoutlen(2)=mem%sLength
		inoutlen(3)=mem%dLength
		inoutlen(4)=mem%cLength
		inoutlen(5)=mem%zLength
		inoutlen(6)=mem%lLength
		inoutlen(7)=mem%aLength
		return
	end subroutine
















	
end module
