module  onlineOptimization_Tools
	use tensor_type
	use LinearSearchTools
	use Tools
	implicit none
	private
	integer,private,parameter::write_time_num=20
	integer,private,parameter::max_stop_counter=3

	!***************************************************
	!       abstract  definitation of OptimEngine_structure
	!***************************************************

	public::OptimEngine_structure
	type, abstract :: OptimEngine_structure
		logical,private::printFlag=.false.
		type(Tensor),allocatable::st(:)
		type(Tensor),allocatable::yt(:)
		type(Tensor),private,allocatable::workingMemory(:)
		integer::length=0
		logical::FullFlag=.false.
		integer::endindex=0

		integer::CG_direction_Flag=2
		real*8::max_step_in_Linear_search=-1d0
		real*8::first_step_in_Linear_search=0.1d0
		real*8::zero_gradient_in_RGM=-1!in RGM, gradient will regard as 0 when gradient< zero_gradient_in_RGM
		real*8::stop_gradient=-1 !it will stop if norm(gradient)<stop_gradient
		real*8::stop_step=1d-10 !it will stop if x<stop_step for 3 times, x is the search step in every step
		character(len=50)::method='null'
		procedure(LinearSearch_subroutine),pointer::LinearSearch=>LinearSearch1
		procedure(Step1Subroutine_interface),pointer::inStep1=>null()
		procedure(Step2Subroutine_interface),pointer::inStep2=>null()
		procedure(Step3Subroutine_interface),pointer::inStep3=>null()
		procedure(BeforeStepSubroutine_interface),pointer::BeforeStep=>null()
		procedure(EndStepSubroutine_interface),pointer::EndStep=>null()
		procedure(RescalDirection_interface),pointer::RescalDirection=>DefaultRescalDirection
		procedure(AllocateMemory_interface),pointer::AllocateWorkingMemory=>DefaultAllocateWorkingMemory
		procedure(set_DefaultDirection1_interface),pointer::set_Direction1=>set_DefaultDirection1
		procedure(set_DefaultDirection2_interface),pointer::set_Direction2=>set_DefaultDirection2
		procedure(T_tau_routine_interface),pointer::OtherRoutine_in_T_tau_version=>Do_nothing_in_T_tau_version
	contains
		procedure(targetOutSampleFunc), deferred :: target_outSample
		procedure(targetFromSampleFunc), deferred :: target_FromSample
		procedure::Optimization1,Optimization2,Optimization3,Optimization4
		generic,public::Optim=>Optimization1,Optimization2,Optimization3,Optimization4

		procedure,public::setprintFlag=>LBFGSsetprintFlag
		procedure,public::NewElement=>pointNewElement
		procedure,public::i=>element_i
		procedure,public::allocate=>allocatememory
		procedure,public::deallocate=>deallocatememory
		procedure,public::getLength=>datalength
		procedure,public::resetEndpoint
		procedure,public::dataSize
		procedure::check_stop
		procedure,public::pointMemory

		procedure,public::set_CG_direction_flag
		procedure,public::set_stop_error
		procedure,public::set_stop_gradient=>set_stop_error
		procedure,public::set_stop_Step
		procedure,public::set_method
		procedure,public::get_method
		

		procedure,public::set_max_step_in_Linear_search
		procedure,public::set_first_step_in_Linear_search
		procedure,public::set_linear_search_type
		procedure,public::set_linear_search_function
		procedure,public::set_zero_gradient_in_RGM
		generic,public::set_linear_search=>set_linear_search_type,set_linear_search_function
		procedure,public::Set_inStep1Func
		procedure,public::Set_inStep2Func
		procedure,public::Set_inStep3Func
		procedure,public::Set_beforeStepFunc
		procedure,public::Set_EndStepFunc
		procedure,public::unSet_inStep1Func
		procedure,public::unSet_inStep2Func
		procedure,public::unSet_inStep3Func
		procedure,public::unSet_beforeStepFunc
		procedure,public::unSet_EndStepFunc
		procedure,public::Set_RescalDirectionFunc
	end type OptimEngine_structure

	abstract interface
		subroutine targetOutSampleFunc(A,outVal,outGradient,point,sample)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: A
			real*8,intent(inout)::outVal
			type(Tensor),intent(inout)::outGradient
			type(Tensor),intent(in)::point
			type(Tensor),intent(inout)::sample
		end subroutine targetOutSampleFunc
	end interface
	abstract interface
		subroutine targetFromSampleFunc(A,outVal,outGradient,point,sample)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: A
			real*8,intent(inout)::outVal
			type(Tensor),intent(inout)::outGradient
			type(Tensor),intent(in)::point
			type(Tensor),intent(in)::sample
		end subroutine targetFromSampleFunc
	end interface

	abstract interface
		subroutine LinearSearch_subroutine(LBFGSType,max_running,point,dir,x,outValue,inoutgra,sample)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::x,outValue
			type(Tensor),intent(inout)::point
			type(Tensor),intent(inout)::inoutgra
			type(Tensor),intent(in)::dir,sample
			integer,intent(in)::max_running
		end subroutine LinearSearch_subroutine
	end interface

	abstract interface
		subroutine Step1Subroutine_interface(LBFGSType,Value,Gradient,point,ith,t)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::Value
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(inout)::point
			integer,intent(in)::ith
			real*8::t
		end subroutine Step1Subroutine_interface
	end interface
	abstract interface
		subroutine Step2Subroutine_interface(LBFGSType,Value,Gradient,point,ith,t)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::Value
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(inout)::point
			integer,intent(in)::ith
			real*8::t
		end subroutine Step2Subroutine_interface
	end interface
	abstract interface
		subroutine Step3Subroutine_interface(LBFGSType,Value,Gradient,point,ith,t)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::Value
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(inout)::point
			integer,intent(in)::ith
			real*8::t
		end subroutine Step3Subroutine_interface
	end interface

	abstract interface
		subroutine BeforeStepSubroutine_interface(LBFGSType,Value,Gradient,point)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::Value
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(inout)::point
		end subroutine BeforeStepSubroutine_interface
	end interface

	abstract interface
		subroutine EndStepSubroutine_interface(LBFGSType,Value,Gradient,point)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::Value
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(inout)::point
		end subroutine EndStepSubroutine_interface
	end interface


	abstract interface
		subroutine RescalDirection_interface(LBFGSType,Dir)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			type(Tensor),intent(inout)::Dir
		end subroutine RescalDirection_interface
	end interface

	abstract interface
		subroutine AllocateMemory_interface(LBFGSType,sample,Gradient,direction)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			type(Tensor),pointer,intent(inout)::Gradient,direction,sample
		end subroutine AllocateMemory_interface
	end interface

	abstract interface
		subroutine set_DefaultDirection1_interface(LBFGSType,direction,Gradient,point)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			type(Tensor),intent(in)::point,Gradient
			type(Tensor),intent(inout)::direction
		end subroutine set_DefaultDirection1_interface
	end interface

	abstract interface
		subroutine set_DefaultDirection2_interface(LBFGSType,direction,Gradient,point)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			type(Tensor),intent(in)::point,Gradient
			type(Tensor),intent(inout)::direction
		end subroutine set_DefaultDirection2_interface
	end interface
	abstract interface
		subroutine T_tau_routine_interface(LBFGSType,outValue,Gradient,point,sample)
			use tensor_type
			import :: OptimEngine_structure
			class(OptimEngine_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::outValue
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(in)::point,sample
		end subroutine T_tau_routine_interface
	end interface

	!***************************************************
	!        definitation of OptimRunner
	!***************************************************

	public::OptimRunner
	type, extends(OptimEngine_structure) :: OptimRunner
		procedure(external_target_OutSample_interface),pointer,NOPASS,private::externalOutSample=>null()
		procedure(external_target_FromSample_interface),pointer,NOPASS,private::externalFromSample=>null()
	contains
		procedure::target_outSample=>LBFGSoutSample
		procedure::target_FromSample=>LBFGSFromSample
		procedure,public::set_target_function
	end type

	interface
		subroutine external_target_OutSample_interface(outVal,outGradient,point,sample)
			use tensor_type
			real*8,intent(inout)::outVal
			type(Tensor),intent(inout)::outGradient
			type(Tensor),intent(in)::point
			type(Tensor),intent(inout)::sample
		end subroutine external_target_OutSample_interface
	end interface
	interface
		subroutine external_target_FromSample_interface(outVal,outGradient,point,sample)
			use tensor_type
			real*8,intent(inout)::outVal
			type(Tensor),intent(inout)::outGradient
			type(Tensor),intent(in)::point
			type(Tensor),intent(in)::sample
		end subroutine external_target_FromSample_interface
	end interface

contains

	!***************************************************
	!           Basic function for OptimEngine_structure
	!***************************************************

	subroutine LBFGSpointerFunc(p,PointTarget,ith)
		type(Tensor),pointer,intent(inout)::p
		type(Tensor),target,intent(in)::PointTarget(:)
		integer::ith
		p=>PointTarget(ith)
		return
	end subroutine

	subroutine point(BTool,st,yt,ith)
		class(OptimEngine_structure),intent(in)::BTool
		type(Tensor),pointer,intent(inout)::st,yt
		integer,intent(in)::ith
		if(ith.gt.BTool%length)then
			call writemess('ERROR in point to LBFGSTool',-1)
			call writemess('ith='+ith,-1)
			call writemess('BTool%length='+BTool%length,-1)
			call error_stop
		end if
		call LBFGSpointerFunc(st,BTool%st,ith)
		call LBFGSpointerFunc(yt,BTool%yt,ith)
		return
	end subroutine

	subroutine pointNewElement(BTool,st,yt)
		class(OptimEngine_structure),intent(inout)::BTool
		type(Tensor),pointer,intent(inout)::st,yt
		BTool%endindex=BTool%endindex+1
		if(BTool%endindex.gt.BTool%length)then
			BTool%endindex=1
			BTool%FullFlag=.true.
		end if
		call point(BTool,st,yt,BTool%endindex)
		return
	end subroutine

	subroutine element_i(BTool,st,yt,ith)
		class(OptimEngine_structure),intent(in)::BTool
		type(Tensor),pointer,intent(inout)::st,yt
		integer,intent(in)::ith
		integer::i
		if(ith.gt.BTool%length)then
			call writemess('ERROR in element_i,1',-1)
			call error_stop
		end if
		if(BTool%FullFlag)then
			i=BTool%endindex+ith
			if(i.gt.BTool%length)then
				i=i-BTool%length
			end if
		else
			i=ith
		end if
		call point(BTool,st,yt,i)
		return
	end subroutine

	subroutine allocatememory(BTool,length)
		class(OptimEngine_structure),intent(inout)::BTool
		integer,intent(in)::length
		if(length.le.0)then
			call writemess('ERROR in allocatememory for LBFGSTool',-1)
			call error_stop
		end if
		allocate(BTool%st(length))
		allocate(BTool%yt(length))
		BTool%length=length
		BTool%FullFlag=.false.
		BTool%endindex=0
		return
	end subroutine

	subroutine deallocatememory(BTool)
		class(OptimEngine_structure),intent(inout)::BTool
		integer::i
		if(BTool%length.eq.0)return
		do i=1,BTool%length
			call BTool%st(i)%deallocate()
			call BTool%yt(i)%deallocate()
		end do
		deallocate(BTool%yt)
		deallocate(BTool%st)
		BTool%length=0
		BTool%FullFlag=.false.
		BTool%endindex=0
		return
	end subroutine

	subroutine resetEndpoint(BTool)
		class(OptimEngine_structure),intent(inout)::BTool
		BTool%endindex=0
		BTool%FullFlag=.false.
		return
	end subroutine
	function datalength(BTool)
		integer::datalength
		class(OptimEngine_structure),intent(in)::BTool
		if(BTool%FullFlag)then
			datalength=BTool%length
		else
			datalength=BTool%endindex
		end if
		return
	end function

	function dataSize(BTool)
		integer::dataSize
		class(OptimEngine_structure),intent(in)::BTool
		dataSize=BTool%length
		return
	end function

	subroutine LBFGSsetprintFlag(LBFGSType,printFlag)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		logical,intent(in)::printFlag
		LBFGSType%printFlag=printFlag
		return
	end subroutine

	subroutine pointMemory(LBFGSType,p,ith)
		class(OptimEngine_structure),target, intent(inout) :: LBFGSType
		type(Tensor),pointer::p
		integer,intent(in)::ith
		if(.not.allocated(LBFGSType%workingMemory))then
			call writemess('ERROR in pointMemory, DO NOT allocate memory yet',-1)
			call error_stop
		end if
		if(ith.gt.size(LBFGSType%workingMemory))then
			call writemess('ERROR in pointMemory, ith> size(memory)',-1)
			call error_stop
		end if
		p=>LBFGSType%workingMemory(ith)
		return
	end subroutine

	function get_method(LBFGSType)
		character(len=200)::get_method
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		get_method=LBFGSType%method
		return
	end function

	!***************************************************
	!           initial function for OptimEngine_structure
	!***************************************************

	subroutine set_CG_direction_flag(LBFGSType,Flag)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		integer,intent(in)::Flag
		LBFGSType%CG_direction_Flag=Flag
		return
	end subroutine

	subroutine set_stop_error(LBFGSType,error)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::error
		LBFGSType%stop_gradient=error
		return
	end subroutine
	subroutine set_stop_Step(LBFGSType,step)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::step
		LBFGSType%stop_step=step
		return
	end subroutine
	subroutine set_max_step_in_Linear_search(LBFGSType,step)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::step
		LBFGSType%max_step_in_Linear_search=step
		return
	end subroutine
	subroutine set_first_step_in_Linear_search(LBFGSType,step)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::step
		LBFGSType%first_step_in_Linear_search=step
		return
	end subroutine
	subroutine set_zero_gradient_in_RGM(LBFGSType,zero_gradient)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::zero_gradient
		LBFGSType%zero_gradient_in_RGM=zero_gradient
		return
	end subroutine

	function check_stop(LBFGSType,gradient,step)
		logical::check_stop
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::gradient
		real*8,intent(in)::step
		real*8::norm
		character(len=characterlen)::w
		integer,save::stop_counter=0

		check_stop=.false.
		if(LBFGSType%stop_gradient.gt.0) then
			norm=gradient%dnorm()
			check_stop=norm.le.LBFGSType%stop_gradient
			if(check_stop)then
				w='Search is going to stop, norm of the gradient='+norm
				w=w+', The giving stop error='+LBFGSType%stop_gradient
				call writemess(w)
				return
			end if
		end if

		if(step.le.LBFGSType%stop_step)then
			stop_counter=stop_counter+1
			if(stop_counter.ge.max_stop_counter)then
				check_stop=.true.
				w='Search is going to stop, the search step='+step
				call writemess(w)
				return
			end if
		else
			stop_counter=0
		end if
		return
	end function

	subroutine set_linear_search_type(LBFGSType,flag)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		integer,intent(in)::flag
		if(flag.eq.1)then
			LBFGSType%LinearSearch=>LinearSearch1
			return
		end if
		if(flag.eq.2)then
			LBFGSType%LinearSearch=>LinearSearch2
			return
		end if
		call writemess('No such case',-1)
		call error_stop
	end subroutine
	subroutine set_linear_search_function(LBFGSType,Func)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		procedure(LinearSearch_subroutine)::Func
		LBFGSType%LinearSearch=>Func
		return
	end subroutine

	subroutine Set_inStep1Func(LBFGSType,Func)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		procedure(Step1Subroutine_interface)::Func
		LBFGSType%inStep1=>Func
		return
	end subroutine
	subroutine Set_inStep2Func(LBFGSType,Func)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		procedure(Step2Subroutine_interface)::Func
		LBFGSType%inStep2=>Func
		return
	end subroutine
	subroutine Set_inStep3Func(LBFGSType,Func)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		procedure(Step3Subroutine_interface)::Func
		LBFGSType%inStep3=>Func
		return
	end subroutine
	subroutine unSet_inStep1Func(LBFGSType)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		LBFGSType%inStep1=>null()
		return
	end subroutine
	subroutine unSet_inStep2Func(LBFGSType)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		LBFGSType%inStep2=>null()
		return
	end subroutine
	subroutine unSet_inStep3Func(LBFGSType)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		LBFGSType%inStep3=>null()
		return
	end subroutine
	subroutine Set_BeforeStepFunc(LBFGSType,Func)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		procedure(BeforeStepSubroutine_interface)::Func
		LBFGSType%BeforeStep=>Func
		return
	end subroutine
	subroutine unSet_BeforeStepFunc(LBFGSType)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		LBFGSType%BeforeStep=>null()
		return
	end subroutine
	subroutine Set_EndStepFunc(LBFGSType,Func)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		procedure(EndStepSubroutine_interface)::Func
		LBFGSType%EndStep=>Func
		return
	end subroutine
	subroutine unSet_EndStepFunc(LBFGSType)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		LBFGSType%EndStep=>null()
		return
	end subroutine

	subroutine Set_RescalDirectionFunc(LBFGSType,Func)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		procedure(RescalDirection_interface)::Func
		LBFGSType%RescalDirection=>Func
		return
	end subroutine

	subroutine set_method(LBFGSType,method)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		character(len=*),intent(in)::method
		LBFGSType%method=method
		call writemess('Set the optimal method='+method)
		select case(method)
			case ('GM')
				LBFGSType%RescalDirection=>DefaultRescalDirection
				LBFGSType%AllocateWorkingMemory=>GMMemory
				LBFGSType%set_Direction1=>set_GMDirection1
				LBFGSType%set_Direction2=>set_GMDirection2
				LBFGSType%OtherRoutine_in_T_tau_version=>Do_nothing_in_T_tau_version
			case ('RGM')
				LBFGSType%RescalDirection=>RandomGradientRescalDirection
				LBFGSType%AllocateWorkingMemory=>RGMMemory
				LBFGSType%set_Direction1=>set_RGMDirection1
				LBFGSType%set_Direction2=>set_RGMDirection2
				LBFGSType%OtherRoutine_in_T_tau_version=>Do_nothing_in_T_tau_version
			case ('CG')
				LBFGSType%RescalDirection=>DefaultRescalDirection
				LBFGSType%AllocateWorkingMemory=>CGMemory
				LBFGSType%set_Direction1=>set_CGDirection1
				LBFGSType%set_Direction2=>set_CGDirection2
				LBFGSType%OtherRoutine_in_T_tau_version=>Do_nothing_in_T_tau_version
			case ('BFGS')
				LBFGSType%RescalDirection=>DefaultRescalDirection
				LBFGSType%AllocateWorkingMemory=>BFGSMemory
				LBFGSType%set_Direction1=>set_BFGSDirection1
				LBFGSType%set_Direction2=>set_BFGSDirection2
				LBFGSType%OtherRoutine_in_T_tau_version=>run_Energy_in_T_tau_version
			case ('LBFGS')
				LBFGSType%RescalDirection=>DefaultRescalDirection
				LBFGSType%AllocateWorkingMemory=>LBFGSMemory
				LBFGSType%set_Direction1=>set_LBFGSDirection1
				LBFGSType%set_Direction2=>set_LBFGSDirection2
				LBFGSType%OtherRoutine_in_T_tau_version=>run_Energy_in_T_tau_version
			case ('RLBFGS')
				LBFGSType%RescalDirection=>RandomRescalDirection
				LBFGSType%AllocateWorkingMemory=>LBFGSMemory
				LBFGSType%set_Direction1=>set_LBFGSDirection1
				LBFGSType%set_Direction2=>set_LBFGSDirection2
				LBFGSType%OtherRoutine_in_T_tau_version=>run_Energy_in_T_tau_version
			case default
				call writemess('NO such case of method, the default method are:')
				call writemess(' GM    :  Gradient Method')
				call writemess(' RGM   : Random Gradient Method')
				call writemess(' CG    : Conjugate Gradient method ')
				call writemess(' BFGS  : BFGS method ')
				call writemess(' LBFGS : LBFGS method ')
				call error_stop
		end select
		return
	end subroutine

	!***************************************************
	!           Basic function for OptimRunner
	!***************************************************


	subroutine set_target_function(LBFGSer,outSampleFunc,FromSampleFunc)
		class(OptimRunner),intent(inout)::LBFGSer
		procedure(external_target_OutSample_interface)::outSampleFunc
		procedure(external_target_FromSample_interface)::FromSampleFunc
		LBFGSer%externalOutSample=>outSampleFunc
		LBFGSer%externalFromSample=>FromSampleFunc
		return
	end subroutine

	subroutine LBFGSoutSample(A,outVal,outGradient,point,sample)
		class(OptimRunner), intent(inout) :: A
		real*8,intent(inout)::outVal
		type(Tensor),intent(inout)::outGradient
		type(Tensor),intent(in)::point
		type(Tensor),intent(inout)::sample
		if(associated(A%externalOutSample))then
			call A%externalOutSample(outVal,outGradient,point,sample)
		else
			call writemess('DO not set the target function yet',-1)
			call error_stop
		end if
		return
	end subroutine
	subroutine LBFGSFromSample(A,outVal,outGradient,point,sample)
		class(OptimRunner), intent(inout) :: A
		real*8,intent(inout)::outVal
		type(Tensor),intent(inout)::outGradient
		type(Tensor),intent(in)::point
		type(Tensor),intent(in)::sample
		if(associated(A%externalOutSample))then
			call A%externalFromSample(outVal,outGradient,point,sample)
		else
			call writemess('DO not set the target function yet',-1)
			call error_stop
		end if
		return
	end subroutine

	!***************************************************
	!           default function
	!***************************************************

	subroutine DefaultRescalDirection(LBFGSType,Dir)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(inout)::Dir
		Dir=Dir/Dir%dnorm()
		return
	end subroutine

	subroutine RandomRescalDirection(LBFGSType,Dir)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(inout)::Dir
		real*8,pointer::Dirp(:)
		integer::i
		call Dir%pointer(Dirp)
		do i=1,Dir%getTotalData()
			if(Dirp(i).gt.0)then
				Dirp(i)=randomnumber()
			else
				Dirp(i)=-1d0*randomnumber()
			end if
		end do
		return
	end subroutine

	subroutine DefaultAllocateWorkingMemory(LBFGSType,sample,Gradient,direction)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),pointer,intent(inout)::sample,Gradient,direction
		call printERRORMessage()
	end subroutine
	subroutine set_DefaultDirection1(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		call printERRORMessage()
	end subroutine
	subroutine set_DefaultDirection2(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		call printERRORMessage()
	end subroutine
	subroutine printERRORMessage()
		call writemess('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
		call writemess('% DO NOT set the optimal method yet     %')
		call writemess('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
		call error_stop
	end subroutine

	subroutine Do_nothing_in_T_tau_version(LBFGSType,outValue,Gradient,point,sample)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(inout)::outValue
		type(Tensor),intent(inout)::Gradient
		type(Tensor),intent(in)::point,sample
		return
	end subroutine
	subroutine run_Energy_in_T_tau_version(LBFGSType,outValue,Gradient,point,sample)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(inout)::outValue
		type(Tensor),intent(inout)::Gradient
		type(Tensor),intent(in)::point,sample
		call LBFGSType%target_FromSample(outValue,Gradient,point,sample)
		return
	end subroutine

	
	!***************************************************
	!            optimization
	!***************************************************


	subroutine Optimization1(LBFGSType,T0,tau0,numStep,Point,outValue)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		real*8::t
		type(Tensor),pointer::Gradient,direction,sample

		if((LBFGSType%method.equ.'LBFGS').and.(LBFGSType%dataSize().le.0))then
			call writemess('ERROR in LBFGS',-1)
			call writemess(' DO NOT allocate memory for running',-1)
			call error_stop
		end if
		if(LBFGSType%printFlag)call reset_time_calculator(numStep,write_time_num) 
		call LBFGSType%resetEndpoint()
		call LBFGSType%AllocateWorkingMemory(sample,Gradient,direction)
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient,t))exit
			call LBFGSType%set_direction1(direction,Gradient,point)
			call LBFGSType%RescalDirection(direction)
			point=point+(direction*t)
			call LBFGSType%OtherRoutine_in_T_tau_version(outValue,Gradient,point,sample)
			call LBFGSType%set_direction2(direction,Gradient,point)
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine Optimization2(LBFGSType,t,numStep,Point,outValue)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor),pointer::Gradient,direction,sample
		if((LBFGSType%method.equ.'LBFGS').and.(LBFGSType%dataSize().le.0))then
			call writemess('ERROR in LBFGS',-1)
			call writemess(' DO NOT allocate memory for running',-1)
			call error_stop
		end if
		if(LBFGSType%printFlag)call reset_time_calculator(numStep,write_time_num) 
			
		call LBFGSType%AllocateWorkingMemory(sample,Gradient,direction)
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient,t))exit
			call LBFGSType%set_direction1(direction,Gradient,point)
			call LBFGSType%RescalDirection(direction)
			point=point+(direction*t)
			call LBFGSType%OtherRoutine_in_T_tau_version(outValue,Gradient,point,sample)
			call LBFGSType%set_direction2(direction,Gradient,point)
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine Optimization3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		integer,intent(in)::NlinearStep,numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		real*8::x
		integer::i
		type(Tensor),pointer::Gradient,direction,sample
		if((LBFGSType%method.equ.'LBFGS').and.(LBFGSType%dataSize().le.0))then
			call writemess('ERROR in LBFGS',-1)
			call writemess(' DO NOT allocate memory for running',-1)
			call error_stop
		end if
		if(LBFGSType%printFlag)call reset_time_calculator(numStep,write_time_num) 
		call LBFGSType%AllocateWorkingMemory(sample,Gradient,direction)
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		x=-1
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,x)
			call LBFGSType%target_outSample(outValue,Gradient,point,sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,x)
			call LBFGSType%set_direction1(direction,Gradient,point)
			call LBFGSType%RescalDirection(direction)
			call LBFGSType%LinearSearch(NlinearStep,point,direction,x,outValue,Gradient,sample)
			if(LBFGSType%check_stop(Gradient,x))exit
			call LBFGSType%set_direction2(direction,Gradient,point)
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,x)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine Optimization4(LBFGSType,numStep,Point,outValue)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::NlinearStep
		call Optimization3(LBFGSType,NlinearStep,numStep,Point,outValue)
		return
	end subroutine

	!***************************************************
	!            Gradient optimization
	!***************************************************

	subroutine GMMemory(LBFGSType,Sample,Gradient,direction)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),pointer,intent(inout)::Gradient,direction,Sample
		integer::memorylen
		memorylen=3
		if(allocated(LBFGSType%workingMemory))then
			if(size(LBFGSType%workingMemory).ne.memorylen)then
				deallocate(LBFGSType%workingMemory)
				allocate(LBFGSType%workingMemory(memorylen))
			end if
		else
			allocate(LBFGSType%workingMemory(memorylen))
		end if
		call LBFGSType%pointMemory(Sample,1)
		call LBFGSType%pointMemory(Gradient,2)
		call LBFGSType%pointMemory(direction,3)
		return
	end subroutine
	subroutine set_GMDirection1(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		direction=(-1d0)*Gradient
		return
	end subroutine
	subroutine set_GMDirection2(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		return
	end subroutine


	!***************************************************
	!           random Gradient optimization
	!***************************************************

	subroutine RandomGradient(LBFGSType,Direction,gradient)!dir=-1*gra
		class(OptimEngine_structure), intent(in) :: LBFGSType
		type(Tensor),intent(in)::gradient
		type(Tensor),intent(inout)::Direction	
		integer::i
		real*8,pointer::dirp(:),grap(:)
		call Direction%empty()
		call Direction%allocate([gradient%getTotalData()],'real*8')
		call gradient%pointer(grap)
		call Direction%pointer(dirp)
		do i=1,gradient%getTotalData()
			if(abs(grap(i)).gt.LBFGSType%zero_gradient_in_RGM)then
				if(grap(i).gt.0)then
					dirp(i)=-1d0*randomnumber()
				else
					dirp(i)=randomnumber()
				end if
			else
				dirp(i)=0
			end if
		end do
		return
	end subroutine
	subroutine RandomGradientRescalDirection(LBFGSType,Dir)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(inout)::Dir
		return
	end subroutine

	subroutine RGMMemory(LBFGSType,sample,Gradient,direction)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),pointer,intent(inout)::Gradient,direction,sample
		integer::memorylen
		memorylen=3
		if(allocated(LBFGSType%workingMemory))then
			if(size(LBFGSType%workingMemory).ne.memorylen)then
				deallocate(LBFGSType%workingMemory)
				allocate(LBFGSType%workingMemory(memorylen))
			end if
		else
			allocate(LBFGSType%workingMemory(memorylen))
		end if
		call LBFGSType%pointMemory(sample,1)
		call LBFGSType%pointMemory(Gradient,2)
		call LBFGSType%pointMemory(direction,3)
		return
	end subroutine
	subroutine set_RGMDirection1(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		call RandomGradient(LBFGSType,direction,Gradient)
		return
	end subroutine
	subroutine set_RGMDirection2(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		return
	end subroutine


	!***************************************************
	!           Conjugate Gradient optimization
	!***************************************************

	subroutine CG_direction(LBFGSType,dir,Gra,priorgra)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::Gra
		type(Tensor),intent(inout)::dir,priorgra
		real*8::direction_corr
		if(dir%getFlag()) then
			direction_corr=correctPara(LBFGSType,gra,priorGra)
			dir=(direction_corr*dir)-gra
		else
			dir=(-1d0)*gra
		end if
		priorgra=Gra
		return
	end subroutine
	
	function correctPara(LBFGSType,newgra,gra)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8::correctPara
		type(Tensor),intent(in)::newgra,gra
		real*8::nor
		if(LBFGSType%CG_direction_Flag.eq.0)then
			correctPara=0d0
			return
		end if
		if(LBFGSType%CG_direction_Flag.eq.1)then
			correctPara=newgra.dot.gra
			correctPara=correctPara/gra%dnorm2()
			return
		end if
		if(LBFGSType%CG_direction_Flag.eq.2)then
			correctPara=newgra.dot.(newgra-gra)
			correctPara=correctPara/gra%dnorm2()
			return
		end if
		nor=1d0/gra%dnorm2()
		correctPara=newgra.dot.gra
		correctPara=correctPara*nor
		if(correctPara.le.0.5) then
			correctPara=(newgra%dnorm2()*nor)-correctPara
		end if
		return
	end function

	subroutine CGMemory(LBFGSType,sample,Gradient,direction)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),pointer,intent(inout)::Gradient,direction,sample
		integer::memorylen
		memorylen=5
		if(allocated(LBFGSType%workingMemory))then
			if(size(LBFGSType%workingMemory).ne.memorylen)then
				deallocate(LBFGSType%workingMemory)
				allocate(LBFGSType%workingMemory(memorylen))
			end if
		else
			allocate(LBFGSType%workingMemory(memorylen))
		end if
		call LBFGSType%pointMemory(sample,1)
		call LBFGSType%pointMemory(Gradient,2)
		call LBFGSType%pointMemory(direction,3)
		return
	end subroutine
	subroutine set_CGDirection1(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		type(Tensor),pointer::gra0,dir0
		call LBFGSType%pointMemory(gra0,4)
		call LBFGSType%pointMemory(dir0,5)
		call CG_direction(LBFGSType,dir0,Gradient,gra0)
		direction=dir0
		return
	end subroutine
	subroutine set_CGDirection2(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		return
	end subroutine


	!***************************************************
	!            BFGS optimization
	!***************************************************

	subroutine BFGS_direction(LBFGSType,C,y,s)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(inout)::C,y,s
		type(Tensor)::one
		integer::dimn
		type(Tensor)::sy,ss,tempC
		real*8::rho
		if(.not.y%getFlag())return
		dimn=y%getTotalData()
		call one%eye(dimn,dimn,y%getclassType())
		if(.not.C%getFlag())then
			call writemess('ERROR in BFGS_direction',-1)
			call error_stop
		end if
		rho=y.dot.s
		rho=1d0/rho
		sy=s.xx.y
		ss=s.xx.s
		tempC=one-(rho*sy)
		C=tempC * C
		C=C*( .p.tempC )
		C=C+(rho*ss)
		!C=( (one-(rho*sy)) * C * ( one-(rho*(.T.sy)) ) )+(rho*ss)
		return
	end subroutine

	subroutine BFGSMemory(LBFGSType,sample,Gradient,direction)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),pointer,intent(inout)::Gradient,direction,sample
		type(Tensor),pointer::Ct,yt,st
		integer::memorylen
		memorylen=8
		if(allocated(LBFGSType%workingMemory))then
			if(size(LBFGSType%workingMemory).ne.memorylen)then
				deallocate(LBFGSType%workingMemory)
				allocate(LBFGSType%workingMemory(memorylen))
			end if
		else
			allocate(LBFGSType%workingMemory(memorylen))
		end if
		call LBFGSType%pointMemory(sample,1)
		call LBFGSType%pointMemory(Gradient,2)
		call LBFGSType%pointMemory(direction,3)
		call LBFGSType%pointMemory(Ct,4)
		call LBFGSType%pointMemory(yt,5)
		call LBFGSType%pointMemory(st,6)
		call yt%empty()
		call St%empty()
		call Ct%empty
		return
	end subroutine

	subroutine set_BFGSDirection1(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		type(Tensor),pointer::Ct,yt,st,SavePoint,SaveGradient
		logical,save::first=.true.
		call LBFGSType%pointMemory(Ct,4)
		call LBFGSType%pointMemory(yt,5)
		call LBFGSType%pointMemory(st,6)
		call LBFGSType%pointMemory(SavePoint,7)
		call LBFGSType%pointMemory(SaveGradient,8)
		if(.not.Ct%getFlag())then
			Ct=eye(Point%getTotalData(),Point%getTotalData(),Point%getclassType())
		end if
		call BFGS_direction(LBFGSType,Ct,yt,st)
		direction=Ct*Gradient*(-1d0)
		SavePoint=point
		SaveGradient=Gradient
		return
	end subroutine
	subroutine set_BFGSDirection2(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		type(Tensor),pointer::Ct,yt,st,SavePoint,SaveGradient
		logical,save::first=.true.
		call LBFGSType%pointMemory(Ct,4)
		call LBFGSType%pointMemory(yt,5)
		call LBFGSType%pointMemory(st,6)
		call LBFGSType%pointMemory(SavePoint,7)
		call LBFGSType%pointMemory(SaveGradient,8)
		st=point-SavePoint
		yt=Gradient-SaveGradient
		if(first)then
			Ct=((st.dot.yt)/(yt.dot.yt))*Ct
			first=.false.
		end if
		return
	end subroutine


	!***************************************************
	!            LBFGS optimization
	!***************************************************

	! p is \Delta f(x_i)
	! s_i= x_{i+1} - x_i
	! y_i= p_{i+1} - p_i

	subroutine LBFGS_direction(LBFGS,p,inputp)
		class(OptimEngine_structure),intent(inout)::LBFGS
		type(Tensor),intent(inout)::p
		type(Tensor),intent(in)::inputp
		real*8::temp,temp2,beta
		real*8,allocatable::alpha(:)
		integer::i,length
		type(Tensor),pointer::s,y
		length=LBFGS%getLength()
		p=inputp*(-1d0)
		if(length.eq.0)return

		allocate(alpha(length))
		do i=length,1,-1
			call LBFGS%i(s,y,i)
			temp=y.dot.s
			alpha(i)=s.dot.p
			alpha(i)=alpha(i)/temp
			p=p-(y*alpha(i))
		end do

		call LBFGS%i(s,y,length)
		temp=y.dot.s
		temp=temp/y%dnorm2()
		p=p*temp
	

		do i=1,length
			call LBFGS%i(s,y,i)
			temp=y.dot.s
			beta=y.dot.p
			beta=beta/temp
			p=p+(s*(alpha(i)-beta))
		end do
		return
	end subroutine

	subroutine LBFGSMemory(LBFGSType,sample,Gradient,direction)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),pointer,intent(inout)::Gradient,direction,sample
		integer::memorylen
		memorylen=5
		if(allocated(LBFGSType%workingMemory))then
			if(size(LBFGSType%workingMemory).ne.memorylen)then
				deallocate(LBFGSType%workingMemory)
				allocate(LBFGSType%workingMemory(memorylen))
			end if
		else
			allocate(LBFGSType%workingMemory(memorylen))
		end if
		call LBFGSType%pointMemory(sample,1)
		call LBFGSType%pointMemory(Gradient,2)
		call LBFGSType%pointMemory(direction,3)
		return
	end subroutine
	subroutine set_LBFGSDirection1(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		type(Tensor),pointer::yt,st,SavePoint,SaveGradient
		call LBFGSType%pointMemory(SavePoint,4)
		call LBFGSType%pointMemory(SaveGradient,5)
		call LBFGS_direction(LBFGSType,Direction,Gradient)
		SaveGradient=Gradient
		SavePoint=point
		return
	end subroutine
	subroutine set_LBFGSDirection2(LBFGSType,direction,Gradient,point)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::point,Gradient
		type(Tensor),intent(inout)::direction
		type(Tensor),pointer::yt,st,SavePoint,SaveGradient
		call LBFGSType%pointMemory(SavePoint,4)
		call LBFGSType%pointMemory(SaveGradient,5)
		call LBFGSType%NewElement(st,yt)
		yt=Gradient-SaveGradient
		st=point-SavePoint
		return
	end subroutine





	!*************************************************************
	!*************************************************************
	!                 linear search2
	!*************************************************************

	subroutine LinearSearch1(LBFGSType,max_running,point,dir,x,outValue,inoutgra,sample)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(inout)::x,outValue
		type(Tensor),intent(inout)::point
		type(Tensor),intent(inout)::inoutgra
		type(Tensor),intent(in)::dir,sample
		integer,intent(in)::max_running
		logical::stopflag
		real*8::x1,x2,x3,outx
		real*8::f1,f2,f3,g1,g2,g3
		type(Tensor)::gradient,NewPoint
		integer::i
		type(Tensor)::Savepoint,Savegradient
		real*8::savex,saveValue
		logical::max_step_flag
		if(max_running.lt.2)then
			call writemess('ERROR in LinearSearch,input max_running should >=2',-1)
			call error_stop
		end if
		max_step_flag=LBFGSType%max_step_in_Linear_search.gt.0
		x1=0d0
		g1=inoutgra.dot.dir
		f1=outValue

		if((dabs(x).le.LBFGSType%stop_step).or.(x.le.0)) then
			x2=LBFGSType%first_step_in_Linear_search
			x=x2
		else 
			x2=x
		end if

		NewPoint=point+(x2*dir)
		call LBFGSType%target_FromSample(f2,gradient,NewPoint,sample)
		g2=gradient.dot.dir
		if(max_step_flag)then
			Savepoint=NewPoint
			Savegradient=gradient
			savex=x2
			saveValue=f2
		end if


		if(dabs(g2).le.LBFGSType%stop_gradient)then
			Point=NewPoint
			x=x2
			outValue=f2
			inoutgra=gradient
			return
		end if
		
		call LinearSearch_third_point(outx,f1,g1,f2,g2,x2)
		x3=outx
		NewPoint=point+(x3*dir)
		call LBFGSType%target_FromSample(f3,gradient,NewPoint,sample)
		g3=gradient.dot.dir

		if(dabs(g3).le.LBFGSType%stop_gradient)then
			Point=NewPoint
			x=x3
			outValue=f3
			inoutgra=gradient
			return
		end if

		stopflag=.false.

		do i=3,max_running
			call LinearSearch_fouth_point(outx,stopflag,x1,f1,g1,x2,f2,g2,x3,f3,g3,x)
			if(isnan(outx).or.(outx.lt.0d0))then
				write(*,*)"ERROR in LinearSearch"
				write(*,*)"x1,x2"
				write(*,*)x1
				write(*,*)x2
				stop
			end if
			NewPoint=Point+(outx*dir)
			call LBFGSType%target_FromSample(f3,gradient,NewPoint,sample)
			g3=gradient.x.dir
			x3=outx
			if((dabs(g3).le.LBFGSType%stop_gradient).or. stopflag) exit
		end do
		if(max_step_flag.and.(outx.gt.LBFGSType%max_step_in_Linear_search))then
			Point=Savepoint
			x=Savex
			outValue=SaveValue
			inoutgra=Savegradient
		else
			Point=NewPoint
			x=outx
			outValue=f3
			inoutgra=gradient
		end if
		return
	end subroutine


	!*************************************************************
	!*************************************************************
	!                 linear search2
	!*************************************************************

	!LinearSearch2
		!Alway keep the point with min value

	subroutine LinearSearch2(LBFGSType,max_running,point,dir,x,outValue,inoutgra,sample)
		class(OptimEngine_structure), intent(inout) :: LBFGSType
		real*8,intent(inout)::x,outValue
		type(Tensor),intent(inout)::point
		type(Tensor),intent(inout)::inoutgra
		type(Tensor),intent(in)::dir,sample
		integer,intent(in)::max_running
		logical::stopflag
		real*8::x1,x2,x3,outx
		real*8::f1,f2,f3,g1,g2,g3
		type(Tensor)::gradient,NewPoint
		integer::i
		type(Tensor)::minpoint,mingradient
		real*8::minx,minValue
			call writemess('ERROR in LinearSearch,DO NOT finsiehd this part yet',-1)
			call error_stop
		return
	end subroutine


end module
