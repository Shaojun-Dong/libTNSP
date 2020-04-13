module  OptimizationFocQMC_Tools
	use tensor_type
	use Tools
	implicit none
	private
	integer,private,parameter::write_time_num=20
	real*8,private,parameter::zero_number=1d-16
	real*8,private,parameter::gradient_zero_number=5d-8!To small will error
	real*8,private,parameter::InfinityNumber=1d100

	!***************************************************
	!       abstract  definitation of oLBFGS_structure
	!***************************************************

	public::oLBFGS_structure
	type, abstract :: oLBFGS_structure
		logical,private::printFlag=.false.
		type(Tensor),allocatable::st(:)
		type(Tensor),allocatable::yt(:)
		integer::length=0
		logical::FullFlag=.false.
		integer::endindex=0
		integer::CG_direction_Flag=2
		real*8::stop_error=-1
		real*8::max_step_in_Linear_search=-1d0
		real*8::first_step_in_Linear_search=0.1d0
		real*8::zero_gradient_in_RGM=-1
		procedure(LinearSearch_subroutine),pointer::LinearSearch=>LinearSearch2
		procedure(Step1Subroutine_interface),pointer::inStep1=>null()
		procedure(Step2Subroutine_interface),pointer::inStep2=>null()
		procedure(Step3Subroutine_interface),pointer::inStep3=>null()
		procedure(BeforeStepSubroutine_interface),pointer::BeforeStep=>null()
		procedure(EndStepSubroutine_interface),pointer::EndStep=>null()
	contains
		procedure(targetOutSampleFunc), deferred :: target_outSample
		procedure(targetFromSampleFunc), deferred :: target_FromSample
		procedure::GradientUpdate1,GradientUpdate2,GradientUpdate3,GradientUpdate4
		generic,public::GM=>GradientUpdate1,GradientUpdate2,GradientUpdate3,GradientUpdate4

		procedure::RandomGradientUpdate1,RandomGradientUpdate2,&
					RandomGradientUpdate3,RandomGradientUpdate4
		generic,public::RGM=>RandomGradientUpdate1,RandomGradientUpdate2,&
					RandomGradientUpdate3,RandomGradientUpdate4

		procedure,public::CG_direction
		procedure::CGUpdate1,CGUpdate2,CGUpdate3,CGUpdate4
		generic,public::CG=>CGUpdate1,CGUpdate2,CGUpdate3,CGUpdate4

		procedure,public::BFGS_direction
		procedure::BFGSUpdate1,BFGSUpdate2,BFGSUpdate3,BFGSUpdate4
		generic,public::BFGS=>BFGSUpdate1,BFGSUpdate2,BFGSUpdate3,BFGSUpdate4

		procedure,public::LBFGS_direction
		procedure::LBFGSUpdate2,LBFGSUpdate1,LBFGSUpdate3,LBFGSUpdate4
		generic,public::LBFGS=>LBFGSUpdate2,LBFGSUpdate1,LBFGSUpdate3,LBFGSUpdate4 

		procedure,public::setprintFlag=>LBFGSsetprintFlag
		procedure,public::NewElement=>pointNewElement
		procedure,public::i=>element_i
		procedure,public::allocate=>allocatememory
		procedure,public::deallocate=>deallocatememory
		procedure,public::getLength=>datalength
		procedure,public::resetEndpoint
		procedure,public::dataSize
		procedure,public::set_CG_direction_flag
		procedure,public::set_stop_error
		procedure::check_stop
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
	end type oLBFGS_structure

	abstract interface
		subroutine targetOutSampleFunc(A,outVal,outGradient,point,sample)
			use tensor_type
			import :: oLBFGS_structure
			class(oLBFGS_structure), intent(inout) :: A
			real*8,intent(inout)::outVal
			type(Tensor),intent(inout)::outGradient
			type(Tensor),intent(in)::point
			type(Tensor),intent(inout)::sample
		end subroutine targetOutSampleFunc
	end interface
	abstract interface
		subroutine targetFromSampleFunc(A,outVal,outGradient,point,sample)
			use tensor_type
			import :: oLBFGS_structure
			class(oLBFGS_structure), intent(inout) :: A
			real*8,intent(inout)::outVal
			type(Tensor),intent(inout)::outGradient
			type(Tensor),intent(in)::point
			type(Tensor),intent(in)::sample
		end subroutine targetFromSampleFunc
	end interface

	abstract interface
		subroutine LinearSearch_subroutine(LBFGSType,max_running,point,dir,x,outValue,inoutgra,sample)
			use tensor_type
			import :: oLBFGS_structure
			class(oLBFGS_structure), intent(inout) :: LBFGSType
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
			import :: oLBFGS_structure
			class(oLBFGS_structure), intent(inout) :: LBFGSType
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
			import :: oLBFGS_structure
			class(oLBFGS_structure), intent(inout) :: LBFGSType
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
			import :: oLBFGS_structure
			class(oLBFGS_structure), intent(inout) :: LBFGSType
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
			import :: oLBFGS_structure
			class(oLBFGS_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::Value
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(inout)::point
		end subroutine BeforeStepSubroutine_interface
	end interface

	abstract interface
		subroutine EndStepSubroutine_interface(LBFGSType,Value,Gradient,point)
			use tensor_type
			import :: oLBFGS_structure
			class(oLBFGS_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::Value
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(inout)::point
		end subroutine EndStepSubroutine_interface
	end interface

	!***************************************************
	!        definitation of oLBFGSrunner
	!***************************************************

	public::oLBFGSrunner
	type, extends(oLBFGS_structure) :: oLBFGSrunner
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
	!           Basic function for oLBFGS_structure
	!***************************************************

	subroutine LBFGSpointerFunc(p,PointTarget,ith)
		type(Tensor),pointer,intent(inout)::p
		type(Tensor),target,intent(in)::PointTarget(:)
		integer::ith
		p=>PointTarget(ith)
		return
	end subroutine

	subroutine point(BTool,st,yt,ith)
		class(oLBFGS_structure),intent(in)::BTool
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
		class(oLBFGS_structure),intent(inout)::BTool
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
		class(oLBFGS_structure),intent(in)::BTool
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
		class(oLBFGS_structure),intent(inout)::BTool
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
		class(oLBFGS_structure),intent(inout)::BTool
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
		class(oLBFGS_structure),intent(inout)::BTool
		BTool%endindex=0
		BTool%FullFlag=.false.
		return
	end subroutine
	function datalength(BTool)
		integer::datalength
		class(oLBFGS_structure),intent(in)::BTool
		if(BTool%FullFlag)then
			datalength=BTool%length
		else
			datalength=BTool%endindex
		end if
		return
	end function

	function dataSize(BTool)
		integer::dataSize
		class(oLBFGS_structure),intent(in)::BTool
		dataSize=BTool%length
		return
	end function

	subroutine LBFGSsetprintFlag(LBFGSType,printFlag)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		logical,intent(in)::printFlag
		LBFGSType%printFlag=printFlag
		return
	end subroutine

	subroutine set_CG_direction_flag(LBFGSType,Flag)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::Flag
		LBFGSType%CG_direction_Flag=Flag
		return
	end subroutine

	subroutine set_stop_error(LBFGSType,error)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::error
		LBFGSType%stop_error=error
		return
	end subroutine
	subroutine set_max_step_in_Linear_search(LBFGSType,step)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::step
		LBFGSType%max_step_in_Linear_search=step
		return
	end subroutine
	subroutine set_first_step_in_Linear_search(LBFGSType,step)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::step
		LBFGSType%first_step_in_Linear_search=step
		return
	end subroutine
	subroutine set_zero_gradient_in_RGM(LBFGSType,zero_gradient)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::zero_gradient
		LBFGSType%zero_gradient_in_RGM=zero_gradient
		return
	end subroutine


	function check_stop(LBFGSType,gradient)
		logical::check_stop
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::gradient
		real*8::norm
		check_stop=.false.
		if(LBFGSType%stop_error.lt.0) return

		norm=gradient%dnorm2()
		check_stop=norm.le.LBFGSType%stop_error
		return
	end function

	subroutine set_linear_search_type(LBFGSType,flag)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::flag
		if(flag.eq.1)then
			LBFGSType%LinearSearch=>LinearSearch1
			return
		end if
		if(flag.eq.2)then
			LBFGSType%LinearSearch=>LinearSearch2
			return
		end if
		if(flag.eq.3)then
			LBFGSType%LinearSearch=>LinearSearch3
			return
		end if
		call writemess('No such case',-1)
		call error_stop
	end subroutine
	subroutine set_linear_search_function(LBFGSType,Func)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		procedure(LinearSearch_subroutine)::Func
		LBFGSType%LinearSearch=>Func
		return
	end subroutine

	subroutine Set_inStep1Func(LBFGSType,Func)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		procedure(Step1Subroutine_interface)::Func
		LBFGSType%inStep1=>Func
		return
	end subroutine
	subroutine Set_inStep2Func(LBFGSType,Func)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		procedure(Step2Subroutine_interface)::Func
		LBFGSType%inStep2=>Func
		return
	end subroutine
	subroutine Set_inStep3Func(LBFGSType,Func)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		procedure(Step3Subroutine_interface)::Func
		LBFGSType%inStep3=>Func
		return
	end subroutine
	subroutine unSet_inStep1Func(LBFGSType)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		LBFGSType%inStep1=>null()
		return
	end subroutine
	subroutine unSet_inStep2Func(LBFGSType)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		LBFGSType%inStep2=>null()
		return
	end subroutine
	subroutine unSet_inStep3Func(LBFGSType)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		LBFGSType%inStep3=>null()
		return
	end subroutine
	subroutine Set_BeforeStepFunc(LBFGSType,Func)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		procedure(BeforeStepSubroutine_interface)::Func
		LBFGSType%BeforeStep=>Func
		return
	end subroutine
	subroutine unSet_BeforeStepFunc(LBFGSType)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		LBFGSType%BeforeStep=>null()
		return
	end subroutine
	subroutine Set_EndStepFunc(LBFGSType,Func)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		procedure(EndStepSubroutine_interface)::Func
		LBFGSType%EndStep=>Func
		return
	end subroutine
	subroutine unSet_EndStepFunc(LBFGSType)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		LBFGSType%EndStep=>null()
		return
	end subroutine

	!***************************************************
	!           Basic function for LBFGSrunner
	!***************************************************

	subroutine set_target_function(LBFGSer,outSampleFunc,FromSampleFunc)
		class(oLBFGSrunner),intent(inout)::LBFGSer
		procedure(external_target_OutSample_interface)::outSampleFunc
		procedure(external_target_FromSample_interface)::FromSampleFunc
		LBFGSer%externalOutSample=>outSampleFunc
		LBFGSer%externalFromSample=>FromSampleFunc
		return
	end subroutine

	subroutine LBFGSoutSample(A,outVal,outGradient,point,sample)
		class(oLBFGSrunner), intent(inout) :: A
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
		class(oLBFGSrunner), intent(inout) :: A
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
	!            Gradient optimization
	!***************************************************

	subroutine GradientUpdate1(LBFGSType,T0,tau0,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::Sample
		integer::i
		real*8::t
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			t=T0*tau0/(T0+dble(numStep))
			call writemess('max step='+tau0+' , min step='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,Sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			
			point=point-(t*Gradient)
			if(LBFGSType%printFlag)call time_calculator()
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine GradientUpdate2(LBFGSType,t,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor)::Gradient
		type(Tensor)::Sample
		if(LBFGSType%printFlag)then
			call writemess('all step t='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,Sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			point=point-(t*Gradient)
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine GradientUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::NlinearStep,numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor)::Gradient,dir
		type(Tensor)::Sample
		real*8::x
		if(LBFGSType%printFlag)then
			call writemess('NlinearStep='+NlinearStep)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		x=-1d0
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,x)
			call LBFGSType%target_outSample(outValue,Gradient,point,Sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,x)
			if(LBFGSType%check_stop(Gradient))exit
			dir=(-1d0)*Gradient
			call LBFGSType%LinearSearch(NlinearStep,point,dir,x,outValue,Gradient,sample)
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,x)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine GradientUpdate4(LBFGSType,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::NlinearStep
		NlinearStep=2
		call GradientUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		return
	end subroutine

	!***************************************************
	!           random Gradient optimization
	!***************************************************

	subroutine RandomGradient(LBFGSType,inoutd,x)
		class(oLBFGS_structure), intent(in) :: LBFGSType
		type(Tensor),intent(inout)::inoutd
		real*8,intent(in)::x
		integer::i
		real*8,pointer::p(:)
		call inoutd%pointer(p)
		do i=1,inoutd%getTotalData()
			if(abs(p(i)).gt.LBFGSType%zero_gradient_in_RGM)then
				if(p(i).gt.0)then
					p(i)=randomnumber()*x
				else
					p(i)=-1*randomnumber()*x
				end if
			else
				p(i)=0
			end if
		end do
		return
	end subroutine

	subroutine RandomGradientUpdate1(LBFGSType,T0,tau0,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		real*8::t
		type(Tensor)::Gradient
			type(Tensor)::Sample
		if(LBFGSType%printFlag)then
			t=T0*tau0/(T0+dble(numStep))
			call writemess('max step='+tau0+' , min step='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,Sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call RandomGradient(LBFGSType,Gradient,t)
			point=point-Gradient
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
			
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine RandomGradientUpdate2(LBFGSType,t,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor)::Sample
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			call writemess('all step t='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,Sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call RandomGradient(LBFGSType,Gradient,t)
			point=point-Gradient
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine RandomGradientUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::NlinearStep,numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor)::Gradient,dir
		real*8::x
		call writemess('NO such case in random Gradient Update',-1)
		call error_stop
		return
	end subroutine

	subroutine RandomGradientUpdate4(LBFGSType,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		call writemess('NO such case in random Gradient Update',-1)
		call error_stop
		return
	end subroutine

	!***************************************************
	!           Conjugate Gradient optimization
	!***************************************************

	subroutine CG_direction(LBFGSType,dir,Gra,priorgra)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
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
		class(oLBFGS_structure), intent(inout) :: LBFGSType
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

	subroutine CGUpdate1(LBFGSType,T0,tau0,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::d,gra0
		integer::i
		real*8::t
		type(Tensor)::Sample
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			t=T0*tau0/(T0+dble(numStep))
			call writemess('max step='+tau0+' , min step='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call d%empty()
		call gra0%empty()
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,Sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%CG_direction(d,Gradient,gra0)
			point=point+(t*d)
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine CGUpdate2(LBFGSType,t,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::d,gra0
		integer::i
		type(Tensor)::Gradient,Sample
		if(LBFGSType%printFlag)then
			call writemess('all step t='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call d%empty()
		call gra0%empty()
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,Sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%CG_direction(d,Gradient,gra0)
			point=point+(t*d)
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine CGUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::NlinearStep,numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::d,gra0
		integer::i
		real*8::x
		type(Tensor)::Gradient,Sample
		if(LBFGSType%printFlag)then
			call writemess('NlinearStep='+NlinearStep)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call d%empty()
		call gra0%empty()
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		x=-1
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,x)
			call LBFGSType%target_outSample(outValue,Gradient,point,Sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,x)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%CG_direction(d,Gradient,gra0)
			call LBFGSType%LinearSearch(NlinearStep,point,d,x,outValue,Gradient,Sample)
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,x)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine CGUpdate4(LBFGSType,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::NlinearStep
		NlinearStep=2
		call CGUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		return
	end subroutine

	!***************************************************
	!            BFGS optimization
	!***************************************************


	subroutine BFGS_direction(LBFGSType,C,y,s)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
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

	subroutine BFGSUpdate1(LBFGSType,T0,tau0,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::Ct,yt,st
		integer::i
		real*8::t
		type(Tensor)::Gradient,sample
		if(LBFGSType%printFlag)then
			t=T0*tau0/(T0+dble(numStep))
			call writemess('max step='+tau0+' , min step='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call yt%empty()
		call st%empty()
		Ct=eye(Point%getTotalData(),Point%getTotalData(),Point%getclassType())
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%BFGS_direction(Ct,yt,st)
			st=Ct*Gradient*(-1d0*t)
			point=point+st
			call LBFGSType%target_FromSample(outValue,yt,point,sample)
			yt=yt-Gradient
			if(i.eq.1)then
				Ct=((st.dot.yt)/(yt.dot.yt))*Ct
			end if
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine BFGSUpdate2(LBFGSType,t,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::Ct,yt,st
		integer::i
		type(Tensor)::Gradient,sample
		if(LBFGSType%printFlag)then
			call writemess('all step t='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call yt%empty()
		call st%empty()
		Ct=eye(Point%getTotalData(),Point%getTotalData(),Point%getclassType())
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%BFGS_direction(Ct,yt,st)
			st=Ct*Gradient*(-1d0*t)
			point=point+st
			call LBFGSType%target_FromSample(outValue,yt,point,sample)
			yt=yt-Gradient
			if(i.eq.1)then
				Ct=((st.dot.yt)/(yt.dot.yt))*Ct
			end if
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine BFGSUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::NlinearStep,numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::Ct,yt,st,d
		real*8::x
		integer::i
		type(Tensor)::Gradient,sample
		if(LBFGSType%printFlag)then
			call writemess('NlinearStep='+NlinearStep)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call yt%empty()
		call st%empty()
		Ct=eye(Point%getTotalData(),Point%getTotalData(),Point%getclassType())
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		x=-1
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,x)
			call LBFGSType%target_outSample(outValue,Gradient,point,sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,x)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%BFGS_direction(Ct,yt,st)
			yt=Gradient
			st=point
			d=Ct*Gradient*(-1d0)
			call LBFGSType%LinearSearch(NlinearStep,point,d,x,outValue,Gradient,sample)
			st=point-st
			yt=Gradient-yt
			if(i.eq.1)then
				Ct=((st.dot.yt)/(yt.dot.yt))*Ct
			end if
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,x)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine BFGSUpdate4(LBFGSType,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::NlinearStep
		NlinearStep=2
		call BFGSUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		return
	end subroutine

	!***************************************************
	!            LBFGS optimization
	!***************************************************

	! p is \Delta f(x_i)
	! s_i= x_{i+1} - x_i
	! y_i= p_{i+1} - p_i

	subroutine LBFGS_direction(LBFGS,p,inputp)
		class(oLBFGS_structure),intent(inout)::LBFGS
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



	subroutine LBFGSUpdate1(LBFGSType,T0,tau0,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		real*8::t
		type(Tensor),pointer::yt,st
		type(Tensor)::Gradient,tempGradient,direction,sample

		if(LBFGSType%dataSize().le.0)then
			call writemess('ERROR in LBFGS',-1)
			call writemess(' DO NOT allocate memory for running',-1)
			call error_stop
		end if
		if(LBFGSType%printFlag)then
			t=T0*tau0/(T0+dble(numStep))
			call writemess('max step='+tau0+' , min step='+t+' ,num of saving vec='+LBFGSType%dataSize())
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call LBFGSType%resetEndpoint()

		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			tempGradient=Gradient
			call LBFGSType%LBFGS_direction(direction,Gradient)
			call LBFGSType%NewElement(st,yt)
			st=direction*t
			point=point+st
			call LBFGSType%target_FromSample(outValue,Gradient,point,sample)
			yt=Gradient-tempGradient
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine LBFGSUpdate2(LBFGSType,t,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor),pointer::yt,st
		type(Tensor)::Gradient,tempGradient,Direction,sample

		if(LBFGSType%dataSize().le.0)then
			call writemess('ERROR in LBFGS',-1)
			call writemess(' DO NOT allocate memory for running',-1)
			call error_stop
		end if
		if(LBFGSType%printFlag)then
			call writemess('all step t='+t+' ,num of saving vec='+LBFGSType%dataSize())
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call LBFGSType%resetEndpoint()

		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_outSample(outValue,Gradient,point,sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			tempGradient=Gradient
			call LBFGSType%LBFGS_direction(Direction,Gradient)
			call LBFGSType%NewElement(st,yt)
			st=Direction*t
			point=point+st
			call LBFGSType%target_FromSample(outValue,Gradient,point,sample)
			yt=Gradient-tempGradient
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,t)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine


	subroutine LBFGSUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::NlinearStep,numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor),pointer::yt,st
		type(Tensor)::Gradient,Direction,sample
		real*8::x

		if(LBFGSType%dataSize().le.0)then
			call writemess('ERROR in LBFGS',-1)
			call writemess(' DO NOT allocate memory for running',-1)
			call error_stop
		end if
		if(LBFGSType%printFlag)then
			call writemess('NlinearStep='+NlinearStep+' ,num of saving vec='+LBFGSType%dataSize())
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call LBFGSType%resetEndpoint()

		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		x=-1
		do i=1,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,x)
			call LBFGSType%target_outSample(outValue,Gradient,point,sample)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,x)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%LBFGS_direction(Direction,Gradient)
			call LBFGSType%NewElement(st,yt)
			st=point
			yt=Gradient
			call LBFGSType%LinearSearch(NlinearStep,point,Direction,x,outValue,Gradient,sample)
			st=point-st
			yt=Gradient-yt
			if(associated(LBFGSType%inStep3))call LBFGSType%inStep3(outValue,Gradient,point,i,x)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine



	subroutine LBFGSUpdate4(LBFGSType,numStep,Point,outValue)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::NlinearStep
		NlinearStep=2
		call LBFGSUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		return
	end subroutine


	!*************************************************************
	!*************************************************************
	!                 linear search
	!*************************************************************

	!linear search NOTE:
		!  Linear search: f(x)=f(P+x*d) is a function of x
		!                 g(x)= vec{g(P+x*d)} \cdot \vec{d}
		!                 g(P+x*d) is a vector but  g(x) not
		!
		! 1. input x0, it is a random number at the first time
		! 2. input         f(P)    --->  f(0)
		!                  g(P)    --->  g(0)
		! 3. calculate   f(P+x0*d) --->  f(x0)
		! 4. calculate g(P+x0*d)*d --->  g(x0)
		! 5. suppose f(x)=a*x^3+b*x^2+c*x+d ,use x=0,f(0),g(0),x0,f(x0),g(x0)
		!   solve a,b,c,d
		! 6. When we have a,b,c,d min f(x)=a*x^3+b*x^2+c*x+d--->output x1
		!   1). f'(x)=3*a*x^2+2*b*x+c=0 ==> x1
		!   2). if Delta= (2*b)^2-4*(3*a)*c <0, no root of f'(x)=0,
		!      use quadratic interpolation
		!   3).Quadratic interpolation: 
		!      suppose g(x)=kx+b, use x=0,g(0),x0,g(x0) solve k and b
		!      x1=-b/k
		! 7. calculate   f(P+x1*d) --->  f(x1)
		! 8. calculate g(P+x1*d)*d --->  g(x1)
		! 9. Now there are 3 piont
		!     xa =  0     xb =  x0     xc =  x1
		!     fa = f(0)   fb = f(x0)   fc = f(x1)
		!     ga = g(0)   gb = g(x0)   gb = g(x1)
		!   1).if ga*gb>0, use Quadratic interpolation, output x2
		!    (1).suppose g(x)=kx+b, use xa,ga,xb,gb solve k and b
		!      x2=-b/k
		!    (2). calculate   f(P+x2*d) --->  f(x2)
		!    (3). calculate g(P+x2*d)*d --->  g(x2)
		!    (4). if fa*fc<0 keep fa ,drop fb
		!            xb=xc,fb=fc,gb=gc
		!            xc=x2,fc=f2,gc=g2
		!        else keep fb ,drop fa
		!           xa=xc,fa=fc,ga=gc
		!            xc=x2,fc=f2,gc=g2
		!          if all point on the sameside,drop xb   
		!    (5).  go to 9.
		!     
		!   2). if ga*gb<0, use cubic interpolation
		!     (1). suppose g(x)=a*x^2+b*x+c and f(x)=\int_0^x g(t)dt=a/3*x^3+b/2*x^2+c*x
		!     (2). xa,ga,xb,gb,xc,gc to solve a,b,c in g(x)=a*x^2+b*x+c
		!     (3). g(x2)=0 find two x2, choose the one that min f(x) as output x2
		!     (4). calculate   f(P+x2*d) --->  f(x2)
		!     (5). calculate g(P+x2*d)*d --->  g(x2)
		!     (6). if fa*fc<0 keep fa ,drop fb
		!            xb=xc,fb=fc,gb=gc
		!            xc=x2,fc=f2,gc=g2
		!        else keep fb ,drop fa
		!           xa=xc,fa=fc,ga=gc
		!            xc=x2,fc=f2,gc=g2
		!       if all point on the sameside,drop xb   
		!     (7). go to 9.
		! 10. repete 9 about LinearSearch_max_running time, output x
		! 11. new P is P+x*d, x will be input x0 in next Linear search circle
		!*************************************************************
		!*************************************************************

	!solvefx:
		!f(x)=ax^3+bx^2+cx+d
		!g(x)=3ax^2+2bx+c
		!input f(0),g(0),f(x1),g(x1),x1==>output a,b,c,d

	subroutine solvefx(a,b,c,d,f0,g0,f1,g1,x1)!use in LinearSearch_third_point
		real*8,intent(inout)::a,b,c,d
		real*8,intent(in)::f0,g0,f1,g1,x1
		real*8::x1_3,temp
		x1_3=x1*x1*x1
		c=g0
		d=f0
		a= ((g1+c)*x1-2d0*(f1-d))/x1_3
		b= (g1-c)/(2d0*x1)-1.5d0*x1*a
		return
	end subroutine

	!zerofx
		!f(x)=ax^3+bx^2+cx+d
		!input a, b, c, d ,find minf(x)
		!outx should be larger than 0

	subroutine zerofx(outx,a,b,c,d,Delta)!use in LinearSearch_third_point
		real*8,intent(in)::a,b,c,d
		real*8,intent(inout)::outx,Delta
		real*8::x1,x2,f1,f2
		!Delta=4d0*b*b-12*a*c
		if(Delta.lt.zero_number) then
			write(*,*)"ERROR, should call zerolinearfx"
			stop
		end if
		if(dabs(a).le.zero_number)then!f(x)=bx^2+cx+d
			if(dabs(b).le.zero_number) then !f(x)=cx+d
				write(*,*)"ERROR in zerofx"
				stop
			end if
			outx=-c/(2d0*b)
			return
		end if
		Delta=dsqrt(Delta)
		x1=(-2d0*b+Delta)/(6d0*a)
		x2=(-2d0*b-Delta)/(6d0*a)
		f1=a*x1*x1*x1+b*x1*x1+c*x1+d
		f2=a*x2*x2*x2+b*x2*x2+c*x2+d
		if(f1.le.f2)then
			outx=x1
			if(outx.le.0) outx=x2
		else
			outx=x2
			if(outx.le.0) outx=x1
		end if
		return
	end subroutine

	!LinearSearch_third_point:
		!input two point
		!x0=0,f0,g0,x1,f1,g1
		!find the next point
		!suppose f(x)=ax^3+bx^2+cx+d,then g(x)=3ax^2+2bx+c
		!input x0=0,f0,g0,x1,f1,g1 can determin a,b,c,d
		!Then find xmin,g(xmin)=0
		!If there is no root in the equation g(x)=0
		!Then regard (0,g0) and (x1,g1) as two point on a line g(x)=kx+b
		!Then find xmin,g(xmin)=0

	subroutine LinearSearch_third_point(outx,f0,g0,f1,g1,x1)
		real*8,intent(inout)::outx
		real*8,intent(in)::f0,g0,f1,g1,x1
		real*8::Delta,a,b,c,d
		call solvefx(a,b,c,d,f0,g0,f1,g1,x1)
		Delta=4d0*b*b-12*a*c
		if(Delta.lt.zero_number) then
			call zerolinearfx(outx,0d0,g0,x1,g1)
			return
		end if
		call zerofx(outx,a,b,c,d,Delta)
		return
	end subroutine

	!zerolinearfx:
		!g(x)=kx+b
		!input x1,g1,x2,g2
		!g(x0)=0,output x0

	subroutine zerolinearfx(outx,x1,g1,x2,g2,stopflag)
		real*8,intent(inout)::outx
		real*8,intent(in)::x1,g1,x2,g2
		logical,optional,intent(inout)::stopflag
		real*8::k,b
		k=(g1-g2)/(x1-x2)
		if(dabs(k).le.zero_number)then
			if(present(stopflag)) then
				stopflag=.true.
				return
			else
				write(*,*)"ERROR in zerolinearfx,k=0"
				write(*,*)x1,g1
				write(*,*)x2,g2
				stop
			end if
		end if
		b=g1- (x1*k)
		outx=-b/k
		return
	end subroutine

	!LinearSearch_fouth_point:
		!on output,fc,gc are useless
		!on input, outx and xc can be the same variable,but other cannot
		!
		!if (ga*gb) >0
		! 
		! use b and c ==> d .because c may be obtain from a,b, if use a and b to get d,  c and d may be the same,go wrong
		! a,b,c keep two point
		!   if(ga*gc<0) keep a and c
		!   if(gb*gc<0) keep b and c
		!   ga * gb is larger than 0,so other case is ga ,gb ,gc are the same sign
		!     in this case ,drop b, keep a ,c
		!If ga,gb,gc are the same sign, use Quadratic interpolation,otherwhile use cubic interpolation

	subroutine LinearSearch_fouth_point(outx,stopflag,xa,fa,ga,xb,fb,gb,xc_,fc,gc,max_x)
		real*8,intent(inout)::outx,xa,fa,ga,xb,fb,gb,max_x
		logical,intent(inout)::stopflag
		real*8,intent(in)::xc_,fc,gc
		real*8::xc
		real*8::a,b,c,Delta
		logical::Quadraticflag
		!Quadraticflag,Quadratic interpolation
		xc=xc_
		if(ga*gb.ge.0d0) then
			call zerolinearfx(outx,xa,ga,xc,gc,stopflag)
			if(stopflag.or.(outx.le.0d0))then
			 	call choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
			 	stopflag=.true.
			 	return
			end if
			fb=fc
			gb=gc
			xb=xc
			return
		end if
		call solvefx2(a,b,c,xa,ga,xb,gb,xc,gc)
		if(dabs(a).le.zero_number)then!g(x)=bx+c
			if(dabs(b).le.zero_number) then
				call choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
			else
				outx=-c/b
			end if
			stopflag=.true.
			if(outx.le.0d0) call choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
			return
		end if
		Delta=b*b-4.*a*c!g(x)=ax^2+bx+c
		if( (Delta.ge.0) .and.(Delta.lt.InfinityNumber)) then
			call zerolinearfx2(outx,a,b,c,Delta)
			if(outx.le.0d0) then
				stopflag=.true.
				call choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
			end if
			if(ga*gc.gt.0d0) then
				ga=gc
				fa=fc
				xa=xc
			end if
			if(gb*gc.gt.0d0) then
				gb=gc
				fb=fc
				xb=xc
			end if
		else! if the g is too small or x are too close,Delta=NAM
			stopflag=.true.
			call choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
		end if
		return
	end subroutine

	!choose_fx:
		!outx should be larger than 0	

	subroutine choose_fx(outx,xa,xb,xc,fa,fb,fc,max_x)
		real*8,intent(in)::xa,xb,xc,fa,fb,fc,max_x
		real*8,intent(out)::outx
		real*8::f
		outx=xa
		f=fa
		if(f.ge.fb) then	
			if(xb.ge.0) then
			 outx=xb
			 f=xb
			end if
		end if
		if(f.ge.fc) then	
			if(xc.ge.0) then
			 outx=xc
			 f=xc
			end if
		end if
		if(outx.le.0d0)then
			outx=max_x
		end if
		return
	end subroutine

	!solvefx2
		!g(x)=ax^2+bx+c
		!output a,b,c

	subroutine solvefx2(a,b,c,x1,g1,x2,g2,x3,g3)
		real*8,intent(inout)::a,b,c
		real*8,intent(in)::g1,x1,g2,x2,g3,x3
		real*8::temp1,x1_2,x2_2,x3_2
		x1_2=x1*x1
		x2_2=x2*x2
		x3_2=x3*x3
		temp1= (x2_2-x1_2)/(x3_2-x1_2)
		b=(temp1*(g3-g1)-g2+g1)/(temp1*(x3-x1)-x2+x1)
		temp1= (x2-x1)/(x3-x1)
		a=(temp1*(g3-g1)-g2+g1)/(temp1*(x3_2-x1_2)-x2_2+x1_2)
		c=g3-x3_2*a-x3*b
		return
	end subroutine

	!zerolinearfx2
		!g(x)=ax^2+bx+c ==>f(x)=int_0^x g(t)dt
		!output minf(x) and outx should be larger than 0

	subroutine zerolinearfx2(outx,a,b,c,Delta)
		real*8,intent(in)::a,b,c
		real*8,intent(out)::outx,Delta
		real*8::x1,x2,f1,f2
		Delta=dsqrt(Delta)
		if(dabs(a).le.zero_number)then!g(x)=bx+c
			write(*,*)"ERROR in zerolinearfx2"
			stop
		end if
		x1=(-b+Delta)/(2d0*a)
		x2=(-b-Delta)/(2d0*a)
		f1=(a*x1*x1*x1/3d0)+(b*x1*x1*0.5d0)+c*x1
		f2=(a*x2*x2*x2/3d0)+(b*x2*x2*0.5d0)+c*x2
		if(f1.le.f2)then
			outx=x1
			if(outx.le.0) outx=x2
		else
			outx=x2
			if(outx.le.0) outx=x1
		end if
		return
	end subroutine

	subroutine LinearSearch1(LBFGSType,max_running,point,dir,x,outValue,inoutgra,sample)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
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

		if((dabs(x).le.zero_number).or.(x.le.0)) then
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


		if(dabs(g2).le.gradient_zero_number)then
			Point=NewPoint
			x=x2
			outValue=f2
			inoutgra=gradient
			return
		end if
		call LinearSearch_third_point(x3,f1,g1,f2,g2,x2)
		NewPoint=point+(x3*dir)
		call LBFGSType%target_FromSample(f3,gradient,NewPoint,sample)
		g3=gradient.dot.dir

		if(dabs(g3).le.gradient_zero_number)then
			Point=NewPoint
			x=x3
			outValue=f3
			inoutgra=gradient
			return
		end if
		if(max_running.eq.2)then
			outx=x3
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
			if((dabs(g3).le.gradient_zero_number).or. stopflag) exit
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

	!linear search NOTE:
		!  Linear search: f(x)=f(P+x*d) is a function of x
		!                 g(x)= vec{g(P+x*d)} \cdot \vec{d}
		!                 g(P+x*d) is a vector but  g(x) not
		!
		! 1. input x0, it is a random number at the first time
		!                  g(P)    --->  g(0)
		! 2. calculate g(P+x0*d)*d --->  g(x0)
		! 3. support g(x)=k*x+b
		!      input   x1, g1=g(x1), x2, g2=g(x2)
		!       --> k=(g1-g2)/(x1-x2)
		!           b=g1- k*x1
		!      the solve k*x+b=0
		!       --> x=-b/k
		!    function of zerolinearfx do this job!
		! 4. 0,x1 ==>x2, choose x3 and x4 with small value g(x)
		!

	subroutine FindxWithMinGx(x1,g1,x2,g2,stopflag)
		real*8,intent(inout)::x1,g1,x2,g2
		logical,optional,intent(inout)::stopflag
		real*8::outx
		call zerolinearfx(outx,x1,g1,x2,g2,stopflag)
		x1=x2
		g1=g2
		x2=outx
		return
	end subroutine


	subroutine LinearSearch2(LBFGSType,max_running,point,dir,x,outValue,inoutgra,sample)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(inout)::x,outValue
		type(Tensor),intent(inout)::point
		type(Tensor),intent(inout)::inoutgra
		type(Tensor),intent(in)::dir,sample
		integer,intent(in)::max_running
		logical::stopflag
		real*8::x1,x2
		real*8::f1,f2,g1,g2
		type(Tensor)::gradient,NewPoint
		integer::i
		if(max_running.lt.1)then
			call writemess('ERROR in LinearSearch2,input max_running should >=1',-1)
			call error_stop
		end if
		x1=0d0
		g1=inoutgra.dot.dir

		if((dabs(x).le.zero_number).or.(x.le.0)) then
			x2=LBFGSType%first_step_in_Linear_search
			x=x2
		else 
			x2=x
		end if

		stopflag=.false.
		do i=1,max_running
			NewPoint=point+(x2*dir)
			call LBFGSType%target_FromSample(f2,gradient,NewPoint,sample)
			g2=gradient.dot.dir
			call FindxWithMinGx(x1,g1,x2,g2,stopflag)
			if(x2.le.0)x2=-0.1*x2
			if(isnan(x2))then
				write(*,*)"ERROR in LinearSearch"
				write(*,*)"x1,x2"
				write(*,*)x1
				write(*,*)x2
				stop
			end if
			if((dabs(g2).le.gradient_zero_number).or. stopflag) exit
		end do
		Point=NewPoint
		x=x2
		outValue=f2
		inoutgra=gradient
		return
	end subroutine

	!*************************************************************
	!*************************************************************
	!                 linear search2
	!*************************************************************

	!LinearSearch3
		!Always output tht min f

	subroutine LinearSearch3(LBFGSType,max_running,point,dir,x,outValue,inoutgra,sample)
		class(oLBFGS_structure), intent(inout) :: LBFGSType
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
		type(Tensor)::Savepoint,Savegradient,minpoint,mingradient
		real*8::savex,saveValue,minx,minvalue
		logical::max_step_flag

		if(max_running.lt.2)then
			call writemess('ERROR in LinearSearch,input max_running should >=2',-1)
			call error_stop
		end if
		max_step_flag=LBFGSType%max_step_in_Linear_search.gt.0
		x1=0d0
		g1=inoutgra.dot.dir

		if((dabs(x).le.zero_number).or.(x.le.0)) then
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
		minpoint=NewPoint
		mingradient=gradient
		minx=x2
		minValue=f2

		if(dabs(g2).le.gradient_zero_number)then
			Point=NewPoint
			x=x2
			outValue=f2
			inoutgra=gradient
			return
		end if
		call LinearSearch_third_point(x3,f1,g1,f2,g2,x2)
		NewPoint=point+(x3*dir)
		call LBFGSType%target_FromSample(f3,gradient,NewPoint,sample)
		g3=gradient.dot.dir

		if(dabs(g3).le.gradient_zero_number)then
			Point=NewPoint
			x=x3
			outValue=f3
			inoutgra=gradient
			return
		end if
		if(max_running.eq.2)then
			Point=NewPoint
			x=outx
			outValue=f3
			inoutgra=gradient
			return
		end if
		if(f3.le.minValue)then
			minpoint=NewPoint
			mingradient=gradient
			minx=x3
			minValue=f3
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
			if((dabs(g3).le.gradient_zero_number).or. stopflag) exit
			if(f3.le.minValue)then
				minpoint=NewPoint
				mingradient=gradient
				minx=outx
				minValue=f3
			end if
		end do
		if(max_step_flag.and.(minx.gt.LBFGSType%max_step_in_Linear_search))then
			Point=Savepoint
			x=Savex
			outValue=SaveValue
			inoutgra=Savegradient
		else
			Point=minpoint
			x=minx
			outValue=minValue
			inoutgra=mingradient
		end if
		return
	end subroutine


end module
