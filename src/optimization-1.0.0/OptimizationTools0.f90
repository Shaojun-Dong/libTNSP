module  Optimization_Tools
	use tensor_type
	use LinearSearchTools
	use Tools
	implicit none
	private
	integer,private,parameter::write_time_num=20
	real*8,private,parameter::zero_number=1d-16
	real*8,private,parameter::gradient_zero_number=5d-8!To small will error
	real*8,private,parameter::InfinityNumber=1d100

	!***************************************************
	!       abstract  definitation of LBFGS_structure
	!***************************************************

	public::LBFGS_structure
	type, abstract :: LBFGS_structure
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
		procedure(LinearSearch_subroutine),pointer::LinearSearch=>LinearSearch1
		procedure(Step1Subroutine_interface),pointer::inStep1=>null()
		procedure(Step2Subroutine_interface),pointer::inStep2=>null()
		procedure(BeforeStepSubroutine_interface),pointer::BeforeStep=>null()
		procedure(EndStepSubroutine_interface),pointer::EndStep=>null()
	contains
		procedure(targetFunc), deferred :: target_Function
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
		procedure,public::Set_beforeStepFunc
		procedure,public::Set_EndStepFunc
		procedure,public::unSet_inStep1Func
		procedure,public::unSet_inStep2Func
		procedure,public::unSet_beforeStepFunc
		procedure,public::unSet_EndStepFunc
	end type LBFGS_structure

	abstract interface
		subroutine targetFunc(A,outVal,outGradient,point)
			use tensor_type
			import :: LBFGS_structure
			class(LBFGS_structure), intent(inout) :: A
			real*8,intent(inout)::outVal
			type(Tensor),intent(inout)::outGradient
			type(Tensor),intent(in)::point
		end subroutine targetFunc
	end interface

	abstract interface
		subroutine LinearSearch_subroutine(LBFGSType,max_running,point,dir,x,outValue,inoutgra)
			use tensor_type
			import :: LBFGS_structure
			class(LBFGS_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::x,outValue
			type(Tensor),intent(inout)::point
			type(Tensor),intent(inout)::inoutgra
			type(Tensor),intent(in)::dir
			integer,intent(in)::max_running
		end subroutine LinearSearch_subroutine
	end interface

	abstract interface
		subroutine Step1Subroutine_interface(LBFGSType,Value,Gradient,point,ith,t)
			use tensor_type
			import :: LBFGS_structure
			class(LBFGS_structure), intent(inout) :: LBFGSType
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
			import :: LBFGS_structure
			class(LBFGS_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::Value
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(inout)::point
			integer,intent(in)::ith
			real*8::t
		end subroutine Step2Subroutine_interface
	end interface

	abstract interface
		subroutine BeforeStepSubroutine_interface(LBFGSType,Value,Gradient,point)
			use tensor_type
			import :: LBFGS_structure
			class(LBFGS_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::Value
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(inout)::point
		end subroutine BeforeStepSubroutine_interface
	end interface

	abstract interface
		subroutine EndStepSubroutine_interface(LBFGSType,Value,Gradient,point)
			use tensor_type
			import :: LBFGS_structure
			class(LBFGS_structure), intent(inout) :: LBFGSType
			real*8,intent(inout)::Value
			type(Tensor),intent(inout)::Gradient
			type(Tensor),intent(inout)::point
		end subroutine EndStepSubroutine_interface
	end interface

	!***************************************************
	!        definitation of LBFGSrunner
	!***************************************************

	public::LBFGSrunner
	type, extends(LBFGS_structure) :: LBFGSrunner
		procedure(external_target_Function_interface),pointer,NOPASS,private::externalFunc=>null()
	contains
		procedure::target_Function=>LBFGSFunc
		procedure,public::set_target_function
	end type

	interface
		subroutine external_target_Function_interface(outVal,outGradient,point)
			use tensor_type
			real*8,intent(inout)::outVal
			type(Tensor),intent(inout)::outGradient
			type(Tensor),intent(in)::point
		end subroutine external_target_Function_interface
	end interface

contains

	!***************************************************
	!           Basic function for LBFGS_structure
	!***************************************************

	subroutine LBFGSpointerFunc(p,PointTarget,ith)
		type(Tensor),pointer,intent(inout)::p
		type(Tensor),target,intent(in)::PointTarget(:)
		integer::ith
		p=>PointTarget(ith)
		return
	end subroutine

	subroutine point(BTool,st,yt,ith)
		class(LBFGS_structure),intent(in)::BTool
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
		class(LBFGS_structure),intent(inout)::BTool
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
		class(LBFGS_structure),intent(in)::BTool
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
		class(LBFGS_structure),intent(inout)::BTool
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
		class(LBFGS_structure),intent(inout)::BTool
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
		class(LBFGS_structure),intent(inout)::BTool
		BTool%endindex=0
		BTool%FullFlag=.false.
		return
	end subroutine
	function datalength(BTool)
		integer::datalength
		class(LBFGS_structure),intent(in)::BTool
		if(BTool%FullFlag)then
			datalength=BTool%length
		else
			datalength=BTool%endindex
		end if
		return
	end function

	function dataSize(BTool)
		integer::dataSize
		class(LBFGS_structure),intent(in)::BTool
		dataSize=BTool%length
		return
	end function

	subroutine LBFGSsetprintFlag(LBFGSType,printFlag)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		logical,intent(in)::printFlag
		LBFGSType%printFlag=printFlag
		return
	end subroutine

	subroutine set_CG_direction_flag(LBFGSType,Flag)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::Flag
		LBFGSType%CG_direction_Flag=Flag
		return
	end subroutine

	subroutine set_stop_error(LBFGSType,error)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::error
		LBFGSType%stop_error=error
		return
	end subroutine
	subroutine set_max_step_in_Linear_search(LBFGSType,step)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::step
		LBFGSType%max_step_in_Linear_search=step
		return
	end subroutine
	subroutine set_first_step_in_Linear_search(LBFGSType,step)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::step
		LBFGSType%first_step_in_Linear_search=step
		return
	end subroutine
	subroutine set_zero_gradient_in_RGM(LBFGSType,zero_gradient)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::zero_gradient
		LBFGSType%zero_gradient_in_RGM=zero_gradient
		return
	end subroutine

	function check_stop(LBFGSType,gradient)
		logical::check_stop
		class(LBFGS_structure), intent(inout) :: LBFGSType
		type(Tensor),intent(in)::gradient
		real*8::norm
		check_stop=.false.
		if(LBFGSType%stop_error.lt.0) return

		norm=gradient%dnorm2()
		check_stop=norm.le.LBFGSType%stop_error
		return
	end function

	subroutine set_linear_search_type(LBFGSType,flag)
		class(LBFGS_structure), intent(inout) :: LBFGSType
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
		class(LBFGS_structure), intent(inout) :: LBFGSType
		procedure(LinearSearch_subroutine)::Func
		LBFGSType%LinearSearch=>Func
		return
	end subroutine

	subroutine Set_inStep1Func(LBFGSType,Func)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		procedure(Step1Subroutine_interface)::Func
		LBFGSType%inStep1=>Func
		return
	end subroutine
	subroutine Set_inStep2Func(LBFGSType,Func)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		procedure(Step2Subroutine_interface)::Func
		LBFGSType%inStep2=>Func
		return
	end subroutine
	subroutine unSet_inStep1Func(LBFGSType)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		LBFGSType%inStep1=>null()
		return
	end subroutine
	subroutine unSet_inStep2Func(LBFGSType)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		LBFGSType%inStep2=>null()
		return
	end subroutine
	subroutine Set_BeforeStepFunc(LBFGSType,Func)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		procedure(BeforeStepSubroutine_interface)::Func
		LBFGSType%BeforeStep=>Func
		return
	end subroutine
	subroutine unSet_BeforeStepFunc(LBFGSType)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		LBFGSType%BeforeStep=>null()
		return
	end subroutine
	subroutine Set_EndStepFunc(LBFGSType,Func)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		procedure(EndStepSubroutine_interface)::Func
		LBFGSType%EndStep=>Func
		return
	end subroutine
	subroutine unSet_EndStepFunc(LBFGSType)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		LBFGSType%EndStep=>null()
		return
	end subroutine

	!***************************************************
	!           Basic function for LBFGSrunner
	!***************************************************

	subroutine set_target_function(LBFGSer,Func)
		class(LBFGSrunner),intent(inout)::LBFGSer
		procedure(external_target_Function_interface)::Func
		LBFGSer%externalFunc=>Func
		return
	end subroutine

	subroutine LBFGSFunc(A,outVal,outGradient,point)
		class(LBFGSrunner), intent(inout) :: A
		real*8,intent(inout)::outVal
		type(Tensor),intent(inout)::outGradient
		type(Tensor),intent(in)::point
		if(associated(A%externalFunc))then
			call A%externalFunc(outVal,outGradient,point)
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
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		real*8::t
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			t=T0*tau0/(T0+dble(numStep))
			call writemess('max step='+tau0+' , min step='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=0,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_Function(outValue,Gradient,point)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			
			point=point-(t*Gradient)
			if(LBFGSType%printFlag)call time_calculator()
			
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine GradientUpdate2(LBFGSType,t,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			call writemess('all step t='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=0,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_Function(outValue,Gradient,point)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit

			point=point-(t*Gradient)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine GradientUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::NlinearStep,numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor)::Gradient,dir
		real*8::x
		if(LBFGSType%printFlag)then
			call writemess('NlinearStep='+NlinearStep)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		x=-1d0
		do i=0,numStep
			if(i.eq.0)call LBFGSType%target_Function(outValue,Gradient,point)
			dir=(-1d0)*Gradient
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,x)
			call LBFGSType%LinearSearch(NlinearStep,point,dir,x,outValue,Gradient)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,x)
			if(LBFGSType%check_stop(Gradient))exit
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine GradientUpdate4(LBFGSType,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
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
		class(LBFGS_structure), intent(in) :: LBFGSType
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
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		real*8::t
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			t=T0*tau0/(T0+dble(numStep))
			call writemess('max step='+tau0+' , min step='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=0,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_Function(outValue,Gradient,point)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call RandomGradient(LBFGSType,Gradient,t)
			point=point-Gradient
			if(LBFGSType%printFlag)call time_calculator()
			
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine RandomGradientUpdate2(LBFGSType,t,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			call writemess('all step t='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=0,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_Function(outValue,Gradient,point)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call RandomGradient(LBFGSType,Gradient,t)
			point=point-Gradient
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine RandomGradientUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
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
		class(LBFGS_structure), intent(inout) :: LBFGSType
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
		class(LBFGS_structure), intent(inout) :: LBFGSType
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
		class(LBFGS_structure), intent(inout) :: LBFGSType
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
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::d,gra0
		integer::i
		real*8::t
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			t=T0*tau0/(T0+dble(numStep))
			call writemess('max step='+tau0+' , min step='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call d%empty()
		call gra0%empty()
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=0,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_Function(outValue,Gradient,point)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%CG_direction(d,Gradient,gra0)
			point=point+(t*d)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine CGUpdate2(LBFGSType,t,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::d,gra0
		integer::i
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			call writemess('all step t='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call d%empty()
		call gra0%empty()
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=0,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_Function(outValue,Gradient,point)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%CG_direction(d,Gradient,gra0)
			point=point+(t*d)
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine CGUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::NlinearStep,numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::d,gra0
		integer::i
		real*8::x
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			call writemess('NlinearStep='+NlinearStep)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call d%empty()
		call gra0%empty()
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		x=-1
		do i=0,numStep
			if(i.eq.0)call LBFGSType%target_Function(outValue,Gradient,point)
			call LBFGSType%CG_direction(d,Gradient,gra0)
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,x)
			call LBFGSType%LinearSearch(NlinearStep,point,d,x,outValue,Gradient)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,x)
			if(LBFGSType%check_stop(Gradient))exit
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine
	subroutine CGUpdate4(LBFGSType,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
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
		class(LBFGS_structure), intent(inout) :: LBFGSType
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
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::Ct,yt,st
		integer::i
		real*8::t
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			t=T0*tau0/(T0+dble(numStep))
			call writemess('max step='+tau0+' , min step='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call yt%empty()
		call st%empty()
		Ct=eye(Point%getTotalData(),Point%getTotalData(),Point%getclassType())
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=0,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_Function(outValue,Gradient,point)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%BFGS_direction(Ct,yt,st)
			st=Ct*Gradient*(-1d0*t)
			point=point+st
			call LBFGSType%target_Function(outValue,yt,point)
			yt=yt-Gradient
			if(LBFGSType%printFlag)call time_calculator()
			if(i.eq.0)then
				Ct=((st.dot.yt)/(yt.dot.yt))*Ct
			end if
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine BFGSUpdate2(LBFGSType,t,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::Ct,yt,st
		integer::i
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			call writemess('all step t='+t)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call yt%empty()
		call st%empty()
		Ct=eye(Point%getTotalData(),Point%getTotalData(),Point%getclassType())
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		do i=0,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_Function(outValue,Gradient,point)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			call LBFGSType%BFGS_direction(Ct,yt,st)
			st=Ct*Gradient*(-1d0*t)
			point=point+st
			call LBFGSType%target_Function(outValue,yt,point)
			yt=yt-Gradient
			if(LBFGSType%printFlag)call time_calculator()
			if(i.eq.0)then
				Ct=((st.dot.yt)/(yt.dot.yt))*Ct
			end if
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine BFGSUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::NlinearStep,numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		type(Tensor)::Ct,yt,st,d
		real*8::x
		integer::i
		type(Tensor)::Gradient
		if(LBFGSType%printFlag)then
			call writemess('NlinearStep='+NlinearStep)
			call reset_time_calculator(numStep,write_time_num) 
		end if
		call yt%empty()
		call st%empty()
		Ct=eye(Point%getTotalData(),Point%getTotalData(),Point%getclassType())
		if(associated(LBFGSType%BeforeStep))call LBFGSType%BeforeStep(outValue,Gradient,point)
		x=-1
		do i=0,numStep
			if(i.eq.0)call LBFGSType%target_Function(outValue,Gradient,point)
			call LBFGSType%BFGS_direction(Ct,yt,st)
			yt=Gradient
			st=point
			d=Ct*Gradient*(-1d0)
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,x)
			call LBFGSType%LinearSearch(NlinearStep,point,d/d%dnorm(),x,outValue,Gradient)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,x)
			if(LBFGSType%check_stop(Gradient))exit
			st=point-st
			yt=Gradient-yt
			if(LBFGSType%printFlag)call time_calculator()
			if(i.eq.0)then
				Ct=((st.dot.yt)/(yt.dot.yt))*Ct
			end if
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine BFGSUpdate4(LBFGSType,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
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
		class(LBFGS_structure),intent(inout)::LBFGS
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
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::T0,tau0
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		real*8::t
		type(Tensor),pointer::yt,st
		type(Tensor)::Gradient,tempGradient,direction

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
		do i=0,numStep
			t=T0*tau0/(T0+dble(i))
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_Function(outValue,Gradient,point)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			tempGradient=Gradient
			call LBFGSType%LBFGS_direction(direction,Gradient)
			call LBFGSType%NewElement(st,yt)
			st=direction*t
			point=point+st
			call LBFGSType%target_Function(outValue,Gradient,point)
			yt=Gradient-tempGradient
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine

	subroutine LBFGSUpdate2(LBFGSType,t,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(in)::t
		integer,intent(in)::numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor),pointer::yt,st
		type(Tensor)::Gradient,tempGradient,Direction

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
		do i=0,numStep
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,t)
			call LBFGSType%target_Function(outValue,Gradient,point)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,t)
			if(LBFGSType%check_stop(Gradient))exit
			tempGradient=Gradient
			call LBFGSType%LBFGS_direction(Direction,Gradient)
			call LBFGSType%NewElement(st,yt)
			st=Direction*t
			point=point+st
			call LBFGSType%target_Function(outValue,Gradient,point)
			yt=Gradient-tempGradient
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine


	subroutine LBFGSUpdate3(LBFGSType,NlinearStep,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		integer,intent(in)::NlinearStep,numStep
		type(Tensor),intent(inout)::Point
		real*8,intent(inout)::outValue
		integer::i
		type(Tensor),pointer::yt,st
		type(Tensor)::Gradient,Direction
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
		do i=0,numStep
			if(i.eq.0)call LBFGSType%target_Function(outValue,Gradient,point)
			call LBFGSType%LBFGS_direction(Direction,Gradient)
			call LBFGSType%NewElement(st,yt)
			st=point
			yt=Gradient
			if(associated(LBFGSType%inStep1))call LBFGSType%inStep1(outValue,Gradient,point,i,x)
			call LBFGSType%LinearSearch(NlinearStep,point,Direction/Direction%dnorm(),x,outValue,Gradient)
			if(associated(LBFGSType%inStep2))call LBFGSType%inStep2(outValue,Gradient,point,i,x)
			if(LBFGSType%check_stop(Gradient))exit
			st=point-st
			yt=Gradient-yt
			if(LBFGSType%printFlag)call time_calculator()
		end do
		if(associated(LBFGSType%EndStep))call LBFGSType%EndStep(outValue,Gradient,point)
		return
	end subroutine



	subroutine LBFGSUpdate4(LBFGSType,numStep,Point,outValue)
		class(LBFGS_structure), intent(inout) :: LBFGSType
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
	!                 linear search2
	!*************************************************************

	subroutine LinearSearch1(LBFGSType,max_running,point,dir,x,outValue,inoutgra)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(inout)::x,outValue
		type(Tensor),intent(inout)::point
		type(Tensor),intent(inout)::inoutgra
		type(Tensor),intent(in)::dir
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

		if((dabs(x).le.zero_number).or.(x.le.0)) then
			x2=LBFGSType%first_step_in_Linear_search
			x=x2
		else 
			x2=x
		end if

		NewPoint=point+(x2*dir)
		call LBFGSType%target_Function(f2,gradient,NewPoint)
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
		call LBFGSType%target_Function(f3,gradient,NewPoint)
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
			call LBFGSType%target_Function(f3,gradient,NewPoint)
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

	!LinearSearch2
		!use the least square method 

	subroutine LinearSearch2(LBFGSType,max_running,point,dir,x,outValue,inoutgra)
		class(LBFGS_structure), intent(inout) :: LBFGSType
		real*8,intent(inout)::x,outValue
		type(Tensor),intent(inout)::point
		type(Tensor),intent(inout)::inoutgra
		type(Tensor),intent(in)::dir
		integer,intent(in)::max_running
		type(Tensor)::Savepoint,Savegradient,minpoint,mingradient
		real*8::savex,saveValue,minx,minvalue,outx,outf
		logical::max_step_flag
		real*8,allocatable::Allx(:),Allf(:),Allg(:)
		integer::Datalength,i,ith
		type(Tensor)::NewPoint,gradient
		if(max_running.lt.1)then
			call writemess('ERROR in LinearSearch2,input max_running should >=1',-1)
			call error_stop
		end if
		Datalength=max_running+1
		allocate(Allx(Datalength))
		allocate(Allf(Datalength))
		allocate(Allg(Datalength))
		max_step_flag=LBFGSType%max_step_in_Linear_search.gt.0

		Allx(1)=0d0
		Allf(1)=outValue
		Allg(1)=inoutgra.dot.dir
		if((dabs(x).le.zero_number).or.(x.le.0)) then
			Allx(2)=LBFGSType%first_step_in_Linear_search
		else 
			Allx(2)=x
		end if

		NewPoint=point+(Allx(2)*dir)
		call LBFGSType%target_Function(Allf(2),gradient,NewPoint)
		Allg(2)=gradient.dot.dir
		outf=Allf(2)
		outx=Allx(2)
		if(max_step_flag)then
			Savepoint=NewPoint
			Savegradient=gradient
			savex=Allx(2)
			saveValue=Allf(2)
		end if
		if(dabs(Allg(2)).le.gradient_zero_number)then
			Point=NewPoint
			x=Allx(2)
			outValue=Allf(2)
			inoutgra=gradient
			return
		end if

		do i=2,max_running
			ith=i+1
			if(ith.gt.Datalength)then
				call writemess('ERROR in LinearSearch2',-1)
				call error_stop
			end if
			call runLSM(outx,Allx(1:i),Allf(1:i),Allg(1:i))
			Allx(ith)=outx
			NewPoint=point+(Allx(ith)*dir)
			call LBFGSType%target_Function(Allf(ith),gradient,NewPoint)
			outf=Allf(ith)
			outx=Allx(ith)
			Allg(ith)=gradient.dot.dir
			if(dabs(Allg(ith)).le.gradient_zero_number)exit
			
		end do

		if(max_step_flag.and.(outx.gt.LBFGSType%max_step_in_Linear_search))then
			Point=Savepoint
			x=Savex
			outValue=SaveValue
			inoutgra=Savegradient
		else
			Point=NewPoint
			x=outx
			outValue=outf
			inoutgra=gradient
		end if

		return
	end subroutine


end module
