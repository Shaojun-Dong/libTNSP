!   (Object-Oriented Programming)OOP languages allow a child object to override a procedure
! inherited from its parent object. we can specify a type-bound procedure in a child type 
! that has the same binding-name as a type-bound procedure in the parent type. When the 
! child overrides a particular type-bound procedure, the version defined in its derived 
! type will get invoked instead of the version defined in the parent. 
! Below is an example where Tensor2 is a child type of Tensor, we write the subroutine T%info
! 
!    If Tensor.f90, info is Type-Bound Procedures which point to
!
!       generic,public::info =>Tprint1,Tprint2,Tprint3,Tprint4
!
!    It pont to three subroutine, they are Tprint1,Tprint2,Tprint3,Tprint4. We rewrite Tprint1
! for example. Note that the rewriting function should have the same input as the one in parent
! object. Since info point to Trint1, we do not need to write the code info =>Tprint1 any more
!   
!
!
module Tensor_extends
	use Tensor_complex
	implicit none
	type,extends (Tensor) :: Tensor2
		private
		integer:: test=1
	contains
		procedure::Tprint1! the Tprint1 is overwrite, so the code of info will point to the new Tprint1. This
		                  ! code is in the parent object(Tensor.f90), which is info =>Tprint1,Tprint2,Tprint3,Tprint4
		                  
		procedure,public::empty =>emptyTensor!empty is not a generic, so one should rewrite empty =>emptyTensor
	end type
contains
	subroutine Tprint1(T)! input should be the same as Tprint1 in parent object
		class(Tensor2),intent(in)::T
		call T%Tensor%info()! print the Tensor data
		write(*,*)"test",T%test! print the newdata
		return
	end subroutine
	
	subroutine emptyTensor(T)
		class(Tensor2),intent(inout)::T
		call T%Tensor%empty()
		write(*,*)"test",T%test
		return
	end subroutine
end module



program aaa
	use Tensor_complex
	use GradientMethod
	use Tensor_extends
	use eigen_value
	implicit none
	type(Tensor2)::TT
	TT=generate((/10/),'real*4')
	call TT%info()
	call TT%empty()
	stop
end 
