!below is test for add  
!num=500 result is
! time for B=A+A using package:   7.1364369999999999     
! time for B=A+A not using package:   3.9298359999999990     
! time for B=A*3 using package:   1.2526309999999992     
! time for B=A*3 not using package:   2.0533389999999994
!
! If use the old type of Tensor, the one only allow for complex*16. the running will be slower
! If use MKL instead of lapack and blas, the package will faster than fortran
!
!  The function of + - is
!	  use zcopy and zscal, there are two operations
!  The function of *num is
!    use zcopy and zaxpy, there are two operations
!  It will be faster than A=A+B when use MKL
!
!
!
!
!
!include '/home/sjdong/Tensor/MPI/Tensor.f90'

program aaa
	use Tensor_complex
	implicit none
	type(Tensor)::A,B,C
	complex*16,allocatable::AA(:,:),BB(:,:),CC(:,:)
	integer::i,N,num
	real*8::time1,time2
	num=500
	A=generate((/num,num/),'complex*16')
	call B%allocate((/num,num/),'complex*16')
	allocate(AA(num,num))
	allocate(BB(num,num))
	N=5000
	call cpu_time(time1)
	do i=1,N
		B=A+A
	end do
	call cpu_time(time2)
	write(*,*)"time for B=A+A using package:",time2-time1
	AA=A
	call cpu_time(time1)
	do i=1,N
		BB=AA+AA
	end do
	call cpu_time(time2)
	write(*,*)"time for B=A+A not using package:",time2-time1
	
	N=1000
	call cpu_time(time1)
	do i=1,N
		B=A*3
	end do
	call cpu_time(time2)
	write(*,*)"time for B=A*3 using package:",time2-time1
	call cpu_time(time1)
	do i=1,N
		BB=AA*3
	end do
	call cpu_time(time2)
	write(*,*)"time for B=A*3 not using package:",time2-time1
end 
