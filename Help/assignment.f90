!below is test for assignment  
!num=10 result is
! time for B=A using package:  0.43771599999999999     
! time for B=A not using package:  0.12551099999999998
!num=100 result is
! time for B=A using package:  0.96707599999999994     
! time for B=A not using package:   1.0824100000000001
!
!If the Tensor is more than 1 dimension, it will faster than fortran even in small Tensor.
program aaa
	use Tensor_complex
	implicit none
	type(Tensor)::A,B,C
	complex*16,allocatable::AA(:),BB(:),CC(:)
	integer::i,N,num
	real*8::time1,time2
	num=20
	A=generate((/num/),'complex*16')
	call B%allocate((/num/),'complex*16')
	allocate(AA(num))
	allocate(BB(num))
	N=9000000
	call cpu_time(time1)
	do i=1,N
		B=A
	end do
	call cpu_time(time2)
	write(*,*)"time for B=A using package:",time2-time1
	AA=A
	call cpu_time(time1)
	do i=1,N
		BB=AA
	end do
	call cpu_time(time2)
	write(*,*)"time for B=A not using package:",time2-time1
	
	stop
end 
