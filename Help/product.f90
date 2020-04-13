!
!below is test for product  
!num=100 result is
! time for B=A*A ,matrix product using package:  0.14220700000000000     
! time for B=A*A ,matrix product not using package:  0.14966699999999999 
! If use the old type of Tensor, the one only allow for complex*16. the running will be the same
! If use MKL instead of lapack and blas, the package will faster than fortran even in small Tensor
!include '/home/sjdong/Tensor/MPI/Tensor.f90'

program aaa
	use Tensor_complex
	implicit none
	type(Tensor)::A,B,C
	complex*16,allocatable::AA(:,:),BB(:,:),CC(:,:)
	integer::i,N,num
	real*8::time1,time2
	num=100
	A=generate((/num,num/),'complex*16')
	call B%allocate((/num,num/),'complex*16')
	allocate(AA(num,num))
	allocate(BB(num,num))
	N=100
	call cpu_time(time1)
	do i=1,N
		B=A*A
	end do
	call cpu_time(time2)
	write(*,*)"time for B=A*A ,matrix product using package:",time2-time1
	AA=A
	call cpu_time(time1)
	do i=1,N
		BB=matmul(AA,AA)
	end do
	call cpu_time(time2)
	write(*,*)"time for B=A*A ,matrix product not using package:",time2-time1
	
	
	stop
end 
