!    This  is a example of how to use external of a Tensor. It is a vary useful function as one 
!  could write its own function on Tensor. It is better not to writen the new code on the original
!  file(on Tensor.f90 or something else).
!    All the data in Tensor are private. So one can not modify it from a outside file. But one can
!  use the function T%extarnal to modify these files. Below is a example. Get the Sub-matrix of T1
!  to store in Tensor T2. 
module test_example
	use Tensor_complex
	implicit none
	integer,private,save::T1dim1,T1dim2,T2dim1,T2dim2,i1,j1,i2,j2
contains
	 type(Tensor) function submatrix(A,i1_,i2_,j1_,j2_) result(X)!This is the main function of getting sub matrix
	 	integer::i1_,i2_,j1_,j2_
		type(Tensor)::A
		call setinfo(A,X,i1_,i2_,j1_,j2_)!the the input data as common data,as the A%external can not input so many data
		call A%external(submatrix_in,X)!submatrix_in is a subroutine, as below
		return
	end function
	subroutine submatrix_in(Tdata,lenT,Bdata,lenB)! the data is one dimension
		integer::lenT,lenB !this line can not change
		real*4::Tdata(lenT),Bdata(lenB)!!this line can not change,exacpt words 'real*4',Only valid on real*4 Tensor
		call submatrix_out(Tdata,Bdata)!as the data is one-dimensional, one should change it shape with less cpu time
		                               !So write another subroutine, it will change the shape of input data without
		                               !using cpu time.
		return
	end subroutine
	subroutine submatrix_out(Tdata,Bdata)               ! the input data is one dimension, but it will regard as two dimension
		real*4::Tdata(T1dim1,T1dim2),Bdata(T2dim1,T2dim2)! Specify the shape of input data
		Bdata=Tdata(i1:i2,j1:j2)
		return
	end subroutine
	subroutine setinfo(T1,T2,i1_,i2_,j1_,j2_)!The data set as common data
		integer::i1_,i2_,j1_,j2_
		type(Tensor)::T1,T2
		i1=i1_
		j1=j1_
		i2=i2_
		j2=j2_
		T1dim1=T1.dim.1
		T1dim2=T1.dim.2
		T2dim1=i2-i1+1
		T2dim2=j2-j1+1
		call T2%allocate((/T2dim1,T2dim2/),T1%getClassType())
		return
	end subroutine
end module

program aaa
	use test_example
	use Tensor_complex
	implicit none
	type(Tensor)::A,B,C
	integer::i1,i2,j1,j2
	i1=2
	i2=3
	j1=2
	j2=4
	A=generate((/4,4/),'real*4')
	B=submatrix(A,2,3,2,4)
	call B%print('B=A('+i1+':'+i2+','+j1+':'+j2+')')
	call A%print('Data in A')
	C=A%sub((/2,3,2,4/))
	call C%print('C=A%sub((/'+i1+','+i2+','+j1+','+j2+'/)), the same as the subroute written in the example')
	stop
end 
