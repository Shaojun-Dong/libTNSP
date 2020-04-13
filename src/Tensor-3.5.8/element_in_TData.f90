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
module element_module
	use Tools
	implicit none
contains
!*****************  integer   *****************
	integer function Element_subroutine_int_dim1(i,Tdata,LD1) result(output)
		integer,intent(in)::i,LD1
		integer,intent(in)::Tdata(LD1)
			output=Tdata(i)
		return
	end function
	integer function  Element_subroutine_int_dim2(i,j,Tdata,LD1,LD2) result(output)
		integer,intent(in)::i,j,LD1,LD2
		integer,intent(in)::Tdata(LD1,LD2)
			output=Tdata(i,j)
		return
	end function
	integer function   Element_subroutine_int_dim3(i,j,k,Tdata,LD1,LD2,LD3) result(output)
		integer,intent(in)::i,j,k,LD1,LD2,LD3
		integer,intent(in)::Tdata(LD1,LD2,LD3)
				output=Tdata(i,j,k)
		return
	end function
	integer function   Element_subroutine_int_dim4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4) result(output)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		integer,intent(in)::Tdata(LD1,LD2,LD3,LD4)
				output=Tdata(i,j,k,l)
		return
	end function
		
!*****************  real*4   *****************
	real(kind=4) function Element_subroutine_real4_dim1(i,Tdata,LD1) result(output)
		integer,intent(in)::i,LD1
		real*4,intent(in)::Tdata(LD1)
				output=Tdata(i)
		return
	end function
	real(kind=4) function  Element_subroutine_real4_dim2(i,j,Tdata,LD1,LD2) result(output)
		integer,intent(in)::i,j,LD1,LD2
		real*4,intent(in)::Tdata(LD1,LD2)
				output=Tdata(i,j)
		return
	end function
	real(kind=4) function Element_subroutine_real4_dim3(i,j,k,Tdata,LD1,LD2,LD3) result(output)
		integer,intent(in)::i,j,k,LD1,LD2,LD3
		real*4,intent(in)::Tdata(LD1,LD2,LD3)
				output=Tdata(i,j,k)
		return
	end function
	real(kind=4) function Element_subroutine_real4_dim4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4) result(output)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real*4,intent(in)::Tdata(LD1,LD2,LD3,LD4)
				output=Tdata(i,j,k,l)
		return
	end function




!*****************  real*8   *****************
	real(kind=8) function Element_subroutine_real8_dim1(i,Tdata,LD1) result(output)
		integer,intent(in)::i,LD1
		real*8,intent(in)::Tdata(LD1)
				output=Tdata(i)
		return
	end function
	real(kind=8) function Element_subroutine_real8_dim2(i,j,Tdata,LD1,LD2) result(output)
		integer,intent(in)::i,j,LD1,LD2
		real*8,intent(in)::Tdata(LD1,LD2)
				output=Tdata(i,j)
		return
	end function
	real(kind=8) function Element_subroutine_real8_dim3(i,j,k,Tdata,LD1,LD2,LD3) result(output)
		integer,intent(in)::i,j,k,LD1,LD2,LD3
		real*8,intent(in)::Tdata(LD1,LD2,LD3)
				output=Tdata(i,j,k)
		return
	end function
	real(kind=8) function Element_subroutine_real8_dim4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4) result(output)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real*8,intent(in)::Tdata(LD1,LD2,LD3,LD4)
				output=Tdata(i,j,k,l)
		return
	end function


!*****************  complex*8 or (complex (kind=4))   *****************
	complex(kind=4) function Element_subroutine_com4_dim1(i,Tdata,LD1) result(output)
		integer,intent(in)::i,LD1
		complex*8,intent(in)::Tdata(LD1)
				output=Tdata(i)
		return
	end function
	complex(kind=4) function Element_subroutine_com4_dim2(i,j,Tdata,LD1,LD2) result(output)
		integer,intent(in)::i,j,LD1,LD2
		complex*8,intent(in)::Tdata(LD1,LD2)
				output=Tdata(i,j)
		return
	end function
	complex(kind=4) function  Element_subroutine_com4_dim3(i,j,k,Tdata,LD1,LD2,LD3) result(output)
		integer,intent(in)::i,j,k,LD1,LD2,LD3
		complex*8,intent(in)::Tdata(LD1,LD2,LD3)
				output=Tdata(i,j,k)
		return
	end function
	complex(kind=4) function  Element_subroutine_com4_dim4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4)result(output)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex*8,intent(in)::Tdata(LD1,LD2,LD3,LD4)
				output=Tdata(i,j,k,l)
		return
	end function

!*****************  complex*16 or (complex (kind=8))   *****************
	complex(kind=8) function Element_subroutine_com8_dim1(i,Tdata,LD1) result(output)
		integer,intent(in)::i,LD1
		complex*16,intent(in)::Tdata(LD1)
				output=Tdata(i)
		return
	end function
	complex(kind=8) function Element_subroutine_com8_dim2(i,j,Tdata,LD1,LD2) result(output)
		integer,intent(in)::i,j,LD1,LD2
		complex*16,intent(in)::Tdata(LD1,LD2)
				output=Tdata(i,j)
		return
	end function
	complex(kind=8) function Element_subroutine_com8_dim3(i,j,k,Tdata,LD1,LD2,LD3) result(output)
		integer,intent(in)::i,j,k,LD1,LD2,LD3
		complex*16,intent(in)::Tdata(LD1,LD2,LD3)
				output=Tdata(i,j,k)
		return
	end function
	complex(kind=8) function Element_subroutine_com8_dim4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4) result(output)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex*16,intent(in)::Tdata(LD1,LD2,LD3,LD4)
				output=Tdata(i,j,k,l)
		return
	end function



!*****************  logical   *****************
	logical function Element_subroutine_logi_dim1(i,Tdata,LD1) result(output)
		integer,intent(in)::i,LD1
		logical,intent(in)::Tdata(LD1)
				output=Tdata(i)
		return
	end function
	logical function Element_subroutine_logi_dim2(i,j,Tdata,LD1,LD2) result(output)
		integer,intent(in)::i,j,LD1,LD2
		logical,intent(in)::Tdata(LD1,LD2)
				output=Tdata(i,j)
		return
	end function
	logical function Element_subroutine_logi_dim3(i,j,k,Tdata,LD1,LD2,LD3) result(output)
		integer,intent(in)::i,j,k,LD1,LD2,LD3
		logical,intent(in)::Tdata(LD1,LD2,LD3)
				output=Tdata(i,j,k)
		return
	end function
	logical function Element_subroutine_logi_dim4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4) result(output)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		logical,intent(in)::Tdata(LD1,LD2,LD3,LD4)
				output=Tdata(i,j,k,l)
		return
	end function
	
!*****************  character(len=*)   *****************
	character(len=max_len_of_char) function Element_subroutine_char_dim1(i,Tdata,LD1) result(output)
		integer,intent(in)::i,LD1
		character(len=*),intent(in)::Tdata(LD1)
				output=Tdata(i)
		return
	end function
	character(len=max_len_of_char) function  Element_subroutine_char_dim2(i,j,Tdata,LD1,LD2) result(output)
		integer,intent(in)::i,j,LD1,LD2
		character(len=*),intent(in)::Tdata(LD1,LD2)
				output=Tdata(i,j)
		return
	end function
	character(len=max_len_of_char) function  Element_subroutine_char_dim3(i,j,k,Tdata,LD1,LD2,LD3) result(output)
		integer,intent(in)::i,j,k,LD1,LD2,LD3
		character(len=*),intent(in)::Tdata(LD1,LD2,LD3)
				output=Tdata(i,j,k)
		return
	end function
	character(len=max_len_of_char) function  Element_subroutine_char_dim4(i,j,k,l,Tdata,LD1,LD2,LD3,LD4) result(output)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		character(len=*),intent(in)::Tdata(LD1,LD2,LD3,LD4)
				output=Tdata(i,j,k,l)
	end function
	
	





























end module
