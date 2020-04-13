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
module permutation_module
	use Tools
	use Dimension_typede
	implicit none
	
contains

!**************************************************************
!                  complex(kind=8)
!
!  the array input can be any dimenison
!  use for inputint T%Tensor_data,and T is a rank-3 Tensor
!  LD1,LD2,LD3 are the 3 dimension of the data
!	do the permutatation, where the first index not change, which means that Tout(i,:,:)=transpose(Tin(i,:,:))
!  outdata and indata can be a same array,which means one can call permutation_rank3_data1(A,A,3,4,5)
!  lenD is used in zcpoy, here lenD=LD1
	subroutine permutation_rank3_data1_com8(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		complex(kind=8),intent(inout)::outdata(LD1,LD3,LD2)
		complex(kind=8),intent(in)::indata(LD1,LD2,LD3)
		integer::i,j,k
		do j=1,LD3
			do k=1,LD2
				call zcopy(lenD,indata(:,k,j),1,outdata(:,j,k),1)!This line is faster than transpose
			end do
		end do
		return
	end subroutine
!  the array input can be any dimenison
!  use for inputint T%Tensor_data,and T is a rank-3 Tensor
!  LD1,LD2,LD3 are the 3 dimension of the data
!	do the permutatation, where the second index not change, which means that Tout(:,i,:)=transpose(Tin(:,i,:))
!  outdata and indata can be a same array,which means one can call permutation_rank3_data1(A,A,3,4,5)
!  lenD is used in zcpoy, here lenD=LD1*LD3
	subroutine permutation_rank3_data2_com8(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		complex(kind=8),intent(inout)::outdata(LD3,LD2,LD1)
		complex(kind=8),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD2
			outdata(:,i,:)=transpose(indata(:,i,:))
		end do
		return
	end subroutine
!  the array input can be any dimenison
!  use for inputint T%Tensor_data,and T is a rank-3 Tensor
!  LD1,LD2,LD3 are the 3 dimension of the data
!	do the permutatation, where the third index not change, which means that Tout(:,:,i)=transpose(Tin(:,:,i))
!  outdata and indata can be a same array,which means one can call permutation_rank3_data1(A,A,3,4,5)
!  lenD is used in zcpoy, here lenD=LD1*LD2
	subroutine permutation_rank3_data3_com8(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		complex(kind=8),intent(inout)::outdata(LD2,LD1,LD3)
		complex(kind=8),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD3
			outdata(:,:,i)=transpose(indata(:,:,i))
		end do
		return
	end subroutine
!  the array input can be any dimenison
!  use for inputint T%Tensor_data,and T is a rank-2 Tensor
!  LD1,LD2 are the 2 dimension of the data
!	do the permutatation, which means that Tout(:,:)=transpose(Tin(:,:))
!  lenD is used in zcpoy, here lenD=LD1*LD2
	subroutine permutation_rank2_data_com8(outdata,indata,LD1,LD2)
		integer,intent(in)::LD1,LD2
		complex(kind=8),intent(inout)::outdata(LD2,LD1)
		complex(kind=8),intent(in)::indata(LD1,LD2)
		outdata=transpose(indata)
		return
	end subroutine

!**************************************************************
!                  complex(kind=4)
!
	subroutine permutation_rank3_data1_com4(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		complex(kind=4),intent(inout)::outdata(LD1,LD3,LD2)
		complex(kind=4),intent(in)::indata(LD1,LD2,LD3)
		integer::i,j,k
		do j=1,LD3
			do k=1,LD2
				call ccopy(lenD,indata(:,k,j),1,outdata(:,j,k),1)!This line is faster than transpose
			end do
		end do
		return
	end subroutine
	subroutine permutation_rank3_data2_com4(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		complex(kind=4),intent(inout)::outdata(LD3,LD2,LD1)
		complex(kind=4),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD2
			outdata(:,i,:)=transpose(indata(:,i,:))
		end do
		return
	end subroutine
	subroutine permutation_rank3_data3_com4(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		complex(kind=4),intent(inout)::outdata(LD2,LD1,LD3)
		complex(kind=4),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD3
			outdata(:,:,i)=transpose(indata(:,:,i))
		end do
		return
	end subroutine
	subroutine permutation_rank2_data_com4(outdata,indata,LD1,LD2)
		integer,intent(in)::LD1,LD2
		complex(kind=4),intent(inout)::outdata(LD2,LD1)
		complex(kind=4),intent(in)::indata(LD1,LD2)
		outdata=transpose(indata)
		return
	end subroutine


!**************************************************************
!                  real(kind=8)
!
	subroutine permutation_rank3_data1_real8(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		real(kind=8),intent(inout)::outdata(LD1,LD3,LD2)
		real(kind=8),intent(in)::indata(LD1,LD2,LD3)
		integer::i,j,k
		do j=1,LD3
			do k=1,LD2
				call dcopy(lenD,indata(:,k,j),1,outdata(:,j,k),1)!This line is faster than transpose
			end do
		end do
		return
	end subroutine
	subroutine permutation_rank3_data2_real8(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		real(kind=8),intent(inout)::outdata(LD3,LD2,LD1)
		real(kind=8),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD2
			outdata(:,i,:)=transpose(indata(:,i,:))
		end do
		return
	end subroutine
	subroutine permutation_rank3_data3_real8(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		real(kind=8),intent(inout)::outdata(LD2,LD1,LD3)
		real(kind=8),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD3
			outdata(:,:,i)=transpose(indata(:,:,i))
		end do
		return
	end subroutine
	subroutine permutation_rank2_data_real8(outdata,indata,LD1,LD2)
		integer,intent(in)::LD1,LD2
		real(kind=8),intent(inout)::outdata(LD2,LD1)
		real(kind=8),intent(in)::indata(LD1,LD2)
		outdata=transpose(indata)
		return
	end subroutine

!**************************************************************
!                  real(kind=4)
!
	subroutine permutation_rank3_data1_real4(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		real(kind=4),intent(inout)::outdata(LD1,LD3,LD2)
		real(kind=4),intent(in)::indata(LD1,LD2,LD3)
		integer::i,j,k
		do j=1,LD3
			do k=1,LD2
				call scopy(lenD,indata(:,k,j),1,outdata(:,j,k),1)!This line is faster than transpose
			end do
		end do
		return
	end subroutine
	subroutine permutation_rank3_data2_real4(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		real(kind=4),intent(inout)::outdata(LD3,LD2,LD1)
		real(kind=4),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD2
			outdata(:,i,:)=transpose(indata(:,i,:))
		end do
		return
	end subroutine
	subroutine permutation_rank3_data3_real4(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		real(kind=4),intent(inout)::outdata(LD2,LD1,LD3)
		real(kind=4),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD3
			outdata(:,:,i)=transpose(indata(:,:,i))
		end do
		return
	end subroutine
	subroutine permutation_rank2_data_real4(outdata,indata,LD1,LD2)
		integer,intent(in)::LD1,LD2
		real(kind=4),intent(inout)::outdata(LD2,LD1)
		real(kind=4),intent(in)::indata(LD1,LD2)
		outdata=transpose(indata)
		return
	end subroutine

!**************************************************************
!                  integer
!
	subroutine permutation_rank3_data1_int(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		integer,intent(inout)::outdata(LD1,LD3,LD2)
		integer,intent(in)::indata(LD1,LD2,LD3)
		integer::i,j,k
		do j=1,LD3
			do k=1,LD2
				outdata(:,j,k)=indata(:,k,j)
			end do
		end do
		return
	end subroutine
	subroutine permutation_rank3_data2_int(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		integer,intent(inout)::outdata(LD3,LD2,LD1)
		integer,intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD2
			outdata(:,i,:)=transpose(indata(:,i,:))
		end do
		return
	end subroutine
	subroutine permutation_rank3_data3_int(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		integer,intent(inout)::outdata(LD2,LD1,LD3)
		integer,intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD3
			outdata(:,:,i)=transpose(indata(:,:,i))
		end do
		return
	end subroutine
	subroutine permutation_rank2_data_int(outdata,indata,LD1,LD2)
		integer,intent(in)::LD1,LD2
		integer,intent(inout)::outdata(LD2,LD1)
		integer,intent(in)::indata(LD1,LD2)
		outdata=transpose(indata)
		return
	end subroutine


!**************************************************************
!                  logical
!
	subroutine permutation_rank3_data1_logi(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		logical,intent(inout)::outdata(LD1,LD3,LD2)
		logical,intent(in)::indata(LD1,LD2,LD3)
		integer::i,j,k
		do j=1,LD3
			do k=1,LD2
				outdata(:,j,k)=indata(:,k,j)
			end do
		end do
		return
	end subroutine
	subroutine permutation_rank3_data2_logi(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		logical,intent(inout)::outdata(LD3,LD2,LD1)
		logical,intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD2
			outdata(:,i,:)=transpose(indata(:,i,:))
		end do
		return
	end subroutine
	subroutine permutation_rank3_data3_logi(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		logical,intent(inout)::outdata(LD2,LD1,LD3)
		logical,intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD3
			outdata(:,:,i)=transpose(indata(:,:,i))
		end do
		return
	end subroutine
	subroutine permutation_rank2_data_logi(outdata,indata,LD1,LD2)
		integer,intent(in)::LD1,LD2
		logical,intent(inout)::outdata(LD2,LD1)
		logical,intent(in)::indata(LD1,LD2)
		outdata=transpose(indata)
		return
	end subroutine


!**************************************************************
!                  characters
!
	subroutine permutation_rank3_data1_char(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		character(len=*),intent(inout)::outdata(LD1,LD3,LD2)
		character(len=*),intent(in)::indata(LD1,LD2,LD3)
		integer::i,j,k
		do j=1,LD3
			do k=1,LD2
				outdata(:,j,k)=indata(:,k,j)
			end do
		end do
		return
	end subroutine
	subroutine permutation_rank3_data2_char(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		character(len=*),intent(inout)::outdata(LD3,LD2,LD1)
		character(len=*),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD2
			outdata(:,i,:)=transpose(indata(:,i,:))
		end do
		return
	end subroutine
	subroutine permutation_rank3_data3_char(outdata,indata,LD1,LD2,LD3,lenD)
		integer,intent(in)::LD1,LD2,LD3,lenD
		character(len=*),intent(inout)::outdata(LD2,LD1,LD3)
		character(len=*),intent(in)::indata(LD1,LD2,LD3)
		integer::i
		do i=1,LD3
			outdata(:,:,i)=transpose(indata(:,:,i))
		end do
		return
	end subroutine
	subroutine permutation_rank2_data_char(outdata,indata,LD1,LD2)
		integer,intent(in)::LD1,LD2
		character(len=*),intent(inout)::outdata(LD2,LD1)
		character(len=*),intent(in)::indata(LD1,LD2)
		outdata=transpose(indata)
		return
	end subroutine









end module
