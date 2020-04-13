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
module print_module
	use Tools
	implicit none
	!printDataMAtrix_int_dim1_unit
	!printDataMAtrix_int_dim1_no_unit(indata,M,words)
	!printDataMAtrix_int_dim2_unit(indata,M,N,uni,words)
	!printDataMAtrix_int_dim2_no_unit(indata,M,N,words)
contains

	
!******************  integer   ***********************
	subroutine printDataMAtrix_int_dim1_unit(indata,M,uni,words)
		integer,intent(in)::M,uni
		integer,intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (uni,w) indata
		else
			write (uni,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_int_dim1_no_unit(indata,M,words)
		integer,intent(in)::M
		integer,intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (*,w) indata
		else
			write (*,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_int_dim2_unit(indata,M,N,uni,words)
		integer,intent(in)::M,N,uni
		integer,intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (uni,w) indata(i,:)
				write(uni,*) ""
			end do 
		else
			do i=1,M
				write (uni,*) indata(i,:)
				write(uni,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_int_dim2_no_unit(indata,M,N,words)
		integer,intent(in)::M,N
		integer,intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (*,w) indata(i,:)
				write(*,*) ""
			end do 
		else
			do i=1,M
				write (*,*) indata(i,:)
				write(*,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_int_dim3_unit(indata,LD1,LD2,LD3,uni,words)
		integer,intent(in)::LD1,LD2,LD3,uni
		integer,intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,w) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,*) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_int_dim3_no_unit(indata,LD1,LD2,LD3,words)
		integer,intent(in)::LD1,LD2,LD3
		integer,intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,w) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,*) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_int_dim4_unit(indata,LD1,LD2,LD3,LD4,uni,words)
		integer,intent(in)::LD1,LD2,LD3,LD4,uni
		integer,intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,w) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,*) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine	
	subroutine printDataMAtrix_int_dim4_no_unit(indata,LD1,LD2,LD3,LD4,words)
		integer,intent(in)::LD1,LD2,LD3,LD4
		integer,intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,w) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,*) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine	
!******************  real*4   ***********************
	subroutine printDataMAtrix_real4_dim1_unit(indata,M,uni,words)
		integer,intent(in)::M,uni
		real(kind=4),intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (uni,w) indata
		else
			write (uni,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real4_dim1_no_unit(indata,M,words)
		integer,intent(in)::M
		real(kind=4),intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (*,w) indata
		else
			write (*,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real4_dim2_unit(indata,M,N,uni,words)
		integer,intent(in)::M,N,uni
		real(kind=4),intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (uni,w) indata(i,:)
				write(uni,*) ""
			end do 
		else
			do i=1,M
				write (uni,*) indata(i,:)
				write(uni,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real4_dim2_no_unit(indata,M,N,words)
		integer,intent(in)::M,N
		real(kind=4),intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (*,w) indata(i,:)
				write(*,*) ""
			end do 
		else
			do i=1,M
				write (*,*) indata(i,:)
				write(*,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real4_dim3_unit(indata,LD1,LD2,LD3,uni,words)
		integer,intent(in)::LD1,LD2,LD3,uni
		real(kind=4),intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,w) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,*) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real4_dim3_no_unit(indata,LD1,LD2,LD3,words)
		integer,intent(in)::LD1,LD2,LD3
		real(kind=4),intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,w) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,*) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real4_dim4_unit(indata,LD1,LD2,LD3,LD4,uni,words)
		integer,intent(in)::LD1,LD2,LD3,LD4,uni
		real(kind=4),intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,w) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,*) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine	
	subroutine printDataMAtrix_real4_dim4_no_unit(indata,LD1,LD2,LD3,LD4,words)
		integer,intent(in)::LD1,LD2,LD3,LD4
		real(kind=4),intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,w) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,*) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine	
!******************  real*8   ***********************
	subroutine printDataMAtrix_real8_dim1_unit(indata,M,uni,words)
		integer,intent(in)::M,uni
		real(kind=8),intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (uni,w) indata
		else
			write (uni,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real8_dim1_no_unit(indata,M,words)
		integer,intent(in)::M
		real(kind=8),intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (*,w) indata
		else
			write (*,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real8_dim2_unit(indata,M,N,uni,words)
		integer,intent(in)::M,N,uni
		real(kind=8),intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (uni,w) indata(i,:)
				write(uni,*) ""
			end do 
		else
			do i=1,M
				write (uni,*) indata(i,:)
				write(uni,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real8_dim2_no_unit(indata,M,N,words)
		integer,intent(in)::M,N
		real(kind=8),intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (*,w) indata(i,:)
				write(*,*) ""
			end do 
		else
			do i=1,M
				write (*,*) indata(i,:)
				write(*,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real8_dim3_unit(indata,LD1,LD2,LD3,uni,words)
		integer,intent(in)::LD1,LD2,LD3,uni
		real(kind=8),intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,w) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,*) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real8_dim3_no_unit(indata,LD1,LD2,LD3,words)
		integer,intent(in)::LD1,LD2,LD3
		real(kind=8),intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,w) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,*) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_real8_dim4_unit(indata,LD1,LD2,LD3,LD4,uni,words)
		integer,intent(in)::LD1,LD2,LD3,LD4,uni
		real(kind=8),intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,w) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,*) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine		
	subroutine printDataMAtrix_real8_dim4_no_unit(indata,LD1,LD2,LD3,LD4,words)
		integer,intent(in)::LD1,LD2,LD3,LD4
		real(kind=8),intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,w) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,*) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine	
	
!******************  complex(kind=4)  ***********************
	subroutine printDataMAtrix_com4_dim1_unit(indata,M,uni,words)
		integer,intent(in)::M,uni
		complex(kind=4),intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (uni,w) indata
		else
			write (uni,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com4_dim1_no_unit(indata,M,words)
		integer,intent(in)::M
		complex(kind=4),intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (*,w) indata
		else
			write (*,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com4_dim2_unit(indata,M,N,uni,words)
		integer,intent(in)::M,N,uni
		complex(kind=4),intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (uni,w) indata(i,:)
				write(uni,*) ""
			end do 
		else
			do i=1,M
				write (uni,*) indata(i,:)
				write(uni,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com4_dim2_no_unit(indata,M,N,words)
		integer,intent(in)::M,N
		complex(kind=4),intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (*,w) indata(i,:)
				write(*,*) ""
			end do 
		else
			do i=1,M
				write (*,*) indata(i,:)
				write(*,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com4_dim3_unit(indata,LD1,LD2,LD3,uni,words)
		integer,intent(in)::LD1,LD2,LD3,uni
		complex(kind=4),intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,w) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,*) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com4_dim3_no_unit(indata,LD1,LD2,LD3,words)
		integer,intent(in)::LD1,LD2,LD3
		complex(kind=4),intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,w) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,*) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com4_dim4_unit(indata,LD1,LD2,LD3,LD4,uni,words)
		integer,intent(in)::LD1,LD2,LD3,LD4,uni
		complex(kind=4),intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,w) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,*) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine	
	subroutine printDataMAtrix_com4_dim4_no_unit(indata,LD1,LD2,LD3,LD4,words)
		integer,intent(in)::LD1,LD2,LD3,LD4
		complex(kind=4),intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,w) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,*) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine	

!******************  complex(kind=8)  ***********************
	subroutine printDataMAtrix_com8_dim1_unit(indata,M,uni,words)
		integer,intent(in)::M,uni
		complex(kind=8),intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (uni,w) real(indata)
			write (uni,w) aimag(indata)
		else
			write (uni,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com8_dim1_no_unit(indata,M,words)
		integer,intent(in)::M
		complex(kind=8),intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (*,w) real(indata)
			write (*,w) aimag(indata)
		else
			write (*,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com8_dim2_unit(indata,M,N,uni,words)
		integer,intent(in)::M,N,uni
		complex(kind=8),intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (uni,w) real(indata(i,:))
				write (uni,w) aimag(indata(i,:))
				write(uni,*) ""
			end do 
		else
			do i=1,M
				write (uni,*) indata(i,:)
				write(uni,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com8_dim2_no_unit(indata,M,N,words)
		integer,intent(in)::M,N
		complex(kind=8),intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (*,w) real(indata(i,:))
				write (*,w) aimag(indata(i,:))
				write(*,*) ""
			end do 
		else
			do i=1,M
				write (*,*) indata(i,:)
				write(*,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com8_dim3_unit(indata,LD1,LD2,LD3,uni,words)
		integer,intent(in)::LD1,LD2,LD3,uni
		complex(kind=8),intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,w) real(indata(i,:,j))
					write (uni,w) aimag(indata(i,:,j))
					write(uni,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,*) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com8_dim3_no_unit(indata,LD1,LD2,LD3,words)
		integer,intent(in)::LD1,LD2,LD3
		complex(kind=8),intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,w) real(indata(i,:,j))
					write (*,w) aimag(indata(i,:,j))
					write(*,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,*) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_com8_dim4_unit(indata,LD1,LD2,LD3,LD4,uni,words)
		integer,intent(in)::LD1,LD2,LD3,LD4,uni
		complex(kind=8),intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,w) real(indata(i,:,k,j))
					write (uni,w) aimag(indata(i,:,k,j))
					write(uni,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,*) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine	
	subroutine printDataMAtrix_com8_dim4_no_unit(indata,LD1,LD2,LD3,LD4,words)
		integer,intent(in)::LD1,LD2,LD3,LD4
		complex(kind=8),intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,w) real(indata(i,:,k,j))
					write (*,w) aimag(indata(i,:,k,j))
					write(*,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,*) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine		
	
	
!******************  logical   ***********************
	subroutine printDataMAtrix_logi_dim1_unit(indata,M,uni,words)
		integer,intent(in)::M,uni
		logical,intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M+words+')'
			write (uni,w) indata
		else
			write (uni,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_logi_dim1_no_unit(indata,M,words)
		integer,intent(in)::M
		logical,intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+M	+words+')'
			write (*,w) indata
		else
			write (*,*) indata
		end if
		return
	end subroutine
	subroutine printDataMAtrix_logi_dim2_unit(indata,M,N,uni,words)
		integer,intent(in)::M,N,uni
		logical,intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (uni,w) indata(i,:)
				write(uni,*) ""
			end do 
		else
			do i=1,M
				write (uni,*) indata(i,:)
				write(uni,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_logi_dim2_no_unit(indata,M,N,words)
		integer,intent(in)::M,N
		logical,intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (*,w) indata(i,:)
				write(*,*) ""
			end do 
		else
			do i=1,M
				write (*,*) indata(i,:)
				write(*,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_logi_dim3_unit(indata,LD1,LD2,LD3,uni,words)
		integer,intent(in)::LD1,LD2,LD3,uni
		logical,intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,w) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,*) indata(i,:,j)
					write(uni,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_logi_dim3_no_unit(indata,LD1,LD2,LD3,words)
		integer,intent(in)::LD1,LD2,LD3
		logical,intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,w) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,*) indata(i,:,j)
					write(*,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_logi_dim4_unit(indata,LD1,LD2,LD3,LD4,uni,words)
		integer,intent(in)::LD1,LD2,LD3,LD4,uni
		logical,intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,w) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,*) indata(i,:,k,j)
					write(uni,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine	
	subroutine printDataMAtrix_logi_dim4_no_unit(indata,LD1,LD2,LD3,LD4,words)
		integer,intent(in)::LD1,LD2,LD3,LD4
		logical,intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,w) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,*) indata(i,:,k,j)
					write(*,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine		
	
	
	
!******************  character   ***********************
	subroutine printDataMAtrix_char_dim1_unit(indata,M,uni,words)
		integer,intent(in)::M,uni
		character(len=*),intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		integer::ii
		if(present(words))then
			w='('+M+words+')'
			write (uni,w) (trim(adjustl(indata(ii)))//' ',ii=1,M)
		else
			write (uni,*) (trim(adjustl(indata(ii)))//' ',ii=1,M)
		end if
		return
	end subroutine
	subroutine printDataMAtrix_char_dim1_no_unit(indata,M,words)
		integer,intent(in)::M
		character(len=*),intent(in) ::indata(M)
		CHARACTER(len=*),optional,intent(in)::words
		CHARACTER(len=100)::w
		integer::ii
		if(present(words))then
			w='('+M	+words+')'
			write (*,w) (trim(adjustl(indata(ii)))//' ',ii=1,M)
		else
			write (*,*) (trim(adjustl(indata(ii)))//' ',ii=1,M)
		end if
		return
	end subroutine
	subroutine printDataMAtrix_char_dim2_unit(indata,M,N,uni,words)
		integer,intent(in)::M,N,uni
		character(len=*),intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,ii
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (uni,w) (trim(adjustl(indata(i,ii)))//' ',ii=1,N)
				write(uni,*) ""
			end do 
		else
			do i=1,M
				write (uni,*) (trim(adjustl(indata(i,ii)))//' ',ii=1,N)
				write(uni,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_char_dim2_no_unit(indata,M,N,words)
		integer,intent(in)::M,N
		character(len=*),intent(in) ::indata(M,N)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,ii
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+N+words+')'
			do i=1,M
				write (*,w) (trim(adjustl(indata(i,ii)))//' ',ii=1,N)
				write(*,*) ""
			end do 
		else
			do i=1,M
				write (*,*) (trim(adjustl(indata(i,ii)))//' ',ii=1,N)
				write(*,*) ""
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_char_dim3_unit(indata,LD1,LD2,LD3,uni,words)
		integer,intent(in)::LD1,LD2,LD3,uni
		character(len=*),intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,ii
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,w) (trim(adjustl(indata(i,ii,j)))//' ',ii=1,LD2)
					write(uni,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(uni,*) "(*,*,",j,")"
				do i=1,LD1
					write (uni,*) (trim(adjustl(indata(i,ii,j)))//' ',ii=1,LD2)
					write(uni,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_char_dim3_no_unit(indata,LD1,LD2,LD3,words)
		integer,intent(in)::LD1,LD2,LD3
		character(len=*),intent(in) ::indata(LD1,LD2,LD3)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,ii
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,w) (trim(adjustl(indata(i,ii,j)))//' ',ii=1,LD2)
					write(*,*) ""
				end do
			end do 
		else
			do j=1,LD3
				write(*,*) "(*,*,",j,")"
				do i=1,LD1
					write (*,*) (trim(adjustl(indata(i,ii,j)))//' ',ii=1,LD2)
					write(*,*) ""
				end do
			end do 
		end if
		return
	end subroutine
	subroutine printDataMAtrix_char_dim4_unit(indata,LD1,LD2,LD3,LD4,uni,words)
		integer,intent(in)::LD1,LD2,LD3,LD4,uni
		character(len=*),intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k,ii
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,w) (trim(adjustl(indata(i,ii,k,j)))//' ',ii=1,LD2)
					write(uni,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(uni,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (uni,*) (trim(adjustl(indata(i,ii,k,j)))//' ',ii=1,LD2)
					write(uni,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine	
	subroutine printDataMAtrix_char_dim4_no_unit(indata,LD1,LD2,LD3,LD4,words)
		integer,intent(in)::LD1,LD2,LD3,LD4
		character(len=*),intent(in) ::indata(LD1,LD2,LD3,LD4)
		CHARACTER(len=*),optional,intent(in)::words
		integer::i,j,k,ii
		CHARACTER(len=100)::w
		if(present(words))then
			w='('+LD2+words+')'
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,w) (trim(adjustl(indata(i,ii,k,j)))//' ',ii=1,LD2)
					write(*,*) ""
					end do
				end do 
			end do
		else
			do j=1,LD4
				do k=1,LD3
					write(*,*) "(*,*,",k,j,")"
					do i=1,LD1
					write (*,*) (trim(adjustl(indata(i,ii,k,j)))//' ',ii=1,LD2)
					write(*,*) ""
					end do
				end do 
			end do
		end if
		return
	end subroutine		
	
	
	
	
	
	











end  module
