
! contractRank23leg13
! contractRank32leg11

module Contract_real8_Tools
	implicit none
contains
	!*********************************************
	!
	!   rank 1 tensor and rank 0 tensor 
	!
	!*********************************************

	subroutine contractScal(T,scal,Maxi) 
		integer::Maxi,maxj
		real*8::T(Maxi)
		real*8 :: scal
		call dscal (Maxi,scal, T, 1)
		return
	end subroutine


	!*********************************************
	!
	!   rank 1 tensor and rank 1 tensor 
	!
	!*********************************************

	subroutine contractRank11leg11(T,A,B,Maxi) 
		integer::Maxi,maxj
		real*8::T(1)
		real*8 :: A(Maxi),B(Maxi)
		real*8,External::	ddot
		T(1)=ddot(Maxi, A, 1, B, 1)
		return
	end subroutine

	!*********************************************
	!
	!   rank 2 tensor and rank 1 tensor 
	!
	!*********************************************

	! T[j]=A[i,j]*B[i]

	subroutine contractRank21leg11(T,A,B,Maxi,maxj) 
		integer::Maxi,maxj
		real*8::T(Maxj)
		real*8 :: A(Maxi,Maxj),B(Maxi)
		call DGEMV('T',Maxi,Maxj,1d0,A,Maxi,B,1,0d0,T,1)
		return
	end subroutine

	! T[i]=A[i,j]*B[j]

	subroutine contractRank21leg21(T,A,B,Maxi,maxj) 
		integer::Maxi,maxj
		real*8::T(Maxi)
		real*8 :: A(Maxi,Maxj),B(Maxj)
		call DGEMV('N',Maxi,Maxj,1d0,A,Maxi,B,1,0d0,T,1)
		return
	end subroutine


	! T[j]=A[i]*B[i,j]

	subroutine contractRank12leg11(T,A,B,Maxi,maxj) 
		integer::Maxi,maxj
		real*8::T(Maxj)
		real*8 :: A(Maxi),B(Maxi,maxj)
		call DGEMV('T',Maxi,Maxj,1d0,B,Maxi,A,1,0d0,T,1)
		return
	end subroutine


	! T[i]=A[j]*B[i,j]

	subroutine contractRank12leg12(T,A,B,Maxi,maxj) 
		integer::Maxi,maxj
		real*8::T(Maxi)
		real*8 :: A(Maxj),B(Maxi,Maxj)
		call DGEMV('N',Maxi,Maxj,1d0,B,Maxi,A,1,0d0,T,1)
		return
	end subroutine


	!*********************************************
	!
	!   rank 2 tensor and rank 2 tensor 
	!
	!*********************************************

	! T[j,k]=A[i,j]*B[i,k]

	subroutine contractRank22leg11(T,A,B,Maxi,maxj,maxk) 
		integer::Maxi,maxj,maxk
		real*8::T(Maxj,Maxk)
		real*8 :: A(Maxi,Maxj),B(Maxi,Maxk)
		call DGEMM('T','N',Maxj,Maxk,Maxi,1d0,A,Maxi,B,Maxi,0d0,T,Maxj)
		return
	end subroutine

	! T[j,k]=A[i,j]*B[k,i]

	subroutine contractRank22leg12(T,A,B,Maxi,maxj,maxk) 
		integer::Maxi,maxj,maxk
		real*8::T(Maxj,Maxk)
		real*8 :: A(Maxi,Maxj),B(Maxk,Maxi)
		call DGEMM('T','T',Maxj,Maxk,Maxi,1d0,A,Maxi,B,Maxk,0d0,T,Maxj)
		return
	end subroutine

	! T[i,k]=A[i,j]*B[j,k]

	subroutine contractRank22leg21(T,A,B,Maxi,maxj,maxk) 
		integer::Maxi,maxj,maxk
		real*8::T(Maxi,Maxk)
		real*8 :: A(Maxi,Maxj),B(Maxj,Maxk)
		call DGEMM('N','N',Maxi,Maxk,Maxj,1d0,A,Maxi,B,Maxj,0d0,T,Maxi)
		return
	end subroutine

	! T[i,k]=A[i,j]*B[k,j]

	subroutine contractRank22leg22(T,A,B,Maxi,maxj,maxk) 
		integer::Maxi,maxj,maxk
		real*8::T(Maxi,Maxk)
		real*8 :: A(Maxi,Maxj),B(Maxk,Maxj)
		call DGEMM('N','T',Maxi,Maxk,Maxj,1d0,A,Maxi,B,Maxk,0d0,T,Maxi)
		return
	end subroutine




	!*********************************************
	!
	!   rank 3 tensor and rank 2 tensor 
	!
	!*********************************************





	! T[i,k,l]=A[i,j,k]*B[j,l]

	subroutine contractRank32leg21(T,A,B,Maxi,maxj,maxk,maxl) 
		integer::Maxi,maxj,maxk,maxl
		real*8::T(Maxi,Maxk,Maxl)
		real*8 :: A(Maxi,Maxj,Maxk),B(Maxj,Maxl)
		real*8 ::temp(Maxj,Maxi,Maxk)
		integer::k,IK,IJ
		IJ=Maxi*Maxj
		do k=1,Maxk
			call dcopy(IJ,transpose(A(:,:,k)),1,temp(:,:,k),1)
		end do
		IK=Maxi*Maxk
		call DGEMM('T','N',IK,Maxl,Maxj,1d0,temp,Maxj,B,Maxj,0d0,T,IK)
		return
	end subroutine

	subroutine contractRank32leg21__(T,A,B,Maxi,maxj,maxk,maxl) 
		integer::Maxi,maxj,maxk,maxl
		real*8::T(Maxi,Maxk,Maxl)
		real*8 :: A(Maxi,Maxj,Maxk),B(Maxj,Maxl)
		real*8 ::temp(Maxi,Maxk,Maxj)
		integer::k,IK,j
		do k=1,Maxk
			do j=1,Maxj
				call dcopy(Maxi,A(:,j,k),1,temp(:,k,j),1)
			end do
		end do
		IK=Maxi*Maxk
		call DGEMM('N','N',IK,Maxl,Maxj,1d0,temp,IK,B,Maxj,0d0,T,IK)
		return
	end subroutine

	! T[l,i,k]=A[j,l]*B[i,j,k]

	subroutine contractRank23leg12(T,A,B,Maxi,maxj,maxk,maxl) 
		integer::Maxi,maxj,maxk,maxl
		real*8::T(Maxl,Maxi,Maxk)
		real*8 :: A(Maxj,Maxl),B(Maxi,Maxj,Maxk)
		integer::k
		do k=1,Maxk
			call DGEMM('T','T',Maxl,Maxi,Maxj,1d0,A,Maxj,B(:,:,k),Maxi,0d0,T(:,:,k),Maxl)
		end do
		return
	end subroutine


	!************************

	! T[i,k,l]=A[i,j,k]*B[l,j]

	subroutine contractRank32leg22__(T,A,B,Maxi,maxj,maxk,maxl) 
		integer::Maxi,maxj,maxk,maxl
		real*8::T(Maxi,Maxk,Maxl)
		real*8 :: A(Maxi,Maxj,Maxk),B(Maxl,Maxj)
		integer::k
		do k=1,Maxk
			call DGEMM('N','T',Maxi,Maxl,Maxj,1d0,A(:,:,k),Maxi,B,Maxl,0d0,T(:,k,:),Maxi)
		end do
		return
	end subroutine
	subroutine contractRank32leg22(T,A,B,Maxi,maxj,maxk,maxl) 
		integer::Maxi,maxj,maxk,maxl
		real*8::T(Maxi,Maxk,Maxl)
		real*8 :: A(Maxi,Maxj,Maxk),B(Maxl,Maxj)
		real*8 :: temp(Maxj,Maxi,Maxk)
		integer::k,IK,IJ
		IJ=Maxi*Maxj
		IK=Maxi*Maxk
		do k=1,Maxk
			call dcopy(IJ,transpose(A(:,:,k)),1,temp(:,:,k),1)
		end do
		call DGEMM('T','T',IK,Maxl,Maxj,1d0,temp,Maxj,B,Maxl,0d0,T,IK)
		return
	end subroutine

	! T[i,l,k]=A[i,j,k]*B[l,j]

	subroutine contractRank32leg22_(T,A,B,Maxi,maxj,maxk,maxl) 
		integer::Maxi,maxj,maxk,maxl
		real*8::T(Maxi,Maxl,Maxk)
		real*8 :: A(Maxi,Maxj,Maxk),B(Maxl,Maxj)
		integer::k
		do k=1,Maxk
			call DGEMM('N','T',Maxi,Maxl,Maxj,1d0,A(:,:,k),Maxi,B,Maxl,0d0,T(:,:,k),Maxi)
		end do
		return
	end subroutine


	! T[l,i,k]=A[l,j]*B[i,j,k]

	subroutine contractRank23leg22(T,A,B,Maxi,maxj,maxk,maxl) 
		integer::Maxi,maxj,maxk,maxl
		real*8::T(Maxl,Maxi,Maxk)
		real*8 :: A(Maxl,Maxj),B(Maxi,Maxj,Maxk)
		integer::k
		do k=1,Maxk
			call DGEMM('N','T',Maxl,Maxi,Maxj,1d0,A,Maxl,B(:,:,k),Maxi,0d0,T(:,:,k),Maxl)
		end do
		return
	end subroutine




	!*********************************************
	!
	!   rank 3 tensor and rank 3 tensor 
	!
	!*********************************************




	! T[i,k,l,m]=A[i,j,k]*B[l,j,m]

	subroutine contractRank33leg22__(T,A,B,Maxi,maxj,maxk,maxl,maxm) 
		integer::Maxi,maxj,maxk,maxl,maxm
		real*8::T(Maxi,Maxk,maxl,Maxm)
		real*8 :: A(Maxi,Maxj,Maxk),B(Maxl,Maxj,Maxm)
		integer::k,m
		do m=1,Maxm
			do k=1,Maxk
				call DGEMM('N','T',Maxi,Maxl,Maxj,1d0,A(:,:,k),Maxi,B(:,:,m),Maxl,0d0,T(:,k,:,m),Maxi)
			end do
		end do
		return
	end subroutine

	! T[i,k,l,m]=A[i,j,k]*B[l,j,m]

	subroutine contractRank33leg22(T,A,B,Maxi,maxj,maxk,maxl,maxm) 
		integer::Maxi,maxj,maxk,maxl,maxm
		real*8::T(Maxi,Maxk,maxl,Maxm)
		real*8 :: A(Maxi,Maxj,Maxk),B(Maxl,Maxj,Maxm)
		real*8 :: temp(Maxj,Maxi,Maxk)
		integer::k,m,IJ,IK
		IK=Maxi*Maxk
		IJ=Maxi*Maxj
		do k=1,Maxk
			call dcopy(IJ,transpose(A(:,:,k)),1,temp(:,:,k),1)
		end do
		do m=1,Maxm
			call DGEMM('T','T',IK,Maxl,Maxj,1d0,temp,Maxj,B(:,:,m),Maxl,0d0,T(:,:,:,m),IK)
		end do
		return
	end subroutine


	! T[i,l,m,k]=A[i,j,k]*B[l,j,m]

	subroutine contractRank33leg22_(T,A,B,Maxi,maxj,maxk,maxl,maxm) 
		integer::Maxi,maxj,maxk,maxl,maxm
		real*8::T(Maxi,Maxl,maxm,Maxk)
		real*8 :: A(Maxi,Maxj,Maxk),B(Maxl,Maxj,Maxm)
		integer::k,m
		do m=1,Maxm
			do k=1,Maxk
				call DGEMM('N','T',Maxi,Maxl,Maxj,1d0,A(:,:,k),Maxi,B(:,:,m),Maxl,0d0,T(:,:,k,m),Maxi)
			end do
		end do
		return
	end subroutine




	



end module