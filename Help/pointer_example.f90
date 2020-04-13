module example_func
	use Tensor_Type
	implicit none
contains
	subroutine QRdecomposition1(T,Q,R) ! I copy this from Tensor.f90, Change a little, see the NOTE
		class(Tensor),intent(in)::T
		type(Tensor),intent(inout)::Q,R
		Type(Tensor)::v,vv,identity,Tau
		type(Dimension)::dimen
		integer :: i,j,M,N,min_MN,classtype,INFO
		if(T%getRank().ne.2) then
			write(*,*)"ERROR in QR decomposition"
			write(*,*)"input Tensor should be a matrix"
			call T%diminfo()
			call error_stop()
		endif
		M = T.dim.1
		N = T.dim.2
		min_MN=min(M,N)
		INFO=999
		classtype=max(2,T%getType())
		call Q%empty()
		call R%empty()
		call R%setType(classtype)
		R=T
		call Tau%allocate((/min_MN/),classtype)
		call TData_QR(Tau,R,M,N,INFO)! NOTE:I change here
		if(info.ne.0) then
			call writemess('Error in QR decomposition ,info='+info,-1)
			call writemess('output The data in ./_QR_ERROR_LOG.err',-1)
			open(unit=9991,file='./_QR_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
			call T%writeinfo('The Matrix in QR',9991)
			close(9991)
			call error_stop()
		end if
		
		call v%setType(classtype)
		call vv%setType(classtype)
		call identity%setType(classtype)
		
		if((M.lt.N)) then
			!compute Q
			v=zeroTen((/M/))
			identity=eye(M,M)
			do i=1,min_MN
				if(i.ne.1)then
					call v%setValue(i-1,0)
				end if
				call v%setValue(i,1)
				do j=i+1,M
					call v%setValue((/j/),R.i.(/j,i/))
				end do
				vv=tau%i(i)*(v.xx.(.h.v))
				if(i.eq.1)then
					Q=identity-vv
				else
					Q=Q*(identity-vv)
				end if
			end do
			Q=Q%subTensor((/-2,1,min_MN/))
			dimen=(T.subdim.1)+(/min_MN/)! NOTE:I change here (.subdim.)
			call Q%resetdim(dimen)! NOTE:I change here (Q%TenDim=dimen)
			!compute R
			do i=1,min_MN
				do j=1,i-1
					call R%setvalue((/i,j/),0)
				end do
			end do
			R=R%subTensor((/-1,1,min_MN/))
			dimen=(/min_MN/)+(T.subdim.2)! NOTE:I change here (.subdim.)
			call R%resetdim(dimen)! NOTE:I change here (R%TenDim=dimen)
		else
			!compute R
			Q=R
			do i=1,min_MN
				do j=1,i-1
					call R%setvalue((/i,j/),0)
				end do
			end do
			R=R%subTensor((/-1,1,min_MN/))
			dimen=(/min_MN/)+(T.subdim.2)! NOTE:I change here (.subdim.)
			call R%resetdim(dimen)! NOTE:I change here (R%TenDim=dimen)
			!compute Q
			INFO=999
			call TData_ORGQR(Tau,Q,M,N,min_MN,INFO)! NOTE:I change here
			dimen=(T.subdim.1)+(/min_MN/)
			call Q%resetdim(dimen)
			if(info.ne.0) then
				call writemess('Error in QR decomposition ,info='+info,-1)
				call writemess('output The data in ./_QR_ERROR_LOG.err',-1)
				open(unit=9991,file='./_QR_ERROR_LOG.err',STATUS='replace',POSITION='APPEND')
				call T%writeinfo('The Matrix in QR',9991)
				call Q%writeinfo('The Q Matrix in QR',9991)
				call R%writeinfo('The R Matrix in QR',9991)
				close(9991)
				call error_stop()
			end if
		end if
		RETURN
	end subroutine
	
	subroutine TData_QR(outTau,inoutTensor,M,N,INFO)!rewrite this part
		type(Tensor),intent(inout)::outTau,inoutTensor
		integer,intent(in)::M,N
		integer,intent(inout)::INFO
		complex(kind=8),allocatable::zwork(:)
		complex(kind=8),pointer::TauData(:),inoutTensorData(:,:)
		integer::LWORK
		LWORK=2*max(M,N)
		call outTau%pointer(TauData)!If outTau not a Tensor of complex(kind=8), program will tell you error
		call inoutTensor%pointer(inoutTensorData)!If outTau not a Tensor of complex(kind=8), program will tell you error
		!now TauData is the data in outTau
		allocate(zwork(LWORK))
		call ZGEQRF( M, N, inoutTensorData, M, TauData, zWORK, LWORK, INFO )
		return
	end subroutine
	
	subroutine TData_ORGQR(Tau,T,M,N,minMN,INFO)!rewrite this part
		type(Tensor),intent(inout)::T,Tau
		integer,intent(in)::M,N,minMN
		integer,intent(inout)::INFO
		complex(kind=8),allocatable::zwork(:)!If outTau not a Tensor of complex(kind=8), program will tell you error
		complex(kind=8),pointer::TauData(:),TData(:,:)!If outTau not a Tensor of complex(kind=8), program will tell you error
		integer::LWORK
		LWORK=2*N
		call Tau%pointer(TauData)
		call T%pointer(TData)
		allocate(zwork(LWORK))
		call ZUNGQR( M, N, minMN, TData, M, TauData, zwork, LWORK, INFO )
		return
	end subroutine
end module


program aaa
	use Tensor_type
	use example_func
	implicit none
	type(Tensor)::A,B,QR(2),Q,R
	call A%allocate([4,2],'complex(kind=8)')
	call A%random([-1d0,1d0])
	QR=A%QRTensor()
	call QRdecomposition1(A,Q,R) 
	
	call QR(1)%print('The Data in Q, from QRTensor')
	call Q%print('The Data in Q, from the function of example_func')
	call writemess(" ")
	call writemess(" ")
	call writemess("%%%%%%%%%%%%%%%%%%%%%%%%")
	call writemess(" ")
	call writemess(" ")
	call QR(2)%print('The Data in R, from QRTensor')
	call R%print('The Data in R, from the function of example_func')
	
	
end 















