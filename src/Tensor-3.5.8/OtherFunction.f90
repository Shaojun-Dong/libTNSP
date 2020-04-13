module OtherFunction
	use Tensor_type
	use tools
	use mpi
	implicit none


	interface generate!it cost time
		module procedure generate_NoName1!generate a random Tensor 
		module procedure generate_NoName1_region4
		module procedure generate_NoName1_regioni
		module procedure generate_NoName2
		module procedure generate_NoName2_region4
		module procedure generate_NoName2_regioni
		module procedure generate_NoName3
		module procedure generate_NoName4
		module procedure generate_dim1
		module procedure generate_dim1_region4
		module procedure generate_dim1_regioni
		module procedure generate_dim2
		module procedure generate_dim2_region4
		module procedure generate_dim2_regioni
		module procedure generate_dim3
		module procedure generate_dim4
	end interface

contains

	!*********************  generate *********************
!		generate a Tensor with random number
	type(Tensor) function generate_noName1(Tdim,region,classtype) result (T)
		integer,intent(in) :: Tdim(:)
		real*8,intent(in)::region(2)
		character(len=*),intent(in)::classtype
		call T%allocate(Tdim,classtype)
		call T%random(region)
		return
	end function	
	type(Tensor) function generate_noName1_region4(Tdim,region,classtype) result (T)
		integer,intent(in) :: Tdim(:)
		real*4,intent(in)::region(2)
		character(len=*),intent(in)::classtype
		call T%allocate(Tdim,classtype)
		call T%random(region)
		return
	end function	
	type(Tensor) function generate_noName1_regioni(Tdim,region,classtype) result (T)
		integer,intent(in) :: Tdim(:)
		integer,intent(in)::region(2)
		character(len=*),intent(in)::classtype
		call T%allocate(Tdim,classtype)
		call T%random(region)
		return
	end function	
	type(Tensor) function generate_noName2(Tdim,region) result (T)
		integer,intent(in) :: Tdim(:)
		real*8,intent(in)::region(2)
		call T%allocate(Tdim)
		call T%random(region)
		return
	end function	
	type(Tensor) function generate_noName2_region4(Tdim,region) result (T)
		integer,intent(in) :: Tdim(:)
		real*4,intent(in)::region(2)
		call T%allocate(Tdim)
		call T%random(region)
		return
	end function	
	type(Tensor) function generate_noName2_regioni(Tdim,region) result (T)
		integer,intent(in) :: Tdim(:)
		integer,intent(in)::region(2)
		call T%allocate(Tdim)
		call T%random(region)
		return
	end function	
	type(Tensor) function generate_noName3(Tdim,classtype) result (T)
		integer,intent(in) :: Tdim(:)
		character(len=*),intent(in)::classtype
		call T%allocate(Tdim,classtype)
		call T%random()
		return
	end function
	type(Tensor) function generate_noName4(Tdim) result (T)
		integer,intent(in) :: Tdim(:)
		call T%allocate(Tdim)
		call T%random( )
		return
	end function
	type(Tensor) function generate_dim1(Tdim,region,classtype) result (T)
		type(Dimension),intent(in) :: Tdim
		character(len=*),intent(in)::classtype
		real*8,intent(in)::region(2)
		call T%allocate(Tdim,classtype)
		call T%random(region)
		return
	end function
	type(Tensor) function generate_dim1_region4(Tdim,region,classtype) result (T)
		type(Dimension),intent(in) :: Tdim
		character(len=*),intent(in)::classtype
		real*4,intent(in)::region(2)
		call T%allocate(Tdim,classtype)
		call T%random(region)
		return
	end function
	type(Tensor) function generate_dim1_regioni(Tdim,region,classtype) result (T)
		type(Dimension),intent(in) :: Tdim
		character(len=*),intent(in)::classtype
		integer,intent(in)::region(2)
		call T%allocate(Tdim,classtype)
		call T%random(region)
		return
	end function
	type(Tensor) function generate_dim2_region4(Tdim,region) result (T)
		type(Dimension),intent(in) :: Tdim
		real*4,intent(in)::region(2)
		call T%allocate(Tdim)
		call T%random(region)
		return
	end function
	type(Tensor) function generate_dim2_regioni(Tdim,region) result (T)
		type(Dimension),intent(in) :: Tdim
		integer,intent(in)::region(2)
		call T%allocate(Tdim)
		call T%random(region)
		return
	end function
	type(Tensor) function generate_dim2(Tdim,region) result (T)
		type(Dimension),intent(in) :: Tdim
		real*8,intent(in)::region(2)
		call T%allocate(Tdim)
		call T%random( )
		return
	end function
	type(Tensor) function generate_dim3(Tdim,classtype) result (T)
		type(Dimension),intent(in) :: Tdim
		character(len=*),intent(in)::classtype
		call T%allocate(Tdim,classtype)
		call T%random( )
		return
	end function
	type(Tensor) function generate_dim4(Tdim) result (T)
		type(Dimension),intent(in) :: Tdim
		call T%allocate(Tdim)
		call T%random( )
		return
	end function


	type(Tensor) function resetMatrixDim(A,m,n)
		type(Tensor),intent(in)::A
		integer,intent(in)::m,n
		real*4,pointer::Ap(:,:)
		integer,pointer::idatap(:,:),idatap2(:,:)
		real*4,pointer::sdatap(:,:),sdatap2(:,:)
		real*8,pointer::ddatap(:,:),ddatap2(:,:)
		complex*8,pointer::cdatap(:,:),cdatap2(:,:)
		complex*16,pointer::zdatap(:,:),zdatap2(:,:)
		logical,pointer::ldatap(:,:),ldatap2(:,:)
		character(len=characterlen),pointer::adatap(:,:),adatap2(:,:)
		integer::minm,minn,classType
		if(A%getRank().ne.2)then
			call writemess('resetMatrixDim only work on matrix (rank=2)')
			call error_stop
		end if
		if(.not.A%if_simple_dimension())then
			call writemess('resetMatrixDim only work on simple dimension')
			call error_stop 
		end if
		if(m*n.eq.0)then
			call resetMatrixDim%empty()
			return
		end if
		classType=A%getType()
		call resetMatrixDim%allocate([m,n],classType)
		minm=min(m,A%dim(1))
		minn=min(n,A%dim(2))
		select case (classType)
			case (1)
				call A%pointer(idatap,[1,minm],[1,minn])
				call resetMatrixDim%pointer(idatap2,[1,m],[1,n])
				idatap2(1:minm,1:minn)=idatap
				idatap=>null()
				idatap2=>null()
			case (2)
				call A%pointer(sdatap,[1,minm],[1,minn])
				call resetMatrixDim%pointer(sdatap2,[1,m],[1,n])
				sdatap2(1:minm,1:minn)=sdatap
				sdatap=>null()
				sdatap2=>null()
			case (3)
				call A%pointer(ddatap,[1,minm],[1,minn])
				call resetMatrixDim%pointer(ddatap2,[1,m],[1,n])
				ddatap2(1:minm,1:minn)=ddatap
				ddatap=>null()
				ddatap2=>null()
			case (4)
				call A%pointer(cdatap,[1,minm],[1,minn])
				call resetMatrixDim%pointer(cdatap2,[1,m],[1,n])
				cdatap2(1:minm,1:minn)=cdatap
				cdatap=>null()
				cdatap2=>null()
			case (5)
				call A%pointer(zdatap,[1,minm],[1,minn])
				call resetMatrixDim%pointer(zdatap2,[1,m],[1,n])
				zdatap2(1:minm,1:minn)=zdatap
				zdatap=>null()
				zdatap2=>null()
			case (6)
				call A%pointer(ldatap,[1,minm],[1,minn])
				call resetMatrixDim%pointer(ldatap2,[1,m],[1,n])
				ldatap2(1:minm,1:minn)=ldatap
				ldatap=>null()
				ldatap2=>null()
			case (7)
				call A%pointer(adatap,[1,minm],[1,minn])
				call resetMatrixDim%pointer(adatap2,[1,m],[1,n])
				adatap2(1:minm,1:minn)=adatap
				adatap=>null()
				adatap2=>null()
		end select
		return
	end function



	subroutine ALLREDUCE_Tensor(inTensor,outTensor,OP,ierr,MPIcommon)
		type(Tensor),target,intent(in)::inTensor
		type(Tensor),target,intent(inout)::outTensor
		integer::ierr
		integer,intent(in)::op
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,len2,istatus(MPI_STATUS_SIZE),mpi_comm
		type(Tensor),pointer::p1,p2
		integer::length,classtype
		integer,pointer::idata(:),idata2(:)
		real*4,pointer::sdata(:),sdata2(:)
		real*8,pointer::ddata(:),ddata2(:)
		complex*8,pointer::cdata(:),cdata2(:)
		complex*16,pointer::zdata(:),zdata2(:)
		logical,pointer::ldata(:),ldata2(:)
		character(len=characterlen),pointer::adata(:),adata2(:)
		logical::ALLgoonFlag,goonFlag

		tag=1
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		call mpi_comm_rank(mpi_comm,proID,ierr)
		call mpi_comm_size(mpi_comm,proNum,ierr )
		p1=>inTensor
		p2=>outTensor
		if(associated(p1,p2))then
			call writemess('ERROR in ALLREDUCE_Tensor,input Tensor and output Tensor can not be the same one',-1)
			call error_stop
		end if
		p1=>null()
		p2=>null()
			! if the Tensor is empty
		goonFlag=inTensor%getFlag()
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('ERROR in ALLREDUCE_Tensor,the is no date in one or some Tensors',-1)
			call error_stop
		end if
			! if the Tensor is the same data type
		classtype=inTensor%getType()
		call MPI_BCAST(classtype,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=.true.
		if(inTensor%getType().ne.classtype)goonFlag=.false.
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('ERROR in ALLREDUCE_Tensor,the Data type in the Tensors are not the sames',-1)
			call error_stop
		end if
			! if the length of the Tensor is the same
		length=inTensor%getTotalData()
		call MPI_BCAST(length,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=.true.
		if(inTensor%getTotalData().ne.length)goonFlag=.false.
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('ERROR in ALLREDUCE_Tensor,the length od the Tensor is not the same',-1)
			call error_stop
		end if
		call outTensor%empty()
		call outTensor%allocate(inTensor)

		select case(inTensor%getType())
			case(1)
				call inTensor%pointer(idata)
				call outTensor%pointer(idata2)
				call MPI_ALLREDUCE(idata,idata2,length,MPI_INTEGER,OP,mpi_comm,ierr)
			case(2)
				call inTensor%pointer(sdata)
				call outTensor%pointer(sdata2)
				call MPI_ALLREDUCE(sdata,sdata2,length,MPI_real,OP,mpi_comm,ierr)
			case(3)
				call inTensor%pointer(ddata)
				call outTensor%pointer(ddata2)
				call MPI_ALLREDUCE(ddata,ddata2,length,MPI_double_precision,OP,mpi_comm,ierr)
			case(4)
				call inTensor%pointer(cdata)
				call outTensor%pointer(cdata2)
				call MPI_ALLREDUCE(cdata,cdata2,length,MPI_complex,OP,mpi_comm,ierr)
			case(5)
				call inTensor%pointer(zdata)
				call outTensor%pointer(zdata2)
				call MPI_ALLREDUCE(zdata,zdata2,length,MPI_double_complex,OP,mpi_comm,ierr)
			case(6)
				call inTensor%pointer(ldata)
				call outTensor%pointer(ldata2)
				call MPI_ALLREDUCE(ldata,ldata2,length,MPI_logical,OP,mpi_comm,ierr)
			case(7)
				call inTensor%pointer(adata)
				call outTensor%pointer(adata2)
				call MPI_ALLREDUCE(adata,adata2,length,MPI_character,OP,mpi_comm,ierr)
		end  select
		return
	end subroutine


	subroutine REDUCE_Tensor(inTensor,outTensor,OP,root,ierr,MPIcommon)
		type(Tensor),target,intent(in)::inTensor
		type(Tensor),target,intent(inout)::outTensor
		integer,intent(in)::root
		integer::ierr
		integer,intent(in)::op
		integer,optional,intent(in)::MPIcommon
		integer::proID,proNum,tag,len1,len2,istatus(MPI_STATUS_SIZE),mpi_comm
		type(Tensor),pointer::p1,p2
		integer::length,classtype
		integer,pointer::idata(:),idata2(:)
		real*4,pointer::sdata(:),sdata2(:)
		real*8,pointer::ddata(:),ddata2(:)
		complex*8,pointer::cdata(:),cdata2(:)
		complex*16,pointer::zdata(:),zdata2(:)
		logical,pointer::ldata(:),ldata2(:)
		character(len=characterlen),pointer::adata(:),adata2(:)
		logical::ALLgoonFlag,goonFlag

		tag=1
		if(present(MPIcommon))then
			mpi_comm=MPIcommon
		else
			mpi_comm=mpi_comm_world
		end if
		call mpi_comm_rank(mpi_comm,proID,ierr)
		call mpi_comm_size(mpi_comm,proNum,ierr )
		p1=>inTensor
		p2=>outTensor
		if(associated(p1,p2))then
			call writemess('ERROR in REDUCE_Tensor,input Tensor and output Tensor can not be the same one',-1)
			call error_stop
		end if
		p1=>null()
		p2=>null()
			! if the Tensor is empty
		goonFlag=inTensor%getFlag()
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('ERROR in REDUCE_Tensor,the is no date in one or some Tensors',-1)
			call error_stop
		end if
			! if the Tensor is the same data type
		classtype=inTensor%getType()
		call MPI_BCAST(classtype,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=.true.
		if(inTensor%getType().ne.classtype)goonFlag=.false.
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('ERROR in REDUCE_Tensor,the Data type in the Tensors are not the sames',-1)
			call error_stop
		end if
			! if the length of the Tensor is the same
		length=inTensor%getTotalData()
		call MPI_BCAST(length,1,MPI_integer,0,mpi_comm,ierr)
		goonFlag=.true.
		if(inTensor%getTotalData().ne.length)goonFlag=.false.
		call MPI_ALLREDUCE(goonFlag,ALLgoonFlag,1,MPI_logical,MPI_LAND,mpi_comm,ierr)
		if(.not.ALLgoonFlag)then
			call writemess('ERROR in REDUCE_Tensor,the length od the Tensor is not the same',-1)
			call error_stop
		end if
		call outTensor%empty()
		call outTensor%allocate(inTensor)

		select case(inTensor%getType())
			case(1)
				call inTensor%pointer(idata)
				call outTensor%pointer(idata2)
				call MPI_REDUCE(idata,idata2,length,MPI_INTEGER,OP,root,mpi_comm,ierr)
			case(2)
				call inTensor%pointer(sdata)
				call outTensor%pointer(sdata2)
				call MPI_REDUCE(sdata,sdata2,length,MPI_real,OP,root,mpi_comm,ierr)
			case(3)
				call inTensor%pointer(ddata)
				call outTensor%pointer(ddata2)
				call MPI_REDUCE(ddata,ddata2,length,MPI_double_precision,OP,root,mpi_comm,ierr)
			case(4)
				call inTensor%pointer(cdata)
				call outTensor%pointer(cdata2)
				call MPI_REDUCE(cdata,cdata2,length,MPI_complex,OP,root,mpi_comm,ierr)
			case(5)
				call inTensor%pointer(zdata)
				call outTensor%pointer(zdata2)
				call MPI_REDUCE(zdata,zdata2,length,MPI_double_complex,OP,root,mpi_comm,ierr)
			case(6)
				call inTensor%pointer(ldata)
				call outTensor%pointer(ldata2)
				call MPI_REDUCE(ldata,ldata2,length,MPI_logical,OP,root,mpi_comm,ierr)
			case(7)
				call inTensor%pointer(adata)
				call outTensor%pointer(adata2)
				call MPI_REDUCE(adata,adata2,length,MPI_character,OP,root,mpi_comm,ierr)
		end  select
		return
	end subroutine

end module