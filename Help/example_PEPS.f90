module example_PEPS
	use Tensor_type
	use usefull_function
	implicit none
	character(len=20)::datatype='real'
	type Node
		type(Tensor)::site
		type(Tensor)::Up
		type(Tensor)::Down
		type(Tensor)::Left
		type(Tensor)::Right
	end type Node
contains
	subroutine initialH(H)
		type(Tensor),intent(inout)::H
		type(Tensor)::Sx,Sy,Sz
		call pauli_matrix(Sx,Sy,Sz,0.5)
		call H%setType(datatype)
		H=(Sx.xx.Sx)+(Sy.xx.Sy)+(Sz.xx.Sz)
		call H%setName('H')
		return
	end subroutine
	type(Tensor) function expmTensor(H,tau)
		type(Tensor),intent(in)::H
		real,intent(in)::tau
		expmTensor=H*tau
		call expmTensor%fuse(1,2)
		call expmTensor%fuse(2,3)
		expmTensor=expm(expmTensor)
		call expmTensor%split()
		return
	end function
	subroutine initialPEPS(A,L1,L2,D)
		type(Node),allocatable,intent(inout)::A(:,:)
		integer,intent(in)::L1,L2,D
		integer::i,j
		allocate(A(L1,L2))
		do j=1,L2
			do i=1,L1
				A(i,j)%site=generate([D,D,D,D,2],datatype)
				call A(i,j)%site%setName(1,'A'+i+'_'+j+'.Left')
				call A(i,j)%site%setName(2,'A'+i+'_'+j+'.Down')
				call A(i,j)%site%setName(3,'A'+i+'_'+j+'.Right')
				call A(i,j)%site%setName(4,'A'+i+'_'+j+'.Up')
				call A(i,j)%site%setName(5,'A'+i+'_'+j+'.phy')
				call A(i,j)%Up%allocate([D,D],datatype)
				call A(i,j)%Up%eye()
				call A(i,j)%Up%setName(1,'Lambda.Leg1')
				call A(i,j)%Up%setName(2,'Lambda.Leg2')
				A(i,j)%Down=A(i,j)%Up
				A(i,j)%Left=A(i,j)%Up
				A(i,j)%Right=A(i,j)%Up
			end do
		end do
		return
	end subroutine
	subroutine step(A,Lambda,B,expmH,CutOff,AName,BName,cha1,cha2)
		type(Tensor),intent(inout)::A,Lambda,B
		type(Tensor),intent(in)::expmH
		character(len=*),intent(in)::cha1,cha2,AName,BName
		integer,intent(in)::CutOff
		type(Tensor)::C,SVD(3)
		C=contract(A,AName+'.'+cha1,Lambda%invTensor(),'Lambda.Leg1')
		C=contract(C,'Lambda.Leg2',B,BName+'.'+cha2)
		C=contract(C,[AName+'.phy',BName+'.phy'],expmH,['H.1','H.2'])
		call C%setName('H.3',AName+'.phy')
		call C%setName('H.4',BName+'.phy')
		SVD=C%SVDTensor(AName,BName,CutOff)
		Lambda=eye(SVD(2))/SVD(2)%smax()
		A=SVD(1)*Lambda
		call A%setName(A%getRank(),AName+'.'+cha1)
		B=Lambda*SVD(3)
		call B%setName(1,BName+'.'+cha2)
		call Lambda%setName(1,'Lambda.Leg1')
		call Lambda%setName(2,'Lambda.Leg2')
		return
	end subroutine	
	subroutine sampleUpdate(A,expmH,L1,L2,CutOff)
		type(Node),intent(inout)::A(:,:)
		type(Tensor),intent(in)::expmH
		integer,intent(in)::L1,L2,CutOff
		integer::i,j
		do i=1,L1
			do j=1,L2-1
				call step(A(i,j)%Site,A(i,j)%Right,A(i,j+1)%Site,expmH,CutOff,'A'+i+'_'+j,'A'+i+'_'+(j+1),'Right','Left')
				A(i,j+1)%Left=A(i,j)%Right
			end do
			call step(A(i,L2)%Site,A(i,L2)%Right,A(i,1)%Site,expmH,CutOff,'A'+i+'_'+L2,'A'+i+'_1','Right','Left')
			A(i,1)%Left=A(i,L2)%Right
		end do
		do j=1,L2
			do i=1,L1-1
				call step(A(i,j)%Site,A(i,j)%Down,A(i+1,j)%Site,expmH,CutOff,'A'+i+'_'+j,'A'+(i+1)+'_'+j,'Down','Up')
				A(i+1,j)%Up=A(i,j)%Down
			end do
			call step(A(L1,j)%Site,A(L1,j)%Down,A(1,j)%Site,expmH,CutOff,'A'+L1+'_'+j,'A1_'+j,'Down','Up')
			A(1,j)%Up=A(L1,j)%Down
		end do
		return
	end subroutine
	subroutine PEPSrun()
		integer::L1,L2,D,i,runningnum
		real::tau
		type(Node),allocatable::A(:,:)
		type(Tensor)::H,expmH
		L1=4
		L2=4
		D=4
		tau=0.1
		runningnum=10
		call initialPEPS(A,L1,L2,D)
		call initialH(H)
		expmH=expmTensor(H,tau)
		do i=1,runningnum
			call sampleUpdate(A,expmH,L1,L2,D)
		end do
		return
	end subroutine

end module
