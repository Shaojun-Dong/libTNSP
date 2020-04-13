!    This  is a example of eig with a input external subroutine
!    The Hamiltonian is contains only two-bodies operator, For example our model
!  is H=\sum_{<i,j>} H_{i,j}, where H_{i,j} is the interaction between the ith and 
!  the jth partical.
!     One should write the part of |Phi'>=H*|Phi>, return |Phi'>. Suppose |Phi>=\sum_{s1,s2,..sn} C_{s1,s2,..sn}|s1,s2,..sn>
!   
!    H*|Phi>=\sum_{<i,j>} H_{i,j} *  \sum_{s1,s2,..sn} C_{s1,s2,..sn}|s1,s2,..sn>
!          = \sum_{s1,s2,..sn} C_{s1,s2,..sn} \sum_{<i,j>} H_{i,j} |s1,s2,..sn>
!
!            H_{i,j} only act on the physical degree of s_i and s_j, and suppose the matrix element of
!            H_{i,j} is <si,sj|H_{i,j}|si',sj'> |si,sj> <si',sj'|
!
!    H*|Phi>= \sum_{<i,j>} (  \sum_{s1,s2,..sn} <si,sj|H_{i,j}|si',sj'> C_{s1,s2,..,s_i,..,s_j,..,sn} |si,sj> <si',sj'||s1,s2,..sn>)
!           =\sum_{<i,j>} ( \sum_{si',sj'} <si,sj|H_{i,j}|si',sj'> C_{s1,s2,..,si',..,sj',..,sn} |s1,s2,..sn>)
!  
!          where <si,sj|H_{i,j}|si',sj'> C_{s1,s2,..,si',..,sj',..,sn} is a 4*4 Tensor contract with a 2^N Tensor C_{s1,s2,..,si',..,sj',..,sn}
!          on the leg of si' sj'
!
!   In code
!           subroutine Hij_phi is to calculate <si,sj|H_{i,j}|si',sj'> C_{s1,s2,..,si',..,sj',..,sn}
!           subroutine H_phi is to calculate \sum_{<i,j>} Hij_phi. This is the result of H*|Phi>
!           subroutine initial_state is to inital a random state in the begin of the H_phi.
!           function initial_H is to output the two-bodies Hamiltonian
!  
!  The graphical  notation of subroutine Hij_phi is
!                                  ___
!                          s1  ___|   |
!                          s2  ___|   |
!                              ___|   |
!                         si'_____|   |  
!         ___        _______/  ___|   |
!    ____|   |______/          ___|   |
!   si   | H |  si'            ___|Phi|
!    ____|   |______           ___|   |
!    sj  |___|  sj' \_______   ___|   |
!                           \_____|   |
!                          sj' ___|   |
!                              ___|   |
!                              ___|   |
!                              ___|   |
!                         sn   ___|___|
module test_example
	use Tensor_Type
	use eigen_value
	implicit none
	character*20,private::classtype='real*8'
contains
	type(Tensor) function initial_H(Jij)result(H)
	 	type(Tensor)::Jij
	 	type(Tensor)::Sx,Sy,Sz
		call H%setType(classtype)
		call pauli_matrix(Sx,Sy,Sz,0.5d0)
		H=(Sx.xx.Sx)+(Sy.xx.Sy)+(Sz.xx.Sz)
		H=Jij*H
		return
	end function
	subroutine initial_state(state,allname,L1,L2)
	!allname is a array of character, it store the order of the dimension of the state,incase that the contract will reorder the dimension.
		type(Tensor),intent(inout)::state
		integer,intent(in)::L1,L2
		character(len=20),allocatable,intent(inout)::allname(:)
		integer,allocatable::dimendata(:)
		integer::k,i,j
		allocate(dimendata(L1*L2))
		if(allocated(allname))deallocate(allname)
		allocate(allname(L1*L2))
		do i=1,L1*L2
			dimendata(i)=2
		end do
		call state%allocate(dimendata,classtype)
		k=1
		do i=1,L1
			do j=1,L2
				call state%setName(k,'A.'+i+'_'+j)
				allname(k)='A.'+i+'_'+j
				k=k+1
			end do
		end do
		return
	end subroutine
! return H_{i,j}*|phi>, H_{i,j} is a two-bodies Hamiltonian
	subroutine Hij_phi(state,site1,site2,allname,H)
		type(Tensor),intent(inout)::state
		type(Tensor),intent(in)::H
		integer,intent(in)::site1(2),site2(2)
		character(len=*)::allname(:)
		character*10::name1,name2
		name1='A.'+site1(1)+'_'+site1(2)
		name2='A.'+site2(1)+'_'+site2(2)
		state=contract(H,(/3,4/),state,(/name1,name2/))
		call setTensorName(state,1,name1)
		call setTensorName(state,2,name2)
		call state%permute(allname)!the contract will reorder the dimension, so one should be order the dimension back to its oringinal order.
		return
	end subroutine	
! return \sum_{<i,j> }H_{i,j}*|phi>, where <i,j> is the neareset neighoor in L1*L2 lattice
! The Hamiltonian is H=\sum_{<i,j> }H_{i,j}
	subroutine  H_phi(parameter,state)
		type(Tensor),intent(in)::parameter(2)
		                        !parameter(1) is a Tensor that store the size of the system: L1 and L2
		                        !parameter(2) is the two-bodies Hamiltonian of \vec{S_i}*\vec{S_j}
		type(Tensor),intent(inout)::state
		character(len=20),allocatable,save::allname(:)
		type(Tensor)::H,statei,sumState
		integer::i,j,k,L1,L2
		L1=parameter(1).ii.1
		L2=parameter(1).ii.2
		if(.not.state%getflag())then
			call initial_state(state,allname,L1,L2)
			return
		end if
		do i=1,L1
			do j=1,L2-1
					statei=state
					call Hij_phi(statei,(/i,j/),(/i,j+1/),allname,parameter(2))
					if(sumState%getFlag())then
						sumState=sumState+statei
					else
						sumState=statei
					end if
			end do
		end do
		do i=1,L1-1
			do j=1,L2
				statei=state
				call Hij_phi(statei,(/i,j/),(/i+1,j/),allname,parameter(2))
				sumState=sumState+statei
			end do
		end do
		state=sumState
		return
	end subroutine
end module

program aaa
	use test_example
	use Tensor_type
	use eigen_value
	implicit none
	type(Tensor)::H,Jij,parameter(2),eig(2)
	type(eigenvalue)::eiger
	real*8::s0
	integer::L1,L2
	Jij=1d0
	L1=3
	L2=3
	H=initial_H(Jij)
	parameter(1)=(/L1,L2/)
	parameter(2)=H
	eig=eiger%eig(H_phi,parameter,'SR',3)
	eig(1)=dble(eig(1))
	call eig(1)%print("min Eigvalues are")
	s0=-3.76
	call eiger%set_ncv(10)
	call eiger%set_tol(1d-10)
	eig=eiger%eig(H_phi,parameter,s0,2)
	call eig(1)%print("Eigvalues that near s0="+s0)
	stop
end 
