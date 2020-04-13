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
	type(Tensor) function initial_H()result(H)
	 	type(Tensor)::Sx,Sy,Sz,Sx2,Sy2,Sz2
		call H%setType(classtype)
		call pauli_matrix(Sx,Sy,Sz,0.5d0)
		call Sx%setName(1,'H1.n')
		call Sx%setName(2,'H1.n2')
		call Sy%setName(1,'H1.n')
		call Sy%setName(2,'H1.n2')
		call Sz%setName(1,'H1.n')
		call Sz%setName(2,'H1.n2')

		Sx2=Sx
		call Sx2%setName('H2')
		Sy2=Sy
		call Sy2%setName('H2')
		Sz2=Sz
		call Sz2%setName('H2')

		H=(Sx.kron.Sx2)+(Sy.kron.Sy2)+(Sz.kron.Sz2)
		!H is a four-legs tensor, by reading the name of the dimension we know the meanning of every legs. 
		return
	end function

	type(Tensor) function initial_Field(field)result(H)
	 	real*8::field
	 	type(Tensor)::Sx,Sy,Sz
		call H%setType(classtype)
		call pauli_matrix(Sx,Sy,Sz,0.5d0)
		call Sz%setName(1,'H1.n')
		call Sz%setName(2,'H1.n2')

		H=field*Sz
		return
	end function

	!allname is a array of character, it store the order of the dimension of the state,incase that the contract will reorder the dimension.

	subroutine initial_state(state,L1,L2)
		type(Tensor),intent(inout)::state
		integer,intent(in)::L1,L2
		integer,allocatable::dimendata(:)
		integer::k,i,j
		allocate(dimendata(L1*L2))
		do i=1,L1*L2
			dimendata(i)=2
		end do
		call state%allocate(dimendata,classtype)
		k=1
		do i=1,L1
			do j=1,L2
				call state%setName(k,'A.'+i+'_'+j)
				k=k+1
			end do
		end do
		return
	end subroutine
	
	subroutine Hij_phi(state,site1,site2,H)! return H_{i,j}*|phi>, H_{i,j} is a two-bodies Hamiltonian
		type(Tensor),intent(inout)::state
		type(Tensor),intent(in)::H
		integer,intent(in)::site1(2),site2(2)
		type(Tensor)::allname
		character*10::name1,name2
		allname=state%getName()
		name1='A.'+site1(1)+'_'+site1(2)
		name2='A.'+site2(1)+'_'+site2(2)
		state=contract(H,['H1.n2','H2.n2'],state,[name1,name2])
		call state%setName('H1.n',name1)
		call state%setName('H2.n',name2)
		call state%permute(allname%ai())!the contract will reorder the dimension, so one should be order the dimension back to its oringinal order.
		return
	end subroutine	

	subroutine Hi_phi(state,site1,H)! return H_{i}*|phi>, H_{i} is a one-body Hamiltonian
		type(Tensor),intent(inout)::state
		type(Tensor),intent(in)::H
		integer,intent(in)::site1(2)
		type(Tensor)::allname
		character*10::name1
		allname=state%getName()
		name1='A.'+site1(1)+'_'+site1(2)
		state=contract(H,'H1.n2',state,name1)
		call state%setName('H1.n',name1)
		call state%permute(allname%ai())!the contract will reorder the dimension, so one should be order the dimension back to its oringinal order.
		return
	end subroutine	


	! return \sum_{<i,j> }H_{i,j}*|phi>, where <i,j> is the neareset neighoor in L1*L2 lattice
	! The Hamiltonian is H=\sum_{<i,j> }H_{i,j}
	! parameter(:) can use to store any data that use by the subroutine of H*phi
	! here I store:
	!        parameter(1): the initial state
	!        parameter(2): L1 ,L2
	!        parameter(3): two-bodies Hamiltonian
	!        parameter(4): one-body Hamiltonian
	
	subroutine  H_phi(parameter,state)
		type(Tensor)::parameter(:)
		type(Tensor)::state
		type(Tensor)::H,statei,sumState
		integer::i,j,k,L1,L2
		
		if(.not.state%getflag())then! if the state is empty, one should initial the state
			state=parameter(1)
			return
		end if
		L1=parameter(2)%ii(1)
		L2=parameter(2)%ii(2)
		do i=1,L1
			do j=1,L2-1
				statei=state
				call Hij_phi(statei,[i,j],[i,j+1],parameter(3))
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
				call Hij_phi(statei,[i,j],[i+1,j],parameter(3))
				sumState=sumState+statei
			end do
		end do

		do i=1,L1
			do j=1,L2
				statei=state
				call Hi_phi(statei,[i,j],parameter(4))
				if(sumState%getFlag())then
					sumState=sumState+statei
				else
					sumState=statei
				end if
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
	type(Tensor)::parameter(4),eig(2)
	type(eigenvalue)::eiger
	real*8::field
	integer::L1,L2,ncv
	field=1d0
	ncv=10
	L1=4
	L2=4

	call initial_state(parameter(1),L1,L2)
	parameter(2)=[L1,L2]
	parameter(3)=initial_H()
	parameter(4)=initial_Field(field)
	call eiger%set_ncv(ncv)     !Set the number of the Lanczos vector
	call eiger%set_Hphi(H_phi)  !Set the subroutine of H*phi
	call writemess('L1='+L1+',L2='+L2)
	call writemess('ncv='+ncv)
	call writemess('field='+field)
	call eiger%set_print_num(10)!print the process
	eig=eiger%eig(parameter,'SR',3)
	eig(1)=dble(eig(1))
	eig(1)=eig(1)/(L1*L2)
	call eig(1)%print("min Eigvalues(per site) are")
	stop
end 
