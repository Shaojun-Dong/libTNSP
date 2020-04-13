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
module parameter_type
	use Tensor_type
	use Tools
	use mpi
	implicit none
	private
	integer::array_character_length=15
	public::List
	type List
		type(Tensor)::parameter
		type(Tensor)::name
		integer::length=0
	contains
			procedure,public::deallocate=>clean 
			procedure,public::getFlag
			procedure,public::getData
			procedure,public::getName
			procedure,public::getLength
			procedure,public::getClassType
			procedure,public::allocate=>allocateParameter
			generic,public::print=>printParameter1,printParameter2,printParameter3
			procedure::printParameter1,printParameter2,printParameter3
			procedure::isetValue,ssetValue,dsetValue,csetValue,zsetValue,lsetValue,asetValue,TsetValue
			procedure::isetValue2,ssetValue2,dsetValue2,csetValue2,zsetValue2,lsetValue2,asetValue2,TsetValue2
			procedure::TsetAllValue,isetAllValue,ssetAllValue,dsetAllValue,csetAllValue,zsetAllValue,lsetAllValue,asetAllValue
			generic,public::setValue=>isetValue,ssetValue,dsetValue,csetValue,zsetValue,lsetValue,asetValue,&
				TsetValue,isetValue2,ssetValue2,dsetValue2,csetValue2,zsetValue2,lsetValue2,asetValue2,TsetValue2,&
				TsetAllValue,isetAllValue,ssetAllValue,dsetAllValue,csetAllValue,zsetAllValue,lsetAllValue,asetAllValue,&
				findValue
			procedure::initialNameAll,initialNamei,setName_w
			procedure,public::ifSetName
			procedure,public::ifName
			procedure,public::index=>elementindex
			generic,public::setName=>initialNameAll,initialNamei,setName_w
			generic,public::write=>writeout1,writeout2
			procedure::writeout1,writeout2
			procedure,public::read=>readout
			procedure::ielement1,selement1,delement1,celement1,zelement1,lelement1,aelement1,Telement1
			procedure::ielement2,selement2,delement2,celement2,zelement2,lelement2,aelement2,Telement2
			procedure::Arrayelement1,Arrayelement2
			generic,public::ii=>ielement1,ielement2
			generic,public::si=>selement1,selement2 
			generic,public::di=>delement1,delement2
			generic,public::ci=>celement1,celement2
			generic,public::zi=>zelement1,zelement2
			generic,public::li=>lelement1,lelement2
			generic,public::ai=>aelement1,aelement2
			generic,public::i=>Telement1,Telement2
			generic,public::Ti=>Arrayelement1,Arrayelement2
			procedure,public::check=>check_paramters
			generic,public::subList=>subList1,subList2
			procedure::subList1,subList2
			procedure,public::findValue
			procedure,public::update
			procedure,public::check_same_name
			procedure,public::ifkillData
			procedure,public::killData
	end type
	
	public::writemess
	interface writemess
		module procedure writeoparametemess
	end interface
	
	
	public::operator(+)
	interface operator(+)
		module procedure AddParameter
	end interface
	
	
	public::assignment(=)
	interface assignment(=)
		module procedure copyParapemter
		module procedure initial
	end interface

	interface List
		procedure constructor_List
		procedure constructor_List2
		procedure iList
		procedure sList
		procedure dList
		procedure aList
		procedure lList
	end interface
	
	public::MPI_BCAST_parameter,MPI_Send_parameter
contains
	type(Tensor) function getData(L)
		class(List),intent(in)::L
		getData=L%parameter
		return
	end function
	type(Tensor) function getName(L)
		class(List),intent(in)::L
		getName=L%name
		return
	end function
	type(List) function constructor_List(A,B)
		type(Tensor),intent(in)::A,B
		if(A%getType().ne.7)then
			call writemess(' ERROR in constructor of list,list(A,B), A should be a Tensor of character',-1)
			call error_stop
		end if
		if(A%getTotalData().ne.B%getTotalData())then
			call writemess(' ERROR in constructor of list,list(A,B)',-1)
			call error_stop
		end if
		constructor_List%parameter=B
		constructor_List%name=A
		constructor_List%length=A%getTotalData()
		return
	end function
	type(List) function constructor_List2(A,B)
		type(Tensor),intent(in)::B
		character(len=*),intent(in)::A(:)
		if(size(A).ne.B%getTotalData())then
			call writemess(' ERROR in constructor of list,list(A,B)',-1)
			call error_stop
		end if
		constructor_List2%parameter=B
		constructor_List2%name=Tensor(A)
		constructor_List2%length=B%getTotalData()
		return
	end function

	subroutine check_same_name(p)
		class(List),intent(inout)::p
		integer::i,j
		character(len=characterlen)::w
		do i=1,p%name%gettotalData()-1
			w=p%name%ai(i)
			do j=i+1,p%name%gettotalData()
				if(w.equ.p%name%ai(j))then
					call writemess('There are two element in the List have the same names')
					call writemess('the ith='+i+',and ith='+j+' are the same')
					call p%print()
					call error_stop
				end if
			end do
		end do
		return
	end subroutine

	logical function getFlag(p)
		class(List),intent(inout)::p
		getFlag=p%length.ne.0
		return
	end function

	subroutine clean(p)
		class(List),intent(inout)::p
		call p%parameter%deallocate()
		call p%name%deallocate()
		p%length=0
		return
	end subroutine
	
	character(len=characterlen) function getClassType(L)
		class(List),intent(in)::L
		getClassType=L%Parameter%getClassType()
		return
	end function
	
	integer function getLength(L)
		class(List),intent(in)::L
		getLength=L%length
		return
	end function
	
	subroutine copyParapemter(pinout,inp)
		type(List),intent(inout)::pinout
		type(List),intent(in)::inp
		if(inp%length.eq.0)then
			call pinout%deallocate()
			return
		end if
		pinout%parameter=inp%parameter
		pinout%name=inp%name
		pinout%length=inp%length
		return
	end subroutine
	subroutine initial(pinout,arrayTensor)
		type(List),intent(inout)::pinout
		type(Tensor),intent(in)::arrayTensor(:)
		if(size(arrayTensor).ne.2)then
			call writemess('ERROR in (=) for type(List)',-1)
			call error_stop
		end if
		if(arrayTensor(1)%getType().ne.7)then
			call writemess('ERROR in (=) for type(List), the name should be character',-1)
			call error_stop
		end if
		pinout%length=arrayTensor(1)%getTotalData()
		if(pinout%length.ne.arrayTensor(2)%getTotalData())then
			call writemess('ERROR in (=) for type(List), ERROR LENGTH',-1)
			call error_stop
		end if
		pinout%parameter=arrayTensor(2)
		pinout%name=arrayTensor(1)
		return
	end subroutine
	
	
	subroutine allocateParameter(p,DataTpye,length)
		class(List),intent(inout)::p
		character(len=*),intent(in)::DataTpye
		integer,intent(in)::length
		call p%parameter%allocate([length],DataTpye)
		call p%name%allocate([length],'character')
		p%length=length
		return
	end subroutine
	
	subroutine isetValue(p,ith,val)
		class(List),intent(inout)::p
		integer,intent(in)::val
		integer,intent(in)::ith
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	subroutine ssetValue(p,ith,val)
		class(List),intent(inout)::p
		real*4,intent(in)::val
		integer,intent(in)::ith
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	subroutine dsetValue(p,ith,val)
		class(List),intent(inout)::p
		real*8,intent(in)::val
		integer,intent(in)::ith
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	subroutine csetValue(p,ith,val)
		class(List),intent(inout)::p
		complex*8,intent(in)::val
		integer,intent(in)::ith
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	subroutine zsetValue(p,ith,val)
		class(List),intent(inout)::p
		complex*16,intent(in)::val
		integer,intent(in)::ith
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	subroutine lsetValue(p,ith,val)
		class(List),intent(inout)::p
		logical,intent(in)::val
		integer,intent(in)::ith
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	subroutine asetValue(p,ith,val)
		class(List),intent(inout)::p
		character(len=*),intent(in)::val
		integer,intent(in)::ith
		call p%parameter%setValue(ith,val)
		return
	end subroutine

	subroutine TsetValue(p,ith,val)
		class(List),intent(inout)::p
		type(Tensor),intent(in)::val
		integer,intent(in)::ith
		if(val%getTotalData().ne.1)then
			call writemess('ERROR in setvalue to a list')
			call writemess('input a Tensor, the length should be 1')
			call error_stop
		end if
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	
	
	subroutine isetValue2(p,namei,val)
		class(List),intent(inout)::p
		integer,intent(in)::val
		character(len=*),intent(in)::namei
		integer::ith
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	
	subroutine ssetValue2(p,namei,val)
		class(List),intent(inout)::p
		real*4,intent(in)::val
		character(len=*),intent(in)::namei
		integer::ith
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	
	subroutine dsetValue2(p,namei,val)
		class(List),intent(inout)::p
		real*8,intent(in)::val
		character(len=*),intent(in)::namei
		integer::ith
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	
	subroutine csetValue2(p,namei,val)
		class(List),intent(inout)::p
		complex*8,intent(in)::val
		character(len=*),intent(in)::namei
		integer::ith
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	
	subroutine zsetValue2(p,namei,val)
		class(List),intent(inout)::p
		complex*16,intent(in)::val
		character(len=*),intent(in)::namei
		integer::ith
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	
	subroutine lsetValue2(p,namei,val)
		class(List),intent(inout)::p
		logical,intent(in)::val
		character(len=*),intent(in)::namei
		integer::ith
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	
	subroutine asetValue2(p,namei,val)
		class(List),intent(inout)::p
		character(len=*),intent(in)::val
		character(len=*),intent(in)::namei
		integer::ith
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		call p%parameter%setValue(ith,val)
		return
	end subroutine

	subroutine TsetValue2(p,namei,val)
		class(List),intent(inout)::p
		type(Tensor),intent(in)::val
		character(len=*),intent(in)::namei
		integer::ith
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		if(val%getTotalData().ne.1)then
			call writemess('ERROR in setvalue to a list',-1)
			call writemess('input a Tensor, the length should be 1',-1)
			call error_stop
		end if
		call p%parameter%setValue(ith,val)
		return
	end subroutine
	subroutine isetAllValue(p,val)
		class(List),intent(inout)::p
		integer,intent(in)::val(:)
		if(p%length.eq.0)then
			p%parameter=val
			p%length=size(val)
			call p%name%allocate([p%length],'character')
		end if
		if(p%length.ne.size(val))then
			call writemess('ERROR in set value to a list',-1)
			call error_stop
		end if
		p%parameter=val
		return
	end subroutine
	subroutine ssetAllValue(p,val)
		class(List),intent(inout)::p
		real*4,intent(in)::val(:)
		if(p%length.eq.0)then
			p%parameter=val
			p%length=size(val)
			call p%name%allocate([p%length],'character')
		end if
		if(p%length.ne.size(val))then
			call writemess('ERROR in set value to a list',-1)
			call error_stop
		end if
		p%parameter=val
		return
	end subroutine
	subroutine dsetAllValue(p,val)
		class(List),intent(inout)::p
		real*8,intent(in)::val(:)
		if(p%length.eq.0)then
			p%parameter=val
			p%length=size(val)
			call p%name%allocate([p%length],'character')
		end if
		if(p%length.ne.size(val))then
			call writemess('ERROR in set value to a list',-1)
			call error_stop
		end if
		p%parameter=val
		return
	end subroutine
	subroutine csetAllValue(p,val)
		class(List),intent(inout)::p
		complex,intent(in)::val(:)
		if(p%length.eq.0)then
			p%parameter=val
			p%length=size(val)
			call p%name%allocate([p%length],'character')
		end if
		if(p%length.ne.size(val))then
			call writemess('ERROR in set value to a list',-1)
			call error_stop
		end if
		p%parameter=val
		return
	end subroutine
	subroutine zsetAllValue(p,val)
		class(List),intent(inout)::p
		complex*16,intent(in)::val(:)
		if(p%length.eq.0)then
			p%parameter=val
			p%length=size(val)
			call p%name%allocate([p%length],'character')
		end if
		if(p%length.ne.size(val))then
			call writemess('ERROR in set value to a list',-1)
			call error_stop
		end if
		p%parameter=val
		return
	end subroutine
	subroutine lsetAllValue(p,val)
		class(List),intent(inout)::p
		logical,intent(in)::val(:)
		if(p%length.eq.0)then
			p%parameter=val
			p%length=size(val)
			call p%name%allocate([p%length],'character')
		end if
		if(p%length.ne.size(val))then
			call writemess('ERROR in set value to a list',-1)
			call error_stop
		end if
		p%parameter=val
		return
	end subroutine
	subroutine asetAllValue(p,val)
		class(List),intent(inout)::p
		character(len=characterlen),intent(in)::val(:)
		if(p%length.eq.0)then
			p%parameter=val
			p%length=size(val)
			call p%name%allocate([p%length],'character')
		end if
		if(p%length.ne.size(val))then
			call writemess('ERROR in set value to a list',-1)
			call error_stop
		end if
		p%parameter=val
		return
	end subroutine
	subroutine TsetAllValue(p,val)
		class(List),intent(inout)::p
		type(Tensor),intent(in)::val
		if(p%length.eq.0)then
			p%parameter=val
			p%length=val%getTotalData()
			call p%name%allocate([p%length],'character')
		end if
		if(p%length.ne.val%getTotalData())then
			call writemess('ERROR in set value to a list',-1)
			call error_stop
		end if
		p%parameter=val
		return
	end subroutine
	
	subroutine initialNameAll(p,Allnamei)
		class(List),intent(inout)::p
		character(len=*),intent(in)::Allnamei(:)
		if(p%length.eq.0)then
			call writemess('ERROR Name length in initial parameter,Do not allocate parameter yet',-1)
			call error_stop
		end if
		if(size(Allnamei).ne.p%length)then
			call writemess('ERROR Name length in initial parameter',-1)
			call error_stop
		end if
		p%name=Allnamei
		return
	end subroutine
	subroutine initialNamei(p,ith,Allnamei)
		class(List),intent(inout)::p
		integer,intent(in)::ith
		character(len=*),intent(in)::Allnamei
		if(ith.gt.p%length)then
			call writemess('ERROR Name length in initial parameter',-1)
			call error_stop
		end if
		if(ith.le.0)then
			call writemess('ERROR in setting Name integer list,ith<=0',-1)
			call error_stop
		end if
		call p%name%setValue(ith,Allnamei)
		return
	end subroutine
	subroutine setName_w(p,oldName,newName)
		class(List),intent(inout)::p
		character(len=*),intent(in)::oldName,newName
		integer::ith
		if(.not.p%getFlag())then
			call writemess('The List is empty')
			call error_stop
		end if
		ith=p%name%which(oldName)
		if(ith.eq.0)then
			call writemess('Can not Find the name in the list')
			call writemess('name='+oldName)
			call error_stop
		end if
		call p%name%setValue(ith,newName)
		return
	end subroutine

	logical function ifName(p,inname)
		class(List),intent(in)::p
		character(len=*),intent(in)::inname
		integer::ith
		ith=p%name%which(inname)
		ifName=ith.ne.0
		return
	end function

	logical function ifSetName(p,oldName,newName)
		class(List),intent(inout)::p
		character(len=*),intent(in)::oldName,newName
		integer::ith
		if(.not.p%getFlag())then
			call writemess('The List is empty')
			call error_stop
		end if
		ith=p%name%which(oldName)
		if(ith.eq.0)then
			ifSetName=.false.
			return
		end if
		ifSetName=.true.
		call p%name%setValue(ith,newName)
		return
	end function

	
	
	integer function ielement1(p,ith)result(Res)
		class(List),intent(in)::p
		integer,intent(in)::ith
		if(p%length.eq.0)then
			call writemess('The list is empty',-1)
			call error_stop
		end if
		Res=p%parameter%ii(ith)
		return
	end function 
	integer function ielement2(p,namei)result(Res)
		class(List),intent(in)::p
		character(len=*),intent(in)::namei
		integer::ith
		if(p%length.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call writemess('The list is empty')
			call error_stop
		end if
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		Res=p%parameter%ii(ith)
		return
	end function 
	real*4 function selement1(p,ith)result(Res)
		class(List),intent(in)::p
		integer,intent(in)::ith
		if(p%length.eq.0)then
			call writemess('The list is empty')
			call error_stop
		end if
		Res=p%parameter%si(ith)
		return
	end function 
	real*4 function selement2(p,namei)result(Res)
		class(List),intent(in)::p
		character(len=*),intent(in)::namei
		integer::ith
		if(p%length.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call writemess('The list is empty')
			call error_stop
		end if
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		Res=p%parameter%si(ith)
		return
	end function 
	real*8 function delement1(p,ith)result(Res)
		class(List),intent(in)::p
		integer,intent(in)::ith
		if(p%length.eq.0)then
			call writemess('The list is empty')
			call error_stop
		end if
		Res=p%parameter%di(ith)
		return
	end function 
	real*8 function delement2(p,namei)result(Res)
		class(List),intent(in)::p
		character(len=*),intent(in)::namei
		integer::ith
		if(p%length.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call writemess('The list is empty')
			call error_stop
		end if
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		Res=p%parameter%di(ith)
		return
	end function 
	complex*8 function celement1(p,ith)result(Res)
		class(List),intent(in)::p
		integer,intent(in)::ith
		if(p%length.eq.0)then
			call writemess('The list is empty')
			call error_stop
		end if
		Res=p%parameter%ci(ith)
		return
	end function 
	complex*8 function celement2(p,namei)result(Res)
		class(List),intent(in)::p
		character(len=*),intent(in)::namei
		integer::ith
		if(p%length.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call writemess('The list is empty')
			call error_stop
		end if
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		Res=p%parameter%ci(ith)
		return
	end function 
	complex*16 function zelement1(p,ith)result(Res)
		class(List),intent(in)::p
		integer,intent(in)::ith
		if(p%length.eq.0)then
			call writemess('The list is empty')
			call error_stop
		end if
		Res=p%parameter%zi(ith)
		return
	end function 
	complex*16 function zelement2(p,namei)result(Res)
		class(List),intent(in)::p
		character(len=*),intent(in)::namei
		integer::ith
		if(p%length.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call writemess('The list is empty')
			call error_stop
		end if
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		Res=p%parameter%zi(ith)
		return
	end function 
	logical function lelement1(p,ith)result(Res)
		class(List),intent(in)::p
		integer,intent(in)::ith
		if(p%length.eq.0)then
			call writemess('The list is empty')
			call error_stop
		end if
		Res=p%parameter%li(ith)
		return
	end function 
	logical function lelement2(p,namei)result(Res)
		class(List),intent(in)::p
		character(len=*),intent(in)::namei
		integer::ith
		if(p%length.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call writemess('The list is empty')
			call error_stop
		end if
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		Res=p%parameter%li(ith)
		return
	end function 
	character(len=max_len_of_char_in_TData) function aelement1(p,ith)result(Res)
		class(List),intent(in)::p
		integer,intent(in)::ith
		Res=p%parameter%ai(ith)
		return
	end function 
	character(len=max_len_of_char_in_TData) function aelement2(p,namei)result(Res)
		class(List),intent(in)::p
		character(len=*),intent(in)::namei
		integer::ith
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		Res=p%parameter%ai(ith)
		return
	end function 
	
	type(Tensor) function Telement1(p,ith)result(Res)
		class(List),intent(in)::p
		integer,intent(in)::ith
		if(p%length.eq.0)then
			call writemess('The list is empty')
			call error_stop
		end if
		Res=p%parameter%i(ith)
		return
	end function 
	type(Tensor) function Telement2(p,namei)result(Res)
		class(List),intent(in)::p
		character(len=*),intent(in)::namei
		integer::ith
		if(p%length.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call writemess('The list is empty')
			call error_stop
		end if
		ith=p%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		Res=p%parameter%i(ith)
		return
	end function 
	
	
	
	subroutine writeout1(p,w,uni)
		class(List),intent(in)::p
		character(len=*),intent(in)::w
		integer,intent(in)::uni
		integer::i,ptype
		ptype=p%parameter%getType()
		write(uni,*)'**********************************************************'
		write(uni,*)'***length_and_type',p%length,p%parameter%getClassType()
		write(uni,*)trim('***'+w)
		write(uni,*)'**********************************************************'
		select case(ptype)
			case(1)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),p%parameter%ii(i)
				end do
			case(2)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),p%parameter%si(i)
				end do
			case(3)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),p%parameter%di(i)
				end do
			case(4)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),real(p%parameter%ci(i),kind=4),aimag(p%parameter%ci(i))
				end do
			case(5)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),real(p%parameter%zi(i),kind=8),dimag(p%parameter%zi(i))
				end do
			case(6)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),p%parameter%li(i)
				end do
			case(7)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),"    ",p%parameter%ai(i)
				end do
		end select
	end subroutine
	
	subroutine writeout2(p,uni)
		class(List),intent(in)::p
		integer,intent(in)::uni
		integer::i,ptype
		ptype=p%parameter%getType()
		write(uni,*)'**********************************************************'
		write(uni,*)'***length_and_type',p%length,p%parameter%getClassType()
		write(uni,*)'***     '
		write(uni,*)'**********************************************************'
		select case(ptype)
			case(1)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),p%parameter%ii(i)
				end do
			case(2)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),p%parameter%si(i)
				end do
			case(3)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),p%parameter%di(i)
				end do
			case(4)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),real(p%parameter%ci(i),kind=4),aimag(p%parameter%ci(i))
				end do
			case(5)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),real(p%parameter%zi(i),kind=8),dimag(p%parameter%zi(i))
				end do
			case(6)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),p%parameter%li(i)
				end do
			case(7)
				do i=1,p%length
					write(uni,*)"    ",trim(p%name%ai(i)),"    ",p%parameter%ai(i)
				end do
		end select
	end subroutine
	
	subroutine readout(p,uni)
		class(List),intent(inout)::p
		integer,intent(in)::uni
		integer::i,ptype,length
		CHARACTER(len=50)::notused,classtype
		integer,allocatable::idata(:)
		real*4,allocatable::sdata(:),sdata2(:)
		real*8,allocatable::ddata(:),ddata2(:)
		character(len=max_len_of_char_in_TData),allocatable::adata(:)
		logical,allocatable::ldata(:)
		character(len=max_len_of_char_in_TData),allocatable::nameadata(:)
		type(Tensor)::temp
		read(uni,*)notused
		read(uni,*)notused,length,classtype
		if(length.eq.0)then
			read(uni,*)notused
			read(uni,*)notused
			call p%deallocate()
			return
		end if
		read(uni,*)notused
		read(uni,*)notused
		call temp%setType(classtype)
		ptype=temp%getType()
		allocate(nameadata(length))
		select case(ptype)
			case(1)
				allocate(idata(length))
				do i=1,length
					read(uni,*)nameadata(i),idata(i)
				end do
				p%parameter=idata
				p%name=nameadata
				p%length=length
			case(2)
				allocate(sdata(length))
				do i=1,length
					read(uni,*)nameadata(i),sdata(i)
				end do
				p%parameter=sdata
				p%name=nameadata
				p%length=length
			case(3)
				allocate(ddata(length))
				do i=1,length
					read(uni,*)nameadata(i),ddata(i)
				end do
				p%parameter=ddata
				p%name=nameadata
				p%length=length
			case(4)
				allocate(sdata(length))
				allocate(sdata2(length))
				do i=1,length
					read(uni,*)nameadata(i),sdata(i),sdata2
				end do
				p%parameter=cmplx(sdata,sdata2,kind=4)
				p%name=nameadata
				p%length=length
			case(5)
				allocate(ddata(length))
				allocate(ddata2(length))
				do i=1,length
					read(uni,*)nameadata(i),ddata(i),ddata2
				end do
				p%parameter=dcmplx(ddata,ddata2)
				p%name=nameadata
				p%length=length
			case(6)
				allocate(ldata(length))
				do i=1,length
					read(uni,*)nameadata(i),ldata(i)
				end do
				p%parameter=ldata
				p%name=nameadata
				p%length=length
			case(7)
				allocate(adata(length))
				do i=1,length
					read(uni,*)nameadata(i),adata(i)
				end do
				p%parameter=adata
				p%name=nameadata
				p%length=length
		end select
	end subroutine
	
	subroutine writeoparametemess(p,cpuInfo)
		class(List),intent(in)::p
		integer,intent(in),optional::cpuInfo
		integer::i,ptype
		ptype=p%parameter%getType()
		select case(ptype)
			case(1)
				do i=1,p%length
					call writemess(' '+p%name%ai(i)+'='+p%parameter%ii(i),cpuInfo )
				end do
			case(2)
				do i=1,p%length
					call writemess(' '+p%name%ai(i)+'='+p%parameter%si(i),cpuInfo )
				end do
			case(3)
				do i=1,p%length
					call writemess(' '+p%name%ai(i)+'='+p%parameter%di(i),cpuInfo )
				end do
			case(4)
				do i=1,p%length
					call writemess(' '+p%name%ai(i)+'='+p%parameter%ci(i),cpuInfo )
				end do
			case(5)
				do i=1,p%length
					call writemess(' '+p%name%ai(i)+'='+p%parameter%zi(i),cpuInfo )
				end do
			case(6)
				do i=1,p%length
					call writemess(' '+p%name%ai(i)+'='+p%parameter%li(i),cpuInfo )
				end do
			case(7)
				do i=1,p%length
					call writemess(' '+p%name%ai(i)+'='+p%parameter%ai(i),cpuInfo )
				end do
		end select
	end subroutine
	
	
		
	subroutine printParameter1(p,lenA,uni)
		class(List),intent(in)::p
		integer,intent(in)::lenA
		integer,intent(in),optional::uni
		character(len=20)::F
		integer::i,ptype
		ptype=p%parameter%getType()
		F='A'+lenA+''
		F='('+F+',A2,'+F+')'
		if(present(uni))then
			select case(ptype)
				case(1)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%ii(i)
					end do
				case(2)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%si(i)
					end do
				case(3)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%di(i)
					end do
				case(4)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%ci(i)
					end do
				case(5)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%zi(i)
					end do
				case(6)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%li(i)
					end do
				case(7)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%ai(i)
					end do
			end select
			return
		end if
		select case(ptype)
			case(1)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%ii(i)
				end do
			case(2)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%si(i)
				end do
			case(3)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%di(i)
				end do
			case(4)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%ci(i)
				end do
			case(5)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%zi(i)
				end do
			case(6)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%li(i)
				end do
			case(7)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%ai(i)
				end do
		end select
		return
	end subroutine
		
	subroutine printParameter2(p,lenA,uni)
		class(List),intent(in)::p
		integer,intent(in)::lenA(2)
		integer,intent(in),optional::uni
		character(len=20)::F
		integer::i,ptype
		ptype=p%parameter%getType()
		F='('+'A'+lenA(1)+',A2,'+'A'+lenA(2)+')'
		if(present(uni))then
			select case(ptype)
				case(1)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%ii(i)
					end do
				case(2)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%si(i)
					end do
				case(3)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%di(i)
					end do
				case(4)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%ci(i)
					end do
				case(5)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%zi(i)
					end do
				case(6)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%li(i)
					end do
				case(7)
					do i=1,p%length
						write(uni,F)' '+p%name%ai(i),'=',' '+p%parameter%ai(i)
					end do
			end select
			return
		end if
		select case(ptype)
			case(1)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%ii(i)
				end do
			case(2)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%si(i)
				end do
			case(3)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%di(i)
				end do
			case(4)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%ci(i)
				end do
			case(5)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%zi(i)
				end do
			case(6)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%li(i)
				end do
			case(7)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%ai(i)
				end do
		end select
		return
	end subroutine
	subroutine printParameter3(p)
		class(List),intent(in)::p
		integer::lenA(2)
		character(len=20)::F
		integer::i,ptype
		lenA=[15,20]
		ptype=p%parameter%getType()
		F='('+'A'+lenA(1)+',A2,'+'A'+lenA(2)+')'
		select case(ptype)
			case(1)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%ii(i)
				end do
			case(2)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%si(i)
				end do
			case(3)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%di(i)
				end do
			case(4)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%ci(i)
				end do
			case(5)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%zi(i)
				end do
			case(6)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%li(i)
				end do
			case(7)
				do i=1,p%length
					write(*,F)' '+p%name%ai(i),'=',' '+p%parameter%ai(i)
				end do
		end select
		return
	end subroutine
	
	integer function elementindex(p,ith)
		class(List),intent(in)::p
		character(len=*),intent(in)::ith
		elementindex=p%name%which(ith)
		return
	end function
		
		
	
	subroutine check_paramters(p2,p1)
		class(List),intent(in)::P2
		type(List),intent(in)::p1
		integer::i,ith,ptype
		character(len=max_len_of_char_in_TData)::namei
		if(p1%length.ne.p2%length)then
			call writemess('The paramters have diferent length')
			return
		end if
		ptype=p1%parameter%getType()
		if(ptype.ne.p2%parameter%getType())then
			call writemess('The paramters have diferent data type')
			return
		end if
		do i=1,p1%length
			namei=p1%name%ai(i)
			ith=p2%index(namei)
			if(ith.eq.0)then
				call writemess('Can not find'+(' '+namei)+'in the secend parameter')
			else	
				select case(ptype)
					case(1)
						if(p1%ii(i).ne.p2%ii(ith))then
							call writemess('The parameter change:'+(' '+namei)+' ='+p1%ii(i)+'-->'+p2%ii(ith))
						end if
					case(2)
						if(p1%si(i).ne.p2%si(ith))then
							call writemess('The parameter change:'+(' '+namei)+' ='+p1%si(i)+'-->'+p2%si(ith))
						end if
					case(3)
						if(p1%di(i).ne.p2%di(ith))then
							call writemess('The parameter change:'+(' '+namei)+' ='+p1%di(i)+'-->'+p2%di(ith))
						end if
					case(4)
						if(p1%ci(i).ne.p2%ci(ith))then
							call writemess('The parameter change:'+(' '+namei)+' ='+p1%ci(i)+'-->'+p2%ci(ith))
						end if
					case(5)
						if(p1%zi(i).ne.p2%zi(ith))then
							call writemess('The parameter change:'+(' '+namei)+' ='+p1%zi(i)+'-->'+p2%zi(ith))
						end if
					case(6)
						if(p1%li(i).neqv.p2%li(ith))then
							call writemess('The parameter change:'+(' '+namei)+' ='+p1%li(i)+'-->'+p2%li(ith))
						end if
					case(7)
						if(p1%ai(i).nequ.p2%ai(ith))then
							call writemess('The parameter change:'+(' '+namei)+' ='+p1%ai(i)+'-->'+p2%ai(ith))
						end if
				end select
			end if
		end do
	end subroutine
	
	type(List) function AddParameter(List1,List2)
		type(List),intent(in)::List1,List2
		integer::length,Classtype
		integer::total1,total2
		length=List1%length+List2%length
		Classtype=List1%parameter%getType()
		if(Classtype.ne.List2%parameter%getType())then
			call writemess('Can not connect the List')
			call error_stop
		end if
		call AddParameter%allocate(List2%parameter%getClassType(),length )
		total1=List1%parameter%getTotalData()
		total2=List2%parameter%getTotalData()
		call AddParameter%Parameter%setValue([1,total1],List1%parameter,[1,total1])
		call AddParameter%name%setValue([1,total1],List1%name,[1,total1])
		call AddParameter%Parameter%setValue([total1+1,length],List2%parameter,[1,total2])
		call AddParameter%name%setValue([total1+1,length],List2%name,[1,total2])
		if(check_same_name_flag)call AddParameter%check_same_name()
		return
	end function
	
	type(List) function subList1(L,ith,jth)result(Res)
		class(List),intent(in)::L
		integer,intent(in)::ith,jth
		integer::subLength
		subLength=jth-ith+1
		if(subLength.le.0)then
			call writemess('ERROR in subList, length<0,ith='+ith+',jth='+jth)
			call error_stop
		end if
		call Res%allocate(L%getClassType(),subLength)
		call Res%parameter%setValue([1,subLength],L%parameter,[ith,jth])
		call Res%name%setValue([1,subLength],L%name,[ith,jth])
		return
	end function
	
	type(List) function subList2(L,namei,namej)result(Res)
		class(List),intent(inout)::L
		character(len=*),intent(in)::namei,namej
		integer::ith,jth
		integer::subLength
		ith=L%name%which(namei)
		if(ith.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namei))
			call error_stop
		end if
		jth=L%name%which(namej)
		if(jth.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+namej))
			call error_stop
		end if
		if(ith.gt.ith)then
			call writemess('ERROR in subList, length<0,ith='+ith+',jth='+jth)
			call writemess('namei='+namei+'namej='+namej)
			call writemess('The data in list are')
			call writemess(L)
			call error_stop
		end if
		subLength=jth-ith+1
		call Res%allocate(L%getClassType(),subLength)
		call Res%parameter%setValue([1,subLength],L%parameter,[ith,jth])
		call Res%name%setValue([1,subLength],L%name,[ith,jth])
		return
	end function 

	subroutine findValue(p,inp)
		class(List),intent(inout)::p
		type(List),intent(in)::inp
		integer::i
		do i=1,p%getLength()
			call p%setValue(i,inp%i(p%name%ai(i)))
		end do
		return
	end subroutine

	subroutine update(p,inp)
		class(List),intent(inout)::p
		type(List),intent(in)::inp
		integer::i,ith,ptype
		character(len=characterlen)::w
		ptype=p%parameter%getType()
		if(ptype.ne.inp%parameter%getType())then
			call writemess('Can not update the list, the data type are not the same')
			call error_stop
		end if
		do i=1,p%getLength()
			w=p%name%ai(i)
			ith=inp%name%which(w)
			if(ith.ne.0)then
				select case(ptype)
					case(1)
						if(p%ii(i).ne.inp%ii(ith))then
							call writemess('The parameter change:'+(' '+w)+' ='+p%ii(i)+'-->'+inp%ii(ith))
						end if
						call p%setValue(i,inp%ii(ith))
					case(2)
						if(p%si(i).ne.inp%si(ith))then
							call writemess('The parameter change:'+(' '+w)+' ='+p%si(i)+'-->'+inp%si(ith))
						end if
						call p%setValue(i,inp%si(ith))
					case(3)
						if(p%di(i).ne.inp%di(ith))then
							call writemess('The parameter change:'+(' '+w)+' ='+p%di(i)+'-->'+inp%di(ith))
						end if
						call p%setValue(i,inp%di(ith))
					case(4)
						if(p%ci(i).ne.inp%ci(ith))then
							call writemess('The parameter change:'+(' '+w)+' ='+p%ci(i)+'-->'+inp%ci(ith))
						end if
						call p%setValue(i,inp%ci(ith))
					case(5)
						if(p%zi(i).ne.inp%zi(ith))then
							call writemess('The parameter change:'+(' '+w)+' ='+p%zi(i)+'-->'+inp%zi(ith))
						end if
						call p%setValue(i,inp%zi(ith))
					case(6)
						if(p%li(i).neqv.inp%li(ith))then
							call writemess('The parameter change:'+(' '+w)+' ='+p%li(i)+'-->'+inp%li(ith))
						end if
						call p%setValue(i,inp%li(ith))
					case(7)
						if(p%ai(i).nequ.inp%ai(ith))then
							call writemess('The parameter change:'+(' '+w)+' ='+p%ai(i)+'-->'+inp%ai(ith))
						end if
						call p%setValue(i,inp%ai(ith))
				end select
			end if
		end do
		return
	end subroutine

	subroutine killData(p,name_kill)
		class(List),intent(inout)::p
		character(len=*),intent(in)::name_kill
		type(List)::tempp
		integer::i,counter
		character(len=characterlen)::namei
		counter=0
		if(p%length.eq.0)then
			return
		end if
		do i=1,p%length
			if(p%name%ai(i).nequ.name_kill)then
				counter=counter+1
			end if
		end do
		if(counter.eq.0)return
		call tempp%allocate(p%getClassType(),counter)
		counter=0
		do i=1,p%length
			namei=p%name%ai(i)
			if(namei.nequ.name_kill)then
				counter=counter+1
				if(counter.gt.tempp%length)then
					call writemess('ERROR in killData,parameter.f90')
					call error_stop
				end if
				call tempp%setName(counter,namei)
				call tempp%setValue(counter,p%i(i))
			end if
		end do
		p=tempp
		return
	end subroutine

	logical function ifkillData(p,name_kill)
		class(List),intent(inout)::p
		character(len=*),intent(in)::name_kill
		type(List)::tempp
		integer::i,counter
		character(len=characterlen)::namei
		counter=0
		if(p%length.eq.0)then
			ifkillData=.false.
			return
		end if
		do i=1,p%length
			if(p%name%ai(i).nequ.name_kill)then
				counter=counter+1
			end if
		end do
		if(counter.eq.0)then
			ifkillData=.false.
			return
		end if
		ifkillData=.true.
		call tempp%allocate(p%getClassType(),counter)
		counter=0
		do i=1,p%length
			namei=p%name%ai(i)
			if(namei.nequ.name_kill)then
				counter=counter+1
				if(counter.gt.tempp%length)then
					call writemess('ERROR in killData,parameter.f90')
					call error_stop
				end if
				call tempp%setName(counter,namei)
				call tempp%setValue(counter,p%i(i))
			end if
		end do
		p=tempp
		return
	end function

	function iList(Charname,indata)
		type(List)::iList
		character(len=*),intent(in)::Charname
		integer,intent(in)::indata(:)
		integer::i,counter,lenindata,j,lenofCharacter
		character(len=characterlen)::words
		character(len=1)::divider,typecha
		type(Tensor)::tempName,tempTensor
		divider=get_array_character_divider()
		lenindata=size(indata)
		lenofCharacter=max(lenindata/array_character_length,1)
		if(lenofCharacter*array_character_length.lt.lenindata)then
			lenofCharacter=lenofCharacter+1
		end if
		call tempName%allocate([lenofCharacter],'character')
		call tempTensor%allocate([lenofCharacter],'character')
		typecha='i'
		words=typecha
		counter=0
		j=0
		do i=1,lenindata
			counter=counter+1
			if(counter.eq.1)then
				words=words+'='+indata(i)
			else
				words=words+divider+indata(i)
			end if
			if(counter.eq.array_character_length)then
				j=j+1
				if(j.gt.lenofCharacter)then
					call writemess('ERROR in iList',-1)
					call error_stop
				end if
				call tempName%setValue(j,Charname+j)
				call tempTensor%setValue(j,words)
				counter=0
				words=typecha
			end if
		end do
		if(counter.ne.0)then
			j=j+1
			if(j.gt.lenofCharacter)then
				call writemess('ERROR in iList',-1)
				call error_stop
			end if
			call tempName%setValue(j,Charname+j)
			call tempTensor%setValue(j,words)
			counter=0
			words=typecha
		end if
		if(j.ne.lenofCharacter)then
			call writemess('ERROR in iList',-1)
			call error_stop
		end if
		iList=constructor_List(tempName,tempTensor)
		return
	end function
	function sList(Charname,indata)
		type(List)::sList
		character(len=*),intent(in)::Charname
		real*4,intent(in)::indata(:)
		integer::i,counter,lenindata,j,lenofCharacter
		character(len=characterlen)::words
		character(len=1)::divider,typecha
		type(Tensor)::tempName,tempTensor
		divider=get_array_character_divider()
		lenindata=size(indata)
		lenofCharacter=max(lenindata/array_character_length,1)
		if(lenofCharacter*array_character_length.lt.lenindata)then
			lenofCharacter=lenofCharacter+1
		end if
		call tempName%allocate([lenofCharacter],'character')
		call tempTensor%allocate([lenofCharacter],'character')
		typecha='s'
		words=typecha
		counter=0
		j=0
		do i=1,lenindata
			counter=counter+1
			if(counter.eq.1)then
				words=words+'='+indata(i)
			else
				words=words+divider+indata(i)
			end if
			if(counter.eq.array_character_length)then
				j=j+1
				if(j.gt.lenofCharacter)then
					call writemess('ERROR in iList',-1)
					call error_stop
				end if
				call tempName%setValue(j,Charname+j)
				call tempTensor%setValue(j,words)
				counter=0
				words=typecha
			end if
		end do
		if(counter.ne.0)then
			j=j+1
			if(j.gt.lenofCharacter)then
				call writemess('ERROR in iList',-1)
				call error_stop
			end if
			call tempName%setValue(j,Charname+j)
			call tempTensor%setValue(j,words)
			counter=0
			words=typecha
		end if
		if(j.ne.lenofCharacter)then
			call writemess('ERROR in iList',-1)
			call error_stop
		end if
		sList=constructor_List(tempName,tempTensor)
		return
	end function
	function dList(Charname,indata)
		type(List)::dList
		character(len=*),intent(in)::Charname
		real*8,intent(in)::indata(:)
		integer::i,counter,lenindata,j,lenofCharacter
		character(len=characterlen)::words
		character(len=1)::divider,typecha
		type(Tensor)::tempName,tempTensor
		divider=get_array_character_divider()
		lenindata=size(indata)
		lenofCharacter=max(lenindata/array_character_length,1)
		if(lenofCharacter*array_character_length.lt.lenindata)then
			lenofCharacter=lenofCharacter+1
		end if
		call tempName%allocate([lenofCharacter],'character')
		call tempTensor%allocate([lenofCharacter],'character')
		typecha='d'
		words=typecha
		counter=0
		j=0
		do i=1,lenindata
			counter=counter+1
			if(counter.eq.1)then
				words=words+'='+indata(i)
			else
				words=words+divider+indata(i)
			end if
			if(counter.eq.array_character_length)then
				j=j+1
				if(j.gt.lenofCharacter)then
					call writemess('ERROR in dList',-1)
					call error_stop
				end if
				call tempName%setValue(j,Charname+j)
				call tempTensor%setValue(j,words)
				counter=0
				words=typecha
			end if
		end do
		if(counter.ne.0)then
			j=j+1
			if(j.gt.lenofCharacter)then
				call writemess('ERROR in dList,2',-1)
				call error_stop
			end if
			call tempName%setValue(j,Charname+j)
			call tempTensor%setValue(j,words)
			counter=0
			words=typecha
		end if
		if(j.ne.lenofCharacter)then
			call writemess('ERROR in dList,3',-1)
			write(*,*)j,lenofCharacter
			call error_stop
		end if
		dList=constructor_List(tempName,tempTensor)
		return
	end function
	function lList(Charname,indata)
		type(List)::lList
		character(len=*),intent(in)::Charname
		logical,intent(in)::indata(:)
		integer::i,counter,lenindata,j,lenofCharacter
		character(len=characterlen)::words
		character(len=1)::divider,typecha
		type(Tensor)::tempName,tempTensor
		divider=get_array_character_divider()
		lenindata=size(indata)
		lenofCharacter=max(lenindata/array_character_length,1)
		if(lenofCharacter*array_character_length.lt.lenindata)then
			lenofCharacter=lenofCharacter+1
		end if
		call tempName%allocate([lenofCharacter],'character')
		call tempTensor%allocate([lenofCharacter],'character')
		typecha='l'
		words=typecha
		counter=0
		j=0
		do i=1,lenindata
			counter=counter+1
			if(counter.eq.1)then
				words=words+'='+indata(i)
			else
				words=words+divider+indata(i)
			end if
			if(counter.eq.array_character_length)then
				j=j+1
				if(j.gt.lenofCharacter)then
					call writemess('ERROR in lList',-1)
					call error_stop
				end if
				call tempName%setValue(j,Charname+j)
				call tempTensor%setValue(j,words)
				counter=0
				words=typecha
			end if
		end do
		if(counter.ne.0)then
			j=j+1
			if(j.gt.lenofCharacter)then
				call writemess('ERROR in lList',-1)
				call error_stop
			end if
			call tempName%setValue(j,Charname+j)
			call tempTensor%setValue(j,words)
			counter=0
			words=typecha
		end if
		if(j.ne.lenofCharacter)then
			call writemess('ERROR in lList',-1)
			call error_stop
		end if
		lList=constructor_List(tempName,tempTensor)
		return
	end function
	function aList(Charname,indata)
		type(List)::aList
		character(len=*),intent(in)::Charname
		character(len=*),intent(in)::indata(:)
		integer::i,counter,lenindata,j,lenofCharacter
		character(len=characterlen)::words
		character(len=1)::divider,typecha
		type(Tensor)::tempName,tempTensor
		divider=get_array_character_divider()
		lenindata=size(indata)
		lenofCharacter=max(lenindata/array_character_length,1)
		if(lenofCharacter*array_character_length.lt.lenindata)then
			lenofCharacter=lenofCharacter+1
		end if
		call tempName%allocate([lenofCharacter],'character')
		call tempTensor%allocate([lenofCharacter],'character')
		typecha='a'
		words=typecha
		counter=0
		j=0
		do i=1,lenindata
			counter=counter+1
			if(counter.eq.1)then
				words=words+'='+indata(i)
			else
				words=words+divider+indata(i)
			end if
			if(counter.eq.array_character_length)then
				j=j+1
				if(j.gt.lenofCharacter)then
					call writemess('ERROR in aList',-1)
					call error_stop
				end if
				call tempName%setValue(j,Charname+j)
				call tempTensor%setValue(j,words)
				counter=0
				words=typecha
			end if
		end do
		if(counter.ne.0)then
			j=j+1
			if(j.gt.lenofCharacter)then
				call writemess('ERROR in aList',-1)
				call error_stop
			end if
			call tempName%setValue(j,Charname+j)
			call tempTensor%setValue(j,words)
			counter=0
			words=typecha
		end if
		if(j.ne.lenofCharacter)then
			call writemess('ERROR in aList',-1)
			call error_stop
		end if
		aList=constructor_List(tempName,tempTensor)
		return
	end function


	type(Tensor) function Arrayelement1(p,ith)result(Res)
		class(List),intent(in)::p
		integer,intent(in)::ith
		if(p%length.eq.0)then
			call writemess('The list is empty')
			call error_stop
		end if
		Res=Tensor(p%parameter%ai(ith))
		return
	end function 
	type(Tensor) function Arrayelement2(L,Cha)result(Res)
		class(List),intent(in)::L
		character(len=*),intent(in)::Cha
		type(Tensor),allocatable::temp(:)
		integer::i,LenData,TotalData,j,k
		logical::goon
		if(L%length.eq.0)then
			call writemess('Do not Find the name in parameter',-1)
			call writemess('The Name is'+(' '+Cha))
			call writemess('The list is empty')
			call error_stop
		end if
		call Res%empty()
		i=1
		LenData=0
		goon=L%ifName(cha+i)
		if(goon)LenData=LenData+1
		do while(goon)
			i=i+1
			goon=L%ifName(cha+i)
			if(goon)LenData=LenData+1
		end do
		if(LenData.eq.0)return
		allocate(temp(LenData))
		TotalData=0
		do i=1,LenData
			temp(i)=Tensor(L%ai(cha+i))
			TotalData=TotalData+temp(i)%getTotalData()
		end do
		call Res%allocate([TotalData],temp(1)%getType())
		i=1
		j=0
		do k=1,LenData
			i=j+1
			j=temp(k)%getTotalData()+i-1
			call Res%setValue(i,j,temp(k))
		end do
		return
	end function 
		
		
	subroutine MPI_BCAST_parameter(P,ID,ierr,MPIcommon)
		type(List),intent(inout)::p
		integer,intent(in)::ID
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		call MPI_BCAST_Tensor(p%parameter,ID,ierr,MPIcommon)
		call MPI_BCAST_Tensor(p%name,ID,ierr,MPIcommon)
		p%length=p%parameter%getTotalData()
		return
	end subroutine
	
	subroutine MPI_send_parameter(p1,p2,ID1,ID2,ierr,MPIcommon)
		type(List),intent(in)::p1
		type(List),intent(inout)::p2
		integer,intent(in)::ID1,ID2
		integer::ierr
		integer,optional,intent(in)::MPIcommon
		call MPI_send_Tensor(p1%parameter,p2%parameter,ID1,ID2,ierr,MPIcommon)
		call MPI_send_Tensor(p1%name,p2%name,ID1,ID2,ierr,MPIcommon)
		p2%length=p2%parameter%getTotalData()
		return
	end subroutine


end module
