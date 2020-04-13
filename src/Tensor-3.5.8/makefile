include ../../make.inc

Tensor := \
Tools.o     \
Dimension.o      \
print_in_TData.o \
element_in_TData.o \
modify_in_TData.o  \
permutation_in_TData.o  \
TData.o      \
Contract_real8.o \
Tensor.o   \
OtherFunction.o\
parameter.o  



all: $(Tensor) 

$(Tensor):%.o : ./%.f90

./%.o :	./%.f90
	$(FC) $(openmp)  -o $@ -c $(<)  $(Optionl)
	
clean:
	rm -rf *.o *.mod *.a

