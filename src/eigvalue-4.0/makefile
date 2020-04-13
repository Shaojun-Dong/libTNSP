include ../../make.inc


eigvalue :=\
eigen.o

all: $(eigvalue) 

$(eigvalue):%.o : ./%.f90

./%.o :	./%.f90
	$(FC) $(openmp)  -o $@ -c $(<)  $(Optionl) -I../Tensor-$(version)  
	
clean:
	rm -rf *.o *.mod *.a
