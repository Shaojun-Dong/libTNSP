include ../../make.inc

LBFGSTool := LinearSearchTools.o OptimizationTools.o onlineOptimizationTools.o



all: $(LBFGSTool) 

$(LBFGSTool):%.o : ./%.f90

./%.o :	./%.f90
	$(FC) $(openmp)  -o $@ -c $(<)  $(Optionl) -I../Tensor-$(version)  
	
clean:
	rm -rf *.o *.mod *.a

