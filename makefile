

include make.inc

lib: 
	( cd $(TNSDIR); $(MAKE) )
	( cd $(SYMM); $(MAKE) )
	( cd $(EIG); $(MAKE) )
	( cd $(OPTI); $(MAKE); )
	(mkdir -p $(outputName);)
	(cp $(TNSDIR)/*.mod $(outputName);)
	(cp $(SYMM)/*.mod $(outputName);)
	(cp $(EIG)/*.mod $(outputName);)
	(cp $(OPTI)/*.mod $(outputName);)
	( ar rc $(outputName)/$(outputName).a $(TNSDIR)/*.o $(SYMM)/*.o $(EIG)/*.o $(OPTI)/*.o )
	make echo

Old: 
	(@$(ECHO) $(version))
	( cd $(TNSDIR); $(MAKE) )
	(mkdir -p $(outputName);)
	(cp $(TNSDIR)/*.mod $(outputName);)
	( ar rc $(outputName)/$(outputName).a $(TNSDIR)/*.o  )
mpi:
	( cd $(MPIDIR); $(MAKE) )
	(cp $(MPIDIR)/*.o $(MPIDIR)/*.mod $(TNSDIR))
	(cp $(MPIDIR)/*.o $(MPIDIR)/*.mod $(SYMM))
	(cp $(MPIDIR)/*.o $(MPIDIR)/*.mod $(EIG))
mpilib:
	( cd $(MPIDIR); $(MAKE) )
	(cp $(MPIDIR)/*.o $(MPIDIR)/*.mod $(TNSDIR))
	(cp $(MPIDIR)/*.o $(MPIDIR)/*.mod $(SYMM))
	(cp $(MPIDIR)/*.o $(MPIDIR)/*.mod $(EIG))	
	( cd $(TNSDIR); $(MAKE) )
	( cd $(SYMM); $(MAKE) )
	( cd $(EIG); $(MAKE) )
	(mkdir -p $(outputName);)
	(cp $(TNSDIR)/*.mod $(outputName);)
	(cp $(SYMM)/*.mod $(outputName);)
	(cp $(EIG)/*.mod $(outputName);)
	( ar rc $(outputName)/$(outputName).a $(TNSDIR)/*.o $(SYMM)/*.o $(EIG)/*.o )
	make echo
clean:
	( cd $(TNSDIR); $(MAKE) clean )
	( cd $(SYMM); $(MAKE) clean )
	( cd $(EIG); $(MAKE) clean )
	( cd $(OPTI); $(MAKE) clean )
cleanmpi:
	( cd $(MPIDIR); $(MAKE) clean )
cleanSym:
	( cd $(SYMM); $(MAKE) clean )
	
cleanEig:
	( cd $(EIG); $(MAKE) clean )

cleanOpt:
	( cd $(OPTI); $(MAKE) clean )

cleanAll:
	( find . -name "*.mod" -exec rm  {} \; )
	( find . -name "*.o" -exec rm  {} \;)
	(rm  -rf libTensor-*)

cleanlib:
	(rm  -rf $(outputName))

cleanAlllib:
	(rm  -rf libTensor-*)

cleanAllMod:
	( rm */*/*.mod )
	( rm */*/*.o )
echo:
	@$(ECHO) " "
	@$(ECHO) " "
	@$(ECHO) "Write out the info of the package to $(outputName)/package.info" 
	@$(ECHO) " "
	@$(ECHO) " ========== info of the package  ================"
	@$(ECHO) "   Package Name              : $(outputName)"
	@$(ECHO) "   Tensor version            : $(version)"
	@$(ECHO) "   Symmetry Tensor version   : $(Symmetry_version)"
	@$(ECHO) "   Eigen value version       : $(eigvalue_version)"
	@$(ECHO) "   optimization Tool version : $(optimization_version)"
	@$(ECHO) "   Compile optionl           : $(openmp) $(Optionl)"
	@$(ECHO) " "
	@$(ECHO) " $(FC) version:"
	@$(ECHO) " "
	@$(FC) --version 
	@$(ECHO) " ">$(outputName)/package.info
	@$(ECHO) " ========== info of the package  ================">>$(outputName)/package.info
	@$(ECHO) "   Package Name              : $(outputName)">>$(outputName)/package.info
	@$(ECHO) "   Tensor version            : $(version)">>$(outputName)/package.info
	@$(ECHO) "   Symmetry Tensor version   : $(Symmetry_version)">>$(outputName)/package.info
	@$(ECHO) "   Eigen value version       : $(eigvalue_version)">>$(outputName)/package.info
	@$(ECHO) "   optimization Tool version : $(optimization_version)">>$(outputName)/package.info
	@$(ECHO) "   Compile optionl           : $(openmp) $(Optionl)">>$(outputName)/package.info
	@$(ECHO) " ">>$(outputName)/package.info
	@$(ECHO) " $(FC) version:">>$(outputName)/package.info
	@$(ECHO) " ">>$(outputName)/package.info
	@$(FC) --version >>$(outputName)/package.info
	@$(ECHO) " ">>$(outputName)/package.info
	@$(ECHO) " ">>$(outputName)/package.info
	@$(ECHO) " "
	@$(ECHO) " Report any bugs to Shaojun Dong via ">>$(outputName)/package.info
	@$(ECHO) "    sj.dong@outlook.com ">>$(outputName)/package.info
	@$(ECHO) " ">>$(outputName)/package.info
	@$(ECHO) " ">>$(outputName)/package.info

