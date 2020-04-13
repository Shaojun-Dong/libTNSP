==================
libTNSP README FILE
==================

VERSION:
  Tensor       3.5.8
  Symmetry     4.5.0
  eigvalue     4.0
  optimization 1.0.0

Tensor Network State Package(libTNSP) is a library of Fortran 90 with subroutines
or function for Tensor Network states(TNS). It is freely-available software,
and is copyrighted. If you have any question, send email to Shaojun Dong via

   sj.dong@outlook.com

The distribution tar file contains the Fortran source for libTNSP and the
testing programs, which is a PEPS with simple update method. To compile
the libTNSP, one should link the package to the lapack and blas.






===================
libTNSP INSTALLATION:
===================

 - libTNSP can be installed with make. Configuration have to be set in the
 make.inc file. A make.inc for a Linux machine(ubuntu16.04) running GNU
 compilers is given in the main directory. 
 
 - Do "make" in the current directory to build the standard library 
   "libTensor-3.1”. And other codes for makefile are:
   

                 make : installed the package with the configuration
                        in the file make.inc


           make clean : clean the files of *.o and *.mod according 
                        to the file make.inc
 
        make cleanAll : clean all the files of *.o, *.mod and the package

        make cleanlib : clean the package according to the file make.inc
        
       make cleanAlllib : clean All the packages

       make cleanAllMod : clean All the file of *.mod *.o
   
   
 - Use the code 
 
      "-I$(libTNSP)/libTensor-$(version) -L$(libTNSP)/libTensor-$(version) -llibTensor-$(version)" 
        
  to link your code with the libTNSP, where $(libTNSP) is the libTNSP directory.
  And make sure you have linked the libTNSP with the blas and lapack. If you
  use the package of eigvalue-4.5.0, the arpack is needed.
  
 - you can look for PEPS/makefile to see how to compile and link libTNSP
 
 - The libTNSP can not run under Windows yet. It test OK in linux and Mac.
 
 For further information on libTNSP please read our manual

 or send email to Shaojun Dong  via

   sj.dong@outlook.com



 


==================
libTNSP User Support
==================

libTNSP has been thoroughly tested on many different
types of computers.  If you meet any errors or poor performance, send the 
information to 

       sj.dong@outlook.com. 
       
The information will gain immediate attention from the developer. 


  



==================
libTNSP directories
==================

      src
      Help
      make.inc
      makefile
      README
      manual.pdf
      Tensorlib.pdf
  






================
libTNSP Project
================

developer:


     Shaojun Dong
     Key Laboratory of Quantum Information,
     University of Science and Technology of China.
     Hefei, Anhui, 230026, China.
     Tel: +86-18715010074,
     Email: sj.dong@outlook.com or dsj2010@mail.ustc.edu.cn


Group leaders:


     Lixin He, Professor of Physics
     Key Laboratory of Quantum Information,
     University of Science and Technology of China.
     Hefei, Anhui, 230026, China.
     Email: helx@ustc.edu.cn.
     

     Yong-Jian Han, Professor of Physics
     Key Laboratory of Quantum Information,
     University of Science and Technology of China.
     Hefei, Anhui, 230026, China.
     Email: smhan@ustc.edu.cn

