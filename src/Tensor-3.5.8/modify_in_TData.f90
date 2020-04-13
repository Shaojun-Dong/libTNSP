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

module modify_module
	use Tools
	implicit none

! !******************************************* integer  ***************************

! 	interface modifyTen_val_dim1_int
! 		module procedure modifyTen_val_dim1_int_i
! 		module procedure modifyTen_val_dim1_int_s
! 		module procedure modifyTen_val_dim1_int_d
! 		module procedure modifyTen_val_dim1_int_c
! 		module procedure modifyTen_val_dim1_int_z
! 		module procedure modifyTen_val_dim1_int_l
! 		module procedure modifyTen_val_dim1_int_a
! 	end interface
	
! 	interface modifyTen_array_dim1_int
! 		module procedure modifyTen_array_dim1_int_i
! 		module procedure modifyTen_array_dim1_int_s
! 		module procedure modifyTen_array_dim1_int_d
! 		module procedure modifyTen_array_dim1_int_c
! 		module procedure modifyTen_array_dim1_int_z
! 		module procedure modifyTen_array_dim1_int_l
! 		module procedure modifyTen_array_dim1_int_a
! 	end interface

! 	interface modifyTen_val_dim2_int
! 		module procedure modifyTen_val_dim2_int_i
! 		module procedure modifyTen_val_dim2_int_s
! 		module procedure modifyTen_val_dim2_int_d
! 		module procedure modifyTen_val_dim2_int_c
! 		module procedure modifyTen_val_dim2_int_z
! 		module procedure modifyTen_val_dim2_int_l
! 		module procedure modifyTen_val_dim2_int_a
! 	end interface

! 	interface modifyTen_array_dim2_int
! 		module procedure modifyTen_array_dim2_int_i
! 		module procedure modifyTen_array_dim2_int_s
! 		module procedure modifyTen_array_dim2_int_d
! 		module procedure modifyTen_array_dim2_int_c
! 		module procedure modifyTen_array_dim2_int_z
! 		module procedure modifyTen_array_dim2_int_l
! 		module procedure modifyTen_array_dim2_int_a
! 	end interface

! 	interface modifyTen_val_dim3_int
! 		module procedure modifyTen_val_dim3_int_i
! 		module procedure modifyTen_val_dim3_int_s
! 		module procedure modifyTen_val_dim3_int_d
! 		module procedure modifyTen_val_dim3_int_c
! 		module procedure modifyTen_val_dim3_int_z
! 		module procedure modifyTen_val_dim3_int_l
! 		module procedure modifyTen_val_dim3_int_a
! 	end interface
	
! 	interface modifyTen_array_dim3_int
! 		module procedure modifyTen_array_dim3_int_i
! 		module procedure modifyTen_array_dim3_int_s
! 		module procedure modifyTen_array_dim3_int_d
! 		module procedure modifyTen_array_dim3_int_c
! 		module procedure modifyTen_array_dim3_int_z
! 		module procedure modifyTen_array_dim3_int_l
! 		module procedure modifyTen_array_dim3_int_a
! 	end interface

! 	interface modifyTen_val_dim4_int
! 		module procedure modifyTen_val_dim4_int_i
! 		module procedure modifyTen_val_dim4_int_s
! 		module procedure modifyTen_val_dim4_int_d
! 		module procedure modifyTen_val_dim4_int_c
! 		module procedure modifyTen_val_dim4_int_z
! 		module procedure modifyTen_val_dim4_int_l
! 		module procedure modifyTen_val_dim4_int_a
! 	end interface

! 	interface modifyTen_array_dim4_int
! 		module procedure modifyTen_array_dim4_int_i
! 		module procedure modifyTen_array_dim4_int_s
! 		module procedure modifyTen_array_dim4_int_d
! 		module procedure modifyTen_array_dim4_int_c
! 		module procedure modifyTen_array_dim4_int_z
! 		module procedure modifyTen_array_dim4_int_l
! 		module procedure modifyTen_array_dim4_int_a
! 	end interface

! !******************************************* real*4  ***************************
	
! 	interface modifyTen_val_dim1_real4
! 		module procedure modifyTen_val_dim1_real4_i
! 		module procedure modifyTen_val_dim1_real4_s
! 		module procedure modifyTen_val_dim1_real4_d
! 		module procedure modifyTen_val_dim1_real4_c
! 		module procedure modifyTen_val_dim1_real4_z
! 		module procedure modifyTen_val_dim1_real4_l
! 		module procedure modifyTen_val_dim1_real4_a
! 	end interface

! 	interface modifyTen_array_dim1_real4
! 		module procedure modifyTen_array_dim1_real4_i
! 		module procedure modifyTen_array_dim1_real4_s
! 		module procedure modifyTen_array_dim1_real4_d
! 		module procedure modifyTen_array_dim1_real4_c
! 		module procedure modifyTen_array_dim1_real4_z
! 		module procedure modifyTen_array_dim1_real4_l
! 		module procedure modifyTen_array_dim1_real4_a
! 	end interface

! 	interface modifyTen_val_dim2_real4
! 		module procedure modifyTen_val_dim2_real4_i
! 		module procedure modifyTen_val_dim2_real4_s
! 		module procedure modifyTen_val_dim2_real4_d
! 		module procedure modifyTen_val_dim2_real4_c
! 		module procedure modifyTen_val_dim2_real4_z
! 		module procedure modifyTen_val_dim2_real4_l
! 		module procedure modifyTen_val_dim2_real4_a
! 	end interface

! 	interface modifyTen_array_dim2_real4
! 		module procedure modifyTen_array_dim2_real4_i
! 		module procedure modifyTen_array_dim2_real4_s
! 		module procedure modifyTen_array_dim2_real4_d
! 		module procedure modifyTen_array_dim2_real4_c
! 		module procedure modifyTen_array_dim2_real4_z
! 		module procedure modifyTen_array_dim2_real4_l
! 		module procedure modifyTen_array_dim2_real4_a
! 	end interface

! 	interface modifyTen_val_dim3_real4
! 		module procedure modifyTen_val_dim3_real4_i
! 		module procedure modifyTen_val_dim3_real4_s
! 		module procedure modifyTen_val_dim3_real4_d
! 		module procedure modifyTen_val_dim3_real4_c
! 		module procedure modifyTen_val_dim3_real4_z
! 		module procedure modifyTen_val_dim3_real4_l
! 		module procedure modifyTen_val_dim3_real4_a
! 	end interface

! 	interface modifyTen_array_dim3_real4
! 		module procedure modifyTen_array_dim3_real4_i
! 		module procedure modifyTen_array_dim3_real4_s
! 		module procedure modifyTen_array_dim3_real4_d
! 		module procedure modifyTen_array_dim3_real4_c
! 		module procedure modifyTen_array_dim3_real4_z
! 		module procedure modifyTen_array_dim3_real4_l
! 		module procedure modifyTen_array_dim3_real4_a
! 	end interface

! 	interface modifyTen_val_dim4_real4
! 		module procedure modifyTen_val_dim4_real4_i
! 		module procedure modifyTen_val_dim4_real4_s
! 		module procedure modifyTen_val_dim4_real4_d
! 		module procedure modifyTen_val_dim4_real4_c
! 		module procedure modifyTen_val_dim4_real4_z
! 		module procedure modifyTen_val_dim4_real4_l
! 		module procedure modifyTen_val_dim4_real4_a
! 	end interface
	
! 	interface modifyTen_array_dim4_real4
! 		module procedure modifyTen_array_dim4_real4_i
! 		module procedure modifyTen_array_dim4_real4_s
! 		module procedure modifyTen_array_dim4_real4_d
! 		module procedure modifyTen_array_dim4_real4_c
! 		module procedure modifyTen_array_dim4_real4_z
! 		module procedure modifyTen_array_dim4_real4_l
! 		module procedure modifyTen_array_dim4_real4_a
! 	end interface
	
! !******************************************* real*8  ***************************
	
! 	interface modifyTen_val_dim1_real8
! 		module procedure modifyTen_val_dim1_real8_i
! 		module procedure modifyTen_val_dim1_real8_s
! 		module procedure modifyTen_val_dim1_real8_d
! 		module procedure modifyTen_val_dim1_real8_c
! 		module procedure modifyTen_val_dim1_real8_z
! 		module procedure modifyTen_val_dim1_real8_l
! 		module procedure modifyTen_val_dim1_real8_a
! 	end interface

! 	interface modifyTen_array_dim1_real8
! 		module procedure modifyTen_array_dim1_real8_i
! 		module procedure modifyTen_array_dim1_real8_s
! 		module procedure modifyTen_array_dim1_real8_d
! 		module procedure modifyTen_array_dim1_real8_c
! 		module procedure modifyTen_array_dim1_real8_z
! 		module procedure modifyTen_array_dim1_real8_l
! 		module procedure modifyTen_array_dim1_real8_a
! 	end interface

! 	interface modifyTen_val_dim2_real8
! 		module procedure modifyTen_val_dim2_real8_i
! 		module procedure modifyTen_val_dim2_real8_s
! 		module procedure modifyTen_val_dim2_real8_d
! 		module procedure modifyTen_val_dim2_real8_c
! 		module procedure modifyTen_val_dim2_real8_z
! 		module procedure modifyTen_val_dim2_real8_l
! 		module procedure modifyTen_val_dim2_real8_a
! 	end interface

! 	interface modifyTen_array_dim2_real8
! 		module procedure modifyTen_array_dim2_real8_i
! 		module procedure modifyTen_array_dim2_real8_s
! 		module procedure modifyTen_array_dim2_real8_d
! 		module procedure modifyTen_array_dim2_real8_c
! 		module procedure modifyTen_array_dim2_real8_z
! 		module procedure modifyTen_array_dim2_real8_l
! 		module procedure modifyTen_array_dim2_real8_a
! 	end interface

! 	interface modifyTen_val_dim3_real8
! 		module procedure modifyTen_val_dim3_real8_i
! 		module procedure modifyTen_val_dim3_real8_s
! 		module procedure modifyTen_val_dim3_real8_d
! 		module procedure modifyTen_val_dim3_real8_c
! 		module procedure modifyTen_val_dim3_real8_z
! 		module procedure modifyTen_val_dim3_real8_l
! 		module procedure modifyTen_val_dim3_real8_a
! 	end interface

! 	interface modifyTen_array_dim3_real8
! 		module procedure modifyTen_array_dim3_real8_i
! 		module procedure modifyTen_array_dim3_real8_s
! 		module procedure modifyTen_array_dim3_real8_d
! 		module procedure modifyTen_array_dim3_real8_c
! 		module procedure modifyTen_array_dim3_real8_z
! 		module procedure modifyTen_array_dim3_real8_l
! 		module procedure modifyTen_array_dim3_real8_a
! 	end interface

! 	interface modifyTen_val_dim4_real8
! 		module procedure modifyTen_val_dim4_real8_i
! 		module procedure modifyTen_val_dim4_real8_s
! 		module procedure modifyTen_val_dim4_real8_d
! 		module procedure modifyTen_val_dim4_real8_c
! 		module procedure modifyTen_val_dim4_real8_z
! 		module procedure modifyTen_val_dim4_real8_l
! 		module procedure modifyTen_val_dim4_real8_a
! 	end interface
	
! 	interface modifyTen_array_dim4_real8
! 		module procedure modifyTen_array_dim4_real8_i
! 		module procedure modifyTen_array_dim4_real8_s
! 		module procedure modifyTen_array_dim4_real8_d
! 		module procedure modifyTen_array_dim4_real8_c
! 		module procedure modifyTen_array_dim4_real8_z
! 		module procedure modifyTen_array_dim4_real8_l
! 		module procedure modifyTen_array_dim4_real8_a
! 	end interface	

! !******************************************* complex(kind=4)  ***************************
	
! 	interface modifyTen_val_dim1_com4
! 		module procedure modifyTen_val_dim1_com4_i
! 		module procedure modifyTen_val_dim1_com4_s
! 		module procedure modifyTen_val_dim1_com4_d
! 		module procedure modifyTen_val_dim1_com4_c
! 		module procedure modifyTen_val_dim1_com4_z
! 		module procedure modifyTen_val_dim1_com4_l
! 		module procedure modifyTen_val_dim1_com4_a
! 	end interface

! 	interface modifyTen_array_dim1_com4
! 		module procedure modifyTen_array_dim1_com4_i
! 		module procedure modifyTen_array_dim1_com4_s
! 		module procedure modifyTen_array_dim1_com4_d
! 		module procedure modifyTen_array_dim1_com4_c
! 		module procedure modifyTen_array_dim1_com4_z
! 		module procedure modifyTen_array_dim1_com4_l
! 		module procedure modifyTen_array_dim1_com4_a
! 	end interface

! 	interface modifyTen_val_dim2_com4
! 		module procedure modifyTen_val_dim2_com4_i
! 		module procedure modifyTen_val_dim2_com4_s
! 		module procedure modifyTen_val_dim2_com4_d
! 		module procedure modifyTen_val_dim2_com4_c
! 		module procedure modifyTen_val_dim2_com4_z
! 		module procedure modifyTen_val_dim2_com4_l
! 		module procedure modifyTen_val_dim2_com4_a
! 	end interface

! 	interface modifyTen_array_dim2_com4
! 		module procedure modifyTen_array_dim2_com4_i
! 		module procedure modifyTen_array_dim2_com4_s
! 		module procedure modifyTen_array_dim2_com4_d
! 		module procedure modifyTen_array_dim2_com4_c
! 		module procedure modifyTen_array_dim2_com4_z
! 		module procedure modifyTen_array_dim2_com4_l
! 		module procedure modifyTen_array_dim2_com4_a
! 	end interface

! 	interface modifyTen_val_dim3_com4
! 		module procedure modifyTen_val_dim3_com4_i
! 		module procedure modifyTen_val_dim3_com4_s
! 		module procedure modifyTen_val_dim3_com4_d
! 		module procedure modifyTen_val_dim3_com4_c
! 		module procedure modifyTen_val_dim3_com4_z
! 		module procedure modifyTen_val_dim3_com4_l
! 		module procedure modifyTen_val_dim3_com4_a
! 	end interface

! 	interface modifyTen_array_dim3_com4
! 		module procedure modifyTen_array_dim3_com4_i
! 		module procedure modifyTen_array_dim3_com4_s
! 		module procedure modifyTen_array_dim3_com4_d
! 		module procedure modifyTen_array_dim3_com4_c
! 		module procedure modifyTen_array_dim3_com4_z
! 		module procedure modifyTen_array_dim3_com4_l
! 		module procedure modifyTen_array_dim3_com4_a
! 	end interface

! 	interface modifyTen_val_dim4_com4
! 		module procedure modifyTen_val_dim4_com4_i
! 		module procedure modifyTen_val_dim4_com4_s
! 		module procedure modifyTen_val_dim4_com4_d
! 		module procedure modifyTen_val_dim4_com4_c
! 		module procedure modifyTen_val_dim4_com4_z
! 		module procedure modifyTen_val_dim4_com4_l
! 		module procedure modifyTen_val_dim4_com4_a
! 	end interface
	
! 	interface modifyTen_array_dim4_com4
! 		module procedure modifyTen_array_dim4_com4_i
! 		module procedure modifyTen_array_dim4_com4_s
! 		module procedure modifyTen_array_dim4_com4_d
! 		module procedure modifyTen_array_dim4_com4_c
! 		module procedure modifyTen_array_dim4_com4_z
! 		module procedure modifyTen_array_dim4_com4_l
! 		module procedure modifyTen_array_dim4_com4_a
! 	end interface	

! !******************************************* complex(kind=8)  ***************************

! 	interface modifyTen_val_dim1_com8
! 		module procedure modifyTen_val_dim1_com8_i
! 		module procedure modifyTen_val_dim1_com8_s
! 		module procedure modifyTen_val_dim1_com8_d
! 		module procedure modifyTen_val_dim1_com8_c
! 		module procedure modifyTen_val_dim1_com8_z
! 		module procedure modifyTen_val_dim1_com8_l
! 		module procedure modifyTen_val_dim1_com8_a
! 	end interface

! 	interface modifyTen_array_dim1_com8
! 		module procedure modifyTen_array_dim1_com8_i
! 		module procedure modifyTen_array_dim1_com8_s
! 		module procedure modifyTen_array_dim1_com8_d
! 		module procedure modifyTen_array_dim1_com8_c
! 		module procedure modifyTen_array_dim1_com8_z
! 		module procedure modifyTen_array_dim1_com8_l
! 		module procedure modifyTen_array_dim1_com8_a
! 	end interface

! 	interface modifyTen_val_dim2_com8
! 		module procedure modifyTen_val_dim2_com8_i
! 		module procedure modifyTen_val_dim2_com8_s
! 		module procedure modifyTen_val_dim2_com8_d
! 		module procedure modifyTen_val_dim2_com8_c
! 		module procedure modifyTen_val_dim2_com8_z
! 		module procedure modifyTen_val_dim2_com8_l
! 		module procedure modifyTen_val_dim2_com8_a
! 	end interface

! 	interface modifyTen_array_dim2_com8
! 		module procedure modifyTen_array_dim2_com8_i
! 		module procedure modifyTen_array_dim2_com8_s
! 		module procedure modifyTen_array_dim2_com8_d
! 		module procedure modifyTen_array_dim2_com8_c
! 		module procedure modifyTen_array_dim2_com8_z
! 		module procedure modifyTen_array_dim2_com8_l
! 		module procedure modifyTen_array_dim2_com8_a
! 	end interface

! 	interface modifyTen_val_dim3_com8
! 		module procedure modifyTen_val_dim3_com8_i
! 		module procedure modifyTen_val_dim3_com8_s
! 		module procedure modifyTen_val_dim3_com8_d
! 		module procedure modifyTen_val_dim3_com8_c
! 		module procedure modifyTen_val_dim3_com8_z
! 		module procedure modifyTen_val_dim3_com8_l
! 		module procedure modifyTen_val_dim3_com8_a
! 	end interface

! 	interface modifyTen_array_dim3_com8
! 		module procedure modifyTen_array_dim3_com8_i
! 		module procedure modifyTen_array_dim3_com8_s
! 		module procedure modifyTen_array_dim3_com8_d
! 		module procedure modifyTen_array_dim3_com8_c
! 		module procedure modifyTen_array_dim3_com8_z
! 		module procedure modifyTen_array_dim3_com8_l
! 		module procedure modifyTen_array_dim3_com8_a
! 	end interface

! 	interface modifyTen_val_dim4_com8
! 		module procedure modifyTen_val_dim4_com8_i
! 		module procedure modifyTen_val_dim4_com8_s
! 		module procedure modifyTen_val_dim4_com8_d
! 		module procedure modifyTen_val_dim4_com8_c
! 		module procedure modifyTen_val_dim4_com8_z
! 		module procedure modifyTen_val_dim4_com8_l
! 		module procedure modifyTen_val_dim4_com8_a
! 	end interface
	
! 	interface modifyTen_array_dim4_com8
! 		module procedure modifyTen_array_dim4_com8_i
! 		module procedure modifyTen_array_dim4_com8_s
! 		module procedure modifyTen_array_dim4_com8_d
! 		module procedure modifyTen_array_dim4_com8_c
! 		module procedure modifyTen_array_dim4_com8_z
! 		module procedure modifyTen_array_dim4_com8_l
! 		module procedure modifyTen_array_dim4_com8_a
! 	end interface	

! !******************************************* logical  ***************************

! 	interface modifyTen_val_dim1_logi
! 		module procedure modifyTen_val_dim1_logi_i
! 		module procedure modifyTen_val_dim1_logi_s
! 		module procedure modifyTen_val_dim1_logi_d
! 		module procedure modifyTen_val_dim1_logi_c
! 		module procedure modifyTen_val_dim1_logi_z
! 		module procedure modifyTen_val_dim1_logi_l
! 		module procedure modifyTen_val_dim1_logi_a
! 	end interface

! 	interface modifyTen_array_dim1_logi
! 		module procedure modifyTen_array_dim1_logi_i
! 		module procedure modifyTen_array_dim1_logi_s
! 		module procedure modifyTen_array_dim1_logi_d
! 		module procedure modifyTen_array_dim1_logi_c
! 		module procedure modifyTen_array_dim1_logi_z
! 		module procedure modifyTen_array_dim1_logi_l
! 		module procedure modifyTen_array_dim1_logi_a
! 	end interface

! 	interface modifyTen_val_dim2_logi
! 		module procedure modifyTen_val_dim2_logi_i
! 		module procedure modifyTen_val_dim2_logi_s
! 		module procedure modifyTen_val_dim2_logi_d
! 		module procedure modifyTen_val_dim2_logi_c
! 		module procedure modifyTen_val_dim2_logi_z
! 		module procedure modifyTen_val_dim2_logi_l
! 		module procedure modifyTen_val_dim2_logi_a
! 	end interface

! 	interface modifyTen_array_dim2_logi
! 		module procedure modifyTen_array_dim2_logi_i
! 		module procedure modifyTen_array_dim2_logi_s
! 		module procedure modifyTen_array_dim2_logi_d
! 		module procedure modifyTen_array_dim2_logi_c
! 		module procedure modifyTen_array_dim2_logi_z
! 		module procedure modifyTen_array_dim2_logi_l
! 		module procedure modifyTen_array_dim2_logi_a
! 	end interface

! 	interface modifyTen_val_dim3_logi
! 		module procedure modifyTen_val_dim3_logi_i
! 		module procedure modifyTen_val_dim3_logi_s
! 		module procedure modifyTen_val_dim3_logi_d
! 		module procedure modifyTen_val_dim3_logi_c
! 		module procedure modifyTen_val_dim3_logi_z
! 		module procedure modifyTen_val_dim3_logi_l
! 		module procedure modifyTen_val_dim3_logi_a
! 	end interface

! 	interface modifyTen_array_dim3_logi
! 		module procedure modifyTen_array_dim3_logi_i
! 		module procedure modifyTen_array_dim3_logi_s
! 		module procedure modifyTen_array_dim3_logi_d
! 		module procedure modifyTen_array_dim3_logi_c
! 		module procedure modifyTen_array_dim3_logi_z
! 		module procedure modifyTen_array_dim3_logi_l
! 		module procedure modifyTen_array_dim3_logi_a
! 	end interface

! 	interface modifyTen_val_dim4_logi
! 		module procedure modifyTen_val_dim4_logi_i
! 		module procedure modifyTen_val_dim4_logi_s
! 		module procedure modifyTen_val_dim4_logi_d
! 		module procedure modifyTen_val_dim4_logi_c
! 		module procedure modifyTen_val_dim4_logi_z
! 		module procedure modifyTen_val_dim4_logi_l
! 		module procedure modifyTen_val_dim4_logi_a
! 	end interface
	
! 	interface modifyTen_array_dim4_logi
! 		module procedure modifyTen_array_dim4_logi_i
! 		module procedure modifyTen_array_dim4_logi_s
! 		module procedure modifyTen_array_dim4_logi_d
! 		module procedure modifyTen_array_dim4_logi_c
! 		module procedure modifyTen_array_dim4_logi_z
! 		module procedure modifyTen_array_dim4_logi_l
! 		module procedure modifyTen_array_dim4_logi_a
! 	end interface	

! !******************************************* character  ***************************

! 	interface modifyTen_val_dim1_char
! 		module procedure modifyTen_val_dim1_char_i
! 		module procedure modifyTen_val_dim1_char_s
! 		module procedure modifyTen_val_dim1_char_d
! 		module procedure modifyTen_val_dim1_char_c
! 		module procedure modifyTen_val_dim1_char_z
! 		module procedure modifyTen_val_dim1_char_l
! 		module procedure modifyTen_val_dim1_char_a
! 	end interface
	
! 	interface modifyTen_array_dim1_char
! 		module procedure modifyTen_array_dim1_char_i
! 		module procedure modifyTen_array_dim1_char_s
! 		module procedure modifyTen_array_dim1_char_d
! 		module procedure modifyTen_array_dim1_char_c
! 		module procedure modifyTen_array_dim1_char_z
! 		module procedure modifyTen_array_dim1_char_l
! 		module procedure modifyTen_array_dim1_char_a
! 	end interface

! 	interface modifyTen_val_dim2_char
! 		module procedure modifyTen_val_dim2_char_i
! 		module procedure modifyTen_val_dim2_char_s
! 		module procedure modifyTen_val_dim2_char_d
! 		module procedure modifyTen_val_dim2_char_c
! 		module procedure modifyTen_val_dim2_char_z
! 		module procedure modifyTen_val_dim2_char_l
! 		module procedure modifyTen_val_dim2_char_a
! 	end interface

! 	interface modifyTen_array_dim2_char
! 		module procedure modifyTen_array_dim2_char_i
! 		module procedure modifyTen_array_dim2_char_s
! 		module procedure modifyTen_array_dim2_char_d
! 		module procedure modifyTen_array_dim2_char_c
! 		module procedure modifyTen_array_dim2_char_z
! 		module procedure modifyTen_array_dim2_char_l
! 		module procedure modifyTen_array_dim2_char_a
! 	end interface

! 	interface modifyTen_val_dim3_char
! 		module procedure modifyTen_val_dim3_char_i
! 		module procedure modifyTen_val_dim3_char_s
! 		module procedure modifyTen_val_dim3_char_d
! 		module procedure modifyTen_val_dim3_char_c
! 		module procedure modifyTen_val_dim3_char_z
! 		module procedure modifyTen_val_dim3_char_l
! 		module procedure modifyTen_val_dim3_char_a
! 	end interface
	
! 	interface modifyTen_array_dim3_char
! 		module procedure modifyTen_array_dim3_char_i
! 		module procedure modifyTen_array_dim3_char_s
! 		module procedure modifyTen_array_dim3_char_d
! 		module procedure modifyTen_array_dim3_char_c
! 		module procedure modifyTen_array_dim3_char_z
! 		module procedure modifyTen_array_dim3_char_l
! 		module procedure modifyTen_array_dim3_char_a
! 	end interface

! 	interface modifyTen_val_dim4_char
! 		module procedure modifyTen_val_dim4_char_i
! 		module procedure modifyTen_val_dim4_char_s
! 		module procedure modifyTen_val_dim4_char_d
! 		module procedure modifyTen_val_dim4_char_c
! 		module procedure modifyTen_val_dim4_char_z
! 		module procedure modifyTen_val_dim4_char_l
! 		module procedure modifyTen_val_dim4_char_a
! 	end interface

! 	interface modifyTen_array_dim4_char
! 		module procedure modifyTen_array_dim4_char_i
! 		module procedure modifyTen_array_dim4_char_s
! 		module procedure modifyTen_array_dim4_char_d
! 		module procedure modifyTen_array_dim4_char_c
! 		module procedure modifyTen_array_dim4_char_z
! 		module procedure modifyTen_array_dim4_char_l
! 		module procedure modifyTen_array_dim4_char_a
! 	end interface

! !***********************************************************************************************************
! !
! !        A(i1:i2)=B(i1:i2)
! !
! !***********************************************************************************************************

! 	interface store_value_int
! 		module procedure store_value_int_i
! 		module procedure store_value_int_s
! 		module procedure store_value_int_d
! 		module procedure store_value_int_c
! 		module procedure store_value_int_z
! 		module procedure store_value_int_l
! 		module procedure store_value_int_a
! 	end interface

! 	interface store_value_real4
! 		module procedure store_value_real4_i
! 		module procedure store_value_real4_s
! 		module procedure store_value_real4_d
! 		module procedure store_value_real4_c
! 		module procedure store_value_real4_z
! 		module procedure store_value_real4_l
! 		module procedure store_value_real4_a
! 	end interface

! 	interface store_value_real8
! 		module procedure store_value_real8_i
! 		module procedure store_value_real8_s
! 		module procedure store_value_real8_d
! 		module procedure store_value_real8_c
! 		module procedure store_value_real8_z
! 		module procedure store_value_real8_l
! 		module procedure store_value_real8_a
! 	end interface
	
! 	interface store_value_com4
! 		module procedure store_value_com4_i
! 		module procedure store_value_com4_s
! 		module procedure store_value_com4_d
! 		module procedure store_value_com4_c
! 		module procedure store_value_com4_z
! 		module procedure store_value_com4_l
! 		module procedure store_value_com4_a
! 	end interface

! 	interface store_value_com8
! 		module procedure store_value_com8_i
! 		module procedure store_value_com8_s
! 		module procedure store_value_com8_d
! 		module procedure store_value_com8_c
! 		module procedure store_value_com8_z
! 		module procedure store_value_com8_l
! 		module procedure store_value_com8_a
! 	end interface

! 	interface store_value_logi
! 		module procedure store_value_logi_i
! 		module procedure store_value_logi_s
! 		module procedure store_value_logi_d
! 		module procedure store_value_logi_c
! 		module procedure store_value_logi_z
! 		module procedure store_value_logi_l
! 		module procedure store_value_logi_a
! 	end interface

! 	interface store_value_char
! 		module procedure store_value_char_i
! 		module procedure store_value_char_s
! 		module procedure store_value_char_d
! 		module procedure store_value_char_c
! 		module procedure store_value_char_z
! 		module procedure store_value_char_l
! 		module procedure store_value_char_a
! 	end interface

! !***********************************************************************************************************
! !
! !        A(i1:i2,j1:j2)=B(i1:i2,j1:j2)
! !
! !***********************************************************************************************************
	
! 	interface store_value2_int
! 		module procedure store_value2_int_i
! 		module procedure store_value2_int_s
! 		module procedure store_value2_int_d
! 		module procedure store_value2_int_c
! 		module procedure store_value2_int_z
! 		module procedure store_value2_int_l
! 		module procedure store_value2_int_a
! 	end interface

! 	interface store_value2_real4
! 		module procedure store_value2_real4_i
! 		module procedure store_value2_real4_s
! 		module procedure store_value2_real4_d
! 		module procedure store_value2_real4_c
! 		module procedure store_value2_real4_z
! 		module procedure store_value2_real4_l
! 		module procedure store_value2_real4_a
! 	end interface

! 	interface store_value2_real8
! 		module procedure store_value2_real8_i
! 		module procedure store_value2_real8_s
! 		module procedure store_value2_real8_d
! 		module procedure store_value2_real8_c
! 		module procedure store_value2_real8_z
! 		module procedure store_value2_real8_l
! 		module procedure store_value2_real8_a
! 	end interface

! 	interface store_value2_com4
! 		module procedure store_value2_com4_i
! 		module procedure store_value2_com4_s
! 		module procedure store_value2_com4_d
! 		module procedure store_value2_com4_c
! 		module procedure store_value2_com4_z
! 		module procedure store_value2_com4_l
! 		module procedure store_value2_com4_a
! 	end interface

! 	interface store_value2_com8
! 		module procedure store_value2_com8_i
! 		module procedure store_value2_com8_s
! 		module procedure store_value2_com8_d
! 		module procedure store_value2_com8_c
! 		module procedure store_value2_com8_z
! 		module procedure store_value2_com8_l
! 		module procedure store_value2_com8_a
! 	end interface

! 	interface store_value2_logi
! 		module procedure store_value2_logi_i
! 		module procedure store_value2_logi_s
! 		module procedure store_value2_logi_d
! 		module procedure store_value2_logi_c
! 		module procedure store_value2_logi_z
! 		module procedure store_value2_logi_l
! 		module procedure store_value2_logi_a
! 	end interface

! 	interface store_value2_char
! 		module procedure store_value2_char_i
! 		module procedure store_value2_char_s
! 		module procedure store_value2_char_d
! 		module procedure store_value2_char_c
! 		module procedure store_value2_char_z
! 		module procedure store_value2_char_l
! 		module procedure store_value2_char_a
! 	end interface


! !***********************************************************************************************************
! !
! !        A(i1:i2,j1:j2,k1:k2)=B(i1:i2,j1:j2,k1:k2)
! !
! !***********************************************************************************************************

! 	interface store_value3_int
! 		module procedure store_value3_int_i
! 		module procedure store_value3_int_s
! 		module procedure store_value3_int_d
! 		module procedure store_value3_int_c
! 		module procedure store_value3_int_z
! 		module procedure store_value3_int_l
! 		module procedure store_value3_int_a
! 	end interface

! 	interface store_value3_real4
! 		module procedure store_value3_real4_i
! 		module procedure store_value3_real4_s
! 		module procedure store_value3_real4_d
! 		module procedure store_value3_real4_c
! 		module procedure store_value3_real4_z
! 		module procedure store_value3_real4_l
! 		module procedure store_value3_real4_a
! 	end interface

! 	interface store_value3_real8
! 		module procedure store_value3_real8_i
! 		module procedure store_value3_real8_s
! 		module procedure store_value3_real8_d
! 		module procedure store_value3_real8_c
! 		module procedure store_value3_real8_z
! 		module procedure store_value3_real8_l
! 		module procedure store_value3_real8_a
! 	end interface

! 	interface store_value3_com4
! 		module procedure store_value3_com4_i
! 		module procedure store_value3_com4_s
! 		module procedure store_value3_com4_d
! 		module procedure store_value3_com4_c
! 		module procedure store_value3_com4_z
! 		module procedure store_value3_com4_l
! 		module procedure store_value3_com4_a
! 	end interface

! 	interface store_value3_com8
! 		module procedure store_value3_com8_i
! 		module procedure store_value3_com8_s
! 		module procedure store_value3_com8_d
! 		module procedure store_value3_com8_c
! 		module procedure store_value3_com8_z
! 		module procedure store_value3_com8_l
! 		module procedure store_value3_com8_a
! 	end interface

! 	interface store_value3_logi
! 		module procedure store_value3_logi_i
! 		module procedure store_value3_logi_s
! 		module procedure store_value3_logi_d
! 		module procedure store_value3_logi_c
! 		module procedure store_value3_logi_z
! 		module procedure store_value3_logi_l
! 		module procedure store_value3_logi_a
! 	end interface

! 	interface store_value3_char
! 		module procedure store_value3_char_i
! 		module procedure store_value3_char_s
! 		module procedure store_value3_char_d
! 		module procedure store_value3_char_c
! 		module procedure store_value3_char_z
! 		module procedure store_value3_char_l
! 		module procedure store_value3_char_a
! 	end interface


! !***********************************************************************************************************
! !
! !        A(i1:i2,j1:j2,k1:k2,l1:l2)=B(i1:i2,j1:j2,k1:k2,l1:l2)
! !
! !***********************************************************************************************************
	
! 	interface store_value4_int
! 		module procedure store_value4_int_i
! 		module procedure store_value4_int_s
! 		module procedure store_value4_int_d
! 		module procedure store_value4_int_c
! 		module procedure store_value4_int_z
! 		module procedure store_value4_int_l
! 		module procedure store_value4_int_a
! 	end interface

! 	interface store_value4_real4
! 		module procedure store_value4_real4_i
! 		module procedure store_value4_real4_s
! 		module procedure store_value4_real4_d
! 		module procedure store_value4_real4_c
! 		module procedure store_value4_real4_z
! 		module procedure store_value4_real4_l
! 		module procedure store_value4_real4_a
! 	end interface

! 	interface store_value4_real8
! 		module procedure store_value4_real8_i
! 		module procedure store_value4_real8_s
! 		module procedure store_value4_real8_d
! 		module procedure store_value4_real8_c
! 		module procedure store_value4_real8_z
! 		module procedure store_value4_real8_l
! 		module procedure store_value4_real8_a
! 	end interface

! 	interface store_value4_com4
! 		module procedure store_value4_com4_i
! 		module procedure store_value4_com4_s
! 		module procedure store_value4_com4_d
! 		module procedure store_value4_com4_c
! 		module procedure store_value4_com4_z
! 		module procedure store_value4_com4_l
! 		module procedure store_value4_com4_a
! 	end interface

! 	interface store_value4_com8
! 		module procedure store_value4_com8_i
! 		module procedure store_value4_com8_s
! 		module procedure store_value4_com8_d
! 		module procedure store_value4_com8_c
! 		module procedure store_value4_com8_z
! 		module procedure store_value4_com8_l
! 		module procedure store_value4_com8_a
! 	end interface

! 	interface store_value4_logi
! 		module procedure store_value4_logi_i
! 		module procedure store_value4_logi_s
! 		module procedure store_value4_logi_d
! 		module procedure store_value4_logi_c
! 		module procedure store_value4_logi_z
! 		module procedure store_value4_logi_l
! 		module procedure store_value4_logi_a
! 	end interface

! 	interface store_value4_char
! 		module procedure store_value4_char_i
! 		module procedure store_value4_char_s
! 		module procedure store_value4_char_d
! 		module procedure store_value4_char_c
! 		module procedure store_value4_char_z
! 		module procedure store_value4_char_l
! 		module procedure store_value4_char_a
! 	end interface

contains


!******************************************* integer  ***************************

	!int(i)=class(*)

	subroutine modifyTen_val_dim1_int_i(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		integer,intent(inout)::Tdata(LenT)
		integer,intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_int_s(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		integer,intent(inout)::Tdata(LenT)
		real*4,intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_int_d(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		integer,intent(inout)::Tdata(LenT)
		real*8,intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_int_c(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		integer,intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_int_z(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		integer,intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_int_l(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		integer,intent(inout)::Tdata(LenT)
		logical,intent(in)::value
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim1_int_a(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		integer,intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_int_i(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		integer,intent(inout)::Tdata(LenT)
		integer,intent(in)::value(LenV)
		Tdata(i(1):i(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim1_int_s(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		integer,intent(inout)::Tdata(LenT)
		real*4,intent(in)::value(LenV)
		Tdata(i(1):i(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim1_int_d(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		integer,intent(inout)::Tdata(LenT)
		real*8,intent(in)::value(LenV)
		Tdata(i(1):i(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim1_int_c(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		integer,intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim1_int_z(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		integer,intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim1_int_l(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		integer,intent(inout)::Tdata(LenT)
		logical,intent(in)::value(LenV)
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
		return
	end subroutine

	subroutine modifyTen_array_dim1_int_a(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		integer,intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value(LenV)
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
		return
	end subroutine

	subroutine modifyTen_val_dim2_int_i(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		integer,intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_int_s(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		integer,intent(inout)::Tdata(LD1,LD2)
		real*4,intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_int_d(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		integer,intent(inout)::Tdata(LD1,LD2)
		real*8,intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_int_c(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		integer,intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_int_z(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		integer,intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_int_l(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		integer,intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_int_a(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		integer,intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_int_i(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		integer,intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim2_int_s(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		integer,intent(inout)::Tdata(LD1,LD2)
		real*4,intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim2_int_d(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		integer,intent(inout)::Tdata(LD1,LD2)
		real*8,intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim2_int_c(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		integer,intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim2_int_z(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		integer,intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim2_int_l(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		integer,intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
		return
	end subroutine

	subroutine modifyTen_array_dim2_int_a(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		integer,intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
		return
	end subroutine

	subroutine modifyTen_val_dim3_int_i(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_int_s(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		real*4,intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_int_d(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		real*8,intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_int_c(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_int_z(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_int_l(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_int_a(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_int_i(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_int_s(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		real*4,intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_int_d(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		real*8,intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_int_c(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_int_z(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_int_l(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_int_a(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		integer,intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
		return
	end subroutine

	subroutine modifyTen_val_dim4_int_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_int_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real*4,intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_int_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real*8,intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_int_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_int_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_int_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_int_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_int_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_int_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real*4,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_int_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real*8,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_int_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_int_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_int_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_int_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		integer,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
	end subroutine


!******************************************* real*4  ***************************
	!real*4(i)=class(*)

	subroutine modifyTen_val_dim1_real4_i(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=4),intent(inout)::Tdata(LenT)
		integer,intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_real4_s(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=4),intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_real4_d(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=4),intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_real4_c(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=4),intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_real4_z(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=4),intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_real4_l(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=4),intent(inout)::Tdata(LenT)
		logical,intent(in)::value
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim1_real4_a(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=4),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_real4_i(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=4),intent(inout)::Tdata(LenT)
		integer,intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_real4_s(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=4),intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_real4_d(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=4),intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_real4_c(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=4),intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_real4_z(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=4),intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_real4_l(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=4),intent(inout)::Tdata(LenT)
		logical,intent(in)::value(LenV)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_real4_a(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=4),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value(LenV)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_real4_i(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_real4_s(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_real4_d(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_real4_c(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_real4_z(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_real4_l(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_real4_a(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_real4_i(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_real4_s(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_real4_d(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_real4_c(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_real4_z(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_real4_l(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_real4_a(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=4),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_real4_i(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_real4_s(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_real4_d(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_real4_c(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_real4_z(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_real4_l(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_real4_a(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_real4_i(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_real4_s(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_real4_d(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_real4_c(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_real4_z(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_real4_l(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_real4_a(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_real4_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_real4_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_real4_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_real4_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_real4_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_real4_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_real4_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_real4_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_real4_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_real4_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_real4_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_real4_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_real4_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
		return
	end subroutine

	subroutine modifyTen_array_dim4_real4_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
		return
	end subroutine

!******************************************* real*8  ***************************
	!real*8(i)=class(*)

	subroutine modifyTen_val_dim1_real8_i(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=8),intent(inout)::Tdata(LenT)
		integer,intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_real8_s(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=8),intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_real8_d(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=8),intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_real8_c(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=8),intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_real8_z(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=8),intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_real8_l(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=8),intent(inout)::Tdata(LenT)
		logical,intent(in)::value
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim1_real8_a(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		real(kind=8),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_real8_i(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=8),intent(inout)::Tdata(LenT)
		integer,intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_real8_s(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=8),intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_real8_d(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=8),intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_real8_c(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=8),intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_real8_z(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=8),intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_real8_l(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=8),intent(inout)::Tdata(LenT)
		logical,intent(in)::value(LenV)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_real8_a(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		real(kind=8),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value(LenV)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_real8_i(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_real8_s(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_real8_d(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_real8_c(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_real8_z(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_real8_l(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_real8_a(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_real8_i(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_real8_s(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_real8_d(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_real8_c(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_real8_z(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_real8_l(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_real8_a(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		real(kind=8),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_real8_i(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_real8_s(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_real8_d(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_real8_c(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_real8_z(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_real8_l(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_real8_a(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_real8_i(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_real8_s(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_real8_d(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_real8_c(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_real8_z(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_real8_l(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_real8_a(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_real8_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_real8_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_real8_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_real8_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_real8_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_real8_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_real8_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_real8_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_real8_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_real8_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_real8_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_real8_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_real8_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
		return
	end subroutine

	subroutine modifyTen_array_dim4_real8_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		real(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
		return
	end subroutine

!******************************************* complex(kind=4)  ***************************
	!complex(kind=4) (i)=class(*)

	subroutine modifyTen_val_dim1_com4_i(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=4),intent(inout)::Tdata(LenT)
		integer,intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_com4_s(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=4),intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_com4_d(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=4),intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_com4_c(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=4),intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_com4_z(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=4),intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_com4_l(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=4),intent(inout)::Tdata(LenT)
		logical,intent(in)::value
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim1_com4_a(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=4),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_com4_i(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=4),intent(inout)::Tdata(LenT)
		integer,intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_com4_s(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=4),intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_com4_d(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=4),intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_com4_c(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=4),intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_com4_z(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=4),intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_com4_l(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=4),intent(inout)::Tdata(LenT)
		logical,intent(in)::value(LenV)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_com4_a(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=4),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value(LenV)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_com4_i(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_com4_s(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_com4_d(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_com4_c(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_com4_z(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_com4_l(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_com4_a(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_com4_i(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_com4_s(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_com4_d(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_com4_c(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_com4_z(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_com4_l(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_com4_a(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=4),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_com4_i(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_com4_s(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_com4_d(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_com4_c(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_com4_z(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_com4_l(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_com4_a(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_com4_i(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_com4_s(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_com4_d(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_com4_c(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_com4_z(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_com4_l(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_com4_a(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_com4_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_com4_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_com4_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_com4_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_com4_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_com4_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_com4_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_com4_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_com4_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_com4_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_com4_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_com4_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_com4_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
		return
	end subroutine

	subroutine modifyTen_array_dim4_com4_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=4),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
		return
	end subroutine

!******************************************* complex(kind=8)  ***************************
	!complex(kind=8) (i)=class(*)

	subroutine modifyTen_val_dim1_com8_i(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=8),intent(inout)::Tdata(LenT)
		integer,intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_com8_s(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=8),intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_com8_d(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=8),intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_com8_c(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=8),intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_com8_z(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=8),intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_com8_l(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=8),intent(inout)::Tdata(LenT)
		logical,intent(in)::value
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim1_com8_a(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		complex(kind=8),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_com8_i(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=8),intent(inout)::Tdata(LenT)
		integer,intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_com8_s(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=8),intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_com8_d(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=8),intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_com8_c(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=8),intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_com8_z(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=8),intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_com8_l(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=8),intent(inout)::Tdata(LenT)
		logical,intent(in)::value(LenV)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_com8_a(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		complex(kind=8),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value(LenV)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_com8_i(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_com8_s(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_com8_d(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_com8_c(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_com8_z(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_com8_l(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_com8_a(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_com8_i(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_com8_s(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_com8_d(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_com8_c(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_com8_z(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_com8_l(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_com8_a(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		complex(kind=8),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_com8_i(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_com8_s(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_com8_d(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_com8_c(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_com8_z(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_com8_l(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_com8_a(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_com8_i(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_com8_s(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_com8_d(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_com8_c(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_com8_z(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_com8_l(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_com8_a(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_com8_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_com8_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_com8_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_com8_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_com8_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_com8_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_com8_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_com8_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_com8_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_com8_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_com8_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_com8_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_com8_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
		return
	end subroutine

	subroutine modifyTen_array_dim4_com8_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		complex(kind=8),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
		return
	end subroutine

!******************************************* logical  ***************************
	!logical (i)=class(*)

	subroutine modifyTen_val_dim1_logi_i(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		logical,intent(inout)::Tdata(LenT)
		integer,intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim1_logi_s(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		logical,intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim1_logi_d(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		logical,intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim1_logi_c(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		logical,intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim1_logi_z(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		logical,intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim1_logi_l(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		logical,intent(inout)::Tdata(LenT)
		logical,intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_logi_a(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		logical,intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, logical=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_logi_i(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		logical,intent(inout)::Tdata(LenT)
		integer,intent(in)::value(LenV)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_logi_s(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		logical,intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value(LenV)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_logi_d(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		logical,intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value(LenV)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_logi_c(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		logical,intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value(LenV)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_logi_z(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		logical,intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value(LenV)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim1_logi_l(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		logical,intent(inout)::Tdata(LenT)
		logical,intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_logi_a(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		logical,intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value(LenV)
		call writemess('ERROR type when setting, logical=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_logi_i(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		logical,intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_logi_s(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		logical,intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_logi_d(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		logical,intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_logi_c(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		logical,intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_logi_z(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		logical,intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim2_logi_l(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		logical,intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_logi_a(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		logical,intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, logical=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_logi_i(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		logical,intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_logi_s(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		logical,intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_logi_d(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		logical,intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_logi_c(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		logical,intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_logi_z(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		logical,intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim2_logi_l(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		logical,intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_array_dim2_logi_a(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		logical,intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value(LDV1,LDV2)
		call writemess('ERROR type when setting, logical=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_logi_i(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_logi_s(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_logi_d(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_logi_c(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_logi_z(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim3_logi_l(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_logi_a(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, logical=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_logi_i(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_logi_s(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_logi_d(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_logi_c(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_logi_z(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim3_logi_l(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_logi_a(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		logical,intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3)
		call writemess('ERROR type when setting, logical=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_logi_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_logi_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_logi_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_logi_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_logi_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_val_dim4_logi_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_logi_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value
		call writemess('ERROR type when setting, logical=character ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_logi_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_logi_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_logi_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_logi_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_logi_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine modifyTen_array_dim4_logi_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_logi_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		logical,intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		call writemess('ERROR type when setting, logical=character ')
		call error_stop
		return
	end subroutine

!******************************************* character  ***************************
	!character (i)=class(*)

	subroutine modifyTen_val_dim1_char_i(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		character(len=*),intent(inout)::Tdata(LenT)
		integer,intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_char_s(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		character(len=*),intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_char_d(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		character(len=*),intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_char_c(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		character(len=*),intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_char_z(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		character(len=*),intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_char_l(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		character(len=*),intent(inout)::Tdata(LenT)
		logical,intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_val_dim1_char_a(i,Tdata,LenT,value)
		integer,intent(in)::i,LenT
		character(len=*),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value
		Tdata(i)=value
		return
	end subroutine

	subroutine modifyTen_array_dim1_char_i(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		character(len=*),intent(inout)::Tdata(LenT)
		integer,intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_char_s(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		character(len=*),intent(inout)::Tdata(LenT)
		real(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_char_d(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		character(len=*),intent(inout)::Tdata(LenT)
		real(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_char_c(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		character(len=*),intent(inout)::Tdata(LenT)
		complex(kind=4),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_char_z(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		character(len=*),intent(inout)::Tdata(LenT)
		complex(kind=8),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_char_l(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		character(len=*),intent(inout)::Tdata(LenT)
		logical,intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_array_dim1_char_a(i,Tdata,LenT,value,LenV)
		integer,intent(in)::i(2),LenT,LenV
		character(len=*),intent(inout)::Tdata(LenT)
		character(len=*),intent(in)::value(LenV)
		Tdata(i(1):i(2))=value(1:LenV)
		return
	end subroutine

	subroutine modifyTen_val_dim2_char_i(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_char_s(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_char_d(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_char_c(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_char_z(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_char_l(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_val_dim2_char_a(i,j,Tdata,LD1,LD2,value)
		integer,intent(in)::i,j,LD1,LD2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value
		Tdata(i,j)=value
		return
	end subroutine

	subroutine modifyTen_array_dim2_char_i(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		integer,intent(in)::value(LDV1,LDV2)
		integer::ii,jj
		do jj=j(1),j(2)
			do ii=i(1),i(2)
				Tdata(ii,jj)=value(ii-i(1)+1,jj-j(1)+1)
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim2_char_s(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		real(kind=4),intent(in)::value(LDV1,LDV2)
		integer::ii,jj
		do jj=j(1),j(2)
			do ii=i(1),i(2)
				Tdata(ii,jj)=value(ii-i(1)+1,jj-j(1)+1)
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim2_char_d(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		real(kind=8),intent(in)::value(LDV1,LDV2)
		integer::ii,jj
		do jj=j(1),j(2)
			do ii=i(1),i(2)
				Tdata(ii,jj)=value(ii-i(1)+1,jj-j(1)+1)
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim2_char_c(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		complex(kind=4),intent(in)::value(LDV1,LDV2)
		integer::ii,jj
		do jj=j(1),j(2)
			do ii=i(1),i(2)
				Tdata(ii,jj)=value(ii-i(1)+1,jj-j(1)+1)
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim2_char_z(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		complex(kind=8),intent(in)::value(LDV1,LDV2)
		integer::ii,jj
		do jj=j(1),j(2)
			do ii=i(1),i(2)
				Tdata(ii,jj)=value(ii-i(1)+1,jj-j(1)+1)
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim2_char_l(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		logical,intent(in)::value(LDV1,LDV2)
		integer::ii,jj
		do jj=j(1),j(2)
			do ii=i(1),i(2)
				Tdata(ii,jj)=value(ii-i(1)+1,jj-j(1)+1)
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim2_char_a(i,j,Tdata,LD1,LD2,value,LDV1,LDV2)
		integer,intent(in)::i(2),j(2),LD1,LD2,LDV1,LDV2
		character(len=*),intent(inout)::Tdata(LD1,LD2)
		character(len=*),intent(in)::value(LDV1,LDV2)
		Tdata(i(1):i(2),j(1):j(2))=value(1:LDV1,1:LDV2)
		return
	end subroutine

	subroutine modifyTen_val_dim3_char_i(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_char_s(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_char_d(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_char_c(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_char_z(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_char_l(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim3_char_a(i,j,l,Tdata,LD1,LD2,LD3,value)
		integer,intent(in)::i,j,l,LD1,LD2,LD3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value
		Tdata(i,j,l)=value
		return
	end subroutine

	subroutine modifyTen_array_dim3_char_i(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		integer,intent(in)::value(LDV1,LDV2,LDV3)
		integer::ii,jj,ll
		do ll=l(1),l(2)
			do jj=j(1),j(2)
				do ii=i(1),i(2)
					Tdata(ii,jj,ll)=value(ii-i(1)+1,jj-j(1)+1,ll-l(1)+1)
				end do
			end do
		end do
		return
		return
	end subroutine

	subroutine modifyTen_array_dim3_char_s(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		integer::ii,jj,ll
		do ll=l(1),l(2)
			do jj=j(1),j(2)
				do ii=i(1),i(2)
					Tdata(ii,jj,ll)=value(ii-i(1)+1,jj-j(1)+1,ll-l(1)+1)
				end do
			end do
		end do
		return
		return
	end subroutine

	subroutine modifyTen_array_dim3_char_d(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		integer::ii,jj,ll
		do ll=l(1),l(2)
			do jj=j(1),j(2)
				do ii=i(1),i(2)
					Tdata(ii,jj,ll)=value(ii-i(1)+1,jj-j(1)+1,ll-l(1)+1)
				end do
			end do
		end do
		return
		return
	end subroutine

	subroutine modifyTen_array_dim3_char_c(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3)
		integer::ii,jj,ll
		do ll=l(1),l(2)
			do jj=j(1),j(2)
				do ii=i(1),i(2)
					Tdata(ii,jj,ll)=value(ii-i(1)+1,jj-j(1)+1,ll-l(1)+1)
				end do
			end do
		end do
		return
		return
	end subroutine

	subroutine modifyTen_array_dim3_char_z(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3)
		integer::ii,jj,ll
		do ll=l(1),l(2)
			do jj=j(1),j(2)
				do ii=i(1),i(2)
					Tdata(ii,jj,ll)=value(ii-i(1)+1,jj-j(1)+1,ll-l(1)+1)
				end do
			end do
		end do
		return
		return
	end subroutine

	subroutine modifyTen_array_dim3_char_l(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		logical,intent(in)::value(LDV1,LDV2,LDV3)
		integer::ii,jj,ll
		do ll=l(1),l(2)
			do jj=j(1),j(2)
				do ii=i(1),i(2)
					Tdata(ii,jj,ll)=value(ii-i(1)+1,jj-j(1)+1,ll-l(1)+1)
				end do
			end do
		end do
		return
		return
	end subroutine

	subroutine modifyTen_array_dim3_char_a(i,j,l,Tdata,LD1,LD2,LD3,value,LDV1,LDV2,LDV3)
		integer,intent(in)::i(2),j(2),l(2),LD1,LD2,LD3,LDV1,LDV2,LDV3
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3)
		Tdata(i(1):i(2),j(1):j(2),l(1):l(2))=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_char_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_char_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_char_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_char_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_char_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_char_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_val_dim4_char_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value)
		integer,intent(in)::i,j,k,l,LD1,LD2,LD3,LD4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value
		Tdata(i,j,k,l)=value
		return
	end subroutine

	subroutine modifyTen_array_dim4_char_i(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		integer,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		integer::ii,jj,kk,ll
		do ll=l(1),l(2)
			do kk=k(1),k(2)
				do jj=j(1),j(2)
					do ii=i(1),i(2)
						Tdata(ii,jj,kk,ll)=value(ii-i(1)+1,jj-j(1)+1,kk-k(1)+1,ll-l(1)+1)
					end do
				end do
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim4_char_s(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		integer::ii,jj,kk,ll
		do ll=l(1),l(2)
			do kk=k(1),k(2)
				do jj=j(1),j(2)
					do ii=i(1),i(2)
						Tdata(ii,jj,kk,ll)=value(ii-i(1)+1,jj-j(1)+1,kk-k(1)+1,ll-l(1)+1)
					end do
				end do
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim4_char_d(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		real(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		integer::ii,jj,kk,ll
		do ll=l(1),l(2)
			do kk=k(1),k(2)
				do jj=j(1),j(2)
					do ii=i(1),i(2)
						Tdata(ii,jj,kk,ll)=value(ii-i(1)+1,jj-j(1)+1,kk-k(1)+1,ll-l(1)+1)
					end do
				end do
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim4_char_c(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=4),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		integer::ii,jj,kk,ll
		do ll=l(1),l(2)
			do kk=k(1),k(2)
				do jj=j(1),j(2)
					do ii=i(1),i(2)
						Tdata(ii,jj,kk,ll)=value(ii-i(1)+1,jj-j(1)+1,kk-k(1)+1,ll-l(1)+1)
					end do
				end do
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim4_char_z(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		complex(kind=8),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		integer::ii,jj,kk,ll
		do ll=l(1),l(2)
			do kk=k(1),k(2)
				do jj=j(1),j(2)
					do ii=i(1),i(2)
						Tdata(ii,jj,kk,ll)=value(ii-i(1)+1,jj-j(1)+1,kk-k(1)+1,ll-l(1)+1)
					end do
				end do
			end do
		end do
	end subroutine

	subroutine modifyTen_array_dim4_char_l(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		logical,intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		integer::ii,jj,kk,ll
		do ll=l(1),l(2)
			do kk=k(1),k(2)
				do jj=j(1),j(2)
					do ii=i(1),i(2)
						Tdata(ii,jj,kk,ll)=value(ii-i(1)+1,jj-j(1)+1,kk-k(1)+1,ll-l(1)+1)
					end do
				end do
			end do
		end do
		return
	end subroutine

	subroutine modifyTen_array_dim4_char_a(i,j,k,l,Tdata,LD1,LD2,LD3,LD4,value,LDV1,LDV2,LDV3,LDV4)
		integer,intent(in)::i(2),j(2),k(2),l(2),LD1,LD2,LD3,LD4,LDV1,LDV2,LDV3,LDV4
		character(len=*),intent(inout)::Tdata(LD1,LD2,LD3,LD4)
		character(len=*),intent(in)::value(LDV1,LDV2,LDV3,LDV4)
		Tdata(i(1):i(2),j(1):j(2),k(1):k(2),l(1):l(2))=value
		return
	end subroutine



!***********************************************************************************************************
!        A(i1:i2)=B(i1:i2)
!***********************************************************************************************************


!********************************* integer ***********************************************

	subroutine store_value_int_i(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		integer,intent(inout)::A(LDA)
		integer,intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_int_s(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		integer,intent(inout)::A(LDA)
		real(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_int_d(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		integer,intent(inout)::A(LDA)
		real(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_int_c(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		integer,intent(inout)::A(LDA)
		complex(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_int_z(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		integer,intent(inout)::A(LDA)
		complex(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_int_l(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		integer,intent(inout)::A(LDA)
		logical,intent(in)::B(LDB)
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
	end subroutine

	subroutine store_value_int_a(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		integer,intent(inout)::A(LDA)
		character(len=*),intent(in)::B(LDB)
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
	end subroutine


!********************************* real(kind=4) ***********************************************

	subroutine store_value_real4_i(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=4),intent(inout)::A(LDA)
		integer,intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_real4_s(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=4),intent(inout)::A(LDA)
		real(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_real4_d(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=4),intent(inout)::A(LDA)
		real(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_real4_c(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=4),intent(inout)::A(LDA)
		complex(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_real4_z(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=4),intent(inout)::A(LDA)
		complex(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_real4_l(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=4),intent(inout)::A(LDA)
		logical,intent(in)::B(LDB)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine store_value_real4_a(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=4),intent(inout)::A(LDA)
		character(len=*),intent(in)::B(LDB)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine


!********************************* real(kind=8) ***********************************************

	subroutine store_value_real8_i(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=8),intent(inout)::A(LDA)
		integer,intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_real8_s(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=8),intent(inout)::A(LDA)
		real(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_real8_d(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=8),intent(inout)::A(LDA)
		real(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_real8_c(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=8),intent(inout)::A(LDA)
		complex(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_real8_z(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=8),intent(inout)::A(LDA)
		complex(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_real8_l(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=8),intent(inout)::A(LDA)
		logical,intent(in)::B(LDB)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine store_value_real8_a(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		real(kind=8),intent(inout)::A(LDA)
		character(len=*),intent(in)::B(LDB)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine


!********************************* complex(kind=4) ***********************************************

	subroutine store_value_com4_i(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=4),intent(inout)::A(LDA)
		integer,intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_com4_s(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=4),intent(inout)::A(LDA)
		real(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_com4_d(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=4),intent(inout)::A(LDA)
		real(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_com4_c(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=4),intent(inout)::A(LDA)
		complex(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_com4_z(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=4),intent(inout)::A(LDA)
		complex(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_com4_l(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=4),intent(inout)::A(LDA)
		logical,intent(in)::B(LDB)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine store_value_com4_a(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=4),intent(inout)::A(LDA)
		character(len=*),intent(in)::B(LDB)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine


!********************************* complex(kind=8) ***********************************************

	subroutine store_value_com8_i(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=8),intent(inout)::A(LDA)
		integer,intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_com8_s(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=8),intent(inout)::A(LDA)
		real(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_com8_d(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=8),intent(inout)::A(LDA)
		real(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_com8_c(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=8),intent(inout)::A(LDA)
		complex(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_com8_z(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=8),intent(inout)::A(LDA)
		complex(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_com8_l(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=8),intent(inout)::A(LDA)
		logical,intent(in)::B(LDB)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine store_value_com8_a(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		complex(kind=8),intent(inout)::A(LDA)
		character(len=*),intent(in)::B(LDB)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

!********************************* logical ***********************************************

	subroutine store_value_logi_i(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		logical,intent(inout)::A(LDA)
		integer,intent(in)::B(LDB)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value_logi_s(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		logical,intent(inout)::A(LDA)
		real(kind=4),intent(in)::B(LDB)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value_logi_d(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		logical,intent(inout)::A(LDA)
		real(kind=8),intent(in)::B(LDB)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value_logi_c(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		logical,intent(inout)::A(LDA)
		complex(kind=4),intent(in)::B(LDB)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value_logi_z(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		logical,intent(inout)::A(LDA)
		complex(kind=8),intent(in)::B(LDB)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value_logi_l(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		logical,intent(inout)::A(LDA)
		logical,intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_logi_a(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		logical,intent(inout)::A(LDA)
		character(len=*),intent(in)::B(LDB)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

!********************************* character ***********************************************
	
	subroutine store_value_char_i(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		character(len=*),intent(inout)::A(LDA)
		integer,intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_char_s(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		character(len=*),intent(inout)::A(LDA)
		real(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_char_d(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		character(len=*),intent(inout)::A(LDA)
		real(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_char_c(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		character(len=*),intent(inout)::A(LDA)
		complex(kind=4),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_char_z(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		character(len=*),intent(inout)::A(LDA)
		complex(kind=8),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_char_l(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		character(len=*),intent(inout)::A(LDA)
		logical,intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

	subroutine store_value_char_a(A,LDA,ia,B,LDB,ib)
		integer,intent(in)::LDA,ia(2),LDB,ib(2)
		character(len=*),intent(inout)::A(LDA)
		character(len=*),intent(in)::B(LDB)
		A(ia(1):ia(2))=B(ib(1):ib(2))
		return
	end subroutine

!***********************************************************************************************************
!
!        A(i1:i2,j1:j2)=B(i1:i2,j1:j2)
!
!***********************************************************************************************************

!**********************************   integer     ************************

	subroutine store_value2_int_i(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_int_s(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_int_d(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_int_c(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_int_z(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_int_l(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		logical,intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
	end subroutine

	subroutine store_value2_int_a(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		integer,intent(inout)::A(LDA,LDA2)
		character(len=*),intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
	end subroutine

!**********************************   real(kind=4)     ************************

	subroutine store_value2_real4_i(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_real4_s(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_real4_d(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_real4_c(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_real4_z(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_real4_l(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2)
		logical,intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine store_value2_real4_a(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2)
		character(len=*),intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

!**********************************   real(kind=8)     ************************

	subroutine store_value2_real8_i(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_real8_s(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_real8_d(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_real8_c(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_real8_z(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_real8_l(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2)
		logical,intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine store_value2_real8_a(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2)
		character(len=*),intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

!**********************************   complex(kind=4)     ************************

	subroutine store_value2_com4_i(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_com4_s(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_com4_d(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_com4_c(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_com4_z(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_com4_l(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2)
		logical,intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine store_value2_com4_a(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2)
		character(len=*),intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

!**********************************   complex(kind=8)     ************************

	subroutine store_value2_com8_i(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_com8_s(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_com8_d(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_com8_c(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_com8_z(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))=B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_com8_l(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2)
		logical,intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine store_value2_com8_a(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2)
		character(len=*),intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

!**********************************   logical     ************************

	subroutine store_value2_logi_i(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		logical,intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value2_logi_s(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		logical,intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value2_logi_d(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		logical,intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value2_logi_c(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		logical,intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value2_logi_z(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		logical,intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value2_logi_l(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		logical,intent(inout)::A(LDA,LDA2)
		logical,intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))= B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine

	subroutine store_value2_logi_a(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		logical,intent(inout)::A(LDA,LDA2)
		character(len=*),intent(in)::B(LDB,LDB2)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

!**********************************   character     ************************

	subroutine store_value2_char_i(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		character(len=*),intent(inout)::A(LDA,LDA2)
		integer,intent(in)::B(LDB,LDB2)
		integer::jj
		do jj=ja(1),ja(2)
			A(ia(1):ia(2),jj)= B(ib(1):ib(2),jj-ja(1)+jb(1))
		end do
		return
	end subroutine

	subroutine store_value2_char_s(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		character(len=*),intent(inout)::A(LDA,LDA2)
		real*4,intent(in)::B(LDB,LDB2)
		integer::jj
		do jj=ja(1),ja(2)
			A(ia(1):ia(2),jj)= B(ib(1):ib(2),jj-ja(1)+jb(1))
		end do
		return
	end subroutine

	subroutine store_value2_char_d(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		character(len=*),intent(inout)::A(LDA,LDA2)
		real*8,intent(in)::B(LDB,LDB2)
		integer::jj
		do jj=ja(1),ja(2)
			A(ia(1):ia(2),jj)= B(ib(1):ib(2),jj-ja(1)+jb(1))
		end do
		return
	end subroutine

	subroutine store_value2_char_c(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		character(len=*),intent(inout)::A(LDA,LDA2)
		complex*8,intent(in)::B(LDB,LDB2)
		integer::jj
		do jj=ja(1),ja(2)
			A(ia(1):ia(2),jj)= B(ib(1):ib(2),jj-ja(1)+jb(1))
		end do
		return
	end subroutine

	subroutine store_value2_char_z(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		character(len=*),intent(inout)::A(LDA,LDA2)
		complex*16,intent(in)::B(LDB,LDB2)
		integer::jj
		do jj=ja(1),ja(2)
			A(ia(1):ia(2),jj)= B(ib(1):ib(2),jj-ja(1)+jb(1))
		end do
		return
	end subroutine

	subroutine store_value2_char_l(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		character(len=*),intent(inout)::A(LDA,LDA2)
		logical,intent(in)::B(LDB,LDB2)
		integer::jj
		do jj=ja(1),ja(2)
			A(ia(1):ia(2),jj)= B(ib(1):ib(2),jj-ja(1)+jb(1))
		end do
		return
	end subroutine

	subroutine store_value2_char_a(A,LDA,LDA2,ia,ja,B,LDB,LDB2,ib,jb)
		integer,intent(in)::LDA,LDA2,ia(2),ja(2),LDB,LDB2,ib(2),jb(2)
		character(len=*),intent(inout)::A(LDA,LDA2)
		character(len=*),intent(in)::B(LDB,LDB2)
		A(ia(1):ia(2),ja(1):ja(2))= B(ib(1):ib(2),jb(1):jb(2))
		return
	end subroutine


!***********************************************************************************************************
!
!        A(i1:i2,j1:j2,k1:k2)=B(i1:i2,j1:j2,k1:k2)
!
!***********************************************************************************************************


!********************************   integer   **********************************************

	subroutine store_value3_int_i(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3)
		integer,intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_int_s(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_int_d(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_int_c(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_int_z(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_int_l(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3)
		logical,intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
	end subroutine

	subroutine store_value3_int_a(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
	end subroutine

!********************************   real(kind=4)   **********************************************

	subroutine store_value3_real4_i(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		integer,intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_real4_s(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_real4_d(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_real4_c(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_real4_z(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_real4_l(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		logical,intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine store_value3_real4_a(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

!********************************   real(kind=8)   **********************************************

	subroutine store_value3_real8_i(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		integer,intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_real8_s(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_real8_d(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_real8_c(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_real8_z(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_real8_l(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		logical,intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine store_value3_real8_a(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine

!********************************   complex(kind=4)   **********************************************

	subroutine store_value3_com4_i(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		integer,intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_com4_s(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_com4_d(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_com4_c(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_com4_z(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_com4_l(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		logical,intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine store_value3_com4_a(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine

!********************************   complex(kind=8)   **********************************************

	subroutine store_value3_com8_i(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		integer,intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_com8_s(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_com8_d(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_com8_c(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_com8_z(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_com8_l(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		logical,intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine store_value3_com8_a(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine	

!********************************   logical   **********************************************

	subroutine store_value3_logi_i(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3)
		integer,intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop		
	end subroutine

	subroutine store_value3_logi_s(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop		
	end subroutine

	subroutine store_value3_logi_d(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop		
	end subroutine

	subroutine store_value3_logi_c(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop		
	end subroutine

	subroutine store_value3_logi_z(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop		
	end subroutine

	subroutine store_value3_logi_l(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3)
		logical,intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))= B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine

	subroutine store_value3_logi_a(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3)
		call writemess('ERROR type when setting, logical=character ')
		call error_stop
	end subroutine

!********************************   character   **********************************************

	subroutine store_value3_char_i(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3)
		integer,intent(in)::B(LDB,LDB2,LDB3)
		integer::jj,kk
		do kk=ka(1),ka(2)
			do jj=ja(1),ja(2)
				A(ia(1):ia(2),jj,kk)=B(ib(1):ib(2),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
			end do
		end do
		return
	end subroutine

	subroutine store_value3_char_s(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		integer::jj,kk
		do kk=ka(1),ka(2)
			do jj=ja(1),ja(2)
				A(ia(1):ia(2),jj,kk)=B(ib(1):ib(2),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
			end do
		end do
		return
	end subroutine

	subroutine store_value3_char_d(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		integer::jj,kk
		do kk=ka(1),ka(2)
			do jj=ja(1),ja(2)
				A(ia(1):ia(2),jj,kk)=B(ib(1):ib(2),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
			end do
		end do
		return
	end subroutine

	subroutine store_value3_char_c(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3)
		integer::jj,kk
		do kk=ka(1),ka(2)
			do jj=ja(1),ja(2)
				A(ia(1):ia(2),jj,kk)=B(ib(1):ib(2),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
			end do
		end do
		return
	end subroutine

	subroutine store_value3_char_z(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3)
		integer::jj,kk
		do kk=ka(1),ka(2)
			do jj=ja(1),ja(2)
				A(ia(1):ia(2),jj,kk)=B(ib(1):ib(2),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
			end do
		end do
		return
	end subroutine

	subroutine store_value3_char_l(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3)
		logical,intent(in)::B(LDB,LDB2,LDB3)
		integer::jj,kk
		do kk=ka(1),ka(2)
			do jj=ja(1),ja(2)
				A(ia(1):ia(2),jj,kk)=B(ib(1):ib(2),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
			end do
		end do
		return
	end subroutine

	subroutine store_value3_char_a(A,LDA,LDA2,LDA3,ia,ja,ka,B,LDB,LDB2,LDB3,ib,jb,kb)
		integer,intent(in)::LDA,LDA2,LDA3,ia(2),ja(2),ka(2),LDB,LDB2,LDB3,ib(2),jb(2),kb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2))
		return
	end subroutine	

!***********************************************************************************************************
!
!        A(i1:i2,j1:j2,k1:k2,l1:l2)=B(i1:i2,j1:j2,k1:k2,l1:l2)
!
!***********************************************************************************************************

!********************************   integer   **********************************************

	subroutine store_value4_int_i(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		integer,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

	subroutine store_value4_int_s(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

	subroutine store_value4_int_d(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_int_c(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_int_z(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_int_l(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		logical,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, integer=logical ')
		call error_stop
	end subroutine

	subroutine store_value4_int_a(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		integer,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, integer=character ')
		call error_stop
	end subroutine

!********************************   real(kind=4)   **********************************************

	subroutine store_value4_real4_i(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		integer,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

	subroutine store_value4_real4_s(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

	subroutine store_value4_real4_d(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_real4_c(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_real4_z(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_real4_l(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		logical,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine store_value4_real4_a(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine


!********************************   real(kind=8)   **********************************************

	subroutine store_value4_real8_i(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		integer,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

	subroutine store_value4_real8_s(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

	subroutine store_value4_real8_d(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_real8_c(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_real8_z(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_real8_l(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		logical,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, real=logical ')
		call error_stop
	end subroutine

	subroutine store_value4_real8_a(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		real(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, real=character ')
		call error_stop
	end subroutine


!********************************   complex(kind=4)   **********************************************

	subroutine store_value4_com4_i(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		integer,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

	subroutine store_value4_com4_s(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

	subroutine store_value4_com4_d(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_com4_c(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_com4_z(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_com4_l(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		logical,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine store_value4_com4_a(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=4),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine


!********************************   complex(kind=8)   **********************************************

	subroutine store_value4_com8_i(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		integer,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

	subroutine store_value4_com8_s(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

	subroutine store_value4_com8_d(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_com8_c(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_com8_z(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine

	subroutine store_value4_com8_l(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		logical,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, complex=logical ')
		call error_stop
	end subroutine

	subroutine store_value4_com8_a(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		complex(kind=8),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, complex=character ')
		call error_stop
	end subroutine


!********************************   logical   **********************************************

	subroutine store_value4_logi_i(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		integer,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine	

	subroutine store_value4_logi_s(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine	

	subroutine store_value4_logi_d(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value4_logi_c(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value4_logi_z(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, logical=number ')
		call error_stop
	end subroutine

	subroutine store_value4_logi_l(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		logical,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))= B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		call error_stop
	end subroutine

	subroutine store_value4_logi_a(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		logical,intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		call writemess('ERROR type when setting, logical=character ')
		call error_stop
	end subroutine

!********************************   character   **********************************************

	subroutine store_value4_char_i(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		integer,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		integer::jj,kk,ll
		do ll=la(1),la(2)
			do kk=ka(1),ka(2)
				do jj=ja(1),ja(2)
					A(ia(1):ia(2),jj,kk,ll)=B(ib(1):ib(2),jj-ja(1)+jb(1),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
				end do
			end do
		end do
		return
	end subroutine	

	subroutine store_value4_char_s(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		integer::jj,kk,ll
		do ll=la(1),la(2)
			do kk=ka(1),ka(2)
				do jj=ja(1),ja(2)
					A(ia(1):ia(2),jj,kk,ll)=B(ib(1):ib(2),jj-ja(1)+jb(1),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
				end do
			end do
		end do
		return
	end subroutine	

	subroutine store_value4_char_d(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		real(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		integer::jj,kk,ll
		do ll=la(1),la(2)
			do kk=ka(1),ka(2)
				do jj=ja(1),ja(2)
					A(ia(1):ia(2),jj,kk,ll)=B(ib(1):ib(2),jj-ja(1)+jb(1),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
				end do
			end do
		end do
		return
	end subroutine

	subroutine store_value4_char_c(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=4),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		integer::jj,kk,ll
		do ll=la(1),la(2)
			do kk=ka(1),ka(2)
				do jj=ja(1),ja(2)
					A(ia(1):ia(2),jj,kk,ll)=B(ib(1):ib(2),jj-ja(1)+jb(1),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
				end do
			end do
		end do
		return
	end subroutine

	subroutine store_value4_char_z(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		complex(kind=8),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		integer::jj,kk,ll
		do ll=la(1),la(2)
			do kk=ka(1),ka(2)
				do jj=ja(1),ja(2)
					A(ia(1):ia(2),jj,kk,ll)=B(ib(1):ib(2),jj-ja(1)+jb(1),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
				end do
			end do
		end do
		return
	end subroutine

	subroutine store_value4_char_l(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		logical,intent(in)::B(LDB,LDB2,LDB3,LDB4)
		integer::jj,kk,ll
		do ll=la(1),la(2)
			do kk=ka(1),ka(2)
				do jj=ja(1),ja(2)
					A(ia(1):ia(2),jj,kk,ll)=B(ib(1):ib(2),jj-ja(1)+jb(1),jj-ja(1)+jb(1),kk-ka(1)+kb(1))
				end do
			end do
		end do
		return
	end subroutine

	subroutine store_value4_char_a(A,LDA,LDA2,LDA3,LDA4,ia,ja,ka,la,B,LDB,LDB2,LDB3,LDB4,ib,jb,kb,lb)
		integer,intent(in)::LDA,LDA2,LDA3,LDA4,ia(2),ja(2),ka(2),la(2),LDB,LDB2,LDB3,LDB4,ib(2),jb(2),kb(2),lb(2)
		character(len=*),intent(inout)::A(LDA,LDA2,LDA3,LDA4)
		character(len=*),intent(in)::B(LDB,LDB2,LDB3,LDB4)
		A(ia(1):ia(2),ja(1):ja(2),ka(1):ka(2),la(1):la(2))=B(ib(1):ib(2),jb(1):jb(2),kb(1):kb(2),lb(1):lb(2))
		return
	end subroutine	

end module