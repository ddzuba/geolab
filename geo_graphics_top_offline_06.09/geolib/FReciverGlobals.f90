!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module FReciverGlobals
use dflogm
implicit none

!  Parameters

integer*4, parameter, public :: SIZEOFAPPNAME = 100

!  Global data

integer		coord ! ��� ������ ����������
REAL*4		kilometr ! ��� ������ ����������
REAL*4		metr ! ��� ������ ������
REAL*4		KILOMETR_MOUSE ! ��� ������ ���������� �� ����
REAL*4		METR_MOUSE ! ��� ������ ������ �� ����
integer		kilometr_0 ! ��� ������ ���������� ������ ������
integer		metr_0	   ! ��� ������ ������ ������ ������
integer		kilometr_1 !,kilometr_1 ! ��� ����� ����� ��������
integer		metr_1	   !,metr_2		  ! ��� ����� ����� ��������
integer		metr_old_ ! ��� ������ ������

integer		ghInstance
integer		ghModule
integer		ghwndMain
integer		ghMenu
integer		nfnt	! ������ ������ �� ������
integer		nfnt1	! ������ ������

integer		isFirst /1/
integer		hMainBitmap /0/
integer		hMainDC /0/
integer		hBackBitmap /0/
integer		hBackDC /0/

REAL*4, ALLOCATABLE, target :: MyTrassa(:)	! ������, ����� ������� �� ����� �������
REAL*4, ALLOCATABLE, target :: trace_buf(:) ! ����� ������, ����� ���������

! ������, � �������� ������� �������� ������ �����
integer sum_line_top / 50 /
integer sum_line_bottom / 300 /

integer NSHPAL_OLD / -1 /

! ��� �������, ���. � ����. ��������, ���������������
! ������ � ������� ������.
real*4	MAX_VAL / 10000  /
real*4	MAX_VAL_OLD / 10000  /
real*4	MIN_VAL / -10000 /

integer NPT / 0 / ! ����� ����� � ������

integer		img_w /1000/ ! ������ �������
integer		img_h /600/ ! ������ �������
integer		img_hh  ! ������ �������

integer		speed_gr_zero /240/ ! ��������� ���� ������� ��������
real*4		speed_gr_scale /30.0/ ! ������� (���������) ������� �������� 
integer		speed_gr_color / 255 /  ! ���� ����� ������� ��������

integer		sum_gr_zero /598/ ! ��������� ���� ������� ���������
real*4		sum_gr_scale /0.01/ ! ������� (���������) ������� ��������� 
integer		sum_gr_color / ZFF0000 / !/ 16711680 /  ! ���� ����� ������� ���������

INTEGER*4 , ALLOCATABLE ::trace_draw_pos(:)	   ! ��������� ������
INTEGER*4 , ALLOCATABLE ::trace_draw_pos_1(:)  ! ������� ���� �������
INTEGER*4 , ALLOCATABLE ::trace_draw_pos_2(:)  ! ������������� �����������
INTEGER*4 , ALLOCATABLE ::trace_draw_pos_3(:)  ! ������������ ����
INTEGER*4 , ALLOCATABLE ::trace_draw_pos_4(:)  ! ������
INTEGER*4 , ALLOCATABLE ::trace_draw_pos_5(:)  ! ��������� �������

integer		TRACE_POS / 0 / ! ������� ������� ������ ��� ���������.

integer*4, target	:: booboo
integer*4, target	:: cursor_pos = -1 ! ��������� ������� (����� ���������� ������ ����)
integer*4, target	:: TRACE_COUNT = 0 ! ������� �������� �����

CHARACTER(100) configFileName / "geolib.cfg"C /
CHARACTER(500) fullConfigFilePath / ""C /

! �������
type(dialog) InfoDialog
integer CONSOLE_LIMIT / 200000 /
character(200000) :: REZTEXT
character(200000) :: CUR_CONSOLE
end module

MODULE PARAM_
USE DFLOGM				! ������������ ������ � ������ DVF
INCLUDE 'RESOURCE.FD'	! ������� ��������� ��������
LOGICAL(4) FLAG
!----------------------------------------------------------------------------------------------------
INTEGER*4  IANTENNA											! ��� ��������� �����
INTEGER*4  J_LEVEL											! ������� ����������� ������������ � ������ 
INTEGER*4  L_REGION,M_REGION,M2_REGION						! ��������� ������ ���� ����������
REAL*4 POROG_INTENS                                         ! ����� ���������� ������
REAL*4 POROG												! ��������� ���������� ������ ����
REAL*4 POROG1												! ��������� ������ ���������
REAL*4 POROG2												! ��������� ���������� ������ ������
INTEGER*4  LM_WINDOW,N_FINISH,N_START						! ������ ���� ��������� � ����������
REAL*4  RN_FINISH,RN_START						            ! ��������� ��� ���������� ������� ���� ��������� � ����������
INTEGER*4  NDEG1                                            ! ������� �������� ��� ������������ ������ �������
INTEGER*4  KEY_MIDL,KEY_MIDL1/0/                            ! ��������� ��������� �� ������ ������� ������
INTEGER*4  KEY_REFLECTION                                   ! ��������� ���������� ����� �������� ���������

!----------------------------------------------------------------------------------------------------
! ���������� �������� -------------------------------------------------------------------------------
TYPE(DIALOG) PARAMETR										!
TYPE(DIALOG) PARAMETR1										!
TYPE(DIALOG) PARAMETR2										!
TYPE(DIALOG) PARAMETR3										!
TYPE(DIALOG) PARAMETR4										!
INTEGER*4  STATUS											!
CHARACTER *200 STRING										!
!----------------------------------------------------------------------------------------------------
! ��������� ����������� ������� ---------------------------------------------------------------------
INTEGER*4  ITEK/0/								! ������� ����� �������������� ������

INTEGER(4), ALLOCATABLE:: IPR_CHEN(:)           ! ������� ���������� ������
INTEGER(4), ALLOCATABLE:: IPR(:)				! ���� ��� �������� �������
INTEGER(4), ALLOCATABLE:: IPR1(:)				! ���� ��� ������� ����������
INTEGER(4), ALLOCATABLE:: IPR1_UP(:)            ! ���� ��� ������� ���������� ����������

!REAL*4, ALLOCATABLE:: UNIFORMITY(:)							! UNIFORMITY_KR ! ������������
!REAL*4, ALLOCATABLE::UNIFORMITY_MIDL(:)	                    ! UNIFORMITY_KR ! ������������

INTEGER*4  N_MIDL_U							                ! ������ ��� ���������� ������������


REAL*4, ALLOCATABLE::REFR(:)				                ! ������������� �����������
REAL*4, ALLOCATABLE::REFR_MIDL(:)			                ! ������������� �����������
REAL*4, ALLOCATABLE::REFR_MAX(:)			                ! ������������ ������������� �����������
REAL*4, ALLOCATABLE:: REFRACTION(:,:)                       ! ������ ������������� ������������
INTEGER*4,ALLOCATABLE::STAR0(:)                             ! ������� ����� ��� ������ ���������  
INTEGER*4,ALLOCATABLE::FIN0(:)                              ! ������� ����� ��� ������ ���������  
INTEGER*4,ALLOCATABLE::STAR1(:)                             ! ������� ����� ��� ������ ���������  
INTEGER*4,ALLOCATABLE::FIN1(:)                              ! ������� ����� ��� ������ ���������  
INTEGER*4,ALLOCATABLE::STAR1_OLD(:)                         ! ������� ����� ��� ������ ���������  
INTEGER*4,ALLOCATABLE::KL_FORPRINT(:)                       ! ���� ��� ������ ��������� (0 � ����� ������� FORPRINT)
INTEGER*4,ALLOCATABLE::FLAG_FORPRINT(:)                     ! ���� ������ ������ ���������
INTEGER*4 DL_BLUE,DL_WIGHT                                  ! ����� ������� � ����� �������� � �������
REAL*4, ALLOCATABLE::RMOM(:)								! RMOM_KR ! ������
REAL*4, ALLOCATABLE::RMOM_MIDL(:)			                ! RMOM_KR ! ������
REAL*4, ALLOCATABLE::RMOM_MAX(:)			                ! ������������ ������������� �����������
			
REAL*4, ALLOCATABLE::R_BALL(:)			                    ! �������  ���										
REAL*4, ALLOCATABLE::R_BALL_UP(:)                           ! ������� ����������� ���										
REAL*4, ALLOCATABLE::R_BALL_MIDL(:)		                    ! �������  ���										
REAL*4, ALLOCATABLE::R_BALL_MIDL_UP(:)		                ! ������� ����������� ���										
INTEGER*4  N_MIDL_BALL						                ! ������ ��� ���������� ������� ����������� ���
! INTEGER*4  N_MIDL_BALL_UP                                 ! ������ ��� ���������� ������� ����������� ���
REAL*4,ALLOCATABLE :: BALL(:,:)
REAL*4,ALLOCATABLE :: BALL_UP(:,:)
REAL*4 RABTIME
!-------------------------------------------------------------------------------------------------
REAL*4, ALLOCATABLE :: SUM_RAB_MIDL(:,:)					! ������� ������ ��� ����������
REAL*4, ALLOCATABLE :: SUM_MIDDLE(:,:)  					! ������� ������ ��� ���������
REAL*4, ALLOCATABLE :: HH(:)					            ! ������ ������
REAL*4, ALLOCATABLE :: T_N(:)					            ! ������ ������
INTEGER*4, ALLOCATABLE :: IHH_50(:)					        ! ������ ������� ����� � ����� 50 ��
INTEGER*4 HH_RAB                                            ! ��� ��� ����������� ����� ������ � ����� �� HH_STEP
REAL*4 HH_STEP                                              ! ��� ��� ����������� ����� �� ����� ������ � ������
INTEGER*4 FLAG_HH                                           ! ���� ��� ����������� ����� ����� ������
INTEGER*4 FLAG_WINDOW                                       ! ���� ��� ����������� ����
INTEGER*4 I_HH                                              ! ����� ����� � ������� ������� ����� � ����� 50 �� 
REAL*4 T_RAZV												! ���������
REAL*4 VC0 /30/                                             ! �������� ����� � ��/��
INTEGER*4 JJ_H	    				                        ! ����� ������ ������� �� �����������		
REAL*4 HH_B	   											    ! ������ ������� ������� 
REAL*4 BAZ                                                  ! ���������� ����� ���������
REAL*4, ALLOCATABLE :: BUFF(:,:,:)							! ���������� ����� ��� ����������
REAL*4, ALLOCATABLE :: BUFF_MIDDLE(:,:,:)                   ! ���������� ����� ��� ��������� ��������
! �������� ���������� ----------------------------------------------------------------------------				    
CHARACTER(200) DISK,PAPKA,PAPKA1,FILE					    ! ���� � ����� ������ �������� 
CHARACTER(200),ALLOCATABLE :: FILE_NUMBER(:)                ! ������ ��� �������� ����
CHARACTER(200) DISK_TEXT,PAPKA_TEXT,FILE_TEXT			    ! ���� � ����� ������ ��������� ����
CHARACTER(200) NAME_FILE                				    ! ��� ������
CHARACTER(200) NAME_FILE_TEXT			        		    ! ��� ����� ����
INTEGER*4  I_LENTH
CHARACTER STR(200)										    ! ��� ������������ �����
!-------------------------------------------------------------------------------------------------
INTEGER*4  N_FILE_DISK      								! ����� ����� B �������
REAL*4 PREL											        ! ���������� ����������� ��������
INTEGER*4  J_DOWN										    ! ����� ������ ��� ��� �����
!-----------------------------------------------------------------------------------------------------------------------										    
! ���������� ���������
REAL*4 R_POSIB,R_POSIB1								    ! �������������� ��������� ��� ����������� �������� ����
REAL*4 R_MOMENT,R_MOMENT1								    ! �������������� ��������� ��� ����������� �������� ����
REAL*4 R_BALLAST										    ! �������������� ��������� ��� ����������� ���������� ����������
!-----------------------------------------------------------------------------------------------------------------------										    
! ��������� ��� ������ �� ������ ������� �������------------------------------------------------------------------------ 
INTEGER*4  NFONT											! ������ ���� ��� ������� ����������
INTEGER*4  JSDVIG											! ����� ������� ����� �������������� ���
INTEGER*4  JSDVIG_TR										! ����� ������� ����� �������������� ���
!----------------------------------------------------------------------------------------------------------------------
REAL*4 PARAM_MIDL1                                          ! �������� ��� ����������� ������� ������� ������ ��� �������
REAL*4 PARAM_MIDL2                                          ! �������� ��� ����������� ������ ������� ������ ��� �������
!----------------------------------------------------------------------------------------------------------------------
END MODULE PARAM_
!-----------------------------------------------------------------------------------------------------------------------
MODULE PARAM_1
! ���������� �������� -------------------------------------------------
INTEGER*4 	ISCH/0/		  ! ������� �������
INTEGER*4, target :: N_CHEN	= -1      ! ����� �������. -1 - ����������, ��� ���������� �� �����������.
INTEGER*4 	N_CHEN_F	      ! ����� ������� ������ � ������� ���������
INTEGER*4 	N_CHEN_TEK    ! ����� ���������� ������
REAL*4      COORDINATE,COORDINATE1          ! ��� ���������� �������� ���������
! ������������ ����� �����---------------------------------------------
! ����� ����������
INTEGER*4  N2_DEV,N2_DEV1
CHARACTER *200 N_DEVICE,N_DEVICE1			! ����� ����������
! ����� �����������
INTEGER*4  N2_VAY,N2_VAY1
CHARACTER *200 N_VAY,N_VAY1					! ����� ������
! ����� �����������
INTEGER*4  N2_DIR,N2_DIR1
CHARACTER *200 N_DIRECTION,N_DIRECTION1		! ��� �����������
! ����� ����
INTEGER*4  N2_TRA,N2_TRA1
CHARACTER *200 N_TRACK,N_TRACK1				! ����� ����
! ����� ����
CHARACTER *200 STRING_CHEN					! ����� ������
! ��� �����
CHARACTER *300 NAME_FILE_IMG,NAME_FILE_IMG_JPEG				! ����� ��� ���������MM
CHARACTER *300 NAME_FILE_GEO,NAME_FILE_GEO_ZIP				! ����� ��� �������
!INTEGER*4  FLAG_NAME_IMG/0/
!INTEGER*4  FLAG_NAME_GEO/0/
!-----------------------------------------------------------------------
! ��������� ��� ������� ���� ������ ������������ �� �����----------------------------
INTEGER*4, target :: N_PRINT		  ! ��� ������ ����� �� ��������� � �������
!INTEGER*4  LM_WINDOW_N_PRINT		  ! ������������ ��������
INTEGER*4  LM_WINDOW1,LM_WINDOW2      ! ������� ���������
INTEGER*4  K_PRINT/1/	  ! ������� �������� ���� ������ ������������ �� �����
! ��� ����������� N_PRINT � K_PRINT ����������� ������
INTEGER*4  NP_PRINT/1/		  ! ����� ����� ��� ������ � ��������� ����
INTEGER*4  KP_PRINT/1/	  ! ������� �������� ���� ������ � ��������� ����
! ��� ����������� NP_PRINT � KP_PRINT ����������� ������ � ��������� ����
!INTEGER*4  N_PRINT5       ! ������� ���������
!------------------------------------------------------------------------------------
INTEGER*4  IFLAG_PRINT				  ! ������� ������
!----------------------------------------------------------------------------------------------------------------------
INTEGER*4  kluch/0/											! ���� ��� ������ ������
!----------------------------------------------------------------------------------------------------------------------
INTEGER*4  K_FLAG/0/			! ��� ������������ ����� �������
INTEGER*4  FL_START/0/			! ��������� ������ ������
!----------------------------------------------------------------------------------------------------------------------
!��������� ������� ����� �� �������
!----------------------------------------------------------------------------------------------------------------------
REAL*4 rang,rang1				! ���������� ����
REAL*4 rang2					! ���������� ���� � ���������
REAL*4 rang3					! ���������� ���� � ������
integer*4 out_trassa			! ����� ����������� �����
!----------------------------------------------------------------------------------------------------------------------
INTEGER*4 , ALLOCATABLE :: N_WRITE_VLAG(:)	! ����� ������� � ���� VLAG
INTEGER*4 , ALLOCATABLE :: N_WRITE_UGLUB(:)	! ����� ������� � ���� UGLUB
INTEGER*4 , ALLOCATABLE :: N_WRITE_TOLSH(:)	! ����� ������� � ���� TOLSH
!----------------------------------------------------------------------------------------------------------------------
REAL*4 LENGTH_HUMIDITY                      ! ����������� ����� �������� ���������
REAL*4 STEP_HUMIDITY                        ! ���������� ����������� �������� ���������
!----------------------------------------------------------------------------------------------------------------------

END MODULE PARAM_1
!----------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------
MODULE N_									! ������������ ��� �������� ����������
INTEGER*4, target :: N                      ! ����� ������
INTEGER*4 NN_CHEN,NN_CHEN_F                 ! ����� ������, ���������� �� ����� �������

REAL*4, ALLOCATABLE :: RDM(:)				! ������� ������
REAL*4, ALLOCATABLE :: RDM1(:)			    ! ������� ������
REAL*4, ALLOCATABLE :: RDM2(:,:)            ! ������� ������
INTEGER*4  IDELTA,IDELTA2,IDELTA4			! ����� ����� ������ �� ����
REAL*4 R_LEV								! ������� ����
INTEGER*4  JJ1_B_0							! ������� ��������  ���� ������ ������� ����������� ���� 
INTEGER*4  JJ2_B_0							! ������ ��������  ���� ������ ������� ����������� ���� 
INTEGER*4  JJ1_PB_0							! ������� ��������  ���� ������ ������� �������������� ���� 
INTEGER*4  JJ2_PB_0							! ������ ��������  ���� ������ ������� �������������� ���� 
INTEGER*4  JJ2_GR			                ! ������ �������� ��������� �������
INTEGER*4  I_B								! ������� ��������
INTEGER*4  I_BSEK,I_PBSEK                   ! ������� ��������
INTEGER*4  I_PB								! ������� �������������� ���������
INTEGER*4 , ALLOCATABLE:: YY(:,:)			! �������� ������� �������� ��� ������������
INTEGER*4 , ALLOCATABLE:: YY1(:,:)			! ��������  ������������� ������� ��� �����������
INTEGER*4 , ALLOCATABLE:: IA(:)				! ������� ���������� �������� ������
REAL*4 RMULTIPLE                            ! �������� �������� � ������������� ����
REAL*4 R_I_B                                ! ��������� ������ �������� ������� ����� ��������
REAL*4 R_I_B1                               ! ��������� ������ �������� ������ ����� ��������
REAL*4 R_I_PB                               ! ��������� ������ �������� ����� �������������� ���������

END MODULE N_
!----------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------
MODULE DOSTUP_									! ������������ ��� ����������� ����� �������
USE DFLOGM				! ������������ ������ � ������ DVF
INCLUDE 'RESOURCE.FD'	! ������� ��������� ��������
INTEGER*4 , DIMENSION(1:8) :: SEED				! ������� ��� ���������
REAL(8), DIMENSION(1:10000) :: ARRAY			! ��������� �����
INTEGER(2), DIMENSION(1:10000) :: KEY_DOPUSK	! ������ ��� �������� 
INTEGER*4  I,J
INTEGER*4  K									! ����� ������
INTEGER*4  KL									! ������� ���������

CHARACTER *200 TEXT								! ���������� ��� 
TYPE(DIALOG) PARAMETR_INFO						! ������������
INTEGER*4  STATUS								! ���������

END MODULE DOSTUP_
!----------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------
MODULE RAILWAY								! ������������ ��� ������
CHARACTER(250) GRAF,NAME_GRAF			    ! ���� ������ � ���� ������
CHARACTER(250) VLAG,NAME_VLAG				! ���� ��������� � ���� ������
CHARACTER(250) UGLUB,NAME_UGLUB				! ���� ���������� ���������� � ���� ������
CHARACTER(250) TOLSH,NAME_TOLSH				! ���� ���������� ���������� � ���� ������
CHARACTER(250) INFO,NAME_INFO				! ���� ���������
CHARACTER(250), ALLOCATABLE ::  GRAF_NUMBER(:)	!������ ��� �������� ����
CHARACTER(250), ALLOCATABLE ::  VLAG_NUMBER(:)	!������ ��� �������� ����
CHARACTER(250), ALLOCATABLE ::  UGLUB_NUMBER(:)	!������ ��� �������� ����
CHARACTER(250), ALLOCATABLE ::  TOLSH_NUMBER(:)	!������ ��� �������� ����
! CHARACTER(1) STRKA
CHARACTER(200) S1,S2,S3,S4,S5,S6				! ������� ������ ��� ������ � ���� .CSV
CHARACTER(200) S1_G,S2_G,S3_G,S4_G,S5_G		! ������� ������ ��� ������ � ���� .CSV
CHARACTER(200), ALLOCATABLE :: S1_V(:),S2_V(:),S3_V(:),S4_V(:),S5_V(:),S6_V(:)		! ������� ������ ��� ������ � ���� .CSV
CHARACTER(200), ALLOCATABLE :: S1_U(:),S2_U(:),S3_U(:),S4_U(:),S5_U(:)		! ������� ������ ��� ������ � ���� .CSV
CHARACTER(200), ALLOCATABLE :: S1_U_UP(:),S2_U_UP(:),S3_U_UP(:),S4_U_UP(:),S5_U_UP(:)		! ������� ������ ��� ������ � ���� .CSV
CHARACTER(200) STRING_200					! ������� ������
INTEGER*4 , ALLOCATABLE::ii_bal(:)				! ����� ������ �������� � ���������
INTEGER*4 , ALLOCATABLE::ii_osn(:)				! ����� ������ �������� � ���������
CHARACTER (8) DDATA
CHARACTER (10) DDATA1
CHARACTER (4) YER				! ���
CHARACTER (2) MAUNS				! �����
CHARACTER (2) DAY				! ����
CHARACTER (2) HAUR				! ���
CHARACTER (2) MINIT				! ������
CHARACTER (2) SEK			    ! �������
       integer(2)  wYear 
       integer(2)  wMonth 
       integer(2)  wDayOfWeek 
       integer(2)  wDay 
       integer(2)  wHour 
       integer(2)  wMinute 
       integer(2)  wSecond 
       integer(2)  wMilliseconds 
REAL(8) , ALLOCATABLE::VREMYA_MOUSE(:)              ! ����� ��� ����� �� ������
REAL(8) SEKK0 /0/                                   ! ��������� �����
REAL(8) SEKK  /0/                                   ! ������� �����
REAL(4) TIME_VIDEO_FILE                             ! ����� ����������� ����� �����
CHARACTER*100 VIDEO_NAME/'ZERO'/                    ! ��� ����� � ����������������
CHARACTER*100 FOTO_NAME/'ZERO'/                     ! ��� ����� � ����������������
CHARACTER(200), ALLOCATABLE :: STRING_GRAF_CSV(:)	! ���������� ��� ���������� ������ 
INTEGER*4  n_GRAF/0/								! ����������� ������
INTEGER*4  i_GRAF/0/								! ������� ���������� ���������� ������ ������

CHARACTER(200), ALLOCATABLE :: STRING_VLAG_CSV(:,:)	! ���������� ��� ���������� �������� ���������
INTEGER*4  n_VLAG/0/								! ����������� ������

CHARACTER(200), ALLOCATABLE :: STRING_UGLUB_CSV(:,:)	! ���������� ��� ���������� �������� ��������
INTEGER*4  n_UGLUB/0/								    ! ����������� ������

CHARACTER(200), ALLOCATABLE :: STRING_TOLSH_CSV(:,:)	! ���������� ��� ���������� �������� ��������
INTEGER*4  n_TOLSH/0/								    ! ����������� ������
!----------------------------------------------------------------------------------------------------------------------
INTEGER*4  N_BIT											    ! ����� ��� �������� ������
!----------------------------------------------------------------------------------------------------------------------
INTEGER*4  IFL_WRITER/1/										! ���� ������ �� ���� �����������
INTEGER*4  IFL_WRITER_TEXT/1/									! ���� ������ ��������� ���� �� ������
INTEGER*4  IFL_WRITER_KASKAD/1/								    ! ���� ������ �� ���� �������

END MODULE RAILWAY

MODULE GRAFICA
INTEGER*4  N_PRIZNAK_GRAFICA/0/		! ������� ������ �� �����
INTEGER*4  M_OUT/4/					! ����� ������������ ���������� ����������
CHARACTER(200) TEXT_2/" ���������"/
CHARACTER(200) TEXT_4/" ���������������"/
INTEGER*4 ISTEP_FOR_GRAFICA
REAL*4 ISTEP_BETWEEN_TRASES
END MODULE GRAFICA
    
MODULE VLAG_CSV
REAL(4),ALLOCATABLE ::KILOMETR_FIRST(:)
REAL(4),ALLOCATABLE ::METR_FIRST(:)
REAL(4) IDL
REAL(4) KILOMETR_SEKOND,METR_SEKOND,IDL_SEKOND	! �������� � ���� ����� ���������� ������ � �� �����
REAL(4)	KILOMETR_OLD		
REAL(4)	METR_OLD		
REAL(4)	KILOMETR_FIRST_OLD		
REAL(4)	METR_FIRST_OLD
REAL(4)	IDL_OLD
END MODULE VLAG_CSV
    
MODULE UGLUB_CSV
REAL(4),ALLOCATABLE ::KILOMETR_FIRST_U(:)
REAL(4),ALLOCATABLE ::METR_FIRST_U(:)
REAL(4) IDL
REAL(4) KILOMETR_SEKOND,METR_SEKOND,IDL_SEKOND	! �������� � ���� ����� ���������� ������ � �� �����
REAL(4)	KILOMETR_OLD		
REAL(4)	METR_OLD		
REAL(4)	KILOMETR_FIRST_OLD		
REAL(4)	METR_FIRST_OLD
REAL(4)	IDL_OLD
END MODULE UGLUB_CSV

MODULE TOLSH_CSV
REAL(4),ALLOCATABLE ::KILOMETR_FIRST_T(:)
REAL(4),ALLOCATABLE ::METR_FIRST_T(:)
REAL(4) IDL
REAL(4) KILOMETR_SEKOND,METR_SEKOND,IDL_SEKOND	! �������� � ���� ����� ���������� ������ � �� �����
REAL(4)	KILOMETR_OLD		
REAL(4)	METR_OLD		
REAL(4)	KILOMETR_FIRST_OLD		
REAL(4)	METR_FIRST_OLD
REAL(4)	IDL_OLD
END MODULE TOLSH_CSV
