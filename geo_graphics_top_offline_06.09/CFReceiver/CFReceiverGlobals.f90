!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module CFReceiverGlobals
use ifwinty
use gl_core
use gl_graphics

implicit none

!  Parameters

integer*4, parameter, public :: SIZEOFAPPNAME = 100
!  Global data

integer(HANDLE)		ghInstance
integer(HANDLE)		ghModule
integer(HANDLE)		ghwndMain
integer(HANDLE)		ghMenu
integer road_number /157/ ! ������� ����� ������
integer flag_file/0/

type(T_MISC_GEO_DATA), target, allocatable :: mainGeoData
type(T_GRAPH_CONTEXT), target, allocatable :: mainGraphContext
type(T_MISC_GEO_DATA), pointer             :: pMainD
type(T_GRAPH_CONTEXT), pointer             :: pMainGC

end module

MODULE GLOBALS_LOG
INTEGER*4 FLAG_INI/0/                    ! ������� �������������� ini-�����
INTEGER*4 FLAG_Cfg/0/                    ! ������� �������������� cfg-�����
CHARACTER *100 PATHWAY_INI               ! ���� � ini-�����
CHARACTER *100 PATHWAY_CFG               ! ���� � cfg-�����
CHARACTER *100 OLD_PATHWAY_INI           ! ���� � ������������ ini-�����
CHARACTER *100 OLD_PATHWAY_CFG           ! ���� � ������������ cfg-�����
CHARACTER *100 NAME_FILE_LOG             ! ��� �������������� �����
CHARACTER *100 FILE_LOG_RED              ! ��� ������������������ �����
CHARACTER *100 NAME_FILE_TXT             ! ��� �������������� �����
CHARACTER *100 FILE_TXT_RED              ! ��� ������������������ �����
CHARACTER *100 FILE_TREK                 ! ��� ������������������ �����
CHARACTER *100 NAME_KIL_METR             ! ��� �����  � ������������ �������   
CHARACTER *100 STOLB_LENCH_FILE          ! ��� �����  ���� ������� 
CHARACTER  *100 TM                       ! ������������� ������
CHARACTER  *1 STR                        ! ������� ������
INTEGER*4 NN                ! ����� �������������� �����
INTEGER*4 NN1               ! ����� ������������������ �����   
INTEGER*4 NN2               ! ����� �����  � ������������ ������� 
INTEGER*4 NN3               ! ����� �����  ���� ������� 
INTEGER*4 MESS              ! ����������
INTEGER*4 IRES              ! ���������� �����
INTEGER*4 POLINOM
END MODULE GLOBALS_LOG
    
MODULE KOORDINATA
CHARACTER*50 FF1,FF2,FF3
CHARACTER *200 GPS_TRAK     ! ���� � ��������� ������
REAL*8, ALLOCATABLE :: VREMYA_TRASSA(:)			! ����� ����������� ������
REAL*8, ALLOCATABLE :: R_TRASSA(:)				! ���������� ����� ��������
REAL*8, ALLOCATABLE :: SHIROTA_TRASSA(:)		! ������ ������
REAL*8, ALLOCATABLE :: DOLGOTA_TRASSA(:)		! ������� ������
REAL*8, ALLOCATABLE ::  R_STOLB (:)             ! ���������� ����� ��������
REAL*8, ALLOCATABLE :: VREMYA(:)	! ������ ������� �� ���������� ����� ���������				
REAL*8, ALLOCATABLE :: SHIROTA(:)	! ������ ������  �� ���������� ����� ���������
REAL*8, ALLOCATABLE :: DOLGOTA(:)	! ������ ������� �� ���������� ����� ���������
INTEGER(4) IPOINT					! ����� ������� � ��������� ����� ������� ��������� � ���������� ����
REAL(8), ALLOCATABLE :: KLM(:)					! ������������ �����
REAL(8), ALLOCATABLE :: SHIR(:)					! ������ ������������� ������
REAL(8), ALLOCATABLE :: DLGT(:)					! ������� ������������� ������
INTEGER(4) IPOINT1					            ! ����� ������� � ����� �������� - ����������
REAL(8), ALLOCATABLE :: RAB(:)                  ! ������� ����
REAL(8) RABZEM/6372795.D0/  					! ������� ������ �����

!INTEGER(4) I_KM						    ! ����� ������������ ������������ ������� �� �������
!INTEGER(4) IPOINT2					! ����� ������� � ����� ������� �����,���������� ����� ������� ����

!INTEGER(4) LKM(1000)				! ������ � �������
!REAL(8) RKM(1000)					! ������� ����� � �������
!INTEGER(4) JPOINT					! ����� ������� � ������� � ��������


END MODULE KOORDINATA

