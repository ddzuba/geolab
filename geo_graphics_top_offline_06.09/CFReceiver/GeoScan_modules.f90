!****************************************************************************
!  Global data, parameters, and structures from GeoScan32
!****************************************************************************
MODULE GEOSCANINPUT
USE DFLOGM				! ������������ ������ � ������ DVF
INCLUDE 'RESOURCE.FD'	! ������� ��������� ��������
TYPE(DIALOG) D_2		! ����� ���������� �������
LOGICAL(4) FLAG
INTEGER(4) STATUS
CHARACTER*50 STRING
END MODULE GEOSCANINPUT 
    
MODULE GEOSKANGLOBALS
CHARACTER*100 NAME_FILE_GPR
!INTEGER*4 NN
!INTEGER*4 IRESULTAT     ! �������� ������ ��� ���������� ��������� GEOSKANGLOBALS=0
CHARACTER *50 N_DEVICE
CHARACTER *50 N_VAY
CHARACTER *50 N_DIRECTION
CHARACTER *50 N_TRACK
CHARACTER, ALLOCATABLE :: table(:)
REAL *4 Ster_Lenth  ! ����� ������� � ��� ����� ��������
REAL *4 First_Point ! ���������� ������ ������
REAL *4 TIME_GPS
END MODULE GEOSKANGLOBALS     
    
MODULE INPUT_RADAROGRAMMA
INTEGER*4:: NVERSYION						!����� ������
INTEGER*4:: IZAG(128)						!��������� ����� - 512 ����
INTEGER*4, TARGET:: NTR=1								!����� �����
INTEGER*4, TARGET:: N=512								!����� �����
INTEGER*4:: NLAB							!����� �����
INTEGER*4:: NSCSOTS							!����� ������� ����������
INTEGER*4:: NSCSOTSSTORAGESIZE				!����� �������� ����������
INTEGER*4:: ICOLOR(256)						!������   ������ - 1024 ����
REAL*4,	   ALLOCATABLE :: RL(:)				!������ ������������� ������������ - 4*N ����
INTEGER*4, ALLOCATABLE :: IZAGTR(:,:)		!��������� ������
INTEGER*4, ALLOCATABLE :: NCHANAL(:)		!����� ������
REAL*4,    ALLOCATABLE :: RSUM(:,:)			!������ - ������������ 	
INTEGER*4, ALLOCATABLE :: ISCCRIN(:)		!������� ����������
INTEGER*4, ALLOCATABLE :: ISCCRINSTOR(:)	!������ ����������
INTEGER*4, ALLOCATABLE :: IMET(:)			!������ �����
INTEGER*4 NCHANAL_GEO                       !����� ������� � ������������
INTEGER*4, ALLOCATABLE :: FLAG_CHANAL(:)			!������ �����
END MODULE INPUT_RADAROGRAMMA
 

