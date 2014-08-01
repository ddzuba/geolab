! ---------------------------------------------------------------------------------------
! ���� �������� ���������� �� ����� 
! ������������ ������ �� ���� �������� � ��������� ������
! ---------------------------------------------------------------------------------------

SUBROUTINE FIRST_READ()							! ����������� ������� ���������
use FReciverGlobals
USE PARAM_
USE PARAM_1
USE RAILWAY
USE N_	
USE GRAFICA
implicit none
integer :: lret, make_folder, FLAG_NAME_GEO
INTEGER K1,K2,I
character*500 TEXT_CFG

    call ReadConfigFromIni()
 
    IF(N_PRINT.LT.1)N_PRINT=1
		IF(N_PRINT.GT.50) N_PRINT=50
		IF(N_PRINT.LE.10)THEN
		NFONT=16;JSDVIG=25
		END IF
		IF(N_PRINT.LE.20.AND.N_PRINT.GT.10)THEN
		NFONT=14;JSDVIG=20
		END IF
		IF(N_PRINT.LE.30.AND.N_PRINT.GT.20)THEN
		NFONT=12;JSDVIG=15
		END IF
		IF(N_PRINT.LE.40.AND.N_PRINT.GT.30)THEN
		NFONT=10;JSDVIG=12
		END IF
		IF(N_PRINT.LE.50.AND.N_PRINT.GT.40)THEN
		NFONT=8;JSDVIG=10
		END IF

!---------------------------------------------------------------------
! ������������� ��������� ����������� ���������� �������
M2_REGION=M_REGION/2                    ! �������� ������ ���� ����������
IDELTA=2.0*SQRT(PREL)/T_RAZV/0.3		! ��������� ��� ���������� ����� � ���������� � 1.0 ����
idelta2=idelta/2						! ��������� ��� ���������� ����� � ���������� � 0.5 �����	
IDELTA4=IDELTA/4                        ! ��������� ��� ���������� ����� � ���������� � 0.25 �����

N_START=j_level+idelta4*RN_START+1		! ������� ������� 
N_FINISH=j_level+idelta4*RN_FINISH			! ������ ������� 
!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*) N
	N_MIDL_BALL=N_MIDL_U					! ����� ����� ��� ���������� ��������� ������� ��������
! ���������� ������� �������
R_POSIB1=1+R_POSIB							! �������������� ��������� ��� ����������� ���������
R_POSIB=1-R_POSIB
R_MOMENT1=0.9+R_MOMENT						! �������������� ��������� ��� ����������� ���������������
R_MOMENT=0.9-R_MOMENT
R_BALLAST=1+R_BALLAST
!---------------------------------------------------------------------------------
CALL POINT_ARRAY() 
!---------------------------------------------------------------------------------
! ��������� �����
! ��� ������ �������� -----------------------------------------------------------
CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
PAPKA=NAME_FILE_IMG
FILE='km_m_km_m_chen.bmp'
FILE_TEXT='MOUSE.DAT'
IF(IFL_WRITER.EQ.1) THEN
!--------------------------------------------------------------------
lret = make_folder(DISK,PAPKA) ! ������� �����
!--------------------------------------------------------------------
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/')
	I_LENTH=LEN_TRIM(NAME_FILE)		!  ����� �����
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(FILE))
	OPEN(UNIT=40,FILE=NAME_FILE ,FORM='BINARY',STATUS='REPLACE') ! �������� ����� ������
	CLOSE(40,STATUS='DELETE')	! �������� ����� ������
! ���� ���� -----------------------------------------------------------------
	FILE_TEXT=TRIM(FILE_TEXT) !//'.DAT'
	NAME_FILE_TEXT=TRIM(TRIM(DISK)//&
     '/'//TRIM(PAPKA)//'/'//TRIM(FILE_TEXT))
	OPEN(UNIT=15,FILE=NAME_FILE_TEXT,STATUS='REPLACE') ! �������� ����� ������
	WRITE(15,105)
	CLOSE(15)	! �������� ����� ������
!--------------------------------------------------------------------
    END IF

! ��� ��������� ������ ---------------------------------------------------------------
CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
PAPKA=NAME_FILE_GEO

!--------------------------------------------------------------------
lret = make_folder(DISK,PAPKA) ! ������� �����
!--------------------------------------------------------------------
DO N_CHEN_TEK=1,N_CHEN						! ����� ���������� ������
	WRITE(STRING_CHEN, '(i1)') N_CHEN_TEK	! ���������� ����� ������ � ������
! ���� GRAF
	NAME_GRAF=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(GRAF)//'_'//TRIM(STRING_CHEN)//'.csv')
	OPEN(UNIT=10,FILE=NAME_GRAF,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
	FLAG_NAME_GEO=1
	STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ��������; ����; ������� ������� ����; ������� ������� ����')
	WRITE(10,106) STRING_200
	GRAF_NUMBER(N_CHEN_TEK)=NAME_GRAF ! ���������� �����
	CLOSE(10)	! �������� ����� ������
!----------------------------------------------------------------------
! ���� VLAG
	NAME_VLAG=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(VLAG)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=20,FILE=NAME_VLAG,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
	STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ������ ��������; ������ ����; ��������� ��������; ��������� ����; �������������; ��������')
	WRITE(20,106) STRING_200
	VLAG_NUMBER(N_CHEN_TEK)=NAME_VLAG ! ���������� �����
	CLOSE(20)	! �������� ����� ������
!----------------------------------------------------------------------
! ���� UGLUB
	NAME_UGLUB=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(UGLUB)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=30,FILE=NAME_UGLUB,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
	STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ������ ��������; ������ ����; ��������� ��������; ��������� ����;�������������;��������������; ��������')
	WRITE(30,106) STRING_200
	UGLUB_NUMBER(N_CHEN_TEK)=NAME_UGLUB ! ���������� �����
	CLOSE(30)	! �������� ����� ������
! ���� TOLSH 
	NAME_TOLSH=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(TOLSH)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=31,FILE=NAME_TOLSH,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
    STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ������ ��������; ������ ����; ��������� ��������; ��������� ����;�������������; ��������')
	WRITE(31,106) STRING_200
	TOLSH_NUMBER(N_CHEN_TEK)=NAME_TOLSH ! ���������� �����
	CLOSE(31)	! �������� ����� ������  
!----------------------------------------------------------------------
! ���� INFO
	NAME_INFO=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(INFO)//'.xml')
    OPEN(UNIT=8,FILE=NAME_INFO,STATUS='REPLACE')  ! �������� ����� ������
	CALL INFO_XML()
	CLOSE(8)	! �������� ����� ������
END DO
!--------------------------------------------------------------------------	
105	FORMAT(1X,'�����',4X,'����� ������',4X,'��������',6x,'  ����  ' )
106 FORMAT(A200)

RETURN							
END SUBROUTINE
!
!SUBROUTINE DATA_READ(K1,K2,STRING)							! ���� ������ �� ����������������� ����� 
!IMPLICIT NONE
!INTEGER*4 J
!INTEGER*4 K1,K2
!INTEGER*4 L/500/ !����� ������
!CHARACTER *500 STRING
!
!READ(999,*,END=1) STRING
!   DO J=1,L                                            ! ���������� ����� ����� ��� ����� �����
!   IF(STRING(J:J).EQ.'=')K1=J+1
!   IF(STRING(J:J).EQ.';')K2=J-1
!   END DO
!   RETURN
!1 STOP
!END 

!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*) N