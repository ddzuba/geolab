! ---------------------------------------------------------------------------------------
! ����������� ���������� ��� ������� ���������� �������� ��������
! ---------------------------------------------------------------------------------------

SUBROUTINE PARAM_400()						 
USE PARAM_
USE N_	
use GeolibGlobals									
EXTERNAL INDATA2							! ���������, ��������� � ������ ��������
FLAG=DLGINITWITHRESOURCEHANDLE(GRAUND_400, dllHandler, PARAMETR2)				! ������������� �������
IF(.NOT.FLAG)STOP'GRAUND_400 NOT FOUND'

FLAG=DLGSETSUB(PARAMETR2,C1,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C2,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C3,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C4,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C5,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C6,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C7,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C8,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C9,INDATA2)

FLAG=DLGSETSUB(PARAMETR2,YES,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,NO,INDATA2)
	
IDELTA=SQRT(PREL)/T_RAZV/0.3		! ��������� ��� ���������� ����� � ���������� � 0.5 �����

N_START=j_level+idelta*1.5+1		! ������� ������� 
N_FINISH=j_level+idelta*3			! ������ ������� 
LM_WINDOW=50						! ����� ����� � ����
N_MIDL_U=50

R_POSIB=    0.3			! �������������� ��������� ��� ����������� ���������
R_POWER=    10.			! �������������� ��������� ��� ����������� ���� �������
R_SCIN =    0.4			! �������������� ��������� ��� ����������� ����������
R_MOMENT  = 0.7			! �������������� ��������� ��� ����������� ���������������
R_BALLAST	  = 0.1			! �������������� ��������� ��� ����������� ���������� ����������
									
WRITE(STRING,*)N_START						! ����������� ����� � ������
FLAG=DLGSET(PARAMETR2,C1,ADJUSTL(STRING))	! �������� � ����

WRITE(STRING,*)N_FINISH						! ����������� ����� � ������
FLAG=DLGSET(PARAMETR2,C2,ADJUSTL(STRING))	! �������� � ����

WRITE(STRING,*)LM_WINDOW					! ����������� ����� � ������
FLAG=DLGSET(PARAMETR2,C3,ADJUSTL(STRING))	! �������� � ����

WRITE(STRING,*)N_MIDL_U						! ����������� ����� � ������
FLAG=DLGSET(PARAMETR2,C4,ADJUSTL(STRING))	! �������� � ����

WRITE(STRING,*)R_POSIB						! ����������� ����� � ������
FLAG=DLGSET(PARAMETR2,C5,ADJUSTL(STRING))	! �������� � ����

WRITE(STRING,*)R_POWER						! ����������� ����� � ������
FLAG=DLGSET(PARAMETR2,C6,ADJUSTL(STRING))	! �������� � ����

WRITE(STRING,*)R_SCIN						! ����������� ����� � ������
FLAG=DLGSET(PARAMETR2,C7,ADJUSTL(STRING))	! �������� � ����

WRITE(STRING,*)R_MOMENT						! ����������� ����� � ������
FLAG=DLGSET(PARAMETR2,C8,ADJUSTL(STRING))	! �������� � ����

WRITE(STRING,*)R_BALLAST					! ����������� ����� � ������
FLAG=DLGSET(PARAMETR2,C9,ADJUSTL(STRING))	! �������� � ����

STATUS=DLGMODAL(PARAMETR2)					! ����������� �������

END SUBROUTINE PARAM_400
!---------------------------------------------------------------
!---------------------------------------------------------------
SUBROUTINE INDATA2(FILE_DLG,C_NAME,CBTYPE)	! �������� ������ �� ����� �������
USE PARAM_
TYPE(DIALOG)::FILE_DLG
INTEGER*4 ::C_NAME,CBTYPE, I
I=CBTYPE									! ���������� �����������
FLAG=DLGGET(FILE_DLG,C_NAME,STRING)
	
IF(C_NAME.EQ.YES) THEN
FLAG=DLGGET(FILE_DLG,C1,STRING)				! ���������
READ(string,*)N_START						! ������� ������� ����� �� ������ ������������
FLAG=DLGGET(FILE_DLG,C2,STRING)
READ(string,*)N_FINISH						! ������ ������� ����� �� ������ ������������		
FLAG=DLGGET(FILE_DLG,C3,STRING)
READ(string,*)LM_WINDOW						! ����� ����� � ����	
FLAG=DLGGET(FILE_DLG,C4,STRING)
READ(string,*) N_MIDL_U						! ����� ����� ��� ���������� ����������	
	N_MIDL_R0=N_MIDL_U						! ����� ����� ��� ���������� ����������	
	N_MIDL_RMOM=N_MIDL_U					! ����� ����� ��� ���������� ����������	
	N_TAN=N_MIDL_U							! ����� ����� ��� ���������� ����������	
	NTRS=N_MIDL_U							! ����� ����� ��� ����������
	N_MIDL_BALL=N_MIDL_U					! ����� ����� ��� ���������� ��������� ������� ��������
	IF(LM_WINDOW.GT.NTRS)LM_WINDOW=NTRS
FLAG=DLGGET(FILE_DLG,C5,STRING)
READ(string,*) R_POSIB						! ����� ����� � ����			
FLAG=DLGGET(FILE_DLG,C6,STRING)
READ(string,*) R_POWER						! ����� ����� � ����			
FLAG=DLGGET(FILE_DLG,C7,STRING)
READ(string,*) R_SCIN						! ����� ����� � ����			
FLAG=DLGGET(FILE_DLG,C8,STRING)
READ(string,*) R_MOMENT						! ����� ����� � ����			
FLAG=DLGGET(FILE_DLG,C9,STRING)
READ(string,*) R_BALLAST					! ����� ����� � ����			
CALL DLGEXIT(FILE_DLG)						! ��������� ������
! ���������� ������� �������
R_POSIB1=1+R_POSIB							! �������������� ��������� ��� ����������� ���������
R_POSIB=1-R_POSIB
R_POWER=-ABS(R_POWER)
R_POWER1=-R_POWER							! �������������� ��������� ��� ����������� ���� �������
R_SCIN1=1+R_SCIN							! �������������� ��������� ��� ����������� ����������
R_SCIN=1-R_SCIN
R_MOMENT1=0.9+R_MOMENT						! �������������� ��������� ��� ����������� ���������������
R_MOMENT=0.9-R_MOMENT
R_BALLAST=1+R_BALLAST
END IF

IF(C_NAME.EQ.NO) THEN
STOP				
END IF

END SUBROUTINE INDATA2 