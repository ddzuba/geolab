! ---------------------------------------------------------------------------------------
! ����������� �������� ���������� ���������� ��� ������� 
! ---------------------------------------------------------------------------------------
SUBROUTINE INDATA(FILE_DLG,C_NAME,CBTYPE)			! �������� ������ �� ����� �������
USE PARAM_
USE PARAM_1
use FReciverGlobals
USE RAILWAY
USE N_										

TYPE(DIALOG)::FILE_DLG
INTEGER(4)::C_NAME,CBTYPE, I
I=CBTYPE												! ���������� �����������

FLAG=DLGGET(FILE_DLG,C_NAME,STRING)

SELECT CASE(C_NAME)
CASE(CHECK0)								! ��������� ������ ������������ �����
	IF(K_FLAG.EQ.0) THEN
	FLAG=DLGSET(FILE_DLG,A21,.TRUE.)
	FLAG=DLGSET(FILE_DLG,A17,.TRUE.)	
	FLAG=DLGSET(FILE_DLG,A18,.TRUE.)	
	FLAG=DLGSET(FILE_DLG,A19,.TRUE.)	
	K_FLAG=1								! �������������� ������������ �����
	ELSE
	FLAG=DLGSET(FILE_DLG,A21,.FALSE.)
	FLAG=DLGSET(FILE_DLG,A17,.FALSE.)	
	FLAG=DLGSET(FILE_DLG,A18,.FALSE.)	
	FLAG=DLGSET(FILE_DLG,A19,.FALSE.)	
	K_FLAG=0								! �������������� ������������ �����
	END IF
CASE(CHECK1)								! ��������� ������ ������ ��������
	IF(IFL_WRITER.EQ.1) THEN
	IFL_WRITER=0							! ���� ������ ������ �� ���� (0 - ������ ���)
	ELSE
	IFL_WRITER=1							! ���� ������ ������ �� ���� (0 - ������ ���)
	END IF
CASE(CHECK2)								! ��������� ������ ������ ��������� ������
	IF(IFL_WRITER_KASKAD.EQ.1) THEN
	IFL_WRITER_KASKAD=0						! ������ ��� ������� �� �����������
	ELSE
	IFL_WRITER_KASKAD=1						! ������ ��� �������  �����������
	END IF

CASE(CHECK3)
	IF(I_PRIZNAK.EQ.0) THEN
	I_PRIZNAK=1								! ������� ��������� ��� ���������� ����� ������ �������� ������
	ELSE
	I_PRIZNAK=0								! ������� ��������� ��� ���������� ����� ������ �������� ������
	END IF

END SELECT

IF(C_NAME.EQ.YES) THEN
	IF(K_FLAG.EQ.1) THEN		! ��������� ����� 
		FLAG=DLGGET(FILE_DLG,A21,STRING)	
		READ(STRING,*)N_DEVICE
		FLAG=DLGGET(FILE_DLG,A17,STRING)	
		READ(STRING,*)N_VAY
		FLAG=DLGGET(FILE_DLG,A18,STRING)	
		READ(STRING,*)N_DIRECTION
		FLAG=DLGGET(FILE_DLG,A19,STRING)	
		READ(STRING,*)N_TRACK
!		ELSE
!		N_DEVICE(1:50)=   '                                                  '
!		N_VAY(1:50)=      '                                                  '
!		N_DIRECTION(1:50)='                                                  '
!		N_TRACK(1:50)=    '                                                  '

!		��������� �������������� ����� �����
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		PAPKA=NAME_FILE_IMG
		WRITE(STRING,*)	PAPKA	
		FLAG=DLGSET(PARAMETR,A33,ADJUSTL(STRING))	! �������� � ���� 
		PAPKA1=NAME_FILE_GEO
		WRITE(STRING,*)	PAPKA1	
		FLAG=DLGSET(PARAMETR,A39,ADJUSTL(STRING))	! �������� � ���� 
		K_FLAG=3									! ��� �������� ������������ �����
		RETURN
	END IF
! ��������� ���������
	FLAG=DLGGET(FILE_DLG,A1,STRING)	
	READ(STRING,*)IANTENNA
	FLAG=DLGGET(FILE_DLG,A16,STRING)	
	READ(STRING,*)N_CHEN
	FLAG=DLGGET(FILE_DLG,A2,STRING)
	READ(STRING,*)J_LEVEL
	IF(J_LEVEL.LT.50.OR.J_LEVEL.GT.200)J_LEVEL=70	! ������� ��������� ������ ������
	FLAG=DLGGET(FILE_DLG,A3,STRING)
	READ(STRING,*)POROG
	FLAG=DLGGET(FILE_DLG,A4,STRING)
	READ(STRING,*)POROG1
	FLAG=DLGGET(FILE_DLG,A5,STRING)
	READ(STRING,*)POROG2
	FLAG=DLGGET(FILE_DLG,A20,STRING)
	READ(STRING,*)T_RAZV
	FLAG=DLGGET(FILE_DLG,A7,STRING)
	READ(STRING,*)M_REGION
	FLAG=DLGGET(FILE_DLG,A8,STRING)
	M2_REGION=M_REGION/2						! �������� ������ ���� ����������
	READ(STRING,*)L_REGION
	FLAG=DLGGET(FILE_DLG,A12,STRING)
	READ(STRING,*)HH_B
	FLAG=DLGGET(FILE_DLG,A11,STRING)
	READ(STRING,*)PREL
	FLAG=DLGGET(FILE_DLG,A13,STRING)
	READ(STRING,*)N_MIDL_U
!	FLAG=DLGGET(FILE_DLG,A24,STRING)				! ���������
!	READ(string,*)N_START						! ������� ������� ����� �� ������ ������������
!	FLAG=DLGGET(FILE_DLG,A25,STRING)
!	READ(string,*)N_FINISH						! ������ ������� ����� �� ������ ������������		
	FLAG=DLGGET(FILE_DLG,A26,STRING)
	READ(string,*)LM_WINDOW						! ����� ����� � ����	
	N_MIDL_RMOM=N_MIDL_U					! ����� ����� ��� ���������� ����������	
	N_MIDL_BALL=N_MIDL_U					! ����� ����� ��� ���������� ��������� ������� ��������
!	IF(LM_WINDOW.GT.L_REGION)LM_WINDOW=L_REGION
	FLAG=DLGGET(FILE_DLG,A27,STRING)
	READ(string,*) R_POSIB						! ����� ����� � ����			
	FLAG=DLGGET(FILE_DLG,A28,STRING)
!	READ(string,*) R_POWER						! ����� ����� � ����			
	FLAG=DLGGET(FILE_DLG,A29,STRING)
!	READ(string,*) R_SCIN						! ����� ����� � ����			
	FLAG=DLGGET(FILE_DLG,A30,STRING)
	READ(string,*) R_MOMENT						! ����� ����� � ����			
	FLAG=DLGGET(FILE_DLG,A31,STRING)
	READ(string,*) R_BALLAST					! ����� ����� � ����			
! ���������� ������� �������
R_POSIB1=1+R_POSIB							! �������������� ��������� ��� ����������� ���������
R_POSIB=1-R_POSIB
R_POWER=-ABS(R_POWER)
R_POWER1=-R_POWER							! �������������� ��������� ��� ����������� ���� �������
!R_SCIN1=1+R_SCIN							! �������������� ��������� ��� ����������� ����������
!R_SCIN=1-R_SCIN
R_MOMENT1=0.9+R_MOMENT						! �������������� ��������� ��� ����������� ���������������
R_MOMENT=0.9-R_MOMENT
R_BALLAST=1+R_BALLAST
	FLAG=DLGGET(FILE_DLG,A22,STRING)
	READ(STRING,*)N_BIT
	IF(N_BIT.EQ.2.OR.N_BIT.EQ.4) THEN
	GOTO 777
	ELSE
	N_BIT=0
	WRITE(STRING,*)N_BIT						! ����������� ����� � ������
	FLAG=DLGSET(FILE_DLG,A22,ADJUSTL(STRING))	! �������� � ����
	RETURN
	END IF
777	CALL POINT_ARRAY() ! ��������� ������� �������

!---------------------------------------------------------------------------
! �������� ����� ������� 
!---------------------------------------------------------------------------
IF(IFL_WRITER.EQ.0) GOTO 776
	FLAG=DLGGET(FILE_DLG,A32,STRING)
	READ(STRING,*)DISK
	FLAG=DLGGET(FILE_DLG,A33,STRING)
	READ(STRING,*)PAPKA
	FLAG=DLGGET(FILE_DLG,A34,STRING)
	READ(STRING,*)FILE_TEXT
	FLAG=DLGGET(FILE_DLG,A35,STRING)
	READ(STRING,*)IFLAG_PRINT
	FLAG=DLGGET(FILE_DLG,A36,STRING)
	READ(STRING,*)N_PRINT
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
	FLAG=DLGGET(FILE_DLG,A37,STRING)
	READ(STRING,*)img_w
	FLAG=DLGGET(FILE_DLG,A38,STRING)
	READ(STRING,*)img_h
	
!--------------------------------------------------------------------
	lret = make_folder(DISK,PAPKA) ! ������� �����
!--------------------------------------------------------------------
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/')

	I_LENTH=LEN_TRIM(NAME_FILE)		!  ����� �����
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(FILE))

	OPEN(UNIT=40,FILE=NAME_FILE ,FORM='BINARY',ERR=1,STATUS='REPLACE') ! �������� ����� ������
	GOTO 2
1	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'������ � ������� ����� ������')
!	CALL PAPKA_CREATE(PAPKA) 
	RETURN
!	FLAG_NAME_IMG=1
2	CLOSE(40,STATUS='DELETE')	! �������� ����� ������
! ���� ���� -----------------------------------------------------------------
	IFL_WRITER_TEXT=1		! ���� ������ ������ �� ���� (1 - ������ �����������)
	FILE_TEXT=TRIM(FILE_TEXT)//'.DAT'
	NAME_FILE_TEXT=TRIM(TRIM(DISK)//&
     '/'//TRIM(PAPKA)//'/'//TRIM(FILE_TEXT))

	OPEN(UNIT=15,FILE=NAME_FILE_TEXT,ERR=3,STATUS='REPLACE') ! �������� ����� ������
	WRITE(15,100)

	GOTO 4
3	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'������ � ������� ����� ������')
!	CALL PAPKA_CREATE(PAPKA_TEXT) 
	RETURN
4	CLOSE(15)	! �������� ����� ������
100	FORMAT(1X,'�����',4X,'��������',6x,'  ����  ' )
!---------------------------------------------------------------------------
! �������� ����� ��������� ������ 
!---------------------------------------------------------------------------
776 IF(IFL_WRITER_KASKAD.EQ.0)GOTO 775
	FLAG=DLGGET(FILE_DLG,A39,STRING)
	READ(STRING,*)PAPKA
	FLAG=DLGGET(FILE_DLG,A40,STRING)
	READ(STRING,*)NP_PRINT
!	FLAG=DLGGET(DLG,F5,STRING)
!	READ(STRING,*)FILE_DAT
	FLAG=DLGGET(FILE_DLG,A41,STRING)
	READ(STRING,*)GRAF
	FLAG=DLGGET(FILE_DLG,A42,STRING)
	READ(STRING,*)VLAG
	FLAG=DLGGET(FILE_DLG,A43,STRING)
	READ(STRING,*)UGLUB
	FLAG=DLGGET(FILE_DLG,A44,STRING)
	READ(STRING,*)INFO
!-----------------------------------------------------------------------
	lret = make_folder(DISK,PAPKA) ! ������� �����
!----------------------------------------------------------------------
DO N_CHEN_TEK=1,N_CHEN						! ����� ���������� ������
	WRITE(STRING_CHEN, '(i1)') N_CHEN_TEK	! ���������� ����� ������ � ������
!-----------------------------------------------------------------------
! ���� GRAF
	NAME_GRAF=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(GRAF)//'_'//TRIM(STRING_CHEN)//'.csv')
	OPEN(UNIT=10,FILE=NAME_GRAF,ERR=5,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
	GOTO 6
5	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'������ � ������� ����� �����')
	RETURN
!	FLAG_NAME_GEO=1
6	STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ��������; ����; ������� ������� ����; ������� ������� ����')
	WRITE(10,101) STRING_200
	GRAF_NUMBER(N_CHEN_TEK)=NAME_GRAF ! ���������� �����
	CLOSE(10)	! �������� ����� ������


!----------------------------------------------------------------------
! ���� VLAG
	NAME_VLAG=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(VLAG)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=20,FILE=NAME_VLAG,ERR=7,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
	GOTO 8
7	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'������ � ������� ����� ����������')
	RETURN
8	STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ������ ��������; ������ ����; ��������� ��������; ��������� ����; �������������; ��������')
	WRITE(20,101) STRING_200
	VLAG_NUMBER(N_CHEN_TEK)=NAME_VLAG ! ���������� �����
	CLOSE(20)	! �������� ����� ������
!----------------------------------------------------------------------
! ���� UGLUB
	NAME_UGLUB=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(UGLUB)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=30,FILE=NAME_UGLUB,ERR=9,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
	GOTO 10
9	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'������ � ������� ����� ���������� ���.')
	RETURN
10	STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ������ ��������; ������ ����; ��������� ��������; ��������� ����;�������������;��������������; ��������')
	WRITE(30,101) STRING_200
	UGLUB_NUMBER(N_CHEN_TEK)=NAME_UGLUB ! ���������� �����
	CLOSE(30)	! �������� ����� ������
! ���� TOLSH 
	NAME_TOLSH=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(TOLSH)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=31,FILE=NAME_TOLSH,ERR=13,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
	GOTO 14  
13  RETURN
14  STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ������ ��������; ������ ����; ��������� ��������; ��������� ����;�������������; ��������')
	WRITE(31,101) STRING_200
	TOLSH_NUMBER(N_CHEN_TEK)=NAME_TOLSH ! ���������� �����
	CLOSE(31)	! �������� ����� ������ 
!----------------------------------------------------------------------
! ���� INFO
	NAME_INFO=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(INFO)//'.xml')
    OPEN(UNIT=8,FILE=NAME_INFO,ERR=11,STATUS='REPLACE')  ! �������� ����� ������
	GOTO 12
11	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'������ � ������� ����� ����������')
	RETURN
12	CALL INFO_XML()
	CLOSE(8)	! �������� ����� ������
END DO
!--------------------------------------------------------------------------
! ��������� ���� �������� ������
IF(I_PRIZNAK.EQ.1) THEN
OPEN(UNIT=999, FILE=configFileName) ! ��������� ����
	WRITE(999,102)N_START,'��������� ������������ ����� ���� ���������'
	WRITE(999,102)N_FINISH,'��������  ������������ ����� ���� ��������� � ����������'
	WRITE(999,102)LM_WINDOW,'����� ����� � ���� ��������� � ����������'
	WRITE(999,103)(1-R_POSIB),'�������������� ��������� ��� ����������� ���������'
	WRITE(999,103)(-R_POWER),'�������������� ��������� ��� ����������� ���� �������'					
!	WRITE(999,103)(1-R_SCIN),'�������������� ��������� ��� ����������� ����������'
	WRITE(999,103)(0.9-R_MOMENT),'�������������� ��������� ��� ����������� ���������������'
	WRITE(999,103)(R_BALLAST-1),'�������������� ��������� ��� ����������� ���������� ����������'
	WRITE(999,102)N_BIT,'����� ���� ������ ��� �������� ����� ������'						
	WRITE(999,102)IANTENNA,'��� ��������� �����'
	WRITE(999,102)N_CHEN,'����� �������'
	WRITE(999,102)J_LEVEL,'������� ����������� ������������ � ������'
	WRITE(999,103)POROG,'��������� ���������� ������ ����'		
	WRITE(999,103)POROG1,'��������� ������ ���������'
	WRITE(999,103)POROG2,'��������� ���������� ������ ������'
	WRITE(999,103)T_RAZV,'���������'
	WRITE(999,102)M_REGION,'������ ���� ���������� � ������'
	WRITE(999,102)L_REGION,'������ ���� ���������� � �������'
	WRITE(999,103)HH_B,'������ ������� �������'
	WRITE(999,103)PREL,'���������� ����������� ��������'
	WRITE(999,102)N_MIDL_U,'����������� �������� ��� ���������� ���������� ����������'					! ����������� ����� � ������
!------------------------------------------------------------------------
	WRITE(999,104) TRIM(DISK),'����'
!	WRITE(999,*)	PAPKA	
!	WRITE(999,104)	FILE_TEXT	
	WRITE(999,102)	N_PRINT,'��� ������ ������������ �� �����'	
!	WRITE(999,102)	img_w,'������ �������'	
!	WRITE(999,102)	img_h,'������ �������'	
!--------------------------------------------------------------------
!	WRITE(999,*)	PAPKA1	
!	WRITE(999,*)	NP_PRINT,''	
	WRITE(999,104)	GRAF,'��� ����� ������ �����'	
	WRITE(999,104)	VLAG,'��� ����� ������ ���������'	
	WRITE(999,104)	UGLUB,'��� ����� ������ ���������� ����������'	
	WRITE(999,104)	INFO,'��� ��������������� �����'
!---------------------------------------------------------------------
	IFL_WRITER=1							! ���� ������ �� ���� �����������
	WRITE(999,102)	IFL_WRITER,'���� ������ �� ���� �����������'	
	NP_PRINT=1								! ����� ����� ��� ������ � ��������� ����
	WRITE(999,102)	NP_PRINT,'����� ����� ��� ������ � ��������� ����'	
	IFL_WRITER_KASKAD=1						! ���� ������ �� ���� �������
	WRITE(999,102)	IFL_WRITER_KASKAD,'���� ������ �� ���� �������'		
    IFL_WRITER_TEXT=1						!���� ������ ����� ����
	WRITE(999,102)	IFL_WRITER_TEXT,'���� ������ ����� ����'	
	I_PRIZNAK=0								! ������� ��������� ��� ���������� ����� ������ �������� ������
	WRITE(999,102)	I_PRIZNAK,'������� ��������� ��� ���������� ����� ������ �������� ������'	
	
END IF
!----------------------------------------------------------------------
101 FORMAT(A200)
102 FORMAT(I10,5X,A70)
103 FORMAT(F10.4,5X,A70)
104 FORMAT(A10,5X,A70)
!--------------------------------------------------------------------------
775 CALL DLGEXIT(FILE_DLG)						! ��������� ������
END IF



IF(C_NAME.EQ.NO) THEN
STOP				
END IF

END SUBROUTINE INDATA

