!------------------------------------------------------------------------------------------
! ������ �������� ������ ������ � ����������� ��� ������ .BMP - ������
!------------------------------------------------------------------------------------------
	SUBROUTINE FILE_WRITE() 
	USE PARAM_
	USE FReciverGlobals
	USE PARAM_1
      use GeoLibGlobals
	IMPLICIT NONE
	EXTERNAL INFILES

	FLAG=DLGINITWITHRESOURCEHANDLE(FILES_BMP, dllHandler, PARAMETR2) 
	! ������������� �������
c ----------------------------------------------------------
	!DISK='C:'
	PAPKA=NAME_FILE_IMG
	FILE='1.bmp'
	IFLAG_PRINT=0				! ���������� ������	
	FLAG=DLGSET(PARAMETR2,ERROR_FILE,'')                ! ������������� ����
	WRITE(STRING,*)	DISK	
	FLAG=DLGSET(PARAMETR2,F1,adjustl(STRING))	! ������������� ����
	WRITE(STRING,*)	PAPKA	
	FLAG=DLGSET(PARAMETR2,F2,adjustl(STRING))	! ������������� ����
	WRITE(STRING,*)	FILE	
	FLAG=DLGSET(PARAMETR2,F3,adjustl(STRING))	! ������������� ����
	WRITE(STRING,*)	IFLAG_PRINT	
	FLAG=DLGSET(PARAMETR2,F6,adjustl(STRING))	! ������������� ����

	FLAG=DLGSETSUB(PARAMETR2,F1,INFILES)
	FLAG=DLGSETSUB(PARAMETR2,F2,INFILES)
	FLAG=DLGSETSUB(PARAMETR2,F3,INFILES)
	FLAG=DLGSETSUB(PARAMETR2,F6,INFILES)

	FLAG=DLGSETSUB(PARAMETR2,YES,INFILES)
	FLAG=DLGSETSUB(PARAMETR2,NO,INFILES)
	FLAG=DLGSETSUB(PARAMETR2,NO1,INFILES)

	STATUS=DLGMODAL(PARAMETR2)
	RETURN
      END SUBROUTINE  FILE_WRITE

	SUBROUTINE INFILES(DLG,C_NAME,CBTYPE)
	USE PARAM_
	USE PARAM_1
	USE FReciverGlobals

	TYPE(DIALOG)::DLG
	INTEGER*4  C_NAME,CBTYPE
	I=CBTYPE
	FLAG=DLGGET(DLG,C_NAME,STRING)
	SELECT CASE (C_NAME)
	CASE(NO)
	IFL_WRITER=0		! ���� ������ ������ �� ���� (0 - ������ ���)
	CALL DLGEXIT(DLG)	! ��������� ������
	CASE(NO1)
	STOP
	CASE(YES)
	IFL_WRITER=1		! ���� ������ ������ �� ���� (1 - ������ �����������)
	FLAG=DLGGET(DLG,F1,STRING)
	READ(STRING,*)DISK
	FLAG=DLGGET(DLG,F2,STRING)
	READ(STRING,*)PAPKA
	FLAG=DLGGET(DLG,F3,STRING)
	READ(STRING,*)FILE
	FLAG=DLGGET(DLG,F6,STRING)
	READ(STRING,*)IFLAG_PRINT

	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(FILE))

	OPEN(UNIT=40,FILE=NAME_FILE ,FORM='BINARY',ERR=1,STATUS='REPLACE') ! �������� ����� ������
	GOTO 2
1	FLAG=DLGSET(DLG,ERROR_FILE,'������ � ������� ����� ������')
	CALL PAPKA_CREATE(PAPKA) 
	RETURN
2	CLOSE(40)	! �������� ����� ������

	CALL DLGEXIT(DLG)
	END SELECT
	END SUBROUTINE INFILES	
	
