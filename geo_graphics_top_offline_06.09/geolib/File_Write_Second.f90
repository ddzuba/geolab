SUBROUTINE FILE_WRITE_SECOND() 
USE PARAM_
USE FReciverGlobals
USE PARAM_1
USE RAILWAY

IMPLICIT NONE

integer lret, make_folder
	
IF(IFL_WRITER.EQ.1) THEN		! ���� ������ ������ �� ���� (1 - ������ �����������)
PAPKA=NAME_FILE_IMG
!--------------------------------------------------------------------
	lret = make_folder(DISK,PAPKA) ! ������� �����
!--------------------------------------------------------------------
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/')

	I_LENTH=LEN_TRIM(NAME_FILE)		!  ����� �����
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(FILE))

	OPEN(UNIT=40,FILE=NAME_FILE ,FORM='BINARY',ERR=1,STATUS='REPLACE') ! �������� ����� ������
	GOTO 2
1	RETURN
!	FLAG_NAME_IMG=1
2	CLOSE(40,STATUS='DELETE')	! �������� ����� ������
! ���� ���� -----------------------------------------------------------------
	IFL_WRITER_TEXT=1		! ���� ������ ������ �� ���� (1 - ������ �����������)
!	FILE_TEXT=TRIM(FILE_TEXT)//'.DAT'
	NAME_FILE_TEXT=TRIM(TRIM(DISK)//&
     '/'//TRIM(PAPKA)//'/'//TRIM(FILE_TEXT))

	OPEN(UNIT=15,FILE=NAME_FILE_TEXT,ERR=3,STATUS='REPLACE') ! �������� ����� ������
	WRITE(15,100)

	GOTO 4
3	RETURN
4	CLOSE(15)	! �������� ����� ������
100	FORMAT(1X,'�����',4X,'��������',6x,'  ����  ' )

END IF
RETURN
END SUBROUTINE FILE_WRITE_SECOND	
	
