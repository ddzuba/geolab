!------------------------------------------------------------------------------------------
! ������ �������� ������ ������ � ����������� ��� ������ .BMP - ������
!------------------------------------------------------------------------------------------
	SUBROUTINE FILE_WRITE_CSV_SECOND() 
	USE PARAM_
	USE FReciverGlobals
	USE PARAM_1
	USE RAILWAY
	IMPLICIT NONE
    
    integer lret, make_folder
    
IF(IFL_WRITER_KASKAD.EQ.1) THEN		! ������ ��� ������� �����������
PAPKA=NAME_FILE_GEO
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
5	RETURN
!	FLAG_NAME_GEO=1
6	STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ��������; ����; ������� ������� ����; ������� ������� ����')
	WRITE(10,100) STRING_200
	GRAF_NUMBER(N_CHEN_TEK)=NAME_GRAF ! ���������� �����
	CLOSE(10)	! �������� ����� ������

!----------------------------------------------------------------------
! ���� VLAG
	NAME_VLAG=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(VLAG)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=20,FILE=NAME_VLAG,ERR=7,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
	GOTO 8
7	RETURN
8	STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ������ ��������; ������ ����; ��������� ��������; ��������� ����; �������������; ��������')
	WRITE(20,100) STRING_200
	VLAG_NUMBER(N_CHEN_TEK)=NAME_VLAG ! ���������� �����
	CLOSE(20)	! �������� ����� ������
!----------------------------------------------------------------------
! ���� UGLUB
	NAME_UGLUB=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(UGLUB)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=30,FILE=NAME_UGLUB,ERR=9,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
	GOTO 10
9	RETURN
10	STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ������ ��������; ������ ����; ��������� ��������; ��������� ����;�������������;��������������; ��������')
	WRITE(30,100) STRING_200
	UGLUB_NUMBER(N_CHEN_TEK)=NAME_UGLUB ! ���������� �����
	CLOSE(30)	! �������� ����� ������
! ���� TOLSH 
	NAME_TOLSH=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(TOLSH)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=31,FILE=NAME_TOLSH,ERR=13,STATUS='REPLACE',POSITION='APPEND')  ! �������� ����� ������
	GOTO 14  
13  RETURN
14  STRING_200=ADJUSTL('��� �����������; ����� ����; ����� ������; ������ ��������; ������ ����; ��������� ��������; ��������� ����; �������������; ��������')
	WRITE(31,100) STRING_200
	TOLSH_NUMBER(N_CHEN_TEK)=NAME_TOLSH ! ���������� �����
	CLOSE(31)	! �������� ����� ������    
!----------------------------------------------------------------------
! ���� INFO
	NAME_INFO=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(INFO)//'.xml')
    OPEN(UNIT=8,FILE=NAME_INFO,ERR=11,STATUS='REPLACE')  ! �������� ����� ������
	GOTO 12
11	RETURN
12	CALL INFO_XML()
	CLOSE(8)	! �������� ����� ������
END DO
!----------------------------------------------------------------------
100 FORMAT(A200)

END IF
RETURN
END SUBROUTINE FILE_WRITE_CSV_SECOND	
	
!open(unit=100, FILE='D:/My_fortran_proects_tvema/OUTPUT.OUT')
!write(100,*) 
