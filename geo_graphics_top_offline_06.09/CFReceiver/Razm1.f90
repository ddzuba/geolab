    SUBROUTINE RAZM1() ! ��������� ������� ��������� �� ��������
    USE KOORDINATA
    USE DFWINA
    USE GLOBALS_LOG
    USE INPUT_RADAROGRAMMA

    IMPLICIT NONE
    INTEGER (4) JJ,J,ISTAT
! ��������� ����������������� ����
    NN1=201
     ! ������ ���� 
    FF1='Cfreceiver.cfg'
    FF2='FILE_TREK'
    FF3='_'
    CALL Ini_Read_Str(FF1,FF2,FF3)  
    FILE_TREK=FF2 
    IF(FF2.NE.'_')OPEN(UNIT=NN1,FILE=FILE_TREK,ERR=4,STATUS='OLD')
    GOTO 1
4   MESS=MESSAGEBOX(NULL,"�������� ������ ������ - ��"C,"���� � ������� ��������� �����������"C,0)    ! ���� �� ��������� ����, ��
    IF(MESS.EQ.1) THEN                                                                                ! ����������� ������
    STOP                                                                                              ! ��� ��������� �������
    END IF
! ���������� ����� ������� � ����� ������ � GPS ����������
1	IPOINT=0							! ��������� ����� ������� � �����
	REWIND(NN1)							! ������������� ������ �� ������ ����� 
2	READ(NN1,101,IOSTAT=ISTAT)TM			! ��������� ��������� ������
	IF(ISTAT.EQ.-1) GOTO 3

	JJ=0								! ��������� ������ ������ 
	DO J=1,74
	IF(TM(J:J).NE.' ')JJ=1
	END DO
	IF(JJ.EQ.0)GOTO 2

	IPOINT=IPOINT+1
	GOTO 2
3	ALLOCATE (VREMYA(IPOINT))			! ��������� ������� ��� ���������� �� GPS ��������� 
	ALLOCATE (SHIROTA(IPOINT))
	ALLOCATE (DOLGOTA(IPOINT))
!	WRITE(30,102) NAMEDATA_LOG,IPOINT
101   FORMAT(A74)
!---------------------------------------------------------------------------------------------------
! ��������� ������� ��� ���������� �� ����� ���������
      ALLOCATE (VREMYA_TRASSA(NTR))			! ����� ����������� ������
      ALLOCATE (R_TRASSA(NTR))				! ���������� ����� ��������
      ALLOCATE (SHIROTA_TRASSA(NTR))			! ������ ������
      ALLOCATE (DOLGOTA_TRASSA(NTR))			! ������� ������
    END SUBROUTINE RAZM1
    
    
    SUBROUTINE RAZM2() ! ��������� ��������� ������������ �������
    USE KOORDINATA
    USE DFWINA
    USE GLOBALS_LOG
    USE INPUT_RADAROGRAMMA

    IMPLICIT NONE
    INTEGER (4) JJ,J,ISTAT
!---------------------------------------------------------------------------------------------------
      ! ��������� ���� � ������������ ������������ �������
    NN2=202
     ! ������ ���� ��� ��������������
    FF1='Cfreceiver.cfg'
    FF2='NAME_KIL_METR'
    FF3='_'
    CALL Ini_Read_Str(FF1,FF2,FF3)  
    NAME_KIL_METR=FF2 
    IF(FF2.NE.'_')OPEN(UNIT=NN2,FILE=NAME_KIL_METR ,ERR=14,STATUS='OLD')
    GOTO 11
14  MESS=MESSAGEBOX(NULL,"�������� ������ ������ - ��"C,"���� � ������������� �������� �����������"C,0)    ! ���� �� ��������� ����, ��
    IF(MESS.EQ.1) THEN                                                                              ! ����������� ������
    STOP                                                                                          ! ��� ��������� �������
    END IF
! ���������� ����� ������� � ����� ������ � GPS ����������
11	IPOINT1=0							! ��������� ����� ������� � �����
	REWIND(NN2)							! ������������� ������ �� ������ ����� 
12	READ(NN2,101,IOSTAT=ISTAT)TM			! ��������� ��������� ������
	IF(ISTAT.EQ.-1) GOTO 13

	JJ=0								! ��������� ������ ������ 
	DO J=1,74
	IF(TM(J:J).NE.' ')JJ=1
	END DO
	IF(JJ.EQ.0)GOTO 12

	IPOINT1=IPOINT1+1
	GOTO 12
13	ALLOCATE (KLM(IPOINT1))			! ��������� ������� ��� ���������� �� GPS ��������� 
	ALLOCATE (SHIR(IPOINT1))
	ALLOCATE (DLGT(IPOINT1))
    ALLOCATE (R_STOLB(IPOINT1))
    ALLOCATE (RAB(NTR))
101   FORMAT(A74)
      RETURN
    END SUBROUTINE RAZM2
! open(unit=100, FILE='c:/1/OUTPUT.OUT')
! write(100,*)