!----------------------------------------------------------------------------------------------------------
! ���� ������, �������� ������������ � ������������ �������
!----------------------------------------------------------------------------------------------------------
SUBROUTINE TAB()		
USE DFLIB
USE KOORDINATA
USE INPUT_RADAROGRAMMA 
USE DFWINA

USE GLOBALS_LOG
IMPLICIT NONE
	INTEGER(4) I,J,JMIN !,I1,I_KM,I_KM2
	INTEGER(4) IPR_1,IPR_2
	REAL(8) RAB1,RAB2

! ��������� ���������� � ����������� ������������ �������
 	REWIND(NN2)								! ������������� ������ �� ������ ����� 
    DO I=1,IPOINT1
	READ(NN2,*)KLM(I),SHIR(I),DLGT(I)	! ��������� ��������� ������
    END DO
    
! ��������� ������������� ���������� ����� ������������� ��������
	R_STOLB (1)=0.D0
	DO I=2,IPOINT1
	R_STOLB(I)=RABZEM*2.D0*DASIN(DSQRT(DSIND((SHIR(I)-SHIR(I-1))/2)**2+DCOSD(SHIR(I))*DCOSD(SHIR(I-1))*DSIND((DLGT(I)-DLGT(I-1))/2)**2))
    END DO

! ������ ���� � �����, ����������� ����� �������
        FF1='Cfreceiver.cfg'
        FF2='STOLB_LENCH_FILE'
        FF3='ZERO'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        STOLB_LENCH_FILE=FF2
    IF(STOLB_LENCH_FILE.NE.'ZERO') THEN	
    NN3=300
 	OPEN(UNIT=NN3,FILE=STOLB_LENCH_FILE,ERR=1,STATUS='REPLACE') ! �������� ����� ������
    GOTO 2
1   MESS=MESSAGEBOX(NULL,"�������� ������ - ��, ���������� ������ ��� ������ - ���"C,"���� ��� ������ ���� ����� ��������� �������� �� ������"C,4)    ! ���� �� ��������� ����, ��
	IF(MESS.EQ.6) THEN                                                                              ! ����������� ������
    STOP                                ! ��������� ������
    ELSE
    GOTO 3                                                                                          ! ��� ��������� �������
    END IF
2   WRITE(NN3,*) '�������� ���������� ����� '
	WRITE(NN3,*) '������������� �������� ����� ����'
	WRITE(NN3,*) ''
	WRITE(NN3,*) '����� ������, ��������, ���������� '
	DO I=1,IPOINT1
	WRITE(NN3,101) I,KLM(I),R_STOLB(I)	! �������� ���������� ����� ������������� ��������
	END DO
101	FORMAT(I5,7X,F10.3,2X,F10.3)
	WRITE(30,*) '-----------------------------------'
    END IF
! ���������� ������������� �� ���������� ������ � ������������ ������� 
! ���� ������, �������� ������������ � ������� � IPOINT1 ������������ �������
3	I=1	! ��������� ���������� ����� ������������� ��������
4   DO J=1,NTR				! � �������� � ���������� � ������ RAB(J)
RAB(J)=RABZEM*2.D0*DASIN(DSQRT(DSIND((SHIR(I)-SHIROTA_TRASSA(J))/2)**2+DCOSD(SHIR(I))*DCOSD(SHIROTA_TRASSA(J))&
*DSIND((DLGT(I)-DOLGOTA_TRASSA(J))/2)**2))
		END DO
	IF(I.EQ.1.AND.RAB(1).GT.RAB(NTR))IPR_1=0       ! ������������ � ������� ������
	IF(I.EQ.1.AND.RAB(1).LT.RAB(NTR))IPR_1=1       ! ��������� �� ������� ������
	IF(I.EQ.IPOINT1.AND.RAB(1).GT.RAB(NTR))IPR_2=0 ! ������������ � ���������� ������
	IF(I.EQ.IPOINT1.AND.RAB(1).LT.RAB(NTR))IPR_2=1 ! ��������� �� ���������� ������
		RAB1=RAB(1)					! ������� ������� ����������
		JMIN=1
		DO J=2,NTR
		IF(RAB(J).LT.RAB1) THEN
		RAB1=RAB(J)
		JMIN=J		! ���������� ����� ������ � ��������� ����������
		END IF
		END DO
	IF(I.EQ.1.AND.DABS(RAB(JMIN)-RAB(1)).GT.15.AND.DABS(RAB(JMIN)-RAB(NTR)).GT.15)IPR_1=2           ! ������������
	IF(I.EQ.IPOINT1.AND.DABS(RAB(JMIN)-RAB(1)).GT.15.AND.DABS(RAB(JMIN)-RAB(NTR)).GT.15)IPR_2=2		! ������������
	IF(I.EQ.1)THEN
	I=IPOINT1
	GOTO 4
    END IF
    
	IF(IPR_1.EQ.0.AND.IPR_2.EQ.0.OR.IPR_1.EQ.1.AND.IPR_2.EQ.1) THEN 
    MESS=MESSAGEBOX(NULL,"�������� ������ ������ - ��"C,"����� ��������� ������������ ������� � ����������� �� ���������"C,0)    
        IF(MESS.EQ.1) THEN                                                                              ! ����������� ������
        STOP                                                                                        
        END IF
    END IF
	RETURN
	END

