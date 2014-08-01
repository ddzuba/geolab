integer*4 function redactor_log()
    !DEC$ ATTRIBUTES STDCALL :: redactor_log
    use Globals_Log
    USE DFWINA
    implicit none
    INTEGER*4 FILE_WAY
    CHARACTER*100 FF1,FF2,FF3
   
    redactor_log=0
    ! ������� ������������� ����
3   NN=200
    ! ������ ���� ��� ��������������
        FF1='Cfreceiver.cfg'
        FF2='NAME_FILE_LOG'
        FF3='______________'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        NAME_FILE_LOG=FF2
    OPEN(UNIT=NN,FILE=TRIM(NAME_FILE_LOG) ,ERR=1,STATUS='OLD')          ! ��������� ���� �� ������ NN
    GOTO 2 
1   MESS=MESSAGEBOX(NULL,"�������� ������ - ��, ��������� ����� - ���"C,"������������� ���� �� ����������"C,4)    ! ���� �� ��������� ����, ��
	IF(MESS.EQ.6) THEN                                                                              ! ����������� ������
    STOP                                ! ��������� ������
    ELSE
    GOTO 3                                                                                          ! ��� ��������� �������
    END IF
 ! ������� ����������������� ����
2   NN1=201
     ! ������ ���� ��� ��������������
    FF1='Cfreceiver.cfg'
    FF2='FILE_LOG_RED'
    FF3='______________'
    CALL Ini_Read_Str(FF1,FF2,FF3)  
    FILE_LOG_RED=FF2 
    IF(TRIM(NAME_FILE_LOG).EQ.TRIM(FILE_LOG_RED)) THEN
        MESS=MESSAGEBOX(NULL,"�������� ����������������� ���� - ��, �������� ������ - ���"C,"����������������� ���� ��� ����������"C,4)    ! ���� �� ��������� ����, ��
        IF(MESS.EQ.6) THEN                                                                              ! ����������� ������
    ! ��������� ������������� ����
        GOTO 2                                ! ��������� ������
        ELSE
        STOP                                                                                          ! ��� ��������� �������
        END IF
    END IF
    OPEN(UNIT=NN1,FILE=FILE_LOG_RED ,ERR=4,STATUS='NEW')
    GOTO 5
4   MESS=MESSAGEBOX(NULL,"���������� ������ ������ - ��, �������� ������ - ���"C,"���������� ��������� ����������������� ����"C,4)    ! ���� �� ��������� ����, ��
 	IF(MESS.EQ.6) THEN                                                                              ! ����������� ������
        OPEN(UNIT=NN1,FILE=FILE_LOG_RED ,ERR=4,STATUS='REPLACE') ! ��������� ����������������� ����
!       NAME_FILE_LOG =''
!       FILE_LOG_RED =''
        GOTO 5                                ! ��������� ������
        ELSE
        STOP                                                                                          ! ��� ��������� �������
        END IF
5 CALL READ_WRITE(NN,NN1)
  CLOSE(NN)
  CLOSE(NN1)
  redactor_log=1
  RETURN
end 

    
!
! ������ � ����� .LOG
!
      SUBROUTINE READ_WRITE(NN,NN1)	! ������ ������� � ���������  
      IMPLICIT NONE
      CHARACTER  *100 TM          ! ������������� ������
      CHARACTER  *1 STR,STR1           ! ������� ������
      INTEGER*4 NN,NN1
      INTEGER*4 I,ISTAT,J,JJ
	REWIND(NN)					! ������������� ������ �� ������ ����� 
	REWIND(NN1)					! ������������� ������ �� ������ ����� 

1	READ(NN,101,IOSTAT=ISTAT)TM	! ��������� ��������� ������
101   FORMAT(A100)
          IF(ISTAT.EQ.-1) THEN    ! ������������ ����� �����
      	RETURN		
          END IF
          	JJ=0		        ! ��������� ������ ������ 
          	DO J=1,100
          	IF(TM(J:J).NE.' ')JJ=1
              END DO
           	IF(JJ.EQ.0)GOTO 1
                  
                  STR=''          ! ��������� ���������� ��������� 
          	    DO J=1,100
          	    IF(TM(J:J).NE.' ')THEN
                    STR1=STR
                    STR=TM(J:J)
                    END IF
                  END DO
           	    IF(STR.EQ.'0'.AND.STR1.EQ.' ')GOTO 1
                  
                  JJ=0		        ! ��������� E,H � ����� ���������
              	DO J=1,100
           	    IF(TM(J:J).EQ.'N'.OR.TM(J:J).EQ.'E')JJ=1
                  END DO  
                  IF(JJ.EQ.0) GOTO 1 ! ��������� �� ����� ������ �����
                  
      WRITE(NN1,101)TM	!  ���������� ��������� ������ 
   	GOTO 1
	END SUBROUTINE READ_WRITE

! open(unit=100, FILE='D:/OUTPUT.OUT')
! write(100,*)
! open(unit=100, FILE='C:/1/OUTPUT.OUT')
! write(100,*)NAME_FILE_LOG