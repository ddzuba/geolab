SUBROUTINE GEOSCAN_INTEGRAL(IRESULTAT,II,NNCHAN)
USE GEOSKANGLOBALS
USE INPUT_RADAROGRAMMA
use dflogm
use CFReceiverGlobals

  implicit none
  INTEGER*4 IRESULTAT     ! �������� ������ ��� ���������� ��������� GEOSKANGLOBALS=0
  INTEGER*4 II,NNCHAN                               ! ����� ������ � ����� ������
  INTEGER*4 J,JJ                                    ! ������� ����
  INTEGER*2 J1                                      ! ������� ����
  integer*4 I,I1,I2
  INTEGER*4 IRAB 
  CHARACTER*1 :: trace_sample(8000)
  integer*2   :: trace_data(5717) 
  CHARACTER*50:: STR50
  CHARACTER*4 :: STR4
  CHARACTER*1 :: STR1  
  CHARACTER*2 :: STR2  
  INTEGER*2 KM,MET,MMET
IRESULTAT=0
  
 JJ=4
!------------------------------------------------------------------------------------
! ��������� ����� ����������
J1=0                        ! ����� ���� � ������ � utf-8
I1=LEN(TRIM(N_DEVICE))      ! ���������� ����� ������
DO I=1,I1                   ! ���� �� ���� ��������
CALL  win1251_utf(STR50,N_DEVICE(I:I),J)
      DO I2=1,J
      trace_sample(JJ+2+I2+J1)=STR50(I2:I2)
      END DO
J1=J1+J
END DO
STR2=TRANSFER(J1,STR2)      ! ����� ���� � ������ � UTF-8 ��������� � ���� ������ ���������
trace_sample(JJ+1)=STR2(1:1)
trace_sample(JJ+2)=STR2(2:2)
JJ=JJ+2+J1                  ! ������� ������ � UTF-8
!----------------------------------------------------------------------------------
! ��������� �������� ������
J1=0                        ! ����� ���� � ������ � utf-8
I1=LEN(TRIM(N_VAY))         ! ���������� ����� ������
DO I=1,I1                   ! ���� �� ���� ��������
CALL  win1251_utf(STR50,N_VAY(I:I),J)
        DO I2=1,J
        trace_sample(JJ+2+I2+J1)=STR50(I2:I2)
        END DO
J1=J1+J
END DO
STR2=TRANSFER(J1,STR2)      ! ����� ���� � ������ � UTF-8 ��������� � ���� ������ ���������
trace_sample(JJ+1)=STR2(1:1)
trace_sample(JJ+2)=STR2(2:2)
JJ=JJ+2+J1                  ! ������� ������ � UTF-8
!----------------------------------------------------------------------------------
! ��������� �������� �����������
J1=0                        ! ����� ���� � ������ � utf-8
I1=LEN(TRIM(N_DIRECTION))   ! ���������� ����� ������
DO I=1,I1                   ! ���� �� ���� ��������
CALL  win1251_utf(STR50,N_DIRECTION(I:I),J)
        DO I2=1,J
        trace_sample(JJ+2+I2+J1)=STR50(I2:I2)
        END DO
J1=J1+J
END DO
STR2=TRANSFER(J1,STR2)      ! ����� ���� � ������ � UTF-8 ��������� � ���� ������ ���������
trace_sample(JJ+1)=STR2(1:1)
trace_sample(JJ+2)=STR2(2:2)
JJ=JJ+2+J1                  ! ������� ������ � UTF-8
!----------------------------------------------------------------------------------
! �������� ����� ����
J1=0                        ! ����� ���� � ������ � utf-8
I1=LEN(TRIM(N_TRACK))       ! ���������� ����� ������
DO I=1,I1                   ! ���� �� ���� ��������
CALL  win1251_utf(STR50,N_TRACK(I:I),J)
        DO I2=1,J
        trace_sample(JJ+2+I2+J1)=STR50(I2:I2)
        END DO
J1=J1+J
END DO
STR2=TRANSFER(J1,STR2)      ! ����� ���� � ������ � UTF-8 ��������� � ���� ������ ���������
trace_sample(JJ+1)=STR2(1:1)
trace_sample(JJ+2)=STR2(2:2)
JJ=JJ+2+J1                  ! ������� ������ � UTF-8
!-------------------------------------------------------------------------------------
! �������� �������� �����
STR4= transfer(IZAGTR(II,1),STR4)
trace_sample(JJ+1)=STR4(1:1)
trace_sample(JJ+2)=STR4(2:2)
trace_sample(JJ+3)=STR4(3:3)
trace_sample(JJ+4)=STR4(4:4)
STR4= transfer(IZAGTR(II,2),STR4)
trace_sample(JJ+5)=STR4(1:1)
trace_sample(JJ+6)=STR4(2:2)
trace_sample(JJ+7)=STR4(3:3)
trace_sample(JJ+8)=STR4(4:4)
JJ=JJ+8						! ������� �� ������ ��������� (��������� �������)
!------------------------------------------------------------------------
! �������� ���������� ������
KM=IZAGTR(II,3)/1000000
STR2=transfer(KM,STR2)
trace_sample(JJ+1)=STR2(1:1)
trace_sample(JJ+2)=STR2(2:2)

MET=(IZAGTR(II,3)/1000-KM*1000)
STR2=transfer(MET,STR2)
trace_sample(JJ+3)=STR2(1:1)
trace_sample(JJ+4)=STR2(2:2)

MMET=IZAGTR(II,3)-KM*1000000-MET*1000
STR2=transfer(MMET,STR2)
trace_sample(JJ+5)=STR2(1:1)
trace_sample(JJ+6)=STR2(2:2)

JJ=JJ+6							! ������� �� ������ ��������� (��������� �������)	

!----------------------------------------------------------------------------------
! ��������� ����� ��������
STR4=TRANSFER(JJ,STR4)      ! ����� ���� � ������ � UTF-8 ��������� � ���� ������ ���������
trace_sample(1)=STR4(1:1)
trace_sample(2)=STR4(2:2)
trace_sample(3)=STR4(3:3)
trace_sample(4)=STR4(4:4)
!---------------------------------------------------------------------------------
FLAG_CHANAL(NNCHAN)=1                 ! ��������� ���������� ������
    
J=-1
DO I=1,512
IF(I.EQ.512) THEN 
IRAB=0
ELSE
IRAB=RSUM(II,I)                          ! ����� ������
END IF
STR2=TRANSFER(IRAB,STR2) 
J=J+2
trace_sample((NNCHAN-1)*1024+JJ+J)=STR2(1:1)
trace_sample((NNCHAN-1)*1024+JJ+J+1)=STR2(2:2)
END DO

DO I=1,NCHANAL_GEO
IF(FLAG_CHANAL(I).EQ.0) THEN
IRESULTAT=0
RETURN 
END IF
END DO

call sendTrace(LOC(trace_sample), 0)
            
DO I=1,NCHANAL_GEO                                              ! �������� ����� ���������� �������
FLAG_CHANAL(I)=0                                                ! ����� ������������ �������
END DO

IRESULTAT=1
RETURN
END SUBROUTINE GEOSCAN_INTEGRAL

! open(unit=100, FILE='D:/OUTPUT.OUT')
! write(100,*)NCHANAL_GEO,NCHANAL(I)
!open(unit=100, FILE='c:/1/OUTPUT.OUT')
!write(100,*)ii,IZAGTR(II,3)

SUBROUTINE AssignDataPointersToGeoscanGlobals()
USE INPUT_RADAROGRAMMA
use CFReceiverGlobals
  implicit none
  mainGeoData%chan_count => NTR
  mainGeoData%chan_size => N    
END