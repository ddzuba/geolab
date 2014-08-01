SUBROUTINE INTEGRAL_INPUT(MyData)
USE PARAM_1
USE RAILWAY
USE FReciverGlobals
USE GDI32		!��� ����������� ��������� �������
USE KERNEL32	!��� ������� �������
USE N_
IMPLICIT NONE
INTEGER*4 N1
INTEGER*4 J,I,II
CHARACTER(1):: MyData(*) ! TODO
INTEGER*4 OnClose
TYPE (T_FILETIME)::FTS
TYPE (T_SYSTEMTIME)::	STT
INTEGER*4 TT1,TT2,iRet
INTEGER*2 km,met,mm
INTEGER*2 IRAB
integer*1 irab1,irab2
REAL*4 RAB

N_DEVICE1(1:50)=   '                                                  '
N_VAY1(1:50)=      '                                                  '
N_DIRECTION1(1:50)='                                                  '
N_TRACK1(1:50)=    '                                                  '

N1= transfer(MyData(1:4),N1)	! ����� �������� ������
J=4								! ������� �� ������ ��������� (��������� �������)

! ������� �� ������� �������-------------------------------------
IF(N1.LT.0) THEN
II=  OnClose()	! ���������� ������ ! ������ � ��������� �����
STOP
END IF
!----------------------------------------------------------------
! ��������� ����� ����������
N2_DEV1= transfer(MyData(J+1:J+2),N2_DEV1)					! ����� ������ 
IF(N2_DEV1.EQ.0) THEN
	N2_DEV1=1
	N_DEVICE1='0'
    J=J+2
ELSE
	CALL UTF8(N_DEVICE1,MyData,J+3,N2_DEV1) ! ������ ����������
	J=J+2+N2_DEV1											! ������� �� ������ ��������� (��������� �������)
		IF(N_DEVICE1(1:N2_DEV1).EQ.'Unknown')THEN
		N2_DEV1=1
		N_DEVICE1='0'
		END IF
END IF
!----------------------------------------------------------------
	! ��������� ����� ������
N2_VAY1= transfer(MyData(J+1:J+2),N2_VAY1)					! ����� ������ 
IF(N2_VAY1.EQ.0) THEN
	N_VAY1(1:1)='0'
	N2_VAY1=1
    J=J+2
ELSE
	CALL UTF8(N_VAY1,MyData,J+3,N2_VAY1) ! ������ ����������
	J=J+2+N2_VAY1											! ������� �� ������ ��������� (��������� �������)
		IF(N_VAY1(1:N2_VAY1).EQ.'<null>')THEN
		N_VAY1='0'
		N2_VAY1=1
		END IF
END IF
!----------------------------------------------------------------
	! ��������� ����� �����������
N2_DIR1= transfer(MyData(J+1:J+2),N2_DIR1)						! ����� ������ 
IF(N2_DIR1.EQ.0) THEN
	N_DIRECTION1='0'
	N2_DIR1=1
    J=J+2
ELSE
	CALL UTF8(N_DIRECTION1,MyData,J+3,N2_DIR1) ! ������ ����������
	J=J+2+N2_DIR1												! ������� �� ������ ��������� (��������� �������)
		IF(N_DIRECTION1(1:N2_DIR1).EQ.'<null>')THEN
		N_DIRECTION1='0'
		N2_DIR1=1
		END IF
END IF
!----------------------------------------------------------------
	! ��������� ����� ����
N2_TRA1= transfer(MyData(J+1:J+2),N2_TRA1)					! ����� ������ 
IF(N2_TRA1.EQ.0) THEN
	N_TRACK1='0'
	N2_TRA1=1
    J=J+2
ELSE
	CALL UTF8(N_TRACK1,MyData,J+3,N2_TRA1) ! ������ ����������
	J=J+2+N2_TRA1
		IF(N_TRACK1(1:N2_TRA1).EQ.'<null>')THEN
		N_TRACK1='0'
		N2_TRA1=1
        END IF
END IF
!---------------------------------------------------------------------
! �������� �������� �����
TT1= transfer(MyData(J+1:J+4),tt1)					
TT2= transfer(MyData(J+5:J+8),tt1)
J=J+8									! ������� �� ������ ��������� (��������� �������)

! ��������� � ��������� �����
FTS%dwLowDateTime=TT1
FTS%dwHighDateTime=TT2
iRet = FileTimeToSystemTime( LOC(FTS), LOC(STT) )
wYear=stt%wYear
wMonth=stt%wMonth
wDay=stt%wDay
wHour=stt%wHour
wMinute=stt%wMinute
wSecond=stt%wSecond 
wMilliseconds=stt%wMilliseconds
!------------------------------------------------------------------------
! �������� ���������� ������
km=transfer(MyData(J+1:J+2),km)
met=transfer(MyData(J+3:J+4),met)
mm=transfer(MyData(J+5:J+6),mm)
J=J+6								! ������� �� ������ ��������� (��������� �������)
kilometr=km							! ��������� � ��� REAL
metr=met+mm/1000.					! ��������� � ��� REAL
IF(METR.GT.1000) THEN
KM=KM+1
MET=MET-1000
KILOMETR=KILOMETR+1
METR=METR-1000
END IF
! ��������� ����������
COORDINATE1=KILOMETR*1000+METR
!----------------------------------------------------------------------
! ������ ������. ���������� �������� � ������ ���������
CALL FIRST_READING()
! ����� �������� � �������� ������
CALL SECOND_READING() 
!----------------------------------------------------------------------
!���������� �����
call TIMEVID(TT1,TT2,SEKK)
IF(TRACE_POS.EQ.0)SEKK0=sekk          !SEKK
VREMYA_MOUSE(TRACE_POS-IMG_W*floor(TRACE_POS*1./IMG_W)+1)=SEKK  !��������� ����� ����� �� ������
!---------------------------------------------------------------------
IF(N_BIT.EQ.2) THEN
DO I=1,NN_CHEN
IRAB=transfer(MyData(J+1+2*NN_CHEN_F:J+N_BIT+2*NN_CHEN_F),IRAB) ! ���������� ��� ������
MyTrassa(I)=IRAB
J=J+N_BIT
END DO
END IF

IF(N_BIT.EQ.4) THEN
DO I=1,NN_CHEN
RAB=transfer(MyData(J+1+4*NN_CHEN_F:J+N_BIT+4*NN_CHEN_F),RAB) ! ���������� ��� ������
J=J+N_BIT
MyTrassa(I)=RAB
END DO
END IF

RETURN
END 


!open(unit=100, FILE='D:/OUTPUT.OUT')
! write(100,*)kilometr,METR
