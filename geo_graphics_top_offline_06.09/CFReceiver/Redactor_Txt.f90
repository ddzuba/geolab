!-----------------------------------------------------------------------------------
integer*4 function redactor_TXT()
    !DEC$ ATTRIBUTES STDCALL :: redactor_txt
    use Globals_Log
    USE DFWINA
    implicit none
    CHARACTER*100 FF1,FF2,FF3
    redactor_txt=0
! ������ ������� �������� ��� ��������������
        FF1='Cfreceiver.cfg'
        FF2='POLINOM'
        FF3='2'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)POLINOM
    ! ������� ������������� ����
3   NN=200
    ! ������ ���� ��� ��������������
        FF1='Cfreceiver.cfg'
        FF2='NAME_FILE_TXT'
        FF3='______________'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        NAME_FILE_TXT=FF2
    OPEN(UNIT=NN,FILE=TRIM(NAME_FILE_TXT) ,ERR=1,STATUS='OLD')          ! ��������� ���� �� ������ NN
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
    FF2='FILE_TXT_RED'
    FF3='______________'
    CALL Ini_Read_Str(FF1,FF2,FF3)  
    FILE_TXT_RED=FF2 
    IF(TRIM(NAME_FILE_TXT).EQ.TRIM(FILE_TXT_RED)) THEN
        MESS=MESSAGEBOX(NULL,"�������� ����������������� ���� - ��, �������� ������ - ���"C,"����������������� ���� ��� ����������"C,4)    ! ���� �� ��������� ����, ��
        IF(MESS.EQ.6) THEN                                                                              ! ����������� ������
    ! ��������� ������������� ����
        GOTO 2                                ! ��������� ������
        ELSE
        STOP                                                                                          ! ��� ��������� �������
        END IF
    END IF
    OPEN(UNIT=NN1,FILE=FILE_TXT_RED ,ERR=4,STATUS='NEW')
    GOTO 5
4   MESS=MESSAGEBOX(NULL,"���������� ������ ������ - ��, �������� ������ - ���"C,"���������� ��������� ����������������� ����"C,4)    ! ���� �� ��������� ����, ��
 	IF(MESS.EQ.6) THEN                                                                              ! ����������� ������
        OPEN(UNIT=NN1,FILE=FILE_TXT_RED ,ERR=4,STATUS='REPLACE') ! ��������� ����������������� ����
        GOTO 5                                ! ��������� ������
        ELSE
        STOP                                                                                          ! ��� ��������� �������
        END IF
5       CONTINUE
  CALL READ_WRITE_TXT()
  CLOSE(NN)
  CLOSE(NN1)
  redactor_txt=1
  RETURN
    end 
    
    SUBROUTINE READ_WRITE_TXT()	    ! ������ ������� � ��������� ������ MagicMaps2Go 
                                ! �� ���������� ����� ��������
	USE DFLIB
	USE KOORDINATA
	USE GLOBALS_LOG
    USE GDI32		!��� ����������� ��������� �������
    USE KERNEL32	!��� ������� �������
    use dfwin
!    USE GEOSKANGLOBALS
    IMPLICIT NONE
    REAL*8 FX_INTERPOL
    REAL*8 B(4),SSPOLY(4),STAT(10)
	INTEGER(4) ISTAT,K,ret
	INTEGER(4) A(2),A1(2),A2(2),A3(2),I,I1,JJ,J
    type (T_FILETIME)::   fTime
    type (T_SYSTEMTIME):: sTime
       integer(2)  WYear
       integer(2)  wMonth 
       integer(2)  WDayofWeek
       integer(2)  wDay 
       integer(2)  wHour 
       integer(2)  wMinute 
       integer(2)  wSecond 
       integer(2)  wMilliseconds 
    REAL*8 , ALLOCATABLE::VREMYA1(:)
    REAL*8 , ALLOCATABLE::SHIROTA1(:)
    REAL*8 , ALLOCATABLE::DOLGOTA1(:)
    REAL*8 , ALLOCATABLE::VR(:)
    REAL*8 , ALLOCATABLE::SHR(:)
    REAL*8 , ALLOCATABLE::DOL(:)
    
! ������������� ���������� IMSL
INCLUDE 'link_fnl_static.h'
!DEC$ OBJCOMMENT LIB:'libiomp5md.lib'
!-----------------------------------------------------------------------------
! ���������� ����� ������� � ����� ������ � GPS ����������
	IPOINT=0							! ��������� ����� ������� � �����
	REWIND(NN)							! ������������� ������ �� ������ ����� 
12	READ(NN,101,IOSTAT=ISTAT)TM			! ��������� ��������� ������
	IF(ISTAT.EQ.-1) GOTO 13

	JJ=0								! ��������� ������ ������ 
	DO J=1,74
	IF(TM(J:J).NE.' ')JJ=1
	END DO
	IF(JJ.EQ.0)GOTO 12

	IPOINT=IPOINT+1
	GOTO 12
!---------------------------------------------------------------------------------------------------
! ��������� ������� ��� ���������� �� ����� ���������
13 CONTINUE
      ALLOCATE (VREMYA(IPOINT))			    ! ����� ����������� ������
      ALLOCATE (SHIROTA(IPOINT))			! ������ ������
      ALLOCATE (DOLGOTA(IPOINT))			! ������� ������
      
! ----------------------------------------------------------------------------
! ��������� ����� � ����������� TXT, ������� ����������� � ��� ������ ������� ��������� � ���������� ����
! �������� �����    �����������     ������		�������		���������� �����
!	������ ������   ������ ������   ������	����	�����	    
!!46.4162672!!41.4615095!!17.8000000!!09.04.13!!10:33:43
!--------------------------------------------------------------------------------
	K=0					! ��������� ����� ������� � �����
	REWIND(NN)					! ������������� ������ �� ������ ����� 
1	READ(NN,101,IOSTAT=ISTAT)TM	! ��������� ��������� ������
101 FORMAT(A74)
! -----------------------------------------------------------------------------
    IF(ISTAT.EQ.-1) THEN
	GOTO 5		! ������������ ����� �����
	END IF
	
	JJ=0		! ��������� ������ ������ 
	DO J=1,74
	IF(TM(J:J).NE.' ')JJ=1
	END DO
	IF(JJ.EQ.0)GOTO 1
	 
	K=K+1
! ���������� �����-------------------------------------------------------------
! ��������� ��������� �����
DO I=1,74
    IF(TM(I:I).EQ.':')THEN
    CALL CALC(1,I-5,I-4,TM,A)    
    WYear= 2000+a(1)                					                ! ���
    CALL CALC(1,I-8,I-7,TM,A)
    wMonth= a(1)                                                        ! �����
    CALL CALC(1,I-11,I-10,TM,A)    
    wDay= a(1)                      					                ! ����
    CALL CALC(1,I-2,I-1,TM,A)   
    wHour= a(1)					                                        ! ���
    CALL CALC(1,I+1,I+2,TM,A)    
    wMinute= a(1)				                                        ! ������
    CALL CALC(1,I+4,I+5,TM,A)    
    wSecond= a(1)				                                    	! �������
    wMilliseconds= 0                                					! �����������
    WDayofWeek=WDAYOFWEEK
   
    
    sTime%WYEAR=wYear          
    sTime%WMONTH=wMonth  
    sTime%WDayofWeek=WDayofWeek       
    sTime%WDAY=wDay           
    sTime%WHOUR=wHour          
    sTime%WMINUTE=wMinute        
    sTime%WSECOND=wSecond        
    sTime%WMILLISECONDS=wMilliseconds  

    GOTO 2
    END IF
    END DO
2   ret = SystemTimeToFileTime(sTime,fTime)
    A(1)=fTime%dwLowDateTime
    A(2)=fTime%dwHighDateTime

    CALL TIMEGPS(A(1),A(2),VREMYA(K))                          ! ����� � ��������
! ----------------------------------------------------------------------------
! ������ ����� ���������
! ���������� ���� 
    DO I=1,74
    IF(TM(I:I).EQ.'.')THEN
    CALL CALC(1,I-2,I-1,TM,A)
    CALL CALC(1,I+1,I+7,TM,A1) 
        DO I1=I+1,74
        IF(TM(I1:I1).EQ.'.')THEN 
        CALL CALC(1,I1-2,I1-1,TM,A2)
        CALL CALC(1,I1+1,I1+7,TM,A3) 
        GOTO 3
        END IF
        END DO
    END IF
    END DO
3   SHIROTA(K)=A(1)+A1(1)/1.D7	! ���� � ��������
    DOLGOTA(K)=A2(1)+A3(1)/1.D7	! ���� � ��������
	GOTO 1					! ��������� �� ����� ������ �����
! ������������� ��������
5 CONTINUE
IPOINT1=VREMYA(K)-VREMYA(1) !����� ������

ALLOCATE(VREMYA1(IPOINT1))
ALLOCATE(SHIROTA1(IPOINT1))
ALLOCATE(DOLGOTA1(IPOINT1))
ALLOCATE(VR(4))
ALLOCATE(SHR(4))
ALLOCATE(DOL(4))

VREMYA1(1)=VREMYA(1)
SHIROTA1(1)=SHIROTA(1)
DOLGOTA1(1)=DOLGOTA(1)
DO I=2,IPOINT1
VREMYA1(I)=VREMYA1(I-1)+1    
IF(VREMYA1(I).LT.VREMYA(1).OR.VREMYA1(I).LE.VREMYA(2)) THEN
VR(1)=VREMYA(1);VR(2)=VREMYA(2);VR(3)=VREMYA(3);VR(4)=VREMYA(4)
SHR(1)=SHIROTA(1);SHR(2)=SHIROTA(2);SHR(3)=SHIROTA(3);SHR(4)=SHIROTA(4)
DOL(1)=DOLGOTA(1);DOL(2)=DOLGOTA(2);DOL(3)=DOLGOTA(3);DOL(4)=DOLGOTA(4)
GOTO 4
END IF
IF(VREMYA1(I).GT.VREMYA(K-1)) THEN
VR(1)=VREMYA(K-3);VR(2)=VREMYA(K-2);VR(3)=VREMYA(K-1);VR(4)=VREMYA(K)
SHR(1)=SHIROTA(K-3);SHR(2)=SHIROTA(K-2);SHR(3)=SHIROTA(K-1);SHR(4)=SHIROTA(K)
DOL(1)=DOLGOTA(K-3);DOL(2)=DOLGOTA(K-2);DOL(3)=DOLGOTA(K-1);DOL(4)=DOLGOTA(K)
GOTO 4
END IF
    DO J=2,K-1
    IF(VREMYA1(I).LE.VREMYA(J).AND.VREMYA1(I).GT.VREMYA(J-1))THEN
    VR(1)=VREMYA(J-2);VR(2)=VREMYA(J-1);VR(3)=VREMYA(J);VR(4)=VREMYA(J+1)
    SHR(1)=SHIROTA(J-2);SHR(2)=SHIROTA(J-1);SHR(3)=SHIROTA(J);SHR(4)=SHIROTA(J+1)
    DOL(1)=DOLGOTA(J-2);DOL(2)=DOLGOTA(J-1);DOL(3)=DOLGOTA(J);DOL(4)=DOLGOTA(J+1)
    GOTO 4
    END IF
    END DO
4   CONTINUE

    CALL DRCURV(4,VR,SHR,POLINOM-1,B,SSPOLY,STAT)
    SHIROTA1(I)=FX_INTERPOL(VREMYA1(I),B,POLINOM)
    CALL DRCURV(4,VR,DOL,POLINOM-1,B,SSPOLY,STAT)
    DOLGOTA1(I)=FX_INTERPOL(VREMYA1(I),B,POLINOM)
END DO

DEALLOCATE (VREMYA)
DEALLOCATE (SHIROTA)
DEALLOCATE (DOLGOTA)
IPOINT=IPOINT1
ALLOCATE(VREMYA(IPOINT))
ALLOCATE(SHIROTA(IPOINT))
ALLOCATE(DOLGOTA(IPOINT))
DO I=1,IPOINT
VREMYA(I)=VREMYA1(I)    
SHIROTA(I)=SHIROTA1(I) 
DOLGOTA(I)=DOLGOTA1(I)
END DO

DO I=1,IPOINT
WRITE(NN1,*)  VREMYA(I),SHIROTA(I),DOLGOTA(I),';'
END DO
DEALLOCATE (VREMYA)
DEALLOCATE (SHIROTA)
DEALLOCATE (DOLGOTA)
DEALLOCATE (VREMYA1)
DEALLOCATE (SHIROTA1)
DEALLOCATE (DOLGOTA1)
DEALLOCATE (VR)
DEALLOCATE (SHR)
DEALLOCATE (DOL)



    RETURN
    END SUBROUTINE READ_WRITE_TXT
    
! open(unit=100, FILE='D:/OUTPUT.OUT')
! write(100,*)POLINOM