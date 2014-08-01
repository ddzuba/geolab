    SUBROUTINE TIME_()	  ! ������ ������� � ���������  
                          ! �� ���������� ����� ��������
	USE DFWIN										
	USE KOORDINATA
	USE  GLOBALS_LOG
    IMPLICIT NONE
	INTEGER(4) A(2),IP1,IP2,IP3,ISTAT
	REAL(8) AAA,AAA1,AAA2
    INTEGER (4) I
    NN1=201
	REWIND(NN1)					! ������������� ������ �� ������ ����� 
! ���������� ��� �������---------------------------------------------------------------
    READ(NN1,101,IOSTAT=ISTAT)TM	         ! ��������� ��������� ������
    DO I=1,100                                ! ���������� ��������� �� ����������� �������������� ���������

    IF(TM(I:I).EQ.'N') THEN
    CALL TIME_LOG()         ! ������ Geoscan.log
    GOTO 1
    END IF

    IF(TM(I:I).EQ.':') THEN
    CALL TIME_TXT()         ! ��������� ����, ���������������� �� MagicMaps2Go
    GOTO 1
    END IF

    IF(TM(I:I).EQ.';') THEN
    CALL TIME_RED()         ! ����������������� ��������� ����, ���������������� �� MagicMaps2Go
    GOTO 1
    END IF
    
    END DO
    
    MESS=MESSAGEBOX(NULL,"�������� ������ ������ - ��"C,"����������� ������ ����� ����������� ���������"C,0)    ! ���� �� ��������� ����, ��
    IF(MESS.EQ.1) THEN                                                                                          ! ����������� ������
    STOP                                                                                                        ! ��� ��������� �������
    END IF      
101 FORMAT(A74)
1   RETURN
	END
!
! ������ � ����� ��������
!
    SUBROUTINE TIME_LOG()	! ������ ������� � ���������  ������ Geoscan.log
                              ! �� ���������� ����� ��������
	USE DFLIB										
	USE KOORDINATA
	USE  GLOBALS_LOG
    IMPLICIT NONE
	INTEGER(4) A(2),IP1,IP2,IP3,ISTAT,JJ,J
	REAL(8) AAA,AAA1,AAA2
! ----------------------------------------------------------------------------
! ��������� ����� � ����������� LOG, ������� ����������� � GEOSKAN ��� ������
! ������� ��������� � ���������� ����
! �������� �����    �����������     ������		�������		���������� �����
!					�����	  ������ ������   ������ ������   ������   ���������
!!!128612775822812500!!32783.00!!!47!6.14730!N!!!39!42.98770!E!!!17.8000!!!10
!--------------------------------------------------------------------------------
    NN1=201
	IPOINT=0					! ��������� ����� ������� � �����
	REWIND(NN1)					! ������������� ������ �� ������ ����� 
1	READ(NN1,101,IOSTAT=ISTAT)TM	! ��������� ��������� ������
101   FORMAT(A74)
	IF(ISTAT.EQ.-1) THEN
	RETURN		! ������������ ����� �����
	END IF
	
	JJ=0		! ��������� ������ ������ 
	DO J=1,74
	IF(TM(J:J).NE.' ')JJ=1
	END DO
	IF(JJ.EQ.0)GOTO 1	 
	IPOINT=IPOINT+1
! ���������� �����-------------------------------------------------------------
	A(1)=0
	A(2)=0
    AAA=0.D0
    AAA1=0.D0
	IP1=-8
	IP2=0
    IP3=9
    DO J=1,2
    IP1=IP1+IP3
    IP2=IP2+IP3
	CALL CALC(J,IP1,IP2,TM,A)
    END DO
    AAA=A(2)
    AAA1=A(1)
    AAA2=AAA1*1000000000+AAA
	VREMYA(IPOINT)= AAA2/10000000			! ����� � ��������
! ----------------------------------------------------------------------------
! ������ ����� ���������
! ���������� ���� 
	IP1=29
	IP2=33
	CALL CALC(1,IP1,IP2,TM,A)
	SHIROTA(IPOINT)= A(1)				
! ���������� ������
	IP1=34
	IP2=IP1
	DO WHILE (TM(IP2:IP2).NE.'.')
	IP2=IP2+1 
	END DO
	IP2=IP2-1
	CALL CALC(1,IP1,IP2,TM,A)
! ���������� ���� ������
	IP1=IP2+2
	IP2=IP2+6
	CALL CALC(2,IP1,IP2,TM,A)
	SHIROTA(IPOINT)=SHIROTA(IPOINT)+(A(1)+A(2)/1.D5)/6.D1	! ���� � ��������

! ----------------------------------------------------------------------------
! ������� ����� ���������
! ���������� ���� 
	IP1=44
	IP2=48
	CALL CALC(1,IP1,IP2,TM,A)
	DOLGOTA(IPOINT)= A(1)				
! ���������� ������
	IP1=50 !49
	IP2=IP1
	DO WHILE (TM(IP2:IP2).NE.'.')
	IP2=IP2+1 
	END DO
	IP2=IP2-1
	CALL CALC(1,IP1,IP2,TM,A)
! ���������� ���� ������
	IP1=IP2+2
	IP2=IP2+6
	CALL CALC(2,IP1,IP2,TM,A)
	DOLGOTA(IPOINT)=DOLGOTA(IPOINT)+(A(1)+A(2)/1.D5)/6.D1	! ���� � ��������
	GOTO 1					! ��������� �� ����� ������ �����
    RETURN
	END 
!-----------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------
    SUBROUTINE TIME_TXT()	    ! ������ ������� � ��������� ������ MagicMaps2Go 
                                ! �� ���������� ����� ��������
	USE DFLIB										
	USE KOORDINATA
	USE GLOBALS_LOG
    USE GDI32		!��� ����������� ��������� �������
    USE KERNEL32	!��� ������� �������
    use dfwin
	INTEGER(4) ISTAT,K,ret
	INTEGER(4) A(2),A1(2),A2(2),A3(2),I,I1
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
! ----------------------------------------------------------------------------
! ��������� ����� � ����������� TXT, ������� ����������� � ��� ������ ������� ��������� � ���������� ����
! �������� �����    �����������     ������		�������		���������� �����
!	������ ������   ������ ������   ������	����	�����	    
!!46.4162672!!41.4615095!!17.8000000!!09.04.13!!10:33:43
!--------------------------------------------------------------------------------
    NN1=201
	K=0					! ��������� ����� ������� � �����
	REWIND(NN1)					! ������������� ������ �� ������ ����� 
1	READ(NN1,101,IOSTAT=ISTAT)TM	! ��������� ��������� ������
101 FORMAT(A74)
! -----------------------------------------------------------------------------
    IF(ISTAT.EQ.-1) THEN
	RETURN		! ������������ ����� �����
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
    VREMYA(K)=VREMYA(K)+TIME_GPS
    
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
    RETURN
    END SUBROUTINE TIME_TXT

!-----------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------
    SUBROUTINE TIME_RED()	    ! ������ ������� � ��������� ������ MagicMaps2Go 
                                ! �� ���������� ����� ��������
	USE DFLIB										
	USE KOORDINATA
	USE GLOBALS_LOG
    USE GDI32		!��� ����������� ��������� �������
    USE KERNEL32	!��� ������� �������
    use dfwin
	INTEGER(4) I
  !--------------------------------------------------------------------------------
    NN1=201
	K=0					! ��������� ����� ������� � �����
	REWIND(NN1)					! ������������� ������ �� ������ ����� 
DO I=1,IPOINT
READ(NN1,*)  VREMYA(I),SHIROTA(I),DOLGOTA(I)
VREMYA(i)=VREMYA(i)+TIME_GPS
END DO
! -----------------------------------------------------------------------------
    RETURN
    END SUBROUTINE TIME_RED


	FUNCTION FX_INTERPOL(J,COEFF,NDEG1)
	IMPLICIT NONE
    REAL*8 FX_INTERPOL
    INTEGER*4 NDEG1,I
	REAL*8 J,COEFF(NDEG1),FX1
	FX1=COEFF(NDEG1)
	DO I=NDEG1-1,1,-1
	FX1=COEFF(I)+J*FX1
    END DO
    FX_INTERPOL=FX1
	RETURN
	END


    
	SUBROUTINE CALC(J,IP1,IP2,TM,A)
    IMPLICIT NONE
	INTEGER(4) J,IP1,IP2,A(2)
	CHARACTER  (74) TM
	INTEGER(4) I
	A(J)=0
	DO 1 I=IP1,IP2
	IF(TM(I:I).EQ.' ')THEN
	GOTO 1
	END IF
	IF(TM(I:I).EQ.'0')THEN
	GOTO 1
	END IF
	IF(TM(I:I).EQ.'1')THEN
	A(J)=A(J)+1*10**(IP2-I)
	GOTO 1
	END IF
	IF(TM(I:I).EQ.'2')THEN
	A(J)=A(J)+2*10**(IP2-I)
	GOTO 1
	END IF
	IF(TM(I:I).EQ.'3')THEN
	A(J)=A(J)+3*10**(IP2-I)
	GOTO 1
	END IF
	IF(TM(I:I).EQ.'4')THEN
	A(J)=A(J)+4*10**(IP2-I)
	GOTO 1
	END IF
	IF(TM(I:I).EQ.'5')THEN
	A(J)=A(J)+5*10**(IP2-I)
	GOTO 1
	END IF
	IF(TM(I:I).EQ.'6')THEN
	A(J)=A(J)+6*10**(IP2-I)
	GOTO 1
	END IF	
	IF(TM(I:I).EQ.'7')THEN
	A(J)=A(J)+7*10**(IP2-I)
	GOTO 1
	END IF	
	IF(TM(I:I).EQ.'8')THEN
	A(J)=A(J)+8*10**(IP2-I)
	GOTO 1
	END IF
	IF(TM(I:I).EQ.'9')THEN
	A(J)=A(J)+9*10**(IP2-I)
	GOTO 1
	END IF
1	CONTINUE
	RETURN
    END SUBROUTINE CALC

   SUBROUTINE TIMEGPS(TTT,TTT1,GGG)		

	IMPLICIT NONE
    integer*4 TTT,TTT1
    REAL*8 AAA,AAA1
    REAL*8 GGG 
    aaa=TTT
    IF(AAA.LT.0.D0)AAA=AAA+2147483647+2147483647+2
    aaa1=TTT1
    IF(AAA1.LT.0.D0)AAA1=AAA1+2147483647+2147483647+2
    AAA1=AAA1*2147483647*2+2*AAA1+AAA          
	GGG=AAA1/10000000
	RETURN
END
   
! open(unit=100, FILE='D:/OUTPUT.OUT')
! write(100,*)
      
