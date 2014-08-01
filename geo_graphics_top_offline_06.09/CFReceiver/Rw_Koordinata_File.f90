!
!   ��������� ����� � ������ ��� ��������
!
    INTEGER*4 FUNCTION RW_KOORDINATA_FILE()
    !DEC$ ATTRIBUTES STDCALL :: RW_KOORDINATA_FILE
    USE GEOSKANGLOBALS
    USE INPUT_RADAROGRAMMA
    USE CFReceiverGlobals
    USE KOORDINATA
    USE DFWINA
    USE GLOBALS_LOG
    IMPLICIT NONE
    INTEGER*4 IRESULTAT     ! �������� ������ ��� ���������� ��������� GEOSKANGLOBALS=0
    INTEGER RET,I,J,ISTEP_BETWEEN_TRASES_OLD
    INTEGER*4 N_ST,JMIN
	REAL*8 RMIN 
    INTEGER*4, ALLOCATABLE :: I_ST(:,:)
! ������ ���� � ����� ���������������
 FF1='Cfreceiver.cfg'
        FF2='NAME_FILE_GPR'
        FF3='C:\0.gpr'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        NAME_FILE_GPR=FF2
                NN=101
                OPEN(UNIT=NN,FILE=TRIM(NAME_FILE_GPR),FORM='BINARY',STATUS='OLD')   ! ��������� ���� � �������������
                CALL RDD1(IRESULTAT,NN)                                             ! ������ ����
                CLOSE(NN)                                                           ! ��������� ���� � �������������

! ������ ����������
        FF1='Cfreceiver.cfg'
        FF2='N_DEVICE'
        FF3='����������_�����'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        N_DEVICE=FF2
! ������ ������        
        FF1='Cfreceiver.cfg'
        FF2='N_VAY'
        FF3='����'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        N_VAY=FF2
! ������ �����������        
        FF1='Cfreceiver.cfg'
        FF2='N_DIRECTION'
        FF3='������-�����'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        N_DIRECTION=FF2
! ������ ����        
        FF1='Cfreceiver.cfg'
        FF2='N_TRACK'
        FF3='1'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        N_TRACK=FF2
! ������ ��� ����� ��������        
!        FF1='Cfreceiver.cfg'
!        FF2='Ster_Lenth'
!        FF3='100'
!        CALL Ini_Read_Str(FF1,FF2,FF3)  
!        READ(FF2,*)Ster_Lenth
!        Ster_Lenth=Ster_Lenth/ntr      ! ���������� ����� �������� � ��
! ������ ���������� ��������� �����
        FF1='Cfreceiver.cfg'
        FF2='First_Point'
        FF3='0'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)First_Point
! ������ ����� ����� ��� ������ �����
        FF1='Cfreceiver.cfg'
        FF2='TIME_GPS'
        FF3='0.0'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)TIME_GPS
! ������ � ��������� ���������� ����� �������� �� INI �����
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'
        FF3='10'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)ISTEP_BETWEEN_TRASES_OLD
! ������������ � INI ����� ���
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'  
        Ster_Lenth=1000                             ! ��� ���������� ��������� �� ������ � ���� ������ ����������
        WRITE(FF3,*)FLOOR(Ster_Lenth)
        CALL Ini_Write_Str(FF1,FF2,FF3)
! ������������ ���� ��������� �������� � ������������
    CALL RAZM1()  
! ������������ ���� ��������� ������������ �������
    CALL RAZM2()  
! ������ ������� � ��������� �� ���������� ����� ��������
! ������������� ���� ����� 'LOG' ��� 'TXT'
    CALL TIME_()
! ���������� ����� ������
    CALL TIME_TRASSA()
! ���������� ���������� ���������� ������ �� ����� ��������� ������
    CALL COORDINATORY()
! ��������� ���������� ������������ �������
! ��������� ���������� ���������� ��������� ����� � ����������, ��������� ����� ����� ��������
    CALL TAB()

! ����������� ����������
DO I=1,NTR
IZAGTR(I,3)=0
END DO

! ������� ���������� � ������������� ��������
DO I=1,IPOINT1
DO J=1,NTR
RAB(J)=RABZEM*2.D0*DASIN(DSQRT(DSIND((SHIR(I)-SHIROTA_TRASSA(J))/2)**2+DCOSD(SHIR(I))*DCOSD(SHIROTA_TRASSA(J))&
*DSIND((DLGT(I)-DOLGOTA_TRASSA(J))/2)**2))

END DO
RMIN=RAB(1)
JMIN=1
DO J=2,NTR
IF(RMIN.GT.RAB(J))THEN
JMIN=J    
RMIN=RAB(J)
END IF
END DO
IF(JMIN.NE.NTR)IZAGTR(JMIN,3)=KLM(I)*1000000                       ! � �����������

END DO


! ������� ����� ������� �� �������
N_ST=0
DO I=1,NTR
IF(IZAGTR(I,3).NE.0) THEN
N_ST=N_ST+1    
END IF
END DO

IF(N_ST.LT.2) THEN         
MESS=MESSAGEBOX(NULL,"�������� ������ ������ - ��"C,&
"����� ������� ������ 2"C,0)    
IF(MESS.EQ.1) THEN                                      
STOP                                                    
END IF 
END IF

ALLOCATE(I_ST(N_ST,2)); I_ST=0
! ��������� ���������
J=0
DO I=1,NTR
IF(IZAGTR(I,3).NE.0) THEN
J=J+1
I_ST(J,1)=I              ! ����� ������
I_ST(J,2)=IZAGTR(I,3)    ! ����������

IF(J.GE.N_ST) GOTO 2
END IF
END DO
2 CONTINUE



! ����������� ���������� ���� �����

DO 5 I=1,NTR
    IF(I.LT.I_ST(1,1)) THEN         ! ������ �� ������� ������
    IZAGTR(I,3)= I_ST(1,2)-(I_ST(2,2)-I_ST(1,2))/(I_ST(2,1)-I_ST(1,1))*(I_ST(1,1)-I) 
    GOTO 5
    END IF
    
    IF(I.GT.I_ST(N_ST,1)) THEN 
    IZAGTR(I,3)= I_ST(N_ST,2)+(I_ST(N_ST,2)-I_ST(N_ST-1,2))/(I_ST(N_ST,1)-I_ST(N_ST-1,1))*(I-I_ST(N_ST,1)) 
    GOTO 5
    END IF
    
    IF(I.EQ.I_ST(N_ST,1)) THEN 
    IZAGTR(I,3)= I_ST(N_ST,2)
    GOTO 5
    END IF   
    
    DO J=1,N_ST-1
    IF(I.GE.I_ST(J,1).AND.I.LT.I_ST(J+1,1)) THEN
    IZAGTR(I,3)= I_ST(J,2)+(I_ST(J+1,2)-I_ST(J,2))/(I_ST(J+1,1)-I_ST(J,1))*(I-I_ST(J,1)) 
    GOTO 5
    END IF
    END DO

5 CONTINUE
           
! ��������� ������� ���������
            CALL TABLE_() 
            DO I=1,NCHANAL_GEO                                             ! �������� ����� ���������� �������
            FLAG_CHANAL(I)=0
            END DO
! ��������� ������� � ����������
            DO I=1,Ntr
            if( flag_file.eq.1) goto 10  
            CALL GEOSCAN_INTEGRAL(IRESULTAT,I,NCHANAL(I))                                            ! ��������� ������� ���������    ! ������ �� ��������� � DLL ���������                  
            END DO
            flag_file=1
! ��������� � ������
10          CALL DE_RDD1()                                                      ! ������� ���������� ������            
            DEALLOCATE (FLAG_CHANAL)                                            ! ������� ���������� ������ ���������� �������
            DEALLOCATE (table)                                                  ! ������� ���������� ������� ���������
! ��������������� INI ����
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'        
        WRITE(FF3,*)ISTEP_BETWEEN_TRASES_OLD
        CALL Ini_Write_Str(FF1,FF2,FF3)
RW_KOORDINATA_FILE=1
IF( IRESULTAT.EQ.0)  RW_KOORDINATA_FILE=0     
DEALLOCATE (VREMYA)			 
DEALLOCATE (SHIROTA)
DEALLOCATE (DOLGOTA)
DEALLOCATE (VREMYA_TRASSA)			
DEALLOCATE (R_TRASSA)				
DEALLOCATE (SHIROTA_TRASSA)			
DEALLOCATE (DOLGOTA_TRASSA)			
DEALLOCATE (KLM) 
DEALLOCATE (SHIR)
DEALLOCATE (DLGT)
DEALLOCATE (R_STOLB)
DEALLOCATE (RAB)
DEALLOCATE (I_ST)
RETURN
END

! open(unit=100, FILE='c:/1/OUTPUT.OUT')
! write(100,*)