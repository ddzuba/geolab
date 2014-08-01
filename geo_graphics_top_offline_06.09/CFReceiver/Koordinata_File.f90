!
!   ��������� ����� � ������ ��� ��������
!
    INTEGER*4 FUNCTION KOORDINATA_FILE()
    !DEC$ ATTRIBUTES STDCALL :: KOORDINATA_FILE
    USE GEOSKANGLOBALS
    USE INPUT_RADAROGRAMMA
    USE CFReceiverGlobals
    USE KOORDINATA
    USE GLOBALS_LOG
    IMPLICIT NONE
    INTEGER*4 IRESULTAT     ! �������� ������ ��� ���������� ��������� GEOSKANGLOBALS=0
    INTEGER RET,I,ISTEP_BETWEEN_TRASES_OLD
	REAL*8 R_RAB  
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
! ������ ������� � ��������� �� ���������� ����� ��������
! ������������� ���� ����� 'LOG' ��� 'TXT'
    CALL TIME_()
! ���������� ����� ������
    CALL TIME_TRASSA()
! ���������� ���������� ���������� ������ �� ����� ��������� ������
    CALL COORDINATORY()
! ���������� ���������� ������������ ������ ������ � ��������� �� ����������
    CALL OTN_RASST()
! ����������� ����������
            IZAGTR(1,3)=First_Point*1000            ! � �����������
            R_RAB=0.D0
            DO I=2,NTR
            R_RAB=R_RAB+ R_TRASSA(I)*1000   ! � �����������
            IZAGTR(I,3)=IZAGTR(1,3)+R_RAB
            END DO
! ��������� ������� ���������
            CALL TABLE_() 
            DO I=1,NCHANAL_GEO                                             ! �������� ����� ���������� �������
            FLAG_CHANAL(I)=0
            END DO
! ��������� ������� � ����������
            DO I=1,Ntr
            if( flag_file.eq.1) goto 1  
            CALL GEOSCAN_INTEGRAL(IRESULTAT,I,NCHANAL(I))                                            ! ��������� ������� ���������    ! ������ �� ��������� � DLL ���������                  
            END DO
            flag_file=1
! ��������� � ������
1           CALL DE_RDD1()                                                      ! ������� ���������� ������            
            DEALLOCATE (FLAG_CHANAL)                                            ! ������� ���������� ������ ���������� �������
            DEALLOCATE (table)                                                  ! ������� ���������� ������� ���������
! ��������������� INI ����
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'        
        WRITE(FF3,*)ISTEP_BETWEEN_TRASES_OLD
        CALL Ini_Write_Str(FF1,FF2,FF3)
KOORDINATA_FILE=1
IF( IRESULTAT.EQ.0)  KOORDINATA_FILE=0   
DEALLOCATE (VREMYA)			 
DEALLOCATE (SHIROTA)
DEALLOCATE (DOLGOTA)
DEALLOCATE (VREMYA_TRASSA)			
DEALLOCATE (R_TRASSA)				
DEALLOCATE (SHIROTA_TRASSA)			
DEALLOCATE (DOLGOTA_TRASSA)	
RETURN
END

! open(unit=100, FILE='c:/1/OUTPUT.OUT')
! write(100,*)