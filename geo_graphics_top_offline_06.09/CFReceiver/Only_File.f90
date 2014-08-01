!
!   ��������� ����� � ������ ��� ��������
!
    INTEGER*4 FUNCTION ONLY_FILE()
    !DEC$ ATTRIBUTES STDCALL :: Only_File
    USE GEOSKANGLOBALS
    USE INPUT_RADAROGRAMMA
    USE CFReceiverGlobals
    USE KOORDINATA
    USE GLOBALS_LOG
    IMPLICIT NONE
    INTEGER*4 IRESULTAT     ! �������� ������ ��� ���������� ��������� GEOSKANGLOBALS=0
    INTEGER RET,I,J,II,ISTEP_BETWEEN_TRASES_OLD

!    CALL Start()                                                                     ! �������� DLL
 !              CALL SendSampleTrace(100)
 !              call GetFileName(hInst,hwnd,filt,NAME_FILE_GPR,IRES)
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
!               CALL GEOSCAN_INPUT()                                                ! ������ ��������
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
        FF1='Cfreceiver.cfg'
        FF2='Ster_Lenth'
        FF3='100'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)Ster_Lenth
        Ster_Lenth=Ster_Lenth/ntr*1000*NCHANAL_GEO     ! ���������� ����� �������� � �� 
                                           ! �������� � ������� ����� �����
! ������ ���������� ��������� �����
        FF1='Cfreceiver.cfg'
        FF2='First_Point'
        FF3='0'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)First_Point
! ������ � ��������� ���������� ����� �������� �� INI �����
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'
        FF3='10'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)ISTEP_BETWEEN_TRASES_OLD
! ������������ � INI ����� ���
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'        
        WRITE(FF3,*)FLOOR(Ster_Lenth)
        CALL Ini_Write_Str(FF1,FF2,FF3)
! ����������� ����������
        II=1
        IZAGTR(II,3)=First_Point*1000                           ! � �����������
        IF(NCHANAL_GEO.GT.1) THEN
        DO J=2,NCHANAL_GEO
            IZAGTR(II+J-1,3)=IZAGTR(II+J-2,3)                       ! � ����������� 
        END DO
        END IF       
        II=II+NCHANAL_GEO     
        DO I=II,NTR,NCHANAL_GEO
            IZAGTR(I,3)=IZAGTR(I-NCHANAL_GEO+1,3)+int(Ster_Lenth)      ! � ����������� 
        IF(NCHANAL_GEO.GT.1) THEN
        DO J=2,NCHANAL_GEO
            IF(I+J-1.GT.NTR) GOTO 444
            IZAGTR(I+J-1,3)=IZAGTR(I+J-2,3)                         ! � �����������
        END DO
        END IF  
        END DO
444     CONTINUE         
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
ONLY_FILE=1
IF( IRESULTAT.EQ.0)  ONLY_FILE=0         
RETURN
END

! open(unit=100, FILE='c:/1/OUTPUT.OUT')
! write(100,*)