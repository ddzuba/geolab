!
!   ��������� ����� � ������ ��������� �� ������
!
    SUBROUTINE ONLY_POINT(IRESULTAT)
    USE GEOSKANGLOBALS
    USE INPUT_RADAROGRAMMA
    IMPLICIT NONE
    INTEGER*4 IRESULTAT     ! �������� ������ ��� ���������� ��������� GEOSKANGLOBALS=0
    INTEGER RET,I
    CHARACTER*50 FF1,FF2,FF3
    CALL Start()                                                                     ! �������� DLL
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
        Ster_Lenth=Ster_Lenth/ntr       ! ���������� ����� ��������
! ������ ���������� ��������� �����
        FF1='Cfreceiver.cfg'
        FF2='First_Point'
        FF3='0'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)First_Point
! ����������� ����������
            IZAGTR(1,3)=First_Point*1000            ! � �����������
            DO I=2,NTR
            IZAGTR(I,3)=IZAGTR(I-1,3)+Ster_Lenth  ! � �����������
            END DO
! ��������� ������� ���������
            CALL TABLE_() 
            DO I=1,NCHANAL_GEO                                             ! �������� ����� ���������� �������
            FLAG_CHANAL(I)=0
            END DO
 !   open(unit=100, FILE='c:/1/OUTPUT.OUT')
        
            
! ��������� ������� � ����������
            DO I=1,Ntr
            CALL GEOSCAN_INTEGRAL(IRESULTAT,I,NCHANAL(I))                                            ! ��������� ������� ���������    ! ������ �� ��������� � DLL ���������                  
         !   write(100,*)I, IZAGTR(I,7), IZAGTR(I,8)             
            END DO
! ��������� � ������
            CALL DE_RDD1()                                                      ! ������� ���������� ������            
            DEALLOCATE (FLAG_CHANAL)                                            ! ������� ���������� ������ ���������� �������
            DEALLOCATE (table)                                                  ! ������� ���������� ������� ���������
RETURN
END

    ! open(unit=100, FILE='c:/1/OUTPUT.OUT')
! write(100,*)