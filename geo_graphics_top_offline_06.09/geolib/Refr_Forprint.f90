!------------------------------------------------------------------------------------
! ���������� ������������� �����������
!-------------------------------------------------------------------------------------

SUBROUTINE REFR_FORPRINT()
USE PARAM_
USE PARAM_1
USE GRAFICA
USE N_
IMPLICIT NONE
!REAL*4 RAB
INTEGER*4 KLL/0/

IF(ITEK.LT.LM_WINDOW) GOTO 777                              ! ������, ���� ���� ����� �� �������
IF(REFR(N_CHEN_TEK).EQ.0.0) GOTO 777                        ! ������, ���� ������ ��� � �� �� ��������������

! ���������� ���� ������
FLAG_FORPRINT(N_CHEN_TEK)=0                                                                                         ! ������ ���������� (1 - ������)
IF(REFR(N_CHEN_TEK).GE.REFR_MIDL(N_CHEN_TEK)*R_POSIB1)  FLAG_FORPRINT(N_CHEN_TEK)=1

! ���� ������ ������, �� ������� ������� ���������
IF(ITEK.EQ.LM_WINDOW) THEN
DL_BLUE=LENGTH_HUMIDITY/(ISTEP_BETWEEN_TRASES/1000)         ! ���������� ��������� ����� ����� � ����� ������
DL_WIGHT=STEP_HUMIDITY/(ISTEP_BETWEEN_TRASES/1000)          ! ����������� ���������� ����� ����� � ������������� ����� ������ 

IF(FLAG_FORPRINT(N_CHEN_TEK).EQ.0) THEN
        KL_FORPRINT(N_CHEN_TEK)=4                                                                                   ! ����������, ��� � ��� ��������
        STAR0(N_CHEN_TEK)=ITEK;FIN0(N_CHEN_TEK)=ITEK;GOTO 777
ELSE
        KL_FORPRINT(N_CHEN_TEK)=2                                                                                   ! ����������, ��� � ��� ��������
        STAR1(N_CHEN_TEK)=ITEK;FIN1(N_CHEN_TEK)=ITEK;GOTO 777
END IF


END IF

! ����� ��������� ����� ������
IF(KL_FORPRINT(N_CHEN_TEK).EQ.6) THEN
STAR1(N_CHEN_TEK)=0;FIN1(N_CHEN_TEK)=0
END IF

! ������
IF (FLAG_FORPRINT(N_CHEN_TEK).EQ.1) THEN  !-----------------------                                                  ! ����� ������
    
    IF(STAR0(N_CHEN_TEK).EQ.0.AND.FIN0(N_CHEN_TEK).EQ.0.AND.STAR1(N_CHEN_TEK).NE.0.AND.FIN1(N_CHEN_TEK).NE.0) THEN  ! ���������� ����� ������
        KL_FORPRINT(N_CHEN_TEK)=1                                                                                   ! ����������, ��� � ��� ��������
        FIN1(N_CHEN_TEK)=FIN1(N_CHEN_TEK)+1;GOTO 777
    END IF
    
    IF(STAR0(N_CHEN_TEK).NE.0.AND.FIN0(N_CHEN_TEK).NE.0.AND.STAR1(N_CHEN_TEK).EQ.0.AND.FIN1(N_CHEN_TEK).EQ.0) THEN  ! �������� ����� ������ 
        IF(FIN0(N_CHEN_TEK)-STAR0(N_CHEN_TEK)+1.LE.DL_WIGHT) THEN                                                   ! ��������� ����� ����� ������   
            IF(KLL.EQ.1) THEN
            STAR1(N_CHEN_TEK)=STAR1_OLD(N_CHEN_TEK);FIN1(N_CHEN_TEK)=ITEK;STAR0(N_CHEN_TEK)=0;FIN0(N_CHEN_TEK)=0        ! ��������
            KLL=0
            ELSE
            STAR1(N_CHEN_TEK)=STAR0(N_CHEN_TEK);FIN1(N_CHEN_TEK)=ITEK;STAR0(N_CHEN_TEK)=0;FIN0(N_CHEN_TEK)=0        ! ��������
            END IF
            KL_FORPRINT(N_CHEN_TEK)=2                                                                               ! ����������, ��� � ��� ��������
        ELSE
            KL_FORPRINT(N_CHEN_TEK)=3                                                                               ! ����������, ��� � ��� ��������
            STAR1(N_CHEN_TEK)=ITEK;FIN1(N_CHEN_TEK)=ITEK;STAR0(N_CHEN_TEK)=0;FIN0(N_CHEN_TEK)=0                     ! �������� ������
        END IF
            GOTO 777
    END IF
ELSE                                        !---------------------                                                  ! ����� ������
    IF(STAR0(N_CHEN_TEK).NE.0.AND.FIN0(N_CHEN_TEK).NE.0.AND.STAR1(N_CHEN_TEK).EQ.0.AND.FIN1(N_CHEN_TEK).EQ.0) THEN  ! ���������� ����� ������
        KL_FORPRINT(N_CHEN_TEK)=4                                                                                   ! ����������, ��� � ��� ��������
        FIN0(N_CHEN_TEK)=FIN0(N_CHEN_TEK)+1;GOTO 777
    END IF
   
    IF(STAR0(N_CHEN_TEK).EQ.0.AND.FIN0(N_CHEN_TEK).EQ.0.AND.STAR1(N_CHEN_TEK).NE.0.AND.FIN1(N_CHEN_TEK).NE.0) THEN  ! �������� ����� ������ 
        IF(FIN1(N_CHEN_TEK)-STAR1(N_CHEN_TEK)+1.GE.DL_BLUE) THEN                                                    ! ��������� ����� ����� ������                      
            KL_FORPRINT(N_CHEN_TEK)=5                                                                               ! ����������, ��� � ��� ��������
            STAR0(N_CHEN_TEK)=ITEK;FIN0(N_CHEN_TEK)=ITEK                                                            ! �������� �����, �� ������ �����
            STAR1(N_CHEN_TEK)=0;FIN1(N_CHEN_TEK)=0
        ELSE
            KL_FORPRINT(N_CHEN_TEK)=6                                                                               ! ����������, ��� � ��� ��������    
            STAR0(N_CHEN_TEK)=ITEK;FIN0(N_CHEN_TEK)=ITEK                                                            ! �������� �����, �� ������� �����    
            STAR1_OLD(N_CHEN_TEK)=STAR1(N_CHEN_TEK)                     ! ���������� ����� ����� �������� ������   
            KLL=1
        END IF
            GOTO 777
    END IF
   
END IF
777 CONTINUE

RETURN
END 

!open(unit=100, FILE='D:/OUTPUT.OUT')
!IF(N_CHEN_TEK.EQ.1)write(100,*)itek