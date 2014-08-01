SUBROUTINE HUMIDITY_CONTAMINATION(II)
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE FReciverGlobals

IMPLICIT NONE
REAL*4 E_LEVEL
INTEGER*4 I,II						! ����� ��������� � ������		

! **************************************************************************************************

E_LEVEL=RDM(J_LEVEL) ! ���������� �������� �� �������

!open(unit=100, FILE='c:/1/OUTPUT.OUT')
!if(N_CHEN_TEK.EQ.1)write(100,*)ITEK,E_LEVEL

! ��������� ������� ������������� ���� � ������� ���������� �����������
IF(II.GE.1.AND.II.LE.N) THEN ! �������� ������ � ��������� ��������� ������� �����
! ���������� ������� �� ��������    
!    E_MIDL(N_CHEN_TEK)=(E_MIDL(N_CHEN_TEK)*(ITEK-1)+E_LEVEL)/ITEK
    E_MIDL(N_CHEN_TEK)=(E_MIDL(N_CHEN_TEK)*(ITEK-1)+RDM(J_LEVEL))/ITEK 
! ���������� ���������� �����������
    INDEX_REFRACTION(N_CHEN_TEK)=(E_LEVEL*(INDEX_REFRACTION_0(N_CHEN_TEK)-1)+E_MIDL(N_CHEN_TEK)*(INDEX_REFRACTION_0(N_CHEN_TEK)+1))/ &
    (E_LEVEL*(1-INDEX_REFRACTION_0(N_CHEN_TEK))+E_MIDL(N_CHEN_TEK)*(INDEX_REFRACTION_0(N_CHEN_TEK)+1))
ELSE
    IF(INDEX_REFRACTION(N_CHEN_TEK).LT.1)INDEX_REFRACTION(N_CHEN_TEK)=SQRT(PREL)
    E_LEVEL=E_MIDL(N_CHEN_TEK)
END IF
! ����������� �� ������ ���������� �����������
IF(ABS(INDEX_REFRACTION(N_CHEN_TEK)-INDEX_REFRACTION_0(N_CHEN_TEK)).GT.INDEX_REFRACTION_0(N_CHEN_TEK)*LEVEL_PREL)INDEX_REFRACTION(N_CHEN_TEK)=INDEX_REFRACTION_0(N_CHEN_TEK)
! ����������� �������� �������� ���������� �����������
INDEX_REFRACTION_0(N_CHEN_TEK)=(INDEX_REFRACTION_0(N_CHEN_TEK)*(ITEK-1)+INDEX_REFRACTION(N_CHEN_TEK))/ITEK 

!if(N_CHEN_TEK.EQ.1)write(100,*) E_MIDL(N_CHEN_TEK)
!if(N_CHEN_TEK.EQ.1)write(100,*) INDEX_REFRACTION(N_CHEN_TEK),INDEX_REFRACTION_0(N_CHEN_TEK)

!--------------------------------------------------------------------------------------------
!��������� ���������� ����������� � ������
IF(ITEK.LE.LM_WINDOW)THEN
BUFF_INDEX_REFRACTION(N_CHEN_TEK,ITEK)=INDEX_REFRACTION(N_CHEN_TEK)
ELSE
I=ITEK-ITEK/LM_WINDOW*LM_WINDOW+1 ! ����������� ������
BUFF_INDEX_REFRACTION(N_CHEN_TEK,I)=INDEX_REFRACTION(N_CHEN_TEK)
END IF
!--------------------------------------------------------------------------------------------
! ������� �������
INDEX_REFRACTION(N_CHEN_TEK)=0.0
IF(ITEK.LE.LM_WINDOW)THEN
    DO I=1,ITEK
    INDEX_REFRACTION(N_CHEN_TEK)=INDEX_REFRACTION(N_CHEN_TEK)+BUFF_INDEX_REFRACTION(N_CHEN_TEK,I)
    END DO
    INDEX_REFRACTION(N_CHEN_TEK)=INDEX_REFRACTION(N_CHEN_TEK)/ITEK
ELSE
    DO I=1,LM_WINDOW
    INDEX_REFRACTION(N_CHEN_TEK)=INDEX_REFRACTION(N_CHEN_TEK)+BUFF_INDEX_REFRACTION(N_CHEN_TEK,I)
    END DO
INDEX_REFRACTION(N_CHEN_TEK)=INDEX_REFRACTION(N_CHEN_TEK)/LM_WINDOW
END IF
IF(INDEX_REFRACTION(N_CHEN_TEK).LT.1)INDEX_REFRACTION(N_CHEN_TEK)=SQRT(PREL)
!**********************************************************************************
! ��������� ���������� �������
INDEX_REFRACTION(N_CHEN_TEK)=SQRT(PREL)
INDEX_REFRACTION_0(N_CHEN_TEK)=SQRT(PREL) 
!--------------------------------------------------------------------------------------------
!open(unit=100, FILE='D:/OUTPUT.OUT')
!IF(N_CHEN_TEK.EQ.1)write(100,*) itek,RDM(J_LEVEL)

! **************************************************************************************************
RETURN
END SUBROUTINE HUMIDITY_CONTAMINATION