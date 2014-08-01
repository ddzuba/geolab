SUBROUTINE TGALFA(MM,N_CHEN_TEK,N_CHEN,OKNO,RDM,POROG1,ITEK,LM_WINDOW,N_FINISH,N_START,N,B_TAN)
INTEGER*4  ITEK
INTEGER*4  LM_WINDOW
INTEGER*4  N_FINISH,N_START
INTEGER*4  N
INTEGER*4 I,J,N_POINT,NM,MM
REAL*4 OKNO(N_CHEN,N,LM_WINDOW)
REAL*4 RDM(N)
REAL*4 POROG1
REAL*4 B_TAN
REAL*4 RAB
REAL*4 XX(MM)
REAL*4 YY(MM)
REAL*4 BB0,BB1 
!---------------------------------------------------------------------------------------------------------
! ��������� ����
IF(ITEK.LE.LM_WINDOW) THEN
	RAB=0.									! ���� �������� � ����
	DO J=N_START,N_FINISH					! ���� �� ������ � ������
		DO I=1,ITEK							! ���� �������
		OKNO(N_CHEN_TEK,J,I)=RDM(J)				! ����������� � ������� ����
		IF(ABS(OKNO(N_CHEN_TEK,J,I)).GT.RAB)&						! ���� �������� � ����
		RAB=ABS(OKNO(N_CHEN_TEK,J,I))
		END DO
	END DO
ELSE
	RAB=0.									! ���� �������� � ����
	DO J=N_START,N_FINISH					! ���� �� ������ � ������
		DO I=2,LM_WINDOW							! ���� �������
		OKNO(N_CHEN_TEK,J,I-1)=OKNO(N_CHEN_TEK,J,I)				! ����������� � ������� ����
		RAB=ABS(OKNO(N_CHEN_TEK,J,I-1))
        IF(ABS(OKNO(N_CHEN_TEK,J,I)).GT.RAB)&						! ���� �������� � ����
		RAB=ABS(OKNO(N_CHEN_TEK,J,I))
        END DO
    END DO
 	DO J=N_START,N_FINISH					            ! ���� �� ������ � ������    
		OKNO(N_CHEN_TEK,J,LM_WINDOW)=RDM(J)				! ����������� � ������� ����
        IF(ABS(OKNO(N_CHEN_TEK,J,LM_WINDOW)).GT.RAB)&						! ���� �������� � ����
		RAB=ABS(OKNO(N_CHEN_TEK,J,LM_WINDOW))
    END DO
END IF

! ���� �������� � ���� 
RAB=RAB*POROG1				
! �������� �� ���� ��������, ������� ����������
	N_POINT=0											! ���������� ����� �����
		DO J=N_START,N_FINISH										! ���������� "Y"
 		DO I=1,LM_WINDOW  									! ���������� "X"
			IF(OKNO(N_CHEN_TEK,J,I).GT.RAB) THEN			! ��������� ��������
!			NM=OKNO(N_CHEN_TEK,J,I)/RAB							! ���� ���� �����
!			DO L=1,NM									! ���� ���� �����
			N_POINT=N_POINT+1							! ���������� ����� �����
			XX(N_POINT)=I								! ���������� �����
			YY(N_POINT)=-J								! ���������� �����
!			END DO										! ���� ���� �����
		END IF
		END DO
		END DO
! ���� ����� �������� ������ ����, �� �������� ������������
	IF(N_POINT.LT.3) THEN								! ���� ��� ����������
	B_TAN=0.
    RETURN
	ELSE												! ������ ��� ��������
	CALL SQDR(N_POINT,XX,YY,BB0,BB1)					! ���������� �������� �������� �������������
B_TAN=BB1
IF(ABS(B_TAN).LT.0.0001)B_TAN=0.
    END IF

END SUBROUTINE TGALFA
!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*)