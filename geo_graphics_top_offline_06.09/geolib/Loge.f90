

SUBROUTINE LOGE(IANTENNA,ITEK,LM_WINDOW,N_FINISH,N_START,MM_WINDOW,&  ! ���������� ������� ���� 
RABMAX,POROG1,BUFF_MAX,SUM_B1,B_TAN)								  ! ������� ���������� �����

INTEGER*4  IANTENNA,ITEK,LM_WINDOW,N_FINISH,N_START,MM_WINDOW
REAL*4 RABMAX,POROG1,BUFF_MAX(511,10000)
INTEGER*4  N_STEP,N_POINT,I,L1,L2,K,J,NM
REAL*4 RAB,OKNO(200,200)
REAL*4 XX(10000),YY(10000),BB0,BB1

IF(IANTENNA.GE.100.AND.IANTENNA.LT.1000)THEN		! �������������� �������
IF(ITEK.GE.LM_WINDOW) THEN							! ������ �������� ����� ������ ����� � ���������� ����
N_STEP=(N_FINISH-N_START)/MM_WINDOW					! ����� ����������� ���� ����� ������
DO I=1,N_STEP										! ����������� ����� ������
L1=N_START+MM_WINDOW*(I-1)
L2=L1+MM_WINDOW-1
	RAB=0.											! ���� �������� � ����
	
	DO J=L1,L2										! ���� �� ������ � ������
		DO K=1,LM_WINDOW							! ���� �������
		OKNO(J-L1+1,K)=BUFF_MAX(J,ITEK-LM_WINDOW+K)	! ����������� � ������� ����
		IF(ABS(OKNO(J-L1+1,K)).GT.RAB)&				! ���� �������� � ����
		RAB=ABS(OKNO(J-L1+1,K))
		END DO
	END DO

	IF(RAB.LT.RABMAX) THEN								! ���� ��� ����������
	GOTO 20	
	END IF						

	N_POINT=0
 		DO K=1,LM_WINDOW  								! ���������� "X"
		DO J=1,MM_WINDOW								! ���������� "Y"
		IF(OKNO(J,K).GT.RAB*POROG1) THEN				! ��������� ��������
		NM=OKNO(J,K)/RABMAX
!			DO L=1,NM
			N_POINT=N_POINT+1
			XX(N_POINT)=K
			YY(N_POINT)=-J
!			END DO
		END IF
		END DO
		END DO

	IF(N_POINT.LT.3) THEN								! ���� ��� ����������
	GOTO 20	
	END IF

	CALL SQDR(N_POINT,XX,YY,BB0,BB1)
	IF(ITEK.EQ.ITEK_OLD) THEN
		
		IF(ABS(BB1).GT.ABS(BB1_OLD)) THEN
		BB1_OLD=BB1
		I_OLD=I
		END IF

	ELSE
		IF(SUM_B1.EQ.0..OR.SUM_B1*BB1_OLD.GT.0.) THEN
		B_TAN=BB1_OLD
		SUM_B1=SUM_B1+BB1_OLD/LM_WINDOW
		ELSE
		SUM_B1=0.
		END IF
	I_OLD=0
	ITEK_OLD=ITEK
	BB1_OLD=BB1
	END IF



20     CONTINUE

END DO										
ITEK_OLD=ITEK	 
END IF
END IF

END SUBROUTINE LOGE