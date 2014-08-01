SUBROUTINE TAN_POZITION(N_CHEN_TEK)
USE PARAM_
USE N_
IMPLICIT NONE
INTEGER*4  N_CHEN_TEK
INTEGER*4  I

IF(ITEK.GE.N_TAN) THEN
	DO I=1,N_TAN-1						    	! ��������� � ������						
	TANG(N_CHEN_TEK,I)=TANG(N_CHEN_TEK,I+1)
	END DO
	TANG(N_CHEN_TEK,N_TAN)=ABS(B_TAN(N_CHEN_TEK))
	TAN_MIDL(N_CHEN_TEK)=0.
	DO I=1,N_TAN
	TAN_MIDL(N_CHEN_TEK)=TAN_MIDL(N_CHEN_TEK)+TANG(N_CHEN_TEK,I)
	END DO
	TAN_MIDL(N_CHEN_TEK)=TAN_MIDL(N_CHEN_TEK)/N_TAN   ! ���������
ELSE
	TANG(N_CHEN_TEK,ITEK)=ABS(B_TAN(N_CHEN_TEK))							! ��������� � ������
	TAN_MIDL(N_CHEN_TEK)=0.
	DO I=1,ITEK
	TAN_MIDL(N_CHEN_TEK)=TAN_MIDL(N_CHEN_TEK)+TANG(N_CHEN_TEK,I)
	END DO
	TAN_MIDL(N_CHEN_TEK)=TAN_MIDL(N_CHEN_TEK)/ITEK		! ���������
END IF
!IF(TAN_MIDL(N_CHEN_TEK).EQ.0)TAN_MIDL(N_CHEN_TEK)=1.
RETURN
END 