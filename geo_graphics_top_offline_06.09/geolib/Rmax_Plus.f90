SUBROUTINE RMAX_PLUS(RDM,RDM1,N,POROG,J_LEVEL,RABMAX) ! ���������� ������ ������������� ��������� ���� ���������� ��������

INTEGER*4  N,J_LEVEL 
REAL*4 RDM(N),RDM1(N),RABMAX,POROG
INTEGER*4  I

! ���������� ������ ������������� ��������
DO I=1,N
IF(RDM(I).LT.0.0001) RDM(I)=0.
END DO	
! ��������� ���������� ��������
RABMAX=0.					
DO I=1,N					!
IF(RDM(I).GT.RABMAX) THEN	! ����� ������������� �������������� ��������
RABMAX=RDM(I)				! ������������ ��������
END IF						!
END DO

DO I=1,N
RDM1(I)=0.
END DO
	
RABMAX=RABMAX*POROG			! ��������� ���������� ���� 
DO I=J_LEVEL-1,N-1
IF(RDM(I).GT.RABMAX)THEN						! ���������� ����
	IF(RDM(I-1).LE.RDM(I).AND.RDM(I+1).LT.RDM(I)) THEN
	RDM1(I)=RDM(I)
	ELSE
	RDM1(I)=0.
	END IF
ELSE
RDM1(I)=0.
END IF
END DO

DO I=1,N
RDM(I)=RDM1(I)
END DO

RETURN
END SUBROUTINE RMAX_PLUS

    
    SUBROUTINE Gilbert(N,RDM,RDM1) ! �������������� ���������
	IMPLICIT NONE
	REAL*4 RDM(N),RDM1(N)
    REAL*4 RAB
	INTEGER*4 N,J,K
! -------------------------------------------------------------
! ������ ������������ �����
! �������������� ���������
		DO J=1,N
			RAB=0.D0
			DO K=1,N
			IF(J.NE.K) RAB=RAB+RDM(K)/3.1416D0/(J-K)
			END DO
			RDM1(J)=RAB
		END DO
!------------------------------------------------------------
! ������ ������ ��������
	DO J=1,N
	RDM(J)=SQRT(RDM(J)**2+RDM1(J)**2)
	END DO



777	RETURN
      END    
!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*) 