SUBROUTINE LEVEL(RDM,RDM1,J_LEVEL,II,N)
IMPLICIT NONE 
INTEGER*4   J_LEVEL,II,N
REAL*4 RDM(N),RDM1(N)
INTEGER*4  I,JJ2

! ���� II ��� ���������, �� ������
IF(II.LT.1.OR.II.GT.N) THEN
    DO I=1,N			! ��������� ���������
    RDM1(I)=RDM(I)
    END DO    
GOTO 777
END IF

! �������� � ������ ������ - J_LEWEL 
! ���������� ��������� ������ - II
! ����� ����� � ������ - N
DO I=1,N			! ��������� ���������
RDM1(I)=0.
END DO
IF (J_LEVEL-II.GT.0) THEN
JJ2=N-J_LEVEL+II
ELSE
JJ2=N+J_LEVEL-II
END IF

DO  I=J_LEVEL,JJ2				
RDM1(I)=RDM(II+I-J_LEVEL) 
END DO

777 RETURN
END SUBROUTINE LEVEL