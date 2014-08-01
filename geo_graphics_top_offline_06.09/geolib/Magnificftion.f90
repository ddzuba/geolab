SUBROUTINE MAGNIFICATION(RDM,N,J_LEVEL,RMULTIPLE)
IMPLICIT NONE 
INTEGER*4 N,J_LEVEL
REAL*4 RDM(N)
REAL*4 RMULTIPLE						
INTEGER*4 I
!----------------------------------------------
! ”—»À»¬¿≈Ã —»√Õ¿À
DO I=J_LEVEL,N
    RDM(I)=RDM(I)*2.7**((I-J_LEVEL)*RMULTIPLE)
END DO
RETURN
    END SUBROUTINE MAGNIFICATION
!--------------------------------------------------------------
!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*) RMULTIPLE