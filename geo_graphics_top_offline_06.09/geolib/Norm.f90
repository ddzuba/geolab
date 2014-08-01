!---------------------------------------------------------------
! ÕŒ–Ã»–”≈Ã —»√Õ¿À
!---------------------------------------------------------------
SUBROUTINE NORM(RSUM,N,II)
REAL*4 RSUM(N)
REAL*4 RAB
INTEGER*4  N
INTEGER*4  J,II

IF(II.LT.1.OR.II.GT.N) GOTO 777
RAB=RSUM(II)

IF(RAB.LT.1.D-20)GOTO 777
RAB=10000./RAB
DO J=1,N
RSUM(J)=RAB*RSUM(J)
END DO
777 RETURN
END SUBROUTINE NORM