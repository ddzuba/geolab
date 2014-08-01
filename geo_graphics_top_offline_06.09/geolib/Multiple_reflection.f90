!---------------------------------------------------------
!  сярпюмемхе йпюрмшу нрпюфемхи
! --------------------------------------------------------------------------
SUBROUTINE MULTIPLE_REFLECTION(J_LEVEL,RSUM,N)
IMPLICIT NONE
INTEGER*4  N
REAL*4 RSUM(N)
REAL*4 RAB
INTEGER*4  J_LEVEL
INTEGER*4  J,J1,II,K

RAB=0.
DO J=J_LEVEL+1,N-1
	IF(RSUM(J).GT.0.0001) THEN
		II=J-J_LEVEL			! бшвхякъел ьюц	
		DO J1=J+II, N-2, II
			DO K=J1-1,J1+1		! опнбепъел рпх рнвйх
			IF(RSUM(K).LT.RSUM(J)*0.05)THEN ! йнмярюмрю ятнплхпнбюмю дкъ дхщкейрпхвеяйху онярнъммшу 1,4,6.5,9
            RSUM(K)=0.
            ELSE
            RSUM(K)= RSUM(K)- RSUM(J)*0.05
            END IF
			END DO
		END DO 
	END IF
END DO

RETURN
END SUBROUTINE MULTIPLE_REFLECTION