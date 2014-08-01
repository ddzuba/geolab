!---------------------------------------------------------
!  бшдекемхе цкюбмшу кхмхи яепхх
! --------------------------------------------------------------------------
SUBROUTINE MAIN_LINE(POROG1,J_LEVEL,RSUM,N)
IMPLICIT NONE
REAL*4 RSUM(N)
REAL*4 RAB,POROG1
INTEGER*4  N
INTEGER*4  J_LEVEL
INTEGER*4  J,JJ,IRAB

IRAB=0                                  ! мюопюбкемхе хглемемхъ люйяхлслнб
JJ=J_LEVEL+1                            ! ярюпне мемскебне гмювемхе
RAB=0.
	DO J=J_LEVEL+1,N-1
	IF(RSUM(J).GT.0.0001) THEN
		IF(RSUM(J).GT.RAB*(2.0-POROG1)) THEN
			RAB=RSUM(J)
            IF(IRAB.EQ.1)RSUM(JJ)=0.
            IRAB=1
            JJ=J
		ELSE
			RAB=RSUM(J)
			RSUM(J)=0.
            IRAB=0
		END IF
	END IF
	END DO
RETURN
END SUBROUTINE MAIN_LINE 
    
!---------------------------------------------------------
!  бшдекемхе цкюбмшу кхмхи яепхх
! --------------------------------------------------------------------------
SUBROUTINE MAIN_LINE1(J_LEVEL,RSUM,N)
REAL*4 RSUM(N)
REAL*4 RAB
INTEGER*4  N
INTEGER*4  J_LEVEL
INTEGER*4  J

RAB=0.
	DO J=J_LEVEL+1,N-1
	IF(RSUM(J).GT.0.0001) THEN
		IF(RSUM(J).GT.RAB) THEN
			RAB=RSUM(J)
		ELSE
			RAB=RSUM(J)
			RSUM(J)=0.
		END IF
	END IF
	END DO
RETURN
END SUBROUTINE MAIN_LINE1
