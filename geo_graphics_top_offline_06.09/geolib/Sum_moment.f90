!--------------------------------------------------------------------
! ондявер ясллюпмнцн лнлемрю мюйкнмю мю охйере
!--------------------------------------------------------------------
SUBROUTINE SUM_MOMENT(N_CHEN_TEK)
USE PARAM_
USE N_
IMPLICIT NONE
INTEGER*4  N_CHEN_TEK
INTEGER*4  I
REAL*4 RAB

RAB=RMOM_MIDL(N_CHEN_TEK)
RMOM(N_CHEN_TEK)=0.D0
DO I=N_START,N_FINISH
IF(RDM2(I,N_CHEN_TEK).GT.0.001) THEN
RMOM(N_CHEN_TEK)=RMOM(N_CHEN_TEK)+(I-J_LEVEL)**2		! дкъ онбшьемхъ бйкюдю опняебьху якнеб
END IF
END DO

RMOM_MIDL(N_CHEN_TEK)= (RMOM_MIDL(N_CHEN_TEK)*(ITEK-1)+RMOM(N_CHEN_TEK))/ITEK

IF(RMOM(N_CHEN_TEK).GT.RMOM_MAX(N_CHEN_TEK))THEN
RMOM_MAX(N_CHEN_TEK)=RMOM(N_CHEN_TEK)
IF(RMOM_MIDL(N_CHEN_TEK).LT.RAB)RMOM_MAX(N_CHEN_TEK)=RMOM_MAX(N_CHEN_TEK)-RAB+RMOM_MIDL(N_CHEN_TEK) ! ямхфюеляъ, еякх ямхфюеряъ япедмее
END IF

!open(unit=100, FILE='D:/OUTPUT.OUT')
!IF(N_CHEN_TEK.EQ.1)write(100,*)RMOM(N_CHEN_TEK),RMOM_MIDL(N_CHEN_TEK),RMOM_MAX(N_CHEN_TEK)
RETURN
END 

