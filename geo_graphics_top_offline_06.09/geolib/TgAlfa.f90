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
! гюонкмъел нймн
IF(ITEK.LE.LM_WINDOW) THEN
	RAB=0.									! хыел люйяхлсл б нйме
	DO J=N_START,N_FINISH					! жхйк он рнвйюл б рпюяяе
		DO I=1,ITEK							! жхйк рпюяяюл
		OKNO(N_CHEN_TEK,J,I)=RDM(J)				! оепелеыемхе б пюанвхи тюик
		IF(ABS(OKNO(N_CHEN_TEK,J,I)).GT.RAB)&						! хыел люйяхлсл б нйме
		RAB=ABS(OKNO(N_CHEN_TEK,J,I))
		END DO
	END DO
ELSE
	RAB=0.									! хыел люйяхлсл б нйме
	DO J=N_START,N_FINISH					! жхйк он рнвйюл б рпюяяе
		DO I=2,LM_WINDOW							! жхйк рпюяяюл
		OKNO(N_CHEN_TEK,J,I-1)=OKNO(N_CHEN_TEK,J,I)				! оепелеыемхе б пюанвхи тюик
		RAB=ABS(OKNO(N_CHEN_TEK,J,I-1))
        IF(ABS(OKNO(N_CHEN_TEK,J,I)).GT.RAB)&						! хыел люйяхлсл б нйме
		RAB=ABS(OKNO(N_CHEN_TEK,J,I))
        END DO
    END DO
 	DO J=N_START,N_FINISH					            ! жхйк он рнвйюл б рпюяяе    
		OKNO(N_CHEN_TEK,J,LM_WINDOW)=RDM(J)				! оепелеыемхе б пюанвхи тюик
        IF(ABS(OKNO(N_CHEN_TEK,J,LM_WINDOW)).GT.RAB)&						! хыел люйяхлсл б нйме
		RAB=ABS(OKNO(N_CHEN_TEK,J,LM_WINDOW))
    END DO
END IF

! хыел люйяхлсл б нйме 
RAB=RAB*POROG1				
! бшахпюел хг нймю гмювемхъ, анкэьхе онпнцнбнцн
	N_POINT=0											! мюйнокемхе вхякю рнвей
		DO J=N_START,N_FINISH										! йннпдхмюрю "Y"
 		DO I=1,LM_WINDOW  									! йннпдхмюрю "X"
			IF(OKNO(N_CHEN_TEK,J,I).GT.RAB) THEN			! мемскебне гмювемхе
!			NM=OKNO(N_CHEN_TEK,J,I)/RAB							! свер беяю рнвйх
!			DO L=1,NM									! свер беяю рнвйх
			N_POINT=N_POINT+1							! мюйнокемхе вхякю рнвей
			XX(N_POINT)=I								! гюонлхмюел рнвйх
			YY(N_POINT)=-J								! гюонлхмюел рнвйх
!			END DO										! свер беяю рнвйх
		END IF
		END DO
		END DO
! еякх рюйху гмювемхи анкэье рпеу, рн кхмеимюъ хмрепонкъжхъ
	IF(N_POINT.LT.3) THEN								! нймн аег люйяхлслнб
	B_TAN=0.
    RETURN
	ELSE												! сундхл аег пюявернб
	CALL SQDR(N_POINT,XX,YY,BB0,BB1)					! бшвхякемхе йнмярюмр кхмеимни юоопнйяхлюжхх
B_TAN=BB1
IF(ABS(B_TAN).LT.0.0001)B_TAN=0.
    END IF

END SUBROUTINE TGALFA
!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*)