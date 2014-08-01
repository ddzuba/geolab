SUBROUTINE RMAX(RDM,POROG1,N,II)
! нопедекъел юаянкчрмши люйяхлсл рейсыеи рпюяяш 
! хяонкэгселше оюпюлерпш - POROG1
IMPLICIT NONE
INTEGER*4  ITEK,N,II
REAL*4 RDM(N),POROG1
REAL*4 RAB,POROG_TRASSA,RABMAX
INTEGER*4  I,I1,J1,J2 
 
! вхякн рнвей б рпюяяе          - N

RAB=0.						! люйяхлюкэмне гмювемхе
DO I=1,N					!
IF(RDM(I).GT.RAB) THEN		! онхяй люйяхлюкэмнцн онкнфхрекэмнцн гмювемхъ
RAB=RDM(I)					!
END IF						!
END DO
						!
POROG_TRASSA=RAB*POROG1		! нряейючыее лхмхлюкэмне гмювемхе 
J1=1
J2=N
DO I=1,N
    IF(RDM(I).GT.POROG_TRASSA) THEN
    J1=I
        DO I1=I,N
            IF(RDM(I1).LT.POROG_TRASSA)THEN
            J2=I1
            GOTO 555
            END IF 
        END DO   
    END IF 
END DO
555 II=0
RAB=0.
DO I=J1,J2-1
	IF(RDM(I).GT.RAB) THEN
	II=I
	RAB=RDM(I)
	END IF
END DO
777 RETURN
END 

