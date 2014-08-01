! нопедекъел юаянкчрмши люйяхлсл рейсыеи рпюяяш х ее ясллс юаянкчрмшу гмювемхи
! хяонкэгселше оюпюлерпш - POROG1	
SUBROUTINE MAXIMUM(N,RDM,J_LEVEL,ITEK,POROG1,II)
INTEGER*4  N,II,J_LEVEL,ITEK
REAL*4 RDM(511),POROG1
INTEGER*4  II_OLD
REAL*4 RAB, RABMAX,POROG_TRASSA
RAB=0.						! люйяхлюкэмне гмювемхе
DO I=1,N					!
IF(RDM(I).GT.RAB) THEN		! онхяй люйяхлюкэмнцн онкнфхрекэмнцн гмювемхъ
RAB=RDM(I)					!
END IF						!
END DO						!
POROG_TRASSA=RAB*POROG1		! нряейючыее лхмхлюкэмне гмювемхе 
II=0
RABMAX=0.
DO I=1,N
	IF(RDM(I).GT.POROG_TRASSA) THEN
		IF(RDM(I).GT.RDM(I-1).AND.RDM(I).GT.RDM(I+1)) THEN
		II=I
		RABMAX=RDM(I)
		GOTO 100
		END IF
	END IF
END DO 
100 &
IF(ITEK.LE.2)II_OLD=II				! гюонлхмюел люйяхлсл
IF (ITEK.GT.2) THEN					! мювхмюъ ян брнпнцн гмювемхъ
	IF(IABS(II-II_OLD).GT.5) THEN	! якедхл врнаш люйяхлсл ме япшбюкяъ
	II=II_OLD
	ELSE
	II_OLD=II
	END IF
END IF
END SUBROUTINE MAXIMUM