!------------------------------------------------------------------------------------
! бшвхякемхе нрпюфюрекэмни яонянамнярх
!-------------------------------------------------------------------------------------

SUBROUTINE REFR_FORPRINT()
USE PARAM_
USE PARAM_1
USE GRAFICA
USE N_
IMPLICIT NONE
!REAL*4 RAB
INTEGER*4 KLL/0/

IF(ITEK.LT.LM_WINDOW) GOTO 777                              ! сундхл, еякх нймн рпюяя ме мюапюмн
IF(REFR(N_CHEN_TEK).EQ.0.0) GOTO 777                        ! сундхл, еякх яхцмюк люк х ме ме напюаюршбюеряъ

! нопедекъел жбер рпюяяш
FLAG_FORPRINT(N_CHEN_TEK)=0                                                                                         ! юмюкхг хмрепбюкнб (1 - бкюфмн)
IF(REFR(N_CHEN_TEK).GE.REFR_MIDL(N_CHEN_TEK)*R_POSIB1)  FLAG_FORPRINT(N_CHEN_TEK)=1

! еякх оепбюъ рпюяяю, рн цнрнбхл пюанвхе йнмярюмрш
IF(ITEK.EQ.LM_WINDOW) THEN
DL_BLUE=LENGTH_HUMIDITY/(ISTEP_BETWEEN_TRASES/1000)         ! лхмхлюкэмн дносярхлн вхякн рпюяя б яхмеи онкняе
DL_WIGHT=STEP_HUMIDITY/(ISTEP_BETWEEN_TRASES/1000)          ! люйяхлюкэмн дносярхлне вхякн рпюяя б гюйпюьхбюелни аекни онкняе 

IF(FLAG_FORPRINT(N_CHEN_TEK).EQ.0) THEN
        KL_FORPRINT(N_CHEN_TEK)=4                                                                                   ! хмтнплюжхъ, врн х йюй пхянбюрэ
        STAR0(N_CHEN_TEK)=ITEK;FIN0(N_CHEN_TEK)=ITEK;GOTO 777
ELSE
        KL_FORPRINT(N_CHEN_TEK)=2                                                                                   ! хмтнплюжхъ, врн х йюй пхянбюрэ
        STAR1(N_CHEN_TEK)=ITEK;FIN1(N_CHEN_TEK)=ITEK;GOTO 777
END IF


END IF

! оняке намскемхъ яхмеи онкняш
IF(KL_FORPRINT(N_CHEN_TEK).EQ.6) THEN
STAR1(N_CHEN_TEK)=0;FIN1(N_CHEN_TEK)=0
END IF

! юмюкхг
IF (FLAG_FORPRINT(N_CHEN_TEK).EQ.1) THEN  !-----------------------                                                  ! яхмъъ онкняю
    
    IF(STAR0(N_CHEN_TEK).EQ.0.AND.FIN0(N_CHEN_TEK).EQ.0.AND.STAR1(N_CHEN_TEK).NE.0.AND.FIN1(N_CHEN_TEK).NE.0) THEN  ! опндкхбюел яхмчч онкняс
        KL_FORPRINT(N_CHEN_TEK)=1                                                                                   ! хмтнплюжхъ, врн х йюй пхянбюрэ
        FIN1(N_CHEN_TEK)=FIN1(N_CHEN_TEK)+1;GOTO 777
    END IF
    
    IF(STAR0(N_CHEN_TEK).NE.0.AND.FIN0(N_CHEN_TEK).NE.0.AND.STAR1(N_CHEN_TEK).EQ.0.AND.FIN1(N_CHEN_TEK).EQ.0) THEN  ! мювхмюел яхмчч онкняс 
        IF(FIN0(N_CHEN_TEK)-STAR0(N_CHEN_TEK)+1.LE.DL_WIGHT) THEN                                                   ! опнбепъел дкхмс аекни онкняш   
            IF(KLL.EQ.1) THEN
            STAR1(N_CHEN_TEK)=STAR1_OLD(N_CHEN_TEK);FIN1(N_CHEN_TEK)=ITEK;STAR0(N_CHEN_TEK)=0;FIN0(N_CHEN_TEK)=0        ! сдкхмъел
            KLL=0
            ELSE
            STAR1(N_CHEN_TEK)=STAR0(N_CHEN_TEK);FIN1(N_CHEN_TEK)=ITEK;STAR0(N_CHEN_TEK)=0;FIN0(N_CHEN_TEK)=0        ! сдкхмъел
            END IF
            KL_FORPRINT(N_CHEN_TEK)=2                                                                               ! хмтнплюжхъ, врн х йюй пхянбюрэ
        ELSE
            KL_FORPRINT(N_CHEN_TEK)=3                                                                               ! хмтнплюжхъ, врн х йюй пхянбюрэ
            STAR1(N_CHEN_TEK)=ITEK;FIN1(N_CHEN_TEK)=ITEK;STAR0(N_CHEN_TEK)=0;FIN0(N_CHEN_TEK)=0                     ! мювхмюел гюмнбн
        END IF
            GOTO 777
    END IF
ELSE                                        !---------------------                                                  ! аекюъ онкняю
    IF(STAR0(N_CHEN_TEK).NE.0.AND.FIN0(N_CHEN_TEK).NE.0.AND.STAR1(N_CHEN_TEK).EQ.0.AND.FIN1(N_CHEN_TEK).EQ.0) THEN  ! опндкхбюел аексч онкняс
        KL_FORPRINT(N_CHEN_TEK)=4                                                                                   ! хмтнплюжхъ, врн х йюй пхянбюрэ
        FIN0(N_CHEN_TEK)=FIN0(N_CHEN_TEK)+1;GOTO 777
    END IF
   
    IF(STAR0(N_CHEN_TEK).EQ.0.AND.FIN0(N_CHEN_TEK).EQ.0.AND.STAR1(N_CHEN_TEK).NE.0.AND.FIN1(N_CHEN_TEK).NE.0) THEN  ! мювхмюел аексч онкняс 
        IF(FIN1(N_CHEN_TEK)-STAR1(N_CHEN_TEK)+1.GE.DL_BLUE) THEN                                                    ! опнбепъел дкхмс яхмеи онкняш                      
            KL_FORPRINT(N_CHEN_TEK)=5                                                                               ! хмтнплюжхъ, врн х йюй пхянбюрэ
            STAR0(N_CHEN_TEK)=ITEK;FIN0(N_CHEN_TEK)=ITEK                                                            ! мювхмюел аексч, ме сахбюъ яхмчч
            STAR1(N_CHEN_TEK)=0;FIN1(N_CHEN_TEK)=0
        ELSE
            KL_FORPRINT(N_CHEN_TEK)=6                                                                               ! хмтнплюжхъ, врн х йюй пхянбюрэ    
            STAR0(N_CHEN_TEK)=ITEK;FIN0(N_CHEN_TEK)=ITEK                                                            ! мювхмюел аексч, мн сахбюел яхмчч    
            STAR1_OLD(N_CHEN_TEK)=STAR1(N_CHEN_TEK)                     ! гюонлхмюел дкхмс яхмеи йнпнрйни онкняш   
            KLL=1
        END IF
            GOTO 777
    END IF
   
END IF
777 CONTINUE

RETURN
END 

!open(unit=100, FILE='D:/OUTPUT.OUT')
!IF(N_CHEN_TEK.EQ.1)write(100,*)itek