!------------------------------------------------------------------------------------
! бшвхякемхе нрпюфюрекэмни яонянамнярх
!-------------------------------------------------------------------------------------
SUBROUTINE REFR_POSIBILITY(N_CHEN_TEK)
USE PARAM_
USE N_
IMPLICIT NONE
INTEGER*4  N_CHEN_TEK
REAL*4 RAB
INTEGER*4  I
! пюяявхршбел нрпюфюрекэмсч яонянамнярэ
RAB=0.
DO I=N_START,N_FINISH
RAB=RAB+ABS(RDM(I))
END DO
! янупюмъел  б астепе дкъ сяпедмемхъ
    IF(ITEK.LE.N_MIDL_U)THEN
	    REFRACTION(N_CHEN_TEK,ITEK)=RAB
        REFR_MIDL(N_CHEN_TEK)=(REFR_MIDL(N_CHEN_TEK)*(ITEK-1)+RAB)/ITEK                                      ! пюяявхршбюел япедмее гмювемхе нрпюфюрекэмни яонянамнярх мю хмрепбюке             
    ELSE
        I=ITEK-ITEK/N_MIDL_U*N_MIDL_U+1                                                                      ! жхйкхвеяйюъ гюохяэ
        REFR_MIDL(N_CHEN_TEK)=(REFR_MIDL(N_CHEN_TEK)*N_MIDL_U+RAB-REFRACTION(N_CHEN_TEK,I))/N_MIDL_U         ! пюяявхршбюел япедмее гмювемхе нрпюфюрекэмни яонянамнярх мю хмрепбюке      
		REFRACTION(N_CHEN_TEK,I)=RAB
    END IF
! гюонлхмюел нрпюфюрекэмсч яонянамнярэ
REFR(N_CHEN_TEK)=RAB
! бшвхякъел люйяхлюкэмсч нрпюфюрекэмсч яонянамнярэ мю хмрепбюке
IF(RAB.GT.REFR_MAX(N_CHEN_TEK)) THEN
    REFR_MAX(N_CHEN_TEK)=RAB
ELSE
    RAB=REFRACTION(N_CHEN_TEK,1)
    DO I=2,N_MIDL_U
    IF(RAB.LT.REFRACTION(N_CHEN_TEK,I))RAB=REFRACTION(N_CHEN_TEK,I)
    END DO
    REFR_MAX(N_CHEN_TEK)=RAB
END IF

!open(unit=100, FILE='D:/OUTPUT.OUT')
!IF(N_CHEN_TEK.EQ.1)write(100,*)itek
RETURN
END 


