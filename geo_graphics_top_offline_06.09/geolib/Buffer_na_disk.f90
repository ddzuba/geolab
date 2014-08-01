    SUBROUTINE BUFFER_NA_DISK()
	use FReciverGlobals
	USE PARAM_1
	USE RAILWAY
	USE GRAFICA
	USE N_
    
    implicit none 
    integer*4 II, OnClose
    
! оепеохяшбюел мю дхяй опх гюонкмемхх астепнб
IF(i_GRAF.EQ.N_GRAF) THEN		! нябнанфдюел астеп мю дхяй
	II=  OnClose()	! астспхгсел гюохях ! гхосел х гюйпшбюел оюойх
	I_GRAF=0		! намскъел яверйх б опнжедспе FIRST
	DO N_CHEN_TEK=1,N_CHEN
	N_WRITE_VLAG(N_CHEN_TEK)=0						! мнлеп гюохях б тюик VLAG
	N_WRITE_UGLUB(N_CHEN_TEK)=0						! мнлеп гюохях б тюик UGLUB
    N_WRITE_TOLSH(N_CHEN_TEK)=0						! мнлеп гюохях б тюик TOLSH
    END DO
! тнплхпсел хлемю оюонй
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL FILE_WRITE_SECOND()									! гюдюмхе тюикю гюохях рейярнбнцн
		CALL FILE_WRITE_CSV_SECOND() 								! тюикю х тюикю пхясмйнб
END IF		
DO N_CHEN_TEK=1,N_CHEN	
	IF(N_WRITE_VLAG(N_CHEN_TEK).EQ.N_VLAG) THEN		! нябнанфдюел астеп мю дхяй
		II=  OnClose()	! астспхгсел гюохях ! гхосел х гюйпшбюел оюойх
		I_GRAF=0		! намскъел яверйх б опнжедспе FIRST
		DO II=1,N_CHEN
		N_WRITE_VLAG(II)=0						! мнлеп гюохях б тюик VLAG
		N_WRITE_UGLUB(II)=0						! мнлеп гюохях б тюик UGLUB
        N_WRITE_TOLSH(II)=0						! мнлеп гюохях б тюик TOLSH
		END DO
! тнплхпсел хлемю оюонй
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL FILE_WRITE_SECOND()									! гюдюмхе тюикю гюохях рейярнбнцн
		CALL FILE_WRITE_CSV_SECOND() 								! тюикю х тюикю пхясмйнб
	END IF
END DO
DO N_CHEN_TEK=1,N_CHEN	
	IF(N_WRITE_UGLUB(N_CHEN_TEK).EQ.N_UGLUB) THEN		! нябнанфдюел астеп мю дхяй
		II=  OnClose()	! астспхгсел гюохях ! гхосел х гюйпшбюел оюойх
		I_GRAF=0		! намскъел яверйх б опнжедспе FIRST
		DO II=1,N_CHEN
		N_WRITE_VLAG(II)=0						! мнлеп гюохях б тюик VLAG
		N_WRITE_UGLUB(II)=0						! мнлеп гюохях б тюик UGLUB
        N_WRITE_TOLSH(II)=0						! мнлеп гюохях б тюик UGLUB
		END DO
! тнплхпсел хлемю оюонй
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL FILE_WRITE_SECOND()									! гюдюмхе тюикю гюохях рейярнбнцн
		CALL FILE_WRITE_CSV_SECOND() 								! тюикю х тюикю пхясмйнб
	END IF
END DO
DO N_CHEN_TEK=1,N_CHEN	
	IF(N_WRITE_TOLSH(N_CHEN_TEK).EQ.N_TOLSH) THEN		! нябнанфдюел астеп мю дхяй
		II=  OnClose()	! астспхгсел гюохях ! гхосел х гюйпшбюел оюойх
		I_GRAF=0		! намскъел яверйх б опнжедспе FIRST
		DO II=1,N_CHEN
		N_WRITE_VLAG(II)=0						! мнлеп гюохях б тюик VLAG
		N_WRITE_UGLUB(II)=0						! мнлеп гюохях б тюик UGLUB
        N_WRITE_TOLSH(II)=0						! мнлеп гюохях б тюик UGLUB
		END DO
! тнплхпсел хлемю оюонй
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL FILE_WRITE_SECOND()									! гюдюмхе тюикю гюохях рейярнбнцн
		CALL FILE_WRITE_CSV_SECOND() 								! тюикю х тюикю пхясмйнб
	END IF
END DO
	END
