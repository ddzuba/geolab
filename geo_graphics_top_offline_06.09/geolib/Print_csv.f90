!----------------------------------------------------------------------------
! нтнплкемхе ябндмшу беднлняреи
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
! беднлнярэ цксахм
! нопедекемхх цксахм б онкняе 0-50 ял,50-150 ял
!----------------------------------------------------------------------------
SUBROUTINE PRINT_GRAF_CSV()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
IMPLICIT NONE
INTEGER(4) I,II,II1

IF(ITEK.GT.LM_WINDOW.AND.ITEK.GT.LM_WINDOW) THEN	! акнйхпсел оевюрэ б мювюкэмшу рнвйюу
	WRITE(S1_G,'(I4)')	INT4(KILOMETR)	
	S1_G=adjustl(S1_G)
	WRITE(S2_G,'(F8.2)')	METR
	S2_G=adjustl(S2_G)	
	WRITE(S3_G,'(F8.4)')	HH(ii_bal(N_CHEN_TEK))*.01
	S3_G=adjustl(S3_G)	
	WRITE(S4_G,'(F8.4)')	HH(ii_osn(N_CHEN_TEK))*.01	
	S4_G=adjustl(S4_G)	
i_GRAF=i_GRAF+1
STRING_GRAF_CSV(i_GRAF)=TRIM(N_DIRECTION(1:N2_DIR)//';'//N_TRACK(1:N2_TRA)//';'//TRIM(STRING_CHEN)//';'//TRIM(S1_G)//';'//TRIM(S2_G)//';'//TRIM(S3_G)//';'//TRIM(S4_G)//';')
END IF
RETURN
END 

!open(unit=100, FILE='D:/My_fortran_proects_tvema/OUTPUT.OUT')
!----------------------------------------------------------------------------
! беднлнярэ бкюфмнярх
!----------------------------------------------------------------------------
SUBROUTINE PRINT_VLAG_CSV()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE VLAG_CSV
IMPLICIT NONE

INTEGER(4) I,II

IF(ITEK.GT.LM_WINDOW) THEN                                                  ! акнйхпсел оевюрэ б мювюкэмшу рнвйюу
	IF(REFR(N_CHEN_TEK).GT.REFR_MIDL(N_CHEN_TEK)*R_POSIB1) THEN	            ! опебшьемхе нрпюфюрекэмни яонянамнярх				
		IF(IPR(N_CHEN_TEK).EQ.0)THEN										! опхгмюй мювюкю рейсыеи гюохях
		KILOMETR_FIRST(N_CHEN_TEK)=KILOMETR									            ! мювюкэмне гмювемхе
		METR_FIRST(N_CHEN_TEK)=METR											            ! мювюкэмне гмювемхе
		IPR(N_CHEN_TEK)=1													! опхгмюй нйнмвюмхъ рейсыеи гюохях
		END IF
	ELSE
		IF(IPR(N_CHEN_TEK).EQ.1)THEN			                            ! опхгмюй нйнмвюмхъ рейсыеи гюохях
			IPR(N_CHEN_TEK)=0					                            ! намскхкх хмрепбюк бкюфмнярх
			! оепбюъ гюохяэ б рюакхже
			IF(N_WRITE_VLAG(N_CHEN_TEK).EQ.0) THEN			
				IDL=ABS((KILOMETR-KILOMETR_FIRST(N_CHEN_TEK))*1000+METR-METR_FIRST(N_CHEN_TEK))
				WRITE(S1_V(N_CHEN_TEK),'(I4)')	INT4(KILOMETR_FIRST(N_CHEN_TEK))	
				WRITE(S2_V(N_CHEN_TEK),'(F8.2)')	KILOMETR_FIRST(N_CHEN_TEK)
				WRITE(S3_V(N_CHEN_TEK),'(I4)')	INT4(KILOMETR)
				WRITE(S4_V(N_CHEN_TEK),'(F8.2)')	METR	
				WRITE(S5_V(N_CHEN_TEK),'(F8.2)')	IDL
                WRITE(S6_V(N_CHEN_TEK),'(F8.2)')	HH(ii_osn(N_CHEN_TEK))*.01	
                N_WRITE_VLAG(N_CHEN_TEK)=N_WRITE_VLAG(N_CHEN_TEK)+1
STRING_VLAG_CSV(N_WRITE_VLAG(N_CHEN_TEK),N_CHEN_TEK)=TRIM(N_DIRECTION(1:N2_DIR)//';'//N_TRACK(1:N2_TRA)//';'//TRIM(STRING_CHEN)//';'&
//TRIM(S1_V(N_CHEN_TEK))//';'//TRIM(S2_V(N_CHEN_TEK))//';'//TRIM(S3_V(N_CHEN_TEK))//';'//TRIM(S4_V(N_CHEN_TEK))//';'//TRIM(S5_V(N_CHEN_TEK)))//';'//TRIM(S6_V(N_CHEN_TEK))//';'
			! брнпюъ х онякедсчыхе гюохях б рюакхже
			! юмюкхгхпсел мю дкхмс гюохях х пюяярнъмхе лефдс гюохяълх
			ELSE						
			! вхрюч хг рюакхжш опедшдсыхе гмювемхъ йннпдхмюр х дкхмш
				READ(S1_V(N_CHEN_TEK),'(I4)')	II
				KILOMETR_FIRST_OLD=REAL(II)				! бняярюмнбхкх мювюкэмне гмювемхе		
				READ(S2_V(N_CHEN_TEK),'(F8.2)')	METR_FIRST_OLD		! бняярюмнбхкх мювюкэмне гмювемхе
				READ(S3_V(N_CHEN_TEK),'(I4)')	II
				KILOMETR_OLD= REAL(II)					! бняярюмюбкхбюел йнмевмне гмювемхе 		
				READ(S4_V(N_CHEN_TEK),'(F8.2)')	METR_OLD			! бняярюмюбкхбюел йнмевмне гмювемхе 
				READ(S5_V(N_CHEN_TEK),'(F8.2)')	IDL_OLD				! бняярюмюбкхбюел  гмювемхе 
				! еякх пюяярнъмхе лефдс опедшдсыеи х онякедсчыеи рнвйюлх лемэье STEP_HUMIDITY рн назедхмъел
				IF(ABS((KILOMETR_OLD-KILOMETR_FIRST(N_CHEN_TEK))*1000+ABS(METR_OLD-METR_FIRST(N_CHEN_TEK))).LT.STEP_HUMIDITY) THEN ! назедхмъел	
!					N_WRITE_VLAG(N_CHEN_TEK)=N_WRITE_VLAG(N_CHEN_TEK)-1
					IDL=ABS((KILOMETR-KILOMETR_FIRST_OLD)*1000+METR-METR_FIRST_OLD)
					WRITE(S1_V(N_CHEN_TEK),'(I4)')		INT4(KILOMETR_FIRST_OLD)	
					WRITE(S2_V(N_CHEN_TEK),'(F8.2)')	METR_FIRST_OLD
					WRITE(S3_V(N_CHEN_TEK),'(I4)')		INT4(KILOMETR)
					WRITE(S4_V(N_CHEN_TEK),'(F8.2)')	METR	
					WRITE(S5_V(N_CHEN_TEK),'(F8.2)')	IDL	
                    WRITE(S6_V(N_CHEN_TEK),'(F8.2)')	HH(ii_osn(N_CHEN_TEK))*.01	
STRING_VLAG_CSV(N_WRITE_VLAG(N_CHEN_TEK),N_CHEN_TEK)=TRIM(N_DIRECTION(1:N2_DIR)//';'//N_TRACK(1:N2_TRA)//';'//TRIM(STRING_CHEN)//';'&
//TRIM(S1_V(N_CHEN_TEK))//';'//TRIM(S2_V(N_CHEN_TEK))//';'//TRIM(S3_V(N_CHEN_TEK))//';'//TRIM(S4_V(N_CHEN_TEK))//';'//TRIM(S5_V(N_CHEN_TEK)))//';'//TRIM(S6_V(N_CHEN_TEK))//';'
					RETURN
				ELSE 
				! кхйбхдхпсел опедшдсысч гюохяэ еякх ее дкхмю лемэье L лерпнб
					IF(IDL_OLD.LT.LENGTH_HUMIDITY) THEN
					N_WRITE_VLAG(N_CHEN_TEK)=N_WRITE_VLAG(N_CHEN_TEK)-1
					END IF
				! гюохяшбюел рейсысч гюохяэ	
				IDL=ABS((KILOMETR-KILOMETR_FIRST(N_CHEN_TEK))*1000+METR-METR_FIRST(N_CHEN_TEK))
				WRITE(S1_V(N_CHEN_TEK),'(I4)')	INT4(KILOMETR_FIRST(N_CHEN_TEK))	
				WRITE(S2_V(N_CHEN_TEK),'(F8.2)')	METR_FIRST(N_CHEN_TEK)
				WRITE(S3_V(N_CHEN_TEK),'(I4)')	INT4(KILOMETR)
				WRITE(S4_V(N_CHEN_TEK),'(F8.2)')	METR	
				WRITE(S5_V(N_CHEN_TEK),'(F8.2)')	IDL	
                WRITE(S6_V(N_CHEN_TEK),'(F8.2)')	HH(ii_osn(N_CHEN_TEK))*.01	
				N_WRITE_VLAG(N_CHEN_TEK)=N_WRITE_VLAG(N_CHEN_TEK)+1
STRING_VLAG_CSV(N_WRITE_VLAG(N_CHEN_TEK),N_CHEN_TEK)=TRIM(N_DIRECTION(1:N2_DIR)//';'//N_TRACK(1:N2_TRA)//';'//TRIM(STRING_CHEN)//';'&
//TRIM(S1_V(N_CHEN_TEK))//';'//TRIM(S2_V(N_CHEN_TEK))//';'//TRIM(S3_V(N_CHEN_TEK))//';'//TRIM(S4_V(N_CHEN_TEK))//';'//TRIM(S5_V(N_CHEN_TEK)))//';'//TRIM(S6_V(N_CHEN_TEK))//';'
				END IF					
			END IF	 
		END IF
	END IF
END IF
RETURN
100 FORMAT(A200)
END 

!open(unit=100, FILE='D:/My_fortran_proects_tvema/OUTPUT.OUT')

!----------------------------------------------------------------------------
! беднлнярэ аюккюярмшу сцксакемхи
!----------------------------------------------------------------------------
SUBROUTINE PRINT_TOLSH_CSV()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE TOLSH_CSV
IMPLICIT NONE
INTEGER I,II
 
IF(ITEK.GT.LM_WINDOW.AND.ITEK.GT.LM_WINDOW) THEN	! акнйхпсел оевюрэ б мювюкэмшу рнвйюу
 	IF(R_BALL_UP(N_CHEN_TEK).GE.0.601.OR.R_BALL_UP(N_CHEN_TEK).LE.0.349) THEN					
		IF(IPR1_UP(N_CHEN_TEK).EQ.0)THEN				                	! опхгмюй мювюкю рейсыеи гюохях
		KILOMETR_FIRST_T(N_CHEN_TEK)=KILOMETR				                            ! мювюкэмне гмювемхе
		METR_FIRST_T(N_CHEN_TEK)=METR						                            ! мювюкэмне гмювемхе
		IPR1_UP(N_CHEN_TEK)=1
		END IF
	ELSE
		IF(IPR1_UP(N_CHEN_TEK).EQ.1)THEN				                	! опхгмюй нйнмвюмхъ рейсыеи гюохях
		IPR1_UP(N_CHEN_TEK)=0
			IF(N_WRITE_TOLSH(N_CHEN_TEK).EQ.0) THEN		            	! оепбюъ гюохяэ б рюакхже
				IDL=ABS((KILOMETR-KILOMETR_FIRST_T(N_CHEN_TEK))*1000+METR-METR_FIRST_T(N_CHEN_TEK))
				WRITE(S1_U_UP(N_CHEN_TEK),'(I4)')		INT4(KILOMETR_FIRST_T(N_CHEN_TEK))	
				WRITE(S2_U_UP(N_CHEN_TEK),'(F8.2)')	METR_FIRST_T(N_CHEN_TEK)
				WRITE(S3_U_UP(N_CHEN_TEK),'(I4)')		INT4(KILOMETR)
				WRITE(S4_U_UP(N_CHEN_TEK),'(F8.2)')	METR	
				WRITE(S5_U_UP(N_CHEN_TEK),'(F8.2)')	IDL	
				N_WRITE_TOLSH(N_CHEN_TEK)=N_WRITE_TOLSH(N_CHEN_TEK)+1
STRING_TOLSH_CSV(N_WRITE_TOLSH(N_CHEN_TEK),N_CHEN_TEK)=TRIM(N_DIRECTION(1:N2_DIR)//';'//N_TRACK(1:N2_TRA)//';'//TRIM(STRING_CHEN)//';'&
//TRIM(S1_U_UP(N_CHEN_TEK))//';'//TRIM(S2_U_UP(N_CHEN_TEK))//';'//TRIM(S3_U_UP(N_CHEN_TEK))//';'//TRIM(S4_U_UP(N_CHEN_TEK))//';'//TRIM(S5_U_UP(N_CHEN_TEK))//';')
            ELSE							                            ! брнпюъ х онякедсчыхе гюохях б рюакхже
				! бняярюмюбкхбюч опедшдсыхе гмювемхъ
				READ(S1_U_UP(N_CHEN_TEK),'(I4)')II	
				KILOMETR_FIRST_OLD=REAL(II)				            ! бняярюмнбхкх мювюкэмне гмювемхе		
				READ(S2_U_UP(N_CHEN_TEK),'(F8.2)')	METR_FIRST_OLD		! бняярюмнбхкх мювюкэмне гмювемхе
				READ(S3_U(N_CHEN_TEK),'(I4)')II	
				KILOMETR_OLD=REAL(II)					            ! бняярюмюбкхбюел йнмевмне гмювемхе 		
				READ(S4_U_UP(N_CHEN_TEK),'(F8.2)')	METR_OLD			! бняярюмюбкхбюел йнмевмне гмювемхе 
				READ(S5_U_UP(N_CHEN_TEK),'(F8.2)')	IDL_OLD				! бняярюмюбкхбюел  гмювемхе 
					IF(ABS((KILOMETR_OLD-KILOMETR_FIRST_T(N_CHEN_TEK))*1000+METR_OLD-METR_FIRST_T(N_CHEN_TEK)).LT.10.0) THEN	! назедхмъел гюохях еякх пюяярнъмхе лемэье 3 лерпнб
						N_WRITE_TOLSH(N_CHEN_TEK)=N_WRITE_TOLSH(N_CHEN_TEK)-1
						IDL=ABS((KILOMETR-KILOMETR_FIRST_OLD)*1000+METR-METR_FIRST_OLD)
						WRITE(S1_U_UP(N_CHEN_TEK),'(I4)')	INT4(KILOMETR_FIRST_OLD)	
						WRITE(S2_U_UP(N_CHEN_TEK),'(F8.2)')	METR_FIRST_OLD
						WRITE(S3_U_UP(N_CHEN_TEK),'(I4)')	INT4(KILOMETR)
						WRITE(S4_U_UP(N_CHEN_TEK),'(F8.2)')	METR	
						WRITE(S5_U_UP(N_CHEN_TEK),'(F8.2)')	IDL	
						N_WRITE_TOLSH(N_CHEN_TEK)=N_WRITE_TOLSH(N_CHEN_TEK)+1
STRING_TOLSH_CSV(N_WRITE_TOLSH(N_CHEN_TEK),N_CHEN_TEK)=TRIM(N_DIRECTION(1:N2_DIR)//';'//N_TRACK(1:N2_TRA)//';'//TRIM(STRING_CHEN)//';'&
//TRIM(S1_U_UP(N_CHEN_TEK))//';'//TRIM(S2_U_UP(N_CHEN_TEK))//';'//TRIM(S3_U_UP(N_CHEN_TEK))//';'//TRIM(S4_U_UP(N_CHEN_TEK))//';'//TRIM(S5_U_UP(N_CHEN_TEK))//';')
						GOTO 1
					ELSE ! кхйбхдхпсел опедшдсысч гюохяэ еякх ее дкхмю лемэье 3 лерпнб
						IF(IDL_OLD.LT.10.0) THEN
								N_WRITE_TOLSH(N_CHEN_TEK)=N_WRITE_TOLSH(N_CHEN_TEK)-1
						END IF	
							IDL=ABS((KILOMETR-KILOMETR_FIRST_T(N_CHEN_TEK))*1000+METR-METR_FIRST_T(N_CHEN_TEK))
							WRITE(S1_U_UP(N_CHEN_TEK),'(I4)')		INT4(KILOMETR_FIRST_T(N_CHEN_TEK))	
							WRITE(S2_U_UP(N_CHEN_TEK),'(F8.2)')	METR_FIRST_T(N_CHEN_TEK)
							WRITE(S3_U_UP(N_CHEN_TEK),'(I4)')		INT4(KILOMETR)
							WRITE(S4_U_UP(N_CHEN_TEK),'(F8.2)')	METR	
							WRITE(S5_U_UP(N_CHEN_TEK),'(F8.2)')	IDL	
							N_WRITE_TOLSH(N_CHEN_TEK)=N_WRITE_TOLSH(N_CHEN_TEK)+1			
STRING_TOLSH_CSV(N_WRITE_TOLSH(N_CHEN_TEK),N_CHEN_TEK)=TRIM(N_DIRECTION(1:N2_DIR)//';'//N_TRACK(1:N2_TRA)//';'//TRIM(STRING_CHEN)//';'&
//TRIM(S1_U_UP(N_CHEN_TEK))//';'//TRIM(S2_U_UP(N_CHEN_TEK))//';'//TRIM(S3_U_UP(N_CHEN_TEK))//';'//TRIM(S4_U_UP(N_CHEN_TEK))//';'//TRIM(S5_U_UP(N_CHEN_TEK))//';')
goto 1
                    END IF					
			END IF	 
        END IF

	END IF
END IF
1   RETURN
END 
!----------------------------------------------------------------------------
! беднлнярэ сцксакемхи пюгдекхрекэмнцн якнъ
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
! беднлнярэ аюккюярмшу сцксакемхи
!----------------------------------------------------------------------------
SUBROUTINE PRINT_UGLUB_CSV()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE UGLUB_CSV
IMPLICIT NONE
INTEGER I,II

IF(ITEK.GT.LM_WINDOW.AND.ITEK.GT.LM_WINDOW) THEN	! акнйхпсел оевюрэ б мювюкэмшу рнвйюу
 	IF(R_BALL(N_CHEN_TEK).GE.R_BALL_MIDL(N_CHEN_TEK)*R_BALLAST) THEN					
		IF(IPR1(N_CHEN_TEK).EQ.0)THEN					! опхгмюй мювюкю рейсыеи гюохях
		KILOMETR_FIRST_U(N_CHEN_TEK)=KILOMETR				! мювюкэмне гмювемхе
		METR_FIRST_U(N_CHEN_TEK)=METR						! мювюкэмне гмювемхе
		IPR1(N_CHEN_TEK)=1
		END IF
	ELSE
		IF(IPR1(N_CHEN_TEK).EQ.1)THEN					! опхгмюй нйнмвюмхъ рейсыеи гюохях
		IPR1(N_CHEN_TEK)=0
			IF(N_WRITE_UGLUB(N_CHEN_TEK).EQ.0) THEN			! оепбюъ гюохяэ б рюакхже
				IDL=ABS((KILOMETR-KILOMETR_FIRST_U(N_CHEN_TEK))*1000+METR-METR_FIRST_U(N_CHEN_TEK))
				WRITE(S1_U(N_CHEN_TEK),'(I4)')		INT4(KILOMETR_FIRST_U(N_CHEN_TEK))	
				WRITE(S2_U(N_CHEN_TEK),'(F8.2)')	METR_FIRST_U(N_CHEN_TEK)
				WRITE(S3_U(N_CHEN_TEK),'(I4)')		INT4(KILOMETR)
				WRITE(S4_U(N_CHEN_TEK),'(F8.2)')	METR	
				WRITE(S5_U(N_CHEN_TEK),'(F8.2)')	IDL	
				N_WRITE_UGLUB(N_CHEN_TEK)=N_WRITE_UGLUB(N_CHEN_TEK)+1
STRING_UGLUB_CSV(N_WRITE_UGLUB(N_CHEN_TEK),N_CHEN_TEK)=TRIM(N_DIRECTION(1:N2_DIR)//';'//N_TRACK(1:N2_TRA)//';'//TRIM(STRING_CHEN)//';'&
//TRIM(S1_U(N_CHEN_TEK))//';'//TRIM(S2_U(N_CHEN_TEK))//';'//TRIM(S3_U(N_CHEN_TEK))//';'//TRIM(S4_U(N_CHEN_TEK))//';'//TRIM(S5_U(N_CHEN_TEK)))
			ELSE							! брнпюъ х онякедсчыхе гюохях б рюакхже
				! бняярюмюбкхбюч опедшдсыхе гмювемхъ
				READ(S1_U(N_CHEN_TEK),'(I4)')II	
				KILOMETR_FIRST_OLD=REAL(II)				! бняярюмнбхкх мювюкэмне гмювемхе		
				READ(S2_U(N_CHEN_TEK),'(F8.2)')	METR_FIRST_OLD		! бняярюмнбхкх мювюкэмне гмювемхе
				READ(S3_U(N_CHEN_TEK),'(I4)')II	
				KILOMETR_OLD=REAL(II)					! бняярюмюбкхбюел йнмевмне гмювемхе 		
				READ(S4_U(N_CHEN_TEK),'(F8.2)')	METR_OLD			! бняярюмюбкхбюел йнмевмне гмювемхе 
				READ(S5_U(N_CHEN_TEK),'(F8.2)')	IDL_OLD				! бняярюмюбкхбюел  гмювемхе 
					IF(ABS((KILOMETR_OLD-KILOMETR_FIRST_U(N_CHEN_TEK))*1000+METR_OLD-METR_FIRST_U(N_CHEN_TEK)).LT.10.0) THEN	! назедхмъел гюохях еякх пюяярнъмхе лемэье 3 лерпнб
						N_WRITE_UGLUB(N_CHEN_TEK)=N_WRITE_UGLUB(N_CHEN_TEK)-1
						IDL=ABS((KILOMETR-KILOMETR_FIRST_OLD)*1000+METR-METR_FIRST_OLD)
						WRITE(S1_U(N_CHEN_TEK),'(I4)')	INT4(KILOMETR_FIRST_OLD)	
						WRITE(S2_U(N_CHEN_TEK),'(F8.2)')	METR_FIRST_OLD
						WRITE(S3_U(N_CHEN_TEK),'(I4)')	INT4(KILOMETR)
						WRITE(S4_U(N_CHEN_TEK),'(F8.2)')	METR	
						WRITE(S5_U(N_CHEN_TEK),'(F8.2)')	IDL	
						N_WRITE_UGLUB(N_CHEN_TEK)=N_WRITE_UGLUB(N_CHEN_TEK)+1
STRING_UGLUB_CSV(N_WRITE_UGLUB(N_CHEN_TEK),N_CHEN_TEK)=TRIM(N_DIRECTION(1:N2_DIR)//';'//N_TRACK(1:N2_TRA)//';'//TRIM(STRING_CHEN)//';'&
//TRIM(S1_U(N_CHEN_TEK))//';'//TRIM(S2_U(N_CHEN_TEK))//';'//TRIM(S3_U(N_CHEN_TEK))//';'//TRIM(S4_U(N_CHEN_TEK))//';'//TRIM(S5_U(N_CHEN_TEK)))
						RETURN
					ELSE ! кхйбхдхпсел опедшдсысч гюохяэ еякх ее дкхмю лемэье 3 лерпнб
						IF(IDL_OLD.LT.10.0) THEN
								N_WRITE_UGLUB(N_CHEN_TEK)=N_WRITE_UGLUB(N_CHEN_TEK)-1
						END IF	
							IDL=ABS((KILOMETR-KILOMETR_FIRST_U(N_CHEN_TEK))*1000+METR-METR_FIRST_U(N_CHEN_TEK))
							WRITE(S1_U(N_CHEN_TEK),'(I4)')		INT4(KILOMETR_FIRST_U(N_CHEN_TEK))	
							WRITE(S2_U(N_CHEN_TEK),'(F8.2)')	METR_FIRST_U(N_CHEN_TEK)
							WRITE(S3_U(N_CHEN_TEK),'(I4)')		INT4(KILOMETR)
							WRITE(S4_U(N_CHEN_TEK),'(F8.2)')	METR	
							WRITE(S5_U(N_CHEN_TEK),'(F8.2)')	IDL	
							N_WRITE_UGLUB(N_CHEN_TEK)=N_WRITE_UGLUB(N_CHEN_TEK)+1			
STRING_UGLUB_CSV(N_WRITE_UGLUB(N_CHEN_TEK),N_CHEN_TEK)=TRIM(N_DIRECTION(1:N2_DIR)//';'//N_TRACK(1:N2_TRA)//';'//TRIM(STRING_CHEN)//';'&
//TRIM(S1_U(N_CHEN_TEK))//';'//TRIM(S2_U(N_CHEN_TEK))//';'//TRIM(S3_U(N_CHEN_TEK))//';'//TRIM(S4_U(N_CHEN_TEK))//';'//TRIM(S5_U(N_CHEN_TEK)))
					END IF					
			END IF	 
		END IF
	END IF
END IF
RETURN
END 
!----------------------------------------------------------------------------
! гюохяэ беднлнярх цксахм якнеб
!----------------------------------------------------------------------------
SUBROUTINE PRINT_GRAF_FINISH()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
IMPLICIT NONE
INTEGER(4) I

! нрйпшбюел тюик гюохях
OPEN(UNIT=10,FILE=GRAF_NUMBER(N_CHEN_TEK),STATUS='OLD',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
! ондбнд й рейсыеи гюохях бшонкмем опх нрйпшрхх
!REWIND 10
!DO 
!READ(10,*,END=1)
!END DO
!1 CONTINUE

DO I=N_CHEN_TEK,i_GRAF,N_CHEN
WRITE(10,100)STRING_GRAF_CSV(I)
END DO
CLOSE (10)
RETURN
100 FORMAT(A200)
END 
!----------------------------------------------------------------------------
! гюохяэ беднлнярх накюяреи бкюфмнярх
!----------------------------------------------------------------------------
SUBROUTINE PRINT_VLAG_FINISH()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
IMPLICIT NONE
INTEGER I

! нрйпшбюел тюик гюохях
OPEN(UNIT=20,FILE=VLAG_NUMBER(N_CHEN_TEK),STATUS='OLD',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
! ондбнд й рейсыеи гюохях бшонкмем опх нрйпшрхх
if(N_WRITE_VLAG(N_CHEN_TEK).ge.1) then
DO I=1,N_WRITE_VLAG(N_CHEN_TEK)
WRITE(20,100)STRING_VLAG_CSV(I,N_CHEN_TEK)
END DO
end if
CLOSE (20)
RETURN
100 FORMAT(A200)
END 

!----------------------------------------------------------------------------
! гюохяэ беднлнярх накюяреи сцксакемхи
!----------------------------------------------------------------------------
SUBROUTINE PRINT_UGLUB_FINISH()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
IMPLICIT NONE
INTEGER I

! нрйпшбюел тюик гюохях----------------------------------------------------------
OPEN(UNIT=30,FILE=UGLUB_NUMBER(N_CHEN_TEK),STATUS='OLD',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
! ондбнд й рейсыеи гюохях бшонкмем опх нрйпшрхх
if(N_WRITE_UGLUB(N_CHEN_TEK).ge.1) then
DO I=1,N_WRITE_UGLUB(N_CHEN_TEK)
WRITE(30,100)STRING_UGLUB_CSV(I,N_CHEN_TEK)
END DO
end if
CLOSE (30)
RETURN
100 FORMAT(A200)
END 

SUBROUTINE PRINT_TOLSH_FINISH()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
IMPLICIT NONE
INTEGER I

! нрйпшбюел тюик гюохях----------------------------------------------------------
OPEN(UNIT=31,FILE=TOLSH_NUMBER(N_CHEN_TEK),STATUS='OLD',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
! ондбнд й рейсыеи гюохях бшонкмем опх нрйпшрхх
if(N_WRITE_TOLSH(N_CHEN_TEK).ge.1) then
DO I=1,N_WRITE_TOLSH(N_CHEN_TEK)
WRITE(31,100)STRING_TOLSH_CSV(I,N_CHEN_TEK)
END DO
end if
CLOSE (31)
RETURN
100 FORMAT(A200)
END 

SUBROUTINE ANALIZ_TOLSH()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE TOLSH_CSV
IMPLICIT NONE
INTEGER I,I1,II,K1,K2,M
CHARACTER*200 RAB_STRING
CHARACTER*200 RAB_STRING1
INTEGER(4) J,J3,J4,J5,J6
INTEGER(4) L3,L4,L5,L6,L7
INTEGER(4) KIL_UGLUB, K_FIRST,K_SEKOND
REAL(4)    METR_UGLUB,M_FIRST,M_SEKOND,DEPTH,DEPTH1
CHARACTER*20 STR_KL,STR_M,STR_DEPTH

IF(N_WRITE_TOLSH(N_CHEN_TEK).EQ.0) RETURN  
DO M=1, N_WRITE_TOLSH(N_CHEN_TEK)                                                          ! онъбхкюяэ мнбюъ ярпнйю
WRITE(RAB_STRING,'(A)')STRING_TOLSH_CSV(M,N_CHEN_TEK)

J=0 
    DO I1=1,200                                                                       ! юмюкхгхпсел ярпнйс
    IF(RAB_STRING(I1:I1).EQ.';') THEN
    J=J+1                                                                             ! явервхй пюгдекхрекеи б ярпнйе
    IF(J.EQ.3)L3=I1;IF(J.EQ.4)L4=I1;IF(J.EQ.5)L5=I1;IF(J.EQ.6)L6=I1;IF(J.EQ.7)L7=I1   ! гюонлхмюел онгхжхх пюгдекхрекеи
    END IF
    END DO 
! бняярюмюбкхбюел цпюмхжш хмрепбюкю------------------------------------------------------------------------------------
    DO II=L3+1,L4-1                                                        ! бняярюмюбкхбюел йхкнлерп
    S1(II-L3:II-L3)=RAB_STRING(II:II)
    END DO
    READ(S1,'(I4)')K_FIRST    
    DO II=L4+1,L5-1                                                        ! бняярюмюбкхбюел лерп
    S2(II-L4:II-L4)=RAB_STRING(II:II)
    END DO
    READ(S2,'(F8.0)')M_FIRST
    DO II=L5+1,L6-1                                                        ! бняярюмюбкхбюел йхкнлерп
    S3(II-L5:II-L5)=RAB_STRING(II:II)
    END DO
    READ(S3,'(I4)')K_SEKOND    
    DO II=L6+1,L7-1                                                        ! бняярюмюбкхбюел лерп
    S4(II-L6:II-L6)=RAB_STRING(II:II)
    END DO
    READ(S4,'(F8.0)')M_SEKOND
! хыел мнлеп ярпнйх б рюакхже цксахм -----------------------------------------------------------------------------
K1=0
K2=0
      DO I=N_CHEN_TEK,i_GRAF,N_CHEN                                             ! бшахпюел йюмюк
      J=0;J3=0;J4=0;J5=0
      WRITE(RAB_STRING1,'(A)')STRING_GRAF_CSV(I)
 
          DO I1=1,200                                                           ! юмюкхгхпсел ярпнйс
          IF(RAB_STRING1(I1:I1).EQ.';') THEN
          J=J+1                                    ! явервхй пюгдекхрекеи б ярпнйе
          IF(J.EQ.3)J3=I1;IF(J.EQ.4)J4=I1;IF(J.EQ.5)J5=I1      ! гюонлхмюел онгхжхх пюгдекхрекеи
          END IF
          END DO
            STR_KL='                    ' 
            DO II=J3+1,J4-1                                                        ! бняярюмюбкхбюел йхкнлерп
            STR_KL(II-J3:II-J3)=RAB_STRING1(II:II)
            END DO  

            READ(STR_KL,'(I4)')KIL_UGLUB

            
            STR_M='                     '           
            DO II=J4+1,J5-1                                                        ! бняярюмюбкхбюел лерп
            STR_M(II-J4:II-J4)=RAB_STRING1(II:II)
            END DO

            READ( STR_M,'(F8.0)') METR_UGLUB 

IF(KIL_UGLUB.EQ.K_FIRST.AND.METR_UGLUB.EQ.M_FIRST)   K1=I
IF(KIL_UGLUB.EQ.K_SEKOND.AND.METR_UGLUB.EQ.M_SEKOND) THEN
K2=I; GOTO 1
END IF
      END DO
1    IF(K1.NE.0.AND.K2.NE.0) THEN
DEPTH=0.0                                                           ! нопедекъел люйяхлюкэмне сцксакемхе
      DO I=K1,K2,N_CHEN                                             ! бшахпюел йюмюк
      J=0;J5=0;J6=0
      WRITE(RAB_STRING1,'(A)')STRING_GRAF_CSV(I)
          DO I1=1,200                                                           ! юмюкхгхпсел ярпнйс
          IF(RAB_STRING1(I1:I1).EQ.';') THEN
          J=J+1                                    ! явервхй пюгдекхрекеи б ярпнйе
         IF(J.EQ.5)J5=I1;IF(J.EQ.6)J6=I1      ! гюонлхмюел онгхжхх пюгдекхрекеи
          END IF
          END DO  
            STR_DEPTH='                     '     
            DO II=J5+1,J6-1                                                        ! бняярюмюбкхбюел йхкнлерп
            STR_DEPTH(II-J5:II-J5)=RAB_STRING1(II:II)
            END DO
STR_DEPTH=ADJUSTL(STR_DEPTH)
READ(STR_DEPTH,'(F8.4)') DEPTH1  
 
IF(DEPTH1.GT.0.601.AND.DEPTH1.GT.DEPTH)DEPTH=DEPTH1
IF(DEPTH1.LT.0.349.AND.DEPTH1.LT.DEPTH.OR.DEPTH.LT.0.1)DEPTH=DEPTH1
END DO
STR_DEPTH='                     '     
WRITE(STR_DEPTH,'(F8.4)') DEPTH
 
STR_DEPTH=ADJUSTL(STR_DEPTH)

STRING_TOLSH_CSV(M,N_CHEN_TEK)= TRIM(STRING_TOLSH_CSV(M,N_CHEN_TEK))//TRIM(STR_DEPTH)//';'     
ELSE
STRING_TOLSH_CSV(M,N_CHEN_TEK)= TRIM(STRING_TOLSH_CSV(M,N_CHEN_TEK))//'-1'//';'    
END IF    
  END DO
END
!----------------------------------------------------------------------------
! юмюкхг  сцксакемхи б ндмнл йюмюке
!  IF(N_CHEN.LT.3) CALL ANALIZ_UGLUB() 
!----------------------------------------------------------------------------
SUBROUTINE ANALIZ_UGLUB()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE UGLUB_CSV
IMPLICIT NONE
INTEGER(4) M,I,II
INTEGER(4)  K_FIRST,K_SEKOND
REAL(4)     M_FIRST,M_SEKOND,DEPTH
! юмюкхгхпсел пегскэрюрш йюфднцн йюмюкю
IF(N_WRITE_UGLUB(N_CHEN_TEK).EQ.0) RETURN 	

II=0
DO 1 M=1, N_WRITE_UGLUB(N_CHEN_TEK)                                                       ! онъбхкюяэ мнбюъ ярпнйю 
CALL ANALIZ_SINGL_CHAN_UGLUB(M,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH)
WRITE(S1_U(N_CHEN_TEK),'(F8.2)')DEPTH
    IF(DEPTH.LT.0.) GOTO 1 
    IF(DEPTH.LE.0.1.AND.DEPTH.GT.0.05) THEN                                               ! аюккюярмне сцксакемхе 
II=II+1        
STRING_UGLUB_CSV(II,N_CHEN_TEK)=TRIM(STRING_UGLUB_CSV(M,N_CHEN_TEK))//';'//'АЮККЮЯРМНЕ СЦКСАКЕМХЕ'//';'//TRIM(S1_U(N_CHEN_TEK))//';' 
GOTO 1
    END IF
    IF(DEPTH.LE.0.3.AND.DEPTH.GT.0.1) THEN                                                 ! аюккюярмне йнпшрн
II=II+1        
STRING_UGLUB_CSV(II,N_CHEN_TEK)=TRIM(STRING_UGLUB_CSV(M,N_CHEN_TEK))//';'//'АЮККЮЯРМНЕ ЙНПШРН'//';'//TRIM(S1_U(N_CHEN_TEK))//';'  
GOTO 1
    END IF      
    IF(DEPTH.GT.0.3) THEN                                                                   ! аюккюярмне кнфе
II=II+1        
STRING_UGLUB_CSV(M,N_CHEN_TEK)=TRIM(STRING_UGLUB_CSV(M,N_CHEN_TEK))//';'//'АЮККЮЯРМНЕ КНФЕ'//';'//TRIM(S1_U(N_CHEN_TEK))//';'  
END IF 
1   CONTINUE
N_WRITE_UGLUB(N_CHEN_TEK)=II
END 

SUBROUTINE ANALIZ_SINGL_CHAN_UGLUB(M,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH)
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE TOLSH_CSV
IMPLICIT NONE
INTEGER I,I1,II,K1,K2,M,JJ
CHARACTER*200 RAB_STRING
CHARACTER*200 RAB_STRING1
INTEGER(4) J,J3,J4,J5,J6,J7
INTEGER(4) L3,L4,L5,L6,L7,L8
INTEGER(4) KIL_UGLUB, K_FIRST,K_SEKOND
REAL(4)    METR_UGLUB,M_FIRST,M_SEKOND,DEPTH,DEPTH1,DEPTH_MIDL,DLINA
CHARACTER*20 STR_KL,STR_M,STR_DEPTH

IF(N_WRITE_UGLUB(N_CHEN_TEK).EQ.0) RETURN  
                                                    
WRITE(RAB_STRING,'(A)')STRING_UGLUB_CSV(M,N_CHEN_TEK)                                 ! онъбхкюяэ мнбюъ ярпнйю
J=0 
    DO I1=1,200                                                                       ! юмюкхгхпсел ярпнйс
    IF(RAB_STRING(I1:I1).EQ.';') THEN
    J=J+1                                                                             ! явервхй пюгдекхрекеи б ярпнйе
    IF(J.EQ.3)L3=I1;IF(J.EQ.4)L4=I1;IF(J.EQ.5)L5=I1;IF(J.EQ.6)L6=I1;IF(J.EQ.7)L7=I1 ;IF(J.EQ.8)L8=I1  ! гюонлхмюел онгхжхх пюгдекхрекеи
    END IF
    END DO 
 
! бняярюмюбкхбюел цпюмхжш хмрепбюкю------------------------------------------------------------------------------------
    DO II=L3+1,L4-1                                                        ! бняярюмюбкхбюел йхкнлерп
    S1(II-L3:II-L3)=RAB_STRING(II:II)
    END DO
    READ(S1,'(I4)')K_FIRST    
    DO II=L4+1,L5-1                                                        ! бняярюмюбкхбюел лерп
    S2(II-L4:II-L4)=RAB_STRING(II:II)
    END DO
    READ(S2,'(F8.0)')M_FIRST
    DO II=L5+1,L6-1                                                        ! бняярюмюбкхбюел йхкнлерп
    S3(II-L5:II-L5)=RAB_STRING(II:II)
    END DO
    READ(S3,'(I4)')K_SEKOND    
    DO II=L6+1,L7-1                                                        ! бняярюмюбкхбюел лерп
    S4(II-L6:II-L6)=RAB_STRING(II:II)
    END DO
    READ(S4,'(F8.0)')M_SEKOND
!    DO II=L7+1,L8-1                                                        ! бняярюмюбкхбюел дкхмс
!    S5(II-L7:II-L7)=RAB_STRING(II:II)
 !   END DO
 !   READ(S5,'(F8.0)')DLINA
! хыел мнлеп ярпнйх б рюакхже цксахм -----------------------------------------------------------------------------
K1=0
K2=0
      DO I=N_CHEN_TEK,i_GRAF,N_CHEN                                             ! бшахпюел йюмюк
      J=0;J3=0;J4=0;J5=0
      WRITE(RAB_STRING1,'(A)')STRING_GRAF_CSV(I)
          DO I1=1,200                                                           ! юмюкхгхпсел ярпнйс
          IF(RAB_STRING1(I1:I1).EQ.';') THEN
          J=J+1                                                                  ! явервхй пюгдекхрекеи б ярпнйе
          IF(J.EQ.3)J3=I1;IF(J.EQ.4)J4=I1;IF(J.EQ.5)J5=I1                         ! гюонлхмюел онгхжхх пюгдекхрекеи
          END IF
          END DO
    STR_KL='                    '
            DO II=J3+1,J4-1                                                        ! бняярюмюбкхбюел йхкнлерп
            STR_KL(II-J3:II-J3)=RAB_STRING1(II:II)
            END DO      
            READ(STR_KL,'(I4)')KIL_UGLUB
   STR_M='                    '        
            DO II=J4+1,J5-1                                                        ! бняярюмюбкхбюел лерп
            STR_M(II-J4:II-J4)=RAB_STRING1(II:II)
            END DO
            READ(STR_M,'(F8.0)') METR_UGLUB 
IF(KIL_UGLUB.EQ.K_FIRST.AND.METR_UGLUB.EQ.M_FIRST)   K1=I
IF(KIL_UGLUB.EQ.K_SEKOND.AND.METR_UGLUB.EQ.M_SEKOND) THEN
K2=I; GOTO 1
END IF
      END DO
1     IF(K1.NE.0.AND.K2.NE.0) THEN
DEPTH=0.0                                                                       ! нопедекъел люйяхлюкэмне сцксакемхе
      DO I=K1,K2,N_CHEN                                                         ! бшахпюел йюмюк
      J=0;J6=0;J7=0
      WRITE(RAB_STRING1,'(A)')STRING_GRAF_CSV(I)
          DO I1=1,200                                                           ! юмюкхгхпсел ярпнйс
          IF(RAB_STRING1(I1:I1).EQ.';') THEN
          J=J+1                                                                 ! явервхй пюгдекхрекеи б ярпнйе
         IF(J.EQ.6)J6=I1;IF(J.EQ.7)J7=I1                                        ! гюонлхмюел онгхжхх пюгдекхрекеи
          END IF
          END DO 
          STR_DEPTH='                    '
            DO II=J6+1,J7-1                                                     ! бняярюмюбкхбюел йхкнлерп
            STR_DEPTH(II-J6:II-J6)=RAB_STRING1(II:II)
            END DO
STR_DEPTH=ADJUSTL(STR_DEPTH)
READ(STR_DEPTH,'(F8.4)') DEPTH1  
IF(DEPTH1.GT.DEPTH)DEPTH=DEPTH1
END DO
    DEPTH_MIDL=0.0
    JJ=0
      DO I=N_CHEN_TEK,i_GRAF,N_CHEN                                             ! бшахпюел йюмюк
      J=0;J6=0;J7=0
      WRITE(RAB_STRING1,'(A)')STRING_GRAF_CSV(I)
          DO I1=1,200                                                           ! юмюкхгхпсел ярпнйс
          IF(RAB_STRING1(I1:I1).EQ.';') THEN
          J=J+1                                                                 ! явервхй пюгдекхрекеи б ярпнйе
          IF(J.EQ.6)J6=I1;IF(J.EQ.7)J7=I1                                        ! гюонлхмюел онгхжхх пюгдекхрекеи
          END IF
          END DO 
          STR_DEPTH='                    '          
            DO II=J6+1,J7-1                                                     ! бняярюмюбкхбюел йхкнлерп
            STR_DEPTH(II-J6:II-J6)=RAB_STRING1(II:II)
            END DO
            STR_DEPTH=ADJUSTL(STR_DEPTH)
READ(STR_DEPTH,'(F8.4)') DEPTH1 
JJ=JJ+1
DEPTH_MIDL=DEPTH_MIDL+ DEPTH1
END DO
DEPTH_MIDL=DEPTH_MIDL/JJ  
DEPTH=DEPTH-DEPTH_MIDL          ! дкъ ндмнцн йюмюкю хлеер ялшяк сцксакемхъ
     END IF   
   
END
    
!----------------------------------------------------------------------------
! юмюкхг  детнплюжхи
! IF(N_CHEN.GE.3) CALL ANALIZ_UGLUB_DEFORM() 
!----------------------------------------------------------------------------
SUBROUTINE ANALIZ_UGLUB_DEFORM()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE UGLUB_CSV
IMPLICIT NONE
INTEGER(4) M,I,II
INTEGER(4)  K_FIRST,K_SEKOND
REAL(4)     M_FIRST,M_SEKOND,DEPTH,DEPTH1,DEPTH2
	
! юмюкхгхпсел пегскэрюрш брнпнцн йюмюкю
IF(N_WRITE_UGLUB(2).EQ.0) GOTO 2
II=0
DO 1 M=1, N_WRITE_UGLUB(2) 
CALL LINE_DEFORM(M,2,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH)     ! онъбхкюяэ мнбюъ ярпнйю он нях
WRITE(S1_U(2),'(F8.2)')DEPTH                                                         ! нопедекъел япедмчч цксахмс
CALL OBOCH(1,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH1)       ! хыел япедмчч цксахмс он нанвхмюл
CALL OBOCH(3,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH2)       ! хыел япедмчч цксахмс он нанвхмюл
DEPTH=DEPTH-(DEPTH1+DEPTH2)/2.
    IF(DEPTH.LE.0.1.AND.DEPTH.GT.0.05) THEN                                               ! аюккюярмне сцксакемхе 
II=II+1        
STRING_UGLUB_CSV(II,2)=TRIM(STRING_UGLUB_CSV(M,2))//';'//'АЮККЮЯРМНЕ СЦКСАКЕМХЕ'//';'//TRIM(S1_U(2))//';' 
GOTO 1
    END IF
    IF(DEPTH.LE.0.3.AND.DEPTH.GT.0.1) THEN                                                ! аюккюярмне йнпшрн

II=II+1        
STRING_UGLUB_CSV(II,2)=TRIM(STRING_UGLUB_CSV(M,2))//';'//'АЮККЮЯРМНЕ ЙНПШРН'//';'//TRIM(S1_U(2))//';'  
GOTO 1
    END IF      
    IF(DEPTH.LE.1.0.AND.DEPTH.GT.0.3) THEN                                                                 ! аюккюярмне кнфе
II=II+1        
STRING_UGLUB_CSV(M,2)=TRIM(STRING_UGLUB_CSV(M,2))//';'//'АЮККЮЯРМНЕ КНФЕ'//';'//TRIM(S1_U(2))//';'  
GOTO 1
    END IF 
    IF(DEPTH.GT.1.0) THEN                                                                 ! аюккюярмне кнфе
II=II+1        
STRING_UGLUB_CSV(M,2)=TRIM(STRING_UGLUB_CSV(M,2))//';'//'АЮККЮЯРМШИ ЛЕЬНЙ'//';'//TRIM(S1_U(2))//';'  
GOTO 1
    END IF     
1 CONTINUE
N_WRITE_UGLUB(2)=II
2 IF(N_WRITE_UGLUB(1).EQ.0) GOTO 3
II=0
DO 4 M=1, N_WRITE_UGLUB(1)                                                        
CALL LINE_DEFORM(M,1,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH)   ! онъбхкюяэ мнбюъ ярпнйю он нанвхме
WRITE(S1_U(1),'(F8.2)')DEPTH                                            ! нопедекъел япедмчч цксахмс
CALL OBOCH(2,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH1)   ! хыел япедмчч цксахмс он нанвхмюл
DEPTH=DEPTH-DEPTH1
    IF(DEPTH.LT.0.) GOTO 4 
    
    IF(DEPTH.LE.0.1.AND.DEPTH.GT.0.05.AND.DEPTH1.LT.1.) THEN                                               ! аюккюярмне сцксакемхе 
II=II+1        
STRING_UGLUB_CSV(II,1)=TRIM(STRING_UGLUB_CSV(M,1))//';'//'АЮККЮЯРМНЕ СЦКСАКЕМХЕ'//';'//TRIM(S1_U(1))//';' 
GOTO 4
    END IF
    
     IF(DEPTH.LE.0.3.AND.DEPTH.GT.0.05.AND.DEPTH1.LE.1.3.AND.DEPTH1.GT.1.0) THEN                                               ! аюккюярмне йнпшрн
II=II+1        
STRING_UGLUB_CSV(II,1)=TRIM(STRING_UGLUB_CSV(M,1))//';'//'АЮККЮЯРМНЕ ЙНПШРН'//';'//TRIM(S1_U(1))//';' 
GOTO 4
     END IF
     
     IF(DEPTH.LE.0.3.AND.DEPTH.GT.0.05.AND.DEPTH1.GT.1.0) THEN                                               ! аюккюярмне кнфе
II=II+1        
STRING_UGLUB_CSV(II,1)=TRIM(STRING_UGLUB_CSV(M,1))//';'//'АЮККЮЯРМНЕ КНФЕ'//';'//TRIM(S1_U(1))//';' 
GOTO 4
     END IF
     
    IF(DEPTH.GT.0.3) THEN                                                                                    ! аюккюярмне цмегдO
II=II+1        
STRING_UGLUB_CSV(II,1)=TRIM(STRING_UGLUB_CSV(M,1))//';'//'АЮККЮЯРМНЕ ЦМЕГДН'//';'//TRIM(S1_U(1))//';'  
GOTO 4
    END IF      
4   CONTINUE
N_WRITE_UGLUB(1)=II  

3 IF(N_WRITE_UGLUB(3).EQ.0) RETURN 
II=0  
DO 5 M=1, N_WRITE_UGLUB(3)                                                        
CALL LINE_DEFORM(M,3,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH)   ! онъбхкюяэ мнбюъ ярпнйю он нанвхме
WRITE(S1_U(3),'(F8.2)')DEPTH                                            ! нопедекъел япедмчч цксахмс
CALL OBOCH(2,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH1)   ! хыел япедмчч цксахмс он нанвхмюл
DEPTH=DEPTH-DEPTH1
    IF(DEPTH.LT.0.) GOTO 5 
    
    IF(DEPTH.LE.0.1.AND.DEPTH.GT.0.05.AND.DEPTH1.LT.1.) THEN                                               ! аюккюярмне сцксакемхе 
II=II+1        
STRING_UGLUB_CSV(II,3)=TRIM(STRING_UGLUB_CSV(M,3))//';'//'АЮККЮЯРМНЕ СЦКСАКЕМХЕ'//';'//TRIM(S1_U(3))//';' 
GOTO 5
    END IF
    
     IF(DEPTH.LE.0.3.AND.DEPTH.GT.0.05.AND.DEPTH1.LE.1.3.AND.DEPTH1.GT.1.0) THEN                                               ! аюккюярмне йнпшрн
II=II+1        
STRING_UGLUB_CSV(II,3)=TRIM(STRING_UGLUB_CSV(M,3))//';'//'АЮККЮЯРМНЕ ЙНПШРН'//';'//TRIM(S1_U(3))//';' 
GOTO 5
     END IF
     
     IF(DEPTH.LE.0.3.AND.DEPTH.GT.0.05.AND.DEPTH1.GT.1.0) THEN                                               ! аюккюярмне кнфе
II=II+1        
STRING_UGLUB_CSV(II,3)=TRIM(STRING_UGLUB_CSV(M,3))//';'//'АЮККЮЯРМНЕ КНФЕ'//';'//TRIM(S1_U(3))//';' 
GOTO 5
     END IF
     
    IF(DEPTH.GT.0.3) THEN                                                                                    ! аюккюярмне цмегдO
II=II+1        
STRING_UGLUB_CSV(II,3)=TRIM(STRING_UGLUB_CSV(M,3))//';'//'АЮККЮЯРМНЕ ЦМЕГДН'//';'//TRIM(S1_U(3))//';'  
GOTO 5
    END IF      
5   CONTINUE
N_WRITE_UGLUB(3)=II  
  
! аюккюярмше йнпшрю
! цпюмхжю пюгдекю он нях осрх хлеер онмхфемхе нр 10 ял дн 30 ял
! хкх
! дкхмю онмхфемхъ дн 3 у лерпнб

! аюккюярмше кнфю
! цпюмхжю пюгдекю он нях осрх хлеер онмхфемхе ябшье 30 ял
! дкхмю онмхфемхъ нр 3 у лерпнб дн 50 лерпнб
! цпюмхжю пюгдекю онкебнцн рнпжю бшье, вел он нях осрх
! хкх цпюмхжю пюгдекю мю лефдсосрмнл рнпже мхфе, вел он нях осрх

! аюккюярмше леьйх
! цпюмхжю пюгдекю он нях осрх нрмняхрекэмн цпюмхж он йпюъл хлеер сйкнм 1:10, цксахмю онмхфемхъ анкее 1 л
! дкхмю онмхфемхъ анкее 10 лерпнб

! аюккюярмше цмегдю
! рнкэйн мю бшянйху мюяшоъу (бшянрю опебшьюер 6 лерпнб)
! тнплхпсчряъ хг аюккюярмшу кнф х аюккюярмшу леьйнб опх мюкхвхх нрпнярйнб
END 

! бняярюмюбкхбюел йннпдхмюрш сцксакемхъ х 
! нопедекъел япедмее сцксакемхе мю йюмюке "лл" б гюохях "л"  
SUBROUTINE LINE_DEFORM(M,MM,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH)
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE TOLSH_CSV
IMPLICIT NONE
INTEGER I,I1,II,K1,K2,M,JJ,MM
CHARACTER*200 RAB_STRING
CHARACTER*200 RAB_STRING1
INTEGER(4) J,J3,J4,J5,J6,J7
INTEGER(4) L3,L4,L5,L6,L7,L8
INTEGER(4) KIL_UGLUB, K_FIRST,K_SEKOND
REAL(4)    METR_UGLUB,M_FIRST,M_SEKOND,DEPTH,DEPTH1,DEPTH_MIDL,DLINA
CHARACTER*20 STR_KL,STR_M,STR_DEPH


IF(N_WRITE_UGLUB(MM).EQ.0) RETURN  
                                                  
WRITE(RAB_STRING,'(A)')STRING_UGLUB_CSV(M,MM)                                 ! онъбхкюяэ мнбюъ ярпнйю
J=0 
    DO I1=1,200                                                                       ! юмюкхгхпсел ярпнйс
    IF(RAB_STRING(I1:I1).EQ.';') THEN
    J=J+1                                                                             ! явервхй пюгдекхрекеи б ярпнйе
    IF(J.EQ.3)L3=I1;IF(J.EQ.4)L4=I1;IF(J.EQ.5)L5=I1;IF(J.EQ.6)L6=I1;IF(J.EQ.7)L7=I1 ;IF(J.EQ.8)L8=I1  ! гюонлхмюел онгхжхх пюгдекхрекеи
    END IF
    END DO 
 
! бняярюмюбкхбюел цпюмхжш хмрепбюкю------------------------------------------------------------------------------------
    DO II=L3+1,L4-1                                                        ! бняярюмюбкхбюел йхкнлерп
    S1(II-L3:II-L3)=RAB_STRING(II:II)
    END DO
    READ(S1,'(I4)')K_FIRST    
    DO II=L4+1,L5-1                                                        ! бняярюмюбкхбюел лерп
    S2(II-L4:II-L4)=RAB_STRING(II:II)
    END DO
    READ(S2,'(F8.0)')M_FIRST
    DO II=L5+1,L6-1                                                        ! бняярюмюбкхбюел йхкнлерп
    S3(II-L5:II-L5)=RAB_STRING(II:II)
    END DO
    READ(S3,'(I4)')K_SEKOND    
    DO II=L6+1,L7-1                                                        ! бняярюмюбкхбюел лерп
    S4(II-L6:II-L6)=RAB_STRING(II:II)
    END DO
    READ(S4,'(F8.0)')M_SEKOND
!    DO II=L7+1,L8-1                                                        ! бняярюмюбкхбюел дкхмс
!    S5(II-L7:II-L7)=RAB_STRING(II:II)
 !   END DO
 !   READ(S5,'(F8.0)')DLINA
! хыел мнлеп ярпнйх б рюакхже цксахм -----------------------------------------------------------------------------
K1=0
K2=0
      DO I=MM,i_GRAF,N_CHEN                                             ! бшахпюел йюмюк
      J=0;J3=0;J4=0;J5=0
      WRITE(RAB_STRING1,'(A)')STRING_GRAF_CSV(I)
          DO I1=1,200                                                           ! юмюкхгхпсел ярпнйс
          IF(RAB_STRING1(I1:I1).EQ.';') THEN
          J=J+1                                                                 ! явервхй пюгдекхрекеи б ярпнйе
          IF(J.EQ.3)J3=I1;IF(J.EQ.4)J4=I1;IF(J.EQ.5)J5=I1                       ! гюонлхмюел онгхжхх пюгдекхрекеи
          END IF
          END DO
          STR_KL='                    '
            DO II=J3+1,J4-1                                                     ! бняярюмюбкхбюел йхкнлерп
            STR_KL(II-J3:II-J3)=RAB_STRING1(II:II)
            END DO      
            READ(STR_KL,'(I4)')KIL_UGLUB
            STR_M='                    '           
            DO II=J4+1,J5-1                                                     ! бняярюмюбкхбюел лерп
            STR_M(II-J4:II-J4)=RAB_STRING1(II:II)
            END DO
            READ(STR_M,'(F8.0)') METR_UGLUB 
IF(KIL_UGLUB.EQ.K_FIRST.AND.METR_UGLUB.EQ.M_FIRST)   K1=I
IF(KIL_UGLUB.EQ.K_SEKOND.AND.METR_UGLUB.EQ.M_SEKOND) THEN
K2=I; GOTO 1
END IF
JJ=I
      END DO
K2=JJ
1     DEPTH=0.0  
      JJ=0
IF(K1.NE.0.AND.K2.NE.0) THEN
        IF(K1.GT.K2) THEN
        J=K1;K1=K2;K2=J
        END IF
! нопедекъел япедмее сцксакемхе
      DO I=K1,K2,N_CHEN                                                         ! бшахпюел йюмюк
      J=0;J6=0;J7=0
      WRITE(RAB_STRING1,'(A)')STRING_GRAF_CSV(I)
          DO I1=1,200                                                           ! юмюкхгхпсел ярпнйс
          IF(RAB_STRING1(I1:I1).EQ.';') THEN
          J=J+1                                                                 ! явервхй пюгдекхрекеи б ярпнйе
         IF(J.EQ.6)J6=I1;IF(J.EQ.7)J7=I1                                        ! гюонлхмюел онгхжхх пюгдекхрекеи
          END IF
          END DO 
          STR_DEPH='                    '
            DO II=J6+1,J7-1                                                     ! бняярюмюбкхбюел йхкнлерп
            STR_DEPH(II-J6:II-J6)=RAB_STRING1(II:II)
            END DO
STR_DEPH=ADJUSTL(STR_DEPH)
READ(STR_DEPH,'(F8.0)') DEPTH1  
DEPTH=DEPTH+DEPTH1
JJ=JJ+1
END DO
DEPTH=DEPTH/JJ
END IF   
END   
 
! нопедекъел япедмее сцксакемхе мю йюмюке "л" б сйюгюммнл хмрепбюке 

SUBROUTINE OBOCH(M,K_FIRST,M_FIRST,K_SEKOND,M_SEKOND,DEPTH)
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE TOLSH_CSV
IMPLICIT NONE

CHARACTER*20 STR_KL,STR_M,STR_DEPH
INTEGER I,I1,II,K1,K2,M,JJ
CHARACTER*200 RAB_STRING
CHARACTER*200 RAB_STRING1
INTEGER(4) J,J3,J4,J5,J6,J7
INTEGER(4) L3,L4,L5,L6,L7,L8
INTEGER(4) KIL_UGLUB, K_FIRST,K_SEKOND
REAL(4)    METR_UGLUB,M_FIRST,M_SEKOND,DEPTH,DEPTH1                                             

! хыел мнлеп ярпнйх б рюакхже цксахм -----------------------------------------------------------------------------
K1=0
K2=0
      DO I=M,i_GRAF,N_CHEN                                             ! бшахпюел йюмюк
      J=0;J3=0;J4=0;J5=0
      WRITE(RAB_STRING1,'(A)')STRING_GRAF_CSV(I)
          DO I1=1,200                                                           ! юмюкхгхпсел ярпнйс
          IF(RAB_STRING1(I1:I1).EQ.';') THEN
          J=J+1                                                                  ! явервхй пюгдекхрекеи б ярпнйе
          IF(J.EQ.3)J3=I1;IF(J.EQ.4)J4=I1;IF(J.EQ.5)J5=I1                         ! гюонлхмюел онгхжхх пюгдекхрекеи
          END IF
          END DO
          STR_KL='                     '
          DO II=J3+1,J4-1                                                        ! бняярюмюбкхбюел йхкнлерп
            STR_KL(II-J3:II-J3)=RAB_STRING1(II:II)
            END DO      
            READ(STR_KL,'(I4)')KIL_UGLUB
           STR_M='                    '
            DO II=J4+1,J5-1                                                        ! бняярюмюбкхбюел лерп
            STR_M(II-J4:II-J4)=RAB_STRING1(II:II)
            END DO
            READ(STR_M,'(F8.0)') METR_UGLUB 
IF(KIL_UGLUB.EQ.K_FIRST.AND.METR_UGLUB.EQ.M_FIRST)   K1=I
IF(KIL_UGLUB.EQ.K_SEKOND.AND.METR_UGLUB.EQ.M_SEKOND) THEN
K2=I; GOTO 1
END IF
      END DO
1     CONTINUE
      IF(K1.NE.0.AND.K2.NE.0) THEN
        IF(K1.GT.K2) THEN
        JJ=K1;K1=K2;K2=JJ
        END IF
DEPTH=0.0 
JJ=0
! нопедекъел люйяхлюкэмне сцксакемхе
      DO I=K1,K2,N_CHEN                                                         ! бшахпюел йюмюк
      J=0;J6=0;J7=0
      WRITE(RAB_STRING1,'(A)')STRING_GRAF_CSV(I)
          DO I1=1,200                                                           ! юмюкхгхпсел ярпнйс
          IF(RAB_STRING1(I1:I1).EQ.';') THEN
          J=J+1                                                                 ! явервхй пюгдекхрекеи б ярпнйе
         IF(J.EQ.6)J6=I1;IF(J.EQ.7)J7=I1                                        ! гюонлхмюел онгхжхх пюгдекхрекеи
          END IF
          END DO   
          STR_DEPH='                    '
            DO II=J6+1,J7-1                                                     ! бняярюмюбкхбюел йхкнлерп
            STR_DEPH(II-J6:II-J6)=RAB_STRING1(II:II)
            END DO
STR_DEPH=ADJUSTL(STR_DEPH)
READ(STR_DEPH,'(F8.0)') DEPTH1 
IF(DEPTH1.GT.DEPTH)DEPTH=DEPTH1
!DEPTH=DEPTH+DEPTH1
!JJ=JJ+1
END DO
!DEPTH=DEPTH/JJ
END IF   
   
END    
    
!open(unit=100, FILE='D:/My_fortran_proects_tvema/OUTPUT.OUT')
!write(100,*) HH(ii_bal(N_CHEN_TEK))*.01	
