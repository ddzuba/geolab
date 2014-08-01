!----------------------------------------------------------------------------------------------------------
! хыел рпюяяш, мюханкее опхакхфеммше й йхкнлерпнбшл ярнкаюл
!----------------------------------------------------------------------------------------------------------
SUBROUTINE TAB()		
USE DFLIB
USE KOORDINATA
USE INPUT_RADAROGRAMMA 
USE DFWINA

USE GLOBALS_LOG
IMPLICIT NONE
	INTEGER(4) I,J,JMIN !,I1,I_KM,I_KM2
	INTEGER(4) IPR_1,IPR_2
	REAL(8) RAB1,RAB2

! явхршбюел хмтнплюжхч н йннпдхмюрюу йхкнлерпнбшу ярнканб
 	REWIND(NN2)								! сярюмюбкхбюел люпйеп мю мювюкн тюикю 
    DO I=1,IPOINT1
	READ(NN2,*)KLM(I),SHIR(I),DLGT(I)	! явхршбюел рейярнбсч ярпнйс
    END DO
    
! опнбепъел нрмняхрекэмше пюяярнъмхъ лефдс йхкнлерпнбшлх ярнкаюлх
	R_STOLB (1)=0.D0
	DO I=2,IPOINT1
	R_STOLB(I)=RABZEM*2.D0*DASIN(DSQRT(DSIND((SHIR(I)-SHIR(I-1))/2)**2+DCOSD(SHIR(I))*DCOSD(SHIR(I-1))*DSIND((DLGT(I)-DLGT(I-1))/2)**2))
    END DO

! ббндхл осрэ й тюикс, яндепфюыелс дкхмс охйернб
        FF1='Cfreceiver.cfg'
        FF2='STOLB_LENCH_FILE'
        FF3='ZERO'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        STOLB_LENCH_FILE=FF2
    IF(STOLB_LENCH_FILE.NE.'ZERO') THEN	
    NN3=300
 	OPEN(UNIT=NN3,FILE=STOLB_LENCH_FILE,ERR=1,STATUS='REPLACE') ! нрйпшрхе тюикю дюммшу
    GOTO 2
1   MESS=MESSAGEBOX(NULL,"оПЕПБЮРЭ ПЮАНРС - дЮ, оПНДНКФХРЭ ПЮАНРС АЕГ ГЮОХЯХ - мЕР"C,"тЮИК ДКЪ ГЮОХЯХ ДКХМ ЛЕФДС ОХЙЕРМШЛХ ЯРНКАЮЛХ МЕ НРЙПШР"C,4)    ! еЯКХ МЕ НРЙПШБЮЕЛ ТЮИК, РН
	IF(MESS.EQ.6) THEN                                                                              ! гЮЙЮМВХБЮЕЛ ПЮАНРС
    STOP                                ! опепшбюел пюанрс
    ELSE
    GOTO 3                                                                                          ! хКХ ОНБРНПЪЕЛ ОНОШРЙС
    END IF
2   WRITE(NN3,*) 'опнбепйю пюяярнъмхъ лефдс '
	WRITE(NN3,*) 'йхкнлерпнбшлх ярнкаюлх бднкэ осрх'
	WRITE(NN3,*) ''
	WRITE(NN3,*) 'мнлеп ярнкаю, йхкнлерп, пюяярнъмхе '
	DO I=1,IPOINT1
	WRITE(NN3,101) I,KLM(I),R_STOLB(I)	! оевюрюел пюяярнъмхе лефдс йхкнлерпнбшлх ярнкаюлх
	END DO
101	FORMAT(I5,7X,F10.3,2X,F10.3)
	WRITE(30,*) '-----------------------------------'
    END IF
! нопедекъел оепейпшбючряъ кх йннпдхмюрш рпюяяш х йхкнлерпнбшу ярнканб 
! хыел рпюяяш, мюханкее опхакхфеммше й оепбнлс х IPOINT1 йхкнлерпнбшл ярнкаюл
3	I=1	! бшвхякъел пюяярнъмхе лефдс йхкнлерпнбшлх ярнкаюлх
4   DO J=1,NTR				! х рпюяяюлх х гюохяшбюел б люяяхб RAB(J)
RAB(J)=RABZEM*2.D0*DASIN(DSQRT(DSIND((SHIR(I)-SHIROTA_TRASSA(J))/2)**2+DCOSD(SHIR(I))*DCOSD(SHIROTA_TRASSA(J))&
*DSIND((DLGT(I)-DOLGOTA_TRASSA(J))/2)**2))
		END DO
	IF(I.EQ.1.AND.RAB(1).GT.RAB(NTR))IPR_1=0       ! опхакхфюеляъ й оепбнлс ярнкас
	IF(I.EQ.1.AND.RAB(1).LT.RAB(NTR))IPR_1=1       ! сдюкъеляъ нр оепбнцн ярнкаю
	IF(I.EQ.IPOINT1.AND.RAB(1).GT.RAB(NTR))IPR_2=0 ! опхакхфюеляъ й онякедмелс ярнкас
	IF(I.EQ.IPOINT1.AND.RAB(1).LT.RAB(NTR))IPR_2=1 ! сдюкъеляъ нр онякедмецн ярнкаю
		RAB1=RAB(1)					! мюундхл лхмхлсл нрйкнмемхъ
		JMIN=1
		DO J=2,NTR
		IF(RAB(J).LT.RAB1) THEN
		RAB1=RAB(J)
		JMIN=J		! гюохяшбюел мнлеп рпюяяш я лхмхлслнл нрйкнмемхъ
		END IF
		END DO
	IF(I.EQ.1.AND.DABS(RAB(JMIN)-RAB(1)).GT.15.AND.DABS(RAB(JMIN)-RAB(NTR)).GT.15)IPR_1=2           ! опхакхфюеляъ
	IF(I.EQ.IPOINT1.AND.DABS(RAB(JMIN)-RAB(1)).GT.15.AND.DABS(RAB(JMIN)-RAB(NTR)).GT.15)IPR_2=2		! опхакхфюеляъ
	IF(I.EQ.1)THEN
	I=IPOINT1
	GOTO 4
    END IF
    
	IF(IPR_1.EQ.0.AND.IPR_2.EQ.0.OR.IPR_1.EQ.1.AND.IPR_2.EQ.1) THEN 
    MESS=MESSAGEBOX(NULL,"оПЕПБЮРЭ ПЮАНРС ПЮАНРС - дЮ"C,"ьЙЮКШ ЙННПДХМЮР ЙХКНЛЕРПНБШУ ЯРНКАНБ Х ПЮДЮПНЦПЮЛЛ МЕ ЯНБОЮДЮЧР"C,0)    
        IF(MESS.EQ.1) THEN                                                                              ! гЮЙЮМВХБЮЕЛ ПЮАНРС
        STOP                                                                                        
        END IF
    END IF
	RETURN
	END

