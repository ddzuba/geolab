! ---------------------------------------------------------------------------------------
! нопедекемхе оюпюлерпнб дкъ пюяверю оюпюлерпнб нямнбмни окныюдйх
! ---------------------------------------------------------------------------------------

SUBROUTINE PARAM_400()						 
USE PARAM_
USE N_	
use GeolibGlobals									
EXTERNAL INDATA2							! опнжедспю, ябъгюммюъ я дюммшл дхюкнцнл
FLAG=DLGINITWITHRESOURCEHANDLE(GRAUND_400, dllHandler, PARAMETR2)				! хмхжхюкхгюжхъ дхюкнцю
IF(.NOT.FLAG)STOP'GRAUND_400 NOT FOUND'

FLAG=DLGSETSUB(PARAMETR2,C1,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C2,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C3,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C4,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C5,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C6,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C7,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C8,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,C9,INDATA2)

FLAG=DLGSETSUB(PARAMETR2,YES,INDATA2)
FLAG=DLGSETSUB(PARAMETR2,NO,INDATA2)
	
IDELTA=SQRT(PREL)/T_RAZV/0.3		! йнмярюмрю дкъ опнбедемхъ кхмхи я хмрепбюкнл б 0.5 лерпю

N_START=j_level+idelta*1.5+1		! бепумъъ цпюмхжю 
N_FINISH=j_level+idelta*3			! мхфмъъ цпюмхжю 
LM_WINDOW=50						! вхякн рпюяя б нйме
N_MIDL_U=50

R_POSIB=    0.3			! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ бкюфмнярх
R_POWER=    10.			! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ сцкю мюйкнмю
R_SCIN =    0.4			! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ якнхярнярх
R_MOMENT  = 0.7			! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ детнплюрхбмнярх
R_BALLAST	  = 0.1			! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ аюккюярмшу сцксакемхи
									
WRITE(STRING,*)N_START						! опенапюгсел вхякн б ярпнйс
FLAG=DLGSET(PARAMETR2,C1,ADJUSTL(STRING))	! нрпюфюел б нйме

WRITE(STRING,*)N_FINISH						! опенапюгсел вхякн б ярпнйс
FLAG=DLGSET(PARAMETR2,C2,ADJUSTL(STRING))	! нрпюфюел б нйме

WRITE(STRING,*)LM_WINDOW					! опенапюгсел вхякн б ярпнйс
FLAG=DLGSET(PARAMETR2,C3,ADJUSTL(STRING))	! нрпюфюел б нйме

WRITE(STRING,*)N_MIDL_U						! опенапюгсел вхякн б ярпнйс
FLAG=DLGSET(PARAMETR2,C4,ADJUSTL(STRING))	! нрпюфюел б нйме

WRITE(STRING,*)R_POSIB						! опенапюгсел вхякн б ярпнйс
FLAG=DLGSET(PARAMETR2,C5,ADJUSTL(STRING))	! нрпюфюел б нйме

WRITE(STRING,*)R_POWER						! опенапюгсел вхякн б ярпнйс
FLAG=DLGSET(PARAMETR2,C6,ADJUSTL(STRING))	! нрпюфюел б нйме

WRITE(STRING,*)R_SCIN						! опенапюгсел вхякн б ярпнйс
FLAG=DLGSET(PARAMETR2,C7,ADJUSTL(STRING))	! нрпюфюел б нйме

WRITE(STRING,*)R_MOMENT						! опенапюгсел вхякн б ярпнйс
FLAG=DLGSET(PARAMETR2,C8,ADJUSTL(STRING))	! нрпюфюел б нйме

WRITE(STRING,*)R_BALLAST					! опенапюгсел вхякн б ярпнйс
FLAG=DLGSET(PARAMETR2,C9,ADJUSTL(STRING))	! нрпюфюел б нйме

STATUS=DLGMODAL(PARAMETR2)					! нрнапюфемхе дхюкнцю

END SUBROUTINE PARAM_400
!---------------------------------------------------------------
!---------------------------------------------------------------
SUBROUTINE INDATA2(FILE_DLG,C_NAME,CBTYPE)	! оепедювю дюммшу хг онкеи дхюкнцю
USE PARAM_
TYPE(DIALOG)::FILE_DLG
INTEGER*4 ::C_NAME,CBTYPE, I
I=CBTYPE									! акнйхпнбйю рпюмякърнпю
FLAG=DLGGET(FILE_DLG,C_NAME,STRING)
	
IF(C_NAME.EQ.YES) THEN
FLAG=DLGGET(FILE_DLG,C1,STRING)				! пюгбепрйю
READ(string,*)N_START						! бепумъъ цпюмхжю ыеамъ нр спнбмъ бшпюбмхбюмхъ
FLAG=DLGGET(FILE_DLG,C2,STRING)
READ(string,*)N_FINISH						! мхфмъъ цпюмхжю ыеамъ нр спнбмъ бшпюбмхбюмхъ		
FLAG=DLGGET(FILE_DLG,C3,STRING)
READ(string,*)LM_WINDOW						! вхякн рпюяя б нйме	
FLAG=DLGGET(FILE_DLG,C4,STRING)
READ(string,*) N_MIDL_U						! вхякн рпюяя опх сяпедмемхх оюпюлерпнб	
	N_MIDL_R0=N_MIDL_U						! вхякн рпюяя опх сяпедмемхх оюпюлерпнб	
	N_MIDL_RMOM=N_MIDL_U					! вхякн рпюяя опх сяпедмемхх оюпюлерпнб	
	N_TAN=N_MIDL_U							! вхякн рпюяя опх сяпедмемхх оюпюлерпнб	
	NTRS=N_MIDL_U							! вхякн рпюяя дкъ сяпедмемхъ
	N_MIDL_BALL=N_MIDL_U					! вхякн рпюяя дкъ сяпедмемхъ онкнфемхъ цпюмхжш аюккюярю
	IF(LM_WINDOW.GT.NTRS)LM_WINDOW=NTRS
FLAG=DLGGET(FILE_DLG,C5,STRING)
READ(string,*) R_POSIB						! вхякн рпюяя б нйме			
FLAG=DLGGET(FILE_DLG,C6,STRING)
READ(string,*) R_POWER						! вхякн рпюяя б нйме			
FLAG=DLGGET(FILE_DLG,C7,STRING)
READ(string,*) R_SCIN						! вхякн рпюяя б нйме			
FLAG=DLGGET(FILE_DLG,C8,STRING)
READ(string,*) R_MOMENT						! вхякн рпюяя б нйме			
FLAG=DLGGET(FILE_DLG,C9,STRING)
READ(string,*) R_BALLAST					! вхякн рпюяя б нйме			
CALL DLGEXIT(FILE_DLG)						! гюйпшбюел дхюкнц
! нопедекъел цпюмхжш бекхвхм
R_POSIB1=1+R_POSIB							! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ бкюфмнярх
R_POSIB=1-R_POSIB
R_POWER=-ABS(R_POWER)
R_POWER1=-R_POWER							! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ сцкю мюйкнмю
R_SCIN1=1+R_SCIN							! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ якнхярнярх
R_SCIN=1-R_SCIN
R_MOMENT1=0.9+R_MOMENT						! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ детнплюрхбмнярх
R_MOMENT=0.9-R_MOMENT
R_BALLAST=1+R_BALLAST
END IF

IF(C_NAME.EQ.NO) THEN
STOP				
END IF

END SUBROUTINE INDATA2 