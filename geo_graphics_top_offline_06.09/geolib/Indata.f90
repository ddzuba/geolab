! ---------------------------------------------------------------------------------------
! нопедекемхе нямнбмшу оюпюлерпнб оюпюлерпнб дкъ пюяверю 
! ---------------------------------------------------------------------------------------
SUBROUTINE INDATA(FILE_DLG,C_NAME,CBTYPE)			! оепедювю дюммшу хг онкеи дхюкнцю
USE PARAM_
USE PARAM_1
use FReciverGlobals
USE RAILWAY
USE N_										

TYPE(DIALOG)::FILE_DLG
INTEGER(4)::C_NAME,CBTYPE, I
I=CBTYPE												! акнйхпнбйю рпюмякърнпю

FLAG=DLGGET(FILE_DLG,C_NAME,STRING)

SELECT CASE(C_NAME)
CASE(CHECK0)								! сйюгшбюел яоняна тнплхпнбюмхъ оюонй
	IF(K_FLAG.EQ.0) THEN
	FLAG=DLGSET(FILE_DLG,A21,.TRUE.)
	FLAG=DLGSET(FILE_DLG,A17,.TRUE.)	
	FLAG=DLGSET(FILE_DLG,A18,.TRUE.)	
	FLAG=DLGSET(FILE_DLG,A19,.TRUE.)	
	K_FLAG=1								! опхмсдхрекэмне тнплхпнбюмхе оюонй
	ELSE
	FLAG=DLGSET(FILE_DLG,A21,.FALSE.)
	FLAG=DLGSET(FILE_DLG,A17,.FALSE.)	
	FLAG=DLGSET(FILE_DLG,A18,.FALSE.)	
	FLAG=DLGSET(FILE_DLG,A19,.FALSE.)	
	K_FLAG=0								! юбрнлюрхвеяйне тнплхпнбюмхе оюонй
	END IF
CASE(CHECK1)								! сйюгшбюел гюопер гюохях пхясмйнб
	IF(IFL_WRITER.EQ.1) THEN
	IFL_WRITER=0							! йкчв гюохях тюикнб мю дхяй (0 - гюохях мер)
	ELSE
	IFL_WRITER=1							! йкчв гюохях тюикнб мю дхяй (0 - гюохях мер)
	END IF
CASE(CHECK2)								! сйюгшбюел гюопер гюохях рейярнбшу тюикнб
	IF(IFL_WRITER_KASKAD.EQ.1) THEN
	IFL_WRITER_KASKAD=0						! гюохяэ дкъ йюяйюдю ме опнхгбндхрэ
	ELSE
	IFL_WRITER_KASKAD=1						! гюохяэ дкъ йюяйюдю  опнхгбндхрэ
	END IF

CASE(CHECK3)
	IF(I_PRIZNAK.EQ.0) THEN
	I_PRIZNAK=1								! пюанвюъ йнмярюмрю дкъ ондцнрнбйх тюикю гюохях хяундмшу дюммшу
	ELSE
	I_PRIZNAK=0								! пюанвюъ йнмярюмрю дкъ ондцнрнбйх тюикю гюохях хяундмшу дюммшу
	END IF

END SELECT

IF(C_NAME.EQ.YES) THEN
	IF(K_FLAG.EQ.1) THEN		! тнплхпсел оюойх 
		FLAG=DLGGET(FILE_DLG,A21,STRING)	
		READ(STRING,*)N_DEVICE
		FLAG=DLGGET(FILE_DLG,A17,STRING)	
		READ(STRING,*)N_VAY
		FLAG=DLGGET(FILE_DLG,A18,STRING)	
		READ(STRING,*)N_DIRECTION
		FLAG=DLGGET(FILE_DLG,A19,STRING)	
		READ(STRING,*)N_TRACK
!		ELSE
!		N_DEVICE(1:50)=   '                                                  '
!		N_VAY(1:50)=      '                                                  '
!		N_DIRECTION(1:50)='                                                  '
!		N_TRACK(1:50)=    '                                                  '

!		тнплхпсел опхмсдхрекэмше хлемю оюонй
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		PAPKA=NAME_FILE_IMG
		WRITE(STRING,*)	PAPKA	
		FLAG=DLGSET(PARAMETR,A33,ADJUSTL(STRING))	! нрпюфюел б нйме 
		PAPKA1=NAME_FILE_GEO
		WRITE(STRING,*)	PAPKA1	
		FLAG=DLGSET(PARAMETR,A39,ADJUSTL(STRING))	! нрпюфюел б нйме 
		K_FLAG=3									! дкъ опнбепйх тнплхпнбюмхъ оюонй
		RETURN
	END IF
! явхршбюел оюпюлерпш
	FLAG=DLGGET(FILE_DLG,A1,STRING)	
	READ(STRING,*)IANTENNA
	FLAG=DLGGET(FILE_DLG,A16,STRING)	
	READ(STRING,*)N_CHEN
	FLAG=DLGGET(FILE_DLG,A2,STRING)
	READ(STRING,*)J_LEVEL
	IF(J_LEVEL.LT.50.OR.J_LEVEL.GT.200)J_LEVEL=70	! цпюмхжш дхюонгнмю бшанпю спнбмъ
	FLAG=DLGGET(FILE_DLG,A3,STRING)
	READ(STRING,*)POROG
	FLAG=DLGGET(FILE_DLG,A4,STRING)
	READ(STRING,*)POROG1
	FLAG=DLGGET(FILE_DLG,A5,STRING)
	READ(STRING,*)POROG2
	FLAG=DLGGET(FILE_DLG,A20,STRING)
	READ(STRING,*)T_RAZV
	FLAG=DLGGET(FILE_DLG,A7,STRING)
	READ(STRING,*)M_REGION
	FLAG=DLGGET(FILE_DLG,A8,STRING)
	M2_REGION=M_REGION/2						! онкнбхмю бшянрю нймю сяпедмемхъ
	READ(STRING,*)L_REGION
	FLAG=DLGGET(FILE_DLG,A12,STRING)
	READ(STRING,*)HH_B
	FLAG=DLGGET(FILE_DLG,A11,STRING)
	READ(STRING,*)PREL
	FLAG=DLGGET(FILE_DLG,A13,STRING)
	READ(STRING,*)N_MIDL_U
!	FLAG=DLGGET(FILE_DLG,A24,STRING)				! пюгбепрйю
!	READ(string,*)N_START						! бепумъъ цпюмхжю ыеамъ нр спнбмъ бшпюбмхбюмхъ
!	FLAG=DLGGET(FILE_DLG,A25,STRING)
!	READ(string,*)N_FINISH						! мхфмъъ цпюмхжю ыеамъ нр спнбмъ бшпюбмхбюмхъ		
	FLAG=DLGGET(FILE_DLG,A26,STRING)
	READ(string,*)LM_WINDOW						! вхякн рпюяя б нйме	
	N_MIDL_RMOM=N_MIDL_U					! вхякн рпюяя опх сяпедмемхх оюпюлерпнб	
	N_MIDL_BALL=N_MIDL_U					! вхякн рпюяя дкъ сяпедмемхъ онкнфемхъ цпюмхжш аюккюярю
!	IF(LM_WINDOW.GT.L_REGION)LM_WINDOW=L_REGION
	FLAG=DLGGET(FILE_DLG,A27,STRING)
	READ(string,*) R_POSIB						! вхякн рпюяя б нйме			
	FLAG=DLGGET(FILE_DLG,A28,STRING)
!	READ(string,*) R_POWER						! вхякн рпюяя б нйме			
	FLAG=DLGGET(FILE_DLG,A29,STRING)
!	READ(string,*) R_SCIN						! вхякн рпюяя б нйме			
	FLAG=DLGGET(FILE_DLG,A30,STRING)
	READ(string,*) R_MOMENT						! вхякн рпюяя б нйме			
	FLAG=DLGGET(FILE_DLG,A31,STRING)
	READ(string,*) R_BALLAST					! вхякн рпюяя б нйме			
! нопедекъел цпюмхжш бекхвхм
R_POSIB1=1+R_POSIB							! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ бкюфмнярх
R_POSIB=1-R_POSIB
R_POWER=-ABS(R_POWER)
R_POWER1=-R_POWER							! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ сцкю мюйкнмю
!R_SCIN1=1+R_SCIN							! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ якнхярнярх
!R_SCIN=1-R_SCIN
R_MOMENT1=0.9+R_MOMENT						! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ детнплюрхбмнярх
R_MOMENT=0.9-R_MOMENT
R_BALLAST=1+R_BALLAST
	FLAG=DLGGET(FILE_DLG,A22,STRING)
	READ(STRING,*)N_BIT
	IF(N_BIT.EQ.2.OR.N_BIT.EQ.4) THEN
	GOTO 777
	ELSE
	N_BIT=0
	WRITE(STRING,*)N_BIT						! опенапюгсел вхякн б ярпнйс
	FLAG=DLGSET(FILE_DLG,A22,ADJUSTL(STRING))	! нрпюфюел б нйме
	RETURN
	END IF
777	CALL POINT_ARRAY() ! пюглеыюел пюанвхе люяяхбш

!---------------------------------------------------------------------------
! нрйпшрхе оюойх напюгнб 
!---------------------------------------------------------------------------
IF(IFL_WRITER.EQ.0) GOTO 776
	FLAG=DLGGET(FILE_DLG,A32,STRING)
	READ(STRING,*)DISK
	FLAG=DLGGET(FILE_DLG,A33,STRING)
	READ(STRING,*)PAPKA
	FLAG=DLGGET(FILE_DLG,A34,STRING)
	READ(STRING,*)FILE_TEXT
	FLAG=DLGGET(FILE_DLG,A35,STRING)
	READ(STRING,*)IFLAG_PRINT
	FLAG=DLGGET(FILE_DLG,A36,STRING)
	READ(STRING,*)N_PRINT
		IF(N_PRINT.LT.1)N_PRINT=1
		IF(N_PRINT.GT.50) N_PRINT=50
		IF(N_PRINT.LE.10)THEN
		NFONT=16;JSDVIG=25
		END IF
		IF(N_PRINT.LE.20.AND.N_PRINT.GT.10)THEN
		NFONT=14;JSDVIG=20
		END IF
		IF(N_PRINT.LE.30.AND.N_PRINT.GT.20)THEN
		NFONT=12;JSDVIG=15
		END IF
		IF(N_PRINT.LE.40.AND.N_PRINT.GT.30)THEN
		NFONT=10;JSDVIG=12
		END IF
		IF(N_PRINT.LE.50.AND.N_PRINT.GT.40)THEN
		NFONT=8;JSDVIG=10
		END IF
	FLAG=DLGGET(FILE_DLG,A37,STRING)
	READ(STRING,*)img_w
	FLAG=DLGGET(FILE_DLG,A38,STRING)
	READ(STRING,*)img_h
	
!--------------------------------------------------------------------
	lret = make_folder(DISK,PAPKA) ! янгдюел оюойс
!--------------------------------------------------------------------
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/')

	I_LENTH=LEN_TRIM(NAME_FILE)		!  дкхмю хлемх
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(FILE))

	OPEN(UNIT=40,FILE=NAME_FILE ,FORM='BINARY',ERR=1,STATUS='REPLACE') ! нрйпшрхе тюикю дюммшу
	GOTO 2
1	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'нЬХАЙЮ Б ГЮДЮМХХ ТЮИКЮ ДЮММШУ')
!	CALL PAPKA_CREATE(PAPKA) 
	RETURN
!	FLAG_NAME_IMG=1
2	CLOSE(40,STATUS='DELETE')	! гюйпшрхе тюикю дюммшу
! тюик лшьх -----------------------------------------------------------------
	IFL_WRITER_TEXT=1		! йкчв гюохях тюикнб мю дхяй (1 - гюохяэ нясыеярбхрэ)
	FILE_TEXT=TRIM(FILE_TEXT)//'.DAT'
	NAME_FILE_TEXT=TRIM(TRIM(DISK)//&
     '/'//TRIM(PAPKA)//'/'//TRIM(FILE_TEXT))

	OPEN(UNIT=15,FILE=NAME_FILE_TEXT,ERR=3,STATUS='REPLACE') ! нрйпшрхе тюикю дюммшу
	WRITE(15,100)

	GOTO 4
3	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'нЬХАЙЮ Б ГЮДЮМХХ ТЮИКЮ ДЮММШУ')
!	CALL PAPKA_CREATE(PAPKA_TEXT) 
	RETURN
4	CLOSE(15)	! гюйпшрхе тюикю дюммшу
100	FORMAT(1X,'мНЛЕП',4X,'йХКНЛЕРП',6x,'  лЕРП  ' )
!---------------------------------------------------------------------------
! нрйпшрхе оюойх рейярнбшу тюикнб 
!---------------------------------------------------------------------------
776 IF(IFL_WRITER_KASKAD.EQ.0)GOTO 775
	FLAG=DLGGET(FILE_DLG,A39,STRING)
	READ(STRING,*)PAPKA
	FLAG=DLGGET(FILE_DLG,A40,STRING)
	READ(STRING,*)NP_PRINT
!	FLAG=DLGGET(DLG,F5,STRING)
!	READ(STRING,*)FILE_DAT
	FLAG=DLGGET(FILE_DLG,A41,STRING)
	READ(STRING,*)GRAF
	FLAG=DLGGET(FILE_DLG,A42,STRING)
	READ(STRING,*)VLAG
	FLAG=DLGGET(FILE_DLG,A43,STRING)
	READ(STRING,*)UGLUB
	FLAG=DLGGET(FILE_DLG,A44,STRING)
	READ(STRING,*)INFO
!-----------------------------------------------------------------------
	lret = make_folder(DISK,PAPKA) ! янгдюел оюойс
!----------------------------------------------------------------------
DO N_CHEN_TEK=1,N_CHEN						! мнлеп бшапюммнцн йюмюкю
	WRITE(STRING_CHEN, '(i1)') N_CHEN_TEK	! гЮОХЯШБЮЕЛ МНЛЕП ЙЮМЮКЮ Б ЯРПНЙС
!-----------------------------------------------------------------------
! тюик GRAF
	NAME_GRAF=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(GRAF)//'_'//TRIM(STRING_CHEN)//'.csv')
	OPEN(UNIT=10,FILE=NAME_GRAF,ERR=5,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	GOTO 6
5	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'нЬХАЙЮ Б ГЮДЮМХХ ТЮИКЮ ЯКНЕБ')
	RETURN
!	FLAG_NAME_GEO=1
6	STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; йхкнлерп; лерп; цксахмю оепбнцн якнъ; цксахмю брнпнцн якнъ')
	WRITE(10,101) STRING_200
	GRAF_NUMBER(N_CHEN_TEK)=NAME_GRAF ! гюонлхмюел хлемю
	CLOSE(10)	! гюйпшрхе тюикю дюммшу


!----------------------------------------------------------------------
! тюик VLAG
	NAME_VLAG=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(VLAG)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=20,FILE=NAME_VLAG,ERR=7,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	GOTO 8
7	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'нЬХАЙЮ Б ГЮДЮМХХ ТЮИКЮ БКЮФМНЯРЕИ')
	RETURN
8	STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; мювюкн йхкнлерп; мювюкн лерп; нйнмвюмхе йхкнлерп; нйнмвюмхе лерп; опнръфеммнярэ; бекхвхмю')
	WRITE(20,101) STRING_200
	VLAG_NUMBER(N_CHEN_TEK)=NAME_VLAG ! гюонлхмюел хлемю
	CLOSE(20)	! гюйпшрхе тюикю дюммшу
!----------------------------------------------------------------------
! тюик UGLUB
	NAME_UGLUB=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(UGLUB)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=30,FILE=NAME_UGLUB,ERR=9,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	GOTO 10
9	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'нЬХАЙЮ Б ГЮДЮМХХ ТЮИКЮ АЮККЮЯРМШУ СЦК.')
	RETURN
10	STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; мювюкн йхкнлерп; мювюкн лерп; нйнмвюмхе йхкнлерп; нйнмвюмхе лерп;опнръфеммнярэ;уюпюйрепхярхйю; бекхвхмю')
	WRITE(30,101) STRING_200
	UGLUB_NUMBER(N_CHEN_TEK)=NAME_UGLUB ! гюонлхмюел хлемю
	CLOSE(30)	! гюйпшрхе тюикю дюммшу
! тюик TOLSH 
	NAME_TOLSH=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(TOLSH)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=31,FILE=NAME_TOLSH,ERR=13,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	GOTO 14  
13  RETURN
14  STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; мювюкн йхкнлерп; мювюкн лерп; нйнмвюмхе йхкнлерп; нйнмвюмхе лерп;опнръфеммнярэ; бекхвхмю')
	WRITE(31,101) STRING_200
	TOLSH_NUMBER(N_CHEN_TEK)=NAME_TOLSH ! гюонлхмюел хлемю
	CLOSE(31)	! гюйпшрхе тюикю дюммшу 
!----------------------------------------------------------------------
! тюик INFO
	NAME_INFO=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(INFO)//'.xml')
    OPEN(UNIT=8,FILE=NAME_INFO,ERR=11,STATUS='REPLACE')  ! нрйпшрхе тюикю дюммшу
	GOTO 12
11	FLAG=DLGSET(FILE_DLG,ERROR_FILE,'нЬХАЙЮ Б ГЮДЮМХХ ТЮИКЮ ХМТНПЛЮЖХХ')
	RETURN
12	CALL INFO_XML()
	CLOSE(8)	! гюйпшрхе тюикю дюммшу
END DO
!--------------------------------------------------------------------------
! тнплхпсел тюик хяундмшу дюммшу
IF(I_PRIZNAK.EQ.1) THEN
OPEN(UNIT=999, FILE=configFileName) ! нрйпшбюел тюик
	WRITE(999,102)N_START,'мювюкэмюъ бепрхйюкэмюъ рнвйю нймю напюанрйх'
	WRITE(999,102)N_FINISH,'йнмевмюъ  бепрхйюкэмюъ рнвйю нймю напюанрйх х сяпедмемхъ'
	WRITE(999,102)LM_WINDOW,'вхякн рпюяя б нйме напюанрйх х сяпедмемхъ'
	WRITE(999,103)(1-R_POSIB),'йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ бкюфмнярх'
	WRITE(999,103)(-R_POWER),'йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ сцкю мюйкнмю'					
!	WRITE(999,103)(1-R_SCIN),'йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ якнхярнярх'
	WRITE(999,103)(0.9-R_MOMENT),'йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ детнплюрхбмнярх'
	WRITE(999,103)(R_BALLAST-1),'йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ аюккюярмшу сцксакемхи'
	WRITE(999,102)N_BIT,'вхякн аюир налемю опх оепедюве рнвей рпюяяш'						
	WRITE(999,102)IANTENNA,'рхо юмреммнцн акнйю'
	WRITE(999,102)N_CHEN,'вхякн йюмюкнб'
	WRITE(999,102)J_LEVEL,'спнбемэ хгнапюфемхъ пюдюпнцпюллш б рнвйюу'
	WRITE(999,103)POROG,'йнмярюмрю ондюбкемхъ аекнцн ьслю'		
	WRITE(999,103)POROG1,'йнмярюмрю онхяйю люйяхлслю'
	WRITE(999,103)POROG2,'йнмярюмрю ондюбкемхъ мхгйху вюярнр'
	WRITE(999,103)T_RAZV,'пюгбепрйю'
	WRITE(999,102)M_REGION,'пюглеп нймю сяпедмемхъ б рнвйюу'
	WRITE(999,102)L_REGION,'пюглеп нймю сяпедмемхъ б рпюяяюу'
	WRITE(999,103)HH_B,'бшянрю ондбеяю юмреммш'
	WRITE(999,103)PREL,'онйюгюрекэ опекнлкемхъ аюккюярю'
	WRITE(999,102)N_MIDL_U,'пюглепмнярэ люяяхбнб дкъ бшвхякемхъ тхгхвеяйху оюпюлерпнб'					! опенапюгсел вхякн б ярпнйс
!------------------------------------------------------------------------
	WRITE(999,104) TRIM(DISK),'дхяй'
!	WRITE(999,*)	PAPKA	
!	WRITE(999,104)	FILE_TEXT	
	WRITE(999,102)	N_PRINT,'ьюц бшдювх пюдюпнцпюллш мю щйпюм'	
!	WRITE(999,102)	img_w,'ьхпхмю пхясмйю'	
!	WRITE(999,102)	img_h,'бшянрю пхясмйю'	
!--------------------------------------------------------------------
!	WRITE(999,*)	PAPKA1	
!	WRITE(999,*)	NP_PRINT,''	
	WRITE(999,104)	GRAF,'хлъ тюикю бшдювх якнеб'	
	WRITE(999,104)	VLAG,'хлъ тюикю бшдювх бкюфмнярх'	
	WRITE(999,104)	UGLUB,'хлъ тюикю бшдювх аюккюярмшу сцксакемхи'	
	WRITE(999,104)	INFO,'хлъ хмтнплюжхнммнцн тюикю'
!---------------------------------------------------------------------
	IFL_WRITER=1							! йкчв гюохях мю дхяй хгнапюфемхи
	WRITE(999,102)	IFL_WRITER,'йкчв гюохях мю дхяй хгнапюфемхи'	
	NP_PRINT=1								! вхякн рпюяя опх гюохях б рейярнбши тюик
	WRITE(999,102)	NP_PRINT,'вхякн рпюяя опх гюохях б рейярнбши тюик'	
	IFL_WRITER_KASKAD=1						! йкчв гюохях мю дхяй рейярнб
	WRITE(999,102)	IFL_WRITER_KASKAD,'йкчв гюохях мю дхяй рейярнб'		
    IFL_WRITER_TEXT=1						!йкчв гюохях тюикю лшьх
	WRITE(999,102)	IFL_WRITER_TEXT,'йкчв гюохях тюикю лшьх'	
	I_PRIZNAK=0								! пюанвюъ йнмярюмрю дкъ ондцнрнбйх тюикю гюохях хяундмшу дюммшу
	WRITE(999,102)	I_PRIZNAK,'пюанвюъ йнмярюмрю дкъ ондцнрнбйх тюикю гюохях хяундмшу дюммшу'	
	
END IF
!----------------------------------------------------------------------
101 FORMAT(A200)
102 FORMAT(I10,5X,A70)
103 FORMAT(F10.4,5X,A70)
104 FORMAT(A10,5X,A70)
!--------------------------------------------------------------------------
775 CALL DLGEXIT(FILE_DLG)						! гюйпшбюел дхюкнц
END IF



IF(C_NAME.EQ.NO) THEN
STOP				
END IF

END SUBROUTINE INDATA

