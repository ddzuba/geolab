! ---------------------------------------------------------------------------------------
! ббнд хяундмшу оюпюлерпнб хг тюикю 
! наъгюрекэмюъ гюохяэ мю дхяй пхясмйнб х рейярнбшу тюикнб
! ---------------------------------------------------------------------------------------

SUBROUTINE FIRST_READ()							! нопедекемхе вюярнрш хгксвемхъ
use FReciverGlobals
USE PARAM_
USE PARAM_1
USE RAILWAY
USE N_	
USE GRAFICA
implicit none
integer :: lret, make_folder, FLAG_NAME_GEO
INTEGER K1,K2,I
character*500 TEXT_CFG

    call ReadConfigFromIni()
 
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

!---------------------------------------------------------------------
! оепеявхршбюел оюпюлерпш нопедекемхъ тхгхвеяйху бекхвхм
M2_REGION=M_REGION/2                    ! онкнбхмю бшянрю нймю сяпедмемхъ
IDELTA=2.0*SQRT(PREL)/T_RAZV/0.3		! йнмярюмрю дкъ опнбедемхъ кхмхи я хмрепбюкнл б 1.0 лерп
idelta2=idelta/2						! йнмярюмрю дкъ опнбедемхъ кхмхи я хмрепбюкнл б 0.5 лерпю	
IDELTA4=IDELTA/4                        ! йнмярюмрю дкъ опнбедемхъ кхмхи я хмрепбюкнл б 0.25 лерпю

N_START=j_level+idelta4*RN_START+1		! бепумъъ цпюмхжю 
N_FINISH=j_level+idelta4*RN_FINISH			! мхфмъъ цпюмхжю 
!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*) N
	N_MIDL_BALL=N_MIDL_U					! вхякн рпюяя дкъ сяпедмемхъ онкнфемхъ цпюмхжш аюккюярю
! нопедекъел цпюмхжш бекхвхм
R_POSIB1=1+R_POSIB							! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ бкюфмнярх
R_POSIB=1-R_POSIB
R_MOMENT1=0.9+R_MOMENT						! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ детнплюрхбмнярх
R_MOMENT=0.9-R_MOMENT
R_BALLAST=1+R_BALLAST
!---------------------------------------------------------------------------------
CALL POINT_ARRAY() 
!---------------------------------------------------------------------------------
! тнплхпсел оюойх
! дкъ гюохях пхясмйнб -----------------------------------------------------------
CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
PAPKA=NAME_FILE_IMG
FILE='km_m_km_m_chen.bmp'
FILE_TEXT='MOUSE.DAT'
IF(IFL_WRITER.EQ.1) THEN
!--------------------------------------------------------------------
lret = make_folder(DISK,PAPKA) ! янгдюел оюойс
!--------------------------------------------------------------------
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/')
	I_LENTH=LEN_TRIM(NAME_FILE)		!  дкхмю хлемх
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(FILE))
	OPEN(UNIT=40,FILE=NAME_FILE ,FORM='BINARY',STATUS='REPLACE') ! нрйпшрхе тюикю дюммшу
	CLOSE(40,STATUS='DELETE')	! гюйпшрхе тюикю дюммшу
! тюик лшьх -----------------------------------------------------------------
	FILE_TEXT=TRIM(FILE_TEXT) !//'.DAT'
	NAME_FILE_TEXT=TRIM(TRIM(DISK)//&
     '/'//TRIM(PAPKA)//'/'//TRIM(FILE_TEXT))
	OPEN(UNIT=15,FILE=NAME_FILE_TEXT,STATUS='REPLACE') ! нрйпшрхе тюикю дюммшу
	WRITE(15,105)
	CLOSE(15)	! гюйпшрхе тюикю дюммшу
!--------------------------------------------------------------------
    END IF

! дкъ рейярнбшу тюикнб ---------------------------------------------------------------
CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
PAPKA=NAME_FILE_GEO

!--------------------------------------------------------------------
lret = make_folder(DISK,PAPKA) ! янгдюел оюойс
!--------------------------------------------------------------------
DO N_CHEN_TEK=1,N_CHEN						! мнлеп бшапюммнцн йюмюкю
	WRITE(STRING_CHEN, '(i1)') N_CHEN_TEK	! гЮОХЯШБЮЕЛ МНЛЕП ЙЮМЮКЮ Б ЯРПНЙС
! тюик GRAF
	NAME_GRAF=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(GRAF)//'_'//TRIM(STRING_CHEN)//'.csv')
	OPEN(UNIT=10,FILE=NAME_GRAF,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	FLAG_NAME_GEO=1
	STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; йхкнлерп; лерп; цксахмю оепбнцн якнъ; цксахмю брнпнцн якнъ')
	WRITE(10,106) STRING_200
	GRAF_NUMBER(N_CHEN_TEK)=NAME_GRAF ! гюонлхмюел хлемю
	CLOSE(10)	! гюйпшрхе тюикю дюммшу
!----------------------------------------------------------------------
! тюик VLAG
	NAME_VLAG=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(VLAG)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=20,FILE=NAME_VLAG,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; мювюкн йхкнлерп; мювюкн лерп; нйнмвюмхе йхкнлерп; нйнмвюмхе лерп; опнръфеммнярэ; бекхвхмю')
	WRITE(20,106) STRING_200
	VLAG_NUMBER(N_CHEN_TEK)=NAME_VLAG ! гюонлхмюел хлемю
	CLOSE(20)	! гюйпшрхе тюикю дюммшу
!----------------------------------------------------------------------
! тюик UGLUB
	NAME_UGLUB=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(UGLUB)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=30,FILE=NAME_UGLUB,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; мювюкн йхкнлерп; мювюкн лерп; нйнмвюмхе йхкнлерп; нйнмвюмхе лерп;опнръфеммнярэ;уюпюйрепхярхйю; бекхвхмю')
	WRITE(30,106) STRING_200
	UGLUB_NUMBER(N_CHEN_TEK)=NAME_UGLUB ! гюонлхмюел хлемю
	CLOSE(30)	! гюйпшрхе тюикю дюммшу
! тюик TOLSH 
	NAME_TOLSH=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(TOLSH)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=31,FILE=NAME_TOLSH,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
    STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; мювюкн йхкнлерп; мювюкн лерп; нйнмвюмхе йхкнлерп; нйнмвюмхе лерп;опнръфеммнярэ; бекхвхмю')
	WRITE(31,106) STRING_200
	TOLSH_NUMBER(N_CHEN_TEK)=NAME_TOLSH ! гюонлхмюел хлемю
	CLOSE(31)	! гюйпшрхе тюикю дюммшу  
!----------------------------------------------------------------------
! тюик INFO
	NAME_INFO=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(INFO)//'.xml')
    OPEN(UNIT=8,FILE=NAME_INFO,STATUS='REPLACE')  ! нрйпшрхе тюикю дюммшу
	CALL INFO_XML()
	CLOSE(8)	! гюйпшрхе тюикю дюммшу
END DO
!--------------------------------------------------------------------------	
105	FORMAT(1X,'мНЛЕП',4X,'мНЛЕП РПЮЯЯШ',4X,'йХКНЛЕРП',6x,'  лЕРП  ' )
106 FORMAT(A200)

RETURN							
END SUBROUTINE
!
!SUBROUTINE DATA_READ(K1,K2,STRING)							! ббнд ярпнйх хг йнмтхцспюжхнммнцн тюикю 
!IMPLICIT NONE
!INTEGER*4 J
!INTEGER*4 K1,K2
!INTEGER*4 L/500/ !дкхмю ярпнйх
!CHARACTER *500 STRING
!
!READ(999,*,END=1) STRING
!   DO J=1,L                                            ! нопедекъел дкхмш онкеи дкъ жекшу вхяек
!   IF(STRING(J:J).EQ.'=')K1=J+1
!   IF(STRING(J:J).EQ.';')K2=J-1
!   END DO
!   RETURN
!1 STOP
!END 

!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*) N