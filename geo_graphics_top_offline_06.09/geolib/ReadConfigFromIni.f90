subroutine ReadConfigFromIni()
use FReciverGlobals
USE PARAM_
USE PARAM_1
USE RAILWAY
USE N_		
USE GRAFICA
use IniFile
!use filetimeUtils ! дКЪ НОПЕДЕКЕМХЪ БПЕЛЕМХ ЯНГДЮМХЪ БХДЕН-ТЮИКЮ
implicit none
INTEGER*4 I,J,ret
    call Ini_Open("geolib.ini", 999)
   
!N_START=j_level+idelta4*N_START+1; (IDELTA4=25 ял) 		мювюкэмюъ бепрхйюкэмюъ рнвйю нймю напюанрйх			
    RN_START = Ini_Read_Real("RN_START", 1.0)
    
!N_FINISH=j_level+idelta4*N_FINISH  (IDELTA4=25 ял); 		йнмевмюъ  бепрхйюкэмюъ рнвйю нймю напюанрйх х сяпедмемхъ
    RN_FINISH = Ini_Read_Real("RN_FINISH", 6.0)

!LM_WINDOW=50; 		вхякн рпюяя б нйме напюанрйх х сяпедмемхъ
    LM_WINDOW = Ini_Read_Int("LM_WINDOW", 50)

!R_POSIB=0.3;  		йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ бкюфмнярх
    R_POSIB = Ini_Read_Real("R_POSIB", 0.3)
    
!R_POWER=10;		йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ сцкю мюйкнмю	
!    R_POWER = Ini_Read_Real("R_POWER", 10.0)

!R_SCIN=0.4;		йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ якнхярнярх
!    R_SCIN = Ini_Read_Real("R_SCIN", 0.4)

!R_MOMENT=0.7;		йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ детнплюрхбмнярх
    R_MOMENT = Ini_Read_Real("R_MOMENT", 0.7)

!R_BALLAST=0.1;		йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ аюккюярмшу сцксакемхи'
    R_BALLAST = Ini_Read_Real("R_BALLAST", 0.1)

!PARAM_MIDL1=0.7;		йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ бепумеи цпюмхжш онкняш опх юмюкхге'
    PARAM_MIDL1 = Ini_Read_Real("PARAM_MIDL1", 0.7)
    
!PARAM_MIDL2=1.3;		йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ мхфмеи цпюмхжш онкняш опх юмюкхге'
    PARAM_MIDL2 = Ini_Read_Real("PARAM_MIDL2", 1.3)
    
!N_BIT=2;		вхякн аюир налемю опх оепедюве рнвей рпюяяш	
    N_BIT = Ini_Read_Int("N_BIT", 2)

!N=512;			вхякн рнвей б гюохях рпюяяш
    N = Ini_Read_Int("N", 512)

!IANTENNA=400;		рхо юмреммнцн акнйю
    IANTENNA = Ini_Read_Int("IANTENNA", 400)
    
!N_CHEN_F=1;		мнлеп оепбнцн йюмюкю
    N_CHEN_F = Ini_Read_Int("N_CHEN_F", 1)
    
!N_CHEN=3;		вхякн йюмюкнб
    N_CHEN = Ini_Read_Int("N_CHEN", 3)
    IF(N_CHEN.GT.3) THEN
        N_CHEN=3
        N_CHEN_F=1
        END IF
!J_LEVEL=70;		спнбемэ хгнапюфемхъ пюдюпнцпюллш б рнвйюу
    J_LEVEL = Ini_Read_Int("J_LEVEL", 70)

!POROG_INTENS=12000.0		йнмярюмрю ондюбкемхъ йюмюкю	
    POROG_INTENS = Ini_Read_Real("POROG_INTENS", 12000.0)
    
!POROG=0.03		йнмярюмрю ондюбкемхъ аекнцн ьслю	
    POROG = Ini_Read_Real("POROG", 0.03)

!POROG1=0.9		йнмярюмрю онхяйю люйяхлслю
    POROG1 = Ini_Read_Real("POROG1", 0.9)

!POROG2=0.2;		йнмярюмрю ондюбкемхъ мхгйху вюярнр
    POROG2 = Ini_Read_Real("POROG2", 0.2)

!KEY_MIDL=0;		йнмярюмрю бшвхрюмхъ япедмеи рпюяяш
    KEY_MIDL = Ini_Read_Int("KEY_MIDL", 0)
    
!KEY_REFLECTION =0;		йнмярюмрю ондюбкемхъ кхмхи йпюрмнцн нрпюфемхъ
    KEY_REFLECTION = Ini_Read_Int("KEY_REFLECTION", 0)
       
!T_RAZV=0.097;		пюгбепрйю он бпелемх
    T_RAZV = Ini_Read_Real("T_RAZV", 0.097)
    
!T_RAZV=10.0;		лхмхлюкэмюъ дкхмю накюяреи бкюфмнярх
    LENGTH_HUMIDITY = Ini_Read_Real("LENGTH_HUMIDITY", 10.0)
    
!STEP_HUMIDITY=0.5;		пюяярнъмхе назедхмемхъ накюяреи бкюфмнярх
    STEP_HUMIDITY = Ini_Read_Real("STEP_HUMIDITY", 0.5)    

!M_REGION=3;		пюглеп нймю сяпедмемхъ б рнвйюу
    M_REGION = Ini_Read_Int("M_REGION", 3)

!L_REGION=3;		пюглеп нймю сяпедмемхъ б рпюяяюу
    L_REGION = Ini_Read_Int("L_REGION", 3)

!HH_B=40.0;		бшянрю ондбеяю юмреммш
    HH_B = Ini_Read_Real("HH_B", 40.0)
    
!BAZ=20.0;		пюяярнъмхе лефдс юмреммюлх
    BAZ = Ini_Read_Real("BAZ", 38.0)    

!PREL=4.5;		онйюгюрекэ опекнлкемхъ аюккюярю
    PREL = Ini_Read_Real("PREL", 5.0)
!    DO I=1,N_CHEN
!    INDEX_REFRACTION_0(I)=SQRT(PREL)
!    END DO
    
!N_MIDL_U=3000;		пюглепмнярэ люяяхбнб дкъ бшвхякемхъ тхгхвеяйху оюпюлерпнб
    N_MIDL_U = Ini_Read_Int("N_MIDL_U", 3000)
    
!DISK=C:\GEODATA;	дхяй
    DISK = Trim(Ini_Read_String_Default("DISK", "C:\GEODATA"))
I=LEN(TRIM(DISK))
DO J=1,I
IF(DISK(J:J).EQ.' ')DISK(J:J)='+'
END DO
!N_PRINT=1;		ьюц бшдювх пюдюпнцпюллш мю щйпюм
    N_PRINT = Ini_Read_Int("N_PRINT", 1)

!GRAF=graf;		хлъ тюикю бшдювх якнеб
    GRAF = Trim(Ini_Read_String_Default("GRAF", "graf"))

!VLAG=vlagh;		хлъ тюикю бшдювх бкюфмнярх
    VLAG = Trim(Ini_Read_String_Default("VLAG", "vlagh"))

!UGLUB=uglub;		хлъ тюикю бшдювх аюккюярмшу сцксакемхи	
    UGLUB = Trim(Ini_Read_String_Default("UGLUB", "uglub"))
    
!TOLSH=TOLSH;		хлъ тюикю бшдювх аюккюярмшу сцксакемхи	
    TOLSH = Trim(Ini_Read_String_Default("TOLSH", "TOLSH"))

!INFO=info;		хлъ хмтнплюжхнммнцн тюикю
    INFO = Trim(Ini_Read_String_Default("INFO", "info"))

!IFL_WRITER=1;		йкчв гюохях мю дхяй хгнапюфемхи'
    IFL_WRITER = Ini_Read_Int("IFL_WRITER", 1)

!NP_PRINT=1;		вхякн рпюяя опх гюохях б рейярнбши тюик	
    NP_PRINT = Ini_Read_Int("NP_PRINT", 1)

!IFL_WRITER_KASKAD=1;	йкчв гюохях мю дхяй рейярнб	
    IFL_WRITER_KASKAD = Ini_Read_Int("IFL_WRITER_KASKAD", 1)

!IFL_WRITER_TEXT=1;  	йкчв гюохях тюикю лшьх
    IFL_WRITER_TEXT = Ini_Read_Int("IFL_WRITER_TEXT", 1)
    
! NDEG1=5; оНПЪДНЙ ОНКХМНЛЮ ДКЪ ХМРЕПОНКЪЖХХ ЦПЮМХЖ ПЮГДЕКЮ
    NDEG1=Ini_Read_Int("NDEG1", 5)

! ISTEP_BETWEEN_TRASES=1; пЮЯЯРНЪМХЕ ЛЕФДС РПЮЯЯЮЛХ, Б ЛЛ
    ISTEP_BETWEEN_TRASES=Ini_Read_Int("ISTEP_BETWEEN_TRASES", 1)
    
! ISTEP_FOR_GRAFICA=10; оЮПЮЛЕРП ДКЪ ПЮЯВЕРЮ ВХЯКЮ ОПНОСЯЙНБ МЮ ЩЙПЮМЕ
    ISTEP_FOR_GRAFICA=Ini_Read_Int("ISTEP_FOR_GRAFICA", 10)
    
!N_PRIZNAK_GRAFICA=1; тКЮЦ, СЙЮГШБЮЧЬХИ ОНЙЮГШБЮРЭ КХ НЙМН Я ЦПЮТХЙНИ.
!        N_PRIZNAK_GRAFICA = Ini_Read_Int("N_PRIZNAK_GRAFICA", 0)

!  R_I_B йнщттхжхемр дкъ бшвхякемхъ сякнбмни бепумси кхмхх аюккюярю:    JJ1_B_0=J_LEVEL+1+IDELTA4*R_I_B (IDELTA4=25 ял)
    R_I_B=Ini_Read_Real("R_I_B", 1.2)

!  R_I_B1 йнщттхжхемр дкъ бшвхякемхъ сякнбмни мхфмеи кхмхх аюккюярю:    JJ2_B_0=J_LEVEL+1+IDELTA4*R_I_B1 (IDELTA4=25 ял)
    R_I_B1=Ini_Read_Real("R_I_B1", 3.0)

!  R_I_PB йнщттхжхемр дкъ бшвхякемхъ сякнбмни мхфмеи кхмхх ондаюккюярмнцн нямнбюмхъ :    JJ2_PB_0=JJ1_PB_0+IDELTA4*R_I_PB (IDELTA4=25 ял)
    R_I_PB=Ini_Read_Real("R_I_PB", 3.0)
	
!  RMULTIPLE йнщттхжхемр сяхкемхъ яхцмюкю б ондаюккюярмни гнме
    RMULTIPLE=Ini_Read_Real("RMULTIPLE", 0.0)

! HH_RAB ьюц дкъ хгнапюфемхъ ьйюкш цксахм б лерпюу б днкъу нр HH_STEP
    HH_RAB= Ini_Read_Int("HH_RAB", 2)

! HH_STEP ьюц дкъ хгнапюфемхъ вхяек мю ьйюке цксахм б лерпюу
    HH_STEP= Ini_Read_Real("HH_STEP", 0.5)    
    
! FLAG_HH йкчв дкъ хгнапюфемхъ яерйх ьйюкш цксахм (FLAG_HH=0 ме хгнапюфюел)
    FLAG_HH=Ini_Read_Int("FLAG_HH", 0)    
    
! йкчв дкъ хгнапюфемхъ нймю (FLAG_WINDOW=0 ме хгнапюфюел)
    FLAG_WINDOW=Ini_Read_Int("FLAG_WINDOW", 0)     
    
!VIDEO_NAME=zero;		хлъ бхден тюикю 	
    VIDEO_NAME = Trim(Ini_Read_String_Default("VIDEO_NAME", "ZERO"))

    if(VIDEO_NAME.ne.'ZERO') then
!  TIME_VIDEO_FILE - гдбхц он бпелемх лефдс бхден онрнйнл х ценпюдхнкнйюжхеи       
    TIME_VIDEO_FILE=Ini_Read_Real("TIME_VIDEO_FILE", 0.0)
    end if     

    !FOTO_NAME=ZERO;		хлъ бхден тюикю 	
    FOTO_NAME = Trim(Ini_Read_String_Default("FOTO_NAME", "ZERO"))
end subroutine ReadConfigFromIni
    
