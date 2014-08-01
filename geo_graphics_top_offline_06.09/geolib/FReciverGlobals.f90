!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module FReciverGlobals
use dflogm
implicit none

!  Parameters

integer*4, parameter, public :: SIZEOFAPPNAME = 100

!  Global data

integer		coord ! ДКЪ ОПХЕЛЮ ЙННПДХМЮРШ
REAL*4		kilometr ! ДКЪ ОПХЕЛЮ ЙХКНЛЕРПНБ
REAL*4		metr ! ДКЪ ОПХЕЛЮ ЛЕРПНБ
REAL*4		KILOMETR_MOUSE ! ДКЪ ОПХЕЛЮ ЙХКНЛЕРПНБ НР ЛШЬХ
REAL*4		METR_MOUSE ! ДКЪ ОПХЕЛЮ ЛЕРПНБ НР ЛШЬХ
integer		kilometr_0 ! ДКЪ ОПХЕЛЮ ЙХКНЛЕРПНБ ОЕПБНИ РПЮЯЯШ
integer		metr_0	   ! ДКЪ ОПХЕЛЮ ЛЕРПНБ ОЕПБНИ РПЮЯЯШ
integer		kilometr_1 !,kilometr_1 ! ДКЪ ХЛЕМХ ТЮИКЮ ПХЯСМЙНБ
integer		metr_1	   !,metr_2		  ! ДКЪ ХЛЕМХ ТЮИКЮ ПХЯСМЙНБ
integer		metr_old_ ! ДКЪ ОПХЕЛЮ ЛЕРПНБ

integer		ghInstance
integer		ghModule
integer		ghwndMain
integer		ghMenu
integer		nfnt	! ПЮГЛЕП ЬПХТРЮ МЮ ЩЙПЮМЕ
integer		nfnt1	! ЬХПХМЮ ОНКНЯШ

integer		isFirst /1/
integer		hMainBitmap /0/
integer		hMainDC /0/
integer		hBackBitmap /0/
integer		hBackDC /0/

REAL*4, ALLOCATABLE, target :: MyTrassa(:)	! РПЮЯЯЮ, ДКХМЮ ГЮБХЯХР НР ВХЯКЮ ЙЮМЮКНБ
REAL*4, ALLOCATABLE, target :: trace_buf(:) ! АСТЕП РПЮЯЯШ, ДКХМЮ ПХЯНБЮМХЪ

! оНКНЯЮ, Б ОПЕДЕКЮУ ЙНРНПНИ ПХЯСЕРЯЪ ЦПЮТХЙ ЯСЛЛШ
integer sum_line_top / 50 /
integer sum_line_bottom / 300 /

integer NSHPAL_OLD / -1 /

! дКЪ ОЮКХРПШ, ЛХМ. Х ЛЮЙЯ. ГМЮВЕМХЪ, ЯННРБЕРЯРБСЧЫРЕ
! АЕКНЛС Х В╦ПМНЛС ЖБЕРЮЛ.
real*4	MAX_VAL / 10000  /
real*4	MAX_VAL_OLD / 10000  /
real*4	MIN_VAL / -10000 /

integer NPT / 0 / ! ВХЯКН РНВЕЙ Б РПЮЯЯЕ

integer		img_w /1000/ ! ЬХПХМЮ ПХЯСМЙЮ
integer		img_h /600/ ! БШЯНРЮ ПХЯСМЙЮ
integer		img_hh  ! БШЯНРЮ ПХЯСМЙЮ

integer		speed_gr_zero /240/ ! ОНКНФЕМХЕ МСКЪ ЦПЮТХЙЮ ЯЙНПНЯРХ
real*4		speed_gr_scale /30.0/ ! ЛЮЯЬРЮА (ЛМНФХРЕКЭ) ЦПЮТХЙЮ ЯЙНПНЯРХ 
integer		speed_gr_color / 255 /  ! ЖБЕР КХМХХ ЦПЮТХЙЮ ЯЙНПНЯРХ

integer		sum_gr_zero /598/ ! ОНКНФЕМХЕ МСКЪ ЦПЮТХЙЮ НЦХАЮЧЫЕИ
real*4		sum_gr_scale /0.01/ ! ЛЮЯЬРЮА (ЛМНФХРЕКЭ) ЦПЮТХЙЮ НЦХАЮЧЫЕИ 
integer		sum_gr_color / ZFF0000 / !/ 16711680 /  ! ЖБЕР КХМХХ ЦПЮТХЙЮ НЦХАЮЧЫЕИ

INTEGER*4 , ALLOCATABLE ::trace_draw_pos(:)	   ! ОНКНФЕМХЕ РПЮЯЯШ
INTEGER*4 , ALLOCATABLE ::trace_draw_pos_1(:)  ! РЮМЦЕМЯ СЦКЮ МЮЙКНМЮ
INTEGER*4 , ALLOCATABLE ::trace_draw_pos_2(:)  ! НРПЮФЮРЕКЭМЮЪ ЯОНЯНАМНЯРЭ
INTEGER*4 , ALLOCATABLE ::trace_draw_pos_3(:)  ! НДМНПНДМНЯРХ ЯКНЪ
INTEGER*4 , ALLOCATABLE ::trace_draw_pos_4(:)  ! ЛНЛЕМР
INTEGER*4 , ALLOCATABLE ::trace_draw_pos_5(:)  ! ОНКНФЕМХЕ ПХЯСМЙЮ

integer		TRACE_POS / 0 / ! РЕЙСЫЮЪ ОНГХЖХЪ РПЮЯЯШ ОПХ НРПХЯНБЙЕ.

integer*4, target	:: booboo
integer*4, target	:: cursor_pos = -1 ! ОНКНФЕМХЕ ЙСПЯНПЮ (ЛЕЯРЮ ОНЯКЕДМЕЦН ЫЕКВЙЮ ЛШЬХ)
integer*4, target	:: TRACE_COUNT = 0 ! ЯВ╦РВХЙ ОПХМЪРШУ РПЮЯЯ

CHARACTER(100) configFileName / "geolib.cfg"C /
CHARACTER(500) fullConfigFilePath / ""C /

! йНМЯНКЭ
type(dialog) InfoDialog
integer CONSOLE_LIMIT / 200000 /
character(200000) :: REZTEXT
character(200000) :: CUR_CONSOLE
end module

MODULE PARAM_
USE DFLOGM				! НАЪГЮРЕКЭМЮЪ ЯЯШКЙЮ Б ЯКСВЮЕ DVF
INCLUDE 'RESOURCE.FD'	! БЙКЧВЮЧ ОЮПЮЛЕРПШ ПЕЯСПЯНБ
LOGICAL(4) FLAG
!----------------------------------------------------------------------------------------------------
INTEGER*4  IANTENNA											! рхо юмреммнцн акнйю
INTEGER*4  J_LEVEL											! спнбемэ хгнапюфемхъ пюдюпнцпюллш б рнвйюу 
INTEGER*4  L_REGION,M_REGION,M2_REGION						! оюпюлерпш бшанпю нймю сяпедмемхъ
REAL*4 POROG_INTENS                                         ! онпнц ондюбкемхъ йюмюкю
REAL*4 POROG												! йнмярюмрю ондюбкемхъ аекнцн ьслю
REAL*4 POROG1												! йнмярюмрю онхяйю люйяхлслю
REAL*4 POROG2												! йнмярюмрю ондюбкемхъ мхгйху вюярнр
INTEGER*4  LM_WINDOW,N_FINISH,N_START						! пюглеп нймю напюанрйх х сяпедмемхъ
REAL*4  RN_FINISH,RN_START						            ! йнмярюмрш дкъ бшвхякемхъ пюглепю нймю напюанрйх х сяпедмемхъ
INTEGER*4  NDEG1                                            ! онпъднй онкхмнлю дкъ хмрепонкъжхх цпюмхж пюгдекю
INTEGER*4  KEY_MIDL,KEY_MIDL1/0/                            ! йнмярюмрю бшвхрюмхъ хг рпюяяш япедмеи рпюяяш
INTEGER*4  KEY_REFLECTION                                   ! йнмярюмрю ондюбкемхъ кхмхи йпюрмнцн нрпюфемхъ

!----------------------------------------------------------------------------------------------------
! оепелеммше дхюкнцнб -------------------------------------------------------------------------------
TYPE(DIALOG) PARAMETR										!
TYPE(DIALOG) PARAMETR1										!
TYPE(DIALOG) PARAMETR2										!
TYPE(DIALOG) PARAMETR3										!
TYPE(DIALOG) PARAMETR4										!
INTEGER*4  STATUS											!
CHARACTER *200 STRING										!
!----------------------------------------------------------------------------------------------------
! напюанрйю пегскэрюрнб пюяверю ---------------------------------------------------------------------
INTEGER*4  ITEK/0/								! рейсыхи мнлеп напюаюршбюелни рпюяяш

INTEGER(4), ALLOCATABLE:: IPR_CHEN(:)           ! опхгмюй ондюбкемхъ йюмюкю
INTEGER(4), ALLOCATABLE:: IPR(:)				! йкчв дкъ бкюфмнцн свюярйю
INTEGER(4), ALLOCATABLE:: IPR1(:)				! йкчв дкъ пюяверю сцксакемхи
INTEGER(4), ALLOCATABLE:: IPR1_UP(:)            ! йкчв дкъ пюяверю аюккюярмшу сцксакемхи

!REAL*4, ALLOCATABLE:: UNIFORMITY(:)							! UNIFORMITY_KR ! ндмнпндмнярэ
!REAL*4, ALLOCATABLE::UNIFORMITY_MIDL(:)	                    ! UNIFORMITY_KR ! ндмнпндмнярэ

INTEGER*4  N_MIDL_U							                ! люяяхб дкъ сяпедмемхъ ндмнпндмнярх


REAL*4, ALLOCATABLE::REFR(:)				                ! нрпюфюрекэмюъ яонянамнярэ
REAL*4, ALLOCATABLE::REFR_MIDL(:)			                ! нрпюфюрекэмюъ яонянамнярэ
REAL*4, ALLOCATABLE::REFR_MAX(:)			                ! люйяхлюкэмюъ нрпюфюрекэмюъ яонянамнярэ
REAL*4, ALLOCATABLE:: REFRACTION(:,:)                       ! асттеп нрпюфюрекэмшу яонянамняреи
INTEGER*4,ALLOCATABLE::STAR0(:)                             ! цпюмхжш онкня дкъ оевюрх бкюфмнярх  
INTEGER*4,ALLOCATABLE::FIN0(:)                              ! цпюмхжш онкня дкъ оевюрх бкюфмнярх  
INTEGER*4,ALLOCATABLE::STAR1(:)                             ! цпюмхжш онкня дкъ оевюрх бкюфмнярх  
INTEGER*4,ALLOCATABLE::FIN1(:)                              ! цпюмхжш онкня дкъ оевюрх бкюфмнярх  
INTEGER*4,ALLOCATABLE::STAR1_OLD(:)                         ! цпюмхжш онкня дкъ оевюрх бкюфмнярх  
INTEGER*4,ALLOCATABLE::KL_FORPRINT(:)                       ! йкчв дкъ оевюрх бкюфмнярх (0 х дкхмю люяяхбю FORPRINT)
INTEGER*4,ALLOCATABLE::FLAG_FORPRINT(:)                     ! ткюц опнярн оевюрх бкюфмнярх
INTEGER*4 DL_BLUE,DL_WIGHT                                  ! дкхмш бкюфмшу х ясуху свюярйнб б рпюяяюу
REAL*4, ALLOCATABLE::RMOM(:)								! RMOM_KR ! лнлемр
REAL*4, ALLOCATABLE::RMOM_MIDL(:)			                ! RMOM_KR ! лнлемр
REAL*4, ALLOCATABLE::RMOM_MAX(:)			                ! люйяхлюкэмюъ нрпюфюрекэмюъ яонянамнярэ
			
REAL*4, ALLOCATABLE::R_BALL(:)			                    ! цксахмю  дмю										
REAL*4, ALLOCATABLE::R_BALL_UP(:)                           ! цксахмю аюккюярмнцн дмю										
REAL*4, ALLOCATABLE::R_BALL_MIDL(:)		                    ! цксахмю  дмю										
REAL*4, ALLOCATABLE::R_BALL_MIDL_UP(:)		                ! цксахмю аюккюярмнцн дмю										
INTEGER*4  N_MIDL_BALL						                ! люяяхб дкъ сяпедмемхъ цксахмш аюккюярмнцн дмю
! INTEGER*4  N_MIDL_BALL_UP                                 ! люяяхб дкъ сяпедмемхъ цксахмш аюккюярмнцн дмю
REAL*4,ALLOCATABLE :: BALL(:,:)
REAL*4,ALLOCATABLE :: BALL_UP(:,:)
REAL*4 RABTIME
!-------------------------------------------------------------------------------------------------
REAL*4, ALLOCATABLE :: SUM_RAB_MIDL(:,:)					! пюанвхи люяяхб дкъ сяпедмемхъ
REAL*4, ALLOCATABLE :: SUM_MIDDLE(:,:)  					! пюанвхи люяяхб дкъ бшвхрюмхъ
REAL*4, ALLOCATABLE :: HH(:)					            ! люяяхб цксахм
REAL*4, ALLOCATABLE :: T_N(:)					            ! люяяхб бпелем
INTEGER*4, ALLOCATABLE :: IHH_50(:)					        ! люяяхб мнлепнб рнвей я ьюцнл 50 ял
INTEGER*4 HH_RAB                                            ! ьюц дкъ хгнапюфемхъ ьйюкш цксахм б днкъу нр HH_STEP
REAL*4 HH_STEP                                              ! ьюц дкъ хгнапюфемхъ вхяек мю ьйюке цксахм б лерпюу
INTEGER*4 FLAG_HH                                           ! йкчв дкъ хгнапюфемхъ яерйх ьйюкш цксахм
INTEGER*4 FLAG_WINDOW                                       ! йкчв дкъ хгнапюфемхъ нймю
INTEGER*4 I_HH                                              ! вхякн рнвей б люяяхбе мнлепнб рнвей я ьюцнл 50 ял 
REAL*4 T_RAZV												! пюгбепрйю
REAL*4 VC0 /30/                                             ! яйнпнярэ яберю б ял/мя
INTEGER*4 JJ_H	    				                        ! рнвйю мювюкю яхцмюкю нр онбепумнярх		
REAL*4 HH_B	   											    ! бшянрю ондбеяю юмреммш 
REAL*4 BAZ                                                  ! пюяярнъмхе лефдс юмреммюлх
REAL*4, ALLOCATABLE :: BUFF(:,:,:)							! мюйнокемхе рпюяя дкъ сяпедмемхъ
REAL*4, ALLOCATABLE :: BUFF_MIDDLE(:,:,:)                   ! мюйнокемхе рпюяя дкъ бшвхрюмхъ япедмецн
! тюикнбюъ хмтнплюжхъ ----------------------------------------------------------------------------				    
CHARACTER(200) DISK,PAPKA,PAPKA1,FILE					    ! осрэ й тюикс гюохях пхясмйнб 
CHARACTER(200),ALLOCATABLE :: FILE_NUMBER(:)                ! люяяхб дкъ упюмемхъ хлем
CHARACTER(200) DISK_TEXT,PAPKA_TEXT,FILE_TEXT			    ! осрэ й тюикс гюохях онкнфемхъ лшьх
CHARACTER(200) NAME_FILE                				    ! хлъ тюикнб
CHARACTER(200) NAME_FILE_TEXT			        		    ! хлъ тюикю лшьх
INTEGER*4  I_LENTH
CHARACTER STR(200)										    ! дкъ тнплхпнбюмхъ хлемх
!-------------------------------------------------------------------------------------------------
INTEGER*4  N_FILE_DISK      								! мнлеп тюикю B явервхй
REAL*4 PREL											        ! онйюгюрекэ опекнлкемхъ аюккюярю
INTEGER*4  J_DOWN										    ! мнлеп рпюяяш дкъ дмю ьоюкш
!-----------------------------------------------------------------------------------------------------------------------										    
! мнплхпнбйю онйюгюмхи
REAL*4 R_POSIB,R_POSIB1								    ! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ лнымнярх якнъ
REAL*4 R_MOMENT,R_MOMENT1								    ! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ лнымнярх якнъ
REAL*4 R_BALLAST										    ! йнппейрхпсчыхи лмнфхрекэ дкъ нопедекемхъ аюккюярмшу сцксакемхи
!-----------------------------------------------------------------------------------------------------------------------										    
! йнмярюмрш дкъ бшбндю мю оевюрэ мнлепнб охйернб------------------------------------------------------------------------ 
INTEGER*4  NFONT											! пюглеп асйб дкъ ондохях йхкнлерпнб
INTEGER*4  JSDVIG											! ядбхц мюдохях бднкэ цнпхгнмрюкэмни нях
INTEGER*4  JSDVIG_TR										! ядбхц мюдохях бднкэ цнпхгнмрюкэмни нях
!----------------------------------------------------------------------------------------------------------------------
REAL*4 PARAM_MIDL1                                          ! оюпюлерп дкъ нопедекемхъ бепумеи цпюмхжш онкняш опх юмюкхге
REAL*4 PARAM_MIDL2                                          ! оюпюлерп дкъ нопедекемхъ мхфмеи цпюмхжш онкняш опх юмюкхге
!----------------------------------------------------------------------------------------------------------------------
END MODULE PARAM_
!-----------------------------------------------------------------------------------------------------------------------
MODULE PARAM_1
! сопюбкемхе йюмюкюлх -------------------------------------------------
INTEGER*4 	ISCH/0/		  ! явервхй йюмюкнб
INTEGER*4, target :: N_CHEN	= -1      ! вхякн йюмюкнб. -1 - ОНЙЮГШБЮЕР, ВРН ОЕПЕЛЕММЮЪ МЕ СЯРЮМНБКЕМЮ.
INTEGER*4 	N_CHEN_F	      ! мнлеп оепбнцн йюмюкю б оняшкйе хмрецпюкю
INTEGER*4 	N_CHEN_TEK    ! мнлеп бшапюммнцн йюмюкю
REAL*4      COORDINATE,COORDINATE1          ! дкъ бшвхякемхъ опносяйю йннпдхмюр
! тнплхпнбюмхе хлемх оюойх---------------------------------------------
! мнлеп сярпниярбю
INTEGER*4  N2_DEV,N2_DEV1
CHARACTER *200 N_DEVICE,N_DEVICE1			! МНЛЕП СЯРПНИЯРБЮ
! мнлеп мюопюбкемхъ
INTEGER*4  N2_VAY,N2_VAY1
CHARACTER *200 N_VAY,N_VAY1					! МНЛЕП ДНПНЦХ
! мнлеп мюопюбкемхъ
INTEGER*4  N2_DIR,N2_DIR1
CHARACTER *200 N_DIRECTION,N_DIRECTION1		! ЙНД МЮОПЮБКЕМХЪ
! мнлеп осрх
INTEGER*4  N2_TRA,N2_TRA1
CHARACTER *200 N_TRACK,N_TRACK1				! МНЛЕП ОСРХ
! мнлеп осрх
CHARACTER *200 STRING_CHEN					! МНЛЕП ЙЮМЮКЮ
! хлъ оюойх
CHARACTER *300 NAME_FILE_IMG,NAME_FILE_IMG_JPEG				! оюойю дкъ пюдюпнцпюMM
CHARACTER *300 NAME_FILE_GEO,NAME_FILE_GEO_ZIP				! оюойю дкъ йюяйюдю
!INTEGER*4  FLAG_NAME_IMG/0/
!INTEGER*4  FLAG_NAME_GEO/0/
!-----------------------------------------------------------------------
! йнмярюмрш дкъ гюдюмхъ ьюцю бшдювх пюдюпнцпюллш мю щйпюм----------------------------
INTEGER*4, target :: N_PRINT		  ! ьюц бшдювх рпюяя мю напюанрйс х лнмхрнп
!INTEGER*4  LM_WINDOW_N_PRINT		  ! опнхгбедемхе йнмярюмр
INTEGER*4  LM_WINDOW1,LM_WINDOW2      ! пюанвхе йнмярюмрш
INTEGER*4  K_PRINT/1/	  ! рейсыее гмювемхе ьюцю бшдювх пюдюпнцпюллш мю щйпюм
! опх янбоюдемхее N_PRINT х K_PRINT бшонкмъеряъ оевюрэ
INTEGER*4  NP_PRINT/1/		  ! вхякн рпюяя опх гюохях б рейярнбши тюик
INTEGER*4  KP_PRINT/1/	  ! рейсыее гмювемхе ьюцю бшдювх б рейярнбши тюик
! опх янбоюдемхее NP_PRINT х KP_PRINT бшонкмъеряъ бшдювю б рейярнбши тюик
!INTEGER*4  N_PRINT5       ! пюанвюъ йнмярюмрю
!------------------------------------------------------------------------------------
INTEGER*4  IFLAG_PRINT				  ! опхгмюй оевюрх
!----------------------------------------------------------------------------------------------------------------------
INTEGER*4  kluch/0/											! ЙКЧВ ДКЪ НОПНЯЮ ЩЙПЮМЮ
!----------------------------------------------------------------------------------------------------------------------
INTEGER*4  K_FLAG/0/			! дкъ тнплхпнбюмхъ оюонй бпсвмсч
INTEGER*4  FL_START/0/			! сйюгюрекэ мювюкю пюанрш
!----------------------------------------------------------------------------------------------------------------------
!ТНПЛХПСЕЛ ОПНОСЯЙ РПЮЯЯ МЮ ПХЯСМЙЕ
!----------------------------------------------------------------------------------------------------------------------
REAL*4 rang,rang1				! ОПНИДЕММШИ ОСРЭ
REAL*4 rang2					! ОПНИДЕММШИ ОСРЭ Б ЙХКЛЕРПЮУ
REAL*4 rang3					! ОПНИДЕММШИ ОСРЭ Б ЛЕРПЮУ
integer*4 out_trassa			! ВХЯКН ОПНОСЫЕММШУ РПЮЯЯ
!----------------------------------------------------------------------------------------------------------------------
INTEGER*4 , ALLOCATABLE :: N_WRITE_VLAG(:)	! вхякн гюохяеи б тюик VLAG
INTEGER*4 , ALLOCATABLE :: N_WRITE_UGLUB(:)	! вхякн гюохяеи б тюик UGLUB
INTEGER*4 , ALLOCATABLE :: N_WRITE_TOLSH(:)	! вхякн гюохяеи б тюик TOLSH
!----------------------------------------------------------------------------------------------------------------------
REAL*4 LENGTH_HUMIDITY                      ! лхмхлюкэмюъ дкхмю накюяреи бкюфмнярх
REAL*4 STEP_HUMIDITY                        ! пюяярнъмхе назедхмемхъ накюяреи бкюфмнярх
!----------------------------------------------------------------------------------------------------------------------

END MODULE PARAM_1
!----------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------
MODULE N_									! хяонкэгсеряъ дкъ оепедювх хмтнплюжхх
INTEGER*4, target :: N                      ! дкхмю рпюяяш
INTEGER*4 NN_CHEN,NN_CHEN_F                 ! дкхмю рпюяяш, слмнфеммюъ мю вхякн йюмюкнб

REAL*4, ALLOCATABLE :: RDM(:)				! пюанвхи люяяхб
REAL*4, ALLOCATABLE :: RDM1(:)			    ! пюанвхи люяяхб
REAL*4, ALLOCATABLE :: RDM2(:,:)            ! пюанвхи люяяхб
INTEGER*4  IDELTA,IDELTA2,IDELTA4			! вхякн рнвей рпюяяш мю лерп
REAL*4 R_LEV								! цксахмю якнъ
INTEGER*4  JJ1_B_0							! бепумее гмювемхе  нймю онхяйю цпюмхжш аюккюярмнцн якнъ 
INTEGER*4  JJ2_B_0							! мхфмее гмювемхе  нймю онхяйю цпюмхжш аюккюярмнцн якнъ 
INTEGER*4  JJ1_PB_0							! бепумее гмювемхе  нймю онхяйю цпюмхжш ондаюккюярмнцн якнъ 
INTEGER*4  JJ2_PB_0							! мхфмее гмювемхе  нймю онхяйю цпюмхжш ондаюккюярмнцн якнъ 
INTEGER*4  JJ2_GR			                ! мхфмее гмювемхе гелкъмнцн онкнрмю
INTEGER*4  I_B								! цпюмхжю аюккюярю
INTEGER*4  I_BSEK,I_PBSEK                   ! цпюмхжю аюккюярю
INTEGER*4  I_PB								! цпюмхжю ондаюккюярмнцн нямнбюмхъ
INTEGER*4 , ALLOCATABLE:: YY(:,:)			! гмювемхъ цпюмхжш аюккюярю дкъ хмрепонкъжхи
INTEGER*4 , ALLOCATABLE:: YY1(:,:)			! гмювемхъ  ондаюккюярмни цпюмхжш дкъ хмрепнкъжхи
INTEGER*4 , ALLOCATABLE:: IA(:)				! явервхй гюонкмемхъ люяяхбнб цпюмхж
REAL*4 RMULTIPLE                            ! бекхвхмю сяхкемхъ б ондаюккюярмнл якне
REAL*4 R_I_B                                ! онярнъммю бшанпю сякнбмни бепумеи кхмхх аюккюярю
REAL*4 R_I_B1                               ! онярнъммю бшанпю сякнбмни мхфмеи кхмхх аюккюярю
REAL*4 R_I_PB                               ! онярнъммю бшанпю сякнбмни кхмхх ондаюккюярмнцн нямнбюмхъ

END MODULE N_
!----------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------
MODULE DOSTUP_									! хяонкэгсеряъ дкъ нопедекемхъ опюбю днярсою
USE DFLOGM				! НАЪГЮРЕКЭМЮЪ ЯЯШКЙЮ Б ЯКСВЮЕ DVF
INCLUDE 'RESOURCE.FD'	! БЙКЧВЮЧ ОЮПЮЛЕРПШ ПЕЯСПЯНБ
INTEGER*4 , DIMENSION(1:8) :: SEED				! люяяхбш дкъ цемепюжхх
REAL(8), DIMENSION(1:10000) :: ARRAY			! яксвюимшу вхяек
INTEGER(2), DIMENSION(1:10000) :: KEY_DOPUSK	! люяяхб дкъ упюмемхъ 
INTEGER*4  I,J
INTEGER*4  K									! мнлеп ъвеийх
INTEGER*4  KL									! яВЕРВХЙ НАПЮЫЕМХИ

CHARACTER *200 TEXT								! оепелеммше дкъ 
TYPE(DIALOG) PARAMETR_INFO						! тнплхпнбюмхъ
INTEGER*4  STATUS								! яннаыемхъ

END MODULE DOSTUP_
!----------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------
MODULE RAILWAY								! хяонкэгсеряъ дкъ оевюрх
CHARACTER(250) GRAF,NAME_GRAF			    ! тюик цксахм х осрэ гюохях
CHARACTER(250) VLAG,NAME_VLAG				! тюик бкюфмнярх х осрэ гюохях
CHARACTER(250) UGLUB,NAME_UGLUB				! тюик аюккюярмшу сцксакемхи х осрэ гюохях
CHARACTER(250) TOLSH,NAME_TOLSH				! тюик аюккюярмшу сцксакемхи х осрэ гюохях
CHARACTER(250) INFO,NAME_INFO				! тюик яксфеамши
CHARACTER(250), ALLOCATABLE ::  GRAF_NUMBER(:)	!люяяхб дкъ упюмемхъ хлем
CHARACTER(250), ALLOCATABLE ::  VLAG_NUMBER(:)	!люяяхб дкъ упюмемхъ хлем
CHARACTER(250), ALLOCATABLE ::  UGLUB_NUMBER(:)	!люяяхб дкъ упюмемхъ хлем
CHARACTER(250), ALLOCATABLE ::  TOLSH_NUMBER(:)	!люяяхб дкъ упюмемхъ хлем
! CHARACTER(1) STRKA
CHARACTER(200) S1,S2,S3,S4,S5,S6				! пюанвхе ъвеийх дкъ бшбндю б тюик .CSV
CHARACTER(200) S1_G,S2_G,S3_G,S4_G,S5_G		! пюанвхе ъвеийх дкъ бшбндю б тюик .CSV
CHARACTER(200), ALLOCATABLE :: S1_V(:),S2_V(:),S3_V(:),S4_V(:),S5_V(:),S6_V(:)		! пюанвхе ъвеийх дкъ бшбндю б тюик .CSV
CHARACTER(200), ALLOCATABLE :: S1_U(:),S2_U(:),S3_U(:),S4_U(:),S5_U(:)		! пюанвхе ъвеийх дкъ бшбндю б тюик .CSV
CHARACTER(200), ALLOCATABLE :: S1_U_UP(:),S2_U_UP(:),S3_U_UP(:),S4_U_UP(:),S5_U_UP(:)		! пюанвхе ъвеийх дкъ бшбндю б тюик .CSV
CHARACTER(200) STRING_200					! пюанвюъ ъвеийю
INTEGER*4 , ALLOCATABLE::ii_bal(:)				! рнвйх цпюмхж аюккюярю х нямнбюмхъ
INTEGER*4 , ALLOCATABLE::ii_osn(:)				! рнвйх цпюмхж аюккюярю х нямнбюмхъ
CHARACTER (8) DDATA
CHARACTER (10) DDATA1
CHARACTER (4) YER				! ЦНД
CHARACTER (2) MAUNS				! ЛЕЯЪЖ
CHARACTER (2) DAY				! ДЕМЭ
CHARACTER (2) HAUR				! ВЮЯ
CHARACTER (2) MINIT				! ЛХМСРЮ
CHARACTER (2) SEK			    ! ЯСЙСМДЮ
       integer(2)  wYear 
       integer(2)  wMonth 
       integer(2)  wDayOfWeek 
       integer(2)  wDay 
       integer(2)  wHour 
       integer(2)  wMinute 
       integer(2)  wSecond 
       integer(2)  wMilliseconds 
REAL(8) , ALLOCATABLE::VREMYA_MOUSE(:)              ! бпелъ дкъ рпюяя мю щйпюме
REAL(8) SEKK0 /0/                                   ! мювюкэмне бпелъ
REAL(8) SEKK  /0/                                   ! рейсыее бпелъ
REAL(4) TIME_VIDEO_FILE                             ! бпелъ напюгнбюмхъ бхден тюикю
CHARACTER*100 VIDEO_NAME/'ZERO'/                    ! хлъ тюикю я бхденхмтнплюжхеи
CHARACTER*100 FOTO_NAME/'ZERO'/                     ! хлъ тюикю я бхденхмтнплюжхеи
CHARACTER(200), ALLOCATABLE :: STRING_GRAF_CSV(:)	! хмтнплюжхъ дкъ мюйнокемхъ цксахм 
INTEGER*4  n_GRAF/0/								! пюглепмнярэ астепю
INTEGER*4  i_GRAF/0/								! явервхй гюонкмемхъ рейярнбнцн астепю цксахм

CHARACTER(200), ALLOCATABLE :: STRING_VLAG_CSV(:,:)	! хмтнплюжхъ дкъ мюйнокемхъ накюяреи бкюфмнярх
INTEGER*4  n_VLAG/0/								! пюглепмнярэ астепю

CHARACTER(200), ALLOCATABLE :: STRING_UGLUB_CSV(:,:)	! хмтнплюжхъ дкъ мюйнокемхъ накюяреи опняюднй
INTEGER*4  n_UGLUB/0/								    ! пюглепмнярэ астепю

CHARACTER(200), ALLOCATABLE :: STRING_TOLSH_CSV(:,:)	! хмтнплюжхъ дкъ мюйнокемхъ накюяреи опняюднй
INTEGER*4  n_TOLSH/0/								    ! пюглепмнярэ астепю
!----------------------------------------------------------------------------------------------------------------------
INTEGER*4  N_BIT											    ! вхякн ахр оепедювх дюммшу
!----------------------------------------------------------------------------------------------------------------------
INTEGER*4  IFL_WRITER/1/										! йкчв гюохях мю дхяй хгнапюфемхи
INTEGER*4  IFL_WRITER_TEXT/1/									! йкчв гюохях онкнфемхъ лшьх мю щйпюме
INTEGER*4  IFL_WRITER_KASKAD/1/								    ! йкчв гюохях мю дхяй рейярнб

END MODULE RAILWAY

MODULE GRAFICA
INTEGER*4  N_PRIZNAK_GRAFICA/0/		! опхгмюй бшбндю мю щйпюм
INTEGER*4  M_OUT/4/					! вхякн нрнапюфюелшу тхгхвеяйху оюпюлерпнб
CHARACTER(200) TEXT_2/" бКЮФМНЯРЭ"/
CHARACTER(200) TEXT_4/" дЕТНПЛЮРХБМНЯРЭ"/
INTEGER*4 ISTEP_FOR_GRAFICA
REAL*4 ISTEP_BETWEEN_TRASES
END MODULE GRAFICA
    
MODULE VLAG_CSV
REAL(4),ALLOCATABLE ::KILOMETR_FIRST(:)
REAL(4),ALLOCATABLE ::METR_FIRST(:)
REAL(4) IDL
REAL(4) KILOMETR_SEKOND,METR_SEKOND,IDL_SEKOND	! йхкнлерп х лерп йнмжю опедшдсыеи гюохях х ее дкхмю
REAL(4)	KILOMETR_OLD		
REAL(4)	METR_OLD		
REAL(4)	KILOMETR_FIRST_OLD		
REAL(4)	METR_FIRST_OLD
REAL(4)	IDL_OLD
END MODULE VLAG_CSV
    
MODULE UGLUB_CSV
REAL(4),ALLOCATABLE ::KILOMETR_FIRST_U(:)
REAL(4),ALLOCATABLE ::METR_FIRST_U(:)
REAL(4) IDL
REAL(4) KILOMETR_SEKOND,METR_SEKOND,IDL_SEKOND	! йхкнлерп х лерп йнмжю опедшдсыеи гюохях х ее дкхмю
REAL(4)	KILOMETR_OLD		
REAL(4)	METR_OLD		
REAL(4)	KILOMETR_FIRST_OLD		
REAL(4)	METR_FIRST_OLD
REAL(4)	IDL_OLD
END MODULE UGLUB_CSV

MODULE TOLSH_CSV
REAL(4),ALLOCATABLE ::KILOMETR_FIRST_T(:)
REAL(4),ALLOCATABLE ::METR_FIRST_T(:)
REAL(4) IDL
REAL(4) KILOMETR_SEKOND,METR_SEKOND,IDL_SEKOND	! йхкнлерп х лерп йнмжю опедшдсыеи гюохях х ее дкхмю
REAL(4)	KILOMETR_OLD		
REAL(4)	METR_OLD		
REAL(4)	KILOMETR_FIRST_OLD		
REAL(4)	METR_FIRST_OLD
REAL(4)	IDL_OLD
END MODULE TOLSH_CSV
