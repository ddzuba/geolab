SUBROUTINE POINT_ARRAY()
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
USE FReciverGlobals
USE VLAG_CSV
USE TOLSH_CSV
USE UGLUB_CSV
IMPLICIT NONE
INTEGER*4 I
!--------------------------------------------------------------------------------------------
! CALL DOSTUP()								! опнбепйю опюбю днярсою
!--------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------
! гюохяэ мю дхяй-----------------------------------------------------------------------------------
	N_FILE_DISK=0						! мнлеп тюикю гюохях мю дхяй
	STR(1)=  '0'; STR(2)= '1'; STR(3)= '2'; STR(4)= '3'; STR(5)= '4'; 
	STR(6)=  '5'; STR(7)= '6'; STR(8)= '7'; STR(9)= '8';STR(10) ='9';
	STR(11)='10';STR(12)='11';STR(13)='12';STR(14)='13';STR(15)='14';
	STR(16)='15';STR(17)='16';STR(18)='17';STR(19)='18';STR(20)='19';
	STR(21)='20';STR(22)='21';STR(23)='22';STR(24)='23';STR(25)='24';
	STR(26)='25';STR(27)='26';STR(28)='27';STR(29)='28';STR(30)='29';
	STR(31)='30';STR(22)='31';STR(33)='32';STR(34)='33';STR(35)='34';
	STR(36)='35';STR(37)='36';STR(38)='37';STR(39)='38';STR(40)='39';
	STR(41)='40';STR(22)='41';STR(43)='42';STR(44)='43';STR(45)='44';
	STR(46)='45';STR(47)='46';STR(48)='47';STR(49)='48';STR(50)='49'
!---------------------------------------------------------------------------------------------------
1	ALLOCATE (REFRACTION(N_CHEN,N_MIDL_U));         REFRACTION=0.0			   
2   ALLOCATE (BALL(N_CHEN,N_MIDL_BALL));	        BALL=0.0
3   ALLOCATE (BALL_UP(N_CHEN,N_MIDL_BALL));	        BALL_UP=0.0
4   ALLOCATE (trace_buf(N_CHEN*512));               trace_buf=0.0         	! астеп дкъ пхянбюмхъ
5	ALLOCATE (MyTrassa(N_CHEN*512));	            MyTrassa=0.0			! рпюяяш б вхякеммнл бхде
6	ALLOCATE(IPR_CHEN(N_CHEN));                     IPR_CHEN=0              ! опхгмюй ондюбкемхъ йюмюкю
7	ALLOCATE(IPR(N_CHEN));	                        IPR=0                   ! йкчв дкъ бкюфмнцн свюярйю
8	ALLOCATE(IPR1(N_CHEN));	                        IPR1=0				   	! йкчв дкъ пюяверю сцксакемхи
9   ALLOCATE(IPR1_UP(N_CHEN));                      IPR1_UP=0               ! йкчв дкъ пюяверю аюккюярмшу сцксакемхи
    ALLOCATE(STAR0(N_CHEN));                        STAR0=0                 ! хмтнплюжхъ дкъ тнплхпнбюмхъ цпюмхж гнм сбкюфмемхъ опх оевюрх
    ALLOCATE(FIN0(N_CHEN));                         FIN0=0                  ! хмтнплюжхъ дкъ тнплхпнбюмхъ цпюмхж гнм сбкюфмемхъ опх оевюрх
    ALLOCATE(STAR1(N_CHEN));                        STAR1=0                 ! хмтнплюжхъ дкъ тнплхпнбюмхъ цпюмхж гнм сбкюфмемхъ опх оевюрх
    ALLOCATE(FIN1(N_CHEN));                         FIN1=0                  ! хмтнплюжхъ дкъ тнплхпнбюмхъ цпюмхж гнм сбкюфмемхъ опх оевюрх
    ALLOCATE(STAR1_OLD(N_CHEN));                    STAR1=0                 ! хмтнплюжхъ дкъ тнплхпнбюмхъ цпюмхж гнм сбкюфмемхъ опх оевюрх
    ALLOCATE(KL_FORPRINT(N_CHEN));                  KL_FORPRINT=0           ! хмтнплюжхъ дкъ тнплхпнбюмхъ гнм сбкюфмемхъ опх оевюрх
    ALLOCATE(FLAG_FORPRINT(N_CHEN));                FLAG_FORPRINT=0         ! хмтнплюжхъ дкъ тнплхпнбюмхъ гнм сбкюфмемхъ опх оевюрх
11  ALLOCATE(REFR_MIDL(N_CHEN));                    REFR_MIDL=0.0
12  ALLOCATE(REFR_MAX(N_CHEN));                     REFR_MAX=0.0
13  ALLOCATE(RMOM_MAX(N_CHEN));                     RMOM_MAX=0.0
14	ALLOCATE(RMOM_MIDL(N_CHEN));                    RMOM_MIDL=0.0
15	ALLOCATE(R_BALL_MIDL(N_CHEN));                  R_BALL_MIDL=0.0
16	ALLOCATE(R_BALL_MIDL_UP(N_CHEN));                  R_BALL_MIDL=0.0
17	ALLOCATE(II_BAL(N_CHEN));                       II_BAL=0
18	ALLOCATE(II_OSN(N_CHEN));                       II_OSN=0
19	ALLOCATE(REFR(N_CHEN));                         REFR=0.0
20	ALLOCATE(RMOM(N_CHEN));                         RMOM=0.0
!21	ALLOCATE(UNIFORMITY(N_CHEN));                   UNIFORMITY=0.0
22	ALLOCATE(FILE_NUMBER(N_CHEN));                  FILE_NUMBER=''
23	ALLOCATE(GRAF_NUMBER(N_CHEN));                  GRAF_NUMBER=''
24	ALLOCATE(VLAG_NUMBER(N_CHEN));                  VLAG_NUMBER=''
25	ALLOCATE(UGLUB_NUMBER(N_CHEN));                 UGLUB_NUMBER=''
26	ALLOCATE(TOLSH_NUMBER(N_CHEN));                 TOLSH_NUMBER=''
27	ALLOCATE(R_BALL(N_CHEN));                       R_BALL=0.0             
28	ALLOCATE(R_BALL_UP(N_CHEN));                       R_BALL_UP=0.0             
! дкъ цпютхйх------------------------------------------------------------------------------------------------
29  ALLOCATE(trace_draw_pos(N_CHEN));               
30  ALLOCATE(trace_draw_pos_1(N_CHEN));            
31  ALLOCATE(trace_draw_pos_2(N_CHEN));             
32  ALLOCATE(trace_draw_pos_3(N_CHEN));             
33  ALLOCATE(trace_draw_pos_4(N_CHEN));             
34  ALLOCATE(trace_draw_pos_5(N_CHEN));             
!-------------------------------------------------------------------------------------------------------------
35  ALLOCATE(N_WRITE_VLAG(N_CHEN));                     N_WRITE_VLAG=0
36  ALLOCATE(N_WRITE_UGLUB(N_CHEN));                    N_WRITE_UGLUB=0
37  ALLOCATE(N_WRITE_TOLSH(N_CHEN));                    N_WRITE_TOLSH=0
N_GRAF= N_CHEN*200000/N_PRINT                                                 ! дкхмю астепю цксахм
38  ALLOCATE(STRING_GRAF_CSV(N_GRAF));                  STRING_GRAF_CSV=''        ! пюглеыюел астеп цксахм
N_VLAG= 200000                                                                ! дкхмю астепю цксахм
39  ALLOCATE(STRING_VLAG_CSV(N_VLAG,N_CHEN));           STRING_VLAG_CSV =''       ! пюглеыюел астеп цксахм
N_UGLUB=200000                                                                ! дкхмю астепю цксахм
40  ALLOCATE(STRING_UGLUB_CSV(N_UGLUB,N_CHEN));         STRING_UGLUB_CSV=''       ! пюглеыюел астеп цксахм
N_TOLSH=200000                                                                 ! дкхмю астепю цксахм
41  ALLOCATE(STRING_TOLSH_CSV(N_TOLSH,N_CHEN));         STRING_TOLSH_CSV=''       ! пюглеыюел астеп цксахм
42  ALLOCATE (S1_V(N_CHEN));                            S1_V=''
43  ALLOCATE (S2_V(N_CHEN));                            S2_V=''
44  ALLOCATE (S3_V(N_CHEN));                            S3_V=''
45  ALLOCATE (S4_V(N_CHEN));                            S4_V=''
46  ALLOCATE (S5_V(N_CHEN));                            S5_V=''
47  ALLOCATE (S6_V(N_CHEN));                            S6_V=''
48  ALLOCATE (S1_U(N_CHEN));                            S1_U=''
49  ALLOCATE (S2_U(N_CHEN));                            S2_U=''
50  ALLOCATE (S3_U(N_CHEN));                            S3_U=''
51  ALLOCATE (S4_U(N_CHEN));                            S4_U=''
52  ALLOCATE (S5_U(N_CHEN));                            S5_U=''
53  ALLOCATE (S1_U_UP(N_CHEN));                         S1_U_UP=''
54  ALLOCATE (S2_U_UP(N_CHEN));                         S2_U_UP=''
55  ALLOCATE (S3_U_UP(N_CHEN));                         S3_U_UP=''
56  ALLOCATE (S4_U_UP(N_CHEN));                         S4_U_UP=''
57  ALLOCATE (S5_U_UP(N_CHEN));                         S5_U_UP=''
!-------------------------------------------------------------------------------------------------
58  ALLOCATE (YY(LM_WINDOW,N_CHEN));	                YY = 0				        ! гмювемхъ цпюмхжш аюккюярю дкъ хмрепонкъжхи
59  ALLOCATE (YY1(LM_WINDOW,N_CHEN));                   YY1 = 0					    ! гмювемхъ  ондаюккюярмни цпюмхжш дкъ хмрепнкъжхи
60  ALLOCATE (IA(N_CHEN));                              IA=0                    	! явервхй гюонкмемхъ люяяхбнб цпюмхж

TRACE_COUNT=1								                                        ! менамскъелши явервхй рпюяя 
											                                        ! дкъ мюмеяемхъ йхкнлерпнбшу ярнканб
!--------------------------------------------------------------------
							! дкъ тнплюрю оепедювх дюммшу хмрецпюкю

61	ALLOCATE (BUFF(N_CHEN,N,L_REGION));                 BUFF=0.0				    ! Mюяяхб дкъ сяпедмемхъ
62	IF(KEY_MIDL.GT.0)&
    ALLOCATE (BUFF_MIDDLE(N_CHEN,N,KEY_MIDL));          BUFF_MIDDLE=0.0             ! Mюяяхб дкъ сяпедмемхъ
63  ALLOCATE (SUM_RAB_MIDL(N_CHEN,N));                  SUM_RAB_MIDL=0.0			! мюйнокемхе рпюяя дкъ сяпедмемхъ
64  IF(KEY_MIDL.GT.0)&
    ALLOCATE (SUM_MIDDLE(N_CHEN,N));                    SUM_MIDDLE=0.0			    ! мюйнокемхе рпюяя дкъ сяпедмемхъ
65	ALLOCATE (RDM(N));                                  RDM=0.0                     ! пюанвхи люяяхб
66	ALLOCATE (RDM1(N));                                 RDM1=0.0                    ! пюанвхи люяяхб
67	ALLOCATE (RDM2(N,N_CHEN));                          RDM2=0.0                    ! пюанвхи люяяхб
68  ALLOCATE (HH(N));                                   HH=0.0                      ! люяяхб цксахм
69  ALLOCATE (IHH_50(N));                               IHH_50=0.0                  ! люяяхб мнлепнб рнвей я ьюцнл 50 ял  
70  ALLOCATE (T_N(N));                                  T_N=0.0                     ! люяяхб бпелемDEALLOCUU   
    ALLOCATE(KILOMETR_FIRST(N_CHEN))                                                ! мювюкэмюъ йннпдхмюрю свюярйю
    ALLOCATE(METR_FIRST(N_CHEN))                                                    ! мювюкэмюъ йннпдхмюрю свюярйю
    ALLOCATE(KILOMETR_FIRST_U(N_CHEN))                                                ! мювюкэмюъ йннпдхмюрю свюярйю
    ALLOCATE(METR_FIRST_U(N_CHEN))                                                    ! мювюкэмюъ йннпдхмюрю свюярйю
    ALLOCATE(KILOMETR_FIRST_T(N_CHEN))                                                ! мювюкэмюъ йннпдхмюрю свюярйю
    ALLOCATE(METR_FIRST_T(N_CHEN))                                                    ! мювюкэмюъ йннпдхмюрю свюярйю

    RABTIME=T_RAZV*0.3/SQRT(PREL)/2.
	JJ_H=HH_B/(30*T_RAZV)					                                        ! пюяярнъпхе дн онбепумнярх, EPS- дкъ бнгдсую 
!----------------------------------------------------------------------------------------------------
	KILOMETR_0=KILOMETR					! гюонлхмюел мювюкэмше йннпдхмюрш дкъ 
	METR_0=METR							! тнплхпнбюмхъ хмтнплюжхнммнцн тюикю
	KILOMETR_1=KILOMETR					! гюонлхмюел мювюкэмше йннпдхмюрш дкъ 
	METR_1=METR							! тнплхпнбюмхъ тюикю .BMP
nfnt=14									! ПЮГЛЕП ЬПХТРЮ
nfnt1=10									! ЬХПХМЮ ОНКНЯШ
img_hh =(img_h-4*(nfnt+1)-4*N_CHEN*(nfnt1+1)-20)/N_CHEN				! БШЯНРЮ ПХЯСМЙЮ ЙЮМЮКЮ
IF(img_hh.gt.511-j_level+3) img_hh=511-j_level+3	! дкъ ндмнйюмюкэмнцн бюпхюмрю
JJ2_GR=IMG_HH+J_LEVEL-3				                                    ! мхфмъъ цпюмхжю цпсмрю

DO N_CHEN_TEK=1,N_CHEN								! жхйк он йюмюкюл
trace_draw_pos(N_CHEN_TEK)=	img_h-N_CHEN*img_hh+(N_CHEN_TEK-1)*img_hh ! МЮВЮКН ХГНАПЮФЕМХЪ РПЮЯЯШ
trace_draw_pos_1(N_CHEN_TEK)=2*nfnt+1+(N_CHEN_TEK-1)*(nfnt1+1) ! РЮМЦЕМЯ СЦКЮ МЮЙКНМЮ
END DO
DO N_CHEN_TEK=1,N_CHEN
trace_draw_pos_2(N_CHEN_TEK)=nfnt+(nfnt1+0)+trace_draw_pos_1(N_CHEN)+(N_CHEN_TEK-1)*(nfnt1+1) ! НРПЮФЮРЕКЭМЮЪ ЯОНЯНАМНЯРЭ
END DO
DO N_CHEN_TEK=1,N_CHEN
trace_draw_pos_3(N_CHEN_TEK)=nfnt+(nfnt1+0)+trace_draw_pos_2(N_CHEN)+(N_CHEN_TEK-1)*(nfnt1+1) ! НДМНПНДМНЯРХ ЯКНЪ
END DO
DO N_CHEN_TEK=1,N_CHEN
trace_draw_pos_4(N_CHEN_TEK)=nfnt+(nfnt1+0)+trace_draw_pos_3(N_CHEN)+(N_CHEN_TEK-1)*(nfnt1+1) ! ЛНЛЕМР
trace_draw_pos_5(N_CHEN_TEK)=(N_CHEN_TEK-1)*img_hh    ! ОНКНФЕМХЕ ПХЯСМЙЮ
END DO
I_GRAF=0		! мнбне мюйнокемхе рейярнбни хмтнплюжхх
! тнплхпнбюмхе пюанвху йнмярюмр дкъ юмюкхгю
    LM_WINDOW1=LM_WINDOW-1
	LM_WINDOW2=LM_WINDOW/2
!------------------------------------------------------------------------------------------------------------------------------------
! дкъ нопедекемхъ йюпрхмйх
71  ALLOCATE (VREMYA_MOUSE(IMG_W))
RETURN
END
!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*) 

