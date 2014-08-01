SUBROUTINE STOP_DLL()
USE PARAM_
USE PARAM_1
USE RAILWAY
USE GRAFICA
USE FReciverGlobals
USE GDI32		!ƒÀﬂ Œœ–≈ƒ≈À≈Õ»ﬂ —“–” “”–€ ¬–≈Ã≈Õ»
USE KERNEL32	!ƒÀﬂ «¿œ–Œ—¿ ¬–≈Ã≈Õ»
USE N_
USE VLAG_CSV
USE TOLSH_CSV
USE UGLUB_CSV
IMPLICIT NONE
! module FReciverGlobals

! integer*4, parameter, public :: SIZEOFAPPNAME = 100

if (isFirst.eq.1) then
    ! ≈ÒÎË ÌË Ó‰ÌÓÈ Ú‡ÒÒ˚ ÌÂ ·˚ÎÓ Ó·‡·ÓÚ‡ÌÓ - ÔÓÒÚÓ ‚˚ıÓ‰ËÏ
    return
end if



isFirst =1

TRACE_POS= 0 

cursor_pos = -1
TRACE_COUNT = 0 

configFileName = "geolib.cfg"C 
fullConfigFilePath = ""C 
CONSOLE_LIMIT = 200000 
!-------------------
ITEK=0
ISCH=0
KP_PRINT=1
kluch=0
K_FLAG=0
FL_START=0
K_PRINT=1
n_GRAF=0							! –¿«Ã≈–ÕŒ—“‹ ¡”‘≈–¿
i_GRAF=0							! —◊≈“◊»  «¿œŒÀÕ≈Õ»ﬂ “≈ —“Œ¬Œ√Œ ¡”‘≈–¿ √À”¡»Õ
n_VLAG=0								! –¿«Ã≈–ÕŒ—“‹ ¡”‘≈–¿
n_UGLUB=0								! –¿«Ã≈–ÕŒ—“‹ ¡”‘≈–¿
n_TOLSH=0								! –¿«Ã≈–ÕŒ—“‹ ¡”‘≈–¿
!N_PRIZNAK_GRAFICA=0		! œ–»«Õ¿  ¬€¬Œƒ¿ Õ¿ › –¿Õ
M_OUT=4					! ◊»—ÀŒ Œ“Œ¡–¿∆¿≈Ã€’ ‘»«»◊≈— »’ œ¿–¿Ã≈“–Œ¬
TEXT_2=" ¬Î‡ÊÌÓÒÚ¸"
TEXT_4=" ƒÂÙÓÏ‡ÚË‚ÌÓÒÚ¸"

1  DEALLOCATE (REFRACTION)			   
2  DEALLOCATE (BALL); 
3  DEALLOCATE(BALL_UP);    
4  DEALLOCATE (trace_buf);                       
5  DEALLOCATE (MyTrassa);
6  DEALLOCATE (IPR_CHEN);
7  DEALLOCATE(IPR);                                   
8  DEALLOCATE(IPR1);
9  DEALLOCATE(IPR1_UP);	   
   DEALLOCATE(STAR0);
   DEALLOCATE(FIN0);   
   DEALLOCATE(STAR1);   
   DEALLOCATE(FIN1);     
   DEALLOCATE(STAR1_OLD);   
   DEALLOCATE(KL_FORPRINT);
   DEALLOCATE(FLAG_FORPRINT);
!10 DEALLOCATE(UNIFORMITY_MIDL); 	            
11 DEALLOCATE(REFR_MIDL);
12 DEALLOCATE(REFR_MAX);
13 DEALLOCATE(RMOM_MAX);                   
14 DEALLOCATE(RMOM_MIDL);                                      
15 DEALLOCATE(R_BALL_MIDL);                
16 DEALLOCATE(R_BALL_MIDL_UP);                
17 DEALLOCATE(II_BAL);                 
18 DEALLOCATE(II_OSN);                                     
19 DEALLOCATE(REFR);                       
20 DEALLOCATE(RMOM);                      
!21 DEALLOCATE(UNIFORMITY);                
22 DEALLOCATE(FILE_NUMBER);                
23 DEALLOCATE(GRAF_NUMBER);                
24 DEALLOCATE(VLAG_NUMBER);                
25 DEALLOCATE(UGLUB_NUMBER);  
26 DEALLOCATE(TOLSH_NUMBER);
27 DEALLOCATE(R_BALL);                           
28 DEALLOCATE(R_BALL_UP);
                          
! ƒÀﬂ √–¿‘» »------------------------------------------------------------------------------------------------
29 DEALLOCATE(trace_draw_pos);               
30 DEALLOCATE(trace_draw_pos_1);            
31 DEALLOCATE(trace_draw_pos_2);             
32 DEALLOCATE(trace_draw_pos_3);             
33 DEALLOCATE(trace_draw_pos_4);             
34 DEALLOCATE(trace_draw_pos_5);             
!-------------------------------------------------------------------------------------------------------------
35 DEALLOCATE(N_WRITE_VLAG);               
36 DEALLOCATE(N_WRITE_UGLUB); 
37 DEALLOCATE(N_WRITE_TOLSH);                                                    
38 DEALLOCATE(STRING_GRAF_CSV);              
39 DEALLOCATE(STRING_VLAG_CSV);         
40 DEALLOCATE(STRING_UGLUB_CSV);
41 DEALLOCATE(STRING_TOLSH_CSV);        
42 DEALLOCATE (S1_V);                           
43 DEALLOCATE (S2_V);                           
44 DEALLOCATE (S3_V);                            
45 DEALLOCATE (S4_V);                          
46 DEALLOCATE (S5_V);  
47 DEALLOCATE (S6_V);                            
48 DEALLOCATE (S1_U);                           
49 DEALLOCATE (S2_U);                           
50 DEALLOCATE (S3_U);                         
51 DEALLOCATE (S4_U);                          
52 DEALLOCATE (S5_U);  
53 DEALLOCATE (S1_U_UP);                           
54 DEALLOCATE (S2_U_UP);                           
55 DEALLOCATE (S3_U_UP);                         
56 DEALLOCATE (S4_U_UP);                          
57 DEALLOCATE (S5_U_UP);
!-------------------------------------------------------------------------------------------------                      
58 DEALLOCATE (YY);	              
59 DEALLOCATE (YY1);                  
60 DEALLOCATE (IA);                              
61 DEALLOCATE (BUFF); 
62 IF(KEY_MIDL.GT.0)&
   DEALLOCATE (BUFF_MIDDLE);                           
63 DEALLOCATE (SUM_RAB_MIDL);            
64 IF(KEY_MIDL.GT.0)&
   DEALLOCATE (SUM_MIDDLE);            
65 DEALLOCATE (RDM);                              
66 DEALLOCATE (RDM1);
67 DEALLOCATE (RDM2);
68 DEALLOCATE (HH);    
69 DEALLOCATE (IHH_50);   
70 DEALLOCATE (T_N);
71 DEALLOCATE (VREMYA_MOUSE);
   DEALLOCATE (KILOMETR_FIRST)
   DEALLOCATE (METR_FIRST)    
   DEALLOCATE (KILOMETR_FIRST_U)
   DEALLOCATE (METR_FIRST_U)    
   DEALLOCATE (KILOMETR_FIRST_T)
   DEALLOCATE (METR_FIRST_T) 
CLOSE(777)
CLOSE(40)
CLOSE(15)
CLOSE(10)
CLOSE(20)
CLOSE(30)
!CLOSE(8)
CLOSE(999)

RETURN
END 

!open(unit=100, FILE='D:/OUTPUT.OUT')
! write(100,*)kilometr,METR
