SUBROUTINE DEPTH_TRASSA()
USE PARAM_
USE N_
IMPLICIT NONE
REAL*8  RL,TR,ALF,BET,SDV,SDV1,T,T1,T2,ZNAK,ZNAK1,RAB,RAB1,H2,RN1,RN2
INTEGER*4 I

!	BAZ     пюяярнъмхе лефдс юмреммюлх
!   HH_B      бшянрю ондзелю юмреммш 
!   RN1     онйюгюрекэ опекнлкемхъ бнгдсую
!	RN2     онйюгюрекэ опекнлкемхъ япедш
!	T_RAZV      бпелеммни хмрепбюк лефдс рнвйюлх рпюяяш
!	T_N     ьйюкю бпелем
!   ALF     сцнк оюдемхъ
!   BET     сцнк опекнлкемхъ

    RN1=1.0D0                                   !онйюгюрекэ опекнлкемхъ бнгдсую
	RN2=DSQRT(DBLE(PREL))                       !онйюгюрекэ опекнлкемхъ япедш	

    IF(HH_B.LT.0.1) THEN                        ! юмреммю мю онбепумнярх
    RL=DBLE(BAZ)           	                    ! осрэ 
	TR=RL*RN2/DBLE(VC0)					        ! бпелъ онбепумнярмнцн яхцмюкю
    ELSE                                        ! юмреммю мюд онбепумнярэч
    RL=2.*SQRT(HH_B**2+BAZ**2/4)	            ! осрэ нр онбепумнярх цпсмрю
	TR=RL*RN1/VC0						        ! бпелъ онбепумнярмнцн яхцмюкю       
    END IF

! пюяярюбкъел бпелъ, рюй врнаш мнкэ опхьEкяъ мю рнвйс я мнлепнл J_LEVEL
	T_N(J_LEVEL) = TR				        	! ьйюкю бпелемх 
	IF(J_LEVEL.GT.2) THEN
        DO I=J_LEVEL-1,1,-1
	    T_N(I)=T_N(I+1)-T_RAZV
	    END DO
	END IF
	IF(J_LEVEL.LT.N-1) THEN
	    DO I=J_LEVEL+1,N
	    T_N(I)=T_N(I-1)+T_RAZV				
        END DO
    END IF
    
!************* юмремю мюд цпсмрнл ****************************

IF(HH_B.LT.0.1)THEN                                     ! юмреммю мю онбепумнярх 
    DO I=1,N                                            ! хдел он бяел рнвйюл рпюяяш; дкъ йюфдни рнвйх рпюяяш
		IF(I.LT.J_LEVEL) THEN                           ! пюгдекъел япедш
    		HH(I)=0.D0                                  ! еякх бшье спнбмъ цпсмрю
		ELSE							                ! еякх мхфе спнбмъ цпсмрю 
    		T=DBLE(T_N(I)/2.0)                          ! спюбмемхе:t1+t2=t
	        HH(I)=DSQRT((T*VC0/RN2)**2-BAZ**2/4.D0)        ! осрэ 
        END IF 
    END DO
ELSE                                                    ! юмреммю мюд онбепумнярэч
       DO  I=1,N                                        ! хдел он бяел рнвйюл рпюяяш; дкъ йюфдни рнвйх рпюяяш пеьюел хрепюрхбмн	
 		IF(I.LT.J_LEVEL) THEN                           ! пюгдекъел япедш
    		HH(I)=0.D0                                  ! еякх бшье спнбмъ цпсмрю
        ELSE                                            ! еякх мхфе спнбмъ цпсмрю 			                
    		ZNAK=0.D0;ZNAK1=0.D0            
            T=DBLE(T_N(I)/2.0)                          ! спюбмемхе:t1+t2=t
	    	SDV=.0D0
	    	SDV1=.1D0*(I-J_LEVEL)/10
! оепбши ьюц - йняхмся сцкю лемэье 1   
2	    	T1=T-SDV                                    						
	    	T2=SDV
	    	RAB=DBLE(HH_B*RN1/T1/VC0)                   ! спюбмемхе: H/cos(alf)=c*t1/n1 => cos(alf)=H*n1/(c*t1)
                IF(RAB.GT.1.D0) RAB=1.D0 !THEN
! йнмеж оепбнцн ьюцю
! бняярюмюбкхбюел сцкш х цксахмс
		    ALF=DACOS(RAB)                              ! бшвхякъел сцкш оюдемхъ х опекнлкемхъ
            BET=DASIN(DSIN(ALF)*(RN1/RN2))              ! спюбмемхе:sin(alf)/sin(bet)=n2/n1
 		    H2=DBLE(VC0/RN2*T2)*DCOS(BET)               ! спюбмемхе: H/cos(bet)=c*t2/n2
! брнпни ьюц - опнбепъел рнфдеярбн               
		    RAB1=DBLE(HH_B)*DTAN(ALF)+H2*DTAN(BET)      ! рнфдеярбн   
            ZNAK=RAB1-DBLE(BAZ/2.D0)       
    
            IF(ABS(ZNAK).LT.0.01D0) GOTO 4              ! еякх рнфдеярбн бшонкмемн - сундхл
       
		        IF(ZNAK*ZNAK1.LT.0.D0) THEN             ! еякх бхкйю    
		        SDV=SDV-SDV1                            ! бнгбпюыюеляъ б опедшдсысч рнвйс
		        SDV1=SDV1/2.D0                          ! декхл ьюц ононкюл
		        SDV=SDV+SDV1                            ! срнвмъел пеьемхе опх мнбнл бпелемх 
		        GOTO 2                                  
                END IF                                  ! еякх ме бхкйю 
            
                IF(ABS(ZNAK1).GT.ABS(ZNAK)) THEN        ! х хдел ме б рс ярнпнмс
                ZNAK1=ZNAK
		        SDV=SDV+SDV1                            ! срнвмъел пеьемхе опх мнбнл бпелемх 
                GOTO 2
                END IF
                
                IF(ABS(ZNAK1).LE.ABS(ZNAK)) THEN        ! х хдел ме б рс ярнпнмс ! хдел я рел фе ьюцнл 
                ZNAK1=ZNAK
		        SDV=SDV+SDV1                            ! срнвмъел пеьемхе опх мнбнл бпелемх 
                GOTO 2
                END IF                

            END IF
4		    HH(I)=H2  
    END DO
END IF
RETURN
END SUBROUTINE DEPTH_TRASSA