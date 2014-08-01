SUBROUTINE DEPTH_TRASSA()
USE PARAM_
USE N_
IMPLICIT NONE
REAL*8  RL,TR,ALF,BET,SDV,SDV1,T,T1,T2,ZNAK,ZNAK1,RAB,RAB1,H2,RN1,RN2
INTEGER*4 I

!	BAZ     ���������� ����� ���������
!   HH_B      ������ ������� ������� 
!   RN1     ���������� ����������� �������
!	RN2     ���������� ����������� �����
!	T_RAZV      ��������� �������� ����� ������� ������
!	T_N     ����� ������
!   ALF     ���� �������
!   BET     ���� �����������

    RN1=1.0D0                                   !���������� ����������� �������
	RN2=DSQRT(DBLE(PREL))                       !���������� ����������� �����	

    IF(HH_B.LT.0.1) THEN                        ! ������� �� �����������
    RL=DBLE(BAZ)           	                    ! ���� 
	TR=RL*RN2/DBLE(VC0)					        ! ����� �������������� �������
    ELSE                                        ! ������� ��� ������������
    RL=2.*SQRT(HH_B**2+BAZ**2/4)	            ! ���� �� ����������� ������
	TR=RL*RN1/VC0						        ! ����� �������������� �������       
    END IF

! ����������� �����, ��� ����� ���� ����E��� �� ����� � ������� J_LEVEL
	T_N(J_LEVEL) = TR				        	! ����� ������� 
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
    
!************* ������ ��� ������� ****************************

IF(HH_B.LT.0.1)THEN                                     ! ������� �� ����������� 
    DO I=1,N                                            ! ���� �� ���� ������ ������; ��� ������ ����� ������
		IF(I.LT.J_LEVEL) THEN                           ! ��������� �����
    		HH(I)=0.D0                                  ! ���� ���� ������ ������
		ELSE							                ! ���� ���� ������ ������ 
    		T=DBLE(T_N(I)/2.0)                          ! ���������:t1+t2=t
	        HH(I)=DSQRT((T*VC0/RN2)**2-BAZ**2/4.D0)        ! ���� 
        END IF 
    END DO
ELSE                                                    ! ������� ��� ������������
       DO  I=1,N                                        ! ���� �� ���� ������ ������; ��� ������ ����� ������ ������ ����������	
 		IF(I.LT.J_LEVEL) THEN                           ! ��������� �����
    		HH(I)=0.D0                                  ! ���� ���� ������ ������
        ELSE                                            ! ���� ���� ������ ������ 			                
    		ZNAK=0.D0;ZNAK1=0.D0            
            T=DBLE(T_N(I)/2.0)                          ! ���������:t1+t2=t
	    	SDV=.0D0
	    	SDV1=.1D0*(I-J_LEVEL)/10
! ������ ��� - ������� ���� ������ 1   
2	    	T1=T-SDV                                    						
	    	T2=SDV
	    	RAB=DBLE(HH_B*RN1/T1/VC0)                   ! ���������: H/cos(alf)=c*t1/n1 => cos(alf)=H*n1/(c*t1)
                IF(RAB.GT.1.D0) RAB=1.D0 !THEN
! ����� ������� ����
! ��������������� ���� � �������
		    ALF=DACOS(RAB)                              ! ��������� ���� ������� � �����������
            BET=DASIN(DSIN(ALF)*(RN1/RN2))              ! ���������:sin(alf)/sin(bet)=n2/n1
 		    H2=DBLE(VC0/RN2*T2)*DCOS(BET)               ! ���������: H/cos(bet)=c*t2/n2
! ������ ��� - ��������� ���������               
		    RAB1=DBLE(HH_B)*DTAN(ALF)+H2*DTAN(BET)      ! ���������   
            ZNAK=RAB1-DBLE(BAZ/2.D0)       
    
            IF(ABS(ZNAK).LT.0.01D0) GOTO 4              ! ���� ��������� ��������� - ������
       
		        IF(ZNAK*ZNAK1.LT.0.D0) THEN             ! ���� �����    
		        SDV=SDV-SDV1                            ! ������������ � ���������� �����
		        SDV1=SDV1/2.D0                          ! ����� ��� �������
		        SDV=SDV+SDV1                            ! �������� ������� ��� ����� ������� 
		        GOTO 2                                  
                END IF                                  ! ���� �� ����� 
            
                IF(ABS(ZNAK1).GT.ABS(ZNAK)) THEN        ! � ���� �� � �� �������
                ZNAK1=ZNAK
		        SDV=SDV+SDV1                            ! �������� ������� ��� ����� ������� 
                GOTO 2
                END IF
                
                IF(ABS(ZNAK1).LE.ABS(ZNAK)) THEN        ! � ���� �� � �� ������� ! ���� � ��� �� ����� 
                ZNAK1=ZNAK
		        SDV=SDV+SDV1                            ! �������� ������� ��� ����� ������� 
                GOTO 2
                END IF                

            END IF
4		    HH(I)=H2  
    END DO
END IF
RETURN
END SUBROUTINE DEPTH_TRASSA