SUBROUTINE ANALIZ()
USE FReciverGlobals
USE PARAM_
USE PARAM_1
USE RAILWAY
USE DFLIB
USE DFWIN
USE N_
IMPLICIT NONE 
REAL*4 B0,B0P,RRAB						! ������� ��������� ��� ����������� �������� �������� � ��������
INTEGER*4 I,J,JA,LM_WIND_TEK,LM_WIND1_TEK				! ������� ����������
INTEGER*4 I_B_DELT,I_PB1

INTEGER*4 MIDL,MIDL1,MIDL2,I_BOLD,I_PBOLD

!!----------------------------------------------
IA(N_CHEN_TEK)=1+IA(N_CHEN_TEK)
!----------------------------------------------
	IF(IA(N_CHEN_TEK).EQ.1) THEN	! ��������� ����������
    
	JJ1_B_0=J_LEVEL+1+IDELTA4*R_I_B                                         ! ��  30 �� - �������� ������� �����
	JJ2_B_0=J_LEVEL+IDELTA4*R_I_B1                                          ! �� 75 ��
	JJ1_PB_0=JJ2_B_0+1						                                ! ��  75 ��  ��������!
	JJ2_PB_0=JJ1_PB_0+IDELTA4*R_I_PB	   	        	                    ! �� 150 ��					                        
    
	
    END IF
       
!----------------------------------------------------------------------------------------
! ����� ������
JSDVIG_TR=(N_CHEN_TEK-1)*(n) ! n=SIZE-1 - �� ����� N_1
!--------------------------------------------------------------------
! ���������� ����; 
! ���� ����������� �������� � ������ �� ��������� ��������� �� ������� �������������� ����
! JJ1_B_0 - JJ2_PB_0
!----------------------------------------------------------------------------------------							   ! 
I_B=0
RRAB=0.
	DO I=JJ1_B_0,JJ2_B_0
	IF(TRACE_BUF(I+JSDVIG_TR).GT.0)THEN
            IF(RRAB.LT.TRACE_BUF(I+JSDVIG_TR))THEN
            RRAB= TRACE_BUF(I+JSDVIG_TR)
            I_B=I
            END IF        
	END IF											 
    END DO
!------------------------------------------------------------------------
!	������������� ���������; ���� ������ �������� � ������ �� ������� ��������� �������� �� ���
!----------------------------------------------
I_PB=0
RRAB=0.
J=JJ1_PB_0              ! ���� ��� ��������� � ��������
IF(I_B.NE.0)J=I_B+1
	DO I=J,JJ2_PB_0
	IF(TRACE_BUF(I+JSDVIG_TR).GT.0)THEN				! �������� � ������ �����
            IF(RRAB.LT.TRACE_BUF(I+JSDVIG_TR))THEN
            RRAB=TRACE_BUF(I+JSDVIG_TR)
            I_PB=I
            I_PB1=I                                  ! ��� ������������� ����������� ����
            END IF            
	END IF
	END DO
1   CONTINUE 
!-----------------------------------------------------------------------------------------
! ���� �������� � �������� ������ �� ������� ��� �������� ���������, �� �������������� ��
!-----------------------------------------------------------------------------------------
!   IF(I_PB1.NE.0.AND.I_B.NE.0.AND.IABS(I_PB1-JJ2_B_0).LT.IABS(I_B-JJ2_B_0))THEN	
!    I_B=I_PB1						! ��������
!    I_PB=0 
!    RRAB=0.
!            DO I=I_B+1,JJ2_PB_0 
!	        IF(TRACE_BUF(I+JSDVIG_TR).GT.0)THEN				! �������� � ������ �����
!                IF(RRAB.EQ.0)THEN
!                RRAB=TRACE_BUF(I+JSDVIG_TR)
!                I_PB=I
!                ELSE
!                    IF(RRAB.GT.TRACE_BUF(I+JSDVIG_TR))THEN
!                    GOTO 2
!                    ELSE
!                    RRAB=TRACE_BUF(I+JSDVIG_TR)
!                    I_PB=I
!                    END IF    
!                 END IF            
!	        END IF
!	        END DO        
!		END IF
!2 CONTINUE 
!---------------------------------------------------------------------------------------
! ��������� ����� ��������� ����� �������� � �������������� ���������
!---------------------------------------------------------------------------------------
IF(IA(N_CHEN_TEK).LE.LM_WINDOW) THEN                ! ������ �������
    YY(IA(N_CHEN_TEK),N_CHEN_TEK)=I_B
    YY1(IA(N_CHEN_TEK),N_CHEN_TEK)=I_PB
            IF(I_B.EQ.0) THEN 
            I_B=(JJ1_B_0+JJ2_B_0)/2
            YY(IA(N_CHEN_TEK),N_CHEN_TEK)=I_B    
            END IF
            IF(I_PB.EQ.0) THEN 
            I_PB=(JJ1_PB_0+JJ2_PB_0)/2
            YY1(IA(N_CHEN_TEK),N_CHEN_TEK)=I_PB             
            END IF    
END IF
!----------------------------------------------
! ���������� ������� �������� � ��������� ��� ������ � ����
!----------------------------------------------
IF(IA(N_CHEN_TEK).GT.LM_WINDOW) THEN  
    MIDL=0
    JA=0
	DO  I=1,LM_WINDOW1          
    YY(I,N_CHEN_TEK)=YY(I+1,N_CHEN_TEK)
        IF(YY(I,N_CHEN_TEK).NE.0) THEN
        JA=JA+1
        MIDL=MIDL+YY(I,N_CHEN_TEK)
        END IF
    END DO
    
    IF(JA.NE.0) THEN
        MIDL=NINT(MIDL*1./JA)
    ELSE
        MIDL=(JJ1_B_0+JJ2_B_0)/2
    END IF 
        MIDL1=NINT(MIDL*PARAM_MIDL1)
        MIDL2=NINT(MIDL*PARAM_MIDL2)
        
        IF(I_B.EQ.0) THEN                                                  !���� �� ������� ��������� (RDM2(I,N_CHEN_TEK)) 
        I_BOLD=0
        I_PBOLD=0
        DO I=MIDL,JJ1_B_0,-1
          IF(RDM2(I,N_CHEN_TEK).NE.0)THEN
          I_BOLD=I
          GOTO 20
          END IF
        END DO
20      DO I=MIDL,JJ2_B_0       
          IF(RDM2(I,N_CHEN_TEK).NE.0)THEN
          I_PBOLD=I
          GOTO 21
          END IF
        END DO
21      IF(I_BOLD.EQ.0.AND.I_PBOLD.EQ.0)THEN                                ! ���� ��� �������� ����� ����������� 
        I_B=YY(LM_WINDOW1,N_CHEN_TEK)                                       ! ��������� ���������� ��������            
        ELSE
        IF(I_BOLD.NE.0.AND.I_PBOLD.NE.0)THEN                                ! ���� ����� �������� ��������� ������ � �����  
        I_B=(I_BOLD+I_PBOLD)/2                                              ! ����� �������
        ELSE
        IF(I_BOLD.NE.0.AND.I_PBOLD.EQ.0) I_B=(I_BOLD+YY(LM_WINDOW1,N_CHEN_TEK))/2 ! ���� ���� �������� �� �������
        IF(I_BOLD.EQ.0.AND.I_PBOLD.NE.0)I_B=(I_PBOLD+YY(LM_WINDOW1,N_CHEN_TEK))/2 ! ���������        
        END IF    
        END IF
        END IF
        
        IF(I_B.GT.MIDL1.AND.I_B.LT.MIDL2) THEN                              ! ���� ������������ �������� �������� � ���������� �������� 
            YY(LM_WINDOW,N_CHEN_TEK)=I_B
            CALL SECOND(NDEG1,LM_WINDOW,LM_WINDOW,I_BSEK,YY,N_CHEN_TEK)     ! ������������ �����������
!            YY(LM_WINDOW,N_CHEN_TEK)=I_BSEK
            I_B=I_BSEK
        ELSE
            YY(LM_WINDOW,N_CHEN_TEK)=0
            CALL SECOND(NDEG1,LM_WINDOW1,LM_WINDOW,I_BSEK,YY,N_CHEN_TEK) 
            YY(LM_WINDOW,N_CHEN_TEK)=I_BSEK
            I_B=I_BSEK
        END IF

 ! ������������� ���������           
    MIDL=0
    JA=0
	DO  I=1,LM_WINDOW1          
    YY1(I,N_CHEN_TEK)=YY1(I+1,N_CHEN_TEK)
        IF(YY1(I,N_CHEN_TEK).NE.0) THEN
        JA=JA+1
        MIDL=MIDL+YY1(I,N_CHEN_TEK)
        END IF
    END DO

    IF(JA.NE.0) THEN
        MIDL=NINT(MIDL*1./JA)
    ELSE
        MIDL=(JJ1_PB_0+JJ2_PB_0)/2
    END IF
        MIDL1=I_B !MIDL2+1                                                       !�������� ���� ��������
        MIDL2=NINT(MIDL*PARAM_MIDL2)      
        IF(I_PB.EQ.0) THEN                                                  !���� �� ������� ��������� (RDM2(I,N_CHEN_TEK)) 
        I_BOLD=0
        I_PBOLD=0
        DO I=MIDL,I_B,-1
          IF(RDM2(I,N_CHEN_TEK).NE.0)THEN
          I_BOLD=I
          GOTO 10
          END IF
        END DO
10      DO I=MIDL,MIDL2      
          IF(RDM2(I,N_CHEN_TEK).NE.0)THEN
          I_PBOLD=I
          GOTO 11
          END IF
        END DO
      
        
11      IF(I_BOLD.EQ.0.AND.I_PBOLD.EQ.0)THEN        ! ���� ��� �������� ����� ����������� 
        I_PB=YY1(LM_WINDOW1,N_CHEN_TEK)             ! ��������� ���������� �������� 
        ELSE
        IF(I_BOLD.NE.0.AND.I_PBOLD.NE.0)THEN        ! ���� ����� �������� ��������� ������ � �����   
        I_PB=(I_BOLD+I_PBOLD)/2                     ! ����� �������
        ELSE
        IF(I_BOLD.NE.0.AND.I_PBOLD.EQ.0) I_PB=(I_BOLD+YY1(LM_WINDOW1,N_CHEN_TEK))/2      ! ���� ���� �������� �� �������
        IF(I_BOLD.EQ.0.AND.I_PBOLD.NE.0) I_PB=(I_PBOLD+YY1(LM_WINDOW1,N_CHEN_TEK))/2     ! ���������    
        END IF    
        END IF
        END IF

        
        IF(I_PB.GT.MIDL1.AND.I_PB.LT.MIDL2) THEN         ! ���� ������������ �������� �������� � ���������� ��������
            YY1(LM_WINDOW,N_CHEN_TEK)=I_PB    
            CALL SECOND(NDEG1,LM_WINDOW,LM_WINDOW,I_PBSEK,YY1,N_CHEN_TEK) ! ������������ �����������
 !           YY1(LM_WINDOW,N_CHEN_TEK)=I_PBSEK
            I_PB=I_PBSEK
        ELSE 
            YY1(LM_WINDOW,N_CHEN_TEK)=0
            CALL SECOND(NDEG1,LM_WINDOW1,LM_WINDOW,I_PBSEK,YY1,N_CHEN_TEK) 
            YY1(LM_WINDOW,N_CHEN_TEK)=MIDL
            I_PB=I_PBSEK
     END IF      

3 IF(I_B+5.GT.I_PB)I_PB=I_B+10
END IF
!---------------------------------------------------------------------------------------

ii_bal(N_CHEN_TEK)=I_B
ii_osn(N_CHEN_TEK)=I_PB 

DO I=J_LEVEL,N
TRACE_BUF(I+JSDVIG_TR)=RDM2(I,N_CHEN_TEK) ! ��� ����� �����������
!                                         ! ���� ������������, �� ������� ����� �����������
END DO
!----------------------------------------------------------------!
RETURN
END SUBROUTINE ANALIZ
    
    
SUBROUTINE SECOND (NDEG1,K,JJJ,III,YDATA1,N_CHEN_TEK)
USE DFLIB
IMPLICIT NONE
INTEGER*4 I
INTEGER*4 FX
INTEGER*4 K               ! ����� ����� � ����
INTEGER*4 NDEG1           ! ������� ��������
INTEGER*4 NDEG            ! ������� ��������-1
INTEGER*4 III             ! �������� ����� ������������
INTEGER*4 JJJ             ! ����� ����� � ����
INTEGER*4 N_CHEN_TEK      ! ����� ������
REAL*4 B(NDEG1),SSPOLY(NDEG1),STAT(10)
INTEGER*4 YDATA1(K,N_CHEN_TEK),II,J,K1,MIDL
REAL*4 XDATA(K),YDATA(K),RAB

III=0
! ������� ������������ ��������
!RAB=YDATA1(1,N_CHEN_TEK)

!DO I=2,K
!IF(YDATA1(I,N_CHEN_TEK).GT.RAB)RAB=YDATA1(I,N_CHEN_TEK)
!END DO

!RAB=RAB*0.7
K1=0
DO I=1,K
IF(YDATA1(I,N_CHEN_TEK).NE.0)THEN
K1=K1+1   
XDATA(K1)=I
YDATA(K1)=YDATA1(I,N_CHEN_TEK) 
END IF
END DO

IF(K1.LE.NDEG1) THEN    
III=YDATA1(K-1,N_CHEN_TEK)
RETURN
END IF

	NDEG=NDEG1-1
	CALL RCURV(K1,XDATA,YDATA,NDEG,B,SSPOLY,STAT)
    III=FX(JJJ,B,NDEG1)
    RETURN
    END

	INTEGER FUNCTION FX(J,COEFF,NDEG1)
	IMPLICIT NONE
    INTEGER*4 NDEG1,J,I
	REAL*4 COEFF(NDEG1),FX1
	FX1=COEFF(NDEG1)
	DO I=NDEG1-1,1,-1
	FX1=COEFF(I)+J*FX1
    END DO
    FX=NINT(FX1)
    RETURN
	END

    
!open(unit=100, FILE='D:/OUTPUT.OUT')   
!IF(N_CHEN_TEK.EQ.1) then
!write(100,*)
!end if