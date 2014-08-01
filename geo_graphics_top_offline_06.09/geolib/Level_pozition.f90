SUBROUTINE LEVEL_INDEX_REFRACTION()
USE PARAM_
USE PARAM_1
USE N_


UNIFORMITY(N_CHEN_TEK)=INDEX_REFRACTION(N_CHEN_TEK)							    	! ����� ������� � ������������
IF(ITEK.GT.N_MIDL_U) THEN
	DO I=2,N_MIDL_U						    	! ��������� � ������						
	UNIF(N_CHEN_TEK,I-1)=UNIF(N_CHEN_TEK,I)
	END DO
	UNIF(N_CHEN_TEK,N_MIDL_U)=INDEX_REFRACTION(N_CHEN_TEK)
	UNIFORMITY_MIDL(N_CHEN_TEK)=0.
	DO I=1,N_MIDL_U
	UNIFORMITY_MIDL(N_CHEN_TEK)=UNIFORMITY_MIDL(N_CHEN_TEK)+UNIF(N_CHEN_TEK,I)
	END DO
	UNIFORMITY_MIDL(N_CHEN_TEK)=INDEX_REFRACTION_0(N_CHEN_TEK)	! ���������
ELSE
	UNIF(N_CHEN_TEK,ITEK)=INDEX_REFRACTION(N_CHEN_TEK)							! ��������� � ������
	UNIFORMITY_MIDL(N_CHEN_TEK)=0.
	DO I=1,ITEK
	UNIFORMITY_MIDL(N_CHEN_TEK)=UNIFORMITY_MIDL(N_CHEN_TEK)+UNIF(N_CHEN_TEK,I)
	END DO
	UNIFORMITY_MIDL(N_CHEN_TEK)=INDEX_REFRACTION_0(N_CHEN_TEK)	! ���������
END IF

RETURN
    END 
    
SUBROUTINE LEVEL_POZITION_400()
USE PARAM_
USE PARAM_1
USE N_
INTEGER*4 I,IMAX1

IMAX1=0
DO I=1,N_FINISH
IF(RDM(I).NE.0)THEN
IMAX1=IMAX1+1										! ����� ��������� �����
END IF
END DO

UNIFORMITY(N_CHEN_TEK)=IMAX1							    	! ����� ������� � ������������
IF(ITEK.GT.N_MIDL_U) THEN
	DO I=2,N_MIDL_U						    	! ��������� � ������						
	UNIF(N_CHEN_TEK,I-1)=UNIF(N_CHEN_TEK,I)
	END DO
	UNIF(N_CHEN_TEK,N_MIDL_U)=IMAX1
	UNIFORMITY_MIDL(N_CHEN_TEK)=0.
	DO I=1,N_MIDL_U
	UNIFORMITY_MIDL(N_CHEN_TEK)=UNIFORMITY_MIDL(N_CHEN_TEK)+UNIF(N_CHEN_TEK,I)
	END DO
	UNIFORMITY_MIDL(N_CHEN_TEK)=UNIFORMITY_MIDL(N_CHEN_TEK)/N_MIDL_U	! ���������
ELSE
	UNIF(N_CHEN_TEK,ITEK)=IMAX1							! ��������� � ������
	UNIFORMITY_MIDL(N_CHEN_TEK)=0.
	DO I=1,ITEK
	UNIFORMITY_MIDL(N_CHEN_TEK)=UNIFORMITY_MIDL(N_CHEN_TEK)+UNIF(N_CHEN_TEK,I)
	END DO
	UNIFORMITY_MIDL(N_CHEN_TEK)=UNIFORMITY_MIDL(N_CHEN_TEK)/ITEK		! ���������
END IF

RETURN
END 
    
    