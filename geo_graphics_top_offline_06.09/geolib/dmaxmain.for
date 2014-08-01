	SUBROUTINE DMAX(NTR,N,RI,R0)
      IMPLICIT REAL*8(A-H,O,P,R-Z)
	DIMENSION RI(1000,512),R0(1000,512)
	DIMENSION RAB(512),IRAB(512)	
	DIMENSION RDM(512),RDM1(512),PRDM(512),PRDM1(512)

	DO 1 J=1,NTR

	DO 2 I=1,N			! �������� ����� ������
	R0(J,I)=0.D0
	RR=RI(J,I)
	RDM1(I)=RR
	IF(RR.GT.0) THEN !  ������� ������ � �������������� �����������
	RDM(I)=RR
	ELSE
	RDM(I)=0.D0
	END IF
2	CONTINUE	 
C ------------------------------------------------------------	
	K=0					! �������� ��� ��������� ��������
						! �� ����������
	DO 10 I=2,N-1
	IF(RDM(I-1).LE.RDM(I).AND.RDM(I+1).LT.RDM(I)) THEN
	K=K+1
	RAB(K)=RDM(I)

	IRAB(K)=I
	END IF
10	CONTINUE

C	goto 777
C ------------------------------------------------------------
						! �������� ��� ��������� ��������
						! �� ����������� �� ����������
	DO 11 I=2,N-1
	PRDM1(I)=RDM1(I+1)-RDM1(I-1)
11	PRDM(I)=DABS(PRDM1(I))
	DO 12 I=2,N-1
	IF(PRDM(I-1).GE.PRDM(I).AND.PRDM(I+1).GT.PRDM(I)
     *.AND.PRDM1(I-1)*PRDM1(I).GT.0.D0.AND.PRDM1(I+1)*PRDM1(I).GT.0.D0) 
     *THEN
	K=K+1
	RAB(K)=RDM(I)
	IRAB(K)=I
	END IF
12	CONTINUE

777	DO 22 I=1,K
22	R0(J,IRAB(I))=RAB(I)
1	CONTINUE

	RETURN
	END

	SUBROUTINE DMAXM(POROG,NTR,N,R0,IOTS)
      IMPLICIT REAL*8(A-H,O,P,R-Z)
	DIMENSION R0(1000,512)
	DIMENSION II(1000),RAB(512)	
	COMMON/NULL/ RAB_NULL,I_NULL
	INTEGER*4  I_NULL
	REAL*8  RAB_NULL
	COMMON/NULL1/ RAB_skin,I_skin
	INTEGER*4  I_skin
	REAL*8  RAB_skin

	DO  I=1,NTR	! 0��������
	DO  J=1,I_NULL-1
	R0(I,J)=0.D0
	end do
	R0(I,J)=RAB_NULL
	end do

	DO I=1,NTR ! ��� ������ ������� �������
!	R0(I,I_skin)=RAB_skin
	END DO

	RETURN
	END



	SUBROUTINE DMAXMAIN(NTR,N,R0,IOTS)
      IMPLICIT REAL*8(A-H,O,P,R-Z)
	DIMENSION R0(1000,512)
	DIMENSION RAB(512)
C ��������� ������� ����������
	DO 1 I=1,NTR				! ���� �� ������
	DO 2 J=1,N
2	RAB(J)=R0(I,J)

	DO 3 J=IOTS,N
	PP0=RAB(J)					! ������� ��������
	IF (PP0.LT.1.D-5) GOTO 3	! ���� 0, �� ������
	K1=J+1
	PT=PP0						! �������� ������� ��������
		DO 4 K=K1,N				! ���������� � ������� ����������
		PP1=RAB(K)
		IF(PP1.LT.1.D-5) GOTO 4	! ���� 0, �� ������
			IF(PP1.GT.PT) THEN		
			! if(j.ne.iots)
			R0(I,J)=0.D0
			GOTO 3
			ELSE
			R0(I,K)=0.D0
			PT=PP1
			GOTO 3 
			END IF
4		CONTINUE
3	CONTINUE
1	CONTINUE
	RETURN
	END