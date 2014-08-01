! ������������ ������ � ������ ������ ������������
! � ������������� �������
 


SUBROUTINE RDD1(IRESULTAT,NN)							 !������ ����� ������� GPR
USE DFWINA
USE INPUT_RADAROGRAMMA
IMPLICIT NONE
INTEGER(4) NN
INTEGER(4) I,J,K,L
INTEGER*4 IRESULTAT
INTEGER*4 MESS
INTEGER*4 CONT,ITABLE(40)
READ(NN,END=777)IZAG						 !���������� ��������� 
NVERSYION=IZAG(2)							 !����� ������
NTR=IZAG(7)								     !����� �����
N=IZAG(8)								     !����� �����
NLAB=IZAG(9)							     !����� �����
    
READ(NN,END=777)ICOLOR						 !����������  ������� ������

ALLOCATE(RL(1:N))
READ(NN,END=777)RL						     !���������� ������� ������������� ������������

L=9											 !������ �1
IF(NVERSYION.GE.2) L=11						 !������ �2
ALLOCATE(IZAGTR(1:NTR,1:L))					 !���������� ����� ��������� ������
ALLOCATE(NCHANAL(1:NTR))					 !���������� ����� ������ ������
ALLOCATE(RSUM(1:NTR,1:N))					 !���������� ����� ������������
DO K=1,NTR			
READ(NN,END=777)(IZAGTR(K,J),J=1,L)			 !���������� ��������� ������
NCHANAL(K)=IZAGTR(K,7)  					 !��������� ����� ������
DO I=1,N				
READ (NN,END=777) RSUM(K,I)					 !����������  ������
END DO
END DO

nlab=0 ! ��� ���������� ���������� �������������� ����������

! ����������� ������ � ������, ����� ����� ����� ����  
DO K=1,NTR			
IF(NCHANAL(K).EQ.0) GOTO 10    
END DO
GOTO 11
10 DO K=1,NTR			
NCHANAL(K)=NCHANAL(K)+1 
END DO
   
11 IF(NVERSYION.GE.2)THEN						 !������ �2
READ(NN,END=777)NSCSOTS						 !����� ������� ����������
IF(NSCSOTS.GT.0)THEN
ALLOCATE(ISCCRIN(1:24*NSCSOTS))
READ(NN,END=777)ISCCRIN						 !������� ����������
READ(NN,END=777)NSCSOTSSTORAGESIZE		     !����� �������� ����������
IF(NSCSOTS.GT.0)THEN
ALLOCATE(ISCCRINSTOR(1:NSCSOTSSTORAGESIZE))
READ(NN,END=777)ISCCRINSTOR					 !������ ����������
END IF
END IF
END IF

IF(NLAB.GT.0)THEN
ALLOCATE(IMET(1:NLAB*12))
READ(NN,END=777)IMET						 !����������  ������� �����
END IF
GOTO 776
777	   MESS=MESSAGEBOX(NULL,"�������� ������ "C,"������������������� ���� �� �����������"C,4)    ! ���� �� ��������� ����, ��
	IF(MESS.EQ.6) THEN                                                                              ! ����������� ������
    IRESULTAT=0
    RETURN
    END IF
776 IRESULTAT=1 
    
! ���������� ����� �������
DO I=1,40
ITABLE(I)=0
END DO

DO I=1,NTR
ITABLE(NCHANAL(I))= NCHANAL(I)
END DO

NCHANAL_GEO=0
DO I=1,40
IF(ITABLE(I).EQ.I)  NCHANAL_GEO= NCHANAL_GEO+1
END DO
IF(NCHANAL_GEO.EQ.0)THEN
NCHANAL_GEO=1
DO I=1,NTR
NCHANAL(I)=1
END DO
END IF

IF( NCHANAL_GEO.EQ.1) THEN
DO I=1,NTR
NCHANAL(I)=1
END DO
END IF

IF( NCHANAL_GEO.EQ.2) THEN
K=0
DO I=1,NTR                                  ! ������� ������������ ��������
IF(K.LT.NCHANAL(I)) K=NCHANAL(I)
END DO
L=K
DO I=1,NTR                                  ! ������� ����������� ��������
IF(L.GT.NCHANAL(I)) L=NCHANAL(I)
END DO
DO I=1,NTR
IF(NCHANAL(I).EQ.L)NCHANAL(I)=1
IF(NCHANAL(I).EQ.K)NCHANAL(I)=2
END DO
!NTR=FLOOR(NTR/2.)
END IF

IF( NCHANAL_GEO.EQ.3) THEN
K=0
DO I=1,NTR                                  ! ������� ������������ ��������
IF(K.LT.NCHANAL(I)) K=NCHANAL(I)
END DO
L=K
DO I=1,NTR                                  ! ������� ����������� ��������
IF(L.GT.NCHANAL(I)) L=NCHANAL(I)
END DO

DO I=1,NTR                                  ! ������� ����������� ��������
IF(L.LT.NCHANAL(I).AND.K.GT.NCHANAL(I)) J=NCHANAL(I)
END DO

DO I=1,NTR
IF(NCHANAL(I).EQ.L)NCHANAL(I)=1
IF(NCHANAL(I).EQ.J)NCHANAL(I)=2
IF(NCHANAL(I).EQ.K)NCHANAL(I)=3
END DO
!NTR=FLOOR(NTR/3.)
END IF

ALLOCATE(FLAG_CHANAL(NCHANAL_GEO))     !����� ���������� �������

RETURN
END SUBROUTINE RDD1
    
SUBROUTINE DE_RDD1()							 !������ ����� ������� GPR
USE DFWINA
USE INPUT_RADAROGRAMMA
IMPLICIT NONE

DEALLOCATE(RL)
DEALLOCATE(IZAGTR)					 !���������� ����� ��������� ������
DEALLOCATE(NCHANAL)					 !���������� ����� ������ ������
DEALLOCATE(RSUM)					 !���������� ����� ������

IF(NSCSOTS.GT.0)THEN
DEALLOCATE(ISCCRIN) 
DEALLOCATE(ISCCRINSTOR)
END IF
IF(NLAB.GT.0) DEALLOCATE(IMET)
RETURN
END SUBROUTINE DE_RDD1   
! open(unit=100, FILE='D:/OUTPUT.OUT')
!  write(100,*)NCHANAL_GEO,NCHANAL(I)



