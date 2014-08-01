SUBROUTINE REFRACTION(RDM,OTR,OTR_ALL,J_LEVEL,L_REGION,ITEK,N,J_DOWN,RSU)
! ���������� ������������� �����������
REAL*4 RDM(511),OTR(60000),OTR_ALL(60000)
REAL*4 RSU,RSU_1,RSU_2
INTEGER*4   J_LEVEL,L_REGION,N
INTEGER*4  I,L1,II
! �������� � ������ ������	    - J_LEWEL 
! ����� ����� � ������          - N
! �������� ����������           - L_REGION

RSU=0					! ������������� ����������� �������
II=J_LEVEL+J_DOWN		! ��������� ������� �����

DO I=II,N				! ������� ������� ���������� ������� ����
RSU=RSU+ABS(RDM(I))		! ���������� ������������� �����������
END DO
OTR(ITEK)=RSU			! ����������� ��� ����������

L1=ITEK-L_REGION		! ���������� ������� ����������
IF(L1.LT.1)L1=1

RSU=0.					! ���������
II=0
DO L=L1,ITEK
II=II+1
RSU=RSU+OTR(L)
END DO
RSU_1=RSU/II

RSU=0					! ������������� ����������� ����� ���������
DO I=1,N				! ������� ������� ���������� ������� ����
RSU=RSU+ABS(RDM(I))		! ��� ������ J_LEWEL=50
END DO
OTR_ALL(ITEK)=RSU		! ����������� ��� ����������

L1=ITEK-L_REGION				! ���������� ������� ����������
IF(L1.LT.1)L1=1

RSU=0.					! ���������
II=0
DO L=L1,ITEK
II=II+1
RSU=RSU+OTR_ALL(L)
END DO
RSU_2=RSU/II

RSU=RSU_1/RSU_2
END SUBROUTINE REFRACTION