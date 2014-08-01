SUBROUTINE REFRACTION(RDM,OTR,OTR_ALL,J_LEVEL,L_REGION,ITEK,N,J_DOWN,RSU)
! нопедекъел нрпюфюрекэмсч яонянамнярэ
REAL*4 RDM(511),OTR(60000),OTR_ALL(60000)
REAL*4 RSU,RSU_1,RSU_2
INTEGER*4   J_LEVEL,L_REGION,N
INTEGER*4  I,L1,II
! опхбндхл й ндмнлс спнбмч	    - J_LEWEL 
! вхякн рнвей б рпюяяе          - N
! дхюонгнм сяпедмемхъ           - L_REGION

RSU=0					! нрпюфюрекэмюъ яонянамнярэ свюярйю
II=J_LEVEL+J_DOWN		! онкнфемхе ондньбш ьоюкш

DO I=II,N				! бепумъъ цпюмхжю нопедекъер ондньбс ьоюк
RSU=RSU+ABS(RDM(I))		! бшвхякемхе нрпюфюрекэмни яонянамнярх
END DO
OTR(ITEK)=RSU			! мюйюокхбюел дкъ сяпедмемхъ

L1=ITEK-L_REGION		! нопедекъел цпюмхжш сяпедмемхъ
IF(L1.LT.1)L1=1

RSU=0.					! сяпедмъел
II=0
DO L=L1,ITEK
II=II+1
RSU=RSU+OTR(L)
END DO
RSU_1=RSU/II

RSU=0					! нрпюфюрекэмюъ яонянамнярэ бяецн хмрепбюкю
DO I=1,N				! бепумъъ цпюмхжю нопедекъер ондньбс ьоюк
RSU=RSU+ABS(RDM(I))		! опх бшанпе J_LEWEL=50
END DO
OTR_ALL(ITEK)=RSU		! мюйюокхбюел дкъ сяпедмемхъ

L1=ITEK-L_REGION				! нопедекъел цпюмхжш сяпедмемхъ
IF(L1.LT.1)L1=1

RSU=0.					! сяпедмъел
II=0
DO L=L1,ITEK
II=II+1
RSU=RSU+OTR_ALL(L)
END DO
RSU_2=RSU/II

RSU=RSU_1/RSU_2
END SUBROUTINE REFRACTION