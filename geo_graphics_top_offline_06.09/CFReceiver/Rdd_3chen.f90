! ондопнцпюллш времхъ х гюохях гюохях пюдюпнцпюллш
! б центхгхвеяйнл тнплюре
 


SUBROUTINE RDD1(IRESULTAT,NN)							 !времхе тюикю тнплюрю GPR
USE DFWINA
USE INPUT_RADAROGRAMMA
IMPLICIT NONE
INTEGER(4) NN
INTEGER(4) I,J,K,L
INTEGER*4 IRESULTAT
INTEGER*4 MESS
INTEGER*4 CONT,ITABLE(40)
READ(NN,END=777)IZAG						 !явхршбюмхе гюцнкнбйю 
NVERSYION=IZAG(2)							 !мнлеп бепяхх
NTR=IZAG(7)								     !вхякн рпюяя
N=IZAG(8)								     !вхякн рнвей
NLAB=IZAG(9)							     !вхякн лернй
    
READ(NN,END=777)ICOLOR						 !явхршбюмхе  люяяхбю жбернб

ALLOCATE(RL(1:N))
READ(NN,END=777)RL						     !явхршбюмхе люяяхбю йнщттхжхемрнб бшпюбмхбюмхъ

L=9											 !бепяхъ ╧1
IF(NVERSYION.GE.2) L=11						 !бепяхъ ╧2
ALLOCATE(IZAGTR(1:NTR,1:L))					 !пюглеыемхе тюикю гюцнкнбйю рпюяяш
ALLOCATE(NCHANAL(1:NTR))					 !пюглеыемхе тюикю мнлепю йюмюкю
ALLOCATE(RSUM(1:NTR,1:N))					 !пюглеыемхе тюикю пюдюпнцпюллш
DO K=1,NTR			
READ(NN,END=777)(IZAGTR(K,J),J=1,L)			 !явхршбюмхе гюцнкнбйю рпюяяш
NCHANAL(K)=IZAGTR(K,7)  					 !тнплхпсел мнлеп йюмюкю
DO I=1,N				
READ (NN,END=777) RSUM(K,I)					 !явхршбюмхе  рпюяяш
END DO
END DO

nlab=0 ! дкъ гюопеыемхъ явхршбюмхъ мехяонкэгселни хмтнплюжхх

! оепемслепсч йюмюкш б яксвюе, йнцдю мнлеп пюбем мскч  
DO K=1,NTR			
IF(NCHANAL(K).EQ.0) GOTO 10    
END DO
GOTO 11
10 DO K=1,NTR			
NCHANAL(K)=NCHANAL(K)+1 
END DO
   
11 IF(NVERSYION.GE.2)THEN						 !бепяхъ ╧2
READ(NN,END=777)NSCSOTS						 !дкхмю рюакхжш яйпхмьнрнб
IF(NSCSOTS.GT.0)THEN
ALLOCATE(ISCCRIN(1:24*NSCSOTS))
READ(NN,END=777)ISCCRIN						 !рюакхжю яйпхмьнрнб
READ(NN,END=777)NSCSOTSSTORAGESIZE		     !дкхмю люяяхбнб яйпхмьнрнб
IF(NSCSOTS.GT.0)THEN
ALLOCATE(ISCCRINSTOR(1:NSCSOTSSTORAGESIZE))
READ(NN,END=777)ISCCRINSTOR					 !люяяхб яйпхмьнрнб
END IF
END IF
END IF

IF(NLAB.GT.0)THEN
ALLOCATE(IMET(1:NLAB*12))
READ(NN,END=777)IMET						 !явхршбюмхе  люяяхбю лернй
END IF
GOTO 776
777	   MESS=MESSAGEBOX(NULL,"оПЕПБЮРЭ ПЮАНРС "C,"цЕНПЮДХНКНЙЮЖХНММШИ ТЮИК МЕ ЯВХРШБЮЕРЯЪ"C,4)    ! еЯКХ МЕ НРЙПШБЮЕЛ ТЮИК, РН
	IF(MESS.EQ.6) THEN                                                                              ! гЮЙЮМВХБЮЕЛ ПЮАНРС
    IRESULTAT=0
    RETURN
    END IF
776 IRESULTAT=1 
    
! нопедекъел вхякн йюмюкнб
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
DO I=1,NTR                                  ! мюундхл люйяхлюкэмне гмювемхе
IF(K.LT.NCHANAL(I)) K=NCHANAL(I)
END DO
L=K
DO I=1,NTR                                  ! мюундхл лхмхлюкэмне гмювемхе
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
DO I=1,NTR                                  ! мюундхл люйяхлюкэмне гмювемхе
IF(K.LT.NCHANAL(I)) K=NCHANAL(I)
END DO
L=K
DO I=1,NTR                                  ! мюундхл лхмхлюкэмне гмювемхе
IF(L.GT.NCHANAL(I)) L=NCHANAL(I)
END DO

DO I=1,NTR                                  ! мюундхл лхмхлюкэмне гмювемхе
IF(L.LT.NCHANAL(I).AND.K.GT.NCHANAL(I)) J=NCHANAL(I)
END DO

DO I=1,NTR
IF(NCHANAL(I).EQ.L)NCHANAL(I)=1
IF(NCHANAL(I).EQ.J)NCHANAL(I)=2
IF(NCHANAL(I).EQ.K)NCHANAL(I)=3
END DO
!NTR=FLOOR(NTR/3.)
END IF

ALLOCATE(FLAG_CHANAL(NCHANAL_GEO))     !ткюцх гюонкмемхъ йюмюкнб

RETURN
END SUBROUTINE RDD1
    
SUBROUTINE DE_RDD1()							 !времхе тюикю тнплюрю GPR
USE DFWINA
USE INPUT_RADAROGRAMMA
IMPLICIT NONE

DEALLOCATE(RL)
DEALLOCATE(IZAGTR)					 !пюглеыемхе тюикю гюцнкнбйю рпюяяш
DEALLOCATE(NCHANAL)					 !пюглеыемхе тюикю мнлепю йюмюкю
DEALLOCATE(RSUM)					 !пюглеыемхе тюикю рпюяяш

IF(NSCSOTS.GT.0)THEN
DEALLOCATE(ISCCRIN) 
DEALLOCATE(ISCCRINSTOR)
END IF
IF(NLAB.GT.0) DEALLOCATE(IMET)
RETURN
END SUBROUTINE DE_RDD1   
! open(unit=100, FILE='D:/OUTPUT.OUT')
!  write(100,*)NCHANAL_GEO,NCHANAL(I)



