!****************************************************************************
!  Global data, parameters, and structures from GeoScan32
!****************************************************************************
MODULE GEOSCANINPUT
USE DFLOGM				! наъгюрекэмюъ яяшкйю б яксвюе DVF
INCLUDE 'RESOURCE.FD'	! бйкчвюч оюпюлерпш пеяспянб
TYPE(DIALOG) D_2		! гюдюч оепелеммсч дхюкнцю
LOGICAL(4) FLAG
INTEGER(4) STATUS
CHARACTER*50 STRING
END MODULE GEOSCANINPUT 
    
MODULE GEOSKANGLOBALS
CHARACTER*100 NAME_FILE_GPR
!INTEGER*4 NN
!INTEGER*4 IRESULTAT     ! оюпюлерп ньхайх опх бшонкмемхх опнжедспш GEOSKANGLOBALS=0
CHARACTER *50 N_DEVICE
CHARACTER *50 N_VAY
CHARACTER *50 N_DIRECTION
CHARACTER *50 N_TRACK
CHARACTER, ALLOCATABLE :: table(:)
REAL *4 Ster_Lenth  ! дкхмю свюярйю х ьюц лефдс рпюяяюлх
REAL *4 First_Point ! йннпдхмюрю оепбни рпюяяш
REAL *4 TIME_GPS
END MODULE GEOSKANGLOBALS     
    
MODULE INPUT_RADAROGRAMMA
INTEGER*4:: NVERSYION						!мнлеп бепяхх
INTEGER*4:: IZAG(128)						!гюцнкнбнй тюикю - 512 аюир
INTEGER*4, TARGET:: NTR=1								!вхякн рпюяя
INTEGER*4, TARGET:: N=512								!вхякн рнвей
INTEGER*4:: NLAB							!вхякн лернй
INTEGER*4:: NSCSOTS							!дкхмю рюакхжш яйпхмьнрнб
INTEGER*4:: NSCSOTSSTORAGESIZE				!дкхмю люяяхбнб яйпхмьнрнб
INTEGER*4:: ICOLOR(256)						!люяяхб   жбернб - 1024 аюир
REAL*4,	   ALLOCATABLE :: RL(:)				!люяяхб йнщттхжхемрнб бшпюбмхбюмхъ - 4*N аюир
INTEGER*4, ALLOCATABLE :: IZAGTR(:,:)		!гюцнкнбнй рпюяяш
INTEGER*4, ALLOCATABLE :: NCHANAL(:)		!мнлеп йюмюкю
REAL*4,    ALLOCATABLE :: RSUM(:,:)			!люяяхб - пюдюпнцпюллю 	
INTEGER*4, ALLOCATABLE :: ISCCRIN(:)		!рюакхжю яйпхмьнрнб
INTEGER*4, ALLOCATABLE :: ISCCRINSTOR(:)	!люяяхб яйпхмьнрнб
INTEGER*4, ALLOCATABLE :: IMET(:)			!люяяхб лернй
INTEGER*4 NCHANAL_GEO                       !вхякн йюмюкнб б пюдюпнцпюлле
INTEGER*4, ALLOCATABLE :: FLAG_CHANAL(:)			!люяяхб лернй
END MODULE INPUT_RADAROGRAMMA
 

