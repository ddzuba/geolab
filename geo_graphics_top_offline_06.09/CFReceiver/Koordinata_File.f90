!
!   напюанрйю тюикю б пефхле аег опхбъгйх
!
    INTEGER*4 FUNCTION KOORDINATA_FILE()
    !DEC$ ATTRIBUTES STDCALL :: KOORDINATA_FILE
    USE GEOSKANGLOBALS
    USE INPUT_RADAROGRAMMA
    USE CFReceiverGlobals
    USE KOORDINATA
    USE GLOBALS_LOG
    IMPLICIT NONE
    INTEGER*4 IRESULTAT     ! оюпюлерп ньхайх опх бшонкмемхх опнжедспш GEOSKANGLOBALS=0
    INTEGER RET,I,ISTEP_BETWEEN_TRASES_OLD
	REAL*8 R_RAB  
! ббндхл осрэ й тюикс ценпюдхнкнйюжхх
 FF1='Cfreceiver.cfg'
        FF2='NAME_FILE_GPR'
        FF3='C:\0.gpr'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        NAME_FILE_GPR=FF2
                NN=101
                OPEN(UNIT=NN,FILE=TRIM(NAME_FILE_GPR),FORM='BINARY',STATUS='OLD')   ! нрйпшбюел тюик я пюдюпюцпюллни
                CALL RDD1(IRESULTAT,NN)                                             ! вхрюел тюик
                CLOSE(NN)                                                           ! гюйпшбюел тюик я пюдюпюцпюллни

! ббндхл сярпниярбн
        FF1='Cfreceiver.cfg'
        FF2='N_DEVICE'
        FF3='юООЮПЮРСПЮ_пцсоя'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        N_DEVICE=FF2
! ббндхл днпнцс        
        FF1='Cfreceiver.cfg'
        FF2='N_VAY'
        FF3='яйфд'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        N_VAY=FF2
! ббндхл мюопюбкемхе        
        FF1='Cfreceiver.cfg'
        FF2='N_DIRECTION'
        FF3='рСЮОЯЕ-юДКЕП'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        N_DIRECTION=FF2
! ббндхл осрэ        
        FF1='Cfreceiver.cfg'
        FF2='N_TRACK'
        FF3='1'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        N_TRACK=FF2
! ббндхл ьюц лефдс рпюяяюлх        
!        FF1='Cfreceiver.cfg'
!        FF2='Ster_Lenth'
!        FF3='100'
!        CALL Ini_Read_Str(FF1,FF2,FF3)  
!        READ(FF2,*)Ster_Lenth
!        Ster_Lenth=Ster_Lenth/ntr      ! пЮЯЯРНЪМХЕ ЛЕФДС РПЮЯЯЮЛХ б лл
! ббндхл йннпдхмюрш мювюкэмни рнвйх
        FF1='Cfreceiver.cfg'
        FF2='First_Point'
        FF3='0'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)First_Point
! ббндхл ядбхц вюянб опх гюохях рпейю
        FF1='Cfreceiver.cfg'
        FF2='TIME_GPS'
        FF3='0.0'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)TIME_GPS
! ббндхл х янупюмъел пюяярнъмхе лефдс рпюяяюлх хг INI тюикю
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'
        FF3='10'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)ISTEP_BETWEEN_TRASES_OLD
! оепеохяшбюел б INI мнбши ьюц
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'  
        Ster_Lenth=1000                             ! дкъ акнйхпнбйх опносяйнб мю щйпюме б щрнл пефхле онярпнемхъ
        WRITE(FF3,*)FLOOR(Ster_Lenth)
        CALL Ini_Write_Str(FF1,FF2,FF3)
! напюаюршбюел тюик йннпдхмюр яосрмхйю х пюдюпнцпюллс
    CALL RAZM1()  
! времхе бпелемх х йннпдхмюр хг рейярнбнцн тюикю яосрмхйю
! пюяонгмнбюмхе рхою тюикю 'LOG' хкх 'TXT'
    CALL TIME_()
! нопедекъел бпелъ рпюяяш
    CALL TIME_TRASSA()
! нопедекъел юаянкчрмше йннпдхмюрш пюдюпю бн бпелъ хглепемхъ рпюяяш
    CALL COORDINATORY()
! нопедекъел пюяярнъмхе нрмняхрекэмн оепбни рпюяяш х свхршбюел ее йннпдхмюрс
    CALL OTN_RASST()
! пюяярюбкъел йннпдхмюрш
            IZAGTR(1,3)=First_Point*1000            ! б ЛХККХЛЕРПЮУ
            R_RAB=0.D0
            DO I=2,NTR
            R_RAB=R_RAB+ R_TRASSA(I)*1000   ! б ЛХККХЛЕРПЮУ
            IZAGTR(I,3)=IZAGTR(1,3)+R_RAB
            END DO
! пюглеыюел рюакхжс йндхпнбйх
            CALL TABLE_() 
            DO I=1,NCHANAL_GEO                                             ! ндмскъел ткюцх гюонкмемхъ йюмюкнб
            FLAG_CHANAL(I)=0
            END DO
! тнплхпсел оняшкйс б ахакхнрейс
            DO I=1,Ntr
            if( flag_file.eq.1) goto 1  
            CALL GEOSCAN_INTEGRAL(IRESULTAT,I,NCHANAL(I))                                            ! тнплхпсел оняшкйс хмрецпюкю    ! сундхл мю напюанрйс б DLL хмрецпюкю                  
            END DO
            flag_file=1
! цнрнбхляъ й бшундс
1           CALL DE_RDD1()                                                      ! ямхлюел пюглеыемхе тюикнб            
            DEALLOCATE (FLAG_CHANAL)                                            ! ямхлюел пюглеыемхе ткюцнб гюонкмемхъ йюмюкнб
            DEALLOCATE (table)                                                  ! ямхлюел пюглеыемхе рюакхжш йндхпнбйх
! бняярюмюбкхбюел INI тюик
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'        
        WRITE(FF3,*)ISTEP_BETWEEN_TRASES_OLD
        CALL Ini_Write_Str(FF1,FF2,FF3)
KOORDINATA_FILE=1
IF( IRESULTAT.EQ.0)  KOORDINATA_FILE=0   
DEALLOCATE (VREMYA)			 
DEALLOCATE (SHIROTA)
DEALLOCATE (DOLGOTA)
DEALLOCATE (VREMYA_TRASSA)			
DEALLOCATE (R_TRASSA)				
DEALLOCATE (SHIROTA_TRASSA)			
DEALLOCATE (DOLGOTA_TRASSA)	
RETURN
END

! open(unit=100, FILE='c:/1/OUTPUT.OUT')
! write(100,*)