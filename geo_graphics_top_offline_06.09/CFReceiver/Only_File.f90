!
!   напюанрйю тюикю б пефхле аег опхбъгйх
!
    INTEGER*4 FUNCTION ONLY_FILE()
    !DEC$ ATTRIBUTES STDCALL :: Only_File
    USE GEOSKANGLOBALS
    USE INPUT_RADAROGRAMMA
    USE CFReceiverGlobals
    USE KOORDINATA
    USE GLOBALS_LOG
    IMPLICIT NONE
    INTEGER*4 IRESULTAT     ! оюпюлерп ньхайх опх бшонкмемхх опнжедспш GEOSKANGLOBALS=0
    INTEGER RET,I,J,II,ISTEP_BETWEEN_TRASES_OLD

!    CALL Start()                                                                     ! ярюпрсел DLL
 !              CALL SendSampleTrace(100)
 !              call GetFileName(hInst,hwnd,filt,NAME_FILE_GPR,IRES)
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
!               CALL GEOSCAN_INPUT()                                                ! вхрюел щрхйерйс
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
        FF1='Cfreceiver.cfg'
        FF2='Ster_Lenth'
        FF3='100'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)Ster_Lenth
        Ster_Lenth=Ster_Lenth/ntr*1000*NCHANAL_GEO     ! пЮЯЯРНЪМХЕ ЛЕФДС РПЮЯЯЮЛХ Б лл 
                                           ! НРМЕЯЕМН Й ОНКМНЛС ВХЯКС РПЮЯЯ
! ббндхл йннпдхмюрш мювюкэмни рнвйх
        FF1='Cfreceiver.cfg'
        FF2='First_Point'
        FF3='0'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)First_Point
! ббндхл х янупюмъел пюяярнъмхе лефдс рпюяяюлх хг INI тюикю
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'
        FF3='10'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)ISTEP_BETWEEN_TRASES_OLD
! оепеохяшбюел б INI мнбши ьюц
        FF1='geolib.ini'
        FF2='ISTEP_BETWEEN_TRASES'        
        WRITE(FF3,*)FLOOR(Ster_Lenth)
        CALL Ini_Write_Str(FF1,FF2,FF3)
! пюяярюбкъел йннпдхмюрш
        II=1
        IZAGTR(II,3)=First_Point*1000                           ! б ЛХККХЛЕРПЮУ
        IF(NCHANAL_GEO.GT.1) THEN
        DO J=2,NCHANAL_GEO
            IZAGTR(II+J-1,3)=IZAGTR(II+J-2,3)                       ! б ЛХККХЛЕРПЮУ 
        END DO
        END IF       
        II=II+NCHANAL_GEO     
        DO I=II,NTR,NCHANAL_GEO
            IZAGTR(I,3)=IZAGTR(I-NCHANAL_GEO+1,3)+int(Ster_Lenth)      ! б ЛХККХЛЕРПЮУ 
        IF(NCHANAL_GEO.GT.1) THEN
        DO J=2,NCHANAL_GEO
            IF(I+J-1.GT.NTR) GOTO 444
            IZAGTR(I+J-1,3)=IZAGTR(I+J-2,3)                         ! б ЛХККХЛЕРПЮУ
        END DO
        END IF  
        END DO
444     CONTINUE         
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
ONLY_FILE=1
IF( IRESULTAT.EQ.0)  ONLY_FILE=0         
RETURN
END

! open(unit=100, FILE='c:/1/OUTPUT.OUT')
! write(100,*)