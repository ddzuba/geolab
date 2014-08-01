!
!   напюанрйю тюикю б пефхле опхбъъгйх он лерйюл
!
    SUBROUTINE ONLY_POINT(IRESULTAT)
    USE GEOSKANGLOBALS
    USE INPUT_RADAROGRAMMA
    IMPLICIT NONE
    INTEGER*4 IRESULTAT     ! оюпюлерп ньхайх опх бшонкмемхх опнжедспш GEOSKANGLOBALS=0
    INTEGER RET,I
    CHARACTER*50 FF1,FF2,FF3
    CALL Start()                                                                     ! ярюпрсел DLL
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
        Ster_Lenth=Ster_Lenth/ntr       ! пЮЯЯРНЪМХЕ ЛЕФДС РПЮЯЯЮЛХ
! ббндхл йннпдхмюрш мювюкэмни рнвйх
        FF1='Cfreceiver.cfg'
        FF2='First_Point'
        FF3='0'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        READ(FF2,*)First_Point
! пюяярюбкъел йннпдхмюрш
            IZAGTR(1,3)=First_Point*1000            ! б ЛХККХЛЕРПЮУ
            DO I=2,NTR
            IZAGTR(I,3)=IZAGTR(I-1,3)+Ster_Lenth  ! б ЛХККХЛЕРПЮУ
            END DO
! пюглеыюел рюакхжс йндхпнбйх
            CALL TABLE_() 
            DO I=1,NCHANAL_GEO                                             ! ндмскъел ткюцх гюонкмемхъ йюмюкнб
            FLAG_CHANAL(I)=0
            END DO
 !   open(unit=100, FILE='c:/1/OUTPUT.OUT')
        
            
! тнплхпсел оняшкйс б ахакхнрейс
            DO I=1,Ntr
            CALL GEOSCAN_INTEGRAL(IRESULTAT,I,NCHANAL(I))                                            ! тнплхпсел оняшкйс хмрецпюкю    ! сундхл мю напюанрйс б DLL хмрецпюкю                  
         !   write(100,*)I, IZAGTR(I,7), IZAGTR(I,8)             
            END DO
! цнрнбхляъ й бшундс
            CALL DE_RDD1()                                                      ! ямхлюел пюглеыемхе тюикнб            
            DEALLOCATE (FLAG_CHANAL)                                            ! ямхлюел пюглеыемхе ткюцнб гюонкмемхъ йюмюкнб
            DEALLOCATE (table)                                                  ! ямхлюел пюглеыемхе рюакхжш йндхпнбйх
RETURN
END

    ! open(unit=100, FILE='c:/1/OUTPUT.OUT')
! write(100,*)