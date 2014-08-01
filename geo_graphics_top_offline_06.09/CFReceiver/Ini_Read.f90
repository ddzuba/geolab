!----------------------------------------------------------------------------
! вРЕМХЕ ДЮММШУ ХГ ТЮИКЮ
!----------------------------------------------------------------------------

Subroutine Ini_Read_Str(str,str1,name)
implicit none
CHARACTER *50 STR,STR1,name
CHARACTER *50 STR2
CHARACTER *100 STROKA, STROKA1,STROKA2
INTEGER NN,I,J,ILENTH,ILENTH1,ILENTH2,K,L,IFLAG,ISTAT
! STR  - ярпнйю я хдемрхтхйюрнпнл оепелеммни тюикю
! STR1 - CTPOKA я хлемел оепелеммни
! NAME - ондярюбкъел гмювемхе, еякх ме мюундхл б тюике
!--------------------------------------------------------
! намскъел бшундмсч ярпнйс
DO I=1,50
STR2(I:I)=' '
END DO

! напюаюршбюел мюгбюмхе STR
ILENTH=0
DO I=1,50
    IF(STR(I:I).NE.' ')THEN
    ILENTH=ILENTH+1
    STR2(ILENTH:ILENTH)=STR(I:I) ! оепеохяюкх аег опнаекнб б STR2
    END IF
END DO
DO I=1,50
    STR(I:I)=STR2(I:I) ! оепеохяюкх аег опнаекнб б STR2
END DO
! нрйпшбюел тюик гюохях йнмтхцспюжхнммшу дюммшу
NN=777
OPEN(UNIT=NN,FILE=TRIM(STR),STATUS='OLD')   ! нрйпшбюел тюик я пюдюпюцпюллни
!---------------------------------------------------------------------------
DO I=1,50
STR2(I:I)=' '
END DO
! напюаюршбюел мюгбюмхе STR1
ILENTH1=0
DO I=1,50
    IF(STR1(I:I).NE.' ')THEN
    ILENTH1=ILENTH1+1
    STR2(ILENTH1:ILENTH1)=STR1(I:I) ! оепеохяюкх аег опнаекнб б STR2
    END IF
END DO
DO I=1,50
    STR1(I:I)=STR2(I:I) ! оепеохяюкх аег опнаекнб б STR1
END DO
!----------------------------------------------------------------------------
 ! намскъел бшундмсч ярпнйс
1   DO I=1,100
    STROKA(I:I)=' '
    STROKA2(I:I)=' '
    END DO
  
  READ(NN,'(A)',IOSTAT=ISTAT)STROKA

  IF(ISTAT.EQ.-1)GOTO 3
  ILENTH2=0
  DO I=1,100
    IF(STROKA(I:I).NE.' ')THEN
    ILENTH2=ILENTH2+1
    STROKA2(ILENTH2:ILENTH2)=STROKA(I:I) ! оепеохяюкх аег опнаекнб б STROKA2
    END IF
  END DO

  IF(STROKA2(1:1) .EQ.'#')GOTO 1 ! опнвкх йнллемрюпхи х оепевхршбюел
  
        DO J=1,ILENTH1
        IF(STROKA2(J:J).NE.STR1(J:J)) GOTO 1      ! опнвкх мюгбюмхе оепелеммни
        END DO
        IFLAG=ILENTH1-1
            
2  DO K=IFLAG,100
    IF (STROKA2(K:K).EQ.'=') THEN              ! мюькх нцпюмхвхрекэ гмювемхъ
         DO L=K+1,K+50                        ! оепелеярхкх оепелеммсч
         STR1(L-K:L-K)=STROKA2(L:L)
         END DO
        CLOSE(NN)
        RETURN
    END IF    
    END DO

3    DO I=1,50
     STR1(I:I)= NAME(I:I)
     END DO
CLOSE(NN)
RETURN
    END
!-------------------------------------------------------------
! гЮОХЯЭ ВХЯКЮ Б ТЮИК
!-------------------------------------------------------------
 Subroutine Ini_Write_Str(str,str1,str3)
implicit none
CHARACTER *50 STR,STR1,STR3
CHARACTER *50 STR2
CHARACTER *100 STROKA, STROKA1,STROKA2
INTEGER NN,I,J,ILENTH,ILENTH1,ILENTH2,K,L,IFLAG,ISTAT
INTEGER II, II_FULL   ! вхякн ярпнй астепю
CHARACTER *100000 BUFF
! STR  - ярпнйю я хдемрхтхйюрнпнл оепелеммни тюикю
! STR1 - CTPOKA я хлемел оепелеммни
!--------------------------------------------------------
! ондюбкъел опнаекш
J=0
DO I=1,50
    IF(STR3(I:I).NE.' ') THEN
    J=J+1
    STR2(J:J)=STR3(I:I)
    END IF
END DO
DO I=1,50
STR3(I:I)=' '
END DO
DO I=1,J
STR3(I:I)= STR2(I:I)
END DO
    
! намскъел бшундмсч ярпнйс
DO I=1,50
STR2(I:I)=' '
END DO

! напюаюршбюел мюгбюмхе STR
ILENTH=0
DO I=1,50
    IF(STR(I:I).NE.' ')THEN
    ILENTH=ILENTH+1
    STR2(ILENTH:ILENTH)=STR(I:I) ! оепеохяюкх аег опнаекнб б STR2
    END IF
END DO
DO I=1,50
    STR(I:I)=STR2(I:I) ! оепеохяюкх аег опнаекнб б STR2
END DO
!----------------------------------------------------------------------------
! нрйпшбюел тюик гюохях йнмтхцспюжхнммшу дюммшу
NN=777
OPEN(UNIT=NN,FILE=TRIM(STR),STATUS='OLD')   ! нрйпшбюел тюик 
! оепеохяшбюел б астеп
II=0
! вхрюел тюик б астеп
5 READ(NN,999,end=4)STROKA 

II=II+1
DO I=1,100
BUFF((II-1)*100+I:(II-1)*100+I)=STROKA(I:I)
END DO
II_FULL=II
GOTO 5
4 CONTINUE ! бшькх он нйнмвюмхх тюикю
998 format(100a1)
999 format(a100)
!---------------------------------------------------------------------------
DO I=1,50
STR2(I:I)=' '
END DO
! напюаюршбюел мюгбюмхе STR1 - хлъ оепелеммни
ILENTH1=0
DO I=1,50
    IF(STR1(I:I).NE.' ')THEN
    ILENTH1=ILENTH1+1
    STR2(ILENTH1:ILENTH1)=STR1(I:I) ! оепеохяюкх аег опнаекнб б STR2
    END IF
END DO
DO I=1,50
    STR1(I:I)=STR2(I:I) ! оепеохяюкх хлъ оепелеммни аег опнаекнб б STR2
END DO
!----------------------------------------------------------------------------
 ! намскъел бшундмсч ярпнйс
 II=0
1   DO I=1,100
    STROKA(I:I)=' '
    STROKA2(I:I)=' '
    END DO
! вхрюел хг астепю
  II=II+1
  IF(II.GT.II_FULL) GOTO 3  ! сундхл еякх ме мюькх ярпнйс 
  DO J=1,100
  STROKA(J:J)=BUFF((II-1)*100+j:(II-1)*100+j)
  END DO 

  ILENTH2=0
  DO I=1,100
    IF(STROKA(I:I).NE.' ')THEN
    ILENTH2=ILENTH2+1
    STROKA2(ILENTH2:ILENTH2)=STROKA(I:I) ! оепеохяюкх аег опнаекнб б STROKA2
    END IF
  END DO

  IF(STROKA2(1:1) .EQ.'#')GOTO 1 ! опнвкх йнллемрюпхи х оепевхршбюел

        DO J=1,ILENTH1
        IF(STROKA2(J:J).NE.STR1(J:J)) GOTO 1     ! опнвкх мюгбюмхе оепелеммни
        END DO
        IFLAG=ILENTH1-1

            
2  DO K=IFLAG,100
    IF (STROKA2(K:K).EQ.'=') THEN              ! мюькх нцпюмхвхрекэ гмювемхъ
         DO L=K+1,K+50                        ! оепелеярхкх оепелеммсч
         STROKA2(L:L)=STR3(L-K:L-K)
         END DO
         DO I=1,100
             BUFF((II-1)*100+I:(II-1)*100+I)=STROKA2(I:I)
         END DO
! бняярюмюбкхбюел тюик 
            CLOSE(NN) 
            OPEN(UNIT=NN,FILE=TRIM(STR),STATUS='OLD')   ! нрйпшбюел тюик 
            DO II=1,II_FULL
            WRITE(NN,998)(BUFF((II-1)*100+I:(II-1)*100+I),I=1,100) 
            END DO
            
        CLOSE(NN)        
        RETURN
    END IF    
    END DO

3    CLOSE(NN)                              ! бшунд аег оепегюохях 
RETURN
END  
    
! open(unit=100, FILE='c:/1/OUTPUT.OUT')
! write(100,*)

