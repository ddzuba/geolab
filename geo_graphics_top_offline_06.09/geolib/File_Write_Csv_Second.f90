!------------------------------------------------------------------------------------------
! дхюкнц нрйпшрхъ тюикнб дюммшу х пегскэрюрнб дкъ гюохях .BMP - тюикнб
!------------------------------------------------------------------------------------------
	SUBROUTINE FILE_WRITE_CSV_SECOND() 
	USE PARAM_
	USE FReciverGlobals
	USE PARAM_1
	USE RAILWAY
	IMPLICIT NONE
    
    integer lret, make_folder
    
IF(IFL_WRITER_KASKAD.EQ.1) THEN		! гюохяэ дкъ йюяйюдю опнхгбндхрэ
PAPKA=NAME_FILE_GEO
!-----------------------------------------------------------------------
	lret = make_folder(DISK,PAPKA) ! янгдюел оюойс
!----------------------------------------------------------------------
DO N_CHEN_TEK=1,N_CHEN						! мнлеп бшапюммнцн йюмюкю
	WRITE(STRING_CHEN, '(i1)') N_CHEN_TEK	! гЮОХЯШБЮЕЛ МНЛЕП ЙЮМЮКЮ Б ЯРПНЙС
!-----------------------------------------------------------------------
! тюик GRAF
	NAME_GRAF=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(GRAF)//'_'//TRIM(STRING_CHEN)//'.csv')
	OPEN(UNIT=10,FILE=NAME_GRAF,ERR=5,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	GOTO 6
5	RETURN
!	FLAG_NAME_GEO=1
6	STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; йхкнлерп; лерп; цксахмю оепбнцн якнъ; цксахмю брнпнцн якнъ')
	WRITE(10,100) STRING_200
	GRAF_NUMBER(N_CHEN_TEK)=NAME_GRAF ! гюонлхмюел хлемю
	CLOSE(10)	! гюйпшрхе тюикю дюммшу

!----------------------------------------------------------------------
! тюик VLAG
	NAME_VLAG=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(VLAG)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=20,FILE=NAME_VLAG,ERR=7,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	GOTO 8
7	RETURN
8	STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; мювюкн йхкнлерп; мювюкн лерп; нйнмвюмхе йхкнлерп; нйнмвюмхе лерп; опнръфеммнярэ; бекхвхмю')
	WRITE(20,100) STRING_200
	VLAG_NUMBER(N_CHEN_TEK)=NAME_VLAG ! гюонлхмюел хлемю
	CLOSE(20)	! гюйпшрхе тюикю дюммшу
!----------------------------------------------------------------------
! тюик UGLUB
	NAME_UGLUB=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(UGLUB)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=30,FILE=NAME_UGLUB,ERR=9,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	GOTO 10
9	RETURN
10	STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; мювюкн йхкнлерп; мювюкн лерп; нйнмвюмхе йхкнлерп; нйнмвюмхе лерп;опнръфеммнярэ;уюпюйрепхярхйю; бекхвхмю')
	WRITE(30,100) STRING_200
	UGLUB_NUMBER(N_CHEN_TEK)=NAME_UGLUB ! гюонлхмюел хлемю
	CLOSE(30)	! гюйпшрхе тюикю дюммшу
! тюик TOLSH 
	NAME_TOLSH=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(TOLSH)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=31,FILE=NAME_TOLSH,ERR=13,STATUS='REPLACE',POSITION='APPEND')  ! нрйпшрхе тюикю дюммшу
	GOTO 14  
13  RETURN
14  STRING_200=ADJUSTL('йнд мюопюбкемхъ; мнлеп осрх; мнлеп йюмюкю; мювюкн йхкнлерп; мювюкн лерп; нйнмвюмхе йхкнлерп; нйнмвюмхе лерп; опнръфеммнярэ; бекхвхмю')
	WRITE(31,100) STRING_200
	TOLSH_NUMBER(N_CHEN_TEK)=NAME_TOLSH ! гюонлхмюел хлемю
	CLOSE(31)	! гюйпшрхе тюикю дюммшу    
!----------------------------------------------------------------------
! тюик INFO
	NAME_INFO=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(INFO)//'.xml')
    OPEN(UNIT=8,FILE=NAME_INFO,ERR=11,STATUS='REPLACE')  ! нрйпшрхе тюикю дюммшу
	GOTO 12
11	RETURN
12	CALL INFO_XML()
	CLOSE(8)	! гюйпшрхе тюикю дюммшу
END DO
!----------------------------------------------------------------------
100 FORMAT(A200)

END IF
RETURN
END SUBROUTINE FILE_WRITE_CSV_SECOND	
	
!open(unit=100, FILE='D:/My_fortran_proects_tvema/OUTPUT.OUT')
!write(100,*) 
