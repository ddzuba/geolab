SUBROUTINE FILE_WRITE_SECOND() 
USE PARAM_
USE FReciverGlobals
USE PARAM_1
USE RAILWAY

IMPLICIT NONE

integer lret, make_folder
	
IF(IFL_WRITER.EQ.1) THEN		! йкчв гюохях тюикнб мю дхяй (1 - гюохяэ нясыеярбхрэ)
PAPKA=NAME_FILE_IMG
!--------------------------------------------------------------------
	lret = make_folder(DISK,PAPKA) ! янгдюел оюойс
!--------------------------------------------------------------------
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/')

	I_LENTH=LEN_TRIM(NAME_FILE)		!  дкхмю хлемх
	NAME_FILE =TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(FILE))

	OPEN(UNIT=40,FILE=NAME_FILE ,FORM='BINARY',ERR=1,STATUS='REPLACE') ! нрйпшрхе тюикю дюммшу
	GOTO 2
1	RETURN
!	FLAG_NAME_IMG=1
2	CLOSE(40,STATUS='DELETE')	! гюйпшрхе тюикю дюммшу
! тюик лшьх -----------------------------------------------------------------
	IFL_WRITER_TEXT=1		! йкчв гюохях тюикнб мю дхяй (1 - гюохяэ нясыеярбхрэ)
!	FILE_TEXT=TRIM(FILE_TEXT)//'.DAT'
	NAME_FILE_TEXT=TRIM(TRIM(DISK)//&
     '/'//TRIM(PAPKA)//'/'//TRIM(FILE_TEXT))

	OPEN(UNIT=15,FILE=NAME_FILE_TEXT,ERR=3,STATUS='REPLACE') ! нрйпшрхе тюикю дюммшу
	WRITE(15,100)

	GOTO 4
3	RETURN
4	CLOSE(15)	! гюйпшрхе тюикю дюммшу
100	FORMAT(1X,'мНЛЕП',4X,'йХКНЛЕРП',6x,'  лЕРП  ' )

END IF
RETURN
END SUBROUTINE FILE_WRITE_SECOND	
	
