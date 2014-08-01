!------------------------------------------------------------------------------------------
!			ДИАЛОГ ОТКРЫТИЯ ФАЙЛА ЗАПИСИ ПОЛОЖЕНИЯ МЫШИ
!------------------------------------------------------------------------------------------
	SUBROUTINE FILE_WRITE_MOUSE() 
	USE PARAM_
	USE PARAM_1
	use GeolibGlobals
	EXTERNAL INFILES_TEXT

	FLAG=DLGINITWITHRESOURCEHANDLE(FILE_MOUSE, dllHandler, PARAMETR3) 
	! ИНИЦИАЛИЗАЦИЯ ДИАЛОГА
c ----------------------------------------------------------
	DISK_TEXT='C:'
	PAPKA_TEXT=NAME_FILE_IMG
	FILE_TEXT='MOUSE'
	FLAG=DLGSET(PARAMETR3,ERROR_FILE1,'')        ! инициализация поля
	WRITE(STRING,*)	DISK_TEXT	
	FLAG=DLGSET(PARAMETR3,GG1,adjustl(STRING))	! инициализация поля
	WRITE(STRING,*)	PAPKA_TEXT	
	FLAG=DLGSET(PARAMETR3,GG2,adjustl(STRING))	! инициализация поля
	WRITE(STRING,*)	FILE_TEXT	
	FLAG=DLGSET(PARAMETR3,GG3,adjustl(STRING))	! инициализация поля

	FLAG=DLGSETSUB(PARAMETR3,GG1,INFILES_TEXT)
	FLAG=DLGSETSUB(PARAMETR3,GG2,INFILES_TEXT)
	FLAG=DLGSETSUB(PARAMETR3,GG3,INFILES_TEXT)

	FLAG=DLGSETSUB(PARAMETR3,YES,INFILES_TEXT)
	FLAG=DLGSETSUB(PARAMETR3,NO,INFILES_TEXT)

	STATUS=DLGMODAL(PARAMETR3)
	RETURN
      END SUBROUTINE  FILE_WRITE_MOUSE

	SUBROUTINE INFILES_TEXT(DLG,C_NAME,CBTYPE)
	USE PARAM_
	USE PARAM_1
	TYPE(DIALOG)::DLG
	INTEGER*4  C_NAME,CBTYPE
	I=CBTYPE
	FLAG=DLGGET(DLG,C_NAME,STRING)
	SELECT CASE (C_NAME)
	CASE(NO)
	IFL_WRITER_TEXT=0		! КЛЮЧ ЗАПИСИ ПОЛОЖЕНИЯ МЫШИ НА ДИСК (0 - ЗАПИСИ НЕТ)
	CALL DLGEXIT(DLG)	! закрываем диалог
	
	CASE(YES)
	IFL_WRITER_TEXT=1		! КЛЮЧ ЗАПИСИ ФАЙЛОВ НА ДИСК (1 - ЗАПИСЬ ОСУЩЕСТВИТЬ)
	FLAG=DLGGET(DLG,GG1,STRING)
	READ(STRING,*)DISK_TEXT
	FLAG=DLGGET(DLG,GG2,STRING)
	READ(STRING,*)PAPKA_TEXT
	FLAG=DLGGET(DLG,GG3,STRING)
	READ(STRING,*)FILE_TEXT

!----------------------------------------------------------------------
!	WRITE(STRING_CHEN, '(i1)') N_CHEN	! Записываем номер канала в строку
!-----------------------------------------------------------------------
!	FILE_TEXT=TRIM(FILE_TEXT)//'_'//TRIM(STRING_CHEN)//'.dat'
	FILE_TEXT=TRIM(FILE_TEXT)//'.dat'
	lret = make_folder(DISK,PAPKA) ! СОЗДАЕМ ПАПКУ


	NAME_FILE_TEXT=TRIM(TRIM(DISK_TEXT)//
     +'/'//TRIM(PAPKA_TEXT)//'/'//TRIM(FILE_TEXT))

	OPEN(UNIT=15,FILE=NAME_FILE_TEXT,ERR=1,STATUS='REPLACE') ! ОТКРЫТИЕ ФАЙЛА ДАННЫХ
	WRITE(15,100)

	GOTO 2
1	FLAG=DLGSET(DLG,ERROR_FILE1,'Ошибка в задании файла данных')
!	CALL PAPKA_CREATE(PAPKA_TEXT) 
	RETURN
2	CLOSE(15)	! ЗАКРЫТИЕ ФАЙЛА ДАННЫХ
	CALL DLGEXIT(DLG)
	END SELECT
100	FORMAT(1X,'Номер',2X,'Километр',2x,'  Метр  ' )
	END SUBROUTINE INFILES_TEXT	
	
