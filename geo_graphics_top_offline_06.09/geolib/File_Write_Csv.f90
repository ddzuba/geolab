!------------------------------------------------------------------------------------------
! ДИАЛОГ ОТКРЫТИЯ ФАЙЛОВ ДАННЫХ И РЕЗУЛЬТАТОВ ДЛЯ ЗАПИСИ .BMP - ФАЙЛОВ
!------------------------------------------------------------------------------------------
	SUBROUTINE FILE_WRITE_CSV() 
	USE PARAM_
	USE FReciverGlobals
	USE PARAM_1
	USE RAILWAY
	use GeolibGlobals
	IMPLICIT NONE
	EXTERNAL INFILES_CSV
	FLAG=DLGINITWITHRESOURCEHANDLE(FILE_CSV, dllHandler, PARAMETR4) ! ИНИЦИАЛИЗАЦИЯ ДИАЛОГА
! ----------------------------------------------------------
	NP_PRINT=1									! ШАГ ВЫДАЧИ ТЕКСТОВОЙ ИНФОРМАЦИИ
!	DISK='C:'
	PAPKA=NAME_FILE_GEO
!	FILE_DAT='All_inform'
	GRAF='graf'					    ! ФАЙЛ ГЛУБИН
	VLAG='vlagh'				    ! ФАЙЛ ВЛАЖНОСТИ
	UGLUB='uglub'					! ФАЙЛ БАЛЛАСТНЫХ УГЛУБЛЕНИЙ
    TOLSH='TOLSH'                     ! ФАЙЛ ГЛУБИН БАЛЛАСТНЫХ УГЛУБЛЕНИЙ
	INFO='info'						! СЛУЖЕБНЫЙ ФАЙЛ
	FLAG=DLGSET(PARAMETR4,ERROR_FILE,'')        ! инициализация поля
	WRITE(STRING,*)	DISK	
	FLAG=DLGSET(PARAMETR4,F1,adjustl(STRING))	! инициализация поля
	WRITE(STRING,*)	PAPKA	
	FLAG=DLGSET(PARAMETR4,F2,adjustl(STRING))	! инициализация поля
	WRITE(STRING,*)	NP_PRINT	
	FLAG=DLGSET(PARAMETR4,F3,adjustl(STRING))	! инициализация поля
!	WRITE(STRING,*)	FILE_DAT	
!	FLAG=DLGSET(PARAMETR4,F5,adjustl(STRING))	! инициализация поля
	WRITE(STRING,*)	GRAF	
	FLAG=DLGSET(PARAMETR4,F6,adjustl(STRING))	! инициализация поля
	WRITE(STRING,*)	VLAG	
	FLAG=DLGSET(PARAMETR4,F7,adjustl(STRING))	! инициализация поля
	WRITE(STRING,*)	UGLUB	
	FLAG=DLGSET(PARAMETR4,F8,adjustl(STRING))	! инициализация поля
	WRITE(STRING,*)	INFO	
	FLAG=DLGSET(PARAMETR4,F10,adjustl(STRING))	! инициализация поля

	FLAG=DLGSETSUB(PARAMETR4,F1,INFILES_CSV)
	FLAG=DLGSETSUB(PARAMETR4,F2,INFILES_CSV)
	FLAG=DLGSETSUB(PARAMETR4,F3,INFILES_CSV)
	FLAG=DLGSETSUB(PARAMETR4,F5,INFILES_CSV)
	FLAG=DLGSETSUB(PARAMETR4,F6,INFILES_CSV)
	FLAG=DLGSETSUB(PARAMETR4,F7,INFILES_CSV)
	FLAG=DLGSETSUB(PARAMETR4,F8,INFILES_CSV)
	FLAG=DLGSETSUB(PARAMETR4,F10,INFILES_CSV)

	FLAG=DLGSETSUB(PARAMETR4,YES,INFILES_CSV)
	FLAG=DLGSETSUB(PARAMETR4,NO,INFILES_CSV)
	FLAG=DLGSETSUB(PARAMETR4,NO1,INFILES_CSV)

	STATUS=DLGMODAL(PARAMETR4)
	RETURN
      END SUBROUTINE  FILE_WRITE_CSV

	SUBROUTINE INFILES_CSV(DLG,C_NAME,CBTYPE)
	USE PARAM_
	USE PARAM_1
	USE FReciverGlobals
	USE RAILWAY
	TYPE(DIALOG)::DLG
	INTEGER*4  C_NAME,CBTYPE
	I=CBTYPE
	FLAG=DLGGET(DLG,C_NAME,STRING)
	SELECT CASE (C_NAME)
	CASE(NO)
	NP_PRINT=0				! КЛЮЧ ЗАПИСИ ТЕКСТОВОГО ФАЙЛА
	IFL_WRITER_KASKAD=0		! ЗАПИСЬ ДЛЯ КАСКАДА НЕ ПРОИЗВОДИТЬ
	CALL DLGEXIT(DLG)		! закрываем диалог
	CASE(NO1)
	STOP
	CASE(YES)
	IFL_WRITER_KASKAD=1		! ЗАПИСЬ ДЛЯ КАСКАДА ПРОИЗВОДИТЬ
	FLAG=DLGGET(DLG,F1,STRING)
	READ(STRING,*)DISK
	FLAG=DLGGET(DLG,F2,STRING)
	READ(STRING,*)PAPKA
	FLAG=DLGGET(DLG,F3,STRING)
	READ(STRING,*)NP_PRINT
!	FLAG=DLGGET(DLG,F5,STRING)
!	READ(STRING,*)FILE_DAT
	FLAG=DLGGET(DLG,F6,STRING)
	READ(STRING,*)GRAF
	FLAG=DLGGET(DLG,F7,STRING)
	READ(STRING,*)VLAG
	FLAG=DLGGET(DLG,F8,STRING)
	READ(STRING,*)UGLUB
	FLAG=DLGGET(DLG,F10,STRING)
	READ(STRING,*)INFO
!-----------------------------------------------------------------------
	lret = make_folder(DISK,PAPKA) ! СОЗДАЕМ ПАПКУ
!----------------------------------------------------------------------
DO N_CHEN_TEK=1,N_CHEN						! НОМЕР ВЫБРАННОГО КАНАЛА
	WRITE(STRING_CHEN, '(i1)') N_CHEN_TEK	! Записываем номер канала в строку
!-----------------------------------------------------------------------
! ФАЙЛ GRAF
	NAME_GRAF=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(GRAF)//'_'//TRIM(STRING_CHEN)//'.csv')
	OPEN(UNIT=10,FILE=NAME_GRAF,ERR=5,STATUS='REPLACE',POSITION='APPEND')  ! ОТКРЫТИЕ ФАЙЛА ДАННЫХ
	GOTO 6
5	FLAG=DLGSET(DLG,ERROR_FILE,'Ошибка в задании файла слоев')
	RETURN
6	FLAG_NAME_GEO=1
	STRING_200=ADJUSTL('КОД НАПРАВЛЕНИЯ; НОМЕР ПУТИ; НОМЕР КАНАЛА; КИЛОМЕТР; МЕТР; ГЛУБИНА ПЕРВОГО СЛОЯ; ГЛУБИНА ВТОРОГО СЛОЯ')
	WRITE(10,100) STRING_200
	GRAF_NUMBER(N_CHEN_TEK)=NAME_GRAF ! ЗАПОМИНАЕМ ИМЕНА
	CLOSE(10)	! ЗАКРЫТИЕ ФАЙЛА ДАННЫХ


!----------------------------------------------------------------------
! ФАЙЛ VLAG
	NAME_VLAG=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(VLAG)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=20,FILE=NAME_VLAG,ERR=7,STATUS='REPLACE',POSITION='APPEND')  ! ОТКРЫТИЕ ФАЙЛА ДАННЫХ
	GOTO 8
7	FLAG=DLGSET(DLG,ERROR_FILE,'Ошибка в задании файла влажностей')
	RETURN
8	STRING_200=ADJUSTL('КОД НАПРАВЛЕНИЯ; НОМЕР ПУТИ; НОМЕР КАНАЛА; НАЧАЛО КИЛОМЕТР; НАЧАЛО МЕТР; ОКОНЧАНИЕ КИЛОМЕТР; ОКОНЧАНИЕ МЕТР; ПРОТЯЖЕННОСТЬ; ВЕЛИЧИНА')
	WRITE(20,100) STRING_200
	VLAG_NUMBER(N_CHEN_TEK)=NAME_VLAG ! ЗАПОМИНАЕМ ИМЕНА
	CLOSE(20)	! ЗАКРЫТИЕ ФАЙЛА ДАННЫХ
!----------------------------------------------------------------------
! ФАЙЛ UGLUB
	NAME_UGLUB=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(UGLUB)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=30,FILE=NAME_UGLUB,ERR=9,STATUS='REPLACE',POSITION='APPEND')  ! ОТКРЫТИЕ ФАЙЛА ДАННЫХ
	GOTO 10
9	FLAG=DLGSET(DLG,ERROR_FILE,'Ошибка в задании файла балластных угл.')
	RETURN
10	STRING_200=ADJUSTL('КОД НАПРАВЛЕНИЯ; НОМЕР ПУТИ; НОМЕР КАНАЛА; НАЧАЛО КИЛОМЕТР; НАЧАЛО МЕТР; ОКОНЧАНИЕ КИЛОМЕТР; ОКОНЧАНИЕ МЕТР;ПРОТЯЖЕННОСТЬ;ХАРАКТЕРИСТИКА; ВЕЛИЧИНА')
	WRITE(30,100) STRING_200
	UGLUB_NUMBER(N_CHEN_TEK)=NAME_UGLUB ! ЗАПОМИНАЕМ ИМЕНА
	CLOSE(30)	! ЗАКРЫТИЕ ФАЙЛА ДАННЫХ
! ФАЙЛ TOLSH 
	NAME_TOLSH=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(TOLSH)//'_'//TRIM(STRING_CHEN)//'.csv')
    OPEN(UNIT=31,FILE=NAME_TOLSH,ERR=13,STATUS='REPLACE',POSITION='APPEND')  ! ОТКРЫТИЕ ФАЙЛА ДАННЫХ
	GOTO 14  
13  RETURN
14  STRING_200=ADJUSTL('КОД НАПРАВЛЕНИЯ; НОМЕР ПУТИ; НОМЕР КАНАЛА; НАЧАЛО КИЛОМЕТР; НАЧАЛО МЕТР; ОКОНЧАНИЕ КИЛОМЕТР; ОКОНЧАНИЕ МЕТР;ПРОТЯЖЕННОСТЬ;ВЕЛИЧИНА')
	WRITE(31,100) STRING_200
	TOLSH_NUMBER(N_CHEN_TEK)=NAME_TOLSH ! ЗАПОМИНАЕМ ИМЕНА
	CLOSE(31)	! ЗАКРЫТИЕ ФАЙЛА ДАННЫХ 
!----------------------------------------------------------------------
! ФАЙЛ INFO
	NAME_INFO=TRIM(TRIM(DISK)//'/'//TRIM(PAPKA)//'/'//TRIM(INFO)//'.xml')
    OPEN(UNIT=8,FILE=NAME_INFO,ERR=11,STATUS='REPLACE')  ! ОТКРЫТИЕ ФАЙЛА ДАННЫХ
	GOTO 12
11	FLAG=DLGSET(DLG,ERROR_FILE,'Ошибка в задании файла информации')
	RETURN
12	CALL INFO_XML()
	CLOSE(8)	! ЗАКРЫТИЕ ФАЙЛА ДАННЫХ
END DO
!----------------------------------------------------------------------
100 FORMAT(A200)

	CALL DLGEXIT(DLG)
	END SELECT
	END SUBROUTINE INFILES_CSV	
	
!open(unit=100, FILE='D:/My_fortran_proects_tvema/OUTPUT.OUT')
!write(100,*) 
