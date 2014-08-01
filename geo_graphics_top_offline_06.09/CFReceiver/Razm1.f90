    SUBROUTINE RAZM1() ! РАЗМЕЩАЕМ МАССИВЫ КООРДИНАТ ОТ СПУТНИКА
    USE KOORDINATA
    USE DFWINA
    USE GLOBALS_LOG
    USE INPUT_RADAROGRAMMA

    IMPLICIT NONE
    INTEGER (4) JJ,J,ISTAT
! ОТКРЫВАЕМ ОТРЕДАКТИРОВАННЫЙ ФАЙЛ
    NN1=201
     ! ВВОДИМ ФАЙЛ 
    FF1='Cfreceiver.cfg'
    FF2='FILE_TREK'
    FF3='_'
    CALL Ini_Read_Str(FF1,FF2,FF3)  
    FILE_TREK=FF2 
    IF(FF2.NE.'_')OPEN(UNIT=NN1,FILE=FILE_TREK,ERR=4,STATUS='OLD')
    GOTO 1
4   MESS=MESSAGEBOX(NULL,"Прервать работу работу - Да"C,"Файл с данными координат отсутствует"C,0)    ! Если не открываем файл, то
    IF(MESS.EQ.1) THEN                                                                                ! Заканчиваем работу
    STOP                                                                                              ! Или повторяем попытку
    END IF
! ОПРЕДЕЛЯЕМ ЧИСЛО ЗАПИСЕЙ В ФАЙЛЕ ОБМЕНА С GPS ПРИЕМНИКОМ
1	IPOINT=0							! ФОРМИРУЕМ ЧИСЛО ЗАПИСЕЙ В ФАЙЛЕ
	REWIND(NN1)							! УСТАНАВЛИВАЕМ МАРКЕР НА НАЧАЛО ФАЙЛА 
2	READ(NN1,101,IOSTAT=ISTAT)TM			! СЧИТЫВАЕМ ТЕКСТОВУЮ СТРОКУ
	IF(ISTAT.EQ.-1) GOTO 3

	JJ=0								! ПОДАВЛЯЕМ ПУСТЫЕ СТРОКИ 
	DO J=1,74
	IF(TM(J:J).NE.' ')JJ=1
	END DO
	IF(JJ.EQ.0)GOTO 2

	IPOINT=IPOINT+1
	GOTO 2
3	ALLOCATE (VREMYA(IPOINT))			! РАЗМЕЩАЕМ МАССИВЫ ДЛЯ ИНФОРМАЦИИ ОТ GPS ПРИЕМНИКА 
	ALLOCATE (SHIROTA(IPOINT))
	ALLOCATE (DOLGOTA(IPOINT))
!	WRITE(30,102) NAMEDATA_LOG,IPOINT
101   FORMAT(A74)
!---------------------------------------------------------------------------------------------------
! РАЗМЕЩАЕМ МАССИВЫ ДЛЯ ИНФОРМАЦИИ ОТ ТРАСС ГЕОРАДАРА
      ALLOCATE (VREMYA_TRASSA(NTR))			! ВРЕМЯ РЕГИСТРАЦИИ ТРАССЫ
      ALLOCATE (R_TRASSA(NTR))				! РАССТОЯНИЕ МЕЖДУ ТРАССАМИ
      ALLOCATE (SHIROTA_TRASSA(NTR))			! ШИРОТА ТРАССЫ
      ALLOCATE (DOLGOTA_TRASSA(NTR))			! ДОЛГОТА ТРАССЫ
    END SUBROUTINE RAZM1
    
    
    SUBROUTINE RAZM2() ! РАЗМЕЩАЕМ КООРДИНАТ КИЛОМЕТРОВЫХ СТОЛБОВ
    USE KOORDINATA
    USE DFWINA
    USE GLOBALS_LOG
    USE INPUT_RADAROGRAMMA

    IMPLICIT NONE
    INTEGER (4) JJ,J,ISTAT
!---------------------------------------------------------------------------------------------------
      ! ОТКРЫВАЕМ ФАЙЛ С КООРДИНАТАМИ КИЛОМЕТРОВЫХ СТОЛБОВ
    NN2=202
     ! ВВОДИМ ФАЙЛ ДЛЯ РЕДАКТИРОВАНИЯ
    FF1='Cfreceiver.cfg'
    FF2='NAME_KIL_METR'
    FF3='_'
    CALL Ini_Read_Str(FF1,FF2,FF3)  
    NAME_KIL_METR=FF2 
    IF(FF2.NE.'_')OPEN(UNIT=NN2,FILE=NAME_KIL_METR ,ERR=14,STATUS='OLD')
    GOTO 11
14  MESS=MESSAGEBOX(NULL,"Прервать работу работу - Да"C,"Файл с километровыми столбами отсутствует"C,0)    ! Если не открываем файл, то
    IF(MESS.EQ.1) THEN                                                                              ! Заканчиваем работу
    STOP                                                                                          ! Или повторяем попытку
    END IF
! ОПРЕДЕЛЯЕМ ЧИСЛО ЗАПИСЕЙ В ФАЙЛЕ ОБМЕНА С GPS ПРИЕМНИКОМ
11	IPOINT1=0							! ФОРМИРУЕМ ЧИСЛО ЗАПИСЕЙ В ФАЙЛЕ
	REWIND(NN2)							! УСТАНАВЛИВАЕМ МАРКЕР НА НАЧАЛО ФАЙЛА 
12	READ(NN2,101,IOSTAT=ISTAT)TM			! СЧИТЫВАЕМ ТЕКСТОВУЮ СТРОКУ
	IF(ISTAT.EQ.-1) GOTO 13

	JJ=0								! ПОДАВЛЯЕМ ПУСТЫЕ СТРОКИ 
	DO J=1,74
	IF(TM(J:J).NE.' ')JJ=1
	END DO
	IF(JJ.EQ.0)GOTO 12

	IPOINT1=IPOINT1+1
	GOTO 12
13	ALLOCATE (KLM(IPOINT1))			! РАЗМЕЩАЕМ МАССИВЫ ДЛЯ ИНФОРМАЦИИ ОТ GPS ПРИЕМНИКА 
	ALLOCATE (SHIR(IPOINT1))
	ALLOCATE (DLGT(IPOINT1))
    ALLOCATE (R_STOLB(IPOINT1))
    ALLOCATE (RAB(NTR))
101   FORMAT(A74)
      RETURN
    END SUBROUTINE RAZM2
! open(unit=100, FILE='c:/1/OUTPUT.OUT')
! write(100,*)