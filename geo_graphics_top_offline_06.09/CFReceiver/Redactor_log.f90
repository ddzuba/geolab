integer*4 function redactor_log()
    !DEC$ ATTRIBUTES STDCALL :: redactor_log
    use Globals_Log
    USE DFWINA
    implicit none
    INTEGER*4 FILE_WAY
    CHARACTER*100 FF1,FF2,FF3
   
    redactor_log=0
    ! Откроем редактируемый файл
3   NN=200
    ! ВВОДИМ ФАЙЛ ДЛЯ РЕДАКТИРОВАНИЯ
        FF1='Cfreceiver.cfg'
        FF2='NAME_FILE_LOG'
        FF3='______________'
        CALL Ini_Read_Str(FF1,FF2,FF3)  
        NAME_FILE_LOG=FF2
    OPEN(UNIT=NN,FILE=TRIM(NAME_FILE_LOG) ,ERR=1,STATUS='OLD')          ! Открываем файл по номеру NN
    GOTO 2 
1   MESS=MESSAGEBOX(NULL,"Прервать работу - Да, Повторить выбор - Нет"C,"Редактируемый файл не существует"C,4)    ! Если не открываем файл, то
	IF(MESS.EQ.6) THEN                                                                              ! Заканчиваем работу
    STOP                                ! ПРЕРЫВАЕМ РАБОТУ
    ELSE
    GOTO 3                                                                                          ! Или повторяем попытку
    END IF
 ! Откроем отредактированный файл
2   NN1=201
     ! ВВОДИМ ФАЙЛ ДЛЯ РЕДАКТИРОВАНИЯ
    FF1='Cfreceiver.cfg'
    FF2='FILE_LOG_RED'
    FF3='______________'
    CALL Ini_Read_Str(FF1,FF2,FF3)  
    FILE_LOG_RED=FF2 
    IF(TRIM(NAME_FILE_LOG).EQ.TRIM(FILE_LOG_RED)) THEN
        MESS=MESSAGEBOX(NULL,"Изменить отредактированный файл - Да, Прервать работу - Нет"C,"Отредактированный файл уже существует"C,4)    ! Если не открываем файл, то
        IF(MESS.EQ.6) THEN                                                                              ! Заканчиваем работу
    ! открываем редактируемый файл
        GOTO 2                                ! ПРЕРЫВАЕМ РАБОТУ
        ELSE
        STOP                                                                                          ! Или повторяем попытку
        END IF
    END IF
    OPEN(UNIT=NN1,FILE=FILE_LOG_RED ,ERR=4,STATUS='NEW')
    GOTO 5
4   MESS=MESSAGEBOX(NULL,"Продолжить работу работу - Да, Прервать работу - Нет"C,"Используем имеющийся отредактированный файл"C,4)    ! Если не открываем файл, то
 	IF(MESS.EQ.6) THEN                                                                              ! Заканчиваем работу
        OPEN(UNIT=NN1,FILE=FILE_LOG_RED ,ERR=4,STATUS='REPLACE') ! открываем отредактированный файл
!       NAME_FILE_LOG =''
!       FILE_LOG_RED =''
        GOTO 5                                ! ПРЕРЫВАЕМ РАБОТУ
        ELSE
        STOP                                                                                          ! Или повторяем попытку
        END IF
5 CALL READ_WRITE(NN,NN1)
  CLOSE(NN)
  CLOSE(NN1)
  redactor_log=1
  RETURN
end 

    
!
! ЧТЕНИЕ В ФОРМЕ .LOG
!
      SUBROUTINE READ_WRITE(NN,NN1)	! ЧТЕНИЕ ВРЕМЕНИ И КООРДИНАТ  
      IMPLICIT NONE
      CHARACTER  *100 TM          ! редактируемая строка
      CHARACTER  *1 STR,STR1           ! рабочая строка
      INTEGER*4 NN,NN1
      INTEGER*4 I,ISTAT,J,JJ
	REWIND(NN)					! УСТАНАВЛИВАЕМ МАРКЕР НА НАЧАЛО ФАЙЛА 
	REWIND(NN1)					! УСТАНАВЛИВАЕМ МАРКЕР НА НАЧАЛО ФАЙЛА 

1	READ(NN,101,IOSTAT=ISTAT)TM	! СЧИТЫВАЕМ ТЕКСТОВУЮ СТРОКУ
101   FORMAT(A100)
          IF(ISTAT.EQ.-1) THEN    ! ОБРАБАТЫВАЕМ КОНЕЦ ФАЙЛА
      	RETURN		
          END IF
          	JJ=0		        ! ПОДАВЛЯЕМ ПУСТЫЕ СТРОКИ 
          	DO J=1,100
          	IF(TM(J:J).NE.' ')JJ=1
              END DO
           	IF(JJ.EQ.0)GOTO 1
                  
                  STR=''          ! ПОДАВЛЯЕМ ОТСУТСТВИЕ СПУТНИКОВ 
          	    DO J=1,100
          	    IF(TM(J:J).NE.' ')THEN
                    STR1=STR
                    STR=TM(J:J)
                    END IF
                  END DO
           	    IF(STR.EQ.'0'.AND.STR1.EQ.' ')GOTO 1
                  
                  JJ=0		        ! ПРОВЕРЯЕМ E,H И ЧИСЛО СПУТНИКОВ
              	DO J=1,100
           	    IF(TM(J:J).EQ.'N'.OR.TM(J:J).EQ.'E')JJ=1
                  END DO  
                  IF(JJ.EQ.0) GOTO 1 ! ПЕРЕХОДИМ НА НОВУЮ СТРОКУ ФАЙЛА
                  
      WRITE(NN1,101)TM	!  ЗАПИСЫВАЕМ ТЕКСТОВУЮ СТРОКУ 
   	GOTO 1
	END SUBROUTINE READ_WRITE

! open(unit=100, FILE='D:/OUTPUT.OUT')
! write(100,*)
! open(unit=100, FILE='C:/1/OUTPUT.OUT')
! write(100,*)NAME_FILE_LOG