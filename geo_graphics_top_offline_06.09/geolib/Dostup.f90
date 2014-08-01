SUBROUTINE DOSTUP
USE DOSTUP_
IMPLICIT NONE
!
! Проверяем наличие системного файла
!
!--------------------------------------------------------------------
K=1			! Можно менять номер ячейки для разных пакетов
!--------------------------------------------------------------------
OPEN(unit=777,file='SYS.SYS',form='binary',err=1,status='OLD')	! Открываем файл для печати
GOTO 2
1 TEXT='Выполните установку программного файла'
CALL INFORM()  
STOP											! Сообщение о необходимости загрузочного
!												! файла
! Считываем счетчик обращений
! 
2 REWIND 777									! Первая запись

READ(777) KEY_DOPUSK
J=IABS(KEY_DOPUSK(K))							! Номер элемента с ключем
IF(J.EQ.K)J=K+1									! Если совпадают и нет перехода
IF(J.EQ.0)J=K+1									! Если совпадают и нет перехода
!
! Увеличиваем счетчик обращений
!
KL=KEY_DOPUSK(J)-1

IF(KL.LT.0) THEN
TEXT='Ресурс использования программы исчерпан'
CALL INFORM() 
STOP
END IF
!
! Заполняем массив случайными числами
!	
I = 8
	  CALL SYSTEM_CLOCK(SEED(1))
      CALL RANDOM_SEED(SIZE=I)
      CALL RANDOM_SEED(PUT=SEED(1:8))
      CALL RANDOM_NUMBER(ARRAY)

      DO I = 1, 10000
			  KEY_DOPUSK(I) = (ARRAY(I) - 0.5) * 1000
      END DO
!
! Записываю ключ
!
J=IABS(KEY_DOPUSK(K))											! Номер массива с ключем
IF(J.EQ.K)J=K+1
IF(J.EQ.0)J=K+1									! Если совпадают и нет перехода
KEY_DOPUSK(J)=KL
	REWIND 777
	WRITE(777) KEY_DOPUSK


! open(unit=100, FILE='D:/My_fortran_proects_Tvema/OUTPUT.OUT')
! write(100,*)

!----------------------------------------------------------------
END 

!------------------------------------------------------------------------------------------
!			ДИАЛОГ ОТКРЫТИЯ ФАЙЛА ДОСТУПА
!------------------------------------------------------------------------------------------
	SUBROUTINE INFORM() 
	USE DOSTUP_
	use GeolibGlobals
	EXTERNAL INFORM_OUT

	FLAG=DLGINITWITHRESOURCEHANDLE(INFORMATION, dllHandler, PARAMETR_INFO) ! ИНИЦИАЛИЗАЦИЯ ДИАЛОГА
! ----------------------------------------------------------
	FLAG=DLGSET(PARAMETR_INFO,ERROR_INFO,TEXT)        ! инициализация поля
	FLAG=DLGSETSUB(PARAMETR_INFO,NO,INFORM_OUT)

	STATUS=DLGMODAL(PARAMETR_INFO)
	RETURN
    END SUBROUTINE  INFORM

	SUBROUTINE INFORM_OUT(DLG,C_NAME,CBTYPE)
	USE DOSTUP_
	TYPE(DIALOG)::DLG
	INTEGER*4  C_NAME,CBTYPE
	I=CBTYPE
	SELECT CASE (C_NAME)
	CASE(NO)
	CALL DLGEXIT(DLG)	! закрываем диалог
	STOP	
	END SELECT
	END SUBROUTINE INFORM_OUT