!-----------------------------------------------------------------------------------
! ФОРМИРУЕМ НОВОЕ ИМЯ ДЛЯ ЗАПИСИ .BMP- ФАЙЛОВ
!-----------------------------------------------------------------------------------
SUBROUTINE CALC_NAME()										
USE PARAM_
USE PARAM_1
USE FReciverGlobals
IMPLICIT NONE	
CHARACTER (200) TEXT,TEXT1,TEXT2,TEXT3,TEXT4

! ----------------------------------------------------------
WRITE(TEXT, '(i1)') N_CHEN	! Записываем номер канала в строку
! ----------------------------------------------------------
WRITE(TEXT1, *) KILOMETR_1		! Записываем номер канала в строку
WRITE(TEXT2, *) METR_1			! Записываем номер канала в строку
WRITE(TEXT3, *)	INT4(KILOMETR)	! Записываем номер канала в строку
WRITE(TEXT4, *) INT4(METR)		! Записываем номер канала в строку

NAME_FILE=TRIM(NAME_FILE(1:I_LENTH))//TRIM(ADJUSTL(TEXT1))//'_'&
//TRIM(ADJUSTL(TEXT2))//'_'//TRIM(ADJUSTL(TEXT3))//'_'&
//TRIM(ADJUSTL(TEXT4))//'_'//TRIM(TEXT)//'.bmp'

KILOMETR_1=KILOMETR
METR_1=METR

RETURN
END SUBROUTINE CALC_NAME