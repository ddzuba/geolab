subroutine MouseClick(x, y)
  use user32
  use gdi32 
  use FReciverGlobals
  USE PARAM_
  USE DFLIB
  USE DFWIN
  USE PARAM_1
  USE RAILWAY
  USE GeolibGlobals
  USE GRAFICA
  USE AviTools
  implicit none
  integer x, y, hdc,I,I1,J,J1,pos,IRAB,ret
  REAL*4 RAB,realtime
  type ( T_PAINTSTRUCT ) ps
  
  if (N_PRINT.le.0 .or. isStarted.eq.0) then
      ! если еще ничего не рисовалось или обработка остановлена - ничего не делаем.
      return
  end if

! cursor_pos = положение курсора (номер трассы)
  cursor_pos = TRACE_COUNT/N_PRINT - img_w + x
  
  if (TRACE_COUNT/N_PRINT.lt.img_w) then
	cursor_pos = x
  end if
  call redrawScreen()
  
IF (IFL_WRITER_TEXT.NE.0.AND.IFL_WRITER.NE.0) THEN
OPEN(UNIT=15,FILE=NAME_FILE_TEXT,STATUS='OLD') ! ОТКРЫТИЕ ФАЙЛА ДАННЫХ
REWIND 15
READ(15,*) ! ЗАГОЛОВОК
I1=IFL_WRITER_TEXT-1
IF(I1.NE.0) THEN
DO I=1,I1
READ(15,*)
END DO
END IF

IF(cursor_pos.GT.TRACE_COUNT/N_PRINT)cursor_pos=TRACE_COUNT/N_PRINT      ! ЧТО-БЫ НЕ ВЫХОДИТЬ ЗА РИСУНОК
RAB=(TRACE_COUNT/N_PRINT-cursor_pos)*(ISTEP_BETWEEN_TRASES/1000.)        ! ПОПРАВКА В МЕТРАХ

IF(RAB.GT.1000) THEN
    IRAB=FLOOR(RAB/1000+0.00001)                                             ! ЧИСЛО КИЛОМЕТРОВ
    KILOMETR_MOUSE=KILOMETR-IRAB
    RAB=RAB-IRAB*1000
    IF(RAB.GT.METR)THEN
    KILOMETR_MOUSE=KILOMETR_MOUSE-1
    METR_MOUSE=1000-(RAB-METR)
    ELSE
    KILOMETR_MOUSE=KILOMETR_MOUSE
    METR_MOUSE=METR-RAB
    END IF
ELSE
    IF(RAB.GT.METR)THEN
    KILOMETR_MOUSE=KILOMETR-1
    METR_MOUSE=1000-(RAB-METR)
    ELSE
    KILOMETR_MOUSE=KILOMETR
    METR_MOUSE=METR-RAB
    END IF
END IF

IF (IFL_WRITER_TEXT.NE.0.AND.IFL_WRITER.NE.0)&
WRITE(15,100)IFL_WRITER_TEXT,cursor_pos,KILOMETR_MOUSE, METR_MOUSE  !cursor_pos*N_PRINT
IFL_WRITER_TEXT=IFL_WRITER_TEXT+1
CLOSE(15)
END IF
100 FORMAT(1X,I3,6X,I6,8X,F8.1,9X,F10.4)
!----------------------------------------------------------------------------------------------
! ВЫЗОВ ИЗОБРАЖЕНИЯ
IF(VIDEO_NAME.NE.'ZERO')THEN
realtime=real(VREMYA_MOUSE(cursor_pos-IMG_W*floor(cursor_pos*1./IMG_W)+1)-SEKK0)
realtime=realtime-TIME_VIDEO_FILE
if(realtime.le.0.0) realtime=0.0
ret= ShowFrame(VIDEO_NAME,realtime,KILOMETR_MOUSE,METR_MOUSE)
END IF
    end

!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*) N