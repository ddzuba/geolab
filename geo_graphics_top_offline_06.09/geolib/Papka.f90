!----------------------------------------------------------------
!		ФОРМИРОВАНИЕ ИМЕНИ ФАЙЛА
!---------------------------------------------------------------
SUBROUTINE PAPKA_PATH_GEO(NAME_FILE0,N_VAY0,N_DIRECTION0,N_LINE0,GEO0) 
USE DFLIB
USE RAILWAY
USE PARAM_1
use FReciverGlobals
CHARACTER *300 NAME_FILE0			! Имя файла 
CHARACTER *200 N_VAY0			! номер дороги
CHARACTER *200 N_DIRECTION0		! код направления
CHARACTER *200 N_LINE0			! номер пути
CHARACTER *3 GEO0				! признак содержимого папки
CHARACTER*10 line1
INTEGER(4) KM
!-----------------------------------------------------------
! Формируем имя папки
! номер дороги_код направления_номер пути_число_месяц_год_geo
!-----------------------------------------------------------

!------------------------------------------------------------
! ИСПОЛЬЗУЕМ ВРЕМЯ ТЕКУЩЕЕ
CALL DATE_AND_TIME(DDATA,DDATA1)

!YER(1:4)=DDATA(1:4)
!MAUNS(1:2)=DDATA(5:6)
!DAY(1:2)=DDATA(7:8)
HAUR(1:2)=DDATA1(1:2)
MINIT(1:2)=DDATA1(3:4)
SEK(1:2)=DDATA1(5:6)
!-----------------------------------------------------------
! ИСПОЛЬЗУЕМ ВРЕМЯ ТРАССЫ
write(YER,'(I4)')wYEAR
write(MAUNS,'(I2)')wMonth
write(DAY,'(I2)')wDAY
!write(HAUR,'(I2)')wHour
!write(MINIT,'(I2)')wMinute
!write(SEK,'(I2)')wSecond
LINE1='          '
KM=kilometr
write(line1,'(I4)')KM

NAME_FILE0(1:50)='                                                  '

NAME_FILE0=TRIM(N_VAY0(1:N2_VAY))//'_'//TRIM(N_DIRECTION0(1:N2_DIR))//'_'//TRIM(N_LINE0(1:N2_TRA))//&
'_'//TRIM(ADJUSTL(DAY))//'_'//TRIM(ADJUSTL(MAUNS))//'_'//TRIM(ADJUSTL(YER))//'_'//TRIM(GEO0)&
//'_'//TRIM(ADJUSTL(HAUR))//'_'//TRIM(ADJUSTL(MINIT))//'_'//TRIM(ADJUSTL(SEK))//'_'//TRIM(ADJUSTL(LINE1))

RETURN
END SUBROUTINE PAPKA_PATH_GEO

!----------------------------------------------------------------
!			ДИАЛОГ ОТКРЫТИЯ ФАЙЛОВ ДАННЫХ И РЕЗУЛЬТАТОВ
!---------------------------------------------------------------
SUBROUTINE PAPKA_CREATE(NAME_FILE) 
USE DFWINA
USE DFLIB
TYPE (T_OPENFILENAME) FILE_PATH_
CHARACTER*(MAX_PATH) BUFFER
CHARACTER*(MAX_PATH) WINDOWSDIR
CHARACTER*(MAX_PATH) FILENAME
CHARACTER*(MAX_PATH) FILT
CHARACTER*(MAX_PATH) TITLE
INTRINSIC TRIM !Позволяет использовать имя вcтроенной функции в качестве фактического параметра
CHARACTER *200 NAME_FILE			! Имя файла 
integer lret
! ---------------------------------------------------------------
FILT="Text(*.csv)"//CHAR(0)//"*.csv"//CHAR(0)// &
	 "Geoskan Files (*.gpr)"//CHAR(0)//"*.gpr"//CHAR(0)// &
	 "Picter Files (*.bmp)"//CHAR(0)//"*.bmp"//CHAR(0)// &
	 "All Files (*.*)"//CHAR(0)//"*.*"//CHAR(0)//CHAR(0)

lret = GETCURRENTDIRECTORY(MAX_PATH,WINDOWSDIR)

BUFFER(1:200)='C:\GEODATA\'//TRIM(NAME_FILE)

TITLE(1:36)='Создание папки для выдачи результата'
! Описание параметров в файле HELP2.txt
FILE_PATH_%LSTRUCTSIZE=SIZEOF(FILE_PATH_)
FILE_PATH_%LPSTRFILTER=LOC(FILT)
FILE_PATH_%LPSTRCUSTOMFILTER=NULL
FILE_PATH_%NMAXCUSTFILTER=0
FILE_PATH_%NFILTERINDEX=0
FILE_PATH_%LPSTRFILE=LOC(BUFFER)						! ПУТЬ К ФАЙЛУ
FILE_PATH_%NMAXFILE=MAX_PATH
FILE_PATH_%LPSTRFILETITLE=NULL
FILE_PATH_%NMAXFILETITLE=0
!FILE_PATH_%LPSTRINITIALDIR=LOC(WINDOWSDIR)				! СТАНДАРТНЫЙ ПУТЬ К ФАЙЛУ
FILE_PATH_%LPSTRTITLE=LOC(TITLE)						! ИМЯ ДИАЛОГА
FILE_PATH_%FLAGS=IOR(OFN_HIDEREADONLY, OFN_PATHMUSTEXIST) !OFN_EXPLORER, OFN_PATHMUSTEXIST
FILE_PATH_%NFILEOFFSET=0
FILE_PATH_%NFILEEXTENSION=0
FILE_PATH_%LPSTRDEFEXT=NULL
FILE_PATH_%LCUSTDATA=0
FILE_PATH_%LPFNHOOK=NULL
FILE_PATH_%LPTEMPLATENAME=NULL

lret = GETOPENFILENAME(FILE_PATH_)

RETURN
END SUBROUTINE  PAPKA_CREATE

!open(unit=100, FILE='C:/OUTPUT.OUT')
!write(100,*) 
