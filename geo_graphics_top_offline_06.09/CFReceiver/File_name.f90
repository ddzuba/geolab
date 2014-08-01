!----------------------------------------------------------------
!			ДИАЛОГ ОТКРЫТИЯ ФАЙЛОВ ДАННЫХ И РЕЗУЛЬТАТОВ
!---------------------------------------------------------------

SUBROUTINE GetFileName(hInst,hwnd,filt,NAME_FILE,IRES) 
USE DFWINA

INTEGER*4 hInst,hwnd,IRES
TYPE (T_OPENFILENAME) OF
CHARACTER*(MAX_PATH) BUFFER
CHARACTER*(MAX_PATH) WINDOWSDIR
CHARACTER*(*) NAME_FILE
CHARACTER*(*) FILT
INTRINSIC TRIM !Позволяет использовать имя вcтроенной функции в качестве фактического параметра

FILT="Geoskan Files (*.gpr)"//CHAR(0)//"*.gpr"//CHAR(0)// &
	 "Picter Files (*.bmp)"//CHAR(0)//"*.bmp"//CHAR(0)// &
     "GPS Files(*.log)"//CHAR(0)//"*.log"//CHAR(0)//&
     "Text Files(*.log)"//CHAR(0)//"*.txt"//CHAR(0)//&
	 "All Files (*.*)"//CHAR(0)//"*.*"//CHAR(0)//CHAR(0)
ires=0
BUFFER=' '
BUFFER(1:1)=CHAR(0)
IRET= GETCURRENTDIRECTORY(MAX_PATH,WINDOWSDIR)
! Описание параметров в файле HELP2.txt
OF%LSTRUCTSIZE=SIZEOF(OF)
OF%hwndOwner=hwnd
of%hInstance=hInst
OF%LPSTRFILTER=LOC(FILT)
OF%LPSTRCUSTOMFILTER=NULL
OF%NMAXCUSTFILTER=0
OF%NFILTERINDEX=0
OF%LPSTRFILE=LOC(BUFFER)
OF%NMAXFILE=MAX_PATH
OF%LPSTRFILETITLE=NULL
OF%NMAXFILETITLE=0
!OF%LPSTRINITIALDIR=LOC(WINDOWSDIR)
OF%LPSTRTITLE=NULL
OF%FLAGS=IOR(OFN_HIDEREADONLY, OFN_EXPLORER)
OF%NFILEOFFSET=0
OF%NFILEEXTENSION=0
OF%LPSTRDEFEXT=NULL
OF%LCUSTDATA=0
OF%LPFNHOOK=NULL
OF%LPTEMPLATENAME=NULL

IF(GETOPENFILENAME(OF) .EQV.0)THEN
IRES=0
ELSE
IRES=1
NAME_FILE=TRIM(BUFFER)
RETURN
END IF
END SUBROUTINE GetFileName
    
! open(unit=100, FILE='D:/OUTPUT.OUT')
! write(100,*)

    