module Fortran_WinPrint



use DFWINTY, only: DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE, &
                   DMDUP_SIMPLEX, DMDUP_HORIZONTAL, DMDUP_VERTICAL


implicit none

private
public Print_Bitmap
INTEGER*4 , parameter, public :: &
    FWP_ERR_BADUNIT = -1 ,&          ! Unit was not open for formatted, sequential access
    FWP_ERR_PRINTDLGFAILED = -2, &   ! Call to PrintDlg failed
    FWP_ERR_GLOBALLOCKFAILED = -3, & ! Call to GlobalLock failed
    FWP_ERR_RESETDCFAILED = -4, &	 ! Call to ResetDC failed
    FWP_ERR_GLOBALUNLOCKFAILED = -5, &  ! Call to GlobalUnlock failed
	FWP_ERR_OPENPRINTERFAILED = -6   ! Call to OpenPrinter failed

! Constants for Orientation argument
!
INTEGER*4 , parameter, public :: &
    FWP_ORIENTATION_DEFAULT = -1, &   ! Use default orientation as returned
                                     ! From PrintDlg
    FWP_ORIENTATION_PORTRAIT = DMORIENT_PORTRAIT, &  ! Force portrait orientation
    FWP_ORIENTATION_LANDSCAPE = DMORIENT_LANDSCAPE    ! Force landscape orientation

! Constants for DuplexMode argument
!
INTEGER*4 , parameter, public :: &
    FWP_DUPLEXMODE_DEFAULT = -1, &      ! Use default duplex mode
	FWP_DUPLEXMODE_HORIZONTAL = DMDUP_HORIZONTAL, &  ! Long edge horizontal
	FWP_DUPLEXMODE_VERTICAL = DMDUP_VERTICAL         ! Long edge vertical


INTEGER*4 , public :: last_error     ! Holds last return status from API routine

contains




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





INTEGER*4  function Print_Bitmap (Default_Printer, Orientation)

use comdlg32, only: PrintDlg, CommDlgExtendedError, T_PRINTDLG, PD_ALLPAGES, PD_RETURNDC, &
                    PD_NOPAGENUMS, PD_RETURNDEFAULT, PD_NOSELECTION
use gdi32, only: DeleteDC, StartDoc, StartPage, EndDoc, EndPage, TextOut, &
		         GetDeviceCaps, GetTextExtentPoint32, GetTextExtentExPoint, &
				 CreateFont, SelectObject, DeleteObject, ResetDC,  SetBkMode,&
				 DM_ORIENTATION, DM_DUPLEX, DM_IN_BUFFER, DM_OUT_BUFFER, &
				 PHYSICALOFFSETX, PHYSICALOFFSETY, HORZRES, VERTRES, &
				 LOGPIXELSX, LOGPIXELSY, T_DOCINFO, T_SIZE, T_DEVMODE, &
				 DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE, TRANSPARENT, &
				 FW_NORMAL, ANSI_CHARSET, OUT_DEFAULT_PRECIS, &
				 CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, &
				 DEFAULT_PITCH, FF_DONTCARE, PRINTER_ACCESS_USE, T_PRINTER_DEFAULTS, &
				 SHIFTJIS_CHARSET, SRCCOPY, BitBlt, StretchBlt, SetStretchBltMode, &
				 HALFTONE, RoundRect, MSFWIN$Rectangle
use kernel32, only: GetLastError, MulDiv, GlobalLock, GlobalUnlock, GlobalFree, &
                    GetProfileString
use user32, only: GetForegroundWindow
use winspool, only: OpenPrinter, ClosePrinter, DocumentProperties
use dfwinty, only: NULL, TRUE, FALSE, MAX_PATH, ERROR_NO_ERROR
use FReciverGlobals

implicit none

logical(4), intent(in), optional :: Default_Printer
  ! If .TRUE., system default printer is used and no
  ! print dialog box is displayed.  If .FALSE., a print
  ! dialog box is displayed to allow the user to select
  ! a printer and number of copies.  Default .TRUE.


INTEGER*4 , intent(in), optional :: Orientation
  ! Specifies what page orientation is desired.  The value
  ! may be one of the following:
  !    FWP_ORIENTATION_DEFAULT  - Uses printer default
  !    FWP_ORIENTATION_PORTRAIT - Forces portrait orientation
  !    FWP_ORIENTATION_LANDSCAPE - Forces landscape orientation
  ! If omitted, DEFAULT is used.  Not all printers may support
  ! this option. Ignored if Default_Printer is .FALSE.

! Return value:
!
!	If successful, a positive integer containing the print
!	job number
!   If the print dialog box was displayed and the user clicked
!	cancel, zero is returned,
!   If unsuccessful, a negative integer that is one of the
!	FWP_ERR_xxx codes defined in this module.
!

type(T_PRINTDLG) :: PRINTDLG_Struct
type(T_DOCINFO) :: DOCINFO_Struct
type(T_SIZE) :: SIZE_Struct
type(T_DEVMODE) :: DEVMODE_Struct
type(T_PRINTER_DEFAULTS) :: PrtrDef

integer hFont,hOldFont, hPrinter, hOldDC, out
integer hDC,xoffset,yoffset,page_width,page_height,pixels_per_inch_x,pixels_per_inch_y
integer line_height, current_x, current_y, initial_x, initial_y
integer max_y, line_width, font_height
pointer (p_DEVMODE_Struct, DEVMODE_Struct)

! Declare default values and local variables for optional arguments
logical(4), parameter :: DDefault_Printer = .FALSE.  ! Use default printer?
logical(4) :: WDefault_Printer
REAL*4, parameter :: Dhoriz_margin = 0.5   ! Left and right margins in inches
REAL*4 :: WHoriz_margin
REAL*4, parameter :: Dvert_margin = 0.5    ! Top and bottom margins in inches
REAL*4 :: WVert_Margin
REAL*4 stretch

integer nLen, max_width, max_height
character*1024 buffer
character*128 DefaultPrinterName
character*(MAX_PATH+1) document_name
INTEGER*4  column, i,j,ret
logical(4) top_of_page,Opened
character*10 formatted,sequential
INTEGER*4  IOS
integer lres

! Establish defaults for optional arguments
!
WDefault_Printer = DDefault_Printer
WHoriz_Margin = DHoriz_Margin
WVert_Margin = DVert_Margin

! Process optional arguments which don't have dependencies.
!
if (present(Default_Printer)) then
  WDefault_Printer = Default_Printer
  end if


! Initialize PRINTDLG_Struct
!
PRINTDLG_Struct%lStructSize = SIZEOF(PRINTDLG_Struct)
PRINTDLG_Struct%hwndOwner = GetForegroundWindow()
PRINTDLG_Struct%hDevMode = NULL
PRINTDLG_Struct%hDevNames = NULL
PRINTDLG_Struct%hDc = NULL
PRINTDLG_Struct%Flags = PD_ALLPAGES .OR. PD_RETURNDC .OR. PD_NOPAGENUMS &
   .OR. PD_NOSELECTION

if (WDefault_Printer) then
    PRINTDLG_Struct%Flags = IOR(PRINTDLG_Struct%Flags,PD_RETURNDEFAULT)
	end if

PRINTDLG_Struct%nFromPage = 1
PRINTDLG_Struct%nToPage = 1
PRINTDLG_Struct%nMinPage = 1
PRINTDLG_Struct%nMaxPage = 1
PRINTDLG_Struct%nCopies = 1
PRINTDLG_Struct%hInstance = NULL
PRINTDLG_Struct%lCustData = NULL
PRINTDLG_Struct%lpfnPrintHook = NULL
PRINTDLG_Struct%lpfnSetupHook = NULL
PRINTDLG_Struct%lpPrintTemplateName = NULL
PRINTDLG_Struct%lpSetupTemplateName = NULL
PRINTDLG_Struct%hPrintTemplate = NULL
PRINTDLG_Struct%hSetupTemplate = NULL

Last_Error = PrintDlg (PRINTDLG_Struct)
if (Last_Error == 0) then
  Last_Error = CommDlgExtendedError ()
  if (Last_Error == 0) then
	ret = 0
  else
    ret = FWP_ERR_PRINTDLGFAILED
	end if
  return
  end if

! See if a change in orientation was requested
!

if (wDefault_Printer .and. present(Orientation)) then

    ! Get Name Of Default Printer see MSKB Q135387
    nLen = GetProfileString("windows"C, "device"C, ",,,"C, DefaultPrinterName, &
	    sizeof(DefaultPrinterName))
    nLen = index(DefaultPrinterName(1:nLen), ",")
    DefaultPrinterName(nLen:nLen) = char(0)

    PrtrDef = T_PRINTER_DEFAULTS(NULL, NULL, PRINTER_ACCESS_USE)
    Last_Error = OpenPrinter(DefaultPrinterName, %LOC(hPrinter), PrtrDef)
    if (Last_Error == 0) then
        Last_Error = GetLastError()
		out = FWP_ERR_OPENPRINTERFAILED
		return
        end if

    ! get access to DevMode
    p_DEVMODE_struct = GlobalLock(PRINTDLG_Struct%hDevMode)
    ! modify the DevMode struct


	 if (IAND(DEVMODE_struct%dmFields, DM_ORIENTATION) == DM_ORIENTATION) then 
!             DEVMODE_struct%dmOrientation =  FWP_ORIENTATION_LANDSCAPE !Orientation
             DEVMODE_struct%field1%dmOrientation =  FWP_ORIENTATION_LANDSCAPE !Orientation
     end if


    ! set the devmode in case print driver stores info related to 
	! these modifications in the device dependant private area of 
	! the DevMode - MSKB Q167345
    Last_Error = DocumentProperties(NULL, hPrinter, DefaultPrinterName, &
		DEVMODE_struct, DEVMODE_struct,    &
        (IOR(DM_IN_BUFFER,DM_OUT_BUFFER))) ! IDOK = succcess
    
    ! update the hDC with the changes
    hOldDC = ResetDC(PRINTDLG_Struct%hDC, DEVMODE_struct)
    Last_Error = GlobalUnlock(PRINTDLG_Struct%hDevMode)
    if (Last_Error == FALSE) then
        Last_Error = GetLastError()
    end if

    Last_Error = ClosePrinter(hPrinter)
end if


	p_DEVMODE_struct = GlobalLock(PRINTDLG_Struct%hDevMode)
	 if (IAND(DEVMODE_struct%dmFields, DM_ORIENTATION) == DM_ORIENTATION) then
!             DEVMODE_struct%dmOrientation =  FWP_ORIENTATION_LANDSCAPE !Orientation
             DEVMODE_struct%field1%dmOrientation =  FWP_ORIENTATION_LANDSCAPE !Orientation
     end if





! Bring up the Print dialog box and allow user to select printer
!
hDC = PRINTDLG_Struct%hDC


! Get various metrics for this printer.  Width and
! height are of the printable area.
!
xoffset = GetDeviceCaps (hDC, PHYSICALOFFSETX)
yoffset = GetDeviceCaps (hDC, PHYSICALOFFSETY)
page_width = GetDeviceCaps (hDC, HORZRES)
page_height = GetDeviceCaps (hDC, VERTRES)
pixels_per_inch_x = GetDeviceCaps (hDC, LOGPIXELSX)
pixels_per_inch_y = GetDeviceCaps (hDC, LOGPIXELSY)


! Compute initial X and Y offset to account for the 
! requested margins.  If the requested margin
! is outside the printable area, the whole printable
! area is used.  Offsets are relative to the printable
! area.
!
initial_x = NINT(REAL(pixels_per_inch_x)*Whoriz_margin) - xoffset
if (initial_x > page_width) initial_x = 0 
initial_y = NINT(REAL(pixels_per_inch_y)*Wvert_margin) - yoffset
if (initial_y > page_height) initial_y = 0 

max_width = page_width - (2*initial_x)
max_height = page_height - (2*initial_y)

stretch = MIN(REAL(max_width)/REAL(img_w), REAL(max_height)/REAL(img_h))	


! Initialize DOCINFO_Struct
!
DOCINFO_Struct%cbSize = SIZEOF(DOCINFO_Struct)


Document_Name = "FReciver"
j = LEN_TRIM(Document_Name)+1
Document_Name(j:j) = CHAR(0)
DOCINFO_Struct%lpszDocName = LOC(Document_Name(1:))
DOCINFO_Struct%lpszOutput = NULL
DOCINFO_Struct%lpszDatatype = NULL
DOCINFO_Struct%fwType = 0

! Start the print job
!
out = StartDoc (hDC, DOCINFO_Struct)
if (Last_Error <= 0) then
  Last_Error = GetLastError()
  write (*,*) "StartDoc failed, code=",Last_Error
  goto 90000
  end if

! Start the first page
Last_Error = StartPage (hDC)


!call BitBlt(hDC, 0, 0, img_w, img_h, hMainDC, 0, 0, SRCCOPY)
lres = MSFWIN$Rectangle(hDC, initial_x-3, initial_y-3, INT(img_w*stretch+initial_x+3), INT(img_h*stretch+initial_y+3))
lres = MSFWIN$Rectangle(hDC, initial_x-1, initial_y-1, INT(img_w*stretch+initial_x+1), INT(img_h*stretch+initial_y+1))
lres = SetStretchBltMode(hDC, HALFTONE)
lres = StretchBlt (hDC, initial_x, initial_y, INT(img_w*stretch), INT(img_h*stretch), hMainDC, 0, 0, INT(img_w), INT(img_h), SRCCOPY)


! End the page
Last_Error = EndPage (hDC)

! End the document
Last_Error = EndDoc (hDC)

Last_Error = DeleteDC (hDC)

90000 continue
! Free the devmode and devname fields of the PRINTDLG structure
! necessary.
!
if (PRINTDLG_struct%hDevMode /= NULL) then
    Last_Error = GlobalFree (PRINTDLG_struct%hDevMode)
	end if
if (PRINTDLG_struct%hDevNames /= NULL) then
	Last_Error = GlobalFree (PRINTDLG_struct%hDevNames)
	end if

Print_Bitmap = Last_Error
return
end function Print_Bitmap

end module Fortran_WinPrint