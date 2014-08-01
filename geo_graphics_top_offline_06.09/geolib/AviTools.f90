module AviTools

use DFWINTY
use msfwin

implicit none

private
integer buf
integer imageDC
integer hImage

integer hBufImage
integer hBufDC

integer		athInstance
integer		athWndMain

integer     hMenu

integer     hImageWindow

integer wasInitialized /0/
real*4 scale /1.0/

integer buf_img_w, buf_img_h

integer, PARAMETER :: WM_BW_10 = 9001
integer, PARAMETER :: WM_BW_1  = 9002
integer, PARAMETER :: WM_FW_1  = 9003
integer, PARAMETER :: WM_FW_10 = 9004
integer, PARAMETER :: WM_SAVE  = 9005

real currentSeconds
character(128) currentAviFile
character(300) current_name/' '/
public loadImageFromFile, showImageWindow, initAviTools, ShowImage, RunProcess, ShowFrame




character(256), public :: temp_dir
character(10) iwndClassName
character(1000) :: aviFileName
character(1000) :: currentAviFileName
integer, public :: hImgDlgWnd
type (T_WNDCLASS), public :: imgWndClass 
integer frameW, frameH


contains


integer function initAviTools ()
implicit none
integer hInstance
integer parentWindowHandler
integer desktopDC, res
	
	athInstance = 0
	athWndMain = 0

	iwndClassName ="ImgWndCls"C
    
    desktopDC = getDC(0)
    
    buf_img_w = GetDeviceCaps (desktopDC, HORZRES)
	buf_img_h = GetDeviceCaps (desktopDC, VERTRES)
    
	hBufImage = CreateCompatibleBitmap(desktopDC, buf_img_w, buf_img_h)	
	hBufDC = CreateCompatibleDC(desktopDC)
	res = SelectObject(hBufDC, hBufImage)
    
    res = releaseDC(0, desktopDC)
    
	initAviTools = 0
end function initAviTools


integer function ShowImage (fileName)
implicit none
	character(*), intent(in) :: fileName
	type (T_BITMAP) :: bmpInfo
	integer hh, res
    
    if (wasInitialized.eq.0) then
        res = initAviTools()  
        wasInitialized = 1
    end if
    
	
	imageDC = 0
	hImage = loadImageFromFile (fileName)

	hh = hImage
	
	res = GetObject(hImage, sizeof(bmpInfo), LOC(bmpInfo))

	frameW = bmpInfo%bmWidth
	frameH = bmpInfo%bmHeight

	res = showImageWindow()

	ShowImage = frameW*frameH

end function


integer(4) function loadImageFromFile (fileName)
implicit none
	character(*), intent(in) :: fileName
	aviFileName = fileName//char(0)	 
	loadImageFromFile = LoadImage(0, fileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE)
end function

integer(4) function showImageWindow ()
implicit none

    integer :: res

	if (hImgDlgWnd.eq.0) then
		res = initImageWindow()	
    	call ResizeWindow(hImgDlgWnd, frameW+30, frameH+50)
	endif

	
!	res = ShowWindow(hImgDlgWnd, SW_SHOW) 
	res = ShowWindow(hImgDlgWnd, SW_SHOWNORMAL) 

	showImageWindow = hImgDlgWnd

end function showImageWindow


integer(4) function initImageWindow ()
implicit none
    
	imgWndClass%lpszClassName = LOC(iwndClassName)
    imgWndClass%lpfnWndProc = LOC(ImageWndProc)
    imgWndClass%style = IOR(CS_VREDRAW , CS_HREDRAW)
    imgWndClass%hInstance = athInstance
    imgWndClass%hIcon = LoadIcon(NULL, IDI_APPLICATION)
    imgWndClass%hCursor = LoadCursor( NULL, IDC_ARROW )
    imgWndClass%hbrBackground = ( COLOR_WINDOW+1 )
    imgWndClass%lpszMenuName = NULL
    imgWndClass%cbClsExtra = 0
    imgWndClass%cbWndExtra = 0
    
    hMenu = CreateMenu();
    buf = AppendMenu(hMenu, MF_STRING, WM_BW_10, "<<10sec");
    buf = AppendMenu(hMenu, MF_STRING, WM_BW_1, "<1sec");
    buf = AppendMenu(hMenu, MF_STRING, WM_FW_1, ">1sec");
    buf = AppendMenu(hMenu, MF_STRING, WM_FW_10, ">>10sec");
    buf = AppendMenu(hMenu, MF_STRING, WM_SAVE, "Save");
	buf = athInstance
    
	buf = RegisterClass(imgWndClass)

    hImgDlgWnd = CreateWindowEx(	0,								& 
								iwndClassName,                  &
                                 "AppName"C,                    &
                                 INT(WS_OVERLAPPEDWINDOW),      &
                                 200,                           &
                                 10,                            &
                                 500,                           &
                                 100,                           &
                                 NULL,                          &
                                 NULL,                          &
                                 athInstance,                   &
                                 NULL                           &
                              )
    
    buf = SetMenu(hImgDlgWnd, hMenu);


	initImageWindow = hImgDlgWnd

end function initImageWindow


subroutine ResizeWindow(hWnd, nWidth, nHeight)
	implicit none
	integer :: hWnd, nWidth, nHeight, res 
    type (T_RECT) :: rcClient, rcWindow
	type (T_POINT) :: ptDiff

	res = GetClientRect(hWnd, rcClient)
	res = GetWindowRect(hWnd, rcWindow)
	ptDiff%x = (rcWindow%right - rcWindow%left) - rcClient%right
	ptDiff%y = (rcWindow%bottom - rcWindow%top) - rcClient%bottom
	res = MoveWindow(hWnd, rcWindow%left, rcWindow%top, nWidth + ptDiff%x, nHeight + ptDiff%y, TRUE);

end subroutine ResizeWindow


subroutine doPaint(hDC)
	implicit none
	integer hDC, res, hBgBrush, prevBrush
    
    type (T_RECT) :: theRect
    
    theRect%left = 10
    theRect%right = 10
    theRect%top = 200 !buf_img_w
    theRect%bottom = 200 !buf_img_h
    
    hBgBrush = createSolidBrush(RGB(100, 200, 50))
    
    !prevBrush = selectObject(hBufDC, hBgBrush)
    
!    res = FillRect(hBufDC, theRect, hBgBrush)
    
	if (imageDC.eq.0) then
		imageDC = CreateCompatibleDC(hDC)

		if (hImage.ne.0) then
			res = SelectObject(imageDC, hImage)
		endif
	endif    
    
    res = MSFWIN$Rectangle(hBufDC, -1, -1, buf_img_w+1, buf_img_h+1)
    
    res = SetStretchBltMode(hBufDC, STRETCH_HALFTONE)
    res = StretchBlt(hBufDC, 10, 30, INT(scale*frameW), INT(scale*frameH), imageDC, 0, 0, INT(frameW), INT(frameH), SRCCOPY)   
    res = BitBlt(hDC, 0, 0, buf_img_w, buf_img_w, hBufDC, 0, 0, SRCCOPY);
    res = InvertRect(hDC, theRect)
    


end subroutine doPaint


integer function onPaint(hWnd)
	implicit none
	integer hWnd, hDC, oldDC, res

	type (T_PAINTSTRUCT) :: paintStruct

	hDC = BeginPaint(hWnd, paintStruct)
    call doPaint(hdc)
	res = EndPaint(hWnd, paintStruct)

	onPaint = 1
end function onPaint

integer function redraw(hWnd)
	implicit none
	integer hWnd, hDC, oldDC, res

	hDC = getDC(hWnd)
    call doPaint(hdc)
	res = releaseDC(hWnd, hDC)

	redraw = 1
end function redraw


integer function ImageWndProc ( hWnd, mesg, wParam, lParam )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_ImageWndProc@16' :: ImageWndProc
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'ImageWndProc' :: ImageWndProc
!DEC$ ENDIF

    use user32
    use RAILWAY
    use FReciverGlobals
    implicit none

    integer*4 hWnd
    integer*4 mesg
    integer*4 wParam
    integer*4 lParam

    integer*4           ret, res, xx, yy, hh, ll, command
    
    character(500) name_file

    select case ( mesg )
    case (WM_CLOSE)  
        res = RunProcess('cmd /c del /F /Q tmpframe1.bmp')
        res = ShowWindow(hImgDlgWnd, SW_HIDE) 
        ImageWndProc = 0
        return
          
          
	  case (WM_PAINT)
		  res = onPaint(hWnd)
          ImageWndProc = 0
          return
          
      case (WM_MOUSEWHEEL)
            ll = IAND(wParam, 16#0000ffff )
		    hh = IAND(wParam, 16#ffff0000 )/16#10000
            xx = IAND(lParam, 16#0000ffff )
		    yy = IAND(lParam, 16#ffff0000 )/16#10000

                
            if (hh.gt.0) then
                scale = scale * 1.2
            else 
                scale = scale / 1.2
            end if
            
		    res = redraw(hWnd)
 
            ImageWndProc = 0
            return          

      case (WM_COMMAND)
            command = IAND(wParam, 16#ffff)
            select case ( command )

                case (WM_BW_10)
                    res = ShowFrame(currentAviFile, currentSeconds-10,kilometr_mouse,metr_mouse)
                    ImageWndProc = 0
                    return
                
                case (WM_BW_1)
                    res = ShowFrame(currentAviFile, currentSeconds-1,kilometr_mouse,metr_mouse)
                    ImageWndProc = 0
                    return 
                    
                case (WM_FW_1)
                    res = ShowFrame(currentAviFile, currentSeconds+1,kilometr_mouse,metr_mouse)
                    ImageWndProc = 0
                    return                      
                    
                case (WM_FW_10)
                    res = ShowFrame(currentAviFile, currentSeconds+10,kilometr_mouse,metr_mouse)
                    ImageWndProc = 0
                    return  
                    
                case (WM_SAVE)
!                    call GetFileName(0, hWnd, name_file, res)
                        IF(FOTO_NAME.NE.'ZERO')res = copyCurrentFrame(FOTO_NAME//char(0),current_name)
                    ImageWndProc = 0
                    return                      
                    
            end select

    ! Let the default window proc handle all other messages
      case default
          ImageWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )

    end select

end function ImageWndProc



integer function ShowFrame(aviFile, seconds,kilometr_mouse,metr_mouse)
	implicit none

	character(*) aviFile
	character(300) commandLine, commandLine2
	character(300) ffmpegProg
	real seconds
	integer ret
    real*4 kilometr_mouse,metr_mouse    
    integer*4 ikilometr_mouse,imetr_mouse,iseconds
    character(10) sr1,sr2,sr3
    
    currentSeconds = seconds
    currentAviFile = aviFile
	ffmpegProg = "ffmpeg"
    ikilometr_mouse=kilometr_mouse
    imetr_mouse=metr_mouse
    iseconds=seconds
    write(sr1,'(i10)')iseconds
    write(sr2,'(i10)')ikilometr_mouse
    write(sr3,'(i10)')imetr_mouse
     
    write(Current_Name,'(a,a,a,a,a,a)')trim(adjustl(sr1)),"sec_",trim(adjustl(sr2)),'km_',trim(adjustl(sr3)),'m.bmp'
  	
	WRITE (commandLine, '(a,a,f8.3,a,a,a)') TRIM(ffmpegProg), " -ss ", seconds, " -i ", TRIM(aviFile), " -y -an -sameq -t 0.001 -f image2 -r 1 tmpframe%01d.bmp"
	commandLine2 = TRIM(commandLine) // char(0)
	ret = RunProcess(commandLine2)

	ret = ShowImage("tmpframe1.bmp")

	ret = RunProcess('cmd /c del /F /Q tmpframe2.bmp')
	ret = RunProcess('cmd /c del /F /Q tmpframe3.bmp')

    ret = redraw(hImgDlgWnd)
    
	ShowFrame = 0
    
	ShowFrame = 1

end function ShowFrame

integer function copyCurrentFrame(path,Current_name)
	implicit none

	character*100 path
	character(300) commandLine, commandLine2
	character(300) ffmpegProg
	character(300) Current_name
	integer ret

	WRITE (commandLine, '(a,a,a)') 'cmd /c copy /B tmpframe1.bmp "', TRIM(adjustl(path))//trim(adjustl(Current_name)), '" /Y'
	commandLine2 = TRIM(commandLine) // char(0)
 
	ret = RunProcess(commandLine2)
    
	copyCurrentFrame = ret

end function copyCurrentFrame

integer function RunProcess ( commandLine )
   use dflib
   use dfwin
   use dfwinty

	implicit none

	character commandLine

	integer         :: iRet           !Main return code

	integer                      :: iWRC           !Return code for WaitForSingleObject
	integer                      :: iCRC           !Return code for CreateProcess

	type (T_StartupInfo)         :: StartInfo      !CreatProcess parms
	type (T_Process_Information) :: ProcInfo       !CreatProcess parms (created process info)


!
! Initialize return code
!
   iRet = 0
!
! Insure console window is suppressed
!
   StartInfo%cb               = 68
   StartInfo%lpReserved       = NULL
   StartInfo%lpDesktop        = NULL
   StartInfo%lpTitle          = NULL
   StartInfo%dwX              = 0
   StartInfo%dwY              = 0
   StartInfo%dwXSize          = 0
   StartInfo%dwYSize          = 0
   StartInfo%dwXCountChars    = 0
   StartInfo%dwYCountChars    = 0
   StartInfo%dwFillAttribute  = 0
   StartInfo%dwFlags          = StartF_UseShowWindow
   StartInfo%wShowWindow      = SW_HIDE
   StartInfo%cbReserved2      = 0
   StartInfo%lpReserved2      = NULL


   RunProcess = CreateProcess(null_character, &
          commandLine, &
          null_Security_Attributes, &
          null_Security_Attributes, &
          .false., &
          Null, &
          Null, &
          Null_Character, &
          StartInfo, &
          ProcInfo)

	iRet = WaitForSingleObject(ProcInfo%hProcess,10000)

end function RunProcess

subroutine OSCall(iWaitMS,Command,Args)

   use dflib
   use dfwin
   use dfwinty

   implicit none

   character(*), intent(in)     :: Command        !Command portion of the command line (i.e. the program name)
   character(*), intent(in)     :: Args           !Argument portion of the command line

   character(256)               :: CmdLine        !Work area for the command line

   integer, intent(in)          :: iWaitMS        !Process completion wait value in milliseconds
!   integer, intent(out)         :: iRet           !Main return code
   integer         :: iRet           !Main return code

   integer                      :: iWRC           !Return code for WaitForSingleObject
   integer                      :: iCRC           !Return code for CreateProcess

   type (T_StartupInfo)         :: StartInfo      !CreatProcess parms
   type (T_Process_Information) :: ProcInfo       !CreatProcess parms (created process info)
!
! Initialize return code
!
   iRet = 0
!
! Insure console window is suppressed
!
   StartInfo%cb               = 68
   StartInfo%lpReserved       = NULL
   StartInfo%lpDesktop        = NULL
   StartInfo%lpTitle          = NULL
   StartInfo%dwX              = 0
   StartInfo%dwY              = 0
   StartInfo%dwXSize          = 0
   StartInfo%dwYSize          = 0
   StartInfo%dwXCountChars    = 0
   StartInfo%dwYCountChars    = 0
   StartInfo%dwFillAttribute  = 0
   StartInfo%dwFlags          = StartF_UseShowWindow
   StartInfo%wShowWindow      = SW_HIDE
   StartInfo%cbReserved2      = 0
   StartInfo%lpReserved2      = NULL
!
! Prepare the command line string and arguments
!
   cmdLine = '"' // trim(command) // '" ' // args // char(0)
!
! Initiate process
!

!   iCRC = CreateProcess(null_character, &
!          cmdLine, &
!          null_Security_Attributes, &
!          null_Security_Attributes, &
!          .false., &
!          Null, &
!          Null, &
!          Null_Character, &
!          StartInfo, &
!          ProcInfo)


   iCRC = CreateProcess(null_character, &
          "notepad", &
          null_Security_Attributes, &
          null_Security_Attributes, &
          .false., &
          Null, &
          Null, &
          Null_Character, &
          StartInfo, &
          ProcInfo)
!
! Check return code from CreateProcess
!
   if (iCRC .eq. 0) then !Nonzero means success (i.e. the process id)
      iRet = -1
      return
   end if
!
! If user specified to wait
!
   if (iWaitMS .ne. 0) then

      iWRC = WaitForSingleObject(ProcInfo%hProcess,iWaitMS) !Wait for completion
      if (iWRC .eq. Wait_Failed) iRet = 4    !Wait failed
      if (iWRC .eq. Wait_Abandoned) iRet = 3 !Timeout abandoned
      if (iWRC .eq. Wait_Timeout) iRet = 2   !Timeout occurred
      if (iWRC .eq. Wait_Object_0) iRet = 1  !Normal termination (signaled)

   end if

   return

end subroutine OSCall



end module AviTools
    
!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*) N
