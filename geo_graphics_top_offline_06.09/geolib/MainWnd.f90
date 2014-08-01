function InitMainWindow( hInstance, hPrevInstance, nCmdShow )
    use user32
    use kernel32
    use dfcom
    use dfauto
    use FReciverGlobals
	use GeolibGlobals
    USE PARAM_
    
	implicit none

    integer*4 hInstance
    integer*4 hPrevInstance
	integer*4 lpszCmdLine
    integer*4 nCmdShow
	integer*4 InitMainWindow

    include 'geolib.fi'

    ! Variables
    type (T_WNDCLASS)       wc
    type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret
    integer                 haccel

    character(SIZEOFAPPNAME) lpszClassName
    character(SIZEOFAPPNAME) lpszIconName
    character(SIZEOFAPPNAME) lpszAppName
    character(SIZEOFAPPNAME) lpszMenuName
    character(SIZEOFAPPNAME) lpszAccelName

	external ConsoleSub

! *************************************
! ���� ���������� ��� ������  ***********************************************
! ���� ��� ������ ����        ***********************************************
! CALL RINPUT()						! ���� ���������� ��� ������
! *************************************
    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL
    call COMINITIALIZE(ret)

    lpszClassName ="GeoLib"C
!    lpszAppName ="CFReciver"C
	lpszAppName ="GeoLib"C
    lpszIconName ="FReciver"C  
    lpszMenuName ="FReciver"C
    lpszAccelName ="FReciver"C

    !  If this is the first instance of the application, register the
    !  window class(es)
    if (hPrevInstance .eq. 0) then
        !  Main window
         wc%lpszClassName = LOC(lpszClassName)
         wc%lpfnWndProc = LOC(MainWndProc)
         ! | 512 ��������� � ����� ����� ������� ������ �������� ���� ����������. ����� ���� ���� ��������������� ������� WM_CLOSE
         ! wc%style = IOR(IOR(CS_VREDRAW, CS_HREDRAW), 512)
         wc%hInstance = hInstance
         wc%hIcon = LoadIcon( hInstance, LOC(lpszIconName))
         wc%hCursor = LoadCursor( NULL, IDC_ARROW )
         wc%hbrBackground = ( COLOR_WINDOW+1 )
         wc%lpszMenuName = NULL
         wc%cbClsExtra = 0
         wc%cbWndExtra = 0
         if (RegisterClass(wc) == 0) goto 99999
    end if

    ! Load the window's menu and accelerators and create the window
    !
    ghMenu = LoadMenu(hInstance, LOC(lpszMenuName))
    if (ghMenu == 0) goto 99999
    haccel = LoadAccelerators(hInstance, LOC(lpszAccelName))
    if (haccel == 0) goto 99999

    ghwndMain = CreateWindowEx(  0, lpszClassName,                 &
                                 lpszAppName,                      &
                                 INT(WS_OVERLAPPEDWINDOW),         &
                                 CW_USEDEFAULT,                    &
                                 0,                                &
                                 CW_USEDEFAULT,                    &
                                 0,                                &
                                 NULL,                             &
                                 ghMenu,                           &
                                 hInstance,                        &
                                 NULL                              &
                              )
    if (ghwndMain == 0) goto 99999

	mainHWND = ghwndMain

    lret = ShowWindow( ghwndMain, nCmdShow )


	!lret = dlginit(IDD_CONSOLE_PAGE,InfoDialog)
	lret = DLGINITWITHRESOURCEHANDLE(IDD_CONSOLE_PAGE, dllHandler, InfoDialog) 
	lret = DlgSetSub(InfoDialog, IDD_CONSOLE_PAGE, ConsoleSub)
! *************************************
! ������� �������             ***********************************************
! ��� ������ ��������� ������ ***********************************************
! ���� ��� ������ ����        ***********************************************
!	call showInfoDalog()      ! ���� ���������� ��� ������
! *************************************

   ! Read and process messages
!   do while( GetMessage (mesg, NULL, 0, 0) )
!     if ( DlgIsDlgMessage(mesg) .EQV. .FALSE. ) then
!       if ( TranslateAccelerator (mesg%hwnd, haccel, mesg) == 0) then
!          lret = TranslateMessage( mesg )
!          ret  = DispatchMessage( mesg )
!       end if
!     end if
!   end do 


    ! Read and process messsages
!    do while( GetMessage (mesg, NULL, 0, 0) ) 
!       if ( TranslateAccelerator (mesg%hwnd, haccel, mesg) == 0) then
!           lret = TranslateMessage( mesg )
!           ret  = DispatchMessage( mesg )
!       end if
!    end do

    call COMUNINITIALIZE()

    InitMainWindow = mesg.wParam
	
    return

99999 &

    ret = MessageBox(ghwndMain, "Error initializing application FReciver"C, &
                     "Error"C, MB_OK)
    call COMUNINITIALIZE()
    InitMainWindow = 0
end 




integer function MainWndProc ( hWnd, mesg, wParam, lParam )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_MainWndProc@16' :: MainWndProc
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'MainWndProc' :: MainWndProc
!DEC$ ENDIF
    USE DFWINTY
	use kernel32
    use gdi32 

    use user32
    use FReciverGlobals
    use GeolibGlobals
	USE PARAM_1
	USE RAILWAY
    use GRAFICA
    implicit none

    integer*4 hWnd
    integer*4 mesg
    integer*4 wParam
    integer*4 lParam
    integer*4 hdc

    include 'resource.fd'

    interface 
    integer*4 function  AboutDlgProc( hwnd, mesg, wParam, lParam )
    !DEC$ IF DEFINED(_X86_)
    !DEC$ ATTRIBUTES STDCALL, ALIAS : '_AboutDlgProc@16' :: AboutDlgProc
    !DEC$ ELSE
    !DEC$ ATTRIBUTES STDCALL, ALIAS : 'AboutDlgProc' :: AboutDlgProc
    !DEC$ ENDIF

    integer*4 mesg  
    integer*4 wParam
    integer*4 lParam 
	end function 


	integer function OnClose()
	end function 

    end interface

    ! Variables
    integer*4           ret, lret

	type ( T_PAINTSTRUCT ) ps
type (T_FILETIME)::FTS
type (T_SYSTEMTIME)::	STT


	type(T_COPYDATASTRUCT):: CDS; pointer(pCDS, CDS)
!	real:: MyData(*); pointer(pMyData, MyData)
	character(1):: MyData(*); pointer(pMyData, MyData)

	integer(2) km,met,mm
	integer iret, lres
	integer*4 xx, yy
!	REAL*4 MyTrassa(1536)

    character(SIZEOFAPPNAME)  lpszName, lpszHelpFileName, lpszContents, lpszMessage
    character(SIZEOFAPPNAME)  lpszHeader

    select case ( mesg )

    ! WM_DESTROY: PostQuitMessage() is called 
    case (WM_DESTROY)
          call PostQuitMessage( 0 )
          MainWndProc = 0
          return
 
 
    case (WM_PAINT)
!        if(N_PRIZNAK_GRAFICA.EQ.0) then
    	    hdc = BeginPaint(hwnd, ps)
            call repaint(hdc)
		    lres = EndPaint(hwnd, ps)
!        end if
        MainWndProc = 0
        return

    case (WM_CLOSE)											! ���������� �����
        ! ������� �������� ���� �� �������. ��� �������� ��������� ������� ������������ �������� ����.
        !  if( OnClose()==0 ) then
		!	call PostQuitMessage( 0 )
		!	lres = CloseWindow(hwnd)
		!  end if
          lret = ShowWindow( mainHWND, 0 )
          MainWndProc = 0
    return


    case (WM_LBUTTONDOWN)									! ������� ����
		xx = IAND(lParam, 16#0000ffff )
		yy = IAND(lParam, 16#ffff0000 )/16#10000
		call mouseClick(xx, yy)
		MainWndProc = 0
    return


    ! WM_COMMAND: user command
      case (WM_COMMAND)
        select case ( IAND(wParam, 16#ffff ) )
 
            case (IDM_EXIT)
                ret = SendMessage( hWnd, WM_CLOSE, 0, 0 )
                MainWndProc = 0
                return
  

            case (IDM_CONSOLE)
				call showInfoDalog()
				MainWndProc = 0
            return


            case (300)  !IDM_HELPCONTENTS
                lpszHelpFileName ="\\FReciver.hlp"C
                lpszContents = "CONTENTS"C
                if (WinHelp (hWnd, lpszHelpFileName, HELP_KEY, &
                               LOC(lpszContents)) .EQV. .FALSE.) then
                lpszMessage = "Unable to activate help"C
                lpszHeader = "FReciver"
                ret = MessageBox (hWnd,                             &
                                 lpszMessage,                       &
                                 lpszHeader,                        &
                                 IOR(MB_SYSTEMMODAL,                &
                                 IOR(MB_OK, MB_ICONHAND)))
                end if
                MainWndProc = 0
                return

            case (IDM_HELPSEARCH)
                lpszHelpFileName ="\\FReciver.hlp"C
                lpszContents = "CONTENTS"C
                if (WinHelp(hWnd, "FReciver.hlp"C,            &
                       HELP_PARTIALKEY, LOC(""C)) .EQV. .FALSE.) then
                   lpszMessage = "Unable to activate help"C
                   lpszHeader = "FReciver"C
                   ret = MessageBox (hWnd,                          &
                                 lpszMessage,                       &
                                 lpszHeader,                        &
                                 IOR(MB_SYSTEMMODAL ,               &
                                 IOR(MB_OK, MB_ICONHAND)))
                end if
                MainWndProc = 0
                return

            case (IDM_HELPHELP)
                if (WinHelp(hWnd, ""C, HELP_HELPONHELP, 0).EQV. .FALSE.)& 
                                                       then
                   lpszMessage = "Unable to activate help"C
                   lpszHeader = "FReciver"C
                   ret = MessageBox (GetFocus(),                    &
                                 lpszMessage,                       &
                                 lpszHeader,                        &
                                 IOR(MB_SYSTEMMODAL,IOR(MB_OK, MB_ICONHAND)))
                end if
                MainWndProc = 0
                return

            ! All of the other possible menu options are currently disabled

            case DEFAULT
                MainWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )
                return
        end select

    ! Let the default window proc handle all other messages
      case default
          MainWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )

    end select

end

!****************************************************************************
!
!  FUNCTION: CenterWindow (HWND, HWND)
!
!  PURPOSE:  Center one window over another
!
!  COMMENTS: Dialog boxes take on the screen position that they were designed
!            at, which is not always appropriate. Centering the dialog over a
!            particular window usually results in a better position.
!
!****************************************************************************

subroutine CenterWindow (hwndChild, hwndParent)

    use user32
    use gdi32 
    use FReciverGlobals

    implicit none

    integer         hwndChild, hwndParent

    include 'resource.fd'

    ! Variables
    type (T_RECT)   rChild, rParent
    integer         wChild, hChild, wParent, hParent
    integer         wScreen, hScreen, xNew, yNew
    integer         hdc
    integer*4       retval

    ! Get the Height and Width of the child window
       retval = GetWindowRect (hwndChild, rChild)
       wChild = rChild.right - rChild.left
       hChild = rChild.bottom - rChild.top

    ! Get the Height and Width of the parent window
       retval = GetWindowRect (hwndParent, rParent)
       wParent = rParent.right - rParent.left
       hParent = rParent.bottom - rParent.top

    ! Get the display limits
       hdc = GetDC (hwndChild)
       wScreen = GetDeviceCaps (hdc, HORZRES)
       hScreen = GetDeviceCaps (hdc, VERTRES)
       retval = ReleaseDC (hwndChild, hdc)

    ! Calculate new X position, then adjust for screen
       xNew = rParent.left + ((wParent - wChild) /2)
       if (xNew .LT. 0) then
          xNew = 0
       else if ((xNew+wChild) .GT. wScreen) then
          xNew = wScreen - wChild
       end if

    ! Calculate new Y position, then adjust for screen
       yNew = rParent.top  + ((hParent - hChild) /2)
       if (yNew .LT. 0) then
          yNew = 0
       else if ((yNew+hChild) .GT. hScreen) then
          yNew = hScreen - hChild
       end if

    ! Set it, and return
       retval = SetWindowPos (hwndChild, NULL, xNew, yNew, 0, 0,      &
                      IOR(SWP_NOSIZE , SWP_NOZORDER))
end  

 
!/****************************************************************************
!
!  FUNCTION: AboutDlgProc(HWND, UINT, WPARAM, LPARAM)
!
!  PURPOSE:  Processes messages for "About" dialog box
!
!  COMMENTS: Display version information from the version section of the
!            application resource.  Wait for user to click on "Ok" button,
!            then close the dialog box.
!
!****************************************************************************/

integer*4 function AboutDlgProc( hDlg, message, uParam, lParam )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_AboutDlgProc@16' :: AboutDlgProc
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'AboutDlgProc' :: AboutDlgProc
!DEC$ ENDIF

    use kernel32
    use user32
    use gdi32
    use dfwbase
    use version
    use FReciverGlobals

    implicit none

    integer     hDlg        ! window handle of the dialog box
    integer     message     ! type of message
    integer     uParam      ! message-specific information
    integer     lParam

    include 'resource.fd'

    ! Variables
    integer*4   hfontDlg
    save        hfontDlg

    integer     dwVerHnd
    integer     dwVerInfoSize
    integer     uVersionLen
    integer     bRetCode
    integer     i
    character*256   szFullPath
    character*256   szResult
    character*256   szGetName
    character*256   lpversion

    integer*4   lpstrVffInfo
    integer*4   hMem
    integer*4   ret

    select case (message)
      case (WM_INITDIALOG)   ! message: initialize dialog box
         ! Create a font to use
         hfontDlg = CreateFont(14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,& 
                        IOR(INT(VARIABLE_PITCH) , INT(FF_SWISS)), ""C)

         ! Center the dialog over the application window
         call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

         ! Get version information from the application
         ret = GetModuleFileName (INT(ghInstance), szFullPath,     &
                               len(szFullPath))
         dwVerInfoSize = GetFileVersionInfoSize(szFullPath,   &
                                       LOC(dwVerHnd))

         if (dwVerInfoSize .NE. 0) then
            ! If we were able to get the information, process it:
            hMem = GlobalAlloc(GMEM_MOVEABLE, INT(dwVerInfoSize))
            lpstrVffInfo  = GlobalLock(hMem)
            ret = GetFileVersionInfo (szFullPath, dwVerHnd, &
            dwVerInfoSize, lpstrVffInfo)

            ! Walk through the dialog items that we want to replace:
            do i = IDC_VER1, IDC_VER5
               ret = GetDlgItemText(hDlg, i, szResult,      &     
                             len(szResult))
                
               szGetName = "\\StringFileInfo\\040904E4\\"C               
               ret =lstrcat(szGetName,szResult)

               bRetCode  =  VersionQueryValue(lpstrVffInfo,       &
                                            LOC(szGetName),       &
                                            LOC(lpVersion),       &
                                            LOC(uVersionLen))

               if ( bRetCode .NE. 0 ) then
                  ! Replace dialog item text with version info
                  ret = lstrcpy(szResult,lpVersion)
                  ret = SetDlgItemText(hDlg, i,szResult)
                  ret = SendMessage (GetDlgItem (hDlg, i),   &
                                   WM_SETFONT, hfontDlg, TRUE)
               end if
            end do 

            ret = GlobalUnlock(hMem)
            ret = GlobalFree(hMem)
         end if 
         AboutDlgProc = 1
         return
      case (WM_COMMAND)                      ! message: received a command
         if ((IAND(uParam,16#ffff) .EQ. IDOK) & !OK Selected?
            .OR. (IAND(uParam,16#ffff) .EQ. IDCANCEL)) then ! Close command?
            ret = EndDialog(hDlg, TRUE)      ! Exit the dialog
            ret = DeleteObject (hfontDlg)
            AboutDlgProc = 1
            return
         end if
    end select  
    AboutDlgProc = 0 ! Didn't process the message
    return
end 


subroutine dataIncome(pData, formatVersion)
    USE DFWINTY
	use kernel32
    use gdi32 

    use user32
    use FReciverGlobals
	use GeolibGlobals
	USE PARAM_1
	USE RAILWAY
	USE GRAFICA
    USE N_
    implicit none

	integer pData, formatVersion
	character(1):: MyData(*); pointer(pMyData, MyData)

	integer*4 hdc

	real foo
	integer(2) km,met,mm
	integer iret, lres, lret
	integer*4 I,II,J,k,n1, mouse_xx, mouse_yy,II1/0/
!	REAL*4 MyTrassa(1536)
	INTEGER*4  tt1,tt2
	INTEGER*4  III, OUT_TRASSA1
	REAL*4 RAB
	character(200) text_data

	
	type (T_FILETIME)::FTS
	type (T_SYSTEMTIME)::	STT

	integer OnClose, checkAndSaveScreenshot

	pMyData = pData

		if(kluch.eq.0) then					! ���������� ������� ���� ���� ���
			hdc = GetDC (0)
			img_w = GetDeviceCaps (hdc, HORZRES)
			img_h = GetDeviceCaps (hdc, VERTRES)-70
			kluch=1
        end if
!        N = 512
        NN_CHEN_F=N*(N_CHEN_F-1)       ! �������� ��� 6- ���������� ��������� � 3-6 ������ 
        NN_CHEN=N*N_CHEN
									   ! N_PRIZNAK_ISHOD=0 ������� ���������� �������� ������ �� �������
        CALL INTEGRAL_INPUT(MyData)    ! ������� �������� ���������� 
									   ! ��������� ������� ������� � ��������� POINT_ARRAY
!----------------------------------------------------------------------
!��������� ������� ����� �� �������

rang1=kilometr*1000+metr
out_trassa=INT4(ABS(rang1-rang)/ISTEP_BETWEEN_TRASES/10+.1)

OUT_TRASSA1=out_trassa
IF(OUT_TRASSA1.GT.5000) THEN                                     !���� ������ ������ 500� - ���������� ����� �� ����
    CALL BUFFER_NA_DISK()
    out_trassa=1
ELSE
    if(out_trassa.lt.ISTEP_FOR_GRAFICA)out_trassa=1
    if(ABS(RANG2-KILOMETR).NE.0) THEN
    out_trassa=1
    ELSE
    !IF(METR.LT.RANG3)out_trassa=-out_trassa !��������� � ������ ���� �����
    END IF
END IF

rang=rang1
rang2=kilometr
RANG3=METR
!--------------------------------------
! ����� ������ � ��������� ������������
!----------------------------------------------------------------------------------------------------------
! N_PRINT  ��� ������ ����� �� ��������� � �������
! K_PRINT  ������� �������� ���� ������ ������������ �� �����
IF(K_PRINT.EQ.N_PRINT) THEN		! ��� ����������� N_PRINT � K_PRINT 
								! ����������� ���������,����� �� ����� 
								! � ������ � ��������� �����
	K_PRINT=1
		call NewTrace(n, MyTrassa)  ! ����� ������ � ��������� ������������
									! ������� ������� ������ ���������
		if (isFirst.gt.0) then
			hdc = getDC(0)
			call CreateBufBitmap(hdc) ! ���������
			lret = ReleaseDC(hdc, 0)
			isFirst = 0
		end if
	TRACE_POS = TRACE_POS + 1	
	DO N_CHEN_TEK=1,N_CHEN          ! ������� �������
        CALL ANALIZ()				! ������ ����������� ��������� 
!-----------------------------------------------------------------------------------------------------
! N_PRIZNAK_GRAFICA=0		!  ������� ������ �� �����
!-----------------------------------------------------------------------------------------------------
    call paintNext()        ! ���������
    END DO
	IF(N_PRIZNAK_GRAFICA.EQ.0)THEN
	call redrawScreen()	! ���������� ���� ��� ��� ���� �������  
 	lret = checkAndSaveScreenshot() ! ���������� ������ � ���� (��������� � SaveBitmap)
    else
 	lret = checkAndSaveScreenshot() ! ���������� ������ � ���� (��������� � SaveBitmap)        
    END IF
!-----------------------------------------------------------------------------------------------------------
! ���������� ������ ��� �������
!-----------------------------------------------------------------------------------------------------------
IF(IFL_WRITER_KASKAD.NE.0) THEN

DO N_CHEN_TEK=1,N_CHEN
    CALL MIDL_UGLUB()		! ��������� ���������� ���������� �� ������ ������
END DO
! ���������� �������� ����� � ��������� �����
IF(NP_PRINT.NE.0) THEN				! ��������, ����� �� ������
	
IF(KP_PRINT.EQ.NP_PRINT) THEN
	KP_PRINT=1
	DO N_CHEN_TEK=1,N_CHEN
		WRITE(STRING_CHEN,*) N_CHEN_TEK
		STRING_CHEN=adjustl(STRING_CHEN)
		CALL  PRINT_GRAF_CSV()      
		CALL  PRINT_VLAG_CSV()     
		CALL  PRINT_UGLUB_CSV()
        CALL  PRINT_TOLSH_CSV()
    END DO
! ������������ �� ���� ���������� ������ ��� ���������� �������
CALL BUFFER_NA_DISK()
	ELSE
		KP_PRINT=KP_PRINT+1
	END IF
END IF
END IF
!-----------------------------------------------------------------------------
ELSE
	K_PRINT=K_PRINT+1
	RETURN
END IF


	return
end subroutine dataIncome


!************************* 
! 
! �������, ����������� ������.
! size - ������ ������.
! trace_data - ������.
!
!*************************
subroutine NewTrace(size, trace_data)

    use FReciverGlobals
	USE PARAM_
	USE PARAM_1
    use GeolibGlobals
    use PARAM_1
    use N_
!	USE REZ_
!	USE ALL_

	integer (4) size, i,III
	real(4):: trace_data(*)
	real foo
	real*4, dimension(size) :: trace
	character*200 text

	NPT = size 
	TRACE_COUNT = TRACE_COUNT + N_PRINT

! *************************************
! ����� ��������� ������������  ***********************************************
! ���� ��� ������ ����          ***********************************************
! ������������ ����� ������ �� ����� n_chen

	CALL FIRST()
    
	do iii=1,N_CHEN
	do i = 1, size
	trace_buf((iii-1)*size+i) = trace_data((iii-1)*size+i) !����� ��� ���������
	end do
	end do
	continue
end 

! ----------------------------------------------------------------
! ������ � ������� (���������������� �������� �������) 
!	WRITE(text, *)
!	call ConsoleOut( text )
! ---------------------------------------------------------------
! *************************************

! ���������� ������ �� �������� ����           **********************************
! � ������ ����������  ! ���� ��� ������ ����    **********************************

integer function OnClose()
use FReciverGlobals
use Fortran_WinPrint
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY
use AviTools
implicit none
integer*4 lret, Rewrite_files

if (isFirst.eq.1) then
    ! ���� �� ����� ������ �� ���� ���������� - ������ �������
    return
end if

!-----------------------------------------------------------------
IF(IFL_WRITER.EQ.1) THEN		                    ! ������� ������������� ������ �� ���� �����������
CALL CALC_NAME()				                    ! ��������� ����� ���
call SaveBitmapToFile(hMainDC, hMainBitmap, NAME_FILE)
END IF

IF(IFLAG_PRINT.NE.0) THEN
lret = Print_Bitmap(.FALSE.)		                ! ������� ������������� ������
END IF
! ------------------------------------------------------------- 
IF(IFL_WRITER_KASKAD.EQ.1) THEN                     ! ������� �������������  ��������� �������
! ��������� �����

    DO N_CHEN_TEK=1,N_CHEN	
	WRITE(STRING_CHEN,*) N_CHEN_TEK
	STRING_CHEN=adjustl(STRING_CHEN)
    IF(N_CHEN.LT.3) CALL  ANALIZ_UGLUB()
    CALL  ANALIZ_TOLSH()
    END DO
    
    IF(N_CHEN.GE.3) CALL ANALIZ_UGLUB_DEFORM()    
    
    DO N_CHEN_TEK=1,N_CHEN	    
	CALL  PRINT_GRAF_FINISH()
	CALL  PRINT_VLAG_FINISH()
	CALL  PRINT_UGLUB_FINISH()
    CALL  PRINT_TOLSH_FINISH()
    END DO

! ���� INFO
    OPEN(UNIT=8,FILE=NAME_INFO,STATUS='REPLACE')    ! �������� ����� ������
	CALL INFO_XML()
	CLOSE(8)	                                    ! �������� ����� ������
END IF
!------------------------------------------------------------------------
!IF(FLAG_NAME_IMG.EQ.1.AND.FLAG_NAME_GEO.EQ.1)
lret = Rewrite_files()	! ���������� ������ ��� �������
!------------------------------------------------------------------------
! ��������� ������� ��������� 
IF(VIDEO_NAME.NE.'ZERO')THEN
        lret = RunProcess('cmd /c del /F /Q tmpframe1.bmp')
END IF
!-------------------------------------------------------------------------
OnClose = 0

end function
!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*)

