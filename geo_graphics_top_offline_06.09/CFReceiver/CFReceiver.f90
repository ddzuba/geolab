!  CFReceiver.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   MainWndProc()  - processes messages for the main window
!   CenterWindow() - centers one window over another
!   AboutDlgProc() - processes messages for the about box
!

!****************************************************************************
!
!  FUNCTION: WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!
!  PURPOSE:  Entry point for the application
!
!  COMMENTS: Displays the main window and processes the message loop
!
!****************************************************************************

integer*4 function WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_WinMain@16' :: WinMain
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'WinMain' :: WinMain
!DEC$ ENDIF

    use user32
    use kernel32
    use CFReceiverGlobals
    use gl_graphics
    use gl_core
    use INPUT_RADAROGRAMMA
    
    implicit none

    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow

    include 'CFReceiver.fi'

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
    
    integer setMiscGeoData
    
    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL

    lpszClassName ="CFReceiver"C
    lpszAppName ="CFReceiver"C
    lpszIconName ="CFReceiver"C  
    lpszMenuName ="CFReceiver"C
    lpszAccelName ="CFReceiver"C

    !  If this is the first instance of the application, register the
    !  window class(es)
    if (hPrevInstance .eq. 0) then
        !  Main window
         wc%lpszClassName = LOC(lpszClassName)
         wc%lpfnWndProc = LOC(MainWndProc)
         wc%style = IOR(CS_VREDRAW , CS_HREDRAW)
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

	lret = SetWindowPos(		&
			ghwndMain,			&
			HWND_TOP,			&
			50,					&
			50,					&
			300,				&
			300,				&
			0					&
			)
    
    allocate(mainGeoData)
    allocate(mainGraphContext)
    pMainD => mainGeoData
    pMainGC => mainGraphContext
    mainGraphContext%chanImgW = 500
    
    call AssignDataPointersToGeoscanGlobals()
    
    call init_graphics(ghwndMain, pMainD, pMainGC)
    lret = setMiscGeoData(pMainD)
    
    lret = ShowWindow( ghwndMain, nCmdShow )
    
    ! Read and process messsages
    do while( GetMessage (mesg, NULL, 0, 0) ) 
       if ( TranslateAccelerator (mesg%hwnd, haccel, mesg) == 0) then
           lret = TranslateMessage( mesg )
           ret  = DispatchMessage( mesg )
       end if
    end do

    WinMain = mesg.wParam
    return

99999 &

    ret = MessageBox(ghwndMain, "Error initializing application CFReceiver"C, &
                     "Error"C, MB_OK)
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: MainWndProc ( hWnd, mesg, wParam, lParam )
!
!  PURPOSE:  Processes messages for the main window
!
!  COMMENTS:
!
!****************************************************************************

integer function MainWndProc ( hWnd, mesg, wParam, lParam )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_MainWndProc@16' :: MainWndProc
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'MainWndProc' :: MainWndProc
!DEC$ ENDIF

    use user32
    use kernel32
    use CFReceiverGlobals
    use GEOSKANGLOBALS
    use INPUT_RADAROGRAMMA
    use KOORDINATA
   USE GLOBALS_LOG
    implicit none
    real*8 time_video
    integer*4 hWnd
    integer*4 mesg
    integer*4 wParam
    integer*4 lParam
    integer*4 i
    include 'resource.fd'

    ! Variables
    integer*4           ret, showGraphicsWindow
    character(SIZEOFAPPNAME)  lpszName, lpszHelpFileName, lpszContents, lpszMessage
    character(SIZEOFAPPNAME)  lpszHeader

	type(T_COPYDATASTRUCT):: CDS; pointer(pCDS, CDS)
	character(1):: MyData(*); pointer(pMyData, MyData)
	integer coord, menuState, bu, lret
!	allocate mif
INTEGER*4 hInst
INTEGER*4 IRESULTAT     ! œ¿–¿Ã≈“– Œÿ»¡ » œ–» ¬€œŒÀÕ≈Õ»» œ–Œ÷≈ƒ”–€ GEOSKANGLOBALS=0


      integer*4 hThread ! Œ·˙ˇ‚ÎÂÌËË ÙÛÌÍˆËË, ÍÓÚÓÛ˛ ÒÓ·Ë‡ÂÏÒˇ ‚˚Á˚‚‡Ú¸ ‚ Ô‡‡ÎÎÂÎ¸ÌÓÏ ÔÓÚÓÍÂ.
      interface
      integer*4 function Only_File()
      !DEC$ ATTRIBUTES STDCALL :: Only_File
      end function
      
      integer*4 function Koordinata_File()
      !DEC$ ATTRIBUTES STDCALL :: Koordinata_File
      end function      
      
      integer*4 function Rw_Koordinata_File()
      !DEC$ ATTRIBUTES STDCALL :: Rw_Koordinata_File
      end function
      end interface
      interface
      
      integer*4 function Redactor_Log( )
      !DEC$ ATTRIBUTES STDCALL :: Redactor_Log
!      integer*4 IRESULTAT
      end function 
      
      integer*4 function Redactor_Txt( )
      !DEC$ ATTRIBUTES STDCALL :: Redactor_Txt
!      integer*4 IRESULTAT
      end function 
      
      integer*4 function RunProcess1(commandLine,kluch)
      character(200) commandLine  
      integer*4 kluch
      end function
      end interface     

character (200) commandLine      
CHARACTER(500) FILT
    select case ( mesg )

    ! WM_DESTROY: PostQuitMessage() is called 
      case (WM_DESTROY)
          call PostQuitMessage( 0 )
          MainWndProc = 0
          return

      case (WM_COPYDATA)
 		  pCDS = lParam   !CDS now points to (COPYDATASTRUCT*)lParam
          coord = CDS%dwData
		  pMyData = CDS%lpData
		  call SendTraceRealTime(pMyData, coord) 
          MainWndProc = 0
          return

    ! WM_COMMAND: user command
      case (WM_COMMAND)
        select case ( IAND(wParam, 16#ffff ) )
 
            case (IDM_EXIT)
                if(flag_ini.ne.0.or.flag_cfg.ne.0)then
                MESS=MESSAGEBOX(NULL,"ƒ‡ - ÒÓı‡ÌˇÂÏ ÓÚÂ‰‡ÍÚËÓ‚‡ÌÌ˚Â, ÕÂÚ - ÒÓı‡ÌˇÂÏ ËÒıÓ‰Ì˚Â"C,"—Óı‡ÌÂÌËÂ Ù‡ÈÎÓ‚ .ini Ë .cfg"C,4)    
	            IF(MESS.EQ.6) THEN                                                                             
                    ! ¬¬Œƒ»Ã œ”“‹   —Œ’–¿Õ≈ÕÕŒÃ” ‘¿…À”
                    FF1='Cfreceiver.cfg'
                    FF2='OLD_PATHWAY_INI'
                    FF3='OLD_CFRECEIVER.INI'
                    CALL Ini_Read_Str(FF1,FF2,FF3)  
                    OLD_PATHWAY_INI=FF2
                    commandLine=' '
                    WRITE (commandLine,'(a,a,a,a,a)') TRIM('cmd /c copy /y '),' ',TRIM(PATHWAY_INI),' ',TRIM(OLD_PATHWAY_INI)
                    commandLine=TRIM(commandLine)//char(0)
                    ret = RunProcess1(commandLine,1) 
                    ! ¬¬Œƒ»Ã œ”“‹   —Œ’–¿Õ≈ÕÕŒÃ” ‘¿…À”
                    FF1='Cfreceiver.cfg'
                    FF2='OLD_PATHWAY_CFG'
                    FF3='OLD_CFRECEIVER.CFG'
                    CALL Ini_Read_Str(FF1,FF2,FF3)  
                    OLD_PATHWAY_CFG=FF2
                    commandLine=' '
                    WRITE (commandLine,'(a,a,a,a,a)') TRIM('cmd /c copy /y '),' ',TRIM(PATHWAY_CFG),' ',TRIM(OLD_PATHWAY_CFG)
                    commandLine=TRIM(commandLine)//char(0)
                    ret = RunProcess1(commandLine,1)                    
                END IF
                end if
                ret = SendMessage( hWnd, WM_CLOSE, 0, 0 )
                MainWndProc = 0
                return

            case (IDM_START)
                flag_file=0                                     !  Àﬁ◊ ƒÀﬂ FINISH
                CALL Start()
                MainWndProc = 0
                return

            case (ID_Redactor_SEND1)
                CALL SendSampleTrace(1)
                MainWndProc = 0
                return

            case (ID_Redactor_SEND100)
                CALL SendSampleTrace(100)
                MainWndProc = 0
                return
                
            case (ID_INSRUCTION)  
                commandLine='notepad.exe Instruction.txt'//char(0)
                ret= RunProcess1(commandLine,0)               
                MainWndProc = 0
                return               
            case (ID_Redactor_Log) 
               hThread = CreateThread(0,0,IADDR(Redactor_Log),0,0,0)
                IF(hThread.EQ.0) THEN
                RETURN
                END IF
                MainWndProc = 0
                return 
            
            case (ID_Redactor_Txt) 
               hThread = CreateThread(0,0,IADDR(Redactor_Txt),0,0,0)
                IF(hThread.EQ.0) THEN
                RETURN
                END IF
               MainWndProc = 0
                return  
                
            case (ID_Create_Txt)
                ! ¬¬Œƒ»Ã œ”“‹   –≈ƒ¿ “Œ–”
                FF1='Cfreceiver.cfg'
                FF2='GPS_TRAK'
                FF3='RouteConverterWindows.exe'
                CALL Ini_Read_Str(FF1,FF2,FF3)  
                GPS_TRAK=FF2
                commandLine=GPS_TRAK//char(0)
                ret= RunProcess1(commandLine,1)
                MainWndProc = 0
                return 
 
            case (ID_Edit_Ini)
                FLAG_INI=1
                ! ¬¬Œƒ»Ã œ”“‹   –≈ƒ¿ “Œ–”
                FF1='Cfreceiver.cfg'
                FF2='PATHWAY_INI'
                FF3='GEOLIB.INI'
                CALL Ini_Read_Str(FF1,FF2,FF3)  
                PATHWAY_INI=FF2
                commandLine='notepad.exe '//trim(PATHWAY_INI)//char(0)
                ret= RunProcess1(commandLine,0)
                MainWndProc = 0
                return
                
            case (ID_Edit_Cfg)
                FLAG_CFG=1
                ! ¬¬Œƒ»Ã œ”“‹   –≈ƒ¿ “Œ–”
                FF1='Cfreceiver.cfg'
                FF2='PATHWAY_CFG'
                FF3='CFRECEIVER.CFG'
                CALL Ini_Read_Str(FF1,FF2,FF3)  
                PATHWAY_CFG=FF2
                commandLine='notepad.exe '//trim(PATHWAY_CFG)//char(0)
                ret= RunProcess1(commandLine,0)
                MainWndProc = 0
                return
                
            case (ID_Reset_Ini)
                IF(FLAG_Ini.EQ.0) THEN
                MainWndProc = 0
                returN
                END IF
                ! ¬¬Œƒ»Ã œ”“‹   —Œ’–¿Õ≈ÕÕŒÃ” ‘¿…À”
                FF1='Cfreceiver.cfg'
                FF2='OLD_PATHWAY_INI'
                FF3='OLD_CFRECEIVER.INI'
                CALL Ini_Read_Str(FF1,FF2,FF3)  
                OLD_PATHWAY_INI=FF2
                commandLine=' '
                WRITE (commandLine,'(a,a,a,a,a)') TRIM('cmd /c copy /y '),' ',TRIM(OLD_PATHWAY_INI),' ',TRIM(PATHWAY_INI)
                commandLine=TRIM(commandLine)//char(0)
                ret = RunProcess1(commandLine,1)
                MainWndProc = 0
                return
                
            case (ID_Reset_Cfg)
                IF(FLAG_Cfg.EQ.0) THEN
                MainWndProc = 0
                return
                END IF
                ! ¬¬Œƒ»Ã œ”“‹   —Œ’–¿Õ≈ÕÕŒÃ” ‘¿…À”
                FF1='Cfreceiver.cfg'
                FF2='OLD_PATHWAY_CFG'
                FF3='OLD_CFRECEIVER.CFG'
                CALL Ini_Read_Str(FF1,FF2,FF3)  
                OLD_PATHWAY_CFG=FF2
                commandLine=' '
                WRITE (commandLine,'(a,a,a,a,a)') TRIM('cmd /c copy /y '),' ',TRIM(OLD_PATHWAY_CFG),' ',TRIM(PATHWAY_CFG)
                commandLine=TRIM(commandLine)//char(0)
                ret = RunProcess1(commandLine,1)
                MainWndProc = 0
                return
                                
            case (ID_GeoScan32_Mode1)   ! Œ¡–¿¡Œ“ ¿ ‘¿…À¿
                hThread = CreateThread(0,0,IADDR(Only_File),0,0,0)
                IF(hThread.EQ.0) THEN
                ret=MESSAGEBOX(NULL,"œÂ‚‡Ú¸ ‡·ÓÚÛ "C,"Œ¯Ë·Í‡ ÙÓÏËÓ‚‡ÌËˇ ÔÓÒ˚ÎÍË ‚ ·Ë·ÎËÓÚÂÍÛ"C,4)
                ret = SendMessage( hWnd, WM_CLOSE, 0, 0 )        ! «‡Í˚‚‡ÂÏ ‚˚ÔÓÎÌÂÌËÂ, ÂÒÎË ‚˚¯ÎË Ò Ó¯Ë·ÍÓÈ !  ÓÚÒÚ‡ÌÚ‡ IRESULTAT ‡ÁÏÂ˘ÂÌ‡ ‚ ÏÓ‰ÛÎÂ  GEOSKANGLOBALS
                RETURN
                END IF
                MainWndProc = 0
                return
                
            case (ID_GeoScan32_Mode2)   ! Œ¡–¿¡Œ“ ¿ ‘¿…À¿ » œ–»¬ﬂ« ¿ –≈«”À‹“¿“Œ¬    ŒŒ–ƒ»Õ¿“¿Ã
                hThread = CreateThread(0,0,IADDR(Koordinata_File),0,0,0)
                IF(hThread.EQ.0) THEN
                ret = SendMessage( hWnd, WM_CLOSE, 0, 0 )        ! «‡Í˚‚‡ÂÏ ‚˚ÔÓÎÌÂÌËÂ, ÂÒÎË ‚˚¯ÎË Ò Ó¯Ë·ÍÓÈ !  ÓÚÒÚ‡ÌÚ‡ IRESULTAT ‡ÁÏÂ˘ÂÌ‡ ‚ ÏÓ‰ÛÎÂ  GEOSKANGLOBALS
                RETURN
                END IF
                MainWndProc = 0
                return
                
            case (ID_GeoScan32_Mode3)
                hThread = CreateThread(0,0,IADDR(Rw_Koordinata_File),0,0,0)
                IF(hThread.EQ.0) THEN
                ret = SendMessage( hWnd, WM_CLOSE, 0, 0 )        ! «‡Í˚‚‡ÂÏ ‚˚ÔÓÎÌÂÌËÂ, ÂÒÎË ‚˚¯ÎË Ò Ó¯Ë·ÍÓÈ !  ÓÚÒÚ‡ÌÚ‡ IRESULTAT ‡ÁÏÂ˘ÂÌ‡ ‚ ÏÓ‰ÛÎÂ  GEOSKANGLOBALS
                RETURN
                END IF
                MainWndProc = 0
                return
                
            case (ID_Stop)
                flag_file=1
                MainWndProc = 0
                return
                
            case (IDM_FINISH)
                IF(flag_file.eq.1) CALL Finish()
                MainWndProc = 0
                return
                
            case (ID_MAINWINDOW_SHOW)
                ret = showGraphicsWindow(1)
                MainWndProc = 0
                return

            case (ID_MAINWINDOW_HIDE)
                ret = showGraphicsWindow(0)
                MainWndProc = 0
                return
                
            case (ID_HEADER_INCREASEROADNUMBER)
                road_number = road_number + 1
                if (road_number .ge. 170) then
                road_number = 170
                end if                  
                MainWndProc = 0
                return

            case (ID_HEADER_DECREASEROADNUMBER)
                road_number = road_number - 1
                if (road_number .le. 150) then
                road_number = 1
                end if                
                MainWndProc = 0
                return                
				
            case (ID_CHANNELS_1)
                CALL SetChannels(1)
                MainWndProc = 0
                return    

            case (ID_CHANNELS_3)
                CALL SetChannels(3)
                MainWndProc = 0
                return    

            case (ID_CHANNELS_6)
                CALL SetChannels(6)
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
    use CFReceiverGlobals

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

 


SUBROUTINE Start()

  use dflogm

  implicit none
  
  integer lres, StartAnalisys

  lres = StartAnalisys(LOC("c:/temp/geodata"C))

END SUBROUTINE Start


SUBROUTINE Finish()

  use dflogm

  implicit none

  integer v, StopAnalysis

  v = StopAnalysis()

END SUBROUTINE Finish




SUBROUTINE SendSampleTrace(count)

  use dflogm
  use CFReceiverGlobals
  use gl_graphics

  implicit none

  integer v, getGval, I, lret
  CHARACTER*1 :: trace_sample(5717) 
  integer*2 :: trace_data(5717) 
  integer :: N, count

  trace_data = (/78, 0, 0, 0, 18, 0, 208, 152, 208, 189, 209, 130, 208, 181, 208, 179, 209, 128, 208, 176, 208, 187,& 
 45, 50, 6, 0, 60, 110, 117, 108, 108, 62, 6, 0, 60, 110, 117, 108, 108, 62, 22, 0, 208, 157, 208, & 
181, 208, 184, 208, 183, 208, 178, 208, 181, 209, 129, 209, 130, 208, 189, 209, 139, 208, 185, 109,& 
 134, 180, 109, 2, 69, 205, 1, 64, 34, 189, 0, 8, 2, 100, 255, 66, 255, 52, 255, 108, 191, 186, 0, 151, & 
221, 156, 255, 182, 255, 197, 255, 201, 255, 199, 255, 194, 255, 192, 255, 193, 255, 196, 255, 200, & 
255, 204, 255, 209, 255, 221, 255, 242, 255, 14, 0, 42, 0, 58, 0, 50, 0, 17, 0, 221, 255, 168, 255, & 
138, 255, 141, 255, 171, 255, 194, 255, 158, 255, 13, 255, 250, 253, 120, 252, 201, 250, 68, 249, 50, & 
248, 160, 247, 74, 247, 160, 246, 240, 244, 172, 241, 176, 236, 121, 230, 32, 224, 40, 219, 37, 217, & 
86, 219, 79, 226, 216, 237, 254, 252, 80, 14, 47, 32, 26, 49, 215, 63, 129, 75, 126, 83, 100, 87, 246, & 
86, 36, 82, 32, 73, 119, 60, 17, 45, 40, 28, 35, 11, 97, 251, 7, 238, 219, 227, 49, 221, 240, 217, 166, & 
217, 165, 219, 40, 223, 114, 227, 236, 231, 61, 236, 81, 240, 83, 244, 147, 248, 95, 253, 218, 2, 219, & 
8, 228, 14, 52, 20, 245, 23, 127, 25, 135, 24, 69, 21, 106, 16, 246, 10, 251, 5, 80, 2, 95, 0, 18, 0, & 
226, 0, 14, 2, 214, 2, 180, 2, 131, 1, 123, 255, 32, 253, 12, 251, 193, 249, 131, 249, 72, 250, 196, & 
251, 129, 253, 11, 255, 15, 0, 113, 0, 75, 0, 222, 255, 122, 255, 95, 255, 171, 255, 85, 0, 54, 1, 19, & 
2, 184, 2, 3, 3, 231, 2, 113, 2, 192, 1, 247, 0, 58, 0, 163, 255, 64, 255, 23, 255, 32, 255, 77, 255, & 
139, 255, 199, 255, 239, 255, 253, 255, 242, 255, 216, 255, 194, 255, 191, 255, 219, 255, 21, 0, 100, & 
0, 184, 0, 253, 0, 37, 1, 41, 1, 12, 1, 215, 0, 151, 0, 85, 0, 25, 0, 231, 255, 190, 255, 158, 255, 137, & 
255, 129, 255, 138, 255, 163, 255, 203, 255, 251, 255, 43, 0, 84, 0, 114, 0, 134, 0, 147, 0, 161, 0, 178, & 
0, 198, 0, 212, 0, 211, 0, 182, 0, 119, 0, 24, 0, 167, 255, 57, 255, 233, 254, 208, 254, 251, 254, 105, & 
255, 10, 0, 191, 0, 98, 1, 209, 1, 244, 1, 194, 1, 72, 1, 161, 0, 240, 255, 87, 255, 241, 254, 199, 254, & 
214, 254, 12, 255, 85, 255, 153, 255, 203, 255, 231, 255, 241, 255, 241, 255, 243, 255, 252, 255, 13, 0, & 
33, 0, 46, 0, 45, 0, 25, 0, 244, 255, 197, 255, 152, 255, 123, 255, 120, 255, 148, 255, 201, 255, 11, 0, & 
73, 0, 113, 0, 119, 0, 90, 0, 35, 0, 230, 255, 186, 255, 183, 255, 233, 255, 81, 0, 223, 0, 121, 1, 252, & 
1, 76, 2, 86, 2, 21, 2, 149, 1, 238, 0, 63, 0, 165, 255, 54, 255, 255, 254, 251, 254, 32, 255, 89, 255, & 
147, 255, 190, 255, 211, 255, 210, 255, 195, 255, 178, 255, 171, 255, 180, 255, 207, 255, 249, 255, 41, & 
0, 87, 0, 125, 0, 151, 0, 165, 0, 169, 0, 165, 0, 157, 0, 145, 0, 128, 0, 107, 0, 80, 0, 47, 0, 12, 0, & 
230, 255, 193, 255, 160, 255, 134, 255, 119, 255, 117, 255, 130, 255, 155, 255, 189, 255, 227, 255, 7, & 
0, 34, 0, 50, 0, 57, 0, 57, 0, 55, 0, 55, 0, 59, 0, 66, 0, 73, 0, 78, 0, 78, 0, 74, 0, 68, 0, 63, 0, 62, & 
0, 64, 0, 67, 0, 69, 0, 66, 0, 57, 0, 43, 0, 26, 0, 12, 0, 3, 0, 0, 0, 3, 0, 8, 0, 12, 0, 10, 0, 3, 0, & 
248, 255, 236, 255, 228, 255, 228, 255, 238, 255, 1, 0, 25, 0, 51, 0, 74, 0, 91, 0, 101, 0, 104, 0, 103, & 
0, 101, 0, 101, 0, 101, 0, 101, 0, 98, 0, 87, 0, 67, 0, 39, 0, 4, 0, 225, 255, 196, 255, 178, 255, 174, & 
255, 181, 255, 196, 255, 213, 255, 226, 255, 231, 255, 229, 255, 221, 255, 212, 255, 209, 255, 214, 255, & 
229, 255, 252, 255, 24, 0, 51, 0, 74, 0, 89, 0, 95, 0, 92, 0, 81, 0, 67, 0, 50, 0, 35, 0, 21, 0, 11, 0, 5, & 
0, 2, 0, 1, 0, 2, 0, 2, 0, 1, 0, 253, 255, 246, 255, 236, 255, 226, 255, 219, 255, 217, 255, 221, 255, 234, & 
255, 252, 255, 18, 0, 39, 0, 54, 0, 61, 0, 59, 0, 48, 0, 31, 0, 12, 0, 250, 255, 237, 255, 231, 255, 231, & 
255, 237, 255, 247, 255, 4, 0, 18, 0, 32, 0, 44, 0, 53, 0, 58, 0, 59, 0, 56, 0, 49, 0, 40, 0, 32, 0, 25, 0, & 
22, 0, 23, 0, 26, 0, 29, 0, 28, 0, 22, 0, 11, 0, 253, 255, 240, 255, 232, 255, 233, 255, 244, 255, 7, 0, 29, & 
0, 49, 0, 62, 0, 63, 0, 53, 0, 33, 0, 8, 0, 239, 255, 218, 255, 204, 255, 197, 255, 195, 255, 196, 255, 197, & 
255, 197, 255, 193, 255, 187, 255, 181, 255, 178, 255, 180, 255, 188, 255, 204, 255, 225, 255, 248, 255, 15, & 
0, 33, 0, 45, 0, 51, 0, 51, 0, 49, 0, 47, 0, 45, 0, 45, 0, 44, 0, 40, 0, 32, 0, 20, 0, 4, 0, 243, 255, 229, & 
255, 221, 255, 221, 255, 229, 255, 244, 255, 7, 0, 26, 0, 42, 0, 53, 0, 57, 0, 56, 0, 50, 0, 41, 0, 32, 0, 23, & 
0, 16, 0, 12, 0, 9, 0, 8, 0, 7, 0, 8, 0, 9, 0, 12, 0, 15, 0, 19, 0, 24, 0, 29, 0, 33, 0, 35, 0, 36, 0, 34, 0, & 
29, 0, 23, 0, 15, 0, 6, 0, 254, 255, 246, 255, 240, 255, 236, 255, 234, 255, 233, 255, 235, 255, 238, 255, 243, & 
255, 248, 255, 253, 255, 2, 0, 5, 0, 8, 0, 9, 0, 8, 0, 7, 0, 5, 0, 5, 0, 6, 0, 9, 0, 14, 0, 20, 0, 26, 0, 30, & 
0, 30, 0, 0, 0, 18, 0, 6, 0, 0, 0, 40, 255, 242, 254, 216, 254, 221, 254, 251, 254, 40, 255, 86, 255, 123, 255,& 
 145, 255, 151, 255, 146, 255, 139, 255, 134, 255, 134, 255, 140, 255, 146, 255, 151, 255, 154, 255, 156, 255, & 
163, 255, 178, 255, 202, 255, 228, 255, 245, 255, 240, 255, 206, 255, 148, 255, 79, 255, 21, 255, 241, 254, 222, & 
254, 191, 254, 103, 254, 170, 253, 120, 252, 237, 250, 78, 249, 241, 2457, 18, 247, 169, 246, 82, 246, 93, 245, & 
15, 243, 238, 238, 27, 233, 119, 226, 143, 220, 68, 217, 69, 218, 150, 224, 49, 236, 9, 252, 75, 14, 219, 32, & 
212, 49, 231, 63, 119, 74, 120, 81, 33, 85, 171, 85, 33, 83, 107, 77, 125, 68, 142, 56, 69, 42, 178, 26, 38, & 
11, 237, 252, 8, 241, 0, 232, 224, 225, 81, 222, 204, 220, 203, 220, 233, 221, 235, 223, 190, 226, 93, 230, & 
198, 234, 234, 239, 168, 245, 205, 251, 16, 2, 18, 8, 105, 13, 165, 17, 98, 20, 96, 21, 148, 20, 51, 18, 173, & 
14, 155, 10, 156, 6, 56, 3, 199, 0, 96, 255, 224, 254, 0, 255, 98, 255, 183, 255, 198, 255, 123, 255, 231, & 
254, 50, 254, 144, 253, 47, 253, 47, 253, 146, 253, 69, 254, 29, 255, 234, 255, 129, 0, 201, 0, 193, 0, 127, & 
0, 40, 0, 223, 255, 190, 255, 203, 255, 250, 255, 50, 0, 90, 0, 98, 0, 71, 0, 22, 0, 227, 255, 192, 255, 182, & 
255, 195, 255, 219, 255, 239, 255, 246, 255, 239, 255, 228, 255, 229, 255, 1, 0, 61, 0, 149, 0, 250, 0, 86, & 
1, 149, 1, 170, 1, 147, 1, 89, 1, 9, 1, 182, 0, 108, 0, 51, 0, 11, 0, 240, 255, 220, 255, 202, 255, 184, 255, & 
167, 255, 157, 255, 156, 255, 168, 255, 191, 255, 221, 255, 253, 255, 22, 0, 37, 0, 40, 0, 35, 0, 26, 0, 21, & 
0, 24, 0, 36, 0, 57, 0, 80, 0, 101, 0, 116, 0, 125, 0, 128, 0, 131, 0, 133, 0, 133, 0, 128, 0, 113, 0, 84, 0, & 
46, 0, 7, 0, 235, 255, 230, 255, 249, 255, 27, 0, 53, 0, 44, 0, 232, 255, 97, 255, 164, 254, 217, 253, 55, & 
253, 246, 252, 61, 253, 24, 254, 108, 255, 255, 0, 133, 2, 180, 3, 87, 4, 91, 4, 207, 3, 224, 2, 198, 1, 184, & 
0, 220, 255, 66, 255, 230, 254, 183, 254, 161, 254, 150, 254, 143, 254, 146, 254, 164, 254, 204, 254, 12, 255, & 
94, 255, 183, 255, 15, 0, 95, 0, 167, 0, 235, 0, 47, 1, 119, 1, 191, 1, 251, 1, 30, 2, 24, 2, 226, 1, 122, 1, & 
236, 0, 75, 0, 175, 255, 44, 255, 211, 254, 171, 254, 177, 254, 219, 254, 29, 255, 103, 255, 170, 255, 221, & 
255, 249, 255, 0, 0, 247, 255, 231, 255, 223, 255, 234, 255, 18, 0, 88, 0, 182, 0, 31, 1, 129, 1, 199, 1, 228, & 
1, 207, 1, 137, 1, 29, 1, 155, 0, 20, 0, 154, 255, 59, 255, 254, 254, 227, 254, 231, 254, 3, 255, 43, 255, 85, & 
255, 122, 255, 146, 255, 156, 255, 156, 255, 150, 255, 146, 255, 151, 255, 169, 255, 201, 255, 244, 255, 35, 0, & 
79, 0, 112, 0, 128, 0, 126, 0, 107, 0, 77, 0, 41, 0, 7, 0, 237, 255, 222, 255, 222, 255, 234, 255, 1, 0, 31, 0, & 
63, 0, 92, 0, 117, 0, 135, 0, 145, 0, 148, 0, 145, 0, 139, 0, 130, 0, 120, 0, 108, 0, 94, 0, 76, 0, 53, 0, 23, & 
0, 245, 255, 208, 255, 174, 255, 149, 255, 139, 255, 147, 255, 174, 255, 217, 255, 10, 0, 56, 0, 89, 0, 101, 0, & 
90, 0, 60, 0, 17, 0, 228, 255, 189, 255, 164, 255, 153, 255, 156, 255, 166, 255, 180, 255, 193, 255, 202, 255, & 
209, 255, 215, 255, 223, 255, 233, 255, 247, 255, 7, 0, 23, 0, 37, 0, 48, 0, 54, 0, 56, 0, 54, 0, 49, 0, 40, 0, & 
28, 0, 14, 0, 255, 255, 238, 255, 222, 255, 206, 255, 193, 255, 183, 255, 177, 255, 177, 255, 182, 255, 192, 255, & 
207, 255, 225, 255, 244, 255, 7, 0, 22, 0, 33, 0, 38, 0, 39, 0, 37, 0, 33, 0, 30, 0, 30, 0, 34, 0, 44, 0, 58, 0, & 
74, 0, 88, 0, 97, 0, 99, 0, 90, 0, 72, 0, 45, 0, 13, 0, 236, 255, 206, 255, 183, 255, 170, 255, 167, 255, 174, & 
255, 190, 255, 213, 255, 239, 255, 9, 0, 29, 0, 42, 0, 45, 0, 40, 0, 30, 0, 19, 0, 12, 0, 12, 0, 20, 0, 35, 0, & 
51, 0, 64, 0, 68, 0, 59, 0, 38, 0, 8, 0, 230, 255, 198, 255, 173, 255, 158, 255, 153, 255, 156, 255, 163, 255, & 
173, 255, 182, 255, 192, 255, 201, 255, 211, 255, 221, 255, 231, 255, 239, 255, 243, 255, 244, 255, 241, 255, & 
236, 255, 232, 255, 230, 255, 233, 255, 239, 255, 248, 255, 1, 0, 7, 0, 10, 0, 8, 0, 3, 0, 254, 255, 250, 255, & 
249, 255, 251, 255, 1, 0, 10, 0, 20, 0, 30, 0, 37, 0, 42, 0, 42, 0, 36, 0, 23, 0, 4, 0, 235, 255, 208, 255, 180, & 
255, 155, 255, 138, 255, 131, 255, 135, 255, 148, 255, 170, 255, 197, 255, 225, 255, 252, 255, 18, 0, 34, 0, 44, & 
0, 47, 0, 44, 0, 39, 0, 32, 0, 27, 0, 24, 0, 23, 0, 25, 0, 26, 0, 25, 0, 21, 0, 12, 0, 255, 255, 238, 255, 221, & 
255, 204, 255, 192, 255, 184, 255, 182, 255, 186, 255, 195, 255, 207, 255, 219, 255, 230, 255, 238, 255, 244, & 
255, 247, 255, 250, 255, 253, 255, 1, 0, 7, 0, 14, 0, 20, 0, 23, 0, 23, 0, 18, 0, 11, 0, 2, 0, 249, 255, 241, & 
255, 234, 255, 227, 255, 0, 0, 212, 255, 205, 255, 0, 0, 51, 255, 252, 254, 221, 254, 219, 254, 241, 254, 22, & 
255, 63, 255, 97, 255, 118, 255, 126, 255, 123, 255, 117, 255, 112, 255, 112, 255, 118, 255, 130, 255, 146, 255, & 
165, 255, 187, 255, 213, 255, 241, 255, 10, 0, 28, 0, 36, 0, 32, 0, 23, 0, 21, 0, 36, 0, 75, 0, 129, 0, 176, 0, & 
184, 0, 120, 0, 224, 255, 250, 254, 231, 253, 212, 252, 222, 251, 249, 250, 225, 249, 29, 248, 36, 245, 140, & 
240, 66, 234, 179, 226, 202, 218, 212, 211, 53, 207, 25, 206, 48, 209, 133, 216, 137, 227, 63, 241, 123, 0, 36, & 
16, 90, 31, 124, 45, 20, 58, 185, 68, 246, 76, 65, 82, 17, 84, 253, 81, 232, 75, 18, 66, 31, 53, 2, 38, 214, 21, & 
187, 5, 175, 246, 128, 233, 193, 222, 205, 214, 207, 209, 198, 207, 136, 208, 194, 211, 4, 217, 203, 223, 141, & 
231, 205, 239, 33, 248, 56, 0, 209, 7, 182, 14, 176, 20, 128, 25, 231, 28, 176, 30, 188, 30, 13, 29, 209, 25, & 
92, 21, 27, 16, 137, 10, 28, 5, 53, 0, 29, 252, 251, 248, 220, 246, 181, 245, 107, 245, 218, 245, 220, 246, 74, & 
248, 0, 250, 222, 251, 195, 253, 146, 255, 45, 1, 120, 2, 96, 3, 216, 3, 227, 3, 143, 3, 245, 2, 53, 2, 110, 1, & 
186, 0, 43, 0, 200, 255, 148, 255, 134, 255, 151, 255, 187, 255, 233, 255, 22, 0, 60, 0, 83, 0, 89, 0, 79, 0, & 
56, 0, 28, 0, 2, 0, 241, 255, 236, 255, 245, 255, 11, 0, 44, 0, 83, 0, 127, 0, 170, 0, 210, 0, 242, 0, 7, 1, & 
14, 1, 6, 1, 238, 0, 204, 0, 162, 0, 120, 0, 81, 0, 49, 0, 25, 0, 11, 0, 4, 0, 6, 0, 14, 0, 28, 0, 46, 0, 65, 0, & 
81, 0, 90, 0, 89, 0, 75, 0, 51, 0, 17, 0, 233, 255, 194, 255, 159, 255, 135, 255, 124, 255, 132, 255, 158, 255, & 
204, 255, 11, 0, 85, 0, 163, 0, 238, 0, 45, 1, 90, 1, 113, 1, 113, 1, 91, 1, 51, 1, 252, 0, 188, 0, 119, 0, 49, & 
0, 239, 255, 179, 255, 128, 255, 88, 255, 63, 255, 52, 255, 57, 255, 78, 255, 114, 255, 161, 255, 217, 255, 18, & 
0, 72, 0, 114, 0, 141, 0, 151, 0, 144, 0, 125, 0, 98, 0, 72, 0, 49, 0, 32, 0, 20, 0, 13, 0, 5, 0, 253, 255, 243, & 
255, 233, 255, 225, 255, 222, 255, 225, 255, 236, 255, 254, 255, 23, 0, 51, 0, 83, 0, 118, 0, 155, 0, 195, 0, & 
237, 0, 24, 1, 66, 1, 102, 1, 128, 1, 138, 1, 127, 1, 92, 1, 33, 1, 208, 0, 112, 0, 11, 0, 171, 255, 90, 255, & 
31, 255, 252, 254, 242, 254, 251, 254, 17, 255, 47, 255, 82, 255, 119, 255, 160, 255, 206, 255, 1, 0, 57, 0, 113, & 
0, 165, 0, 207, 0, 236, 0, 248, 0, 243, 0, 223, 0, 192, 0, 152, 0, 109, 0, 66, 0, 27, 0, 250, 255, 222, 255, & 
201, 255, 184, 255, 170, 255, 159, 255, 150, 255, 146, 255, 149, 255, 161, 255, 184, 255, 217, 255, 0, 0, 41, & 
0, 80, 0, 111, 0, 133, 0, 147, 0, 156, 0, 161, 0, 167, 0, 175, 0, 184, 0, 193, 0, 200, 0, 201, 0, 197, 0, 185, & 
0, 166, 0, 142, 0, 114, 0, 85, 0, 56, 0, 31, 0, 11, 0, 253, 255, 247, 255, 249, 255, 1, 0, 14, 0, 31, 0, 49, 0, & 
66, 0, 80, 0, 89, 0, 93, 0, 91, 0, 86, 0, 79, 0, 73, 0, 70, 0, 73, 0, 81, 0, 96, 0, 114, 0, 132, 0, 149, 0, 161, & 
0, 167, 0, 168, 0, 164, 0, 155, 0, 143, 0, 128, 0, 108, 0, 85, 0, 58, 0, 28, 0, 255, 255, 230, 255, 211, 255, 202, & 
255, 201, 255, 209, 255, 222, 255, 240, 255, 3, 0, 24, 0, 46, 0, 69, 0, 93, 0, 116, 0, 137, 0, 151, 0, 155, 0, & 
150, 0, 134, 0, 109, 0, 80, 0, 50, 0, 22, 0, 255, 255, 237, 255, 226, 255, 220, 255, 218, 255, 219, 255, 222, & 
255, 228, 255, 236, 255, 247, 255, 5, 0, 20, 0, 36, 0, 50, 0, 62, 0, 70, 0, 74, 0, 76, 0, 78, 0, 81, 0, 87, 0, & 
95, 0, 105, 0, 112, 0, 116, 0, 115, 0, 108, 0, 97, 0, 83, 0, 70, 0, 59, 0, 50, 0, 44, 0, 40, 0, 37, 0, 35, 0, & 
34, 0, 35, 0, 39, 0, 46, 0, 54, 0, 62, 0, 68, 0, 70, 0, 68, 0, 62, 0, 55, 0, 49, 0, 44, 0, 43, 0, 45, 0, 48, 0, & 
53, 0, 57, 0, 58, 0, 56, 0, 50, 0, 41, 0, 31, 0, 21, 0, 13, 0, 8, 0, 8, 0, 8, 0, 8, 0, 3, 0, 248, 255, 229, 255, & 
205, 255, 179, 255, 158, 255, 146, 255, 146, 255, 159, 255, 183, 255, 212, 255, 241, 255, 10, 0, 29, 0, 42, 0, 51, & 
0, 59, 0, 67, 0, 77, 0, 89, 0, 101, 0, 109, 0, 111, 0, 105, 0, 92, 0, 73, 0, 52, 0, 31, 0, 16, 0, 9, 0, 9, 0, & 
16, 0, 27, 0, 39, 0, 49, 0, 55, 0, 57, 0, 54, 0, 50, 0, 46, 0, 43, 0, 42, 0, 42, 0, 43, 0, 45, 0, 48, 0, 52, 0, & 
55, 0, 58, 0, 60, 0, 58, 0, 53, 0, 44, 0, 32, 0, 21, 0, 12, 0, 8, 0, 10, 0, 16, 0, 25, 0, 34, 0, 39, 0, 38, 0, & 
31, 0, 18, 0, 1, 0, 238, 255, 220, 255, 205, 255, 195, 255, 190, 255, 191, 255, 199, 255, 212, 255, 229, 255, 248, & 
255, 11, 0, 27, 0, 38, 0, 43, 0, 43, 0, 39, 0, 33, 0, 28, 0, 25, 0, 25, 0, 0, 0, 27, 0, 28, 0, 0, 0, 9, 0, 10, 0, & 
11, 0, 12, 0, 14, 0, 17, 0, 24, 0, 31, 0, 41, 0, 51, 0, 59, 0, 65, 0, 67, 0, 65, 0, 65, 0, 65, 0, 76, 0, 102, 0, & 
146, 0, 197, 0, 249, 0, 33, 1, 43, 1, 13, 1, 199, 0, 102, 0, 2, 0, 189, 255, 180, 255, 242, 255, 86, 0, 146, 0, & 
46, 0, 137, 254, 253, 250, 7, 245, 113, 236, 119, 225, 219, 212, 225, 199, 48, 188, 154, 179, 211, 175, 40, 178, & 
60, 187, 230, 202, 42, 224, 84, 249, 52, 20, 111, 46, 200, 69, 110, 88, 43, 101, 124, 107, 138, 107, 13, 102, 34, & 
92, 23, 79, 59, 64, 190, 48, 151, 33, 124, 19, 232, 6, 33, 252, 70, 243, 102, 236, 123, 231, 117, 228, 62, 227, & 
177, 227, 155, 229, 186, 232, 187, 236, 64, 241, 239, 245, 111, 250, 123, 254, 224, 1, 122, 4, 64, 6, 39, 7, 46, & 
7, 87, 6, 180, 4, 80, 2, 80, 255, 242, 251, 141, 248, 142, 245, 111, 243, 160, 242, 119, 243, 20, 246, 91, 250, & 
243, 255, 75, 6, 163, 12, 64, 18, 116, 22, 193, 24, 232, 24, 240, 22, 36, 19, 2, 14, 39, 8, 54, 2, 197, 252, 75, & 
248, 16, 245, 51, 243, 167, 242, 60, 243, 174, 244, 173, 246, 233, 248, 29, 251, 20, 253, 170, 254, 207, 255, & 
131, 0, 211, 0, 211, 0, 154, 0, 63, 0, 213, 255, 107, 255, 13, 255, 190, 254, 134, 254, 94, 254, 70, 254, 58, & 
254, 55, 254, 60, 254, 72, 254, 94, 254, 130, 254, 188, 254, 18, 255, 140, 255, 44, 0, 241, 0, 211, 1, 196, 2, & 
178, 3, 133, 4, 37, 5, 121, 5, 112, 5, 255, 4, 33, 4, 224, 2, 78, 1, 136, 255, 177, 253, 241, 251, 110, 250, 76, & 
249, 164, 248, 132, 248, 236, 248, 206, 249, 17, 251, 146, 252, 45, 254, 189, 255, 36, 1, 77, 2, 44, 3, 192, 3, & 
13, 4, 27, 4, 246, 3, 166, 3, 50, 3, 159, 2, 242, 1, 47, 1, 92, 0, 131, 255, 175, 254, 238, 253, 77, 253, 215, & 
252, 146, 252, 128, 252, 158, 252, 230, 252, 78, 253, 204, 253, 88, 254, 233, 254, 125, 255, 13, 0, 152, 0, 25, & 
1, 137, 1, 224, 1, 23, 2, 34, 2, 252, 1, 161, 1, 18, 1, 88, 0, 128, 255, 157, 254, 196, 253, 9, 253, 126, 252, & 
47, 252, 34, 252, 87, 252, 195, 252, 91, 253, 13, 254, 199, 254, 122, 255, 23, 0, 150, 0, 241, 0, 40, 1, 60, 1, & 
51, 1, 18, 1, 224, 0, 162, 0, 94, 0, 25, 0, 215, 255, 157, 255, 110, 255, 74, 255, 53, 255, 46, 255, 52, 255, 68, & 
255, 92, 255, 119, 255, 145, 255, 166, 255, 178, 255, 180, 255, 171, 255, 152, 255, 126, 255, 98, 255, 73, 255, & 
53, 255, 45, 255, 48, 255, 65, 255, 93, 255, 130, 255, 170, 255, 210, 255, 244, 255, 12, 0, 24, 0, 23, 0, 9, 0, & 
241, 255, 210, 255, 175, 255, 140, 255, 109, 255, 83, 255, 64, 255, 52, 255, 47, 255, 49, 255, 56, 255, 66, 255, & 
80, 255, 96, 255, 114, 255, 134, 255, 154, 255, 176, 255, 197, 255, 217, 255, 234, 255, 247, 255, 254, 255, 254, & 
255, 246, 255, 230, 255, 206, 255, 175, 255, 138, 255, 97, 255, 54, 255, 11, 255, 227, 254, 192, 254, 164, 254, & 
146, 254, 139, 254, 145, 254, 163, 254, 192, 254, 232, 254, 24, 255, 76, 255, 131, 255, 185, 255, 236, 255, 26, & 
0, 65, 0, 97, 0, 120, 0, 134, 0, 139, 0, 135, 0, 123, 0, 102, 0, 74, 0, 40, 0, 2, 0, 218, 255, 178, 255, 140, & 
255, 107, 255, 80, 255, 60, 255, 48, 255, 44, 255, 46, 255, 53, 255, 65, 255, 80, 255, 96, 255, 113, 255, 128, & 
255, 142, 255, 152, 255, 160, 255, 163, 255, 162, 255, 157, 255, 148, 255, 135, 255, 120, 255, 105, 255, 90, & 
255, 78, 255, 71, 255, 71, 255, 80, 255, 97, 255, 124, 255, 160, 255, 203, 255, 249, 255, 40, 0, 84, 0, 120, & 
0, 146, 0, 161, 0, 162, 0, 152, 0, 131, 0, 101, 0, 67, 0, 30, 0, 250, 255, 216, 255, 188, 255, 165, 255, 149, & 
255, 139, 255, 136, 255, 138, 255, 145, 255, 155, 255, 166, 255, 177, 255, 185, 255, 190, 255, 192, 255, 189, & 
255, 182, 255, 172, 255, 161, 255, 148, 255, 136, 255, 125, 255, 116, 255, 111, 255, 109, 255, 110, 255, 115, & 
255, 123, 255, 133, 255, 147, 255, 162, 255, 180, 255, 199, 255, 221, 255, 244, 255, 13, 0, 40, 0, 67, 0, 94, & 
0, 119, 0, 141, 0, 158, 0, 168, 0, 171, 0, 166, 0, 152, 0, 132, 0, 105, 0, 73, 0, 38, 0, 1, 0, 221, 255, 187, & 
255, 155, 255, 128, 255, 106, 255, 89, 255, 79, 255, 75, 255, 77, 255, 84, 255, 97, 255, 112, 255, 130, 255, & 
147, 255, 164, 255, 178, 255, 189, 255, 198, 255, 205, 255, 210, 255, 216, 255, 222, 255, 231, 255, 241, 255, & 
254, 255, 13, 0, 30, 0, 47, 0, 64, 0, 79, 0, 92, 0, 103, 0, 110, 0, 114, 0, 113, 0, 109, 0, 100, 0, 86, 0, 69, & 
0, 48, 0, 25, 0, 1, 0, 232, 255, 208, 255, 185, 255, 165, 255, 148, 255, 135, 255, 126, 255, 122, 255, 122, & 
255, 128, 255, 138, 255, 151, 255, 168, 255, 186, 255, 205, 255, 223, 255, 240, 255, 255, 255, 11, 0, 20, 0, & 
26, 0, 28, 0, 28, 0, 24, 0, 18, 0, 9, 0, 0, 0, 246, 255, 237, 255, 231, 255, 228, 255, 229, 255, 233, 255, 242, & 
255, 253, 255, 9, 0, 21, 0, 33, 0, 42, 0, 48, 0, 51, 0, 50, 0, 44, 0, 36, 0, 24, 0, 10, 0, 252, 255, 238, 255, & 
224, 255, 213, 255, 204, 255, 0, 0, 196, 255, 196, 255, 0, 0, 9, 0, 14, 0, 21, 0, 28, 0, 38, 0, 51, 0, 63, 0, & 
72, 0, 78, 0, 80, 0, 79, 0, 78, 0, 78, 0, 82, 0, 104, 0, 147, 0, 207, 0, 22, 1, 86, 1, 128, 1, 131, 1, 85, 1, & 
252, 0, 142, 0, 49, 0, 14, 0, 72, 0, 224, 0, 173, 1, 69, 2, 12, 2, 59, 0, 11, 252, 225, 244, 124, 234, 31, 221, & 
167, 205, 125, 189, 121, 174, 159, 162, 213, 155, 149, 155, 172, 162, 22, 177, 249, 197, 192, 223, 87, 252, 110, & 
25, 199, 52, 110, 76, 242, 94, 111, 107, 154, 113, 169, 113, 63, 108, 71, 98, 217, 84, 32, 69, 68, 52, 89, 35, & 
84, 19, 255, 4, 243, 248, 148, 239, 18, 233, 100, 229, 89, 228, 146, 229, 147, 232, 204, 236, 172, 241, 162, & 
246, 45, 251, 236, 254, 162, 1, 61, 3, 221, 3, 192, 3, 56, 3, 158, 2, 70, 2, 95, 2, 238, 2, 205, 3, 179, 4, 67, & 
5, 36, 5, 23, 4, 10, 2, 35, 255, 184, 251, 74, 248, 101, 245, 143, 243, 35, 243, 74, 244, 233, 246, 166, 250, & 
252, 254, 79, 3, 7, 7, 173, 9, 252, 10, 235, 10, 175, 9, 168, 7, 80, 5, 38, 3, 149, 1, 227, 0, 39, 1, 73, 2, 8, & 
4, 8, 6, 230, 7, 71, 9, 229, 9, 158, 9, 113, 8, 129, 6, 8, 4, 82, 1, 171, 254, 86, 252, 135, 250, 90, 249, 210, & 
248, 224, 248, 96, 249, 42, 250, 17, 251, 240, 251, 168, 252, 44, 253, 119, 253, 146, 253, 141, 253, 126, 253,& 
 123, 253, 150, 253, 222, 253, 92, 254, 17, 255, 247, 255, 2, 1, 33, 2, 63, 3, 70, 4, 33, 5, 191, 5, 19, 6, 27, & 
6, 219, 5, 93, 5, 181, 4, 245, 3, 51, 3, 127, 2, 227, 1, 99, 1, 253, 0, 165, 0, 82, 0, 245, 255, 136, 255, 4, & 
255, 107, 254, 197, 253, 31, 253, 137, 252, 18, 252, 201, 251, 184, 251, 228, 251, 75, 252, 229, 252, 164, 253, & 
119, 254, 75, 255, 14, 0, 177, 0, 40, 1, 108, 1, 123, 1, 90, 1, 16, 1, 169, 0, 49, 0, 183, 255, 71, 255, 238, & 
254, 180, 254, 159, 254, 178, 254, 234, 254, 67, 255, 183, 255, 59, 0, 195, 0, 68, 1, 177, 1, 0, 2, 39, 2, 34, & 
2, 239, 1, 146, 1, 19, 1, 124, 0, 219, 255, 63, 255, 179, 254, 66, 254, 239, 253, 189, 253, 167, 253, 170, 253, & 
188, 253, 217, 253, 251, 253, 30, 254, 68, 254, 109, 254, 156, 254, 211, 254, 17, 255, 86, 255, 157, 255, 225, & 
255, 27, 0, 68, 0, 85, 0, 74, 0, 35, 0, 226, 255, 140, 255, 42, 255, 198, 254, 108, 254, 41, 254, 4, 254, 5, 254, & 
47, 254, 127, 254, 240, 254, 119, 255, 9, 0, 154, 0, 29, 1, 137, 1, 217, 1, 8, 2, 23, 2, 9, 2, 226, 1, 165, 1, & 
86, 1, 247, 0, 138, 0, 16, 0, 138, 255, 252, 254, 104, 254, 213, 253, 75, 253, 209, 252, 112, 252, 48, 252, 20, & 
252, 32, 252, 83, 252, 171, 252, 37, 253, 185, 253, 96, 254, 20, 255, 203, 255, 126, 0, 36, 1, 182, 1, 43, 2, & 
125, 2, 169, 2, 170, 2, 131, 2, 54, 2, 201, 1, 69, 1, 181, 0, 35, 0, 156, 255, 39, 255, 204, 254, 141, 254, 108, & 
254, 102, 254, 119, 254, 151, 254, 194, 254, 241, 254, 31, 255, 74, 255, 110, 255, 141, 255, 167, 255, 188, 255, & 
207, 255, 224, 255, 242, 255, 3, 0, 20, 0, 35, 0, 46, 0, 50, 0, 44, 0, 27, 0, 254, 255, 214, 255, 165, 255, 111, & 
255, 58, 255, 12, 255, 233, 254, 216, 254, 218, 254, 240, 254, 25, 255, 81, 255, 147, 255, 216, 255, 27, 0, 85, & 
0, 129, 0, 156, 0, 163, 0, 147, 0, 111, 0, 56, 0, 241, 255, 160, 255, 76, 255, 252, 254, 183, 254, 131, 254, 102, & 
254, 98, 254, 118, 254, 160, 254, 218, 254, 29, 255, 100, 255, 165, 255, 220, 255, 5, 0, 30, 0, 39, 0, 33, 0, & 
17, 0, 249, 255, 222, 255, 195, 255, 171, 255, 152, 255, 140, 255, 135, 255, 136, 255, 140, 255, 148, 255, 155, & 
255, 160, 255, 161, 255, 157, 255, 148, 255, 134, 255, 117, 255, 99, 255, 83, 255, 71, 255, 65, 255, 67, 255, & 
78, 255, 96, 255, 120, 255, 148, 255, 177, 255, 206, 255, 231, 255, 252, 255, 11, 0, 19, 0, 22, 0, 18, 0, 10, & 
0, 253, 255, 237, 255, 218, 255, 198, 255, 177, 255, 158, 255, 141, 255, 130, 255, 125, 255, 126, 255, 133, 255, & 
146, 255, 163, 255, 181, 255, 197, 255, 211, 255, 219, 255, 221, 255, 217, 255, 207, 255, 194, 255, 177, 255, & 
160, 255, 143, 255, 128, 255, 114, 255, 104, 255, 96, 255, 92, 255, 91, 255, 93, 255, 100, 255, 110, 255, 124, & 
255, 143, 255, 165, 255, 190, 255, 218, 255, 246, 255, 18, 0, 44, 0, 66, 0, 83, 0, 93, 0, 97, 0, 94, 0, 84, 0, & 
67, 0, 46, 0, 22, 0, 252, 255, 226, 255, 201, 255, 178, 255, 158, 255, 142, 255, 128, 255, 119, 255, 112, 255, & 
109, 255, 109, 255, 112, 255, 118, 255, 127, 255, 137, 255, 149, 255, 163, 255, 177, 255, 191, 255, 205, 255, & 
220, 255, 234, 255, 248, 255, 5, 0, 17, 0, 28, 0, 36, 0, 41, 0, 42, 0, 39, 0, 33, 0, 24, 0, 14, 0, 4, 0, 251, & 
255, 245, 255, 241, 255, 241, 255, 242, 255, 244, 255, 247, 255, 248, 255, 247, 255, 244, 255, 238, 255, 230, & 
255, 220, 255, 210, 255, 199, 255, 189, 255, 181, 255, 177, 255, 175, 255, 178, 255, 186, 255, 196, 255, 209, & 
255, 223, 255, 235, 255, 245, 255, 251, 255, 253, 255, 251, 255, 246, 255, 239, 255, 233, 255, 0, 0, 224, 255, & 
223, 255, 0, 0, 3, 0, 2, 0, 0, 0, 254, 255, 253, 255, 252, 255, 254, 255, 0, 0, 6, 0, 12, 0, 21, 0, 28, 0, 35, & 
0, 43, 0, 54, 0, 63, 0, 70, 0, 75, 0, 78, 0, 88, 0, 101, 0, 118, 0, 136, 0, 155, 0, 180, 0, 201, 0, 217, 0, 222, & 
0, 212, 0, 189, 0, 158, 0, 124, 0, 85, 0, 33, 0, 202, 255, 41, 255, 1, 254, 3, 252, 223, 248, 78, 244, 38, 238, & 
110, 230, 116, 221, 206, 211, 97, 202, 75, 194, 198, 188, 4, 187, 251, 189, 62, 198, 218, 211, 69, 230, 93, 252, & 
135, 20, 217, 44, 82, 67, 28, 86, 193, 99, 90, 107, 158, 108, 231, 103, 23, 94, 112, 80, 101, 64, 105, 47, 198, & 
30, 125, 15, 62, 2, 101, 247, 6, 239, 5, 233, 41, 229, 38, 227, 183, 226, 154, 227, 151, 229, 121, 232, 17, 236, & 
47, 240, 161, 244, 49, 249, 167, 253, 190, 1, 43, 5, 181, 7, 31, 9, 64, 9, 4, 8, 113, 5, 196, 1, 91, 253, 190, & 
248, 139, 244, 100, 241, 213, 239, 65, 240, 210, 242, 103, 247, 152, 253, 195, 4, 33, 12, 229, 18, 80, 24, 208, & 
27, 13, 29, 240, 27, 165, 24, 144, 19, 59, 13, 73, 6, 100, 255, 39, 249, 21, 244, 138, 240, 180, 238, 144, 238, & 
239, 239, 123, 242, 196, 245, 81, 249, 174, 252, 127, 255, 133, 1, 168, 2, 245, 2, 147, 2, 190, 1, 178, 0, 165, & 
255, 192, 254, 28, 254, 187, 253, 148, 253, 155, 253, 187, 253, 232, 253, 23, 254, 68, 254, 112, 254, 160, 254, & 
216, 254, 30, 255, 120, 255, 237, 255, 128, 0, 50, 1, 2, 2, 234, 2, 219, 3, 193, 4, 131, 5, 2, 6, 35, 6, 205, 5, & 
243, 4, 148, 3, 192, 1, 148, 255, 61, 253, 238, 250, 221, 248, 61, 247, 52, 246, 220, 245, 58, 246, 65, 247, 210, & 
248, 195, 250, 226, 252, 252, 254, 228, 0, 119, 2, 161, 3, 90, 4, 166, 4, 150, 4, 62, 4, 182, 3, 23, 3, 115, 2, & 
219, 1, 88, 1, 239, 0, 161, 0, 106, 0, 69, 0, 42, 0, 17, 0, 242, 255, 199, 255, 137, 255, 56, 255, 213, 254, 101, & 
254, 239, 253, 124, 253, 22, 253, 199, 252, 151, 252, 138, 252, 165, 252, 230, 252, 72, 253, 194, 253, 73, 254, & 
209, 254, 79, 255, 182, 255, 254, 255, 35, 0, 37, 0, 8, 0, 211, 255, 144, 255, 74, 255, 10, 255, 214, 254, 180, & 
254, 166, 254, 170, 254, 190, 254, 223, 254, 10, 255, 59, 255, 113, 255, 167, 255, 220, 255, 12, 0, 54, 0, 85, & 
0, 102, 0, 105, 0, 90, 0, 60, 0, 15, 0, 215, 255, 152, 255, 87, 255, 25, 255, 225, 254, 180, 254, 147, 254, 128, & 
254, 124, 254, 133, 254, 155, 254, 186, 254, 225, 254, 12, 255, 56, 255, 97, 255, 133, 255, 162, 255, 180, 255, & 
189, 255, 189, 255, 182, 255, 170, 255, 157, 255, 1/)

    trace_data(44) = road_number
  
	do N = 1, 5717
		trace_sample(N) = char(trace_data(N))
    end do	

	do N = 1, count
		call sendTrace(LOC(trace_sample), 0)
        call onNextTrace(pMainD, pMainGC)
        call redraw_on_default_window(pMainD, pMainGC)
    end do

    
END SUBROUTINE SendSampleTrace


SUBROUTINE SetChannels(channels)
	implicit none
	integer :: channels, lres, setChannelsNumber

	lres = setChannelsNumber(channels)
    call checkErrors(lres)
end subroutine SetChannels


SUBROUTINE sendTrace(pData, opt)
	implicit none
	integer :: pData, opt, lres, ProcessFrame

	lres = ProcessFrame(pData, opt)
    !call checkErrors(lres)
end subroutine sendTrace

subroutine checkErrors(lret)
    use user32
    use kernel32
    use CFReceiverGlobals

	implicit none
	integer :: lret, ret
    
    if (lret.ne.0) then
        ret = MessageBox(ghwndMain, "Error"C, &
                     "Error"C, MB_OK)        
    end if
    
end subroutine checkErrors

SUBROUTINE dlltest
	use dfwin 
	integer                ::  p
    pointer                 (q, sum_1d_array)   ! this is non-standard 
                                                  ! (but a common extension)
    integer                ::  n
    real, dimension(100)   ::  array
    real                   ::  total

    p = loadlibrary    ("testsub.dll"C) ! the C at the end says -
                                        ! add a null byte as in C
    q = getprocaddress (p, "SUM_1D_ARRAY"C)
    call sum_1d_array  (n, array, total)

END SUBROUTINE dlltest