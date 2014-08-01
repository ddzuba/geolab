! Параметр kluch управляет появлением окна
! kluch=0 окно есть; kluch=1 окна нет
    integer*4 function RunProcess1 (commandLine,kluch)
   use dflib
   use dfwin
   use dfwinty

	implicit none
    integer*4 kluch
	character(200) commandLine

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
   if(kluch.eq.1)then
   StartInfo%dwFlags          = StartF_UseShowWindow
   else
   StartInfo%dwFlags          = 0
   end if
   StartInfo%wShowWindow      = SW_HIDE
   StartInfo%cbReserved2      = 0
   StartInfo%lpReserved2      = NULL


   RunProcess1 = CreateProcess(null_character, &
          commandLine, &
          null_Security_Attributes, &
          null_Security_Attributes, &
          .false., &
          Null, &
          Null, &
          Null_Character, &
          StartInfo, &
          ProcInfo)

	iRet = WaitForSingleObject(ProcInfo%hProcess,1000000)

end function RunProcess1