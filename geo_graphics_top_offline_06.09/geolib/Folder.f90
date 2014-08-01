!------------------------------------------------------------------------------------------------
! создание папки folderName
! в корне помещен файл makedir.bat
!------------------------------------------------------------------------------------------------
integer function make_folder(diskName,folderName) ! Если папка уже есть, 
												  ! то игнарируем выполнение 
												  ! автоматически
implicit none
character(*) diskName,folderName 
character(300) commandLine,commandLine2
character(200) programName
integer ret, RunProcess
programName = "cmd /c mkdir "
WRITE (commandLine, '(a,a,a)')  TRIM(programName),' ',TRIM(diskName)//'\'//TRIM(folderName)
commandLine2 = TRIM(commandLine)//char(0)
ret = RunProcess(commandLine2)
make_folder = ret
end function make_folder

!------------------------------------------------------------------------------------------------
! уничтожение папки folderName
! в корне помещен файл deldir.bat
!------------------------------------------------------------------------------------------------
integer function delete_folder(diskName,folderName)
implicit none
character(*) diskName,folderName 
character(300) commandLine,commandLine2
character(200) programName
integer ret, RunProcess
programName = "cmd /c rd /s /q "
WRITE (commandLine, '(a,a,a)')  TRIM(programName),' ',TRIM(diskName)//'\'//TRIM(folderName)
commandLine2 = TRIM(commandLine)//char(0)
ret = RunProcess(commandLine2)
delete_folder = ret
end function delete_folder

!------------------------------------------------------------------------------------------------
! зипование папки folderName
! в корне помещен файл zip -r
!------------------------------------------------------------------------------------------------

integer function zip_folder(diskName,folderName, archiveName)
implicit none
character(300) commandLine,commandLine2
character(*) diskName,folderName, archiveName
character(200) programName
integer ret, RunProcess
programName = "zip -r -j "
WRITE (commandLine, '(a,a,a,a,a)') TRIM(programName),' ',TRIM(diskName)//'\'//TRIM(archiveName),' ',TRIM(diskName)//'\'//TRIM(folderName)
commandLine2 = TRIM(commandLine)//char(0)
ret = RunProcess(commandLine2)
zip_folder = ret
end function zip_folder

!------------------------------------------------------------------------------------------------
! создание jpeg файлов из bmp файлов
! в корне помещен файл BMP2JPG
!------------------------------------------------------------------------------------------------

integer function bmp_to_jpg(diskName,bmpFolder, jpgFolder)

implicit none

character(*) diskName,bmpFolder, jpgFolder
character(300) commandLine, commandLine2
character(300) programName
integer ret, RunProcess

programName = "BMP2JPG"

WRITE (commandLine,'(a,a,a,a,a)') TRIM(programName),' ',TRIM(diskName)//'\'//TRIM(bmpFolder),' ',TRIM(diskName)//'\'//TRIM(jpgFolder)

commandLine2 = TRIM(commandLine)//char(0)
ret = RunProcess(commandLine2)
bmp_to_jpg = ret
	
end function bmp_to_jpg
!
!------------------------------------------------------------------------------------------------
! перезапись файла
!------------------------------------------------------------------------------------------------

integer function copy_file(diskName,bmpFolder, jpgFolder,FILE_TEXT)

implicit none

character(*) diskName,bmpFolder, jpgFolder,FILE_TEXT
character(300) commandLine, commandLine2
character(300) programName
integer ret, RunProcess

programName = "cmd /c copy /y "

WRITE (commandLine,'(a,a,a,a,a)') TRIM(programName),' ',TRIM(diskName)//'\'//TRIM(bmpFolder)//'\'//TRIM(FILE_TEXT),' ',TRIM(diskName)//'\'//TRIM(jpgFolder)//'\'//TRIM(FILE_TEXT)

commandLine2 = TRIM(commandLine)//char(0)
ret = RunProcess(commandLine2)
copy_file = ret
	
end function copy_file
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
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

	iRet = WaitForSingleObject(ProcInfo%hProcess,1000000)

end function RunProcess