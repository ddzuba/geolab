!	1. Добавить в проект файл FileTime.f90 
!	2. Подключить этот модуль там, где это нужно: use filetimeUtils
!	3. Объявить переменную типа T_SYSTEMTIME, в которую будем получать время. 
!	Пусть будет называться sysTime:  
!	type (T_SYSTEMTIME) sysTime
!	4. Вызвать функцию модуля, чтобы заполнить эту переменную:
!	call getCreationTime("c:/a/fr.bmp", sysTime),
!	где первый входной параметр - путь к файлу.
!	5. Теперь время доступно из этой переменной. Вот поля T_SYSTEMTIME:
!	sysTime%WYEAR
!	sysTime%WMONTH
!	sysTime%WDAYOFWEEK
!	sysTime%WDAY
!	sysTime%WHOUR
!	sysTime%WMINUTE
!	sysTime%WSECOND
!	sysTime%WMILLISECONDS
!	Тип каждого поля - INTEGER(2). 

!module filetimeUtils
!use dfwin
!integer*4, parameter, public :: IDE_API                  =   101
!integer*4, parameter, public :: IDE_RETSTAT              =   102
!integer*4, parameter, public :: IDB_STEP                 =   103
!integer*4, parameter, public :: IDE_TIME                 =   104
!
!integer*4, parameter, public :: DIALOGHEIGHT             =   80
!
!integer*4, parameter, public :: WM_REPSTAT               =   WM_USER+1
!
!
!integer*4, parameter, public :: IDE_FILENAME             =   101
!integer*4, parameter, public :: IDB_OPENFILE             =   102
!integer*4, parameter, public :: IDE_MONTH                =   103
!integer*4, parameter, public :: IDE_DAY                  =   104
!integer*4, parameter, public :: IDE_YEAR                 =   105
!integer*4, parameter, public :: IDE_HOURS                =   201
!integer*4, parameter, public :: IDE_MINUTES              =   202
!integer*4, parameter, public :: IDE_SECONDS              =   203
!integer*4, parameter, public :: IDB_SET                  =   204
!integer*4, parameter, public :: IDE_SIZE                 =   205
!integer*4, parameter, public :: IDC_HIDE                 =   206
!integer*4, parameter, public :: IDC_SYSTEM               =   207
!integer*4, parameter, public :: IDC_ARC                  =   208
!integer*4, parameter, public :: IDC_READ                 =   209
!integer*4, parameter, public :: IDC_NORMAL               =   210
!integer*4, parameter, public :: IDB_ATTR                 =   222
!
!
!integer*4, parameter, public :: YRMASK     =   #FE00
!integer*4, parameter, public :: YRSHIFT    =   9
!
!integer*4, parameter, public :: MONMASK    =   #01E0
!integer*4, parameter, public :: MONSHIFT   =   5
!
!integer*4, parameter, public :: DAYMASK    =   #001F
!integer*4, parameter, public :: DAYSHIFT   =   0
!
!integer*4, parameter, public :: HRMASK     =   #F800
!integer*4, parameter, public :: HRSHIFT    =   11
!integer*4, parameter, public :: MINMASK    =   #07E0
!integer*4, parameter, public :: MINSHIFT   =   5
!integer*4, parameter, public :: SECMASK    =   #001F
!integer*4, parameter, public :: SECSHIFT   =   0
!
!integer*4   hInst
!integer*4   StepTime
!
!
!
!contains 
!integer*4 function getCreationTime(fileName, TIME_VIDEO_FILE) 
!
!character (len=*) :: fileName
!integer*4           hFile
!integer*4           retCode
!type (T_FILETIME)   ftCreation, ftAccessed, ftWrittenTo
!type (T_SYSTEMTIME) sysTime
!integer*2           wDosDate, wDosTime
!character           DD*3, MM*3, YY*3, HH*3, Mn*3, SS*3
!integer*4           dwFileSize
!character           lpsFileSize*10
!integer*4           dwFileAttr
!logical(4)          bret
!integer*4 TT1,TT2
!       integer(2)  WYear
!       integer(2)  wMonth 
!       integer(2)  WDayofWeek        
!       integer(2)  wDay 
!       integer(2)  wHour 
!       integer(2)  wMinute 
!       integer(2)  wSecond 
!       integer(2)  wMilliseconds 
!REAL*8 TIME_VIDEO_FILE       
!data    retCode /0/
!    
!    hFile = CreateFile (fileName,                       &
!        GENERIC_READ,                             &
!        IOR(FILE_SHARE_READ , FILE_SHARE_WRITE),  &
!        NULL_SECURITY_ATTRIBUTES,                 &
!        OPEN_EXISTING,                            &
!        FILE_ATTRIBUTE_NORMAL,                    &
!        NULL)
!    
!   retCode = GetFileTime (hFile, ftCreation, ftAccessed, ftWrittenTo)
!    retCode = CloseHandle (hFile)
!    TT1=ftAccessed%dwLowDateTime
!    TT2=ftAccessed%dwHighDateTime
!    CALL TIMEVID(TT1,TT2,TIME_VIDEO_FILE)
!    retCode = FileTimeToSystemTime(ftCreation, sysTime)
!    WYear          =sysTime%WYEAR
!    wMonth         =sysTime%WMONTH
!    WDayofWeek     =WDAYOFWEEK
!    wDay           =sysTime%WDAY
!    wHour          =sysTime%WHOUR
!    wMinute        =sysTime%WMINUTE
!    wSecond        =sysTime%WSECOND
!    wMilliseconds  =sysTime%WMILLISECONDS
!	
!    TIME_VIDEO_FILE=wDay*86400d0+wHour*3600d0+wMinute*60d0+wSecond*1d0+wMilliseconds/1000d0		
!
!end function
!
!
!
!    end module filetimeUtils
    
SUBROUTINE TIMEVID(TTT,TTT1,GGG)		

	IMPLICIT NONE
    integer*4 TTT,TTT1
    REAL*8 AAA,AAA1
    REAL*8 GGG 
    aaa=TTT
    IF(AAA.LT.0.D0)AAA=AAA+2147483647+2147483647+2
    aaa1=TTT1
    IF(AAA1.LT.0.D0)AAA1=AAA1+2147483647+2147483647+2
    AAA1=AAA1*2147483647*2+2*AAA1+AAA          
	GGG=AAA1/10000000
	RETURN
END
!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*) TIME_VIDEO_FILE
!write(100,*) WYear,wMonth,wDay,wHour,wMinute,wSecond
