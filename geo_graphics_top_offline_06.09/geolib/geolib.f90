


!  geolib.f90 
!
!  FUNCTIONS/SUBROUTINES exported from geolib.dll:
!	geolib      - subroutine 
!
subroutine geolib

  ! Expose subroutine geolib to users of this DLL
  !
  !DEC$ ATTRIBUTES DLLEXPORT::geolib

  ! Variables

  ! Body of geolib

end subroutine geolib


function setGval(v)
	!DEC$ ATTRIBUTES DLLEXPORT::setGval
	
	use GeolibGlobals

	implicit none

	INTEGER*4  v, setGval

	gval = v

	setGval = gval
	
end function setGval


function getGval()
	!DEC$ ATTRIBUTES DLLEXPORT::getGval
	
	use GeolibGlobals

	implicit none

	INTEGER*4  v, getGval

	getGval = gval
	
end function getGval

function setMiscGeoData(pMGD)
	!DEC$ ATTRIBUTES DLLEXPORT::setMiscGeoData
	use GeolibGlobals

	implicit none
	INTEGER*4 setMiscGeoData 
    type(T_MISC_GEO_DATA), pointer :: pMGD

    setMiscGeoData = LOC(pMainGeoData)
    pMainGeoData => pMGD
	
end function setMiscGeoData


    


! int StartAnalisys(wchar_t* resultFolder); // принимает путь к папке с результатами анализа, возвращает 0 в случае успеха.
function StartAnalisys(inPFolder)
	!DEC$ ATTRIBUTES DLLEXPORT::StartAnalisys
	
	use GeolibGlobals
	use FReciverGlobals
    use GRAFICA
    use IniFile
	implicit none

	INTEGER*4  StartAnalisys, inPFolder, ret, InitMainWindow, lret, showGraphicsWindow
	character(255) folderName; pointer (pFolder, folderName)
	
	if (isStarted.ne.0) then
		StartAnalisys = 1
		return
	end if

    call Ini_Open("geolib.ini", 999)
    !N_PRIZNAK_GRAFICA=1; ФЛАГ, указываюший показывать ли окно с графикой.
    N_PRIZNAK_GRAFICA = Ini_Read_Int("N_PRIZNAK_GRAFICA", 0)

	 
	pFolder = inPFolder
	targetFolder = folderName

	isStarted = 1
	StartAnalisys = 0
    
    
    
    if (N_PRIZNAK_GRAFICA.EQ.1) then
        return
    end if
        
	if(isWindow.eq.0) then
	    lret = InitMainWindow( dllHandler, 0, 1 )
        isWindow=1
    else
        lret = showGraphicsWindow(1)     
    end if
end function StartAnalisys


function showGraphicsWindow(nCmdShow)
	!DEC$ ATTRIBUTES DLLEXPORT::showGraphicsWindow
	
	use GeolibGlobals
	use FReciverGlobals
    use user32
    use kernel32
	
    implicit none

	INTEGER*4  showGraphicsWindow, nCmdShow, lret
    
	if (isStarted.ne.1) then
        ! если обработка не идет - окно не показывается
		showGraphicsWindow = 1
		return
    end if

    lret = ShowWindow( mainHWND, nCmdShow )
    
end function showGraphicsWindow


function setChannelsNumber(channels)
	!DEC$ ATTRIBUTES DLLEXPORT::setChannelsNumber
	use GeolibGlobals
    use PARAM_1

	implicit none

	INTEGER*4 channels, setChannelsNumber

	if (isStarted.eq.1) then
		setChannelsNumber = 1
		return
	end if

	N_CHEN = channels

    setChannelsNumber = 0
end function setChannelsNumber



function ProcessFrame(pData, formatVersion)
	!DEC$ ATTRIBUTES DLLEXPORT::ProcessFrame
	use GeolibGlobals

	implicit none

	INTEGER*4  ProcessFrame, formatVersion, pData
	real*4 D(90000); pointer(pD, D)

	if (isStarted.eq.0) then
		ProcessFrame = 1
		return
	end if

!	CALL log_info("FRAME PROCESS STARTED")
	call dataIncome(pData, formatVersion)
!	CALL log_info("FRAME PROCESS DONE")


	ProcessFrame = 0
end function ProcessFrame


function StopAnalysis()
	!DEC$ ATTRIBUTES DLLEXPORT::StopAnalysis
	use GeolibGlobals
    use GRAFICA
    use IniFile
    
	implicit none

	INTEGER*4  StopAnalysis, OnClose, lret, showGraphicsWindow
!
	if (isStarted.eq.0) then
		StopAnalysis = 1
		return
	end if
!
!	CALL log_info("STOPPING...")
!
	if ( OnClose()==0 ) then
!		call PostQuitMessage( 0 )
		StopAnalysis = 0
	end if

!	CALL log_info("STOPPED")
    CALL  STOP_DLL()

     if (N_PRIZNAK_GRAFICA.EQ.0) then
        lret = showGraphicsWindow(0)    
    end if
 
	isStarted = 0
    
end function StopAnalysis


! int ProcessFrame(void* data, int formatVersion); // обрабатывает очередную пачку данных, принимает указатель на буффер с данными и номер версии формата данных, возвращает 0 в случае успеха.
!int StopAnalysis(); // полностью завершает анализ, после вызова этой функции все результаты полностью готовы в папке, переданной в качестве аргумента в функцию StartAnalisys, возвращает 0 в случае 
! успеха.



function DllMain (hInstDLL, fdwReason, lpReserved)  
!DEC$ IF DEFINED(_X86_) 
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_DllMain@12' :: DllMain 
!DEC$ ELSE 
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'DllMain' :: DllMain 
!DEC$ ENDIF 

USE GeolibGlobals  
IMPLICIT NONE  
integer :: DllMain  
integer, intent(IN) :: hinstDLL  
integer, intent(IN) :: fdwReason  
integer, intent(IN) :: lpReserved 

dllHandler = hInstDLL
DllMain = 1 

end function DllMain