========================================================================
    Fortran Dll : "geolib" Project Overview
========================================================================

The Intel Fortran Dll Wizard has created this 
"geolib" project for you as a starting point.

This file contains a summary of what you will find in each of the files 
that make up your project.

geolib.vfproj
    This is the main project file for Fortran projects generated using an 
    Application Wizard.  It contains information about the version of 
    Intel Fortran that generated the file, and information about the 
    platforms, configurations, and project features selected with the 
    Application Wizard.

geolib.f90
    This is the main source file for the Fortran Dll. 

/////////////////////////////////////////////////////////////////////////////
Other notes:

/////////////////////////////////////////////////////////////////////////////
integer function MainWndProc ( hWnd, mesg, wParam, lParam )
case (WM_COPYDATA)
	call sendTrace(pMyData, coord) ! ОПХЕЛ РПЮЯЯШ
		SUBROUTINE sendTrace(pData, opt)
		lres = ProcessFrame(pData, opt)
	function ProcessFrame(pData, formatVersion)
	call dataIncome(pData, formatVersion)
		subroutine dataIncome(pData, formatVersion)
		CALL INTEGRAL_INPUT(MyData)
	SUBROUTINE INTEGRAL_INPUT(MyData)
	! оепбне времхе. гюонлхмюел щрхйерйс х ббндхл оюпюлерпш
	CALL FIRST_READING()
	! ялемю щрхйерйх б опнжеяяе пюанрш
	CALL SECOND_READING() 
		SUBROUTINE FIRST_READING()
		CALL FIRST_READ()			
		! гюдюмхе нямнбмшу оюпюлерпнб хг тюикю
		! пюглеыюел пюанвхе люяяхбш б опнжедспе POINT_ARRAY
	SUBROUTINE FIRST_READ()
	call ReadConfigFromIni()
		CALL POINT_ARRAY()
		! тнплхпсел оюойх дкъ гюохях пхясмйнб Х рейярнбшу тюикнб
case (IDM_SEND100)
        CALL SendSampleTrace(100)										SUBROUTINE SendSampleTrace(count)
		integer*2 :: trace_data(5717)
		CHARACTER*1 :: trace_sample(5717) 
		trace_sample(N) = char(trace_data(N))
		call sendTrace(LOC(trace_sample), 0)							SUBROUTINE sendTrace(pData, opt)
	lres = ProcessFrame(pData, opt)
		function ProcessFrame(pData, formatVersion)
		call dataIncome(pData, formatVersion)
	subroutine dataIncome(pData, formatVersion)
	CALL INTEGRAL_INPUT(MyData)