module logger
	integer  logInited / 0 /
end module logger

SUBROUTINE log_info(TEXT)										
!USE PARAM_
USE logger
USE FReciverGlobals
IMPLICIT NONE	
	CHARACTER (*) :: TEXT
	INTEGER*4 :: F

!	if (logInited.eq.0) then
!		call make_folder("c:", "geolog")
!		logInited = 1	
!	end if

!	F = 59870

!	open(UNIT=F, FILE="c:\\geolog\\info.log",  ACCESS='APPEND')

!	write (F,*) TRIM(TEXT)


!	close(F)

RETURN
END SUBROUTINE log_info