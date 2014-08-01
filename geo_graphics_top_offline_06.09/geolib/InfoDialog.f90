
subroutine showInfoDalog()
	use dfwin
	use dflogm
	use DFLIB
	use FReciverGlobals
    include 'resource.fd'
	logical(4) flag

	integer*4 i

!	character(200) CT(10)
!	character(200*10) REZTEXT
	character*100 OUTt

character*10 s1, s2, s3

character*2 CRLF 

	external ConsoleSub
	
	CRLF = CHAR(13)//CHAR(10)

	flag = DlgModeless(InfoDialog, SW_SHOWNORMAL, ghwndMain)
!	flag = DlgSet(InfoDialog, IDC_EDIT_INFO, OUTt)
!	flag = DlgModal(InfoDialog)

!	call CenterWindow (InfoDialog%HWND, ghwndMain)
end

subroutine TextPass( text )

	
	character*200 text

end



subroutine ConsoleOut( text )
	use dfwin
	use dflogm
	use DFLIB
	use FReciverGlobals
    include 'resource.fd'
	logical(4) flag

	integer cc(200)

	integer*4 i, e
	character*200 text


	character*2 CRLF 
	CRLF = CHAR(13)//CHAR(10)


	flag = DlgGet(InfoDialog, IDC_EDIT_INFO, CUR_CONSOLE)

	CUR_CONSOLE = CUR_CONSOLE(2:LEN_TRIM(CUR_CONSOLE))

	i = INDEX(text, CHAR(0)) - 1
	if(i <= 0) i = LEN_TRIM(text)
!	WRITE(CT(ConsolePos),*)text(1:i)
!	CT(ConsolePos) = text(1:i)


	e = LEN_TRIM(text(1:i))+LEN_TRIM(CUR_CONSOLE)
	if( e.ge.CONSOLE_LIMIT ) then
		call ClearConsole()
		return
	end if


	WRITE(REZTEXT,*)TRIM(CUR_CONSOLE),CRLF,TRIM(text(1:i))


	flag = DlgSet(InfoDialog, IDC_EDIT_INFO, REZTEXT)

end


subroutine ClearConsole()

	use dfwin
	use dflogm
	use DFLIB
	use FReciverGlobals
    include 'resource.fd'

	logical(4) flag

	flag = DlgSet(InfoDialog, IDC_EDIT_INFO, "")


end


SUBROUTINE ConsoleSub( dlg, control_name, callbacktype )
	USE DFLOGM
	IMPLICIT NONE
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	CHARACTER(256) text
	INTEGER cel, far, retint
	LOGICAL retlog
	INTEGER local_callbacktype
	local_callbacktype = callbacktype



end subroutine ConsoleSub



