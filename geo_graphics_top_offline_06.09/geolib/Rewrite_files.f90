integer function Rewrite_files()
use FReciverGlobals
use Fortran_WinPrint
USE PARAM_
USE PARAM_1
USE N_
USE RAILWAY

implicit none 
integer lret, bmp_to_jpg, make_FOLDER, COPY_FILE, zip_folder, DELETE_FOLDER
CHARACTER*10 line1
INTEGER(4) KM

LINE1='          '
KM=kilometr
write(line1,'(I4)')KM

!	IFL_WRITER=1			! гюохяэ пхясмйнб
!	IFL_WRITER_TEXT=1		! гюохяэ онкнфемхъ лшьх опнхгбндхрэ
!	IFL_WRITER_KASKAD=1		! гюохяэ дкъ йюяйюдю опнхгбндхрэ

! опенапюгсел пхясмйх х оепеохяшбюел тюик лшьх


IF(	IFL_WRITER.EQ.1)THEN
NAME_FILE_IMG_JPEG=TRIM(NAME_FILE_IMG)//'_'//TRIM(ADJUSTL(LINE1))//'_JPEG'
lret = make_FOLDER(DISK,NAME_FILE_IMG_JPEG)
lret = bmp_to_jpg(DISK,NAME_FILE_IMG, NAME_FILE_IMG_JPEG) 
	! оепеохяшбюел тюик  лшьх
    IF(	IFL_WRITER_TEXT.GE.1)THEN
	lret = COPY_FILE(DISK,NAME_FILE_IMG,NAME_FILE_IMG_JPEG,FILE_TEXT) ! йнохпсел тюик лшьх
	END IF
END IF

! гхосел тюик йюяйюдю 
IF(	IFL_WRITER_KASKAD.EQ.1)THEN
NAME_FILE_GEO_ZIP=TRIM(NAME_FILE_GEO)//'_'//TRIM(ADJUSTL(LINE1))//'.zip'
lret = zip_folder(DISK,NAME_FILE_GEO, NAME_FILE_GEO_ZIP)
!CALL DELETE_FOLDER(DISK,NAME_FILE_GEO) !сдюкъел мегхонбюммсч оюойс
END IF

! смхврнфюел оюойх
close(8)	! INFO.XML
!close(9)	! All_information
close(10)	! GRAF
close(20)	! VLAG
close(30)	! UGLUB
close(31)	! TOLSH
close(40)	! пхясмйх
lret = DELETE_FOLDER(DISK,NAME_FILE_GEO)
IF(	IFL_WRITER.EQ.1) lret = DELETE_FOLDER(DISK,NAME_FILE_IMG)
!------------------------------------------------------------------------
Rewrite_files = 0

end function

!open(unit=100, FILE='D:/OUTPUT.OUT')
!write(100,*)

