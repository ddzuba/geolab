!----------------------------------------------------------------------
! Структуры, используемые при работе с bitmap - изображением
! Созданы на основе Win32 SDK для тех версий транслятора с 
! FORTRAN, в которых они могут отсутствовать   
!----------------------------------------------------------------------
module BitmapSpecificStruct
use dfwina
!DEC$ PACK:1
type BMP_RGBQUAD
	sequence
	BYTE rgbBlue;	
	BYTE rgbGreen;
	BYTE rgbRed;
	BYTE rgbReserved;
end type BMP_RGBQUAD

type BMP_CIEXYZ
	sequence
    integer*4 ciexyzX;
    integer*4 ciexyzY;
    integer*4 ciexyzZ;
end type BMP_CIEXYZ

type BMP_CIEXYZTRIPLE
	sequence
    type(BMP_CIEXYZ) ciexyzRed;
    type(BMP_CIEXYZ) ciexyzGreen;
    type(BMP_CIEXYZ) ciexyzBlue;
end type BMP_CIEXYZTRIPLE

type BMP_BITMAPINFOHEADER4
	sequence
    integer*4 biSize;
    integer*4 biWidth;
    integer*4 biHeight;
    integer*2 biPlanes;
    integer*2 biBitCount;
    integer*4 biCompression;
    integer*4 biSizeImage;
    integer*4 biXPelsPerMeter;
    integer*4 biYPelsPerMeter;
    integer*4 biClrUsed;
    integer*4 biClrImportant;
    integer*4 biRedMask;
    integer*4 biGreenMask;
    integer*4 biBlueMask; 
    integer*4 biAlphaMask;   
	integer*4 biCSType;
	type(BMP_CIEXYZTRIPLE) biEndPoints;
    integer*4 biGammaRed;
    integer*4 biGammaGreen;
    integer*4 biGammaBlue; 
end type BMP_BITMAPINFOHEADER4

type BMP_BITMAPINFOHEADER
	sequence
    integer*4 biSize;
    integer*4 biWidth;
    integer*4 biHeight;
    integer*2 biPlanes;
    integer*2 biBitCount;
    integer*4 biCompression;
    integer*4 biSizeImage;
    integer*4 biXPelsPerMeter;
    integer*4 biYPelsPerMeter;
    integer*4 biClrUsed;
    integer*4 biClrImportant;
end type BMP_BITMAPINFOHEADER

type BMP_BITMAPINFO
	sequence
    type(BMP_BITMAPINFOHEADER4) bmiHeader
    type(BMP_RGBQUAD) bmiColors(1)
end type BMP_BITMAPINFO

type BMP_BITMAPFILEHEADER
	sequence
    integer*2 bfType;
    integer*4 bfSize;
    integer*2 bfReserved1;
    integer*2 bfReserved2;
    integer*4 bfOffBits;
end type BMP_BITMAPFILEHEADER
!DEC$ PACK:
end module BitmapSpecificStruct


!#######################################################################
!
! SaveBitmapValues - модуль с переменными для сохранения скриншота
!
!#######################################################################
module SaveBitmapValues
implicit none 

	integer*4 last_save /0/

end module SaveBitmapValues


! Основная функция, которая сохраняет рисунки
subroutine SaveBitmapToFile(hDC, hBmp, FileName)

use BitmapSpecificStruct
USE  RAILWAY
implicit none

integer*4 hWnd, RasterOperationCode, IO_Unit
character*(*)                                 FileName
type(T_RECT)                                  SaveRect
type(BMP_BITMAPINFO)                          internal_str
type(BMP_BITMAPFILEHEADER)                    hdr
byte                                          tmpbyte
byte, allocatable::pBits(:)
integer*4                                     hDC, hBmp, w, h, iresult
integer*4 iBmpHeaderLength, iquadLength, iBmpFileHeaderLength, iret, ierr, isizBMI, imem, j


interface
integer*4 function BMP_GetDIBits(dummy0, dummy1, dummy2, dummy3, &
                                 dummy4, dummy5, dummy6)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_GetDIBits@28' :: BMP_GetDIBits
use dfwina
integer*4         dummy0
integer*4         dummy1
integer*4         dummy2
integer*4         dummy3
integer*4         dummy4
integer*4         dummy5
integer*4         dummy6
end function BMP_GetDIBits
end interface

IO_Unit = 40

iresult = 0
 !-----------------------------------------------------------------------------------------------------------
 ! Вычисляем размерности структур, которые будут использоваться
 !-----------------------------------------------------------------------------------------------------------
iBmpHeaderLength         = sizeof(internal_str%bmiHeader)
iquadLength                   = sizeof(internal_str%bmiColors(1))
iBmpFileHeaderLength   = sizeof(hdr)
!-------------------------------------------------------------------------------------------------------------
! Определяем количество цветов в изображении
!-------------------------------------------------------------------------------------------------------------
internal_str%bmiHeader%biSize                = 40 ! Обязательное значение
internal_str%bmiHeader%biBitCount          = 0   ! не получать таблицу цветов
iret = BMP_GetDIBits(hDC, hBmp, 0, 0, NULL,							&
                              LOC(internal_str), DIB_RGB_COLORS)
if(iret.eq.0) then  ! невозможно распознать структуру
                        ! bitmap-изображения
       iresult = 1
   goto 3
endif
!--------------------------------------------------------------------------------------------------------------
! Распределяем память для собственноо изображения (байт данных)
!--------------------------------------------------------------------------------------------------------------
ierr = 0
allocate(pBits(internal_str%bmiHeader%biSizeImage),  STAT=ierr)
if(ierr.ne.0) then ! невозможно выделить память для bitmap
     iresult = 2
goto 3
endif
!--------------------------------------------------------------------------------------------------------------
! Вычисляем количество памяти для структуры BITMAPINFO
!--------------------------------------------------------------------------------------------------------------
select case(internal_str%bmiHeader%biBitCount)
      case(24)
         isizBMI = iBmpHeaderLength
      case(16, 32)
         isizBMI = iBmpHeaderLength + 12
      case default
                 isizBMI = iBmpHeaderLength +                             &
                    iquadLength*(2**internal_str%bmiHeader%biBitCount)
end select
!-------------------------------------------------------------------------------------------------------------
!  Распределяем память для этой структуры
!-------------------------------------------------------------------------------------------------------------
imem = GlobalAlloc(IOR(GMEM_FIXED, GMEM_ZEROINIT), isizBMI+4)
if(imem.eq.0) then                   ! невозможно выделить память
                                     ! структуры bitmapinfo
          iresult = 3
       goto 2
endif
!------------------------------------------------------------------------------------------------------------
! Копируем заполненные поля в шаблон в памяти
!------------------------------------------------------------------------------------------------------------
call CopyMemory(imem, loc(internal_str), iBmpHeaderLength)
!------------------------------------------------------------------------------------------------------------
! Заполняем поля в BitmapFileHeader
!------------------------------------------------------------------------------------------------------------
hdr%bfType                      = #4D42                                   ! 'BM'
hdr%bfSize                      = iBmpFileHeaderLength +                  &
                                         iBmpHeaderLength +               &
                                         isizBMI +                        &
                                         internal_str%bmiHeader%biSizeImage
hdr%bfReserved1 = 0
hdr%bfReserved2 = 0
hdr%bfOffBits      = iBmpFileHeaderLength+isizBMI

!---------------------------------------------------------------------------------------------------------------
! Начинаем запись в файл
!---------------------------------------------------------------------------------------------------------------
IF(IFL_WRITER.EQ.1) THEN
open(unit=IO_Unit, file=FileName, form='binary')
     write(IO_Unit) hdr
     iret = BMP_GetDIBits(hDC, hBmp, 0,                                  &
                                       internal_str%bmiHeader%biHeight,        &
                                       LOC(pBits), imem, DIB_RGB_COLORS)
     if(iret.eq.0)  then          ! невозможно получить данные для
                                      ! bitmap-изображения
                  iresult = 4
          goto 1
      endif
  do j=1,   isizBMI
      call Copymemory(loc(tmpbyte), imem+(j-1), 1)
      write(IO_Unit) tmpbyte
enddo
    write(IO_Unit)   pBits
1  continue
close(IO_Unit)
END IF
!----------------------------------------------------------------------------------------------------------------
! Освобождаем память для этой BITMAPINFO
!----------------------------------------------------------------------------------------------------------------
    iret = GlobalFree(imem)
2  continue
!----------------------------------------------------------------------------------------------------------------
! Освобождаем память для собственного изображения (байт данных)
!----------------------------------------------------------------------------------------------------------------
     deallocate(pBits)
3   continue

return
end


function saveCurrentImage()
use FReciverGlobals
use PARAM_
USE RAILWAY

implicit none
integer*4 saveCurrentImage
character(200)  getBitmapFileName

	if(IFL_WRITER.EQ.1) then
	CALL CALC_NAME() ! ФОРМИРУЕМ НОВОЕ ИМЯ
	call SaveBitmapToFile(hMainDC, hMainBitmap, NAME_FILE)
	end if

	saveCurrentImage = 0

end function

function checkAndSaveScreenshot()
use FReciverGlobals
use SaveBitmapValues
use Fortran_WinPrint
USE PARAM_
USE PARAM_1
implicit none
integer*4 saveCurrentImage, checkAndSaveScreenshot, lres
	
	last_save = last_save + 1

	if (last_save .ge. img_w-45) then 
		lres = saveCurrentImage()
		if(IFLAG_PRINT.NE.0) lres = Print_Bitmap(.FALSE.) ! ПЕЧАТЬ
		last_save = 0
	end if
 checkAndSaveScreenshot=0	
end function