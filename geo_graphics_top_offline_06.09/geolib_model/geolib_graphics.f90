MODULE gl_graph_context
    
    USE gl_core

	implicit none
    
   
END MODULE gl_graph_context
    
MODULE gl_graphics
    USE gl_core

	implicit none

    ! графический контекст с данными отрисовки
    TYPE T_GRAPH_CONTEXT
        integer :: imgW ! полна€ ширина всей картинки
        integer :: imgH ! полна€ максимальна€ высота всей картинки.
        integer :: chanImgW ! размер(ширина) картинки с каналом (высота равна числу точек в трассе)
        integer :: hMainBitmap ! внеэкранна€ обща€ картинка дл€ рисовани€, 
        integer :: hMainDC ! графический контекст внеэкранной картинки,
        integer :: hDefaultWnd  ! указатель на окно дл€ рисовани€ по-умолчанию      
        integer :: isGraphicsInitialized = 1
        integer :: groupingFactor = 1
        integer init_chan_num  ! количество каналов, с которым была проинициализирована графика.
        integer init_chan_size ! количество точек в канале, с которым была проинициализирована графика.
        real*4, DIMENSION(:), ALLOCATABLE :: trace_draw_buffer ! данные каналов дл€ накоплени€+усреднени€.
        integer, dimension(:), allocatable :: channelImgDC ! контексты картинок с изображением данных калналов
        integer, dimension(:), allocatable :: channelImg   ! картинки с изображением данных калналов
        integer :: linePos = 1 ! место рисовани€ последней трассы
        real*4 :: max_palette_value = 500, min_palette_value = 500
    END TYPE T_GRAPH_CONTEXT      
    
    integer :: frameTop = 10, frameBottom = 5, frameLeft = 50, frameRight = 5 ! –амка главного окна
    integer :: channelFrame = 1 ! рамка вокруг рисунка одного канала    
    
contains

! ѕодготавливает картинку в пам€ти, где будет происходить рисование.
! inPggd - указатель на структуру с данными дл€ отрисовки.
subroutine init_graphics(hWnd, D, GC)
    use user32
    use gdi32 
	USE DFLIB
	USE DFWIN
	implicit none
    
    integer hdc, h, res, hWnd, i, totalW
    type(T_GRAPH_CONTEXT), pointer :: GC
    type(T_MISC_GEO_DATA), pointer :: D
    type (T_RECT) rect
    
    GC%imgW = GC%chanImgW + frameLeft + frameRight + channelFrame*2
    GC%imgH = (D%chan_size + channelFrame*2) * D%chan_count + frameTop + frameBottom

    hdc = GetDC(hWnd)
    
    res = DeleteObject(GC%hMainBitmap)
    res = DeleteDC(GC%hMainDC)
    
    ! —оздаЄм главное внеэкранное изображение
	GC%hMainBitmap = CreateCompatibleBitmap(hdc, GC%imgW, GC%imgH)
	GC%hMainDC = CreateCompatibleDC(hdc)
	res = SelectObject(GC%hMainDC, GC%hMainBitmap)
    
    if (ALLOCATED(GC%channelImg)) then
        do i=1, GC%init_chan_num
            res = DeleteObject(GC%channelImg(i))
            res = DeleteDC(GC%channelImgDC(i))
        end do
    end if
    
    if (ALLOCATED(GC%channelImg)) DEALLOCATE (GC%channelImg)
    if (ALLOCATED(GC%channelImgDC)) DEALLOCATE (GC%channelImgDC)
    if (ALLOCATED(GC%trace_draw_buffer)) DEALLOCATE (GC%trace_draw_buffer)
    
    ALLOCATE(GC%channelImg(D%chan_count))
    ALLOCATE(GC%channelImgDC(D%chan_count))
    ALLOCATE(GC%trace_draw_buffer(D%chan_count*D%chan_size))
    
    do i = 1, D%chan_count
       GC%channelImg(i) = CreateCompatibleBitmap(hdc, GC%chanImgW, D%chan_size)
       GC%channelImgDC(i) = CreateCompatibleDC(hdc)
       res = SelectObject(GC%channelImgDC(i), GC%channelImg(i))
    end do
    
    hdc = ReleaseDC(hWnd, hdc)
    
    rect%left = 0
    rect%right = 0
    rect%top = 100
    rect%bottom = 100

    res = MSFWIN$Rectangle(GC%hMainDC, -1, -1, GC%imgW+1, GC%imgH+1)
  
    GC%hDefaultWnd = hWnd
    GC%linePos = 0
    GC%init_chan_num = D%chan_count
    GC%init_chan_size = D%chan_size
    GC%isGraphicsInitialized = 1
    GC%trace_draw_buffer = 0.0
    
end subroutine init_graphics


subroutine reinit_graphics_if_needed(D, GC)
  use user32
  use gdi32 
  USE DFLIB
  USE DFWIN

  implicit none
  TYPE(T_GRAPH_CONTEXT), pointer :: GC
  type(T_MISC_GEO_DATA), pointer :: D

  if(GC%init_chan_num .NE. D%chan_count) then
      call init_graphics(GC%hDefaultWnd, D, GC)
      return
  end if
  
  if(GC%init_chan_size .NE. D%chan_size) then
      call init_graphics(GC%hDefaultWnd, D, GC)
      return
  end if  
  
end subroutine reinit_graphics_if_needed


! перерисовывает всю картинку на указазанном заранее окне (hDefaultWnd)
subroutine redraw_on_default_window(D, GC)  
  use user32
  use gdi32 
  USE DFLIB
  USE DFWIN

  implicit none
  type(T_GRAPH_CONTEXT), pointer :: GC
  type(T_MISC_GEO_DATA), pointer :: D
  integer hdc, lret
  
  hdc = GetDC(GC%hDefaultWnd)
  call redraw_on_dc(hdc, D, GC)
  lret = ReleaseDC (GC%hDefaultWnd, hdc)
end subroutine redraw_on_default_window


subroutine redraw_on_dc(hdc, D, GC)
  use user32
  use gdi32 
  USE DFLIB
  USE DFWIN

  implicit none
  type(T_GRAPH_CONTEXT), pointer :: GC
  type(T_MISC_GEO_DATA), pointer :: D
  integer hdc, hPen, cur_pos_to_paint, lret
  type (T_POINT) point

  if(GC%isGraphicsInitialized.eq.0) then
	return
  end if
  
  IF (.NOT.ASSOCIATED(D)) then
	return
  end if      
  
     lret = BitBlt(hdc, 0, 0, GC%imgW, GC%imgH, GC%hMainDC, 0, 0, SRCCOPY);
    
     hPen = CreatePen( PS_DOT, 1, RGB(0, 0, 200) )       ! рисование курсора 
	 lret = selectObject(hdc, hPen)
	 lret = MoveToEx(hdc, 0, 0, point)
	 lret = MSFWIN$LineTo(hdc, 300,  300)
	 lret = deleteObject(hPen)
     
end subroutine redraw_on_dc
    

! ¬ызываетс€, когда по€вилась нова€ трасса, 
! и над этой трассой произведены необходимые вычислени€.
! ќповечает графику о том, что возможно надо обновить рисунки каналов.
subroutine onNextTrace(D, GC)
  use user32
  use gdi32 
  USE DFLIB
  USE DFWIN

  implicit none
  TYPE(T_GRAPH_CONTEXT), pointer :: GC
  type(T_MISC_GEO_DATA), pointer :: D
  integer hdc, y, i
  integer hFont, hPen, lret
  integer getColorForValue	! окраска балласта
  real*4, dimension(:), pointer :: oneChannelData
  type (T_POINT) point

    if (.NOT.ASSOCIATED(D)) then
	    return
    end if
    
    call reinit_graphics_if_needed(D, GC)
    
	y = D%trace_count
    GC%trace_draw_buffer = GC%trace_draw_buffer + D%new_trace
    
    ! «аписали трассу в буфер и выходим, если не набрали достаточно трасс дл€ группировки.
    if ( MOD(D%trace_count, GC%groupingFactor) .NE. 0 ) RETURN
    
    GC%trace_draw_buffer = GC%trace_draw_buffer/GC%groupingFactor
    
    ! ¬ызываем поканальную отрисовку.
    DO i=1, D%chan_count
        call drawOneChannelTrace(GC%channelImgDC(i), i, D, GC)  
    END DO
    
    DO i=1, D%chan_count
        lret = BitBlt(GC%hMainDC, frameLeft + channelFrame,  frameTop + (channelFrame*2 + GC%init_chan_size)*(i-1)+channelFrame, &
            GC%chanImgW, GC%init_chan_size, GC%channelImgDC(i), 0, 0, SRCCOPY);
    END DO
    
    GC%linePos = GC%linePos+1
end subroutine onNextTrace



subroutine drawOneChannelTrace(hdc, chanNum, D, GC)
	USE DFWIN
    
    implicit none
    TYPE(T_GRAPH_CONTEXT) :: GC
    type(T_MISC_GEO_DATA), pointer :: D
    integer :: hdc, i, lret, valPos, chanNum
    real*4 :: val

    do i=1,D%chan_size
        valPos = (chanNum-1)*D%chan_size + i
        val = GC%trace_draw_buffer(valPos)
        lret = MSFWIN$SetPixel(hdc, GC%linePos, i-1, getColorForValue(val, GC) )    
    end do
end subroutine drawOneChannelTrace



function getColorForValue(val, GC)		! преобразование значени€ точки трассы в цвет
	USE DFWIN

	implicit none
	integer*4 getColorForValue, byteVal
    TYPE(T_GRAPH_CONTEXT) :: GC
	real*4 val

	if (val .GE. GC%max_palette_value) then
		getColorForValue = 0
		return	
    end if

	if (val .LE. GC%min_palette_value) then
		getColorForValue = RGB(255, 255, 255)
		return	
    end if
    
    byteVal = (val - GC%min_palette_value) * 255
	getColorForValue = RGB(byteVal, byteVal, byteVal)

end function getColorForValue



END MODULE gl_graphics
