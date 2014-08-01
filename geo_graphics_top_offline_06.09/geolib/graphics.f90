
subroutine CreateBufBitmap(hdc)           ! �������� � ������ �������� 
										  ! ��������� ���� ��� � ������ ������	
    use user32
    use gdi32 
	use FReciverGlobals
	USE DFLIB
	USE DFWIN

	implicit none
	integer hdc, res, lret
    type (T_RECT) rect
! img_w - ������ �������, img_h - ������ �������
	hMainBitmap = CreateCompatibleBitmap(hdc, img_w, img_h)	
	hMainDC = CreateCompatibleDC(0)
	res = SelectObject(hMainDC, hMainBitmap)

	hBackBitmap = CreateCompatibleBitmap(hdc, img_w, img_h)	
	hBackDC = CreateCompatibleDC(hdc)
	res = SelectObject(hBackDC, hBackBitmap)

  rect%left = 0
  rect%right = 0
  rect%top = 100
  rect%bottom = 100

  lret = MSFWIN$Rectangle(hMainDC, -1, -1, img_w+1, img_h+1)
end

! �������������� ���� ���� �� ������
!--------------------------------------------------------------------
subroutine redrawScreen()  
  use user32
  use gdi32 
  use FReciverGlobals

  USE DFLIB
  USE DFWIN


  implicit none
  integer hdc, lret
  
  hdc = GetDC (ghwndMain)
  call repaint(hdc)
  lret = ReleaseDC (ghwndMain, hdc)

end

subroutine repaint(hdc)
 
  use user32
  use gdi32 
  use FReciverGlobals

  USE DFLIB
  USE DFWIN
  USE PARAM_1

  implicit none
  integer hdc, hPen, cur_pos_to_paint, lret
  type (T_POINT) point



 if(isFirst.gt.0) then
	return
 end if

 lret = BitBlt(hdc, 0, 0, img_w, img_h, hMainDC, 0, 0, SRCCOPY);


 cur_pos_to_paint = cursor_pos - TRACE_COUNT/N_PRINT + img_w
 if (TRACE_COUNT/N_PRINT.lt.img_w) then
	cur_pos_to_paint = cursor_pos
 end if

if (cur_pos_to_paint.ge.0) then
 	hPen = CreatePen( PS_DOT, 1, RGB(0, 0, 200) )       ! ��������� ������� 
	lret = selectObject(hdc, hPen)
	lret = MoveToEx(hdc, cur_pos_to_paint, 0, point)
	lret = MSFWIN$LineTo(hdc, cur_pos_to_paint,  + img_h)
	lret = deleteObject(hPen)
end if

end

!------------------------------------------------------------------------------------------

subroutine paintNext()		! ������ ��������� ������

  use user32
  use gdi32 
  use FReciverGlobals
  use PARAM_
  use PARAM_1
  use RAILWAY
  USE DFLIB
  USE DFWIN
  USE N_

  implicit none
  integer hdc, y, i,I1,I2,JJ1,jj2,jj3, KK,r1, g1, b1, r2, g2, b2, pixel, v_sh
  integer hFont, hPen, lret
  integer getColorForValue	! ������� ��������
  integer getColorForValue1	! ������� �������������� ���������
  integer getColorForValue2	! ������� �����
  integer getColorForValue3	! ������� ���������� �����
  integer ii
  integer ii1
  integer ii2
  type (T_POINT) point
  	
  character*200 str_ntr,str_ntr1,str_ntr2
!----------------------------------------------------------------------------------------
! ����� ������ ������� ������
!----------------------------------------------
!----------------------------------------------

!JSDVIG_TR=(N_CHEN_TEK-1)*(n) ! n=SIZE-1 - �� ����� N_1
							   ! 
!--------------------------------------------
! ����� �������� �� ���� �������          
!--------------------------------------------
IF(N_CHEN_TEK.eq.1) THEN		
  do ii=1,out_trassa
	y = TRACE_POS
	if ( TRACE_POS > img_w-46 ) then 
	lret = BitBlt(hBackDC, -1, 0, img_w, img_h, hMainDC, 0, 0, SRCCOPY);
	do	i = 0, img_h
		lret = MSFWIN$SetPixel(hBackDC, img_w-1, i, RGB(255, 255, 255))
	end do
	hdc = hMainDC;
	hMainDC = hBackDC;
	hBackDC = hdc;
     end if
  end do
  if(TRACE_POS .le. img_w-45)then 
  TRACE_POS=TRACE_POS+out_trassa-1
  end if
END IF

!----------------------------------------------------------------------------
IF(TRACE_POS > img_w-46) THEN ! ���������� ��� ���������� ������������
    y = img_w - 46
ELSE
    y = TRACE_POS
END IF
!-----------------------------------------------------------------------------
  hdc = hMainDC

IF(ITEK.GE.LM_WINDOW)  THEN ! ��������� ������ � ������
!--------------------------------------------
! ������� ���������� ���� 
!--------------------------------------------
	do i = j_level+1,I_B
		lret = MSFWIN$SetPixel(hdc, y, trace_draw_pos(N_CHEN_TEK)+i-j_level, getColorForValue(MAX_VAL) )
	end do
!----------------------------------------------
!	������ ������������� ��������� 
!----------------------------------------------

	do i = I_B, I_PB ! ��������� ������ ���� ����� 5 ��
	    lret = MSFWIN$SetPixel(hdc, y, trace_draw_pos(N_CHEN_TEK)+i-j_level, getColorForValue1(MAX_VAL) )
	end do

!----------------------------------------------
!	������ ����� 
!----------------------------------------------
!
	do i = I_PB+1, JJ2_GR 
	    lret = MSFWIN$SetPixel(hdc, y, trace_draw_pos(N_CHEN_TEK)+i-j_level, getColorForValue2(MAX_VAL) )
	end do
!----------------------------------------------
! �������� ����� �����������
!----------------------------------------------
	MIN_VAL =0.
    do i = j_level,JJ2_GR
    if(min_val.lt. trace_buf(i+JSDVIG_TR)) then
        min_val=trace_buf(i+JSDVIG_TR)
        i1=i
        end if
    end do
    if(i1.lt.JJ2_GR-1)trace_buf(i1+1+JSDVIG_TR)=min_val
	do i = j_level,JJ2_GR
	IF(trace_buf(i+JSDVIG_TR).gt.0.) THEN					! �������� � ������ �����
	 lret = MSFWIN$SetPixel(hdc, y, trace_draw_pos(N_CHEN_TEK)+i-j_level, getColorForValue3(trace_buf(i+JSDVIG_TR),min_val) )
	end if
	end do
!----------------------------------------------

END IF
!----------------------------------------------------------------------------------------
! ��������� �������� ������
!----------------------------------------------
!----------------------------------------------------------------------------------------

!! ������� ��������� �� 90 ��������
!!hfont = CreateFont(12,0,900,0,FW_BOLD,TRUE, FALSE, 0, RUSSIAN_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH+FF_SWISS, "Arial"C)
!!������� �������������� 
hfont = CreateFont(18,0,0,0,FW_BOLD,TRUE, FALSE, 0, RUSSIAN_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH+FF_SWISS, "Arial"C)
lret = SelectObject(hdc, hfont)
lret = deleteObject(hfont)	! ������ �������� ������
!----------------------------------------------------------------------------------------
! ��������� ������ ������
!----------------------------------------------------------------------------------------

!IF(N_CHEN_TEK.eq.1) ITK=ITK+N_PRINT			! ����� ������  �������������/��������� ������	 
IF(N_CHEN_TEK.eq.1) THEN					! �������� ������ ��� ������� ������
	WRITE(str_ntr, '(I10)') TRACE_COUNT
	lret = MSFWIN$SetTextColor (hdc, 0)
	lret = TextOut(hdc, 0, 0, " ����� ������", 13) !����� ������ ������
	lret = TextOut(hdc, 200, 0, str_ntr, 10)		!����� ������ ������
END IF
IF(ITEK.GE.LM_WINDOW)  THEN

!---------------------------------------------------------------------------------
! ��������� ������������� �����������
!---------------------------------------------------------------------------------
9   lret = TextOut(hdc,0,trace_draw_pos_1(N_CHEN), " ���������", 10)
	IF(REFR(N_CHEN_TEK).EQ.0.OR.ITEK.LT.LM_WINDOW) THEN			! ��� ����������� �������	� ���������� � ������ (���� 1000)
		do jj1=1,10
		    lret = MSFWIN$SetPixel(hdc, y, trace_draw_pos_2(N_CHEN_TEK)+JJ1, RGB(255,255, 255)) ! ����� (��� ���������)
		end do
		GOTO 11
	END IF

IF(N_CHEN_TEK.EQ.3) then
    if(REFR(N_CHEN_TEK).GT.REFR_MIDL(N_CHEN_TEK)*R_POSIB1) then
else
   
end if
end if
    
    IF(KL_FORPRINT(N_CHEN_TEK).EQ.1.OR.KL_FORPRINT(N_CHEN_TEK).EQ.3) THEN                   ! ������ ����� ����� ������
        DO JJ1=1,10
        lret = MSFWIN$SetPixel(hdc, y, trace_draw_pos_2(N_CHEN_TEK)+JJ1, RGB(0,0, 125))     ! ����������
        END DO
        GOTO 11
    END IF 
    
     IF(KL_FORPRINT(N_CHEN_TEK).EQ.2) THEN                                                      ! ������� ����� ������
        I1=1; I2=FIN1(N_CHEN_TEK)-STAR1(N_CHEN_TEK)+1
        DO I=I1,I2
        DO JJ1=1,10
        lret = MSFWIN$SetPixel(hdc, y-I+1, trace_draw_pos_2(N_CHEN_TEK)+JJ1, RGB(0,0, 125))     ! ���������� 
        END DO
        END DO
        GOTO 11
    END IF        
    
    IF(KL_FORPRINT(N_CHEN_TEK).EQ.4.OR.KL_FORPRINT(N_CHEN_TEK).EQ.5) THEN                       ! ������ �� �����
        GOTO 11
    END IF 
   
    IF(KL_FORPRINT(N_CHEN_TEK).EQ.6) THEN                                                       ! ������� ����� �����
        I1=1; I2=FIN1(N_CHEN_TEK)-STAR1(N_CHEN_TEK)+1
        DO I=I1,I2
        DO JJ1=1,10
        lret = MSFWIN$SetPixel(hdc, y-I, trace_draw_pos_2(N_CHEN_TEK)+JJ1, RGB(255,255,255))    ! ������ ����� ������
        END DO
        END DO
        GOTO 11
    END IF 
    
!---------------------------------------------------------------------------------
! ��������� �������
!---------------------------------------------------------------------------------
11	lret = TextOut(hdc, 0, trace_draw_pos_3(N_CHEN), " ���������������", 16)
	IF(RMOM(N_CHEN_TEK).EQ.0.OR.ITEK.LT.LM_WINDOW) THEN	
		do jj1=1,10
		    lret = MSFWIN$SetPixel(hdc, y, trace_draw_pos_4(N_CHEN_TEK)+JJ1, RGB(255,255,255)) ! ����� (��� ���������)
		end do
	GOTO 12
    END IF
    
	IF(RMOM(N_CHEN_TEK).GE.RMOM_MIDL(N_CHEN_TEK)*R_MOMENT1) THEN	                                            ! �������� ���� ������
            IF(RMOM(N_CHEN_TEK).GE.RMOM_MIDL(N_CHEN_TEK)*R_MOMENT1+(RMOM_MAX(N_CHEN_TEK)-RMOM_MIDL(N_CHEN_TEK)*R_MOMENT1)/3) THEN	! �������� ����� �������        
		        do jj1=1,10
		        lret = MSFWIN$SetPixel(hdc, y, trace_draw_pos_4(N_CHEN_TEK)+JJ1, RGB(0,0,0)) ! ������
		        end do
            ELSE
                IF(RMOM(N_CHEN_TEK).GE.RMOM_MIDL(N_CHEN_TEK)*R_MOMENT1+(RMOM_MAX(N_CHEN_TEK)-RMOM_MIDL(N_CHEN_TEK)*R_MOMENT1)/5) THEN
                do jj1=1,10
		        lret = MSFWIN$SetPixel(hdc, y, trace_draw_pos_4(N_CHEN_TEK)+JJ1, RGB(220,220, 220)) ! �����������
                end do
                END IF
	        END IF
	END IF

12  CONTINUE
END IF
!---------------------------------------------------------------------------------
! ��������� ����� ������� 
!---------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------
! �������� �����
!----------------------------------------------------------------------------------------
            IF(FLAG_HH.NE.0) THEN
                DO JJ1=1,I_HH
                    IF(IHH_50(JJ1).LE.img_hh) THEN
                    lret = MSFWIN$SetPixel(hdc,y,trace_draw_pos(N_CHEN_TEK)+IHH_50(JJ1),RGB(0,0,250)) ! ������� ��������� �����
                    END IF
                END DO
            END IF
!----------------------------------------------------------------------------------------
! �������� �������� ����� �������� � �������������� ���������
!----------------------------------------------------------------------------------------
            IF(FLAG_WINDOW.NE.0) THEN
	        lret = MSFWIN$SetPixel(hdc,y,trace_draw_pos(N_CHEN_TEK)+JJ2_B_0-J_LEVEL,RGB(250,0,0)) ! ������� ��������� �����
            lret = MSFWIN$SetPixel(hdc,y,trace_draw_pos(N_CHEN_TEK)+JJ2_PB_0-J_LEVEL,RGB(250,0,0)) ! ������� ��������� �����
            END IF    
!---------------------------------------------------------------------------------
! ������ �����
!----------------------------------------------------------------------------------
DO JJ1=1,I_HH
        IF(IHH_50(JJ1).LE.img_hh) THEN
        DO II=1,5    
        lret = MSFWIN$SetPixel(hdc,II,trace_draw_pos(N_CHEN_TEK)+IHH_50(JJ1),RGB(0,0,0)) ! ������� ��������� �����
        END DO
        END IF
END DO
!----------------------------------------------------------------------------------
! ����������� �����
!--------------------------------------------------------------------------------- 
R_LEV=0.
	        WRITE(str_ntr, '(F5.2)') R_LEV	             ! ���������� ����� � ������
	        str_ntr=TRIM(TRIM(str_ntr)//' m')
	        lret = TextOut(hdc, 6, trace_draw_pos(N_CHEN_TEK)+IHH_50(1)-nfnt/2-1, str_ntr, 6)
II=1
DO JJ1=2,I_HH
        IF(IHH_50(JJ1).LE.img_hh) THEN
            IF(II.EQ.HH_RAB) THEN
            R_LEV=R_LEV+HH_STEP
            WRITE(str_ntr, '(F5.2)') R_LEV	             ! ���������� ����� � ������
	        str_ntr=TRIM(TRIM(str_ntr)//' m')
	        lret = TextOut(hdc, 6, trace_draw_pos(N_CHEN_TEK)+IHH_50(JJ1)-nfnt/2-1, str_ntr, 6)
            II=1
            ELSE
            II=II+1
            END IF
        END IF
END DO
!----------------------------------------------------------------------------------------
! ��������� �������� ������
!----------------------------------------------
!----------------------------------------------------------------------------------------
!������������ ������
!hfont = CreateFont(12,0,900,0,FW_BOLD,TRUE, FALSE, 0, RUSSIAN_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH+FF_SWISS, "Arial"C)
! �������������� ������
hfont = CreateFont(12,0,0,0,FW_BOLD,TRUE, FALSE, 0, RUSSIAN_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH+FF_SWISS, "Arial"C)
lret = SelectObject(hdc, hfont)
lret = deleteObject(hfont) ! ������ �������� ������
!----------------------------------------------
! ��������� ������������ �������
!----------------------------------------------
IF (N_CHEN_TEK.EQ.N_CHEN) THEN  ! ������������ ����� �� ���������� ������, 
								! ����� ��������� ������� ���� ���
	i=metr/100
	ii=i*100
if(int4(metr-ii).le.1.and.(metr_old_.ge.ii+1.or.metr_old_.le.ii-1)) then
	metr_old_=i*100
	DO jj1=1,img_h								  ! ���������� ������������ �����
	    lret = MSFWIN$SetPixel(hdc, y, JJ1, RGB(250, 0, 0)) 
	END DO

!	piket=kilometr+metr/1000.		
	WRITE(str_ntr1, '(i4)') INT4(kilometr)	! ���������� ����� � ������
	WRITE(str_ntr2, '(i4)') INT4(ii)		! ���������� ����� � ������
!	str_ntr=TRIM('��'//TRIM(str_ntr1)//'; � '//TRIM(str_ntr2))
	str_ntr=TRIM(TRIM(str_ntr1)//' ��;'//TRIM(str_ntr2)//' �')
	lret = MSFWIN$SetTextColor (hdc, speed_gr_color)  ! ������������� ����
	IF (y.LT.img_w-45) THEN 
! ������� -50 � ������� 180 �� 160 ��� �������������� �������											! ����� ������� ����� �������������� ���
!	CALL TextOut(hdc, y-JSDVIG,180,str_ntr,14)		! ����� ������
ii1=y-JSDVIG-12 !-50
	lret = TextOut(hdc, ii1,30,str_ntr,14)		! ����� ������
	ELSE
! ������� -50 � ������� 180 �� 160 ��� �������������� �������											! ����� ������� ����� �������������� ���
!	CALL TextOut(hdc, img_w-JSDVIG,180,  str_ntr, 10)		! ����� ������
!	lret = TextOut(hdc, img_w-45-JSDVIG-50,140,  str_ntr, 10)		! ����� ������
	END IF

end if
END IF
! ����� � �����
CALL Scale_Finish(img_w,N_CHEN_TEK)

return
end



function getColorForValue3(value,value1)		! ��������������� �������� ����� ������ � ����
    use user32
    use gdi32 
    use FReciverGlobals
	USE DFLIB
	USE DFWIN

	implicit none
	integer*4 getColorForValue3, col
	real*4 value,value1
        col=150*(1-value/value1)
		getColorForValue3 = RGB(col,col,col)
		return	
end

function getColorForValue(value)		! ��������������� �������� ����� ������ � ����
    use user32
    use gdi32 
    use FReciverGlobals
	USE DFLIB
	USE DFWIN

	implicit none
	integer*4 getColorForValue, col
	real*4 val, value

	if (value.gt.max_val) then
		getColorForValue = 0
		return	
	end if

	if (value.eq.max_val) then
		getColorForValue = RGB(150, 150, 150)
		return	
	end if

end

function getColorForValue1(value)		! ��������������� �������� ����� ������ � ����
    use user32
    use gdi32 
    use FReciverGlobals
	USE DFLIB
	USE DFWIN

	implicit none
	integer*4 getColorForValue1, col
	real*4 val, value

	if (value.gt.max_val) then
		getColorForValue1 = 0
		return	
	end if

	if (value.eq.max_val) then
		getColorForValue1 = RGB(175, 115, 0)
		return	
	end if

end

function getColorForValue2(value)		! ��������������� �������� ����� ������ � ����
    use user32
    use gdi32 
    use FReciverGlobals
	USE DFLIB
	USE DFWIN

	implicit none
	integer*4 getColorForValue2, col
	real*4 val, value

	if (value.gt.max_val) then
		getColorForValue2 = 0
		return	
	end if

	if (value.eq.max_val) then
		getColorForValue2 = RGB(150, 100, 0)
		return	
	end if

    end


    
! ������� �������� ����� ������ �������    
  Subroutine Scale_Finish(y,M_CHEN_TEK)
  use user32
  use gdi32 
  use FReciverGlobals
  use PARAM_
  use PARAM_1
  use RAILWAY
  USE DFLIB
  USE DFWIN
  USE N_
  implicit none
  integer hdc, y, JJ1,M_CHEN_TEK
  integer hFont, hPen, lret
  integer ii
  	
  character*200 str_ntr
type (T_POINT) point

  	hdc = hMainDC;

!----------------------------------------------------------------------------------------
! ��������� �������� ������
!----------------------------------------------------------------------------------------

!! ������� ��������� �� 90 ��������
!!hfont = CreateFont(12,0,900,0,FW_BOLD,TRUE, FALSE, 0, RUSSIAN_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH+FF_SWISS, "Arial"C)
!!������� �������������� 
hfont = CreateFont(18,0,0,0,FW_BOLD,TRUE, FALSE, 0, RUSSIAN_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH+FF_SWISS, "Arial"C)
lret = SelectObject(hdc, hfont)
lret = deleteObject(hfont)	! ������ �������� ������
!----------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------
! ������ �����  � ����� �������
!----------------------------------------------------------------------------------
DO JJ1=1,I_HH
        IF(IHH_50(JJ1).LE.img_hh) THEN
        DO II=1,5    
        lret = MSFWIN$SetPixel(hdc,y-45+II,trace_draw_pos(M_CHEN_TEK)+IHH_50(JJ1),RGB(0,0,0)) ! ������� ��������� �����
        END DO
        END IF
END DO
!----------------------------------------------------------------------------------
! ����������� �����
!--------------------------------------------------------------------------------- 
R_LEV=0.
	        WRITE(str_ntr, '(F5.2)') R_LEV	             ! ���������� ����� � ������
	        str_ntr=TRIM(TRIM(str_ntr)//' m')
	        lret = TextOut(hdc, y-40, trace_draw_pos(M_CHEN_TEK)+IHH_50(1)-nfnt/2-1, str_ntr, 6)
II=1
DO JJ1=2,I_HH
        IF(IHH_50(JJ1).LE.img_hh) THEN
            IF(II.EQ.HH_RAB) THEN
            R_LEV=R_LEV+HH_STEP
            WRITE(str_ntr, '(F5.2)') R_LEV	             ! ���������� ����� � ������
	        str_ntr=TRIM(TRIM(str_ntr)//' m')
	        lret = TextOut(hdc,y-40, trace_draw_pos(M_CHEN_TEK)+IHH_50(JJ1)-nfnt/2-1, str_ntr, 6)
            II=1
            ELSE
            II=II+1
            END IF
        END IF
END DO

!----------------------------------------------------------------------------------------
 return
end Subroutine Scale_Finish
!open(unit=100, FILE='d:/OUTPUT.OUT')
!write(100,*) 
!open(unit=100, FILE='C:/GEODATA/OUTPUT.OUT')
!write(100,*) y,M_CHEN_TEK,img_w