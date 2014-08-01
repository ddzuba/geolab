SUBROUTINE FIRST_READING()
USE FReciverGlobals
USE PARAM_1
USE RAILWAY
USE GRAFICA
USE N_
USE PARAM_
IMPLICIT NONE
INTEGER*4 I
REAL*4 RAB,RAB0
! FL_START	 ��������� ������ ������

IF(FL_START.eq.0) THEN			! ������ ������
! �������� ������ � ��������� FIRST
!	I_GRAF=0
!	DO N_CHEN_TEK=1,N_CHEN
!	N_WRITE_VLAG(N_CHEN_TEK)=0						! ����� ������ � ���� VLAG
!	N_WRITE_UGLUB(N_CHEN_TEK)=0						! ����� ������ � ���� UGLUB
!	N_WRITE_TOLSH(N_CHEN_TEK)=0						! ����� ������ � ���� UGLUB
!   END DO

! ��������� ����� �����
	N2_DEV=N2_DEV1;	N_DEVICE(1:N2_DEV)=N_DEVICE1(1:N2_DEV1)
	N2_VAY=N2_VAY1;	N_VAY(1:N2_VAY)=N_VAY1(1:N2_VAY1)
	N2_DIR=N2_DIR1;	N_DIRECTION(1:N2_DIR)=N_DIRECTION1(1:N2_DIR1)
	N2_TRA=N2_TRA1;	N_TRACK(1:N2_TRA)=N_TRACK1(1:N2_TRA1)
! ���������� ����������
    COORDINATE=COORDINATE1
! ������ ����� ������ � ���������� ������� ���� ��������
    CALL FIRST_READ()			! ������� �������� ���������� �� �����
                                ! ��������� ������� ������� � ��������� POINT_ARRAY
    CALL DEPTH_TRASSA()         ! ���������� ������� ���� ������ ������
! ��������� ������ ������� ����� ��� ���������� ����� � ����� � 0.5 �����
I_HH=1
IHH_50(I_HH)=0
RAB0=HH_STEP/HH_RAB*100.
RAB=RAB0
DO I=J_LEVEL+1,N    
IF(HH(I-1).LT.RAB.AND.HH(I).GE.RAB) THEN   
I_HH=I_HH+1
IHH_50(I_HH)=I-J_LEVEL
RAB=RAB+RAB0
END IF
END DO

CALL activate_graphics
    
END IF
RETURN
END


subroutine activate_graphics
use GeolibGlobals
use gl_core
USE FReciverGlobals
USE PARAM_1
USE RAILWAY
USE GRAFICA
USE N_
USE PARAM_
IMPLICIT NONE

    !init_graphics(hWnd, w, nCh, nPt, inPggd)

    IF (ASSOCIATED(pMainGeoData)) THEN
        pMainGeoData%cursor_position => cursor_pos ! ��������� �������
        pMainGeoData%trace_count => trace_count     ! ����� ������� ������
        pMainGeoData%trace_goruping => N_PRINT  ! ��� ���������� N_PRINT - ������� ����� ���������� � ���� ��� ���������.
        pMainGeoData%chan_count => N_CHEN      ! ����� �������. (N_CHEN)
        pMainGeoData%chan_size => N            ! ����� ������ (N)
        pMainGeoData%new_trace => trace_buf    ! ������� ������ (trace_buf ��� MyTrassa)
    END IF

end subroutine activate_graphics
    
!open(unit=100, FILE='d:/OUTPUT.OUT')
!write(100,*) 

