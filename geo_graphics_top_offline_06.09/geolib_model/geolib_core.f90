
! ������ ������.
! ������ �������� �������� ������ � ������� ������ � ����     
MODULE gl_core
	implicit none
    
    ! ��������� �������, ������� ����� �������� ��� ��������� ��������� �������
    !ABSTRACT INTERFACE
    !FUNCTION cursor_movement_template(new_cur_pos) RESULT(res)
    !    INTEGER :: new_cur_pos
    !    INTEGER :: res
    !END FUNCTION cursor_movement_template
    !END INTERFACE    
    
  ABSTRACT INTERFACE
  FUNCTION cursor_movement_template(n,x) RESULT(y)
      INTEGER, INTENT(in) :: n
      REAL, INTENT(in) :: x(n)
      REAL :: y
  END FUNCTION cursor_movement_template
  END INTERFACE
  
    INTERFACE
    SUBROUTINE TEMPLATE (Xqq)
    REAL, INTENT(IN) :: Xqq
    END SUBROUTINE TEMPLATE
    END INTERFACE
    
    ! � ���� ��������� ����������� ��������� �� ������, ������������ ��� ���������.
    ! � ����� ������, ���� ��������� ������� - ��������������� ������ �� ��������.
    type T_MISC_GEO_DATA
      integer, pointer :: cursor_position ! ��������� �������
      integer, pointer :: trace_count     ! ����� ������� ������
      !integer, pointer :: trace_goruping  ! ��� ���������� N_PRINT - ������� ����� ���������� � ���� ��� ���������.
      integer, pointer :: chan_count      ! ����� �������. (N_CHEN)
      integer, pointer :: chan_size      ! ����� ������. (N)
      real*4,  pointer :: new_trace(:)    ! ������� ������ (trace_buf ��� MyTrassa)
    end type T_MISC_GEO_DATA

    !type(T_MISC_GEO_DATA) :: GGD ! global geo data  -  ���������� ��������� ��������� � ����������� (��. ����)
    PROCEDURE (TEMPLATE) :: SUB_ONE
          
END MODULE gl_core
