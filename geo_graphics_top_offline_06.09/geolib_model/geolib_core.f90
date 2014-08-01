
! Модель данных.
! Модуль содержит основные данные и функции работы с ними     
MODULE gl_core
	implicit none
    
    ! интерфейс функции, которую можно вызывать при изменении положения курсора
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
    
    ! В этой структуре содержаться указатели на данные, используемые для рисования.
    ! В общем случае, если указатель нулевой - соответствующие данные не рисуются.
    type T_MISC_GEO_DATA
      integer, pointer :: cursor_position ! положение курсора
      integer, pointer :: trace_count     ! номер текущей трассы
      !integer, pointer :: trace_goruping  ! Еще назывылось N_PRINT - сколько трасс объединять в одну при рисовании.
      integer, pointer :: chan_count      ! Число каналов. (N_CHEN)
      integer, pointer :: chan_size      ! Длина канала. (N)
      real*4,  pointer :: new_trace(:)    ! текущая трасса (trace_buf или MyTrassa)
    end type T_MISC_GEO_DATA

    !type(T_MISC_GEO_DATA) :: GGD ! global geo data  -  глобальный экземпляр структуры с указателями (см. выше)
    PROCEDURE (TEMPLATE) :: SUB_ONE
          
END MODULE gl_core
