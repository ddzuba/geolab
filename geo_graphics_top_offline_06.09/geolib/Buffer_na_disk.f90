    SUBROUTINE BUFFER_NA_DISK()
	use FReciverGlobals
	USE PARAM_1
	USE RAILWAY
	USE GRAFICA
	USE N_
    
    implicit none 
    integer*4 II, OnClose
    
! ������������ �� ���� ��� ���������� �������
IF(i_GRAF.EQ.N_GRAF) THEN		! ����������� ����� �� ����
	II=  OnClose()	! ���������� ������ ! ������ � ��������� �����
	I_GRAF=0		! �������� ������ � ��������� FIRST
	DO N_CHEN_TEK=1,N_CHEN
	N_WRITE_VLAG(N_CHEN_TEK)=0						! ����� ������ � ���� VLAG
	N_WRITE_UGLUB(N_CHEN_TEK)=0						! ����� ������ � ���� UGLUB
    N_WRITE_TOLSH(N_CHEN_TEK)=0						! ����� ������ � ���� TOLSH
    END DO
! ��������� ����� �����
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL FILE_WRITE_SECOND()									! ������� ����� ������ ����������
		CALL FILE_WRITE_CSV_SECOND() 								! ����� � ����� ��������
END IF		
DO N_CHEN_TEK=1,N_CHEN	
	IF(N_WRITE_VLAG(N_CHEN_TEK).EQ.N_VLAG) THEN		! ����������� ����� �� ����
		II=  OnClose()	! ���������� ������ ! ������ � ��������� �����
		I_GRAF=0		! �������� ������ � ��������� FIRST
		DO II=1,N_CHEN
		N_WRITE_VLAG(II)=0						! ����� ������ � ���� VLAG
		N_WRITE_UGLUB(II)=0						! ����� ������ � ���� UGLUB
        N_WRITE_TOLSH(II)=0						! ����� ������ � ���� TOLSH
		END DO
! ��������� ����� �����
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL FILE_WRITE_SECOND()									! ������� ����� ������ ����������
		CALL FILE_WRITE_CSV_SECOND() 								! ����� � ����� ��������
	END IF
END DO
DO N_CHEN_TEK=1,N_CHEN	
	IF(N_WRITE_UGLUB(N_CHEN_TEK).EQ.N_UGLUB) THEN		! ����������� ����� �� ����
		II=  OnClose()	! ���������� ������ ! ������ � ��������� �����
		I_GRAF=0		! �������� ������ � ��������� FIRST
		DO II=1,N_CHEN
		N_WRITE_VLAG(II)=0						! ����� ������ � ���� VLAG
		N_WRITE_UGLUB(II)=0						! ����� ������ � ���� UGLUB
        N_WRITE_TOLSH(II)=0						! ����� ������ � ���� UGLUB
		END DO
! ��������� ����� �����
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL FILE_WRITE_SECOND()									! ������� ����� ������ ����������
		CALL FILE_WRITE_CSV_SECOND() 								! ����� � ����� ��������
	END IF
END DO
DO N_CHEN_TEK=1,N_CHEN	
	IF(N_WRITE_TOLSH(N_CHEN_TEK).EQ.N_TOLSH) THEN		! ����������� ����� �� ����
		II=  OnClose()	! ���������� ������ ! ������ � ��������� �����
		I_GRAF=0		! �������� ������ � ��������� FIRST
		DO II=1,N_CHEN
		N_WRITE_VLAG(II)=0						! ����� ������ � ���� VLAG
		N_WRITE_UGLUB(II)=0						! ����� ������ � ���� UGLUB
        N_WRITE_TOLSH(II)=0						! ����� ������ � ���� UGLUB
		END DO
! ��������� ����� �����
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL FILE_WRITE_SECOND()									! ������� ����� ������ ����������
		CALL FILE_WRITE_CSV_SECOND() 								! ����� � ����� ��������
	END IF
END DO
	END
