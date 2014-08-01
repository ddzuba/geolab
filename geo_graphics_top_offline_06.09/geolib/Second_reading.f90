SUBROUTINE SECOND_READING() 
USE PARAM_1
USE RAILWAY
IMPLICIT NONE
INTEGER(4) II,OnClose
! FL_START	 ��������� ������ ������
! K_FLAG	 ��� ������������ ����� �������

IF(FL_START.eq.0) THEN			! ������ ������
FL_START=1
RETURN
END IF

IF(K_FLAG.EQ.0.) THEN			! ���������� ������� ��������������� ������������ �����
! ����� �������� � �������� ������
	IF(N_VAY(1:N2_VAY).NE.N_VAY1(1:N2_VAY1).OR.&
	  N_DIRECTION(1:N2_DIR).NE.N_DIRECTION1(1:N2_DIR1).OR.&
	  N_TRACK(1:N2_TRA).NE.N_TRACK1(1:N2_TRA1).OR.&
      ABS(COORDINATE-COORDINATE1).GT.500.0  ) THEN ! �������������� �������� �����
		II=  OnClose()	! ���������� ������ ! ������ � ��������� �����
		I_GRAF=0		! �������� ������ � ��������� FIRST
		DO N_CHEN_TEK=1,N_CHEN
		N_WRITE_VLAG(N_CHEN_TEK)=0						! ����� ������ � ���� VLAG
		N_WRITE_UGLUB(N_CHEN_TEK)=0						! ����� ������ � ���� UGLUB
		N_WRITE_TOLSH(N_CHEN_TEK)=0						! ����� ������ � ���� UGLUB
        END DO
! ��������� ����� �����
!----------------------------------------------------------------
! ��������� ����� ����������
IF(N2_DEV1.EQ.0) THEN
	N2_DEV1=1
	N_DEVICE1(1:1)='0'
	ELSE
		IF(N_DEVICE1(1:N2_DEV1).EQ.'Unknown')THEN
		N2_DEV1=1
		N_DEVICE1(1:1)='0'
		END IF
END IF
	N2_DEV=N2_DEV1;	N_DEVICE(1:N2_DEV)=N_DEVICE1(1:N2_DEV1)
!----------------------------------------------------------------
! ��������� ����� ������
IF(N2_VAY1.EQ.0) THEN
	N_VAY1(1:1)='0'
	N2_VAY1=1
	ELSE
		IF(N_VAY1(1:N2_VAY1).EQ.'<null>')THEN
		N_VAY1(1:1)='0'
		N2_VAY1=1
		END IF
END IF
	N2_VAY=N2_VAY1;	N_VAY(1:N2_VAY)=N_VAY1(1:N2_VAY1)
!----------------------------------------------------------------
	! ��������� ����� �����������
IF(N2_DIR1.EQ.0) THEN
	N_DIRECTION1(1:1)='0'
	N2_DIR1=1
	ELSE
		IF(N_DIRECTION1(1:N2_DIR1).EQ.'<null>')THEN
		N_DIRECTION1(1:1)='0'
		N2_DIR1=1
		END IF
END IF
	N2_DIR=N2_DIR1;	N_DIRECTION(1:N2_DIR)=N_DIRECTION1(1:N2_DIR1)
!----------------------------------------------------------------
! ��������� ����� ����
IF(N2_TRA1.EQ.0) THEN
	N_TRACK1(1:1)='0'
	N2_TRA1=1
	ELSE
		IF(N_TRACK1(1:N2_TRA1).EQ.'<null>')THEN
		N_TRACK1(1:1)='0'
		N2_TRA1=1
		END IF
END IF
N2_TRA=N2_TRA1;	N_TRACK(1:N2_TRA)=N_TRACK1(1:N2_TRA1)
!----------------------------------------------------------------
! ��������� ����� �����
		CALL PAPKA_PATH_GEO(NAME_FILE_GEO,N_VAY,N_DIRECTION,N_TRACK,'GEO')
		CALL PAPKA_PATH_GEO(NAME_FILE_IMG,N_VAY,N_DIRECTION,N_TRACK,'IMG')
		CALL FILE_WRITE_SECOND()									! ������� ����� ������ ����������
		CALL FILE_WRITE_CSV_SECOND() 								! ����� � ����� ��������
	END IF
END IF
! ���������� ����������
COORDINATE=COORDINATE1
RETURN
END
