! ----------------------------------------------------------
! ���������� ��������� ������
! ----------------------------------------------------------
SUBROUTINE COORDINATORY()
USE DFLIB
USE KOORDINATA
USE INPUT_RADAROGRAMMA 
USE DFWINA
IMPLICIT NONE
INTEGER(4) I,J,KL,MESS
KL=0
DO 1 I=1,NTR
IF(VREMYA_TRASSA(I).LT.VREMYA(1))THEN
SHIROTA_TRASSA(I)=SHIROTA(2)-(SHIROTA(2)-SHIROTA(1))*(VREMYA(2)-VREMYA_TRASSA(I))/(VREMYA(2)-VREMYA(1))
DOLGOTA_TRASSA(I)=DOLGOTA(2)-(DOLGOTA(2)-DOLGOTA(1))*(VREMYA(2)-VREMYA_TRASSA(I))/(VREMYA(2)-VREMYA(1))
GOTO 1
END IF
IF(VREMYA_TRASSA(I).GT.VREMYA(IPOINT))THEN
SHIROTA_TRASSA(I)=SHIROTA(IPOINT)+(SHIROTA(IPOINT)-SHIROTA(IPOINT-1))*(VREMYA_TRASSA(I)-VREMYA(IPOINT))/(VREMYA(IPOINT)-VREMYA(IPOINT-1))
DOLGOTA_TRASSA(I)=DOLGOTA(IPOINT)+(DOLGOTA(IPOINT)-DOLGOTA(IPOINT-1))*(VREMYA_TRASSA(I)-VREMYA(IPOINT))/(VREMYA(IPOINT)-VREMYA(IPOINT-1))
GOTO 1
END IF
DO 2 J=1,IPOINT-1
IF(VREMYA_TRASSA(I).GE.VREMYA(J).AND.VREMYA_TRASSA(I).LE.VREMYA(J+1))THEN
KL=1
SHIROTA_TRASSA(I)=SHIROTA(J+1)-(SHIROTA(J+1)-SHIROTA(J))*(VREMYA(J+1)-VREMYA_TRASSA(I))/(VREMYA(J+1)-VREMYA(J))
DOLGOTA_TRASSA(I)=DOLGOTA(J+1)-(DOLGOTA(J+1)-DOLGOTA(J))*(VREMYA(J+1)-VREMYA_TRASSA(I))/(VREMYA(J+1)-VREMYA(J))
GOTO 1
END IF
2   CONTINUE
1 CONTINUE
!--------------------------------------------------------------------
IF(KL.EQ.0) THEN
MESS=MESSAGEBOX(NULL,"�������� ������ ������ - ��"C,"����� ������ ������ �� ������������"C,0)    
IF(MESS.EQ.1)STOP                                                                                         
END IF
RETURN
END
!open(unit=105, FILE='d:/OUTPUT4.OUT')
!write(105,*)VREMYA
!write(105,*)VREMYA_TRASSA