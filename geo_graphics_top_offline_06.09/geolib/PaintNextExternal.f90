SUBROUTINE PAINTNEXTEXTERNAL()		! Рисуем следующую трассу

  use FReciverGlobals
  use PARAM_
  use PARAM_1
  use RAILWAY
  USE DFLIB
  USE DFWIN
  USE N_
  USE GRAFICA
!CALL GRAFICA(N_CHEN,N_CHEN_TEK,I_B,I_PB,trace_buf,M_OUT,&
!TEXT1,B_TAN(N_CHEN_TEK),TEXT2,REFR(N_CHEN_TEK),TEXT3,&
!UNIFORMITY(N_CHEN_TEK),TEXT4,RMOM(N_CHEN_TEK))
!N_CHEN							! Число каналов, тип: INTEGER*4 
!N_CHEN_TEK						! Номер канала,	 тип: INTEGER*4 
!I_B							! Граница балласта
!I_PB							! Граница подбалластного основания
!trace_buf						! Массив трасс, тип: REAL*4, размерность N_CHEN*512
!M_OUT							! Число расчитанных физических параметров, тип: INTEGER*4  
!TEXT_1							! " Наклон слоев", тип:  Character (200)
!B_TAN_TEK						! Тангенс угла наклона, B_TAN(N_CHEN_TEK), тип: REAL*4
!TEXT_2							! " Влажность", тип:  Character (200)
!REFR_TEK						! Отражательная способность, REFR(N_CHEN_TEK), тип: REAL*4
!TEXT_3							! " Слоистость", тип:  Character (200)
!UNIFORMITY_TEK					! Однородность слоя, UNIFORMITY(N_CHEN_TEK), тип: REAL*4
!TEXT_4							! " Деформативность", тип:  Character (200)
!RMOM_TEK						! Момент: RMOM(N_CHEN_TEK), тип: REAL*4	


END SUBROUTINE PAINTNEXTEXTERNAL