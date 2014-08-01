SUBROUTINE SendTraceRealTime(pMyData, coord)

  implicit none
  integer*4 i,n_bit 
  	integer :: pMyData, coord
   character(1):: MyData(*); pointer(pMyData, MyData)
   real*4 rab,rab1
   
!open(unit=100, FILE='D:/OUTPUT.OUT')
!n_bit=4
!DO I=1,512
RAB=transfer(MyData(1:4),RAB) ! вбрасываем все каналы
rab1=transfer(MyData(1+510*4:4+510*4),RAB)
!J=J+N_BIT
!MyTrassa(I)=RAB
!END DO


!write(100,*)pMyData, coord,rab,rab1


RETURN
END SUBROUTINE SendTraceRealTime
! open(unit=100, FILE='D:/OUTPUT.OUT')
! write(100,*)NCHANAL_GEO,NCHANAL(I)

