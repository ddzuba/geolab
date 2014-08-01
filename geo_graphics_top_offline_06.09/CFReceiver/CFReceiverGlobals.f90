!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module CFReceiverGlobals
use ifwinty
use gl_core
use gl_graphics

implicit none

!  Parameters

integer*4, parameter, public :: SIZEOFAPPNAME = 100
!  Global data

integer(HANDLE)		ghInstance
integer(HANDLE)		ghModule
integer(HANDLE)		ghwndMain
integer(HANDLE)		ghMenu
integer road_number /157/ ! текущий номер дороги
integer flag_file/0/

type(T_MISC_GEO_DATA), target, allocatable :: mainGeoData
type(T_GRAPH_CONTEXT), target, allocatable :: mainGraphContext
type(T_MISC_GEO_DATA), pointer             :: pMainD
type(T_GRAPH_CONTEXT), pointer             :: pMainGC

end module

MODULE GLOBALS_LOG
INTEGER*4 FLAG_INI/0/                    ! признак редактирования ini-файла
INTEGER*4 FLAG_Cfg/0/                    ! признак редактирования cfg-файла
CHARACTER *100 PATHWAY_INI               ! путь к ini-файлу
CHARACTER *100 PATHWAY_CFG               ! путь к cfg-файлу
CHARACTER *100 OLD_PATHWAY_INI           ! путь к сохраненному ini-файлу
CHARACTER *100 OLD_PATHWAY_CFG           ! путь к сохраненному cfg-файлу
CHARACTER *100 NAME_FILE_LOG             ! имя редактируемого файла
CHARACTER *100 FILE_LOG_RED              ! имя отредактированного файла
CHARACTER *100 NAME_FILE_TXT             ! имя редактируемого файла
CHARACTER *100 FILE_TXT_RED              ! имя отредактированного файла
CHARACTER *100 FILE_TREK                 ! имя отредактированного файла
CHARACTER *100 NAME_KIL_METR             ! имя файла  с координатами столбов   
CHARACTER *100 STOLB_LENCH_FILE          ! имя файла  длин пикетов 
CHARACTER  *100 TM                       ! редактируемая строка
CHARACTER  *1 STR                        ! рабочая строка
INTEGER*4 NN                ! номер редактируемого файла
INTEGER*4 NN1               ! номер отредактированного файла   
INTEGER*4 NN2               ! номер файла  с координатами столбов 
INTEGER*4 NN3               ! номер файла  длин пикетов 
INTEGER*4 MESS              ! сообощение
INTEGER*4 IRES              ! считывание имени
INTEGER*4 POLINOM
END MODULE GLOBALS_LOG
    
MODULE KOORDINATA
CHARACTER*50 FF1,FF2,FF3
CHARACTER *200 GPS_TRAK     ! путь к редактору данных
REAL*8, ALLOCATABLE :: VREMYA_TRASSA(:)			! ВРЕМЯ РЕГИСТРАЦИИ ТРАССЫ
REAL*8, ALLOCATABLE :: R_TRASSA(:)				! РАССТОЯНИЕ МЕЖДУ ТРАССАМИ
REAL*8, ALLOCATABLE :: SHIROTA_TRASSA(:)		! ШИРОТА ТРАССЫ
REAL*8, ALLOCATABLE :: DOLGOTA_TRASSA(:)		! ДОЛГОТА ТРАССЫ
REAL*8, ALLOCATABLE ::  R_STOLB (:)             ! РАССТОЯНИЕ МЕЖДУ СТОЛБАМИ
REAL*8, ALLOCATABLE :: VREMYA(:)	! ЧТЕНИЕ ВРЕМЕНИ ИЗ ТЕКСТОВОГО ФАЙЛА ГЕОРАДАРА				
REAL*8, ALLOCATABLE :: SHIROTA(:)	! ЧТЕНИЕ ШИРОТЫ  ИЗ ТЕКСТОВОГО ФАЙЛА ГЕОРАДАРА
REAL*8, ALLOCATABLE :: DOLGOTA(:)	! ЧТЕНИЕ ДОЛГОТЫ ИЗ ТЕКСТОВОГО ФАЙЛА ГЕОРАДАРА
INTEGER(4) IPOINT					! ЧИСЛО ЗАПИСЕЙ В ТЕКСТОВОМ ФАЙЛЕ ПОЗИЦИЙ ГЕОРАДАРА В ГЛОБАЛЬНОЙ СЕТИ
REAL(8), ALLOCATABLE :: KLM(:)					! КИЛОМЕТРОВЫЙ СТОЛБ
REAL(8), ALLOCATABLE :: SHIR(:)					! ШИРОТА КИЛОМЕТРОВОГО СТОЛБА
REAL(8), ALLOCATABLE :: DLGT(:)					! ДОЛГОТА КИЛОМЕТРОВОГО СТОЛБА
INTEGER(4) IPOINT1					            ! ЧИСЛО ЗАПИСЕЙ В ФАЙЛЕ КИЛОМЕТР - КООРДИНАТЫ
REAL(8), ALLOCATABLE :: RAB(:)                  ! РАБОЧИЙ ФАЙЛ
REAL(8) RABZEM/6372795.D0/  					! СРЕДНИЙ РАДИУС ЗЕМЛИ

!INTEGER(4) I_KM						    ! ЧИСЛО ИСПОЛЬЗУЕМЫХ КИЛОМЕТРОВЫХ СТОЛБОВ ИЗ ТАБЛИЦЫ
!INTEGER(4) IPOINT2					! ЧИСЛО ЗАПИСЕЙ В ФАЙЛЕ НОМЕРОВ ТРАСС,ОТМЕЧЕННЫХ ЛЕВОЙ КНОПКОЙ МЫШИ

!INTEGER(4) LKM(1000)				! ТРАССЫ С МЕТКАМИ
!REAL(8) RKM(1000)					! ПИКЕТАЖ ТРАСС С МЕТКАМИ
!INTEGER(4) JPOINT					! ЧИСЛО ЗАПИСЕЙ В ТАБЛИЦЕ С ПИКЕТАМИ


END MODULE KOORDINATA

