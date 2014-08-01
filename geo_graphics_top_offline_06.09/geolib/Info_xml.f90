! Формирование служебного файла для системы "Каскад"
! Файл info.xml
!---------------------------------------------------------------------
SUBROUTINE INFO_XML()
USE PARAM_1
USE RAILWAY
USE  FReciverGlobals

CHARACTER (200) TEXT,TEXT1

TEXT='<?xml version="1.0" encoding="WINDOWS-1251"?>' !TEXT='<?xml version="1.0" encoding="UTF-8"?>'
WRITE(8,*)TEXT
TEXT='<result>'
WRITE(8,*)TEXT
TEXT='<date>'//TRIM(DAY)//'.'//TRIM(MAUNS)//'.'//TRIM(YER)//'</date>' 
WRITE(8,*)TEXT
TEXT='<device>'//TRIM(N_DEVICE(1:N2_DEV))//'</device>' 
WRITE(8,*)TEXT
TEXT='<railwaydb>'//TRIM(N_VAY(1:N2_VAY))//'</railwaydb>' 
WRITE(8,*)TEXT
TEXT='<direction>'//TRIM(N_DIRECTION(1:N2_DIR))//'</direction>' 
WRITE(8,*)TEXT
TEXT='<way>'//TRIM(N_TRACK(1:N2_TRA))//'</way>' 
WRITE(8,*)TEXT
WRITE(TEXT1,*)KILOMETR_0					! ПРЕОБРАЗУЕМ ЧИСЛО В СТРОКУ
TEXT='<km_begin>'//TRIM(TEXT1)//'</km_begin>' 
WRITE(8,*)TEXT
WRITE(TEXT1,*)METR_0						! ПРЕОБРАЗУЕМ ЧИСЛО В СТРОКУ
TEXT='<m_begin>'//TRIM(TEXT1)//'</m_begin>' 
WRITE(8,*)TEXT
WRITE(TEXT1,*)int(KILOMETR)						! ПРЕОБРАЗУЕМ ЧИСЛО В СТРОКУ
TEXT='<km_end>'//TRIM(TEXT1)//'</km_end>' 
WRITE(8,*)TEXT
WRITE(TEXT1,*)int(METR)							! ПРЕОБРАЗУЕМ ЧИСЛО В СТРОКУ
TEXT='<m_end>'//TRIM(TEXT1)//'</m_end>' 
WRITE(8,*)TEXT
TEXT='<files>' 
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>graf_1.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>graf_1</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km">километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m">метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="h_1">глубина первого слоя</item>'
WRITE(8,*)TEXT
TEXT=' <item name="h_2">глубина второго слоя</item>'
WRITE(8,*)TEXT
TEXT='</attributes>' 
WRITE(8,*)TEXT
TEXT='</file>' 
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>vlagh_1.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>vlagh_1</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_beg">начало километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_beg">начало метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_end">окончание километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_end">окончание метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="length">протяженность</item>'
WRITE(8,*)TEXT
TEXT=' <item name="value">величина</item>'
WRITE(8,*)TEXT
TEXT='</attributes>' 
WRITE(8,*)TEXT
TEXT='</file>'
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>uglub_1.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>uglub_1</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_beg">начало километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_beg">начало метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_end">окончание километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_end">окончание метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="length">протяженность</item>'
WRITE(8,*)TEXT
TEXT=' <item name="Name">характеристика</item>'
WRITE(8,*)TEXT 
TEXT=' <item name="value">величина</item>'
WRITE(8,*)TEXT
TEXT='</attributes>' 
WRITE(8,*)TEXT
TEXT='</file>' 
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>tolsh_1.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>tolsh_1</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_beg">начало километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_beg">начало метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_end">окончание километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_end">окончание метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="length">протяженность</item>'
WRITE(8,*)TEXT
TEXT=' <item name="value">величина</item>'
WRITE(8,*)TEXT                           
TEXT='</attributes>' 
WRITE(8,*)TEXT
TEXT='</file>'  
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>graf_2.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>graf_2</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT='<item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km">километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m">метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="h_1">глубина первого слоя</item>'
WRITE(8,*)TEXT
TEXT=' <item name="h_2">глубина второго слоя</item>'
WRITE(8,*)TEXT
TEXT='</attributes>' 
WRITE(8,*)TEXT
TEXT='</file>' 
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>vlagh_2.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>vlagh_2</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_beg">начало километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_beg">начало метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_end">окончание километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_end">окончание метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="length">протяженность</item>'
WRITE(8,*)TEXT
TEXT=' <item name="value">величина</item>'
WRITE(8,*)TEXT
TEXT='</attributes>'
WRITE(8,*)TEXT
TEXT='</file>'  
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>uglub_2.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>uglub_2</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_beg">начало километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_beg">начало метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_end">окончание километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_end">окончание метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="length">протяженность</item>'
WRITE(8,*)TEXT
TEXT=' <item name="Name">характеристика</item>'
WRITE(8,*)TEXT
TEXT=' <item name="value">величина</item>'
WRITE(8,*)TEXT
TEXT='</attributes>' 
WRITE(8,*)TEXT
TEXT='</file>' 
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>tolsh_2.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>tolsh_2</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_beg">начало километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_beg">начало метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_end">окончание километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_end">окончание метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="length">протяженность</item>'
WRITE(8,*)TEXT
TEXT=' <item name="value">величина</item>'
WRITE(8,*)TEXT                           
TEXT='</attributes>' 
WRITE(8,*)TEXT
TEXT='</file>'  
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>graf_3.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>graf_3</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km">километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m">метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="h_1">глубина первого слоя</item>'
WRITE(8,*)TEXT
TEXT=' <item name="h_2">глубина второго слоя</item>'
WRITE(8,*)TEXT
TEXT='</attributes>' 
WRITE(8,*)TEXT
TEXT='</file>' 
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>vlagh_3.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>vlagh_3</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_beg">начало километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_beg">начало метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_end">окончание километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_end">окончание метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="length">протяженность</item>'
WRITE(8,*)TEXT
TEXT=' <item name="value">величина</item>'
WRITE(8,*)TEXT
TEXT='</attributes>'
WRITE(8,*)TEXT
TEXT='</file>' 
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>uglub_3.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>uglub_3</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_beg">начало километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_beg">начало метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_end">окончание километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_end">окончание метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="length">протяженность</item>'
WRITE(8,*)TEXT
TEXT=' <item name="Name">характеристика</item>'
WRITE(8,*)TEXT
TEXT=' <item name="value">величина</item>'
WRITE(8,*)TEXT
TEXT='</attributes>' 
WRITE(8,*)TEXT
TEXT='</file>'
WRITE(8,*)TEXT
TEXT='<file>' 
WRITE(8,*)TEXT
TEXT='<name>tolsh_3.csv</name>' 
WRITE(8,*)TEXT
TEXT='<object>tolsh_3</object>' 
WRITE(8,*)TEXT
TEXT='<attributes>' 
WRITE(8,*)TEXT
TEXT=' <item name="direction">код направления</item>'
WRITE(8,*)TEXT
TEXT=' <item name="way">номер пути</item>'
WRITE(8,*)TEXT
TEXT=' <item name="nchannel">номер канала</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_beg">начало километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_beg">начало метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="km_end">окончание километр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="m_end">окончание метр</item>'
WRITE(8,*)TEXT
TEXT=' <item name="length">протяженность</item>'
WRITE(8,*)TEXT
TEXT=' <item name="value">величина</item>'
WRITE(8,*)TEXT                           
TEXT='</attributes>' 
WRITE(8,*)TEXT
TEXT='</file>'  
WRITE(8,*)TEXT
TEXT='</files>' 
WRITE(8,*)TEXT
TEXT='</result>' 
WRITE(8,*)TEXT

RETURN
END