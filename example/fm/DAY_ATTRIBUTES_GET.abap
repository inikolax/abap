*& Confidential and Proprietary
*& Copyright 2014
*& All Rights Reserved
*& Title: Пример использования функционального модуля DAY_ATTRIBUTES_GET
*& Web site: http://abap.kz/
*& Description:
*& Возвращает даты прописью
 
REPORT  ZBC_REPT_TEST_SOURCE.
 
select-OPTIONS s_datum FOR sy-datum DEFAULT sy-datum.
 
DATA:
      lt_day_atr TYPE TABLE OF casdayattr
     ,ls_day_atr TYPE casdayattr.
 
CALL FUNCTION 'DAY_ATTRIBUTES_GET'
  EXPORTING
    DATE_FROM      = s_datum-LOW
    DATE_TO        = s_datum-HIGH
  TABLES
    DAY_ATTRIBUTES = lt_day_atr.
 
 
LOOP AT lt_day_atr INTO ls_day_atr.
  WRITE:/ ls_day_atr-day_string.
ENDLOOP.
