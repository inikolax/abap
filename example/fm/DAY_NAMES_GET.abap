*& Confidential and Proprietary
*& Copyright 2014
*& All Rights Reserved
*& Abap name:  ZBC_REPT_DAY_NAMES_GET
*& Description: Пример использования функционального модуля MONTH_NAMES_GET
*& Created by:   Черкашин Н.
*& Web site: http://abap.kz/
 
 
REPORT  ZBC_REPT_DAY_NAMES_GET.
 
DATA:
      lt_days TYPE TABLE OF T246
     ,ls_day TYPE T246.
 
CALL FUNCTION 'DAY_NAMES_GET'
  EXPORTING
    LANGUAGE  = SY-LANGU
  TABLES
    DAY_NAMES = lt_days.
 
 
LOOP AT lt_days INTO ls_day.
  WRITE:/ 'Номер дня:',ls_day-wotnr, 'Название', ls_day-langt.
ENDLOOP.
