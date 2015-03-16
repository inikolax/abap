*& Confidential and Proprietary
*& Copyright 2014
*& All Rights Reserved
*& Abap name:  ZBC_REPT_LAST_DAY
*& Description: Пример использования функционального модуля FKK_GET_LAST_DAY_OF_MONTH
*& Created by:   Черкашин Н.
*& Web site: http://abap.kz/
 
 
REPORT  ZBC_REPT_LAST_DAY.
 
DATA:
      lv_last_datum TYPE datum.
 
CALL FUNCTION 'FKK_GET_LAST_DAY_OF_MONTH'
 EXPORTING
   I_DATE_IN        = sy-datum
 IMPORTING
   E_DATE_OUT       = lv_last_datum.
 
*** Вывод последнего дня месяца на экран 
WRITE lv_last_datum.
