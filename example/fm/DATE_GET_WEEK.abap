*& Confidential and Proprietary
*& Copyright 2014
*& All Rights Reserved
*& Description: Пример использования функционального модуля DATE_GET_WEEK
*& Web site: http://abap.kz/
 
REPORT  ZBC_REPT_TEST_SOURCE.
 
DATA: lv_week LIKE scal-week.
 
CALL FUNCTION 'DATE_GET_WEEK'
EXPORTING
  DATE         = sy-datum
IMPORTING
  week         = lv_week
EXCEPTIONS
  date_invalid = 1
  OTHERS       = 2.
 
 
WRITE lv_week.
