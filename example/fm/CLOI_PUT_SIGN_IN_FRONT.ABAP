*& Confidential and Proprietary
*& Copyright 2014
*& All Rights Reserved
*& Title: Пример использования функционального модуля CLOI_PUT_SIGN_IN_FRONT
*& Web site: http://abap.kz/
*& Description:
*& Выводит минус перед числом
 
REPORT  ZBC_REPT_TEST_SOURCE.
 
DATA: lv_num TYPE d VALUE '341-'.
 
CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
CHANGING
  VALUE = lv_num.
 
WRITE: / lv_num.
