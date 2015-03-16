*& Confidential and Proprietary
*& Copyright 2015
*& All Rights Reserved
*& Abap name:  ZHR_REPT_TEST
*& Description: Пример использования функционального модуля FP_FUNCTION_MODULE_NAME
*& Created by:   Черкашин Н.
*& Web site: http://abap.kz/
 
REPORT  ZHR_REPT_TEST.
 
 DATA lv_f_name TYPE rs38l_fnam.
 
 CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
   EXPORTING
     i_name = 'ZPDFTEST' "Имя вашего формуляра в SFP
  IMPORTING
    E_FUNCNAME  = lv_f_name. "Вернет название ФМ
    
    
