*** Класс для преобразования наборов (SET - транзакция gs01) в range
*** http://abap.kz/blog/152/user/inikolax

*******  Пример использования *******
*** Определяем новый RANGE
*      DATA:lr_bwasl TYPE RANGE OF bwasl.
* 
*** Метод для чтения набора в RANGE
*      CALL METHOD zcl_set_tools=>get_set_to_range
*        EXPORTING
*          im_setname = 'SET_TS'
*        CHANGING
*          ch_range   = lr_bwasl.
          
          
          
CLASS ZCL_SET_TOOLS DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
 
PUBLIC SECTION.
 
  CLASS-METHODS GET_SET_TO_RANGE
    IMPORTING
      !IM_SETNAME TYPE C
    CHANGING
      !CH_RANGE TYPE TABLE .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.
 
 
 
CLASS ZCL_SET_TOOLS IMPLEMENTATION.
 
 
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SET_TOOLS=>GET_SET_TO_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_SETNAME                     TYPE        C
* | [<-->] CH_RANGE                       TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_set_to_range.
 
    DATA: l_new_setid   TYPE          sethier-setid,
          lt_set_values TYPE TABLE OF rgsb4,
          lr_ref        TYPE REF TO   DATA.
 
    FIELD-SYMBOLS: <l_set_values> TYPE rgsb4,
                   <l_range>      TYPE ANY,
                   <l_sign>       TYPE ANY,
                   <l_option>     TYPE ANY,
                   <l_low>        TYPE ANY,
                   <l_high>       TYPE ANY.
*---------------------------------------------------------------------
 
    CREATE DATA lr_ref LIKE LINE OF ch_range.
    ASSIGN lr_ref->* TO <l_range>.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'SIGN' OF STRUCTURE <l_range> TO <l_sign>.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'OPTION' OF STRUCTURE <l_range> TO <l_option>.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'LOW' OF STRUCTURE <l_range> TO <l_low>.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'HIGH' OF STRUCTURE <l_range> TO <l_high>.
    CHECK sy-subrc = 0.
 
    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
      EXPORTING
        shortname                = im_setname
      IMPORTING
        new_setid                = l_new_setid
      EXCEPTIONS
        no_set_found             = 1
        no_set_picked_from_popup = 2
        wrong_class              = 3
        wrong_subclass           = 4
        table_field_not_found    = 5
        fields_dont_match        = 6
        set_is_empty             = 7
        formula_in_set           = 8
        set_is_dynamic           = 9
        OTHERS                   = 10.
 
    CHECK sy-subrc EQ 0.
 
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = l_new_setid
      TABLES
        set_values    = lt_set_values
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
 
    CHECK sy-subrc EQ 0.
 
    LOOP AT lt_set_values ASSIGNING <l_set_values>.
      CLEAR: <l_sign>, <l_low>, <l_option>, <l_high>.
      <l_sign> = 'I'.
      <l_low> = <l_set_values>-FROM.
      IF <l_set_values>-FROM EQ <l_set_values>-TO.
        <l_option> = 'EQ'.
      ELSE.
        <l_option> = 'BT'.
        <l_high> = <l_set_values>-TO.
      ENDIF.
      READ TABLE ch_range WITH KEY TABLE_LINE = <l_range> BINARY SEARCH TRANSPORTING NO FIELDS.
      CHECK sy-subrc <> 0.
      INSERT <l_range> INTO ch_range INDEX sy-tabix.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
