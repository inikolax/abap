*&---------------------------------------------------------------------*
*& Report  ZBC_IMP_EXP_TABLE
*&
*&---------------------------------------------------------------------*
*&
*& Программа для выгрузки и загрузки таблиц SAP
*&---------------------------------------------------------------------*
 
REPORT  zbc_imp_exp_table LINE-SIZE 80
                LINE-COUNT 65
                NO STANDARD PAGE HEADING..
TYPE-POOLS: abap.
 
CONSTANTS: gc_separator      TYPE char3   VALUE '#~#',
           gc_name_rec       TYPE char2   VALUE 'A0',
           gc_header_rec     TYPE char2   VALUE 'A1',
           gc_record_rec     TYPE char2   VALUE 'A2',
           gc_0d_replace     TYPE char6   VALUE '#~0D~#',  " Чем заменить перевод строки
           gc_0a_replace     TYPE char6   VALUE '#~0A~#'.  " Чем заменить перевод строки
 
TABLES: dd02l.
 
TYPES: BEGIN OF lty_tabline,
         rectype        TYPE char2,   " Тип записи файла
         LINE           TYPE STRING,  " Строка файла, поля разделены разделителями
       END OF lty_tabline,
       BEGIN OF lty_fieldname,
         fieldname   TYPE dd03l-fieldname,
       END OF lty_fieldname.
 
TYPES: lty_t_tabline     TYPE TABLE OF lty_tabline,
       lty_t_fieldname   TYPE TABLE OF lty_fieldname.
 
PARAMETERS: p_file    TYPE text255 OBLIGATORY.
select-OPTIONS: s_tables   FOR dd02l-tabname.
PARAMETERS: p_exp     TYPE c RADIOBUTTON GROUP 1 DEFAULT 'X' USER-COMMAND EXP,
            p_imp     TYPE c RADIOBUTTON GROUP 1,
            p_del     TYPE c AS CHECKBOX DEFAULT ' '.
 
SELECTION-SCREEN BEGIN OF BLOCK badd WITH FRAME TITLE text-ADD.
PARAMETERS: p_cond1   TYPE text50 MODIF ID EXP,
            p_cond2   TYPE text50 MODIF ID EXP,
            p_cond3   TYPE text50 MODIF ID EXP.
SELECTION-SCREEN END OF BLOCK badd.
 
*----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM open_file_name CHANGING p_file.
 
*----------------------------------------------------------------------
AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'EXP'.
      PERFORM OUTPUT.
  ENDCASE.
 
AT SELECTION-SCREEN OUTPUT.
  PERFORM OUTPUT.
 
*----------------------------------------------------------------------
START-OF-SELECTION.
  IF p_exp = abap_true.
    PERFORM main_export.
  ELSE.
    PERFORM main_import.
  ENDIF.
 
*&---------------------------------------------------------------------*
*&      Form  output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM OUTPUT.
  LOOP AT SCREEN.
    IF screen-NAME = 'P_DEL'.
      IF p_exp = abap_true.
        screen-INPUT = 0.
      ELSE.
        screen-INPUT = 1.
      ENDIF.
    ENDIF.
 
    IF screen-group1 = 'EXP'.
      IF p_exp = abap_true.
        screen-INPUT = 1.
      ELSE.
        screen-INPUT = 0.
      ENDIF.
    ENDIF.
 
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    "output
 
*&---------------------------------------------------------------------*
*&      Form  export_table_to_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_table_to_file USING VALUE(u_tabname)  TYPE dd02l-tabname
                       CHANGING ct_contents    TYPE lty_t_tabline
                                VALUE(c_rc)    TYPE sysubrc.
 
  DATA: lt_fields         TYPE TABLE OF lty_fieldname,
        lr_data           TYPE REF TO DATA,
        lv_value          TYPE STRING,
        lv_tabix          TYPE sy-tabix,
        lv_dummy          TYPE bapiret2-MESSAGE.
 
  FIELD-SYMBOLS: <ls_tabline>        TYPE lty_tabline,
                 <ls_fieldname>      TYPE lty_fieldname,
                 <lt_table>          TYPE TABLE,
                 <ls_line>           TYPE ANY,
                 <lv_value>          TYPE ANY.
 
* Check if table exists
  SELECT SINGLE tabname
    INTO u_tabname
    FROM dd02l
    WHERE tabname  = u_tabname
      AND tabclass = 'TRANSP'.
 
  IF sy-subrc <> 0.
    MESSAGE e000(clhp) WITH 'Не найдена в системе таблица'(001) u_tabname
      INTO lv_dummy.
    c_rc = 1.
    RETURN.
  ENDIF.
 
* Заголовок - имя таблицы
  APPEND INITIAL LINE TO ct_contents ASSIGNING <ls_tabline>.
  <ls_tabline>-rectype = gc_name_rec.
  <ls_tabline>-LINE    = u_tabname.
 
* Заголовок таблицы (имена полей)
  PERFORM get_table_fields USING u_tabname CHANGING lt_fields.
 
  APPEND INITIAL LINE TO ct_contents ASSIGNING <ls_tabline>.
  <ls_tabline>-rectype = gc_header_rec.
 
  LOOP AT lt_fields ASSIGNING <ls_fieldname>.
 
    IF sy-tabix > 1.
      CONCATENATE <ls_tabline>-LINE gc_separator INTO <ls_tabline>-LINE.
    ENDIF.
 
    CONCATENATE <ls_tabline>-LINE <ls_fieldname>-fieldname INTO <ls_tabline>-LINE.
  ENDLOOP.
 
* Строки таблицы
  CREATE DATA lr_data TYPE TABLE OF (u_tabname).
  ASSIGN lr_data->* TO <lt_table>.
 
  " Применяем подходящие условия
  DO 1 TIMES.
 
    IF p_cond1 IS NOT INITIAL.
      TRY.
 
          SELECT *
            FROM (u_tabname)
            INTO TABLE <lt_table>
            WHERE (p_cond1).
 
          EXIT.
        CATCH cx_sy_dynamic_osql_semantics.
      ENDTRY.
    ENDIF.
 
    IF p_cond2 IS NOT INITIAL.
      TRY.
 
          SELECT *
            FROM (u_tabname)
            INTO TABLE <lt_table>
            WHERE (p_cond2).
 
          EXIT.
        CATCH cx_sy_dynamic_osql_semantics.
      ENDTRY.
    ENDIF.
 
    IF p_cond3 IS NOT INITIAL.
      TRY.
 
          SELECT *
            FROM (u_tabname)
            INTO TABLE <lt_table>
            WHERE (p_cond3).
 
          EXIT.
        CATCH cx_sy_dynamic_osql_semantics.
      ENDTRY.
    ENDIF.
 
    SELECT *
      FROM (u_tabname)
      INTO TABLE <lt_table>.
  ENDDO.
 
  LOOP AT <lt_table> ASSIGNING <ls_line>.
 
    APPEND INITIAL LINE TO ct_contents ASSIGNING <ls_tabline>.
    <ls_tabline>-rectype = gc_record_rec.
 
    LOOP AT lt_fields ASSIGNING <ls_fieldname>.
 
      lv_tabix = sy-tabix.
 
      ASSIGN COMPONENT <ls_fieldname>-fieldname OF STRUCTURE <ls_line> TO <lv_value>.
      IF sy-subrc = 0.
        lv_value = <lv_value>.
      ELSE.
        CLEAR lv_value.
      ENDIF.
 
      REPLACE ALL occurrences OF cl_abap_char_utilities=>cr_lf(1) IN lv_value
        WITH gc_0d_replace.
 
      REPLACE ALL occurrences OF cl_abap_char_utilities=>cr_lf+1(1) IN lv_value
        WITH gc_0a_replace.
 
      IF lv_tabix > 1.
        CONCATENATE <ls_tabline>-LINE gc_separator INTO <ls_tabline>-LINE.
      ENDIF.
 
      CONCATENATE <ls_tabline>-LINE lv_value INTO <ls_tabline>-LINE.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " export_table_to_file
*&---------------------------------------------------------------------*
*&      Form  get_table_fields
*&---------------------------------------------------------------------*
*       Возвращает имена полей таблицы без mandt
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_table_fields USING VALUE(u_tabname)  TYPE dd02l-tabname
                      CHANGING ct_fields      TYPE lty_t_fieldname.
 
  SELECT fieldname
    FROM dd03l
    INTO TABLE ct_fields
    WHERE tabname   = u_tabname
      AND precfield = SPACE       " Исключаем include
      AND domname   <> 'MANDT'.   " Исключаем мандант
ENDFORM.                    " get_table_fields
*&---------------------------------------------------------------------*
*&      Form  get_all_tabnames_from_file
*&---------------------------------------------------------------------*
*       Возвращает список таблиц, которые есть в файле
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_all_tabnames_from_file USING    ut_content   TYPE lty_t_tabline
                                CHANGING ct_names     TYPE string_table.
 
  FIELD-SYMBOLS: <ls_tabline>     TYPE lty_tabline.
 
  REFRESH ct_names[].
 
  LOOP AT ut_content ASSIGNING <ls_tabline>
    WHERE rectype = gc_name_rec.
 
    APPEND <ls_tabline>-LINE TO ct_names.
  ENDLOOP.
ENDFORM.                    " get_all_tabnames_from_file
*&---------------------------------------------------------------------*
*&      Form  check_table_structure
*&---------------------------------------------------------------------*
*       Выполняет проверку структуры таблицы, возвращает флаг
*       успешности-неуспешности и список ошибок если неуспех
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_table_structure USING VALUE(u_tabname)    TYPE dd02l-tabname
                                 ut_content          TYPE lty_t_tabline
                           CHANGING ct_return        TYPE bapiret2_t
                                    VALUE(c_rc)      TYPE sysubrc.
 
  DATA: lv_index              TYPE sy-tabix,
        lv_text               TYPE STRING,
        lt_fields_file        TYPE TABLE OF lty_fieldname,
        lt_fields_sap         TYPE TABLE OF lty_fieldname,
        lv_num_fields_sap     TYPE i,
        lv_num_fields_file    TYPE i,
        lv_not_found          TYPE wdy_boolean.
 
  FIELD-SYMBOLS: <ls_tabline>      TYPE lty_tabline,
                 <ls_field_file>   TYPE lty_fieldname,
                 <ls_field_sap>    TYPE lty_fieldname.
 
* Find the table record
  READ TABLE ut_content WITH KEY rectype = gc_name_rec
                                 LINE    = u_tabname
                                 TRANSPORTING NO FIELDS.
 
  IF sy-subrc <> 0.
 
    CONCATENATE 'Таблица'(002) u_tabname 'не найдена в файле'(003) INTO lv_text SEPARATED BY SPACE.
    PERFORM msg_to_ret2_string USING lv_text 'E' CHANGING ct_return[].
    c_rc = 1.
    RETURN.
  ENDIF.
 
* Look for header description line to compare to the database state
  lv_index = sy-tabix.
  lv_index = lv_index + 1.
 
  PERFORM get_first_header_after_line
              USING
                  u_tabname
                  ut_content
                  lv_index
              CHANGING
                  lt_fields_file[]
                  c_rc.
 
  IF c_rc <> 0.
    PERFORM msg_to_ret2_string USING lv_text 'E' CHANGING ct_return[].
    RETURN.
  ENDIF.
 
* Get fields list from database and perform the comparison
  PERFORM get_table_fields USING u_tabname CHANGING lt_fields_sap.
 
  DESCRIBE TABLE lt_fields_sap LINES lv_num_fields_sap.
  DESCRIBE TABLE lt_fields_file LINES lv_num_fields_file.
 
  LOOP AT lt_fields_file ASSIGNING <ls_field_file>.
 
    lv_index = sy-tabix.
    lv_not_found = abap_false.
 
    unassign <ls_field_sap>.
    IF lv_index <= lv_num_fields_sap.
      READ TABLE lt_fields_sap INDEX sy-tabix ASSIGNING <ls_field_sap>.
    ENDIF.
 
    IF <ls_field_sap> IS ASSIGNED.
      IF <ls_field_sap>-fieldname <> <ls_field_file>-fieldname.
        lv_not_found = abap_true.
      ENDIF.
    ELSE.
      lv_not_found = abap_true.
    ENDIF.
 
    IF lv_not_found = abap_true.
      CONCATENATE 'В SAP отсутствует (или находится в другой позиции) поле'(004)
        <ls_field_file>-fieldname 'таблицы'(005) u_tabname ', имеющееся в файле'(006)
        INTO lv_text SEPARATED BY SPACE.
      PERFORM msg_to_ret2_string USING lv_text 'E' CHANGING ct_return[].
      c_rc = 1.
    ENDIF.
  ENDLOOP.
 
  IF lv_num_fields_sap <> lv_num_fields_file.
    LOOP AT lt_fields_sap ASSIGNING <ls_field_sap>.
 
      READ TABLE lt_fields_file WITH KEY fieldname = <ls_field_sap>-fieldname
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CONCATENATE 'В файле отсутствует поле'(007) <ls_field_sap>-fieldname
          'таблицы'(005) u_tabname ', имеющееся в SAP'(008) INTO lv_text SEPARATED BY SPACE.
        PERFORM msg_to_ret2_string USING lv_text 'E' CHANGING ct_return[].
        c_rc = 1.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " check_table_structure
*&---------------------------------------------------------------------*
*&      Form  modify_dbtab_from_file
*&---------------------------------------------------------------------*
*       Выполняет модицикацию таблицы из файла
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_dbtab_from_file USING VALUE(u_tabname)     TYPE dd02l-tabname
                                  ut_content           TYPE lty_t_tabline
                                  VALUE(u_delete)      TYPE wdy_boolean
                            CHANGING VALUE(c_rc)       TYPE sysubrc.
 
  DATA: lv_dummy           TYPE bapiret2-MESSAGE,
        lv_tabix           TYPE sy-tabix,
        lr_data            TYPE REF TO DATA,
        lt_values          TYPE TABLE OF STRING,
        lv_value           TYPE STRING,
        lv_clidep          TYPE dd02l-clidep.
 
  FIELD-SYMBOLS: <ls_tabline>       TYPE lty_tabline,
                 <ls_dbtab_line>    TYPE ANY,
                 <lv_value>         TYPE ANY,
                 <ls_fieldname>     TYPE lty_fieldname.
 
* Find the table record
  READ TABLE ut_content WITH KEY rectype = gc_name_rec
                                 LINE    = u_tabname
                                 TRANSPORTING NO FIELDS.
 
  IF sy-subrc <> 0.
    MESSAGE e000(clhp) WITH 'Таблица'(002) u_tabname 'не найдена в файле'(003)
      INTO lv_dummy.
    c_rc = 1.
    RETURN.
  ENDIF.
 
  lv_tabix = sy-tabix + 1.
 
  SELECT SINGLE clidep
    INTO lv_clidep
    FROM dd02l
    WHERE tabname = u_tabname
      AND tabclass = 'TRANSP'.
 
  CREATE DATA lr_data TYPE (u_tabname).
  ASSIGN lr_data->* TO <ls_dbtab_line>.
 
  IF u_delete = abap_true.
    DELETE FROM (u_tabname).                            "#EC CI_NOWHERE
  ENDIF.
 
  LOOP AT ut_content ASSIGNING <ls_tabline>
    FROM lv_tabix.
    "WHERE rectype = gc_record_rec.
 
    " Началась следующая таблица - выходим
    IF <ls_tabline>-rectype = gc_name_rec.
      EXIT.
    ENDIF.
 
    CHECK <ls_tabline>-rectype = gc_record_rec.
 
    SPLIT <ls_tabline>-LINE AT gc_separator INTO TABLE lt_values.
 
    LOOP AT lt_values INTO lv_value.
 
      lv_tabix = sy-tabix.
 
      " Если таблица зависима от манданта, то номер поля будет на 1 больше
      IF lv_clidep = abap_true.
        lv_tabix = lv_tabix + 1.
      ENDIF.
 
      ASSIGN COMPONENT lv_tabix OF STRUCTURE <ls_dbtab_line> TO <lv_value>.
      IF sy-subrc = 0.
        <lv_value> = lv_value.
      ENDIF.
    ENDLOOP.
 
    MODIFY (u_tabname) FROM <ls_dbtab_line>.
  ENDLOOP.
ENDFORM.                    " modify_dbtab_from_file
*&---------------------------------------------------------------------*
*&      Form  get_first_header_after_line
*&---------------------------------------------------------------------*
*       Технический код: возвращает поля первого заголовка файла
*       в загруженном контенте после определенной строки этого файла
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_first_header_after_line USING VALUE(u_tabname)       TYPE dd02l-tabname
                                       ut_content             TYPE lty_t_tabline
                                       VALUE(u_line_from)     TYPE i
                                 CHANGING ct_header_fields    TYPE lty_t_fieldname
                                          VALUE(c_rc)         TYPE sysubrc.
 
  DATA: lv_dummy        TYPE bapiret2-MESSAGE.
 
  FIELD-SYMBOLS: <ls_tabline>     TYPE lty_tabline.
 
  LOOP AT ut_content ASSIGNING <ls_tabline>
    FROM u_line_from
    WHERE rectype = gc_header_rec.
 
    EXIT.
  ENDLOOP.
 
  IF sy-subrc <> 0.
    MESSAGE e000(clhp) WITH 'Для таблицы'(009) u_tabname
      'не найдена строка заголовка в файле'(010) INTO lv_dummy.
    c_rc = 1.
    RETURN.
  ENDIF.
 
* Parse the header string in order to get field list from it
  SPLIT <ls_tabline>-LINE AT gc_separator INTO TABLE ct_header_fields.
ENDFORM.                    " get_first_header_after_line
*&---------------------------------------------------------------------*
*&      Form  load_contents_from_file
*&---------------------------------------------------------------------*
*       Загружает файл с диска во внутреннюю таблицу
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_contents_from_file USING u_filename
                             CHANGING ct_content     TYPE lty_t_tabline
                                      VALUE(c_rc)    TYPE sysubrc.
 
  DATA: BEGIN OF lt_data_tab OCCURS 0,
          LINE(3000)    TYPE c,
        END OF lt_data_tab.
 
  DATA: lv_filename           TYPE STRING,
        lv_contents           TYPE STRING,
        lt_lines              TYPE TABLE OF STRING,
        lv_text               TYPE STRING.
 
  FIELD-SYMBOLS: <ls_tabline>        TYPE lty_tabline.
 
  lv_filename = u_filename.
 
  REFRESH ct_content[].
 
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_filename
      read_by_line            = abap_false
    CHANGING
      data_tab                = lt_data_tab[]
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
 
  c_rc = sy-subrc.
 
  IF c_rc <> 0.
    RETURN.
  ENDIF.
 
  LOOP AT lt_data_tab.
    CONCATENATE lv_contents lt_data_tab INTO lv_contents.
  ENDLOOP.
 
  SPLIT lv_contents AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_lines.
 
* Каждая строка файла содержит вид записи (2 символа), разделитель, а затем
* уже информацию в зависимости от вида записи
  LOOP AT lt_lines INTO lv_text.
 
    CHECK lv_text IS NOT INITIAL.
    CHECK STRLEN( lv_text ) >= 5.
 
    APPEND INITIAL LINE TO ct_content ASSIGNING <ls_tabline>.
    SPLIT lv_text AT gc_separator INTO <ls_tabline>-rectype <ls_tabline>-LINE.
 
    REPLACE ALL occurrences OF gc_0d_replace IN <ls_tabline>-LINE
      WITH cl_abap_char_utilities=>cr_lf(1).
 
    REPLACE ALL occurrences OF gc_0a_replace IN <ls_tabline>-LINE
      WITH cl_abap_char_utilities=>cr_lf+1(1).
  ENDLOOP.
ENDFORM.                    " load_contents_from_file
*&---------------------------------------------------------------------*
*&      Form  save_contents_to_file
*&---------------------------------------------------------------------*
*       Сохраняет содержимое файла из внутренней таблицы на диск
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_contents_to_file USING u_filename
                                 ut_content         TYPE lty_t_tabline
                           CHANGING VALUE(c_rc)     TYPE sysubrc.
 
  DATA: BEGIN OF lt_data_tab OCCURS 0,
          LINE(3000)    TYPE c,
        END OF lt_data_tab.
 
  DATA: lv_string            TYPE STRING,
        lv_lines             TYPE i,
        lv_filename          TYPE STRING.
 
  FIELD-SYMBOLS: <ls_tabline>           TYPE lty_tabline.
 
  lv_filename = u_filename.
 
  lv_lines = LINES( ut_content ).
 
  LOOP AT ut_content ASSIGNING <ls_tabline>.
 
    IF lv_string IS NOT INITIAL.
      CONCATENATE lv_string cl_abap_char_utilities=>cr_lf INTO lv_string.
    ENDIF.
 
    CONCATENATE lv_string <ls_tabline>-rectype gc_separator <ls_tabline>-LINE
      into lv_string.
  ENDLOOP.
 
  CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
    EXPORTING
      text      = lv_string
    TABLES
      ftext_tab = lt_data_tab[].
 
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                 = lv_filename
      write_lf                 = abap_false
      write_lf_after_last_line = abap_false
    CHANGING
      data_tab                 = lt_data_tab[]      "trunc_trailing_blanks = abap_true
    EXCEPTIONS
      file_write_error         = 1
      no_batch                 = 2
      gui_refuse_filetransfer  = 3
      invalid_type             = 4
      no_authority             = 5
      unknown_error            = 6
      header_not_allowed       = 7
      separator_not_allowed    = 8
      filesize_not_allowed     = 9
      header_too_long          = 10
      dp_error_create          = 11
      dp_error_send            = 12
      dp_error_write           = 13
      unknown_dp_error         = 14
      access_denied            = 15
      dp_out_of_memory         = 16
      disk_full                = 17
      dp_timeout               = 18
      file_not_found           = 19
      dataprovider_exception   = 20
      control_flush_error      = 21
      not_supported_by_gui     = 22
      error_no_gui             = 23
      OTHERS                   = 24.
 
  c_rc = sy-subrc.
ENDFORM.                    " save_contents_to_file
 
*&---------------------------------------------------------------------*
*&      Form  open_file_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UC_FILE    text
*----------------------------------------------------------------------*
FORM open_file_name CHANGING uc_file TYPE c.
 
  DATA: lt_filetable  TYPE filetable,
        ld_str        TYPE STRING,
        ld_rc         TYPE i.
 
* Отображаем стандартный диалог открытия файла
  ld_str = 'Выберите файл для загрузки'(011).
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title   = ld_str
      file_filter    = 'Text files|*.txt'
      multiselection = ' '
    CHANGING
      file_table     = lt_filetable[]
      rc             = ld_rc.                               "#EC NOTEXT
 
* Записываем имя файла в параметр
  IF sy-subrc = 0.
    READ TABLE lt_filetable INDEX 1 INTO uc_file.
  ENDIF.
ENDFORM.                    "open_file_name
*&---------------------------------------------------------------------*
*&      Form  main_export
*&---------------------------------------------------------------------*
*       Главная процедура, выполняющая экспорт
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM main_export.
 
  DATA: lt_contents        TYPE TABLE OF lty_tabline,
        lv_rc              TYPE sysubrc,
        lt_log             TYPE TABLE OF bapiret2,
        lv_text            TYPE STRING.
 
  FIELD-SYMBOLS: <ls_sel_table>   LIKE LINE OF s_tables.
 
  IF s_tables[] IS INITIAL.
    MESSAGE e000(clhp) WITH 'Ни одной таблицы не выбрано'(012).
  ENDIF.
 
* Формируем файл на основе всех таблиц во внутренней таблице
  LOOP AT s_tables ASSIGNING <ls_sel_table>.
 
    CONCATENATE 'Подготовка таблицы'(013) <ls_sel_table>-LOW INTO lv_text SEPARATED BY SPACE.
    PERFORM statusbar_message USING 0 lv_text.
 
    PERFORM add_table_to_file
                USING
                   <ls_sel_table>-LOW
                CHANGING
                   lt_contents[]
                   lv_rc.
 
    IF lv_rc <> 0.
      PERFORM log_msg CHANGING lt_log[].
      PERFORM show_log USING lt_log[].
      RETURN.
    ENDIF.
  ENDLOOP.
 
  lv_text = 'Сохранение данных в файл'(014).
  PERFORM statusbar_message USING 0 lv_text.
 
* Сохраняем сформированную таблицу в файл
  PERFORM save_contents_to_file
              USING
                 p_file
                 lt_contents[]
              CHANGING
                  lv_rc.
 
  IF lv_rc <> 0.
    PERFORM log_msg CHANGING lt_log[].
    PERFORM show_log USING lt_log[].
    RETURN.
  ENDIF.
 
  MESSAGE s000(clhp) WITH 'Данные успешно выгружены в файл'(015) p_file.
ENDFORM.                    " main_export
*&---------------------------------------------------------------------*
*&      Form  main_import
*&---------------------------------------------------------------------*
*       Главная процедура, выполняющая импорт
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM main_import.
 
  DATA: lt_contents          TYPE TABLE OF lty_tabline,
        lv_rc                TYPE sysubrc,
        lt_names             TYPE TABLE OF STRING,
        lv_name              TYPE dd02l-tabname,
        lt_log               TYPE TABLE OF bapiret2,
        lt_return            TYPE TABLE OF bapiret2,
        lv_global_rc         TYPE sysubrc,
        lv_text              TYPE STRING,
        lv_lines             TYPE i,
        lv_ans               TYPE char1.
 
  lv_text = 'Загрузка данных'(016).
  PERFORM statusbar_message USING 0 lv_text.
 
* Load file to internal table
  PERFORM load_contents_from_file
              USING
                 p_file
              CHANGING
                 lt_contents
                 lv_rc.
 
  IF lv_rc <> 0.
    PERFORM log_msg CHANGING lt_log[].
    PERFORM show_log USING lt_log[].
    RETURN.
  ENDIF.
 
  lv_text = 'Проверка данных'(017).
  PERFORM statusbar_message USING 0 lv_text.
 
* List tables stored in file
  PERFORM get_all_tabnames_from_file
              USING
                 lt_contents
              CHANGING
                 lt_names.
 
  IF lt_names[] IS INITIAL.
    PERFORM msg_to_ret2_string USING 'В файле не найдено ни одной таблицы'(018) 'E' CHANGING lt_log[].
    PERFORM show_log USING lt_log[].
    RETURN.
  ENDIF.
 
* Check each table, table structure must match
  LOOP AT lt_names INTO lv_name.
 
    PERFORM check_table_structure
                USING
                   lv_name
                   lt_contents[]
                CHANGING
                   lt_return
                   lv_rc.
 
    IF lv_rc <> 0.
      APPEND LINES OF lt_return TO lt_log.
      lv_global_rc = lv_rc.
    ENDIF.
  ENDLOOP.
 
  IF lv_global_rc <> 0.
    PERFORM msg_to_ret2_string USING 'При загрузке файла были ошибки'(019) 'E' CHANGING lt_log[].
    PERFORM show_log USING lt_log[].
    RETURN.
  ENDIF.
 
* Delete tables not matching the selection
  LOOP AT lt_names INTO lv_name.
    IF lv_name NOT IN s_tables.
      DELETE lt_names.
    ENDIF.
  ENDLOOP.
 
  IF lt_names[] IS INITIAL.
    PERFORM msg_to_ret2_string USING 'Ни один из содержащихся в файле таблиц не подходит под критерии выбора, ничего не загружено'(020) 'W'
          CHANGING lt_log[].
    PERFORM show_log USING lt_log[].
    RETURN.
  ENDIF.
 
  IF lv_global_rc <> 0.
    PERFORM show_log USING lt_log[].
    RETURN.
  ENDIF.
 
* Several user asks and dialogs
  lv_lines = LINES( lt_names ).
  lv_text = lv_lines.
  CONDENSE lv_text.
  CONCATENATE 'Всего из файла будет загружено'(022) lv_text 'таблиц. Продолжить загрузку?'(023)
    INTO lv_text SEPARATED BY SPACE.
 
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
*     TITLEBAR                    = ' '
*     DIAGNOSE_OBJECT             = ' '
      text_question               = lv_text
*     TEXT_BUTTON_1               = 'Ja'(001)
*     ICON_BUTTON_1               = ' '
*     TEXT_BUTTON_2               = 'Nein'(002)
*     ICON_BUTTON_2               = ' '
*     DEFAULT_BUTTON              = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN                = 25
*     START_ROW                   = 6
*     POPUP_TYPE                  =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      answer                      = lv_ans
*   TABLES
*     PARAMETER                   =
    EXCEPTIONS
      OTHERS                      = 1.
 
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 
  IF lv_ans <> '1'.
    MESSAGE s000(clhp) WITH 'Загрузка отменена пользователем'(024).
    RETURN.
  ENDIF.
 
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
*     TITLEBAR                    = ' '
*     DIAGNOSE_OBJECT             = ' '
      text_question               = 'Хотите ли вы посмотреть список загружаемых таблиц?'(025)
*     TEXT_BUTTON_1               = 'Ja'(001)
*     ICON_BUTTON_1               = ' '
*     TEXT_BUTTON_2               = 'Nein'(002)
*     ICON_BUTTON_2               = ' '
*     DEFAULT_BUTTON              = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN                = 25
*     START_ROW                   = 6
*     POPUP_TYPE                  =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      answer                      = lv_ans
*   TABLES
*     PARAMETER                   =
    EXCEPTIONS
      OTHERS                      = 1.
 
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 
  IF lv_ans = '1'.
 
    CALL FUNCTION 'POPUP_WITH_TABLE'
      EXPORTING
        endpos_col   = 50
        endpos_row   = 30
        startpos_col = 10
        startpos_row = 10
        titletext    = 'Список загружаемых таблиц'(026)
      TABLES
        valuetab     = lt_names[]
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.
 
    IF sy-subrc <> 0 AND sy-subrc <> 1.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
 
* Perform load
  LOOP AT lt_names INTO lv_name.
 
    CONCATENATE 'Загрузка данных таблицы'(027) lv_name INTO lv_text SEPARATED BY SPACE.
    PERFORM statusbar_message USING 0 lv_text.
 
    PERFORM modify_dbtab_from_file
                USING
                   lv_name
                   lt_contents[]
                   p_del
                CHANGING
                   lv_rc.
 
    IF lv_rc <> 0.
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK
      PERFORM log_msg CHANGING lt_log[].
      PERFORM show_log USING lt_log[].
      RETURN.
    ENDIF.
  ENDLOOP.
 
  COMMIT WORK.
 
  lv_text = lv_lines.
  CONDENSE lv_text.
  MESSAGE i000(clhp) WITH 'Содержимое'(028) lv_text 'таблиц успешно загружено'(029).
ENDFORM.                    " main_import
*&---------------------------------------------------------------------*
*&      Form  show_log
*&---------------------------------------------------------------------*
*       Показывает лог на экран
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_log USING ut_log  TYPE bapiret2_t.
 
  CALL FUNCTION 'SUSR_DISPLAY_LOG'
    EXPORTING
      display_in_popup           = abap_true
      log_title                  = 'Журнал обработки программы'(030)
    TABLES
*     IT_LOG_SPROT               =
      it_log_bapiret2            = ut_log[]
    EXCEPTIONS
      OTHERS                     = 1.
 
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " show_log
 
*&---------------------------------------------------------------------*
*&      Form  statusbar_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UD_PERCENT text
*      -->UD_TEXT    text
*----------------------------------------------------------------------*
FORM statusbar_message USING ud_percent ud_text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = ud_percent
      text       = ud_text.
ENDFORM.                    "statusbar_message
 
*&---------------------------------------------------------------------*
*&      Form  msg_to_ret2_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_MESSAGE_TEXT  text
*      -->VALUE(I_MSGTY)  text
*      -->CT_RETURN       text
*----------------------------------------------------------------------*
FORM msg_to_ret2_string USING i_message_text
                              VALUE(i_msgty) TYPE symsgty
                        CHANGING ct_return  TYPE bapiret2_t.
 
  DATA: ls_msg      TYPE symsg,
        lv_len      TYPE i,
        lv_dummy    TYPE bapiret2-MESSAGE.
 
  IF i_msgty <> 'I'
    AND i_msgty <> 'S'
    AND i_msgty <> 'W'
    AND i_msgty <> 'E'
    AND i_msgty <> 'A'
    AND i_msgty <> 'X'.
 
    i_msgty = 'S'.
  ENDIF.
 
  ls_msg-msgid = 'CLHP'.
  ls_msg-msgno = '000'.
  ls_msg-msgty = i_msgty.
 
  lv_len = STRLEN( i_message_text ).
  IF lv_len <= 50.
    ls_msg-msgv1 = i_message_text.
  ENDIF.
  IF lv_len > 50 AND lv_len <= 100.
    ls_msg-msgv1 = i_message_text(50).
    ls_msg-msgv2 = i_message_text+50.
  ENDIF.
  IF lv_len > 100 AND lv_len <= 150.
    ls_msg-msgv1 = i_message_text(50).
    ls_msg-msgv2 = i_message_text+50(50).
    ls_msg-msgv3 = i_message_text+100.
  ENDIF.
  IF lv_len > 150.
    ls_msg-msgv1 = i_message_text(50).
    ls_msg-msgv2 = i_message_text+50(50).
    ls_msg-msgv3 = i_message_text+100(50).
    ls_msg-msgv4 = i_message_text+150.
  ENDIF.
 
  MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
    WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO lv_dummy.
 
  PERFORM log_msg CHANGING ct_return[].
ENDFORM.                    "msg_to_ret2_string
 
*&---------------------------------------------------------------------*
*&      Form  log_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CT_RETURN  text
*----------------------------------------------------------------------*
FORM log_msg CHANGING ct_return      TYPE bapiret2_t.
 
  FIELD-SYMBOLS: <ls_return>    TYPE bapiret2.
 
  APPEND INITIAL LINE TO ct_return ASSIGNING <ls_return>.
  <ls_return>-ID         = sy-msgid.
  <ls_return>-NUMBER     = sy-msgno.
  <ls_return>-TYPE       = sy-msgty.
  <ls_return>-message_v1 = sy-msgv1.
  <ls_return>-message_v2 = sy-msgv2.
  <ls_return>-message_v3 = sy-msgv3.
  <ls_return>-message_v4 = sy-msgv4.
ENDFORM.                    "log_msg
