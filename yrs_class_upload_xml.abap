********************************************************************
* Report for generating class from XML-file
* Author: Sergey Korolev (slkorolev@mail.ru)
* Does not work with interfaces and exception classes
********************************************************************
REPORT  yrs_class_upload_xml                                        .

PARAMETERS:
  xmlfile TYPE localfile.

TYPE-POOLS: seok, seop.

INCLUDE yrs_class_xml_const.

DATA:
  corr_mode,

* Extracted structures (from XML file)
  gt_locals_src    TYPE	seop_source_string,
  gt_locals_def    TYPE	seop_source_string,
  gt_locals_imp    TYPE	seop_source_string,
  gt_locals_mac    TYPE	seop_source_string,

  gs_class  TYPE  vseoclass,
  gt_attributes TYPE  seoo_attributes_r,
  gt_methods  TYPE  seoo_methods_r,
  gt_events TYPE  seoo_events_r,
  gt_types  TYPE  seoo_types_r,
  gt_parameters TYPE  seos_parameters_r,
  gt_exceps TYPE  seos_exceptions_r,
  gt_implementings  TYPE  seor_implementings_r,
  gs_inheritance  TYPE  vseoextend,
  gt_redefinitions  TYPE  seor_redefinitions_r,
  gt_impl_details TYPE  seor_redefinitions_r,
  gt_friendships  TYPE  seof_friendships_r,
  gt_typepusages  TYPE  seot_typepusages_r,
  gt_clsdeferrds  TYPE  seot_clsdeferrds_r,
  gt_intdeferrds  TYPE  seot_intdeferrds_r,
  gt_aliases  TYPE  seoo_aliases_r,
  gt_interfaces TYPE  seok_int_typeinfos,
  gs_includes TYPE  seop_methods_w_include,

  gt_texts TYPE TABLE OF textpool,

  gt_source TYPE  seop_source,
  gt_type_source     TYPE	seop_source,
  gt_mtd_source   TYPE seo_method_source_table,
  wa_mtd_source   TYPE seo_method_source.


CLASS lcx_xml_error DEFINITION DEFERRED.

DATA:
  ex TYPE REF TO lcx_xml_error.

*----------------------------------------------------------------------*
*       CLASS lCX_xml_error  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_xml_error DEFINITION
  INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    DATA: error TYPE string.
    METHODS: constructor IMPORTING value(i_error) TYPE string OPTIONAL.
ENDCLASS.                    "lCX_xml_error  DEFINITIO

*----------------------------------------------------------------------*
*       CLASS lcx_xml_section_error  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_xml_section_error DEFINITION
  INHERITING FROM lcx_xml_error.
  PUBLIC SECTION.
   METHODS: constructor IMPORTING value(i_section) TYPE string OPTIONAL
   .
ENDCLASS.                    "lCX_xml_error  DEFINITIO

*----------------------------------------------------------------------*
*       CLASS lcx_xml_error IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_xml_error IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    error = i_error.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcx_xml_error IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcx_xml__section_error IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_xml_section_error IMPLEMENTATION.
  METHOD constructor.
    DATA:
      msg TYPE string.
    CONCATENATE 'File does not contain obligatory section'
                i_section
                INTO msg
                SEPARATED BY space.
    CALL METHOD super->constructor
      EXPORTING
        i_error = msg.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcx_xml__section_error IMPLEMENTATION

AT SELECTION-SCREEN ON VALUE-REQUEST FOR xmlfile.
  DATA:
    file_table  TYPE filetable,
    action TYPE i,
    rc TYPE sysubrc.

  FIELD-SYMBOLS:
    <file> TYPE file_table.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      default_extension       = 'xml'
      file_filter             = cl_gui_frontend_services=>filetype_xml
    CHANGING
      file_table              = file_table
      rc                      = rc
      user_action             = action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK action = cl_gui_frontend_services=>action_ok.

  READ TABLE file_table INDEX 1 ASSIGNING <file>.

  xmlfile = <file>.

START-OF-SELECTION.
  DATA:
    retcode TYPE sysubrc,
    source_node TYPE REF TO if_ixml_node,
    xml TYPE REF TO cl_xml_document.

  CREATE OBJECT xml.

  TRY.
      CALL METHOD xml->import_from_file
        EXPORTING
          filename = xmlfile
        RECEIVING
          retcode  = retcode.

      IF retcode NE xml->c_ok.
        DATA:
          msg TYPE string.
        CASE retcode.
          WHEN xml->c_no_ixml.
            msg = 'This is not XML file'.
          WHEN xml->c_failed.
            msg = 'Failed to parse the XML file'.
          WHEN xml->c_not_found.
            msg = 'XML file not found'.
        ENDCASE.
        RAISE EXCEPTION TYPE lcx_xml_error
          EXPORTING i_error = msg.
      ENDIF.

      DEFINE load_section.
        call method xml->get_data
          exporting
            name       = &1
          importing
            retcode    = retcode
          changing
            dataobject = &2.

        if retcode ne xml->c_ok.
          raise exception type lcx_xml_section_error
            exporting i_section = &1.
        endif.
      END-OF-DEFINITION.

      load_section:
        c_xml_root gs_class,

        c_xml_attributes gt_attributes,
        c_xml_methods gt_methods,
        c_xml_events gt_events,
        c_xml_types gt_types,
        c_xml_parameters gt_parameters,
        c_xml_exceps gt_exceps,
        c_xml_implementings gt_implementings,
        c_xml_impl_details gt_impl_details,
        c_xml_friendships gt_friendships,
        c_xml_typepusages gt_typepusages,
        c_xml_clsdeferrds gt_clsdeferrds,
        c_xml_intdeferrds gt_intdeferrds,
        c_xml_aliases gt_aliases,
        c_xml_inheritance gs_inheritance,
        c_xml_interfaces gt_interfaces,
        c_xml_redefinitions gt_redefinitions,

        c_xml_typesource gt_type_source,

*        c_xml_text_pool gt_texts,

        c_xml_locals_src gt_locals_src,
        c_xml_locals_def gt_locals_def,
        c_xml_locals_mac gt_locals_mac,
        c_xml_locals_imp gt_locals_imp.

* Now find method implementation node containing sources
* for each method as a subnode
      CALL METHOD xml->find_node
        EXPORTING
          name = c_xml_method_src
        RECEIVING
          node = source_node.

      IF source_node IS INITIAL.
        RAISE EXCEPTION TYPE lcx_xml_section_error
          EXPORTING i_section = c_xml_methods.
      ENDIF.

      source_node = source_node->get_first_child( ).

      DATA:
        name1 TYPE string,
        name2 TYPE string,
        xml_sep(5) VALUE '_--7E'.

      WHILE NOT source_node IS INITIAL.

        CLEAR wa_mtd_source.

        name1 = source_node->get_name( ).
        SPLIT name1 AT xml_sep INTO name1 name2.

        IF name2 IS INITIAL.
          wa_mtd_source-cpdname = name1.
        ELSE.
          CONCATENATE name1 name2 INTO wa_mtd_source-cpdname
                      SEPARATED BY '~'.
        ENDIF.

        CALL METHOD xml->get_node_data
          EXPORTING
            node       = source_node
          IMPORTING
            dataobject = wa_mtd_source-source
            retcode    = retcode.

        IF retcode = xml->c_ok.
          APPEND wa_mtd_source TO gt_mtd_source.
        ENDIF.

        source_node = source_node->get_next( ).
      ENDWHILE.

    CATCH lcx_xml_error INTO ex.
      MESSAGE ex->error TYPE 'E'.
  ENDTRY.

  DATA:
    answer,
    fields TYPE TABLE OF sval WITH HEADER LINE,
    parameter	TYPE TABLE OF spar WITH HEADER LINE,
    clskey TYPE seoclskey.

  clskey-clsname = gs_class-clsname.

  corr_mode = 'I'.
  DO.
    CALL FUNCTION 'SEO_CLASS_GET'
      EXPORTING
        clskey       = clskey
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        is_interface = 3
        model_only   = 4
        OTHERS       = 5.

    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    REFRESH parameter.
    parameter-param = 'CLASS'.
    parameter-value = clskey-clsname.
    APPEND parameter.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Class already exists'
        text_question         =
        'Class &CLASS& already exists. Overwrite?'
        default_button        = '2'
      IMPORTING
        answer                = answer
      TABLES
        parameter             = parameter
      EXCEPTIONS
        text_not_found        = 0
        OTHERS                = 0.

    CASE answer.
      WHEN '1'.
        corr_mode = 'U'.
        EXIT.
      WHEN 'A'.
        RETURN.
    ENDCASE.

    REFRESH fields.
    fields-tabname = 'SEOCLSKEY'.
    fields-fieldname = 'CLSNAME'.
    fields-value = clskey-clsname.
    APPEND fields.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Enter new class name'
      IMPORTING
        returncode      = answer
      TABLES
        fields          = fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF answer = 'A'.
      RETURN.
    ENDIF.

    READ TABLE fields INDEX 1.
    clskey-clsname = fields-value.

  ENDDO.

  IF clskey-clsname NE gs_class-clsname.
    gs_class-clsname = clskey-clsname.
    gs_class-author = sy-uname.
    gs_class-changedby = sy-uname.
    gs_class-changedon = sy-datum.

    gs_inheritance-clsname = clskey-clsname.

    PERFORM rename_class TABLES gt_implementings USING clskey-clsname.
    PERFORM rename_class TABLES gt_attributes USING clskey-clsname.
    PERFORM rename_class TABLES gt_methods USING clskey-clsname.
    PERFORM rename_class TABLES gt_events USING clskey-clsname.
    PERFORM rename_class TABLES gt_types USING clskey-clsname.
    PERFORM rename_class TABLES gt_parameters USING clskey-clsname.
    PERFORM rename_class TABLES gt_exceps USING clskey-clsname.
    PERFORM rename_class TABLES gt_aliases USING clskey-clsname.
    PERFORM rename_class TABLES gt_typepusages USING clskey-clsname.
    PERFORM rename_class TABLES gt_clsdeferrds USING clskey-clsname.
    PERFORM rename_class TABLES gt_intdeferrds USING clskey-clsname.

    PERFORM rename_class TABLES gt_redefinitions USING clskey-clsname.
    PERFORM rename_class TABLES gt_impl_details USING clskey-clsname.
    PERFORM rename_class TABLES gt_friendships USING clskey-clsname.
  ENDIF.

  DATA:
    devclass  LIKE  tadir-devclass,
    korrnum LIKE  e070-trkorr.

  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object              = gs_class-clsname
      object_class        = 'CLAS'
      mode                = corr_mode
      global_lock         = 'X'
      master_language     = gs_class-langu
    IMPORTING
      devclass            = devclass
      korrnum             = korrnum
    EXCEPTIONS
      cancelled           = 1
      permission_failure  = 2
      unknown_objectclass = 3
      OTHERS              = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA:
    overwrite TYPE seox_boolean.

  IF corr_mode = 'U'.
    overwrite = seox_true.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Generating class definition'.

  CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
    EXPORTING
      corrnr          = korrnum
      devclass        = devclass
      version         = seoc_version_inactive
      overwrite       = overwrite
    IMPORTING
      korrnr          = korrnum
    CHANGING
      class           = gs_class
      inheritance     = gs_inheritance
      redefinitions   = gt_redefinitions
      implementings   = gt_implementings
      impl_details    = gt_impl_details
      attributes      = gt_attributes
      methods         = gt_methods
      events          = gt_events
      types           = gt_types
      type_source     = gt_type_source
      PARAMETERS      = gt_parameters
      exceps          = gt_exceps
      aliases         = gt_aliases
      typepusages     = gt_typepusages
      clsdeferrds     = gt_clsdeferrds
      intdeferrds     = gt_intdeferrds
      friendships     = gt_friendships
    EXCEPTIONS
      existing        = 1
      is_interface    = 2
      db_error        = 3
      component_error = 4
      no_access       = 5
      other           = 6
      OTHERS          = 7.

  IF sy-subrc <> 0.
    MESSAGE 'Error generating class' TYPE 'E'.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Generating local class definitions'.

  CALL FUNCTION 'SEO_CLASS_GENERATE_LOCALS'
    EXPORTING
      clskey                 = clskey
      force                  = seox_true
      corrnr                 = korrnum
      implementation         = gt_locals_src
      locals_def             = gt_locals_def
      locals_imp             = gt_locals_imp
      locals_mac             = gt_locals_mac
    EXCEPTIONS
      not_existing           = 1
      model_only             = 2
      locals_not_generated   = 3
      locals_not_initialised = 4
      OTHERS                 = 5.

  IF sy-subrc <> 0.
    MESSAGE 'Error generating local class definitions' TYPE 'E'.
  ENDIF.

  FIELD-SYMBOLS:
    <mtd_source> TYPE seo_method_source.

  DATA:
    status_line(128),
    mtdkey TYPE seocpdkey.

  LOOP AT gt_mtd_source ASSIGNING <mtd_source>.

    CONCATENATE 'Generating implementation of the method'
                <mtd_source>-cpdname
                INTO status_line
                SEPARATED BY space.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = status_line.

    mtdkey-clsname = clskey-clsname.
    mtdkey-cpdname = <mtd_source>-cpdname.
    CALL FUNCTION 'SEO_METHOD_GENERATE_INCLUDE'
      EXPORTING
        mtdkey                         = mtdkey
        force                          = seox_true
        suppress_corr                  = seox_true
        implementation_expanded        = <mtd_source>-source
        corrnr                         = korrnum
        without_method_frame           = seox_true
      EXCEPTIONS
        not_existing                   = 1
        model_only                     = 2
        include_existing               = 3
        method_imp_not_generated       = 4
        method_imp_not_initialised     = 5
        _internal_class_not_existing   = 6
        _internal_method_overflow      = 7
        cancelled                      = 8
        method_is_abstract_implemented = 9
        method_is_final_implemented    = 10
        internal_error_insert_report   = 11
        OTHERS                         = 12.

*    IF sy-subrc <> 0.
*      MESSAGE 'Error generating class' TYPE 'E'.
*    ENDIF.
  ENDLOOP.

* Now find text pool node containing textpools for
* different languages as subnodes
  DATA:
    langu TYPE sy-langu,
    include_name TYPE programm.

  TRY.
      CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_BY_NAME'
        EXPORTING
          clskey   = clskey
        IMPORTING
          progname = include_name.

      CALL METHOD xml->find_node
        EXPORTING
          name = c_xml_text_pool
        RECEIVING
          node = source_node.

      IF source_node IS INITIAL.
        RAISE EXCEPTION TYPE lcx_xml_section_error
          EXPORTING i_section = c_xml_text_pool.
      ENDIF.

      source_node = source_node->get_first_child( ).

      WHILE NOT source_node IS INITIAL.
        langu = source_node->get_name( ).

        REFRESH gt_texts.
        CALL METHOD xml->get_node_data
          EXPORTING
            node       = source_node
          IMPORTING
            dataobject = gt_texts
            retcode    = retcode.

        source_node = source_node->get_next( ).

        CHECK retcode = xml->c_ok
        AND   NOT gt_texts IS INITIAL.

        CONCATENATE 'Generating class text pool for language'
                    langu
                    INTO msg
                    SEPARATED BY space.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            text = msg.

        INSERT textpool include_name
               FROM gt_texts
               LANGUAGE langu.
      ENDWHILE.

    CATCH lcx_xml_section_error.
      MESSAGE 'XML-file does not contain Text pool data' TYPE 'S'.
  ENDTRY.

  SET PARAMETER ID 'CLASS' FIELD clskey-clsname.
  CALL TRANSACTION 'SE24'.

*&---------------------------------------------------------------------*
*&      Form  rename_class
*&---------------------------------------------------------------------*
*       Renames class references in arbitrary table
*----------------------------------------------------------------------*
*      -->P_TABLE    some generation structure
*      -->P_NAME     new class name
*----------------------------------------------------------------------*
FORM rename_class  TABLES   p_table
                   USING    p_name.
  FIELD-SYMBOLS:
    <field> TYPE ANY,
    <line> TYPE ANY.

  DEFINE set_field.
    assign component &1 of structure <line> to <field>.
    if sy-subrc = 0.
      <field> = &2.
    endif.
  END-OF-DEFINITION.

  LOOP AT p_table ASSIGNING <line>.
    set_field 'CLSNAME' p_name.
    set_field 'AUTHOR' sy-uname.
    set_field 'CHANGEDBY' sy-uname.
  ENDLOOP.
ENDFORM.                    " rename_class
