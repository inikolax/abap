********************************************************************
* Report for generating XML-file from class
* Author: Sergey Korolev (slkorolev@mail.ru)
* Does not work with interfaces and exception classes
********************************************************************
REPORT yrs_class_download.

TABLES: t002.

PARAMETERS: class TYPE seoclskey OBLIGATORY.

SELECTION-SCREEN SKIP.

PARAMETERS: p_show AS CHECKBOX.
SELECT-OPTIONS: s_langu FOR t002-spras
                        DEFAULT sy-langu
                        NO INTERVALS.

TYPE-POOLS: seok, seop.

INCLUDE yrs_class_xml_const.

DATA:
  g_xml TYPE REF TO cl_xml_document,

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

  gt_source TYPE  seop_source.

DATA:
  typkey TYPE seocmpkey,
  single_source TYPE seop_source,
  type_source TYPE seop_source.

FIELD-SYMBOLS:
  <type> TYPE seoo_type_r,
  <source_line> TYPE LINE OF seop_source,
  <info> TYPE seok_cls_typeinfo_by_vis.

FIELD-SYMBOLS:
  <include> TYPE LINE OF seop_methods_w_include.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR class.
  DATA:
    clsname TYPE seoclass-clsname.
  clsname = class.

  CALL FUNCTION 'F4_OBJECTS_CLASS'
    EXPORTING
      object = clsname
    IMPORTING
      RESULT = clsname.

  class = clsname.

AT SELECTION-SCREEN ON class.
  DATA:
    clskey  TYPE  seoclskey.

  clskey-clsname = class.

* Check if active version of class exists
  CALL FUNCTION 'SEO_CLASS_GET'
    EXPORTING
      clskey       = clskey
      version      = '1'
      state        = '1'
    EXCEPTIONS
      not_existing = 1
      deleted      = 2
      is_interface = 3
      model_only   = 4
      OTHERS       = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

START-OF-SELECTION.
  CALL FUNCTION 'SEO_CLASS_TYPEINFO_GET'
    EXPORTING
      clskey        = class
      version       = seoc_version_active
    IMPORTING
      class         = gs_class
      attributes    = gt_attributes
      methods       = gt_methods
      events        = gt_events
      types         = gt_types
      PARAMETERS    = gt_parameters
      exceps        = gt_exceps
      implementings = gt_implementings
      inheritance   = gs_inheritance
      redefinitions = gt_redefinitions
      impl_details  = gt_impl_details
      friendships   = gt_friendships
      typepusages   = gt_typepusages
      clsdeferrds   = gt_clsdeferrds
      intdeferrds   = gt_intdeferrds
      aliases       = gt_aliases
    EXCEPTIONS
      not_existing  = 1
      is_interface  = 2
      model_only    = 3
      OTHERS        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Load all method include names
  CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
    EXPORTING
      clskey                       = class
    IMPORTING
      includes                     = gs_includes
    EXCEPTIONS
      _internal_class_not_existing = 1
      OTHERS                       = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Gathering together internal type source
  LOOP AT gt_types ASSIGNING <type>.

    typkey-clsname = <type>-clsname.
    typkey-cmpname = <type>-cmpname.

    REFRESH single_source.

    CALL FUNCTION 'SEO_CLASS_GET_TYPE_SOURCE'
      EXPORTING
        typkey                       = typkey
      IMPORTING
        SOURCE                       = single_source
      EXCEPTIONS
        _internal_class_not_existing = 1
        not_existing                 = 2
        not_edited                   = 3
        OTHERS                       = 4.

    CHECK sy-subrc = 0 AND NOT single_source[] IS INITIAL.

    <type>-srcrow1 = LINES( type_source ) + 1.
    <type>-srccolumn1 = 0.

    LOOP AT single_source ASSIGNING <source_line>.
      CONDENSE <source_line>.
      <type>-srccolumn2 = STRLEN( <source_line> ).
      APPEND <source_line> TO type_source.
    ENDLOOP.

    <type>-srcrow2 = LINES( type_source ).


  ENDLOOP.

END-OF-SELECTION.
  DATA:
    mtd_name TYPE string,
    class_root TYPE REF TO if_ixml_node,
    section    TYPE REF TO if_ixml_node,
    l_description TYPE sdok_descr.

  l_description = gs_class-descript.

  CREATE OBJECT g_xml
    EXPORTING
      description = l_description
      object_type = cl_xml_document=>c_bor_classtype
      object_name = gs_class-clsname.

* Simply generate sections of XML document


* First create root section
  g_xml->set_data( name       = c_xml_root
                   dataobject = gs_class ).

  class_root = g_xml->find_node( name   = c_xml_root ).

* All other sections will be under the root (see parent_node)
  DEFINE set_section_data.
    g_xml->set_data( name = &1
                     parent_node = class_root
                     dataobject  = &2 ).
  END-OF-DEFINITION.

  set_section_data:
    c_xml_attributes    gt_attributes,
    c_xml_methods       gt_methods,
    c_xml_events        gt_events,
    c_xml_types         gt_types,
    c_xml_parameters    gt_parameters,
    c_xml_exceps        gt_exceps,
    c_xml_implementings gt_implementings,
    c_xml_impl_details  gt_impl_details,
    c_xml_friendships   gt_friendships,
    c_xml_typepusages   gt_typepusages,
    c_xml_clsdeferrds   gt_clsdeferrds,
    c_xml_intdeferrds   gt_intdeferrds,
    c_xml_aliases       gt_aliases,
    c_xml_inheritance   gs_inheritance,
    c_xml_interfaces    gt_interfaces,
    c_xml_redefinitions gt_redefinitions,
    c_xml_typesource    type_source.

* Extract local definitions (classes, macros)
  DEFINE set_locals.
    refresh single_source.
    refresh single_source.
    call function 'SEO_CLASS_GET_INCLUDE_SOURCE'
      exporting
        clskey                       = class
        inctype                      = &1
      importing
        source                       = single_source
      exceptions
        _internal_class_not_existing = 0
        not_existing                 = 0
        others                       = 0.

    g_xml->set_data( name = &2
                     parent_node = class_root
                     dataobject  = single_source ).

  END-OF-DEFINITION.

  DATA:
    include_name TYPE programm.

  CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_BY_NAME'
    EXPORTING
      clskey   = class
      limu     = seok_limu_locals
    IMPORTING
      progname = include_name.

  set_locals:
    include_name c_xml_locals_src,
    seop_ext_class_locals_def c_xml_locals_def,
    seop_ext_class_locals_imp c_xml_locals_imp,
    seop_ext_class_macros c_xml_locals_mac.

* Extract text pool
  DATA:
    name TYPE string,
    gt_texts TYPE TABLE OF textpool.

  CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_BY_NAME'
    EXPORTING
      clskey   = class
    IMPORTING
      progname = include_name.

  section = g_xml->create_simple_element( name   = c_xml_text_pool
                                          parent = class_root ).

  SELECT spras FROM t002 INTO t002-spras.
    REFRESH gt_texts.
    READ TEXTPOOL include_name INTO gt_texts
                               LANGUAGE t002-spras.

    CHECK sy-subrc = 0.

    name = t002-spras.
    g_xml->set_data( name = name
                     parent_node = section
                     dataobject  = gt_texts ).

  ENDSELECT.

* Create section for method source.
* Then each metod as a single node under the common section
  section = g_xml->create_simple_element( name   = c_xml_method_src
                                          parent = class_root ).

  LOOP AT gs_includes ASSIGNING <include>.

    REFRESH gt_source.
    CALL FUNCTION 'SEO_METHOD_GET_SOURCE'
      EXPORTING
        mtdkey                        = <include>-cpdkey
        state                         = 'A'
      IMPORTING
        SOURCE                        = gt_source
      EXCEPTIONS
        _internal_method_not_existing = 1
        _internal_class_not_existing  = 2
        version_not_existing          = 3
        inactive_new                  = 4
        inactive_deleted              = 5
        OTHERS                        = 6.


    CHECK sy-subrc = 0.

    mtd_name = <include>-cpdkey-cpdname.

    g_xml->set_data( name = mtd_name
                     parent_node = section
                     dataobject  = gt_source[] ).

  ENDLOOP.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Now display resulting XML-file or export it into workstation file
  mtd_name = class.

  IF p_show = 'X'.
    g_xml->display( ).
    EXIT.
  ENDIF.

  DATA:
    user_action TYPE i,
    path TYPE string,
    full_path TYPE string,
    file_name TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension    = 'xml'
      default_file_name    = mtd_name
      file_filter          = cl_gui_frontend_services=>filetype_xml
    CHANGING
      filename             = file_name
      path                 = path
      fullpath             = full_path
      user_action          = user_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK user_action = cl_gui_frontend_services=>action_ok.

  DATA:
    localfile TYPE localfile.

  localfile = full_path.

  g_xml->export_to_file( filename = localfile ).
