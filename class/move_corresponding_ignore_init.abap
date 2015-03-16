CLASS lcl_utilities DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      move_corresponding_ignore_init
        IMPORTING
          i_str_source TYPE ANY
        CHANGING
          c_str_dest   TYPE ANY.
ENDCLASS.
 
CLASS lcl_utilities IMPLEMENTATION.
  METHOD move_corresponding_ignore_init.
    DATA:
      l_rcl_abap_structdescr TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS:
      <fs_str_component> LIKE LINE OF l_rcl_abap_structdescr->COMPONENTS,
      <fs_source_field>  TYPE ANY,
      <fs_dest_field>    TYPE ANY.
 
    l_rcl_abap_structdescr ?= cl_abap_typedescr=>describe_by_data( i_str_source ).
    LOOP AT l_rcl_abap_structdescr->COMPONENTS ASSIGNING <fs_str_component>.
      ASSIGN COMPONENT <fs_str_component>-NAME OF STRUCTURE c_str_dest TO <fs_dest_field>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT <fs_str_component>-NAME OF STRUCTURE i_str_source TO <fs_source_field>.
        ASSERT sy-subrc = 0.
        IF <fs_source_field> IS NOT INITIAL.
          <fs_dest_field> = <fs_source_field>.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "move_corresponding_ignore_init
ENDCLASS.
 
DEFINE move_corresponding_ignore_init.
  lcl_utilities=>move_corresponding_ignore_init(
    EXPORTING
      i_str_source = &1
    CHANGING
      c_str_dest   = &2
  ).
END-OF-DEFINITION.
