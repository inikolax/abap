DATA: temp_dir   TYPE STRING,
      sapworkdir TYPE STRING.
 
cl_gui_frontend_services=>get_temp_directory(
  CHANGING
    temp_dir             = temp_dir    " Temporary Directory
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
    OTHERS               = 4
).
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
