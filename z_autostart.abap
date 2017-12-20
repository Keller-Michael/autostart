FUNCTION z_autostart.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TCODE) TYPE  SYST_TCODE
*"     VALUE(IV_REPORT) TYPE  RALDB_REPO
*"     VALUE(IV_USE_VAR) TYPE  XFELD
*"  EXCEPTIONS
*"      SYSTEM_FAILURE
*"      COMMUNICATION_FAILURE
*"      RESOURCE_FAILURE
*"----------------------------------------------------------------------

  IF iv_use_var = abap_false AND iv_tcode IS NOT INITIAL.
    CALL TRANSACTION iv_tcode.
  ENDIF.

  IF iv_use_var = abap_true AND iv_report IS NOT INITIAL.
    SUBMIT (iv_report) USING SELECTION-SET sy-uname.
  ENDIF.
