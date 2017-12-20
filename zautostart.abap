*&---------------------------------------------------------------------*
*& Report ZAUTOSTART
*&---------------------------------------------------------------------*
*&
*& start transactions from a folder of your favorites
*&
*& written by Michael Keller
*&
*&---------------------------------------------------------------------*

REPORT zautostart.

CLASS lcl_autostart DEFINITION.

  PUBLIC SECTION.
    TYPES favorites TYPE TABLE OF smen_buffc.

    CLASS-METHODS run.

  PRIVATE SECTION.
    CLASS-METHODS do_prechecks
      IMPORTING
        iv_tcode      TYPE syst_tcode
      EXPORTING
        ev_report     TYPE raldb_repo
        ev_var_exists TYPE abap_bool
      EXCEPTIONS
        error.

    CLASS-METHODS get_favorites_from_folder
      IMPORTING
        iv_folder    TYPE char100sm
      EXPORTING
        et_favorites TYPE favorites.

ENDCLASS.

CLASS lcl_autostart IMPLEMENTATION.

  METHOD do_prechecks.

    DATA: ls_tstc    TYPE tstc,
          lv_message TYPE text100,
          lv_report  TYPE raldb_repo,
          lv_variant TYPE raldb_vari,
          lv_subrc   TYPE sysubrc.

    " check if transaction code exists
    SELECT SINGLE *
           FROM tstc
           INTO ls_tstc
           WHERE tcode = iv_tcode.

    IF sy-subrc <> 0.
      lv_message = 'Transaction code &1 is invalid.'.
      REPLACE FIRST OCCURRENCE OF '&1' IN lv_message WITH iv_tcode.
      MESSAGE lv_message TYPE 'I'.
      RAISE error.
    ENDIF.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = iv_tcode
      EXCEPTIONS
        ok     = 1
        not_ok = 2
        OTHERS = 3.

    IF sy-subrc > 1. " a little bit special
      lv_message = 'No authorization for transaction code &1.'.
      REPLACE FIRST OCCURRENCE OF '&1' IN lv_message WITH iv_tcode.
      MESSAGE lv_message TYPE 'I'.
      RAISE error.
    ENDIF.

    lv_report = ls_tstc-pgmna.
    lv_variant = sy-uname.

    CALL FUNCTION 'RS_VARIANT_EXISTS'
      EXPORTING
        report              = lv_report
        variant             = lv_variant
      IMPORTING
        r_c                 = lv_subrc
      EXCEPTIONS
        not_authorized      = 1
        no_report           = 2
        report_not_existent = 3
        report_not_supplied = 4
        OTHERS              = 5.

    IF lv_subrc = 0.
      ev_report     = lv_report.
      ev_var_exists = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_favorites_from_folder.

    DATA: lt_favorites TYPE favorites,
          ls_favorites LIKE LINE OF lt_favorites,
          lv_object_id TYPE menu_num_5.

    CALL FUNCTION 'NAVIGATION_LOAD_FAVORITES'
      EXPORTING
        user_name     = sy-uname
      TABLES
        favorites_tab = lt_favorites.
*       FAVORITES_FOR_GUI       =
*       LINKS_LIST              =

    " look for the folder
    LOOP AT lt_favorites INTO ls_favorites
                         WHERE reporttype = space
                         AND   report     = space
                         AND   text IS NOT INITIAL.
      TRANSLATE ls_favorites-text TO UPPER CASE.
      IF ls_favorites-text = iv_folder.
        lv_object_id = ls_favorites-object_id.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF sy-subrc <> 0 OR lv_object_id IS INITIAL.
      RETURN.
    ENDIF.

    " reduce to relevant transactions
    DELETE lt_favorites WHERE parent_id <> lv_object_id
                        OR    reporttype <> 'TR'
                        OR    report = sy-tcode.

    IF lt_favorites IS INITIAL.
      RETURN.
    ENDIF.

    " sort favorites, important for starting sequence
    SORT lt_favorites BY sort_order ASCENDING.
    READ TABLE lt_favorites INTO ls_favorites INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    APPEND ls_favorites TO lt_favorites.
    DELETE lt_favorites INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    et_favorites = lt_favorites.

  ENDMETHOD.


  METHOD run.

    DATA: lt_parts     TYPE TABLE OF syst_tcode,
          lv_lines     TYPE i,
          lv_folder    TYPE char100sm,
          lt_favorites TYPE favorites,
          ls_favorites TYPE smen_buffc,
          lv_tcode     TYPE syst_tcode,
          lv_task      TYPE text100,
          lv_report    TYPE raldb_repo,
          lv_use_var   TYPE abap_bool,
          lv_answer    TYPE char1,
          lv_question  TYPE itex132,
          lv_tabix     TYPE sytabix.

    FIELD-SYMBOLS <favorites> TYPE smen_buffc.

    " get name of favorites folder from transaction code
    SPLIT sy-tcode AT '_' INTO TABLE lt_parts.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_lines = lines( lt_parts ).
    READ TABLE lt_parts INTO lv_folder INDEX lv_lines.
    IF sy-subrc <> 0 OR lv_folder IS INITIAL.
      RETURN.
    ENDIF.

    get_favorites_from_folder(
      EXPORTING
        iv_folder = lv_folder
      IMPORTING
        et_favorites = lt_favorites ).

    IF lt_favorites IS INITIAL.
      RETURN.
    ENDIF.

    lv_lines = lines( lt_favorites ).

    CONCATENATE 'Start transactions from folder "' lv_folder '"?' INTO lv_question.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Autostart'
*       DIAGNOSE_OBJECT       = ' '
        text_question         = lv_question
        text_button_1         = 'Yes'
        icon_button_1         = 'ICON_EXECUTE_OBJECT'
        text_button_2         = 'Yes, delayed'
        icon_button_2         = 'ICON_TIME'
        default_button        = '2'
        display_cancel_button = 'X'
*       USERDEFINED_F1_HELP   = ' '
*       START_COLUMN          = 25
*       START_ROW             = 6
*       POPUP_TYPE            =
*       IV_QUICKINFO_BUTTON_1 = ' '
*       IV_QUICKINFO_BUTTON_2 = ' '
      IMPORTING
        answer                = lv_answer
*     TABLES
*       PARAMETER             =
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc <> 0 OR lv_answer = 'A'.
      RETURN.
    ENDIF.

    " find all transaction codes in folder and start them
    LOOP AT lt_favorites ASSIGNING <favorites>.
      CLEAR: lv_report,
             lv_use_var.

      lv_tabix = sy-tabix.
      lv_tcode = <favorites>-report.
      lv_task  = lv_tcode.

      do_prechecks(
        EXPORTING
          iv_tcode = lv_tcode
        IMPORTING
          ev_report     = lv_report
          ev_var_exists = lv_use_var
        EXCEPTIONS
          error  = 1
          OTHERS = 2 ).

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF lv_tabix <> lv_lines.
        CALL FUNCTION 'Z_AUTOSTART'
          STARTING NEW TASK lv_task
          DESTINATION IN GROUP DEFAULT
          EXPORTING
            iv_tcode              = lv_tcode
            iv_report             = lv_report
            iv_use_var            = lv_use_var
          EXCEPTIONS
            system_failure        = 1
            communication_failure = 2
            resource_failure      = 3
            OTHERS                = 4.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF lv_lines > 2 AND lv_answer = '2'.
          WAIT UP TO 3 SECONDS.
        ELSE.
          " do not start too fast
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ELSE.
        CALL FUNCTION 'Z_AUTOSTART'
          EXPORTING
            iv_tcode   = lv_tcode
            iv_report  = lv_report
            iv_use_var = lv_use_var.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_autostart=>run( ).
