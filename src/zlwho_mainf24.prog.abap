*&---------------------------------------------------------------------*
*&      Form  rebundle_unassign_to
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*      -->P_LT_ORDIM_O  text
*      -->P_LO_LOG  text
*----------------------------------------------------------------------*
********************************************************************
*& Key           : <AAHMEDOV>-23.06.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
*& unassign TOs from current warehouse orders
*    this implies deletion/confirmation of WOs
*    additionally not needed pick HUs will be deleted
********************************************************************
*----------------------------------------------------------------------*
*& INCLUDE ZLWHO_MAINF24
*& Original Object: /SCWM/LWHO_MAINF24
*----------------------------------------------------------------------*
FORM rebundle_unassign_to  USING iv_lgnum       TYPE /scwm/lgnum
                                 iv_keep_pickhu TYPE xfeld
                                 it_ordim_o     TYPE /scwm/tt_ordim_o.

  DATA: lv_wo    TYPE /scwm/ordim_o-who,
        lv_mtext TYPE string.
  DATA: ls_bapiret  TYPE bapiret2.
  DATA: lt_ordim_o  TYPE /scwm/tt_ordim_o,
        lt_tanum    TYPE /scwm/tt_tanum,
        lt_bapiret  TYPE bapirettab,
        lt_bapiret2 TYPE bapirettab,
        ls_who      TYPE /scwm/s_who_int.
  FIELD-SYMBOLS: <fs_ordim_o> TYPE /scwm/ordim_o.

  lt_ordim_o = it_ordim_o.
* sort TOs by WO because UNASSIGN has to be called per WO
  SORT lt_ordim_o BY who.

  LOOP AT lt_ordim_o ASSIGNING <fs_ordim_o>.
    IF sy-tabix > 1.
      IF lv_wo NE <fs_ordim_o>-who AND
         lv_wo IS NOT INITIAL.
        TRY.
            CALL FUNCTION '/SCWM/WHO_TO_UNASSIGN'
              EXPORTING
                iv_lgnum       = iv_lgnum
                iv_who         = lv_wo
                iv_update      = abap_false
                iv_commit      = abap_false
                iv_keep_pickhu = iv_keep_pickhu
                it_to          = lt_tanum
              IMPORTING
                et_bapiret     = lt_bapiret.

            PERFORM update_global_who_parameters USING iv_lgnum lv_wo.
*
          CATCH /scwm/cx_core.
        ENDTRY.
        APPEND LINES OF lt_bapiret TO lt_bapiret2.
        CLEAR: lt_tanum.
*       don't clear global tables for further calls of WHO_TO_UNASSIGN
        CLEAR: gv_clear_global.

      ENDIF.
    ENDIF.

    APPEND <fs_ordim_o>-tanum TO lt_tanum.
    lv_wo = <fs_ordim_o>-who.

  ENDLOOP.

  IF lt_tanum IS NOT INITIAL AND
     <fs_ordim_o>-who IS NOT INITIAL.
    TRY.
        CALL FUNCTION '/SCWM/WHO_TO_UNASSIGN'
          EXPORTING
            iv_lgnum       = iv_lgnum
            iv_who         = <fs_ordim_o>-who
            iv_update      = abap_false
            iv_commit      = abap_false
            iv_keep_pickhu = iv_keep_pickhu
            it_to          = lt_tanum
          IMPORTING
            et_bapiret     = lt_bapiret.

        PERFORM update_global_who_parameters USING iv_lgnum <fs_ordim_o>-who.
*
      CATCH /scwm/cx_core.
    ENDTRY.
    APPEND LINES OF lt_bapiret TO lt_bapiret2.
    CLEAR: lt_tanum.
  ENDIF.
* clear global table with next run
  gv_clear_global = abap_true.

* changes will be posted together with new WOs

* add messages to global WO log
  LOOP AT lt_bapiret2 INTO ls_bapiret.
    MESSAGE ID ls_bapiret-id TYPE ls_bapiret-type
            NUMBER ls_bapiret-number
            WITH ls_bapiret-message_v1 ls_bapiret-message_v2
                 ls_bapiret-message_v3 ls_bapiret-message_v4
            INTO lv_mtext.
    PERFORM who_write_log USING gc_msg_lvl2 CHANGING gs_log-handle.
  ENDLOOP.
ENDFORM.                    " rebundle_unassign_to
*&---------------------------------------------------------------------*
*&      Form  rebundle_create_wo
*&---------------------------------------------------------------------*
*    Create WOs for marked TOs
*    all TOs have to be part of the same aarea and queue
*    => create one warehouse order per aarea/queue
*----------------------------------------------------------------------*
*      -->P_LGNUM  text
*      -->P_LT_ORDIMO  text
*      -->P_LO_LOG  text
*----------------------------------------------------------------------*
FORM rebundle_create_wo
           USING iv_lgnum       TYPE /scwm/lgnum
                 is_wo_template TYPE /scwm/s_who_int
                 it_orig_pickhu TYPE /scwm/tt_whohu_int
                 it_ordim_o     TYPE /scwm/tt_ordim_o
        CHANGING et_wo          TYPE /scwm/tt_who_int
                 et_change_att  TYPE /scwm/tt_to_change_att.
  DATA: lv_tabix    TYPE sytabix,
        lv_mtext    TYPE string,
        lv_whoseq   TYPE i,
        lv_no_print TYPE abap_bool.
  DATA: ls_change_att TYPE /scwm/s_to_change_att,
        ls_changed    TYPE /scwm/s_changed,
        ls_wcr        TYPE /scwm/twcr,
        ls_limsum     TYPE /scwm/s_wholimsum,
        ls_wo         TYPE /scwm/s_who_int,
        ls_to         TYPE /scwm/s_ordim_o_int,
        ls_last_to    TYPE /scwm/ordim_o.
  DATA: lt_ordim_o    TYPE /scwm/tt_ordim_o,
        lt_to         TYPE /scwm/tt_ordim_o_int,
        lt_to_print   TYPE /scwm/tt_ordim_o_int,
        lt_changed    TYPE /scwm/tt_changed,
        lt_change_att TYPE /scwm/tt_to_change_att,
        lt_wo         TYPE /scwm/tt_who_int.
  FIELD-SYMBOLS: <fs_ordim_o> TYPE /scwm/ordim_o.

  lt_ordim_o = it_ordim_o.
* sort TOs by aarea/queue because WOs may only contain one aarea/queue
* and pathseq
  SORT lt_ordim_o BY aarea queue pathseq.

  LOOP AT lt_ordim_o ASSIGNING <fs_ordim_o>.

    MOVE-CORRESPONDING <fs_ordim_o> TO ls_to.
    APPEND ls_to TO lt_to.

    ADD 1 TO ls_limsum-items.
*   sum up planned duration of WO
    ADD <fs_ordim_o>-solpo TO ls_limsum-reachtime.
    IF ls_limsum-unit_t IS INITIAL.
      MOVE: <fs_ordim_o>-zeiei TO ls_limsum-unit_t.
    ENDIF.

*   append TOs to change table for later change
*     assignment to pick HUs will be deleted
    lv_whoseq = lv_whoseq + 1.
    MOVE: 'WHO'                TO ls_changed-fieldname,
          ''                   TO ls_changed-value_c.
    APPEND ls_changed          TO lt_changed.
    IF it_orig_pickhu IS INITIAL.
      MOVE: 'HUID'               TO ls_changed-fieldname.
      APPEND ls_changed          TO lt_changed.
    ENDIF.
    MOVE: 'WHOSEQ'             TO ls_changed-fieldname,
          lv_whoseq            TO ls_changed-value_c.
    APPEND ls_changed          TO lt_changed.
    MOVE: <fs_ordim_o>-tanum   TO ls_change_att-tanum,
          lt_changed           TO ls_change_att-tt_changed.
    APPEND ls_change_att TO lt_change_att.
    CLEAR: ls_change_att, ls_changed, lt_changed.

    ls_last_to = <fs_ordim_o>.
  ENDLOOP.
  IF ls_limsum IS NOT INITIAL.
    PERFORM rebundle_create_wo_int USING iv_lgnum is_wo_template
                                         it_orig_pickhu
                                CHANGING ls_limsum
                                         <fs_ordim_o>
                                         ls_last_to lt_to
                                         et_wo lt_change_att.
    APPEND LINES OF lt_change_att TO et_change_att.
    APPEND LINES OF lt_to TO lt_to_print.
    CLEAR: lt_change_att, lt_to, lv_whoseq.
  ENDIF.

  "check whether printing is required based on settings on queue/rsrc
  lt_wo = et_wo.
  LOOP AT lt_wo ASSIGNING FIELD-SYMBOL(<ls_wo>).
    lv_tabix = sy-tabix.
    CLEAR: lv_no_print.
    PERFORM print_check USING iv_lgnum
                              <ls_wo>-queue
                              <ls_wo>-rsrc  "theoretically, rsrc could be pre-assigned
                     CHANGING lv_no_print.
    IF lv_no_print = abap_true.
      DELETE lt_to_print WHERE lgnum = iv_lgnum
                         AND   who   = <ls_wo>-who.
      DELETE lt_wo INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

* Print documents for warehouse order (at TO creation)
  IF lt_wo IS NOT INITIAL.
    CALL FUNCTION '/SCWM/PRINT_WO'
      EXPORTING
        it_who            = lt_wo
        it_ordim_o        = lt_to_print
        iv_tostep         = wmegc_to_step_create
      EXCEPTIONS
        no_previous_print = 1
        error_on_log_save = 2
        previous_print    = 3
        OTHERS            = 4.

*   Exception 'Previous print' is okay in this process
    IF sy-subrc <> 0 AND sy-subrc <> 3.
      PERFORM who_write_log USING gc_msg_lvl1 CHANGING gs_log-handle.
    ENDIF.
  ENDIF.
ENDFORM.                    " rebundle_create_wo
*&---------------------------------------------------------------------*
*&      Form  rebundle_create_wo_int
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_LGNUM  text
*      -->P_LS_LIMSUM  text
*      -->P_LS_LAST_TO  text
*      -->P_LS_TO1ST  text
*      -->P_LT_TO  text
*      <--P_ET_WO  text
*      <--P_ET_CHANGE_ATT  text
*      <--P_ENDIF  text
*----------------------------------------------------------------------*
FORM rebundle_create_wo_int
                      USING iv_lgnum       TYPE /scwm/lgnum
                            is_wo_template TYPE /scwm/s_who_int
                            it_orig_pickhu TYPE /scwm/tt_whohu_int
                   CHANGING cs_limsum      TYPE /scwm/s_wholimsum
                            cs_to          TYPE /scwm/ordim_o
                            cs_last_to     TYPE /scwm/ordim_o
                            ct_to          TYPE /scwm/tt_ordim_o_int
                            et_wo          TYPE /scwm/tt_who_int
                            et_change_att  TYPE /scwm/tt_to_change_att.
  DATA: lv_mtext       TYPE string.
  DATA: ls_changed     TYPE /scwm/s_changed,
        ls_wcr         TYPE /scwm/twcr,
        ls_to          TYPE /scwm/s_ordim_o_int,
        ls_wo          TYPE /scwm/s_who_int,
        ls_ewl_message TYPE /scwm/if_api_message=>ys_message ##NEEDED.
  DATA: lt_wo_rsrc     TYPE /scwm/tt_wo_rsrc_ty. "dummy
  DATA: lt_wrkl        TYPE /scwm/tt_wrkl_int.
  FIELD-SYMBOLS: <fs_change_att> TYPE /scwm/s_to_change_att.

* Activity area lv_aarea used
  MESSAGE i076 WITH cs_last_to-aarea INTO lv_mtext.
  PERFORM who_write_log USING gc_msg_lvl2
                     CHANGING gs_log-handle.
* Queue lv_queue used
  MESSAGE i077 WITH cs_last_to-queue INTO lv_mtext.
  PERFORM who_write_log USING gc_msg_lvl2
                     CHANGING gs_log-handle.
* create structure for WO
  PERFORM who_structure_create
       USING iv_lgnum ls_wcr cs_limsum ct_to
    CHANGING ls_wo.
* a new creation time will be used
  GET TIME STAMP FIELD ls_wo-created_at.
  MOVE: abap_true TO ls_wo-flgto.
* inform resource management about new WO
  PERFORM rsrcmgmt_inform USING iv_lgnum abap_false
                       CHANGING ls_wo lt_wo_rsrc ct_to.
  "if applicable take over pickHUs of former WO(s)
  PERFORM take_over_pick_hu USING iv_lgnum ls_wo it_orig_pickhu.
  "take over WO template fields if applicable
  PERFORM take_over_wo_template USING is_wo_template
                             CHANGING ls_wo et_change_att.

  "In rebundling we have to set the CREATED_BY again.
  ls_wo-created_by = sy-uname.

  MOVE: 'WHO' TO ls_changed-fieldname,
        ls_wo-who TO ls_changed-value_c.
  LOOP AT et_change_att ASSIGNING <fs_change_att>.
    MODIFY <fs_change_att>-tt_changed FROM ls_changed
      TRANSPORTING fieldname value_c WHERE fieldname = 'WHO'.
  ENDLOOP.
  ls_to-who = ls_wo-who.
  MODIFY ct_to FROM ls_to TRANSPORTING who
    WHERE lgnum = iv_lgnum.
  APPEND ls_wo TO et_wo.

* Create Planned Workload for LM
  PERFORM wrkl_who USING iv_lgnum
                         wmegc_wrkl_create
                         abap_false
                CHANGING ct_to
                         et_wo
                         lt_wrkl
                         ls_ewl_message.

* warehouse order created
  MESSAGE s012 WITH ls_wo-who '' cs_limsum-items INTO lv_mtext.
  PERFORM who_write_log USING gc_msg_lvl2
                     CHANGING gs_log-handle.
  CLEAR: cs_limsum, ls_wcr.
ENDFORM.                    " rebundle_create_wo_int
*&---------------------------------------------------------------------*
*&      Form  rebundle_create_wo_int
*&---------------------------------------------------------------------*
*       take over some attributes of a template (potentially
*       predecessor WO) into the newly created WO.
*       Please note that there are fields that are not taken over
*       because they were determined anew before.
*
*       Also take over WOCR of template into WTs
*----------------------------------------------------------------------*
FORM take_over_wo_template
                      USING is_wo_template TYPE /scwm/s_who_int
                   CHANGING cs_wo          TYPE /scwm/s_who_int
                            ct_change_att  TYPE /scwm/tt_to_change_att.

  IF is_wo_template IS NOT INITIAL.
    DATA(ls_wo_temp) = cs_wo.
    cs_wo = is_wo_template.

    "keep fields that were determined anew
    "(do this this way to take over potential appends)
    cs_wo-areawho     = ls_wo_temp-areawho.
    cs_wo-changed_at  = ls_wo_temp-changed_at.
    cs_wo-changed_by  = ls_wo_temp-changed_by.
    cs_wo-created_at  = ls_wo_temp-created_at.
    cs_wo-created_by  = ls_wo_temp-created_by.
    cs_wo-db_lock     = ls_wo_temp-db_lock.
    cs_wo-flginv      = ls_wo_temp-flginv.
    cs_wo-flgsplit    = ls_wo_temp-flgsplit.
    cs_wo-flgto       = ls_wo_temp-flgto.
    cs_wo-flgwho      = ls_wo_temp-flgwho.
    cs_wo-lgnum       = ls_wo_temp-lgnum.
    cs_wo-lgpla       = ls_wo_temp-lgpla.
    cs_wo-lgtyp       = ls_wo_temp-lgtyp.
    cs_wo-lsd         = ls_wo_temp-lsd.
    cs_wo-man_assign  = ls_wo_temp-man_assign.
    cs_wo-mandt       = ls_wo_temp-mandt.
    cs_wo-plandura    = ls_wo_temp-plandura.
    cs_wo-queue       = ls_wo_temp-queue.
    cs_wo-refwhoid    = ls_wo_temp-refwhoid.
    cs_wo-splitwhoid  = ls_wo_temp-splitwhoid.
    cs_wo-started_at  = ls_wo_temp-started_at.
    cs_wo-start_fixed = ls_wo_temp-start_fixed.
    cs_wo-status      = ls_wo_temp-status.
    cs_wo-topwhoid    = ls_wo_temp-topwhoid.
    cs_wo-updkz       = ls_wo_temp-updkz.
    cs_wo-who         = ls_wo_temp-who.
    cs_wo-whoid       = ls_wo_temp-whoid.
    cs_wo-whologno    = ls_wo_temp-whologno.

    IF is_wo_template-wcr IS NOT INITIAL.
      LOOP AT ct_change_att ASSIGNING FIELD-SYMBOL(<ls_change_att>).
        APPEND INITIAL LINE TO <ls_change_att>-tt_changed ASSIGNING FIELD-SYMBOL(<ls_changed>).
        <ls_changed>-fieldname = 'WCR'.
        <ls_changed>-value_c   = is_wo_template-wcr.
      ENDLOOP.
    ENDIF.

    "add a message to the log about taking over attributes
    MESSAGE i435 WITH is_wo_template-who INTO DATA(lv_mtext).
    PERFORM who_write_log USING gc_msg_lvl2
                       CHANGING gs_log-handle.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  map_wo_log2bapiret
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_EV_SEVERITY  text
*      <--P_ET_BAPIRET  text
*----------------------------------------------------------------------*
FORM map_wo_log2bapiret  CHANGING ev_severity TYPE bapi_mtype
                                  et_bapiret  TYPE bapirettab.
  DATA: ls_log     TYPE bal_s_msg,
        ls_bapiret TYPE bapiret2.

* move messages to bapiret
  LOOP AT gt_log INTO ls_log.
    MOVE: ls_log-msgty TO ls_bapiret-type,
          ls_log-msgid TO ls_bapiret-id,
          ls_log-msgno TO ls_bapiret-number,
          ls_log-msgv1 TO ls_bapiret-message_v1,
          ls_log-msgv2 TO ls_bapiret-message_v2,
          ls_log-msgv3 TO ls_bapiret-message_v3,
          ls_log-msgv4 TO ls_bapiret-message_v4.
    MESSAGE ID ls_bapiret-id TYPE ls_bapiret-type
            NUMBER ls_bapiret-number
            WITH ls_bapiret-message_v1 ls_bapiret-message_v2
                 ls_bapiret-message_v3 ls_bapiret-message_v4
            INTO ls_bapiret-message.
    APPEND ls_bapiret TO et_bapiret.
    CLEAR: ls_bapiret.
*   severity
    CHECK ev_severity NE 'A'.
    CASE ls_log-msgty.
      WHEN 'A'.
        ev_severity = ls_log-msgty.
      WHEN 'E'.
        ev_severity = ls_log-msgty.
      WHEN 'W'.
        IF ev_severity <> 'E'.
          ev_severity = ls_log-msgty.
        ENDIF.
      WHEN 'I'.
        IF ev_severity CN 'EWA'.
          ev_severity = ls_log-msgty.
        ENDIF.
    ENDCASE.
  ENDLOOP. "gt_log
ENDFORM.                    " map_wo_log2bapiret
*&---------------------------------------------------------------------*
*&      Form  PRINT_CHECK
*&---------------------------------------------------------------------*
*       check if printing is necessary at all
*       pre-check agains queue and resource for performance reasons
*----------------------------------------------------------------------*
FORM print_check  USING    iv_lgnum     TYPE /scwm/lgnum
                           iv_queue     TYPE /scwm/de_queue
                           iv_rsrc      TYPE /scwm/de_rsrc
                  CHANGING ev_no_print  TYPE xfeld.

  DATA: ls_t346 TYPE /scwm/t346,
        ls_rsrc TYPE /scwm/rsrc.

  CLEAR: ev_no_print.

  IF iv_rsrc IS NOT INITIAL.

    CALL FUNCTION '/SCWM/RSRC_READ_SINGLE'
      EXPORTING
        iv_lgnum = iv_lgnum
        iv_rsrc  = iv_rsrc
      IMPORTING
        es_rsrc  = ls_rsrc
      EXCEPTIONS
        OTHERS   = 0.

    IF ls_rsrc-rsrc_print = '2'.           "no print at all
      ev_no_print = abap_true.
    ENDIF.
  ENDIF.

  IF ls_rsrc-rsrc_print IS INITIAL AND "check queue
     iv_queue           IS NOT INITIAL.

    CALL FUNCTION '/SCWM/T346_READ_SINGLE'
      EXPORTING
        iv_lgnum = iv_lgnum
        iv_queue = iv_queue
      IMPORTING
        es_t346  = ls_t346
      EXCEPTIONS
        OTHERS   = 0.

    IF ls_t346-queue_print = '1'.          "no print at all
      ev_no_print = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.                    " PRINT_CHECK

FORM update_global_who_parameters USING iv_lgnum TYPE /scwm/lgnum
                                        iv_who   TYPE /scwm/de_who.
  DATA: ls_who   TYPE /scwm/s_who_int,
        lt_whohu TYPE /scwm/tt_whohu_int.
  TRY.
      CALL FUNCTION '/SCWM/WHO_GET'
        EXPORTING
          iv_lgnum = iv_lgnum
          iv_whoid = iv_who
        IMPORTING
          es_who   = ls_who
          et_whohu = lt_whohu.
    CATCH /scwm/cx_core.
  ENDTRY.

  IF ls_who IS NOT INITIAL.
    ASSIGN gt_who[ who = ls_who-who ] TO FIELD-SYMBOL(<ls_who_ref>).
    IF sy-subrc = 0.
      <ls_who_ref> = ls_who.
    ELSE.
      APPEND ls_who TO gt_who.
    ENDIF.
  ENDIF.

  LOOP AT lt_whohu ASSIGNING FIELD-SYMBOL(<ls_whohu>).
    ASSIGN gt_whohu[ who = <ls_whohu>-who hukng = <ls_whohu>-hukng ] TO FIELD-SYMBOL(<ls_whohu_global>).
    IF sy-subrc = 0.
      <ls_whohu_global> = <ls_whohu>.
    ELSE.
      APPEND <ls_whohu> TO gt_whohu.
    ENDIF.
  ENDLOOP.

ENDFORM.
