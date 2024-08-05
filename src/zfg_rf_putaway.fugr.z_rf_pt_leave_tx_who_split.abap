FUNCTION z_rf_pt_leave_tx_who_split.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"     REFERENCE(CS_ADMIN) TYPE  /SCWM/S_RF_ADMIN
*"     REFERENCE(CT_PTWY) TYPE  /SCWM/TT_RF_PTWY
*"--------------------------------------------------------------------

  BREAK-POINT ID /scwm/rf_putaway.

*--------------------------------------------------------------
* Data Definition
*--------------------------------------------------------------
  DATA: lv_sev                TYPE bapi_mtype,              "#EC NEEDED
        lv_help_confirmed_tos TYPE xfeld.

  DATA: ls_ptwy_att TYPE /scwm/s_rf_ptwy_att,
        ls_whoid    TYPE /scwm/s_whoid,
        lt_whoid    TYPE /scwm/tt_whoid.

  DATA: lt_ordim_o TYPE /scwm/tt_ordim_o,
        lt_ordim_c TYPE /scwm/tt_ordim_c,
        lt_new_who TYPE /scwm/tt_who_int,
        ls_rsrc    TYPE /scwm/rsrc,
        ls_s_rsrc  TYPE /scwm/s_rsrc,
        lt_who_int TYPE /scwm/tt_who_int.
  DATA: lv_queue TYPE /scwm/de_queue.
  DATA: lv_work_hu TYPE /scwm/de_huident.
*--------------------------------------------------------------
* Program Logic
*--------------------------------------------------------------

* 1) any WHOs for the splitting?
* 2) Select open WTs
* 3) Question: Send rest of work back to pool?
* 4) Yes: Split open WTs from WHO
* 5) No: unassign WHO from resource

*1) Anything to split?
*---------------------------------------------------
  LOOP AT ct_ptwy INTO ls_ptwy_att.
    IF ls_ptwy_att-processed IS INITIAL.
      IF ls_ptwy_att-who IS NOT INITIAL.
        ls_whoid-who   = ls_ptwy_att-who.
        APPEND ls_whoid   TO lt_whoid.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT lt_whoid.
  DELETE ADJACENT DUPLICATES FROM lt_whoid.

* 2) Select open WTs
*---------------------------------------------------
  IF lt_whoid IS NOT INITIAL.
    TRY.
        CALL FUNCTION '/SCWM/WHO_SELECT'
          EXPORTING
            iv_to      = 'X'
            iv_lgnum   = cs_admin-lgnum
*           iv_who     = iv_who
            it_who     = lt_whoid
          IMPORTING
            et_who     = lt_who_int
            et_ordim_o = lt_ordim_o
            et_ordim_c = lt_ordim_c.
      CATCH /scwm/cx_core.                              "#EC NO_HANDLER
    ENDTRY.

* 3) Question: Send rest of work back to pool?
*---------------------------------------------------
    IF lt_ordim_c IS NOT INITIAL.
      LOOP AT lt_ordim_o TRANSPORTING NO FIELDS WHERE vlpla IS NOT INITIAL.
        EXIT.
      ENDLOOP.

      IF sy-subrc IS INITIAL.
* 4) Yes: Split open WTs from WHO and unassign whos
        PERFORM who_split_and_unassign IN PROGRAM /scwm/saplrf_putaway
            USING    cs_admin-lgnum
                     'X'          " unassign from rsrc
                     lt_whoid
                     lt_ordim_o
            CHANGING lt_new_who.
      ENDIF.
    ELSE.
*     No confirmed WT, unassign the WO from rsrc if possible
      CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
        EXPORTING
          iv_uname = sy-uname
        CHANGING
          cs_rsrc  = ls_rsrc.

      MOVE-CORRESPONDING ls_rsrc TO ls_s_rsrc.

      CALL FUNCTION '/SCWM/RSRC_WHO_UNASSIGN'
        EXPORTING
          iv_clear_start = 'X'
          iv_check_split = ' '
          is_rsrc        = ls_s_rsrc
          it_who         = lt_who_int
        EXCEPTIONS
          error_message  = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF. "/scwm/cl_rf_dynpro_srvc=.....
  ENDIF. "lt_whoid IS NOT INITIAL.

*     Requesting RF Framework to release locks to cover cases where
*     locking of WO was done before calling RF picking transaction.
  /scwm/cl_rf_bll_srvc=>set_flg_dequeue_all( ).

  CLEAR lv_queue.
  SET PARAMETER ID '/SCWM/WKSYSQ_QUEUE' FIELD lv_queue.

  SET PARAMETER ID '/SCWM/WORK_WHO' FIELD space.
  SET PARAMETER ID '/SCWM/WORK_HU' FIELD lv_work_hu.

  DATA   lv_einame TYPE c LENGTH 60.
  CONCATENATE '/SCWM/' sy-uname 'WKSYSQ' INTO lv_einame.
  EXPORT lv_queue FROM lv_queue TO MEMORY ID lv_einame.

* set fcode
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
    EXPORTING
      iv_fcode = zif_rfui_c=>gs_fcode-cmpltx.

ENDFUNCTION.
