*----------------------------------------------------------------------*
*& INCLUDE ZLWHO_MAINF50
*& Original Object: /SCWM/LWHO_MAINF50
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form REBUNDLE_CHECK_PICKHU
*&---------------------------------------------------------------------*
*&  Allow the caller to specify that pickHUs of old WOs should be
*&  taken over for the new WO.
*&
*&  !!! It is in the responsibility of the caller to verify
*&      that this makes sense.  !!!
*&
*&  only allow taking over pick HUs if all WTs will get into one WO
*&  => have the same aarea and queue
*&  additionally, special WO rules are not allowed
*&  there must not be confirmed WTs for any of the WOs
*&  all WTs of involved WOs have to be rebundled
*&---------------------------------------------------------------------*

********************************************************************
*& Key           : BSUGAREV-23.06.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
*----------------------------------------------------------------------*
*& INCLUDE ZLWHO_MAINF50
*& Original Object: /SCWM/LWHO_MAINF50
*----------------------------------------------------------------------*
FORM rebundle_check_pickhu
                USING iv_lgnum         TYPE /scwm/lgnum
                      iv_keep_pickhu   TYPE xfeld
                      it_ordim_o       TYPE /scwm/tt_ordim_o
             CHANGING cv_keep_pickhu   TYPE xfeld
                      ct_orig_pickhu   TYPE /scwm/tt_whohu_int.

  DATA: lt_who        TYPE /scwm/tt_who_int,
        lt_ordim_o_wo TYPE /scwm/tt_ordim_o,
        lt_ordim_c_wo TYPE /scwm/tt_ordim_c.

  CLEAR: cv_keep_pickhu, ct_orig_pickhu.

  IF iv_keep_pickhu = abap_false.
    "nothing to do
    RETURN.
  ELSE.
    cv_keep_pickhu = iv_keep_pickhu.
  ENDIF.

  "message that pick HUs should be taken over
  MESSAGE i436(/scwm/who) INTO DATA(lv_mtext).
  PERFORM who_write_log USING gc_msg_lvl2 CHANGING gs_log-handle.

  "there must not be different aareas or queues
  DATA(lt_ordim_o) = it_ordim_o.

  "there must not be any special WO creation rules used
  "(e.g. pick, pack, pass with pickHUS at top WO)
  lt_ordim_o = it_ordim_o.
  SORT lt_ordim_o BY who.
  DELETE ADJACENT DUPLICATES FROM lt_ordim_o COMPARING who.
  TRY .
      "lock WO and WT, so that FM /SCWM/WHO_TO_UNASSIGN can make use of buffer
      CALL FUNCTION '/SCWM/WHO_GET'
        EXPORTING
          iv_lgnum    = iv_lgnum
          iv_to       = abap_true
          iv_lock_who = abap_true
          iv_lock_to  = abap_true
          it_whoid    = CORRESPONDING /scwm/tt_whoid( lt_ordim_o )
        IMPORTING
          et_who      = lt_who
          et_whohu    = ct_orig_pickhu
          et_ordim_o  = lt_ordim_o_wo
          et_ordim_c  = lt_ordim_c_wo.
    CATCH /scwm/cx_core INTO DATA(lx_core).
      "in case of issues do not keep pickHUs
      CLEAR: cv_keep_pickhu, ct_orig_pickhu.
  ENDTRY.
  "try to reuse buffer in FM /SCWM/WHO_TO_UNASSIGN
  CLEAR: gv_clear_global.

  IF lt_who IS INITIAL.
    "should not happen => do not try to take over HUs if WO cannot be read
    MESSAGE e438(/scwm/who) INTO lv_mtext.
    PERFORM who_write_log USING gc_msg_lvl3 CHANGING gs_log-handle.
    CLEAR: cv_keep_pickhu, ct_orig_pickhu.
    RETURN.
  ENDIF.

  LOOP AT lt_who ASSIGNING FIELD-SYMBOL(<ls_who>).
    "we only allow WO rules of type:
    " space = e.g. default rule
    " A     = consolidation group
    " B     = pick path
    " E     = pick & pack
    IF <ls_who>-type NE wmegc_wcr_pw AND
       <ls_who>-type NE wmegc_wcr_pl AND
       <ls_who>-type NE wmegc_wcr_pp AND
       <ls_who>-type NE space.
      "message that pick HUs cannot be taken over for WOs with rule type &1
      MESSAGE w439(/scwm/who) WITH <ls_who>-type INTO lv_mtext.
      PERFORM who_write_log USING gc_msg_lvl3 CHANGING gs_log-handle.
      CLEAR: cv_keep_pickhu, ct_orig_pickhu.
      RETURN.
    ENDIF.
  ENDLOOP.

  "if there is any confirmed WT for one of the involved WOs, we do NOT
  "allow to take over pick HUs
  IF lt_ordim_c_wo IS NOT INITIAL.
    "message that pick HUs cannot be taken over for WOs with confirmed WTs
    MESSAGE w440(/scwm/who) INTO lv_mtext.
    PERFORM who_write_log USING gc_msg_lvl3 CHANGING gs_log-handle.
    CLEAR: cv_keep_pickhu, ct_orig_pickhu.
    RETURN.
  ENDIF.

  "if a not all WTs of involved WOs are rebundled taking over pick HUs is not allowed
  lt_ordim_o = it_ordim_o.
  SORT lt_ordim_o BY tanum.
  LOOP AT lt_ordim_o_wo ASSIGNING FIELD-SYMBOL(<ls_ordim_o_wo>).
    DATA(lv_tabix) = sy-tabix.
    READ TABLE lt_ordim_o TRANSPORTING NO FIELDS
         WITH KEY lgnum = iv_lgnum
                  tanum = <ls_ordim_o_wo>-tanum
         BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      DELETE lt_ordim_o_wo INDEX lv_tabix.
    ENDIF.
  ENDLOOP.
  IF lt_ordim_o_wo IS NOT INITIAL.
    "message that pick HUs cannot be taken over if not all WTs of original WOs are rebundled
    MESSAGE w441(/scwm/who) INTO lv_mtext.
    PERFORM who_write_log USING gc_msg_lvl3 CHANGING gs_log-handle.
    CLEAR: cv_keep_pickhu, ct_orig_pickhu.
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form TAKE_OVER_PICK_HUS
*&---------------------------------------------------------------------*
*&  Take over pick HUs of original WOs if applicable
*&  Checks have been executed upfront
*&---------------------------------------------------------------------*
FORM take_over_pick_hu USING iv_lgnum        TYPE /scwm/lgnum
                             is_wo           TYPE /scwm/s_who_int
                             it_orig_pickhu  TYPE /scwm/tt_whohu_int.
  IF it_orig_pickhu IS INITIAL.
    "nothing to do
    RETURN.
  ENDIF.

  "WOs have been locked by /SCWM/WO_REBUNDLE_MAN before
  DATA(lt_pickhu) = it_orig_pickhu.

  LOOP AT lt_pickhu ASSIGNING FIELD-SYMBOL(<ls_pickhu>).
    DATA(lv_tabix) = sy-tabix.
    READ TABLE gt_whohu ASSIGNING FIELD-SYMBOL(<ls_whohu>)
         WITH KEY who   = <ls_pickhu>-who
                  hukng = <ls_pickhu>-hukng.
    IF sy-subrc IS INITIAL.
      "the assumption is that this is ALWAYS the case

      "insert entry with new WO
      <ls_pickhu>-who   = is_wo-who.
      <ls_pickhu>-hukng = lv_tabix.
      <ls_pickhu>-updkz = 'I'.
      APPEND <ls_pickhu> TO gt_whohu.
      "mark old entry for deletion
      <ls_whohu>-updkz = 'D'.
    ELSE.
      "should NEVER happen - but still create entries for new WO
      <ls_pickhu>-who   = is_wo-who.
      <ls_pickhu>-hukng = lv_tabix.
      <ls_pickhu>-updkz = 'I'.
      APPEND <ls_pickhu> TO gt_whohu.
    ENDIF.
  ENDLOOP.
  SORT gt_whohu BY lgnum who hukng.

  "add a message to the log that pick HUs are taken over
  MESSAGE i442 INTO DATA(lv_mtext).
  PERFORM who_write_log USING gc_msg_lvl2
                     CHANGING gs_log-handle.
ENDFORM.
