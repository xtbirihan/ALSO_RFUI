CLASS zcl_who_rsrc_picking DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA mv_who_sel_exec TYPE boole_d .

    METHODS constructor
      IMPORTING
        !iv_lgnum TYPE /scwm/lgnum .
    METHODS rsrc_who_unassign_no_split
      IMPORTING
        !is_rsrc TYPE /scwm/s_rsrc
        !iv_who  TYPE /scwm/de_who
        !it_who  TYPE /scwm/tt_who_int OPTIONAL
      RAISING
        zcx_core_exception .
    METHODS rsrc_who_assign
      IMPORTING
        !iv_who  TYPE /scwm/de_who
        !is_rsrc TYPE /scwm/s_rsrc
      RAISING
        zcx_core_exception .
    "! <p class="shorttext synchronized" lang="en">Select WOs for queue</p>
    "! Used in RF picking processes
    "! @parameter iv_queue | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rt_wo_rsrc_ty | <p class="shorttext synchronized" lang="en"></p>
    METHODS select_first_who_for_cart
      IMPORTING
        !iv_queue            TYPE /scwm/de_queue
        !iv_rsrc             TYPE /scwm/de_rsrc
      RETURNING
        VALUE(rt_wo_rsrc_ty) TYPE /scwm/tt_wo_rsrc_ty .

    "! <p class="shorttext synchronized" lang="en">OBSOLETE</p>
    METHODS rsrc_proc_wo_select
      RETURNING
        VALUE(rt_wo_rsrc_ty) TYPE /scwm/tt_wo_rsrc_ty .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_msg_dummy TYPE string  ##NEEDED.
    DATA mv_lgnum TYPE /scwm/lgnum .

    METHODS index_table_update
      IMPORTING
        !is_rsrc TYPE /scwm/s_rsrc .
ENDCLASS.



CLASS ZCL_WHO_RSRC_PICKING IMPLEMENTATION.


  METHOD constructor.
********************************************************************
*& Key          : <AYORDANOV>-05.09.2023
*& Request No.  :
********************************************************************
*& Description
*&
********************************************************************
    mv_lgnum = iv_lgnum.

  ENDMETHOD.


  METHOD index_table_update.


    DATA: lv_resource TYPE /scwm/de_rsrc.
    DATA: ls_wo_rsrc_ty TYPE /scwm/wo_rsrc_ty,
          ls_whoid      TYPE /scwm/s_whoid,
          ls_who        TYPE /scwm/s_who_int.
    DATA: lt_wo_rsrc_ty TYPE /scwm/tt_wo_rsrc_ty,
          lt_whoid      TYPE /scwm/tt_whoid,
          lt_who        TYPE /scwm/tt_who_int.

    FIELD-SYMBOLS: <ls_wo_rsrc_ty> TYPE /scwm/wo_rsrc_ty.

    lv_resource = is_rsrc-rsrc.

* Get wo_rsrc_ty records assigned to resource
    CALL FUNCTION '/SCWM/RSRC_WHO_RSTYP_GET'
      EXPORTING
        iv_lgnum      = is_rsrc-lgnum
        iv_rfind      = is_rsrc-rfind
        iv_rsrc       = is_rsrc-rsrc
      CHANGING
        ct_wo_rsrc_ty = lt_wo_rsrc_ty.

    " protect those WOs which are assigned manually, otherwise
    " the rsrc index table will be cleared but the WO still
    " contains the rsrc
    LOOP AT lt_wo_rsrc_ty ASSIGNING <ls_wo_rsrc_ty>.
      ls_whoid-who = <ls_wo_rsrc_ty>-who.
      COLLECT ls_whoid INTO lt_whoid.
    ENDLOOP.

    TRY.
        CALL FUNCTION '/SCWM/WHO_GET'
          EXPORTING
            iv_lgnum = is_rsrc-lgnum
            it_whoid = lt_whoid
          IMPORTING
            et_who   = lt_who.
      CATCH /scwm/cx_core.
        RETURN.
    ENDTRY.

    LOOP AT lt_who INTO ls_who.
      "   do not touch the manual assignment
      IF ls_who-man_assign IS NOT INITIAL.
        DELETE lt_wo_rsrc_ty WHERE who = ls_who-who.
        CONTINUE.
      ENDIF.

      "   do not touch which is assigned to any rsrc
      IF ls_who-rsrc IS NOT INITIAL.
        DELETE lt_wo_rsrc_ty WHERE who = ls_who-who.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    " Clear records' resource and status
    LOOP AT lt_wo_rsrc_ty ASSIGNING <ls_wo_rsrc_ty>.
      CLEAR <ls_wo_rsrc_ty>-rsrc.
      CLEAR <ls_wo_rsrc_ty>-status.
    ENDLOOP.

    CHECK lt_wo_rsrc_ty IS NOT INITIAL.

    " Update table /scwm/wo_rsrc_ty
    CALL FUNCTION '/SCWM/WHO_RSRC_TYP_SET'
      EXPORTING
        iv_action     = wmegc_update
        it_wo_rsrc_ty = lt_wo_rsrc_ty.

  ENDMETHOD.


  METHOD rsrc_proc_wo_select.
********************************************************************
*& Key          : <BSUGAREV>-Oct 31, 2023
*& Request No.  :
********************************************************************
*& Description  : Select WOs for RF picking process
*&
*&
********************************************************************
    DATA: lt_whohu TYPE /scwm/tt_whohu_int.

*    IF mv_who_sel_exec = abap_false.
    RETURN.
*    ENDIF.

    CLEAR: mv_who_sel_exec.

    IF zcl_switch=>get_switch_state( iv_lgnum = mv_lgnum
                                     iv_devid = zif_switch_const=>c_zrfui_003 ) EQ abap_false.
      RETURN.
    ENDIF.

    " get an instance of picking cart and check if queue is set
    " in RF where we need to select the WHO in the current BADI, queue will always be set
    TRY.
        DATA(lo_pick_cart) = zcl_rf_pick_cart=>get_instance( iv_lgnum = mv_lgnum ).
      CATCH zcx_core_exception.
    ENDTRY.

    IF lo_pick_cart IS NOT BOUND OR lo_pick_cart->mv_queue IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_who) = NEW zcl_whse_order( iv_lgnum = mv_lgnum
                                       it_selection_parameters = VALUE #(
                                       ( field = zif_whse_order=>wo_mapping_fieldname-queue
                                         select_options =  VALUE #( ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = lo_pick_cart->mv_queue ) ) )
                                       ( field = zif_whse_order=>wo_mapping_fieldname-status
                                         select_options =  VALUE #( ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = zif_whse_order=>wo_status-open ) ) ) ) ).

    lo_who->wo_data_select( IMPORTING et_who = DATA(lt_whos) ).

    " WHOs with BUNDLE flag are not wanted
    DELETE lt_whos WHERE zz_bundled = abap_true.

    IF lines( lt_whos ) = 0.
      RETURN.
    ENDIF.

    DATA(lt_who_sel) = VALUE /scwm/tt_whoid( FOR <l> IN lt_whos ( who = <l>-who ) ).

    TRY.
        CALL FUNCTION '/SCWM/WHO_SELECT'
          EXPORTING
            iv_lgnum = mv_lgnum
            it_who   = lt_who_sel
          IMPORTING
            et_whohu = lt_whohu.
      CATCH /scwm/cx_core.
    ENDTRY.

    " remove all WHOs which already have a cart assigned to them
    DATA(lt_who_remove) = VALUE rseloption( ).
    LOOP AT lt_whohu ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( who = <ls_dummy>-who )
                     ASSIGNING FIELD-SYMBOL(<lt_whohu_gr>).

      IF abap_true = zcl_rsrc_rebundle=>pick_cart_exists(
         iv_lgnum  = mv_lgnum
         it_whohu  = VALUE #( FOR <gr> IN GROUP <lt_whohu_gr> ( <gr> ) ) ).

        lt_who_remove = VALUE #( BASE lt_who_remove
            ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <lt_whohu_gr>-who ) ).
      ENDIF.

    ENDLOOP.

    SORT lt_whohu BY who.
    IF lines( lt_who_remove ) > 0.
      DELETE lt_whohu WHERE who IN lt_who_remove.
    ENDIF.

    IF lines( lt_whohu ) > 0.
      SELECT * FROM /scwm/wo_rsrc_ty INTO TABLE rt_wo_rsrc_ty
         FOR ALL ENTRIES IN lt_whohu
        WHERE lgnum = mv_lgnum
          AND who = lt_whohu-who.
    ENDIF.

  ENDMETHOD.


  METHOD rsrc_who_assign.
********************************************************************
*& Key          : <AYORDANOV>-05.09.2023
*& Request No.  :
********************************************************************
*& Description
*& Assign WHO TO RSRC
********************************************************************

    DATA: ls_who        TYPE /scwm/s_who_int,
          ls_t346       TYPE /scwm/t346,
          ls_rsrc       TYPE /scwm/rsrc ##NEEDED,
          lt_wo_rsrc_ty TYPE /scwm/tt_wo_rsrc_ty,
          lt_ordim_o    TYPE /scwm/tt_ordim_o.

    TRY.
        CALL FUNCTION '/SCWM/WHO_SELECT'
          EXPORTING
            iv_to       = abap_true
            iv_lgnum    = mv_lgnum
            iv_lock_who = abap_true
            iv_who      = iv_who
          IMPORTING
            es_who      = ls_who
            et_ordim_o  = lt_ordim_o.

      CATCH /scwm/cx_core.
        CALL FUNCTION 'DEQUEUE_ALL'.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg_dummy.
        RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDTRY.


    SORT lt_ordim_o BY lgnum who tanum.

    LOOP AT lt_ordim_o ASSIGNING  FIELD-SYMBOL(<ls_ordim_o>).

      CHECK <ls_ordim_o>-srsrc IS NOT INITIAL AND
        ( ls_who-status = wmegc_wo_in_process AND
          is_rsrc-rsrc <> <ls_ordim_o>-srsrc ).
      CALL FUNCTION 'DEQUEUE_ALL'.

      MESSAGE e140(/scwm/rf_en) WITH <ls_ordim_o>-srsrc <ls_ordim_o>-who INTO mv_msg_dummy.
      RAISE EXCEPTION NEW zcx_core_exception( ).

    ENDLOOP.

    lt_wo_rsrc_ty = VALUE #( ( CORRESPONDING #( ls_who ) ) ).

    "  Read queue to get operating environment
    CALL FUNCTION '/SCWM/T346_READ_SINGLE'
      EXPORTING
        iv_lgnum  = mv_lgnum
        iv_queue  = ls_who-queue
      IMPORTING
        es_t346   = ls_t346
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg_dummy.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    ls_rsrc = is_rsrc.

    "   Select/assign warehouse order for a resource
    CALL FUNCTION '/SCWM/RSRC_WHO_SELECT'
      EXPORTING
        iv_rfrsrc         = ls_t346-rfrsrc
        iv_norsrc_upd     = abap_true
        iv_man_wo_sel     = abap_true
        iv_no_rec_call    = abap_true "NO REC-Control by call from NON RF-Environment
      CHANGING
        cs_rsrc           = ls_rsrc
        ct_wo_rsrc_ty     = lt_wo_rsrc_ty
      EXCEPTIONS
        no_rstyp_attached = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      CALL FUNCTION 'DEQUEUE_ALL'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg_dummy.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD rsrc_who_unassign_no_split.
********************************************************************
*& Key          : <AYORDANOV>-01.09.2023
*& Request No.  :
********************************************************************
*& Description
*& Unassign WHO from RSRC without split. This code is 99% standard
********************************************************************

    DATA: ls_who         TYPE /scwm/s_who_int,
          lt_who         TYPE /scwm/tt_who_int,
          ls_attributes  TYPE /scwm/s_who_att,
          lt_ordim_o     TYPE /scwm/tt_ordim_o,
          lt_ordim_c     TYPE /scwm/tt_ordim_c,
          lt_canc        TYPE /scwm/tt_cancl,
          lt_bapiret     TYPE bapirettab,
          ls_canc        TYPE /scwm/cancl,
          ls_ordim_o     TYPE /scwm/ordim_o,
          lv_clear_rsrc  TYPE xfeld,
          lv_clear_start TYPE xfeld,
          lv_severity    TYPE bapi_mtype.

    IF ( is_rsrc-rsrc IS INITIAL OR
         iv_who       IS INITIAL ).
      "  Missing data
      MESSAGE e042(/scwm/rsrc) INTO mv_msg_dummy.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    " Get warehouse orders of the resource
    CALL FUNCTION '/SCWM/RSRC_WHO_RESOURCE_GET'
      EXPORTING
        iv_lgnum      = mv_lgnum
        iv_rsrc       = is_rsrc-rsrc
      IMPORTING
        et_rsrc_who   = lt_who
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg_dummy.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    DELETE lt_who WHERE who <> iv_who.

    IF lt_who IS INITIAL AND
       it_who IS NOT INITIAL.
      lt_who = it_who.
    ENDIF.

    IF lt_who IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_who ASSIGNING FIELD-SYMBOL(<ls_who>).

      CLEAR ls_attributes.
      MOVE-CORRESPONDING <ls_who> TO ls_attributes.
      CLEAR: ls_attributes-started_at,
             ls_attributes-start_bin,
             ls_attributes-processor.

      "     Update table /scwm/who
      CALL FUNCTION '/SCWM/WHO_UPDATE'
        EXPORTING
          iv_lgnum      = mv_lgnum
          iv_db_update  = abap_true
          iv_who        = <ls_who>-who
          is_attributes = ls_attributes
          iv_synchron   = abap_true
        EXCEPTIONS
          read_error    = 1
          attributes    = 2
          OTHERS        = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg_dummy.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.

    ENDLOOP.

    IF lt_who IS INITIAL.
      RETURN.
    ENDIF.

    "Continue with warehouse orders assigned (not manually)
    " to the resource, that can be unassigned automatically (all open TOs)
    " Clear records' resource and status
    LOOP AT lt_who ASSIGNING <ls_who>.
      TRY.
          CALL FUNCTION '/SCWM/WHO_SELECT'
            EXPORTING
              iv_to      = abap_true
              iv_lgnum   = mv_lgnum
              iv_who     = <ls_who>-who
            IMPORTING
              es_who     = ls_who
              et_ordim_o = lt_ordim_o
              et_ordim_c = lt_ordim_c.
        CATCH /scwm/cx_core ##NO_HANDLER..
      ENDTRY.

      CLEAR ls_attributes.
      MOVE-CORRESPONDING ls_who TO ls_attributes.

      lv_clear_rsrc  = abap_true.
      lv_clear_start = abap_true.
      "   If we have open WT from a resource we can't delete the rsrc
      "  If we have no confirmed WT we can clear the started_at and status
      IF ls_who-rsrc IS NOT INITIAL AND is_rsrc-rsrc IS NOT INITIAL.
        READ TABLE lt_ordim_o INTO ls_ordim_o
          WITH KEY srsrc = ls_who-rsrc.
        IF sy-subrc = 0. "We found open WT from the resource
          IF ls_who-type <> wmegc_wcr_ppp_sd AND
             ls_who-type <> wmegc_wcr_ppp_ud.
            CLEAR lv_clear_rsrc.
          ELSE.
            IF lines( lt_ordim_o ) > 1.
              "  cancel open HU WT from resource to destination
              "  if there is at least one other open pickWT
              "  with this, the WHO can be picked up by a different resource
              ls_canc-tanum = ls_ordim_o-tanum.
              APPEND ls_canc TO lt_canc.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF lt_ordim_c IS NOT INITIAL. "We found confirmed WT
        CLEAR lv_clear_start.
      ENDIF.

      IF lv_clear_start = abap_true.
        CLEAR ls_attributes-started_at.
      ENDIF.
      IF lv_clear_rsrc = abap_true.
        CLEAR ls_attributes-rsrc.
      ENDIF.
      CLEAR ls_attributes-processor.
      CLEAR ls_attributes-man_assign.
      CLEAR ls_attributes-start_bin.

      CALL FUNCTION '/SCWM/WHO_UPDATE'
        EXPORTING
          iv_lgnum      = mv_lgnum
          iv_db_update  = abap_true
          iv_who        = <ls_who>-who
          is_attributes = ls_attributes
          iv_synchron   = abap_true
        EXCEPTIONS
          read_error    = 1
          attributes    = 2
          OTHERS        = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg_dummy.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.

      CHECK lt_canc IS NOT INITIAL.

      CALL FUNCTION '/SCWM/TO_CANCEL'
        EXPORTING
          iv_lgnum       = mv_lgnum
          it_cancl       = lt_canc
          iv_commit_work = abap_true
          iv_update_task = abap_true
        IMPORTING
          et_bapiret     = lt_bapiret
          ev_severity    = lv_severity.

      IF lv_severity CA wmegc_severity_eax.
        LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapiret>) WHERE type = lv_severity.
          MESSAGE ID <ls_bapiret>-id TYPE <ls_bapiret>-type NUMBER <ls_bapiret>-number
                WITH <ls_bapiret>-message_v1 <ls_bapiret>-message_v2 <ls_bapiret>-message_v3 <ls_bapiret>-message_v4 INTO mv_msg_dummy.
          RAISE EXCEPTION NEW zcx_core_exception( ).
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD select_first_who_for_cart.
********************************************************************
*& Key          : BSUGAREV-Dec 7, 2023
*& Request No.  : Used in several gaps in RF picking
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lt_whohu TYPE /scwm/tt_whohu_int.

    DATA(lo_who) = NEW zcl_whse_order( iv_lgnum = mv_lgnum
                                       it_selection_parameters = VALUE #(
                                       ( field = zif_whse_order=>wo_mapping_fieldname-queue
                                         select_options =  VALUE #( ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = iv_queue ) ) )
                                       ( field = zif_whse_order=>wo_mapping_fieldname-status
                                         select_options =  VALUE #( ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = wmegc_wo_open )
                                                                    ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = wmegc_wo_in_process ) ) ) ) ).

    lo_who->wo_data_select( IMPORTING et_who = DATA(lt_whos) ).

    " WHOs with BUNDLE flag are not wanted
*    DELETE lt_whos WHERE zz_bundled = abap_true AND
*                         status     = wmegc_wo_open.
*    " WOs with bundled flag and not assigned to resource have already been process
*    DELETE lt_whos WHERE rsrc IS INITIAL AND
*                         zz_bundled = abap_true.
*
*    IF lines( lt_whos ) = 0.
*      RETURN.
*    ENDIF.

*    DELETE lt_whos WHERE status = wmegc_wo_in_process AND
*                         rsrc   <> iv_rsrc.
    " remove all WO which are assigned to different resource
    DELETE lt_whos WHERE rsrc IS NOT INITIAL AND
                         rsrc <> iv_rsrc.

    IF lines( lt_whos ) = 0.
      RETURN.
    ENDIF.

    DATA(lt_who_sel) = VALUE /scwm/tt_whoid( FOR <l> IN lt_whos ( who = <l>-who ) ).

    TRY.
        CALL FUNCTION '/SCWM/WHO_SELECT'
          EXPORTING
            iv_lgnum = mv_lgnum
            it_who   = lt_who_sel
          IMPORTING
            et_whohu = lt_whohu.
      CATCH /scwm/cx_core.
    ENDTRY.

    " remove all WHOs which have a pick HU already created
    " we need only a brand new untouched orders
    DATA(lt_who_remove) = VALUE rseloption( ).
    LOOP AT lt_whohu ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( who = <ls_dummy>-who )
                     ASSIGNING FIELD-SYMBOL(<lt_whohu_gr>).

      CHECK line_exists( lt_whos[ status = wmegc_wo_open
                                  who   = <lt_whohu_gr>-who ] ).

      LOOP AT GROUP <lt_whohu_gr> ASSIGNING FIELD-SYMBOL(<ls_whohu>) WHERE huident IS NOT INITIAL.
        lt_who_remove = VALUE #( BASE lt_who_remove
            ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <lt_whohu_gr>-who ) ).
      ENDLOOP.

      IF abap_true = zcl_rsrc_rebundle=>pick_cart_exists(
         iv_lgnum  = mv_lgnum
         it_whohu  = VALUE #( FOR <gr> IN GROUP <lt_whohu_gr> ( <gr> ) ) ).

        lt_who_remove = VALUE #( BASE lt_who_remove
            ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <lt_whohu_gr>-who ) ).
      ENDIF.

    ENDLOOP.

    SORT lt_whohu BY who.
    IF lines( lt_who_remove ) > 0.
      DELETE lt_whohu WHERE who IN lt_who_remove.
    ENDIF.

    IF lines( lt_whohu ) > 0.
      SELECT * FROM /scwm/wo_rsrc_ty INTO TABLE rt_wo_rsrc_ty
         FOR ALL ENTRIES IN lt_whohu
        WHERE lgnum = mv_lgnum
          AND who = lt_whohu-who.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
