CLASS zcl_rsrc_rebundle DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES /scwm/if_ex_rsrc_rebundle .

    CLASS-METHODS pick_cart_exists
      IMPORTING
        !iv_lgnum        TYPE /scwm/lgnum
        !it_whohu        TYPE /scwm/tt_whohu_int
      RETURNING
        VALUE(rv_exists) TYPE xfeld .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS mc_pick
      IMPORTING
        !it_ordim_o_all_fields TYPE /scwm/tt_ordim_o
        !it_who_params         TYPE zif_whse_order=>tty_to
        !is_mc_pick            TYPE zcl_crud_ztout_mc_pick=>ts_mc_pick
        !it_sel_ordim_o        TYPE /scwm/tt_ordim_o
        !it_who_sorted         TYPE /scwm/tt_wo_det_mon
      CHANGING
        !ct_tanum              TYPE /scwm/tt_rf_tanum .
    METHODS get_wo_sorted
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        !iv_queue       TYPE /scwm/de_queue
        !iv_current_who TYPE /scwm/s_who_int-who
      EXPORTING
        !et_who_sorted  TYPE /scwm/tt_wo_det_mon
        !et_who_params  TYPE zif_whse_order=>tty_to
        !et_wt          TYPE /scwm/tt_ordim_o .
    METHODS bundle_wo_by_wo
      IMPORTING
        !is_who_main       TYPE /scwm/s_who_int
        !is_rsrc           TYPE /scwm/rsrc
      CHANGING
        !ct_wt_bundle_list TYPE /scwm/tt_rf_tanum .
ENDCLASS.



CLASS ZCL_RSRC_REBUNDLE IMPLEMENTATION.


  METHOD /scwm/if_ex_rsrc_rebundle~post_rebundle_man.
********************************************************************
*& Key          : BSUGAREV-Dec 8, 2023
*& Request No.  : GAP-017 FS Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_rsrc_rebundle.

    cv_commit_work = abap_true.

  ENDMETHOD.


  METHOD /scwm/if_ex_rsrc_rebundle~resource_capacity.
********************************************************************
*& Key          : BSUGAREV-Dec 8, 2023
*& Request No.  : GAP-017 FS Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_rsrc_rebundle.

    CLEAR cv_num_wt.

    IF abap_true = zcl_switch=>get_switch_state(
        iv_lgnum  = iv_lgnum
        iv_devid  = zif_switch_const=>c_zrfui_004
        it_fields = VALUE #( ( field = zif_switch_const=>c_ltrans
                               field_value = /scwm/cl_rf_bll_srvc=>get_ltrans( ) ) ) ).
      cv_num_wt = 1.
    ENDIF.

  ENDMETHOD.


  METHOD /scwm/if_ex_rsrc_rebundle~wt_select.
********************************************************************
*& Key           : <AAHMEDOV>-23.06.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_rsrc_rebundle.

    IF iv_num_wt IS INITIAL.
      " No rebundling necessary
      CLEAR: ct_tanum, cv_keep_pickhu.
      RETURN.
    ENDIF.

    " append tasks of selected WHO to rebundle
    ct_tanum = VALUE #( FOR <tanum> IN it_ordim_o ( <tanum>-tanum ) ).

    DATA(lv_ltrans) = /scwm/cl_rf_bll_srvc=>get_ltrans( ).

    IF lv_ltrans = zif_rfui_c=>gs_ltrans-zpicar OR
       lv_ltrans = zif_rfui_c=>gs_ltrans-zpicrb .

      bundle_wo_by_wo(
        EXPORTING
          is_who_main         = is_who
          is_rsrc             = is_rsrc
        CHANGING
          ct_wt_bundle_list   = ct_tanum ).

      IF lines( ct_tanum ) > 0.
        cv_keep_pickhu = abap_true.
        zcl_who_eew_change=>set_bundle_flag( abap_true ).
      ENDIF.

      RETURN.
    ENDIF.

    get_wo_sorted(
      EXPORTING
        iv_lgnum       = iv_lgnum
        iv_queue       = iv_queue
        iv_current_who = is_who-who
      IMPORTING
        et_who_sorted  = DATA(lt_who_sorted)
        et_who_params  = DATA(lt_who_params)
        et_wt          = DATA(lt_ordim_o_all_fields) ).

    zcl_crud_ztout_mc_pick=>select_single_by_queue(
      EXPORTING
        iv_lgnum   = iv_lgnum                     " Warehouse Number/Warehouse Complex
        iv_queue   = is_who-queue                 " Range Structure for Queue
      IMPORTING
        es_mc_pick = DATA(ls_mc_pick)             " Master Carton Picking Flow
    ).

    IF ls_mc_pick IS NOT INITIAL.

      TRY.
          DATA(lv_pick_cart_exists) = pick_cart_exists(
            iv_lgnum = iv_lgnum
            it_whohu = it_whohu ).
          ##NO_HANDLER
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      IF is_who-zz_bundled EQ abap_true
        AND lv_pick_cart_exists EQ abap_true.

        "reject the selected WO and all other WOs
        CLEAR: ct_tanum,
               cv_keep_pickhu.

      ELSEIF is_who-zz_bundled EQ abap_true.
        "No rebundling necessary
        CLEAR: ct_tanum,
               cv_keep_pickhu.
      ELSE.

        " in this case we should not re-bundule WHOs
        DELETE lt_who_params WHERE zz_bundled = abap_true.

        mc_pick(
          EXPORTING
            it_ordim_o_all_fields = lt_ordim_o_all_fields      " Table: Warehouse Tasks Open
            it_who_params         = lt_who_params
            is_mc_pick            = ls_mc_pick                 " Master Carton Picking Flow
            it_sel_ordim_o        = it_ordim_o                 " Table: Warehouse Tasks Open
            it_who_sorted         = lt_who_sorted              " Warehouse Order Details for WM Monitor
          CHANGING
            ct_tanum              = ct_tanum                   " Table of warehouse task numbers
        ).
      ENDIF.
    ELSE.
      CLEAR ct_tanum.
    ENDIF.

    IF ct_tanum IS NOT INITIAL.
      cv_keep_pickhu = abap_true.
      zcl_who_eew_change=>set_bundle_flag( abap_true ).
    ENDIF.

  ENDMETHOD.


  METHOD bundle_wo_by_wo.
********************************************************************
*& Key          : BSUGAREV-Dec 8, 2023
*& Request No.  : GAP-017 FS Picking WO Bundling
*&                GAP-050 RF Picking for Shipping Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  : Bundle current WO to WO already locked by user
*&
*&
********************************************************************
    DATA(lt_wt_bundle_temp) = ct_wt_bundle_list.
    CLEAR: ct_wt_bundle_list.

    DATA(lo_who) = NEW zcl_whse_order( iv_lgnum = is_who_main-lgnum
                                       it_selection_parameters = VALUE #(
                                       ( field = zif_whse_order=>wo_mapping_fieldname-queue
                                         select_options =  VALUE #( ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = is_who_main-queue ) ) )
                                       ( field = zif_whse_order=>wo_mapping_fieldname-status
                                         select_options =  VALUE #( ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = zif_whse_order=>wo_status-in_process ) ) ) ) ).

    lo_who->wo_data_select( IMPORTING et_who                = DATA(lt_whos)
                                      et_ordim_o_all_fields = DATA(lt_wt) ).

    DELETE lt_whos WHERE rsrc <> is_rsrc-rsrc.

    IF lines( lt_whos ) = 0.
      " we need to call this method on order to find all open WHO and
      "  sort them by SAP algorithm. This will set the update of custom fields
      "  in the main WHO
      get_wo_sorted(
        EXPORTING
          iv_lgnum       = is_who_main-lgnum
          iv_queue       = is_who_main-queue
          iv_current_who = is_who_main-who ).

      RETURN.
    ENDIF.

    " there is already a locked WHO and pathseq is updated for it.
    " Here the update of pathseq is reset but bellow if bundling is successful is updated again
    zcl_who_eew_change=>set_pathseq_category( is_who_eew = VALUE #( ) ).

    IF lines( lt_whos ) <> 1.
      " RSRC is assigned to more than one WHO or no WO is found
      RETURN.
    ENDIF.

    DATA(ls_who_assigned_to_rsrc) = VALUE #( lt_whos[ 1 ] OPTIONAL ).

    " when the process is start for the first time WHO zz_bundled will be initial.
    " Leave the method without "ct_wt_bundle_list" and set zz_bundled flag
    IF ls_who_assigned_to_rsrc-zz_bundled = abap_false.
      RETURN.
    ENDIF.

    " at this point "zz_bundled = X", most probably the cart has been left and later scanned again
    "   can happen that we reach the BADI the the bundled WHO, in this case we don't want to do anything
    "   we exit the BADI without changing any data
    IF ls_who_assigned_to_rsrc-who = is_who_main-who.
      RETURN.
    ENDIF.

    " pathseq and category should be updated with the values of the very first WHO
    zcl_who_eew_change=>set_pathseq_category(
        is_who_eew = VALUE #( lgnum = ls_who_assigned_to_rsrc-lgnum
                              who   = ls_who_assigned_to_rsrc-who
                              zz_category   = ls_who_assigned_to_rsrc-zz_category
                              zz_pathseq_fr = ls_who_assigned_to_rsrc-zz_pathseq_fr ) ).

    ct_wt_bundle_list = lt_wt_bundle_temp.

    " most common case, BADI is started with a a new WHO from the queue and we have to bundle it with the
    "   WHO assigned to the RSRC
    ct_wt_bundle_list = VALUE #( BASE ct_wt_bundle_list
                                 FOR <l> IN lt_wt WHERE ( who = ls_who_assigned_to_rsrc-who )
                                 ( <l>-tanum ) ).
  ENDMETHOD.


  METHOD get_wo_sorted.
********************************************************************
*& Key           : <AAHMEDOV>-25.01.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************

    DATA(lo_who) = NEW zcl_whse_order( iv_lgnum = iv_lgnum
                                       it_selection_parameters = VALUE #(
                                       ( field = zif_whse_order=>wo_mapping_fieldname-queue
                                         select_options =  VALUE #( ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = iv_queue ) ) )
                                       ( field = zif_whse_order=>wo_mapping_fieldname-status
                                         select_options =  VALUE #( ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = zif_whse_order=>wo_status-open ) ) ) ) ).

    lo_who->wo_data_select( IMPORTING et_who                = DATA(lt_whos)
                                      et_ordim_o            = et_who_params
                                      et_ordim_o_all_fields = et_wt ).

    DELETE lt_whos WHERE zz_bundled EQ abap_true
                     AND who <> iv_current_who.

    DATA(lo_who_prio) = NEW zcl_out_who_priority( ).

    et_who_sorted = lo_who_prio->sort_who_by_heatmap(
      iv_lgnum    = iv_lgnum
      iv_who_base = iv_current_who
      it_who_list = lt_whos ).

    " Andriyan Yordanov --- not sure but just back up vertion solution bla bla
    " maybe this should be deleted
    IF et_who_sorted IS INITIAL.
      LOOP AT et_who_params  ASSIGNING FIELD-SYMBOL(<ls_who_sorted>).
        CHECK line_exists( lt_whos[ who = <ls_who_sorted>-who ] ) AND
          <ls_who_sorted>-who <> iv_current_who. "this line was added by aahmedov
        et_who_sorted = VALUE #( BASE et_who_sorted ( CORRESPONDING #( <ls_who_sorted> ) ) ).
      ENDLOOP.
    ENDIF.
    " end Andriyan Yordanov

  ENDMETHOD.


  METHOD mc_pick.
********************************************************************
*& Key           : <AAHMEDOV>-25.01.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************

    DATA: ls_who_cmp_w TYPE zif_whse_order=>ty_to_cmp,
          ls_who_cmp_v TYPE zif_whse_order=>ty_to_cmp.

    DATA: lo_delivery   TYPE REF TO /scwm/cl_dlv_management_prd,
          lt_selections TYPE TABLE OF /scwm/dlv_selection_str.

    DATA: lv_conv_volume     TYPE /scdl/dl_qty,
          lv_conv_weight     TYPE /scdl/dl_qty,
          lv_curr_who_vol    TYPE /scdl/dl_qty,
          lv_curr_who_weight TYPE /scdl/dl_qty.

    FIELD-SYMBOLS: <ls_who_cmp_w> TYPE zif_whse_order=>ty_to_cmp,
                   <ls_who_cmp_v> TYPE zif_whse_order=>ty_to_cmp.

    "this code is part of logic MC_PICK table

    DATA(lt_who_sorted) = it_who_sorted.
    DATA(lt_ordim_o_all_fields) = it_ordim_o_all_fields.
    DATA(lt_who_params) = it_who_params.
    DATA(ls_mc_pick) = is_mc_pick.

    ASSIGN ls_mc_pick TO FIELD-SYMBOL(<ls_mc_pick>).

    IF ls_mc_pick-carrier EQ abap_true.

      lo_delivery = /scwm/cl_dlv_management_prd=>get_instance( ).

      lt_selections = VALUE #( BASE lt_selections FOR GROUPS OF <ordim_rdocid> IN lt_ordim_o_all_fields
                                                GROUP BY <ordim_rdocid>-rdocid
                                                  ( fieldname = /scdl/if_dl_logfname_c=>sc_docid_h
                                                    sign = wmegc_sign_inclusive
                                                    option = wmegc_option_eq
                                                    low = <ordim_rdocid>-rdocid ) ).

      TRY.
          CALL METHOD lo_delivery->query
            EXPORTING
              iv_doccat       = /scdl/if_dl_c=>sc_doccat_out_prd
              it_selection    = lt_selections
              is_read_options = VALUE #( data_retrival_only = abap_true )
              is_include_data = VALUE #( head_partyloc = abap_true )
            IMPORTING
              et_headers      = DATA(lt_headers).
        CATCH /scdl/cx_delivery.
          RETURN.
      ENDTRY.

      DATA(lv_curr_carr) = VALUE #( lt_headers[ docid = it_sel_ordim_o[ 1 ]-rdocid ]-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr ]-partyno OPTIONAL ).

      IF lv_curr_carr IS NOT INITIAL.

        LOOP AT lt_ordim_o_all_fields ASSIGNING FIELD-SYMBOL(<ls_ordim_all_fields>).

          TRY.
              DATA(lv_carr_cmp) = VALUE #( lt_headers[ docid = <ls_ordim_all_fields>-rdocid ]-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr ]-partyno OPTIONAL ).
            CATCH cx_sy_itab_line_not_found.
              CONTINUE.
          ENDTRY.

          CHECK lv_curr_carr IS NOT INITIAL
          AND lv_curr_carr <> lv_carr_cmp.

          DELETE lt_who_sorted WHERE who EQ <ls_ordim_all_fields>-who.
          DELETE lt_who_params WHERE who EQ <ls_ordim_all_fields>-who.
          DELETE lt_ordim_o_all_fields.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF lt_who_sorted IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_conv) = /scmb/cl_md_access_mdl=>get_md_access( ).

    "update volume and weight from open WTs
    LOOP AT lt_who_sorted ASSIGNING FIELD-SYMBOL(<ls_who_sorted>).

      DATA(ls_who_p) = REF #( lt_who_params[ who = <ls_who_sorted>-who ] OPTIONAL ).

      CHECK ls_who_p IS NOT INITIAL.

      IF ls_who_p->unit_v <> <ls_mc_pick>-vol_uom OR
        ls_who_p->unit_w <> <ls_mc_pick>-weight_uom.

        DATA(lt_ordim_o_for_who) = VALUE /scwm/tt_ordim_o( FOR <ordim_o> IN lt_ordim_o_all_fields
                                                               WHERE ( who = <ls_who_sorted>-who )
                                                            ( CORRESPONDING #( <ordim_o> ) ) ).

        IF ls_who_p->unit_v <> <ls_mc_pick>-vol_uom.

          ASSIGN ls_who_cmp_v TO <ls_who_cmp_v>.

          LOOP AT lt_ordim_o_for_who ASSIGNING FIELD-SYMBOL(<ls_ordim_o_for_who>).

            TRY.
                lv_conv_volume = lo_conv->prod_quan_conversion(
                  iv_prodid   = <ls_ordim_o_for_who>-matid
                  iv_uom_from = <ls_ordim_o_for_who>-unit_v
                  iv_uom_to   = <ls_mc_pick>-vol_uom
                  iv_quan     = CONV #( <ls_ordim_o_for_who>-volum ) ).
              CATCH /scmb/cx_md_access  ##NO_HANDLER.
            ENDTRY.

            <ls_who_cmp_v>-sum_volum += lv_conv_volume.

          ENDLOOP.

          <ls_who_cmp_v>-unit_v = <ls_mc_pick>-vol_uom.

        ENDIF.

        IF ls_who_p->unit_w <> <ls_mc_pick>-weight_uom.

          ASSIGN ls_who_cmp_w TO <ls_who_cmp_w>.

          LOOP AT lt_ordim_o_for_who ASSIGNING <ls_ordim_o_for_who>.

            TRY.
                lv_conv_weight = lo_conv->prod_quan_conversion(
                  iv_prodid   = <ls_ordim_o_for_who>-matid
                  iv_uom_from = <ls_ordim_o_for_who>-unit_w
                  iv_uom_to   = <ls_mc_pick>-weight_uom
                  iv_quan     = CONV #( <ls_ordim_o_for_who>-weight ) ).
              CATCH /scmb/cx_md_access ##NO_HANDLER.
            ENDTRY.

            <ls_who_cmp_w>-sum_weight += lv_conv_weight.

          ENDLOOP.

          <ls_who_cmp_w>-unit_w = <ls_mc_pick>-weight_uom.

        ENDIF.

      ENDIF.

      IF <ls_who_cmp_v>-sum_volum IS ASSIGNED.
        <ls_who_sorted>-sum_volum = <ls_who_cmp_v>-sum_volum.
        <ls_who_sorted>-unit_v = <ls_who_cmp_v>-unit_v.
        CLEAR <ls_who_cmp_v>.
      ELSE.
        <ls_who_sorted>-sum_volum = ls_who_p->sum_volum.
        <ls_who_sorted>-unit_v = ls_who_p->unit_v.
      ENDIF.

      IF <ls_who_cmp_w>-sum_weight IS ASSIGNED.
        <ls_who_sorted>-sum_weight = <ls_who_cmp_w>-sum_weight.
        <ls_who_sorted>-unit_w = <ls_who_cmp_w>-unit_w.
        CLEAR <ls_who_cmp_w>.
      ELSE.
        <ls_who_sorted>-sum_weight = ls_who_p->sum_weight.
        <ls_who_sorted>-unit_w = ls_who_p->unit_w.
      ENDIF.

    ENDLOOP.

    "loop tasks of already selected WHO
    "convert already selected WHO volume & weight
    LOOP AT it_sel_ordim_o ASSIGNING FIELD-SYMBOL(<ls_ordim_sel_o>).

      IF <ls_ordim_sel_o>-unit_v <> ls_mc_pick-vol_uom.
        TRY.
            lv_curr_who_vol += lo_conv->prod_quan_conversion(
              iv_prodid   = <ls_ordim_sel_o>-matid
              iv_uom_from = <ls_ordim_sel_o>-unit_v
              iv_uom_to   = ls_mc_pick-vol_uom
              iv_quan     = CONV #( <ls_ordim_sel_o>-volum ) ).
          CATCH /scmb/cx_md_access ##NO_HANDLER.
        ENDTRY.
      ELSE.
        lv_curr_who_vol += <ls_ordim_sel_o>-volum.
      ENDIF.

      "convert already selected WHO weight
      IF <ls_ordim_sel_o>-unit_w <> ls_mc_pick-weight_uom.
        TRY.
            lv_curr_who_weight += lo_conv->prod_quan_conversion(
              iv_prodid   = <ls_ordim_sel_o>-matid
              iv_uom_from = <ls_ordim_sel_o>-unit_w
              iv_uom_to   = ls_mc_pick-weight_uom
              iv_quan     = CONV #( <ls_ordim_sel_o>-weight ) ).
          CATCH /scmb/cx_md_access ##NO_HANDLER.
        ENDTRY.
      ELSE.
        lv_curr_who_weight  += <ls_ordim_sel_o>-weight.
      ENDIF.
    ENDLOOP.

    "subtract the weight & volume from max weight & volume
    "for the already selected WHO
    <ls_mc_pick>-max_vol -= lv_curr_who_vol.
    <ls_mc_pick>-max_weight -= lv_curr_who_weight.

    LOOP AT lt_who_sorted ASSIGNING <ls_who_sorted>.

      DATA(lv_is_volume_ok) = COND xfeld( WHEN <ls_mc_pick>-max_vol - <ls_who_sorted>-sum_volum  GE 0
                                          THEN abap_true
                                          ELSE abap_false ).

      DATA(ls_is_weight_ok) = COND xfeld( WHEN <ls_mc_pick>-max_weight - <ls_who_sorted>-sum_weight GE 0
                                          THEN abap_true
                                          ELSE abap_false ).
*
      "both weight & volume has to be alright,
      "in order for the WHO to be rebundled
      CHECK lv_is_volume_ok EQ abap_true AND
        ls_is_weight_ok EQ abap_true.

      "Check if WO is not locked by another resource/user
      "Keep the lock to avoid that a parallel user gets the same WO
      CALL FUNCTION 'ENQUEUE_/SCWM/EWHO'
        EXPORTING
          lgnum        = <ls_who_sorted>-lgnum
          who          = <ls_who_sorted>-who
        EXCEPTIONS
          foreign_lock = 1
          OTHERS       = 99.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ct_tanum = VALUE /scwm/tt_rf_tanum( BASE ct_tanum
                            FOR <tanum> IN lt_ordim_o_all_fields
                             WHERE ( who = <ls_who_sorted>-who )
                              ( <tanum>-tanum ) ).

      "subtract the weight & volume for each WHO which is to be
      "rebundled into the already selected WHO
      <ls_mc_pick>-max_vol -= <ls_who_sorted>-sum_volum.
      <ls_mc_pick>-max_weight -= <ls_who_sorted>-sum_weight.

    ENDLOOP.

  ENDMETHOD.


  METHOD pick_cart_exists.
********************************************************************
*& Key          : <AYORDANOV>-21.09.2023
*& Request No.  : GAP-017 FS Picking WO Bundling
********************************************************************
*& Description
*& Check if pick cart exist
********************************************************************

    DATA: lt_huhdr TYPE /scwm/tt_huhdr_int,
          lt_t307  TYPE /scwm/tt_t307.

    DATA(lt_matid) = VALUE /scmb/mdl_matid_tab( FOR <ls_pmat_id> IN it_whohu
                                WHERE ( huident IS NOT INITIAL )
                             ( matid = <ls_pmat_id>-pmat_guid ) ).

    NEW /scwm/cl_ui_stock_fields( )->prefetch_matkey_by_id(
      EXPORTING
        it_matid            = lt_matid                 " Internal Key for Product
      IMPORTING
        et_matid_extkey     = DATA(lt_matid_extkey) ).  " Product ID and External Key (Number/Short Text)

    " load pack. mat from parameter which are relevant for cart
    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = iv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zgen_0006                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_pmat_cart_id                 " Parameter ID for process
      IMPORTING
        et_list      = DATA(lt_cart_pmat) ).               " SELECT-OPTIONS Table

    LOOP AT lt_matid_extkey ASSIGNING FIELD-SYMBOL(<ls_all_pickhu_pmats>).
      CHECK line_exists( lt_cart_pmat[ table_line = |{ <ls_all_pickhu_pmats>-matnr ALPHA = OUT }| ] ).
      rv_exists = abap_true.
      RETURN.
    ENDLOOP.

    " SPED logic - even that we don't have rebundle logic we use this method
    DATA(lt_pall_hutypegrp) = zcl_crud_ztcross_cart_type=>select_by_carton_type(
      EXPORTING
        iv_lgnum             = iv_lgnum
        iv_pack_cartons_type = zcl_crud_ztcross_cart_type=>c_pall_type ).                 " Packing HU/Cartons Type

    IF lt_pall_hutypegrp IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_hupall_sped) = VALUE /scwm/tt_huident( FOR <ls_hu_str> IN it_whohu
                                            WHERE ( huident IS NOT INITIAL )
                                            ( huident = <ls_hu_str>-huident
                                              lgnum   = iv_lgnum ) ).

    IF lt_hupall_sped IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/HU_SELECT'
      EXPORTING
        it_huident = lt_hupall_sped
      IMPORTING
        et_huhdr   = lt_huhdr
      EXCEPTIONS
        not_found  = 1
        error      = 2
        OTHERS     = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/T307_READ'
      EXPORTING
        iv_lgnum  = iv_lgnum
      IMPORTING
        et_t307   = lt_t307
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_huhdr ASSIGNING FIELD-SYMBOL(<ls_check_pick_cart>).
      ASSIGN lt_t307[ letyp = <ls_check_pick_cart>-letyp ] TO FIELD-SYMBOL(<ls_hutypegrp>).
      CHECK sy-subrc = 0.

      CHECK line_exists( lt_pall_hutypegrp[ hutypgrp = <ls_hutypegrp>-hutypgrp ] ).

      ASSIGN it_whohu[ huident = <ls_check_pick_cart>-huident ] TO FIELD-SYMBOL(<ls_who_with_pall>).
      CHECK sy-subrc = 0.

      rv_exists = abap_true.
      RETURN.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
