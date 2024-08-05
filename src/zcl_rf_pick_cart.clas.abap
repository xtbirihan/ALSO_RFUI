CLASS zcl_rf_pick_cart DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_to_capa,
        who           TYPE /scwm/who-who,
        count_to      TYPE /scwm/s_wo_det_mon_out-count_to,
        sum_weight    TYPE /scwm/s_wo_det_mon_out-sum_weight,
        unit_w        TYPE /scwm/s_wo_det_mon_out-unit_w,
        sum_volum     TYPE /scwm/s_wo_det_mon_out-sum_volum,
        unit_v        TYPE /scwm/s_wo_det_mon_out-unit_v,
        sum_reachtime TYPE /scwm/s_wo_det_mon_out-sum_reachtime,
        unit_rt       TYPE /scwm/s_wo_det_mon_out-unit_rt,
        pick_comp_dt  TYPE /scwm/s_wo_det_mon-pick_comp_dt,
      END OF ty_to_capa .
    TYPES:
      tty_to_capa TYPE STANDARD TABLE OF ty_to_capa .
    TYPES:
      BEGIN OF ty_cart_pos_status,
        total    TYPE i,
        occupied TYPE i,
      END OF ty_cart_pos_status .
    TYPES:
      BEGIN OF ty_avail_tote,
        guid_tote    TYPE /scwm/guid_hu,
        tote         TYPE /scwm/de_huident,
        pmatid       TYPE /scwm/s_huhdr_int-pmat_guid,
        position     TYPE /scwm/s_huhdr_int-logpos,
        occupied_pos TYPE ztt_pick_cart_tote_content,
      END OF ty_avail_tote .
    TYPES:
      BEGIN OF ty_hu_packtyp,
        huid    TYPE /scwm/de_whohuid,
        packtyp TYPE /scwm/s_huhdr_int-zz_packtyp,
      END OF ty_hu_packtyp .
    TYPES:
      tt_hu_packtyp TYPE STANDARD TABLE OF ty_hu_packtyp WITH EMPTY KEY .

    DATA ms_pick_cart TYPE /scwm/s_huhdr_int READ-ONLY .
    DATA mv_queue TYPE /scwm/de_queue READ-ONLY .

    CLASS-METHODS do_print_rf
      IMPORTING
        !iv_ldest           TYPE /scwm/de_rf_ldest
        !is_huhdr_mast_cart TYPE /scwm/s_huhdr_int
      RAISING
        zcx_core_exception .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_lgnum          TYPE /scwm/lgnum OPTIONAL
        !iv_new_inst       TYPE abap_bool DEFAULT abap_false
        !iv_queue          TYPE /scwm/de_queue OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_rf_pick_cart
      RAISING
        zcx_core_exception .
    CLASS-METHODS init .
    METHODS assign_hu_to_who
      IMPORTING
        !iv_who           TYPE /scwm/de_who
        !iv_updtyp        TYPE /scwm/s_whohu_maint-updkz DEFAULT /scwm/cl_ui_to_conf=>sc_insert
        !iv_commit        TYPE boole_d DEFAULT abap_true
        !iv_upd_huid      TYPE boole_d DEFAULT abap_false
        !it_huhdr         TYPE /scwm/tt_huhdr_int
        !it_ordim_confirm TYPE /scwm/tt_rf_ordim_confirm OPTIONAL
      RAISING
        zcx_core_exception .
    METHODS calc_capa_who
      IMPORTING
        !it_who     TYPE /scwm/tt_wo_det_mon
      EXPORTING
        !et_ordim_o TYPE tty_to_capa
        !et_ordim_c TYPE tty_to_capa .
    METHODS constructor
      IMPORTING
        !iv_lgnum TYPE /scwm/lgnum
        !iv_queue TYPE /scwm/de_queue OPTIONAL
      RAISING
        zcx_core_exception .
    METHODS create_cart_on_rsrc
      IMPORTING
        !iv_pick_cart   TYPE /scwm/de_huident
        !iv_rsrc        TYPE /scwm/de_rsrc OPTIONAL
        !iv_pack_matnr  TYPE /scwm/de_matnr OPTIONAL
        !iv_commit_work TYPE boole_d DEFAULT abap_true
      RAISING
        zcx_core_exception .
    METHODS create_cart_on_rsrc_set_who
      IMPORTING
        !iv_lgnum     TYPE /scwm/lgnum
        !iv_pick_cart TYPE /scwm/de_huident
        !iv_who       TYPE /scwm/de_who
      RAISING
        zcx_core_exception .
    METHODS create_pick_hu
      IMPORTING
        !iv_who                 TYPE /scwm/de_who
        !iv_pmatid              TYPE /scwm/de_pmatid OPTIONAL
        !iv_huident             TYPE /scwm/huident OPTIONAL
        !iv_avoid_assign_to_who TYPE abap_bool OPTIONAL
        !is_hu_create           TYPE /scwm/s_huhdr_create_ext OPTIONAL
        !iv_location            TYPE any OPTIONAL
        !is_ident               TYPE /scwm/s_ident OPTIONAL
      RETURNING
        VALUE(rs_huhdr_new)     TYPE /scwm/s_huhdr_int
      RAISING
        zcx_core_exception .
    METHODS create_tote
      IMPORTING
        !iv_pmat_guid  TYPE /scwm/s_huhdr_int-pmat_guid
        !iv_huident    TYPE /scwm/huident OPTIONAL
        !is_hu_create  TYPE /scwm/s_huhdr_create_ext OPTIONAL
        !iv_location   TYPE any OPTIONAL
        !is_ident      TYPE /scwm/s_ident OPTIONAL
        !iv_commit     TYPE char1 DEFAULT 'X'
        !iv_wait       TYPE char1 DEFAULT 'X'
      RETURNING
        VALUE(rs_tote) TYPE /scwm/s_huhdr_int
      RAISING
        zcx_core_exception .
    METHODS delete_cart
      IMPORTING
        !iv_who       TYPE /scwm/de_who
        !is_huhdr_int TYPE /scwm/s_huhdr_int
        !iv_commit    TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_core_exception .
    METHODS get_available_tote
      IMPORTING
        !iv_pmat_tote  TYPE ztout_tote_pmat-tote
      RETURNING
        VALUE(rs_tote) TYPE ty_avail_tote .
    METHODS get_cart_content
      RETURNING
        VALUE(rt_result) TYPE /scwm/tt_huhdr_int .
    METHODS get_cart_content_items
      EXPORTING
        et_hudhr TYPE /scwm/tt_huhdr_int
        et_huitm TYPE /scwm/tt_huitm_int.
    METHODS get_cart_layout
      RETURNING
        VALUE(rt_result) TYPE zcl_crud_ztout_layout_typ=>tt_layout_typ .
    METHODS get_cart_possitions_status
      RETURNING
        VALUE(rs_result) TYPE zcl_rf_pick_cart=>ty_cart_pos_status .
    METHODS get_tote_layout
      RETURNING
        VALUE(rt_result) TYPE zcl_crud_ztout_tote_pmat=>tt_tote_pmat_map .
    "! <p class="shorttext synchronized" lang="en"></p>
    METHODS move_cart_from_rsrc_to_bin
      IMPORTING
        !iv_bin TYPE /scwm/de_lgpla
        !iv_who TYPE /scwm/de_who OPTIONAL
      RAISING
        zcx_core_exception .
    METHODS move_cart_to_rsrc
      IMPORTING
        !iv_rsrc TYPE /scwm/de_rsrc
      RAISING
        zcx_core_exception .
    METHODS move_hu_in_tote
      IMPORTING
        !iv_tote   TYPE /scwm/s_rf_ptwy-nlenr
        !iv_new_hu TYPE /scwm/s_huhdr_int-guid_hu
      RAISING
        zcx_core_exception .
    METHODS move_hu_to_cart
      IMPORTING
        !iv_guid_hu TYPE /scwm/guid_hu
      RAISING
        zcx_core_exception .
    METHODS move_mc_to_cart_adhu
      IMPORTING
        !is_mc_huhdr TYPE /scwm/s_huhdr_int
      RAISING
        zcx_core_exception .
    METHODS save
      IMPORTING
        !iv_commit TYPE char1 DEFAULT 'X'
        !iv_wait   TYPE char1 DEFAULT 'X'
      RAISING
        zcx_core_exception .
    METHODS set_pick_cart
      IMPORTING
        !iv_pick_cart TYPE /scwm/de_huident
        !iv_lock      TYPE boole_d OPTIONAL
      RAISING
        zcx_core_exception .
    METHODS set_resource
      IMPORTING
        !iv_rsrc TYPE /scwm/de_rsrc
      RAISING
        zcx_core_exception .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA mo_pick_cart TYPE REF TO zcl_rf_pick_cart .
    DATA mo_pack TYPE REF TO /scwm/cl_wm_packing .
    DATA ms_resource TYPE /scwm/rsrc .
    DATA mt_cart_layout TYPE zcl_crud_ztout_layout_typ=>tt_layout_typ .
    DATA mt_tote_layout TYPE zcl_crud_ztout_tote_pmat=>tt_tote_pmat_map .
    DATA mv_lgnum TYPE /scwm/lgnum .
    DATA mv_msg TYPE string .
    DATA mv_pack_procty TYPE /scwm/de_procty .

    METHODS set_cart_profile
      IMPORTING
        !iv_queue TYPE /scwm/de_queue
      RAISING
        zcx_core_exception .
ENDCLASS.



CLASS ZCL_RF_PICK_CART IMPLEMENTATION.


  METHOD assign_hu_to_who.
********************************************************************
*& Key          : <BSUGAREV>-Aug 21, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description
*&
*&
********************************************************************
    DATA: lv_severity       TYPE bapi_mtype,
          lt_whohu          TYPE /scwm/tt_whohu_int,
          lt_whohu_maint    TYPE /scwm/tt_whohu_maint,
          lt_whohu_new_huid TYPE /scwm/tt_whohu_int,
          lt_bapiret        TYPE bapirettab.

    IF iv_who IS INITIAL.
      RETURN.
    ENDIF.

    " assign the new HU to the WHO like a pick
    " read assigned pick HUs from the WHO
    TRY.
        CALL FUNCTION '/SCWM/WHO_SELECT'
          EXPORTING
            iv_lgnum = mv_lgnum
            iv_who   = iv_who
          IMPORTING
            et_whohu = lt_whohu.
      CATCH /scwm/cx_core.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDTRY.

    SORT lt_whohu BY hukng DESCENDING.

    DATA(lv_update_type) = iv_updtyp.

    CASE lv_update_type.
      WHEN /scwm/cl_ui_to_conf=>sc_insert.

        DATA(lv_hukng_max) = VALUE #( lt_whohu[ 1 ]-hukng OPTIONAL ).

        LOOP AT it_huhdr ASSIGNING FIELD-SYMBOL(<huhdr_ins>).

          IF iv_upd_huid = abap_false.
            DATA(lv_huident) = <huhdr_ins>-huident.
          ELSE.
            " we just add new pack. proposal
            CLEAR lv_huident.
            DATA(lv_pack_mat_pall) = <huhdr_ins>-pmat_guid.

          ENDIF.

          lv_hukng_max = lv_hukng_max + 1.
          APPEND VALUE #( hukng      = lv_hukng_max
                          pmat_guid  = <huhdr_ins>-pmat_guid
                          huident    = lv_huident
                          updkz      = lv_update_type ) TO lt_whohu_maint.

        ENDLOOP.

      WHEN /scwm/cl_ui_to_conf=>sc_update.

        IF it_ordim_confirm IS NOT INITIAL.

          DATA(lt_huhdr) = it_huhdr.
          " if we have special case where we should cotrol the updated pick HUs
          LOOP AT it_ordim_confirm ASSIGNING FIELD-SYMBOL(<ls_ordim_o_huid>).

            ASSIGN lt_whohu[ huident      = space
                             huid         = <ls_ordim_o_huid>-huid ] TO FIELD-SYMBOL(<ls_who_hu>).
            CHECK sy-subrc = 0.

            ASSIGN lt_huhdr[ pmat_guid    = <ls_who_hu>-pmat_guid ] TO FIELD-SYMBOL(<ls_pickhu>).

            CHECK sy-subrc = 0.

            APPEND VALUE #( hukng      = <ls_who_hu>-hukng
                            pmat_guid  = <ls_pickhu>-pmat_guid
                            huident    = <ls_pickhu>-huident
                            updkz      = lv_update_type ) TO lt_whohu_maint.

            DELETE lt_whohu WHERE hukng   = <ls_who_hu>-hukng.
            DELETE lt_huhdr WHERE huident = <ls_pickhu>-huident.

          ENDLOOP.

        ELSE.

          LOOP AT it_huhdr ASSIGNING <ls_pickhu>.

            ASSIGN lt_whohu[  huident      = space
                              pmat_guid    = <ls_pickhu>-pmat_guid ] TO <ls_who_hu>.
            CHECK sy-subrc = 0.

            APPEND VALUE #( hukng      = <ls_who_hu>-hukng
                            pmat_guid  = <ls_pickhu>-pmat_guid
                            huident    = <ls_pickhu>-huident
                            updkz      = lv_update_type ) TO lt_whohu_maint.

            DELETE lt_whohu WHERE hukng = <ls_who_hu>-hukng.

          ENDLOOP.

        ENDIF.

    ENDCASE.

    CALL FUNCTION '/SCWM/WHO_WHOHU_MAINT'
      EXPORTING
        iv_lgnum    = mv_lgnum
        iv_who      = iv_who
        it_whohu    = lt_whohu_maint
      IMPORTING
        ev_severity = lv_severity
        et_bapiret  = lt_bapiret.

    IF lv_severity CA wmegc_severity_ea.
      LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapi_msg_movehu>)
                                       WHERE type CA wmegc_severity_ea.
        MESSAGE ID <ls_bapi_msg_movehu>-id TYPE <ls_bapi_msg_movehu>-type NUMBER <ls_bapi_msg_movehu>-number
           WITH <ls_bapi_msg_movehu>-message_v1 <ls_bapi_msg_movehu>-message_v2
                <ls_bapi_msg_movehu>-message_v3 <ls_bapi_msg_movehu>-message_v4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDLOOP.
    ENDIF.

    IF iv_commit = abap_true.
      COMMIT WORK AND WAIT.

      IF iv_upd_huid = abap_true.
        SELECT SINGLE * FROM /scwm/whohu
                                   WHERE lgnum   = @mv_lgnum AND
                                         who     = @iv_who AND
                                         pmat_guid = @lv_pack_mat_pall AND
                                         huident = @space
          INTO @DATA(ls_pall_whohu).


        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        APPEND INITIAL LINE TO lt_whohu_new_huid ASSIGNING FIELD-SYMBOL(<ls_whohu_new_shp>).
        MOVE-CORRESPONDING ls_pall_whohu TO <ls_whohu_new_shp>.

        <ls_whohu_new_shp>-updkz = /scwm/cl_ui_to_conf=>sc_update.

        ASSIGN it_ordim_confirm[ 1 ] TO FIELD-SYMBOL(<ls_ordim_shiphu>).

        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        <ls_whohu_new_shp>-huid = <ls_ordim_shiphu>-shiphuid.


        CALL FUNCTION '/SCWM/WHO_DB_UPDATE'
          IN UPDATE TASK
          EXPORTING
            it_whohu = lt_whohu_new_huid.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD calc_capa_who.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    DATA: lv_lines TYPE i.

    LOOP AT it_who ASSIGNING FIELD-SYMBOL(<ls_who>).
      SELECT who
             COUNT( * ) AS count_to
             SUM( weight ) AS sum_weight
             unit_w AS unit_w
             SUM( volum ) AS sum_volum
             unit_v AS unit_v
             SUM( solpo ) AS sum_reachtime
             zeiei AS unit_rt
             MIN( pick_comp_dt ) AS pick_comp_dt
             FROM /scwm/ordim_o
             APPENDING TABLE et_ordim_o
             WHERE lgnum = mv_lgnum AND
                   who   = <ls_who>-who
             GROUP BY who unit_w unit_v zeiei.
      IF sy-subrc = 0.
        DESCRIBE TABLE et_ordim_o LINES lv_lines.
        READ TABLE et_ordim_o ASSIGNING FIELD-SYMBOL(<ls_to>) INDEX lv_lines.
        IF sy-subrc = 0 AND <ls_to>-pick_comp_dt IS INITIAL.
          SELECT MIN( pick_comp_dt ) INTO <ls_to>-pick_comp_dt
                FROM /scwm/ordim_o
                WHERE lgnum = mv_lgnum AND
                      who   = <ls_who>-who AND
                      pick_comp_dt <> <ls_to>-pick_comp_dt.
        ENDIF.
      ENDIF.

    ENDLOOP.

    SORT et_ordim_o BY who.
  ENDMETHOD.


  METHOD constructor.
********************************************************************
*& Key          : <BSUGAREV>-Aug 21, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description
*&
*&
********************************************************************
    DATA: ls_t340d TYPE /scwm/s_t340d.

    IF iv_lgnum IS INITIAL.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    mv_lgnum = iv_lgnum.

    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = mo_pack ).

    mo_pack->/scwm/if_pack_bas~cleanup( ).

    CALL FUNCTION '/SCWM/TO_INIT_NEW'
      EXPORTING
        iv_lgnum = mv_lgnum.

    mo_pack->init( iv_lgnum = mv_lgnum ).

    CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
      EXPORTING
        iv_lgnum  = mv_lgnum
      IMPORTING
        es_t340d  = ls_t340d
      EXCEPTIONS
        not_found = 1
        OTHERS    = 99.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    mv_pack_procty = ls_t340d-pack_procty.

    IF iv_queue IS NOT INITIAL.

      mv_queue = iv_queue.
      set_cart_profile( iv_queue = iv_queue ).

    ENDIF.

  ENDMETHOD.


  METHOD create_cart_on_rsrc.
********************************************************************
*& Key          : <AYORDANOV>-15.08.2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description
*& Create Picking Cart on the resource and assign it like a pick HU
*& to the WHO
********************************************************************
    DATA: lv_pack_mat_pick_cart TYPE /scwm/de_matnr.

    IF ms_resource IS INITIAL AND iv_rsrc IS NOT INITIAL.
      set_resource( iv_rsrc ).
    ENDIF.

    IF iv_pack_matnr IS  INITIAL.
      zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_process   = zif_param_const=>c_zgen_0006
        iv_parameter = zif_param_const=>c_pmat_cart_id
      IMPORTING
        ev_constant  = DATA(lv_pack_cart) ).
    ELSE.
      lv_pack_cart =  iv_pack_matnr.
    ENDIF.

    IF lv_pack_cart IS INITIAL.
      " missing master data settings for packing material of pick.cart
      MESSAGE e026(zmc_rfui) INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = lv_pack_cart
        IMPORTING
          output       = lv_pack_mat_pick_cart
        EXCEPTIONS
          length_error = 1.
      IF sy-subrc <> 0.
        MESSAGE e026(zmc_rfui) INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.

      DATA(lv_pmat_guid_cart) = NEW /scwm/cl_ui_stock_fields( )->get_matid_by_no( iv_matnr = lv_pack_mat_pick_cart ).
    ENDIF.

    mo_pack->create_hu_on_resource(
       EXPORTING
         iv_pmat      = lv_pmat_guid_cart                " material guid16 with conversion exit
         iv_huident   = iv_pick_cart                     " handling unit identification
         iv_resource  = ms_resource-rsrc                 " resource (means of transportation or user)
       RECEIVING
         es_huhdr     = ms_pick_cart
       EXCEPTIONS
         error        = 1
         OTHERS       = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    IF iv_commit_work = abap_true.
      mo_pack->save(
        EXPORTING
          iv_commit = abap_true
          iv_wait   = abap_true
        EXCEPTIONS
          error     = 1                " see log
          OTHERS    = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ELSE.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD create_cart_on_rsrc_set_who.
********************************************************************
*& Key          : <AYORDANOV>-15.08.2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description
*& Create Picking Cart on the resource and assign it like a pick HU
*& to the WHO
********************************************************************
    DATA: lv_hukng_max          TYPE i,
          lv_pack_mat_pick_cart TYPE /scwm/de_matnr,
          lv_severity           TYPE  bapi_mtype,
          ls_resource           TYPE /scwm/rsrc,
          lt_bapiret            TYPE bapirettab,
          lt_whohu_maint        TYPE /scwm/tt_whohu_maint,
          lo_packing            TYPE REF TO /scwm/cl_wm_packing.

    IF iv_pick_cart IS INITIAL.
      " Please scan cart first.
      MESSAGE e028(zmc_rfui) INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    lo_packing = NEW #( ).

    IF lo_packing IS NOT BOUND.
      " Pick HU can not be created. Contact support team
      MESSAGE e029(zmc_rfui) INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    lo_packing->/scwm/if_pack_bas~cleanup( ).

    CALL FUNCTION '/SCWM/TO_INIT_NEW'
      EXPORTING
        iv_lgnum = iv_lgnum.

    lo_packing->init( EXPORTING iv_lgnum = iv_lgnum ).

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = iv_lgnum
        iv_process   = zif_param_const=>c_zgen_0006
        iv_parameter = zif_param_const=>c_pmat_cart_id
      IMPORTING
        ev_constant  = DATA(lv_pack_cart) ).

    IF lv_pack_cart IS INITIAL.
      " Missing master data settings for packing material of pick.cart
      MESSAGE e026(zmc_rfui) INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = lv_pack_cart
        IMPORTING
          output       = lv_pack_mat_pick_cart
        EXCEPTIONS
          length_error = 1.
      IF sy-subrc <> 0.
        MESSAGE e026(zmc_rfui) INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.

      DATA(lv_pmat_guid_cart) = NEW /scwm/cl_ui_stock_fields( )->get_matid_by_no( iv_matnr = lv_pack_mat_pick_cart ).
    ENDIF.


    CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
      EXPORTING
        iv_uname = sy-uname
      CHANGING
        cs_rsrc  = ls_resource.

    IF ls_resource IS INITIAL.
      RETURN.
    ENDIF.

    lo_packing->create_hu_on_resource(
       EXPORTING
         iv_pmat      = lv_pmat_guid_cart                " Material GUID16 with Conversion Exit
         iv_huident   = iv_pick_cart                     " Handling Unit Identification
         iv_resource  = ls_resource-rsrc                 " Resource (Means of Transportation or User)
       RECEIVING
         es_huhdr     = DATA(ls_new_pickhu)              " Internal Structure for Processing the HU Header
       EXCEPTIONS
         error        = 1
         OTHERS       = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    lo_packing->save(
      EXPORTING
        iv_commit = abap_false
        iv_wait   = abap_false
      EXCEPTIONS
        error     = 1                " See Log
        OTHERS    = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ELSE.
      COMMIT WORK AND WAIT.
    ENDIF.

    IF iv_who IS INITIAL.
      RETURN.
    ENDIF.

    lv_hukng_max = lv_hukng_max + 1.

    lt_whohu_maint = VALUE #( ( hukng      = lv_hukng_max
                                pmat_guid  = lv_pmat_guid_cart
                                huident    = ls_new_pickhu-huident
                                updkz      = /scwm/cl_ui_to_conf=>sc_insert ) ).

    CALL FUNCTION '/SCWM/WHO_WHOHU_MAINT'
      EXPORTING
        iv_lgnum    = iv_lgnum
        iv_who      = iv_who
        it_whohu    = lt_whohu_maint
      IMPORTING
        ev_severity = lv_severity
        et_bapiret  = lt_bapiret.

    IF lv_severity CA wmegc_severity_ea.
      LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapi_msg_movehu>)
                                       WHERE type CA wmegc_severity_ea.
        MESSAGE ID <ls_bapi_msg_movehu>-id TYPE <ls_bapi_msg_movehu>-type NUMBER <ls_bapi_msg_movehu>-number
           WITH <ls_bapi_msg_movehu>-message_v1 <ls_bapi_msg_movehu>-message_v2
                <ls_bapi_msg_movehu>-message_v3 <ls_bapi_msg_movehu>-message_v4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDLOOP.
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD create_pick_hu.
********************************************************************
*& Key          : <BSUGAREV>-Nov 15, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  : Create new HU
*&
********************************************************************
    DATA: lt_pickhu_upd TYPE /scwm/tt_huhdr_int,
          lt_whohu      TYPE /scwm/tt_whohu_int,
          lt_pmatid_r   TYPE rseloption.

    " assign the new HU to the WHO like a pick
    " read assigned pick HUs from the WHO
    TRY.
        CALL FUNCTION '/SCWM/WHO_SELECT'
          EXPORTING
            iv_lgnum = mv_lgnum
            iv_who   = iv_who
          IMPORTING
            et_whohu = lt_whohu.
      CATCH /scwm/cx_core.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDTRY.

    IF iv_pmatid IS NOT INITIAL.
      lt_pmatid_r = VALUE #( ( sign   = wmegc_sign_inclusive
                               option = wmegc_option_eq
                               low    = iv_pmatid ) ).

    ENDIF.

    LOOP AT lt_whohu ASSIGNING FIELD-SYMBOL(<ls_whohu>) WHERE huident IS INITIAL AND
                                                              pmat_guid IN lt_pmatid_r .
      DATA(ls_whohu) = <ls_whohu>.
      EXIT.
    ENDLOOP.

    IF ls_whohu IS INITIAL.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    IF iv_location = ms_resource-rsrc.
      mo_pack->create_hu_on_resource(
        EXPORTING
          iv_pmat      = ls_whohu-pmat_guid
          iv_huident   = iv_huident
          is_hu_create = is_hu_create
          iv_resource  = ms_resource-rsrc
        RECEIVING
          es_huhdr     = rs_huhdr_new
        EXCEPTIONS
          error        = 1
          OTHERS       = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.
    ELSE.
      mo_pack->create_hu(
        EXPORTING
          iv_pmat      = ls_whohu-pmat_guid
          iv_huident   = iv_huident
          is_hu_create = is_hu_create
          i_location   = iv_location
        RECEIVING
          es_huhdr     = rs_huhdr_new
        EXCEPTIONS
          error        = 1
          OTHERS       = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.
    ENDIF.

    " update IDENT of the newly created HU
    IF is_ident IS NOT INITIAL AND rs_huhdr_new IS NOT INITIAL.
      mo_pack->/scwm/if_pack_bas~hu_ident_set(
        EXPORTING
          iv_guid_hu = rs_huhdr_new-guid_hu
          iv_huident = is_ident-ident
          iv_idart   = is_ident-idart
        EXCEPTIONS
          error      = 1
          OTHERS     = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.
    ENDIF.

    IF iv_avoid_assign_to_who = abap_false.
      mo_pack->save(
        EXCEPTIONS
          error     = 1
          OTHERS    = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      APPEND rs_huhdr_new TO lt_pickhu_upd.

      TRY.
          assign_hu_to_who(
            iv_who    = iv_who
            iv_commit = abap_true
            iv_updtyp = /scwm/cl_ui_to_conf=>sc_update
            it_huhdr  = lt_pickhu_upd ).
        CATCH zcx_core_exception.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
          RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD create_tote.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    IF iv_location = ms_resource-rsrc.
      mo_pack->create_hu_on_resource(
        EXPORTING
          iv_pmat      = iv_pmat_guid
          iv_huident   = iv_huident
          is_hu_create = is_hu_create
          iv_resource  = ms_resource-rsrc
        RECEIVING
          es_huhdr     = DATA(ls_huhdr)
        EXCEPTIONS
          error        = 1
          OTHERS       = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.
    ELSE.
      mo_pack->create_hu(
        EXPORTING
          iv_pmat      = iv_pmat_guid
          iv_huident   = iv_huident
          is_hu_create = is_hu_create
          i_location   = iv_location
        RECEIVING
          es_huhdr     = ls_huhdr
        EXCEPTIONS
          error        = 1
          OTHERS       = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.
    ENDIF.

    " update IDENT of the newly created HU
    IF is_ident IS NOT INITIAL AND ls_huhdr IS NOT INITIAL.
      mo_pack->/scwm/if_pack_bas~hu_ident_set(
        EXPORTING
          iv_guid_hu = ls_huhdr-guid_hu
          iv_huident = is_ident-ident
          iv_idart   = is_ident-idart
        EXCEPTIONS
          error      = 1
          OTHERS     = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.
    ENDIF.

    mo_pack->save(
      EXPORTING
        iv_commit = iv_commit
        iv_wait   = iv_wait
      EXCEPTIONS
        error     = 1
        OTHERS    = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    rs_tote = ls_huhdr.
  ENDMETHOD.


  METHOD delete_cart.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    DATA: lt_whohu   TYPE /scwm/tt_whohu_maint,
          lt_bapiret TYPE bapirettab ##NEEDED.

    IF ms_pick_cart-huident <> is_huhdr_int-huident.
      " cart should be assigned first to the object
      RETURN.
    ENDIF.

    " check if this cart is still assigned to WHO
    SELECT SINGLE hukng, pmat_guid, huident
             FROM /scwm/whohu
            WHERE lgnum     = @mv_lgnum AND
                  who       = @iv_who   AND
                  huident   = @is_huhdr_int-huident
             INTO @DATA(ls_whohu).

    IF sy-subrc <> 0.
      " DELETE HU using pack object
      IF mo_pack IS NOT BOUND.
        RETURN.
      ENDIF.

      mo_pack->/scwm/if_pack_bas~delete_hu(
        EXPORTING
          iv_hu  =  is_huhdr_int-guid_hu                " Unique Internal Identification of a Handling Unit
        EXCEPTIONS
          error  = 1                " error
          OTHERS = 2 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.

      mo_pack->save(
        EXPORTING
          iv_commit = abap_false
          iv_wait   = abap_true
        EXCEPTIONS
          error     = 1                " See Log
          OTHERS    = 2 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.

      CLEAR ms_pick_cart.

    ELSE.

      lt_whohu = VALUE #( ( hukng = ls_whohu-hukng
                            pmat_guid = ls_whohu-pmat_guid
                            huident   = ls_whohu-huident
                            updkz     = /scwm/cl_ui_to_conf=>sc_delete ) ).

      CALL FUNCTION '/SCWM/WHO_WHOHU_MAINT'
        EXPORTING
          iv_lgnum    = mv_lgnum
          iv_who      = iv_who
          iv_simulate = abap_false
          it_whohu    = lt_whohu
        IMPORTING
          et_bapiret  = lt_bapiret.

      LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapi_msg>)
                                 WHERE type CA wmegc_severity_ea.
        MESSAGE ID <ls_bapi_msg>-id TYPE <ls_bapi_msg>-type NUMBER <ls_bapi_msg>-number
          WITH <ls_bapi_msg>-message_v1 <ls_bapi_msg>-message_v2  <ls_bapi_msg>-message_v3  <ls_bapi_msg>-message_v4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( )..
      ENDLOOP.

    ENDIF.

    IF iv_commit = abap_true.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD do_print_rf.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    DATA: lv_ldest TYPE /scwm/lvs_ldest.

    CALL FUNCTION '/SCWM/PRINT_GLOBAL_DATA'
      EXPORTING
        iv_rf_applic = /scwm/cl_rf_bll_srvc=>get_applic( )
        iv_ltrans    = /scwm/cl_rf_bll_srvc=>get_ltrans( )
        iv_step      = /scwm/cl_rf_bll_srvc=>get_step( )
        iv_fcode     = /scwm/cl_rf_bll_srvc=>get_fcode( ).

    lv_ldest = iv_ldest.

    CALL FUNCTION '/SCWM/PRINT_HU'
      EXPORTING
        it_huhdr          = VALUE /scwm/tt_huhdr_int( ( is_huhdr_mast_cart ) )
        iv_caller         = wmegc_hu_processing
        iv_hustep         = zif_wme_c=>gs_hustep-init "'I'
        iv_ldest          = lv_ldest
      EXCEPTIONS
        no_previous_print = 1
        error_on_log_save = 2
        previous_print    = 3
        OTHERS            = 4.

    IF sy-subrc <> 0.
      ##NEEDED
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_msg_dummy).
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_available_tote.
********************************************************************
*& Key          : BSUGAREV-Dec 14, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    DATA(lt_hus_on_cart) = get_cart_content( ).
    DATA(ls_tote_layout) = VALUE #( mt_tote_layout[ tote = iv_pmat_tote ] OPTIONAL ).

    " sort by descending to start with the last TOTE created
    SORT lt_hus_on_cart BY huident DESCENDING.

    LOOP AT lt_hus_on_cart ASSIGNING FIELD-SYMBOL(<ls_tote>) WHERE pmat_guid = iv_pmat_tote.
      rs_tote-guid_tote = <ls_tote>-guid_hu.
      rs_tote-tote = <ls_tote>-huident.
      rs_tote-position = <ls_tote>-logpos.
      rs_tote-pmatid = <ls_tote>-pmat_guid.

      LOOP AT lt_hus_on_cart ASSIGNING FIELD-SYMBOL(<ls_tote_cont>) WHERE higher_guid = <ls_tote>-guid_hu.
        rs_tote-occupied_pos = VALUE #( BASE rs_tote-occupied_pos ( id = <ls_tote_cont>-logpos ) ).
      ENDLOOP.

      IF lines( rs_tote-occupied_pos ) < ls_tote_layout-col_num.
        EXIT.
      ENDIF.

      CLEAR: rs_tote.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_cart_content.
********************************************************************
*& Key          : <BSUGAREV>-Nov 20, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  : Read all HUs on the cart
********************************************************************
    DATA(lt_hu_sel) = VALUE /scwm/tt_guid_hu( ( guid_hu = ms_pick_cart-guid_hu ) ).

    CALL FUNCTION '/SCWM/HU_SELECT'
      EXPORTING
        it_guid_hu = lt_hu_sel
      IMPORTING
        et_huhdr   = rt_result
      EXCEPTIONS
        not_found  = 1
        error      = 2
        OTHERS     = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
    ENDIF.
  ENDMETHOD.


  METHOD get_cart_content_items.
********************************************************************
*& Key          : <BSUGAREV>-Jan 19, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  : Read all HUs on the cart
********************************************************************
    DATA(lt_hu_sel) = VALUE /scwm/tt_guid_hu( ( guid_hu = ms_pick_cart-guid_hu ) ).

    CALL FUNCTION '/SCWM/HU_SELECT'
      EXPORTING
        it_guid_hu = lt_hu_sel
      IMPORTING
        et_huhdr   = et_hudhr
        et_huitm   = et_huitm
      EXCEPTIONS
        not_found  = 1
        error      = 2
        OTHERS     = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
    ENDIF.
  ENDMETHOD.


  METHOD get_cart_layout.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    rt_result = mt_cart_layout.
  ENDMETHOD.


  METHOD get_cart_possitions_status.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    DATA(lt_layout) = mt_cart_layout.

    SORT lt_layout BY row_num DESCENDING.

    DATA(ls_cart_positions) = VALUE #( lt_layout[ 1 ] OPTIONAL ).
    IF ls_cart_positions IS INITIAL.
      RETURN.
    ENDIF.

    rs_result-total = ( ls_cart_positions-col_num * ls_cart_positions-row_num ).

    DATA(lt_hus_on_cart) = get_cart_content( ).

    LOOP AT lt_hus_on_cart ASSIGNING FIELD-SYMBOL(<ls_hus>) WHERE higher_guid = ms_pick_cart-guid_hu.
      rs_result-occupied += 1.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    IF mo_pick_cart IS NOT BOUND OR
       iv_new_inst = abap_true.
      mo_pick_cart = NEW #( iv_lgnum = iv_lgnum
                            iv_queue = iv_queue ).
    ENDIF.

    ro_instance = mo_pick_cart.
  ENDMETHOD.


  METHOD get_tote_layout.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    rt_result = mt_tote_layout.
  ENDMETHOD.


  METHOD init.
********************************************************************
*& Key          : BSUGAREV-Jan 8, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF mo_pick_cart IS BOUND.
      CLEAR: mo_pick_cart.
    ENDIF.
  ENDMETHOD.


  METHOD move_cart_from_rsrc_to_bin.
********************************************************************
*& Key          : BSUGAREV-Dec 7, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    DATA: lt_bapiret TYPE bapirettab,
          ls_whohu   TYPE /scwm/whohu.

    IF iv_who IS INITIAL.
      CALL FUNCTION '/SCWM/WHOHU_SELECT'
        EXPORTING
          iv_lgnum   = mv_lgnum
          iv_huident = ms_pick_cart-huident
        IMPORTING
          es_whohu   = ls_whohu
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ELSE.
      ls_whohu-who = iv_who.
    ENDIF.

    DATA(lt_create_hu) = VALUE /scwm/tt_to_crea_hu(
        ( guid_hu = ms_pick_cart-guid_hu
          nlpla   = iv_bin
          procty  = mv_pack_procty
          norou   = wmegc_norou_yes
          squit   = abap_true
          who     = ls_whohu-who ) ).

    CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'
      EXPORTING
        iv_lgnum     = mv_lgnum
        it_create_hu = lt_create_hu
        iv_wtcode    = wmegc_wtcode_pack
      IMPORTING
        et_bapiret   = lt_bapiret.
    LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_msg>) WHERE type CA wmegc_severity_eax.
      MESSAGE ID <ls_msg>-id TYPE <ls_msg>-type NUMBER <ls_msg>-number
         WITH <ls_msg>-message_v1 <ls_msg>-message_v2 <ls_msg>-message_v3 <ls_msg>-message_v4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD move_cart_to_rsrc.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    DATA: lt_bapiret TYPE bapirettab,
          ls_whohu   TYPE /scwm/whohu.

    IF ms_resource IS INITIAL.
      set_resource( iv_rsrc = iv_rsrc ).
    ENDIF.

    CALL FUNCTION '/SCWM/WHOHU_SELECT'
      EXPORTING
        iv_lgnum   = mv_lgnum
        iv_huident = ms_pick_cart-huident
      IMPORTING
        es_whohu   = ls_whohu
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    DATA(lt_create_hu) = VALUE /scwm/tt_to_crea_hu(
        ( guid_hu = ms_pick_cart-guid_hu
          drsrc   = ms_resource-rsrc
          procty  = mv_pack_procty
          squit   = abap_true
          who     = ls_whohu-who ) ).

    CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'
      EXPORTING
        iv_lgnum     = mv_lgnum
        it_create_hu = lt_create_hu
        iv_wtcode    = wmegc_wtcode_pack
      IMPORTING
        et_bapiret   = lt_bapiret.
    LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_msg>) WHERE type CA wmegc_severity_eax.
      MESSAGE ID <ls_msg>-id TYPE <ls_msg>-type NUMBER <ls_msg>-number
         WITH <ls_msg>-message_v1 <ls_msg>-message_v2 <ls_msg>-message_v3 <ls_msg>-message_v4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD move_hu_in_tote.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    DATA: lt_bapiret TYPE bapirettab.

    DATA(lt_cart_hus) = get_cart_content( ).

    DATA(ls_tote) = VALUE #( lt_cart_hus[ huident = iv_tote ] OPTIONAL ).
    IF ls_tote IS INITIAL.
      MESSAGE e053(zmc_rfui) INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    DATA(lt_create_hu) = VALUE /scwm/tt_to_crea_hu(
        ( guid_hu  = iv_new_hu
          dguid_hu = ls_tote-guid_hu
          procty   = mv_pack_procty
          squit    = abap_true
          norou    = wmegc_norou_yes ) ).

    CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'
      EXPORTING
        iv_lgnum     = mv_lgnum
        it_create_hu = lt_create_hu
        iv_wtcode    = wmegc_wtcode_pack
      IMPORTING
        et_bapiret   = lt_bapiret.
    LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_msg>) WHERE type CA wmegc_severity_eax.
      MESSAGE ID <ls_msg>-id TYPE <ls_msg>-type NUMBER <ls_msg>-number
         WITH <ls_msg>-message_v1 <ls_msg>-message_v2 <ls_msg>-message_v3 <ls_msg>-message_v4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD move_hu_to_cart.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
*&
********************************************************************
    DATA: lt_bapiret TYPE bapirettab.

    DATA(lt_create_hu) = VALUE /scwm/tt_to_crea_hu(
        ( guid_hu  = iv_guid_hu
          dguid_hu = ms_pick_cart-guid_hu
          procty   = mv_pack_procty
          squit    = abap_true
          norou    = wmegc_norou_yes ) ).

    CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'
      EXPORTING
        iv_lgnum     = mv_lgnum
        it_create_hu = lt_create_hu
        iv_wtcode    = wmegc_wtcode_pack
      IMPORTING
        et_bapiret   = lt_bapiret.
    LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_msg>) WHERE type CA wmegc_severity_eax.
      MESSAGE ID <ls_msg>-id TYPE <ls_msg>-type NUMBER <ls_msg>-number
         WITH <ls_msg>-message_v1 <ls_msg>-message_v2 <ls_msg>-message_v3 <ls_msg>-message_v4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD move_mc_to_cart_adhu.
********************************************************************
*& Key          : <AYORDANOV>-31.08.2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description
*& Move MC to Pick Cart via TO Create manually ( like that we will
*& avoid cons.group check in /SCWM/EX_WRKC_PACK and we will not be in
*& backgroun )
********************************************************************
    DATA: lv_severity      TYPE bapi_mtype,
          ls_t340d         TYPE /scwm/s_t340d,
          lt_bapiret       TYPE bapiret2_t,
          lt_create_hu_int TYPE /scwm/tt_to_crea_hu_int.

    IF ms_pick_cart IS INITIAL.
      " Pick Cart is not created ( or missing in the object )
      MESSAGE e024(zmc_out) INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    IF is_mc_huhdr IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
      EXPORTING
        iv_lgnum  = mv_lgnum
      IMPORTING
        es_t340d  = ls_t340d
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    lt_create_hu_int = VALUE #( ( huident  = is_mc_huhdr-huident
                                  guid_hu  = is_mc_huhdr-guid_hu
                                  procty   = ls_t340d-pack_procty
                                  squit    = abap_true
                                  norou    = wmegc_norou_yes
                                  nlenr    = ms_pick_cart-huident
                                  dguid_hu = ms_pick_cart-guid_hu ) ).

    CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'                    "#EC ENHOK
      EXPORTING
        iv_lgnum       = mv_lgnum
        it_create_hu   = lt_create_hu_int
        iv_wtcode      = wmegc_wtcode_adhoc_hu
        iv_update_task = abap_true
        iv_commit_work = abap_false
      IMPORTING
        et_bapiret     = lt_bapiret
        ev_severity    = lv_severity.

    IF lv_severity CA wmegc_severity_ea.

      ROLLBACK WORK.

      " in a case of error system will raise just the first error message
      LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_error>) WHERE type CA wmegc_severity_ea .
        MESSAGE ID <ls_error>-id TYPE <ls_error>-type NUMBER <ls_error>-number
                      WITH <ls_error>-message_v1 <ls_error>-message_v2 <ls_error>-message_v3 <ls_error>-message_v4
                      INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDLOOP.

    ELSE.

      COMMIT WORK AND WAIT.

    ENDIF.
  ENDMETHOD.


  METHOD save.
********************************************************************
*& Key          : <BSUGAREV>-Nov 21, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  : Save entries
********************************************************************
    mo_pack->save(
      EXPORTING
        iv_commit = iv_commit
        iv_wait   = iv_wait
      EXCEPTIONS
        error     = 1
        OTHERS    = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD set_cart_profile.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    zcl_crud_ztout_que_ly_map=>select_single_by_queue(
      EXPORTING
        iv_lgnum      = mv_lgnum
        iv_queue      = iv_queue                 " Queue
      IMPORTING
        es_que_ly_map = DATA(ls_que_ly_map) ).

    IF ls_que_ly_map IS NOT INITIAL.
      zcl_crud_ztout_layout_typ=>select_multi_by_layout(
        EXPORTING
          iv_lgnum      = mv_lgnum
          it_layout_r   = VALUE rseloption( ( sign   = wmegc_sign_inclusive
                                              option = wmegc_option_eq
                                              low    = ls_que_ly_map-type_id ) )
        IMPORTING
          et_layout_typ = mt_cart_layout ).
    ELSE.
      RETURN.
    ENDIF.

    IF lines( mt_cart_layout ) = 0.
      MESSAGE e034(zmc_rfui) WITH iv_queue INTO mv_msg.
      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    zcl_crud_ztout_tote_pmat=>select_all(
      EXPORTING
        iv_lgnum         = mv_lgnum
      IMPORTING
        et_tote_pmat_map = mt_tote_layout ).

  ENDMETHOD.


  METHOD set_pick_cart.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    IF ms_pick_cart IS NOT INITIAL.
      RETURN.
    ENDIF.

    mo_pack->get_hu(
      EXPORTING
        iv_huident = iv_pick_cart
        iv_lock    = iv_lock
      IMPORTING
        es_huhdr   = ms_pick_cart
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD set_resource.
********************************************************************
*& Key          : BSUGAREV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
********************************************************************
    IF iv_rsrc IS INITIAL.
      CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
        EXPORTING
          iv_uname = sy-uname
        CHANGING
          cs_rsrc  = ms_resource.

      IF ms_resource IS INITIAL.
        MESSAGE e204(/scwm/rf_de) INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.
    ELSE.
      CALL FUNCTION '/SCWM/RSRC_READ_SINGLE'
        EXPORTING
          iv_lgnum    = mv_lgnum
          iv_rsrc     = iv_rsrc
*         iv_db_lock  = ' '
        IMPORTING
          es_rsrc     = ms_resource
        EXCEPTIONS
          wrong_input = 1
          not_found   = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mv_msg.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
