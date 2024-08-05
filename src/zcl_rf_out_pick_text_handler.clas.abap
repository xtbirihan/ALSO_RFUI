class ZCL_RF_OUT_PICK_TEXT_HANDLER definition
  public
  final
  create public .

public section.

  class-methods GET_TEXTS
    importing
      !IV_HEADER type BOOLE_D optional
      !IV_ITEM type BOOLE_D optional
      !IV_ACTTY type /SCWM/DE_ACTTY
      !IS_DOCID_ITMID type /SCWM/DLV_DOCID_ITEM_STR
    returning
      value(RV_RESULT) type /SCWM/DE_RF_TEXT_SCR .
  class-methods MAPPING_TO_SCR_FIELD
    importing
      !IV_TEXT_RESULT type /SCWM/DE_RF_TEXT_SCR .
  class-methods GET_TEXT_MATNR
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IV_MATNR type /SCDL/DL_PRODUCTNO
    returning
      value(RV_RESULT_MATNR_TEXT) type /SCWM/DE_RF_TEXT_SCR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_RF_OUT_PICK_TEXT_HANDLER IMPLEMENTATION.


  METHOD get_texts.
********************************************************************
*& Key          : <BSUGAREV>-Nov 23, 2023
*& Request No.  :
********************************************************************
*& Description  : Collect texts
*&
*&
********************************************************************
    DATA: lt_text_id TYPE /scwm/tt_tactty_tid,
          lt_result  TYPE  zif_text_handling=>tt_text.

    TRY.
        NEW /scwm/cl_dlv_management_prd( )->query(
          EXPORTING
            it_docid                    = VALUE #( ( is_docid_itmid ) )
            is_read_options             = VALUE #( data_retrival_only = abap_true
                                                   mix_in_object_instances = abap_true )
            is_include_data             = VALUE #( head_partyloc = abap_true
                                                   head_text     = abap_true
                                                   head_textline = abap_true
                                                   item_partyloc = abap_true
                                                   item_text     = abap_true
                                                   item_textline = abap_true )
          IMPORTING
            et_headers                  = DATA(lt_dlv_head)
            et_items                    = DATA(lt_dlv_item) ).
      CATCH /scdl/cx_delivery.
    ENDTRY.

    IF lines( lt_dlv_item ) = 0.
      RETURN.
    ENDIF.

    DATA(ls_head) = lt_dlv_head[ 1 ].
    DATA(ls_item) = lt_dlv_item[ 1 ].

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = ls_item-sapext-/scwm/whno
        iv_process   = zif_param_const=>c_zout_0001
        iv_parameter = zif_param_const=>c_procty_kep
      IMPORTING
        ev_constant  = DATA(lv_procty_kep) ).

    " we have texts for KEP and SPED. If it is not KEP, then it is SPED
    IF lv_procty_kep <> ls_item-sapext-/scwm/procty.
      CLEAR: lv_procty_kep.
    ENDIF.

    DATA(lt_text_type) = VALUE zif_text_handling=>tt_text_type( ).

    IF lv_procty_kep IS NOT INITIAL.
      lt_text_type = VALUE #( ( zif_wme_c=>gc_text_types-general_material_picking )
                              ( zif_wme_c=>gc_text_types-kep_material_picking )
                              ( zif_wme_c=>gc_text_types-cust_general_picking )
                              ( zif_wme_c=>gc_text_types-cust_kep_picking ) ).
    ELSE.
      lt_text_type = VALUE #( ( zif_wme_c=>gc_text_types-general_material_picking )
                              ( zif_wme_c=>gc_text_types-sped_material_picking )
                              ( zif_wme_c=>gc_text_types-cust_general_picking )
                              ( zif_wme_c=>gc_text_types-cust_sped_picking ) ).
    ENDIF.

    " Text id's and text type's are determined now
    CALL FUNCTION '/SCWM/TEXTID_TEXTTYP_READ'
      EXPORTING
        iv_lgnum     = ls_item-sapext-/scwm/whno
        iv_actty     = iv_actty
      IMPORTING
        et_actty_tid = lt_text_id
      EXCEPTIONS
        wrong_input  = 1
        not_found    = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " determine texts
    DATA(lo_text) = zcl_text_handling=>get_instance_for_warehouse( ls_item-sapext-/scwm/whno ).

    IF iv_header = abap_true.
      " find correct BP
      DATA(ls_bp) = VALUE #( ls_head-partyloc[ party_role = zif_wme_c=>gs_partyloc_c-sc_party_role_zcuspr ] OPTIONAL ).
      IF ls_bp IS INITIAL.
        ls_bp = VALUE #( ls_item-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ] OPTIONAL ).
      ENDIF.

      DATA(lt_text_bp) = lo_text->get_texts_from_bp( iv_business_partner = ls_bp-partyno
                                                     it_text_type        = lt_text_type
                                                     iv_no_del_note      = abap_true ).

      lt_result = VALUE #( BASE lt_result FOR <l> IN lt_text_bp ( <l> ) ).

      lt_result = VALUE #( BASE lt_result
                           FOR <tid> IN lt_text_id WHERE ( tobject = wmegc_tobject_dlvh )
                           FOR <td> IN ls_head-text WHERE ( text_type = <tid>-text_id AND langu = sy-langu )
                           ( text_type = <td>-text_type
                             text      = VALUE #( <td>-text_lines[ 1 ]-tdline ) ) ).
    ENDIF.

    IF iv_item = abap_true.
      DATA(lt_text_prod) = lo_text->get_texts_from_prod( iv_lgnum     = ls_item-sapext-/scwm/whno
                                                         iv_entitled  = ls_item-sapext-entitled
                                                         iv_product   = ls_item-product-productno
                                                         it_text_type = lt_text_type ).

      lt_result = VALUE #( FOR <l> IN lt_text_prod ( <l> ) ).

      lt_result = VALUE #( BASE lt_result
                           FOR <tid> IN lt_text_id WHERE ( tobject = wmegc_tobject_dlvi )
                           FOR <ti> IN ls_item-text WHERE ( text_type = <tid>-text_id AND langu = sy-langu )
                           ( text_type = <ti>-text_type
                             text      = VALUE #( <ti>-text_lines[ 1 ]-tdline ) ) ).
    ENDIF.


    IF lines( lt_result ) = 0.
      RETURN.
    ENDIF.

    DATA(lv_str) = VALUE #( lt_result[ 1 ]-text OPTIONAL ).

    DELETE lt_result INDEX 1.

    rv_result = REDUCE string( INIT txt = lv_str FOR <t> IN lt_result NEXT txt = |{ txt };{ <t>-text }| ).


  ENDMETHOD.


  METHOD get_text_matnr.

    DATA: lt_result  TYPE  zif_text_handling=>tt_text.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = iv_lgnum
        iv_process   = zif_param_const=>c_zout_0001
        iv_parameter = zif_param_const=>c_procty_kep
      IMPORTING
        ev_constant  = DATA(lv_procty_kep) ).

    DATA(lt_text_type) = VALUE zif_text_handling=>tt_text_type( ).

    IF lv_procty_kep IS NOT INITIAL.
      lt_text_type = VALUE #( ( zif_wme_c=>gc_text_types-general_material_picking )
                              ( zif_wme_c=>gc_text_types-kep_material_picking )
                              ( zif_wme_c=>gc_text_types-cust_general_picking )
                              ( zif_wme_c=>gc_text_types-cust_kep_picking ) ).
    ELSE.
      lt_text_type = VALUE #( ( zif_wme_c=>gc_text_types-general_material_picking )
                              ( zif_wme_c=>gc_text_types-sped_material_picking )
                              ( zif_wme_c=>gc_text_types-cust_general_picking )
                              ( zif_wme_c=>gc_text_types-cust_sped_picking ) ).
    ENDIF.

    DATA(lo_text) = zcl_text_handling=>get_instance_for_warehouse( iv_lgnum ).

    DATA(lt_text_prod) = lo_text->get_texts_from_prod( iv_lgnum     = iv_lgnum
                                                       iv_entitled  = iv_entitled
                                                       iv_product   = iv_matnr
                                                       it_text_type = lt_text_type ).

    lt_result = VALUE #( FOR <l> IN lt_text_prod ( <l> ) ).

    IF lines( lt_result ) = 0.
      RETURN.
    ENDIF.

    DATA(lv_str) = VALUE #( lt_result[ 1 ]-text OPTIONAL ).

    DELETE lt_result INDEX 1.

    rv_result_matnr_text = REDUCE string( INIT txt = lv_str FOR <t> IN lt_result NEXT txt = |{ txt };{ <t>-text }| ).

  ENDMETHOD.


  METHOD mapping_to_scr_field.


    DATA(lv_txt_to_str) = CONV string( iv_text_result ).
    DATA(lv_row_first) = lv_txt_to_str.

    DATA(lv_str_len) = strlen( lv_txt_to_str ).

    IF strlen( lv_txt_to_str ) > 132.
      lv_str_len -= 132.

      lv_row_first = lv_txt_to_str+0(132).
      DATA(lv_row_sec) = lv_txt_to_str+132(lv_str_len).
    ENDIF.

    /scwm/cl_rf_bll_srvc=>set_rf_text( it_text = VALUE #( ( CONV #( lv_row_first ) )
                                                          ( CONV #( lv_row_sec ) ) ) ).


  ENDMETHOD.
ENDCLASS.
