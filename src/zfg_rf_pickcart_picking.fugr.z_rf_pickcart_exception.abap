FUNCTION z_rf_pickcart_exception.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_NESTED_HU) TYPE  /SCWM/TT_RF_NESTED_HU
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(WHO) TYPE  /SCWM/S_WHO_INT
*"     REFERENCE(WME_VERIF) TYPE  /SCWM/S_WME_VERIF
*"     REFERENCE(CT_SERNR) TYPE  /SCWM/TT_RF_SERNR
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  : Update task with exception code: split or pick denial
*&
********************************************************************

  CASE /scwm/cl_rf_bll_srvc=>get_fcode( ).

    WHEN zif_rfui_c=>gs_fcode-split.
      zcl_param=>get_parameter(
        EXPORTING
          iv_lgnum     = resource-lgnum
          iv_process   = zif_param_const=>c_zrfui_0004
          iv_parameter = zif_param_const=>c_exccode_split
        IMPORTING
          ev_constant  = DATA(lv_exccode_split) ).

      /scwm/cl_rf_bll_srvc=>set_shortcut( iv_shortcut = CONV #( lv_exccode_split ) ).

    WHEN zif_rfui_c=>gs_fcode-bindpb.
      zcl_param=>get_parameter(
        EXPORTING
          iv_lgnum     = resource-lgnum
          iv_process   = zif_param_const=>c_zrfui_0004
          iv_parameter = zif_param_const=>c_exccode_bidp
        IMPORTING
          ev_constant  = DATA(lv_exccode_bidp) ).

      /scwm/cl_rf_bll_srvc=>set_shortcut( iv_shortcut = CONV #( lv_exccode_bidp ) ).

  ENDCASE.

  /scwm/cl_rf_bll_srvc=>set_line( 1 ).

  CALL FUNCTION '/SCWM/RF_PICK_SET_GLOBVAR'
    EXPORTING
      iv_exec_step = wmegc_execstep_05.

  CALL FUNCTION '/SCWM/RF_PICK_EXCEPTION'
    CHANGING
      tt_ordim_confirm = tt_ordim_confirm
      ordim_confirm    = ordim_confirm
      tt_nested_hu     = tt_nested_hu
      resource         = resource
      selection        = selection
      who              = who
      wme_verif        = wme_verif
      ct_sernr         = ct_sernr.

ENDFUNCTION.
