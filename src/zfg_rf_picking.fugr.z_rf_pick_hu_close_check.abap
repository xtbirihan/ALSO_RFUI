FUNCTION z_rf_pick_hu_close_check.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(WHO) TYPE  /SCWM/S_WHO_INT
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_NESTED_HU) TYPE  /SCWM/TT_RF_NESTED_HU
*"     REFERENCE(T_RF_PICK_HUS) TYPE  /SCWM/TT_RF_PICK_HUS
*"----------------------------------------------------------------------

  NEW lcl_ptwy_close_hu_check( )->execute(
    CHANGING
      cs_selection        = selection
      cs_resource         = resource
      cs_who              = who
      cs_ordim_confirm    = ordim_confirm
      ct_ordim_confirm = tt_ordim_confirm
      ct_nested_hu     = tt_nested_hu
      ct_rf_pick_hus    = t_rf_pick_hus ).

  NEW lcl_pick_hu_close_check( )->execute(
    CHANGING
      cs_selection        = selection
      cs_resource         = resource
      cs_who              = who
      cs_ordim_confirm    = ordim_confirm
      ct_ordim_confirm = tt_ordim_confirm
      ct_nested_hu     = tt_nested_hu
      ct_rf_pick_hus    = t_rf_pick_hus ).

ENDFUNCTION.
