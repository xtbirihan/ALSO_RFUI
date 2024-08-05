FUNCTION z_rf_pick_pichuty_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(CS_REHU_HU) TYPE  /SCWM/S_RF_REHU_HU
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"----------------------------------------------------------------------

  NEW lcl_pichty_pai( )->execute(
    CHANGING
      cs_resource      = resource
      cs_ordim_confirm = ordim_confirm
      ct_ordim_confirm = tt_ordim_confirm
      cs_rehu_hu       = cs_rehu_hu ).

ENDFUNCTION.
