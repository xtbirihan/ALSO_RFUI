FUNCTION z_rf_pick_piplhu_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(WME_VERIF) TYPE  /SCWM/S_WME_VERIF
*"     REFERENCE(CS_REHU_HU) TYPE  /SCWM/S_RF_REHU_HU
*"----------------------------------------------------------------------

  BREAK-POINT ID /scwm/rf_picking.


  NEW lcl_piplhu_pbo( )->execute(
    CHANGING
      cs_resource      = resource
      cs_ordim_confirm = ordim_confirm
      ct_ordim_confirm = tt_ordim_confirm
      cs_wme_verif     = wme_verif
      cs_rehu_hu       = cs_rehu_hu ).

ENDFUNCTION.
