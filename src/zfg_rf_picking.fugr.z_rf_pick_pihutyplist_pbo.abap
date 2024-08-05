FUNCTION z_rf_pick_pihutyplist_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_HUHDR) TYPE  /SCWM/S_HUHDR_INT
*"     REFERENCE(CS_REHU_HU) TYPE  /SCWM/S_RF_REHU_HU
*"     REFERENCE(CT_REHU_HU) TYPE  /SCWM/TT_RF_REHU_HU
*"----------------------------------------------------------------------

  NEW lcl_hutyplist_pbo( )->execute(
    CHANGING
      cs_huhdr   = cs_huhdr
      cs_rehu_hu = cs_rehu_hu
      ct_rehu_hu = ct_rehu_hu ).

ENDFUNCTION.
