"Name: \TY:/SCWM/CL_RF_RSRC_REBUNDLING\ME:CHECK_WT_BEFORE_REBUNDLING\SE:BEGIN\EI
ENHANCEMENT 0 ZEI_CL_RF_RSRC_REBUNDLING.
********************************************************************
*& Key          : BSUGAREV-Jan 24, 2024
*& Request No.  : GAP-017 FS Picking WO Bundling
********************************************************************
*& Description	: Enh. object: /SCWM/CL_RF_RSRC_REBUNDLING=>CHECK_WT_BEFORE_REBUNDLING
*&
********************************************************************
  IF zcl_rf_wo_rebundle_man=>is_zrebundle_active( iv_lgnum ) = abap_true.
    RETURN.
  ENDIF.
ENDENHANCEMENT.
