FUNCTION z_rf_pick_xsernr_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_SN) TYPE  /SCWM/S_RF_SN
*"     REFERENCE(CT_SERNR) TYPE  /SCWM/TT_RF_SERNR
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
********************************************************************
*& Description  : Handle scanned serial number
*&
*&
********************************************************************

  NEW lcl_serial_numb_pai( )->execute(
    CHANGING
      cs_sn    = cs_sn
      ct_sernr = ct_sernr
      cs_ordim_confirm = ordim_confirm
      ct_ordim_confirm = tt_ordim_confirm ).

ENDFUNCTION.
