FUNCTION z_rf_pick_xsernr_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_SN) TYPE  /SCWM/S_RF_SN
*"     REFERENCE(CT_SERNR) TYPE  /SCWM/TT_RF_SERNR
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
********************************************************************
*& Description  : Serial Number PBO step. Loading of the screen
*&
********************************************************************

  NEW lcl_serial_numb_pbo( )->execute(
    CHANGING
      cs_sn    = cs_sn
      ct_sernr = ct_sernr ).

ENDFUNCTION.
