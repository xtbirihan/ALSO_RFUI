FUNCTION z_rf_pick_kep_conv_bin_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"----------------------------------------------------------------------
********************************************************************
*& Key          : BSUGAREV-Jan 26, 2024
*& Request No.  : ​​GAP-051 RF Picking transaction for KEP Master Cartons
********************************************************************
*& Description  : Check scanned staring bin
*&
********************************************************************

  NEW lcl_out_kep_mc_picking( )->conveyer_start_bin_pai(
    CHANGING
      cs_sel_queue_pickcart = zcs_sel_queue_pickcart ).

ENDFUNCTION.
