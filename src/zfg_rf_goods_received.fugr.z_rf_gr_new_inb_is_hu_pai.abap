FUNCTION z_rf_gr_new_inb_is_hu_pai .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_RF_GR_DUMMY_HU) TYPE  ZSTR_RF_GR_DUMMY_HU
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Create Inbound Shipment (IS) number for GR Dummy HU
**********************************************************************

  NEW lcl_create_inb_shipment( )->inbound_shipment_create_pai(
       CHANGING
         zcs_rf_gr_dummy_hu = zcs_rf_gr_dummy_hu ).

ENDFUNCTION.
