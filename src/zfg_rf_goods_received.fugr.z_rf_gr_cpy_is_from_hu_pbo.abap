FUNCTION z_rf_gr_cpy_is_from_hu_pbo .
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
*& Copy Inbound Shipment (IS) number for GR Dummy HU
*& from the HU intered in the sceen
**********************************************************************


  NEW lcl_copy_is_from_hu( )->inbound_shipment_copy_pbo(
      CHANGING
        zcs_rf_gr_dummy_hu = zcs_rf_gr_dummy_hu ).

ENDFUNCTION.
