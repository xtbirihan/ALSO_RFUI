FUNCTION Z_RF_PICK_CONVB_ULCMPL_PBO.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"--------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&
*&
********************************************************************
  NEW lcl_unload_pbo( )->execute(
    CHANGING
      cs_selection     = selection
      cs_resource      = resource
      cs_ordim_confirm = ordim_confirm ).

ENDFUNCTION.
