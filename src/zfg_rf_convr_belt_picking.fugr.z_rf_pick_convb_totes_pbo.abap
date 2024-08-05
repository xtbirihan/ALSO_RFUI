FUNCTION Z_RF_PICK_CONVB_TOTES_PBO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(CS_HUHDR) TYPE  /SCWM/S_HUHDR_INT
*"     REFERENCE(CT_HUHDR) TYPE  /SCWM/TT_HUHDR_INT
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 28, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&
*&
********************************************************************
  NEW lcl_totes_pbo( )->execute(
    CHANGING
      cs_selection = selection
      cs_huhdr     = cs_huhdr
      ct_huhdr     = ct_huhdr ).
ENDFUNCTION.
