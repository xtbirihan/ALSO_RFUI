class ZCL_WHO_EEW_CHANGE_FLAG definition
  public
  final
  create public .

public section.

  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_WHO_EEW_CHANGE_FLAG .
  class-methods SET_FLAG
    importing
      !IV_FLAG type XFELD .
  class-methods GET_FLAG
    returning
      value(RV_FLAG) type XFELD .
protected section.
private section.

  class-data MO_INSTANCE type ref to ZCL_WHO_EEW_CHANGE_FLAG .
  class-data MO_FLAG type XFELD .
ENDCLASS.



CLASS ZCL_WHO_EEW_CHANGE_FLAG IMPLEMENTATION.


  METHOD get_flag.
*********************************************************************
*& Key           : <aahmedov>-05.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************
    rv_flag = mo_flag.

  ENDMETHOD.


  METHOD get_instance.
*********************************************************************
*& Key           : <aahmedov>-05.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************

    IF mo_instance IS NOT BOUND.
      mo_instance = NEW zcl_who_eew_change_flag( ).
    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.


  METHOD set_flag.
*********************************************************************
*& Key           : <aahmedov>-05.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************

    mo_flag = iv_flag.

  ENDMETHOD.
ENDCLASS.
