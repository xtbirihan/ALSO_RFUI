CLASS zcl_crud_ztrfui_picdstseq DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES tt_ztrfui_picdstseq TYPE STANDARD TABLE OF ztrfui_picdstseq WITH EMPTY KEY.

    CLASS-METHODS select_multi_by_lgnum
      IMPORTING
        iv_lgnum         TYPE /scwm/lgnum
      RETURNING
        VALUE(rt_result) TYPE tt_ztrfui_picdstseq.

    CLASS-METHODS select_single_by_key
      IMPORTING
        iv_lgnum         TYPE /scwm/lgnum
        iv_lgtyp         TYPE /scwm/lgtyp
      RETURNING
        VALUE(rs_result) TYPE ztrfui_picdstseq.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTRFUI_PICDSTSEQ IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
********************************************************************
*& Key          : <BSUGAREV>-Oct 30, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM ztrfui_picdstseq
      INTO TABLE @rt_result
     WHERE lgnum = @iv_lgnum.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ZTRFUI_PICDSTSEQ' sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_single_by_key.
********************************************************************
*& Key          : <BSUGAREV>-Oct 30, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztrfui_picdstseq
      INTO @rs_result
     WHERE lgnum = @iv_lgnum
       AND lgtyp = @iv_lgtyp.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ZTRFUI_PICDSTSEQ' sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
