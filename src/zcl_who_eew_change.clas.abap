CLASS zcl_who_eew_change DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_who_eew_change .
    INTERFACES if_badi_interface .

    CLASS-METHODS set_bundle_flag
      IMPORTING
        !iv_flag TYPE boole_d.

    CLASS-METHODS set_pathseq_category
      IMPORTING
        is_who_eew TYPE /scwm/s_wo_eew_update.

    CLASS-METHODS update_pathseq_category
      IMPORTING
        it_who       TYPE /scwm/tt_who_int
      CHANGING
        cv_badi_used TYPE xfeld
        ct_who_eew   TYPE /scwm/tt_wo_eew_update.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: ss_who_eew     TYPE /scwm/s_wo_eew_update,
                sv_bundle_flag TYPE boole_d.

ENDCLASS.



CLASS ZCL_WHO_EEW_CHANGE IMPLEMENTATION.


  METHOD /scwm/if_ex_who_eew_change~update.
*********************************************************************
*& Key           : <aahmedov>-04.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_rsrc_rebundle.

    update_pathseq_category(
      EXPORTING
        it_who       = it_who
      CHANGING
        cv_badi_used = cv_badi_used
        ct_who_eew   = ct_who_eew ).

    IF it_who IS INITIAL OR sv_bundle_flag = abap_false.
      RETURN.
    ENDIF.

    LOOP AT it_who ASSIGNING FIELD-SYMBOL(<ls_who>).

      CHECK <ls_who>-zz_bundled <> abap_true.

      ASSIGN ct_who_eew[ who = <ls_who>-who ] TO FIELD-SYMBOL(<ls_who_eew>).
      IF <ls_who_eew> IS INITIAL.
        APPEND INITIAL LINE TO ct_who_eew ASSIGNING <ls_who_eew>.
      ENDIF.

      <ls_who_eew>-lgnum = <ls_who>-lgnum.
      <ls_who_eew>-who = <ls_who>-who.
      <ls_who_eew>-zz_bundled = abap_true.

      DATA(lv_subrc) = abap_true.

    ENDLOOP.

    " if at least one WHO flag was changed,
    " then set BADI_USED flag to TRUE
    " and the WHO_EEW_CHANGE flag to FALSE
    cv_badi_used = lv_subrc.

    sv_bundle_flag = xsdbool( lv_subrc <> abap_true ).
  ENDMETHOD.


  METHOD set_bundle_flag.
*********************************************************************
*& Key           : <aahmedov>-05.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************

    sv_bundle_flag = iv_flag.
  ENDMETHOD.


  METHOD set_pathseq_category.
********************************************************************
*& Key          : BSUGAREV-Jan 16, 2024
*& Request No.  : GAP-076 Outbound Priority settings
********************************************************************
*& Description  :
*&
********************************************************************
    ss_who_eew = is_who_eew.
  ENDMETHOD.


  METHOD update_pathseq_category.
********************************************************************
*& Key          : BSUGAREV-Jan 16, 2024
*& Request No.  : GAP-076 Outbound Priority settings
********************************************************************
*& Description  :
*&
********************************************************************
    DATA(ls_who_eew) = ss_who_eew.

    CLEAR: ss_who_eew.

    IF lines( it_who ) = 0 OR ls_who_eew IS INITIAL.
      RETURN.
    ENDIF.

    IF lines( it_who ) > 1.
      DATA(ls_who) = VALUE #( it_who[ who = ls_who_eew-who ] OPTIONAL ).
    ELSE.
      ls_who = it_who[ 1 ].
    ENDIF.

    IF ls_who IS INITIAL.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO ct_who_eew ASSIGNING FIELD-SYMBOL(<ls_who_eew>).

    <ls_who_eew> = VALUE #( lgnum = ls_who-lgnum
                            who   = ls_who-who
                            zz_pathseq_fr = ls_who_eew-zz_pathseq_fr
                            zz_category   = ls_who_eew-zz_category ).

    cv_badi_used = abap_true.
  ENDMETHOD.
ENDCLASS.
