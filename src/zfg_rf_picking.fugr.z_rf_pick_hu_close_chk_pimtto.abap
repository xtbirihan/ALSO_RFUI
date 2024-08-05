FUNCTION z_rf_pick_hu_close_chk_pimtto.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(WHO) TYPE  /SCWM/S_WHO_INT
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_NESTED_HU) TYPE  /SCWM/TT_RF_NESTED_HU
*"     REFERENCE(T_RF_PICK_HUS) TYPE  /SCWM/TT_RF_PICK_HUS
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-190423
*& Request No.   : GAP-004 – Display Partial Replenisment
**********************************************************************
*& Description (short)
*& Fill some verification fields automatically if partial replenishment
*& is in process
**********************************************************************
* need to check data and to define next step according to customizing
  DATA: lv_applic         TYPE /scwm/de_applic,
        lv_pres_prf       TYPE /scwm/de_pres_prf,
        lv_prsn_prf       TYPE /scwm/de_prsn_prf,
        lv_ltrans         TYPE /scwm/de_ltrans,
        lv_step           TYPE /scwm/de_step,
        lt_valid_prf      TYPE /scwm/tt_valid_prf_ext,
        ls_valid_prf      TYPE /scwm/s_valid_prf_ext,
        lv_close_hu_verif TYPE xfeld VALUE IS INITIAL,
        lv_field          TYPE text60.

  DATA: lv_subrc TYPE sysubrc,
        lv_nlenr TYPE /scwm/ltap_nlenr,
        lv_huent TYPE /scwm/ltap_hu_huent,
        lv_line  TYPE numc4.

  DATA: ls_ordim_o     TYPE /scwm/ordim_o,
        ls_t331        TYPE /scwm/t331,
        ls_t333        TYPE /scwm/t333,
        ls_vlenr_check TYPE c.
  DATA: ls_stock        TYPE /scwm/s_stock,
        lv_guid_stock   TYPE /lime/guid_stock,
        lv_vlenr        TYPE /scwm/de_huident,
        lt_huitm        TYPE /scwm/tt_huitm_int,
        ls_huitm        TYPE /scwm/s_huitm_int,
        lv_quantity     TYPE /lime/quantity,
        lv_quantity_dis TYPE /lime/quantity,
        ls_exc_tab      TYPE /scwm/s_rf_exc,
        lv_need_errmsg  TYPE boolean,
        lt_huident      TYPE /scwm/tt_huident,
        ls_huident      TYPE /scwm/s_huident,
        lv_huident      TYPE /scwm/de_rf_huident,
        hu_charg        TYPE /scwm/de_rf_charg.

  DATA  lo_stock_fields          TYPE REF TO /scwm/cl_ui_stock_fields.

  DATA: lv_vsola TYPE /scwm/de_quantity,
        lv_fcode TYPE /scwm/de_fcode,
        lv_buom  TYPE meins.

  DATA: ls_ordim_o2 TYPE /scwm/ordim_o,
        lt_mat_uom  TYPE /scwm/tt_material_uom.

  DATA: ls_ordim_confirm TYPE /scwm/s_rf_ordim_confirm,
        ls_comb_tanum    TYPE /scwm/s_rf_comb_tanum.

  FIELD-SYMBOLS: <ordim_confirm> TYPE /scwm/s_rf_ordim_confirm,
                 <ls_comb_tanum> TYPE /scwm/s_rf_comb_tanum.

  DATA lv_flg_suom               TYPE xfeld.

  BREAK-POINT ID /scwm/rf_picking.

* get data
  lv_applic   = /scwm/cl_rf_bll_srvc=>get_applic( ).
  lv_pres_prf = /scwm/cl_rf_bll_srvc=>get_pres_prf( ).
  lv_prsn_prf = /scwm/cl_rf_bll_srvc=>get_prsn_prf( ).
  lv_ltrans = /scwm/cl_rf_bll_srvc=>get_ltrans( ).
  lv_step     = /scwm/cl_rf_bll_srvc=>get_step( ).
  lv_line     = /scwm/cl_rf_bll_srvc=>get_line( ).
  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).

* Get verification profile
  lt_valid_prf = /scwm/cl_rf_bll_srvc=>get_valid_prf( ).

  lv_field = /scwm/cl_rf_bll_srvc=>get_cursor_field( ).

  /scwm/cl_rf_bll_srvc=>set_field( lv_field ).


  LOOP AT lt_valid_prf TRANSPORTING NO FIELDS
    WHERE flg_disable NE 'C' AND
          flg_disable NE 'X'.
    EXIT.
  ENDLOOP.
  lv_subrc = sy-subrc.

* Special logic for CW
* If we have open verification fields and cursor is on quantiy and we are
*   CW relevant we jump to CW screen
  IF lv_subrc = 0 AND
     lv_field EQ '/SCWM/S_RF_ORDIM_CONFIRM-NISTA_VERIF' AND
     ordim_confirm-cwrel = abap_true.
    /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_backgr ).
    RETURN.
  ENDIF.

  READ TABLE tt_ordim_confirm INTO ordim_confirm INDEX lv_line.

  IF ordim_confirm-tanum IS NOT INITIAL.
*   Read original WT
    CALL FUNCTION '/SCWM/TO_READ_SINGLE'
      EXPORTING
        iv_lgnum     = ordim_confirm-lgnum
        iv_tanum     = ordim_confirm-tanum
        iv_flglock   = ' '
      IMPORTING
        es_ordim_o   = ls_ordim_o
      EXCEPTIONS
        wrong_input  = 1
        not_found    = 2
        foreign_lock = 3
        error        = 4
        OTHERS       = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  ">> Check partial replenishment
  IF ordim_confirm-vlpla_verif IS NOT INITIAL AND NEW lcl_piplhu_pbo( )->check_partial_replenishment( cs_ordim_confirm = ordim_confirm ).
    ordim_confirm-nista_verif = ordim_confirm-vsola_chr.
    ordim_confirm-pickhu_verif = ordim_confirm-pickhu.
    ordim_confirm-matid_verif = ordim_confirm-matnr.
    MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
  ENDIF.
  "<< Check partial replenishment


  IF lv_step = 'PIMTTO' OR
     lv_step = 'PIBLMT' OR
     lv_step = 'PVMTTO' OR
     lv_step = step_pbv_cpmt OR
     lv_step = step_pbv_blcp OR
     lv_step = 'PVBLMT' OR
     lv_step = 'PIBLCP' OR
     lv_step = 'PICPMT'.

    READ TABLE tt_ordim_confirm INTO ordim_confirm INDEX lv_line.

*   batch field is EAN128 enable, need to update back the
*   standard field
    ordim_confirm-batch = ordim_confirm-rfbatch.
    MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
      TRANSPORTING batch.

    IF lv_step = 'PICPMT' OR lv_step = step_pbv_cpmt OR
        lv_step = step_source_blcp OR lv_step = step_pbv_blcp.
      LOOP AT ordim_confirm-comb_tanums INTO ls_comb_tanum.
        MODIFY tt_ordim_confirm FROM ordim_confirm
          TRANSPORTING batch WHERE tanum = ls_comb_tanum-comb_tanum.
      ENDLOOP.
    ENDIF.


* Read the source storage type
    CALL FUNCTION '/SCWM/T331_READ_SINGLE'
      EXPORTING
        iv_lgnum  = ordim_confirm-lgnum
        iv_lgtyp  = ordim_confirm-vltyp
      IMPORTING
        es_t331   = ls_t331
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*   srcHU population is mandatory, if on storage type level the HU is obligatory
    IF ( lv_step = 'PIBLMT' OR
         lv_step = 'PIBLCP' OR
         lv_step = 'PVBLMT' OR
         lv_step = step_pbv_blcp ) AND
       ls_t331-huobl = abap_true AND
       ordim_confirm-vlenr IS INITIAL AND
       /scwm/cl_rf_bll_srvc=>get_field( ) = '/SCWM/S_RF_ORDIM_CONFIRM-VLENR_VERIF'.

*      Do this check only if no more validation fields are open
*        or if cursor is on VLENR
      IF lv_subrc <> 0 OR
         lv_field = gc_scr_elmnt_vlenr_vrf.

        lv_need_errmsg = abap_true.
*    HU requirement not necessary with some exception code
        LOOP AT ordim_confirm-exc_tab INTO ls_exc_tab
        WHERE ( iprcode = wmegc_iprcode_bidf OR
                iprcode = wmegc_iprcode_bidu ).
          lv_need_errmsg = abap_false.
        ENDLOOP.

        IF lv_need_errmsg IS NOT INITIAL.
          /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_scr_elmnt_vlenr_vrf ).
          CALL METHOD /scwm/cl_rf_bll_srvc=>message
            EXPORTING
              iv_msgid = '/SCWM/RF_EN'
              iv_msgty = 'E'
              iv_msgno = '076'.
        ENDIF.
      ENDIF.
    ENDIF. "lv_step = 'PIBLMT'

    IF lv_step = step_source_blmt OR lv_step = step_source_blcp.
*   Check if the source HU has the correct product/batch info combination.
      IF ordim_confirm-vlenr_verif IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ordim_confirm-vlenr_verif
          IMPORTING
            output = lv_huident.

        CALL FUNCTION '/SCWM/HU_READ'
          EXPORTING
            iv_lgnum   = ordim_confirm-lgnum
            iv_huident = lv_huident
          IMPORTING
            et_huitm   = lt_huitm
          EXCEPTIONS
            not_found  = 1
            error      = 2
            OTHERS     = 3.
        IF sy-subrc = 0.
*         First Check if Material is in the HU
          READ TABLE lt_huitm
              WITH KEY matid = ordim_confirm-matid
              TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL AND
             lt_huitm IS NOT INITIAL.
            "Clear input field otherwise it will NOT be open for input
            CLEAR ordim_confirm-vlenr_verif.
            MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
              TRANSPORTING vlenr_verif.
            MESSAGE e325(/scwm/rf_en) WITH ordim_confirm-matnr lv_huident.
          ENDIF.

*         On batch neutral st. type we may have no batch info at this time
*         Therefore it is enough to check if the HU contains the same material only.
          IF ls_t331-avqbtc = wmegc_batch_neutral AND ordim_confirm-batchid IS INITIAL.
            READ TABLE lt_huitm TRANSPORTING NO FIELDS
              WITH KEY matid   = ordim_confirm-matid.
          ELSE.
            READ TABLE lt_huitm TRANSPORTING NO FIELDS
              WITH KEY matid   = ordim_confirm-matid
                       batchid = ordim_confirm-batchid.
          ENDIF.
          IF sy-subrc <> 0.
            CLEAR ordim_confirm-vlenr_verif.
            MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
              TRANSPORTING vlenr_verif.
            MESSAGE e758(/scwm/rf_en).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*   storage type is batch neutral?
    IF ls_t331-avqbtc = wmegc_batch_neutral AND ls_ordim_o-batchid IS INITIAL.

      IF ordim_confirm-vlenr IS NOT INITIAL AND
         tt_nested_hu IS INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ordim_confirm-vlenr
          IMPORTING
            output = lv_vlenr.

        CLEAR lt_huitm.

*       Read HU and get guid
        CALL FUNCTION '/SCWM/HU_READ'
          EXPORTING
            iv_lgnum   = ordim_confirm-lgnum
            iv_huident = lv_vlenr
          IMPORTING
*           EV_MIX     =
            et_huitm   = lt_huitm
          EXCEPTIONS
            deleted    = 1
            not_found  = 2
            error      = 3
            OTHERS     = 4.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        DELETE lt_huitm WHERE matid <> ordim_confirm-matid.

        LOOP AT lt_huitm INTO ls_huitm.
*         If you move a batch managed product, but the stock is available
*         in a batch neutral storage type, then GUIDs will be different.
          CLEAR: lv_guid_stock,
          ls_stock.

*         product is batch managed and the storage type is batch neutral
          IF ls_huitm-batchid IS NOT INITIAL.

            MOVE-CORRESPONDING ordim_confirm TO ls_stock.
            IF lo_stock_fields IS NOT BOUND.
              CREATE OBJECT lo_stock_fields.
            ENDIF.
            CALL METHOD lo_stock_fields->get_batchid_by_no
              EXPORTING
                iv_matid    = ordim_confirm-matid
                iv_charg    = ordim_confirm-batch
                iv_entitled = ordim_confirm-entitled
              RECEIVING
                ev_batchid  = ls_stock-batchid.

*           CLEAR: ls_stock-batchid,
*           ls_stock-stock_cnt.
            CALL FUNCTION '/SCWM/STOCK_GUID_MAT'
              EXPORTING
                iv_lgnum      = ordim_confirm-lgnum
                is_stock      = ls_stock
              IMPORTING
                ev_guid_stock = lv_guid_stock
              EXCEPTIONS
                error         = 1
                OTHERS        = 2.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            IF lv_guid_stock = ls_huitm-guid_stock0.
              lv_quantity = lv_quantity + ls_huitm-quan - ls_huitm-resq.
            ENDIF.
          ENDIF.
        ENDLOOP.

*       convert UoM of HU into AUoM of WT, if necessary.
        IF lv_quantity IS NOT INITIAL.
          READ TABLE lt_huitm INTO ls_huitm
            WITH KEY guid_stock0 = lv_guid_stock.
          IF ls_huitm-meins <> ordim_confirm-altme.
            TRY.
                CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                  EXPORTING
                    iv_matid     = ls_huitm-matid
                    iv_quan      = lv_quantity
                    iv_unit_from = ls_huitm-meins
                    iv_unit_to   = ordim_confirm-altme
                    iv_batchid   = ls_huitm-batchid
                  IMPORTING
                    ev_quan      = lv_quantity.
              CATCH /scwm/cx_md_interface
                    /scwm/cx_md_batch_required
                    /scwm/cx_md_internal_error
                    /scwm/cx_md_batch_not_required
                    /scwm/cx_md_material_exist.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDTRY.
          ENDIF.
        ENDIF.

        " If quantity is equal to the target quantity of first WT in combination group
        " Undo-combine for the first WT.
        IF lv_quantity IS NOT INITIAL.
          IF ( lv_step = step_source_blcp OR
               lv_step = step_pick_cpmt ) AND
              lv_quantity = ordim_confirm-vsola.
            ordim_confirm-uncomb = abap_true.
            MODIFY tt_ordim_confirm FROM ordim_confirm
                  TRANSPORTING uncomb WHERE tanum = ordim_confirm-tanum.

            IF lv_step = step_pick_cpmt.
              /scwm/cl_rf_bll_srvc=>set_prmod(
                            /scwm/cl_rf_bll_srvc=>c_prmod_background ).
              /scwm/cl_rf_bll_srvc=>set_fcode( fcode_go_to_pick_mtto ).
              RETURN.
            ELSEIF lv_step = step_source_blcp.
              /scwm/cl_rf_bll_srvc=>set_prmod(
                            /scwm/cl_rf_bll_srvc=>c_prmod_background ).
              /scwm/cl_rf_bll_srvc=>set_fcode( fcode_go_to_pick_blmt ).
              RETURN.
            ENDIF.
*      HU contains less qty than WT
*      then provide a partial confirmation.
          ELSEIF lv_quantity < ordim_confirm-vsola.
            ordim_confirm-vsola = lv_quantity.
            ordim_confirm-parti = gc_xfeld.
            ordim_confirm-nista = lv_quantity.
            ordim_confirm-uncomb = abap_true.

            CALL FUNCTION 'CONVERSION_EXIT_QNTY1_INPUT'
              EXPORTING
                input  = ordim_confirm-vsola
              IMPORTING
                output = ordim_confirm-vsola_chr.

            MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
            IF lv_step = step_pick_cpmt.
              /scwm/cl_rf_bll_srvc=>set_prmod(
                            /scwm/cl_rf_bll_srvc=>c_prmod_background ).
              /scwm/cl_rf_bll_srvc=>set_fcode( fcode_go_to_pick_mtto ).
              RETURN.
            ELSEIF lv_step = step_source_blcp.
              /scwm/cl_rf_bll_srvc=>set_prmod(
                            /scwm/cl_rf_bll_srvc=>c_prmod_background ).
              /scwm/cl_rf_bll_srvc=>set_fcode( fcode_go_to_pick_blmt ).
              RETURN.
            ENDIF.
          ELSEIF ( lv_step = step_source_blcp OR
               lv_step = step_pick_cpmt ) AND
              lv_quantity < ordim_confirm-combqty.
            " distribute partial confirm in combination group
            lv_quantity_dis = lv_quantity.
            ordim_confirm-combqty = lv_quantity.
            ordim_confirm-combqty_chr = ordim_confirm-combqty.
            LOOP AT ordim_confirm-comb_tanums ASSIGNING <ls_comb_tanum>.
              READ TABLE tt_ordim_confirm ASSIGNING <ordim_confirm> WITH KEY tanum = <ls_comb_tanum>-comb_tanum.
              IF sy-subrc = 0.
                IF lv_quantity_dis > <ordim_confirm>-vsola.
                  <ordim_confirm>-nista = <ordim_confirm>-vsola.
                  lv_quantity_dis = lv_quantity_dis - <ordim_confirm>-vsola.
                ELSEIF lv_quantity_dis = <ordim_confirm>-vsola.
                  <ordim_confirm>-nista = <ordim_confirm>-vsola.
                  lv_quantity_dis = lv_quantity_dis - <ordim_confirm>-vsola.
                ELSEIF lv_quantity_dis > 0.
                  <ordim_confirm>-parti = gc_xfeld.
                  <ordim_confirm>-vsola = lv_quantity_dis.
                  <ordim_confirm>-vsola_chr = lv_quantity_dis.
                  <ordim_confirm>-nista = <ordim_confirm>-vsola.
                  " update ls_comb_tanum, when quantity changed
                  <ls_comb_tanum>-vsola_chr = <ordim_confirm>-vsola.
                  lv_quantity_dis = lv_quantity_dis - <ordim_confirm>-vsola.
                ELSE.
                  DELETE ordim_confirm-comb_tanums WHERE comb_tanum >= <ordim_confirm>-tanum.
                  DESCRIBE TABLE ordim_confirm-comb_tanums LINES ordim_confirm-samsorce.
                  CLEAR <ordim_confirm>-comb_tanums.
                  CLEAR <ordim_confirm>-samsorce.
                  CLEAR <ordim_confirm>-batch.
                ENDIF.
                IF <ordim_confirm>-tanum = ordim_confirm-tanum.
                  ordim_confirm-parti = <ordim_confirm>-parti.
                  ordim_confirm-vsola = <ordim_confirm>-vsola.
                  ordim_confirm-vsola_chr = <ordim_confirm>-vsola_chr.
                  ordim_confirm-nista = <ordim_confirm>-nista.
                ENDIF.
              ENDIF.
            ENDLOOP.

            MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


    IF ls_t331-avqlvl = '1'.
      CLEAR ls_ordim_o.
      MOVE-CORRESPONDING ordim_confirm TO ls_ordim_o.

      DATA: ls_pickhu      TYPE /scwm/s_huident,
            ls_rf_pick_hus TYPE /scwm/s_rf_pick_hus,
            lt_pickhu      TYPE /scwm/tt_huident.
      CLEAR lt_pickhu.
      LOOP AT t_rf_pick_hus INTO ls_rf_pick_hus.
        ls_pickhu-lgnum = ordim_confirm-lgnum.
        ls_pickhu-huident = ls_rf_pick_hus-huident.
        APPEND ls_pickhu TO lt_pickhu.
      ENDLOOP.

*     Read warehouse process type
      CALL FUNCTION '/SCWM/T333_READ_SINGLE'
        EXPORTING
          iv_lgnum    = ordim_confirm-lgnum
          iv_procty   = ordim_confirm-procty
        IMPORTING
          es_t333     = ls_t333
        EXCEPTIONS
          not_found   = 1
          wrong_input = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*     retrieve batch info
      IF ordim_confirm-batch_req = 'X' AND
         ordim_confirm-batchid IS INITIAL AND
         ordim_confirm-batch IS NOT INITIAL.

        IF lo_stock_fields IS NOT BOUND.
          CREATE OBJECT lo_stock_fields.
        ENDIF.
        CALL METHOD lo_stock_fields->get_batchid_by_no
          EXPORTING
            iv_matid    = ordim_confirm-matid
            iv_charg    = ordim_confirm-batch
            iv_entitled = ordim_confirm-entitled
          RECEIVING
            ev_batchid  = ls_ordim_o-batchid.
      ENDIF.
      " in case orverwriting huent determined in combined picking partial confirm
      IF ordim_confirm-huent IS INITIAL.
        CALL FUNCTION '/SCWM/HUENT_DET'
          EXPORTING
            is_ordim_o       = ls_ordim_o
            iv_srchu_changed = 'X'
            iv_rsrc          = resource-rsrc
            it_pickhu        = lt_pickhu
            iv_desc_sort     = 'X'
          IMPORTING
            ev_huent         = lv_huent
            ev_nlenr         = lv_nlenr
          EXCEPTIONS
            wrong_data       = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

*     In case of difference on diff account the previously set pickHU and HU WD
*     should be kept, because we can move the srcHU.
        IF ordim_confirm-difty <> gc_difty_account.
*       if HUWD changed manually(set or removed) then don't overwrite it
          IF gv_manual_no_huwd IS INITIAL.
            ordim_confirm-huent  = lv_huent.
          ENDIF.
        ENDIF.
        IF ls_t331-pikta = wmegc_pikta_src_hu OR
           ( ls_t331-pikta = wmegc_pikta_procty AND ls_t333-pikta = wmegc_pikta_src_hu ).
          READ TABLE ordim_confirm-exc_tab TRANSPORTING NO FIELDS
            WITH KEY iprcode = wmegc_iprcode_chhu.
*         check if CHHU exception is used then no need to change pickhu
          IF sy-subrc IS NOT INITIAL.
            ordim_confirm-pickhu = lv_nlenr.
          ENDIF.
        ENDIF.
      ENDIF.
      MODIFY  tt_ordim_confirm FROM ordim_confirm TRANSPORTING pickhu huent WHERE tanum = ordim_confirm-tanum.
    ENDIF.

    BREAK-POINT ID /scwm/suom.

    IF lv_fcode = 'ENTER' AND
       ordim_confirm-altme IS NOT INITIAL.
*     Check if entered ALTME is really stock-specific
      IF ordim_confirm-difty IS NOT INITIAL.
*       Read original WT from database and check if ALTME is changed
        CALL FUNCTION '/SCWM/TO_READ_SINGLE'
          EXPORTING
            iv_lgnum         = ordim_confirm-lgnum
            iv_tanum         = ordim_confirm-tanum
            iv_flglock       = ' '
            iv_add_to_memory = ' '
            iv_read_from_db  = 'X'
          IMPORTING
            es_ordim_o       = ls_ordim_o2
          EXCEPTIONS
            wrong_input      = 1
            not_found        = 2
            foreign_lock     = 3
            error            = 4
            OTHERS           = 5.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF ls_ordim_o2-altme <> ordim_confirm-altme.
          CALL FUNCTION '/SCWM/SUOM_CHECK'
            EXPORTING
              iv_lgnum    = ordim_confirm-lgnum
              iv_matid    = ordim_confirm-matid
              iv_uom      = ordim_confirm-altme
            IMPORTING
              ev_flg_suom = lv_flg_suom.

          IF lv_flg_suom IS NOT INITIAL.
*           The SUOM check has meaning, when the new UOM in SUOM by difference
            CALL FUNCTION '/SCWM/MATERIAL_SUOM_RELEVANT'
              EXPORTING
                iv_lgnum    = ordim_confirm-lgnum
                iv_matid    = ordim_confirm-matid
                iv_entitled = ordim_confirm-entitled
              IMPORTING
                ev_buom     = lv_buom
                et_mat_uom  = lt_mat_uom.
*           The code runs also here when the unit of measurement was changed by exception
*           For example DIFS was used by confirmation the WT to the resource by picking
            IF lines( lt_mat_uom ) > 1. "Base unit of measurement is always returned
*             At least 1 UOM of the product defined as SUOM
              READ TABLE lt_mat_uom TRANSPORTING NO FIELDS
                WITH KEY meinh = ordim_confirm-altme.
              IF sy-subrc <> 0.
                CALL METHOD /scwm/cl_rf_bll_srvc=>set_field
                  EXPORTING
                    iv_field = '/SCWM/S_RF_ORDIM_CONFIRM-ALTME'.
                MESSAGE e870(/scwm/l3) WITH ordim_confirm-altme.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*   Check ALTME if changed by function code.
    IF lv_field EQ '/SCWM/S_RF_ORDIM_CONFIRM-ALTME'.
      IF ordim_confirm-altme IS INITIAL.
        MESSAGE e600(/scwm/rf_en).
      ELSE.
*       Switch off input and calculate quantities
        /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                               '/SCWM/S_RF_ORDIM_CONFIRM-ALTME' ).

        IF ordim_confirm-nista_verif IS NOT INITIAL.
          ordim_confirm-nista = ordim_confirm-nista_verif.

*         Calculate from-quantity in new AUoM
          TRY.
              CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                EXPORTING
                  iv_matid     = ordim_confirm-matid
                  iv_quan      = ordim_confirm-vsolm
                  iv_unit_from = ordim_confirm-meins
                  iv_unit_to   = ordim_confirm-altme
                  iv_batchid   = ordim_confirm-batchid
                IMPORTING
                  ev_quan      = lv_vsola.
            CATCH /scwm/cx_md.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDTRY.

          ordim_confirm-ndifa = lv_vsola -
                                   ordim_confirm-nista.

          MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
        ELSE.
          CALL METHOD /scwm/cl_rf_bll_srvc=>set_field
            EXPORTING
              iv_field = '/SCWM/S_RF_ORDIM_CONFIRM-NISTA_VERIF'.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.


* New logic because of quick ENTER.
* If we have open verification fields we display the screen again.
  IF ( lv_step = step_source_blcp OR lv_step = step_pick_cpmt ) AND
  ordim_confirm-uncomb = abap_true.
* for combined picking partial confirmation, process mode stays background.
    RETURN.
  ELSEIF lv_subrc = 0.
    IF ( lv_step = 'PIPLMT' OR
         lv_step = 'PIPLHU' ) AND
         lv_field = '/SCWM/S_RF_ORDIM_CONFIRM-NLPLA' AND
         ls_ordim_o-nlpla IS INITIAL.
      /scwm/cl_rf_bll_srvc=>set_field( '' ).
    ENDIF.
    /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_foregr ).
    RETURN.
  ENDIF.

  IF gv_huobl = wmegc_huobl_all.

    IF lv_step = step_source_mtto OR
       lv_step = step_pick_cpmt OR
       lv_step = step_pbv_cpmt OR
       lv_step = 'PVMTTO'.

*     initializing data
      lv_close_hu_verif = gc_xfeld.
      CLEAR ls_valid_prf.

      LOOP AT lt_valid_prf INTO ls_valid_prf
           WHERE param_name = 'TT_ORDIM_CONFIRM' AND
                 valid_obj <> 'VLENR' AND
                 flg_verif = gc_xfeld.
        IF ls_valid_prf-flg_disable IS INITIAL.
          CLEAR lv_close_hu_verif.
          EXIT.
        ENDIF.
      ENDLOOP.

      CLEAR ls_valid_prf.
      READ TABLE lt_valid_prf INTO ls_valid_prf
           WITH KEY param_name = 'TT_ORDIM_CONFIRM'
                     valid_obj = 'VLENR'.

      IF NOT ls_valid_prf IS INITIAL AND
         NOT lv_close_hu_verif IS INITIAL AND
         ordim_confirm-vlenr IS INITIAL.
*        Verification profile is open for VLENR.
*        All other verifications are closed.
*        HU is not must but possible.
*        HU is empty in original TO.

        IF NOT ls_valid_prf-flg_verif IS INITIAL.

          /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                      gc_scr_elmnt_vlenr_vrf ).

        ENDIF.
      ENDIF.

    ELSEIF lv_step = step_dest_plhu OR
           lv_step = step_dest_plmt OR
           lv_step = 'PVPLHU' OR
           lv_step = 'PVPLMT'.

*     initializing data
      lv_close_hu_verif = gc_xfeld.
      CLEAR ls_valid_prf.

      LOOP AT lt_valid_prf INTO ls_valid_prf
           WHERE param_name = 'TT_ORDIM_CONFIRM' AND
                 valid_obj <> 'PICKHU' AND
                 flg_verif = gc_xfeld.
        IF ls_valid_prf-flg_disable IS INITIAL.
          CLEAR lv_close_hu_verif.
          EXIT.
        ENDIF.
      ENDLOOP.

      CLEAR ls_valid_prf.
      READ TABLE lt_valid_prf INTO ls_valid_prf
           WITH KEY param_name = 'TT_ORDIM_CONFIRM'
                     valid_obj = 'PICKHU'.

      IF NOT ls_valid_prf IS INITIAL AND
         NOT lv_close_hu_verif IS INITIAL AND
         ordim_confirm-pickhu IS INITIAL.
*     Verification profile is open for VLENR.
*     All other verifications are closed.
*     HU is not must but possible.
*     HU is empty in original TO.

        IF NOT ls_valid_prf-flg_verif IS INITIAL.

          /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                      gc_scr_elmnt_pickhu_vrf ).

        ENDIF.
      ENDIF.

    ENDIF. "lv_step

  ENDIF. "gv_huobl

* dd picking with quick-enter
  IF who-type = wmegc_wcr_dd.
    BREAK-POINT ID /scwm/dd_picking.
    DATA(lo_dd_picking) = /scwm/cl_dd_picking=>get_instance( ).
*check if logpos_ext-setting is "hidden"
    lo_dd_picking->get_dd_settings(
          EXPORTING
             is_who = who
          IMPORTING
             es_twho_dd = DATA(ls_twho_dd) ).
    IF ls_twho_dd-hide_logpos_ext IS INITIAL.
      "If Bin Denial Full was entered before, the following coding must not be performed
      IF ordim_confirm-bind IS INITIAL OR
         ordim_confirm-nista > 0.
        IF ordim_confirm-dlogpos_ext_wt IS INITIAL AND
           ordim_confirm-logpos_ext IS INITIAL AND
           lv_field <> '/SCWM/S_RF_ORDIM_CONFIRM-LOGPOS_EXT'.
          "we stay on the picking screen so that user can enter logpos_ext
          /scwm/cl_rf_bll_srvc=>set_prmod(
                        /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).
        ELSE.
          "check if user entered a logpos_ext and if logpos_ext is valid
          CALL METHOD lo_dd_picking->validate_logpos_ext
            EXPORTING
              it_pick_hus      = t_rf_pick_hus
              is_who           = who
              is_resource      = resource
            CHANGING
              cs_ordim_confirm = ordim_confirm
              ct_ordim_confirm = tt_ordim_confirm.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
