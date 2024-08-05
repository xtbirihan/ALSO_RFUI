CLASS zcl_rf_putaway_with_cart DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-DATA ss_resource TYPE /scwm/rsrc READ-ONLY.

    CLASS-METHODS get_instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_rf_putaway_with_cart.
    CLASS-METHODS validate_cart_hu IMPORTING iv_cart_hu        TYPE /scwm/de_huident
                                   RETURNING VALUE(rs_message) TYPE bapiret2.
    CLASS-METHODS assign_cart_to_resource IMPORTING iv_cart_hu        TYPE /scwm/de_huident
                                          RETURNING VALUE(rs_message) TYPE bapiret2.
    CLASS-METHODS validate_parking_lot IMPORTING iv_parking_lot    TYPE /scwm/ltap_nlpla
                                       RETURNING VALUE(rs_message) TYPE bapiret2.
    CLASS-METHODS assign_cart_to_parking_lot IMPORTING iv_cart_hu        TYPE /scwm/de_huident
                                                       iv_parking_lot    TYPE /scwm/ltap_nlpla
                                             RETURNING VALUE(rs_message) TYPE bapiret2.
    CLASS-METHODS show_next_cart_position CHANGING  cs_cart_postion             TYPE zstr_rf_processes_putaway_cart.
    CLASS-METHODS finish_cart_positon IMPORTING is_cart_postion   TYPE zstr_rf_processes_putaway_cart
                                      RETURNING VALUE(rs_message) TYPE bapiret2.
    CLASS-METHODS cart_is_empty RETURNING VALUE(rv_cart_is_empty) TYPE abap_bool.
    CLASS-METHODS propose_storage_bin CHANGING  cs_cart_postion   TYPE zstr_rf_processes_putaway_cart
                                      RETURNING VALUE(rs_message) TYPE bapiret2.
    CLASS-METHODS change_screen_layout IMPORTING is_cart_postion   TYPE zstr_rf_processes_putaway_cart.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA sv_current_cart_position TYPE int8.
    CLASS-DATA sv_parking_lot_storage_type TYPE /scwm/lgtyp.

    CLASS-DATA ss_warehouse_task TYPE /scwm/ordim_o.
    CLASS-DATA ss_warehouse_parameter     TYPE /scwm/t340d.
    CLASS-DATA ss_warehouse_process_type     TYPE /scwm/t333.

    CLASS-DATA st_hus_on_cart TYPE /scwm/tt_huhdr_int.
    CLASS-DATA st_hu_items TYPE /scwm/tt_huitm_int.

    CLASS-DATA so_singleton_instance TYPE REF TO zcl_rf_putaway_with_cart.
    CLASS-DATA so_application_log TYPE REF TO /scwm/cl_log.

    CLASS-METHODS read_hus_from_cart IMPORTING iv_chart_hu TYPE /scwm/de_huident.
    CLASS-METHODS read_warehouse_task IMPORTING iv_handling_unit TYPE /scwm/de_huident.
    CLASS-METHODS read_material IMPORTING iv_matid        TYPE /scwm/de_matid
                                CHANGING  cs_cart_postion TYPE zstr_rf_processes_putaway_cart.
    CLASS-METHODS confirm_warehouse_task IMPORTING iv_dest_bin       TYPE /scwm/ltap_nlpla OPTIONAL
                                                   iv_exception      TYPE /scwm/de_exccode OPTIONAL
                                         RETURNING VALUE(rs_message) TYPE bapiret2.
    CLASS-METHODS get_global_parameter RETURNING VALUE(rv_parameter_loaded) TYPE abap_bool.

    CLASS-METHODS move_handlung_unit IMPORTING iv_cart_hu        TYPE /scwm/de_huident
                                               iv_parking_bin    TYPE /scwm/ltap_nlpla   OPTIONAL
                                     RETURNING VALUE(rs_message) TYPE bapiret2.
    CLASS-METHODS set_counter_text IMPORTING iv_matid        TYPE /scwm/de_matid
                                   CHANGING  cs_cart_postion TYPE zstr_rf_processes_putaway_cart.
ENDCLASS.



CLASS ZCL_RF_PUTAWAY_WITH_CART IMPLEMENTATION.


  METHOD assign_cart_to_parking_lot.
**********************************************************************
*& Key           : AD-230828
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Assign cart to parking lot (special storage bin) to finish clearing later.
*& This helps to keep track of the whereabouts of the carts.
**********************************************************************
    " There are no more HUs on the cart left. So the cart does not have to be parked.
    IF cart_is_empty(  ).
      RETURN.
    ENDIF.

    rs_message =  move_handlung_unit( iv_cart_hu = iv_cart_hu
                                      iv_parking_bin = iv_parking_lot ).
  ENDMETHOD.


  METHOD assign_cart_to_resource.
**********************************************************************
*& Key           : AD-230728
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Assign Cart Handling Unit to Resource via Warehouse Task
*&
**********************************************************************
    " Cart is already assigned to current resource.
    " No assigning necessary.
    IF st_hus_on_cart[ 1 ]-rsrc = ss_resource-rsrc.
      RETURN.
    ENDIF.

    " Cart ID can't be assigned to other resource
    IF st_hus_on_cart[ 1 ]-rsrc IS NOT INITIAL.
      so_application_log->add_message( ip_msgid = 'ZMC_RFUI'
                                       ip_msgno = '021' " Cart ID &1 already assigned to other Resource &2
                                       ip_msgty = 'E'
                                       ip_msgv1 = |{ iv_cart_hu ALPHA = OUT }|
                                       ip_msgv2 = CONV #( st_hus_on_cart[ 1 ]-rsrc ) ).
      so_application_log->get_last_message_complete( IMPORTING ev_msg = rs_message ).
      RETURN.
    ENDIF.

    rs_message =  move_handlung_unit( iv_cart_hu = iv_cart_hu ).
  ENDMETHOD.


  METHOD cart_is_empty.
**********************************************************************
*& Key           : AD-230828
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Determines, whether there are still postions left on the cart by checking the contente of the iTab.
*&
**********************************************************************
    IF lines( st_hus_on_cart ) = 0.
      rv_cart_is_empty = abap_true.
    ELSE.
      rv_cart_is_empty = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD change_screen_layout.
**********************************************************************
*& Key           : AD-230907
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Changes the screen layout based on specific conditions
*&
**********************************************************************
    IF is_cart_postion-dest_storage_type(3) = 'KDK'.
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( iv_screlm_name = 'ZSTR_RF_PROCESSES_PUTAWAY_CART-ITEM_AQUANTITY' ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( iv_screlm_name = 'LBL_EACH' ).
    ELSE.
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( iv_screlm_name = 'ZSTR_RF_PROCESSES_PUTAWAY_CART-ITEM_AQUANTITY' ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( iv_screlm_name = 'LBL_EACH' ).
    ENDIF.

    IF is_cart_postion-number_of_totes = 0.
      " Hide the fields for number_of_totes and scanned_totes if the number of totes is 0
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( iv_screlm_name = 'ZSTR_RF_PROCESSES_PUTAWAY_CART-NUMBER_OF_TOTES' ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( iv_screlm_name = 'ZSTR_RF_PROCESSES_PUTAWAY_CART-SCANNED_TOTES' ).
    ELSE.
      " Show the fields for number_of_totes and scanned_totes if the number of totes > 0
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( iv_screlm_name = 'ZSTR_RF_PROCESSES_PUTAWAY_CART-NUMBER_OF_TOTES' ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( iv_screlm_name = 'ZSTR_RF_PROCESSES_PUTAWAY_CART-SCANNED_TOTES' ).
    ENDIF.
  ENDMETHOD.


  METHOD confirm_warehouse_task.
**********************************************************************
*& Key           : AD-230807
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Confirms the Warehouse Task to clear the Cart
*&
**********************************************************************
    DATA lt_bapiret TYPE bapirettab.
    DATA lv_severity TYPE bapi_mtype.
    DATA lt_wt_to_confirm TYPE /scwm/to_conf_tt.
    DATA lt_exception_codes TYPE /scwm/tt_conf_exc.
    DATA ls_execpetion_code TYPE /scwm/s_conf_exc.


    " If a Exception was triggered provide a additoinal parameter for the Confirmation
    " so that the Destination Bin can be changed.
    IF iv_exception IS SUPPLIED.
      ls_execpetion_code-tanum = ss_warehouse_task-tanum.
      ls_execpetion_code-exccode = iv_exception.
      ls_execpetion_code-buscon = 'TPT'.

      IF ss_warehouse_task-flghuto = 'X'.
        ls_execpetion_code-exec_step = '04'.
      ELSE.
        ls_execpetion_code-exec_step = '06'.
      ENDIF.

      APPEND ls_execpetion_code TO lt_exception_codes.

      lt_wt_to_confirm = VALUE #( ( tanum = ss_warehouse_task-tanum
                                    squit = 'X'
                                    nlpla = iv_dest_bin ) ).

      CALL FUNCTION '/SCWM/TO_CONFIRM'
        EXPORTING
          iv_lgnum    = ss_resource-lgnum
          it_conf     = lt_wt_to_confirm
          it_conf_exc = lt_exception_codes
        IMPORTING
          et_bapiret  = lt_bapiret
          ev_severity = lv_severity.
    ELSE.
      lt_wt_to_confirm = VALUE #( ( tanum = ss_warehouse_task-tanum
                                    squit = 'X' ) ).


      CALL FUNCTION '/SCWM/TO_CONFIRM'
        EXPORTING
          iv_lgnum    = ss_resource-lgnum
          it_conf     = lt_wt_to_confirm
        IMPORTING
          et_bapiret  = lt_bapiret
          ev_severity = lv_severity.
    ENDIF.

    IF lv_severity CA 'EA'.
      so_application_log->add_message( ip_msgid = 'ZMC_RFUI'
                                       ip_msgno = '023' " Error while confirming WT &1
                                       ip_msgty = 'E'
                                       ip_msgv1 =  CONV #( ss_warehouse_task-tanum ) ).
      so_application_log->get_last_message_complete( IMPORTING ev_msg = rs_message ).
    ELSE.
      CLEAR rs_message.
    ENDIF.
  ENDMETHOD.


  METHOD finish_cart_positon.
**********************************************************************
*& Key           : AD-230810
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Confirms the Warehouse Task for the current Cart Position
*&
**********************************************************************
    read_warehouse_task( is_cart_postion-source_hu  ).

    IF is_cart_postion-execption IS NOT INITIAL.
      rs_message = confirm_warehouse_task( iv_dest_bin = is_cart_postion-dest_storage_bin
                                           iv_exception = is_cart_postion-execption  ).
    ELSE.
      rs_message = confirm_warehouse_task(  ).
    ENDIF.

    IF rs_message IS INITIAL.
      " After succesfully confirming the Warehouse Taks of the current cart position, delete position from the iTab.
      " Caution: We have to decrese the counter for the cart position, to compensate the now missing line in the iTab.
      DELETE st_hus_on_cart INDEX sv_current_cart_position.
      IF sv_current_cart_position > 0.
        sv_current_cart_position = sv_current_cart_position - 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_global_parameter.
**********************************************************************
*& Key           : AD-230825
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Determines the required parameter for the current warehouse from customizing
*&
**********************************************************************
    " Get warehouse number for user(ressource)
    CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
      EXPORTING
        iv_uname = sy-uname
      CHANGING
        cs_rsrc  = ss_resource.

    IF sy-subrc <> 0.
      CLEAR ss_resource.
      rv_parameter_loaded = abap_false.
      RETURN.
    ENDIF.

    " Read Warehouse Default Settings
    "(To get the Distributive Putaway Warehouse Type)
    CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
      EXPORTING
        iv_lgnum  = ss_resource-lgnum
      IMPORTING
        es_t340d  = ss_warehouse_parameter
      EXCEPTIONS
        not_found = 1
        OTHERS    = 99.

    IF sy-subrc <> 0.
      CLEAR ss_warehouse_parameter.
      rv_parameter_loaded = abap_false.
      RETURN.
    ENDIF.

    " Read Settings for Distributive Putaway Warehouse Type
    CALL FUNCTION '/SCWM/T333_READ_SINGLE'
      EXPORTING
        iv_lgnum    = ss_resource-lgnum
        iv_procty   = ss_warehouse_parameter-pt_invpick
      IMPORTING
        es_t333     = ss_warehouse_process_type
      EXCEPTIONS
        not_found   = 1
        wrong_input = 2
        OTHERS      = 99.

    IF sy-subrc <> 0.
      CLEAR ss_warehouse_process_type.
      rv_parameter_loaded = abap_false.
      RETURN.
    ENDIF.

    " Get storage type for parking lot verification
    zcl_param=>get_parameter(
        EXPORTING
          iv_lgnum = ss_warehouse_parameter-lgnum
          iv_process = zif_param_const=>c_zrfui_0005
          iv_parameter = zif_param_const=>c_parking_storage_type
       IMPORTING
        ev_constant  =  DATA(lv_parameter) ).

    IF lv_parameter IS INITIAL.
      rv_parameter_loaded = abap_false.
      RETURN.
    ENDIF.

    sv_parking_lot_storage_type = CONV #( lv_parameter ).


    rv_parameter_loaded = abap_true.
  ENDMETHOD.


  METHOD get_instance.
**********************************************************************
*& Key           : AD-230727
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Returns the singleton instance of this class
*& The RF transaction is split over several function modules. In order to
*& pass the application data bewtween the function modules wo us a static class.
**********************************************************************
    IF so_singleton_instance IS NOT BOUND.
      " Create singleton instance
      so_singleton_instance = NEW #( ).

      " Create application log instance
      /scwm/cl_log=>get_instance( IMPORTING eo_instance = so_application_log ).

      IF get_global_parameter( ) = abap_false.
        FREE so_singleton_instance.
      ENDIF.
    ENDIF.

    " Return singleton instance
    ro_instance = so_singleton_instance.
  ENDMETHOD.


  METHOD move_handlung_unit.
**********************************************************************
*& Key           : AD-230728
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Creates, and immediately  confirms a warehouse task to move the hadling unit
*& to another storage bin or to a rescource.
**********************************************************************
    DATA lt_to_create TYPE /scwm/tt_to_crea_hu.
    DATA lv_tanum TYPE /scwm/tanum.
    DATA lt_bapiret   TYPE bapirettab.
    DATA lt_ltap_vb   TYPE /scwm/tt_ltap_vb.
    DATA lv_severity  TYPE bapi_mtype.

    " Check, Whether the Distributive Putaway Warehouse Task is configured properly
    IF ss_warehouse_process_type-trart      <> wmegc_trart_put OR
       ss_warehouse_process_type-vltyp      IS NOT INITIAL     OR
       ss_warehouse_process_type-vlpla      IS NOT INITIAL     OR
       ss_warehouse_process_type-nltyp      IS NOT INITIAL     OR
       ss_warehouse_process_type-nlpla      IS NOT INITIAL     OR
       ss_warehouse_process_type-squit      IS INITIAL     OR
       ss_warehouse_process_type-tbobl      IS NOT INITIAL     OR
       ss_warehouse_process_type-norou      <>  wmegc_norou_yes.
      so_application_log->add_message( ip_msgid = '/SCWM/RF_DE'
                                       ip_msgno = '051' " Warehouse Process Type for distributive Putaway not maintained
                                       ip_msgty = 'E' ).
      so_application_log->get_last_message_complete( IMPORTING ev_msg = rs_message ).
    ENDIF.

    " Create HU-TO and immediately confirm
    " (This is used to move(assign) the Handlung Unit to the Resource/Parking bin
    IF iv_parking_bin IS SUPPLIED.
      lt_to_create = VALUE #( ( huident = iv_cart_hu
                        procty = ss_warehouse_parameter-pt_invpick
                        squit = 'X'
                        nlpla = iv_parking_bin
                    ) ).
    ELSE.
      lt_to_create = VALUE #( ( huident = iv_cart_hu
                                procty = ss_warehouse_parameter-pt_invpick
                                squit = 'X'
                                drsrc = ss_resource-rsrc
                            ) ).
    ENDIF.

    CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'
      EXPORTING
        iv_lgnum         = ss_resource-lgnum
        it_create_hu     = lt_to_create
        iv_commit_work   = ' '
        iv_wtcode        = wmegc_wtcode_rf_pd_hu
        iv_processor_det = 'X'
      IMPORTING
        ev_tanum         = lv_tanum
        et_ltap_vb       = lt_ltap_vb
        et_bapiret       = lt_bapiret
        ev_severity      = lv_severity.

    IF lv_severity CA 'EA'.
      ROLLBACK WORK.
      so_application_log->add_message( ip_msgid = 'ZMC_RFUI'
                                       ip_msgno = '022' " Error while assigning Cart ID
                                       ip_msgty = 'E' ).
      so_application_log->get_last_message_complete( IMPORTING ev_msg = rs_message ).
      RETURN.
    ENDIF.

    COMMIT WORK AND WAIT.

    " Call resource execution control
    CALL FUNCTION '/SCWM/REC_WT_CHANGE'
      EXPORTING
        it_ltap_vb        = lt_ltap_vb
      EXCEPTIONS
        no_free_wt_in_who = 1
        internal_error    = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      so_application_log->add_message( ip_msgid = 'ZMC_RFUI'
                                       ip_msgno = '022' " Error while assigning Cart ID
                                       ip_msgty = 'E' ).
      so_application_log->get_last_message_complete( IMPORTING ev_msg = rs_message ).
      RETURN.
    ENDIF.

    CALL METHOD /scwm/cl_tm=>cleanup( ).

    CLEAR  rs_message.
  ENDMETHOD.


  METHOD propose_storage_bin.
**********************************************************************
*& Key           : AD-230825
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Detemrines an alternative storage bin using the SAP default logic.
*&
**********************************************************************
    DATA ls_ptwy  TYPE /scwm/s_rf_ptwy.
    DATA ls_admin  TYPE /scwm/s_rf_admin.
    DATA lt_ptwy  TYPE /scwm/tt_rf_ptwy.
    DATA lt_lgpla  TYPE /scwm/tt_lgpla.

* changing cs_ptwy  type /scwm/s_rf_ptwy
*  cs_admin  type /scwm/s_rf_admin
*  ct_ptwy  type /scwm/tt_rf_ptwy
*  ct_lgpla  type /scwm/tt_lgpla

    ls_admin-lgnum = cs_cart_postion-lgnum.
    MOVE-CORRESPONDING ss_warehouse_task TO ls_ptwy.
    ls_ptwy-nista = ss_warehouse_task-vsolm.
    ls_ptwy-nlpla = cs_cart_postion-org_dest_storage_bin.

    CALL FUNCTION '/SCWM/RF_PT_MAT_DEST_PROP_PBO'
      CHANGING
        cs_ptwy  = ls_ptwy
        cs_admin = ls_admin
        ct_ptwy  = lt_ptwy
        ct_lgpla = lt_lgpla.

    cs_cart_postion-dest_storage_bin = ls_ptwy-nlpla.
  ENDMETHOD.


  METHOD read_hus_from_cart.
**********************************************************************
*& Key           : AD-230728
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Reads the Handling Units for the Cart
*&
**********************************************************************
    DATA lt_huhdr TYPE /scwm/tt_huhdr_int.
    DATA lv_severity TYPE bapi_mtype.
    DATA lt_hutree  TYPE /scwm/tt_hutree.


    REFRESH st_hus_on_cart.
    REFRESH st_hu_items.

    DATA(lt_huident) = VALUE  rseloption( ( sign = 'I'
                                            option = 'EQ'
                                            low = iv_chart_hu  ) ).

    CALL FUNCTION '/SCWM/HU_SELECT_GEN'
      EXPORTING
        iv_lgnum      = ss_resource-lgnum
        iv_hierarchy  = abap_true
        ir_huident    = lt_huident
      IMPORTING
        et_huhdr      = lt_huhdr
        et_huitm      = st_hu_items
        e_rc_severity = lv_severity
        et_hutree     = lt_hutree
      EXCEPTIONS
        error         = 1
        not_possible  = 2
        wrong_input   = 3
        error_message = 1
        OTHERS        = 99.

    IF lv_severity CA 'EA'.
      REFRESH st_hus_on_cart.
      REFRESH st_hu_items.
    ENDIF.

    " The Function Module will return an iTab, where sub handling units are placed at the end of the list, regardless of there dependency.
    " So we're going to sort the iTab ourself and place the sub HU directly behind ther parent HU.
    " When the user skips through the Cart Positions, the Parent and child HUs will be displayed together.
    LOOP AT lt_huhdr ASSIGNING FIELD-SYMBOL(<ls_huhdr>).
      " If the HU was already added to the iTab we skip it. We don't want to have duplicate entries.
      IF line_exists( st_hus_on_cart[ huident = <ls_huhdr>-huident ] ).
        CONTINUE.
      ENDIF.

      CASE <ls_huhdr>-bottom.
        WHEN abap_true. " HU has no child HUs and can be added directly.
          read_warehouse_task( <ls_huhdr>-huident ).
          IF ss_warehouse_task IS NOT INITIAL.
            st_hus_on_cart = VALUE #( BASE st_hus_on_cart ( <ls_huhdr> ) ).
          ENDIF.
        WHEN abap_false. " HU has child HUs. Add the HU directly, but then look for the matching child HUs.
          read_warehouse_task( <ls_huhdr>-huident ).
          IF ss_warehouse_task IS NOT INITIAL.
            st_hus_on_cart = VALUE #( BASE st_hus_on_cart ( <ls_huhdr> ) ).
          ENDIF.

          LOOP AT lt_hutree ASSIGNING FIELD-SYMBOL(<ls_hierarchy_entry>) WHERE guid_parent = <ls_huhdr>-guid_hu.
            IF NOT  line_exists( lt_huhdr[ guid_hu = <ls_hierarchy_entry>-guid ]  ).
              CONTINUE.
            ENDIF.
            read_warehouse_task( lt_huhdr[ guid_hu = <ls_hierarchy_entry>-guid ]-huident ).
            IF ss_warehouse_task IS NOT INITIAL.
              st_hus_on_cart = VALUE #( BASE st_hus_on_cart ( lt_huhdr[ guid_hu = <ls_hierarchy_entry>-guid ] ) ).
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.

    "Reset marker for Cart postion
    CLEAR sv_current_cart_position.
  ENDMETHOD.


  METHOD read_material.
**********************************************************************
*& Key           : AD-230803
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Reads the material master data and passes the information to the
*& application parameter
**********************************************************************
    DATA ls_mat_global TYPE /scwm/s_material_global.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = iv_matid
            iv_langu      = sy-langu
            iv_lgnum      = ss_resource-lgnum
          IMPORTING
            es_mat_global = ls_mat_global.

        cs_cart_postion-item_desrc = ls_mat_global-maktx.
        cs_cart_postion-product = ls_mat_global-matnr.

        SELECT SINGLE mfrpn
          INTO cs_cart_postion-mfrpn
          FROM mara
         WHERE matnr = cs_cart_postion-product.

        IF sy-subrc <> 0.
          CLEAR cs_cart_postion-mfrpn.
        ENDIF.

      CATCH  /scwm/cx_md  INTO DATA(lx_exception).
        CLEAR cs_cart_postion-product.
        CLEAR cs_cart_postion-item_desrc.
        CLEAR cs_cart_postion-mfrpn.
    ENDTRY.
  ENDMETHOD.


  METHOD read_warehouse_task.
**********************************************************************
*& Key           : AD-230728
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Reads the open Warehouse tasks for the provided Handling Unit
*&
**********************************************************************
    DATA lt_warehouse_tasks TYPE /scwm/tt_ordim_o.

    " Read Warehouse Task for provided Cart ID
    CALL FUNCTION '/SCWM/TO_READ_SRC'
      EXPORTING
        iv_lgnum     = ss_resource-lgnum
        iv_huident   = iv_handling_unit
      IMPORTING
        et_ordim_o   = lt_warehouse_tasks
      EXCEPTIONS
        wrong_input  = 1
        not_found    = 2
        foreign_lock = 3
        OTHERS       = 4.

    IF sy-subrc <> 0.
      CLEAR ss_warehouse_task.
      RETURN.
    ENDIF.

    " There is a 1:1 relation between Cart Handling Unit and Warehouse Task.
    IF line_exists( lt_warehouse_tasks[ 1 ] ).
      ss_warehouse_task = lt_warehouse_tasks[ 1 ].
    ENDIF.
  ENDMETHOD.


  METHOD set_counter_text.
**********************************************************************
*& Key           : AD-230907
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Set the display text for the scann counter.
*&
**********************************************************************
    DATA lv_matid22 TYPE /sapapo/matid.
    DATA lt_locprodwhst  TYPE /sapapo/dm_matlwhst_tab.

    CLEAR cs_cart_postion-counter_txt.

    IF cs_cart_postion-number_of_totes = 0.
      RETURN.
    ENDIF.

    IF cs_cart_postion-dest_storage_type(3) <> 'KDK'.
      cs_cart_postion-counter_txt = 'Totes/Scanned'(003).
      RETURN.
    ENDIF.

    zcl_core_util_convert=>convert_guid( EXPORTING
                                            iv_guid16 = iv_matid
                                         IMPORTING
                                            ev_guid22 = lv_matid22 ).

    CALL FUNCTION '/SAPAPO/MATLWHST_READ_MULTI_2'
      EXPORTING
        it_key              = VALUE /sapapo/dm_matlwhst_id_tab( ( matid = lv_matid22 lgtyp = cs_cart_postion-dest_storage_type ) )
      IMPORTING
        et_locprodwhst      = lt_locprodwhst
      EXCEPTIONS
        data_not_found      = 1
        interface_incorrect = 2
        error_message       = 3
        OTHERS              = 99.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lv_direct_replenishment_flag) = VALUE char1( lt_locprodwhst[ 1 ]-zz1_dirrpl_stt OPTIONAL ).

    IF lv_direct_replenishment_flag = abap_true.
      cs_cart_postion-counter_txt = 'MC/Scanned'(002).
    ELSE.
      cs_cart_postion-counter_txt = 'Totes/Scanned'(003).
    ENDIF.
  ENDMETHOD.


  METHOD show_next_cart_position.
**********************************************************************
*& Key           : AD-230731
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Transfer next Cart Position to the application paramter in order
*& to display it on the screen.
**********************************************************************

    DATA ls_cart_position TYPE /scwm/s_huhdr_int.

    " Rest the screen fileds.
    CLEAR cs_cart_postion-cart_position.
    CLEAR cs_cart_postion-dest_storage_bin.
    CLEAR cs_cart_postion-org_dest_storage_bin.
    CLEAR cs_cart_postion-dest_storage_type.
    CLEAR cs_cart_postion-execption.
    CLEAR cs_cart_postion-product.
    CLEAR cs_cart_postion-item_desrc.
    CLEAR cs_cart_postion-item_quantity.
    CLEAR cs_cart_postion-item_uom.
    CLEAR cs_cart_postion-item_aquantity.
    CLEAR cs_cart_postion-mfrpn.
    CLEAR cs_cart_postion-number_of_totes.
    CLEAR cs_cart_postion-scanned_totes.
    CLEAR cs_cart_postion-source_hu.
    CLEAR cs_cart_postion-counter_txt.

    " Move to next Cart position
    sv_current_cart_position = sv_current_cart_position + 1.

    " If the Cart Position is greater than the max. lines of the cart, we have reatced the end of the list
    " => Jump back to the beginning.
    IF sv_current_cart_position > lines( st_hus_on_cart ).
      sv_current_cart_position = 1.
    ENDIF.

    ls_cart_position = VALUE #( st_hus_on_cart[ sv_current_cart_position ] OPTIONAL ).

    cs_cart_postion-source_hu = ls_cart_position-huident.

    " HU starting with 20 contain the Cart Position in the number. This has to be extracted
    " and displayed seperatly on the RF Screen.
    DATA(lv_source_hu) = |{ cs_cart_postion-source_hu ALPHA = OUT }|.
    IF lv_source_hu(2) = '20'.
      cs_cart_postion-cart_position = lv_source_hu+6(4).
    ENDIF.

    cs_cart_postion-number_of_totes = ls_cart_position-zz_plan_totes.

    IF NOT line_exists( st_hu_items[ guid_parent = ls_cart_position-guid_hu ] ).
      RETURN.
    ENDIF.

    read_material( EXPORTING
                      iv_matid = st_hu_items[ guid_parent = ls_cart_position-guid_hu ]-matid
                    CHANGING
                       cs_cart_postion = cs_cart_postion ).

    read_warehouse_task( ls_cart_position-huident ).

    cs_cart_postion-item_quantity = ss_warehouse_task-vsolm.
    cs_cart_postion-item_uom = ss_warehouse_task-meins.

    IF ss_warehouse_task-vsola > 0.
      cs_cart_postion-item_aquantity = ss_warehouse_task-vsolm / ss_warehouse_task-vsola.
    ENDIF.

    cs_cart_postion-dest_storage_type = ss_warehouse_task-nltyp.
    cs_cart_postion-dest_storage_bin = ss_warehouse_task-nlpla.

    " Set the text for the scann counter.
    set_counter_text( EXPORTING
                         iv_matid = st_hu_items[ guid_parent = ls_cart_position-guid_hu ]-matid
                      CHANGING
                         cs_cart_postion = cs_cart_postion ).
  ENDMETHOD.


  METHOD validate_cart_hu.
**********************************************************************
*& Key           : AD-230727
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Validates the entered Cart HU
*&
**********************************************************************
    " Cart ID HU can't be empty
    IF iv_cart_hu IS INITIAL.
      so_application_log->add_message( ip_msgid = 'ZMC_RFUI'
                                       ip_msgno = '019' " Cart ID is invalid
                                       ip_msgty = 'E'
                                       ip_msgv1 = |{ iv_cart_hu ALPHA = OUT }| ).
      so_application_log->get_last_message_complete( IMPORTING ev_msg = rs_message ).
      RETURN.
    ENDIF.

    " Cart ID HU must start with '10'.
    DATA(lv_cart_id) = |{ iv_cart_hu ALPHA = OUT }|.
    IF substring( val = lv_cart_id len = 2 ) <> '10'.
      so_application_log->add_message( ip_msgid = 'ZMC_RFUI'
                                       ip_msgno = '019' " Cart ID is invalid
                                       ip_msgty = 'E'
                                       ip_msgv1 = |{ iv_cart_hu ALPHA = OUT }| ).
      so_application_log->get_last_message_complete( IMPORTING ev_msg = rs_message ).
      RETURN.
    ENDIF.

    " Cart ID HU must have open Handling Units
    read_hus_from_cart( iv_cart_hu ).

    IF st_hus_on_cart IS INITIAL.
      so_application_log->add_message( ip_msgid = 'ZMC_RFUI'
                                       ip_msgno = '020' " Cart is empty
                                       ip_msgty = 'E'
                                       ip_msgv1 = |{ iv_cart_hu ALPHA = OUT }| ).
      so_application_log->get_last_message_complete( IMPORTING ev_msg = rs_message ).
      RETURN.
    ENDIF.

    CLEAR rs_message.
  ENDMETHOD.


  METHOD validate_parking_lot.
**********************************************************************
*& Key           : AD-230907
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Validates the entered parking lot (storage bin)
*&
**********************************************************************
    DATA ls_storage_bin TYPE /scwm/lagp.

    CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
      EXPORTING
        iv_lgnum      = ss_warehouse_parameter-lgnum
        iv_lgpla      = iv_parking_lot
      IMPORTING
        es_lagp       = ls_storage_bin
      EXCEPTIONS
        not_found     = 1
        wrong_input   = 2
        error_message = 3
        OTHERS        = 99.

    IF sy-subrc <> 0.
      so_application_log->add_message( ip_msgid = 'ZMC_RFUI'
                                   ip_msgno = '046' " Error while checking Storage bin for parking
                                   ip_msgty = 'E' ).
      so_application_log->get_last_message_complete( IMPORTING ev_msg = rs_message ).
      RETURN.
    ENDIF.

    IF ls_storage_bin-lgtyp <> sv_parking_lot_storage_type.
      so_application_log->add_message( ip_msgid = 'ZMC_RFUI'
                                   ip_msgno = '047' " Storage type &1 is not allowed for parking
                                   ip_msgty = 'E'
                                   ip_msgv1 = |{ ls_storage_bin-lgtyp }| ).
      so_application_log->get_last_message_complete( IMPORTING ev_msg = rs_message ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
