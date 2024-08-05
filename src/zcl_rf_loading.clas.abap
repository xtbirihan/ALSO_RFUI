CLASS zcl_rf_loading DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_huident TYPE STANDARD TABLE OF /scwm/huident WITH EMPTY KEY .

    CLASS-METHODS class_constructor .
    METHODS assign_hu_to_tu
      IMPORTING
        !it_tu_key   TYPE /scwm/tt_aspk_tu
        !it_hu_key   TYPE /scwm/tt_aspk_tu_open_hu
      RETURNING
        VALUE(rv_ok) TYPE abap_bool .
    METHODS constructor
      IMPORTING
        !iv_lgnum TYPE /scwm/lgnum .
    METHODS get_hu_hdr
      IMPORTING
        !iv_lgnum    TYPE /scwm/lgnum
        !iv_huident  TYPE /scwm/huident
      EXPORTING
        !es_huhdr    TYPE /scwm/s_huhdr_int
        !et_huref    TYPE /scwm/tt_huref_int
        !et_huhdr    TYPE /scwm/tt_huhdr_int
        !ev_unnested TYPE abap_bool .
    METHODS get_open_wt_for_hu
      IMPORTING
        !iv_huident        TYPE /scwm/huident
        !iv_lgnum          TYPE /scwm/lgnum
      RETURNING
        VALUE(rs_ltap_mon) TYPE /scwm/s_to_det_mon .
    METHODS get_tu_for_hu
      IMPORTING
        !iv_guid_hu    TYPE /scwm/guid_hu
      EXPORTING
        !et_tu_act_key TYPE /scwm/tt_tu_sr_act_num .
    METHODS select_hu_deliveries
      IMPORTING
        !iv_lgnum    TYPE /scwm/lgnum
        !it_guid_hu  TYPE /scwm/tt_guid_hu
      EXPORTING
        !et_hu_seq   TYPE /scwm/tt_hu_load_seq
        !et_huhdr    TYPE /scwm/tt_huhdr_int
        !et_prd_hdr  TYPE /scwm/dlv_header_out_prd_tab
        !et_prd_item TYPE /scwm/dlv_item_out_prd_tab .
    METHODS select_not_loaded_hus
      IMPORTING
        !it_hu_seq     TYPE /scwm/tt_hu_load_seq
        !iv_lgnum      TYPE /scwm/lgnum
      EXPORTING
        !ev_not_loaded TYPE boole_d .
    METHODS select_tu_deliveries
      IMPORTING
        !iv_lgnum          TYPE /scwm/lgnum
        !iv_tu_num         TYPE /scwm/de_tu_num
        !iv_tu_sr_act_num  TYPE /scwm/de_tu_sr_act_num
        !iv_huident_async  TYPE /scwm/huident
      EXPORTING
        !et_prd_hdr        TYPE /scwm/dlv_header_out_prd_tab
        !et_prd_item       TYPE /scwm/dlv_item_out_prd_tab
        !et_hu_prd         TYPE /scwm/dlv_item_out_prd_hu_tab
        !ev_nof_loaded_hus TYPE i
        !ev_all_hus        TYPE i .
    METHODS get_confirmed_wt_for_hu
      IMPORTING
        !iv_huident        TYPE /scwm/huident
      RETURNING
        VALUE(rs_ltap_mon) TYPE /scwm/s_to_det_mon .
    METHODS reverse_loading_hu_perform
      IMPORTING
        !is_tu_act             TYPE /scwm/s_aspk_tu
        !iv_huident            TYPE /scwm/huident
        !iv_unassign_hu        TYPE abap_bool OPTIONAL
        !iv_guid_hu            TYPE /scwm/guid_hu
      EXPORTING
        !ev_not_loaded         TYPE abap_bool
        !ev_wrong_tu           TYPE abap_bool
        !ev_cannot_be_unloaded TYPE abap_bool
        !ev_cannot_unassign    TYPE abap_bool .
    METHODS reverse_loading_hu
      IMPORTING
        !is_tu_act             TYPE /scwm/s_aspk_tu
        !iv_huident            TYPE /scwm/huident
        !iv_unassign_hu        TYPE abap_bool OPTIONAL
        !iv_guid_hu            TYPE /scwm/guid_hu
      EXPORTING
        !ev_not_loaded         TYPE abap_bool
        !ev_wrong_tu           TYPE abap_bool
        !ev_cannot_be_unloaded TYPE abap_bool
        !ev_cannot_unassign    TYPE abap_bool .
    METHODS unassign_hu_from_tu
      IMPORTING
        !is_hu_tu    TYPE /scwm/s_aspk_tu_hu
      RETURNING
        VALUE(rv_ok) TYPE abap_bool .
    METHODS set_tu_load_complete
      IMPORTING
        !is_tu_sr_act_num     TYPE /scwm/s_tu_sr_act_num
        !iv_check_completness TYPE abap_bool
      RETURNING
        VALUE(rv_completed)   TYPE abap_bool .
    METHODS assign_dlv_to_tu
      IMPORTING
        !is_tu_sr_act_num   TYPE /scwm/s_tu_sr_act_num
        !it_delivery        TYPE /scwm/tt_aspk_tu_open_whr
      RETURNING
        VALUE(rv_completed) TYPE abap_bool .
    METHODS unnest_hu
      IMPORTING
        !iv_huident TYPE /scwm/huident
      EXPORTING
        !et_huident TYPE tt_huident .
    METHODS to_for_hu_read
      IMPORTING
        !iv_iproc         TYPE /scwm/de_iproc
        !iv_tu_num        TYPE /scwm/de_rf_tu_num_load
        !iv_read_only     TYPE boole_d
        !iv_skip_tu_check TYPE boole_d
        !iv_door_bin      TYPE /scwm/lgpla
        !iv_lgnum         TYPE /scwm/lgnum
        !iv_unnest        TYPE abap_bool
        iv_unnested       TYPE abap_bool
      CHANGING
        !cv_huident       TYPE /scwm/huident
        !cv_tanum         TYPE /scwm/tanum
        !cv_nlpla         TYPE /scwm/lgpla
        !cv_pt_load       TYPE /scwm/de_load_procty .
    METHODS denest_and_load
      IMPORTING
        !iv_iproc         TYPE /scwm/de_iproc
        !iv_tu_num        TYPE /scwm/de_rf_tu_num_load
        !iv_read_only     TYPE boole_d
        !iv_skip_tu_check TYPE boole_d
        !iv_door_bin      TYPE /scwm/lgpla
        !iv_lgnum         TYPE /scwm/lgnum
        !iv_huident       TYPE /scwm/huident
        !iv_commit        TYPE abap_bool DEFAULT abap_true .
    METHODS init_log
      IMPORTING
        !iv_external_id TYPE string .
    METHODS save_applog .
    METHODS mixing_hu_is_not_allowed
      IMPORTING
        !iv_partner_id        TYPE /scdl/dl_partyid
      RETURNING
        VALUE(rv_not_allowed) TYPE abap_bool .
    METHODS reset_buffer .
    METHODS dlvs_with_ini_date
      IMPORTING it_huident TYPE ztt_huident
                iv_lgnum   TYPE /scwm/lgnum
      CHANGING  ct_items   TYPE /scwm/tt_huitm.
    METHODS async_dlv_status_set
      IMPORTING iv_lgnum    TYPE /scwm/lgnum
                iv_status   TYPE string
                it_dlv_item TYPE /scwm/tt_huitm.

    METHODS delivery_status_set
      IMPORTING
        it_huident    TYPE ztt_huident
        iv_status     TYPE string
        is_admin_load TYPE /scwm/s_rf_admin_load.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_bupa_buff,
        partner_guid      TYPE but000-partner_guid,
        zz_no_mix_hu_alwd TYPE but000-zz_no_mix_hu_alwd,
      END OF ts_bupa_buff .
    TYPES:
      tt_bupa_buff TYPE SORTED TABLE OF ts_bupa_buff WITH UNIQUE KEY partner_guid .

    DATA mo_log TYPE REF TO /scwm/cl_log .
    DATA mv_loghandle TYPE balloghndl .
    CONSTANTS c_fcode_syexc_hm TYPE /scwm/de_fcode VALUE 'SHHUMI' ##NO_TEXT.     "HU missing
    CONSTANTS c_fcode_syexc_hl TYPE /scwm/de_fcode VALUE 'SHHULE' ##NO_TEXT.     "HU left
    CONSTANTS c_fcode_syexc_hd TYPE /scwm/de_fcode VALUE 'SHHUDA' ##NO_TEXT.     "HU damaged
    CONSTANTS c_dl_status_type_dlo TYPE /scdl/dl_status_type VALUE /scdl/if_dl_status_c=>sc_t_loading ##NO_TEXT ##needed.
    CONSTANTS c_dl_status_type_dld TYPE /scdl/dl_status_type VALUE /scdl/if_dl_status_c=>sc_t_loading_dist ##NO_TEXT.
    CONSTANTS c_dl_status_type_dsp TYPE /scdl/dl_status_type VALUE /scdl/if_dl_status_c=>sc_t_split ##NO_TEXT.
    CONSTANTS c_dl_status_type_dpi TYPE /scdl/dl_status_type VALUE /scdl/if_dl_status_c=>sc_t_picking ##NO_TEXT.
    CONSTANTS c_dl_status_value_completed TYPE /scdl/dl_status_value VALUE /scdl/if_dl_status_c=>sc_v_partly_confirmed ##NO_TEXT ##NEEDED.
    CONSTANTS c_dl_status_value_finished TYPE /scdl/dl_status_value VALUE /scdl/if_dl_status_c=>sc_v_partly_confirmed ##NO_TEXT ##NEEDED.
    CONSTANTS c_dl_status_value_loadnotstar TYPE /scdl/dl_status_value VALUE /scdl/if_dl_status_c=>sc_v_not_started ##NO_TEXT.
    CONSTANTS c_dl_status_value_loadinproc TYPE /scdl/dl_status_value VALUE /scdl/if_dl_status_c=>sc_v_in_process ##NO_TEXT.
    CONSTANTS c_dl_status_value_partly TYPE /scdl/dl_status_value VALUE /scdl/if_dl_status_c=>sc_v_partly_confirmed ##NO_TEXT.
    CONSTANTS c_dl_status_value_notstarted TYPE /scdl/dl_status_value VALUE /scdl/if_dl_status_c=>sc_v_not_started ##NO_TEXT.
    CONSTANTS:
      c_dl_date_type       TYPE /scdl/dl_tsttype VALUE /scdl/if_dl_date_c=>sc_tsttype_sload,
      c_dl_date_cat_actual TYPE /scdl/dl_tst_category VALUE /scdl/if_dl_date_c=>sc_tstcat_actual.
    DATA mv_lgnum TYPE /scwm/lgnum .
    CLASS-DATA so_msg_handler TYPE REF TO /scmb/cl_message_handler .
    CLASS-DATA st_bupa_buff TYPE tt_bupa_buff .
    DATA mv_no_commit TYPE abap_bool .

    METHODS add_message .
    METHODS to_for_hu_read_perform
      IMPORTING
        !iv_tu_num        TYPE /scwm/de_rf_tu_num_load
        !iv_read_only     TYPE boole_d
        !iv_skip_tu_check TYPE boole_d
        !iv_door_bin      TYPE /scwm/lgpla
        !iv_lgnum         TYPE /scwm/lgnum
      CHANGING
        !cv_huident       TYPE /scwm/huident
        !cv_tanum         TYPE /scwm/tanum OPTIONAL
        !cv_nlpla         TYPE /scwm/lgpla OPTIONAL
        !cv_pt_load       TYPE /scwm/de_load_procty OPTIONAL .
ENDCLASS.



CLASS ZCL_RF_LOADING IMPLEMENTATION.


  METHOD add_message.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Add message to the log
**********************************************************************
    IF mo_log IS BOUND.
      mo_log->add_message( ip_row = 0 ).
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD assign_dlv_to_tu.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Assign delivery to the TU
**********************************************************************
    DATA:
      lt_bo_tu_dlv_no_sort TYPE /scwm/tt_bo_tu_dlv_no_sort,
      ls_bo_tu_dlv_no_sort LIKE LINE OF lt_bo_tu_dlv_no_sort.

    TRY.
        DATA(lv_lgnum) = /scwm/cl_tm=>sv_lgnum.
        DATA(lo_bom) = /scwm/cl_sr_bom=>get_instance( ).

        DATA(lo_bo_tu) = lo_bom->get_bo_tu_by_key( is_tu_sr_act_num = is_tu_sr_act_num ).

        LOOP AT it_delivery REFERENCE INTO DATA(lr_dlv).
          CLEAR ls_bo_tu_dlv_no_sort.
          MOVE-CORRESPONDING lr_dlv->* TO ls_bo_tu_dlv_no_sort ##enh_ok.
          MOVE-CORRESPONDING is_tu_sr_act_num TO ls_bo_tu_dlv_no_sort ##enh_ok.
          APPEND ls_bo_tu_dlv_no_sort TO lt_bo_tu_dlv_no_sort.
        ENDLOOP.

        lo_bo_tu->add_tu_dlv(
          EXPORTING
            iv_check_only                = abap_false
            iv_action_only               = abap_true
            it_bo_tu_dlv_no_sort         = lt_bo_tu_dlv_no_sort
            iv_cross_hu_add              = abap_true ).

        lo_bom->save( ).
        COMMIT WORK AND WAIT.

        /scwm/cl_tm=>cleanup( ).
        /scwm/cl_tm=>set_lgnum( lv_lgnum ).

      CATCH /scwm/cx_sr_error .
        ROLLBACK WORK.
        /scwm/cl_tm=>cleanup( ).
        /scwm/cl_tm=>set_lgnum( lv_lgnum ).

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.
  ENDMETHOD.


  METHOD assign_hu_to_tu.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Assign HU to transport unit
**********************************************************************
    DATA lt_out_tu TYPE /scwm/tt_asp_tu ##needed.
    DATA lt_out_open_hu TYPE /scwm/tt_asp_tu_open_hu ##needed.

    /scwm/cl_tm=>set_lgnum( mv_lgnum ).

    DATA(lo_sp) = NEW /scwm/cl_ui_tu_sp( io_message_handler = so_msg_handler io_attribute_handler = NEW /scmb/cl_attribute_handler( ) ).

    lo_sp->lock(
      EXPORTING
        inkeys       =  it_tu_key                " Restricting Input Keys
        aspect       =  /scwm/if_ui_tu_const=>sc_asp_oip_1                " Aspect Name to Be Read from
        lockmode     = /scdl/cl_sp_prd_inb=>/scdl/if_sp1_locking~sc_exclusive_lock                 " Lock Mode
      IMPORTING
        rejected     = DATA(lv_rejected)                 " Error on Back End
*      return_codes =                  " Table of Return Codes
    ).
    IF lv_rejected EQ abap_false.
      lo_sp->select(
        EXPORTING
          inkeys       =  it_tu_key                " Restricting Input Keys
          aspect       =  /scwm/if_ui_tu_const=>sc_asp_oip_1                " Aspect Name to Be Read from
        IMPORTING
          outrecords         = lt_out_tu                 " Changed Aspect Objects
          rejected           = lv_rejected                 " Error on Back End
      ).
    ENDIF.
    IF lv_rejected EQ abap_false.

      lo_sp->select(
        EXPORTING
          inkeys       =  it_hu_key                " Restricting Input Keys
          aspect       =  /scwm/if_ui_tu_const=>sc_asp_odp1_6 " sc_asp_oip_4                " Aspect Name to Be Read from
        IMPORTING
          outrecords         = lt_out_open_hu                 " Changed Aspect Objects
          rejected           = lv_rejected  ).               " Error on Back End

      lo_sp->action(
        EXPORTING
          aspect             =  /scwm/if_ui_tu_const=>sc_asp_oip_1                " Aspect Name
          inkeys             =  it_tu_key
          action             =  /scwm/if_ui_tu_const=>sc_act_tab_change ).                 " Name of Action
      lo_sp->action(
        EXPORTING
          aspect             =  /scwm/if_ui_tu_const=>sc_asp_odp1_6                " Aspect Name
          inkeys             =  it_hu_key
          action             =  /scwm/if_ui_tu_const=>sc_act_asgn_hu                 " Name of Action
        IMPORTING
          outrecords         = lt_out_open_hu                 " Changed Aspect Objects
          rejected           = lv_rejected                 " Error on Back End
      ).
    ENDIF.

    IF lv_rejected EQ abap_false.
      lo_sp->save(
        EXPORTING
          synchronously = abap_true
        IMPORTING
          rejected      =  lv_rejected
      ).
    ENDIF.

    IF lv_rejected = abap_false.
      COMMIT WORK AND WAIT.
      /scwm/cl_tm=>cleanup( ).
      rv_ok = abap_true.
    ELSE.
      ROLLBACK WORK.
      /scwm/cl_tm=>cleanup( ).

    ENDIF.
  ENDMETHOD.


  METHOD async_dlv_status_set.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Set Status of delivery item asyncron
**********************************************************************
    DATA: lv_qname      TYPE trfcqnam.

    DATA: ls_resource   TYPE /scwm/rsrc.


    CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
      EXPORTING
        iv_uname = sy-uname
      CHANGING
        cs_rsrc  = ls_resource.

*   get queue name by warehouse and resource
    CONCATENATE 'SH' iv_lgnum ls_resource-rsrc INTO lv_qname.
    CONDENSE lv_qname.

*   set queue name
    CALL FUNCTION 'TRFC_SET_QUEUE_NAME'
      EXPORTING
        qname              = lv_qname
      EXCEPTIONS
        invalid_queue_name = 0.

*  set delivery status asynchronously
    CALL FUNCTION '/SCWM/RF_SH_STATUS_SET'
      IN BACKGROUND TASK AS SEPARATE UNIT
      EXPORTING
        iv_lgnum    = iv_lgnum
        iv_status   = iv_status
        iv_async    = 'X'
      TABLES
        it_dlv_item = it_dlv_item ##ENH_OK.
  ENDMETHOD.


  METHOD class_constructor.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Create message handler
**********************************************************************
    so_msg_handler = NEW /scmb/cl_message_handler( ).
  ENDMETHOD.


  METHOD constructor.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Initialize
**********************************************************************
    mv_lgnum = iv_lgnum.
  ENDMETHOD.


  METHOD delivery_status_set.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Set delivery status
**********************************************************************
*----------------------------------------------------------------------*
* data declarations                                                    *
*----------------------------------------------------------------------*
    DATA: lv_rejected TYPE boole_d,
          lt_dlv_item TYPE /scwm/tt_huitm.

    DATA: ls_t_sr_gen   TYPE /scwm/t_sr_gen.

    " Max how many deliveries can updated synchronously
    CONSTANTS:
      c_sync_dlv   TYPE i VALUE 10.
*----------------------------------------------------------------------*
* program logic                                                        *
*----------------------------------------------------------------------*

* check the customize, whether start date should be stored or not
    CALL METHOD /scwm/cl_sr_customizing=>get_sr_gen
      EXPORTING
        iv_lgnum    = is_admin_load-lgnum
      RECEIVING
        es_t_sr_gen = ls_t_sr_gen.

    CHECK ls_t_sr_gen-begin_load_odo = wmesr_upd_load_start_date.

* collect those dlvs where the startdate is initial
    dlvs_with_ini_date(
      EXPORTING
        it_huident = it_huident
        iv_lgnum   = is_admin_load-lgnum
      CHANGING
        ct_items   = lt_dlv_item ).

* All deliveries start date is filled -> nothing to do
    CHECK lt_dlv_item IS NOT INITIAL.

    IF lines( lt_dlv_item ) > c_sync_dlv.
*   to much dlv items -> async update
      async_dlv_status_set( iv_lgnum =  is_admin_load-lgnum
                            iv_status = iv_status
                            it_dlv_item = lt_dlv_item ).
      COMMIT WORK.
    ELSE.
* try to update delivery synchronously
      CALL FUNCTION '/SCWM/RF_SH_STATUS_SET'
        EXPORTING
          iv_lgnum    = is_admin_load-lgnum
          iv_status   = iv_status
          iv_async    = ' '
        IMPORTING
          ev_rejected = lv_rejected
        TABLES
          it_dlv_item = lt_dlv_item.

      IF lv_rejected IS NOT INITIAL.
*     sync update was not succesfull, do async update
        async_dlv_status_set( iv_lgnum =  is_admin_load-lgnum
                              iv_status = iv_status
                              it_dlv_item = lt_dlv_item ).
        COMMIT WORK.
        CALL METHOD /scwm/cl_tm=>cleanup( ).
      ELSE.
        COMMIT WORK AND WAIT.
        CALL METHOD /scwm/cl_tm=>cleanup( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD denest_and_load.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Denest the HU and perform load
**********************************************************************
    /scwm/cl_tm=>set_lgnum( iv_lgnum ).

    mv_no_commit = xsdbool( iv_commit = abap_false ).
    init_log( |{ iv_tu_num ALPHA = OUT }{ iv_huident ALPHA = OUT }| ).

    unnest_hu(
      EXPORTING
        iv_huident = iv_huident                 " Unique Internal Identification of a Handling Unit
      IMPORTING
        et_huident = DATA(lt_hu_ident)
    ).

    IF lt_hu_ident IS NOT INITIAL.
      LOOP AT lt_hu_ident INTO DATA(lv_huident).
        to_for_hu_read_perform(
          EXPORTING
            iv_tu_num        = iv_tu_num                        " Transportation Unit for Loading
            iv_read_only     = iv_read_only
            iv_skip_tu_check = iv_skip_tu_check
            iv_door_bin      = iv_door_bin                      " Storage Bin
            iv_lgnum         = iv_lgnum                         " Warehouse Number/Warehouse Complex
          CHANGING
            cv_huident       = lv_huident
        ).
      ENDLOOP.
    ENDIF.

    IF mo_log->get_severity( ) CA 'AEX'.
      READ TABLE mo_log->get_prot( ) INTO DATA(ls_msg)
           WITH KEY type = mo_log->get_severity( ).
      IF sy-subrc EQ 0.
        MESSAGE ID ls_msg-id TYPE ls_msg-type NUMBER ls_msg-number
                WITH ls_msg-message_v1 ls_msg-message_v2
                     ls_msg-message_v3 ls_msg-message_v4.
      ENDIF.
    ENDIF.

    save_applog( ).
  ENDMETHOD.


  METHOD dlvs_with_ini_date.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Read deliveries with a specific status not started
**********************************************************************
    DATA: lt_items       TYPE /scwm/dlv_item_out_prd_tab,
          lt_docid       TYPE /scwm/dlv_docid_item_tab,
          lt_message     TYPE /scdl/dm_message_tab,
          lt_huitm       TYPE /scwm/tt_huitm_int.

    DATA: ls_docid      TYPE /scwm/dlv_docid_item_str,
          ls_dlv_item   TYPE LINE OF /scwm/tt_huitm.

    DATA: ls_read_options TYPE /scwm/dlv_query_contr_str,
          ls_include_data TYPE /scwm/dlv_query_incl_str_prd.

    DATA: lo_dlv     TYPE REF TO /scwm/cl_dlv_management_prd,
          lo_message TYPE REF TO /scwm/cl_dm_message_no.

    FIELD-SYMBOLS: <fs_dlv_items>  TYPE /scwm/dlv_item_out_prd_str,
                   <fs_item_dates> TYPE /scdl/dl_date_str,
                   <fs_msge>       TYPE /scdl/dm_message_str, "#EC *
                   <s_huitm>       TYPE /scwm/s_huitm_int.


* create delivery object
    IF lo_dlv IS NOT BOUND.
      CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
        RECEIVING
          eo_instance = lo_dlv.
    ENDIF.

    CLEAR: ct_items.

    CALL FUNCTION '/SCWM/HU_SELECT'
      EXPORTING
        iv_lock    = ' '
        it_huident = VALUE /scwm/tt_huident( FOR id IN it_huident ( lgnum = iv_lgnum huident = id ) )
      IMPORTING
        et_huitm   = lt_huitm
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


* collect deliveries to be checked
    LOOP AT lt_huitm ASSIGNING <s_huitm>.
      ls_docid-docid  = <s_huitm>-qdocid.
      ls_docid-doccat = <s_huitm>-qdoccat.
      ls_docid-itemid = <s_huitm>-qitmid.
      APPEND ls_docid TO lt_docid.
    ENDLOOP.

    CHECK lt_docid IS NOT INITIAL.

    SORT lt_docid.
    DELETE ADJACENT DUPLICATES FROM lt_docid.

* ensures the only needed data is read:
    ls_include_data-head_date   = 'X'.
    ls_read_options-data_retrival_only = 'X'.
    ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance.
    ls_include_data-item_date = 'X'.

* ensures fast DB readwithout BO instantiation.
* read status for deliveries
    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            it_docid        = lt_docid
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_items        = lt_items
            eo_message      = lo_message.
      CATCH  /scdl/cx_delivery.                         "#EC NO_HANDLER
        CHECK lo_message IS NOT INITIAL.
        lt_message = lo_message->get_messages( ).

        READ TABLE lt_message
          ASSIGNING <fs_msge>
          WITH KEY msgty = wmegc_severity_err.

        IF sy-subrc = 0.
          MESSAGE ID     <fs_msge>-msgid
                  TYPE   <fs_msge>-msgty
                  NUMBER <fs_msge>-msgno
                  WITH   <fs_msge>-msgv1
                         <fs_msge>-msgv2
                         <fs_msge>-msgv3
                         <fs_msge>-msgv4.
        ENDIF.
    ENDTRY.


* check the delivery items
    LOOP AT lt_items ASSIGNING <fs_dlv_items>.

      CLEAR ls_dlv_item.
      ls_dlv_item-qdocid = <s_huitm>-qdocid.
      ls_dlv_item-qitmid = <s_huitm>-qitmid.

*   collect those dlv items which the start date is initial
      READ TABLE <fs_dlv_items>-dates ASSIGNING <fs_item_dates>
        WITH KEY tsttype      = c_dl_date_type
                 tst_category = c_dl_date_cat_actual.
      IF sy-subrc <> 0.
        APPEND ls_dlv_item TO ct_items.
      ELSE.
*     entry could exist w/o value -> update required
        IF <fs_item_dates>-tstfr IS INITIAL.
          APPEND ls_dlv_item TO ct_items.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_confirmed_wt_for_hu.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Get the last confirmed wt to the TU
**********************************************************************
    DATA: ls_selcrit TYPE /scwm/s_to_selcrit_mon,
          lt_to      TYPE /scwm/tt_to_det_mon.

    "Add open WT selection criteria
    ls_selcrit-r_tostat = VALUE #( ( sign = wmegc_sign_inclusive
                                     option = wmegc_option_eq
                                     low = wmegc_to_confirmed )
                                     ).

    "Add HU Number selection criteria
    ls_selcrit-r_lenr = VALUE #( (   sign = wmegc_sign_inclusive
                                     option = wmegc_option_eq
                                     low = iv_huident ) ).

    "Select Open HU WTs
    CALL FUNCTION '/SCWM/TO_GET_WIP'
      EXPORTING
        iv_lgnum   = mv_lgnum
        iv_srcdata = abap_true
        iv_dstdata = abap_true
        is_selcrit = ls_selcrit
        iv_conf    = abap_true
      IMPORTING
        et_to      = lt_to.

    SORT lt_to BY confirmed_at DESCENDING.
    LOOP AT lt_to REFERENCE INTO DATA(lr_to).
      IF lr_to->dloc_type EQ wmegc_tu and lr_to->sloc_type EQ wmegc_bin. "loaded from bin to tu
        rs_ltap_mon = lr_to->*.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_hu_hdr.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Get HU header
**********************************************************************
    DATA lt_hdr  TYPE /scwm/tt_huhdr_int.
    DATA(lo_packing) = NEW /scwm/cl_wm_packing( ).
    CLEAR: es_huhdr, et_huref, et_huhdr, ev_unnested.

    CALL METHOD lo_packing->get_hu
      EXPORTING
        iv_huident = iv_huident
      IMPORTING
        es_huhdr   = es_huhdr
        et_huref   = et_huref
        et_huhdr   = et_huhdr
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc NE 0. "Search by tracking no
      CALL FUNCTION '/SCWM/HU_SELECT'
        EXPORTING
          it_ident    = VALUE /scwm/tt_ident( ( idart = zif_wme_c=>gs_huidart-v ident = iv_huident ) )
          is_location = VALUE /lime/loc_key( lgnum = iv_lgnum )
        IMPORTING
          et_huhdr    = lt_hdr
          et_ref      = et_huref
        EXCEPTIONS
          not_found   = 1
          OTHERS      = 2.
      IF sy-subrc EQ 0.
        IF lt_hdr IS NOT INITIAL.
          es_huhdr = VALUE #( lt_hdr[ lgnum = iv_lgnum ] OPTIONAL ).
        ENDIF.
      ELSE. "Search by top hu for loading unloading
        CALL FUNCTION '/SCWM/HU_SELECT'
          EXPORTING
            it_ident    = VALUE /scwm/tt_ident( ( idart = zif_wme_c=>gs_huidart-h ident = iv_huident ) )
            is_location = VALUE /lime/loc_key( lgnum = iv_lgnum )
          IMPORTING
            et_huhdr    = lt_hdr
            et_ref      = et_huref
          EXCEPTIONS
            OTHERS      = 0.
        IF lt_hdr IS NOT INITIAL.
          DELETE lt_hdr WHERE lgnum NE iv_lgnum.
          et_huhdr = lt_hdr.
          ev_unnested = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_open_wt_for_hu.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Get open WT for the HU
**********************************************************************
    DATA: ls_selcrit TYPE /scwm/s_to_selcrit_mon,
          lt_to      TYPE /scwm/tt_to_det_mon.

    "Add open WT selection criteria
    ls_selcrit-r_tostat = VALUE #( ( sign = wmegc_sign_inclusive
                                     option = wmegc_option_eq
                                     low = wmegc_to_open )
                                     ).

    "Add HU Number selection criteria
    ls_selcrit-r_lenr = VALUE #( (   sign = wmegc_sign_inclusive
                                     option = wmegc_option_eq
                                     low = iv_huident ) ).

    "Select Open HU WTs
    CALL FUNCTION '/SCWM/TO_GET_WIP'
      EXPORTING
        iv_lgnum   = iv_lgnum
        iv_open    = abap_true
        iv_srcdata = abap_true
        iv_dstdata = abap_true
        is_selcrit = ls_selcrit
      IMPORTING
        et_to      = lt_to.

    READ TABLE lt_to INTO rs_ltap_mon
         WITH KEY tostat = wmegc_to_inactiv.
    IF sy-subrc NE 0.
      READ TABLE lt_to INTO rs_ltap_mon INDEX 1.
    ENDIF.

  ENDMETHOD.


  METHOD get_tu_for_hu.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Get transportation units to the HU
**********************************************************************
    DATA(lo_tudlv_manager) = /scwm/cl_sr_tudlv=>get_instance( ).

    lo_tudlv_manager->setup(
                        EXPORTING
                          it_guid_hu = VALUE #( ( guid_hu = iv_guid_hu ) ) ).

    lo_tudlv_manager->get_tu_by_hu(
                        EXPORTING
                          iv_guid_hu    = iv_guid_hu
                          iv_direct     = abap_false
                          iv_doccat     = wmegc_doccat_pdo
                        IMPORTING
                          et_tu_act_key = et_tu_act_key ).
  ENDMETHOD.


  METHOD init_log.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Initialize log
**********************************************************************
    DATA: ls_log TYPE bal_s_log.
    CREATE OBJECT mo_log
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_balobj    = zif_wme_c=>gs_msgobj-zewm
        iv_balsubobj = zif_wme_c=>gs_msgsubobj-zloadhuto.

    "Set log params
    ls_log-extnumber = iv_external_id.
    ls_log-object = zif_wme_c=>gs_msgobj-zewm.
    ls_log-subobject = zif_wme_c=>gs_msgsubobj-zloadhuto.
    ls_log-aluser = sy-uname.
    ls_log-alprog = sy-repid.

    "Create Log
    mo_log->create_log(
      EXPORTING
        is_log       = ls_log
      IMPORTING
        ev_loghandle = mv_loghandle ).

  ENDMETHOD.


  METHOD mixing_hu_is_not_allowed.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Determine, whether mixing HU is allowed
**********************************************************************
    READ TABLE st_bupa_buff INTO DATA(ls_buff)
         WITH TABLE KEY partner_guid = iv_partner_id.
    IF sy-subrc NE 0.
      SELECT SINGLE FROM but000
             FIELDS zz_no_mix_hu_alwd
              WHERE partner_guid = @iv_partner_id
               INTO @ls_buff-zz_no_mix_hu_alwd ##warn_ok.
      ls_buff-partner_guid = iv_partner_id.
      INSERT ls_buff INTO TABLE st_bupa_buff.
    ENDIF.

    rv_not_allowed = ls_buff-zz_no_mix_hu_alwd.

  ENDMETHOD.


  METHOD reset_buffer.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Buffer cleanup
**********************************************************************
    CLEAR st_bupa_buff.
  ENDMETHOD.


  METHOD reverse_loading_hu.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Reverse loading: move HU from TU to staging
**********************************************************************
    DATA lv_msg TYPE string ##needed.
    CLEAR: ev_not_loaded, ev_wrong_tu, ev_cannot_be_unloaded, ev_cannot_unassign.

    get_hu_hdr(
      EXPORTING
        iv_lgnum   = mv_lgnum
        iv_huident = iv_huident                 " Unique Internal Identification of a Handling Unit
      IMPORTING
        et_huhdr   = DATA(lt_huhdr)                   " Table Type for HU Headers in the Internal Structure
        ev_unnested = DATA(lv_unnested) ).
    IF lt_huhdr IS INITIAL.
      MESSAGE e014(/scwm/rf_de) WITH iv_huident INTO lv_msg.
      add_message( ).
      RETURN.
    ENDIF.

    IF lv_unnested EQ abap_false.
      reverse_loading_hu_perform(
        EXPORTING
          is_tu_act             = is_tu_act                             " Key for Aspect: TU
          iv_huident            = iv_huident                       " Handling Unit Identification
          iv_unassign_hu        = iv_unassign_hu
          iv_guid_hu            = iv_guid_hu                            " Unique Internal Identification of a Handling Unit
        IMPORTING
          ev_not_loaded         = ev_not_loaded
          ev_wrong_tu           = ev_wrong_tu
          ev_cannot_be_unloaded = ev_cannot_be_unloaded
          ev_cannot_unassign    = ev_cannot_unassign
      ).
    ELSE.
      DATA(lv_qname) = CONV trfcqin-qname( |{ zif_wme_c=>gs_queue-ztuunloadhu }{ sy-datum }{ iv_huident ALPHA = OUT }| ).

      CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
        EXPORTING
          qin_name = lv_qname.

      CALL FUNCTION 'Z_RF_PERFORM_UNLOAD' IN BACKGROUND TASK
        EXPORTING
          iv_lgnum       = mv_lgnum
          is_tu_act      = is_tu_act                             " Key for Aspect: TU
          iv_huident     = iv_huident                       " Handling Unit Identification
          iv_unassign_hu = iv_unassign_hu
          iv_guid_hu     = iv_guid_hu.                            " Unique Internal Identification of a Handling Unit
      COMMIT WORK.

    ENDIF.
  ENDMETHOD.


  METHOD reverse_loading_hu_perform.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Reverse loading: move HU from TU to staging
**********************************************************************
    DATA: lt_bapiret  TYPE bapirettab ##needed,
          lv_severity TYPE bapi_mtype,
          lt_ltap_vb  TYPE /scwm/tt_ltap_vb ##needed.

    CLEAR: ev_not_loaded, ev_cannot_be_unloaded, ev_wrong_tu, ev_cannot_unassign.

    DATA(ls_ltap) = get_confirmed_wt_for_hu( iv_huident  = iv_huident ).
    IF ls_ltap IS INITIAL.
      ev_not_loaded = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE FROM /scwm/t340d FIELDS pt_rev_load
           WHERE lgnum EQ @mv_lgnum
           INTO @DATA(lv_pr_rev_load).

    IF is_tu_act-tu_num NE ls_ltap-dtu_num.
      ev_wrong_tu = abap_true.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'                    "#EC ENHOK
      EXPORTING
        iv_lgnum       = mv_lgnum
        it_create_hu   = VALUE /scwm/tt_to_crea_hu( (
                                            huident = iv_huident
                                            squit   = abap_true
                                            nlpla   = ls_ltap-vlpla
                                            procty  = lv_pr_rev_load
                                            norou   = wmegc_norou_yes
                                         ) )
        iv_wtcode      = wmegc_wtcode_adhoc_hu
        iv_commit_work = abap_true
      IMPORTING
        et_ltap_vb     = lt_ltap_vb
        et_bapiret     = lt_bapiret
        ev_severity    = lv_severity.
    IF lv_severity CA wmegc_severity_eax.
      ev_cannot_be_unloaded = abap_true.
      RETURN.
    ENDIF.

    IF iv_unassign_hu EQ abap_false.
      RETURN.
    ENDIF.

    DATA(lv_ok) = unassign_hu_from_tu(
        is_hu_tu = VALUE #( tu_num = is_tu_act-tu_num
                            tu_sr_act_num = is_tu_act-tu_sr_act_num
                            lgnum = mv_lgnum
                            huident = iv_huident
                            guid_hu = iv_guid_hu
                            )                 " Table Type: Key Aspect for TU-HU Assignment
    ).
    IF lv_ok EQ abap_false.
      ev_cannot_unassign = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD save_applog.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Save application log
**********************************************************************
    DATA: ls_log TYPE bal_s_log.

    CHECK mo_log IS BOUND.

    TRY.

        "Get Log
        mo_log->get_log(
          IMPORTING
          es_log = ls_log
        ).

        "Save to App Log
        mo_log->save_applog(
          EXPORTING
            is_log       = ls_log
          IMPORTING
            ev_loghandle = mv_loghandle
        ).

        mo_log->save_applog2db(
          EXPORTING
            iv_loghandle = mv_loghandle
        ).
      CATCH /scwm/cx_basics.
        add_message( ).
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD select_hu_deliveries.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Get the deliveries assigned to the HU
**********************************************************************
*- data
    DATA:
      lt_docid   TYPE /scwm/tt_docid,
      lt_qitmid  TYPE /scwm/tt_itemid,
      lt_guid_hu TYPE /scwm/tt_guid_hu,
      ls_huhdr   TYPE /scwm/s_huhdr_int,
      ls_hu_seq  TYPE /scwm/s_hu_load_seq,
      lt_qdocid  TYPE /scwm/tt_docid,
      lt_huitm   TYPE  /scwm/tt_huitm_int,
      lt_dlv_sel TYPE /scwm/dlv_selection_tab,
      ls_dlv_sel TYPE /scwm/dlv_selection_str.


    CLEAR: et_hu_seq, et_huhdr, et_prd_hdr, et_prd_item.
*---------------------------
*- prepare for call

    IF it_guid_hu IS NOT INITIAL.
      lt_guid_hu = it_guid_hu.
    ENDIF.

*- read top HUs
    CALL FUNCTION '/SCWM/HU_SELECT_GEN'
      EXPORTING
        iv_lgnum     = iv_lgnum
        it_guid_hu   = lt_guid_hu
        it_docid     = lt_docid
        it_qdocid    = lt_qdocid
        it_qitmid    = lt_qitmid
      IMPORTING
        et_high      = et_huhdr
        et_huitm     = lt_huitm
      EXCEPTIONS
        wrong_input  = 1
        not_possible = 2
        OTHERS       = 3.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lt_huitm IS INITIAL.
      RETURN.
    ENDIF.
    ls_dlv_sel-fieldname = /scwm/if_dl_logfname_c=>sc_whno_i.
    ls_dlv_sel-option    = 'EQ'.
    ls_dlv_sel-low       = iv_lgnum.
    CLEAR ls_dlv_sel-high.
    APPEND ls_dlv_sel TO lt_dlv_sel.
*   collect the deliveries
    CLEAR ls_dlv_sel-high.
    LOOP AT lt_huitm INTO DATA(ls_huitm) ##INTO_OK.
      ls_dlv_sel-fieldname = /scdl/if_dl_logfname_c=>sc_docid_h.
      ls_dlv_sel-option    = wmegc_option_eq.
      ls_dlv_sel-low       = ls_huitm-qdocid.
      APPEND ls_dlv_sel TO lt_dlv_sel.
      CLEAR ls_dlv_sel.
      ls_dlv_sel-fieldname = /scdl/if_dl_logfname_c=>sc_itemid_i.
      ls_dlv_sel-option    = wmegc_option_eq.
      ls_dlv_sel-low       = ls_huitm-qitmid.
      APPEND ls_dlv_sel TO lt_dlv_sel.
    ENDLOOP.

    DATA(ls_include_data) = VALUE /scwm/dlv_query_incl_str_prd( head_transport = 'X' head_partyloc = abap_true ).
    DATA(ls_read_options) = VALUE /scwm/dlv_query_contr_str(  data_retrival_only = 'X'
                                                              mix_in_object_instances = 'X'
                                                              item_part_select = 'X' ).

    DATA(lo_dlv) = /scwm/cl_dlv_management_prd=>get_instance( ).

    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            iv_doccat       = wmegc_doccat_pdo
            it_selection    = lt_dlv_sel
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_headers      = et_prd_hdr
            et_items        = et_prd_item.

      CATCH /scdl/cx_delivery.                          "#EC NO_HANDLER
    ENDTRY.


*- filter for correct warehouse number
    LOOP AT et_huhdr TRANSPORTING NO FIELDS
      WHERE lgnum NE iv_lgnum.
      DELETE et_huhdr INDEX sy-tabix.
    ENDLOOP.


    LOOP AT et_huhdr INTO ls_huhdr ##INTO_OK.
      CLEAR ls_hu_seq.
*   determine HU status (loaded y/n)
      CALL FUNCTION 'CRM_STATUS_CHECK'
        EXPORTING
          objnr             = ls_huhdr-guid_hu
          status            = wmegc_hustat_loaded
        EXCEPTIONS
          object_not_found  = 1
          status_not_active = 2
          OTHERS            = 3.
      CASE sy-subrc.
        WHEN 0.
*     HU loaded
          ls_hu_seq-status_load = abap_true.
        WHEN 1.
          CONTINUE.
        WHEN 2. " not yet loaded
*     HU not yet loaded -> nothing to do
        WHEN OTHERS.
*     error
          MESSAGE ID     sy-msgid
                  TYPE   sy-msgty
                  NUMBER sy-msgno
                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDCASE.

      ls_hu_seq-huident = ls_huhdr-huident.
      ls_hu_seq-guid_hu = ls_huhdr-guid_hu.
      APPEND ls_hu_seq TO et_hu_seq.
    ENDLOOP.

    SORT et_hu_seq BY huident.
    DELETE ADJACENT DUPLICATES FROM et_hu_seq COMPARING huident.

  ENDMETHOD.


  METHOD select_not_loaded_hus.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Select HUs which are not loaded
**********************************************************************
    DATA: ls_huhdr  TYPE /scwm/s_huhdr_int,
          lt_huhdr  TYPE /scwm/tt_huhdr_int,
          lt_hustat TYPE /scwm/tt_hustatus.

    FIELD-SYMBOLS: <s_hu_seq>  TYPE /scwm/s_hu_load_seq.

    CLEAR ev_not_loaded.

* Filling buffer for HU status reading
    LOOP AT it_hu_seq ASSIGNING <s_hu_seq>
      WHERE status_load = ' ' AND
            fcode_exc NE c_fcode_syexc_hm AND   "missing HU
            fcode_exc NE c_fcode_syexc_hl AND   "HU left - TU is full
            fcode_exc NE c_fcode_syexc_hd.  "HU damaged
      ls_huhdr-guid_hu = <s_hu_seq>-guid_hu.
      APPEND ls_huhdr TO lt_huhdr.
    ENDLOOP.

    CALL FUNCTION '/SCWM/HU_STATUS_SELECT'
      EXPORTING
        iv_stat   = wmegc_hustat_loaded
        it_huhdr  = lt_huhdr
      IMPORTING
        et_hustat = lt_hustat.

    LOOP AT lt_huhdr INTO ls_huhdr ##INTO_OK.
*   Read do the HU have status "loaded".
      READ TABLE lt_hustat TRANSPORTING NO FIELDS WITH KEY objnr = ls_huhdr-guid_hu inact = abap_false.
      IF sy-subrc IS NOT INITIAL.
        ev_not_loaded = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD select_tu_deliveries.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Get the deliveries for the TU
**********************************************************************
*- data
    DATA: lt_dlv_sel          TYPE /scwm/dlv_selection_tab,
          ls_dlv_sel          TYPE /scwm/dlv_selection_str,
          ls_read_options     TYPE /scwm/dlv_query_contr_str,
          ls_include_data     TYPE /scwm/dlv_query_incl_str_prd,
          lt_tu_act_key       TYPE /scwm/tt_tu_sr_act_num,
          ls_tu_act_key       TYPE /scwm/s_tu_sr_act_num,
          lt_docid_itmid_i    TYPE /scwm/tt_docid_itmid,
          ls_docid_itmid_i    TYPE /scwm/s_docid_itmid,
          lo_dlv              TYPE REF TO /scwm/cl_dlv_management_prd,
          lo_tudlv_manager    TYPE REF TO /scwm/cl_sr_tudlv,
          lt_huhdr            TYPE  /scwm/tt_huhdr_int,
          lt_ident            TYPE  /scwm/tt_ident_int,
          lt_hustat           TYPE /scwm/tt_hustatus,
          lt_huident_top_load TYPE SORTED TABLE OF /scwm/huident WITH UNIQUE KEY table_line,
          lt_huident_top_all  TYPE SORTED TABLE OF /scwm/huident WITH UNIQUE KEY table_line.

    CLEAR: et_hu_prd, et_prd_hdr, et_prd_item, ev_nof_loaded_hus, ev_all_hus.

    lo_tudlv_manager = /scwm/cl_sr_tudlv=>get_instance( ).

    ls_tu_act_key-tu_num = iv_tu_num.
    ls_tu_act_key-tu_sr_act_num = iv_tu_sr_act_num.
    APPEND ls_tu_act_key TO lt_tu_act_key.
    lo_tudlv_manager->setup( EXPORTING it_tu_act_key = lt_tu_act_key
                                       iv_check_db   = abap_true ).
    lo_tudlv_manager->get_dlvi_by_tu(
                        EXPORTING is_tu_act_key  = ls_tu_act_key
                        IMPORTING et_docid_itmid = lt_docid_itmid_i ).
    lo_tudlv_manager->get_hu_by_tu(
      EXPORTING
        is_tu_act_key   = ls_tu_act_key                 " Number of an S&R Activity of a TU
      IMPORTING
        et_guid_hu      = DATA(lt_guid_hu)                  " Table of HU GUIDs
    ).
    IF lt_docid_itmid_i IS INITIAL.
      RETURN.
    ENDIF.
*   query deliveries
*   warehouse number
    ls_dlv_sel-fieldname = /scwm/if_dl_logfname_c=>sc_whno_i.
    ls_dlv_sel-option    = 'EQ'.
    ls_dlv_sel-low       = iv_lgnum.
    CLEAR ls_dlv_sel-high.
    APPEND ls_dlv_sel TO lt_dlv_sel.
*   collect the deliveries
    CLEAR ls_dlv_sel-high.
    LOOP AT lt_docid_itmid_i INTO ls_docid_itmid_i.
      ls_dlv_sel-fieldname = /scdl/if_dl_logfname_c=>sc_docid_h.
      ls_dlv_sel-option    = wmegc_option_eq.
      ls_dlv_sel-low       = ls_docid_itmid_i-docid.
      APPEND ls_dlv_sel TO lt_dlv_sel.
      CLEAR ls_dlv_sel.
      ls_dlv_sel-fieldname = /scdl/if_dl_logfname_c=>sc_itemid_i.
      ls_dlv_sel-option    = wmegc_option_eq.
      ls_dlv_sel-low       = ls_docid_itmid_i-itmid.
      APPEND ls_dlv_sel TO lt_dlv_sel.
    ENDLOOP.
*   LOAD STATUS EQ NOT STRATED
    ls_dlv_sel-fieldname = /scdl/if_dl_logfname_c=>sc_status_value_dlo_i.
    ls_dlv_sel-option    = 'EQ'.
    ls_dlv_sel-low       = /scdl/if_dl_status_c=>sc_v_not_started.
    CLEAR ls_dlv_sel-high.
    APPEND ls_dlv_sel TO lt_dlv_sel.
*   LOAD STATUS EQ IN PROCESS
    ls_dlv_sel-fieldname = /scdl/if_dl_logfname_c=>sc_status_value_dlo_i.
    ls_dlv_sel-option    = 'EQ'.
    ls_dlv_sel-low       = /scdl/if_dl_status_c=>sc_v_in_process.
    CLEAR ls_dlv_sel-high.
    APPEND ls_dlv_sel TO lt_dlv_sel.
*   Completed
    ls_dlv_sel-fieldname = /scdl/if_dl_logfname_c=>sc_status_value_dlo_i.
    ls_dlv_sel-option    = 'EQ'.
    ls_dlv_sel-low       = /scdl/if_dl_status_c=>sc_v_confirmed.
    CLEAR ls_dlv_sel-high.
    APPEND ls_dlv_sel TO lt_dlv_sel.

    CLEAR ls_include_data.
    ls_include_data-head_transport = 'X'.

    ls_read_options-data_retrival_only = 'X'.
    ls_read_options-mix_in_object_instances = 'X'.
    ls_read_options-item_part_select = 'X'.

    IF lo_dlv IS NOT BOUND.
      CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
        RECEIVING
          eo_instance = lo_dlv.
    ENDIF.

    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            iv_doccat       = wmegc_doccat_pdo
            it_selection    = lt_dlv_sel
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_headers      = et_prd_hdr
            et_items        = et_prd_item
            et_hu_prd       = et_hu_prd.

      CATCH /scdl/cx_delivery.                          "#EC NO_HANDLER
    ENDTRY.

    CALL FUNCTION '/SCWM/HU_SELECT'
      EXPORTING
        it_guid_hu = VALUE /scwm/tt_guid_hu( FOR guid IN lt_guid_hu ( guid_hu = guid-guid_hu ) )
      IMPORTING
        et_huhdr   = lt_huhdr
        et_ident   = lt_ident
      EXCEPTIONS
        OTHERS     = 0.

    IF lt_huhdr IS NOT INITIAL.
      CALL FUNCTION '/SCWM/HU_STATUS_SELECT'
        EXPORTING
          iv_stat   = wmegc_hustat_loaded
          it_huhdr  = VALUE /scwm/tt_huhdr_int( FOR huhdr IN lt_huhdr ( guid_hu = huhdr-guid_hu ) )
        IMPORTING
          et_hustat = lt_hustat.
    ENDIF.

    LOOP AT et_prd_item REFERENCE INTO DATA(lr_prd_item).
      READ TABLE et_hu_prd TRANSPORTING NO FIELDS
           WITH KEY docid = lr_prd_item->docid
                    itemid = lr_prd_item->itemid.
      IF sy-subrc NE 0.
        DELETE et_prd_item.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_huhdr INTO DATA(ls_huhdr)
         WHERE top EQ abap_true.
*   Read do the HU have status "loaded".
      READ TABLE lt_hustat TRANSPORTING NO FIELDS WITH KEY objnr = ls_huhdr-guid_hu inact = abap_false.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_ident INTO DATA(ls_ident_top_hu)
             WITH KEY guid_hu = ls_huhdr-guid_hu
                      idart   = zif_wme_c=>gs_huidart-h.
        IF sy-subrc EQ 0.
          COLLECT CONV /scwm/huident( ls_ident_top_hu-huident ) INTO lt_huident_top_load.
        ELSE.
          ADD 1 TO ev_nof_loaded_hus.
        ENDIF.
      ELSEIF ls_huhdr-huident EQ iv_huident_async.
        ADD 1 TO ev_nof_loaded_hus.
      ENDIF.
      READ TABLE lt_ident INTO ls_ident_top_hu
           WITH KEY guid_hu = ls_huhdr-guid_hu
                    idart   = zif_wme_c=>gs_huidart-h.
      IF sy-subrc EQ 0.
        COLLECT CONV /scwm/huident( ls_ident_top_hu-huident ) INTO lt_huident_top_all.
      ELSE.
        ADD 1 TO ev_all_hus.
      ENDIF.
    ENDLOOP.
    ev_nof_loaded_hus = lines( lt_huident_top_load ) + ev_nof_loaded_hus.
    ev_all_hus = lines( lt_huident_top_all ) + ev_all_hus.

  ENDMETHOD.


  METHOD set_tu_load_complete.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Set the load and to the TU
**********************************************************************
    DATA: lv_tu_end_load  TYPE /scmb/tmdl_status_value.
    DATA: lo_bom          TYPE REF TO /scwm/cl_sr_bom.
    DATA: lo_bo_tu        TYPE REF TO /scwm/cl_sr_bo_tu.
    DATA: lv_msgv1 TYPE symsgv,
          lv_msgv2 TYPE symsgv.

    DATA  lo_dlv          TYPE REF TO /scwm/cl_dlv_management_prd.
    DATA  ls_include_data TYPE /scwm/dlv_query_incl_str_prd.
    DATA  ls_read_options TYPE /scwm/dlv_query_contr_str.
    DATA  lt_stat_dyn_det TYPE /scwm/dlv_status_type_tab.
    DATA  ls_stat_dyn_det TYPE /scdl/dl_status_type.

    DATA  lt_prd_itm      TYPE /scwm/dlv_item_out_prd_tab.
    DATA  lv_open_dlvs    TYPE xfeld.
    DATA  lv_locked       TYPE xfeld.
    DATA  lv_nosave       TYPE xfeld.
    DATA  lv_user         TYPE symsgv.

    DATA:
      lo_tudlv      TYPE REF TO /scwm/cl_sr_tudlv,
      ls_tu_act_key TYPE /scwm/s_tu_sr_act_num,
      lt_tu_act_key TYPE /scwm/tt_tu_sr_act_num.

    DATA: ls_docid_itemid  TYPE /scwm/dlv_docid_item_str,
          lt_docid_itemid  TYPE /scwm/dlv_docid_item_tab,
          lt_docid_itmid_i TYPE /scwm/tt_docid_itmid,
          ls_docid_itmid_i TYPE /scwm/s_docid_itmid,
          lo_tudlv_manager TYPE REF TO /scwm/cl_sr_tudlv.

    FIELD-SYMBOLS: <fs_prd_itm> TYPE /scwm/dlv_item_out_prd_str,
                   <fs_status>  TYPE /scdl/dl_status_str.


    CLEAR lt_docid_itemid.

    lo_tudlv_manager = /scwm/cl_sr_tudlv=>get_instance( ).
    lo_tudlv = /scwm/cl_sr_tudlv=>get_instance( ).
    ls_tu_act_key-tu_num = is_tu_sr_act_num-tu_num.
    ls_tu_act_key-tu_sr_act_num = is_tu_sr_act_num-tu_sr_act_num.
    APPEND ls_tu_act_key TO lt_tu_act_key.
    lo_tudlv->setup( it_tu_act_key = lt_tu_act_key ).

    lo_tudlv_manager->setup( EXPORTING it_tu_act_key = lt_tu_act_key
                                          iv_check_db   = abap_true ).

*Retrieve all delivery items which are assigned directly or indirectly to the TU.
    lo_tudlv_manager->get_dlvi_by_tu(
                           EXPORTING is_tu_act_key  = ls_tu_act_key
                           IMPORTING et_docid_itmid = lt_docid_itmid_i ).

    LOOP AT lt_docid_itmid_i INTO ls_docid_itmid_i.
      ls_docid_itemid-docid = ls_docid_itmid_i-docid.
      ls_docid_itemid-itemid = ls_docid_itmid_i-itmid.
      ls_docid_itemid-doccat = ls_docid_itmid_i-doccat.
      APPEND ls_docid_itemid TO lt_docid_itemid.
    ENDLOOP.

    IF lo_dlv IS NOT BOUND.
      CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
        RECEIVING
          eo_instance = lo_dlv.
    ENDIF.

    ls_read_options-data_retrival_only  = abap_true.
    ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance.
    ls_include_data-item_status = abap_true.

    ls_stat_dyn_det = /scdl/if_dl_status_c=>sc_t_split.
    APPEND ls_stat_dyn_det TO lt_stat_dyn_det.
    ls_include_data-item_status_dyn_detail = lt_stat_dyn_det.

    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            iv_doccat       = wmegc_doccat_pdo
            it_docid        = lt_docid_itemid
            iv_whno         = mv_lgnum
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_items        = lt_prd_itm.
      CATCH /scdl/cx_delivery ##no_handler.
    ENDTRY.

* DLD status is active we use it.
* DLO status is active we use it
* DL and DLO inactive --> old algorithm with picking status.

* Now check all the assigned items to see if the required status are set.
* Accordingly the variable 'lv_open_dlvs' will dictate if pop-up is shown or not.
    IF iv_check_completness EQ abap_true.
      LOOP AT lt_prd_itm ASSIGNING <fs_prd_itm>.
*   Check status loading and distribution
        READ TABLE <fs_prd_itm>-status ASSIGNING <fs_status>
          WITH KEY status_type = c_dl_status_type_dld.
        IF sy-subrc IS INITIAL AND
           ( <fs_status>-status_value = c_dl_status_value_loadinproc OR
             <fs_status>-status_value = c_dl_status_value_loadnotstar ).
          lv_open_dlvs = 'X'.
          EXIT.
        ENDIF.

*   Check status picking
        READ TABLE <fs_prd_itm>-status ASSIGNING <fs_status>
          WITH KEY status_type = c_dl_status_type_dpi.
        IF sy-subrc IS INITIAL AND
           ( <fs_status>-status_value = c_dl_status_value_partly OR
             <fs_status>-status_value = c_dl_status_value_notstarted ).
          lv_open_dlvs = 'X'.
          EXIT.
        ENDIF.

*   Check status distribution
        READ TABLE <fs_prd_itm>-status ASSIGNING <fs_status>
          WITH KEY status_type = c_dl_status_type_dsp.
        IF sy-subrc IS INITIAL AND
           <fs_status>-status_value = c_dl_status_value_partly.
          lv_open_dlvs = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

* check if a delivery exists with not completed picking status or with
* not completed distribution status
      IF lv_open_dlvs IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

* message do you want to set the status of the TU to load complete
    IF /scwm/cl_rf_dynpro_srvc=>display_message(
                  iv_msgid           = '/SCWM/RF_DE'
                  iv_msgty           = 'Q'
                  iv_msgno           = '232'
                  iv_msgv1           = lv_msgv1
                  iv_msgv2           = lv_msgv2 ) =
                  /scwm/cl_rf_bll_srvc=>c_answer_yes.


** yes, go on

*    CHECK gv_tu_num IS NOT INITIAL.
      CHECK is_tu_sr_act_num IS NOT INITIAL.

* check that we have the TU BO
* read transport unit
      IF lo_bom IS NOT BOUND.
        TRY.
            lo_bom = /scwm/cl_sr_bom=>get_instance( ).
          CATCH /scwm/cx_sr_error .
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDTRY.
      ENDIF.

*   fill buffer for TU and corresponding door
      TRY.
          CALL METHOD lo_bom->get_bo_tu_by_key
            EXPORTING
              is_tu_sr_act_num = is_tu_sr_act_num
            RECEIVING
              ro_bo_tu         = lo_bo_tu.
        CATCH /scwm/cx_sr_error .
          rv_completed = abap_false.
          RETURN.
      ENDTRY.

*   check if status is already set
      TRY.
          CALL METHOD lo_bo_tu->get_status_by_id
            EXPORTING
              iv_status_type  = wmesr_status_load_end
            RECEIVING
              ev_status_value = lv_tu_end_load.
        CATCH /scwm/cx_sr_error ##no_handler.
      ENDTRY.


      IF lv_tu_end_load IS INITIAL.
*     set TU status to 'end load'
        TRY.
            lo_bo_tu->set_tu_status_change(
                iv_activity   = wmesr_act_end_load ).
          CATCH /scwm/cx_sr_error .
            IF sy-msgid = 'MC' AND sy-msgno = '601'.
              lv_user   = sy-msgv1.
              lv_locked = abap_true.
              WHILE lv_locked IS NOT INITIAL.
                CLEAR lv_locked.
                IF /scwm/cl_rf_dynpro_srvc=>display_message(
                  iv_msgid           = '/SCWM/RF_DE'
                  iv_msgty           = 'Q'
                  iv_msgno           = '267'
                  iv_msgv1           = lv_user ) =
                  /scwm/cl_rf_bll_srvc=>c_answer_yes.

                  TRY.
                      lo_bo_tu->set_tu_status_change(
                          iv_activity   = wmesr_act_end_load ).
                    CATCH /scwm/cx_sr_error .
                      IF sy-msgid = 'MC' AND sy-msgno = '601'.
                        lv_user   = sy-msgv1.
                        lv_locked = abap_true.
                      ELSE.
*                     To avoid inconsystency in DLV we need to do a rollback and a cleanup.
                        ROLLBACK WORK.
                        CALL METHOD /scwm/cl_tm=>cleanup( ).
                        MESSAGE e207(/scwm/rf_de).
                      ENDIF.
                  ENDTRY.
                ELSE.
                  lv_nosave = abap_true.
                ENDIF.
              ENDWHILE.
            ELSE.
*           To avoid inconsystency in DLV we need to do a rollback and a cleanup.
              ROLLBACK WORK.
              CALL METHOD /scwm/cl_tm=>cleanup( ).
              MESSAGE e207(/scwm/rf_de).
            ENDIF.
        ENDTRY.

        IF lv_nosave = abap_true.
          ROLLBACK WORK.
          CALL METHOD /scwm/cl_tm=>cleanup( ).
        ELSE.

          "save TU
          TRY.
              lo_bom->save( ).
              COMMIT WORK AND WAIT.
              "Set undock from door

              TRY.
                  "save TU

                  lo_bo_tu->set_tu_door_docking(
                    iv_activity     =  wmesr_act_undock_from_door                " Business Transaction
                  ).

                  TRY.
                      lo_bom->save( ).
                    CATCH /scwm/cx_sr_error .
                      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                  ENDTRY.

                CATCH /scwm/cx_sr_error .
                  ROLLBACK WORK.
                  CALL METHOD /scwm/cl_tm=>cleanup( ).
                  MESSAGE e207(/scwm/rf_de).
              ENDTRY.

            CATCH /scwm/cx_sr_error .
              ROLLBACK WORK.
              CALL METHOD /scwm/cl_tm=>cleanup( ).

              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDTRY.

*       commit work
          COMMIT WORK AND WAIT.
          CALL METHOD /scwm/cl_tm=>cleanup( ).
          rv_completed = abap_true.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD to_for_hu_read.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Decision of syncron/asyncron call of loading
**********************************************************************
    IF iv_unnest EQ abap_false AND iv_unnested EQ abap_false. "Syncron call
      to_for_hu_read_perform(
        EXPORTING
          iv_tu_num        = iv_tu_num                        " Transportation Unit for Loading
          iv_read_only     = iv_read_only
          iv_skip_tu_check = iv_skip_tu_check
          iv_door_bin      = iv_door_bin                      " Storage Bin
          iv_lgnum         = iv_lgnum                         " Warehouse Number/Warehouse Complex
        CHANGING
          cv_huident       = cv_huident
          cv_tanum         = cv_tanum
          cv_nlpla         = cv_nlpla
          cv_pt_load       = cv_pt_load
      ).
    ELSE.
      get_hu_hdr(
        EXPORTING
          iv_lgnum   = iv_lgnum                 " Warehouse Number/Warehouse Complex
          iv_huident = cv_huident                  " Handling Unit Identification
        IMPORTING
          et_huhdr   = DATA(lt_huhdr)
      ).


      DATA(lv_qname) = CONV trfcqin-qname( |{ zif_wme_c=>gs_queue-ztuloadhu }{ sy-datum }{ cv_huident ALPHA = OUT }| ).

      CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
        EXPORTING
          qin_name = lv_qname.

      CALL FUNCTION 'Z_RF_PERFORM_DENEST_AND_LOAD' IN BACKGROUND TASK
        EXPORTING
          iv_iproc         = iv_iproc
          iv_tu_num        = iv_tu_num
          iv_read_only     = iv_read_only
          iv_skip_tu_check = iv_skip_tu_check
          iv_door_bin      = iv_door_bin
          iv_lgnum         = iv_lgnum
          iv_syncron       = abap_true
          iv_huident       = cv_huident.
      COMMIT WORK.


      READ TABLE lt_huhdr INTO DATA(ls_bottom_hu)
           WITH KEY bottom = abap_true.
      cv_huident = ls_bottom_hu-huident.
    ENDIF.

  ENDMETHOD.


  METHOD to_for_hu_read_perform.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Loading copied from function module /SCWM/RF_SH_DL_HU_SEL_PAI
**********************************************************************

*- data
    DATA: lv_severity      TYPE bapi_mtype.
    DATA: lt_ordim_o       TYPE /scwm/tt_ordim_o.
    DATA: lv_ltrans        TYPE /scwm/de_ltrans.
    DATA: lv_huident       TYPE /scwm/de_huident.
    DATA: lt_ltap_vb       TYPE /scwm/tt_ltap_vb.
    DATA: lt_create_hu_int TYPE /scwm/tt_to_crea_hu.
    DATA: ls_create_hu_int TYPE /scwm/s_to_crea_hu.
    DATA: lt_bapiret       TYPE bapirettab.
    DATA: lv_create_to     TYPE boole_d.
    DATA: lv_dtu_num       TYPE /scwm/de_tu_num.
    DATA: ls_t340d         TYPE /scwm/t340d.
    DATA: ls_t331          TYPE /scwm/t331.
    DATA: lv_tu_num_ext_msg TYPE /scwm/de_tu_num_ext.
    DATA: lv_scac_msg       TYPE /scmb/mdl_prt_scac.
    DATA: ls_ordim_o_int    TYPE /scwm/s_ordim_o_int.

*- field symbols
    FIELD-SYMBOLS: <fs_ordim_o> TYPE /scwm/ordim_o.
    FIELD-SYMBOLS: <fs_bapiret> TYPE bapiret2.
    FIELD-SYMBOLS: <fs_ltap_vb> TYPE /scwm/ltap.


*------------------------
    lv_huident = cv_huident.

*- read
    CALL FUNCTION '/SCWM/TO_READ_SRC'
      EXPORTING
        iv_lgnum     = iv_lgnum
        iv_huident   = lv_huident
        iv_nobuf     = 'X'
      IMPORTING
        et_ordim_o   = lt_ordim_o
      EXCEPTIONS
        wrong_input  = 1
        not_found    = 2
        foreign_lock = 3
        OTHERS       = 4.
    IF ( sy-subrc NE 0 ) OR ( lt_ordim_o IS INITIAL ).
      lv_create_to = abap_true.
    ENDIF.

*- one HU-TO for the HU?
    SORT lt_ordim_o BY vlenr.
    READ TABLE lt_ordim_o ASSIGNING <fs_ordim_o>
      WITH KEY flghuto = 'X'.
    IF sy-subrc NE 0.
      lv_create_to = abap_true.
    ENDIF.

    IF iv_read_only = abap_true.
      IF lv_create_to = abap_true.
        MESSAGE e017(/scwm/rf_de) WITH lv_huident.     " no WT exists, no WT can be read
      ELSE.
        cv_nlpla = <fs_ordim_o>-nlpla.
        cv_tanum = <fs_ordim_o>-tanum.
      ENDIF.
      RETURN. " nothing more to do
    ENDIF.

*    lv_ltrans  = /scwm/cl_rf_bll_srvc=>get_ltrans( ).
*    IF lv_ltrans IS INITIAL.
    lv_ltrans = 'SHTUSI'.
*    ENDIF.

*- check if TO is meant for complex loading (not for loading by HU)
    IF  lv_create_to =  abap_false AND
        lv_ltrans    <> 'SHHUSI'.
      CALL FUNCTION '/SCWM/IPROC_IN_PRCES_CHECK'
        EXPORTING
          iv_lgnum        = iv_lgnum
          iv_prces        = <fs_ordim_o>-prces
          iv_procs        = <fs_ordim_o>-procs
          iv_iproc        = wmegc_iproc_load
        IMPORTING
          ev_severity     = lv_severity
        EXCEPTIONS
          interface_error = 1
          OTHERS          = 2.
      IF ( ( sy-subrc NE 0 ) OR ( lv_severity <> wmegc_severity_suc ) ).
        CLEAR cv_huident.
        MESSAGE e112(/scwm/rf_de) WITH lv_huident.
      ENDIF.

    ELSEIF lv_create_to =  abap_false AND
        lv_ltrans = 'SHHUSI'.

*   Destination could be TU or door bin
      IF <fs_ordim_o>-nltyp IS NOT INITIAL.
        CALL FUNCTION '/SCWM/T331_READ_SINGLE'
          EXPORTING
            iv_lgnum  = iv_lgnum
            iv_lgtyp  = <fs_ordim_o>-nltyp
          IMPORTING
            es_t331   = ls_t331
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

      IF NOT ( ls_t331-st_role = wmegc_strole_door OR
         <fs_ordim_o>-dtu_num IS NOT INITIAL ).
        MESSAGE e112(/scwm/rf_de) WITH lv_huident.
      ENDIF.

    ENDIF.

    IF  lv_create_to <> abap_true.
*- assign the TO number and to-location
      cv_tanum   = <fs_ordim_o>-tanum.
      cv_nlpla   = <fs_ordim_o>-nlpla.
      lv_dtu_num = <fs_ordim_o>-dtu_num.

    ELSE.
      IF lv_ltrans = 'SHDOSI' OR
         lv_ltrans = 'SHTUSI' .
*     check if RF_Procty for loading set -> loading by TU or Door in 1 step possible
        CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
          EXPORTING
            iv_lgnum  = iv_lgnum
          IMPORTING
            es_t340d  = ls_t340d
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF ls_t340d-pt_load IS NOT INITIAL.
*     "ping"-mode: scaning of destination ("pong") not required for TU and door
          cv_pt_load               = ls_t340d-pt_load.
          ls_create_hu_int-squit   = abap_true.
          ls_create_hu_int-dtu_num = iv_tu_num.
          ls_create_hu_int-nlpla   = iv_door_bin.
          ls_create_hu_int-procty  = ls_t340d-pt_load.
          ls_create_hu_int-norou   = wmegc_norou_yes.
        ENDIF.
      ENDIF.

      ls_create_hu_int-huident = lv_huident.
      APPEND ls_create_hu_int TO lt_create_hu_int.

      CALL FUNCTION '/SCWM/TO_CREA_HU_INT'
        EXPORTING
          iv_bname         = sy-uname
          it_create_hu_int = lt_create_hu_int
        IMPORTING
          et_ltap_vb       = lt_ltap_vb
          et_bapiret       = lt_bapiret
          ev_severity      = lv_severity.

      IF lv_severity CA 'E'.
        ROLLBACK WORK.
        CALL METHOD /scwm/cl_tm=>cleanup( ).
        READ TABLE lt_bapiret ASSIGNING <fs_bapiret>
          WITH KEY type = 'E'.
        IF sy-subrc = 0.
          MESSAGE ID <fs_bapiret>-id
            TYPE     <fs_bapiret>-type
            NUMBER   <fs_bapiret>-number
            WITH     <fs_bapiret>-message_v1
                     <fs_bapiret>-message_v2
                     <fs_bapiret>-message_v3
                     <fs_bapiret>-message_v4.
        ENDIF.
      ENDIF.

      ASSERT ID /scwm/sr_check_err CONDITION lt_ltap_vb IS NOT INITIAL.

      IF lt_ltap_vb IS NOT INITIAL.
        READ TABLE lt_ltap_vb ASSIGNING <fs_ltap_vb> INDEX 1.
        cv_tanum   = <fs_ltap_vb>-tanum.
        cv_nlpla   = <fs_ltap_vb>-nlpla.
        lv_dtu_num = <fs_ltap_vb>-dtu_num.
        IF ls_t340d-pt_load IS     INITIAL.
*-    check if TO is meant for complex loading (not for loading by HU)
          CALL FUNCTION '/SCWM/IPROC_IN_PRCES_CHECK'
            EXPORTING
              iv_lgnum        = iv_lgnum
              iv_prces        = <fs_ltap_vb>-prces
              iv_procs        = <fs_ltap_vb>-procs
              iv_iproc        = wmegc_iproc_load
            IMPORTING
              ev_severity     = lv_severity
            EXCEPTIONS
              interface_error = 1
              OTHERS          = 2.
          IF ( ( sy-subrc NE 0 ) OR ( lv_severity <> wmegc_severity_suc ) ).
            CLEAR cv_huident.
            ROLLBACK WORK.
            CALL METHOD /scwm/cl_tm=>cleanup( ).
            MESSAGE e257(/scwm/rf_de) WITH lv_huident.
          ENDIF.
        ENDIF.

*     S & R check
        IF iv_skip_tu_check = abap_false.
          MOVE-CORRESPONDING <fs_ltap_vb> TO ls_ordim_o_int.
          CALL FUNCTION '/SCWM/SR_CHECK_TO_CONFIRM'
            EXPORTING
              iv_check_only  = abap_true
              is_ordim_o_int = ls_ordim_o_int
            IMPORTING
              et_bapiret     = lt_bapiret
              ev_severity    = lv_severity.
          IF lv_severity CA 'E'.
            ROLLBACK WORK.
            CALL METHOD /scwm/cl_tm=>cleanup( ).

            READ TABLE lt_bapiret ASSIGNING <fs_bapiret>
            WITH KEY type = 'E'.
            IF sy-subrc = 0.
              MESSAGE ID <fs_bapiret>-id
                TYPE     <fs_bapiret>-type
                NUMBER   <fs_bapiret>-number
                WITH     <fs_bapiret>-message_v1
                         <fs_bapiret>-message_v2
                         <fs_bapiret>-message_v3
                         <fs_bapiret>-message_v4.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.
        MESSAGE e222(/scwm/rf_de).
      ENDIF.

    ENDIF. " lv_create_to <> abap_true.

    IF lv_create_to = abap_true.
      CALL FUNCTION '/SCWM/TO_POST'
        IMPORTING
          et_ltap_vb  = lt_ltap_vb
          et_bapiret  = lt_bapiret
          ev_severity = lv_severity.

      IF lv_severity CA 'E'.
        READ TABLE lt_bapiret ASSIGNING <fs_bapiret>
          WITH KEY type = 'E'.
        IF sy-subrc = 0.
          MESSAGE ID <fs_bapiret>-id
            TYPE     <fs_bapiret>-type
            NUMBER   <fs_bapiret>-number
            WITH     <fs_bapiret>-message_v1
                     <fs_bapiret>-message_v2
                     <fs_bapiret>-message_v3
                     <fs_bapiret>-message_v4.
        ENDIF.
      ENDIF.

      IF mv_no_commit EQ abap_false.
        COMMIT WORK AND WAIT.
        CALL METHOD /scwm/cl_tm=>cleanup( ).
      ENDIF.

      IF lt_ltap_vb IS NOT INITIAL.
        READ TABLE lt_ltap_vb ASSIGNING <fs_ltap_vb> INDEX 1.
        cv_tanum   = <fs_ltap_vb>-tanum.
        cv_nlpla   = <fs_ltap_vb>-nlpla.
        lv_dtu_num = <fs_ltap_vb>-dtu_num.
      ENDIF.
    ENDIF.

*- fill the destination bin if only TU is available.
    IF lv_dtu_num IS NOT INITIAL AND
       cv_nlpla   IS     INITIAL.
      CALL FUNCTION '/SCWM/TU_GET_DOOR_NOW'
        EXPORTING
          iv_tu_num   = lv_dtu_num
        IMPORTING
          ev_lgpla    = cv_nlpla
          et_bapiret  = lt_bapiret
          ev_severity = lv_severity
        EXCEPTIONS
          OTHERS      = 4.

      IF sy-subrc <> 0 OR lv_severity = wmegc_severity_err.
        CLEAR cv_huident.

        CALL FUNCTION '/SCWM/TU_GET_EXT_NUM_FROM_INT'
          EXPORTING
            iv_tu_num     = <fs_ordim_o>-dtu_num
          IMPORTING
            ev_tu_num_ext = lv_tu_num_ext_msg
            ev_scac       = lv_scac_msg
          EXCEPTIONS
            tu_not_found  = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        MESSAGE e231(/scwm/rf_de) WITH lv_tu_num_ext_msg lv_scac_msg.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD unassign_hu_from_tu.
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Remove HU assignement from the TU
**********************************************************************
    DATA: lt_out_tu     TYPE /scwm/tt_asp_tu ##needed,
          lt_out_tu_hu  TYPE /scwm/tt_asp_tu_hu ##needed,
          lt_aspk_tu_hu TYPE /scwm/tt_aspk_tu_hu.

    /scwm/cl_tm=>set_lgnum( mv_lgnum ).

    DATA(lo_sp) = NEW /scwm/cl_ui_tu_sp( io_message_handler = so_msg_handler io_attribute_handler = NEW /scmb/cl_attribute_handler( ) ).

    lo_sp->lock(
      EXPORTING
        inkeys       = VALUE /scwm/tt_aspk_tu( ( CORRESPONDING #( is_hu_tu ) ) )                 " Restricting Input Keys
        aspect       = /scwm/if_ui_tu_const=>sc_asp_oip_1                " Aspect Name to Be Read from
        lockmode     = /scdl/cl_sp_prd_inb=>/scdl/if_sp1_locking~sc_exclusive_lock                 " Lock Mode
      IMPORTING
        rejected     = DATA(lv_rejected)                 " Error on Back End
*      return_codes =                  " Table of Return Codes
    ).
    IF lv_rejected EQ abap_false.
      lo_sp->select(
        EXPORTING
          inkeys       =  VALUE /scwm/tt_aspk_tu( ( CORRESPONDING #( is_hu_tu ) ) )                 " Restricting Input Keys
          aspect       =  /scwm/if_ui_tu_const=>sc_asp_oip_1                " Aspect Name to Be Read from
        IMPORTING
          outrecords         = lt_out_tu                 " Changed Aspect Objects
          rejected           = lv_rejected                 " Error on Back End
      ).
    ENDIF.
    IF lv_rejected EQ abap_false.
      APPEND is_hu_tu TO lt_aspk_tu_hu.

      lo_sp->select(
        EXPORTING
          inkeys       =  lt_aspk_tu_hu                " Restricting Input Keys
          aspect       =  /scwm/if_ui_tu_const=>sc_asp_odp1_3 " sc_asp_oip_4                " Aspect Name to Be Read from
        IMPORTING
          outrecords         = lt_out_tu_hu                 " Changed Aspect Objects
          rejected           = lv_rejected  ).               " Error on Back End

      lo_sp->action(
        EXPORTING
          aspect             =  /scwm/if_ui_tu_const=>sc_asp_odp1_3                " Aspect Name
          inkeys             =  lt_aspk_tu_hu
          action             =  /scwm/if_ui_tu_const=>sc_act_tab_change ).                 " Name of Action
      lo_sp->action(
        EXPORTING
          aspect             =  /scwm/if_ui_tu_const=>sc_asp_odp1_3                " Aspect Name
          inkeys             =  lt_aspk_tu_hu
          action             =  /scwm/if_ui_tu_const=>sc_act_asgn_hu_del                 " Name of Action
          relation_inkey     = CORRESPONDING /scwm/s_aspk_tu( is_hu_tu  )
        IMPORTING
          outrecords         = lt_out_tu_hu                 " Changed Aspect Objects
          rejected           = lv_rejected                 " Error on Back End
      ).
    ENDIF.

    IF lv_rejected EQ abap_false.
      lo_sp->save(
        EXPORTING
          synchronously = abap_true
        IMPORTING
          rejected      =  lv_rejected
      ).
    ENDIF.

    IF lv_rejected = abap_false.
      COMMIT WORK AND WAIT.
      /scwm/cl_tm=>cleanup( ).
      rv_ok = abap_true.
    ELSE.
      ROLLBACK WORK.
      /scwm/cl_tm=>cleanup( ).

    ENDIF.
  ENDMETHOD.


  METHOD unnest_hu.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Unnest HU: remove the HUs from the TOP hu
**********************************************************************
    DATA lv_msg TYPE string ##needed.
    CLEAR: et_huident.

    get_hu_hdr(
      EXPORTING
        iv_lgnum   = mv_lgnum
        iv_huident = iv_huident                 " Unique Internal Identification of a Handling Unit
      IMPORTING
        et_huhdr   = DATA(lt_huhdr)                   " Table Type for HU Headers in the Internal Structure
        ev_unnested = DATA(lv_unnested) ).
    IF lt_huhdr IS INITIAL.
      MESSAGE e014(/scwm/rf_de) WITH iv_huident INTO lv_msg.
      add_message( ).
      RETURN.
    ENDIF.
    IF lv_unnested EQ abap_true.
      et_huident = VALUE #( FOR huhdr IN lt_huhdr ( huhdr-huident ) ).
      RETURN.
    ENDIF.


    /scwm/cl_tm=>set_lgnum( /scwm/cl_tm=>sv_lgnum ).

    /scwm/cl_wm_packing=>get_instance(
      IMPORTING
        eo_instance = DATA(lo_packing) ).

    lo_packing->init_pack(
      EXPORTING
        iv_badi_appl  = 'WME'                  " Handling Unit Application
        iv_lgnum      = /scwm/cl_tm=>sv_lgnum
       EXCEPTIONS
         error         = 1                " Error, see log
         OTHERS        = 2
    ).
    IF sy-subrc <> 0.
      add_message( ).
    ENDIF.



    READ TABLE lt_huhdr INTO DATA(ls_huhdr_top)
         WITH KEY top = abap_true.
    LOOP AT lt_huhdr INTO DATA(ls_huhdr)
        WHERE bottom EQ abap_true.
      lo_packing->move_hu(
        EXPORTING
          iv_hu  = ls_huhdr-guid_hu                 " Unique Internal Identification of a Handling Unit
          iv_bin = ls_huhdr_top-lgpla                 " Storage Bin for Stock Transfer
        EXCEPTIONS
          error  = 1                " Error, see log
          OTHERS = 2
      ).
      IF sy-subrc <> 0.
        add_message( ).
        DATA(lv_error) = abap_true.
        CONTINUE.
      ENDIF.
      lo_packing->/scwm/if_pack_bas~hu_ident_remove(
        EXPORTING
          iv_guid_hu = ls_huhdr-guid_hu
          iv_idart   = zif_wme_c=>gs_huidart-h
        EXCEPTIONS
          error      = 0
      ).
      lo_packing->/scwm/if_pack_bas~hu_ident_set(
        EXPORTING
          iv_guid_hu = ls_huhdr-guid_hu
          iv_huident = CONV #( ls_huhdr_top-huident )
          iv_idart   = zif_wme_c=>gs_huidart-h
        EXCEPTIONS
          error      = 1
          OTHERS     = 2
      ).
      IF sy-subrc <> 0.
        add_message( ).
        lv_error = abap_true.
        CONTINUE.
      ENDIF.
      APPEND ls_huhdr-huident TO et_huident.
    ENDLOOP.
    IF lv_error EQ abap_false.
      lo_packing->save(
        EXPORTING
          iv_commit = COND #( WHEN mv_no_commit EQ abap_true THEN abap_false ELSE abap_true )
          iv_wait   = 'X'
        EXCEPTIONS
          error     = 1                " See Log
          OTHERS    = 2
      ).
      IF sy-subrc <> 0.
        add_message( ).
      ENDIF.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
    IF mv_no_commit EQ abap_false.
      /scwm/cl_tm=>cleanup( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
