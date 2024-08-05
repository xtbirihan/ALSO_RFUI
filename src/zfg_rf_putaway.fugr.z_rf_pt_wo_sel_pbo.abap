FUNCTION z_rf_pt_wo_sel_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"     REFERENCE(CS_ADMIN) TYPE  /SCWM/S_RF_ADMIN
*"     REFERENCE(CT_PTWY) TYPE  /SCWM/TT_RF_PTWY
*"     REFERENCE(ZCS_PARTIAL_REPLENISHMENT) TYPE
*"        ZSTR_RF_PARTIAL_REPLENISHMENT
*"----------------------------------------------------------------------
*********************************************************************
*& Key           : LH-190423
*& Request No.   : GAP-004 – Display Partial Replenisment
**********************************************************************
*& Description (short)
*& Detect partial replenishment
*&
**********************************************************************
  DATA: lv_who_repl TYPE /scwm/s_rf_ptwy-who.
* Initialize interface parameters
  CLEAR: cs_ptwy, cs_admin.

* Introduce the parameters
  CALL METHOD /scwm/cl_rf_bll_srvc=>init_screen_param.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'CS_PTWY'.

  BREAK-POINT ID /scwm/rf_putaway.


* read default values
  PERFORM lgnum_rsrc_get IN PROGRAM /scwm/saplrf_putaway
          CHANGING cs_admin-lgnum
                   cs_admin-rsrc
                   cs_admin-rsrc_grp
                   cs_admin-pos_manag.

* Check if putaway is called from system-guided work
  DATA: lv_work_who     TYPE /scwm/de_who,
        lv_work_who_int TYPE i.
* Get warehouse order parameter
  GET PARAMETER ID '/SCWM/WORK_WHO' FIELD lv_work_who.
* move to help-variable, because lv_work_who = ' 000000000' and one missing '0'
* with help-variable it is possible to check for initial

*  the move to an integer could cause short dump if the HU is quite long
*  MOVE lv_work_who TO lv_work_who_int.
  IMPORT who = lv_who_repl FROM MEMORY ID zif_rfui_c=>c_part_repl_memory.
  IF lv_who_repl EQ lv_work_who.
    zcs_partial_replenishment-ptwosl_call = abap_true.
    zcs_partial_replenishment-pw_header = 'Restorage Remaining Quantity'(res).
    FREE MEMORY ID 'ZPART_REPL'.
  ELSE.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
        iv_screlm_name = 'ZCS_PARTIAL_REPLENISHMENT-PW_HEADER' ).
  ENDIF.

  IF lv_work_who CN ' 0'.
*   Called from system-guided work -> suppress selection screen
    /scwm/cl_rf_bll_srvc=>set_prmod('1').
    /scwm/cl_rf_bll_srvc=>set_fcode('ENTER').
*   Set warehouse order in internal used structures
    cs_ptwy-who = lv_work_who.
  ELSE.
*   Display selection screen
    /scwm/cl_rf_bll_srvc=>set_prmod('2').
  ENDIF.

  SET PARAMETER ID '/SCWM/WORK_WHO' FIELD space.

* no rebundling necessary, if entry-point = Warehouse Order
  cs_admin-rebundled = 'X'.

ENDFUNCTION.
