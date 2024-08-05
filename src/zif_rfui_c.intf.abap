INTERFACE zif_rfui_c
  PUBLIC .


  CONSTANTS:
    BEGIN OF gs_fcode,
      pbo1   TYPE /scwm/de_fcode VALUE 'PBO1',
      pbo2   TYPE /scwm/de_fcode VALUE 'PBO2',
      next   TYPE /scwm/de_fcode VALUE 'NEXT',
      huput  TYPE /scwm/de_fcode VALUE 'LTXHUP', "leave Transaction, put HU
      gtnwhu TYPE /scwm/de_fcode VALUE 'GTNWHU',
      wosplt TYPE /scwm/de_fcode VALUE 'LTXSWO', "leave Transaction, split WHO
      cmpltx TYPE /scwm/de_fcode VALUE /scwm/cl_rf_bll_srvc=>c_fcode_compl_ltrans,
      zpcful TYPE /scwm/de_fcode VALUE 'ZPCFUL',
      skip   TYPE /scwm/de_fcode VALUE 'SKIP',
      skipwo TYPE /scwm/de_fcode VALUE 'SKIPWO',
      return TYPE /scwm/de_fcode VALUE 'RETURN',
      miprsl TYPE /scwm/de_fcode VALUE 'MIPRSL',
      miqusl TYPE /scwm/de_fcode VALUE 'MIQUSL',
      mihusl TYPE /scwm/de_fcode VALUE 'MIHUSL',
      confto TYPE /scwm/de_fcode VALUE 'CONFTO',
      delsn  TYPE /scwm/de_fcode VALUE 'DELSN',
      gtsncl TYPE /scwm/de_fcode VALUE 'GTSNCL',
      save   TYPE /scwm/de_fcode VALUE 'SAVE',
      split  TYPE /scwm/de_fcode VALUE 'SPLIT',
      bindpb TYPE /scwm/de_fcode VALUE 'BINDPB',
      zsnerr TYPE /scwm/de_fcode VALUE 'ZSNERR',
      zhuful TYPE /scwm/de_fcode VALUE 'ZHUFUL',
      zgtchu TYPE /scwm/de_fcode VALUE 'ZGTCHU',
      zgtcpe TYPE /scwm/de_fcode VALUE 'ZGTCPE',
      zgtpwl TYPE /scwm/de_fcode VALUE 'ZGTPWL',
      zprest TYPE /scwm/de_fcode VALUE 'ZPREST',
      uldlen TYPE /scwm/de_fcode VALUE 'ULDLEN',
      huinfo TYPE /scwm/de_fcode VALUE 'HUINFO',
      hulist TYPE /scwm/de_fcode VALUE 'HULIST',
      wodetl TYPE /scwm/de_fcode VALUE 'WODETL',
      zpcrto TYPE /scwm/de_fcode VALUE 'ZPCRTO',
      binent TYPE /scwm/de_fcode VALUE 'BINENT',
    END OF gs_fcode .
  CONSTANTS:
    BEGIN OF gs_state,
      general TYPE /scwm/de_state VALUE '******',
      intlmv  TYPE /scwm/de_state VALUE 'INTLMV',
      hufull  TYPE /scwm/de_state VALUE 'HUFULL',
      pick    TYPE /scwm/de_state VALUE 'PICK',
    END OF gs_state .

  CONSTANTS:
    BEGIN OF gs_ltrans,
      zpicar TYPE /scwm/de_ltrans VALUE 'ZPICAR',
      zpicrb TYPE /scwm/de_ltrans VALUE 'ZPICRB',
    END OF gs_ltrans.

  CONSTANTS:
    BEGIN OF gs_step,
      zpicar TYPE /scwm/de_step VALUE 'ZPICAR',
      zpichu TYPE /scwm/de_step VALUE 'ZPICHU',
      zpicpe TYPE /scwm/de_step VALUE 'ZPICPE',
      zpipwl TYPE /scwm/de_step VALUE 'ZPIPWL',
      ulcmpl TYPE /scwm/de_step VALUE 'ULCMPL',
    END OF gs_step.

  CONSTANTS c_part_repl_memory TYPE char20 VALUE 'ZPART_REPL' ##NO_TEXT.
ENDINTERFACE.
