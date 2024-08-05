*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_PICDSTSEQ...................................*
TABLES: ZMV_PICDSTSEQ, *ZMV_PICDSTSEQ. "view work areas
CONTROLS: TCTRL_ZMV_PICDSTSEQ
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_PICDSTSEQ. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_PICDSTSEQ.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_PICDSTSEQ_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_PICDSTSEQ.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_PICDSTSEQ_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_PICDSTSEQ_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_PICDSTSEQ.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_PICDSTSEQ_TOTAL.

*.........table declarations:.................................*
TABLES: ZTRFUI_PICDSTSEQ               .
