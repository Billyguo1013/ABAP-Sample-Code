*&---------------------------------------------------------------------*
*& INCLUDE          ZRPUR032_AI01
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
