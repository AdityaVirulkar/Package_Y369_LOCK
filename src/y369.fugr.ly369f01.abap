*----------------------------------------------------------------------*
***INCLUDE LY369F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_FUGR_FOR_NAMESPACE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COMPLETE_AREA  text
*      <--P_L_NAMESPACE  text
*      <--P_L_GROUP  text
*----------------------------------------------------------------------*
form f_check_fugr_for_namespace using    p_complete_area
                                changing p_namespace
                                         p_group  .
  data: l_include like rs38l-include,
        l_c_namespace like rs38l-include,
        l_length type i.

  l_include = p_complete_area.

* Check for use of namespace
  if l_include cs '/'.
    if l_include(1) = '/'.

* A name area should exist
      shift l_include left by 1 places.

      if l_include ca '/'.
* End delimiter identified
        add 1 to sy-fdpos.
        shift l_include left by sy-fdpos places.
        add 1 to sy-fdpos.

* Identify namespace
        l_c_namespace = p_complete_area(sy-fdpos).
        l_length = strlen( l_c_namespace ).

        if l_length > 10.
* Namespace is too long
          exit.
        else.
          p_namespace = p_complete_area(sy-fdpos).
          p_group = l_include.
        endif.

      else.
* Too few delimiters
        exit.
      endif.
    else.
* Delimiter is in the wrong position
      exit.
    endif.
* No namespace identified; area is function group name
  else.
    p_group = p_complete_area.
  endif.
endform.                    " F_CHECK_FUGR_FOR_NAMESPACE

*&---------------------------------------------------------------------*
*&      Form  F_FIND_VALUE                                             *
*&---------------------------------------------------------------------*
*       Determine the value of a DATA variable or CONSTANT
*----------------------------------------------------------------------*
*      -->P_TOKEN_ITAB2  Table containing source code tokens
*      <--P_TOKEN        Token string
*----------------------------------------------------------------------*
form f_find_value tables   p_token_itab2 structure stokex
                  changing p_token       like      p_token_itab2-str.
  data: l_pos like sy-tabix,
        l_max like sy-tabix,
        l_len type i,
        l_token like p_token,
        l_token2 like p_token.
  constants:  c_squote   type c value ''''.    "Single quote

* Prepare variables
  l_token = p_token.
  clear p_token.
  l_len = strlen( l_token ).

* Determine the number of tokens in the source code
  describe table p_token_itab2 lines l_max.

* Process the DATA variable declaration and CONSTANT declaration tokens
  loop at p_token_itab2 where str = 'CONSTANTS'
                           or str = 'DATA'.

* Retrieve the source following the declaration
    l_pos = sy-tabix + 1.
    read table p_token_itab2 index l_pos.

* Get the name of the variable or constant
    l_token2 = p_token_itab2-str(l_len).

* When the variable/constant equals the passed token, cycle through the
* subsequent tokens in an attempt to find the "VALUE" token.
    if l_token2 eq l_token.
      do 3 times.
        add 1 to l_pos.
        read table p_token_itab2 index l_pos.
        check sy-subrc eq 0.
        if p_token_itab2-str = 'VALUE'.
          add 1 to l_pos.
          read table p_token_itab2 index l_pos.
          check sy-subrc eq 0.
* When the token following the "VALUE" token is a string literal,
* the value of the variable or constant has been determined.
          if p_token_itab2-type eq 'S'.
            p_token = p_token_itab2-str.
            shift p_token left.
            replace
            c_squote with space into p_token.
          endif.
          exit.
        endif.
      enddo.
      exit.
    endif.
  endloop.

* tcode not determined
  if p_token is initial.
    p_token = 'UNKNOWN'.
  endif.

endform.                               " F_FIND_VALUE
