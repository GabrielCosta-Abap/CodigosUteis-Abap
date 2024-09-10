
report zZMONTA_SHDB.

constants: c_%bdc(4) value '%BDC',
           c_msg_in(50) value '',"'*&BOBWIZARD: Início - Criado Automaticamente',
           c_msg_fn(50) value ''."'*&BOBWIZARD: Fim - Criado Automaticamente'.
tables apqi.                           " Definição da informação da fila
parameters: p_grpid like apqi-groupid obligatory,
            p_retsb as checkbox default 'X'.
* Identificação da fila SHDB (Chave única)
data: p_qid like apqi-qid.data: gt_dynprotab like bdcdata occurs 0 with header line.
data: begin of ti_source occurs 0,
        line(255),
      end of ti_source.
*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_grpid.                   *
*---------------------------------------------------------------------*
* Popula e/ou valida a variável p_grpid(Nome do SHDB)                 *
*---------------------------------------------------------------------*
at selection-screen on value-request for p_grpid.

  perform f_choose_shdb using 'GROUPID'
                              'P_GRPID'
                     changing p_grpid.

  check not p_grpid is initial.

  select qid into p_qid from apqi up to 2 rows
                       where groupid = p_grpid
                         and mandant = sy-mandt
                         and datatyp = c_%bdc.
  endselect.

  check sy-dbcnt = 2.

  perform f_choose_shdb using 'QID' ''
                     changing p_qid.
*---------------------------------------------------------------------*
* AT SELECTION-SCREEN                                                 *
*---------------------------------------------------------------------*
* Valida a existência do SHDB e após isso, popula o qid (Cód. Fila)   *
*---------------------------------------------------------------------*
at selection-screen.

  if not p_qid is initial.
*   SHDB existe?
    select single qid into p_qid from apqi
                         where qid     = p_qid
                           and groupid = p_grpid
                           and mandant = sy-mandt
                           and datatyp = c_%bdc.
    check sy-subrc ne 0.
  endif.
* Registro SHDB existe?
  select single qid into p_qid from apqi
                       where groupid = p_grpid
                         and mandant = sy-mandt
                         and datatyp = c_%bdc.

  check sy-subrc ne 0.
* Gravação não disponível
  message i627(ms) with p_grpid.
  stop.
*---------------------------------------------------------------------*
* START-OF-SELECTION.                                                 *
*---------------------------------------------------------------------*
* Seleciona os registros do SHDB                                      *
*---------------------------------------------------------------------*
start-of-selection.
* Lê o registro SHDB
  call function 'BDC_OBJECT_READ'
    exporting
      queue_id         = p_qid
    tables
      dynprotab        = gt_dynprotab
    exceptions
      not_found        = 1
      system_failure   = 2
      invalid_datatype = 3
      others           = 4.

  perform separador using 'DECLARAÇÕES DE VARIÁVEIS'.
  perform monta_declaracoes_var.
  perform separador using 'FORMS'.
  perform monta_forms.
  perform separador using 'FORM DE CHAMADA DO CALL TRANSACTION'.
  perform monta_form_ct.


  editor-call for ti_source.
*---------------------------------------------------------------------*
*      Form  F_CHOOSE_SHDB
*---------------------------------------------------------------------*
form f_choose_shdb using u_value1 type fieldname
                         u_value2 type dynfnam
                changing u_val.

  types:
    begin of ty_s_values,
      qid      type apq_quid,
      groupid  type apq_grpn,
      creator  type apq_mapn,
      credate  type apq_crda,
      cretime  type apq_crti,
      transcnt type apq_tran,
      msgcnt   type apq_reco,
    end of ty_s_values.

  data:
    lt_values  type table of ty_s_values,
    lt_return  type hrreturn_tab with header line,
    l_progname type sy-repid,
    l_dynnum   type sy-dynnr.

  l_progname = sy-repid.
  l_dynnum   = sy-dynnr.

  if u_value1 = 'GROUPID'.
    select *
      from apqi
      into corresponding fields of table lt_values
     where datatyp = c_%bdc
       and mandant = sy-mandt.
  else.
    select *
      from apqi
      into corresponding fields of table lt_values
     where datatyp = c_%bdc
       and mandant = sy-mandt
       and groupid = p_grpid.
  endif.
* F4 help
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = u_value1
      dynpprog        = l_progname
      dynpnr          = l_dynnum
      dynprofield     = u_value2
      value_org       = 'S'
    tables
      value_tab       = lt_values
      return_tab      = lt_return
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  check not lt_return[] is initial.
  read table lt_return index 1.
  u_val = lt_return-fieldval.endform.                               " F_CHOOSE_SHDB
*&---------------------------------------------------------------------*
*&      Form  MONTA_DECLARACOES_VAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form monta_forms .
  perform adiciona_linha using:  c_msg_in,
                                 'FORM dynpro USING dynbegin name value.',
                                 '  IF dynbegin = ''X''.',
                                 '    CLEAR bdc_tab.',
                                 '    MOVE: name  TO bdc_tab-program,',
                                 '          value TO bdc_tab-dynpro,',
                                 '          ''X'' TO bdc_tab-dynbegin.',
                                 '    APPEND bdc_tab.',
                                 '  ELSE.',
                                 '    CLEAR bdc_tab.',
                                 '    MOVE: name TO bdc_tab-fnam,',
                                 '          value TO bdc_tab-fval.',
                                 '    APPEND bdc_tab.',
                                 '  ENDIF.',
                                 'ENDFORM.',
                                 c_msg_fn.
  perform adiciona_linha using: ''.
endform.                    " MONTA_DECLARACOES_VAR
*&---------------------------------------------------------------------*
*&      Form  MONTA_DECLARACOES_VAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form monta_declaracoes_var .
  perform adiciona_linha using: c_msg_in,
                                '"Variáveis para CALL TRANSACTION',
                                'DATA: BEGIN OF bdc_tab OCCURS 0.',
                                '        INCLUDE STRUCTURE bdcdata.',
                                'DATA: END OF bdc_tab.',
                                '',
                                'DATA: wa_opcoes TYPE ctu_params.',
                                '',
                                'DATA: BEGIN OF messtab OCCURS 0.',
                                'INCLUDE STRUCTURE bdcmsgcoll.',
                                'DATA: END OF messtab.',
                                c_msg_fn.
  perform adiciona_linha using: ''.
endform.                    " MONTA_DECLARACOES_VAR
*&---------------------------------------------------------------------*
*&      Form  adiciona_linha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LINE     text
*----------------------------------------------------------------------*
form adiciona_linha using p_line.
  clear ti_source.
  ti_source-line = p_line.
  append ti_source.endform.                    " ADICIONA_LINHA
*&---------------------------------------------------------------------*
*&      Form  SEPARADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form separador using comentario.
  data: s_comentario(255).
  s_comentario = comentario.
  concatenate '*&' comentario into s_comentario.
  perform adiciona_linha using: '*&---------------------------------------------------------------------*',
                                s_comentario,
                                '*&---------------------------------------------------------------------*'.
endform.                    " SEPARADOR
*&---------------------------------------------------------------------*
*&      Form  MONTA_FORM_CT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form monta_form_ct .
  data: linha(255).
  perform adiciona_linha using c_msg_in.

  perform adiciona_linha using 'FORM chamada_ct.'.
  perform adiciona_linha using '  REFRESH: bdc_tab, messtab.'.

  "Loop na tabela do SHDB
  loop at gt_dynprotab.
    if gt_dynprotab-dynbegin eq 'T' or
       ( p_retsb eq 'X' and
         gt_dynprotab-fnam eq 'BDC_SUBSCR' ).
      continue.
    endif.

    linha = '  PERFORM dynpro USING'.
    concatenate '''' gt_dynprotab-dynbegin '''' into linha+24.

    if gt_dynprotab-dynbegin eq 'X'.
      perform adiciona_linha using space.
      concatenate '''' gt_dynprotab-program '''' into linha+30.
      concatenate '''' gt_dynprotab-dynpro '''.' into linha+59.
    else.
      concatenate '''' gt_dynprotab-fnam '''' into linha+30.
      concatenate '''' gt_dynprotab-fval '''.' into linha+59.
    endif.

    perform adiciona_linha using linha.
  endloop.

  "Opções do CT
  perform adiciona_linha using: ''.
  perform adiciona_linha using: '*Opções do CALL TRANSACTION'.
  perform adiciona_linha using: '  CLEAR wa_opcoes.',
                                '    wa_opcoes-dismode  = ''A''.',
                                '    wa_opcoes-updmode  = ''A''.',
                                '    wa_opcoes-cattmode = '' ''.',
                                '    wa_opcoes-racommit = '' ''.',
                                '    wa_opcoes-nobinpt  = '' ''.',
                                '    wa_opcoes-nobiend  = ''X''.'.
  perform adiciona_linha using: ''.

  "Chamada ao CALL TRANSACTION
  perform adiciona_linha using: '*Chamada ao CALL TRANSACTION'.
  read table gt_dynprotab with key dynbegin = 'T'.
  linha = '  CALL TRANSACTION'.

  concatenate '''' gt_dynprotab-fnam '''' into linha+19.
  concatenate linha 'USING bdc_tab OPTIONS FROM wa_opcoes MESSAGES INTO messtab.' into linha separated by space.
  perform adiciona_linha using: linha.
  perform adiciona_linha using '  LOOP AT messtab.'.
  perform adiciona_linha using '  ENDLOOP.'.
  perform adiciona_linha using 'ENDFORM.'.
  perform adiciona_linha using c_msg_fn.endform.                    " MONTA_FORM_CT
