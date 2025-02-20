      *--------------------------------------------------------------*
       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           AC00EX06.
       AUTHOR.               GUILHERME PACHECO.
      *--------------------------------------------------------------*
      * LER ARQUIVOS CADCLI E ALTCLI E GERAR CADCLIN, DADAS AS
      * CONDICOES SE O CADASTRO FOR INCLUSO, ALTERADO OU EXCLUIDO
      *--------------------------------------------------------------*
       ENVIRONMENT           DIVISION.
      *--------------------------------------------------------------*
       CONFIGURATION         SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *--------------------------------------------------------------*
       INPUT-OUTPUT          SECTION.
      *--------------------------------------------------------------*
      *
       FILE-CONTROL.
      *     
           SELECT CADCLI   ASSIGN    TO CADCLI
                      FILE STATUS    IS WRK-FS-CADCLI.
      *
           SELECT ALTCLI   ASSIGN    TO ALTCLI
                      FILE STATUS    IS WRK-FS-ALTCLI.
      *
GUI   *    INCLUIR O SELECT DO ARQUIVO CADCLIN
           SELECT CADCLIN  ASSIGN    TO CADCLIN
                      FILE STATUS    IS WRK-FS-CADCLIN.
      *--------------------------------------------------------------*
       DATA                  DIVISION.
       FILE                  SECTION.
      *--------------------------------------------------------------*
      *    ARQUIVO CADCLI
      *--------------------------------------------------------------*
       FD  CADCLI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      *
GUI   *AJUSTAR O TAMANHO DO ARQUIVO CONFORME DOCUMENTO WORD
       01  FD-CADCLI-REGISTRO     PIC X(076).
      *
      *--------------------------------------------------------------*
      *    ARQUIVO ALTCLI
      *--------------------------------------------------------------*
       FD  ALTCLI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      *
GUI   *AJUSTAR O TAMANHO DO ARQUIVO CONFORME DOCUMENTO WORD
       01  FD-ALTCLI-REGISTRO     PIC X(076).
      *
      *--------------------------------------------------------------*
      *    ARQUIVO CADCLIN
      *--------------------------------------------------------------*
       FD  CADCLIN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      *
GUI   *AJUSTAR O TAMANHO DO ARQUIVO CONFORME DOCUMENTO WORD
       01  FD-CADCLIN-REGISTRO    PIC X(089).
      *
      *--------------------------------------------------------------*
       WORKING-STORAGE       SECTION.
      *--------------------------------------------------------------*
       01  WRK-CONTADORES.
            03  WRK-CONT-LIDOS-CAD   PIC 9(003) VALUE ZEROS.
            03  WRK-CONT-LIDOS-ALT   PIC 9(003) VALUE ZEROS.
            03  WRK-CONT-GRAVADOS    PIC 9(003) VALUE ZEROS.
            03  WRK-CONT-ALTERADOS   PIC 9(003) VALUE ZEROS.
            03  WRK-CONT-EXCLUIDOS   PIC 9(003) VALUE ZEROS.
      *
       01  WRK-AREA-FILE-STATUS.
            05 WRK-FS-CADCLI     PIC X(002) VALUE SPACES.
            05 WRK-FS-ALTCLI     PIC X(002) VALUE SPACES.
GUI   *    INCLUIR A VARIAVEL DE FS DO ARQUIVO CADCLIN
            05 WRK-FS-CADCLIN    PIC X(002) VALUE SPACES.
      *
       01  WRK-DATA-SIS.
           05 WRK-ANO-SIS           PIC 9(04).
           05 WRK-MES-SIS           PIC 9(02).
           05 WRK-DIA-SIS           PIC 9(02).
      *
       01  WRK-HORA-SIS.
           05 WRK-HOR-SIS           PIC 9(02).
           05 WRK-MIN-SIS           PIC 9(02).
           05 WRK-SEG-SIS           PIC 9(02).
      *
       01  WRK-FORMATAR-DATA-SIS.
           05 WRK-VAL-DIA-SIS       PIC 9(02).
           05 FILLER                PIC X(01) VALUE "/".
           05 WRK-VAL-MES-SIS       PIC 9(02).
           05 FILLER                PIC X(01) VALUE "/".
           05 WRK-VAL-ANO-SIS       PIC 9(04).
      *
       01  WRK-FORMATAR-HORA-SIS.
           05 WRK-VAL-HORA-SIS      PIC 9(02).
           05 FILLER                PIC X(01) VALUE ":".
           05 WRK-VAL-MIN-SIS       PIC 9(02).
           05 FILLER                PIC X(01) VALUE ":".
           05 WRK-VAL-SEG-SIS       PIC 9(02).
      *
GUI   *DEFINIR O LAYOUT DO ARQUIVO CADCLI CONFORME DOCUMENTO WORD
      *
       01  WRK-CADCLI-REGISTRO.                            
           05 CADCLI-CHAVE.
                07 CADCLI-AGENCIA            PIC  9(005).
                07 CADCLI-NUMERO-CONTA       PIC  9(010).
           05 CADCLI-NOME-CLIENTE            PIC  X(040).    
           05 CADCLI-TELEFONE                PIC  9(008). 
           05 CADCLI-SALDO-ATUAL             PIC S9(011)V99.

GUI   *DEFINIR O LAYOUT DO ARQUIVO ALTCLI CONFORME DOCUMENTO WORD
      *
       01  WRK-ALTCLI-REGISTRO.                            
           05 ALTCLI-CHAVE.
                07 ALTCLI-AGENCIA            PIC  9(005).
                07 ALTCLI-NUMERO-CONTA       PIC  9(010).
           05 ALTCLI-NOME-CLIENTE            PIC  X(040).    
           05 ALTCLI-TELEFONE                PIC  9(008). 
           05 ALTCLI-RENDA-MENSAL            PIC  S9(011)V99. 


GUI   *DEFINIR O LAYOUT DO ARQUIVO CADCLIN CONFORME DOCUMENTO WORD
      * 
       01  WRK-CADCLIN-REGISTRO.                            
           05 CADCLIN-CHAVE.
               07 CADCLIN-AGENCIA           PIC  9(005).
               07 CADCLIN-NUMERO-CONTA      PIC  9(010).
           05 CADCLIN-NOME-CLIENTE          PIC  X(040). 
           05 CADCLIN-TELEFONE              PIC  9(008).
           05 CADCLIN-RENDA-MENSAL          PIC S9(011)V99. 
           05 CADCLIN-SALDO-ATUAL           PIC S9(011)V99.

      *--------------------------------------------------------------*
       PROCEDURE             DIVISION.
      *--------------------------------------------------------------*
       000-AC00EX06.
           PERFORM 010-INICIALIZAR.
GUI   *    AJUSTAR O UNTIL PARA PROCESSAR ATE O FIM DO ARQUIVO 1 E ARQUIVO 2
           PERFORM 030-PROCESSAR 
             UNTIL WRK-FS-CADCLI = 10 AND WRK-FS-ALTCLI = 10.
           PERFORM 050-FINALIZAR.
           GOBACK.
      *
       010-INICIALIZAR.
           DISPLAY "PROGRAMA AC00EX06 INICIADO"
           PERFORM 011-INICIALIZAR-VARIAVEIS.
           PERFORM 060-FORMATA-DATA.
           PERFORM 061-FORMATA-HORA.
           PERFORM 062-MOSTRAR-DATA-HORA.
GUI   *    CHAMAR O PARAGRAFO DE ABRIR ARQUIVOS
           PERFORM 012-ABRIR-ARQUIVOS.

           PERFORM 033-LER-CADCLI.
GUI   *    INCLUIR VALIDACAO SE O CADCLI ESTA VAZIO
           IF WRK-FS-CADCLI = "10"
              DISPLAY "ARQUIVO CADCLI VAZIO"
           END-IF.

           PERFORM 034-LER-ALTCLI
GUI   *    INCLUIR VALIDACAO SE O ALTCLI ESTA VAZIO
           IF WRK-FS-ALTCLI = "10"
              DISPLAY "ARQUIVO ALTCLI VAZIO"
           END-IF.

      *
       011-INICIALIZAR-VARIAVEIS.
           INITIALIZE WRK-CONTADORES
           ACCEPT  WRK-DATA-SIS FROM DATE YYYYMMDD.
           ACCEPT  WRK-HORA-SIS FROM TIME.
      *
       012-ABRIR-ARQUIVOS.
GUI   *    INCLUIR OPEN DO ARQUIVO CADCLI
      *
           OPEN INPUT    CADCLI.
           IF WRK-FS-CADCLI NOT = ZEROS
                DISPLAY "ERRO ABERTURA CADCLI - FS: " WRK-FS-CADCLI
                PERFORM 999-ROTINA-ABEND
           END-IF.


           OPEN INPUT    ALTCLI.
           IF WRK-FS-ALTCLI NOT = ZEROS
              DISPLAY "ERRO ABERTURA ALTCLI - FS: " WRK-FS-ALTCLI
              PERFORM 999-ROTINA-ABEND
           END-IF.
      *
           OPEN OUTPUT  CADCLIN.
           IF WRK-FS-CADCLIN NOT = ZEROS
              DISPLAY "ERRO ABERTURA CADCLIN - FS: " WRK-FS-CADCLIN
              PERFORM 999-ROTINA-ABEND
           END-IF.

      *
       030-PROCESSAR.
GUI   * ESCREVER A LOGICA DO PROCESSAR CONFORME DOCUMENTO EM ANEXO

           IF CADCLI-CHAVE = ALTCLI-CHAVE
               PERFORM 031-MOVER-DADOS-ALTCLI
               PERFORM 032-GRAVAR-CADCLIN

               ADD 1 TO WRK-CONT-ALTERADOS 
               PERFORM 033-LER-CADCLI
               PERFORM 034-LER-ALTCLI
           ELSE
                IF CADCLI-CHAVE > ALTCLI-CHAVE
                    PERFORM 031-MOVER-DADOS-ALTCLI
                    PERFORM 032-GRAVAR-CADCLIN

                    ADD 1 TO WRK-CONT-GRAVADOS
                    PERFORM 034-LER-ALTCLI
                ELSE 
                  ADD 1 TO WRK-CONT-EXCLUIDOS
                  PERFORM 033-LER-CADCLI
                END-IF
           END-IF.
           
      *
       031-MOVER-DADOS-ALTCLI.
GUI   * MOVIMENTAR OS DADOS DO ARQUIVO ALTCLI PARA O ARQUIVO CADCLIN
GUI   * ESSE PARAGRAFO DEVE SER ACIONADO DENTRO DO 030-PROCESSAR QUANDO
GUI   * CADCLI-CHAVE = ALTCLI-CHAVE OU CADCLI-CHAVE > ALTCLI-CHAVE
           MOVE ALTCLI-CHAVE TO CADCLIN-CHAVE.
           MOVE ALTCLI-NOME-CLIENTE TO CADCLIN-NOME-CLIENTE.
           MOVE ALTCLI-TELEFONE TO CADCLIN-TELEFONE.
           MOVE ALTCLI-RENDA-MENSAL TO CADCLIN-RENDA-MENSAL.
           IF CADCLI-CHAVE = ALTCLI-CHAVE
               MOVE CADCLI-SALDO-ATUAL TO CADCLIN-SALDO-ATUAL
           ELSE
               INITIALIZE CADCLIN-SALDO-ATUAL
           END-IF.

      *
       032-GRAVAR-CADCLIN.
           WRITE FD-CADCLIN-REGISTRO FROM WRK-CADCLIN-REGISTRO.
           IF WRK-FS-CADCLIN NOT = ZEROS
              DISPLAY "ERRO GRAVACAO CADCLIN - FS: " WRK-FS-CADCLIN
              PERFORM 999-ROTINA-ABEND
           END-IF.
           
      *
       033-LER-CADCLI.
           READ CADCLI    INTO WRK-CADCLI-REGISTRO.
           IF WRK-FS-CADCLI NOT = "00" AND "10"
              DISPLAY "ERRO LEITURA CADCLI - FS: " WRK-FS-CADCLI
              PERFORM 999-ROTINA-ABEND
           END-IF.
           IF WRK-FS-CADCLI = "00"
GUI   *    ADICIONAR CONTADOR
                ADD 1 TO WRK-CONT-LIDOS-CAD
           END-IF.
           IF WRK-FS-CADCLI = "10"
              MOVE ALL "9" TO CADCLI-CHAVE
           END-IF.
           
      *
       034-LER-ALTCLI.
           READ ALTCLI    INTO WRK-ALTCLI-REGISTRO.
           IF WRK-FS-ALTCLI NOT = "00" AND "10"
              DISPLAY "ERRO LEITURA ALTCLI - FS: " WRK-FS-ALTCLI
              PERFORM 999-ROTINA-ABEND
           END-IF.
           IF WRK-FS-ALTCLI = "00"
GUI   *    ADICIONAR CONTADOR
                ADD 1 TO WRK-CONT-LIDOS-ALT
           END-IF.
           IF WRK-FS-ALTCLI = "10"
              MOVE ALL "9" TO ALTCLI-CHAVE
           END-IF.

      *
       050-FINALIZAR.
JUH   *    CHAMAR OS PARAGRAFOS DE FINALIZACAO
           PERFORM 051-MOSTRAR-CONTADORES.
           PERFORM 052-FECHAR-ARQUIVOS.
      *
       051-MOSTRAR-CONTADORES.
GUI   *    MONTAR O DISPLAY DOS CONTADORES CONFORME WORD
           DISPLAY "TOTAL REGISTROS LIDOS CADCLI........: " 
               WRK-CONT-LIDOS-CAD.
           DISPLAY "TOTAL REGISTROS LIDOS ALTCLI........: "
               WRK-CONT-LIDOS-ALT.
           DISPLAY "TOTAL REGISTROS INCLUIDOS (CADCLIN).: "
               WRK-CONT-GRAVADOS.
           DISPLAY "TOTAL REGISTROS ALTERADOS (CADCLIN).: "
               WRK-CONT-ALTERADOS.
           DISPLAY "TOTAL REGISTROS EXCLUIDOS...........: "
               WRK-CONT-EXCLUIDOS.
      * 
       052-FECHAR-ARQUIVOS.
           CLOSE CADCLI.
           IF WRK-FS-CADCLI NOT = "00"
              DISPLAY "ERRO CLOSE CADCLI - FS: " WRK-FS-CADCLI
              PERFORM 999-ROTINA-ABEND
           END-IF.
      *
GUI   *    INCLUIR O CLOSE DO ARQUIVO ALTCLI
      *
           CLOSE ALTCLI.
           IF WRK-FS-ALTCLI NOT = "00"
              DISPLAY "ERRO CLOSE ALTCLI - FS: " WRK-FS-ALTCLI
              PERFORM 999-ROTINA-ABEND
           END-IF.

           CLOSE CADCLIN
           IF WRK-FS-CADCLIN NOT = "00"
              DISPLAY "ERRO CLOSE CADCLIN - FS: " WRK-FS-CADCLIN
              PERFORM 999-ROTINA-ABEND
           END-IF.
          
      *
       060-FORMATA-DATA.
           MOVE WRK-ANO-SIS TO WRK-VAL-ANO-SIS.
           MOVE WRK-MES-SIS TO WRK-VAL-MES-SIS.
           MOVE WRK-DIA-SIS TO WRK-VAL-DIA-SIS.
           
      *
       061-FORMATA-HORA.
           MOVE WRK-HOR-SIS TO WRK-VAL-HORA-SIS.
           MOVE WRK-MIN-SIS TO WRK-VAL-MIN-SIS.
           MOVE WRK-SEG-SIS TO WRK-VAL-SEG-SIS.
          
      *
       062-MOSTRAR-DATA-HORA.
           DISPLAY "DATA E HORA DO SISTEMA: " WRK-FORMATAR-DATA-SIS " -
      -              "" WRK-FORMATAR-HORA-SIS.
     
      *
       999-ROTINA-ABEND.
           DISPLAY "ABEND DO PROGRAMA - AC00EX06".
           GOBACK.

      
