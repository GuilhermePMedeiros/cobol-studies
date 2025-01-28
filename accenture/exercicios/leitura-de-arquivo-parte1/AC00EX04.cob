       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           AC00EX04.
       AUTHOR.               GUILHERME PACHECO.
      *--------------------------------------------------------------*
      * EXIBIR O CADASTRO DE USUARIOS COM SALDO >= QUE 7000 E
      * FOR DA AGENCIA 1 OU 2 E GERAR ARQUIVO CADMEDIA.
      *--------------------------------------------------------------*
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *--------------------------------------------------------------*
       INPUT-OUTPUT          SECTION.
       FILE-CONTROL.
           SELECT CADAGE   ASSIGN    TO CADAGE
                      FILE STATUS    IS WRK-FS-CADAGE.
      *
           SELECT CADMEDIA ASSIGN    TO CADMEDIA
                      FILE STATUS    IS WRK-FS-CADMEDIA.
      *--------------------------------------------------------------*
       DATA                  DIVISION.
       FILE                  SECTION.
      *--------------------------------------------------------------*
      *    ARQUIVO CADAGE
      *--------------------------------------------------------------*
       FD  CADAGE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      *
       01  FD-CADAGE-REGISTRO     PIC X(056).
      *
      *--------------------------------------------------------------*
      *    ARQUIVO CADMEDIA
      *--------------------------------------------------------------*
       FD  CADMEDIA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      *
       01  FD-CADMEDIA-REGISTRO   PIC X(056).
      *
      *--------------------------------------------------------------*
       WORKING-STORAGE       SECTION.
      *--------------------------------------------------------------*
       77  WRK-CONT-LIDOS       PIC 9(003).
       77  WRK-CONT-PROC        PIC 9(003).
       77  WRK-CONT-IGN         PIC 9(003).
      *
       01  WRK-AREA-FILE-STATUS.
            05 WRK-FS-CADAGE     PIC X(002) VALUE SPACES.
            05 WRK-FS-CADMEDIA   PIC X(002) VALUE SPACES.
      *--------------------------------------------------------------*
      * LAYOUT ARQUIVO ENTRADA CADAGE
      *--------------------------------------------------------------*
       01  WRK-REGISTRO-CADAGE.
            03 AGENCIA-E         PIC X(03).
            03 COD-E             PIC 9(02).
            03 NOME-E            PIC X(40).
            03 SALDOM-E          PIC 9(09)V99.
      *--------------------------------------------------------------*
      * LAYOUT ARQUIVO SAIDA CADMEDIA
      *--------------------------------------------------------------*
       01  WRK-REGISTRO-CADMEDIA.
            03 AGENCIA-S         PIC X(03).
            03 COD-S             PIC 9(02).
            03 NOME-S            PIC X(40).
            03 SALDOM-S          PIC 9(09)V99.
      *--------------------------------------------------------------*
       PROCEDURE             DIVISION.
      *--------------------------------------------------------------*
       000-PRINCIPAL.
           PERFORM 010-INICIALIZAR
           PERFORM 030-PROCESSAR UNTIL WRK-FS-CADAGE = "10"
           PERFORM 050-FINALIZAR
           GOBACK.
      *
       010-INICIALIZAR.
           DISPLAY "PROGRAMA AC00EX04 INICIADO"
           PERFORM 011-INICIALIZAR-VARIAVEIS
           PERFORM 012-ABRIR-ARQUIVOS
           PERFORM 013-LER-CADAGE
           IF WRK-FS-CADAGE = "10"
              DISPLAY "ARQUIVO CADAGE VAZIO"
           END-IF.
      *
       011-INICIALIZAR-VARIAVEIS.
JUH   *       ADICIONAR ZEROS NAS VARIAVEIS DE CONTADORES
              INITIALIZE WRK-CONT-IGN.
              INITIALIZE WRK-CONT-LIDOS.
              INITIALIZE WRK-CONT-PROC.
      *
       012-ABRIR-ARQUIVOS.
           OPEN INPUT    CADAGE
           IF WRK-FS-CADAGE NOT = ZEROS
              DISPLAY "ERRO ABERTURA CADAGE - FS: " WRK-FS-CADAGE
              PERFORM 999-ROTINA-ABEND
           END-IF.
      *
           OPEN OUTPUT  CADMEDIA
           IF WRK-FS-CADMEDIA NOT = ZEROS
              DISPLAY "ERRO ABERTURA CADMEDIA - FS: " WRK-FS-CADMEDIA
              PERFORM 999-ROTINA-ABEND
           END-IF.
      *
       013-LER-CADAGE.
           INITIALIZE WRK-REGISTRO-CADAGE.
           READ CADAGE    INTO WRK-REGISTRO-CADAGE.
           IF WRK-FS-CADAGE NOT = "00" AND "10"
              DISPLAY "ERRO LEITURA CADAGE - FS: " WRK-FS-CADAGE
              PERFORM 999-ROTINA-ABEND
           END-IF.
           IF WRK-FS-CADAGE = "00"
JUH   *       ADICIONAR 1 NO CONTADOR DE PROCESSADOS - (LIDOS NA VERDADE)
              ADD 1 TO WRK-CONT-LIDOS
           END-IF.

      *
       030-PROCESSAR.
JUH   *    COLOCAR SEU CODIGO AQUI
           IF (AGENCIA-E = 'A01' OR AGENCIA-E = 'A02') 
              AND SALDOM-E >= 7000 
           THEN
              PERFORM 031-MOVER-DADOS-CADMEDIA
              PERFORM 032-GRAVAR-CADMEDIA
           ELSE
              ADD 1 TO WRK-CONT-IGN
           END-IF.
           PERFORM 013-LER-CADAGE.
      *
       031-MOVER-DADOS-CADMEDIA.
           MOVE AGENCIA-E     TO AGENCIA-S.
           MOVE COD-E         TO COD-S.
           MOVE NOME-E        TO NOME-S.
           MOVE SALDOM-E      TO SALDOM-S.
      *
       032-GRAVAR-CADMEDIA.
           WRITE FD-CADMEDIA-REGISTRO FROM WRK-REGISTRO-CADMEDIA
           IF WRK-FS-CADMEDIA NOT = ZEROS
              DISPLAY "ERRO GRAVACAO CADMEDIA - FS: " WRK-FS-CADMEDIA
              PERFORM 999-ROTINA-ABEND
           END-IF.
           IF WRK-FS-CADMEDIA = ZEROS
JUH   *       ADICIONAR 1 NO CONTADOR DE PROCESSADOS
              ADD 1 TO WRK-CONT-PROC
           END-IF.
      *
       050-FINALIZAR.
           PERFORM 051-MOSTRAR-CONTADORES.
           PERFORM 052-FECHAR-ARQUIVOS.

      *
       051-MOSTRAR-CONTADORES.
JUH   *    COLOCAR OS DISPLAYS DOS CONTADORES
           DISPLAY "Quantidade de Registros Lidos........: " 
                    WRK-CONT-LIDOS.
           DISPLAY "Quantidade de Registro Processados: " WRK-CONT-PROC.
           DISPLAY "Quantidade de Registro Ignorados...: " WRK-CONT-IGN.

      *
       052-FECHAR-ARQUIVOS.
           CLOSE CADAGE
           IF WRK-FS-CADAGE NOT = "00"
              DISPLAY "ERRO CLOSE CADAGE - FS: " WRK-FS-CADAGE
              PERFORM 999-ROTINA-ABEND
           END-IF.
      *
           CLOSE CADMEDIA
           IF WRK-FS-CADMEDIA NOT = "00"
              DISPLAY "ERRO CLOSE CADMEDIA - FS: " WRK-FS-CADMEDIA
              PERFORM 999-ROTINA-ABEND
           END-IF.
           
      *
       999-ROTINA-ABEND.
           DISPLAY "ABEND DO PROGRAMA - AC00EX04".
           GOBACK.
