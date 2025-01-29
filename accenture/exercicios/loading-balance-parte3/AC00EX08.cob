       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           AC00EX08.
       AUTHOR.               GUILHERME PACHECO.

       ENVIRONMENT           DIVISION.
      
       CONFIGURATION         SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      
       INPUT-OUTPUT          SECTION.
       FILE-CONTROL.
      
       SELECT CADCLIE   ASSIGN    TO CADCLIE
                      FILE STATUS    IS WRK-FS-CADCLIE.

       SELECT CADPROD   ASSIGN    TO CADPROD
                      FILE STATUS    IS WRK-FS-CADPROD.
 
       SELECT CLISEGU   ASSIGN    TO CLISEGU
                      FILE STATUS    IS WRK-FS-CLISEGU.

       SELECT RELCLI    ASSIGN    TO RELCLI 
                      FILE STATUS    IS WRK-FS-RELCLI.

       DATA                  DIVISION.
       FILE                  SECTION.
    
       FD  CADCLIE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-CADCLIE-REGISTRO     PIC X(067).

       FD  CADPROD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-CADPROD-REGISTRO     PIC X(061).


       FD  CLISEGU
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-CLISEGU-REGISTRO     PIC X(101).


       FD  RELCLI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-RELCLI-REGISTRO     PIC X(059).
    
       WORKING-STORAGE       SECTION.

       01  WRK-AREA-FILE-STATUS.
           05 WRK-FS-CADCLIE     PIC X(002) VALUE SPACES.
           05 WRK-FS-CADPROD     PIC X(002) VALUE SPACES.
           05 WRK-FS-CLISEGU     PIC X(002) VALUE SPACES.
           05 WRK-FS-RELCLI      PIC X(002) VALUE SPACES.
       
       01 WRK-CADCLIE-REGISTRO.
           05 CADCLIE-CHAVE.
             07 CADCLIE-COD-CLIENTE PIC 9(005).
           05 CADCLIE-NOME-CLIENTE  PIC X(040).
           05 CADCLIE-TELEFONE      PIC X(011).
           05 CADCLIE-RENDA-MENSAL  PIC S9(009)V99.

       01 WRK-CADPROD-REGISTRO.
           05 CADPROD-CHAVE.
             07 CADPROD-COD-CLIENTE PIC 9(005).
           05 CADPROD-COD-PRODUTO   PIC 9(005).
           05 CADPROD-NOME-PRODUTO  PIC X(040).
           05 CADPROD-VALOR-PRODUTO PIC S9(009)V99.  

       01 WRK-CLISEGU-REGISTRO.
           05 CLISEGU-COD-CLIENTE   PIC 9(005).
           05 CLISEGU-NOME-CLIENTE  PIC X(040).
           05 CLISEGU-COD-PRODUTO   PIC 9(005).
           05 CLISEGU-NOME-PRODUTO  PIC X(040).
           05 CLISEGU-VALOR-PRODUTO PIC S9(009)V99.

       01 WRK-RELCLI-REGISTRO.
           05 RELCLI-COD-CLIENTE  PIC 9(005).
           05 FILLER              PIC X(1) VALUE ";".
           05 RELCLI-NOME-CLIENTE PIC X(040).
           05 FILLER              PIC X(1) VALUE ";".
           05 RELCLI-RENDA-MENSAL PIC -999.999.999,99.
       
       01  WRK-DATA-SIS.
           05 WRK-ANO-SIS           PIC 9(04).
           05 WRK-MES-SIS           PIC 9(02).
           05 WRK-DIA-SIS           PIC 9(02).
      
       01  WRK-HORA-SIS.
           05 WRK-HOR-SIS           PIC 9(02).
           05 WRK-MIN-SIS           PIC 9(02).
           05 WRK-SEG-SIS           PIC 9(02).
      
       01  WRK-FORMATAR-DATA-SIS.
           05 WRK-VAL-DIA-SIS       PIC 9(02).
           05 FILLER                PIC X(01) VALUE "/".
           05 WRK-VAL-MES-SIS       PIC 9(02).
           05 FILLER                PIC X(01) VALUE "/".
           05 WRK-VAL-ANO-SIS       PIC 9(04).
      
       01  WRK-FORMATAR-HORA-SIS.
           05 WRK-VAL-HORA-SIS      PIC 9(02).
           05 FILLER                PIC X(01) VALUE ":".
           05 WRK-VAL-MIN-SIS       PIC 9(02).
           05 FILLER                PIC X(01) VALUE ":".
           05 WRK-VAL-SEG-SIS       PIC 9(02).

       01  WRK-CONTADORES.
            03  WRK-CONT-LIDOS-CADCLIE     PIC 9(003) VALUE ZEROS.
            03  WRK-CONT-LIDOS-CADPROD     PIC 9(003) VALUE ZEROS.
            03  WRK-CONT-GRAVADOS-CLISEGU  PIC 9(003) VALUE ZEROS.
            03  WRK-CONT-GRAVADOS-RELCLI   PIC 9(003) VALUE ZEROS.
            03  WRK-CONT-CLI-PRODUTOS      PIC 9(003) VALUE ZEROS.

       PROCEDURE             DIVISION.
       
       000-AC00EX08.
           PERFORM 010-INICIALIZAR.
           PERFORM 020-PROCESSAR 
             UNTIL WRK-FS-CADCLIE = "10" AND WRK-FS-CADPROD = "10".
           PERFORM 050-FINALIZAR.
           GOBACK.

       010-INICIALIZAR.
           DISPLAY "PROGRAMA AC00EX08 INICIADO"
           PERFORM 011-INICIALIZAR-VARIAVEIS.
           PERFORM 060-FORMATA-DATA.
           PERFORM 061-FORMATA-HORA.
           PERFORM 062-MOSTRAR-DATA-HORA.
           PERFORM 012-ABRIR-ARQUIVOS.

           PERFORM 041-LER-CADCLIE.
           IF WRK-FS-CADCLIE = "10"
              DISPLAY "ARQUIVO CADCLIE VAZIO"
           END-IF.

           PERFORM 042-LER-CADPROD.
           IF WRK-FS-CADPROD = "10"
              DISPLAY "ARQUIVO CADPROD VAZIO"
           END-IF.

       011-INICIALIZAR-VARIAVEIS.
           INITIALIZE WRK-CONTADORES.
           ACCEPT  WRK-DATA-SIS FROM DATE YYYYMMDD.
           ACCEPT  WRK-HORA-SIS FROM TIME.
      
       012-ABRIR-ARQUIVOS.
           OPEN INPUT    CADCLIE.
           IF WRK-FS-CADCLIE NOT = ZEROS
                DISPLAY "ERRO ABERTURA CADCLIE - FS: " WRK-FS-CADCLIE
                PERFORM 999-ROTINA-ABEND
           END-IF.

           OPEN INPUT    CADPROD.
           IF WRK-FS-CADPROD NOT = ZEROS
                DISPLAY "ERRO ABERTURA CADPROD - FS: " WRK-FS-CADPROD
                PERFORM 999-ROTINA-ABEND
           END-IF.
       
           OPEN OUTPUT  CLISEGU.
           IF WRK-FS-CLISEGU NOT = ZEROS
              DISPLAY "ERRO ABERTURA CLISEGU - FS: " WRK-FS-CLISEGU
              PERFORM 999-ROTINA-ABEND
           END-IF.

           OPEN OUTPUT  RELCLI.
           IF WRK-FS-RELCLI NOT = ZEROS
              DISPLAY "ERRO ABERTURA RELCLI - FS: " WRK-FS-RELCLI
              PERFORM 999-ROTINA-ABEND
           END-IF.    
      
       020-PROCESSAR.
           IF CADCLIE-CHAVE = CADPROD-CHAVE
               IF CADPROD-COD-PRODUTO = 00005 OR 00009
                      PERFORM 031-MOVER-DADOS-CLISEGU 
                      PERFORM 043-GRAVAR-CLISEGU

                      ADD 1 TO WRK-CONT-CLI-PRODUTOS
               END-IF

               PERFORM 042-LER-CADPROD
           ELSE
               IF CADCLIE-CHAVE > CADPROD-CHAVE
                   PERFORM 042-LER-CADPROD
               ELSE
                   IF WRK-CONT-CLI-PRODUTOS = 0
                     PERFORM 032-MOVER-DADOS-RELCLI
                     PERFORM 044-GRAVAR-RELCLI
                   END-IF

                   PERFORM 041-LER-CADCLIE
                   INITIALIZE WRK-CONT-CLI-PRODUTOS  
               END-IF
           END-IF.    

      
       031-MOVER-DADOS-CLISEGU.
           MOVE CADCLIE-COD-CLIENTE TO CLISEGU-COD-CLIENTE.
           MOVE CADCLIE-NOME-CLIENTE TO CLISEGU-NOME-CLIENTE.
           MOVE CADPROD-COD-PRODUTO TO CLISEGU-COD-PRODUTO.
           MOVE CADPROD-NOME-PRODUTO TO CLISEGU-NOME-PRODUTO.
           MOVE CADPROD-VALOR-PRODUTO TO CLISEGU-VALOR-PRODUTO.
           
       032-MOVER-DADOS-RELCLI.    
           MOVE CADCLIE-COD-CLIENTE TO RELCLI-COD-CLIENTE.
           MOVE CADCLIE-NOME-CLIENTE TO RELCLI-NOME-CLIENTE.
           MOVE CADCLIE-RENDA-MENSAL TO RELCLI-RENDA-MENSAL.

       041-LER-CADCLIE.
           READ CADCLIE    INTO WRK-CADCLIE-REGISTRO.
           IF WRK-FS-CADCLIE NOT = "00" AND "10"
              DISPLAY "ERRO LEITURA CADCLIE - FS: " WRK-FS-CADCLIE
              PERFORM 999-ROTINA-ABEND
           END-IF.
           IF WRK-FS-CADCLIE = "00"
                ADD 1 TO WRK-CONT-LIDOS-CADCLIE
           END-IF.
           IF WRK-FS-CADCLIE = "10"
              MOVE ALL "9" TO CADCLIE-CHAVE
           END-IF.
           
       042-LER-CADPROD.
           READ CADPROD    INTO WRK-CADPROD-REGISTRO.
           IF WRK-FS-CADPROD NOT = "00" AND "10"
              DISPLAY "ERRO LEITURA CADPROD - FS: " WRK-FS-CADPROD
              PERFORM 999-ROTINA-ABEND
           END-IF.
           IF WRK-FS-CADPROD = "00"
                ADD 1 TO WRK-CONT-LIDOS-CADPROD
           END-IF.
           IF WRK-FS-CADPROD = "10"
              MOVE ALL "9" TO CADPROD-CHAVE
           END-IF.

       043-GRAVAR-CLISEGU.
           WRITE FD-CLISEGU-REGISTRO FROM WRK-CLISEGU-REGISTRO.
           IF WRK-FS-CLISEGU NOT = ZEROS
              DISPLAY "ERRO GRAVACAO CLISEGU - FS: " WRK-FS-CLISEGU
              PERFORM 999-ROTINA-ABEND
           END-IF.

           ADD 1 TO WRK-CONT-GRAVADOS-CLISEGU.

       044-GRAVAR-RELCLI.
           WRITE FD-RELCLI-REGISTRO FROM WRK-RELCLI-REGISTRO.
           IF WRK-FS-RELCLI NOT = ZEROS
              DISPLAY "ERRO GRAVACAO RELCLI - FS: " WRK-FS-RELCLI
              PERFORM 999-ROTINA-ABEND
           END-IF.
           
           ADD 1 TO WRK-CONT-GRAVADOS-RELCLI.



       050-FINALIZAR.
           PERFORM 051-MOSTRAR-CONTADORES.
           PERFORM 052-FECHAR-ARQUIVOS.

       051-MOSTRAR-CONTADORES.
           DISPLAY "TOTAL REGISTROS LIDOS CADCLIE.....: " 
               WRK-CONT-LIDOS-CADCLIE.
           DISPLAY "TOTAL REGISTROS LIDOS CADPROD.....: "
               WRK-CONT-LIDOS-CADPROD.
           DISPLAY "TOTAL REGISTROS GRAVADOS CLISEGU..: "
               WRK-CONT-GRAVADOS-CLISEGU.
           DISPLAY "TOTAL REGISTROS GRAVADOS RELCLI...: "
               WRK-CONT-GRAVADOS-RELCLI.

       052-FECHAR-ARQUIVOS.
           CLOSE CADCLIE.
           IF WRK-FS-CADCLIE NOT = "00"
              DISPLAY "ERRO CLOSE CADCLIE - FS: " WRK-FS-CADCLIE
              PERFORM 999-ROTINA-ABEND
           END-IF.

           CLOSE CADPROD.
           IF WRK-FS-CADPROD NOT = "00"
              DISPLAY "ERRO CLOSE CADPROD - FS: " WRK-FS-CADPROD
              PERFORM 999-ROTINA-ABEND
           END-IF.

           CLOSE CLISEGU.
           IF WRK-FS-CLISEGU NOT = "00"
              DISPLAY "ERRO CLOSE CLISEGU - FS: " WRK-FS-CLISEGU
              PERFORM 999-ROTINA-ABEND
           END-IF.

           CLOSE RELCLI.
           IF WRK-FS-RELCLI NOT = "00"
              DISPLAY "ERRO CLOSE RELCLI - FS: " WRK-FS-RELCLI
              PERFORM 999-ROTINA-ABEND
           END-IF.
          
       060-FORMATA-DATA.
           MOVE WRK-ANO-SIS TO WRK-VAL-ANO-SIS.
           MOVE WRK-MES-SIS TO WRK-VAL-MES-SIS.
           MOVE WRK-DIA-SIS TO WRK-VAL-DIA-SIS.
           
       061-FORMATA-HORA.
           MOVE WRK-HOR-SIS TO WRK-VAL-HORA-SIS.
           MOVE WRK-MIN-SIS TO WRK-VAL-MIN-SIS.
           MOVE WRK-SEG-SIS TO WRK-VAL-SEG-SIS.
          
       062-MOSTRAR-DATA-HORA.
           DISPLAY "DATA E HORA DO SISTEMA: " WRK-FORMATAR-DATA-SIS " -
      -              "" WRK-FORMATAR-HORA-SIS.
     
       999-ROTINA-ABEND.
           DISPLAY "ABEND DO PROGRAMA - AC00EX08".
           GOBACK.
           
