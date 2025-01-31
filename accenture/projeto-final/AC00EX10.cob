       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           AC00EX08.
       AUTHOR.               GUILHERME PACHECO.

       ENVIRONMENT           DIVISION.
      
       CONFIGURATION         SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      
       INPUT-OUTPUT          SECTION.
       FILE-CONTROL.
      
      *IMPORTANDO OS ARQUIVOS E CONECTANDO COM OS FILES STATUS 
       SELECT CLIENTE   ASSIGN    TO CLIENTE
                      FILE STATUS    IS WRK-FS-CLIENTE.              

       SELECT COMPRA   ASSIGN    TO COMPRA
                      FILE STATUS    IS WRK-FS-COMPRA.
       
       SELECT COMPROD   ASSIGN    TO COMPROD
                      FILE STATUS    IS WRK-FS-COMPROD.

       SELECT PRODUTO   ASSIGN    TO PRODUTO
                      FILE STATUS    IS WRK-FS-PRODUTO.

       SELECT BOLCHE   ASSIGN    TO BOLCHE
                      FILE STATUS    IS WRK-FS-BOLCHE.
       
       SELECT DEBCRE   ASSIGN    TO DEBCRE
                      FILE STATUS    IS WRK-FS-DEBCRE.

       SELECT RELTOT   ASSIGN    TO RELTOT
                      FILE STATUS    IS WRK-FS-RELTOT.

       DATA                  DIVISION.
       FILE                  SECTION.

      *CRIANDO O FD DOS ARQUIVOS 
       FD  CLIENTE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-CLIENTE-REGISTRO     PIC X(023).
    
       FD  COMPRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-COMPRA-REGISTRO     PIC X(016).

       FD  COMPROD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-COMPROD-REGISTRO     PIC X(013).

       FD  PRODUTO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-PRODUTO-REGISTRO     PIC X(026).

       FD  BOLCHE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-BOLCHE-REGISTRO     PIC X(048).

       FD  DEBCRE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-DEBCRE-REGISTRO     PIC X(048).

       FD  RELTOT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      
       01  FD-RELTOT-REGISTRO     PIC X(056).

       WORKING-STORAGE       SECTION.

      * VARIAVEIS DE FILE STATUS
       01  WRK-AREA-FILE-STATUS.
           05 WRK-FS-CLIENTE        PIC X(002) VALUE SPACES.
           05 WRK-FS-COMPRA         PIC X(002) VALUE SPACES.
           05 WRK-FS-COMPROD        PIC X(002) VALUE SPACES.
           05 WRK-FS-PRODUTO        PIC X(002) VALUE SPACES.
           05 WRK-FS-BOLCHE         PIC X(002) VALUE SPACES.
           05 WRK-FS-DEBCRE         PIC X(002) VALUE SPACES.
           05 WRK-FS-RELTOT         PIC X(002) VALUE SPACES.
       
      *LAYOUT DE CLIENTE
       01  WRK-CLIENTE-REGISTRO.                            
           05 CLIENTE-COD-CLINTE	PIC  9(003).
           05 CLIENTE-NOME		    PIC  X(020).    
       
      *LAYOUT DE COMPRA
       01  WRK-COMPRA-REGISTRO.                            
           05 COMPRA-COD-COMPRA	    PIC  9(005).  
           05 COMPRA-TIPO-PAGTO	    PIC  X(008). 
           05 COMPRA-COD-CLIENTE	PIC  9(003). 
      
      *LAYOUT DE COMPROD
       01  WRK-COMPROD-REGISTRO.                            
           05 COMPROD-COD-COMPRA	PIC  9(005).
           05 COMPROD-COD-PRODUTO	PIC  9(005).    
           05 COMPROD-QUANTIDADE	PIC  9(003). 
      
      *LAYOUT DE PRODUTO 
       01  WRK-PRODUTO-REGISTRO.                            
           05 PRODUTO-COD-PRODUTO	PIC  9(005).
           05 PRODUTO-NOME		    PIC  X(010).    
           05 PRODUTO-VALOR		    PIC  9(009)V99. 

      *LAYOUT DE BOLCHE 
       01  WRK-BOLCHE-REGISTRO.                            
           05 BOLCHE-COD-COMPRA	    PIC  9(005).
           05 BOLCHE-NOME-PRODUTO	PIC  X(010).    
           05 BOLCHE-TIPO-PAGTO	    PIC  X(008).
           05 BOLCHE-VALOR 		    PIC  9(009)V99.
           05 BOLCHE-QUANTIDADE  	PIC  9(003).
           05 BOLCHE-VALOR-LIQUIDO	PIC  9(009)V99.
       
      *LAYOUT DE DEBCRE 
       01  WRK-DEBCRE-REGISTRO.                            
           05 DEBCRE-COD-COMPRA	    PIC  9(005). 
           05 DEBCRE-NOME-PRODUTO	PIC  X(010).    
           05 DEBCRE-TIPO-PAGTO	    PIC  X(008).
           05 DEBCRE-VALOR 		    PIC  9(009)V99.
           05 DEBCRE-QUANTIDADE	    PIC  9(003).
           05 DEBCRE-VALOR-LIQUIDO	PIC  9(009)V99.

      *LAYOUT DE RELTOT 
       01  WRK-RELTOT-REGISTRO.                            
           05 RELTOT-COD-COMPRA	    PIC  9(005).
           05 FILLER			    PIC  X(001) VALUE ";".
           05 RELTOT-NOME-CLIENTE	PIC  X(020). 
           05 FILLER			    PIC  X(001) VALUE ";".
           05 RELTOT-TOT-VALOR-BTO	PIC  ZZZ.ZZZ.Z99,99.
           05 FILLER			    PIC  X(001) VALUE ";".
           05 RELTOT-TOT-VALOR-LIQ	PIC  ZZZ.ZZZ.Z99,99.

      *DATA E HORA DO SISTEMA.
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

      *VARIAVEIS DE CONTADORES.
       01  WRK-CONTADORES.
           03 WRK-CONT-LIDOS-CLIENTE      PIC 9(02) VALUE ZEROS.
           03 WRK-CONT-LIDOS-COMPRA       PIC 9(02) VALUE ZEROS.
           03 WRK-CONT-LIDOS-COMPROD      PIC 9(02) VALUE ZEROS.
           03 WRK-CONT-LIDOS-PRODUTO      PIC 9(02) VALUE ZEROS.
           03 WRK-CONT-GRAVADOS-BOLCHE    PIC 9(02) VALUE ZEROS.
           03 WRK-CONT-GRAVADOS-DEBCRE    PIC 9(02) VALUE ZEROS.
           03 WRK-CONT-GRAVADOS-RELTOT    PIC 9(02) VALUE ZEROS.

      *VARIAVEIS DE CONTADORES PERFORMACE
       01  WRK-CONTADORES-PERFORMACE.
           03 WRK-CONT-PER      PIC 9(02) VALUE ZEROS.
               

      *TABELAS INTERNAS
       01  TAB-COMPROD-REGISTRO OCCURS 8 TIMES.                            
           05 TAB-COMPROD-COD-COMPRA	PIC  9(005).
           05 TAB-COMPROD-COD-PRODUTO	PIC  9(005).    
           05 TAB-COMPROD-QUANTIDADE	PIC  9(003). 
      
       01  TAB-PRODUTO-REGISTRO OCCURS 5 TIMES.                            
           05 TAB-PRODUTO-COD-PRODUTO	PIC  9(005).
           05 TAB-PRODUTO-NOME		    PIC  X(010).    
           05 TAB-PRODUTO-VALOR		    PIC  9(009)V99. 

       PROCEDURE             DIVISION.
       
       000-AC00EX10-APP.
           PERFORM 010-INICIALIZAR.
           PERFORM 020-PROCESSAR 
             UNTIL WRK-FS-CLIENTE = 10 AND WRK-FS-COMPRA = 10.
           PERFORM 050-FINALIZAR.
           GOBACK.

       010-INICIALIZAR.
           DISPLAY "PROGRAMA AC00EX10 INICIADO"
           PERFORM 011-INICIALIZAR-VARIAVEIS.
           PERFORM 060-FORMATA-DATA.
           PERFORM 061-FORMATA-HORA.
           PERFORM 062-MOSTRAR-DATA-HORA.

           PERFORM 012-ABRIR-ARQUIVOS.
           PERFORM 040-LER-ARQUIVOS.
           
           PERFORM 013-CARREGAR-TABELAS.

       011-INICIALIZAR-VARIAVEIS.
           INITIALIZE WRK-CONTADORES.
           INITIALIZE WRK-CONTADORES-PERFORMACE.
           ACCEPT  WRK-DATA-SIS FROM DATE YYYYMMDD.
           ACCEPT  WRK-HORA-SIS FROM TIME.
      
       012-ABRIR-ARQUIVOS.
           OPEN INPUT    CLIENTE.
           IF WRK-FS-CLIENTE NOT = ZEROS
                DISPLAY "ERRO ABERTURA CLIENTE - FS: " WRK-FS-CLIENTE
                PERFORM 999-ROTINA-ABEND
           END-IF.

           OPEN INPUT    COMPRA.
           IF WRK-FS-COMPRA NOT = ZEROS
                DISPLAY "ERRO ABERTURA COMPRA - FS: " WRK-FS-COMPRA
                PERFORM 999-ROTINA-ABEND
           END-IF.
       
           OPEN INPUT    COMPROD.
           IF WRK-FS-COMPROD NOT = ZEROS
                DISPLAY "ERRO ABERTURA COMPROD - FS: " WRK-FS-COMPROD
                PERFORM 999-ROTINA-ABEND
           END-IF.
           
           OPEN INPUT    PRODUTO.
           IF WRK-FS-PRODUTO NOT = ZEROS
                DISPLAY "ERRO ABERTURA PRODUTO - FS: " WRK-FS-PRODUTO
                PERFORM 999-ROTINA-ABEND
           END-IF.

           OPEN OUTPUT  BOLCHE.
           IF WRK-FS-BOLCHE NOT = ZEROS
              DISPLAY "ERRO ABERTURA BOLCHE - FS: " WRK-FS-BOLCHE
              PERFORM 999-ROTINA-ABEND
           END-IF.

           OPEN OUTPUT  DEBCRE.
           IF WRK-FS-DEBCRE NOT = ZEROS
              DISPLAY "ERRO ABERTURA DEBCRE - FS: " WRK-FS-DEBCRE
              PERFORM 999-ROTINA-ABEND
           END-IF.    
      
           OPEN OUTPUT  RELTOT.
           IF WRK-FS-RELTOT NOT = ZEROS
              DISPLAY "ERRO ABERTURA RELTOT - FS: " WRK-FS-RELTOT
              PERFORM 999-ROTINA-ABEND
           END-IF. 
       
       013-CARREGAR-TABELAS.
           PERFORM 014-CARRREGAR-TABELA-COMPROD.
           PERFORM 015-CARREGAR-TABELA-PRODUTO.

       014-CARRREGAR-TABELA-COMPROD.
           PERFORM 031-MOVER-DADOS-TAB-COMPROD 
           VARYING WRK-CONT-PER
           FROM 0 BY 1
           UNTIL WRK-FS-COMPROD = 10.
            
       015-CARREGAR-TABELA-PRODUTO.
           PERFORM 032-MOVER-DADOS-TAB-PRODUTO 
           VARYING WRK-CONT-PER
           FROM 0 BY 1
           UNTIL WRK-FS-PRODUTO = 10.
           
       020-PROCESSAR.
               IF COMPRA-COD-CLIENTE = CLIENTE-COD-CLINTE THEN
                   
                   DISPLAY CLIENTE-COD-CLINTE " + " COMPRA-COD-COMPRA
      * todo: Validar se esta no COMPROD
                   EVALUATE COMPRA-TIPO-PAGTO
                       WHEN "BOLETO"
      *                0,20  20%
                       DISPLAY 'BO'
                       WHEN "DEBITO"
      *                0,15  15%
                       DISPLAY 'DE'
                       WHEN "CREDITO"
      *                0,10  10%
                       DISPLAY 'CRE'
                       WHEN "CHEQUE"
                       DISPLAY "CHE"
      *                NENHUM DESCONTO                  
                   END-EVALUATE

                   PERFORM 042-LER-COMPRA
               ELSE 
                   IF COMPRA-COD-CLIENTE < CLIENTE-COD-CLINTE THEN
                       PERFORM 042-LER-COMPRA
                   ELSE
                       PERFORM 041-LER-CLIENTE
                   END-IF
               END-IF.
      
       031-MOVER-DADOS-TAB-COMPROD.
           MOVE WRK-COMPROD-REGISTRO 
           TO TAB-COMPROD-REGISTRO(WRK-CONT-PER).
           
           PERFORM 043-LER-COMPROD.

       032-MOVER-DADOS-TAB-PRODUTO. 
           MOVE WRK-PRODUTO-REGISTRO
           TO TAB-PRODUTO-REGISTRO(WRK-CONT-PER).

           PERFORM 044-LER-PRODUTO.
       
       040-LER-ARQUIVOS.
           PERFORM 041-LER-CLIENTE.
           IF WRK-FS-CLIENTE = "10"
              DISPLAY "ARQUIVO CLIENTE VAZIO"
           END-IF.

           PERFORM 042-LER-COMPRA.
           IF WRK-FS-COMPRA = "10"
              DISPLAY "ARQUIVO COMPRA VAZIO"
           END-IF.

           PERFORM 043-LER-COMPROD.
           IF WRK-FS-COMPROD = "10"
              DISPLAY "ARQUIVO COMPROD VAZIO"
           END-IF.

           PERFORM 044-LER-PRODUTO.
           IF WRK-FS-PRODUTO = "10"
              DISPLAY "ARQUIVO PRODUTO VAZIO"
           END-IF.

       041-LER-CLIENTE.
           READ CLIENTE    INTO WRK-CLIENTE-REGISTRO.
           IF WRK-FS-CLIENTE NOT = "00" AND "10"
              DISPLAY "ERRO LEITURA CLIENTE - FS: " WRK-FS-CLIENTE
              PERFORM 999-ROTINA-ABEND
           END-IF.
           IF WRK-FS-CLIENTE = "00"
                ADD 1 TO WRK-CONT-LIDOS-CLIENTE
           END-IF.
           IF WRK-FS-CLIENTE = "10"
              MOVE 999 TO CLIENTE-COD-CLINTE
           END-IF.
       
       042-LER-COMPRA.
           READ COMPRA    INTO WRK-COMPRA-REGISTRO.
           IF WRK-FS-COMPRA NOT = "00" AND "10"
              DISPLAY "ERRO LEITURA COMPRA - FS: " WRK-FS-COMPRA
              PERFORM 999-ROTINA-ABEND
           END-IF.
           IF WRK-FS-COMPRA = "00"
                ADD 1 TO WRK-CONT-LIDOS-COMPRA
           END-IF.
           IF WRK-FS-COMPRA = "10"
              MOVE 99999 TO COMPRA-COD-COMPRA
              MOVE 999 TO COMPRA-COD-CLIENTE
           END-IF.

       043-LER-COMPROD.
           READ COMPROD    INTO WRK-COMPROD-REGISTRO.
           IF WRK-FS-COMPROD NOT = "00" AND "10"
              DISPLAY "ERRO LEITURA COMPROD - FS: " WRK-FS-COMPROD
              PERFORM 999-ROTINA-ABEND
           END-IF.
           IF WRK-FS-COMPROD = "00"
                ADD 1 TO WRK-CONT-LIDOS-COMPROD
           END-IF.  

       044-LER-PRODUTO.
           READ PRODUTO    INTO WRK-PRODUTO-REGISTRO.
           IF WRK-FS-PRODUTO NOT = "00" AND "10"
              DISPLAY "ERRO LEITURA PRODUTO - FS: " WRK-FS-PRODUTO
              PERFORM 999-ROTINA-ABEND
           END-IF.
           IF WRK-FS-PRODUTO = "00"
                ADD 1 TO WRK-CONT-LIDOS-PRODUTO
           END-IF.    

       045-GRAVAR-BOLCHE.
           WRITE FD-BOLCHE-REGISTRO FROM WRK-BOLCHE-REGISTRO.
           IF WRK-FS-BOLCHE NOT = ZEROS
              DISPLAY "ERRO GRAVACAO BOLCHE - FS: " WRK-FS-BOLCHE
              PERFORM 999-ROTINA-ABEND
           END-IF.

           ADD 1 TO WRK-CONT-GRAVADOS-BOLCHE.

       046-GRAVAR-DEBCRE.
           WRITE FD-DEBCRE-REGISTRO FROM WRK-DEBCRE-REGISTRO.
           IF WRK-FS-DEBCRE NOT = ZEROS
              DISPLAY "ERRO GRAVACAO DEBCRE - FS: " WRK-FS-DEBCRE
              PERFORM 999-ROTINA-ABEND
           END-IF.

           ADD 1 TO WRK-CONT-GRAVADOS-DEBCRE.

       047-GRAVAR-RELTOT.
           WRITE FD-RELTOT-REGISTRO FROM WRK-RELTOT-REGISTRO.
           IF WRK-FS-RELTOT NOT = ZEROS
              DISPLAY "ERRO GRAVACAO RELTOT - FS: " WRK-FS-RELTOT
              PERFORM 999-ROTINA-ABEND
           END-IF.

           ADD 1 TO WRK-CONT-GRAVADOS-RELTOT. 

       050-FINALIZAR.
           PERFORM 051-MOSTRAR-CONTADORES.
           PERFORM 052-FECHAR-ARQUIVOS.

       051-MOSTRAR-CONTADORES.
           DISPLAY "Quantidade de Registros Lidos    CLIENTE..: "
                   WRK-CONT-LIDOS-CLIENTE.
           DISPLAY "Quantidade de Registros Lidos    COMPRA...: "
                   WRK-CONT-LIDOS-COMPRA.
           DISPLAY "Quantidade de Registros Lidos    COMPROD..: "
                   WRK-CONT-LIDOS-COMPROD.
           DISPLAY "Quantidade de Registros Lidos    PRODUTO..: "
                   WRK-CONT-LIDOS-PRODUTO.
           DISPLAY "Quantidade de Registros Gravados BOLCHE...: "
                   WRK-CONT-GRAVADOS-BOLCHE. 
           DISPLAY "Quantidade de Registros Gravados DEBCRE...: "
                   WRK-CONT-GRAVADOS-DEBCRE.
           DISPLAY "Quantidade de Registros Gravados RELTOT...: "
                   WRK-CONT-GRAVADOS-RELTOT.

       052-FECHAR-ARQUIVOS.
           CLOSE CLIENTE.
           IF WRK-FS-CLIENTE NOT = "00"
              DISPLAY "ERRO CLOSE CLIENTE - FS: " WRK-FS-CLIENTE
              PERFORM 999-ROTINA-ABEND
           END-IF.
           
           CLOSE COMPRA.
           IF WRK-FS-COMPRA NOT = "00"
              DISPLAY "ERRO CLOSE COMPRA - FS: " WRK-FS-COMPRA
              PERFORM 999-ROTINA-ABEND
           END-IF.
           
           CLOSE COMPROD.
           IF WRK-FS-COMPROD NOT = "00"
              DISPLAY "ERRO CLOSE COMPROD - FS: " WRK-FS-COMPROD
              PERFORM 999-ROTINA-ABEND
           END-IF.
           
           CLOSE PRODUTO.
           IF WRK-FS-PRODUTO NOT = "00"
              DISPLAY "ERRO CLOSE PRODUTO - FS: " WRK-FS-PRODUTO
              PERFORM 999-ROTINA-ABEND
           END-IF.
           
           CLOSE BOLCHE.
           IF WRK-FS-BOLCHE NOT = "00"
              DISPLAY "ERRO CLOSE BOLCHE - FS: " WRK-FS-BOLCHE
              PERFORM 999-ROTINA-ABEND
           END-IF.
           
           CLOSE DEBCRE.
           IF WRK-FS-DEBCRE NOT = "00"
              DISPLAY "ERRO CLOSE DEBCRE - FS: " WRK-FS-DEBCRE
              PERFORM 999-ROTINA-ABEND
           END-IF.
           
           CLOSE RELTOT.
           IF WRK-FS-RELTOT NOT = "00"
              DISPLAY "ERRO CLOSE RELTOT - FS: " WRK-FS-RELTOT
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
           DISPLAY "ABEND DO PROGRAMA - AC00EX010".
           GOBACK.
            