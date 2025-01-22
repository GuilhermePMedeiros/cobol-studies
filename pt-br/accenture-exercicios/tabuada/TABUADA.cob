       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TABUADA'.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMERO PIC 9(2) COMP-3 VALUE ZEROS.
       01 WS-CONTADOR-NUMERO PIC 9(2) COMP-3 VALUE 1.
       01 WS-RESULTADO PIC  9(2) COMP-3 VALUE ZEROS.
       
       77 WS-E-NUMERO-VALIDO PIC 9(1) VALUE ZEROS.
           88 WS-NUMERO-VALIDO VALUE 1.
           88 WS-NUMERO-INVALIDO VALUE 0.

       PROCEDURE DIVISION.
           PERFORM SOLICITAR-NUMERO.
           PERFORM VERIFICAR-NUMERO UNTIL WS-E-NUMERO-VALIDO = 1.
           PERFORM IMPRIMIR-TABUADA.
           STOP RUN.

       SOLICITAR-NUMERO.
           DISPLAY "________________________________________".
           DISPLAY "TABUADA APP".
           DISPLAY "________________________________________".
           DISPLAY " ".
           DISPLAY "DIGITE O NUMERO PARA REALIZAR A OPERACAO: ".
           ACCEPT WS-NUMERO.
           DISPLAY " ".

       VERIFICAR-NUMERO.
           IF WS-NUMERO < 0 OR WS-NUMERO > 10 THEN
                  DISPLAY "NUMERO INFORMADO E INVALIDO!"
                  DISPLAY "DIGITE NOVAMENTE UM NUMERO ENTRE 1 E 9."
                  SET WS-NUMERO-INVALIDO TO TRUE
                  INITIALIZE WS-NUMERO
                  PERFORM SOLICITAR-NUMERO
           ELSE 
                  SET WS-NUMERO-VALIDO TO TRUE
           END-IF.

       IMPRIMIR-TABUADA.
           DISPLAY "________________________________________".
           DISPLAY "TABUADA DO NUMERO: " WS-NUMERO ".".
           DISPLAY " ".
           PERFORM CALCULAR-TABUADA UNTIL WS-CONTADOR-NUMERO > 10.
           DISPLAY "________________________________________".

       CALCULAR-TABUADA.
           COMPUTE WS-RESULTADO = WS-NUMERO * WS-CONTADOR-NUMERO. 
           DISPLAY WS-NUMERO " X " WS-CONTADOR-NUMERO " : " WS-RESULTADO.
           ADD 1 TO WS-CONTADOR-NUMERO.
