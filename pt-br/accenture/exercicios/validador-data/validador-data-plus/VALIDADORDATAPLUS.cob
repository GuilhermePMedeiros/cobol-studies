       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'VALIDADORDATAPLUS'.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.    
       01 WS-DATA.
           05 WS-DIA PIC 9(2) VALUE ZEROS.
           05 FILLER PIC X(1) VALUE '/'. 
           05 WS-MES PIC 9(2) VALUE ZEROS.
           05 FILLER PIC X(1) VALUE '/'.
           05 WS-ANO PIC 9(4) VALUE ZEROS.

       77 WS-E-VALIDA PIC X(1) VALUE 'S'.
           88 WS-DATA-VALIDA VALUE 'S'.
           88 WS-DATA-INVALIDA VALUE 'N'.

       77 WS-E-ANO-BISSEXTO PIC X(1) VALUE 'S'.
           88 WS-ANO-BISSEXTO VALUE 'S'.
           88 WS-ANO-NAO-BISSEXTO VALUE 'N'. 

       01 WS-MOD-DE-QUATRO PIC 9(1).
       01 WS-MOD-DE-CEM PIC 9(1).
       01 WS-MOD-DE-QUATROCENTROS PIC 9(1).
       
       PROCEDURE DIVISION.
           
           PERFORM SOLICITAR-DATA.
           PERFORM VALIDAR-DATA.
           PERFORM MOSTRAR-RESULTADO.
           STOP RUN.

       SOLICITAR-DATA.
           DISPLAY "________________________________________".
           DISPLAY "PROGRAMA VALIDADOR DE DATA".
           DISPLAY "________________________________________".
           DISPLAY "DIGITE A DATA NESTE FORMATO DIA/MES/ANO: ".
           ACCEPT WS-DATA
           DISPLAY "________________________________________".
           DISPLAY " ".

       VALIDAR-DATA.
           IF WS-ANO < 2000 THEN
               SET WS-DATA-INVALIDA TO TRUE
           ELSE 
               IF WS-MES <= 0 OR WS-MES > 12 THEN
                   SET WS-DATA-INVALIDA TO TRUE
               ELSE
                   IF WS-DIA <= 0 OR WS-DIA > 31 THEN 
                     SET WS-DATA-INVALIDA TO TRUE
                   ELSE
                     IF WS-MES = 2 THEN
                       PERFORM VALIDAR-ANO-BISSEXTO
                       IF WS-E-ANO-BISSEXTO = 'S' THEN
                          IF WS-DIA > 29 THEN 
                             SET WS-DATA-INVALIDA TO TRUE 
                          END-IF
                       ELSE 
                         IF WS-DIA > 28 THEN 
                           SET WS-DATA-INVALIDA TO TRUE 
                         END-IF
                       END-IF  
                     ELSE
                         IF WS-MES = 4 OR WS-MES = 6 OR WS-MES = 9 
                            OR WS-MES = 11 THEN
                               IF WS-DIA > 30 THEN
                                 SET WS-DATA-INVALIDA TO TRUE 
                               END-IF
                         END-IF
                     END-IF
                   END-IF        
               END-IF
           END-IF.

       VALIDAR-ANO-BISSEXTO.
           COMPUTE WS-MOD-DE-QUATRO = FUNCTION MOD(WS-ANO 4).
           COMPUTE WS-MOD-DE-CEM = FUNCTION MOD(WS-ANO 100).
           COMPUTE WS-MOD-DE-QUATROCENTROS = FUNCTION MOD(WS-ANO 400).

           IF WS-MOD-DE-QUATRO = 0 THEN
               IF WS-MOD-DE-CEM = 0 AND WS-MOD-DE-QUATRO THEN
                   SET WS-ANO-BISSEXTO TO TRUE
               ELSE
                   SET WS-ANO-NAO-BISSEXTO TO TRUE
               END-IF
           ELSE
                SET WS-ANO-NAO-BISSEXTO TO TRUE
           END-IF.

       MOSTRAR-RESULTADO.
           IF WS-E-VALIDA EQUAL 'N' THEN
               DISPLAY "DATA INFORMADA " WS-DATA " E INVALIDA."
           ELSE
               DISPLAY "DATA INFORMADA " WS-DATA " E VALIDA." 
           END-IF.
           DISPLAY "________________________________________".
           