       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'VALIDADORDATA'.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.    
       01 WS-DATA.
           05 WS-DIA PIC 9(2) VALUE ZEROS.
           05 FILLER PIC X(1) VALUE '/'. 
           05 WS-MES PIC 9(2) VALUE ZEROS.
           05 FILLER PIC X(1) VALUE '/'.
           05 WS-ANO PIC 9(4) VALUE ZEROS.

       77 WS-EVALIDA PIC 9(1) VALUE 1.
           88 WS-DATA-VALIDA VALUE 1.
           88 WS-DATA-INVALIDA VALUE 0.
      
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
                       IF WS-ANO = 2016 THEN
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

       MOSTRAR-RESULTADO.
           IF WS-EVALIDA EQUAL ZEROS THEN
               DISPLAY "DATA INFORMADA " WS-DATA " E INVALIDA."
           ELSE
               DISPLAY "DATA INFORMADA " WS-DATA " E VALIDA." 
           END-IF.
           DISPLAY "________________________________________".
           