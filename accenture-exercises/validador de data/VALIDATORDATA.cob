       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'VALIDATORDATA'.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.    
       01 WS-DATA.
           05 WS-DIA PIC 9(2) VALUE ZEROS.
           05 FILLER PIC X(1) VALUE '/'. 
           05 WS-MES PIC 9(2) VALUE ZEROS.
           05 FILLER PIC X(1) VALUE '/'.
           05 WS-ANO PIC 9(4) VALUE ZEROS.

       77 WS-ISVALID PIC 9(1) VALUE 1.
      
       PROCEDURE DIVISION.
           
           PERFORM SOLICITAR-DATA.
           PERFORM VALIDAR-DATA.
           PERFORM MOSTRAR-RESULTADO.
           STOP RUN.

       SOLICITAR-DATA.
           DISPLAY "________________________________________".
           DISPLAY "PROGRAMA VALIDADOR DE DATA".
           DISPLAY "________________________________________".
           DISPLAY "DIGITE O DIA: ".
           ACCEPT WS-DIA.
           DISPLAY "DIGITE O MES: ".
           ACCEPT WS-MES.
           DISPLAY "DIGITE O ANO: ".
           ACCEPT WS-ANO.
           DISPLAY "________________________________________".
           DISPLAY " ".

       VALIDAR-DATA.
           IF WS-ANO < 2000 THEN
               MOVE 0 TO WS-ISVALID
           ELSE 
               IF WS-MES <= 0 OR WS-MES > 12 THEN
                   MOVE 0 TO WS-ISVALID
               ELSE
                   IF WS-DIA <= 0 OR WS-DIA > 31 THEN 
                     MOVE 0 TO WS-ISVALID
                   ELSE
                     IF WS-MES = 2 THEN
                       IF WS-ANO = 2016 THEN
                          IF WS-DIA > 29 THEN 
                             MOVE 0 TO WS-ISVALID 
                          END-IF
                       ELSE 
                         IF WS-DIA > 28 THEN 
                           MOVE 0 TO WS-ISVALID 
                         END-IF
                       END-IF  
                     ELSE
                         IF WS-MES = 4 OR WS-MES = 6 OR WS-MES = 9 
                            OR WS-MES = 11 THEN
                               IF WS-DIA > 30 THEN
                                 MOVE 0 TO WS-ISVALID 
                               END-IF
                         END-IF
                     END-IF
                   END-IF        
               END-IF
           END-IF.

       MOSTRAR-RESULTADO.
           IF WS-ISVALID EQUAL ZEROS THEN
               DISPLAY "DATA INFORMADA " WS-DATA " E INVALIDA."
           ELSE
               DISPLAY "DATA INFORMADA " WS-DATA " E VALIDA." 
           END-IF.
           DISPLAY "________________________________________".
           