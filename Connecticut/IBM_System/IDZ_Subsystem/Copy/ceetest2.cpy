           MOVE 'STEP' TO COMMAND                                               
           CALL 'CEETEST' USING CEETEST-VSTRING, FEEDBACK.                      
           IF FB-SEV NOT = 0                                                    
               DISPLAY 'CALL TO CEETEST FAILED'                                 
               DISPLAY 'FACILITY & MSG = ', FB-FAC-ID, FB-MSGNO                 
           ELSE                                                                 
               DISPLAY 'YOU SHOULD HAVE GOTTEN INTO DEBUG TOOL'                 
           END-IF.                                                              