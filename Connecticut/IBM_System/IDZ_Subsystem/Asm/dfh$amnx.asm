DFH$AMNX CSECT                                                                  
*                                                                               
         DFHREGS                                                                
DFHEISTG DSECT                                                                  
OUTAREA  DS    0CL512                                                           
         DS    CL8                                                              
OUTLEN   DS    H                                                                
         DS    H                                                                
OUTDATA  DS    CL500                                                            
INLEN    DS    H                                                                
INAREA   DS    CL256                                                            
PROOF    DS    CL60                                                             
         COPY  DFH$AGA                                                          
         COPY  DFHBMSCA                                                         
DFH$AMNU CSECT                                                                  
         EXEC CICS HANDLE AID PF3(PF3_ROUTINE)                                  
*                                                                               
         XC    DFH$AGAS(DFH$AGAL),DFH$AGAS                                      
         MVC   MSGO(L'APPLMSG),APPLMSG                                          
         EXEC  CICS SEND MAP('DFH$$AGA') FROM(DFH$AGAO) ERASE                   
         MVC   OUTAREA(256),0(R6)                                               
         MVC   OUTAREA+256(256),256(R6)                                         
         EXEC  CICS SEND TEXT MAPPED FROM(OUTDATA) LENGTH(OUTLEN)               
*                                                                               
         EXEC CICS RECEIVE INTO(INAREA) LENGTH(INLEN)                           
*                                                                               
         EXEC CICS RECEIVE MAP('DFH$AGA') SET(R7) LENGTH(INLEN)                 
*                                                                               
         XC    PROOF,PROOF                                                      
         MVC   PROOF(25),=C'You just keyed in number '                          
         MVC   PROOF+25(6),KEYI-DFH$$AGAI(R7)FINISH   DS    0H                  
         EXEC CICS SEND TEXT FROM(PROOF) LENGTH(60) ERASE FREEKB                
         TM    MSGF-DFH$AGAI(R7),X'02'                                          
         BNO   RETURN                                                           
         XC    PROOF,PROOF                                                      
         MVC   PROOF(33),=C'Input cursor located in MSG field'                  
         EXEC CICS SEND TEXT FROM(PROOF) LENGTH(60) ERASE FREEKB                
*                                                                               
*        THE RETURN COMMAND ENDS THE PROGRAM.                                   
*                                                                               
RETURN   DS    0H                                                               
         EXEC  CICS RETURN                                                      
*                                                                               
PF3_ROUTINE DS 0H                                                               
         XC    PROOF,PROOF                                                      
         MVC   PROOF(30),=C'RECEIVE MAP specified AID(PF3)'                     
         B     RETURN                                                           
MAXLEN   DC    H'256'                                                           
APPLMSG  DC    C'This is a MAPPINGDEV application'                              
         END                                                                    
