         PRINT NOGEN                                                            
PRINTOUT START                                                                  
         STM  14,12,12(13)                                                      
         BALR 3,0         LOAD BASE REGISTER                                    
         USING *,12                                                             
         ST  13,SAVEAREA+4                                                      
         LA  13,SAVEAREA                                                        
         MOPEN (INFILE,(INPUT))                                                 
         MOPEN (OUTFILE,(OUTPUT))                                               
CLEAR    MVC  OUTAREA,SPACES                                                    
         MGET  INFILE,RECORD                                                    
         MVC  LNAME,LAST                                                        
         MVC  SALOUT,SALARY                                                     
         MPUT OUTFILE,OUTAREA                                                   
         B    CLEAR                                                             
EOF      MCLOSE (INFILE)                                                        
         MCLOSE (OUTFILE)                                                       
         L    13,SAVEAREA+4                                                     
         LM   14,12,12(13)                                                      
         BR   14                                                                
*                                                                               
SAVEAREA DS   18F                                                               
INFILE   MDCBIN EOF=EOF                                                         
OUTFILE  MDCBOUT                                                                
*                                                                               
RECORD   DS   0CL80                                                             
LAST     DS   CL15                                                              
SALARY   DS   CL5                                                               
         DS   CL60                                                              
SPACES   DC   CL1' '                                                            
OUTAREA  DS   0CL132                                                            
LNAME    DS   CL15                                                              
SALOUT   DS   CL5                                                               
         DS   CL30                                                              
