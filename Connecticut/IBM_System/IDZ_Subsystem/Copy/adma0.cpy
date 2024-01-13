             DS    0H                 ENSURE ALIGNMENT                          
ADMENUS  EQU  *      .             START OF MAP DEFINITION                      
         DS    12C .             TIOA PREFIX                                    
        SPACE                                                                   
ADBDAYL DS    CL2 .  INPUT DATA FIELD LEN                                       
ADBDAYF DS    0C .   DATA FIELD FLAG                                            
ADBDAYA DS    C .    DATA FIELD ATTRIBUTE                                       
ADBDAYC DS    C .   COLOUR ATTRIBUTE                                            
ADBDAYH DS    C .   HIGHLIGHTING ATTRIBUTE                                      
ADBDAYI DS    0CL8 .  INPUT DATA FIELD                                          
ADBDAYO DS    CL8 .   OUTPUT DATA FIELD                                         
        SPACE                                                                   
ADBDAYML DS    CL2 .  INPUT DATA FIELD LEN                                      
ADBDAYMF DS    0C .   DATA FIELD FLAG                                           
ADBDAYMA DS    C .    DATA FIELD ATTRIBUTE                                      
ADBDAYMC DS    C .   COLOUR ATTRIBUTE                                           
ADBDAYMH DS    C .   HIGHLIGHTING ATTRIBUTE                                     
ADBDAYMI DS    0CL38 .  INPUT DATA FIELD                                        
ADBDAYMO DS    CL38 .   OUTPUT DATA FIELD                                       
        SPACE                                                                   
ADBDAYIL DS    CL2 .  INPUT DATA FIELD LEN                                      
ADBDAYIF DS    0C .   DATA FIELD FLAG                                           
ADBDAYIA DS    C .    DATA FIELD ATTRIBUTE                                      
ADBDAYIC DS    C .   COLOUR ATTRIBUTE                                           
ADBDAYIH DS    C .   HIGHLIGHTING ATTRIBUTE                                     
ADBDAYII DS    0CL1 .  INPUT DATA FIELD                                         
ADBDAYIO DS    CL1 .   OUTPUT DATA FIELD                                        
        SPACE                                                                   
ADBDAYDL DS    CL2 .  INPUT DATA FIELD LEN                                      
ADBDAYDF DS    0C .   DATA FIELD FLAG                                           
ADBDAYDA DS    C .    DATA FIELD ATTRIBUTE                                      
ADBDAYDC DS    C .   COLOUR ATTRIBUTE                                           
ADBDAYDH DS    C .   HIGHLIGHTING ATTRIBUTE                                     
ADBDAYDI DS    0CL45 .  INPUT DATA FIELD                                        
ADBDAYDO DS    CL45 .   OUTPUT DATA FIELD                                       
        SPACE                                                                   
ADBDAYRL DS    CL2 .  INPUT DATA FIELD LEN                                      
ADBDAYRF DS    0C .   DATA FIELD FLAG                                           
ADBDAYRA DS    C .    DATA FIELD ATTRIBUTE                                      
ADBDAYRC DS    C .   COLOUR ATTRIBUTE                                           
ADBDAYRH DS    C .   HIGHLIGHTING ATTRIBUTE                                     
ADBDAYRI DS    0CL45 .  INPUT DATA FIELD                                        
ADBDAYRO DS    CL45 .   OUTPUT DATA FIELD                                       
        SPACE                                                                   
ADBDFMTL DS    CL2 .  INPUT DATA FIELD LEN                                      
ADBDFMTF DS    0C .   DATA FIELD FLAG                                           
ADBDFMTA DS    C .    DATA FIELD ATTRIBUTE                                      
ADBDFMTC DS    C .   COLOUR ATTRIBUTE                                           
ADBDFMTH DS    C .   HIGHLIGHTING ATTRIBUTE                                     
ADBDFMTI DS    0CL45 .  INPUT DATA FIELD                                        
ADBDFMTO DS    CL45 .   OUTPUT DATA FIELD                                       
        SPACE                                                                   
ADBDFM1L DS    CL2 .  INPUT DATA FIELD LEN                                      
ADBDFM1F DS    0C .   DATA FIELD FLAG                                           
ADBDFM1A DS    C .    DATA FIELD ATTRIBUTE                                      
ADBDFM1C DS    C .   COLOUR ATTRIBUTE                                           
ADBDFM1H DS    C .   HIGHLIGHTING ATTRIBUTE                                     
ADBDFM1I DS    0CL24 .  INPUT DATA FIELD                                        
ADBDFM1O DS    CL24 .   OUTPUT DATA FIELD                                       
        SPACE                                                                   
ADBDFM2L DS    CL2 .  INPUT DATA FIELD LEN                                      
ADBDFM2F DS    0C .   DATA FIELD FLAG                                           
ADBDFM2A DS    C .    DATA FIELD ATTRIBUTE                                      
ADBDFM2C DS    C .   COLOUR ATTRIBUTE                                           
ADBDFM2H DS    C .   HIGHLIGHTING ATTRIBUTE                                     
ADBDFM2I DS    0CL29 .  INPUT DATA FIELD                                        
ADBDFM2O DS    CL29 .   OUTPUT DATA FIELD                                       
        SPACE                                                                   
ADDIFF1L DS    CL2 .  INPUT DATA FIELD LEN                                      
ADDIFF1F DS    0C .   DATA FIELD FLAG                                           
ADDIFF1A DS    C .    DATA FIELD ATTRIBUTE                                      
ADDIFF1C DS    C .   COLOUR ATTRIBUTE                                           
ADDIFF1H DS    C .   HIGHLIGHTING ATTRIBUTE                                     
ADDIFF1I DS    0CL24 .  INPUT DATA FIELD                                        
ADDIFF1O DS    CL24 .   OUTPUT DATA FIELD                                       
        SPACE                                                                   
ADDIFF2L DS    CL2 .  INPUT DATA FIELD LEN                                      
ADDIFF2F DS    0C .   DATA FIELD FLAG                                           
ADDIFF2A DS    C .    DATA FIELD ATTRIBUTE                                      
ADDIFF2C DS    C .   COLOUR ATTRIBUTE                                           
ADDIFF2H DS    C .   HIGHLIGHTING ATTRIBUTE                                     
ADDIFF2I DS    0CL12 .  INPUT DATA FIELD                                        
ADDIFF2O DS    CL12 .   OUTPUT DATA FIELD                                       
        SPACE                                                                   
ADDIFF3L DS    CL2 .  INPUT DATA FIELD LEN                                      
ADDIFF3F DS    0C .   DATA FIELD FLAG                                           
ADDIFF3A DS    C .    DATA FIELD ATTRIBUTE                                      
ADDIFF3C DS    C .   COLOUR ATTRIBUTE                                           
ADDIFF3H DS    C .   HIGHLIGHTING ATTRIBUTE                                     
ADDIFF3I DS    0CL4 .  INPUT DATA FIELD                                         
ADDIFF3O DS    CL4 .   OUTPUT DATA FIELD                                        
        SPACE                                                                   
ADBRETL DS    CL2 .  INPUT DATA FIELD LEN                                       
ADBRETF DS    0C .   DATA FIELD FLAG                                            
ADBRETA DS    C .    DATA FIELD ATTRIBUTE                                       
ADBRETC DS    C .   COLOUR ATTRIBUTE                                            
ADBRETH DS    C .   HIGHLIGHTING ATTRIBUTE                                      
ADBRETI DS    0CL27 .  INPUT DATA FIELD                                         
ADBRETO DS    CL27 .   OUTPUT DATA FIELD                                        
        SPACE                                                                   
ADRET1L DS    CL2 .  INPUT DATA FIELD LEN                                       
ADRET1F DS    0C .   DATA FIELD FLAG                                            
ADRET1A DS    C .    DATA FIELD ATTRIBUTE                                       
ADRET1C DS    C .   COLOUR ATTRIBUTE                                            
ADRET1H DS    C .   HIGHLIGHTING ATTRIBUTE                                      
ADRET1I DS    0CL26 .  INPUT DATA FIELD                                         
ADRET1O DS    CL26 .   OUTPUT DATA FIELD                                        
        SPACE                                                                   
ADRET2L DS    CL2 .  INPUT DATA FIELD LEN                                       
ADRET2F DS    0C .   DATA FIELD FLAG                                            
ADRET2A DS    C .    DATA FIELD ATTRIBUTE                                       
ADRET2C DS    C .   COLOUR ATTRIBUTE                                            
ADRET2H DS    C .   HIGHLIGHTING ATTRIBUTE                                      
ADRET2I DS    0CL35 .  INPUT DATA FIELD                                         
ADRET2O DS    CL35 .   OUTPUT DATA FIELD                                        
        SPACE                                                                   
MSGOUTL DS    CL2 .  INPUT DATA FIELD LEN                                       
MSGOUTF DS    0C .   DATA FIELD FLAG                                            
MSGOUTA DS    C .    DATA FIELD ATTRIBUTE                                       
MSGOUTC DS    C .   COLOUR ATTRIBUTE                                            
MSGOUTH DS    C .   HIGHLIGHTING ATTRIBUTE                                      
MSGOUTI DS    0CL35 .  INPUT DATA FIELD                                         
MSGOUTO DS    CL35 .   OUTPUT DATA FIELD                                        
        SPACE                                                                   
MSGERRL DS    CL2 .  INPUT DATA FIELD LEN                                       
MSGERRF DS    0C .   DATA FIELD FLAG                                            
MSGERRA DS    C .    DATA FIELD ATTRIBUTE                                       
MSGERRC DS    C .   COLOUR ATTRIBUTE                                            
MSGERRH DS    C .   HIGHLIGHTING ATTRIBUTE                                      
MSGERRI DS    0CL60 .  INPUT DATA FIELD                                         
MSGERRO DS    CL60 .   OUTPUT DATA FIELD                                        
        SPACE                                                                   
ADMENUE  EQU   *     .               END OF MAP DEFINITION                      
           ORG  ADMENUS  .               ADDRESS START OF MAP                   
* CALCULATE MAPLENGTH, ASSIGNING A VALUE OF ONE WHERE LENGTH=ZERO               
ADMENUL  EQU  ADMENUE-ADMENUS                                                   
ADMENUI  DS  0CL(ADMENUL+1-(ADMENUL/ADMENUL))                                   
ADMENUO  DS  0CL(ADMENUL+1-(ADMENUL/ADMENUL))                                   
           ORG                                                                  
* * * END OF MAP DEFINITION * * *                                               
         SPACE 3                                                                
         ORG                                                                    
ADMA0T EQU *       * END OF MAP SET                                             
* * * END OF MAP SET DEFINITION * * *                                           
         SPACE 3                                                                