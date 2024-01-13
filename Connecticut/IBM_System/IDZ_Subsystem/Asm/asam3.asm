ASAM2    AMODE 31                                                               
ASAM2    CSECT                                                                  
* *************************************************                             
*  SAMPLE PROGRAM ASAM2   (HEXCHARS ROUTINE)                                    
*     AUTHOR: DOUG STOUT                                                        
*             IBM CORPORATION                                                   
*                                                                               
*  INPUT:  A CHARACTER STRING                                                   
*  OUTPUT: THE HEX EQUIVALENT OF THE CHARACTER STRING                           
*          IN DISPLAY FORMAT                                                    
*                                                                               
*  PARAMETERS PASSED FROM CALLING PROGRAM:                                      
*    1: A FULLWORD LENGTH OF THE DATA IN PARM 2                                 
*    2: A CHARACTER STRING                                                      
*    3: A STORAGE AREA TO RECEIVE THE LEFT NYBLES OF EACH BYTE                  
*    4: A STORAGE AREA TO RECEIVE THE RIGHT NYBLES OF EACH BYTE                 
*                                                                               
*  EXAMPLE.   THE CALLING PROGRAM SENDS PARMS:                                  
*    1. FULLWORD WITH A VALUE OF 10                                             
*    2. CHARACTER STRING: "A1B2C3D4E5"                                          
*    3. A STORAGE AREA AT LEAST 10 BYTES IN LENGTH TO RECEIVE DATA              
*    4. A STORAGE AREA AT LEAST 10 BYTES IN LENGTH TO RECEIVE DATA              
*  FOR THIS EXAMPLE, THIS PROGRAM WILL MODIFY THE CONTENTS OF                   
*  PARMS 3 AND 4 AS FOLLOWS:                                                    
*    RETURNED IN PARM 3: "FCFCFCFCFC"                                           
*    RETURNED IN PARM 4: "1122334455"                                           
*                                                                               
************************************************************                    
         STM   R14,R12,12(R13)    STANDARD LINKAGE CONVENTION                   
         LR    R12,R15            INIT BASE REGISTER                            
         LA    R11,2048(R12)      SECOND BASE REGISTER IF NEEDED                
         LA    R11,2048(R11)      BUMP IT TO THE NEXT 4096                      
         USING ASAM2,R12,R11   ESTABLISH BASE REGISTERS                         
         ST    R13,SAVEAREA+4                                                   
         LA    R10,SAVEAREA                                                     
         ST    R10,8(R10)                                                       
         B     MAINLINE           GO AROUND EYECATCHER/SAVEAREA                 
         DS    0D                                                               
EYECATCH DC    CL32'******** PROGRAM ASAM2 *********'                           
SAVEAREA DC    18F'0'                                                           
*                                                                               
DIAG     DS    0F                                              @ZV!CHNG         
         MVC  STATUS,=C'PROGRAM STARTED               '                         
*                                                                               
* ADDRESS / SAVE THE PASSED PARMS                                               
*                                                                               
GETPARMS EQU   *                                                                
         L     R10,0(R1)         R10 = ADDR OF PARM 1                           
         MVC   DATALEN,0(R10)    SAVE PARM 1: DATA LENGTH                       
         L     R2PRM2,4(,R1)     R2 = POINTER TO PARM 2                         
         L     R3PRM3,8(,R1)     R3 = POINTER TO PARM 3                         
         L     R4PRM4,12(,R1)    R4 = POINTER TO PARM 4                         
*                                                                               
         MVC   STATUS,=C'GENERATING HEX DATA           '                        
*                                                                               
* CHECK FOR DEMO ABEND                                                          
*                                                                               
         CLC   0(5,R2),=C'ABEND'  CHECK FOR DEMO ABEND REQUEST                  
         BNE   GENHEX            IF NOT, SKIP                                   
         MVC   NUM2CHAR,=C'@#$%!'   MOVE GARBAGE TO PACKED FIELD                
         AP    NUM1,NUM2         ABEND                                          
*                                                                               
* GENERATE HEX DATA INTO THE 2 HEX STRING AREAS                                 
*                                                                               
PHARM    EQU  *                                                @ZV!CHNG         
         LA    R5OFFST,0          R5 = 0  (INITIALIZE DATA OFFSET)              
HEXLOOP  LA    R6CHAR,0           R6 = 0  (INITIALIZE WORK CHARACTER)           
*                                                                               
*           GET AND STORE THE LEFT HEX CHARACTER                                
         IC    R6CHAR,0(R5OFFST,R2PRM2)  R6 = THE 8-BIT CHARACTER               
         SRL   R6CHAR,4                  R6 = JUST THE LEFT 4 BITS              
         IC    R7HEX,HEXCHAR(R6CHAR)     R7 = THE HEX CHARACTER                 
         STC   R7HEX,0(R5OFFST,R3PRM3)   STORE LEFT HEX CHAR IN OUTPUT          
*                                                                               
*           GET AND STORE THE RIGHT HEX CHARACTER                               
         IC    R6CHAR,0(R5OFFST,R2PRM2)  R6 = THE 8-BIT CHARACTER               
         SLL   R6CHAR,28                                                        
         SRL   R6CHAR,28                 R6 = JUST THE RIGHT 4 BITS             
         IC    R7HEX,HEXCHAR(R6CHAR)     R7 = THE HEX CHARACTER                 
         STC   R7HEX,0(R5OFFST,R4PRM4)   STORE RIGHT HEX CHAR IN OUTPUT         
*                                                                               
*           DONE WITH THE INPUT YET?                                            
         LA    R5OFFST,1(,R5OFFST)       INCREMENT POINTER                      
         C     R5OFFST,DATALEN           POINTER PAST END OF DATA?              
         BL    HEXLOOP                                                          
LOOPDONE EQU   *                                                                
*                                                                               
         MVC  STATUS,=C'RETURNING TO CALLING PROGRAM  '                         
*                                                                               
RETURN00 DS    0H                 GOOD RETURN CODE                              
         L     R13,SAVEAREA+4                                                   
         LM    R14,R12,12(R13)                                                  
         LA    R15,0                                                            
         BR    R14                                                              
*                                                                               
********************************************************                        
* CONSTANTS AND WORK AREAS                                                      
*                                                                               
DATALEN  DC    F'0'                  PARM 1 = LENGTH OF PASSED DATA             
STATUS   DC    CL30' '               CURRENT PROGRAM STATUS                     
HEXCHAR  DC    CL16'0123456789ABCDEF'   HEX CHARACTERS                          
NUM1     DC    PL5'1'                                                           
NUM2     DC    PL5'999'                                                         
         ORG   NUM2                                                             
NUM2CHAR DS    CL5                                                              
*                                                                               
* REGISTER EQUATES                                                              
*                                                                               
R0       EQU   0                                                                
R1       EQU   1                                                                
R2       EQU   2                                                                
R2PRM2   EQU   2                                                                
R3       EQU   3                                                                
R3PRM3   EQU   3                                                                
R4       EQU   4                                                                
R4PRM4   EQU   4                                                                
R5       EQU   5                                                                
R5OFFST  EQU   5                                                                
R6       EQU   6                                                                
R6CHAR   EQU   6                                                                
R7       EQU   7                                                                
R7HEX    EQU   7                                                                
R8       EQU   8                                                                
R9       EQU   9                                                                
R10      EQU   10                                                               
R11      EQU   11                                                               
R12      EQU   12                                                               
R13      EQU   13                                                               
R14      EQU   14                                                               
R15      EQU   15                                                               
*                                                                               
         END                                                                    
