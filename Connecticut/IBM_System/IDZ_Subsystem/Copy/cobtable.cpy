010200 01  CUSTOMER-TABLE.                                                      
010300     05  CUSTOMER-DATA.                                                   
010400         10  FILLER     PIC X(22)  VALUE '0602500USAIR          '.        
010500         10  FILLER     PIC X(22)  VALUE '0103210AMERICAN       '.        
010600         10  FILLER     PIC X(22)  VALUE '0403340EASTERN        '.        
010700         10  FILLER     PIC X(22)  VALUE '1004120SABENA         '.        
010800         10  FILLER     PIC X(22)  VALUE '0204860BRANIFF        '.        
010900         10  FILLER     PIC X(22)  VALUE '0505010TWA            '.        
011000         10  FILLER     PIC X(22)  VALUE '0806790PEOPLES        '.        
011100         10  FILLER     PIC X(22)  VALUE '0306810DELTA          '.        
011200         10  FILLER     PIC X(22)  VALUE '0708190UNITED         '.        
011300         10  FILLER     PIC X(22)  VALUE '0908520PIEDMONT       '.        
011400     05  CUSTOMER-INFO REDEFINES CUSTOMER-DATA.                           
011500         10  CUSTOMER-REC OCCURS 10 TIMES INDEXED BY CUST-INDEX.          
011600             15  CUSTOMER-CODE        PIC 99.                             
011700             15  CUSTOMER-ACCOUNT     PIC 9(5).                           
011800             15  CUSTOMER-NAME        PIC X(15).                          
