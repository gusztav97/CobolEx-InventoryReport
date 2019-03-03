	IDENTIFICATION DIVISION.
	PROGRAM-ID. PROGRAM1.
	AUTHOR. GUSZTAV FRANCIA.
	
	
      *  This program produces the Inventory Report and the current date, 3 initials, and the page number.
      *  
      * 
      *
      *  INPUT:  Catalog Number, Item Description, Unit Purchase Price, Quantity on Hand, quantity on order, reorder point,
      *		      quantity-received, quantity-sold, quantity-returned
      *  OUTPUT: report-title, date, initials, catalog-number, item-description, price, quantity on hand, quantity order, reorder point
      *
      *
      *
      *
      **********************************************************
	  
		ENVIRONMENT DIVISION.
		CONFIGURATION SECTION.
		SOURCE-COMPUTER.    IBMPC.
		OBJECT-COMPUTER.    IBMPC.

		INPUT-OUTPUT SECTION.
		FILE-CONTROL.

      * ASSIGN A FILE NAME FOR THE DISK FILE

        SELECT PROGRAM-FILE
			ASSIGN TO 'PR1FA17.txt'
            ORGANIZATION IS LINE SEQUENTIAL.

      * USE REPORT.DAT FOR THE PRINTER FILE

        SELECT REPORT-FILE 
			ASSIGN TO PRINTER 'PROGRAM1FA18.TXT'
			ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.


      * SAME NAME AS THE SELECT STATEMENT

       FD PROGRAM-FILE
			RECORD CONTAINS 80 CHARACTERS.
			
			
		01  INVENTORY-REC.
      * MUST IDENTIFY AND ACCOUNT FOR ALL FIELDS OF THE 
      * INCOMING RECORD.
      * USE 05 LEVELS
           05  CATALOG-NUMBER         PIC X(5).
           05  ITEM-DESCRIPTION       PIC X(20).
           05  UNIT-PURCHASE-PRICE    PIC 999V99.
           05  FILLER                 PIC X(6).
           05  QUANTITY-ON-HAND       PIC X(4).
           05  QUANTITY-ON-ORDER      PIC X(4).
           05  REORDER-POINT          PIC X(4).
           05  QUANTITY-RECIEVED      PIC X(4).
           05  QUANTITY-SOLD          PIC X(4).
           05  QUANTITY-RETURNED      PIC X(4).
           05  FILLER                 PIC X(20).

      *MUST IDENTIFY AND ACCOUNT FOR ALL FIELDS OF THE INCOMING RECORD.
      *USE 05 LEVELS

       FD    REPORT-FILE
			RECORD CONTAINS 80 CHARACTERS.
       01    REPORT-REC.
			05				PIC X(80).

       WORKING-STORAGE SECTION.
        01  WS-WORK-AREAS.
			05  ARE-THERE-MORE-RECORDS PIC X(3) VALUE 'YES'.
        01  WS-DATE.
			05  WS-YEAR                PIC 99.
		    05  WS-MONTH               PIC 99.
            05  WS-DAY                 PIC 99.
        01 WS-HEADING-1.
           05  H1-DATE.
				10 H1-MONTH            	PIC 99.
				10 FILLER              	PIC X VALUE '/'.
				10 H1-DAY              	PIC 99.
				10 FILLER              	PIC X VALUE '/'.
				10 H1-YEAR             	PIC 9999.
			05 FILLER                	PIC X(5).
            05  H1-INITIALS            PIC X(3) VALUE 'WGF'.
            05  FILLER                 PIC X(10).
            05  H1-TITLE               PIC X(26) VALUE 
               'DRAKEA CAR PARTS WAREHOUSE'.
            05  FILLER                 PIC X(12).
            05  H1-PAGE-NUMBER         PIC X(7) VALUE 'PAGE 01'.
            05  FILLER                 PIC X.
        01  WS-HEADING-2.
            05  FILLER                   PIC X(36).
            05  H2-TITLE                 PIC X(12) VALUE 'STOCK REPORT'.
            05 FILLER                   PIC X(30).
        01 WS-HEADING-3.
            05 FILLER                   PIC X(1).
            05 H3-CAT-NAME              PIC X(3) VALUE 'CAT'.
            05 FILLER                   PIC X(11).
            05 H3-ITEM-NAME             PIC X(4) VALUE 'ITEM'.
            05 FILLER                   PIC X(11).
            05 H3-PURCHACE-NAME         PIC X(8) VALUE 'PURCHACE'.
            05 FILLER                   PIC X(3).
            05 H3-QUANTITY-NAME         PIC X(8) VALUE 'QUANTITY'.
            05 FILLER                   PIC X(4).
            05 H3-QUANTITY2-NAME        PIC X(8) VALUE 'QUANTITY'.
            05 FILLER                   PIC X(4).
            05 H3-REORDER-NAME          PIC X(7) VALUE 'REORDER'.
            05 FILLER                   PIC X(7).
		01 WS-HEADING-4.
		    05 FILLER                   PIC X(1).
            05 H4-NUM-NAME              PIC X(3) VALUE 'NUM'.
            05 FILLER                   PIC X(8).
            05 H4-DESC-NAME             PIC X(11) VALUE 'DESCRIPTION'.
            05 FILLER                   PIC X(8).
            05 H4-PRICE-NAME            PIC X(5) VALUE 'PRICE'.
            05 FILLER                   PIC X(6).
            05 H4-STOCK-NAME            PIC X(6) VALUE 'IN STK'.
            05 FILLER                   PIC X(5).
            05 H4-ORDER-NAME            PIC X(8) VALUE 'ON ORDER'.
            05 FILLER                   PIC X(5).
            05 H4-POINT-NAME            PIC X(5) VALUE 'POINT'.
            05 FILLER                   PIC X(8).
			
			
		


      *DECLARE ANY VARIABLES IN THIS AREA.


       01 DETAIL-LINE.
      * CREATE A PLACE FOR OUTPUT USE 05 LEVELS INCLUDE NAME,
      * 
      * 
            05 CATALOG-NUMBER-OUT		PIC X(5).
            05 FILLER                   PIC X(3) VALUE SPACES.
            05 ITEM-DESCRIPTION-OUT		PIC X(20).
            05 FILLER                   PIC X(3) VALUE SPACES.
            05 UNIT-PURCHASE-PRICE-OUT	PIC $999.99.
            05 FILLER                   PIC X(4) VALUE SPACES.
            05 QUANTITY-IN-STOCK-OUT	PIC X(6).
            05 FILLER                   PIC X(6) VALUE SPACES.
            05 QUANTITY-ON-ORDER-OUT	PIC X(6).
            05 FILLER                   PIC X(5) VALUE SPACES.
            05 REORDER-POINT-OUT		PIC X(6).
            05 FILLER                   PIC X(8) VALUE SPACES.
			


       PROCEDURE DIVISION.

       100-MAIN-MODULE.

           PERFORM 125-HOUSEKEEPING
           PERFORM 150-READ-RECORDS
           PERFORM 250-CLOSE-ROUTINE

           .



       125-HOUSEKEEPING.

      *SAME FILE NAME AS SELECT STATEMENT
           OPEN    INPUT	PROGRAM-FILE
                   OUTPUT    REPORT-FILE
				   
				   
			ACCEPT WS-DATE FROM DATE
			MOVE WS-YEAR TO H1-YEAR
			MOVE WS-MONTH TO H1-MONTH
			MOVE WS-DAY TO H1-DAY
			MOVE WS-HEADING-1 TO REPORT-REC
			WRITE REPORT-REC AFTER ADVANCING 1 LINE.
			MOVE WS-HEADING-2 TO REPORT-REC
			WRITE REPORT-REC AFTER ADVANCING 2 LINES.
			MOVE WS-HEADING-3 TO REPORT-REC
			WRITE REPORT-REC AFTER ADVANCING 3 LINES.
			MOVE WS-HEADING-4 TO REPORT-REC
			WRITE REPORT-REC AFTER ADVANCING 1 LINE.
           .


       150-READ-RECORDS.

             PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
                  READ PROGRAM-FILE
                      AT END
                          MOVE 'NO' TO ARE-THERE-MORE-RECORDS
                      NOT AT END
                          PERFORM 200-PROCESS-RTN
                  END-READ
              END-PERFORM
           .



       200-PROCESS-RTN.

      *MATCH YOUR DATANAMES TO THESE DATANAMES

		  MOVE CATALOG-NUMBER TO CATALOG-NUMBER-OUT
		  MOVE ITEM-DESCRIPTION TO ITEM-DESCRIPTION-OUT
		  MOVE UNIT-PURCHASE-PRICE TO UNIT-PURCHASE-PRICE-OUT
		  MOVE QUANTITY-ON-HAND TO QUANTITY-IN-STOCK-OUT
		  MOVE QUANTITY-ON-ORDER TO QUANTITY-ON-ORDER-OUT
		  MOVE REORDER-POINT TO REORDER-POINT-OUT
              

			  MOVE DETAIL-LINE TO REPORT-REC
              WRITE REPORT-REC
					AFTER ADVANCING 1 LINE.
           .

       250-CLOSE-ROUTINE.

      *SAME FILE NAME AS SELECT STATEMENT
              CLOSE	PROGRAM-FILE
                       REPORT-FILE
              STOP RUN
           .

