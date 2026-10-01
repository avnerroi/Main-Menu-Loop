
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOP-FUNCTIONS.

      *> Programmed by: Roi


      *> The four function categories are:
      *> 1. No argument, no return value
      *> 2. Arguments passed, no return value
      *> 3. No arguments, returns a value
      *> 4. Arguments passed, returns a value

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  MAIN-CHOICE             PIC 99 VALUE 0.
       01  SUB-CHOICE              PIC 99 VALUE 0.

      *> ---------------- "ARGUMENT" VARIABLES --------------------------
       01  LOOP-COUNT              PIC 9(4) VALUE 0.
       01  N-VAL                   PIC S9(9) VALUE 0.
       01  M-VAL                   PIC S9(9) VALUE 0.

      *> ---------------- "RETURN VALUE" VARIABLES ----------------------
       01  RET-INT                 PIC S9(18) VALUE 0.
       01  RET-BIN                 PIC X(32) VALUE SPACES.
       01  INPUT-OK                PIC X VALUE "Y".

      *> ---------------- WORKING VARIABLES -----------------------------
       01  CTR                     PIC S9(10) VALUE 0.
       01  TEMP-N                  PIC S9(10) VALUE 0.
       01  DIGIT                   PIC S9(9) VALUE 0.
       01  FIB-A                   PIC S9(20) VALUE 0.
       01  FIB-B                   PIC S9(20) VALUE 0.
       01  FIB-NEXT                PIC S9(20) VALUE 0.
       01  BIT-POS                 PIC 99 VALUE 0.
       01  BIT-DIGIT               PIC 9 VALUE 0.

      *> ---------------- EDITED FIELDS FOR NEAT OUTPUT -----------------
       01  ED-N                    PIC -(9)9.
       01  ED-INT                  PIC -(18)9.

       PROCEDURE DIVISION.

      *> ================================================================
      *> MAIN MENU
      *> ================================================================
       MAIN-PROGRAM.
           PERFORM UNTIL MAIN-CHOICE = 5
               DISPLAY " "
               DISPLAY "MAIN MENU"
               DISPLAY "1. No Argument + No Return"
               DISPLAY "2. Argument + No Return"
               DISPLAY "3. No Argument + Return"
               DISPLAY "4. Argument + Return"
               DISPLAY "5. Exit"
               DISPLAY "Enter your choice: "
               ACCEPT MAIN-CHOICE

               EVALUATE MAIN-CHOICE
                   WHEN 1
                       PERFORM FUNCTION-1-MENU
                   WHEN 2
                       PERFORM FUNCTION-2-MENU
                   WHEN 3
                       PERFORM FUNCTION-3-MENU
                   WHEN 4
                       PERFORM FUNCTION-4-MENU
                   WHEN 5
                       DISPLAY "Goodbye!"
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM

           STOP RUN.

      *> Shared list of the ten items shown in every function menu.
       SHOW-FUNCTION-LIST.
           DISPLAY "1. Print Name"
           DISPLAY "2. Print Numbers"
           DISPLAY "3. Even Numbers from 1 to N"
           DISPLAY "4. Even Numbers from N to M"
           DISPLAY "5. Sum of Odd Numbers from N to M"
           DISPLAY "6. Factorial"
           DISPLAY "7. Sum of the Digits"
           DISPLAY "8. Fibonacci"
           DISPLAY "9. Prime Number"
           DISPLAY "10. Binary"
           DISPLAY "11. Back"
           DISPLAY "Enter your choice: ".

      *> ================================================================
      *> FUNCTION 1 MENU - NO ARGUMENT AND NO RETURN VALUE
      *> Each paragraph asks for its own input and prints its result.
      *> ================================================================
       FUNCTION-1-MENU.
           MOVE 0 TO SUB-CHOICE

           PERFORM UNTIL SUB-CHOICE = 11
               DISPLAY " "
               DISPLAY "FUNCTION 1 MENU"
               PERFORM SHOW-FUNCTION-LIST
               ACCEPT SUB-CHOICE

               EVALUATE SUB-CHOICE
                   WHEN 1
                       PERFORM PRINT-NAME-1
                   WHEN 2
                       PERFORM PRINT-NUMBERS-1
                   WHEN 3
                       PERFORM EVEN-1-N-1
                   WHEN 4
                       PERFORM EVEN-N-M-1
                   WHEN 5
                       PERFORM SUM-ODD-1
                   WHEN 6
                       PERFORM FACTORIAL-1
                   WHEN 7
                       PERFORM DIGIT-SUM-1
                   WHEN 8
                       PERFORM FIBONACCI-1
                   WHEN 9
                       PERFORM PRIME-1
                   WHEN 10
                       PERFORM BINARY-1
                   WHEN 11
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM.

       PRINT-NAME-1.
           MOVE 5 TO LOOP-COUNT
           PERFORM DO-PRINT-NAME.

       PRINT-NUMBERS-1.
           MOVE 5 TO LOOP-COUNT
           PERFORM DO-PRINT-NUMBERS.

       EVEN-1-N-1.
           PERFORM GET-N
           PERFORM DO-EVEN-1-TO-N.

       EVEN-N-M-1.
           PERFORM GET-N-M
           PERFORM DO-EVEN-N-TO-M.

       SUM-ODD-1.
           PERFORM GET-N-M
           PERFORM DO-SUM-ODD
           PERFORM SHOW-SUM-ODD.

       FACTORIAL-1.
           PERFORM GET-N
           PERFORM DO-FACTORIAL
           PERFORM SHOW-FACTORIAL.

       DIGIT-SUM-1.
           PERFORM GET-N
           PERFORM DO-DIGIT-SUM
           PERFORM SHOW-DIGIT-SUM.

       FIBONACCI-1.
           PERFORM GET-NUMBER
           PERFORM DO-FIBONACCI.

       PRIME-1.
           PERFORM GET-NUMBER
           PERFORM DO-PRIME
           PERFORM SHOW-PRIME.

       BINARY-1.
           PERFORM GET-NUMBER
           PERFORM DO-BINARY
           PERFORM SHOW-BINARY.

      *> ================================================================
      *> FUNCTION 2 MENU - ARGUMENTS PASSED AND NO RETURN VALUE
      *> The menu (the "caller") fills in the variables first, then
      *> PERFORMs the paragraph, which prints the result.
      *> ================================================================
       FUNCTION-2-MENU.
           MOVE 0 TO SUB-CHOICE

           PERFORM UNTIL SUB-CHOICE = 11
               DISPLAY " "
               DISPLAY "FUNCTION 2 MENU"
               PERFORM SHOW-FUNCTION-LIST
               ACCEPT SUB-CHOICE

               EVALUATE SUB-CHOICE
                   WHEN 1
                       MOVE 5 TO LOOP-COUNT
                       PERFORM PRINT-NAME-2
                   WHEN 2
                       MOVE 5 TO LOOP-COUNT
                       PERFORM PRINT-NUMBERS-2
                   WHEN 3
                       PERFORM GET-N
                       PERFORM EVEN-1-N-2
                   WHEN 4
                       PERFORM GET-N-M
                       PERFORM EVEN-N-M-2
                   WHEN 5
                       PERFORM GET-N-M
                       PERFORM SUM-ODD-2
                   WHEN 6
                       PERFORM GET-N
                       PERFORM FACTORIAL-2
                   WHEN 7
                       PERFORM GET-N
                       PERFORM DIGIT-SUM-2
                   WHEN 8
                       PERFORM GET-NUMBER
                       PERFORM FIBONACCI-2
                   WHEN 9
                       PERFORM GET-NUMBER
                       PERFORM PRIME-2
                   WHEN 10
                       PERFORM GET-NUMBER
                       PERFORM BINARY-2
                   WHEN 11
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM.

       PRINT-NAME-2.
           PERFORM DO-PRINT-NAME.

       PRINT-NUMBERS-2.
           PERFORM DO-PRINT-NUMBERS.

       EVEN-1-N-2.
           PERFORM DO-EVEN-1-TO-N.

       EVEN-N-M-2.
           PERFORM DO-EVEN-N-TO-M.

       SUM-ODD-2.
           PERFORM DO-SUM-ODD
           PERFORM SHOW-SUM-ODD.

       FACTORIAL-2.
           PERFORM DO-FACTORIAL
           PERFORM SHOW-FACTORIAL.

       DIGIT-SUM-2.
           PERFORM DO-DIGIT-SUM
           PERFORM SHOW-DIGIT-SUM.

       FIBONACCI-2.
           PERFORM DO-FIBONACCI.

       PRIME-2.
           PERFORM DO-PRIME
           PERFORM SHOW-PRIME.

       BINARY-2.
           PERFORM DO-BINARY
           PERFORM SHOW-BINARY.

      *> ================================================================
      *> FUNCTION 3 MENU - NO ARGUMENTS AND RETURNS A VALUE
      *> Each paragraph asks for its own input, prints its result and
      *> leaves a value in RET-INT / RET-BIN, which the menu (the
      *> "caller") then displays.
      *> ================================================================
       FUNCTION-3-MENU.
           MOVE 0 TO SUB-CHOICE

           PERFORM UNTIL SUB-CHOICE = 11
               DISPLAY " "
               DISPLAY "FUNCTION 3 MENU"
               PERFORM SHOW-FUNCTION-LIST
               ACCEPT SUB-CHOICE

               EVALUATE SUB-CHOICE
                   WHEN 1
                       PERFORM PRINT-NAME-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 2
                       PERFORM PRINT-NUMBERS-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 3
                       PERFORM EVEN-1-N-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 4
                       PERFORM EVEN-N-M-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 5
                       PERFORM SUM-ODD-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 6
                       PERFORM FACTORIAL-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 7
                       PERFORM DIGIT-SUM-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 8
                       PERFORM FIBONACCI-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 9
                       PERFORM PRIME-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 10
                       PERFORM BINARY-3
                       PERFORM SHOW-RETURN-TEXT
                   WHEN 11
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM.

       PRINT-NAME-3.
      *> Returns how many times the name was printed.
           MOVE 5 TO LOOP-COUNT
           PERFORM DO-PRINT-NAME.

       PRINT-NUMBERS-3.
      *> Returns how many numbers were printed.
           MOVE 5 TO LOOP-COUNT
           PERFORM DO-PRINT-NUMBERS.

       EVEN-1-N-3.
      *> Returns how many even numbers were printed.
           PERFORM GET-N
           PERFORM DO-EVEN-1-TO-N.

       EVEN-N-M-3.
      *> Returns how many even numbers were printed.
           PERFORM GET-N-M
           PERFORM DO-EVEN-N-TO-M.

       SUM-ODD-3.
      *> Returns the sum of the odd numbers.
           PERFORM GET-N-M
           PERFORM DO-SUM-ODD
           PERFORM SHOW-SUM-ODD.

       FACTORIAL-3.
      *> Returns the factorial.
           PERFORM GET-N
           PERFORM DO-FACTORIAL
           PERFORM SHOW-FACTORIAL.

       DIGIT-SUM-3.
      *> Returns the sum of the digits.
           PERFORM GET-N
           PERFORM DO-DIGIT-SUM
           PERFORM SHOW-DIGIT-SUM.

       FIBONACCI-3.
      *> Returns the last Fibonacci number printed.
           PERFORM GET-NUMBER
           PERFORM DO-FIBONACCI.

       PRIME-3.
      *> Returns 1 (prime) or 0 (not prime).
           PERFORM GET-NUMBER
           PERFORM DO-PRIME
           PERFORM SHOW-PRIME.

       BINARY-3.
      *> Returns the binary form of the number (text).
           PERFORM GET-NUMBER
           PERFORM DO-BINARY
           PERFORM SHOW-BINARY.

      *> ================================================================
      *> FUNCTION 4 MENU - ARGUMENTS PASSED AND RETURNS A VALUE
      *> The menu fills in the variables, PERFORMs the paragraph (which
      *> "returns" a value), then the menu uses the returned value.
      *> ================================================================
       FUNCTION-4-MENU.
           MOVE 0 TO SUB-CHOICE

           PERFORM UNTIL SUB-CHOICE = 11
               DISPLAY " "
               DISPLAY "FUNCTION 4 MENU"
               PERFORM SHOW-FUNCTION-LIST
               ACCEPT SUB-CHOICE

               EVALUATE SUB-CHOICE
                   WHEN 1
                       MOVE 5 TO LOOP-COUNT
                       PERFORM PRINT-NAME-4
                       PERFORM SHOW-RETURN-INT
                   WHEN 2
                       MOVE 5 TO LOOP-COUNT
                       PERFORM PRINT-NUMBERS-4
                       PERFORM SHOW-RETURN-INT
                   WHEN 3
                       PERFORM GET-N
                       PERFORM EVEN-1-N-4
                       PERFORM SHOW-RETURN-INT
                   WHEN 4
                       PERFORM GET-N-M
                       PERFORM EVEN-N-M-4
                       PERFORM SHOW-RETURN-INT
                   WHEN 5
                       PERFORM GET-N-M
                       PERFORM SUM-ODD-4
                       PERFORM SHOW-SUM-ODD
                   WHEN 6
                       PERFORM GET-N
                       PERFORM FACTORIAL-4
                       PERFORM SHOW-FACTORIAL
                   WHEN 7
                       PERFORM GET-N
                       PERFORM DIGIT-SUM-4
                       PERFORM SHOW-DIGIT-SUM
                   WHEN 8
                       PERFORM GET-NUMBER
                       PERFORM FIBONACCI-4
                       PERFORM SHOW-RETURN-INT
                   WHEN 9
                       PERFORM GET-NUMBER
                       PERFORM PRIME-4
                       PERFORM SHOW-PRIME
                   WHEN 10
                       PERFORM GET-NUMBER
                       PERFORM BINARY-4
                       PERFORM SHOW-BINARY
                   WHEN 11
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM.

       PRINT-NAME-4.
      *> Prints the name LOOP-COUNT times, returns LOOP-COUNT.
           PERFORM DO-PRINT-NAME.

       PRINT-NUMBERS-4.
      *> Prints 1 to LOOP-COUNT, returns LOOP-COUNT.
           PERFORM DO-PRINT-NUMBERS.

       EVEN-1-N-4.
      *> Prints the even numbers 1 to N, returns how many were printed.
           PERFORM DO-EVEN-1-TO-N.

       EVEN-N-M-4.
      *> Prints the even numbers N to M, returns how many were printed.
           PERFORM DO-EVEN-N-TO-M.

       SUM-ODD-4.
      *> Returns the sum of the odd numbers from N to M.
           PERFORM DO-SUM-ODD.

       FACTORIAL-4.
      *> Returns the factorial of N.
           PERFORM DO-FACTORIAL.

       DIGIT-SUM-4.
      *> Returns the sum of the digits of N.
           PERFORM DO-DIGIT-SUM.

       FIBONACCI-4.
      *> Prints N Fibonacci numbers, returns the last one.
           PERFORM DO-FIBONACCI.

       PRIME-4.
      *> Returns 1 (prime) or 0 (not prime).
           PERFORM DO-PRIME.

       BINARY-4.
      *> Returns the binary form of N (text).
           PERFORM DO-BINARY.

      *> ================================================================
      *> GET-... PARAGRAPHS (these supply the "arguments")
      *> ================================================================
       GET-N.
           DISPLAY "Enter the value of N: "
           ACCEPT N-VAL.

       GET-N-M.
           DISPLAY "Enter the value of N: "
           ACCEPT N-VAL
           DISPLAY "Enter the value of M: "
           ACCEPT M-VAL.

       GET-NUMBER.
           DISPLAY "Enter Number: "
           ACCEPT N-VAL.

      *> ================================================================
      *> DO-... PARAGRAPHS (the actual loop logic)
      *> Results are left in RET-INT / RET-BIN. INPUT-OK is "N" when the
      *> input was rejected.
      *> ================================================================
       DO-PRINT-NAME.
           MOVE "Y" TO INPUT-OK
           PERFORM VARYING CTR FROM 1 BY 1 UNTIL CTR > LOOP-COUNT
               DISPLAY "Roi"
           END-PERFORM
           MOVE LOOP-COUNT TO RET-INT.

       DO-PRINT-NUMBERS.
           MOVE "Y" TO INPUT-OK
           PERFORM VARYING CTR FROM 1 BY 1 UNTIL CTR > LOOP-COUNT
               MOVE CTR TO ED-N
               DISPLAY FUNCTION TRIM(ED-N)
           END-PERFORM
           MOVE LOOP-COUNT TO RET-INT.

       DO-EVEN-1-TO-N.
           MOVE "Y" TO INPUT-OK
           MOVE 0 TO RET-INT
           PERFORM VARYING CTR FROM 1 BY 1 UNTIL CTR > N-VAL
               IF FUNCTION MOD(CTR, 2) = 0
                   MOVE CTR TO ED-N
                   DISPLAY FUNCTION TRIM(ED-N)
                   ADD 1 TO RET-INT
               END-IF
           END-PERFORM.

       DO-EVEN-N-TO-M.
           MOVE "Y" TO INPUT-OK
           MOVE 0 TO RET-INT
           PERFORM VARYING CTR FROM N-VAL BY 1 UNTIL CTR > M-VAL
               IF FUNCTION MOD(CTR, 2) = 0
                   MOVE CTR TO ED-N
                   DISPLAY FUNCTION TRIM(ED-N)
                   ADD 1 TO RET-INT
               END-IF
           END-PERFORM.

       DO-SUM-ODD.
           MOVE "Y" TO INPUT-OK
           MOVE 0 TO RET-INT
           PERFORM VARYING CTR FROM N-VAL BY 1 UNTIL CTR > M-VAL
               IF FUNCTION MOD(CTR, 2) = 1
                   ADD CTR TO RET-INT
               END-IF
           END-PERFORM.

       DO-FACTORIAL.
      *> 19! is the largest factorial that fits in RET-INT.
           MOVE "Y" TO INPUT-OK
           IF N-VAL < 0 OR N-VAL > 19
               DISPLAY "Invalid. Enter a value from 0 to 19."
               MOVE "N" TO INPUT-OK
               MOVE 0 TO RET-INT
           ELSE
               MOVE 1 TO RET-INT
               PERFORM VARYING CTR FROM 1 BY 1 UNTIL CTR > N-VAL
                   COMPUTE RET-INT = RET-INT * CTR
               END-PERFORM
           END-IF.

       DO-DIGIT-SUM.
           MOVE "Y" TO INPUT-OK
           MOVE 0 TO RET-INT
           COMPUTE TEMP-N = FUNCTION ABS(N-VAL)
           PERFORM UNTIL TEMP-N = 0
               COMPUTE DIGIT = FUNCTION MOD(TEMP-N, 10)
               ADD DIGIT TO RET-INT
               DIVIDE TEMP-N BY 10 GIVING TEMP-N
           END-PERFORM.

       DO-FIBONACCI.
      *> Prints N terms (1, 1, 2, 3, 5, ...) and returns the last one.
      *> 87 is the largest N whose term fits in RET-INT.
           MOVE "Y" TO INPUT-OK
           MOVE 0 TO RET-INT
           IF N-VAL < 1 OR N-VAL > 87
               DISPLAY "Invalid. Enter a value from 1 to 87."
               MOVE "N" TO INPUT-OK
           ELSE
               MOVE 0 TO FIB-A
               MOVE 1 TO FIB-B
               PERFORM VARYING CTR FROM 1 BY 1 UNTIL CTR > N-VAL
                   MOVE FIB-B TO ED-INT
                   DISPLAY FUNCTION TRIM(ED-INT)
                   COMPUTE FIB-NEXT = FIB-A + FIB-B
                   MOVE FIB-B TO FIB-A
                   MOVE FIB-NEXT TO FIB-B
               END-PERFORM
               MOVE FIB-A TO RET-INT
           END-IF.

       DO-PRIME.
      *> RET-INT = 1 when N-VAL is prime, otherwise 0.
           MOVE "Y" TO INPUT-OK
           IF N-VAL <= 1
               MOVE 0 TO RET-INT
           ELSE
               MOVE 1 TO RET-INT
               PERFORM VARYING CTR FROM 2 BY 1
                   UNTIL CTR * CTR > N-VAL OR RET-INT = 0
                   IF FUNCTION MOD(N-VAL, CTR) = 0
                       MOVE 0 TO RET-INT
                   END-IF
               END-PERFORM
           END-IF.

       DO-BINARY.
      *> The bits are filled in from the right end of RET-BIN.
           MOVE "Y" TO INPUT-OK
           MOVE SPACES TO RET-BIN
           IF N-VAL < 0
               DISPLAY "Invalid. Enter 0 or a positive number."
               MOVE "N" TO INPUT-OK
           ELSE
               IF N-VAL = 0
                   MOVE "0" TO RET-BIN
               ELSE
                   MOVE N-VAL TO TEMP-N
                   MOVE 32 TO BIT-POS
                   PERFORM UNTIL TEMP-N = 0
                       COMPUTE BIT-DIGIT = FUNCTION MOD(TEMP-N, 2)
                       MOVE BIT-DIGIT TO RET-BIN(BIT-POS:1)
                       DIVIDE TEMP-N BY 2 GIVING TEMP-N
                       SUBTRACT 1 FROM BIT-POS
                   END-PERFORM
               END-IF
           END-IF.

      *> ================================================================
      *> SHOW-... PARAGRAPHS (print the result from the stored values)
      *> ================================================================
       SHOW-SUM-ODD.
           MOVE RET-INT TO ED-INT
           DISPLAY "Sum of all odd numbers = " FUNCTION TRIM(ED-INT).

       SHOW-FACTORIAL.
           IF INPUT-OK = "Y"
               MOVE N-VAL TO ED-N
               MOVE RET-INT TO ED-INT
               DISPLAY "Factorial of " FUNCTION TRIM(ED-N) " is: "
                   FUNCTION TRIM(ED-INT)
           END-IF.

       SHOW-DIGIT-SUM.
           MOVE N-VAL TO ED-N
           MOVE RET-INT TO ED-INT
           DISPLAY "The sum of digits of " FUNCTION TRIM(ED-N) " is "
               FUNCTION TRIM(ED-INT).

       SHOW-PRIME.
           IF RET-INT = 1
               DISPLAY "Prime"
           ELSE
               DISPLAY "Not Prime"
           END-IF.

       SHOW-BINARY.
           IF INPUT-OK = "Y"
               DISPLAY "Binary = " FUNCTION TRIM(RET-BIN)
           END-IF.

      *> Used by the FUNCTION 3 and 4 menus to show the returned value.
       SHOW-RETURN-INT.
           IF INPUT-OK = "Y"
               MOVE RET-INT TO ED-INT
               DISPLAY "Return value: " FUNCTION TRIM(ED-INT)
           END-IF.

       SHOW-RETURN-TEXT.
           IF INPUT-OK = "Y"
               DISPLAY "Return value: " FUNCTION TRIM(RET-BIN)
           END-IF.
