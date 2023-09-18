*> Michael Sirna
*> 1094947
*> 2023-03-27
*> CIS3190 Assignment 3

identification division.
program-id. roman-numeral.
*> Program to convert roman numerals to their decimal equivalent        

environment division.

input-output section.
file-control.

*> Input file set to be read dynamically and read line by line
    select input-file assign to dynamic fname
        organization is line sequential.

data division.
file section.
fd  input-file.
    01 input-record   pic X(80).
    
working-storage section.
    77 fname          pic X(30).
    77 ws-end-of-file pic 9(1)     value 0.
    77 ws-end-of-line pic 9(1)     value 0.
    77 is-valid       pic 9(1)     value 1.
    77 input-line     pic X(80)    value " ".
    77 to-be-eval     pic X(1)     value " ".
    77 output-line    pic X(15)    value " ".
    77 evaluated      pic 9(10)    value 0.
    77 prev-val       pic 9(10)    value 0.
    77 curr-val       pic 9(10)    value 0.
    77 sum-val        pic 9(10)    value 0.
    77 i              pic S99 usage is computational.
    01 output-table-record.
       02 filler      pic X        value space.
       02 out-r       pic X(15).
       02 filler      pic X(3)     value spaces.
       02 v           pic Z(9).
    01 output-error-message.
       02 filler      pic X        value space.
       02 out-er-r    pic X(15).
       02 filler      pic X(3)     value spaces.
       02 filler      pic X(24)    value "Illegal Roman Numeral".

procedure division.

    *> Ask users for filename first.
    display " "
    display "Please enter the name of the file to read: "
        with no advancing.
    accept fname. 

    *> Print the title / table header.
    display " "
    display "   Roman Number Equivalents   ".
    display "------------------------------".
    display "|           Values           |".
    display "------------------------------".
    display "|           I = 1            |".
    display "|           V = 5            |".
    display "|           X = 10           |".
    display "|           L = 50           |".
    display "|           C = 100          |".
    display "|           D = 500          |".
    display "|           M = 1000         |".
    display "------------------------------".
    display "  ROMAN NUMBER     DEC. EQUIV.".
    display "------------------------------".
   
    
    *> Loop that reads the file until the end.
    open input input-file   
        perform until ws-end-of-file = 1
            read input-file
                at end move 1 to ws-end-of-file
                not at end

               *> Convert each line to uppercase and store it for conv.
                move function upper-case(input-record) to input-line
                perform conv

            end-read
        end-perform
    close input-file
stop run.

conv.
    *> Reset variables for the next loop
    move 1   to i
    move 1   to is-valid
    move 0   to ws-end-of-line
    move 0   to prev-val
    move 0   to curr-val
    move 0   to sum-val
    move " " to output-line
    
    *> Loop that reads the line until the end
    perform until ws-end-of-line = 1

        *> If the current element is a letter and not a space,
        *> add the letter to an output line.
        *> This will be used to print an output nicely.
        if input-line(i:1) is alphabetic and input-line(i:1) not = space
            string output-line delimited by space
                input-line(i:1) delimited by space
                into output-line
            end-string
        end-if

        *> If the current element is not a letter, is a space,
        *> or is not valid (i.e is not a valid roman numeral),
        *> Then we exit the loop
        if input-line(i:1) is not alphabetic or input-line(i:1) = space 
               or is-valid = 0
            move 1 to ws-end-of-line
            exit perform
        end-if

        *> Gets the value of the last roman numeral in the line. 
        *> This only happens if there is more than one element
        if i > 1
            move input-line(i - 1: 1) to to-be-eval
            perform get-value
            move evaluated to prev-val
        end-if

        *> Gets the value of the current roman numeral in the line.
        move input-line(i : 1) to to-be-eval
        perform get-value
        move evaluated to curr-val

        *> If the current numeral is bigger than the last,
        *> subtract twice the previous value from the current value
        if prev-val < curr-val
            compute curr-val = curr-val - (2 * prev-val)
        end-if

        *> Compute the current sum and increment the i
        compute sum-val = sum-val + curr-val
        compute i = i + 1
    
    end-perform
    
    *> Print out all values if the roman numeral is valid.
    *> Print an error message if it's not valid.
    if is-valid = 1
        move output-line to out-r
        move sum-val to v
        display output-table-record
    else 
        move output-line to out-er-r
        display output-error-message
    end-if.

get-value.
    evaluate to-be-eval
        when 'I'   move 1    to evaluated
        when 'V'   move 5    to evaluated
        when 'X'   move 10   to evaluated
        when 'L'   move 50   to evaluated
        when 'C'   move 100  to evaluated
        when 'D'   move 500  to evaluated
	    when 'M'   move 1000 to evaluated
        when other move 0    to is-valid
    end-evaluate.
    
