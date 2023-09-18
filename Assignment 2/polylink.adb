with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with polymath;                          use polymath;

package body polylink is


     procedure appendTerm (p : in out Polynomial; c : in Float; e : in Integer) is
        newTerm :  constant Pointer := new Term'(coefficient => c, exponent => e, next => null);
        tail : Pointer := p.head;
    begin

        -- If the list is empty, the head becomes the new term
        -- Otherwise, we make the tail of the last term the new term.
        if (tail = null) then
            p.head := newTerm;
        else
            while (tail.next /= null) loop
                tail := tail.next;
            end loop;
            tail.next := newTerm;
        end if;
    end appendTerm;




    procedure removeTerm (p : in out Polynomial; c : in Float; e : in Integer)
    is
        prevTerm : Pointer := null;
        currentTerm : Pointer := p.head;
    begin

        -- Loop through the list and if the current term has the same 
        -- coefficient and exponent, remove it.
        while (currentTerm /= null) loop
            if (currentTerm.coefficient = c and currentTerm.exponent = e) then
                if (prevTerm = null) then
                    p.head := currentTerm.next;
                else
                    prevTerm.next := currentTerm.next;
                end if;
                return;
            end if;
            prevTerm := currentTerm;
            currentTerm := currentTerm.next;
        end loop;
    end removeTerm;




    procedure readPOLY is
        tempPoly        : Polynomial;
        numberOfPoly    : Integer := 0;
        highestExp      : Integer := 0;
        tempCoefficent  : Float   := 0.0;
        emptySpot       : Boolean := False;
        overwriteIndex  : Integer := 0;
    begin

        -- Get the number of polynomials the person wants to enter.
        -- If the person types 0, it tells the user what they did and goes back to the menu.
        Put_Line("How many polynomials would you like to enter?: ");
        numberOfPoly := Integer'value (Get_Line);

        if (numberOfPoly = 0) then
            clearScreen;
            Put_Line("You entered 0 for the number of polynomials to enter. Going back to menu...");
        end if;

        -- Start a loop from 1 to the number of polynomials the person wants to enter
        for i in 1..numberOfPoly loop

            New_Line;

            -- If the user is entering multiple, state which polynomial they are entering
            if (numberOfPoly > 1) then
                New_Line;
                Put_Line("--- POLYNOMIAL" & i'image & "---");
            end if;

            -- Get the highest exponent from the user so we can start
            -- This will be used for the next loop to get the coefficient for each term.
            while (highestExp <= 0) loop
                Put_Line("What is the highest exponent?: ");
                highestExp := Integer'value (Get_Line);

                -- The highest exponent must be greater than 0. If the user enters someothing else, then they are alerted.
                if (highestExp <= 0) then
                    New_Line;
                    Put_Line("Polynomial's highest exponent must be greater than 0!");
                    New_Line;
                end if;
            end loop;
            
            New_Line;

            -- Start a loop using the highest exponent. It asks for each coefficient individually.
            -- For example, if the highest exponent is 2, it'll as for x^2, x^1, then x^0 in that order
            for j in 0..highestExp loop
                Put_Line("What is the coefficient for term" & Integer'image(j + 1) & " (x^" & Trim(Integer(highestExp - j)'image, Ada.Strings.Left) & ")?: ");
                tempCoefficent := Float'value (Get_Line);
                New_Line;

                -- If the user entered 0 for the coefficient, then don't add the term.
                if (tempCoefficent /= 0.0) then
                    appendTerm (tempPoly, tempCoefficent, highestExp - j);
                end if;
            end loop;

            -- If the user entered 0 for all terms, then tell the user it wasn't added and go back to the menu.
            if (tempPoly.head = null) then
                clearScreen;
                Put_Line("Polynomial was not added (Polynomial cannot be null!).");
                Put_Line("Returning to menu....");
                exit;
            end if;

            -- Loop through the polyList and try to find a spot for the new polynomial.
            -- If found, it adds it to the list
            for i in pList'range loop
                if (pList(i).head = null) then
                    pList(i)  := tempPoly;
                    emptySpot := True;
                    if (i = numberOfPoly) then
                        clearScreen;
                    end if;
                    Put_Line("Polynomial added successfully!");
                    exit;
                end if;
            end loop;

            -- If the previous loop doesn't find a spot, then tell the user.
            -- Prints the list to show the user which polynomials are in the list,
            -- then as the user which one they want to replace. Once they pick a valid index,
            -- that polynomial is replaced in the list.
            if (not emptySpot) then
                Put_Line ("All spots are full:");
                printAll;
                New_Line;
                Put_Line("Please enter the number corresponding with the polynomial you would like to replace:");
                overwriteIndex := Integer'value (Get_Line);

                while (overwriteIndex <= 0 and overwriteIndex > 10) loop
                    Put_Line ("Please enter a valid index!");
                    overwriteIndex := Integer'value (Get_Line);
                end loop;

                pList (overwriteIndex) := tempPoly;
                if (i = numberOfPoly) then
                    clearScreen;
                end if;
                Put_Line("Polynomial replaced successfully!");
            end if;

            --Reset the variables for the loop.
            highestExp     := 0;
            tempPoly.head  := null;
            emptySpot      := False;
            overwriteIndex := 0;
        end loop;
    end readPOLY;




    procedure writePOLY (index : in Integer) is
    begin
        writePOLY (pList(index));
    end writePOLY;




    procedure writePOLY (p : in Polynomial) is
        currentTerm   : Pointer := p.head;
        firstTermDone : Boolean := False;
        type Fixed is delta 0.01 range -1.0e6 .. 1.0e6;
    begin
        while (currentTerm /= null) loop

            -- If the currentTerm is not the first term in the polynomial
            -- And it's negative, we print a "-". Otherwise, we print a "+"
            if (firstTermDone and currentTerm.coefficient /= 0.0) then
                Put(if (currentTerm.coefficient < 0.0) then " - " else " + ");
            end if;

            -- If the coefficient isn't 0 and isn't a 1 on a variable,
            -- Then print the absolute value of the coefficient so we don't have a negative sign
            if ((currentTerm.exponent >= 1 and (abs currentTerm.coefficient) /= 1.0) or (currentTerm.exponent = 0 and (abs currentTerm.coefficient) > 0.0)) then
                Put (Trim(Fixed(abs currentTerm.coefficient)'image, Ada.Strings.Left));
            end if;

            -- If the current term has an exponent, print "x" if the exponent is 1, and x^n if it's more than 1
            if (currentTerm.exponent /= 0) then
                Put(if (currentTerm.exponent = 1) then "x" else "x^" & Trim(currentTerm.exponent'image, Ada.Strings.Left));
            end if;

            -- Increment the currentTerm and set the firstTerm to done
            currentTerm  := currentTerm.next;
            firstTermDone := True;
        end loop;
        New_Line;
    end writePOLY;




    procedure printAll is
        temp : Polynomial;
    begin

        -- Loop through all the elements in the list
        -- If there is nothing in the first index, tell the user there
        -- are not polynomials in the list.
        -- Otherwise, print the index number and the polynomial in that position.
        for i in pList'range loop
            temp := pList (i);
            if (temp.head = null) then
                if (i = 1) then
                    Put_Line("There are no polynomials in the list!");
                end if;
                exit;
            else
                Put (i'image & ": ");
                writePOLY (i);
            end if;
        end loop;
    end printAll;




    function getPoly(index : in Integer) return Polynomial is
    begin
        return pList(index);
    end getPoly;




    function sortPoly (p : Polynomial) return Polynomial is
        tempPoly   : Polynomial := p;
        sortedPoly : Polynomial;
    begin

        -- A general insertion sort type of sorting.
        -- If the previous node's exponent is less than the current node,
        -- the previous node is set to the current node.
        -- Whatever happens, the previous node is added to a sortedList,
        -- and removed from the temporary polynomial so we don't see it again.
        while (tempPoly.head /= null) loop
            declare
                prevNode : Pointer := tempPoly.head;
                currentNode : Pointer := prevNode.next;
            begin
                while (currentNode /= null) loop
                    if (currentNode.exponent > prevNode.exponent) then
                        prevNode := currentNode;
                    end if;
                    currentNode := currentNode.next;
                end loop;
                appendTerm (sortedPoly, prevNode.coefficient, prevNode.exponent);
                removeTerm (tempPoly, prevNode.coefficient, prevNode.exponent);
            end;
        end loop;
        return sortedPoly;
    end sortPoly;



    procedure asMenu (t : String)  is
        validOption : Boolean := False;
        poly1       : Integer;
        poly2       : Integer;
    begin

        -- If there are at least 2 polynomials in the list, then we can
        -- proceed, otherwise we alert the user and go back to the menu.
        if (checkArraySize(2)) then

            -- Print the polyList so the user knows which ones are in the list.
            printAll;
            New_Line;

            -- Loop until the user picks a polynomial from the list
            while (not validOption) loop
                Put_Line ("Please enter the number corresponding to the first polynomial you'd like to " & t & ":");
                poly1 := Integer'value (Get_Line);
                validOption := checkValidOption(poly1);
            end loop;

            --Print a new line and reset the validOption Boolean
            New_Line;
            validOption := False;

            -- Loop until the user picks another polynomial from the list
            while (not validOption) loop
                Put_Line ("Please enter the number corresponding to the second polynomial you'd like to " & t & ":");
                poly2 := Integer'value (Get_Line);
                validOption := checkValidOption(poly2);
            end loop;

            -- Once we get the user's choice for adding, both choices are printed in the format:
            --     polynomial 1
            -- +/- polynomial 2
            -- ---------------------
            --     sum or difference
            clearScreen;
            Put("   ");
            writePOLY(getPoly(poly1));
            Put(if (t = "add") then " + " else " - ");
            writePOLY(getPoly(poly2));
            Put_Line("----------------------------------------");
            Put("   ");

            -- At the end, we call the asPoly function and pass whether the type
            -- is add or sub so we can calculate properly.
            writePOLY (aspoly (getPoly (poly1), getPoly (poly2), t));
        else
            Put_Line ("There are not enough polynomials in the list!");
        end if;
    end asMenu;




    procedure multPolyMenu is
        validOption : Boolean := False;
        poly1     : Integer;
        poly2     : Integer;
    begin

        -- If there are at least 2 polynomials in the list, then we can
        -- proceed, otherwise we alert the user and go back to the menu.
        if (checkArraySize(2)) then
            
            -- Print the polyList so the user knows which ones are in the list.
            printAll;
            New_Line;
            
            -- Loop until the user picks a polynomial from the list
            while (not validOption) loop
                Put_Line ("Please enter the number corresponding to the first polynomial you'd like to multiply:");
                poly1 := Integer'value (Get_Line);
                validOption := checkValidOption(poly1);
            end loop;
            
            --Print a new line and reset the validOption Boolean
            New_Line;
            validOption := False;

            -- Loop until the user picks a polynomial from the list
            while (not validOption) loop
                Put_Line ("Please enter the number corresponding to the second polynomial you'd like to multiply:");
                poly2 := Integer'value (Get_Line);
                validOption := checkValidOption(poly2);
            end loop;
            
            -- Once we get the user's choice for adding, both choices are printed in the format:
            --    polynomial 1
            --  * polynomial 2
            --  --------------------
            --    product
            clearScreen;
            Put("   ");
            writePOLY(getPoly(poly1));
            Put(" * ");
            writePOLY(getPoly(poly2));
            Put_Line("----------------------------------------");
            Put("   ");
            writePOLY (multpoly (getPoly (poly1), getPoly (poly2)));
        else
            Put_Line ("There are not enough polynomials in the list!");
        end if;
    end multPolyMenu;




    procedure evalPolyMenu is
        validOption : Boolean := False;
        option  : Integer;
        x        : Float;
        type Fixed is delta 0.01 range -1.0e6 .. 1.0e6;
    begin
        
        -- If there are at least 1 polynomial in the list, then we can
        -- proceed, otherwise we alert the user and go back to the menu.
        if (checkArraySize(1)) then
            printAll;
            New_Line;

            -- Loop until the user picks a polynomial from the list
            while (not validOption) loop
                Put_Line ("Please enter the number corresponding to the polynomial you'd like to evaluate:");
                option := Integer'value (Get_Line);
                validOption := checkValidOption(option);
            end loop;
            
            New_Line;

            Put_Line ("What is the value of x:");
            x := Float'value (Get_Line);


            -- Once we get the polynomial and x value, the original polynomial is
            -- printed, then the answer is printed in the format:
            --  f(x) = polynomial 1
            --  f(inputed number) = evaluated number
            clearScreen;
            Put("f(x) = ");
            writePOLY(getPoly(option));
            Put("f(" & Trim(Fixed(x)'image, Ada.Strings.Left) & ") = ");
            Put(Trim(Fixed(evalpoly(getPoly(option), x))'image, Ada.Strings.Left));
            New_Line;
        else
            Put_Line ("There are not enough polynomials in the list!");
        end if;
    end evalPolyMenu;




    function checkArraySize (i : Integer) return Boolean is
        temp     : Polynomial;
        counter : Integer := 0;
    begin 

        -- Loop through all elements in the list and count as we pass each one
        -- If the number of elements is greater than the i value, then return true
        for i in pList'range loop
            temp := pList (i);
            if (temp.head /= null) then
                counter := counter + 1;
            end if;
        end loop;

        if (counter >= i) then
            return True;
        else
            return False;
        end if;
    end checkArraySize;




    function checkValidOption(i : Integer) return Boolean is
    begin

        -- If the polynomial at the given index doesn't exist, tell the user
        -- and return false so the loop continues
        if (getPoly(i).head = null) then
            New_Line;
            Put_Line("There is no polynomial at that index!");
            New_Line;
            return False;
        end if;
        return True;
    end checkValidOption;




    procedure clearScreen is
        ---Control_Preamble : constant Character  := Character'Val (8#33#); -- '\033'
        ---Clear_Screen_Code: constant String     := "[2J";
        ---Home_Cursor_Code : constant String     := "[;H";
        ---Clear_Screen_Sequence: constant String := Control_Preamble & Clear_Screen_Code & Control_Preamble & Home_Cursor_Code;
    begin
        ---Put (Clear_Screen_Sequence);
        New_Line;
        Put_Line("----------------------------------------------------");
        New_Line;
    end clearScreen;

end polylink;
