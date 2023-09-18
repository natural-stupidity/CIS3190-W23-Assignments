-- Michael Sirna
-- 1094947
-- 2023-03-03
-- CIS3190 Assignment 2

with Ada.Text_IO; use Ada.Text_IO;
with polylink;    use polylink;

procedure poly is
    menuChoice : Integer;
begin
    menuChoice := -1;
    clearScreen;
    Put_Line ("POLYNOMIAL Arithmetic");

    while (menuChoice /= 7) loop
        New_Line;
        Put_Line("1. Add Polynomial(s) to List");
        Put_Line("2. Print All Stored Polynomials");
        Put_Line("3. Add Two Polynomials");
        Put_Line("4. Subtract Two Polynomials");
        Put_Line("5. Multiply Two Polynomials");
        Put_Line("6. Evaluate Polynomial");
        Put_Line("7. Exit");
        New_Line;
        Put_Line("Please input an action (Numbers 1 - 7): ");
        menuChoice := Integer'value (Get_Line);

        clearScreen;
        case menuChoice is
            when 1 =>
                readPOLY;
            when 2 =>
                printAll;
            when 3 =>
                asMenu("add");
            when 4 =>
                asMenu("sub");
            when 5 =>
                multPolyMenu;
            when 6 =>
                evalPolyMenu;
            when 7 =>
                Put_Line ("Exiting Program....");
                Put_Line ("Goodbye!");
                New_Line;
            when others =>
                Put_Line ("Invalid menu choice!");
        end case;
    end loop;
end poly;
