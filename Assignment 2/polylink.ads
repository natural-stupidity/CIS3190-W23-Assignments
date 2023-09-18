package polylink is

    type Term;

    type Pointer is access Term;

    type Term is record
        coefficient : Float;
        exponent    : Integer;
        next        : Pointer := null;
    end record;

    type Polynomial is record
        head : Pointer := null;
    end record;

    type polyList is array (1 .. 10) of Polynomial;
    
    pList : polyList;

    -- Adds a new term to the end of a polynomial.
    -- @param p - The polynomial we are adding a term to.
    -- @param c - The coefficent of the term.
    -- @param e - The exponent of the term.
    procedure appendTerm (p : in out Polynomial; c : in Float; e : in Integer);
    
    -- Removes a term from a polynomial
    -- @param p - The polynomial we are removing a term from.
    -- @param c - The coefficent of the term.
    -- @param e - The exponent of the term.
    procedure removeTerm (p : in out Polynomial; c : in Float; e : in Integer);

    -- Takes user input to create one or multiple polynomials and
    -- add them to the polyList.
    procedure readPOLY;

    -- Gets the polynomial at an index and uses the writePOLY(polynomial) 
    -- procedure to print it.
    -- @param index - The index of the polynomial we want to print
    procedure writePOLY (index: in Integer);

    -- Prints a polynomial in a readable format.
    -- Takes all terms, and prints them individually with their corresponding
    -- signs in between (i.e "+" or "-").
    -- @param p - the polynomial we want to print
    procedure writePOLY (p : in Polynomial);

    -- Prints all polynomials in the list.
    procedure printAll;

    -- Gets a polynomial from the polyList given an index.
    -- @param index - The index of the polynomial we are looking for.
    -- @returns The polynomial at that index.
    function getPoly (index : in Integer) return Polynomial;

    -- Sorts a polynomial from highest exponent in descending order.
    -- This function is only really needed after multiplication so
    -- The polynomial is printed properly
    -- @param p - The polynomial we want to sort.
    -- @returns The sorted polynomial
    function sortPoly (p : Polynomial) return Polynomial;
    
    -- The menu for adding and subtracting polynomials. 
    -- Asks the user for inputs and calls functions needed to add or subtract.
    -- @param t - The type; whether we will be adding or subtracting
    procedure asMenu (t : String);

    -- The menu for multiplying polynomials.
    -- Asks the user for inputs and calls functions needed to multiply.
    procedure multPolyMenu;
    
    -- The menu for evaluating polynomials.
    -- Asks the user for inputs and calls functions needed to evaluate.
    procedure evalPolyMenu;

    -- Checks if the number of elements in the polylist is greater than
    -- or equal to a certian number.
    -- @param i - The number that the arraylist needs to be to return true
    -- @returns True if the arraysize >= i. Otherwise, false.
    function checkArraySize (i : Integer) return Boolean;

    -- Checks if a user's polynomial selection is a valid option.
    -- Pretty much; checks if the polynomial they want is null or not.
    -- If it's null returns false;
    -- @param i - The index of the polynomial the user wants
    -- @returns True if the polynomial exists.
    function checkValidOption(i : Integer) return Boolean;

    -- Originally, "cleared" the terminal in VS Code (Just sets text to the top of the terminal).
    -- I found out that doesn't work for most other terminals, so I just made it print a couple new lines
    -- Makes things less messy.
    procedure clearScreen;

end polylink;
