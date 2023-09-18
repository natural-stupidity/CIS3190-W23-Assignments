with polylink; use polylink;

package polymath is

    -- Adds a new term to a polynomial given two coefficients and their exponenents.
    -- @param p - The polynomial we will add the new term to.
    -- @param a - The coefficient of the first term we are adding
    -- @param b - The coefficient of the second term we are adding
    -- @param exp - The common exponent between both terms
    procedure addpoly (p : in out Polynomial; a : in Float; b : in Float; exp : in Integer);

    -- Subtracts a new term to a polynomial given two coefficients and their exponenents.
    -- @param p - The polynomial we will add the new term to.
    -- @param a - The coefficient of the first term we are subtracting
    -- @param b - The coefficient of the second term we are subtracting
    -- @param exp - The common exponent between both terms
    procedure subpoly (p : in out Polynomial; a : in Float; b : in Float; exp : in Integer);

    -- Adds or subtracts terms of polynomials based on the type
    -- @param a - The first polynomial we are adding or subtracting
    -- @param b - The second polynomial we are adding or subtracting
    -- @param t - The type; tells the function whether we are adding or subtracting
    function aspoly (a : in Polynomial; b : in Polynomial; t : String) return Polynomial;

    -- Multiplies the terms of two polynomials using the FOIL method.
    -- @param a - The first polynomial we are multiplying
    -- @param b - The second polynomial we are multiplying
    function multpoly (a : in Polynomial; b : in Polynomial) return Polynomial;

    -- Evaluates a polynomial given an "x" value.
    -- @param a - The polynomial we will evaluate.
    -- @param x - The value of "x" in the polynomial
    function evalpoly (a : in Polynomial; x : in Float) return Float;

end polymath;
