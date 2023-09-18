package body polymath is


    procedure addpoly (p : in out Polynomial; a : in Float; b : in Float; exp : in Integer) is
    begin
        -- If the polynomials added is not 0, then add the coefficients and add the term to the polynomial
        if (a + b /= 0.0) then
            appendTerm (p, a + b, exp);
        end if;
    end addpoly; 




    procedure subpoly (p : in out Polynomial; a : Float; b : Float; exp : Integer) is
    begin
        -- If the polynomials subtracted is not 0, then subtract the coefficients and add the term to the polynomial
        if (a - b /= 0.0) then
            appendTerm (p, a - b, exp);
        end if;
    end subpoly; 




    function aspoly(a : Polynomial; b : Polynomial; t : String) return Polynomial is
        aSorted      : constant Polynomial := sortPoly(a);
        bSorted      : constant Polynomial := sortPoly(b);
        currentNodeA : Pointer             := aSorted.Head;
        currentNodeB : Pointer             := bSorted.Head;
        sod          : Polynomial;

    begin

        while (currentNodeA /= null and currentNodeB /= null) loop

            -- If the exponent of the currentNodeA > the exponent of the currentNodeB,
            -- Then add currentNodeA since there are no other nodes with that exponent.
            if (currentNodeA.exponent > currentNodeB.exponent) then
                appendTerm (sod, currentNodeA.coefficient, currentNodeA.exponent);
                currentNodeA := currentNodeA.next;
            
            -- Same with this but vice versa
            elsif (currentNodeA.exponent < currentNodeB.exponent) then
                appendTerm (sod, currentNodeB.coefficient, currentNodeB.exponent);
                currentNodeB := currentNodeB.next;

            -- If the two exponents are equal, add or subtract them depending on the value of t
            else
                if (t = "add") then
                    addpoly(sod, currentNodeA.coefficient, currentNodeB.coefficient, currentNodeA.exponent);
                else
                    subpoly(sod, currentNodeA.coefficient, currentNodeB.coefficient, currentNodeA.exponent);
                end if;
                currentNodeA := currentNodeA.next;
                currentNodeB := currentNodeB.next;
            end if;
        end loop;

        -- Grab any variables at the end that didn't get added or subtracted
        -- Because they were not compared.
        while (currentNodeA /= null) loop
            appendTerm (sod, currentNodeA.coefficient, currentNodeA.exponent);
            currentNodeA := currentNodeA.next;
        end loop;

        while (currentNodeB /= null) loop
            appendTerm (sod, currentNodeB.coefficient, currentNodeB.exponent);
            currentNodeB := currentNodeB.next;
        end loop;

        return sod;
    end aspoly;



    function multpoly (a : Polynomial; b : Polynomial) return Polynomial is
        aSorted      : constant Polynomial := sortPoly (a);
        bSorted      : constant Polynomial := sortPoly (b);
        currentNodeA : Pointer             := aSorted.Head;
        currentNodeB : Pointer             := bSorted.Head;
        product      : Polynomial;
        productNode  : Pointer             := product.Head;
        doNotAppend  : Boolean := False;

    begin
        while (currentNodeA /= null) loop
            while (currentNodeB /= null) loop

                -- Loop through the product polynomial.
                -- If the exponent of the currentNode of the product polynomial = the exponent of currentNodeA + the exponent of currentNodeB,
                -- Then we set the coefficient of the to the sum of the coefficients. 
                -- Essentially; add like terms
                -- Set doNotAppend so we don't repeat it later.
                while (productNode /= null) loop
                    if (productNode.exponent = (currentNodeA.exponent + currentNodeB.exponent)) then
                        productNode.coefficient := productNode.coefficient + (currentNodeA.coefficient * currentNodeB.coefficient);
                        doNotAppend := True;
                    end if;
                    productNode := productNode.next;
                end loop;
                
                -- If there wasn't any other term with the same coefficient, add it to the polynomial
                if (not doNotAppend) then
                    appendTerm(product, currentNodeA.coefficient * currentNodeB.coefficient, currentNodeA.exponent + currentNodeB.exponent);
                end if;

                --Reset everything to continue the loop
                currentNodeB := currentNodeB.next;
                productNode := product.Head;
                doNotAppend := False;
            end loop;
            currentNodeA := currentNodeA.next;
            currentNodeB := bSorted.Head;
        end loop;

        -- Sort the list at the end so it's nice to read
        product := sortPoly(product);

        return product;
    end multpoly;




    function evalpoly (a : Polynomial; x : Float) return Float is
        total        : Float   := 0.0;
        currentNodeA : Pointer := a.Head;
    begin
        -- Loop through the list and solve each term.
        -- Add each to a total.
        while (currentNodeA /= null) loop
            total := total + (currentNodeA.coefficient * x ** currentNodeA.exponent);
            currentNodeA := currentNodeA.next;
        end loop;

        return total;
    end evalpoly;

end polymath;
