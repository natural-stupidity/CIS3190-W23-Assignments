-- Michael Sirna
-- 1094947
-- 2023-04-08
-- CIS3190 Assignment 4

with ada.text_io;                   use ada.text_io;
with ada.integer_text_io;           use ada.integer_text_io;
with ada.strings.unbounded;         use ada.strings.unbounded;
with ada.strings.unbounded.text_io; use ada.strings.unbounded.text_io;
with ada.directories;               use ada.directories;

procedure textyzer is

    -- File variables
    fileName           : unbounded_string;
    fileExists         : boolean := false;
    fileContent        : file_type;
    currentLine        : unbounded_string;

    -- Text Stats Variables
    characterCount     : integer := 0;
    letterCount        : integer := 0;
    wordCount          : integer := 0;
    digitCount         : integer := 0;
    numberCount        : integer := 0;
    spaceCount         : integer := 0;
    puncCount          : integer := 0;
    sentenceCount      : integer := 0;
    lineCount          : integer := 0;

    -- Average Count Variables (Letters Per Word, Words Per Setence)
    avgLW              : float := 0.0;
    avgWS              : float := 0.0;

    -- Histogram Variables (Assumes max word length of 45)
    wordLengthArray    : array (1..45) of integer;
    currWordLength     : integer := 0;
    maxWordLength      : integer := 0;
    mostFrequentLength : integer := 0;

    type fixed is delta 0.01 range -1.0e6 .. 1.0e6;




    -- Prompts the user to enter a file name.
    -- If the file exists, then it returns the file.
    -- Otherwise, the user is continually prompted until they enter a proper name.

    function getFileName return unbounded_string is
    begin
        while (not fileExists) loop

            -- Prompt user
            put ("Please enter a file name: ");
            get_line(fileName);
            put_line("");
            
            -- If the user enters something, check if the file exists
            -- If it doesn't exist, tell the user.
            if (fileName /= "") then
                fileExists := exists(to_string(fileName));

                if (not fileExists) then
                    put_line("Could not find file with the name: '" & fileName & "'");
                end if;
  
            -- If the user entered nothing / just pressed enter,
            -- Tell them and reprompt.
            else
                put_line("Please type a file name.");
            end if;
            
        end loop;

        return(fileName);
        
    end getFileName;




    -- Checks if the Decimal Form of a character is a punctuation
    -- @param char - The decimal form of the character we are checking.

    function isPunc(char : character) return boolean is
    begin
        -- Returns true if the character is any of the following characters
        if (char = '!' or char = '"' or char = ''' or char = '(' or char = ')' or char = ',' or char = '-' or char ='.' or char = ':' or char = ';' or char ='?') then
            
            -- Increment the sentence count if the current character is ending punctuation (! . or ?)
            if (char = '!' or char = '.' or char = '?') then
                sentenceCount := sentenceCount + 1;
            end if;

            return true;
        end if;
        
        return false;
    end isPunc;
    



    -- Checks if a string of text is a word and increments values accordingly.
    -- @param text - The string of text to parse through.

    function isWord(text : unbounded_string) return boolean is
        isWordBool    : boolean := true;
        containsAlpha : boolean := false;
    begin

        -- Loop through every character of the text
        for i in 1..length(text) loop

            -- If the current character is punctuation, increment the punctuation count.
            if (isPunc(element(text,i))) then
                puncCount := puncCount + 1;

            -- If the current character is A-Z or a-z, increment the various counts.
            elsif (element(text,i) in 'A'..'Z' or element(text,i) in 'a'..'z') then
                letterCount    := letterCount + 1;
                currWordLength := currWordLength + 1;
                containsAlpha  := true;

            -- If it's any other character / unknown, then it's not a word.
            else
                isWordBool := false;
                exit;
            end if;
        end loop;

        -- If the word doesn't contain any numbers, then it's not a number. Return false.
        -- i.e. if the word is only punctuation
        if (not containsAlpha) then
            isWordBool := false;
        end if;

        return isWordBool;
    end isWord;




    -- Checks if a string of text is a number and increments values accordingly
    -- @param text - The string of text to parse through.

    function isNumber(text : unbounded_string) return boolean is
        isNumBool   : boolean := true;
        containsNum : boolean := false;
    begin

        -- Loop through every character of the text
        for i in 1..length(text) loop

            -- If the current character is punctuation, increment the punctuation count.
            -- Although this wouldn't make sense with decimal numbers, the example given does this.
            if (isPunc(element(text,i))) then
                puncCount := puncCount + 1;

            -- If the current character is a 0-9, increment the digit count.
            elsif (element(text,i) in '0'..'9') then
                digitCount    := digitCount + 1;
                containsNum   := true;

            -- If it's any other character / unknown, then it's not a number.
            else
                isNumBool := false;
                exit;
            end if;
        end loop;

        -- If the word doesn't contain any numbers, then it's not a number. Return false.
        -- i.e. if the word is only punctuation
        if (not containsNum) then
            isNumBool := false;
        end if;

        return isNumBool;
    end isNumber;




    -- Runs a word through the helper functions above and increments values.
    -- @param tempWord - The word that will be evaluated.
    
    procedure checkWord(tempWord : in out unbounded_string) is
    begin

        -- If the tempWord is a word, then:
        if (isWord(tempWord)) then

            -- Increment the word count
            wordCount := wordCount + 1;

            -- Increment the index of the word's length by one
            wordLengthArray(currWordLength) := wordLengthArray(currWordLength) + 1;

            -- If the current word is the longest, then set maxWordLength accordingly.
            if (currWordLength > maxWordLength) then
                maxWordLength := currWordLength;
            end if;

            -- If the most frequent word length in the text is the current word's length, set mostFrequentLength accordingly.
            if (wordLengthArray(currWordLength) > mostFrequentLength) then
                mostFrequentLength := wordLengthArray(currWordLength);
            end if;

            -- Reset the current word length for the next word
            currWordLength := 0;


        -- If the tempWord is a number, increment the number count
        elsif (isNumber(tempWord)) then
            numberCount := numberCount + 1;
        end if;

        -- Reset the tempWord for the next word in the text.
        tempWord := null_unbounded_string;
    end checkWord;




    -- Opens a file and reads through all the text in the file.
    -- Extracts each word from the text then runs it through functions that calculate
    -- various stats about the text.
    -- Prints all stats at the end.
    -- @param fileName - The name of the file to open and read.

    procedure analyzeText(fileName : unbounded_string) is
        tempWord : unbounded_string;
    begin

        -- Open the file and store it's contents in the fileContent variable
        open(fileContent, in_file, to_string(fileName));

        while (not end_of_file(fileContent)) loop
            -- Process each line from the file
            get_line(fileContent,currentLine);

            for i in 1..length(currentLine) loop

                -- If the current Character's not a " ", then we add it to a word
                if (element(currentLine, i) /= ' ') then
                    append(tempWord, element(currentLine, i));
                
                -- If we detected a space, we test if tempWord is a word
                else
                    spaceCount := spaceCount + 1;
                    if (tempWord /= null_unbounded_string) then
                        checkWord(tempWord);
                    end if;
                end if;

                characterCount := characterCount + 1;

                -- If we've reached the end of the line and there's no space, check the word.
                -- Increment the line count too.
                if (i = length(currentLine) and tempWord /= null_unbounded_string and element(currentLine, i) /= ' ') then
                    checkWord(tempWord);
                    lineCount := lineCount + 1;
                end if;

            end loop;

            
        end loop;

        -- Catch any word that wasn't read cause we reached the end of the file.
        if (tempWord /= null_unbounded_string) then
            checkWord(tempWord);
        end if;

        if (wordCount /= 0) then
            avgLW := float(letterCount) / float(wordCount);
        end if;
        if (sentenceCount /= 0) then
            avgWS := float(wordCount) / float(sentenceCount);
        end if;

        put_line("-----------------------");
        put_line("      Text Stats:      ");
        put_line("-----------------------");
        put_line("Total Character Count : " & characterCount'image);
        put_line("Letter Count          : " & letterCount'image);
        put_line("Word Count            : " & wordCount'image);
        put_line("Digit Count           : " & digitCount'image);
        put_line("Number Count          : " & numberCount'image);
        put_line("Space Count           : " & spaceCount'image);
        put_line("Punctuation Count     : " & puncCount'image);
        put_line("Sentence Count        : " & sentenceCount'image);
        put_line("Line Count            : " & lineCount'image);
        put_line("Letters per word      : " & fixed(avgLW)'image);
        put_line("Words per sentence    : " & fixed(avgWS)'image);
    
    end analyzeText;




    -- Prints a vertical histogram of the word length distribution

    procedure printHist is
    begin

        -- Print a title and legend (I was too lazy to make the x-axis and y-axis show up.)
        put_line("------------------------------------");
        put_line("Histogram - Word Length Distribution");
        put_line("------------------------------------");
        put_line("         X AXIS: Word Length        ");
        put_line("         Y AXIS: Frequency          ");
        put_line("------------------------------------");
        put_line("");


        -- Start a loop from the most frequent length, and work our way down.
        -- So if the length that appeared the most had 20 entries, then we go from 20 to 1.
        for i in reverse 1..mostFrequentLength loop

            -- Print the y-axis first
            -- Add some spaces depending on the current length
            for z in 1..(4 - i'image'length) loop
                put(" ");
            end loop;

            put (i'image & " ");

            -- Go across every index of the wordLengthArray
            for j in 1..maxWordLength loop

                -- If the current index of the array is >= the row that we are at, we print a "*"
                -- So if there are 5 entries of a word length, assuming that's not the max, it won't get printed until we are 5 rows to the bottom.
                if (wordLengthArray(j) >= i) then
                    put(" *");

                -- Print a space if the word length entries are >= to the row we're on
                else
                    put("  ");
                end if;

                -- If we are in the column for word lengths >= 10, add an extra space at the end for nice formatting
                if (j >= 10) then
                    put(" ");
                end if;

            end loop;

            -- Add a new line for the next row.
            put_line("");

        end loop;

        put("     ");

        -- This prints the separator for the x-axis values
        -- If the longest word has more than 10 characters, use 3 dashes instead of two.
        -- Just some formatting stuff to make it look nice.
        if (maxWordLength >= 10) then
            for k in 1..(maxWordLength - 3) loop
                put("---");
            end loop;
        else
            for k in 1..maxWordLength loop
                put("--");
            end loop;
        end if;

        put_line("");
        put("     ");
        -- Puts the length of each "column" so you know how many occurences of each there were
        for l in 1..maxWordLength loop
            put(l'image);
        end loop;


        -- Horizontal histogram that I used before making a vertical one.
        --for i in 1..maxWordLength loop
        --    put(i'image & "  ");
        --    for j in 1..wordLengthArray(i) loop
        --        put("*");
        --    end loop;
        --    put(" " & wordLengthArray(i)'image);
        --    put_line("");\
        --end loop;

    end printHist;




begin
    put_line("");
    put_line("------------------------");
    put_line("      Text Analyzer     ");
    put_line("------------------------");
    put_line("");

    -- Initialize the wordLengthArray
    for i in 1..26 loop
        wordLengthArray(i) := 0;
    end loop;


    fileName := getFileName;
    put_line("");
    
    analyzeText(fileName);
    put_line("");
    put_line("");

    printHist;
    put_line("");
    put_line("");
    
end textyzer;
