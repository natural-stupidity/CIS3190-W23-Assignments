! Michael Sirna
! 1094947
! 2023-02-06
! CIS3190 Assignment 1

MODULE jmodule

    ! Derived Type for a word
    ! word              - The original jumbled word
    ! solvedWord        - The solved jumbled word
    ! anagramList       - The list of possible anagrams for a specific word
    ! numberOfAnagrams  - The maximum number of anagrams for a word (factorial(len(word))
    ! wordLength        - The length of the word. 
    TYPE JUMBLED_WORD
        CHARACTER(len=:), ALLOCATABLE               :: word
        CHARACTER(len=:), ALLOCATABLE               :: solvedWord
        CHARACTER(len=8), DIMENSION(:), ALLOCATABLE :: anagramList
        INTEGER                                     :: numberOfAnagrams, wordLength
    END TYPE JUMBLED_WORD
    
    CONTAINS
    
    
    ! A subroutine that deallocates all allocatable variables in the JUMBLED_WORD type
    SUBROUTINE freeJumbledWords(list)
        TYPE(JUMBLED_WORD), DIMENSION(:), ALLOCATABLE :: list
        
        ! Loop through all the words and deallocate
        DO i = 1, numberOfWords
            DEALLOCATE(list(i)%word)
            DEALLOCATE(list(i)%solvedWord)
        END DO
        DEALLOCATE(list)
    END SUBROUTINE freeJumbledWords
    
    
    ! A subroutine that gets the input from the user.
    SUBROUTINE inputJumble(jumbledWord, i)
        USE lexicon
        IMPLICIT NONE
        TYPE(JUMBLED_WORD), INTENT(out) :: jumbledWord
        CHARACTER(LEN=8)                :: word
        INTEGER                         :: i
        
        ! First write the index number for the word, then prompt for input
        WRITE(*, "(4g0)", ADVANCE="no") i, ". "
        READ (*,*)  word
        ! Trim the empty spaces off the word and get the wordLength for allocation
        jumbledWord%wordLength = LEN(TRIM(word))
        
        ! Allocate space for the word and solvedWord
        ALLOCATE(CHARACTER(jumbledWord%wordLength) :: jumbledWord%word)
        ALLOCATE(CHARACTER(jumbledWord%wordLength) :: jumbledWord%solvedWord)
        
        ! Trim the empty space from the word and set the word pointer.
        jumbledWord%word = TRIM(word)
        ! Make the word lowercase for easier searching
        CALL lowercase(jumbledWord%word)
    
    END SUBROUTINE inputJumble
    
    
    ! A subroutine that uses recursion to create anagrams of a given word.
    ! Each anagram is added to an anagramList
    RECURSIVE SUBROUTINE generateAnagram(currentWord, i)
        TYPE(JUMBLED_WORD), INTENT(INOUT) :: currentWord
        INTEGER, INTENT(IN)               :: i
        INTEGER                           :: j
    
        ! If the index is the same as the wordLength, we add it to the anagramList
        ! Otherwise, we use a recursive call to swap characters and find more anagrams and continue the same process
        IF (i == currentWord%wordLength) THEN
            currentWord%anagramList(currentWord%numberOfAnagrams) = currentWord%word
            currentWord%numberOfAnagrams = currentWord%numberOfAnagrams + 1
        ELSE
            DO j = i, currentWord%wordLength
                CALL swap(currentWord%word, i, j)
                CALL generateAnagram(currentWord, i + 1)
                CALL swap(currentWord%word, i, j)
            END DO
        END IF
    END SUBROUTINE generateAnagram
    
    
     ! A simple swap function that swaps characters in a string
    SUBROUTINE swap(str, i, j)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(INOUT) :: str
        INTEGER, INTENT(IN)             :: i, j
        CHARACTER(LEN=1)                :: temp
        
        temp = str(i:i)
        str(i:i) = str(j:j)
        str(j:j) = temp
    END SUBROUTINE swap
    
    
    ! A subroutine that finds the word in the dictionary. 
    ! If found, it sets solvedWord and exits the loop.
    ! Deallocate at the end
    RECURSIVE SUBROUTINE findAnagram(currentWord)
        USE lexicon
        TYPE(JUMBLED_WORD), INTENT(INOUT)          :: currentWord
        INTEGER                                    :: i
        LOGICAL                                    :: exists
        CHARACTER(LEN=:), ALLOCATABLE              :: currentAnagram
        
        ALLOCATE(CHARACTER(currentWord%wordLength) :: currentAnagram)
        
        DO i = 1, SIZE(currentWord%anagramList)
            currentAnagram = TRIM(currentWord%anagramList(i))
            CALL findlex(currentAnagram, exists)
            IF (exists) THEN
                currentWord%solvedWord = currentAnagram
                EXIT
            END IF
        END DO
        DEALLOCATE(currentAnagram)
    END SUBROUTINE
    
    
    ! https://www.bottomscience.com/factorial-of-a-number-fortran-95/
    ! A function that finds the factorial of a number
    INTEGER FUNCTION factorial(n)
        INTEGER, INTENT(IN) :: n
        factorial = 1
        
        DO i = 2, n
            factorial = factorial * i
        END DO
    END FUNCTION factorial
    
END MODULE jmodule

PROGRAM solvejumble
    USE lexicon
    USE jmodule
    IMPLICIT NONE
    
    TYPE(JUMBLED_WORD), DIMENSION(:), ALLOCATABLE :: jumbledWordList
    TYPE(JUMBLED_WORD)                            :: finalWord
    INTEGER                                       :: ierror, numberOfWords, i, j, finalWordLength
    CHARACTER                                     :: shouldSolveJumble
    CHARACTER(LEN=8), DIMENSION(:), ALLOCATABLE   :: temp
    
    ! Build the dicitonary and welcome the user
    CALL buildlexicon()
    WRITE(*,*)
    WRITE(*,"(4g0)") "Welcome to the Word Jumble Solver!"
    
    ! Get the number of jumbled words.
    ! Keep looping if the user doesn't enter an integer
    DO
        WRITE(*,"(4g0)", ADVANCE="no") "How many jumbled words are there?: "
        READ(*,*, IOSTAT=ierror) numberOfWords
        
        IF (ierror == 0) THEN
            EXIT
        END IF
        WRITE(*,*)
        WRITE(*,"(4g0)") "Please enter an integer!"
        WRITE(*,*)
    END DO

    ALLOCATE(jumbledWordList(numberOfWords))
    
    ! Prompt the user for the jumbled words and use inputjumble to store them into the jumbledWordList
    WRITE(*,*)
    WRITE(*,"(4g0)") "Please enter the ", numberOfWords, " jumbled words:"
    DO i = 1, numberOfWords
        CALL inputJumble(jumbledWordList(i), i)
    END DO
    
    ! Tell the user loading so they don't think it broke since this part might take a while
    WRITE(*,*)
    WRITE(*,"(4g0)") "Loading..."
    WRITE(*,*)
    
    ! A loop that generates a list of anagrams, then solves by looping through the dicitonary.
    DO i = 1, numberOfWords
        ALLOCATE(jumbledWordList(i)%anagramList(factorial(jumbledWordList(i)%wordLength)))
        Jumbledwordlist(i)%numberOfAnagrams = 1
        CALL generateAnagram(jumbledWordList(i), 1)
        CALL findAnagram(jumbledWordList(i))
    END DO
    
    ! Show the solved words
    WRITE(*,"(4g0)") "The following jumbles have been solved:"
    DO i = 1, numberOfWords
        CALL uppercase(jumbledWordList(i)%word)
        WRITE(*,"(4g0)") jumbledWordList(i)%word, ACHAR(9), ACHAR(9), jumbledWordList(i)%solvedWord
    END DO
    
    ! Ask the user if they want to solve the jumble. If they type anything other than Y, y, YES, or yes, the program ends
    WRITE(*,*)
    WRITE(*,"(4g0)", ADVANCE="no") "Would you like to solve the jumble puzzle (Y/N)?: "
    READ(*,*) shouldSolveJumble
    CALL lowercase(shouldSolveJumble)
    
    IF (shouldSolveJumble == "y" .or. shouldSolveJumble == "yes") THEN
        ALLOCATE(temp(numberOfWords))
        
        ! First prompt the user for the circled letters and count the number of letters while excluding white spaces
        finalWordLength = 0
        WRITE(*,*)
        WRITE(*,"(4g0)") "Please type in the circled letters from the word puzzle: "
        DO i = 1, numberOfWords
            WRITE(*,"(4g0)", ADVANCE="no") i, ". ", jumbledWordList(i)%solvedWord, " - "
            READ(*,"(A)") temp(i)
            DO j = 1, LEN(temp(i))
                IF (temp(i)(J:J) /= " ") THEN
                    finalWordLength = finalWordLength + 1
                END IF
            END DO
        END DO
        
        ! Then allocate the finalword's word so we can store stuff in it
        ALLOCATE(CHARACTER(finalWordLength) :: finalWord%word)
        finalWord%numberOfAnagrams = 1
        
        ! Loop through again to store each letter as a final word
        DO i = 1, numberOfWords
            DO j = 1, LEN(temp(i))
                IF (temp(i)(J:J) /= " ") THEN
                    finalWord%word(finalWord%numberOfAnagrams : finalWord%numberOfAnagrams) = temp(i)(J:J)
                    finalWord%numberOfAnagrams = finalWord%numberOfAnagrams + 1
                END IF
            END DO
        END DO
        
        ! Show that we have what the final jumbled word is and tell the user we're solving 
        ! cause this can take a really long time
        WRITE(*,*)
        WRITE(*,"(4g0)") "Final Jumbled Word: ", finalWord%word
        WRITE(*,"(4g0)") "Solving Puzzle..."
        
        ! Get the word length and do the same steps as the loop above to solve the puzzle
        finalWord%wordLength = LEN(TRIM(finalWord%word))
        ALLOCATE(CHARACTER(finalWord%wordLength) :: finalWord%solvedWord)
        
        ALLOCATE(finalWord%anagramList(factorial(finalWord%wordLength)))
        finalWord%numberOfAnagrams = 1
        
        CALL generateAnagram(finalWord, 1)
        CALL findAnagram(finalWord)
        
        ! Show the solved word
        WRITE(*,*)
        WRITE(*,"(4g0)") "Solved Jumble: ", finalWord%solvedWord
    END IF
    
    ! Tell the user that it's the end of the program and free the lists
    WRITE(*,*)
    WRITE(*,"(4g0)") "Thank you for using the Word Jumble Solver! Goodbye!"
    CALL freeJumbledWords(jumbledWordList)
    CALL freeList()
    
END PROGRAM solvejumble
