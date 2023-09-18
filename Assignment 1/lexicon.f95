! Michael Sirna
! 1094947
! 2023-02-06
! CIS3190 Assignment 1

MODULE lexicon
   
    ! Derived Type for a List Node
    ! word - The data / word for a node
    ! next - The pointer for the next node
    TYPE LIST_NODE
        CHARACTER(LEN=8)         :: word
        TYPE(LIST_NODE), POINTER :: next => null()
    END TYPE LIST_NODE
    TYPE(LIST_NODE), POINTER     :: head, tempNode

    CONTAINS
    
    ! A subroutine that adds a new node to the linked list
    SUBROUTINE addNode(newNode)
        IMPLICIT NONE
        TYPE(LIST_NODE), POINTER, INTENT(in) :: newNode
        TYPE(LIST_NODE), POINTER             :: temp
        temp => head

        ! Loop through each node in the list until the pointer to the next node is null
        DO
            IF (ASSOCIATED(temp%next)) THEN
                temp => temp%next
            ELSE
                EXIT
            END IF
        END DO
        ! Set the first null next value to the new node
        temp%next => newNode
    END SUBROUTINE
    
    ! A subroutine that goes through the list and frees each node
    SUBROUTINE freeList()
        IMPLICIT NONE

        tempNode => head%next
        DEALLOCATE(head)
        
        ! Continue freeing things until the pointer for the next node is false
        DO
            IF (ASSOCIATED(tempNode%next) .eqv. .false.) THEN
                EXIT
            END IF
            head => tempNode
            tempNode => tempNode%next
            DEALLOCATE(head)
        END DO
    END SUBROUTINE freeList
    

    ! Subroutine that makes the dictionary / lexicon
    SUBROUTINE buildlexicon()
        IMPLICIT NONE
        CHARACTER(LEN=12) :: fname = "./dict2.txt"
        CHARACTER(LEN=8)  :: currWord
        INTEGER           :: sts
        LOGICAL           :: lexist
        
        ! Initializing the list essentially
        ALLOCATE(head)
        head%word = ""
        head%next => null()
      
        ! Basic filename reading stuff from unit 3 / 4
        ! If the file exists, we read it, make each word lowercase, then store it in the list
        ! Otherwise, tell the user the dictionary wasn't found and end the program cause it won't work
        INQUIRE(FILE=fname, EXIST=lexist)
        
        IF (lexist) THEN
        
            ! Tell the user this might take a while cause it does. 
            ! Pretty much ensuring the user that it's working
            WRITE(*,"(4g0)") "Creating Dictionary. This might take a while..."
        
            OPEN (UNIT=1, FILE=fname, ACTION='read')

            DO
                READ(1,*,IOSTAT=sts) currWord
                call lowercase(currWord)

                IF (sts < 0) THEN 
                    EXIT
                ELSE 
                    IF (head%word == "") THEN
                        head%word = currWord
                    ELSE
                       ALLOCATE(tempNode)
                       tempNode%word = currWord
                        CALL addNode(tempNode)
                    END IF
                END IF
            END DO
            CLOSE(1)
        ELSE
            WRITE(*,"(4g0)") "ERROR: Could not find file dict2.txt. Please check your directory"
            STOP
        END IF
            
    END SUBROUTINE buildlexicon
    
    
    ! A subroutine that finds a word given to it in the dictionary
    ! Essentially just loops through the list until the word given is the same as the one in the node
    ! Returns true if the word is there.
    SUBROUTINE findlex(word, exists)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(in) :: word
        LOGICAL, INTENT(out)         :: exists
        LOGICAL                      :: nextNodeExists
        exists = .false.
        tempNode => head

        DO
            nextNodeExists = ASSOCIATED(tempNode%next)
            IF (trim(tempNode%word) == TRIM(word)) THEN
                exists = .true.
                EXIT
            END IF
            IF (nextNodeExists) THEN
                tempNode => tempNode%next
            ELSE
                EXIT
            END IF
        END DO

    END SUBROUTINE findlex

    ! Uppercase and lowercase are both modified subroutines from the unit 2 sheet
    SUBROUTINE uppercase(str)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(inout) :: str
        INTEGER                         :: i

        DO i = 1, LEN(str)
            IF (str(i:i) >= 'a' .and. str(i:i) <= 'z') THEN
                str(i:i) = ACHAR(IACHAR(str(i:i)) - 32)
            END IF
        END DO
    END SUBROUTINE uppercase
        
        
    SUBROUTINE lowercase(str)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(inout) :: str
        INTEGER                         :: i

        DO i = 1, LEN(str)
            IF (str(i:i) >= 'A' .and. str(i:i) <= 'Z') THEN
                str(i:i) = ACHAR(IACHAR(str(i:i)) + 32)
            END IF
        END DO
    END SUBROUTINE lowercase

END MODULE lexicon
