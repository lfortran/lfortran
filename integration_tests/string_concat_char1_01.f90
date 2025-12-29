program string_concat_char1_01
    ! MRE for string concatenation buffer overflow with CHARACTER*1 dummies
    ! When a CHARACTER*1 dummy receives a longer actual argument,
    ! concatenation should use declared length (1), not actual length.
    ! Bug: uses actual length causing buffer overflow.
    implicit none
    
    call test_concat('Upper', 'Diag')
    print *, "PASS"
    
contains
    subroutine test_concat(a, b)
        character, intent(in) :: a, b  ! Both are CHARACTER*1
        character(2) :: result
        
        ! a should be 'U' (first char of 'Upper'), b should be 'D' (first char of 'Diag')
        ! result should be 'UD' (length 2)
        ! Bug: concatenation uses len('Upper')=5 + len('Diag')=4 = 9 chars
        !      causing buffer overflow into 2-char result
        result = a // b
        
        print *, "len(a)=", len(a), " len(b)=", len(b)
        print *, "result='", result, "'"
        
        if (len(a) /= 1) error stop "len(a) should be 1"
        if (len(b) /= 1) error stop "len(b) should be 1"
        if (result /= 'UD') error stop "result should be 'UD'"
    end subroutine
end program
