module class_47_mod
    implicit none
    type :: toml_lexer
        integer :: dummy = 0
    end type toml_lexer
contains
    subroutine wrapper(lexer)
        class(toml_lexer) :: lexer
        call temp()
    contains
        subroutine temp()
            call temp2(lexer)
        end subroutine temp
    end subroutine wrapper
    subroutine temp2(lex)
        class(toml_lexer), intent(in) :: lex
        print *, lex%dummy
        if (lex%dummy /= 123) error stop
    end subroutine temp2
end module class_47_mod

program class_47
    use class_47_mod
    implicit none
    type(toml_lexer) :: my_lexer
    my_lexer%dummy = 123
    call wrapper(my_lexer)
end program class_47
