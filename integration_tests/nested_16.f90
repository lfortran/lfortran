module nested_16_mod
    implicit none
    type :: toml_lexer
        integer :: dummy = 0
    end type toml_lexer

contains

    subroutine temp2(lex)
        class(toml_lexer), intent(inout) :: lex
        lex%dummy = 400
    end subroutine temp2

    subroutine wrapper(lexer)
        class(toml_lexer) :: lexer
        call temp()
        if (lexer%dummy /= 400) error stop

    contains

        subroutine temp()
            call temp2(lexer)
        end subroutine temp

    end subroutine wrapper

end module nested_16_mod

program nested_16
    use nested_16_mod
    implicit none
    type(toml_lexer) :: my_lexer
    my_lexer%dummy = 123
    call wrapper(my_lexer)
end program nested_16
