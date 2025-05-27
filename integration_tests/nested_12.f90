module struct_var_nested_12
    type :: toml_value
        integer :: key = 0
    end type toml_value
end module

module nested_12_mod
    use struct_var_nested_12, only: toml_lexer => toml_value
contains
    subroutine char_num()
        type(toml_lexer) :: lexer
        lexer%key = 5
        call temp()
    contains
        subroutine temp()
            call semantics(lexer)
        end subroutine
    end subroutine

    subroutine semantics(lexer)
        class(toml_lexer), intent(inout) :: lexer
        if(lexer%key /= 5) error stop
    end subroutine semantics
end module 

program nested_12
    use struct_var_nested_12
    use nested_12_mod
   call char_num()
end program