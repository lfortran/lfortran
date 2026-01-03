 module class_30_mod
    type :: abstract_lexer
    end type

   type :: stack_item
      integer :: scope
   end type stack_item

   type, extends(abstract_lexer) :: toml_lexer
      integer :: top = 1
      type(stack_item), allocatable :: stack(:)
   end type toml_lexer
end module

program class_30
    use class_30_mod
    type(toml_lexer) :: lexer
    allocate(lexer%stack(10))
    lexer%stack(lexer%top)%scope = 1
    call check_scope(lexer)

contains

    subroutine check_scope(lex)
        class(toml_lexer), intent(in) :: lex
        if (lex%stack(lex%top)%scope /= 1) error stop
    end subroutine check_scope
end program