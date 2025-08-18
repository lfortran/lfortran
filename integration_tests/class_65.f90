module class_65_mod_a
  implicit none
  type, abstract :: toml_lexer
  contains
    procedure(func_interface), deferred :: f
  end type toml_lexer

  abstract interface
     subroutine func_interface(self)
       import :: toml_lexer
       class(toml_lexer), intent(inout) :: self
     end subroutine func_interface
  end interface

end module

module class_65_mod_b 
    use class_65_mod_a
 type, extends(toml_lexer) :: mocked_lexer
    integer :: key = 1
  contains
    procedure :: f => f_mock
  end type
    contains 
  subroutine f_mock(self)
    class(mocked_lexer), intent(inout) :: self
    self%key = 100
  end subroutine f_mock
  subroutine temp(lex)
    class(toml_lexer), intent(inout) :: lex
    call lex%f()
  end subroutine
end module

program class_65
    use class_65_mod_b
    type(mocked_lexer) :: x
    call temp(x)
    if (x%key /= 100) error stop 
end program 
