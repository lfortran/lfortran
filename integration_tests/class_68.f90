module class_68_mod
  implicit none
  type, abstract :: toml_lexer
  contains
    procedure(func_interface), deferred :: f   ! deferred binding
  end type toml_lexer

  abstract interface
     subroutine func_interface(self)
       import :: toml_lexer
       class(toml_lexer), intent(inout) :: self
     end subroutine func_interface
  end interface

end module

module class_68_mod2 
    use class_68_mod
 type, extends(toml_lexer) :: mocked_lexer
    integer :: key
  contains
    procedure :: f => f_mock
  end type
    contains 
  subroutine f_mock(self)
    class(mocked_lexer), intent(inout) :: self
    print *, "Mock function called"
    self%key = 100
  end subroutine f_mock
  subroutine temp(lex)
    class(toml_lexer), intent(inout) :: lex
    call lex%f()
  end subroutine
end module

program class_68
    use class_68_mod2
    type(mocked_lexer) :: x
    call temp(x)
    if (x%key /= 100) error stop
end program 