! A syntax error inside a module makes the parser skip the erroneous
! declaration and keep the rest of the module. The symbol table visitor then
! skips the contained procedures that depend on the discarded declaration, so
! the body visitor must not assume their symbols exist.
module module_error_recovery_01
    type :: t
    contains
      foo    end type t
contains
    pure function foo(self, x) result(res)
      class(t), intent(in) :: self
      real, intent(in) :: x(:)
      real :: res(size(x))
    end function foo
end module
