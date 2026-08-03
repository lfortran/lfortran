module struct_tbp_codegen_m
  type t
   contains
    procedure :: tbp => f
  end type
 contains
  function f(this)
    class(t), intent(in) :: this
    real :: f
    f = 0.0
  end function
end module

program struct_tbp_codegen
  use struct_tbp_codegen_m
  implicit none
  type(t), target :: xt
end program
