module struct_tbp_codegen_12296_m
  type t
   contains
    procedure :: tbp => f
  end type
 contains
  function f(this)
    class(t), pointer, intent(in) :: this
    real :: f
    f = 0.0
  end function
end module

program struct_tbp_codegen_12296
  use struct_tbp_codegen_12296_m
  implicit none
  type(t), target :: xt
end program
