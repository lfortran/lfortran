module template_compose_m
  implicit none
  private
  public :: op_t

  requirement pure_oper(t, op)
    type, deferred :: t
    pure function op(x, y) result(z)
      type(t), intent(in) :: x, y
      type(t) :: z
    end function
  end requirement
  
  ! define current_template_args
    ! define them in ast_body_visitor for now, then move them later
  template op_t(t, plus, times)
    ! create a type parameter x (if x is not t)
    ! create an external symbol that points plus to op
    ! then we can remove some fields
    requires pure_oper(t, plus)
    requires pure_oper(t, times)
    private
    public :: op_generic
  contains
    function op_generic(x, y) result(z)
      type(t), intent(in) :: x, y
      type(t) :: z
      z = times(x, plus(x, y))
    end function
  end template

contains
  
  subroutine test_template()
  end subroutine
  
end module
  
program template_compose
use template_compose_m
implicit none

end program template_compose