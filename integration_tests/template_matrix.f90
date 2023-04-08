module template_matrix_m
  implicit none
  private
  public :: matrix_t

  requirement pure_oper(t, op)
    type, deferred :: t
    pure function op(x, y) result(z)
      type(t), intent(in) :: x, y
      type(t) :: z
    end function
  end requirement

  requirement elemental_oper(t, op)
    type, deferred :: t
    elemental function op(x, y) result(z)
      type(t), intent(in) :: x, y
      type(t) :: z
    end function
  end requirement

  template matrix_t(t, plus, times, n)
    requires elemental_oper(t, plus)
    requires elemental_oper(t, times)
  end template

end module

program template_matrix
use template_matrix_m
implicit none

end program template_matrix