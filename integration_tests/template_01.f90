module template_compose_m
    implicit none
    private
    public :: op_t
  
    ! first adding a Requirement structure is the
    ! simplest way to process this
    requirement pure_oper(t, op)
      type, deferred :: t
      pure function op(x, y) result(z)
        type(t), intent(in) :: x, y
        type(t) :: z
      end function
    end requirement

    !template op_t(s, plus, minus)
    !    requires pure_oper(s, plus)
    !contains

       
    !end template
  
  contains
    
    subroutine test_template()
    end subroutine
    
  end module
    
  program template_compose
  use template_compose_m
  implicit none
  
  end program template_compose