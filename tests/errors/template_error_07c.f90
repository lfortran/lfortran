module template_01_m
    implicit none
    private
    public :: op_t

    requirement semigroup(t, combine)
        type, deferred :: t
        elemental function combine(x, y) result(combined)
            type(t), intent(in) :: x, y
            type(t) :: combined
        end function
    end requirement
  
    requirement extended_semigroup(t, combine, sconcat, stimes)
        requires semigroup(t, scombine)
        pure function sconcat(list) result(combined)
            type(t), intent(in) :: list(:)
            type(t) :: combined
        end function
        elemental function stimes(n, a) result(repeated)
            integer, intent(in) :: n
            type(t), intent(in) :: a
            type(t) :: repeated
        end function
    end requirement

  contains
    
    subroutine test_template()
    end subroutine
    
  end module
    
  program template_01
  use template_01_m
  implicit none
  
  end program template_01