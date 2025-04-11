module template_add_01c_m
    implicit none
    private
    public :: add_t, test_template

    requirement R(T, F)
        type, deferred :: T
        function F(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
        end function
    end requirement

    template add_t(T, F)
        require :: R(T, F)
        private
        public :: add_generic
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = F(x, y)
        end function
    end template

contains

  subroutine test_template()
      integer :: n
      instantiate add_t(integer, operator(+))
      n = add_generic(5, 9)
      print*, "The result is", n
  end subroutine
end module

program template_add_01c
use template_add_01c_m
implicit none

call test_template()

end program
