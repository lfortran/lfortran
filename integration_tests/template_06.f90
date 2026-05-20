module mod_template_06
    implicit none
    private
    public :: error_T

    template error_T(T, U)
        type, deferred :: T
        type, deferred :: U
        private
        public :: mycopy
      contains
        subroutine mycopy(lhs, rhs)
            type(T), intent(out) :: lhs
            type(T), intent(in) :: rhs
            lhs = rhs
        end subroutine
    end template

end module

program template_06
    print *, "Test drive"
end program