module lt_m_template_07
    implicit none
    private
    public :: my_type, lt_my_type

    type my_type
        real :: d
    end type

    interface operator(<)
       procedure lt_my_type
    end interface

contains

    pure elemental function lt_my_type(lhs, rhs) result(res)
        type(my_type), intent(in) :: lhs, rhs
        logical :: res
        res = lhs%d < rhs%d
    end function

end module

module template_apply_m_template_07
    implicit none
    private
    public :: apply_t, no_args_t

    ! R1633: the deferred-arg-name-list of a REQUIREMENT is optional
    requirement no_args_r {}
    end requirement

    requirement op_r {T, U, V, op_func}
        deferred type :: T
        deferred type :: U
        deferred type :: V
        deferred interface
            pure elemental function op_func(lhs, rhs) result(res)
                type(T), intent(in) :: lhs
                type(U), intent(in) :: rhs
                type(V) :: res
            end function
        end interface
    end requirement

    template apply_t(T, lt)
        ! R1636: the :: is optional and the instantiation-arg-spec-list may be empty
        require no_args_r {}
        require op_r {T, T, logical, lt}
        private
        public :: apply_lt
    contains
        pure function apply_lt(lhs, rhs) result(res)
            type(T), intent(inout) :: lhs(:)
            type(T), intent(inout) :: rhs(:)
            logical :: res
            res = all ( lt(lhs,rhs) )
        end function
    end template

    ! R1602: the deferred-arg-name-list of a TEMPLATE is optional
    template no_args_t()
        private
        public :: answer
    contains
        pure function answer() result(res)
            integer :: res
            res = 42
        end function
    end template
end module

program template_07
        use lt_m_template_07
        use template_apply_m_template_07

        ! R1625: the :: is optional and the instantiation-arg-spec-list may be empty
        instantiate :: apply_t {my_type,operator(<)}, only : my_apply => apply_lt
        instantiate :: no_args_t {}


        type(my_type) :: a(5), b(5)
        type(my_type) :: x
        type(my_type) :: y

        x = my_type(5)
        y = my_type(6)
        a = x
        b = y

        print *, x < y
        if (.not. (x < y)) error stop 

        print *, "a=", a
        print *, "b=", b

        print *, my_apply(a,b)
        if (.not. my_apply(a, b)) error stop

        print *, answer()
        if (answer() /= 42) error stop
end program