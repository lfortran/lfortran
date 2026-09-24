! A deferred constant consumed through a requirement (`require ::`), with the
! deferred constant listed both after and before the deferred type in the
! requirement's argument list. Each instantiation must bind the type and the
! constant to its own arguments, independent of the order.

module template_deferred_const_02_m
    implicit none
    private
    public :: test_type_first, test_const_first

    integer, parameter :: three = 3, four = 4, five = 5, six = 6

    requirement r_tn {t, n}
        deferred type :: t
        deferred integer, parameter :: n
    end requirement

    requirement r_nt {n, t}
        deferred integer, parameter :: n
        deferred type :: t
    end requirement

    template tm_tn {t, n}
        require :: r_tn {t, n}
        private
        public :: get_n, fill
    contains
        function get_n() result(k)
            integer :: k
            k = n
        end function
        function fill(x) result(y)
            type(t), intent(in) :: x
            type(t) :: y(n)
            integer :: i
            do i = 1, n
                y(i) = x
            end do
        end function
    end template

    template tm_nt {n, t}
        require :: r_nt {n, t}
        private
        public :: get_n, twice
    contains
        function get_n() result(k)
            integer :: k
            k = n
        end function
        function twice(x) result(y)
            type(t), intent(in) :: x
            type(t) :: y
            y = x
        end function
    end template

contains

    subroutine test_type_first()
        instantiate tm_tn {real, three}, only: get_n_r => get_n, fill_r => fill
        instantiate tm_tn {integer, four}, only: get_n_i => get_n, fill_i => fill
        real :: a(3)
        integer :: b(4)
        if (get_n_r() /= 3) error stop
        if (get_n_i() /= 4) error stop
        a = fill_r(1.5)
        if (any(a /= 1.5)) error stop
        b = fill_i(7)
        if (any(b /= 7)) error stop
        print *, get_n_r(), a, get_n_i(), b
    end subroutine

    subroutine test_const_first()
        instantiate tm_nt {five, real}, only: get_n_r => get_n, twice_r => twice
        instantiate tm_nt {six, integer}, only: get_n_i => get_n, twice_i => twice
        real :: a
        integer :: b
        if (get_n_r() /= 5) error stop
        if (get_n_i() /= 6) error stop
        a = twice_r(2.5)
        if (a /= 2.5) error stop
        b = twice_i(9)
        if (b /= 9) error stop
        print *, get_n_r(), a, get_n_i(), b
    end subroutine

end module

program template_deferred_const_02
    use template_deferred_const_02_m, only: test_type_first, test_const_first
    implicit none
    call test_type_first()
    call test_const_first()
end program
