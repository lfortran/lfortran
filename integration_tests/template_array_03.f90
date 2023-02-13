module math

    implicit none
    private
    public :: add_integer, zero_integer, add_real, zero_real, mult_integer, mult_real

contains

    pure function add_integer(x, y) result(r)
        integer, intent(in) :: x, y
        integer :: r
        r = x + y
    end function

    pure function zero_integer(x) result(r)
        integer, intent(in) :: x
        integer :: r
        r = 0
    end function

    pure function mult_integer(x, y) result(r)
        integer, intent(in) :: x, y
        integer :: r
        r = x * y
    end function

    pure function add_real(x, y) result(r)
        real, intent(in) :: x, y
        real :: r
        r = x + y
    end function

    pure function zero_real(x) result(r)
        real, intent(in) :: x
        real :: r
        r = 0
    end function

    pure function mult_real(x, y) result(r)
        real, intent(in) :: x, y
        real :: r
        r = x * y
    end function

end module

module template_array_03_m

    use math
    implicit none
    private
    public :: test_template

    requirement operations(t, plus_t, zero_t, mult_t)

        type :: t; end type

        pure function plus_t(l, r) result(result)
            type(t), intent(in) :: l, r
            type(t) :: result
        end function

        pure function zero_t(x) result(result)
            type(t), intent(in) :: x
            type(t) :: result
        end function

        pure function mult_t(l, r) result(result)
            type(t), intent(in) :: l, r
            type(t) :: result
        end function

    end requirement
!
    template array_tmpl(t, plus_t, zero_t, mult_t)

        requires operations(t, plus_t, zero_t, mult_t)
        private
        public :: mymatmul_t

    contains

        subroutine mymatmul_t(i, j, k, a, b, r)
            integer, parameter, intent(in) :: i, j, k
            type(t), intent(in) :: a(i,j), b(j,k)
            type(t) :: r(i,k)
            integer, parameter :: x, y, z
            type(t) :: elem
            do x = 1, i
                do z = 1, k
                    elem = zero_t(a(1,1))
                    do y = 1, j
                        elem = plus_t(elem, mult_t(a(x,y), b(y,z)))
                    end do
                    r(x,z) = elem
                end do
            end do
        end subroutine

    end template

contains

    subroutine test_template()
        integer :: arr(2,2)
        integer :: r(2,2)
        arr(1,1) = 1
        arr(1,2) = 1
        arr(2,1) = 0
        arr(2,2) = 1
        instantiate array_tmpl(integer, add_integer, zero_integer, mult_integer), &
            only: mymatmul_int => mymatmul_t
        call mymatmul_int(2, 2, 2, arr, arr, r)
        print *, r(1,1)
        print *, r(1,2)
        print *, r(2,1)
        print *, r(2,2)
    end subroutine

end module

program template_array_03

    use template_array_03_m
    implicit none

    call test_template()

end
