module template_array_03_math

    implicit none
    private
    public :: zero_integer, mult_integer

contains

    pure function zero_integer() result(r)
        integer :: r
        r = 0
    end function

    pure function zero_real() result(r)
        real :: r
        r = 0
    end function

end module

module template_array_03_m

    use template_array_03_math
    implicit none
    private
    public :: test_template

    requirement operations{t, plus_t, zero_t, mult_t}

        deferred type :: t

        pure function plus_t(l, r) result(result)
            type(t), intent(in) :: l, r
            type(t) :: result
        end function

        pure function zero_t() result(result)
            type(t) :: result
        end function

        pure function mult_t(l, r) result(result)
            type(t), intent(in) :: l, r
            type(t) :: result
        end function

    end requirement
!
    template array_tmpl{t, plus_t, zero_t, mult_t}

        require :: operations{t, plus_t, zero_t, mult_t}
        private
        public :: mymatmul_t

    contains

        subroutine mymatmul_t(i, j, k, a, b, r)
            integer, parameter, intent(in) :: i, j, k
            type(t), intent(in) :: a(i,j), b(j,k)
            type(t), intent(out) :: r(i,k)
            integer :: x, y, z
            type(t) :: elem
            do x = 1, i
                do z = 1, k
                    elem = zero_t()
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
        instantiate array_tmpl{integer, operator(+), zero_integer, operator(*)}, &
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

end program
