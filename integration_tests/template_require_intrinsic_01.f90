module template_require_intrinsic_01_m
    implicit none

    requirement binary_r{T, U, V, bin}
        deferred type :: T
        deferred type :: U
        deferred type :: V
        deferred interface
            function bin(x, y) result(z)
                type(T), intent(in) :: x
                type(U), intent(in) :: y
                type(V) :: z
            end function
        end interface
    end requirement

    ! A requirement reusing another requirement with an intrinsic type actual
    requirement predicate2_r{T, U, pred2}
        require :: binary_r{T, U, logical, pred2}
    end requirement

    requirement count_r{T, cnt}
        require :: binary_r{T, T, integer, cnt}
    end requirement

    template count_t{T, U, pred2}
        require :: predicate2_r{T, U, pred2}
    contains
        function count_if(xs, y) result(n)
            type(T), intent(in) :: xs(:)
            type(U), intent(in) :: y
            integer :: n
            integer :: i
            n = 0
            do i = 1, size(xs)
                if (pred2(xs(i), y)) n = n + 1
            end do
        end function
    end template

    template sum_t{T, cnt}
        require :: count_r{T, cnt}
    contains
        function sum_pairs(xs, ys) result(s)
            type(T), intent(in) :: xs(:), ys(:)
            integer :: s
            integer :: i
            s = 0
            do i = 1, size(xs)
                s = s + cnt(xs(i), ys(i))
            end do
        end function
    end template

contains

    function greater(x, y) result(r)
        integer, intent(in) :: x
        real, intent(in) :: y
        logical :: r
        r = real(x) > y
    end function

    function add_int(x, y) result(r)
        integer, intent(in) :: x, y
        integer :: r
        r = x + y
    end function

end module

program template_require_intrinsic_01
    use template_require_intrinsic_01_m
    implicit none
    instantiate count_t{integer, real, greater}, only: count_if
    instantiate sum_t{integer, add_int}, only: sum_pairs
    integer :: n, s
    n = count_if([1, 5, 3, 7], 2.5)
    print *, n
    if (n /= 3) error stop
    s = sum_pairs([1, 2, 3], [10, 20, 30])
    print *, s
    if (s /= 66) error stop
end program
