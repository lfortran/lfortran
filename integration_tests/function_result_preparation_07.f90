module function_result_preparation_07_m
    implicit none
    integer :: cnt = 0
contains
    integer function f()
        cnt = cnt + 1
        f = 3
    end function

    function h(n) result(r)
        integer, intent(in) :: n
        integer :: r(n)
        r = n
    end function
end module

program function_result_preparation_07
    use function_result_preparation_07_m
    implicit none
    integer :: arr(4), brr(4)
    arr = [1, 2, 3, 4]

    brr = arr + h(1 + f())
    if (cnt /= 1) error stop 1
    if (any(brr /= [5, 6, 7, 8])) error stop 2

    cnt = 0
    brr = h(1 + f())
    if (cnt /= 1) error stop 3
    if (any(brr /= 4)) error stop 4

    cnt = 0
    if (size(h(1 + f())) /= 4) error stop 5
    if (cnt /= 1) error stop 6
end program
