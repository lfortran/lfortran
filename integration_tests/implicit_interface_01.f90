program main

integer, dimension(2, 4) :: a
integer :: b(2) = [10, 20]
integer :: m = 4, i, j, n = 2

do 20 j = 1, n
        do 10 i = 1, m
            a(j, i) = i*j
   10    continue
   20 continue

call driver(implicit_interface_check, a, b, a(1, m), n, m)


contains

subroutine driver(fnc, arr, b, c, n, m)
integer, intent(in) :: n, m, c, b(n), arr(n, m)
call fnc(arr(1, m), b, c, n, m)
end subroutine

subroutine implicit_interface_check(arr_e, b, c, n, m)
integer, intent(in) :: n, m, b(n), c, arr_e
if (arr_e /= m) error stop
if (b(1) /= 10) error stop
if (b(2) /= 20) error stop
if (c /= m) error stop
end subroutine

end program
