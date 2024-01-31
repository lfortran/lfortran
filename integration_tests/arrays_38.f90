pure function linspace_n_1_cdp_cdp(n) result(res)
integer, intent(in) :: n
complex :: res(max(n, 0))

res = (1.0, 3.0)
end function linspace_n_1_cdp_cdp

program arrays_38
implicit none
integer :: n, i
complex :: res(5)
interface
pure function linspace_n_1_cdp_cdp(n) result(res)
integer, intent(in) :: n
complex :: res(max(n, 0))
end function linspace_n_1_cdp_cdp
end interface

res = linspace_n_1_cdp_cdp(5)
print *, res
do i = 1, 5
    if (abs(res(i) - (1.0, 3.0)) > 1e-8) error stop
end do
end program
