program real16_host_assoc_01
implicit none
real(16) :: one
one = 1
print *, f()
if (f() /= 2) error stop
contains
    function f() result(r)
        real(16) :: r
        r = one * 2
    end function
end program
