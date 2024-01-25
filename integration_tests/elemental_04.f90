program elemental_04

integer :: res(2)
res = is_close([1., 2.], 2.)
print *, res
if (any(res /= 5)) error stop

contains

    elemental integer function is_close(a, b) result(r)
    real, intent(in) :: a
    real, intent(in) :: b
    r = 5
    end function is_close

end program
