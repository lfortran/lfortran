module array_constructor_06_mod
implicit none
contains
function copy0_vec(xx) result(yy)
    real, intent(in) :: xx(:)
    real, allocatable :: yy(:)
    integer :: i
    allocate(yy(size(xx)))
    yy = [ (copy_vec([xx(i)]), i=1,size(xx)) ]
end function copy0_vec

function copy_vec(xx) result(yy)
    real, intent(in) :: xx(:)
    real, allocatable :: yy(:)
    allocate(yy(size(xx)))
    yy = xx
end function copy_vec
end module array_constructor_06_mod

program array_constructor_06
    use array_constructor_06_mod
    implicit none
    real :: xx(3) = [10.0, 20.0, 30.0]
    real, allocatable :: yy(:)
    yy = copy0_vec(xx)

    if (any(abs(yy - xx) > 1e-6)) error stop "array_constructor_06 failed"
    print *, "test passed"
end program array_constructor_06