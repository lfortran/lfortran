module doloop_09_mod

contains

subroutine f()
integer :: i, j
j = 0
do i = 1, 10
    j = j + i
end do
if (j /= 55) error stop
print *, j

call g()

contains

    subroutine g()
    integer :: i, j
    j = 0
    do i = 1, 10
        j = j + i
    end do
    if (j /= 55) error stop
    print *, j
    end subroutine

end subroutine

end module

program doloop_09
use doloop_09_mod, only: f

call f()

end program
