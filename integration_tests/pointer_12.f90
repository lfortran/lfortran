program pointer_12
    implicit none
    real, target :: w(4)
    integer :: i
    w = [1.0, 2.0, 3.0, 4.0]
    call check(w)
    print *, "OK"
contains
    subroutine check(w)
        real, intent(in), target :: w(:)
        real, pointer :: p(:) => null()
        p(1:size(w)) => w
        if (size(p) /= 4) error stop "FAIL: size mismatch"
        if (abs(p(1) - 1.0) > 1e-6) error stop "FAIL: p(1)"
        if (abs(p(4) - 4.0) > 1e-6) error stop "FAIL: p(4)"
    end subroutine check
end program pointer_12
