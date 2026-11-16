program maxloc_runtime_mask_01
    ! maxloc/minloc of a PARAMETER array with a mask known only at run
    ! time must not be folded as if the mask were all true; a run-time
    ! scalar mask or back= leaves the call to run time as well
    implicit none
    integer, parameter :: c(4) = [1, 9, 3, 2]
    real(8), parameter :: x(5) = [4.0d0, -7.0d0, 2.0d0, 8.0d0, -1.0d0]
    integer, parameter :: d(4) = [1, 9, 3, 9]
    logical :: l(4), lx(5), flag, b
    integer :: r(1), s

    l = [.true., .false., .true., .true.]
    lx = [.true., .false., .true., .false., .true.]

    r = maxloc(c, mask=l)
    if (r(1) /= 3) error stop 1
    r = minloc(c, mask=l)
    if (r(1) /= 1) error stop 2
    s = maxloc(c, dim=1, mask=l)
    if (s /= 3) error stop 3
    s = minloc(c, dim=1, mask=l)
    if (s /= 1) error stop 4

    r = maxloc(x, mask=lx)
    if (r(1) /= 1) error stop 5
    r = minloc(x, mask=lx)
    if (r(1) /= 5) error stop 6
    s = maxloc(x, dim=1, mask=lx)
    if (s /= 1) error stop 7
    s = minloc(x, dim=1, mask=lx)
    if (s /= 5) error stop 8

    l = .false.
    r = maxloc(c, mask=l)
    if (r(1) /= 0) error stop 9
    s = minloc(c, dim=1, mask=l)
    if (s /= 0) error stop 10

    flag = .false.
    r = maxloc(c, mask=flag)
    if (r(1) /= 0) error stop 11
    s = minloc(c, dim=1, mask=flag)
    if (s /= 0) error stop 12
    flag = .true.
    r = maxloc(c, mask=flag)
    if (r(1) /= 2) error stop 13
    s = minloc(x, dim=1, mask=flag)
    if (s /= 2) error stop 14

    b = .true.
    r = maxloc(d, back=b)
    if (r(1) /= 4) error stop 15
    s = maxloc(d, dim=1, back=b)
    if (s /= 4) error stop 16
    b = .false.
    r = maxloc(d, back=b)
    if (r(1) /= 2) error stop 17
    s = minloc(d, dim=1, back=b)
    if (s /= 1) error stop 18

    print *, "ok"
end program
