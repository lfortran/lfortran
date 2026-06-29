program unpack_scalar_field_01
    implicit none

    integer :: a(5)
    logical :: mask(5)
    real :: b(4)
    logical :: rmask(4)
    logical :: c(3)
    logical :: lmask(3)
    integer :: d(2,2)
    logical :: mask2d(2,2)

    ! Integer scalar field
    mask = [.true., .false., .true., .false., .true.]
    a = unpack([100, 200, 300], mask, 0)
    if (any(a /= [100, 0, 200, 0, 300])) error stop 1

    ! Real scalar field
    rmask = [.false., .true., .false., .true.]
    b = unpack([1.5, 2.5], rmask, -1.0)
    if (any(abs(b - [-1.0, 1.5, -1.0, 2.5]) > 0.001)) error stop 2

    ! Logical scalar field
    lmask = [.true., .true., .false.]
    c = unpack([.true., .false.], lmask, .true.)
    if (c(1) .neqv. .true.) error stop 3
    if (c(2) .neqv. .false.) error stop 4
    if (c(3) .neqv. .true.) error stop 5

    ! 2D mask with scalar field
    mask2d = reshape([.true., .false., .false., .true.], [2,2])
    d = unpack([10, 20], mask2d, 99)
    if (d(1,1) /= 10 .or. d(2,1) /= 99) error stop 6
    if (d(1,2) /= 99 .or. d(2,2) /= 20) error stop 7

    print *, "PASS"

end program
