program intrinsic_reduction_dim_02
    ! PARITY with a DIM known only at run time (#16305)
    implicit none
    logical :: mask(1, 1), result(1)
    logical :: m(2, 3), m3(2, 3, 4)
    logical :: r1(3), r2(2), r3(2, 3)
    integer :: dim

    mask = .true.
    dim = 2
    result = parity(mask, dim)
    if (.not. all(result)) error stop

    m = reshape([.true., .false., .true., .true., .false., .false.], [2, 3])
    dim = 1
    r1 = parity(m, dim)
    if (any(r1 .neqv. [.true., .false., .false.])) error stop
    dim = 2
    r2 = parity(m, dim)
    if (any(r2 .neqv. [.false., .true.])) error stop

    m3 = .false.
    m3(1, 2, :) = .true.
    m3(2, :, 3) = .true.
    dim = 3
    r3 = parity(m3, dim)
    if (any(r3(1, :) .neqv. [.false., .false., .false.])) error stop
    if (any(r3(2, :) .neqv. [.true., .true., .true.])) error stop

    call check(m, 1, [.true., .false., .false.])
    call check(m, 2, [.false., .true.])
    print *, r3

contains

    subroutine check(a, d, expected)
        logical, intent(in) :: a(:, :)
        integer, intent(in) :: d
        logical, intent(in) :: expected(:)
        logical :: p(size(expected))
        p = parity(a, d)
        if (any(p .neqv. expected)) error stop
    end subroutine

end program
