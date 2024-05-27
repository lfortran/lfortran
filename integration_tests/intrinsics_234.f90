program intrinsics_234

    implicit none
    logical :: mask(3, 4)
    logical :: mask_(6, 9)
    logical :: mask_3(3, 4, 3)
    logical :: mask_4(4, 5, 3, 1)

    logical :: res
    integer, dimension(2,3) :: a, b
    logical, dimension(2,3) :: mask2

    logical, parameter :: c1 = parity([.true., .false., .true., .false.])
    logical, parameter :: c2 = parity([.true., .false., .true., .false., .true., .false.])

    mask = reshape([ .true., .false., .true., .false., &
                    .true., .false., .true., .false., &
                    .true., .false., .true., .true.], [3, 4])

    mask_ = .false.
    mask_(1, 1) = .true.
    mask_(1, 2) = .true.
    mask_(5, 1) = .true.
    mask_(5, 2) = .true.
    mask_(6, 7) = .true.
    mask_(4, 8) = .true.

    mask_3 = .false.
    mask_3(1, 1, :) = .true.
    mask_3(1, 2, :) = .true.
    mask_3(3, 1, :) = .true.
    mask_3(2, 2, :) = .true.
    mask_3(2, 4, :) = .true.
    mask_3(3, 3, :) = .true.

    mask_4 = .true.
    mask_4(1, 1, 1, 1) = .false.
    mask_4(1, 1, 1, :) = .false.
    mask_4(1, 1, 1, :) = .false.
    mask_4(1, 2, 1, :) = .false.
    mask_4(1, 2, 3, :) = .false.


    print *, parity(mask)
    if (parity(mask) .neqv. .true.) error stop
    print *, parity(mask_)
    if (parity(mask_) .neqv. .false.) error stop
    print *, parity(mask_3)
    if (parity(mask_3) .neqv. .false.) error stop

    print *, parity(mask, 1)
    if (any(parity(mask, 1)) .neqv. .true.) error stop
    print *, parity(mask, 2)
    if (any(parity(mask, 2)) .neqv. .true.) error stop

    print *, any(parity(mask_, 1))
    if (any(parity(mask_, 1)) .neqv. .true.) error stop
    print *, shape(parity(mask_, 1))
    print *, any(parity(mask_, 2))
    if (any(parity(mask_, 2)) .neqv. .true.) error stop
    print *, shape(parity(mask_, 2))

    print *, any(parity(mask_3, 1))
    if (any(parity(mask_3, 1)) .neqv. .true.) error stop
    print *, shape(parity(mask_3, 1))
    print *, any(parity(mask_3, 2))
    if (any(parity(mask_3, 2)) .neqv. .false.) error stop
    print *, shape(parity(mask_3, 2))
    print *, any(parity(mask_3, 3))
    if (any(parity(mask_3, 3)) .neqv. .true.) error stop

    print *, any(parity(mask_4, 1))
    if (any(parity(mask_4, 1)) .neqv. .true.) error stop
    print *, shape(parity(mask_4, 1))

    res = parity(mask_4)
    if (res .neqv. .true.) error stop

    res = parity(mask_4)
    if (res .neqv. .true.) error stop

    a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])
    b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])
    print *, a(1,:)
    print *, a(2,:)
    print *
    print *, b(1,:)

    print *, b(2,:)
    print *
    mask2 = a.ne.b
    print '(3l3)', mask2(1,:)
    print '(3l3)', mask2(2,:)
    print *
    print *, parity(mask2)
    if (parity(mask2) .neqv. .true.) error stop
    print *
    print *, parity(mask2, 1)
    print *
    print *, parity(mask2, 2)

    print*, c1
    if (c1 .neqv. .false.) error stop

    print*, c2
    if (c2 .neqv. .true.) error stop

    print*, kind(c2)
    if (kind(c2) /= 4) error stop

end program
