program intrinsic_reduction_dim_01
    implicit none
    logical :: mask(2, 3), cube(2, 2, 3), empty_mask(2, 0)
    real :: values(2, 3), empty_values(2, 0)
    integer :: dim, counts(2)
    logical :: parities(2)
    real :: norms(2)

    mask = reshape([.true., .false., .true., .true., .false., .true.], [2, 3])
    values(1, :) = [3.0, 4.0, 0.0]
    values(2, :) = [0.0, 0.0, 2.0]
    dim = 2
    counts = count(mask, dim)
    if (any(counts /= [2, 2])) error stop "runtime-dim count"
    parities = parity(mask, dim=2)
    if (any(parities)) error stop "last-dimension parity"
    norms = norm2(values, dim=2)
    if (any(abs(norms - [5.0, 2.0]) > 1e-6)) error stop "last-dimension norm2"

    cube = .true.
    if (any(count(cube, dim=3) /= 3)) error stop "rank-three count"
    if (.not. all(parity(cube, dim=3))) error stop "rank-three parity"

    empty_mask = .false.
    empty_values = 0.0
    if (any(count(empty_mask, dim=2) /= 0)) error stop "empty count"
    if (any(parity(empty_mask, dim=2))) error stop "empty parity"
    if (any(norm2(empty_values, dim=2) /= 0.0)) error stop "empty norm2"
end program
