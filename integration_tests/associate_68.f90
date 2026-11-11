program associate_68
    ! A vector subscript keeps its dimension in an associate name.
    ! `delx(idx, :)` is rank 2 (F2018 9.5.3.1): one dimension from the
    ! vector subscript and one from the triplet.
    implicit none
    integer :: idx(2)
    real, allocatable :: delx(:, :)
    real :: row(2), updated(2), s
    idx = [1, 3]
    allocate(delx(4, 1))
    delx = 1.0
    delx(1, 1) = 2.0
    delx(3, 1) = 4.0
    row = [1.0, 2.0]
    associate (dx => delx(idx, :))
        s = dot_product(dx(:, 1), dx(:, 1))
        updated = row + matmul((3.0 - matmul(row, dx)), transpose(dx)) / s
    end associate
    if (s /= 20.0) error stop 1
    if (abs(updated(1) - 0.3) > 1.0e-5) error stop 2
    if (abs(updated(2) - 0.6) > 1.0e-5) error stop 3
end program
