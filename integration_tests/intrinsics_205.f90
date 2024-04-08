program compile_time
    implicit none
    INTEGER, parameter :: p1(4) = [1, 0, 0, 2]
    logical, parameter :: l1(4) = [.TRUE., .FALSE., .FALSE., .TRUE.]
    ! the assignment itself is a test that "pack"'s value
    ! below is assigned at compile-time, as it's a "parameter"
    integer, parameter :: p1_res(7) = pack(p1, l1, [ 5, 6, 3, 4, 0, 13, 29 ])
    ! INTEGER, parameter :: p2(3) = pack([1, 0, 0, 2], [.TRUE., .TRUE., .FALSE., .TRUE.]) 

    print *, p1_res
    if (p1_res(1) /= 1) error stop
    if (p1_res(2) /= 2) error stop
    if (p1_res(3) /= 3) error stop
    if (p1_res(4) /= 4) error stop
    if (p1_res(5) /= 0) error stop
    if (p1_res(6) /= 13) error stop
    if (p1_res(7) /= 29) error stop

    ! print *, p2
    ! if (p2(1) /= 1) error stop
    ! if (p2(2) /= 0) error stop
    ! if (p2(3) /= 2) error stop
end program
