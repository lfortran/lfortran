program all_01
    implicit none
    logical :: empty(0)
    logical :: non_empty(1)

    ! Issue 15109: ALL(empty .eqv. .true.) on a zero-size logical array
    if (.not. all(empty .eqv. .true.)) error stop 1
    if (.not. all(empty .and. .true.)) error stop 2
    if (.not. all(empty .or. .false.)) error stop 3
    if (.not. all(empty .neqv. .false.)) error stop 4
    if (.not. all(.not. empty)) error stop 5
    if (any(empty .eqv. .true.)) error stop 6

    ! Controls
    if (.not. all(empty)) error stop 7
    if (any(empty)) error stop 8

    non_empty = .true.
    if (.not. all(non_empty)) error stop 9
    if (.not. all(non_empty .eqv. .true.)) error stop 10
end program
