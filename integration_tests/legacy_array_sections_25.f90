module legacy_array_sections_25_mod
    implicit none
contains
    subroutine mod_sum(values, s)
        integer, intent(in) :: values(:)
        integer, intent(out) :: s
        s = values(1) * 100 + values(2) * 10 + values(3)
    end subroutine
end module

program legacy_array_sections_25
    use legacy_array_sections_25_mod, only: mod_sum
    implicit none
    integer :: a(3) = [1, 2, 3], indices(3) = [3, 1, 2]
    integer :: b(3, 2), s
    integer, allocatable :: c(:), c_indices(:)
    b(:, 1) = [4, 5, 6]
    b(:, 2) = [7, 8, 9]
    allocate(c(3), c_indices(3))
    c = [9, 7, 8]
    c_indices = [1, 2, 3]

    call consume(a(indices))
    call consume_explicit(a(indices), 3)
    call consume_assumed_size(a(indices))
    call consume(b(indices, 2))
    call consume(c(c_indices))
    call mod_sum(a(indices), s)
    if (s /= 312) error stop
    if (first(a(indices)) /= 3) error stop
    if (any(a /= [1, 2, 3])) error stop
    print *, "ok"
contains
    subroutine consume(values)
        integer, intent(in) :: values(:)
        print *, values
        if (size(values) /= 3) error stop
        if (values(1) == 3) then
            if (any(values /= [3, 1, 2])) error stop
        else
            if (any(values /= [9, 7, 8])) error stop
        end if
    end subroutine

    subroutine consume_explicit(values, n)
        integer, intent(in) :: n
        integer, intent(in) :: values(n)
        if (any(values /= [3, 1, 2])) error stop
    end subroutine

    subroutine consume_assumed_size(values)
        integer, intent(in) :: values(*)
        if (values(1) /= 3 .or. values(2) /= 1 .or. values(3) /= 2) error stop
    end subroutine

    integer function first(values)
        integer, intent(in) :: values(:)
        first = values(1)
    end function
end program
