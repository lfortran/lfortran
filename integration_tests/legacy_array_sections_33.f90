! An element of an associate name with a contiguous selector, passed to an
! explicit-shape dummy, is associated with the elements that start at it
program legacy_array_sections_33
    implicit none
    type :: t
        integer, allocatable :: arr(:)
    end type
    type(t) :: x
    integer :: e(6)
    e = 0
    associate (a => e)
        call s(a(3))
    end associate
    if (any(e /= [0, 0, 1, 2, 0, 0])) error stop 1
    allocate(x%arr(6))
    x%arr = 0
    associate (a => x%arr)
        call s(a(5))
    end associate
    if (any(x%arr /= [0, 0, 0, 0, 1, 2])) error stop 2
    e = 0
    associate (a => e)
        associate (b => a)
            call s(b(1))
        end associate
    end associate
    if (any(e /= [1, 2, 0, 0, 0, 0])) error stop 3
contains
    subroutine s(v)
        integer, intent(inout) :: v(2)
        v = [1, 2]
    end subroutine
end program
