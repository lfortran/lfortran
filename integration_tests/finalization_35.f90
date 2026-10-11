! A pointer array component associated with an array section must not be
! freed when the enclosing derived-type variable goes out of scope.
module finalization_35_mod
    implicit none
    type :: t
        integer :: y = 0
    end type
    type :: h
        type(t), pointer :: p(:) => null()
    end type
    type :: hi
        integer, pointer :: p(:) => null()
    end type
contains
    subroutine contiguous_section()
        type(h) :: q
        type(t), target :: tw(5)
        q%p => tw(1:3)
        q%p%y = 3
        if (size(q%p) /= 3) error stop 1
        if (any(tw%y /= [3, 3, 3, 0, 0])) error stop 2
    end subroutine

    subroutine strided_section()
        type(h) :: q
        type(t), target :: tw(5)
        q%p => tw(1:5:2)
        q%p%y = 4
        if (size(q%p) /= 3) error stop 3
        if (any(tw%y /= [4, 0, 4, 0, 4])) error stop 4
    end subroutine

    subroutine reversed_section()
        type(h) :: q
        type(t), target :: tw(5)
        q%p => tw(5:1:-2)
        q%p(1)%y = 5
        if (size(q%p) /= 3) error stop 5
        if (any(tw%y /= [0, 0, 0, 0, 5])) error stop 6
    end subroutine

    subroutine integer_section()
        type(hi) :: q
        integer, target :: tw(5)
        tw = 0
        q%p => tw(2:4)
        q%p = 7
        if (size(q%p) /= 3) error stop 7
        if (any(tw /= [0, 7, 7, 7, 0])) error stop 8
    end subroutine

    subroutine association_only()
        type(hi) :: q
        integer, target :: tw(5)
        q%p => tw(2:4)
    end subroutine

    subroutine reassociated()
        type(hi) :: q
        integer, target :: tw(6)
        integer :: i
        tw = [1, 2, 3, 4, 5, 6]
        do i = 1, 3
            q%p => tw(i:6:i)
            if (q%p(1) /= i) error stop 9
        end do
        q%p => tw
        if (size(q%p) /= 6) error stop 10
        q%p => tw(6:1:-1)
        if (q%p(1) /= 6 .or. q%p(6) /= 1) error stop 11
    end subroutine
end module

program finalization_35
    use finalization_35_mod
    implicit none
    call contiguous_section()
    call strided_section()
    call reversed_section()
    call integer_section()
    call association_only()
    call reassociated()
    print *, "done"
end program
