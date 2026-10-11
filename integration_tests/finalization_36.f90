! A pointer array associated with an array section inside a procedure stays
! valid after the procedure returns: a component of a dummy argument, a module
! pointer and a pointer dummy argument. Reassociating a pointer with a section
! leaves the array it was associated with before unchanged.
module finalization_36_mod
    implicit none
    type :: h
        integer, pointer :: p(:) => null()
    end type
    integer, target :: tw(6)
    integer, pointer :: gp(:) => null()
contains
    subroutine set_component(q)
        type(h), intent(inout) :: q
        q%p => tw(2:6:2)
    end subroutine

    subroutine set_module_pointer()
        gp => tw(5:1:-2)
    end subroutine

    subroutine set_dummy(p)
        integer, pointer, intent(inout) :: p(:)
        p => tw(1:3)
    end subroutine

    subroutine clobber_stack()
        integer :: junk(256)
        junk = -7
        if (any(junk /= -7)) error stop 1
    end subroutine

    subroutine reassociate_from_other()
        integer, allocatable, target :: a(:)
        integer, target :: b(4)
        integer, pointer :: p(:), r(:)
        b = [10, 20, 30, 40]
        allocate(a(3))
        a = [1, 2, 3]
        p => a
        p => b(2:3)
        if (size(a) /= 3 .or. any(a /= [1, 2, 3])) error stop 2
        if (any(p /= [20, 30])) error stop 3
        r => b
        p => r
        p => b(1:4:3)
        if (size(r) /= 4 .or. any(r /= b)) error stop 4
        if (any(p /= [10, 40])) error stop 5
    end subroutine

    subroutine copy_struct()
        type(h) :: q, q2
        q%p => tw(1:5:2)
        q2 = q
        q%p => tw(1:2)
        if (size(q2%p) /= 3 .or. any(q2%p /= [1, 3, 5])) error stop 6
        if (size(q%p) /= 2 .or. any(q%p /= [1, 2])) error stop 7
    end subroutine
end module

program finalization_36
    use finalization_36_mod
    implicit none
    type(h) :: q
    integer, pointer :: p(:)
    tw = [1, 2, 3, 4, 5, 6]
    call set_component(q)
    call set_module_pointer()
    call set_dummy(p)
    call clobber_stack()
    if (size(q%p) /= 3 .or. any(q%p /= [2, 4, 6])) error stop 10
    if (size(gp) /= 3 .or. any(gp /= [5, 3, 1])) error stop 11
    if (size(p) /= 3 .or. any(p /= [1, 2, 3])) error stop 12
    call reassociate_from_other()
    call copy_struct()
    print *, "done"
end program
