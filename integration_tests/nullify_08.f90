! nullify() of a pointer component of a derived type (#4673)
module nullify_08_mod
    implicit none

    type :: node
        integer :: value
        type(node), pointer :: next => null()
    end type node

    type(node), pointer :: head => null()

contains

    subroutine nullify_list(count)
        integer, intent(out) :: count
        type(node), pointer :: current, next_node

        count = 0
        current => head
        do while (associated(current))
            next_node => current%next
            nullify(current%next)
            if (associated(current%next)) error stop
            deallocate(current)
            count = count + 1
            current => next_node
        end do
        nullify(head)
    end subroutine nullify_list

end module nullify_08_mod

program nullify_08
    use nullify_08_mod
    implicit none
    type(node), pointer :: p
    integer :: i, n

    call nullify_list(n)
    if (n /= 0) error stop

    do i = 1, 3
        allocate(p)
        p%value = i
        p%next => head
        head => p
    end do
    if (head%value /= 3) error stop
    if (.not. associated(head%next)) error stop

    call nullify_list(n)
    print *, n
    if (n /= 3) error stop
    if (associated(head)) error stop
end program nullify_08
