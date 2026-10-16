program legacy_array_sections_29
    ! An element of a contiguous pointer array passed to an array dummy is
    ! sequence associated with the elements that follow it (#15332). This
    ! is an extension: GFortran rejects an element of a pointer array as an
    ! actual argument for an array dummy.
    implicit none
    integer, pointer :: p(:,:)
    integer :: i
    allocate(p(2,3))
    p = reshape([(i, i = 1, 6)], [2, 3])
    call negate(p(2,1))
    if (any(reshape(p, [6]) /= [1, -2, -3, -4, -5, 6])) error stop 2
    deallocate(p)
    print *, "ok"
contains
    subroutine negate(x)
        integer, intent(inout) :: x(2,2)
        if (any(reshape(x, [4]) /= [2, 3, 4, 5])) error stop 1
        x = -x
    end subroutine
end program
