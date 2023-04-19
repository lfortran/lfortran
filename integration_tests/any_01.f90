program any_01
    logical :: c1(2), c2(3)
    integer :: a(2, 3), b(2, 3)

    ! TODO: Deal with passing array constants to intrinsics
    ! logical :: l
    ! l = any((/.true., .true., .true./))
    ! print *, l

    a = 1
    b = 1
    b(2, 2) = 2
    call section(a, b, 1, c1)
    print *, c1
    call section(a, b, 2, c2)
    print *, c2

contains

    subroutine section(a, b, axis, c)
        integer, intent(in) :: a(2, 3), b(2, 3), axis
        logical, intent(out) :: c(:)
        c = any(a == b, axis)
    end subroutine section

end program any_01
