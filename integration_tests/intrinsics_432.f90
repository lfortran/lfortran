program intrinsics_432
    implicit none
    integer :: a

    ! Test nested ieor with value parameters
    call test_ieor(a, 1, 2)
    if (a /= 1) error stop

    ! Test nested iand with value parameters
    call test_iand(a, 15, 6)
    if (a /= 6) error stop

    ! Test nested ior with value parameters
    call test_ior(a, 5, 2)
    if (a /= 7) error stop

    print *, "ok"

contains
    pure subroutine test_ieor(a, b, c)
        integer, intent(out) :: a
        integer, intent(in), value :: b, c
        a = ieor(ieor(b, c), c)
    end subroutine

    pure subroutine test_iand(a, b, c)
        integer, intent(out) :: a
        integer, intent(in), value :: b, c
        a = iand(iand(b, c), c)
    end subroutine

    pure subroutine test_ior(a, b, c)
        integer, intent(out) :: a
        integer, intent(in), value :: b, c
        a = ior(ior(b, c), c)
    end subroutine
end program
