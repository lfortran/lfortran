! A defined assignment whose dummies are class(*) must not replace an
! intrinsic assignment between conforming intrinsic types (including the
! assignments inside the specific itself), but it still applies when
! intrinsic assignment does not.
module defined_assignment_04_m
    implicit none
    type :: box
        integer :: v
    end type
    integer :: ncalls = 0
    interface assignment(=)
        module procedure asg_int, asg_real, asg_arr
    end interface
contains
    subroutine asg_int(lhs, rhs)
        integer, intent(out) :: lhs
        class(*), intent(in) :: rhs
        ncalls = ncalls + 1
        lhs = -1
        select type (rhs)
        type is (box)
            lhs = rhs%v
        type is (logical)
            lhs = merge(1, 0, rhs)
        type is (character(*))
            lhs = len(rhs)
        end select
    end subroutine

    subroutine asg_real(lhs, rhs)
        real, intent(out) :: lhs
        class(*), intent(in) :: rhs
        ncalls = ncalls + 1
        lhs = 2.5
    end subroutine

    subroutine asg_arr(lhs, rhs)
        integer, intent(out) :: lhs(:)
        class(*), intent(in) :: rhs(:)
        ncalls = ncalls + 1
        lhs = -1
        select type (rhs)
        type is (box)
            lhs = 10 * size(lhs)
        end select
    end subroutine
end module

module defined_assignment_04_both
    implicit none
    type :: tag
        integer :: v
    end type
    integer :: nboth = 0
    interface assignment(=)
        module procedure asg_any
    end interface
contains
    subroutine asg_any(lhs, rhs)
        class(*), intent(inout) :: lhs
        class(*), intent(in) :: rhs
        nboth = nboth + 1
        select type (lhs)
        type is (integer)
            select type (rhs)
            type is (integer)
                lhs = rhs
            type is (tag)
                lhs = rhs%v
            end select
        end select
    end subroutine

    subroutine check_both()
        integer :: x
        x = 7
        if (x /= 7) error stop
        if (nboth /= 0) error stop
        x = tag(9)
        if (x /= 9) error stop
        if (nboth /= 1) error stop
    end subroutine
end module

program defined_assignment_04
    use defined_assignment_04_m
    use defined_assignment_04_both, only: check_both
    implicit none
    integer :: x, a(2)
    real :: y

    ! Intrinsic assignments: the defined assignment is never called.
    x = 5
    if (x /= 5) error stop
    x = 2.75
    if (x /= 2) error stop
    y = 1.5
    if (abs(y - 1.5) > 1e-6) error stop
    y = 3
    if (abs(y - 3.0) > 1e-6) error stop
    a = [1, 2]
    if (a(1) /= 1 .or. a(2) /= 2) error stop
    a = 7
    if (any(a /= 7)) error stop
    if (ncalls /= 0) error stop

    ! Intrinsic assignment does not apply: the defined assignment is called.
    x = box(42)
    if (x /= 42) error stop
    if (ncalls /= 1) error stop
    x = .true.
    if (x /= 1) error stop
    if (ncalls /= 2) error stop
    x = "abc"
    if (x /= 3) error stop
    if (ncalls /= 3) error stop
    a = [box(3), box(4)]
    if (any(a /= 20)) error stop
    if (ncalls /= 4) error stop

    call check_both()
    print *, "ok"
end program
