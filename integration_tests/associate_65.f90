! An associate name spelled like a derived type that is not accessible in
! the scope (only reached as a component type), in a procedure that is
! copied for its assumed-shape dummy argument.
module associate_65_m1
    implicit none
    type :: t1
        integer :: i = 42
    end type t1
    type :: t2
        type(t1) :: c
    end type t2
end module associate_65_m1

module associate_65_m2
    use associate_65_m1, only: t2
    implicit none
contains
    subroutine s_block(x, f, r)
        type(t2), intent(in) :: x
        real, intent(in) :: f(:)
        integer, intent(out) :: r
        block
            associate (t1 => x%c)
                r = t1%i + size(f)
            end associate
        end block
    end subroutine s_block

    subroutine s_select_type(x, f, r)
        class(t2), intent(in) :: x
        real, intent(in) :: f(:)
        integer, intent(out) :: r
        r = 0
        select type (x)
        class is (t2)
            associate (t1 => x%c)
                r = t1%i + size(f)
            end associate
        end select
    end subroutine s_select_type

    subroutine s_host_variable(x, f, r)
        type(t2), intent(in) :: x
        real, intent(in) :: f(:)
        integer, intent(out) :: r
        integer :: t1
        t1 = 7
        block
            associate (t1 => x%c)
                r = t1%i + size(f)
            end associate
        end block
        r = r + t1
    end subroutine s_host_variable
end module associate_65_m2

program associate_65
    use associate_65_m1, only: t2
    use associate_65_m2, only: s_block, s_select_type, s_host_variable
    implicit none
    type(t2) :: x
    integer :: r
    call s_block(x, [1.0, 2.0], r)
    print *, r
    if (r /= 44) error stop 1
    call s_select_type(x, [1.0, 2.0, 3.0], r)
    print *, r
    if (r /= 45) error stop 2
    call s_host_variable(x, [1.0], r)
    print *, r
    if (r /= 50) error stop 3
end program associate_65
