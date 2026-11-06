! An ASSOCIATE name spelled like a derived type that is not accessible in
! the scope (`t1` below only reaches m2 as the type of a component of `t2`),
! inside a BLOCK and a SELECT TYPE of a procedure with an assumed-shape dummy
module associate_64_m1
    implicit none
    type :: t1
        integer :: i = 42
    end type t1
    type :: t2
        type(t1) :: c
    end type t2
end module associate_64_m1

module associate_64_m2
    use associate_64_m1, only: t2
    implicit none
contains
    subroutine s_block(x, f, n)
        type(t2), intent(in) :: x
        real, intent(in) :: f(:)
        integer, intent(out) :: n
        block
            associate (t1 => x%c)
                if (t1%i /= 42) error stop
                n = t1%i + size(f)
            end associate
        end block
    end subroutine s_block

    subroutine s_select_type(x, f, n)
        class(t2), intent(in) :: x
        real, intent(in) :: f(:)
        integer, intent(out) :: n
        n = 0
        select type (x)
        class is (t2)
            associate (t1 => x%c)
                if (t1%i /= 42) error stop
                n = t1%i + size(f)
            end associate
        end select
    end subroutine s_select_type
end module associate_64_m2

program associate_64
    use associate_64_m1, only: t2
    use associate_64_m2, only: s_block, s_select_type
    implicit none
    type(t2) :: x
    integer :: n
    call s_block(x, [1.0, 2.0], n)
    if (n /= 44) error stop
    call s_select_type(x, [1.0, 2.0, 3.0], n)
    if (n /= 45) error stop
    print *, "ok"
end program associate_64
