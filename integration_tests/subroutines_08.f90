program main
    integer :: x = 10
    integer :: y(1) = [20]

    print *, x
    print *, y(1)

    call test1(1, x)
    print *, x
    if (x /= 1) error stop

    call test2(2, x)
    print *, x
    if (x /= 2) error stop

    call test3(3, x)
    print *, x
    if (x /= 3) error stop

    ! call test1(2,y(1)) ! Not yet supported
    ! print *, y(1)
    ! if (y(1) /= 2) error stop
end program main

subroutine test1(i,a)
    integer, intent(in) :: i
    integer :: a
    a = i
end subroutine test1

subroutine test2(i,a)
    integer, intent(in) :: i
    integer, intent(inout) :: a
    a = i
end subroutine test2

subroutine test3(i,a)
    integer, intent(in) :: i
    integer, intent(out) :: a
    a = i
end subroutine test3
