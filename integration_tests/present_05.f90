! MRE: External procedure called multiple times in same subroutine
! This tests that interface creation for external procedures does not
! create duplicate symbols when the procedure is called multiple times.
subroutine test_select(select_fn, n, wr, wi, result)
    implicit none
    logical, external :: select_fn
    integer, intent(in) :: n
    real, intent(in) :: wr(*), wi(*)
    real, intent(out) :: result(*)
    integer :: i
    logical :: flag

    ! First call to select_fn
    do i = 1, n
        result(i) = 0.0
        if (select_fn(wr(i), wi(i))) then
            result(i) = 1.0
        end if
    end do

    ! Second call to select_fn (same scope - must not create duplicate interface)
    do i = 1, n
        flag = select_fn(wr(i), wi(i))
        if (flag) result(i) = result(i) + 1.0
    end do
end subroutine

logical function my_select(x, y)
    implicit none
    real, intent(in) :: x, y
    my_select = x > 0.0
end function

program test_present_05
    implicit none
    real :: wr(3), wi(3), r(3)
    logical, external :: my_select

    wr = [1.0, -1.0, 2.0]
    wi = [0.0, 0.0, 0.0]

    call test_select(my_select, 3, wr, wi, r)

    ! Expected: r(1)=2.0 (selected twice), r(2)=0.0 (never selected), r(3)=2.0 (selected twice)
    if (abs(r(1) - 2.0) > 1e-6) error stop
    if (abs(r(2) - 0.0) > 1e-6) error stop
    if (abs(r(3) - 2.0) > 1e-6) error stop

    print *, "PASS"
end program
