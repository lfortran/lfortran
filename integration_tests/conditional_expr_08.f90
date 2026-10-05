module conditional_expr_08_mod
    implicit none
    integer :: calls = 0
contains
    logical function counted(c)
        logical, intent(in) :: c
        calls = calls + 1
        counted = c
    end function

    real function row_sum(x)
        real, intent(in) :: x(:)
        row_sum = sum(x)
    end function
end module

! Fortran 2023 conditional expressions whose arms need temporaries, such as
! an array section passed to a procedure or an array expression. Only the arm
! that is chosen is evaluated (10.1.4 NOTE 3), so a section that does not
! exist, or an array that is not allocated, in the arm that is not chosen is
! never read.
!
! This test is not labelled `gfortran`, for the reason given next to
! conditional_expr_01 in CMakeLists.txt.
program conditional_expr_08
    use conditional_expr_08_mod
    implicit none
    real :: a(3,5), w(3), r
    real, allocatable :: x(:)
    integer :: i, k

    a = 1
    w = 0
    ! a(3,1:6) does not exist, and that arm is not taken for i = 3.
    do i = 1, 3
        w(i) = (i <= 2 ? sum(a(i,1:2*i)) : 0.0)
    end do
    if (any(w /= [2.0, 4.0, 0.0])) error stop 1

    do i = 1, 3
        w(i) = (i > 2 ? -1.0 : row_sum(a(i,1:2*i)) + 1)
    end do
    if (any(w /= [3.0, 5.0, -1.0])) error stop 2

    ! x is not allocated, so neither arm reading it may be evaluated.
    r = (allocated(x) ? sum(x(2:3)) : -1.0)
    if (r /= -1.0) error stop 3
    r = (allocated(x) ? sum(x + 1) : -2.0)
    if (r /= -2.0) error stop 4
    r = (allocated(x) ? row_sum(x(2:3)) : (size(a, 2) > 4 ? 5.0 : sum(a(1,2:6))))
    if (r /= 5.0) error stop 5

    allocate(x(3))
    x = [1.0, 2.0, 3.0]
    r = (allocated(x) ? sum(x(2:3)) : -1.0)
    if (r /= 5.0) error stop 6
    r = (allocated(x) ? sum(x + 1) : -2.0)
    if (r /= 9.0) error stop 7

    ! The condition is evaluated once.
    calls = 0
    r = (counted(.true.) ? sum(x(1:2)) : sum(x(2:3)))
    if (r /= 3.0 .or. calls /= 1) error stop 8

    ! a(1,1:6) does not exist, and that arm is not taken for k = 3.
    k = 0
    do while ((k < 3 ? row_sum(a(1,1:k+3)) : 10.0) < 6.0)
        k = k + 1
    end do
    if (k /= 3) error stop 9
    print *, w, r, k
end program
