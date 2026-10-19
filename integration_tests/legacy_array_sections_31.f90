program legacy_array_sections_31
    implicit none
    real(8) :: w(20)
    integer :: i
    w = [(real(i, 8), i = 1, 20)]
    call outer(w, 4)
end program

subroutine outer(work, n)
    implicit none
    integer, intent(in) :: n
    real(8), dimension(1) :: work
    real(8) :: s
    integer :: i
    s = 0
    do i = 1, 2*n
        s = s + work(i)
    end do
    print *, s
    if (abs(s - 36) > 1e-12) error stop
    call inner(work(5), n)
end subroutine

subroutine inner(phi, n)
    implicit none
    integer, intent(in) :: n
    real(8) :: phi(n, 2)
    print *, sum(phi)
    if (abs(sum(phi) - 68) > 1e-12) error stop
end subroutine
