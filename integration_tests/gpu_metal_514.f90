module gpu_metal_514_m
    ! A stencil with one-sided rows at the ends of both dimensions that
    ! reads its input at two rows along the outer dimension: index-set
    ! splitting makes a jammed and a plain interior of it, and the host
    ! picks one by the cache size (--gpu-cache-bytes in CMakeLists.txt):
    ! the jammed one for the small grid, the plain one for the large one.
    implicit none
contains
    subroutine smooth(a, b, n, m)
        real, intent(in) :: a(:,:)
        real, intent(out) :: b(:,:)
        integer, intent(in) :: n, m
        integer :: i, j
        do concurrent (j = 1:m, i = 1:n)
            if (j == 1) then
                b(i, j) = a(i, 2) - a(i, 1)
            else if (j == m) then
                b(i, j) = a(i, m) - a(i, m - 1)
            else if (i == 1 .or. i == n) then
                b(i, j) = a(i, j)
            else
                b(i, j) = 0.25 * (a(i - 1, j) + a(i + 1, j)) &
                    + 0.5 * (a(i, j + 1) - a(i, j - 1))
            end if
        end do
    end subroutine

    subroutine check(n, m)
        integer, intent(in) :: n, m
        real, allocatable :: a(:,:), b(:,:)
        real :: e
        integer :: i, j
        allocate(a(n, m), b(n, m))
        do j = 1, m
            do i = 1, n
                a(i, j) = real(mod(i * 7 + j * 13, 17))
            end do
        end do
        call smooth(a, b, n, m)
        do j = 1, m
            do i = 1, n
                if (j == 1) then
                    e = a(i, 2) - a(i, 1)
                else if (j == m) then
                    e = a(i, m) - a(i, m - 1)
                else if (i == 1 .or. i == n) then
                    e = a(i, j)
                else
                    e = 0.25 * (a(i - 1, j) + a(i + 1, j)) &
                        + 0.5 * (a(i, j + 1) - a(i, j - 1))
                end if
                if (abs(b(i, j) - e) > 1e-5) error stop 1
            end do
        end do
    end subroutine
end module

program gpu_metal_514
    use gpu_metal_514_m
    implicit none
    call check(13, 11)
    call check(67, 71)
    call check(4, 3)
    print *, "ok"
end program
