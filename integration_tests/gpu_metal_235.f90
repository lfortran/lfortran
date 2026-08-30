module gpu_metal_235_mod
implicit none

contains

    ! Leading extent of the written local array is `size()` of an
    ! assumed-shape dummy.  The kernel used to linearize `z` with a
    ! dim-1 extent of 0, so only the j == 1 column was written.
    subroutine fill_size_lit(x, y, out)
        real, intent(in) :: x(:), y(:)
        real, intent(out) :: out(:,:)
        real :: z(size(x),3)
        integer :: j
        do concurrent (j = 1:3)
            z(:,j) = x + y(j)
        end do
        out = z
    end subroutine

    ! Both extents are `size()` calls.
    subroutine fill_size_size(x, y, out)
        real, intent(in) :: x(:), y(:)
        real, intent(out) :: out(:,:)
        real :: z(size(x),size(y))
        integer :: j
        do concurrent (j = 1:size(y))
            z(:,j) = x + y(j)
        end do
        out = z
    end subroutine

    ! Same leading `size()` extent, but the dummies are explicit shape.
    subroutine fill_explicit(x, y, n, m, out)
        integer, intent(in) :: n, m
        real, intent(in) :: x(n), y(m)
        real, intent(out) :: out(n,m)
        real :: z(size(x),3)
        integer :: j
        do concurrent (j = 1:m)
            z(:,j) = x + y(j)
        end do
        out = z
    end subroutine

    ! Rank 3, every extent a `size()` call -- the shape real codes use.
    subroutine fill_rank3(x, y, w, out)
        real, intent(in) :: x(:), y(:), w(:)
        real, intent(out) :: out(:,:,:)
        real :: s(size(x),size(y),size(w))
        integer :: j, k
        do concurrent (k = 1:size(w))
            do j = 1, size(y)
                s(:,j,k) = x + y(j) + w(k)
            end do
        end do
        out = s
    end subroutine

    ! Fence: `size()` only in a non-leading dimension.
    subroutine fill_trailing(x, y, out)
        real, intent(in) :: x(:), y(:)
        real, intent(out) :: out(:,:)
        real :: z(4,size(y))
        integer :: j
        do concurrent (j = 1:size(y))
            z(:,j) = x + y(j)
        end do
        out = z
    end subroutine

    ! Fence: leading extent is a plain integer dummy.
    subroutine fill_dummy(x, y, n, out)
        integer, intent(in) :: n
        real, intent(in) :: x(:), y(:)
        real, intent(out) :: out(:,:)
        real :: z(n,3)
        integer :: j
        do concurrent (j = 1:3)
            z(:,j) = x + y(j)
        end do
        out = z
    end subroutine

    ! Fence: all extents are literals.
    subroutine fill_literal(x, y, out)
        real, intent(in) :: x(:), y(:)
        real, intent(out) :: out(:,:)
        real :: z(4,3)
        integer :: j
        do concurrent (j = 1:3)
            z(:,j) = x + y(j)
        end do
        out = z
    end subroutine

end module

program gpu_metal_235
use gpu_metal_235_mod
implicit none
real :: xg(4), yg(3), wg(2)
real :: r(4,3), r3(4,3,2)
integer :: i, j, k

xg = [1.0, 2.0, 3.0, 4.0]
yg = [10.0, 20.0, 30.0]
wg = [100.0, 200.0]

r = -1.0
call fill_size_lit(xg, yg, r)
call check2(r, "fill_size_lit")

r = -1.0
call fill_size_size(xg, yg, r)
call check2(r, "fill_size_size")

r = -1.0
call fill_explicit(xg, yg, 4, 3, r)
call check2(r, "fill_explicit")

r = -1.0
call fill_trailing(xg, yg, r)
call check2(r, "fill_trailing")

r = -1.0
call fill_dummy(xg, yg, 4, r)
call check2(r, "fill_dummy")

r = -1.0
call fill_literal(xg, yg, r)
call check2(r, "fill_literal")

r3 = -1.0
call fill_rank3(xg, yg, wg, r3)
do k = 1, 2
    do j = 1, 3
        do i = 1, 4
            print *, r3(i,j,k)
            if (abs(r3(i,j,k) - (xg(i) + yg(j) + wg(k))) > 1.0e-5) then
                error stop "fill_rank3"
            end if
        end do
    end do
end do

print *, "ok"

contains

    subroutine check2(a, tag)
        real, intent(in) :: a(4,3)
        character(*), intent(in) :: tag
        integer :: ii, jj
        do jj = 1, 3
            do ii = 1, 4
                print *, a(ii,jj)
                if (abs(a(ii,jj) - (xg(ii) + yg(jj))) > 1.0e-5) then
                    print *, "failed: ", tag
                    error stop
                end if
            end do
        end do
    end subroutine

end program
