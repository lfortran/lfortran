module gpu_block_shared_08_m
implicit none
integer, parameter :: n = 128

contains

! Short phases of an iteration that runs on the lanes of a block
! (gpu_vector_lanes.cpp, plan_parts): rows that run on narrow groups of
! lanes of a warp, which combine their sums with shuffles over the group
! (30 rows on 8 lanes each, the lanes past the last group taking the last
! row again, and 16 rows on 16), and phases that several lanes of a row
! must not run alike: one that adds to an element of a block array after
! its shared-out sum, one that adds to the elements of a row inside the
! loop it shares out, while lanes past the last group repeat the row, and
! a loop around a shared-out sum whose number of iterations differs from
! row to row.
subroutine phases(st, kv, o1, o2, o3, o4, o5)
    real, intent(in) :: st(:,:,:), kv(:,:)
    real, intent(out) :: o1(:,:), o2(:,:,:), o3(:,:), o4(:,:), o5(:,:)
    integer :: h
    do concurrent (h = 1:size(st, 3))
        block
            real :: k(n), acc(n), w(30, n), r(64)
            real :: sk
            integer :: i, j, q
            do concurrent (j = 1:n)
                k(j) = kv(j, h)
                acc(j) = 1
                do i = 1, 30
                    w(i, j) = i - j
                end do
            end do
            do concurrent (i = 1:30)
                sk = 0
                do j = 1, n
                    sk = sk + st(i, j, h)*k(j)
                end do
                o4(i, h) = sk
            end do
            do concurrent (i = 1:16)
                sk = 0
                do j = 1, n
                    sk = sk + st(i + 16, j, h)*k(j)
                end do
                o5(i, h) = sk
            end do
            do concurrent (i = 1:n)
                sk = 0
                do j = 1, n
                    sk = sk + st(i, j, h)*k(j)
                end do
                acc(i) = acc(i) + sk
            end do
            do concurrent (i = 1:n)
                o1(i, h) = acc(i)
            end do
            do concurrent (i = 1:30)
                sk = 0
                do j = 1, n
                    sk = sk + k(j)*j
                    w(i, j) = w(i, j) + k(j)
                end do
            end do
            do concurrent (j = 1:n)
                do i = 1, 30
                    o2(i, j, h) = w(i, j)
                end do
            end do
            do concurrent (i = 1:64)
                do q = 1, i/16 + 1
                    sk = 0
                    do j = 1, n
                        sk = sk + k(j)*q
                    end do
                    r(i) = sk
                end do
            end do
            do concurrent (i = 1:64)
                o3(i, h) = r(65 - i)
            end do
        end block
    end do
end subroutine

end module

program gpu_block_shared_08
use gpu_block_shared_08_m
implicit none
integer, parameter :: nh = 3
real :: st(n, n, nh), kv(n, nh), o1(n, nh), o2(30, n, nh), o3(64, nh), &
    o4(30, nh), o5(16, nh), sk
integer :: h, i, j

do h = 1, nh
    do j = 1, n
        kv(j, h) = mod(j + 2*h, 5) - 2
        do i = 1, n
            st(i, j, h) = mod(i + 3*j + h, 7) - 3
        end do
    end do
end do
call phases(st, kv, o1, o2, o3, o4, o5)
do h = 1, nh
    do i = 1, n
        sk = sum(st(i, :, h)*kv(:, h))
        if (o1(i, h) /= 1 + sk) then
            print *, "o1", i, h, o1(i, h), 1 + sk
            error stop 1
        end if
        if (i <= 30) then
            if (o4(i, h) /= sk) then
                print *, "o4", i, h, o4(i, h), sk
                error stop 2
            end if
        end if
    end do
    do i = 1, 16
        if (o5(i, h) /= sum(st(i + 16, :, h)*kv(:, h))) then
            print *, "o5", i, h, o5(i, h)
            error stop 3
        end if
    end do
    do j = 1, n
        do i = 1, 30
            if (o2(i, j, h) /= i - j + kv(j, h)) then
                print *, "o2", i, j, h, o2(i, j, h), i - j + kv(j, h)
                error stop 4
            end if
        end do
    end do
    do i = 1, 64
        if (o3(i, h) /= sum(kv(:, h))*((65 - i)/16 + 1)) then
            print *, "o3", i, h, o3(i, h)
            error stop 5
        end if
    end do
end do
print *, "ok"
end program
