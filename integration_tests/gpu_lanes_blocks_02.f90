module gpu_lanes_blocks_02_m
! A loop inside a `do concurrent` of few iterations whose body is a block
! with locals of its own: the lanes of a SIMD-group share it out like any
! other loop whose iterations are independent (gpu_vector_lanes.cpp), as
! each run of the block has its own locals. One whose block leaves the loop
! (`exit`) is not shared out. Every result is compared exactly.
implicit none
contains
    pure subroutine scale(q, out, first)
        real, contiguous, intent(in) :: q(:, :, :)
        real, contiguous, intent(out) :: out(:, :, :)
        integer, contiguous, intent(out) :: first(:, :)
        do concurrent (integer :: token = 1:size(q, 3), h = 1:size(q, 2))
            block
                integer :: d, found
                do d = 1, size(q, 1)
                    block
                        real :: acc, twice
                        acc = 2*q(d, h, token)
                        twice = acc + q(d, h, token)
                        out(d, h, token) = acc*twice
                    end block
                end do
                found = 0
                do d = 1, size(q, 1)
                    block
                        if (q(d, h, token) > 7) then
                            found = d
                            exit
                        end if
                    end block
                end do
                first(h, token) = found
            end block
        end do
    end subroutine
end module

program gpu_lanes_blocks_02
use gpu_lanes_blocks_02_m
implicit none
integer, parameter :: nd = 256, nh = 24, nt = 2
real :: q(nd, nh, nt), out(nd, nh, nt)
integer :: first(nh, nt), d, h, t, ref
do t = 1, nt
    do h = 1, nh
        do d = 1, nd
            q(d, h, t) = real(mod(d + 5*h + 3*t, 9))
        end do
    end do
end do
call scale(q, out, first)
do t = 1, nt
    do h = 1, nh
        ref = 0
        do d = 1, nd
            if (out(d, h, t) /= 2*q(d, h, t)*3*q(d, h, t)) error stop "out"
            if (ref == 0 .and. q(d, h, t) > 7) ref = d
        end do
        if (first(h, t) /= ref) error stop "first"
    end do
end do
print *, "ok"
end program
