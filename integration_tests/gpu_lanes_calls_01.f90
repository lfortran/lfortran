program gpu_lanes_calls_01
! A `do concurrent` of few iterations whose inner loops call intrinsic
! functions that the compiler implements as functions of its own (`modulo`,
! `ibits`), which are not declared pure but have no effect besides their
! value: the lanes of an iteration share such loops out, an independent
! loop and an accumulation, as they do loops calling pure functions
! (gpu_vector_lanes.cpp). Decoding the bytes of packed words this way is
! how a quantized matrix-vector product reads its weights. An integer(8)
! offset computed with `int(..., int64)` is such a call too: every lane
! computes it, where the first lane alone could not hand on 8 bytes and the
! kernel ran one thread per iteration.
use, intrinsic :: iso_fortran_env, only: int32, int64
implicit none
integer, parameter :: k = 512, n = 40
integer(int32) :: w(k/4, n)
integer :: codes(k, n), total(n), i, j, d, ref
integer(int64) :: offset
real :: q(k, n), o(k, n)
integer :: r
do j = 1, n
    do i = 1, k/4
        w(i, j) = int(z'01020304', int32) + 17*modulo(i + 3*j, 5)
    end do
end do
do concurrent (r = 1:n)
    do d = 1, k
        codes(d, r) = ibits(w((d - 1)/4 + 1, r), 8*modulo(d - 1, 4), 8)
    end do
end do
do concurrent (r = 1:n) local(d)
    block
        integer :: s
        s = 0
        do d = 1, k
            s = s + ibits(w((d - 1)/4 + 1, r), 8*modulo(d - 1, 4), 8)
        end do
        total(r) = s
    end block
end do
offset = 3
do j = 1, n
    do i = 1, k
        q(i, j) = real(mod(i*j, 13))
    end do
end do
do concurrent (r = 1:n)
    block
        integer(int64) :: position
        position = offset + int(r - 1, int64)
        do d = 1, k
            o(d, r) = 2*q(d, r) + position
        end do
    end block
end do
do j = 1, n
    do d = 1, k
        if (o(d, j) /= 2*q(d, j) + (offset + j - 1)) error stop "offset"
    end do
    ref = 0
    do d = 1, k
        i = ibits(w((d - 1)/4 + 1, j), 8*modulo(d - 1, 4), 8)
        if (codes(d, j) /= i) error stop "codes"
        ref = ref + i
    end do
    if (total(j) /= ref) error stop "total"
end do
print *, "ok"
end program
