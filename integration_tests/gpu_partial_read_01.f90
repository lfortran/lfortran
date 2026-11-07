! A launch writes part of an array and the next launch reads all of it, as
! appending to a cache and then attending over the cache does. The arrays
! are small, so the Metal runtime keeps what the first launch writes in a
! device buffer of its own: the second launch gets the host bytes with that
! part copied over them on the device. The 16-bit array is written at an
! offset that is not a multiple of 4 bytes, which the runtime syncs for.
module gpu_partial_read_01_m
use, intrinsic :: iso_fortran_env, only: int16
implicit none
contains
subroutine store(part, step)
    real, contiguous, intent(out) :: part(:)
    integer, intent(in) :: step
    integer :: i
    do concurrent (i = 1:size(part))
        part(i) = real(100*step + i)
    end do
end subroutine
subroutine store16(part, step)
    integer(int16), contiguous, intent(out) :: part(:)
    integer, intent(in) :: step
    integer :: i
    do concurrent (i = 1:size(part))
        part(i) = int(10*step + i, int16)
    end do
end subroutine
subroutine accumulate(whole, total)
    real, contiguous, intent(in) :: whole(:)
    real, contiguous, intent(inout) :: total(:)
    integer :: i
    do concurrent (i = 1:size(whole))
        total(i) = total(i) + whole(i)
    end do
end subroutine
subroutine accumulate16(whole, total)
    integer(int16), contiguous, intent(in) :: whole(:)
    integer, contiguous, intent(inout) :: total(:)
    integer :: i
    do concurrent (i = 1:size(whole))
        total(i) = total(i) + whole(i)
    end do
end subroutine
end module

program gpu_partial_read_01
use gpu_partial_read_01_m
implicit none
integer, parameter :: n = 64, parts = 4, width = n/parts
real :: a(n), total(n), expected
integer(int16) :: b(n + 1)
integer :: total16(n + 1), expected16, i, step, first
a = 0
total = 0
b = 0
total16 = 0
do step = 1, parts
    first = (step - 1)*width
    call store(a(first + 1:first + width), step)
    call accumulate(a, total)
    call store16(b(first + 2:first + width + 1), step)
    call accumulate16(b, total16)
end do
do i = 1, n
    step = (i - 1)/width + 1
    expected = real((parts - step + 1)*(100*step + i - (step - 1)*width))
    if (total(i) /= expected) error stop "real cache"
end do
if (total16(1) /= 0) error stop "int16 cache first"
do i = 2, n + 1
    step = (i - 2)/width + 1
    expected16 = (parts - step + 1)*(10*step + i - 1 - (step - 1)*width)
    if (total16(i) /= expected16) error stop "int16 cache"
end do
print *, "ok"
end program
