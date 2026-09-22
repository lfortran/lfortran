! A do concurrent that calls a pure function doing internal-file output. The
! function runs on the device too, and no device has formatted I/O, so the
! loop is on the unsupported list of every device: a compile error, or with
! --gpu-allow-cpu-fallback a loop that runs on the CPU.
program gpu_unsupported_07
implicit none
integer :: i
integer :: d(4)
do concurrent (i = 1:4)
    d(i) = digits_of(i * 11)
end do
do i = 1, 4
    if (d(i) /= 2) error stop
end do
print *, "PASS"
contains
    pure integer function digits_of(k)
        integer, intent(in) :: k
        character(len=12) :: s
        write(s, '(i0)') k
        digits_of = len_trim(s)
    end function
end program
