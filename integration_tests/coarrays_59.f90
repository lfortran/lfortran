! Saved coarrays with initial values in a module that only C drives: once
! coarrays_59c.c has called the host startup, lfortran_initialize(),
! on every image, another image's initial values can be read at once, with
! no SYNC ALL of the program's own: the startup that initialized them on
! every image waits for all of them before it returns.
module coarrays_59_m
    use iso_c_binding, only: c_int
    implicit none
    integer, save :: counter[*] = 7
    integer, save :: values(3)[*] = [1, 2, 3]
contains
    ! 0, or the number of the first check that failed.
    integer(c_int) function coarrays_59_run() bind(c)
        integer :: other, i
        other = num_images() + 1 - this_image()
        coarrays_59_run = 2
        if (counter[other] /= 7) return
        coarrays_59_run = 3
        do i = 1, 3
            if (values(i)[other] /= i) return
        end do
        sync all
        coarrays_59_run = 0
    end function coarrays_59_run
end module coarrays_59_m
