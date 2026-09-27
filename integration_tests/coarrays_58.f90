! A saved coarray with an initial value in a module that only C drives:
! after coarrays_58c.c calls the host startup entry, lcompilers_initialize(),
! on every image, the coarray is allocated and holds its initial value on
! both images, with no Fortran main program.
module coarrays_58_m
    use iso_c_binding, only: c_int
    implicit none
    integer, save :: counter[*] = 5
contains
    ! 0, or the number of the first check that failed.
    integer(c_int) function coarrays_58_run() bind(c)
        integer :: me, other
        coarrays_58_run = 1
        if (num_images() /= 2) return
        me = this_image()
        other = 3 - me
        coarrays_58_run = 2
        if (counter /= 5) return
        sync all
        coarrays_58_run = 3
        if (counter[other] /= 5) return
        sync all
        counter = counter + me
        sync all
        coarrays_58_run = 4
        if (counter[other] /= 5 + other) return
        sync all
        coarrays_58_run = 0
    end function coarrays_58_run
end module coarrays_58_m
