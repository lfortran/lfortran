! Allocatable coarrays of a module that only C drives: coarrays_57c.c calls
! the host startup, lfortran_initialize(), on every image and then
! this procedure. There is no Fortran main program, so that startup is what
! has to start the coarray runtime.
module coarrays_57_m
    use iso_c_binding, only: c_int
    implicit none
    integer, allocatable :: a(:)[:]
contains
    ! 0, or the number of the first check that failed.
    integer(c_int) function coarrays_57_run() bind(c)
        integer :: me, other, i
        coarrays_57_run = 1
        if (num_images() /= 2) return
        me = this_image()
        other = 3 - me
        coarrays_57_run = 2
        if (allocated(a)) return
        allocate(a(3)[*])
        a = 10 * me
        sync all
        coarrays_57_run = 3
        do i = 1, 3
            if (a(i)[other] /= 10 * other) return
        end do
        sync all
        deallocate(a)
        coarrays_57_run = 0
    end function coarrays_57_run
end module coarrays_57_m
