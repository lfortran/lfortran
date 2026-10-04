! A plugin that uses only an allocatable coarray, which coarrays_61c.c loads,
! closes and loads again on every image; ci/test_caffeine.sh builds and runs
! it. Nothing of it stays registered with the coarray runtime once the check
! returns, so the library can be unloaded.
module coarrays_61_m
    use iso_c_binding, only: c_int
    implicit none
    integer, allocatable :: a(:)[:]
contains
    ! 0, or the number of the first check that failed.
    integer(c_int) function coarrays_61_run() bind(c)
        integer :: me, other, i
        me = this_image()
        other = num_images() + 1 - me
        coarrays_61_run = 2
        if (allocated(a)) return
        allocate(a(2)[*])
        a = 100 * me
        sync all
        coarrays_61_run = 3
        do i = 1, 2
            if (a(i)[other] /= 100 * other) return
        end do
        deallocate(a)
        coarrays_61_run = 0
    end function coarrays_61_run
end module coarrays_61_m
