! coarrays_57 and coarrays_59 for a host that starts the coarray runtime
! itself: coarrays_60c.c calls prif_init before the host startup entry,
! lcompilers_initialize(), whose bootstrap then finds the runtime already
! started and has to accept that. The saved coarray still has to be
! allocated and hold its initial value on every image afterwards, and a
! second call of lcompilers_initialize() must neither fail nor allocate or
! initialize it again.
module coarrays_60_m
    use iso_c_binding, only: c_int
    implicit none
    integer, save :: counter[*] = 7
    integer, allocatable :: a(:)[:]
contains
    ! 0, or the number of the first check that failed.
    integer(c_int) function coarrays_60_run() bind(c)
        integer :: me, other, i
        coarrays_60_run = 1
        if (num_images() /= 2) return
        me = this_image()
        other = 3 - me
        coarrays_60_run = 2
        if (counter /= 7 .or. counter[other] /= 7) return
        coarrays_60_run = 3
        allocate(a(3)[*])
        a = 10 * me
        sync all
        do i = 1, 3
            if (a(i)[other] /= 10 * other) return
        end do
        sync all
        deallocate(a)
        counter = counter + me
        sync all
        coarrays_60_run = 0
    end function coarrays_60_run

    ! After the second lcompilers_initialize(): what coarrays_60_run left.
    integer(c_int) function coarrays_60_again() bind(c)
        integer :: me, other
        me = this_image()
        other = 3 - me
        coarrays_60_again = 1
        if (counter /= 7 + me .or. counter[other] /= 7 + other) return
        sync all
        coarrays_60_again = 0
    end function coarrays_60_again
end module coarrays_60_m
