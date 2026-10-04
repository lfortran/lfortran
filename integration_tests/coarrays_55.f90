! Saved coarrays of separately compiled external procedures, which the two
! images call in opposite orders, and one that no image calls. Allocating a
! coarray is collective, so allocating each one on first entry would pair
! image 1's allocation of `ca` with image 2's of `cb`: all of them have to be
! allocated at startup, in the same order on every image.
program coarrays_55
    implicit none
    interface
        subroutine coarrays_55_never()
        end subroutine coarrays_55_never
        subroutine coarrays_55_a(other)
            integer, intent(in) :: other
        end subroutine coarrays_55_a
        subroutine coarrays_55_b(other)
            integer, intent(in) :: other
        end subroutine coarrays_55_b
    end interface
    integer :: me

    me = this_image()
    if (num_images() /= 2) error stop 9

    if (me == 1) then
        call coarrays_55_a(3 - me)
        call coarrays_55_b(3 - me)
    else
        call coarrays_55_b(3 - me)
        call coarrays_55_a(3 - me)
    end if
    if (me > 100) call coarrays_55_never()

    if (me == 1) print *, "ok"
end program coarrays_55
