! coarrays_53 with the modules reached only through a separately compiled
! external subroutine: the program does not use them, yet every image has to
! allocate the saved coarray, and associate `p` with it, before the first
! statement of the program.
program coarrays_54
    implicit none
    interface
        subroutine coarrays_54_s()
        end subroutine coarrays_54_s
    end interface
    call coarrays_54_s()
end program coarrays_54
