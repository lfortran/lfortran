! Saved coarrays of a submodule: one declared in the submodule itself, one
! in a separate module procedure it defines. Only image 1 calls that
! procedure, so both have to be allocated at startup on every image rather
! than on first entry; coarrays_48 and coarrays_50 cover the ones of modules,
! programs and procedures.
module coarrays_56_m
    implicit none
    interface
        module subroutine remote_sum(other, v)
            integer, intent(in) :: other
            integer, intent(out) :: v
        end subroutine remote_sum
    end interface
end module coarrays_56_m

submodule (coarrays_56_m) coarrays_56_s
    implicit none
    integer, save :: cs[*] = 30
contains
    module subroutine remote_sum(other, v)
        integer, intent(in) :: other
        integer, intent(out) :: v
        integer, save :: cp[*] = 40
        v = cs[other] + cp[other]
    end subroutine remote_sum
end submodule coarrays_56_s

program coarrays_56
    use coarrays_56_m
    implicit none
    integer :: me, v

    me = this_image()
    sync all
    if (me == 1) then
        call remote_sum(2, v)
        if (v /= 70) error stop 1
        print *, "ok"
    end if
    sync all
end program coarrays_56
