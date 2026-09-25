module template_host_var_01_m
    implicit none
    integer :: gcounter = 0
    integer :: ghist(3) = 0
    template tmpl {t}
        deferred type :: t
    contains
        subroutine bump()
            gcounter = gcounter + 3
            ghist(1) = ghist(1) + 1
        end subroutine
        integer function get()
            get = gcounter
        end function
        integer function get_hist(i)
            integer, intent(in) :: i
            get_hist = ghist(i)
        end function
    end template
end module

program template_host_var_01
    use template_host_var_01_m, only: tmpl, gcounter, ghist
    implicit none
    instantiate tmpl {integer}, only: bump, get, get_hist

    ! write through the host-associated module variables
    call bump()
    print *, gcounter, ghist
    if (gcounter /= 3) error stop
    if (ghist(1) /= 1) error stop

    ! reads see the current values, not the initial ones
    gcounter = 7
    ghist(2) = 5
    if (get() /= 7) error stop
    if (get_hist(2) /= 5) error stop

    call bump()
    if (gcounter /= 10) error stop
    if (get() /= 10) error stop
    if (get_hist(1) /= 2) error stop
    print *, gcounter, ghist
end program
