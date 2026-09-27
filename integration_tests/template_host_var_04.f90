program template_host_var_04
    ! A template hosted in the main program refers to the program's
    ! variables by host association: its instantiation must share them,
    ! not own private copies.
    implicit none
    integer :: counter = 0
    integer :: hist(3) = 0
    integer :: n = 4
    integer :: m = 4
    template tmpl {t}
        deferred type :: t
    contains
        subroutine bump()
            counter = counter + 3
            hist(1) = hist(1) + 1
        end subroutine
        integer function get()
            get = counter
        end function
        ! the declaration bound and the loop bound are the same variables
        ! as the program's, so they must agree with each other
        integer function fill()
            integer :: loc(n)
            integer :: i
            do i = 1, m
                loc(i) = i
            end do
            fill = size(loc) + sum(loc)
        end function
    end template
    instantiate tmpl {integer}, only: bump, get, fill

    ! write through the host-associated program variables
    call bump()
    print *, counter, hist
    if (counter /= 3) error stop
    if (hist(1) /= 1) error stop

    ! reads see the current values, not the initial ones
    counter = 7
    if (get() /= 7) error stop
    call bump()
    if (counter /= 10) error stop
    if (hist(1) /= 2) error stop

    if (fill() /= 14) error stop
    n = 100
    m = 100
    print *, fill()
    if (fill() /= 5150) error stop
end program
