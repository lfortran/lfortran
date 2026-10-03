program template_host_var_05
    ! A template hosted in the main program and instantiated inside an
    ! internal procedure of the program refers to the program's variables
    ! by host association: its instantiation must share them, not own
    ! private copies.
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

    call s()
    print *, counter, hist
    if (counter /= 103) error stop
    if (hist(1) /= 1) error stop

    call s_local()
    print *, counter, hist
    if (counter /= 106) error stop
    if (hist(1) /= 2) error stop

    counter = 7
    if (f() /= 7) error stop

    if (fill_in_s() /= 14) error stop
    n = 100
    m = 100
    print *, fill_in_s()
    if (fill_in_s() /= 5150) error stop
contains
    subroutine s()
        instantiate tmpl {integer}, only: bump
        counter = 100
        call bump()
    end subroutine

    ! a local of the same name does not capture the template's reference
    subroutine s_local()
        instantiate tmpl {integer}, only: bump
        integer :: counter
        counter = 50
        call bump()
        if (counter /= 50) error stop
    end subroutine

    integer function f()
        instantiate tmpl {integer}, only: get
        f = get()
    end function

    integer function fill_in_s()
        instantiate tmpl {integer}, only: fill
        fill_in_s = fill()
    end function
end program
