program coarrays_19
    integer :: ia(3)
    real :: r
    complex :: z
    character(3) :: s, sa(2)
    logical :: l

    if (this_image() == 2) then
        ia = [10, 20, 30]
        r = 2.5
        z = (2.0, -3.0)
        s = "yes"
        sa = ["foo", "bar"]
        l = .true.
    else
        ia = [0, 0, 0]
        r = 0.0
        z = (0.0, 0.0)
        s = "no "
        sa = ["xxx", "yyy"]
        l = .false.
    end if

    call co_broadcast(ia, source_image=2)
    call co_broadcast(r, source_image=2)
    call co_broadcast(z, source_image=2)
    call co_broadcast(s, source_image=2)
    call co_broadcast(sa, source_image=2)
    call co_broadcast(l, source_image=2)

    if (ia(1) /= 10 .or. ia(2) /= 20 .or. ia(3) /= 30) error stop 1
    if (r /= 2.5) error stop 2
    if (z /= (2.0, -3.0)) error stop 3
    if (s /= "yes") error stop 4
    if (sa(1) /= "foo" .or. sa(2) /= "bar") error stop 5
    if (.not. l) error stop 6
end program coarrays_19
