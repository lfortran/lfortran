program string_111
    implicit none

    if (.not. ('a' < CHAR(255))) error stop
    if (.not. (CHAR(0) < CHAR(255))) error stop
    if (.not. (CHAR(127) < CHAR(128))) error stop
    if (.not. (CHAR(255) > 'a')) error stop
    if (.not. (CHAR(255) >= CHAR(255))) error stop
    if (.not. (CHAR(200) <= CHAR(200))) error stop
    if (.not. (CHAR(100) /= CHAR(200))) error stop
    if (.not. (CHAR(200) == CHAR(200))) error stop

end program
