! Test RETURN <expr> with alternate returns in subroutines with ENTRY
program entry_21
    implicit none
    integer :: r

    ! Test 1: RETURN 2 should branch to 2nd alternate return label
    r = 0
    call sub(2, *10, *20)
    r = 1
    go to 30
10  r = 3
    go to 30
20  r = 5
30  if (r /= 5) error stop

    ! Test 2: RETURN 1 should branch to 1st alternate return label
    r = 0
    call sub(1, *40, *50)
    r = 1
    go to 60
40  r = 3
    go to 60
50  r = 5
60  if (r /= 3) error stop

    ! Test 3: Call via entry point, RETURN m should also work
    r = 0
    call sub2(2, *70, *80)
    r = 1
    go to 90
70  r = 3
    go to 90
80  r = 5
90  if (r /= 5) error stop

    print *, "OK"
end program

subroutine sub(n, *, *)
    integer :: n, m
    return n
    entry sub2(m, *, *)
    return m
end subroutine
