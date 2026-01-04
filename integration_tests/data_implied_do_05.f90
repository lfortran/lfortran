program data_implied_do_05
    implicit none
    integer :: a(2, 3)
    integer :: b(2, 3)
    integer :: i, j

    ! Nested implied do loop: inner loop varies i, outer loop varies j
    ! Order: (1,1)=1, (2,1)=2, (1,2)=3, (2,2)=4, (1,3)=5, (2,3)=6
    data ((a(i,j),i=1,2),j=1,3) / 1, 2, 3, 4, 5, 6 /

    ! Nested implied do loop: inner loop varies j, outer loop varies i
    ! Order: (1,1)=1, (1,2)=2, (1,3)=3, (2,1)=4, (2,2)=5, (2,3)=6
    data ((b(i,j),j=1,3),i=1,2) / 1, 2, 3, 4, 5, 6 /

    ! Verify a
    if (a(1,1) /= 1) error stop
    if (a(2,1) /= 2) error stop
    if (a(1,2) /= 3) error stop
    if (a(2,2) /= 4) error stop
    if (a(1,3) /= 5) error stop
    if (a(2,3) /= 6) error stop

    ! Verify b
    if (b(1,1) /= 1) error stop
    if (b(1,2) /= 2) error stop
    if (b(1,3) /= 3) error stop
    if (b(2,1) /= 4) error stop
    if (b(2,2) /= 5) error stop
    if (b(2,3) /= 6) error stop

    print *, "PASS"
end program
