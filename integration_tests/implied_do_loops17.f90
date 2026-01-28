! Test that the loop variable in an array constructor implied-DO
! is isolated and does not affect outer variables with the same name
program implied_do_loops17
    implicit none
    integer :: a(5)
    integer :: i, j
    real :: b(3)

    ! Test 1: Basic isolation test
    i = 0
    a = [(1, i = 1, 5)]
    if (i /= 0) error stop

    ! Test 2: Multiple implied-DO loops should each restore their variable
    j = 42
    a = [(j, j = 1, 5)]
    if (j /= 42) error stop

    ! Test 3: With expression in the implied-DO body
    i = 100
    a = [(i*2, i = 1, 5)]
    if (i /= 100) error stop

    ! Test 4: Nested implied-DO should also isolate variables
    i = 10
    j = 20
    b = [(real(i+j), i = 1, 3)]
    if (i /= 10) error stop
    if (j /= 20) error stop

    print *, "PASSED"
end program implied_do_loops17
