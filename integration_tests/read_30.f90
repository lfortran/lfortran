! Test for issue #9518: READ with implied DO loop and explicit format
program read_implied_do
    implicit none
    integer :: i
    integer :: arr(10)
    integer :: unit_num
    character(len=100) :: test_file

    test_file = 'read_30_data.txt'

    ! Write test data
    open(newunit=unit_num, file=test_file, status='replace', action='write')
    write(unit_num, '(5I5)') 1, 2, 3, 4, 5
    write(unit_num, '(5I5)') 6, 7, 8, 9, 10
    close(unit_num)

    ! Initialize array to zeros
    arr = 0

    ! Read using implied DO loop with explicit format (the case from issue #9518)
    ! The bounds are constant (1, 5) so the ASR pass should flatten this to:
    ! read(unit_num, '(5I5)') arr(1), arr(2), arr(3), arr(4), arr(5)
    open(newunit=unit_num, file=test_file, status='old', action='read')
    read(unit_num, '(5I5)') (arr(i), i=1, 5)
    
    ! Verify first 5 values
    if (arr(1) /= 1) error stop "arr(1) incorrect"
    if (arr(2) /= 2) error stop "arr(2) incorrect"
    if (arr(3) /= 3) error stop "arr(3) incorrect"
    if (arr(4) /= 4) error stop "arr(4) incorrect"
    if (arr(5) /= 5) error stop "arr(5) incorrect"

    ! Read second line
    read(unit_num, '(5I5)') (arr(i), i=6, 10)
    close(unit_num, status='delete')

    ! Verify remaining values
    if (arr(6) /= 6) error stop "arr(6) incorrect"
    if (arr(7) /= 7) error stop "arr(7) incorrect"
    if (arr(8) /= 8) error stop "arr(8) incorrect"
    if (arr(9) /= 9) error stop "arr(9) incorrect"
    if (arr(10) /= 10) error stop "arr(10) incorrect"

    print *, "All tests passed!"

end program read_implied_do
