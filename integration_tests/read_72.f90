function get_adv() result(r)
    character(:), allocatable :: r
    r = "YES"
end function

program read_72
    ! Test that advance= specifier works with non-constant expressions
    ! (function calls and variables, not just string literals)
    implicit none
    interface
        function get_adv() result(r)
            character(:), allocatable :: r
        end function
    end interface
    integer :: u, ios
    character(1) :: c
    character(3) :: adv_var

    open(newunit=u, status='scratch', form='formatted', action='readwrite')
    write(u, '(A)') "AB"
    write(u, '(A)') "CD"
    write(u, '(A)') "EF"
    rewind(u)

    ! Test 1: advance= with function call returning allocatable string
    read(u, '(A1)', advance='NO', iostat=ios) c
    if (c /= 'A' .or. ios /= 0) error stop
    read(u, '(A1)', advance=get_adv(), iostat=ios) c
    if (c /= 'B' .or. ios /= 0) error stop
    ! Should now be on second record
    read(u, '(A1)', advance='NO', iostat=ios) c
    if (c /= 'C' .or. ios /= 0) error stop

    ! Test 2: advance= with variable
    adv_var = "YES"
    read(u, '(A1)', advance=adv_var, iostat=ios) c
    if (c /= 'D' .or. ios /= 0) error stop
    ! Should now be on third record
    read(u, '(A1)', advance='NO', iostat=ios) c
    if (c /= 'E' .or. ios /= 0) error stop

    close(u)
    print *, "PASS"
end program
