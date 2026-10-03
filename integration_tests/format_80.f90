program format_80
    implicit none
    real :: a, b
    character(16) :: string(2)
    integer :: i

    a = .087654
    b = .87654
    write (string, '(+3p," ",(f6.2))') a, b

    if (.not. all(string == ['  87.65', '876.54 '])) then
        print *, 'string(1)=', string(1)
        print *, 'string(2)=', string(2)
        error stop 1
    end if

    do i = 1, 16
        if (iachar(string(2)(i:i)) == 0) then
            print *, 'nul found at index', i
            error stop 2
        end if
    end do
end program
