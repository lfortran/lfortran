program array_shape_10
    ! The array assigned to the substring of each element of `names(:)` has
    ! more elements than the section
    implicit none
    character(len=4) :: names(3)
    character(len=2) :: t(4)
    integer :: n
    character(len=16) :: buf
    buf = "4"
    read(buf, *) n
    t = ['xy', 'zw', 'uv', 'qr']
    names = ['abcd', 'efgh', 'ijkl']
    names(:)(2:3) = t(1:n)
    print *, names
end program array_shape_10
