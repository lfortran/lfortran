program test_backspace_01

    implicit none
    character(80) :: line
    integer :: u = 10
    open(10, file='file_01_data.txt', status='old')

    read(u, '(A)') line
    write(*, '(A)') line

    backspace(u)

    read(u, '(A)') line
    write(*, '(A)') "Read and printed the same line again:"
    write(*, '(A)') line

    if (line /= "10130") error stop
    close(u)

end program test_backspace_01
