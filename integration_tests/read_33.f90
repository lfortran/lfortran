program read_33
    implicit none
    integer :: ivi, jvi
    integer :: i1i(5)

    open(10, file='read_33.txt', status='replace')
    write(10, '(6I5)') 3, 10, 20, 30, 0, 0
    close(10)

    open(10, file='read_33.txt', status='old')
    read(10, '(100I5)') ivi, (i1i(jvi), jvi=1,ivi)
    close(10, status='delete')

    if (ivi /= 3 .or. any(i1i(1:3) /= [10, 20, 30])) error stop    
end program read_33