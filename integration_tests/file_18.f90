program file_18
    implicit none
    integer :: j = 11, x = 43, y =-54
    integer :: nlines = 0, io

    open(j, file="file_18_data.txt", status='old')
    write(j, *) x
    write(j, *) y
    close(j)

    open (j, file = 'file_18_data.txt', status='old')
    do
      read(j, *, iostat=io)
      if (io/=0)  exit
      nlines = nlines + 1
    end do
    close (j)

    print *, nlines
    if (nlines /= 2) error stop

end program file_18
