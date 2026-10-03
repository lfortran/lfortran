program inquire_11
    implicit none
    integer :: u, pos_val
    u = 10

    open(unit=u, file="inquire_11_tmp.txt", status="replace")
    write(u, '(A)') 'Line 1'
    inquire(unit=u, pos=pos_val)
    if (pos_val /= 6) error stop

    write(u, '(A)') 'Line 2'
    inquire(unit=u, pos=pos_val)
    if (pos_val /= 12) error stop

    write(u, '(A)') 'Hello World!'
    inquire(unit=u, pos=pos_val)
    if (pos_val /= 24) error stop

    close(u, status="delete")
    print *, "All tests passed."
end program inquire_11
