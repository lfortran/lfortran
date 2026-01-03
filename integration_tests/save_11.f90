subroutine foo_save_11(reference)
    integer , intent(in) :: reference 
    integer, save , dimension(2) :: arr = [1,1]
    complex :: c  = (1,1) 
        arr = arr + 1 
        c = c + (1, 1)
    print *, arr
    if(any(arr /= reference)) error stop
    print *, c
    if(c /= complex(reference, reference)) error stop

end subroutine foo_save_11
program save_11
    call foo_save_11(2)
    call foo_save_11(3)
end program save_11