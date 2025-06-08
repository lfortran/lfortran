subroutine foo_save_10(reference)
    integer, intent(in) :: reference(2)
    integer, save :: calls(2) = [1, 1]
    calls = calls + 1
    print *, 'FOO: ', calls, reference
    if (all(calls /= reference)) error stop
end subroutine foo_save_10
    
program save_10
    integer i
    do i=2,4
        call foo_save_10([i,i])
    end do
end program save_10