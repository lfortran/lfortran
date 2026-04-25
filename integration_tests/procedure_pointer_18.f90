subroutine add_ten(n)
    integer, intent(inout) :: n
    n = n + 10
end subroutine

program procedure_pointer_18
    pointer :: sp
    external :: sp, add_ten
    integer :: val

    val = 5
    sp => add_ten
    call sp(val)
    if (val /= 15) error stop
    print *, "PASS"
end program
