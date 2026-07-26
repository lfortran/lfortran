program nested_vars_12
    use nested_vars_12_mod_a, only: process_a => process
    use nested_vars_12_mod_b, only: process_b => process
    implicit none
    integer :: val1, val2
    val1 = 5
    val2 = 5
    call process_a(val1)
    call process_b(val2)
    if (val1 /= 15 .or. val2 /= 10) then
        print *, "Failed: ", val1, val2
        error stop
    end if
    print *, "Ok"
end program nested_vars_12
