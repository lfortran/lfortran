program binop_01
    use iso_fortran_env, only: int8
    integer(int8) :: result(5), start_, end_
    result = 24_int8
    start_ = 1_int8
    end_ = 3_int8
    print *, result(end_ - start_)
    if (any(result /= 24_int8)) error stop
end program
