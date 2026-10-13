program infer_walrus_loop_02
    implicit none
    integer :: i = 42, lfortran_inferred_idl_i = -7

    first := [(real(i), i := 1, 3)]
    second := [(i + 1, i := 1, 3)]
    third := [(real(I), I := 3, 1, -1)]
    if (any(first /= [1.0, 2.0, 3.0])) error stop 1
    if (any(second /= [2, 3, 4])) error stop 2
    if (any(third /= [3.0, 2.0, 1.0])) error stop 3
    if (i /= 42 .or. lfortran_inferred_idl_i /= -7) error stop 4
end program
