program mre_eoshift_shift_rank
    implicit none
    integer :: a(2, 3), shifts(2, 2)
    shifts = 1
    a = eoshift(a, shifts)
end program
