subroutine sub (array)
    dimension array(3)
    double precision array
end subroutine

program subroutines_19
    implicit none
    double precision , dimension(3) :: arr
    call sub(arr)
end program