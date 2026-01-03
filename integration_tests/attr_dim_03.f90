subroutine sub (array)
    dimension array(*)
    double precision array
end subroutine

program attr_dim_03
    double precision , dimension(3) :: arr
    call sub(arr)
end program