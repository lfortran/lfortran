subroutine sub (array)
    dimension array(*)
    double precision array
end subroutine

program attr_dim_01
    double precision , dimension(3) :: arr
    call sub(arr)
end program