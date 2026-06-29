program pointer_17
    use iso_c_binding, only: c_float
    implicit none
    real(c_float), target :: data(4) = [1.0, 2.0, 3.0, 4.0]
    real(c_float), pointer :: src(:) => null()
    real(8) :: dst(4)
    real(8) :: expected(4)

    expected = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
    src => data
    dst = src

    if (any(dst /= expected)) error stop
    print *, dst
end program
