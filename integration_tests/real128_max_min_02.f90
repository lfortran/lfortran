program real128_max_min_02
    implicit none
    real(16) :: q(3) = [1._16, 2._16, 3._16]
    real(16), parameter :: p(3) = [-1._16, 5._16, 3._16]
    real(16), parameter :: pmax = maxval(p)
    real(16), parameter :: pmin = minval(p)
    real(16) :: e(0)

    if (maxval(q) /= 3._16 .or. minval(q) /= 1._16) error stop 1
    if (pmax /= 5._16 .or. pmin /= -1._16) error stop 2
    if (maxval(p) /= 5._16 .or. minval(p) /= -1._16) error stop 3
    if (maxval(q, q < 3._16) /= 2._16) error stop 4
    if (minval(q, q > 1._16) /= 2._16) error stop 5
    if (maxval(e) /= -huge(1._16)) error stop 6
    if (minval(e) /= huge(1._16)) error stop 7
    print *, maxval(q), minval(q)
end program
