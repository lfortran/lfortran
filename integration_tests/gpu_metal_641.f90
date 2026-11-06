program gpu_metal_641
    ! SUM over array expressions whose shape is found through an array
    ! conversion (integer array times a real scalar, real(ix)), a unary
    ! minus or abs, SUM of an elemental call with two array arguments,
    ! and ALL over an elemental call, inside an offloaded do concurrent.
    implicit none
    integer :: ix(4), x(3), y(3), j, k(3)
    real :: b(4), r1(3), r2(3), r3(3), r4(3), r5(3), r6(3), r7(3)
    logical :: l(3)
    ix = [1, -2, 3, -4]
    b = [1.0, -2.0, 3.0, -4.0]
    x = [1, 2, 3]
    y = [4, 5, 6]
    do concurrent (j = 1:3)
        r1(j) = sum(ix * 0.5) * j
        r2(j) = sum(ix**2 * 0.5) + j
        r3(j) = sum(-b**2) + j
        r4(j) = sum(-real(ix)*2) + j
        k(j) = sum(-ix*2) * j
        r5(j) = sum(abs(b)) * j
    end do
    if (any(abs(r1 - [-1.0, -2.0, -3.0]) > 1e-3)) error stop 1
    if (any(abs(r2 - [16.0, 17.0, 18.0]) > 1e-3)) error stop 2
    if (any(abs(r3 - [-29.0, -28.0, -27.0]) > 1e-3)) error stop 3
    if (any(abs(r4 - [5.0, 6.0, 7.0]) > 1e-3)) error stop 4
    if (any(k /= [4, 8, 12])) error stop 5
    if (any(abs(r5 - [10.0, 20.0, 30.0]) > 1e-3)) error stop 6
    do concurrent (j = 1:3)
        l(j) = all(f(ix) > real(j) - 5.5)
        r6(j) = sum(g(x, y)) + j
        r7(j) = sum(g(x, y)**2) + j
    end do
    if (.not. l(1) .or. l(2) .or. l(3)) error stop 7
    if (any(abs(r6 - [36.0, 37.0, 38.0]) > 1e-3)) error stop 8
    if (any(abs(r7 - [508.0, 509.0, 510.0]) > 1e-2)) error stop 9
contains
    elemental real function f(i)
        integer, intent(in) :: i
        f = real(i)
    end function
    elemental real function g(a, c)
        integer, intent(in) :: a, c
        g = real(a*c) + 1
    end function
end program
