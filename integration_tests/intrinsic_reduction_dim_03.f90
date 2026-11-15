program intrinsic_reduction_dim_03
    ! SUM, PRODUCT, MAXVAL, MINVAL, IANY, IALL and IPARITY with a constant
    ! DIM of a constant array of rank > 1 reduce along DIM when folded at
    ! compile time (#15391), also for a zero-size array (#14667).
    implicit none
    integer :: i
    integer, parameter :: a(2,3) = reshape([1, -2, 3, 4, -5, 6], [2, 3])
    logical, parameter :: m(2,3) = reshape([.true., .false., .true., &
        .true., .false., .true.], [2, 3])
    integer, parameter :: c(2,3,2) = reshape([(i, i = 1, 12)], [2, 3, 2])
    integer(1), parameter :: a1(2,3) = int(a, 1)
    integer(2), parameter :: a2(2,3) = int(a, 2)
    integer(8), parameter :: a8(2,3) = int(a, 8)
    real, parameter :: x(2,2) = reshape([1.5, 2.0, -3.0, 4.0], [2, 2])
    real(8), parameter :: y(2,2) = reshape([1.5d0, 2.0d0, -3.0d0, 4.0d0], [2, 2])
    complex, parameter :: z(2,2) = reshape([(1.0, 2.0), (3.0, 4.0), &
        (5.0, 6.0), (7.0, 8.0)], [2, 2])
    integer, parameter :: b(2,2) = reshape([3, 5, 6, 12], [2, 2])
    integer, parameter :: e(0,3) = 0

    integer, parameter :: s1(3) = sum(a, dim=1), s2(2) = sum(a, dim=2)
    integer, parameter :: p1(3) = product(a, dim=1), p2(2) = product(a, dim=2)
    integer, parameter :: mx1(3) = maxval(a, dim=1), mx2(2) = maxval(a, dim=2)
    integer, parameter :: mn1(3) = minval(a, dim=1), mn2(2) = minval(a, dim=2)
    integer, parameter :: sm1(3) = sum(a, dim=1, mask=m)
    integer, parameter :: mxm2(2) = maxval(a, dim=2, mask=m)
    integer, parameter :: sf1(3) = sum(a, dim=1, mask=.false.)
    integer, parameter :: c1(3,2) = sum(c, dim=1), c2(2,2) = sum(c, dim=2)
    integer, parameter :: c3(2,3) = sum(c, dim=3)
    integer, parameter :: cp2(2,2) = product(c, dim=2)
    integer, parameter :: cmx1(3,2) = maxval(c, dim=1)
    integer, parameter :: cmn3(2,3) = minval(c, dim=3)
    integer(1), parameter :: k1(2) = sum(a1, dim=2)
    integer(2), parameter :: k2(3) = product(a2, dim=1)
    integer(8), parameter :: k8s(3) = sum(a8, dim=1), k8m(2) = maxval(a8, dim=2)
    real, parameter :: xs1(2) = sum(x, dim=1), xn2(2) = minval(x, dim=2)
    real(8), parameter :: yp1(2) = product(y, dim=1), ym2(2) = maxval(y, dim=2)
    complex, parameter :: zs1(2) = sum(z, dim=1), zp2(2) = product(z, dim=2)
    integer, parameter :: ia1(2) = iany(b, dim=1), ia2(2) = iany(b, dim=2)
    integer, parameter :: il1(2) = iall(b, dim=1), il2(2) = iall(b, dim=2)
    integer, parameter :: ip1(2) = iparity(b, dim=1), ip2(2) = iparity(b, dim=2)
    integer, parameter :: ez(3) = iparity(e, dim=1), es(3) = sum(e, dim=1)

    integer :: r(2), r3(3)
    real :: rx(2)

    ! The program of #15391
    r = sum(reshape([1, 2, 3, 4], [2, 2]), dim=2)
    if (any(r /= [4, 6])) error stop 1
    print *, sum(reshape([1, 2, 3, 4], [2, 2]), dim=2)
    if (any(sum(reshape([1, 2, 3, 4], [2, 2]), dim=1) /= [3, 7])) error stop 2

    r = sum(a, dim=2)
    if (any(r /= [-1, 8])) error stop 3
    r3 = product(a, dim=1)
    if (any(r3 /= [-2, 12, -30])) error stop 4
    r = product(a, dim=2)
    if (any(r /= [-15, -48])) error stop 5
    r3 = maxval(a, dim=1)
    if (any(r3 /= [1, 4, 6])) error stop 6
    r = maxval(a, dim=2)
    if (any(r /= [3, 6])) error stop 7
    r3 = minval(a, dim=1)
    if (any(r3 /= [-2, 3, -5])) error stop 8
    r = minval(a, dim=2)
    if (any(r /= [-5, -2])) error stop 9
    r = sum(a, dim=2, mask=m)
    if (any(r /= [4, 10])) error stop 10
    r = product(a, dim=2, mask=.false.)
    if (any(r /= [1, 1])) error stop 11
    if (any(minval(a, dim=1, mask=m) /= [1, 3, 6])) error stop 12
    rx = maxval(x, dim=2)
    if (any(abs(rx - [1.5, 4.0]) > 1e-6)) error stop 13
    r = iparity(b, dim=2)
    if (any(r /= [5, 9])) error stop 14
    r3 = iany(e, dim=1)
    if (any(r3 /= 0)) error stop 15
    r3 = iall(e, dim=1)
    if (any(r3 /= -1)) error stop 16

    if (any(s1 /= [-1, 7, 1])) error stop 17
    if (any(s2 /= [-1, 8])) error stop 18
    if (any(p1 /= [-2, 12, -30])) error stop 19
    if (any(p2 /= [-15, -48])) error stop 20
    if (any(mx1 /= [1, 4, 6])) error stop 21
    if (any(mx2 /= [3, 6])) error stop 22
    if (any(mn1 /= [-2, 3, -5])) error stop 23
    if (any(mn2 /= [-5, -2])) error stop 24
    if (any(sm1 /= [1, 7, 6])) error stop 25
    if (any(mxm2 /= [3, 6])) error stop 26
    if (any(sf1 /= 0)) error stop 27
    if (any(c1 /= reshape([3, 7, 11, 15, 19, 23], [3, 2]))) error stop 28
    if (any(c2 /= reshape([9, 12, 27, 30], [2, 2]))) error stop 29
    if (any(c3 /= reshape([8, 10, 12, 14, 16, 18], [2, 3]))) error stop 30
    if (any(cp2 /= reshape([15, 48, 693, 960], [2, 2]))) error stop 31
    if (any(cmx1 /= reshape([2, 4, 6, 8, 10, 12], [3, 2]))) error stop 32
    if (any(cmn3 /= reshape([1, 2, 3, 4, 5, 6], [2, 3]))) error stop 33
    if (any(shape(sum(c, dim=3)) /= [2, 3])) error stop 34
    if (any(k1 /= [-1_1, 8_1])) error stop 35
    if (any(k2 /= [-2_2, 12_2, -30_2])) error stop 36
    if (any(k8s /= [-1_8, 7_8, 1_8])) error stop 37
    if (any(k8m /= [3_8, 6_8])) error stop 38
    if (any(abs(xs1 - [3.5, 1.0]) > 1e-6)) error stop 39
    if (any(abs(xn2 - [-3.0, 2.0]) > 1e-6)) error stop 40
    if (any(abs(yp1 - [3.0d0, -12.0d0]) > 1d-12)) error stop 41
    if (any(abs(ym2 - [1.5d0, 4.0d0]) > 1d-12)) error stop 42
    if (any(abs(zs1 - [(4.0, 6.0), (12.0, 14.0)]) > 1e-6)) error stop 43
    if (any(abs(zp2 - [(-7.0, 16.0), (-11.0, 52.0)]) > 1e-6)) error stop 44
    if (any(ia1 /= [7, 14]) .or. any(ia2 /= [7, 13])) error stop 45
    if (any(il1 /= [1, 4]) .or. any(il2 /= [2, 4])) error stop 46
    if (any(ip1 /= [6, 10]) .or. any(ip2 /= [5, 9])) error stop 47
    if (any(ez /= 0) .or. any(es /= 0)) error stop 48
    if (size(sum(e, dim=2)) /= 0) error stop 49
    print *, s1, s2, c2, ip1
end program
