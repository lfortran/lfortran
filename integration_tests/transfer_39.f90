! `transfer` of a character array to an array mold whose elements have
! another size copies the characters of the source.
program transfer_39
    implicit none
    character(4) :: s(2), t(2, 2)
    character(kind=4, len=2) :: u(2)
    integer(1) :: b(8), b2(16)
    integer(2) :: k2(4)
    integer(4) :: k4(4)
    integer(8) :: k8(1)
    real(8) :: r8(1)
    integer(1), parameter :: bytes(8) = [integer(1) :: 119, 120, 121, 122, &
        112, 113, 114, 115]

    s = ["wxyz", "pqrs"]
    b = transfer(s, b)
    if (any(b /= bytes)) error stop 1
    k2 = transfer(s, k2)
    if (any(k2 /= transfer(bytes, k2))) error stop 2
    k8 = transfer(s, k8)
    if (any(k8 /= transfer(bytes, k8))) error stop 3
    r8 = transfer(s, r8)
    if (any(transfer(r8, k8) /= k8)) error stop 4
    if (any(transfer(s, b) /= bytes)) error stop 5
    if (size(transfer(s, [0_1])) /= 8) error stop 6

    t = reshape(["wxyz", "pqrs", "pqrs", "wxyz"], [2, 2])
    b2 = transfer(t, b2)
    if (any(b2(1:8) /= bytes)) error stop 7
    if (any(b2(9:12) /= bytes(5:8))) error stop 8
    if (any(b2(13:16) /= bytes(1:4))) error stop 9

    u = [4_"wx", 4_"yz"]
    k4 = transfer(u, k4)
    if (any(k4 /= [119, 120, 121, 122])) error stop 10
    print *, "OK"
end program transfer_39
