! Assignment of a rank-reduced section whose scalar subscripts precede
! its section subscripts must check the shape of the section, not of the
! leading subscripts (issue #16051).
program array_section_assign_08
    implicit none
    type :: seeds_t
        integer :: paraseed(2,3)
    end type
    type(seeds_t) :: s
    real :: a(2,5), b(4,2,5), c(2,3), d(4,3), e(2,5)
    integer :: prngout(4,2,3)
    integer :: i, j, k, mepos

    b = reshape([(real(i), i=1,40)], shape(b))

    a = b(2,:,:)
    do k = 1, 5
        do j = 1, 2
            if (a(j,k) /= b(2,j,k)) error stop 1
        end do
    end do

    i = 3
    a = b(i,:,:)
    if (any(a /= b(3,:,:))) error stop 2

    c = b(4,1:2,2:4)
    if (any(c /= b(4,:,2:4))) error stop 3

    c = b(1,:,1:5:2)
    if (abs(c(2,3) - b(1,2,5)) > 0) error stop 4

    d = b(:,2,1:3)
    if (abs(d(4,3) - b(4,2,3)) > 0) error stop 5

    e = b(1,[2,1],:)
    if (abs(e(1,1) - b(1,2,1)) > 0) error stop 6
    if (abs(e(2,5) - b(1,1,5)) > 0) error stop 7

    prngout = reshape([(i, i=1,24)], shape(prngout))
    mepos = 1
    s%paraseed = prngout(mepos+1,:,:)
    if (any(s%paraseed /= reshape([2, 6, 10, 14, 18, 22], [2, 3]))) error stop 8

    print *, a(:,1)
    print *, s%paraseed
end program
