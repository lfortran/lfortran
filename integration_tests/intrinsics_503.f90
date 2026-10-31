program intrinsics_503
    implicit none
    integer :: a(2,2), r(2,2), boundary(-3:-2), shifts(2), parent(4)
    integer :: cube(2,3,4), faces(2,4), shift3(2,4), result3(2,3,4), expected3(2,3,4)
    integer :: i, j, k, q, delta, mode
    character(2) :: words(2,2), ends(2), result_words(2,2)
    real(8) :: values(2,2), limits(2), result_values(2,2)

    a = reshape([1,2,3,4], [2,2])
    boundary = [90,80]
    shifts = [1,-1]
    r = eoshift(a, 1, boundary, dim=1)
    if (any(r /= reshape([2,90,4,80], [2,2]))) error stop 1
    r = eoshift(a, shifts, boundary, dim=1)
    if (any(r /= reshape([2,90,80,3], [2,2]))) error stop 2
    r = eoshift(a, 1, boundary)
    if (any(r /= reshape([2,90,4,80], [2,2]))) error stop 3
    r = eoshift(a, 1, boundary, dim=2)
    if (any(r /= reshape([3,4,90,80], [2,2]))) error stop 4
    r = eoshift(a, shifts, boundary, dim=2)
    if (any(r /= reshape([3,80,90,2], [2,2]))) error stop 5
    r = eoshift(a, -1, boundary, dim=1)
    if (any(r /= reshape([90,1,80,3], [2,2]))) error stop 6
    r = eoshift(a, -1, boundary, dim=2)
    if (any(r /= reshape([90,80,1,2], [2,2]))) error stop 7
    r = eoshift(a, 0, boundary, dim=1)
    if (any(r /= a)) error stop 8
    r = eoshift(a, 0, boundary, dim=2)
    if (any(r /= a)) error stop 9
    r = eoshift(a, shifts, 99)
    if (any(r /= reshape([2,99,99,3], [2,2]))) error stop 10

    parent = [90,91,80,81]
    r = eoshift(a, 1, parent(1:4:2), dim=1)
    if (any(r /= reshape([2,90,4,80], [2,2]))) error stop 11
    r = eoshift(a, shifts, parent(1:4:2), dim=1)
    if (any(r /= reshape([2,90,80,3], [2,2]))) error stop 12
    r = eoshift(a, -1, parent(4:1:-2), dim=2)
    if (any(r /= reshape([81,91,1,2], [2,2]))) error stop 13
    r = eoshift(a, shifts, parent(4:1:-2), dim=2)
    if (any(r /= reshape([3,91,81,2], [2,2]))) error stop 14
    if (any(parent /= [90,91,80,81])) error stop 15
    if (any(boundary /= [90,80])) error stop 16

    cube = reshape([(i,i=1,24)], [2,3,4])
    faces = reshape([91,92,81,82,71,72,61,62], [2,4])
    shift3 = reshape([1,-1,0,1,-1,0,1,-1], [2,4])
    do mode = -1,2
        do k = 1,4
            do j = 1,3
                do i = 1,2
                    delta = mode
                    if (mode == 2) delta = shift3(i,k)
                    q = j + delta
                    expected3(i,j,k) = faces(i,k)
                    if (q >= 1 .and. q <= 3) expected3(i,j,k) = cube(i,q,k)
                end do
            end do
        end do
        if (mode == 2) then
            result3 = eoshift(cube, shift3, faces, dim=2)
        else
            result3 = eoshift(cube, mode, faces, dim=2)
        end if
        if (any(result3 /= expected3)) error stop 17
    end do

    words = reshape(['aa','bb','cc','dd'], [2,2])
    ends = ['xx','yy']
    result_words = eoshift(words, 1, ends, dim=1)
    if (any(result_words /= reshape(['bb','xx','dd','yy'], [2,2]))) error stop 18
    result_words = eoshift(words, shifts, ends, dim=1)
    if (any(result_words /= reshape(['bb','xx','yy','cc'], [2,2]))) error stop 19
    values = reshape([1.0_8,2.0_8,3.0_8,4.0_8], [2,2])
    limits = [90.0_8,80.0_8]
    result_values = eoshift(values, 1, limits, dim=1)
    if (any(result_values /= reshape([2.0_8,90.0_8,4.0_8,80.0_8], [2,2]))) error stop 20
    result_values = eoshift(values, shifts, limits, dim=1)
    if (any(result_values /= reshape([2.0_8,90.0_8,80.0_8,3.0_8], [2,2]))) error stop 21
end program
