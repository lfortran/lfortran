program arrays_reshape_61
    ! reshape with order= and a shape that is not of default integer kind (#17234)
    implicit none
    character(len=3) :: src(4), grid(2,2)
    character(len=3), allocatable :: asrc(:)
    integer :: isrc(4), igrid(2,2), cube_src(24), cube(2,3,4), i, j, k
    integer, allocatable :: ia(:)
    real(8), allocatable :: ra(:)
    real(8) :: rgrid(2,2)
    integer(8) :: shp8(2), n8, m8
    integer(2) :: shp2(2)
    integer(1) :: shp1(2)

    src = ["one", "two", "six", "ten"]
    isrc = [1, 2, 3, 4]
    shp8 = [2_8, 2_8]
    shp2 = [2_2, 2_2]
    shp1 = [2_1, 2_1]
    n8 = 2
    m8 = 2

    ! character source, shape of kind 8: constant, variable, constructor
    grid = reshape(src, [2_8, 2_8], order=[2, 1])
    if (any(grid(1,:) /= ["one", "two"]) .or. any(grid(2,:) /= ["six", "ten"])) error stop 1
    grid = reshape(src, shp8, order=[2, 1])
    if (any(grid(1,:) /= ["one", "two"]) .or. any(grid(2,:) /= ["six", "ten"])) error stop 2
    grid = reshape(src, [n8, m8], order=[2, 1])
    if (any(grid(1,:) /= ["one", "two"]) .or. any(grid(2,:) /= ["six", "ten"])) error stop 3

    ! shape of kinds 2 and 1
    grid = reshape(src, shp2, order=[2, 1])
    if (any(grid(1,:) /= ["one", "two"]) .or. any(grid(2,:) /= ["six", "ten"])) error stop 4
    grid = reshape(src, shp1, order=[2, 1])
    if (any(grid(1,:) /= ["one", "two"]) .or. any(grid(2,:) /= ["six", "ten"])) error stop 5

    ! allocatable character source
    allocate(asrc(4))
    asrc = src
    grid = reshape(asrc, [2_8, 2_8], order=[2, 1])
    if (any(grid(1,:) /= ["one", "two"]) .or. any(grid(2,:) /= ["six", "ten"])) error stop 6

    ! shape and order of default kind
    grid = reshape(src, [2, 2], order=[2, 1])
    if (any(grid(1,:) /= ["one", "two"]) .or. any(grid(2,:) /= ["six", "ten"])) error stop 7

    ! allocatable integer and real sources
    allocate(ia(4))
    ia = isrc
    igrid = reshape(ia, [2_8, 2_8], order=[2, 1])
    if (any(igrid(1,:) /= [1, 2]) .or. any(igrid(2,:) /= [3, 4])) error stop 8
    igrid = reshape(ia, shp8, order=[2, 1])
    if (any(igrid(1,:) /= [1, 2]) .or. any(igrid(2,:) /= [3, 4])) error stop 9
    igrid = reshape(ia, [n8, m8], order=[2, 1])
    if (any(igrid(1,:) /= [1, 2]) .or. any(igrid(2,:) /= [3, 4])) error stop 10
    igrid = reshape(ia, shp2, order=[2, 1])
    if (any(igrid(1,:) /= [1, 2]) .or. any(igrid(2,:) /= [3, 4])) error stop 11
    allocate(ra(4))
    ra = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
    rgrid = reshape(ra, shp8, order=[2, 1])
    if (any(rgrid(1,:) /= [1.0d0, 2.0d0]) .or. any(rgrid(2,:) /= [3.0d0, 4.0d0])) error stop 12

    ! fixed-size integer source
    igrid = reshape(isrc, shp8, order=[2, 1])
    if (any(igrid(1,:) /= [1, 2]) .or. any(igrid(2,:) /= [3, 4])) error stop 13

    ! without order
    igrid = reshape(ia, shp8)
    if (any(igrid(:,1) /= [1, 2]) .or. any(igrid(:,2) /= [3, 4])) error stop 14
    if (size(reshape(ia, shp8), 1) /= 2 .or. size(reshape(ia, shp8), 2) /= 2) error stop 15

    ! rank 3
    cube_src = [(i, i = 1, 24)]
    cube = reshape(cube_src, [2_8, 3_8, 4_8], order=[3, 1, 2])
    do k = 1, 4
        do j = 1, 3
            do i = 1, 2
                if (cube(i,j,k) /= k + 4*(i-1) + 8*(j-1)) error stop 16
            end do
        end do
    end do

    print *, "OK"
end program
