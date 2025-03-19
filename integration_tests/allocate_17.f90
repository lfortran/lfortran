program allocate_17
    implicit none
    integer, allocatable :: arr1(:), arr2(:), arr3(:), arr4(:)
    logical, allocatable :: l_arr1(:), l_arr2(:), l_arr3(:)
    integer, parameter :: src(5) = [2,1,3,4,1]
    logical, parameter :: l_src(3) = [.true.,.false.,.true.]
    integer :: isrc
    isrc = 10

    allocate(arr1(5))
    arr1 = isrc
    if (any(arr1 /= 10)) error stop

    allocate(arr2(5), source=isrc)
    if (any(arr2 /= 10)) error stop

    allocate(arr3(5), arr4(5), source=src)
    if (any(arr3 /= [2,1,3,4,1])) error stop
    if (any(arr4 /= [2,1,3,4,1])) error stop

    allocate(l_arr1(3), source = .true.)
    if (any(l_arr1 .neqv. .true.)) error stop

    allocate(l_arr2(3), l_arr3(3), source = l_src)
    if (any(l_arr2 .neqv. [.true., .false., .true.])) error stop
    if (any(l_arr3 .neqv. [.true., .false., .true.])) error stop
end program
