! Try lots of different print statements
program print_07
    integer(8) :: ii1
    integer :: ii2
    integer :: ii3
    real(8) :: rr
    real(4) :: rr2
    logical :: ll
    logical :: ll_arr(10)
    integer,allocatable :: arr(:)
    integer, allocatable :: aarr(:)
    character :: arr2(10)
    integer(8) :: arr23(10)
    character(:), allocatable :: str
    character(10) :: str2
    character(5) :: ssss(10)
    complex :: cc
    allocate(aarr(10))
    allocate(arr(10))
    ssss = "medoo"
    ii1 = 20000000000000000_8
    ii2 = 22
    ii3 = 33
    rr = 10.3_8
    rr2 = 10.3
    ll = .false.
    arr = 1
    arr(1) =  33
    arr2 = 's'
    print *,   ii1, ii2, ii3, ll
    str = "HelloLFortran"
    str2 = "HelloLCompilers"
    arr23 = 1
    cc = complex(2,3)
    ll_arr = .true.
    ll_arr(10) = .false.
    aarr = 1
    print *,aarr
    print *, cc
    print *, arr23
    print "(a,2x,a,3x,a)", str, str, str
    print "(a,2x,a,3x,a)", str2
    print "(i0)", ii1 ,ii1, ii1
    print *,str
    print *, str,str
    print "(A3)", ssss
    print *, ii2
    print *, rr
    print *, ll_arr
    print *, rr2
    print *, ll
    print *, 232
    print *, "string", "string"
    PRINT "(I0, 2X , I0)", arr23
    print *,arr23
    print *, arr
    print *, "here"
    print *, str, str
    print "(l)", ll
  
  
end program