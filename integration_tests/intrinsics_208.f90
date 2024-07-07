program intrinsics_208
    use iso_fortran_env, only: sp=>real32, dp=>real64

    integer :: i = 123
    integer(8) :: j = 1234567
    integer :: size
    integer :: arr1(3) = [11, 121, 1234]
    integer, parameter :: i1 = bit_size(11)
    integer(8), parameter :: i2 = bit_size(11)
    integer, parameter :: ar1 = bit_size([1,2,3])
    integer(8), parameter :: ar2 = bit_size([1,2,3])

    print *, i1
    if( i1 /= 32 ) error stop
    print *, i2
    if( i2 /= 32 ) error stop
    print *, ar1
    if( ar1 /= 32 ) error stop
    print *, ar2
    if( ar2 /= 32 ) error stop
  
    print*, bit_size(1)
    if (bit_size(1) /= 32) error stop
  
    print*, bit_size(1_dp)
    if (bit_size(1_dp) /= 64) error stop
    
    size = bit_size(i)
    print *, size
    if( size /= 32 ) error stop
  
    print*, bit_size(j)
    if( bit_size(j) /= 64 ) error stop

    print *, bit_size(arr1)
    if( bit_size(arr1) /= 32 ) error stop

    print *, kind(bit_size(1_8))
    if( kind(bit_size(1_8)) /= 8 ) error stop
  
end program