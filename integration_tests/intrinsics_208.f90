program intrinsics_208
    use iso_fortran_env, only: sp=>real32, dp=>real64

    integer :: i = 123
    integer(8) :: j = 1234567
    integer :: size
  
    print*, bit_size(1)
    if (bit_size(1) /= 32) error stop
  
    print*, bit_size(1_dp)
    if (bit_size(1_dp) /= 64) error stop
    
    size = bit_size(i)
    print *, size
    if( size /= 32 ) error stop
  
    print*, bit_size(j)
    if( bit_size(j) /= 64 ) error stop
  
end program