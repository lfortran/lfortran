program read_01
    character(len=2) :: str = "42"
 
    character(len=2) :: c
    integer :: i32
    integer(8) :: i64
    real :: f32
    real(8) :: f64
 
    read(str, *) c
    print *, c
    if (c /= "42") error stop
 
    read(str, *) i32
    print *, i32
    if (i32 /= 42) error stop
 
    read(str, *) i64
    print *, i64
    if (i64 /= 42) error stop
 
    read(str, *) f32
    print *, f32
    if (f32 /= 42.0) error stop
 
    read(str, *) f64
    print *, f64
    if (f64 /= 42.0) error stop
 
 end program
 