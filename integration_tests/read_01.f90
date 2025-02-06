program read_01
    character(len=2) :: str = "42"
 
    character(len=2) :: c
    integer :: i32
    integer(8) :: i64
    real :: f32
    real(8) :: f64
    integer, save, dimension(3) :: i32_arr_size3 = [-1,-1,-1]
    character(20) :: key
 
    character(5) :: val
    character(len=:), allocatable :: al_val

    character(len=:), allocatable :: al_str

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

    allocate(character(len=2) :: al_str)

    val = "ab"

    read(val, *) al_str
    print *, al_str
    if (al_str /= "ab") error stop

    allocate(character(len=2) :: al_val)

    str = "cd"

    read(str, *) al_val
    print *, al_val
    if (al_val /= "cd") error stop

    key = "int_read"
    select case(key)
        case ("int_read")
            read(str, *) i32
        case ("int_arr_size3_read")
            read(str, *) i32_arr_size3
    end select
    print *, i32
    if (i32 /= 42) error stop
end program
