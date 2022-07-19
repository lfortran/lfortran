program wasm1
    implicit none

    print *, a_sqr_i64(1000000_8)
    print *, add_i64(1000000000000_8, 1000000000000_8)
    print *, test_i64()

    contains

    function a_sqr_i64(x) result(r)
        implicit none
        integer(8), intent(in):: x
        integer(8) :: r
        r = x * x
        return
    end function

    function add_i64(x, y) result(r)
        implicit none
        integer(8), intent(in):: x, y
        integer(8) :: r
        r = x + y
        return
    end function

    function test_i64() result(r)
        implicit none
        integer(8) :: x64, y64
        integer(8) :: r

        x64 = 1000000000000_8
        y64 = 2000000000000_8

        r = add_i64(x64, y64)
        return
    end function
end program
