program wasm_unary_minus
    implicit none

    print *, get_num_neg_i32(13), get_num_neg_i64(130000000000000000_8)
    print *, get_num_neg_i32(-13), get_num_neg_i64(-130000000000000000_8)
    print *, test_unary_minus()
    
    contains
    function get_num_neg_i32(x) result(r)
        implicit none
        integer(4), intent(in) :: x
        integer(4) :: r
        r = -x
        return 
    end function

    function get_num_neg_i64(x) result(r)
        implicit none
        integer(8), intent(in) :: x
        integer(8):: r
        r = -x
        return 
    end function

    function test_unary_minus() result(r)
        implicit none
        integer(8) :: r
        r = (-15000000000000_8) - (-16000000000000_8)
        return
    end function

end program
