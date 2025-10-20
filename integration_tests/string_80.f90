module string_80_mod
    integer :: i = 10
end module 
  
program string_80
    use string_80_mod
    call ff(10)
    call ff(3)

    contains 

    subroutine ff(expected_len)
        integer, intent(in) :: expected_len
        character(i), allocatable :: cc
        i = 3
        cc = "as"
        print *, len(cc)
        if(len(cc) /= expected_len) error stop
    end subroutine

end program