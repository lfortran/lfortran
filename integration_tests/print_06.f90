program print_06
    ! Type declaration
    type even_more_nested
        character(10) :: s
    end type even_more_nested

    type another_type
        integer :: ll
        type(even_more_nested) :: inn
    end type another_type

    type tt
        type(another_type) :: another_inst
        integer :: internal_integer
        integer :: arr(5)
    end type tt

    ! Variable declaration
    type(tt) :: inst(2)
    character(100) :: output
    inst(1)%internal_integer = 100
    inst(1)%arr = 33
    inst(1)%another_inst%ll = 99
    inst(1)%another_inst%inn%s = "1234567890"
    inst(2)%internal_integer = 101
    inst(2)%arr = 33
    inst(2)%another_inst%ll = 99
    inst(2)%another_inst%inn%s = "1234567890"
    print "(I0,2X,A,10X,I0,11X,5I0)", inst

    write(output, "(I0,2X,A,10X,I0,11X,5I0)") inst(1)
    print *, output
    if (output /= "99  1234567890          100           3333333333") error stop 

    write(output, "(I0,2X,A,10X,I0,11X,5I0)") inst(2)
    print *, output
    if (output /= "99  1234567890          101           3333333333") error stop 
end program