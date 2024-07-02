program derived_type_06
    implicit none
    
    TYPE test_type2
        integer :: num
    END TYPE test_type2

    TYPE test_type1
        type(test_type2):: arr_2(3)
    END TYPE test_type1

    TYPE(test_type1), DIMENSION(5) :: main_arr

    main_arr%arr_2%num = 22


end program derived_type_06
