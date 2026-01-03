program derived_types_37
    implicit none
    
    TYPE test_type2
        integer :: num
    END TYPE test_type2

    TYPE test_type1
        integer :: num
        integer :: arr_1(3)
        type(test_type2):: arr_2(3)
    END TYPE test_type1

    integer :: i
    
    TYPE(test_type1), DIMENSION(5) :: main_arr
    TYPE(test_type1), allocatable :: main_arr_alloc(:)
    
    main_arr%num = 44
    do i =1, 5
        print *, main_arr(i)%num
        if(main_arr(i)%num /= 44) error stop
    end do
    
    
    main_arr(1)%arr_1 = 33
    do i =1, 3
        print *,main_arr(1)%arr_1(i)
        if(main_arr(1)%arr_1(i) /= 33) error stop
    end do

    main_arr(1)%arr_2%num = 22
    do i =1, 3
        print *,main_arr(1)%arr_2(i)%num
        if(main_arr(1)%arr_2(i)%num /= 22) error stop
    end do

    ! Duplicate test with allocatable array
    ! to check that implied array assignment in structInstanceMember isn't dependent on known compile time size.
    allocate(main_arr_alloc(5))
    main_arr_alloc%num = 44
    do i =1, 5
        print *, main_arr_alloc(i)%num
        if(main_arr_alloc(i)%num /= 44) error stop
    end do
    
    
    main_arr_alloc(1)%arr_1 = 33
    do i =1, 3
        print *,main_arr_alloc(1)%arr_1(i)
        if(main_arr_alloc(1)%arr_1(i) /= 33) error stop
    end do

    main_arr_alloc(1)%arr_2%num = 22
    do i =1, 3
        print *,main_arr_alloc(1)%arr_2(i)%num
        if(main_arr_alloc(1)%arr_2(i)%num /= 22) error stop
    end do
end program derived_types_37
