program compare_pointers_01
    implicit none

    integer, target :: int_num1, int_num2
    integer, pointer :: int_ptr1 => null(), int_ptr2 => null()

    real, target :: real_num1, real_num2
    real, pointer :: real_ptr1 => null(), real_ptr2 => null()

    int_num1 = 1
    int_num2 = 2

    real_num1 = 1.0
    real_num2 = 2.0

    int_ptr1 => int_num1
    int_ptr2 => int_num2

    real_ptr1 => real_num1
    real_ptr2 => real_num2

    ! FIXME: this prints incorrectly in LFortran as `False`
    print *, int_ptr1 < int_ptr2
    if (int_ptr1 < int_ptr2 ) then
        print *, "comparison passed"
    else
        error stop
    end if

    ! FIXME: this prints incorrectly in LFortran as `False`
    print *, int_ptr2 > int_ptr1
    if (int_ptr2 > int_ptr1) then
        print *, "comparison passed"
    else
        error stop
    end if

    ! FIXME: this comparison currently fails in LLVM backend
    ! print *, real_ptr1 < real_ptr2
    ! if (real_ptr1 < real_ptr2 ) then
    !     print *, "comparison passed"
    ! else
    !     error stop
    ! end if
end program compare_pointers_01
