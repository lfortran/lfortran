module c_module_fixed_array_scope_01_a
    implicit none
    type :: target_box
        integer :: data(3)
        integer :: marker
    end type
    type(target_box), target :: target_obj
end module

module c_module_fixed_array_scope_01_b
    implicit none
    type :: plain_box
        integer :: data(2, 2)
        integer :: marker
    end type
    type(plain_box) :: plain_obj
end module

program c_module_fixed_array_scope_01
    use c_module_fixed_array_scope_01_a, only: target_box, target_obj
    use c_module_fixed_array_scope_01_b, only: plain_obj
    implicit none
    type(target_box) :: local_obj

    target_obj%marker = 7
    plain_obj%marker = 11
    local_obj%marker = 13
    if (target_obj%marker /= 7) error stop 1
    if (plain_obj%marker /= 11) error stop 2
    if (local_obj%marker /= 13) error stop 3
    print *, 7
end program
