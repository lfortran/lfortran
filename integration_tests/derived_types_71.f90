module derived_type_71_mod
    type build_target_t
        integer :: key
    end type
    type build_target_ptr
        type(build_target_t), pointer :: ptr
    end type build_target_ptr
end module

program derived_type_71
    use derived_type_71_mod
    type(build_target_t), pointer :: x
    type(build_target_t), target :: z
    type(build_target_ptr) :: y

    z%key = 12
    x => z
    y = build_target_ptr(x)
    if (y%ptr%key /= 12) error stop
end program  