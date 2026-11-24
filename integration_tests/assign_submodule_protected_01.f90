! A PROTECTED variable of a module may be defined in the module's
! submodules, also in a submodule of a submodule (F2018 8.5.15, #17988).
module assign_submodule_protected_01_m
    implicit none
    type info_t
        integer :: version = 0
    end type
    type(info_t), protected :: info
    integer, protected :: counter = 0
    interface
        module subroutine init_info()
        end subroutine
        module subroutine count_up()
        end subroutine
    end interface
end module

submodule (assign_submodule_protected_01_m) assign_submodule_protected_01_s
contains
    module subroutine init_info()
        info%version = 3
    end subroutine
end submodule

submodule (assign_submodule_protected_01_m:assign_submodule_protected_01_s) &
        assign_submodule_protected_01_t
contains
    module subroutine count_up()
        counter = counter + 1
    end subroutine
end submodule

program assign_submodule_protected_01
    use assign_submodule_protected_01_m
    implicit none
    call init_info()
    call count_up()
    call count_up()
    if (info%version /= 3) error stop 1
    if (counter /= 2) error stop 2
    print *, info%version, counter
end program
