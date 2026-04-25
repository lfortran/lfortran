module mod_submodule_32
    use iso_c_binding, only: c_int, c_ptr, c_f_pointer
    implicit none

    type :: team_t
        integer(c_int) :: num_images
    end type

    type :: event_t
        integer(c_int) :: counter = 0
    end type

    type(team_t), target :: initial_team
    type(event_t), pointer :: si_evt(:)

    interface
        module subroutine sync_init()
        end subroutine
    end interface

end module

submodule(mod_submodule_32) private_s
    implicit none
contains
    module procedure sync_init
        type(c_ptr) :: ptr
        associate(n => initial_team%num_images)
            call c_f_pointer(ptr, si_evt, [n])
        end associate
    end procedure
end submodule

program test_submodule_32
    use mod_submodule_32, only: initial_team
    implicit none
    initial_team%num_images = 5
    print *, "ok"
end program
