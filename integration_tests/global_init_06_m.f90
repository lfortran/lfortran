! A chain of modules, each with a declaration initializer that has to run as
! an executable statement, so each one gets a startup initializer of its own.
module global_init_06_base
    implicit none
    integer, target :: base_target = 3
    integer, pointer :: base_ptr => base_target
end module

module global_init_06_mid
    use global_init_06_base
    implicit none
    integer, target :: mid_target = 2
    integer, pointer :: mid_ptr => mid_target
end module

module global_init_06_top
    use global_init_06_mid
    implicit none
    integer, target :: top_target = 1
    integer, pointer :: top_ptr => top_target
end module
