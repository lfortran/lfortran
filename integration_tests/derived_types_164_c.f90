module derived_types_164_c
    use derived_types_164_a, only: u => t, z, zk, tgt
    implicit none
    type(u) :: mv = z
    type(u) :: mvk = zk
    type(u), parameter :: mp = z
    type(u), pointer :: ptr => tgt
    type :: holder
        type(u) :: c = z
    end type
contains
    integer function local_value()
        use derived_types_164_a, only: t, z
        type(t) :: x = z
        local_value = x%i + x%j
    end function
end module
