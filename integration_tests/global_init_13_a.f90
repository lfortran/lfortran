! The derived type whose pointer components have initial targets, and the
! external procedures two of them are associated with; see global_init_13.f90.
module global_init_13_a
    use iso_c_binding, only: c_int
    use global_init_13_c, only: c_tgt, renamed_arr => c_arr, c_twice
    implicit none
    integer, parameter :: k = 3
    integer, target :: arr(3) = [11, 12, 13]
    character(len=5), target :: str = "abcde"
    type :: holder
        integer :: v = 21
        integer :: w(2) = [22, 23]
    end type
    type(holder), target :: dt
    type(holder), target :: dts(2)
    abstract interface
        integer function iface(x)
            integer, intent(in) :: x
        end function iface
        integer(c_int) function iface_c(x) bind(c)
            import :: c_int
            integer(c_int), value :: x
        end function iface_c
    end interface
    interface
        integer function ext_thrice(x)
            integer, intent(in) :: x
        end function ext_thrice
        integer(c_int) function ext_bindc(x) bind(c, name="global_init_13_ext_bindc")
            import :: c_int
            integer(c_int), value :: x
        end function ext_bindc
    end interface
    type :: t
        integer, pointer :: p_elem => arr(2)
        integer, pointer :: p_elem_k => arr(k)
        integer, pointer :: p_again => arr(2)
        integer, pointer :: p_comp => dt%v
        integer, pointer :: p_comp_elem => dt%w(2)
        integer, pointer :: p_elem_comp => dts(2)%v
        character(len=3), pointer :: p_sub => str(2:4)
        integer, pointer :: p_c => c_tgt
        integer, pointer :: p_ren_elem => renamed_arr(3)
        procedure(iface), pointer, nopass :: fp_c => c_twice
        procedure(iface), pointer, nopass :: fp_ext => ext_thrice
        procedure(iface_c), pointer, nopass :: fp_bindc => ext_bindc
    end type
end module global_init_13_a

integer function ext_thrice(x)
    integer, intent(in) :: x
    ext_thrice = 3*x
end function ext_thrice

integer(c_int) function ext_bindc(x) bind(c, name="global_init_13_ext_bindc")
    use iso_c_binding, only: c_int
    integer(c_int), value :: x
    ext_bindc = 4*x
end function ext_bindc
