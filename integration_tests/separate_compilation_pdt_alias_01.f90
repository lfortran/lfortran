module sc_pdt_alias_base_m
    implicit none
    type payload_t(k)
        integer, kind :: k = 4
        integer(k) :: value
    end type
end module

module sc_pdt_alias_user_m
    use sc_pdt_alias_base_m, only: renamed_t => payload_t
    implicit none
    private :: renamed_t
    type wrapper_t(k)
        integer, kind :: k = 4
        type(renamed_t(k)) :: inner
    contains
        procedure :: get
    end type
    interface
        module function get(self) result(v)
            class(wrapper_t), intent(in) :: self
            type(renamed_t) :: v
        end function
    end interface
end module

submodule (sc_pdt_alias_user_m) sc_pdt_alias_user_s
contains
    module procedure get
        v = self%inner
    end procedure
end submodule

program separate_compilation_pdt_alias_01
    use sc_pdt_alias_base_m, only: payload_t
    use sc_pdt_alias_user_m, only: wrapper_t
    implicit none
    type(payload_t) :: x
    type(wrapper_t) :: box
    box%inner%value = 42
    x = box%get()
    if (x%value /= 42) error stop 1
end program
