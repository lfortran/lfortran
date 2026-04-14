module submodule_52_mod
    implicit none
    interface
        module function string_len_dim(key) result(h)
            character(*), intent(in) :: key
            integer :: h
        end function
    end interface
end module

submodule(submodule_52_mod) submodule_52_impl
    implicit none
contains
    module function string_len_dim(key) result(h)
        character(*), intent(in) :: key
        integer :: h
        integer :: arr(len(key))
        h = size(arr)
    end function
end submodule
