submodule(enum_07_parent_mod) enum_07_sub_impl
contains
    module subroutine do_work(x)
        integer, intent(inout) :: x
        x = x + 1
    end subroutine
end submodule
