module pdt_nondefault_nested_base_m
    use pdt_nondefault_nested_leaf_m, only: local_leaf => leaf_t
    implicit none
    private :: local_leaf
    type :: root_t(k)
        integer, kind :: k = 4
        type(local_leaf(k)) :: leaf
        integer :: parent_default = 17
    end type
end module
