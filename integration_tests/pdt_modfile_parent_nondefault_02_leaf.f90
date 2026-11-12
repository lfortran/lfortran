module pdt_nondefault_nested_leaf_m
    implicit none
    type :: leaf_t(j)
        integer, kind :: j = 4
        integer(kind=j) :: value
    end type
end module
