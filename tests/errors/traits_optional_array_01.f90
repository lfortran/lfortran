! An optional borrowed runtime trait view is supported; an optional
! runtime trait array dummy is diagnosed as not implemented yet.
module traits_optional_array_01_m
    implicit none
    abstract interface :: IValue
    end interface
contains
    integer function count_views(objs)
        class(IValue), optional, intent(in) :: objs(:)
        count_views = 0
    end function count_views
end module traits_optional_array_01_m
