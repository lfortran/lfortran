module add_m
    implicit none
    private
    public :: add_t

    template logical_t(T)
        private
        public :: add

        type :: T
        end type

    contains
        function eq_generic(x, y)
            type(T) :: x, y
            logical :: eq_generic

            eq_generic = x == y

        end function
    end template
end module
