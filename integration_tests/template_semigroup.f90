module semigroup_m
    implicit none
    private
    public :: semigroup, extended_semigroup, derive_extended_semigroup

    requirement semigroup(T, combine)
        type, deferred :: T
        elemental function combine(x, y) result(combined)
            type(T), intent(in) :: x, y
            type(T) :: combined
        end function
    end requirement

    requirement extended_semigroup(T, combine, sconcat, stimes)
        require :: semigroup(T, combine)
        pure function sconcat(list) result(combined)
            type(T), intent(in) :: list(:) !! Must contain at least one element
            type(T) :: combined
        end function
        elemental function stimes(n, a) result(repeated)
            integer, intent(in) :: n
            type(T), intent(in) :: a
            type(T) :: repeated
        end function
    end requirement

    template derive_extended_semigroup(T, combine)
        require :: semigroup(T, combine)
        private
        public :: sconcat, stimes
    contains
        pure function sconcat(list) result(combined)
            type(T), intent(in) :: list(:)
            type(T) :: combined
            integer :: i
            if (size(list) > 0) then
                combined = list(1)
                do i = 2, size(list)
                    combined = combine(combined, list(i))
                end do
            else
                error stop "Attempted to sconcat empty list"
            end if
        end function

        elemental function stimes(n, a) result(repeated)
            integer, intent(in) :: n
            type(T), intent(in) :: a
            type(T) :: repeated
            integer :: i
            if (n < 1) error stop "n must be > 0"
            repeated = a
            do i = 2, n
                repeated = combine(repeated, a)
            end do
        end function
    end template
end module

program template_semigroup
implicit none

end program