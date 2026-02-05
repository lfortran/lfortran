module separate_compilation_class_star_01_mod
implicit none
private
public :: get_value

contains

subroutine get_value(g)
    class(*), intent(out) :: g
    select type(g)
        type is (integer)
            g = 42
    end select
end subroutine get_value

end module separate_compilation_class_star_01_mod
