module semigroup_m
    requirement semigroup(T, combine)
        type, deferred :: T
        subroutine combine(x)                            
            type(T) :: x       
        end subroutine
    end requirement
contains

end module