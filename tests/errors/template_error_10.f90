module Requirement_ordering_m
    implicit none

    requirement op_R ( T, T, V, V, op_func )
        type, deferred :: T
        type, deferred :: V
        pure elemental function op_func(lhs, rhs) result(res)
            type(T), intent(in) :: lhs
            type(T), intent(in) :: rhs
            type(V) :: res
        end function
    end requirement

    template op_t ( T, V, op_func )
      require :: op_R ( T, T, V, V, op_func )
    contains
      pure elemental function call_op_func ( x ) result(res)
        type(T) :: x
        type(V) :: res
        res = op_func(x,x)
      end function
    end template

end module
