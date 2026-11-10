! A requirement argument that is not declared in the requirement is reported,
! also when a template then passes the same argument twice in a REQUIRE
! statement for that requirement (#11344).
module requirement_undeclared_argument_01_m
    implicit none

    requirement op_R { T, U, V, op_func }  ! {Error} requirement argument 'u' has not been declared in requirement 'op_r'
        deferred type :: T
        deferred type :: V
        deferred interface
            pure elemental function op_func(lhs, rhs) result(res)
                type(T), intent(in) :: lhs
                type(T), intent(in) :: rhs
                type(V) :: res
            end function
        end interface
    end requirement

    template op_t { T, V, op_func }
      require :: op_R { T, T, V, op_func }
    contains
      pure elemental function call_op_func ( x ) result(res)
        type(T), intent(in) :: x
        type(V) :: res
        res = op_func(x,x)
      end function
    end template

end module
