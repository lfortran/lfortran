module std_prop_m
    implicit none
    public

    requirement magma_r(T, bin)
        type, deferred :: T
        pure elemental function bin(x, y) result(bin)
            type(T), intent(in) :: x
            type(T), intent(in) :: y
            type(T) :: bin
        end function
    end requirement

    template commutative_prop(T,bin)
        require :: magma_r(T,bin)
      contains
        pure function commutative_p(x, y) result(prop)
            type(T), intent(in) :: x, y
            type(logical) :: prop

            prop = bin(x,y) == bin(y,x)
        end function
    end template
end module std_prop_m


program test_intrinsic_p
  use std_prop_m
  instantiate commutative_prop(integer,operator(+)), only: int_plus_comm => commutative_p
  print *, "test commutative"
  print *, "int_plus_comm: ", int_plus_comm(3,4)
end program
