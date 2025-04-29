module template_commutative_m
    implicit none
    public

    requirement magma_r(T, bin, equal)
        type, deferred :: T
        pure elemental function bin(x, y)
            type(T), intent(in) :: x
            type(T), intent(in) :: y
            type(T) :: bin
        end function
        pure elemental function equal(x, y)
            type(T), intent(in) :: x
            type(T), intent(in) :: y
            type(T) :: equal
        end function
    end requirement

    template commutative_prop(T, bin, equal)
        require :: magma_r(T, bin, equal)
      contains
        pure function commutative_p(x, y) result(prop)
            interface operator(==)
                procedure equal
            end interface
            type(T), intent(in) :: x, y
            type(logical) :: prop

            prop = bin(x,y) == bin(y,x)
        end function
    end template

    template alt_commutative_prop(bin, equal)
        require :: magma_r(integer, bin, equal)
      contains
        pure function commutative_p(x, y) result(prop)
            interface operator(==)
                procedure equal
            end interface
            type(integer), intent(in) :: x, y
            type(logical) :: prop

            prop = bin(x,y) == bin(y,x)
        end function
    end template
end module template_commutative_m


program test_template_commutative_p
  use template_commutative_m
  instantiate alt_commutative_prop(operator(+), operator(==)), only: plus_comm => commutative_p
  instantiate commutative_prop(integer, operator(+), operator(==)), only: int_plus_comm => commutative_p
  instantiate alt_commutative_prop(operator(-), operator(==)), only: minus_comm => commutative_p
  instantiate commutative_prop(integer, operator(-), operator(==)), only: int_minus_comm => commutative_p
  print *, "test commutative"
  print *, "plus_comm: ", plus_comm(3, 4)
  print *, "int_plus_comm: ", int_plus_comm(3, 4)
  print *, "minus_comm: ", minus_comm(3, 4)
  print *, "int_minus_comm: ", int_minus_comm(3, 4)
end program
