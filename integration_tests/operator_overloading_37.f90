module operator_overloading_37_mod
   implicit none
   type, abstract :: base
   contains
      procedure(binop), deferred :: add
      procedure(binop), deferred :: sub
      generic :: operator(+) => add
      generic :: operator(-) => sub
   end type base

   type, extends(base) :: mytype
      real :: val
   contains
      procedure :: add => my_add
      procedure :: sub => my_sub
   end type mytype

   abstract interface
      function binop(a, b) result(r)
         import :: base
         class(base), intent(in) :: a, b
         class(base), allocatable :: r
      end function binop
   end interface

contains

   function my_add(a, b) result(r)
      class(mytype), intent(in) :: a
      class(base), intent(in) :: b
      class(base), allocatable :: r
      select type (b)
      type is (mytype)
         allocate(mytype :: r)
         select type (r)
         type is (mytype)
            r%val = a%val + b%val
         end select
      end select
   end function my_add

   function my_sub(a, b) result(r)
      class(mytype), intent(in) :: a
      class(base), intent(in) :: b
      class(base), allocatable :: r
      select type (b)
      type is (mytype)
         allocate(mytype :: r)
         select type (r)
         type is (mytype)
            r%val = a%val - b%val
         end select
      end select
   end function my_sub

end module operator_overloading_37_mod

program operator_overloading_37
   use operator_overloading_37_mod
   implicit none
   class(base), allocatable :: a, b, tmp, res

   allocate(mytype :: a)
   select type (a)
   type is (mytype)
      a%val = 0.0
   end select

   allocate(tmp, source = (a - a))

   select type (tmp)
   type is (mytype)
      allocate(b, source = tmp)
      tmp%val = 1.0
   end select

   allocate(res, source = (b + tmp))

   select type (res)
   type is (mytype)
      print *, "res =", res%val
      if (abs(res%val - 1.0) > 1.0e-6) error stop "FAIL: expected 1.0"
   end select

   ! Also verify subtraction still works
   deallocate(res)
   allocate(res, source = (tmp - b))

   select type (res)
   type is (mytype)
      print *, "res =", res%val
      if (abs(res%val - 1.0) > 1.0e-6) error stop "FAIL: expected 1.0 from sub"
   end select
end program operator_overloading_37
