subroutine save_sub()
   integer :: i, j
   save
   real :: k, l
   double precision :: m(5), n(5, 5), o(5, 6, 7)
end subroutine

subroutine save_nested_sub()
   integer :: i, j
   contains
      subroutine save_sub()
         save
         real :: k, l
         double precision :: m(5), n(5, 5), o(5, 6, 7)
      end subroutine

      real function save_func()
         save
         real :: k, l
         double precision :: m(5), n(5, 5), o(5, 6, 7)
      end function
end subroutine

real function save_func()
   integer :: i, j
   save
   real :: k, l
   double precision :: m(5), n(5, 5), o(5, 6, 7)
end function

real function save_nested_func()
   integer :: i, j
   contains
      real function save_func()
         save
         real :: k, l
         double precision :: m(5), n(5, 5), o(5, 6, 7)
      end function

      subroutine save_sub()
         save
         real :: k, l
         double precision :: m(5), n(5, 5), o(5, 6, 7)
      end subroutine
end function

module save_module_03
   integer :: i, j
   save
   real :: k, l
   contains
      subroutine save_sub_mod()
         integer :: i, j
         save
         real :: k, l
         double precision :: m(5), n(5, 5), o(5, 6, 7)
      end subroutine

      real function save_func_mod()
         integer :: i, j
         save
         real :: k, l
         double precision :: m(5), n(5, 5), o(5, 6, 7)
      end function
end module


program save_03
   integer :: i, j
   save
   real :: k, l
   double precision :: m(5), n(5, 5), o(5, 6, 7)
end program
