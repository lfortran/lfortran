module module_optional_04
contains
   subroutine sub( a )
      integer, intent(in), optional :: a(10)
      call sub2( a(5) )
   end subroutine
   subroutine sub2( a )
      integer, intent(in), optional :: a
      if ( .not. present(a) ) error stop
   end subroutine
end module

program optional_04
   use module_optional_04
   call sub()
end
