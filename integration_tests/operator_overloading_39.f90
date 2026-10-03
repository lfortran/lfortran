module operator_overloading_39_mod
   implicit none
   public :: string

   type :: string
      character(len=:), allocatable :: str
   contains

    procedure :: concat
    generic :: operator(+) => concat
   end type

contains

   function concat(self, value) result(other)
      class(string), intent(in) :: self
      class(*),      intent(in) :: value
      type(string)              :: other
      select type(value)
         type is (string)
            other%str = self%str // value%str
      end select
   end function

end module 

program operator_overloading_39
  use operator_overloading_39_mod
  implicit none
  type(string) :: str1, str2, str3

  str1%str = "Hello, "
  str2%str = "world!"
  str3 = str1 + str2
  
  print *, str3%str
  if(str3%str /= "Hello, world!") error stop
end program
