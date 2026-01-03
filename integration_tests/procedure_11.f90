module procedure_11_mod
contains
   subroutine say_something(message)
      character(len=*),intent(in) :: message
      print *, message
      if (message /= "Hello from proc!") error stop
   end subroutine
end module 
program procedure_11
   use procedure_11_mod
   procedure(say_something), pointer :: proc
   procedure(say_something), pointer :: proc2 => say_something
   proc => say_something
   call proc("Hello from proc!")
   call proc2("Hello from proc!")
end program procedure_11