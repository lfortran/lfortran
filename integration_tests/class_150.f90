module class_150_mod
   implicit none

   integer :: animal_calls = 0
   integer :: dog_calls = 0
   integer :: cat_calls = 0

   type :: animal_t
   contains
      procedure :: speak => animal_speak
   end type

   type, extends(animal_t) :: dog_t
   contains
      procedure :: speak => dog_speak
   end type

   type, extends(animal_t) :: cat_t
   contains
      procedure :: speak => cat_speak
   end type

contains

   subroutine animal_speak(self)
      class(animal_t), intent(in) :: self
      animal_calls = animal_calls + 1
   end subroutine

   subroutine dog_speak(self)
      class(dog_t), intent(in) :: self
      dog_calls = dog_calls + 1
   end subroutine

   subroutine cat_speak(self)
      class(cat_t), intent(in) :: self
      cat_calls = cat_calls + 1
   end subroutine

end module

program class_150
   use class_150_mod
   implicit none

   integer :: i

   type :: animal_wrapper_t
      class(animal_t), allocatable :: ref
   end type

   type(animal_wrapper_t), allocatable :: animals(:)

   allocate(animals(3))
   allocate(cat_t :: animals(1)%ref)
   allocate(dog_t :: animals(2)%ref)

   do i = 1, size(animals)
      call animals(i)%ref%speak()
   end do

   if (cat_calls /= 1) error stop 1
   if (dog_calls /= 1) error stop 2
   if (animal_calls /= 1) error stop 3
   if (allocated(animals(3)%ref)) error stop 4
end program
