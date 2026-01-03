module modules_40_tomlf_de_tokenizer
   implicit none

   integer, parameter :: tfc = 4

    type :: toml_key
      character(kind=tfc, len=:), allocatable :: key
   end type toml_key


   type, abstract :: toml_tokenizer

    contains

      procedure, private :: parse_select

   end type toml_tokenizer

contains


subroutine parse_select(de)
    class(toml_tokenizer), intent(inout), target :: de

contains

   subroutine fill_stack(de, top, stack)
      class(toml_tokenizer), intent(inout), target :: de
      integer, intent(out) :: top
      type(toml_key), allocatable, intent(out) :: stack(:)

      do
         if (top >= size(stack)) then
            call resize(stack)
         end if
      end do

   end subroutine fill_stack

   subroutine resize(stack, n)
      type(toml_key), allocatable, intent(inout) :: stack(:)
      integer, intent(in), optional :: n
   end subroutine resize

end subroutine parse_select

end module modules_40_tomlf_de_tokenizer

program modules_40
implicit none

print *, "executing modules_40"

end program modules_40
