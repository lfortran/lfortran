program derived_types_139
   implicit none
   type :: species_t
      character(len=3) :: name
   end type species_t
   type :: basis_t
      type(species_t), allocatable :: spec(:)
      integer :: nspec = 0
   end type basis_t

   type(basis_t) :: this
   character(3), dimension(2) :: syms = ['Cu ', 'Pt ']

   this%nspec = 2
   allocate(this%spec(this%nspec))
   if (size(syms) == this%nspec) then
      this%spec(:)%name = syms
   end if
   if (this%spec(1)%name /= 'Cu ') error stop
   if (this%spec(2)%name /= 'Pt ') error stop
end program
