module derived_types_118_mod1
   implicit none

   type :: SomeType
      integer :: id
      real(8) :: value
   end type SomeType

end module derived_types_118_mod1


module derived_types_118_mod2
   use derived_types_118_mod1
   implicit none

   type, abstract :: BaseType
      integer :: base_id
   end type BaseType

   type, abstract, extends(BaseType) :: Extended1Type
      class(SomeType), allocatable :: objs(:)
   end type Extended1Type

   type, extends(Extended1Type) :: Extended2Type
      integer :: extra_data
   end type Extended2Type

   interface Extended2Type
      procedure :: constructor
   end interface Extended2Type

contains

   function constructor(objs) result(self)
      type(Extended2Type)         :: self
      class(SomeType), intent(in) :: objs(:)

      self%base_id   = 100
      self%extra_data = 999

      allocate(self%objs, source=objs)

   end function constructor

end module derived_types_118_mod2


program derived_types_118
   use derived_types_118_mod2
   implicit none

   type(SomeType)        :: input(3)
   type(Extended2Type)   :: obj
   integer               :: i

   do i = 1, 3
      input(i)%id    = i
      input(i)%value = 10.0d0 * i
   end do

   obj = Extended2Type(input)

   ! 1. Check inherited base member
   if (obj%base_id /= 100) then
      error stop "Base member not initialized correctly"
   end if

   ! 2. Check own member
   if (obj%extra_data /= 999) then
      error stop "Extended member not initialized correctly"
   end if

   ! 3. Check allocation
   if (.not. allocated(obj%objs)) then
      error stop "objs not allocated"
   end if

   ! 4. Check size
   if (size(obj%objs) /= 3) then
      error stop "objs size incorrect"
   end if

   ! 5. Check values copied correctly
   do i = 1, 3
      if (obj%objs(i)%id /= i) then
         error stop "ID not copied correctly"
      end if
      if (abs(obj%objs(i)%value - 10.0d0*i) > 1.0d-12) then
         error stop "Value not copied correctly"
      end if
   end do

   print *, "All inheritance + constructor tests passed."

end program derived_types_118