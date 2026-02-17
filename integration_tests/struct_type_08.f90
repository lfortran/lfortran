module struct_type_08_mod
  implicit none

  type :: Arrival
     integer :: NTopBnc, NBotBnc
     real    :: A, Phase
     complex :: delay
  end type

  type(Arrival), allocatable :: Arr(:, :, :)
  integer,       allocatable :: NArr(:, :)
  integer                    :: MaxNArr

contains

  subroutine AddArr(id, ir, Amp)
    integer, intent(in) :: id, ir
    real,    intent(in) :: Amp
    integer             :: iArr(1), Nt

    Nt = NArr(id, ir)

    if (Nt >= MaxNArr) then
       iArr = MINLOC( Arr(id, ir, :)%A )
       if (Amp > Arr(id, ir, iArr(1))%A) then
          Arr(id, ir, iArr(1))%A = Amp
       end if
    end if
  end subroutine

end module

program struct_type_08
  use struct_type_08_mod
  implicit none

  MaxNArr = 5
  allocate( Arr(2, 2, MaxNArr), NArr(2, 2) )
  NArr = 0
  Arr%A = 1.0

  NArr(1,1) = 5
  call AddArr(1, 1, 2.0)
  if (abs(Arr(1, 1, 1)%A - 2.0) > 1e-6) error stop
end program
