module functions_14
interface str
   module procedure msg_scalar
end interface str

contains

function msg_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none

! ident_14="@(#) M_CLI2 msg_scalar(3fp) writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep_local)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=''
   if(present(generic0))call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   msg_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=1));     write(line(istart:),'(i0)') generic
      type is (integer(kind=2));    write(line(istart:),'(i0)') generic
      type is (integer(kind=4));    write(line(istart:),'(i0)') generic
      type is (integer(kind=8));    write(line(istart:),'(i0)') generic
      type is (real(kind=4));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=8))
         write(line(istart:),'(1pg0)') generic
      !x! DOES NOT WORK WITH NVFORTRAN: type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical)
         write(line(istart:),'(l1)') generic
      type is (character(len=*))
         write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_scalar

subroutine journal(where, g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep)
implicit none

! ident_13="@(#) M_CLI2 journal(3f) writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1, g2, g3, g4, g5, g6, g7, g8 ,g9
class(*),intent(in),optional  :: ga, gb, gc, gd, ge, gf, gg, gh ,gi, gj
character(len=*),intent(in),optional :: sep
write(*,'(a)') str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep)
end subroutine journal

end module functions_14
