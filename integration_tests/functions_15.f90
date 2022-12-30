module functions_15
implicit none
character(len=:),allocatable,save :: keywords(:)
character(len=:),allocatable,save :: shorts(:)
character(len=:),allocatable,save :: values(:)
logical,allocatable,save          :: present_in(:)
contains

function atleast(line,length,pattern) result(strout)

! ident_40="@(#) M_strings atleast(3f) return string padded to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=*),intent(in),optional       :: pattern
character(len=max(length,len(trim(line)))) :: strout
if(present(pattern))then
   strout=line//repeat(pattern,len(strout)/len(pattern)+1)
else
   strout=line
endif
end function atleast

subroutine print_dictionary(header,stop)
character(len=*),intent(in),optional :: header
logical,intent(in),optional          :: stop
integer          :: i
   write(*,'(a,1x,a,1x,a,1x,a)')atleast('KEYWORD',max(len(keywords),8)),'SHORT','PRESENT','VALUE'
   write(*,'(*(a,1x,a5,1x,l1,8x,"[",a,"]",/))') &
   & (atleast(keywords(i),max(len(keywords),8)),shorts(i),present_in(i),values(i)(:counts(i)),i=1,size(keywords))
end subroutine print_dictionary
end module functions_15
