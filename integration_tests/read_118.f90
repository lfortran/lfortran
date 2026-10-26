! List-directed internal read of a count followed by an allocatable
! deferred-size character array dummy (issue #15947)
module read_118_mod
implicit none
contains
subroutine read_words_line(iu,words,nread)
integer,intent(in)::iu
character(len=*),allocatable,intent(out)::words(:)
integer,intent(out),optional::nread
character(len=100)::text
integer::nw
read(iu,'(a)') text
read(text,*) nw
nw=max(nw,0)
allocate(words(nw))
if(nw>0) read(text,*) nw,words
if(present(nread)) nread=size(words)
end subroutine
end module

program read_118
use read_118_mod
implicit none
character(len=20),allocatable::words(:)
integer::i, n
open(10,status='scratch',action='readwrite')
write(10,'(a)') '3 alpha beta gamma'
rewind(10)
call read_words_line(10,words)
close(10)
print *,size(words)
do i=1,size(words)
   print '(a)',trim(words(i))
end do
if (size(words) /= 3) error stop 1
if (words(1) /= 'alpha') error stop 2
if (words(2) /= 'beta') error stop 3
if (words(3) /= 'gamma') error stop 4

open(11,status='scratch',action='readwrite')
write(11,'(a)') '2 one two'
rewind(11)
call read_words_line(11,words,n)
close(11)
if (n /= 2) error stop 5
if (size(words) /= 2) error stop 6
if (words(1) /= 'one') error stop 7
if (words(2) /= 'two') error stop 8
end program
