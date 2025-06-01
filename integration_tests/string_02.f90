program string_02
implicit none

character(len = 15) :: surname, firstname
character(len = 6) :: title
character(len = 25)::greetings
integer, parameter :: SCK = selected_char_kind("ascii")
character(kind=SCK) :: one_len

title = 'Mr. '
firstname = 'Rowan '
surname = 'Atkinson'
greetings = 'A big hello from Mr. Bean'
one_len = "hello"
print *, 'Here is ', title, firstname, surname
print *, greetings

if (one_len /= "h") error stop
end program
