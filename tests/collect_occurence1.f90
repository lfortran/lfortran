subroutine sub()
integer :: abc
print *, abc
abc = 12
print *, abc+9
end subroutine

program collection_occurence1
integer :: abc = 15
abc = abc - 9
print *, abc
call sub()
end program
