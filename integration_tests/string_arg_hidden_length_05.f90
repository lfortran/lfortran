! The length of a character actual can be an expression with a call in it,
! here the length of the associate name `text_after_colon` (which depends on
! `index(raw_line, ':')`) used in a substring passed to `index`. The length
! passed for the actual must be evaluated where the actual is.
program string_arg_hidden_length_05
implicit none
character(len=:), allocatable :: raw_line
character(len=:), allocatable :: v
raw_line = 'key: "abc"'
associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
    associate(o => index(text_after_colon, '"'))
        associate(c => o + index(text_after_colon(o+1:), '"'))
            v = text_after_colon(o+1:c-1)
        end associate
    end associate
end associate
print *, v
if (v /= 'abc') error stop
end program
