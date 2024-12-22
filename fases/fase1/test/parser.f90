
module tokenizer
implicit none

contains
function tolower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i

        lower_str = str 
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
end function tolower

function nextSym(input, cursor) result(lexeme)
    character(len=*), intent(in) :: input
    integer, intent(inout) :: cursor
    character(len=:), allocatable :: lexeme
    integer :: initialCursor

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    


    initialCursor = cursor
    do while (cursor <= len_trim(input) .and. ((iachar(input(cursor:cursor)) >= iachar("0") .and. &
        iachar(input(cursor:cursor)) <= iachar("9"))))
        cursor = cursor + 1
    end do
    if (cursor > initialCursor) then
        allocate(character(len=cursor-initialCursor)::lexeme)
        lexeme = input(initialCursor:cursor-1)
        return
    end if

    initialCursor = cursor
    do while (cursor <= len_trim(input) .and. ((iachar(input(cursor:cursor)) >= iachar("a") .and. &
        iachar(input(cursor:cursor)) <= iachar("z"))& 
    .or. (iachar(input(cursor:cursor)) >= iachar("A") .and. &
        iachar(input(cursor:cursor)) <= iachar("Z"))))
        cursor = cursor + 1
    end do
    if (cursor > initialCursor) then
        allocate(character(len=cursor-initialCursor)::lexeme)
        lexeme = input(initialCursor:cursor-1)
        return
    end if

    initialCursor = cursor
    do while (cursor <= len_trim(input) .and. ((input(cursor:cursor) == char(32))& 
    .or. (input(cursor:cursor) == char(9))& 
    .or. (input(cursor:cursor) == char(10))& 
    .or. (input(cursor:cursor) == char(13))))
        cursor = cursor + 1
    end do
    if (cursor > initialCursor) then
        allocate(character(len=cursor-initialCursor)::lexeme)
        lexeme = input(initialCursor:cursor-1)
        return
    end if


    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module tokenizer 

program parser
    use tokenizer
    implicit none

    character(len=*), parameter :: input = "10 hola 3 5 mundo       &
    &56"
    character(len=:), allocatable :: lexeme
    integer :: cursor

    cursor = 1
    do while (lexeme /= "EOF" .and. lexeme /= "ERROR")
        lexeme = nextSym(input, cursor)
        print *, lexeme
    end do
end program parser