module MazeSolving
    use PositionStacks

    ! character parameters for representing maze space types
    character, parameter :: WALL = '*',   &
                               OPEN = '.',   &
                               START = 'o',  &
                               FINISH = 'e', &
                               PATH = '#'
    
    ! Data Structure to hold:
    !   - Map Representation of the maze (integer 2d array)
    !       - Array indices from; 1:width 1:height
    !   - Width and Height of the maze
    !   - Position of start and finish points within map (Position type)    
    !       - See PositionStacks.f95
    type MazeMap 
        character, dimension(:, :), allocatable :: map
        integer :: width, height
        type( Position ), pointer :: start, finish
        type( PStack ), pointer :: stack
    end type MazeMap

    CONTAINS

    ! Allocate memory for the MazeMap array, aand Position type members
    ! Sets the width and height variables within the MazeMap as well
    subroutine initializeMazeMap(maze, width, height) 
        type( MazeMap ), intent(inout) :: maze
        integer, intent(in) :: width, height

        allocate( maze%map(height, width), maze%stack, maze%start, maze%finish )
    
        maze%width = width
        maze%height = height
    end subroutine initializeMazeMap

    ! Reads a maze in from filename, translates it into an intger map representation
    ! and then stores it within the MazeMap structure.
    ! While translating the maze from char -> int representation, the algorithm recognizes
    ! and stores the values of the start and end points.
    ! LIMITATION: Mazes can be up to 100 spaces wide.
    subroutine readMazeMap(maze, filename)
        type( MazeMap ), intent(inout) :: maze
        character( len = * ), intent(in) :: filename
        character( 100 ) :: line
        integer :: width, height, i = 0, j = 1, start_lock = 0, finish_lock = 0
        logical :: fflag

        ! open the file and read the first line to get COLUMNS, ROWS
        inquire(file=filename, exist = fflag)
        if (fflag) then
            open(unit = 15, file = filename)
        else
            write(*,*) 'File does not exist'
            stop
        end if

        read(15, *) width, height

        ! initialize the MazeMap with the newly read values
        call initializeMazeMap(maze, width, height)

        do 
            i = i + 1
            read( 15,*, IOSTAT = io_stat ) line
            if (io_stat > 0) then ! IO error occured
                write(*,*) '*****IO ERROR*****'
                stop !program
            else if (io_stat < 0) then ! EOF was reached
                exit !loop
            else !proceed normally
                j=1 !reset j to iterate over j chars in line
                do while( j <= width )
                    !Store maze in 2d char array in MazeMap DS
                    select case (line(j:j))
                        case (WALL, OPEN)
                            maze%map(i,j) = line(j:j)
                        case (START)
                            ! checkpoint to make sure there are not multiple start points
                            if (start_lock == 0) then
                                maze%start%r = i
                                maze%start%c = j

                                start_lock = 1
                            else
                                write(*,*) 'ERROR - multiple starts detected'
                                stop !exit program
                            end if
                            maze%map(i,j) = line(j:j)
                        case (FINISH)
                            ! checkpoint to make sure there are not multiple finish points
                            if (finish_lock == 0) then
                                maze%finish%r = i
                                maze%finish%c = j

                                finish_lock = 1
                            else
                                write(*,*) 'ERROR - multiple finishes detected'
                                stop !exit program
                            end if
                            maze%map(i,j) = line(j:j)
                        case default
                            write(*,*) "ERROR - invalid char in maze"
                            stop !exit program
                    end select
                    j = j + 1
                end do
            end if
        end do

        if (start_lock == 0 ) then
            write(*,*) 'ERROR - no start detected'
            stop
        else if (finish_lock == 0) then
            write(*,*) 'ERROR - no finish detected'
            stop
        endif
            
    end subroutine readMazeMap

    subroutine solveMazeMap(maze)
        type( MazeMap ), intent(inout) :: maze
        type( Position ), pointer :: curr_pos, next_pos
        integer :: i = 0, r, c
        character :: test_location
        logical :: master_lock = .true.

        ! allocate 2 position type pointers for current and next positions
        allocate( curr_pos )
        allocate( next_pos )

        !set current position to start position read from maze file
        call setPosition( curr_pos, maze%start%r, maze%start%c )

        !push the start position onto the stack
        call push( maze%stack, curr_pos%r, curr_pos%c )

        do while( master_lock )
            !check if we are at the finish 
            if ( curr_pos%r == maze%finish%r .AND. curr_pos%c == maze%finish%c ) then 
                master_lock = .false.
                exit ! break out of loop immemdiatley if at finish
            endif
            
            !Set the current position in the maze map to WALL type so it cannot be 
            ! revisited if a new path must be found.
            maze%map( curr_pos%r, curr_pos%c ) = WALL

            i=0
            !Use i variable to loop through possible moves (S,E,N,W...tested in that order).
            !When i == 4, we know that the tests of S,E,N,W all failed, so we pop the stack and try on the previous square. 
            do while ( i < 5 )
                !Compute a move to [S,E,N,W], and store those new co-ordinates in the next_pos variable.
                if ( i ==  0 ) then !move south
                    next_pos%r = curr_pos%r + 1
                    next_pos%c = curr_pos%c;
                else if ( i == 1 ) then !move east
                    next_pos%r = curr_pos%r
                    next_pos%c = curr_pos%c + 1
                else if ( i == 2 ) then !move north
                    next_pos%r = curr_pos%r - 1
                    next_pos%c = curr_pos%c
                else if ( i == 3 ) then !move west
                    next_pos%r = curr_pos%r
                    next_pos%c = curr_pos%c - 1
                end if

                !check that we do not overrun the array indices (they start at 1!)
                if( next_pos%r > 0 .and. next_pos%c > 0 ) then
                    !Use next_pos co-ordinates to get equiv position in the MazeMap (array)
                    !We then test this position for suitability for the next move.
                    test_location = maze%map(next_pos%r , next_pos%c)
                    select case (test_location)
                        !We only want to move to spaces that are open or are the finish.
                        case( OPEN, FINISH )
                            !If we find one of these spaces, move the value from the next_pos varialbe into curr_pos .
                            !   -In essence, the next_pos variable is a proposed space to move to, 
                            !    and if it is found suitable we validate its value by making curr_pos = next_pos).
                            curr_pos%r = next_pos%r
                            curr_pos%c = next_pos%c
                            !Push the updated current position onto the stack.
                            call push(maze%stack, curr_pos%r, curr_pos%c)
                            !Set i=5 to break out of loop before next iteration
                            i = 5
                    end select
                end if

                i = i + 1

                !i=4 when all of S,E,N,W tests fail. Thus we must pop the head off the stack and
                !move back a space.
                if ( i == 4 ) then
                    ! Check to make sure we are not popping the start node
                    ! if start node is being popped, then the maze has no solution.
                    if (maze%stack%head%pos%r == maze%start%r .AND. maze%stack%head%pos%c == maze%start%c) then
                        write(*,'(A)', advance ='no') ''//NEW_LINE('A')//'No solution to the maze' & 
                            //NEW_LINE('A')//'  * attempted solution shown below:' //NEW_LINE('A')//''
                        !break out of solving loop, there is no solution
                        master_lock = .false.
                    else !everthing is normal 
                        call pop(maze%stack, r, c)
                    end if

                    !Current position is always on the top of the stack.
                    curr_pos%r = maze%stack%head%pos%r
                    curr_pos%c = maze%stack%head%pos%c

                    !Ensure inner loop is broken out of.
                    i = 5
                end if
            end do
        end do

    end subroutine solveMazeMap

    !Print the results of solving the maze
    subroutine printMazeMapResults(maze)
        type( MazeMap ), intent(inout) :: maze
        type( PStackNode ), pointer :: temp
        integer :: i = 1, j = 1, r = -1, c = -1

        !For iterating over the stack
        temp => maze%stack%head
        !Trace out the final path by iterating over the stack.
        do while (associated(temp)) 
            r = temp%pos%r
            c = temp%pos%c

            maze%map(r,c) = PATH

            temp => temp%next
        end do

        !Set the start and finish back to their appropriate chars for richer user view
        maze%map(maze%start%r, maze%start%c) = START
        maze%map(maze%finish%r, maze%finish%c) = FINISH

        write(*,'(A)', advance ='no') ''//NEW_LINE('A')//''//NEW_LINE('A')// &
            '   ***SOLUTION***' //NEW_LINE('A')// ''

        !Print out the maze.
        do while (i <= maze%height)
            write(*, "(A)", advance = "no") ''//NEW_LINE('A')//"    "
            j=1
            do while (j <= maze%width)
                write(*, "(A)", advance = "no") maze%map(i,j) 
                write(*, "(A)", advance = "no") ' '
                j = j+1
            end do
            i = i + 1
        end do

        !Print out maze/solution stats.
        write(*,'(A)', advance ='no') ''//NEW_LINE('A')//''//NEW_LINE('A')// &
            '   Maze Stats' //NEW_LINE('A')// '   ----------' //NEW_LINE('A')//''
        write(*,*) "   Start(r,c):        ", maze%start%r, maze%start%c
        write(*,*) "   Finish(r,c):       ", maze%finish%r, maze%finish%c
        write(*,*) "   Width:             ", maze%width
        write(*,*) "   Height:            ", maze%height
        write(*,*) "   Total Path Length: ", maze%stack%size
        write(*,*) "   Required Moves:    ", maze%stack%size - 1 

        if((maze%stack%size - 1) == 0) then
            write(*,'(A)', advance ='no') ''//NEW_LINE('A')//''//NEW_LINE('A')// &
                '   ***SOLUTION FAILED***' //NEW_LINE('A')// '' 
        else
            write(*,'(A)', advance ='no') ''//NEW_LINE('A')//''//NEW_LINE('A')// &
                '   ***SOLUTION COMPLETED***' //NEW_LINE('A')// ''  
        end if
    end subroutine printMazeMapResults
end module MazeSolving
 
