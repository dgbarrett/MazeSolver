program main
    use MazeSolving
    
    character(len = 25) :: filename !Filename is max 25 chars long
    
    type( MazeMap ), pointer :: maze
    allocate(maze)

    write(*,*) "Please enter a maze filename, then hit enter:"
    write(*,*) "----------------------------------------------"
    read(*,*) filename
    
    call readMazeMap(maze, filename)
    call solveMazeMap(maze)
    call printMazeMapResults(maze) 
end program 