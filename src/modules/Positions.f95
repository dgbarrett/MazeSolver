module Positions
    IMPLICIT NONE
    !DS Holds an row and column value to denote a position in map
    type Position
        integer :: r, c 
    end type Position

    CONTAINS

     !Set the (r,c) values of a Position type variable (pos)
    subroutine setPosition(pos, r ,c) 
        type( Position ), intent(inout) :: pos
        integer, intent(in) :: r, c
        pos%r = r
        pos%c = c
    end subroutine setPosition
end module Positions