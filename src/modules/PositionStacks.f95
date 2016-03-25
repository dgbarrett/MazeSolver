module PositionStacks
    use Positions

    !Node for use in Stack-like DS to be used for maze solving
    !Each node holds a Position, along with navigational pointers
    type PStackNode
        type( Position ), pointer :: pos
        type( PStackNode ), pointer :: next => null()
    end type PStackNode

    !Stack DS
    type PStack 
        integer :: size = 0; 
        type( PStackNode ), pointer :: head => null() !head of stack
    end type PStack

    CONTAINS

    !Push the co-ordinate pair (r,c) onto the stack
    subroutine push(stack, r, c)
        type( PStack ), intent(inout) :: stack 
        integer, intent(in) :: r, c

        !Create new node with r,c = this.r,this.c
        type( PStackNode ), pointer :: new_node
        allocate(new_node)
        allocate(new_node%pos)
        call setPosition(new_node%pos, r, c)

        !If the head of the stack is not null
        !Set the new node next pointer to point to Stack heads
        if(associated(stack%head)) then
            new_node%next => stack%head
        end if

        !Set head pointer in Stack DS to new node 
        !   - always adding new node to the top of the Stack
        stack%head => new_node
        stack%size = stack%size + 1
    end subroutine push

    !Pop a Position (r,c) value pair off the stack
    subroutine pop(stack, r, c)
        type( PStack ), intent(inout) :: stack
        integer, intent(out) :: r, c !for returning the popped values

        if(associated(stack%head)) then 
            !Pop values off top of Stack
            r = stack%head%pos%r
            c = stack%head%pos%c

            !Move Stack head pointer to the next node in Stack
            stack%head = stack%head%next
            stack%size = stack%size - 1
        else
            r = -1
            c = -1
        end if
    end subroutine pop
end module PositionStacks
