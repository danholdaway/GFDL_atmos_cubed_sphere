module mpi
    implicit none
    
    integer :: mpi_comm_world

contains

    subroutine mpi_barrier(comm, ierror)
        integer, intent(in) :: comm, ierror
        print *, comm
    end subroutine mpi_barrier
    
end module mpi