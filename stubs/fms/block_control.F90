
module block_control_mod
    implicit none
    
    public block_control_type
    
    !> Type to dereference packed index from global index.
    !> @ingroup block_control_mod
    type :: ix_type
      integer, dimension(:,:), allocatable :: ix
    end type ix_type
    
    !> Type to dereference packed index from global indices.
    !> @ingroup block_control_mod
    type :: pk_type
      integer, dimension(:), allocatable :: ii
      integer, dimension(:), allocatable :: jj
    end type pk_type
    
    !> @brief Block data and extents for OpenMP threading of column-based calculations
    !> @ingroup block_control_mod
    type :: block_control_type
      integer :: nx_block, ny_block  !< blocking factor using mpp-style decomposition
      integer :: nblks               !< number of blocks cover MPI domain
      integer :: isc, iec, jsc, jec  !< MPI domain global extents
      integer :: npz                 !< vertical extent
      integer, dimension(:),        allocatable :: ibs  , &  !< block extents for mpp-style
                                                   ibe  , &  !! decompositions
                                                   jbs  , &
                                                   jbe
      type(ix_type), dimension(:),  allocatable :: ix    !< dereference packed index from global index
      !--- packed blocking fields
      integer, dimension(:),        allocatable :: blksz !< number of points in each individual block
                                                                !! blocks are not required to be uniforom in size
      integer, dimension(:,:),      allocatable :: blkno !< dereference block number using global indices
      integer, dimension(:,:),      allocatable :: ixp   !< dereference packed index from global indices
                                                                !! must be used in conjuction with blkno
      type(pk_type), dimension(:),  allocatable :: index !< dereference global indices from
                                                                !! block/ixp combo
    end type block_control_type
    
    end module block_control_mod