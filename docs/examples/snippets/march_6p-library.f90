module io_library
!< A library that prints on its own: here, with print; in real life, an I/O or a solver library, the MPI runtime, ...
implicit none
contains
   subroutine save_solution(step)
   !< Save the solution of a step, and say so.
   integer, intent(in) :: step ! time step
   print '(A,I4.4,A)', 'io_library: writing solution_', step, '.h5'
   print '(A)', 'io_library: done, 2.1 MB'
   endsubroutine save_solution
endmodule io_library
