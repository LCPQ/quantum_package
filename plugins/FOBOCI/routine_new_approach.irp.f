subroutine update_dressing_matrix(dressing_matrix_1h1p,dressing_matrix_2h1p,dressing_restart_good_det,dressing_matrix_restart_1h1p, &
                                  dressing_matrix_restart_2h1p,dressing_diag_good_det,psi_good_det,n_good_det,n_max_good_det)
 implicit none
 integer, intent(in) :: n_max_good_det
 integer, intent(inout) :: n_good_det
 integer(bit_kind), intent(inout) :: psi_good_det(N_int,2,n_max_good_det)
 double precision, intent(in) :: dressing_matrix_1h1p(N_det_generators,N_det_generators)
 double precision, intent(in) :: dressing_matrix_2h1p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_restart_1h1p(N_det_generators_restart, N_det_generators_restart)
 double precision, intent(inout) :: dressing_matrix_restart_2h1p(N_det_generators_restart, N_det_generators_restart)
 double precision, intent(inout) :: dressing_restart_good_det(n_max_good_det,N_det_generators_restart)
 double precision, intent(inout) :: dressing_diag_good_det(n_max_good_det)
  use bitmasks
 integer :: k,l,degree
 logical, allocatable :: is_a_ref_det(:)
 integer, allocatable :: index_restart_generators(:)
 allocate(is_a_ref_det(N_det_generators),index_restart_generators(N_det_generators))
 is_a_ref_det = .False.
  do k = 1, N_det_generators
   do l = 1, N_det_generators_restart
    call get_excitation_degree(psi_det_generators(1,1,k),psi_det_generators_restart(1,1,l), degree, N_int)
    if(degree==0)then
     is_a_ref_det(k) = .True.
     index_restart_generators(k) = l
    endif
   enddo
  enddo
  do k = 1, N_det_generators
   if(is_a_ref_det(k))then
    do l = 1, N_det_generators 
     if(.not.is_a_ref_det(l))cycle
      !!!! Dressing of the reference space in the order of the restart determinants
      dressing_matrix_restart_1h1p(index_restart_generators(l),index_restart_generators(k)) += dressing_matrix_1h1p(k,l) 
      print*,' dressing_matrix_1h1p(k,l) = ',dressing_matrix_1h1p(k,l)
      dressing_matrix_restart_2h1p(index_restart_generators(l),index_restart_generators(k)) += dressing_matrix_2h1p(k,l) 
    enddo
   else
      !!!! Incrementing the counting of the good determinants
    n_good_det +=1
      !!!! Adding the good determinant to the global_list (psi_good_det)
    do l = 1, N_int
     psi_good_det(l,1,n_good_det) = psi_det_generators(l,1,k)
     psi_good_det(l,2,n_good_det) = psi_det_generators(l,2,k)
    enddo
      !!! Storing the diagonal dressing of the good det
    dressing_diag_good_det(n_good_det) = dressing_matrix_1h1p(k,k) + dressing_matrix_2h1p(k,k)
    do l = 1, N_det_generators 
     if(.not.is_a_ref_det(l))cycle
      !!! Storing the extra diagonal dressing of the good det with the restart determinants
      dressing_restart_good_det(n_good_det,index_restart_generators(l)) = dressing_matrix_1h1p(k,l) +  dressing_matrix_2h1p(k,l)
    enddo
   endif
  enddo
 deallocate(is_a_ref_det,index_restart_generators)

end
