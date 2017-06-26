BEGIN_PROVIDER [ double precision, ao_ortho_mono_elec_integral_dressing, (ao_num_align,ao_num) ]
  implicit none
  BEGIN_DOC
  ! Dressing of the core hamiltonian in the orthogonal AO basis set
  END_DOC

  integer                        :: i,j,k
  integer                        :: mu, nu, lambda, A
  double precision               :: tmp

  ao_ortho_mono_elec_integral_dressing = 0.d0
  i = idx_dressing
  do mu=1,ao_num
    if (dabs(mo_coef_in_ao_ortho_basis(mu,i)) > 1.d-5) then
      do A=1,nucl_num
        tmp = 0.d0
        do nu=1,ao_num
          tmp += AO_orthoSlaOverlap_matrix(nu,A) * ao_ortho_mono_elec_integral(mu,nu)
        enddo
        ao_ortho_mono_elec_integral_dressing(mu,mu) += cusp_C(A,i) * (AO_orthoSlaH_matrix(mu,A) - tmp)
      enddo
      ao_ortho_mono_elec_integral_dressing(mu,mu) *= 1.d0/mo_coef_in_ao_ortho_basis(mu,i)
    endif
  enddo
END_PROVIDER


BEGIN_PROVIDER [ double precision, ao_ortho_mono_elec_integral, (ao_num_align, ao_num) ]
 implicit none
 BEGIN_DOC
 ! h core in orthogonal AO basis
 END_DOC
  double precision, allocatable ::  T(:,:)
  allocate(T(ao_num,ao_num))
  call dgemm('T','N',ao_num,ao_num,ao_num,1.d0,                    &
      ao_ortho_canonical_coef, size(ao_ortho_canonical_coef,1),      &
      ao_mono_elec_integral, size(ao_mono_elec_integral,1),          &
      0.d0, T, size(T,1))
  call dgemm('N','N',ao_num,ao_num,ao_num,1.d0,                    &
      T, size(T,1),                                                  &
      ao_ortho_canonical_coef, size(ao_ortho_canonical_coef,1),      &
      0.d0, ao_ortho_mono_elec_integral, size(ao_ortho_mono_elec_integral,1))
  deallocate(T)
END_PROVIDER


BEGIN_PROVIDER [ double precision, ao_mono_elec_integral_dressing, (ao_num,ao_num) ]
  implicit none
  BEGIN_DOC
  ! Dressing of the core hamiltonian in the AO basis set
  END_DOC
  call ao_ortho_cano_to_ao(ao_ortho_mono_elec_integral_dressing,size(ao_ortho_mono_elec_integral_dressing,1),&
    ao_mono_elec_integral_dressing,size(ao_mono_elec_integral_dressing,1))
END_PROVIDER

BEGIN_PROVIDER [ double precision, mo_mono_elec_integral_dressing, (mo_tot_num_align,mo_tot_num) ]
  implicit none
  BEGIN_DOC
  ! Dressing of the core hamiltonian in the MO basis set
  END_DOC

  call ao_to_mo(ao_mono_elec_integral_dressing,size(ao_mono_elec_integral_dressing,1),&
    mo_mono_elec_integral_dressing,size(mo_mono_elec_integral_dressing,1))
END_PROVIDER


BEGIN_PROVIDER [ integer, idx_dressing ]
 implicit none
 BEGIN_DOC
 ! Index of the MO which is being dressed
 END_DOC
 idx_dressing = 1
END_PROVIDER


BEGIN_PROVIDER [ double precision, cusp_corrected_mos, (ao_num_align,mo_tot_num) ]
  implicit none
  BEGIN_DOC
  ! Dressing core hamiltonian in the AO basis set
  END_DOC
  integer :: i,j
  double precision, allocatable :: F(:,:), M(:,:)
  allocate(F(mo_tot_num_align,mo_tot_num),M(ao_num,mo_tot_num))

  logical :: oneshot

!  oneshot = .True. 
  oneshot = .False.

  if (oneshot) then
    cusp_corrected_mos(1:ao_num,1:mo_tot_num) = mo_coef(1:ao_num,1:mo_tot_num)
    slater_coef(1:nucl_num,1:mo_tot_num) = cusp_C(1:nucl_num,1:mo_tot_num)
    return

  else


    do idx_dressing=1,mo_tot_num

      if (idx_dressing>1) then
        TOUCH idx_dressing
      endif

      do j=1,mo_tot_num
        do i=1,mo_tot_num
          F(i,j) = Fock_matrix_mo(i,j)
        enddo
      enddo

      do j=1,mo_tot_num
        do i=1,ao_num
          M(i,j) = mo_coef(i,j)
        enddo
      enddo

      integer :: it
      do it=1,128

  !      print *,  'X', ao_ortho_canonical_coef(1:ao_num,1:ao_num)
  !      print *,  'C', mo_coef(1:ao_num,1:mo_tot_num)
  !      print *,  'Cp', mo_coef_in_ao_ortho_basis(1:ao_num,1:mo_tot_num)
  !      print *,  'cAi', cusp_C(1:nucl_num,1:mo_tot_num)
  !      print *,  'FmuA', AO_orthoSlaH_matrix(1:ao_num,1:nucl_num)
  !      print *,  'Fock:', Fock_matrix_ao(1:ao_num,1:ao_num)
  !      print *,  'Diag Dressing:', ao_ortho_mono_elec_integral_dressing(1:ao_num,1:ao_num)
  !      print *,  'Dressing:', ao_mono_elec_integral_dressing(1:ao_num,1:ao_num)
  !      print *,  'Dressed Fock:', Fock_matrix_ao(1:ao_num,1:ao_num) + ao_mono_elec_integral_dressing(1:ao_num,1:ao_num)
  !      print *,  'AO_orthoSlaOverlap_matrix', AO_orthoSlaOverlap_matrix(1:ao_num,1:nucl_num)
  !      print *,  'AO_orthoSlaH_matrix', AO_orthoSlaH_matrix(1:ao_num,1:nucl_num)
  !      print *,  'ao_ortho_mono_elec_integral', ao_ortho_mono_elec_integral(1:ao_num,1:ao_num)
  !      print *,  'Fock MO:', Fock_matrix_mo(1:mo_tot_num,1:mo_tot_num) 
        do j=1,mo_tot_num
          do i=1,mo_tot_num
            Fock_matrix_mo(i,j) += mo_mono_elec_integral_dressing(i,j)
          enddo
        enddo
        do i=1,mo_tot_num
          Fock_matrix_diag_mo(i) = Fock_matrix_mo(i,i)
        enddo
  !      print *,  'Dressed Fock MO:', Fock_matrix_mo(1:mo_tot_num,1:mo_tot_num) 
        double precision :: conv
        conv = 0.d0
        do j=1,mo_tot_num
          do i=1,mo_tot_num
            if (i==j) cycle
            conv = max(conv,Fock_matrix_mo(i,j))
          enddo
        enddo
        TOUCH Fock_matrix_mo Fock_matrix_diag_mo

        mo_coef(1:ao_num,1:mo_tot_num) = eigenvectors_fock_matrix_mo(1:ao_num,1:mo_tot_num) 
        TOUCH mo_coef
  !print *,  'C', mo_coef(1:ao_num,1:mo_tot_num)
  !print *,  '-----'
        print *, idx_dressing, it, real(mo_coef(1,idx_dressing)), real(conv)
        if (conv < 1.d-5) exit
  !stop

      enddo
      cusp_corrected_mos(1:ao_num,idx_dressing) = mo_coef(1:ao_num,idx_dressing)
      slater_coef(1:nucl_num,idx_dressing) = cusp_C(1:nucl_num,idx_dressing)
    enddo

    idx_dressing = 1
    mo_coef(1:ao_num,1:mo_tot_num) = M(1:ao_num,1:mo_tot_num)
    soft_TOUCH mo_coef idx_dressing slater_coef

  endif

END_PROVIDER


