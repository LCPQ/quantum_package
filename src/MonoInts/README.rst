Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `MOs <http://github.com/LCPQ/quantum_package/tree/master/src/MOs>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`ao_mono_elec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/ao_mono_ints.irp.f#L/BEGIN_PROVIDER [ double precision, ao_mono_elec_integral,(ao_num_align,ao_num)]/;">`_
  array of the mono electronic hamiltonian on the AOs basis
  : sum of the kinetic and nuclear electronic potential

`ao_overlap <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/ao_mono_ints.irp.f#L/BEGIN_PROVIDER [ double precision, ao_overlap,(ao_num_align,ao_num) ]/;">`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`

`ao_overlap_abs <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/ao_mono_ints.irp.f#L/BEGIN_PROVIDER [ double precision, ao_overlap_abs,(ao_num_align,ao_num) ]/;">`_
  Overlap between absolute value of atomic basis functions:
  :math:`\int |\chi_i(r)| |\chi_j(r)| dr)`

`ao_overlap_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/ao_mono_ints.irp.f#L/&BEGIN_PROVIDER [ double precision, ao_overlap_x,(ao_num_align,ao_num) ]/;">`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`

`ao_overlap_y <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/ao_mono_ints.irp.f#L/&BEGIN_PROVIDER [ double precision, ao_overlap_y,(ao_num_align,ao_num) ]/;">`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`

`ao_overlap_z <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/ao_mono_ints.irp.f#L/&BEGIN_PROVIDER [ double precision, ao_overlap_z,(ao_num_align,ao_num) ]/;">`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`

`check_ortho <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/check_orthonormality.irp.f#L/subroutine check_ortho/;">`_
  Undocumented

`do_print <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/check_orthonormality.irp.f#L/subroutine do_print/;">`_
  Undocumented

`n_pt_max_i_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/dimensions.irp.f#L/&BEGIN_PROVIDER [ integer, n_pt_max_i_x]/;">`_
  Undocumented

`n_pt_max_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/dimensions.irp.f#L/BEGIN_PROVIDER [ integer, n_pt_max_integrals ]/;">`_
  Undocumented

`ao_deriv2_x <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/kin_ao_ints.irp.f#L/BEGIN_PROVIDER [ double precision, ao_deriv2_x,(ao_num_align,ao_num) ]/;">`_
  second derivatives matrix elements in the ao basis
  .. math::
  .br
  {\tt ao_deriv2_x} = \langle \chi_i(x,y,z) \frac{\partial^2}{\partial x^2} |\chi_j (x,y,z) \rangle

`ao_deriv2_y <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/kin_ao_ints.irp.f#L/&BEGIN_PROVIDER [ double precision, ao_deriv2_y,(ao_num_align,ao_num) ]/;">`_
  second derivatives matrix elements in the ao basis
  .. math::
  .br
  {\tt ao_deriv2_x} = \langle \chi_i(x,y,z) \frac{\partial^2}{\partial x^2} |\chi_j (x,y,z) \rangle

`ao_deriv2_z <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/kin_ao_ints.irp.f#L/&BEGIN_PROVIDER [ double precision, ao_deriv2_z,(ao_num_align,ao_num) ]/;">`_
  second derivatives matrix elements in the ao basis
  .. math::
  .br
  {\tt ao_deriv2_x} = \langle \chi_i(x,y,z) \frac{\partial^2}{\partial x^2} |\chi_j (x,y,z) \rangle

`ao_kinetic_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/kin_ao_ints.irp.f#L/BEGIN_PROVIDER [double precision, ao_kinetic_integral, (ao_num_align,ao_num)]/;">`_
  array of the priminitve basis kinetic integrals
  \langle \chi_i |\hat{T}| \chi_j \rangle

`mo_kinetic_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/kin_mo_ints.irp.f#L/BEGIN_PROVIDER [double precision, mo_kinetic_integral, (mo_tot_num_align,mo_tot_num)]/;">`_
  Undocumented

`mo_mono_elec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/mo_mono_ints.irp.f#L/BEGIN_PROVIDER [ double precision, mo_mono_elec_integral,(mo_tot_num_align,mo_tot_num)]/;">`_
  array of the mono electronic hamiltonian on the MOs basis
  : sum of the kinetic and nuclear electronic potential

`mo_overlap <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/mo_mono_ints.irp.f#L/BEGIN_PROVIDER [ double precision, mo_overlap,(mo_tot_num_align,mo_tot_num)]/;">`_
  Undocumented

`orthonormalize_mos <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/orthonormalize.irp.f#L/subroutine orthonormalize_mos/;">`_
  Undocumented

`ao_nucl_elec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/BEGIN_PROVIDER [ double precision, ao_nucl_elec_integral, (ao_num_align,ao_num)]/;">`_
  interaction nuclear electron

`give_polynom_mult_center_mono_elec <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/subroutine give_polynom_mult_center_mono_elec(A_center,B_center,alpha,beta,power_A,power_B,C_center,n_pt_in,d,n_pt_out)/;">`_
  Undocumented

`i_x1_pol_mult_mono_elec <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/recursive subroutine I_x1_pol_mult_mono_elec(a,c,R1x,R1xp,R2x,d,nd,n_pt_in)/;">`_
  Undocumented

`i_x2_pol_mult_mono_elec <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/recursive subroutine I_x2_pol_mult_mono_elec(c,R1x,R1xp,R2x,d,nd,dim)/;">`_
  Undocumented

`int_gaus_pol <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/double precision function int_gaus_pol(alpha,n)/;">`_
  Undocumented

`nai_pol_mult <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/double precision function NAI_pol_mult(A_center,B_center,power_A,power_B,alpha,beta,C_center,n_pt_in)/;">`_
  Undocumented

`v_e_n <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/double precision function V_e_n(a_x,a_y,a_z,b_x,b_y,b_z,alpha,beta)/;">`_
  Undocumented

`v_phi <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/double precision function V_phi(n,m)/;">`_
  Undocumented

`v_r <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/double precision function V_r(n,alpha)/;">`_
  Undocumented

`v_theta <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/double precision function V_theta(n,m)/;">`_
  Undocumented

`wallis <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_ao_ints.irp.f#L/double precision function Wallis(n)/;">`_
  Undocumented

`mo_nucl_elec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/pot_mo_ints.irp.f#L/BEGIN_PROVIDER [double precision, mo_nucl_elec_integral, (mo_tot_num_align,mo_tot_num)]/;">`_
  Undocumented

`save_ortho_mos <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts/save_ortho_mos.irp.f#L/subroutine save_ortho_mos/;">`_
  Undocumented



