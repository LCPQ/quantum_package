/*
 *  This file is a part of Libint.
 *  Copyright (C) 2004-2014 Edward F. Valeev
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see http://www.gnu.org/licenses/.
 *
 */

#if __cplusplus <= 199711L
# error "Hartree-Fock test requires C++11 support"
#endif

// standard C++ headers
#include <cmath>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <vector>
#include <chrono>
#include <stdlib.h>

// Libint Gaussian integrals library
#include <libint2.hpp>

/*** ================ ***/
/*** Exposed Function ***/
/*** ================ ***/
extern "C" 
{
  void init_libint(char ezfio_filename[]);
  void finalize_libint();
  int nb_shell();
  void map_shell_to_basis_function_interval(int sze, int* out_val);

  double ao_bielec_integral(int bf1f, int bf2f, int bf3f, int bf4f);
  void compute_ao_bielec_integrals_jkl(int bf2, int bf3, int bf4, int sze, double* values);
  void compute_ao_bielec_integrals_shell(int s1, int s2, int s3, int s4, int sze, double* values);
}

using libint2::Shell;

/*** ================= ***/
/*** Internal Function ***/
/*** ================= ***/

size_t nbasis(const std::vector<Shell>& shells);
std::vector<size_t> map_shell_to_basis_function(const std::vector<Shell>& shells);
std::vector<size_t> map_basis_function_to_shell(const std::vector<Shell>& shells);
 
/*** ================ ***/
/*** Exposed Function ***/
/*** ================ ***/

void init_libint(char ezfio_filename[]);
void finalize_libint();
int nb_shell();
void map_shell_to_basis_function_interval(int sze, int* out_val);


double ao_bielec_integral(int bf1f, int bf2f, int bf3f, int bf4f);
void compute_ao_bielec_integrals_jkl(int i, int j, int k, int sze, double* values);
void compute_ao_bielec_integrals_shell(int s1, int s2, int s3, int s4, int sze, double* values);

/*** =============== ***/
/*** Global Variable ***/
/*** =============== ***/

std::vector<Shell> shells_global;
std::vector<size_t> shell2bf;
std::vector<size_t> bf2shell;
static libint2::TwoBodyEngine<libint2::Coulomb> *engine_pointer;

// ___                             _                         
//  |  ._ _|_  _  ._ ._   _. |   _|_    ._   _ _|_ o  _  ._  
// _|_ | | |_ (/_ |  | | (_| |    | |_| | | (_  |_ | (_) | | 
//                                                           

size_t nbasis(const std::vector<Shell>& shells) {
  size_t n = 0;
  for (const auto& shell: shells)
    n += shell.size();
  return n;
}

size_t max_nprim(const std::vector<Shell>& shells) {
  size_t n = 0;
  for (auto shell: shells)
    n = std::max(shell.nprim(), n);
  return n;
}

int max_l(const std::vector<Shell>& shells) {
  int l = 0;
  for (auto shell: shells)
    for (auto c: shell.contr)
      l = std::max(c.l, l);
  return l;
}


std::vector<size_t> map_shell_to_basis_function(const std::vector<Shell>& shells) {
  std::vector<size_t> result;
  result.reserve(shells.size());

  size_t n = 0;
  for (auto shell: shells) {
    result.push_back(n);
    n += shell.size();
  }

  return result;
}

std::vector<size_t> map_basis_function_to_shell(const std::vector<Shell>& shells) {

  std::vector<size_t> result;
  result.reserve(nbasis(shells));

  size_t n = 0;

  for (auto shell: shells) {
    for (auto i=0; i!=shell.size(); ++i){
      result.push_back(n);
    }
    n++;
  }

  return result;
}

//  _                           _                         
// |_    ._   _   _  _   _|   _|_    ._   _ _|_ o  _  ._  
// |_ >< |_) (_) _> (/_ (_|    | |_| | | (_  |_ | (_) | | 
//       |                                                
void init_libint(char ezfio_filename[]){

    /*** =========================== ***/
    /*** initialize molecule         ***/
    /*** =========================== ***/

    std::string xyz_path = ezfio_filename + std::string("/libint/xyz");
    // read geometry from a filename
    std::ifstream input_file(xyz_path);
    std::vector<libint2::Atom> atoms = libint2::read_dotxyz(input_file);

    /*** =========================== ***/
    /*** create basis set            ***/
    /*** =========================== ***/

    std::string basis_path = ezfio_filename + std::string("/libint");
    setenv("LIBINT_DATA_PATH", basis_path.c_str(), 1);

    libint2::BasisSet shells("basis", atoms);

    shells_global = shells;

    for(auto& shell: shells_global)
      for(auto& contraction: shell.contr)
            contraction.pure = false;

  // initializes the Libint integrals library ... now ready to compute
  libint2::init();

  // construct the electron repulsion integrals engine
  engine_pointer = new libint2::TwoBodyEngine<libint2::Coulomb> (max_nprim(shells_global), max_l(shells_global), 0);

  shell2bf = map_shell_to_basis_function(shells_global);
  bf2shell = map_basis_function_to_shell(shells_global);

}

void finalize_libint(){
  libint2::finalize(); // done with libint2
}

int nb_shell(){
  return shells_global.size();
}

void map_shell_to_basis_function_interval(int sze, int* out_val) {
  size_t n = 1;
  for (auto i=0; i<shells_global.size() ; i++) {

    out_val[2*i] = n;
    n += shells_global[i].size();
    out_val[2*i+1] = n-1;
  }
}

double ao_bielec_integral(int bf1f, int bf2f, int bf3f, int bf4f){

  auto bf1 = bf1f-1;
  auto bf2 = bf2f-1;
  auto bf3 = bf3f-1;
  auto bf4 = bf4f-1;

  // construct the electron repulsion integrals engine
  libint2::TwoBodyEngine<libint2::Coulomb> &engine = *engine_pointer;

  auto s1 = bf2shell[bf1];
  auto n1 = shells_global[s1].size();
  auto f1 = bf1-shell2bf[s1];

  auto s2 = bf2shell[bf2];
  auto n2 = shells_global[s2].size();
  auto f2 = bf2-shell2bf[s2];

  auto s3 = bf2shell[bf3];
  auto n3 = shells_global[s3].size();
  auto f3 = bf3-shell2bf[s3];;

  auto s4 = bf2shell[bf4];
  auto n4 = shells_global[s4].size();
  auto f4 = bf4- shell2bf[s4];

 // std::cout << "o: compute shell set {" << s1 << "," << s2 <<"," << s3 <<"," << s4 << "} ... ";
  const auto* buf_1234 = engine.compute(shells_global[s1], shells_global[s2], shells_global[s3], shells_global[s4]);
//  std::cout << "done" << std::endl;

  auto f1234 = f1*n2*n3*n4+f2*n3*n4+f3*n4+f4;
  auto result = buf_1234[f1234]; 
  
  return result;

};


void compute_ao_bielec_integrals_shell(int s1, int s2, int s3, int s4, int sze, double* values){
  libint2::TwoBodyEngine<libint2::Coulomb> &engine = *engine_pointer;

  const auto*  buf_1234 = engine.compute(shells_global[s1-1], shells_global[s2-1], shells_global[s3-1], shells_global[s4-1]);

  for(auto i=0; i!=sze; i++)
    values[i] = buf_1234[i];
  };
