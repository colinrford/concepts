
cmake_minimum_required(VERSION 3.31.6 FATAL_ERROR)

# C++23 import-std UUID ladder + standard settings (must precede project())
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/LamCxxModuleStd.cmake)

file(READ "${CMAKE_CURRENT_SOURCE_DIR}/VERSION" _lam_concepts_version)
string(STRIP "${_lam_concepts_version}" _lam_concepts_version)

project(lam_concepts VERSION ${_lam_concepts_version} LANGUAGES CXX)

# Re-run CMake configure whenever VERSION changes.
set_property(DIRECTORY APPEND PROPERTY
  CMAKE_CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/VERSION")

# Only vcpp-consumed lam modules (concepts, symbols, linearalgebra) carry
# this helper; must run after project() since it creates a target.
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/LamEmscriptenStd.cmake)
lam_setup_emscripten_std()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# lam build helpers (lam_add_config_partition, lam_install_module_package).
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/LamModule.cmake)

add_library(concepts STATIC)
add_library(lam_concepts::concepts ALIAS concepts)
add_library(lam::concepts ALIAS concepts)
set_target_properties(concepts PROPERTIES CXX_MODULE_STD ON OUTPUT_NAME lam_concepts)
target_sources(concepts PUBLIC
  FILE_SET CXX_MODULES
  TYPE CXX_MODULES
  BASE_DIRS src/experimental
  FILES src/experimental/concepts.cppm
)
target_compile_features(concepts PUBLIC cxx_std_23)

# Generate + attach the lam.concepts:config partition (build-time git_hash refresh)
lam_add_config_partition(concepts NAME concepts)

# config_dump smoke test + build-config diagnostic (imports lam.concepts)
enable_testing()
lam_add_config_dump(concepts NAME concepts)

# On macOS with -DCMAKE_INSTALL_PREFIX=/usr/local:
#   Libraries     -> /usr/local/lib/liblam_concepts.a
#   CMake config  -> /usr/local/lib/cmake/lam_concepts/
#   C++ modules   -> /usr/local/include/lam/concepts/
#   CPS metadata  -> /usr/local/lib/cps/
lam_install_module_package(concepts
  NAME        concepts
  DESCRIPTION "lam.concepts — algebraic, numeric, and categorical C++ concepts"
  LICENSE     "Apache-2.0 WITH LLVM-exception OR MIT OR BSL-1.0"
)
