
cmake_minimum_required(VERSION 3.31.6 FATAL_ERROR)

if(CMAKE_VERSION VERSION_GREATER_EQUAL 4.3.0)
  set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD "451f2fe2-a8a2-47c3-bc32-94786d8fc91b")
elseif(CMAKE_VERSION VERSION_GREATER_EQUAL 4.0.3)
  set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD "d0edc3af-4c50-42ea-a356-e2862fe7a444")
elseif(CMAKE_VERSION VERSION_GREATER_EQUAL 4.0.0)
  set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD "a9e1cf81-9932-4810-974b-6eccaf14e457")
elseif(CMAKE_VERSION VERSION_GREATER_EQUAL 3.31.6)
  set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD "0e5b6991-d74f-4b3d-a41c-cf096e0b2508")
else()
  message(FATAL_ERROR "Version lower than 3.31.6 not supported (by me)")
endif()

set(CMAKE_CXX_EXTENSIONS OFF CACHE BOOL "Use compiler extensions" FORCE)
set(CMAKE_CXX_STANDARD_REQUIRED True) #is this needed?
set(CMAKE_CXX_MODULE_STD ON)
set(CMAKE_CXX_SCAN_FOR_MODULES ON)

file(READ "${CMAKE_CURRENT_SOURCE_DIR}/VERSION" _lam_concepts_version)
string(STRIP "${_lam_concepts_version}" _lam_concepts_version)

project(lam_concepts VERSION ${_lam_concepts_version} LANGUAGES CXX)

# Re-run CMake configure whenever VERSION changes.
set_property(DIRECTORY APPEND PROPERTY
  CMAKE_CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/VERSION")

# Emscripten needs manual __CMAKE::CXX23 target creation since CMake's
# detection doesn't find the std modules in Emscripten's toolchain
if(EMSCRIPTEN AND NOT TARGET "__CMAKE::CXX23")
  set(_emscripten_std_modules "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/emscripten-std-modules")
  set(_emscripten_libcxx_headers "/usr/local/Cellar/emscripten/5.0.0/libexec/cache/sysroot/include/c++/v1")
  set(_emscripten_sysroot "/usr/local/Cellar/emscripten/5.0.0/libexec/cache/sysroot/include")
  if(EXISTS "${_emscripten_std_modules}/std.cppm")
    add_library(__cmake_cxx23 STATIC)
    target_sources(__cmake_cxx23 INTERFACE "$<$<STREQUAL:$<TARGET_PROPERTY:TYPE>,STATIC_LIBRARY>:$<TARGET_OBJECTS:__cmake_cxx23>>")
    set_property(TARGET __cmake_cxx23 PROPERTY EXCLUDE_FROM_ALL 1)
    set_property(TARGET __cmake_cxx23 PROPERTY CXX_SCAN_FOR_MODULES 1)
    set_property(TARGET __cmake_cxx23 PROPERTY CXX_MODULE_STD 0)
    target_compile_features(__cmake_cxx23 PUBLIC cxx_std_23)
    target_compile_options(__cmake_cxx23 PRIVATE -Wno-reserved-module-identifier)
    target_include_directories(__cmake_cxx23 PRIVATE
      "${_emscripten_libcxx_headers}"
      "${_emscripten_sysroot}")
    target_sources(__cmake_cxx23
      PUBLIC
      FILE_SET std TYPE CXX_MODULES
        BASE_DIRS "${_emscripten_std_modules}"
        FILES "${_emscripten_std_modules}/std.cppm" "${_emscripten_std_modules}/std.compat.cppm")
    add_library(__CMAKE::CXX23 ALIAS __cmake_cxx23)
    list(APPEND CMAKE_CXX_COMPILER_IMPORT_STD "23")
    message(STATUS "Manually created __CMAKE::CXX23 target for Emscripten")
  else()
    message(WARNING "Emscripten std.cppm not found at ${_emscripten_std_modules}")
  endif()
endif()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Vendored lam build helpers (lam_add_config_partition, lam_install_module_package).
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

# Generate + attach the lam.concepts:config partition (build-time git_hash refresh).
lam_add_config_partition(concepts NAME concepts)

# config_dump smoke test + build-config diagnostic (imports lam.concepts).
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
