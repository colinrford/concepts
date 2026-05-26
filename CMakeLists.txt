
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

project(lam_concepts VERSION 0.1.260526 LANGUAGES CXX)

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

# --- lam.concepts:config generation ------------------------------------------
# git_hash refreshes whenever .git/logs/HEAD changes (any HEAD-affecting op).
# See symbols/CMakeLists.txt for the design rationale.
execute_process(
  COMMAND git -C "${CMAKE_CURRENT_SOURCE_DIR}" rev-parse --git-path logs/HEAD
  OUTPUT_VARIABLE _git_logs_head
  OUTPUT_STRIP_TRAILING_WHITESPACE
  ERROR_QUIET
)
get_filename_component(_git_logs_head "${_git_logs_head}"
  ABSOLUTE BASE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

if(CMAKE_BUILD_TYPE AND CMAKE_BUILD_TYPE MATCHES "^[Dd][Ee][Bb][Uu][Gg]$")
  set(IS_DEBUG "true")
else()
  set(IS_DEBUG "false")
endif()

set(_cfg_out ${CMAKE_CURRENT_BINARY_DIR}/concepts_config.cppm)
add_custom_command(
  OUTPUT  ${_cfg_out}
  COMMAND ${CMAKE_COMMAND}
          -DTEMPLATE=${CMAKE_CURRENT_SOURCE_DIR}/concepts_config.cppm.in
          -DOUTPUT=${_cfg_out}
          -DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}
          -DPROJECT_VERSION=${PROJECT_VERSION}
          -DPROJECT_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}
          -DPROJECT_VERSION_MINOR=${PROJECT_VERSION_MINOR}
          -DPROJECT_VERSION_PATCH=${PROJECT_VERSION_PATCH}
          -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
          -DIS_DEBUG=${IS_DEBUG}
          -DCMAKE_CXX_COMPILER_ID=${CMAKE_CXX_COMPILER_ID}
          -DCMAKE_CXX_COMPILER_VERSION=${CMAKE_CXX_COMPILER_VERSION}
          -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
          -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME}
          -DCMAKE_SYSTEM_PROCESSOR=${CMAKE_SYSTEM_PROCESSOR}
          -DCMAKE_GENERATOR=${CMAKE_GENERATOR}
          -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}
          "-DCMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS}"
          -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/GenerateLamConfig.cmake
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/concepts_config.cppm.in
          ${_git_logs_head}
  VERBATIM
)

add_library(concepts STATIC)
add_library(lam_concepts::concepts ALIAS concepts)
add_library(lam::concepts ALIAS concepts)
set_target_properties(concepts PROPERTIES OUTPUT_NAME lam_concepts)
target_sources(concepts PUBLIC
  FILE_SET CXX_MODULES
  TYPE CXX_MODULES
  BASE_DIRS src/experimental ${CMAKE_CURRENT_BINARY_DIR}
  FILES src/experimental/concepts.cppm
        ${CMAKE_CURRENT_BINARY_DIR}/concepts_config.cppm
)
target_compile_features(concepts PUBLIC cxx_std_23)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# On macOS with -DCMAKE_INSTALL_PREFIX=/usr/local:
#   Libraries     -> /usr/local/lib/liblam_concepts.a
#   CMake config  -> /usr/local/lib/cmake/lam_concepts/
#   C++ modules   -> /usr/local/include/lam_concepts/
#   CPS metadata  -> /usr/local/lib/cps/

install(TARGETS concepts
  EXPORT lam_conceptsTargets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  FILE_SET CXX_MODULES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}
)

install(EXPORT lam_conceptsTargets
  FILE lam_conceptsTargets.cmake
  NAMESPACE lam_concepts::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/lam_concepts
)

# --- CPS (Common Package Specification) export ------------------------------
# Emits lam_concepts.cps alongside the legacy lam_conceptsConfig.cmake.
# Guarded so older CMake (< 4.3, which lacks install(PACKAGE_INFO)) still configures.
if(CMAKE_VERSION VERSION_GREATER_EQUAL 4.3)
  install(PACKAGE_INFO lam_concepts
    EXPORT       lam_conceptsTargets
    VERSION      ${PROJECT_VERSION}
    COMPAT_VERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.0
    DESCRIPTION  "lam.concepts — algebraic, numeric, and categorical C++23 concepts"
    LICENSE      "Apache-2.0 WITH LLVM-exception OR MIT OR BSL-1.0"
    CXX_MODULES_DIRECTORY "include/${PROJECT_NAME}"
    DESTINATION  ${CMAKE_INSTALL_LIBDIR}/cps
  )
endif()



file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/lam_conceptsConfig.cmake" [[
include(CMakeFindDependencyMacro)
include("${CMAKE_CURRENT_LIST_DIR}/lam_conceptsTargets.cmake")
# Create the legacy alias so users can still type lam::concepts
if(TARGET lam_concepts::concepts AND NOT TARGET lam::concepts)
  get_target_property(_aliased lam_concepts::concepts ALIASED_TARGET)
  if(_aliased)
    add_library(lam::concepts ALIAS ${_aliased})
  else()
    add_library(lam::concepts ALIAS lam_concepts::concepts)
  endif()
  unset(_aliased)
endif()
]])

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/lam_conceptsConfigVersion.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion
)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/lam_conceptsConfig.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/lam_conceptsConfigVersion.cmake"
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/lam_concepts
)
