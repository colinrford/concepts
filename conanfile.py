# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception OR MIT OR BSL-1.0
# SPDX-FileCopyrightText: 2025-2026 Colin Ford
#
# Conan 2.x recipe for lam.concepts.
#
# Local development:
#   conan create . --profile <your-profile>
#
# Consumer projects can then declare:
#   requires = "lam_concepts/0.1.260526"
#
# This recipe is supplementary to the project's primary CMake+CPS distribution
# path — see CMakeLists.txt and the install(PACKAGE_INFO) block for the
# canonical install layout. The recipe delegates entirely to CMake.

from conan import ConanFile
from conan.tools.cmake import CMake, CMakeToolchain, CMakeDeps, cmake_layout
from conan.tools.files import copy
import os

class LamConceptsConan(ConanFile):
    name = "lam_concepts"
    version = "0.1.260526"
    license = "Apache-2.0 WITH LLVM-exception OR MIT OR BSL-1.0"
    author = "Colin Ford"
    url = "https://github.com/colinrford/concepts"
    description = (
        "Algebraic, numeric, and categorical C++23 concepts for the lam project."
    )
    topics = ("c++23", "modules", "concepts", "algebra", "lam")

    settings = "os", "compiler", "build_type", "arch"
    package_type = "static-library"

    # No external Conan deps for lam.concepts; it sits at the bottom of the
    # lam dependency tree.

    exports_sources = (
        "CMakeLists.txt",
        "concepts_config.cppm.in",
        "src/*",
        "cmake/*",
        "LICENSE",
        "README.md",
    )

    def layout(self):
        cmake_layout(self)

    def validate(self):
        cppstd = self.settings.compiler.cppstd
        if cppstd is not None:
            std = int(str(cppstd).replace("gnu", ""))
            if std < 23:
                raise Exception(
                    "lam_concepts requires C++23 (compiler.cppstd >= 23)."
                )

    def generate(self):
        tc = CMakeToolchain(self)
        # Mirror what the project's CMakeLists already assumes.
        tc.cache_variables["CMAKE_CXX_STANDARD"] = "23"
        tc.cache_variables["CMAKE_CXX_SCAN_FOR_MODULES"] = "ON"
        tc.generate()
        deps = CMakeDeps(self)
        deps.generate()

    def build(self):
        cmake = CMake(self)
        cmake.configure()
        cmake.build()

    def package(self):
        cmake = CMake(self)
        cmake.install()
        copy(
            self,
            "LICENSE",
            src=self.source_folder,
            dst=os.path.join(self.package_folder, "licenses"),
        )

    def package_info(self):
        # Match the CMake export names so find_package(lam_concepts) and the
        # Conan-generated CMakeDeps both yield the same lam::concepts target.
        self.cpp_info.set_property("cmake_file_name", "lam_concepts")
        self.cpp_info.set_property("cmake_target_name", "lam::concepts")
        self.cpp_info.libs = ["lam_concepts"]
        # CPS metadata lives under <pkg>/lib/cps for CMake 4.3+ consumers that
        # read CPS in addition to the legacy *Config.cmake.
        self.cpp_info.builddirs = [
            os.path.join("lib", "cmake", "lam_concepts"),
            os.path.join("lib", "cps"),
        ]
