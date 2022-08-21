cmake_minimum_required(VERSION 3.12)

project(selfc CXX C)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(COMPILER_SRC ${CMAKE_SOURCE_DIR}/src)
set(STDLIB_SRC ${CMAKE_SOURCE_DIR}/stdlib)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR})

include(FetchContent)
include(CheckCXXCompilerFlag)

FetchContent_Declare(json
URL https://github.com/nlohmann/json/releases/download/v3.10.5/json.tar.xz)
FetchContent_Declare(argparse
GIT_REPOSITORY https://github.com/p-ranav/argparse.git
GIT_TAG 878bd5edac1a2483e8fcdbb3c7db6e4b5ebc54eb)
FetchContent_Declare(re2
GIT_REPOSITORY https://github.com/google/re2.git
GIT_TAG ba2e7c2f25047a6848f2baa9bc4cb700ea9dda84)
FetchContent_Declare(fmt
GIT_REPOSITORY https://github.com/fmtlib/fmt.git
GIT_TAG c48be439f1ae03f2726e30ac93fce3a667dc4be2)
FetchContent_MakeAvailable(json argparse re2 fmt)

if(NOT TARGET re2::re2)
  message(FATAL_ERROR "re2 has failed to build")
endif()
if(NOT TARGET argparse::argparse)
  message(FATAL_ERROR "argparse has failed to build")
endif()
if(NOT TARGET fmt::fmt)
  message(FATAL_ERROR "fmt has failed to build")
endif()

add_subdirectory(src)
add_subdirectory(stdlib)
add_subdirectory(tests)