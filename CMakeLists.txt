cmake_minimum_required(VERSION 3.12)

project(selfc CXX C)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(COMPILER_SRC ${CMAKE_SOURCE_DIR}/src)
set(STDLIB_SRC ${CMAKE_SOURCE_DIR}/stdlib)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR})

include(FetchContent)
set(JSON_BuildTests OFF CACHE INTERNAL "")

FetchContent_Declare(json
URL https://github.com/nlohmann/json/releases/download/v3.10.5/json.tar.xz)
FetchContent_Declare(argh
GIT_REPOSITORY https://github.com/adishavit/argh.git
GIT_TAG 39b9a00046143f6b300dcf98ea989486cf62097d)
FetchContent_Declare(re2
GIT_REPOSITORY https://github.com/google/re2.git
GIT_TAG ba2e7c2f25047a6848f2baa9bc4cb700ea9dda84)
FetchContent_Declare(fmt
GIT_REPOSITORY https://github.com/fmtlib/fmt.git
GIT_TAG c48be439f1ae03f2726e30ac93fce3a667dc4be2)
FetchContent_MakeAvailable(json argh re2 fmt)

if(NOT TARGET re2::re2)
  message(FATAL_ERROR "re2 has failed to build")
endif()
if(NOT TARGET argh)
  message(FATAL_ERROR "argh has failed to build")
endif()
if(NOT TARGET fmt::fmt)
  message(FATAL_ERROR "fmt has failed to build")
endif()

include(CheckCXXCompilerFlag)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# this should be done by llvm_map_components, but it doesn't work
# for some reason. I'll have to investigate that later.
execute_process(
  COMMAND llvm-config --ignore-libllvm --libs all-targets 
  OUTPUT_VARIABLE TARGET_INFO
  OUTPUT_STRIP_TRAILING_WHITESPACE)
llvm_map_components_to_libnames(llvm_libs support core target codegen)

add_library(selfast ${COMPILER_SRC}/ast.cpp ${COMPILER_SRC}/complete_types.cpp)
target_include_directories(selfast PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfast PROPERTY CXX_STANDARD 20)

add_library(selfffi ${COMPILER_SRC}/ast.cpp ${COMPILER_SRC}/ffi_parse.cpp)
target_include_directories(selfffi PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfffi PROPERTY CXX_STANDARD 20)
target_link_libraries(selfffi PRIVATE clang)

add_library(selfparse ${COMPILER_SRC}/lexer.cpp ${COMPILER_SRC}/fsa_parser.cpp)
target_include_directories(selfparse PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfparse PROPERTY CXX_STANDARD 20)
target_link_libraries(selfparse PRIVATE re2::re2 selfast selfffi)
# target_compile_options(selfparse PUBLIC -fsanitize=address)
# target_link_options(selfparse PUBLIC -fsanitize=address)
# For now I haven't been having mem leak issues, and the
# indirect leak warnings from llvm have been pretty annoying.
add_library(selfgen ${COMPILER_SRC}/codegen.cpp ${COMPILER_SRC}/backend_config.cpp)
target_include_directories(selfgen PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfgen PROPERTY CXX_STANDARD 20)
target_link_libraries(selfgen PRIVATE ${llvm_libs} ${TARGET_INFO} selfast)
# target_compile_options(selfgen PUBLIC -fsanitize=address)
# target_link_options(selfgen PUBLIC -fsanitize=address)


add_library(selfstd ${STDLIB_SRC}/io.c)
target_include_directories(selfstd PUBLIC ${STDLIB_SRC}/include)

if(MSVC)
  target_compile_options(selfparse PRIVATE /W4 /WX)
else()
  target_compile_options(selfparse PRIVATE -Wall -Wextra -pedantic)
endif()

add_executable(selfc ${COMPILER_SRC}/selfc.cpp)
target_include_directories(selfc PRIVATE ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfc PROPERTY CXX_STANDARD 20)
target_link_libraries(selfc selfparse argh)

add_executable(lex_test ${CMAKE_SOURCE_DIR}/tests/lex_test.cpp)
set_property(TARGET lex_test PROPERTY CXX_STANDARD 20)
target_link_libraries(lex_test selfparse fmt::fmt)

add_executable(IR_test ${CMAKE_SOURCE_DIR}/tests/IR_test.cpp)
set_property(TARGET IR_test PROPERTY CXX_STANDARD 20)
target_link_libraries(IR_test ${llvm_libs} fmt::fmt)

add_executable(integration_test ${CMAKE_SOURCE_DIR}/tests/integration_test.cpp)
set_property(TARGET integration_test PROPERTY CXX_STANDARD 20)
target_link_libraries(integration_test selfparse selfgen fmt::fmt)

add_executable(exprfoldtest ${CMAKE_SOURCE_DIR}/tests/exprfolding.cpp)
set_property(TARGET exprfoldtest PROPERTY CXX_STANDARD 20)
target_link_libraries(exprfoldtest selfparse selfgen fmt::fmt)

add_executable(cstd_test ${CMAKE_SOURCE_DIR}/tests/cstd_test.c)
target_link_libraries(cstd_test selfstd)

add_executable(clang_parse ${CMAKE_SOURCE_DIR}/tests/clang_parse.cpp)
set_property(TARGET clang_parse PROPERTY CXX_STANDARD 20)
# this is not portable but I'll figure something else out later
target_link_libraries(clang_parse fmt::fmt argh clang)
# target_compile_options(clang_parse PUBLIC -fsanitize=address)
# target_link_options(clang_parse PUBLIC -fsanitize=address)