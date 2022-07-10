cmake_minimum_required(VERSION 3.12)

project(selfc CXX C)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(COMPILER_SRC ${CMAKE_SOURCE_DIR}/src)
set(STDLIB_SRC ${CMAKE_SOURCE_DIR}/stdlib)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR})

find_package(argh REQUIRED)
find_package(re2 REQUIRED)
find_package(fmt REQUIRED)

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

add_compile_options(-fsanitize=address)
add_link_options(-fsanitize=address)

add_library(selfparse ${COMPILER_SRC}/lexer.cpp ${COMPILER_SRC}/fsa_parser.cpp)
target_include_directories(selfparse PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfparse PROPERTY CXX_STANDARD 20)
target_link_libraries(selfparse PRIVATE re2::re2)

add_library(selfgen ${COMPILER_SRC}/codegen.cpp ${COMPILER_SRC}/backend_config.cpp)
target_include_directories(selfgen PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfgen PROPERTY CXX_STANDARD 20)
target_link_libraries(selfgen ${llvm_libs} ${TARGET_INFO})

add_library(selfstd
${STDLIB_SRC}/io.c)
target_include_directories(selfstd PUBLIC
${STDLIB_SRC}/include)

if(MSVC)
  target_compile_options(selfparse PRIVATE /W4 /WX)
else()
  target_compile_options(selfparse PRIVATE -Wall -Wextra)
endif()

add_executable(selfc ${COMPILER_SRC}/selfc.cpp)
target_include_directories(selfc PRIVATE ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfc PROPERTY CXX_STANDARD 20)
target_link_libraries(selfc selfparse argh::argh)

add_executable(regex_test ${CMAKE_SOURCE_DIR}/tests/regex_test.cpp)
set_property(TARGET regex_test PROPERTY CXX_STANDARD 20)
target_link_libraries(regex_test re2::re2)

add_executable(lex_test ${CMAKE_SOURCE_DIR}/tests/lex_test.cpp)
set_property(TARGET lex_test PROPERTY CXX_STANDARD 20)
target_link_libraries(lex_test selfparse fmt::fmt)

add_executable(IR_test ${CMAKE_SOURCE_DIR}/tests/IR_test.cpp)
set_property(TARGET IR_test PROPERTY CXX_STANDARD 20)
target_link_libraries(IR_test ${llvm_libs} fmt::fmt)

add_executable(integration_test ${CMAKE_SOURCE_DIR}/tests/integration_test.cpp)
set_property(TARGET integration_test PROPERTY CXX_STANDARD 20)
target_link_libraries(integration_test selfparse selfgen fmt::fmt)

add_executable(cstd_test ${CMAKE_SOURCE_DIR}/tests/cstd_test.c)
target_link_libraries(cstd_test selfstd)