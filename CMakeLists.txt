cmake_minimum_required(VERSION 3.12)

project(selfc CXX C)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(PROJECT_SRC ${CMAKE_SOURCE_DIR}/src)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR})

find_package(argh REQUIRED)
find_package(re2 REQUIRED)
find_package(function_pipes REQUIRED)
find_package(fmt REQUIRED)
find_package(polymorphic_list REQUIRED)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

llvm_map_components_to_libnames(llvm_libs support core)

add_compile_options(-fsanitize=address)
add_link_options(-fsanitize=address)

add_library(selflib ${PROJECT_SRC}/lexer.cpp ${PROJECT_SRC}/fsa_parser.cpp ${PROJECT_SRC}/codegen.cpp)
target_include_directories(selflib PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selflib PROPERTY CXX_STANDARD 20)
target_link_libraries(selflib PUBLIC argh::argh re2::re2 function_pipes::function_pipes ${llvm_libs})

if(MSVC)
  target_compile_options(selflib PRIVATE /W4 /WX)
else()
  target_compile_options(selflib PRIVATE -Wall -Wextra)
endif()

add_executable(selfc ${PROJECT_SRC}/selfc.cpp)
target_include_directories(selfc PRIVATE ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfc PROPERTY CXX_STANDARD 20)
target_link_libraries(selfc selflib)

add_executable(regex_test ${CMAKE_SOURCE_DIR}/tests/regex_test.cpp)
set_property(TARGET regex_test PROPERTY CXX_STANDARD 20)
target_link_libraries(regex_test re2::re2)

add_executable(lex_test ${CMAKE_SOURCE_DIR}/tests/lex_test.cpp)
set_property(TARGET lex_test PROPERTY CXX_STANDARD 20)
target_link_libraries(lex_test selflib fmt::fmt)

add_executable(IR_test ${CMAKE_SOURCE_DIR}/tests/IR_test.cpp)
set_property(TARGET IR_test PROPERTY CXX_STANDARD 20)
target_link_libraries(IR_test ${llvm_libs} fmt::fmt)

add_executable(integration_test ${CMAKE_SOURCE_DIR}/tests/integration_test.cpp)
set_property(TARGET integration_test PROPERTY CXX_STANDARD 20)
target_link_libraries(integration_test selflib fmt::fmt)