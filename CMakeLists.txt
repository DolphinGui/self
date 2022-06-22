cmake_minimum_required(VERSION 3.12)

project(selfc CXX)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(PROJECT_SRC ${CMAKE_SOURCE_DIR}/src)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR})

find_package(argh REQUIRED)
find_package(re2 REQUIRED)
find_package(function_pipes REQUIRED)
find_package(sml REQUIRED)
find_package(fmt REQUIRED)
find_package(rva REQUIRED)
find_package(polymorphic_list REQUIRED)

add_compile_options(-fsanitize=address)
add_link_options(-fsanitize=address)

add_library(selflib ${PROJECT_SRC}/lexer.cpp ${PROJECT_SRC}/fsa_parser.cpp)
target_include_directories(selflib PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selflib PROPERTY CXX_STANDARD 20)
target_link_libraries(selflib PUBLIC argh::argh re2::re2 function_pipes::function_pipes sml::sml rva::rva polymorphic_list::polymorphic_list)

add_executable(selfc ${PROJECT_SRC}/selfc.cpp)
target_include_directories(selfc PRIVATE ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfc PROPERTY CXX_STANDARD 20)
target_link_libraries(selfc selflib)

add_executable(regex_test ${CMAKE_SOURCE_DIR}/tests/regex_test.cpp)
set_property(TARGET regex_test PROPERTY CXX_STANDARD 20)
target_link_libraries(regex_test re2::re2)

add_executable(lex_test ${CMAKE_SOURCE_DIR}/tests/lex_test.cpp)
set_property(TARGET lex_test PROPERTY CXX_STANDARD 20)
target_link_libraries(lex_test selflib)

add_executable(sml_test ${CMAKE_SOURCE_DIR}/tests/sml_test.cpp)
set_property(TARGET sml_test PROPERTY CXX_STANDARD 20)
target_link_libraries(sml_test sml::sml fmt::fmt)

add_executable(rva_test ${CMAKE_SOURCE_DIR}/tests/rva_test.cpp)
set_property(TARGET rva_test PROPERTY CXX_STANDARD 20)
target_link_libraries(rva_test rva::rva fmt::fmt)
