cmake_minimum_required(VERSION 3.12)

project(c+ CXX)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(PROJECT_SRC ${CMAKE_SOURCE_DIR}/src)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR})

add_executable(c+ ${PROJECT_SRC}/cpc.cpp ${PROJECT_SRC}/lexer.cpp)
target_include_directories(c+ PRIVATE ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET c+ PROPERTY CXX_STANDARD 20)

add_executable(regex_test ${CMAKE_SOURCE_DIR}/tests/regex_test.cpp)
set_property(TARGET regex_test PROPERTY CXX_STANDARD 20)

find_package(argh REQUIRED)
find_package(re2 REQUIRED)

if(TARGET argh::argh)
   target_link_libraries(c+ argh::argh)
endif()
if(TARGET re2::re2)
   target_link_libraries(c+ re2::re2)
   target_link_libraries(regex_test re2::re2)
endif()