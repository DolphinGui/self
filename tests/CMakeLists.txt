add_executable(lex_test ${CMAKE_SOURCE_DIR}/tests/lex_test.cpp)
set_property(TARGET lex_test PROPERTY CXX_STANDARD 20)
target_link_libraries(lex_test selfparse fmt::fmt)

add_executable(IR_test ${CMAKE_SOURCE_DIR}/tests/IR_test.cpp)
set_property(TARGET IR_test PROPERTY CXX_STANDARD 20)
target_link_libraries(IR_test LLVM fmt::fmt)

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
target_link_libraries(clang_parse fmt::fmt argparse::argparse clang)

add_executable(ffi_test ${CMAKE_SOURCE_DIR}/tests/ffi_test.cpp)
set_property(TARGET ffi_test PROPERTY CXX_STANDARD 20)
target_link_libraries(ffi_test PRIVATE fmt::fmt selfffi)
target_compile_definitions(ffi_test PRIVATE PROJECT_SOURCE=${CMAKE_SOURCE_DIR})

add_executable(error_test ${CMAKE_SOURCE_DIR}/tests/error_test.cpp)
set_property(TARGET error_test PROPERTY CXX_STANDARD 20)
target_link_libraries(error_test PRIVATE fmt::fmt selfparse)

add_executable(bumpalloc ${CMAKE_SOURCE_DIR}/tests/bumpalloc.cpp)
set_property(TARGET bumpalloc PROPERTY CXX_STANDARD 20)
target_compile_options(bumpalloc PRIVATE -fsanitize=address)
target_link_options(bumpalloc PRIVATE -fsanitize=address)
target_link_libraries(bumpalloc PRIVATE fmt::fmt selfparse GTest::gtest_main)
gtest_discover_tests(bumpalloc)