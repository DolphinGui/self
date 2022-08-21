add_library(selfffi ffi_parse.cpp)
target_include_directories(selfffi PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfffi PROPERTY CXX_STANDARD 20)
target_link_libraries(selfffi PRIVATE clang selfast)