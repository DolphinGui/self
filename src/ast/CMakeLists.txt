add_library(selfast ast.cpp complete_types.cpp)
target_include_directories(selfast PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfast PROPERTY CXX_STANDARD 20)