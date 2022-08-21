add_library(selfgen codegen.cpp backend_config.cpp)
target_include_directories(selfgen PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_property(TARGET selfgen PROPERTY CXX_STANDARD 20)
target_link_libraries(selfgen PUBLIC LLVM selfast)