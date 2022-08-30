add_library(selfparse
lexer.cpp
externs.cpp
declarations.cpp
expression.cpp
parser.cpp)
target_include_directories(selfparse 
PUBLIC ${CMAKE_SOURCE_DIR}/include 
PRIVATE ${CMAKE_SOURCE_DIR})
set_property(TARGET selfparse PROPERTY CXX_STANDARD 20)
target_link_libraries(selfparse PRIVATE re2::re2 selfast selfffi)

if(MSVC)
  target_compile_options(selfparse PRIVATE /W4 /WX)
else()
  target_compile_options(selfparse PRIVATE -Wall -Wextra -pedantic)
endif()
