add_library(selfstd ${STDLIB_SRC}/io.c)
target_include_directories(selfstd PUBLIC ${STDLIB_SRC}/include)