cmake_minimum_required(VERSION 3.19)

file(GLOB_RECURSE LUNAR_TCC_SOURCES  "../lib/*.cpp")

add_executable(
    LunarTcc
    Driver.cpp
    ${LUNAR_TCC_SOURCES}
)

target_link_libraries(LunarTcc PRIVATE fmt::fmt)
