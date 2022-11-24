cmake_minimum_required(VERSION 3.20.0)
project(
  LunarTcc
  VERSION 0.1.0
  LANGUAGES CXX
)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXE_LINKER_FLAGS "-fuse-ld=mold")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin)
add_link_options("-fuse-ld=mold")

include(FetchContent)
FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG master
)
FetchContent_MakeAvailable(fmt)

include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/include ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_subdirectory(lib)
add_subdirectory(tool)
