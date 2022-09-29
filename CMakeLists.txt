cmake_minimum_required(VERSION 3.20.0)
project(
  LunarTcc
  VERSION 0.1.0
  LANGUAGES CXX
)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin)
include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/include ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_subdirectory(lib)

add_subdirectory(tests)
# add_subdirectory(tools)
