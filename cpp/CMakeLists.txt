cmake_minimum_required(VERSION 3.28)
project(cpp)

set(CMAKE_CXX_STANDARD 20)

find_package(Arrow REQUIRED)

add_executable(cpp main.cpp)
target_link_libraries(cpp PRIVATE Arrow::arrow_shared)