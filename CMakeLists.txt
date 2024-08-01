cmake_minimum_required(VERSION 3.28)
project(ConMemMapPerfTest)

set(CMAKE_CXX_STANDARD 20)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_executable(${PROJECT_NAME} main.cpp)

target_sources(${PROJECT_NAME} PRIVATE
        FILE_SET CXX_MODULES FILES
        ConMemMap/ConMemMap.cppm
        Util/Util.cppm
        Util/PerfUtil.cppm
        STLModules/std.compat.ixx
        STLModules/std.ixx
)