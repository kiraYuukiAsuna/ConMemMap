cmake_minimum_required(VERSION 3.28)
project(ConMemMapPerfTest)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_executable(${PROJECT_NAME} main.cpp)

target_sources(${PROJECT_NAME} PRIVATE
        FILE_SET CXX_MODULES FILES
        "ConMemMap/ConMemMap.ixx"
        "Util/Util.ixx"
        "Util/PerfUtil.ixx"
        STLModules/std.compat.ixx
        STLModules/std.ixx
)

target_include_directories(${PROJECT_NAME} PRIVATE
		${CMAKE_CURRENT_SOURCE_DIR}
        hopscotch-map-2.3.1/include/tsl

)
