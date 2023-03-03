cmake_minimum_required(VERSION 3.9.0)
SET(PROJECT_NAME "cxx-wxw")
project(${PROJECT_NAME})
set(default_build_type "Release")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)

set(wxWidgets_CONFIGURATION mswu)
find_package(wxWidgets COMPONENTS core base adv aui gl html media propgrid qa ribbon richtext stc webview xrc REQUIRED)
include(${wxWidgets_USE_FILE})

set(MY_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/include)
set(MY_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp)

include_directories(${MY_INCLUDE_DIR} ${MY_SOURCE_DIR}/include ${CLCXX_INCLUDE_DIR})

find_library(CLCXX NAMES ClCxx ClCxx.dll PATHS ${CLCXX_LIB_DIR} NO_DEFAULT_PATH)
link_directories(${CLCXX_LIB_DIR} ${wxWidgets_LIB_DIR})

file(GLOB HEADER_GLOB
  ${MY_INCLUDE_DIR}/*.hpp
  )

file(GLOB SOURCES_GLOB
  ${MY_SOURCE_DIR}/*.cpp
  )

set(MY_HEADERS
  ${HEADER_GLOB}
  )

set(MY_SOURCES
  ${SOURCES_GLOB}
  )
  
add_library(${PROJECT_NAME} SHARED ${SOURCES_GLOB})
target_link_libraries(${PROJECT_NAME} ClCxx ${wxWidgets_LIBRARIES})

install(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/lib) 
 

  
