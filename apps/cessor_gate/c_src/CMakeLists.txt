cmake_minimum_required(VERSION 3.21.0)

project(cessor_gate
    VERSION 0.1.0
    DESCRIPTION "Cessor Gate"
    LANGUAGES C CXX)

#====== Global Configurations ==================#

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(PRIV_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../priv)

set(CMAKE_PREFIX_PATH  ~/Qt/5.15.2/gcc_64/lib/cmake)
set(Qt5_DIR ~/Qt/5.15.2/gcc_64/lib/cmake/Qt5)
set(CMAKE_INSTALL_RPATH ${PRIV_DIR})
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PRIV_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PRIV_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PRIV_DIR})

# Export ALL DLLs symbols on Windows without __declspec(xxxx) annotations.
#set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true)

find_package(
    Qt5 5.15
        REQUIRED
            COMPONENTS
                Core
                #Gui
                Widgets
                #WebEngineCore
                WebEngineWidgets
)

set(CMAKE_CXX_FLAGS_COVERAGE "${CMAKE_CXX_FLAGS_RELEASE} -fprofile-arcs -ftest-coverage")
set_target_properties(Qt5::Core PROPERTIES MAP_IMPORTED_CONFIG_COVERAGE "RELEASE")
set_target_properties(Qt5::Widgets PROPERTIES MAP_IMPORTED_CONFIG_COVERAGE "RELEASE")


#=============== Interface GUI Configurations ============#

# --------------------------------------------------#
set(
    API_SOURCES
        src/cessor_gate.cpp
        src/resources.qrc

        src/window/mainwindow.h src/window/mainwindow.cpp
        src/window/windowposition.h src/window/windowposition.cpp
        src/window/windowagent.h src/window/windowagent.cpp

        src/window/header/header.h src/window/header/header.cpp
        src/window/header/windowcontrol.h src/window/header/windowcontrol.cpp
        src/window/header/tabbar.h src/window/header/tabbar.cpp
        src/window/header/tablist.h src/window/header/tablist.cpp
        src/window/header/tabobject.h src/window/header/tabobject.cpp

        src/window/body/body.h src/window/body/body.cpp
        src/window/body/tabpage.h src/window/body/tabpage.cpp
        src/window/body/urlrequestinterceptor.h src/window/body/urlrequestinterceptor.cpp

        src/window/services/servicemanager.h src/window/services/servicemanager.cpp
        src/window/services/webservice.h src/window/services/webservice.cpp
)

add_library(
    cessor_gate_api
        SHARED
            ${API_SOURCES}           
)

target_link_libraries(
    cessor_gate_api
        PRIVATE
            Qt5::Core
            Qt5::Widgets
            #Qt5::WebEngineCore
            Qt5::WebEngineWidgets
            #Qt5::Gui
)

set(
    NIF_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cessor_gate.h
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cessor_gate.c
)

add_library(
    cessor_gate
        SHARED
            ${NIF_SOURCES}
)

target_link_libraries(
    cessor_gate
        PRIVATE
            cessor_gate_api
)

target_include_directories(
    cessor_gate
        PRIVATE
            $ERL_ROOT/usr/include/
)

#=============== Interface Remote Configurations ============#


set(SSL_DIR /usr/local/openssl)
#set ( OPENSSL_INCLUDE_DIR "${SSL_DIR}/include/openssl/" )
set ( OPENSSL_CRYPTO_LIBRARY ${SSL_DIR}/lib64/libcrypto.a )
set ( OPENSSL_SSL_LIBRARY ${SSL_DIR}/lib64/libssl.a )
set (
        OPENSSL_LIBRARIES
            #/usr/local/src/openssl-3.0.2/crypto/comp/libcrypto-lib-c_zlib.a
            "${SSL_DIR}/lib64/libssl.a;${SSL_DIR}/lib64/libcrypto.a;-lpthread;dl"
         )
set(OPENSSL_ROOT_DIR "${SSL_DIR}/*")
set ( OPENSSL_INCLUDE_DIR "${SSL_DIR}/include/" )
set(OPENSSL_USE_STATIC_LIBS TRUE)
#set(CMAKE_REQUIRED_LIBRARIES /usr/local/src/openssl-3.0.2/crypto/comp)
#set(CMAKE_REQUIRED_LIBRARIES /usr/local/src/openssl-3.0.2/crypto/comp/libcrypto-lib-c_zlib.a)
include(FindOpenSSL)
#include_directories(${OPENSSL_INCLUDE_DIR})

find_package(OpenSSL 3.0.2 REQUIRED)

#find_package(OpenSSL REQUIRED)
message(STATUS "OPENSSL_FOUND=${OPENSSL_FOUND}")
message(STATUS "OPENSSL_INCLUDE_DIR=${OPENSSL_INCLUDE_DIR}")
message(STATUS "OPENSSL_CRYPTO_LIBRARY=${OPENSSL_CRYPTO_LIBRARY}")
message(STATUS "OPENSSL_SSL_LIBRARY=${OPENSSL_SSL_LIBRARY}")
message(STATUS "OPENSSL_LIBRARIES=${OPENSSL_LIBRARIES}")
message(STATUS "CMAKE_REQUIRED_LIBRARIES=${CMAKE_REQUIRED_LIBRARIES}")
message(STATUS "OPENSSL_VERSION=${OPENSSL_VERSION}")

set(
    INTERFACE_SOURCES
        src/interface/main.cpp

        src/interface/interface_state.h
        src/interface/interface_remote.h src/interface/interface_remote.cpp

        src/interface/profile/profile.h src/interface/profile/profile.cpp

        src/interface/io/hidden_service_input.hpp
        src/interface/io/io.h src/interface/io/io.cpp
        src/interface/io/input_service.h src/interface/io/input_service.cpp

        src/interface/launcher/launcher.h src/interface/launcher/launcher.cpp

        src/interface/cstp/cstx.h
        src/interface/cstp/cstp_callback.h
        src/interface/cstp/cstp.h src/interface/cstp/cstp.cpp
        src/interface/cstp/coldstate.h src/interface/cstp/coldstate.cpp
        src/interface/cstp/state/cstp_000000_0000_0303_00.h src/interface/cstp/state/cstp_000000_0000_0303_00.cpp
        src/interface/cstp/call/cstp_000000_0000_0303_01.h src/interface/cstp/call/cstp_000000_0000_0303_01.cpp

        src/interface/crypto/crypto.h src/interface/crypto/crypto.cpp
)
add_executable(
    cessor
        ${INTERFACE_SOURCES}
)
target_include_directories(
    cessor
        PRIVATE
            ${OPENSSL_INCLUDE_DIR}
            ##$ERL_ROOT/usr/include/
            $ERL_ROOT/lib/erl_interface-3.13.1/include
            $ERL_ROOT/lib/erl_interface-3.13.1/src/connect

)
target_link_libraries(
    cessor
        PRIVATE
            Qt5::Widgets
            Qt5::Core
            ${OPENSSL_LIBRARIES}
            #OpenSSL::SSL
            #OpenSSL::Crypto

            #/usr/local/src/openssl-3.0.2/crypto/comp/
)
target_link_directories(
    cessor
        PRIVATE
        $OPENSSL_LIBRARIES
        ##$ERL_ROOT/usr/lib
        $ERL_ROOT/lib/erl_interface-3.13.1/lib
        /usr/local/ssl/lib64
        #/usr/local/src/openssl-3.0.2/crypto/
)

#include(InstallRequiredSystemLibraries)

#add_executable(
    #cessor_gate_gui_debug
     #   src/cessor_gate.h
      #  src/cessor_gate_gui_debug.c
#)
#target_link_libraries(
    #cessor_gate_gui_debug
     #   PRIVATE
      #      cessor_gate_api
#)

install(
    TARGETS
        cessor
        cessor_gate
        cessor_gate_api
        #cessor_gate_gui_debug
    RUNTIME DESTINATION ${PRIV_DIR})
