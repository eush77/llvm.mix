add_entrypoint_object(
  strtoimax
  SRCS
    strtoimax.cpp
  HDRS
    strtoimax.h
  DEPENDS
    libc.src.__support.str_conv_utils
)

add_entrypoint_object(
  strtoumax
  SRCS
    strtoumax.cpp
  HDRS
    strtoumax.h
  DEPENDS
    libc.src.__support.str_conv_utils
)

add_entrypoint_object(
  imaxdiv
  SRCS
    imaxdiv.cpp
  HDRS
    imaxdiv.h
  DEPENDS
    libc.include.inttypes
    libc.src.__support.integer_operations
)
