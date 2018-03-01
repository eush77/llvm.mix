add_llvm_library(LLVMMix
  Mix.cpp
  Types.cpp

  DEPENDS
  intrinsics_gen
  )
